"use strict";
/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.UpdateProject = void 0;
const path_1 = require("path");
const ts = require("typescript");
const component_resource_collector_1 = require("./component-resource-collector");
const logger_1 = require("./logger");
const parse_tsconfig_1 = require("./utils/parse-tsconfig");
/**
 * An update project that can be run against individual migrations. An update project
 * accepts a TypeScript program and a context that is provided to all migrations. The
 * context is usually not used by migrations, but in some cases migrations rely on
 * specifics from the tool that performs the update (e.g. the Angular CLI). In those cases,
 * the context can provide the necessary specifics to the migrations in a type-safe way.
 */
class UpdateProject {
    constructor(_context, _program, _fileSystem, _analyzedFiles = new Set(), _logger = logger_1.defaultLogger) {
        this._context = _context;
        this._program = _program;
        this._fileSystem = _fileSystem;
        this._analyzedFiles = _analyzedFiles;
        this._logger = _logger;
        this._typeChecker = this._program.getTypeChecker();
    }
    /**
     * Migrates the project to the specified target version.
     * @param migrationTypes Migrations that should be run.
     * @param target Version the project should be updated to.
     * @param data Upgrade data that is passed to all migration rules.
     * @param additionalStylesheetPaths Additional stylesheets that should be migrated, if not
     *   referenced in an Angular component. This is helpful for global stylesheets in a project.
     */
    migrate(migrationTypes, target, data, additionalStylesheetPaths) {
        // Create instances of the specified migrations.
        const migrations = this._createMigrations(migrationTypes, target, data);
        // Creates the component resource collector. The collector can visit arbitrary
        // TypeScript nodes and will find Angular component resources. Resources include
        // templates and stylesheets. It also captures inline stylesheets and templates.
        const resourceCollector = new component_resource_collector_1.ComponentResourceCollector(this._typeChecker, this._fileSystem);
        // Collect all of the TypeScript source files we want to migrate. We don't
        // migrate type definition files, or source files from external libraries.
        const sourceFiles = this._program.getSourceFiles().filter(f => !f.isDeclarationFile && !this._program.isSourceFileFromExternalLibrary(f));
        // Helper function that visits a given TypeScript node and collects all referenced
        // component resources (i.e. stylesheets or templates). Additionally, the helper
        // visits the node in each instantiated migration.
        const visitNodeAndCollectResources = (node) => {
            migrations.forEach(r => r.visitNode(node));
            ts.forEachChild(node, visitNodeAndCollectResources);
            resourceCollector.visitNode(node);
        };
        // Walk through all source file, if it has not been visited before, and
        // visit found nodes while collecting potential resources.
        sourceFiles.forEach(sourceFile => {
            const resolvedPath = this._fileSystem.resolve(sourceFile.fileName);
            // Do not visit source files which have been checked as part of a
            // previously migrated TypeScript project.
            if (!this._analyzedFiles.has(resolvedPath)) {
                visitNodeAndCollectResources(sourceFile);
                this._analyzedFiles.add(resolvedPath);
            }
        });
        // Walk through all resolved templates and visit them in each instantiated
        // migration. Note that this can only happen after source files have been
        // visited because we find templates through the TypeScript source files.
        resourceCollector.resolvedTemplates.forEach(template => {
            // Do not visit the template if it has been checked before. Inline
            // templates cannot be referenced multiple times.
            if (template.inline || !this._analyzedFiles.has(template.filePath)) {
                migrations.forEach(m => m.visitTemplate(template));
                this._analyzedFiles.add(template.filePath);
            }
        });
        // Walk through all resolved stylesheets and visit them in each instantiated
        // migration. Note that this can only happen after source files have been
        // visited because we find stylesheets through the TypeScript source files.
        resourceCollector.resolvedStylesheets.forEach(stylesheet => {
            // Do not visit the stylesheet if it has been checked before. Inline
            // stylesheets cannot be referenced multiple times.
            if (stylesheet.inline || !this._analyzedFiles.has(stylesheet.filePath)) {
                migrations.forEach(r => r.visitStylesheet(stylesheet));
                this._analyzedFiles.add(stylesheet.filePath);
            }
        });
        // In some applications, developers will have global stylesheets which are not
        // specified in any Angular component. Therefore we allow for additional stylesheets
        // being specified. We visit them in each migration unless they have been already
        // discovered before as actual component resource.
        additionalStylesheetPaths.forEach(filePath => {
            const resolvedPath = this._fileSystem.resolve(filePath);
            const stylesheet = resourceCollector.resolveExternalStylesheet(resolvedPath, null);
            // Do not visit stylesheets which have been referenced from a component.
            if (!this._analyzedFiles.has(resolvedPath)) {
                migrations.forEach(r => r.visitStylesheet(stylesheet));
                this._analyzedFiles.add(resolvedPath);
            }
        });
        // Call the "postAnalysis" method for each migration.
        migrations.forEach(r => r.postAnalysis());
        // Collect all failures reported by individual migrations.
        const failures = migrations.reduce((res, m) => res.concat(m.failures), []);
        // In case there are failures, print these to the CLI logger as warnings.
        if (failures.length) {
            failures.forEach(({ filePath, message, position }) => {
                const lineAndCharacter = position ? `@${position.line + 1}:${position.character + 1}` : '';
                this._logger.warn(`${filePath}${lineAndCharacter} - ${message}`);
            });
        }
        return {
            hasFailures: !!failures.length,
        };
    }
    /**
     * Creates instances of the given migrations with the specified target
     * version and data.
     */
    _createMigrations(types, target, data) {
        const result = [];
        for (const ctor of types) {
            const instance = new ctor(this._program, this._typeChecker, target, this._context, data, this._fileSystem, this._logger);
            instance.init();
            if (instance.enabled) {
                result.push(instance);
            }
        }
        return result;
    }
    /**
     * Creates a program form the specified tsconfig and patches the host
     * to read files through the given file system.
     */
    static createProgramFromTsconfig(tsconfigFsPath, fs) {
        const parsed = parse_tsconfig_1.parseTsconfigFile(tsconfigFsPath, path_1.dirname(tsconfigFsPath));
        const host = ts.createCompilerHost(parsed.options, true);
        // Patch the host to read files through the specified file system.
        host.readFile = fileName => {
            const fileContent = fs.read(fs.resolve(fileName));
            // Strip BOM as otherwise TSC methods (e.g. "getWidth") will return an offset which
            // which breaks the CLI UpdateRecorder. https://github.com/angular/angular/pull/30719
            return fileContent !== null ? fileContent.replace(/^\uFEFF/, '') : undefined;
        };
        return ts.createProgram(parsed.fileNames, parsed.options, host);
    }
}
exports.UpdateProject = UpdateProject;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5kZXguanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL3NjaGVtYXRpY3MvdXBkYXRlLXRvb2wvaW5kZXgudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBOzs7Ozs7R0FNRzs7O0FBRUgsK0JBQTZCO0FBQzdCLGlDQUFpQztBQUVqQyxpRkFBMEU7QUFFMUUscUNBQXFEO0FBR3JELDJEQUF5RDtBQUV6RDs7Ozs7O0dBTUc7QUFDSCxNQUFhLGFBQWE7SUFHeEIsWUFBb0IsUUFBaUIsRUFDakIsUUFBb0IsRUFDcEIsV0FBdUIsRUFDdkIsaUJBQXFDLElBQUksR0FBRyxFQUFFLEVBQzlDLFVBQXdCLHNCQUFhO1FBSnJDLGFBQVEsR0FBUixRQUFRLENBQVM7UUFDakIsYUFBUSxHQUFSLFFBQVEsQ0FBWTtRQUNwQixnQkFBVyxHQUFYLFdBQVcsQ0FBWTtRQUN2QixtQkFBYyxHQUFkLGNBQWMsQ0FBZ0M7UUFDOUMsWUFBTyxHQUFQLE9BQU8sQ0FBOEI7UUFOeEMsaUJBQVksR0FBbUIsSUFBSSxDQUFDLFFBQVEsQ0FBQyxjQUFjLEVBQUUsQ0FBQztJQU1uQixDQUFDO0lBRTdEOzs7Ozs7O09BT0c7SUFDSCxPQUFPLENBQU8sY0FBOEMsRUFBRSxNQUFxQixFQUFFLElBQVUsRUFDM0YseUJBQW9DO1FBQ3RDLGdEQUFnRDtRQUNoRCxNQUFNLFVBQVUsR0FBRyxJQUFJLENBQUMsaUJBQWlCLENBQUMsY0FBYyxFQUFFLE1BQU0sRUFBRSxJQUFJLENBQUMsQ0FBQztRQUN4RSw4RUFBOEU7UUFDOUUsZ0ZBQWdGO1FBQ2hGLGdGQUFnRjtRQUNoRixNQUFNLGlCQUFpQixHQUFHLElBQUkseURBQTBCLENBQUMsSUFBSSxDQUFDLFlBQVksRUFBRSxJQUFJLENBQUMsV0FBVyxDQUFDLENBQUM7UUFDOUYsMEVBQTBFO1FBQzFFLDBFQUEwRTtRQUMxRSxNQUFNLFdBQVcsR0FBRyxJQUFJLENBQUMsUUFBUSxDQUFDLGNB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QUFDLGFBQWEsQ0FBQyxNQUFNLENBQUMsU0FBUyxFQUFFLE1BQU0sQ0FBQyxPQUFPLEVBQUUsSUFBSSxDQUFDLENBQUM7SUFDbEUsQ0FBQztDQUNGO0FBL0lELHNDQStJQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge2Rpcm5hbWV9IGZyb20gJ3BhdGgnO1xuaW1wb3J0ICogYXMgdHMgZnJvbSAndHlwZXNjcmlwdCc7XG5cbmltcG9ydCB7Q29tcG9uZW50UmVzb3VyY2VDb2xsZWN0b3J9IGZyb20gJy4vY29tcG9uZW50LXJlc291cmNlLWNvbGxlY3Rvcic7XG5pbXBvcnQge0ZpbGVTeXN0ZW0sIFdvcmtzcGFjZVBhdGh9IGZyb20gJy4vZmlsZS1zeXN0ZW0nO1xuaW1wb3J0IHtkZWZhdWx0TG9nZ2VyLCBVcGRhdGVMb2dnZXJ9IGZyb20gJy4vbG9nZ2VyJztcbmltcG9ydCB7TWlncmF0aW9uLCBNaWdyYXRpb25DdG9yLCBNaWdyYXRpb25GYWlsdXJlfSBmcm9tICcuL21pZ3JhdGlvbic7XG5pbXBvcnQge1RhcmdldFZlcnNpb259IGZyb20gJy4vdGFyZ2V0LXZlcnNpb24nO1xuaW1wb3J0IHtwYXJzZVRzY29uZmlnRmlsZX0gZnJvbSAnLi91dGlscy9wYXJzZS10c2NvbmZpZyc7XG5cbi8qKlxuICogQW4gdXBkYXRlIHByb2plY3QgdGhhdCBjYW4gYmUgcnVuIGFnYWluc3QgaW5kaXZpZHVhbCBtaWdyYXRpb25zLiBBbiB1cGRhdGUgcHJvamVjdFxuICogYWNjZXB0cyBhIFR5cGVTY3JpcHQgcHJvZ3JhbSBhbmQgYSBjb250ZXh0IHRoYXQgaXMgcHJvdmlkZWQgdG8gYWxsIG1pZ3JhdGlvbnMuIFRoZVxuICogY29udGV4dCBpcyB1c3VhbGx5IG5vdCB1c2VkIGJ5IG1pZ3JhdGlvbnMsIGJ1dCBpbiBzb21lIGNhc2VzIG1pZ3JhdGlvbnMgcmVseSBvblxuICogc3BlY2lmaWNzIGZyb20gdGhlIHRvb2wgdGhhdCBwZXJmb3JtcyB0aGUgdXBkYXRlIChlLmcuIHRoZSBBbmd1bGFyIENMSSkuIEluIHRob3NlIGNhc2VzLFxuICogdGhlIGNvbnRleHQgY2FuIHByb3ZpZGUgdGhlIG5lY2Vzc2FyeSBzcGVjaWZpY3MgdG8gdGhlIG1pZ3JhdGlvbnMgaW4gYSB0eXBlLXNhZmUgd2F5LlxuICovXG5leHBvcnQgY2xhc3MgVXBkYXRlUHJvamVjdDxDb250ZXh0PiB7XG4gIHByaXZhdGUgcmVhZG9ubHkgX3R5cGVDaGVja2VyOiB0cy5UeXBlQ2hlY2tlciA9IHRoaXMuX3Byb2dyYW0uZ2V0VHlwZUNoZWNrZXIoKTtcblxuICBjb25zdHJ1Y3Rvcihwcml2YXRlIF9jb250ZXh0OiBDb250ZXh0LFxuICAgICAgICAgICAgICBwcml2YXRlIF9wcm9ncmFtOiB0cy5Qcm9ncmFtLFxuICAgICAgICAgICAgICBwcml2YXRlIF9maWxlU3lzdGVtOiBGaWxlU3lzdGVtLFxuICAgICAgICAgICAgICBwcml2YXRlIF9hbmFseXplZEZpbGVzOiBTZXQ8V29ya3NwYWNlUGF0aD4gPSBuZXcgU2V0KCksXG4gICAgICAgICAgICAgIHByaXZhdGUgX2xvZ2dlcjogVXBkYXRlTG9nZ2VyID0gZGVmYXVsdExvZ2dlcikge31cblxuICAvKipcbiAgICogTWlncmF0ZXMgdGhlIHByb2plY3QgdG8gdGhlIHNwZWNpZmllZCB0YXJnZXQgdmVyc2lvbi5cbiAgICogQHBhcmFtIG1pZ3JhdGlvblR5cGVzIE1pZ3JhdGlvbnMgdGhhdCBzaG91bGQgYmUgcnVuLlxuICAgKiBAcGFyYW0gdGFyZ2V0IFZlcnNpb24gdGhlIHByb2plY3Qgc2hvdWxkIGJlIHVwZGF0ZWQgdG8uXG4gICAqIEBwYXJhbSBkYXRhIFVwZ3JhZGUgZGF0YSB0aGF0IGlzIHBhc3NlZCB0byBhbGwgbWlncmF0aW9uIHJ1bGVzLlxuICAgKiBAcGFyYW0gYWRkaXRpb25hbFN0eWxlc2hlZXRQYXRocyBBZGRpdGlvbmFsIHN0eWxlc2hlZXRzIHRoYXQgc2hvdWxkIGJlIG1pZ3JhdGVkLCBpZiBub3RcbiAgICogICByZWZlcmVuY2VkIGluIGFuIEFuZ3VsYXIgY29tcG9uZW50LiBUaGlzIGlzIGhlbHBmdWwgZm9yIGdsb2JhbCBzdHlsZXNoZWV0cyBpbiBhIHByb2plY3QuXG4gICAqL1xuICBtaWdyYXRlPERhdGE+KG1pZ3JhdGlvblR5cGVzOiBNaWdyYXRpb25DdG9yPERhdGEsIENvbnRleHQ+W10sIHRhcmdldDogVGFyZ2V0VmVyc2lvbiwgZGF0YTogRGF0YSxcbiAgICAgIGFkZGl0aW9uYWxTdHlsZXNoZWV0UGF0aHM/OiBzdHJpbmdbXSk6IHtoYXNGYWlsdXJlczogYm9vbGVhbn0ge1xuICAgIC8vIENyZWF0ZSBpbnN0YW5jZXMgb2YgdGhlIHNwZWNpZmllZCBtaWdyYXRpb25zLlxuICAgIGNvbnN0IG1pZ3JhdGlvbnMgPSB0aGlzLl9jcmVhdGVNaWdyYXRpb25zKG1pZ3JhdGlvblR5cGVzLCB0YXJnZXQsIGRhdGEpO1xuICAgIC8vIENyZWF0ZXMgdGhlIGNvbXBvbmVudCByZXNvdXJjZSBjb2xsZWN0b3IuIFRoZSBjb2xsZWN0b3IgY2FuIHZpc2l0IGFyYml0cmFyeVxuICAgIC8vIFR5cGVTY3JpcHQgbm9kZXMgYW5kIHdpbGwgZmluZCBBbmd1bGFyIGNvbXBvbmVudCByZXNvdXJjZXMuIFJlc291cmNlcyBpbmNsdWRlXG4gICAgLy8gdGVtcGxhdGVzIGFuZCBzdHlsZXNoZWV0cy4gSXQgYWxzbyBjYXB0dXJlcyBpbmxpbmUgc3R5bGVzaGVldHMgYW5kIHRlbXBsYXRlcy5cbiAgICBjb25zdCByZXNvdXJjZUNvbGxlY3RvciA9IG5ldyBDb21wb25lbnRSZXNvdXJjZUNvbGxlY3Rvcih0aGlzLl90eXBlQ2hlY2tlciwgdGhpcy5fZmlsZVN5c3RlbSk7XG4gICAgLy8gQ29sbGVjdCBhbGwgb2YgdGhlIFR5cGVTY3JpcHQgc291cmNlIGZpbGVzIHdlIHdhbnQgdG8gbWlncmF0ZS4gV2UgZG9uJ3RcbiAgICAvLyBtaWdyYXRlIHR5cGUgZGVmaW5pdGlvbiBmaWxlcywgb3Igc291cmNlIGZpbGVzIGZyb20gZXh0ZXJuYWwgbGlicmFyaWVzLlxuICAgIGNvbnN0IHNvdXJjZUZpbGVzID0gdGhpcy5fcHJvZ3JhbS5nZXRTb3VyY2VGaWxlcygpLmZpbHRlcihcbiAgICAgIGYgPT4gIWYuaXNEZWNsYXJhdGlvbkZpbGUgJiYgIXRoaXMuX3Byb2dyYW0uaXNTb3VyY2VGaWxlRnJvbUV4dGVybmFsTGlicmFyeShmKSk7XG5cbiAgICAvLyBIZWxwZXIgZnVuY3Rpb24gdGhhdCB2aXNpdHMgYSBnaXZlbiBUeXBlU2NyaXB0IG5vZGUgYW5kIGNvbGxlY3RzIGFsbCByZWZlcmVuY2VkXG4gICAgLy8gY29tcG9uZW50IHJlc291cmNlcyAoaS5lLiBzdHlsZXNoZWV0cyBvciB0ZW1wbGF0ZXMpLiBBZGRpdGlvbmFsbHksIHRoZSBoZWxwZXJcbiAgICAvLyB2aXNpdHMgdGhlIG5vZGUgaW4gZWFjaCBpbnN0YW50aWF0ZWQgbWlncmF0aW9uLlxuICAgIGNvbnN0IHZpc2l0Tm9kZUFuZENvbGxlY3RSZXNvdXJjZXMgPSAobm9kZTogdHMuTm9kZSkgPT4ge1xuICAgICAgbWlncmF0aW9ucy5mb3JFYWNoKHIgPT4gci52aXNpdE5vZGUobm9kZSkpO1xuICAgICAgdHMuZm9yRWFjaENoaWxkKG5vZGUsIHZpc2l0Tm9kZUFuZENvbGxlY3RSZXNvdXJjZXMpO1xuICAgICAgcmVzb3VyY2VDb2xsZWN0b3IudmlzaXROb2RlKG5vZGUpO1xuICAgIH07XG5cbiAgICAvLyBXYWxrIHRocm91Z2ggYWxsIHNvdXJjZSBmaWxlLCBpZiBpdCBoYXMgbm90IGJlZW4gdmlzaXRlZCBiZWZvcmUsIGFuZFxuICAgIC8vIHZpc2l0IGZvdW5kIG5vZGVzIHdoaWxlIGNvbGxlY3RpbmcgcG90ZW50aWFsIHJlc291cmNlcy5cbiAgICBzb3VyY2VGaWxlcy5mb3JFYWNoKHNvdXJjZUZpbGUgPT4ge1xuICAgICAgY29uc3QgcmVzb2x2ZWRQYXRoID0gdGhpcy5fZmlsZVN5c3RlbS5yZXNvbHZlKHNvdXJjZUZpbGUuZmlsZU5hbWUpO1xuICAgICAgLy8gRG8gbm90IHZpc2l0IHNvdXJjZSBmaWxlcyB3aGljaCBoYXZlIGJlZW4gY2hlY2tlZCBhcyBwYXJ0IG9mIGFcbiAgICAgIC8vIHByZXZpb3VzbHkgbWlncmF0ZWQgVHlwZVNjcmlwdCBwcm9qZWN0LlxuICAgICAgaWYgKCF0aGlzLl9hbmFseXplZEZpbGVzLmhhcyhyZXNvbHZlZFBhdGgpKSB7XG4gICAgICAgIHZpc2l0Tm9kZUFuZENvbGxlY3RSZXNvdXJjZXMoc291cmNlRmlsZSk7XG4gICAgICAgIHRoaXMuX2FuYWx5emVkRmlsZXMuYWRkKHJlc29sdmVkUGF0aCk7XG4gICAgICB9XG4gICAgfSk7XG5cbiAgICAvLyBXYWxrIHRocm91Z2ggYWxsIHJlc29sdmVkIHRlbXBsYXRlcyBhbmQgdmlzaXQgdGhlbSBpbiBlYWNoIGluc3RhbnRpYXRlZFxuICAgIC8vIG1pZ3JhdGlvbi4gTm90ZSB0aGF0IHRoaXMgY2FuIG9ubHkgaGFwcGVuIGFmdGVyIHNvdXJjZSBmaWxlcyBoYXZlIGJlZW5cbiAgICAvLyB2aXNpdGVkIGJlY2F1c2Ugd2UgZmluZCB0ZW1wbGF0ZXMgdGhyb3VnaCB0aGUgVHlwZVNjcmlwdCBzb3VyY2UgZmlsZXMuXG4gICAgcmVzb3VyY2VDb2xsZWN0b3IucmVzb2x2ZWRUZW1wbGF0ZXMuZm9yRWFjaCh0ZW1wbGF0ZSA9PiB7XG4gICAgICAvLyBEbyBub3QgdmlzaXQgdGhlIHRlbXBsYXRlIGlmIGl0IGhhcyBiZWVuIGNoZWNrZWQgYmVmb3JlLiBJbmxpbmVcbiAgICAgIC8vIHRlbXBsYXRlcyBjYW5ub3QgYmUgcmVmZXJlbmNlZCBtdWx0aXBsZSB0aW1lcy5cbiAgICAgIGlmICh0ZW1wbGF0ZS5pbmxpbmUgfHwgIXRoaXMuX2FuYWx5emVkRmlsZXMuaGFzKHRlbXBsYXRlLmZpbGVQYXRoKSkge1xuICAgICAgICBtaWdyYXRpb25zLmZvckVhY2gobSA9PiBtLnZpc2l0VGVtcGxhdGUodGVtcGxhdGUpKTtcbiAgICAgICAgdGhpcy5fYW5hbHl6ZWRGaWxlcy5hZGQodGVtcGxhdGUuZmlsZVBhdGgpO1xuICAgICAgfVxuICAgIH0pO1xuXG4gICAgLy8gV2FsayB0aHJvdWdoIGFsbCByZXNvbHZlZCBzdHlsZXNoZWV0cyBhbmQgdmlzaXQgdGhlbSBpbiBlYWNoIGluc3RhbnRpYXRlZFxuICAgIC8vIG1pZ3JhdGlvbi4gTm90ZSB0aGF0IHRoaXMgY2FuIG9ubHkgaGFwcGVuIGFmdGVyIHNvdXJjZSBmaWxlcyBoYXZlIGJlZW5cbiAgICAvLyB2aXNpdGVkIGJlY2F1c2Ugd2UgZmluZCBzdHlsZXNoZWV0cyB0aHJvdWdoIHRoZSBUeXBlU2NyaXB0IHNvdXJjZSBmaWxlcy5cbiAgICByZXNvdXJjZUNvbGxlY3Rvci5yZXNvbHZlZFN0eWxlc2hlZXRzLmZvckVhY2goc3R5bGVzaGVldCA9PiB7XG4gICAgICAvLyBEbyBub3QgdmlzaXQgdGhlIHN0eWxlc2hlZXQgaWYgaXQgaGFzIGJlZW4gY2hlY2tlZCBiZWZvcmUuIElubGluZVxuICAgICAgLy8gc3R5bGVzaGVldHMgY2Fubm90IGJlIHJlZmVyZW5jZWQgbXVsdGlwbGUgdGltZXMuXG4gICAgICBpZiAoc3R5bGVzaGVldC5pbmxpbmUgfHwgIXRoaXMuX2FuYWx5emVkRmlsZXMuaGFzKHN0eWxlc2hlZXQuZmlsZVBhdGgpKSB7XG4gICAgICAgIG1pZ3JhdGlvbnMuZm9yRWFjaChyID0+IHIudmlzaXRTdHlsZXNoZWV0KHN0eWxlc2hlZXQpKTtcbiAgICAgICAgdGhpcy5fYW5hbHl6ZWRGaWxlcy5hZGQoc3R5bGVzaGVldC5maWxlUGF0aCk7XG4gICAgICB9XG4gICAgfSk7XG5cbiAgICAvLyBJbiBzb21lIGFwcGxpY2F0aW9ucywgZGV2ZWxvcGVycyB3aWxsIGhhdmUgZ2xvYmFsIHN0eWxlc2hlZXRzIHdoaWNoIGFyZSBub3RcbiAgICAvLyBzcGVjaWZpZWQgaW4gYW55IEFuZ3VsYXIgY29tcG9uZW50LiBUaGVyZWZvcmUgd2UgYWxsb3cgZm9yIGFkZGl0aW9uYWwgc3R5bGVzaGVldHNcbiAgICAvLyBiZWluZyBzcGVjaWZpZWQuIFdlIHZpc2l0IHRoZW0gaW4gZWFjaCBtaWdyYXRpb24gdW5sZXNzIHRoZXkgaGF2ZSBiZWVuIGFscmVhZHlcbiAgICAvLyBkaXNjb3ZlcmVkIGJlZm9yZSBhcyBhY3R1YWwgY29tcG9uZW50IHJlc291cmNlLlxuICAgIGFkZGl0aW9uYWxTdHlsZXNoZWV0UGF0aHMuZm9yRWFjaChmaWxlUGF0aCA9PiB7XG4gICAgICBjb25zdCByZXNvbHZlZFBhdGggPSB0aGlzLl9maWxlU3lzdGVtLnJlc29sdmUoZmlsZVBhdGgpO1xuICAgICAgY29uc3Qgc3R5bGVzaGVldCA9IHJlc291cmNlQ29sbGVjdG9yLnJlc29sdmVFeHRlcm5hbFN0eWxlc2hlZXQocmVzb2x2ZWRQYXRoLCBudWxsKTtcbiAgICAgIC8vIERvIG5vdCB2aXNpdCBzdHlsZXNoZWV0cyB3aGljaCBoYXZlIGJlZW4gcmVmZXJlbmNlZCBmcm9tIGEgY29tcG9uZW50LlxuICAgICAgaWYgKCF0aGlzLl9hbmFseXplZEZpbGVzLmhhcyhyZXNvbHZlZFBhdGgpKSB7XG4gICAgICAgIG1pZ3JhdGlvbnMuZm9yRWFjaChyID0+IHIudmlzaXRTdHlsZXNoZWV0KHN0eWxlc2hlZXQpKTtcbiAgICAgICAgdGhpcy5fYW5hbHl6ZWRGaWxlcy5hZGQocmVzb2x2ZWRQYXRoKTtcbiAgICAgIH1cbiAgICB9KTtcblxuICAgIC8vIENhbGwgdGhlIFwicG9zdEFuYWx5c2lzXCIgbWV0aG9kIGZvciBlYWNoIG1pZ3JhdGlvbi5cbiAgICBtaWdyYXRpb25zLmZvckVhY2gociA9PiByLnBvc3RBbmFseXNpcygpKTtcblxuICAgIC8vIENvbGxlY3QgYWxsIGZhaWx1cmVzIHJlcG9ydGVkIGJ5IGluZGl2aWR1YWwgbWlncmF0aW9ucy5cbiAgICBjb25zdCBmYWlsdXJlcyA9IG1pZ3JhdGlvbnMucmVkdWNlKChyZXMsIG0pID0+XG4gICAgICAgIHJlcy5jb25jYXQobS5mYWlsdXJlcyksIFtdIGFzIE1pZ3JhdGlvbkZhaWx1cmVbXSk7XG5cbiAgICAvLyBJbiBjYXNlIHRoZXJlIGFyZSBmYWlsdXJlcywgcHJpbnQgdGhlc2UgdG8gdGhlIENMSSBsb2dnZXIgYXMgd2FybmluZ3MuXG4gICAgaWYgKGZhaWx1cmVzLmxlbmd0aCkge1xuICAgICAgZmFpbHVyZXMuZm9yRWFjaCgoe2ZpbGVQYXRoLCBtZXNzYWdlLCBwb3NpdGlvbn0pID0+IHtcbiAgICAgICAgY29uc3QgbGluZUFuZENoYXJhY3RlciA9IHBvc2l0aW9uID8gYEAke3Bvc2l0aW9uLmxpbmUgKyAxfToke3Bvc2l0aW9uLmNoYXJhY3RlciArIDF9YCA6ICcnO1xuICAgICAgICB0aGlzLl9sb2dnZXIud2FybihgJHtmaWxlUGF0aH0ke2xpbmVBbmRDaGFyYWN0ZXJ9IC0gJHttZXNzYWdlfWApO1xuICAgICAgfSk7XG4gICAgfVxuXG4gICAgcmV0dXJuIHtcbiAgICAgIGhhc0ZhaWx1cmVzOiAhIWZhaWx1cmVzLmxlbmd0aCxcbiAgICB9O1xuICB9XG5cbiAgLyoqXG4gICAqIENyZWF0ZXMgaW5zdGFuY2VzIG9mIHRoZSBnaXZlbiBtaWdyYXRpb25zIHdpdGggdGhlIHNwZWNpZmllZCB0YXJnZXRcbiAgICogdmVyc2lvbiBhbmQgZGF0YS5cbiAgICovXG4gIHByaXZhdGUgX2NyZWF0ZU1pZ3JhdGlvbnM8RGF0YT4odHlwZXM6IE1pZ3JhdGlvbkN0b3I8RGF0YSwgQ29udGV4dD5bXSwgdGFyZ2V0OiBUYXJnZXRWZXJzaW9uLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IERhdGEpOiBNaWdyYXRpb248RGF0YSwgQ29udGV4dD5bXSB7XG4gICAgY29uc3QgcmVzdWx0OiBNaWdyYXRpb248RGF0YSwgQ29udGV4dD5bXSA9IFtdO1xuICAgIGZvciAoY29uc3QgY3RvciBvZiB0eXBlcykge1xuICAgICAgY29uc3QgaW5zdGFuY2UgPSBuZXcgY3Rvcih0aGlzLl9wcm9ncmFtLCB0aGlzLl90eXBlQ2hlY2tlciwgdGFyZ2V0LCB0aGlzLl9jb250ZXh0LFxuICAgICAgICBkYXRhLCB0aGlzLl9maWxlU3lzdGVtLCB0aGlzLl9sb2dnZXIpO1xuICAgICAgaW5zdGFuY2UuaW5pdCgpO1xuICAgICAgaWYgKGluc3RhbmNlLmVuYWJsZWQpIHtcbiAgICAgICAgcmVzdWx0LnB1c2goaW5zdGFuY2UpO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gcmVzdWx0O1xuICB9XG5cbiAgLyoqXG4gICAqIENyZWF0ZXMgYSBwcm9ncmFtIGZvcm0gdGhlIHNwZWNpZmllZCB0c2NvbmZpZyBhbmQgcGF0Y2hlcyB0aGUgaG9zdFxuICAgKiB0byByZWFkIGZpbGVzIHRocm91Z2ggdGhlIGdpdmVuIGZpbGUgc3lzdGVtLlxuICAgKi9cbiAgc3RhdGljIGNyZWF0ZVByb2dyYW1Gcm9tVHNjb25maWcodHNjb25maWdGc1BhdGg6IHN0cmluZywgZnM6IEZpbGVTeXN0ZW0pOiB0cy5Qcm9ncmFtIHtcbiAgICBjb25zdCBwYXJzZWQgPSBwYXJzZVRzY29uZmlnRmlsZSh0c2NvbmZpZ0ZzUGF0aCwgZGlybmFtZSh0c2NvbmZpZ0ZzUGF0aCkpO1xuICAgIGNvbnN0IGhvc3QgPSB0cy5jcmVhdGVDb21waWxlckhvc3QocGFyc2VkLm9wdGlvbnMsIHRydWUpO1xuICAgIC8vIFBhdGNoIHRoZSBob3N0IHRvIHJlYWQgZmlsZXMgdGhyb3VnaCB0aGUgc3BlY2lmaWVkIGZpbGUgc3lzdGVtLlxuICAgIGhvc3QucmVhZEZpbGUgPSBmaWxlTmFtZSA9PiB7XG4gICAgICBjb25zdCBmaWxlQ29udGVudCA9IGZzLnJlYWQoZnMucmVzb2x2ZShmaWxlTmFtZSkpO1xuICAgICAgLy8gU3RyaXAgQk9NIGFzIG90aGVyd2lzZSBUU0MgbWV0aG9kcyAoZS5nLiBcImdldFdpZHRoXCIpIHdpbGwgcmV0dXJuIGFuIG9mZnNldCB3aGljaFxuICAgICAgLy8gd2hpY2ggYnJlYWtzIHRoZSBDTEkgVXBkYXRlUmVjb3JkZXIuIGh0dHBzOi8vZ2l0aHViLmNvbS9hbmd1bGFyL2FuZ3VsYXIvcHVsbC8zMDcxOVxuICAgICAgcmV0dXJuIGZpbGVDb250ZW50ICE9PSBudWxsID8gZmlsZUNvbnRlbnQucmVwbGFjZSgvXlxcdUZFRkYvLCAnJykgOiB1bmRlZmluZWQ7XG4gICAgfTtcbiAgICByZXR1cm4gdHMuY3JlYXRlUHJvZ3JhbShwYXJzZWQuZmlsZU5hbWVzLCBwYXJzZWQub3B0aW9ucywgaG9zdCk7XG4gIH1cbn1cbiJdfQ==