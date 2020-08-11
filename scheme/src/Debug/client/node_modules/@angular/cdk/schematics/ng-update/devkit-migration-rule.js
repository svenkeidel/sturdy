"use strict";
/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.isDevkitMigration = exports.createMigrationSchematicRule = exports.cdkMigrations = void 0;
const tasks_1 = require("@angular-devkit/schematics/tasks");
const glob_1 = require("glob");
const path_1 = require("path");
const update_tool_1 = require("../update-tool");
const project_tsconfig_paths_1 = require("../utils/project-tsconfig-paths");
const devkit_file_system_1 = require("./devkit-file-system");
const devkit_migration_1 = require("./devkit-migration");
const attribute_selectors_1 = require("./migrations/attribute-selectors");
const class_inheritance_1 = require("./migrations/class-inheritance");
const class_names_1 = require("./migrations/class-names");
const constructor_signature_1 = require("./migrations/constructor-signature");
const css_selectors_1 = require("./migrations/css-selectors");
const element_selectors_1 = require("./migrations/element-selectors");
const input_names_1 = require("./migrations/input-names");
const method_call_arguments_1 = require("./migrations/method-call-arguments");
const misc_template_1 = require("./migrations/misc-template");
const output_names_1 = require("./migrations/output-names");
const property_names_1 = require("./migrations/property-names");
/** List of migrations which run for the CDK update. */
exports.cdkMigrations = [
    attribute_selectors_1.AttributeSelectorsMigration,
    class_inheritance_1.ClassInheritanceMigration,
    class_names_1.ClassNamesMigration,
    constructor_signature_1.ConstructorSignatureMigration,
    css_selectors_1.CssSelectorsMigration,
    element_selectors_1.ElementSelectorsMigration,
    input_names_1.InputNamesMigration,
    method_call_arguments_1.MethodCallArgumentsMigration,
    misc_template_1.MiscTemplateMigration,
    output_names_1.OutputNamesMigration,
    property_names_1.PropertyNamesMigration,
];
/**
 * Creates a Angular schematic rule that runs the upgrade for the
 * specified target version.
 */
function createMigrationSchematicRule(targetVersion, extraMigrations, upgradeData, onMigrationCompleteFn) {
    return (tree, context) => __awaiter(this, void 0, void 0, function* () {
        const logger = context.logger;
        const workspace = project_tsconfig_paths_1.getWorkspaceConfigGracefully(tree);
        if (workspace === null) {
            logger.error('Could not find workspace configuration file.');
            return;
        }
        // Keep track of all project source files which have been checked/migrated. This is
        // necessary because multiple TypeScript projects can contain the same source file and
        // we don't want to check these again, as this would result in duplicated failure messages.
        const analyzedFiles = new Set();
        // The CLI uses the working directory as the base directory for the virtual file system tree.
        const workspaceFsPath = process.cwd();
        const fileSystem = new devkit_file_system_1.DevkitFileSystem(tree, workspaceFsPath);
        const projectNames = Object.keys(workspace.projects);
        const migrations = [...exports.cdkMigrations, ...extraMigrations];
        let hasFailures = false;
        for (const projectName of projectNames) {
            const project = workspace.projects[projectName];
            const buildTsconfigPath = project_tsconfig_paths_1.getTargetTsconfigPath(project, 'build');
            const testTsconfigPath = project_tsconfig_paths_1.getTargetTsconfigPath(project, 'test');
            if (!buildTsconfigPath && !testTsconfigPath) {
                logger.warn(`Could not find TypeScript project for project: ${projectName}`);
                continue;
            }
            if (buildTsconfigPath !== null) {
                runMigrations(project, projectName, buildTsconfigPath, false);
            }
            if (testTsconfigPath !== null) {
                runMigrations(project, projectName, testTsconfigPath, true);
            }
        }
        let runPackageManager = false;
        // Run the global post migration static members for all
        // registered devkit migrations.
        migrations.forEach(m => {
            const actionResult = isDevkitMigration(m) && m.globalPostMigration !== undefined ?
                m.globalPostMigration(tree, context) : null;
            if (actionResult) {
                runPackageManager = runPackageManager || actionResult.runPackageManager;
            }
        });
        // If a migration requested the package manager to run, we run it as an
        // asynchronous post migration task. We cannot run it synchronously,
        // as file changes from the current migration task are not applied to
        // the file system yet.
        if (runPackageManager) {
            context.addTask(new tasks_1.NodePackageInstallTask({ quiet: false }));
        }
        if (onMigrationCompleteFn) {
            onMigrationCompleteFn(context, targetVersion, hasFailures);
        }
        /** Runs the migrations for the specified workspace project. */
        function runMigrations(project, projectName, tsconfigPath, isTestTarget) {
            const projectRootFsPath = path_1.join(workspaceFsPath, project.root);
            const tsconfigFsPath = path_1.join(workspaceFsPath, tsconfigPath);
            const program = update_tool_1.UpdateProject.createProgramFromTsconfig(tsconfigFsPath, fileSystem);
            const updateContext = {
                workspaceFsPath,
                isTestTarget,
                projectName,
                project,
                tree,
            };
            const updateProject = new update_tool_1.UpdateProject(updateContext, program, fileSystem, analyzedFiles, context.logger);
            // In some applications, developers will have global stylesheets which are not
            // specified in any Angular component. Therefore we glob up all CSS and SCSS files
            // outside of node_modules and dist. The files will be read by the individual
            // stylesheet rules and checked.
            // TODO: rework this to collect global stylesheets from the workspace config. COMP-280.
            const additionalStylesheets = glob_1.sync('!(node_modules|dist)/**/*.+(css|scss)', { absolute: true, cwd: projectRootFsPath, nodir: true });
            const result = updateProject.migrate(migrations, targetVersion, upgradeData, additionalStylesheets);
            // Commit all recorded edits in the update recorder. We apply the edits after all
            // migrations ran because otherwise offsets in the TypeScript program would be
            // shifted and individual migrations could no longer update the same source file.
            fileSystem.commitEdits();
            hasFailures = hasFailures || result.hasFailures;
        }
    });
}
exports.createMigrationSchematicRule = createMigrationSchematicRule;
/** Whether the given migration type refers to a devkit migration */
function isDevkitMigration(value) {
    return devkit_migration_1.DevkitMigration.isPrototypeOf(value);
}
exports.isDevkitMigration = isDevkitMigration;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGV2a2l0LW1pZ3JhdGlvbi1ydWxlLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9zY2hlbWF0aWNzL25nLXVwZGF0ZS9kZXZraXQtbWlncmF0aW9uLXJ1bGUudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBOzs7Ozs7R0FNRzs7Ozs7Ozs7Ozs7O0FBR0gsNERBQXdFO0FBRXhFLCtCQUFzQztBQUN0QywrQkFBMEI7QUFFMUIsZ0RBQTZDO0FBSTdDLDRFQUFvRztBQUVwRyw2REFBc0Q7QUFDdEQseURBQXVGO0FBQ3ZGLDBFQUE2RTtBQUM3RSxzRUFBeUU7QUFDekUsMERBQTZEO0FBQzdELDhFQUFpRjtBQUNqRiw4REFBaUU7QUFDakUsc0VBQXlFO0FBQ3pFLDBEQUE2RDtBQUM3RCw4RUFBZ0Y7QUFDaEYsOERBQWlFO0FBQ2pFLDREQUErRDtBQUMvRCxnRUFBbUU7QUFJbkUsdURBQXVEO0FBQzFDLFFBQUEsYUFBYSxHQUFpQztJQUN6RCxpREFBMkI7SUFDM0IsNkNBQXlCO0lBQ3pCLGlDQUFtQjtJQUNuQixxREFBNkI7SUFDN0IscUNBQXFCO0lBQ3JCLDZDQUF5QjtJQUN6QixpQ0FBbUI7SUFDbkIsb0RBQTRCO0lBQzVCLHFDQUFxQjtJQUNyQixtQ0FBb0I7SUFDcEIsdUNBQXNCO0NBQ3ZCLENBQUM7QUFPRjs7O0dBR0c7QUFDSCxTQUFnQiw0QkFBNEIsQ0FDeEMsYUFBNEIsRUFBRSxlQUEwQyxFQUN4RSxXQUF3QixFQUFFLHFCQUF1QztJQUNuRSxPQUFPLENBQU8sSUFBVSxFQUFFLE9BQXlCLEVBQUUsRUFBRTtRQUNyRCxNQUFNLE1BQU0sR0FBRyxPQUFPLENBQUMsTUFBTSxDQUFDO1FBQzlCLE1BQU0sU0FBUyxHQUFHLHFEQUE0QixDQUFDLElBQUksQ0FBQyxDQUFDO1FBRXJELElBQUksU0FBUyxLQUFLLElBQUksRUFBRTtZQUN0QixNQUFNLENBQUMsS0FBSyxDQUFDLDhDQUE4QyxDQUFD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YUFBYSxHQUFrQjtnQkFDbkMsZUFBZTtnQkFDZixZQUFZO2dCQUNaLFdBQVc7Z0JBQ1gsT0FBTztnQkFDUCxJQUFJO2FBQ0wsQ0FBQztZQUVGLE1BQU0sYUFBYSxHQUFHLElBQUksMkJBQWEsQ0FDckMsYUFBYSxFQUNiLE9BQU8sRUFDUCxVQUFVLEVBQ1YsYUFBYSxFQUNiLE9BQU8sQ0FBQyxNQUFNLENBQ2YsQ0FBQztZQUVGLDhFQUE4RTtZQUM5RSxrRkFBa0Y7WUFDbEYsNkVBQTZFO1lBQzdFLGdDQUFnQztZQUNoQyx1RkFBdUY7WUFDdkYsTUFBTSxxQkFBcUIsR0FBRyxXQUFRLENBQ3BDLHVDQUF1QyxFQUN2QyxFQUFDLFFBQVEsRUFBRSxJQUFJLEVBQUUsR0FBRyxFQUFFLGlCQUFpQixFQUFFLEtBQUssRUFBRSxJQUFJLEVBQUMsQ0FBQyxDQUFDO1lBRXpELE1BQU0sTUFBTSxHQUNWLGFBQWEsQ0FBQyxPQUFPLENBQUMsVUFBVSxFQUFFLGFBQWEsRUFBRSxXQUFXLEVBQUUscUJBQXFCLENBQUMsQ0FBQztZQUV2RixpRkFBaUY7WUFDakYsOEVBQThFO1lBQzlFLGlGQUFpRjtZQUNqRixVQUFVLENBQUMsV0FBVyxFQUFFLENBQUM7WUFFekIsV0FBVyxHQUFHLFdBQVcsSUFBSSxNQUFNLENBQUMsV0FBVyxDQUFDO1FBQ2xELENBQUM7SUFDSCxDQUFDLENBQUEsQ0FBQztBQUNKLENBQUM7QUF6R0Qsb0VBeUdDO0FBRUQsb0VBQW9FO0FBQ3BFLFNBQWdCLGlCQUFpQixDQUFDLEtBQThCO0lBRTlELE9BQU8sa0NBQWUsQ0FBQyxhQUFhLENBQUMsS0FBSyxDQUFDLENBQUM7QUFDOUMsQ0FBQztBQUhELDhDQUdDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7UnVsZSwgU2NoZW1hdGljQ29udGV4dCwgVHJlZX0gZnJvbSAnQGFuZ3VsYXItZGV2a2l0L3NjaGVtYXRpY3MnO1xuaW1wb3J0IHtOb2RlUGFja2FnZUluc3RhbGxUYXNrfSBmcm9tICdAYW5ndWxhci1kZXZraXQvc2NoZW1hdGljcy90YXNrcyc7XG5pbXBvcnQge1dvcmtzcGFjZVByb2plY3R9IGZyb20gJ0BzY2hlbWF0aWNzL2FuZ3VsYXIvdXRpbGl0eS93b3Jrc3BhY2UtbW9kZWxzJztcbmltcG9ydCB7c3luYyBhcyBnbG9iU3luY30gZnJvbSAnZ2xvYic7XG5pbXBvcnQge2pvaW59IGZyb20gJ3BhdGgnO1xuXG5pbXBvcnQge1VwZGF0ZVByb2plY3R9IGZyb20gJy4uL3VwZGF0ZS10b29sJztcbmltcG9ydCB7TWlncmF0aW9uQ3Rvcn0gZnJvbSAnLi4vdXBkYXRlLXRvb2wvbWlncmF0aW9uJztcbmltcG9ydCB7VGFyZ2V0VmVyc2lvbn0gZnJvbSAnLi4vdXBkYXRlLXRvb2wvdGFyZ2V0LXZlcnNpb24nO1xuaW1wb3J0IHtXb3Jrc3BhY2VQYXRofSBmcm9tICcuLi91cGRhdGUtdG9vbC9maWxlLXN5c3RlbSc7XG5pbXBvcnQge2dldFRhcmdldFRzY29uZmlnUGF0aCwgZ2V0V29ya3NwYWNlQ29uZmlnR3JhY2VmdWxseX0gZnJvbSAnLi4vdXRpbHMvcHJvamVjdC10c2NvbmZpZy1wYXRocyc7XG5cbmltcG9ydCB7RGV2a2l0RmlsZVN5c3RlbX0gZnJvbSAnLi9kZXZraXQtZmlsZS1zeXN0ZW0nO1xuaW1wb3J0IHtEZXZraXRDb250ZXh0LCBEZXZraXRNaWdyYXRpb24sIERldmtpdE1pZ3JhdGlvbkN0b3J9IGZyb20gJy4vZGV2a2l0LW1pZ3JhdGlvbic7XG5pbXBvcnQge0F0dHJpYnV0ZVNlbGVjdG9yc01pZ3JhdGlvbn0gZnJvbSAnLi9taWdyYXRpb25zL2F0dHJpYnV0ZS1zZWxlY3RvcnMnO1xuaW1wb3J0IHtDbGFzc0luaGVyaXRhbmNlTWlncmF0aW9ufSBmcm9tICcuL21pZ3JhdGlvbnMvY2xhc3MtaW5oZXJpdGFuY2UnO1xuaW1wb3J0IHtDbGFzc05hbWVzTWlncmF0aW9ufSBmcm9tICcuL21pZ3JhdGlvbnMvY2xhc3MtbmFtZXMnO1xuaW1wb3J0IHtDb25zdHJ1Y3RvclNpZ25hdHVyZU1pZ3JhdGlvbn0gZnJvbSAnLi9taWdyYXRpb25zL2NvbnN0cnVjdG9yLXNpZ25hdHVyZSc7XG5pbXBvcnQge0Nzc1NlbGVjdG9yc01pZ3JhdGlvbn0gZnJvbSAnLi9taWdyYXRpb25zL2Nzcy1zZWxlY3RvcnMnO1xuaW1wb3J0IHtFbGVtZW50U2VsZWN0b3JzTWlncmF0aW9ufSBmcm9tICcuL21pZ3JhdGlvbnMvZWxlbWVudC1zZWxlY3RvcnMnO1xuaW1wb3J0IHtJbnB1dE5hbWVzTWlncmF0aW9ufSBmcm9tICcuL21pZ3JhdGlvbnMvaW5wdXQtbmFtZXMnO1xuaW1wb3J0IHtNZXRob2RDYWxsQXJndW1lbnRzTWlncmF0aW9ufSBmcm9tICcuL21pZ3JhdGlvbnMvbWV0aG9kLWNhbGwtYXJndW1lbnRzJztcbmltcG9ydCB7TWlzY1RlbXBsYXRlTWlncmF0aW9ufSBmcm9tICcuL21pZ3JhdGlvbnMvbWlzYy10ZW1wbGF0ZSc7XG5pbXBvcnQge091dHB1dE5hbWVzTWlncmF0aW9ufSBmcm9tICcuL21pZ3JhdGlvbnMvb3V0cHV0LW5hbWVzJztcbmltcG9ydCB7UHJvcGVydHlOYW1lc01pZ3JhdGlvbn0gZnJvbSAnLi9taWdyYXRpb25zL3Byb3BlcnR5LW5hbWVzJztcbmltcG9ydCB7VXBncmFkZURhdGF9IGZyb20gJy4vdXBncmFkZS1kYXRhJztcblxuXG4vKiogTGlzdCBvZiBtaWdyYXRpb25zIHdoaWNoIHJ1biBmb3IgdGhlIENESyB1cGRhdGUuICovXG5leHBvcnQgY29uc3QgY2RrTWlncmF0aW9uczogTWlncmF0aW9uQ3RvcjxVcGdyYWRlRGF0YT5bXSA9IFtcbiAgQXR0cmlidXRlU2VsZWN0b3JzTWlncmF0aW9uLFxuICBDbGFzc0luaGVyaXRhbmNlTWlncmF0aW9uLFxuICBDbGFzc05hbWVzTWlncmF0aW9uLFxuICBDb25zdHJ1Y3RvclNpZ25hdHVyZU1pZ3JhdGlvbixcbiAgQ3NzU2VsZWN0b3JzTWlncmF0aW9uLFxuICBFbGVtZW50U2VsZWN0b3JzTWlncmF0aW9uLFxuICBJbnB1dE5hbWVzTWlncmF0aW9uLFxuICBNZXRob2RDYWxsQXJndW1lbnRzTWlncmF0aW9uLFxuICBNaXNjVGVtcGxhdGVNaWdyYXRpb24sXG4gIE91dHB1dE5hbWVzTWlncmF0aW9uLFxuICBQcm9wZXJ0eU5hbWVzTWlncmF0aW9uLFxuXTtcblxuZXhwb3J0IHR5cGUgTnVsbGFibGVEZXZraXRNaWdyYXRpb24gPSBNaWdyYXRpb25DdG9yPFVwZ3JhZGVEYXRhfG51bGwsIERldmtpdENvbnRleHQ+O1xuXG50eXBlIFBvc3RNaWdyYXRpb25GbiA9XG4gICAgKGNvbnRleHQ6IFNjaGVtYXRpY0NvbnRleHQsIHRhcmdldFZlcnNpb246IFRhcmdldFZlcnNpb24sIGhhc0ZhaWx1cmU6IGJvb2xlYW4pID0+IHZvaWQ7XG5cbi8qKlxuICogQ3JlYXRlcyBhIEFuZ3VsYXIgc2NoZW1hdGljIHJ1bGUgdGhhdCBydW5zIHRoZSB1cGdyYWRlIGZvciB0aGVcbiAqIHNwZWNpZmllZCB0YXJnZXQgdmVyc2lvbi5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGNyZWF0ZU1pZ3JhdGlvblNjaGVtYXRpY1J1bGUoXG4gICAgdGFyZ2V0VmVyc2lvbjogVGFyZ2V0VmVyc2lvbiwgZXh0cmFNaWdyYXRpb25zOiBOdWxsYWJsZURldmtpdE1pZ3JhdGlvbltdLFxuICAgIHVwZ3JhZGVEYXRhOiBVcGdyYWRlRGF0YSwgb25NaWdyYXRpb25Db21wbGV0ZUZuPzogUG9zdE1pZ3JhdGlvbkZuKTogUnVsZSB7XG4gIHJldHVybiBhc3luYyAodHJlZTogVHJlZSwgY29udGV4dDogU2NoZW1hdGljQ29udGV4dCkgPT4ge1xuICAgIGNvbnN0IGxvZ2dlciA9IGNvbnRleHQubG9nZ2VyO1xuICAgIGNvbnN0IHdvcmtzcGFjZSA9IGdldFdvcmtzcGFjZUNvbmZpZ0dyYWNlZnVsbHkodHJlZSk7XG5cbiAgICBpZiAod29ya3NwYWNlID09PSBudWxsKSB7XG4gICAgICBsb2dnZXIuZXJyb3IoJ0NvdWxkIG5vdCBmaW5kIHdvcmtzcGFjZSBjb25maWd1cmF0aW9uIGZpbGUuJyk7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgLy8gS2VlcCB0cmFjayBvZiBhbGwgcHJvamVjdCBzb3VyY2UgZmlsZXMgd2hpY2ggaGF2ZSBiZWVuIGNoZWNrZWQvbWlncmF0ZWQuIFRoaXMgaXNcbiAgICAvLyBuZWNlc3NhcnkgYmVjYXVzZSBtdWx0aXBsZSBUeXBlU2NyaXB0IHByb2plY3RzIGNhbiBjb250YWluIHRoZSBzYW1lIHNvdXJjZSBmaWxlIGFuZFxuICAgIC8vIHdlIGRvbid0IHdhbnQgdG8gY2hlY2sgdGhlc2UgYWdhaW4sIGFzIHRoaXMgd291bGQgcmVzdWx0IGluIGR1cGxpY2F0ZWQgZmFpbHVyZSBtZXNzYWdlcy5cbiAgICBjb25zdCBhbmFseXplZEZpbGVzID0gbmV3IFNldDxXb3Jrc3BhY2VQYXRoPigpO1xuICAgIC8vIFRoZSBDTEkgdXNlcyB0aGUgd29ya2luZyBkaXJlY3RvcnkgYXMgdGhlIGJhc2UgZGlyZWN0b3J5IGZvciB0aGUgdmlydHVhbCBmaWxlIHN5c3RlbSB0cmVlLlxuICAgIGNvbnN0IHdvcmtzcGFjZUZzUGF0aCA9IHByb2Nlc3MuY3dkKCk7XG4gICAgY29uc3QgZmlsZVN5c3RlbSA9IG5ldyBEZXZraXRGaWxlU3lzdGVtKHRyZWUsIHdvcmtzcGFjZUZzUGF0aCk7XG4gICAgY29uc3QgcHJvamVjdE5hbWVzID0gT2JqZWN0LmtleXMod29ya3NwYWNlLnByb2plY3RzKTtcbiAgICBjb25zdCBtaWdyYXRpb25zOiBOdWxsYWJsZURldmtpdE1pZ3JhdGlvbltdID0gWy4uLmNka01pZ3JhdGlvbnMsIC4uLmV4dHJhTWlncmF0aW9uc107XG4gICAgbGV0IGhhc0ZhaWx1cmVzID0gZmFsc2U7XG5cbiAgICBmb3IgKGNvbnN0IHByb2plY3ROYW1lIG9mIHByb2plY3ROYW1lcykge1xuICAgICAgY29uc3QgcHJvamVjdCA9IHdvcmtzcGFjZS5wcm9qZWN0c1twcm9qZWN0TmFtZV07XG4gICAgICBjb25zdCBidWlsZFRzY29uZmlnUGF0aCA9IGdldFRhcmdldFRzY29uZmlnUGF0aChwcm9qZWN0LCAnYnVpbGQnKTtcbiAgICAgIGNvbnN0IHRlc3RUc2NvbmZpZ1BhdGggPSBnZXRUYXJnZXRUc2NvbmZpZ1BhdGgocHJvamVjdCwgJ3Rlc3QnKTtcblxuICAgICAgaWYgKCFidWlsZFRzY29uZmlnUGF0aCAmJiAhdGVzdFRzY29uZmlnUGF0aCkge1xuICAgICAgICBsb2dnZXIud2FybihgQ291bGQgbm90IGZpbmQgVHlwZVNjcmlwdCBwcm9qZWN0IGZvciBwcm9qZWN0OiAke3Byb2plY3ROYW1lfWApO1xuICAgICAgICBjb250aW51ZTtcbiAgICAgIH1cbiAgICAgIGlmIChidWlsZFRzY29uZmlnUGF0aCAhPT0gbnVsbCkge1xuICAgICAgICBydW5NaWdyYXRpb25zKHByb2plY3QsIHByb2plY3ROYW1lLCBidWlsZFRzY29uZmlnUGF0aCwgZmFsc2UpO1xuICAgICAgfVxuICAgICAgaWYgKHRlc3RUc2NvbmZpZ1BhdGggIT09IG51bGwpIHtcbiAgICAgICAgcnVuTWlncmF0aW9ucyhwcm9qZWN0LCBwcm9qZWN0TmFtZSwgdGVzdFRzY29uZmlnUGF0aCwgdHJ1ZSk7XG4gICAgICB9XG4gICAgfVxuXG4gICAgbGV0IHJ1blBhY2thZ2VNYW5hZ2VyID0gZmFsc2U7XG4gICAgLy8gUnVuIHRoZSBnbG9iYWwgcG9zdCBtaWdyYXRpb24gc3RhdGljIG1lbWJlcnMgZm9yIGFsbFxuICAgIC8vIHJlZ2lzdGVyZWQgZGV2a2l0IG1pZ3JhdGlvbnMuXG4gICAgbWlncmF0aW9ucy5mb3JFYWNoKG0gPT4ge1xuICAgICAgY29uc3QgYWN0aW9uUmVzdWx0ID0gaXNEZXZraXRNaWdyYXRpb24obSkgJiYgbS5nbG9iYWxQb3N0TWlncmF0aW9uICE9PSB1bmRlZmluZWQgP1xuICAgICAgICAgIG0uZ2xvYmFsUG9zdE1pZ3JhdGlvbih0cmVlLCBjb250ZXh0KSA6IG51bGw7XG4gICAgICBpZiAoYWN0aW9uUmVzdWx0KSB7XG4gICAgICAgIHJ1blBhY2thZ2VNYW5hZ2VyID0gcnVuUGFja2FnZU1hbmFnZXIgfHwgYWN0aW9uUmVzdWx0LnJ1blBhY2thZ2VNYW5hZ2VyO1xuICAgICAgfVxuICAgIH0pO1xuXG4gICAgLy8gSWYgYSBtaWdyYXRpb24gcmVxdWVzdGVkIHRoZSBwYWNrYWdlIG1hbmFnZXIgdG8gcnVuLCB3ZSBydW4gaXQgYXMgYW5cbiAgICAvLyBhc3luY2hyb25vdXMgcG9zdCBtaWdyYXRpb24gdGFzay4gV2UgY2Fubm90IHJ1biBpdCBzeW5jaHJvbm91c2x5LFxuICAgIC8vIGFzIGZpbGUgY2hhbmdlcyBmcm9tIHRoZSBjdXJyZW50IG1pZ3JhdGlvbiB0YXNrIGFyZSBub3QgYXBwbGllZCB0b1xuICAgIC8vIHRoZSBmaWxlIHN5c3RlbSB5ZXQuXG4gICAgaWYgKHJ1blBhY2thZ2VNYW5hZ2VyKSB7XG4gICAgICBjb250ZXh0LmFkZFRhc2sobmV3IE5vZGVQYWNrYWdlSW5zdGFsbFRhc2soe3F1aWV0OiBmYWxzZX0pKTtcbiAgICB9XG5cbiAgICBpZiAob25NaWdyYXRpb25Db21wbGV0ZUZuKSB7XG4gICAgICBvbk1pZ3JhdGlvbkNvbXBsZXRlRm4oY29udGV4dCwgdGFyZ2V0VmVyc2lvbiwgaGFzRmFpbHVyZXMpO1xuICAgIH1cblxuICAgIC8qKiBSdW5zIHRoZSBtaWdyYXRpb25zIGZvciB0aGUgc3BlY2lmaWVkIHdvcmtzcGFjZSBwcm9qZWN0LiAqL1xuICAgIGZ1bmN0aW9uIHJ1bk1pZ3JhdGlvbnMocHJvamVjdDogV29ya3NwYWNlUHJvamVjdCwgcHJvamVjdE5hbWU6IHN0cmluZyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIHRzY29uZmlnUGF0aDogc3RyaW5nLCBpc1Rlc3RUYXJnZXQ6IGJvb2xlYW4pIHtcbiAgICAgIGNvbnN0IHByb2plY3RSb290RnNQYXRoID0gam9pbih3b3Jrc3BhY2VGc1BhdGgsIHByb2plY3Qucm9vdCk7XG4gICAgICBjb25zdCB0c2NvbmZpZ0ZzUGF0aCA9IGpvaW4od29ya3NwYWNlRnNQYXRoLCB0c2NvbmZpZ1BhdGgpO1xuICAgICAgY29uc3QgcHJvZ3JhbSA9IFVwZGF0ZVByb2plY3QuY3JlYXRlUHJvZ3JhbUZyb21Uc2NvbmZpZyh0c2NvbmZpZ0ZzUGF0aCwgZmlsZVN5c3RlbSk7XG4gICAgICBjb25zdCB1cGRhdGVDb250ZXh0OiBEZXZraXRDb250ZXh0ID0ge1xuICAgICAgICB3b3Jrc3BhY2VGc1BhdGgsXG4gICAgICAgIGlzVGVzdFRhcmdldCxcbiAgICAgICAgcHJvamVjdE5hbWUsXG4gICAgICAgIHByb2plY3QsXG4gICAgICAgIHRyZWUsXG4gICAgICB9O1xuXG4gICAgICBjb25zdCB1cGRhdGVQcm9qZWN0ID0gbmV3IFVwZGF0ZVByb2plY3QoXG4gICAgICAgIHVwZGF0ZUNvbnRleHQsXG4gICAgICAgIHByb2dyYW0sXG4gICAgICAgIGZpbGVTeXN0ZW0sXG4gICAgICAgIGFuYWx5emVkRmlsZXMsXG4gICAgICAgIGNvbnRleHQubG9nZ2VyLFxuICAgICAgKTtcblxuICAgICAgLy8gSW4gc29tZSBhcHBsaWNhdGlvbnMsIGRldmVsb3BlcnMgd2lsbCBoYXZlIGdsb2JhbCBzdHlsZXNoZWV0cyB3aGljaCBhcmUgbm90XG4gICAgICAvLyBzcGVjaWZpZWQgaW4gYW55IEFuZ3VsYXIgY29tcG9uZW50LiBUaGVyZWZvcmUgd2UgZ2xvYiB1cCBhbGwgQ1NTIGFuZCBTQ1NTIGZpbGVzXG4gICAgICAvLyBvdXRzaWRlIG9mIG5vZGVfbW9kdWxlcyBhbmQgZGlzdC4gVGhlIGZpbGVzIHdpbGwgYmUgcmVhZCBieSB0aGUgaW5kaXZpZHVhbFxuICAgICAgLy8gc3R5bGVzaGVldCBydWxlcyBhbmQgY2hlY2tlZC5cbiAgICAgIC8vIFRPRE86IHJld29yayB0aGlzIHRvIGNvbGxlY3QgZ2xvYmFsIHN0eWxlc2hlZXRzIGZyb20gdGhlIHdvcmtzcGFjZSBjb25maWcuIENPTVAtMjgwLlxuICAgICAgY29uc3QgYWRkaXRpb25hbFN0eWxlc2hlZXRzID0gZ2xvYlN5bmMoXG4gICAgICAgICchKG5vZGVfbW9kdWxlc3xkaXN0KS8qKi8qLisoY3NzfHNjc3MpJyxcbiAgICAgICAge2Fic29sdXRlOiB0cnVlLCBjd2Q6IHByb2plY3RSb290RnNQYXRoLCBub2RpcjogdHJ1ZX0pO1xuXG4gICAgICBjb25zdCByZXN1bHQgPVxuICAgICAgICB1cGRhdGVQcm9qZWN0Lm1pZ3JhdGUobWlncmF0aW9ucywgdGFyZ2V0VmVyc2lvbiwgdXBncmFkZURhdGEsIGFkZGl0aW9uYWxTdHlsZXNoZWV0cyk7XG5cbiAgICAgIC8vIENvbW1pdCBhbGwgcmVjb3JkZWQgZWRpdHMgaW4gdGhlIHVwZGF0ZSByZWNvcmRlci4gV2UgYXBwbHkgdGhlIGVkaXRzIGFmdGVyIGFsbFxuICAgICAgLy8gbWlncmF0aW9ucyByYW4gYmVjYXVzZSBvdGhlcndpc2Ugb2Zmc2V0cyBpbiB0aGUgVHlwZVNjcmlwdCBwcm9ncmFtIHdvdWxkIGJlXG4gICAgICAvLyBzaGlmdGVkIGFuZCBpbmRpdmlkdWFsIG1pZ3JhdGlvbnMgY291bGQgbm8gbG9uZ2VyIHVwZGF0ZSB0aGUgc2FtZSBzb3VyY2UgZmlsZS5cbiAgICAgIGZpbGVTeXN0ZW0uY29tbWl0RWRpdHMoKTtcblxuICAgICAgaGFzRmFpbHVyZXMgPSBoYXNGYWlsdXJlcyB8fCByZXN1bHQuaGFzRmFpbHVyZXM7XG4gICAgfVxuICB9O1xufVxuXG4vKiogV2hldGhlciB0aGUgZ2l2ZW4gbWlncmF0aW9uIHR5cGUgcmVmZXJzIHRvIGEgZGV2a2l0IG1pZ3JhdGlvbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGlzRGV2a2l0TWlncmF0aW9uKHZhbHVlOiBNaWdyYXRpb25DdG9yPGFueSwgYW55PilcbiAgICA6IHZhbHVlIGlzIERldmtpdE1pZ3JhdGlvbkN0b3I8YW55PiB7XG4gIHJldHVybiBEZXZraXRNaWdyYXRpb24uaXNQcm90b3R5cGVPZih2YWx1ZSk7XG59XG4iXX0=