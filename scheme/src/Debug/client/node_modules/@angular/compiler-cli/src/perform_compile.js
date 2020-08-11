/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define("@angular/compiler-cli/src/perform_compile", ["require", "exports", "tslib", "@angular/compiler", "typescript", "@angular/compiler-cli/src/ngtsc/file_system", "@angular/compiler-cli/src/ngtsc/diagnostics", "@angular/compiler-cli/src/transformers/api", "@angular/compiler-cli/src/transformers/entry_points", "@angular/compiler-cli/src/transformers/util"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.defaultGatherDiagnostics = exports.performCompilation = exports.exitCodeFromResult = exports.readConfiguration = exports.createNgCompilerOptions = exports.calcProjectFileAndBasePath = exports.formatDiagnostics = exports.formatDiagnostic = exports.flattenDiagnosticMessageChain = exports.formatDiagnosticPosition = exports.filterErrorsAndWarnings = void 0;
    var tslib_1 = require("tslib");
    var compiler_1 = require("@angular/compiler");
    var ts = require("typescript");
    var file_system_1 = require("@angular/compiler-cli/src/ngtsc/file_system");
    var diagnostics_1 = require("@angular/compiler-cli/src/ngtsc/diagnostics");
    var api = require("@angular/compiler-cli/src/transformers/api");
    var ng = require("@angular/compiler-cli/src/transformers/entry_points");
    var util_1 = require("@angular/compiler-cli/src/transformers/util");
    function filterErrorsAndWarnings(diagnostics) {
        return diagnostics.filter(function (d) { return d.category !== ts.DiagnosticCategory.Message; });
    }
    exports.filterErrorsAndWarnings = filterErrorsAndWarnings;
    var defaultFormatHost = {
        getCurrentDirectory: function () { return ts.sys.getCurrentDirectory(); },
        getCanonicalFileName: function (fileName) { return fileName; },
        getNewLine: function () { return ts.sys.newLine; }
    };
    function displayFileName(fileName, host) {
        return file_system_1.relative(file_system_1.resolve(host.getCurrentDirectory()), file_system_1.resolve(host.getCanonicalFileName(fileName)));
    }
    function formatDiagnosticPosition(position, host) {
        if (host === void 0) { host = defaultFormatHost; }
        return displayFileName(position.fileName, host) + "(" + (position.line + 1) + "," + (position.column + 1) + ")";
    }
    exports.formatDiagnosticPosition = formatDiagnosticPosition;
    function flattenDiagnosticMessageChain(chain, host, indent) {
        var e_1, _a;
        if (host === void 0) { host = defaultFormatHost; }
        if (indent === void 0) { indent = 0; }
        var newLine = host.getNewLine();
        var result = '';
        if (indent) {
            result += newLine;
            for (var i = 0; i < indent; i++) {
                result += '  ';
            }
        }
        result += chain.messageText;
        var position = chain.position;
        // add position if available, and we are not at the depest frame
        if (position && indent !== 0) {
            result += " at " + formatDiagnosticPosition(position, host);
        }
        indent++;
        if (chain.next) {
            try {
                for (var _b = tslib_1.__values(chain.next), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var kid = _c.value;
                    result += flattenDiagnosticMessageChain(kid, host, indent);
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                }
                finally { if (e_1) throw e_1.error; }
            }
        }
        return result;
    }
    exports.flattenDiagnosticMessageChain = flattenDiagnosticMessageChain;
    function formatDiagnostic(diagnostic, host) {
        if (host === void 0) { host = defaultFormatHost; }
        var result = '';
        var newLine = host.getNewLine();
        var span = diagnostic.span;
        if (span) {
            result += formatDiagnosticPosition({ fileName: span.start.file.url, line: span.start.line, column: span.start.col }, host) + ": ";
        }
        else if (diagnostic.position) {
            result += formatDiagnosticPosition(diagnostic.position, host) + ": ";
        }
        if (diagnostic.span && diagnostic.span.details) {
            result += diagnostic.span.details + ", " + diagnostic.messageText + newLine;
        }
        else if (diagnostic.chain) {
            result += flattenDiagnosticMessageChain(diagnostic.chain, host) + "." + newLine;
        }
        else {
            result += "" + diagnostic.messageText + newLine;
        }
        return result;
    }
    exports.formatDiagnostic = formatDiagnostic;
    function formatDiagnostics(diags, host) {
        if (host === void 0) { host = defaultFormatHost; }
        if (diags && diags.length) {
            return diags
                .map(function (diagnostic) {
                if (api.isTsDiagnostic(diagnostic)) {
                    return diagnostics_1.replaceTsWithNgInErrors(ts.formatDiagnosticsWithColorAndContext([diagnostic], host));
                }
                else {
                    return formatDiagnostic(diagnostic, host);
                }
            })
                .join('');
        }
        else {
            return '';
        }
    }
    exports.formatDiagnostics = formatDiagnostics;
    function calcProjectFileAndBasePath(project) {
        var fs = file_system_1.getFileSystem();
        var absProject = fs.resolve(project);
        var projectIsDir = fs.lstat(absProject).isDirectory();
        var projectFile = projectIsDir ? fs.join(absProject, 'tsconfig.json') : absProject;
        var projectDir = projectIsDir ? absProject : fs.dirname(absProject);
        var basePath = fs.resolve(projectDir);
        return { projectFile: projectFile, basePath: basePath };
    }
    exports.calcProjectFileAndBasePath = calcProjectFileAndBasePath;
    function createNgCompilerOptions(basePath, config, tsOptions) {
        // enableIvy `ngtsc` is an alias for `true`.
        var _a = config.angularCompilerOptions, angularCompilerOptions = _a === void 0 ? {} : _a;
        var enableIvy = angularCompilerOptions.enableIvy;
        angularCompilerOptions.enableIvy = enableIvy !== false && enableIvy !== 'tsc';
        return tslib_1.__assign(tslib_1.__assign(tslib_1.__assign({}, tsOptions), angularCompilerOptions), { genDir: basePath, basePath: basePath });
    }
    exports.createNgCompilerOptions = createNgCompilerOptions;
    function readConfiguration(project, existingOptions) {
        try {
            var fs_1 = file_system_1.getFileSystem();
            var _a = calcProjectFileAndBasePath(project), projectFile = _a.projectFile, basePath = _a.basePath;
            var readExtendedConfigFile_1 = function (configFile, existingConfig) {
                var _a = ts.readConfigFile(configFile, ts.sys.readFile), config = _a.config, error = _a.error;
                if (error) {
                    return { error: error };
                }
                // we are only interested into merging 'angularCompilerOptions' as
                // other options like 'compilerOptions' are merged by TS
                var baseConfig = existingConfig || config;
                if (existingConfig) {
                    baseConfig.angularCompilerOptions = tslib_1.__assign(tslib_1.__assign({}, config.angularCompilerOptions), baseConfig.angularCompilerOptions);
                }
                if (config.extends) {
                    var extendedConfigPath = fs_1.resolve(fs_1.dirname(configFile), config.extends);
                    extendedConfigPath = fs_1.extname(extendedConfigPath) ?
                        extendedConfigPath :
                        file_system_1.absoluteFrom(extendedConfigPath + ".json");
                    if (fs_1.exists(extendedConfigPath)) {
                        // Call read config recursively as TypeScript only merges CompilerOptions
                        return readExtendedConfigFile_1(extendedConfigPath, baseConfig);
                    }
                }
                return { config: baseConfig };
            };
            var _b = readExtendedConfigFile_1(projectFile), config = _b.config, error = _b.error;
            if (error) {
                return {
                    project: project,
                    errors: [error],
                    rootNames: [],
                    options: {},
                    emitFlags: api.EmitFlags.Default
                };
            }
            var parseConfigHost = {
                useCaseSensitiveFileNames: true,
                fileExists: fs_1.exists.bind(fs_1),
                readDirectory: ts.sys.readDirectory,
                readFile: ts.sys.readFile
            };
            var configFileName = fs_1.resolve(fs_1.pwd(), projectFile);
            var parsed = ts.parseJsonConfigFileContent(config, parseConfigHost, basePath, existingOptions, configFileName);
            var rootNames = parsed.fileNames;
            var options = createNgCompilerOptions(basePath, config, parsed.options);
            var emitFlags = api.EmitFlags.Default;
            if (!(options.skipMetadataEmit || options.flatModuleOutFile)) {
                emitFlags |= api.EmitFlags.Metadata;
            }
            if (options.skipTemplateCodegen) {
                emitFlags = emitFlags & ~api.EmitFlags.Codegen;
            }
            return { project: projectFile, rootNames: rootNames, options: options, errors: parsed.errors, emitFlags: emitFlags };
        }
        catch (e) {
            var errors = [{
                    category: ts.DiagnosticCategory.Error,
                    messageText: e.stack,
                    source: api.SOURCE,
                    code: api.UNKNOWN_ERROR_CODE
                }];
            return { project: '', errors: errors, rootNames: [], options: {}, emitFlags: api.EmitFlags.Default };
        }
    }
    exports.readConfiguration = readConfiguration;
    function exitCodeFromResult(diags) {
        if (!diags || filterErrorsAndWarnings(diags).length === 0) {
            // If we have a result and didn't get any errors, we succeeded.
            return 0;
        }
        // Return 2 if any of the errors were unknown.
        return diags.some(function (d) { return d.source === 'angular' && d.code === api.UNKNOWN_ERROR_CODE; }) ? 2 : 1;
    }
    exports.exitCodeFromResult = exitCodeFromResult;
    function performCompilation(_a) {
        var rootNames = _a.rootNames, options = _a.options, host = _a.host, oldProgram = _a.oldProgram, emitCallback = _a.emitCallback, mergeEmitResultsCallback = _a.mergeEmitResultsCallback, _b = _a.gatherDiagnostics, gatherDiagnostics = _b === void 0 ? defaultGatherDiagnostics : _b, customTransformers = _a.customTransformers, _c = _a.emitFlags, emitFlags = _c === void 0 ? api.EmitFlags.Default : _c, _d = _a.modifiedResourceFiles, modifiedResourceFiles = _d === void 0 ? null : _d;
        var program;
        var emitResult;
        var allDiagnostics = [];
        try {
            if (!host) {
                host = ng.createCompilerHost({ options: options });
            }
            if (modifiedResourceFiles) {
                host.getModifiedResourceFiles = function () { return modifiedResourceFiles; };
            }
            program = ng.createProgram({ rootNames: rootNames, host: host, options: options, oldProgram: oldProgram });
            var beforeDiags = Date.now();
            allDiagnostics.push.apply(allDiagnostics, tslib_1.__spread(gatherDiagnostics(program)));
            if (options.diagnostics) {
                var afterDiags = Date.now();
                allDiagnostics.push(util_1.createMessageDiagnostic("Time for diagnostics: " + (afterDiags - beforeDiags) + "ms."));
            }
            if (!hasErrors(allDiagnostics)) {
                emitResult =
                    program.emit({ emitCallback: emitCallback, mergeEmitResultsCallback: mergeEmitResultsCallback, customTransformers: customTransformers, emitFlags: emitFlags });
                allDiagnostics.push.apply(allDiagnostics, tslib_1.__spread(emitResult.diagnostics));
                return { diagnostics: allDiagnostics, program: program, emitResult: emitResult };
            }
            return { diagnostics: allDiagnostics, program: program };
        }
        catch (e) {
            var errMsg = void 0;
            var code = void 0;
            if (compiler_1.isSyntaxError(e)) {
                // don't report the stack for syntax errors as they are well known errors.
                errMsg = e.message;
                code = api.DEFAULT_ERROR_CODE;
            }
            else {
                errMsg = e.stack;
                // It is not a syntax error we might have a program with unknown state, discard it.
                program = undefined;
                code = api.UNKNOWN_ERROR_CODE;
            }
            allDiagnostics.push({ category: ts.DiagnosticCategory.Error, messageText: errMsg, code: code, source: api.SOURCE });
            return { diagnostics: allDiagnostics, program: program };
        }
    }
    exports.performCompilation = performCompilation;
    function defaultGatherDiagnostics(program) {
        var allDiagnostics = [];
        function checkDiagnostics(diags) {
            if (diags) {
                allDiagnostics.push.apply(allDiagnostics, tslib_1.__spread(diags));
                return !hasErrors(diags);
            }
            return true;
        }
        var checkOtherDiagnostics = true;
        // Check parameter diagnostics
        checkOtherDiagnostics = checkOtherDiagnostics &&
            checkDiagnostics(tslib_1.__spread(program.getTsOptionDiagnostics(), program.getNgOptionDiagnostics()));
        // Check syntactic diagnostics
        checkOtherDiagnostics =
            checkOtherDiagnostics && checkDiagnostics(program.getTsSyntacticDiagnostics());
        // Check TypeScript semantic and Angular structure diagnostics
        checkOtherDiagnostics =
            checkOtherDiagnostics &&
                checkDiagnostics(tslib_1.__spread(program.getTsSemanticDiagnostics(), program.getNgStructuralDiagnostics()));
        // Check Angular semantic diagnostics
        checkOtherDiagnostics =
            checkOtherDiagnostics && checkDiagnostics(program.getNgSemanticDiagnostics());
        return allDiagnostics;
    }
    exports.defaultGatherDiagnostics = defaultGatherDiagnostics;
    function hasErrors(diags) {
        return diags.some(function (d) { return d.category === ts.DiagnosticCategory.Error; });
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGVyZm9ybV9jb21waWxlLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vcGFja2FnZXMvY29tcGlsZXItY2xpL3NyYy9wZXJmb3JtX2NvbXBpbGUudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOzs7Ozs7Ozs7Ozs7OztJQUVILDhDQUEwRDtJQUMxRCwrQkFBaUM7SUFFakMsMkVBQXdHO0lBRXhHLDJFQUE0RDtJQUM1RCxnRUFBMEM7SUFDMUMsd0VBQWtEO0lBQ2xELG9FQUE0RDtJQUk1RCxTQUFnQix1QkFBdUIsQ0FBQyxXQUF3QjtRQUM5RCxPQUFPLFdBQVcsQ0FBQyxNQUFNLENBQUMsVUFBQSxDQUFDLElBQUksT0FBQSxDQUFDLENBQUMsUUFBUSxLQUFLLEVBQUUsQ0FBQyxrQkFBa0IsQ0FBQyxPQUFPLEVBQTVDLENBQTRDLENBQUMsQ0FBQztJQUMvRSxDQUFDO0lBRkQsMERBRUM7SUFFRCxJQUFNLGlCQUFpQixHQUE2QjtRQUNsRCxtQkFBbUIsRUFBRSxjQUFNLE9BQUEsRUFBRSxDQUFDLEdBQUcsQ0FBQyxtQkFBbUIsRUFBRSxFQUE1QixDQUE0QjtRQUN2RCxvQkFBb0IsRUFBRSxVQUFBLFFBQVEsSUFBSSxPQUFBLFFBQVEsRUFBUixDQUFRO1FBQzFDLFVBQVUsRUFBRSxjQUFNLE9BQUEsRUFBRSxDQUFDLEdBQUcsQ0FBQyxPQUFPLEVBQWQsQ0FBYztLQUNqQyxDQUFDO0lBRUYsU0FBUyxlQUFlLENBQUMsUUFBZ0IsRUFBRSxJQUE4QjtRQUN2RSxPQUFPLHNCQUFRLENBQ1gscUJBQU8sQ0FBQyxJQUFJLENBQUMsbUJBQW1CLEVBQUUsQ0FBQyxFQUFFLHFCQUFPLENBQUMsSUFBSSxDQUFDLG9CQUFvQixDQUFDLFFBQVEsQ0FBQyxDQUFDLENBQUMsQ0FBQztJQUN6RixDQUFDO0lBRUQsU0FBZ0Isd0JBQXdCLENBQ3BDLFFBQWtCLEVBQUUsSUFBa0Q7UUFBbEQscUJBQUEsRUFBQSx3QkFB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QUFHLElBQUksQ0FBQztRQUNqQyw4QkFBOEI7UUFDOUIscUJBQXFCLEdBQUcscUJBQXFCO1lBQ3pDLGdCQUFnQixrQkFBSyxPQUFPLENBQUMsc0JBQXNCLEVBQUUsRUFBSyxPQUFPLENBQUMsc0JBQXNCLEVBQUUsRUFBRSxDQUFDO1FBRWpHLDhCQUE4QjtRQUM5QixxQkFBcUI7WUFDakIscUJBQXFCLElBQUksZ0JBQWdCLENBQUMsT0FBTyxDQUFDLHlCQUF5QixFQUFpQixDQUFDLENBQUM7UUFFbEcsOERBQThEO1FBQzlELHFCQUFxQjtZQUNqQixxQkFBcUI7Z0JBQ3JCLGdCQUFnQixrQkFDUixPQUFPLENBQUMsd0JBQXdCLEVBQUUsRUFBSyxPQUFPLENBQUMsMEJBQTBCLEVBQUUsRUFBRSxDQUFDO1FBRTFGLHFDQUFxQztRQUNyQyxxQkFBcUI7WUFDakIscUJBQXFCLElBQUksZ0JBQWdCLENBQUMsT0FBTyxDQUFDLHdCQUF3QixFQUFpQixDQUFDLENBQUM7UUFFakcsT0FBTyxjQUFjLENBQUM7SUFDeEIsQ0FBQztJQS9CRCw0REErQkM7SUFFRCxTQUFTLFNBQVMsQ0FBQyxLQUFrQjtRQUNuQyxPQUFPLEtBQUssQ0FBQyxJQUFJLENBQUMsVUFBQSxDQUFDLElBQUksT0FBQSxDQUFDLENBQUMsUUFBUSxLQUFLLEVBQUUsQ0FBQyxrQkFBa0IsQ0FBQyxLQUFLLEVBQTFDLENBQTBDLENBQUMsQ0FBQztJQUNyRSxDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7aXNTeW50YXhFcnJvciwgUG9zaXRpb259IGZyb20gJ0Bhbmd1bGFyL2NvbXBpbGVyJztcbmltcG9ydCAqIGFzIHRzIGZyb20gJ3R5cGVzY3JpcHQnO1xuXG5pbXBvcnQge2Fic29sdXRlRnJvbSwgQWJzb2x1dGVGc1BhdGgsIGdldEZpbGVTeXN0ZW0sIHJlbGF0aXZlLCByZXNvbHZlfSBmcm9tICcuLi9zcmMvbmd0c2MvZmlsZV9zeXN0ZW0nO1xuXG5pbXBvcnQge3JlcGxhY2VUc1dpdGhOZ0luRXJyb3JzfSBmcm9tICcuL25ndHNjL2RpYWdub3N0aWNzJztcbmltcG9ydCAqIGFzIGFwaSBmcm9tICcuL3RyYW5zZm9ybWVycy9hcGknO1xuaW1wb3J0ICogYXMgbmcgZnJvbSAnLi90cmFuc2Zvcm1lcnMvZW50cnlfcG9pbnRzJztcbmltcG9ydCB7Y3JlYXRlTWVzc2FnZURpYWdub3N0aWN9IGZyb20gJy4vdHJhbnNmb3JtZXJzL3V0aWwnO1xuXG5leHBvcnQgdHlwZSBEaWFnbm9zdGljcyA9IFJlYWRvbmx5QXJyYXk8dHMuRGlhZ25vc3RpY3xhcGkuRGlhZ25vc3RpYz47XG5cbmV4cG9ydCBmdW5jdGlvbiBmaWx0ZXJFcnJvcnNBbmRXYXJuaW5ncyhkaWFnbm9zdGljczogRGlhZ25vc3RpY3MpOiBEaWFnbm9zdGljcyB7XG4gIHJldHVybiBkaWFnbm9zdGljcy5maWx0ZXIoZCA9PiBkLmNhdGVnb3J5ICE9PSB0cy5EaWFnbm9zdGljQ2F0ZWdvcnkuTWVzc2FnZSk7XG59XG5cbmNvbnN0IGRlZmF1bHRGb3JtYXRIb3N0OiB0cy5Gb3JtYXREaWFnbm9zdGljc0hvc3QgPSB7XG4gIGdldEN1cnJlbnREaXJlY3Rvcnk6ICgpID0+IHRzLnN5cy5nZXRDdXJyZW50RGlyZWN0b3J5KCksXG4gIGdldENhbm9uaWNhbEZpbGVOYW1lOiBmaWxlTmFtZSA9PiBmaWxlTmFtZSxcbiAgZ2V0TmV3TGluZTogKCkgPT4gdHMuc3lzLm5ld0xpbmVcbn07XG5cbmZ1bmN0aW9uIGRpc3BsYXlGaWxlTmFtZShmaWxlTmFtZTogc3RyaW5nLCBob3N0OiB0cy5Gb3JtYXREaWFnbm9zdGljc0hvc3QpOiBzdHJpbmcge1xuICByZXR1cm4gcmVsYXRpdmUoXG4gICAgICByZXNvbHZlKGhvc3QuZ2V0Q3VycmVudERpcmVjdG9yeSgpKSwgcmVzb2x2ZShob3N0LmdldENhbm9uaWNhbEZpbGVOYW1lKGZpbGVOYW1lKSkpO1xufVxuXG5leHBvcnQgZnVuY3Rpb24gZm9ybWF0RGlhZ25vc3RpY1Bvc2l0aW9uKFxuICAgIHBvc2l0aW9uOiBQb3NpdGlvbiwgaG9zdDogdHMuRm9ybWF0RGlhZ25vc3RpY3NIb3N0ID0gZGVmYXVsdEZvcm1hdEhvc3QpOiBzdHJpbmcge1xuICByZXR1cm4gYCR7ZGlzcGxheUZpbGVOYW1lKHBvc2l0aW9uLmZpbGVOYW1lLCBob3N0KX0oJHtwb3NpdGlvbi5saW5lICsgMX0sJHtwb3NpdGlvbi5jb2x1bW4gKyAxfSlgO1xufVxuXG5leHBvcnQgZnVuY3Rpb24gZmxhdHRlbkRpYWdub3N0aWNNZXNzYWdlQ2hhaW4oXG4gICAgY2hhaW46IGFwaS5EaWFnbm9zdGljTWVzc2FnZUNoYWluLCBob3N0OiB0cy5Gb3JtYXREaWFnbm9zdGljc0hvc3QgPSBkZWZhdWx0Rm9ybWF0SG9zdCxcbiAgICBpbmRlbnQgPSAwKTogc3RyaW5nIHtcbiAgY29uc3QgbmV3TGluZSA9IGhvc3QuZ2V0TmV3TGluZSgpO1xuICBsZXQgcmVzdWx0ID0gJyc7XG4gIGlmIChpbmRlbnQpIHtcbiAgICByZXN1bHQgKz0gbmV3TGluZTtcblxuICAgIGZvciAobGV0IGkgPSAwOyBpIDwgaW5kZW50OyBpKyspIHtcbiAgICAgIHJlc3VsdCArPSAnICAnO1xuICAgIH1cbiAgfVxuICByZXN1bHQgKz0gY2hhaW4ubWVzc2FnZVRleHQ7XG5cbiAgY29uc3QgcG9zaXRpb24gPSBjaGFpbi5wb3NpdGlvbjtcbiAgLy8gYWRkIHBvc2l0aW9uIGlmIGF2YWlsYWJsZSwgYW5kIHdlIGFyZSBub3QgYXQgdGhlIGRlcGVzdCBmcmFtZVxuICBpZiAocG9zaXRpb24gJiYgaW5kZW50ICE9PSAwKSB7XG4gICAgcmVzdWx0ICs9IGAgYXQgJHtmb3JtYXREaWFnbm9zdGljUG9zaXRpb24ocG9zaXRpb24sIGhvc3QpfWA7XG4gIH1cblxuICBpbmRlbnQrKztcbiAgaWYgKGNoYWluLm5leHQpIHtcbiAgICBmb3IgKGNvbnN0IGtpZCBvZiBjaGFpbi5uZXh0KSB7XG4gICAgICByZXN1bHQgKz0gZmxhdHRlbkRpYWdub3N0aWNNZXNzYWdlQ2hhaW4oa2lkLCBob3N0LCBpbmRlbnQpO1xuICAgIH1cbiAgfVxuICByZXR1cm4gcmVzdWx0O1xufVxuXG5leHBvcnQgZnVuY3Rpb24gZm9ybWF0RGlhZ25vc3RpYyhcbiAgICBkaWFnbm9zdGljOiBhcGkuRGlhZ25vc3RpYywgaG9zdDogdHMuRm9ybWF0RGlhZ25vc3RpY3NIb3N0ID0gZGVmYXVsdEZvcm1hdEhvc3QpIHtcbiAgbGV0IHJlc3VsdCA9ICcnO1xuICBjb25zdCBuZXdMaW5lID0gaG9zdC5nZXROZXdMaW5lKCk7XG4gIGNvbnN0IHNwYW4gPSBkaWFnbm9zdGljLnNwYW47XG4gIGlmIChzcGFuKSB7XG4gICAgcmVzdWx0ICs9IGAke1xuICAgICAgICBmb3JtYXREaWFnbm9zdGljUG9zaXRpb24oXG4gICAgICAgICAgICB7ZmlsZU5hbWU6IHNwYW4uc3RhcnQuZmlsZS51cmwsIGxpbmU6IHNwYW4uc3RhcnQubGluZSwgY29sdW1uOiBzcGFuLnN0YXJ0LmNvbH0sXG4gICAgICAgICAgICBob3N0KX06IGA7XG4gIH0gZWxzZSBpZiAoZGlhZ25vc3RpYy5wb3NpdGlvbikge1xuICAgIHJlc3VsdCArPSBgJHtmb3JtYXREaWFnbm9zdGljUG9zaXRpb24oZGlhZ25vc3RpYy5wb3NpdGlvbiwgaG9zdCl9OiBgO1xuICB9XG4gIGlmIChkaWFnbm9zdGljLnNwYW4gJiYgZGlhZ25vc3RpYy5zcGFuLmRldGFpbHMpIHtcbiAgICByZXN1bHQgKz0gYCR7ZGlhZ25vc3RpYy5zcGFuLmRldGFpbHN9LCAke2RpYWdub3N0aWMubWVzc2FnZVRleHR9JHtuZXdMaW5lfWA7XG4gIH0gZWxzZSBpZiAoZGlhZ25vc3RpYy5jaGFpbikge1xuICAgIHJlc3VsdCArPSBgJHtmbGF0dGVuRGlhZ25vc3RpY01lc3NhZ2VDaGFpbihkaWFnbm9zdGljLmNoYWluLCBob3N0KX0uJHtuZXdMaW5lfWA7XG4gIH0gZWxzZSB7XG4gICAgcmVzdWx0ICs9IGAke2RpYWdub3N0aWMubWVzc2FnZVRleHR9JHtuZXdMaW5lfWA7XG4gIH1cbiAgcmV0dXJuIHJlc3VsdDtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGZvcm1hdERpYWdub3N0aWNzKFxuICAgIGRpYWdzOiBEaWFnbm9zdGljcywgaG9zdDogdHMuRm9ybWF0RGlhZ25vc3RpY3NIb3N0ID0gZGVmYXVsdEZvcm1hdEhvc3QpOiBzdHJpbmcge1xuICBpZiAoZGlhZ3MgJiYgZGlhZ3MubGVuZ3RoKSB7XG4gICAgcmV0dXJuIGRpYWdzXG4gICAgICAgIC5tYXAoZGlhZ25vc3RpYyA9PiB7XG4gICAgICAgICAgaWYgKGFwaS5pc1RzRGlhZ25vc3RpYyhkaWFnbm9zdGljKSkge1xuICAgICAgICAgICAgcmV0dXJuIHJlcGxhY2VUc1dpdGhOZ0luRXJyb3JzKFxuICAgICAgICAgICAgICAgIHRzLmZvcm1hdERpYWdub3N0aWNzV2l0aENvbG9yQW5kQ29udGV4dChbZGlhZ25vc3RpY10sIGhvc3QpKTtcbiAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgcmV0dXJuIGZvcm1hdERpYWdub3N0aWMoZGlhZ25vc3RpYywgaG9zdCk7XG4gICAgICAgICAgfVxuICAgICAgICB9KVxuICAgICAgICAuam9pbignJyk7XG4gIH0gZWxzZSB7XG4gICAgcmV0dXJuICcnO1xuICB9XG59XG5cbmV4cG9ydCBpbnRlcmZhY2UgUGFyc2VkQ29uZmlndXJhdGlvbiB7XG4gIHByb2plY3Q6IHN0cmluZztcbiAgb3B0aW9uczogYXBpLkNvbXBpbGVyT3B0aW9ucztcbiAgcm9vdE5hbWVzOiBzdHJpbmdbXTtcbiAgZW1pdEZsYWdzOiBhcGkuRW1pdEZsYWdzO1xuICBlcnJvcnM6IERpYWdub3N0aWNzO1xufVxuXG5leHBvcnQgZnVuY3Rpb24gY2FsY1Byb2plY3RGaWxlQW5kQmFzZVBhdGgocHJvamVjdDogc3RyaW5nKTpcbiAgICB7cHJvamVjdEZpbGU6IEFic29sdXRlRnNQYXRoLCBiYXNlUGF0aDogQWJzb2x1dGVGc1BhdGh9IHtcbiAgY29uc3QgZnMgPSBnZXRGaWxlU3lzdGVtKCk7XG4gIGNvbnN0IGFic1Byb2plY3QgPSBmcy5yZXNvbHZlKHByb2plY3QpO1xuICBjb25zdCBwcm9qZWN0SXNEaXIgPSBmcy5sc3RhdChhYnNQcm9qZWN0KS5pc0RpcmVjdG9yeSgpO1xuICBjb25zdCBwcm9qZWN0RmlsZSA9IHByb2plY3RJc0RpciA/IGZzLmpvaW4oYWJzUHJvamVjdCwgJ3RzY29uZmlnLmpzb24nKSA6IGFic1Byb2plY3Q7XG4gIGNvbnN0IHByb2plY3REaXIgPSBwcm9qZWN0SXNEaXIgPyBhYnNQcm9qZWN0IDogZnMuZGlybmFtZShhYnNQcm9qZWN0KTtcbiAgY29uc3QgYmFzZVBhdGggPSBmcy5yZXNvbHZlKHByb2plY3REaXIpO1xuICByZXR1cm4ge3Byb2plY3RGaWxlLCBiYXNlUGF0aH07XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBjcmVhdGVOZ0NvbXBpbGVyT3B0aW9ucyhcbiAgICBiYXNlUGF0aDogc3RyaW5nLCBjb25maWc6IGFueSwgdHNPcHRpb25zOiB0cy5Db21waWxlck9wdGlvbnMpOiBhcGkuQ29tcGlsZXJPcHRpb25zIHtcbiAgLy8gZW5hYmxlSXZ5IGBuZ3RzY2AgaXMgYW4gYWxpYXMgZm9yIGB0cnVlYC5cbiAgY29uc3Qge2FuZ3VsYXJDb21waWxlck9wdGlvbnMgPSB7fX0gPSBjb25maWc7XG4gIGNvbnN0IHtlbmFibGVJdnl9ID0gYW5ndWxhckNvbXBpbGVyT3B0aW9ucztcbiAgYW5ndWxhckNvbXBpbGVyT3B0aW9ucy5lbmFibGVJdnkgPSBlbmFibGVJdnkgIT09IGZhbHNlICYmIGVuYWJsZUl2eSAhPT0gJ3RzYyc7XG5cbiAgcmV0dXJuIHsuLi50c09wdGlvbnMsIC4uLmFuZ3VsYXJDb21waWxlck9wdGlvbnMsIGdlbkRpcjogYmFzZVBhdGgsIGJhc2VQYXRofTtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHJlYWRDb25maWd1cmF0aW9uKFxuICAgIHByb2plY3Q6IHN0cmluZywgZXhpc3RpbmdPcHRpb25zPzogdHMuQ29tcGlsZXJPcHRpb25zKTogUGFyc2VkQ29uZmlndXJhdGlvbiB7XG4gIHRyeSB7XG4gICAgY29uc3QgZnMgPSBnZXRGaWxlU3lzdGVtKCk7XG4gICAgY29uc3Qge3Byb2plY3RGaWxlLCBiYXNlUGF0aH0gPSBjYWxjUHJvamVjdEZpbGVBbmRCYXNlUGF0aChwcm9qZWN0KTtcblxuICAgIGNvbnN0IHJlYWRFeHRlbmRlZENvbmZpZ0ZpbGUgPVxuICAgICAgICAoY29uZmlnRmlsZTogc3RyaW5nLCBleGlzdGluZ0NvbmZpZz86IGFueSk6IHtjb25maWc/OiBhbnksIGVycm9yPzogdHMuRGlhZ25vc3RpY30gPT4ge1xuICAgICAgICAgIGNvbnN0IHtjb25maWcsIGVycm9yfSA9IHRzLnJlYWRDb25maWdGaWxlKGNvbmZpZ0ZpbGUsIHRzLnN5cy5yZWFkRmlsZSk7XG5cbiAgICAgICAgICBpZiAoZXJyb3IpIHtcbiAgICAgICAgICAgIHJldHVybiB7ZXJyb3J9O1xuICAgICAgICAgIH1cblxuICAgICAgICAgIC8vIHdlIGFyZSBvbmx5IGludGVyZXN0ZWQgaW50byBtZXJnaW5nICdhbmd1bGFyQ29tcGlsZXJPcHRpb25zJyBhc1xuICAgICAgICAgIC8vIG90aGVyIG9wdGlvbnMgbGlrZSAnY29tcGlsZXJPcHRpb25zJyBhcmUgbWVyZ2VkIGJ5IFRTXG4gICAgICAgICAgY29uc3QgYmFzZUNvbmZpZyA9IGV4aXN0aW5nQ29uZmlnIHx8IGNvbmZpZztcbiAgICAgICAgICBpZiAoZXhpc3RpbmdDb25maWcpIHtcbiAgICAgICAgICAgIGJhc2VDb25maWcuYW5ndWxhckNvbXBpbGVyT3B0aW9ucyA9IHtcbiAgICAgICAgICAgICAgLi4uY29uZmlnLmFuZ3VsYXJDb21waWxlck9wdGlvbnMsXG4gICAgICAgICAgICAgIC4uLmJhc2VDb25maWcuYW5ndWxhckNvbXBpbGVyT3B0aW9uc1xuICAgICAgICAgICAgfTtcbiAgICAgICAgICB9XG5cbiAgICAgICAgICBpZiAoY29uZmlnLmV4dGVuZHMpIHtcbiAgICAgICAgICAgIGxldCBleHRlbmRlZENvbmZpZ1BhdGggPSBmcy5yZXNvbHZlKGZzLmRpcm5hbWUoY29uZmlnRmlsZSksIGNvbmZpZy5leHRlbmRzKTtcbiAgICAgICAgICAgIGV4dGVuZGVkQ29uZmlnUGF0aCA9IGZzLmV4dG5hbWUoZXh0ZW5kZWRDb25maWdQYXRoKSA/XG4gICAgICAgICAgICAgICAgZXh0ZW5kZWRDb25maWdQYXRoIDpcbiAgICAgICAgICAgICAgICBhYnNvbHV0ZUZyb20oYCR7ZXh0ZW5kZWRDb25maWdQYXRofS5qc29uYCk7XG5cbiAgICAgICAgICAgIGlmIChmcy5leGlzdHMoZXh0ZW5kZWRDb25maWdQYXRoKSkge1xuICAgICAgICAgICAgICAvLyBDYWxsIHJlYWQgY29uZmlnIHJlY3Vyc2l2ZWx5IGFzIFR5cGVTY3JpcHQgb25seSBtZXJnZXMgQ29tcGlsZXJPcHRpb25zXG4gICAgICAgICAgICAgIHJldHVybiByZWFkRXh0ZW5kZWRDb25maWdGaWxlKGV4dGVuZGVkQ29uZmlnUGF0aCwgYmFzZUNvbmZpZyk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgfVxuXG4gICAgICAgICAgcmV0dXJuIHtjb25maWc6IGJhc2VDb25maWd9O1xuICAgICAgICB9O1xuXG4gICAgY29uc3Qge2NvbmZpZywgZXJyb3J9ID0gcmVhZEV4dGVuZGVkQ29uZmlnRmlsZShwcm9qZWN0RmlsZSk7XG5cbiAgICBpZiAoZXJyb3IpIHtcbiAgICAgIHJldHVybiB7XG4gICAgICAgIHByb2plY3QsXG4gICAgICAgIGVycm9yczogW2Vycm9yXSxcbiAgICAgICAgcm9vdE5hbWVzOiBbXSxcbiAgICAgICAgb3B0aW9uczoge30sXG4gICAgICAgIGVtaXRGbGFnczogYXBpLkVtaXRGbGFncy5EZWZhdWx0XG4gICAgICB9O1xuICAgIH1cbiAgICBjb25zdCBwYXJzZUNvbmZpZ0hvc3QgPSB7XG4gICAgICB1c2VDYXNlU2Vuc2l0aXZlRmlsZU5hbWVzOiB0cnVlLFxuICAgICAgZmlsZUV4aXN0czogZnMuZXhpc3RzLmJpbmQoZnMpLFxuICAgICAgcmVhZERpcmVjdG9yeTogdHMuc3lzLnJlYWREaXJlY3RvcnksXG4gICAgICByZWFkRmlsZTogdHMuc3lzLnJlYWRGaWxlXG4gICAgfTtcbiAgICBjb25zdCBjb25maWdGaWxlTmFtZSA9IGZzLnJlc29sdmUoZnMucHdkKCksIHByb2plY3RGaWxlKTtcbiAgICBjb25zdCBwYXJzZWQgPSB0cy5wYXJzZUpzb25Db25maWdGaWxlQ29udGVudChcbiAgICAgICAgY29uZmlnLCBwYXJzZUNvbmZpZ0hvc3QsIGJhc2VQYXRoLCBleGlzdGluZ09wdGlvbnMsIGNvbmZpZ0ZpbGVOYW1lKTtcbiAgICBjb25zdCByb290TmFtZXMgPSBwYXJzZWQuZmlsZU5hbWVzO1xuXG4gICAgY29uc3Qgb3B0aW9ucyA9IGNyZWF0ZU5nQ29tcGlsZXJPcHRpb25zKGJhc2VQYXRoLCBjb25maWcsIHBhcnNlZC5vcHRpb25zKTtcbiAgICBsZXQgZW1pdEZsYWdzID0gYXBpLkVtaXRGbGFncy5EZWZhdWx0O1xuICAgIGlmICghKG9wdGlvbnMuc2tpcE1ldGFkYXRhRW1pdCB8fCBvcHRpb25zLmZsYXRNb2R1bGVPdXRGaWxlKSkge1xuICAgICAgZW1pdEZsYWdzIHw9IGFwaS5FbWl0RmxhZ3MuTWV0YWRhdGE7XG4gICAgfVxuICAgIGlmIChvcHRpb25zLnNraXBUZW1wbGF0ZUNvZGVnZW4pIHtcbiAgICAgIGVtaXRGbGFncyA9IGVtaXRGbGFncyAmIH5hcGkuRW1pdEZsYWdzLkNvZGVnZW47XG4gICAgfVxuICAgIHJldHVybiB7cHJvamVjdDogcHJvamVjdEZpbGUsIHJvb3ROYW1lcywgb3B0aW9ucywgZXJyb3JzOiBwYXJzZWQuZXJyb3JzLCBlbWl0RmxhZ3N9O1xuICB9IGNhdGNoIChlKSB7XG4gICAgY29uc3QgZXJyb3JzOiBEaWFnbm9zdGljcyA9IFt7XG4gICAgICBjYXRlZ29yeTogdHMuRGlhZ25vc3RpY0NhdGVnb3J5LkVycm9yLFxuICAgICAgbWVzc2FnZVRleHQ6IGUuc3RhY2ssXG4gICAgICBzb3VyY2U6IGFwaS5TT1VSQ0UsXG4gICAgICBjb2RlOiBhcGkuVU5LTk9XTl9FUlJPUl9DT0RFXG4gICAgfV07XG4gICAgcmV0dXJuIHtwcm9qZWN0OiAnJywgZXJyb3JzLCByb290TmFtZXM6IFtdLCBvcHRpb25zOiB7fSwgZW1pdEZsYWdzOiBhcGkuRW1pdEZsYWdzLkRlZmF1bHR9O1xuICB9XG59XG5cbmV4cG9ydCBpbnRlcmZhY2UgUGVyZm9ybUNvbXBpbGF0aW9uUmVzdWx0IHtcbiAgZGlhZ25vc3RpY3M6IERpYWdub3N0aWNzO1xuICBwcm9ncmFtPzogYXBpLlByb2dyYW07XG4gIGVtaXRSZXN1bHQ/OiB0cy5FbWl0UmVzdWx0O1xufVxuXG5leHBvcnQgZnVuY3Rpb24gZXhpdENvZGVGcm9tUmVzdWx0KGRpYWdzOiBEaWFnbm9zdGljc3x1bmRlZmluZWQpOiBudW1iZXIge1xuICBpZiAoIWRpYWdzIHx8IGZpbHRlckVycm9yc0FuZFdhcm5pbmdzKGRpYWdzKS5sZW5ndGggPT09IDApIHtcbiAgICAvLyBJZiB3ZSBoYXZlIGEgcmVzdWx0IGFuZCBkaWRuJ3QgZ2V0IGFueSBlcnJvcnMsIHdlIHN1Y2NlZWRlZC5cbiAgICByZXR1cm4gMDtcbiAgfVxuXG4gIC8vIFJldHVybiAyIGlmIGFueSBvZiB0aGUgZXJyb3JzIHdlcmUgdW5rbm93bi5cbiAgcmV0dXJuIGRpYWdzLnNvbWUoZCA9PiBkLnNvdXJjZSA9PT0gJ2FuZ3VsYXInICYmIGQuY29kZSA9PT0gYXBpLlVOS05PV05fRVJST1JfQ09ERSkgPyAyIDogMTtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHBlcmZvcm1Db21waWxhdGlvbih7XG4gIHJvb3ROYW1lcyxcbiAgb3B0aW9ucyxcbiAgaG9zdCxcbiAgb2xkUHJvZ3JhbSxcbiAgZW1pdENhbGxiYWNrLFxuICBtZXJnZUVtaXRSZXN1bHRzQ2FsbGJhY2ssXG4gIGdhdGhlckRpYWdub3N0aWNzID0gZGVmYXVsdEdhdGhlckRpYWdub3N0aWNzLFxuICBjdXN0b21UcmFuc2Zvcm1lcnMsXG4gIGVtaXRGbGFncyA9IGFwaS5FbWl0RmxhZ3MuRGVmYXVsdCxcbiAgbW9kaWZpZWRSZXNvdXJjZUZpbGVzID0gbnVsbFxufToge1xuICByb290TmFtZXM6IHN0cmluZ1tdLFxuICBvcHRpb25zOiBhcGkuQ29tcGlsZXJPcHRpb25zLFxuICBob3N0PzogYXBpLkNvbXBpbGVySG9zdCxcbiAgb2xkUHJvZ3JhbT86IGFwaS5Qcm9ncmFtLFxuICBlbWl0Q2FsbGJhY2s/OiBhcGkuVHNFbWl0Q2FsbGJhY2ssXG4gIG1lcmdlRW1pdFJlc3VsdHNDYWxsYmFjaz86IGFwaS5Uc01lcmdlRW1pdFJlc3VsdHNDYWxsYmFjayxcbiAgZ2F0aGVyRGlhZ25vc3RpY3M/OiAocHJvZ3JhbTogYXBpLlByb2dyYW0pID0+IERpYWdub3N0aWNzLFxuICBjdXN0b21UcmFuc2Zvcm1lcnM/OiBhcGkuQ3VzdG9tVHJhbnNmb3JtZXJzLFxuICBlbWl0RmxhZ3M/OiBhcGkuRW1pdEZsYWdzLFxuICBtb2RpZmllZFJlc291cmNlRmlsZXM/OiBTZXQ8c3RyaW5nPnwgbnVsbCxcbn0pOiBQZXJmb3JtQ29tcGlsYXRpb25SZXN1bHQge1xuICBsZXQgcHJvZ3JhbTogYXBpLlByb2dyYW18dW5kZWZpbmVkO1xuICBsZXQgZW1pdFJlc3VsdDogdHMuRW1pdFJlc3VsdHx1bmRlZmluZWQ7XG4gIGxldCBhbGxEaWFnbm9zdGljczogQXJyYXk8dHMuRGlhZ25vc3RpY3xhcGkuRGlhZ25vc3RpYz4gPSBbXTtcbiAgdHJ5IHtcbiAgICBpZiAoIWhvc3QpIHtcbiAgICAgIGhvc3QgPSBuZy5jcmVhdGVDb21waWxlckhvc3Qoe29wdGlvbnN9KTtcbiAgICB9XG4gICAgaWYgKG1vZGlmaWVkUmVzb3VyY2VGaWxlcykge1xuICAgICAgaG9zdC5nZXRNb2RpZmllZFJlc291cmNlRmlsZXMgPSAoKSA9PiBtb2RpZmllZFJlc291cmNlRmlsZXM7XG4gICAgfVxuXG4gICAgcHJvZ3JhbSA9IG5nLmNyZWF0ZVByb2dyYW0oe3Jvb3ROYW1lcywgaG9zdCwgb3B0aW9ucywgb2xkUHJvZ3JhbX0pO1xuXG4gICAgY29uc3QgYmVmb3JlRGlhZ3MgPSBEYXRlLm5vdygpO1xuICAgIGFsbERpYWdub3N0aWNzLnB1c2goLi4uZ2F0aGVyRGlhZ25vc3RpY3MocHJvZ3JhbSEpKTtcbiAgICBpZiAob3B0aW9ucy5kaWFnbm9zdGljcykge1xuICAgICAgY29uc3QgYWZ0ZXJEaWFncyA9IERhdGUubm93KCk7XG4gICAgICBhbGxEaWFnbm9zdGljcy5wdXNoKFxuICAgICAgICAgIGNyZWF0ZU1lc3NhZ2VEaWFnbm9zdGljKGBUaW1lIGZvciBkaWFnbm9zdGljczogJHthZnRlckRpYWdzIC0gYmVmb3JlRGlhZ3N9bXMuYCkpO1xuICAgIH1cblxuICAgIGlmICghaGFzRXJyb3JzKGFsbERpYWdub3N0aWNzKSkge1xuICAgICAgZW1pdFJlc3VsdCA9XG4gICAgICAgICAgcHJvZ3JhbSEuZW1pdCh7ZW1pdENhbGxiYWNrLCBtZXJnZUVtaXRSZXN1bHRzQ2FsbGJhY2ssIGN1c3RvbVRyYW5zZm9ybWVycywgZW1pdEZsYWdzfSk7XG4gICAgICBhbGxEaWFnbm9zdGljcy5wdXNoKC4uLmVtaXRSZXN1bHQuZGlhZ25vc3RpY3MpO1xuICAgICAgcmV0dXJuIHtkaWFnbm9zdGljczogYWxsRGlhZ25vc3RpY3MsIHByb2dyYW0sIGVtaXRSZXN1bHR9O1xuICAgIH1cbiAgICByZXR1cm4ge2RpYWdub3N0aWNzOiBhbGxEaWFnbm9zdGljcywgcHJvZ3JhbX07XG4gIH0gY2F0Y2ggKGUpIHtcbiAgICBsZXQgZXJyTXNnOiBzdHJpbmc7XG4gICAgbGV0IGNvZGU6IG51bWJlcjtcbiAgICBpZiAoaXNTeW50YXhFcnJvcihlKSkge1xuICAgICAgLy8gZG9uJ3QgcmVwb3J0IHRoZSBzdGFjayBmb3Igc3ludGF4IGVycm9ycyBhcyB0aGV5IGFyZSB3ZWxsIGtub3duIGVycm9ycy5cbiAgICAgIGVyck1zZyA9IGUubWVzc2FnZTtcbiAgICAgIGNvZGUgPSBhcGkuREVGQVVMVF9FUlJPUl9DT0RFO1xuICAgIH0gZWxzZSB7XG4gICAgICBlcnJNc2cgPSBlLnN0YWNrO1xuICAgICAgLy8gSXQgaXMgbm90IGEgc3ludGF4IGVycm9yIHdlIG1pZ2h0IGhhdmUgYSBwcm9ncmFtIHdpdGggdW5rbm93biBzdGF0ZSwgZGlzY2FyZCBpdC5cbiAgICAgIHByb2dyYW0gPSB1bmRlZmluZWQ7XG4gICAgICBjb2RlID0gYXBpLlVOS05PV05fRVJST1JfQ09ERTtcbiAgICB9XG4gICAgYWxsRGlhZ25vc3RpY3MucHVzaChcbiAgICAgICAge2NhdGVnb3J5OiB0cy5EaWFnbm9zdGljQ2F0ZWdvcnkuRXJyb3IsIG1lc3NhZ2VUZXh0OiBlcnJNc2csIGNvZGUsIHNvdXJjZTogYXBpLlNPVVJDRX0pO1xuICAgIHJldHVybiB7ZGlhZ25vc3RpY3M6IGFsbERpYWdub3N0aWNzLCBwcm9ncmFtfTtcbiAgfVxufVxuZXhwb3J0IGZ1bmN0aW9uIGRlZmF1bHRHYXRoZXJEaWFnbm9zdGljcyhwcm9ncmFtOiBhcGkuUHJvZ3JhbSk6IERpYWdub3N0aWNzIHtcbiAgY29uc3QgYWxsRGlhZ25vc3RpY3M6IEFycmF5PHRzLkRpYWdub3N0aWN8YXBpLkRpYWdub3N0aWM+ID0gW107XG5cbiAgZnVuY3Rpb24gY2hlY2tEaWFnbm9zdGljcyhkaWFnczogRGlhZ25vc3RpY3N8dW5kZWZpbmVkKSB7XG4gICAgaWYgKGRpYWdzKSB7XG4gICAgICBhbGxEaWFnbm9zdGljcy5wdXNoKC4uLmRpYWdzKTtcbiAgICAgIHJldHVybiAhaGFzRXJyb3JzKGRpYWdzKTtcbiAgICB9XG4gICAgcmV0dXJuIHRydWU7XG4gIH1cblxuICBsZXQgY2hlY2tPdGhlckRpYWdub3N0aWNzID0gdHJ1ZTtcbiAgLy8gQ2hlY2sgcGFyYW1ldGVyIGRpYWdub3N0aWNzXG4gIGNoZWNrT3RoZXJEaWFnbm9zdGljcyA9IGNoZWNrT3RoZXJEaWFnbm9zdGljcyAmJlxuICAgICAgY2hlY2tEaWFnbm9zdGljcyhbLi4ucHJvZ3JhbS5nZXRUc09wdGlvbkRpYWdub3N0aWNzKCksIC4uLnByb2dyYW0uZ2V0TmdPcHRpb25EaWFnbm9zdGljcygpXSk7XG5cbiAgLy8gQ2hlY2sgc3ludGFjdGljIGRpYWdub3N0aWNzXG4gIGNoZWNrT3RoZXJEaWFnbm9zdGljcyA9XG4gICAgICBjaGVja090aGVyRGlhZ25vc3RpY3MgJiYgY2hlY2tEaWFnbm9zdGljcyhwcm9ncmFtLmdldFRzU3ludGFjdGljRGlhZ25vc3RpY3MoKSBhcyBEaWFnbm9zdGljcyk7XG5cbiAgLy8gQ2hlY2sgVHlwZVNjcmlwdCBzZW1hbnRpYyBhbmQgQW5ndWxhciBzdHJ1Y3R1cmUgZGlhZ25vc3RpY3NcbiAgY2hlY2tPdGhlckRpYWdub3N0aWNzID1cbiAgICAgIGNoZWNrT3RoZXJEaWFnbm9zdGljcyAmJlxuICAgICAgY2hlY2tEaWFnbm9zdGljcyhcbiAgICAgICAgICBbLi4ucHJvZ3JhbS5nZXRUc1NlbWFudGljRGlhZ25vc3RpY3MoKSwgLi4ucHJvZ3JhbS5nZXROZ1N0cnVjdHVyYWxEaWFnbm9zdGljcygpXSk7XG5cbiAgLy8gQ2hlY2sgQW5ndWxhciBzZW1hbnRpYyBkaWFnbm9zdGljc1xuICBjaGVja090aGVyRGlhZ25vc3RpY3MgPVxuICAgICAgY2hlY2tPdGhlckRpYWdub3N0aWNzICYmIGNoZWNrRGlhZ25vc3RpY3MocHJvZ3JhbS5nZXROZ1NlbWFudGljRGlhZ25vc3RpY3MoKSBhcyBEaWFnbm9zdGljcyk7XG5cbiAgcmV0dXJuIGFsbERpYWdub3N0aWNzO1xufVxuXG5mdW5jdGlvbiBoYXNFcnJvcnMoZGlhZ3M6IERpYWdub3N0aWNzKSB7XG4gIHJldHVybiBkaWFncy5zb21lKGQgPT4gZC5jYXRlZ29yeSA9PT0gdHMuRGlhZ25vc3RpY0NhdGVnb3J5LkVycm9yKTtcbn1cbiJdfQ==