#!/usr/bin/env node
(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define("@angular/compiler-cli/ngcc/src/command_line_options", ["require", "exports", "yargs", "@angular/compiler-cli/src/ngtsc/file_system", "@angular/compiler-cli/ngcc/src/logging/console_logger", "@angular/compiler-cli/ngcc/src/logging/logger"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.parseCommandLineOptions = void 0;
    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    var yargs = require("yargs");
    var file_system_1 = require("@angular/compiler-cli/src/ngtsc/file_system");
    var console_logger_1 = require("@angular/compiler-cli/ngcc/src/logging/console_logger");
    var logger_1 = require("@angular/compiler-cli/ngcc/src/logging/logger");
    function parseCommandLineOptions(args) {
        var options = yargs
            .option('s', {
            alias: 'source',
            describe: 'A path (relative to the working directory) of the `node_modules` folder to process.',
            default: './node_modules'
        })
            .option('f', { alias: 'formats', hidden: true, array: true })
            .option('p', {
            alias: 'properties',
            array: true,
            describe: 'An array of names of properties in package.json to compile (e.g. `module` or `main`)\n' +
                'Each of these properties should hold the path to a bundle-format.\n' +
                'If provided, only the specified properties are considered for processing.\n' +
                'If not provided, all the supported format properties (e.g. fesm2015, fesm5, es2015, esm2015, esm5, main, module) in the package.json are considered.'
        })
            .option('t', {
            alias: 'target',
            describe: 'A relative path (from the `source` path) to a single entry-point to process (plus its dependencies).\n' +
                'If this property is provided then `error-on-failed-entry-point` is forced to true.\n' +
                'This option overrides the `--use-program-dependencies` option.',
        })
            .option('use-program-dependencies', {
            type: 'boolean',
            describe: 'If this property is provided then the entry-points to process are parsed from the program defined by the loaded tsconfig.json. See `--tsconfig`.\n' +
                'This option is overridden by the `--target` option.',
        })
            .option('first-only', {
            describe: 'If specified then only the first matching package.json property will be compiled.',
            type: 'boolean'
        })
            .option('create-ivy-entry-points', {
            describe: 'If specified then new `*_ivy_ngcc` entry-points will be added to package.json rather than modifying the ones in-place.\n' +
                'For this to work you need to have custom resolution set up (e.g. in webpack) to look for these new entry-points.\n' +
                'The Angular CLI does this already, so it is safe to use this option if the project is being built via the CLI.',
            type: 'boolean',
        })
            .option('legacy-message-ids', {
            describe: 'Render `$localize` messages with legacy format ids.\n' +
                'The default value is `true`. Only set this to `false` if you do not want legacy message ids to\n' +
                'be rendered. For example, if you are not using legacy message ids in your translation files\n' +
                'AND are not doing compile-time inlining of translations, in which case the extra message ids\n' +
                'would add unwanted size to the final source bundle.\n' +
                'It is safe to leave this set to true if you are doing compile-time inlining because the extra\n' +
                'legacy message ids will all be stripped during translation.',
            type: 'boolean',
            default: true,
        })
            .option('async', {
            describe: 'Whether to compile asynchronously. This is enabled by default as it allows compilations to be parallelized.\n' +
                'Disabling asynchronous compilation may be useful for debugging.',
            type: 'boolean',
            default: true,
        })
            .option('l', {
            alias: 'loglevel',
            describe: 'The lowest severity logging message that should be output.',
            choices: ['debug', 'info', 'warn', 'error'],
        })
            .option('invalidate-entry-point-manifest', {
            describe: 'If this is set then ngcc will not read an entry-point manifest file from disk.\n' +
                'Instead it will walk the directory tree as normal looking for entry-points, and then write a new manifest file.',
            type: 'boolean',
            default: false,
        })
            .option('error-on-failed-entry-point', {
            describe: 'Set this option in order to terminate immediately with an error code if an entry-point fails to be processed.\n' +
                'If `-t`/`--target` is provided then this property is always true and cannot be changed. Otherwise the default is false.\n' +
                'When set to false, ngcc will continue to process entry-points after a failure. In which case it will log an error and resume processing other entry-points.',
            type: 'boolean',
            default: false,
        })
            .option('tsconfig', {
            describe: 'A path to a tsconfig.json file that will be used to configure the Angular compiler and module resolution used by ngcc.\n' +
                'If not provided, ngcc will attempt to read a `tsconfig.json` file from the folder above that given by the `-s` option.\n' +
                'Set to false (via `--no-tsconfig`) if you do not want ngcc to use any `tsconfig.json` file.',
            type: 'string',
        })
            .strict()
            .help()
            .parse(args);
        if (options['f'] && options['f'].length) {
            console.error('The formats option (-f/--formats) has been removed. Consider the properties option (-p/--properties) instead.');
            process.exit(1);
        }
        file_system_1.setFileSystem(new file_system_1.NodeJSFileSystem());
        var baseSourcePath = file_system_1.resolve(options['s'] || './node_modules');
        var propertiesToConsider = options['p'];
        var targetEntryPointPath = options['t'] ? options['t'] : undefined;
        var compileAllFormats = !options['first-only'];
        var createNewEntryPointFormats = options['create-ivy-entry-points'];
        var logLevel = options['l'];
        var enableI18nLegacyMessageIdFormat = options['legacy-message-ids'];
        var invalidateEntryPointManifest = options['invalidate-entry-point-manifest'];
        var errorOnFailedEntryPoint = options['error-on-failed-entry-point'];
        var findEntryPointsFromTsConfigProgram = options['use-program-dependencies'];
        // yargs is not so great at mixed string+boolean types, so we have to test tsconfig against a
        // string "false" to capture the `tsconfig=false` option.
        // And we have to convert the option to a string to handle `no-tsconfig`, which will be `false`.
        var tsConfigPath = "" + options['tsconfig'] === 'false' ? null : options['tsconfig'];
        var logger = logLevel && new console_logger_1.ConsoleLogger(logger_1.LogLevel[logLevel]);
        return {
            basePath: baseSourcePath,
            propertiesToConsider: propertiesToConsider,
            targetEntryPointPath: targetEntryPointPath,
            compileAllFormats: compileAllFormats,
            createNewEntryPointFormats: createNewEntryPointFormats,
            logger: logger,
            enableI18nLegacyMessageIdFormat: enableI18nLegacyMessageIdFormat,
            async: options['async'],
            invalidateEntryPointManifest: invalidateEntryPointManifest,
            errorOnFailedEntryPoint: errorOnFailedEntryPoint,
            tsConfigPath: tsConfigPath,
            findEntryPointsFromTsConfigProgram: findEntryPointsFromTsConfigProgram,
        };
    }
    exports.parseCommandLineOptions = parseCommandLineOptions;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY29tbWFuZF9saW5lX29wdGlvbnMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9jb21waWxlci1jbGkvbmdjYy9zcmMvY29tbWFuZF9saW5lX29wdGlvbnMudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7Ozs7Ozs7Ozs7OztJQUNBOzs7Ozs7T0FNRztJQUNILDZCQUErQjtJQUUvQiwyRUFBcUY7SUFDckYsd0ZBQXVEO0lBQ3ZELHdFQUEwQztJQUcxQyxTQUFnQix1QkFBdUIsQ0FBQyxJQUFjO1FBQ3BELElBQU0sT0FBTyxHQUNULEtBQUs7YUFDQSxNQUFNLENBQUMsR0FBRyxFQUFFO1lBQ1gsS0FBSyxFQUFFLFFBQVE7WUFDZixRQUFRLEVBQ0oscUZBQXFGO1lBQ3pGLE9BQU8sRUFBRSxnQkFBZ0I7U0FDMUIsQ0FBQzthQUNELE1BQU0sQ0FBQyxHQUFHLEVBQUUsRUFBQyxLQUFLLEVBQUUsU0FBUyxFQUFFLE1BQU0sRUFBRSxJQUFJLEVBQUUsS0FBSyxFQUFFLElBQUksRUFBQyxDQUFDO2FBQzFELE1BQU0sQ0FBQyxHQUFHLEVBQUU7WUFDWCxLQUFLLEVBQUUsWUFBWTtZQUNuQixLQUFLLEVBQUUsSUFBSTtZQUNYLFFBQVEsRUFDSix3RkFBd0Y7Z0JBQ3hGLHFFQUFxRTtnQkFDckUsNkVBQTZFO2dCQUM3RSxzSkFBc0o7U0FDM0osQ0FBQzthQUNELE1BQU0sQ0FBQyxHQUFHLEVBQUU7WUFDWCxLQUFLLEVBQUUsUUFBUTtZQUNmLFFBQVEsRUFDSix3R0FBd0c7Z0JBQ3hHLHNGQUFzRjtnQkFDdEYsZ0VBQWdFO1NBQ3JFLENBQUM7YUFDRCxNQUFNLENBQUMsMEJBQTBCLEVBQUU7WUFDbEMsSUFBSSxFQUFFLFNBQVM7WUFDZixRQUFRLEVBQ0osb0pBQW9KO2dCQUNwSixxREFBcUQ7U0FDMUQsQ0FBQzthQUNELE1BQU0sQ0FBQyxZQUFZLEVBQUU7WUFDcEIsUUFBUSxFQUNKLG1GQUFtRjtZQUN2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QWdHO1FBQ2hHLElBQU0sWUFBWSxHQUFHLEtBQUcsT0FBTyxDQUFDLFVBQVUsQ0FBRyxLQUFLLE9BQU8sQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQyxPQUFPLENBQUMsVUFBVSxDQUFDLENBQUM7UUFFdkYsSUFBTSxNQUFNLEdBQUcsUUFBUSxJQUFJLElBQUksOEJBQWEsQ0FBQyxpQkFBUSxDQUFDLFFBQVEsQ0FBQyxDQUFDLENBQUM7UUFFakUsT0FBTztZQUNMLFFBQVEsRUFBRSxjQUFjO1lBQ3hCLG9CQUFvQixzQkFBQTtZQUNwQixvQkFBb0Isc0JBQUE7WUFDcEIsaUJBQWlCLG1CQUFBO1lBQ2pCLDBCQUEwQiw0QkFBQTtZQUMxQixNQUFNLFFBQUE7WUFDTiwrQkFBK0IsaUNBQUE7WUFDL0IsS0FBSyxFQUFFLE9BQU8sQ0FBQyxPQUFPLENBQUM7WUFDdkIsNEJBQTRCLDhCQUFBO1lBQzVCLHVCQUF1Qix5QkFBQTtZQUN2QixZQUFZLGNBQUE7WUFDWixrQ0FBa0Msb0NBQUE7U0FDbkMsQ0FBQztJQUNKLENBQUM7SUFwSUQsMERBb0lDIiwic291cmNlc0NvbnRlbnQiOlsiIyEvdXNyL2Jpbi9lbnYgbm9kZVxuLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5pbXBvcnQgKiBhcyB5YXJncyBmcm9tICd5YXJncyc7XG5cbmltcG9ydCB7cmVzb2x2ZSwgc2V0RmlsZVN5c3RlbSwgTm9kZUpTRmlsZVN5c3RlbX0gZnJvbSAnLi4vLi4vc3JjL25ndHNjL2ZpbGVfc3lzdGVtJztcbmltcG9ydCB7Q29uc29sZUxvZ2dlcn0gZnJvbSAnLi9sb2dnaW5nL2NvbnNvbGVfbG9nZ2VyJztcbmltcG9ydCB7TG9nTGV2ZWx9IGZyb20gJy4vbG9nZ2luZy9sb2dnZXInO1xuaW1wb3J0IHtOZ2NjT3B0aW9uc30gZnJvbSAnLi9uZ2NjX29wdGlvbnMnO1xuXG5leHBvcnQgZnVuY3Rpb24gcGFyc2VDb21tYW5kTGluZU9wdGlvbnMoYXJnczogc3RyaW5nW10pOiBOZ2NjT3B0aW9ucyB7XG4gIGNvbnN0IG9wdGlvbnMgPVxuICAgICAgeWFyZ3NcbiAgICAgICAgICAub3B0aW9uKCdzJywge1xuICAgICAgICAgICAgYWxpYXM6ICdzb3VyY2UnLFxuICAgICAgICAgICAgZGVzY3JpYmU6XG4gICAgICAgICAgICAgICAgJ0EgcGF0aCAocmVsYXRpdmUgdG8gdGhlIHdvcmtpbmcgZGlyZWN0b3J5KSBvZiB0aGUgYG5vZGVfbW9kdWxlc2AgZm9sZGVyIHRvIHByb2Nlc3MuJyxcbiAgICAgICAgICAgIGRlZmF1bHQ6ICcuL25vZGVfbW9kdWxlcydcbiAgICAgICAgICB9KVxuICAgICAgICAgIC5vcHRpb24oJ2YnLCB7YWxpYXM6ICdmb3JtYXRzJywgaGlkZGVuOsKgdHJ1ZSwgYXJyYXk6IHRydWV9KVxuICAgICAgICAgIC5vcHRpb24oJ3AnLCB7XG4gICAgICAgICAgICBhbGlhczogJ3Byb3BlcnRpZXMnLFxuICAgICAgICAgICAgYXJyYXk6IHRydWUsXG4gICAgICAgICAgICBkZXNjcmliZTpcbiAgICAgICAgICAgICAgICAnQW4gYXJyYXkgb2YgbmFtZXMgb2YgcHJvcGVydGllcyBpbiBwYWNrYWdlLmpzb24gdG8gY29tcGlsZSAoZS5nLiBgbW9kdWxlYCBvciBgbWFpbmApXFxuJyArXG4gICAgICAgICAgICAgICAgJ0VhY2ggb2YgdGhlc2UgcHJvcGVydGllcyBzaG91bGQgaG9sZCB0aGUgcGF0aCB0byBhIGJ1bmRsZS1mb3JtYXQuXFxuJyArXG4gICAgICAgICAgICAgICAgJ0lmIHByb3ZpZGVkLCBvbmx5IHRoZSBzcGVjaWZpZWQgcHJvcGVydGllcyBhcmUgY29uc2lkZXJlZCBmb3IgcHJvY2Vzc2luZy5cXG4nICtcbiAgICAgICAgICAgICAgICAnSWYgbm90IHByb3ZpZGVkLCBhbGwgdGhlIHN1cHBvcnRlZCBmb3JtYXQgcHJvcGVydGllcyAoZS5nLiBmZXNtMjAxNSwgZmVzbTUsIGVzMjAxNSwgZXNtMjAxNSwgZXNtNSwgbWFpbiwgbW9kdWxlKSBpbiB0aGUgcGFja2FnZS5qc29uIGFyZSBjb25zaWRlcmVkLidcbiAgICAgICAgICB9KVxuICAgICAgICAgIC5vcHRpb24oJ3QnLCB7XG4gICAgICAgICAgICBhbGlhczogJ3RhcmdldCcsXG4gICAgICAgICAgICBkZXNjcmliZTpcbiAgICAgICAgICAgICAgICAnQSByZWxhdGl2ZSBwYXRoIChmcm9tIHRoZSBgc291cmNlYCBwYXRoKSB0byBhIHNpbmdsZSBlbnRyeS1wb2ludCB0byBwcm9jZXNzIChwbHVzIGl0cyBkZXBlbmRlbmNpZXMpLlxcbicgK1xuICAgICAgICAgICAgICAgICdJZiB0aGlzIHByb3BlcnR5IGlzIHByb3ZpZGVkIHRoZW4gYGVycm9yLW9uLWZhaWxlZC1lbnRyeS1wb2ludGAgaXMgZm9yY2VkIHRvIHRydWUuXFxuJyArXG4gICAgICAgICAgICAgICAgJ1RoaXMgb3B0aW9uIG92ZXJyaWRlcyB0aGUgYC0tdXNlLXByb2dyYW0tZGVwZW5kZW5jaWVzYCBvcHRpb24uJyxcbiAgICAgICAgICB9KVxuICAgICAgICAgIC5vcHRpb24oJ3VzZS1wcm9ncmFtLWRlcGVuZGVuY2llcycsIHtcbiAgICAgICAgICAgIHR5cGU6ICdib29sZWFuJyxcbiAgICAgICAgICAgIGRlc2NyaWJlOlxuICAgICAgICAgICAgICAgICdJZiB0aGlzIHByb3BlcnR5IGlzIHByb3ZpZGVkIHRoZW4gdGhlIGVudHJ5LXBvaW50cyB0byBwcm9jZXNzIGFyZSBwYXJzZWQgZnJvbSB0aGUgcHJvZ3JhbSBkZWZpbmVkIGJ5IHRoZSBsb2FkZWQgdHNjb25maWcuanNvbi4gU2VlIGAtLXRzY29uZmlnYC5cXG4nICtcbiAgICAgICAgICAgICAgICAnVGhpcyBvcHRpb24gaXMgb3ZlcnJpZGRlbiBieSB0aGUgYC0tdGFyZ2V0YCBvcHRpb24uJyxcbiAgICAgICAgICB9KVxuICAgICAgICAgIC5vcHRpb24oJ2ZpcnN0LW9ubHknLCB7XG4gICAgICAgICAgICBkZXNjcmliZTpcbiAgICAgICAgICAgICAgICAnSWYgc3BlY2lmaWVkIHRoZW4gb25seSB0aGUgZmlyc3QgbWF0Y2hpbmcgcGFja2FnZS5qc29uIHByb3BlcnR5IHdpbGwgYmUgY29tcGlsZWQuJyxcbiAgICAgICAgICAgIHR5cGU6ICdib29sZWFuJ1xuICAgICAgICAgIH0pXG4gICAgICAgICAgLm9wdGlvbignY3JlYXRlLWl2eS1lbnRyeS1wb2ludHMnLCB7XG4gICAgICAgICAgICBkZXNjcmliZTpcbiAgICAgICAgICAgICAgICAnSWYgc3BlY2lmaWVkIHRoZW4gbmV3IGAqX2l2eV9uZ2NjYCBlbnRyeS1wb2ludHMgd2lsbCBiZSBhZGRlZCB0byBwYWNrYWdlLmpzb24gcmF0aGVyIHRoYW4gbW9kaWZ5aW5nIHRoZSBvbmVzIGluLXBsYWNlLlxcbicgK1xuICAgICAgICAgICAgICAgICdGb3IgdGhpcyB0byB3b3JrIHlvdSBuZWVkIHRvIGhhdmUgY3VzdG9tIHJlc29sdXRpb24gc2V0IHVwIChlLmcuIGluIHdlYnBhY2spIHRvIGxvb2sgZm9yIHRoZXNlIG5ldyBlbnRyeS1wb2ludHMuXFxuJyArXG4gICAgICAgICAgICAgICAgJ1RoZSBBbmd1bGFyIENMSSBkb2VzIHRoaXMgYWxyZWFkeSwgc28gaXQgaXMgc2FmZSB0byB1c2UgdGhpcyBvcHRpb24gaWYgdGhlIHByb2plY3QgaXMgYmVpbmcgYnVpbHQgdmlhIHRoZSBDTEkuJyxcbiAgICAgICAgICAgIHR5cGU6ICdib29sZWFuJyxcbiAgICAgICAgICB9KVxuICAgICAgICAgIC5vcHRpb24oJ2xlZ2FjeS1tZXNzYWdlLWlkcycsIHtcbiAgICAgICAgICAgIGRlc2NyaWJlOiAnUmVuZGVyIGAkbG9jYWxpemVgIG1lc3NhZ2VzIHdpdGggbGVnYWN5IGZvcm1hdCBpZHMuXFxuJyArXG4gICAgICAgICAgICAgICAgJ1RoZSBkZWZhdWx0IHZhbHVlIGlzIGB0cnVlYC4gT25seSBzZXQgdGhpcyB0byBgZmFsc2VgIGlmIHlvdSBkbyBub3Qgd2FudCBsZWdhY3kgbWVzc2FnZSBpZHMgdG9cXG4nICtcbiAgICAgICAgICAgICAgICAnYmUgcmVuZGVyZWQuIEZvciBleGFtcGxlLCBpZiB5b3UgYXJlIG5vdCB1c2luZyBsZWdhY3kgbWVzc2FnZSBpZHMgaW4geW91ciB0cmFuc2xhdGlvbiBmaWxlc1xcbicgK1xuICAgICAgICAgICAgICAgICdBTkQgYXJlIG5vdCBkb2luZyBjb21waWxlLXRpbWUgaW5saW5pbmcgb2YgdHJhbnNsYXRpb25zLCBpbiB3aGljaCBjYXNlIHRoZSBleHRyYSBtZXNzYWdlIGlkc1xcbicgK1xuICAgICAgICAgICAgICAgICd3b3VsZCBhZGQgdW53YW50ZWQgc2l6ZSB0byB0aGUgZmluYWwgc291cmNlIGJ1bmRsZS5cXG4nICtcbiAgICAgICAgICAgICAgICAnSXQgaXMgc2FmZSB0byBsZWF2ZSB0aGlzIHNldCB0byB0cnVlIGlmIHlvdSBhcmUgZG9pbmcgY29tcGlsZS10aW1lIGlubGluaW5nIGJlY2F1c2UgdGhlIGV4dHJhXFxuJyArXG4gICAgICAgICAgICAgICAgJ2xlZ2FjeSBtZXNzYWdlIGlkcyB3aWxsIGFsbCBiZSBzdHJpcHBlZCBkdXJpbmcgdHJhbnNsYXRpb24uJyxcbiAgICAgICAgICAgIHR5cGU6ICdib29sZWFuJyxcbiAgICAgICAgICAgIGRlZmF1bHQ6IHRydWUsXG4gICAgICAgICAgfSlcbiAgICAgICAgICAub3B0aW9uKCdhc3luYycsIHtcbiAgICAgICAgICAgIGRlc2NyaWJlOlxuICAgICAgICAgICAgICAgICdXaGV0aGVyIHRvIGNvbXBpbGUgYXN5bmNocm9ub3VzbHkuIFRoaXMgaXMgZW5hYmxlZCBieSBkZWZhdWx0IGFzIGl0IGFsbG93cyBjb21waWxhdGlvbnMgdG8gYmUgcGFyYWxsZWxpemVkLlxcbicgK1xuICAgICAgICAgICAgICAgICdEaXNhYmxpbmcgYXN5bmNocm9ub3VzIGNvbXBpbGF0aW9uIG1heSBiZSB1c2VmdWwgZm9yIGRlYnVnZ2luZy4nLFxuICAgICAgICAgICAgdHlwZTogJ2Jvb2xlYW4nLFxuICAgICAgICAgICAgZGVmYXVsdDogdHJ1ZSxcbiAgICAgICAgICB9KVxuICAgICAgICAgIC5vcHRpb24oJ2wnLCB7XG4gICAgICAgICAgICBhbGlhczogJ2xvZ2xldmVsJyxcbiAgICAgICAgICAgIGRlc2NyaWJlOiAnVGhlIGxvd2VzdCBzZXZlcml0eSBsb2dnaW5nIG1lc3NhZ2UgdGhhdCBzaG91bGQgYmUgb3V0cHV0LicsXG4gICAgICAgICAgICBjaG9pY2VzOiBbJ2RlYnVnJywgJ2luZm8nLCAnd2FybicsICdlcnJvciddLFxuICAgICAgICAgIH0pXG4gICAgICAgICAgLm9wdGlvbignaW52YWxpZGF0ZS1lbnRyeS1wb2ludC1tYW5pZmVzdCcsIHtcbiAgICAgICAgICAgIGRlc2NyaWJlOlxuICAgICAgICAgICAgICAgICdJZiB0aGlzIGlzIHNldCB0aGVuIG5nY2Mgd2lsbCBub3QgcmVhZCBhbiBlbnRyeS1wb2ludCBtYW5pZmVzdCBmaWxlIGZyb20gZGlzay5cXG4nICtcbiAgICAgICAgICAgICAgICAnSW5zdGVhZCBpdCB3aWxsIHdhbGsgdGhlIGRpcmVjdG9yeSB0cmVlIGFzIG5vcm1hbCBsb29raW5nIGZvciBlbnRyeS1wb2ludHMsIGFuZCB0aGVuIHdyaXRlIGEgbmV3IG1hbmlmZXN0IGZpbGUuJyxcbiAgICAgICAgICAgIHR5cGU6ICdib29sZWFuJyxcbiAgICAgICAgICAgIGRlZmF1bHQ6IGZhbHNlLFxuICAgICAgICAgIH0pXG4gICAgICAgICAgLm9wdGlvbignZXJyb3Itb24tZmFpbGVkLWVudHJ5LXBvaW50Jywge1xuICAgICAgICAgICAgZGVzY3JpYmU6XG4gICAgICAgICAgICAgICAgJ1NldCB0aGlzIG9wdGlvbiBpbiBvcmRlciB0byB0ZXJtaW5hdGUgaW1tZWRpYXRlbHkgd2l0aCBhbiBlcnJvciBjb2RlIGlmIGFuIGVudHJ5LXBvaW50IGZhaWxzIHRvIGJlIHByb2Nlc3NlZC5cXG4nICtcbiAgICAgICAgICAgICAgICAnSWYgYC10YC9gLS10YXJnZXRgIGlzIHByb3ZpZGVkIHRoZW4gdGhpcyBwcm9wZXJ0eSBpcyBhbHdheXMgdHJ1ZSBhbmQgY2Fubm90IGJlIGNoYW5nZWQuIE90aGVyd2lzZSB0aGUgZGVmYXVsdCBpcyBmYWxzZS5cXG4nICtcbiAgICAgICAgICAgICAgICAnV2hlbiBzZXQgdG8gZmFsc2UsIG5nY2Mgd2lsbCBjb250aW51ZSB0byBwcm9jZXNzIGVudHJ5LXBvaW50cyBhZnRlciBhIGZhaWx1cmUuIEluIHdoaWNoIGNhc2UgaXQgd2lsbCBsb2cgYW4gZXJyb3IgYW5kIHJlc3VtZSBwcm9jZXNzaW5nIG90aGVyIGVudHJ5LXBvaW50cy4nLFxuICAgICAgICAgICAgdHlwZTogJ2Jvb2xlYW4nLFxuICAgICAgICAgICAgZGVmYXVsdDogZmFsc2UsXG4gICAgICAgICAgfSlcbiAgICAgICAgICAub3B0aW9uKCd0c2NvbmZpZycsIHtcbiAgICAgICAgICAgIGRlc2NyaWJlOlxuICAgICAgICAgICAgICAgICdBIHBhdGggdG8gYSB0c2NvbmZpZy5qc29uIGZpbGUgdGhhdCB3aWxsIGJlIHVzZWQgdG8gY29uZmlndXJlIHRoZSBBbmd1bGFyIGNvbXBpbGVyIGFuZCBtb2R1bGUgcmVzb2x1dGlvbiB1c2VkIGJ5IG5nY2MuXFxuJyArXG4gICAgICAgICAgICAgICAgJ0lmIG5vdCBwcm92aWRlZCwgbmdjYyB3aWxsIGF0dGVtcHQgdG8gcmVhZCBhIGB0c2NvbmZpZy5qc29uYCBmaWxlIGZyb20gdGhlIGZvbGRlciBhYm92ZSB0aGF0IGdpdmVuIGJ5IHRoZSBgLXNgIG9wdGlvbi5cXG4nICtcbiAgICAgICAgICAgICAgICAnU2V0IHRvIGZhbHNlICh2aWEgYC0tbm8tdHNjb25maWdgKSBpZiB5b3UgZG8gbm90IHdhbnQgbmdjYyB0byB1c2UgYW55IGB0c2NvbmZpZy5qc29uYCBmaWxlLicsXG4gICAgICAgICAgICB0eXBlOiAnc3RyaW5nJyxcbiAgICAgICAgICB9KVxuICAgICAgICAgIC5zdHJpY3QoKVxuICAgICAgICAgIC5oZWxwKClcbiAgICAgICAgICAucGFyc2UoYXJncyk7XG5cbiAgaWYgKG9wdGlvbnNbJ2YnXSAmJiBvcHRpb25zWydmJ10ubGVuZ3RoKSB7XG4gICAgY29uc29sZS5lcnJvcihcbiAgICAgICAgJ1RoZSBmb3JtYXRzIG9wdGlvbiAoLWYvLS1mb3JtYXRzKSBoYXMgYmVlbiByZW1vdmVkLiBDb25zaWRlciB0aGUgcHJvcGVydGllcyBvcHRpb24gKC1wLy0tcHJvcGVydGllcykgaW5zdGVhZC4nKTtcbiAgICBwcm9jZXNzLmV4aXQoMSk7XG4gIH1cblxuICBzZXRGaWxlU3lzdGVtKG5ldyBOb2RlSlNGaWxlU3lzdGVtKCkpO1xuXG4gIGNvbnN0IGJhc2VTb3VyY2VQYXRoID0gcmVzb2x2ZShvcHRpb25zWydzJ10gfHwgJy4vbm9kZV9tb2R1bGVzJyk7XG4gIGNvbnN0IHByb3BlcnRpZXNUb0NvbnNpZGVyOiBzdHJpbmdbXSA9IG9wdGlvbnNbJ3AnXTtcbiAgY29uc3QgdGFyZ2V0RW50cnlQb2ludFBhdGggPSBvcHRpb25zWyd0J10gPyBvcHRpb25zWyd0J10gOiB1bmRlZmluZWQ7XG4gIGNvbnN0IGNvbXBpbGVBbGxGb3JtYXRzID0gIW9wdGlvbnNbJ2ZpcnN0LW9ubHknXTtcbiAgY29uc3QgY3JlYXRlTmV3RW50cnlQb2ludEZvcm1hdHMgPSBvcHRpb25zWydjcmVhdGUtaXZ5LWVudHJ5LXBvaW50cyddO1xuICBjb25zdCBsb2dMZXZlbCA9IG9wdGlvbnNbJ2wnXSBhcyBrZXlvZiB0eXBlb2YgTG9nTGV2ZWwgfCB1bmRlZmluZWQ7XG4gIGNvbnN0IGVuYWJsZUkxOG5MZWdhY3lNZXNzYWdlSWRGb3JtYXQgPSBvcHRpb25zWydsZWdhY3ktbWVzc2FnZS1pZHMnXTtcbiAgY29uc3QgaW52YWxpZGF0ZUVudHJ5UG9pbnRNYW5pZmVzdCA9IG9wdGlvbnNbJ2ludmFsaWRhdGUtZW50cnktcG9pbnQtbWFuaWZlc3QnXTtcbiAgY29uc3QgZXJyb3JPbkZhaWxlZEVudHJ5UG9pbnQgPSBvcHRpb25zWydlcnJvci1vbi1mYWlsZWQtZW50cnktcG9pbnQnXTtcbiAgY29uc3QgZmluZEVudHJ5UG9pbnRzRnJvbVRzQ29uZmlnUHJvZ3JhbSA9IG9wdGlvbnNbJ3VzZS1wcm9ncmFtLWRlcGVuZGVuY2llcyddO1xuICAvLyB5YXJncyBpcyBub3Qgc28gZ3JlYXQgYXQgbWl4ZWQgc3RyaW5nK2Jvb2xlYW4gdHlwZXMsIHNvIHdlIGhhdmUgdG8gdGVzdCB0c2NvbmZpZyBhZ2FpbnN0IGFcbiAgLy8gc3RyaW5nIFwiZmFsc2VcIiB0byBjYXB0dXJlIHRoZSBgdHNjb25maWc9ZmFsc2VgIG9wdGlvbi5cbiAgLy8gQW5kIHdlIGhhdmUgdG8gY29udmVydCB0aGUgb3B0aW9uIHRvIGEgc3RyaW5nIHRvIGhhbmRsZSBgbm8tdHNjb25maWdgLCB3aGljaCB3aWxsIGJlIGBmYWxzZWAuXG4gIGNvbnN0IHRzQ29uZmlnUGF0aCA9IGAke29wdGlvbnNbJ3RzY29uZmlnJ119YCA9PT0gJ2ZhbHNlJyA/IG51bGwgOiBvcHRpb25zWyd0c2NvbmZpZyddO1xuXG4gIGNvbnN0IGxvZ2dlciA9IGxvZ0xldmVsICYmIG5ldyBDb25zb2xlTG9nZ2VyKExvZ0xldmVsW2xvZ0xldmVsXSk7XG5cbiAgcmV0dXJuIHtcbiAgICBiYXNlUGF0aDogYmFzZVNvdXJjZVBhdGgsXG4gICAgcHJvcGVydGllc1RvQ29uc2lkZXIsXG4gICAgdGFyZ2V0RW50cnlQb2ludFBhdGgsXG4gICAgY29tcGlsZUFsbEZvcm1hdHMsXG4gICAgY3JlYXRlTmV3RW50cnlQb2ludEZvcm1hdHMsXG4gICAgbG9nZ2VyLFxuICAgIGVuYWJsZUkxOG5MZWdhY3lNZXNzYWdlSWRGb3JtYXQsXG4gICAgYXN5bmM6IG9wdGlvbnNbJ2FzeW5jJ10sXG4gICAgaW52YWxpZGF0ZUVudHJ5UG9pbnRNYW5pZmVzdCxcbiAgICBlcnJvck9uRmFpbGVkRW50cnlQb2ludCxcbiAgICB0c0NvbmZpZ1BhdGgsXG4gICAgZmluZEVudHJ5UG9pbnRzRnJvbVRzQ29uZmlnUHJvZ3JhbSxcbiAgfTtcbn1cbiJdfQ==