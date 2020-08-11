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
        define("@angular/compiler-cli/src/ngtsc/typecheck/src/augmented_program", ["require", "exports", "tslib", "typescript", "@angular/compiler-cli/src/ngtsc/shims", "@angular/compiler-cli/src/ngtsc/typecheck/src/api", "@angular/compiler-cli/src/ngtsc/typecheck/src/host"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.ReusedProgramStrategy = void 0;
    var tslib_1 = require("tslib");
    var ts = require("typescript");
    var shims_1 = require("@angular/compiler-cli/src/ngtsc/shims");
    var api_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/api");
    var host_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/host");
    /**
     * Implements a template type-checking program using `ts.createProgram` and TypeScript's program
     * reuse functionality.
     */
    var ReusedProgramStrategy = /** @class */ (function () {
        function ReusedProgramStrategy(originalProgram, originalHost, options, shimExtensionPrefixes) {
            this.originalProgram = originalProgram;
            this.originalHost = originalHost;
            this.options = options;
            this.shimExtensionPrefixes = shimExtensionPrefixes;
            /**
             * A map of source file paths to replacement `ts.SourceFile`s for those paths.
             *
             * Effectively, this tracks the delta between the user's program (represented by the
             * `originalHost`) and the template type-checking program being managed.
             */
            this.sfMap = new Map();
            this.program = this.originalProgram;
        }
        ReusedProgramStrategy.prototype.getProgram = function () {
            return this.program;
        };
        ReusedProgramStrategy.prototype.updateFiles = function (contents, updateMode) {
            var e_1, _a;
            if (contents.size === 0) {
                // No changes have been requested. Is it safe to skip updating entirely?
                // If UpdateMode is Incremental, then yes. If UpdateMode is Complete, then it's safe to skip
                // only if there are no active changes already (that would be cleared by the update).
                if (updateMode !== api_1.UpdateMode.Complete || this.sfMap.size === 0) {
                    // No changes would be made to the `ts.Program` anyway, so it's safe to do nothing here.
                    return;
                }
            }
            if (updateMode === api_1.UpdateMode.Complete) {
                this.sfMap.clear();
            }
            try {
                for (var _b = tslib_1.__values(contents.entries()), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var _d = tslib_1.__read(_c.value, 2), filePath = _d[0], text = _d[1];
                    this.sfMap.set(filePath, ts.createSourceFile(filePath, text, ts.ScriptTarget.Latest, true));
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                }
                finally { if (e_1) throw e_1.error; }
            }
            var host = new host_1.TypeCheckProgramHost(this.sfMap, this.originalProgram, this.originalHost, this.shimExtensionPrefixes);
            var oldProgram = this.program;
            // Retag the old program's `ts.SourceFile`s with shim tags, to allow TypeScript to reuse the
            // most data.
            shims_1.retagAllTsFiles(oldProgram);
            this.program = ts.createProgram({
                host: host,
                rootNames: this.program.getRootFileNames(),
                options: this.options,
                oldProgram: oldProgram,
            });
            host.postProgramCreationCleanup();
            // And untag them afterwards. We explicitly untag both programs here, because the oldProgram
            // may still be used for emit and needs to not contain tags.
            shims_1.untagAllTsFiles(this.program);
            shims_1.untagAllTsFiles(oldProgram);
        };
        return ReusedProgramStrategy;
    }());
    exports.ReusedProgramStrategy = ReusedProgramStrategy;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYXVnbWVudGVkX3Byb2dyYW0uanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9jb21waWxlci1jbGkvc3JjL25ndHNjL3R5cGVjaGVjay9zcmMvYXVnbWVudGVkX3Byb2dyYW0udHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOzs7Ozs7Ozs7Ozs7OztJQUVILCtCQUFpQztJQUdqQywrREFBNkQ7SUFFN0QseUVBQThEO0lBQzlELDJFQUE0QztJQUU1Qzs7O09BR0c7SUFDSDtRQVdFLCtCQUNZLGVBQTJCLEVBQVUsWUFBNkIsRUFDbEUsT0FBMkIsRUFBVSxxQkFBK0I7WUFEcEUsb0JBQWUsR0FBZixlQUFlLENBQVk7WUFBVSxpQkFBWSxHQUFaLFlBQVksQ0FBaUI7WUFDbEUsWUFBTyxHQUFQLE9BQU8sQ0FBb0I7WUFBVSwwQkFBcUIsR0FBckIscUJBQXFCLENBQVU7WUFaaEY7Ozs7O2VBS0c7WUFDSyxVQUFLLEdBQUcsSUFBSSxHQUFHLEVBQXlCLENBQUM7WUFFekMsWUFBTyxHQUFlLElBQUksQ0FBQyxlQUFlLENBQUM7UUFJZ0MsQ0FBQztRQUVwRiwwQ0FBVSxHQUFWO1lBQ0UsT0FBTyxJQUFJLENBQUMsT0FBTyxDQUFDO1FBQ3RCLENBQUM7UUFFRCwyQ0FBVyxHQUFYLFVBQVksUUFBcUMsRUFBRSxVQUFzQjs7WUFDdkUsSUFBSSxRQUFRLENBQUMsSUFBSSxLQUFLLENBQUMsRUFBRTtnQkFDdkIsd0VBQXdFO2dCQUN4RSw0RkFBNEY7Z0JBQzVGLHFGQUFxRjtnQkFFckYsSUFBSSxVQUFVLEtBQUssZ0JBQVUsQ0FBQyxRQUFRLElBQUksSUFBSSxDQUFDLEtBQUssQ0FBQyxJQUFJLEtBQUssQ0FBQyxFQUFFO29CQUMvRCx3RkFBd0Y7b0JBQ3hGLE9BQU87aUJBQ1I7YUFDRjtZQUVELElBQUksVUFBVSxLQUFLLGdCQUFVLENBQUMsUUFBUSxFQUFFO2dCQUN0QyxJQUFJLENBQUMsS0FBSyxDQUFDLEtBQUssRUFBRSxDQUFDO2FBQ3BCOztnQkFFRCxLQUErQixJQUFBLEtBQUEsaUJBQUEsUUFBUSxDQUFDLE9BQU8sRUFBRSxDQUFBLGdCQUFBLDRCQUFFO29CQUF4QyxJQUFBLEtBQUEsMkJBQWdCLEVBQWYsUUFBUSxRQUFBLEVBQUUsSUFBSSxRQUFBO29CQUN4QixJQUFJLENBQUMsS0FBSyxDQUFDLEdBQUcsQ0FBQyxRQUFRLEVBQUUsRUFBRSxDQUFDLGdCQUFnQixDQUFDLFFBQVEsRUFBRSxJQUFJLEVBQUUsRUFBRSxDQUFDLFlBQVksQ0FBQyxNQUFNLEVBQUUsSUFBSSxDQUFDLENBQUMsQ0FBQztpQkFDN0Y7Ozs7Ozs7OztZQUVELElBQU0sSUFBSSxHQUFHLElBQUksMkJBQW9CLENBQ2pDLElBQUksQ0FBQyxLQUFLLEVBQUUsSUFBSSxDQUFDLGVBQWUsRUFBRSxJQUFJLENBQUMsWUFBWSxFQUFFLElBQUksQ0FBQyxxQkFBcUIsQ0FBQyxDQUFDO1lBQ3JGLElBQU0sVUFBVSxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUM7WUFFaEMsNEZBQTRGO1lBQzVGLGFBQWE7WUFDYix1QkFBZSxDQUFDLFVBQVUsQ0FBQyxDQUFDO1lBRTVCLElBQUksQ0FBQyxPQUFPLEdBQUcsRUFBRSxDQUFDLGFBQWEsQ0FBQztnQkFDOUIsSUFBSSxNQUFBO2dCQUNKLFNBQVMsRUFBRSxJQUFJLENBQUMsT0FBTyxDQUFDLGdCQUFnQixFQUFFO2dCQUMxQyxPQUFPLEVBQUUsSUFBSSxDQUFDLE9BQU87Z0JBQ3JCLFVBQVUsWUFBQTthQUNYLENBQUMsQ0FBQztZQUNILElBQUksQ0FBQywwQkFBMEIsRUFBRSxDQUFDO1lBRWxDLDRGQUE0RjtZQUM1Riw0REFBNEQ7WUFDNUQsdUJBQWUsQ0FBQyxJQUFJLENBQUMsT0FBTyxDQUFDLENBQUM7WUFDOUIsdUJBQWUsQ0FBQyxVQUFVLENBQUMsQ0FBQztRQUM5QixDQUFDO1FBQ0gsNEJBQUM7SUFBRCxDQUFDLEFBNURELElBNERDO0lBNURZLHNEQUFxQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQgKiBhcyB0cyBmcm9tICd0eXBlc2NyaXB0JztcblxuaW1wb3J0IHtBYnNvbHV0ZUZzUGF0aH0gZnJvbSAnLi4vLi4vZmlsZV9zeXN0ZW0nO1xuaW1wb3J0IHtyZXRhZ0FsbFRzRmlsZXMsIHVudGFnQWxsVHNGaWxlc30gZnJvbSAnLi4vLi4vc2hpbXMnO1xuXG5pbXBvcnQge1R5cGVDaGVja2luZ1Byb2dyYW1TdHJhdGVneSwgVXBkYXRlTW9kZX0gZnJvbSAnLi9hcGknO1xuaW1wb3J0IHtUeXBlQ2hlY2tQcm9ncmFtSG9zdH0gZnJvbSAnLi9ob3N0JztcblxuLyoqXG4gKiBJbXBsZW1lbnRzIGEgdGVtcGxhdGUgdHlwZS1jaGVja2luZyBwcm9ncmFtIHVzaW5nIGB0cy5jcmVhdGVQcm9ncmFtYCBhbmQgVHlwZVNjcmlwdCdzIHByb2dyYW1cbiAqIHJldXNlIGZ1bmN0aW9uYWxpdHkuXG4gKi9cbmV4cG9ydCBjbGFzcyBSZXVzZWRQcm9ncmFtU3RyYXRlZ3kgaW1wbGVtZW50cyBUeXBlQ2hlY2tpbmdQcm9ncmFtU3RyYXRlZ3kge1xuICAvKipcbiAgICogQSBtYXAgb2Ygc291cmNlIGZpbGUgcGF0aHMgdG8gcmVwbGFjZW1lbnQgYHRzLlNvdXJjZUZpbGVgcyBmb3IgdGhvc2UgcGF0aHMuXG4gICAqXG4gICAqIEVmZmVjdGl2ZWx5LCB0aGlzIHRyYWNrcyB0aGUgZGVsdGEgYmV0d2VlbiB0aGUgdXNlcidzIHByb2dyYW0gKHJlcHJlc2VudGVkIGJ5IHRoZVxuICAgKiBgb3JpZ2luYWxIb3N0YCkgYW5kIHRoZSB0ZW1wbGF0ZSB0eXBlLWNoZWNraW5nIHByb2dyYW0gYmVpbmcgbWFuYWdlZC5cbiAgICovXG4gIHByaXZhdGUgc2ZNYXAgPSBuZXcgTWFwPHN0cmluZywgdHMuU291cmNlRmlsZT4oKTtcblxuICBwcml2YXRlIHByb2dyYW06IHRzLlByb2dyYW0gPSB0aGlzLm9yaWdpbmFsUHJvZ3JhbTtcblxuICBjb25zdHJ1Y3RvcihcbiAgICAgIHByaXZhdGUgb3JpZ2luYWxQcm9ncmFtOiB0cy5Qcm9ncmFtLCBwcml2YXRlIG9yaWdpbmFsSG9zdDogdHMuQ29tcGlsZXJIb3N0LFxuICAgICAgcHJpdmF0ZSBvcHRpb25zOiB0cy5Db21waWxlck9wdGlvbnMsIHByaXZhdGUgc2hpbUV4dGVuc2lvblByZWZpeGVzOiBzdHJpbmdbXSkge31cblxuICBnZXRQcm9ncmFtKCk6IHRzLlByb2dyYW0ge1xuICAgIHJldHVybiB0aGlzLnByb2dyYW07XG4gIH1cblxuICB1cGRhdGVGaWxlcyhjb250ZW50czogTWFwPEFic29sdXRlRnNQYXRoLCBzdHJpbmc+LCB1cGRhdGVNb2RlOiBVcGRhdGVNb2RlKTogdm9pZCB7XG4gICAgaWYgKGNvbnRlbnRzLnNpemUgPT09IDApIHtcbiAgICAgIC8vIE5vIGNoYW5nZXMgaGF2ZSBiZWVuIHJlcXVlc3RlZC4gSXMgaXQgc2FmZSB0byBza2lwIHVwZGF0aW5nIGVudGlyZWx5P1xuICAgICAgLy8gSWYgVXBkYXRlTW9kZSBpcyBJbmNyZW1lbnRhbCwgdGhlbiB5ZXMuIElmIFVwZGF0ZU1vZGUgaXMgQ29tcGxldGUsIHRoZW4gaXQncyBzYWZlIHRvIHNraXBcbiAgICAgIC8vIG9ubHkgaWYgdGhlcmUgYXJlIG5vIGFjdGl2ZSBjaGFuZ2VzIGFscmVhZHkgKHRoYXQgd291bGQgYmUgY2xlYXJlZCBieSB0aGUgdXBkYXRlKS5cblxuICAgICAgaWYgKHVwZGF0ZU1vZGUgIT09IFVwZGF0ZU1vZGUuQ29tcGxldGUgfHwgdGhpcy5zZk1hcC5zaXplID09PSAwKSB7XG4gICAgICAgIC8vIE5vIGNoYW5nZXMgd291bGQgYmUgbWFkZSB0byB0aGUgYHRzLlByb2dyYW1gIGFueXdheSwgc28gaXQncyBzYWZlIHRvIGRvIG5vdGhpbmcgaGVyZS5cbiAgICAgICAgcmV0dXJuO1xuICAgICAgfVxuICAgIH1cblxuICAgIGlmICh1cGRhdGVNb2RlID09PSBVcGRhdGVNb2RlLkNvbXBsZXRlKSB7XG4gICAgICB0aGlzLnNmTWFwLmNsZWFyKCk7XG4gICAgfVxuXG4gICAgZm9yIChjb25zdCBbZmlsZVBhdGgsIHRleHRdIG9mIGNvbnRlbnRzLmVudHJpZXMoKSkge1xuICAgICAgdGhpcy5zZk1hcC5zZXQoZmlsZVBhdGgsIHRzLmNyZWF0ZVNvdXJjZUZpbGUoZmlsZVBhdGgsIHRleHQsIHRzLlNjcmlwdFRhcmdldC5MYXRlc3QsIHRydWUpKTtcbiAgICB9XG5cbiAgICBjb25zdCBob3N0ID0gbmV3IFR5cGVDaGVja1Byb2dyYW1Ib3N0KFxuICAgICAgICB0aGlzLnNmTWFwLCB0aGlzLm9yaWdpbmFsUHJvZ3JhbSwgdGhpcy5vcmlnaW5hbEhvc3QsIHRoaXMuc2hpbUV4dGVuc2lvblByZWZpeGVzKTtcbiAgICBjb25zdCBvbGRQcm9ncmFtID0gdGhpcy5wcm9ncmFtO1xuXG4gICAgLy8gUmV0YWcgdGhlIG9sZCBwcm9ncmFtJ3MgYHRzLlNvdXJjZUZpbGVgcyB3aXRoIHNoaW0gdGFncywgdG8gYWxsb3cgVHlwZVNjcmlwdCB0byByZXVzZSB0aGVcbiAgICAvLyBtb3N0IGRhdGEuXG4gICAgcmV0YWdBbGxUc0ZpbGVzKG9sZFByb2dyYW0pO1xuXG4gICAgdGhpcy5wcm9ncmFtID0gdHMuY3JlYXRlUHJvZ3JhbSh7XG4gICAgICBob3N0LFxuICAgICAgcm9vdE5hbWVzOiB0aGlzLnByb2dyYW0uZ2V0Um9vdEZpbGVOYW1lcygpLFxuICAgICAgb3B0aW9uczogdGhpcy5vcHRpb25zLFxuICAgICAgb2xkUHJvZ3JhbSxcbiAgICB9KTtcbiAgICBob3N0LnBvc3RQcm9ncmFtQ3JlYXRpb25DbGVhbnVwKCk7XG5cbiAgICAvLyBBbmQgdW50YWcgdGhlbSBhZnRlcndhcmRzLiBXZSBleHBsaWNpdGx5IHVudGFnIGJvdGggcHJvZ3JhbXMgaGVyZSwgYmVjYXVzZSB0aGUgb2xkUHJvZ3JhbVxuICAgIC8vIG1heSBzdGlsbCBiZSB1c2VkIGZvciBlbWl0IGFuZCBuZWVkcyB0byBub3QgY29udGFpbiB0YWdzLlxuICAgIHVudGFnQWxsVHNGaWxlcyh0aGlzLnByb2dyYW0pO1xuICAgIHVudGFnQWxsVHNGaWxlcyhvbGRQcm9ncmFtKTtcbiAgfVxufVxuIl19