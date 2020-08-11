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
        define("@angular/compiler-cli/src/ngtsc/typecheck/src/checker", ["require", "exports", "tslib", "@angular/compiler-cli/src/ngtsc/file_system", "@angular/compiler-cli/src/ngtsc/shims", "@angular/compiler-cli/src/ngtsc/typecheck/src/api", "@angular/compiler-cli/src/ngtsc/typecheck/src/context", "@angular/compiler-cli/src/ngtsc/typecheck/src/diagnostics"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.TemplateTypeChecker = void 0;
    var tslib_1 = require("tslib");
    var file_system_1 = require("@angular/compiler-cli/src/ngtsc/file_system");
    var shims_1 = require("@angular/compiler-cli/src/ngtsc/shims");
    var api_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/api");
    var context_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/context");
    var diagnostics_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/diagnostics");
    /**
     * Primary template type-checking engine, which performs type-checking using a
     * `TypeCheckingProgramStrategy` for type-checking program maintenance, and the
     * `ProgramTypeCheckAdapter` for generation of template type-checking code.
     */
    var TemplateTypeChecker = /** @class */ (function () {
        function TemplateTypeChecker(originalProgram, typeCheckingStrategy, typeCheckAdapter, config, refEmitter, reflector, compilerHost, priorBuild) {
            this.originalProgram = originalProgram;
            this.typeCheckingStrategy = typeCheckingStrategy;
            this.typeCheckAdapter = typeCheckAdapter;
            this.config = config;
            this.refEmitter = refEmitter;
            this.reflector = reflector;
            this.compilerHost = compilerHost;
            this.priorBuild = priorBuild;
            this.files = new Map();
        }
        /**
         * Reset the internal type-checking program by generating type-checking code from the user's
         * program.
         */
        TemplateTypeChecker.prototype.refresh = function () {
            var e_1, _a, e_2, _b;
            this.files.clear();
            var ctx = new context_1.TypeCheckContext(this.config, this.compilerHost, this.refEmitter, this.reflector);
            try {
                // Typecheck all the files.
                for (var _c = tslib_1.__values(this.originalProgram.getSourceFiles()), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var sf = _d.value;
                    if (sf.isDeclarationFile || shims_1.isShim(sf)) {
                        continue;
                    }
                    var previousResults = this.priorBuild.priorTypeCheckingResultsFor(sf);
                    if (previousResults === null) {
                        // Previous results were not available, so generate new type-checking code for this file.
                        this.typeCheckAdapter.typeCheck(sf, ctx);
                    }
                    else {
                        // Previous results were available, and can be adopted into the current build.
                        ctx.adoptPriorResults(sf, previousResults);
                    }
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                }
                finally { if (e_1) throw e_1.error; }
            }
            var results = ctx.finalize();
            this.typeCheckingStrategy.updateFiles(results.updates, api_1.UpdateMode.Complete);
            try {
                for (var _e = tslib_1.__values(results.perFileData), _f = _e.next(); !_f.done; _f = _e.next()) {
                    var _g = tslib_1.__read(_f.value, 2), file = _g[0], fileData = _g[1];
                    this.files.set(file, fileData);
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
                }
                finally { if (e_2) throw e_2.error; }
            }
            return results;
        };
        /**
         * Retrieve type-checking diagnostics from the given `ts.SourceFile` using the most recent
         * type-checking program.
         */
        TemplateTypeChecker.prototype.getDiagnosticsForFile = function (sf) {
            var path = file_system_1.absoluteFromSourceFile(sf);
            if (!this.files.has(path)) {
                return [];
            }
            var record = this.files.get(path);
            var typeCheckProgram = this.typeCheckingStrategy.getProgram();
            var typeCheckSf = file_system_1.getSourceFileOrError(typeCheckProgram, record.typeCheckFile);
            var rawDiagnostics = [];
            rawDiagnostics.push.apply(rawDiagnostics, tslib_1.__spread(typeCheckProgram.getSemanticDiagnostics(typeCheckSf)));
            if (record.hasInlines) {
                var inlineSf = file_system_1.getSourceFileOrError(typeCheckProgram, path);
                rawDiagnostics.push.apply(rawDiagnostics, tslib_1.__spread(typeCheckProgram.getSemanticDiagnostics(inlineSf)));
            }
            return rawDiagnostics
                .map(function (diag) {
                if (!diagnostics_1.shouldReportDiagnostic(diag)) {
                    return null;
                }
                return diagnostics_1.translateDiagnostic(diag, record.sourceResolver);
            })
                .filter(function (diag) { return diag !== null; })
                .concat(record.genesisDiagnostics);
        };
        return TemplateTypeChecker;
    }());
    exports.TemplateTypeChecker = TemplateTypeChecker;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2hlY2tlci5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2NvbXBpbGVyLWNsaS9zcmMvbmd0c2MvdHlwZWNoZWNrL3NyYy9jaGVja2VyLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7Ozs7Ozs7Ozs7Ozs7SUFJSCwyRUFBK0Y7SUFJL0YsK0RBQW1DO0lBRW5DLHlFQUFrRjtJQUNsRixpRkFBbUY7SUFDbkYseUZBQTBFO0lBVTFFOzs7O09BSUc7SUFDSDtRQUdFLDZCQUNZLGVBQTJCLEVBQzNCLG9CQUFpRCxFQUNqRCxnQkFBeUMsRUFBVSxNQUEwQixFQUM3RSxVQUE0QixFQUFVLFNBQXlCLEVBQy9ELFlBQTJELEVBQzNELFVBQTJEO1lBTDNELG9CQUFlLEdBQWYsZUFBZSxDQUFZO1lBQzNCLHlCQUFvQixHQUFwQixvQkFBb0IsQ0FBNkI7WUFDakQscUJBQWdCLEdBQWhCLGdCQUFnQixDQUF5QjtZQUFVLFdBQU0sR0FBTixNQUFNLENBQW9CO1lBQzdFLGVBQVUsR0FBVixVQUFVLENBQWtCO1lBQVUsY0FBUyxHQUFULFNBQVMsQ0FBZ0I7WUFDL0QsaUJBQVksR0FBWixZQUFZLENBQStDO1lBQzNELGVBQVUsR0FBVixVQUFVLENBQWlEO1lBUi9ELFVBQUssR0FBRyxJQUFJLEdBQUcsRUFBd0MsQ0FBQztRQVFVLENBQUM7UUFFM0U7OztXQUdHO1FBQ0gscUNBQU8sR0FBUDs7WUFDRSxJQUFJLENBQUMsS0FBSyxDQUFDLEtBQUssRUFBRSxDQUFDO1lBRW5CLElBQU0sR0FBRyxHQUNMLElBQUksMEJBQWdCLENBQUMsSUFBSSxDQUFDLE1BQU0sRUFBRSxJQUFJLENBQUMsWUFBWSxFQUFFLElBQUksQ0FBQyxVQUFVLEVBQUUsSUFBSSxDQUFDLFNBQVMsQ0FBQyxDQUFDOztnQkFFMUYsMkJBQTJCO2dCQUMzQixLQUFpQixJQUFBLEtBQUEsaUJBQUEsSUFBSSxDQUFDLGVBQWUsQ0FBQyxjQUFjLEVBQUUsQ0FBQSxnQkFBQSw0QkFBRTtvQkFBbkQsSUFBTSxFQUFFLFdBQUE7b0JBQ1gsSUFBSSxFQUFFLENBQUMsaUJBQWlCLElBQUksY0FBTSxDQUFDLEVBQUUsQ0FBQyxFQUFFO3dCQUN0QyxTQUFTO3FCQUNWO29CQUVELElBQU0sZUFBZSxHQUFHLElBQUksQ0FBQyxVQUFVLENBQUMsMkJBQTJCLENBQUMsRUFBRSxDQUFDLENBQUM7b0JBQ3hFLElBQUksZUFBZSxLQUFLLElBQUksRUFBRTt3QkFDNUIseUZBQXlGO3dCQUN6RixJQUFJLENBQUMsZ0JBQWdCLENBQUMsU0FBUyxDQUFDLEVBQUUsRUFBRSxHQUFHLENBQUMsQ0FBQztxQkFDMUM7eUJBQU07d0JBQ0wsOEVBQThFO3dCQUM5RSxHQUFHLENBQUMsaUJBQWlCLENBQUMsRUFBRSxFQUFFLGVBQWUsQ0FBQyxDQUFDO3FCQUM1QztpQkFDRjs7Ozs7Ozs7O1lBRUQsSUFBTSxPQUFPLEdBQUcsR0FBRyxDQUFDLFFBQVEsRUFBRSxDQUFDO1lBQy9CLElBQUksQ0FBQyxvQkFBb0IsQ0FBQyxXQUFXLENBQUMsT0FBTyxDQUFDLE9BQU8sRUFBRSxnQkFBVSxDQUFDLFFBQVEsQ0FBQyxDQUFDOztnQkFDNUUsS0FBK0IsSUFBQSxLQUFBLGlCQUFBLE9BQU8sQ0FBQyxXQUFXLENBQUEsZ0JBQUEsNEJBQUU7b0JBQXpDLElBQUEsS0FBQSwyQkFBZ0IsRUFBZixJQUFJLFFBQUEsRUFBRSxRQUFRLFFBQUE7b0JBQ3hCLElBQUksQ0FBQyxLQUFLLENBQUMsR0FBRyxDQUFDLElBQUksRUFBRSxRQUFRLENBQUMsQ0FBQztpQkFDaEM7Ozs7Ozs7OztZQUVELE9BQU8sT0FBTyxDQUFDO1FBQ2pCLENBQUM7UUFFRDs7O1dBR0c7UUFDSCxtREFBcUIsR0FBckIsVUFBc0IsRUFBaUI7WUFDckMsSUFBTSxJQUFJLEdBQUcsb0NBQXNCLENBQUMsRUFBRSxDQUFDLENBQUM7WUFDeEMsSUFBSSxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsR0FBRyxDQUFDLElBQUksQ0FBQyxFQUFFO2dCQUN6QixPQUFPLEVBQUUsQ0FBQzthQUNYO1lBQ0QsSUFBTSxNQUFNLEdBQUcsSUFBSSxDQUFDLEtBQUssQ0FBQyxHQUFHLENBQUMsSUFBSSxDQUFFLENBQUM7WUFFckMsSUFBTSxnQkFBZ0IsR0FBRyxJQUFJLENBQUMsb0JBQW9CLENBQUMsVUFBVSxFQUFFLENBQUM7WUFDaEUsSUFBTSxXQUFXLEdBQUcsa0NBQW9CLENBQUMsZ0JBQWdCLEVBQUUsTUFBTSxDQUFDLGFBQWEsQ0FBQyxDQUFDO1lBQ2pGLElBQU0sY0FBYyxHQUFHLEVBQUUsQ0FBQztZQUMxQixjQUFjLENBQUMsSUFBSSxPQUFuQixjQUFjLG1CQUFTLGdCQUFnQixDQUFDLHNCQUFzQixDQUFDLFdBQVcsQ0FBQyxHQUFFO1lBQzdFLElBQUksTUFBTSxDQUFDLFVBQVUsRUFBRTtnQkFDckIsSUFBTSxRQUFRLEdBQUcsa0NBQW9CLENBQUMsZ0JBQWdCLEVBQUUsSUFBSSxDQUFDLENBQUM7Z0JBQzlELGNBQWMsQ0FBQyxJQUFJLE9BQW5CLGNBQWMsbUJBQVMsZ0JBQWdCLENBQUMsc0JBQXNCLENBQUMsUUFBUSxDQUFDLEdBQUU7YUFDM0U7WUFFRCxPQUFPLGNBQWM7aUJBQ2hCLEdBQUcsQ0FBQyxVQUFBLElBQUk7Z0JBQ1AsSUFBSSxDQUFDLG9DQUFzQixDQUFDLElBQUksQ0FBQyxFQUFFO29CQUNqQyxPQUFPLElBQUksQ0FBQztpQkFDYjtnQkFDRCxPQUFPLGlDQUFtQixDQUFDLElBQUksRUFBRSxNQUFNLENBQUMsY0FBYyxDQUFDLENBQUM7WUFDMUQsQ0FBQyxDQUFDO2lCQUNELE1BQU0sQ0FBQyxVQUFDLElBQXdCLElBQTRCLE9BQUEsSUFBSSxLQUFLLElBQUksRUFBYixDQUFhLENBQUM7aUJBQzFFLE1BQU0sQ0FBQyxNQUFNLENBQUMsa0JBQWtCLENBQUMsQ0FBQztRQUN6QyxDQUFDO1FBQ0gsMEJBQUM7SUFBRCxDQUFDLEFBNUVELElBNEVDO0lBNUVZLGtEQUFtQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQgKiBhcyB0cyBmcm9tICd0eXBlc2NyaXB0JztcblxuaW1wb3J0IHthYnNvbHV0ZUZyb21Tb3VyY2VGaWxlLCBBYnNvbHV0ZUZzUGF0aCwgZ2V0U291cmNlRmlsZU9yRXJyb3J9IGZyb20gJy4uLy4uL2ZpbGVfc3lzdGVtJztcbmltcG9ydCB7UmVmZXJlbmNlRW1pdHRlcn0gZnJvbSAnLi4vLi4vaW1wb3J0cyc7XG5pbXBvcnQge0luY3JlbWVudGFsQnVpbGR9IGZyb20gJy4uLy4uL2luY3JlbWVudGFsL2FwaSc7XG5pbXBvcnQge1JlZmxlY3Rpb25Ib3N0fSBmcm9tICcuLi8uLi9yZWZsZWN0aW9uJztcbmltcG9ydCB7aXNTaGltfSBmcm9tICcuLi8uLi9zaGltcyc7XG5cbmltcG9ydCB7VHlwZUNoZWNraW5nQ29uZmlnLCBUeXBlQ2hlY2tpbmdQcm9ncmFtU3RyYXRlZ3ksIFVwZGF0ZU1vZGV9IGZyb20gJy4vYXBpJztcbmltcG9ydCB7RmlsZVR5cGVDaGVja2luZ0RhdGEsIFR5cGVDaGVja0NvbnRleHQsIFR5cGVDaGVja1JlcXVlc3R9IGZyb20gJy4vY29udGV4dCc7XG5pbXBvcnQge3Nob3VsZFJlcG9ydERpYWdub3N0aWMsIHRyYW5zbGF0ZURpYWdub3N0aWN9IGZyb20gJy4vZGlhZ25vc3RpY3MnO1xuXG4vKipcbiAqIEludGVyZmFjZSB0byB0cmlnZ2VyIGdlbmVyYXRpb24gb2YgdHlwZS1jaGVja2luZyBjb2RlIGZvciBhIHByb2dyYW0gZ2l2ZW4gYSBuZXdcbiAqIGBUeXBlQ2hlY2tDb250ZXh0YC5cbiAqL1xuZXhwb3J0IGludGVyZmFjZSBQcm9ncmFtVHlwZUNoZWNrQWRhcHRlciB7XG4gIHR5cGVDaGVjayhzZjogdHMuU291cmNlRmlsZSwgY3R4OiBUeXBlQ2hlY2tDb250ZXh0KTogdm9pZDtcbn1cblxuLyoqXG4gKiBQcmltYXJ5IHRlbXBsYXRlIHR5cGUtY2hlY2tpbmcgZW5naW5lLCB3aGljaCBwZXJmb3JtcyB0eXBlLWNoZWNraW5nIHVzaW5nIGFcbiAqIGBUeXBlQ2hlY2tpbmdQcm9ncmFtU3RyYXRlZ3lgIGZvciB0eXBlLWNoZWNraW5nIHByb2dyYW0gbWFpbnRlbmFuY2UsIGFuZCB0aGVcbiAqIGBQcm9ncmFtVHlwZUNoZWNrQWRhcHRlcmAgZm9yIGdlbmVyYXRpb24gb2YgdGVtcGxhdGUgdHlwZS1jaGVja2luZyBjb2RlLlxuICovXG5leHBvcnQgY2xhc3MgVGVtcGxhdGVUeXBlQ2hlY2tlciB7XG4gIHByaXZhdGUgZmlsZXMgPSBuZXcgTWFwPEFic29sdXRlRnNQYXRoLCBGaWxlVHlwZUNoZWNraW5nRGF0YT4oKTtcblxuICBjb25zdHJ1Y3RvcihcbiAgICAgIHByaXZhdGUgb3JpZ2luYWxQcm9ncmFtOiB0cy5Qcm9ncmFtLFxuICAgICAgcHJpdmF0ZSB0eXBlQ2hlY2tpbmdTdHJhdGVneTogVHlwZUNoZWNraW5nUHJvZ3JhbVN0cmF0ZWd5LFxuICAgICAgcHJpdmF0ZSB0eXBlQ2hlY2tBZGFwdGVyOiBQcm9ncmFtVHlwZUNoZWNrQWRhcHRlciwgcHJpdmF0ZSBjb25maWc6IFR5cGVDaGVja2luZ0NvbmZpZyxcbiAgICAgIHByaXZhdGUgcmVmRW1pdHRlcjogUmVmZXJlbmNlRW1pdHRlciwgcHJpdmF0ZSByZWZsZWN0b3I6IFJlZmxlY3Rpb25Ib3N0LFxuICAgICAgcHJpdmF0ZSBjb21waWxlckhvc3Q6IFBpY2s8dHMuQ29tcGlsZXJIb3N0LCAnZ2V0Q2Fub25pY2FsRmlsZU5hbWUnPixcbiAgICAgIHByaXZhdGUgcHJpb3JCdWlsZDogSW5jcmVtZW50YWxCdWlsZDx1bmtub3duLCBGaWxlVHlwZUNoZWNraW5nRGF0YT4pIHt9XG5cbiAgLyoqXG4gICAqIFJlc2V0IHRoZSBpbnRlcm5hbCB0eXBlLWNoZWNraW5nIHByb2dyYW0gYnkgZ2VuZXJhdGluZyB0eXBlLWNoZWNraW5nIGNvZGUgZnJvbSB0aGUgdXNlcidzXG4gICAqIHByb2dyYW0uXG4gICAqL1xuICByZWZyZXNoKCk6IFR5cGVDaGVja1JlcXVlc3Qge1xuICAgIHRoaXMuZmlsZXMuY2xlYXIoKTtcblxuICAgIGNvbnN0IGN0eCA9XG4gICAgICAgIG5ldyBUeXBlQ2hlY2tDb250ZXh0KHRoaXMuY29uZmlnLCB0aGlzLmNvbXBpbGVySG9zdCwgdGhpcy5yZWZFbWl0dGVyLCB0aGlzLnJlZmxlY3Rvcik7XG5cbiAgICAvLyBUeXBlY2hlY2sgYWxsIHRoZSBmaWxlcy5cbiAgICBmb3IgKGNvbnN0IHNmIG9mIHRoaXMub3JpZ2luYWxQcm9ncmFtLmdldFNvdXJjZUZpbGVzKCkpIHtcbiAgICAgIGlmIChzZi5pc0RlY2xhcmF0aW9uRmlsZSB8fCBpc1NoaW0oc2YpKSB7XG4gICAgICAgIGNvbnRpbnVlO1xuICAgICAgfVxuXG4gICAgICBjb25zdCBwcmV2aW91c1Jlc3VsdHMgPSB0aGlzLnByaW9yQnVpbGQucHJpb3JUeXBlQ2hlY2tpbmdSZXN1bHRzRm9yKHNmKTtcbiAgICAgIGlmIChwcmV2aW91c1Jlc3VsdHMgPT09IG51bGwpIHtcbiAgICAgICAgLy8gUHJldmlvdXMgcmVzdWx0cyB3ZXJlIG5vdCBhdmFpbGFibGUsIHNvIGdlbmVyYXRlIG5ldyB0eXBlLWNoZWNraW5nIGNvZGUgZm9yIHRoaXMgZmlsZS5cbiAgICAgICAgdGhpcy50eXBlQ2hlY2tBZGFwdGVyLnR5cGVDaGVjayhzZiwgY3R4KTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIC8vIFByZXZpb3VzIHJlc3VsdHMgd2VyZSBhdmFpbGFibGUsIGFuZCBjYW4gYmUgYWRvcHRlZCBpbnRvIHRoZSBjdXJyZW50IGJ1aWxkLlxuICAgICAgICBjdHguYWRvcHRQcmlvclJlc3VsdHMoc2YsIHByZXZpb3VzUmVzdWx0cyk7XG4gICAgICB9XG4gICAgfVxuXG4gICAgY29uc3QgcmVzdWx0cyA9IGN0eC5maW5hbGl6ZSgpO1xuICAgIHRoaXMudHlwZUNoZWNraW5nU3RyYXRlZ3kudXBkYXRlRmlsZXMocmVzdWx0cy51cGRhdGVzLCBVcGRhdGVNb2RlLkNvbXBsZXRlKTtcbiAgICBmb3IgKGNvbnN0IFtmaWxlLCBmaWxlRGF0YV0gb2YgcmVzdWx0cy5wZXJGaWxlRGF0YSkge1xuICAgICAgdGhpcy5maWxlcy5zZXQoZmlsZSwgZmlsZURhdGEpO1xuICAgIH1cblxuICAgIHJldHVybiByZXN1bHRzO1xuICB9XG5cbiAgLyoqXG4gICAqIFJldHJpZXZlIHR5cGUtY2hlY2tpbmcgZGlhZ25vc3RpY3MgZnJvbSB0aGUgZ2l2ZW4gYHRzLlNvdXJjZUZpbGVgIHVzaW5nIHRoZSBtb3N0IHJlY2VudFxuICAgKiB0eXBlLWNoZWNraW5nIHByb2dyYW0uXG4gICAqL1xuICBnZXREaWFnbm9zdGljc0ZvckZpbGUoc2Y6IHRzLlNvdXJjZUZpbGUpOiB0cy5EaWFnbm9zdGljW10ge1xuICAgIGNvbnN0IHBhdGggPSBhYnNvbHV0ZUZyb21Tb3VyY2VGaWxlKHNmKTtcbiAgICBpZiAoIXRoaXMuZmlsZXMuaGFzKHBhdGgpKSB7XG4gICAgICByZXR1cm4gW107XG4gICAgfVxuICAgIGNvbnN0IHJlY29yZCA9IHRoaXMuZmlsZXMuZ2V0KHBhdGgpITtcblxuICAgIGNvbnN0IHR5cGVDaGVja1Byb2dyYW0gPSB0aGlzLnR5cGVDaGVja2luZ1N0cmF0ZWd5LmdldFByb2dyYW0oKTtcbiAgICBjb25zdCB0eXBlQ2hlY2tTZiA9IGdldFNvdXJjZUZpbGVPckVycm9yKHR5cGVDaGVja1Byb2dyYW0sIHJlY29yZC50eXBlQ2hlY2tGaWxlKTtcbiAgICBjb25zdCByYXdEaWFnbm9zdGljcyA9IFtdO1xuICAgIHJhd0RpYWdub3N0aWNzLnB1c2goLi4udHlwZUNoZWNrUHJvZ3JhbS5nZXRTZW1hbnRpY0RpYWdub3N0aWNzKHR5cGVDaGVja1NmKSk7XG4gICAgaWYgKHJlY29yZC5oYXNJbmxpbmVzKSB7XG4gICAgICBjb25zdCBpbmxpbmVTZiA9IGdldFNvdXJjZUZpbGVPckVycm9yKHR5cGVDaGVja1Byb2dyYW0sIHBhdGgpO1xuICAgICAgcmF3RGlhZ25vc3RpY3MucHVzaCguLi50eXBlQ2hlY2tQcm9ncmFtLmdldFNlbWFudGljRGlhZ25vc3RpY3MoaW5saW5lU2YpKTtcbiAgICB9XG5cbiAgICByZXR1cm4gcmF3RGlhZ25vc3RpY3NcbiAgICAgICAgLm1hcChkaWFnID0+IHtcbiAgICAgICAgICBpZiAoIXNob3VsZFJlcG9ydERpYWdub3N0aWMoZGlhZykpIHtcbiAgICAgICAgICAgIHJldHVybiBudWxsO1xuICAgICAgICAgIH1cbiAgICAgICAgICByZXR1cm4gdHJhbnNsYXRlRGlhZ25vc3RpYyhkaWFnLCByZWNvcmQuc291cmNlUmVzb2x2ZXIpO1xuICAgICAgICB9KVxuICAgICAgICAuZmlsdGVyKChkaWFnOiB0cy5EaWFnbm9zdGljfG51bGwpOiBkaWFnIGlzIHRzLkRpYWdub3N0aWMgPT4gZGlhZyAhPT0gbnVsbClcbiAgICAgICAgLmNvbmNhdChyZWNvcmQuZ2VuZXNpc0RpYWdub3N0aWNzKTtcbiAgfVxufVxuIl19