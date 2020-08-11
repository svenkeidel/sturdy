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
        define("@angular/compiler-cli/src/ngtsc/typecheck", ["require", "exports", "tslib", "@angular/compiler-cli/src/ngtsc/typecheck/src/api", "@angular/compiler-cli/src/ngtsc/typecheck/src/augmented_program", "@angular/compiler-cli/src/ngtsc/typecheck/src/checker", "@angular/compiler-cli/src/ngtsc/typecheck/src/context", "@angular/compiler-cli/src/ngtsc/typecheck/src/diagnostics", "@angular/compiler-cli/src/ngtsc/typecheck/src/shim", "@angular/compiler-cli/src/ngtsc/typecheck/src/host", "@angular/compiler-cli/src/ngtsc/typecheck/src/type_check_file"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var tslib_1 = require("tslib");
    tslib_1.__exportStar(require("@angular/compiler-cli/src/ngtsc/typecheck/src/api"), exports);
    var augmented_program_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/augmented_program");
    Object.defineProperty(exports, "ReusedProgramStrategy", { enumerable: true, get: function () { return augmented_program_1.ReusedProgramStrategy; } });
    var checker_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/checker");
    Object.defineProperty(exports, "TemplateTypeChecker", { enumerable: true, get: function () { return checker_1.TemplateTypeChecker; } });
    var context_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/context");
    Object.defineProperty(exports, "TypeCheckContext", { enumerable: true, get: function () { return context_1.TypeCheckContext; } });
    var diagnostics_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/diagnostics");
    Object.defineProperty(exports, "isTemplateDiagnostic", { enumerable: true, get: function () { return diagnostics_1.isTemplateDiagnostic; } });
    var shim_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/shim");
    Object.defineProperty(exports, "TypeCheckShimGenerator", { enumerable: true, get: function () { return shim_1.TypeCheckShimGenerator; } });
    var host_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/host");
    Object.defineProperty(exports, "TypeCheckProgramHost", { enumerable: true, get: function () { return host_1.TypeCheckProgramHost; } });
    var type_check_file_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/type_check_file");
    Object.defineProperty(exports, "typeCheckFilePath", { enumerable: true, get: function () { return type_check_file_1.typeCheckFilePath; } });
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5kZXguanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9jb21waWxlci1jbGkvc3JjL25ndHNjL3R5cGVjaGVjay9pbmRleC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7SUFFSCw0RkFBMEI7SUFDMUIscUdBQThEO0lBQXRELDBIQUFBLHFCQUFxQixPQUFBO0lBQzdCLGlGQUEyRTtJQUFuRSw4R0FBQSxtQkFBbUIsT0FBQTtJQUMzQixpRkFBK0M7SUFBdkMsMkdBQUEsZ0JBQWdCLE9BQUE7SUFDeEIseUZBQTJFO0lBQS9DLG1IQUFBLG9CQUFvQixPQUFBO0lBQ2hELDJFQUFrRDtJQUExQyw4R0FBQSxzQkFBc0IsT0FBQTtJQUM5QiwyRUFBZ0Q7SUFBeEMsNEdBQUEsb0JBQW9CLE9BQUE7SUFDNUIsaUdBQXdEO0lBQWhELG9IQUFBLGlCQUFpQixPQUFBIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmV4cG9ydCAqIGZyb20gJy4vc3JjL2FwaSc7XG5leHBvcnQge1JldXNlZFByb2dyYW1TdHJhdGVneX0gZnJvbSAnLi9zcmMvYXVnbWVudGVkX3Byb2dyYW0nO1xuZXhwb3J0IHtUZW1wbGF0ZVR5cGVDaGVja2VyLCBQcm9ncmFtVHlwZUNoZWNrQWRhcHRlcn0gZnJvbSAnLi9zcmMvY2hlY2tlcic7XG5leHBvcnQge1R5cGVDaGVja0NvbnRleHR9IGZyb20gJy4vc3JjL2NvbnRleHQnO1xuZXhwb3J0IHtUZW1wbGF0ZURpYWdub3N0aWMsIGlzVGVtcGxhdGVEaWFnbm9zdGljfSBmcm9tICcuL3NyYy9kaWFnbm9zdGljcyc7XG5leHBvcnQge1R5cGVDaGVja1NoaW1HZW5lcmF0b3J9IGZyb20gJy4vc3JjL3NoaW0nO1xuZXhwb3J0IHtUeXBlQ2hlY2tQcm9ncmFtSG9zdH0gZnJvbSAnLi9zcmMvaG9zdCc7XG5leHBvcnQge3R5cGVDaGVja0ZpbGVQYXRofSBmcm9tICcuL3NyYy90eXBlX2NoZWNrX2ZpbGUnO1xuIl19