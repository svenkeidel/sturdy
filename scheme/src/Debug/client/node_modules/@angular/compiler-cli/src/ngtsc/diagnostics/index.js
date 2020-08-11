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
        define("@angular/compiler-cli/src/ngtsc/diagnostics", ["require", "exports", "@angular/compiler-cli/src/ngtsc/diagnostics/src/error", "@angular/compiler-cli/src/ngtsc/diagnostics/src/error_code", "@angular/compiler-cli/src/ngtsc/diagnostics/src/util"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var error_1 = require("@angular/compiler-cli/src/ngtsc/diagnostics/src/error");
    Object.defineProperty(exports, "FatalDiagnosticError", { enumerable: true, get: function () { return error_1.FatalDiagnosticError; } });
    Object.defineProperty(exports, "isFatalDiagnosticError", { enumerable: true, get: function () { return error_1.isFatalDiagnosticError; } });
    Object.defineProperty(exports, "makeDiagnostic", { enumerable: true, get: function () { return error_1.makeDiagnostic; } });
    var error_code_1 = require("@angular/compiler-cli/src/ngtsc/diagnostics/src/error_code");
    Object.defineProperty(exports, "ErrorCode", { enumerable: true, get: function () { return error_code_1.ErrorCode; } });
    Object.defineProperty(exports, "ngErrorCode", { enumerable: true, get: function () { return error_code_1.ngErrorCode; } });
    var util_1 = require("@angular/compiler-cli/src/ngtsc/diagnostics/src/util");
    Object.defineProperty(exports, "replaceTsWithNgInErrors", { enumerable: true, get: function () { return util_1.replaceTsWithNgInErrors; } });
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5kZXguanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9jb21waWxlci1jbGkvc3JjL25ndHNjL2RpYWdub3N0aWNzL2luZGV4LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7Ozs7Ozs7Ozs7O0lBRUgsK0VBQXlGO0lBQWpGLDZHQUFBLG9CQUFvQixPQUFBO0lBQUUsK0dBQUEsc0JBQXNCLE9BQUE7SUFBRSx1R0FBQSxjQUFjLE9BQUE7SUFDcEUseUZBQXdEO0lBQWhELHVHQUFBLFNBQVMsT0FBQTtJQUFFLHlHQUFBLFdBQVcsT0FBQTtJQUM5Qiw2RUFBbUQ7SUFBM0MsK0dBQUEsdUJBQXVCLE9BQUEiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuZXhwb3J0IHtGYXRhbERpYWdub3N0aWNFcnJvciwgaXNGYXRhbERpYWdub3N0aWNFcnJvciwgbWFrZURpYWdub3N0aWN9IGZyb20gJy4vc3JjL2Vycm9yJztcbmV4cG9ydCB7RXJyb3JDb2RlLCBuZ0Vycm9yQ29kZX0gZnJvbSAnLi9zcmMvZXJyb3JfY29kZSc7XG5leHBvcnQge3JlcGxhY2VUc1dpdGhOZ0luRXJyb3JzfSBmcm9tICcuL3NyYy91dGlsJztcbiJdfQ==