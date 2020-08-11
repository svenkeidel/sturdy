(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define("@angular/compiler-cli/src/ngtsc/file_system", ["require", "exports", "@angular/compiler-cli/src/ngtsc/file_system/src/compiler_host", "@angular/compiler-cli/src/ngtsc/file_system/src/helpers", "@angular/compiler-cli/src/ngtsc/file_system/src/logical", "@angular/compiler-cli/src/ngtsc/file_system/src/node_js_file_system", "@angular/compiler-cli/src/ngtsc/file_system/src/util"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    var compiler_host_1 = require("@angular/compiler-cli/src/ngtsc/file_system/src/compiler_host");
    Object.defineProperty(exports, "NgtscCompilerHost", { enumerable: true, get: function () { return compiler_host_1.NgtscCompilerHost; } });
    var helpers_1 = require("@angular/compiler-cli/src/ngtsc/file_system/src/helpers");
    Object.defineProperty(exports, "absoluteFrom", { enumerable: true, get: function () { return helpers_1.absoluteFrom; } });
    Object.defineProperty(exports, "absoluteFromSourceFile", { enumerable: true, get: function () { return helpers_1.absoluteFromSourceFile; } });
    Object.defineProperty(exports, "basename", { enumerable: true, get: function () { return helpers_1.basename; } });
    Object.defineProperty(exports, "dirname", { enumerable: true, get: function () { return helpers_1.dirname; } });
    Object.defineProperty(exports, "getFileSystem", { enumerable: true, get: function () { return helpers_1.getFileSystem; } });
    Object.defineProperty(exports, "isRoot", { enumerable: true, get: function () { return helpers_1.isRoot; } });
    Object.defineProperty(exports, "isRooted", { enumerable: true, get: function () { return helpers_1.isRooted; } });
    Object.defineProperty(exports, "join", { enumerable: true, get: function () { return helpers_1.join; } });
    Object.defineProperty(exports, "relative", { enumerable: true, get: function () { return helpers_1.relative; } });
    Object.defineProperty(exports, "relativeFrom", { enumerable: true, get: function () { return helpers_1.relativeFrom; } });
    Object.defineProperty(exports, "resolve", { enumerable: true, get: function () { return helpers_1.resolve; } });
    Object.defineProperty(exports, "setFileSystem", { enumerable: true, get: function () { return helpers_1.setFileSystem; } });
    var logical_1 = require("@angular/compiler-cli/src/ngtsc/file_system/src/logical");
    Object.defineProperty(exports, "LogicalFileSystem", { enumerable: true, get: function () { return logical_1.LogicalFileSystem; } });
    Object.defineProperty(exports, "LogicalProjectPath", { enumerable: true, get: function () { return logical_1.LogicalProjectPath; } });
    var node_js_file_system_1 = require("@angular/compiler-cli/src/ngtsc/file_system/src/node_js_file_system");
    Object.defineProperty(exports, "NodeJSFileSystem", { enumerable: true, get: function () { return node_js_file_system_1.NodeJSFileSystem; } });
    var util_1 = require("@angular/compiler-cli/src/ngtsc/file_system/src/util");
    Object.defineProperty(exports, "getSourceFileOrError", { enumerable: true, get: function () { return util_1.getSourceFileOrError; } });
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5kZXguanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9jb21waWxlci1jbGkvc3JjL25ndHNjL2ZpbGVfc3lzdGVtL2luZGV4LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7Ozs7Ozs7Ozs7O0lBQUE7Ozs7OztPQU1HO0lBQ0gsK0ZBQXNEO0lBQTlDLGtIQUFBLGlCQUFpQixPQUFBO0lBQ3pCLG1GQUE2SztJQUFySyx1R0FBQSxZQUFZLE9BQUE7SUFBRSxpSEFBQSxzQkFBc0IsT0FBQTtJQUFFLG1HQUFBLFFBQVEsT0FBQTtJQUFFLGtHQUFBLE9BQU8sT0FBQTtJQUFFLHdHQUFBLGFBQWEsT0FBQTtJQUFFLGlHQUFBLE1BQU0sT0FBQTtJQUFFLG1HQUFBLFFBQVEsT0FBQTtJQUFFLCtGQUFBLElBQUksT0FBQTtJQUFFLG1HQUFBLFFBQVEsT0FBQTtJQUFFLHVHQUFBLFlBQVksT0FBQTtJQUFFLGtHQUFBLE9BQU8sT0FBQTtJQUFFLHdHQUFBLGFBQWEsT0FBQTtJQUN0SixtRkFBb0U7SUFBNUQsNEdBQUEsaUJBQWlCLE9BQUE7SUFBRSw2R0FBQSxrQkFBa0IsT0FBQTtJQUM3QywyR0FBMkQ7SUFBbkQsdUhBQUEsZ0JBQWdCLE9BQUE7SUFFeEIsNkVBQWdEO0lBQXhDLDRHQUFBLG9CQUFvQixPQUFBIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5leHBvcnQge05ndHNjQ29tcGlsZXJIb3N0fSBmcm9tICcuL3NyYy9jb21waWxlcl9ob3N0JztcbmV4cG9ydCB7YWJzb2x1dGVGcm9tLCBhYnNvbHV0ZUZyb21Tb3VyY2VGaWxlLCBiYXNlbmFtZSwgZGlybmFtZSwgZ2V0RmlsZVN5c3RlbSwgaXNSb290LCBpc1Jvb3RlZCwgam9pbiwgcmVsYXRpdmUsIHJlbGF0aXZlRnJvbSwgcmVzb2x2ZSwgc2V0RmlsZVN5c3RlbX0gZnJvbSAnLi9zcmMvaGVscGVycyc7XG5leHBvcnQge0xvZ2ljYWxGaWxlU3lzdGVtLCBMb2dpY2FsUHJvamVjdFBhdGh9IGZyb20gJy4vc3JjL2xvZ2ljYWwnO1xuZXhwb3J0IHtOb2RlSlNGaWxlU3lzdGVtfSBmcm9tICcuL3NyYy9ub2RlX2pzX2ZpbGVfc3lzdGVtJztcbmV4cG9ydCB7QWJzb2x1dGVGc1BhdGgsIEZpbGVTdGF0cywgRmlsZVN5c3RlbSwgUGF0aFNlZ21lbnQsIFBhdGhTdHJpbmd9IGZyb20gJy4vc3JjL3R5cGVzJztcbmV4cG9ydCB7Z2V0U291cmNlRmlsZU9yRXJyb3J9IGZyb20gJy4vc3JjL3V0aWwnO1xuIl19