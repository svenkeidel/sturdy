(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define("@angular/compiler-cli/src/ngtsc/util/src/path", ["require", "exports", "@angular/compiler-cli/src/ngtsc/file_system"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.normalizeSeparators = exports.relativePathBetween = void 0;
    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    var file_system_1 = require("@angular/compiler-cli/src/ngtsc/file_system");
    var TS_DTS_JS_EXTENSION = /(?:\.d)?\.ts$|\.js$/;
    function relativePathBetween(from, to) {
        var relativePath = file_system_1.relative(file_system_1.dirname(file_system_1.resolve(from)), file_system_1.resolve(to)).replace(TS_DTS_JS_EXTENSION, '');
        if (relativePath === '') {
            return null;
        }
        // path.relative() does not include the leading './'.
        if (!relativePath.startsWith('.')) {
            relativePath = "./" + relativePath;
        }
        return relativePath;
    }
    exports.relativePathBetween = relativePathBetween;
    function normalizeSeparators(path) {
        // TODO: normalize path only for OS that need it.
        return path.replace(/\\/g, '/');
    }
    exports.normalizeSeparators = normalizeSeparators;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGF0aC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2NvbXBpbGVyLWNsaS9zcmMvbmd0c2MvdXRpbC9zcmMvcGF0aC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7Ozs7Ozs7Ozs7SUFBQTs7Ozs7O09BTUc7SUFDSCwyRUFBNkQ7SUFFN0QsSUFBTSxtQkFBbUIsR0FBRyxxQkFBcUIsQ0FBQztJQUVsRCxTQUFnQixtQkFBbUIsQ0FBQyxJQUFZLEVBQUUsRUFBVTtRQUMxRCxJQUFJLFlBQVksR0FBRyxzQkFBUSxDQUFDLHFCQUFPLENBQUMscUJBQU8sQ0FBQyxJQUFJLENBQUMsQ0FBQyxFQUFFLHFCQUFPLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQyxPQUFPLENBQUMsbUJBQW1CLEVBQUUsRUFBRSxDQUFDLENBQUM7UUFFbEcsSUFBSSxZQUFZLEtBQUssRUFBRSxFQUFFO1lBQ3ZCLE9BQU8sSUFBSSxDQUFDO1NBQ2I7UUFFRCxxREFBcUQ7UUFDckQsSUFBSSxDQUFDLFlBQVksQ0FBQyxVQUFVLENBQUMsR0FBRyxDQUFDLEVBQUU7WUFDakMsWUFBWSxHQUFHLE9BQUssWUFBYyxDQUFDO1NBQ3BDO1FBRUQsT0FBTyxZQUFZLENBQUM7SUFDdEIsQ0FBQztJQWJELGtEQWFDO0lBRUQsU0FBZ0IsbUJBQW1CLENBQUMsSUFBWTtRQUM5QyxpREFBaUQ7UUFDakQsT0FBTyxJQUFJLENBQUMsT0FBTyxDQUFDLEtBQUssRUFBRSxHQUFHLENBQUMsQ0FBQztJQUNsQyxDQUFDO0lBSEQsa0RBR0MiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cbmltcG9ydCB7ZGlybmFtZSwgcmVsYXRpdmUsIHJlc29sdmV9IGZyb20gJy4uLy4uL2ZpbGVfc3lzdGVtJztcblxuY29uc3QgVFNfRFRTX0pTX0VYVEVOU0lPTiA9IC8oPzpcXC5kKT9cXC50cyR8XFwuanMkLztcblxuZXhwb3J0IGZ1bmN0aW9uIHJlbGF0aXZlUGF0aEJldHdlZW4oZnJvbTogc3RyaW5nLCB0bzogc3RyaW5nKTogc3RyaW5nfG51bGwge1xuICBsZXQgcmVsYXRpdmVQYXRoID0gcmVsYXRpdmUoZGlybmFtZShyZXNvbHZlKGZyb20pKSwgcmVzb2x2ZSh0bykpLnJlcGxhY2UoVFNfRFRTX0pTX0VYVEVOU0lPTiwgJycpO1xuXG4gIGlmIChyZWxhdGl2ZVBhdGggPT09ICcnKSB7XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuICAvLyBwYXRoLnJlbGF0aXZlKCkgZG9lcyBub3QgaW5jbHVkZSB0aGUgbGVhZGluZyAnLi8nLlxuICBpZiAoIXJlbGF0aXZlUGF0aC5zdGFydHNXaXRoKCcuJykpIHtcbiAgICByZWxhdGl2ZVBhdGggPSBgLi8ke3JlbGF0aXZlUGF0aH1gO1xuICB9XG5cbiAgcmV0dXJuIHJlbGF0aXZlUGF0aDtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIG5vcm1hbGl6ZVNlcGFyYXRvcnMocGF0aDogc3RyaW5nKTogc3RyaW5nIHtcbiAgLy8gVE9ETzogbm9ybWFsaXplIHBhdGggb25seSBmb3IgT1MgdGhhdCBuZWVkIGl0LlxuICByZXR1cm4gcGF0aC5yZXBsYWNlKC9cXFxcL2csICcvJyk7XG59XG4iXX0=