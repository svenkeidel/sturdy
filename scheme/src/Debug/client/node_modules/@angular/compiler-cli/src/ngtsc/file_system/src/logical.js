(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define("@angular/compiler-cli/src/ngtsc/file_system/src/logical", ["require", "exports", "@angular/compiler-cli/src/ngtsc/file_system/src/helpers", "@angular/compiler-cli/src/ngtsc/file_system/src/util"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.LogicalFileSystem = exports.LogicalProjectPath = void 0;
    var helpers_1 = require("@angular/compiler-cli/src/ngtsc/file_system/src/helpers");
    var util_1 = require("@angular/compiler-cli/src/ngtsc/file_system/src/util");
    exports.LogicalProjectPath = {
        /**
         * Get the relative path between two `LogicalProjectPath`s.
         *
         * This will return a `PathSegment` which would be a valid module specifier to use in `from` when
         * importing from `to`.
         */
        relativePathBetween: function (from, to) {
            var relativePath = helpers_1.relative(helpers_1.dirname(helpers_1.resolve(from)), helpers_1.resolve(to));
            if (!relativePath.startsWith('../')) {
                relativePath = ('./' + relativePath);
            }
            return relativePath;
        },
    };
    /**
     * A utility class which can translate absolute paths to source files into logical paths in
     * TypeScript's logical file system, based on the root directories of the project.
     */
    var LogicalFileSystem = /** @class */ (function () {
        function LogicalFileSystem(rootDirs, compilerHost) {
            var _this = this;
            this.compilerHost = compilerHost;
            /**
             * A cache of file paths to project paths, because computation of these paths is slightly
             * expensive.
             */
            this.cache = new Map();
            // Make a copy and sort it by length in reverse order (longest first). This speeds up lookups,
            // since there's no need to keep going through the array once a match is found.
            this.rootDirs = rootDirs.concat([]).sort(function (a, b) { return b.length - a.length; });
            this.canonicalRootDirs =
                this.rootDirs.map(function (dir) { return _this.compilerHost.getCanonicalFileName(dir); });
        }
        /**
         * Get the logical path in the project of a `ts.SourceFile`.
         *
         * This method is provided as a convenient alternative to calling
         * `logicalPathOfFile(absoluteFromSourceFile(sf))`.
         */
        LogicalFileSystem.prototype.logicalPathOfSf = function (sf) {
            return this.logicalPathOfFile(helpers_1.absoluteFrom(sf.fileName));
        };
        /**
         * Get the logical path in the project of a source file.
         *
         * @returns A `LogicalProjectPath` to the source file, or `null` if the source file is not in any
         * of the TS project's root directories.
         */
        LogicalFileSystem.prototype.logicalPathOfFile = function (physicalFile) {
            var canonicalFilePath = this.compilerHost.getCanonicalFileName(physicalFile);
            if (!this.cache.has(canonicalFilePath)) {
                var logicalFile = null;
                for (var i = 0; i < this.rootDirs.length; i++) {
                    var rootDir = this.rootDirs[i];
                    var canonicalRootDir = this.canonicalRootDirs[i];
                    if (isWithinBasePath(canonicalRootDir, canonicalFilePath)) {
                        // Note that we match against canonical paths but then create the logical path from
                        // original paths.
                        logicalFile = this.createLogicalProjectPath(physicalFile, rootDir);
                        // The logical project does not include any special "node_modules" nested directories.
                        if (logicalFile.indexOf('/node_modules/') !== -1) {
                            logicalFile = null;
                        }
                        else {
                            break;
                        }
                    }
                }
                this.cache.set(canonicalFilePath, logicalFile);
            }
            return this.cache.get(canonicalFilePath);
        };
        LogicalFileSystem.prototype.createLogicalProjectPath = function (file, rootDir) {
            var logicalPath = util_1.stripExtension(file.substr(rootDir.length));
            return (logicalPath.startsWith('/') ? logicalPath : '/' + logicalPath);
        };
        return LogicalFileSystem;
    }());
    exports.LogicalFileSystem = LogicalFileSystem;
    /**
     * Is the `path` a descendant of the `base`?
     * E.g. `foo/bar/zee` is within `foo/bar` but not within `foo/car`.
     */
    function isWithinBasePath(base, path) {
        return !helpers_1.relative(base, path).startsWith('..');
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibG9naWNhbC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2NvbXBpbGVyLWNsaS9zcmMvbmd0c2MvZmlsZV9zeXN0ZW0vc3JjL2xvZ2ljYWwudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7Ozs7Ozs7Ozs7O0lBU0EsbUZBQW1FO0lBRW5FLDZFQUFzQztJQVl6QixRQUFBLGtCQUFrQixHQUFHO1FBQ2hDOzs7OztXQUtHO1FBQ0gsbUJBQW1CLEVBQUUsVUFBUyxJQUF3QixFQUFFLEVBQXNCO1lBQzVFLElBQUksWUFBWSxHQUFHLGtCQUFRLENBQUMsaUJBQU8sQ0FBQyxpQkFBTyxDQUFDLElBQUksQ0FBQyxDQUFDLEVBQUUsaUJBQU8sQ0FBQyxFQUFFLENBQUMsQ0FBQyxDQUFDO1lBQ2pFLElBQUksQ0FBQyxZQUFZLENBQUMsVUFBVSxDQUFDLEtBQUssQ0FBQyxFQUFFO2dCQUNuQyxZQUFZLEdBQUcsQ0FBQyxJQUFJLEdBQUcsWUFBWSxDQUFnQixDQUFDO2FBQ3JEO1lBQ0QsT0FBTyxZQUEyQixDQUFDO1FBQ3JDLENBQUM7S0FDRixDQUFDO0lBRUY7OztPQUdHO0lBQ0g7UUFrQkUsMkJBQ0ksUUFBMEIsRUFDbEIsWUFBMkQ7WUFGdkUsaUJBUUM7WUFOVyxpQkFBWSxHQUFaLFlBQVksQ0FBK0M7WUFSdkU7OztlQUdHO1lBQ0ssVUFBSyxHQUFpRCxJQUFJLEdBQUcsRUFBRSxDQUFDO1lBS3RFLDhGQUE4RjtZQUM5RiwrRUFBK0U7WUFDL0UsSUFBSSxDQUFDLFFBQVEsR0FBRyxRQUFRLENBQUMsTUFBTSxDQUFDLEVBQUUsQ0FBQyxDQUFDLElBQUksQ0FBQyxVQUFDLENBQUMsRUFBRSxDQUFDLElBQUssT0FBQSxDQUFDLENBQUMsTUFBTSxHQUFHLENBQUMsQ0FBQyxNQUFNLEVBQW5CLENBQW1CLENBQUMsQ0FBQztZQUN4RSxJQUFJLENBQUMsaUJBQWlCO2dCQUNsQixJQUFJLENBQUMsUUFBUSxDQUFDLEdBQUcsQ0FBQyxVQUFBLEdBQUcsSUFBSSxPQUFBLEtBQUksQ0FBQyxZQUFZLENBQUMsb0JBQW9CLENBQUMsR0FBRyxDQUFtQixFQUE3RCxDQUE2RCxDQUFDLENBQUM7UUFDOUYsQ0FBQztRQUVEOzs7OztXQUtHO1FBQ0gsMkNBQWUsR0FBZixVQUFnQixFQUFpQjtZQUMvQixPQUFPLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxzQkFBWSxDQUFDLEVBQUUsQ0FBQyxRQUFRLENBQUMsQ0FBQyxDQUFDO1FBQzNELENBQUM7UUFFRDs7Ozs7V0FLRztRQUNILDZDQUFpQixHQUFqQixVQUFrQixZQUE0QjtZQUM1QyxJQUFNLGlCQUFpQixHQUNuQixJQUFJLENBQUMsWUFBWSxDQUFDLG9CQUFvQixDQUFDLFlBQVksQ0FBbUIsQ0FBQztZQUMzRSxJQUFJLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxHQUFHLENBQUMsaUJBQWlCLENBQUMsRUFBRTtnQkFDdEMsSUFBSSxXQUFXLEdBQTRCLElBQUksQ0FBQztnQkFDaEQsS0FBSyxJQUFJLENBQUMsR0FBRyxDQUFDLEVBQUUsQ0FBQyxHQUFHLElBQUksQ0FBQyxRQUFRLENBQUMsTUFBTSxFQUFFLENBQUMsRUFBRSxFQUFFO29CQUM3QyxJQUFNLE9BQU8sR0FBRyxJQUFJLENBQUMsUUFBUSxDQUFDLENBQUMsQ0FBQyxDQUFDO29CQUNqQyxJQUFNLGdCQUFnQixHQUFHLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxDQUFDLENBQUMsQ0FBQztvQkFDbkQsSUFBSSxnQkFBZ0IsQ0FBQyxnQkFBZ0IsRUFBRSxpQkFBaUIsQ0FBQyxFQUFFO3dCQUN6RCxtRkFBbUY7d0JBQ25GLGtCQUFrQjt3QkFDbEIsV0FBVyxHQUFHLElBQUksQ0FBQyx3QkFBd0IsQ0FBQyxZQUFZLEVBQUUsT0FBTyxDQUFDLENBQUM7d0JBQ25FLHNGQUFzRjt3QkFDdEYsSUFBSSxXQUFXLENBQUMsT0FBTyxDQUFDLGdCQUFnQixDQUFDLEtBQUssQ0FBQyxDQUFDLEVBQUU7NEJBQ2hELFdBQVcsR0FBRyxJQUFJLENBQUM7eUJBQ3BCOzZCQUFNOzRCQUNMLE1BQU07eUJBQ1A7cUJBQ0Y7aUJBQ0Y7Z0JBQ0QsSUFBSSxDQUFDLEtBQUssQ0FBQyxHQUFHLENBQUMsaUJBQWlCLEVBQUUsV0FBVyxDQUFDLENBQUM7YUFDaEQ7WUFDRCxPQUFPLElBQUksQ0FBQyxLQUFLLENBQUMsR0FBRyxDQUFDLGlCQUFpQixDQUFFLENBQUM7UUFDNUMsQ0FBQztRQUVPLG9EQUF3QixHQUFoQyxVQUFpQyxJQUFvQixFQUFFLE9BQXVCO1lBRTVFLElBQU0sV0FBVyxHQUFHLHFCQUFjLENBQUMsSUFBSSxDQUFDLE1BQU0sQ0FBQyxPQUFPLENBQUMsTUFBTSxDQUFDLENBQUMsQ0FBQztZQUNoRSxPQUFPLENBQUMsV0FBVyxDQUFDLFVBQVUsQ0FBQyxHQUFHLENBQUMsQ0FBQyxDQUFDLENBQUMsV0FBVyxDQUFDLENBQUMsQ0FBQyxHQUFHLEdBQUcsV0FBVyxDQUF1QixDQUFDO1FBQy9GLENBQUM7UUFDSCx3QkFBQztJQUFELENBQUMsQUExRUQsSUEwRUM7SUExRVksOENBQWlCO0lBNEU5Qjs7O09BR0c7SUFDSCxTQUFTLGdCQUFnQixDQUFDLElBQW9CLEVBQUUsSUFBb0I7UUFDbEUsT0FBTyxDQUFDLGtCQUFRLENBQUMsSUFBSSxFQUFFLElBQUksQ0FBQyxDQUFDLFVBQVUsQ0FBQyxJQUFJLENBQUMsQ0FBQztJQUNoRCxDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5pbXBvcnQgKiBhcyB0cyBmcm9tICd0eXBlc2NyaXB0JztcblxuaW1wb3J0IHthYnNvbHV0ZUZyb20sIGRpcm5hbWUsIHJlbGF0aXZlLCByZXNvbHZlfSBmcm9tICcuL2hlbHBlcnMnO1xuaW1wb3J0IHtBYnNvbHV0ZUZzUGF0aCwgQnJhbmRlZFBhdGgsIFBhdGhTZWdtZW50fSBmcm9tICcuL3R5cGVzJztcbmltcG9ydCB7c3RyaXBFeHRlbnNpb259IGZyb20gJy4vdXRpbCc7XG5cblxuXG4vKipcbiAqIEEgcGF0aCB0aGF0J3MgcmVsYXRpdmUgdG8gdGhlIGxvZ2ljYWwgcm9vdCBvZiBhIFR5cGVTY3JpcHQgcHJvamVjdCAob25lIG9mIHRoZSBwcm9qZWN0J3NcbiAqIHJvb3REaXJzKS5cbiAqXG4gKiBQYXRocyBpbiB0aGUgdHlwZSBzeXN0ZW0gdXNlIFBPU0lYIGZvcm1hdC5cbiAqL1xuZXhwb3J0IHR5cGUgTG9naWNhbFByb2plY3RQYXRoID0gQnJhbmRlZFBhdGg8J0xvZ2ljYWxQcm9qZWN0UGF0aCc+O1xuXG5leHBvcnQgY29uc3QgTG9naWNhbFByb2plY3RQYXRoID0ge1xuICAvKipcbiAgICogR2V0IHRoZSByZWxhdGl2ZSBwYXRoIGJldHdlZW4gdHdvIGBMb2dpY2FsUHJvamVjdFBhdGhgcy5cbiAgICpcbiAgICogVGhpcyB3aWxsIHJldHVybiBhIGBQYXRoU2VnbWVudGAgd2hpY2ggd291bGQgYmUgYSB2YWxpZCBtb2R1bGUgc3BlY2lmaWVyIHRvIHVzZSBpbiBgZnJvbWAgd2hlblxuICAgKiBpbXBvcnRpbmcgZnJvbSBgdG9gLlxuICAgKi9cbiAgcmVsYXRpdmVQYXRoQmV0d2VlbjogZnVuY3Rpb24oZnJvbTogTG9naWNhbFByb2plY3RQYXRoLCB0bzogTG9naWNhbFByb2plY3RQYXRoKTogUGF0aFNlZ21lbnQge1xuICAgIGxldCByZWxhdGl2ZVBhdGggPSByZWxhdGl2ZShkaXJuYW1lKHJlc29sdmUoZnJvbSkpLCByZXNvbHZlKHRvKSk7XG4gICAgaWYgKCFyZWxhdGl2ZVBhdGguc3RhcnRzV2l0aCgnLi4vJykpIHtcbiAgICAgIHJlbGF0aXZlUGF0aCA9ICgnLi8nICsgcmVsYXRpdmVQYXRoKSBhcyBQYXRoU2VnbWVudDtcbiAgICB9XG4gICAgcmV0dXJuIHJlbGF0aXZlUGF0aCBhcyBQYXRoU2VnbWVudDtcbiAgfSxcbn07XG5cbi8qKlxuICogQSB1dGlsaXR5IGNsYXNzIHdoaWNoIGNhbiB0cmFuc2xhdGUgYWJzb2x1dGUgcGF0aHMgdG8gc291cmNlIGZpbGVzIGludG8gbG9naWNhbCBwYXRocyBpblxuICogVHlwZVNjcmlwdCdzIGxvZ2ljYWwgZmlsZSBzeXN0ZW0sIGJhc2VkIG9uIHRoZSByb290IGRpcmVjdG9yaWVzIG9mIHRoZSBwcm9qZWN0LlxuICovXG5leHBvcnQgY2xhc3MgTG9naWNhbEZpbGVTeXN0ZW0ge1xuICAvKipcbiAgICogVGhlIHJvb3QgZGlyZWN0b3JpZXMgb2YgdGhlIHByb2plY3QsIHNvcnRlZCB3aXRoIHRoZSBsb25nZXN0IHBhdGggZmlyc3QuXG4gICAqL1xuICBwcml2YXRlIHJvb3REaXJzOiBBYnNvbHV0ZUZzUGF0aFtdO1xuXG4gIC8qKlxuICAgKiBUaGUgc2FtZSByb290IGRpcmVjdG9yaWVzIGFzIGByb290RGlyc2AgYnV0IHdpdGggZWFjaCBvbmUgY29udmVydGVkIHRvIGl0c1xuICAgKiBjYW5vbmljYWwgZm9ybSBmb3IgbWF0Y2hpbmcgaW4gY2FzZS1pbnNlbnNpdGl2ZSBmaWxlLXN5c3RlbXMuXG4gICAqL1xuICBwcml2YXRlIGNhbm9uaWNhbFJvb3REaXJzOiBBYnNvbHV0ZUZzUGF0aFtdO1xuXG4gIC8qKlxuICAgKiBBIGNhY2hlIG9mIGZpbGUgcGF0aHMgdG8gcHJvamVjdCBwYXRocywgYmVjYXVzZSBjb21wdXRhdGlvbiBvZiB0aGVzZSBwYXRocyBpcyBzbGlnaHRseVxuICAgKiBleHBlbnNpdmUuXG4gICAqL1xuICBwcml2YXRlIGNhY2hlOiBNYXA8QWJzb2x1dGVGc1BhdGgsIExvZ2ljYWxQcm9qZWN0UGF0aHxudWxsPiA9IG5ldyBNYXAoKTtcblxuICBjb25zdHJ1Y3RvcihcbiAgICAgIHJvb3REaXJzOiBBYnNvbHV0ZUZzUGF0aFtdLFxuICAgICAgcHJpdmF0ZSBjb21waWxlckhvc3Q6IFBpY2s8dHMuQ29tcGlsZXJIb3N0LCAnZ2V0Q2Fub25pY2FsRmlsZU5hbWUnPikge1xuICAgIC8vIE1ha2UgYSBjb3B5IGFuZCBzb3J0IGl0IGJ5IGxlbmd0aCBpbiByZXZlcnNlIG9yZGVyIChsb25nZXN0IGZpcnN0KS4gVGhpcyBzcGVlZHMgdXAgbG9va3VwcyxcbiAgICAvLyBzaW5jZSB0aGVyZSdzIG5vIG5lZWQgdG8ga2VlcCBnb2luZyB0aHJvdWdoIHRoZSBhcnJheSBvbmNlIGEgbWF0Y2ggaXMgZm91bmQuXG4gICAgdGhpcy5yb290RGlycyA9IHJvb3REaXJzLmNvbmNhdChbXSkuc29ydCgoYSwgYikgPT4gYi5sZW5ndGggLSBhLmxlbmd0aCk7XG4gICAgdGhpcy5jYW5vbmljYWxSb290RGlycyA9XG4gICAgICAgIHRoaXMucm9vdERpcnMubWFwKGRpciA9PiB0aGlzLmNvbXBpbGVySG9zdC5nZXRDYW5vbmljYWxGaWxlTmFtZShkaXIpIGFzIEFic29sdXRlRnNQYXRoKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXQgdGhlIGxvZ2ljYWwgcGF0aCBpbiB0aGUgcHJvamVjdCBvZiBhIGB0cy5Tb3VyY2VGaWxlYC5cbiAgICpcbiAgICogVGhpcyBtZXRob2QgaXMgcHJvdmlkZWQgYXMgYSBjb252ZW5pZW50IGFsdGVybmF0aXZlIHRvIGNhbGxpbmdcbiAgICogYGxvZ2ljYWxQYXRoT2ZGaWxlKGFic29sdXRlRnJvbVNvdXJjZUZpbGUoc2YpKWAuXG4gICAqL1xuICBsb2dpY2FsUGF0aE9mU2Yoc2Y6IHRzLlNvdXJjZUZpbGUpOiBMb2dpY2FsUHJvamVjdFBhdGh8bnVsbCB7XG4gICAgcmV0dXJuIHRoaXMubG9naWNhbFBhdGhPZkZpbGUoYWJzb2x1dGVGcm9tKHNmLmZpbGVOYW1lKSk7XG4gIH1cblxuICAvKipcbiAgICogR2V0IHRoZSBsb2dpY2FsIHBhdGggaW4gdGhlIHByb2plY3Qgb2YgYSBzb3VyY2UgZmlsZS5cbiAgICpcbiAgICogQHJldHVybnMgQSBgTG9naWNhbFByb2plY3RQYXRoYCB0byB0aGUgc291cmNlIGZpbGUsIG9yIGBudWxsYCBpZiB0aGUgc291cmNlIGZpbGUgaXMgbm90IGluIGFueVxuICAgKiBvZiB0aGUgVFMgcHJvamVjdCdzIHJvb3QgZGlyZWN0b3JpZXMuXG4gICAqL1xuICBsb2dpY2FsUGF0aE9mRmlsZShwaHlzaWNhbEZpbGU6IEFic29sdXRlRnNQYXRoKTogTG9naWNhbFByb2plY3RQYXRofG51bGwge1xuICAgIGNvbnN0IGNhbm9uaWNhbEZpbGVQYXRoID1cbiAgICAgICAgdGhpcy5jb21waWxlckhvc3QuZ2V0Q2Fub25pY2FsRmlsZU5hbWUocGh5c2ljYWxGaWxlKSBhcyBBYnNvbHV0ZUZzUGF0aDtcbiAgICBpZiAoIXRoaXMuY2FjaGUuaGFzKGNhbm9uaWNhbEZpbGVQYXRoKSkge1xuICAgICAgbGV0IGxvZ2ljYWxGaWxlOiBMb2dpY2FsUHJvamVjdFBhdGh8bnVsbCA9IG51bGw7XG4gICAgICBmb3IgKGxldCBpID0gMDsgaSA8IHRoaXMucm9vdERpcnMubGVuZ3RoOyBpKyspIHtcbiAgICAgICAgY29uc3Qgcm9vdERpciA9IHRoaXMucm9vdERpcnNbaV07XG4gICAgICAgIGNvbnN0IGNhbm9uaWNhbFJvb3REaXIgPSB0aGlzLmNhbm9uaWNhbFJvb3REaXJzW2ldO1xuICAgICAgICBpZiAoaXNXaXRoaW5CYXNlUGF0aChjYW5vbmljYWxSb290RGlyLCBjYW5vbmljYWxGaWxlUGF0aCkpIHtcbiAgICAgICAgICAvLyBOb3RlIHRoYXQgd2UgbWF0Y2ggYWdhaW5zdCBjYW5vbmljYWwgcGF0aHMgYnV0IHRoZW4gY3JlYXRlIHRoZSBsb2dpY2FsIHBhdGggZnJvbVxuICAgICAgICAgIC8vIG9yaWdpbmFsIHBhdGhzLlxuICAgICAgICAgIGxvZ2ljYWxGaWxlID0gdGhpcy5jcmVhdGVMb2dpY2FsUHJvamVjdFBhdGgocGh5c2ljYWxGaWxlLCByb290RGlyKTtcbiAgICAgICAgICAvLyBUaGUgbG9naWNhbCBwcm9qZWN0IGRvZXMgbm90IGluY2x1ZGUgYW55IHNwZWNpYWwgXCJub2RlX21vZHVsZXNcIiBuZXN0ZWQgZGlyZWN0b3JpZXMuXG4gICAgICAgICAgaWYgKGxvZ2ljYWxGaWxlLmluZGV4T2YoJy9ub2RlX21vZHVsZXMvJykgIT09IC0xKSB7XG4gICAgICAgICAgICBsb2dpY2FsRmlsZSA9IG51bGw7XG4gICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICAgIH1cbiAgICAgICAgfVxuICAgICAgfVxuICAgICAgdGhpcy5jYWNoZS5zZXQoY2Fub25pY2FsRmlsZVBhdGgsIGxvZ2ljYWxGaWxlKTtcbiAgICB9XG4gICAgcmV0dXJuIHRoaXMuY2FjaGUuZ2V0KGNhbm9uaWNhbEZpbGVQYXRoKSE7XG4gIH1cblxuICBwcml2YXRlIGNyZWF0ZUxvZ2ljYWxQcm9qZWN0UGF0aChmaWxlOiBBYnNvbHV0ZUZzUGF0aCwgcm9vdERpcjogQWJzb2x1dGVGc1BhdGgpOlxuICAgICAgTG9naWNhbFByb2plY3RQYXRoIHtcbiAgICBjb25zdCBsb2dpY2FsUGF0aCA9IHN0cmlwRXh0ZW5zaW9uKGZpbGUuc3Vic3RyKHJvb3REaXIubGVuZ3RoKSk7XG4gICAgcmV0dXJuIChsb2dpY2FsUGF0aC5zdGFydHNXaXRoKCcvJykgPyBsb2dpY2FsUGF0aCA6ICcvJyArIGxvZ2ljYWxQYXRoKSBhcyBMb2dpY2FsUHJvamVjdFBhdGg7XG4gIH1cbn1cblxuLyoqXG4gKiBJcyB0aGUgYHBhdGhgIGEgZGVzY2VuZGFudCBvZiB0aGUgYGJhc2VgP1xuICogRS5nLiBgZm9vL2Jhci96ZWVgIGlzIHdpdGhpbiBgZm9vL2JhcmAgYnV0IG5vdCB3aXRoaW4gYGZvby9jYXJgLlxuICovXG5mdW5jdGlvbiBpc1dpdGhpbkJhc2VQYXRoKGJhc2U6IEFic29sdXRlRnNQYXRoLCBwYXRoOiBBYnNvbHV0ZUZzUGF0aCk6IGJvb2xlYW4ge1xuICByZXR1cm4gIXJlbGF0aXZlKGJhc2UsIHBhdGgpLnN0YXJ0c1dpdGgoJy4uJyk7XG59XG4iXX0=