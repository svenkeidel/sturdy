"use strict";
/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.DevkitFileSystem = void 0;
const core_1 = require("@angular-devkit/core");
const path = require("path");
const file_system_1 = require("../update-tool/file-system");
/**
 * File system that leverages the virtual tree from the CLI devkit. This file
 * system is commonly used by `ng update` migrations that run as part of the
 * Angular CLI.
 */
class DevkitFileSystem extends file_system_1.FileSystem {
    constructor(_tree, workspaceFsPath) {
        super();
        this._tree = _tree;
        this._updateRecorderCache = new Map();
        this._workspaceFsPath = core_1.normalize(workspaceFsPath);
    }
    resolve(...segments) {
        // Note: We use `posix.resolve` as the devkit paths are using posix separators.
        const resolvedPath = core_1.normalize(path.posix.resolve(...segments.map(core_1.normalize)));
        // If the resolved path points to the workspace root, then this is an absolute disk
        // path and we need to compute a devkit tree relative path.
        if (resolvedPath.startsWith(this._workspaceFsPath)) {
            return core_1.relative(this._workspaceFsPath, resolvedPath);
        }
        // Otherwise we know that the path is absolute (due to the resolve), and that it
        // refers to an absolute devkit tree path (like `/angular.json`). We keep those
        // unmodified as they are already resolved workspace paths.
        return resolvedPath;
    }
    edit(filePath) {
        if (this._updateRecorderCache.has(filePath)) {
            return this._updateRecorderCache.get(filePath);
        }
        const recorder = this._tree.beginUpdate(filePath);
        this._updateRecorderCache.set(filePath, recorder);
        return recorder;
    }
    commitEdits() {
        this._updateRecorderCache.forEach(r => this._tree.commitUpdate(r));
        this._updateRecorderCache.clear();
    }
    exists(filePath) {
        return this._tree.exists(filePath);
    }
    overwrite(filePath, content) {
        this._tree.overwrite(filePath, content);
    }
    create(filePath, content) {
        this._tree.create(filePath, content);
    }
    delete(filePath) {
        this._tree.delete(filePath);
    }
    read(filePath) {
        const buffer = this._tree.read(filePath);
        return buffer !== null ? buffer.toString() : null;
    }
}
exports.DevkitFileSystem = DevkitFileSystem;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGV2a2l0LWZpbGUtc3lzdGVtLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9zY2hlbWF0aWNzL25nLXVwZGF0ZS9kZXZraXQtZmlsZS1zeXN0ZW0udHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBOzs7Ozs7R0FNRzs7O0FBRUgsK0NBQStEO0FBRS9ELDZCQUE2QjtBQUM3Qiw0REFBc0Q7QUFFdEQ7Ozs7R0FJRztBQUNILE1BQWEsZ0JBQWlCLFNBQVEsd0JBQWdCO0lBSXBELFlBQW9CLEtBQVcsRUFBRSxlQUF1QjtRQUN0RCxLQUFLLEVBQUUsQ0FBQztRQURVLFVBQUssR0FBTCxLQUFLLENBQU07UUFIdkIseUJBQW9CLEdBQUcsSUFBSSxHQUFHLEVBQTBCLENBQUM7UUFLL0QsSUFBSSxDQUFDLGdCQUFnQixHQUFHLGdCQUFTLENBQUMsZUFBZSxDQUFDLENBQUM7SUFDckQsQ0FBQztJQUVELE9BQU8sQ0FBQyxHQUFHLFFBQWtCO1FBQzNCLCtFQUErRTtRQUMvRSxNQUFNLFlBQVksR0FBRyxnQkFBUyxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsT0FBTyxDQUFDLEdBQUcsUUFBUSxDQUFDLEdBQUcsQ0FBQyxnQkFBUyxDQUFDLENBQUMsQ0FBQyxDQUFDO1FBQy9FLG1GQUFtRjtRQUNuRiwyREFBMkQ7UUFDM0QsSUFBSSxZQUFZLENBQUMsVUFBVSxDQUFDLElBQUksQ0FBQyxnQkFBZ0IsQ0FBQyxFQUFFO1lBQ2xELE9BQU8sZUFBUSxDQUFDLElBQUksQ0FBQyxnQkFBZ0IsRUFBRSxZQUFZLENBQUMsQ0FBQztTQUN0RDtRQUNELGdGQUFnRjtRQUNoRiwrRUFBK0U7UUFDL0UsMkRBQTJEO1FBQzNELE9BQU8sWUFBWSxDQUFDO0lBQ3RCLENBQUM7SUFFRCxJQUFJLENBQUMsUUFBYztRQUNqQixJQUFJLElBQUksQ0FBQyxvQkFBb0IsQ0FBQyxHQUFHLENBQUMsUUFBUSxDQUFDLEVBQUU7WUFDM0MsT0FBTyxJQUFJLENBQUMsb0JBQW9CLENBQUMsR0FBRyxDQUFDLFFBQVEsQ0FBRSxDQUFDO1NBQ2pEO1FBQ0QsTUFBTSxRQUFRLEdBQUcsSUFBSSxDQUFDLEtBQUssQ0FBQyxXQUFXLENBQUMsUUFBUSxDQUFDLENBQUM7UUFDbEQsSUFBSSxDQUFDLG9CQUFvQixDQUFDLEdBQUcsQ0FBQyxRQUFRLEVBQUUsUUFBUSxDQUFDLENBQUM7UUFDbEQsT0FBTyxRQUFRLENBQUM7SUFDbEIsQ0FBQztJQUVELFdBQVc7UUFDVCxJQUFJLENBQUMsb0JBQW9CLENBQUMsT0FBTyxDQUFDLENBQUMsQ0FBQyxFQUFFLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxZQUFZLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQztRQUNuRSxJQUFJLENBQUMsb0JBQW9CLENBQUMsS0FBSyxFQUFFLENBQUM7SUFDcEMsQ0FBQztJQUVELE1BQU0sQ0FBQyxRQUFjO1FBQ25CLE9BQU8sSUFBSSxDQUFDLEtBQUssQ0FBQyxNQUFNLENBQUMsUUFBUSxDQUFDLENBQUM7SUFDckMsQ0FBQztJQUVELFNBQVMsQ0FBQyxRQUFjLEVBQUUsT0FBZTtRQUN2QyxJQUFJLENBQUMsS0FBSyxDQUFDLFNBQVMsQ0FBQyxRQUFRLEVBQUUsT0FBTyxDQUFDLENBQUM7SUFDMUMsQ0FBQztJQUVELE1BQU0sQ0FBQyxRQUFjLEVBQUUsT0FBZTtRQUNwQyxJQUFJLENBQUMsS0FBSyxDQUFDLE1BQU0sQ0FBQyxRQUFRLEVBQUUsT0FBTyxDQUFDLENBQUM7SUFDdkMsQ0FBQztJQUVELE1BQU0sQ0FBQyxRQUFjO1FBQ25CLElBQUksQ0FBQyxLQUFLLENBQUMsTUFBTSxDQUFDLFFBQVEsQ0FBQyxDQUFDO0lBQzlCLENBQUM7SUFFRCxJQUFJLENBQUMsUUFBYztRQUNqQixNQUFNLE1BQU0sR0FBRyxJQUFJLENBQUMsS0FBSyxDQUFDLElBQUksQ0FBQyxRQUFRLENBQUMsQ0FBQztRQUN6QyxPQUFPLE1BQU0sS0FBSyxJQUFJLENBQUMsQ0FBQyxDQUFDLE1BQU0sQ0FBQyxRQUFRLEVBQUUsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDO0lBQ3BELENBQUM7Q0FDRjtBQXpERCw0Q0F5REMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtub3JtYWxpemUsIFBhdGgsIHJlbGF0aXZlfSBmcm9tICdAYW5ndWxhci1kZXZraXQvY29yZSc7XG5pbXBvcnQge1RyZWUsIFVwZGF0ZVJlY29yZGVyfSBmcm9tICdAYW5ndWxhci1kZXZraXQvc2NoZW1hdGljcyc7XG5pbXBvcnQgKiBhcyBwYXRoIGZyb20gJ3BhdGgnO1xuaW1wb3J0IHtGaWxlU3lzdGVtfSBmcm9tICcuLi91cGRhdGUtdG9vbC9maWxlLXN5c3RlbSc7XG5cbi8qKlxuICogRmlsZSBzeXN0ZW0gdGhhdCBsZXZlcmFnZXMgdGhlIHZpcnR1YWwgdHJlZSBmcm9tIHRoZSBDTEkgZGV2a2l0LiBUaGlzIGZpbGVcbiAqIHN5c3RlbSBpcyBjb21tb25seSB1c2VkIGJ5IGBuZyB1cGRhdGVgIG1pZ3JhdGlvbnMgdGhhdCBydW4gYXMgcGFydCBvZiB0aGVcbiAqIEFuZ3VsYXIgQ0xJLlxuICovXG5leHBvcnQgY2xhc3MgRGV2a2l0RmlsZVN5c3RlbSBleHRlbmRzIEZpbGVTeXN0ZW08UGF0aD4ge1xuICBwcml2YXRlIF91cGRhdGVSZWNvcmRlckNhY2hlID0gbmV3IE1hcDxzdHJpbmcsIFVwZGF0ZVJlY29yZGVyPigpO1xuICBwcml2YXRlIF93b3Jrc3BhY2VGc1BhdGg6IFBhdGg7XG5cbiAgY29uc3RydWN0b3IocHJpdmF0ZSBfdHJlZTogVHJlZSwgd29ya3NwYWNlRnNQYXRoOiBzdHJpbmcpIHtcbiAgICBzdXBlcigpO1xuICAgIHRoaXMuX3dvcmtzcGFjZUZzUGF0aCA9IG5vcm1hbGl6ZSh3b3Jrc3BhY2VGc1BhdGgpO1xuICB9XG5cbiAgcmVzb2x2ZSguLi5zZWdtZW50czogc3RyaW5nW10pOiBQYXRoIHtcbiAgICAvLyBOb3RlOiBXZSB1c2UgYHBvc2l4LnJlc29sdmVgIGFzIHRoZSBkZXZraXQgcGF0aHMgYXJlIHVzaW5nIHBvc2l4IHNlcGFyYXRvcnMuXG4gICAgY29uc3QgcmVzb2x2ZWRQYXRoID0gbm9ybWFsaXplKHBhdGgucG9zaXgucmVzb2x2ZSguLi5zZWdtZW50cy5tYXAobm9ybWFsaXplKSkpO1xuICAgIC8vIElmIHRoZSByZXNvbHZlZCBwYXRoIHBvaW50cyB0byB0aGUgd29ya3NwYWNlIHJvb3QsIHRoZW4gdGhpcyBpcyBhbiBhYnNvbHV0ZSBkaXNrXG4gICAgLy8gcGF0aCBhbmQgd2UgbmVlZCB0byBjb21wdXRlIGEgZGV2a2l0IHRyZWUgcmVsYXRpdmUgcGF0aC5cbiAgICBpZiAocmVzb2x2ZWRQYXRoLnN0YXJ0c1dpdGgodGhpcy5fd29ya3NwYWNlRnNQYXRoKSkge1xuICAgICAgcmV0dXJuIHJlbGF0aXZlKHRoaXMuX3dvcmtzcGFjZUZzUGF0aCwgcmVzb2x2ZWRQYXRoKTtcbiAgICB9XG4gICAgLy8gT3RoZXJ3aXNlIHdlIGtub3cgdGhhdCB0aGUgcGF0aCBpcyBhYnNvbHV0ZSAoZHVlIHRvIHRoZSByZXNvbHZlKSwgYW5kIHRoYXQgaXRcbiAgICAvLyByZWZlcnMgdG8gYW4gYWJzb2x1dGUgZGV2a2l0IHRyZWUgcGF0aCAobGlrZSBgL2FuZ3VsYXIuanNvbmApLiBXZSBrZWVwIHRob3NlXG4gICAgLy8gdW5tb2RpZmllZCBhcyB0aGV5IGFyZSBhbHJlYWR5IHJlc29sdmVkIHdvcmtzcGFjZSBwYXRocy5cbiAgICByZXR1cm4gcmVzb2x2ZWRQYXRoO1xuICB9XG5cbiAgZWRpdChmaWxlUGF0aDogUGF0aCkge1xuICAgIGlmICh0aGlzLl91cGRhdGVSZWNvcmRlckNhY2hlLmhhcyhmaWxlUGF0aCkpIHtcbiAgICAgIHJldHVybiB0aGlzLl91cGRhdGVSZWNvcmRlckNhY2hlLmdldChmaWxlUGF0aCkhO1xuICAgIH1cbiAgICBjb25zdCByZWNvcmRlciA9IHRoaXMuX3RyZWUuYmVnaW5VcGRhdGUoZmlsZVBhdGgpO1xuICAgIHRoaXMuX3VwZGF0ZVJlY29yZGVyQ2FjaGUuc2V0KGZpbGVQYXRoLCByZWNvcmRlcik7XG4gICAgcmV0dXJuIHJlY29yZGVyO1xuICB9XG5cbiAgY29tbWl0RWRpdHMoKSB7XG4gICAgdGhpcy5fdXBkYXRlUmVjb3JkZXJDYWNoZS5mb3JFYWNoKHIgPT4gdGhpcy5fdHJlZS5jb21taXRVcGRhdGUocikpO1xuICAgIHRoaXMuX3VwZGF0ZVJlY29yZGVyQ2FjaGUuY2xlYXIoKTtcbiAgfVxuXG4gIGV4aXN0cyhmaWxlUGF0aDogUGF0aCkge1xuICAgIHJldHVybiB0aGlzLl90cmVlLmV4aXN0cyhmaWxlUGF0aCk7XG4gIH1cblxuICBvdmVyd3JpdGUoZmlsZVBhdGg6IFBhdGgsIGNvbnRlbnQ6IHN0cmluZykge1xuICAgIHRoaXMuX3RyZWUub3ZlcndyaXRlKGZpbGVQYXRoLCBjb250ZW50KTtcbiAgfVxuXG4gIGNyZWF0ZShmaWxlUGF0aDogUGF0aCwgY29udGVudDogc3RyaW5nKSB7XG4gICAgdGhpcy5fdHJlZS5jcmVhdGUoZmlsZVBhdGgsIGNvbnRlbnQpO1xuICB9XG5cbiAgZGVsZXRlKGZpbGVQYXRoOiBQYXRoKSB7XG4gICAgdGhpcy5fdHJlZS5kZWxldGUoZmlsZVBhdGgpO1xuICB9XG5cbiAgcmVhZChmaWxlUGF0aDogUGF0aCkge1xuICAgIGNvbnN0IGJ1ZmZlciA9IHRoaXMuX3RyZWUucmVhZChmaWxlUGF0aCk7XG4gICAgcmV0dXJuIGJ1ZmZlciAhPT0gbnVsbCA/IGJ1ZmZlci50b1N0cmluZygpIDogbnVsbDtcbiAgfVxufVxuIl19