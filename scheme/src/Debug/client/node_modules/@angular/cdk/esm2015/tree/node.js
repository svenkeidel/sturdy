/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Directive, TemplateRef } from '@angular/core';
/** Context provided to the tree node component. */
export class CdkTreeNodeOutletContext {
    constructor(data) {
        this.$implicit = data;
    }
}
/**
 * Data node definition for the CdkTree.
 * Captures the node's template and a when predicate that describes when this node should be used.
 */
let CdkTreeNodeDef = /** @class */ (() => {
    class CdkTreeNodeDef {
        /** @docs-private */
        constructor(template) {
            this.template = template;
        }
    }
    CdkTreeNodeDef.decorators = [
        { type: Directive, args: [{
                    selector: '[cdkTreeNodeDef]',
                    inputs: [
                        'when: cdkTreeNodeDefWhen'
                    ],
                },] }
    ];
    CdkTreeNodeDef.ctorParameters = () => [
        { type: TemplateRef }
    ];
    return CdkTreeNodeDef;
})();
export { CdkTreeNodeDef };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibm9kZS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9jZGsvdHJlZS9ub2RlLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU8sRUFBQyxTQUFTLEVBQUUsV0FBVyxFQUFDLE1BQU0sZUFBZSxDQUFDO0FBR3JELG1EQUFtRDtBQUNuRCxNQUFNLE9BQU8sd0JBQXdCO0lBYW5DLFlBQVksSUFBTztRQUNqQixJQUFJLENBQUMsU0FBUyxHQUFHLElBQUksQ0FBQztJQUN4QixDQUFDO0NBQ0Y7QUFFRDs7O0dBR0c7QUFDSDtJQUFBLE1BTWEsY0FBYztRQVV6QixvQkFBb0I7UUFDcEIsWUFBbUIsUUFBMEI7WUFBMUIsYUFBUSxHQUFSLFFBQVEsQ0FBa0I7UUFBRyxDQUFDOzs7Z0JBakJsRCxTQUFTLFNBQUM7b0JBQ1QsUUFBUSxFQUFFLGtCQUFrQjtvQkFDNUIsTUFBTSxFQUFFO3dCQUNOLDBCQUEwQjtxQkFDM0I7aUJBQ0Y7OztnQkEvQmtCLFdBQVc7O0lBNEM5QixxQkFBQztLQUFBO1NBWlksY0FBYyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0RpcmVjdGl2ZSwgVGVtcGxhdGVSZWZ9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuXG5cbi8qKiBDb250ZXh0IHByb3ZpZGVkIHRvIHRoZSB0cmVlIG5vZGUgY29tcG9uZW50LiAqL1xuZXhwb3J0IGNsYXNzIENka1RyZWVOb2RlT3V0bGV0Q29udGV4dDxUPiB7XG4gIC8qKiBEYXRhIGZvciB0aGUgbm9kZS4gKi9cbiAgJGltcGxpY2l0OiBUO1xuXG4gIC8qKiBEZXB0aCBvZiB0aGUgbm9kZS4gKi9cbiAgbGV2ZWw6IG51bWJlcjtcblxuICAvKiogSW5kZXggbG9jYXRpb24gb2YgdGhlIG5vZGUuICovXG4gIGluZGV4PzogbnVtYmVyO1xuXG4gIC8qKiBMZW5ndGggb2YgdGhlIG51bWJlciBvZiB0b3RhbCBkYXRhTm9kZXMuICovXG4gIGNvdW50PzogbnVtYmVyO1xuXG4gIGNvbnN0cnVjdG9yKGRhdGE6IFQpIHtcbiAgICB0aGlzLiRpbXBsaWNpdCA9IGRhdGE7XG4gIH1cbn1cblxuLyoqXG4gKiBEYXRhIG5vZGUgZGVmaW5pdGlvbiBmb3IgdGhlIENka1RyZWUuXG4gKiBDYXB0dXJlcyB0aGUgbm9kZSdzIHRlbXBsYXRlIGFuZCBhIHdoZW4gcHJlZGljYXRlIHRoYXQgZGVzY3JpYmVzIHdoZW4gdGhpcyBub2RlIHNob3VsZCBiZSB1c2VkLlxuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICdbY2RrVHJlZU5vZGVEZWZdJyxcbiAgaW5wdXRzOiBbXG4gICAgJ3doZW46IGNka1RyZWVOb2RlRGVmV2hlbidcbiAgXSxcbn0pXG5leHBvcnQgY2xhc3MgQ2RrVHJlZU5vZGVEZWY8VD4ge1xuICAvKipcbiAgICogRnVuY3Rpb24gdGhhdCBzaG91bGQgcmV0dXJuIHRydWUgaWYgdGhpcyBub2RlIHRlbXBsYXRlIHNob3VsZCBiZSB1c2VkIGZvciB0aGUgcHJvdmlkZWQgbm9kZVxuICAgKiBkYXRhIGFuZCBpbmRleC4gSWYgbGVmdCB1bmRlZmluZWQsIHRoaXMgbm9kZSB3aWxsIGJlIGNvbnNpZGVyZWQgdGhlIGRlZmF1bHQgbm9kZSB0ZW1wbGF0ZSB0b1xuICAgKiB1c2Ugd2hlbiBubyBvdGhlciB3aGVuIGZ1bmN0aW9ucyByZXR1cm4gdHJ1ZSBmb3IgdGhlIGRhdGEuXG4gICAqIEZvciBldmVyeSBub2RlLCB0aGVyZSBtdXN0IGJlIGF0IGxlYXN0IG9uZSB3aGVuIGZ1bmN0aW9uIHRoYXQgcGFzc2VzIG9yIGFuIHVuZGVmaW5lZCB0b1xuICAgKiBkZWZhdWx0LlxuICAgKi9cbiAgd2hlbjogKGluZGV4OiBudW1iZXIsIG5vZGVEYXRhOiBUKSA9PiBib29sZWFuO1xuXG4gIC8qKiBAZG9jcy1wcml2YXRlICovXG4gIGNvbnN0cnVjdG9yKHB1YmxpYyB0ZW1wbGF0ZTogVGVtcGxhdGVSZWY8YW55Pikge31cbn1cbiJdfQ==