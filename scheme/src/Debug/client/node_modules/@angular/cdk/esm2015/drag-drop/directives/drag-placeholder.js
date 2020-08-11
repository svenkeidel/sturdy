/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Directive, TemplateRef, Input } from '@angular/core';
/**
 * Element that will be used as a template for the placeholder of a CdkDrag when
 * it is being dragged. The placeholder is displayed in place of the element being dragged.
 */
let CdkDragPlaceholder = /** @class */ (() => {
    class CdkDragPlaceholder {
        constructor(templateRef) {
            this.templateRef = templateRef;
        }
    }
    CdkDragPlaceholder.decorators = [
        { type: Directive, args: [{
                    selector: 'ng-template[cdkDragPlaceholder]'
                },] }
    ];
    CdkDragPlaceholder.ctorParameters = () => [
        { type: TemplateRef }
    ];
    CdkDragPlaceholder.propDecorators = {
        data: [{ type: Input }]
    };
    return CdkDragPlaceholder;
})();
export { CdkDragPlaceholder };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZHJhZy1wbGFjZWhvbGRlci5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9jZGsvZHJhZy1kcm9wL2RpcmVjdGl2ZXMvZHJhZy1wbGFjZWhvbGRlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsU0FBUyxFQUFFLFdBQVcsRUFBRSxLQUFLLEVBQUMsTUFBTSxlQUFlLENBQUM7QUFFNUQ7OztHQUdHO0FBQ0g7SUFBQSxNQUdhLGtCQUFrQjtRQUc3QixZQUFtQixXQUEyQjtZQUEzQixnQkFBVyxHQUFYLFdBQVcsQ0FBZ0I7UUFBRyxDQUFDOzs7Z0JBTm5ELFNBQVMsU0FBQztvQkFDVCxRQUFRLEVBQUUsaUNBQWlDO2lCQUM1Qzs7O2dCQVJrQixXQUFXOzs7dUJBVzNCLEtBQUs7O0lBRVIseUJBQUM7S0FBQTtTQUpZLGtCQUFrQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0RpcmVjdGl2ZSwgVGVtcGxhdGVSZWYsIElucHV0fSBmcm9tICdAYW5ndWxhci9jb3JlJztcblxuLyoqXG4gKiBFbGVtZW50IHRoYXQgd2lsbCBiZSB1c2VkIGFzIGEgdGVtcGxhdGUgZm9yIHRoZSBwbGFjZWhvbGRlciBvZiBhIENka0RyYWcgd2hlblxuICogaXQgaXMgYmVpbmcgZHJhZ2dlZC4gVGhlIHBsYWNlaG9sZGVyIGlzIGRpc3BsYXllZCBpbiBwbGFjZSBvZiB0aGUgZWxlbWVudCBiZWluZyBkcmFnZ2VkLlxuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICduZy10ZW1wbGF0ZVtjZGtEcmFnUGxhY2Vob2xkZXJdJ1xufSlcbmV4cG9ydCBjbGFzcyBDZGtEcmFnUGxhY2Vob2xkZXI8VCA9IGFueT4ge1xuICAvKiogQ29udGV4dCBkYXRhIHRvIGJlIGFkZGVkIHRvIHRoZSBwbGFjZWhvbGRlciB0ZW1wbGF0ZSBpbnN0YW5jZS4gKi9cbiAgQElucHV0KCkgZGF0YTogVDtcbiAgY29uc3RydWN0b3IocHVibGljIHRlbXBsYXRlUmVmOiBUZW1wbGF0ZVJlZjxUPikge31cbn1cbiJdfQ==