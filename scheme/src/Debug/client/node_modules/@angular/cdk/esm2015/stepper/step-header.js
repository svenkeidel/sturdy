/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Directive, ElementRef } from '@angular/core';
let CdkStepHeader = /** @class */ (() => {
    class CdkStepHeader {
        constructor(_elementRef) {
            this._elementRef = _elementRef;
        }
        /** Focuses the step header. */
        focus() {
            this._elementRef.nativeElement.focus();
        }
    }
    CdkStepHeader.decorators = [
        { type: Directive, args: [{
                    selector: '[cdkStepHeader]',
                    host: {
                        'role': 'tab',
                    },
                },] }
    ];
    CdkStepHeader.ctorParameters = () => [
        { type: ElementRef }
    ];
    return CdkStepHeader;
})();
export { CdkStepHeader };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic3RlcC1oZWFkZXIuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL3N0ZXBwZXIvc3RlcC1oZWFkZXIudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFNBQVMsRUFBRSxVQUFVLEVBQUMsTUFBTSxlQUFlLENBQUM7QUFJcEQ7SUFBQSxNQU1hLGFBQWE7UUFDeEIsWUFBc0IsV0FBb0M7WUFBcEMsZ0JBQVcsR0FBWCxXQUFXLENBQXlCO1FBQUcsQ0FBQztRQUU5RCwrQkFBK0I7UUFDL0IsS0FBSztZQUNILElBQUksQ0FBQyxXQUFXLENBQUMsYUFBYSxDQUFDLEtBQUssRUFBRSxDQUFDO1FBQ3pDLENBQUM7OztnQkFaRixTQUFTLFNBQUM7b0JBQ1QsUUFBUSxFQUFFLGlCQUFpQjtvQkFDM0IsSUFBSSxFQUFFO3dCQUNKLE1BQU0sRUFBRSxLQUFLO3FCQUNkO2lCQUNGOzs7Z0JBVGtCLFVBQVU7O0lBaUI3QixvQkFBQztLQUFBO1NBUFksYUFBYSIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0RpcmVjdGl2ZSwgRWxlbWVudFJlZn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQge0ZvY3VzYWJsZU9wdGlvbn0gZnJvbSAnQGFuZ3VsYXIvY2RrL2ExMXknO1xuXG5cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogJ1tjZGtTdGVwSGVhZGVyXScsXG4gIGhvc3Q6IHtcbiAgICAncm9sZSc6ICd0YWInLFxuICB9LFxufSlcbmV4cG9ydCBjbGFzcyBDZGtTdGVwSGVhZGVyIGltcGxlbWVudHMgRm9jdXNhYmxlT3B0aW9uIHtcbiAgY29uc3RydWN0b3IocHJvdGVjdGVkIF9lbGVtZW50UmVmOiBFbGVtZW50UmVmPEhUTUxFbGVtZW50Pikge31cblxuICAvKiogRm9jdXNlcyB0aGUgc3RlcCBoZWFkZXIuICovXG4gIGZvY3VzKCkge1xuICAgIHRoaXMuX2VsZW1lbnRSZWYubmF0aXZlRWxlbWVudC5mb2N1cygpO1xuICB9XG59XG4iXX0=