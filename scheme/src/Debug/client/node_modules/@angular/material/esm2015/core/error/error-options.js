/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Injectable } from '@angular/core';
import * as i0 from "@angular/core";
/** Error state matcher that matches when a control is invalid and dirty. */
let ShowOnDirtyErrorStateMatcher = /** @class */ (() => {
    class ShowOnDirtyErrorStateMatcher {
        isErrorState(control, form) {
            return !!(control && control.invalid && (control.dirty || (form && form.submitted)));
        }
    }
    ShowOnDirtyErrorStateMatcher.decorators = [
        { type: Injectable }
    ];
    return ShowOnDirtyErrorStateMatcher;
})();
export { ShowOnDirtyErrorStateMatcher };
/** Provider that defines how form controls behave with regards to displaying error messages. */
let ErrorStateMatcher = /** @class */ (() => {
    class ErrorStateMatcher {
        isErrorState(control, form) {
            return !!(control && control.invalid && (control.touched || (form && form.submitted)));
        }
    }
    ErrorStateMatcher.ɵprov = i0.ɵɵdefineInjectable({ factory: function ErrorStateMatcher_Factory() { return new ErrorStateMatcher(); }, token: ErrorStateMatcher, providedIn: "root" });
    ErrorStateMatcher.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    return ErrorStateMatcher;
})();
export { ErrorStateMatcher };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZXJyb3Itb3B0aW9ucy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9jb3JlL2Vycm9yL2Vycm9yLW9wdGlvbnMudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFVBQVUsRUFBQyxNQUFNLGVBQWUsQ0FBQzs7QUFHekMsNEVBQTRFO0FBQzVFO0lBQUEsTUFDYSw0QkFBNEI7UUFDdkMsWUFBWSxDQUFDLE9BQTJCLEVBQUUsSUFBd0M7WUFDaEYsT0FBTyxDQUFDLENBQUMsQ0FBQyxPQUFPLElBQUksT0FBTyxDQUFDLE9BQU8sSUFBSSxDQUFDLE9BQU8sQ0FBQyxLQUFLLElBQUksQ0FBQyxJQUFJLElBQUksSUFBSSxDQUFDLFNBQVMsQ0FBQyxDQUFDLENBQUMsQ0FBQztRQUN2RixDQUFDOzs7Z0JBSkYsVUFBVTs7SUFLWCxtQ0FBQztLQUFBO1NBSlksNEJBQTRCO0FBTXpDLGdHQUFnRztBQUNoRztJQUFBLE1BQ2EsaUJBQWlCO1FBQzVCLFlBQVksQ0FBQyxPQUEyQixFQUFFLElBQXdDO1lBQ2hGLE9BQU8sQ0FBQyxDQUFDLENBQUMsT0FBTyxJQUFJLE9BQU8sQ0FBQyxPQUFPLElBQUksQ0FBQyxPQUFPLENBQUMsT0FBTyxJQUFJLENBQUMsSUFBSSxJQUFJLElBQUksQ0FBQyxTQUFTLENBQUMsQ0FBQyxDQUFDLENBQUM7UUFDekYsQ0FBQzs7OztnQkFKRixVQUFVLFNBQUMsRUFBQyxVQUFVLEVBQUUsTUFBTSxFQUFDOzs0QkFwQmhDO0tBeUJDO1NBSlksaUJBQWlCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7SW5qZWN0YWJsZX0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQge0Zvcm1Hcm91cERpcmVjdGl2ZSwgTmdGb3JtLCBGb3JtQ29udHJvbH0gZnJvbSAnQGFuZ3VsYXIvZm9ybXMnO1xuXG4vKiogRXJyb3Igc3RhdGUgbWF0Y2hlciB0aGF0IG1hdGNoZXMgd2hlbiBhIGNvbnRyb2wgaXMgaW52YWxpZCBhbmQgZGlydHkuICovXG5ASW5qZWN0YWJsZSgpXG5leHBvcnQgY2xhc3MgU2hvd09uRGlydHlFcnJvclN0YXRlTWF0Y2hlciBpbXBsZW1lbnRzIEVycm9yU3RhdGVNYXRjaGVyIHtcbiAgaXNFcnJvclN0YXRlKGNvbnRyb2w6IEZvcm1Db250cm9sIHwgbnVsbCwgZm9ybTogRm9ybUdyb3VwRGlyZWN0aXZlIHwgTmdGb3JtIHwgbnVsbCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiAhIShjb250cm9sICYmIGNvbnRyb2wuaW52YWxpZCAmJiAoY29udHJvbC5kaXJ0eSB8fCAoZm9ybSAmJiBmb3JtLnN1Ym1pdHRlZCkpKTtcbiAgfVxufVxuXG4vKiogUHJvdmlkZXIgdGhhdCBkZWZpbmVzIGhvdyBmb3JtIGNvbnRyb2xzIGJlaGF2ZSB3aXRoIHJlZ2FyZHMgdG8gZGlzcGxheWluZyBlcnJvciBtZXNzYWdlcy4gKi9cbkBJbmplY3RhYmxlKHtwcm92aWRlZEluOiAncm9vdCd9KVxuZXhwb3J0IGNsYXNzIEVycm9yU3RhdGVNYXRjaGVyIHtcbiAgaXNFcnJvclN0YXRlKGNvbnRyb2w6IEZvcm1Db250cm9sIHwgbnVsbCwgZm9ybTogRm9ybUdyb3VwRGlyZWN0aXZlIHwgTmdGb3JtIHwgbnVsbCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiAhIShjb250cm9sICYmIGNvbnRyb2wuaW52YWxpZCAmJiAoY29udHJvbC50b3VjaGVkIHx8IChmb3JtICYmIGZvcm0uc3VibWl0dGVkKSkpO1xuICB9XG59XG4iXX0=