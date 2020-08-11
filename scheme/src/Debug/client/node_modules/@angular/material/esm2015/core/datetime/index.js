/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { PlatformModule } from '@angular/cdk/platform';
import { NgModule } from '@angular/core';
import { DateAdapter } from './date-adapter';
import { MAT_DATE_FORMATS } from './date-formats';
import { NativeDateAdapter } from './native-date-adapter';
import { MAT_NATIVE_DATE_FORMATS } from './native-date-formats';
export * from './date-adapter';
export * from './date-formats';
export * from './native-date-adapter';
export * from './native-date-formats';
let NativeDateModule = /** @class */ (() => {
    class NativeDateModule {
    }
    NativeDateModule.decorators = [
        { type: NgModule, args: [{
                    imports: [PlatformModule],
                    providers: [
                        { provide: DateAdapter, useClass: NativeDateAdapter },
                    ],
                },] }
    ];
    return NativeDateModule;
})();
export { NativeDateModule };
const ɵ0 = MAT_NATIVE_DATE_FORMATS;
let MatNativeDateModule = /** @class */ (() => {
    class MatNativeDateModule {
    }
    MatNativeDateModule.decorators = [
        { type: NgModule, args: [{
                    imports: [NativeDateModule],
                    providers: [{ provide: MAT_DATE_FORMATS, useValue: ɵ0 }],
                },] }
    ];
    return MatNativeDateModule;
})();
export { MatNativeDateModule };
export { ɵ0 };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5kZXguanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvY29yZS9kYXRldGltZS9pbmRleC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsY0FBYyxFQUFDLE1BQU0sdUJBQXVCLENBQUM7QUFDckQsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUN2QyxPQUFPLEVBQUMsV0FBVyxFQUFDLE1BQU0sZ0JBQWdCLENBQUM7QUFDM0MsT0FBTyxFQUFDLGdCQUFnQixFQUFDLE1BQU0sZ0JBQWdCLENBQUM7QUFDaEQsT0FBTyxFQUFDLGlCQUFpQixFQUFDLE1BQU0sdUJBQXVCLENBQUM7QUFDeEQsT0FBTyxFQUFDLHVCQUF1QixFQUFDLE1BQU0sdUJBQXVCLENBQUM7QUFFOUQsY0FBYyxnQkFBZ0IsQ0FBQztBQUMvQixjQUFjLGdCQUFnQixDQUFDO0FBQy9CLGNBQWMsdUJBQXVCLENBQUM7QUFDdEMsY0FBYyx1QkFBdUIsQ0FBQztBQUd0QztJQUFBLE1BTWEsZ0JBQWdCOzs7Z0JBTjVCLFFBQVEsU0FBQztvQkFDUixPQUFPLEVBQUUsQ0FBQyxjQUFjLENBQUM7b0JBQ3pCLFNBQVMsRUFBRTt3QkFDVCxFQUFDLE9BQU8sRUFBRSxXQUFXLEVBQUUsUUFBUSxFQUFFLGlCQUFpQixFQUFDO3FCQUNwRDtpQkFDRjs7SUFDOEIsdUJBQUM7S0FBQTtTQUFuQixnQkFBZ0I7V0FLdUIsdUJBQXVCO0FBRjNFO0lBQUEsTUFJYSxtQkFBbUI7OztnQkFKL0IsUUFBUSxTQUFDO29CQUNSLE9BQU8sRUFBRSxDQUFDLGdCQUFnQixDQUFDO29CQUMzQixTQUFTLEVBQUUsQ0FBQyxFQUFDLE9BQU8sRUFBRSxnQkFBZ0IsRUFBRSxRQUFRLElBQXlCLEVBQUMsQ0FBQztpQkFDNUU7O0lBQ2lDLDBCQUFDO0tBQUE7U0FBdEIsbUJBQW1CIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7UGxhdGZvcm1Nb2R1bGV9IGZyb20gJ0Bhbmd1bGFyL2Nkay9wbGF0Zm9ybSc7XG5pbXBvcnQge05nTW9kdWxlfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7RGF0ZUFkYXB0ZXJ9IGZyb20gJy4vZGF0ZS1hZGFwdGVyJztcbmltcG9ydCB7TUFUX0RBVEVfRk9STUFUU30gZnJvbSAnLi9kYXRlLWZvcm1hdHMnO1xuaW1wb3J0IHtOYXRpdmVEYXRlQWRhcHRlcn0gZnJvbSAnLi9uYXRpdmUtZGF0ZS1hZGFwdGVyJztcbmltcG9ydCB7TUFUX05BVElWRV9EQVRFX0ZPUk1BVFN9IGZyb20gJy4vbmF0aXZlLWRhdGUtZm9ybWF0cyc7XG5cbmV4cG9ydCAqIGZyb20gJy4vZGF0ZS1hZGFwdGVyJztcbmV4cG9ydCAqIGZyb20gJy4vZGF0ZS1mb3JtYXRzJztcbmV4cG9ydCAqIGZyb20gJy4vbmF0aXZlLWRhdGUtYWRhcHRlcic7XG5leHBvcnQgKiBmcm9tICcuL25hdGl2ZS1kYXRlLWZvcm1hdHMnO1xuXG5cbkBOZ01vZHVsZSh7XG4gIGltcG9ydHM6IFtQbGF0Zm9ybU1vZHVsZV0sXG4gIHByb3ZpZGVyczogW1xuICAgIHtwcm92aWRlOiBEYXRlQWRhcHRlciwgdXNlQ2xhc3M6IE5hdGl2ZURhdGVBZGFwdGVyfSxcbiAgXSxcbn0pXG5leHBvcnQgY2xhc3MgTmF0aXZlRGF0ZU1vZHVsZSB7fVxuXG5cbkBOZ01vZHVsZSh7XG4gIGltcG9ydHM6IFtOYXRpdmVEYXRlTW9kdWxlXSxcbiAgcHJvdmlkZXJzOiBbe3Byb3ZpZGU6IE1BVF9EQVRFX0ZPUk1BVFMsIHVzZVZhbHVlOiBNQVRfTkFUSVZFX0RBVEVfRk9STUFUU31dLFxufSlcbmV4cG9ydCBjbGFzcyBNYXROYXRpdmVEYXRlTW9kdWxlIHt9XG4iXX0=