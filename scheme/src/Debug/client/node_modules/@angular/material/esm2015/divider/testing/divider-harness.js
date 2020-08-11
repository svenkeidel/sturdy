/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
/**
 * Harness for interacting with a `mat-divider`.
 * @dynamic
 */
let MatDividerHarness = /** @class */ (() => {
    class MatDividerHarness extends ComponentHarness {
        static with(options = {}) {
            return new HarnessPredicate(MatDividerHarness, options);
        }
        getOrientation() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('aria-orientation');
            });
        }
        isInset() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-divider-inset');
            });
        }
    }
    MatDividerHarness.hostSelector = 'mat-divider';
    return MatDividerHarness;
})();
export { MatDividerHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGl2aWRlci1oYXJuZXNzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL2RpdmlkZXIvdGVzdGluZy9kaXZpZGVyLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBR3hFOzs7R0FHRztBQUNIO0lBQUEsTUFBYSxpQkFBa0IsU0FBUSxnQkFBZ0I7UUFHckQsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUFpQyxFQUFFO1lBQzdDLE9BQU8sSUFBSSxnQkFBZ0IsQ0FBQyxpQkFBaUIsRUFBRSxPQUFPLENBQUMsQ0FBQztRQUMxRCxDQUFDO1FBRUssY0FBYzs7Z0JBQ2xCLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFlBQVksQ0FBQyxrQkFBa0IsQ0FDcEIsQ0FBQztZQUN6QyxDQUFDO1NBQUE7UUFFSyxPQUFPOztnQkFDWCxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxRQUFRLENBQUMsbUJBQW1CLENBQUMsQ0FBQztZQUMzRCxDQUFDO1NBQUE7O0lBYk0sOEJBQVksR0FBRyxhQUFhLENBQUM7SUFjdEMsd0JBQUM7S0FBQTtTQWZZLGlCQUFpQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0NvbXBvbmVudEhhcm5lc3MsIEhhcm5lc3NQcmVkaWNhdGV9IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7RGl2aWRlckhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL2RpdmlkZXItaGFybmVzcy1maWx0ZXJzJztcblxuLyoqXG4gKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgYG1hdC1kaXZpZGVyYC5cbiAqIEBkeW5hbWljXG4gKi9cbmV4cG9ydCBjbGFzcyBNYXREaXZpZGVySGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJ21hdC1kaXZpZGVyJztcblxuICBzdGF0aWMgd2l0aChvcHRpb25zOiBEaXZpZGVySGFybmVzc0ZpbHRlcnMgPSB7fSkge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXREaXZpZGVySGFybmVzcywgb3B0aW9ucyk7XG4gIH1cblxuICBhc3luYyBnZXRPcmllbnRhdGlvbigpOiBQcm9taXNlPCdob3Jpem9udGFsJyB8ICd2ZXJ0aWNhbCc+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ2FyaWEtb3JpZW50YXRpb24nKSBhc1xuICAgICAgICBQcm9taXNlPCdob3Jpem9udGFsJyB8ICd2ZXJ0aWNhbCc+O1xuICB9XG5cbiAgYXN5bmMgaXNJbnNldCgpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5oYXNDbGFzcygnbWF0LWRpdmlkZXItaW5zZXQnKTtcbiAgfVxufVxuIl19