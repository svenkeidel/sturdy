/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MatSortHeaderHarness } from './sort-header-harness';
/** Harness for interacting with a standard `mat-sort` in tests. */
let MatSortHarness = /** @class */ (() => {
    class MatSortHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `mat-sort` with specific attributes.
         * @param options Options for narrowing the search.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatSortHarness, options);
        }
        /** Gets all of the sort headers in the `mat-sort`. */
        getSortHeaders(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatSortHeaderHarness.with(filter))();
            });
        }
        /** Gets the selected header in the `mat-sort`. */
        getActiveHeader() {
            return __awaiter(this, void 0, void 0, function* () {
                const headers = yield this.getSortHeaders();
                for (let i = 0; i < headers.length; i++) {
                    if (yield headers[i].isActive()) {
                        return headers[i];
                    }
                }
                return null;
            });
        }
    }
    MatSortHarness.hostSelector = '.mat-sort';
    return MatSortHarness;
})();
export { MatSortHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic29ydC1oYXJuZXNzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL3NvcnQvdGVzdGluZy9zb3J0LWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBRXhFLE9BQU8sRUFBQyxvQkFBb0IsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBRTNELG1FQUFtRTtBQUNuRTtJQUFBLE1BQWEsY0FBZSxTQUFRLGdCQUFnQjtRQUdsRDs7OztXQUlHO1FBQ0gsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUE4QixFQUFFO1lBQzFDLE9BQU8sSUFBSSxnQkFBZ0IsQ0FBQyxjQUFjLEVBQUUsT0FBTyxDQUFDLENBQUM7UUFDdkQsQ0FBQztRQUVELHNEQUFzRDtRQUNoRCxjQUFjLENBQUMsU0FBbUMsRUFBRTs7Z0JBQ3hELE9BQU8sSUFBSSxDQUFDLGFBQWEsQ0FBQyxvQkFBb0IsQ0FBQyxJQUFJLENBQUMsTUFBTSxDQUFDLENBQUMsRUFBRSxDQUFDO1lBQ2pFLENBQUM7U0FBQTtRQUVELGtEQUFrRDtRQUM1QyxlQUFlOztnQkFDbkIsTUFBTSxPQUFPLEdBQUcsTUFBTSxJQUFJLENBQUMsY0FBYyxFQUFFLENBQUM7Z0JBQzVDLEtBQUssSUFBSSxDQUFDLEdBQUcsQ0FBQyxFQUFFLENBQUMsR0FBRyxPQUFPLENBQUMsTUFBTSxFQUFFLENBQUMsRUFBRSxFQUFFO29CQUN2QyxJQUFJLE1BQU0sT0FBTyxDQUFDLENBQUMsQ0FBQyxDQUFDLFFBQVEsRUFBRSxFQUFFO3dCQUMvQixPQUFPLE9BQU8sQ0FBQyxDQUFDLENBQUMsQ0FBQztxQkFDbkI7aUJBQ0Y7Z0JBQ0QsT0FBTyxJQUFJLENBQUM7WUFDZCxDQUFDO1NBQUE7O0lBekJNLDJCQUFZLEdBQUcsV0FBVyxDQUFDO0lBMEJwQyxxQkFBQztLQUFBO1NBM0JZLGNBQWMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtDb21wb25lbnRIYXJuZXNzLCBIYXJuZXNzUHJlZGljYXRlfSBmcm9tICdAYW5ndWxhci9jZGsvdGVzdGluZyc7XG5pbXBvcnQge1NvcnRIYXJuZXNzRmlsdGVycywgU29ydEhlYWRlckhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL3NvcnQtaGFybmVzcy1maWx0ZXJzJztcbmltcG9ydCB7TWF0U29ydEhlYWRlckhhcm5lc3N9IGZyb20gJy4vc29ydC1oZWFkZXItaGFybmVzcyc7XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgYG1hdC1zb3J0YCBpbiB0ZXN0cy4gKi9cbmV4cG9ydCBjbGFzcyBNYXRTb3J0SGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJy5tYXQtc29ydCc7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYG1hdC1zb3J0YCB3aXRoIHNwZWNpZmljIGF0dHJpYnV0ZXMuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIG5hcnJvd2luZyB0aGUgc2VhcmNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IFNvcnRIYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXRTb3J0SGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXRTb3J0SGFybmVzcywgb3B0aW9ucyk7XG4gIH1cblxuICAvKiogR2V0cyBhbGwgb2YgdGhlIHNvcnQgaGVhZGVycyBpbiB0aGUgYG1hdC1zb3J0YC4gKi9cbiAgYXN5bmMgZ2V0U29ydEhlYWRlcnMoZmlsdGVyOiBTb3J0SGVhZGVySGFybmVzc0ZpbHRlcnMgPSB7fSk6IFByb21pc2U8TWF0U29ydEhlYWRlckhhcm5lc3NbXT4ge1xuICAgIHJldHVybiB0aGlzLmxvY2F0b3JGb3JBbGwoTWF0U29ydEhlYWRlckhhcm5lc3Mud2l0aChmaWx0ZXIpKSgpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHNlbGVjdGVkIGhlYWRlciBpbiB0aGUgYG1hdC1zb3J0YC4gKi9cbiAgYXN5bmMgZ2V0QWN0aXZlSGVhZGVyKCk6IFByb21pc2U8TWF0U29ydEhlYWRlckhhcm5lc3N8bnVsbD4ge1xuICAgIGNvbnN0IGhlYWRlcnMgPSBhd2FpdCB0aGlzLmdldFNvcnRIZWFkZXJzKCk7XG4gICAgZm9yIChsZXQgaSA9IDA7IGkgPCBoZWFkZXJzLmxlbmd0aDsgaSsrKSB7XG4gICAgICBpZiAoYXdhaXQgaGVhZGVyc1tpXS5pc0FjdGl2ZSgpKSB7XG4gICAgICAgIHJldHVybiBoZWFkZXJzW2ldO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gbnVsbDtcbiAgfVxufVxuIl19