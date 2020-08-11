/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { coerceNumberProperty } from '@angular/cdk/coercion';
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
/** Harness for interacting with a standard mat-progress-spinner in tests. */
let MatProgressSpinnerHarness = /** @class */ (() => {
    class MatProgressSpinnerHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatProgressSpinnerHarness` that
         * meets certain criteria.
         * @param options Options for filtering which progress spinner instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatProgressSpinnerHarness, options);
        }
        /** Gets the progress spinner's value. */
        getValue() {
            return __awaiter(this, void 0, void 0, function* () {
                const host = yield this.host();
                const ariaValue = yield host.getAttribute('aria-valuenow');
                return ariaValue ? coerceNumberProperty(ariaValue) : null;
            });
        }
        /** Gets the progress spinner's mode. */
        getMode() {
            return __awaiter(this, void 0, void 0, function* () {
                const modeAttr = (yield this.host()).getAttribute('mode');
                return yield modeAttr;
            });
        }
    }
    /** The selector for the host element of a `MatProgressSpinner` instance. */
    MatProgressSpinnerHarness.hostSelector = 'mat-progress-spinner,mat-spinner';
    return MatProgressSpinnerHarness;
})();
export { MatProgressSpinnerHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicHJvZ3Jlc3Mtc3Bpbm5lci1oYXJuZXNzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL3Byb2dyZXNzLXNwaW5uZXIvdGVzdGluZy9wcm9ncmVzcy1zcGlubmVyLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxvQkFBb0IsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQzNELE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBSXhFLDZFQUE2RTtBQUM3RTtJQUFBLE1BQWEseUJBQTBCLFNBQVEsZ0JBQWdCO1FBSTdEOzs7OztXQUtHO1FBQ0gsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUF5QyxFQUFFO1lBRXJELE9BQU8sSUFBSSxnQkFBZ0IsQ0FBQyx5QkFBeUIsRUFBRSxPQUFPLENBQUMsQ0FBQztRQUNsRSxDQUFDO1FBRUQseUNBQXlDO1FBQ25DLFFBQVE7O2dCQUNaLE1BQU0sSUFBSSxHQUFHLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDO2dCQUMvQixNQUFNLFNBQVMsR0FBRyxNQUFNLElBQUksQ0FBQyxZQUFZLENBQUMsZUFBZSxDQUFDLENBQUM7Z0JBQzNELE9BQU8sU0FBUyxDQUFDLENBQUMsQ0FBQyxvQkFBb0IsQ0FBQyxTQUFTLENBQUMsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDO1lBQzVELENBQUM7U0FBQTtRQUVELHdDQUF3QztRQUNsQyxPQUFPOztnQkFDWCxNQUFNLFFBQVEsR0FBRyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsWUFBWSxDQUFDLE1BQU0sQ0FBQyxDQUFDO2dCQUMxRCxPQUFPLE1BQU0sUUFBK0IsQ0FBQztZQUMvQyxDQUFDO1NBQUE7O0lBekJELDRFQUE0RTtJQUNyRSxzQ0FBWSxHQUFHLGtDQUFrQyxDQUFDO0lBeUIzRCxnQ0FBQztLQUFBO1NBM0JZLHlCQUF5QiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge2NvZXJjZU51bWJlclByb3BlcnR5fSBmcm9tICdAYW5ndWxhci9jZGsvY29lcmNpb24nO1xuaW1wb3J0IHtDb21wb25lbnRIYXJuZXNzLCBIYXJuZXNzUHJlZGljYXRlfSBmcm9tICdAYW5ndWxhci9jZGsvdGVzdGluZyc7XG5pbXBvcnQge1Byb2dyZXNzU3Bpbm5lck1vZGV9IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL3Byb2dyZXNzLXNwaW5uZXInO1xuaW1wb3J0IHtQcm9ncmVzc1NwaW5uZXJIYXJuZXNzRmlsdGVyc30gZnJvbSAnLi9wcm9ncmVzcy1zcGlubmVyLWhhcm5lc3MtZmlsdGVycyc7XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgbWF0LXByb2dyZXNzLXNwaW5uZXIgaW4gdGVzdHMuICovXG5leHBvcnQgY2xhc3MgTWF0UHJvZ3Jlc3NTcGlubmVySGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdFByb2dyZXNzU3Bpbm5lcmAgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnbWF0LXByb2dyZXNzLXNwaW5uZXIsbWF0LXNwaW5uZXInO1xuXG4gIC8qKlxuICAgKiBHZXRzIGEgYEhhcm5lc3NQcmVkaWNhdGVgIHRoYXQgY2FuIGJlIHVzZWQgdG8gc2VhcmNoIGZvciBhIGBNYXRQcm9ncmVzc1NwaW5uZXJIYXJuZXNzYCB0aGF0XG4gICAqIG1lZXRzIGNlcnRhaW4gY3JpdGVyaWEuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIGZpbHRlcmluZyB3aGljaCBwcm9ncmVzcyBzcGlubmVyIGluc3RhbmNlcyBhcmUgY29uc2lkZXJlZCBhIG1hdGNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IFByb2dyZXNzU3Bpbm5lckhhcm5lc3NGaWx0ZXJzID0ge30pOlxuICAgICAgSGFybmVzc1ByZWRpY2F0ZTxNYXRQcm9ncmVzc1NwaW5uZXJIYXJuZXNzPiB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdFByb2dyZXNzU3Bpbm5lckhhcm5lc3MsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHByb2dyZXNzIHNwaW5uZXIncyB2YWx1ZS4gKi9cbiAgYXN5bmMgZ2V0VmFsdWUoKTogUHJvbWlzZTxudW1iZXJ8bnVsbD4ge1xuICAgIGNvbnN0IGhvc3QgPSBhd2FpdCB0aGlzLmhvc3QoKTtcbiAgICBjb25zdCBhcmlhVmFsdWUgPSBhd2FpdCBob3N0LmdldEF0dHJpYnV0ZSgnYXJpYS12YWx1ZW5vdycpO1xuICAgIHJldHVybiBhcmlhVmFsdWUgPyBjb2VyY2VOdW1iZXJQcm9wZXJ0eShhcmlhVmFsdWUpIDogbnVsbDtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSBwcm9ncmVzcyBzcGlubmVyJ3MgbW9kZS4gKi9cbiAgYXN5bmMgZ2V0TW9kZSgpOiBQcm9taXNlPFByb2dyZXNzU3Bpbm5lck1vZGU+IHtcbiAgICBjb25zdCBtb2RlQXR0ciA9IChhd2FpdCB0aGlzLmhvc3QoKSkuZ2V0QXR0cmlidXRlKCdtb2RlJyk7XG4gICAgcmV0dXJuIGF3YWl0IG1vZGVBdHRyIGFzIFByb2dyZXNzU3Bpbm5lck1vZGU7XG4gIH1cbn1cbiJdfQ==