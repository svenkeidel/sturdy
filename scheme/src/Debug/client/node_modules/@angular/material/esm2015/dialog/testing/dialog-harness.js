/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate, TestKey } from '@angular/cdk/testing';
/** Harness for interacting with a standard `MatDialog` in tests. */
let MatDialogHarness = /** @class */ (() => {
    class MatDialogHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatDialogHarness` that meets
         * certain criteria.
         * @param options Options for filtering which dialog instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatDialogHarness, options);
        }
        /** Gets the id of the dialog. */
        getId() {
            return __awaiter(this, void 0, void 0, function* () {
                const id = yield (yield this.host()).getAttribute('id');
                // In case no id has been specified, the "id" property always returns
                // an empty string. To make this method more explicit, we return null.
                return id !== '' ? id : null;
            });
        }
        /** Gets the role of the dialog. */
        getRole() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('role');
            });
        }
        /** Gets the value of the dialog's "aria-label" attribute. */
        getAriaLabel() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('aria-label');
            });
        }
        /** Gets the value of the dialog's "aria-labelledby" attribute. */
        getAriaLabelledby() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('aria-labelledby');
            });
        }
        /** Gets the value of the dialog's "aria-describedby" attribute. */
        getAriaDescribedby() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('aria-describedby');
            });
        }
        /**
         * Closes the dialog by pressing escape.
         *
         * Note: this method does nothing if `disableClose` has been set to `true` for the dialog.
         */
        close() {
            return __awaiter(this, void 0, void 0, function* () {
                yield (yield this.host()).sendKeys(TestKey.ESCAPE);
            });
        }
    }
    // Developers can provide a custom component or template for the
    // dialog. The canonical dialog parent is the "MatDialogContainer".
    /** The selector for the host element of a `MatDialog` instance. */
    MatDialogHarness.hostSelector = '.mat-dialog-container';
    return MatDialogHarness;
})();
export { MatDialogHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGlhbG9nLWhhcm5lc3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvZGlhbG9nL3Rlc3RpbmcvZGlhbG9nLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBRSxPQUFPLEVBQUMsTUFBTSxzQkFBc0IsQ0FBQztBQUlqRixvRUFBb0U7QUFDcEU7SUFBQSxNQUFhLGdCQUFpQixTQUFRLGdCQUFnQjtRQU1wRDs7Ozs7V0FLRztRQUNILE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBZ0MsRUFBRTtZQUM1QyxPQUFPLElBQUksZ0JBQWdCLENBQUMsZ0JBQWdCLEVBQUUsT0FBTyxDQUFDLENBQUM7UUFDekQsQ0FBQztRQUVELGlDQUFpQztRQUMzQixLQUFLOztnQkFDVCxNQUFNLEVBQUUsR0FBRyxNQUFNLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxZQUFZLENBQUMsSUFBSSxDQUFDLENBQUM7Z0JBQ3hELHFFQUFxRTtnQkFDckUsc0VBQXNFO2dCQUN0RSxPQUFPLEVBQUUsS0FBSyxFQUFFLENBQUMsQ0FBQyxDQUFDLEVBQUUsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDO1lBQy9CLENBQUM7U0FBQTtRQUVELG1DQUFtQztRQUM3QixPQUFPOztnQkFDWCxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxZQUFZLENBQUMsTUFBTSxDQUE2QixDQUFDO1lBQzlFLENBQUM7U0FBQTtRQUVELDZEQUE2RDtRQUN2RCxZQUFZOztnQkFDaEIsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsWUFBWSxDQUFDLFlBQVksQ0FBQyxDQUFDO1lBQ3hELENBQUM7U0FBQTtRQUVELGtFQUFrRTtRQUM1RCxpQkFBaUI7O2dCQUNyQixPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxZQUFZLENBQUMsaUJBQWlCLENBQUMsQ0FBQztZQUM3RCxDQUFDO1NBQUE7UUFFRCxtRUFBbUU7UUFDN0Qsa0JBQWtCOztnQkFDdEIsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsWUFBWSxDQUFDLGtCQUFrQixDQUFDLENBQUM7WUFDOUQsQ0FBQztTQUFBO1FBRUQ7Ozs7V0FJRztRQUNHLEtBQUs7O2dCQUNULE1BQU0sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQUMsTUFBTSxDQUFDLENBQUM7WUFDckQsQ0FBQztTQUFBOztJQWxERCxnRUFBZ0U7SUFDaEUsbUVBQW1FO0lBQ25FLG1FQUFtRTtJQUM1RCw2QkFBWSxHQUFHLHVCQUF1QixDQUFDO0lBZ0RoRCx1QkFBQztLQUFBO1NBcERZLGdCQUFnQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0NvbXBvbmVudEhhcm5lc3MsIEhhcm5lc3NQcmVkaWNhdGUsIFRlc3RLZXl9IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7RGlhbG9nUm9sZX0gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvZGlhbG9nJztcbmltcG9ydCB7RGlhbG9nSGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vZGlhbG9nLWhhcm5lc3MtZmlsdGVycyc7XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgYE1hdERpYWxvZ2AgaW4gdGVzdHMuICovXG5leHBvcnQgY2xhc3MgTWF0RGlhbG9nSGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICAvLyBEZXZlbG9wZXJzIGNhbiBwcm92aWRlIGEgY3VzdG9tIGNvbXBvbmVudCBvciB0ZW1wbGF0ZSBmb3IgdGhlXG4gIC8vIGRpYWxvZy4gVGhlIGNhbm9uaWNhbCBkaWFsb2cgcGFyZW50IGlzIHRoZSBcIk1hdERpYWxvZ0NvbnRhaW5lclwiLlxuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdERpYWxvZ2AgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1kaWFsb2ctY29udGFpbmVyJztcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3IgYSBgTWF0RGlhbG9nSGFybmVzc2AgdGhhdCBtZWV0c1xuICAgKiBjZXJ0YWluIGNyaXRlcmlhLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBmaWx0ZXJpbmcgd2hpY2ggZGlhbG9nIGluc3RhbmNlcyBhcmUgY29uc2lkZXJlZCBhIG1hdGNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IERpYWxvZ0hhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdERpYWxvZ0hhcm5lc3M+IHtcbiAgICByZXR1cm4gbmV3IEhhcm5lc3NQcmVkaWNhdGUoTWF0RGlhbG9nSGFybmVzcywgb3B0aW9ucyk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgaWQgb2YgdGhlIGRpYWxvZy4gKi9cbiAgYXN5bmMgZ2V0SWQoKTogUHJvbWlzZTxzdHJpbmd8bnVsbD4ge1xuICAgIGNvbnN0IGlkID0gYXdhaXQgKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ2lkJyk7XG4gICAgLy8gSW4gY2FzZSBubyBpZCBoYXMgYmVlbiBzcGVjaWZpZWQsIHRoZSBcImlkXCIgcHJvcGVydHkgYWx3YXlzIHJldHVybnNcbiAgICAvLyBhbiBlbXB0eSBzdHJpbmcuIFRvIG1ha2UgdGhpcyBtZXRob2QgbW9yZSBleHBsaWNpdCwgd2UgcmV0dXJuIG51bGwuXG4gICAgcmV0dXJuIGlkICE9PSAnJyA/IGlkIDogbnVsbDtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSByb2xlIG9mIHRoZSBkaWFsb2cuICovXG4gIGFzeW5jIGdldFJvbGUoKTogUHJvbWlzZTxEaWFsb2dSb2xlfG51bGw+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ3JvbGUnKSBhcyBQcm9taXNlPERpYWxvZ1JvbGV8bnVsbD47XG4gIH1cblxuICAvKiogR2V0cyB0aGUgdmFsdWUgb2YgdGhlIGRpYWxvZydzIFwiYXJpYS1sYWJlbFwiIGF0dHJpYnV0ZS4gKi9cbiAgYXN5bmMgZ2V0QXJpYUxhYmVsKCk6IFByb21pc2U8c3RyaW5nfG51bGw+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ2FyaWEtbGFiZWwnKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSB2YWx1ZSBvZiB0aGUgZGlhbG9nJ3MgXCJhcmlhLWxhYmVsbGVkYnlcIiBhdHRyaWJ1dGUuICovXG4gIGFzeW5jIGdldEFyaWFMYWJlbGxlZGJ5KCk6IFByb21pc2U8c3RyaW5nfG51bGw+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ2FyaWEtbGFiZWxsZWRieScpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHZhbHVlIG9mIHRoZSBkaWFsb2cncyBcImFyaWEtZGVzY3JpYmVkYnlcIiBhdHRyaWJ1dGUuICovXG4gIGFzeW5jIGdldEFyaWFEZXNjcmliZWRieSgpOiBQcm9taXNlPHN0cmluZ3xudWxsPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuZ2V0QXR0cmlidXRlKCdhcmlhLWRlc2NyaWJlZGJ5Jyk7XG4gIH1cblxuICAvKipcbiAgICogQ2xvc2VzIHRoZSBkaWFsb2cgYnkgcHJlc3NpbmcgZXNjYXBlLlxuICAgKlxuICAgKiBOb3RlOiB0aGlzIG1ldGhvZCBkb2VzIG5vdGhpbmcgaWYgYGRpc2FibGVDbG9zZWAgaGFzIGJlZW4gc2V0IHRvIGB0cnVlYCBmb3IgdGhlIGRpYWxvZy5cbiAgICovXG4gIGFzeW5jIGNsb3NlKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIGF3YWl0IChhd2FpdCB0aGlzLmhvc3QoKSkuc2VuZEtleXMoVGVzdEtleS5FU0NBUEUpO1xuICB9XG59XG4iXX0=