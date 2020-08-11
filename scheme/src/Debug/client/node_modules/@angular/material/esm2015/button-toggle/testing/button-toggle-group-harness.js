/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MatButtonToggleHarness } from './button-toggle-harness';
/** Harness for interacting with a standard mat-button-toggle in tests. */
let MatButtonToggleGroupHarness = /** @class */ (() => {
    class MatButtonToggleGroupHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatButtonToggleGroupHarness`
         * that meets certain criteria.
         * @param options Options for filtering which button toggle instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatButtonToggleGroupHarness, options);
        }
        /**
         * Gets the button toggles that are inside the group.
         * @param filter Optionally filters which toggles are included.
         */
        getToggles(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatButtonToggleHarness.with(filter))();
            });
        }
        /** Gets whether the button toggle group is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield (yield this.host()).getAttribute('aria-disabled')) === 'true';
            });
        }
        /** Gets whether the button toggle group is laid out vertically. */
        isVertical() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-button-toggle-vertical');
            });
        }
        /** Gets the appearance that the group is using. */
        getAppearance() {
            return __awaiter(this, void 0, void 0, function* () {
                const host = yield this.host();
                const className = 'mat-button-toggle-group-appearance-standard';
                return (yield host.hasClass(className)) ? 'standard' : 'legacy';
            });
        }
    }
    /** The selector for the host element of a `MatButton` instance. */
    MatButtonToggleGroupHarness.hostSelector = 'mat-button-toggle-group';
    return MatButtonToggleGroupHarness;
})();
export { MatButtonToggleGroupHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYnV0dG9uLXRvZ2dsZS1ncm91cC1oYXJuZXNzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL2J1dHRvbi10b2dnbGUvdGVzdGluZy9idXR0b24tdG9nZ2xlLWdyb3VwLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBSXhFLE9BQU8sRUFBQyxzQkFBc0IsRUFBQyxNQUFNLHlCQUF5QixDQUFDO0FBRy9ELDBFQUEwRTtBQUMxRTtJQUFBLE1BQWEsMkJBQTRCLFNBQVEsZ0JBQWdCO1FBSS9EOzs7OztXQUtHO1FBQ0gsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUEyQyxFQUFFO1lBRXZELE9BQU8sSUFBSSxnQkFBZ0IsQ0FBQywyQkFBMkIsRUFBRSxPQUFPLENBQUMsQ0FBQztRQUNwRSxDQUFDO1FBRUQ7OztXQUdHO1FBQ0csVUFBVSxDQUFDLFNBQXFDLEVBQUU7O2dCQUN0RCxPQUFPLElBQUksQ0FBQyxhQUFhLENBQUMsc0JBQXNCLENBQUMsSUFBSSxDQUFDLE1BQU0sQ0FBQyxDQUFDLEVBQUUsQ0FBQztZQUNuRSxDQUFDO1NBQUE7UUFFRCx3REFBd0Q7UUFDbEQsVUFBVTs7Z0JBQ2QsT0FBTyxDQUFBLE1BQU0sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFlBQVksQ0FBQyxlQUFlLENBQUMsTUFBSyxNQUFNLENBQUM7WUFDNUUsQ0FBQztTQUFBO1FBRUQsbUVBQW1FO1FBQzdELFVBQVU7O2dCQUNkLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFFBQVEsQ0FBQyw0QkFBNEIsQ0FBQyxDQUFDO1lBQ3BFLENBQUM7U0FBQTtRQUVELG1EQUFtRDtRQUM3QyxhQUFhOztnQkFDakIsTUFBTSxJQUFJLEdBQUcsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUM7Z0JBQy9CLE1BQU0sU0FBUyxHQUFHLDZDQUE2QyxDQUFDO2dCQUNoRSxPQUFPLENBQUEsTUFBTSxJQUFJLENBQUMsUUFBUSxDQUFDLFNBQVMsQ0FBQyxFQUFDLENBQUMsQ0FBQyxVQUFVLENBQUMsQ0FBQyxDQUFDLFFBQVEsQ0FBQztZQUNoRSxDQUFDO1NBQUE7O0lBckNELG1FQUFtRTtJQUM1RCx3Q0FBWSxHQUFHLHlCQUF5QixDQUFDO0lBcUNsRCxrQ0FBQztLQUFBO1NBdkNZLDJCQUEyQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0NvbXBvbmVudEhhcm5lc3MsIEhhcm5lc3NQcmVkaWNhdGV9IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7TWF0QnV0dG9uVG9nZ2xlQXBwZWFyYW5jZX0gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvYnV0dG9uLXRvZ2dsZSc7XG5pbXBvcnQge0J1dHRvblRvZ2dsZUdyb3VwSGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vYnV0dG9uLXRvZ2dsZS1ncm91cC1oYXJuZXNzLWZpbHRlcnMnO1xuaW1wb3J0IHtCdXR0b25Ub2dnbGVIYXJuZXNzRmlsdGVyc30gZnJvbSAnLi9idXR0b24tdG9nZ2xlLWhhcm5lc3MtZmlsdGVycyc7XG5pbXBvcnQge01hdEJ1dHRvblRvZ2dsZUhhcm5lc3N9IGZyb20gJy4vYnV0dG9uLXRvZ2dsZS1oYXJuZXNzJztcblxuXG4vKiogSGFybmVzcyBmb3IgaW50ZXJhY3Rpbmcgd2l0aCBhIHN0YW5kYXJkIG1hdC1idXR0b24tdG9nZ2xlIGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdEJ1dHRvblRvZ2dsZUdyb3VwSGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdEJ1dHRvbmAgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnbWF0LWJ1dHRvbi10b2dnbGUtZ3JvdXAnO1xuXG4gIC8qKlxuICAgKiBHZXRzIGEgYEhhcm5lc3NQcmVkaWNhdGVgIHRoYXQgY2FuIGJlIHVzZWQgdG8gc2VhcmNoIGZvciBhIGBNYXRCdXR0b25Ub2dnbGVHcm91cEhhcm5lc3NgXG4gICAqIHRoYXQgbWVldHMgY2VydGFpbiBjcml0ZXJpYS5cbiAgICogQHBhcmFtIG9wdGlvbnMgT3B0aW9ucyBmb3IgZmlsdGVyaW5nIHdoaWNoIGJ1dHRvbiB0b2dnbGUgaW5zdGFuY2VzIGFyZSBjb25zaWRlcmVkIGEgbWF0Y2guXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogQnV0dG9uVG9nZ2xlR3JvdXBIYXJuZXNzRmlsdGVycyA9IHt9KTpcbiAgICBIYXJuZXNzUHJlZGljYXRlPE1hdEJ1dHRvblRvZ2dsZUdyb3VwSGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXRCdXR0b25Ub2dnbGVHcm91cEhhcm5lc3MsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgdGhlIGJ1dHRvbiB0b2dnbGVzIHRoYXQgYXJlIGluc2lkZSB0aGUgZ3JvdXAuXG4gICAqIEBwYXJhbSBmaWx0ZXIgT3B0aW9uYWxseSBmaWx0ZXJzIHdoaWNoIHRvZ2dsZXMgYXJlIGluY2x1ZGVkLlxuICAgKi9cbiAgYXN5bmMgZ2V0VG9nZ2xlcyhmaWx0ZXI6IEJ1dHRvblRvZ2dsZUhhcm5lc3NGaWx0ZXJzID0ge30pOiBQcm9taXNlPE1hdEJ1dHRvblRvZ2dsZUhhcm5lc3NbXT4ge1xuICAgIHJldHVybiB0aGlzLmxvY2F0b3JGb3JBbGwoTWF0QnV0dG9uVG9nZ2xlSGFybmVzcy53aXRoKGZpbHRlcikpKCk7XG4gIH1cblxuICAvKiogR2V0cyB3aGV0aGVyIHRoZSBidXR0b24gdG9nZ2xlIGdyb3VwIGlzIGRpc2FibGVkLiAqL1xuICBhc3luYyBpc0Rpc2FibGVkKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIHJldHVybiBhd2FpdCAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldEF0dHJpYnV0ZSgnYXJpYS1kaXNhYmxlZCcpID09PSAndHJ1ZSc7XG4gIH1cblxuICAvKiogR2V0cyB3aGV0aGVyIHRoZSBidXR0b24gdG9nZ2xlIGdyb3VwIGlzIGxhaWQgb3V0IHZlcnRpY2FsbHkuICovXG4gIGFzeW5jIGlzVmVydGljYWwoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuaGFzQ2xhc3MoJ21hdC1idXR0b24tdG9nZ2xlLXZlcnRpY2FsJyk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgYXBwZWFyYW5jZSB0aGF0IHRoZSBncm91cCBpcyB1c2luZy4gKi9cbiAgYXN5bmMgZ2V0QXBwZWFyYW5jZSgpOiBQcm9taXNlPE1hdEJ1dHRvblRvZ2dsZUFwcGVhcmFuY2U+IHtcbiAgICBjb25zdCBob3N0ID0gYXdhaXQgdGhpcy5ob3N0KCk7XG4gICAgY29uc3QgY2xhc3NOYW1lID0gJ21hdC1idXR0b24tdG9nZ2xlLWdyb3VwLWFwcGVhcmFuY2Utc3RhbmRhcmQnO1xuICAgIHJldHVybiBhd2FpdCBob3N0Lmhhc0NsYXNzKGNsYXNzTmFtZSkgPyAnc3RhbmRhcmQnIDogJ2xlZ2FjeSc7XG4gIH1cbn1cbiJdfQ==