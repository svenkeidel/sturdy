/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
/** Harness for interacting with a standard Angular Material sort header in tests. */
let MatSortHeaderHarness = /** @class */ (() => {
    class MatSortHeaderHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._button = this.locatorFor('.mat-sort-header-button');
        }
        /**
         * Gets a `HarnessPredicate` that can be used to
         * search for a sort header with specific attributes.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatSortHeaderHarness, options)
                .addOption('label', options.label, (harness, label) => HarnessPredicate.stringMatches(harness.getLabel(), label))
                .addOption('sortDirection', options.sortDirection, (harness, sortDirection) => {
                return HarnessPredicate.stringMatches(harness.getSortDirection(), sortDirection);
            });
        }
        /** Gets the label of the sort header. */
        getLabel() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._button()).text();
            });
        }
        /** Gets the sorting direction of the header. */
        getSortDirection() {
            return __awaiter(this, void 0, void 0, function* () {
                const host = yield this.host();
                const ariaSort = yield host.getAttribute('aria-sort');
                if (ariaSort === 'ascending') {
                    return 'asc';
                }
                else if (ariaSort === 'descending') {
                    return 'desc';
                }
                return '';
            });
        }
        /** Gets the aria-label of the sort header. */
        getAriaLabel() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._button()).getAttribute('aria-label');
            });
        }
        /** Gets whether the sort header is currently being sorted by. */
        isActive() {
            return __awaiter(this, void 0, void 0, function* () {
                return !!(yield this.getSortDirection());
            });
        }
        /** Whether the sort header is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                const button = yield this._button();
                return (yield button.getAttribute('disabled')) != null;
            });
        }
        /** Clicks the header to change its sorting direction. Only works if the header is enabled. */
        click() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).click();
            });
        }
    }
    MatSortHeaderHarness.hostSelector = '.mat-sort-header';
    return MatSortHeaderHarness;
})();
export { MatSortHeaderHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic29ydC1oZWFkZXItaGFybmVzcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9zb3J0L3Rlc3Rpbmcvc29ydC1oZWFkZXItaGFybmVzcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7O0FBRUgsT0FBTyxFQUFDLGdCQUFnQixFQUFFLGdCQUFnQixFQUFDLE1BQU0sc0JBQXNCLENBQUM7QUFJeEUscUZBQXFGO0FBQ3JGO0lBQUEsTUFBYSxvQkFBcUIsU0FBUSxnQkFBZ0I7UUFBMUQ7O1lBRVUsWUFBTyxHQUFHLElBQUksQ0FBQyxVQUFVLENBQUMseUJBQXlCLENBQUMsQ0FBQztRQXNEL0QsQ0FBQztRQXBEQzs7O1dBR0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQW9DLEVBQUU7WUFDaEQsT0FBTyxJQUFJLGdCQUFnQixDQUFDLG9CQUFvQixFQUFFLE9BQU8sQ0FBQztpQkFDckQsU0FBUyxDQUFDLE9BQU8sRUFBRSxPQUFPLENBQUMsS0FBSyxFQUM3QixDQUFDLE9BQU8sRUFBRSxLQUFLLEVBQUUsRUFBRSxDQUFDLGdCQUFnQixDQUFDLGFBQWEsQ0FBQyxPQUFPLENBQUMsUUFBUSxFQUFFLEVBQUUsS0FBSyxDQUFDLENBQUM7aUJBQ2pGLFNBQVMsQ0FBQyxlQUFlLEVBQUUsT0FBTyxDQUFDLGFBQWEsRUFBRSxDQUFDLE9BQU8sRUFBRSxhQUFhLEVBQUUsRUFBRTtnQkFDNUUsT0FBTyxnQkFBZ0IsQ0FBQyxhQUFhLENBQUMsT0FBTyxDQUFDLGdCQUFnQixFQUFFLEVBQUUsYUFBYSxDQUFDLENBQUM7WUFDbkYsQ0FBQyxDQUFDLENBQUM7UUFDVCxDQUFDO1FBRUQseUNBQXlDO1FBQ25DLFFBQVE7O2dCQUNaLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxPQUFPLEVBQUUsQ0FBQyxDQUFDLElBQUksRUFBRSxDQUFDO1lBQ3ZDLENBQUM7U0FBQTtRQUVELGdEQUFnRDtRQUMxQyxnQkFBZ0I7O2dCQUNwQixNQUFNLElBQUksR0FBRyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQztnQkFDL0IsTUFBTSxRQUFRLEdBQUcsTUFBTSxJQUFJLENBQUMsWUFBWSxDQUFDLFdBQVcsQ0FBQyxDQUFDO2dCQUV0RCxJQUFJLFFBQVEsS0FBSyxXQUFXLEVBQUU7b0JBQzVCLE9BQU8sS0FBSyxDQUFDO2lCQUNkO3FCQUFNLElBQUksUUFBUSxLQUFLLFlBQVksRUFBRTtvQkFDcEMsT0FBTyxNQUFNLENBQUM7aUJBQ2Y7Z0JBRUQsT0FBTyxFQUFFLENBQUM7WUFDWixDQUFDO1NBQUE7UUFFRCw4Q0FBOEM7UUFDeEMsWUFBWTs7Z0JBQ2hCLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxPQUFPLEVBQUUsQ0FBQyxDQUFDLFlBQVksQ0FBQyxZQUFZLENBQUMsQ0FBQztZQUMzRCxDQUFDO1NBQUE7UUFFRCxpRUFBaUU7UUFDM0QsUUFBUTs7Z0JBQ1osT0FBTyxDQUFDLENBQUMsQ0FBQyxNQUFNLElBQUksQ0FBQyxnQkFBZ0IsRUFBRSxDQUFDLENBQUM7WUFDM0MsQ0FBQztTQUFBO1FBRUQsMkNBQTJDO1FBQ3JDLFVBQVU7O2dCQUNkLE1BQU0sTUFBTSxHQUFHLE1BQU0sSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDO2dCQUNwQyxPQUFPLENBQUMsTUFBTSxNQUFNLENBQUMsWUFBWSxDQUFDLFVBQVUsQ0FBQyxDQUFDLElBQUksSUFBSSxDQUFDO1lBQ3pELENBQUM7U0FBQTtRQUVELDhGQUE4RjtRQUN4RixLQUFLOztnQkFDVCxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxLQUFLLEVBQUUsQ0FBQztZQUNyQyxDQUFDO1NBQUE7O0lBdERNLGlDQUFZLEdBQUcsa0JBQWtCLENBQUM7SUF1RDNDLDJCQUFDO0tBQUE7U0F4RFksb0JBQW9CIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Q29tcG9uZW50SGFybmVzcywgSGFybmVzc1ByZWRpY2F0ZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3Rlc3RpbmcnO1xuaW1wb3J0IHtTb3J0RGlyZWN0aW9ufSBmcm9tICdAYW5ndWxhci9tYXRlcmlhbC9zb3J0JztcbmltcG9ydCB7U29ydEhlYWRlckhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL3NvcnQtaGFybmVzcy1maWx0ZXJzJztcblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBBbmd1bGFyIE1hdGVyaWFsIHNvcnQgaGVhZGVyIGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdFNvcnRIZWFkZXJIYXJuZXNzIGV4dGVuZHMgQ29tcG9uZW50SGFybmVzcyB7XG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1zb3J0LWhlYWRlcic7XG4gIHByaXZhdGUgX2J1dHRvbiA9IHRoaXMubG9jYXRvckZvcignLm1hdC1zb3J0LWhlYWRlci1idXR0b24nKTtcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvXG4gICAqIHNlYXJjaCBmb3IgYSBzb3J0IGhlYWRlciB3aXRoIHNwZWNpZmljIGF0dHJpYnV0ZXMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBTb3J0SGVhZGVySGFybmVzc0ZpbHRlcnMgPSB7fSk6IEhhcm5lc3NQcmVkaWNhdGU8TWF0U29ydEhlYWRlckhhcm5lc3M+IHtcbiAgICByZXR1cm4gbmV3IEhhcm5lc3NQcmVkaWNhdGUoTWF0U29ydEhlYWRlckhhcm5lc3MsIG9wdGlvbnMpXG4gICAgICAgIC5hZGRPcHRpb24oJ2xhYmVsJywgb3B0aW9ucy5sYWJlbCxcbiAgICAgICAgICAgIChoYXJuZXNzLCBsYWJlbCkgPT4gSGFybmVzc1ByZWRpY2F0ZS5zdHJpbmdNYXRjaGVzKGhhcm5lc3MuZ2V0TGFiZWwoKSwgbGFiZWwpKVxuICAgICAgICAuYWRkT3B0aW9uKCdzb3J0RGlyZWN0aW9uJywgb3B0aW9ucy5zb3J0RGlyZWN0aW9uLCAoaGFybmVzcywgc29ydERpcmVjdGlvbikgPT4ge1xuICAgICAgICAgIHJldHVybiBIYXJuZXNzUHJlZGljYXRlLnN0cmluZ01hdGNoZXMoaGFybmVzcy5nZXRTb3J0RGlyZWN0aW9uKCksIHNvcnREaXJlY3Rpb24pO1xuICAgICAgICB9KTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSBsYWJlbCBvZiB0aGUgc29ydCBoZWFkZXIuICovXG4gIGFzeW5jIGdldExhYmVsKCk6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9idXR0b24oKSkudGV4dCgpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHNvcnRpbmcgZGlyZWN0aW9uIG9mIHRoZSBoZWFkZXIuICovXG4gIGFzeW5jIGdldFNvcnREaXJlY3Rpb24oKTogUHJvbWlzZTxTb3J0RGlyZWN0aW9uPiB7XG4gICAgY29uc3QgaG9zdCA9IGF3YWl0IHRoaXMuaG9zdCgpO1xuICAgIGNvbnN0IGFyaWFTb3J0ID0gYXdhaXQgaG9zdC5nZXRBdHRyaWJ1dGUoJ2FyaWEtc29ydCcpO1xuXG4gICAgaWYgKGFyaWFTb3J0ID09PSAnYXNjZW5kaW5nJykge1xuICAgICAgcmV0dXJuICdhc2MnO1xuICAgIH0gZWxzZSBpZiAoYXJpYVNvcnQgPT09ICdkZXNjZW5kaW5nJykge1xuICAgICAgcmV0dXJuICdkZXNjJztcbiAgICB9XG5cbiAgICByZXR1cm4gJyc7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgYXJpYS1sYWJlbCBvZiB0aGUgc29ydCBoZWFkZXIuICovXG4gIGFzeW5jIGdldEFyaWFMYWJlbCgpOiBQcm9taXNlPHN0cmluZ3xudWxsPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9idXR0b24oKSkuZ2V0QXR0cmlidXRlKCdhcmlhLWxhYmVsJyk7XG4gIH1cblxuICAvKiogR2V0cyB3aGV0aGVyIHRoZSBzb3J0IGhlYWRlciBpcyBjdXJyZW50bHkgYmVpbmcgc29ydGVkIGJ5LiAqL1xuICBhc3luYyBpc0FjdGl2ZSgpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICByZXR1cm4gISEoYXdhaXQgdGhpcy5nZXRTb3J0RGlyZWN0aW9uKCkpO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIHNvcnQgaGVhZGVyIGlzIGRpc2FibGVkLiAqL1xuICBhc3luYyBpc0Rpc2FibGVkKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIGNvbnN0IGJ1dHRvbiA9IGF3YWl0IHRoaXMuX2J1dHRvbigpO1xuICAgIHJldHVybiAoYXdhaXQgYnV0dG9uLmdldEF0dHJpYnV0ZSgnZGlzYWJsZWQnKSkgIT0gbnVsbDtcbiAgfVxuXG4gIC8qKiBDbGlja3MgdGhlIGhlYWRlciB0byBjaGFuZ2UgaXRzIHNvcnRpbmcgZGlyZWN0aW9uLiBPbmx5IHdvcmtzIGlmIHRoZSBoZWFkZXIgaXMgZW5hYmxlZC4gKi9cbiAgYXN5bmMgY2xpY2soKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuY2xpY2soKTtcbiAgfVxufVxuIl19