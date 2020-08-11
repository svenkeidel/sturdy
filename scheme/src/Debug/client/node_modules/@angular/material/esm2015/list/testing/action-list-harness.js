/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { HarnessPredicate } from '@angular/cdk/testing';
import { MatListHarnessBase } from './list-harness-base';
import { getListItemPredicate, MatListItemHarnessBase } from './list-item-harness-base';
/** Harness for interacting with a standard mat-action-list in tests. */
let MatActionListHarness = /** @class */ (() => {
    class MatActionListHarness extends MatListHarnessBase {
        constructor() {
            super(...arguments);
            this._itemHarness = MatActionListItemHarness;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatActionListHarness` that meets
         * certain criteria.
         * @param options Options for filtering which action list instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatActionListHarness, options);
        }
    }
    /** The selector for the host element of a `MatActionList` instance. */
    MatActionListHarness.hostSelector = 'mat-action-list';
    return MatActionListHarness;
})();
export { MatActionListHarness };
/** Harness for interacting with an action list item. */
let MatActionListItemHarness = /** @class */ (() => {
    class MatActionListItemHarness extends MatListItemHarnessBase {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatActionListItemHarness` that
         * meets certain criteria.
         * @param options Options for filtering which action list item instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return getListItemPredicate(MatActionListItemHarness, options);
        }
        /** Clicks on the action list item. */
        click() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).click();
            });
        }
        /** Focuses the action list item. */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).focus();
            });
        }
        /** Blurs the action list item. */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).blur();
            });
        }
    }
    /** The selector for the host element of a `MatListItem` instance. */
    MatActionListItemHarness.hostSelector = ['mat-list-item', 'a[mat-list-item]', 'button[mat-list-item]']
        .map(selector => `${MatActionListHarness.hostSelector} ${selector}`)
        .join(',');
    return MatActionListItemHarness;
})();
export { MatActionListItemHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYWN0aW9uLWxpc3QtaGFybmVzcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9saXN0L3Rlc3RpbmcvYWN0aW9uLWxpc3QtaGFybmVzcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7O0FBRUgsT0FBTyxFQUFDLGdCQUFnQixFQUFDLE1BQU0sc0JBQXNCLENBQUM7QUFDdEQsT0FBTyxFQUFDLGtCQUFrQixFQUFDLE1BQU0scUJBQXFCLENBQUM7QUFFdkQsT0FBTyxFQUFDLG9CQUFvQixFQUFFLHNCQUFzQixFQUFDLE1BQU0sMEJBQTBCLENBQUM7QUFFdEYsd0VBQXdFO0FBQ3hFO0lBQUEsTUFBYSxvQkFBcUIsU0FBUSxrQkFDa0Q7UUFENUY7O1lBZUUsaUJBQVksR0FBRyx3QkFBd0IsQ0FBQztRQUMxQyxDQUFDO1FBWEM7Ozs7O1dBS0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQW9DLEVBQUU7WUFDaEQsT0FBTyxJQUFJLGdCQUFnQixDQUFDLG9CQUFvQixFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQzdELENBQUM7O0lBWEQsdUVBQXVFO0lBQ2hFLGlDQUFZLEdBQUcsaUJBQWlCLENBQUM7SUFhMUMsMkJBQUM7S0FBQTtTQWhCWSxvQkFBb0I7QUFrQmpDLHdEQUF3RDtBQUN4RDtJQUFBLE1BQWEsd0JBQXlCLFNBQVEsc0JBQXNCO1FBTWxFOzs7OztXQUtHO1FBQ0gsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUF3QyxFQUFFO1lBRXBELE9BQU8sb0JBQW9CLENBQUMsd0JBQXdCLEVBQUUsT0FBTyxDQUFDLENBQUM7UUFDakUsQ0FBQztRQUVELHNDQUFzQztRQUNoQyxLQUFLOztnQkFDVCxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxLQUFLLEVBQUUsQ0FBQztZQUNyQyxDQUFDO1NBQUE7UUFFRCxvQ0FBb0M7UUFDOUIsS0FBSzs7Z0JBQ1QsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsS0FBSyxFQUFFLENBQUM7WUFDckMsQ0FBQztTQUFBO1FBRUQsa0NBQWtDO1FBQzVCLElBQUk7O2dCQUNSLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLElBQUksRUFBRSxDQUFDO1lBQ3BDLENBQUM7U0FBQTs7SUE3QkQscUVBQXFFO0lBQzlELHFDQUFZLEdBQUcsQ0FBQyxlQUFlLEVBQUUsa0JBQWtCLEVBQUUsdUJBQXVCLENBQUM7U0FDL0UsR0FBRyxDQUFDLFFBQVEsQ0FBQyxFQUFFLENBQUMsR0FBRyxvQkFBb0IsQ0FBQyxZQUFZLElBQUksUUFBUSxFQUFFLENBQUM7U0FDbkUsSUFBSSxDQUFDLEdBQUcsQ0FBQyxDQUFDO0lBMkJqQiwrQkFBQztLQUFBO1NBL0JZLHdCQUF3QiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0hhcm5lc3NQcmVkaWNhdGV9IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7TWF0TGlzdEhhcm5lc3NCYXNlfSBmcm9tICcuL2xpc3QtaGFybmVzcy1iYXNlJztcbmltcG9ydCB7QWN0aW9uTGlzdEhhcm5lc3NGaWx0ZXJzLCBBY3Rpb25MaXN0SXRlbUhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL2xpc3QtaGFybmVzcy1maWx0ZXJzJztcbmltcG9ydCB7Z2V0TGlzdEl0ZW1QcmVkaWNhdGUsIE1hdExpc3RJdGVtSGFybmVzc0Jhc2V9IGZyb20gJy4vbGlzdC1pdGVtLWhhcm5lc3MtYmFzZSc7XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgbWF0LWFjdGlvbi1saXN0IGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdEFjdGlvbkxpc3RIYXJuZXNzIGV4dGVuZHMgTWF0TGlzdEhhcm5lc3NCYXNlPFxuICAgIHR5cGVvZiBNYXRBY3Rpb25MaXN0SXRlbUhhcm5lc3MsIE1hdEFjdGlvbkxpc3RJdGVtSGFybmVzcywgQWN0aW9uTGlzdEl0ZW1IYXJuZXNzRmlsdGVycz4ge1xuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdEFjdGlvbkxpc3RgIGluc3RhbmNlLiAqL1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJ21hdC1hY3Rpb24tbGlzdCc7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYE1hdEFjdGlvbkxpc3RIYXJuZXNzYCB0aGF0IG1lZXRzXG4gICAqIGNlcnRhaW4gY3JpdGVyaWEuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIGZpbHRlcmluZyB3aGljaCBhY3Rpb24gbGlzdCBpbnN0YW5jZXMgYXJlIGNvbnNpZGVyZWQgYSBtYXRjaC5cbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBBY3Rpb25MaXN0SGFybmVzc0ZpbHRlcnMgPSB7fSk6IEhhcm5lc3NQcmVkaWNhdGU8TWF0QWN0aW9uTGlzdEhhcm5lc3M+IHtcbiAgICByZXR1cm4gbmV3IEhhcm5lc3NQcmVkaWNhdGUoTWF0QWN0aW9uTGlzdEhhcm5lc3MsIG9wdGlvbnMpO1xuICB9XG5cbiAgX2l0ZW1IYXJuZXNzID0gTWF0QWN0aW9uTGlzdEl0ZW1IYXJuZXNzO1xufVxuXG4vKiogSGFybmVzcyBmb3IgaW50ZXJhY3Rpbmcgd2l0aCBhbiBhY3Rpb24gbGlzdCBpdGVtLiAqL1xuZXhwb3J0IGNsYXNzIE1hdEFjdGlvbkxpc3RJdGVtSGFybmVzcyBleHRlbmRzIE1hdExpc3RJdGVtSGFybmVzc0Jhc2Uge1xuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdExpc3RJdGVtYCBpbnN0YW5jZS4gKi9cbiAgc3RhdGljIGhvc3RTZWxlY3RvciA9IFsnbWF0LWxpc3QtaXRlbScsICdhW21hdC1saXN0LWl0ZW1dJywgJ2J1dHRvblttYXQtbGlzdC1pdGVtXSddXG4gICAgICAubWFwKHNlbGVjdG9yID0+IGAke01hdEFjdGlvbkxpc3RIYXJuZXNzLmhvc3RTZWxlY3Rvcn0gJHtzZWxlY3Rvcn1gKVxuICAgICAgLmpvaW4oJywnKTtcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3IgYSBgTWF0QWN0aW9uTGlzdEl0ZW1IYXJuZXNzYCB0aGF0XG4gICAqIG1lZXRzIGNlcnRhaW4gY3JpdGVyaWEuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIGZpbHRlcmluZyB3aGljaCBhY3Rpb24gbGlzdCBpdGVtIGluc3RhbmNlcyBhcmUgY29uc2lkZXJlZCBhIG1hdGNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IEFjdGlvbkxpc3RJdGVtSGFybmVzc0ZpbHRlcnMgPSB7fSk6XG4gICAgICBIYXJuZXNzUHJlZGljYXRlPE1hdEFjdGlvbkxpc3RJdGVtSGFybmVzcz4ge1xuICAgIHJldHVybiBnZXRMaXN0SXRlbVByZWRpY2F0ZShNYXRBY3Rpb25MaXN0SXRlbUhhcm5lc3MsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqIENsaWNrcyBvbiB0aGUgYWN0aW9uIGxpc3QgaXRlbS4gKi9cbiAgYXN5bmMgY2xpY2soKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuY2xpY2soKTtcbiAgfVxuXG4gIC8qKiBGb2N1c2VzIHRoZSBhY3Rpb24gbGlzdCBpdGVtLiAqL1xuICBhc3luYyBmb2N1cygpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5mb2N1cygpO1xuICB9XG5cbiAgLyoqIEJsdXJzIHRoZSBhY3Rpb24gbGlzdCBpdGVtLiAqL1xuICBhc3luYyBibHVyKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmJsdXIoKTtcbiAgfVxufVxuIl19