/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { HarnessPredicate } from '@angular/cdk/testing';
import { MatListHarnessBase } from './list-harness-base';
import { getListItemPredicate, MatListItemHarnessBase } from './list-item-harness-base';
/** Harness for interacting with a standard mat-list in tests. */
let MatListHarness = /** @class */ (() => {
    class MatListHarness extends MatListHarnessBase {
        constructor() {
            super(...arguments);
            this._itemHarness = MatListItemHarness;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatListHarness` that meets certain
         * criteria.
         * @param options Options for filtering which list instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatListHarness, options);
        }
    }
    /** The selector for the host element of a `MatList` instance. */
    MatListHarness.hostSelector = 'mat-list';
    return MatListHarness;
})();
export { MatListHarness };
/** Harness for interacting with a list item. */
let MatListItemHarness = /** @class */ (() => {
    class MatListItemHarness extends MatListItemHarnessBase {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatListItemHarness` that meets
         * certain criteria.
         * @param options Options for filtering which list item instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return getListItemPredicate(MatListItemHarness, options);
        }
    }
    /** The selector for the host element of a `MatListItem` instance. */
    MatListItemHarness.hostSelector = ['mat-list-item', 'a[mat-list-item]', 'button[mat-list-item]']
        .map(selector => `${MatListHarness.hostSelector} ${selector}`)
        .join(',');
    return MatListItemHarness;
})();
export { MatListItemHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibGlzdC1oYXJuZXNzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL2xpc3QvdGVzdGluZy9saXN0LWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLGdCQUFnQixFQUFDLE1BQU0sc0JBQXNCLENBQUM7QUFDdEQsT0FBTyxFQUFDLGtCQUFrQixFQUFDLE1BQU0scUJBQXFCLENBQUM7QUFFdkQsT0FBTyxFQUFDLG9CQUFvQixFQUFFLHNCQUFzQixFQUFDLE1BQU0sMEJBQTBCLENBQUM7QUFFdEYsaUVBQWlFO0FBQ2pFO0lBQUEsTUFBYSxjQUFlLFNBQ3hCLGtCQUF5RjtRQUQ3Rjs7WUFlRSxpQkFBWSxHQUFHLGtCQUFrQixDQUFDO1FBQ3BDLENBQUM7UUFYQzs7Ozs7V0FLRztRQUNILE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBOEIsRUFBRTtZQUMxQyxPQUFPLElBQUksZ0JBQWdCLENBQUMsY0FBYyxFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQ3ZELENBQUM7O0lBWEQsaUVBQWlFO0lBQzFELDJCQUFZLEdBQUcsVUFBVSxDQUFDO0lBYW5DLHFCQUFDO0tBQUE7U0FoQlksY0FBYztBQWtCM0IsZ0RBQWdEO0FBQ2hEO0lBQUEsTUFBYSxrQkFBbUIsU0FBUSxzQkFBc0I7UUFNNUQ7Ozs7O1dBS0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQWtDLEVBQUU7WUFDOUMsT0FBTyxvQkFBb0IsQ0FBQyxrQkFBa0IsRUFBRSxPQUFPLENBQUMsQ0FBQztRQUMzRCxDQUFDOztJQWJELHFFQUFxRTtJQUM5RCwrQkFBWSxHQUFHLENBQUMsZUFBZSxFQUFFLGtCQUFrQixFQUFFLHVCQUF1QixDQUFDO1NBQy9FLEdBQUcsQ0FBQyxRQUFRLENBQUMsRUFBRSxDQUFDLEdBQUcsY0FBYyxDQUFDLFlBQVksSUFBSSxRQUFRLEVBQUUsQ0FBQztTQUM3RCxJQUFJLENBQUMsR0FBRyxDQUFDLENBQUM7SUFXakIseUJBQUM7S0FBQTtTQWZZLGtCQUFrQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0hhcm5lc3NQcmVkaWNhdGV9IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7TWF0TGlzdEhhcm5lc3NCYXNlfSBmcm9tICcuL2xpc3QtaGFybmVzcy1iYXNlJztcbmltcG9ydCB7TGlzdEhhcm5lc3NGaWx0ZXJzLCBMaXN0SXRlbUhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL2xpc3QtaGFybmVzcy1maWx0ZXJzJztcbmltcG9ydCB7Z2V0TGlzdEl0ZW1QcmVkaWNhdGUsIE1hdExpc3RJdGVtSGFybmVzc0Jhc2V9IGZyb20gJy4vbGlzdC1pdGVtLWhhcm5lc3MtYmFzZSc7XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgbWF0LWxpc3QgaW4gdGVzdHMuICovXG5leHBvcnQgY2xhc3MgTWF0TGlzdEhhcm5lc3MgZXh0ZW5kc1xuICAgIE1hdExpc3RIYXJuZXNzQmFzZTx0eXBlb2YgTWF0TGlzdEl0ZW1IYXJuZXNzLCBNYXRMaXN0SXRlbUhhcm5lc3MsIExpc3RJdGVtSGFybmVzc0ZpbHRlcnM+IHtcbiAgLyoqIFRoZSBzZWxlY3RvciBmb3IgdGhlIGhvc3QgZWxlbWVudCBvZiBhIGBNYXRMaXN0YCBpbnN0YW5jZS4gKi9cbiAgc3RhdGljIGhvc3RTZWxlY3RvciA9ICdtYXQtbGlzdCc7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYE1hdExpc3RIYXJuZXNzYCB0aGF0IG1lZXRzIGNlcnRhaW5cbiAgICogY3JpdGVyaWEuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIGZpbHRlcmluZyB3aGljaCBsaXN0IGluc3RhbmNlcyBhcmUgY29uc2lkZXJlZCBhIG1hdGNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IExpc3RIYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXRMaXN0SGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXRMaXN0SGFybmVzcywgb3B0aW9ucyk7XG4gIH1cblxuICBfaXRlbUhhcm5lc3MgPSBNYXRMaXN0SXRlbUhhcm5lc3M7XG59XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgbGlzdCBpdGVtLiAqL1xuZXhwb3J0IGNsYXNzIE1hdExpc3RJdGVtSGFybmVzcyBleHRlbmRzIE1hdExpc3RJdGVtSGFybmVzc0Jhc2Uge1xuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdExpc3RJdGVtYCBpbnN0YW5jZS4gKi9cbiAgc3RhdGljIGhvc3RTZWxlY3RvciA9IFsnbWF0LWxpc3QtaXRlbScsICdhW21hdC1saXN0LWl0ZW1dJywgJ2J1dHRvblttYXQtbGlzdC1pdGVtXSddXG4gICAgICAubWFwKHNlbGVjdG9yID0+IGAke01hdExpc3RIYXJuZXNzLmhvc3RTZWxlY3Rvcn0gJHtzZWxlY3Rvcn1gKVxuICAgICAgLmpvaW4oJywnKTtcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3IgYSBgTWF0TGlzdEl0ZW1IYXJuZXNzYCB0aGF0IG1lZXRzXG4gICAqIGNlcnRhaW4gY3JpdGVyaWEuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIGZpbHRlcmluZyB3aGljaCBsaXN0IGl0ZW0gaW5zdGFuY2VzIGFyZSBjb25zaWRlcmVkIGEgbWF0Y2guXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogTGlzdEl0ZW1IYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXRMaXN0SXRlbUhhcm5lc3M+IHtcbiAgICByZXR1cm4gZ2V0TGlzdEl0ZW1QcmVkaWNhdGUoTWF0TGlzdEl0ZW1IYXJuZXNzLCBvcHRpb25zKTtcbiAgfVxufVxuIl19