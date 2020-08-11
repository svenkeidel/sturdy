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
/** Harness for interacting with a standard mat-nav-list in tests. */
let MatNavListHarness = /** @class */ (() => {
    class MatNavListHarness extends MatListHarnessBase {
        constructor() {
            super(...arguments);
            this._itemHarness = MatNavListItemHarness;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatNavListHarness` that meets
         * certain criteria.
         * @param options Options for filtering which nav list instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatNavListHarness, options);
        }
    }
    /** The selector for the host element of a `MatNavList` instance. */
    MatNavListHarness.hostSelector = 'mat-nav-list';
    return MatNavListHarness;
})();
export { MatNavListHarness };
/** Harness for interacting with a nav list item. */
let MatNavListItemHarness = /** @class */ (() => {
    class MatNavListItemHarness extends MatListItemHarnessBase {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatNavListItemHarness` that
         * meets certain criteria.
         * @param options Options for filtering which nav list item instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return getListItemPredicate(MatNavListItemHarness, options)
                .addOption('href', options.href, (harness, href) => __awaiter(this, void 0, void 0, function* () { return HarnessPredicate.stringMatches(harness.getHref(), href); }));
        }
        /** Gets the href for this nav list item. */
        getHref() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('href');
            });
        }
        /** Clicks on the nav list item. */
        click() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).click();
            });
        }
        /** Focuses the nav list item. */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).focus();
            });
        }
        /** Blurs the nav list item. */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).blur();
            });
        }
    }
    /** The selector for the host element of a `MatListItem` instance. */
    MatNavListItemHarness.hostSelector = ['mat-list-item', 'a[mat-list-item]', 'button[mat-list-item]']
        .map(selector => `${MatNavListHarness.hostSelector} ${selector}`)
        .join(',');
    return MatNavListItemHarness;
})();
export { MatNavListItemHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibmF2LWxpc3QtaGFybmVzcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9saXN0L3Rlc3RpbmcvbmF2LWxpc3QtaGFybmVzcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7O0FBRUgsT0FBTyxFQUFDLGdCQUFnQixFQUFDLE1BQU0sc0JBQXNCLENBQUM7QUFDdEQsT0FBTyxFQUFDLGtCQUFrQixFQUFDLE1BQU0scUJBQXFCLENBQUM7QUFFdkQsT0FBTyxFQUFDLG9CQUFvQixFQUFFLHNCQUFzQixFQUFDLE1BQU0sMEJBQTBCLENBQUM7QUFFdEYscUVBQXFFO0FBQ3JFO0lBQUEsTUFBYSxpQkFBa0IsU0FBUSxrQkFDNEM7UUFEbkY7O1lBZUUsaUJBQVksR0FBRyxxQkFBcUIsQ0FBQztRQUN2QyxDQUFDO1FBWEM7Ozs7O1dBS0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQWlDLEVBQUU7WUFDN0MsT0FBTyxJQUFJLGdCQUFnQixDQUFDLGlCQUFpQixFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQzFELENBQUM7O0lBWEQsb0VBQW9FO0lBQzdELDhCQUFZLEdBQUcsY0FBYyxDQUFDO0lBYXZDLHdCQUFDO0tBQUE7U0FoQlksaUJBQWlCO0FBa0I5QixvREFBb0Q7QUFDcEQ7SUFBQSxNQUFhLHFCQUFzQixTQUFRLHNCQUFzQjtRQU0vRDs7Ozs7V0FLRztRQUNILE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBcUMsRUFBRTtZQUNqRCxPQUFPLG9CQUFvQixDQUFDLHFCQUFxQixFQUFFLE9BQU8sQ0FBQztpQkFDdEQsU0FBUyxDQUFDLE1BQU0sRUFBRSxPQUFPLENBQUMsSUFBSSxFQUMzQixDQUFPLE9BQU8sRUFBRSxJQUFJLEVBQUUsRUFBRSxnREFBQyxPQUFBLGdCQUFnQixDQUFDLGFBQWEsQ0FBQyxPQUFPLENBQUMsT0FBTyxFQUFFLEVBQUUsSUFBSSxDQUFDLENBQUEsR0FBQSxDQUFDLENBQUM7UUFDNUYsQ0FBQztRQUVELDRDQUE0QztRQUN0QyxPQUFPOztnQkFDWCxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxZQUFZLENBQUMsTUFBTSxDQUFDLENBQUM7WUFDbEQsQ0FBQztTQUFBO1FBRUQsbUNBQW1DO1FBQzdCLEtBQUs7O2dCQUNULE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLEtBQUssRUFBRSxDQUFDO1lBQ3JDLENBQUM7U0FBQTtRQUVELGlDQUFpQztRQUMzQixLQUFLOztnQkFDVCxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxLQUFLLEVBQUUsQ0FBQztZQUNyQyxDQUFDO1NBQUE7UUFFRCwrQkFBK0I7UUFDekIsSUFBSTs7Z0JBQ1IsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsSUFBSSxFQUFFLENBQUM7WUFDcEMsQ0FBQztTQUFBOztJQW5DRCxxRUFBcUU7SUFDOUQsa0NBQVksR0FBRyxDQUFDLGVBQWUsRUFBRSxrQkFBa0IsRUFBRSx1QkFBdUIsQ0FBQztTQUMvRSxHQUFHLENBQUMsUUFBUSxDQUFDLEVBQUUsQ0FBQyxHQUFHLGlCQUFpQixDQUFDLFlBQVksSUFBSSxRQUFRLEVBQUUsQ0FBQztTQUNoRSxJQUFJLENBQUMsR0FBRyxDQUFDLENBQUM7SUFpQ2pCLDRCQUFDO0tBQUE7U0FyQ1kscUJBQXFCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7SGFybmVzc1ByZWRpY2F0ZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3Rlc3RpbmcnO1xuaW1wb3J0IHtNYXRMaXN0SGFybmVzc0Jhc2V9IGZyb20gJy4vbGlzdC1oYXJuZXNzLWJhc2UnO1xuaW1wb3J0IHtOYXZMaXN0SGFybmVzc0ZpbHRlcnMsIE5hdkxpc3RJdGVtSGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vbGlzdC1oYXJuZXNzLWZpbHRlcnMnO1xuaW1wb3J0IHtnZXRMaXN0SXRlbVByZWRpY2F0ZSwgTWF0TGlzdEl0ZW1IYXJuZXNzQmFzZX0gZnJvbSAnLi9saXN0LWl0ZW0taGFybmVzcy1iYXNlJztcblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBtYXQtbmF2LWxpc3QgaW4gdGVzdHMuICovXG5leHBvcnQgY2xhc3MgTWF0TmF2TGlzdEhhcm5lc3MgZXh0ZW5kcyBNYXRMaXN0SGFybmVzc0Jhc2U8XG4gICAgdHlwZW9mIE1hdE5hdkxpc3RJdGVtSGFybmVzcywgTWF0TmF2TGlzdEl0ZW1IYXJuZXNzLCBOYXZMaXN0SXRlbUhhcm5lc3NGaWx0ZXJzPiB7XG4gIC8qKiBUaGUgc2VsZWN0b3IgZm9yIHRoZSBob3N0IGVsZW1lbnQgb2YgYSBgTWF0TmF2TGlzdGAgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnbWF0LW5hdi1saXN0JztcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3IgYSBgTWF0TmF2TGlzdEhhcm5lc3NgIHRoYXQgbWVldHNcbiAgICogY2VydGFpbiBjcml0ZXJpYS5cbiAgICogQHBhcmFtIG9wdGlvbnMgT3B0aW9ucyBmb3IgZmlsdGVyaW5nIHdoaWNoIG5hdiBsaXN0IGluc3RhbmNlcyBhcmUgY29uc2lkZXJlZCBhIG1hdGNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IE5hdkxpc3RIYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXROYXZMaXN0SGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXROYXZMaXN0SGFybmVzcywgb3B0aW9ucyk7XG4gIH1cblxuICBfaXRlbUhhcm5lc3MgPSBNYXROYXZMaXN0SXRlbUhhcm5lc3M7XG59XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgbmF2IGxpc3QgaXRlbS4gKi9cbmV4cG9ydCBjbGFzcyBNYXROYXZMaXN0SXRlbUhhcm5lc3MgZXh0ZW5kcyBNYXRMaXN0SXRlbUhhcm5lc3NCYXNlIHtcbiAgLyoqIFRoZSBzZWxlY3RvciBmb3IgdGhlIGhvc3QgZWxlbWVudCBvZiBhIGBNYXRMaXN0SXRlbWAgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSBbJ21hdC1saXN0LWl0ZW0nLCAnYVttYXQtbGlzdC1pdGVtXScsICdidXR0b25bbWF0LWxpc3QtaXRlbV0nXVxuICAgICAgLm1hcChzZWxlY3RvciA9PiBgJHtNYXROYXZMaXN0SGFybmVzcy5ob3N0U2VsZWN0b3J9ICR7c2VsZWN0b3J9YClcbiAgICAgIC5qb2luKCcsJyk7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYE1hdE5hdkxpc3RJdGVtSGFybmVzc2AgdGhhdFxuICAgKiBtZWV0cyBjZXJ0YWluIGNyaXRlcmlhLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBmaWx0ZXJpbmcgd2hpY2ggbmF2IGxpc3QgaXRlbSBpbnN0YW5jZXMgYXJlIGNvbnNpZGVyZWQgYSBtYXRjaC5cbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBOYXZMaXN0SXRlbUhhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdE5hdkxpc3RJdGVtSGFybmVzcz4ge1xuICAgIHJldHVybiBnZXRMaXN0SXRlbVByZWRpY2F0ZShNYXROYXZMaXN0SXRlbUhhcm5lc3MsIG9wdGlvbnMpXG4gICAgICAgIC5hZGRPcHRpb24oJ2hyZWYnLCBvcHRpb25zLmhyZWYsXG4gICAgICAgICAgICBhc3luYyAoaGFybmVzcywgaHJlZikgPT4gSGFybmVzc1ByZWRpY2F0ZS5zdHJpbmdNYXRjaGVzKGhhcm5lc3MuZ2V0SHJlZigpLCBocmVmKSk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgaHJlZiBmb3IgdGhpcyBuYXYgbGlzdCBpdGVtLiAqL1xuICBhc3luYyBnZXRIcmVmKCk6IFByb21pc2U8c3RyaW5nIHwgbnVsbD4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldEF0dHJpYnV0ZSgnaHJlZicpO1xuICB9XG5cbiAgLyoqIENsaWNrcyBvbiB0aGUgbmF2IGxpc3QgaXRlbS4gKi9cbiAgYXN5bmMgY2xpY2soKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuY2xpY2soKTtcbiAgfVxuXG4gIC8qKiBGb2N1c2VzIHRoZSBuYXYgbGlzdCBpdGVtLiAqL1xuICBhc3luYyBmb2N1cygpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5mb2N1cygpO1xuICB9XG5cbiAgLyoqIEJsdXJzIHRoZSBuYXYgbGlzdCBpdGVtLiAqL1xuICBhc3luYyBibHVyKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmJsdXIoKTtcbiAgfVxufVxuIl19