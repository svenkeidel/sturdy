/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MatSelectHarness } from '@angular/material/select/testing';
import { coerceNumberProperty } from '@angular/cdk/coercion';
/** Harness for interacting with a standard mat-paginator in tests. */
let MatPaginatorHarness = /** @class */ (() => {
    class MatPaginatorHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._nextButton = this.locatorFor('.mat-paginator-navigation-next');
            this._previousButton = this.locatorFor('.mat-paginator-navigation-previous');
            this._firstPageButton = this.locatorForOptional('.mat-paginator-navigation-first');
            this._lastPageButton = this.locatorForOptional('.mat-paginator-navigation-last');
            this._select = this.locatorForOptional(MatSelectHarness.with({
                ancestor: '.mat-paginator-page-size'
            }));
            this._pageSizeFallback = this.locatorFor('.mat-paginator-page-size-value');
            this._rangeLabel = this.locatorFor('.mat-paginator-range-label');
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatPaginatorHarness` that meets
         * certain criteria.
         * @param options Options for filtering which paginator instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatPaginatorHarness, options);
        }
        /** Goes to the next page in the paginator. */
        goToNextPage() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._nextButton()).click();
            });
        }
        /** Goes to the previous page in the paginator. */
        goToPreviousPage() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._previousButton()).click();
            });
        }
        /** Goes to the first page in the paginator. */
        goToFirstPage() {
            return __awaiter(this, void 0, void 0, function* () {
                const button = yield this._firstPageButton();
                // The first page button isn't enabled by default so we need to check for it.
                if (!button) {
                    throw Error('Could not find first page button inside paginator. ' +
                        'Make sure that `showFirstLastButtons` is enabled.');
                }
                return button.click();
            });
        }
        /** Goes to the last page in the paginator. */
        goToLastPage() {
            return __awaiter(this, void 0, void 0, function* () {
                const button = yield this._lastPageButton();
                // The last page button isn't enabled by default so we need to check for it.
                if (!button) {
                    throw Error('Could not find last page button inside paginator. ' +
                        'Make sure that `showFirstLastButtons` is enabled.');
                }
                return button.click();
            });
        }
        /**
         * Sets the page size of the paginator.
         * @param size Page size that should be select.
         */
        setPageSize(size) {
            return __awaiter(this, void 0, void 0, function* () {
                const select = yield this._select();
                // The select is only available if the `pageSizeOptions` are
                // set to an array with more than one item.
                if (!select) {
                    throw Error('Cannot find page size selector in paginator. ' +
                        'Make sure that the `pageSizeOptions` have been configured.');
                }
                return select.clickOptions({ text: `${size}` });
            });
        }
        /** Gets the page size of the paginator. */
        getPageSize() {
            return __awaiter(this, void 0, void 0, function* () {
                const select = yield this._select();
                const value = select ? select.getValueText() : (yield this._pageSizeFallback()).text();
                return coerceNumberProperty(yield value);
            });
        }
        /** Gets the text of the range labe of the paginator. */
        getRangeLabel() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._rangeLabel()).text();
            });
        }
    }
    /** Selector used to find paginator instances. */
    MatPaginatorHarness.hostSelector = '.mat-paginator';
    return MatPaginatorHarness;
})();
export { MatPaginatorHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGFnaW5hdG9yLWhhcm5lc3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvcGFnaW5hdG9yL3Rlc3RpbmcvcGFnaW5hdG9yLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBQ3hFLE9BQU8sRUFBQyxnQkFBZ0IsRUFBQyxNQUFNLGtDQUFrQyxDQUFDO0FBQ2xFLE9BQU8sRUFBQyxvQkFBb0IsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBSTNELHNFQUFzRTtBQUN0RTtJQUFBLE1BQWEsbUJBQW9CLFNBQVEsZ0JBQWdCO1FBQXpEOztZQUdVLGdCQUFXLEdBQUcsSUFBSSxDQUFDLFVBQVUsQ0FBQyxnQ0FBZ0MsQ0FBQyxDQUFDO1lBQ2hFLG9CQUFlLEdBQUcsSUFBSSxDQUFDLFVBQVUsQ0FBQyxvQ0FBb0MsQ0FBQyxDQUFDO1lBQ3hFLHFCQUFnQixHQUFHLElBQUksQ0FBQyxrQkFBa0IsQ0FBQyxpQ0FBaUMsQ0FBQyxDQUFDO1lBQzlFLG9CQUFlLEdBQUcsSUFBSSxDQUFDLGtCQUFrQixDQUFDLGdDQUFnQyxDQUFDLENBQUM7WUFDNUUsWUFBTyxHQUFHLElBQUksQ0FBQyxrQkFBa0IsQ0FBQyxnQkFBZ0IsQ0FBQyxJQUFJLENBQUM7Z0JBQzlELFFBQVEsRUFBRSwwQkFBMEI7YUFDckMsQ0FBQyxDQUFDLENBQUM7WUFDSSxzQkFBaUIsR0FBRyxJQUFJLENBQUMsVUFBVSxDQUFDLGdDQUFnQyxDQUFDLENBQUM7WUFDdEUsZ0JBQVcsR0FBRyxJQUFJLENBQUMsVUFBVSxDQUFDLDRCQUE0QixDQUFDLENBQUM7UUE0RXRFLENBQUM7UUExRUM7Ozs7O1dBS0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQW1DLEVBQUU7WUFDL0MsT0FBTyxJQUFJLGdCQUFnQixDQUFDLG1CQUFtQixFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQzVELENBQUM7UUFFRCw4Q0FBOEM7UUFDeEMsWUFBWTs7Z0JBQ2hCLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxXQUFXLEVBQUUsQ0FBQyxDQUFDLEtBQUssRUFBRSxDQUFDO1lBQzVDLENBQUM7U0FBQTtRQUVELGtEQUFrRDtRQUM1QyxnQkFBZ0I7O2dCQUNwQixPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsZUFBZSxFQUFFLENBQUMsQ0FBQyxLQUFLLEVBQUUsQ0FBQztZQUNoRCxDQUFDO1NBQUE7UUFFRCwrQ0FBK0M7UUFDekMsYUFBYTs7Z0JBQ2pCLE1BQU0sTUFBTSxHQUFHLE1BQU0sSUFBSSxDQUFDLGdCQUFnQixFQUFFLENBQUM7Z0JBRTdDLDZFQUE2RTtnQkFDN0UsSUFBSSxDQUFDLE1BQU0sRUFBRTtvQkFDWCxNQUFNLEtBQUssQ0FBQyxxREFBcUQ7d0JBQ3JELG1EQUFtRCxDQUFDLENBQUM7aUJBQ2xFO2dCQUVELE9BQU8sTUFBTSxDQUFDLEtBQUssRUFBRSxDQUFDO1lBQ3hCLENBQUM7U0FBQTtRQUVELDhDQUE4QztRQUN4QyxZQUFZOztnQkFDaEIsTUFBTSxNQUFNLEdBQUcsTUFBTSxJQUFJLENBQUMsZUFBZSxFQUFFLENBQUM7Z0JBRTVDLDRFQUE0RTtnQkFDNUUsSUFBSSxDQUFDLE1BQU0sRUFBRTtvQkFDWCxNQUFNLEtBQUssQ0FBQyxvREFBb0Q7d0JBQ3BELG1EQUFtRCxDQUFDLENBQUM7aUJBQ2xFO2dCQUVELE9BQU8sTUFBTSxDQUFDLEtBQUssRUFBRSxDQUFDO1lBQ3hCLENBQUM7U0FBQTtRQUVEOzs7V0FHRztRQUNHLFdBQVcsQ0FBQyxJQUFZOztnQkFDNUIsTUFBTSxNQUFNLEdBQUcsTUFBTSxJQUFJLENBQUMsT0FBTyxFQUFFLENBQUM7Z0JBRXBDLDREQUE0RDtnQkFDNUQsMkNBQTJDO2dCQUMzQyxJQUFJLENBQUMsTUFBTSxFQUFFO29CQUNYLE1BQU0sS0FBSyxDQUFDLCtDQUErQzt3QkFDL0MsNERBQTRELENBQUMsQ0FBQztpQkFDM0U7Z0JBRUQsT0FBTyxNQUFNLENBQUMsWUFBWSxDQUFDLEVBQUMsSUFBSSxFQUFFLEdBQUcsSUFBSSxFQUFFLEVBQUMsQ0FBQyxDQUFDO1lBQ2hELENBQUM7U0FBQTtRQUVELDJDQUEyQztRQUNyQyxXQUFXOztnQkFDZixNQUFNLE1BQU0sR0FBRyxNQUFNLElBQUksQ0FBQyxPQUFPLEVBQUUsQ0FBQztnQkFDcEMsTUFBTSxLQUFLLEdBQUcsTUFBTSxDQUFDLENBQUMsQ0FBQyxNQUFNLENBQUMsWUFBWSxFQUFFLENBQUMsQ0FBQyxDQUFDLENBQUMsTUFBTSxJQUFJLENBQUMsaUJBQWlCLEVBQUUsQ0FBQyxDQUFDLElBQUksRUFBRSxDQUFDO2dCQUN2RixPQUFPLG9CQUFvQixDQUFDLE1BQU0sS0FBSyxDQUFDLENBQUM7WUFDM0MsQ0FBQztTQUFBO1FBRUQsd0RBQXdEO1FBQ2xELGFBQWE7O2dCQUNqQixPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsV0FBVyxFQUFFLENBQUMsQ0FBQyxJQUFJLEVBQUUsQ0FBQztZQUMzQyxDQUFDO1NBQUE7O0lBckZELGlEQUFpRDtJQUMxQyxnQ0FBWSxHQUFHLGdCQUFnQixDQUFDO0lBcUZ6QywwQkFBQztLQUFBO1NBdkZZLG1CQUFtQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0NvbXBvbmVudEhhcm5lc3MsIEhhcm5lc3NQcmVkaWNhdGV9IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7TWF0U2VsZWN0SGFybmVzc30gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvc2VsZWN0L3Rlc3RpbmcnO1xuaW1wb3J0IHtjb2VyY2VOdW1iZXJQcm9wZXJ0eX0gZnJvbSAnQGFuZ3VsYXIvY2RrL2NvZXJjaW9uJztcbmltcG9ydCB7UGFnaW5hdG9ySGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vcGFnaW5hdG9yLWhhcm5lc3MtZmlsdGVycyc7XG5cblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBtYXQtcGFnaW5hdG9yIGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdFBhZ2luYXRvckhhcm5lc3MgZXh0ZW5kcyBDb21wb25lbnRIYXJuZXNzIHtcbiAgLyoqIFNlbGVjdG9yIHVzZWQgdG8gZmluZCBwYWdpbmF0b3IgaW5zdGFuY2VzLiAqL1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJy5tYXQtcGFnaW5hdG9yJztcbiAgcHJpdmF0ZSBfbmV4dEJ1dHRvbiA9IHRoaXMubG9jYXRvckZvcignLm1hdC1wYWdpbmF0b3ItbmF2aWdhdGlvbi1uZXh0Jyk7XG4gIHByaXZhdGUgX3ByZXZpb3VzQnV0dG9uID0gdGhpcy5sb2NhdG9yRm9yKCcubWF0LXBhZ2luYXRvci1uYXZpZ2F0aW9uLXByZXZpb3VzJyk7XG4gIHByaXZhdGUgX2ZpcnN0UGFnZUJ1dHRvbiA9IHRoaXMubG9jYXRvckZvck9wdGlvbmFsKCcubWF0LXBhZ2luYXRvci1uYXZpZ2F0aW9uLWZpcnN0Jyk7XG4gIHByaXZhdGUgX2xhc3RQYWdlQnV0dG9uID0gdGhpcy5sb2NhdG9yRm9yT3B0aW9uYWwoJy5tYXQtcGFnaW5hdG9yLW5hdmlnYXRpb24tbGFzdCcpO1xuICBwcml2YXRlIF9zZWxlY3QgPSB0aGlzLmxvY2F0b3JGb3JPcHRpb25hbChNYXRTZWxlY3RIYXJuZXNzLndpdGgoe1xuICAgIGFuY2VzdG9yOiAnLm1hdC1wYWdpbmF0b3ItcGFnZS1zaXplJ1xuICB9KSk7XG4gIHByaXZhdGUgX3BhZ2VTaXplRmFsbGJhY2sgPSB0aGlzLmxvY2F0b3JGb3IoJy5tYXQtcGFnaW5hdG9yLXBhZ2Utc2l6ZS12YWx1ZScpO1xuICBwcml2YXRlIF9yYW5nZUxhYmVsID0gdGhpcy5sb2NhdG9yRm9yKCcubWF0LXBhZ2luYXRvci1yYW5nZS1sYWJlbCcpO1xuXG4gIC8qKlxuICAgKiBHZXRzIGEgYEhhcm5lc3NQcmVkaWNhdGVgIHRoYXQgY2FuIGJlIHVzZWQgdG8gc2VhcmNoIGZvciBhIGBNYXRQYWdpbmF0b3JIYXJuZXNzYCB0aGF0IG1lZXRzXG4gICAqIGNlcnRhaW4gY3JpdGVyaWEuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIGZpbHRlcmluZyB3aGljaCBwYWdpbmF0b3IgaW5zdGFuY2VzIGFyZSBjb25zaWRlcmVkIGEgbWF0Y2guXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogUGFnaW5hdG9ySGFybmVzc0ZpbHRlcnMgPSB7fSk6IEhhcm5lc3NQcmVkaWNhdGU8TWF0UGFnaW5hdG9ySGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXRQYWdpbmF0b3JIYXJuZXNzLCBvcHRpb25zKTtcbiAgfVxuXG4gIC8qKiBHb2VzIHRvIHRoZSBuZXh0IHBhZ2UgaW4gdGhlIHBhZ2luYXRvci4gKi9cbiAgYXN5bmMgZ29Ub05leHRQYWdlKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5fbmV4dEJ1dHRvbigpKS5jbGljaygpO1xuICB9XG5cbiAgLyoqIEdvZXMgdG8gdGhlIHByZXZpb3VzIHBhZ2UgaW4gdGhlIHBhZ2luYXRvci4gKi9cbiAgYXN5bmMgZ29Ub1ByZXZpb3VzUGFnZSgpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuX3ByZXZpb3VzQnV0dG9uKCkpLmNsaWNrKCk7XG4gIH1cblxuICAvKiogR29lcyB0byB0aGUgZmlyc3QgcGFnZSBpbiB0aGUgcGFnaW5hdG9yLiAqL1xuICBhc3luYyBnb1RvRmlyc3RQYWdlKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIGNvbnN0IGJ1dHRvbiA9IGF3YWl0IHRoaXMuX2ZpcnN0UGFnZUJ1dHRvbigpO1xuXG4gICAgLy8gVGhlIGZpcnN0IHBhZ2UgYnV0dG9uIGlzbid0IGVuYWJsZWQgYnkgZGVmYXVsdCBzbyB3ZSBuZWVkIHRvIGNoZWNrIGZvciBpdC5cbiAgICBpZiAoIWJ1dHRvbikge1xuICAgICAgdGhyb3cgRXJyb3IoJ0NvdWxkIG5vdCBmaW5kIGZpcnN0IHBhZ2UgYnV0dG9uIGluc2lkZSBwYWdpbmF0b3IuICcgK1xuICAgICAgICAgICAgICAgICAgJ01ha2Ugc3VyZSB0aGF0IGBzaG93Rmlyc3RMYXN0QnV0dG9uc2AgaXMgZW5hYmxlZC4nKTtcbiAgICB9XG5cbiAgICByZXR1cm4gYnV0dG9uLmNsaWNrKCk7XG4gIH1cblxuICAvKiogR29lcyB0byB0aGUgbGFzdCBwYWdlIGluIHRoZSBwYWdpbmF0b3IuICovXG4gIGFzeW5jIGdvVG9MYXN0UGFnZSgpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICBjb25zdCBidXR0b24gPSBhd2FpdCB0aGlzLl9sYXN0UGFnZUJ1dHRvbigpO1xuXG4gICAgLy8gVGhlIGxhc3QgcGFnZSBidXR0b24gaXNuJ3QgZW5hYmxlZCBieSBkZWZhdWx0IHNvIHdlIG5lZWQgdG8gY2hlY2sgZm9yIGl0LlxuICAgIGlmICghYnV0dG9uKSB7XG4gICAgICB0aHJvdyBFcnJvcignQ291bGQgbm90IGZpbmQgbGFzdCBwYWdlIGJ1dHRvbiBpbnNpZGUgcGFnaW5hdG9yLiAnICtcbiAgICAgICAgICAgICAgICAgICdNYWtlIHN1cmUgdGhhdCBgc2hvd0ZpcnN0TGFzdEJ1dHRvbnNgIGlzIGVuYWJsZWQuJyk7XG4gICAgfVxuXG4gICAgcmV0dXJuIGJ1dHRvbi5jbGljaygpO1xuICB9XG5cbiAgLyoqXG4gICAqIFNldHMgdGhlIHBhZ2Ugc2l6ZSBvZiB0aGUgcGFnaW5hdG9yLlxuICAgKiBAcGFyYW0gc2l6ZSBQYWdlIHNpemUgdGhhdCBzaG91bGQgYmUgc2VsZWN0LlxuICAgKi9cbiAgYXN5bmMgc2V0UGFnZVNpemUoc2l6ZTogbnVtYmVyKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgY29uc3Qgc2VsZWN0ID0gYXdhaXQgdGhpcy5fc2VsZWN0KCk7XG5cbiAgICAvLyBUaGUgc2VsZWN0IGlzIG9ubHkgYXZhaWxhYmxlIGlmIHRoZSBgcGFnZVNpemVPcHRpb25zYCBhcmVcbiAgICAvLyBzZXQgdG8gYW4gYXJyYXkgd2l0aCBtb3JlIHRoYW4gb25lIGl0ZW0uXG4gICAgaWYgKCFzZWxlY3QpIHtcbiAgICAgIHRocm93IEVycm9yKCdDYW5ub3QgZmluZCBwYWdlIHNpemUgc2VsZWN0b3IgaW4gcGFnaW5hdG9yLiAnICtcbiAgICAgICAgICAgICAgICAgICdNYWtlIHN1cmUgdGhhdCB0aGUgYHBhZ2VTaXplT3B0aW9uc2AgaGF2ZSBiZWVuIGNvbmZpZ3VyZWQuJyk7XG4gICAgfVxuXG4gICAgcmV0dXJuIHNlbGVjdC5jbGlja09wdGlvbnMoe3RleHQ6IGAke3NpemV9YH0pO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHBhZ2Ugc2l6ZSBvZiB0aGUgcGFnaW5hdG9yLiAqL1xuICBhc3luYyBnZXRQYWdlU2l6ZSgpOiBQcm9taXNlPG51bWJlcj4ge1xuICAgIGNvbnN0IHNlbGVjdCA9IGF3YWl0IHRoaXMuX3NlbGVjdCgpO1xuICAgIGNvbnN0IHZhbHVlID0gc2VsZWN0ID8gc2VsZWN0LmdldFZhbHVlVGV4dCgpIDogKGF3YWl0IHRoaXMuX3BhZ2VTaXplRmFsbGJhY2soKSkudGV4dCgpO1xuICAgIHJldHVybiBjb2VyY2VOdW1iZXJQcm9wZXJ0eShhd2FpdCB2YWx1ZSk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgdGV4dCBvZiB0aGUgcmFuZ2UgbGFiZSBvZiB0aGUgcGFnaW5hdG9yLiAqL1xuICBhc3luYyBnZXRSYW5nZUxhYmVsKCk6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9yYW5nZUxhYmVsKCkpLnRleHQoKTtcbiAgfVxufVxuIl19