/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
/** Harness for interacting with a standard `MatGridTitle` in tests. */
let MatGridTileHarness = /** @class */ (() => {
    class MatGridTileHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._header = this.locatorForOptional('.mat-grid-tile-header');
            this._footer = this.locatorForOptional('.mat-grid-tile-footer');
            this._avatar = this.locatorForOptional('.mat-grid-avatar');
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatGridTileHarness`
         * that meets certain criteria.
         * @param options Options for filtering which dialog instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatGridTileHarness, options)
                .addOption('headerText', options.headerText, (harness, pattern) => HarnessPredicate.stringMatches(harness.getHeaderText(), pattern))
                .addOption('footerText', options.footerText, (harness, pattern) => HarnessPredicate.stringMatches(harness.getFooterText(), pattern));
        }
        /** Gets the amount of rows that the grid-tile takes up. */
        getRowspan() {
            return __awaiter(this, void 0, void 0, function* () {
                return Number(yield (yield this.host()).getAttribute('rowspan'));
            });
        }
        /** Gets the amount of columns that the grid-tile takes up. */
        getColspan() {
            return __awaiter(this, void 0, void 0, function* () {
                return Number(yield (yield this.host()).getAttribute('colspan'));
            });
        }
        /** Whether the grid-tile has a header. */
        hasHeader() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._header()) !== null;
            });
        }
        /** Whether the grid-tile has a footer. */
        hasFooter() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._footer()) !== null;
            });
        }
        /** Whether the grid-tile has an avatar. */
        hasAvatar() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._avatar()) !== null;
            });
        }
        /** Gets the text of the header if present. */
        getHeaderText() {
            return __awaiter(this, void 0, void 0, function* () {
                // For performance reasons, we do not use "hasHeader" as
                // we would then need to query twice for the header.
                const headerEl = yield this._header();
                return headerEl ? headerEl.text() : null;
            });
        }
        /** Gets the text of the footer if present. */
        getFooterText() {
            return __awaiter(this, void 0, void 0, function* () {
                // For performance reasons, we do not use "hasFooter" as
                // we would then need to query twice for the footer.
                const headerEl = yield this._footer();
                return headerEl ? headerEl.text() : null;
            });
        }
    }
    /** The selector for the host element of a `MatGridTile` instance. */
    MatGridTileHarness.hostSelector = '.mat-grid-tile';
    return MatGridTileHarness;
})();
export { MatGridTileHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZ3JpZC10aWxlLWhhcm5lc3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvZ3JpZC1saXN0L3Rlc3RpbmcvZ3JpZC10aWxlLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBR3hFLHVFQUF1RTtBQUN2RTtJQUFBLE1BQWEsa0JBQW1CLFNBQVEsZ0JBQWdCO1FBQXhEOztZQW9CVSxZQUFPLEdBQUcsSUFBSSxDQUFDLGtCQUFrQixDQUFDLHVCQUF1QixDQUFDLENBQUM7WUFDM0QsWUFBTyxHQUFHLElBQUksQ0FBQyxrQkFBa0IsQ0FBQyx1QkFBdUIsQ0FBQyxDQUFDO1lBQzNELFlBQU8sR0FBRyxJQUFJLENBQUMsa0JBQWtCLENBQUMsa0JBQWtCLENBQUMsQ0FBQztRQTBDaEUsQ0FBQztRQTVEQzs7Ozs7V0FLRztRQUNILE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBa0MsRUFBRTtZQUM5QyxPQUFPLElBQUksZ0JBQWdCLENBQUMsa0JBQWtCLEVBQUUsT0FBTyxDQUFDO2lCQUNuRCxTQUFTLENBQ04sWUFBWSxFQUFFLE9BQU8sQ0FBQyxVQUFVLEVBQ2hDLENBQUMsT0FBTyxFQUFFLE9BQU8sRUFBRSxFQUFFLENBQUMsZ0JBQWdCLENBQUMsYUFBYSxDQUFDLE9BQU8sQ0FBQyxhQUFhLEVBQUUsRUFBRSxPQUFPLENBQUMsQ0FBQztpQkFDMUYsU0FBUyxDQUNOLFlBQVksRUFBRSxPQUFPLENBQUMsVUFBVSxFQUNoQyxDQUFDLE9BQU8sRUFBRSxPQUFPLEVBQUUsRUFBRSxDQUFDLGdCQUFnQixDQUFDLGFBQWEsQ0FBQyxPQUFPLENBQUMsYUFBYSxFQUFFLEVBQUUsT0FBTyxDQUFDLENBQUMsQ0FBQztRQUNsRyxDQUFDO1FBTUQsMkRBQTJEO1FBQ3JELFVBQVU7O2dCQUNkLE9BQU8sTUFBTSxDQUFDLE1BQU0sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFlBQVksQ0FBQyxTQUFTLENBQUMsQ0FBQyxDQUFDO1lBQ25FLENBQUM7U0FBQTtRQUVELDhEQUE4RDtRQUN4RCxVQUFVOztnQkFDZCxPQUFPLE1BQU0sQ0FBQyxNQUFNLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxZQUFZLENBQUMsU0FBUyxDQUFDLENBQUMsQ0FBQztZQUNuRSxDQUFDO1NBQUE7UUFFRCwwQ0FBMEM7UUFDcEMsU0FBUzs7Z0JBQ2IsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDLEtBQUssSUFBSSxDQUFDO1lBQ3pDLENBQUM7U0FBQTtRQUVELDBDQUEwQztRQUNwQyxTQUFTOztnQkFDYixPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsT0FBTyxFQUFFLENBQUMsS0FBSyxJQUFJLENBQUM7WUFDekMsQ0FBQztTQUFBO1FBRUQsMkNBQTJDO1FBQ3JDLFNBQVM7O2dCQUNiLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxPQUFPLEVBQUUsQ0FBQyxLQUFLLElBQUksQ0FBQztZQUN6QyxDQUFDO1NBQUE7UUFFRCw4Q0FBOEM7UUFDeEMsYUFBYTs7Z0JBQ2pCLHdEQUF3RDtnQkFDeEQsb0RBQW9EO2dCQUNwRCxNQUFNLFFBQVEsR0FBRyxNQUFNLElBQUksQ0FBQyxPQUFPLEVBQUUsQ0FBQztnQkFDdEMsT0FBTyxRQUFRLENBQUMsQ0FBQyxDQUFDLFFBQVEsQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDO1lBQzNDLENBQUM7U0FBQTtRQUVELDhDQUE4QztRQUN4QyxhQUFhOztnQkFDakIsd0RBQXdEO2dCQUN4RCxvREFBb0Q7Z0JBQ3BELE1BQU0sUUFBUSxHQUFHLE1BQU0sSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDO2dCQUN0QyxPQUFPLFFBQVEsQ0FBQyxDQUFDLENBQUMsUUFBUSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUM7WUFDM0MsQ0FBQztTQUFBOztJQTlERCxxRUFBcUU7SUFDOUQsK0JBQVksR0FBRyxnQkFBZ0IsQ0FBQztJQThEekMseUJBQUM7S0FBQTtTQWhFWSxrQkFBa0IiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtDb21wb25lbnRIYXJuZXNzLCBIYXJuZXNzUHJlZGljYXRlfSBmcm9tICdAYW5ndWxhci9jZGsvdGVzdGluZyc7XG5pbXBvcnQge0dyaWRUaWxlSGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vZ3JpZC1saXN0LWhhcm5lc3MtZmlsdGVycyc7XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgYE1hdEdyaWRUaXRsZWAgaW4gdGVzdHMuICovXG5leHBvcnQgY2xhc3MgTWF0R3JpZFRpbGVIYXJuZXNzIGV4dGVuZHMgQ29tcG9uZW50SGFybmVzcyB7XG4gIC8qKiBUaGUgc2VsZWN0b3IgZm9yIHRoZSBob3N0IGVsZW1lbnQgb2YgYSBgTWF0R3JpZFRpbGVgIGluc3RhbmNlLiAqL1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJy5tYXQtZ3JpZC10aWxlJztcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3IgYSBgTWF0R3JpZFRpbGVIYXJuZXNzYFxuICAgKiB0aGF0IG1lZXRzIGNlcnRhaW4gY3JpdGVyaWEuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIGZpbHRlcmluZyB3aGljaCBkaWFsb2cgaW5zdGFuY2VzIGFyZSBjb25zaWRlcmVkIGEgbWF0Y2guXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogR3JpZFRpbGVIYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXRHcmlkVGlsZUhhcm5lc3M+IHtcbiAgICByZXR1cm4gbmV3IEhhcm5lc3NQcmVkaWNhdGUoTWF0R3JpZFRpbGVIYXJuZXNzLCBvcHRpb25zKVxuICAgICAgICAuYWRkT3B0aW9uKFxuICAgICAgICAgICAgJ2hlYWRlclRleHQnLCBvcHRpb25zLmhlYWRlclRleHQsXG4gICAgICAgICAgICAoaGFybmVzcywgcGF0dGVybikgPT4gSGFybmVzc1ByZWRpY2F0ZS5zdHJpbmdNYXRjaGVzKGhhcm5lc3MuZ2V0SGVhZGVyVGV4dCgpLCBwYXR0ZXJuKSlcbiAgICAgICAgLmFkZE9wdGlvbihcbiAgICAgICAgICAgICdmb290ZXJUZXh0Jywgb3B0aW9ucy5mb290ZXJUZXh0LFxuICAgICAgICAgICAgKGhhcm5lc3MsIHBhdHRlcm4pID0+IEhhcm5lc3NQcmVkaWNhdGUuc3RyaW5nTWF0Y2hlcyhoYXJuZXNzLmdldEZvb3RlclRleHQoKSwgcGF0dGVybikpO1xuICB9XG5cbiAgcHJpdmF0ZSBfaGVhZGVyID0gdGhpcy5sb2NhdG9yRm9yT3B0aW9uYWwoJy5tYXQtZ3JpZC10aWxlLWhlYWRlcicpO1xuICBwcml2YXRlIF9mb290ZXIgPSB0aGlzLmxvY2F0b3JGb3JPcHRpb25hbCgnLm1hdC1ncmlkLXRpbGUtZm9vdGVyJyk7XG4gIHByaXZhdGUgX2F2YXRhciA9IHRoaXMubG9jYXRvckZvck9wdGlvbmFsKCcubWF0LWdyaWQtYXZhdGFyJyk7XG5cbiAgLyoqIEdldHMgdGhlIGFtb3VudCBvZiByb3dzIHRoYXQgdGhlIGdyaWQtdGlsZSB0YWtlcyB1cC4gKi9cbiAgYXN5bmMgZ2V0Um93c3BhbigpOiBQcm9taXNlPG51bWJlcj4ge1xuICAgIHJldHVybiBOdW1iZXIoYXdhaXQgKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ3Jvd3NwYW4nKSk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgYW1vdW50IG9mIGNvbHVtbnMgdGhhdCB0aGUgZ3JpZC10aWxlIHRha2VzIHVwLiAqL1xuICBhc3luYyBnZXRDb2xzcGFuKCk6IFByb21pc2U8bnVtYmVyPiB7XG4gICAgcmV0dXJuIE51bWJlcihhd2FpdCAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldEF0dHJpYnV0ZSgnY29sc3BhbicpKTtcbiAgfVxuXG4gIC8qKiBXaGV0aGVyIHRoZSBncmlkLXRpbGUgaGFzIGEgaGVhZGVyLiAqL1xuICBhc3luYyBoYXNIZWFkZXIoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9oZWFkZXIoKSkgIT09IG51bGw7XG4gIH1cblxuICAvKiogV2hldGhlciB0aGUgZ3JpZC10aWxlIGhhcyBhIGZvb3Rlci4gKi9cbiAgYXN5bmMgaGFzRm9vdGVyKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5fZm9vdGVyKCkpICE9PSBudWxsO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGdyaWQtdGlsZSBoYXMgYW4gYXZhdGFyLiAqL1xuICBhc3luYyBoYXNBdmF0YXIoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9hdmF0YXIoKSkgIT09IG51bGw7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgdGV4dCBvZiB0aGUgaGVhZGVyIGlmIHByZXNlbnQuICovXG4gIGFzeW5jIGdldEhlYWRlclRleHQoKTogUHJvbWlzZTxzdHJpbmd8bnVsbD4ge1xuICAgIC8vIEZvciBwZXJmb3JtYW5jZSByZWFzb25zLCB3ZSBkbyBub3QgdXNlIFwiaGFzSGVhZGVyXCIgYXNcbiAgICAvLyB3ZSB3b3VsZCB0aGVuIG5lZWQgdG8gcXVlcnkgdHdpY2UgZm9yIHRoZSBoZWFkZXIuXG4gICAgY29uc3QgaGVhZGVyRWwgPSBhd2FpdCB0aGlzLl9oZWFkZXIoKTtcbiAgICByZXR1cm4gaGVhZGVyRWwgPyBoZWFkZXJFbC50ZXh0KCkgOiBudWxsO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHRleHQgb2YgdGhlIGZvb3RlciBpZiBwcmVzZW50LiAqL1xuICBhc3luYyBnZXRGb290ZXJUZXh0KCk6IFByb21pc2U8c3RyaW5nfG51bGw+IHtcbiAgICAvLyBGb3IgcGVyZm9ybWFuY2UgcmVhc29ucywgd2UgZG8gbm90IHVzZSBcImhhc0Zvb3RlclwiIGFzXG4gICAgLy8gd2Ugd291bGQgdGhlbiBuZWVkIHRvIHF1ZXJ5IHR3aWNlIGZvciB0aGUgZm9vdGVyLlxuICAgIGNvbnN0IGhlYWRlckVsID0gYXdhaXQgdGhpcy5fZm9vdGVyKCk7XG4gICAgcmV0dXJuIGhlYWRlckVsID8gaGVhZGVyRWwudGV4dCgpIDogbnVsbDtcbiAgfVxufVxuIl19