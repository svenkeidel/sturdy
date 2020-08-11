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
/** Harness for interacting with a standard mat-selection-list in tests. */
let MatSelectionListHarness = /** @class */ (() => {
    class MatSelectionListHarness extends MatListHarnessBase {
        constructor() {
            super(...arguments);
            this._itemHarness = MatListOptionHarness;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatSelectionListHarness` that meets
         * certain criteria.
         * @param options Options for filtering which selection list instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatSelectionListHarness, options);
        }
        /** Whether the selection list is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield (yield this.host()).getAttribute('aria-disabled')) === 'true';
            });
        }
        /**
         * Selects all items matching any of the given filters.
         * @param filters Filters that specify which items should be selected.
         */
        selectItems(...filters) {
            return __awaiter(this, void 0, void 0, function* () {
                const items = yield this._getItems(filters);
                yield Promise.all(items.map(item => item.select()));
            });
        }
        /**
         * Deselects all items matching any of the given filters.
         * @param filters Filters that specify which items should be deselected.
         */
        deselectItems(...filters) {
            return __awaiter(this, void 0, void 0, function* () {
                const items = yield this._getItems(filters);
                yield Promise.all(items.map(item => item.deselect()));
            });
        }
        /** Gets all items matching the given list of filters. */
        _getItems(filters) {
            return __awaiter(this, void 0, void 0, function* () {
                if (!filters.length) {
                    return this.getItems();
                }
                return [].concat(...yield Promise.all(filters.map(filter => this.locatorForAll(MatListOptionHarness.with(filter))())));
            });
        }
    }
    /** The selector for the host element of a `MatSelectionList` instance. */
    MatSelectionListHarness.hostSelector = 'mat-selection-list';
    return MatSelectionListHarness;
})();
export { MatSelectionListHarness };
/** Harness for interacting with a list option. */
let MatListOptionHarness = /** @class */ (() => {
    class MatListOptionHarness extends MatListItemHarnessBase {
        constructor() {
            super(...arguments);
            this._itemContent = this.locatorFor('.mat-list-item-content');
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatListOptionHarness` that
         * meets certain criteria.
         * @param options Options for filtering which list option instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return getListItemPredicate(MatListOptionHarness, options)
                .addOption('is selected', options.selected, (harness, selected) => __awaiter(this, void 0, void 0, function* () { return (yield harness.isSelected()) === selected; }));
        }
        /** Gets the position of the checkbox relative to the list option content. */
        getCheckboxPosition() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield (yield this._itemContent()).hasClass('mat-list-item-content-reverse')) ?
                    'after' : 'before';
            });
        }
        /** Whether the list option is selected. */
        isSelected() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield (yield this.host()).getAttribute('aria-selected')) === 'true';
            });
        }
        /** Whether the list option is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield (yield this.host()).getAttribute('aria-disabled')) === 'true';
            });
        }
        /** Focuses the list option. */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).focus();
            });
        }
        /** Blurs the list option. */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).blur();
            });
        }
        /** Toggles the checked state of the checkbox. */
        toggle() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).click();
            });
        }
        /**
         * Puts the list option in a checked state by toggling it if it is currently unchecked, or doing
         * nothing if it is already checked.
         */
        select() {
            return __awaiter(this, void 0, void 0, function* () {
                if (!(yield this.isSelected())) {
                    return this.toggle();
                }
            });
        }
        /**
         * Puts the list option in an unchecked state by toggling it if it is currently checked, or doing
         * nothing if it is already unchecked.
         */
        deselect() {
            return __awaiter(this, void 0, void 0, function* () {
                if (yield this.isSelected()) {
                    return this.toggle();
                }
            });
        }
    }
    /** The selector for the host element of a `MatListOption` instance. */
    MatListOptionHarness.hostSelector = 'mat-list-option';
    return MatListOptionHarness;
})();
export { MatListOptionHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic2VsZWN0aW9uLWxpc3QtaGFybmVzcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9saXN0L3Rlc3Rpbmcvc2VsZWN0aW9uLWxpc3QtaGFybmVzcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7O0FBRUgsT0FBTyxFQUFDLGdCQUFnQixFQUFDLE1BQU0sc0JBQXNCLENBQUM7QUFDdEQsT0FBTyxFQUFDLGtCQUFrQixFQUFDLE1BQU0scUJBQXFCLENBQUM7QUFNdkQsT0FBTyxFQUFDLG9CQUFvQixFQUFFLHNCQUFzQixFQUFDLE1BQU0sMEJBQTBCLENBQUM7QUFFdEYsMkVBQTJFO0FBQzNFO0lBQUEsTUFBYSx1QkFBd0IsU0FBUSxrQkFDbUM7UUFEaEY7O1lBZ0JFLGlCQUFZLEdBQUcsb0JBQW9CLENBQUM7UUFpQ3RDLENBQUM7UUE1Q0M7Ozs7O1dBS0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQXVDLEVBQUU7WUFFbkQsT0FBTyxJQUFJLGdCQUFnQixDQUFDLHVCQUF1QixFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQ2hFLENBQUM7UUFJRCw4Q0FBOEM7UUFDeEMsVUFBVTs7Z0JBQ2QsT0FBTyxDQUFBLE1BQU0sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFlBQVksQ0FBQyxlQUFlLENBQUMsTUFBSyxNQUFNLENBQUM7WUFDNUUsQ0FBQztTQUFBO1FBRUQ7OztXQUdHO1FBQ0csV0FBVyxDQUFDLEdBQUcsT0FBbUM7O2dCQUN0RCxNQUFNLEtBQUssR0FBRyxNQUFNLElBQUksQ0FBQyxTQUFTLENBQUMsT0FBTyxDQUFDLENBQUM7Z0JBQzVDLE1BQU0sT0FBTyxDQUFDLEdBQUcsQ0FBQyxLQUFLLENBQUMsR0FBRyxDQUFDLElBQUksQ0FBQyxFQUFFLENBQUMsSUFBSSxDQUFDLE1BQU0sRUFBRSxDQUFDLENBQUMsQ0FBQztZQUN0RCxD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QUFDLENBQUEsTUFBTSxJQUFJLENBQUMsVUFBVSxFQUFFLENBQUEsRUFBRTtvQkFDNUIsT0FBTyxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUM7aUJBQ3RCO1lBQ0gsQ0FBQztTQUFBO1FBRUQ7OztXQUdHO1FBQ0csUUFBUTs7Z0JBQ1osSUFBSSxNQUFNLElBQUksQ0FBQyxVQUFVLEVBQUUsRUFBRTtvQkFDM0IsT0FBTyxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUM7aUJBQ3RCO1lBQ0gsQ0FBQztTQUFBOztJQWxFRCx1RUFBdUU7SUFDaEUsaUNBQVksR0FBRyxpQkFBaUIsQ0FBQztJQWtFMUMsMkJBQUM7S0FBQTtTQXBFWSxvQkFBb0IiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtIYXJuZXNzUHJlZGljYXRlfSBmcm9tICdAYW5ndWxhci9jZGsvdGVzdGluZyc7XG5pbXBvcnQge01hdExpc3RIYXJuZXNzQmFzZX0gZnJvbSAnLi9saXN0LWhhcm5lc3MtYmFzZSc7XG5pbXBvcnQge1xuICBMaXN0SXRlbUhhcm5lc3NGaWx0ZXJzLFxuICBMaXN0T3B0aW9uSGFybmVzc0ZpbHRlcnMsXG4gIFNlbGVjdGlvbkxpc3RIYXJuZXNzRmlsdGVyc1xufSBmcm9tICcuL2xpc3QtaGFybmVzcy1maWx0ZXJzJztcbmltcG9ydCB7Z2V0TGlzdEl0ZW1QcmVkaWNhdGUsIE1hdExpc3RJdGVtSGFybmVzc0Jhc2V9IGZyb20gJy4vbGlzdC1pdGVtLWhhcm5lc3MtYmFzZSc7XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgbWF0LXNlbGVjdGlvbi1saXN0IGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdFNlbGVjdGlvbkxpc3RIYXJuZXNzIGV4dGVuZHMgTWF0TGlzdEhhcm5lc3NCYXNlPFxuICAgIHR5cGVvZiBNYXRMaXN0T3B0aW9uSGFybmVzcywgTWF0TGlzdE9wdGlvbkhhcm5lc3MsIExpc3RPcHRpb25IYXJuZXNzRmlsdGVycz4ge1xuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdFNlbGVjdGlvbkxpc3RgIGluc3RhbmNlLiAqL1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJ21hdC1zZWxlY3Rpb24tbGlzdCc7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYE1hdFNlbGVjdGlvbkxpc3RIYXJuZXNzYCB0aGF0IG1lZXRzXG4gICAqIGNlcnRhaW4gY3JpdGVyaWEuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIGZpbHRlcmluZyB3aGljaCBzZWxlY3Rpb24gbGlzdCBpbnN0YW5jZXMgYXJlIGNvbnNpZGVyZWQgYSBtYXRjaC5cbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBTZWxlY3Rpb25MaXN0SGFybmVzc0ZpbHRlcnMgPSB7fSk6XG4gICAgICBIYXJuZXNzUHJlZGljYXRlPE1hdFNlbGVjdGlvbkxpc3RIYXJuZXNzPiB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdFNlbGVjdGlvbkxpc3RIYXJuZXNzLCBvcHRpb25zKTtcbiAgfVxuXG4gIF9pdGVtSGFybmVzcyA9IE1hdExpc3RPcHRpb25IYXJuZXNzO1xuXG4gIC8qKiBXaGV0aGVyIHRoZSBzZWxlY3Rpb24gbGlzdCBpcyBkaXNhYmxlZC4gKi9cbiAgYXN5bmMgaXNEaXNhYmxlZCgpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICByZXR1cm4gYXdhaXQgKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ2FyaWEtZGlzYWJsZWQnKSA9PT0gJ3RydWUnO1xuICB9XG5cbiAgLyoqXG4gICAqIFNlbGVjdHMgYWxsIGl0ZW1zIG1hdGNoaW5nIGFueSBvZiB0aGUgZ2l2ZW4gZmlsdGVycy5cbiAgICogQHBhcmFtIGZpbHRlcnMgRmlsdGVycyB0aGF0IHNwZWNpZnkgd2hpY2ggaXRlbXMgc2hvdWxkIGJlIHNlbGVjdGVkLlxuICAgKi9cbiAgYXN5bmMgc2VsZWN0SXRlbXMoLi4uZmlsdGVyczogTGlzdE9wdGlvbkhhcm5lc3NGaWx0ZXJzW10pOiBQcm9taXNlPHZvaWQ+IHtcbiAgICBjb25zdCBpdGVtcyA9IGF3YWl0IHRoaXMuX2dldEl0ZW1zKGZpbHRlcnMpO1xuICAgIGF3YWl0IFByb21pc2UuYWxsKGl0ZW1zLm1hcChpdGVtID0+IGl0ZW0uc2VsZWN0KCkpKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBEZXNlbGVjdHMgYWxsIGl0ZW1zIG1hdGNoaW5nIGFueSBvZiB0aGUgZ2l2ZW4gZmlsdGVycy5cbiAgICogQHBhcmFtIGZpbHRlcnMgRmlsdGVycyB0aGF0IHNwZWNpZnkgd2hpY2ggaXRlbXMgc2hvdWxkIGJlIGRlc2VsZWN0ZWQuXG4gICAqL1xuICBhc3luYyBkZXNlbGVjdEl0ZW1zKC4uLmZpbHRlcnM6IExpc3RJdGVtSGFybmVzc0ZpbHRlcnNbXSk6IFByb21pc2U8dm9pZD4ge1xuICAgIGNvbnN0IGl0ZW1zID0gYXdhaXQgdGhpcy5fZ2V0SXRlbXMoZmlsdGVycyk7XG4gICAgYXdhaXQgUHJvbWlzZS5hbGwoaXRlbXMubWFwKGl0ZW0gPT4gaXRlbS5kZXNlbGVjdCgpKSk7XG4gIH1cblxuICAvKiogR2V0cyBhbGwgaXRlbXMgbWF0Y2hpbmcgdGhlIGdpdmVuIGxpc3Qgb2YgZmlsdGVycy4gKi9cbiAgcHJpdmF0ZSBhc3luYyBfZ2V0SXRlbXMoZmlsdGVyczogTGlzdE9wdGlvbkhhcm5lc3NGaWx0ZXJzW10pOiBQcm9taXNlPE1hdExpc3RPcHRpb25IYXJuZXNzW10+IHtcbiAgICBpZiAoIWZpbHRlcnMubGVuZ3RoKSB7XG4gICAgICByZXR1cm4gdGhpcy5nZXRJdGVtcygpO1xuICAgIH1cbiAgICByZXR1cm4gKFtdIGFzIE1hdExpc3RPcHRpb25IYXJuZXNzW10pLmNvbmNhdCguLi5hd2FpdCBQcm9taXNlLmFsbChcbiAgICAgICAgZmlsdGVycy5tYXAoZmlsdGVyID0+IHRoaXMubG9jYXRvckZvckFsbChNYXRMaXN0T3B0aW9uSGFybmVzcy53aXRoKGZpbHRlcikpKCkpKSk7XG4gIH1cbn1cblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBsaXN0IG9wdGlvbi4gKi9cbmV4cG9ydCBjbGFzcyBNYXRMaXN0T3B0aW9uSGFybmVzcyBleHRlbmRzIE1hdExpc3RJdGVtSGFybmVzc0Jhc2Uge1xuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdExpc3RPcHRpb25gIGluc3RhbmNlLiAqL1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJ21hdC1saXN0LW9wdGlvbic7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYE1hdExpc3RPcHRpb25IYXJuZXNzYCB0aGF0XG4gICAqIG1lZXRzIGNlcnRhaW4gY3JpdGVyaWEuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIGZpbHRlcmluZyB3aGljaCBsaXN0IG9wdGlvbiBpbnN0YW5jZXMgYXJlIGNvbnNpZGVyZWQgYSBtYXRjaC5cbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBMaXN0T3B0aW9uSGFybmVzc0ZpbHRlcnMgPSB7fSk6IEhhcm5lc3NQcmVkaWNhdGU8TWF0TGlzdE9wdGlvbkhhcm5lc3M+IHtcbiAgICByZXR1cm4gZ2V0TGlzdEl0ZW1QcmVkaWNhdGUoTWF0TGlzdE9wdGlvbkhhcm5lc3MsIG9wdGlvbnMpXG4gICAgICAgIC5hZGRPcHRpb24oJ2lzIHNlbGVjdGVkJywgb3B0aW9ucy5zZWxlY3RlZCxcbiAgICAgICAgICAgIGFzeW5jIChoYXJuZXNzLCBzZWxlY3RlZCkgPT4gYXdhaXQgaGFybmVzcy5pc1NlbGVjdGVkKCkgPT09IHNlbGVjdGVkKTtcbiAgfVxuXG4gIHByaXZhdGUgX2l0ZW1Db250ZW50ID0gdGhpcy5sb2NhdG9yRm9yKCcubWF0LWxpc3QtaXRlbS1jb250ZW50Jyk7XG5cbiAgLyoqIEdldHMgdGhlIHBvc2l0aW9uIG9mIHRoZSBjaGVja2JveCByZWxhdGl2ZSB0byB0aGUgbGlzdCBvcHRpb24gY29udGVudC4gKi9cbiAgYXN5bmMgZ2V0Q2hlY2tib3hQb3NpdGlvbigpOiBQcm9taXNlPCdiZWZvcmUnIHwgJ2FmdGVyJz4ge1xuICAgIHJldHVybiBhd2FpdCAoYXdhaXQgdGhpcy5faXRlbUNvbnRlbnQoKSkuaGFzQ2xhc3MoJ21hdC1saXN0LWl0ZW0tY29udGVudC1yZXZlcnNlJykgP1xuICAgICAgICAnYWZ0ZXInIDogJ2JlZm9yZSc7XG4gIH1cblxuICAvKiogV2hldGhlciB0aGUgbGlzdCBvcHRpb24gaXMgc2VsZWN0ZWQuICovXG4gIGFzeW5jIGlzU2VsZWN0ZWQoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIGF3YWl0IChhd2FpdCB0aGlzLmhvc3QoKSkuZ2V0QXR0cmlidXRlKCdhcmlhLXNlbGVjdGVkJykgPT09ICd0cnVlJztcbiAgfVxuXG4gIC8qKiBXaGV0aGVyIHRoZSBsaXN0IG9wdGlvbiBpcyBkaXNhYmxlZC4gKi9cbiAgYXN5bmMgaXNEaXNhYmxlZCgpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICByZXR1cm4gYXdhaXQgKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ2FyaWEtZGlzYWJsZWQnKSA9PT0gJ3RydWUnO1xuICB9XG5cbiAgLyoqIEZvY3VzZXMgdGhlIGxpc3Qgb3B0aW9uLiAqL1xuICBhc3luYyBmb2N1cygpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5mb2N1cygpO1xuICB9XG5cbiAgLyoqIEJsdXJzIHRoZSBsaXN0IG9wdGlvbi4gKi9cbiAgYXN5bmMgYmx1cigpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5ibHVyKCk7XG4gIH1cblxuICAvKiogVG9nZ2xlcyB0aGUgY2hlY2tlZCBzdGF0ZSBvZiB0aGUgY2hlY2tib3guICovXG4gIGFzeW5jIHRvZ2dsZSgpIHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5jbGljaygpO1xuICB9XG5cbiAgLyoqXG4gICAqIFB1dHMgdGhlIGxpc3Qgb3B0aW9uIGluIGEgY2hlY2tlZCBzdGF0ZSBieSB0b2dnbGluZyBpdCBpZiBpdCBpcyBjdXJyZW50bHkgdW5jaGVja2VkLCBvciBkb2luZ1xuICAgKiBub3RoaW5nIGlmIGl0IGlzIGFscmVhZHkgY2hlY2tlZC5cbiAgICovXG4gIGFzeW5jIHNlbGVjdCgpIHtcbiAgICBpZiAoIWF3YWl0IHRoaXMuaXNTZWxlY3RlZCgpKSB7XG4gICAgICByZXR1cm4gdGhpcy50b2dnbGUoKTtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogUHV0cyB0aGUgbGlzdCBvcHRpb24gaW4gYW4gdW5jaGVja2VkIHN0YXRlIGJ5IHRvZ2dsaW5nIGl0IGlmIGl0IGlzIGN1cnJlbnRseSBjaGVja2VkLCBvciBkb2luZ1xuICAgKiBub3RoaW5nIGlmIGl0IGlzIGFscmVhZHkgdW5jaGVja2VkLlxuICAgKi9cbiAgYXN5bmMgZGVzZWxlY3QoKSB7XG4gICAgaWYgKGF3YWl0IHRoaXMuaXNTZWxlY3RlZCgpKSB7XG4gICAgICByZXR1cm4gdGhpcy50b2dnbGUoKTtcbiAgICB9XG4gIH1cbn1cbiJdfQ==