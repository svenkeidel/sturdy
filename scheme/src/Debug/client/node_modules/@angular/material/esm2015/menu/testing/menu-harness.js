/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate, TestKey } from '@angular/cdk/testing';
import { coerceBooleanProperty } from '@angular/cdk/coercion';
/** Harness for interacting with a standard mat-menu in tests. */
let MatMenuHarness = /** @class */ (() => {
    class MatMenuHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._documentRootLocator = this.documentRootLocatorFactory();
        }
        // TODO: potentially extend MatButtonHarness
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatMenuHarness` that meets certain
         * criteria.
         * @param options Options for filtering which menu instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatMenuHarness, options)
                .addOption('triggerText', options.triggerText, (harness, text) => HarnessPredicate.stringMatches(harness.getTriggerText(), text));
        }
        /** Whether the menu is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                const disabled = (yield this.host()).getAttribute('disabled');
                return coerceBooleanProperty(yield disabled);
            });
        }
        /** Whether the menu is open. */
        isOpen() {
            return __awaiter(this, void 0, void 0, function* () {
                return !!(yield this._getMenuPanel());
            });
        }
        /** Gets the text of the menu's trigger element. */
        getTriggerText() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).text();
            });
        }
        /** Focuses the menu. */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).focus();
            });
        }
        /** Blurs the menu. */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).blur();
            });
        }
        /** Opens the menu. */
        open() {
            return __awaiter(this, void 0, void 0, function* () {
                if (!(yield this.isOpen())) {
                    return (yield this.host()).click();
                }
            });
        }
        /** Closes the menu. */
        close() {
            return __awaiter(this, void 0, void 0, function* () {
                const panel = yield this._getMenuPanel();
                if (panel) {
                    return panel.sendKeys(TestKey.ESCAPE);
                }
            });
        }
        /**
         * Gets a list of `MatMenuItemHarness` representing the items in the menu.
         * @param filters Optionally filters which menu items are included.
         */
        getItems(filters = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                const panelId = yield this._getPanelId();
                if (panelId) {
                    return this._documentRootLocator.locatorForAll(MatMenuItemHarness.with(Object.assign(Object.assign({}, filters), { ancestor: `#${panelId}` })))();
                }
                return [];
            });
        }
        /**
         * Clicks an item in the menu, and optionally continues clicking items in subsequent sub-menus.
         * @param itemFilter A filter used to represent which item in the menu should be clicked. The
         *     first matching menu item will be clicked.
         * @param subItemFilters A list of filters representing the items to click in any subsequent
         *     sub-menus. The first item in the sub-menu matching the corresponding filter in
         *     `subItemFilters` will be clicked.
         */
        clickItem(itemFilter, ...subItemFilters) {
            return __awaiter(this, void 0, void 0, function* () {
                yield this.open();
                const items = yield this.getItems(itemFilter);
                if (!items.length) {
                    throw Error(`Could not find item matching ${JSON.stringify(itemFilter)}`);
                }
                if (!subItemFilters.length) {
                    return yield items[0].click();
                }
                const menu = yield items[0].getSubmenu();
                if (!menu) {
                    throw Error(`Item matching ${JSON.stringify(itemFilter)} does not have a submenu`);
                }
                return menu.clickItem(...subItemFilters);
            });
        }
        /** Gets the menu panel associated with this menu. */
        _getMenuPanel() {
            return __awaiter(this, void 0, void 0, function* () {
                const panelId = yield this._getPanelId();
                return panelId ? this._documentRootLocator.locatorForOptional(`#${panelId}`)() : null;
            });
        }
        /** Gets the id of the menu panel associated with this menu. */
        _getPanelId() {
            return __awaiter(this, void 0, void 0, function* () {
                const panelId = yield (yield this.host()).getAttribute('aria-controls');
                return panelId || null;
            });
        }
    }
    /** The selector for the host element of a `MatMenu` instance. */
    MatMenuHarness.hostSelector = '.mat-menu-trigger';
    return MatMenuHarness;
})();
export { MatMenuHarness };
/** Harness for interacting with a standard mat-menu-item in tests. */
let MatMenuItemHarness = /** @class */ (() => {
    class MatMenuItemHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatMenuItemHarness` that meets
         * certain criteria.
         * @param options Options for filtering which menu item instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatMenuItemHarness, options)
                .addOption('text', options.text, (harness, text) => HarnessPredicate.stringMatches(harness.getText(), text))
                .addOption('hasSubmenu', options.hasSubmenu, (harness, hasSubmenu) => __awaiter(this, void 0, void 0, function* () { return (yield harness.hasSubmenu()) === hasSubmenu; }));
        }
        /** Whether the menu is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                const disabled = (yield this.host()).getAttribute('disabled');
                return coerceBooleanProperty(yield disabled);
            });
        }
        /** Gets the text of the menu item. */
        getText() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).text();
            });
        }
        /** Focuses the menu item. */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).focus();
            });
        }
        /** Blurs the menu item. */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).blur();
            });
        }
        /** Clicks the menu item. */
        click() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).click();
            });
        }
        /** Whether this item has a submenu. */
        hasSubmenu() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).matchesSelector(MatMenuHarness.hostSelector);
            });
        }
        /** Gets the submenu associated with this menu item, or null if none. */
        getSubmenu() {
            return __awaiter(this, void 0, void 0, function* () {
                if (yield this.hasSubmenu()) {
                    return new MatMenuHarness(this.locatorFactory);
                }
                return null;
            });
        }
    }
    /** The selector for the host element of a `MatMenuItem` instance. */
    MatMenuItemHarness.hostSelector = '.mat-menu-item';
    return MatMenuItemHarness;
})();
export { MatMenuItemHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibWVudS1oYXJuZXNzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL21lbnUvdGVzdGluZy9tZW51LWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBZSxPQUFPLEVBQUMsTUFBTSxzQkFBc0IsQ0FBQztBQUM5RixPQUFPLEVBQUMscUJBQXFCLEVBQUMsTUFBTSx1QkFBdUIsQ0FBQztBQUc1RCxpRUFBaUU7QUFDakU7SUFBQSxNQUFhLGNBQWUsU0FBUSxnQkFBZ0I7UUFBcEQ7O1lBSVUseUJBQW9CLEdBQUcsSUFBSSxDQUFDLDBCQUEwQixFQUFFLENBQUM7UUE4R25FLENBQUM7UUE1R0MsNENBQTRDO1FBRTVDOzs7OztXQUtHO1FBQ0gsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUE4QixFQUFFO1lBQzFDLE9BQU8sSUFBSSxnQkFBZ0IsQ0FBQyxjQUFjLEVBQUUsT0FBTyxDQUFDO2lCQUMvQyxTQUFTLENBQUMsYUFBYSxFQUFFLE9BQU8sQ0FBQyxXQUFXLEVBQ3pDLENBQUMsT0FBTyxFQUFFLElBQUksRUFBRSxFQUFFLENBQUMsZ0JBQWdCLENBQUMsYUFBYSxDQUFDLE9BQU8sQ0FBQyxjQUFjLEVBQUUsRUFBRSxJQUFJLENBQUMsQ0FBQyxDQUFDO1FBQzdGLENBQUM7UUFFRCxvQ0FBb0M7UUFDOUIsVUFBVTs7Z0JBQ2QsTUFBTSxRQUFRLEdBQUcsQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFlBQVksQ0FBQyxVQUFVLENBQUMsQ0FBQztnQkFDOUQsT0FBTyxxQkFBcUIsQ0FBQyxNQUFNLFFBQVEsQ0FBQyxDQUFDO1lBQy9DLENBQUM7U0FBQTtRQUVELGdDQUFnQztRQUMxQixNQUFNOztnQkFDVixPQUFPLENBQUMsQ0FBQyxDQUFDLE1BQU0sSUFBSSxDQUFDLGFB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QUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsSUFBSSxFQUFFLENBQUM7WUFDcEMsQ0FBQztTQUFBO1FBRUQsNkJBQTZCO1FBQ3ZCLEtBQUs7O2dCQUNULE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLEtBQUssRUFBRSxDQUFDO1lBQ3JDLENBQUM7U0FBQTtRQUVELDJCQUEyQjtRQUNyQixJQUFJOztnQkFDUixPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxJQUFJLEVBQUUsQ0FBQztZQUNwQyxDQUFDO1NBQUE7UUFFRCw0QkFBNEI7UUFDdEIsS0FBSzs7Z0JBQ1QsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsS0FBSyxFQUFFLENBQUM7WUFDckMsQ0FBQztTQUFBO1FBRUQsdUNBQXVDO1FBQ2pDLFVBQVU7O2dCQUNkLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLGVBQWUsQ0FBQyxjQUFjLENBQUMsWUFBWSxDQUFDLENBQUM7WUFDMUUsQ0FBQztTQUFBO1FBRUQsd0VBQXdFO1FBQ2xFLFVBQVU7O2dCQUNkLElBQUksTUFBTSxJQUFJLENBQUMsVUFBVSxFQUFFLEVBQUU7b0JBQzNCLE9BQU8sSUFBSSxjQUFjLENBQUMsSUFBSSxDQUFDLGNBQWMsQ0FBQyxDQUFDO2lCQUNoRDtnQkFDRCxPQUFPLElBQUksQ0FBQztZQUNkLENBQUM7U0FBQTs7SUF0REQscUVBQXFFO0lBQzlELCtCQUFZLEdBQUcsZ0JBQWdCLENBQUM7SUFzRHpDLHlCQUFDO0tBQUE7U0F4RFksa0JBQWtCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Q29tcG9uZW50SGFybmVzcywgSGFybmVzc1ByZWRpY2F0ZSwgVGVzdEVsZW1lbnQsIFRlc3RLZXl9IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7Y29lcmNlQm9vbGVhblByb3BlcnR5fSBmcm9tICdAYW5ndWxhci9jZGsvY29lcmNpb24nO1xuaW1wb3J0IHtNZW51SGFybmVzc0ZpbHRlcnMsIE1lbnVJdGVtSGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vbWVudS1oYXJuZXNzLWZpbHRlcnMnO1xuXG4vKiogSGFybmVzcyBmb3IgaW50ZXJhY3Rpbmcgd2l0aCBhIHN0YW5kYXJkIG1hdC1tZW51IGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdE1lbnVIYXJuZXNzIGV4dGVuZHMgQ29tcG9uZW50SGFybmVzcyB7XG4gIC8qKiBUaGUgc2VsZWN0b3IgZm9yIHRoZSBob3N0IGVsZW1lbnQgb2YgYSBgTWF0TWVudWAgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1tZW51LXRyaWdnZXInO1xuXG4gIHByaXZhdGUgX2RvY3VtZW50Um9vdExvY2F0b3IgPSB0aGlzLmRvY3VtZW50Um9vdExvY2F0b3JGYWN0b3J5KCk7XG5cbiAgLy8gVE9ETzogcG90ZW50aWFsbHkgZXh0ZW5kIE1hdEJ1dHRvbkhhcm5lc3NcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3IgYSBgTWF0TWVudUhhcm5lc3NgIHRoYXQgbWVldHMgY2VydGFpblxuICAgKiBjcml0ZXJpYS5cbiAgICogQHBhcmFtIG9wdGlvbnMgT3B0aW9ucyBmb3IgZmlsdGVyaW5nIHdoaWNoIG1lbnUgaW5zdGFuY2VzIGFyZSBjb25zaWRlcmVkIGEgbWF0Y2guXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogTWVudUhhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdE1lbnVIYXJuZXNzPiB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdE1lbnVIYXJuZXNzLCBvcHRpb25zKVxuICAgICAgICAuYWRkT3B0aW9uKCd0cmlnZ2VyVGV4dCcsIG9wdGlvbnMudHJpZ2dlclRleHQsXG4gICAgICAgICAgICAoaGFybmVzcywgdGV4dCkgPT4gSGFybmVzc1ByZWRpY2F0ZS5zdHJpbmdNYXRjaGVzKGhhcm5lc3MuZ2V0VHJpZ2dlclRleHQoKSwgdGV4dCkpO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIG1lbnUgaXMgZGlzYWJsZWQuICovXG4gIGFzeW5jIGlzRGlzYWJsZWQoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgY29uc3QgZGlzYWJsZWQgPSAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldEF0dHJpYnV0ZSgnZGlzYWJsZWQnKTtcbiAgICByZXR1cm4gY29lcmNlQm9vbGVhblByb3BlcnR5KGF3YWl0IGRpc2FibGVkKTtcbiAgfVxuXG4gIC8qKiBXaGV0aGVyIHRoZSBtZW51IGlzIG9wZW4uICovXG4gIGFzeW5jIGlzT3BlbigpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICByZXR1cm4gISEoYXdhaXQgdGhpcy5fZ2V0TWVudVBhbmVsKCkpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHRleHQgb2YgdGhlIG1lbnUncyB0cmlnZ2VyIGVsZW1lbnQuICovXG4gIGFzeW5jIGdldFRyaWdnZXJUZXh0KCk6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkudGV4dCgpO1xuICB9XG5cbiAgLyoqIEZvY3VzZXMgdGhlIG1lbnUuICovXG4gIGFzeW5jIGZvY3VzKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmZvY3VzKCk7XG4gIH1cblxuICAvKiogQmx1cnMgdGhlIG1lbnUuICovXG4gIGFzeW5jIGJsdXIoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuYmx1cigpO1xuICB9XG5cbiAgLyoqIE9wZW5zIHRoZSBtZW51LiAqL1xuICBhc3luYyBvcGVuKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIGlmICghYXdhaXQgdGhpcy5pc09wZW4oKSkge1xuICAgICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuY2xpY2soKTtcbiAgICB9XG4gIH1cblxuICAvKiogQ2xvc2VzIHRoZSBtZW51LiAqL1xuICBhc3luYyBjbG9zZSgpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICBjb25zdCBwYW5lbCA9IGF3YWl0IHRoaXMuX2dldE1lbnVQYW5lbCgpO1xuICAgIGlmIChwYW5lbCkge1xuICAgICAgcmV0dXJuIHBhbmVsLnNlbmRLZXlzKFRlc3RLZXkuRVNDQVBFKTtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogR2V0cyBhIGxpc3Qgb2YgYE1hdE1lbnVJdGVtSGFybmVzc2AgcmVwcmVzZW50aW5nIHRoZSBpdGVtcyBpbiB0aGUgbWVudS5cbiAgICogQHBhcmFtIGZpbHRlcnMgT3B0aW9uYWxseSBmaWx0ZXJzIHdoaWNoIG1lbnUgaXRlbXMgYXJlIGluY2x1ZGVkLlxuICAgKi9cbiAgYXN5bmMgZ2V0SXRlbXMoZmlsdGVyczogT21pdDxNZW51SXRlbUhhcm5lc3NGaWx0ZXJzLCAnYW5jZXN0b3InPiA9IHt9KTpcbiAgICAgIFByb21pc2U8TWF0TWVudUl0ZW1IYXJuZXNzW10+IHtcbiAgICBjb25zdCBwYW5lbElkID0gYXdhaXQgdGhpcy5fZ2V0UGFuZWxJZCgpO1xuICAgIGlmIChwYW5lbElkKSB7XG4gICAgICByZXR1cm4gdGhpcy5fZG9jdW1lbnRSb290TG9jYXRvci5sb2NhdG9yRm9yQWxsKFxuICAgICAgICAgIE1hdE1lbnVJdGVtSGFybmVzcy53aXRoKHsuLi5maWx0ZXJzLCBhbmNlc3RvcjogYCMke3BhbmVsSWR9YH0pKSgpO1xuICAgIH1cbiAgICByZXR1cm4gW107XG4gIH1cblxuICAvKipcbiAgICogQ2xpY2tzIGFuIGl0ZW0gaW4gdGhlIG1lbnUsIGFuZCBvcHRpb25hbGx5IGNvbnRpbnVlcyBjbGlja2luZyBpdGVtcyBpbiBzdWJzZXF1ZW50IHN1Yi1tZW51cy5cbiAgICogQHBhcmFtIGl0ZW1GaWx0ZXIgQSBmaWx0ZXIgdXNlZCB0byByZXByZXNlbnQgd2hpY2ggaXRlbSBpbiB0aGUgbWVudSBzaG91bGQgYmUgY2xpY2tlZC4gVGhlXG4gICAqICAgICBmaXJzdCBtYXRjaGluZyBtZW51IGl0ZW0gd2lsbCBiZSBjbGlja2VkLlxuICAgKiBAcGFyYW0gc3ViSXRlbUZpbHRlcnMgQSBsaXN0IG9mIGZpbHRlcnMgcmVwcmVzZW50aW5nIHRoZSBpdGVtcyB0byBjbGljayBpbiBhbnkgc3Vic2VxdWVudFxuICAgKiAgICAgc3ViLW1lbnVzLiBUaGUgZmlyc3QgaXRlbSBpbiB0aGUgc3ViLW1lbnUgbWF0Y2hpbmcgdGhlIGNvcnJlc3BvbmRpbmcgZmlsdGVyIGluXG4gICAqICAgICBgc3ViSXRlbUZpbHRlcnNgIHdpbGwgYmUgY2xpY2tlZC5cbiAgICovXG4gIGFzeW5jIGNsaWNrSXRlbShcbiAgICAgIGl0ZW1GaWx0ZXI6IE9taXQ8TWVudUl0ZW1IYXJuZXNzRmlsdGVycywgJ2FuY2VzdG9yJz4sXG4gICAgICAuLi5zdWJJdGVtRmlsdGVyczogT21pdDxNZW51SXRlbUhhcm5lc3NGaWx0ZXJzLCAnYW5jZXN0b3InPltdKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgYXdhaXQgdGhpcy5vcGVuKCk7XG4gICAgY29uc3QgaXRlbXMgPSBhd2FpdCB0aGlzLmdldEl0ZW1zKGl0ZW1GaWx0ZXIpO1xuICAgIGlmICghaXRlbXMubGVuZ3RoKSB7XG4gICAgICB0aHJvdyBFcnJvcihgQ291bGQgbm90IGZpbmQgaXRlbSBtYXRjaGluZyAke0pTT04uc3RyaW5naWZ5KGl0ZW1GaWx0ZXIpfWApO1xuICAgIH1cblxuICAgIGlmICghc3ViSXRlbUZpbHRlcnMubGVuZ3RoKSB7XG4gICAgICByZXR1cm4gYXdhaXQgaXRlbXNbMF0uY2xpY2soKTtcbiAgICB9XG5cbiAgICBjb25zdCBtZW51ID0gYXdhaXQgaXRlbXNbMF0uZ2V0U3VibWVudSgpO1xuICAgIGlmICghbWVudSkge1xuICAgICAgdGhyb3cgRXJyb3IoYEl0ZW0gbWF0Y2hpbmcgJHtKU09OLnN0cmluZ2lmeShpdGVtRmlsdGVyKX0gZG9lcyBub3QgaGF2ZSBhIHN1Ym1lbnVgKTtcbiAgICB9XG4gICAgcmV0dXJuIG1lbnUuY2xpY2tJdGVtKC4uLnN1Ykl0ZW1GaWx0ZXJzIGFzIFtPbWl0PE1lbnVJdGVtSGFybmVzc0ZpbHRlcnMsICdhbmNlc3Rvcic+XSk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgbWVudSBwYW5lbCBhc3NvY2lhdGVkIHdpdGggdGhpcyBtZW51LiAqL1xuICBwcml2YXRlIGFzeW5jIF9nZXRNZW51UGFuZWwoKTogUHJvbWlzZTxUZXN0RWxlbWVudCB8IG51bGw+IHtcbiAgICBjb25zdCBwYW5lbElkID0gYXdhaXQgdGhpcy5fZ2V0UGFuZWxJZCgpO1xuICAgIHJldHVybiBwYW5lbElkID8gdGhpcy5fZG9jdW1lbnRSb290TG9jYXRvci5sb2NhdG9yRm9yT3B0aW9uYWwoYCMke3BhbmVsSWR9YCkoKSA6IG51bGw7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgaWQgb2YgdGhlIG1lbnUgcGFuZWwgYXNzb2NpYXRlZCB3aXRoIHRoaXMgbWVudS4gKi9cbiAgcHJpdmF0ZSBhc3luYyBfZ2V0UGFuZWxJZCgpOiBQcm9taXNlPHN0cmluZyB8IG51bGw+IHtcbiAgICBjb25zdCBwYW5lbElkID0gYXdhaXQgKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ2FyaWEtY29udHJvbHMnKTtcbiAgICByZXR1cm4gcGFuZWxJZCB8fCBudWxsO1xuICB9XG59XG5cblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBtYXQtbWVudS1pdGVtIGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdE1lbnVJdGVtSGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdE1lbnVJdGVtYCBpbnN0YW5jZS4gKi9cbiAgc3RhdGljIGhvc3RTZWxlY3RvciA9ICcubWF0LW1lbnUtaXRlbSc7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYE1hdE1lbnVJdGVtSGFybmVzc2AgdGhhdCBtZWV0c1xuICAgKiBjZXJ0YWluIGNyaXRlcmlhLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBmaWx0ZXJpbmcgd2hpY2ggbWVudSBpdGVtIGluc3RhbmNlcyBhcmUgY29uc2lkZXJlZCBhIG1hdGNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IE1lbnVJdGVtSGFybmVzc0ZpbHRlcnMgPSB7fSk6IEhhcm5lc3NQcmVkaWNhdGU8TWF0TWVudUl0ZW1IYXJuZXNzPiB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdE1lbnVJdGVtSGFybmVzcywgb3B0aW9ucylcbiAgICAgICAgLmFkZE9wdGlvbigndGV4dCcsIG9wdGlvbnMudGV4dCxcbiAgICAgICAgICAgIChoYXJuZXNzLCB0ZXh0KSA9PiBIYXJuZXNzUHJlZGljYXRlLnN0cmluZ01hdGNoZXMoaGFybmVzcy5nZXRUZXh0KCksIHRleHQpKVxuICAgICAgICAuYWRkT3B0aW9uKCdoYXNTdWJtZW51Jywgb3B0aW9ucy5oYXNTdWJtZW51LFxuICAgICAgICAgICAgYXN5bmMgKGhhcm5lc3MsIGhhc1N1Ym1lbnUpID0+IChhd2FpdCBoYXJuZXNzLmhhc1N1Ym1lbnUoKSkgPT09IGhhc1N1Ym1lbnUpO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIG1lbnUgaXMgZGlzYWJsZWQuICovXG4gIGFzeW5jIGlzRGlzYWJsZWQoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgY29uc3QgZGlzYWJsZWQgPSAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldEF0dHJpYnV0ZSgnZGlzYWJsZWQnKTtcbiAgICByZXR1cm4gY29lcmNlQm9vbGVhblByb3BlcnR5KGF3YWl0IGRpc2FibGVkKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSB0ZXh0IG9mIHRoZSBtZW51IGl0ZW0uICovXG4gIGFzeW5jIGdldFRleHQoKTogUHJvbWlzZTxzdHJpbmc+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS50ZXh0KCk7XG4gIH1cblxuICAvKiogRm9jdXNlcyB0aGUgbWVudSBpdGVtLiAqL1xuICBhc3luYyBmb2N1cygpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5mb2N1cygpO1xuICB9XG5cbiAgLyoqIEJsdXJzIHRoZSBtZW51IGl0ZW0uICovXG4gIGFzeW5jIGJsdXIoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuYmx1cigpO1xuICB9XG5cbiAgLyoqIENsaWNrcyB0aGUgbWVudSBpdGVtLiAqL1xuICBhc3luYyBjbGljaygpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5jbGljaygpO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhpcyBpdGVtIGhhcyBhIHN1Ym1lbnUuICovXG4gIGFzeW5jIGhhc1N1Ym1lbnUoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkubWF0Y2hlc1NlbGVjdG9yKE1hdE1lbnVIYXJuZXNzLmhvc3RTZWxlY3Rvcik7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgc3VibWVudSBhc3NvY2lhdGVkIHdpdGggdGhpcyBtZW51IGl0ZW0sIG9yIG51bGwgaWYgbm9uZS4gKi9cbiAgYXN5bmMgZ2V0U3VibWVudSgpOiBQcm9taXNlPE1hdE1lbnVIYXJuZXNzIHwgbnVsbD4ge1xuICAgIGlmIChhd2FpdCB0aGlzLmhhc1N1Ym1lbnUoKSkge1xuICAgICAgcmV0dXJuIG5ldyBNYXRNZW51SGFybmVzcyh0aGlzLmxvY2F0b3JGYWN0b3J5KTtcbiAgICB9XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cbn1cbiJdfQ==