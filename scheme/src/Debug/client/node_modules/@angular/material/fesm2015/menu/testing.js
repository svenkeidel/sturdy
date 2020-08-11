import { __awaiter } from 'tslib';
import { ComponentHarness, HarnessPredicate, TestKey } from '@angular/cdk/testing';
import { coerceBooleanProperty } from '@angular/cdk/coercion';

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
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

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */

export { MatMenuHarness, MatMenuItemHarness };
//# sourceMappingURL=testing.js.map
