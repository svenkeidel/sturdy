import { __awaiter } from 'tslib';
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** Harness for interacting with a standard Angular Material tab-label in tests. */
let MatTabHarness = /** @class */ (() => {
    class MatTabHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatTabHarness` that meets
         * certain criteria.
         * @param options Options for filtering which tab instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatTabHarness, options)
                .addOption('label', options.label, (harness, label) => HarnessPredicate.stringMatches(harness.getLabel(), label));
        }
        /** Gets the label of the tab. */
        getLabel() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).text();
            });
        }
        /** Gets the aria-label of the tab. */
        getAriaLabel() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('aria-label');
            });
        }
        /** Gets the value of the "aria-labelledby" attribute. */
        getAriaLabelledby() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('aria-labelledby');
            });
        }
        /** Whether the tab is selected. */
        isSelected() {
            return __awaiter(this, void 0, void 0, function* () {
                const hostEl = yield this.host();
                return (yield hostEl.getAttribute('aria-selected')) === 'true';
            });
        }
        /** Whether the tab is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                const hostEl = yield this.host();
                return (yield hostEl.getAttribute('aria-disabled')) === 'true';
            });
        }
        /** Selects the given tab by clicking on the label. Tab cannot be selected if disabled. */
        select() {
            return __awaiter(this, void 0, void 0, function* () {
                yield (yield this.host()).click();
            });
        }
        /** Gets the text content of the tab. */
        getTextContent() {
            return __awaiter(this, void 0, void 0, function* () {
                const contentId = yield this._getContentId();
                const contentEl = yield this.documentRootLocatorFactory().locatorFor(`#${contentId}`)();
                return contentEl.text();
            });
        }
        /**
         * Gets a `HarnessLoader` that can be used to load harnesses for components within the tab's
         * content area.
         */
        getHarnessLoaderForContent() {
            return __awaiter(this, void 0, void 0, function* () {
                const contentId = yield this._getContentId();
                return this.documentRootLocatorFactory().harnessLoaderFor(`#${contentId}`);
            });
        }
        /** Gets the element id for the content of the current tab. */
        _getContentId() {
            return __awaiter(this, void 0, void 0, function* () {
                const hostEl = yield this.host();
                // Tabs never have an empty "aria-controls" attribute.
                return (yield hostEl.getAttribute('aria-controls'));
            });
        }
    }
    /** The selector for the host element of a `MatTab` instance. */
    MatTabHarness.hostSelector = '.mat-tab-label';
    return MatTabHarness;
})();

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** Harness for interacting with a standard mat-tab-group in tests. */
let MatTabGroupHarness = /** @class */ (() => {
    class MatTabGroupHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatTabGroupHarness` that meets
         * certain criteria.
         * @param options Options for filtering which tab group instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatTabGroupHarness, options)
                .addOption('selectedTabLabel', options.selectedTabLabel, (harness, label) => __awaiter(this, void 0, void 0, function* () {
                const selectedTab = yield harness.getSelectedTab();
                return HarnessPredicate.stringMatches(yield selectedTab.getLabel(), label);
            }));
        }
        /**
         * Gets the list of tabs in the tab group.
         * @param filter Optionally filters which tabs are included.
         */
        getTabs(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatTabHarness.with(filter))();
            });
        }
        /** Gets the selected tab of the tab group. */
        getSelectedTab() {
            return __awaiter(this, void 0, void 0, function* () {
                const tabs = yield this.getTabs();
                const isSelected = yield Promise.all(tabs.map(t => t.isSelected()));
                for (let i = 0; i < tabs.length; i++) {
                    if (isSelected[i]) {
                        return tabs[i];
                    }
                }
                throw new Error('No selected tab could be found.');
            });
        }
        /**
         * Selects a tab in this tab group.
         * @param filter An optional filter to apply to the child tabs. The first tab matching the filter
         *     will be selected.
         */
        selectTab(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                const tabs = yield this.getTabs(filter);
                if (!tabs.length) {
                    throw Error(`Cannot find mat-tab matching filter ${JSON.stringify(filter)}`);
                }
                yield tabs[0].select();
            });
        }
    }
    /** The selector for the host element of a `MatTabGroup` instance. */
    MatTabGroupHarness.hostSelector = '.mat-tab-group';
    return MatTabGroupHarness;
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

export { MatTabGroupHarness, MatTabHarness };
//# sourceMappingURL=testing.js.map
