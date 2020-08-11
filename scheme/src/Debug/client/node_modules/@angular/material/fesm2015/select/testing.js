import { __awaiter } from 'tslib';
import { HarnessPredicate } from '@angular/cdk/testing';
import { MatFormFieldControlHarness } from '@angular/material/form-field/testing/control';
import { MatOptionHarness, MatOptgroupHarness } from '@angular/material/core/testing';

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** Harness for interacting with a standard mat-select in tests. */
let MatSelectHarness = /** @class */ (() => {
    class MatSelectHarness extends MatFormFieldControlHarness {
        constructor() {
            super(...arguments);
            this._documentRootLocator = this.documentRootLocatorFactory();
            this._backdrop = this._documentRootLocator.locatorFor('.cdk-overlay-backdrop');
            this._trigger = this.locatorFor('.mat-select-trigger');
            this._value = this.locatorFor('.mat-select-value');
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatSelectHarness` that meets
         * certain criteria.
         * @param options Options for filtering which select instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatSelectHarness, options);
        }
        /** Gets a boolean promise indicating if the select is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-select-disabled');
            });
        }
        /** Gets a boolean promise indicating if the select is valid. */
        isValid() {
            return __awaiter(this, void 0, void 0, function* () {
                return !(yield (yield this.host()).hasClass('ng-invalid'));
            });
        }
        /** Gets a boolean promise indicating if the select is required. */
        isRequired() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-select-required');
            });
        }
        /** Gets a boolean promise indicating if the select is empty (no value is selected). */
        isEmpty() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-select-empty');
            });
        }
        /** Gets a boolean promise indicating if the select is in multi-selection mode. */
        isMultiple() {
            return __awaiter(this, void 0, void 0, function* () {
                const ariaMultiselectable = (yield this.host()).getAttribute('aria-multiselectable');
                return (yield ariaMultiselectable) === 'true';
            });
        }
        /** Gets a promise for the select's value text. */
        getValueText() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._value()).text();
            });
        }
        /** Focuses the select and returns a void promise that indicates when the action is complete. */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).focus();
            });
        }
        /** Blurs the select and returns a void promise that indicates when the action is complete. */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).blur();
            });
        }
        /** Gets the options inside the select panel. */
        getOptions(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this._documentRootLocator.locatorForAll(MatOptionHarness.with(Object.assign(Object.assign({}, filter), { ancestor: yield this._getPanelSelector() })))();
            });
        }
        /** Gets the groups of options inside the panel. */
        getOptionGroups(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this._documentRootLocator.locatorForAll(MatOptgroupHarness.with(Object.assign(Object.assign({}, filter), { ancestor: yield this._getPanelSelector() })))();
            });
        }
        /** Gets whether the select is open. */
        isOpen() {
            return __awaiter(this, void 0, void 0, function* () {
                return !!(yield this._documentRootLocator.locatorForOptional(yield this._getPanelSelector())());
            });
        }
        /** Opens the select's panel. */
        open() {
            return __awaiter(this, void 0, void 0, function* () {
                if (!(yield this.isOpen())) {
                    return (yield this._trigger()).click();
                }
            });
        }
        /**
         * Clicks the options that match the passed-in filter. If the select is in multi-selection
         * mode all options will be clicked, otherwise the harness will pick the first matching option.
         */
        clickOptions(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                yield this.open();
                const [isMultiple, options] = yield Promise.all([this.isMultiple(), this.getOptions(filter)]);
                if (options.length === 0) {
                    throw Error('Select does not have options matching the specified filter');
                }
                if (isMultiple) {
                    yield Promise.all(options.map(option => option.click()));
                }
                else {
                    yield options[0].click();
                }
            });
        }
        /** Closes the select's panel. */
        close() {
            return __awaiter(this, void 0, void 0, function* () {
                if (yield this.isOpen()) {
                    // This is the most consistent way that works both in both single and multi-select modes,
                    // but it assumes that only one overlay is open at a time. We should be able to make it
                    // a bit more precise after #16645 where we can dispatch an ESCAPE press to the host instead.
                    return (yield this._backdrop()).click();
                }
            });
        }
        /** Gets the selector that should be used to find this select's panel. */
        _getPanelSelector() {
            return __awaiter(this, void 0, void 0, function* () {
                const id = yield (yield this.host()).getAttribute('id');
                return `#${id}-panel`;
            });
        }
    }
    MatSelectHarness.hostSelector = '.mat-select';
    return MatSelectHarness;
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

export { MatSelectHarness };
//# sourceMappingURL=testing.js.map
