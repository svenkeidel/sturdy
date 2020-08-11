import { __awaiter } from 'tslib';
import { HarnessPredicate } from '@angular/cdk/testing';
import { MatFormFieldControlHarness } from '@angular/material/form-field/testing/control';

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** Harness for interacting with a standard Material inputs in tests. */
let MatInputHarness = /** @class */ (() => {
    class MatInputHarness extends MatFormFieldControlHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatInputHarness` that meets
         * certain criteria.
         * @param options Options for filtering which input instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatInputHarness, options)
                .addOption('value', options.value, (harness, value) => __awaiter(this, void 0, void 0, function* () {
                return (yield harness.getValue()) === value;
            }))
                .addOption('placeholder', options.placeholder, (harness, placeholder) => __awaiter(this, void 0, void 0, function* () {
                return (yield harness.getPlaceholder()) === placeholder;
            }));
        }
        /** Whether the input is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getProperty('disabled');
            });
        }
        /** Whether the input is required. */
        isRequired() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getProperty('required');
            });
        }
        /** Whether the input is readonly. */
        isReadonly() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getProperty('readOnly');
            });
        }
        /** Gets the value of the input. */
        getValue() {
            return __awaiter(this, void 0, void 0, function* () {
                // The "value" property of the native input is never undefined.
                return (yield (yield this.host()).getProperty('value'));
            });
        }
        /** Gets the name of the input. */
        getName() {
            return __awaiter(this, void 0, void 0, function* () {
                // The "name" property of the native input is never undefined.
                return (yield (yield this.host()).getProperty('name'));
            });
        }
        /**
         * Gets the type of the input. Returns "textarea" if the input is
         * a textarea.
         */
        getType() {
            return __awaiter(this, void 0, void 0, function* () {
                // The "type" property of the native input is never undefined.
                return (yield (yield this.host()).getProperty('type'));
            });
        }
        /** Gets the placeholder of the input. */
        getPlaceholder() {
            return __awaiter(this, void 0, void 0, function* () {
                // The "placeholder" property of the native input is never undefined.
                return (yield (yield this.host()).getProperty('placeholder'));
            });
        }
        /** Gets the id of the input. */
        getId() {
            return __awaiter(this, void 0, void 0, function* () {
                // The input directive always assigns a unique id to the input in
                // case no id has been explicitly specified.
                return (yield (yield this.host()).getProperty('id'));
            });
        }
        /**
         * Focuses the input and returns a promise that indicates when the
         * action is complete.
         */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).focus();
            });
        }
        /**
         * Blurs the input and returns a promise that indicates when the
         * action is complete.
         */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).blur();
            });
        }
        /**
         * Sets the value of the input. The value will be set by simulating
         * keypresses that correspond to the given value.
         */
        setValue(newValue) {
            return __awaiter(this, void 0, void 0, function* () {
                const inputEl = yield this.host();
                yield inputEl.clear();
                // We don't want to send keys for the value if the value is an empty
                // string in order to clear the value. Sending keys with an empty string
                // still results in unnecessary focus events.
                if (newValue) {
                    yield inputEl.sendKeys(newValue);
                }
                // Some input types won't respond to key presses (e.g. `color`) so to be sure that the
                // value is set, we also set the property after the keyboard sequence. Note that we don't
                // want to do it before, because it can cause the value to be entered twice.
                // @breaking-change 11.0.0 Remove non-null assertion once `setInputValue` is required.
                if (inputEl.setInputValue) {
                    yield inputEl.setInputValue(newValue);
                }
            });
        }
    }
    // TODO: We do not want to handle `select` elements with `matNativeControl` because
    // not all methods of this harness work reasonably for native select elements.
    // For more details. See: https://github.com/angular/components/pull/18221.
    MatInputHarness.hostSelector = '[matInput], input[matNativeControl], textarea[matNativeControl]';
    return MatInputHarness;
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

export { MatInputHarness };
//# sourceMappingURL=testing.js.map
