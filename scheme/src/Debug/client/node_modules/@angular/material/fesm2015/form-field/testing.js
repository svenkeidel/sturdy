export * from '@angular/material/form-field/testing/control';
import { __awaiter } from 'tslib';
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MatInputHarness } from '@angular/material/input/testing';
import { MatSelectHarness } from '@angular/material/select/testing';

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** Harness for interacting with a standard Material form-field's in tests. */
let MatFormFieldHarness = /** @class */ (() => {
    class MatFormFieldHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._prefixContainer = this.locatorForOptional('.mat-form-field-prefix');
            this._suffixContainer = this.locatorForOptional('.mat-form-field-suffix');
            this._label = this.locatorForOptional('.mat-form-field-label');
            this._errors = this.locatorForAll('.mat-error');
            this._hints = this.locatorForAll('mat-hint, .mat-hint');
            this._inputControl = this.locatorForOptional(MatInputHarness);
            this._selectControl = this.locatorForOptional(MatSelectHarness);
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatFormFieldHarness` that meets
         * certain criteria.
         * @param options Options for filtering which form field instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatFormFieldHarness, options)
                .addOption('floatingLabelText', options.floatingLabelText, (harness, text) => __awaiter(this, void 0, void 0, function* () { return HarnessPredicate.stringMatches(yield harness.getLabel(), text); }))
                .addOption('hasErrors', options.hasErrors, (harness, hasErrors) => __awaiter(this, void 0, void 0, function* () { return (yield harness.hasErrors()) === hasErrors; }));
        }
        /** Gets the appearance of the form-field. */
        getAppearance() {
            return __awaiter(this, void 0, void 0, function* () {
                const hostClasses = yield (yield this.host()).getAttribute('class');
                if (hostClasses !== null) {
                    const appearanceMatch = hostClasses.match(/mat-form-field-appearance-(legacy|standard|fill|outline)(?:$| )/);
                    if (appearanceMatch) {
                        return appearanceMatch[1];
                    }
                }
                throw Error('Could not determine appearance of form-field.');
            });
        }
        // Implementation of the "getControl" method overload signatures.
        getControl(type) {
            return __awaiter(this, void 0, void 0, function* () {
                if (type) {
                    return this.locatorForOptional(type)();
                }
                const hostEl = yield this.host();
                const [isInput, isSelect] = yield Promise.all([
                    hostEl.hasClass('mat-form-field-type-mat-input'),
                    hostEl.hasClass('mat-form-field-type-mat-select'),
                ]);
                if (isInput) {
                    return this._inputControl();
                }
                else if (isSelect) {
                    return this._selectControl();
                }
                return null;
            });
        }
        /** Whether the form-field has a label. */
        hasLabel() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-form-field-has-label');
            });
        }
        /** Gets the label of the form-field. */
        getLabel() {
            return __awaiter(this, void 0, void 0, function* () {
                const labelEl = yield this._label();
                return labelEl ? labelEl.text() : null;
            });
        }
        /** Whether the form-field has errors. */
        hasErrors() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.getTextErrors()).length > 0;
            });
        }
        /** Whether the label is currently floating. */
        isLabelFloating() {
            return __awaiter(this, void 0, void 0, function* () {
                const [hasLabel, shouldFloat] = yield Promise.all([
                    this.hasLabel(),
                    (yield this.host()).hasClass('mat-form-field-should-float'),
                ]);
                // If there is no label, the label conceptually can never float. The `should-float` class
                // is just always set regardless of whether the label is displayed or not.
                return hasLabel && shouldFloat;
            });
        }
        /** Whether the form-field is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-form-field-disabled');
            });
        }
        /** Whether the form-field is currently autofilled. */
        isAutofilled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-form-field-autofilled');
            });
        }
        /** Gets the theme color of the form-field. */
        getThemeColor() {
            return __awaiter(this, void 0, void 0, function* () {
                const hostEl = yield this.host();
                const [isAccent, isWarn] = yield Promise.all([hostEl.hasClass('mat-accent'), hostEl.hasClass('mat-warn')]);
                if (isAccent) {
                    return 'accent';
                }
                else if (isWarn) {
                    return 'warn';
                }
                return 'primary';
            });
        }
        /** Gets error messages which are currently displayed in the form-field. */
        getTextErrors() {
            return __awaiter(this, void 0, void 0, function* () {
                return Promise.all((yield this._errors()).map(e => e.text()));
            });
        }
        /** Gets hint messages which are currently displayed in the form-field. */
        getTextHints() {
            return __awaiter(this, void 0, void 0, function* () {
                return Promise.all((yield this._hints()).map(e => e.text()));
            });
        }
        /**
         * Gets a reference to the container element which contains all projected
         * prefixes of the form-field.
         */
        getHarnessLoaderForPrefix() {
            return __awaiter(this, void 0, void 0, function* () {
                return this._prefixContainer();
            });
        }
        /**
         * Gets a reference to the container element which contains all projected
         * suffixes of the form-field.
         */
        getHarnessLoaderForSuffix() {
            return __awaiter(this, void 0, void 0, function* () {
                return this._suffixContainer();
            });
        }
        /**
         * Whether the form control has been touched. Returns "null"
         * if no form control is set up.
         */
        isControlTouched() {
            return __awaiter(this, void 0, void 0, function* () {
                if (!(yield this._hasFormControl())) {
                    return null;
                }
                return (yield this.host()).hasClass('ng-touched');
            });
        }
        /**
         * Whether the form control is dirty. Returns "null"
         * if no form control is set up.
         */
        isControlDirty() {
            return __awaiter(this, void 0, void 0, function* () {
                if (!(yield this._hasFormControl())) {
                    return null;
                }
                return (yield this.host()).hasClass('ng-dirty');
            });
        }
        /**
         * Whether the form control is valid. Returns "null"
         * if no form control is set up.
         */
        isControlValid() {
            return __awaiter(this, void 0, void 0, function* () {
                if (!(yield this._hasFormControl())) {
                    return null;
                }
                return (yield this.host()).hasClass('ng-valid');
            });
        }
        /**
         * Whether the form control is pending validation. Returns "null"
         * if no form control is set up.
         */
        isControlPending() {
            return __awaiter(this, void 0, void 0, function* () {
                if (!(yield this._hasFormControl())) {
                    return null;
                }
                return (yield this.host()).hasClass('ng-pending');
            });
        }
        /** Checks whether the form-field control has set up a form control. */
        _hasFormControl() {
            return __awaiter(this, void 0, void 0, function* () {
                const hostEl = yield this.host();
                // If no form "NgControl" is bound to the form-field control, the form-field
                // is not able to forward any control status classes. Therefore if either the
                // "ng-touched" or "ng-untouched" class is set, we know that it has a form control
                const [isTouched, isUntouched] = yield Promise.all([hostEl.hasClass('ng-touched'), hostEl.hasClass('ng-untouched')]);
                return isTouched || isUntouched;
            });
        }
    }
    MatFormFieldHarness.hostSelector = '.mat-form-field';
    return MatFormFieldHarness;
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

export { MatFormFieldHarness };
//# sourceMappingURL=testing.js.map
