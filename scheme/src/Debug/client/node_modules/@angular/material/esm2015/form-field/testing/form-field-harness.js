/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MatInputHarness } from '@angular/material/input/testing';
import { MatSelectHarness } from '@angular/material/select/testing';
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
export { MatFormFieldHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZm9ybS1maWVsZC1oYXJuZXNzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL2Zvcm0tZmllbGQvdGVzdGluZy9mb3JtLWZpZWxkLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFDTCxnQkFBZ0IsRUFFaEIsZ0JBQWdCLEVBR2pCLE1BQU0sc0JBQXNCLENBQUM7QUFFOUIsT0FBTyxFQUFDLGVBQWUsRUFBQyxNQUFNLGlDQUFpQyxDQUFDO0FBQ2hFLE9BQU8sRUFBQyxnQkFBZ0IsRUFBQyxNQUFNLGtDQUFrQyxDQUFDO0FBUWxFLDhFQUE4RTtBQUM5RTtJQUFBLE1BQWEsbUJBQW9CLFNBQVEsZ0JBQWdCO1FBQXpEOztZQWlCVSxxQkFBZ0IsR0FBRyxJQUFJLENBQUMsa0JBQWtCLENBQUMsd0JBQXdCLENBQUMsQ0FBQztZQUNyRSxxQkFBZ0IsR0FBRyxJQUFJLENBQUMsa0JBQWtCLENBQUMsd0JBQXdCLENBQUMsQ0FBQztZQUNyRSxXQUFNLEdBQUcsSUFBSSxDQUFDLGtCQUFrQixDQUFDLHVCQUF1QixDQUFDLENBQUM7WUFDMUQsWUFBTyxHQUFHLElBQUksQ0FBQyxhQUFhLENBQUMsWUFBWSxDQUFDLENBQUM7WUFDM0MsV0FBTSxHQUFHLElBQUksQ0FBQyxhQUFhLENBQUMscUJBQXFCLENBQUMsQ0FBQztZQUVuRCxrQkFBYSxHQUFHLElBQUksQ0FBQyxrQkFBa0IsQ0FBQyxlQUFlLENBQUMsQ0FBQztZQUN6RCxtQkFBYyxHQUFHLElBQUksQ0FBQyxrQkFBa0IsQ0FBQyxnQkFBZ0IsQ0FBQyxDQUFDO1FBd0xyRSxDQUFDO1FBN01DOzs7OztXQUtHO1FBQ0gsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUFtQyxFQUFFO1lBQy9DLE9BQU8sSUFBSSxnQkFBZ0IsQ0FBQyxtQkFBbUIsRUFBRSxPQUFPLENBQUM7aUJBQ3RELFNBQVMsQ0FB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QyxlQUFlLEVBQUUsQ0FBQSxFQUFFO29CQUNqQyxPQUFPLElBQUksQ0FBQztpQkFDYjtnQkFDRCxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxRQUFRLENBQUMsWUFBWSxDQUFDLENBQUM7WUFDcEQsQ0FBQztTQUFBO1FBRUQsdUVBQXVFO1FBQ3pELGVBQWU7O2dCQUMzQixNQUFNLE1BQU0sR0FBRyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQztnQkFDakMsNEVBQTRFO2dCQUM1RSw2RUFBNkU7Z0JBQzdFLGtGQUFrRjtnQkFDbEYsTUFBTSxDQUFDLFNBQVMsRUFBRSxXQUFXLENBQUMsR0FDMUIsTUFBTSxPQUFPLENBQUMsR0FBRyxDQUFDLENBQUMsTUFBTSxDQUFDLFFBQVEsQ0FBQyxZQUFZLENBQUMsRUFBRSxNQUFNLENBQUMsUUFBUSxDQUFDLGNBQWMsQ0FBQyxDQUFDLENBQUMsQ0FBQztnQkFDeEYsT0FBTyxTQUFTLElBQUksV0FBVyxDQUFDO1lBQ2xDLENBQUM7U0FBQTs7SUE5TU0sZ0NBQVksR0FBRyxpQkFBaUIsQ0FBQztJQStNMUMsMEJBQUM7S0FBQTtTQWhOWSxtQkFBbUIiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtcbiAgQ29tcG9uZW50SGFybmVzcyxcbiAgQ29tcG9uZW50SGFybmVzc0NvbnN0cnVjdG9yLFxuICBIYXJuZXNzUHJlZGljYXRlLFxuICBIYXJuZXNzUXVlcnksXG4gIFRlc3RFbGVtZW50XG59IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7TWF0Rm9ybUZpZWxkQ29udHJvbEhhcm5lc3N9IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL2Zvcm0tZmllbGQvdGVzdGluZy9jb250cm9sJztcbmltcG9ydCB7TWF0SW5wdXRIYXJuZXNzfSBmcm9tICdAYW5ndWxhci9tYXRlcmlhbC9pbnB1dC90ZXN0aW5nJztcbmltcG9ydCB7TWF0U2VsZWN0SGFybmVzc30gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvc2VsZWN0L3Rlc3RpbmcnO1xuaW1wb3J0IHtGb3JtRmllbGRIYXJuZXNzRmlsdGVyc30gZnJvbSAnLi9mb3JtLWZpZWxkLWhhcm5lc3MtZmlsdGVycyc7XG5cbi8vIFRPRE8oZGV2dmVyc2lvbik6IHN1cHBvcnQgZGF0ZXBpY2tlciBoYXJuZXNzIG9uY2UgZGV2ZWxvcGVkIChDT01QLTIwMykuXG4vLyBBbHNvIHN1cHBvcnQgY2hpcCBsaXN0IGhhcm5lc3MuXG4vKiogUG9zc2libGUgaGFybmVzc2VzIG9mIGNvbnRyb2xzIHdoaWNoIGNhbiBiZSBib3VuZCB0byBhIGZvcm0tZmllbGQuICovXG5leHBvcnQgdHlwZSBGb3JtRmllbGRDb250cm9sSGFybmVzcyA9IE1hdElucHV0SGFybmVzc3xNYXRTZWxlY3RIYXJuZXNzO1xuXG4vKiogSGFybmVzcyBmb3IgaW50ZXJhY3Rpbmcgd2l0aCBhIHN0YW5kYXJkIE1hdGVyaWFsIGZvcm0tZmllbGQncyBpbiB0ZXN0cy4gKi9cbmV4cG9ydCBjbGFzcyBNYXRGb3JtRmllbGRIYXJuZXNzIGV4dGVuZHMgQ29tcG9uZW50SGFybmVzcyB7XG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1mb3JtLWZpZWxkJztcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3IgYSBgTWF0Rm9ybUZpZWxkSGFybmVzc2AgdGhhdCBtZWV0c1xuICAgKiBjZXJ0YWluIGNyaXRlcmlhLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBmaWx0ZXJpbmcgd2hpY2ggZm9ybSBmaWVsZCBpbnN0YW5jZXMgYXJlIGNvbnNpZGVyZWQgYSBtYXRjaC5cbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBGb3JtRmllbGRIYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXRGb3JtRmllbGRIYXJuZXNzPiB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdEZvcm1GaWVsZEhhcm5lc3MsIG9wdGlvbnMpXG4gICAgICAuYWRkT3B0aW9uKCdmbG9hdGluZ0xhYmVsVGV4dCcsIG9wdGlvbnMuZmxvYXRpbmdMYWJlbFRleHQsIGFzeW5jIChoYXJuZXNzLCB0ZXh0KSA9PlxuICAgICAgICAgIEhhcm5lc3NQcmVkaWNhdGUuc3RyaW5nTWF0Y2hlcyhhd2FpdCBoYXJuZXNzLmdldExhYmVsKCksIHRleHQpKVxuICAgICAgLmFkZE9wdGlvbignaGFzRXJyb3JzJywgb3B0aW9ucy5oYXNFcnJvcnMsIGFzeW5jIChoYXJuZXNzLCBoYXNFcnJvcnMpID0+XG4gICAgICAgICAgYXdhaXQgaGFybmVzcy5oYXNFcnJvcnMoKSA9PT0gaGFzRXJyb3JzKTtcbiAgfVxuXG4gIHByaXZhdGUgX3ByZWZpeENvbnRhaW5lciA9IHRoaXMubG9jYXRvckZvck9wdGlvbmFsKCcubWF0LWZvcm0tZmllbGQtcHJlZml4Jyk7XG4gIHByaXZhdGUgX3N1ZmZpeENvbnRhaW5lciA9IHRoaXMubG9jYXRvckZvck9wdGlvbmFsKCcubWF0LWZvcm0tZmllbGQtc3VmZml4Jyk7XG4gIHByaXZhdGUgX2xhYmVsID0gdGhpcy5sb2NhdG9yRm9yT3B0aW9uYWwoJy5tYXQtZm9ybS1maWVsZC1sYWJlbCcpO1xuICBwcml2YXRlIF9lcnJvcnMgPSB0aGlzLmxvY2F0b3JGb3JBbGwoJy5tYXQtZXJyb3InKTtcbiAgcHJpdmF0ZSBfaGludHMgPSB0aGlzLmxvY2F0b3JGb3JBbGwoJ21hdC1oaW50LCAubWF0LWhpbnQnKTtcblxuICBwcml2YXRlIF9pbnB1dENvbnRyb2wgPSB0aGlzLmxvY2F0b3JGb3JPcHRpb25hbChNYXRJbnB1dEhhcm5lc3MpO1xuICBwcml2YXRlIF9zZWxlY3RDb250cm9sID0gdGhpcy5sb2NhdG9yRm9yT3B0aW9uYWwoTWF0U2VsZWN0SGFybmVzcyk7XG5cbiAgLyoqIEdldHMgdGhlIGFwcGVhcmFuY2Ugb2YgdGhlIGZvcm0tZmllbGQuICovXG4gIGFzeW5jIGdldEFwcGVhcmFuY2UoKTogUHJvbWlzZTwnbGVnYWN5J3wnc3RhbmRhcmQnfCdmaWxsJ3wnb3V0bGluZSc+IHtcbiAgICBjb25zdCBob3N0Q2xhc3NlcyA9IGF3YWl0IChhd2FpdCB0aGlzLmhvc3QoKSkuZ2V0QXR0cmlidXRlKCdjbGFzcycpO1xuICAgIGlmIChob3N0Q2xhc3NlcyAhPT0gbnVsbCkge1xuICAgICAgY29uc3QgYXBwZWFyYW5jZU1hdGNoID1cbiAgICAgICAgICBob3N0Q2xhc3Nlcy5tYXRjaCgvbWF0LWZvcm0tZmllbGQtYXBwZWFyYW5jZS0obGVnYWN5fHN0YW5kYXJkfGZpbGx8b3V0bGluZSkoPzokfCApLyk7XG4gICAgICBpZiAoYXBwZWFyYW5jZU1hdGNoKSB7XG4gICAgICAgIHJldHVybiBhcHBlYXJhbmNlTWF0Y2hbMV0gYXMgJ2xlZ2FjeScgfCAnc3RhbmRhcmQnIHwgJ2ZpbGwnIHwgJ291dGxpbmUnO1xuICAgICAgfVxuICAgIH1cbiAgICB0aHJvdyBFcnJvcignQ291bGQgbm90IGRldGVybWluZSBhcHBlYXJhbmNlIG9mIGZvcm0tZmllbGQuJyk7XG4gIH1cblxuICAvKipcbiAgICogR2V0cyB0aGUgaGFybmVzcyBvZiB0aGUgY29udHJvbCB0aGF0IGlzIGJvdW5kIHRvIHRoZSBmb3JtLWZpZWxkLiBPbmx5XG4gICAqIGRlZmF1bHQgY29udHJvbHMgc3VjaCBhcyBcIk1hdElucHV0SGFybmVzc1wiIGFuZCBcIk1hdFNlbGVjdEhhcm5lc3NcIiBhcmVcbiAgICogc3VwcG9ydGVkLlxuICAgKi9cbiAgYXN5bmMgZ2V0Q29udHJvbCgpOiBQcm9taXNlPEZvcm1GaWVsZENvbnRyb2xIYXJuZXNzfG51bGw+O1xuXG4gIC8qKlxuICAgKiBHZXRzIHRoZSBoYXJuZXNzIG9mIHRoZSBjb250cm9sIHRoYXQgaXMgYm91bmQgdG8gdGhlIGZvcm0tZmllbGQuIFNlYXJjaGVzXG4gICAqIGZvciBhIGNvbnRyb2wgdGhhdCBtYXRjaGVzIHRoZSBzcGVjaWZpZWQgaGFybmVzcyB0eXBlLlxuICAgKi9cbiAgYXN5bmMgZ2V0Q29udHJvbDxYIGV4dGVuZHMgTWF0Rm9ybUZpZWxkQ29udHJvbEhhcm5lc3M+KHR5cGU6IENvbXBvbmVudEhhcm5lc3NDb25zdHJ1Y3RvcjxYPik6XG4gICAgICBQcm9taXNlPFh8bnVsbD47XG5cbiAgLyoqXG4gICAqIEdldHMgdGhlIGhhcm5lc3Mgb2YgdGhlIGNvbnRyb2wgdGhhdCBpcyBib3VuZCB0byB0aGUgZm9ybS1maWVsZC4gU2VhcmNoZXNcbiAgICogZm9yIGEgY29udHJvbCB0aGF0IG1hdGNoZXMgdGhlIHNwZWNpZmllZCBoYXJuZXNzIHByZWRpY2F0ZS5cbiAgICovXG4gIGFzeW5jIGdldENvbnRyb2w8WCBleHRlbmRzIE1hdEZvcm1GaWVsZENvbnRyb2xIYXJuZXNzPih0eXBlOiBIYXJuZXNzUHJlZGljYXRlPFg+KTpcbiAgICAgIFByb21pc2U8WHxudWxsPjtcblxuICAvLyBJbXBsZW1lbnRhdGlvbiBvZiB0aGUgXCJnZXRDb250cm9sXCIgbWV0aG9kIG92ZXJsb2FkIHNpZ25hdHVyZXMuXG4gIGFzeW5jIGdldENvbnRyb2w8WCBleHRlbmRzIE1hdEZvcm1GaWVsZENvbnRyb2xIYXJuZXNzPih0eXBlPzogSGFybmVzc1F1ZXJ5PFg+KSB7XG4gICAgaWYgKHR5cGUpIHtcbiAgICAgIHJldHVybiB0aGlzLmxvY2F0b3JGb3JPcHRpb25hbCh0eXBlKSgpO1xuICAgIH1cbiAgICBjb25zdCBob3N0RWwgPSBhd2FpdCB0aGlzLmhvc3QoKTtcbiAgICBjb25zdCBbaXNJbnB1dCwgaXNTZWxlY3RdID0gYXdhaXQgUHJvbWlzZS5hbGwoW1xuICAgICAgaG9zdEVsLmhhc0NsYXNzKCdtYXQtZm9ybS1maWVsZC10eXBlLW1hdC1pbnB1dCcpLFxuICAgICAgaG9zdEVsLmhhc0NsYXNzKCdtYXQtZm9ybS1maWVsZC10eXBlLW1hdC1zZWxlY3QnKSxcbiAgICBdKTtcbiAgICBpZiAoaXNJbnB1dCkge1xuICAgICAgcmV0dXJuIHRoaXMuX2lucHV0Q29udHJvbCgpO1xuICAgIH0gZWxzZSBpZiAoaXNTZWxlY3QpIHtcbiAgICAgIHJldHVybiB0aGlzLl9zZWxlY3RDb250cm9sKCk7XG4gICAgfVxuICAgIHJldHVybiBudWxsO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGZvcm0tZmllbGQgaGFzIGEgbGFiZWwuICovXG4gIGFzeW5jIGhhc0xhYmVsKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmhhc0NsYXNzKCdtYXQtZm9ybS1maWVsZC1oYXMtbGFiZWwnKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSBsYWJlbCBvZiB0aGUgZm9ybS1maWVsZC4gKi9cbiAgYXN5bmMgZ2V0TGFiZWwoKTogUHJvbWlzZTxzdHJpbmd8bnVsbD4ge1xuICAgIGNvbnN0IGxhYmVsRWwgPSBhd2FpdCB0aGlzLl9sYWJlbCgpO1xuICAgIHJldHVybiBsYWJlbEVsID8gbGFiZWxFbC50ZXh0KCkgOiBudWxsO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGZvcm0tZmllbGQgaGFzIGVycm9ycy4gKi9cbiAgYXN5bmMgaGFzRXJyb3JzKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5nZXRUZXh0RXJyb3JzKCkpLmxlbmd0aCA+IDA7XG4gIH1cblxuICAvKiogV2hldGhlciB0aGUgbGFiZWwgaXMgY3VycmVudGx5IGZsb2F0aW5nLiAqL1xuICBhc3luYyBpc0xhYmVsRmxvYXRpbmcoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgY29uc3QgW2hhc0xhYmVsLCBzaG91bGRGbG9hdF0gPSBhd2FpdCBQcm9taXNlLmFsbChbXG4gICAgICB0aGlzLmhhc0xhYmVsKCksXG4gICAgICAoYXdhaXQgdGhpcy5ob3N0KCkpLmhhc0NsYXNzKCdtYXQtZm9ybS1maWVsZC1zaG91bGQtZmxvYXQnKSxcbiAgICBdKTtcbiAgICAvLyBJZiB0aGVyZSBpcyBubyBsYWJlbCwgdGhlIGxhYmVsIGNvbmNlcHR1YWxseSBjYW4gbmV2ZXIgZmxvYXQuIFRoZSBgc2hvdWxkLWZsb2F0YCBjbGFzc1xuICAgIC8vIGlzIGp1c3QgYWx3YXlzIHNldCByZWdhcmRsZXNzIG9mIHdoZXRoZXIgdGhlIGxhYmVsIGlzIGRpc3BsYXllZCBvciBub3QuXG4gICAgcmV0dXJuIGhhc0xhYmVsICYmIHNob3VsZEZsb2F0O1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGZvcm0tZmllbGQgaXMgZGlzYWJsZWQuICovXG4gIGFzeW5jIGlzRGlzYWJsZWQoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuaGFzQ2xhc3MoJ21hdC1mb3JtLWZpZWxkLWRpc2FibGVkJyk7XG4gIH1cblxuICAvKiogV2hldGhlciB0aGUgZm9ybS1maWVsZCBpcyBjdXJyZW50bHkgYXV0b2ZpbGxlZC4gKi9cbiAgYXN5bmMgaXNBdXRvZmlsbGVkKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmhhc0NsYXNzKCdtYXQtZm9ybS1maWVsZC1hdXRvZmlsbGVkJyk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgdGhlbWUgY29sb3Igb2YgdGhlIGZvcm0tZmllbGQuICovXG4gIGFzeW5jIGdldFRoZW1lQ29sb3IoKTogUHJvbWlzZTwncHJpbWFyeSd8J2FjY2VudCd8J3dhcm4nPiB7XG4gICAgY29uc3QgaG9zdEVsID0gYXdhaXQgdGhpcy5ob3N0KCk7XG4gICAgY29uc3QgW2lzQWNjZW50LCBpc1dhcm5dID1cbiAgICAgICAgYXdhaXQgUHJvbWlzZS5hbGwoW2hvc3RFbC5oYXNDbGFzcygnbWF0LWFjY2VudCcpLCBob3N0RWwuaGFzQ2xhc3MoJ21hdC13YXJuJyldKTtcbiAgICBpZiAoaXNBY2NlbnQpIHtcbiAgICAgIHJldHVybiAnYWNjZW50JztcbiAgICB9IGVsc2UgaWYgKGlzV2Fybikge1xuICAgICAgcmV0dXJuICd3YXJuJztcbiAgICB9XG4gICAgcmV0dXJuICdwcmltYXJ5JztcbiAgfVxuXG4gIC8qKiBHZXRzIGVycm9yIG1lc3NhZ2VzIHdoaWNoIGFyZSBjdXJyZW50bHkgZGlzcGxheWVkIGluIHRoZSBmb3JtLWZpZWxkLiAqL1xuICBhc3luYyBnZXRUZXh0RXJyb3JzKCk6IFByb21pc2U8c3RyaW5nW10+IHtcbiAgICByZXR1cm4gUHJvbWlzZS5hbGwoKGF3YWl0IHRoaXMuX2Vycm9ycygpKS5tYXAoZSA9PiBlLnRleHQoKSkpO1xuICB9XG5cbiAgLyoqIEdldHMgaGludCBtZXNzYWdlcyB3aGljaCBhcmUgY3VycmVudGx5IGRpc3BsYXllZCBpbiB0aGUgZm9ybS1maWVsZC4gKi9cbiAgYXN5bmMgZ2V0VGV4dEhpbnRzKCk6IFByb21pc2U8c3RyaW5nW10+IHtcbiAgICByZXR1cm4gUHJvbWlzZS5hbGwoKGF3YWl0IHRoaXMuX2hpbnRzKCkpLm1hcChlID0+IGUudGV4dCgpKSk7XG4gIH1cblxuICAvKipcbiAgICogR2V0cyBhIHJlZmVyZW5jZSB0byB0aGUgY29udGFpbmVyIGVsZW1lbnQgd2hpY2ggY29udGFpbnMgYWxsIHByb2plY3RlZFxuICAgKiBwcmVmaXhlcyBvZiB0aGUgZm9ybS1maWVsZC5cbiAgICovXG4gIGFzeW5jIGdldEhhcm5lc3NMb2FkZXJGb3JQcmVmaXgoKTogUHJvbWlzZTxUZXN0RWxlbWVudHxudWxsPiB7XG4gICAgcmV0dXJuIHRoaXMuX3ByZWZpeENvbnRhaW5lcigpO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgYSByZWZlcmVuY2UgdG8gdGhlIGNvbnRhaW5lciBlbGVtZW50IHdoaWNoIGNvbnRhaW5zIGFsbCBwcm9qZWN0ZWRcbiAgICogc3VmZml4ZXMgb2YgdGhlIGZvcm0tZmllbGQuXG4gICAqL1xuICBhc3luYyBnZXRIYXJuZXNzTG9hZGVyRm9yU3VmZml4KCk6IFByb21pc2U8VGVzdEVsZW1lbnR8bnVsbD4ge1xuICAgIHJldHVybiB0aGlzLl9zdWZmaXhDb250YWluZXIoKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBXaGV0aGVyIHRoZSBmb3JtIGNvbnRyb2wgaGFzIGJlZW4gdG91Y2hlZC4gUmV0dXJucyBcIm51bGxcIlxuICAgKiBpZiBubyBmb3JtIGNvbnRyb2wgaXMgc2V0IHVwLlxuICAgKi9cbiAgYXN5bmMgaXNDb250cm9sVG91Y2hlZCgpOiBQcm9taXNlPGJvb2xlYW58bnVsbD4ge1xuICAgIGlmICghYXdhaXQgdGhpcy5faGFzRm9ybUNvbnRyb2woKSkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmhhc0NsYXNzKCduZy10b3VjaGVkJyk7XG4gIH1cblxuICAvKipcbiAgICogV2hldGhlciB0aGUgZm9ybSBjb250cm9sIGlzIGRpcnR5LiBSZXR1cm5zIFwibnVsbFwiXG4gICAqIGlmIG5vIGZvcm0gY29udHJvbCBpcyBzZXQgdXAuXG4gICAqL1xuICBhc3luYyBpc0NvbnRyb2xEaXJ0eSgpOiBQcm9taXNlPGJvb2xlYW58bnVsbD4ge1xuICAgIGlmICghYXdhaXQgdGhpcy5faGFzRm9ybUNvbnRyb2woKSkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmhhc0NsYXNzKCduZy1kaXJ0eScpO1xuICB9XG5cbiAgLyoqXG4gICAqIFdoZXRoZXIgdGhlIGZvcm0gY29udHJvbCBpcyB2YWxpZC4gUmV0dXJucyBcIm51bGxcIlxuICAgKiBpZiBubyBmb3JtIGNvbnRyb2wgaXMgc2V0IHVwLlxuICAgKi9cbiAgYXN5bmMgaXNDb250cm9sVmFsaWQoKTogUHJvbWlzZTxib29sZWFufG51bGw+IHtcbiAgICBpZiAoIWF3YWl0IHRoaXMuX2hhc0Zvcm1Db250cm9sKCkpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5oYXNDbGFzcygnbmctdmFsaWQnKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBXaGV0aGVyIHRoZSBmb3JtIGNvbnRyb2wgaXMgcGVuZGluZyB2YWxpZGF0aW9uLiBSZXR1cm5zIFwibnVsbFwiXG4gICAqIGlmIG5vIGZvcm0gY29udHJvbCBpcyBzZXQgdXAuXG4gICAqL1xuICBhc3luYyBpc0NvbnRyb2xQZW5kaW5nKCk6IFByb21pc2U8Ym9vbGVhbnxudWxsPiB7XG4gICAgaWYgKCFhd2FpdCB0aGlzLl9oYXNGb3JtQ29udHJvbCgpKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuaGFzQ2xhc3MoJ25nLXBlbmRpbmcnKTtcbiAgfVxuXG4gIC8qKiBDaGVja3Mgd2hldGhlciB0aGUgZm9ybS1maWVsZCBjb250cm9sIGhhcyBzZXQgdXAgYSBmb3JtIGNvbnRyb2wuICovXG4gIHByaXZhdGUgYXN5bmMgX2hhc0Zvcm1Db250cm9sKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIGNvbnN0IGhvc3RFbCA9IGF3YWl0IHRoaXMuaG9zdCgpO1xuICAgIC8vIElmIG5vIGZvcm0gXCJOZ0NvbnRyb2xcIiBpcyBib3VuZCB0byB0aGUgZm9ybS1maWVsZCBjb250cm9sLCB0aGUgZm9ybS1maWVsZFxuICAgIC8vIGlzIG5vdCBhYmxlIHRvIGZvcndhcmQgYW55IGNvbnRyb2wgc3RhdHVzIGNsYXNzZXMuIFRoZXJlZm9yZSBpZiBlaXRoZXIgdGhlXG4gICAgLy8gXCJuZy10b3VjaGVkXCIgb3IgXCJuZy11bnRvdWNoZWRcIiBjbGFzcyBpcyBzZXQsIHdlIGtub3cgdGhhdCBpdCBoYXMgYSBmb3JtIGNvbnRyb2xcbiAgICBjb25zdCBbaXNUb3VjaGVkLCBpc1VudG91Y2hlZF0gPVxuICAgICAgICBhd2FpdCBQcm9taXNlLmFsbChbaG9zdEVsLmhhc0NsYXNzKCduZy10b3VjaGVkJyksIGhvc3RFbC5oYXNDbGFzcygnbmctdW50b3VjaGVkJyldKTtcbiAgICByZXR1cm4gaXNUb3VjaGVkIHx8IGlzVW50b3VjaGVkO1xuICB9XG59XG4iXX0=