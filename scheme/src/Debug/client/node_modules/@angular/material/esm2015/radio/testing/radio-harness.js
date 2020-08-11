/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { coerceBooleanProperty } from '@angular/cdk/coercion';
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
/** Harness for interacting with a standard mat-radio-group in tests. */
let MatRadioGroupHarness = /** @class */ (() => {
    class MatRadioGroupHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatRadioGroupHarness` that meets
         * certain criteria.
         * @param options Options for filtering which radio group instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatRadioGroupHarness, options)
                .addOption('name', options.name, this._checkRadioGroupName);
        }
        /** Gets the name of the radio-group. */
        getName() {
            return __awaiter(this, void 0, void 0, function* () {
                const hostName = yield this._getGroupNameFromHost();
                // It's not possible to always determine the "name" of a radio-group by reading
                // the attribute. This is because the radio-group does not set the "name" as an
                // element attribute if the "name" value is set through a binding.
                if (hostName !== null) {
                    return hostName;
                }
                // In case we couldn't determine the "name" of a radio-group by reading the
                // "name" attribute, we try to determine the "name" of the group by going
                // through all radio buttons.
                const radioNames = yield this._getNamesFromRadioButtons();
                if (!radioNames.length) {
                    return null;
                }
                if (!this._checkRadioNamesInGroupEqual(radioNames)) {
                    throw Error('Radio buttons in radio-group have mismatching names.');
                }
                return radioNames[0];
            });
        }
        /** Gets the id of the radio-group. */
        getId() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getProperty('id');
            });
        }
        /** Gets the checked radio-button in a radio-group. */
        getCheckedRadioButton() {
            return __awaiter(this, void 0, void 0, function* () {
                for (let radioButton of yield this.getRadioButtons()) {
                    if (yield radioButton.isChecked()) {
                        return radioButton;
                    }
                }
                return null;
            });
        }
        /** Gets the checked value of the radio-group. */
        getCheckedValue() {
            return __awaiter(this, void 0, void 0, function* () {
                const checkedRadio = yield this.getCheckedRadioButton();
                if (!checkedRadio) {
                    return null;
                }
                return checkedRadio.getValue();
            });
        }
        /**
         * Gets a list of radio buttons which are part of the radio-group.
         * @param filter Optionally filters which radio buttons are included.
         */
        getRadioButtons(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatRadioButtonHarness.with(filter))();
            });
        }
        /**
         * Checks a radio button in this group.
         * @param filter An optional filter to apply to the child radio buttons. The first tab matching
         *     the filter will be selected.
         */
        checkRadioButton(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                const radioButtons = yield this.getRadioButtons(filter);
                if (!radioButtons.length) {
                    throw Error(`Could not find radio button matching ${JSON.stringify(filter)}`);
                }
                return radioButtons[0].check();
            });
        }
        /** Gets the name attribute of the host element. */
        _getGroupNameFromHost() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('name');
            });
        }
        /** Gets a list of the name attributes of all child radio buttons. */
        _getNamesFromRadioButtons() {
            return __awaiter(this, void 0, void 0, function* () {
                const groupNames = [];
                for (let radio of yield this.getRadioButtons()) {
                    const radioName = yield radio.getName();
                    if (radioName !== null) {
                        groupNames.push(radioName);
                    }
                }
                return groupNames;
            });
        }
        /** Checks if the specified radio names are all equal. */
        _checkRadioNamesInGroupEqual(radioNames) {
            let groupName = null;
            for (let radioName of radioNames) {
                if (groupName === null) {
                    groupName = radioName;
                }
                else if (groupName !== radioName) {
                    return false;
                }
            }
            return true;
        }
        /**
         * Checks if a radio-group harness has the given name. Throws if a radio-group with
         * matching name could be found but has mismatching radio-button names.
         */
        static _checkRadioGroupName(harness, name) {
            return __awaiter(this, void 0, void 0, function* () {
                // Check if there is a radio-group which has the "name" attribute set
                // to the expected group name. It's not possible to always determine
                // the "name" of a radio-group by reading the attribute. This is because
                // the radio-group does not set the "name" as an element attribute if the
                // "name" value is set through a binding.
                if ((yield harness._getGroupNameFromHost()) === name) {
                    return true;
                }
                // Check if there is a group with radio-buttons that all have the same
                // expected name. This implies that the group has the given name. It's
                // not possible to always determine the name of a radio-group through
                // the attribute because there is
                const radioNames = yield harness._getNamesFromRadioButtons();
                if (radioNames.indexOf(name) === -1) {
                    return false;
                }
                if (!harness._checkRadioNamesInGroupEqual(radioNames)) {
                    throw Error(`The locator found a radio-group with name "${name}", but some ` +
                        `radio-button's within the group have mismatching names, which is invalid.`);
                }
                return true;
            });
        }
    }
    /** The selector for the host element of a `MatRadioGroup` instance. */
    MatRadioGroupHarness.hostSelector = 'mat-radio-group';
    return MatRadioGroupHarness;
})();
export { MatRadioGroupHarness };
/** Harness for interacting with a standard mat-radio-button in tests. */
let MatRadioButtonHarness = /** @class */ (() => {
    class MatRadioButtonHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._textLabel = this.locatorFor('.mat-radio-label-content');
            this._clickLabel = this.locatorFor('.mat-radio-label');
            this._input = this.locatorFor('input');
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatRadioButtonHarness` that meets
         * certain criteria.
         * @param options Options for filtering which radio button instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatRadioButtonHarness, options)
                .addOption('label', options.label, (harness, label) => HarnessPredicate.stringMatches(harness.getLabelText(), label))
                .addOption('name', options.name, (harness, name) => __awaiter(this, void 0, void 0, function* () { return (yield harness.getName()) === name; }));
        }
        /** Whether the radio-button is checked. */
        isChecked() {
            return __awaiter(this, void 0, void 0, function* () {
                const checked = (yield this._input()).getProperty('checked');
                return coerceBooleanProperty(yield checked);
            });
        }
        /** Whether the radio-button is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                const disabled = (yield this._input()).getAttribute('disabled');
                return coerceBooleanProperty(yield disabled);
            });
        }
        /** Whether the radio-button is required. */
        isRequired() {
            return __awaiter(this, void 0, void 0, function* () {
                const required = (yield this._input()).getAttribute('required');
                return coerceBooleanProperty(yield required);
            });
        }
        /** Gets the radio-button's name. */
        getName() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._input()).getAttribute('name');
            });
        }
        /** Gets the radio-button's id. */
        getId() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getProperty('id');
            });
        }
        /**
         * Gets the value of the radio-button. The radio-button value will be converted to a string.
         *
         * Note: This means that for radio-button's with an object as a value `[object Object]` is
         * intentionally returned.
         */
        getValue() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._input()).getProperty('value');
            });
        }
        /** Gets the radio-button's label text. */
        getLabelText() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._textLabel()).text();
            });
        }
        /** Focuses the radio-button. */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._input()).focus();
            });
        }
        /** Blurs the radio-button. */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._input()).blur();
            });
        }
        /**
         * Puts the radio-button in a checked state by clicking it if it is currently unchecked,
         * or doing nothing if it is already checked.
         */
        check() {
            return __awaiter(this, void 0, void 0, function* () {
                if (!(yield this.isChecked())) {
                    return (yield this._clickLabel()).click();
                }
            });
        }
    }
    /** The selector for the host element of a `MatRadioButton` instance. */
    MatRadioButtonHarness.hostSelector = 'mat-radio-button';
    return MatRadioButtonHarness;
})();
export { MatRadioButtonHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicmFkaW8taGFybmVzcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9yYWRpby90ZXN0aW5nL3JhZGlvLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxxQkFBcUIsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQzVELE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBR3hFLHdFQUF3RTtBQUN4RTtJQUFBLE1BQWEsb0JBQXFCLFNBQVEsZ0JBQWdCO1FBSXhEOzs7OztXQUtHO1FBQ0gsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUFvQyxFQUFFO1lBQ2hELE9BQU8sSUFBSSxnQkFBZ0IsQ0FBQyxvQkFBb0IsRUFBRSxPQUFPLENBQUM7aUJBQ3JELFNBQVMsQ0FBQyxNQUFNLEVBQUUsT0FBTyxDQUFDLElBQUksRUFBRSxJQUFJLENBQUMsb0JBQW9CLENBQUMsQ0FBQztRQUNsRSxDQUFDO1FBRUQsd0NBQXdDO1FBQ2xDLE9BQU87O2dCQUNYLE1BQU0sUUFBUSxHQUFHLE1BQU0sSUFBSSxDQUFDLHFCQUFxQixFQUFFLENBQUM7Z0JBQ3BELCtFQUErRTtnQkFDL0UsK0VBQStFO2dCQUMvRSxrRUFBa0U7Z0JBQ2xFLElBQUksUUFBUSxLQUFLLElBQUksRUFBRTtvQkFDckIsT0FBTyxRQUFRLENBQUM7aUJBQ2pCO2dCQUNELDJFQUEyRTtnQkFDM0UseUVBQXlFO2dCQUN6RSw2QkFBNkI7Z0JBQzdCLE1BQU0sVUFBVSxHQUFHLE1BQU0sSUFBSSxDQUFDLHlCQUF5QixFQUFFLENBQUM7Z0JBQzFELElBQUksQ0FBQyxVQUFVLENBQUMsTUFBTSxFQUFFO29CQUN0QixPQUFPLElBQUksQ0FBQztpQkFDYjtnQkFDRCxJQUFJLENBQUMsSUFBSSxDQUFDLDRCQUE0QixDQUFDLFVBQVUsQ0FBQyxFQUFF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UUFFRCxrQ0FBa0M7UUFDNUIsS0FBSzs7Z0JBQ1QsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsV0FBVyxDQUFDLElBQUksQ0FBQyxDQUFDO1lBQy9DLENBQUM7U0FBQTtRQUVEOzs7OztXQUtHO1FBQ0csUUFBUTs7Z0JBQ1osT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLE1BQU0sRUFBRSxDQUFDLENBQUMsV0FBVyxDQUFDLE9BQU8sQ0FBQyxDQUFDO1lBQ3BELENBQUM7U0FBQTtRQUVELDBDQUEwQztRQUNwQyxZQUFZOztnQkFDaEIsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLFVBQVUsRUFBRSxDQUFDLENBQUMsSUFBSSxFQUFFLENBQUM7WUFDMUMsQ0FBQztTQUFBO1FBRUQsZ0NBQWdDO1FBQzFCLEtBQUs7O2dCQUNULE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxNQUFNLEVBQUUsQ0FBQyxDQUFDLEtBQUssRUFBRSxDQUFDO1lBQ3ZDLENBQUM7U0FBQTtRQUVELDhCQUE4QjtRQUN4QixJQUFJOztnQkFDUixPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUMsQ0FBQyxJQUFJLEVBQUUsQ0FBQztZQUN0QyxDQUFDO1NBQUE7UUFFRDs7O1dBR0c7UUFDRyxLQUFLOztnQkFDVCxJQUFJLENBQUMsQ0FBQyxNQUFNLElBQUksQ0FBQyxTQUFTLEVBQUUsQ0FBQyxFQUFFO29CQUM3QixPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsV0FBVyxFQUFFLENBQUMsQ0FBQyxLQUFLLEVBQUUsQ0FBQztpQkFDM0M7WUFDSCxDQUFDO1NBQUE7O0lBbkZELHdFQUF3RTtJQUNqRSxrQ0FBWSxHQUFHLGtCQUFrQixDQUFDO0lBbUYzQyw0QkFBQztLQUFBO1NBckZZLHFCQUFxQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge2NvZXJjZUJvb2xlYW5Qcm9wZXJ0eX0gZnJvbSAnQGFuZ3VsYXIvY2RrL2NvZXJjaW9uJztcbmltcG9ydCB7Q29tcG9uZW50SGFybmVzcywgSGFybmVzc1ByZWRpY2F0ZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3Rlc3RpbmcnO1xuaW1wb3J0IHtSYWRpb0J1dHRvbkhhcm5lc3NGaWx0ZXJzLCBSYWRpb0dyb3VwSGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vcmFkaW8taGFybmVzcy1maWx0ZXJzJztcblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBtYXQtcmFkaW8tZ3JvdXAgaW4gdGVzdHMuICovXG5leHBvcnQgY2xhc3MgTWF0UmFkaW9Hcm91cEhhcm5lc3MgZXh0ZW5kcyBDb21wb25lbnRIYXJuZXNzIHtcbiAgLyoqIFRoZSBzZWxlY3RvciBmb3IgdGhlIGhvc3QgZWxlbWVudCBvZiBhIGBNYXRSYWRpb0dyb3VwYCBpbnN0YW5jZS4gKi9cbiAgc3RhdGljIGhvc3RTZWxlY3RvciA9ICdtYXQtcmFkaW8tZ3JvdXAnO1xuXG4gIC8qKlxuICAgKiBHZXRzIGEgYEhhcm5lc3NQcmVkaWNhdGVgIHRoYXQgY2FuIGJlIHVzZWQgdG8gc2VhcmNoIGZvciBhIGBNYXRSYWRpb0dyb3VwSGFybmVzc2AgdGhhdCBtZWV0c1xuICAgKiBjZXJ0YWluIGNyaXRlcmlhLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBmaWx0ZXJpbmcgd2hpY2ggcmFkaW8gZ3JvdXAgaW5zdGFuY2VzIGFyZSBjb25zaWRlcmVkIGEgbWF0Y2guXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogUmFkaW9Hcm91cEhhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdFJhZGlvR3JvdXBIYXJuZXNzPiB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdFJhZGlvR3JvdXBIYXJuZXNzLCBvcHRpb25zKVxuICAgICAgICAuYWRkT3B0aW9uKCduYW1lJywgb3B0aW9ucy5uYW1lLCB0aGlzLl9jaGVja1JhZGlvR3JvdXBOYW1lKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSBuYW1lIG9mIHRoZSByYWRpby1ncm91cC4gKi9cbiAgYXN5bmMgZ2V0TmFtZSgpOiBQcm9taXNlPHN0cmluZ3xudWxsPiB7XG4gICAgY29uc3QgaG9zdE5hbWUgPSBhd2FpdCB0aGlzLl9nZXRHcm91cE5hbWVGcm9tSG9zdCgpO1xuICAgIC8vIEl0J3Mgbm90IHBvc3NpYmxlIHRvIGFsd2F5cyBkZXRlcm1pbmUgdGhlIFwibmFtZVwiIG9mIGEgcmFkaW8tZ3JvdXAgYnkgcmVhZGluZ1xuICAgIC8vIHRoZSBhdHRyaWJ1dGUuIFRoaXMgaXMgYmVjYXVzZSB0aGUgcmFkaW8tZ3JvdXAgZG9lcyBub3Qgc2V0IHRoZSBcIm5hbWVcIiBhcyBhblxuICAgIC8vIGVsZW1lbnQgYXR0cmlidXRlIGlmIHRoZSBcIm5hbWVcIiB2YWx1ZSBpcyBzZXQgdGhyb3VnaCBhIGJpbmRpbmcuXG4gICAgaWYgKGhvc3ROYW1lICE9PSBudWxsKSB7XG4gICAgICByZXR1cm4gaG9zdE5hbWU7XG4gICAgfVxuICAgIC8vIEluIGNhc2Ugd2UgY291bGRuJ3QgZGV0ZXJtaW5lIHRoZSBcIm5hbWVcIiBvZiBhIHJhZGlvLWdyb3VwIGJ5IHJlYWRpbmcgdGhlXG4gICAgLy8gXCJuYW1lXCIgYXR0cmlidXRlLCB3ZSB0cnkgdG8gZGV0ZXJtaW5lIHRoZSBcIm5hbWVcIiBvZiB0aGUgZ3JvdXAgYnkgZ29pbmdcbiAgICAvLyB0aHJvdWdoIGFsbCByYWRpbyBidXR0b25zLlxuICAgIGNvbnN0IHJhZGlvTmFtZXMgPSBhd2FpdCB0aGlzLl9nZXROYW1lc0Zyb21SYWRpb0J1dHRvbnMoKTtcbiAgICBpZiAoIXJhZGlvTmFtZXMubGVuZ3RoKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG4gICAgaWYgKCF0aGlzLl9jaGVja1JhZGlvTmFtZXNJbkdyb3VwRXF1YWwocmFkaW9OYW1lcykpIHtcbiAgICAgIHRocm93IEVycm9yKCdSYWRpbyBidXR0b25zIGluIHJhZGlvLWdyb3VwIGhhdmUgbWlzbWF0Y2hpbmcgbmFtZXMuJyk7XG4gICAgfVxuICAgIHJldHVybiByYWRpb05hbWVzWzBdITtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSBpZCBvZiB0aGUgcmFkaW8tZ3JvdXAuICovXG4gIGFzeW5jIGdldElkKCk6IFByb21pc2U8c3RyaW5nfG51bGw+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRQcm9wZXJ0eSgnaWQnKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSBjaGVja2VkIHJhZGlvLWJ1dHRvbiBpbiBhIHJhZGlvLWdyb3VwLiAqL1xuICBhc3luYyBnZXRDaGVja2VkUmFkaW9CdXR0b24oKTogUHJvbWlzZTxNYXRSYWRpb0J1dHRvbkhhcm5lc3N8bnVsbD4ge1xuICAgIGZvciAobGV0IHJhZGlvQnV0dG9uIG9mIGF3YWl0IHRoaXMuZ2V0UmFkaW9CdXR0b25zKCkpIHtcbiAgICAgIGlmIChhd2FpdCByYWRpb0J1dHRvbi5pc0NoZWNrZWQoKSkge1xuICAgICAgICByZXR1cm4gcmFkaW9CdXR0b247XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiBudWxsO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIGNoZWNrZWQgdmFsdWUgb2YgdGhlIHJhZGlvLWdyb3VwLiAqL1xuICBhc3luYyBnZXRDaGVja2VkVmFsdWUoKTogUHJvbWlzZTxzdHJpbmd8bnVsbD4ge1xuICAgIGNvbnN0IGNoZWNrZWRSYWRpbyA9IGF3YWl0IHRoaXMuZ2V0Q2hlY2tlZFJhZGlvQnV0dG9uKCk7XG4gICAgaWYgKCFjaGVja2VkUmFkaW8pIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cbiAgICByZXR1cm4gY2hlY2tlZFJhZGlvLmdldFZhbHVlKCk7XG4gIH1cblxuICAvKipcbiAgICogR2V0cyBhIGxpc3Qgb2YgcmFkaW8gYnV0dG9ucyB3aGljaCBhcmUgcGFydCBvZiB0aGUgcmFkaW8tZ3JvdXAuXG4gICAqIEBwYXJhbSBmaWx0ZXIgT3B0aW9uYWxseSBmaWx0ZXJzIHdoaWNoIHJhZGlvIGJ1dHRvbnMgYXJlIGluY2x1ZGVkLlxuICAgKi9cbiAgYXN5bmMgZ2V0UmFkaW9CdXR0b25zKGZpbHRlcjogUmFkaW9CdXR0b25IYXJuZXNzRmlsdGVycyA9IHt9KTogUHJvbWlzZTxNYXRSYWRpb0J1dHRvbkhhcm5lc3NbXT4ge1xuICAgIHJldHVybiB0aGlzLmxvY2F0b3JGb3JBbGwoTWF0UmFkaW9CdXR0b25IYXJuZXNzLndpdGgoZmlsdGVyKSkoKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBDaGVja3MgYSByYWRpbyBidXR0b24gaW4gdGhpcyBncm91cC5cbiAgICogQHBhcmFtIGZpbHRlciBBbiBvcHRpb25hbCBmaWx0ZXIgdG8gYXBwbHkgdG8gdGhlIGNoaWxkIHJhZGlvIGJ1dHRvbnMuIFRoZSBmaXJzdCB0YWIgbWF0Y2hpbmdcbiAgICogICAgIHRoZSBmaWx0ZXIgd2lsbCBiZSBzZWxlY3RlZC5cbiAgICovXG4gIGFzeW5jIGNoZWNrUmFkaW9CdXR0b24oZmlsdGVyOiBSYWRpb0J1dHRvbkhhcm5lc3NGaWx0ZXJzID0ge30pOiBQcm9taXNlPHZvaWQ+IHtcbiAgICBjb25zdCByYWRpb0J1dHRvbnMgPSBhd2FpdCB0aGlzLmdldFJhZGlvQnV0dG9ucyhmaWx0ZXIpO1xuICAgIGlmICghcmFkaW9CdXR0b25zLmxlbmd0aCkge1xuICAgICAgdGhyb3cgRXJyb3IoYENvdWxkIG5vdCBmaW5kIHJhZGlvIGJ1dHRvbiBtYXRjaGluZyAke0pTT04uc3RyaW5naWZ5KGZpbHRlcil9YCk7XG4gICAgfVxuICAgIHJldHVybiByYWRpb0J1dHRvbnNbMF0uY2hlY2soKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSBuYW1lIGF0dHJpYnV0ZSBvZiB0aGUgaG9zdCBlbGVtZW50LiAqL1xuICBwcml2YXRlIGFzeW5jIF9nZXRHcm91cE5hbWVGcm9tSG9zdCgpIHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ25hbWUnKTtcbiAgfVxuXG4gIC8qKiBHZXRzIGEgbGlzdCBvZiB0aGUgbmFtZSBhdHRyaWJ1dGVzIG9mIGFsbCBjaGlsZCByYWRpbyBidXR0b25zLiAqL1xuICBwcml2YXRlIGFzeW5jIF9nZXROYW1lc0Zyb21SYWRpb0J1dHRvbnMoKTogUHJvbWlzZTxzdHJpbmdbXT4ge1xuICAgIGNvbnN0IGdyb3VwTmFtZXM6IHN0cmluZ1tdID0gW107XG4gICAgZm9yIChsZXQgcmFkaW8gb2YgYXdhaXQgdGhpcy5nZXRSYWRpb0J1dHRvbnMoKSkge1xuICAgICAgY29uc3QgcmFkaW9OYW1lID0gYXdhaXQgcmFkaW8uZ2V0TmFtZSgpO1xuICAgICAgaWYgKHJhZGlvTmFtZSAhPT0gbnVsbCkge1xuICAgICAgICBncm91cE5hbWVzLnB1c2gocmFkaW9OYW1lKTtcbiAgICAgIH1cbiAgICB9XG4gICAgcmV0dXJuIGdyb3VwTmFtZXM7XG4gIH1cblxuICAvKiogQ2hlY2tzIGlmIHRoZSBzcGVjaWZpZWQgcmFkaW8gbmFtZXMgYXJlIGFsbCBlcXVhbC4gKi9cbiAgcHJpdmF0ZSBfY2hlY2tSYWRpb05hbWVzSW5Hcm91cEVxdWFsKHJhZGlvTmFtZXM6IHN0cmluZ1tdKTogYm9vbGVhbiB7XG4gICAgbGV0IGdyb3VwTmFtZTogc3RyaW5nfG51bGwgPSBudWxsO1xuICAgIGZvciAobGV0IHJhZGlvTmFtZSBvZiByYWRpb05hbWVzKSB7XG4gICAgICBpZiAoZ3JvdXBOYW1lID09PSBudWxsKSB7XG4gICAgICAgIGdyb3VwTmFtZSA9IHJhZGlvTmFtZTtcbiAgICAgIH0gZWxzZSBpZiAoZ3JvdXBOYW1lICE9PSByYWRpb05hbWUpIHtcbiAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gdHJ1ZTtcbiAgfVxuXG4gIC8qKlxuICAgKiBDaGVja3MgaWYgYSByYWRpby1ncm91cCBoYXJuZXNzIGhhcyB0aGUgZ2l2ZW4gbmFtZS4gVGhyb3dzIGlmIGEgcmFkaW8tZ3JvdXAgd2l0aFxuICAgKiBtYXRjaGluZyBuYW1lIGNvdWxkIGJlIGZvdW5kIGJ1dCBoYXMgbWlzbWF0Y2hpbmcgcmFkaW8tYnV0dG9uIG5hbWVzLlxuICAgKi9cbiAgcHJpdmF0ZSBzdGF0aWMgYXN5bmMgX2NoZWNrUmFkaW9Hcm91cE5hbWUoaGFybmVzczogTWF0UmFkaW9Hcm91cEhhcm5lc3MsIG5hbWU6IHN0cmluZykge1xuICAgIC8vIENoZWNrIGlmIHRoZXJlIGlzIGEgcmFkaW8tZ3JvdXAgd2hpY2ggaGFzIHRoZSBcIm5hbWVcIiBhdHRyaWJ1dGUgc2V0XG4gICAgLy8gdG8gdGhlIGV4cGVjdGVkIGdyb3VwIG5hbWUuIEl0J3Mgbm90IHBvc3NpYmxlIHRvIGFsd2F5cyBkZXRlcm1pbmVcbiAgICAvLyB0aGUgXCJuYW1lXCIgb2YgYSByYWRpby1ncm91cCBieSByZWFkaW5nIHRoZSBhdHRyaWJ1dGUuIFRoaXMgaXMgYmVjYXVzZVxuICAgIC8vIHRoZSByYWRpby1ncm91cCBkb2VzIG5vdCBzZXQgdGhlIFwibmFtZVwiIGFzIGFuIGVsZW1lbnQgYXR0cmlidXRlIGlmIHRoZVxuICAgIC8vIFwibmFtZVwiIHZhbHVlIGlzIHNldCB0aHJvdWdoIGEgYmluZGluZy5cbiAgICBpZiAoYXdhaXQgaGFybmVzcy5fZ2V0R3JvdXBOYW1lRnJvbUhvc3QoKSA9PT0gbmFtZSkge1xuICAgICAgcmV0dXJuIHRydWU7XG4gICAgfVxuICAgIC8vIENoZWNrIGlmIHRoZXJlIGlzIGEgZ3JvdXAgd2l0aCByYWRpby1idXR0b25zIHRoYXQgYWxsIGhhdmUgdGhlIHNhbWVcbiAgICAvLyBleHBlY3RlZCBuYW1lLiBUaGlzIGltcGxpZXMgdGhhdCB0aGUgZ3JvdXAgaGFzIHRoZSBnaXZlbiBuYW1lLiBJdCdzXG4gICAgLy8gbm90IHBvc3NpYmxlIHRvIGFsd2F5cyBkZXRlcm1pbmUgdGhlIG5hbWUgb2YgYSByYWRpby1ncm91cCB0aHJvdWdoXG4gICAgLy8gdGhlIGF0dHJpYnV0ZSBiZWNhdXNlIHRoZXJlIGlzXG4gICAgY29uc3QgcmFkaW9OYW1lcyA9IGF3YWl0IGhhcm5lc3MuX2dldE5hbWVzRnJvbVJhZGlvQnV0dG9ucygpO1xuICAgIGlmIChyYWRpb05hbWVzLmluZGV4T2YobmFtZSkgPT09IC0xKSB7XG4gICAgICByZXR1cm4gZmFsc2U7XG4gICAgfVxuICAgIGlmICghaGFybmVzcy5fY2hlY2tSYWRpb05hbWVzSW5Hcm91cEVxdWFsKHJhZGlvTmFtZXMpKSB7XG4gICAgICB0aHJvdyBFcnJvcihcbiAgICAgICAgICBgVGhlIGxvY2F0b3IgZm91bmQgYSByYWRpby1ncm91cCB3aXRoIG5hbWUgXCIke25hbWV9XCIsIGJ1dCBzb21lIGAgK1xuICAgICAgICAgIGByYWRpby1idXR0b24ncyB3aXRoaW4gdGhlIGdyb3VwIGhhdmUgbWlzbWF0Y2hpbmcgbmFtZXMsIHdoaWNoIGlzIGludmFsaWQuYCk7XG4gICAgfVxuICAgIHJldHVybiB0cnVlO1xuICB9XG59XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgbWF0LXJhZGlvLWJ1dHRvbiBpbiB0ZXN0cy4gKi9cbmV4cG9ydCBjbGFzcyBNYXRSYWRpb0J1dHRvbkhhcm5lc3MgZXh0ZW5kcyBDb21wb25lbnRIYXJuZXNzIHtcbiAgLyoqIFRoZSBzZWxlY3RvciBmb3IgdGhlIGhvc3QgZWxlbWVudCBvZiBhIGBNYXRSYWRpb0J1dHRvbmAgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnbWF0LXJhZGlvLWJ1dHRvbic7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYE1hdFJhZGlvQnV0dG9uSGFybmVzc2AgdGhhdCBtZWV0c1xuICAgKiBjZXJ0YWluIGNyaXRlcmlhLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBmaWx0ZXJpbmcgd2hpY2ggcmFkaW8gYnV0dG9uIGluc3RhbmNlcyBhcmUgY29uc2lkZXJlZCBhIG1hdGNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IFJhZGlvQnV0dG9uSGFybmVzc0ZpbHRlcnMgPSB7fSk6IEhhcm5lc3NQcmVkaWNhdGU8TWF0UmFkaW9CdXR0b25IYXJuZXNzPiB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdFJhZGlvQnV0dG9uSGFybmVzcywgb3B0aW9ucylcbiAgICAgICAgLmFkZE9wdGlvbihcbiAgICAgICAgICAgICdsYWJlbCcsIG9wdGlvbnMubGFiZWwsXG4gICAgICAgICAgICAoaGFybmVzcywgbGFiZWwpID0+IEhhcm5lc3NQcmVkaWNhdGUuc3RyaW5nTWF0Y2hlcyhoYXJuZXNzLmdldExhYmVsVGV4dCgpLCBsYWJlbCkpXG4gICAgICAgIC5hZGRPcHRpb24oXG4gICAgICAgICAgICAnbmFtZScsIG9wdGlvbnMubmFtZSwgYXN5bmMgKGhhcm5lc3MsIG5hbWUpID0+IChhd2FpdCBoYXJuZXNzLmdldE5hbWUoKSkgPT09IG5hbWUpO1xuICB9XG5cbiAgcHJpdmF0ZSBfdGV4dExhYmVsID0gdGhpcy5sb2NhdG9yRm9yKCcubWF0LXJhZGlvLWxhYmVsLWNvbnRlbnQnKTtcbiAgcHJpdmF0ZSBfY2xpY2tMYWJlbCA9IHRoaXMubG9jYXRvckZvcignLm1hdC1yYWRpby1sYWJlbCcpO1xuICBwcml2YXRlIF9pbnB1dCA9IHRoaXMubG9jYXRvckZvcignaW5wdXQnKTtcblxuICAvKiogV2hldGhlciB0aGUgcmFkaW8tYnV0dG9uIGlzIGNoZWNrZWQuICovXG4gIGFzeW5jIGlzQ2hlY2tlZCgpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICBjb25zdCBjaGVja2VkID0gKGF3YWl0IHRoaXMuX2lucHV0KCkpLmdldFByb3BlcnR5KCdjaGVja2VkJyk7XG4gICAgcmV0dXJuIGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eShhd2FpdCBjaGVja2VkKTtcbiAgfVxuXG4gIC8qKiBXaGV0aGVyIHRoZSByYWRpby1idXR0b24gaXMgZGlzYWJsZWQuICovXG4gIGFzeW5jIGlzRGlzYWJsZWQoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgY29uc3QgZGlzYWJsZWQgPSAoYXdhaXQgdGhpcy5faW5wdXQoKSkuZ2V0QXR0cmlidXRlKCdkaXNhYmxlZCcpO1xuICAgIHJldHVybiBjb2VyY2VCb29sZWFuUHJvcGVydHkoYXdhaXQgZGlzYWJsZWQpO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIHJhZGlvLWJ1dHRvbiBpcyByZXF1aXJlZC4gKi9cbiAgYXN5bmMgaXNSZXF1aXJlZCgpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICBjb25zdCByZXF1aXJlZCA9IChhd2FpdCB0aGlzLl9pbnB1dCgpKS5nZXRBdHRyaWJ1dGUoJ3JlcXVpcmVkJyk7XG4gICAgcmV0dXJuIGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eShhd2FpdCByZXF1aXJlZCk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgcmFkaW8tYnV0dG9uJ3MgbmFtZS4gKi9cbiAgYXN5bmMgZ2V0TmFtZSgpOiBQcm9taXNlPHN0cmluZ3xudWxsPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9pbnB1dCgpKS5nZXRBdHRyaWJ1dGUoJ25hbWUnKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSByYWRpby1idXR0b24ncyBpZC4gKi9cbiAgYXN5bmMgZ2V0SWQoKTogUHJvbWlzZTxzdHJpbmd8bnVsbD4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldFByb3BlcnR5KCdpZCcpO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgdGhlIHZhbHVlIG9mIHRoZSByYWRpby1idXR0b24uIFRoZSByYWRpby1idXR0b24gdmFsdWUgd2lsbCBiZSBjb252ZXJ0ZWQgdG8gYSBzdHJpbmcuXG4gICAqXG4gICAqIE5vdGU6IFRoaXMgbWVhbnMgdGhhdCBmb3IgcmFkaW8tYnV0dG9uJ3Mgd2l0aCBhbiBvYmplY3QgYXMgYSB2YWx1ZSBgW29iamVjdCBPYmplY3RdYCBpc1xuICAgKiBpbnRlbnRpb25hbGx5IHJldHVybmVkLlxuICAgKi9cbiAgYXN5bmMgZ2V0VmFsdWUoKTogUHJvbWlzZTxzdHJpbmd8bnVsbD4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5faW5wdXQoKSkuZ2V0UHJvcGVydHkoJ3ZhbHVlJyk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgcmFkaW8tYnV0dG9uJ3MgbGFiZWwgdGV4dC4gKi9cbiAgYXN5bmMgZ2V0TGFiZWxUZXh0KCk6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl90ZXh0TGFiZWwoKSkudGV4dCgpO1xuICB9XG5cbiAgLyoqIEZvY3VzZXMgdGhlIHJhZGlvLWJ1dHRvbi4gKi9cbiAgYXN5bmMgZm9jdXMoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9pbnB1dCgpKS5mb2N1cygpO1xuICB9XG5cbiAgLyoqIEJsdXJzIHRoZSByYWRpby1idXR0b24uICovXG4gIGFzeW5jIGJsdXIoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9pbnB1dCgpKS5ibHVyKCk7XG4gIH1cblxuICAvKipcbiAgICogUHV0cyB0aGUgcmFkaW8tYnV0dG9uIGluIGEgY2hlY2tlZCBzdGF0ZSBieSBjbGlja2luZyBpdCBpZiBpdCBpcyBjdXJyZW50bHkgdW5jaGVja2VkLFxuICAgKiBvciBkb2luZyBub3RoaW5nIGlmIGl0IGlzIGFscmVhZHkgY2hlY2tlZC5cbiAgICovXG4gIGFzeW5jIGNoZWNrKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIGlmICghKGF3YWl0IHRoaXMuaXNDaGVja2VkKCkpKSB7XG4gICAgICByZXR1cm4gKGF3YWl0IHRoaXMuX2NsaWNrTGFiZWwoKSkuY2xpY2soKTtcbiAgICB9XG4gIH1cbn1cbiJdfQ==