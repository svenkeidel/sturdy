/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Component, ChangeDetectionStrategy, ViewEncapsulation, Input, Optional, ContentChild, ChangeDetectorRef, Self, ElementRef, } from '@angular/core';
import { MatFormFieldControl, MatFormField } from '@angular/material/form-field';
import { DateAdapter } from '@angular/material/core';
import { ControlContainer } from '@angular/forms';
import { Subject, merge } from 'rxjs';
import { coerceBooleanProperty } from '@angular/cdk/coercion';
import { MatStartDate, MatEndDate, MAT_DATE_RANGE_INPUT_PARENT, } from './date-range-input-parts';
import { createMissingDateImplError } from './datepicker-errors';
import { MatDateRangePicker } from './date-range-picker';
let nextUniqueId = 0;
let MatDateRangeInput = /** @class */ (() => {
    class MatDateRangeInput {
        constructor(_changeDetectorRef, _elementRef, control, _dateAdapter, _formField) {
            this._changeDetectorRef = _changeDetectorRef;
            this._elementRef = _elementRef;
            this._dateAdapter = _dateAdapter;
            this._formField = _formField;
            /** Emits when the input's state has changed. */
            this.stateChanges = new Subject();
            /** Unique ID for the input. */
            this.id = `mat-date-range-input-${nextUniqueId++}`;
            /** Whether the control is focused. */
            this.focused = false;
            /** Name of the form control. */
            this.controlType = 'mat-date-range-input';
            this._groupDisabled = false;
            /** Value for the `aria-describedby` attribute of the inputs. */
            this._ariaDescribedBy = null;
            /** Value for the `aria-labelledby` attribute of the inputs. */
            this._ariaLabelledBy = null;
            /** Separator text to be shown between the inputs. */
            this.separator = '–';
            /** Start of the comparison range that should be shown in the calendar. */
            this.comparisonStart = null;
            /** End of the comparison range that should be shown in the calendar. */
            this.comparisonEnd = null;
            /** Emits when the input's disabled state changes. */
            this._disabledChange = new Subject();
            if (!_dateAdapter) {
                throw createMissingDateImplError('DateAdapter');
            }
            // TODO(crisbeto): remove `as any` after #18206 lands.
            this.ngControl = control;
            this._ariaLabelledBy = _formField ? _formField._labelId : null;
        }
        /** Current value of the range input. */
        get value() {
            return this._model ? this._model.selection : null;
        }
        /** Whether the control's label should float. */
        get shouldLabelFloat() {
            return this.focused || !this.empty;
        }
        /**
         * Implemented as a part of `MatFormFieldControl`.
         * Set the placeholder attribute on `matStartDate` and `matEndDate`.
         * @docs-private
         */
        get placeholder() {
            var _a, _b;
            const start = ((_a = this._startInput) === null || _a === void 0 ? void 0 : _a._getPlaceholder()) || '';
            const end = ((_b = this._endInput) === null || _b === void 0 ? void 0 : _b._getPlaceholder()) || '';
            return (start || end) ? `${start} ${this.separator} ${end}` : '';
        }
        /** The range picker that this input is associated with. */
        get rangePicker() { return this._rangePicker; }
        set rangePicker(rangePicker) {
            if (rangePicker) {
                this._model = rangePicker._registerInput(this);
                this._rangePicker = rangePicker;
                this._registerModel(this._model);
            }
        }
        /** Whether the input is required. */
        get required() { return !!this._required; }
        set required(value) {
            this._required = coerceBooleanProperty(value);
        }
        /** Function that can be used to filter out dates within the date range picker. */
        get dateFilter() { return this._dateFilter; }
        set dateFilter(value) {
            this._dateFilter = value;
            this._revalidate();
        }
        /** The minimum valid date. */
        get min() { return this._min; }
        set min(value) {
            this._min = this._getValidDateOrNull(this._dateAdapter.deserialize(value));
            this._revalidate();
        }
        /** The maximum valid date. */
        get max() { return this._max; }
        set max(value) {
            this._max = this._getValidDateOrNull(this._dateAdapter.deserialize(value));
            this._revalidate();
        }
        /** Whether the input is disabled. */
        get disabled() {
            return (this._startInput && this._endInput) ?
                (this._startInput.disabled && this._endInput.disabled) :
                this._groupDisabled;
        }
        set disabled(value) {
            const newValue = coerceBooleanProperty(value);
            if (newValue !== this._groupDisabled) {
                this._groupDisabled = newValue;
                this._disabledChange.next(this.disabled);
            }
        }
        /** Whether the input is in an error state. */
        get errorState() {
            if (this._startInput && this._endInput) {
                return this._startInput.errorState || this._endInput.errorState;
            }
            return false;
        }
        /** Whether the datepicker input is empty. */
        get empty() {
            const startEmpty = this._startInput ? this._startInput.isEmpty() : false;
            const endEmpty = this._endInput ? this._endInput.isEmpty() : false;
            return startEmpty && endEmpty;
        }
        /**
         * Implemented as a part of `MatFormFieldControl`.
         * @docs-private
         */
        setDescribedByIds(ids) {
            this._ariaDescribedBy = ids.length ? ids.join(' ') : null;
        }
        /**
         * Implemented as a part of `MatFormFieldControl`.
         * @docs-private
         */
        onContainerClick() {
            if (!this.focused && !this.disabled) {
                if (!this._model || !this._model.selection.start) {
                    this._startInput.focus();
                }
                else {
                    this._endInput.focus();
                }
            }
        }
        ngAfterContentInit() {
            if (!this._startInput) {
                throw Error('mat-date-range-input must contain a matStartDate input');
            }
            if (!this._endInput) {
                throw Error('mat-date-range-input must contain a matEndDate input');
            }
            if (this._model) {
                this._registerModel(this._model);
            }
            // We don't need to unsubscribe from this, because we
            // know that the input streams will be completed on destroy.
            merge(this._startInput._disabledChange, this._endInput._disabledChange).subscribe(() => {
                this._disabledChange.next(this.disabled);
            });
        }
        ngOnDestroy() {
            this.stateChanges.complete();
            this._disabledChange.unsubscribe();
        }
        /** Gets the date at which the calendar should start. */
        getStartValue() {
            return this.value ? this.value.start : null;
        }
        /** Gets the input's theme palette. */
        getThemePalette() {
            return this._formField ? this._formField.color : undefined;
        }
        /** Gets the element to which the calendar overlay should be attached. */
        getConnectedOverlayOrigin() {
            return this._formField ? this._formField.getConnectedOverlayOrigin() : this._elementRef;
        }
        /** Gets the value that is used to mirror the state input. */
        _getInputMirrorValue() {
            return this._startInput ? this._startInput.getMirrorValue() : '';
        }
        /** Whether the input placeholders should be hidden. */
        _shouldHidePlaceholders() {
            return this._startInput ? !this._startInput.isEmpty() : false;
        }
        /** Handles the value in one of the child inputs changing. */
        _handleChildValueChange() {
            this.stateChanges.next();
            this._changeDetectorRef.markForCheck();
        }
        /** Opens the date range picker associated with the input. */
        _openDatepicker() {
            if (this._rangePicker) {
                this._rangePicker.open();
            }
        }
        /** Whether the separate text should be hidden. */
        _shouldHideSeparator() {
            return (!this._formField || this._formField._hideControlPlaceholder()) && this.empty;
        }
        /**
         * @param obj The object to check.
         * @returns The given object if it is both a date instance and valid, otherwise null.
         */
        _getValidDateOrNull(obj) {
            return (this._dateAdapter.isDateInstance(obj) && this._dateAdapter.isValid(obj)) ? obj : null;
        }
        /** Re-runs the validators on the start/end inputs. */
        _revalidate() {
            if (this._startInput) {
                this._startInput._validatorOnChange();
            }
            if (this._endInput) {
                this._endInput._validatorOnChange();
            }
        }
        /** Registers the current date selection model with the start/end inputs. */
        _registerModel(model) {
            if (this._startInput) {
                this._startInput._registerModel(model);
            }
            if (this._endInput) {
                this._endInput._registerModel(model);
            }
        }
    }
    MatDateRangeInput.decorators = [
        { type: Component, args: [{
                    selector: 'mat-date-range-input',
                    template: "<div\n  class=\"mat-date-range-input-container\"\n  cdkMonitorSubtreeFocus\n  (cdkFocusChange)=\"focused = $event !== null\">\n  <div class=\"mat-date-range-input-start-wrapper\">\n    <ng-content select=\"input[matStartDate]\"></ng-content>\n    <span\n      class=\"mat-date-range-input-mirror\"\n      aria-hidden=\"true\">{{_getInputMirrorValue()}}</span>\n  </div>\n\n  <span\n    class=\"mat-date-range-input-separator\"\n    [class.mat-date-range-input-separator-hidden]=\"_shouldHideSeparator()\">{{separator}}</span>\n\n  <div class=\"mat-date-range-input-end-wrapper\">\n    <ng-content select=\"input[matEndDate]\"></ng-content>\n  </div>\n</div>\n\n",
                    exportAs: 'matDateRangeInput',
                    host: {
                        'class': 'mat-date-range-input',
                        '[class.mat-date-range-input-hide-placeholders]': '_shouldHidePlaceholders()',
                        '[attr.id]': 'null',
                        'role': 'group',
                        '[attr.aria-labelledby]': '_ariaLabelledBy',
                        '[attr.aria-describedby]': '_ariaDescribedBy',
                    },
                    changeDetection: ChangeDetectionStrategy.OnPush,
                    encapsulation: ViewEncapsulation.None,
                    providers: [
                        { provide: MatFormFieldControl, useExisting: MatDateRangeInput },
                        { provide: MAT_DATE_RANGE_INPUT_PARENT, useExisting: MatDateRangeInput },
                    ],
                    styles: [".mat-date-range-input{display:block;width:100%}.mat-date-range-input-container{display:flex;align-items:center}.mat-date-range-input-separator{transition:opacity 400ms 133.3333333333ms cubic-bezier(0.25, 0.8, 0.25, 1);margin:0 4px}.mat-date-range-input-separator-hidden{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;opacity:0;transition:none}.mat-date-range-input-inner{font:inherit;background:transparent;color:currentColor;border:none;outline:none;padding:0;margin:0;vertical-align:bottom;text-align:inherit;-webkit-appearance:none;width:100%}.mat-date-range-input-inner::-ms-clear,.mat-date-range-input-inner::-ms-reveal{display:none}.mat-date-range-input-inner::placeholder{transition:color 400ms 133.3333333333ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-date-range-input-inner::-moz-placeholder{transition:color 400ms 133.3333333333ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-date-range-input-inner::-webkit-input-placeholder{transition:color 400ms 133.3333333333ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-date-range-input-inner:-ms-input-placeholder{transition:color 400ms 133.3333333333ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-form-field-hide-placeholder .mat-date-range-input-inner::placeholder,.mat-date-range-input-hide-placeholders .mat-date-range-input-inner::placeholder{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;color:transparent !important;-webkit-text-fill-color:transparent;transition:none}.mat-form-field-hide-placeholder .mat-date-range-input-inner::-moz-placeholder,.mat-date-range-input-hide-placeholders .mat-date-range-input-inner::-moz-placeholder{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;color:transparent !important;-webkit-text-fill-color:transparent;transition:none}.mat-form-field-hide-placeholder .mat-date-range-input-inner::-webkit-input-placeholder,.mat-date-range-input-hide-placeholders .mat-date-range-input-inner::-webkit-input-placeholder{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;color:transparent !important;-webkit-text-fill-color:transparent;transition:none}.mat-form-field-hide-placeholder .mat-date-range-input-inner:-ms-input-placeholder,.mat-date-range-input-hide-placeholders .mat-date-range-input-inner:-ms-input-placeholder{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;color:transparent !important;-webkit-text-fill-color:transparent;transition:none}.mat-date-range-input-mirror{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;visibility:hidden;white-space:nowrap;display:inline-block;min-width:2px}.mat-date-range-input-start-wrapper{position:relative;overflow:hidden;max-width:calc(50% - 4px)}.mat-date-range-input-start-wrapper .mat-date-range-input-inner{position:absolute;top:0;left:0}.mat-date-range-input-end-wrapper{flex-grow:1;max-width:calc(50% - 4px)}.mat-form-field-type-mat-date-range-input .mat-form-field-infix{width:200px}\n"]
                },] }
    ];
    MatDateRangeInput.ctorParameters = () => [
        { type: ChangeDetectorRef },
        { type: ElementRef },
        { type: ControlContainer, decorators: [{ type: Optional }, { type: Self }] },
        { type: DateAdapter, decorators: [{ type: Optional }] },
        { type: MatFormField, decorators: [{ type: Optional }] }
    ];
    MatDateRangeInput.propDecorators = {
        rangePicker: [{ type: Input }],
        required: [{ type: Input }],
        dateFilter: [{ type: Input }],
        min: [{ type: Input }],
        max: [{ type: Input }],
        disabled: [{ type: Input }],
        separator: [{ type: Input }],
        comparisonStart: [{ type: Input }],
        comparisonEnd: [{ type: Input }],
        _startInput: [{ type: ContentChild, args: [MatStartDate,] }],
        _endInput: [{ type: ContentChild, args: [MatEndDate,] }]
    };
    return MatDateRangeInput;
})();
export { MatDateRangeInput };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGF0ZS1yYW5nZS1pbnB1dC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9kYXRlcGlja2VyL2RhdGUtcmFuZ2UtaW5wdXQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUNMLFNBQVMsRUFDVCx1QkFBdUIsRUFDdkIsaUJBQWlCLEVBQ2pCLEtBQUssRUFDTCxRQUFRLEVBRVIsWUFBWSxFQUVaLGlCQUFpQixFQUNqQixJQUFJLEVBQ0osVUFBVSxHQUNYLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBQyxtQkFBbUIsRUFBRSxZQUFZLEVBQUMsTUFBTSw4QkFBOEIsQ0FBQztBQUMvRSxPQUFPLEVBQWUsV0FBVyxFQUFDLE1BQU0sd0JBQXdCLENBQUM7QUFDakUsT0FBTyxFQUFZLGdCQUFnQixFQUFDLE1BQU0sZ0JBQWdCLENBQUM7QUFDM0QsT0FBTyxFQUFDLE9BQU8sRUFBRSxLQUFLLEVBQUMsTUFBTSxNQUFNLENBQUM7QUFDcEMsT0FBTyxFQUFDLHFCQUFxQixFQUFlLE1BQU0sdUJBQXVCLENBQUM7QUFDMUUsT0FBTyxFQUNMLFlBQVksRUFDWixVQUFVLEVBRVYsMkJBQTJCLEdBQzVCLE1BQU0sMEJBQTBCLENBQUM7QUFFbEMsT0FBTyxFQUFDLDBCQUEwQixFQUFDLE1BQU0scUJBQXFCLENBQUM7QUFFL0QsT0FBTyxFQUFDLGtCQUFrQixFQUFDLE1BQU0scUJBQXFCLENBQUM7QUFHdkQsSUFBSSxZQUFZLEdBQUcsQ0FBQyxDQUFDO0FBRXJCO0lBQUEsTUFvQmEsaUJBQWlCO1FBa0o1QixZQUNVLGtCQUFxQyxFQUNyQyxXQUFvQyxFQUN4QixPQUF5QixFQUN6QixZQUE0QixFQUM1QixVQUF5QjtZQUpyQyx1QkFBa0IsR0FBbEIsa0JBQWtCLENBQW1CO1lBQ3JDLGdCQUFXLEdBQVgsV0FBVyxDQUF5QjtZQUV4QixpQkFBWSxHQUFaLFlBQVks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QUFDLE9BQU8sRUFBRSwyQkFBMkIsRUFBRSxXQUFXLEVBQUUsaUJBQWlCLEVBQUM7cUJBQ3ZFOztpQkFDRjs7O2dCQTFDQyxpQkFBaUI7Z0JBRWpCLFVBQVU7Z0JBSU8sZ0JBQWdCLHVCQTBMOUIsUUFBUSxZQUFJLElBQUk7Z0JBM0xDLFdBQVcsdUJBNEw1QixRQUFRO2dCQTdMZ0IsWUFBWSx1QkE4THBDLFFBQVE7Ozs4QkFuSFYsS0FBSzsyQkFZTCxLQUFLOzZCQVFMLEtBQUs7c0JBU0wsS0FBSztzQkFTTCxLQUFLOzJCQVNMLEtBQUs7NEJBMENMLEtBQUs7a0NBR0wsS0FBSztnQ0FHTCxLQUFLOzhCQUVMLFlBQVksU0FBQyxZQUFZOzRCQUN6QixZQUFZLFNBQUMsVUFBVTs7SUFzSjFCLHdCQUFDO0tBQUE7U0E1UlksaUJBQWlCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7XG4gIENvbXBvbmVudCxcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksXG4gIFZpZXdFbmNhcHN1bGF0aW9uLFxuICBJbnB1dCxcbiAgT3B0aW9uYWwsXG4gIE9uRGVzdHJveSxcbiAgQ29udGVudENoaWxkLFxuICBBZnRlckNvbnRlbnRJbml0LFxuICBDaGFuZ2VEZXRlY3RvclJlZixcbiAgU2VsZixcbiAgRWxlbWVudFJlZixcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQge01hdEZvcm1GaWVsZENvbnRyb2wsIE1hdEZvcm1GaWVsZH0gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvZm9ybS1maWVsZCc7XG5pbXBvcnQge1RoZW1lUGFsZXR0ZSwgRGF0ZUFkYXB0ZXJ9IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL2NvcmUnO1xuaW1wb3J0IHtOZ0NvbnRyb2wsIENvbnRyb2xDb250YWluZXJ9IGZyb20gJ0Bhbmd1bGFyL2Zvcm1zJztcbmltcG9ydCB7U3ViamVjdCwgbWVyZ2V9IGZyb20gJ3J4anMnO1xuaW1wb3J0IHtjb2VyY2VCb29sZWFuUHJvcGVydHksIEJvb2xlYW5JbnB1dH0gZnJvbSAnQGFuZ3VsYXIvY2RrL2NvZXJjaW9uJztcbmltcG9ydCB7XG4gIE1hdFN0YXJ0RGF0ZSxcbiAgTWF0RW5kRGF0ZSxcbiAgTWF0RGF0ZVJhbmdlSW5wdXRQYXJlbnQsXG4gIE1BVF9EQVRFX1JBTkdFX0lOUFVUX1BBUkVOVCxcbn0gZnJvbSAnLi9kYXRlLXJhbmdlLWlucHV0LXBhcnRzJztcbmltcG9ydCB7TWF0RGF0ZXBpY2tlckNvbnRyb2x9IGZyb20gJy4vZGF0ZXBpY2tlci1iYXNlJztcbmltcG9ydCB7Y3JlYXRlTWlzc2luZ0RhdGVJbXBsRXJyb3J9IGZyb20gJy4vZGF0ZXBpY2tlci1lcnJvcnMnO1xuaW1wb3J0IHtEYXRlRmlsdGVyRm59IGZyb20gJy4vZGF0ZXBpY2tlci1pbnB1dC1iYXNlJztcbmltcG9ydCB7TWF0RGF0ZVJhbmdlUGlja2VyfSBmcm9tICcuL2RhdGUtcmFuZ2UtcGlja2VyJztcbmltcG9ydCB7RGF0ZVJhbmdlLCBNYXREYXRlU2VsZWN0aW9uTW9kZWx9IGZyb20gJy4vZGF0ZS1zZWxlY3Rpb24tbW9kZWwnO1xuXG5sZXQgbmV4dFVuaXF1ZUlkID0gMDtcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnbWF0LWRhdGUtcmFuZ2UtaW5wdXQnLFxuICB0ZW1wbGF0ZVVybDogJ2RhdGUtcmFuZ2UtaW5wdXQuaHRtbCcsXG4gIHN0eWxlVXJsczogWydkYXRlLXJhbmdlLWlucHV0LmNzcyddLFxuICBleHBvcnRBczogJ21hdERhdGVSYW5nZUlucHV0JyxcbiAgaG9zdDoge1xuICAgICdjbGFzcyc6ICdtYXQtZGF0ZS1yYW5nZS1pbnB1dCcsXG4gICAgJ1tjbGFzcy5tYXQtZGF0ZS1yYW5nZS1pbnB1dC1oaWRlLXBsYWNlaG9sZGVyc10nOiAnX3Nob3VsZEhpZGVQbGFjZWhvbGRlcnMoKScsXG4gICAgJ1thdHRyLmlkXSc6ICdudWxsJyxcbiAgICAncm9sZSc6ICdncm91cCcsXG4gICAgJ1thdHRyLmFyaWEtbGFiZWxsZWRieV0nOiAnX2FyaWFMYWJlbGxlZEJ5JyxcbiAgICAnW2F0dHIuYXJpYS1kZXNjcmliZWRieV0nOiAnX2FyaWFEZXNjcmliZWRCeScsXG4gIH0sXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoLFxuICBlbmNhcHN1bGF0aW9uOiBWaWV3RW5jYXBzdWxhdGlvbi5Ob25lLFxuICBwcm92aWRlcnM6IFtcbiAgICB7cHJvdmlkZTogTWF0Rm9ybUZpZWxkQ29udHJvbCwgdXNlRXhpc3Rpbmc6IE1hdERhdGVSYW5nZUlucHV0fSxcbiAgICB7cHJvdmlkZTogTUFUX0RBVEVfUkFOR0VfSU5QVVRfUEFSRU5ULCB1c2VFeGlzdGluZzogTWF0RGF0ZVJhbmdlSW5wdXR9LFxuICBdXG59KVxuZXhwb3J0IGNsYXNzIE1hdERhdGVSYW5nZUlucHV0PEQ+IGltcGxlbWVudHMgTWF0Rm9ybUZpZWxkQ29udHJvbDxEYXRlUmFuZ2U8RD4+LFxuICBNYXREYXRlcGlja2VyQ29udHJvbDxEPiwgTWF0RGF0ZVJhbmdlSW5wdXRQYXJlbnQ8RD4sIEFmdGVyQ29udGVudEluaXQsIE9uRGVzdHJveSB7XG4gIC8qKiBDdXJyZW50IHZhbHVlIG9mIHRoZSByYW5nZSBpbnB1dC4gKi9cbiAgZ2V0IHZhbHVlKCkge1xuICAgIHJldHVybiB0aGlzLl9tb2RlbCA/IHRoaXMuX21vZGVsLnNlbGVjdGlvbiA6IG51bGw7XG4gIH1cblxuICAvKiogRW1pdHMgd2hlbiB0aGUgaW5wdXQncyBzdGF0ZSBoYXMgY2hhbmdlZC4gKi9cbiAgc3RhdGVDaGFuZ2VzID0gbmV3IFN1YmplY3Q8dm9pZD4oKTtcblxuICAvKiogVW5pcXVlIElEIGZvciB0aGUgaW5wdXQuICovXG4gIGlkID0gYG1hdC1kYXRlLXJhbmdlLWlucHV0LSR7bmV4dFVuaXF1ZUlkKyt9YDtcblxuICAvKiogV2hldGhlciB0aGUgY29udHJvbCBpcyBmb2N1c2VkLiAqL1xuICBmb2N1c2VkID0gZmFsc2U7XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGNvbnRyb2wncyBsYWJlbCBzaG91bGQgZmxvYXQuICovXG4gIGdldCBzaG91bGRMYWJlbEZsb2F0KCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLmZvY3VzZWQgfHwgIXRoaXMuZW1wdHk7XG4gIH1cblxuICAvKiogTmFtZSBvZiB0aGUgZm9ybSBjb250cm9sLiAqL1xuICBjb250cm9sVHlwZSA9ICdtYXQtZGF0ZS1yYW5nZS1pbnB1dCc7XG5cbiAgLyoqXG4gICAqIEltcGxlbWVudGVkIGFzIGEgcGFydCBvZiBgTWF0Rm9ybUZpZWxkQ29udHJvbGAuXG4gICAqIFNldCB0aGUgcGxhY2Vob2xkZXIgYXR0cmlidXRlIG9uIGBtYXRTdGFydERhdGVgIGFuZCBgbWF0RW5kRGF0ZWAuXG4gICAqIEBkb2NzLXByaXZhdGVcbiAgICovXG4gIGdldCBwbGFjZWhvbGRlcigpIHtcbiAgICBjb25zdCBzdGFydCA9IHRoaXMuX3N0YXJ0SW5wdXQ/Ll9nZXRQbGFjZWhvbGRlcigpIHx8ICcnO1xuICAgIGNvbnN0IGVuZCA9IHRoaXMuX2VuZElucHV0Py5fZ2V0UGxhY2Vob2xkZXIoKSB8fCAnJztcbiAgICByZXR1cm4gKHN0YXJ0IHx8IGVuZCkgPyBgJHtzdGFydH0gJHt0aGlzLnNlcGFyYXRvcn0gJHtlbmR9YCA6ICcnO1xuICB9XG5cbiAgLyoqIFRoZSByYW5nZSBwaWNrZXIgdGhhdCB0aGlzIGlucHV0IGlzIGFzc29jaWF0ZWQgd2l0aC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IHJhbmdlUGlja2VyKCkgeyByZXR1cm4gdGhpcy5fcmFuZ2VQaWNrZXI7IH1cbiAgc2V0IHJhbmdlUGlja2VyKHJhbmdlUGlja2VyOiBNYXREYXRlUmFuZ2VQaWNrZXI8RD4pIHtcbiAgICBpZiAocmFuZ2VQaWNrZXIpIHtcbiAgICAgIHRoaXMuX21vZGVsID0gcmFuZ2VQaWNrZXIuX3JlZ2lzdGVySW5wdXQodGhpcyk7XG4gICAgICB0aGlzLl9yYW5nZVBpY2tlciA9IHJhbmdlUGlja2VyO1xuICAgICAgdGhpcy5fcmVnaXN0ZXJNb2RlbCh0aGlzLl9tb2RlbCEpO1xuICAgIH1cbiAgfVxuICBwcml2YXRlIF9yYW5nZVBpY2tlcjogTWF0RGF0ZVJhbmdlUGlja2VyPEQ+O1xuXG4gIC8qKiBXaGV0aGVyIHRoZSBpbnB1dCBpcyByZXF1aXJlZC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IHJlcXVpcmVkKCk6IGJvb2xlYW4geyByZXR1cm4gISF0aGlzLl9yZXF1aXJlZDsgfVxuICBzZXQgcmVxdWlyZWQodmFsdWU6IGJvb2xlYW4pIHtcbiAgICB0aGlzLl9yZXF1aXJlZCA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh2YWx1ZSk7XG4gIH1cbiAgcHJpdmF0ZSBfcmVxdWlyZWQ6IGJvb2xlYW47XG5cbiAgLyoqIEZ1bmN0aW9uIHRoYXQgY2FuIGJlIHVzZWQgdG8gZmlsdGVyIG91dCBkYXRlcyB3aXRoaW4gdGhlIGRhdGUgcmFuZ2UgcGlja2VyLiAqL1xuICBASW5wdXQoKVxuICBnZXQgZGF0ZUZpbHRlcigpIHsgcmV0dXJuIHRoaXMuX2RhdGVGaWx0ZXI7IH1cbiAgc2V0IGRhdGVGaWx0ZXIodmFsdWU6IERhdGVGaWx0ZXJGbjxEPikge1xuICAgIHRoaXMuX2RhdGVGaWx0ZXIgPSB2YWx1ZTtcbiAgICB0aGlzLl9yZXZhbGlkYXRlKCk7XG4gIH1cbiAgcHJpdmF0ZSBfZGF0ZUZpbHRlcjogRGF0ZUZpbHRlckZuPEQ+O1xuXG4gIC8qKiBUaGUgbWluaW11bSB2YWxpZCBkYXRlLiAqL1xuICBASW5wdXQoKVxuICBnZXQgbWluKCk6IEQgfCBudWxsIHsgcmV0dXJuIHRoaXMuX21pbjsgfVxuICBzZXQgbWluKHZhbHVlOiBEIHwgbnVsbCkge1xuICAgIHRoaXMuX21pbiA9IHRoaXMuX2dldFZhbGlkRGF0ZU9yTnVsbCh0aGlzLl9kYXRlQWRhcHRlci5kZXNlcmlhbGl6ZSh2YWx1ZSkpO1xuICAgIHRoaXMuX3JldmFsaWRhdGUoKTtcbiAgfVxuICBwcml2YXRlIF9taW46IEQgfCBudWxsO1xuXG4gIC8qKiBUaGUgbWF4aW11bSB2YWxpZCBkYXRlLiAqL1xuICBASW5wdXQoKVxuICBnZXQgbWF4KCk6IEQgfCBudWxsIHsgcmV0dXJuIHRoaXMuX21heDsgfVxuICBzZXQgbWF4KHZhbHVlOiBEIHwgbnVsbCkge1xuICAgIHRoaXMuX21heCA9IHRoaXMuX2dldFZhbGlkRGF0ZU9yTnVsbCh0aGlzLl9kYXRlQWRhcHRlci5kZXNlcmlhbGl6ZSh2YWx1ZSkpO1xuICAgIHRoaXMuX3JldmFsaWRhdGUoKTtcbiAgfVxuICBwcml2YXRlIF9tYXg6IEQgfCBudWxsO1xuXG4gIC8qKiBXaGV0aGVyIHRoZSBpbnB1dCBpcyBkaXNhYmxlZC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IGRpc2FibGVkKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiAodGhpcy5fc3RhcnRJbnB1dCAmJiB0aGlzLl9lbmRJbnB1dCkgP1xuICAgICAgKHRoaXMuX3N0YXJ0SW5wdXQuZGlzYWJsZWQgJiYgdGhpcy5fZW5kSW5wdXQuZGlzYWJsZWQpIDpcbiAgICAgIHRoaXMuX2dyb3VwRGlzYWJsZWQ7XG4gIH1cbiAgc2V0IGRpc2FibGVkKHZhbHVlOiBib29sZWFuKSB7XG4gICAgY29uc3QgbmV3VmFsdWUgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkodmFsdWUpO1xuXG4gICAgaWYgKG5ld1ZhbHVlICE9PSB0aGlzLl9ncm91cERpc2FibGVkKSB7XG4gICAgICB0aGlzLl9ncm91cERpc2FibGVkID0gbmV3VmFsdWU7XG4gICAgICB0aGlzLl9kaXNhYmxlZENoYW5nZS5uZXh0KHRoaXMuZGlzYWJsZWQpO1xuICAgIH1cbiAgfVxuICBfZ3JvdXBEaXNhYmxlZCA9IGZhbHNlO1xuXG4gIC8qKiBXaGV0aGVyIHRoZSBpbnB1dCBpcyBpbiBhbiBlcnJvciBzdGF0ZS4gKi9cbiAgZ2V0IGVycm9yU3RhdGUoKTogYm9vbGVhbiB7XG4gICAgaWYgKHRoaXMuX3N0YXJ0SW5wdXQgJiYgdGhpcy5fZW5kSW5wdXQpIHtcbiAgICAgIHJldHVybiB0aGlzLl9zdGFydElucHV0LmVycm9yU3RhdGUgfHwgdGhpcy5fZW5kSW5wdXQuZXJyb3JTdGF0ZTtcbiAgICB9XG5cbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cblxuICAvKiogV2hldGhlciB0aGUgZGF0ZXBpY2tlciBpbnB1dCBpcyBlbXB0eS4gKi9cbiAgZ2V0IGVtcHR5KCk6IGJvb2xlYW4ge1xuICAgIGNvbnN0IHN0YXJ0RW1wdHkgPSB0aGlzLl9zdGFydElucHV0ID8gdGhpcy5fc3RhcnRJbnB1dC5pc0VtcHR5KCkgOiBmYWxzZTtcbiAgICBjb25zdCBlbmRFbXB0eSA9IHRoaXMuX2VuZElucHV0ID8gdGhpcy5fZW5kSW5wdXQuaXNFbXB0eSgpIDogZmFsc2U7XG4gICAgcmV0dXJuIHN0YXJ0RW1wdHkgJiYgZW5kRW1wdHk7XG4gIH1cblxuICAvKiogVmFsdWUgZm9yIHRoZSBgYXJpYS1kZXNjcmliZWRieWAgYXR0cmlidXRlIG9mIHRoZSBpbnB1dHMuICovXG4gIF9hcmlhRGVzY3JpYmVkQnk6IHN0cmluZyB8IG51bGwgPSBudWxsO1xuXG4gIC8qKiBWYWx1ZSBmb3IgdGhlIGBhcmlhLWxhYmVsbGVkYnlgIGF0dHJpYnV0ZSBvZiB0aGUgaW5wdXRzLiAqL1xuICBfYXJpYUxhYmVsbGVkQnk6IHN0cmluZyB8IG51bGwgPSBudWxsO1xuXG4gIC8qKiBEYXRlIHNlbGVjdGlvbiBtb2RlbCBjdXJyZW50bHkgcmVnaXN0ZXJlZCB3aXRoIHRoZSBpbnB1dC4gKi9cbiAgcHJpdmF0ZSBfbW9kZWw6IE1hdERhdGVTZWxlY3Rpb25Nb2RlbDxEYXRlUmFuZ2U8RD4+IHwgdW5kZWZpbmVkO1xuXG4gIC8qKiBTZXBhcmF0b3IgdGV4dCB0byBiZSBzaG93biBiZXR3ZWVuIHRoZSBpbnB1dHMuICovXG4gIEBJbnB1dCgpIHNlcGFyYXRvciA9ICfigJMnO1xuXG4gIC8qKiBTdGFydCBvZiB0aGUgY29tcGFyaXNvbiByYW5nZSB0aGF0IHNob3VsZCBiZSBzaG93biBpbiB0aGUgY2FsZW5kYXIuICovXG4gIEBJbnB1dCgpIGNvbXBhcmlzb25TdGFydDogRCB8IG51bGwgPSBudWxsO1xuXG4gIC8qKiBFbmQgb2YgdGhlIGNvbXBhcmlzb24gcmFuZ2UgdGhhdCBzaG91bGQgYmUgc2hvd24gaW4gdGhlIGNhbGVuZGFyLiAqL1xuICBASW5wdXQoKSBjb21wYXJpc29uRW5kOiBEIHwgbnVsbCA9IG51bGw7XG5cbiAgQENvbnRlbnRDaGlsZChNYXRTdGFydERhdGUpIF9zdGFydElucHV0OiBNYXRTdGFydERhdGU8RD47XG4gIEBDb250ZW50Q2hpbGQoTWF0RW5kRGF0ZSkgX2VuZElucHV0OiBNYXRFbmREYXRlPEQ+O1xuXG4gIC8qKlxuICAgKiBJbXBsZW1lbnRlZCBhcyBhIHBhcnQgb2YgYE1hdEZvcm1GaWVsZENvbnRyb2xgLlxuICAgKiBUT0RPKGNyaXNiZXRvKTogY2hhbmdlIHR5cGUgdG8gYEFic3RyYWN0Q29udHJvbERpcmVjdGl2ZWAgYWZ0ZXIgIzE4MjA2IGxhbmRzLlxuICAgKiBAZG9jcy1wcml2YXRlXG4gICAqL1xuICBuZ0NvbnRyb2w6IE5nQ29udHJvbCB8IG51bGw7XG5cbiAgLyoqIEVtaXRzIHdoZW4gdGhlIGlucHV0J3MgZGlzYWJsZWQgc3RhdGUgY2hhbmdlcy4gKi9cbiAgX2Rpc2FibGVkQ2hhbmdlID0gbmV3IFN1YmplY3Q8Ym9vbGVhbj4oKTtcblxuICBjb25zdHJ1Y3RvcihcbiAgICBwcml2YXRlIF9jaGFuZ2VEZXRlY3RvclJlZjogQ2hhbmdlRGV0ZWN0b3JSZWYsXG4gICAgcHJpdmF0ZSBfZWxlbWVudFJlZjogRWxlbWVudFJlZjxIVE1MRWxlbWVudD4sXG4gICAgQE9wdGlvbmFsKCkgQFNlbGYoKSBjb250cm9sOiBDb250cm9sQ29udGFpbmVyLFxuICAgIEBPcHRpb25hbCgpIHByaXZhdGUgX2RhdGVBZGFwdGVyOiBEYXRlQWRhcHRlcjxEPixcbiAgICBAT3B0aW9uYWwoKSBwcml2YXRlIF9mb3JtRmllbGQ/OiBNYXRGb3JtRmllbGQpIHtcblxuICAgIGlmICghX2RhdGVBZGFwdGVyKSB7XG4gICAgICB0aHJvdyBjcmVhdGVNaXNzaW5nRGF0ZUltcGxFcnJvcignRGF0ZUFkYXB0ZXInKTtcbiAgICB9XG5cbiAgICAvLyBUT0RPKGNyaXNiZXRvKTogcmVtb3ZlIGBhcyBhbnlgIGFmdGVyICMxODIwNiBsYW5kcy5cbiAgICB0aGlzLm5nQ29udHJvbCA9IGNvbnRyb2wgYXMgYW55O1xuICAgIHRoaXMuX2FyaWFMYWJlbGxlZEJ5ID0gX2Zvcm1GaWVsZCA/IF9mb3JtRmllbGQuX2xhYmVsSWQgOiBudWxsO1xuICB9XG5cbiAgLyoqXG4gICAqIEltcGxlbWVudGVkIGFzIGEgcGFydCBvZiBgTWF0Rm9ybUZpZWxkQ29udHJvbGAuXG4gICAqIEBkb2NzLXByaXZhdGVcbiAgICovXG4gIHNldERlc2NyaWJlZEJ5SWRzKGlkczogc3RyaW5nW10pOiB2b2lkIHtcbiAgICB0aGlzLl9hcmlhRGVzY3JpYmVkQnkgPSBpZHMubGVuZ3RoID8gaWRzLmpvaW4oJyAnKSA6IG51bGw7XG4gIH1cblxuICAvKipcbiAgICogSW1wbGVtZW50ZWQgYXMgYSBwYXJ0IG9mIGBNYXRGb3JtRmllbGRDb250cm9sYC5cbiAgICogQGRvY3MtcHJpdmF0ZVxuICAgKi9cbiAgb25Db250YWluZXJDbGljaygpOiB2b2lkIHtcbiAgICBpZiAoIXRoaXMuZm9jdXNlZCAmJiAhdGhpcy5kaXNhYmxlZCkge1xuICAgICAgaWYgKCF0aGlzLl9tb2RlbCB8fCAhdGhpcy5fbW9kZWwuc2VsZWN0aW9uLnN0YXJ0KSB7XG4gICAgICAgIHRoaXMuX3N0YXJ0SW5wdXQuZm9jdXMoKTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIHRoaXMuX2VuZElucHV0LmZvY3VzKCk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgbmdBZnRlckNvbnRlbnRJbml0KCkge1xuICAgIGlmICghdGhpcy5fc3RhcnRJbnB1dCkge1xuICAgICAgdGhyb3cgRXJyb3IoJ21hdC1kYXRlLXJhbmdlLWlucHV0IG11c3QgY29udGFpbiBhIG1hdFN0YXJ0RGF0ZSBpbnB1dCcpO1xuICAgIH1cblxuICAgIGlmICghdGhpcy5fZW5kSW5wdXQpIHtcbiAgICAgIHRocm93IEVycm9yKCdtYXQtZGF0ZS1yYW5nZS1pbnB1dCBtdXN0IGNvbnRhaW4gYSBtYXRFbmREYXRlIGlucHV0Jyk7XG4gICAgfVxuXG4gICAgaWYgKHRoaXMuX21vZGVsKSB7XG4gICAgICB0aGlzLl9yZWdpc3Rlck1vZGVsKHRoaXMuX21vZGVsKTtcbiAgICB9XG5cbiAgICAvLyBXZSBkb24ndCBuZWVkIHRvIHVuc3Vic2NyaWJlIGZyb20gdGhpcywgYmVjYXVzZSB3ZVxuICAgIC8vIGtub3cgdGhhdCB0aGUgaW5wdXQgc3RyZWFtcyB3aWxsIGJlIGNvbXBsZXRlZCBvbiBkZXN0cm95LlxuICAgIG1lcmdlKHRoaXMuX3N0YXJ0SW5wdXQuX2Rpc2FibGVkQ2hhbmdlLCB0aGlzLl9lbmRJbnB1dC5fZGlzYWJsZWRDaGFuZ2UpLnN1YnNjcmliZSgoKSA9PiB7XG4gICAgICB0aGlzLl9kaXNhYmxlZENoYW5nZS5uZXh0KHRoaXMuZGlzYWJsZWQpO1xuICAgIH0pO1xuICB9XG5cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgdGhpcy5zdGF0ZUNoYW5nZXMuY29tcGxldGUoKTtcbiAgICB0aGlzLl9kaXNhYmxlZENoYW5nZS51bnN1YnNjcmliZSgpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIGRhdGUgYXQgd2hpY2ggdGhlIGNhbGVuZGFyIHNob3VsZCBzdGFydC4gKi9cbiAgZ2V0U3RhcnRWYWx1ZSgpOiBEIHwgbnVsbCB7XG4gICAgcmV0dXJuIHRoaXMudmFsdWUgPyB0aGlzLnZhbHVlLnN0YXJ0IDogbnVsbDtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSBpbnB1dCdzIHRoZW1lIHBhbGV0dGUuICovXG4gIGdldFRoZW1lUGFsZXR0ZSgpOiBUaGVtZVBhbGV0dGUge1xuICAgIHJldHVybiB0aGlzLl9mb3JtRmllbGQgPyB0aGlzLl9mb3JtRmllbGQuY29sb3IgOiB1bmRlZmluZWQ7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgZWxlbWVudCB0byB3aGljaCB0aGUgY2FsZW5kYXIgb3ZlcmxheSBzaG91bGQgYmUgYXR0YWNoZWQuICovXG4gIGdldENvbm5lY3RlZE92ZXJsYXlPcmlnaW4oKTogRWxlbWVudFJlZiB7XG4gICAgcmV0dXJuIHRoaXMuX2Zvcm1GaWVsZCA/IHRoaXMuX2Zvcm1GaWVsZC5nZXRDb25uZWN0ZWRPdmVybGF5T3JpZ2luKCkgOiB0aGlzLl9lbGVtZW50UmVmO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHZhbHVlIHRoYXQgaXMgdXNlZCB0byBtaXJyb3IgdGhlIHN0YXRlIGlucHV0LiAqL1xuICBfZ2V0SW5wdXRNaXJyb3JWYWx1ZSgpIHtcbiAgICByZXR1cm4gdGhpcy5fc3RhcnRJbnB1dCA/IHRoaXMuX3N0YXJ0SW5wdXQuZ2V0TWlycm9yVmFsdWUoKSA6ICcnO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGlucHV0IHBsYWNlaG9sZGVycyBzaG91bGQgYmUgaGlkZGVuLiAqL1xuICBfc2hvdWxkSGlkZVBsYWNlaG9sZGVycygpIHtcbiAgICByZXR1cm4gdGhpcy5fc3RhcnRJbnB1dCA/ICF0aGlzLl9zdGFydElucHV0LmlzRW1wdHkoKSA6IGZhbHNlO1xuICB9XG5cbiAgLyoqIEhhbmRsZXMgdGhlIHZhbHVlIGluIG9uZSBvZiB0aGUgY2hpbGQgaW5wdXRzIGNoYW5naW5nLiAqL1xuICBfaGFuZGxlQ2hpbGRWYWx1ZUNoYW5nZSgpIHtcbiAgICB0aGlzLnN0YXRlQ2hhbmdlcy5uZXh0KCk7XG4gICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3JSZWYubWFya0ZvckNoZWNrKCk7XG4gIH1cblxuICAvKiogT3BlbnMgdGhlIGRhdGUgcmFuZ2UgcGlja2VyIGFzc29jaWF0ZWQgd2l0aCB0aGUgaW5wdXQuICovXG4gIF9vcGVuRGF0ZXBpY2tlcigpIHtcbiAgICBpZiAodGhpcy5fcmFuZ2VQaWNrZXIpIHtcbiAgICAgIHRoaXMuX3JhbmdlUGlja2VyLm9wZW4oKTtcbiAgICB9XG4gIH1cblxuICAvKiogV2hldGhlciB0aGUgc2VwYXJhdGUgdGV4dCBzaG91bGQgYmUgaGlkZGVuLiAqL1xuICBfc2hvdWxkSGlkZVNlcGFyYXRvcigpIHtcbiAgICByZXR1cm4gKCF0aGlzLl9mb3JtRmllbGQgfHwgdGhpcy5fZm9ybUZpZWxkLl9oaWRlQ29udHJvbFBsYWNlaG9sZGVyKCkpICYmIHRoaXMuZW1wdHk7XG4gIH1cblxuICAvKipcbiAgICogQHBhcmFtIG9iaiBUaGUgb2JqZWN0IHRvIGNoZWNrLlxuICAgKiBAcmV0dXJucyBUaGUgZ2l2ZW4gb2JqZWN0IGlmIGl0IGlzIGJvdGggYSBkYXRlIGluc3RhbmNlIGFuZCB2YWxpZCwgb3RoZXJ3aXNlIG51bGwuXG4gICAqL1xuICBwcml2YXRlIF9nZXRWYWxpZERhdGVPck51bGwob2JqOiBhbnkpOiBEIHwgbnVsbCB7XG4gICAgcmV0dXJuICh0aGlzLl9kYXRlQWRhcHRlci5pc0RhdGVJbnN0YW5jZShvYmopICYmIHRoaXMuX2RhdGVBZGFwdGVyLmlzVmFsaWQob2JqKSkgPyBvYmogOiBudWxsO1xuICB9XG5cbiAgLyoqIFJlLXJ1bnMgdGhlIHZhbGlkYXRvcnMgb24gdGhlIHN0YXJ0L2VuZCBpbnB1dHMuICovXG4gIHByaXZhdGUgX3JldmFsaWRhdGUoKSB7XG4gICAgaWYgKHRoaXMuX3N0YXJ0SW5wdXQpIHtcbiAgICAgIHRoaXMuX3N0YXJ0SW5wdXQuX3ZhbGlkYXRvck9uQ2hhbmdlKCk7XG4gICAgfVxuXG4gICAgaWYgKHRoaXMuX2VuZElucHV0KSB7XG4gICAgICB0aGlzLl9lbmRJbnB1dC5fdmFsaWRhdG9yT25DaGFuZ2UoKTtcbiAgICB9XG4gIH1cblxuICAvKiogUmVnaXN0ZXJzIHRoZSBjdXJyZW50IGRhdGUgc2VsZWN0aW9uIG1vZGVsIHdpdGggdGhlIHN0YXJ0L2VuZCBpbnB1dHMuICovXG4gIHByaXZhdGUgX3JlZ2lzdGVyTW9kZWwobW9kZWw6IE1hdERhdGVTZWxlY3Rpb25Nb2RlbDxEYXRlUmFuZ2U8RD4+KSB7XG4gICAgaWYgKHRoaXMuX3N0YXJ0SW5wdXQpIHtcbiAgICAgIHRoaXMuX3N0YXJ0SW5wdXQuX3JlZ2lzdGVyTW9kZWwobW9kZWwpO1xuICAgIH1cblxuICAgIGlmICh0aGlzLl9lbmRJbnB1dCkge1xuICAgICAgdGhpcy5fZW5kSW5wdXQuX3JlZ2lzdGVyTW9kZWwobW9kZWwpO1xuICAgIH1cbiAgfVxuXG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV9yZXF1aXJlZDogQm9vbGVhbklucHV0O1xuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfZGlzYWJsZWQ6IEJvb2xlYW5JbnB1dDtcbn1cbiJdfQ==