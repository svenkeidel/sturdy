/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { FocusMonitor } from '@angular/cdk/a11y';
import { coerceBooleanProperty } from '@angular/cdk/coercion';
import { SelectionModel } from '@angular/cdk/collections';
import { Attribute, ChangeDetectionStrategy, ChangeDetectorRef, Component, ContentChildren, Directive, ElementRef, EventEmitter, forwardRef, Input, Optional, Output, QueryList, ViewChild, ViewEncapsulation, InjectionToken, Inject, } from '@angular/core';
import { NG_VALUE_ACCESSOR } from '@angular/forms';
import { mixinDisableRipple, } from '@angular/material/core';
/**
 * Injection token that can be used to configure the
 * default options for all button toggles within an app.
 */
export const MAT_BUTTON_TOGGLE_DEFAULT_OPTIONS = new InjectionToken('MAT_BUTTON_TOGGLE_DEFAULT_OPTIONS');
/**
 * Provider Expression that allows mat-button-toggle-group to register as a ControlValueAccessor.
 * This allows it to support [(ngModel)].
 * @docs-private
 */
export const MAT_BUTTON_TOGGLE_GROUP_VALUE_ACCESSOR = {
    provide: NG_VALUE_ACCESSOR,
    useExisting: forwardRef(() => MatButtonToggleGroup),
    multi: true
};
let _uniqueIdCounter = 0;
/** Change event object emitted by MatButtonToggle. */
export class MatButtonToggleChange {
    constructor(
    /** The MatButtonToggle that emits the event. */
    source, 
    /** The value assigned to the MatButtonToggle. */
    value) {
        this.source = source;
        this.value = value;
    }
}
/** Exclusive selection button toggle group that behaves like a radio-button group. */
let MatButtonToggleGroup = /** @class */ (() => {
    class MatButtonToggleGroup {
        constructor(_changeDetector, defaultOptions) {
            this._changeDetector = _changeDetector;
            this._vertical = false;
            this._multiple = false;
            this._disabled = false;
            /**
             * The method to be called in order to update ngModel.
             * Now `ngModel` binding is not supported in multiple selection mode.
             */
            this._controlValueAccessorChangeFn = () => { };
            /** onTouch function registered via registerOnTouch (ControlValueAccessor). */
            this._onTouched = () => { };
            this._name = `mat-button-toggle-group-${_uniqueIdCounter++}`;
            /**
             * Event that emits whenever the value of the group changes.
             * Used to facilitate two-way data binding.
             * @docs-private
             */
            this.valueChange = new EventEmitter();
            /** Event emitted when the group's value changes. */
            this.change = new EventEmitter();
            this.appearance =
                defaultOptions && defaultOptions.appearance ? defaultOptions.appearance : 'standard';
        }
        /** `name` attribute for the underlying `input` element. */
        get name() { return this._name; }
        set name(value) {
            this._name = value;
            if (this._buttonToggles) {
                this._buttonToggles.forEach(toggle => {
                    toggle.name = this._name;
                    toggle._markForCheck();
                });
            }
        }
        /** Whether the toggle group is vertical. */
        get vertical() { return this._vertical; }
        set vertical(value) {
            this._vertical = coerceBooleanProperty(value);
        }
        /** Value of the toggle group. */
        get value() {
            const selected = this._selectionModel ? this._selectionModel.selected : [];
            if (this.multiple) {
                return selected.map(toggle => toggle.value);
            }
            return selected[0] ? selected[0].value : undefined;
        }
        set value(newValue) {
            this._setSelectionByValue(newValue);
            this.valueChange.emit(this.value);
        }
        /** Selected button toggles in the group. */
        get selected() {
            const selected = this._selectionModel ? this._selectionModel.selected : [];
            return this.multiple ? selected : (selected[0] || null);
        }
        /** Whether multiple button toggles can be selected. */
        get multiple() { return this._multiple; }
        set multiple(value) {
            this._multiple = coerceBooleanProperty(value);
        }
        /** Whether multiple button toggle group is disabled. */
        get disabled() { return this._disabled; }
        set disabled(value) {
            this._disabled = coerceBooleanProperty(value);
            if (this._buttonToggles) {
                this._buttonToggles.forEach(toggle => toggle._markForCheck());
            }
        }
        ngOnInit() {
            this._selectionModel = new SelectionModel(this.multiple, undefined, false);
        }
        ngAfterContentInit() {
            this._selectionModel.select(...this._buttonToggles.filter(toggle => toggle.checked));
        }
        /**
         * Sets the model value. Implemented as part of ControlValueAccessor.
         * @param value Value to be set to the model.
         */
        writeValue(value) {
            this.value = value;
            this._changeDetector.markForCheck();
        }
        // Implemented as part of ControlValueAccessor.
        registerOnChange(fn) {
            this._controlValueAccessorChangeFn = fn;
        }
        // Implemented as part of ControlValueAccessor.
        registerOnTouched(fn) {
            this._onTouched = fn;
        }
        // Implemented as part of ControlValueAccessor.
        setDisabledState(isDisabled) {
            this.disabled = isDisabled;
        }
        /** Dispatch change event with current selection and group value. */
        _emitChangeEvent() {
            const selected = this.selected;
            const source = Array.isArray(selected) ? selected[selected.length - 1] : selected;
            const event = new MatButtonToggleChange(source, this.value);
            this._controlValueAccessorChangeFn(event.value);
            this.change.emit(event);
        }
        /**
         * Syncs a button toggle's selected state with the model value.
         * @param toggle Toggle to be synced.
         * @param select Whether the toggle should be selected.
         * @param isUserInput Whether the change was a result of a user interaction.
         * @param deferEvents Whether to defer emitting the change events.
         */
        _syncButtonToggle(toggle, select, isUserInput = false, deferEvents = false) {
            // Deselect the currently-selected toggle, if we're in single-selection
            // mode and the button being toggled isn't selected at the moment.
            if (!this.multiple && this.selected && !toggle.checked) {
                this.selected.checked = false;
            }
            if (this._selectionModel) {
                if (select) {
                    this._selectionModel.select(toggle);
                }
                else {
                    this._selectionModel.deselect(toggle);
                }
            }
            else {
                deferEvents = true;
            }
            // We need to defer in some cases in order to avoid "changed after checked errors", however
            // the side-effect is that we may end up updating the model value out of sequence in others
            // The `deferEvents` flag allows us to decide whether to do it on a case-by-case basis.
            if (deferEvents) {
                Promise.resolve().then(() => this._updateModelValue(isUserInput));
            }
            else {
                this._updateModelValue(isUserInput);
            }
        }
        /** Checks whether a button toggle is selected. */
        _isSelected(toggle) {
            return this._selectionModel && this._selectionModel.isSelected(toggle);
        }
        /** Determines whether a button toggle should be checked on init. */
        _isPrechecked(toggle) {
            if (typeof this._rawValue === 'undefined') {
                return false;
            }
            if (this.multiple && Array.isArray(this._rawValue)) {
                return this._rawValue.some(value => toggle.value != null && value === toggle.value);
            }
            return toggle.value === this._rawValue;
        }
        /** Updates the selection state of the toggles in the group based on a value. */
        _setSelectionByValue(value) {
            this._rawValue = value;
            if (!this._buttonToggles) {
                return;
            }
            if (this.multiple && value) {
                if (!Array.isArray(value)) {
                    throw Error('Value must be an array in multiple-selection mode.');
                }
                this._clearSelection();
                value.forEach((currentValue) => this._selectValue(currentValue));
            }
            else {
                this._clearSelection();
                this._selectValue(value);
            }
        }
        /** Clears the selected toggles. */
        _clearSelection() {
            this._selectionModel.clear();
            this._buttonToggles.forEach(toggle => toggle.checked = false);
        }
        /** Selects a value if there's a toggle that corresponds to it. */
        _selectValue(value) {
            const correspondingOption = this._buttonToggles.find(toggle => {
                return toggle.value != null && toggle.value === value;
            });
            if (correspondingOption) {
                correspondingOption.checked = true;
                this._selectionModel.select(correspondingOption);
            }
        }
        /** Syncs up the group's value with the model and emits the change event. */
        _updateModelValue(isUserInput) {
            // Only emit the change event for user input.
            if (isUserInput) {
                this._emitChangeEvent();
            }
            // Note: we emit this one no matter whether it was a user interaction, because
            // it is used by Angular to sync up the two-way data binding.
            this.valueChange.emit(this.value);
        }
    }
    MatButtonToggleGroup.decorators = [
        { type: Directive, args: [{
                    selector: 'mat-button-toggle-group',
                    providers: [MAT_BUTTON_TOGGLE_GROUP_VALUE_ACCESSOR],
                    host: {
                        'role': 'group',
                        'class': 'mat-button-toggle-group',
                        '[attr.aria-disabled]': 'disabled',
                        '[class.mat-button-toggle-vertical]': 'vertical',
                        '[class.mat-button-toggle-group-appearance-standard]': 'appearance === "standard"',
                    },
                    exportAs: 'matButtonToggleGroup',
                },] }
    ];
    MatButtonToggleGroup.ctorParameters = () => [
        { type: ChangeDetectorRef },
        { type: undefined, decorators: [{ type: Optional }, { type: Inject, args: [MAT_BUTTON_TOGGLE_DEFAULT_OPTIONS,] }] }
    ];
    MatButtonToggleGroup.propDecorators = {
        _buttonToggles: [{ type: ContentChildren, args: [forwardRef(() => MatButtonToggle), {
                        // Note that this would technically pick up toggles
                        // from nested groups, but that's not a case that we support.
                        descendants: true
                    },] }],
        appearance: [{ type: Input }],
        name: [{ type: Input }],
        vertical: [{ type: Input }],
        value: [{ type: Input }],
        valueChange: [{ type: Output }],
        multiple: [{ type: Input }],
        disabled: [{ type: Input }],
        change: [{ type: Output }]
    };
    return MatButtonToggleGroup;
})();
export { MatButtonToggleGroup };
// Boilerplate for applying mixins to the MatButtonToggle class.
/** @docs-private */
class MatButtonToggleBase {
}
const _MatButtonToggleMixinBase = mixinDisableRipple(MatButtonToggleBase);
/** Single button inside of a toggle group. */
let MatButtonToggle = /** @class */ (() => {
    class MatButtonToggle extends _MatButtonToggleMixinBase {
        constructor(toggleGroup, _changeDetectorRef, _elementRef, _focusMonitor, defaultTabIndex, defaultOptions) {
            super();
            this._changeDetectorRef = _changeDetectorRef;
            this._elementRef = _elementRef;
            this._focusMonitor = _focusMonitor;
            this._isSingleSelector = false;
            this._checked = false;
            /**
             * Users can specify the `aria-labelledby` attribute which will be forwarded to the input element
             */
            this.ariaLabelledby = null;
            this._disabled = false;
            /** Event emitted when the group value changes. */
            this.change = new EventEmitter();
            const parsedTabIndex = Number(defaultTabIndex);
            this.tabIndex = (parsedTabIndex || parsedTabIndex === 0) ? parsedTabIndex : null;
            this.buttonToggleGroup = toggleGroup;
            this.appearance =
                defaultOptions && defaultOptions.appearance ? defaultOptions.appearance : 'standard';
        }
        /** Unique ID for the underlying `button` element. */
        get buttonId() { return `${this.id}-button`; }
        /** The appearance style of the button. */
        get appearance() {
            return this.buttonToggleGroup ? this.buttonToggleGroup.appearance : this._appearance;
        }
        set appearance(value) {
            this._appearance = value;
        }
        /** Whether the button is checked. */
        get checked() {
            return this.buttonToggleGroup ? this.buttonToggleGroup._isSelected(this) : this._checked;
        }
        set checked(value) {
            const newValue = coerceBooleanProperty(value);
            if (newValue !== this._checked) {
                this._checked = newValue;
                if (this.buttonToggleGroup) {
                    this.buttonToggleGroup._syncButtonToggle(this, this._checked);
                }
                this._changeDetectorRef.markForCheck();
            }
        }
        /** Whether the button is disabled. */
        get disabled() {
            return this._disabled || (this.buttonToggleGroup && this.buttonToggleGroup.disabled);
        }
        set disabled(value) { this._disabled = coerceBooleanProperty(value); }
        ngOnInit() {
            const group = this.buttonToggleGroup;
            this._isSingleSelector = group && !group.multiple;
            this.id = this.id || `mat-button-toggle-${_uniqueIdCounter++}`;
            if (this._isSingleSelector) {
                this.name = group.name;
            }
            if (group) {
                if (group._isPrechecked(this)) {
                    this.checked = true;
                }
                else if (group._isSelected(this) !== this._checked) {
                    // As as side effect of the circular dependency between the toggle group and the button,
                    // we may end up in a state where the button is supposed to be checked on init, but it
                    // isn't, because the checked value was assigned too early. This can happen when Ivy
                    // assigns the static input value before the `ngOnInit` has run.
                    group._syncButtonToggle(this, this._checked);
                }
            }
        }
        ngAfterViewInit() {
            this._focusMonitor.monitor(this._elementRef, true);
        }
        ngOnDestroy() {
            const group = this.buttonToggleGroup;
            this._focusMonitor.stopMonitoring(this._elementRef);
            // Remove the toggle from the selection once it's destroyed. Needs to happen
            // on the next tick in order to avoid "changed after checked" errors.
            if (group && group._isSelected(this)) {
                group._syncButtonToggle(this, false, false, true);
            }
        }
        /** Focuses the button. */
        focus(options) {
            this._buttonElement.nativeElement.focus(options);
        }
        /** Checks the button toggle due to an interaction with the underlying native button. */
        _onButtonClick() {
            const newChecked = this._isSingleSelector ? true : !this._checked;
            if (newChecked !== this._checked) {
                this._checked = newChecked;
                if (this.buttonToggleGroup) {
                    this.buttonToggleGroup._syncButtonToggle(this, this._checked, true);
                    this.buttonToggleGroup._onTouched();
                }
            }
            // Emit a change event when it's the single selector
            this.change.emit(new MatButtonToggleChange(this, this.value));
        }
        /**
         * Marks the button toggle as needing checking for change detection.
         * This method is exposed because the parent button toggle group will directly
         * update bound properties of the radio button.
         */
        _markForCheck() {
            // When the group value changes, the button will not be notified.
            // Use `markForCheck` to explicit update button toggle's status.
            this._changeDetectorRef.markForCheck();
        }
    }
    MatButtonToggle.decorators = [
        { type: Component, args: [{
                    selector: 'mat-button-toggle',
                    template: "<button #button class=\"mat-button-toggle-button mat-focus-indicator\"\n        type=\"button\"\n        [id]=\"buttonId\"\n        [attr.tabindex]=\"disabled ? -1 : tabIndex\"\n        [attr.aria-pressed]=\"checked\"\n        [disabled]=\"disabled || null\"\n        [attr.name]=\"name || null\"\n        [attr.aria-label]=\"ariaLabel\"\n        [attr.aria-labelledby]=\"ariaLabelledby\"\n        (click)=\"_onButtonClick()\">\n  <div class=\"mat-button-toggle-label-content\">\n    <ng-content></ng-content>\n  </div>\n</button>\n\n<div class=\"mat-button-toggle-focus-overlay\"></div>\n<div class=\"mat-button-toggle-ripple\" matRipple\n     [matRippleTrigger]=\"button\"\n     [matRippleDisabled]=\"this.disableRipple || this.disabled\">\n</div>\n",
                    encapsulation: ViewEncapsulation.None,
                    exportAs: 'matButtonToggle',
                    changeDetection: ChangeDetectionStrategy.OnPush,
                    inputs: ['disableRipple'],
                    host: {
                        '[class.mat-button-toggle-standalone]': '!buttonToggleGroup',
                        '[class.mat-button-toggle-checked]': 'checked',
                        '[class.mat-button-toggle-disabled]': 'disabled',
                        '[class.mat-button-toggle-appearance-standard]': 'appearance === "standard"',
                        'class': 'mat-button-toggle',
                        // Always reset the tabindex to -1 so it doesn't conflict with the one on the `button`,
                        // but can still receive focus from things like cdkFocusInitial.
                        '[attr.tabindex]': '-1',
                        '[attr.id]': 'id',
                        '[attr.name]': 'null',
                        '(focus)': 'focus()',
                    },
                    styles: [".mat-button-toggle-standalone,.mat-button-toggle-group{position:relative;display:inline-flex;flex-direction:row;white-space:nowrap;overflow:hidden;border-radius:2px;-webkit-tap-highlight-color:transparent}.cdk-high-contrast-active .mat-button-toggle-standalone,.cdk-high-contrast-active .mat-button-toggle-group{outline:solid 1px}.mat-button-toggle-standalone.mat-button-toggle-appearance-standard,.mat-button-toggle-group-appearance-standard{border-radius:4px}.cdk-high-contrast-active .mat-button-toggle-standalone.mat-button-toggle-appearance-standard,.cdk-high-contrast-active .mat-button-toggle-group-appearance-standard{outline:0}.mat-button-toggle-vertical{flex-direction:column}.mat-button-toggle-vertical .mat-button-toggle-label-content{display:block}.mat-button-toggle{white-space:nowrap;position:relative}.mat-button-toggle .mat-icon svg{vertical-align:top}.mat-button-toggle.cdk-keyboard-focused .mat-button-toggle-focus-overlay{opacity:1}.cdk-high-contrast-active .mat-button-toggle.cdk-keyboard-focused .mat-button-toggle-focus-overlay{opacity:.5}.mat-button-toggle-appearance-standard:not(.mat-button-toggle-disabled):hover .mat-button-toggle-focus-overlay{opacity:.04}.mat-button-toggle-appearance-standard.cdk-keyboard-focused:not(.mat-button-toggle-disabled) .mat-button-toggle-focus-overlay{opacity:.12}.cdk-high-contrast-active .mat-button-toggle-appearance-standard.cdk-keyboard-focused:not(.mat-button-toggle-disabled) .mat-button-toggle-focus-overlay{opacity:.5}@media(hover: none){.mat-button-toggle-appearance-standard:not(.mat-button-toggle-disabled):hover .mat-button-toggle-focus-overlay{display:none}}.mat-button-toggle-label-content{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;display:inline-block;line-height:36px;padding:0 16px;position:relative}.mat-button-toggle-appearance-standard .mat-button-toggle-label-content{padding:0 12px}.mat-button-toggle-label-content>*{vertical-align:middle}.mat-button-toggle-focus-overlay{border-radius:inherit;pointer-events:none;opacity:0;top:0;left:0;right:0;bottom:0;position:absolute}.mat-button-toggle-checked .mat-button-toggle-focus-overlay{border-bottom:solid 36px}.cdk-high-contrast-active .mat-button-toggle-checked .mat-button-toggle-focus-overlay{opacity:.5;height:0}.cdk-high-contrast-active .mat-button-toggle-checked.mat-button-toggle-appearance-standard .mat-button-toggle-focus-overlay{border-bottom:solid 500px}.mat-button-toggle .mat-button-toggle-ripple{top:0;left:0;right:0;bottom:0;position:absolute;pointer-events:none}.mat-button-toggle-button{border:0;background:none;color:inherit;padding:0;margin:0;font:inherit;outline:none;width:100%;cursor:pointer}.mat-button-toggle-disabled .mat-button-toggle-button{cursor:default}.mat-button-toggle-button::-moz-focus-inner{border:0}\n"]
                },] }
    ];
    MatButtonToggle.ctorParameters = () => [
        { type: MatButtonToggleGroup, decorators: [{ type: Optional }] },
        { type: ChangeDetectorRef },
        { type: ElementRef },
        { type: FocusMonitor },
        { type: String, decorators: [{ type: Attribute, args: ['tabindex',] }] },
        { type: undefined, decorators: [{ type: Optional }, { type: Inject, args: [MAT_BUTTON_TOGGLE_DEFAULT_OPTIONS,] }] }
    ];
    MatButtonToggle.propDecorators = {
        ariaLabel: [{ type: Input, args: ['aria-label',] }],
        ariaLabelledby: [{ type: Input, args: ['aria-labelledby',] }],
        _buttonElement: [{ type: ViewChild, args: ['button',] }],
        id: [{ type: Input }],
        name: [{ type: Input }],
        value: [{ type: Input }],
        tabIndex: [{ type: Input }],
        appearance: [{ type: Input }],
        checked: [{ type: Input }],
        disabled: [{ type: Input }],
        change: [{ type: Output }]
    };
    return MatButtonToggle;
})();
export { MatButtonToggle };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYnV0dG9uLXRvZ2dsZS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9idXR0b24tdG9nZ2xlL2J1dHRvbi10b2dnbGUudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFlBQVksRUFBQyxNQUFNLG1CQUFtQixDQUFDO0FBQy9DLE9BQU8sRUFBZSxxQkFBcUIsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQzFFLE9BQU8sRUFBQyxjQUFjLEVBQUMsTUFBTSwwQkFBMEIsQ0FBQztBQUN4RCxPQUFPLEVBRUwsU0FBUyxFQUNULHVCQUF1QixFQUN2QixpQkFBaUIsRUFDakIsU0FBUyxFQUNULGVBQWUsRUFDZixTQUFTLEVBQ1QsVUFBVSxFQUNWLFlBQVksRUFDWixVQUFVLEVBQ1YsS0FBSyxFQUdMLFFBQVEsRUFDUixNQUFNLEVBQ04sU0FBUyxFQUNULFNBQVMsRUFDVCxpQkFBaUIsRUFDakIsY0FBYyxFQUNkLE1BQU0sR0FFUCxNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQXVCLGlCQUFpQixFQUFDLE1BQU0sZ0JBQWdCLENBQUM7QUFDdkUsT0FBTyxFQUVMLGtCQUFrQixHQUVuQixNQUFNLHdCQUF3QixDQUFDO0FBb0JoQzs7O0dBR0c7QUFDSCxNQUFNLENBQUMsTUFBTSxpQ0FBaUMsR0FDMUMsSUFBSSxjQUFjLENBQWdDLG1DQUFtQyxDQUFDLENBQUM7QUFJM0Y7Ozs7R0FJRztBQUNILE1BQU0sQ0FBQyxNQUFNLHNDQUFzQyxHQUFRO0lBQ3pELE9BQU8sRUFBRSxpQkFBaUI7SUFDMUIsV0FBVyxFQUFFLFVBQVUsQ0FBQyxHQUFHLEVBQUUsQ0FBQyxvQkFBb0IsQ0FBQztJQUNuRCxLQUFLLEVBQUUsSUFBSTtDQUNaLENBQUM7QUFFRixJQUFJLGdCQUFnQixHQUFHLENBQUMsQ0FBQztBQUV6QixzREFBc0Q7QUFDdEQsTUFBTSxPQUFPLHFCQUFx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QyxNQUFNLEVBQUUsQ0FBQyxlQUFlLENBQUM7b0JBQ3pCLElBQUksRUFBRTt3QkFDSixzQ0FBc0MsRUFBRSxvQkFBb0I7d0JBQzVELG1DQUFtQyxFQUFFLFNBQVM7d0JBQzlDLG9DQUFvQyxFQUFFLFVBQVU7d0JBQ2hELCtDQUErQyxFQUFFLDJCQUEyQjt3QkFDNUUsT0FBTyxFQUFFLG1CQUFtQjt3QkFDNUIsdUZBQXVGO3dCQUN2RixnRUFBZ0U7d0JBQ2hFLGlCQUFpQixFQUFFLElBQUk7d0JBQ3ZCLFdBQVcsRUFBRSxJQUFJO3dCQUNqQixhQUFhLEVBQUUsTUFBTTt3QkFDckIsU0FBUyxFQUFFLFNBQVM7cUJBQ3JCOztpQkFDRjs7O2dCQStFc0Msb0JBQW9CLHVCQUE1QyxRQUFRO2dCQS9jckIsaUJBQWlCO2dCQUlqQixVQUFVO2dCQVhKLFlBQVk7NkNBMGRMLFNBQVMsU0FBQyxVQUFVO2dEQUNwQixRQUFRLFlBQUksTUFBTSxTQUFDLGlDQUFpQzs7OzRCQXpFaEUsS0FBSyxTQUFDLFlBQVk7aUNBS2xCLEtBQUssU0FBQyxpQkFBaUI7aUNBRXZCLFNBQVMsU0FBQyxRQUFRO3FCQVNsQixLQUFLO3VCQUdMLEtBQUs7d0JBR0wsS0FBSzsyQkFHTCxLQUFLOzZCQUdMLEtBQUs7MEJBVUwsS0FBSzsyQkFtQkwsS0FBSzt5QkFRTCxNQUFNOztJQTZGVCxzQkFBQztLQUFBO1NBeEtZLGVBQWUiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtGb2N1c01vbml0b3J9IGZyb20gJ0Bhbmd1bGFyL2Nkay9hMTF5JztcbmltcG9ydCB7Qm9vbGVhbklucHV0LCBjb2VyY2VCb29sZWFuUHJvcGVydHl9IGZyb20gJ0Bhbmd1bGFyL2Nkay9jb2VyY2lvbic7XG5pbXBvcnQge1NlbGVjdGlvbk1vZGVsfSBmcm9tICdAYW5ndWxhci9jZGsvY29sbGVjdGlvbnMnO1xuaW1wb3J0IHtcbiAgQWZ0ZXJDb250ZW50SW5pdCxcbiAgQXR0cmlidXRlLFxuICBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSxcbiAgQ2hhbmdlRGV0ZWN0b3JSZWYsXG4gIENvbXBvbmVudCxcbiAgQ29udGVudENoaWxkcmVuLFxuICBEaXJlY3RpdmUsXG4gIEVsZW1lbnRSZWYsXG4gIEV2ZW50RW1pdHRlcixcbiAgZm9yd2FyZFJlZixcbiAgSW5wdXQsXG4gIE9uRGVzdHJveSxcbiAgT25Jbml0LFxuICBPcHRpb25hbCxcbiAgT3V0cHV0LFxuICBRdWVyeUxpc3QsXG4gIFZpZXdDaGlsZCxcbiAgVmlld0VuY2Fwc3VsYXRpb24sXG4gIEluamVjdGlvblRva2VuLFxuICBJbmplY3QsXG4gIEFmdGVyVmlld0luaXQsXG59IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtDb250cm9sVmFsdWVBY2Nlc3NvciwgTkdfVkFMVUVfQUNDRVNTT1J9IGZyb20gJ0Bhbmd1bGFyL2Zvcm1zJztcbmltcG9ydCB7XG4gIENhbkRpc2FibGVSaXBwbGUsXG4gIG1peGluRGlzYWJsZVJpcHBsZSxcbiAgQ2FuRGlzYWJsZVJpcHBsZUN0b3IsXG59IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL2NvcmUnO1xuXG5cbi8qKlxuICogQGRlcHJlY2F0ZWQgTm8gbG9uZ2VyIHVzZWQuXG4gKiBAYnJlYWtpbmctY2hhbmdlIDExLjAuMFxuICovXG5leHBvcnQgdHlwZSBUb2dnbGVUeXBlID0gJ2NoZWNrYm94JyB8ICdyYWRpbyc7XG5cbi8qKiBQb3NzaWJsZSBhcHBlYXJhbmNlIHN0eWxlcyBmb3IgdGhlIGJ1dHRvbiB0b2dnbGUuICovXG5leHBvcnQgdHlwZSBNYXRCdXR0b25Ub2dnbGVBcHBlYXJhbmNlID0gJ2xlZ2FjeScgfCAnc3RhbmRhcmQnO1xuXG4vKipcbiAqIFJlcHJlc2VudHMgdGhlIGRlZmF1bHQgb3B0aW9ucyBmb3IgdGhlIGJ1dHRvbiB0b2dnbGUgdGhhdCBjYW4gYmUgY29uZmlndXJlZFxuICogdXNpbmcgdGhlIGBNQVRfQlVUVE9OX1RPR0dMRV9ERUZBVUxUX09QVElPTlNgIGluamVjdGlvbiB0b2tlbi5cbiAqL1xuZXhwb3J0IGludGVyZmFjZSBNYXRCdXR0b25Ub2dnbGVEZWZhdWx0T3B0aW9ucyB7XG4gIGFwcGVhcmFuY2U/OiBNYXRCdXR0b25Ub2dnbGVBcHBlYXJhbmNlO1xufVxuXG4vKipcbiAqIEluamVjdGlvbiB0b2tlbiB0aGF0IGNhbiBiZSB1c2VkIHRvIGNvbmZpZ3VyZSB0aGVcbiAqIGRlZmF1bHQgb3B0aW9ucyBmb3IgYWxsIGJ1dHRvbiB0b2dnbGVzIHdpdGhpbiBhbiBhcHAuXG4gKi9cbmV4cG9ydCBjb25zdCBNQVRfQlVUVE9OX1RPR0dMRV9ERUZBVUxUX09QVElPTlMgPVxuICAgIG5ldyBJbmplY3Rpb25Ub2tlbjxNYXRCdXR0b25Ub2dnbGVEZWZhdWx0T3B0aW9ucz4oJ01BVF9CVVRUT05fVE9HR0xFX0RFRkFVTFRfT1BUSU9OUycpO1xuXG5cblxuLyoqXG4gKiBQcm92aWRlciBFeHByZXNzaW9uIHRoYXQgYWxsb3dzIG1hdC1idXR0b24tdG9nZ2xlLWdyb3VwIHRvIHJlZ2lzdGVyIGFzIGEgQ29udHJvbFZhbHVlQWNjZXNzb3IuXG4gKiBUaGlzIGFsbG93cyBpdCB0byBzdXBwb3J0IFsobmdNb2RlbCldLlxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5leHBvcnQgY29uc3QgTUFUX0JVVFRPTl9UT0dHTEVfR1JPVVBfVkFMVUVfQUNDRVNTT1I6IGFueSA9IHtcbiAgcHJvdmlkZTogTkdfVkFMVUVfQUNDRVNTT1IsXG4gIHVzZUV4aXN0aW5nOiBmb3J3YXJkUmVmKCgpID0+IE1hdEJ1dHRvblRvZ2dsZUdyb3VwKSxcbiAgbXVsdGk6IHRydWVcbn07XG5cbmxldCBfdW5pcXVlSWRDb3VudGVyID0gMDtcblxuLyoqIENoYW5nZSBldmVudCBvYmplY3QgZW1pdHRlZCBieSBNYXRCdXR0b25Ub2dnbGUuICovXG5leHBvcnQgY2xhc3MgTWF0QnV0dG9uVG9nZ2xlQ2hhbmdlIHtcbiAgY29uc3RydWN0b3IoXG4gICAgLyoqIFRoZSBNYXRCdXR0b25Ub2dnbGUgdGhhdCBlbWl0cyB0aGUgZXZlbnQuICovXG4gICAgcHVibGljIHNvdXJjZTogTWF0QnV0dG9uVG9nZ2xlLFxuXG4gICAgLyoqIFRoZSB2YWx1ZSBhc3NpZ25lZCB0byB0aGUgTWF0QnV0dG9uVG9nZ2xlLiAqL1xuICAgIHB1YmxpYyB2YWx1ZTogYW55KSB7fVxufVxuXG4vKiogRXhjbHVzaXZlIHNlbGVjdGlvbiBidXR0b24gdG9nZ2xlIGdyb3VwIHRoYXQgYmVoYXZlcyBsaWtlIGEgcmFkaW8tYnV0dG9uIGdyb3VwLiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnbWF0LWJ1dHRvbi10b2dnbGUtZ3JvdXAnLFxuICBwcm92aWRlcnM6IFtNQVRfQlVUVE9OX1RPR0dMRV9HUk9VUF9WQUxVRV9BQ0NFU1NPUl0sXG4gIGhvc3Q6IHtcbiAgICAncm9sZSc6ICdncm91cCcsXG4gICAgJ2NsYXNzJzogJ21hdC1idXR0b24tdG9nZ2xlLWdyb3VwJyxcbiAgICAnW2F0dHIuYXJpYS1kaXNhYmxlZF0nOiAnZGlzYWJsZWQnLFxuICAgICdbY2xhc3MubWF0LWJ1dHRvbi10b2dnbGUtdmVydGljYWxdJzogJ3ZlcnRpY2FsJyxcbiAgICAnW2NsYXNzLm1hdC1idXR0b24tdG9nZ2xlLWdyb3VwLWFwcGVhcmFuY2Utc3RhbmRhcmRdJzogJ2FwcGVhcmFuY2UgPT09IFwic3RhbmRhcmRcIicsXG4gIH0sXG4gIGV4cG9ydEFzOiAnbWF0QnV0dG9uVG9nZ2xlR3JvdXAnLFxufSlcbmV4cG9ydCBjbGFzcyBNYXRCdXR0b25Ub2dnbGVHcm91cCBpbXBsZW1lbnRzIENvbnRyb2xWYWx1ZUFjY2Vzc29yLCBPbkluaXQsIEFmdGVyQ29udGVudEluaXQge1xuICBwcml2YXRlIF92ZXJ0aWNhbCA9IGZhbHNlO1xuICBwcml2YXRlIF9tdWx0aXBsZSA9IGZhbHNlO1xuICBwcml2YXRlIF9kaXNhYmxlZCA9IGZhbHNlO1xuICBwcml2YXRlIF9zZWxlY3Rpb25Nb2RlbDogU2VsZWN0aW9uTW9kZWw8TWF0QnV0dG9uVG9nZ2xlPjtcblxuICAvKipcbiAgICogUmVmZXJlbmNlIHRvIHRoZSByYXcgdmFsdWUgdGhhdCB0aGUgY29uc3VtZXIgdHJpZWQgdG8gYXNzaWduLiBUaGUgcmVhbFxuICAgKiB2YWx1ZSB3aWxsIGV4Y2x1ZGUgYW55IHZhbHVlcyBmcm9tIHRoaXMgb25lIHRoYXQgZG9uJ3QgY29ycmVzcG9uZCB0byBhXG4gICAqIHRvZ2dsZS4gVXNlZnVsIGZvciB0aGUgY2FzZXMgd2hlcmUgdGhlIHZhbHVlIGlzIGFzc2lnbmVkIGJlZm9yZSB0aGUgdG9nZ2xlc1xuICAgKiBoYXZlIGJlZW4gaW5pdGlhbGl6ZWQgb3IgYXQgdGhlIHNhbWUgdGhhdCB0aGV5J3JlIGJlaW5nIHN3YXBwZWQgb3V0LlxuICAgKi9cbiAgcHJpdmF0ZSBfcmF3VmFsdWU6IGFueTtcblxuICAvKipcbiAgICogVGhlIG1ldGhvZCB0byBiZSBjYWxsZWQgaW4gb3JkZXIgdG8gdXBkYXRlIG5nTW9kZWwuXG4gICAqIE5vdyBgbmdNb2RlbGAgYmluZGluZyBpcyBub3Qgc3VwcG9ydGVkIGluIG11bHRpcGxlIHNlbGVjdGlvbiBtb2RlLlxuICAgKi9cbiAgX2NvbnRyb2xWYWx1ZUFjY2Vzc29yQ2hhbmdlRm46ICh2YWx1ZTogYW55KSA9PiB2b2lkID0gKCkgPT4ge307XG5cbiAgLyoqIG9uVG91Y2ggZnVuY3Rpb24gcmVnaXN0ZXJlZCB2aWEgcmVnaXN0ZXJPblRvdWNoIChDb250cm9sVmFsdWVBY2Nlc3NvcikuICovXG4gIF9vblRvdWNoZWQ6ICgpID0+IGFueSA9ICgpID0+IHt9O1xuXG4gIC8qKiBDaGlsZCBidXR0b24gdG9nZ2xlIGJ1dHRvbnMuICovXG4gIEBDb250ZW50Q2hpbGRyZW4oZm9yd2FyZFJlZigoKSA9PiBNYXRCdXR0b25Ub2dnbGUpLCB7XG4gICAgLy8gTm90ZSB0aGF0IHRoaXMgd291bGQgdGVjaG5pY2FsbHkgcGljayB1cCB0b2dnbGVzXG4gICAgLy8gZnJvbSBuZXN0ZWQgZ3JvdXBzLCBidXQgdGhhdCdzIG5vdCBhIGNhc2UgdGhhdCB3ZSBzdXBwb3J0LlxuICAgIGRlc2NlbmRhbnRzOiB0cnVlXG4gIH0pIF9idXR0b25Ub2dnbGVzOiBRdWVyeUxpc3Q8TWF0QnV0dG9uVG9nZ2xlPjtcblxuICAvKiogVGhlIGFwcGVhcmFuY2UgZm9yIGFsbCB0aGUgYnV0dG9ucyBpbiB0aGUgZ3JvdXAuICovXG4gIEBJbnB1dCgpIGFwcGVhcmFuY2U6IE1hdEJ1dHRvblRvZ2dsZUFwcGVhcmFuY2U7XG5cbiAgLyoqIGBuYW1lYCBhdHRyaWJ1dGUgZm9yIHRoZSB1bmRlcmx5aW5nIGBpbnB1dGAgZWxlbWVudC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IG5hbWUoKTogc3RyaW5nIHsgcmV0dXJuIHRoaXMuX25hbWU7IH1cbiAgc2V0IG5hbWUodmFsdWU6IHN0cmluZykge1xuICAgIHRoaXMuX25hbWUgPSB2YWx1ZTtcblxuICAgIGlmICh0aGlzLl9idXR0b25Ub2dnbGVzKSB7XG4gICAgICB0aGlzLl9idXR0b25Ub2dnbGVzLmZvckVhY2godG9nZ2xlID0+IHtcbiAgICAgICAgdG9nZ2xlLm5hbWUgPSB0aGlzLl9uYW1lO1xuICAgICAgICB0b2dnbGUuX21hcmtGb3JDaGVjaygpO1xuICAgICAgfSk7XG4gICAgfVxuICB9XG4gIHByaXZhdGUgX25hbWUgPSBgbWF0LWJ1dHRvbi10b2dnbGUtZ3JvdXAtJHtfdW5pcXVlSWRDb3VudGVyKyt9YDtcblxuICAvKiogV2hldGhlciB0aGUgdG9nZ2xlIGdyb3VwIGlzIHZlcnRpY2FsLiAqL1xuICBASW5wdXQoKVxuICBnZXQgdmVydGljYWwoKTogYm9vbGVhbiB7IHJldHVybiB0aGlzLl92ZXJ0aWNhbDsgfVxuICBzZXQgdmVydGljYWwodmFsdWU6IGJvb2xlYW4pIHtcbiAgICB0aGlzLl92ZXJ0aWNhbCA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh2YWx1ZSk7XG4gIH1cblxuICAvKiogVmFsdWUgb2YgdGhlIHRvZ2dsZSBncm91cC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IHZhbHVlKCk6IGFueSB7XG4gICAgY29uc3Qgc2VsZWN0ZWQgPSB0aGlzLl9zZWxlY3Rpb25Nb2RlbCA/IHRoaXMuX3NlbGVjdGlvbk1vZGVsLnNlbGVjdGVkIDogW107XG5cbiAgICBpZiAodGhpcy5tdWx0aXBsZSkge1xuICAgICAgcmV0dXJuIHNlbGVjdGVkLm1hcCh0b2dnbGUgPT4gdG9nZ2xlLnZhbHVlKTtcbiAgICB9XG5cbiAgICByZXR1cm4gc2VsZWN0ZWRbMF0gPyBzZWxlY3RlZFswXS52YWx1ZSA6IHVuZGVmaW5lZDtcbiAgfVxuICBzZXQgdmFsdWUobmV3VmFsdWU6IGFueSkge1xuICAgIHRoaXMuX3NldFNlbGVjdGlvbkJ5VmFsdWUobmV3VmFsdWUpO1xuICAgIHRoaXMudmFsdWVDaGFuZ2UuZW1pdCh0aGlzLnZhbHVlKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBFdmVudCB0aGF0IGVtaXRzIHdoZW5ldmVyIHRoZSB2YWx1ZSBvZiB0aGUgZ3JvdXAgY2hhbmdlcy5cbiAgICogVXNlZCB0byBmYWNpbGl0YXRlIHR3by13YXkgZGF0YSBiaW5kaW5nLlxuICAgKiBAZG9jcy1wcml2YXRlXG4gICAqL1xuICBAT3V0cHV0KCkgcmVhZG9ubHkgdmFsdWVDaGFuZ2UgPSBuZXcgRXZlbnRFbWl0dGVyPGFueT4oKTtcblxuICAvKiogU2VsZWN0ZWQgYnV0dG9uIHRvZ2dsZXMgaW4gdGhlIGdyb3VwLiAqL1xuICBnZXQgc2VsZWN0ZWQoKSB7XG4gICAgY29uc3Qgc2VsZWN0ZWQgPSB0aGlzLl9zZWxlY3Rpb25Nb2RlbCA/IHRoaXMuX3NlbGVjdGlvbk1vZGVsLnNlbGVjdGVkIDogW107XG4gICAgcmV0dXJuIHRoaXMubXVsdGlwbGUgPyBzZWxlY3RlZCA6IChzZWxlY3RlZFswXSB8fCBudWxsKTtcbiAgfVxuXG4gIC8qKiBXaGV0aGVyIG11bHRpcGxlIGJ1dHRvbiB0b2dnbGVzIGNhbiBiZSBzZWxlY3RlZC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IG11bHRpcGxlKCk6IGJvb2xlYW4geyByZXR1cm4gdGhpcy5fbXVsdGlwbGU7IH1cbiAgc2V0IG11bHRpcGxlKHZhbHVlOiBib29sZWFuKSB7XG4gICAgdGhpcy5fbXVsdGlwbGUgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkodmFsdWUpO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgbXVsdGlwbGUgYnV0dG9uIHRvZ2dsZSBncm91cCBpcyBkaXNhYmxlZC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IGRpc2FibGVkKCk6IGJvb2xlYW4geyByZXR1cm4gdGhpcy5fZGlzYWJsZWQ7IH1cbiAgc2V0IGRpc2FibGVkKHZhbHVlOiBib29sZWFuKSB7XG4gICAgdGhpcy5fZGlzYWJsZWQgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkodmFsdWUpO1xuXG4gICAgaWYgKHRoaXMuX2J1dHRvblRvZ2dsZXMpIHtcbiAgICAgIHRoaXMuX2J1dHRvblRvZ2dsZXMuZm9yRWFjaCh0b2dnbGUgPT4gdG9nZ2xlLl9tYXJrRm9yQ2hlY2soKSk7XG4gICAgfVxuICB9XG5cbiAgLyoqIEV2ZW50IGVtaXR0ZWQgd2hlbiB0aGUgZ3JvdXAncyB2YWx1ZSBjaGFuZ2VzLiAqL1xuICBAT3V0cHV0KCkgcmVhZG9ubHkgY2hhbmdlOiBFdmVudEVtaXR0ZXI8TWF0QnV0dG9uVG9nZ2xlQ2hhbmdlPiA9XG4gICAgICBuZXcgRXZlbnRFbWl0dGVyPE1hdEJ1dHRvblRvZ2dsZUNoYW5nZT4oKTtcblxuICBjb25zdHJ1Y3RvcihcbiAgICBwcml2YXRlIF9jaGFuZ2VEZXRlY3RvcjogQ2hhbmdlRGV0ZWN0b3JSZWYsXG4gICAgQE9wdGlvbmFsKCkgQEluamVjdChNQVRfQlVUVE9OX1RPR0dMRV9ERUZBVUxUX09QVElPTlMpXG4gICAgICAgIGRlZmF1bHRPcHRpb25zPzogTWF0QnV0dG9uVG9nZ2xlRGVmYXVsdE9wdGlvbnMpIHtcblxuICAgICAgdGhpcy5hcHBlYXJhbmNlID1cbiAgICAgICAgICBkZWZhdWx0T3B0aW9ucyAmJiBkZWZhdWx0T3B0aW9ucy5hcHBlYXJhbmNlID8gZGVmYXVsdE9wdGlvbnMuYXBwZWFyYW5jZSA6ICdzdGFuZGFyZCc7XG4gICAgfVxuXG4gIG5nT25Jbml0KCkge1xuICAgIHRoaXMuX3NlbGVjdGlvbk1vZGVsID0gbmV3IFNlbGVjdGlvbk1vZGVsPE1hdEJ1dHRvblRvZ2dsZT4odGhpcy5tdWx0aXBsZSwgdW5kZWZpbmVkLCBmYWxzZSk7XG4gIH1cblxuICBuZ0FmdGVyQ29udGVudEluaXQoKSB7XG4gICAgdGhpcy5fc2VsZWN0aW9uTW9kZWwuc2VsZWN0KC4uLnRoaXMuX2J1dHRvblRvZ2dsZXMuZmlsdGVyKHRvZ2dsZSA9PiB0b2dnbGUuY2hlY2tlZCkpO1xuICB9XG5cbiAgLyoqXG4gICAqIFNldHMgdGhlIG1vZGVsIHZhbHVlLiBJbXBsZW1lbnRlZCBhcyBwYXJ0IG9mIENvbnRyb2xWYWx1ZUFjY2Vzc29yLlxuICAgKiBAcGFyYW0gdmFsdWUgVmFsdWUgdG8gYmUgc2V0IHRvIHRoZSBtb2RlbC5cbiAgICovXG4gIHdyaXRlVmFsdWUodmFsdWU6IGFueSkge1xuICAgIHRoaXMudmFsdWUgPSB2YWx1ZTtcbiAgICB0aGlzLl9jaGFuZ2VEZXRlY3Rvci5tYXJrRm9yQ2hlY2soKTtcbiAgfVxuXG4gIC8vIEltcGxlbWVudGVkIGFzIHBhcnQgb2YgQ29udHJvbFZhbHVlQWNjZXNzb3IuXG4gIHJlZ2lzdGVyT25DaGFuZ2UoZm46ICh2YWx1ZTogYW55KSA9PiB2b2lkKSB7XG4gICAgdGhpcy5fY29udHJvbFZhbHVlQWNjZXNzb3JDaGFuZ2VGbiA9IGZuO1xuICB9XG5cbiAgLy8gSW1wbGVtZW50ZWQgYXMgcGFydCBvZiBDb250cm9sVmFsdWVBY2Nlc3Nvci5cbiAgcmVnaXN0ZXJPblRvdWNoZWQoZm46IGFueSkge1xuICAgIHRoaXMuX29uVG91Y2hlZCA9IGZuO1xuICB9XG5cbiAgLy8gSW1wbGVtZW50ZWQgYXMgcGFydCBvZiBDb250cm9sVmFsdWVBY2Nlc3Nvci5cbiAgc2V0RGlzYWJsZWRTdGF0ZShpc0Rpc2FibGVkOiBib29sZWFuKTogdm9pZCB7XG4gICAgdGhpcy5kaXNhYmxlZCA9IGlzRGlzYWJsZWQ7XG4gIH1cblxuICAvKiogRGlzcGF0Y2ggY2hhbmdlIGV2ZW50IHdpdGggY3VycmVudCBzZWxlY3Rpb24gYW5kIGdyb3VwIHZhbHVlLiAqL1xuICBfZW1pdENoYW5nZUV2ZW50KCk6IHZvaWQge1xuICAgIGNvbnN0IHNlbGVjdGVkID0gdGhpcy5zZWxlY3RlZDtcbiAgICBjb25zdCBzb3VyY2UgPSBBcnJheS5pc0FycmF5KHNlbGVjdGVkKSA/IHNlbGVjdGVkW3NlbGVjdGVkLmxlbmd0aCAtIDFdIDogc2VsZWN0ZWQ7XG4gICAgY29uc3QgZXZlbnQgPSBuZXcgTWF0QnV0dG9uVG9nZ2xlQ2hhbmdlKHNvdXJjZSEsIHRoaXMudmFsdWUpO1xuICAgIHRoaXMuX2NvbnRyb2xWYWx1ZUFjY2Vzc29yQ2hhbmdlRm4oZXZlbnQudmFsdWUpO1xuICAgIHRoaXMuY2hhbmdlLmVtaXQoZXZlbnQpO1xuICB9XG5cbiAgLyoqXG4gICAqIFN5bmNzIGEgYnV0dG9uIHRvZ2dsZSdzIHNlbGVjdGVkIHN0YXRlIHdpdGggdGhlIG1vZGVsIHZhbHVlLlxuICAgKiBAcGFyYW0gdG9nZ2xlIFRvZ2dsZSB0byBiZSBzeW5jZWQuXG4gICAqIEBwYXJhbSBzZWxlY3QgV2hldGhlciB0aGUgdG9nZ2xlIHNob3VsZCBiZSBzZWxlY3RlZC5cbiAgICogQHBhcmFtIGlzVXNlcklucHV0IFdoZXRoZXIgdGhlIGNoYW5nZSB3YXMgYSByZXN1bHQgb2YgYSB1c2VyIGludGVyYWN0aW9uLlxuICAgKiBAcGFyYW0gZGVmZXJFdmVudHMgV2hldGhlciB0byBkZWZlciBlbWl0dGluZyB0aGUgY2hhbmdlIGV2ZW50cy5cbiAgICovXG4gIF9zeW5jQnV0dG9uVG9nZ2xlKHRvZ2dsZTogTWF0QnV0dG9uVG9nZ2xlLFxuICAgICAgICAgICAgICAgICAgICBzZWxlY3Q6IGJvb2xlYW4sXG4gICAgICAgICAgICAgICAgICAgIGlzVXNlcklucHV0ID0gZmFsc2UsXG4gICAgICAgICAgICAgICAgICAgIGRlZmVyRXZlbnRzID0gZmFsc2UpIHtcbiAgICAvLyBEZXNlbGVjdCB0aGUgY3VycmVudGx5LXNlbGVjdGVkIHRvZ2dsZSwgaWYgd2UncmUgaW4gc2luZ2xlLXNlbGVjdGlvblxuICAgIC8vIG1vZGUgYW5kIHRoZSBidXR0b24gYmVpbmcgdG9nZ2xlZCBpc24ndCBzZWxlY3RlZCBhdCB0aGUgbW9tZW50LlxuICAgIGlmICghdGhpcy5tdWx0aXBsZSAmJiB0aGlzLnNlbGVjdGVkICYmICF0b2dnbGUuY2hlY2tlZCkge1xuICAgICAgKHRoaXMuc2VsZWN0ZWQgYXMgTWF0QnV0dG9uVG9nZ2xlKS5jaGVja2VkID0gZmFsc2U7XG4gICAgfVxuXG4gICAgaWYgKHRoaXMuX3NlbGVjdGlvbk1vZGVsKSB7XG4gICAgICBpZiAoc2VsZWN0KSB7XG4gICAgICAgIHRoaXMuX3NlbGVjdGlvbk1vZGVsLnNlbGVjdCh0b2dnbGUpO1xuICAgICAgfSBlbHNlIHtcbiAgICAgICAgdGhpcy5fc2VsZWN0aW9uTW9kZWwuZGVzZWxlY3QodG9nZ2xlKTtcbiAgICAgIH1cbiAgICB9IGVsc2Uge1xuICAgICAgZGVmZXJFdmVudHMgPSB0cnVlO1xuICAgIH1cblxuICAgIC8vIFdlIG5lZWQgdG8gZGVmZXIgaW4gc29tZSBjYXNlcyBpbiBvcmRlciB0byBhdm9pZCBcImNoYW5nZWQgYWZ0ZXIgY2hlY2tlZCBlcnJvcnNcIiwgaG93ZXZlclxuICAgIC8vIHRoZSBzaWRlLWVmZmVjdCBpcyB0aGF0IHdlIG1heSBlbmQgdXAgdXBkYXRpbmcgdGhlIG1vZGVsIHZhbHVlIG91dCBvZiBzZXF1ZW5jZSBpbiBvdGhlcnNcbiAgICAvLyBUaGUgYGRlZmVyRXZlbnRzYCBmbGFnIGFsbG93cyB1cyB0byBkZWNpZGUgd2hldGhlciB0byBkbyBpdCBvbiBhIGNhc2UtYnktY2FzZSBiYXNpcy5cbiAgICBpZiAoZGVmZXJFdmVudHMpIHtcbiAgICAgIFByb21pc2UucmVzb2x2ZSgpLnRoZW4oKCkgPT4gdGhpcy5fdXBkYXRlTW9kZWxWYWx1ZShpc1VzZXJJbnB1dCkpO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLl91cGRhdGVNb2RlbFZhbHVlKGlzVXNlcklucHV0KTtcbiAgICB9XG4gIH1cblxuICAvKiogQ2hlY2tzIHdoZXRoZXIgYSBidXR0b24gdG9nZ2xlIGlzIHNlbGVjdGVkLiAqL1xuICBfaXNTZWxlY3RlZCh0b2dnbGU6IE1hdEJ1dHRvblRvZ2dsZSkge1xuICAgIHJldHVybiB0aGlzLl9zZWxlY3Rpb25Nb2RlbCAmJiB0aGlzLl9zZWxlY3Rpb25Nb2RlbC5pc1NlbGVjdGVkKHRvZ2dsZSk7XG4gIH1cblxuICAvKiogRGV0ZXJtaW5lcyB3aGV0aGVyIGEgYnV0dG9uIHRvZ2dsZSBzaG91bGQgYmUgY2hlY2tlZCBvbiBpbml0LiAqL1xuICBfaXNQcmVjaGVja2VkKHRvZ2dsZTogTWF0QnV0dG9uVG9nZ2xlKSB7XG4gICAgaWYgKHR5cGVvZiB0aGlzLl9yYXdWYWx1ZSA9PT0gJ3VuZGVmaW5lZCcpIHtcbiAgICAgIHJldHVybiBmYWxzZTtcbiAgICB9XG5cbiAgICBpZiAodGhpcy5tdWx0aXBsZSAmJiBBcnJheS5pc0FycmF5KHRoaXMuX3Jhd1ZhbHVlKSkge1xuICAgICAgcmV0dXJuIHRoaXMuX3Jhd1ZhbHVlLnNvbWUodmFsdWUgPT4gdG9nZ2xlLnZhbHVlICE9IG51bGwgJiYgdmFsdWUgPT09IHRvZ2dsZS52YWx1ZSk7XG4gICAgfVxuXG4gICAgcmV0dXJuIHRvZ2dsZS52YWx1ZSA9PT0gdGhpcy5fcmF3VmFsdWU7XG4gIH1cblxuICAvKiogVXBkYXRlcyB0aGUgc2VsZWN0aW9uIHN0YXRlIG9mIHRoZSB0b2dnbGVzIGluIHRoZSBncm91cCBiYXNlZCBvbiBhIHZhbHVlLiAqL1xuICBwcml2YXRlIF9zZXRTZWxlY3Rpb25CeVZhbHVlKHZhbHVlOiBhbnl8YW55W10pIHtcbiAgICB0aGlzLl9yYXdWYWx1ZSA9IHZhbHVlO1xuXG4gICAgaWYgKCF0aGlzLl9idXR0b25Ub2dnbGVzKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgaWYgKHRoaXMubXVsdGlwbGUgJiYgdmFsdWUpIHtcbiAgICAgIGlmICghQXJyYXkuaXNBcnJheSh2YWx1ZSkpIHtcbiAgICAgICAgdGhyb3cgRXJyb3IoJ1ZhbHVlIG11c3QgYmUgYW4gYXJyYXkgaW4gbXVsdGlwbGUtc2VsZWN0aW9uIG1vZGUuJyk7XG4gICAgICB9XG5cbiAgICAgIHRoaXMuX2NsZWFyU2VsZWN0aW9uKCk7XG4gICAgICB2YWx1ZS5mb3JFYWNoKChjdXJyZW50VmFsdWU6IGFueSkgPT4gdGhpcy5fc2VsZWN0VmFsdWUoY3VycmVudFZhbHVlKSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHRoaXMuX2NsZWFyU2VsZWN0aW9uKCk7XG4gICAgICB0aGlzLl9zZWxlY3RWYWx1ZSh2YWx1ZSk7XG4gICAgfVxuICB9XG5cbiAgLyoqIENsZWFycyB0aGUgc2VsZWN0ZWQgdG9nZ2xlcy4gKi9cbiAgcHJpdmF0ZSBfY2xlYXJTZWxlY3Rpb24oKSB7XG4gICAgdGhpcy5fc2VsZWN0aW9uTW9kZWwuY2xlYXIoKTtcbiAgICB0aGlzLl9idXR0b25Ub2dnbGVzLmZvckVhY2godG9nZ2xlID0+IHRvZ2dsZS5jaGVja2VkID0gZmFsc2UpO1xuICB9XG5cbiAgLyoqIFNlbGVjdHMgYSB2YWx1ZSBpZiB0aGVyZSdzIGEgdG9nZ2xlIHRoYXQgY29ycmVzcG9uZHMgdG8gaXQuICovXG4gIHByaXZhdGUgX3NlbGVjdFZhbHVlKHZhbHVlOiBhbnkpIHtcbiAgICBjb25zdCBjb3JyZXNwb25kaW5nT3B0aW9uID0gdGhpcy5fYnV0dG9uVG9nZ2xlcy5maW5kKHRvZ2dsZSA9PiB7XG4gICAgICByZXR1cm4gdG9nZ2xlLnZhbHVlICE9IG51bGwgJiYgdG9nZ2xlLnZhbHVlID09PSB2YWx1ZTtcbiAgICB9KTtcblxuICAgIGlmIChjb3JyZXNwb25kaW5nT3B0aW9uKSB7XG4gICAgICBjb3JyZXNwb25kaW5nT3B0aW9uLmNoZWNrZWQgPSB0cnVlO1xuICAgICAgdGhpcy5fc2VsZWN0aW9uTW9kZWwuc2VsZWN0KGNvcnJlc3BvbmRpbmdPcHRpb24pO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBTeW5jcyB1cCB0aGUgZ3JvdXAncyB2YWx1ZSB3aXRoIHRoZSBtb2RlbCBhbmQgZW1pdHMgdGhlIGNoYW5nZSBldmVudC4gKi9cbiAgcHJpdmF0ZSBfdXBkYXRlTW9kZWxWYWx1ZShpc1VzZXJJbnB1dDogYm9vbGVhbikge1xuICAgIC8vIE9ubHkgZW1pdCB0aGUgY2hhbmdlIGV2ZW50IGZvciB1c2VyIGlucHV0LlxuICAgIGlmIChpc1VzZXJJbnB1dCkge1xuICAgICAgdGhpcy5fZW1pdENoYW5nZUV2ZW50KCk7XG4gICAgfVxuXG4gICAgLy8gTm90ZTogd2UgZW1pdCB0aGlzIG9uZSBubyBtYXR0ZXIgd2hldGhlciBpdCB3YXMgYSB1c2VyIGludGVyYWN0aW9uLCBiZWNhdXNlXG4gICAgLy8gaXQgaXMgdXNlZCBieSBBbmd1bGFyIHRvIHN5bmMgdXAgdGhlIHR3by13YXkgZGF0YSBiaW5kaW5nLlxuICAgIHRoaXMudmFsdWVDaGFuZ2UuZW1pdCh0aGlzLnZhbHVlKTtcbiAgfVxuXG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV9kaXNhYmxlZDogQm9vbGVhbklucHV0O1xuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfbXVsdGlwbGU6IEJvb2xlYW5JbnB1dDtcbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX3ZlcnRpY2FsOiBCb29sZWFuSW5wdXQ7XG59XG5cbi8vIEJvaWxlcnBsYXRlIGZvciBhcHBseWluZyBtaXhpbnMgdG8gdGhlIE1hdEJ1dHRvblRvZ2dsZSBjbGFzcy5cbi8qKiBAZG9jcy1wcml2YXRlICovXG5jbGFzcyBNYXRCdXR0b25Ub2dnbGVCYXNlIHt9XG5jb25zdCBfTWF0QnV0dG9uVG9nZ2xlTWl4aW5CYXNlOiBDYW5EaXNhYmxlUmlwcGxlQ3RvciAmIHR5cGVvZiBNYXRCdXR0b25Ub2dnbGVCYXNlID1cbiAgICBtaXhpbkRpc2FibGVSaXBwbGUoTWF0QnV0dG9uVG9nZ2xlQmFzZSk7XG5cbi8qKiBTaW5nbGUgYnV0dG9uIGluc2lkZSBvZiBhIHRvZ2dsZSBncm91cC4gKi9cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ21hdC1idXR0b24tdG9nZ2xlJyxcbiAgdGVtcGxhdGVVcmw6ICdidXR0b24tdG9nZ2xlLmh0bWwnLFxuICBzdHlsZVVybHM6IFsnYnV0dG9uLXRvZ2dsZS5jc3MnXSxcbiAgZW5jYXBzdWxhdGlvbjogVmlld0VuY2Fwc3VsYXRpb24uTm9uZSxcbiAgZXhwb3J0QXM6ICdtYXRCdXR0b25Ub2dnbGUnLFxuICBjaGFuZ2VEZXRlY3Rpb246IENoYW5nZURldGVjdGlvblN0cmF0ZWd5Lk9uUHVzaCxcbiAgaW5wdXRzOiBbJ2Rpc2FibGVSaXBwbGUnXSxcbiAgaG9zdDoge1xuICAgICdbY2xhc3MubWF0LWJ1dHRvbi10b2dnbGUtc3RhbmRhbG9uZV0nOiAnIWJ1dHRvblRvZ2dsZUdyb3VwJyxcbiAgICAnW2NsYXNzLm1hdC1idXR0b24tdG9nZ2xlLWNoZWNrZWRdJzogJ2NoZWNrZWQnLFxuICAgICdbY2xhc3MubWF0LWJ1dHRvbi10b2dnbGUtZGlzYWJsZWRdJzogJ2Rpc2FibGVkJyxcbiAgICAnW2NsYXNzLm1hdC1idXR0b24tdG9nZ2xlLWFwcGVhcmFuY2Utc3RhbmRhcmRdJzogJ2FwcGVhcmFuY2UgPT09IFwic3RhbmRhcmRcIicsXG4gICAgJ2NsYXNzJzogJ21hdC1idXR0b24tdG9nZ2xlJyxcbiAgICAvLyBBbHdheXMgcmVzZXQgdGhlIHRhYmluZGV4IHRvIC0xIHNvIGl0IGRvZXNuJ3QgY29uZmxpY3Qgd2l0aCB0aGUgb25lIG9uIHRoZSBgYnV0dG9uYCxcbiAgICAvLyBidXQgY2FuIHN0aWxsIHJlY2VpdmUgZm9jdXMgZnJvbSB0aGluZ3MgbGlrZSBjZGtGb2N1c0luaXRpYWwuXG4gICAgJ1thdHRyLnRhYmluZGV4XSc6ICctMScsXG4gICAgJ1thdHRyLmlkXSc6ICdpZCcsXG4gICAgJ1thdHRyLm5hbWVdJzogJ251bGwnLFxuICAgICcoZm9jdXMpJzogJ2ZvY3VzKCknLFxuICB9XG59KVxuZXhwb3J0IGNsYXNzIE1hdEJ1dHRvblRvZ2dsZSBleHRlbmRzIF9NYXRCdXR0b25Ub2dnbGVNaXhpbkJhc2UgaW1wbGVtZW50cyBPbkluaXQsIEFmdGVyVmlld0luaXQsXG4gIENhbkRpc2FibGVSaXBwbGUsIE9uRGVzdHJveSB7XG5cbiAgcHJpdmF0ZSBfaXNTaW5nbGVTZWxlY3RvciA9IGZhbHNlO1xuICBwcml2YXRlIF9jaGVja2VkID0gZmFsc2U7XG5cbiAgLyoqXG4gICAqIEF0dGFjaGVkIHRvIHRoZSBhcmlhLWxhYmVsIGF0dHJpYnV0ZSBvZiB0aGUgaG9zdCBlbGVtZW50LiBJbiBtb3N0IGNhc2VzLCBhcmlhLWxhYmVsbGVkYnkgd2lsbFxuICAgKiB0YWtlIHByZWNlZGVuY2Ugc28gdGhpcyBtYXkgYmUgb21pdHRlZC5cbiAgICovXG4gIEBJbnB1dCgnYXJpYS1sYWJlbCcpIGFyaWFMYWJlbDogc3RyaW5nO1xuXG4gIC8qKlxuICAgKiBVc2VycyBjYW4gc3BlY2lmeSB0aGUgYGFyaWEtbGFiZWxsZWRieWAgYXR0cmlidXRlIHdoaWNoIHdpbGwgYmUgZm9yd2FyZGVkIHRvIHRoZSBpbnB1dCBlbGVtZW50XG4gICAqL1xuICBASW5wdXQoJ2FyaWEtbGFiZWxsZWRieScpIGFyaWFMYWJlbGxlZGJ5OiBzdHJpbmcgfCBudWxsID0gbnVsbDtcblxuICBAVmlld0NoaWxkKCdidXR0b24nKSBfYnV0dG9uRWxlbWVudDogRWxlbWVudFJlZjxIVE1MQnV0dG9uRWxlbWVudD47XG5cbiAgLyoqIFRoZSBwYXJlbnQgYnV0dG9uIHRvZ2dsZSBncm91cCAoZXhjbHVzaXZlIHNlbGVjdGlvbikuIE9wdGlvbmFsLiAqL1xuICBidXR0b25Ub2dnbGVHcm91cDogTWF0QnV0dG9uVG9nZ2xlR3JvdXA7XG5cbiAgLyoqIFVuaXF1ZSBJRCBmb3IgdGhlIHVuZGVybHlpbmcgYGJ1dHRvbmAgZWxlbWVudC4gKi9cbiAgZ2V0IGJ1dHRvbklkKCk6IHN0cmluZyB7IHJldHVybiBgJHt0aGlzLmlkfS1idXR0b25gOyB9XG5cbiAgLyoqIFRoZSB1bmlxdWUgSUQgZm9yIHRoaXMgYnV0dG9uIHRvZ2dsZS4gKi9cbiAgQElucHV0KCkgaWQ6IHN0cmluZztcblxuICAvKiogSFRNTCdzICduYW1lJyBhdHRyaWJ1dGUgdXNlZCB0byBncm91cCByYWRpb3MgZm9yIHVuaXF1ZSBzZWxlY3Rpb24uICovXG4gIEBJbnB1dCgpIG5hbWU6IHN0cmluZztcblxuICAvKiogTWF0QnV0dG9uVG9nZ2xlR3JvdXAgcmVhZHMgdGhpcyB0byBhc3NpZ24gaXRzIG93biB2YWx1ZS4gKi9cbiAgQElucHV0KCkgdmFsdWU6IGFueTtcblxuICAvKiogVGFiaW5kZXggZm9yIHRoZSB0b2dnbGUuICovXG4gIEBJbnB1dCgpIHRhYkluZGV4OiBudW1iZXIgfCBudWxsO1xuXG4gIC8qKiBUaGUgYXBwZWFyYW5jZSBzdHlsZSBvZiB0aGUgYnV0dG9uLiAqL1xuICBASW5wdXQoKVxuICBnZXQgYXBwZWFyYW5jZSgpOiBNYXRCdXR0b25Ub2dnbGVBcHBlYXJhbmNlIHtcbiAgICByZXR1cm4gdGhpcy5idXR0b25Ub2dnbGVHcm91cCA/IHRoaXMuYnV0dG9uVG9nZ2xlR3JvdXAuYXBwZWFyYW5jZSA6IHRoaXMuX2FwcGVhcmFuY2U7XG4gIH1cbiAgc2V0IGFwcGVhcmFuY2UodmFsdWU6IE1hdEJ1dHRvblRvZ2dsZUFwcGVhcmFuY2UpIHtcbiAgICB0aGlzLl9hcHBlYXJhbmNlID0gdmFsdWU7XG4gIH1cbiAgcHJpdmF0ZSBfYXBwZWFyYW5jZTogTWF0QnV0dG9uVG9nZ2xlQXBwZWFyYW5jZTtcblxuICAvKiogV2hldGhlciB0aGUgYnV0dG9uIGlzIGNoZWNrZWQuICovXG4gIEBJbnB1dCgpXG4gIGdldCBjaGVja2VkKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLmJ1dHRvblRvZ2dsZUdyb3VwID8gdGhpcy5idXR0b25Ub2dnbGVHcm91cC5faXNTZWxlY3RlZCh0aGlzKSA6IHRoaXMuX2NoZWNrZWQ7XG4gIH1cbiAgc2V0IGNoZWNrZWQodmFsdWU6IGJvb2xlYW4pIHtcbiAgICBjb25zdCBuZXdWYWx1ZSA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh2YWx1ZSk7XG5cbiAgICBpZiAobmV3VmFsdWUgIT09IHRoaXMuX2NoZWNrZWQpIHtcbiAgICAgIHRoaXMuX2NoZWNrZWQgPSBuZXdWYWx1ZTtcblxuICAgICAgaWYgKHRoaXMuYnV0dG9uVG9nZ2xlR3JvdXApIHtcbiAgICAgICAgdGhpcy5idXR0b25Ub2dnbGVHcm91cC5fc3luY0J1dHRvblRvZ2dsZSh0aGlzLCB0aGlzLl9jaGVja2VkKTtcbiAgICAgIH1cblxuICAgICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3JSZWYubWFya0ZvckNoZWNrKCk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGJ1dHRvbiBpcyBkaXNhYmxlZC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IGRpc2FibGVkKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLl9kaXNhYmxlZCB8fCAodGhpcy5idXR0b25Ub2dnbGVHcm91cCAmJiB0aGlzLmJ1dHRvblRvZ2dsZUdyb3VwLmRpc2FibGVkKTtcbiAgfVxuICBzZXQgZGlzYWJsZWQodmFsdWU6IGJvb2xlYW4pIHsgdGhpcy5fZGlzYWJsZWQgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkodmFsdWUpOyB9XG4gIHByaXZhdGUgX2Rpc2FibGVkOiBib29sZWFuID0gZmFsc2U7XG5cbiAgLyoqIEV2ZW50IGVtaXR0ZWQgd2hlbiB0aGUgZ3JvdXAgdmFsdWUgY2hhbmdlcy4gKi9cbiAgQE91dHB1dCgpIHJlYWRvbmx5IGNoYW5nZTogRXZlbnRFbWl0dGVyPE1hdEJ1dHRvblRvZ2dsZUNoYW5nZT4gPVxuICAgICAgbmV3IEV2ZW50RW1pdHRlcjxNYXRCdXR0b25Ub2dnbGVDaGFuZ2U+KCk7XG5cbiAgY29uc3RydWN0b3IoQE9wdGlvbmFsKCkgdG9nZ2xlR3JvdXA6IE1hdEJ1dHRvblRvZ2dsZUdyb3VwLFxuICAgICAgICAgICAgICBwcml2YXRlIF9jaGFuZ2VEZXRlY3RvclJlZjogQ2hhbmdlRGV0ZWN0b3JSZWYsXG4gICAgICAgICAgICAgIHByaXZhdGUgX2VsZW1lbnRSZWY6IEVsZW1lbnRSZWY8SFRNTEVsZW1lbnQ+LFxuICAgICAgICAgICAgICBwcml2YXRlIF9mb2N1c01vbml0b3I6IEZvY3VzTW9uaXRvcixcbiAgICAgICAgICAgICAgQEF0dHJpYnV0ZSgndGFiaW5kZXgnKSBkZWZhdWx0VGFiSW5kZXg6IHN0cmluZyxcbiAgICAgICAgICAgICAgQE9wdGlvbmFsKCkgQEluamVjdChNQVRfQlVUVE9OX1RPR0dMRV9ERUZBVUxUX09QVElPTlMpXG4gICAgICAgICAgICAgICAgICBkZWZhdWx0T3B0aW9ucz86IE1hdEJ1dHRvblRvZ2dsZURlZmF1bHRPcHRpb25zKSB7XG4gICAgc3VwZXIoKTtcblxuICAgIGNvbnN0IHBhcnNlZFRhYkluZGV4ID0gTnVtYmVyKGRlZmF1bHRUYWJJbmRleCk7XG4gICAgdGhpcy50YWJJbmRleCA9IChwYXJzZWRUYWJJbmRleCB8fCBwYXJzZWRUYWJJbmRleCA9PT0gMCkgPyBwYXJzZWRUYWJJbmRleCA6IG51bGw7XG4gICAgdGhpcy5idXR0b25Ub2dnbGVHcm91cCA9IHRvZ2dsZUdyb3VwO1xuICAgIHRoaXMuYXBwZWFyYW5jZSA9XG4gICAgICAgIGRlZmF1bHRPcHRpb25zICYmIGRlZmF1bHRPcHRpb25zLmFwcGVhcmFuY2UgPyBkZWZhdWx0T3B0aW9ucy5hcHBlYXJhbmNlIDogJ3N0YW5kYXJkJztcbiAgfVxuXG4gIG5nT25Jbml0KCkge1xuICAgIGNvbnN0IGdyb3VwID0gdGhpcy5idXR0b25Ub2dnbGVHcm91cDtcbiAgICB0aGlzLl9pc1NpbmdsZVNlbGVjdG9yID0gZ3JvdXAgJiYgIWdyb3VwLm11bHRpcGxlO1xuICAgIHRoaXMuaWQgPSB0aGlzLmlkIHx8IGBtYXQtYnV0dG9uLXRvZ2dsZS0ke191bmlxdWVJZENvdW50ZXIrK31gO1xuXG4gICAgaWYgKHRoaXMuX2lzU2luZ2xlU2VsZWN0b3IpIHtcbiAgICAgIHRoaXMubmFtZSA9IGdyb3VwLm5hbWU7XG4gICAgfVxuXG4gICAgaWYgKGdyb3VwKSB7XG4gICAgICBpZiAoZ3JvdXAuX2lzUHJlY2hlY2tlZCh0aGlzKSkge1xuICAgICAgICB0aGlzLmNoZWNrZWQgPSB0cnVlO1xuICAgICAgfSBlbHNlIGlmIChncm91cC5faXNTZWxlY3RlZCh0aGlzKSAhPT0gdGhpcy5fY2hlY2tlZCkge1xuICAgICAgICAvLyBBcyBhcyBzaWRlIGVmZmVjdCBvZiB0aGUgY2lyY3VsYXIgZGVwZW5kZW5jeSBiZXR3ZWVuIHRoZSB0b2dnbGUgZ3JvdXAgYW5kIHRoZSBidXR0b24sXG4gICAgICAgIC8vIHdlIG1heSBlbmQgdXAgaW4gYSBzdGF0ZSB3aGVyZSB0aGUgYnV0dG9uIGlzIHN1cHBvc2VkIHRvIGJlIGNoZWNrZWQgb24gaW5pdCwgYnV0IGl0XG4gICAgICAgIC8vIGlzbid0LCBiZWNhdXNlIHRoZSBjaGVja2VkIHZhbHVlIHdhcyBhc3NpZ25lZCB0b28gZWFybHkuIFRoaXMgY2FuIGhhcHBlbiB3aGVuIEl2eVxuICAgICAgICAvLyBhc3NpZ25zIHRoZSBzdGF0aWMgaW5wdXQgdmFsdWUgYmVmb3JlIHRoZSBgbmdPbkluaXRgIGhhcyBydW4uXG4gICAgICAgIGdyb3VwLl9zeW5jQnV0dG9uVG9nZ2xlKHRoaXMsIHRoaXMuX2NoZWNrZWQpO1xuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIG5nQWZ0ZXJWaWV3SW5pdCgpIHtcbiAgICB0aGlzLl9mb2N1c01vbml0b3IubW9uaXRvcih0aGlzLl9lbGVtZW50UmVmLCB0cnVlKTtcbiAgfVxuXG4gIG5nT25EZXN0cm95KCkge1xuICAgIGNvbnN0IGdyb3VwID0gdGhpcy5idXR0b25Ub2dnbGVHcm91cDtcblxuICAgIHRoaXMuX2ZvY3VzTW9uaXRvci5zdG9wTW9uaXRvcmluZyh0aGlzLl9lbGVtZW50UmVmKTtcblxuICAgIC8vIFJlbW92ZSB0aGUgdG9nZ2xlIGZyb20gdGhlIHNlbGVjdGlvbiBvbmNlIGl0J3MgZGVzdHJveWVkLiBOZWVkcyB0byBoYXBwZW5cbiAgICAvLyBvbiB0aGUgbmV4dCB0aWNrIGluIG9yZGVyIHRvIGF2b2lkIFwiY2hhbmdlZCBhZnRlciBjaGVja2VkXCIgZXJyb3JzLlxuICAgIGlmIChncm91cCAmJiBncm91cC5faXNTZWxlY3RlZCh0aGlzKSkge1xuICAgICAgZ3JvdXAuX3N5bmNCdXR0b25Ub2dnbGUodGhpcywgZmFsc2UsIGZhbHNlLCB0cnVlKTtcbiAgICB9XG4gIH1cblxuICAvKiogRm9jdXNlcyB0aGUgYnV0dG9uLiAqL1xuICBmb2N1cyhvcHRpb25zPzogRm9jdXNPcHRpb25zKTogdm9pZCB7XG4gICAgdGhpcy5fYnV0dG9uRWxlbWVudC5uYXRpdmVFbGVtZW50LmZvY3VzKG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqIENoZWNrcyB0aGUgYnV0dG9uIHRvZ2dsZSBkdWUgdG8gYW4gaW50ZXJhY3Rpb24gd2l0aCB0aGUgdW5kZXJseWluZyBuYXRpdmUgYnV0dG9uLiAqL1xuICBfb25CdXR0b25DbGljaygpIHtcbiAgICBjb25zdCBuZXdDaGVja2VkID0gdGhpcy5faXNTaW5nbGVTZWxlY3RvciA/IHRydWUgOiAhdGhpcy5fY2hlY2tlZDtcblxuICAgIGlmIChuZXdDaGVja2VkICE9PSB0aGlzLl9jaGVja2VkKSB7XG4gICAgICB0aGlzLl9jaGVja2VkID0gbmV3Q2hlY2tlZDtcbiAgICAgIGlmICh0aGlzLmJ1dHRvblRvZ2dsZUdyb3VwKSB7XG4gICAgICAgIHRoaXMuYnV0dG9uVG9nZ2xlR3JvdXAuX3N5bmNCdXR0b25Ub2dnbGUodGhpcywgdGhpcy5fY2hlY2tlZCwgdHJ1ZSk7XG4gICAgICAgIHRoaXMuYnV0dG9uVG9nZ2xlR3JvdXAuX29uVG91Y2hlZCgpO1xuICAgICAgfVxuICAgIH1cbiAgICAvLyBFbWl0IGEgY2hhbmdlIGV2ZW50IHdoZW4gaXQncyB0aGUgc2luZ2xlIHNlbGVjdG9yXG4gICAgdGhpcy5jaGFuZ2UuZW1pdChuZXcgTWF0QnV0dG9uVG9nZ2xlQ2hhbmdlKHRoaXMsIHRoaXMudmFsdWUpKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBNYXJrcyB0aGUgYnV0dG9uIHRvZ2dsZSBhcyBuZWVkaW5nIGNoZWNraW5nIGZvciBjaGFuZ2UgZGV0ZWN0aW9uLlxuICAgKiBUaGlzIG1ldGhvZCBpcyBleHBvc2VkIGJlY2F1c2UgdGhlIHBhcmVudCBidXR0b24gdG9nZ2xlIGdyb3VwIHdpbGwgZGlyZWN0bHlcbiAgICogdXBkYXRlIGJvdW5kIHByb3BlcnRpZXMgb2YgdGhlIHJhZGlvIGJ1dHRvbi5cbiAgICovXG4gIF9tYXJrRm9yQ2hlY2soKSB7XG4gICAgLy8gV2hlbiB0aGUgZ3JvdXAgdmFsdWUgY2hhbmdlcywgdGhlIGJ1dHRvbiB3aWxsIG5vdCBiZSBub3RpZmllZC5cbiAgICAvLyBVc2UgYG1hcmtGb3JDaGVja2AgdG8gZXhwbGljaXQgdXBkYXRlIGJ1dHRvbiB0b2dnbGUncyBzdGF0dXMuXG4gICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3JSZWYubWFya0ZvckNoZWNrKCk7XG4gIH1cblxuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfY2hlY2tlZDogQm9vbGVhbklucHV0O1xuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfZGlzYWJsZWQ6IEJvb2xlYW5JbnB1dDtcbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX3ZlcnRpY2FsOiBCb29sZWFuSW5wdXQ7XG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV9tdWx0aXBsZTogQm9vbGVhbklucHV0O1xuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfZGlzYWJsZVJpcHBsZTogQm9vbGVhbklucHV0O1xufVxuIl19