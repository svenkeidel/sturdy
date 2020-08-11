/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { FocusKeyManager } from '@angular/cdk/a11y';
import { coerceBooleanProperty } from '@angular/cdk/coercion';
import { SelectionModel } from '@angular/cdk/collections';
import { A, DOWN_ARROW, END, ENTER, hasModifierKey, HOME, SPACE, UP_ARROW, } from '@angular/cdk/keycodes';
import { Attribute, ChangeDetectionStrategy, ChangeDetectorRef, Component, ContentChild, ContentChildren, ElementRef, EventEmitter, forwardRef, Inject, Input, isDevMode, Output, QueryList, ViewChild, ViewEncapsulation, } from '@angular/core';
import { NG_VALUE_ACCESSOR } from '@angular/forms';
import { MatLine, mixinDisableRipple, setLines, } from '@angular/material/core';
import { Subject } from 'rxjs';
import { startWith, takeUntil } from 'rxjs/operators';
import { MatListAvatarCssMatStyler, MatListIconCssMatStyler } from './list';
class MatSelectionListBase {
}
const _MatSelectionListMixinBase = mixinDisableRipple(MatSelectionListBase);
class MatListOptionBase {
}
const _MatListOptionMixinBase = mixinDisableRipple(MatListOptionBase);
/** @docs-private */
export const MAT_SELECTION_LIST_VALUE_ACCESSOR = {
    provide: NG_VALUE_ACCESSOR,
    useExisting: forwardRef(() => MatSelectionList),
    multi: true
};
/** Change event that is being fired whenever the selected state of an option changes. */
export class MatSelectionListChange {
    constructor(
    /** Reference to the selection list that emitted the event. */
    source, 
    /** Reference to the option that has been changed. */
    option) {
        this.source = source;
        this.option = option;
    }
}
/**
 * Component for list-options of selection-list. Each list-option can automatically
 * generate a checkbox and can put current item into the selectionModel of selection-list
 * if the current item is selected.
 */
let MatListOption = /** @class */ (() => {
    class MatListOption extends _MatListOptionMixinBase {
        constructor(_element, _changeDetector, 
        /** @docs-private */
        selectionList) {
            super();
            this._element = _element;
            this._changeDetector = _changeDetector;
            this.selectionList = selectionList;
            this._selected = false;
            this._disabled = false;
            this._hasFocus = false;
            /** Whether the label should appear before or after the checkbox. Defaults to 'after' */
            this.checkboxPosition = 'after';
            /**
             * This is set to true after the first OnChanges cycle so we don't clear the value of `selected`
             * in the first cycle.
             */
            this._inputsInitialized = false;
        }
        /** Theme color of the list option. This sets the color of the checkbox. */
        get color() { return this._color || this.selectionList.color; }
        set color(newValue) { this._color = newValue; }
        /** Value of the option */
        get value() { return this._value; }
        set value(newValue) {
            if (this.selected && newValue !== this.value && this._inputsInitialized) {
                this.selected = false;
            }
            this._value = newValue;
        }
        /** Whether the option is disabled. */
        get disabled() { return this._disabled || (this.selectionList && this.selectionList.disabled); }
        set disabled(value) {
            const newValue = coerceBooleanProperty(value);
            if (newValue !== this._disabled) {
                this._disabled = newValue;
                this._changeDetector.markForCheck();
            }
        }
        /** Whether the option is selected. */
        get selected() { return this.selectionList.selectedOptions.isSelected(this); }
        set selected(value) {
            const isSelected = coerceBooleanProperty(value);
            if (isSelected !== this._selected) {
                this._setSelected(isSelected);
                this.selectionList._reportValueChange();
            }
        }
        ngOnInit() {
            const list = this.selectionList;
            if (list._value && list._value.some(value => list.compareWith(value, this._value))) {
                this._setSelected(true);
            }
            const wasSelected = this._selected;
            // List options that are selected at initialization can't be reported properly to the form
            // control. This is because it takes some time until the selection-list knows about all
            // available options. Also it can happen that the ControlValueAccessor has an initial value
            // that should be used instead. Deferring the value change report to the next tick ensures
            // that the form control value is not being overwritten.
            Promise.resolve().then(() => {
                if (this._selected || wasSelected) {
                    this.selected = true;
                    this._changeDetector.markForCheck();
                }
            });
            this._inputsInitialized = true;
        }
        ngAfterContentInit() {
            setLines(this._lines, this._element);
        }
        ngOnDestroy() {
            if (this.selected) {
                // We have to delay this until the next tick in order
                // to avoid changed after checked errors.
                Promise.resolve().then(() => {
                    this.selected = false;
                });
            }
            const hadFocus = this._hasFocus;
            const newActiveItem = this.selectionList._removeOptionFromList(this);
            // Only move focus if this option was focused at the time it was destroyed.
            if (hadFocus && newActiveItem) {
                newActiveItem.focus();
            }
        }
        /** Toggles the selection state of the option. */
        toggle() {
            this.selected = !this.selected;
        }
        /** Allows for programmatic focusing of the option. */
        focus() {
            this._element.nativeElement.focus();
        }
        /**
         * Returns the list item's text label. Implemented as a part of the FocusKeyManager.
         * @docs-private
         */
        getLabel() {
            return this._text ? (this._text.nativeElement.textContent || '') : '';
        }
        /** Whether this list item should show a ripple effect when clicked. */
        _isRippleDisabled() {
            return this.disabled || this.disableRipple || this.selectionList.disableRipple;
        }
        _handleClick() {
            if (!this.disabled && (this.selectionList.multiple || !this.selected)) {
                this.toggle();
                // Emit a change event if the selected state of the option changed through user interaction.
                this.selectionList._emitChangeEvent(this);
            }
        }
        _handleFocus() {
            this.selectionList._setFocusedOption(this);
            this._hasFocus = true;
        }
        _handleBlur() {
            this.selectionList._onTouched();
            this._hasFocus = false;
        }
        /** Retrieves the DOM element of the component host. */
        _getHostElement() {
            return this._element.nativeElement;
        }
        /** Sets the selected state of the option. Returns whether the value has changed. */
        _setSelected(selected) {
            if (selected === this._selected) {
                return false;
            }
            this._selected = selected;
            if (selected) {
                this.selectionList.selectedOptions.select(this);
            }
            else {
                this.selectionList.selectedOptions.deselect(this);
            }
            this._changeDetector.markForCheck();
            return true;
        }
        /**
         * Notifies Angular that the option needs to be checked in the next change detection run. Mainly
         * used to trigger an update of the list option if the disabled state of the selection list
         * changed.
         */
        _markForCheck() {
            this._changeDetector.markForCheck();
        }
    }
    MatListOption.decorators = [
        { type: Component, args: [{
                    selector: 'mat-list-option',
                    exportAs: 'matListOption',
                    inputs: ['disableRipple'],
                    host: {
                        'role': 'option',
                        'class': 'mat-list-item mat-list-option mat-focus-indicator',
                        '(focus)': '_handleFocus()',
                        '(blur)': '_handleBlur()',
                        '(click)': '_handleClick()',
                        '[class.mat-list-item-disabled]': 'disabled',
                        '[class.mat-list-item-with-avatar]': '_avatar || _icon',
                        // Manually set the "primary" or "warn" class if the color has been explicitly
                        // set to "primary" or "warn". The pseudo checkbox picks up these classes for
                        // its theme.
                        '[class.mat-primary]': 'color === "primary"',
                        // Even though accent is the default, we need to set this class anyway, because the  list might
                        // be placed inside a parent that has one of the other colors with a higher specificity.
                        '[class.mat-accent]': 'color !== "primary" && color !== "warn"',
                        '[class.mat-warn]': 'color === "warn"',
                        '[class.mat-list-single-selected-option]': 'selected && !selectionList.multiple',
                        '[attr.aria-selected]': 'selected',
                        '[attr.aria-disabled]': 'disabled',
                        '[attr.tabindex]': '-1',
                    },
                    template: "<div class=\"mat-list-item-content\"\n  [class.mat-list-item-content-reverse]=\"checkboxPosition == 'after'\">\n\n  <div mat-ripple\n    class=\"mat-list-item-ripple\"\n    [matRippleTrigger]=\"_getHostElement()\"\n    [matRippleDisabled]=\"_isRippleDisabled()\"></div>\n\n  <mat-pseudo-checkbox\n    *ngIf=\"selectionList.multiple\"\n    [state]=\"selected ? 'checked' : 'unchecked'\"\n    [disabled]=\"disabled\"></mat-pseudo-checkbox>\n\n  <div class=\"mat-list-text\" #text><ng-content></ng-content></div>\n\n  <ng-content select=\"[mat-list-avatar], [mat-list-icon], [matListAvatar], [matListIcon]\">\n  </ng-content>\n\n</div>\n",
                    encapsulation: ViewEncapsulation.None,
                    changeDetection: ChangeDetectionStrategy.OnPush
                },] }
    ];
    MatListOption.ctorParameters = () => [
        { type: ElementRef },
        { type: ChangeDetectorRef },
        { type: MatSelectionList, decorators: [{ type: Inject, args: [forwardRef(() => MatSelectionList),] }] }
    ];
    MatListOption.propDecorators = {
        _avatar: [{ type: ContentChild, args: [MatListAvatarCssMatStyler,] }],
        _icon: [{ type: ContentChild, args: [MatListIconCssMatStyler,] }],
        _lines: [{ type: ContentChildren, args: [MatLine, { descendants: true },] }],
        _text: [{ type: ViewChild, args: ['text',] }],
        checkboxPosition: [{ type: Input }],
        color: [{ type: Input }],
        value: [{ type: Input }],
        disabled: [{ type: Input }],
        selected: [{ type: Input }]
    };
    return MatListOption;
})();
export { MatListOption };
/**
 * Material Design list component where each item is a selectable option. Behaves as a listbox.
 */
let MatSelectionList = /** @class */ (() => {
    class MatSelectionList extends _MatSelectionListMixinBase {
        constructor(_element, 
        // @breaking-change 11.0.0 Remove `tabIndex` parameter.
        tabIndex, _changeDetector) {
            super();
            this._element = _element;
            this._changeDetector = _changeDetector;
            this._multiple = true;
            this._contentInitialized = false;
            /** Emits a change event whenever the selected state of an option changes. */
            this.selectionChange = new EventEmitter();
            /**
             * Tabindex of the selection list.
             * @breaking-change 11.0.0 Remove `tabIndex` input.
             */
            this.tabIndex = 0;
            /** Theme color of the selection list. This sets the checkbox color for all list options. */
            this.color = 'accent';
            /**
             * Function used for comparing an option against the selected value when determining which
             * options should appear as selected. The first argument is the value of an options. The second
             * one is a value from the selected value. A boolean must be returned.
             */
            this.compareWith = (a1, a2) => a1 === a2;
            this._disabled = false;
            /** The currently selected options. */
            this.selectedOptions = new SelectionModel(this._multiple);
            /** The tabindex of the selection list. */
            this._tabIndex = -1;
            /** View to model callback that should be called whenever the selected options change. */
            this._onChange = (_) => { };
            /** Emits when the list has been destroyed. */
            this._destroyed = new Subject();
            /** View to model callback that should be called if the list or its options lost focus. */
            this._onTouched = () => { };
        }
        /** Whether the selection list is disabled. */
        get disabled() { return this._disabled; }
        set disabled(value) {
            this._disabled = coerceBooleanProperty(value);
            // The `MatSelectionList` and `MatListOption` are using the `OnPush` change detection
            // strategy. Therefore the options will not check for any changes if the `MatSelectionList`
            // changed its state. Since we know that a change to `disabled` property of the list affects
            // the state of the options, we manually mark each option for check.
            this._markOptionsForCheck();
        }
        /** Whether selection is limited to one or multiple items (default multiple). */
        get multiple() { return this._multiple; }
        set multiple(value) {
            const newValue = coerceBooleanProperty(value);
            if (newValue !== this._multiple) {
                if (isDevMode() && this._contentInitialized) {
                    throw new Error('Cannot change `multiple` mode of mat-selection-list after initialization.');
                }
                this._multiple = newValue;
                this.selectedOptions = new SelectionModel(this._multiple, this.selectedOptions.selected);
            }
        }
        ngAfterContentInit() {
            this._contentInitialized = true;
            this._keyManager = new FocusKeyManager(this.options)
                .withWrap()
                .withTypeAhead()
                // Allow disabled items to be focusable. For accessibility reasons, there must be a way for
                // screenreader users, that allows reading the different options of the list.
                .skipPredicate(() => false)
                .withAllowedModifierKeys(['shiftKey']);
            if (this._value) {
                this._setOptionsFromValues(this._value);
            }
            // If the user attempts to tab out of the selection list, allow focus to escape.
            this._keyManager.tabOut.pipe(takeUntil(this._destroyed)).subscribe(() => {
                this._allowFocusEscape();
            });
            // When the number of options change, update the tabindex of the selection list.
            this.options.changes.pipe(startWith(null), takeUntil(this._destroyed)).subscribe(() => {
                this._updateTabIndex();
            });
            // Sync external changes to the model back to the options.
            this.selectedOptions.changed.pipe(takeUntil(this._destroyed)).subscribe(event => {
                if (event.added) {
                    for (let item of event.added) {
                        item.selected = true;
                    }
                }
                if (event.removed) {
                    for (let item of event.removed) {
                        item.selected = false;
                    }
                }
            });
        }
        ngOnChanges(changes) {
            const disableRippleChanges = changes['disableRipple'];
            const colorChanges = changes['color'];
            if ((disableRippleChanges && !disableRippleChanges.firstChange) ||
                (colorChanges && !colorChanges.firstChange)) {
                this._markOptionsForCheck();
            }
        }
        ngOnDestroy() {
            this._destroyed.next();
            this._destroyed.complete();
            this._isDestroyed = true;
        }
        /** Focuses the selection list. */
        focus(options) {
            this._element.nativeElement.focus(options);
        }
        /** Selects all of the options. */
        selectAll() {
            this._setAllOptionsSelected(true);
        }
        /** Deselects all of the options. */
        deselectAll() {
            this._setAllOptionsSelected(false);
        }
        /** Sets the focused option of the selection-list. */
        _setFocusedOption(option) {
            this._keyManager.updateActiveItem(option);
        }
        /**
         * Removes an option from the selection list and updates the active item.
         * @returns Currently-active item.
         */
        _removeOptionFromList(option) {
            const optionIndex = this._getOptionIndex(option);
            if (optionIndex > -1 && this._keyManager.activeItemIndex === optionIndex) {
                // Check whether the option is the last item
                if (optionIndex > 0) {
                    this._keyManager.updateActiveItem(optionIndex - 1);
                }
                else if (optionIndex === 0 && this.options.length > 1) {
                    this._keyManager.updateActiveItem(Math.min(optionIndex + 1, this.options.length - 1));
                }
            }
            return this._keyManager.activeItem;
        }
        /** Passes relevant key presses to our key manager. */
        _keydown(event) {
            const keyCode = event.keyCode;
            const manager = this._keyManager;
            const previousFocusIndex = manager.activeItemIndex;
            const hasModifier = hasModifierKey(event);
            switch (keyCode) {
                case SPACE:
                case ENTER:
                    if (!hasModifier && !manager.isTyping()) {
                        this._toggleFocusedOption();
                        // Always prevent space from scrolling the page since the list has focus
                        event.preventDefault();
                    }
                    break;
                case HOME:
                case END:
                    if (!hasModifier) {
                        keyCode === HOME ? manager.setFirstItemActive() : manager.setLastItemActive();
                        event.preventDefault();
                    }
                    break;
                default:
                    // The "A" key gets special treatment, because it's used for the "select all" functionality.
                    if (keyCode === A && this.multiple && hasModifierKey(event, 'ctrlKey') &&
                        !manager.isTyping()) {
                        const shouldSelect = this.options.some(option => !option.disabled && !option.selected);
                        this._setAllOptionsSelected(shouldSelect, true);
                        event.preventDefault();
                    }
                    else {
                        manager.onKeydown(event);
                    }
            }
            if (this.multiple && (keyCode === UP_ARROW || keyCode === DOWN_ARROW) && event.shiftKey &&
                manager.activeItemIndex !== previousFocusIndex) {
                this._toggleFocusedOption();
            }
        }
        /** Reports a value change to the ControlValueAccessor */
        _reportValueChange() {
            // Stop reporting value changes after the list has been destroyed. This avoids
            // cases where the list might wrongly reset its value once it is removed, but
            // the form control is still live.
            if (this.options && !this._isDestroyed) {
                const value = this._getSelectedOptionValues();
                this._onChange(value);
                this._value = value;
            }
        }
        /** Emits a change event if the selected state of an option changed. */
        _emitChangeEvent(option) {
            this.selectionChange.emit(new MatSelectionListChange(this, option));
        }
        /**
         * When the selection list is focused, we want to move focus to an option within the list. Do this
         * by setting the appropriate option to be active.
         */
        _onFocus() {
            const activeIndex = this._keyManager.activeItemIndex;
            if (!activeIndex || (activeIndex === -1)) {
                // If there is no active index, set focus to the first option.
                this._keyManager.setFirstItemActive();
            }
            else {
                // Otherwise, set focus to the active option.
                this._keyManager.setActiveItem(activeIndex);
            }
        }
        /** Implemented as part of ControlValueAccessor. */
        writeValue(values) {
            this._value = values;
            if (this.options) {
                this._setOptionsFromValues(values || []);
            }
        }
        /** Implemented as a part of ControlValueAccessor. */
        setDisabledState(isDisabled) {
            this.disabled = isDisabled;
        }
        /** Implemented as part of ControlValueAccessor. */
        registerOnChange(fn) {
            this._onChange = fn;
        }
        /** Implemented as part of ControlValueAccessor. */
        registerOnTouched(fn) {
            this._onTouched = fn;
        }
        /** Sets the selected options based on the specified values. */
        _setOptionsFromValues(values) {
            this.options.forEach(option => option._setSelected(false));
            values.forEach(value => {
                const correspondingOption = this.options.find(option => {
                    // Skip options that are already in the model. This allows us to handle cases
                    // where the same primitive value is selected multiple times.
                    return option.selected ? false : this.compareWith(option.value, value);
                });
                if (correspondingOption) {
                    correspondingOption._setSelected(true);
                }
            });
        }
        /** Returns the values of the selected options. */
        _getSelectedOptionValues() {
            return this.options.filter(option => option.selected).map(option => option.value);
        }
        /** Toggles the state of the currently focused option if enabled. */
        _toggleFocusedOption() {
            let focusedIndex = this._keyManager.activeItemIndex;
            if (focusedIndex != null && this._isValidIndex(focusedIndex)) {
                let focusedOption = this.options.toArray()[focusedIndex];
                if (focusedOption && !focusedOption.disabled && (this._multiple || !focusedOption.selected)) {
                    focusedOption.toggle();
                    // Emit a change event because the focused option changed its state through user
                    // interaction.
                    this._emitChangeEvent(focusedOption);
                }
            }
        }
        /**
         * Sets the selected state on all of the options
         * and emits an event if anything changed.
         */
        _setAllOptionsSelected(isSelected, skipDisabled) {
            // Keep track of whether anything changed, because we only want to
            // emit the changed event when something actually changed.
            let hasChanged = false;
            this.options.forEach(option => {
                if ((!skipDisabled || !option.disabled) && option._setSelected(isSelected)) {
                    hasChanged = true;
                }
            });
            if (hasChanged) {
                this._reportValueChange();
            }
        }
        /**
         * Utility to ensure all indexes are valid.
         * @param index The index to be checked.
         * @returns True if the index is valid for our list of options.
         */
        _isValidIndex(index) {
            return index >= 0 && index < this.options.length;
        }
        /** Returns the index of the specified list option. */
        _getOptionIndex(option) {
            return this.options.toArray().indexOf(option);
        }
        /** Marks all the options to be checked in the next change detection run. */
        _markOptionsForCheck() {
            if (this.options) {
                this.options.forEach(option => option._markForCheck());
            }
        }
        /**
         * Removes the tabindex from the selection list and resets it back afterwards, allowing the user
         * to tab out of it. This prevents the list from capturing focus and redirecting it back within
         * the list, creating a focus trap if it user tries to tab away.
         */
        _allowFocusEscape() {
            this._tabIndex = -1;
            setTimeout(() => {
                this._tabIndex = 0;
                this._changeDetector.markForCheck();
            });
        }
        /** Updates the tabindex based upon if the selection list is empty. */
        _updateTabIndex() {
            this._tabIndex = (this.options.length === 0) ? -1 : 0;
        }
    }
    MatSelectionList.decorators = [
        { type: Component, args: [{
                    selector: 'mat-selection-list',
                    exportAs: 'matSelectionList',
                    inputs: ['disableRipple'],
                    host: {
                        'role': 'listbox',
                        'class': 'mat-selection-list mat-list-base',
                        '(focus)': '_onFocus()',
                        '(keydown)': '_keydown($event)',
                        '[attr.aria-multiselectable]': 'multiple',
                        '[attr.aria-disabled]': 'disabled.toString()',
                        '[attr.tabindex]': '_tabIndex',
                    },
                    template: '<ng-content></ng-content>',
                    encapsulation: ViewEncapsulation.None,
                    providers: [MAT_SELECTION_LIST_VALUE_ACCESSOR],
                    changeDetection: ChangeDetectionStrategy.OnPush,
                    styles: [".mat-subheader{display:flex;box-sizing:border-box;padding:16px;align-items:center}.mat-list-base .mat-subheader{margin:0}.mat-list-base{padding-top:8px;display:block;-webkit-tap-highlight-color:transparent}.mat-list-base .mat-subheader{height:48px;line-height:16px}.mat-list-base .mat-subheader:first-child{margin-top:-8px}.mat-list-base .mat-list-item,.mat-list-base .mat-list-option{display:block;height:48px;-webkit-tap-highlight-color:transparent;width:100%;padding:0}.mat-list-base .mat-list-item .mat-list-item-content,.mat-list-base .mat-list-option .mat-list-item-content{display:flex;flex-direction:row;align-items:center;box-sizing:border-box;padding:0 16px;position:relative;height:inherit}.mat-list-base .mat-list-item .mat-list-item-content-reverse,.mat-list-base .mat-list-option .mat-list-item-content-reverse{display:flex;align-items:center;padding:0 16px;flex-direction:row-reverse;justify-content:space-around}.mat-list-base .mat-list-item .mat-list-item-ripple,.mat-list-base .mat-list-option .mat-list-item-ripple{top:0;left:0;right:0;bottom:0;position:absolute;pointer-events:none}.mat-list-base .mat-list-item.mat-list-item-with-avatar,.mat-list-base .mat-list-option.mat-list-item-with-avatar{height:56px}.mat-list-base .mat-list-item.mat-2-line,.mat-list-base .mat-list-option.mat-2-line{height:72px}.mat-list-base .mat-list-item.mat-3-line,.mat-list-base .mat-list-option.mat-3-line{height:88px}.mat-list-base .mat-list-item.mat-multi-line,.mat-list-base .mat-list-option.mat-multi-line{height:auto}.mat-list-base .mat-list-item.mat-multi-line .mat-list-item-content,.mat-list-base .mat-list-option.mat-multi-line .mat-list-item-content{padding-top:16px;padding-bottom:16px}.mat-list-base .mat-list-item .mat-list-text,.mat-list-base .mat-list-option .mat-list-text{display:flex;flex-direction:column;width:100%;box-sizing:border-box;overflow:hidden;padding:0}.mat-list-base .mat-list-item .mat-list-text>*,.mat-list-base .mat-list-option .mat-list-text>*{margin:0;padding:0;font-weight:normal;font-size:inherit}.mat-list-base .mat-list-item .mat-list-text:empty,.mat-list-base .mat-list-option .mat-list-text:empty{display:none}.mat-list-base .mat-list-item.mat-list-item-with-avatar .mat-list-item-content .mat-list-text,.mat-list-base .mat-list-item.mat-list-option .mat-list-item-content .mat-list-text,.mat-list-base .mat-list-option.mat-list-item-with-avatar .mat-list-item-content .mat-list-text,.mat-list-base .mat-list-option.mat-list-option .mat-list-item-content .mat-list-text{padding-right:0;padding-left:16px}[dir=rtl] .mat-list-base .mat-list-item.mat-list-item-with-avatar .mat-list-item-content .mat-list-text,[dir=rtl] .mat-list-base .mat-list-item.mat-list-option .mat-list-item-content .mat-list-text,[dir=rtl] .mat-list-base .mat-list-option.mat-list-item-with-avatar .mat-list-item-content .mat-list-text,[dir=rtl] .mat-list-base .mat-list-option.mat-list-option .mat-list-item-content .mat-list-text{padding-right:16px;padding-left:0}.mat-list-base .mat-list-item.mat-list-item-with-avatar .mat-list-item-content-reverse .mat-list-text,.mat-list-base .mat-list-item.mat-list-option .mat-list-item-content-reverse .mat-list-text,.mat-list-base .mat-list-option.mat-list-item-with-avatar .mat-list-item-content-reverse .mat-list-text,.mat-list-base .mat-list-option.mat-list-option .mat-list-item-content-reverse .mat-list-text{padding-left:0;padding-right:16px}[dir=rtl] .mat-list-base .mat-list-item.mat-list-item-with-avatar .mat-list-item-content-reverse .mat-list-text,[dir=rtl] .mat-list-base .mat-list-item.mat-list-option .mat-list-item-content-reverse .mat-list-text,[dir=rtl] .mat-list-base .mat-list-option.mat-list-item-with-avatar .mat-list-item-content-reverse .mat-list-text,[dir=rtl] .mat-list-base .mat-list-option.mat-list-option .mat-list-item-content-reverse .mat-list-text{padding-right:0;padding-left:16px}.mat-list-base .mat-list-item.mat-list-item-with-avatar.mat-list-option .mat-list-item-content-reverse .mat-list-text,.mat-list-base .mat-list-item.mat-list-item-with-avatar.mat-list-option .mat-list-item-content .mat-list-text,.mat-list-base .mat-list-option.mat-list-item-with-avatar.mat-list-option .mat-list-item-content-reverse .mat-list-text,.mat-list-base .mat-list-option.mat-list-item-with-avatar.mat-list-option .mat-list-item-content .mat-list-text{padding-right:16px;padding-left:16px}.mat-list-base .mat-list-item .mat-list-avatar,.mat-list-base .mat-list-option .mat-list-avatar{flex-shrink:0;width:40px;height:40px;border-radius:50%;object-fit:cover}.mat-list-base .mat-list-item .mat-list-avatar~.mat-divider-inset,.mat-list-base .mat-list-option .mat-list-avatar~.mat-divider-inset{margin-left:72px;width:calc(100% - 72px)}[dir=rtl] .mat-list-base .mat-list-item .mat-list-avatar~.mat-divider-inset,[dir=rtl] .mat-list-base .mat-list-option .mat-list-avatar~.mat-divider-inset{margin-left:auto;margin-right:72px}.mat-list-base .mat-list-item .mat-list-icon,.mat-list-base .mat-list-option .mat-list-icon{flex-shrink:0;width:24px;height:24px;font-size:24px;box-sizing:content-box;border-radius:50%;padding:4px}.mat-list-base .mat-list-item .mat-list-icon~.mat-divider-inset,.mat-list-base .mat-list-option .mat-list-icon~.mat-divider-inset{margin-left:64px;width:calc(100% - 64px)}[dir=rtl] .mat-list-base .mat-list-item .mat-list-icon~.mat-divider-inset,[dir=rtl] .mat-list-base .mat-list-option .mat-list-icon~.mat-divider-inset{margin-left:auto;margin-right:64px}.mat-list-base .mat-list-item .mat-divider,.mat-list-base .mat-list-option .mat-divider{position:absolute;bottom:0;left:0;width:100%;margin:0}[dir=rtl] .mat-list-base .mat-list-item .mat-divider,[dir=rtl] .mat-list-base .mat-list-option .mat-divider{margin-left:auto;margin-right:0}.mat-list-base .mat-list-item .mat-divider.mat-divider-inset,.mat-list-base .mat-list-option .mat-divider.mat-divider-inset{position:absolute}.mat-list-base[dense]{padding-top:4px;display:block}.mat-list-base[dense] .mat-subheader{height:40px;line-height:8px}.mat-list-base[dense] .mat-subheader:first-child{margin-top:-4px}.mat-list-base[dense] .mat-list-item,.mat-list-base[dense] .mat-list-option{display:block;height:40px;-webkit-tap-highlight-color:transparent;width:100%;padding:0}.mat-list-base[dense] .mat-list-item .mat-list-item-content,.mat-list-base[dense] .mat-list-option .mat-list-item-content{display:flex;flex-direction:row;align-items:center;box-sizing:border-box;padding:0 16px;position:relative;height:inherit}.mat-list-base[dense] .mat-list-item .mat-list-item-content-reverse,.mat-list-base[dense] .mat-list-option .mat-list-item-content-reverse{display:flex;align-items:center;padding:0 16px;flex-direction:row-reverse;justify-content:space-around}.mat-list-base[dense] .mat-list-item .mat-list-item-ripple,.mat-list-base[dense] .mat-list-option .mat-list-item-ripple{top:0;left:0;right:0;bottom:0;position:absolute;pointer-events:none}.mat-list-base[dense] .mat-list-item.mat-list-item-with-avatar,.mat-list-base[dense] .mat-list-option.mat-list-item-with-avatar{height:48px}.mat-list-base[dense] .mat-list-item.mat-2-line,.mat-list-base[dense] .mat-list-option.mat-2-line{height:60px}.mat-list-base[dense] .mat-list-item.mat-3-line,.mat-list-base[dense] .mat-list-option.mat-3-line{height:76px}.mat-list-base[dense] .mat-list-item.mat-multi-line,.mat-list-base[dense] .mat-list-option.mat-multi-line{height:auto}.mat-list-base[dense] .mat-list-item.mat-multi-line .mat-list-item-content,.mat-list-base[dense] .mat-list-option.mat-multi-line .mat-list-item-content{padding-top:16px;padding-bottom:16px}.mat-list-base[dense] .mat-list-item .mat-list-text,.mat-list-base[dense] .mat-list-option .mat-list-text{display:flex;flex-direction:column;width:100%;box-sizing:border-box;overflow:hidden;padding:0}.mat-list-base[dense] .mat-list-item .mat-list-text>*,.mat-list-base[dense] .mat-list-option .mat-list-text>*{margin:0;padding:0;font-weight:normal;font-size:inherit}.mat-list-base[dense] .mat-list-item .mat-list-text:empty,.mat-list-base[dense] .mat-list-option .mat-list-text:empty{display:none}.mat-list-base[dense] .mat-list-item.mat-list-item-with-avatar .mat-list-item-content .mat-list-text,.mat-list-base[dense] .mat-list-item.mat-list-option .mat-list-item-content .mat-list-text,.mat-list-base[dense] .mat-list-option.mat-list-item-with-avatar .mat-list-item-content .mat-list-text,.mat-list-base[dense] .mat-list-option.mat-list-option .mat-list-item-content .mat-list-text{padding-right:0;padding-left:16px}[dir=rtl] .mat-list-base[dense] .mat-list-item.mat-list-item-with-avatar .mat-list-item-content .mat-list-text,[dir=rtl] .mat-list-base[dense] .mat-list-item.mat-list-option .mat-list-item-content .mat-list-text,[dir=rtl] .mat-list-base[dense] .mat-list-option.mat-list-item-with-avatar .mat-list-item-content .mat-list-text,[dir=rtl] .mat-list-base[dense] .mat-list-option.mat-list-option .mat-list-item-content .mat-list-text{padding-right:16px;padding-left:0}.mat-list-base[dense] .mat-list-item.mat-list-item-with-avatar .mat-list-item-content-reverse .mat-list-text,.mat-list-base[dense] .mat-list-item.mat-list-option .mat-list-item-content-reverse .mat-list-text,.mat-list-base[dense] .mat-list-option.mat-list-item-with-avatar .mat-list-item-content-reverse .mat-list-text,.mat-list-base[dense] .mat-list-option.mat-list-option .mat-list-item-content-reverse .mat-list-text{padding-left:0;padding-right:16px}[dir=rtl] .mat-list-base[dense] .mat-list-item.mat-list-item-with-avatar .mat-list-item-content-reverse .mat-list-text,[dir=rtl] .mat-list-base[dense] .mat-list-item.mat-list-option .mat-list-item-content-reverse .mat-list-text,[dir=rtl] .mat-list-base[dense] .mat-list-option.mat-list-item-with-avatar .mat-list-item-content-reverse .mat-list-text,[dir=rtl] .mat-list-base[dense] .mat-list-option.mat-list-option .mat-list-item-content-reverse .mat-list-text{padding-right:0;padding-left:16px}.mat-list-base[dense] .mat-list-item.mat-list-item-with-avatar.mat-list-option .mat-list-item-content-reverse .mat-list-text,.mat-list-base[dense] .mat-list-item.mat-list-item-with-avatar.mat-list-option .mat-list-item-content .mat-list-text,.mat-list-base[dense] .mat-list-option.mat-list-item-with-avatar.mat-list-option .mat-list-item-content-reverse .mat-list-text,.mat-list-base[dense] .mat-list-option.mat-list-item-with-avatar.mat-list-option .mat-list-item-content .mat-list-text{padding-right:16px;padding-left:16px}.mat-list-base[dense] .mat-list-item .mat-list-avatar,.mat-list-base[dense] .mat-list-option .mat-list-avatar{flex-shrink:0;width:36px;height:36px;border-radius:50%;object-fit:cover}.mat-list-base[dense] .mat-list-item .mat-list-avatar~.mat-divider-inset,.mat-list-base[dense] .mat-list-option .mat-list-avatar~.mat-divider-inset{margin-left:68px;width:calc(100% - 68px)}[dir=rtl] .mat-list-base[dense] .mat-list-item .mat-list-avatar~.mat-divider-inset,[dir=rtl] .mat-list-base[dense] .mat-list-option .mat-list-avatar~.mat-divider-inset{margin-left:auto;margin-right:68px}.mat-list-base[dense] .mat-list-item .mat-list-icon,.mat-list-base[dense] .mat-list-option .mat-list-icon{flex-shrink:0;width:20px;height:20px;font-size:20px;box-sizing:content-box;border-radius:50%;padding:4px}.mat-list-base[dense] .mat-list-item .mat-list-icon~.mat-divider-inset,.mat-list-base[dense] .mat-list-option .mat-list-icon~.mat-divider-inset{margin-left:60px;width:calc(100% - 60px)}[dir=rtl] .mat-list-base[dense] .mat-list-item .mat-list-icon~.mat-divider-inset,[dir=rtl] .mat-list-base[dense] .mat-list-option .mat-list-icon~.mat-divider-inset{margin-left:auto;margin-right:60px}.mat-list-base[dense] .mat-list-item .mat-divider,.mat-list-base[dense] .mat-list-option .mat-divider{position:absolute;bottom:0;left:0;width:100%;margin:0}[dir=rtl] .mat-list-base[dense] .mat-list-item .mat-divider,[dir=rtl] .mat-list-base[dense] .mat-list-option .mat-divider{margin-left:auto;margin-right:0}.mat-list-base[dense] .mat-list-item .mat-divider.mat-divider-inset,.mat-list-base[dense] .mat-list-option .mat-divider.mat-divider-inset{position:absolute}.mat-nav-list a{text-decoration:none;color:inherit}.mat-nav-list .mat-list-item{cursor:pointer;outline:none}mat-action-list button{background:none;color:inherit;border:none;font:inherit;outline:inherit;-webkit-tap-highlight-color:transparent;text-align:left}[dir=rtl] mat-action-list button{text-align:right}mat-action-list button::-moz-focus-inner{border:0}mat-action-list .mat-list-item{cursor:pointer;outline:inherit}.mat-list-option:not(.mat-list-item-disabled){cursor:pointer;outline:none}.mat-list-item-disabled{pointer-events:none}.cdk-high-contrast-active .mat-list-item-disabled{opacity:.5}.cdk-high-contrast-active :host .mat-list-item-disabled{opacity:.5}.cdk-high-contrast-active .mat-selection-list:focus{outline-style:dotted}.cdk-high-contrast-active .mat-list-option:hover,.cdk-high-contrast-active .mat-list-option:focus,.cdk-high-contrast-active .mat-nav-list .mat-list-item:hover,.cdk-high-contrast-active .mat-nav-list .mat-list-item:focus,.cdk-high-contrast-active mat-action-list .mat-list-item:hover,.cdk-high-contrast-active mat-action-list .mat-list-item:focus{outline:dotted 1px}.cdk-high-contrast-active .mat-list-single-selected-option::after{content:\"\";position:absolute;top:50%;right:16px;transform:translateY(-50%);width:10px;height:0;border-bottom:solid 10px;border-radius:10px}.cdk-high-contrast-active [dir=rtl] .mat-list-single-selected-option::after{right:auto;left:16px}@media(hover: none){.mat-list-option:not(.mat-list-item-disabled):hover,.mat-nav-list .mat-list-item:not(.mat-list-item-disabled):hover,.mat-action-list .mat-list-item:not(.mat-list-item-disabled):hover{background:none}}\n"]
                },] }
    ];
    MatSelectionList.ctorParameters = () => [
        { type: ElementRef },
        { type: String, decorators: [{ type: Attribute, args: ['tabindex',] }] },
        { type: ChangeDetectorRef }
    ];
    MatSelectionList.propDecorators = {
        options: [{ type: ContentChildren, args: [MatListOption, { descendants: true },] }],
        selectionChange: [{ type: Output }],
        tabIndex: [{ type: Input }],
        color: [{ type: Input }],
        compareWith: [{ type: Input }],
        disabled: [{ type: Input }],
        multiple: [{ type: Input }]
    };
    return MatSelectionList;
})();
export { MatSelectionList };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic2VsZWN0aW9uLWxpc3QuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvbGlzdC9zZWxlY3Rpb24tbGlzdC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQWtCLGVBQWUsRUFBQyxNQUFNLG1CQUFtQixDQUFDO0FBQ25FLE9BQU8sRUFBZSxxQkFBcUIsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQzFFLE9BQU8sRUFBQyxjQUFjLEVBQUMsTUFBTSwwQkFBMEIsQ0FBQztBQUN4RCxPQUFPLEVBQ0wsQ0FBQyxFQUNELFVBQVUsRUFDVixHQUFHLEVBQ0gsS0FBSyxFQUNMLGNBQWMsRUFDZCxJQUFJLEVBQ0osS0FBSyxFQUNMLFFBQVEsR0FDVCxNQUFNLHVCQUF1QixDQUFDO0FBQy9CLE9BQU8sRUFFTCxTQUFTLEVBQ1QsdUJBQXVCLEVBQ3ZCLGlCQUFpQixFQUNqQixTQUFTLEVBQ1QsWUFBWSxFQUNaLGVBQWUsRUFDZixVQUFVLEVBQ1YsWUFBWSxFQUNaLFVBQVUsRUFDVixNQUFNLEVBQ04sS0FBSyxFQUNMLFNBQVMsRUFJVCxNQUFNLEVBQ04sU0FBUyxFQUVULFNBQVMsRUFDVCxpQkFBaUIsR0FDbEIsTUFBTSxlQUFlLENBQUM7QUFDdkIsT0FBTyxFQUF1QixpQkFBaUIsRUFBQyxNQUFNLGdCQUFnQixDQUFDO0FBQ3ZFLE9BQU8sRUFHTCxPQUFPLEVBQ1Asa0JBQWtCLEVBQ2xCLFFBQVEsR0FFVCxNQUFNLHdCQUF3QixDQUFDO0FBQ2hDLE9BQU8sRUFBQyxPQUFPLEVBQUMsTUFBTSxNQUFNLENBQUM7QUFDN0IsT0FBTyxFQUFDLFNBQVMsRUFBRSxTQUFTLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQUNwRCxPQUFPLEVBQUMseUJBQXlCLEVBQUUsdUJBQXVCLEVBQUMsTUFBTSxRQUFRLENBQUM7QUFFMUUsTUFBTSxvQkFBb0I7Q0FBRztBQUM3QixNQUFN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XG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtGb2N1c2FibGVPcHRpb24sIEZvY3VzS2V5TWFuYWdlcn0gZnJvbSAnQGFuZ3VsYXIvY2RrL2ExMXknO1xuaW1wb3J0IHtCb29sZWFuSW5wdXQsIGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eX0gZnJvbSAnQGFuZ3VsYXIvY2RrL2NvZXJjaW9uJztcbmltcG9ydCB7U2VsZWN0aW9uTW9kZWx9IGZyb20gJ0Bhbmd1bGFyL2Nkay9jb2xsZWN0aW9ucyc7XG5pbXBvcnQge1xuICBBLFxuICBET1dOX0FSUk9XLFxuICBFTkQsXG4gIEVOVEVSLFxuICBoYXNNb2RpZmllcktleSxcbiAgSE9NRSxcbiAgU1BBQ0UsXG4gIFVQX0FSUk9XLFxufSBmcm9tICdAYW5ndWxhci9jZGsva2V5Y29kZXMnO1xuaW1wb3J0IHtcbiAgQWZ0ZXJDb250ZW50SW5pdCxcbiAgQXR0cmlidXRlLFxuICBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSxcbiAgQ2hhbmdlRGV0ZWN0b3JSZWYsXG4gIENvbXBvbmVudCxcbiAgQ29udGVudENoaWxkLFxuICBDb250ZW50Q2hpbGRyZW4sXG4gIEVsZW1lbnRSZWYsXG4gIEV2ZW50RW1pdHRlcixcbiAgZm9yd2FyZFJlZixcbiAgSW5qZWN0LFxuICBJbnB1dCxcbiAgaXNEZXZNb2RlLFxuICBPbkNoYW5nZXMsXG4gIE9uRGVzdHJveSxcbiAgT25Jbml0LFxuICBPdXRwdXQsXG4gIFF1ZXJ5TGlzdCxcbiAgU2ltcGxlQ2hhbmdlcyxcbiAgVmlld0NoaWxkLFxuICBWaWV3RW5jYXBzdWxhdGlvbixcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQge0NvbnRyb2xWYWx1ZUFjY2Vzc29yLCBOR19WQUxVRV9BQ0NFU1NPUn0gZnJvbSAnQGFuZ3VsYXIvZm9ybXMnO1xuaW1wb3J0IHtcbiAgQ2FuRGlzYWJsZVJpcHBsZSxcbiAgQ2FuRGlzYWJsZVJpcHBsZUN0b3IsXG4gIE1hdExpbmUsXG4gIG1peGluRGlzYWJsZVJpcHBsZSxcbiAgc2V0TGluZXMsXG4gIFRoZW1lUGFsZXR0ZSxcbn0gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvY29yZSc7XG5pbXBvcnQge1N1YmplY3R9IGZyb20gJ3J4anMnO1xuaW1wb3J0IHtzdGFydFdpdGgsIHRha2VVbnRpbH0gZnJvbSAncnhqcy9vcGVyYXRvcnMnO1xuaW1wb3J0IHtNYXRMaXN0QXZhdGFyQ3NzTWF0U3R5bGVyLCBNYXRMaXN0SWNvbkNzc01hdFN0eWxlcn0gZnJvbSAnLi9saXN0JztcblxuY2xhc3MgTWF0U2VsZWN0aW9uTGlzdEJhc2Uge31cbmNvbnN0IF9NYXRTZWxlY3Rpb25MaXN0TWl4aW5CYXNlOiBDYW5EaXNhYmxlUmlwcGxlQ3RvciAmIHR5cGVvZiBNYXRTZWxlY3Rpb25MaXN0QmFzZSA9XG4gICAgbWl4aW5EaXNhYmxlUmlwcGxlKE1hdFNlbGVjdGlvbkxpc3RCYXNlKTtcblxuY2xhc3MgTWF0TGlzdE9wdGlvbkJhc2Uge31cbmNvbnN0IF9NYXRMaXN0T3B0aW9uTWl4aW5CYXNlOiBDYW5EaXNhYmxlUmlwcGxlQ3RvciAmIHR5cGVvZiBNYXRMaXN0T3B0aW9uQmFzZSA9XG4gICAgbWl4aW5EaXNhYmxlUmlwcGxlKE1hdExpc3RPcHRpb25CYXNlKTtcblxuLyoqIEBkb2NzLXByaXZhdGUgKi9cbmV4cG9ydCBjb25zdCBNQVRfU0VMRUNUSU9OX0xJU1RfVkFMVUVfQUNDRVNTT1I6IGFueSA9IHtcbiAgcHJvdmlkZTogTkdfVkFMVUVfQUNDRVNTT1IsXG4gIHVzZUV4aXN0aW5nOiBmb3J3YXJkUmVmKCgpID0+IE1hdFNlbGVjdGlvbkxpc3QpLFxuICBtdWx0aTogdHJ1ZVxufTtcblxuLyoqIENoYW5nZSBldmVudCB0aGF0IGlzIGJlaW5nIGZpcmVkIHdoZW5ldmVyIHRoZSBzZWxlY3RlZCBzdGF0ZSBvZiBhbiBvcHRpb24gY2hhbmdlcy4gKi9cbmV4cG9ydCBjbGFzcyBNYXRTZWxlY3Rpb25MaXN0Q2hhbmdlIHtcbiAgY29uc3RydWN0b3IoXG4gICAgLyoqIFJlZmVyZW5jZSB0byB0aGUgc2VsZWN0aW9uIGxpc3QgdGhhdCBlbWl0dGVkIHRoZSBldmVudC4gKi9cbiAgICBwdWJsaWMgc291cmNlOiBNYXRTZWxlY3Rpb25MaXN0LFxuICAgIC8qKiBSZWZlcmVuY2UgdG8gdGhlIG9wdGlvbiB0aGF0IGhhcyBiZWVuIGNoYW5nZWQuICovXG4gICAgcHVibGljIG9wdGlvbjogTWF0TGlzdE9wdGlvbikge31cbn1cblxuLyoqXG4gKiBDb21wb25lbnQgZm9yIGxpc3Qtb3B0aW9ucyBvZiBzZWxlY3Rpb24tbGlzdC4gRWFjaCBsaXN0LW9wdGlvbiBjYW4gYXV0b21hdGljYWxseVxuICogZ2VuZXJhdGUgYSBjaGVja2JveCBhbmQgY2FuIHB1dCBjdXJyZW50IGl0ZW0gaW50byB0aGUgc2VsZWN0aW9uTW9kZWwgb2Ygc2VsZWN0aW9uLWxpc3RcbiAqIGlmIHRoZSBjdXJyZW50IGl0ZW0gaXMgc2VsZWN0ZWQuXG4gKi9cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ21hdC1saXN0LW9wdGlvbicsXG4gIGV4cG9ydEFzOiAnbWF0TGlzdE9wdGlvbicsXG4gIGlucHV0czogWydkaXNhYmxlUmlwcGxlJ10sXG4gIGhvc3Q6IHtcbiAgICAncm9sZSc6ICdvcHRpb24nLFxuICAgICdjbGFzcyc6ICdtYXQtbGlzdC1pdGVtIG1hdC1saXN0LW9wdGlvbiBtYXQtZm9jdXMtaW5kaWNhdG9yJyxcbiAgICAnKGZvY3VzKSc6ICdfaGFuZGxlRm9jdXMoKScsXG4gICAgJyhibHVyKSc6ICdfaGFuZGxlQmx1cigpJyxcbiAgICAnKGNsaWNrKSc6ICdfaGFuZGxlQ2xpY2soKScsXG4gICAgJ1tjbGFzcy5tYXQtbGlzdC1pdGVtLWRpc2FibGVkXSc6ICdkaXNhYmxlZCcsXG4gICAgJ1tjbGFzcy5tYXQtbGlzdC1pdGVtLXdpdGgtYXZhdGFyXSc6ICdfYXZhdGFyIHx8IF9pY29uJyxcbiAgICAvLyBNYW51YWxseSBzZXQgdGhlIFwicHJpbWFyeVwiIG9yIFwid2FyblwiIGNsYXNzIGlmIHRoZSBjb2xvciBoYXMgYmVlbiBleHBsaWNpdGx5XG4gICAgLy8gc2V0IHRvIFwicHJpbWFyeVwiIG9yIFwid2FyblwiLiBUaGUgcHNldWRvIGNoZWNrYm94IHBpY2tzIHVwIHRoZXNlIGNsYXNzZXMgZm9yXG4gICAgLy8gaXRzIHRoZW1lLlxuICAgICdbY2xhc3MubWF0LXByaW1hcnldJzogJ2NvbG9yID09PSBcInByaW1hcnlcIicsXG4gICAgLy8gRXZlbiB0aG91Z2ggYWNjZW50IGlzIHRoZSBkZWZhdWx0LCB3ZSBuZWVkIHRvIHNldCB0aGlzIGNsYXNzIGFueXdheSwgYmVjYXVzZSB0aGUgIGxpc3QgbWlnaHRcbiAgICAvLyBiZSBwbGFjZWQgaW5zaWRlIGEgcGFyZW50IHRoYXQgaGFzIG9uZSBvZiB0aGUgb3RoZXIgY29sb3JzIHdpdGggYSBoaWdoZXIgc3BlY2lmaWNpdHkuXG4gICAgJ1tjbGFzcy5tYXQtYWNjZW50XSc6ICdjb2xvciAhPT0gXCJwcmltYXJ5XCIgJiYgY29sb3IgIT09IFwid2FyblwiJyxcbiAgICAnW2NsYXNzLm1hdC13YXJuXSc6ICdjb2xvciA9PT0gXCJ3YXJuXCInLFxuICAgICdbY2xhc3MubWF0LWxpc3Qtc2luZ2xlLXNlbGVjdGVkLW9wdGlvbl0nOiAnc2VsZWN0ZWQgJiYgIXNlbGVjdGlvbkxpc3QubXVsdGlwbGUnLFxuICAgICdbYXR0ci5hcmlhLXNlbGVjdGVkXSc6ICdzZWxlY3RlZCcsXG4gICAgJ1thdHRyLmFyaWEtZGlzYWJsZWRdJzogJ2Rpc2FibGVkJyxcbiAgICAnW2F0dHIudGFiaW5kZXhdJzogJy0xJyxcbiAgfSxcbiAgdGVtcGxhdGVVcmw6ICdsaXN0LW9wdGlvbi5odG1sJyxcbiAgZW5jYXBzdWxhdGlvbjogVmlld0VuY2Fwc3VsYXRpb24uTm9uZSxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2gsXG59KVxuZXhwb3J0IGNsYXNzIE1hdExpc3RPcHRpb24gZXh0ZW5kcyBfTWF0TGlzdE9wdGlvbk1peGluQmFzZSBpbXBsZW1lbnRzIEFmdGVyQ29udGVudEluaXQsIE9uRGVzdHJveSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBPbkluaXQsIEZvY3VzYWJsZU9wdGlvbixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBDYW5EaXNhYmxlUmlwcGxlIHtcbiAgcHJpdmF0ZSBfc2VsZWN0ZWQgPSBmYWxzZTtcbiAgcHJpdmF0ZSBfZGlzYWJsZWQgPSBmYWxzZTtcbiAgcHJpdmF0ZSBfaGFzRm9jdXMgPSBmYWxzZTtcblxuICBAQ29udGVudENoaWxkKE1hdExpc3RBdmF0YXJDc3NNYXRTdHlsZXIpIF9hdmF0YXI6IE1hdExpc3RBdmF0YXJDc3NNYXRTdHlsZXI7XG4gIEBDb250ZW50Q2hpbGQoTWF0TGlzdEljb25Dc3NNYXRTdHlsZXIpIF9pY29uOiBNYXRMaXN0SWNvbkNzc01hdFN0eWxlcjtcbiAgQENvbnRlbnRDaGlsZHJlbihNYXRMaW5lLCB7ZGVzY2VuZGFudHM6IHRydWV9KSBfbGluZXM6IFF1ZXJ5TGlzdDxNYXRMaW5lPjtcblxuICAvKiogRE9NIGVsZW1lbnQgY29udGFpbmluZyB0aGUgaXRlbSdzIHRleHQuICovXG4gIEBWaWV3Q2hpbGQoJ3RleHQnKSBfdGV4dDogRWxlbWVudFJlZjtcblxuICAvKiogV2hldGhlciB0aGUgbGFiZWwgc2hvdWxkIGFwcGVhciBiZWZvcmUgb3IgYWZ0ZXIgdGhlIGNoZWNrYm94LiBEZWZhdWx0cyB0byAnYWZ0ZXInICovXG4gIEBJbnB1dCgpIGNoZWNrYm94UG9zaXRpb246ICdiZWZvcmUnIHwgJ2FmdGVyJyA9ICdhZnRlcic7XG5cbiAgLyoqIFRoZW1lIGNvbG9yIG9mIHRoZSBsaXN0IG9wdGlvbi4gVGhpcyBzZXRzIHRoZSBjb2xvciBvZiB0aGUgY2hlY2tib3guICovXG4gIEBJbnB1dCgpXG4gIGdldCBjb2xvcigpOiBUaGVtZVBhbGV0dGUgeyByZXR1cm4gdGhpcy5fY29sb3IgfHwgdGhpcy5zZWxlY3Rpb25MaXN0LmNvbG9yOyB9XG4gIHNldCBjb2xvcihuZXdWYWx1ZTogVGhlbWVQYWxldHRlKSB7IHRoaXMuX2NvbG9yID0gbmV3VmFsdWU7IH1cbiAgcHJpdmF0ZSBfY29sb3I6IFRoZW1lUGFsZXR0ZTtcblxuICAvKipcbiAgICogVGhpcyBpcyBzZXQgdG8gdHJ1ZSBhZnRlciB0aGUgZmlyc3QgT25DaGFuZ2VzIGN5Y2xlIHNvIHdlIGRvbid0IGNsZWFyIHRoZSB2YWx1ZSBvZiBgc2VsZWN0ZWRgXG4gICAqIGluIHRoZSBmaXJzdCBjeWNsZS5cbiAgICovXG4gIHByaXZhdGUgX2lucHV0c0luaXRpYWxpemVkID0gZmFsc2U7XG4gIC8qKiBWYWx1ZSBvZiB0aGUgb3B0aW9uICovXG4gIEBJbnB1dCgpXG4gIGdldCB2YWx1ZSgpOiBhbnkgeyByZXR1cm4gdGhpcy5fdmFsdWU7IH1cbiAgc2V0IHZhbHVlKG5ld1ZhbHVlOiBhbnkpIHtcbiAgICBpZiAodGhpcy5zZWxlY3RlZCAmJiBuZXdWYWx1ZSAhPT0gdGhpcy52YWx1ZSAmJiB0aGlzLl9pbnB1dHNJbml0aWFsaXplZCkge1xuICAgICAgdGhpcy5zZWxlY3RlZCA9IGZhbHNlO1xuICAgIH1cblxuICAgIHRoaXMuX3ZhbHVlID0gbmV3VmFsdWU7XG4gIH1cbiAgcHJpdmF0ZSBfdmFsdWU6IGFueTtcblxuICAvKiogV2hldGhlciB0aGUgb3B0aW9uIGlzIGRpc2FibGVkLiAqL1xuICBASW5wdXQoKVxuICBnZXQgZGlzYWJsZWQoKSB7IHJldHVybiB0aGlzLl9kaXNhYmxlZCB8fCAodGhpcy5zZWxlY3Rpb25MaXN0ICYmIHRoaXMuc2VsZWN0aW9uTGlzdC5kaXNhYmxlZCk7IH1cbiAgc2V0IGRpc2FibGVkKHZhbHVlOiBhbnkpIHtcbiAgICBjb25zdCBuZXdWYWx1ZSA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh2YWx1ZSk7XG5cbiAgICBpZiAobmV3VmFsdWUgIT09IHRoaXMuX2Rpc2FibGVkKSB7XG4gICAgICB0aGlzLl9kaXNhYmxlZCA9IG5ld1ZhbHVlO1xuICAgICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3IubWFya0ZvckNoZWNrKCk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIG9wdGlvbiBpcyBzZWxlY3RlZC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IHNlbGVjdGVkKCk6IGJvb2xlYW4geyByZXR1cm4gdGhpcy5zZWxlY3Rpb25MaXN0LnNlbGVjdGVkT3B0aW9ucy5pc1NlbGVjdGVkKHRoaXMpOyB9XG4gIHNldCBzZWxlY3RlZCh2YWx1ZTogYm9vbGVhbikge1xuICAgIGNvbnN0IGlzU2VsZWN0ZWQgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkodmFsdWUpO1xuXG4gICAgaWYgKGlzU2VsZWN0ZWQgIT09IHRoaXMuX3NlbGVjdGVkKSB7XG4gICAgICB0aGlzLl9zZXRTZWxlY3RlZChpc1NlbGVjdGVkKTtcbiAgICAgIHRoaXMuc2VsZWN0aW9uTGlzdC5fcmVwb3J0VmFsdWVDaGFuZ2UoKTtcbiAgICB9XG4gIH1cblxuICBjb25zdHJ1Y3Rvcihwcml2YXRlIF9lbGVtZW50OiBFbGVtZW50UmVmPEhUTUxFbGVtZW50PixcbiAgICAgICAgICAgICAgcHJpdmF0ZSBfY2hhbmdlRGV0ZWN0b3I6IENoYW5nZURldGVjdG9yUmVmLFxuICAgICAgICAgICAgICAvKiogQGRvY3MtcHJpdmF0ZSAqL1xuICAgICAgICAgICAgICBASW5qZWN0KGZvcndhcmRSZWYoKCkgPT4gTWF0U2VsZWN0aW9uTGlzdCkpIHB1YmxpYyBzZWxlY3Rpb25MaXN0OiBNYXRTZWxlY3Rpb25MaXN0KSB7XG4gICAgc3VwZXIoKTtcbiAgfVxuXG4gIG5nT25Jbml0KCkge1xuICAgIGNvbnN0IGxpc3QgPSB0aGlzLnNlbGVjdGlvbkxpc3Q7XG5cbiAgICBpZiAobGlzdC5fdmFsdWUgJiYgbGlzdC5fdmFsdWUuc29tZSh2YWx1ZSA9PiBsaXN0LmNvbXBhcmVXaXRoKHZhbHVlLCB0aGlzLl92YWx1ZSkpKSB7XG4gICAgICB0aGlzLl9zZXRTZWxlY3RlZCh0cnVlKTtcbiAgICB9XG5cbiAgICBjb25zdCB3YXNTZWxlY3RlZCA9IHRoaXMuX3NlbGVjdGVkO1xuXG4gICAgLy8gTGlzdCBvcHRpb25zIHRoYXQgYXJlIHNlbGVjdGVkIGF0IGluaXRpYWxpemF0aW9uIGNhbid0IGJlIHJlcG9ydGVkIHByb3Blcmx5IHRvIHRoZSBmb3JtXG4gICAgLy8gY29udHJvbC4gVGhpcyBpcyBiZWNhdXNlIGl0IHRha2VzIHNvbWUgdGltZSB1bnRpbCB0aGUgc2VsZWN0aW9uLWxpc3Qga25vd3MgYWJvdXQgYWxsXG4gICAgLy8gYXZhaWxhYmxlIG9wdGlvbnMuIEFsc28gaXQgY2FuIGhhcHBlbiB0aGF0IHRoZSBDb250cm9sVmFsdWVBY2Nlc3NvciBoYXMgYW4gaW5pdGlhbCB2YWx1ZVxuICAgIC8vIHRoYXQgc2hvdWxkIGJlIHVzZWQgaW5zdGVhZC4gRGVmZXJyaW5nIHRoZSB2YWx1ZSBjaGFuZ2UgcmVwb3J0IHRvIHRoZSBuZXh0IHRpY2sgZW5zdXJlc1xuICAgIC8vIHRoYXQgdGhlIGZvcm0gY29udHJvbCB2YWx1ZSBpcyBub3QgYmVpbmcgb3ZlcndyaXR0ZW4uXG4gICAgUHJvbWlzZS5yZXNvbHZlKCkudGhlbigoKSA9PiB7XG4gICAgICBpZiAodGhpcy5fc2VsZWN0ZWQgfHwgd2FzU2VsZWN0ZWQpIHtcbiAgICAgICAgdGhpcy5zZWxlY3RlZCA9IHRydWU7XG4gICAgICAgIHRoaXMuX2NoYW5nZURldGVjdG9yLm1hcmtGb3JDaGVjaygpO1xuICAgICAgfVxuICAgIH0pO1xuICAgIHRoaXMuX2lucHV0c0luaXRpYWxpemVkID0gdHJ1ZTtcbiAgfVxuXG4gIG5nQWZ0ZXJDb250ZW50SW5pdCgpIHtcbiAgICBzZXRMaW5lcyh0aGlzLl9saW5lcywgdGhpcy5fZWxlbWVudCk7XG4gIH1cblxuICBuZ09uRGVzdHJveSgpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5zZWxlY3RlZCkge1xuICAgICAgLy8gV2UgaGF2ZSB0byBkZWxheSB0aGlzIHVudGlsIHRoZSBuZXh0IHRpY2sgaW4gb3JkZXJcbiAgICAgIC8vIHRvIGF2b2lkIGNoYW5nZWQgYWZ0ZXIgY2hlY2tlZCBlcnJvcnMuXG4gICAgICBQcm9taXNlLnJlc29sdmUoKS50aGVuKCgpID0+IHtcbiAgICAgICAgdGhpcy5zZWxlY3RlZCA9IGZhbHNlO1xuICAgICAgfSk7XG4gICAgfVxuXG4gICAgY29uc3QgaGFkRm9jdXMgPSB0aGlzLl9oYXNGb2N1cztcbiAgICBjb25zdCBuZXdBY3RpdmVJdGVtID0gdGhpcy5zZWxlY3Rpb25MaXN0Ll9yZW1vdmVPcHRpb25Gcm9tTGlzdCh0aGlzKTtcblxuICAgIC8vIE9ubHkgbW92ZSBmb2N1cyBpZiB0aGlzIG9wdGlvbiB3YXMgZm9jdXNlZCBhdCB0aGUgdGltZSBpdCB3YXMgZGVzdHJveWVkLlxuICAgIGlmIChoYWRGb2N1cyAmJiBuZXdBY3RpdmVJdGVtKSB7XG4gICAgICBuZXdBY3RpdmVJdGVtLmZvY3VzKCk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFRvZ2dsZXMgdGhlIHNlbGVjdGlvbiBzdGF0ZSBvZiB0aGUgb3B0aW9uLiAqL1xuICB0b2dnbGUoKTogdm9pZCB7XG4gICAgdGhpcy5zZWxlY3RlZCA9ICF0aGlzLnNlbGVjdGVkO1xuICB9XG5cbiAgLyoqIEFsbG93cyBmb3IgcHJvZ3JhbW1hdGljIGZvY3VzaW5nIG9mIHRoZSBvcHRpb24uICovXG4gIGZvY3VzKCk6IHZvaWQge1xuICAgIHRoaXMuX2VsZW1lbnQubmF0aXZlRWxlbWVudC5mb2N1cygpO1xuICB9XG5cbiAgLyoqXG4gICAqIFJldHVybnMgdGhlIGxpc3QgaXRlbSdzIHRleHQgbGFiZWwuIEltcGxlbWVudGVkIGFzIGEgcGFydCBvZiB0aGUgRm9jdXNLZXlNYW5hZ2VyLlxuICAgKiBAZG9jcy1wcml2YXRlXG4gICAqL1xuICBnZXRMYWJlbCgpIHtcbiAgICByZXR1cm4gdGhpcy5fdGV4dCA/ICh0aGlzLl90ZXh0Lm5hdGl2ZUVsZW1lbnQudGV4dENvbnRlbnQgfHwgJycpIDogJyc7XG4gIH1cblxuICAvKiogV2hldGhlciB0aGlzIGxpc3QgaXRlbSBzaG91bGQgc2hvdyBhIHJpcHBsZSBlZmZlY3Qgd2hlbiBjbGlja2VkLiAqL1xuICBfaXNSaXBwbGVEaXNhYmxlZCgpIHtcbiAgICByZXR1cm4gdGhpcy5kaXNhYmxlZCB8fCB0aGlzLmRpc2FibGVSaXBwbGUgfHwgdGhpcy5zZWxlY3Rpb25MaXN0LmRpc2FibGVSaXBwbGU7XG4gIH1cblxuICBfaGFuZGxlQ2xpY2soKSB7XG4gICAgaWYgKCF0aGlzLmRpc2FibGVkICYmICh0aGlzLnNlbGVjdGlvbkxpc3QubXVsdGlwbGUgfHwgIXRoaXMuc2VsZWN0ZWQpKSB7XG4gICAgICB0aGlzLnRvZ2dsZSgpO1xuXG4gICAgICAvLyBFbWl0IGEgY2hhbmdlIGV2ZW50IGlmIHRoZSBzZWxlY3RlZCBzdGF0ZSBvZiB0aGUgb3B0aW9uIGNoYW5nZWQgdGhyb3VnaCB1c2VyIGludGVyYWN0aW9uLlxuICAgICAgdGhpcy5zZWxlY3Rpb25MaXN0Ll9lbWl0Q2hhbmdlRXZlbnQodGhpcyk7XG4gICAgfVxuICB9XG5cbiAgX2hhbmRsZUZvY3VzKCkge1xuICAgIHRoaXMuc2VsZWN0aW9uTGlzdC5fc2V0Rm9jdXNlZE9wdGlvbih0aGlzKTtcbiAgICB0aGlzLl9oYXNGb2N1cyA9IHRydWU7XG4gIH1cblxuICBfaGFuZGxlQmx1cigpIHtcbiAgICB0aGlzLnNlbGVjdGlvbkxpc3QuX29uVG91Y2hlZCgpO1xuICAgIHRoaXMuX2hhc0ZvY3VzID0gZmFsc2U7XG4gIH1cblxuICAvKiogUmV0cmlldmVzIHRoZSBET00gZWxlbWVudCBvZiB0aGUgY29tcG9uZW50IGhvc3QuICovXG4gIF9nZXRIb3N0RWxlbWVudCgpOiBIVE1MRWxlbWVudCB7XG4gICAgcmV0dXJuIHRoaXMuX2VsZW1lbnQubmF0aXZlRWxlbWVudDtcbiAgfVxuXG4gIC8qKiBTZXRzIHRoZSBzZWxlY3RlZCBzdGF0ZSBvZiB0aGUgb3B0aW9uLiBSZXR1cm5zIHdoZXRoZXIgdGhlIHZhbHVlIGhhcyBjaGFuZ2VkLiAqL1xuICBfc2V0U2VsZWN0ZWQoc2VsZWN0ZWQ6IGJvb2xlYW4pOiBib29sZWFuIHtcbiAgICBpZiAoc2VsZWN0ZWQgPT09IHRoaXMuX3NlbGVjdGVkKSB7XG4gICAgICByZXR1cm4gZmFsc2U7XG4gICAgfVxuXG4gICAgdGhpcy5fc2VsZWN0ZWQgPSBzZWxlY3RlZDtcblxuICAgIGlmIChzZWxlY3RlZCkge1xuICAgICAgdGhpcy5zZWxlY3Rpb25MaXN0LnNlbGVjdGVkT3B0aW9ucy5zZWxlY3QodGhpcyk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHRoaXMuc2VsZWN0aW9uTGlzdC5zZWxlY3RlZE9wdGlvbnMuZGVzZWxlY3QodGhpcyk7XG4gICAgfVxuXG4gICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3IubWFya0ZvckNoZWNrKCk7XG4gICAgcmV0dXJuIHRydWU7XG4gIH1cblxuICAvKipcbiAgICogTm90aWZpZXMgQW5ndWxhciB0aGF0IHRoZSBvcHRpb24gbmVlZHMgdG8gYmUgY2hlY2tlZCBpbiB0aGUgbmV4dCBjaGFuZ2UgZGV0ZWN0aW9uIHJ1bi4gTWFpbmx5XG4gICAqIHVzZWQgdG8gdHJpZ2dlciBhbiB1cGRhdGUgb2YgdGhlIGxpc3Qgb3B0aW9uIGlmIHRoZSBkaXNhYmxlZCBzdGF0ZSBvZiB0aGUgc2VsZWN0aW9uIGxpc3RcbiAgICogY2hhbmdlZC5cbiAgICovXG4gIF9tYXJrRm9yQ2hlY2soKSB7XG4gICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3IubWFya0ZvckNoZWNrKCk7XG4gIH1cblxuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfZGlzYWJsZWQ6IEJvb2xlYW5JbnB1dDtcbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX3NlbGVjdGVkOiBCb29sZWFuSW5wdXQ7XG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV9kaXNhYmxlUmlwcGxlOiBCb29sZWFuSW5wdXQ7XG59XG5cblxuLyoqXG4gKiBNYXRlcmlhbCBEZXNpZ24gbGlzdCBjb21wb25lbnQgd2hlcmUgZWFjaCBpdGVtIGlzIGEgc2VsZWN0YWJsZSBvcHRpb24uIEJlaGF2ZXMgYXMgYSBsaXN0Ym94LlxuICovXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdtYXQtc2VsZWN0aW9uLWxpc3QnLFxuICBleHBvcnRBczogJ21hdFNlbGVjdGlvbkxpc3QnLFxuICBpbnB1dHM6IFsnZGlzYWJsZVJpcHBsZSddLFxuICBob3N0OiB7XG4gICAgJ3JvbGUnOiAnbGlzdGJveCcsXG4gICAgJ2NsYXNzJzogJ21hdC1zZWxlY3Rpb24tbGlzdCBtYXQtbGlzdC1iYXNlJyxcbiAgICAnKGZvY3VzKSc6ICdfb25Gb2N1cygpJyxcbiAgICAnKGtleWRvd24pJzogJ19rZXlkb3duKCRldmVudCknLFxuICAgICdbYXR0ci5hcmlhLW11bHRpc2VsZWN0YWJsZV0nOiAnbXVsdGlwbGUnLFxuICAgICdbYXR0ci5hcmlhLWRpc2FibGVkXSc6ICdkaXNhYmxlZC50b1N0cmluZygpJyxcbiAgICAnW2F0dHIudGFiaW5kZXhdJzogJ190YWJJbmRleCcsXG4gIH0sXG4gIHRlbXBsYXRlOiAnPG5nLWNvbnRlbnQ+PC9uZy1jb250ZW50PicsXG4gIHN0eWxlVXJsczogWydsaXN0LmNzcyddLFxuICBlbmNhcHN1bGF0aW9uOiBWaWV3RW5jYXBzdWxhdGlvbi5Ob25lLFxuICBwcm92aWRlcnM6IFtNQVRfU0VMRUNUSU9OX0xJU1RfVkFMVUVfQUNDRVNTT1JdLFxuICBjaGFuZ2VEZXRlY3Rpb246IENoYW5nZURldGVjdGlvblN0cmF0ZWd5Lk9uUHVzaFxufSlcbmV4cG9ydCBjbGFzcyBNYXRTZWxlY3Rpb25MaXN0IGV4dGVuZHMgX01hdFNlbGVjdGlvbkxpc3RNaXhpbkJhc2UgaW1wbGVtZW50cyBDYW5EaXNhYmxlUmlwcGxlLFxuICBBZnRlckNvbnRlbnRJbml0LCBDb250cm9sVmFsdWVBY2Nlc3NvciwgT25EZXN0cm95LCBPbkNoYW5nZXMge1xuICBwcml2YXRlIF9tdWx0aXBsZSA9IHRydWU7XG4gIHByaXZhdGUgX2NvbnRlbnRJbml0aWFsaXplZCA9IGZhbHNlO1xuXG4gIC8qKiBUaGUgRm9jdXNLZXlNYW5hZ2VyIHdoaWNoIGhhbmRsZXMgZm9jdXMuICovXG4gIF9rZXlNYW5hZ2VyOiBGb2N1c0tleU1hbmFnZXI8TWF0TGlzdE9wdGlvbj47XG5cbiAgLyoqIFRoZSBvcHRpb24gY29tcG9uZW50cyBjb250YWluZWQgd2l0aGluIHRoaXMgc2VsZWN0aW9uLWxpc3QuICovXG4gIEBDb250ZW50Q2hpbGRyZW4oTWF0TGlzdE9wdGlvbiwge2Rlc2NlbmRhbnRzOiB0cnVlfSkgb3B0aW9uczogUXVlcnlMaXN0PE1hdExpc3RPcHRpb24+O1xuXG4gIC8qKiBFbWl0cyBhIGNoYW5nZSBldmVudCB3aGVuZXZlciB0aGUgc2VsZWN0ZWQgc3RhdGUgb2YgYW4gb3B0aW9uIGNoYW5nZXMuICovXG4gIEBPdXRwdXQoKSByZWFkb25seSBzZWxlY3Rpb25DaGFuZ2U6IEV2ZW50RW1pdHRlcjxNYXRTZWxlY3Rpb25MaXN0Q2hhbmdlPiA9XG4gICAgICBuZXcgRXZlbnRFbWl0dGVyPE1hdFNlbGVjdGlvbkxpc3RDaGFuZ2U+KCk7XG5cbiAgLyoqXG4gICAqIFRhYmluZGV4IG9mIHRoZSBzZWxlY3Rpb24gbGlzdC5cbiAgICogQGJyZWFraW5nLWNoYW5nZSAxMS4wLjAgUmVtb3ZlIGB0YWJJbmRleGAgaW5wdXQuXG4gICAqL1xuICBASW5wdXQoKSB0YWJJbmRleDogbnVtYmVyID0gMDtcblxuICAvKiogVGhlbWUgY29sb3Igb2YgdGhlIHNlbGVjdGlvbiBsaXN0LiBUaGlzIHNldHMgdGhlIGNoZWNrYm94IGNvbG9yIGZvciBhbGwgbGlzdCBvcHRpb25zLiAqL1xuICBASW5wdXQoKSBjb2xvcjogVGhlbWVQYWxldHRlID0gJ2FjY2VudCc7XG5cbiAgLyoqXG4gICAqIEZ1bmN0aW9uIHVzZWQgZm9yIGNvbXBhcmluZyBhbiBvcHRpb24gYWdhaW5zdCB0aGUgc2VsZWN0ZWQgdmFsdWUgd2hlbiBkZXRlcm1pbmluZyB3aGljaFxuICAgKiBvcHRpb25zIHNob3VsZCBhcHBlYXIgYXMgc2VsZWN0ZWQuIFRoZSBmaXJzdCBhcmd1bWVudCBpcyB0aGUgdmFsdWUgb2YgYW4gb3B0aW9ucy4gVGhlIHNlY29uZFxuICAgKiBvbmUgaXMgYSB2YWx1ZSBmcm9tIHRoZSBzZWxlY3RlZCB2YWx1ZS4gQSBib29sZWFuIG11c3QgYmUgcmV0dXJuZWQuXG4gICAqL1xuICBASW5wdXQoKSBjb21wYXJlV2l0aDogKG8xOiBhbnksIG8yOiBhbnkpID0+IGJvb2xlYW4gPSAoYTEsIGEyKSA9PiBhMSA9PT0gYTI7XG5cbiAgLyoqIFdoZXRoZXIgdGhlIHNlbGVjdGlvbiBsaXN0IGlzIGRpc2FibGVkLiAqL1xuICBASW5wdXQoKVxuICBnZXQgZGlzYWJsZWQoKTogYm9vbGVhbiB7IHJldHVybiB0aGlzLl9kaXNhYmxlZDsgfVxuICBzZXQgZGlzYWJsZWQodmFsdWU6IGJvb2xlYW4pIHtcbiAgICB0aGlzLl9kaXNhYmxlZCA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh2YWx1ZSk7XG5cbiAgICAvLyBUaGUgYE1hdFNlbGVjdGlvbkxpc3RgIGFuZCBgTWF0TGlzdE9wdGlvbmAgYXJlIHVzaW5nIHRoZSBgT25QdXNoYCBjaGFuZ2UgZGV0ZWN0aW9uXG4gICAgLy8gc3RyYXRlZ3kuIFRoZXJlZm9yZSB0aGUgb3B0aW9ucyB3aWxsIG5vdCBjaGVjayBmb3IgYW55IGNoYW5nZXMgaWYgdGhlIGBNYXRTZWxlY3Rpb25MaXN0YFxuICAgIC8vIGNoYW5nZWQgaXRzIHN0YXRlLiBTaW5jZSB3ZSBrbm93IHRoYXQgYSBjaGFuZ2UgdG8gYGRpc2FibGVkYCBwcm9wZXJ0eSBvZiB0aGUgbGlzdCBhZmZlY3RzXG4gICAgLy8gdGhlIHN0YXRlIG9mIHRoZSBvcHRpb25zLCB3ZSBtYW51YWxseSBtYXJrIGVhY2ggb3B0aW9uIGZvciBjaGVjay5cbiAgICB0aGlzLl9tYXJrT3B0aW9uc0ZvckNoZWNrKCk7XG4gIH1cbiAgcHJpdmF0ZSBfZGlzYWJsZWQ6IGJvb2xlYW4gPSBmYWxzZTtcblxuICAvKiogV2hldGhlciBzZWxlY3Rpb24gaXMgbGltaXRlZCB0byBvbmUgb3IgbXVsdGlwbGUgaXRlbXMgKGRlZmF1bHQgbXVsdGlwbGUpLiAqL1xuICBASW5wdXQoKVxuICBnZXQgbXVsdGlwbGUoKTogYm9vbGVhbiB7IHJldHVybiB0aGlzLl9tdWx0aXBsZTsgfVxuICBzZXQgbXVsdGlwbGUodmFsdWU6IGJvb2xlYW4pIHtcbiAgICBjb25zdCBuZXdWYWx1ZSA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh2YWx1ZSk7XG5cbiAgICBpZiAobmV3VmFsdWUgIT09IHRoaXMuX211bHRpcGxlKSB7XG4gICAgICBpZiAoaXNEZXZNb2RlKCkgJiYgdGhpcy5fY29udGVudEluaXRpYWxpemVkKSB7XG4gICAgICAgIHRocm93IG5ldyBFcnJvcihcbiAgICAgICAgICAgICdDYW5ub3QgY2hhbmdlIGBtdWx0aXBsZWAgbW9kZSBvZiBtYXQtc2VsZWN0aW9uLWxpc3QgYWZ0ZXIgaW5pdGlhbGl6YXRpb24uJyk7XG4gICAgICB9XG5cbiAgICAgIHRoaXMuX211bHRpcGxlID0gbmV3VmFsdWU7XG4gICAgICB0aGlzLnNlbGVjdGVkT3B0aW9ucyA9IG5ldyBTZWxlY3Rpb25Nb2RlbCh0aGlzLl9tdWx0aXBsZSwgdGhpcy5zZWxlY3RlZE9wdGlvbnMuc2VsZWN0ZWQpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBUaGUgY3VycmVudGx5IHNlbGVjdGVkIG9wdGlvbnMuICovXG4gIHNlbGVjdGVkT3B0aW9ucyA9IG5ldyBTZWxlY3Rpb25Nb2RlbDxNYXRMaXN0T3B0aW9uPih0aGlzLl9tdWx0aXBsZSk7XG5cbiAgLyoqIFRoZSB0YWJpbmRleCBvZiB0aGUgc2VsZWN0aW9uIGxpc3QuICovXG4gIF90YWJJbmRleCA9IC0xO1xuXG4gIC8qKiBWaWV3IHRvIG1vZGVsIGNhbGxiYWNrIHRoYXQgc2hvdWxkIGJlIGNhbGxlZCB3aGVuZXZlciB0aGUgc2VsZWN0ZWQgb3B0aW9ucyBjaGFuZ2UuICovXG4gIHByaXZhdGUgX29uQ2hhbmdlOiAodmFsdWU6IGFueSkgPT4gdm9pZCA9IChfOiBhbnkpID0+IHt9O1xuXG4gIC8qKiBLZWVwcyB0cmFjayBvZiB0aGUgY3VycmVudGx5LXNlbGVjdGVkIHZhbHVlLiAqL1xuICBfdmFsdWU6IHN0cmluZ1tdfG51bGw7XG5cbiAgLyoqIEVtaXRzIHdoZW4gdGhlIGxpc3QgaGFzIGJlZW4gZGVzdHJveWVkLiAqL1xuICBwcml2YXRlIF9kZXN0cm95ZWQgPSBuZXcgU3ViamVjdDx2b2lkPigpO1xuXG4gIC8qKiBWaWV3IHRvIG1vZGVsIGNhbGxiYWNrIHRoYXQgc2hvdWxkIGJlIGNhbGxlZCBpZiB0aGUgbGlzdCBvciBpdHMgb3B0aW9ucyBsb3N0IGZvY3VzLiAqL1xuICBfb25Ub3VjaGVkOiAoKSA9PiB2b2lkID0gKCkgPT4ge307XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGxpc3QgaGFzIGJlZW4gZGVzdHJveWVkLiAqL1xuICBwcml2YXRlIF9pc0Rlc3Ryb3llZDogYm9vbGVhbjtcblxuICBjb25zdHJ1Y3Rvcihwcml2YXRlIF9lbGVtZW50OiBFbGVtZW50UmVmPEhUTUxFbGVtZW50PixcbiAgICAvLyBAYnJlYWtpbmctY2hhbmdlIDExLjAuMCBSZW1vdmUgYHRhYkluZGV4YCBwYXJhbWV0ZXIuXG4gICAgQEF0dHJpYnV0ZSgndGFiaW5kZXgnKSB0YWJJbmRleDogc3RyaW5nLFxuICAgIHByaXZhdGUgX2NoYW5nZURldGVjdG9yOiBDaGFuZ2VEZXRlY3RvclJlZikge1xuICAgIHN1cGVyKCk7XG4gIH1cblxuICBuZ0FmdGVyQ29udGVudEluaXQoKTogdm9pZCB7XG4gICAgdGhpcy5fY29udGVudEluaXRpYWxpemVkID0gdHJ1ZTtcblxuICAgIHRoaXMuX2tleU1hbmFnZXIgPSBuZXcgRm9jdXNLZXlNYW5hZ2VyPE1hdExpc3RPcHRpb24+KHRoaXMub3B0aW9ucylcbiAgICAgIC53aXRoV3JhcCgpXG4gICAgICAud2l0aFR5cGVBaGVhZCgpXG4gICAgICAvLyBBbGxvdyBkaXNhYmxlZCBpdGVtcyB0byBiZSBmb2N1c2FibGUuIEZvciBhY2Nlc3NpYmlsaXR5IHJlYXNvbnMsIHRoZXJlIG11c3QgYmUgYSB3YXkgZm9yXG4gICAgICAvLyBzY3JlZW5yZWFkZXIgdXNlcnMsIHRoYXQgYWxsb3dzIHJlYWRpbmcgdGhlIGRpZmZlcmVudCBvcHRpb25zIG9mIHRoZSBsaXN0LlxuICAgICAgLnNraXBQcmVkaWNhdGUoKCkgPT4gZmFsc2UpXG4gICAgICAud2l0aEFsbG93ZWRNb2RpZmllcktleXMoWydzaGlmdEtleSddKTtcblxuICAgIGlmICh0aGlzLl92YWx1ZSkge1xuICAgICAgdGhpcy5fc2V0T3B0aW9uc0Zyb21WYWx1ZXModGhpcy5fdmFsdWUpO1xuICAgIH1cblxuICAgIC8vIElmIHRoZSB1c2VyIGF0dGVtcHRzIHRvIHRhYiBvdXQgb2YgdGhlIHNlbGVjdGlvbiBsaXN0LCBhbGxvdyBmb2N1cyB0byBlc2NhcGUuXG4gICAgdGhpcy5fa2V5TWFuYWdlci50YWJPdXQucGlwZSh0YWtlVW50aWwodGhpcy5fZGVzdHJveWVkKSkuc3Vic2NyaWJlKCgpID0+IHtcbiAgICAgIHRoaXMuX2FsbG93Rm9jdXNFc2NhcGUoKTtcbiAgICB9KTtcblxuICAgIC8vIFdoZW4gdGhlIG51bWJlciBvZiBvcHRpb25zIGNoYW5nZSwgdXBkYXRlIHRoZSB0YWJpbmRleCBvZiB0aGUgc2VsZWN0aW9uIGxpc3QuXG4gICAgdGhpcy5vcHRpb25zLmNoYW5nZXMucGlwZShzdGFydFdpdGgobnVsbCksIHRha2VVbnRpbCh0aGlzLl9kZXN0cm95ZWQpKS5zdWJzY3JpYmUoKCkgPT4ge1xuICAgICAgdGhpcy5fdXBkYXRlVGFiSW5kZXgoKTtcbiAgICB9KTtcblxuICAgIC8vIFN5bmMgZXh0ZXJuYWwgY2hhbmdlcyB0byB0aGUgbW9kZWwgYmFjayB0byB0aGUgb3B0aW9ucy5cbiAgICB0aGlzLnNlbGVjdGVkT3B0aW9ucy5jaGFuZ2VkLnBpcGUodGFrZVVudGlsKHRoaXMuX2Rlc3Ryb3llZCkpLnN1YnNjcmliZShldmVudCA9PiB7XG4gICAgICBpZiAoZXZlbnQuYWRkZWQpIHtcbiAgICAgICAgZm9yIChsZXQgaXRlbSBvZiBldmVudC5hZGRlZCkge1xuICAgICAgICAgIGl0ZW0uc2VsZWN0ZWQgPSB0cnVlO1xuICAgICAgICB9XG4gICAgICB9XG5cbiAgICAgIGlmIChldmVudC5yZW1vdmVkKSB7XG4gICAgICAgIGZvciAobGV0IGl0ZW0gb2YgZXZlbnQucmVtb3ZlZCkge1xuICAgICAgICAgIGl0ZW0uc2VsZWN0ZWQgPSBmYWxzZTtcbiAgICAgICAgfVxuICAgICAgfVxuICAgIH0pO1xuICB9XG5cbiAgbmdPbkNoYW5nZXMoY2hhbmdlczogU2ltcGxlQ2hhbmdlcykge1xuICAgIGNvbnN0IGRpc2FibGVSaXBwbGVDaGFuZ2VzID0gY2hhbmdlc1snZGlzYWJsZVJpcHBsZSddO1xuICAgIGNvbnN0IGNvbG9yQ2hhbmdlcyA9IGNoYW5nZXNbJ2NvbG9yJ107XG5cbiAgICBpZiAoKGRpc2FibGVSaXBwbGVDaGFuZ2VzICYmICFkaXNhYmxlUmlwcGxlQ2hhbmdlcy5maXJzdENoYW5nZSkgfHxcbiAgICAgICAgKGNvbG9yQ2hhbmdlcyAmJiAhY29sb3JDaGFuZ2VzLmZpcnN0Q2hhbmdlKSkge1xuICAgICAgdGhpcy5fbWFya09wdGlvbnNGb3JDaGVjaygpO1xuICAgIH1cbiAgfVxuXG4gIG5nT25EZXN0cm95KCkge1xuICAgIHRoaXMuX2Rlc3Ryb3llZC5uZXh0KCk7XG4gICAgdGhpcy5fZGVzdHJveWVkLmNvbXBsZXRlKCk7XG4gICAgdGhpcy5faXNEZXN0cm95ZWQgPSB0cnVlO1xuICB9XG5cbiAgLyoqIEZvY3VzZXMgdGhlIHNlbGVjdGlvbiBsaXN0LiAqL1xuICBmb2N1cyhvcHRpb25zPzogRm9jdXNPcHRpb25zKSB7XG4gICAgdGhpcy5fZWxlbWVudC5uYXRpdmVFbGVtZW50LmZvY3VzKG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqIFNlbGVjdHMgYWxsIG9mIHRoZSBvcHRpb25zLiAqL1xuICBzZWxlY3RBbGwoKSB7XG4gICAgdGhpcy5fc2V0QWxsT3B0aW9uc1NlbGVjdGVkKHRydWUpO1xuICB9XG5cbiAgLyoqIERlc2VsZWN0cyBhbGwgb2YgdGhlIG9wdGlvbnMuICovXG4gIGRlc2VsZWN0QWxsKCkge1xuICAgIHRoaXMuX3NldEFsbE9wdGlvbnNTZWxlY3RlZChmYWxzZSk7XG4gIH1cblxuICAvKiogU2V0cyB0aGUgZm9jdXNlZCBvcHRpb24gb2YgdGhlIHNlbGVjdGlvbi1saXN0LiAqL1xuICBfc2V0Rm9jdXNlZE9wdGlvbihvcHRpb246IE1hdExpc3RPcHRpb24pIHtcbiAgICB0aGlzLl9rZXlNYW5hZ2VyLnVwZGF0ZUFjdGl2ZUl0ZW0ob3B0aW9uKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZW1vdmVzIGFuIG9wdGlvbiBmcm9tIHRoZSBzZWxlY3Rpb24gbGlzdCBhbmQgdXBkYXRlcyB0aGUgYWN0aXZlIGl0ZW0uXG4gICAqIEByZXR1cm5zIEN1cnJlbnRseS1hY3RpdmUgaXRlbS5cbiAgICovXG4gIF9yZW1vdmVPcHRpb25Gcm9tTGlzdChvcHRpb246IE1hdExpc3RPcHRpb24pOiBNYXRMaXN0T3B0aW9uIHwgbnVsbCB7XG4gICAgY29uc3Qgb3B0aW9uSW5kZXggPSB0aGlzLl9nZXRPcHRpb25JbmRleChvcHRpb24pO1xuXG4gICAgaWYgKG9wdGlvbkluZGV4ID4gLTEgJiYgdGhpcy5fa2V5TWFuYWdlci5hY3RpdmVJdGVtSW5kZXggPT09IG9wdGlvbkluZGV4KSB7XG4gICAgICAvLyBDaGVjayB3aGV0aGVyIHRoZSBvcHRpb24gaXMgdGhlIGxhc3QgaXRlbVxuICAgICAgaWYgKG9wdGlvbkluZGV4ID4gMCkge1xuICAgICAgICB0aGlzLl9rZXlNYW5hZ2VyLnVwZGF0ZUFjdGl2ZUl0ZW0ob3B0aW9uSW5kZXggLSAxKTtcbiAgICAgIH0gZWxzZSBpZiAob3B0aW9uSW5kZXggPT09IDAgJiYgdGhpcy5vcHRpb25zLmxlbmd0aCA+IDEpIHtcbiAgICAgICAgdGhpcy5fa2V5TWFuYWdlci51cGRhdGVBY3RpdmVJdGVtKE1hdGgubWluKG9wdGlvbkluZGV4ICsgMSwgdGhpcy5vcHRpb25zLmxlbmd0aCAtIDEpKTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gdGhpcy5fa2V5TWFuYWdlci5hY3RpdmVJdGVtO1xuICB9XG5cbiAgLyoqIFBhc3NlcyByZWxldmFudCBrZXkgcHJlc3NlcyB0byBvdXIga2V5IG1hbmFnZXIuICovXG4gIF9rZXlkb3duKGV2ZW50OiBLZXlib2FyZEV2ZW50KSB7XG4gICAgY29uc3Qga2V5Q29kZSA9IGV2ZW50LmtleUNvZGU7XG4gICAgY29uc3QgbWFuYWdlciA9IHRoaXMuX2tleU1hbmFnZXI7XG4gICAgY29uc3QgcHJldmlvdXNGb2N1c0luZGV4ID0gbWFuYWdlci5hY3RpdmVJdGVtSW5kZXg7XG4gICAgY29uc3QgaGFzTW9kaWZpZXIgPSBoYXNNb2RpZmllcktleShldmVudCk7XG5cbiAgICBzd2l0Y2ggKGtleUNvZGUpIHtcbiAgICAgIGNhc2UgU1BBQ0U6XG4gICAgICBjYXNlIEVOVEVSOlxuICAgICAgICBpZiAoIWhhc01vZGlmaWVyICYmICFtYW5hZ2VyLmlzVHlwaW5nKCkpIHtcbiAgICAgICAgICB0aGlzLl90b2dnbGVGb2N1c2VkT3B0aW9uKCk7XG4gICAgICAgICAgLy8gQWx3YXlzIHByZXZlbnQgc3BhY2UgZnJvbSBzY3JvbGxpbmcgdGhlIHBhZ2Ugc2luY2UgdGhlIGxpc3QgaGFzIGZvY3VzXG4gICAgICAgICAgZXZlbnQucHJldmVudERlZmF1bHQoKTtcbiAgICAgICAgfVxuICAgICAgICBicmVhaztcbiAgICAgIGNhc2UgSE9NRTpcbiAgICAgIGNhc2UgRU5EOlxuICAgICAgICBpZiAoIWhhc01vZGlmaWVyKSB7XG4gICAgICAgICAga2V5Q29kZSA9PT0gSE9NRSA/IG1hbmFnZXIuc2V0Rmlyc3RJdGVtQWN0aXZlKCkgOiBtYW5hZ2VyLnNldExhc3RJdGVtQWN0aXZlKCk7XG4gICAgICAgICAgZXZlbnQucHJldmVudERlZmF1bHQoKTtcbiAgICAgICAgfVxuICAgICAgICBicmVhaztcbiAgICAgIGRlZmF1bHQ6XG4gICAgICAgIC8vIFRoZSBcIkFcIiBrZXkgZ2V0cyBzcGVjaWFsIHRyZWF0bWVudCwgYmVjYXVzZSBpdCdzIHVzZWQgZm9yIHRoZSBcInNlbGVjdCBhbGxcIiBmdW5jdGlvbmFsaXR5LlxuICAgICAgICBpZiAoa2V5Q29kZSA9PT0gQSAmJiB0aGlzLm11bHRpcGxlICYmIGhhc01vZGlmaWVyS2V5KGV2ZW50LCAnY3RybEtleScpICYmXG4gICAgICAgICAgICAhbWFuYWdlci5pc1R5cGluZygpKSB7XG4gICAgICAgICAgY29uc3Qgc2hvdWxkU2VsZWN0ID0gdGhpcy5vcHRpb25zLnNvbWUob3B0aW9uID0+ICFvcHRpb24uZGlzYWJsZWQgJiYgIW9wdGlvbi5zZWxlY3RlZCk7XG4gICAgICAgICAgdGhpcy5fc2V0QWxsT3B0aW9uc1NlbGVjdGVkKHNob3VsZFNlbGVjdCwgdHJ1ZSk7XG4gICAgICAgICAgZXZlbnQucHJldmVudERlZmF1bHQoKTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICBtYW5hZ2VyLm9uS2V5ZG93bihldmVudCk7XG4gICAgICAgIH1cbiAgICB9XG5cbiAgICBpZiAodGhpcy5tdWx0aXBsZSAmJiAoa2V5Q29kZSA9PT0gVVBfQVJST1cgfHwga2V5Q29kZSA9PT0gRE9XTl9BUlJPVykgJiYgZXZlbnQuc2hpZnRLZXkgJiZcbiAgICAgICAgbWFuYWdlci5hY3RpdmVJdGVtSW5kZXggIT09IHByZXZpb3VzRm9jdXNJbmRleCkge1xuICAgICAgdGhpcy5fdG9nZ2xlRm9jdXNlZE9wdGlvbigpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBSZXBvcnRzIGEgdmFsdWUgY2hhbmdlIHRvIHRoZSBDb250cm9sVmFsdWVBY2Nlc3NvciAqL1xuICBfcmVwb3J0VmFsdWVDaGFuZ2UoKSB7XG4gICAgLy8gU3RvcCByZXBvcnRpbmcgdmFsdWUgY2hhbmdlcyBhZnRlciB0aGUgbGlzdCBoYXMgYmVlbiBkZXN0cm95ZWQuIFRoaXMgYXZvaWRzXG4gICAgLy8gY2FzZXMgd2hlcmUgdGhlIGxpc3QgbWlnaHQgd3JvbmdseSByZXNldCBpdHMgdmFsdWUgb25jZSBpdCBpcyByZW1vdmVkLCBidXRcbiAgICAvLyB0aGUgZm9ybSBjb250cm9sIGlzIHN0aWxsIGxpdmUuXG4gICAgaWYgKHRoaXMub3B0aW9ucyAmJiAhdGhpcy5faXNEZXN0cm95ZWQpIHtcbiAgICAgIGNvbnN0IHZhbHVlID0gdGhpcy5fZ2V0U2VsZWN0ZWRPcHRpb25WYWx1ZXMoKTtcbiAgICAgIHRoaXMuX29uQ2hhbmdlKHZhbHVlKTtcbiAgICAgIHRoaXMuX3ZhbHVlID0gdmFsdWU7XG4gICAgfVxuICB9XG5cbiAgLyoqIEVtaXRzIGEgY2hhbmdlIGV2ZW50IGlmIHRoZSBzZWxlY3RlZCBzdGF0ZSBvZiBhbiBvcHRpb24gY2hhbmdlZC4gKi9cbiAgX2VtaXRDaGFuZ2VFdmVudChvcHRpb246IE1hdExpc3RPcHRpb24pIHtcbiAgICB0aGlzLnNlbGVjdGlvbkNoYW5nZS5lbWl0KG5ldyBNYXRTZWxlY3Rpb25MaXN0Q2hhbmdlKHRoaXMsIG9wdGlvbikpO1xuICB9XG5cbiAgLyoqXG4gICAqIFdoZW4gdGhlIHNlbGVjdGlvbiBsaXN0IGlzIGZvY3VzZWQsIHdlIHdhbnQgdG8gbW92ZSBmb2N1cyB0byBhbiBvcHRpb24gd2l0aGluIHRoZSBsaXN0LiBEbyB0aGlzXG4gICAqIGJ5IHNldHRpbmcgdGhlIGFwcHJvcHJpYXRlIG9wdGlvbiB0byBiZSBhY3RpdmUuXG4gICAqL1xuICBfb25Gb2N1cygpOiB2b2lkIHtcbiAgICBjb25zdCBhY3RpdmVJbmRleCA9IHRoaXMuX2tleU1hbmFnZXIuYWN0aXZlSXRlbUluZGV4O1xuXG4gICAgaWYgKCFhY3RpdmVJbmRleCB8fCAoYWN0aXZlSW5kZXggPT09IC0xKSkge1xuICAgICAgLy8gSWYgdGhlcmUgaXMgbm8gYWN0aXZlIGluZGV4LCBzZXQgZm9jdXMgdG8gdGhlIGZpcnN0IG9wdGlvbi5cbiAgICAgIHRoaXMuX2tleU1hbmFnZXIuc2V0Rmlyc3RJdGVtQWN0aXZlKCk7XG4gICAgfSBlbHNlIHtcbiAgICAgIC8vIE90aGVyd2lzZSwgc2V0IGZvY3VzIHRvIHRoZSBhY3RpdmUgb3B0aW9uLlxuICAgICAgdGhpcy5fa2V5TWFuYWdlci5zZXRBY3RpdmVJdGVtKGFjdGl2ZUluZGV4KTtcbiAgICB9XG4gIH1cblxuICAvKiogSW1wbGVtZW50ZWQgYXMgcGFydCBvZiBDb250cm9sVmFsdWVBY2Nlc3Nvci4gKi9cbiAgd3JpdGVWYWx1ZSh2YWx1ZXM6IHN0cmluZ1tdKTogdm9pZCB7XG4gICAgdGhpcy5fdmFsdWUgPSB2YWx1ZXM7XG5cbiAgICBpZiAodGhpcy5vcHRpb25zKSB7XG4gICAgICB0aGlzLl9zZXRPcHRpb25zRnJvbVZhbHVlcyh2YWx1ZXMgfHwgW10pO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBJbXBsZW1lbnRlZCBhcyBhIHBhcnQgb2YgQ29udHJvbFZhbHVlQWNjZXNzb3IuICovXG4gIHNldERpc2FibGVkU3RhdGUoaXNEaXNhYmxlZDogYm9vbGVhbik6IHZvaWQge1xuICAgIHRoaXMuZGlzYWJsZWQgPSBpc0Rpc2FibGVkO1xuICB9XG5cbiAgLyoqIEltcGxlbWVudGVkIGFzIHBhcnQgb2YgQ29udHJvbFZhbHVlQWNjZXNzb3IuICovXG4gIHJlZ2lzdGVyT25DaGFuZ2UoZm46ICh2YWx1ZTogYW55KSA9PiB2b2lkKTogdm9pZCB7XG4gICAgdGhpcy5fb25DaGFuZ2UgPSBmbjtcbiAgfVxuXG4gIC8qKiBJbXBsZW1lbnRlZCBhcyBwYXJ0IG9mIENvbnRyb2xWYWx1ZUFjY2Vzc29yLiAqL1xuICByZWdpc3Rlck9uVG91Y2hlZChmbjogKCkgPT4gdm9pZCk6IHZvaWQge1xuICAgIHRoaXMuX29uVG91Y2hlZCA9IGZuO1xuICB9XG5cbiAgLyoqIFNldHMgdGhlIHNlbGVjdGVkIG9wdGlvbnMgYmFzZWQgb24gdGhlIHNwZWNpZmllZCB2YWx1ZXMuICovXG4gIHByaXZhdGUgX3NldE9wdGlvbnNGcm9tVmFsdWVzKHZhbHVlczogc3RyaW5nW10pIHtcbiAgICB0aGlzLm9wdGlvbnMuZm9yRWFjaChvcHRpb24gPT4gb3B0aW9uLl9zZXRTZWxlY3RlZChmYWxzZSkpO1xuXG4gICAgdmFsdWVzLmZvckVhY2godmFsdWUgPT4ge1xuICAgICAgY29uc3QgY29ycmVzcG9uZGluZ09wdGlvbiA9IHRoaXMub3B0aW9ucy5maW5kKG9wdGlvbiA9PiB7XG4gICAgICAgIC8vIFNraXAgb3B0aW9ucyB0aGF0IGFyZSBhbHJlYWR5IGluIHRoZSBtb2RlbC4gVGhpcyBhbGxvd3MgdXMgdG8gaGFuZGxlIGNhc2VzXG4gICAgICAgIC8vIHdoZXJlIHRoZSBzYW1lIHByaW1pdGl2ZSB2YWx1ZSBpcyBzZWxlY3RlZCBtdWx0aXBsZSB0aW1lcy5cbiAgICAgICAgcmV0dXJuIG9wdGlvbi5zZWxlY3RlZCA/IGZhbHNlIDogdGhpcy5jb21wYXJlV2l0aChvcHRpb24udmFsdWUsIHZhbHVlKTtcbiAgICAgIH0pO1xuXG4gICAgICBpZiAoY29ycmVzcG9uZGluZ09wdGlvbikge1xuICAgICAgICBjb3JyZXNwb25kaW5nT3B0aW9uLl9zZXRTZWxlY3RlZCh0cnVlKTtcbiAgICAgIH1cbiAgICB9KTtcbiAgfVxuXG4gIC8qKiBSZXR1cm5zIHRoZSB2YWx1ZXMgb2YgdGhlIHNlbGVjdGVkIG9wdGlvbnMuICovXG4gIHByaXZhdGUgX2dldFNlbGVjdGVkT3B0aW9uVmFsdWVzKCk6IHN0cmluZ1tdIHtcbiAgICByZXR1cm4gdGhpcy5vcHRpb25zLmZpbHRlcihvcHRpb24gPT4gb3B0aW9uLnNlbGVjdGVkKS5tYXAob3B0aW9uID0+IG9wdGlvbi52YWx1ZSk7XG4gIH1cblxuICAvKiogVG9nZ2xlcyB0aGUgc3RhdGUgb2YgdGhlIGN1cnJlbnRseSBmb2N1c2VkIG9wdGlvbiBpZiBlbmFibGVkLiAqL1xuICBwcml2YXRlIF90b2dnbGVGb2N1c2VkT3B0aW9uKCk6IHZvaWQge1xuICAgIGxldCBmb2N1c2VkSW5kZXggPSB0aGlzLl9rZXlNYW5hZ2VyLmFjdGl2ZUl0ZW1JbmRleDtcblxuICAgIGlmIChmb2N1c2VkSW5kZXggIT0gbnVsbCAmJiB0aGlzLl9pc1ZhbGlkSW5kZXgoZm9jdXNlZEluZGV4KSkge1xuICAgICAgbGV0IGZvY3VzZWRPcHRpb246IE1hdExpc3RPcHRpb24gPSB0aGlzLm9wdGlvbnMudG9BcnJheSgpW2ZvY3VzZWRJbmRleF07XG5cbiAgICAgIGlmIChmb2N1c2VkT3B0aW9uICYmICFmb2N1c2VkT3B0aW9uLmRpc2FibGVkICYmICh0aGlzLl9tdWx0aXBsZSB8fCAhZm9jdXNlZE9wdGlvbi5zZWxlY3RlZCkpIHtcbiAgICAgICAgZm9jdXNlZE9wdGlvbi50b2dnbGUoKTtcblxuICAgICAgICAvLyBFbWl0IGEgY2hhbmdlIGV2ZW50IGJlY2F1c2UgdGhlIGZvY3VzZWQgb3B0aW9uIGNoYW5nZWQgaXRzIHN0YXRlIHRocm91Z2ggdXNlclxuICAgICAgICAvLyBpbnRlcmFjdGlvbi5cbiAgICAgICAgdGhpcy5fZW1pdENoYW5nZUV2ZW50KGZvY3VzZWRPcHRpb24pO1xuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBTZXRzIHRoZSBzZWxlY3RlZCBzdGF0ZSBvbiBhbGwgb2YgdGhlIG9wdGlvbnNcbiAgICogYW5kIGVtaXRzIGFuIGV2ZW50IGlmIGFueXRoaW5nIGNoYW5nZWQuXG4gICAqL1xuICBwcml2YXRlIF9zZXRBbGxPcHRpb25zU2VsZWN0ZWQoaXNTZWxlY3RlZDogYm9vbGVhbiwgc2tpcERpc2FibGVkPzogYm9vbGVhbikge1xuICAgIC8vIEtlZXAgdHJhY2sgb2Ygd2hldGhlciBhbnl0aGluZyBjaGFuZ2VkLCBiZWNhdXNlIHdlIG9ubHkgd2FudCB0b1xuICAgIC8vIGVtaXQgdGhlIGNoYW5nZWQgZXZlbnQgd2hlbiBzb21ldGhpbmcgYWN0dWFsbHkgY2hhbmdlZC5cbiAgICBsZXQgaGFzQ2hhbmdlZCA9IGZhbHNlO1xuXG4gICAgdGhpcy5vcHRpb25zLmZvckVhY2gob3B0aW9uID0+IHtcbiAgICAgIGlmICgoIXNraXBEaXNhYmxlZCB8fCAhb3B0aW9uLmRpc2FibGVkKSAmJiBvcHRpb24uX3NldFNlbGVjdGVkKGlzU2VsZWN0ZWQpKSB7XG4gICAgICAgIGhhc0NoYW5nZWQgPSB0cnVlO1xuICAgICAgfVxuICAgIH0pO1xuXG4gICAgaWYgKGhhc0NoYW5nZWQpIHtcbiAgICAgIHRoaXMuX3JlcG9ydFZhbHVlQ2hhbmdlKCk7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIFV0aWxpdHkgdG8gZW5zdXJlIGFsbCBpbmRleGVzIGFyZSB2YWxpZC5cbiAgICogQHBhcmFtIGluZGV4IFRoZSBpbmRleCB0byBiZSBjaGVja2VkLlxuICAgKiBAcmV0dXJucyBUcnVlIGlmIHRoZSBpbmRleCBpcyB2YWxpZCBmb3Igb3VyIGxpc3Qgb2Ygb3B0aW9ucy5cbiAgICovXG4gIHByaXZhdGUgX2lzVmFsaWRJbmRleChpbmRleDogbnVtYmVyKTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIGluZGV4ID49IDAgJiYgaW5kZXggPCB0aGlzLm9wdGlvbnMubGVuZ3RoO1xuICB9XG5cbiAgLyoqIFJldHVybnMgdGhlIGluZGV4IG9mIHRoZSBzcGVjaWZpZWQgbGlzdCBvcHRpb24uICovXG4gIHByaXZhdGUgX2dldE9wdGlvbkluZGV4KG9wdGlvbjogTWF0TGlzdE9wdGlvbik6IG51bWJlciB7XG4gICAgcmV0dXJuIHRoaXMub3B0aW9ucy50b0FycmF5KCkuaW5kZXhPZihvcHRpb24pO1xuICB9XG5cbiAgLyoqIE1hcmtzIGFsbCB0aGUgb3B0aW9ucyB0byBiZSBjaGVja2VkIGluIHRoZSBuZXh0IGNoYW5nZSBkZXRlY3Rpb24gcnVuLiAqL1xuICBwcml2YXRlIF9tYXJrT3B0aW9uc0ZvckNoZWNrKCkge1xuICAgIGlmICh0aGlzLm9wdGlvbnMpIHtcbiAgICAgIHRoaXMub3B0aW9ucy5mb3JFYWNoKG9wdGlvbiA9PiBvcHRpb24uX21hcmtGb3JDaGVjaygpKTtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogUmVtb3ZlcyB0aGUgdGFiaW5kZXggZnJvbSB0aGUgc2VsZWN0aW9uIGxpc3QgYW5kIHJlc2V0cyBpdCBiYWNrIGFmdGVyd2FyZHMsIGFsbG93aW5nIHRoZSB1c2VyXG4gICAqIHRvIHRhYiBvdXQgb2YgaXQuIFRoaXMgcHJldmVudHMgdGhlIGxpc3QgZnJvbSBjYXB0dXJpbmcgZm9jdXMgYW5kIHJlZGlyZWN0aW5nIGl0IGJhY2sgd2l0aGluXG4gICAqIHRoZSBsaXN0LCBjcmVhdGluZyBhIGZvY3VzIHRyYXAgaWYgaXQgdXNlciB0cmllcyB0byB0YWIgYXdheS5cbiAgICovXG4gIHByaXZhdGUgX2FsbG93Rm9jdXNFc2NhcGUoKSB7XG4gICAgdGhpcy5fdGFiSW5kZXggPSAtMTtcblxuICAgIHNldFRpbWVvdXQoKCkgPT4ge1xuICAgICAgdGhpcy5fdGFiSW5kZXggPSAwO1xuICAgICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3IubWFya0ZvckNoZWNrKCk7XG4gICAgfSk7XG4gIH1cblxuICAvKiogVXBkYXRlcyB0aGUgdGFiaW5kZXggYmFzZWQgdXBvbiBpZiB0aGUgc2VsZWN0aW9uIGxpc3QgaXMgZW1wdHkuICovXG4gIHByaXZhdGUgX3VwZGF0ZVRhYkluZGV4KCk6IHZvaWQge1xuICAgIHRoaXMuX3RhYkluZGV4ID0gKHRoaXMub3B0aW9ucy5sZW5ndGggPT09IDApID8gLTEgOiAwO1xuICB9XG5cbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX2Rpc2FibGVkOiBCb29sZWFuSW5wdXQ7XG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV9kaXNhYmxlUmlwcGxlOiBCb29sZWFuSW5wdXQ7XG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV9tdWx0aXBsZTogQm9vbGVhbklucHV0O1xufVxuIl19