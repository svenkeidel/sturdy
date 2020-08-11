import { CdkTextareaAutosize, AutofillMonitor, TextFieldModule } from '@angular/cdk/text-field';
import { Directive, Input, InjectionToken, ElementRef, Optional, Self, Inject, NgZone, HostListener, NgModule } from '@angular/core';
import { coerceBooleanProperty } from '@angular/cdk/coercion';
import { getSupportedInputTypes, Platform } from '@angular/cdk/platform';
import { NgControl, NgForm, FormGroupDirective } from '@angular/forms';
import { mixinErrorState, ErrorStateMatcher } from '@angular/material/core';
import { MatFormFieldControl, MatFormFieldModule } from '@angular/material/form-field';
import { Subject } from 'rxjs';

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/**
 * Directive to automatically resize a textarea to fit its content.
 * @deprecated Use `cdkTextareaAutosize` from `@angular/cdk/text-field` instead.
 * @breaking-change 8.0.0
 */
let MatTextareaAutosize = /** @class */ (() => {
    class MatTextareaAutosize extends CdkTextareaAutosize {
        get matAutosizeMinRows() { return this.minRows; }
        set matAutosizeMinRows(value) { this.minRows = value; }
        get matAutosizeMaxRows() { return this.maxRows; }
        set matAutosizeMaxRows(value) { this.maxRows = value; }
        get matAutosize() { return this.enabled; }
        set matAutosize(value) { this.enabled = value; }
        get matTextareaAutosize() { return this.enabled; }
        set matTextareaAutosize(value) { this.enabled = value; }
    }
    MatTextareaAutosize.decorators = [
        { type: Directive, args: [{
                    selector: 'textarea[mat-autosize], textarea[matTextareaAutosize]',
                    exportAs: 'matTextareaAutosize',
                    inputs: ['cdkAutosizeMinRows', 'cdkAutosizeMaxRows'],
                    host: {
                        'class': 'cdk-textarea-autosize mat-autosize',
                        // Textarea elements that have the directive applied should have a single row by default.
                        // Browsers normally show two rows by default and therefore this limits the minRows binding.
                        'rows': '1',
                    },
                },] }
    ];
    MatTextareaAutosize.propDecorators = {
        matAutosizeMinRows: [{ type: Input }],
        matAutosizeMaxRows: [{ type: Input }],
        matAutosize: [{ type: Input, args: ['mat-autosize',] }],
        matTextareaAutosize: [{ type: Input }]
    };
    return MatTextareaAutosize;
})();

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** @docs-private */
function getMatInputUnsupportedTypeError(type) {
    return Error(`Input type "${type}" isn't supported by matInput.`);
}

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/**
 * This token is used to inject the object whose value should be set into `MatInput`. If none is
 * provided, the native `HTMLInputElement` is used. Directives like `MatDatepickerInput` can provide
 * themselves for this token, in order to make `MatInput` delegate the getting and setting of the
 * value to them.
 */
const MAT_INPUT_VALUE_ACCESSOR = new InjectionToken('MAT_INPUT_VALUE_ACCESSOR');

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
// Invalid input type. Using one of these will throw an MatInputUnsupportedTypeError.
const MAT_INPUT_INVALID_TYPES = [
    'button',
    'checkbox',
    'file',
    'hidden',
    'image',
    'radio',
    'range',
    'reset',
    'submit'
];
let nextUniqueId = 0;
// Boilerplate for applying mixins to MatInput.
/** @docs-private */
class MatInputBase {
    constructor(_defaultErrorStateMatcher, _parentForm, _parentFormGroup, 
    /** @docs-private */
    ngControl) {
        this._defaultErrorStateMatcher = _defaultErrorStateMatcher;
        this._parentForm = _parentForm;
        this._parentFormGroup = _parentFormGroup;
        this.ngControl = ngControl;
    }
}
const _MatInputMixinBase = mixinErrorState(MatInputBase);
/** Directive that allows a native input to work inside a `MatFormField`. */
let MatInput = /** @class */ (() => {
    class MatInput extends _MatInputMixinBase {
        constructor(_elementRef, _platform, 
        /** @docs-private */
        ngControl, _parentForm, _parentFormGroup, _defaultErrorStateMatcher, inputValueAccessor, _autofillMonitor, ngZone) {
            super(_defaultErrorStateMatcher, _parentForm, _parentFormGroup, ngControl);
            this._elementRef = _elementRef;
            this._platform = _platform;
            this.ngControl = ngControl;
            this._autofillMonitor = _autofillMonitor;
            this._uid = `mat-input-${nextUniqueId++}`;
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            this.focused = false;
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            this.stateChanges = new Subject();
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            this.controlType = 'mat-input';
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            this.autofilled = false;
            this._disabled = false;
            this._required = false;
            this._type = 'text';
            this._readonly = false;
            this._neverEmptyInputTypes = [
                'date',
                'datetime',
                'datetime-local',
                'month',
                'time',
                'week'
            ].filter(t => getSupportedInputTypes().has(t));
            const element = this._elementRef.nativeElement;
            const nodeName = element.nodeName.toLowerCase();
            // If no input value accessor was explicitly specified, use the element as the input value
            // accessor.
            this._inputValueAccessor = inputValueAccessor || element;
            this._previousNativeValue = this.value;
            // Force setter to be called in case id was not specified.
            this.id = this.id;
            // On some versions of iOS the caret gets stuck in the wrong place when holding down the delete
            // key. In order to get around this we need to "jiggle" the caret loose. Since this bug only
            // exists on iOS, we only bother to install the listener on iOS.
            if (_platform.IOS) {
                ngZone.runOutsideAngular(() => {
                    _elementRef.nativeElement.addEventListener('keyup', (event) => {
                        let el = event.target;
                        if (!el.value && !el.selectionStart && !el.selectionEnd) {
                            // Note: Just setting `0, 0` doesn't fix the issue. Setting
                            // `1, 1` fixes it for the first time that you type text and
                            // then hold delete. Toggling to `1, 1` and then back to
                            // `0, 0` seems to completely fix it.
                            el.setSelectionRange(1, 1);
                            el.setSelectionRange(0, 0);
                        }
                    });
                });
            }
            this._isServer = !this._platform.isBrowser;
            this._isNativeSelect = nodeName === 'select';
            this._isTextarea = nodeName === 'textarea';
            if (this._isNativeSelect) {
                this.controlType = element.multiple ? 'mat-native-select-multiple' :
                    'mat-native-select';
            }
        }
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        get disabled() {
            if (this.ngControl && this.ngControl.disabled !== null) {
                return this.ngControl.disabled;
            }
            return this._disabled;
        }
        set disabled(value) {
            this._disabled = coerceBooleanProperty(value);
            // Browsers may not fire the blur event if the input is disabled too quickly.
            // Reset from here to ensure that the element doesn't become stuck.
            if (this.focused) {
                this.focused = false;
                this.stateChanges.next();
            }
        }
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        get id() { return this._id; }
        set id(value) { this._id = value || this._uid; }
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        get required() { return this._required; }
        set required(value) { this._required = coerceBooleanProperty(value); }
        /** Input type of the element. */
        get type() { return this._type; }
        set type(value) {
            this._type = value || 'text';
            this._validateType();
            // When using Angular inputs, developers are no longer able to set the properties on the native
            // input element. To ensure that bindings for `type` work, we need to sync the setter
            // with the native property. Textarea elements don't support the type property or attribute.
            if (!this._isTextarea && getSupportedInputTypes().has(this._type)) {
                this._elementRef.nativeElement.type = this._type;
            }
        }
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        get value() { return this._inputValueAccessor.value; }
        set value(value) {
            if (value !== this.value) {
                this._inputValueAccessor.value = value;
                this.stateChanges.next();
            }
        }
        /** Whether the element is readonly. */
        get readonly() { return this._readonly; }
        set readonly(value) { this._readonly = coerceBooleanProperty(value); }
        ngAfterViewInit() {
            if (this._platform.isBrowser) {
                this._autofillMonitor.monitor(this._elementRef.nativeElement).subscribe(event => {
                    this.autofilled = event.isAutofilled;
                    this.stateChanges.next();
                });
            }
        }
        ngOnChanges() {
            this.stateChanges.next();
        }
        ngOnDestroy() {
            this.stateChanges.complete();
            if (this._platform.isBrowser) {
                this._autofillMonitor.stopMonitoring(this._elementRef.nativeElement);
            }
        }
        ngDoCheck() {
            if (this.ngControl) {
                // We need to re-evaluate this on every change detection cycle, because there are some
                // error triggers that we can't subscribe to (e.g. parent form submissions). This means
                // that whatever logic is in here has to be super lean or we risk destroying the performance.
                this.updateErrorState();
            }
            // We need to dirty-check the native element's value, because there are some cases where
            // we won't be notified when it changes (e.g. the consumer isn't using forms or they're
            // updating the value using `emitEvent: false`).
            this._dirtyCheckNativeValue();
        }
        /** Focuses the input. */
        focus(options) {
            this._elementRef.nativeElement.focus(options);
        }
        // We have to use a `HostListener` here in order to support both Ivy and ViewEngine.
        // In Ivy the `host` bindings will be merged when this class is extended, whereas in
        // ViewEngine they're overwritten.
        // TODO(crisbeto): we move this back into `host` once Ivy is turned on by default.
        /** Callback for the cases where the focused state of the input changes. */
        // tslint:disable:no-host-decorator-in-concrete
        // tslint:enable:no-host-decorator-in-concrete
        _focusChanged(isFocused) {
            if (isFocused !== this.focused && (!this.readonly || !isFocused)) {
                this.focused = isFocused;
                this.stateChanges.next();
            }
        }
        // We have to use a `HostListener` here in order to support both Ivy and ViewEngine.
        // In Ivy the `host` bindings will be merged when this class is extended, whereas in
        // ViewEngine they're overwritten.
        // TODO(crisbeto): we move this back into `host` once Ivy is turned on by default.
        // tslint:disable-next-line:no-host-decorator-in-concrete
        _onInput() {
            // This is a noop function and is used to let Angular know whenever the value changes.
            // Angular will run a new change detection each time the `input` event has been dispatched.
            // It's necessary that Angular recognizes the value change, because when floatingLabel
            // is set to false and Angular forms aren't used, the placeholder won't recognize the
            // value changes and will not disappear.
            // Listening to the input event wouldn't be necessary when the input is using the
            // FormsModule or ReactiveFormsModule, because Angular forms also listens to input events.
        }
        /** Does some manual dirty checking on the native input `value` property. */
        _dirtyCheckNativeValue() {
            const newValue = this._elementRef.nativeElement.value;
            if (this._previousNativeValue !== newValue) {
                this._previousNativeValue = newValue;
                this.stateChanges.next();
            }
        }
        /** Make sure the input is a supported type. */
        _validateType() {
            if (MAT_INPUT_INVALID_TYPES.indexOf(this._type) > -1) {
                throw getMatInputUnsupportedTypeError(this._type);
            }
        }
        /** Checks whether the input type is one of the types that are never empty. */
        _isNeverEmpty() {
            return this._neverEmptyInputTypes.indexOf(this._type) > -1;
        }
        /** Checks whether the input is invalid based on the native validation. */
        _isBadInput() {
            // The `validity` property won't be present on platform-server.
            let validity = this._elementRef.nativeElement.validity;
            return validity && validity.badInput;
        }
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        get empty() {
            return !this._isNeverEmpty() && !this._elementRef.nativeElement.value && !this._isBadInput() &&
                !this.autofilled;
        }
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        get shouldLabelFloat() {
            if (this._isNativeSelect) {
                // For a single-selection `<select>`, the label should float when the selected option has
                // a non-empty display value. For a `<select multiple>`, the label *always* floats to avoid
                // overlapping the label with the options.
                const selectElement = this._elementRef.nativeElement;
                const firstOption = selectElement.options[0];
                // On most browsers the `selectedIndex` will always be 0, however on IE and Edge it'll be
                // -1 if the `value` is set to something, that isn't in the list of options, at a later point.
                return this.focused || selectElement.multiple || !this.empty ||
                    !!(selectElement.selectedIndex > -1 && firstOption && firstOption.label);
            }
            else {
                return this.focused || !this.empty;
            }
        }
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        setDescribedByIds(ids) {
            this._ariaDescribedby = ids.join(' ');
        }
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        onContainerClick() {
            // Do not re-focus the input element if the element is already focused. Otherwise it can happen
            // that someone clicks on a time input and the cursor resets to the "hours" field while the
            // "minutes" field was actually clicked. See: https://github.com/angular/components/issues/12849
            if (!this.focused) {
                this.focus();
            }
        }
    }
    MatInput.decorators = [
        { type: Directive, args: [{
                    selector: `input[matInput], textarea[matInput], select[matNativeControl],
      input[matNativeControl], textarea[matNativeControl]`,
                    exportAs: 'matInput',
                    host: {
                        /**
                         * @breaking-change 8.0.0 remove .mat-form-field-autofill-control in favor of AutofillMonitor.
                         */
                        'class': 'mat-input-element mat-form-field-autofill-control',
                        '[class.mat-input-server]': '_isServer',
                        // Native input properties that are overwritten by Angular inputs need to be synced with
                        // the native input element. Otherwise property bindings for those don't work.
                        '[attr.id]': 'id',
                        '[attr.placeholder]': 'placeholder',
                        '[disabled]': 'disabled',
                        '[required]': 'required',
                        '[attr.readonly]': 'readonly && !_isNativeSelect || null',
                        '[attr.aria-describedby]': '_ariaDescribedby || null',
                        '[attr.aria-invalid]': 'errorState',
                        '[attr.aria-required]': 'required.toString()',
                    },
                    providers: [{ provide: MatFormFieldControl, useExisting: MatInput }],
                },] }
    ];
    MatInput.ctorParameters = () => [
        { type: ElementRef },
        { type: Platform },
        { type: NgControl, decorators: [{ type: Optional }, { type: Self }] },
        { type: NgForm, decorators: [{ type: Optional }] },
        { type: FormGroupDirective, decorators: [{ type: Optional }] },
        { type: ErrorStateMatcher },
        { type: undefined, decorators: [{ type: Optional }, { type: Self }, { type: Inject, args: [MAT_INPUT_VALUE_ACCESSOR,] }] },
        { type: AutofillMonitor },
        { type: NgZone }
    ];
    MatInput.propDecorators = {
        disabled: [{ type: Input }],
        id: [{ type: Input }],
        placeholder: [{ type: Input }],
        required: [{ type: Input }],
        type: [{ type: Input }],
        errorStateMatcher: [{ type: Input }],
        value: [{ type: Input }],
        readonly: [{ type: Input }],
        _focusChanged: [{ type: HostListener, args: ['focus', ['true'],] }, { type: HostListener, args: ['blur', ['false'],] }],
        _onInput: [{ type: HostListener, args: ['input',] }]
    };
    return MatInput;
})();

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
let MatInputModule = /** @class */ (() => {
    class MatInputModule {
    }
    MatInputModule.decorators = [
        { type: NgModule, args: [{
                    declarations: [MatInput, MatTextareaAutosize],
                    imports: [
                        TextFieldModule,
                        MatFormFieldModule,
                    ],
                    exports: [
                        TextFieldModule,
                        // We re-export the `MatFormFieldModule` since `MatInput` will almost always
                        // be used together with `MatFormField`.
                        MatFormFieldModule,
                        MatInput,
                        MatTextareaAutosize,
                    ],
                    providers: [ErrorStateMatcher],
                },] }
    ];
    return MatInputModule;
})();

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */

/**
 * Generated bundle index. Do not edit.
 */

export { MAT_INPUT_VALUE_ACCESSOR, MatInput, MatInputModule, MatTextareaAutosize, getMatInputUnsupportedTypeError };
//# sourceMappingURL=input.js.map
