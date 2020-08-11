/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { BooleanInput } from '@angular/cdk/coercion';
import { ElementRef, EventEmitter, OnDestroy, AfterViewInit } from '@angular/core';
import { AbstractControl, ControlValueAccessor, ValidationErrors, Validator, ValidatorFn } from '@angular/forms';
import { DateAdapter, MatDateFormats } from '@angular/material/core';
import { ExtractDateTypeFromSelection, MatDateSelectionModel } from './date-selection-model';
/**
 * An event used for datepicker input and change events. We don't always have access to a native
 * input or change event because the event may have been triggered by the user clicking on the
 * calendar popup. For consistency, we always use MatDatepickerInputEvent instead.
 */
export declare class MatDatepickerInputEvent<D, S = unknown> {
    /** Reference to the datepicker input component that emitted the event. */
    target: MatDatepickerInputBase<S, D>;
    /** Reference to the native input element associated with the datepicker input. */
    targetElement: HTMLElement;
    /** The new value for the target datepicker input. */
    value: D | null;
    constructor(
    /** Reference to the datepicker input component that emitted the event. */
    target: MatDatepickerInputBase<S, D>, 
    /** Reference to the native input element associated with the datepicker input. */
    targetElement: HTMLElement);
}
/** Function that can be used to filter out dates from a calendar. */
export declare type DateFilterFn<D> = (date: D | null) => boolean;
/** Base class for datepicker inputs. */
export declare abstract class MatDatepickerInputBase<S, D = ExtractDateTypeFromSelection<S>> implements ControlValueAccessor, AfterViewInit, OnDestroy, Validator {
    protected _elementRef: ElementRef<HTMLInputElement>;
    _dateAdapter: DateAdapter<D>;
    private _dateFormats;
    /** Whether the component has been initialized. */
    private _isInitialized;
    /** The value of the input. */
    get value(): D | null;
    set value(value: D | null);
    protected _model: MatDateSelectionModel<S, D> | undefined;
    /** Whether the datepicker-input is disabled. */
    get disabled(): boolean;
    set disabled(value: boolean);
    private _disabled;
    /** Emits when a `change` event is fired on this `<input>`. */
    readonly dateChange: EventEmitter<MatDatepickerInputEvent<D, S>>;
    /** Emits when an `input` event is fired on this `<input>`. */
    readonly dateInput: EventEmitter<MatDatepickerInputEvent<D, S>>;
    /** Emits when the value changes (either due to user input or programmatic change). */
    _valueChange: EventEmitter<D | null>;
    /** Emits when the disabled state has changed */
    _disabledChange: EventEmitter<boolean>;
    _onTouched: () => void;
    _validatorOnChange: () => void;
    protected _cvaOnChange: (value: any) => void;
    private _valueChangesSubscription;
    private _localeSubscription;
    /**
     * Since the value is kept on the model which is assigned in an Input,
     * we might get a value before we have a model. This property keeps track
     * of the value until we have somewhere to assign it.
     */
    private _pendingValue;
    /** The form control validator for whether the input parses. */
    private _parseValidator;
    /** The form control validator for the date filter. */
    private _filterValidator;
    /** The form control validator for the min date. */
    private _minValidator;
    /** The form control validator for the max date. */
    private _maxValidator;
    /** Gets the base validator functions. */
    protected _getValidators(): ValidatorFn[];
    /** Gets the minimum date for the input. Used for validation. */
    abstract _getMinDate(): D | null;
    /** Gets the maximum date for the input. Used for validation. */
    abstract _getMaxDate(): D | null;
    /** Gets the date filter function. Used for validation. */
    protected abstract _getDateFilter(): DateFilterFn<D> | undefined;
    /** Registers a date selection model with the input. */
    _registerModel(model: MatDateSelectionModel<S, D>): void;
    /** Opens the popup associated with the input. */
    protected abstract _openPopup(): void;
    /** Assigns a value to the input's model. */
    protected abstract _assignValueToModel(model: D | null): void;
    /** Converts a value from the model into a native value for the input. */
    protected abstract _getValueFromModel(modelValue: S): D | null;
    /** Combined form control validator for this input. */
    protected abstract _validator: ValidatorFn | null;
    /**
     * Callback that'll be invoked when the selection model is changed
     * from somewhere that's not the current datepicker input.
     */
    protected abstract _outsideValueChanged?: () => void;
    /** Whether the last value set on the input was valid. */
    protected _lastValueValid: boolean;
    constructor(_elementRef: ElementRef<HTMLInputElement>, _dateAdapter: DateAdapter<D>, _dateFormats: MatDateFormats);
    ngAfterViewInit(): void;
    ngOnDestroy(): void;
    /** @docs-private */
    registerOnValidatorChange(fn: () => void): void;
    /** @docs-private */
    validate(c: AbstractControl): ValidationErrors | null;
    writeValue(value: D): void;
    registerOnChange(fn: (value: any) => void): void;
    registerOnTouched(fn: () => void): void;
    setDisabledState(isDisabled: boolean): void;
    _onKeydown(event: KeyboardEvent): void;
    _onInput(value: string): void;
    _onChange(): void;
    /** Handles blur events on the input. */
    _onBlur(): void;
    /** Formats a value and sets it on the input element. */
    protected _formatValue(value: D | null): void;
    /**
     * @param obj The object to check.
     * @returns The given object if it is both a date instance and valid, otherwise null.
     */
    protected _getValidDateOrNull(obj: any): D | null;
    /** Assigns a value to the model. */
    private _assignValue;
    /** Whether a value is considered valid. */
    private _isValidValue;
    /**
     * Checks whether a parent control is disabled. This is in place so that it can be overridden
     * by inputs extending this one which can be placed inside of a group that can be disabled.
     */
    protected _parentDisabled(): boolean;
    static ngAcceptInputType_value: any;
    static ngAcceptInputType_disabled: BooleanInput;
}
