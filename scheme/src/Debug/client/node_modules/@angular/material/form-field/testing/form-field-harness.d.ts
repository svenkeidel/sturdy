/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ComponentHarness, ComponentHarnessConstructor, HarnessPredicate, TestElement } from '@angular/cdk/testing';
import { MatFormFieldControlHarness } from '@angular/material/form-field/testing/control';
import { MatInputHarness } from '@angular/material/input/testing';
import { MatSelectHarness } from '@angular/material/select/testing';
import { FormFieldHarnessFilters } from './form-field-harness-filters';
/** Possible harnesses of controls which can be bound to a form-field. */
export declare type FormFieldControlHarness = MatInputHarness | MatSelectHarness;
/** Harness for interacting with a standard Material form-field's in tests. */
export declare class MatFormFieldHarness extends ComponentHarness {
    static hostSelector: string;
    /**
     * Gets a `HarnessPredicate` that can be used to search for a `MatFormFieldHarness` that meets
     * certain criteria.
     * @param options Options for filtering which form field instances are considered a match.
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: FormFieldHarnessFilters): HarnessPredicate<MatFormFieldHarness>;
    private _prefixContainer;
    private _suffixContainer;
    private _label;
    private _errors;
    private _hints;
    private _inputControl;
    private _selectControl;
    /** Gets the appearance of the form-field. */
    getAppearance(): Promise<'legacy' | 'standard' | 'fill' | 'outline'>;
    /**
     * Gets the harness of the control that is bound to the form-field. Only
     * default controls such as "MatInputHarness" and "MatSelectHarness" are
     * supported.
     */
    getControl(): Promise<FormFieldControlHarness | null>;
    /**
     * Gets the harness of the control that is bound to the form-field. Searches
     * for a control that matches the specified harness type.
     */
    getControl<X extends MatFormFieldControlHarness>(type: ComponentHarnessConstructor<X>): Promise<X | null>;
    /**
     * Gets the harness of the control that is bound to the form-field. Searches
     * for a control that matches the specified harness predicate.
     */
    getControl<X extends MatFormFieldControlHarness>(type: HarnessPredicate<X>): Promise<X | null>;
    /** Whether the form-field has a label. */
    hasLabel(): Promise<boolean>;
    /** Gets the label of the form-field. */
    getLabel(): Promise<string | null>;
    /** Whether the form-field has errors. */
    hasErrors(): Promise<boolean>;
    /** Whether the label is currently floating. */
    isLabelFloating(): Promise<boolean>;
    /** Whether the form-field is disabled. */
    isDisabled(): Promise<boolean>;
    /** Whether the form-field is currently autofilled. */
    isAutofilled(): Promise<boolean>;
    /** Gets the theme color of the form-field. */
    getThemeColor(): Promise<'primary' | 'accent' | 'warn'>;
    /** Gets error messages which are currently displayed in the form-field. */
    getTextErrors(): Promise<string[]>;
    /** Gets hint messages which are currently displayed in the form-field. */
    getTextHints(): Promise<string[]>;
    /**
     * Gets a reference to the container element which contains all projected
     * prefixes of the form-field.
     */
    getHarnessLoaderForPrefix(): Promise<TestElement | null>;
    /**
     * Gets a reference to the container element which contains all projected
     * suffixes of the form-field.
     */
    getHarnessLoaderForSuffix(): Promise<TestElement | null>;
    /**
     * Whether the form control has been touched. Returns "null"
     * if no form control is set up.
     */
    isControlTouched(): Promise<boolean | null>;
    /**
     * Whether the form control is dirty. Returns "null"
     * if no form control is set up.
     */
    isControlDirty(): Promise<boolean | null>;
    /**
     * Whether the form control is valid. Returns "null"
     * if no form control is set up.
     */
    isControlValid(): Promise<boolean | null>;
    /**
     * Whether the form control is pending validation. Returns "null"
     * if no form control is set up.
     */
    isControlPending(): Promise<boolean | null>;
    /** Checks whether the form-field control has set up a form control. */
    private _hasFormControl;
}
