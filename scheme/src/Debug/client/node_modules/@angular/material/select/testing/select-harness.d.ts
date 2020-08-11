/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { HarnessPredicate } from '@angular/cdk/testing';
import { MatFormFieldControlHarness } from '@angular/material/form-field/testing/control';
import { MatOptionHarness, MatOptgroupHarness, OptionHarnessFilters, OptgroupHarnessFilters } from '@angular/material/core/testing';
import { SelectHarnessFilters } from './select-harness-filters';
/** Harness for interacting with a standard mat-select in tests. */
export declare class MatSelectHarness extends MatFormFieldControlHarness {
    private _documentRootLocator;
    private _backdrop;
    private _trigger;
    private _value;
    static hostSelector: string;
    /**
     * Gets a `HarnessPredicate` that can be used to search for a `MatSelectHarness` that meets
     * certain criteria.
     * @param options Options for filtering which select instances are considered a match.
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: SelectHarnessFilters): HarnessPredicate<MatSelectHarness>;
    /** Gets a boolean promise indicating if the select is disabled. */
    isDisabled(): Promise<boolean>;
    /** Gets a boolean promise indicating if the select is valid. */
    isValid(): Promise<boolean>;
    /** Gets a boolean promise indicating if the select is required. */
    isRequired(): Promise<boolean>;
    /** Gets a boolean promise indicating if the select is empty (no value is selected). */
    isEmpty(): Promise<boolean>;
    /** Gets a boolean promise indicating if the select is in multi-selection mode. */
    isMultiple(): Promise<boolean>;
    /** Gets a promise for the select's value text. */
    getValueText(): Promise<string>;
    /** Focuses the select and returns a void promise that indicates when the action is complete. */
    focus(): Promise<void>;
    /** Blurs the select and returns a void promise that indicates when the action is complete. */
    blur(): Promise<void>;
    /** Gets the options inside the select panel. */
    getOptions(filter?: Omit<OptionHarnessFilters, 'ancestor'>): Promise<MatOptionHarness[]>;
    /** Gets the groups of options inside the panel. */
    getOptionGroups(filter?: Omit<OptgroupHarnessFilters, 'ancestor'>): Promise<MatOptgroupHarness[]>;
    /** Gets whether the select is open. */
    isOpen(): Promise<boolean>;
    /** Opens the select's panel. */
    open(): Promise<void>;
    /**
     * Clicks the options that match the passed-in filter. If the select is in multi-selection
     * mode all options will be clicked, otherwise the harness will pick the first matching option.
     */
    clickOptions(filter?: OptionHarnessFilters): Promise<void>;
    /** Closes the select's panel. */
    close(): Promise<void>;
    /** Gets the selector that should be used to find this select's panel. */
    private _getPanelSelector;
}
