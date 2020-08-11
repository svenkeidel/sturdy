/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MatOptgroupHarness, MatOptionHarness, OptgroupHarnessFilters, OptionHarnessFilters } from '@angular/material/core/testing';
import { AutocompleteHarnessFilters } from './autocomplete-harness-filters';
/** Harness for interacting with a standard mat-autocomplete in tests. */
export declare class MatAutocompleteHarness extends ComponentHarness {
    private _documentRootLocator;
    /** The selector for the host element of a `MatAutocomplete` instance. */
    static hostSelector: string;
    /**
     * Gets a `HarnessPredicate` that can be used to search for a `MatAutocompleteHarness` that meets
     * certain criteria.
     * @param options Options for filtering which autocomplete instances are considered a match.
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: AutocompleteHarnessFilters): HarnessPredicate<MatAutocompleteHarness>;
    /** Gets the value of the autocomplete input. */
    getValue(): Promise<string>;
    /** Whether the autocomplete input is disabled. */
    isDisabled(): Promise<boolean>;
    /** Focuses the autocomplete input. */
    focus(): Promise<void>;
    /** Blurs the autocomplete input. */
    blur(): Promise<void>;
    /** Enters text into the autocomplete. */
    enterText(value: string): Promise<void>;
    /** Gets the options inside the autocomplete panel. */
    getOptions(filters?: Omit<OptionHarnessFilters, 'ancestor'>): Promise<MatOptionHarness[]>;
    /** Gets the option groups inside the autocomplete panel. */
    getOptionGroups(filters?: Omit<OptgroupHarnessFilters, 'ancestor'>): Promise<MatOptgroupHarness[]>;
    /** Selects the first option matching the given filters. */
    selectOption(filters: OptionHarnessFilters): Promise<void>;
    /** Whether the autocomplete is open. */
    isOpen(): Promise<boolean>;
    /** Gets the panel associated with this autocomplete trigger. */
    private _getPanel;
    /** Gets the selector that can be used to find the autocomplete trigger's panel. */
    private _getPanelSelector;
}
