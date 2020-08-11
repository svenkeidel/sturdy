/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { ButtonHarnessFilters } from './button-harness-filters';
/** Harness for interacting with a standard mat-button in tests. */
export declare class MatButtonHarness extends ComponentHarness {
    /** The selector for the host element of a `MatButton` instance. */
    static hostSelector: string;
    /**
     * Gets a `HarnessPredicate` that can be used to search for a `MatButtonHarness` that meets
     * certain criteria.
     * @param options Options for filtering which button instances are considered a match.
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: ButtonHarnessFilters): HarnessPredicate<MatButtonHarness>;
    /** Clicks the button. */
    click(): Promise<void>;
    /** Whether the button is disabled. */
    isDisabled(): Promise<boolean>;
    /** Gets the button's label text. */
    getText(): Promise<string>;
    /** Focuses the button. */
    focus(): Promise<void>;
    /** Blurs the button. */
    blur(): Promise<void>;
}
