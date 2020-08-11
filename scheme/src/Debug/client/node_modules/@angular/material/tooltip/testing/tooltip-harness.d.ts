/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { TooltipHarnessFilters } from './tooltip-harness-filters';
/** Harness for interacting with a standard mat-tooltip in tests. */
export declare class MatTooltipHarness extends ComponentHarness {
    private _optionalPanel;
    static hostSelector: string;
    /**
     * Gets a `HarnessPredicate` that can be used to search
     * for a tooltip trigger with specific attributes.
     * @param options Options for narrowing the search.
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: TooltipHarnessFilters): HarnessPredicate<MatTooltipHarness>;
    /** Shows the tooltip. */
    show(): Promise<void>;
    /** Hides the tooltip. */
    hide(): Promise<void>;
    /** Gets whether the tooltip is open. */
    isOpen(): Promise<boolean>;
    /** Gets a promise for the tooltip panel's text. */
    getTooltipText(): Promise<string>;
}
