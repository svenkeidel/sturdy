/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { RowHarnessFilters, CellHarnessFilters } from './table-harness-filters';
import { MatCellHarness, MatHeaderCellHarness, MatFooterCellHarness } from './cell-harness';
/** Text extracted from a table row organized by columns. */
export interface MatRowHarnessColumnsText {
    [columnName: string]: string;
}
/** Harness for interacting with a standard Angular Material table row. */
export declare class MatRowHarness extends ComponentHarness {
    /** The selector for the host element of a `MatRowHarness` instance. */
    static hostSelector: string;
    /**
     * Gets a `HarnessPredicate` that can be used to search for a table row with specific attributes.
     * @param options Options for narrowing the search
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: RowHarnessFilters): HarnessPredicate<MatRowHarness>;
    /** Gets a list of `MatCellHarness` for all cells in the row. */
    getCells(filter?: CellHarnessFilters): Promise<MatCellHarness[]>;
    /** Gets the text of the cells in the row. */
    getCellTextByIndex(filter?: CellHarnessFilters): Promise<string[]>;
    /** Gets the text inside the row organized by columns. */
    getCellTextByColumnName(): Promise<MatRowHarnessColumnsText>;
}
/** Harness for interacting with a standard Angular Material table header row. */
export declare class MatHeaderRowHarness extends ComponentHarness {
    /** The selector for the host element of a `MatHeaderRowHarness` instance. */
    static hostSelector: string;
    /**
     * Gets a `HarnessPredicate` that can be used to search for
     * a table header row with specific attributes.
     * @param options Options for narrowing the search
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: RowHarnessFilters): HarnessPredicate<MatHeaderRowHarness>;
    /** Gets a list of `MatHeaderCellHarness` for all cells in the row. */
    getCells(filter?: CellHarnessFilters): Promise<MatHeaderCellHarness[]>;
    /** Gets the text of the cells in the header row. */
    getCellTextByIndex(filter?: CellHarnessFilters): Promise<string[]>;
    /** Gets the text inside the header row organized by columns. */
    getCellTextByColumnName(): Promise<MatRowHarnessColumnsText>;
}
/** Harness for interacting with a standard Angular Material table footer row. */
export declare class MatFooterRowHarness extends ComponentHarness {
    /** The selector for the host element of a `MatFooterRowHarness` instance. */
    static hostSelector: string;
    /**
     * Gets a `HarnessPredicate` that can be used to search for
     * a table footer row cell with specific attributes.
     * @param options Options for narrowing the search
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: RowHarnessFilters): HarnessPredicate<MatFooterRowHarness>;
    /** Gets a list of `MatFooterCellHarness` for all cells in the row. */
    getCells(filter?: CellHarnessFilters): Promise<MatFooterCellHarness[]>;
    /** Gets the text of the cells in the footer row. */
    getCellTextByIndex(filter?: CellHarnessFilters): Promise<string[]>;
    /** Gets the text inside the footer row organized by columns. */
    getCellTextByColumnName(): Promise<MatRowHarnessColumnsText>;
}
