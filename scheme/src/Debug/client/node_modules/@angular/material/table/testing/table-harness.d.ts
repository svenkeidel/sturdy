/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { TableHarnessFilters, RowHarnessFilters } from './table-harness-filters';
import { MatRowHarness, MatHeaderRowHarness, MatFooterRowHarness } from './row-harness';
/** Text extracted from a table organized by columns. */
export interface MatTableHarnessColumnsText {
    [columnName: string]: {
        text: string[];
        headerText: string[];
        footerText: string[];
    };
}
/** Harness for interacting with a standard mat-table in tests. */
export declare class MatTableHarness extends ComponentHarness {
    /** The selector for the host element of a `MatTableHarness` instance. */
    static hostSelector: string;
    /**
     * Gets a `HarnessPredicate` that can be used to search for a table with specific attributes.
     * @param options Options for narrowing the search
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: TableHarnessFilters): HarnessPredicate<MatTableHarness>;
    /** Gets all of the header rows in a table. */
    getHeaderRows(filter?: RowHarnessFilters): Promise<MatHeaderRowHarness[]>;
    /** Gets all of the regular data rows in a table. */
    getRows(filter?: RowHarnessFilters): Promise<MatRowHarness[]>;
    /** Gets all of the footer rows in a table. */
    getFooterRows(filter?: RowHarnessFilters): Promise<MatFooterRowHarness[]>;
    /** Gets the text inside the entire table organized by rows. */
    getCellTextByIndex(): Promise<string[][]>;
    /** Gets the text inside the entire table organized by columns. */
    getCellTextByColumnName(): Promise<MatTableHarnessColumnsText>;
}
