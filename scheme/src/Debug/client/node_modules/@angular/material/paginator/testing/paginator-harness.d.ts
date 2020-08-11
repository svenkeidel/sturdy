/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { PaginatorHarnessFilters } from './paginator-harness-filters';
/** Harness for interacting with a standard mat-paginator in tests. */
export declare class MatPaginatorHarness extends ComponentHarness {
    /** Selector used to find paginator instances. */
    static hostSelector: string;
    private _nextButton;
    private _previousButton;
    private _firstPageButton;
    private _lastPageButton;
    private _select;
    private _pageSizeFallback;
    private _rangeLabel;
    /**
     * Gets a `HarnessPredicate` that can be used to search for a `MatPaginatorHarness` that meets
     * certain criteria.
     * @param options Options for filtering which paginator instances are considered a match.
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: PaginatorHarnessFilters): HarnessPredicate<MatPaginatorHarness>;
    /** Goes to the next page in the paginator. */
    goToNextPage(): Promise<void>;
    /** Goes to the previous page in the paginator. */
    goToPreviousPage(): Promise<void>;
    /** Goes to the first page in the paginator. */
    goToFirstPage(): Promise<void>;
    /** Goes to the last page in the paginator. */
    goToLastPage(): Promise<void>;
    /**
     * Sets the page size of the paginator.
     * @param size Page size that should be select.
     */
    setPageSize(size: number): Promise<void>;
    /** Gets the page size of the paginator. */
    getPageSize(): Promise<number>;
    /** Gets the text of the range labe of the paginator. */
    getRangeLabel(): Promise<string>;
}
