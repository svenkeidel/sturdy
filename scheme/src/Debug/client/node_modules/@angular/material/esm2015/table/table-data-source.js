/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { _isNumberValue } from '@angular/cdk/coercion';
import { DataSource } from '@angular/cdk/table';
import { BehaviorSubject, combineLatest, merge, of as observableOf, Subscription, Subject, } from 'rxjs';
import { map } from 'rxjs/operators';
/**
 * Corresponds to `Number.MAX_SAFE_INTEGER`. Moved out into a variable here due to
 * flaky browser support and the value not being defined in Closure's typings.
 */
const MAX_SAFE_INTEGER = 9007199254740991;
/**
 * Data source that accepts a client-side data array and includes native support of filtering,
 * sorting (using MatSort), and pagination (using MatPaginator).
 *
 * Allows for sort customization by overriding sortingDataAccessor, which defines how data
 * properties are accessed. Also allows for filter customization by overriding filterTermAccessor,
 * which defines how row data is converted to a string for filter matching.
 *
 * **Note:** This class is meant to be a simple data source to help you get started. As such
 * it isn't equipped to handle some more advanced cases like robust i18n support or server-side
 * interactions. If your app needs to support more advanced use cases, consider implementing your
 * own `DataSource`.
 */
export class MatTableDataSource extends DataSource {
    constructor(initialData = []) {
        super();
        /** Stream emitting render data to the table (depends on ordered data changes). */
        this._renderData = new BehaviorSubject([]);
        /** Stream that emits when a new filter string is set on the data source. */
        this._filter = new BehaviorSubject('');
        /** Used to react to internal changes of the paginator that are made by the data source itself. */
        this._internalPageChanges = new Subject();
        /**
         * Subscription to the changes that should trigger an update to the table's rendered rows, such
         * as filtering, sorting, pagination, or base data changes.
         */
        this._renderChangesSubscription = Subscription.EMPTY;
        /**
         * Data accessor function that is used for accessing data properties for sorting through
         * the default sortData function.
         * This default function assumes that the sort header IDs (which defaults to the column name)
         * matches the data's properties (e.g. column Xyz represents data['Xyz']).
         * May be set to a custom function for different behavior.
         * @param data Data object that is being accessed.
         * @param sortHeaderId The name of the column that represents the data.
         */
        this.sortingDataAccessor = (data, sortHeaderId) => {
            const value = data[sortHeaderId];
            if (_isNumberValue(value)) {
                const numberValue = Number(value);
                // Numbers beyond `MAX_SAFE_INTEGER` can't be compared reliably so we
                // leave them as strings. For more info: https://goo.gl/y5vbSg
                return numberValue < MAX_SAFE_INTEGER ? numberValue : value;
            }
            return value;
        };
        /**
         * Gets a sorted copy of the data array based on the state of the MatSort. Called
         * after changes are made to the filtered data or when sort changes are emitted from MatSort.
         * By default, the function retrieves the active sort and its direction and compares data
         * by retrieving data using the sortingDataAccessor. May be overridden for a custom implementation
         * of data ordering.
         * @param data The array of data that should be sorted.
         * @param sort The connected MatSort that holds the current sort state.
         */
        this.sortData = (data, sort) => {
            const active = sort.active;
            const direction = sort.direction;
            if (!active || direction == '') {
                return data;
            }
            return data.sort((a, b) => {
                let valueA = this.sortingDataAccessor(a, active);
                let valueB = this.sortingDataAccessor(b, active);
                // If both valueA and valueB exist (truthy), then compare the two. Otherwise, check if
                // one value exists while the other doesn't. In this case, existing value should come last.
                // This avoids inconsistent results when comparing values to undefined/null.
                // If neither value exists, return 0 (equal).
                let comparatorResult = 0;
                if (valueA != null && valueB != null) {
                    // Check if one value is greater than the other; if equal, comparatorResult should remain 0.
                    if (valueA > valueB) {
                        comparatorResult = 1;
                    }
                    else if (valueA < valueB) {
                        comparatorResult = -1;
                    }
                }
                else if (valueA != null) {
                    comparatorResult = 1;
                }
                else if (valueB != null) {
                    comparatorResult = -1;
                }
                return comparatorResult * (direction == 'asc' ? 1 : -1);
            });
        };
        /**
         * Checks if a data object matches the data source's filter string. By default, each data object
         * is converted to a string of its properties and returns true if the filter has
         * at least one occurrence in that string. By default, the filter string has its whitespace
         * trimmed and the match is case-insensitive. May be overridden for a custom implementation of
         * filter matching.
         * @param data Data object used to check against the filter.
         * @param filter Filter string that has been set on the data source.
         * @returns Whether the filter matches against the data
         */
        this.filterPredicate = (data, filter) => {
            // Transform the data into a lowercase string of all property values.
            const dataStr = Object.keys(data).reduce((currentTerm, key) => {
                // Use an obscure Unicode character to delimit the words in the concatenated string.
                // This avoids matches where the values of two columns combined will match the user's query
                // (e.g. `Flute` and `Stop` will match `Test`). The character is intended to be something
                // that has a very low chance of being typed in by somebody in a text field. This one in
                // particular is "White up-pointing triangle with dot" from
                // https://en.wikipedia.org/wiki/List_of_Unicode_characters
                return currentTerm + data[key] + '◬';
            }, '').toLowerCase();
            // Transform the filter by converting it to lowercase and removing whitespace.
            const transformedFilter = filter.trim().toLowerCase();
            return dataStr.indexOf(transformedFilter) != -1;
        };
        this._data = new BehaviorSubject(initialData);
        this._updateChangeSubscription();
    }
    /** Array of data that should be rendered by the table, where each object represents one row. */
    get data() { return this._data.value; }
    set data(data) { this._data.next(data); }
    /**
     * Filter term that should be used to filter out objects from the data array. To override how
     * data objects match to this filter string, provide a custom function for filterPredicate.
     */
    get filter() { return this._filter.value; }
    set filter(filter) { this._filter.next(filter); }
    /**
     * Instance of the MatSort directive used by the table to control its sorting. Sort changes
     * emitted by the MatSort will trigger an update to the table's rendered data.
     */
    get sort() { return this._sort; }
    set sort(sort) {
        this._sort = sort;
        this._updateChangeSubscription();
    }
    /**
     * Instance of the MatPaginator component used by the table to control what page of the data is
     * displayed. Page changes emitted by the MatPaginator will trigger an update to the
     * table's rendered data.
     *
     * Note that the data source uses the paginator's properties to calculate which page of data
     * should be displayed. If the paginator receives its properties as template inputs,
     * e.g. `[pageLength]=100` or `[pageIndex]=1`, then be sure that the paginator's view has been
     * initialized before assigning it to this data source.
     */
    get paginator() { return this._paginator; }
    set paginator(paginator) {
        this._paginator = paginator;
        this._updateChangeSubscription();
    }
    /**
     * Subscribe to changes that should trigger an update to the table's rendered rows. When the
     * changes occur, process the current state of the filter, sort, and pagination along with
     * the provided base data and send it to the table for rendering.
     */
    _updateChangeSubscription() {
        // Sorting and/or pagination should be watched if MatSort and/or MatPaginator are provided.
        // The events should emit whenever the component emits a change or initializes, or if no
        // component is provided, a stream with just a null event should be provided.
        // The `sortChange` and `pageChange` acts as a signal to the combineLatests below so that the
        // pipeline can progress to the next step. Note that the value from these streams are not used,
        // they purely act as a signal to progress in the pipeline.
        const sortChange = this._sort ?
            merge(this._sort.sortChange, this._sort.initialized) :
            observableOf(null);
        const pageChange = this._paginator ?
            merge(this._paginator.page, this._internalPageChanges, this._paginator.initialized) :
            observableOf(null);
        const dataStream = this._data;
        // Watch for base data or filter changes to provide a filtered set of data.
        const filteredData = combineLatest([dataStream, this._filter])
            .pipe(map(([data]) => this._filterData(data)));
        // Watch for filtered data or sort changes to provide an ordered set of data.
        const orderedData = combineLatest([filteredData, sortChange])
            .pipe(map(([data]) => this._orderData(data)));
        // Watch for ordered data or page changes to provide a paged set of data.
        const paginatedData = combineLatest([orderedData, pageChange])
            .pipe(map(([data]) => this._pageData(data)));
        // Watched for paged data changes and send the result to the table to render.
        this._renderChangesSubscription.unsubscribe();
        this._renderChangesSubscription = paginatedData.subscribe(data => this._renderData.next(data));
    }
    /**
     * Returns a filtered data array where each filter object contains the filter string within
     * the result of the filterTermAccessor function. If no filter is set, returns the data array
     * as provided.
     */
    _filterData(data) {
        // If there is a filter string, filter out data that does not contain it.
        // Each data object is converted to a string using the function defined by filterTermAccessor.
        // May be overridden for customization.
        this.filteredData =
            !this.filter ? data : data.filter(obj => this.filterPredicate(obj, this.filter));
        if (this.paginator) {
            this._updatePaginator(this.filteredData.length);
        }
        return this.filteredData;
    }
    /**
     * Returns a sorted copy of the data if MatSort has a sort applied, otherwise just returns the
     * data array as provided. Uses the default data accessor for data lookup, unless a
     * sortDataAccessor function is defined.
     */
    _orderData(data) {
        // If there is no active sort or direction, return the data without trying to sort.
        if (!this.sort) {
            return data;
        }
        return this.sortData(data.slice(), this.sort);
    }
    /**
     * Returns a paged slice of the provided data array according to the provided MatPaginator's page
     * index and length. If there is no paginator provided, returns the data array as provided.
     */
    _pageData(data) {
        if (!this.paginator) {
            return data;
        }
        const startIndex = this.paginator.pageIndex * this.paginator.pageSize;
        return data.slice(startIndex, startIndex + this.paginator.pageSize);
    }
    /**
     * Updates the paginator to reflect the length of the filtered data, and makes sure that the page
     * index does not exceed the paginator's last page. Values are changed in a resolved promise to
     * guard against making property changes within a round of change detection.
     */
    _updatePaginator(filteredDataLength) {
        Promise.resolve().then(() => {
            const paginator = this.paginator;
            if (!paginator) {
                return;
            }
            paginator.length = filteredDataLength;
            // If the page index is set beyond the page, reduce it to the last page.
            if (paginator.pageIndex > 0) {
                const lastPageIndex = Math.ceil(paginator.length / paginator.pageSize) - 1 || 0;
                const newPageIndex = Math.min(paginator.pageIndex, lastPageIndex);
                if (newPageIndex !== paginator.pageIndex) {
                    paginator.pageIndex = newPageIndex;
                    // Since the paginator only emits after user-generated changes,
                    // we need our own stream so we know to should re-render the data.
                    this._internalPageChanges.next();
                }
            }
        });
    }
    /**
     * Used by the MatTable. Called when it connects to the data source.
     * @docs-private
     */
    connect() { return this._renderData; }
    /**
     * Used by the MatTable. Called when it is destroyed. No-op.
     * @docs-private
     */
    disconnect() { }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidGFibGUtZGF0YS1zb3VyY2UuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvdGFibGUvdGFibGUtZGF0YS1zb3VyY2UudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLGNBQWMsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQ3JELE9BQU8sRUFBQyxVQUFVLEVBQUMsTUFBTSxvQkFBb0IsQ0FBQztBQUM5QyxPQUFPLEVBQ0wsZUFBZSxFQUNmLGFBQWEsRUFDYixLQUFLLEVBRUwsRUFBRSxJQUFJLFlBQVksRUFDbEIsWUFBWSxFQUNaLE9BQU8sR0FDUixNQUFNLE1BQU0sQ0FBQztBQUdkLE9BQU8sRUFBQyxHQUFHLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQUVuQzs7O0dBR0c7QUFDSCxNQUFNLGdCQUFnQixHQUFHLGdCQUFnQixDQUFDO0FBRTFDOzs7Ozs7Ozs7Ozs7R0FZRztBQUNILE1BQU0sT0FBTyxrQkFBc0IsU0FBUSxVQUFhO0lBOEp0RCxZQUFZLGNBQW1CLEVBQUU7UUFDL0IsS0FBSyxFQUFFLENBQUM7UUEzSlYsa0ZBQWtGO1FBQ2pFLGdCQUFXLEdBQUcsSUFBSSxlQUFlLENBQU0sRUFBRSxDQUFDLENBQUM7UUFFNUQsNEVBQTRFO1FBQzNELFlBQU8sR0FBRyxJQUFJLGVBQWUsQ0FBUyxFQUFFLENBQUMsQ0FBQztRQUUzRCxrR0FBa0c7UUFDakYseUJBQW9CLEdBQUcsSUFBSSxPQUFPLEVBQVEsQ0FBQztRQUU1RDs7O1dBR0c7UUFDSCwrQkFBMEIsR0FBRyxZQUFZLENBQUMsS0FBSyxDQUFDO1FBaURoRDs7Ozs7Ozs7V0FRRztRQUNILHdCQUFtQixHQUNmLENBQUMsSUFBTyxFQUFFLFlBQW9CLEVBQWlCLEVBQUU7WUFDbkQsTUFBTSxLQUFLLEdBQUksSUFBNkIsQ0FBQyxZQUFZLENBQUMsQ0FBQztZQUUzRCxJQUFJLGNBQWMsQ0FBQyxL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IFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7X2lzTnVtYmVyVmFsdWV9IGZyb20gJ0Bhbmd1bGFyL2Nkay9jb2VyY2lvbic7XG5pbXBvcnQge0RhdGFTb3VyY2V9IGZyb20gJ0Bhbmd1bGFyL2Nkay90YWJsZSc7XG5pbXBvcnQge1xuICBCZWhhdmlvclN1YmplY3QsXG4gIGNvbWJpbmVMYXRlc3QsXG4gIG1lcmdlLFxuICBPYnNlcnZhYmxlLFxuICBvZiBhcyBvYnNlcnZhYmxlT2YsXG4gIFN1YnNjcmlwdGlvbixcbiAgU3ViamVjdCxcbn0gZnJvbSAncnhqcyc7XG5pbXBvcnQge01hdFBhZ2luYXRvciwgUGFnZUV2ZW50fSBmcm9tICdAYW5ndWxhci9tYXRlcmlhbC9wYWdpbmF0b3InO1xuaW1wb3J0IHtNYXRTb3J0LCBTb3J0fSBmcm9tICdAYW5ndWxhci9tYXRlcmlhbC9zb3J0JztcbmltcG9ydCB7bWFwfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5cbi8qKlxuICogQ29ycmVzcG9uZHMgdG8gYE51bWJlci5NQVhfU0FGRV9JTlRFR0VSYC4gTW92ZWQgb3V0IGludG8gYSB2YXJpYWJsZSBoZXJlIGR1ZSB0b1xuICogZmxha3kgYnJvd3NlciBzdXBwb3J0IGFuZCB0aGUgdmFsdWUgbm90IGJlaW5nIGRlZmluZWQgaW4gQ2xvc3VyZSdzIHR5cGluZ3MuXG4gKi9cbmNvbnN0IE1BWF9TQUZFX0lOVEVHRVIgPSA5MDA3MTk5MjU0NzQwOTkxO1xuXG4vKipcbiAqIERhdGEgc291cmNlIHRoYXQgYWNjZXB0cyBhIGNsaWVudC1zaWRlIGRhdGEgYXJyYXkgYW5kIGluY2x1ZGVzIG5hdGl2ZSBzdXBwb3J0IG9mIGZpbHRlcmluZyxcbiAqIHNvcnRpbmcgKHVzaW5nIE1hdFNvcnQpLCBhbmQgcGFnaW5hdGlvbiAodXNpbmcgTWF0UGFnaW5hdG9yKS5cbiAqXG4gKiBBbGxvd3MgZm9yIHNvcnQgY3VzdG9taXphdGlvbiBieSBvdmVycmlkaW5nIHNvcnRpbmdEYXRhQWNjZXNzb3IsIHdoaWNoIGRlZmluZXMgaG93IGRhdGFcbiAqIHByb3BlcnRpZXMgYXJlIGFjY2Vzc2VkLiBBbHNvIGFsbG93cyBmb3IgZmlsdGVyIGN1c3RvbWl6YXRpb24gYnkgb3ZlcnJpZGluZyBmaWx0ZXJUZXJtQWNjZXNzb3IsXG4gKiB3aGljaCBkZWZpbmVzIGhvdyByb3cgZGF0YSBpcyBjb252ZXJ0ZWQgdG8gYSBzdHJpbmcgZm9yIGZpbHRlciBtYXRjaGluZy5cbiAqXG4gKiAqKk5vdGU6KiogVGhpcyBjbGFzcyBpcyBtZWFudCB0byBiZSBhIHNpbXBsZSBkYXRhIHNvdXJjZSB0byBoZWxwIHlvdSBnZXQgc3RhcnRlZC4gQXMgc3VjaFxuICogaXQgaXNuJ3QgZXF1aXBwZWQgdG8gaGFuZGxlIHNvbWUgbW9yZSBhZHZhbmNlZCBjYXNlcyBsaWtlIHJvYnVzdCBpMThuIHN1cHBvcnQgb3Igc2VydmVyLXNpZGVcbiAqIGludGVyYWN0aW9ucy4gSWYgeW91ciBhcHAgbmVlZHMgdG8gc3VwcG9ydCBtb3JlIGFkdmFuY2VkIHVzZSBjYXNlcywgY29uc2lkZXIgaW1wbGVtZW50aW5nIHlvdXJcbiAqIG93biBgRGF0YVNvdXJjZWAuXG4gKi9cbmV4cG9ydCBjbGFzcyBNYXRUYWJsZURhdGFTb3VyY2U8VD4gZXh0ZW5kcyBEYXRhU291cmNlPFQ+IHtcbiAgLyoqIFN0cmVhbSB0aGF0IGVtaXRzIHdoZW4gYSBuZXcgZGF0YSBhcnJheSBpcyBzZXQgb24gdGhlIGRhdGEgc291cmNlLiAqL1xuICBwcml2YXRlIHJlYWRvbmx5IF9kYXRhOiBCZWhhdmlvclN1YmplY3Q8VFtdPjtcblxuICAvKiogU3RyZWFtIGVtaXR0aW5nIHJlbmRlciBkYXRhIHRvIHRoZSB0YWJsZSAoZGVwZW5kcyBvbiBvcmRlcmVkIGRhdGEgY2hhbmdlcykuICovXG4gIHByaXZhdGUgcmVhZG9ubHkgX3JlbmRlckRhdGEgPSBuZXcgQmVoYXZpb3JTdWJqZWN0PFRbXT4oW10pO1xuXG4gIC8qKiBTdHJlYW0gdGhhdCBlbWl0cyB3aGVuIGEgbmV3IGZpbHRlciBzdHJpbmcgaXMgc2V0IG9uIHRoZSBkYXRhIHNvdXJjZS4gKi9cbiAgcHJpdmF0ZSByZWFkb25seSBfZmlsdGVyID0gbmV3IEJlaGF2aW9yU3ViamVjdDxzdHJpbmc+KCcnKTtcblxuICAvKiogVXNlZCB0byByZWFjdCB0byBpbnRlcm5hbCBjaGFuZ2VzIG9mIHRoZSBwYWdpbmF0b3IgdGhhdCBhcmUgbWFkZSBieSB0aGUgZGF0YSBzb3VyY2UgaXRzZWxmLiAqL1xuICBwcml2YXRlIHJlYWRvbmx5IF9pbnRlcm5hbFBhZ2VDaGFuZ2VzID0gbmV3IFN1YmplY3Q8dm9pZD4oKTtcblxuICAvKipcbiAgICogU3Vic2NyaXB0aW9uIHRvIHRoZSBjaGFuZ2VzIHRoYXQgc2hvdWxkIHRyaWdnZXIgYW4gdXBkYXRlIHRvIHRoZSB0YWJsZSdzIHJlbmRlcmVkIHJvd3MsIHN1Y2hcbiAgICogYXMgZmlsdGVyaW5nLCBzb3J0aW5nLCBwYWdpbmF0aW9uLCBvciBiYXNlIGRhdGEgY2hhbmdlcy5cbiAgICovXG4gIF9yZW5kZXJDaGFuZ2VzU3Vic2NyaXB0aW9uID0gU3Vic2NyaXB0aW9uLkVNUFRZO1xuXG4gIC8qKlxuICAgKiBUaGUgZmlsdGVyZWQgc2V0IG9mIGRhdGEgdGhhdCBoYXMgYmVlbiBtYXRjaGVkIGJ5IHRoZSBmaWx0ZXIgc3RyaW5nLCBvciBhbGwgdGhlIGRhdGEgaWYgdGhlcmVcbiAgICogaXMgbm8gZmlsdGVyLiBVc2VmdWwgZm9yIGtub3dpbmcgdGhlIHNldCBvZiBkYXRhIHRoZSB0YWJsZSByZXByZXNlbnRzLlxuICAgKiBGb3IgZXhhbXBsZSwgYSAnc2VsZWN0QWxsKCknIGZ1bmN0aW9uIHdvdWxkIGxpa2VseSB3YW50IHRvIHNlbGVjdCB0aGUgc2V0IG9mIGZpbHRlcmVkIGRhdGFcbiAgICogc2hvd24gdG8gdGhlIHVzZXIgcmF0aGVyIHRoYW4gYWxsIHRoZSBkYXRhLlxuICAgKi9cbiAgZmlsdGVyZWREYXRhOiBUW107XG5cbiAgLyoqIEFycmF5IG9mIGRhdGEgdGhhdCBzaG91bGQgYmUgcmVuZGVyZWQgYnkgdGhlIHRhYmxlLCB3aGVyZSBlYWNoIG9iamVjdCByZXByZXNlbnRzIG9uZSByb3cuICovXG4gIGdldCBkYXRhKCkgeyByZXR1cm4gdGhpcy5fZGF0YS52YWx1ZTsgfVxuICBzZXQgZGF0YShkYXRhOiBUW10pIHsgdGhpcy5fZGF0YS5uZXh0KGRhdGEpOyB9XG5cbiAgLyoqXG4gICAqIEZpbHRlciB0ZXJtIHRoYXQgc2hvdWxkIGJlIHVzZWQgdG8gZmlsdGVyIG91dCBvYmplY3RzIGZyb20gdGhlIGRhdGEgYXJyYXkuIFRvIG92ZXJyaWRlIGhvd1xuICAgKiBkYXRhIG9iamVjdHMgbWF0Y2ggdG8gdGhpcyBmaWx0ZXIgc3RyaW5nLCBwcm92aWRlIGEgY3VzdG9tIGZ1bmN0aW9uIGZvciBmaWx0ZXJQcmVkaWNhdGUuXG4gICAqL1xuICBnZXQgZmlsdGVyKCk6IHN0cmluZyB7IHJldHVybiB0aGlzLl9maWx0ZXIudmFsdWU7IH1cbiAgc2V0IGZpbHRlcihmaWx0ZXI6IHN0cmluZykgeyB0aGlzLl9maWx0ZXIubmV4dChmaWx0ZXIpOyB9XG5cbiAgLyoqXG4gICAqIEluc3RhbmNlIG9mIHRoZSBNYXRTb3J0IGRpcmVjdGl2ZSB1c2VkIGJ5IHRoZSB0YWJsZSB0byBjb250cm9sIGl0cyBzb3J0aW5nLiBTb3J0IGNoYW5nZXNcbiAgICogZW1pdHRlZCBieSB0aGUgTWF0U29ydCB3aWxsIHRyaWdnZXIgYW4gdXBkYXRlIHRvIHRoZSB0YWJsZSdzIHJlbmRlcmVkIGRhdGEuXG4gICAqL1xuICBnZXQgc29ydCgpOiBNYXRTb3J0IHwgbnVsbCB7IHJldHVybiB0aGlzLl9zb3J0OyB9XG4gIHNldCBzb3J0KHNvcnQ6IE1hdFNvcnR8bnVsbCkge1xuICAgIHRoaXMuX3NvcnQgPSBzb3J0O1xuICAgIHRoaXMuX3VwZGF0ZUNoYW5nZVN1YnNjcmlwdGlvbigpO1xuICB9XG4gIHByaXZhdGUgX3NvcnQ6IE1hdFNvcnR8bnVsbDtcblxuICAvKipcbiAgICogSW5zdGFuY2Ugb2YgdGhlIE1hdFBhZ2luYXRvciBjb21wb25lbnQgdXNlZCBieSB0aGUgdGFibGUgdG8gY29udHJvbCB3aGF0IHBhZ2Ugb2YgdGhlIGRhdGEgaXNcbiAgICogZGlzcGxheWVkLiBQYWdlIGNoYW5nZXMgZW1pdHRlZCBieSB0aGUgTWF0UGFnaW5hdG9yIHdpbGwgdHJpZ2dlciBhbiB1cGRhdGUgdG8gdGhlXG4gICAqIHRhYmxlJ3MgcmVuZGVyZWQgZGF0YS5cbiAgICpcbiAgICogTm90ZSB0aGF0IHRoZSBkYXRhIHNvdXJjZSB1c2VzIHRoZSBwYWdpbmF0b3IncyBwcm9wZXJ0aWVzIHRvIGNhbGN1bGF0ZSB3aGljaCBwYWdlIG9mIGRhdGFcbiAgICogc2hvdWxkIGJlIGRpc3BsYXllZC4gSWYgdGhlIHBhZ2luYXRvciByZWNlaXZlcyBpdHMgcHJvcGVydGllcyBhcyB0ZW1wbGF0ZSBpbnB1dHMsXG4gICAqIGUuZy4gYFtwYWdlTGVuZ3RoXT0xMDBgIG9yIGBbcGFnZUluZGV4XT0xYCwgdGhlbiBiZSBzdXJlIHRoYXQgdGhlIHBhZ2luYXRvcidzIHZpZXcgaGFzIGJlZW5cbiAgICogaW5pdGlhbGl6ZWQgYmVmb3JlIGFzc2lnbmluZyBpdCB0byB0aGlzIGRhdGEgc291cmNlLlxuICAgKi9cbiAgZ2V0IHBhZ2luYXRvcigpOiBNYXRQYWdpbmF0b3IgfCBudWxsIHsgcmV0dXJuIHRoaXMuX3BhZ2luYXRvcjsgfVxuICBzZXQgcGFnaW5hdG9yKHBhZ2luYXRvcjogTWF0UGFnaW5hdG9yfG51bGwpIHtcbiAgICB0aGlzLl9wYWdpbmF0b3IgPSBwYWdpbmF0b3I7XG4gICAgdGhpcy5fdXBkYXRlQ2hhbmdlU3Vic2NyaXB0aW9uKCk7XG4gIH1cbiAgcHJpdmF0ZSBfcGFnaW5hdG9yOiBNYXRQYWdpbmF0b3J8bnVsbDtcblxuICAvKipcbiAgICogRGF0YSBhY2Nlc3NvciBmdW5jdGlvbiB0aGF0IGlzIHVzZWQgZm9yIGFjY2Vzc2luZyBkYXRhIHByb3BlcnRpZXMgZm9yIHNvcnRpbmcgdGhyb3VnaFxuICAgKiB0aGUgZGVmYXVsdCBzb3J0RGF0YSBmdW5jdGlvbi5cbiAgICogVGhpcyBkZWZhdWx0IGZ1bmN0aW9uIGFzc3VtZXMgdGhhdCB0aGUgc29ydCBoZWFkZXIgSURzICh3aGljaCBkZWZhdWx0cyB0byB0aGUgY29sdW1uIG5hbWUpXG4gICAqIG1hdGNoZXMgdGhlIGRhdGEncyBwcm9wZXJ0aWVzIChlLmcuIGNvbHVtbiBYeXogcmVwcmVzZW50cyBkYXRhWydYeXonXSkuXG4gICAqIE1heSBiZSBzZXQgdG8gYSBjdXN0b20gZnVuY3Rpb24gZm9yIGRpZmZlcmVudCBiZWhhdmlvci5cbiAgICogQHBhcmFtIGRhdGEgRGF0YSBvYmplY3QgdGhhdCBpcyBiZWluZyBhY2Nlc3NlZC5cbiAgICogQHBhcmFtIHNvcnRIZWFkZXJJZCBUaGUgbmFtZSBvZiB0aGUgY29sdW1uIHRoYXQgcmVwcmVzZW50cyB0aGUgZGF0YS5cbiAgICovXG4gIHNvcnRpbmdEYXRhQWNjZXNzb3I6ICgoZGF0YTogVCwgc29ydEhlYWRlcklkOiBzdHJpbmcpID0+IHN0cmluZ3xudW1iZXIpID1cbiAgICAgIChkYXRhOiBULCBzb3J0SGVhZGVySWQ6IHN0cmluZyk6IHN0cmluZ3xudW1iZXIgPT4ge1xuICAgIGNvbnN0IHZhbHVlID0gKGRhdGEgYXMge1trZXk6IHN0cmluZ106IGFueX0pW3NvcnRIZWFkZXJJZF07XG5cbiAgICBpZiAoX2lzTnVtYmVyVmFsdWUodmFsdWUpKSB7XG4gICAgICBjb25zdCBudW1iZXJWYWx1ZSA9IE51bWJlcih2YWx1ZSk7XG5cbiAgICAgIC8vIE51bWJlcnMgYmV5b25kIGBNQVhfU0FGRV9JTlRFR0VSYCBjYW4ndCBiZSBjb21wYXJlZCByZWxpYWJseSBzbyB3ZVxuICAgICAgLy8gbGVhdmUgdGhlbSBhcyBzdHJpbmdzLiBGb3IgbW9yZSBpbmZvOiBodHRwczovL2dvby5nbC95NXZiU2dcbiAgICAgIHJldHVybiBudW1iZXJWYWx1ZSA8IE1BWF9TQUZFX0lOVEVHRVIgPyBudW1iZXJWYWx1ZSA6IHZhbHVlO1xuICAgIH1cblxuICAgIHJldHVybiB2YWx1ZTtcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXRzIGEgc29ydGVkIGNvcHkgb2YgdGhlIGRhdGEgYXJyYXkgYmFzZWQgb24gdGhlIHN0YXRlIG9mIHRoZSBNYXRTb3J0LiBDYWxsZWRcbiAgICogYWZ0ZXIgY2hhbmdlcyBhcmUgbWFkZSB0byB0aGUgZmlsdGVyZWQgZGF0YSBvciB3aGVuIHNvcnQgY2hhbmdlcyBhcmUgZW1pdHRlZCBmcm9tIE1hdFNvcnQuXG4gICAqIEJ5IGRlZmF1bHQsIHRoZSBmdW5jdGlvbiByZXRyaWV2ZXMgdGhlIGFjdGl2ZSBzb3J0IGFuZCBpdHMgZGlyZWN0aW9uIGFuZCBjb21wYXJlcyBkYXRhXG4gICAqIGJ5IHJldHJpZXZpbmcgZGF0YSB1c2luZyB0aGUgc29ydGluZ0RhdGFBY2Nlc3Nvci4gTWF5IGJlIG92ZXJyaWRkZW4gZm9yIGEgY3VzdG9tIGltcGxlbWVudGF0aW9uXG4gICAqIG9mIGRhdGEgb3JkZXJpbmcuXG4gICAqIEBwYXJhbSBkYXRhIFRoZSBhcnJheSBvZiBkYXRhIHRoYXQgc2hvdWxkIGJlIHNvcnRlZC5cbiAgICogQHBhcmFtIHNvcnQgVGhlIGNvbm5lY3RlZCBNYXRTb3J0IHRoYXQgaG9sZHMgdGhlIGN1cnJlbnQgc29ydCBzdGF0ZS5cbiAgICovXG4gIHNvcnREYXRhOiAoKGRhdGE6IFRbXSwgc29ydDogTWF0U29ydCkgPT4gVFtdKSA9IChkYXRhOiBUW10sIHNvcnQ6IE1hdFNvcnQpOiBUW10gPT4ge1xuICAgIGNvbnN0IGFjdGl2ZSA9IHNvcnQuYWN0aXZlO1xuICAgIGNvbnN0IGRpcmVjdGlvbiA9IHNvcnQuZGlyZWN0aW9uO1xuICAgIGlmICghYWN0aXZlIHx8IGRpcmVjdGlvbiA9PSAnJykgeyByZXR1cm4gZGF0YTsgfVxuXG4gICAgcmV0dXJuIGRhdGEuc29ydCgoYSwgYikgPT4ge1xuICAgICAgbGV0IHZhbHVlQSA9IHRoaXMuc29ydGluZ0RhdGFBY2Nlc3NvcihhLCBhY3RpdmUpO1xuICAgICAgbGV0IHZhbHVlQiA9IHRoaXMuc29ydGluZ0RhdGFBY2Nlc3NvcihiLCBhY3RpdmUpO1xuXG4gICAgICAvLyBJZiBib3RoIHZhbHVlQSBhbmQgdmFsdWVCIGV4aXN0ICh0cnV0aHkpLCB0aGVuIGNvbXBhcmUgdGhlIHR3by4gT3RoZXJ3aXNlLCBjaGVjayBpZlxuICAgICAgLy8gb25lIHZhbHVlIGV4aXN0cyB3aGlsZSB0aGUgb3RoZXIgZG9lc24ndC4gSW4gdGhpcyBjYXNlLCBleGlzdGluZyB2YWx1ZSBzaG91bGQgY29tZSBsYXN0LlxuICAgICAgLy8gVGhpcyBhdm9pZHMgaW5jb25zaXN0ZW50IHJlc3VsdHMgd2hlbiBjb21wYXJpbmcgdmFsdWVzIHRvIHVuZGVmaW5lZC9udWxsLlxuICAgICAgLy8gSWYgbmVpdGhlciB2YWx1ZSBleGlzdHMsIHJldHVybiAwIChlcXVhbCkuXG4gICAgICBsZXQgY29tcGFyYXRvclJlc3VsdCA9IDA7XG4gICAgICBpZiAodmFsdWVBICE9IG51bGwgJiYgdmFsdWVCICE9IG51bGwpIHtcbiAgICAgICAgLy8gQ2hlY2sgaWYgb25lIHZhbHVlIGlzIGdyZWF0ZXIgdGhhbiB0aGUgb3RoZXI7IGlmIGVxdWFsLCBjb21wYXJhdG9yUmVzdWx0IHNob3VsZCByZW1haW4gMC5cbiAgICAgICAgaWYgKHZhbHVlQSA+IHZhbHVlQikge1xuICAgICAgICAgIGNvbXBhcmF0b3JSZXN1bHQgPSAxO1xuICAgICAgICB9IGVsc2UgaWYgKHZhbHVlQSA8IHZhbHVlQikge1xuICAgICAgICAgIGNvbXBhcmF0b3JSZXN1bHQgPSAtMTtcbiAgICAgICAgfVxuICAgICAgfSBlbHNlIGlmICh2YWx1ZUEgIT0gbnVsbCkge1xuICAgICAgICBjb21wYXJhdG9yUmVzdWx0ID0gMTtcbiAgICAgIH0gZWxzZSBpZiAodmFsdWVCICE9IG51bGwpIHtcbiAgICAgICAgY29tcGFyYXRvclJlc3VsdCA9IC0xO1xuICAgICAgfVxuXG4gICAgICByZXR1cm4gY29tcGFyYXRvclJlc3VsdCAqIChkaXJlY3Rpb24gPT0gJ2FzYycgPyAxIDogLTEpO1xuICAgIH0pO1xuICB9XG5cbiAgLyoqXG4gICAqIENoZWNrcyBpZiBhIGRhdGEgb2JqZWN0IG1hdGNoZXMgdGhlIGRhdGEgc291cmNlJ3MgZmlsdGVyIHN0cmluZy4gQnkgZGVmYXVsdCwgZWFjaCBkYXRhIG9iamVjdFxuICAgKiBpcyBjb252ZXJ0ZWQgdG8gYSBzdHJpbmcgb2YgaXRzIHByb3BlcnRpZXMgYW5kIHJldHVybnMgdHJ1ZSBpZiB0aGUgZmlsdGVyIGhhc1xuICAgKiBhdCBsZWFzdCBvbmUgb2NjdXJyZW5jZSBpbiB0aGF0IHN0cmluZy4gQnkgZGVmYXVsdCwgdGhlIGZpbHRlciBzdHJpbmcgaGFzIGl0cyB3aGl0ZXNwYWNlXG4gICAqIHRyaW1tZWQgYW5kIHRoZSBtYXRjaCBpcyBjYXNlLWluc2Vuc2l0aXZlLiBNYXkgYmUgb3ZlcnJpZGRlbiBmb3IgYSBjdXN0b20gaW1wbGVtZW50YXRpb24gb2ZcbiAgICogZmlsdGVyIG1hdGNoaW5nLlxuICAgKiBAcGFyYW0gZGF0YSBEYXRhIG9iamVjdCB1c2VkIHRvIGNoZWNrIGFnYWluc3QgdGhlIGZpbHRlci5cbiAgICogQHBhcmFtIGZpbHRlciBGaWx0ZXIgc3RyaW5nIHRoYXQgaGFzIGJlZW4gc2V0IG9uIHRoZSBkYXRhIHNvdXJjZS5cbiAgICogQHJldHVybnMgV2hldGhlciB0aGUgZmlsdGVyIG1hdGNoZXMgYWdhaW5zdCB0aGUgZGF0YVxuICAgKi9cbiAgZmlsdGVyUHJlZGljYXRlOiAoKGRhdGE6IFQsIGZpbHRlcjogc3RyaW5nKSA9PiBib29sZWFuKSA9IChkYXRhOiBULCBmaWx0ZXI6IHN0cmluZyk6IGJvb2xlYW4gPT4ge1xuICAgIC8vIFRyYW5zZm9ybSB0aGUgZGF0YSBpbnRvIGEgbG93ZXJjYXNlIHN0cmluZyBvZiBhbGwgcHJvcGVydHkgdmFsdWVzLlxuICAgIGNvbnN0IGRhdGFTdHIgPSBPYmplY3Qua2V5cyhkYXRhKS5yZWR1Y2UoKGN1cnJlbnRUZXJtOiBzdHJpbmcsIGtleTogc3RyaW5nKSA9PiB7XG4gICAgICAvLyBVc2UgYW4gb2JzY3VyZSBVbmljb2RlIGNoYXJhY3RlciB0byBkZWxpbWl0IHRoZSB3b3JkcyBpbiB0aGUgY29uY2F0ZW5hdGVkIHN0cmluZy5cbiAgICAgIC8vIFRoaXMgYXZvaWRzIG1hdGNoZXMgd2hlcmUgdGhlIHZhbHVlcyBvZiB0d28gY29sdW1ucyBjb21iaW5lZCB3aWxsIG1hdGNoIHRoZSB1c2VyJ3MgcXVlcnlcbiAgICAgIC8vIChlLmcuIGBGbHV0ZWAgYW5kIGBTdG9wYCB3aWxsIG1hdGNoIGBUZXN0YCkuIFRoZSBjaGFyYWN0ZXIgaXMgaW50ZW5kZWQgdG8gYmUgc29tZXRoaW5nXG4gICAgICAvLyB0aGF0IGhhcyBhIHZlcnkgbG93IGNoYW5jZSBvZiBiZWluZyB0eXBlZCBpbiBieSBzb21lYm9keSBpbiBhIHRleHQgZmllbGQuIFRoaXMgb25lIGluXG4gICAgICAvLyBwYXJ0aWN1bGFyIGlzIFwiV2hpdGUgdXAtcG9pbnRpbmcgdHJpYW5nbGUgd2l0aCBkb3RcIiBmcm9tXG4gICAgICAvLyBodHRwczovL2VuLndpa2lwZWRpYS5vcmcvd2lraS9MaXN0X29mX1VuaWNvZGVfY2hhcmFjdGVyc1xuICAgICAgcmV0dXJuIGN1cnJlbnRUZXJtICsgKGRhdGEgYXMge1trZXk6IHN0cmluZ106IGFueX0pW2tleV0gKyAn4pesJztcbiAgICB9LCAnJykudG9Mb3dlckNhc2UoKTtcblxuICAgIC8vIFRyYW5zZm9ybSB0aGUgZmlsdGVyIGJ5IGNvbnZlcnRpbmcgaXQgdG8gbG93ZXJjYXNlIGFuZCByZW1vdmluZyB3aGl0ZXNwYWNlLlxuICAgIGNvbnN0IHRyYW5zZm9ybWVkRmlsdGVyID0gZmlsdGVyLnRyaW0oKS50b0xvd2VyQ2FzZSgpO1xuXG4gICAgcmV0dXJuIGRhdGFTdHIuaW5kZXhPZih0cmFuc2Zvcm1lZEZpbHRlcikgIT0gLTE7XG4gIH1cblxuICBjb25zdHJ1Y3Rvcihpbml0aWFsRGF0YTogVFtdID0gW10pIHtcbiAgICBzdXBlcigpO1xuICAgIHRoaXMuX2RhdGEgPSBuZXcgQmVoYXZpb3JTdWJqZWN0PFRbXT4oaW5pdGlhbERhdGEpO1xuICAgIHRoaXMuX3VwZGF0ZUNoYW5nZVN1YnNjcmlwdGlvbigpO1xuICB9XG5cbiAgLyoqXG4gICAqIFN1YnNjcmliZSB0byBjaGFuZ2VzIHRoYXQgc2hvdWxkIHRyaWdnZXIgYW4gdXBkYXRlIHRvIHRoZSB0YWJsZSdzIHJlbmRlcmVkIHJvd3MuIFdoZW4gdGhlXG4gICAqIGNoYW5nZXMgb2NjdXIsIHByb2Nlc3MgdGhlIGN1cnJlbnQgc3RhdGUgb2YgdGhlIGZpbHRlciwgc29ydCwgYW5kIHBhZ2luYXRpb24gYWxvbmcgd2l0aFxuICAgKiB0aGUgcHJvdmlkZWQgYmFzZSBkYXRhIGFuZCBzZW5kIGl0IHRvIHRoZSB0YWJsZSBmb3IgcmVuZGVyaW5nLlxuICAgKi9cbiAgX3VwZGF0ZUNoYW5nZVN1YnNjcmlwdGlvbigpIHtcbiAgICAvLyBTb3J0aW5nIGFuZC9vciBwYWdpbmF0aW9uIHNob3VsZCBiZSB3YXRjaGVkIGlmIE1hdFNvcnQgYW5kL29yIE1hdFBhZ2luYXRvciBhcmUgcHJvdmlkZWQuXG4gICAgLy8gVGhlIGV2ZW50cyBzaG91bGQgZW1pdCB3aGVuZXZlciB0aGUgY29tcG9uZW50IGVtaXRzIGEgY2hhbmdlIG9yIGluaXRpYWxpemVzLCBvciBpZiBub1xuICAgIC8vIGNvbXBvbmVudCBpcyBwcm92aWRlZCwgYSBzdHJlYW0gd2l0aCBqdXN0IGEgbnVsbCBldmVudCBzaG91bGQgYmUgcHJvdmlkZWQuXG4gICAgLy8gVGhlIGBzb3J0Q2hhbmdlYCBhbmQgYHBhZ2VDaGFuZ2VgIGFjdHMgYXMgYSBzaWduYWwgdG8gdGhlIGNvbWJpbmVMYXRlc3RzIGJlbG93IHNvIHRoYXQgdGhlXG4gICAgLy8gcGlwZWxpbmUgY2FuIHByb2dyZXNzIHRvIHRoZSBuZXh0IHN0ZXAuIE5vdGUgdGhhdCB0aGUgdmFsdWUgZnJvbSB0aGVzZSBzdHJlYW1zIGFyZSBub3QgdXNlZCxcbiAgICAvLyB0aGV5IHB1cmVseSBhY3QgYXMgYSBzaWduYWwgdG8gcHJvZ3Jlc3MgaW4gdGhlIHBpcGVsaW5lLlxuICAgIGNvbnN0IHNvcnRDaGFuZ2U6IE9ic2VydmFibGU8U29ydHxudWxsfHZvaWQ+ID0gdGhpcy5fc29ydCA/XG4gICAgICAgIG1lcmdlKHRoaXMuX3NvcnQuc29ydENoYW5nZSwgdGhpcy5fc29ydC5pbml0aWFsaXplZCkgYXMgT2JzZXJ2YWJsZTxTb3J0fHZvaWQ+IDpcbiAgICAgICAgb2JzZXJ2YWJsZU9mKG51bGwpO1xuICAgIGNvbnN0IHBhZ2VDaGFuZ2U6IE9ic2VydmFibGU8UGFnZUV2ZW50fG51bGx8dm9pZD4gPSB0aGlzLl9wYWdpbmF0b3IgP1xuICAgICAgICBtZXJnZShcbiAgICAgICAgICB0aGlzLl9wYWdpbmF0b3IucGFnZSxcbiAgICAgICAgICB0aGlzLl9pbnRlcm5hbFBhZ2VDaGFuZ2VzLFxuICAgICAgICAgIHRoaXMuX3BhZ2luYXRvci5pbml0aWFsaXplZFxuICAgICAgICApIGFzIE9ic2VydmFibGU8UGFnZUV2ZW50fHZvaWQ+IDpcbiAgICAgICAgb2JzZXJ2YWJsZU9mKG51bGwpO1xuICAgIGNvbnN0IGRhdGFTdHJlYW0gPSB0aGlzLl9kYXRhO1xuICAgIC8vIFdhdGNoIGZvciBiYXNlIGRhdGEgb3IgZmlsdGVyIGNoYW5nZXMgdG8gcHJvdmlkZSBhIGZpbHRlcmVkIHNldCBvZiBkYXRhLlxuICAgIGNvbnN0IGZpbHRlcmVkRGF0YSA9IGNvbWJpbmVMYXRlc3QoW2RhdGFTdHJlYW0sIHRoaXMuX2ZpbHRlcl0pXG4gICAgICAucGlwZShtYXAoKFtkYXRhXSkgPT4gdGhpcy5fZmlsdGVyRGF0YShkYXRhKSkpO1xuICAgIC8vIFdhdGNoIGZvciBmaWx0ZXJlZCBkYXRhIG9yIHNvcnQgY2hhbmdlcyB0byBwcm92aWRlIGFuIG9yZGVyZWQgc2V0IG9mIGRhdGEuXG4gICAgY29uc3Qgb3JkZXJlZERhdGEgPSBjb21iaW5lTGF0ZXN0KFtmaWx0ZXJlZERhdGEsIHNvcnRDaGFuZ2VdKVxuICAgICAgLnBpcGUobWFwKChbZGF0YV0pID0+IHRoaXMuX29yZGVyRGF0YShkYXRhKSkpO1xuICAgIC8vIFdhdGNoIGZvciBvcmRlcmVkIGRhdGEgb3IgcGFnZSBjaGFuZ2VzIHRvIHByb3ZpZGUgYSBwYWdlZCBzZXQgb2YgZGF0YS5cbiAgICBjb25zdCBwYWdpbmF0ZWREYXRhID0gY29tYmluZUxhdGVzdChbb3JkZXJlZERhdGEsIHBhZ2VDaGFuZ2VdKVxuICAgICAgLnBpcGUobWFwKChbZGF0YV0pID0+IHRoaXMuX3BhZ2VEYXRhKGRhdGEpKSk7XG4gICAgLy8gV2F0Y2hlZCBmb3IgcGFnZWQgZGF0YSBjaGFuZ2VzIGFuZCBzZW5kIHRoZSByZXN1bHQgdG8gdGhlIHRhYmxlIHRvIHJlbmRlci5cbiAgICB0aGlzLl9yZW5kZXJDaGFuZ2VzU3Vic2NyaXB0aW9uLnVuc3Vic2NyaWJlKCk7XG4gICAgdGhpcy5fcmVuZGVyQ2hhbmdlc1N1YnNjcmlwdGlvbiA9IHBhZ2luYXRlZERhdGEuc3Vic2NyaWJlKGRhdGEgPT4gdGhpcy5fcmVuZGVyRGF0YS5uZXh0KGRhdGEpKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZXR1cm5zIGEgZmlsdGVyZWQgZGF0YSBhcnJheSB3aGVyZSBlYWNoIGZpbHRlciBvYmplY3QgY29udGFpbnMgdGhlIGZpbHRlciBzdHJpbmcgd2l0aGluXG4gICAqIHRoZSByZXN1bHQgb2YgdGhlIGZpbHRlclRlcm1BY2Nlc3NvciBmdW5jdGlvbi4gSWYgbm8gZmlsdGVyIGlzIHNldCwgcmV0dXJucyB0aGUgZGF0YSBhcnJheVxuICAgKiBhcyBwcm92aWRlZC5cbiAgICovXG4gIF9maWx0ZXJEYXRhKGRhdGE6IFRbXSkge1xuICAgIC8vIElmIHRoZXJlIGlzIGEgZmlsdGVyIHN0cmluZywgZmlsdGVyIG91dCBkYXRhIHRoYXQgZG9lcyBub3QgY29udGFpbiBpdC5cbiAgICAvLyBFYWNoIGRhdGEgb2JqZWN0IGlzIGNvbnZlcnRlZCB0byBhIHN0cmluZyB1c2luZyB0aGUgZnVuY3Rpb24gZGVmaW5lZCBieSBmaWx0ZXJUZXJtQWNjZXNzb3IuXG4gICAgLy8gTWF5IGJlIG92ZXJyaWRkZW4gZm9yIGN1c3RvbWl6YXRpb24uXG4gICAgdGhpcy5maWx0ZXJlZERhdGEgPVxuICAgICAgICAhdGhpcy5maWx0ZXIgPyBkYXRhIDogZGF0YS5maWx0ZXIob2JqID0+IHRoaXMuZmlsdGVyUHJlZGljYXRlKG9iaiwgdGhpcy5maWx0ZXIpKTtcblxuICAgIGlmICh0aGlzLnBhZ2luYXRvcikgeyB0aGlzLl91cGRhdGVQYWdpbmF0b3IodGhpcy5maWx0ZXJlZERhdGEubGVuZ3RoKTsgfVxuXG4gICAgcmV0dXJuIHRoaXMuZmlsdGVyZWREYXRhO1xuICB9XG5cbiAgLyoqXG4gICAqIFJldHVybnMgYSBzb3J0ZWQgY29weSBvZiB0aGUgZGF0YSBpZiBNYXRTb3J0IGhhcyBhIHNvcnQgYXBwbGllZCwgb3RoZXJ3aXNlIGp1c3QgcmV0dXJucyB0aGVcbiAgICogZGF0YSBhcnJheSBhcyBwcm92aWRlZC4gVXNlcyB0aGUgZGVmYXVsdCBkYXRhIGFjY2Vzc29yIGZvciBkYXRhIGxvb2t1cCwgdW5sZXNzIGFcbiAgICogc29ydERhdGFBY2Nlc3NvciBmdW5jdGlvbiBpcyBkZWZpbmVkLlxuICAgKi9cbiAgX29yZGVyRGF0YShkYXRhOiBUW10pOiBUW10ge1xuICAgIC8vIElmIHRoZXJlIGlzIG5vIGFjdGl2ZSBzb3J0IG9yIGRpcmVjdGlvbiwgcmV0dXJuIHRoZSBkYXRhIHdpdGhvdXQgdHJ5aW5nIHRvIHNvcnQuXG4gICAgaWYgKCF0aGlzLnNvcnQpIHsgcmV0dXJuIGRhdGE7IH1cblxuICAgIHJldHVybiB0aGlzLnNvcnREYXRhKGRhdGEuc2xpY2UoKSwgdGhpcy5zb3J0KTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZXR1cm5zIGEgcGFnZWQgc2xpY2Ugb2YgdGhlIHByb3ZpZGVkIGRhdGEgYXJyYXkgYWNjb3JkaW5nIHRvIHRoZSBwcm92aWRlZCBNYXRQYWdpbmF0b3IncyBwYWdlXG4gICAqIGluZGV4IGFuZCBsZW5ndGguIElmIHRoZXJlIGlzIG5vIHBhZ2luYXRvciBwcm92aWRlZCwgcmV0dXJucyB0aGUgZGF0YSBhcnJheSBhcyBwcm92aWRlZC5cbiAgICovXG4gIF9wYWdlRGF0YShkYXRhOiBUW10pOiBUW10ge1xuICAgIGlmICghdGhpcy5wYWdpbmF0b3IpIHsgcmV0dXJuIGRhdGE7IH1cblxuICAgIGNvbnN0IHN0YXJ0SW5kZXggPSB0aGlzLnBhZ2luYXRvci5wYWdlSW5kZXggKiB0aGlzLnBhZ2luYXRvci5wYWdlU2l6ZTtcbiAgICByZXR1cm4gZGF0YS5zbGljZShzdGFydEluZGV4LCBzdGFydEluZGV4ICsgdGhpcy5wYWdpbmF0b3IucGFnZVNpemUpO1xuICB9XG5cbiAgLyoqXG4gICAqIFVwZGF0ZXMgdGhlIHBhZ2luYXRvciB0byByZWZsZWN0IHRoZSBsZW5ndGggb2YgdGhlIGZpbHRlcmVkIGRhdGEsIGFuZCBtYWtlcyBzdXJlIHRoYXQgdGhlIHBhZ2VcbiAgICogaW5kZXggZG9lcyBub3QgZXhjZWVkIHRoZSBwYWdpbmF0b3IncyBsYXN0IHBhZ2UuIFZhbHVlcyBhcmUgY2hhbmdlZCBpbiBhIHJlc29sdmVkIHByb21pc2UgdG9cbiAgICogZ3VhcmQgYWdhaW5zdCBtYWtpbmcgcHJvcGVydHkgY2hhbmdlcyB3aXRoaW4gYSByb3VuZCBvZiBjaGFuZ2UgZGV0ZWN0aW9uLlxuICAgKi9cbiAgX3VwZGF0ZVBhZ2luYXRvcihmaWx0ZXJlZERhdGFMZW5ndGg6IG51bWJlcikge1xuICAgIFByb21pc2UucmVzb2x2ZSgpLnRoZW4oKCkgPT4ge1xuICAgICAgY29uc3QgcGFnaW5hdG9yID0gdGhpcy5wYWdpbmF0b3I7XG5cbiAgICAgIGlmICghcGFnaW5hdG9yKSB7IHJldHVybjsgfVxuXG4gICAgICBwYWdpbmF0b3IubGVuZ3RoID0gZmlsdGVyZWREYXRhTGVuZ3RoO1xuXG4gICAgICAvLyBJZiB0aGUgcGFnZSBpbmRleCBpcyBzZXQgYmV5b25kIHRoZSBwYWdlLCByZWR1Y2UgaXQgdG8gdGhlIGxhc3QgcGFnZS5cbiAgICAgIGlmIChwYWdpbmF0b3IucGFnZUluZGV4ID4gMCkge1xuICAgICAgICBjb25zdCBsYXN0UGFnZUluZGV4ID0gTWF0aC5jZWlsKHBhZ2luYXRvci5sZW5ndGggLyBwYWdpbmF0b3IucGFnZVNpemUpIC0gMSB8fCAwO1xuICAgICAgICBjb25zdCBuZXdQYWdlSW5kZXggPSBNYXRoLm1pbihwYWdpbmF0b3IucGFnZUluZGV4LCBsYXN0UGFnZUluZGV4KTtcblxuICAgICAgICBpZiAobmV3UGFnZUluZGV4ICE9PSBwYWdpbmF0b3IucGFnZUluZGV4KSB7XG4gICAgICAgICAgcGFnaW5hdG9yLnBhZ2VJbmRleCA9IG5ld1BhZ2VJbmRleDtcblxuICAgICAgICAgIC8vIFNpbmNlIHRoZSBwYWdpbmF0b3Igb25seSBlbWl0cyBhZnRlciB1c2VyLWdlbmVyYXRlZCBjaGFuZ2VzLFxuICAgICAgICAgIC8vIHdlIG5lZWQgb3VyIG93biBzdHJlYW0gc28gd2Uga25vdyB0byBzaG91bGQgcmUtcmVuZGVyIHRoZSBkYXRhLlxuICAgICAgICAgIHRoaXMuX2ludGVybmFsUGFnZUNoYW5nZXMubmV4dCgpO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfSk7XG4gIH1cblxuICAvKipcbiAgICogVXNlZCBieSB0aGUgTWF0VGFibGUuIENhbGxlZCB3aGVuIGl0IGNvbm5lY3RzIHRvIHRoZSBkYXRhIHNvdXJjZS5cbiAgICogQGRvY3MtcHJpdmF0ZVxuICAgKi9cbiAgY29ubmVjdCgpIHsgcmV0dXJuIHRoaXMuX3JlbmRlckRhdGE7IH1cblxuICAvKipcbiAgICogVXNlZCBieSB0aGUgTWF0VGFibGUuIENhbGxlZCB3aGVuIGl0IGlzIGRlc3Ryb3llZC4gTm8tb3AuXG4gICAqIEBkb2NzLXByaXZhdGVcbiAgICovXG4gIGRpc2Nvbm5lY3QoKSB7IH1cbn1cbiJdfQ==