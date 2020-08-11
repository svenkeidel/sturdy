/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Injectable, Optional, SkipSelf, Directive } from '@angular/core';
import { DateAdapter } from '@angular/material/core';
import { Subject } from 'rxjs';
/** A class representing a range of dates. */
export class DateRange {
    constructor(
    /** The start date of the range. */
    start, 
    /** The end date of the range. */
    end) {
        this.start = start;
        this.end = end;
    }
}
/** A selection model containing a date selection. */
let MatDateSelectionModel = /** @class */ (() => {
    class MatDateSelectionModel {
        constructor(
        /** The current selection. */
        selection, _adapter) {
            this.selection = selection;
            this._adapter = _adapter;
            this._selectionChanged = new Subject();
            /** Emits when the selection has changed. */
            this.selectionChanged = this._selectionChanged.asObservable();
            this.selection = selection;
        }
        /**
         * Updates the current selection in the model.
         * @param value New selection that should be assigned.
         * @param source Object that triggered the selection change.
         */
        updateSelection(value, source) {
            this.selection = value;
            this._selectionChanged.next({ selection: value, source });
        }
        ngOnDestroy() {
            this._selectionChanged.complete();
        }
        _isValidDateInstance(date) {
            return this._adapter.isDateInstance(date) && this._adapter.isValid(date);
        }
    }
    MatDateSelectionModel.decorators = [
        { type: Directive }
    ];
    MatDateSelectionModel.ctorParameters = () => [
        { type: undefined },
        { type: DateAdapter }
    ];
    return MatDateSelectionModel;
})();
export { MatDateSelectionModel };
/**  A selection model that contains a single date. */
let MatSingleDateSelectionModel = /** @class */ (() => {
    class MatSingleDateSelectionModel extends MatDateSelectionModel {
        constructor(adapter) {
            super(null, adapter);
        }
        /**
         * Adds a date to the current selection. In the case of a single date selection, the added date
         * simply overwrites the previous selection
         */
        add(date) {
            super.updateSelection(date, this);
        }
        /** Checks whether the current selection is valid. */
        isValid() {
            return this.selection != null && this._isValidDateInstance(this.selection);
        }
        /**
         * Checks whether the current selection is complete. In the case of a single date selection, this
         * is true if the current selection is not null.
         */
        isComplete() {
            return this.selection != null;
        }
    }
    MatSingleDateSelectionModel.decorators = [
        { type: Injectable }
    ];
    MatSingleDateSelectionModel.ctorParameters = () => [
        { type: DateAdapter }
    ];
    return MatSingleDateSelectionModel;
})();
export { MatSingleDateSelectionModel };
/**  A selection model that contains a date range. */
let MatRangeDateSelectionModel = /** @class */ (() => {
    class MatRangeDateSelectionModel extends MatDateSelectionModel {
        constructor(adapter) {
            super(new DateRange(null, null), adapter);
        }
        /**
         * Adds a date to the current selection. In the case of a date range selection, the added date
         * fills in the next `null` value in the range. If both the start and the end already have a date,
         * the selection is reset so that the given date is the new `start` and the `end` is null.
         */
        add(date) {
            let { start, end } = this.selection;
            if (start == null) {
                start = date;
            }
            else if (end == null) {
                end = date;
            }
            else {
                start = date;
                end = null;
            }
            super.updateSelection(new DateRange(start, end), this);
        }
        /** Checks whether the current selection is valid. */
        isValid() {
            const { start, end } = this.selection;
            // Empty ranges are valid.
            if (start == null && end == null) {
                return true;
            }
            // Complete ranges are only valid if both dates are valid and the start is before the end.
            if (start != null && end != null) {
                return this._isValidDateInstance(start) && this._isValidDateInstance(end) &&
                    this._adapter.compareDate(start, end) <= 0;
            }
            // Partial ranges are valid if the start/end is valid.
            return (start == null || this._isValidDateInstance(start)) &&
                (end == null || this._isValidDateInstance(end));
        }
        /**
         * Checks whether the current selection is complete. In the case of a date range selection, this
         * is true if the current selection has a non-null `start` and `end`.
         */
        isComplete() {
            return this.selection.start != null && this.selection.end != null;
        }
    }
    MatRangeDateSelectionModel.decorators = [
        { type: Injectable }
    ];
    MatRangeDateSelectionModel.ctorParameters = () => [
        { type: DateAdapter }
    ];
    return MatRangeDateSelectionModel;
})();
export { MatRangeDateSelectionModel };
/** @docs-private */
export function MAT_SINGLE_DATE_SELECTION_MODEL_FACTORY(parent, adapter) {
    return parent || new MatSingleDateSelectionModel(adapter);
}
/** Used to provide a single selection model to a component. */
export const MAT_SINGLE_DATE_SELECTION_MODEL_PROVIDER = {
    provide: MatDateSelectionModel,
    deps: [[new Optional(), new SkipSelf(), MatDateSelectionModel], DateAdapter],
    useFactory: MAT_SINGLE_DATE_SELECTION_MODEL_FACTORY,
};
/** @docs-private */
export function MAT_RANGE_DATE_SELECTION_MODEL_FACTORY(parent, adapter) {
    return parent || new MatRangeDateSelectionModel(adapter);
}
/** Used to provide a range selection model to a component. */
export const MAT_RANGE_DATE_SELECTION_MODEL_PROVIDER = {
    provide: MatDateSelectionModel,
    deps: [[new Optional(), new SkipSelf(), MatDateSelectionModel], DateAdapter],
    useFactory: MAT_RANGE_DATE_SELECTION_MODEL_FACTORY,
};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGF0ZS1zZWxlY3Rpb24tbW9kZWwuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvZGF0ZXBpY2tlci9kYXRlLXNlbGVjdGlvbi1tb2RlbC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQWtCLFVBQVUsRUFBRSxRQUFRLEVBQUUsUUFBUSxFQUFhLFNBQVMsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUNwRyxPQUFPLEVBQUMsV0FBVyxFQUFDLE1BQU0sd0JBQXdCLENBQUM7QUFDbkQsT0FBTyxFQUFhLE9BQU8sRUFBQyxNQUFNLE1BQU0sQ0FBQztBQUV6Qyw2Q0FBNkM7QUFDN0MsTUFBTSxPQUFPLFNBQVM7SUFRcEI7SUFDRSxtQ0FBbUM7SUFDMUIsS0FBZTtJQUN4QixpQ0FBaUM7SUFDeEIsR0FBYTtRQUZiLFVBQUssR0FBTCxLQUFLLENBQVU7UUFFZixRQUFHLEdBQUgsR0FBRyxDQUFVO0lBQUcsQ0FBQztDQUM3QjtBQWlCRCxxREFBcUQ7QUFDckQ7SUFBQSxNQUNzQixxQkFBcUI7UUFPekM7UUFDRSw2QkFBNkI7UUFDcEIsU0FBWSxFQUNYLFFBQXdCO1lBRHpCLGNBQVMsR0FBVCxTQUFTLENBQUc7WUFDWCxhQUFRLEdBQVIsUUFBUSxDQUFnQjtZQVI1QixzQkFBaUIsR0FBRyxJQUFJLE9BQU8sRUFBK0IsQ0FBQztZQUV2RSw0Q0FBNEM7WUFDNUMscUJBQWdCLEdBQTRDLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxZQUFZLEVBQUUsQ0FBQztZQU1oRyxJQUFJLENBQUMsU0FBUyxHQUFHLFNBQVMsQ0FBQztRQUM3QixDQUFDO1FBRUQ7Ozs7V0FJRztRQUNILGVBQWUsQ0FBQyxLQUFRLEVBQUUsTUFBZTtZQUN0QyxJQUF1QixDQUFDLFNBQVMsR0FBRyxLQUFLLENBQUM7WUFDM0MsSUFBSSxDQUFDLGlCQUFpQixDQUFDLElBQUksQ0FBQyxFQUFDLFNBQVMs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LyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7RmFjdG9yeVByb3ZpZGVyLCBJbmplY3RhYmxlLCBPcHRpb25hbCwgU2tpcFNlbGYsIE9uRGVzdHJveSwgRGlyZWN0aXZlfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7RGF0ZUFkYXB0ZXJ9IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL2NvcmUnO1xuaW1wb3J0IHtPYnNlcnZhYmxlLCBTdWJqZWN0fSBmcm9tICdyeGpzJztcblxuLyoqIEEgY2xhc3MgcmVwcmVzZW50aW5nIGEgcmFuZ2Ugb2YgZGF0ZXMuICovXG5leHBvcnQgY2xhc3MgRGF0ZVJhbmdlPEQ+IHtcbiAgLyoqXG4gICAqIEVuc3VyZXMgdGhhdCBvYmplY3RzIHdpdGggYSBgc3RhcnRgIGFuZCBgZW5kYCBwcm9wZXJ0eSBjYW4ndCBiZSBhc3NpZ25lZCB0byBhIHZhcmlhYmxlIHRoYXRcbiAgICogZXhwZWN0cyBhIGBEYXRlUmFuZ2VgXG4gICAqL1xuICAvLyB0c2xpbnQ6ZGlzYWJsZS1uZXh0LWxpbmU6bm8tdW51c2VkLXZhcmlhYmxlXG4gIHByaXZhdGUgX2Rpc2FibGVTdHJ1Y3R1cmFsRXF1aXZhbGVuY3k6IG5ldmVyO1xuXG4gIGNvbnN0cnVjdG9yKFxuICAgIC8qKiBUaGUgc3RhcnQgZGF0ZSBvZiB0aGUgcmFuZ2UuICovXG4gICAgcmVhZG9ubHkgc3RhcnQ6IEQgfCBudWxsLFxuICAgIC8qKiBUaGUgZW5kIGRhdGUgb2YgdGhlIHJhbmdlLiAqL1xuICAgIHJlYWRvbmx5IGVuZDogRCB8IG51bGwpIHt9XG59XG5cbi8qKlxuICogQ29uZGl0aW9uYWxseSBwaWNrcyB0aGUgZGF0ZSB0eXBlLCBpZiBhIERhdGVSYW5nZSBpcyBwYXNzZWQgaW4uXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbmV4cG9ydCB0eXBlIEV4dHJhY3REYXRlVHlwZUZyb21TZWxlY3Rpb248VD4gPSBUIGV4dGVuZHMgRGF0ZVJhbmdlPGluZmVyIEQ+ID8gRCA6IE5vbk51bGxhYmxlPFQ+O1xuXG4vKiogRXZlbnQgZW1pdHRlZCBieSB0aGUgZGF0ZSBzZWxlY3Rpb24gbW9kZWwgd2hlbiBpdHMgc2VsZWN0aW9uIGNoYW5nZXMuICovXG5leHBvcnQgaW50ZXJmYWNlIERhdGVTZWxlY3Rpb25Nb2RlbENoYW5nZTxTPiB7XG4gIC8qKiBOZXcgdmFsdWUgZm9yIHRoZSBzZWxlY3Rpb24uICovXG4gIHNlbGVjdGlvbjogUztcblxuICAvKiogT2JqZWN0IHRoYXQgdHJpZ2dlcmVkIHRoZSBjaGFuZ2UuICovXG4gIHNvdXJjZTogdW5rbm93bjtcbn1cblxuLyoqIEEgc2VsZWN0aW9uIG1vZGVsIGNvbnRhaW5pbmcgYSBkYXRlIHNlbGVjdGlvbi4gKi9cbkBEaXJlY3RpdmUoKVxuZXhwb3J0IGFic3RyYWN0IGNsYXNzIE1hdERhdGVTZWxlY3Rpb25Nb2RlbDxTLCBEID0gRXh0cmFjdERhdGVUeXBlRnJvbVNlbGVjdGlvbjxTPj5cbiAgICBpbXBsZW1lbnRzIE9uRGVzdHJveSB7XG4gIHByaXZhdGUgX3NlbGVjdGlvbkNoYW5nZWQgPSBuZXcgU3ViamVjdDxEYXRlU2VsZWN0aW9uTW9kZWxDaGFuZ2U8Uz4+KCk7XG5cbiAgLyoqIEVtaXRzIHdoZW4gdGhlIHNlbGVjdGlvbiBoYXMgY2hhbmdlZC4gKi9cbiAgc2VsZWN0aW9uQ2hhbmdlZDogT2JzZXJ2YWJsZTxEYXRlU2VsZWN0aW9uTW9kZWxDaGFuZ2U8Uz4+ID0gdGhpcy5fc2VsZWN0aW9uQ2hhbmdlZC5hc09ic2VydmFibGUoKTtcblxuICBwcm90ZWN0ZWQgY29uc3RydWN0b3IoXG4gICAgLyoqIFRoZSBjdXJyZW50IHNlbGVjdGlvbi4gKi9cbiAgICByZWFkb25seSBzZWxlY3Rpb246IFMsXG4gICAgcHJvdGVjdGVkIF9hZGFwdGVyOiBEYXRlQWRhcHRlcjxEPikge1xuICAgIHRoaXMuc2VsZWN0aW9uID0gc2VsZWN0aW9uO1xuICB9XG5cbiAgLyoqXG4gICAqIFVwZGF0ZXMgdGhlIGN1cnJlbnQgc2VsZWN0aW9uIGluIHRoZSBtb2RlbC5cbiAgICogQHBhcmFtIHZhbHVlIE5ldyBzZWxlY3Rpb24gdGhhdCBzaG91bGQgYmUgYXNzaWduZWQuXG4gICAqIEBwYXJhbSBzb3VyY2UgT2JqZWN0IHRoYXQgdHJpZ2dlcmVkIHRoZSBzZWxlY3Rpb24gY2hhbmdlLlxuICAgKi9cbiAgdXBkYXRlU2VsZWN0aW9uKHZhbHVlOiBTLCBzb3VyY2U6IHVua25vd24pIHtcbiAgICAodGhpcyBhcyB7c2VsZWN0aW9uOiBTfSkuc2VsZWN0aW9uID0gdmFsdWU7XG4gICAgdGhpcy5fc2VsZWN0aW9uQ2hhbmdlZC5uZXh0KHtzZWxlY3Rpb246IHZhbHVlLCBzb3VyY2V9KTtcbiAgfVxuXG4gIG5nT25EZXN0cm95KCkge1xuICAgIHRoaXMuX3NlbGVjdGlvbkNoYW5nZWQuY29tcGxldGUoKTtcbiAgfVxuXG4gIHByb3RlY3RlZCBfaXNWYWxpZERhdGVJbnN0YW5jZShkYXRlOiBEKTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIHRoaXMuX2FkYXB0ZXIuaXNEYXRlSW5zdGFuY2UoZGF0ZSkgJiYgdGhpcy5fYWRhcHRlci5pc1ZhbGlkKGRhdGUpO1xuICB9XG5cbiAgLyoqIEFkZHMgYSBkYXRlIHRvIHRoZSBjdXJyZW50IHNlbGVjdGlvbi4gKi9cbiAgYWJzdHJhY3QgYWRkKGRhdGU6IEQgfCBudWxsKTogdm9pZDtcblxuICAvKiogQ2hlY2tzIHdoZXRoZXIgdGhlIGN1cnJlbnQgc2VsZWN0aW9uIGlzIHZhbGlkLiAqL1xuICBhYnN0cmFjdCBpc1ZhbGlkKCk6IGJvb2xlYW47XG5cbiAgLyoqIENoZWNrcyB3aGV0aGVyIHRoZSBjdXJyZW50IHNlbGVjdGlvbiBpcyBjb21wbGV0ZS4gKi9cbiAgYWJzdHJhY3QgaXNDb21wbGV0ZSgpOiBib29sZWFuO1xufVxuXG4vKiogIEEgc2VsZWN0aW9uIG1vZGVsIHRoYXQgY29udGFpbnMgYSBzaW5nbGUgZGF0ZS4gKi9cbkBJbmplY3RhYmxlKClcbmV4cG9ydCBjbGFzcyBNYXRTaW5nbGVEYXRlU2VsZWN0aW9uTW9kZWw8RD4gZXh0ZW5kcyBNYXREYXRlU2VsZWN0aW9uTW9kZWw8RCB8IG51bGwsIEQ+IHtcbiAgY29uc3RydWN0b3IoYWRhcHRlcjogRGF0ZUFkYXB0ZXI8RD4pIHtcbiAgICBzdXBlcihudWxsLCBhZGFwdGVyKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBBZGRzIGEgZGF0ZSB0byB0aGUgY3VycmVudCBzZWxlY3Rpb24uIEluIHRoZSBjYXNlIG9mIGEgc2luZ2xlIGRhdGUgc2VsZWN0aW9uLCB0aGUgYWRkZWQgZGF0ZVxuICAgKiBzaW1wbHkgb3ZlcndyaXRlcyB0aGUgcHJldmlvdXMgc2VsZWN0aW9uXG4gICAqL1xuICBhZGQoZGF0ZTogRCB8IG51bGwpIHtcbiAgICBzdXBlci51cGRhdGVTZWxlY3Rpb24oZGF0ZSwgdGhpcyk7XG4gIH1cblxuICAvKiogQ2hlY2tzIHdoZXRoZXIgdGhlIGN1cnJlbnQgc2VsZWN0aW9uIGlzIHZhbGlkLiAqL1xuICBpc1ZhbGlkKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLnNlbGVjdGlvbiAhPSBudWxsICYmIHRoaXMuX2lzVmFsaWREYXRlSW5zdGFuY2UodGhpcy5zZWxlY3Rpb24pO1xuICB9XG5cbiAgLyoqXG4gICAqIENoZWNrcyB3aGV0aGVyIHRoZSBjdXJyZW50IHNlbGVjdGlvbiBpcyBjb21wbGV0ZS4gSW4gdGhlIGNhc2Ugb2YgYSBzaW5nbGUgZGF0ZSBzZWxlY3Rpb24sIHRoaXNcbiAgICogaXMgdHJ1ZSBpZiB0aGUgY3VycmVudCBzZWxlY3Rpb24gaXMgbm90IG51bGwuXG4gICAqL1xuICBpc0NvbXBsZXRlKCkge1xuICAgIHJldHVybiB0aGlzLnNlbGVjdGlvbiAhPSBudWxsO1xuICB9XG59XG5cbi8qKiAgQSBzZWxlY3Rpb24gbW9kZWwgdGhhdCBjb250YWlucyBhIGRhdGUgcmFuZ2UuICovXG5ASW5qZWN0YWJsZSgpXG5leHBvcnQgY2xhc3MgTWF0UmFuZ2VEYXRlU2VsZWN0aW9uTW9kZWw8RD4gZXh0ZW5kcyBNYXREYXRlU2VsZWN0aW9uTW9kZWw8RGF0ZVJhbmdlPEQ+LCBEPiB7XG4gIGNvbnN0cnVjdG9yKGFkYXB0ZXI6IERhdGVBZGFwdGVyPEQ+KSB7XG4gICAgc3VwZXIobmV3IERhdGVSYW5nZTxEPihudWxsLCBudWxsKSwgYWRhcHRlcik7XG4gIH1cblxuICAvKipcbiAgICogQWRkcyBhIGRhdGUgdG8gdGhlIGN1cnJlbnQgc2VsZWN0aW9uLiBJbiB0aGUgY2FzZSBvZiBhIGRhdGUgcmFuZ2Ugc2VsZWN0aW9uLCB0aGUgYWRkZWQgZGF0ZVxuICAgKiBmaWxscyBpbiB0aGUgbmV4dCBgbnVsbGAgdmFsdWUgaW4gdGhlIHJhbmdlLiBJZiBib3RoIHRoZSBzdGFydCBhbmQgdGhlIGVuZCBhbHJlYWR5IGhhdmUgYSBkYXRlLFxuICAgKiB0aGUgc2VsZWN0aW9uIGlzIHJlc2V0IHNvIHRoYXQgdGhlIGdpdmVuIGRhdGUgaXMgdGhlIG5ldyBgc3RhcnRgIGFuZCB0aGUgYGVuZGAgaXMgbnVsbC5cbiAgICovXG4gIGFkZChkYXRlOiBEIHwgbnVsbCk6IHZvaWQge1xuICAgIGxldCB7c3RhcnQsIGVuZH0gPSB0aGlzLnNlbGVjdGlvbjtcblxuICAgIGlmIChzdGFydCA9PSBudWxsKSB7XG4gICAgICBzdGFydCA9IGRhdGU7XG4gICAgfSBlbHNlIGlmIChlbmQgPT0gbnVsbCkge1xuICAgICAgZW5kID0gZGF0ZTtcbiAgICB9IGVsc2Uge1xuICAgICAgc3RhcnQgPSBkYXRlO1xuICAgICAgZW5kID0gbnVsbDtcbiAgICB9XG5cbiAgICBzdXBlci51cGRhdGVTZWxlY3Rpb24obmV3IERhdGVSYW5nZTxEPihzdGFydCwgZW5kKSwgdGhpcyk7XG4gIH1cblxuICAvKiogQ2hlY2tzIHdoZXRoZXIgdGhlIGN1cnJlbnQgc2VsZWN0aW9uIGlzIHZhbGlkLiAqL1xuICBpc1ZhbGlkKCk6IGJvb2xlYW4ge1xuICAgIGNvbnN0IHtzdGFydCwgZW5kfSA9IHRoaXMuc2VsZWN0aW9uO1xuXG4gICAgLy8gRW1wdHkgcmFuZ2VzIGFyZSB2YWxpZC5cbiAgICBpZiAoc3RhcnQgPT0gbnVsbCAmJiBlbmQgPT0gbnVsbCkge1xuICAgICAgcmV0dXJuIHRydWU7XG4gICAgfVxuXG4gICAgLy8gQ29tcGxldGUgcmFuZ2VzIGFyZSBvbmx5IHZhbGlkIGlmIGJvdGggZGF0ZXMgYXJlIHZhbGlkIGFuZCB0aGUgc3RhcnQgaXMgYmVmb3JlIHRoZSBlbmQuXG4gICAgaWYgKHN0YXJ0ICE9IG51bGwgJiYgZW5kICE9IG51bGwpIHtcbiAgICAgIHJldHVybiB0aGlzLl9pc1ZhbGlkRGF0ZUluc3RhbmNlKHN0YXJ0KSAmJiB0aGlzLl9pc1ZhbGlkRGF0ZUluc3RhbmNlKGVuZCkgJiZcbiAgICAgICAgICAgICB0aGlzLl9hZGFwdGVyLmNvbXBhcmVEYXRlKHN0YXJ0LCBlbmQpIDw9IDA7XG4gICAgfVxuXG4gICAgLy8gUGFydGlhbCByYW5nZXMgYXJlIHZhbGlkIGlmIHRoZSBzdGFydC9lbmQgaXMgdmFsaWQuXG4gICAgcmV0dXJuIChzdGFydCA9PSBudWxsIHx8IHRoaXMuX2lzVmFsaWREYXRlSW5zdGFuY2Uoc3RhcnQpKSAmJlxuICAgICAgICAgICAoZW5kID09IG51bGwgfHwgdGhpcy5faXNWYWxpZERhdGVJbnN0YW5jZShlbmQpKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBDaGVja3Mgd2hldGhlciB0aGUgY3VycmVudCBzZWxlY3Rpb24gaXMgY29tcGxldGUuIEluIHRoZSBjYXNlIG9mIGEgZGF0ZSByYW5nZSBzZWxlY3Rpb24sIHRoaXNcbiAgICogaXMgdHJ1ZSBpZiB0aGUgY3VycmVudCBzZWxlY3Rpb24gaGFzIGEgbm9uLW51bGwgYHN0YXJ0YCBhbmQgYGVuZGAuXG4gICAqL1xuICBpc0NvbXBsZXRlKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLnNlbGVjdGlvbi5zdGFydCAhPSBudWxsICYmIHRoaXMuc2VsZWN0aW9uLmVuZCAhPSBudWxsO1xuICB9XG59XG5cbi8qKiBAZG9jcy1wcml2YXRlICovXG5leHBvcnQgZnVuY3Rpb24gTUFUX1NJTkdMRV9EQVRFX1NFTEVDVElPTl9NT0RFTF9GQUNUT1JZKFxuICAgIHBhcmVudDogTWF0U2luZ2xlRGF0ZVNlbGVjdGlvbk1vZGVsPHVua25vd24+LCBhZGFwdGVyOiBEYXRlQWRhcHRlcjx1bmtub3duPikge1xuICByZXR1cm4gcGFyZW50IHx8IG5ldyBNYXRTaW5nbGVEYXRlU2VsZWN0aW9uTW9kZWwoYWRhcHRlcik7XG59XG5cbi8qKiBVc2VkIHRvIHByb3ZpZGUgYSBzaW5nbGUgc2VsZWN0aW9uIG1vZGVsIHRvIGEgY29tcG9uZW50LiAqL1xuZXhwb3J0IGNvbnN0IE1BVF9TSU5HTEVfREFURV9TRUxFQ1RJT05fTU9ERUxfUFJPVklERVI6IEZhY3RvcnlQcm92aWRlciA9IHtcbiAgcHJvdmlkZTogTWF0RGF0ZVNlbGVjdGlvbk1vZGVsLFxuICBkZXBzOiBbW25ldyBPcHRpb25hbCgpLCBuZXcgU2tpcFNlbGYoKSwgTWF0RGF0ZVNlbGVjdGlvbk1vZGVsXSwgRGF0ZUFkYXB0ZXJdLFxuICB1c2VGYWN0b3J5OiBNQVRfU0lOR0xFX0RBVEVfU0VMRUNUSU9OX01PREVMX0ZBQ1RPUlksXG59O1xuXG5cbi8qKiBAZG9jcy1wcml2YXRlICovXG5leHBvcnQgZnVuY3Rpb24gTUFUX1JBTkdFX0RBVEVfU0VMRUNUSU9OX01PREVMX0ZBQ1RPUlkoXG4gICAgcGFyZW50OiBNYXRTaW5nbGVEYXRlU2VsZWN0aW9uTW9kZWw8dW5rbm93bj4sIGFkYXB0ZXI6IERhdGVBZGFwdGVyPHVua25vd24+KSB7XG4gIHJldHVybiBwYXJlbnQgfHwgbmV3IE1hdFJhbmdlRGF0ZVNlbGVjdGlvbk1vZGVsKGFkYXB0ZXIpO1xufVxuXG4vKiogVXNlZCB0byBwcm92aWRlIGEgcmFuZ2Ugc2VsZWN0aW9uIG1vZGVsIHRvIGEgY29tcG9uZW50LiAqL1xuZXhwb3J0IGNvbnN0IE1BVF9SQU5HRV9EQVRFX1NFTEVDVElPTl9NT0RFTF9QUk9WSURFUjogRmFjdG9yeVByb3ZpZGVyID0ge1xuICBwcm92aWRlOiBNYXREYXRlU2VsZWN0aW9uTW9kZWwsXG4gIGRlcHM6IFtbbmV3IE9wdGlvbmFsKCksIG5ldyBTa2lwU2VsZigpLCBNYXREYXRlU2VsZWN0aW9uTW9kZWxdLCBEYXRlQWRhcHRlcl0sXG4gIHVzZUZhY3Rvcnk6IE1BVF9SQU5HRV9EQVRFX1NFTEVDVElPTl9NT0RFTF9GQUNUT1JZLFxufTtcbiJdfQ==