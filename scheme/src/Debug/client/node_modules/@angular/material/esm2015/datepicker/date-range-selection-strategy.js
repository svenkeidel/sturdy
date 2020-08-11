/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Injectable, InjectionToken } from '@angular/core';
import { DateAdapter } from '@angular/material/core';
import { DateRange } from './date-selection-model';
/** Injection token used to customize the date range selection behavior. */
export const MAT_DATE_RANGE_SELECTION_STRATEGY = new InjectionToken('MAT_DATE_RANGE_SELECTION_STRATEGY');
/** Provides the default date range selection behavior. */
let DefaultMatCalendarRangeStrategy = /** @class */ (() => {
    class DefaultMatCalendarRangeStrategy {
        constructor(_dateAdapter) {
            this._dateAdapter = _dateAdapter;
        }
        selectionFinished(date, currentRange) {
            let { start, end } = currentRange;
            if (start == null) {
                start = date;
            }
            else if (end == null && date && this._dateAdapter.compareDate(date, start) >= 0) {
                end = date;
            }
            else {
                start = date;
                end = null;
            }
            return new DateRange(start, end);
        }
        createPreview(activeDate, currentRange) {
            let start = null;
            let end = null;
            if (currentRange.start && !currentRange.end && activeDate) {
                start = currentRange.start;
                end = activeDate;
            }
            return new DateRange(start, end);
        }
    }
    DefaultMatCalendarRangeStrategy.decorators = [
        { type: Injectable }
    ];
    DefaultMatCalendarRangeStrategy.ctorParameters = () => [
        { type: DateAdapter }
    ];
    return DefaultMatCalendarRangeStrategy;
})();
export { DefaultMatCalendarRangeStrategy };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGF0ZS1yYW5nZS1zZWxlY3Rpb24tc3RyYXRlZ3kuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvZGF0ZXBpY2tlci9kYXRlLXJhbmdlLXNlbGVjdGlvbi1zdHJhdGVneS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsVUFBVSxFQUFFLGNBQWMsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUN6RCxPQUFPLEVBQUMsV0FBVyxFQUFDLE1BQU0sd0JBQXdCLENBQUM7QUFDbkQsT0FBTyxFQUFDLFNBQVMsRUFBQyxNQUFNLHdCQUF3QixDQUFDO0FBRWpELDJFQUEyRTtBQUMzRSxNQUFNLENBQUMsTUFBTSxpQ0FBaUMsR0FDMUMsSUFBSSxjQUFjLENBQXFDLG1DQUFtQyxDQUFDLENBQUM7QUEwQmhHLDBEQUEwRDtBQUMxRDtJQUFBLE1BQ2EsK0JBQStCO1FBQzFDLFlBQW9CLFlBQTRCO1lBQTVCLGlCQUFZLEdBQVosWUFBWSxDQUFnQjtRQUFHLENBQUM7UUFFcEQsaUJBQWlCLENBQUMsSUFBTyxFQUFFLFlBQTBCO1lBQ25ELElBQUksRUFBQyxLQUFLLEVBQUUsR0FBRyxFQUFDLEdBQUcsWUFBWSxDQUFDO1lBRWhDLElBQUksS0FBSyxJQUFJLElBQUksRUFBRTtnQkFDakIsS0FBSyxHQUFHLElBQUksQ0FBQzthQUNkO2lCQUFNLElBQUksR0FBRyxJQUFJLElBQUksSUFBSSxJQUFJLElBQUksSUFBSSxDQUFDLFlBQVksQ0FBQyxXQUFXLENBQUMsSUFBSSxFQUFFLEtBQUssQ0FBQyxJQUFJLENBQUMsRUFBRTtnQkFDakYsR0FBRyxHQUFHLElBQUksQ0FBQzthQUNaO2lCQUFNO2dCQUNMLEtBQUssR0FBRyxJQUFJLENBQUM7Z0JBQ2IsR0FBRyxHQUFHLElBQUksQ0FBQzthQUNaO1lBRUQsT0FBTyxJQUFJLFNBQVMsQ0FBSSxLQUFLLEVBQUUsR0FBRyxDQUFDLENBQUM7UUFDdEMsQ0FBQztRQUVELGFBQWEsQ0FBQyxVQUFvQixFQUFFLFlBQTBCO1lBQzVELElBQUksS0FBSyxHQUFhLElBQUksQ0FBQztZQUMzQixJQUFJLEdBQUcsR0FBYSxJQUFJLENBQUM7WUFFekIsSUFBSSxZQUFZLENBQUMsS0FBSyxJQUFJLENBQUMsWUFBWSxDQUFDLEdBQUcsSUFBSSxVQUFVLEVBQUU7Z0JBQ3pELEtBQUssR0FBRyxZQUFZLENBQUMsS0FBSyxDQUFDO2dCQUMzQixHQUFHLEdBQUcsVUFBVSxDQUFDO2FBQ2xCO1lBRUQsT0FBTyxJQUFJLFNBQVMsQ0FBSSxLQUFLLEVBQUUsR0FBRyxDQUFDLENBQUM7UUFDdEMsQ0FBQzs7O2dCQTdCRixVQUFVOzs7Z0JBaENILFdBQVc7O0lBOERuQixzQ0FBQztLQUFBO1NBN0JZLCtCQUErQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0luamVjdGFibGUsIEluamVjdGlvblRva2VufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7RGF0ZUFkYXB0ZXJ9IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL2NvcmUnO1xuaW1wb3J0IHtEYXRlUmFuZ2V9IGZyb20gJy4vZGF0ZS1zZWxlY3Rpb24tbW9kZWwnO1xuXG4vKiogSW5qZWN0aW9uIHRva2VuIHVzZWQgdG8gY3VzdG9taXplIHRoZSBkYXRlIHJhbmdlIHNlbGVjdGlvbiBiZWhhdmlvci4gKi9cbmV4cG9ydCBjb25zdCBNQVRfREFURV9SQU5HRV9TRUxFQ1RJT05fU1RSQVRFR1kgPVxuICAgIG5ldyBJbmplY3Rpb25Ub2tlbjxNYXREYXRlUmFuZ2VTZWxlY3Rpb25TdHJhdGVneTxhbnk+PignTUFUX0RBVEVfUkFOR0VfU0VMRUNUSU9OX1NUUkFURUdZJyk7XG5cbi8qKiBPYmplY3QgdGhhdCBjYW4gYmUgcHJvdmlkZWQgaW4gb3JkZXIgdG8gY3VzdG9taXplIHRoZSBkYXRlIHJhbmdlIHNlbGVjdGlvbiBiZWhhdmlvci4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgTWF0RGF0ZVJhbmdlU2VsZWN0aW9uU3RyYXRlZ3k8RD4ge1xuICAvKipcbiAgICogQ2FsbGVkIHdoZW4gdGhlIHVzZXIgaGFzIGZpbmlzaGVkIHNlbGVjdGluZyBhIHZhbHVlLlxuICAgKiBAcGFyYW0gZGF0ZSBEYXRlIHRoYXQgd2FzIHNlbGVjdGVkLiBXaWxsIGJlIG51bGwgaWYgdGhlIHVzZXIgY2xlYXJlZCB0aGUgc2VsZWN0aW9uLlxuICAgKiBAcGFyYW0gY3VycmVudFJhbmdlIFJhbmdlIHRoYXQgaXMgY3VycmVudGx5IHNob3cgaW4gdGhlIGNhbGVuZGFyLlxuICAgKiBAcGFyYW0gZXZlbnQgRE9NIGV2ZW50IHRoYXQgdHJpZ2dlcmVkIHRoZSBzZWxlY3Rpb24uIEN1cnJlbnRseSBvbmx5IGNvcnJlc3BvbmRzIHRvIGEgYGNsaWNrYFxuICAgKiAgICBldmVudCwgYnV0IGl0IG1heSBnZXQgZXhwYW5kZWQgaW4gdGhlIGZ1dHVyZS5cbiAgICovXG4gIHNlbGVjdGlvbkZpbmlzaGVkKGRhdGU6IEQgfCBudWxsLCBjdXJyZW50UmFuZ2U6IERhdGVSYW5nZTxEPiwgZXZlbnQ6IEV2ZW50KTogRGF0ZVJhbmdlPEQ+O1xuXG4gIC8qKlxuICAgKiBDYWxsZWQgd2hlbiB0aGUgdXNlciBoYXMgYWN0aXZhdGVkIGEgbmV3IGRhdGUgKGUuZy4gYnkgaG92ZXJpbmcgb3ZlclxuICAgKiBpdCBvciBtb3ZpbmcgZm9jdXMpIGFuZCB0aGUgY2FsZW5kYXIgdHJpZXMgdG8gZGlzcGxheSBhIGRhdGUgcmFuZ2UuXG4gICAqXG4gICAqIEBwYXJhbSBhY3RpdmVEYXRlIERhdGUgdGhhdCB0aGUgdXNlciBoYXMgYWN0aXZhdGVkLiBXaWxsIGJlIG51bGwgaWYgdGhlIHVzZXIgbW92ZWRcbiAgICogICAgZm9jdXMgdG8gYW4gZWxlbWVudCB0aGF0J3Mgbm8gYSBjYWxlbmRhciBjZWxsLlxuICAgKiBAcGFyYW0gY3VycmVudFJhbmdlIFJhbmdlIHRoYXQgaXMgY3VycmVudGx5IHNob3duIGluIHRoZSBjYWxlbmRhci5cbiAgICogQHBhcmFtIGV2ZW50IERPTSBldmVudCB0aGF0IGNhdXNlZCB0aGUgcHJldmlldyB0byBiZSBjaGFuZ2VkLiBXaWxsIGJlIGVpdGhlciBhXG4gICAqICAgIGBtb3VzZWVudGVyYC9gbW91c2VsZWF2ZWAgb3IgYGZvY3VzYC9gYmx1cmAgZGVwZW5kaW5nIG9uIGhvdyB0aGUgdXNlciBpcyBuYXZpZ2F0aW5nLlxuICAgKi9cbiAgY3JlYXRlUHJldmlldyhhY3RpdmVEYXRlOiBEIHwgbnVsbCwgY3VycmVudFJhbmdlOiBEYXRlUmFuZ2U8RD4sIGV2ZW50OiBFdmVudCk6IERhdGVSYW5nZTxEPjtcbn1cblxuLyoqIFByb3ZpZGVzIHRoZSBkZWZhdWx0IGRhdGUgcmFuZ2Ugc2VsZWN0aW9uIGJlaGF2aW9yLiAqL1xuQEluamVjdGFibGUoKVxuZXhwb3J0IGNsYXNzIERlZmF1bHRNYXRDYWxlbmRhclJhbmdlU3RyYXRlZ3k8RD4gaW1wbGVtZW50cyBNYXREYXRlUmFuZ2VTZWxlY3Rpb25TdHJhdGVneTxEPiB7XG4gIGNvbnN0cnVjdG9yKHByaXZhdGUgX2RhdGVBZGFwdGVyOiBEYXRlQWRhcHRlcjxEPikge31cblxuICBzZWxlY3Rpb25GaW5pc2hlZChkYXRlOiBELCBjdXJyZW50UmFuZ2U6IERhdGVSYW5nZTxEPikge1xuICAgIGxldCB7c3RhcnQsIGVuZH0gPSBjdXJyZW50UmFuZ2U7XG5cbiAgICBpZiAoc3RhcnQgPT0gbnVsbCkge1xuICAgICAgc3RhcnQgPSBkYXRlO1xuICAgIH0gZWxzZSBpZiAoZW5kID09IG51bGwgJiYgZGF0ZSAmJiB0aGlzLl9kYXRlQWRhcHRlci5jb21wYXJlRGF0ZShkYXRlLCBzdGFydCkgPj0gMCkge1xuICAgICAgZW5kID0gZGF0ZTtcbiAgICB9IGVsc2Uge1xuICAgICAgc3RhcnQgPSBkYXRlO1xuICAgICAgZW5kID0gbnVsbDtcbiAgICB9XG5cbiAgICByZXR1cm4gbmV3IERhdGVSYW5nZTxEPihzdGFydCwgZW5kKTtcbiAgfVxuXG4gIGNyZWF0ZVByZXZpZXcoYWN0aXZlRGF0ZTogRCB8IG51bGwsIGN1cnJlbnRSYW5nZTogRGF0ZVJhbmdlPEQ+KSB7XG4gICAgbGV0IHN0YXJ0OiBEIHwgbnVsbCA9IG51bGw7XG4gICAgbGV0IGVuZDogRCB8IG51bGwgPSBudWxsO1xuXG4gICAgaWYgKGN1cnJlbnRSYW5nZS5zdGFydCAmJiAhY3VycmVudFJhbmdlLmVuZCAmJiBhY3RpdmVEYXRlKSB7XG4gICAgICBzdGFydCA9IGN1cnJlbnRSYW5nZS5zdGFydDtcbiAgICAgIGVuZCA9IGFjdGl2ZURhdGU7XG4gICAgfVxuXG4gICAgcmV0dXJuIG5ldyBEYXRlUmFuZ2U8RD4oc3RhcnQsIGVuZCk7XG4gIH1cbn1cbiJdfQ==