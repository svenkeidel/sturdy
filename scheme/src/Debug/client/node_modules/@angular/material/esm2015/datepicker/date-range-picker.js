/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ChangeDetectionStrategy, Component, ViewEncapsulation } from '@angular/core';
import { MatDatepickerBase } from './datepicker-base';
import { MAT_RANGE_DATE_SELECTION_MODEL_PROVIDER } from './date-selection-model';
// TODO(mmalerba): We use a component instead of a directive here so the user can use implicit
// template reference variables (e.g. #d vs #d="matDateRangePicker"). We can change this to a
// directive if angular adds support for `exportAs: '$implicit'` on directives.
/** Component responsible for managing the date range picker popup/dialog. */
let MatDateRangePicker = /** @class */ (() => {
    class MatDateRangePicker extends MatDatepickerBase {
        _forwardContentValues(instance) {
            super._forwardContentValues(instance);
            const input = this._datepickerInput;
            if (input) {
                instance.comparisonStart = input.comparisonStart;
                instance.comparisonEnd = input.comparisonEnd;
            }
        }
    }
    MatDateRangePicker.decorators = [
        { type: Component, args: [{
                    selector: 'mat-date-range-picker',
                    template: '',
                    exportAs: 'matDateRangePicker',
                    changeDetection: ChangeDetectionStrategy.OnPush,
                    encapsulation: ViewEncapsulation.None,
                    providers: [MAT_RANGE_DATE_SELECTION_MODEL_PROVIDER]
                },] }
    ];
    return MatDateRangePicker;
})();
export { MatDateRangePicker };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGF0ZS1yYW5nZS1waWNrZXIuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvZGF0ZXBpY2tlci9kYXRlLXJhbmdlLXBpY2tlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsdUJBQXVCLEVBQUUsU0FBUyxFQUFFLGlCQUFpQixFQUFDLE1BQU0sZUFBZSxDQUFDO0FBQ3BGLE9BQU8sRUFBQyxpQkFBaUIsRUFBdUIsTUFBTSxtQkFBbUIsQ0FBQztBQUUxRSxPQUFPLEVBQUMsdUNBQXVDLEVBQVksTUFBTSx3QkFBd0IsQ0FBQztBQUUxRiw4RkFBOEY7QUFDOUYsNkZBQTZGO0FBQzdGLCtFQUErRTtBQUMvRSw2RUFBNkU7QUFDN0U7SUFBQSxNQVFhLGtCQUNYLFNBQVEsaUJBQXdEO1FBRXRELHFCQUFxQixDQUFDLFFBQStDO1lBQzdFLEtBQUssQ0FBQyxxQkFBcUIsQ0FBQyxRQUFRLENBQUMsQ0FBQztZQUV0QyxNQUFNLEtBQUssR0FBRyxJQUFJLENBQUMsZ0JBQWdCLENBQUM7WUFFcEMsSUFBSSxLQUFLLEVBQUU7Z0JBQ1QsUUFBUSxDQUFDLGVBQWUsR0FBRyxLQUFLLENBQUMsZUFBZSxDQUFDO2dCQUNqRCxRQUFRLENBQUMsYUFBYSxHQUFHLEtBQUssQ0FBQyxhQUFhLENBQUM7YUFDOUM7UUFDSCxDQUFDOzs7Z0JBcEJGLFNBQVMsU0FBQztvQkFDVCxRQUFRLEVBQUUsdUJBQXVCO29CQUNqQyxRQUFRLEVBQUUsRUFBRTtvQkFDWixRQUFRLEVBQUUsb0JBQW9CO29CQUM5QixlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTtvQkFDL0MsYUFBYSxFQUFFLGlCQUFpQixDQUFDLElBQUk7b0JBQ3JDLFNBQVMsRUFBRSxDQUFDLHVDQUF1QyxDQUFDO2lCQUNyRDs7SUFjRCx5QkFBQztLQUFBO1NBYlksa0JBQWtCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Q2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksIENvbXBvbmVudCwgVmlld0VuY2Fwc3VsYXRpb259IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtNYXREYXRlcGlja2VyQmFzZSwgTWF0RGF0ZXBpY2tlckNvbnRlbnR9IGZyb20gJy4vZGF0ZXBpY2tlci1iYXNlJztcbmltcG9ydCB7TWF0RGF0ZVJhbmdlSW5wdXR9IGZyb20gJy4vZGF0ZS1yYW5nZS1pbnB1dCc7XG5pbXBvcnQge01BVF9SQU5HRV9EQVRFX1NFTEVDVElPTl9NT0RFTF9QUk9WSURFUiwgRGF0ZVJhbmdlfSBmcm9tICcuL2RhdGUtc2VsZWN0aW9uLW1vZGVsJztcblxuLy8gVE9ETyhtbWFsZXJiYSk6IFdlIHVzZSBhIGNvbXBvbmVudCBpbnN0ZWFkIG9mIGEgZGlyZWN0aXZlIGhlcmUgc28gdGhlIHVzZXIgY2FuIHVzZSBpbXBsaWNpdFxuLy8gdGVtcGxhdGUgcmVmZXJlbmNlIHZhcmlhYmxlcyAoZS5nLiAjZCB2cyAjZD1cIm1hdERhdGVSYW5nZVBpY2tlclwiKS4gV2UgY2FuIGNoYW5nZSB0aGlzIHRvIGFcbi8vIGRpcmVjdGl2ZSBpZiBhbmd1bGFyIGFkZHMgc3VwcG9ydCBmb3IgYGV4cG9ydEFzOiAnJGltcGxpY2l0J2Agb24gZGlyZWN0aXZlcy5cbi8qKiBDb21wb25lbnQgcmVzcG9uc2libGUgZm9yIG1hbmFnaW5nIHRoZSBkYXRlIHJhbmdlIHBpY2tlciBwb3B1cC9kaWFsb2cuICovXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdtYXQtZGF0ZS1yYW5nZS1waWNrZXInLFxuICB0ZW1wbGF0ZTogJycsXG4gIGV4cG9ydEFzOiAnbWF0RGF0ZVJhbmdlUGlja2VyJyxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2gsXG4gIGVuY2Fwc3VsYXRpb246IFZpZXdFbmNhcHN1bGF0aW9uLk5vbmUsXG4gIHByb3ZpZGVyczogW01BVF9SQU5HRV9EQVRFX1NFTEVDVElPTl9NT0RFTF9QUk9WSURFUl1cbn0pXG5leHBvcnQgY2xhc3MgTWF0RGF0ZVJhbmdlUGlja2VyPEQ+XG4gIGV4dGVuZHMgTWF0RGF0ZXBpY2tlckJhc2U8TWF0RGF0ZVJhbmdlSW5wdXQ8RD4sIERhdGVSYW5nZTxEPiwgRD4ge1xuXG4gIHByb3RlY3RlZCBfZm9yd2FyZENvbnRlbnRWYWx1ZXMoaW5zdGFuY2U6IE1hdERhdGVwaWNrZXJDb250ZW50PERhdGVSYW5nZTxEPiwgRD4pIHtcbiAgICBzdXBlci5fZm9yd2FyZENvbnRlbnRWYWx1ZXMoaW5zdGFuY2UpO1xuXG4gICAgY29uc3QgaW5wdXQgPSB0aGlzLl9kYXRlcGlja2VySW5wdXQ7XG5cbiAgICBpZiAoaW5wdXQpIHtcbiAgICAgIGluc3RhbmNlLmNvbXBhcmlzb25TdGFydCA9IGlucHV0LmNvbXBhcmlzb25TdGFydDtcbiAgICAgIGluc3RhbmNlLmNvbXBhcmlzb25FbmQgPSBpbnB1dC5jb21wYXJpc29uRW5kO1xuICAgIH1cbiAgfVxufVxuIl19