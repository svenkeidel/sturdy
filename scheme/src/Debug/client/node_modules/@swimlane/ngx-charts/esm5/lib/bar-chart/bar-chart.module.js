import { __decorate } from "tslib";
import { NgModule } from '@angular/core';
import { ChartCommonModule } from '../common/chart-common.module';
import { BarComponent } from './bar.component';
import { BarHorizontalComponent } from './bar-horizontal.component';
import { BarHorizontal2DComponent } from './bar-horizontal-2d.component';
import { BarHorizontalNormalizedComponent } from './bar-horizontal-normalized.component';
import { BarHorizontalStackedComponent } from './bar-horizontal-stacked.component';
import { BarVerticalComponent } from './bar-vertical.component';
import { BarVertical2DComponent } from './bar-vertical-2d.component';
import { BarVerticalNormalizedComponent } from './bar-vertical-normalized.component';
import { BarVerticalStackedComponent } from './bar-vertical-stacked.component';
import { SeriesHorizontal } from './series-horizontal.component';
import { SeriesVerticalComponent } from './series-vertical.component';
import { BarLabelComponent } from './bar-label.component';
var BarChartModule = /** @class */ (function () {
    function BarChartModule() {
    }
    BarChartModule = __decorate([
        NgModule({
            imports: [ChartCommonModule],
            declarations: [
                BarComponent,
                BarHorizontalComponent,
                BarHorizontal2DComponent,
                BarHorizontalNormalizedComponent,
                BarHorizontalStackedComponent,
                BarVerticalComponent,
                BarVertical2DComponent,
                BarVerticalNormalizedComponent,
                BarVerticalStackedComponent,
                BarLabelComponent,
                SeriesHorizontal,
                SeriesVerticalComponent
            ],
            exports: [
                BarComponent,
                BarHorizontalComponent,
                BarHorizontal2DComponent,
                BarHorizontalNormalizedComponent,
                BarHorizontalStackedComponent,
                BarVerticalComponent,
                BarVertical2DComponent,
                BarVerticalNormalizedComponent,
                BarVerticalStackedComponent,
                BarLabelComponent,
                SeriesHorizontal,
                SeriesVerticalComponent
            ]
        })
    ], BarChartModule);
    return BarChartModule;
}());
export { BarChartModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYmFyLWNoYXJ0Lm1vZHVsZS5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2Jhci1jaGFydC9iYXItY2hhcnQubW9kdWxlLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQUUsUUFBUSxFQUFFLE1BQU0sZUFBZSxDQUFDO0FBQ3pDLE9BQU8sRUFBRSxpQkFBaUIsRUFBRSxNQUFNLCtCQUErQixDQUFDO0FBQ2xFLE9BQU8sRUFBRSxZQUFZLEVBQUUsTUFBTSxpQkFBaUIsQ0FBQztBQUMvQyxPQUFPLEVBQUUsc0JBQXNCLEVBQUUsTUFBTSw0QkFBNEIsQ0FBQztBQUNwRSxPQUFPLEVBQUUsd0JBQXdCLEVBQUUsTUFBTSwrQkFBK0IsQ0FBQztBQUN6RSxPQUFPLEVBQUUsZ0NBQWdDLEVBQUUsTUFBTSx1Q0FBdUMsQ0FBQztBQUN6RixPQUFPLEVBQUUsNkJBQTZCLEVBQUUsTUFBTSxvQ0FBb0MsQ0FBQztBQUNuRixPQUFPLEVBQUUsb0JBQW9CLEVBQUUsTUFBTSwwQkFBMEIsQ0FBQztBQUNoRSxPQUFPLEVBQUUsc0JBQXNCLEVBQUUsTUFBTSw2QkFBNkIsQ0FBQztBQUNyRSxPQUFPLEVBQUUsOEJBQThCLEVBQUUsTUFBTSxxQ0FBcUMsQ0FBQztBQUNyRixPQUFPLEVBQUUsMkJBQTJCLEVBQUUsTUFBTSxrQ0FBa0MsQ0FBQztBQUMvRSxPQUFPLEVBQUUsZ0JBQWdCLEVBQUUsTUFBTSwrQkFBK0IsQ0FBQztBQUNqRSxPQUFPLEVBQUUsdUJBQXVCLEVBQUUsTUFBTSw2QkFBNkIsQ0FBQztBQUN0RSxPQUFPLEVBQUUsaUJBQWlCLEVBQUUsTUFBTSx1QkFBdUIsQ0FBQztBQWlDMUQ7SUFBQTtJQUE2QixDQUFDO0lBQWpCLGNBQWM7UUEvQjFCLFFBQVEsQ0FBQztZQUNSLE9BQU8sRUFBRSxDQUFDLGlCQUFpQixDQUFDO1lBQzVCLFlBQVksRUFBRTtnQkFDWixZQUFZO2dCQUNaLHNCQUFzQjtnQkFDdEIsd0JBQXdCO2dCQUN4QixnQ0FBZ0M7Z0JBQ2hDLDZCQUE2QjtnQkFDN0Isb0JBQW9CO2dCQUNwQixzQkFBc0I7Z0JBQ3RCLDhCQUE4QjtnQkFDOUIsMkJBQTJCO2dCQUMzQixpQkFBaUI7Z0JBQ2pCLGdCQUFnQjtnQkFDaEIsdUJBQXVCO2FBQ3hCO1lBQ0QsT0FBTyxFQUFFO2dCQUNQLFlBQVk7Z0JBQ1osc0JBQXNCO2dCQUN0Qix3QkFBd0I7Z0JBQ3hCLGdDQUFnQztnQkFDaEMsNkJBQTZCO2dCQUM3QixvQkFBb0I7Z0JBQ3BCLHNCQUFzQjtnQkFDdEIsOEJBQThCO2dCQUM5QiwyQkFBMkI7Z0JBQzNCLGlCQUFpQjtnQkFDakIsZ0JBQWdCO2dCQUNoQix1QkFBdUI7YUFDeEI7U0FDRixDQUFDO09BQ1csY0FBYyxDQUFHO0lBQUQscUJBQUM7Q0FBQSxBQUE5QixJQUE4QjtTQUFqQixjQUFjIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgTmdNb2R1bGUgfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IENoYXJ0Q29tbW9uTW9kdWxlIH0gZnJvbSAnLi4vY29tbW9uL2NoYXJ0LWNvbW1vbi5tb2R1bGUnO1xuaW1wb3J0IHsgQmFyQ29tcG9uZW50IH0gZnJvbSAnLi9iYXIuY29tcG9uZW50JztcbmltcG9ydCB7IEJhckhvcml6b250YWxDb21wb25lbnQgfSBmcm9tICcuL2Jhci1ob3Jpem9udGFsLmNvbXBvbmVudCc7XG5pbXBvcnQgeyBCYXJIb3Jpem9udGFsMkRDb21wb25lbnQgfSBmcm9tICcuL2Jhci1ob3Jpem9udGFsLTJkLmNvbXBvbmVudCc7XG5pbXBvcnQgeyBCYXJIb3Jpem9udGFsTm9ybWFsaXplZENvbXBvbmVudCB9IGZyb20gJy4vYmFyLWhvcml6b250YWwtbm9ybWFsaXplZC5jb21wb25lbnQnO1xuaW1wb3J0IHsgQmFySG9yaXpvbnRhbFN0YWNrZWRDb21wb25lbnQgfSBmcm9tICcuL2Jhci1ob3Jpem9udGFsLXN0YWNrZWQuY29tcG9uZW50JztcbmltcG9ydCB7IEJhclZlcnRpY2FsQ29tcG9uZW50IH0gZnJvbSAnLi9iYXItdmVydGljYWwuY29tcG9uZW50JztcbmltcG9ydCB7IEJhclZlcnRpY2FsMkRDb21wb25lbnQgfSBmcm9tICcuL2Jhci12ZXJ0aWNhbC0yZC5jb21wb25lbnQnO1xuaW1wb3J0IHsgQmFyVmVydGljYWxOb3JtYWxpemVkQ29tcG9uZW50IH0gZnJvbSAnLi9iYXItdmVydGljYWwtbm9ybWFsaXplZC5jb21wb25lbnQnO1xuaW1wb3J0IHsgQmFyVmVydGljYWxTdGFja2VkQ29tcG9uZW50IH0gZnJvbSAnLi9iYXItdmVydGljYWwtc3RhY2tlZC5jb21wb25lbnQnO1xuaW1wb3J0IHsgU2VyaWVzSG9yaXpvbnRhbCB9IGZyb20gJy4vc2VyaWVzLWhvcml6b250YWwuY29tcG9uZW50JztcbmltcG9ydCB7IFNlcmllc1ZlcnRpY2FsQ29tcG9uZW50IH0gZnJvbSAnLi9zZXJpZXMtdmVydGljYWwuY29tcG9uZW50JztcbmltcG9ydCB7IEJhckxhYmVsQ29tcG9uZW50IH0gZnJvbSAnLi9iYXItbGFiZWwuY29tcG9uZW50JztcblxuQE5nTW9kdWxlKHtcbiAgaW1wb3J0czogW0NoYXJ0Q29tbW9uTW9kdWxlXSxcbiAgZGVjbGFyYXRpb25zOiBbXG4gICAgQmFyQ29tcG9uZW50LFxuICAgIEJhckhvcml6b250YWxDb21wb25lbnQsXG4gICAgQmFySG9yaXpvbnRhbDJEQ29tcG9uZW50LFxuICAgIEJhckhvcml6b250YWxOb3JtYWxpemVkQ29tcG9uZW50LFxuICAgIEJhckhvcml6b250YWxTdGFja2VkQ29tcG9uZW50LFxuICAgIEJhclZlcnRpY2FsQ29tcG9uZW50LFxuICAgIEJhclZlcnRpY2FsMkRDb21wb25lbnQsXG4gICAgQmFyVmVydGljYWxOb3JtYWxpemVkQ29tcG9uZW50LFxuICAgIEJhclZlcnRpY2FsU3RhY2tlZENvbXBvbmVudCxcbiAgICBCYXJMYWJlbENvbXBvbmVudCxcbiAgICBTZXJpZXNIb3Jpem9udGFsLFxuICAgIFNlcmllc1ZlcnRpY2FsQ29tcG9uZW50XG4gIF0sXG4gIGV4cG9ydHM6IFtcbiAgICBCYXJDb21wb25lbnQsXG4gICAgQmFySG9yaXpvbnRhbENvbXBvbmVudCxcbiAgICBCYXJIb3Jpem9udGFsMkRDb21wb25lbnQsXG4gICAgQmFySG9yaXpvbnRhbE5vcm1hbGl6ZWRDb21wb25lbnQsXG4gICAgQmFySG9yaXpvbnRhbFN0YWNrZWRDb21wb25lbnQsXG4gICAgQmFyVmVydGljYWxDb21wb25lbnQsXG4gICAgQmFyVmVydGljYWwyRENvbXBvbmVudCxcbiAgICBCYXJWZXJ0aWNhbE5vcm1hbGl6ZWRDb21wb25lbnQsXG4gICAgQmFyVmVydGljYWxTdGFja2VkQ29tcG9uZW50LFxuICAgIEJhckxhYmVsQ29tcG9uZW50LFxuICAgIFNlcmllc0hvcml6b250YWwsXG4gICAgU2VyaWVzVmVydGljYWxDb21wb25lbnRcbiAgXVxufSlcbmV4cG9ydCBjbGFzcyBCYXJDaGFydE1vZHVsZSB7fVxuIl19