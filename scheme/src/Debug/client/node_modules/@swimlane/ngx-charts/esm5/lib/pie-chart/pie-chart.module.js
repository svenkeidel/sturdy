import { __decorate } from "tslib";
import { NgModule } from '@angular/core';
import { ChartCommonModule } from '../common/chart-common.module';
import { AdvancedPieChartComponent } from './advanced-pie-chart.component';
import { PieLabelComponent } from './pie-label.component';
import { PieArcComponent } from './pie-arc.component';
import { PieChartComponent } from './pie-chart.component';
import { PieGridComponent } from './pie-grid.component';
import { PieGridSeriesComponent } from './pie-grid-series.component';
import { PieSeriesComponent } from './pie-series.component';
var PieChartModule = /** @class */ (function () {
    function PieChartModule() {
    }
    PieChartModule = __decorate([
        NgModule({
            imports: [ChartCommonModule],
            declarations: [
                AdvancedPieChartComponent,
                PieLabelComponent,
                PieArcComponent,
                PieChartComponent,
                PieGridComponent,
                PieGridSeriesComponent,
                PieSeriesComponent
            ],
            exports: [
                AdvancedPieChartComponent,
                PieLabelComponent,
                PieArcComponent,
                PieChartComponent,
                PieGridComponent,
                PieGridSeriesComponent,
                PieSeriesComponent
            ]
        })
    ], PieChartModule);
    return PieChartModule;
}());
export { PieChartModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGllLWNoYXJ0Lm1vZHVsZS5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL3BpZS1jaGFydC9waWUtY2hhcnQubW9kdWxlLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQUUsUUFBUSxFQUFFLE1BQU0sZUFBZSxDQUFDO0FBQ3pDLE9BQU8sRUFBRSxpQkFBaUIsRUFBRSxNQUFNLCtCQUErQixDQUFDO0FBQ2xFLE9BQU8sRUFBRSx5QkFBeUIsRUFBRSxNQUFNLGdDQUFnQyxDQUFDO0FBQzNFLE9BQU8sRUFBRSxpQkFBaUIsRUFBRSxNQUFNLHVCQUF1QixDQUFDO0FBQzFELE9BQU8sRUFBRSxlQUFlLEVBQUUsTUFBTSxxQkFBcUIsQ0FBQztBQUN0RCxPQUFPLEVBQUUsaUJBQWlCLEVBQUUsTUFBTSx1QkFBdUIsQ0FBQztBQUMxRCxPQUFPLEVBQUUsZ0JBQWdCLEVBQUUsTUFBTSxzQkFBc0IsQ0FBQztBQUN4RCxPQUFPLEVBQUUsc0JBQXNCLEVBQUUsTUFBTSw2QkFBNkIsQ0FBQztBQUNyRSxPQUFPLEVBQUUsa0JBQWtCLEVBQUUsTUFBTSx3QkFBd0IsQ0FBQztBQXVCNUQ7SUFBQTtJQUE2QixDQUFDO0lBQWpCLGNBQWM7UUFyQjFCLFFBQVEsQ0FBQztZQUNSLE9BQU8sRUFBRSxDQUFDLGlCQUFpQixDQUFDO1lBQzVCLFlBQVksRUFBRTtnQkFDWix5QkFBeUI7Z0JBQ3pCLGlCQUFpQjtnQkFDakIsZUFBZTtnQkFDZixpQkFBaUI7Z0JBQ2pCLGdCQUFnQjtnQkFDaEIsc0JBQXNCO2dCQUN0QixrQkFBa0I7YUFDbkI7WUFDRCxPQUFPLEVBQUU7Z0JBQ1AseUJBQXlCO2dCQUN6QixpQkFBaUI7Z0JBQ2pCLGVBQWU7Z0JBQ2YsaUJBQWlCO2dCQUNqQixnQkFBZ0I7Z0JBQ2hCLHNCQUFzQjtnQkFDdEIsa0JBQWtCO2FBQ25CO1NBQ0YsQ0FBQztPQUNXLGNBQWMsQ0FBRztJQUFELHFCQUFDO0NBQUEsQUFBOUIsSUFBOEI7U0FBakIsY0FBYyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IE5nTW9kdWxlIH0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBDaGFydENvbW1vbk1vZHVsZSB9IGZyb20gJy4uL2NvbW1vbi9jaGFydC1jb21tb24ubW9kdWxlJztcbmltcG9ydCB7IEFkdmFuY2VkUGllQ2hhcnRDb21wb25lbnQgfSBmcm9tICcuL2FkdmFuY2VkLXBpZS1jaGFydC5jb21wb25lbnQnO1xuaW1wb3J0IHsgUGllTGFiZWxDb21wb25lbnQgfSBmcm9tICcuL3BpZS1sYWJlbC5jb21wb25lbnQnO1xuaW1wb3J0IHsgUGllQXJjQ29tcG9uZW50IH0gZnJvbSAnLi9waWUtYXJjLmNvbXBvbmVudCc7XG5pbXBvcnQgeyBQaWVDaGFydENvbXBvbmVudCB9IGZyb20gJy4vcGllLWNoYXJ0LmNvbXBvbmVudCc7XG5pbXBvcnQgeyBQaWVHcmlkQ29tcG9uZW50IH0gZnJvbSAnLi9waWUtZ3JpZC5jb21wb25lbnQnO1xuaW1wb3J0IHsgUGllR3JpZFNlcmllc0NvbXBvbmVudCB9IGZyb20gJy4vcGllLWdyaWQtc2VyaWVzLmNvbXBvbmVudCc7XG5pbXBvcnQgeyBQaWVTZXJpZXNDb21wb25lbnQgfSBmcm9tICcuL3BpZS1zZXJpZXMuY29tcG9uZW50JztcblxuQE5nTW9kdWxlKHtcbiAgaW1wb3J0czogW0NoYXJ0Q29tbW9uTW9kdWxlXSxcbiAgZGVjbGFyYXRpb25zOiBbXG4gICAgQWR2YW5jZWRQaWVDaGFydENvbXBvbmVudCxcbiAgICBQaWVMYWJlbENvbXBvbmVudCxcbiAgICBQaWVBcmNDb21wb25lbnQsXG4gICAgUGllQ2hhcnRDb21wb25lbnQsXG4gICAgUGllR3JpZENvbXBvbmVudCxcbiAgICBQaWVHcmlkU2VyaWVzQ29tcG9uZW50LFxuICAgIFBpZVNlcmllc0NvbXBvbmVudFxuICBdLFxuICBleHBvcnRzOiBbXG4gICAgQWR2YW5jZWRQaWVDaGFydENvbXBvbmVudCxcbiAgICBQaWVMYWJlbENvbXBvbmVudCxcbiAgICBQaWVBcmNDb21wb25lbnQsXG4gICAgUGllQ2hhcnRDb21wb25lbnQsXG4gICAgUGllR3JpZENvbXBvbmVudCxcbiAgICBQaWVHcmlkU2VyaWVzQ29tcG9uZW50LFxuICAgIFBpZVNlcmllc0NvbXBvbmVudFxuICBdXG59KVxuZXhwb3J0IGNsYXNzIFBpZUNoYXJ0TW9kdWxlIHt9XG4iXX0=