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
let PieChartModule = class PieChartModule {
};
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
export { PieChartModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGllLWNoYXJ0Lm1vZHVsZS5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL3BpZS1jaGFydC9waWUtY2hhcnQubW9kdWxlLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQUUsUUFBUSxFQUFFLE1BQU0sZUFBZSxDQUFDO0FBQ3pDLE9BQU8sRUFBRSxpQkFBaUIsRUFBRSxNQUFNLCtCQUErQixDQUFDO0FBQ2xFLE9BQU8sRUFBRSx5QkFBeUIsRUFBRSxNQUFNLGdDQUFnQyxDQUFDO0FBQzNFLE9BQU8sRUFBRSxpQkFBaUIsRUFBRSxNQUFNLHVCQUF1QixDQUFDO0FBQzFELE9BQU8sRUFBRSxlQUFlLEVBQUUsTUFBTSxxQkFBcUIsQ0FBQztBQUN0RCxPQUFPLEVBQUUsaUJBQWlCLEVBQUUsTUFBTSx1QkFBdUIsQ0FBQztBQUMxRCxPQUFPLEVBQUUsZ0JBQWdCLEVBQUUsTUFBTSxzQkFBc0IsQ0FBQztBQUN4RCxPQUFPLEVBQUUsc0JBQXNCLEVBQUUsTUFBTSw2QkFBNkIsQ0FBQztBQUNyRSxPQUFPLEVBQUUsa0JBQWtCLEVBQUUsTUFBTSx3QkFBd0IsQ0FBQztBQXVCNUQsSUFBYSxjQUFjLEdBQTNCLE1BQWEsY0FBYztDQUFHLENBQUE7QUFBakIsY0FBYztJQXJCMUIsUUFBUSxDQUFDO1FBQ1IsT0FBTyxFQUFFLENBQUMsaUJBQWlCLENBQUM7UUFDNUIsWUFBWSxFQUFFO1lBQ1oseUJBQXlCO1lBQ3pCLGlCQUFpQjtZQUNqQixlQUFlO1lBQ2YsaUJBQWlCO1lBQ2pCLGdCQUFnQjtZQUNoQixzQkFBc0I7WUFDdEIsa0JBQWtCO1NBQ25CO1FBQ0QsT0FBTyxFQUFFO1lBQ1AseUJBQXlCO1lBQ3pCLGlCQUFpQjtZQUNqQixlQUFlO1lBQ2YsaUJBQWlCO1lBQ2pCLGdCQUFnQjtZQUNoQixzQkFBc0I7WUFDdEIsa0JBQWtCO1NBQ25CO0tBQ0YsQ0FBQztHQUNXLGNBQWMsQ0FBRztTQUFqQixjQUFjIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgTmdNb2R1bGUgfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IENoYXJ0Q29tbW9uTW9kdWxlIH0gZnJvbSAnLi4vY29tbW9uL2NoYXJ0LWNvbW1vbi5tb2R1bGUnO1xuaW1wb3J0IHsgQWR2YW5jZWRQaWVDaGFydENvbXBvbmVudCB9IGZyb20gJy4vYWR2YW5jZWQtcGllLWNoYXJ0LmNvbXBvbmVudCc7XG5pbXBvcnQgeyBQaWVMYWJlbENvbXBvbmVudCB9IGZyb20gJy4vcGllLWxhYmVsLmNvbXBvbmVudCc7XG5pbXBvcnQgeyBQaWVBcmNDb21wb25lbnQgfSBmcm9tICcuL3BpZS1hcmMuY29tcG9uZW50JztcbmltcG9ydCB7IFBpZUNoYXJ0Q29tcG9uZW50IH0gZnJvbSAnLi9waWUtY2hhcnQuY29tcG9uZW50JztcbmltcG9ydCB7IFBpZUdyaWRDb21wb25lbnQgfSBmcm9tICcuL3BpZS1ncmlkLmNvbXBvbmVudCc7XG5pbXBvcnQgeyBQaWVHcmlkU2VyaWVzQ29tcG9uZW50IH0gZnJvbSAnLi9waWUtZ3JpZC1zZXJpZXMuY29tcG9uZW50JztcbmltcG9ydCB7IFBpZVNlcmllc0NvbXBvbmVudCB9IGZyb20gJy4vcGllLXNlcmllcy5jb21wb25lbnQnO1xuXG5ATmdNb2R1bGUoe1xuICBpbXBvcnRzOiBbQ2hhcnRDb21tb25Nb2R1bGVdLFxuICBkZWNsYXJhdGlvbnM6IFtcbiAgICBBZHZhbmNlZFBpZUNoYXJ0Q29tcG9uZW50LFxuICAgIFBpZUxhYmVsQ29tcG9uZW50LFxuICAgIFBpZUFyY0NvbXBvbmVudCxcbiAgICBQaWVDaGFydENvbXBvbmVudCxcbiAgICBQaWVHcmlkQ29tcG9uZW50LFxuICAgIFBpZUdyaWRTZXJpZXNDb21wb25lbnQsXG4gICAgUGllU2VyaWVzQ29tcG9uZW50XG4gIF0sXG4gIGV4cG9ydHM6IFtcbiAgICBBZHZhbmNlZFBpZUNoYXJ0Q29tcG9uZW50LFxuICAgIFBpZUxhYmVsQ29tcG9uZW50LFxuICAgIFBpZUFyY0NvbXBvbmVudCxcbiAgICBQaWVDaGFydENvbXBvbmVudCxcbiAgICBQaWVHcmlkQ29tcG9uZW50LFxuICAgIFBpZUdyaWRTZXJpZXNDb21wb25lbnQsXG4gICAgUGllU2VyaWVzQ29tcG9uZW50XG4gIF1cbn0pXG5leHBvcnQgY2xhc3MgUGllQ2hhcnRNb2R1bGUge31cbiJdfQ==