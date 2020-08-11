import { __decorate } from "tslib";
import { NgModule } from '@angular/core';
import { ChartCommonModule } from './common/chart-common.module';
import { AreaChartModule } from './area-chart/area-chart.module';
import { BarChartModule } from './bar-chart/bar-chart.module';
import { BubbleChartModule } from './bubble-chart/bubble-chart.module';
import { HeatMapModule } from './heat-map/heat-map.module';
import { LineChartModule } from './line-chart/line-chart.module';
import { PolarChartModule } from './polar-chart/polar-chart.module';
import { NumberCardModule } from './number-card/number-card.module';
import { PieChartModule } from './pie-chart/pie-chart.module';
import { TreeMapModule } from './tree-map/tree-map.module';
import { GaugeModule } from './gauge/gauge.module';
import { ngxChartsPolyfills } from './polyfills';
let NgxChartsModule = class NgxChartsModule {
    constructor() {
        ngxChartsPolyfills();
    }
};
NgxChartsModule = __decorate([
    NgModule({
        exports: [
            ChartCommonModule,
            AreaChartModule,
            BarChartModule,
            BubbleChartModule,
            HeatMapModule,
            LineChartModule,
            PolarChartModule,
            NumberCardModule,
            PieChartModule,
            TreeMapModule,
            GaugeModule
        ]
    })
], NgxChartsModule);
export { NgxChartsModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibmd4LWNoYXJ0cy5tb2R1bGUuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9uZ3gtY2hhcnRzLm1vZHVsZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUFFLFFBQVEsRUFBRSxNQUFNLGVBQWUsQ0FBQztBQUN6QyxPQUFPLEVBQUUsaUJBQWlCLEVBQUUsTUFBTSw4QkFBOEIsQ0FBQztBQUNqRSxPQUFPLEVBQUUsZUFBZSxFQUFFLE1BQU0sZ0NBQWdDLENBQUM7QUFDakUsT0FBTyxFQUFFLGNBQWMsRUFBRSxNQUFNLDhCQUE4QixDQUFDO0FBQzlELE9BQU8sRUFBRSxpQkFBaUIsRUFBRSxNQUFNLG9DQUFvQyxDQUFDO0FBQ3ZFLE9BQU8sRUFBRSxhQUFhLEVBQUUsTUFBTSw0QkFBNEIsQ0FBQztBQUMzRCxPQUFPLEVBQUUsZUFBZSxFQUFFLE1BQU0sZ0NBQWdDLENBQUM7QUFDakUsT0FBTyxFQUFFLGdCQUFnQixFQUFFLE1BQU0sa0NBQWtDLENBQUM7QUFDcEUsT0FBTyxFQUFFLGdCQUFnQixFQUFFLE1BQU0sa0NBQWtDLENBQUM7QUFDcEUsT0FBTyxFQUFFLGNBQWMsRUFBRSxNQUFNLDhCQUE4QixDQUFDO0FBQzlELE9BQU8sRUFBRSxhQUFhLEVBQUUsTUFBTSw0QkFBNEIsQ0FBQztBQUMzRCxPQUFPLEVBQUUsV0FBVyxFQUFFLE1BQU0sc0JBQXNCLENBQUM7QUFDbkQsT0FBTyxFQUFFLGtCQUFrQixFQUFFLE1BQU0sYUFBYSxDQUFDO0FBaUJqRCxJQUFhLGVBQWUsR0FBNUIsTUFBYSxlQUFlO0lBQzFCO1FBQ0Usa0JBQWtCLEVBQUUsQ0FBQztJQUN2QixDQUFDO0NBQ0YsQ0FBQTtBQUpZLGVBQWU7SUFmM0IsUUFBUSxDQUFDO1FBQ1IsT0FBTyxFQUFFO1lBQ1AsaUJBQWlCO1lBQ2pCLGVBQWU7WUFDZixjQUFjO1lBQ2QsaUJBQWlCO1lBQ2pCLGFBQWE7WUFDYixlQUFlO1lBQ2YsZ0JBQWdCO1lBQ2hCLGdCQUFnQjtZQUNoQixjQUFjO1lBQ2QsYUFBYTtZQUNiLFdBQVc7U0FDWjtLQUNGLENBQUM7R0FDVyxlQUFlLENBSTNCO1NBSlksZUFBZSIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IE5nTW9kdWxlIH0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBDaGFydENvbW1vbk1vZHVsZSB9IGZyb20gJy4vY29tbW9uL2NoYXJ0LWNvbW1vbi5tb2R1bGUnO1xuaW1wb3J0IHsgQXJlYUNoYXJ0TW9kdWxlIH0gZnJvbSAnLi9hcmVhLWNoYXJ0L2FyZWEtY2hhcnQubW9kdWxlJztcbmltcG9ydCB7IEJhckNoYXJ0TW9kdWxlIH0gZnJvbSAnLi9iYXItY2hhcnQvYmFyLWNoYXJ0Lm1vZHVsZSc7XG5pbXBvcnQgeyBCdWJibGVDaGFydE1vZHVsZSB9IGZyb20gJy4vYnViYmxlLWNoYXJ0L2J1YmJsZS1jaGFydC5tb2R1bGUnO1xuaW1wb3J0IHsgSGVhdE1hcE1vZHVsZSB9IGZyb20gJy4vaGVhdC1tYXAvaGVhdC1tYXAubW9kdWxlJztcbmltcG9ydCB7IExpbmVDaGFydE1vZHVsZSB9IGZyb20gJy4vbGluZS1jaGFydC9saW5lLWNoYXJ0Lm1vZHVsZSc7XG5pbXBvcnQgeyBQb2xhckNoYXJ0TW9kdWxlIH0gZnJvbSAnLi9wb2xhci1jaGFydC9wb2xhci1jaGFydC5tb2R1bGUnO1xuaW1wb3J0IHsgTnVtYmVyQ2FyZE1vZHVsZSB9IGZyb20gJy4vbnVtYmVyLWNhcmQvbnVtYmVyLWNhcmQubW9kdWxlJztcbmltcG9ydCB7IFBpZUNoYXJ0TW9kdWxlIH0gZnJvbSAnLi9waWUtY2hhcnQvcGllLWNoYXJ0Lm1vZHVsZSc7XG5pbXBvcnQgeyBUcmVlTWFwTW9kdWxlIH0gZnJvbSAnLi90cmVlLW1hcC90cmVlLW1hcC5tb2R1bGUnO1xuaW1wb3J0IHsgR2F1Z2VNb2R1bGUgfSBmcm9tICcuL2dhdWdlL2dhdWdlLm1vZHVsZSc7XG5pbXBvcnQgeyBuZ3hDaGFydHNQb2x5ZmlsbHMgfSBmcm9tICcuL3BvbHlmaWxscyc7XG5cbkBOZ01vZHVsZSh7XG4gIGV4cG9ydHM6IFtcbiAgICBDaGFydENvbW1vbk1vZHVsZSxcbiAgICBBcmVhQ2hhcnRNb2R1bGUsXG4gICAgQmFyQ2hhcnRNb2R1bGUsXG4gICAgQnViYmxlQ2hhcnRNb2R1bGUsXG4gICAgSGVhdE1hcE1vZHVsZSxcbiAgICBMaW5lQ2hhcnRNb2R1bGUsXG4gICAgUG9sYXJDaGFydE1vZHVsZSxcbiAgICBOdW1iZXJDYXJkTW9kdWxlLFxuICAgIFBpZUNoYXJ0TW9kdWxlLFxuICAgIFRyZWVNYXBNb2R1bGUsXG4gICAgR2F1Z2VNb2R1bGVcbiAgXVxufSlcbmV4cG9ydCBjbGFzcyBOZ3hDaGFydHNNb2R1bGUge1xuICBjb25zdHJ1Y3RvcigpIHtcbiAgICBuZ3hDaGFydHNQb2x5ZmlsbHMoKTtcbiAgfVxufVxuIl19