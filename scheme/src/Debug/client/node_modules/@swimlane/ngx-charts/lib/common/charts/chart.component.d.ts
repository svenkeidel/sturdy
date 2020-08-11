import { OnChanges, EventEmitter, SimpleChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class ChartComponent implements OnChanges {
    view: any;
    showLegend: boolean;
    legendOptions: any;
    data: any;
    legendData: any;
    legendType: any;
    colors: any;
    activeEntries: any[];
    animations: boolean;
    legendLabelClick: EventEmitter<any>;
    legendLabelActivate: EventEmitter<any>;
    legendLabelDeactivate: EventEmitter<any>;
    chartWidth: any;
    title: any;
    legendWidth: any;
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    getLegendType(): string;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<ChartComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<ChartComponent, "ngx-charts-chart", never, { "showLegend": "showLegend"; "animations": "animations"; "legendType": "legendType"; "view": "view"; "legendOptions": "legendOptions"; "data": "data"; "legendData": "legendData"; "colors": "colors"; "activeEntries": "activeEntries"; }, { "legendLabelClick": "legendLabelClick"; "legendLabelActivate": "legendLabelActivate"; "legendLabelDeactivate": "legendLabelDeactivate"; }, never, ["*"]>;
}

//# sourceMappingURL=chart.component.d.ts.map