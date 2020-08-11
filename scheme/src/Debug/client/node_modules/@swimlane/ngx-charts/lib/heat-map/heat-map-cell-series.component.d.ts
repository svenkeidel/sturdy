import { SimpleChanges, EventEmitter, OnChanges, OnInit, TemplateRef } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class HeatCellSeriesComponent implements OnChanges, OnInit {
    data: any;
    colors: any;
    xScale: any;
    yScale: any;
    gradient: boolean;
    tooltipDisabled: boolean;
    tooltipText: any;
    tooltipTemplate: TemplateRef<any>;
    animations: boolean;
    select: EventEmitter<any>;
    activate: EventEmitter<any>;
    deactivate: EventEmitter<any>;
    cells: any[];
    ngOnInit(): void;
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    getCells(): any[];
    getTooltipText({ label, data, series }: {
        label: any;
        data: any;
        series: any;
    }): string;
    trackBy(index: any, item: any): string;
    onClick(data: any): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<HeatCellSeriesComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<HeatCellSeriesComponent, "g[ngx-charts-heat-map-cell-series]", never, { "tooltipDisabled": "tooltipDisabled"; "animations": "animations"; "tooltipText": "tooltipText"; "data": "data"; "colors": "colors"; "xScale": "xScale"; "yScale": "yScale"; "gradient": "gradient"; "tooltipTemplate": "tooltipTemplate"; }, { "select": "select"; "activate": "activate"; "deactivate": "deactivate"; }, never, never>;
}

//# sourceMappingURL=heat-map-cell-series.component.d.ts.map