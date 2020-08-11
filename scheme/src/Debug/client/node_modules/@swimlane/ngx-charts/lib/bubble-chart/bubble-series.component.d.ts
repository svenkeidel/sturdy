import { SimpleChanges, EventEmitter, OnChanges, TemplateRef } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class BubbleSeriesComponent implements OnChanges {
    data: any;
    xScale: any;
    yScale: any;
    rScale: any;
    xScaleType: any;
    yScaleType: any;
    colors: any;
    visibleValue: any;
    activeEntries: any[];
    xAxisLabel: string;
    yAxisLabel: string;
    tooltipDisabled: boolean;
    tooltipTemplate: TemplateRef<any>;
    select: EventEmitter<any>;
    activate: EventEmitter<any>;
    deactivate: EventEmitter<any>;
    areaPath: any;
    circles: any[];
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    getCircles(): any[];
    getTooltipText(circle: any): string;
    onClick(data: any): void;
    isActive(entry: any): boolean;
    isVisible(circle: any): boolean;
    activateCircle(circle: any): void;
    deactivateCircle(circle: any): void;
    trackBy(index: any, circle: any): string;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<BubbleSeriesComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<BubbleSeriesComponent, "g[ngx-charts-bubble-series]", never, { "tooltipDisabled": "tooltipDisabled"; "data": "data"; "xScale": "xScale"; "yScale": "yScale"; "rScale": "rScale"; "xScaleType": "xScaleType"; "yScaleType": "yScaleType"; "colors": "colors"; "visibleValue": "visibleValue"; "activeEntries": "activeEntries"; "xAxisLabel": "xAxisLabel"; "yAxisLabel": "yAxisLabel"; "tooltipTemplate": "tooltipTemplate"; }, { "select": "select"; "activate": "activate"; "deactivate": "deactivate"; }, never, never>;
}

//# sourceMappingURL=bubble-series.component.d.ts.map