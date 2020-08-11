import { OnChanges, SimpleChanges, TemplateRef, EventEmitter } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class PolarSeriesComponent implements OnChanges {
    name: any;
    data: any;
    xScale: any;
    yScale: any;
    colors: any;
    scaleType: any;
    curve: any;
    activeEntries: any[];
    rangeFillOpacity: number;
    tooltipDisabled: boolean;
    tooltipText: (o: any) => string;
    gradient: boolean;
    tooltipTemplate: TemplateRef<any>;
    animations: boolean;
    select: EventEmitter<any>;
    activate: EventEmitter<any>;
    deactivate: EventEmitter<any>;
    path: string;
    circles: any[];
    circleRadius: number;
    outerPath: string;
    areaPath: string;
    gradientId: string;
    gradientUrl: string;
    hasGradient: boolean;
    gradientStops: any[];
    areaGradientStops: any[];
    seriesColor: string;
    active: boolean;
    inactive: boolean;
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    getAngle(d: any): any;
    getRadius(d: any): any;
    getLineGenerator(): any;
    sortData(data: any): any;
    isActive(entry: any): boolean;
    isInactive(entry: any): boolean;
    defaultTooltipText({ label, value }: {
        label: any;
        value: any;
    }): string;
    updateGradients(): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<PolarSeriesComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<PolarSeriesComponent, "g[ngx-charts-polar-series]", never, { "tooltipDisabled": "tooltipDisabled"; "gradient": "gradient"; "animations": "animations"; "tooltipText": "tooltipText"; "name": "name"; "data": "data"; "xScale": "xScale"; "yScale": "yScale"; "colors": "colors"; "scaleType": "scaleType"; "curve": "curve"; "activeEntries": "activeEntries"; "rangeFillOpacity": "rangeFillOpacity"; "tooltipTemplate": "tooltipTemplate"; }, { "select": "select"; "activate": "activate"; "deactivate": "deactivate"; }, never, never>;
}

//# sourceMappingURL=polar-series.component.d.ts.map