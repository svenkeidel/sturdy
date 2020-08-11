import { EventEmitter, OnChanges, SimpleChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class AreaSeriesComponent implements OnChanges {
    data: any;
    xScale: any;
    yScale: any;
    baseValue: any;
    colors: any;
    scaleType: any;
    stacked: boolean;
    normalized: boolean;
    gradient: any;
    curve: any;
    activeEntries: any[];
    animations: boolean;
    select: EventEmitter<any>;
    opacity: number;
    path: string;
    startingPath: string;
    hasGradient: boolean;
    gradientStops: any[];
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    updateGradient(): void;
    isActive(entry: any): boolean;
    isInactive(entry: any): boolean;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<AreaSeriesComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<AreaSeriesComponent, "g[ngx-charts-area-series]", never, { "baseValue": "baseValue"; "stacked": "stacked"; "normalized": "normalized"; "animations": "animations"; "data": "data"; "xScale": "xScale"; "yScale": "yScale"; "colors": "colors"; "scaleType": "scaleType"; "gradient": "gradient"; "curve": "curve"; "activeEntries": "activeEntries"; }, { "select": "select"; }, never, never>;
}

//# sourceMappingURL=area-series.component.d.ts.map