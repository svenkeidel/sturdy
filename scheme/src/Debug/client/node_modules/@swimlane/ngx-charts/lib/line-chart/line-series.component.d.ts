import { OnChanges, SimpleChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class LineSeriesComponent implements OnChanges {
    data: any;
    xScale: any;
    yScale: any;
    colors: any;
    scaleType: any;
    curve: any;
    activeEntries: any[];
    rangeFillOpacity: number;
    hasRange: boolean;
    animations: boolean;
    path: string;
    outerPath: string;
    areaPath: string;
    gradientId: string;
    gradientUrl: string;
    hasGradient: boolean;
    gradientStops: any[];
    areaGradientStops: any[];
    stroke: any;
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    getLineGenerator(): any;
    getRangeGenerator(): any;
    getAreaGenerator(): any;
    sortData(data: any): any;
    updateGradients(): void;
    isActive(entry: any): boolean;
    isInactive(entry: any): boolean;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<LineSeriesComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<LineSeriesComponent, "g[ngx-charts-line-series]", never, { "animations": "animations"; "data": "data"; "xScale": "xScale"; "yScale": "yScale"; "colors": "colors"; "scaleType": "scaleType"; "curve": "curve"; "activeEntries": "activeEntries"; "rangeFillOpacity": "rangeFillOpacity"; "hasRange": "hasRange"; }, {}, never, never>;
}

//# sourceMappingURL=line-series.component.d.ts.map