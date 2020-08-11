import { EventEmitter, ElementRef, OnChanges, SimpleChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class PieGridSeriesComponent implements OnChanges {
    colors: any;
    data: any;
    innerRadius: number;
    outerRadius: number;
    animations: boolean;
    select: EventEmitter<any>;
    activate: EventEmitter<any>;
    deactivate: EventEmitter<any>;
    element: HTMLElement;
    layout: any;
    arcs: any;
    constructor(element: ElementRef);
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    getArcs(): any[];
    onClick(data: any): void;
    trackBy(index: any, item: any): string;
    label(arc: any): string;
    color(arc: any): any;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<PieGridSeriesComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<PieGridSeriesComponent, "g[ngx-charts-pie-grid-series]", never, { "innerRadius": "innerRadius"; "outerRadius": "outerRadius"; "animations": "animations"; "colors": "colors"; "data": "data"; }, { "select": "select"; "activate": "activate"; "deactivate": "deactivate"; }, never, never>;
}

//# sourceMappingURL=pie-grid-series.component.d.ts.map