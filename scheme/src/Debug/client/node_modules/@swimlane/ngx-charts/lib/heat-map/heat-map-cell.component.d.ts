import { EventEmitter, SimpleChanges, ElementRef, OnChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class HeatMapCellComponent implements OnChanges {
    fill: any;
    x: any;
    y: any;
    width: any;
    height: any;
    data: any;
    label: any;
    gradient: boolean;
    animations: boolean;
    select: EventEmitter<any>;
    activate: EventEmitter<any>;
    deactivate: EventEmitter<any>;
    element: HTMLElement;
    transform: string;
    activeRange: any[];
    startOpacity: number;
    gradientId: string;
    gradientUrl: string;
    gradientStops: any[];
    constructor(element: ElementRef);
    ngOnChanges(changes: SimpleChanges): void;
    getGradientStops(): {
        offset: number;
        color: any;
        opacity: number;
    }[];
    loadAnimation(): void;
    animateToCurrentForm(): void;
    onClick(): void;
    onMouseEnter(): void;
    onMouseLeave(): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<HeatMapCellComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<HeatMapCellComponent, "g[ngx-charts-heat-map-cell]", never, { "gradient": "gradient"; "animations": "animations"; "fill": "fill"; "x": "x"; "y": "y"; "width": "width"; "height": "height"; "data": "data"; "label": "label"; }, { "select": "select"; "activate": "activate"; "deactivate": "deactivate"; }, never, never>;
}

//# sourceMappingURL=heat-map-cell.component.d.ts.map