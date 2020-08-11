import { EventEmitter, ElementRef, OnChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class TreeMapCellComponent implements OnChanges {
    data: any;
    fill: any;
    x: any;
    y: any;
    width: any;
    height: any;
    label: any;
    value: any;
    valueType: any;
    valueFormatting: any;
    labelFormatting: any;
    gradient: boolean;
    animations: boolean;
    select: EventEmitter<any>;
    gradientStops: any[];
    gradientId: string;
    gradientUrl: string;
    element: HTMLElement;
    transform: string;
    formattedLabel: string;
    formattedValue: string;
    initialized: boolean;
    constructor(element: ElementRef);
    ngOnChanges(): void;
    update(): void;
    loadAnimation(): void;
    getTextColor(): string;
    animateToCurrentForm(): void;
    onClick(): void;
    getGradientStops(): {
        offset: number;
        color: any;
        opacity: number;
    }[];
    static ɵfac: ɵngcc0.ɵɵFactoryDef<TreeMapCellComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<TreeMapCellComponent, "g[ngx-charts-tree-map-cell]", never, { "gradient": "gradient"; "animations": "animations"; "valueFormatting": "valueFormatting"; "data": "data"; "fill": "fill"; "x": "x"; "y": "y"; "width": "width"; "height": "height"; "label": "label"; "value": "value"; "valueType": "valueType"; "labelFormatting": "labelFormatting"; }, { "select": "select"; }, never, never>;
}

//# sourceMappingURL=tree-map-cell.component.d.ts.map