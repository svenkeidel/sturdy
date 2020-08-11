import { ElementRef, OnChanges, SimpleChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class AxisLabelComponent implements OnChanges {
    orient: any;
    label: any;
    offset: any;
    width: any;
    height: any;
    x: any;
    y: any;
    transform: any;
    strokeWidth: any;
    textAnchor: any;
    element: ElementRef;
    textHeight: number;
    margin: number;
    constructor(element: ElementRef);
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<AxisLabelComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<AxisLabelComponent, "g[ngx-charts-axis-label]", never, { "orient": "orient"; "label": "label"; "offset": "offset"; "width": "width"; "height": "height"; }, {}, never, never>;
}

//# sourceMappingURL=axis-label.component.d.ts.map