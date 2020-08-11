import { SimpleChanges, EventEmitter, OnChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class CircleComponent implements OnChanges {
    cx: any;
    cy: any;
    r: any;
    fill: any;
    stroke: any;
    data: any;
    classNames: any;
    circleOpacity: any;
    pointerEvents: any;
    select: EventEmitter<any>;
    activate: EventEmitter<any>;
    deactivate: EventEmitter<any>;
    onClick(): void;
    onMouseEnter(): void;
    onMouseLeave(): void;
    ngOnChanges(changes: SimpleChanges): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<CircleComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<CircleComponent, "g[ngx-charts-circle]", never, { "classNames": "classNames"; "cx": "cx"; "cy": "cy"; "r": "r"; "fill": "fill"; "stroke": "stroke"; "data": "data"; "circleOpacity": "circleOpacity"; "pointerEvents": "pointerEvents"; }, { "select": "select"; "activate": "activate"; "deactivate": "deactivate"; }, never, never>;
}

//# sourceMappingURL=circle.component.d.ts.map