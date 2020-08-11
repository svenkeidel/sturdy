import { EventEmitter, OnChanges, ElementRef, SimpleChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class LineComponent implements OnChanges {
    private element;
    path: any;
    stroke: any;
    data: any;
    fill: string;
    animations: boolean;
    select: EventEmitter<any>;
    initialized: boolean;
    initialPath: string;
    constructor(element: ElementRef);
    ngOnChanges(changes: SimpleChanges): void;
    updatePathEl(): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<LineComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<LineComponent, "g[ngx-charts-line]", never, { "fill": "fill"; "animations": "animations"; "path": "path"; "stroke": "stroke"; "data": "data"; }, { "select": "select"; }, never, never>;
}

//# sourceMappingURL=line.component.d.ts.map