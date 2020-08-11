import { OnChanges, SimpleChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class GaugeAxisComponent implements OnChanges {
    bigSegments: any;
    smallSegments: any;
    min: any;
    max: any;
    angleSpan: number;
    startAngle: number;
    radius: any;
    valueScale: any;
    tickFormatting: any;
    ticks: any;
    rotationAngle: number;
    rotate: string;
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    getTicks(): any;
    getTextAnchor(angle: any): string;
    getTickPath(startDistance: any, tickLength: any, angle: any): any;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<GaugeAxisComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<GaugeAxisComponent, "g[ngx-charts-gauge-axis]", never, { "bigSegments": "bigSegments"; "smallSegments": "smallSegments"; "min": "min"; "max": "max"; "angleSpan": "angleSpan"; "startAngle": "startAngle"; "radius": "radius"; "valueScale": "valueScale"; "tickFormatting": "tickFormatting"; }, {}, never, never>;
}

//# sourceMappingURL=gauge-axis.component.d.ts.map