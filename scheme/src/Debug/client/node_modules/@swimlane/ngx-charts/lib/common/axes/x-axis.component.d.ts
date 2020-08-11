import { SimpleChanges, EventEmitter, OnChanges } from '@angular/core';
import { XAxisTicksComponent } from './x-axis-ticks.component';
import * as ɵngcc0 from '@angular/core';
export declare class XAxisComponent implements OnChanges {
    xScale: any;
    dims: any;
    trimTicks: boolean;
    rotateTicks: boolean;
    maxTickLength: number;
    tickFormatting: any;
    showGridLines: boolean;
    showLabel: any;
    labelText: any;
    ticks: any[];
    xAxisTickInterval: any;
    xAxisTickCount: any;
    xOrient: string;
    xAxisOffset: number;
    dimensionsChanged: EventEmitter<any>;
    xAxisClassName: string;
    tickArguments: any;
    transform: any;
    labelOffset: number;
    fill: string;
    stroke: string;
    tickStroke: string;
    strokeWidth: string;
    padding: number;
    ticksComponent: XAxisTicksComponent;
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    emitTicksHeight({ height }: {
        height: any;
    }): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<XAxisComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<XAxisComponent, "g[ngx-charts-x-axis]", never, { "rotateTicks": "rotateTicks"; "showGridLines": "showGridLines"; "xOrient": "xOrient"; "xAxisOffset": "xAxisOffset"; "xScale": "xScale"; "dims": "dims"; "trimTicks": "trimTicks"; "maxTickLength": "maxTickLength"; "tickFormatting": "tickFormatting"; "showLabel": "showLabel"; "labelText": "labelText"; "ticks": "ticks"; "xAxisTickInterval": "xAxisTickInterval"; "xAxisTickCount": "xAxisTickCount"; }, { "dimensionsChanged": "dimensionsChanged"; }, never, never>;
}

//# sourceMappingURL=x-axis.component.d.ts.map