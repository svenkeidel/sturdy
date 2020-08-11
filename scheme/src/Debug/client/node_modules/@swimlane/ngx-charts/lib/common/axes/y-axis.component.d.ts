import { EventEmitter, OnChanges, SimpleChanges } from '@angular/core';
import { YAxisTicksComponent } from './y-axis-ticks.component';
import * as ɵngcc0 from '@angular/core';
export declare class YAxisComponent implements OnChanges {
    yScale: any;
    dims: any;
    trimTicks: boolean;
    maxTickLength: number;
    tickFormatting: any;
    ticks: any[];
    showGridLines: boolean;
    showLabel: any;
    labelText: any;
    yAxisTickInterval: any;
    yAxisTickCount: any;
    yOrient: string;
    referenceLines: any;
    showRefLines: any;
    showRefLabels: any;
    yAxisOffset: number;
    dimensionsChanged: EventEmitter<any>;
    yAxisClassName: string;
    tickArguments: any;
    offset: any;
    transform: any;
    labelOffset: number;
    fill: string;
    stroke: string;
    tickStroke: string;
    strokeWidth: number;
    padding: number;
    ticksComponent: YAxisTicksComponent;
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    emitTicksWidth({ width }: {
        width: any;
    }): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<YAxisComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<YAxisComponent, "g[ngx-charts-y-axis]", never, { "showGridLines": "showGridLines"; "yOrient": "yOrient"; "yAxisOffset": "yAxisOffset"; "yScale": "yScale"; "dims": "dims"; "trimTicks": "trimTicks"; "maxTickLength": "maxTickLength"; "tickFormatting": "tickFormatting"; "ticks": "ticks"; "showLabel": "showLabel"; "labelText": "labelText"; "yAxisTickInterval": "yAxisTickInterval"; "yAxisTickCount": "yAxisTickCount"; "referenceLines": "referenceLines"; "showRefLines": "showRefLines"; "showRefLabels": "showRefLabels"; }, { "dimensionsChanged": "dimensionsChanged"; }, never, never>;
}

//# sourceMappingURL=y-axis.component.d.ts.map