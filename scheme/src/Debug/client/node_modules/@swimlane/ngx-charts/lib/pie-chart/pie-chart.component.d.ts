import { EventEmitter, TemplateRef } from '@angular/core';
import { ColorHelper } from '../common/color.helper';
import { BaseChartComponent } from '../common/base-chart.component';
import { DataItem } from '../models/chart-data.model';
import * as ɵngcc0 from '@angular/core';
export declare class PieChartComponent extends BaseChartComponent {
    labels: boolean;
    legend: boolean;
    legendTitle: string;
    legendPosition: string;
    explodeSlices: boolean;
    doughnut: boolean;
    arcWidth: number;
    gradient: boolean;
    activeEntries: any[];
    tooltipDisabled: boolean;
    labelFormatting: any;
    trimLabels: boolean;
    maxLabelLength: number;
    tooltipText: any;
    dblclick: EventEmitter<any>;
    margins: number[];
    select: EventEmitter<any>;
    activate: EventEmitter<any>;
    deactivate: EventEmitter<any>;
    tooltipTemplate: TemplateRef<any>;
    translation: string;
    outerRadius: number;
    innerRadius: number;
    data: any;
    colors: ColorHelper;
    domain: any;
    dims: any;
    legendOptions: any;
    update(): void;
    getDomain(): any[];
    onClick(data: DataItem): void;
    setColors(): void;
    getLegendOptions(): {
        scaleType: string;
        domain: any;
        colors: ColorHelper;
        title: string;
        position: string;
    };
    onActivate(item: any, fromLegend?: boolean): void;
    onDeactivate(item: any, fromLegend?: boolean): void;
    private hasNoOptionalMarginsSet;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<PieChartComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<PieChartComponent, "ngx-charts-pie-chart", never, { "labels": "labels"; "legend": "legend"; "legendTitle": "legendTitle"; "legendPosition": "legendPosition"; "explodeSlices": "explodeSlices"; "doughnut": "doughnut"; "arcWidth": "arcWidth"; "activeEntries": "activeEntries"; "tooltipDisabled": "tooltipDisabled"; "trimLabels": "trimLabels"; "maxLabelLength": "maxLabelLength"; "margins": "margins"; "gradient": "gradient"; "labelFormatting": "labelFormatting"; "tooltipText": "tooltipText"; }, { "dblclick": "dblclick"; "select": "select"; "activate": "activate"; "deactivate": "deactivate"; }, ["tooltipTemplate"], never>;
}

//# sourceMappingURL=pie-chart.component.d.ts.map