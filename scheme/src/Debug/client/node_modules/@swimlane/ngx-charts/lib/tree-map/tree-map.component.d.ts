import { EventEmitter, TemplateRef } from '@angular/core';
import { BaseChartComponent } from '../common/base-chart.component';
import { ColorHelper } from '../common/color.helper';
import * as ɵngcc0 from '@angular/core';
export declare class TreeMapComponent extends BaseChartComponent {
    results: any;
    tooltipDisabled: boolean;
    valueFormatting: any;
    labelFormatting: any;
    gradient: boolean;
    select: EventEmitter<any>;
    tooltipTemplate: TemplateRef<any>;
    dims: any;
    domain: any;
    transform: any;
    colors: ColorHelper;
    treemap: any;
    data: any;
    margin: number[];
    update(): void;
    getDomain(): any[];
    onClick(data: any): void;
    setColors(): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<TreeMapComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<TreeMapComponent, "ngx-charts-tree-map", never, { "tooltipDisabled": "tooltipDisabled"; "gradient": "gradient"; "results": "results"; "valueFormatting": "valueFormatting"; "labelFormatting": "labelFormatting"; }, { "select": "select"; }, ["tooltipTemplate"], never>;
}

//# sourceMappingURL=tree-map.component.d.ts.map