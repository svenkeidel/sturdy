import { OnChanges, SimpleChanges, EventEmitter, TemplateRef } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class TreeMapCellSeriesComponent implements OnChanges {
    data: any;
    dims: any;
    colors: any;
    valueFormatting: any;
    labelFormatting: any;
    gradient: boolean;
    tooltipDisabled: boolean;
    tooltipTemplate: TemplateRef<any>;
    animations: boolean;
    select: EventEmitter<any>;
    cells: any[];
    ngOnChanges(changes: SimpleChanges): void;
    getCells(): any[];
    getTooltipText({ label, value }: {
        label: any;
        value: any;
    }): string;
    onClick(data: any): void;
    trackBy(index: any, item: any): string;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<TreeMapCellSeriesComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<TreeMapCellSeriesComponent, "g[ngx-charts-tree-map-cell-series]", never, { "gradient": "gradient"; "tooltipDisabled": "tooltipDisabled"; "animations": "animations"; "data": "data"; "dims": "dims"; "colors": "colors"; "valueFormatting": "valueFormatting"; "labelFormatting": "labelFormatting"; "tooltipTemplate": "tooltipTemplate"; }, { "select": "select"; }, never, never>;
}

//# sourceMappingURL=tree-map-cell-series.component.d.ts.map