import { EventEmitter, OnChanges, SimpleChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class AdvancedLegendComponent implements OnChanges {
    width: number;
    data: any;
    colors: any;
    label: string;
    animations: boolean;
    select: EventEmitter<any>;
    activate: EventEmitter<any>;
    deactivate: EventEmitter<any>;
    legendItems: any[];
    total: number;
    roundedTotal: number;
    valueFormatting: (value: number) => any;
    labelFormatting: (value: string) => any;
    percentageFormatting: (value: number) => any;
    defaultValueFormatting: (value: number) => any;
    ngOnChanges(changes: SimpleChanges): void;
    getTotal(): number;
    update(): void;
    getLegendItems(): any;
    trackBy(item: any): any;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<AdvancedLegendComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<AdvancedLegendComponent, "ngx-charts-advanced-legend", never, { "label": "label"; "animations": "animations"; "labelFormatting": "labelFormatting"; "percentageFormatting": "percentageFormatting"; "width": "width"; "data": "data"; "colors": "colors"; "valueFormatting": "valueFormatting"; }, { "select": "select"; "activate": "activate"; "deactivate": "deactivate"; }, never, never>;
}

//# sourceMappingURL=advanced-legend.component.d.ts.map