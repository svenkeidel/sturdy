import { EventEmitter, SimpleChanges, OnChanges, ChangeDetectorRef } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class LegendComponent implements OnChanges {
    private cd;
    data: any;
    title: any;
    colors: any;
    height: any;
    width: any;
    activeEntries: any;
    horizontal: boolean;
    labelClick: EventEmitter<any>;
    labelActivate: EventEmitter<any>;
    labelDeactivate: EventEmitter<any>;
    legendEntries: any[];
    constructor(cd: ChangeDetectorRef);
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    getLegendEntries(): any[];
    isActive(entry: any): boolean;
    activate(item: any): void;
    deactivate(item: any): void;
    trackBy(index: any, item: any): string;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<LegendComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<LegendComponent, "ngx-charts-legend", never, { "horizontal": "horizontal"; "data": "data"; "title": "title"; "colors": "colors"; "height": "height"; "width": "width"; "activeEntries": "activeEntries"; }, { "labelClick": "labelClick"; "labelActivate": "labelActivate"; "labelDeactivate": "labelDeactivate"; }, never, never>;
}

//# sourceMappingURL=legend.component.d.ts.map