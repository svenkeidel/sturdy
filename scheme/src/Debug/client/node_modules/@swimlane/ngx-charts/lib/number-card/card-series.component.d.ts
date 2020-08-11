import { EventEmitter, OnChanges, SimpleChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export interface CardModel {
    x: any;
    y: any;
    width: number;
    height: number;
    color: string;
    label: string;
    data: any;
    tooltipText: string;
}
export declare class CardSeriesComponent implements OnChanges {
    data: any[];
    slots: any[];
    dims: any;
    colors: any;
    innerPadding: number;
    cardColor: any;
    bandColor: any;
    emptyColor: string;
    textColor: any;
    valueFormatting: any;
    labelFormatting: any;
    animations: boolean;
    select: EventEmitter<any>;
    cards: CardModel[];
    emptySlots: any[];
    medianSize: number;
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    getCards(): any[];
    trackBy(index: any, card: any): string;
    onClick(data: any): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<CardSeriesComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<CardSeriesComponent, "g[ngx-charts-card-series]", never, { "innerPadding": "innerPadding"; "emptyColor": "emptyColor"; "animations": "animations"; "data": "data"; "slots": "slots"; "dims": "dims"; "colors": "colors"; "cardColor": "cardColor"; "bandColor": "bandColor"; "textColor": "textColor"; "valueFormatting": "valueFormatting"; "labelFormatting": "labelFormatting"; }, { "select": "select"; }, never, never>;
}

//# sourceMappingURL=card-series.component.d.ts.map