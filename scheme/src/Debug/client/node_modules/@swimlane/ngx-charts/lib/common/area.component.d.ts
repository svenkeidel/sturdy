import { SimpleChanges, EventEmitter, ElementRef, OnChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class AreaComponent implements OnChanges {
    data: any;
    path: any;
    startingPath: any;
    fill: any;
    opacity: number;
    startOpacity: number;
    endOpacity: number;
    activeLabel: any;
    gradient: boolean;
    stops: any[];
    animations: boolean;
    select: EventEmitter<any>;
    element: HTMLElement;
    gradientId: string;
    gradientFill: string;
    areaPath: string;
    initialized: boolean;
    gradientStops: any[];
    hasGradient: boolean;
    constructor(element: ElementRef);
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    loadAnimation(): void;
    updatePathEl(): void;
    getGradient(): any[];
    static ɵfac: ɵngcc0.ɵɵFactoryDef<AreaComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<AreaComponent, "g[ngx-charts-area]", never, { "opacity": "opacity"; "startOpacity": "startOpacity"; "endOpacity": "endOpacity"; "gradient": "gradient"; "animations": "animations"; "data": "data"; "path": "path"; "startingPath": "startingPath"; "fill": "fill"; "activeLabel": "activeLabel"; "stops": "stops"; }, { "select": "select"; }, never, never>;
}

//# sourceMappingURL=area.component.d.ts.map