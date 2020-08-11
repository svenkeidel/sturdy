import { EventEmitter, ElementRef, SimpleChanges, OnChanges } from '@angular/core';
import * as ɵngcc0 from '@angular/core';
export declare class PieArcComponent implements OnChanges {
    fill: any;
    startAngle: number;
    endAngle: number;
    innerRadius: any;
    outerRadius: any;
    cornerRadius: number;
    value: any;
    max: any;
    data: any;
    explodeSlices: boolean;
    gradient: boolean;
    animate: boolean;
    pointerEvents: boolean;
    isActive: boolean;
    select: EventEmitter<any>;
    activate: EventEmitter<any>;
    deactivate: EventEmitter<any>;
    dblclick: EventEmitter<any>;
    element: HTMLElement;
    path: any;
    startOpacity: number;
    radialGradientId: string;
    linearGradientId: string;
    gradientFill: string;
    initialized: boolean;
    private _timeout;
    constructor(element: ElementRef);
    ngOnChanges(changes: SimpleChanges): void;
    getGradient(): any;
    getPointerEvents(): "auto" | "none";
    update(): void;
    calculateArc(): any;
    loadAnimation(): void;
    updateAnimation(): void;
    onClick(): void;
    onDblClick(event: MouseEvent): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<PieArcComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<PieArcComponent, "g[ngx-charts-pie-arc]", never, { "startAngle": "startAngle"; "endAngle": "endAngle"; "cornerRadius": "cornerRadius"; "explodeSlices": "explodeSlices"; "gradient": "gradient"; "animate": "animate"; "pointerEvents": "pointerEvents"; "isActive": "isActive"; "fill": "fill"; "innerRadius": "innerRadius"; "outerRadius": "outerRadius"; "value": "value"; "max": "max"; "data": "data"; }, { "select": "select"; "activate": "activate"; "deactivate": "deactivate"; "dblclick": "dblclick"; }, never, never>;
}

//# sourceMappingURL=pie-arc.component.d.ts.map