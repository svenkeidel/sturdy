import { EventEmitter, NgZone, ElementRef } from '@angular/core';
/**
 * Visibility Observer
 */
import * as ɵngcc0 from '@angular/core';
export declare class VisibilityObserver {
    private element;
    private zone;
    visible: EventEmitter<any>;
    timeout: any;
    isVisible: boolean;
    constructor(element: ElementRef, zone: NgZone);
    destroy(): void;
    onVisibilityChange(): void;
    runCheck(): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<VisibilityObserver, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<VisibilityObserver, "visibility-observer", never, {}, { "visible": "visible"; }, never>;
}

//# sourceMappingURL=visibility-observer.d.ts.map