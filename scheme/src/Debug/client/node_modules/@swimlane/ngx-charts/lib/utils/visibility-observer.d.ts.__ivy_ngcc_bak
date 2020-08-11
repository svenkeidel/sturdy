import { EventEmitter, NgZone, ElementRef } from '@angular/core';
/**
 * Visibility Observer
 */
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
}
