/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Platform } from '@angular/cdk/platform';
import { Directive, ElementRef, Inject, InjectionToken, Input, NgZone, Optional, } from '@angular/core';
import { RippleRenderer } from './ripple-renderer';
import { ANIMATION_MODULE_TYPE } from '@angular/platform-browser/animations';
/** Injection token that can be used to specify the global ripple options. */
export const MAT_RIPPLE_GLOBAL_OPTIONS = new InjectionToken('mat-ripple-global-options');
let MatRipple = /** @class */ (() => {
    class MatRipple {
        constructor(_elementRef, ngZone, platform, globalOptions, _animationMode) {
            this._elementRef = _elementRef;
            this._animationMode = _animationMode;
            /**
             * If set, the radius in pixels of foreground ripples when fully expanded. If unset, the radius
             * will be the distance from the center of the ripple to the furthest corner of the host element's
             * bounding rectangle.
             */
            this.radius = 0;
            this._disabled = false;
            /** Whether ripple directive is initialized and the input bindings are set. */
            this._isInitialized = false;
            this._globalOptions = globalOptions || {};
            this._rippleRenderer = new RippleRenderer(this, ngZone, _elementRef, platform);
        }
        /**
         * Whether click events will not trigger the ripple. Ripples can be still launched manually
         * by using the `launch()` method.
         */
        get disabled() { return this._disabled; }
        set disabled(value) {
            this._disabled = value;
            this._setupTriggerEventsIfEnabled();
        }
        /**
         * The element that triggers the ripple when click events are received.
         * Defaults to the directive's host element.
         */
        get trigger() { return this._trigger || this._elementRef.nativeElement; }
        set trigger(trigger) {
            this._trigger = trigger;
            this._setupTriggerEventsIfEnabled();
        }
        ngOnInit() {
            this._isInitialized = true;
            this._setupTriggerEventsIfEnabled();
        }
        ngOnDestroy() {
            this._rippleRenderer._removeTriggerEvents();
        }
        /** Fades out all currently showing ripple elements. */
        fadeOutAll() {
            this._rippleRenderer.fadeOutAll();
        }
        /**
         * Ripple configuration from the directive's input values.
         * @docs-private Implemented as part of RippleTarget
         */
        get rippleConfig() {
            return {
                centered: this.centered,
                radius: this.radius,
                color: this.color,
                animation: Object.assign(Object.assign(Object.assign({}, this._globalOptions.animation), (this._animationMode === 'NoopAnimations' ? { enterDuration: 0, exitDuration: 0 } : {})), this.animation),
                terminateOnPointerUp: this._globalOptions.terminateOnPointerUp,
            };
        }
        /**
         * Whether ripples on pointer-down are disabled or not.
         * @docs-private Implemented as part of RippleTarget
         */
        get rippleDisabled() {
            return this.disabled || !!this._globalOptions.disabled;
        }
        /** Sets up the trigger event listeners if ripples are enabled. */
        _setupTriggerEventsIfEnabled() {
            if (!this.disabled && this._isInitialized) {
                this._rippleRenderer.setupTriggerEvents(this.trigger);
            }
        }
        /** Launches a manual ripple at the specified coordinated or just by the ripple config. */
        launch(configOrX, y = 0, config) {
            if (typeof configOrX === 'number') {
                return this._rippleRenderer.fadeInRipple(configOrX, y, Object.assign(Object.assign({}, this.rippleConfig), config));
            }
            else {
                return this._rippleRenderer.fadeInRipple(0, 0, Object.assign(Object.assign({}, this.rippleConfig), configOrX));
            }
        }
    }
    MatRipple.decorators = [
        { type: Directive, args: [{
                    selector: '[mat-ripple], [matRipple]',
                    exportAs: 'matRipple',
                    host: {
                        'class': 'mat-ripple',
                        '[class.mat-ripple-unbounded]': 'unbounded'
                    }
                },] }
    ];
    MatRipple.ctorParameters = () => [
        { type: ElementRef },
        { type: NgZone },
        { type: Platform },
        { type: undefined, decorators: [{ type: Optional }, { type: Inject, args: [MAT_RIPPLE_GLOBAL_OPTIONS,] }] },
        { type: String, decorators: [{ type: Optional }, { type: Inject, args: [ANIMATION_MODULE_TYPE,] }] }
    ];
    MatRipple.propDecorators = {
        color: [{ type: Input, args: ['matRippleColor',] }],
        unbounded: [{ type: Input, args: ['matRippleUnbounded',] }],
        centered: [{ type: Input, args: ['matRippleCentered',] }],
        radius: [{ type: Input, args: ['matRippleRadius',] }],
        animation: [{ type: Input, args: ['matRippleAnimation',] }],
        disabled: [{ type: Input, args: ['matRippleDisabled',] }],
        trigger: [{ type: Input, args: ['matRippleTrigger',] }]
    };
    return MatRipple;
})();
export { MatRipple };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicmlwcGxlLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL2NvcmUvcmlwcGxlL3JpcHBsZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsUUFBUSxFQUFDLE1BQU0sdUJBQXVCLENBQUM7QUFDL0MsT0FBTyxFQUNMLFNBQVMsRUFDVCxVQUFVLEVBQ1YsTUFBTSxFQUNOLGNBQWMsRUFDZCxLQUFLLEVBQ0wsTUFBTSxFQUdOLFFBQVEsR0FDVCxNQUFNLGVBQWUsQ0FBQztBQUV2QixPQUFPLEVBQUMsY0FBYyxFQUFlLE1BQU0sbUJBQW1CLENBQUM7QUFDL0QsT0FBTyxFQUFDLHFCQUFxQixFQUFDLE1BQU0sc0NBQXNDLENBQUM7QUF3QjNFLDZFQUE2RTtBQUM3RSxNQUFNLENBQUMsTUFBTSx5QkFBeUIsR0FDbEMsSUFBSSxjQUFjLENBQXNCLDJCQUEyQixDQUFDLENBQUM7QUFFekU7SUFBQSxNQVFhLFNBQVM7UUE2RHBCLFlBQW9CLFdBQW9DLEVBQzVDLE1BQWMsRUFDZCxRQUFrQixFQUM2QixhQUFtQyxFQUMvQixjQUF1QjtZQUpsRSxnQkFBVyxHQUFYLFdBQVcsQ0FBeUI7WUFJTyxtQkFBYyxHQUFkLGNBQWMsQ0FBUztZQW5EdEY7Ozs7ZUFJRztZQUN1QixXQUFNLEdBQVcsQ0FBQyxDQUFDO1lBbUJyQyxjQUFTLEdBQVksS0FBSyxDQUFDO1lBb0JuQyw4RUFBOEU7WUFDdEUsbUJBQWMsR0FBWSxLQUFLLENBQUM7WUFRdEMsSUFBSSxDQUFDLGNBQWMsR0FBRyxhQUFhLElBQUksRUFBRSxDQUFDO1lBQzFDLElBQUksQ0FBQyxlQUFlLEdBQUcsSUFBSSxjQUFjLENBQUMsSUFBSSxFQUFFLE1BQU0sRUFBRSxXQUFXLEVBQUUsUUFBUSxDQUFDLENBQUM7UUFDakYsQ0FBQztRQXpDRDs7O1dBR0c7UUFDSCxJQUNJLFFBQVEsS0FBSyxPQUFPLElB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QUFLLFNBQUMsZ0JBQWdCOzRCQUd0QixLQUFLLFNBQUMsb0JBQW9COzJCQU0xQixLQUFLLFNBQUMsbUJBQW1CO3lCQU96QixLQUFLLFNBQUMsaUJBQWlCOzRCQU92QixLQUFLLFNBQUMsb0JBQW9COzJCQU0xQixLQUFLLFNBQUMsbUJBQW1COzBCQVl6QixLQUFLLFNBQUMsa0JBQWtCOztJQWdHM0IsZ0JBQUM7S0FBQTtTQTVJWSxTQUFTIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7UGxhdGZvcm19IGZyb20gJ0Bhbmd1bGFyL2Nkay9wbGF0Zm9ybSc7XG5pbXBvcnQge1xuICBEaXJlY3RpdmUsXG4gIEVsZW1lbnRSZWYsXG4gIEluamVjdCxcbiAgSW5qZWN0aW9uVG9rZW4sXG4gIElucHV0LFxuICBOZ1pvbmUsXG4gIE9uRGVzdHJveSxcbiAgT25Jbml0LFxuICBPcHRpb25hbCxcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQge1JpcHBsZUFuaW1hdGlvbkNvbmZpZywgUmlwcGxlQ29uZmlnLCBSaXBwbGVSZWZ9IGZyb20gJy4vcmlwcGxlLXJlZic7XG5pbXBvcnQge1JpcHBsZVJlbmRlcmVyLCBSaXBwbGVUYXJnZXR9IGZyb20gJy4vcmlwcGxlLXJlbmRlcmVyJztcbmltcG9ydCB7QU5JTUFUSU9OX01PRFVMRV9UWVBFfSBmcm9tICdAYW5ndWxhci9wbGF0Zm9ybS1icm93c2VyL2FuaW1hdGlvbnMnO1xuXG4vKiogQ29uZmlndXJhYmxlIG9wdGlvbnMgZm9yIGBtYXRSaXBwbGVgLiAqL1xuZXhwb3J0IGludGVyZmFjZSBSaXBwbGVHbG9iYWxPcHRpb25zIHtcbiAgLyoqXG4gICAqIFdoZXRoZXIgcmlwcGxlcyBzaG91bGQgYmUgZGlzYWJsZWQuIFJpcHBsZXMgY2FuIGJlIHN0aWxsIGxhdW5jaGVkIG1hbnVhbGx5IGJ5IHVzaW5nXG4gICAqIHRoZSBgbGF1bmNoKClgIG1ldGhvZC4gVGhlcmVmb3JlIGZvY3VzIGluZGljYXRvcnMgd2lsbCBzdGlsbCBzaG93IHVwLlxuICAgKi9cbiAgZGlzYWJsZWQ/OiBib29sZWFuO1xuXG4gIC8qKlxuICAgKiBDb25maWd1cmF0aW9uIGZvciB0aGUgYW5pbWF0aW9uIGR1cmF0aW9uIG9mIHRoZSByaXBwbGVzLiBUaGVyZSBhcmUgdHdvIHBoYXNlcyB3aXRoIGRpZmZlcmVudFxuICAgKiBkdXJhdGlvbnMgZm9yIHRoZSByaXBwbGVzLiBUaGUgYW5pbWF0aW9uIGR1cmF0aW9ucyB3aWxsIGJlIG92ZXJ3cml0dGVuIGlmIHRoZVxuICAgKiBgTm9vcEFuaW1hdGlvbnNNb2R1bGVgIGlzIGJlaW5nIHVzZWQuXG4gICAqL1xuICBhbmltYXRpb24/OiBSaXBwbGVBbmltYXRpb25Db25maWc7XG5cbiAgLyoqXG4gICAqIFdoZXRoZXIgcmlwcGxlcyBzaG91bGQgc3RhcnQgZmFkaW5nIG91dCBpbW1lZGlhdGVseSBhZnRlciB0aGUgbW91c2Ugb3IgdG91Y2ggaXMgcmVsZWFzZWQuIEJ5XG4gICAqIGRlZmF1bHQsIHJpcHBsZXMgd2lsbCB3YWl0IGZvciB0aGUgZW50ZXIgYW5pbWF0aW9uIHRvIGNvbXBsZXRlIGFuZCBmb3IgbW91c2Ugb3IgdG91Y2ggcmVsZWFzZS5cbiAgICovXG4gIHRlcm1pbmF0ZU9uUG9pbnRlclVwPzogYm9vbGVhbjtcbn1cblxuLyoqIEluamVjdGlvbiB0b2tlbiB0aGF0IGNhbiBiZSB1c2VkIHRvIHNwZWNpZnkgdGhlIGdsb2JhbCByaXBwbGUgb3B0aW9ucy4gKi9cbmV4cG9ydCBjb25zdCBNQVRfUklQUExFX0dMT0JBTF9PUFRJT05TID1cbiAgICBuZXcgSW5qZWN0aW9uVG9rZW48UmlwcGxlR2xvYmFsT3B0aW9ucz4oJ21hdC1yaXBwbGUtZ2xvYmFsLW9wdGlvbnMnKTtcblxuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnW21hdC1yaXBwbGVdLCBbbWF0UmlwcGxlXScsXG4gIGV4cG9ydEFzOiAnbWF0UmlwcGxlJyxcbiAgaG9zdDoge1xuICAgICdjbGFzcyc6ICdtYXQtcmlwcGxlJyxcbiAgICAnW2NsYXNzLm1hdC1yaXBwbGUtdW5ib3VuZGVkXSc6ICd1bmJvdW5kZWQnXG4gIH1cbn0pXG5leHBvcnQgY2xhc3MgTWF0UmlwcGxlIGltcGxlbWVudHMgT25Jbml0LCBPbkRlc3Ryb3ksIFJpcHBsZVRhcmdldCB7XG5cbiAgLyoqIEN1c3RvbSBjb2xvciBmb3IgYWxsIHJpcHBsZXMuICovXG4gIEBJbnB1dCgnbWF0UmlwcGxlQ29sb3InKSBjb2xvcjogc3RyaW5nO1xuXG4gIC8qKiBXaGV0aGVyIHRoZSByaXBwbGVzIHNob3VsZCBiZSB2aXNpYmxlIG91dHNpZGUgdGhlIGNvbXBvbmVudCdzIGJvdW5kcy4gKi9cbiAgQElucHV0KCdtYXRSaXBwbGVVbmJvdW5kZWQnKSB1bmJvdW5kZWQ6IGJvb2xlYW47XG5cbiAgLyoqXG4gICAqIFdoZXRoZXIgdGhlIHJpcHBsZSBhbHdheXMgb3JpZ2luYXRlcyBmcm9tIHRoZSBjZW50ZXIgb2YgdGhlIGhvc3QgZWxlbWVudCdzIGJvdW5kcywgcmF0aGVyXG4gICAqIHRoYW4gb3JpZ2luYXRpbmcgZnJvbSB0aGUgbG9jYXRpb24gb2YgdGhlIGNsaWNrIGV2ZW50LlxuICAgKi9cbiAgQElucHV0KCdtYXRSaXBwbGVDZW50ZXJlZCcpIGNlbnRlcmVkOiBib29sZWFuO1xuXG4gIC8qKlxuICAgKiBJZiBzZXQsIHRoZSByYWRpdXMgaW4gcGl4ZWxzIG9mIGZvcmVncm91bmQgcmlwcGxlcyB3aGVuIGZ1bGx5IGV4cGFuZGVkLiBJZiB1bnNldCwgdGhlIHJhZGl1c1xuICAgKiB3aWxsIGJlIHRoZSBkaXN0YW5jZSBmcm9tIHRoZSBjZW50ZXIgb2YgdGhlIHJpcHBsZSB0byB0aGUgZnVydGhlc3QgY29ybmVyIG9mIHRoZSBob3N0IGVsZW1lbnQnc1xuICAgKiBib3VuZGluZyByZWN0YW5nbGUuXG4gICAqL1xuICBASW5wdXQoJ21hdFJpcHBsZVJhZGl1cycpIHJhZGl1czogbnVtYmVyID0gMDtcblxuICAvKipcbiAgICogQ29uZmlndXJhdGlvbiBmb3IgdGhlIHJpcHBsZSBhbmltYXRpb24uIEFsbG93cyBtb2RpZnlpbmcgdGhlIGVudGVyIGFuZCBleGl0IGFuaW1hdGlvblxuICAgKiBkdXJhdGlvbiBvZiB0aGUgcmlwcGxlcy4gVGhlIGFuaW1hdGlvbiBkdXJhdGlvbnMgd2lsbCBiZSBvdmVyd3JpdHRlbiBpZiB0aGVcbiAgICogYE5vb3BBbmltYXRpb25zTW9kdWxlYCBpcyBiZWluZyB1c2VkLlxuICAgKi9cbiAgQElucHV0KCdtYXRSaXBwbGVBbmltYXRpb24nKSBhbmltYXRpb246IFJpcHBsZUFuaW1hdGlvbkNvbmZpZztcblxuICAvKipcbiAgICogV2hldGhlciBjbGljayBldmVudHMgd2lsbCBub3QgdHJpZ2dlciB0aGUgcmlwcGxlLiBSaXBwbGVzIGNhbiBiZSBzdGlsbCBsYXVuY2hlZCBtYW51YWxseVxuICAgKiBieSB1c2luZyB0aGUgYGxhdW5jaCgpYCBtZXRob2QuXG4gICAqL1xuICBASW5wdXQoJ21hdFJpcHBsZURpc2FibGVkJylcbiAgZ2V0IGRpc2FibGVkKCkgeyByZXR1cm4gdGhpcy5fZGlzYWJsZWQ7IH1cbiAgc2V0IGRpc2FibGVkKHZhbHVlOiBib29sZWFuKSB7XG4gICAgdGhpcy5fZGlzYWJsZWQgPSB2YWx1ZTtcbiAgICB0aGlzLl9zZXR1cFRyaWdnZXJFdmVudHNJZkVuYWJsZWQoKTtcbiAgfVxuICBwcml2YXRlIF9kaXNhYmxlZDogYm9vbGVhbiA9IGZhbHNlO1xuXG4gIC8qKlxuICAgKiBUaGUgZWxlbWVudCB0aGF0IHRyaWdnZXJzIHRoZSByaXBwbGUgd2hlbiBjbGljayBldmVudHMgYXJlIHJlY2VpdmVkLlxuICAgKiBEZWZhdWx0cyB0byB0aGUgZGlyZWN0aXZlJ3MgaG9zdCBlbGVtZW50LlxuICAgKi9cbiAgQElucHV0KCdtYXRSaXBwbGVUcmlnZ2VyJylcbiAgZ2V0IHRyaWdnZXIoKSB7IHJldHVybiB0aGlzLl90cmlnZ2VyIHx8IHRoaXMuX2VsZW1lbnRSZWYubmF0aXZlRWxlbWVudDsgfVxuICBzZXQgdHJpZ2dlcih0cmlnZ2VyOiBIVE1MRWxlbWVudCkge1xuICAgIHRoaXMuX3RyaWdnZXIgPSB0cmlnZ2VyO1xuICAgIHRoaXMuX3NldHVwVHJpZ2dlckV2ZW50c0lmRW5hYmxlZCgpO1xuICB9XG4gIHByaXZhdGUgX3RyaWdnZXI6IEhUTUxFbGVtZW50O1xuXG4gIC8qKiBSZW5kZXJlciBmb3IgdGhlIHJpcHBsZSBET00gbWFuaXB1bGF0aW9ucy4gKi9cbiAgcHJpdmF0ZSBfcmlwcGxlUmVuZGVyZXI6IFJpcHBsZVJlbmRlcmVyO1xuXG4gIC8qKiBPcHRpb25zIHRoYXQgYXJlIHNldCBnbG9iYWxseSBmb3IgYWxsIHJpcHBsZXMuICovXG4gIHByaXZhdGUgX2dsb2JhbE9wdGlvbnM6IFJpcHBsZUdsb2JhbE9wdGlvbnM7XG5cbiAgLyoqIFdoZXRoZXIgcmlwcGxlIGRpcmVjdGl2ZSBpcyBpbml0aWFsaXplZCBhbmQgdGhlIGlucHV0IGJpbmRpbmdzIGFyZSBzZXQuICovXG4gIHByaXZhdGUgX2lzSW5pdGlhbGl6ZWQ6IGJvb2xlYW4gPSBmYWxzZTtcblxuICBjb25zdHJ1Y3Rvcihwcml2YXRlIF9lbGVtZW50UmVmOiBFbGVtZW50UmVmPEhUTUxFbGVtZW50PixcbiAgICAgICAgICAgICAgbmdab25lOiBOZ1pvbmUsXG4gICAgICAgICAgICAgIHBsYXRmb3JtOiBQbGF0Zm9ybSxcbiAgICAgICAgICAgICAgQE9wdGlvbmFsKCkgQEluamVjdChNQVRfUklQUExFX0dMT0JBTF9PUFRJT05TKSBnbG9iYWxPcHRpb25zPzogUmlwcGxlR2xvYmFsT3B0aW9ucyxcbiAgICAgICAgICAgICAgQE9wdGlvbmFsKCkgQEluamVjdChBTklNQVRJT05fTU9EVUxFX1RZUEUpIHByaXZhdGUgX2FuaW1hdGlvbk1vZGU/OiBzdHJpbmcpIHtcblxuICAgIHRoaXMuX2dsb2JhbE9wdGlvbnMgPSBnbG9iYWxPcHRpb25zIHx8IHt9O1xuICAgIHRoaXMuX3JpcHBsZVJlbmRlcmVyID0gbmV3IFJpcHBsZVJlbmRlcmVyKHRoaXMsIG5nWm9uZSwgX2VsZW1lbnRSZWYsIHBsYXRmb3JtKTtcbiAgfVxuXG4gIG5nT25Jbml0KCkge1xuICAgIHRoaXMuX2lzSW5pdGlhbGl6ZWQgPSB0cnVlO1xuICAgIHRoaXMuX3NldHVwVHJpZ2dlckV2ZW50c0lmRW5hYmxlZCgpO1xuICB9XG5cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgdGhpcy5fcmlwcGxlUmVuZGVyZXIuX3JlbW92ZVRyaWdnZXJFdmVudHMoKTtcbiAgfVxuXG4gIC8qKiBGYWRlcyBvdXQgYWxsIGN1cnJlbnRseSBzaG93aW5nIHJpcHBsZSBlbGVtZW50cy4gKi9cbiAgZmFkZU91dEFsbCgpIHtcbiAgICB0aGlzLl9yaXBwbGVSZW5kZXJlci5mYWRlT3V0QWxsKCk7XG4gIH1cblxuICAvKipcbiAgICogUmlwcGxlIGNvbmZpZ3VyYXRpb24gZnJvbSB0aGUgZGlyZWN0aXZlJ3MgaW5wdXQgdmFsdWVzLlxuICAgKiBAZG9jcy1wcml2YXRlIEltcGxlbWVudGVkIGFzIHBhcnQgb2YgUmlwcGxlVGFyZ2V0XG4gICAqL1xuICBnZXQgcmlwcGxlQ29uZmlnKCk6IFJpcHBsZUNvbmZpZyB7XG4gICAgcmV0dXJuIHtcbiAgICAgIGNlbnRlcmVkOiB0aGlzLmNlbnRlcmVkLFxuICAgICAgcmFkaXVzOiB0aGlzLnJhZGl1cyxcbiAgICAgIGNvbG9yOiB0aGlzLmNvbG9yLFxuICAgICAgYW5pbWF0aW9uOiB7XG4gICAgICAgIC4uLnRoaXMuX2dsb2JhbE9wdGlvbnMuYW5pbWF0aW9uLFxuICAgICAgICAuLi4odGhpcy5fYW5pbWF0aW9uTW9kZSA9PT0gJ05vb3BBbmltYXRpb25zJyA/IHtlbnRlckR1cmF0aW9uOiAwLCBleGl0RHVyYXRpb246IDB9IDoge30pLFxuICAgICAgICAuLi50aGlzLmFuaW1hdGlvblxuICAgICAgfSxcbiAgICAgIHRlcm1pbmF0ZU9uUG9pbnRlclVwOiB0aGlzLl9nbG9iYWxPcHRpb25zLnRlcm1pbmF0ZU9uUG9pbnRlclVwLFxuICAgIH07XG4gIH1cblxuICAvKipcbiAgICogV2hldGhlciByaXBwbGVzIG9uIHBvaW50ZXItZG93biBhcmUgZGlzYWJsZWQgb3Igbm90LlxuICAgKiBAZG9jcy1wcml2YXRlIEltcGxlbWVudGVkIGFzIHBhcnQgb2YgUmlwcGxlVGFyZ2V0XG4gICAqL1xuICBnZXQgcmlwcGxlRGlzYWJsZWQoKTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIHRoaXMuZGlzYWJsZWQgfHwgISF0aGlzLl9nbG9iYWxPcHRpb25zLmRpc2FibGVkO1xuICB9XG5cbiAgLyoqIFNldHMgdXAgdGhlIHRyaWdnZXIgZXZlbnQgbGlzdGVuZXJzIGlmIHJpcHBsZXMgYXJlIGVuYWJsZWQuICovXG4gIHByaXZhdGUgX3NldHVwVHJpZ2dlckV2ZW50c0lmRW5hYmxlZCgpIHtcbiAgICBpZiAoIXRoaXMuZGlzYWJsZWQgJiYgdGhpcy5faXNJbml0aWFsaXplZCkge1xuICAgICAgdGhpcy5fcmlwcGxlUmVuZGVyZXIuc2V0dXBUcmlnZ2VyRXZlbnRzKHRoaXMudHJpZ2dlcik7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIExhdW5jaGVzIGEgbWFudWFsIHJpcHBsZSB1c2luZyB0aGUgc3BlY2lmaWVkIHJpcHBsZSBjb25maWd1cmF0aW9uLlxuICAgKiBAcGFyYW0gY29uZmlnIENvbmZpZ3VyYXRpb24gZm9yIHRoZSBtYW51YWwgcmlwcGxlLlxuICAgKi9cbiAgbGF1bmNoKGNvbmZpZzogUmlwcGxlQ29uZmlnKTogUmlwcGxlUmVmO1xuXG4gIC8qKlxuICAgKiBMYXVuY2hlcyBhIG1hbnVhbCByaXBwbGUgYXQgdGhlIHNwZWNpZmllZCBjb29yZGluYXRlcyB3aXRoaW4gdGhlIGVsZW1lbnQuXG4gICAqIEBwYXJhbSB4IENvb3JkaW5hdGUgd2l0aGluIHRoZSBlbGVtZW50LCBhbG9uZyB0aGUgWCBheGlzIGF0IHdoaWNoIHRvIGZhZGUtaW4gdGhlIHJpcHBsZS5cbiAgICogQHBhcmFtIHkgQ29vcmRpbmF0ZSB3aXRoaW4gdGhlIGVsZW1lbnQsIGFsb25nIHRoZSBZIGF4aXMgYXQgd2hpY2ggdG8gZmFkZS1pbiB0aGUgcmlwcGxlLlxuICAgKiBAcGFyYW0gY29uZmlnIE9wdGlvbmFsIHJpcHBsZSBjb25maWd1cmF0aW9uIGZvciB0aGUgbWFudWFsIHJpcHBsZS5cbiAgICovXG4gIGxhdW5jaCh4OiBudW1iZXIsIHk6IG51bWJlciwgY29uZmlnPzogUmlwcGxlQ29uZmlnKTogUmlwcGxlUmVmO1xuXG4gIC8qKiBMYXVuY2hlcyBhIG1hbnVhbCByaXBwbGUgYXQgdGhlIHNwZWNpZmllZCBjb29yZGluYXRlZCBvciBqdXN0IGJ5IHRoZSByaXBwbGUgY29uZmlnLiAqL1xuICBsYXVuY2goY29uZmlnT3JYOiBudW1iZXIgfCBSaXBwbGVDb25maWcsIHk6IG51bWJlciA9IDAsIGNvbmZpZz86IFJpcHBsZUNvbmZpZyk6IFJpcHBsZVJlZiB7XG4gICAgaWYgKHR5cGVvZiBjb25maWdPclggPT09ICdudW1iZXInKSB7XG4gICAgICByZXR1cm4gdGhpcy5fcmlwcGxlUmVuZGVyZXIuZmFkZUluUmlwcGxlKGNvbmZpZ09yWCwgeSwgey4uLnRoaXMucmlwcGxlQ29uZmlnLCAuLi5jb25maWd9KTtcbiAgICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIHRoaXMuX3JpcHBsZVJlbmRlcmVyLmZhZGVJblJpcHBsZSgwLCAwLCB7Li4udGhpcy5yaXBwbGVDb25maWcsIC4uLmNvbmZpZ09yWH0pO1xuICAgIH1cbiAgfVxufVxuXG4iXX0=