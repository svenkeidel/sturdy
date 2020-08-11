/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Component, ChangeDetectorRef, Input, Inject, Output, EventEmitter, ElementRef, Directive, Optional, ViewEncapsulation, ChangeDetectionStrategy, ComponentFactoryResolver, ViewContainerRef, forwardRef, ViewChild, } from '@angular/core';
import { TemplatePortal, CdkPortalOutlet, PortalHostDirective } from '@angular/cdk/portal';
import { Directionality } from '@angular/cdk/bidi';
import { DOCUMENT } from '@angular/common';
import { Subscription, Subject } from 'rxjs';
import { matTabsAnimations } from './tabs-animations';
import { startWith, distinctUntilChanged } from 'rxjs/operators';
/**
 * The portal host directive for the contents of the tab.
 * @docs-private
 */
let MatTabBodyPortal = /** @class */ (() => {
    class MatTabBodyPortal extends CdkPortalOutlet {
        constructor(componentFactoryResolver, viewContainerRef, _host, 
        /**
         * @deprecated `_document` parameter to be made required.
         * @breaking-change 9.0.0
         */
        _document) {
            super(componentFactoryResolver, viewContainerRef, _document);
            this._host = _host;
            /** Subscription to events for when the tab body begins centering. */
            this._centeringSub = Subscription.EMPTY;
            /** Subscription to events for when the tab body finishes leaving from center position. */
            this._leavingSub = Subscription.EMPTY;
        }
        /** Set initial visibility or set up subscription for changing visibility. */
        ngOnInit() {
            super.ngOnInit();
            this._centeringSub = this._host._beforeCentering
                .pipe(startWith(this._host._isCenterPosition(this._host._position)))
                .subscribe((isCentering) => {
                if (isCentering && !this.hasAttached()) {
                    this.attach(this._host._content);
                }
            });
            this._leavingSub = this._host._afterLeavingCenter.subscribe(() => {
                this.detach();
            });
        }
        /** Clean up centering subscription. */
        ngOnDestroy() {
            super.ngOnDestroy();
            this._centeringSub.unsubscribe();
            this._leavingSub.unsubscribe();
        }
    }
    MatTabBodyPortal.decorators = [
        { type: Directive, args: [{
                    selector: '[matTabBodyHost]'
                },] }
    ];
    MatTabBodyPortal.ctorParameters = () => [
        { type: ComponentFactoryResolver },
        { type: ViewContainerRef },
        { type: MatTabBody, decorators: [{ type: Inject, args: [forwardRef(() => MatTabBody),] }] },
        { type: undefined, decorators: [{ type: Inject, args: [DOCUMENT,] }] }
    ];
    return MatTabBodyPortal;
})();
export { MatTabBodyPortal };
/**
 * Base class with all of the `MatTabBody` functionality.
 * @docs-private
 */
let _MatTabBodyBase = /** @class */ (() => {
    class _MatTabBodyBase {
        constructor(_elementRef, _dir, changeDetectorRef) {
            this._elementRef = _elementRef;
            this._dir = _dir;
            /** Subscription to the directionality change observable. */
            this._dirChangeSubscription = Subscription.EMPTY;
            /** Emits when an animation on the tab is complete. */
            this._translateTabComplete = new Subject();
            /** Event emitted when the tab begins to animate towards the center as the active tab. */
            this._onCentering = new EventEmitter();
            /** Event emitted before the centering of the tab begins. */
            this._beforeCentering = new EventEmitter();
            /** Event emitted before the centering of the tab begins. */
            this._afterLeavingCenter = new EventEmitter();
            /** Event emitted when the tab completes its animation towards the center. */
            this._onCentered = new EventEmitter(true);
            // Note that the default value will always be overwritten by `MatTabBody`, but we need one
            // anyway to prevent the animations module from throwing an error if the body is used on its own.
            /** Duration for the tab's animation. */
            this.animationDuration = '500ms';
            if (_dir) {
                this._dirChangeSubscription = _dir.change.subscribe((dir) => {
                    this._computePositionAnimationState(dir);
                    changeDetectorRef.markForCheck();
                });
            }
            // Ensure that we get unique animation events, because the `.done` callback can get
            // invoked twice in some browsers. See https://github.com/angular/angular/issues/24084.
            this._translateTabComplete.pipe(distinctUntilChanged((x, y) => {
                return x.fromState === y.fromState && x.toState === y.toState;
            })).subscribe(event => {
                // If the transition to the center is complete, emit an event.
                if (this._isCenterPosition(event.toState) && this._isCenterPosition(this._position)) {
                    this._onCentered.emit();
                }
                if (this._isCenterPosition(event.fromState) && !this._isCenterPosition(this._position)) {
                    this._afterLeavingCenter.emit();
                }
            });
        }
        /** The shifted index position of the tab body, where zero represents the active center tab. */
        set position(position) {
            this._positionIndex = position;
            this._computePositionAnimationState();
        }
        /**
         * After initialized, check if the content is centered and has an origin. If so, set the
         * special position states that transition the tab from the left or right before centering.
         */
        ngOnInit() {
            if (this._position == 'center' && this.origin != null) {
                this._position = this._computePositionFromOrigin(this.origin);
            }
        }
        ngOnDestroy() {
            this._dirChangeSubscription.unsubscribe();
            this._translateTabComplete.complete();
        }
        _onTranslateTabStarted(event) {
            const isCentering = this._isCenterPosition(event.toState);
            this._beforeCentering.emit(isCentering);
            if (isCentering) {
                this._onCentering.emit(this._elementRef.nativeElement.clientHeight);
            }
        }
        /** The text direction of the containing app. */
        _getLayoutDirection() {
            return this._dir && this._dir.value === 'rtl' ? 'rtl' : 'ltr';
        }
        /** Whether the provided position state is considered center, regardless of origin. */
        _isCenterPosition(position) {
            return position == 'center' ||
                position == 'left-origin-center' ||
                position == 'right-origin-center';
        }
        /** Computes the position state that will be used for the tab-body animation trigger. */
        _computePositionAnimationState(dir = this._getLayoutDirection()) {
            if (this._positionIndex < 0) {
                this._position = dir == 'ltr' ? 'left' : 'right';
            }
            else if (this._positionIndex > 0) {
                this._position = dir == 'ltr' ? 'right' : 'left';
            }
            else {
                this._position = 'center';
            }
        }
        /**
         * Computes the position state based on the specified origin position. This is used if the
         * tab is becoming visible immediately after creation.
         */
        _computePositionFromOrigin(origin) {
            const dir = this._getLayoutDirection();
            if ((dir == 'ltr' && origin <= 0) || (dir == 'rtl' && origin > 0)) {
                return 'left-origin-center';
            }
            return 'right-origin-center';
        }
    }
    _MatTabBodyBase.decorators = [
        { type: Directive }
    ];
    _MatTabBodyBase.ctorParameters = () => [
        { type: ElementRef },
        { type: Directionality, decorators: [{ type: Optional }] },
        { type: ChangeDetectorRef }
    ];
    _MatTabBodyBase.propDecorators = {
        _onCentering: [{ type: Output }],
        _beforeCentering: [{ type: Output }],
        _afterLeavingCenter: [{ type: Output }],
        _onCentered: [{ type: Output }],
        _content: [{ type: Input, args: ['content',] }],
        origin: [{ type: Input }],
        animationDuration: [{ type: Input }],
        position: [{ type: Input }]
    };
    return _MatTabBodyBase;
})();
export { _MatTabBodyBase };
/**
 * Wrapper for the contents of a tab.
 * @docs-private
 */
let MatTabBody = /** @class */ (() => {
    class MatTabBody extends _MatTabBodyBase {
        constructor(elementRef, dir, changeDetectorRef) {
            super(elementRef, dir, changeDetectorRef);
        }
    }
    MatTabBody.decorators = [
        { type: Component, args: [{
                    selector: 'mat-tab-body',
                    template: "<div class=\"mat-tab-body-content\" #content\n     [@translateTab]=\"{\n        value: _position,\n        params: {animationDuration: animationDuration}\n     }\"\n     (@translateTab.start)=\"_onTranslateTabStarted($event)\"\n     (@translateTab.done)=\"_translateTabComplete.next($event)\">\n  <ng-template matTabBodyHost></ng-template>\n</div>\n",
                    encapsulation: ViewEncapsulation.None,
                    // tslint:disable-next-line:validate-decorators
                    changeDetection: ChangeDetectionStrategy.Default,
                    animations: [matTabsAnimations.translateTab],
                    host: {
                        'class': 'mat-tab-body',
                    },
                    styles: [".mat-tab-body-content{height:100%;overflow:auto}.mat-tab-group-dynamic-height .mat-tab-body-content{overflow:hidden}\n"]
                },] }
    ];
    MatTabBody.ctorParameters = () => [
        { type: ElementRef },
        { type: Directionality, decorators: [{ type: Optional }] },
        { type: ChangeDetectorRef }
    ];
    MatTabBody.propDecorators = {
        _portalHost: [{ type: ViewChild, args: [PortalHostDirective,] }]
    };
    return MatTabBody;
})();
export { MatTabBody };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidGFiLWJvZHkuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvdGFicy90YWItYm9keS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQ0wsU0FBUyxFQUNULGlCQUFpQixFQUNqQixLQUFLLEVBQ0wsTUFBTSxFQUNOLE1BQU0sRUFDTixZQUFZLEVBR1osVUFBVSxFQUNWLFNBQVMsRUFDVCxRQUFRLEVBQ1IsaUJBQWlCLEVBQ2pCLHVCQUF1QixFQUN2Qix3QkFBd0IsRUFDeEIsZ0JBQWdCLEVBQ2hCLFVBQVUsRUFDVixTQUFTLEdBQ1YsTUFBTSxlQUFlLENBQUM7QUFFdkIsT0FBTyxFQUFDLGNBQWMsRUFBRSxlQUFlLEVBQUUsbUJBQW1CLEVBQUMsTUFBTSxxQkFBcUIsQ0FBQztBQUN6RixPQUFPLEVBQUMsY0FBYyxFQUFZLE1BQU0sbUJBQW1CLENBQUM7QUFDNUQsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLGlCQUFpQixDQUFDO0FBQ3pDLE9BQU8sRUFBQyxZQUFZLEVBQUUsT0FBTyxFQUFDLE1BQU0sTUFBTSxDQUFDO0FBQzNDLE9BQU8sRUFBQyxpQkFBaUIsRUFBQyxNQUFNLG1CQUFtQixDQUFDO0FBQ3BELE9BQU8sRUFBQyxTQUFTLEVBQUUsb0JBQW9CLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQXVCL0Q7OztHQUdHO0FBQ0g7SUFBQSxNQUdhLGdCQUFpQixTQUFRLGVBQWU7UUFNbkQsWUFDRSx3QkFBa0QsRUFDbEQsZ0JBQWtDLEVBQ1ksS0FBaUI7UUFDL0Q7OztXQUdHO1FBQ2UsU0FBZTtZQUNqQyxLQUFLLENBQUMsd0JBQXdCLEVBQUUsZ0JBQWdCLEVBQUUsU0FBUyxDQUFDLENBQUM7WUFOZixVQUFLLEdBQUwsS0FBSyxDQUFZO1lBUmpFLHFFQUFxRTtZQUM3RCxrQkFBYSxHQUFHLFlBQVksQ0FBQyxLQUFLLENBQUM7WUFDM0MsMEZBQTBG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aUJBQUM7S0FBQTtTQVJZLFVBQVUiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtcbiAgQ29tcG9uZW50LFxuICBDaGFuZ2VEZXRlY3RvclJlZixcbiAgSW5wdXQsXG4gIEluamVjdCxcbiAgT3V0cHV0LFxuICBFdmVudEVtaXR0ZXIsXG4gIE9uRGVzdHJveSxcbiAgT25Jbml0LFxuICBFbGVtZW50UmVmLFxuICBEaXJlY3RpdmUsXG4gIE9wdGlvbmFsLFxuICBWaWV3RW5jYXBzdWxhdGlvbixcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksXG4gIENvbXBvbmVudEZhY3RvcnlSZXNvbHZlcixcbiAgVmlld0NvbnRhaW5lclJlZixcbiAgZm9yd2FyZFJlZixcbiAgVmlld0NoaWxkLFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7QW5pbWF0aW9uRXZlbnR9IGZyb20gJ0Bhbmd1bGFyL2FuaW1hdGlvbnMnO1xuaW1wb3J0IHtUZW1wbGF0ZVBvcnRhbCwgQ2RrUG9ydGFsT3V0bGV0LCBQb3J0YWxIb3N0RGlyZWN0aXZlfSBmcm9tICdAYW5ndWxhci9jZGsvcG9ydGFsJztcbmltcG9ydCB7RGlyZWN0aW9uYWxpdHksIERpcmVjdGlvbn0gZnJvbSAnQGFuZ3VsYXIvY2RrL2JpZGknO1xuaW1wb3J0IHtET0NVTUVOVH0gZnJvbSAnQGFuZ3VsYXIvY29tbW9uJztcbmltcG9ydCB7U3Vic2NyaXB0aW9uLCBTdWJqZWN0fSBmcm9tICdyeGpzJztcbmltcG9ydCB7bWF0VGFic0FuaW1hdGlvbnN9IGZyb20gJy4vdGFicy1hbmltYXRpb25zJztcbmltcG9ydCB7c3RhcnRXaXRoLCBkaXN0aW5jdFVudGlsQ2hhbmdlZH0gZnJvbSAncnhqcy9vcGVyYXRvcnMnO1xuXG4vKipcbiAqIFRoZXNlIHBvc2l0aW9uIHN0YXRlcyBhcmUgdXNlZCBpbnRlcm5hbGx5IGFzIGFuaW1hdGlvbiBzdGF0ZXMgZm9yIHRoZSB0YWIgYm9keS4gU2V0dGluZyB0aGVcbiAqIHBvc2l0aW9uIHN0YXRlIHRvIGxlZnQsIHJpZ2h0LCBvciBjZW50ZXIgd2lsbCB0cmFuc2l0aW9uIHRoZSB0YWIgYm9keSBmcm9tIGl0cyBjdXJyZW50XG4gKiBwb3NpdGlvbiB0byBpdHMgcmVzcGVjdGl2ZSBzdGF0ZS4gSWYgdGhlcmUgaXMgbm90IGN1cnJlbnQgcG9zaXRpb24gKHZvaWQsIGluIHRoZSBjYXNlIG9mIGEgbmV3XG4gKiB0YWIgYm9keSksIHRoZW4gdGhlcmUgd2lsbCBiZSBubyB0cmFuc2l0aW9uIGFuaW1hdGlvbiB0byBpdHMgc3RhdGUuXG4gKlxuICogSW4gdGhlIGNhc2Ugb2YgYSBuZXcgdGFiIGJvZHkgdGhhdCBzaG91bGQgaW1tZWRpYXRlbHkgYmUgY2VudGVyZWQgd2l0aCBhbiBhbmltYXRpbmcgdHJhbnNpdGlvbixcbiAqIHRoZW4gbGVmdC1vcmlnaW4tY2VudGVyIG9yIHJpZ2h0LW9yaWdpbi1jZW50ZXIgY2FuIGJlIHVzZWQsIHdoaWNoIHdpbGwgdXNlIGxlZnQgb3IgcmlnaHQgYXMgaXRzXG4gKiBwc3VlZG8tcHJpb3Igc3RhdGUuXG4gKi9cbmV4cG9ydCB0eXBlIE1hdFRhYkJvZHlQb3NpdGlvblN0YXRlID1cbiAgICAnbGVmdCcgfCAnY2VudGVyJyB8ICdyaWdodCcgfCAnbGVmdC1vcmlnaW4tY2VudGVyJyB8ICdyaWdodC1vcmlnaW4tY2VudGVyJztcblxuLyoqXG4gKiBUaGUgb3JpZ2luIHN0YXRlIGlzIGFuIGludGVybmFsbHkgdXNlZCBzdGF0ZSB0aGF0IGlzIHNldCBvbiBhIG5ldyB0YWIgYm9keSBpbmRpY2F0aW5nIGlmIGl0XG4gKiBiZWdhbiB0byB0aGUgbGVmdCBvciByaWdodCBvZiB0aGUgcHJpb3Igc2VsZWN0ZWQgaW5kZXguIEZvciBleGFtcGxlLCBpZiB0aGUgc2VsZWN0ZWQgaW5kZXggd2FzXG4gKiBzZXQgdG8gMSwgYW5kIGEgbmV3IHRhYiBpcyBjcmVhdGVkIGFuZCBzZWxlY3RlZCBhdCBpbmRleCAyLCB0aGVuIHRoZSB0YWIgYm9keSB3b3VsZCBoYXZlIGFuXG4gKiBvcmlnaW4gb2YgcmlnaHQgYmVjYXVzZSBpdHMgaW5kZXggd2FzIGdyZWF0ZXIgdGhhbiB0aGUgcHJpb3Igc2VsZWN0ZWQgaW5kZXguXG4gKi9cbmV4cG9ydCB0eXBlIE1hdFRhYkJvZHlPcmlnaW5TdGF0ZSA9ICdsZWZ0JyB8ICdyaWdodCc7XG5cbi8qKlxuICogVGhlIHBvcnRhbCBob3N0IGRpcmVjdGl2ZSBmb3IgdGhlIGNvbnRlbnRzIG9mIHRoZSB0YWIuXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogJ1ttYXRUYWJCb2R5SG9zdF0nXG59KVxuZXhwb3J0IGNsYXNzIE1hdFRhYkJvZHlQb3J0YWwgZXh0ZW5kcyBDZGtQb3J0YWxPdXRsZXQgaW1wbGVtZW50cyBPbkluaXQsIE9uRGVzdHJveSB7XG4gIC8qKiBTdWJzY3JpcHRpb24gdG8gZXZlbnRzIGZvciB3aGVuIHRoZSB0YWIgYm9keSBiZWdpbnMgY2VudGVyaW5nLiAqL1xuICBwcml2YXRlIF9jZW50ZXJpbmdTdWIgPSBTdWJzY3JpcHRpb24uRU1QVFk7XG4gIC8qKiBTdWJzY3JpcHRpb24gdG8gZXZlbnRzIGZvciB3aGVuIHRoZSB0YWIgYm9keSBmaW5pc2hlcyBsZWF2aW5nIGZyb20gY2VudGVyIHBvc2l0aW9uLiAqL1xuICBwcml2YXRlIF9sZWF2aW5nU3ViID0gU3Vic2NyaXB0aW9uLkVNUFRZO1xuXG4gIGNvbnN0cnVjdG9yKFxuICAgIGNvbXBvbmVudEZhY3RvcnlSZXNvbHZlcjogQ29tcG9uZW50RmFjdG9yeVJlc29sdmVyLFxuICAgIHZpZXdDb250YWluZXJSZWY6IFZpZXdDb250YWluZXJSZWYsXG4gICAgQEluamVjdChmb3J3YXJkUmVmKCgpID0+IE1hdFRhYkJvZHkpKSBwcml2YXRlIF9ob3N0OiBNYXRUYWJCb2R5LFxuICAgIC8qKlxuICAgICAqIEBkZXByZWNhdGVkIGBfZG9jdW1lbnRgIHBhcmFtZXRlciB0byBiZSBtYWRlIHJlcXVpcmVkLlxuICAgICAqIEBicmVha2luZy1jaGFuZ2UgOS4wLjBcbiAgICAgKi9cbiAgICBASW5qZWN0KERPQ1VNRU5UKSBfZG9jdW1lbnQ/OiBhbnkpIHtcbiAgICBzdXBlcihjb21wb25lbnRGYWN0b3J5UmVzb2x2ZXIsIHZpZXdDb250YWluZXJSZWYsIF9kb2N1bWVudCk7XG4gIH1cblxuICAvKiogU2V0IGluaXRpYWwgdmlzaWJpbGl0eSBvciBzZXQgdXAgc3Vic2NyaXB0aW9uIGZvciBjaGFuZ2luZyB2aXNpYmlsaXR5LiAqL1xuICBuZ09uSW5pdCgpOiB2b2lkIHtcbiAgICBzdXBlci5uZ09uSW5pdCgpO1xuXG4gICAgdGhpcy5fY2VudGVyaW5nU3ViID0gdGhpcy5faG9zdC5fYmVmb3JlQ2VudGVyaW5nXG4gICAgICAucGlwZShzdGFydFdpdGgodGhpcy5faG9zdC5faXNDZW50ZXJQb3NpdGlvbih0aGlzLl9ob3N0Ll9wb3NpdGlvbikpKVxuICAgICAgLnN1YnNjcmliZSgoaXNDZW50ZXJpbmc6IGJvb2xlYW4pID0+IHtcbiAgICAgICAgaWYgKGlzQ2VudGVyaW5nICYmICF0aGlzLmhhc0F0dGFjaGVkKCkpIHtcbiAgICAgICAgICB0aGlzLmF0dGFjaCh0aGlzLl9ob3N0Ll9jb250ZW50KTtcbiAgICAgICAgfVxuICAgICAgfSk7XG5cbiAgICB0aGlzLl9sZWF2aW5nU3ViID0gdGhpcy5faG9zdC5fYWZ0ZXJMZWF2aW5nQ2VudGVyLnN1YnNjcmliZSgoKSA9PiB7XG4gICAgICB0aGlzLmRldGFjaCgpO1xuICAgIH0pO1xuICB9XG5cbiAgLyoqIENsZWFuIHVwIGNlbnRlcmluZyBzdWJzY3JpcHRpb24uICovXG4gIG5nT25EZXN0cm95KCk6IHZvaWQge1xuICAgIHN1cGVyLm5nT25EZXN0cm95KCk7XG4gICAgdGhpcy5fY2VudGVyaW5nU3ViLnVuc3Vic2NyaWJlKCk7XG4gICAgdGhpcy5fbGVhdmluZ1N1Yi51bnN1YnNjcmliZSgpO1xuICB9XG59XG5cbi8qKlxuICogQmFzZSBjbGFzcyB3aXRoIGFsbCBvZiB0aGUgYE1hdFRhYkJvZHlgIGZ1bmN0aW9uYWxpdHkuXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbkBEaXJlY3RpdmUoKVxuLy8gdHNsaW50OmRpc2FibGUtbmV4dC1saW5lOmNsYXNzLW5hbWVcbmV4cG9ydCBhYnN0cmFjdCBjbGFzcyBfTWF0VGFiQm9keUJhc2UgaW1wbGVtZW50cyBPbkluaXQsIE9uRGVzdHJveSB7XG4gIC8qKiBDdXJyZW50IHBvc2l0aW9uIG9mIHRoZSB0YWItYm9keSBpbiB0aGUgdGFiLWdyb3VwLiBaZXJvIG1lYW5zIHRoYXQgdGhlIHRhYiBpcyB2aXNpYmxlLiAqL1xuICBwcml2YXRlIF9wb3NpdGlvbkluZGV4OiBudW1iZXI7XG5cbiAgLyoqIFN1YnNjcmlwdGlvbiB0byB0aGUgZGlyZWN0aW9uYWxpdHkgY2hhbmdlIG9ic2VydmFibGUuICovXG4gIHByaXZhdGUgX2RpckNoYW5nZVN1YnNjcmlwdGlvbiA9IFN1YnNjcmlwdGlvbi5FTVBUWTtcblxuICAvKiogVGFiIGJvZHkgcG9zaXRpb24gc3RhdGUuIFVzZWQgYnkgdGhlIGFuaW1hdGlvbiB0cmlnZ2VyIGZvciB0aGUgY3VycmVudCBzdGF0ZS4gKi9cbiAgX3Bvc2l0aW9uOiBNYXRUYWJCb2R5UG9zaXRpb25TdGF0ZTtcblxuICAvKiogRW1pdHMgd2hlbiBhbiBhbmltYXRpb24gb24gdGhlIHRhYiBpcyBjb21wbGV0ZS4gKi9cbiAgX3RyYW5zbGF0ZVRhYkNvbXBsZXRlID0gbmV3IFN1YmplY3Q8QW5pbWF0aW9uRXZlbnQ+KCk7XG5cbiAgLyoqIEV2ZW50IGVtaXR0ZWQgd2hlbiB0aGUgdGFiIGJlZ2lucyB0byBhbmltYXRlIHRvd2FyZHMgdGhlIGNlbnRlciBhcyB0aGUgYWN0aXZlIHRhYi4gKi9cbiAgQE91dHB1dCgpIHJlYWRvbmx5IF9vbkNlbnRlcmluZzogRXZlbnRFbWl0dGVyPG51bWJlcj4gPSBuZXcgRXZlbnRFbWl0dGVyPG51bWJlcj4oKTtcblxuICAvKiogRXZlbnQgZW1pdHRlZCBiZWZvcmUgdGhlIGNlbnRlcmluZyBvZiB0aGUgdGFiIGJlZ2lucy4gKi9cbiAgQE91dHB1dCgpIHJlYWRvbmx5IF9iZWZvcmVDZW50ZXJpbmc6IEV2ZW50RW1pdHRlcjxib29sZWFuPiA9IG5ldyBFdmVudEVtaXR0ZXI8Ym9vbGVhbj4oKTtcblxuICAvKiogRXZlbnQgZW1pdHRlZCBiZWZvcmUgdGhlIGNlbnRlcmluZyBvZiB0aGUgdGFiIGJlZ2lucy4gKi9cbiAgQE91dHB1dCgpIHJlYWRvbmx5IF9hZnRlckxlYXZpbmdDZW50ZXI6IEV2ZW50RW1pdHRlcjx2b2lkPiA9IG5ldyBFdmVudEVtaXR0ZXI8dm9pZD4oKTtcblxuICAvKiogRXZlbnQgZW1pdHRlZCB3aGVuIHRoZSB0YWIgY29tcGxldGVzIGl0cyBhbmltYXRpb24gdG93YXJkcyB0aGUgY2VudGVyLiAqL1xuICBAT3V0cHV0KCkgcmVhZG9ubHkgX29uQ2VudGVyZWQ6IEV2ZW50RW1pdHRlcjx2b2lkPiA9IG5ldyBFdmVudEVtaXR0ZXI8dm9pZD4odHJ1ZSk7XG5cbiAgIC8qKiBUaGUgcG9ydGFsIGhvc3QgaW5zaWRlIG9mIHRoaXMgY29udGFpbmVyIGludG8gd2hpY2ggdGhlIHRhYiBib2R5IGNvbnRlbnQgd2lsbCBiZSBsb2FkZWQuICovXG4gIGFic3RyYWN0IF9wb3J0YWxIb3N0OiBQb3J0YWxIb3N0RGlyZWN0aXZlO1xuXG4gIC8qKiBUaGUgdGFiIGJvZHkgY29udGVudCB0byBkaXNwbGF5LiAqL1xuICBASW5wdXQoJ2NvbnRlbnQnKSBfY29udGVudDogVGVtcGxhdGVQb3J0YWw7XG5cbiAgLyoqIFBvc2l0aW9uIHRoYXQgd2lsbCBiZSB1c2VkIHdoZW4gdGhlIHRhYiBpcyBpbW1lZGlhdGVseSBiZWNvbWluZyB2aXNpYmxlIGFmdGVyIGNyZWF0aW9uLiAqL1xuICBASW5wdXQoKSBvcmlnaW46IG51bWJlciB8IG51bGw7XG5cbiAgLy8gTm90ZSB0aGF0IHRoZSBkZWZhdWx0IHZhbHVlIHdpbGwgYWx3YXlzIGJlIG92ZXJ3cml0dGVuIGJ5IGBNYXRUYWJCb2R5YCwgYnV0IHdlIG5lZWQgb25lXG4gIC8vIGFueXdheSB0byBwcmV2ZW50IHRoZSBhbmltYXRpb25zIG1vZHVsZSBmcm9tIHRocm93aW5nIGFuIGVycm9yIGlmIHRoZSBib2R5IGlzIHVzZWQgb24gaXRzIG93bi5cbiAgLyoqIER1cmF0aW9uIGZvciB0aGUgdGFiJ3MgYW5pbWF0aW9uLiAqL1xuICBASW5wdXQoKSBhbmltYXRpb25EdXJhdGlvbjogc3RyaW5nID0gJzUwMG1zJztcblxuICAvKiogVGhlIHNoaWZ0ZWQgaW5kZXggcG9zaXRpb24gb2YgdGhlIHRhYiBib2R5LCB3aGVyZSB6ZXJvIHJlcHJlc2VudHMgdGhlIGFjdGl2ZSBjZW50ZXIgdGFiLiAqL1xuICBASW5wdXQoKVxuICBzZXQgcG9zaXRpb24ocG9zaXRpb246IG51bWJlcikge1xuICAgIHRoaXMuX3Bvc2l0aW9uSW5kZXggPSBwb3NpdGlvbjtcbiAgICB0aGlzLl9jb21wdXRlUG9zaXRpb25BbmltYXRpb25TdGF0ZSgpO1xuICB9XG5cbiAgY29uc3RydWN0b3IocHJpdmF0ZSBfZWxlbWVudFJlZjogRWxlbWVudFJlZjxIVE1MRWxlbWVudD4sXG4gICAgICAgICAgICAgIEBPcHRpb25hbCgpIHByaXZhdGUgX2RpcjogRGlyZWN0aW9uYWxpdHksXG4gICAgICAgICAgICAgIGNoYW5nZURldGVjdG9yUmVmOiBDaGFuZ2VEZXRlY3RvclJlZikge1xuXG4gICAgaWYgKF9kaXIpIHtcbiAgICAgIHRoaXMuX2RpckNoYW5nZVN1YnNjcmlwdGlvbiA9IF9kaXIuY2hhbmdlLnN1YnNjcmliZSgoZGlyOiBEaXJlY3Rpb24pID0+IHtcbiAgICAgICAgdGhpcy5fY29tcHV0ZVBvc2l0aW9uQW5pbWF0aW9uU3RhdGUoZGlyKTtcbiAgICAgICAgY2hhbmdlRGV0ZWN0b3JSZWYubWFya0ZvckNoZWNrKCk7XG4gICAgICB9KTtcbiAgICB9XG5cbiAgICAvLyBFbnN1cmUgdGhhdCB3ZSBnZXQgdW5pcXVlIGFuaW1hdGlvbiBldmVudHMsIGJlY2F1c2UgdGhlIGAuZG9uZWAgY2FsbGJhY2sgY2FuIGdldFxuICAgIC8vIGludm9rZWQgdHdpY2UgaW4gc29tZSBicm93c2Vycy4gU2VlIGh0dHBzOi8vZ2l0aHViLmNvbS9hbmd1bGFyL2FuZ3VsYXIvaXNzdWVzLzI0MDg0LlxuICAgIHRoaXMuX3RyYW5zbGF0ZVRhYkNvbXBsZXRlLnBpcGUoZGlzdGluY3RVbnRpbENoYW5nZWQoKHgsIHkpID0+IHtcbiAgICAgIHJldHVybiB4LmZyb21TdGF0ZSA9PT0geS5mcm9tU3RhdGUgJiYgeC50b1N0YXRlID09PSB5LnRvU3RhdGU7XG4gICAgfSkpLnN1YnNjcmliZShldmVudCA9PiB7XG4gICAgICAvLyBJZiB0aGUgdHJhbnNpdGlvbiB0byB0aGUgY2VudGVyIGlzIGNvbXBsZXRlLCBlbWl0IGFuIGV2ZW50LlxuICAgICAgaWYgKHRoaXMuX2lzQ2VudGVyUG9zaXRpb24oZXZlbnQudG9TdGF0ZSkgJiYgdGhpcy5faXNDZW50ZXJQb3NpdGlvbih0aGlzLl9wb3NpdGlvbikpIHtcbiAgICAgICAgdGhpcy5fb25DZW50ZXJlZC5lbWl0KCk7XG4gICAgICB9XG5cbiAgICAgIGlmICh0aGlzLl9pc0NlbnRlclBvc2l0aW9uKGV2ZW50LmZyb21TdGF0ZSkgJiYgIXRoaXMuX2lzQ2VudGVyUG9zaXRpb24odGhpcy5fcG9zaXRpb24pKSB7XG4gICAgICAgIHRoaXMuX2FmdGVyTGVhdmluZ0NlbnRlci5lbWl0KCk7XG4gICAgICB9XG4gICAgfSk7XG4gIH1cblxuICAvKipcbiAgICogQWZ0ZXIgaW5pdGlhbGl6ZWQsIGNoZWNrIGlmIHRoZSBjb250ZW50IGlzIGNlbnRlcmVkIGFuZCBoYXMgYW4gb3JpZ2luLiBJZiBzbywgc2V0IHRoZVxuICAgKiBzcGVjaWFsIHBvc2l0aW9uIHN0YXRlcyB0aGF0IHRyYW5zaXRpb24gdGhlIHRhYiBmcm9tIHRoZSBsZWZ0IG9yIHJpZ2h0IGJlZm9yZSBjZW50ZXJpbmcuXG4gICAqL1xuICBuZ09uSW5pdCgpIHtcbiAgICBpZiAodGhpcy5fcG9zaXRpb24gPT0gJ2NlbnRlcicgJiYgdGhpcy5vcmlnaW4gIT0gbnVsbCkge1xuICAgICAgdGhpcy5fcG9zaXRpb24gPSB0aGlzLl9jb21wdXRlUG9zaXRpb25Gcm9tT3JpZ2luKHRoaXMub3JpZ2luKTtcbiAgICB9XG4gIH1cblxuICBuZ09uRGVzdHJveSgpIHtcbiAgICB0aGlzLl9kaXJDaGFuZ2VTdWJzY3JpcHRpb24udW5zdWJzY3JpYmUoKTtcbiAgICB0aGlzLl90cmFuc2xhdGVUYWJDb21wbGV0ZS5jb21wbGV0ZSgpO1xuICB9XG5cbiAgX29uVHJhbnNsYXRlVGFiU3RhcnRlZChldmVudDogQW5pbWF0aW9uRXZlbnQpOiB2b2lkIHtcbiAgICBjb25zdCBpc0NlbnRlcmluZyA9IHRoaXMuX2lzQ2VudGVyUG9zaXRpb24oZXZlbnQudG9TdGF0ZSk7XG4gICAgdGhpcy5fYmVmb3JlQ2VudGVyaW5nLmVtaXQoaXNDZW50ZXJpbmcpO1xuICAgIGlmIChpc0NlbnRlcmluZykge1xuICAgICAgdGhpcy5fb25DZW50ZXJpbmcuZW1pdCh0aGlzLl9lbGVtZW50UmVmLm5hdGl2ZUVsZW1lbnQuY2xpZW50SGVpZ2h0KTtcbiAgICB9XG4gIH1cblxuICAvKiogVGhlIHRleHQgZGlyZWN0aW9uIG9mIHRoZSBjb250YWluaW5nIGFwcC4gKi9cbiAgX2dldExheW91dERpcmVjdGlvbigpOiBEaXJlY3Rpb24ge1xuICAgIHJldHVybiB0aGlzLl9kaXIgJiYgdGhpcy5fZGlyLnZhbHVlID09PSAncnRsJyA/ICdydGwnIDogJ2x0cic7XG4gIH1cblxuICAvKiogV2hldGhlciB0aGUgcHJvdmlkZWQgcG9zaXRpb24gc3RhdGUgaXMgY29uc2lkZXJlZCBjZW50ZXIsIHJlZ2FyZGxlc3Mgb2Ygb3JpZ2luLiAqL1xuICBfaXNDZW50ZXJQb3NpdGlvbihwb3NpdGlvbjogTWF0VGFiQm9keVBvc2l0aW9uU3RhdGV8c3RyaW5nKTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIHBvc2l0aW9uID09ICdjZW50ZXInIHx8XG4gICAgICAgIHBvc2l0aW9uID09ICdsZWZ0LW9yaWdpbi1jZW50ZXInIHx8XG4gICAgICAgIHBvc2l0aW9uID09ICdyaWdodC1vcmlnaW4tY2VudGVyJztcbiAgfVxuXG4gIC8qKiBDb21wdXRlcyB0aGUgcG9zaXRpb24gc3RhdGUgdGhhdCB3aWxsIGJlIHVzZWQgZm9yIHRoZSB0YWItYm9keSBhbmltYXRpb24gdHJpZ2dlci4gKi9cbiAgcHJpdmF0ZSBfY29tcHV0ZVBvc2l0aW9uQW5pbWF0aW9uU3RhdGUoZGlyOiBEaXJlY3Rpb24gPSB0aGlzLl9nZXRMYXlvdXREaXJlY3Rpb24oKSkge1xuICAgIGlmICh0aGlzLl9wb3NpdGlvbkluZGV4IDwgMCkge1xuICAgICAgdGhpcy5fcG9zaXRpb24gPSBkaXIgPT0gJ2x0cicgPyAnbGVmdCcgOiAncmlnaHQnO1xuICAgIH0gZWxzZSBpZiAodGhpcy5fcG9zaXRpb25JbmRleCA+IDApIHtcbiAgICAgIHRoaXMuX3Bvc2l0aW9uID0gZGlyID09ICdsdHInID8gJ3JpZ2h0JyA6ICdsZWZ0JztcbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5fcG9zaXRpb24gPSAnY2VudGVyJztcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogQ29tcHV0ZXMgdGhlIHBvc2l0aW9uIHN0YXRlIGJhc2VkIG9uIHRoZSBzcGVjaWZpZWQgb3JpZ2luIHBvc2l0aW9uLiBUaGlzIGlzIHVzZWQgaWYgdGhlXG4gICAqIHRhYiBpcyBiZWNvbWluZyB2aXNpYmxlIGltbWVkaWF0ZWx5IGFmdGVyIGNyZWF0aW9uLlxuICAgKi9cbiAgcHJpdmF0ZSBfY29tcHV0ZVBvc2l0aW9uRnJvbU9yaWdpbihvcmlnaW46IG51bWJlcik6IE1hdFRhYkJvZHlQb3NpdGlvblN0YXRlIHtcbiAgICBjb25zdCBkaXIgPSB0aGlzLl9nZXRMYXlvdXREaXJlY3Rpb24oKTtcblxuICAgIGlmICgoZGlyID09ICdsdHInICYmIG9yaWdpbiA8PSAwKSB8fCAoZGlyID09ICdydGwnICYmIG9yaWdpbiA+IDApKSB7XG4gICAgICByZXR1cm4gJ2xlZnQtb3JpZ2luLWNlbnRlcic7XG4gICAgfVxuXG4gICAgcmV0dXJuICdyaWdodC1vcmlnaW4tY2VudGVyJztcbiAgfVxufVxuXG4vKipcbiAqIFdyYXBwZXIgZm9yIHRoZSBjb250ZW50cyBvZiBhIHRhYi5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnbWF0LXRhYi1ib2R5JyxcbiAgdGVtcGxhdGVVcmw6ICd0YWItYm9keS5odG1sJyxcbiAgc3R5bGVVcmxzOiBbJ3RhYi1ib2R5LmNzcyddLFxuICBlbmNhcHN1bGF0aW9uOiBWaWV3RW5jYXBzdWxhdGlvbi5Ob25lLFxuICAvLyB0c2xpbnQ6ZGlzYWJsZS1uZXh0LWxpbmU6dmFsaWRhdGUtZGVjb3JhdG9yc1xuICBjaGFuZ2VEZXRlY3Rpb246IENoYW5nZURldGVjdGlvblN0cmF0ZWd5LkRlZmF1bHQsXG4gIGFuaW1hdGlvbnM6IFttYXRUYWJzQW5pbWF0aW9ucy50cmFuc2xhdGVUYWJdLFxuICBob3N0OiB7XG4gICAgJ2NsYXNzJzogJ21hdC10YWItYm9keScsXG4gIH1cbn0pXG5leHBvcnQgY2xhc3MgTWF0VGFiQm9keSBleHRlbmRzIF9NYXRUYWJCb2R5QmFzZSB7XG4gIEBWaWV3Q2hpbGQoUG9ydGFsSG9zdERpcmVjdGl2ZSkgX3BvcnRhbEhvc3Q6IFBvcnRhbEhvc3REaXJlY3RpdmU7XG5cbiAgY29uc3RydWN0b3IoZWxlbWVudFJlZjogRWxlbWVudFJlZjxIVE1MRWxlbWVudD4sXG4gICAgICAgICAgICAgIEBPcHRpb25hbCgpIGRpcjogRGlyZWN0aW9uYWxpdHksXG4gICAgICAgICAgICAgIGNoYW5nZURldGVjdG9yUmVmOiBDaGFuZ2VEZXRlY3RvclJlZikge1xuICAgIHN1cGVyKGVsZW1lbnRSZWYsIGRpciwgY2hhbmdlRGV0ZWN0b3JSZWYpO1xuICB9XG59XG4iXX0=