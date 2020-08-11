/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { BasePortalOutlet, CdkPortalOutlet, } from '@angular/cdk/portal';
import { ChangeDetectionStrategy, ChangeDetectorRef, Component, ElementRef, NgZone, ViewChild, ViewEncapsulation, } from '@angular/core';
import { Subject } from 'rxjs';
import { take } from 'rxjs/operators';
import { matSnackBarAnimations } from './snack-bar-animations';
import { MatSnackBarConfig } from './snack-bar-config';
/**
 * Internal component that wraps user-provided snack bar content.
 * @docs-private
 */
let MatSnackBarContainer = /** @class */ (() => {
    class MatSnackBarContainer extends BasePortalOutlet {
        constructor(_ngZone, _elementRef, _changeDetectorRef, 
        /** The snack bar configuration. */
        snackBarConfig) {
            super();
            this._ngZone = _ngZone;
            this._elementRef = _elementRef;
            this._changeDetectorRef = _changeDetectorRef;
            this.snackBarConfig = snackBarConfig;
            /** Whether the component has been destroyed. */
            this._destroyed = false;
            /** Subject for notifying that the snack bar has exited from view. */
            this._onExit = new Subject();
            /** Subject for notifying that the snack bar has finished entering the view. */
            this._onEnter = new Subject();
            /** The state of the snack bar animations. */
            this._animationState = 'void';
            /**
             * Attaches a DOM portal to the snack bar container.
             * @deprecated To be turned into a method.
             * @breaking-change 10.0.0
             */
            this.attachDomPortal = (portal) => {
                this._assertNotAttached();
                this._applySnackBarClasses();
                return this._portalOutlet.attachDomPortal(portal);
            };
            // Based on the ARIA spec, `alert` and `status` roles have an
            // implicit `assertive` and `polite` politeness respectively.
            if (snackBarConfig.politeness === 'assertive' && !snackBarConfig.announcementMessage) {
                this._role = 'alert';
            }
            else if (snackBarConfig.politeness === 'off') {
                this._role = null;
            }
            else {
                this._role = 'status';
            }
        }
        /** Attach a component portal as content to this snack bar container. */
        attachComponentPortal(portal) {
            this._assertNotAttached();
            this._applySnackBarClasses();
            return this._portalOutlet.attachComponentPortal(portal);
        }
        /** Attach a template portal as content to this snack bar container. */
        attachTemplatePortal(portal) {
            this._assertNotAttached();
            this._applySnackBarClasses();
            return this._portalOutlet.attachTemplatePortal(portal);
        }
        /** Handle end of animations, updating the state of the snackbar. */
        onAnimationEnd(event) {
            const { fromState, toState } = event;
            if ((toState === 'void' && fromState !== 'void') || toState === 'hidden') {
                this._completeExit();
            }
            if (toState === 'visible') {
                // Note: we shouldn't use `this` inside the zone callback,
                // because it can cause a memory leak.
                const onEnter = this._onEnter;
                this._ngZone.run(() => {
                    onEnter.next();
                    onEnter.complete();
                });
            }
        }
        /** Begin animation of snack bar entrance into view. */
        enter() {
            if (!this._destroyed) {
                this._animationState = 'visible';
                this._changeDetectorRef.detectChanges();
            }
        }
        /** Begin animation of the snack bar exiting from view. */
        exit() {
            // Note: this one transitions to `hidden`, rather than `void`, in order to handle the case
            // where multiple snack bars are opened in quick succession (e.g. two consecutive calls to
            // `MatSnackBar.open`).
            this._animationState = 'hidden';
            // Mark this element with an 'exit' attribute to indicate that the snackbar has
            // been dismissed and will soon be removed from the DOM. This is used by the snackbar
            // test harness.
            this._elementRef.nativeElement.setAttribute('mat-exit', '');
            return this._onExit;
        }
        /** Makes sure the exit callbacks have been invoked when the element is destroyed. */
        ngOnDestroy() {
            this._destroyed = true;
            this._completeExit();
        }
        /**
         * Waits for the zone to settle before removing the element. Helps prevent
         * errors where we end up removing an element which is in the middle of an animation.
         */
        _completeExit() {
            this._ngZone.onMicrotaskEmpty.asObservable().pipe(take(1)).subscribe(() => {
                this._onExit.next();
                this._onExit.complete();
            });
        }
        /** Applies the various positioning and user-configured CSS classes to the snack bar. */
        _applySnackBarClasses() {
            const element = this._elementRef.nativeElement;
            const panelClasses = this.snackBarConfig.panelClass;
            if (panelClasses) {
                if (Array.isArray(panelClasses)) {
                    // Note that we can't use a spread here, because IE doesn't support multiple arguments.
                    panelClasses.forEach(cssClass => element.classList.add(cssClass));
                }
                else {
                    element.classList.add(panelClasses);
                }
            }
            if (this.snackBarConfig.horizontalPosition === 'center') {
                element.classList.add('mat-snack-bar-center');
            }
            if (this.snackBarConfig.verticalPosition === 'top') {
                element.classList.add('mat-snack-bar-top');
            }
        }
        /** Asserts that no content is already attached to the container. */
        _assertNotAttached() {
            if (this._portalOutlet.hasAttached()) {
                throw Error('Attempting to attach snack bar content after content is already attached');
            }
        }
    }
    MatSnackBarContainer.decorators = [
        { type: Component, args: [{
                    selector: 'snack-bar-container',
                    template: "<ng-template cdkPortalOutlet></ng-template>\n",
                    // In Ivy embedded views will be change detected from their declaration place, rather than
                    // where they were stamped out. This means that we can't have the snack bar container be OnPush,
                    // because it might cause snack bars that were opened from a template not to be out of date.
                    // tslint:disable-next-line:validate-decorators
                    changeDetection: ChangeDetectionStrategy.Default,
                    encapsulation: ViewEncapsulation.None,
                    animations: [matSnackBarAnimations.snackBarState],
                    host: {
                        '[attr.role]': '_role',
                        'class': 'mat-snack-bar-container',
                        '[@state]': '_animationState',
                        '(@state.done)': 'onAnimationEnd($event)'
                    },
                    styles: [".mat-snack-bar-container{border-radius:4px;box-sizing:border-box;display:block;margin:24px;max-width:33vw;min-width:344px;padding:14px 16px;min-height:48px;transform-origin:center}.cdk-high-contrast-active .mat-snack-bar-container{border:solid 1px}.mat-snack-bar-handset{width:100%}.mat-snack-bar-handset .mat-snack-bar-container{margin:8px;max-width:100%;min-width:0;width:100%}\n"]
                },] }
    ];
    MatSnackBarContainer.ctorParameters = () => [
        { type: NgZone },
        { type: ElementRef },
        { type: ChangeDetectorRef },
        { type: MatSnackBarConfig }
    ];
    MatSnackBarContainer.propDecorators = {
        _portalOutlet: [{ type: ViewChild, args: [CdkPortalOutlet, { static: true },] }]
    };
    return MatSnackBarContainer;
})();
export { MatSnackBarContainer };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic25hY2stYmFyLWNvbnRhaW5lci5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9zbmFjay1iYXIvc25hY2stYmFyLWNvbnRhaW5lci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFHSCxPQUFPLEVBQ0wsZ0JBQWdCLEVBQ2hCLGVBQWUsR0FJaEIsTUFBTSxxQkFBcUIsQ0FBQztBQUM3QixPQUFPLEVBQ0wsdUJBQXVCLEVBQ3ZCLGlCQUFpQixFQUNqQixTQUFTLEVBRVQsVUFBVSxFQUVWLE1BQU0sRUFFTixTQUFTLEVBQ1QsaUJBQWlCLEdBQ2xCLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBYSxPQUFPLEVBQUMsTUFBTSxNQUFNLENBQUM7QUFDekMsT0FBTyxFQUFDLElBQUksRUFBQyxNQUFNLGdCQUFnQixDQUFDO0FBQ3BDLE9BQU8sRUFBQyxxQkFBcUIsRUFBQyxNQUFNLHdCQUF3QixDQUFDO0FBQzdELE9BQU8sRUFBQyxpQkFBaUIsRUFBQyxNQUFNLG9CQUFvQixDQUFDO0FBR3JEOzs7R0FHRztBQUNIO0lBQUEsTUFrQmEsb0JBQXFCLFNBQVEsZ0JBQWdCO1FBbUJ4RCxZQUNVLE9BQWUsRUFDZixXQUFvQyxFQUNwQyxrQkFBcUM7UUFDN0MsbUNBQW1DO1FBQzVCLGNBQWlDO1lBRXhDLEtBQUssRUFBRSxDQUFDO1lBTkEsWUFBTyxHQUFQLE9BQU8sQ0FBUTtZQUNmLGdCQUFXLEdBQVgsV0FBVyxDQUF5QjtZQUNwQyx1QkFBa0IsR0FBbEIsa0JBQWtCLENBQW1CO1lBRXRDLG1CQUFjLEdBQWQsY0FBYyxDQUFtQjtZQXZCMUMsZ0RBQWdEO1lBQ3hDLGVBQVUsR0FBRyxLQUFLLENBQUM7WUFLM0IscUVBQXFFO1lBQzVELFlBQU8sR0FBaUIsSUFBSSxPQUFPLEVBQUUsQ0FBQztZQUUvQywrRUFBK0U7WUFDdEUsYUFBUSxHQUFpQixJQUFJLE9BQU8sRUFBRSxD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QUFrQjtZQUN4QixJQUFJLElBQUksQ0FBQyxhQUFhLENBQUMsV0FBVyxFQUFFLEVBQUU7Z0JBQ3BDLE1BQU0sS0FBSyxDQUFDLDBFQUEwRSxDQUFDLENBQUM7YUFDekY7UUFDSCxDQUFDOzs7Z0JBMUtGLFNBQVMsU0FBQztvQkFDVCxRQUFRLEVBQUUscUJBQXFCO29CQUMvQix5REFBdUM7b0JBRXZDLDBGQUEwRjtvQkFDMUYsZ0dBQWdHO29CQUNoRyw0RkFBNEY7b0JBQzVGLCtDQUErQztvQkFDL0MsZUFBZSxFQUFFLHVCQUF1QixDQUFDLE9BQU87b0JBQ2hELGFBQWEsRUFBRSxpQkFBaUIsQ0FBQyxJQUFJO29CQUNyQyxVQUFVLEVBQUUsQ0FBQyxxQkFBcUIsQ0FBQyxhQUFhLENBQUM7b0JBQ2pELElBQUksRUFBRTt3QkFDSixhQUFhLEVBQUUsT0FBTzt3QkFDdEIsT0FBTyxFQUFFLHlCQUF5Qjt3QkFDbEMsVUFBVSxFQUFFLGlCQUFpQjt3QkFDN0IsZUFBZSxFQUFFLHdCQUF3QjtxQkFDMUM7O2lCQUNGOzs7Z0JBaENDLE1BQU07Z0JBRk4sVUFBVTtnQkFIVixpQkFBaUI7Z0JBYVgsaUJBQWlCOzs7Z0NBOEJ0QixTQUFTLFNBQUMsZUFBZSxFQUFFLEVBQUMsTUFBTSxFQUFFLElBQUksRUFBQzs7SUFvSjVDLDJCQUFDO0tBQUE7U0F6Slksb0JBQW9CIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7QW5pbWF0aW9uRXZlbnR9IGZyb20gJ0Bhbmd1bGFyL2FuaW1hdGlvbnMnO1xuaW1wb3J0IHtcbiAgQmFzZVBvcnRhbE91dGxldCxcbiAgQ2RrUG9ydGFsT3V0bGV0LFxuICBDb21wb25lbnRQb3J0YWwsXG4gIFRlbXBsYXRlUG9ydGFsLFxuICBEb21Qb3J0YWwsXG59IGZyb20gJ0Bhbmd1bGFyL2Nkay9wb3J0YWwnO1xuaW1wb3J0IHtcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksXG4gIENoYW5nZURldGVjdG9yUmVmLFxuICBDb21wb25lbnQsXG4gIENvbXBvbmVudFJlZixcbiAgRWxlbWVudFJlZixcbiAgRW1iZWRkZWRWaWV3UmVmLFxuICBOZ1pvbmUsXG4gIE9uRGVzdHJveSxcbiAgVmlld0NoaWxkLFxuICBWaWV3RW5jYXBzdWxhdGlvbixcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQge09ic2VydmFibGUsIFN1YmplY3R9IGZyb20gJ3J4anMnO1xuaW1wb3J0IHt0YWtlfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5pbXBvcnQge21hdFNuYWNrQmFyQW5pbWF0aW9uc30gZnJvbSAnLi9zbmFjay1iYXItYW5pbWF0aW9ucyc7XG5pbXBvcnQge01hdFNuYWNrQmFyQ29uZmlnfSBmcm9tICcuL3NuYWNrLWJhci1jb25maWcnO1xuXG5cbi8qKlxuICogSW50ZXJuYWwgY29tcG9uZW50IHRoYXQgd3JhcHMgdXNlci1wcm92aWRlZCBzbmFjayBiYXIgY29udGVudC5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnc25hY2stYmFyLWNvbnRhaW5lcicsXG4gIHRlbXBsYXRlVXJsOiAnc25hY2stYmFyLWNvbnRhaW5lci5odG1sJyxcbiAgc3R5bGVVcmxzOiBbJ3NuYWNrLWJhci1jb250YWluZXIuY3NzJ10sXG4gIC8vIEluIEl2eSBlbWJlZGRlZCB2aWV3cyB3aWxsIGJlIGNoYW5nZSBkZXRlY3RlZCBmcm9tIHRoZWlyIGRlY2xhcmF0aW9uIHBsYWNlLCByYXRoZXIgdGhhblxuICAvLyB3aGVyZSB0aGV5IHdlcmUgc3RhbXBlZCBvdXQuIFRoaXMgbWVhbnMgdGhhdCB3ZSBjYW4ndCBoYXZlIHRoZSBzbmFjayBiYXIgY29udGFpbmVyIGJlIE9uUHVzaCxcbiAgLy8gYmVjYXVzZSBpdCBtaWdodCBjYXVzZSBzbmFjayBiYXJzIHRoYXQgd2VyZSBvcGVuZWQgZnJvbSBhIHRlbXBsYXRlIG5vdCB0byBiZSBvdXQgb2YgZGF0ZS5cbiAgLy8gdHNsaW50OmRpc2FibGUtbmV4dC1saW5lOnZhbGlkYXRlLWRlY29yYXRvcnNcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5EZWZhdWx0LFxuICBlbmNhcHN1bGF0aW9uOiBWaWV3RW5jYXBzdWxhdGlvbi5Ob25lLFxuICBhbmltYXRpb25zOiBbbWF0U25hY2tCYXJBbmltYXRpb25zLnNuYWNrQmFyU3RhdGVdLFxuICBob3N0OiB7XG4gICAgJ1thdHRyLnJvbGVdJzogJ19yb2xlJyxcbiAgICAnY2xhc3MnOiAnbWF0LXNuYWNrLWJhci1jb250YWluZXInLFxuICAgICdbQHN0YXRlXSc6ICdfYW5pbWF0aW9uU3RhdGUnLFxuICAgICcoQHN0YXRlLmRvbmUpJzogJ29uQW5pbWF0aW9uRW5kKCRldmVudCknXG4gIH0sXG59KVxuZXhwb3J0IGNsYXNzIE1hdFNuYWNrQmFyQ29udGFpbmVyIGV4dGVuZHMgQmFzZVBvcnRhbE91dGxldCBpbXBsZW1lbnRzIE9uRGVzdHJveSB7XG4gIC8qKiBXaGV0aGVyIHRoZSBjb21wb25lbnQgaGFzIGJlZW4gZGVzdHJveWVkLiAqL1xuICBwcml2YXRlIF9kZXN0cm95ZWQgPSBmYWxzZTtcblxuICAvKiogVGhlIHBvcnRhbCBvdXRsZXQgaW5zaWRlIG9mIHRoaXMgY29udGFpbmVyIGludG8gd2hpY2ggdGhlIHNuYWNrIGJhciBjb250ZW50IHdpbGwgYmUgbG9hZGVkLiAqL1xuICBAVmlld0NoaWxkKENka1BvcnRhbE91dGxldCwge3N0YXRpYzogdHJ1ZX0pIF9wb3J0YWxPdXRsZXQ6IENka1BvcnRhbE91dGxldDtcblxuICAvKiogU3ViamVjdCBmb3Igbm90aWZ5aW5nIHRoYXQgdGhlIHNuYWNrIGJhciBoYXMgZXhpdGVkIGZyb20gdmlldy4gKi9cbiAgcmVhZG9ubHkgX29uRXhpdDogU3ViamVjdDxhbnk+ID0gbmV3IFN1YmplY3QoKTtcblxuICAvKiogU3ViamVjdCBmb3Igbm90aWZ5aW5nIHRoYXQgdGhlIHNuYWNrIGJhciBoYXMgZmluaXNoZWQgZW50ZXJpbmcgdGhlIHZpZXcuICovXG4gIHJlYWRvbmx5IF9vbkVudGVyOiBTdWJqZWN0PGFueT4gPSBuZXcgU3ViamVjdCgpO1xuXG4gIC8qKiBUaGUgc3RhdGUgb2YgdGhlIHNuYWNrIGJhciBhbmltYXRpb25zLiAqL1xuICBfYW5pbWF0aW9uU3RhdGUgPSAndm9pZCc7XG5cbiAgLyoqIEFSSUEgcm9sZSBmb3IgdGhlIHNuYWNrIGJhciBjb250YWluZXIuICovXG4gIF9yb2xlOiAnYWxlcnQnIHwgJ3N0YXR1cycgfCBudWxsO1xuXG4gIGNvbnN0cnVjdG9yKFxuICAgIHByaXZhdGUgX25nWm9uZTogTmdab25lLFxuICAgIHByaXZhdGUgX2VsZW1lbnRSZWY6IEVsZW1lbnRSZWY8SFRNTEVsZW1lbnQ+LFxuICAgIHByaXZhdGUgX2NoYW5nZURldGVjdG9yUmVmOiBDaGFuZ2VEZXRlY3RvclJlZixcbiAgICAvKiogVGhlIHNuYWNrIGJhciBjb25maWd1cmF0aW9uLiAqL1xuICAgIHB1YmxpYyBzbmFja0JhckNvbmZpZzogTWF0U25hY2tCYXJDb25maWcpIHtcblxuICAgIHN1cGVyKCk7XG5cbiAgICAvLyBCYXNlZCBvbiB0aGUgQVJJQSBzcGVjLCBgYWxlcnRgIGFuZCBgc3RhdHVzYCByb2xlcyBoYXZlIGFuXG4gICAgLy8gaW1wbGljaXQgYGFzc2VydGl2ZWAgYW5kIGBwb2xpdGVgIHBvbGl0ZW5lc3MgcmVzcGVjdGl2ZWx5LlxuICAgIGlmIChzbmFja0JhckNvbmZpZy5wb2xpdGVuZXNzID09PSAnYXNzZXJ0aXZlJyAmJiAhc25hY2tCYXJDb25maWcuYW5ub3VuY2VtZW50TWVzc2FnZSkge1xuICAgICAgdGhpcy5fcm9sZSA9ICdhbGVydCc7XG4gICAgfSBlbHNlIGlmIChzbmFja0JhckNvbmZpZy5wb2xpdGVuZXNzID09PSAnb2ZmJykge1xuICAgICAgdGhpcy5fcm9sZSA9IG51bGw7XG4gICAgfSBlbHNlIHtcbiAgICAgIHRoaXMuX3JvbGUgPSAnc3RhdHVzJztcbiAgICB9XG4gIH1cblxuICAvKiogQXR0YWNoIGEgY29tcG9uZW50IHBvcnRhbCBhcyBjb250ZW50IHRvIHRoaXMgc25hY2sgYmFyIGNvbnRhaW5lci4gKi9cbiAgYXR0YWNoQ29tcG9uZW50UG9ydGFsPFQ+KHBvcnRhbDogQ29tcG9uZW50UG9ydGFsPFQ+KTogQ29tcG9uZW50UmVmPFQ+IHtcbiAgICB0aGlzLl9hc3NlcnROb3RBdHRhY2hlZCgpO1xuICAgIHRoaXMuX2FwcGx5U25hY2tCYXJDbGFzc2VzKCk7XG4gICAgcmV0dXJuIHRoaXMuX3BvcnRhbE91dGxldC5hdHRhY2hDb21wb25lbnRQb3J0YWwocG9ydGFsKTtcbiAgfVxuXG4gIC8qKiBBdHRhY2ggYSB0ZW1wbGF0ZSBwb3J0YWwgYXMgY29udGVudCB0byB0aGlzIHNuYWNrIGJhciBjb250YWluZXIuICovXG4gIGF0dGFjaFRlbXBsYXRlUG9ydGFsPEM+KHBvcnRhbDogVGVtcGxhdGVQb3J0YWw8Qz4pOiBFbWJlZGRlZFZpZXdSZWY8Qz4ge1xuICAgIHRoaXMuX2Fzc2VydE5vdEF0dGFjaGVkKCk7XG4gICAgdGhpcy5fYXBwbHlTbmFja0JhckNsYXNzZXMoKTtcbiAgICByZXR1cm4gdGhpcy5fcG9ydGFsT3V0bGV0LmF0dGFjaFRlbXBsYXRlUG9ydGFsKHBvcnRhbCk7XG4gIH1cblxuICAvKipcbiAgICogQXR0YWNoZXMgYSBET00gcG9ydGFsIHRvIHRoZSBzbmFjayBiYXIgY29udGFpbmVyLlxuICAgKiBAZGVwcmVjYXRlZCBUbyBiZSB0dXJuZWQgaW50byBhIG1ldGhvZC5cbiAgICogQGJyZWFraW5nLWNoYW5nZSAxMC4wLjBcbiAgICovXG4gIGF0dGFjaERvbVBvcnRhbCA9IChwb3J0YWw6IERvbVBvcnRhbCkgPT4ge1xuICAgIHRoaXMuX2Fzc2VydE5vdEF0dGFjaGVkKCk7XG4gICAgdGhpcy5fYXBwbHlTbmFja0JhckNsYXNzZXMoKTtcbiAgICByZXR1cm4gdGhpcy5fcG9ydGFsT3V0bGV0LmF0dGFjaERvbVBvcnRhbChwb3J0YWwpO1xuICB9XG5cbiAgLyoqIEhhbmRsZSBlbmQgb2YgYW5pbWF0aW9ucywgdXBkYXRpbmcgdGhlIHN0YXRlIG9mIHRoZSBzbmFja2Jhci4gKi9cbiAgb25BbmltYXRpb25FbmQoZXZlbnQ6IEFuaW1hdGlvbkV2ZW50KSB7XG4gICAgY29uc3Qge2Zyb21TdGF0ZSwgdG9TdGF0ZX0gPSBldmVudDtcblxuICAgIGlmICgodG9TdGF0ZSA9PT0gJ3ZvaWQnICYmIGZyb21TdGF0ZSAhPT0gJ3ZvaWQnKSB8fCB0b1N0YXRlID09PSAnaGlkZGVuJykge1xuICAgICAgdGhpcy5fY29tcGxldGVFeGl0KCk7XG4gICAgfVxuXG4gICAgaWYgKHRvU3RhdGUgPT09ICd2aXNpYmxlJykge1xuICAgICAgLy8gTm90ZTogd2Ugc2hvdWxkbid0IHVzZSBgdGhpc2AgaW5zaWRlIHRoZSB6b25lIGNhbGxiYWNrLFxuICAgICAgLy8gYmVjYXVzZSBpdCBjYW4gY2F1c2UgYSBtZW1vcnkgbGVhay5cbiAgICAgIGNvbnN0IG9uRW50ZXIgPSB0aGlzLl9vbkVudGVyO1xuXG4gICAgICB0aGlzLl9uZ1pvbmUucnVuKCgpID0+IHtcbiAgICAgICAgb25FbnRlci5uZXh0KCk7XG4gICAgICAgIG9uRW50ZXIuY29tcGxldGUoKTtcbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBCZWdpbiBhbmltYXRpb24gb2Ygc25hY2sgYmFyIGVudHJhbmNlIGludG8gdmlldy4gKi9cbiAgZW50ZXIoKTogdm9pZCB7XG4gICAgaWYgKCF0aGlzLl9kZXN0cm95ZWQpIHtcbiAgICAgIHRoaXMuX2FuaW1hdGlvblN0YXRlID0gJ3Zpc2libGUnO1xuICAgICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3JSZWYuZGV0ZWN0Q2hhbmdlcygpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBCZWdpbiBhbmltYXRpb24gb2YgdGhlIHNuYWNrIGJhciBleGl0aW5nIGZyb20gdmlldy4gKi9cbiAgZXhpdCgpOiBPYnNlcnZhYmxlPHZvaWQ+IHtcbiAgICAvLyBOb3RlOiB0aGlzIG9uZSB0cmFuc2l0aW9ucyB0byBgaGlkZGVuYCwgcmF0aGVyIHRoYW4gYHZvaWRgLCBpbiBvcmRlciB0byBoYW5kbGUgdGhlIGNhc2VcbiAgICAvLyB3aGVyZSBtdWx0aXBsZSBzbmFjayBiYXJzIGFyZSBvcGVuZWQgaW4gcXVpY2sgc3VjY2Vzc2lvbiAoZS5nLiB0d28gY29uc2VjdXRpdmUgY2FsbHMgdG9cbiAgICAvLyBgTWF0U25hY2tCYXIub3BlbmApLlxuICAgIHRoaXMuX2FuaW1hdGlvblN0YXRlID0gJ2hpZGRlbic7XG5cbiAgICAvLyBNYXJrIHRoaXMgZWxlbWVudCB3aXRoIGFuICdleGl0JyBhdHRyaWJ1dGUgdG8gaW5kaWNhdGUgdGhhdCB0aGUgc25hY2tiYXIgaGFzXG4gICAgLy8gYmVlbiBkaXNtaXNzZWQgYW5kIHdpbGwgc29vbiBiZSByZW1vdmVkIGZyb20gdGhlIERPTS4gVGhpcyBpcyB1c2VkIGJ5IHRoZSBzbmFja2JhclxuICAgIC8vIHRlc3QgaGFybmVzcy5cbiAgICB0aGlzLl9lbGVtZW50UmVmLm5hdGl2ZUVsZW1lbnQuc2V0QXR0cmlidXRlKCdtYXQtZXhpdCcsICcnKTtcblxuICAgIHJldHVybiB0aGlzLl9vbkV4aXQ7XG4gIH1cblxuICAvKiogTWFrZXMgc3VyZSB0aGUgZXhpdCBjYWxsYmFja3MgaGF2ZSBiZWVuIGludm9rZWQgd2hlbiB0aGUgZWxlbWVudCBpcyBkZXN0cm95ZWQuICovXG4gIG5nT25EZXN0cm95KCkge1xuICAgIHRoaXMuX2Rlc3Ryb3llZCA9IHRydWU7XG4gICAgdGhpcy5fY29tcGxldGVFeGl0KCk7XG4gIH1cblxuICAvKipcbiAgICogV2FpdHMgZm9yIHRoZSB6b25lIHRvIHNldHRsZSBiZWZvcmUgcmVtb3ZpbmcgdGhlIGVsZW1lbnQuIEhlbHBzIHByZXZlbnRcbiAgICogZXJyb3JzIHdoZXJlIHdlIGVuZCB1cCByZW1vdmluZyBhbiBlbGVtZW50IHdoaWNoIGlzIGluIHRoZSBtaWRkbGUgb2YgYW4gYW5pbWF0aW9uLlxuICAgKi9cbiAgcHJpdmF0ZSBfY29tcGxldGVFeGl0KCkge1xuICAgIHRoaXMuX25nWm9uZS5vbk1pY3JvdGFza0VtcHR5LmFzT2JzZXJ2YWJsZSgpLnBpcGUodGFrZSgxKSkuc3Vic2NyaWJlKCgpID0+IHtcbiAgICAgIHRoaXMuX29uRXhpdC5uZXh0KCk7XG4gICAgICB0aGlzLl9vbkV4aXQuY29tcGxldGUoKTtcbiAgICB9KTtcbiAgfVxuXG4gIC8qKiBBcHBsaWVzIHRoZSB2YXJpb3VzIHBvc2l0aW9uaW5nIGFuZCB1c2VyLWNvbmZpZ3VyZWQgQ1NTIGNsYXNzZXMgdG8gdGhlIHNuYWNrIGJhci4gKi9cbiAgcHJpdmF0ZSBfYXBwbHlTbmFja0JhckNsYXNzZXMoKSB7XG4gICAgY29uc3QgZWxlbWVudDogSFRNTEVsZW1lbnQgPSB0aGlzLl9lbGVtZW50UmVmLm5hdGl2ZUVsZW1lbnQ7XG4gICAgY29uc3QgcGFuZWxDbGFzc2VzID0gdGhpcy5zbmFja0JhckNvbmZpZy5wYW5lbENsYXNzO1xuXG4gICAgaWYgKHBhbmVsQ2xhc3Nlcykge1xuICAgICAgaWYgKEFycmF5LmlzQXJyYXkocGFuZWxDbGFzc2VzKSkge1xuICAgICAgICAvLyBOb3RlIHRoYXQgd2UgY2FuJ3QgdXNlIGEgc3ByZWFkIGhlcmUsIGJlY2F1c2UgSUUgZG9lc24ndCBzdXBwb3J0IG11bHRpcGxlIGFyZ3VtZW50cy5cbiAgICAgICAgcGFuZWxDbGFzc2VzLmZvckVhY2goY3NzQ2xhc3MgPT4gZWxlbWVudC5jbGFzc0xpc3QuYWRkKGNzc0NsYXNzKSk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICBlbGVtZW50LmNsYXNzTGlzdC5hZGQocGFuZWxDbGFzc2VzKTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICBpZiAodGhpcy5zbmFja0JhckNvbmZpZy5ob3Jpem9udGFsUG9zaXRpb24gPT09ICdjZW50ZXInKSB7XG4gICAgICBlbGVtZW50LmNsYXNzTGlzdC5hZGQoJ21hdC1zbmFjay1iYXItY2VudGVyJyk7XG4gICAgfVxuXG4gICAgaWYgKHRoaXMuc25hY2tCYXJDb25maWcudmVydGljYWxQb3NpdGlvbiA9PT0gJ3RvcCcpIHtcbiAgICAgIGVsZW1lbnQuY2xhc3NMaXN0LmFkZCgnbWF0LXNuYWNrLWJhci10b3AnKTtcbiAgICB9XG4gIH1cblxuICAvKiogQXNzZXJ0cyB0aGF0IG5vIGNvbnRlbnQgaXMgYWxyZWFkeSBhdHRhY2hlZCB0byB0aGUgY29udGFpbmVyLiAqL1xuICBwcml2YXRlIF9hc3NlcnROb3RBdHRhY2hlZCgpIHtcbiAgICBpZiAodGhpcy5fcG9ydGFsT3V0bGV0Lmhhc0F0dGFjaGVkKCkpIHtcbiAgICAgIHRocm93IEVycm9yKCdBdHRlbXB0aW5nIHRvIGF0dGFjaCBzbmFjayBiYXIgY29udGVudCBhZnRlciBjb250ZW50IGlzIGFscmVhZHkgYXR0YWNoZWQnKTtcbiAgICB9XG4gIH1cbn1cbiJdfQ==