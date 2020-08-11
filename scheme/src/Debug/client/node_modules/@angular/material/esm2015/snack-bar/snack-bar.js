/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { LiveAnnouncer } from '@angular/cdk/a11y';
import { BreakpointObserver, Breakpoints } from '@angular/cdk/layout';
import { Overlay, OverlayConfig } from '@angular/cdk/overlay';
import { ComponentPortal, PortalInjector, TemplatePortal } from '@angular/cdk/portal';
import { Inject, Injectable, InjectionToken, Injector, Optional, SkipSelf, TemplateRef, } from '@angular/core';
import { takeUntil } from 'rxjs/operators';
import { SimpleSnackBar } from './simple-snack-bar';
import { MAT_SNACK_BAR_DATA, MatSnackBarConfig } from './snack-bar-config';
import { MatSnackBarContainer } from './snack-bar-container';
import { MatSnackBarModule } from './snack-bar-module';
import { MatSnackBarRef } from './snack-bar-ref';
import * as i0 from "@angular/core";
import * as i1 from "@angular/cdk/overlay";
import * as i2 from "@angular/cdk/a11y";
import * as i3 from "@angular/cdk/layout";
import * as i4 from "./snack-bar-module";
/** Injection token that can be used to specify default snack bar. */
export const MAT_SNACK_BAR_DEFAULT_OPTIONS = new InjectionToken('mat-snack-bar-default-options', {
    providedIn: 'root',
    factory: MAT_SNACK_BAR_DEFAULT_OPTIONS_FACTORY,
});
/** @docs-private */
export function MAT_SNACK_BAR_DEFAULT_OPTIONS_FACTORY() {
    return new MatSnackBarConfig();
}
/**
 * Service to dispatch Material Design snack bar messages.
 */
let MatSnackBar = /** @class */ (() => {
    class MatSnackBar {
        constructor(_overlay, _live, _injector, _breakpointObserver, _parentSnackBar, _defaultConfig) {
            this._overlay = _overlay;
            this._live = _live;
            this._injector = _injector;
            this._breakpointObserver = _breakpointObserver;
            this._parentSnackBar = _parentSnackBar;
            this._defaultConfig = _defaultConfig;
            /**
             * Reference to the current snack bar in the view *at this level* (in the Angular injector tree).
             * If there is a parent snack-bar service, all operations should delegate to that parent
             * via `_openedSnackBarRef`.
             */
            this._snackBarRefAtThisLevel = null;
        }
        /** Reference to the currently opened snackbar at *any* level. */
        get _openedSnackBarRef() {
            const parent = this._parentSnackBar;
            return parent ? parent._openedSnackBarRef : this._snackBarRefAtThisLevel;
        }
        set _openedSnackBarRef(value) {
            if (this._parentSnackBar) {
                this._parentSnackBar._openedSnackBarRef = value;
            }
            else {
                this._snackBarRefAtThisLevel = value;
            }
        }
        /**
         * Creates and dispatches a snack bar with a custom component for the content, removing any
         * currently opened snack bars.
         *
         * @param component Component to be instantiated.
         * @param config Extra configuration for the snack bar.
         */
        openFromComponent(component, config) {
            return this._attach(component, config);
        }
        /**
         * Creates and dispatches a snack bar with a custom template for the content, removing any
         * currently opened snack bars.
         *
         * @param template Template to be instantiated.
         * @param config Extra configuration for the snack bar.
         */
        openFromTemplate(template, config) {
            return this._attach(template, config);
        }
        /**
         * Opens a snackbar with a message and an optional action.
         * @param message The message to show in the snackbar.
         * @param action The label for the snackbar action.
         * @param config Additional configuration options for the snackbar.
         */
        open(message, action = '', config) {
            const _config = Object.assign(Object.assign({}, this._defaultConfig), config);
            // Since the user doesn't have access to the component, we can
            // override the data to pass in our own message and action.
            _config.data = { message, action };
            if (!_config.announcementMessage) {
                _config.announcementMessage = message;
            }
            return this.openFromComponent(SimpleSnackBar, _config);
        }
        /**
         * Dismisses the currently-visible snack bar.
         */
        dismiss() {
            if (this._openedSnackBarRef) {
                this._openedSnackBarRef.dismiss();
            }
        }
        ngOnDestroy() {
            // Only dismiss the snack bar at the current level on destroy.
            if (this._snackBarRefAtThisLevel) {
                this._snackBarRefAtThisLevel.dismiss();
            }
        }
        /**
         * Attaches the snack bar container component to the overlay.
         */
        _attachSnackBarContainer(overlayRef, config) {
            const userInjector = config && config.viewContainerRef && config.viewContainerRef.injector;
            const injector = new PortalInjector(userInjector || this._injector, new WeakMap([
                [MatSnackBarConfig, config]
            ]));
            const containerPortal = new ComponentPortal(MatSnackBarContainer, config.viewContainerRef, injector);
            const containerRef = overlayRef.attach(containerPortal);
            containerRef.instance.snackBarConfig = config;
            return containerRef.instance;
        }
        /**
         * Places a new component or a template as the content of the snack bar container.
         */
        _attach(content, userConfig) {
            const config = Object.assign(Object.assign(Object.assign({}, new MatSnackBarConfig()), this._defaultConfig), userConfig);
            const overlayRef = this._createOverlay(config);
            const container = this._attachSnackBarContainer(overlayRef, config);
            const snackBarRef = new MatSnackBarRef(container, overlayRef);
            if (content instanceof TemplateRef) {
                const portal = new TemplatePortal(content, null, {
                    $implicit: config.data,
                    snackBarRef
                });
                snackBarRef.instance = container.attachTemplatePortal(portal);
            }
            else {
                const injector = this._createInjector(config, snackBarRef);
                const portal = new ComponentPortal(content, undefined, injector);
                const contentRef = container.attachComponentPortal(portal);
                // We can't pass this via the injector, because the injector is created earlier.
                snackBarRef.instance = contentRef.instance;
            }
            // Subscribe to the breakpoint observer and attach the mat-snack-bar-handset class as
            // appropriate. This class is applied to the overlay element because the overlay must expand to
            // fill the width of the screen for full width snackbars.
            this._breakpointObserver.observe(Breakpoints.HandsetPortrait).pipe(takeUntil(overlayRef.detachments())).subscribe(state => {
                const classList = overlayRef.overlayElement.classList;
                const className = 'mat-snack-bar-handset';
                state.matches ? classList.add(className) : classList.remove(className);
            });
            this._animateSnackBar(snackBarRef, config);
            this._openedSnackBarRef = snackBarRef;
            return this._openedSnackBarRef;
        }
        /** Animates the old snack bar out and the new one in. */
        _animateSnackBar(snackBarRef, config) {
            // When the snackbar is dismissed, clear the reference to it.
            snackBarRef.afterDismissed().subscribe(() => {
                // Clear the snackbar ref if it hasn't already been replaced by a newer snackbar.
                if (this._openedSnackBarRef == snackBarRef) {
                    this._openedSnackBarRef = null;
                }
                if (config.announcementMessage) {
                    this._live.clear();
                }
            });
            if (this._openedSnackBarRef) {
                // If a snack bar is already in view, dismiss it and enter the
                // new snack bar after exit animation is complete.
                this._openedSnackBarRef.afterDismissed().subscribe(() => {
                    snackBarRef.containerInstance.enter();
                });
                this._openedSnackBarRef.dismiss();
            }
            else {
                // If no snack bar is in view, enter the new snack bar.
                snackBarRef.containerInstance.enter();
            }
            // If a dismiss timeout is provided, set up dismiss based on after the snackbar is opened.
            if (config.duration && config.duration > 0) {
                snackBarRef.afterOpened().subscribe(() => snackBarRef._dismissAfter(config.duration));
            }
            if (config.announcementMessage) {
                this._live.announce(config.announcementMessage, config.politeness);
            }
        }
        /**
         * Creates a new overlay and places it in the correct location.
         * @param config The user-specified snack bar config.
         */
        _createOverlay(config) {
            const overlayConfig = new OverlayConfig();
            overlayConfig.direction = config.direction;
            let positionStrategy = this._overlay.position().global();
            // Set horizontal position.
            const isRtl = config.direction === 'rtl';
            const isLeft = (config.horizontalPosition === 'left' ||
                (config.horizontalPosition === 'start' && !isRtl) ||
                (config.horizontalPosition === 'end' && isRtl));
            const isRight = !isLeft && config.horizontalPosition !== 'center';
            if (isLeft) {
                positionStrategy.left('0');
            }
            else if (isRight) {
                positionStrategy.right('0');
            }
            else {
                positionStrategy.centerHorizontally();
            }
            // Set horizontal position.
            if (config.verticalPosition === 'top') {
                positionStrategy.top('0');
            }
            else {
                positionStrategy.bottom('0');
            }
            overlayConfig.positionStrategy = positionStrategy;
            return this._overlay.create(overlayConfig);
        }
        /**
         * Creates an injector to be used inside of a snack bar component.
         * @param config Config that was used to create the snack bar.
         * @param snackBarRef Reference to the snack bar.
         */
        _createInjector(config, snackBarRef) {
            const userInjector = config && config.viewContainerRef && config.viewContainerRef.injector;
            return new PortalInjector(userInjector || this._injector, new WeakMap([
                [MatSnackBarRef, snackBarRef],
                [MAT_SNACK_BAR_DATA, config.data]
            ]));
        }
    }
    MatSnackBar.ɵprov = i0.ɵɵdefineInjectable({ factory: function MatSnackBar_Factory() { return new MatSnackBar(i0.ɵɵinject(i1.Overlay), i0.ɵɵinject(i2.LiveAnnouncer), i0.ɵɵinject(i0.INJECTOR), i0.ɵɵinject(i3.BreakpointObserver), i0.ɵɵinject(MatSnackBar, 12), i0.ɵɵinject(MAT_SNACK_BAR_DEFAULT_OPTIONS)); }, token: MatSnackBar, providedIn: i4.MatSnackBarModule });
    MatSnackBar.decorators = [
        { type: Injectable, args: [{ providedIn: MatSnackBarModule },] }
    ];
    MatSnackBar.ctorParameters = () => [
        { type: Overlay },
        { type: LiveAnnouncer },
        { type: Injector },
        { type: BreakpointObserver },
        { type: MatSnackBar, decorators: [{ type: Optional }, { type: SkipSelf }] },
        { type: MatSnackBarConfig, decorators: [{ type: Inject, args: [MAT_SNACK_BAR_DEFAULT_OPTIONS,] }] }
    ];
    return MatSnackBar;
})();
export { MatSnackBar };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic25hY2stYmFyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL3NuYWNrLWJhci9zbmFjay1iYXIudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLGFBQWEsRUFBQyxNQUFNLG1CQUFtQixDQUFDO0FBQ2hELE9BQU8sRUFBQyxrQkFBa0IsRUFBRSxXQUFXLEVBQUMsTUFBTSxxQkFBcUIsQ0FBQztBQUNwRSxPQUFPLEVBQUMsT0FBTyxFQUFFLGFBQWEsRUFBYSxNQUFNLHNCQUFzQixDQUFDO0FBQ3hFLE9BQU8sRUFBQyxlQUFlLEVBQWlCLGNBQWMsRUFBRSxjQUFjLEVBQUMsTUFBTSxxQkFBcUIsQ0FBQztBQUNuRyxPQUFPLEVBR0wsTUFBTSxFQUNOLFVBQVUsRUFDVixjQUFjLEVBQ2QsUUFBUSxFQUNSLFFBQVEsRUFDUixRQUFRLEVBQ1IsV0FBVyxHQUVaLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBQyxTQUFTLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQUN6QyxPQUFPLEVBQUMsY0FBYyxFQUFDLE1BQU0sb0JBQW9CLENBQUM7QUFDbEQsT0FBTyxFQUFDLGtCQUFrQixFQUFFLGlCQUFpQixFQUFDLE1BQU0sb0JBQW9CLENBQUM7QUFDekUsT0FBTyxFQUFDLG9CQUFvQixFQUFDLE1BQU0sdUJBQXVCLENBQUM7QUFDM0QsT0FBTyxFQUFDLGlCQUFpQixFQUFDLE1BQU0sb0JBQW9CLENBQUM7QUFDckQsT0FBTyxFQUFDLGNBQWMsRUFBQyxNQUFNLGlCQUFpQixDQUFDOzs7Ozs7QUFHL0MscUVBQXFFO0FBQ3JFLE1BQU0sQ0FBQyxNQUFNLDZCQUE2QixHQUN0QyxJQUFJLGNBQWMsQ0FBb0IsK0JBQStCLEVBQUU7SUFDckUsVUFBVSxFQUFFLE1BQU07SUFDbEIsT0FBTyxFQUFFLHFDQUFxQztDQUMvQyxDQUFDLENBQUM7QUFFUCxvQkFBb0I7QUFDcEIsTUFBTSxVQUFVLHFDQUFxQztJQUNuRCxPQUFPLElBQUksaUJBQWlCLEVBQUUsQ0FBQztBQUNqQyxDQUFDO0FBRUQ7O0dBRUc7QUFDSDtJQUFBLE1BQ2EsV0FBVztRQXNCdEIsWUFDWSxRQUFpQixFQUNqQixLQUFvQixFQUNwQixTQUFtQixFQUNuQixtQkFBdUMsRUFDZixlQUE0QixFQUNiLGNBQWlDO1lBTHhFLGFBQVEsR0FBUixRQUFRLENBQVM7WUFDakIsVUFBSyxHQUFMLEtBQUssQ0FBZTtZQUNwQixjQUFTLEdBQVQsU0FBUyxDQUFVO1lBQ25CLHdCQUFtQixHQUFuQixtQkFBbUIsQ0FBb0I7WUFDZixvQkFBZSxHQUFmLGVBQWUsQ0FBYTtZQUNiLG1CQUFjLEdBQWQsY0FBYyxDQUFtQjtZQTNCcEY7Ozs7ZUFJRztZQUNLLDRCQUF1QixHQUErQixJQUFJLENBQUM7UUFzQm9CLENBQUM7UUFwQnhGLGlFQUFpRTtRQUNqRSxJQUFJLGtCQUFrQjtZQUNwQixNQUFNLE1BQU0sR0FBRyxJQUFJLENBQUMsZUFBZSxDQUFDO1lBQ3BDLE9BQU8sTUFBTSxDQUFDLENBQUMsQ0FBQyxNQUFNLENBQUMsa0JBQWtCLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyx1QkFBdUIsQ0FBQztRQUMzRSxDQUFDO1FBRUQsSUFBSSxrQkFBa0IsQ0FBQyxLQUFpQztZQUN0RCxJQUFJLElBQUksQ0FBQyxlQUFlLEVBQUU7Z0JBQ3hCLElBQUksQ0FBQyxlQUFlLENBQUMsa0JBQWtCLEdBQUcsS0FBSyxDQUFDO2FBQ2pEO2lCQUFNO2dCQUNMLElBQUksQ0FBQyx1QkFBdUIsR0FBRyxLQUFLLENBQUM7YUFDdEM7UUFDSCxDQUFDO1FBVUQ7Ozs7OztXQU1HO1FBQ0gsaUJBQWlCLENBQUksU0FBMkIsRUFBRSxNQUEwQjtZQUUxRSxPQUFPLElBQUksQ0FBQyxPQUFPLENBQUMsU0FBUyxFQUFFLE1BQU0sQ0FBc0IsQ0FBQztRQUM5RCxDQUFDO1FBRUQ7Ozs7OztXQU1HO1FBQ0gsZ0JBQWdCLENBQUMsUUFBMEIsRUFBRSxNQUEwQjtZQUVyRSxPQUFPLElBQUksQ0FBQyxPQUFPLENBQUMsUUFBUSxFQUFFLE1BQU0sQ0FBQyxDQUFDO1FBQ3hDLENBQUM7UUFFRDs7Ozs7V0FLRztRQUNILElBQUksQ0FBQyxPQUFlLEVBQUUsU0FBaUIsRUFBRSxFQUFFLE1BQTBCO1lBRW5FLE1BQU0sT0FBTyxtQ0FBTyxJQUFJLENBQUMsY0FBYyxHQUFLLE1BQU0sQ0FBQyxDQUFDO1lBRXBELDhEQUE4RDtZQUM5RCwyREFBMkQ7WUFDM0QsT0FBTyxDQUFDLElBQUksR0FBRyxFQUFDLE9BQU8sRUFBRSxNQUFNLEVBQUMsQ0FBQztZQUVqQyxJQUFJLENBQUMsT0FBTyxDQUFDLG1CQUFtQixFQUFFO2dCQUNoQyxPQUFPLENBQUMsbUJBQW1CLEdBQUcsT0FBTyxDQUFDO2FBQ3ZDO1lBRUQsT0FBTyxJQUFJLENBQUMsaUJBQWlCLENBQUMsY0FBYyxFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQ3pELENBQUM7UUFFRDs7V0FFRztRQUNILE9BQU87WUFDTCxJQUFJLElBQUksQ0FBQyxrQkFBa0IsRUFBRTtnQkFDM0IsSUFBSSxDQUFDLGtCQUFrQixDQUFDLE9BQU8sRUFBRSxDQUFDO2FBQ25DO1FBQ0gsQ0FBQztRQUVELFdBQVc7WUFDVCw4REFBOEQ7WUFDOUQsSUFBSSxJQUFJLENBQUMsdUJBQXVCLEVBQUU7Z0JBQ2hDLElBQUksQ0FBQyx1QkFBdUIsQ0FBQyxPQUFPLEVBQUUsQ0FBQzthQUN4QztRQUNILENBQUM7UUFFRDs7V0FFRztRQUNLLHdCQUF3QixDQUFDLFVBQXNCLEVBQ3RCLE1BQXlCO1lBRXhELE1BQU0sWUFBWSxHQUFHLE1BQU0sSUFBSSxNQUFNLENBQUMsZ0JBQWdCLElBQUksTUFBTSxDQUFDLGdCQUFnQixDQUFDLFFBQVEsQ0FBQztZQUMzRixNQUFNLFFBQVEsR0FBRyxJQUFJLGNBQWMsQ0FBQyxZQUFZLElBQUksSUFBSSxDQUFDLFNBQVMsRUFBRSxJQUFJLE9BQU8sQ0FBQztnQkFDOUUsQ0FBQyxpQkFBaUIsRUFBRSxNQUFNLENBQUM7YUFDNUIsQ0FBQyxDQUFDLENBQUM7WUFFSixNQUFNLGVBQWUsR0FDakIsSUFBSSxlQUFlLENBQUMsb0JBQW9CLEVBQUUsTUFBTSxDQUFDLGdCQUFnQixFQUFFLFFBQVEsQ0FBQyxDQUFDO1lBQ2pGLE1BQU0sWUFBWSxHQUF1QyxVQUFVLENBQUMsTUFBTSxDQUFDLGVBQWUsQ0FBQyxDQUFDO1lBQzVGLFlBQVksQ0FBQyxRQUFRLENBQUMsY0FBYyxHQUFHLE1BQU0sQ0FBQztZQUM5QyxPQUFPLFlBQVksQ0FBQyxRQUFRLENBQUM7UUFDL0IsQ0FBQztRQUVEOztXQUVHO1FBQ0ssT0FBTyxDQUFJLE9BQTBDLEVBQUUsVUFBOEI7WUFHM0YsTUFBTSxNQUFNLGlEQUFPLElBQUksaUJBQWlCLEVBQUUsR0FBSyxJQUFJLENBQUMsY0FBYyxHQUFLLFVBQVUsQ0FBQyxDQUFDO1lBQ25GLE1BQU0sVUFBVSxHQUFHLElBQUksQ0FBQyxjQUFjLENBQUMsTUFBTSxDQUFDLENBQUM7WUFDL0MsTUFBTSxTQUFTLEdBQUcsSUFBSSxDQUFDLHdCQUF3QixDQUFDLFVBQVUsRUFBRSxNQUFNLENBQUMsQ0FBQztZQUNwRSxNQUFNLFdBQVcsR0FBRyxJQUFJLGNBQWMsQ0FBMkIsU0FBUyxFQUFFLFVBQVUsQ0FBQyxDQUFDO1lBRXhGLElBQUksT0FBTyxZQUFZLFdBQVcsRUFBRTtnQkFDbEMsTUFBTSxNQUFNLEdBQUcsSUFBSSxjQUFjLENBQUMsT0FBTyxFQUFFLElBQUssRUFBRTtvQkFDaEQsU0FBUyxFQUFFLE1BQU0sQ0FBQyxJQUFJO29CQUN0QixXQUFXO2lCQUNMLENBQUMsQ0FBQztnQkFFVixXQUFXLENBQUMsUUFBUSxHQUFHLFNBQVMsQ0FBQyxvQkFBb0IsQ0FBQyxNQUFNLENBQUMsQ0FBQzthQUMvRDtpQkFBTTtnQkFDTCxNQUFNLFFBQVEsR0FBRyxJQUFJLENBQUMsZUFBZSxDQUFDLE1BQU0sRUFBRSxXQUFXLENBQUMsQ0FBQztnQkFDM0QsTUFBTSxNQUFNLEdBQUcsSUFBSSxlQUFlLENBQUMsT0FBTyxFQUFFLFNBQVMsRUFBRSxRQUFRLENBQUMsQ0FBQztnQkFDakUsTUFBTSxVQUFVLEdBQUcsU0FBUyxDQUFDLHFCQUFxQixDQUFJLE1BQU0sQ0FBQyxDQUFDO2dCQUU5RCxnRkFBZ0Y7Z0JBQ2hGLFdBQVcsQ0FBQyxRQUFRLEdBQUcsVUFBVSxDQUFDLFFBQVEsQ0FBQzthQUM1QztZQUVELHFGQUFxRjtZQUNyRiwrRkFBK0Y7WUFDL0YseURBQXlEO1lBQ3pELElBQUksQ0FBQyxtQkFBbUIsQ0FBQyxPQUFPLENBQUMsV0FBVyxDQUFDLGVBQWUsQ0FBQyxDQUFDLElBQUksQ0FDaEUsU0FBUyxDQUFDLFVBQVUsQ0FBQyxXQUFXLEVBQUUsQ0FBQyxDQUNwQyxDQUFDLFNBQVMsQ0FBQyxLQUFLLENBQUMsRUFBRTtnQkFDbEIsTUFBTSxTQUFTLEdBQUcsVUFBVSxDQUFDLGNBQWMsQ0FBQyxTQUFTLENBQUM7Z0JBQ3RELE1BQU0sU0FBUyxHQUFHLHVCQUF1QixDQUFDO2dCQUMxQyxLQUFLLENBQUMsT0FBTyxDQUFDLENBQUMsQ0FBQyxTQUFTLENBQUMsR0FBRyxDQUFDLFNBQVMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxTQUFTLENBQUMsTUFBTSxDQUFDLFNBQVMsQ0FBQyxDQUFDO1lBQ3pFLENBQUMsQ0FBQyxDQUFDO1lBRUgsSUFBSSxDQUFDLGdCQUFnQixDQUFDLFdBQVcsRUFBRSxNQUFNLENBQUMsQ0FBQztZQUMzQyxJQUFJLENBQUMsa0JBQWtCLEdBQUcsV0FBVyxDQUFDO1lBQ3RDLE9BQU8sSUFBSSxDQUFDLGtCQUFrQixDQUFDO1FBQ2pDLENBQUM7UUFFRCx5REFBeUQ7UUFDakQsZ0JBQWdCLENBQUMsV0FBZ0MsRUFBRSxNQUF5QjtZQUNsRiw2REFBNkQ7WUFDN0QsV0FBVyxDQUFDLGNBQWMsRUFBRSxDQUFDLFNBQVMsQ0FBQyxHQUFHLEVBQUU7Z0JBQzFDLGlGQUFpRjtnQkFDakYsSUFBSSxJQUFJLENBQUMsa0JBQWtCLElBQUksV0FBVyxFQUFFO29CQUMxQyxJQUFJLENBQUMsa0JBQWtCLEdBQUcsSUFBSSxDQUFDO2lCQUNoQztnQkFFRCxJQUFJLE1BQU0sQ0FBQyxtQkFBbUIsRUFBRTtvQkFDOUIsSUFBSSxDQUFDLEtBQUssQ0FBQyxLQUFLLEVBQUUsQ0FBQztpQkFDcEI7WUFDSCxDQUFDLENBQUMsQ0FBQztZQUVILElBQUksSUFBSSxDQUFDLGtCQUFrQixFQUFFO2dCQUMzQiw4REFBOEQ7Z0JBQzlELGtEQUFrRDtnQkFDbEQsSUFBSSxDQUFDLGtCQUFrQixDQUFDLGNBQWMsRUFBRSxDQUFDLFNBQVMsQ0FBQyxHQUFHLEVBQUU7b0JBQ3RELFdBQVcsQ0FBQyxpQkFBaUIsQ0FBQyxLQUFLLEVBQUUsQ0FBQztnQkFDeEMsQ0FBQyxDQUFDLENBQUM7Z0JBQ0gsSUFBSSxDQUFDLGtCQUFrQixDQUFDLE9BQU8sRUFBRSxDQUFDO2FBQ25DO2lCQUFNO2dCQUNMLHVEQUF1RDtnQkFDdkQsV0FBVyxDQUFDLGlCQUFpQixDQUFDLEtBQUssRUFBRSxDQUFDO2FBQ3ZDO1lBRUQsMEZBQTBGO1lBQzFGLElBQUksTUFBTSxDQUFDLFFBQVEsSUFBSSxNQUFNLENBQUMsUUFBUSxHQUFHLENBQUMsRUFBRTtnQkFDMUMsV0FBVyxDQUFDLFdBQVcsRUFBRSxDQUFDLFNBQVMsQ0FBQyxHQUFHLEVBQUUsQ0FBQyxXQUFXLENBQUMsYUFBYSxDQUFDLE1BQU0sQ0FBQyxRQUFTLENBQUMsQ0FBQyxDQUFDO2FBQ3hGO1lBRUQsSUFBSSxNQUFNLENBQUMsbUJBQW1CLEVBQUU7Z0JBQzlCLElBQUksQ0FBQyxLQUFLLENBQUMsUUFBUSxDQUFDLE1BQU0sQ0FBQyxtQkFBbUIsRUFBRSxNQUFNLENBQUMsVUFBVSxDQUFDLENBQUM7YUFDcEU7UUFDSCxDQUFDO1FBRUQ7OztXQUdHO1FBQ0ssY0FBYyxDQUFDLE1BQXlCO1lBQzlDLE1BQU0sYUFBYSxHQUFHLElBQUksYUFBYSxFQUFFLENBQUM7WUFDMUMsYUFBYSxDQUFDLFNBQVMsR0FBRyxNQUFNLENBQUMsU0FBUyxDQUFDO1lBRTNDLElBQUksZ0JBQWdCLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQyxRQUFRLEVBQUUsQ0FBQyxNQUFNLEVBQUUsQ0FBQztZQUN6RCwyQkFBMkI7WUFDM0IsTUFBTSxLQUFLLEdBQUcsTUFBTSxDQUFDLFNBQVMsS0FBSyxLQUFLLENBQUM7WUFDekMsTUFBTSxNQUFNLEdBQUcsQ0FDYixNQUFNLENBQUMsa0JBQWtCLEtBQUssTUFBTTtnQkFDcEMsQ0FBQyxNQUFNLENBQUMsa0JBQWtCLEtBQUssT0FBTyxJQUFJLENBQUMsS0FBSyxDQUFDO2dCQUNqRCxDQUFDLE1BQU0sQ0FBQyxrQkFBa0IsS0FBSyxLQUFLLElBQUksS0FBSyxDQUFDLENBQUMsQ0FBQztZQUNsRCxNQUFNLE9BQU8sR0FBRyxDQUFDLE1BQU0sSUFBSSxNQUFNLENBQUMsa0JBQWtCLEtBQUssUUFBUSxDQUFDO1lBQ2xFLElBQUksTUFBTSxFQUFFO2dCQUNWLGdCQUFnQixDQUFDLElBQUksQ0FBQyxHQUFHLENBQUMsQ0FBQzthQUM1QjtpQkFBTSxJQUFJLE9BQU8sRUFBRTtnQkFDbEIsZ0JBQWdCLENBQUMsS0FBSyxDQUFDLEdBQUcsQ0FBQyxDQUFDO2FBQzdCO2lCQUFNO2dCQUNMLGdCQUFnQixDQUFDLGtCQUFrQixFQUFFLENBQUM7YUFDdkM7WUFDRCwyQkFBMkI7WUFDM0IsSUFBSSxNQUFNLENBQUMsZ0JBQWdCLEtBQUssS0FBSyxFQUFFO2dCQUNyQyxnQkFBZ0IsQ0FBQyxHQUFHLENBQUMsR0FBRyxDQUFDLENBQUM7YUFDM0I7aUJBQU07Z0JBQ0wsZ0JBQWdCLENBQUMsTUFBTSxDQUFDLEdBQUcsQ0FBQyxDQUFDO2FBQzlCO1lBRUQsYUFBYSxDQUFDLGdCQUFnQixHQUFHLGdCQUFnQixDQUFDO1lBQ2xELE9BQU8sSUFBSSxDQUFDLFFBQVEsQ0FBQyxNQUFNLENBQUMsYUFBYSxDQUFDLENBQUM7UUFDN0MsQ0FBQztRQUVEOzs7O1dBSUc7UUFDSyxlQUFlLENBQ25CLE1BQXlCLEVBQ3pCLFdBQThCO1lBRWhDLE1BQU0sWUFBWSxHQUFHLE1BQU0sSUFBSSxNQUFNLENBQUMsZ0JBQWdCLElBQUksTUFBTSxDQUFDLGdCQUFnQixDQUFDLFFBQVEsQ0FBQztZQUUzRixPQUFPLElBQUksY0FBYyxDQUFDLFlBQVksSUFBSSxJQUFJLENBQUMsU0FBUyxFQUFFLElBQUksT0FBTyxDQUFXO2dCQUM5RSxDQUFDLGNBQWMsRUFBRSxXQUFXLENBQUM7Z0JBQzdCLENBQUMsa0JBQWtCLEVBQUUsTUFBTSxDQUFDLElBQUksQ0FBQzthQUNsQyxDQUFDLENBQUMsQ0FBQztRQUNOLENBQUM7Ozs7Z0JBOU9GLFVBQVUsU0FBQyxFQUFDLFVBQVUsRUFBRSxpQkFBaUIsRUFBQzs7O2dCQXJDbkMsT0FBTztnQkFGUCxhQUFhO2dCQVVuQixRQUFRO2dCQVRGLGtCQUFrQjtnQkFrRTZCLFdBQVcsdUJBQTNELFFBQVEsWUFBSSxRQUFRO2dCQWpEQyxpQkFBaUIsdUJBa0R0QyxNQUFNLFNBQUMsNkJBQTZCOztzQkE1RTNDO0tBOFJDO1NBOU9ZLFdBQVciLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtMaXZlQW5ub3VuY2VyfSBmcm9tICdAYW5ndWxhci9jZGsvYTExeSc7XG5pbXBvcnQge0JyZWFrcG9pbnRPYnNlcnZlciwgQnJlYWtwb2ludHN9IGZyb20gJ0Bhbmd1bGFyL2Nkay9sYXlvdXQnO1xuaW1wb3J0IHtPdmVybGF5LCBPdmVybGF5Q29uZmlnLCBPdmVybGF5UmVmfSBmcm9tICdAYW5ndWxhci9jZGsvb3ZlcmxheSc7XG5pbXBvcnQge0NvbXBvbmVudFBvcnRhbCwgQ29tcG9uZW50VHlwZSwgUG9ydGFsSW5qZWN0b3IsIFRlbXBsYXRlUG9ydGFsfSBmcm9tICdAYW5ndWxhci9jZGsvcG9ydGFsJztcbmltcG9ydCB7XG4gIENvbXBvbmVudFJlZixcbiAgRW1iZWRkZWRWaWV3UmVmLFxuICBJbmplY3QsXG4gIEluamVjdGFibGUsXG4gIEluamVjdGlvblRva2VuLFxuICBJbmplY3RvcixcbiAgT3B0aW9uYWwsXG4gIFNraXBTZWxmLFxuICBUZW1wbGF0ZVJlZixcbiAgT25EZXN0cm95LFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7dGFrZVVudGlsfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5pbXBvcnQge1NpbXBsZVNuYWNrQmFyfSBmcm9tICcuL3NpbXBsZS1zbmFjay1iYXInO1xuaW1wb3J0IHtNQVRfU05BQ0tfQkFSX0RBVEEsIE1hdFNuYWNrQmFyQ29uZmlnfSBmcm9tICcuL3NuYWNrLWJhci1jb25maWcnO1xuaW1wb3J0IHtNYXRTbmFja0JhckNvbnRhaW5lcn0gZnJvbSAnLi9zbmFjay1iYXItY29udGFpbmVyJztcbmltcG9ydCB7TWF0U25hY2tCYXJNb2R1bGV9IGZyb20gJy4vc25hY2stYmFyLW1vZHVsZSc7XG5pbXBvcnQge01hdFNuYWNrQmFyUmVmfSBmcm9tICcuL3NuYWNrLWJhci1yZWYnO1xuXG5cbi8qKiBJbmplY3Rpb24gdG9rZW4gdGhhdCBjYW4gYmUgdXNlZCB0byBzcGVjaWZ5IGRlZmF1bHQgc25hY2sgYmFyLiAqL1xuZXhwb3J0IGNvbnN0IE1BVF9TTkFDS19CQVJfREVGQVVMVF9PUFRJT05TID1cbiAgICBuZXcgSW5qZWN0aW9uVG9rZW48TWF0U25hY2tCYXJDb25maWc+KCdtYXQtc25hY2stYmFyLWRlZmF1bHQtb3B0aW9ucycsIHtcbiAgICAgIHByb3ZpZGVkSW46ICdyb290JyxcbiAgICAgIGZhY3Rvcnk6IE1BVF9TTkFDS19CQVJfREVGQVVMVF9PUFRJT05TX0ZBQ1RPUlksXG4gICAgfSk7XG5cbi8qKiBAZG9jcy1wcml2YXRlICovXG5leHBvcnQgZnVuY3Rpb24gTUFUX1NOQUNLX0JBUl9ERUZBVUxUX09QVElPTlNfRkFDVE9SWSgpOiBNYXRTbmFja0JhckNvbmZpZyB7XG4gIHJldHVybiBuZXcgTWF0U25hY2tCYXJDb25maWcoKTtcbn1cblxuLyoqXG4gKiBTZXJ2aWNlIHRvIGRpc3BhdGNoIE1hdGVyaWFsIERlc2lnbiBzbmFjayBiYXIgbWVzc2FnZXMuXG4gKi9cbkBJbmplY3RhYmxlKHtwcm92aWRlZEluOiBNYXRTbmFja0Jhck1vZHVsZX0pXG5leHBvcnQgY2xhc3MgTWF0U25hY2tCYXIgaW1wbGVtZW50cyBPbkRlc3Ryb3kge1xuICAvKipcbiAgICogUmVmZXJlbmNlIHRvIHRoZSBjdXJyZW50IHNuYWNrIGJhciBpbiB0aGUgdmlldyAqYXQgdGhpcyBsZXZlbCogKGluIHRoZSBBbmd1bGFyIGluamVjdG9yIHRyZWUpLlxuICAgKiBJZiB0aGVyZSBpcyBhIHBhcmVudCBzbmFjay1iYXIgc2VydmljZSwgYWxsIG9wZXJhdGlvbnMgc2hvdWxkIGRlbGVnYXRlIHRvIHRoYXQgcGFyZW50XG4gICAqIHZpYSBgX29wZW5lZFNuYWNrQmFyUmVmYC5cbiAgICovXG4gIHByaXZhdGUgX3NuYWNrQmFyUmVmQXRUaGlzTGV2ZWw6IE1hdFNuYWNrQmFyUmVmPGFueT4gfCBudWxsID0gbnVsbDtcblxuICAvKiogUmVmZXJlbmNlIHRvIHRoZSBjdXJyZW50bHkgb3BlbmVkIHNuYWNrYmFyIGF0ICphbnkqIGxldmVsLiAqL1xuICBnZXQgX29wZW5lZFNuYWNrQmFyUmVmKCk6IE1hdFNuYWNrQmFyUmVmPGFueT4gfCBudWxsIHtcbiAgICBjb25zdCBwYXJlbnQgPSB0aGlzLl9wYXJlbnRTbmFja0JhcjtcbiAgICByZXR1cm4gcGFyZW50ID8gcGFyZW50Ll9vcGVuZWRTbmFja0JhclJlZiA6IHRoaXMuX3NuYWNrQmFyUmVmQXRUaGlzTGV2ZWw7XG4gIH1cblxuICBzZXQgX29wZW5lZFNuYWNrQmFyUmVmKHZhbHVlOiBNYXRTbmFja0JhclJlZjxhbnk+IHwgbnVsbCkge1xuICAgIGlmICh0aGlzLl9wYXJlbnRTbmFja0Jhcikge1xuICAgICAgdGhpcy5fcGFyZW50U25hY2tCYXIuX29wZW5lZFNuYWNrQmFyUmVmID0gdmFsdWU7XG4gICAgfSBlbHNlIHtcbiAgICAgIHRoaXMuX3NuYWNrQmFyUmVmQXRUaGlzTGV2ZWwgPSB2YWx1ZTtcbiAgICB9XG4gIH1cblxuICBjb25zdHJ1Y3RvcihcbiAgICAgIHByaXZhdGUgX292ZXJsYXk6IE92ZXJsYXksXG4gICAgICBwcml2YXRlIF9saXZlOiBMaXZlQW5ub3VuY2VyLFxuICAgICAgcHJpdmF0ZSBfaW5qZWN0b3I6IEluamVjdG9yLFxuICAgICAgcHJpdmF0ZSBfYnJlYWtwb2ludE9ic2VydmVyOiBCcmVha3BvaW50T2JzZXJ2ZXIsXG4gICAgICBAT3B0aW9uYWwoKSBAU2tpcFNlbGYoKSBwcml2YXRlIF9wYXJlbnRTbmFja0JhcjogTWF0U25hY2tCYXIsXG4gICAgICBASW5qZWN0KE1BVF9TTkFDS19CQVJfREVGQVVMVF9PUFRJT05TKSBwcml2YXRlIF9kZWZhdWx0Q29uZmlnOiBNYXRTbmFja0JhckNvbmZpZykge31cblxuICAvKipcbiAgICogQ3JlYXRlcyBhbmQgZGlzcGF0Y2hlcyBhIHNuYWNrIGJhciB3aXRoIGEgY3VzdG9tIGNvbXBvbmVudCBmb3IgdGhlIGNvbnRlbnQsIHJlbW92aW5nIGFueVxuICAgKiBjdXJyZW50bHkgb3BlbmVkIHNuYWNrIGJhcnMuXG4gICAqXG4gICAqIEBwYXJhbSBjb21wb25lbnQgQ29tcG9uZW50IHRvIGJlIGluc3RhbnRpYXRlZC5cbiAgICogQHBhcmFtIGNvbmZpZyBFeHRyYSBjb25maWd1cmF0aW9uIGZvciB0aGUgc25hY2sgYmFyLlxuICAgKi9cbiAgb3BlbkZyb21Db21wb25lbnQ8VD4oY29tcG9uZW50OiBDb21wb25lbnRUeXBlPFQ+LCBjb25maWc/OiBNYXRTbmFja0JhckNvbmZpZyk6XG4gICAgTWF0U25hY2tCYXJSZWY8VD4ge1xuICAgIHJldHVybiB0aGlzLl9hdHRhY2goY29tcG9uZW50LCBjb25maWcpIGFzIE1hdFNuYWNrQmFyUmVmPFQ+O1xuICB9XG5cbiAgLyoqXG4gICAqIENyZWF0ZXMgYW5kIGRpc3BhdGNoZXMgYSBzbmFjayBiYXIgd2l0aCBhIGN1c3RvbSB0ZW1wbGF0ZSBmb3IgdGhlIGNvbnRlbnQsIHJlbW92aW5nIGFueVxuICAgKiBjdXJyZW50bHkgb3BlbmVkIHNuYWNrIGJhcnMuXG4gICAqXG4gICAqIEBwYXJhbSB0ZW1wbGF0ZSBUZW1wbGF0ZSB0byBiZSBpbnN0YW50aWF0ZWQuXG4gICAqIEBwYXJhbSBjb25maWcgRXh0cmEgY29uZmlndXJhdGlvbiBmb3IgdGhlIHNuYWNrIGJhci5cbiAgICovXG4gIG9wZW5Gcm9tVGVtcGxhdGUodGVtcGxhdGU6IFRlbXBsYXRlUmVmPGFueT4sIGNvbmZpZz86IE1hdFNuYWNrQmFyQ29uZmlnKTpcbiAgICBNYXRTbmFja0JhclJlZjxFbWJlZGRlZFZpZXdSZWY8YW55Pj4ge1xuICAgIHJldHVybiB0aGlzLl9hdHRhY2godGVtcGxhdGUsIGNvbmZpZyk7XG4gIH1cblxuICAvKipcbiAgICogT3BlbnMgYSBzbmFja2JhciB3aXRoIGEgbWVzc2FnZSBhbmQgYW4gb3B0aW9uYWwgYWN0aW9uLlxuICAgKiBAcGFyYW0gbWVzc2FnZSBUaGUgbWVzc2FnZSB0byBzaG93IGluIHRoZSBzbmFja2Jhci5cbiAgICogQHBhcmFtIGFjdGlvbiBUaGUgbGFiZWwgZm9yIHRoZSBzbmFja2JhciBhY3Rpb24uXG4gICAqIEBwYXJhbSBjb25maWcgQWRkaXRpb25hbCBjb25maWd1cmF0aW9uIG9wdGlvbnMgZm9yIHRoZSBzbmFja2Jhci5cbiAgICovXG4gIG9wZW4obWVzc2FnZTogc3RyaW5nLCBhY3Rpb246IHN0cmluZyA9ICcnLCBjb25maWc/OiBNYXRTbmFja0JhckNvbmZpZyk6XG4gICAgICBNYXRTbmFja0JhclJlZjxTaW1wbGVTbmFja0Jhcj4ge1xuICAgIGNvbnN0IF9jb25maWcgPSB7Li4udGhpcy5fZGVmYXVsdENvbmZpZywgLi4uY29uZmlnfTtcblxuICAgIC8vIFNpbmNlIHRoZSB1c2VyIGRvZXNuJ3QgaGF2ZSBhY2Nlc3MgdG8gdGhlIGNvbXBvbmVudCwgd2UgY2FuXG4gICAgLy8gb3ZlcnJpZGUgdGhlIGRhdGEgdG8gcGFzcyBpbiBvdXIgb3duIG1lc3NhZ2UgYW5kIGFjdGlvbi5cbiAgICBfY29uZmlnLmRhdGEgPSB7bWVzc2FnZSwgYWN0aW9ufTtcblxuICAgIGlmICghX2NvbmZpZy5hbm5vdW5jZW1lbnRNZXNzYWdlKSB7XG4gICAgICBfY29uZmlnLmFubm91bmNlbWVudE1lc3NhZ2UgPSBtZXNzYWdlO1xuICAgIH1cblxuICAgIHJldHVybiB0aGlzLm9wZW5Gcm9tQ29tcG9uZW50KFNpbXBsZVNuYWNrQmFyLCBfY29uZmlnKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBEaXNtaXNzZXMgdGhlIGN1cnJlbnRseS12aXNpYmxlIHNuYWNrIGJhci5cbiAgICovXG4gIGRpc21pc3MoKTogdm9pZCB7XG4gICAgaWYgKHRoaXMuX29wZW5lZFNuYWNrQmFyUmVmKSB7XG4gICAgICB0aGlzLl9vcGVuZWRTbmFja0JhclJlZi5kaXNtaXNzKCk7XG4gICAgfVxuICB9XG5cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgLy8gT25seSBkaXNtaXNzIHRoZSBzbmFjayBiYXIgYXQgdGhlIGN1cnJlbnQgbGV2ZWwgb24gZGVzdHJveS5cbiAgICBpZiAodGhpcy5fc25hY2tCYXJSZWZBdFRoaXNMZXZlbCkge1xuICAgICAgdGhpcy5fc25hY2tCYXJSZWZBdFRoaXNMZXZlbC5kaXNtaXNzKCk7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIEF0dGFjaGVzIHRoZSBzbmFjayBiYXIgY29udGFpbmVyIGNvbXBvbmVudCB0byB0aGUgb3ZlcmxheS5cbiAgICovXG4gIHByaXZhdGUgX2F0dGFjaFNuYWNrQmFyQ29udGFpbmVyKG92ZXJsYXlSZWY6IE92ZXJsYXlSZWYsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNvbmZpZzogTWF0U25hY2tCYXJDb25maWcpOiBNYXRTbmFja0JhckNvbnRhaW5lciB7XG5cbiAgICBjb25zdCB1c2VySW5qZWN0b3IgPSBjb25maWcgJiYgY29uZmlnLnZpZXdDb250YWluZXJSZWYgJiYgY29uZmlnLnZpZXdDb250YWluZXJSZWYuaW5qZWN0b3I7XG4gICAgY29uc3QgaW5qZWN0b3IgPSBuZXcgUG9ydGFsSW5qZWN0b3IodXNlckluamVjdG9yIHx8IHRoaXMuX2luamVjdG9yLCBuZXcgV2Vha01hcChbXG4gICAgICBbTWF0U25hY2tCYXJDb25maWcsIGNvbmZpZ11cbiAgICBdKSk7XG5cbiAgICBjb25zdCBjb250YWluZXJQb3J0YWwgPVxuICAgICAgICBuZXcgQ29tcG9uZW50UG9ydGFsKE1hdFNuYWNrQmFyQ29udGFpbmVyLCBjb25maWcudmlld0NvbnRhaW5lclJlZiwgaW5qZWN0b3IpO1xuICAgIGNvbnN0IGNvbnRhaW5lclJlZjogQ29tcG9uZW50UmVmPE1hdFNuYWNrQmFyQ29udGFpbmVyPiA9IG92ZXJsYXlSZWYuYXR0YWNoKGNvbnRhaW5lclBvcnRhbCk7XG4gICAgY29udGFpbmVyUmVmLmluc3RhbmNlLnNuYWNrQmFyQ29uZmlnID0gY29uZmlnO1xuICAgIHJldHVybiBjb250YWluZXJSZWYuaW5zdGFuY2U7XG4gIH1cblxuICAvKipcbiAgICogUGxhY2VzIGEgbmV3IGNvbXBvbmVudCBvciBhIHRlbXBsYXRlIGFzIHRoZSBjb250ZW50IG9mIHRoZSBzbmFjayBiYXIgY29udGFpbmVyLlxuICAgKi9cbiAgcHJpdmF0ZSBfYXR0YWNoPFQ+KGNvbnRlbnQ6IENvbXBvbmVudFR5cGU8VD4gfCBUZW1wbGF0ZVJlZjxUPiwgdXNlckNvbmZpZz86IE1hdFNuYWNrQmFyQ29uZmlnKTpcbiAgICBNYXRTbmFja0JhclJlZjxUIHwgRW1iZWRkZWRWaWV3UmVmPGFueT4+IHtcblxuICAgIGNvbnN0IGNvbmZpZyA9IHsuLi5uZXcgTWF0U25hY2tCYXJDb25maWcoKSwgLi4udGhpcy5fZGVmYXVsdENvbmZpZywgLi4udXNlckNvbmZpZ307XG4gICAgY29uc3Qgb3ZlcmxheVJlZiA9IHRoaXMuX2NyZWF0ZU92ZXJsYXkoY29uZmlnKTtcbiAgICBjb25zdCBjb250YWluZXIgPSB0aGlzLl9hdHRhY2hTbmFja0JhckNvbnRhaW5lcihvdmVybGF5UmVmLCBjb25maWcpO1xuICAgIGNvbnN0IHNuYWNrQmFyUmVmID0gbmV3IE1hdFNuYWNrQmFyUmVmPFQgfCBFbWJlZGRlZFZpZXdSZWY8YW55Pj4oY29udGFpbmVyLCBvdmVybGF5UmVmKTtcblxuICAgIGlmIChjb250ZW50IGluc3RhbmNlb2YgVGVtcGxhdGVSZWYpIHtcbiAgICAgIGNvbnN0IHBvcnRhbCA9IG5ldyBUZW1wbGF0ZVBvcnRhbChjb250ZW50LCBudWxsISwge1xuICAgICAgICAkaW1wbGljaXQ6IGNvbmZpZy5kYXRhLFxuICAgICAgICBzbmFja0JhclJlZlxuICAgICAgfSBhcyBhbnkpO1xuXG4gICAgICBzbmFja0JhclJlZi5pbnN0YW5jZSA9IGNvbnRhaW5lci5hdHRhY2hUZW1wbGF0ZVBvcnRhbChwb3J0YWwpO1xuICAgIH0gZWxzZSB7XG4gICAgICBjb25zdCBpbmplY3RvciA9IHRoaXMuX2NyZWF0ZUluamVjdG9yKGNvbmZpZywgc25hY2tCYXJSZWYpO1xuICAgICAgY29uc3QgcG9ydGFsID0gbmV3IENvbXBvbmVudFBvcnRhbChjb250ZW50LCB1bmRlZmluZWQsIGluamVjdG9yKTtcbiAgICAgIGNvbnN0IGNvbnRlbnRSZWYgPSBjb250YWluZXIuYXR0YWNoQ29tcG9uZW50UG9ydGFsPFQ+KHBvcnRhbCk7XG5cbiAgICAgIC8vIFdlIGNhbid0IHBhc3MgdGhpcyB2aWEgdGhlIGluamVjdG9yLCBiZWNhdXNlIHRoZSBpbmplY3RvciBpcyBjcmVhdGVkIGVhcmxpZXIuXG4gICAgICBzbmFja0JhclJlZi5pbnN0YW5jZSA9IGNvbnRlbnRSZWYuaW5zdGFuY2U7XG4gICAgfVxuXG4gICAgLy8gU3Vic2NyaWJlIHRvIHRoZSBicmVha3BvaW50IG9ic2VydmVyIGFuZCBhdHRhY2ggdGhlIG1hdC1zbmFjay1iYXItaGFuZHNldCBjbGFzcyBhc1xuICAgIC8vIGFwcHJvcHJpYXRlLiBUaGlzIGNsYXNzIGlzIGFwcGxpZWQgdG8gdGhlIG92ZXJsYXkgZWxlbWVudCBiZWNhdXNlIHRoZSBvdmVybGF5IG11c3QgZXhwYW5kIHRvXG4gICAgLy8gZmlsbCB0aGUgd2lkdGggb2YgdGhlIHNjcmVlbiBmb3IgZnVsbCB3aWR0aCBzbmFja2JhcnMuXG4gICAgdGhpcy5fYnJlYWtwb2ludE9ic2VydmVyLm9ic2VydmUoQnJlYWtwb2ludHMuSGFuZHNldFBvcnRyYWl0KS5waXBlKFxuICAgICAgdGFrZVVudGlsKG92ZXJsYXlSZWYuZGV0YWNobWVudHMoKSlcbiAgICApLnN1YnNjcmliZShzdGF0ZSA9PiB7XG4gICAgICBjb25zdCBjbGFzc0xpc3QgPSBvdmVybGF5UmVmLm92ZXJsYXlFbGVtZW50LmNsYXNzTGlzdDtcbiAgICAgIGNvbnN0IGNsYXNzTmFtZSA9ICdtYXQtc25hY2stYmFyLWhhbmRzZXQnO1xuICAgICAgc3RhdGUubWF0Y2hlcyA/IGNsYXNzTGlzdC5hZGQoY2xhc3NOYW1lKSA6IGNsYXNzTGlzdC5yZW1vdmUoY2xhc3NOYW1lKTtcbiAgICB9KTtcblxuICAgIHRoaXMuX2FuaW1hdGVTbmFja0JhcihzbmFja0JhclJlZiwgY29uZmlnKTtcbiAgICB0aGlzLl9vcGVuZWRTbmFja0JhclJlZiA9IHNuYWNrQmFyUmVmO1xuICAgIHJldHVybiB0aGlzLl9vcGVuZWRTbmFja0JhclJlZjtcbiAgfVxuXG4gIC8qKiBBbmltYXRlcyB0aGUgb2xkIHNuYWNrIGJhciBvdXQgYW5kIHRoZSBuZXcgb25lIGluLiAqL1xuICBwcml2YXRlIF9hbmltYXRlU25hY2tCYXIoc25hY2tCYXJSZWY6IE1hdFNuYWNrQmFyUmVmPGFueT4sIGNvbmZpZzogTWF0U25hY2tCYXJDb25maWcpIHtcbiAgICAvLyBXaGVuIHRoZSBzbmFja2JhciBpcyBkaXNtaXNzZWQsIGNsZWFyIHRoZSByZWZlcmVuY2UgdG8gaXQuXG4gICAgc25hY2tCYXJSZWYuYWZ0ZXJEaXNtaXNzZWQoKS5zdWJzY3JpYmUoKCkgPT4ge1xuICAgICAgLy8gQ2xlYXIgdGhlIHNuYWNrYmFyIHJlZiBpZiBpdCBoYXNuJ3QgYWxyZWFkeSBiZWVuIHJlcGxhY2VkIGJ5IGEgbmV3ZXIgc25hY2tiYXIuXG4gICAgICBpZiAodGhpcy5fb3BlbmVkU25hY2tCYXJSZWYgPT0gc25hY2tCYXJSZWYpIHtcbiAgICAgICAgdGhpcy5fb3BlbmVkU25hY2tCYXJSZWYgPSBudWxsO1xuICAgICAgfVxuXG4gICAgICBpZiAoY29uZmlnLmFubm91bmNlbWVudE1lc3NhZ2UpIHtcbiAgICAgICAgdGhpcy5fbGl2ZS5jbGVhcigpO1xuICAgICAgfVxuICAgIH0pO1xuXG4gICAgaWYgKHRoaXMuX29wZW5lZFNuYWNrQmFyUmVmKSB7XG4gICAgICAvLyBJZiBhIHNuYWNrIGJhciBpcyBhbHJlYWR5IGluIHZpZXcsIGRpc21pc3MgaXQgYW5kIGVudGVyIHRoZVxuICAgICAgLy8gbmV3IHNuYWNrIGJhciBhZnRlciBleGl0IGFuaW1hdGlvbiBpcyBjb21wbGV0ZS5cbiAgICAgIHRoaXMuX29wZW5lZFNuYWNrQmFyUmVmLmFmdGVyRGlzbWlzc2VkKCkuc3Vic2NyaWJlKCgpID0+IHtcbiAgICAgICAgc25hY2tCYXJSZWYuY29udGFpbmVySW5zdGFuY2UuZW50ZXIoKTtcbiAgICAgIH0pO1xuICAgICAgdGhpcy5fb3BlbmVkU25hY2tCYXJSZWYuZGlzbWlzcygpO1xuICAgIH0gZWxzZSB7XG4gICAgICAvLyBJZiBubyBzbmFjayBiYXIgaXMgaW4gdmlldywgZW50ZXIgdGhlIG5ldyBzbmFjayBiYXIuXG4gICAgICBzbmFja0JhclJlZi5jb250YWluZXJJbnN0YW5jZS5lbnRlcigpO1xuICAgIH1cblxuICAgIC8vIElmIGEgZGlzbWlzcyB0aW1lb3V0IGlzIHByb3ZpZGVkLCBzZXQgdXAgZGlzbWlzcyBiYXNlZCBvbiBhZnRlciB0aGUgc25hY2tiYXIgaXMgb3BlbmVkLlxuICAgIGlmIChjb25maWcuZHVyYXRpb24gJiYgY29uZmlnLmR1cmF0aW9uID4gMCkge1xuICAgICAgc25hY2tCYXJSZWYuYWZ0ZXJPcGVuZWQoKS5zdWJzY3JpYmUoKCkgPT4gc25hY2tCYXJSZWYuX2Rpc21pc3NBZnRlcihjb25maWcuZHVyYXRpb24hKSk7XG4gICAgfVxuXG4gICAgaWYgKGNvbmZpZy5hbm5vdW5jZW1lbnRNZXNzYWdlKSB7XG4gICAgICB0aGlzLl9saXZlLmFubm91bmNlKGNvbmZpZy5hbm5vdW5jZW1lbnRNZXNzYWdlLCBjb25maWcucG9saXRlbmVzcyk7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIENyZWF0ZXMgYSBuZXcgb3ZlcmxheSBhbmQgcGxhY2VzIGl0IGluIHRoZSBjb3JyZWN0IGxvY2F0aW9uLlxuICAgKiBAcGFyYW0gY29uZmlnIFRoZSB1c2VyLXNwZWNpZmllZCBzbmFjayBiYXIgY29uZmlnLlxuICAgKi9cbiAgcHJpdmF0ZSBfY3JlYXRlT3ZlcmxheShjb25maWc6IE1hdFNuYWNrQmFyQ29uZmlnKTogT3ZlcmxheVJlZiB7XG4gICAgY29uc3Qgb3ZlcmxheUNvbmZpZyA9IG5ldyBPdmVybGF5Q29uZmlnKCk7XG4gICAgb3ZlcmxheUNvbmZpZy5kaXJlY3Rpb24gPSBjb25maWcuZGlyZWN0aW9uO1xuXG4gICAgbGV0IHBvc2l0aW9uU3RyYXRlZ3kgPSB0aGlzLl9vdmVybGF5LnBvc2l0aW9uKCkuZ2xvYmFsKCk7XG4gICAgLy8gU2V0IGhvcml6b250YWwgcG9zaXRpb24uXG4gICAgY29uc3QgaXNSdGwgPSBjb25maWcuZGlyZWN0aW9uID09PSAncnRsJztcbiAgICBjb25zdCBpc0xlZnQgPSAoXG4gICAgICBjb25maWcuaG9yaXpvbnRhbFBvc2l0aW9uID09PSAnbGVmdCcgfHxcbiAgICAgIChjb25maWcuaG9yaXpvbnRhbFBvc2l0aW9uID09PSAnc3RhcnQnICYmICFpc1J0bCkgfHxcbiAgICAgIChjb25maWcuaG9yaXpvbnRhbFBvc2l0aW9uID09PSAnZW5kJyAmJiBpc1J0bCkpO1xuICAgIGNvbnN0IGlzUmlnaHQgPSAhaXNMZWZ0ICYmIGNvbmZpZy5ob3Jpem9udGFsUG9zaXRpb24gIT09ICdjZW50ZXInO1xuICAgIGlmIChpc0xlZnQpIHtcbiAgICAgIHBvc2l0aW9uU3RyYXRlZ3kubGVmdCgnMCcpO1xuICAgIH0gZWxzZSBpZiAoaXNSaWdodCkge1xuICAgICAgcG9zaXRpb25TdHJhdGVneS5yaWdodCgnMCcpO1xuICAgIH0gZWxzZSB7XG4gICAgICBwb3NpdGlvblN0cmF0ZWd5LmNlbnRlckhvcml6b250YWxseSgpO1xuICAgIH1cbiAgICAvLyBTZXQgaG9yaXpvbnRhbCBwb3NpdGlvbi5cbiAgICBpZiAoY29uZmlnLnZlcnRpY2FsUG9zaXRpb24gPT09ICd0b3AnKSB7XG4gICAgICBwb3NpdGlvblN0cmF0ZWd5LnRvcCgnMCcpO1xuICAgIH0gZWxzZSB7XG4gICAgICBwb3NpdGlvblN0cmF0ZWd5LmJvdHRvbSgnMCcpO1xuICAgIH1cblxuICAgIG92ZXJsYXlDb25maWcucG9zaXRpb25TdHJhdGVneSA9IHBvc2l0aW9uU3RyYXRlZ3k7XG4gICAgcmV0dXJuIHRoaXMuX292ZXJsYXkuY3JlYXRlKG92ZXJsYXlDb25maWcpO1xuICB9XG5cbiAgLyoqXG4gICAqIENyZWF0ZXMgYW4gaW5qZWN0b3IgdG8gYmUgdXNlZCBpbnNpZGUgb2YgYSBzbmFjayBiYXIgY29tcG9uZW50LlxuICAgKiBAcGFyYW0gY29uZmlnIENvbmZpZyB0aGF0IHdhcyB1c2VkIHRvIGNyZWF0ZSB0aGUgc25hY2sgYmFyLlxuICAgKiBAcGFyYW0gc25hY2tCYXJSZWYgUmVmZXJlbmNlIHRvIHRoZSBzbmFjayBiYXIuXG4gICAqL1xuICBwcml2YXRlIF9jcmVhdGVJbmplY3RvcjxUPihcbiAgICAgIGNvbmZpZzogTWF0U25hY2tCYXJDb25maWcsXG4gICAgICBzbmFja0JhclJlZjogTWF0U25hY2tCYXJSZWY8VD4pOiBQb3J0YWxJbmplY3RvciB7XG5cbiAgICBjb25zdCB1c2VySW5qZWN0b3IgPSBjb25maWcgJiYgY29uZmlnLnZpZXdDb250YWluZXJSZWYgJiYgY29uZmlnLnZpZXdDb250YWluZXJSZWYuaW5qZWN0b3I7XG5cbiAgICByZXR1cm4gbmV3IFBvcnRhbEluamVjdG9yKHVzZXJJbmplY3RvciB8fCB0aGlzLl9pbmplY3RvciwgbmV3IFdlYWtNYXA8YW55LCBhbnk+KFtcbiAgICAgIFtNYXRTbmFja0JhclJlZiwgc25hY2tCYXJSZWZdLFxuICAgICAgW01BVF9TTkFDS19CQVJfREFUQSwgY29uZmlnLmRhdGFdXG4gICAgXSkpO1xuICB9XG59XG4iXX0=