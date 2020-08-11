/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Subject, merge, Subscription } from 'rxjs';
import { take, takeUntil } from 'rxjs/operators';
import { coerceCssPixelValue, coerceArray } from '@angular/cdk/coercion';
/**
 * Reference to an overlay that has been created with the Overlay service.
 * Used to manipulate or dispose of said overlay.
 */
export class OverlayRef {
    constructor(_portalOutlet, _host, _pane, _config, _ngZone, _keyboardDispatcher, _document, 
    // @breaking-change 8.0.0 `_location` parameter to be made required.
    _location) {
        this._portalOutlet = _portalOutlet;
        this._host = _host;
        this._pane = _pane;
        this._config = _config;
        this._ngZone = _ngZone;
        this._keyboardDispatcher = _keyboardDispatcher;
        this._document = _document;
        this._location = _location;
        this._backdropElement = null;
        this._backdropClick = new Subject();
        this._attachments = new Subject();
        this._detachments = new Subject();
        this._locationChanges = Subscription.EMPTY;
        this._backdropClickHandler = (event) => this._backdropClick.next(event);
        /** Stream of keydown events dispatched to this overlay. */
        this._keydownEvents = new Subject();
        if (_config.scrollStrategy) {
            this._scrollStrategy = _config.scrollStrategy;
            this._scrollStrategy.attach(this);
        }
        this._positionStrategy = _config.positionStrategy;
    }
    /** The overlay's HTML element */
    get overlayElement() {
        return this._pane;
    }
    /** The overlay's backdrop HTML element. */
    get backdropElement() {
        return this._backdropElement;
    }
    /**
     * Wrapper around the panel element. Can be used for advanced
     * positioning where a wrapper with specific styling is
     * required around the overlay pane.
     */
    get hostElement() {
        return this._host;
    }
    /**
     * Attaches content, given via a Portal, to the overlay.
     * If the overlay is configured to have a backdrop, it will be created.
     *
     * @param portal Portal instance to which to attach the overlay.
     * @returns The portal attachment result.
     */
    attach(portal) {
        let attachResult = this._portalOutlet.attach(portal);
        // Update the pane element with the given configuration.
        if (!this._host.parentElement && this._previousHostParent) {
            this._previousHostParent.appendChild(this._host);
        }
        if (this._positionStrategy) {
            this._positionStrategy.attach(this);
        }
        this._updateStackingOrder();
        this._updateElementSize();
        this._updateElementDirection();
        if (this._scrollStrategy) {
            this._scrollStrategy.enable();
        }
        // Update the position once the zone is stable so that the overlay will be fully rendered
        // before attempting to position it, as the position may depend on the size of the rendered
        // content.
        this._ngZone.onStable
            .asObservable()
            .pipe(take(1))
            .subscribe(() => {
            // The overlay could've been detached before the zone has stabilized.
            if (this.hasAttached()) {
                this.updatePosition();
            }
        });
        // Enable pointer events for the overlay pane element.
        this._togglePointerEvents(true);
        if (this._config.hasBackdrop) {
            this._attachBackdrop();
        }
        if (this._config.panelClass) {
            this._toggleClasses(this._pane, this._config.panelClass, true);
        }
        // Only emit the `attachments` event once all other setup is done.
        this._attachments.next();
        // Track this overlay by the keyboard dispatcher
        this._keyboardDispatcher.add(this);
        // @breaking-change 8.0.0 remove the null check for `_location`
        // once the constructor parameter is made required.
        if (this._config.disposeOnNavigation && this._location) {
            this._locationChanges = this._location.subscribe(() => this.dispose());
        }
        return attachResult;
    }
    /**
     * Detaches an overlay from a portal.
     * @returns The portal detachment result.
     */
    detach() {
        if (!this.hasAttached()) {
            return;
        }
        this.detachBackdrop();
        // When the overlay is detached, the pane element should disable pointer events.
        // This is necessary because otherwise the pane element will cover the page and disable
        // pointer events therefore. Depends on the position strategy and the applied pane boundaries.
        this._togglePointerEvents(false);
        if (this._positionStrategy && this._positionStrategy.detach) {
            this._positionStrategy.detach();
        }
        if (this._scrollStrategy) {
            this._scrollStrategy.disable();
        }
        const detachmentResult = this._portalOutlet.detach();
        // Only emit after everything is detached.
        this._detachments.next();
        // Remove this overlay from keyboard dispatcher tracking.
        this._keyboardDispatcher.remove(this);
        // Keeping the host element in the DOM can cause scroll jank, because it still gets
        // rendered, even though it's transparent and unclickable which is why we remove it.
        this._detachContentWhenStable();
        // Stop listening for location changes.
        this._locationChanges.unsubscribe();
        return detachmentResult;
    }
    /** Cleans up the overlay from the DOM. */
    dispose() {
        const isAttached = this.hasAttached();
        if (this._positionStrategy) {
            this._positionStrategy.dispose();
        }
        this._disposeScrollStrategy();
        this.detachBackdrop();
        this._locationChanges.unsubscribe();
        this._keyboardDispatcher.remove(this);
        this._portalOutlet.dispose();
        this._attachments.complete();
        this._backdropClick.complete();
        this._keydownEvents.complete();
        if (this._host && this._host.parentNode) {
            this._host.parentNode.removeChild(this._host);
            this._host = null;
        }
        this._previousHostParent = this._pane = null;
        if (isAttached) {
            this._detachments.next();
        }
        this._detachments.complete();
    }
    /** Whether the overlay has attached content. */
    hasAttached() {
        return this._portalOutlet.hasAttached();
    }
    /** Gets an observable that emits when the backdrop has been clicked. */
    backdropClick() {
        return this._backdropClick.asObservable();
    }
    /** Gets an observable that emits when the overlay has been attached. */
    attachments() {
        return this._attachments.asObservable();
    }
    /** Gets an observable that emits when the overlay has been detached. */
    detachments() {
        return this._detachments.asObservable();
    }
    /** Gets an observable of keydown events targeted to this overlay. */
    keydownEvents() {
        return this._keydownEvents.asObservable();
    }
    /** Gets the current overlay configuration, which is immutable. */
    getConfig() {
        return this._config;
    }
    /** Updates the position of the overlay based on the position strategy. */
    updatePosition() {
        if (this._positionStrategy) {
            this._positionStrategy.apply();
        }
    }
    /** Switches to a new position strategy and updates the overlay position. */
    updatePositionStrategy(strategy) {
        if (strategy === this._positionStrategy) {
            return;
        }
        if (this._positionStrategy) {
            this._positionStrategy.dispose();
        }
        this._positionStrategy = strategy;
        if (this.hasAttached()) {
            strategy.attach(this);
            this.updatePosition();
        }
    }
    /** Update the size properties of the overlay. */
    updateSize(sizeConfig) {
        this._config = Object.assign(Object.assign({}, this._config), sizeConfig);
        this._updateElementSize();
    }
    /** Sets the LTR/RTL direction for the overlay. */
    setDirection(dir) {
        this._config = Object.assign(Object.assign({}, this._config), { direction: dir });
        this._updateElementDirection();
    }
    /** Add a CSS class or an array of classes to the overlay pane. */
    addPanelClass(classes) {
        if (this._pane) {
            this._toggleClasses(this._pane, classes, true);
        }
    }
    /** Remove a CSS class or an array of classes from the overlay pane. */
    removePanelClass(classes) {
        if (this._pane) {
            this._toggleClasses(this._pane, classes, false);
        }
    }
    /**
     * Returns the layout direction of the overlay panel.
     */
    getDirection() {
        const direction = this._config.direction;
        if (!direction) {
            return 'ltr';
        }
        return typeof direction === 'string' ? direction : direction.value;
    }
    /** Switches to a new scroll strategy. */
    updateScrollStrategy(strategy) {
        if (strategy === this._scrollStrategy) {
            return;
        }
        this._disposeScrollStrategy();
        this._scrollStrategy = strategy;
        if (this.hasAttached()) {
            strategy.attach(this);
            strategy.enable();
        }
    }
    /** Updates the text direction of the overlay panel. */
    _updateElementDirection() {
        this._host.setAttribute('dir', this.getDirection());
    }
    /** Updates the size of the overlay element based on the overlay config. */
    _updateElementSize() {
        if (!this._pane) {
            return;
        }
        const style = this._pane.style;
        style.width = coerceCssPixelValue(this._config.width);
        style.height = coerceCssPixelValue(this._config.height);
        style.minWidth = coerceCssPixelValue(this._config.minWidth);
        style.minHeight = coerceCssPixelValue(this._config.minHeight);
        style.maxWidth = coerceCssPixelValue(this._config.maxWidth);
        style.maxHeight = coerceCssPixelValue(this._config.maxHeight);
    }
    /** Toggles the pointer events for the overlay pane element. */
    _togglePointerEvents(enablePointer) {
        this._pane.style.pointerEvents = enablePointer ? 'auto' : 'none';
    }
    /** Attaches a backdrop for this overlay. */
    _attachBackdrop() {
        const showingClass = 'cdk-overlay-backdrop-showing';
        this._backdropElement = this._document.createElement('div');
        this._backdropElement.classList.add('cdk-overlay-backdrop');
        if (this._config.backdropClass) {
            this._toggleClasses(this._backdropElement, this._config.backdropClass, true);
        }
        // Insert the backdrop before the pane in the DOM order,
        // in order to handle stacked overlays properly.
        this._host.parentElement.insertBefore(this._backdropElement, this._host);
        // Forward backdrop clicks such that the consumer of the overlay can perform whatever
        // action desired when such a click occurs (usually closing the overlay).
        this._backdropElement.addEventListener('click', this._backdropClickHandler);
        // Add class to fade-in the backdrop after one frame.
        if (typeof requestAnimationFrame !== 'undefined') {
            this._ngZone.runOutsideAngular(() => {
                requestAnimationFrame(() => {
                    if (this._backdropElement) {
                        this._backdropElement.classList.add(showingClass);
                    }
                });
            });
        }
        else {
            this._backdropElement.classList.add(showingClass);
        }
    }
    /**
     * Updates the stacking order of the element, moving it to the top if necessary.
     * This is required in cases where one overlay was detached, while another one,
     * that should be behind it, was destroyed. The next time both of them are opened,
     * the stacking will be wrong, because the detached element's pane will still be
     * in its original DOM position.
     */
    _updateStackingOrder() {
        if (this._host.nextSibling) {
            this._host.parentNode.appendChild(this._host);
        }
    }
    /** Detaches the backdrop (if any) associated with the overlay. */
    detachBackdrop() {
        let backdropToDetach = this._backdropElement;
        if (!backdropToDetach) {
            return;
        }
        let timeoutId;
        let finishDetach = () => {
            // It may not be attached to anything in certain cases (e.g. unit tests).
            if (backdropToDetach) {
                backdropToDetach.removeEventListener('click', this._backdropClickHandler);
                backdropToDetach.removeEventListener('transitionend', finishDetach);
                if (backdropToDetach.parentNode) {
                    backdropToDetach.parentNode.removeChild(backdropToDetach);
                }
            }
            // It is possible that a new portal has been attached to this overlay since we started
            // removing the backdrop. If that is the case, only clear the backdrop reference if it
            // is still the same instance that we started to remove.
            if (this._backdropElement == backdropToDetach) {
                this._backdropElement = null;
            }
            if (this._config.backdropClass) {
                this._toggleClasses(backdropToDetach, this._config.backdropClass, false);
            }
            clearTimeout(timeoutId);
        };
        backdropToDetach.classList.remove('cdk-overlay-backdrop-showing');
        this._ngZone.runOutsideAngular(() => {
            backdropToDetach.addEventListener('transitionend', finishDetach);
        });
        // If the backdrop doesn't have a transition, the `transitionend` event won't fire.
        // In this case we make it unclickable and we try to remove it after a delay.
        backdropToDetach.style.pointerEvents = 'none';
        // Run this outside the Angular zone because there's nothing that Angular cares about.
        // If it were to run inside the Angular zone, every test that used Overlay would have to be
        // either async or fakeAsync.
        timeoutId = this._ngZone.runOutsideAngular(() => setTimeout(finishDetach, 500));
    }
    /** Toggles a single CSS class or an array of classes on an element. */
    _toggleClasses(element, cssClasses, isAdd) {
        const classList = element.classList;
        coerceArray(cssClasses).forEach(cssClass => {
            // We can't do a spread here, because IE doesn't support setting multiple classes.
            // Also trying to add an empty string to a DOMTokenList will throw.
            if (cssClass) {
                isAdd ? classList.add(cssClass) : classList.remove(cssClass);
            }
        });
    }
    /** Detaches the overlay content next time the zone stabilizes. */
    _detachContentWhenStable() {
        // Normally we wouldn't have to explicitly run this outside the `NgZone`, however
        // if the consumer is using `zone-patch-rxjs`, the `Subscription.unsubscribe` call will
        // be patched to run inside the zone, which will throw us into an infinite loop.
        this._ngZone.runOutsideAngular(() => {
            // We can't remove the host here immediately, because the overlay pane's content
            // might still be animating. This stream helps us avoid interrupting the animation
            // by waiting for the pane to become empty.
            const subscription = this._ngZone.onStable
                .asObservable()
                .pipe(takeUntil(merge(this._attachments, this._detachments)))
                .subscribe(() => {
                // Needs a couple of checks for the pane and host, because
                // they may have been removed by the time the zone stabilizes.
                if (!this._pane || !this._host || this._pane.children.length === 0) {
                    if (this._pane && this._config.panelClass) {
                        this._toggleClasses(this._pane, this._config.panelClass, false);
                    }
                    if (this._host && this._host.parentElement) {
                        this._previousHostParent = this._host.parentElement;
                        this._previousHostParent.removeChild(this._host);
                    }
                    subscription.unsubscribe();
                }
            });
        });
    }
    /** Disposes of a scroll strategy. */
    _disposeScrollStrategy() {
        const scrollStrategy = this._scrollStrategy;
        if (scrollStrategy) {
            scrollStrategy.disable();
            if (scrollStrategy.detach) {
                scrollStrategy.detach();
            }
        }
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoib3ZlcmxheS1yZWYuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL292ZXJsYXkvb3ZlcmxheS1yZWYudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBTUgsT0FBTyxFQUFhLE9BQU8sRUFBRSxLQUFLLEVBQW9CLFlBQVksRUFBQyxNQUFNLE1BQU0sQ0FBQztBQUNoRixPQUFPLEVBQUMsSUFBSSxFQUFFLFNBQVMsRUFBQyxNQUFNLGdCQUFnQixDQUFDO0FBRy9DLE9BQU8sRUFBQyxtQkFBbUIsRUFBRSxXQUFXLEVBQUMsTUFBTSx1QkFBdUIsQ0FBQztBQVd2RTs7O0dBR0c7QUFDSCxNQUFNLE9BQU8sVUFBVTtJQW1CckIsWUFDWSxhQUEyQixFQUMzQixLQUFrQixFQUNsQixLQUFrQixFQUNsQixPQUF1QyxFQUN2QyxPQUFlLEVBQ2YsbUJBQThDLEVBQzlDLFNBQW1CO0lBQzNCLG9FQUFvRTtJQUM1RCxTQUFvQjtRQVJwQixrQkFBYSxHQUFiLGFBQWEsQ0FBYztRQUMzQixVQUFLLEdBQUwsS0FBSyxDQUFhO1FBQ2xCLFVBQUssR0FBTCxLQUFLLENBQWE7UUFDbEIsWUFBTyxHQUFQLE9BQU8sQ0FBZ0M7UUFDdkMsWUFBTyxHQUFQLE9BQU8sQ0FBUTtRQUNmLHdCQUFtQixHQUFuQixtQkFBbUIsQ0FBMkI7UUFDOUMsY0FBUyxHQUFULFNBQVMsQ0FBVTtRQUVuQixjQUFTLEdBQVQsU0FBUyxDQUFXO1FBM0J4QixxQkFBZ0IsR0FBdUIsSUFBSSxDQUFDO1FBQzVDLG1CQUFjLEdBQXdCLElBQUksT0FBTyxFQUFFLENBQUM7UUFDcEQsaUJBQVksR0FBRyxJQUFJLE9BQU8sRUFBUSxDQUFDO1FBQ25DLGlCQUFZLEdBQUcsSUFBSSxPQUFPLEVBQVEsQ0FBQztRQUduQyxxQkFBZ0IsR0FBcUIsWUFBWSxDQUFDLEtBQUssQ0FBQztRQUN4RCwwQkFBcUIsR0FBRyxDQUFDLEtBQWlCLEVBQUUs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YUFDekI7U0FDRjtJQUNILENBQUM7Q0FDRiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0RpcmVjdGlvbiwgRGlyZWN0aW9uYWxpdHl9IGZyb20gJ0Bhbmd1bGFyL2Nkay9iaWRpJztcbmltcG9ydCB7Q29tcG9uZW50UG9ydGFsLCBQb3J0YWwsIFBvcnRhbE91dGxldCwgVGVtcGxhdGVQb3J0YWx9IGZyb20gJ0Bhbmd1bGFyL2Nkay9wb3J0YWwnO1xuaW1wb3J0IHtDb21wb25lbnRSZWYsIEVtYmVkZGVkVmlld1JlZiwgTmdab25lfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7TG9jYXRpb259IGZyb20gJ0Bhbmd1bGFyL2NvbW1vbic7XG5pbXBvcnQge09ic2VydmFibGUsIFN1YmplY3QsIG1lcmdlLCBTdWJzY3JpcHRpb25MaWtlLCBTdWJzY3JpcHRpb259IGZyb20gJ3J4anMnO1xuaW1wb3J0IHt0YWtlLCB0YWtlVW50aWx9IGZyb20gJ3J4anMvb3BlcmF0b3JzJztcbmltcG9ydCB7T3ZlcmxheUtleWJvYXJkRGlzcGF0Y2hlcn0gZnJvbSAnLi9rZXlib2FyZC9vdmVybGF5LWtleWJvYXJkLWRpc3BhdGNoZXInO1xuaW1wb3J0IHtPdmVybGF5Q29uZmlnfSBmcm9tICcuL292ZXJsYXktY29uZmlnJztcbmltcG9ydCB7Y29lcmNlQ3NzUGl4ZWxWYWx1ZSwgY29lcmNlQXJyYXl9IGZyb20gJ0Bhbmd1bGFyL2Nkay9jb2VyY2lvbic7XG5pbXBvcnQge092ZXJsYXlSZWZlcmVuY2V9IGZyb20gJy4vb3ZlcmxheS1yZWZlcmVuY2UnO1xuaW1wb3J0IHtQb3NpdGlvblN0cmF0ZWd5fSBmcm9tICcuL3Bvc2l0aW9uL3Bvc2l0aW9uLXN0cmF0ZWd5JztcbmltcG9ydCB7U2Nyb2xsU3RyYXRlZ3l9IGZyb20gJy4vc2Nyb2xsJztcblxuXG4vKiogQW4gb2JqZWN0IHdoZXJlIGFsbCBvZiBpdHMgcHJvcGVydGllcyBjYW5ub3QgYmUgd3JpdHRlbi4gKi9cbmV4cG9ydCB0eXBlIEltbXV0YWJsZU9iamVjdDxUPiA9IHtcbiAgcmVhZG9ubHkgW1AgaW4ga2V5b2YgVF06IFRbUF07XG59O1xuXG4vKipcbiAqIFJlZmVyZW5jZSB0byBhbiBvdmVybGF5IHRoYXQgaGFzIGJlZW4gY3JlYXRlZCB3aXRoIHRoZSBPdmVybGF5IHNlcnZpY2UuXG4gKiBVc2VkIHRvIG1hbmlwdWxhdGUgb3IgZGlzcG9zZSBvZiBzYWlkIG92ZXJsYXkuXG4gKi9cbmV4cG9ydCBjbGFzcyBPdmVybGF5UmVmIGltcGxlbWVudHMgUG9ydGFsT3V0bGV0LCBPdmVybGF5UmVmZXJlbmNlIHtcbiAgcHJpdmF0ZSBfYmFja2Ryb3BFbGVtZW50OiBIVE1MRWxlbWVudCB8IG51bGwgPSBudWxsO1xuICBwcml2YXRlIF9iYWNrZHJvcENsaWNrOiBTdWJqZWN0PE1vdXNlRXZlbnQ+ID0gbmV3IFN1YmplY3QoKTtcbiAgcHJpdmF0ZSBfYXR0YWNobWVudHMgPSBuZXcgU3ViamVjdDx2b2lkPigpO1xuICBwcml2YXRlIF9kZXRhY2htZW50cyA9IG5ldyBTdWJqZWN0PHZvaWQ+KCk7XG4gIHByaXZhdGUgX3Bvc2l0aW9uU3RyYXRlZ3k6IFBvc2l0aW9uU3RyYXRlZ3kgfCB1bmRlZmluZWQ7XG4gIHByaXZhdGUgX3Njcm9sbFN0cmF0ZWd5OiBTY3JvbGxTdHJhdGVneSB8IHVuZGVmaW5lZDtcbiAgcHJpdmF0ZSBfbG9jYXRpb25DaGFuZ2VzOiBTdWJzY3JpcHRpb25MaWtlID0gU3Vic2NyaXB0aW9uLkVNUFRZO1xuICBwcml2YXRlIF9iYWNrZHJvcENsaWNrSGFuZGxlciA9IChldmVudDogTW91c2VFdmVudCkgPT4gdGhpcy5fYmFja2Ryb3BDbGljay5uZXh0KGV2ZW50KTtcblxuICAvKipcbiAgICogUmVmZXJlbmNlIHRvIHRoZSBwYXJlbnQgb2YgdGhlIGBfaG9zdGAgYXQgdGhlIHRpbWUgaXQgd2FzIGRldGFjaGVkLiBVc2VkIHRvIHJlc3RvcmVcbiAgICogdGhlIGBfaG9zdGAgdG8gaXRzIG9yaWdpbmFsIHBvc2l0aW9uIGluIHRoZSBET00gd2hlbiBpdCBnZXRzIHJlLWF0dGFjaGVkLlxuICAgKi9cbiAgcHJpdmF0ZSBfcHJldmlvdXNIb3N0UGFyZW50OiBIVE1MRWxlbWVudDtcblxuICAvKiogU3RyZWFtIG9mIGtleWRvd24gZXZlbnRzIGRpc3BhdGNoZWQgdG8gdGhpcyBvdmVybGF5LiAqL1xuICBfa2V5ZG93bkV2ZW50cyA9IG5ldyBTdWJqZWN0PEtleWJvYXJkRXZlbnQ+KCk7XG5cbiAgY29uc3RydWN0b3IoXG4gICAgICBwcml2YXRlIF9wb3J0YWxPdXRsZXQ6IFBvcnRhbE91dGxldCxcbiAgICAgIHByaXZhdGUgX2hvc3Q6IEhUTUxFbGVtZW50LFxuICAgICAgcHJpdmF0ZSBfcGFuZTogSFRNTEVsZW1lbnQsXG4gICAgICBwcml2YXRlIF9jb25maWc6IEltbXV0YWJsZU9iamVjdDxPdmVybGF5Q29uZmlnPixcbiAgICAgIHByaXZhdGUgX25nWm9uZTogTmdab25lLFxuICAgICAgcHJpdmF0ZSBfa2V5Ym9hcmREaXNwYXRjaGVyOiBPdmVybGF5S2V5Ym9hcmREaXNwYXRjaGVyLFxuICAgICAgcHJpdmF0ZSBfZG9jdW1lbnQ6IERvY3VtZW50LFxuICAgICAgLy8gQGJyZWFraW5nLWNoYW5nZSA4LjAuMCBgX2xvY2F0aW9uYCBwYXJhbWV0ZXIgdG8gYmUgbWFkZSByZXF1aXJlZC5cbiAgICAgIHByaXZhdGUgX2xvY2F0aW9uPzogTG9jYXRpb24pIHtcblxuICAgIGlmIChfY29uZmlnLnNjcm9sbFN0cmF0ZWd5KSB7XG4gICAgICB0aGlzLl9zY3JvbGxTdHJhdGVneSA9IF9jb25maWcuc2Nyb2xsU3RyYXRlZ3k7XG4gICAgICB0aGlzLl9zY3JvbGxTdHJhdGVneS5hdHRhY2godGhpcyk7XG4gICAgfVxuXG4gICAgdGhpcy5fcG9zaXRpb25TdHJhdGVneSA9IF9jb25maWcucG9zaXRpb25TdHJhdGVneTtcbiAgfVxuXG4gIC8qKiBUaGUgb3ZlcmxheSdzIEhUTUwgZWxlbWVudCAqL1xuICBnZXQgb3ZlcmxheUVsZW1lbnQoKTogSFRNTEVsZW1lbnQge1xuICAgIHJldHVybiB0aGlzLl9wYW5lO1xuICB9XG5cbiAgLyoqIFRoZSBvdmVybGF5J3MgYmFja2Ryb3AgSFRNTCBlbGVtZW50LiAqL1xuICBnZXQgYmFja2Ryb3BFbGVtZW50KCk6IEhUTUxFbGVtZW50IHwgbnVsbCB7XG4gICAgcmV0dXJuIHRoaXMuX2JhY2tkcm9wRWxlbWVudDtcbiAgfVxuXG4gIC8qKlxuICAgKiBXcmFwcGVyIGFyb3VuZCB0aGUgcGFuZWwgZWxlbWVudC4gQ2FuIGJlIHVzZWQgZm9yIGFkdmFuY2VkXG4gICAqIHBvc2l0aW9uaW5nIHdoZXJlIGEgd3JhcHBlciB3aXRoIHNwZWNpZmljIHN0eWxpbmcgaXNcbiAgICogcmVxdWlyZWQgYXJvdW5kIHRoZSBvdmVybGF5IHBhbmUuXG4gICAqL1xuICBnZXQgaG9zdEVsZW1lbnQoKTogSFRNTEVsZW1lbnQge1xuICAgIHJldHVybiB0aGlzLl9ob3N0O1xuICB9XG5cbiAgYXR0YWNoPFQ+KHBvcnRhbDogQ29tcG9uZW50UG9ydGFsPFQ+KTogQ29tcG9uZW50UmVmPFQ+O1xuICBhdHRhY2g8VD4ocG9ydGFsOiBUZW1wbGF0ZVBvcnRhbDxUPik6IEVtYmVkZGVkVmlld1JlZjxUPjtcbiAgYXR0YWNoKHBvcnRhbDogYW55KTogYW55O1xuXG4gIC8qKlxuICAgKiBBdHRhY2hlcyBjb250ZW50LCBnaXZlbiB2aWEgYSBQb3J0YWwsIHRvIHRoZSBvdmVybGF5LlxuICAgKiBJZiB0aGUgb3ZlcmxheSBpcyBjb25maWd1cmVkIHRvIGhhdmUgYSBiYWNrZHJvcCwgaXQgd2lsbCBiZSBjcmVhdGVkLlxuICAgKlxuICAgKiBAcGFyYW0gcG9ydGFsIFBvcnRhbCBpbnN0YW5jZSB0byB3aGljaCB0byBhdHRhY2ggdGhlIG92ZXJsYXkuXG4gICAqIEByZXR1cm5zIFRoZSBwb3J0YWwgYXR0YWNobWVudCByZXN1bHQuXG4gICAqL1xuICBhdHRhY2gocG9ydGFsOiBQb3J0YWw8YW55Pik6IGFueSB7XG4gICAgbGV0IGF0dGFjaFJlc3VsdCA9IHRoaXMuX3BvcnRhbE91dGxldC5hdHRhY2gocG9ydGFsKTtcblxuICAgIC8vIFVwZGF0ZSB0aGUgcGFuZSBlbGVtZW50IHdpdGggdGhlIGdpdmVuIGNvbmZpZ3VyYXRpb24uXG4gICAgaWYgKCF0aGlzLl9ob3N0LnBhcmVudEVsZW1lbnQgJiYgdGhpcy5fcHJldmlvdXNIb3N0UGFyZW50KSB7XG4gICAgICB0aGlzLl9wcmV2aW91c0hvc3RQYXJlbnQuYXBwZW5kQ2hpbGQodGhpcy5faG9zdCk7XG4gICAgfVxuXG4gICAgaWYgKHRoaXMuX3Bvc2l0aW9uU3RyYXRlZ3kpIHtcbiAgICAgIHRoaXMuX3Bvc2l0aW9uU3RyYXRlZ3kuYXR0YWNoKHRoaXMpO1xuICAgIH1cblxuICAgIHRoaXMuX3VwZGF0ZVN0YWNraW5nT3JkZXIoKTtcbiAgICB0aGlzLl91cGRhdGVFbGVtZW50U2l6ZSgpO1xuICAgIHRoaXMuX3VwZGF0ZUVsZW1lbnREaXJlY3Rpb24oKTtcblxuICAgIGlmICh0aGlzLl9zY3JvbGxTdHJhdGVneSkge1xuICAgICAgdGhpcy5fc2Nyb2xsU3RyYXRlZ3kuZW5hYmxlKCk7XG4gICAgfVxuXG4gICAgLy8gVXBkYXRlIHRoZSBwb3NpdGlvbiBvbmNlIHRoZSB6b25lIGlzIHN0YWJsZSBzbyB0aGF0IHRoZSBvdmVybGF5IHdpbGwgYmUgZnVsbHkgcmVuZGVyZWRcbiAgICAvLyBiZWZvcmUgYXR0ZW1wdGluZyB0byBwb3NpdGlvbiBpdCwgYXMgdGhlIHBvc2l0aW9uIG1heSBkZXBlbmQgb24gdGhlIHNpemUgb2YgdGhlIHJlbmRlcmVkXG4gICAgLy8gY29udGVudC5cbiAgICB0aGlzLl9uZ1pvbmUub25TdGFibGVcbiAgICAgIC5hc09ic2VydmFibGUoKVxuICAgICAgLnBpcGUodGFrZSgxKSlcbiAgICAgIC5zdWJzY3JpYmUoKCkgPT4ge1xuICAgICAgICAvLyBUaGUgb3ZlcmxheSBjb3VsZCd2ZSBiZWVuIGRldGFjaGVkIGJlZm9yZSB0aGUgem9uZSBoYXMgc3RhYmlsaXplZC5cbiAgICAgICAgaWYgKHRoaXMuaGFzQXR0YWNoZWQoKSkge1xuICAgICAgICAgIHRoaXMudXBkYXRlUG9zaXRpb24oKTtcbiAgICAgICAgfVxuICAgICAgfSk7XG5cbiAgICAvLyBFbmFibGUgcG9pbnRlciBldmVudHMgZm9yIHRoZSBvdmVybGF5IHBhbmUgZWxlbWVudC5cbiAgICB0aGlzLl90b2dnbGVQb2ludGVyRXZlbnRzKHRydWUpO1xuXG4gICAgaWYgKHRoaXMuX2NvbmZpZy5oYXNCYWNrZHJvcCkge1xuICAgICAgdGhpcy5fYXR0YWNoQmFja2Ryb3AoKTtcbiAgICB9XG5cbiAgICBpZiAodGhpcy5fY29uZmlnLnBhbmVsQ2xhc3MpIHtcbiAgICAgIHRoaXMuX3RvZ2dsZUNsYXNzZXModGhpcy5fcGFuZSwgdGhpcy5fY29uZmlnLnBhbmVsQ2xhc3MsIHRydWUpO1xuICAgIH1cblxuICAgIC8vIE9ubHkgZW1pdCB0aGUgYGF0dGFjaG1lbnRzYCBldmVudCBvbmNlIGFsbCBvdGhlciBzZXR1cCBpcyBkb25lLlxuICAgIHRoaXMuX2F0dGFjaG1lbnRzLm5leHQoKTtcblxuICAgIC8vIFRyYWNrIHRoaXMgb3ZlcmxheSBieSB0aGUga2V5Ym9hcmQgZGlzcGF0Y2hlclxuICAgIHRoaXMuX2tleWJvYXJkRGlzcGF0Y2hlci5hZGQodGhpcyk7XG5cbiAgICAvLyBAYnJlYWtpbmctY2hhbmdlIDguMC4wIHJlbW92ZSB0aGUgbnVsbCBjaGVjayBmb3IgYF9sb2NhdGlvbmBcbiAgICAvLyBvbmNlIHRoZSBjb25zdHJ1Y3RvciBwYXJhbWV0ZXIgaXMgbWFkZSByZXF1aXJlZC5cbiAgICBpZiAodGhpcy5fY29uZmlnLmRpc3Bvc2VPbk5hdmlnYXRpb24gJiYgdGhpcy5fbG9jYXRpb24pIHtcbiAgICAgIHRoaXMuX2xvY2F0aW9uQ2hhbmdlcyA9IHRoaXMuX2xvY2F0aW9uLnN1YnNjcmliZSgoKSA9PiB0aGlzLmRpc3Bvc2UoKSk7XG4gICAgfVxuXG4gICAgcmV0dXJuIGF0dGFjaFJlc3VsdDtcbiAgfVxuXG4gIC8qKlxuICAgKiBEZXRhY2hlcyBhbiBvdmVybGF5IGZyb20gYSBwb3J0YWwuXG4gICAqIEByZXR1cm5zIFRoZSBwb3J0YWwgZGV0YWNobWVudCByZXN1bHQuXG4gICAqL1xuICBkZXRhY2goKTogYW55IHtcbiAgICBpZiAoIXRoaXMuaGFzQXR0YWNoZWQoKSkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIHRoaXMuZGV0YWNoQmFja2Ryb3AoKTtcblxuICAgIC8vIFdoZW4gdGhlIG92ZXJsYXkgaXMgZGV0YWNoZWQsIHRoZSBwYW5lIGVsZW1lbnQgc2hvdWxkIGRpc2FibGUgcG9pbnRlciBldmVudHMuXG4gICAgLy8gVGhpcyBpcyBuZWNlc3NhcnkgYmVjYXVzZSBvdGhlcndpc2UgdGhlIHBhbmUgZWxlbWVudCB3aWxsIGNvdmVyIHRoZSBwYWdlIGFuZCBkaXNhYmxlXG4gICAgLy8gcG9pbnRlciBldmVudHMgdGhlcmVmb3JlLiBEZXBlbmRzIG9uIHRoZSBwb3NpdGlvbiBzdHJhdGVneSBhbmQgdGhlIGFwcGxpZWQgcGFuZSBib3VuZGFyaWVzLlxuICAgIHRoaXMuX3RvZ2dsZVBvaW50ZXJFdmVudHMoZmFsc2UpO1xuXG4gICAgaWYgKHRoaXMuX3Bvc2l0aW9uU3RyYXRlZ3kgJiYgdGhpcy5fcG9zaXRpb25TdHJhdGVneS5kZXRhY2gpIHtcbiAgICAgIHRoaXMuX3Bvc2l0aW9uU3RyYXRlZ3kuZGV0YWNoKCk7XG4gICAgfVxuXG4gICAgaWYgKHRoaXMuX3Njcm9sbFN0cmF0ZWd5KSB7XG4gICAgICB0aGlzLl9zY3JvbGxTdHJhdGVneS5kaXNhYmxlKCk7XG4gICAgfVxuXG4gICAgY29uc3QgZGV0YWNobWVudFJlc3VsdCA9IHRoaXMuX3BvcnRhbE91dGxldC5kZXRhY2goKTtcblxuICAgIC8vIE9ubHkgZW1pdCBhZnRlciBldmVyeXRoaW5nIGlzIGRldGFjaGVkLlxuICAgIHRoaXMuX2RldGFjaG1lbnRzLm5leHQoKTtcblxuICAgIC8vIFJlbW92ZSB0aGlzIG92ZXJsYXkgZnJvbSBrZXlib2FyZCBkaXNwYXRjaGVyIHRyYWNraW5nLlxuICAgIHRoaXMuX2tleWJvYXJkRGlzcGF0Y2hlci5yZW1vdmUodGhpcyk7XG5cbiAgICAvLyBLZWVwaW5nIHRoZSBob3N0IGVsZW1lbnQgaW4gdGhlIERPTSBjYW4gY2F1c2Ugc2Nyb2xsIGphbmssIGJlY2F1c2UgaXQgc3RpbGwgZ2V0c1xuICAgIC8vIHJlbmRlcmVkLCBldmVuIHRob3VnaCBpdCdzIHRyYW5zcGFyZW50IGFuZCB1bmNsaWNrYWJsZSB3aGljaCBpcyB3aHkgd2UgcmVtb3ZlIGl0LlxuICAgIHRoaXMuX2RldGFjaENvbnRlbnRXaGVuU3RhYmxlKCk7XG5cbiAgICAvLyBTdG9wIGxpc3RlbmluZyBmb3IgbG9jYXRpb24gY2hhbmdlcy5cbiAgICB0aGlzLl9sb2NhdGlvbkNoYW5nZXMudW5zdWJzY3JpYmUoKTtcblxuICAgIHJldHVybiBkZXRhY2htZW50UmVzdWx0O1xuICB9XG5cbiAgLyoqIENsZWFucyB1cCB0aGUgb3ZlcmxheSBmcm9tIHRoZSBET00uICovXG4gIGRpc3Bvc2UoKTogdm9pZCB7XG4gICAgY29uc3QgaXNBdHRhY2hlZCA9IHRoaXMuaGFzQXR0YWNoZWQoKTtcblxuICAgIGlmICh0aGlzLl9wb3NpdGlvblN0cmF0ZWd5KSB7XG4gICAgICB0aGlzLl9wb3NpdGlvblN0cmF0ZWd5LmRpc3Bvc2UoKTtcbiAgICB9XG5cbiAgICB0aGlzLl9kaXNwb3NlU2Nyb2xsU3RyYXRlZ3koKTtcbiAgICB0aGlzLmRldGFjaEJhY2tkcm9wKCk7XG4gICAgdGhpcy5fbG9jYXRpb25DaGFuZ2VzLnVuc3Vic2NyaWJlKCk7XG4gICAgdGhpcy5fa2V5Ym9hcmREaXNwYXRjaGVyLnJlbW92ZSh0aGlzKTtcbiAgICB0aGlzLl9wb3J0YWxPdXRsZXQuZGlzcG9zZSgpO1xuICAgIHRoaXMuX2F0dGFjaG1lbnRzLmNvbXBsZXRlKCk7XG4gICAgdGhpcy5fYmFja2Ryb3BDbGljay5jb21wbGV0ZSgpO1xuICAgIHRoaXMuX2tleWRvd25FdmVudHMuY29tcGxldGUoKTtcblxuICAgIGlmICh0aGlzLl9ob3N0ICYmIHRoaXMuX2hvc3QucGFyZW50Tm9kZSkge1xuICAgICAgdGhpcy5faG9zdC5wYXJlbnROb2RlLnJlbW92ZUNoaWxkKHRoaXMuX2hvc3QpO1xuICAgICAgdGhpcy5faG9zdCA9IG51bGwhO1xuICAgIH1cblxuICAgIHRoaXMuX3ByZXZpb3VzSG9zdFBhcmVudCA9IHRoaXMuX3BhbmUgPSBudWxsITtcblxuICAgIGlmIChpc0F0dGFjaGVkKSB7XG4gICAgICB0aGlzLl9kZXRhY2htZW50cy5uZXh0KCk7XG4gICAgfVxuXG4gICAgdGhpcy5fZGV0YWNobWVudHMuY29tcGxldGUoKTtcbiAgfVxuXG4gIC8qKiBXaGV0aGVyIHRoZSBvdmVybGF5IGhhcyBhdHRhY2hlZCBjb250ZW50LiAqL1xuICBoYXNBdHRhY2hlZCgpOiBib29sZWFuIHtcbiAgICByZXR1cm4gdGhpcy5fcG9ydGFsT3V0bGV0Lmhhc0F0dGFjaGVkKCk7XG4gIH1cblxuICAvKiogR2V0cyBhbiBvYnNlcnZhYmxlIHRoYXQgZW1pdHMgd2hlbiB0aGUgYmFja2Ryb3AgaGFzIGJlZW4gY2xpY2tlZC4gKi9cbiAgYmFja2Ryb3BDbGljaygpOiBPYnNlcnZhYmxlPE1vdXNlRXZlbnQ+IHtcbiAgICByZXR1cm4gdGhpcy5fYmFja2Ryb3BDbGljay5hc09ic2VydmFibGUoKTtcbiAgfVxuXG4gIC8qKiBHZXRzIGFuIG9ic2VydmFibGUgdGhhdCBlbWl0cyB3aGVuIHRoZSBvdmVybGF5IGhhcyBiZWVuIGF0dGFjaGVkLiAqL1xuICBhdHRhY2htZW50cygpOiBPYnNlcnZhYmxlPHZvaWQ+IHtcbiAgICByZXR1cm4gdGhpcy5fYXR0YWNobWVudHMuYXNPYnNlcnZhYmxlKCk7XG4gIH1cblxuICAvKiogR2V0cyBhbiBvYnNlcnZhYmxlIHRoYXQgZW1pdHMgd2hlbiB0aGUgb3ZlcmxheSBoYXMgYmVlbiBkZXRhY2hlZC4gKi9cbiAgZGV0YWNobWVudHMoKTogT2JzZXJ2YWJsZTx2b2lkPiB7XG4gICAgcmV0dXJuIHRoaXMuX2RldGFjaG1lbnRzLmFzT2JzZXJ2YWJsZSgpO1xuICB9XG5cbiAgLyoqIEdldHMgYW4gb2JzZXJ2YWJsZSBvZiBrZXlkb3duIGV2ZW50cyB0YXJnZXRlZCB0byB0aGlzIG92ZXJsYXkuICovXG4gIGtleWRvd25FdmVudHMoKTogT2JzZXJ2YWJsZTxLZXlib2FyZEV2ZW50PiB7XG4gICAgcmV0dXJuIHRoaXMuX2tleWRvd25FdmVudHMuYXNPYnNlcnZhYmxlKCk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgY3VycmVudCBvdmVybGF5IGNvbmZpZ3VyYXRpb24sIHdoaWNoIGlzIGltbXV0YWJsZS4gKi9cbiAgZ2V0Q29uZmlnKCk6IE92ZXJsYXlDb25maWcge1xuICAgIHJldHVybiB0aGlzLl9jb25maWc7XG4gIH1cblxuICAvKiogVXBkYXRlcyB0aGUgcG9zaXRpb24gb2YgdGhlIG92ZXJsYXkgYmFzZWQgb24gdGhlIHBvc2l0aW9uIHN0cmF0ZWd5LiAqL1xuICB1cGRhdGVQb3NpdGlvbigpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5fcG9zaXRpb25TdHJhdGVneSkge1xuICAgICAgdGhpcy5fcG9zaXRpb25TdHJhdGVneS5hcHBseSgpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBTd2l0Y2hlcyB0byBhIG5ldyBwb3NpdGlvbiBzdHJhdGVneSBhbmQgdXBkYXRlcyB0aGUgb3ZlcmxheSBwb3NpdGlvbi4gKi9cbiAgdXBkYXRlUG9zaXRpb25TdHJhdGVneShzdHJhdGVneTogUG9zaXRpb25TdHJhdGVneSk6IHZvaWQge1xuICAgIGlmIChzdHJhdGVneSA9PT0gdGhpcy5fcG9zaXRpb25TdHJhdGVneSkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGlmICh0aGlzLl9wb3NpdGlvblN0cmF0ZWd5KSB7XG4gICAgICB0aGlzLl9wb3NpdGlvblN0cmF0ZWd5LmRpc3Bvc2UoKTtcbiAgICB9XG5cbiAgICB0aGlzLl9wb3NpdGlvblN0cmF0ZWd5ID0gc3RyYXRlZ3k7XG5cbiAgICBpZiAodGhpcy5oYXNBdHRhY2hlZCgpKSB7XG4gICAgICBzdHJhdGVneS5hdHRhY2godGhpcyk7XG4gICAgICB0aGlzLnVwZGF0ZVBvc2l0aW9uKCk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFVwZGF0ZSB0aGUgc2l6ZSBwcm9wZXJ0aWVzIG9mIHRoZSBvdmVybGF5LiAqL1xuICB1cGRhdGVTaXplKHNpemVDb25maWc6IE92ZXJsYXlTaXplQ29uZmlnKTogdm9pZCB7XG4gICAgdGhpcy5fY29uZmlnID0gey4uLnRoaXMuX2NvbmZpZywgLi4uc2l6ZUNvbmZpZ307XG4gICAgdGhpcy5fdXBkYXRlRWxlbWVudFNpemUoKTtcbiAgfVxuXG4gIC8qKiBTZXRzIHRoZSBMVFIvUlRMIGRpcmVjdGlvbiBmb3IgdGhlIG92ZXJsYXkuICovXG4gIHNldERpcmVjdGlvbihkaXI6IERpcmVjdGlvbiB8IERpcmVjdGlvbmFsaXR5KTogdm9pZCB7XG4gICAgdGhpcy5fY29uZmlnID0gey4uLnRoaXMuX2NvbmZpZywgZGlyZWN0aW9uOiBkaXJ9O1xuICAgIHRoaXMuX3VwZGF0ZUVsZW1lbnREaXJlY3Rpb24oKTtcbiAgfVxuXG4gIC8qKiBBZGQgYSBDU1MgY2xhc3Mgb3IgYW4gYXJyYXkgb2YgY2xhc3NlcyB0byB0aGUgb3ZlcmxheSBwYW5lLiAqL1xuICBhZGRQYW5lbENsYXNzKGNsYXNzZXM6IHN0cmluZyB8IHN0cmluZ1tdKTogdm9pZCB7XG4gICAgaWYgKHRoaXMuX3BhbmUpIHtcbiAgICAgIHRoaXMuX3RvZ2dsZUNsYXNzZXModGhpcy5fcGFuZSwgY2xhc3NlcywgdHJ1ZSk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFJlbW92ZSBhIENTUyBjbGFzcyBvciBhbiBhcnJheSBvZiBjbGFzc2VzIGZyb20gdGhlIG92ZXJsYXkgcGFuZS4gKi9cbiAgcmVtb3ZlUGFuZWxDbGFzcyhjbGFzc2VzOiBzdHJpbmcgfCBzdHJpbmdbXSk6IHZvaWQge1xuICAgIGlmICh0aGlzLl9wYW5lKSB7XG4gICAgICB0aGlzLl90b2dnbGVDbGFzc2VzKHRoaXMuX3BhbmUsIGNsYXNzZXMsIGZhbHNlKTtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogUmV0dXJucyB0aGUgbGF5b3V0IGRpcmVjdGlvbiBvZiB0aGUgb3ZlcmxheSBwYW5lbC5cbiAgICovXG4gIGdldERpcmVjdGlvbigpOiBEaXJlY3Rpb24ge1xuICAgIGNvbnN0IGRpcmVjdGlvbiA9IHRoaXMuX2NvbmZpZy5kaXJlY3Rpb247XG5cbiAgICBpZiAoIWRpcmVjdGlvbikge1xuICAgICAgcmV0dXJuICdsdHInO1xuICAgIH1cblxuICAgIHJldHVybiB0eXBlb2YgZGlyZWN0aW9uID09PSAnc3RyaW5nJyA/IGRpcmVjdGlvbiA6IGRpcmVjdGlvbi52YWx1ZTtcbiAgfVxuXG4gIC8qKiBTd2l0Y2hlcyB0byBhIG5ldyBzY3JvbGwgc3RyYXRlZ3kuICovXG4gIHVwZGF0ZVNjcm9sbFN0cmF0ZWd5KHN0cmF0ZWd5OiBTY3JvbGxTdHJhdGVneSk6IHZvaWQge1xuICAgIGlmIChzdHJhdGVneSA9PT0gdGhpcy5fc2Nyb2xsU3RyYXRlZ3kpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICB0aGlzLl9kaXNwb3NlU2Nyb2xsU3RyYXRlZ3koKTtcbiAgICB0aGlzLl9zY3JvbGxTdHJhdGVneSA9IHN0cmF0ZWd5O1xuXG4gICAgaWYgKHRoaXMuaGFzQXR0YWNoZWQoKSkge1xuICAgICAgc3RyYXRlZ3kuYXR0YWNoKHRoaXMpO1xuICAgICAgc3RyYXRlZ3kuZW5hYmxlKCk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFVwZGF0ZXMgdGhlIHRleHQgZGlyZWN0aW9uIG9mIHRoZSBvdmVybGF5IHBhbmVsLiAqL1xuICBwcml2YXRlIF91cGRhdGVFbGVtZW50RGlyZWN0aW9uKCkge1xuICAgIHRoaXMuX2hvc3Quc2V0QXR0cmlidXRlKCdkaXInLCB0aGlzLmdldERpcmVjdGlvbigpKTtcbiAgfVxuXG4gIC8qKiBVcGRhdGVzIHRoZSBzaXplIG9mIHRoZSBvdmVybGF5IGVsZW1lbnQgYmFzZWQgb24gdGhlIG92ZXJsYXkgY29uZmlnLiAqL1xuICBwcml2YXRlIF91cGRhdGVFbGVtZW50U2l6ZSgpIHtcbiAgICBpZiAoIXRoaXMuX3BhbmUpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICBjb25zdCBzdHlsZSA9IHRoaXMuX3BhbmUuc3R5bGU7XG5cbiAgICBzdHlsZS53aWR0aCA9IGNvZXJjZUNzc1BpeGVsVmFsdWUodGhpcy5fY29uZmlnLndpZHRoKTtcbiAgICBzdHlsZS5oZWlnaHQgPSBjb2VyY2VDc3NQaXhlbFZhbHVlKHRoaXMuX2NvbmZpZy5oZWlnaHQpO1xuICAgIHN0eWxlLm1pbldpZHRoID0gY29lcmNlQ3NzUGl4ZWxWYWx1ZSh0aGlzLl9jb25maWcubWluV2lkdGgpO1xuICAgIHN0eWxlLm1pbkhlaWdodCA9IGNvZXJjZUNzc1BpeGVsVmFsdWUodGhpcy5fY29uZmlnLm1pbkhlaWdodCk7XG4gICAgc3R5bGUubWF4V2lkdGggPSBjb2VyY2VDc3NQaXhlbFZhbHVlKHRoaXMuX2NvbmZpZy5tYXhXaWR0aCk7XG4gICAgc3R5bGUubWF4SGVpZ2h0ID0gY29lcmNlQ3NzUGl4ZWxWYWx1ZSh0aGlzLl9jb25maWcubWF4SGVpZ2h0KTtcbiAgfVxuXG4gIC8qKiBUb2dnbGVzIHRoZSBwb2ludGVyIGV2ZW50cyBmb3IgdGhlIG92ZXJsYXkgcGFuZSBlbGVtZW50LiAqL1xuICBwcml2YXRlIF90b2dnbGVQb2ludGVyRXZlbnRzKGVuYWJsZVBvaW50ZXI6IGJvb2xlYW4pIHtcbiAgICB0aGlzLl9wYW5lLnN0eWxlLnBvaW50ZXJFdmVudHMgPSBlbmFibGVQb2ludGVyID8gJ2F1dG8nIDogJ25vbmUnO1xuICB9XG5cbiAgLyoqIEF0dGFjaGVzIGEgYmFja2Ryb3AgZm9yIHRoaXMgb3ZlcmxheS4gKi9cbiAgcHJpdmF0ZSBfYXR0YWNoQmFja2Ryb3AoKSB7XG4gICAgY29uc3Qgc2hvd2luZ0NsYXNzID0gJ2Nkay1vdmVybGF5LWJhY2tkcm9wLXNob3dpbmcnO1xuXG4gICAgdGhpcy5fYmFja2Ryb3BFbGVtZW50ID0gdGhpcy5fZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgnZGl2Jyk7XG4gICAgdGhpcy5fYmFja2Ryb3BFbGVtZW50LmNsYXNzTGlzdC5hZGQoJ2Nkay1vdmVybGF5LWJhY2tkcm9wJyk7XG5cbiAgICBpZiAodGhpcy5fY29uZmlnLmJhY2tkcm9wQ2xhc3MpIHtcbiAgICAgIHRoaXMuX3RvZ2dsZUNsYXNzZXModGhpcy5fYmFja2Ryb3BFbGVtZW50LCB0aGlzLl9jb25maWcuYmFja2Ryb3BDbGFzcywgdHJ1ZSk7XG4gICAgfVxuXG4gICAgLy8gSW5zZXJ0IHRoZSBiYWNrZHJvcCBiZWZvcmUgdGhlIHBhbmUgaW4gdGhlIERPTSBvcmRlcixcbiAgICAvLyBpbiBvcmRlciB0byBoYW5kbGUgc3RhY2tlZCBvdmVybGF5cyBwcm9wZXJseS5cbiAgICB0aGlzLl9ob3N0LnBhcmVudEVsZW1lbnQhLmluc2VydEJlZm9yZSh0aGlzLl9iYWNrZHJvcEVsZW1lbnQsIHRoaXMuX2hvc3QpO1xuXG4gICAgLy8gRm9yd2FyZCBiYWNrZHJvcCBjbGlja3Mgc3VjaCB0aGF0IHRoZSBjb25zdW1lciBvZiB0aGUgb3ZlcmxheSBjYW4gcGVyZm9ybSB3aGF0ZXZlclxuICAgIC8vIGFjdGlvbiBkZXNpcmVkIHdoZW4gc3VjaCBhIGNsaWNrIG9jY3VycyAodXN1YWxseSBjbG9zaW5nIHRoZSBvdmVybGF5KS5cbiAgICB0aGlzLl9iYWNrZHJvcEVsZW1lbnQuYWRkRXZlbnRMaXN0ZW5lcignY2xpY2snLCB0aGlzLl9iYWNrZHJvcENsaWNrSGFuZGxlcik7XG5cbiAgICAvLyBBZGQgY2xhc3MgdG8gZmFkZS1pbiB0aGUgYmFja2Ryb3AgYWZ0ZXIgb25lIGZyYW1lLlxuICAgIGlmICh0eXBlb2YgcmVxdWVzdEFuaW1hdGlvbkZyYW1lICE9PSAndW5kZWZpbmVkJykge1xuICAgICAgdGhpcy5fbmdab25lLnJ1bk91dHNpZGVBbmd1bGFyKCgpID0+IHtcbiAgICAgICAgcmVxdWVzdEFuaW1hdGlvbkZyYW1lKCgpID0+IHtcbiAgICAgICAgICBpZiAodGhpcy5fYmFja2Ryb3BFbGVtZW50KSB7XG4gICAgICAgICAgICB0aGlzLl9iYWNrZHJvcEVsZW1lbnQuY2xhc3NMaXN0LmFkZChzaG93aW5nQ2xhc3MpO1xuICAgICAgICAgIH1cbiAgICAgICAgfSk7XG4gICAgICB9KTtcbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5fYmFja2Ryb3BFbGVtZW50LmNsYXNzTGlzdC5hZGQoc2hvd2luZ0NsYXNzKTtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogVXBkYXRlcyB0aGUgc3RhY2tpbmcgb3JkZXIgb2YgdGhlIGVsZW1lbnQsIG1vdmluZyBpdCB0byB0aGUgdG9wIGlmIG5lY2Vzc2FyeS5cbiAgICogVGhpcyBpcyByZXF1aXJlZCBpbiBjYXNlcyB3aGVyZSBvbmUgb3ZlcmxheSB3YXMgZGV0YWNoZWQsIHdoaWxlIGFub3RoZXIgb25lLFxuICAgKiB0aGF0IHNob3VsZCBiZSBiZWhpbmQgaXQsIHdhcyBkZXN0cm95ZWQuIFRoZSBuZXh0IHRpbWUgYm90aCBvZiB0aGVtIGFyZSBvcGVuZWQsXG4gICAqIHRoZSBzdGFja2luZyB3aWxsIGJlIHdyb25nLCBiZWNhdXNlIHRoZSBkZXRhY2hlZCBlbGVtZW50J3MgcGFuZSB3aWxsIHN0aWxsIGJlXG4gICAqIGluIGl0cyBvcmlnaW5hbCBET00gcG9zaXRpb24uXG4gICAqL1xuICBwcml2YXRlIF91cGRhdGVTdGFja2luZ09yZGVyKCkge1xuICAgIGlmICh0aGlzLl9ob3N0Lm5leHRTaWJsaW5nKSB7XG4gICAgICB0aGlzLl9ob3N0LnBhcmVudE5vZGUhLmFwcGVuZENoaWxkKHRoaXMuX2hvc3QpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBEZXRhY2hlcyB0aGUgYmFja2Ryb3AgKGlmIGFueSkgYXNzb2NpYXRlZCB3aXRoIHRoZSBvdmVybGF5LiAqL1xuICBkZXRhY2hCYWNrZHJvcCgpOiB2b2lkIHtcbiAgICBsZXQgYmFja2Ryb3BUb0RldGFjaCA9IHRoaXMuX2JhY2tkcm9wRWxlbWVudDtcblxuICAgIGlmICghYmFja2Ryb3BUb0RldGFjaCkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGxldCB0aW1lb3V0SWQ6IG51bWJlcjtcbiAgICBsZXQgZmluaXNoRGV0YWNoID0gKCkgPT4ge1xuICAgICAgLy8gSXQgbWF5IG5vdCBiZSBhdHRhY2hlZCB0byBhbnl0aGluZyBpbiBjZXJ0YWluIGNhc2VzIChlLmcuIHVuaXQgdGVzdHMpLlxuICAgICAgaWYgKGJhY2tkcm9wVG9EZXRhY2gpIHtcbiAgICAgICAgYmFja2Ryb3BUb0RldGFjaC5yZW1vdmVFdmVudExpc3RlbmVyKCdjbGljaycsIHRoaXMuX2JhY2tkcm9wQ2xpY2tIYW5kbGVyKTtcbiAgICAgICAgYmFja2Ryb3BUb0RldGFjaC5yZW1vdmVFdmVudExpc3RlbmVyKCd0cmFuc2l0aW9uZW5kJywgZmluaXNoRGV0YWNoKTtcblxuICAgICAgICBpZiAoYmFja2Ryb3BUb0RldGFjaC5wYXJlbnROb2RlKSB7XG4gICAgICAgICAgYmFja2Ryb3BUb0RldGFjaC5wYXJlbnROb2RlLnJlbW92ZUNoaWxkKGJhY2tkcm9wVG9EZXRhY2gpO1xuICAgICAgICB9XG4gICAgICB9XG5cbiAgICAgIC8vIEl0IGlzIHBvc3NpYmxlIHRoYXQgYSBuZXcgcG9ydGFsIGhhcyBiZWVuIGF0dGFjaGVkIHRvIHRoaXMgb3ZlcmxheSBzaW5jZSB3ZSBzdGFydGVkXG4gICAgICAvLyByZW1vdmluZyB0aGUgYmFja2Ryb3AuIElmIHRoYXQgaXMgdGhlIGNhc2UsIG9ubHkgY2xlYXIgdGhlIGJhY2tkcm9wIHJlZmVyZW5jZSBpZiBpdFxuICAgICAgLy8gaXMgc3RpbGwgdGhlIHNhbWUgaW5zdGFuY2UgdGhhdCB3ZSBzdGFydGVkIHRvIHJlbW92ZS5cbiAgICAgIGlmICh0aGlzLl9iYWNrZHJvcEVsZW1lbnQgPT0gYmFja2Ryb3BUb0RldGFjaCkge1xuICAgICAgICB0aGlzLl9iYWNrZHJvcEVsZW1lbnQgPSBudWxsO1xuICAgICAgfVxuXG4gICAgICBpZiAodGhpcy5fY29uZmlnLmJhY2tkcm9wQ2xhc3MpIHtcbiAgICAgICAgdGhpcy5fdG9nZ2xlQ2xhc3NlcyhiYWNrZHJvcFRvRGV0YWNoISwgdGhpcy5fY29uZmlnLmJhY2tkcm9wQ2xhc3MsIGZhbHNlKTtcbiAgICAgIH1cblxuICAgICAgY2xlYXJUaW1lb3V0KHRpbWVvdXRJZCk7XG4gICAgfTtcblxuICAgIGJhY2tkcm9wVG9EZXRhY2guY2xhc3NMaXN0LnJlbW92ZSgnY2RrLW92ZXJsYXktYmFja2Ryb3Atc2hvd2luZycpO1xuXG4gICAgdGhpcy5fbmdab25lLnJ1bk91dHNpZGVBbmd1bGFyKCgpID0+IHtcbiAgICAgIGJhY2tkcm9wVG9EZXRhY2ghLmFkZEV2ZW50TGlzdGVuZXIoJ3RyYW5zaXRpb25lbmQnLCBmaW5pc2hEZXRhY2gpO1xuICAgIH0pO1xuXG4gICAgLy8gSWYgdGhlIGJhY2tkcm9wIGRvZXNuJ3QgaGF2ZSBhIHRyYW5zaXRpb24sIHRoZSBgdHJhbnNpdGlvbmVuZGAgZXZlbnQgd29uJ3QgZmlyZS5cbiAgICAvLyBJbiB0aGlzIGNhc2Ugd2UgbWFrZSBpdCB1bmNsaWNrYWJsZSBhbmQgd2UgdHJ5IHRvIHJlbW92ZSBpdCBhZnRlciBhIGRlbGF5LlxuICAgIGJhY2tkcm9wVG9EZXRhY2guc3R5bGUucG9pbnRlckV2ZW50cyA9ICdub25lJztcblxuICAgIC8vIFJ1biB0aGlzIG91dHNpZGUgdGhlIEFuZ3VsYXIgem9uZSBiZWNhdXNlIHRoZXJlJ3Mgbm90aGluZyB0aGF0IEFuZ3VsYXIgY2FyZXMgYWJvdXQuXG4gICAgLy8gSWYgaXQgd2VyZSB0byBydW4gaW5zaWRlIHRoZSBBbmd1bGFyIHpvbmUsIGV2ZXJ5IHRlc3QgdGhhdCB1c2VkIE92ZXJsYXkgd291bGQgaGF2ZSB0byBiZVxuICAgIC8vIGVpdGhlciBhc3luYyBvciBmYWtlQXN5bmMuXG4gICAgdGltZW91dElkID0gdGhpcy5fbmdab25lLnJ1bk91dHNpZGVBbmd1bGFyKCgpID0+IHNldFRpbWVvdXQoZmluaXNoRGV0YWNoLCA1MDApKTtcbiAgfVxuXG4gIC8qKiBUb2dnbGVzIGEgc2luZ2xlIENTUyBjbGFzcyBvciBhbiBhcnJheSBvZiBjbGFzc2VzIG9uIGFuIGVsZW1lbnQuICovXG4gIHByaXZhdGUgX3RvZ2dsZUNsYXNzZXMoZWxlbWVudDogSFRNTEVsZW1lbnQsIGNzc0NsYXNzZXM6IHN0cmluZyB8IHN0cmluZ1tdLCBpc0FkZDogYm9vbGVhbikge1xuICAgIGNvbnN0IGNsYXNzTGlzdCA9IGVsZW1lbnQuY2xhc3NMaXN0O1xuXG4gICAgY29lcmNlQXJyYXkoY3NzQ2xhc3NlcykuZm9yRWFjaChjc3NDbGFzcyA9PiB7XG4gICAgICAvLyBXZSBjYW4ndCBkbyBhIHNwcmVhZCBoZXJlLCBiZWNhdXNlIElFIGRvZXNuJ3Qgc3VwcG9ydCBzZXR0aW5nIG11bHRpcGxlIGNsYXNzZXMuXG4gICAgICAvLyBBbHNvIHRyeWluZyB0byBhZGQgYW4gZW1wdHkgc3RyaW5nIHRvIGEgRE9NVG9rZW5MaXN0IHdpbGwgdGhyb3cuXG4gICAgICBpZiAoY3NzQ2xhc3MpIHtcbiAgICAgICAgaXNBZGQgPyBjbGFzc0xpc3QuYWRkKGNzc0NsYXNzKSA6IGNsYXNzTGlzdC5yZW1vdmUoY3NzQ2xhc3MpO1xuICAgICAgfVxuICAgIH0pO1xuICB9XG5cbiAgLyoqIERldGFjaGVzIHRoZSBvdmVybGF5IGNvbnRlbnQgbmV4dCB0aW1lIHRoZSB6b25lIHN0YWJpbGl6ZXMuICovXG4gIHByaXZhdGUgX2RldGFjaENvbnRlbnRXaGVuU3RhYmxlKCkge1xuICAgIC8vIE5vcm1hbGx5IHdlIHdvdWxkbid0IGhhdmUgdG8gZXhwbGljaXRseSBydW4gdGhpcyBvdXRzaWRlIHRoZSBgTmdab25lYCwgaG93ZXZlclxuICAgIC8vIGlmIHRoZSBjb25zdW1lciBpcyB1c2luZyBgem9uZS1wYXRjaC1yeGpzYCwgdGhlIGBTdWJzY3JpcHRpb24udW5zdWJzY3JpYmVgIGNhbGwgd2lsbFxuICAgIC8vIGJlIHBhdGNoZWQgdG8gcnVuIGluc2lkZSB0aGUgem9uZSwgd2hpY2ggd2lsbCB0aHJvdyB1cyBpbnRvIGFuIGluZmluaXRlIGxvb3AuXG4gICAgdGhpcy5fbmdab25lLnJ1bk91dHNpZGVBbmd1bGFyKCgpID0+IHtcbiAgICAgIC8vIFdlIGNhbid0IHJlbW92ZSB0aGUgaG9zdCBoZXJlIGltbWVkaWF0ZWx5LCBiZWNhdXNlIHRoZSBvdmVybGF5IHBhbmUncyBjb250ZW50XG4gICAgICAvLyBtaWdodCBzdGlsbCBiZSBhbmltYXRpbmcuIFRoaXMgc3RyZWFtIGhlbHBzIHVzIGF2b2lkIGludGVycnVwdGluZyB0aGUgYW5pbWF0aW9uXG4gICAgICAvLyBieSB3YWl0aW5nIGZvciB0aGUgcGFuZSB0byBiZWNvbWUgZW1wdHkuXG4gICAgICBjb25zdCBzdWJzY3JpcHRpb24gPSB0aGlzLl9uZ1pvbmUub25TdGFibGVcbiAgICAgICAgLmFzT2JzZXJ2YWJsZSgpXG4gICAgICAgIC5waXBlKHRha2VVbnRpbChtZXJnZSh0aGlzLl9hdHRhY2htZW50cywgdGhpcy5fZGV0YWNobWVudHMpKSlcbiAgICAgICAgLnN1YnNjcmliZSgoKSA9PiB7XG4gICAgICAgICAgLy8gTmVlZHMgYSBjb3VwbGUgb2YgY2hlY2tzIGZvciB0aGUgcGFuZSBhbmQgaG9zdCwgYmVjYXVzZVxuICAgICAgICAgIC8vIHRoZXkgbWF5IGhhdmUgYmVlbiByZW1vdmVkIGJ5IHRoZSB0aW1lIHRoZSB6b25lIHN0YWJpbGl6ZXMuXG4gICAgICAgICAgaWYgKCF0aGlzLl9wYW5lIHx8ICF0aGlzLl9ob3N0IHx8IHRoaXMuX3BhbmUuY2hpbGRyZW4ubGVuZ3RoID09PSAwKSB7XG4gICAgICAgICAgICBpZiAodGhpcy5fcGFuZSAmJiB0aGlzLl9jb25maWcucGFuZWxDbGFzcykge1xuICAgICAgICAgICAgICB0aGlzLl90b2dnbGVDbGFzc2VzKHRoaXMuX3BhbmUsIHRoaXMuX2NvbmZpZy5wYW5lbENsYXNzLCBmYWxzZSk7XG4gICAgICAgICAgICB9XG5cbiAgICAgICAgICAgIGlmICh0aGlzLl9ob3N0ICYmIHRoaXMuX2hvc3QucGFyZW50RWxlbWVudCkge1xuICAgICAgICAgICAgICB0aGlzLl9wcmV2aW91c0hvc3RQYXJlbnQgPSB0aGlzLl9ob3N0LnBhcmVudEVsZW1lbnQ7XG4gICAgICAgICAgICAgIHRoaXMuX3ByZXZpb3VzSG9zdFBhcmVudC5yZW1vdmVDaGlsZCh0aGlzLl9ob3N0KTtcbiAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgc3Vic2NyaXB0aW9uLnVuc3Vic2NyaWJlKCk7XG4gICAgICAgICAgfVxuICAgICAgICB9KTtcbiAgICB9KTtcbiAgfVxuXG4gIC8qKiBEaXNwb3NlcyBvZiBhIHNjcm9sbCBzdHJhdGVneS4gKi9cbiAgcHJpdmF0ZSBfZGlzcG9zZVNjcm9sbFN0cmF0ZWd5KCkge1xuICAgIGNvbnN0IHNjcm9sbFN0cmF0ZWd5ID0gdGhpcy5fc2Nyb2xsU3RyYXRlZ3k7XG5cbiAgICBpZiAoc2Nyb2xsU3RyYXRlZ3kpIHtcbiAgICAgIHNjcm9sbFN0cmF0ZWd5LmRpc2FibGUoKTtcblxuICAgICAgaWYgKHNjcm9sbFN0cmF0ZWd5LmRldGFjaCkge1xuICAgICAgICBzY3JvbGxTdHJhdGVneS5kZXRhY2goKTtcbiAgICAgIH1cbiAgICB9XG4gIH1cbn1cblxuXG4vKiogU2l6ZSBwcm9wZXJ0aWVzIGZvciBhbiBvdmVybGF5LiAqL1xuZXhwb3J0IGludGVyZmFjZSBPdmVybGF5U2l6ZUNvbmZpZyB7XG4gIHdpZHRoPzogbnVtYmVyIHwgc3RyaW5nO1xuICBoZWlnaHQ/OiBudW1iZXIgfCBzdHJpbmc7XG4gIG1pbldpZHRoPzogbnVtYmVyIHwgc3RyaW5nO1xuICBtaW5IZWlnaHQ/OiBudW1iZXIgfCBzdHJpbmc7XG4gIG1heFdpZHRoPzogbnVtYmVyIHwgc3RyaW5nO1xuICBtYXhIZWlnaHQ/OiBudW1iZXIgfCBzdHJpbmc7XG59XG4iXX0=