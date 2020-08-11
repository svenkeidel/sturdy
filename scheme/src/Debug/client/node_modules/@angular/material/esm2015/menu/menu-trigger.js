/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { FocusMonitor, isFakeMousedownFromScreenReader } from '@angular/cdk/a11y';
import { Directionality } from '@angular/cdk/bidi';
import { LEFT_ARROW, RIGHT_ARROW } from '@angular/cdk/keycodes';
import { Overlay, OverlayConfig, } from '@angular/cdk/overlay';
import { TemplatePortal } from '@angular/cdk/portal';
import { Directive, ElementRef, EventEmitter, Inject, InjectionToken, Input, Optional, Output, Self, ViewContainerRef, } from '@angular/core';
import { normalizePassiveListenerOptions } from '@angular/cdk/platform';
import { asapScheduler, merge, of as observableOf, Subscription } from 'rxjs';
import { delay, filter, take, takeUntil } from 'rxjs/operators';
import { MatMenu } from './menu';
import { throwMatMenuMissingError } from './menu-errors';
import { MatMenuItem } from './menu-item';
/** Injection token that determines the scroll handling while the menu is open. */
export const MAT_MENU_SCROLL_STRATEGY = new InjectionToken('mat-menu-scroll-strategy');
/** @docs-private */
export function MAT_MENU_SCROLL_STRATEGY_FACTORY(overlay) {
    return () => overlay.scrollStrategies.reposition();
}
/** @docs-private */
export const MAT_MENU_SCROLL_STRATEGY_FACTORY_PROVIDER = {
    provide: MAT_MENU_SCROLL_STRATEGY,
    deps: [Overlay],
    useFactory: MAT_MENU_SCROLL_STRATEGY_FACTORY,
};
/** Default top padding of the menu panel. */
export const MENU_PANEL_TOP_PADDING = 8;
/** Options for binding a passive event listener. */
const passiveEventListenerOptions = normalizePassiveListenerOptions({ passive: true });
// TODO(andrewseguin): Remove the kebab versions in favor of camelCased attribute selectors
/** Directive applied to an element that should trigger a `mat-menu`. */
let MatMenuTrigger = /** @class */ (() => {
    class MatMenuTrigger {
        constructor(_overlay, _element, _viewContainerRef, scrollStrategy, _parentMenu, _menuItemInstance, _dir, 
        // TODO(crisbeto): make the _focusMonitor required when doing breaking changes.
        // @breaking-change 8.0.0
        _focusMonitor) {
            this._overlay = _overlay;
            this._element = _element;
            this._viewContainerRef = _viewContainerRef;
            this._parentMenu = _parentMenu;
            this._menuItemInstance = _menuItemInstance;
            this._dir = _dir;
            this._focusMonitor = _focusMonitor;
            this._overlayRef = null;
            this._menuOpen = false;
            this._closingActionsSubscription = Subscription.EMPTY;
            this._hoverSubscription = Subscription.EMPTY;
            this._menuCloseSubscription = Subscription.EMPTY;
            /**
             * Handles touch start events on the trigger.
             * Needs to be an arrow function so we can easily use addEventListener and removeEventListener.
             */
            this._handleTouchStart = () => this._openedBy = 'touch';
            // Tracking input type is necessary so it's possible to only auto-focus
            // the first item of the list when the menu is opened via the keyboard
            this._openedBy = null;
            /**
             * Whether focus should be restored when the menu is closed.
             * Note that disabling this option can have accessibility implications
             * and it's up to you to manage focus, if you decide to turn it off.
             */
            this.restoreFocus = true;
            /** Event emitted when the associated menu is opened. */
            this.menuOpened = new EventEmitter();
            /**
             * Event emitted when the associated menu is opened.
             * @deprecated Switch to `menuOpened` instead
             * @breaking-change 8.0.0
             */
            // tslint:disable-next-line:no-output-on-prefix
            this.onMenuOpen = this.menuOpened;
            /** Event emitted when the associated menu is closed. */
            this.menuClosed = new EventEmitter();
            /**
             * Event emitted when the associated menu is closed.
             * @deprecated Switch to `menuClosed` instead
             * @breaking-change 8.0.0
             */
            // tslint:disable-next-line:no-output-on-prefix
            this.onMenuClose = this.menuClosed;
            _element.nativeElement.addEventListener('touchstart', this._handleTouchStart, passiveEventListenerOptions);
            if (_menuItemInstance) {
                _menuItemInstance._triggersSubmenu = this.triggersSubmenu();
            }
            this._scrollStrategy = scrollStrategy;
        }
        /**
         * @deprecated
         * @breaking-change 8.0.0
         */
        get _deprecatedMatMenuTriggerFor() { return this.menu; }
        set _deprecatedMatMenuTriggerFor(v) {
            this.menu = v;
        }
        /** References the menu instance that the trigger is associated with. */
        get menu() { return this._menu; }
        set menu(menu) {
            if (menu === this._menu) {
                return;
            }
            this._menu = menu;
            this._menuCloseSubscription.unsubscribe();
            if (menu) {
                this._menuCloseSubscription = menu.close.asObservable().subscribe(reason => {
                    this._destroyMenu();
                    // If a click closed the menu, we should close the entire chain of nested menus.
                    if ((reason === 'click' || reason === 'tab') && this._parentMenu) {
                        this._parentMenu.closed.emit(reason);
                    }
                });
            }
        }
        ngAfterContentInit() {
            this._checkMenu();
            this._handleHover();
        }
        ngOnDestroy() {
            if (this._overlayRef) {
                this._overlayRef.dispose();
                this._overlayRef = null;
            }
            this._element.nativeElement.removeEventListener('touchstart', this._handleTouchStart, passiveEventListenerOptions);
            this._menuCloseSubscription.unsubscribe();
            this._closingActionsSubscription.unsubscribe();
            this._hoverSubscription.unsubscribe();
        }
        /** Whether the menu is open. */
        get menuOpen() {
            return this._menuOpen;
        }
        /** The text direction of the containing app. */
        get dir() {
            return this._dir && this._dir.value === 'rtl' ? 'rtl' : 'ltr';
        }
        /** Whether the menu triggers a sub-menu or a top-level one. */
        triggersSubmenu() {
            return !!(this._menuItemInstance && this._parentMenu);
        }
        /** Toggles the menu between the open and closed states. */
        toggleMenu() {
            return this._menuOpen ? this.closeMenu() : this.openMenu();
        }
        /** Opens the menu. */
        openMenu() {
            if (this._menuOpen) {
                return;
            }
            this._checkMenu();
            const overlayRef = this._createOverlay();
            const overlayConfig = overlayRef.getConfig();
            this._setPosition(overlayConfig.positionStrategy);
            overlayConfig.hasBackdrop = this.menu.hasBackdrop == null ? !this.triggersSubmenu() :
                this.menu.hasBackdrop;
            overlayRef.attach(this._getPortal());
            if (this.menu.lazyContent) {
                this.menu.lazyContent.attach(this.menuData);
            }
            this._closingActionsSubscription = this._menuClosingActions().subscribe(() => this.closeMenu());
            this._initMenu();
            if (this.menu instanceof MatMenu) {
                this.menu._startAnimation();
            }
        }
        /** Closes the menu. */
        closeMenu() {
            this.menu.close.emit();
        }
        /**
         * Focuses the menu trigger.
         * @param origin Source of the menu trigger's focus.
         */
        focus(origin = 'program', options) {
            if (this._focusMonitor) {
                this._focusMonitor.focusVia(this._element, origin, options);
            }
            else {
                this._element.nativeElement.focus(options);
            }
        }
        /** Closes the menu and does the necessary cleanup. */
        _destroyMenu() {
            if (!this._overlayRef || !this.menuOpen) {
                return;
            }
            const menu = this.menu;
            this._closingActionsSubscription.unsubscribe();
            this._overlayRef.detach();
            this._restoreFocus();
            if (menu instanceof MatMenu) {
                menu._resetAnimation();
                if (menu.lazyContent) {
                    // Wait for the exit animation to finish before detaching the content.
                    menu._animationDone
                        .pipe(filter(event => event.toState === 'void'), take(1), 
                    // Interrupt if the content got re-attached.
                    takeUntil(menu.lazyContent._attached))
                        .subscribe({
                        next: () => menu.lazyContent.detach(),
                        // No matter whether the content got re-attached, reset the menu.
                        complete: () => this._setIsMenuOpen(false)
                    });
                }
                else {
                    this._setIsMenuOpen(false);
                }
            }
            else {
                this._setIsMenuOpen(false);
                if (menu.lazyContent) {
                    menu.lazyContent.detach();
                }
            }
        }
        /**
         * This method sets the menu state to open and focuses the first item if
         * the menu was opened via the keyboard.
         */
        _initMenu() {
            this.menu.parentMenu = this.triggersSubmenu() ? this._parentMenu : undefined;
            this.menu.direction = this.dir;
            this._setMenuElevation();
            this._setIsMenuOpen(true);
            this.menu.focusFirstItem(this._openedBy || 'program');
        }
        /** Updates the menu elevation based on the amount of parent menus that it has. */
        _setMenuElevation() {
            if (this.menu.setElevation) {
                let depth = 0;
                let parentMenu = this.menu.parentMenu;
                while (parentMenu) {
                    depth++;
                    parentMenu = parentMenu.parentMenu;
                }
                this.menu.setElevation(depth);
            }
        }
        /** Restores focus to the element that was focused before the menu was open. */
        _restoreFocus() {
            // We should reset focus if the user is navigating using a keyboard or
            // if we have a top-level trigger which might cause focus to be lost
            // when clicking on the backdrop.
            if (this.restoreFocus) {
                if (!this._openedBy) {
                    // Note that the focus style will show up both for `program` and
                    // `keyboard` so we don't have to specify which one it is.
                    this.focus();
                }
                else if (!this.triggersSubmenu()) {
                    this.focus(this._openedBy);
                }
            }
            this._openedBy = null;
        }
        // set state rather than toggle to support triggers sharing a menu
        _setIsMenuOpen(isOpen) {
            this._menuOpen = isOpen;
            this._menuOpen ? this.menuOpened.emit() : this.menuClosed.emit();
            if (this.triggersSubmenu()) {
                this._menuItemInstance._highlighted = isOpen;
            }
        }
        /**
         * This method checks that a valid instance of MatMenu has been passed into
         * matMenuTriggerFor. If not, an exception is thrown.
         */
        _checkMenu() {
            if (!this.menu) {
                throwMatMenuMissingError();
            }
        }
        /**
         * This method creates the overlay from the provided menu's template and saves its
         * OverlayRef so that it can be attached to the DOM when openMenu is called.
         */
        _createOverlay() {
            if (!this._overlayRef) {
                const config = this._getOverlayConfig();
                this._subscribeToPositions(config.positionStrategy);
                this._overlayRef = this._overlay.create(config);
                // Consume the `keydownEvents` in order to prevent them from going to another overlay.
                // Ideally we'd also have our keyboard event logic in here, however doing so will
                // break anybody that may have implemented the `MatMenuPanel` themselves.
                this._overlayRef.keydownEvents().subscribe();
            }
            return this._overlayRef;
        }
        /**
         * This method builds the configuration object needed to create the overlay, the OverlayState.
         * @returns OverlayConfig
         */
        _getOverlayConfig() {
            return new OverlayConfig({
                positionStrategy: this._overlay.position()
                    .flexibleConnectedTo(this._element)
                    .withLockedPosition()
                    .withTransformOriginOn('.mat-menu-panel, .mat-mdc-menu-panel'),
                backdropClass: this.menu.backdropClass || 'cdk-overlay-transparent-backdrop',
                scrollStrategy: this._scrollStrategy(),
                direction: this._dir
            });
        }
        /**
         * Listens to changes in the position of the overlay and sets the correct classes
         * on the menu based on the new position. This ensures the animation origin is always
         * correct, even if a fallback position is used for the overlay.
         */
        _subscribeToPositions(position) {
            if (this.menu.setPositionClasses) {
                position.positionChanges.subscribe(change => {
                    const posX = change.connectionPair.overlayX === 'start' ? 'after' : 'before';
                    const posY = change.connectionPair.overlayY === 'top' ? 'below' : 'above';
                    this.menu.setPositionClasses(posX, posY);
                });
            }
        }
        /**
         * Sets the appropriate positions on a position strategy
         * so the overlay connects with the trigger correctly.
         * @param positionStrategy Strategy whose position to update.
         */
        _setPosition(positionStrategy) {
            let [originX, originFallbackX] = this.menu.xPosition === 'before' ? ['end', 'start'] : ['start', 'end'];
            let [overlayY, overlayFallbackY] = this.menu.yPosition === 'above' ? ['bottom', 'top'] : ['top', 'bottom'];
            let [originY, originFallbackY] = [overlayY, overlayFallbackY];
            let [overlayX, overlayFallbackX] = [originX, originFallbackX];
            let offsetY = 0;
            if (this.triggersSubmenu()) {
                // When the menu is a sub-menu, it should always align itself
                // to the edges of the trigger, instead of overlapping it.
                overlayFallbackX = originX = this.menu.xPosition === 'before' ? 'start' : 'end';
                originFallbackX = overlayX = originX === 'end' ? 'start' : 'end';
                offsetY = overlayY === 'bottom' ? MENU_PANEL_TOP_PADDING : -MENU_PANEL_TOP_PADDING;
            }
            else if (!this.menu.overlapTrigger) {
                originY = overlayY === 'top' ? 'bottom' : 'top';
                originFallbackY = overlayFallbackY === 'top' ? 'bottom' : 'top';
            }
            positionStrategy.withPositions([
                { originX, originY, overlayX, overlayY, offsetY },
                { originX: originFallbackX, originY, overlayX: overlayFallbackX, overlayY, offsetY },
                {
                    originX,
                    originY: originFallbackY,
                    overlayX,
                    overlayY: overlayFallbackY,
                    offsetY: -offsetY
                },
                {
                    originX: originFallbackX,
                    originY: originFallbackY,
                    overlayX: overlayFallbackX,
                    overlayY: overlayFallbackY,
                    offsetY: -offsetY
                }
            ]);
        }
        /** Returns a stream that emits whenever an action that should close the menu occurs. */
        _menuClosingActions() {
            const backdrop = this._overlayRef.backdropClick();
            const detachments = this._overlayRef.detachments();
            const parentClose = this._parentMenu ? this._parentMenu.closed : observableOf();
            const hover = this._parentMenu ? this._parentMenu._hovered().pipe(filter(active => active !== this._menuItemInstance), filter(() => this._menuOpen)) : observableOf();
            return merge(backdrop, parentClose, hover, detachments);
        }
        /** Handles mouse presses on the trigger. */
        _handleMousedown(event) {
            if (!isFakeMousedownFromScreenReader(event)) {
                // Since right or middle button clicks won't trigger the `click` event,
                // we shouldn't consider the menu as opened by mouse in those cases.
                this._openedBy = event.button === 0 ? 'mouse' : null;
                // Since clicking on the trigger won't close the menu if it opens a sub-menu,
                // we should prevent focus from moving onto it via click to avoid the
                // highlight from lingering on the menu item.
                if (this.triggersSubmenu()) {
                    event.preventDefault();
                }
            }
        }
        /** Handles key presses on the trigger. */
        _handleKeydown(event) {
            const keyCode = event.keyCode;
            if (this.triggersSubmenu() && ((keyCode === RIGHT_ARROW && this.dir === 'ltr') ||
                (keyCode === LEFT_ARROW && this.dir === 'rtl'))) {
                this.openMenu();
            }
        }
        /** Handles click events on the trigger. */
        _handleClick(event) {
            if (this.triggersSubmenu()) {
                // Stop event propagation to avoid closing the parent menu.
                event.stopPropagation();
                this.openMenu();
            }
            else {
                this.toggleMenu();
            }
        }
        /** Handles the cases where the user hovers over the trigger. */
        _handleHover() {
            // Subscribe to changes in the hovered item in order to toggle the panel.
            if (!this.triggersSubmenu()) {
                return;
            }
            this._hoverSubscription = this._parentMenu._hovered()
                // Since we might have multiple competing triggers for the same menu (e.g. a sub-menu
                // with different data and triggers), we have to delay it by a tick to ensure that
                // it won't be closed immediately after it is opened.
                .pipe(filter(active => active === this._menuItemInstance && !active.disabled), delay(0, asapScheduler))
                .subscribe(() => {
                this._openedBy = 'mouse';
                // If the same menu is used between multiple triggers, it might still be animating
                // while the new trigger tries to re-open it. Wait for the animation to finish
                // before doing so. Also interrupt if the user moves to another item.
                if (this.menu instanceof MatMenu && this.menu._isAnimating) {
                    // We need the `delay(0)` here in order to avoid
                    // 'changed after checked' errors in some cases. See #12194.
                    this.menu._animationDone
                        .pipe(take(1), delay(0, asapScheduler), takeUntil(this._parentMenu._hovered()))
                        .subscribe(() => this.openMenu());
                }
                else {
                    this.openMenu();
                }
            });
        }
        /** Gets the portal that should be attached to the overlay. */
        _getPortal() {
            // Note that we can avoid this check by keeping the portal on the menu panel.
            // While it would be cleaner, we'd have to introduce another required method on
            // `MatMenuPanel`, making it harder to consume.
            if (!this._portal || this._portal.templateRef !== this.menu.templateRef) {
                this._portal = new TemplatePortal(this.menu.templateRef, this._viewContainerRef);
            }
            return this._portal;
        }
    }
    MatMenuTrigger.decorators = [
        { type: Directive, args: [{
                    selector: `[mat-menu-trigger-for], [matMenuTriggerFor]`,
                    host: {
                        'class': 'mat-menu-trigger',
                        'aria-haspopup': 'true',
                        '[attr.aria-expanded]': 'menuOpen || null',
                        '[attr.aria-controls]': 'menuOpen ? menu.panelId : null',
                        '(mousedown)': '_handleMousedown($event)',
                        '(keydown)': '_handleKeydown($event)',
                        '(click)': '_handleClick($event)',
                    },
                    exportAs: 'matMenuTrigger'
                },] }
    ];
    MatMenuTrigger.ctorParameters = () => [
        { type: Overlay },
        { type: ElementRef },
        { type: ViewContainerRef },
        { type: undefined, decorators: [{ type: Inject, args: [MAT_MENU_SCROLL_STRATEGY,] }] },
        { type: MatMenu, decorators: [{ type: Optional }] },
        { type: MatMenuItem, decorators: [{ type: Optional }, { type: Self }] },
        { type: Directionality, decorators: [{ type: Optional }] },
        { type: FocusMonitor }
    ];
    MatMenuTrigger.propDecorators = {
        _deprecatedMatMenuTriggerFor: [{ type: Input, args: ['mat-menu-trigger-for',] }],
        menu: [{ type: Input, args: ['matMenuTriggerFor',] }],
        menuData: [{ type: Input, args: ['matMenuTriggerData',] }],
        restoreFocus: [{ type: Input, args: ['matMenuTriggerRestoreFocus',] }],
        menuOpened: [{ type: Output }],
        onMenuOpen: [{ type: Output }],
        menuClosed: [{ type: Output }],
        onMenuClose: [{ type: Output }]
    };
    return MatMenuTrigger;
})();
export { MatMenuTrigger };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibWVudS10cmlnZ2VyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL21lbnUvbWVudS10cmlnZ2VyLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU8sRUFBQyxZQUFZLEVBQWUsK0JBQStCLEVBQUMsTUFBTSxtQkFBbUIsQ0FBQztBQUM3RixPQUFPLEVBQVksY0FBYyxFQUFDLE1BQU0sbUJBQW1CLENBQUM7QUFDNUQsT0FBTyxFQUFDLFVBQVUsRUFBRSxXQUFXLEVBQUMsTUFBTSx1QkFBdUIsQ0FBQztBQUM5RCxPQUFPLEVBR0wsT0FBTyxFQUNQLGFBQWEsR0FJZCxNQUFNLHNCQUFzQixDQUFDO0FBQzlCLE9BQU8sRUFBQyxjQUFjLEVBQUMsTUFBTSxxQkFBcUIsQ0FBQztBQUNuRCxPQUFPLEVBRUwsU0FBUyxFQUNULFVBQVUsRUFDVixZQUFZLEVBQ1osTUFBTSxFQUNOLGNBQWMsRUFDZCxLQUFLLEVBRUwsUUFBUSxFQUNSLE1BQU0sRUFDTixJQUFJLEVBQ0osZ0JBQWdCLEdBQ2pCLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBQywrQkFBK0IsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQ3RFLE9BQU8sRUFBQyxhQUFhLEVBQUUsS0FBSyxFQUFFLEVBQUUsSUFBSSxZQUFZLEVBQUUsWUFBWSxFQUFDLE1BQU0sTUFBTSxDQUFDO0FBQzVFLE9BQU8sRUFBQyxLQUFLLEVBQUUsTUFBTSxFQUFFLElBQUksRUFBRSxTQUFTLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQUM5RCxPQUFPLEVBQUMsT0FBTyxFQUFDLE1BQU0sUUFBUSxDQUFDO0FBQy9CLE9BQU8sRUFBQyx3QkFBd0IsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUN2RCxPQUFPLEVBQUMsV0FBVyxFQUFDLE1BQU0sYUFBYSxDQUFDO0FBSXhDLGtGQUFrRjtBQUNsRixNQUFNLENBQUMsTUFBTSx3QkFB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QUFJLElBQUksQ0FBQyxPQUFPLENBQUMsV0FBVyxLQUFLLElBQUksQ0FBQyxJQUFJLENBQUMsV0FBVyxFQUFFO2dCQUN2RSxJQUFJLENBQUMsT0FBTyxHQUFHLElBQUksY0FBYyxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsV0FBVyxFQUFFLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxDQUFDO2FBQ2xGO1lBRUQsT0FBTyxJQUFJLENBQUMsT0FBTyxDQUFDO1FBQ3RCLENBQUM7OztnQkFwZkYsU0FBUyxTQUFDO29CQUNULFFBQVEsRUFBRSw2Q0FBNkM7b0JBQ3ZELElBQUksRUFBRTt3QkFDSixPQUFPLEVBQUUsa0JBQWtCO3dCQUMzQixlQUFlLEVBQUUsTUFBTTt3QkFDdkIsc0JBQXNCLEVBQUUsa0JBQWtCO3dCQUMxQyxzQkFBc0IsRUFBRSxnQ0FBZ0M7d0JBQ3hELGFBQWEsRUFBRSwwQkFBMEI7d0JBQ3pDLFdBQVcsRUFBRSx3QkFBd0I7d0JBQ3JDLFNBQVMsRUFBRSxzQkFBc0I7cUJBQ2xDO29CQUNELFFBQVEsRUFBRSxnQkFBZ0I7aUJBQzNCOzs7Z0JBbkVDLE9BQU87Z0JBVVAsVUFBVTtnQkFTVixnQkFBZ0I7Z0RBeUlILE1BQU0sU0FBQyx3QkFBd0I7Z0JBcEl0QyxPQUFPLHVCQXFJQSxRQUFRO2dCQW5JZixXQUFXLHVCQW9JSixRQUFRLFlBQUksSUFBSTtnQkFuS1osY0FBYyx1QkFvS2xCLFFBQVE7Z0JBcktmLFlBQVk7OzsrQ0FpR2pCLEtBQUssU0FBQyxzQkFBc0I7dUJBTzVCLEtBQUssU0FBQyxtQkFBbUI7MkJBd0J6QixLQUFLLFNBQUMsb0JBQW9COytCQU8xQixLQUFLLFNBQUMsNEJBQTRCOzZCQUdsQyxNQUFNOzZCQVFOLE1BQU07NkJBR04sTUFBTTs4QkFRTixNQUFNOztJQXNaVCxxQkFBQztLQUFBO1NBemVZLGNBQWMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtGb2N1c01vbml0b3IsIEZvY3VzT3JpZ2luLCBpc0Zha2VNb3VzZWRvd25Gcm9tU2NyZWVuUmVhZGVyfSBmcm9tICdAYW5ndWxhci9jZGsvYTExeSc7XG5pbXBvcnQge0RpcmVjdGlvbiwgRGlyZWN0aW9uYWxpdHl9IGZyb20gJ0Bhbmd1bGFyL2Nkay9iaWRpJztcbmltcG9ydCB7TEVGVF9BUlJPVywgUklHSFRfQVJST1d9IGZyb20gJ0Bhbmd1bGFyL2Nkay9rZXljb2Rlcyc7XG5pbXBvcnQge1xuICBGbGV4aWJsZUNvbm5lY3RlZFBvc2l0aW9uU3RyYXRlZ3ksXG4gIEhvcml6b250YWxDb25uZWN0aW9uUG9zLFxuICBPdmVybGF5LFxuICBPdmVybGF5Q29uZmlnLFxuICBPdmVybGF5UmVmLFxuICBWZXJ0aWNhbENvbm5lY3Rpb25Qb3MsXG4gIFNjcm9sbFN0cmF0ZWd5LFxufSBmcm9tICdAYW5ndWxhci9jZGsvb3ZlcmxheSc7XG5pbXBvcnQge1RlbXBsYXRlUG9ydGFsfSBmcm9tICdAYW5ndWxhci9jZGsvcG9ydGFsJztcbmltcG9ydCB7XG4gIEFmdGVyQ29udGVudEluaXQsXG4gIERpcmVjdGl2ZSxcbiAgRWxlbWVudFJlZixcbiAgRXZlbnRFbWl0dGVyLFxuICBJbmplY3QsXG4gIEluamVjdGlvblRva2VuLFxuICBJbnB1dCxcbiAgT25EZXN0cm95LFxuICBPcHRpb25hbCxcbiAgT3V0cHV0LFxuICBTZWxmLFxuICBWaWV3Q29udGFpbmVyUmVmLFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7bm9ybWFsaXplUGFzc2l2ZUxpc3RlbmVyT3B0aW9uc30gZnJvbSAnQGFuZ3VsYXIvY2RrL3BsYXRmb3JtJztcbmltcG9ydCB7YXNhcFNjaGVkdWxlciwgbWVyZ2UsIG9mIGFzIG9ic2VydmFibGVPZiwgU3Vic2NyaXB0aW9ufSBmcm9tICdyeGpzJztcbmltcG9ydCB7ZGVsYXksIGZpbHRlciwgdGFrZSwgdGFrZVVudGlsfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5pbXBvcnQge01hdE1lbnV9IGZyb20gJy4vbWVudSc7XG5pbXBvcnQge3Rocm93TWF0TWVudU1pc3NpbmdFcnJvcn0gZnJvbSAnLi9tZW51LWVycm9ycyc7XG5pbXBvcnQge01hdE1lbnVJdGVtfSBmcm9tICcuL21lbnUtaXRlbSc7XG5pbXBvcnQge01hdE1lbnVQYW5lbH0gZnJvbSAnLi9tZW51LXBhbmVsJztcbmltcG9ydCB7TWVudVBvc2l0aW9uWCwgTWVudVBvc2l0aW9uWX0gZnJvbSAnLi9tZW51LXBvc2l0aW9ucyc7XG5cbi8qKiBJbmplY3Rpb24gdG9rZW4gdGhhdCBkZXRlcm1pbmVzIHRoZSBzY3JvbGwgaGFuZGxpbmcgd2hpbGUgdGhlIG1lbnUgaXMgb3Blbi4gKi9cbmV4cG9ydCBjb25zdCBNQVRfTUVOVV9TQ1JPTExfU1RSQVRFR1kgPVxuICAgIG5ldyBJbmplY3Rpb25Ub2tlbjwoKSA9PiBTY3JvbGxTdHJhdGVneT4oJ21hdC1tZW51LXNjcm9sbC1zdHJhdGVneScpO1xuXG4vKiogQGRvY3MtcHJpdmF0ZSAqL1xuZXhwb3J0IGZ1bmN0aW9uIE1BVF9NRU5VX1NDUk9MTF9TVFJBVEVHWV9GQUNUT1JZKG92ZXJsYXk6IE92ZXJsYXkpOiAoKSA9PiBTY3JvbGxTdHJhdGVneSB7XG4gIHJldHVybiAoKSA9PiBvdmVybGF5LnNjcm9sbFN0cmF0ZWdpZXMucmVwb3NpdGlvbigpO1xufVxuXG4vKiogQGRvY3MtcHJpdmF0ZSAqL1xuZXhwb3J0IGNvbnN0IE1BVF9NRU5VX1NDUk9MTF9TVFJBVEVHWV9GQUNUT1JZX1BST1ZJREVSID0ge1xuICBwcm92aWRlOiBNQVRfTUVOVV9TQ1JPTExfU1RSQVRFR1ksXG4gIGRlcHM6IFtPdmVybGF5XSxcbiAgdXNlRmFjdG9yeTogTUFUX01FTlVfU0NST0xMX1NUUkFURUdZX0ZBQ1RPUlksXG59O1xuXG4vKiogRGVmYXVsdCB0b3AgcGFkZGluZyBvZiB0aGUgbWVudSBwYW5lbC4gKi9cbmV4cG9ydCBjb25zdCBNRU5VX1BBTkVMX1RPUF9QQURESU5HID0gODtcblxuLyoqIE9wdGlvbnMgZm9yIGJpbmRpbmcgYSBwYXNzaXZlIGV2ZW50IGxpc3RlbmVyLiAqL1xuY29uc3QgcGFzc2l2ZUV2ZW50TGlzdGVuZXJPcHRpb25zID0gbm9ybWFsaXplUGFzc2l2ZUxpc3RlbmVyT3B0aW9ucyh7cGFzc2l2ZTogdHJ1ZX0pO1xuXG4vLyBUT0RPKGFuZHJld3NlZ3Vpbik6IFJlbW92ZSB0aGUga2ViYWIgdmVyc2lvbnMgaW4gZmF2b3Igb2YgY2FtZWxDYXNlZCBhdHRyaWJ1dGUgc2VsZWN0b3JzXG5cbi8qKiBEaXJlY3RpdmUgYXBwbGllZCB0byBhbiBlbGVtZW50IHRoYXQgc2hvdWxkIHRyaWdnZXIgYSBgbWF0LW1lbnVgLiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiBgW21hdC1tZW51LXRyaWdnZXItZm9yXSwgW21hdE1lbnVUcmlnZ2VyRm9yXWAsXG4gIGhvc3Q6IHtcbiAgICAnY2xhc3MnOiAnbWF0LW1lbnUtdHJpZ2dlcicsXG4gICAgJ2FyaWEtaGFzcG9wdXAnOiAndHJ1ZScsXG4gICAgJ1thdHRyLmFyaWEtZXhwYW5kZWRdJzogJ21lbnVPcGVuIHx8IG51bGwnLFxuICAgICdbYXR0ci5hcmlhLWNvbnRyb2xzXSc6ICdtZW51T3BlbiA/IG1lbnUucGFuZWxJZCA6IG51bGwnLFxuICAgICcobW91c2Vkb3duKSc6ICdfaGFuZGxlTW91c2Vkb3duKCRldmVudCknLFxuICAgICcoa2V5ZG93biknOiAnX2hhbmRsZUtleWRvd24oJGV2ZW50KScsXG4gICAgJyhjbGljayknOiAnX2hhbmRsZUNsaWNrKCRldmVudCknLFxuICB9LFxuICBleHBvcnRBczogJ21hdE1lbnVUcmlnZ2VyJ1xufSlcbmV4cG9ydCBjbGFzcyBNYXRNZW51VHJpZ2dlciBpbXBsZW1lbnRzIEFmdGVyQ29udGVudEluaXQsIE9uRGVzdHJveSB7XG4gIHByaXZhdGUgX3BvcnRhbDogVGVtcGxhdGVQb3J0YWw7XG4gIHByaXZhdGUgX292ZXJsYXlSZWY6IE92ZXJsYXlSZWYgfCBudWxsID0gbnVsbDtcbiAgcHJpdmF0ZSBfbWVudU9wZW46IGJvb2xlYW4gPSBmYWxzZTtcbiAgcHJpdmF0ZSBfY2xvc2luZ0FjdGlvbnNTdWJzY3JpcHRpb24gPSBTdWJzY3JpcHRpb24uRU1QVFk7XG4gIHByaXZhdGUgX2hvdmVyU3Vic2NyaXB0aW9uID0gU3Vic2NyaXB0aW9uLkVNUFRZO1xuICBwcml2YXRlIF9tZW51Q2xvc2VTdWJzY3JpcHRpb24gPSBTdWJzY3JpcHRpb24uRU1QVFk7XG4gIHByaXZhdGUgX3Njcm9sbFN0cmF0ZWd5OiAoKSA9PiBTY3JvbGxTdHJhdGVneTtcblxuICAvKipcbiAgICogSGFuZGxlcyB0b3VjaCBzdGFydCBldmVudHMgb24gdGhlIHRyaWdnZXIuXG4gICAqIE5lZWRzIHRvIGJlIGFuIGFycm93IGZ1bmN0aW9uIHNvIHdlIGNhbiBlYXNpbHkgdXNlIGFkZEV2ZW50TGlzdGVuZXIgYW5kIHJlbW92ZUV2ZW50TGlzdGVuZXIuXG4gICAqL1xuICBwcml2YXRlIF9oYW5kbGVUb3VjaFN0YXJ0ID0gKCkgPT4gdGhpcy5fb3BlbmVkQnkgPSAndG91Y2gnO1xuXG4gIC8vIFRyYWNraW5nIGlucHV0IHR5cGUgaXMgbmVjZXNzYXJ5IHNvIGl0J3MgcG9zc2libGUgdG8gb25seSBhdXRvLWZvY3VzXG4gIC8vIHRoZSBmaXJzdCBpdGVtIG9mIHRoZSBsaXN0IHdoZW4gdGhlIG1lbnUgaXMgb3BlbmVkIHZpYSB0aGUga2V5Ym9hcmRcbiAgX29wZW5lZEJ5OiAnbW91c2UnIHwgJ3RvdWNoJyB8IG51bGwgPSBudWxsO1xuXG4gIC8qKlxuICAgKiBAZGVwcmVjYXRlZFxuICAgKiBAYnJlYWtpbmctY2hhbmdlIDguMC4wXG4gICAqL1xuICBASW5wdXQoJ21hdC1tZW51LXRyaWdnZXItZm9yJylcbiAgZ2V0IF9kZXByZWNhdGVkTWF0TWVudVRyaWdnZXJGb3IoKTogTWF0TWVudVBhbmVsIHsgcmV0dXJuIHRoaXMubWVudTsgfVxuICBzZXQgX2RlcHJlY2F0ZWRNYXRNZW51VHJpZ2dlckZvcih2OiBNYXRNZW51UGFuZWwpIHtcbiAgICB0aGlzLm1lbnUgPSB2O1xuICB9XG5cbiAgLyoqIFJlZmVyZW5jZXMgdGhlIG1lbnUgaW5zdGFuY2UgdGhhdCB0aGUgdHJpZ2dlciBpcyBhc3NvY2lhdGVkIHdpdGguICovXG4gIEBJbnB1dCgnbWF0TWVudVRyaWdnZXJGb3InKVxuICBnZXQgbWVudSgpIHsgcmV0dXJuIHRoaXMuX21lbnU7IH1cbiAgc2V0IG1lbnUobWVudTogTWF0TWVudVBhbmVsKSB7XG4gICAgaWYgKG1lbnUgPT09IHRoaXMuX21lbnUpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICB0aGlzLl9tZW51ID0gbWVudTtcbiAgICB0aGlzLl9tZW51Q2xvc2VTdWJzY3JpcHRpb24udW5zdWJzY3JpYmUoKTtcblxuICAgIGlmIChtZW51KSB7XG4gICAgICB0aGlzLl9tZW51Q2xvc2VTdWJzY3JpcHRpb24gPSBtZW51LmNsb3NlLmFzT2JzZXJ2YWJsZSgpLnN1YnNjcmliZShyZWFzb24gPT4ge1xuICAgICAgICB0aGlzLl9kZXN0cm95TWVudSgpO1xuXG4gICAgICAgIC8vIElmIGEgY2xpY2sgY2xvc2VkIHRoZSBtZW51LCB3ZSBzaG91bGQgY2xvc2UgdGhlIGVudGlyZSBjaGFpbiBvZiBuZXN0ZWQgbWVudXMuXG4gICAgICAgIGlmICgocmVhc29uID09PSAnY2xpY2snIHx8IHJlYXNvbiA9PT0gJ3RhYicpICYmIHRoaXMuX3BhcmVudE1lbnUpIHtcbiAgICAgICAgICB0aGlzLl9wYXJlbnRNZW51LmNsb3NlZC5lbWl0KHJlYXNvbik7XG4gICAgICAgIH1cbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuICBwcml2YXRlIF9tZW51OiBNYXRNZW51UGFuZWw7XG5cbiAgLyoqIERhdGEgdG8gYmUgcGFzc2VkIGFsb25nIHRvIGFueSBsYXppbHktcmVuZGVyZWQgY29udGVudC4gKi9cbiAgQElucHV0KCdtYXRNZW51VHJpZ2dlckRhdGEnKSBtZW51RGF0YTogYW55O1xuXG4gIC8qKlxuICAgKiBXaGV0aGVyIGZvY3VzIHNob3VsZCBiZSByZXN0b3JlZCB3aGVuIHRoZSBtZW51IGlzIGNsb3NlZC5cbiAgICogTm90ZSB0aGF0IGRpc2FibGluZyB0aGlzIG9wdGlvbiBjYW4gaGF2ZSBhY2Nlc3NpYmlsaXR5IGltcGxpY2F0aW9uc1xuICAgKiBhbmQgaXQncyB1cCB0byB5b3UgdG8gbWFuYWdlIGZvY3VzLCBpZiB5b3UgZGVjaWRlIHRvIHR1cm4gaXQgb2ZmLlxuICAgKi9cbiAgQElucHV0KCdtYXRNZW51VHJpZ2dlclJlc3RvcmVGb2N1cycpIHJlc3RvcmVGb2N1czogYm9vbGVhbiA9IHRydWU7XG5cbiAgLyoqIEV2ZW50IGVtaXR0ZWQgd2hlbiB0aGUgYXNzb2NpYXRlZCBtZW51IGlzIG9wZW5lZC4gKi9cbiAgQE91dHB1dCgpIHJlYWRvbmx5IG1lbnVPcGVuZWQ6IEV2ZW50RW1pdHRlcjx2b2lkPiA9IG5ldyBFdmVudEVtaXR0ZXI8dm9pZD4oKTtcblxuICAvKipcbiAgICogRXZlbnQgZW1pdHRlZCB3aGVuIHRoZSBhc3NvY2lhdGVkIG1lbnUgaXMgb3BlbmVkLlxuICAgKiBAZGVwcmVjYXRlZCBTd2l0Y2ggdG8gYG1lbnVPcGVuZWRgIGluc3RlYWRcbiAgICogQGJyZWFraW5nLWNoYW5nZSA4LjAuMFxuICAgKi9cbiAgLy8gdHNsaW50OmRpc2FibGUtbmV4dC1saW5lOm5vLW91dHB1dC1vbi1wcmVmaXhcbiAgQE91dHB1dCgpIHJlYWRvbmx5IG9uTWVudU9wZW46IEV2ZW50RW1pdHRlcjx2b2lkPiA9IHRoaXMubWVudU9wZW5lZDtcblxuICAvKiogRXZlbnQgZW1pdHRlZCB3aGVuIHRoZSBhc3NvY2lhdGVkIG1lbnUgaXMgY2xvc2VkLiAqL1xuICBAT3V0cHV0KCkgcmVhZG9ubHkgbWVudUNsb3NlZDogRXZlbnRFbWl0dGVyPHZvaWQ+ID0gbmV3IEV2ZW50RW1pdHRlcjx2b2lkPigpO1xuXG4gIC8qKlxuICAgKiBFdmVudCBlbWl0dGVkIHdoZW4gdGhlIGFzc29jaWF0ZWQgbWVudSBpcyBjbG9zZWQuXG4gICAqIEBkZXByZWNhdGVkIFN3aXRjaCB0byBgbWVudUNsb3NlZGAgaW5zdGVhZFxuICAgKiBAYnJlYWtpbmctY2hhbmdlIDguMC4wXG4gICAqL1xuICAvLyB0c2xpbnQ6ZGlzYWJsZS1uZXh0LWxpbmU6bm8tb3V0cHV0LW9uLXByZWZpeFxuICBAT3V0cHV0KCkgcmVhZG9ubHkgb25NZW51Q2xvc2U6IEV2ZW50RW1pdHRlcjx2b2lkPiA9IHRoaXMubWVudUNsb3NlZDtcblxuICBjb25zdHJ1Y3Rvcihwcml2YXRlIF9vdmVybGF5OiBPdmVybGF5LFxuICAgICAgICAgICAgICBwcml2YXRlIF9lbGVtZW50OiBFbGVtZW50UmVmPEhUTUxFbGVtZW50PixcbiAgICAgICAgICAgICAgcHJpdmF0ZSBfdmlld0NvbnRhaW5lclJlZjogVmlld0NvbnRhaW5lclJlZixcbiAgICAgICAgICAgICAgQEluamVjdChNQVRfTUVOVV9TQ1JPTExfU1RSQVRFR1kpIHNjcm9sbFN0cmF0ZWd5OiBhbnksXG4gICAgICAgICAgICAgIEBPcHRpb25hbCgpIHByaXZhdGUgX3BhcmVudE1lbnU6IE1hdE1lbnUsXG4gICAgICAgICAgICAgIEBPcHRpb25hbCgpIEBTZWxmKCkgcHJpdmF0ZSBfbWVudUl0ZW1JbnN0YW5jZTogTWF0TWVudUl0ZW0sXG4gICAgICAgICAgICAgIEBPcHRpb25hbCgpIHByaXZhdGUgX2RpcjogRGlyZWN0aW9uYWxpdHksXG4gICAgICAgICAgICAgIC8vIFRPRE8oY3Jpc2JldG8pOiBtYWtlIHRoZSBfZm9jdXNNb25pdG9yIHJlcXVpcmVkIHdoZW4gZG9pbmcgYnJlYWtpbmcgY2hhbmdlcy5cbiAgICAgICAgICAgICAgLy8gQGJyZWFraW5nLWNoYW5nZSA4LjAuMFxuICAgICAgICAgICAgICBwcml2YXRlIF9mb2N1c01vbml0b3I/OiBGb2N1c01vbml0b3IpIHtcblxuICAgIF9lbGVtZW50Lm5hdGl2ZUVsZW1lbnQuYWRkRXZlbnRMaXN0ZW5lcigndG91Y2hzdGFydCcsIHRoaXMuX2hhbmRsZVRvdWNoU3RhcnQsXG4gICAgICAgIHBhc3NpdmVFdmVudExpc3RlbmVyT3B0aW9ucyk7XG5cbiAgICBpZiAoX21lbnVJdGVtSW5zdGFuY2UpIHtcbiAgICAgIF9tZW51SXRlbUluc3RhbmNlLl90cmlnZ2Vyc1N1Ym1lbnUgPSB0aGlzLnRyaWdnZXJzU3VibWVudSgpO1xuICAgIH1cblxuICAgIHRoaXMuX3Njcm9sbFN0cmF0ZWd5ID0gc2Nyb2xsU3RyYXRlZ3k7XG4gIH1cblxuICBuZ0FmdGVyQ29udGVudEluaXQoKSB7XG4gICAgdGhpcy5fY2hlY2tNZW51KCk7XG4gICAgdGhpcy5faGFuZGxlSG92ZXIoKTtcbiAgfVxuXG4gIG5nT25EZXN0cm95KCkge1xuICAgIGlmICh0aGlzLl9vdmVybGF5UmVmKSB7XG4gICAgICB0aGlzLl9vdmVybGF5UmVmLmRpc3Bvc2UoKTtcbiAgICAgIHRoaXMuX292ZXJsYXlSZWYgPSBudWxsO1xuICAgIH1cblxuICAgIHRoaXMuX2VsZW1lbnQubmF0aXZlRWxlbWVudC5yZW1vdmVFdmVudExpc3RlbmVyKCd0b3VjaHN0YXJ0JywgdGhpcy5faGFuZGxlVG91Y2hTdGFydCxcbiAgICAgICAgcGFzc2l2ZUV2ZW50TGlzdGVuZXJPcHRpb25zKTtcblxuICAgIHRoaXMuX21lbnVDbG9zZVN1YnNjcmlwdGlvbi51bnN1YnNjcmliZSgpO1xuICAgIHRoaXMuX2Nsb3NpbmdBY3Rpb25zU3Vic2NyaXB0aW9uLnVuc3Vic2NyaWJlKCk7XG4gICAgdGhpcy5faG92ZXJTdWJzY3JpcHRpb24udW5zdWJzY3JpYmUoKTtcbiAgfVxuXG4gIC8qKiBXaGV0aGVyIHRoZSBtZW51IGlzIG9wZW4uICovXG4gIGdldCBtZW51T3BlbigpOiBib29sZWFuIHtcbiAgICByZXR1cm4gdGhpcy5fbWVudU9wZW47XG4gIH1cblxuICAvKiogVGhlIHRleHQgZGlyZWN0aW9uIG9mIHRoZSBjb250YWluaW5nIGFwcC4gKi9cbiAgZ2V0IGRpcigpOiBEaXJlY3Rpb24ge1xuICAgIHJldHVybiB0aGlzLl9kaXIgJiYgdGhpcy5fZGlyLnZhbHVlID09PSAncnRsJyA/ICdydGwnIDogJ2x0cic7XG4gIH1cblxuICAvKiogV2hldGhlciB0aGUgbWVudSB0cmlnZ2VycyBhIHN1Yi1tZW51IG9yIGEgdG9wLWxldmVsIG9uZS4gKi9cbiAgdHJpZ2dlcnNTdWJtZW51KCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiAhISh0aGlzLl9tZW51SXRlbUluc3RhbmNlICYmIHRoaXMuX3BhcmVudE1lbnUpO1xuICB9XG5cbiAgLyoqIFRvZ2dsZXMgdGhlIG1lbnUgYmV0d2VlbiB0aGUgb3BlbiBhbmQgY2xvc2VkIHN0YXRlcy4gKi9cbiAgdG9nZ2xlTWVudSgpOiB2b2lkIHtcbiAgICByZXR1cm4gdGhpcy5fbWVudU9wZW4gPyB0aGlzLmNsb3NlTWVudSgpIDogdGhpcy5vcGVuTWVudSgpO1xuICB9XG5cbiAgLyoqIE9wZW5zIHRoZSBtZW51LiAqL1xuICBvcGVuTWVudSgpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5fbWVudU9wZW4pIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICB0aGlzLl9jaGVja01lbnUoKTtcblxuICAgIGNvbnN0IG92ZXJsYXlSZWYgPSB0aGlzLl9jcmVhdGVPdmVybGF5KCk7XG4gICAgY29uc3Qgb3ZlcmxheUNvbmZpZyA9IG92ZXJsYXlSZWYuZ2V0Q29uZmlnKCk7XG5cbiAgICB0aGlzLl9zZXRQb3NpdGlvbihvdmVybGF5Q29uZmlnLnBvc2l0aW9uU3RyYXRlZ3kgYXMgRmxleGlibGVDb25uZWN0ZWRQb3NpdGlvblN0cmF0ZWd5KTtcbiAgICBvdmVybGF5Q29uZmlnLmhhc0JhY2tkcm9wID0gdGhpcy5tZW51Lmhhc0JhY2tkcm9wID09IG51bGwgPyAhdGhpcy50cmlnZ2Vyc1N1Ym1lbnUoKSA6XG4gICAgICAgIHRoaXMubWVudS5oYXNCYWNrZHJvcDtcbiAgICBvdmVybGF5UmVmLmF0dGFjaCh0aGlzLl9nZXRQb3J0YWwoKSk7XG5cbiAgICBpZiAodGhpcy5tZW51LmxhenlDb250ZW50KSB7XG4gICAgICB0aGlzLm1lbnUubGF6eUNvbnRlbnQuYXR0YWNoKHRoaXMubWVudURhdGEpO1xuICAgIH1cblxuICAgIHRoaXMuX2Nsb3NpbmdBY3Rpb25zU3Vic2NyaXB0aW9uID0gdGhpcy5fbWVudUNsb3NpbmdBY3Rpb25zKCkuc3Vic2NyaWJlKCgpID0+IHRoaXMuY2xvc2VNZW51KCkpO1xuICAgIHRoaXMuX2luaXRNZW51KCk7XG5cbiAgICBpZiAodGhpcy5tZW51IGluc3RhbmNlb2YgTWF0TWVudSkge1xuICAgICAgdGhpcy5tZW51Ll9zdGFydEFuaW1hdGlvbigpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBDbG9zZXMgdGhlIG1lbnUuICovXG4gIGNsb3NlTWVudSgpOiB2b2lkIHtcbiAgICB0aGlzLm1lbnUuY2xvc2UuZW1pdCgpO1xuICB9XG5cbiAgLyoqXG4gICAqIEZvY3VzZXMgdGhlIG1lbnUgdHJpZ2dlci5cbiAgICogQHBhcmFtIG9yaWdpbiBTb3VyY2Ugb2YgdGhlIG1lbnUgdHJpZ2dlcidzIGZvY3VzLlxuICAgKi9cbiAgZm9jdXMob3JpZ2luOiBGb2N1c09yaWdpbiA9ICdwcm9ncmFtJywgb3B0aW9ucz86IEZvY3VzT3B0aW9ucykge1xuICAgIGlmICh0aGlzLl9mb2N1c01vbml0b3IpIHtcbiAgICAgIHRoaXMuX2ZvY3VzTW9uaXRvci5mb2N1c1ZpYSh0aGlzLl9lbGVtZW50LCBvcmlnaW4sIG9wdGlvbnMpO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLl9lbGVtZW50Lm5hdGl2ZUVsZW1lbnQuZm9jdXMob3B0aW9ucyk7XG4gICAgfVxuICB9XG5cbiAgLyoqIENsb3NlcyB0aGUgbWVudSBhbmQgZG9lcyB0aGUgbmVjZXNzYXJ5IGNsZWFudXAuICovXG4gIHByaXZhdGUgX2Rlc3Ryb3lNZW51KCkge1xuICAgIGlmICghdGhpcy5fb3ZlcmxheVJlZiB8fCAhdGhpcy5tZW51T3Blbikge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGNvbnN0IG1lbnUgPSB0aGlzLm1lbnU7XG4gICAgdGhpcy5fY2xvc2luZ0FjdGlvbnNTdWJzY3JpcHRpb24udW5zdWJzY3JpYmUoKTtcbiAgICB0aGlzLl9vdmVybGF5UmVmLmRldGFjaCgpO1xuICAgIHRoaXMuX3Jlc3RvcmVGb2N1cygpO1xuXG4gICAgaWYgKG1lbnUgaW5zdGFuY2VvZiBNYXRNZW51KSB7XG4gICAgICBtZW51Ll9yZXNldEFuaW1hdGlvbigpO1xuXG4gICAgICBpZiAobWVudS5sYXp5Q29udGVudCkge1xuICAgICAgICAvLyBXYWl0IGZvciB0aGUgZXhpdCBhbmltYXRpb24gdG8gZmluaXNoIGJlZm9yZSBkZXRhY2hpbmcgdGhlIGNvbnRlbnQuXG4gICAgICAgIG1lbnUuX2FuaW1hdGlvbkRvbmVcbiAgICAgICAgICAucGlwZShcbiAgICAgICAgICAgIGZpbHRlcihldmVudCA9PiBldmVudC50b1N0YXRlID09PSAndm9pZCcpLFxuICAgICAgICAgICAgdGFrZSgxKSxcbiAgICAgICAgICAgIC8vIEludGVycnVwdCBpZiB0aGUgY29udGVudCBnb3QgcmUtYXR0YWNoZWQuXG4gICAgICAgICAgICB0YWtlVW50aWwobWVudS5sYXp5Q29udGVudC5fYXR0YWNoZWQpXG4gICAgICAgICAgKVxuICAgICAgICAgIC5zdWJzY3JpYmUoe1xuICAgICAgICAgICAgbmV4dDogKCkgPT4gbWVudS5sYXp5Q29udGVudCEuZGV0YWNoKCksXG4gICAgICAgICAgICAvLyBObyBtYXR0ZXIgd2hldGhlciB0aGUgY29udGVudCBnb3QgcmUtYXR0YWNoZWQsIHJlc2V0IHRoZSBtZW51LlxuICAgICAgICAgICAgY29tcGxldGU6ICgpID0+IHRoaXMuX3NldElzTWVudU9wZW4oZmFsc2UpXG4gICAgICAgICAgfSk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICB0aGlzLl9zZXRJc01lbnVPcGVuKGZhbHNlKTtcbiAgICAgIH1cbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5fc2V0SXNNZW51T3BlbihmYWxzZSk7XG5cbiAgICAgIGlmIChtZW51LmxhenlDb250ZW50KSB7XG4gICAgICAgIG1lbnUubGF6eUNvbnRlbnQuZGV0YWNoKCk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIFRoaXMgbWV0aG9kIHNldHMgdGhlIG1lbnUgc3RhdGUgdG8gb3BlbiBhbmQgZm9jdXNlcyB0aGUgZmlyc3QgaXRlbSBpZlxuICAgKiB0aGUgbWVudSB3YXMgb3BlbmVkIHZpYSB0aGUga2V5Ym9hcmQuXG4gICAqL1xuICBwcml2YXRlIF9pbml0TWVudSgpOiB2b2lkIHtcbiAgICB0aGlzLm1lbnUucGFyZW50TWVudSA9IHRoaXMudHJpZ2dlcnNTdWJtZW51KCkgPyB0aGlzLl9wYXJlbnRNZW51IDogdW5kZWZpbmVkO1xuICAgIHRoaXMubWVudS5kaXJlY3Rpb24gPSB0aGlzLmRpcjtcbiAgICB0aGlzLl9zZXRNZW51RWxldmF0aW9uKCk7XG4gICAgdGhpcy5fc2V0SXNNZW51T3Blbih0cnVlKTtcbiAgICB0aGlzLm1lbnUuZm9jdXNGaXJzdEl0ZW0odGhpcy5fb3BlbmVkQnkgfHwgJ3Byb2dyYW0nKTtcbiAgfVxuXG4gIC8qKiBVcGRhdGVzIHRoZSBtZW51IGVsZXZhdGlvbiBiYXNlZCBvbiB0aGUgYW1vdW50IG9mIHBhcmVudCBtZW51cyB0aGF0IGl0IGhhcy4gKi9cbiAgcHJpdmF0ZSBfc2V0TWVudUVsZXZhdGlvbigpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5tZW51LnNldEVsZXZhdGlvbikge1xuICAgICAgbGV0IGRlcHRoID0gMDtcbiAgICAgIGxldCBwYXJlbnRNZW51ID0gdGhpcy5tZW51LnBhcmVudE1lbnU7XG5cbiAgICAgIHdoaWxlIChwYXJlbnRNZW51KSB7XG4gICAgICAgIGRlcHRoKys7XG4gICAgICAgIHBhcmVudE1lbnUgPSBwYXJlbnRNZW51LnBhcmVudE1lbnU7XG4gICAgICB9XG5cbiAgICAgIHRoaXMubWVudS5zZXRFbGV2YXRpb24oZGVwdGgpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBSZXN0b3JlcyBmb2N1cyB0byB0aGUgZWxlbWVudCB0aGF0IHdhcyBmb2N1c2VkIGJlZm9yZSB0aGUgbWVudSB3YXMgb3Blbi4gKi9cbiAgcHJpdmF0ZSBfcmVzdG9yZUZvY3VzKCkge1xuICAgIC8vIFdlIHNob3VsZCByZXNldCBmb2N1cyBpZiB0aGUgdXNlciBpcyBuYXZpZ2F0aW5nIHVzaW5nIGEga2V5Ym9hcmQgb3JcbiAgICAvLyBpZiB3ZSBoYXZlIGEgdG9wLWxldmVsIHRyaWdnZXIgd2hpY2ggbWlnaHQgY2F1c2UgZm9jdXMgdG8gYmUgbG9zdFxuICAgIC8vIHdoZW4gY2xpY2tpbmcgb24gdGhlIGJhY2tkcm9wLlxuICAgIGlmICh0aGlzLnJlc3RvcmVGb2N1cykge1xuICAgICAgaWYgKCF0aGlzLl9vcGVuZWRCeSkge1xuICAgICAgICAvLyBOb3RlIHRoYXQgdGhlIGZvY3VzIHN0eWxlIHdpbGwgc2hvdyB1cCBib3RoIGZvciBgcHJvZ3JhbWAgYW5kXG4gICAgICAgIC8vIGBrZXlib2FyZGAgc28gd2UgZG9uJ3QgaGF2ZSB0byBzcGVjaWZ5IHdoaWNoIG9uZSBpdCBpcy5cbiAgICAgICAgdGhpcy5mb2N1cygpO1xuICAgICAgfSBlbHNlIGlmICghdGhpcy50cmlnZ2Vyc1N1Ym1lbnUoKSkge1xuICAgICAgICB0aGlzLmZvY3VzKHRoaXMuX29wZW5lZEJ5KTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICB0aGlzLl9vcGVuZWRCeSA9IG51bGw7XG4gIH1cblxuICAvLyBzZXQgc3RhdGUgcmF0aGVyIHRoYW4gdG9nZ2xlIHRvIHN1cHBvcnQgdHJpZ2dlcnMgc2hhcmluZyBhIG1lbnVcbiAgcHJpdmF0ZSBfc2V0SXNNZW51T3Blbihpc09wZW46IGJvb2xlYW4pOiB2b2lkIHtcbiAgICB0aGlzLl9tZW51T3BlbiA9IGlzT3BlbjtcbiAgICB0aGlzLl9tZW51T3BlbiA/IHRoaXMubWVudU9wZW5lZC5lbWl0KCkgOiB0aGlzLm1lbnVDbG9zZWQuZW1pdCgpO1xuXG4gICAgaWYgKHRoaXMudHJpZ2dlcnNTdWJtZW51KCkpIHtcbiAgICAgIHRoaXMuX21lbnVJdGVtSW5zdGFuY2UuX2hpZ2hsaWdodGVkID0gaXNPcGVuO1xuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBUaGlzIG1ldGhvZCBjaGVja3MgdGhhdCBhIHZhbGlkIGluc3RhbmNlIG9mIE1hdE1lbnUgaGFzIGJlZW4gcGFzc2VkIGludG9cbiAgICogbWF0TWVudVRyaWdnZXJGb3IuIElmIG5vdCwgYW4gZXhjZXB0aW9uIGlzIHRocm93bi5cbiAgICovXG4gIHByaXZhdGUgX2NoZWNrTWVudSgpIHtcbiAgICBpZiAoIXRoaXMubWVudSkge1xuICAgICAgdGhyb3dNYXRNZW51TWlzc2luZ0Vycm9yKCk7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIFRoaXMgbWV0aG9kIGNyZWF0ZXMgdGhlIG92ZXJsYXkgZnJvbSB0aGUgcHJvdmlkZWQgbWVudSdzIHRlbXBsYXRlIGFuZCBzYXZlcyBpdHNcbiAgICogT3ZlcmxheVJlZiBzbyB0aGF0IGl0IGNhbiBiZSBhdHRhY2hlZCB0byB0aGUgRE9NIHdoZW4gb3Blbk1lbnUgaXMgY2FsbGVkLlxuICAgKi9cbiAgcHJpdmF0ZSBfY3JlYXRlT3ZlcmxheSgpOiBPdmVybGF5UmVmIHtcbiAgICBpZiAoIXRoaXMuX292ZXJsYXlSZWYpIHtcbiAgICAgIGNvbnN0IGNvbmZpZyA9IHRoaXMuX2dldE92ZXJsYXlDb25maWcoKTtcbiAgICAgIHRoaXMuX3N1YnNjcmliZVRvUG9zaXRpb25zKGNvbmZpZy5wb3NpdGlvblN0cmF0ZWd5IGFzIEZsZXhpYmxlQ29ubmVjdGVkUG9zaXRpb25TdHJhdGVneSk7XG4gICAgICB0aGlzLl9vdmVybGF5UmVmID0gdGhpcy5fb3ZlcmxheS5jcmVhdGUoY29uZmlnKTtcblxuICAgICAgLy8gQ29uc3VtZSB0aGUgYGtleWRvd25FdmVudHNgIGluIG9yZGVyIHRvIHByZXZlbnQgdGhlbSBmcm9tIGdvaW5nIHRvIGFub3RoZXIgb3ZlcmxheS5cbiAgICAgIC8vIElkZWFsbHkgd2UnZCBhbHNvIGhhdmUgb3VyIGtleWJvYXJkIGV2ZW50IGxvZ2ljIGluIGhlcmUsIGhvd2V2ZXIgZG9pbmcgc28gd2lsbFxuICAgICAgLy8gYnJlYWsgYW55Ym9keSB0aGF0IG1heSBoYXZlIGltcGxlbWVudGVkIHRoZSBgTWF0TWVudVBhbmVsYCB0aGVtc2VsdmVzLlxuICAgICAgdGhpcy5fb3ZlcmxheVJlZi5rZXlkb3duRXZlbnRzKCkuc3Vic2NyaWJlKCk7XG4gICAgfVxuXG4gICAgcmV0dXJuIHRoaXMuX292ZXJsYXlSZWY7XG4gIH1cblxuICAvKipcbiAgICogVGhpcyBtZXRob2QgYnVpbGRzIHRoZSBjb25maWd1cmF0aW9uIG9iamVjdCBuZWVkZWQgdG8gY3JlYXRlIHRoZSBvdmVybGF5LCB0aGUgT3ZlcmxheVN0YXRlLlxuICAgKiBAcmV0dXJucyBPdmVybGF5Q29uZmlnXG4gICAqL1xuICBwcml2YXRlIF9nZXRPdmVybGF5Q29uZmlnKCk6IE92ZXJsYXlDb25maWcge1xuICAgIHJldHVybiBuZXcgT3ZlcmxheUNvbmZpZyh7XG4gICAgICBwb3NpdGlvblN0cmF0ZWd5OiB0aGlzLl9vdmVybGF5LnBvc2l0aW9uKClcbiAgICAgICAgICAuZmxleGlibGVDb25uZWN0ZWRUbyh0aGlzLl9lbGVtZW50KVxuICAgICAgICAgIC53aXRoTG9ja2VkUG9zaXRpb24oKVxuICAgICAgICAgIC53aXRoVHJhbnNmb3JtT3JpZ2luT24oJy5tYXQtbWVudS1wYW5lbCwgLm1hdC1tZGMtbWVudS1wYW5lbCcpLFxuICAgICAgYmFja2Ryb3BDbGFzczogdGhpcy5tZW51LmJhY2tkcm9wQ2xhc3MgfHwgJ2Nkay1vdmVybGF5LXRyYW5zcGFyZW50LWJhY2tkcm9wJyxcbiAgICAgIHNjcm9sbFN0cmF0ZWd5OiB0aGlzLl9zY3JvbGxTdHJhdGVneSgpLFxuICAgICAgZGlyZWN0aW9uOiB0aGlzLl9kaXJcbiAgICB9KTtcbiAgfVxuXG4gIC8qKlxuICAgKiBMaXN0ZW5zIHRvIGNoYW5nZXMgaW4gdGhlIHBvc2l0aW9uIG9mIHRoZSBvdmVybGF5IGFuZCBzZXRzIHRoZSBjb3JyZWN0IGNsYXNzZXNcbiAgICogb24gdGhlIG1lbnUgYmFzZWQgb24gdGhlIG5ldyBwb3NpdGlvbi4gVGhpcyBlbnN1cmVzIHRoZSBhbmltYXRpb24gb3JpZ2luIGlzIGFsd2F5c1xuICAgKiBjb3JyZWN0LCBldmVuIGlmIGEgZmFsbGJhY2sgcG9zaXRpb24gaXMgdXNlZCBmb3IgdGhlIG92ZXJsYXkuXG4gICAqL1xuICBwcml2YXRlIF9zdWJzY3JpYmVUb1Bvc2l0aW9ucyhwb3NpdGlvbjogRmxleGlibGVDb25uZWN0ZWRQb3NpdGlvblN0cmF0ZWd5KTogdm9pZCB7XG4gICAgaWYgKHRoaXMubWVudS5zZXRQb3NpdGlvbkNsYXNzZXMpIHtcbiAgICAgIHBvc2l0aW9uLnBvc2l0aW9uQ2hhbmdlcy5zdWJzY3JpYmUoY2hhbmdlID0+IHtcbiAgICAgICAgY29uc3QgcG9zWDogTWVudVBvc2l0aW9uWCA9IGNoYW5nZS5jb25uZWN0aW9uUGFpci5vdmVybGF5WCA9PT0gJ3N0YXJ0JyA/ICdhZnRlcicgOiAnYmVmb3JlJztcbiAgICAgICAgY29uc3QgcG9zWTogTWVudVBvc2l0aW9uWSA9IGNoYW5nZS5jb25uZWN0aW9uUGFpci5vdmVybGF5WSA9PT0gJ3RvcCcgPyAnYmVsb3cnIDogJ2Fib3ZlJztcblxuICAgICAgICB0aGlzLm1lbnUuc2V0UG9zaXRpb25DbGFzc2VzIShwb3NYLCBwb3NZKTtcbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBTZXRzIHRoZSBhcHByb3ByaWF0ZSBwb3NpdGlvbnMgb24gYSBwb3NpdGlvbiBzdHJhdGVneVxuICAgKiBzbyB0aGUgb3ZlcmxheSBjb25uZWN0cyB3aXRoIHRoZSB0cmlnZ2VyIGNvcnJlY3RseS5cbiAgICogQHBhcmFtIHBvc2l0aW9uU3RyYXRlZ3kgU3RyYXRlZ3kgd2hvc2UgcG9zaXRpb24gdG8gdXBkYXRlLlxuICAgKi9cbiAgcHJpdmF0ZSBfc2V0UG9zaXRpb24ocG9zaXRpb25TdHJhdGVneTogRmxleGlibGVDb25uZWN0ZWRQb3NpdGlvblN0cmF0ZWd5KSB7XG4gICAgbGV0IFtvcmlnaW5YLCBvcmlnaW5GYWxsYmFja1hdOiBIb3Jpem9udGFsQ29ubmVjdGlvblBvc1tdID1cbiAgICAgICAgdGhpcy5tZW51LnhQb3NpdGlvbiA9PT0gJ2JlZm9yZScgPyBbJ2VuZCcsICdzdGFydCddIDogWydzdGFydCcsICdlbmQnXTtcblxuICAgIGxldCBbb3ZlcmxheVksIG92ZXJsYXlGYWxsYmFja1ldOiBWZXJ0aWNhbENvbm5lY3Rpb25Qb3NbXSA9XG4gICAgICAgIHRoaXMubWVudS55UG9zaXRpb24gPT09ICdhYm92ZScgPyBbJ2JvdHRvbScsICd0b3AnXSA6IFsndG9wJywgJ2JvdHRvbSddO1xuXG4gICAgbGV0IFtvcmlnaW5ZLCBvcmlnaW5GYWxsYmFja1ldID0gW292ZXJsYXlZLCBvdmVybGF5RmFsbGJhY2tZXTtcbiAgICBsZXQgW292ZXJsYXlYLCBvdmVybGF5RmFsbGJhY2tYXSA9IFtvcmlnaW5YLCBvcmlnaW5GYWxsYmFja1hdO1xuICAgIGxldCBvZmZzZXRZID0gMDtcblxuICAgIGlmICh0aGlzLnRyaWdnZXJzU3VibWVudSgpKSB7XG4gICAgICAvLyBXaGVuIHRoZSBtZW51IGlzIGEgc3ViLW1lbnUsIGl0IHNob3VsZCBhbHdheXMgYWxpZ24gaXRzZWxmXG4gICAgICAvLyB0byB0aGUgZWRnZXMgb2YgdGhlIHRyaWdnZXIsIGluc3RlYWQgb2Ygb3ZlcmxhcHBpbmcgaXQuXG4gICAgICBvdmVybGF5RmFsbGJhY2tYID0gb3JpZ2luWCA9IHRoaXMubWVudS54UG9zaXRpb24gPT09ICdiZWZvcmUnID8gJ3N0YXJ0JyA6ICdlbmQnO1xuICAgICAgb3JpZ2luRmFsbGJhY2tYID0gb3ZlcmxheVggPSBvcmlnaW5YID09PSAnZW5kJyA/ICdzdGFydCcgOiAnZW5kJztcbiAgICAgIG9mZnNldFkgPSBvdmVybGF5WSA9PT0gJ2JvdHRvbScgPyBNRU5VX1BBTkVMX1RPUF9QQURESU5HIDogLU1FTlVfUEFORUxfVE9QX1BBRERJTkc7XG4gICAgfSBlbHNlIGlmICghdGhpcy5tZW51Lm92ZXJsYXBUcmlnZ2VyKSB7XG4gICAgICBvcmlnaW5ZID0gb3ZlcmxheVkgPT09ICd0b3AnID8gJ2JvdHRvbScgOiAndG9wJztcbiAgICAgIG9yaWdpbkZhbGxiYWNrWSA9IG92ZXJsYXlGYWxsYmFja1kgPT09ICd0b3AnID8gJ2JvdHRvbScgOiAndG9wJztcbiAgICB9XG5cbiAgICBwb3NpdGlvblN0cmF0ZWd5LndpdGhQb3NpdGlvbnMoW1xuICAgICAge29yaWdpblgsIG9yaWdpblksIG92ZXJsYXlYLCBvdmVybGF5WSwgb2Zmc2V0WX0sXG4gICAgICB7b3JpZ2luWDogb3JpZ2luRmFsbGJhY2tYLCBvcmlnaW5ZLCBvdmVybGF5WDogb3ZlcmxheUZhbGxiYWNrWCwgb3ZlcmxheVksIG9mZnNldFl9LFxuICAgICAge1xuICAgICAgICBvcmlnaW5YLFxuICAgICAgICBvcmlnaW5ZOiBvcmlnaW5GYWxsYmFja1ksXG4gICAgICAgIG92ZXJsYXlYLFxuICAgICAgICBvdmVybGF5WTogb3ZlcmxheUZhbGxiYWNrWSxcbiAgICAgICAgb2Zmc2V0WTogLW9mZnNldFlcbiAgICAgIH0sXG4gICAgICB7XG4gICAgICAgIG9yaWdpblg6IG9yaWdpbkZhbGxiYWNrWCxcbiAgICAgICAgb3JpZ2luWTogb3JpZ2luRmFsbGJhY2tZLFxuICAgICAgICBvdmVybGF5WDogb3ZlcmxheUZhbGxiYWNrWCxcbiAgICAgICAgb3ZlcmxheVk6IG92ZXJsYXlGYWxsYmFja1ksXG4gICAgICAgIG9mZnNldFk6IC1vZmZzZXRZXG4gICAgICB9XG4gICAgXSk7XG4gIH1cblxuICAvKiogUmV0dXJucyBhIHN0cmVhbSB0aGF0IGVtaXRzIHdoZW5ldmVyIGFuIGFjdGlvbiB0aGF0IHNob3VsZCBjbG9zZSB0aGUgbWVudSBvY2N1cnMuICovXG4gIHByaXZhdGUgX21lbnVDbG9zaW5nQWN0aW9ucygpIHtcbiAgICBjb25zdCBiYWNrZHJvcCA9IHRoaXMuX292ZXJsYXlSZWYhLmJhY2tkcm9wQ2xpY2soKTtcbiAgICBjb25zdCBkZXRhY2htZW50cyA9IHRoaXMuX292ZXJsYXlSZWYhLmRldGFjaG1lbnRzKCk7XG4gICAgY29uc3QgcGFyZW50Q2xvc2UgPSB0aGlzLl9wYXJlbnRNZW51ID8gdGhpcy5fcGFyZW50TWVudS5jbG9zZWQgOiBvYnNlcnZhYmxlT2YoKTtcbiAgICBjb25zdCBob3ZlciA9IHRoaXMuX3BhcmVudE1lbnUgPyB0aGlzLl9wYXJlbnRNZW51Ll9ob3ZlcmVkKCkucGlwZShcbiAgICAgIGZpbHRlcihhY3RpdmUgPT4gYWN0aXZlICE9PSB0aGlzLl9tZW51SXRlbUluc3RhbmNlKSxcbiAgICAgIGZpbHRlcigoKSA9PiB0aGlzLl9tZW51T3BlbilcbiAgICApIDogb2JzZXJ2YWJsZU9mKCk7XG5cbiAgICByZXR1cm4gbWVyZ2UoYmFja2Ryb3AsIHBhcmVudENsb3NlLCBob3ZlciwgZGV0YWNobWVudHMpO1xuICB9XG5cbiAgLyoqIEhhbmRsZXMgbW91c2UgcHJlc3NlcyBvbiB0aGUgdHJpZ2dlci4gKi9cbiAgX2hhbmRsZU1vdXNlZG93bihldmVudDogTW91c2VFdmVudCk6IHZvaWQge1xuICAgIGlmICghaXNGYWtlTW91c2Vkb3duRnJvbVNjcmVlblJlYWRlcihldmVudCkpIHtcbiAgICAgIC8vIFNpbmNlIHJpZ2h0IG9yIG1pZGRsZSBidXR0b24gY2xpY2tzIHdvbid0IHRyaWdnZXIgdGhlIGBjbGlja2AgZXZlbnQsXG4gICAgICAvLyB3ZSBzaG91bGRuJ3QgY29uc2lkZXIgdGhlIG1lbnUgYXMgb3BlbmVkIGJ5IG1vdXNlIGluIHRob3NlIGNhc2VzLlxuICAgICAgdGhpcy5fb3BlbmVkQnkgPSBldmVudC5idXR0b24gPT09IDAgPyAnbW91c2UnIDogbnVsbDtcblxuICAgICAgLy8gU2luY2UgY2xpY2tpbmcgb24gdGhlIHRyaWdnZXIgd29uJ3QgY2xvc2UgdGhlIG1lbnUgaWYgaXQgb3BlbnMgYSBzdWItbWVudSxcbiAgICAgIC8vIHdlIHNob3VsZCBwcmV2ZW50IGZvY3VzIGZyb20gbW92aW5nIG9udG8gaXQgdmlhIGNsaWNrIHRvIGF2b2lkIHRoZVxuICAgICAgLy8gaGlnaGxpZ2h0IGZyb20gbGluZ2VyaW5nIG9uIHRoZSBtZW51IGl0ZW0uXG4gICAgICBpZiAodGhpcy50cmlnZ2Vyc1N1Ym1lbnUoKSkge1xuICAgICAgICBldmVudC5wcmV2ZW50RGVmYXVsdCgpO1xuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIC8qKiBIYW5kbGVzIGtleSBwcmVzc2VzIG9uIHRoZSB0cmlnZ2VyLiAqL1xuICBfaGFuZGxlS2V5ZG93bihldmVudDogS2V5Ym9hcmRFdmVudCk6IHZvaWQge1xuICAgIGNvbnN0IGtleUNvZGUgPSBldmVudC5rZXlDb2RlO1xuXG4gICAgaWYgKHRoaXMudHJpZ2dlcnNTdWJtZW51KCkgJiYgKFxuICAgICAgICAgICAgKGtleUNvZGUgPT09IFJJR0hUX0FSUk9XICYmIHRoaXMuZGlyID09PSAnbHRyJykgfHxcbiAgICAgICAgICAgIChrZXlDb2RlID09PSBMRUZUX0FSUk9XICYmIHRoaXMuZGlyID09PSAncnRsJykpKSB7XG4gICAgICB0aGlzLm9wZW5NZW51KCk7XG4gICAgfVxuICB9XG5cbiAgLyoqIEhhbmRsZXMgY2xpY2sgZXZlbnRzIG9uIHRoZSB0cmlnZ2VyLiAqL1xuICBfaGFuZGxlQ2xpY2soZXZlbnQ6IE1vdXNlRXZlbnQpOiB2b2lkIHtcbiAgICBpZiAodGhpcy50cmlnZ2Vyc1N1Ym1lbnUoKSkge1xuICAgICAgLy8gU3RvcCBldmVudCBwcm9wYWdhdGlvbiB0byBhdm9pZCBjbG9zaW5nIHRoZSBwYXJlbnQgbWVudS5cbiAgICAgIGV2ZW50LnN0b3BQcm9wYWdhdGlvbigpO1xuICAgICAgdGhpcy5vcGVuTWVudSgpO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLnRvZ2dsZU1lbnUoKTtcbiAgICB9XG4gIH1cblxuICAvKiogSGFuZGxlcyB0aGUgY2FzZXMgd2hlcmUgdGhlIHVzZXIgaG92ZXJzIG92ZXIgdGhlIHRyaWdnZXIuICovXG4gIHByaXZhdGUgX2hhbmRsZUhvdmVyKCkge1xuICAgIC8vIFN1YnNjcmliZSB0byBjaGFuZ2VzIGluIHRoZSBob3ZlcmVkIGl0ZW0gaW4gb3JkZXIgdG8gdG9nZ2xlIHRoZSBwYW5lbC5cbiAgICBpZiAoIXRoaXMudHJpZ2dlcnNTdWJtZW51KCkpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICB0aGlzLl9ob3ZlclN1YnNjcmlwdGlvbiA9IHRoaXMuX3BhcmVudE1lbnUuX2hvdmVyZWQoKVxuICAgICAgLy8gU2luY2Ugd2UgbWlnaHQgaGF2ZSBtdWx0aXBsZSBjb21wZXRpbmcgdHJpZ2dlcnMgZm9yIHRoZSBzYW1lIG1lbnUgKGUuZy4gYSBzdWItbWVudVxuICAgICAgLy8gd2l0aCBkaWZmZXJlbnQgZGF0YSBhbmQgdHJpZ2dlcnMpLCB3ZSBoYXZlIHRvIGRlbGF5IGl0IGJ5IGEgdGljayB0byBlbnN1cmUgdGhhdFxuICAgICAgLy8gaXQgd29uJ3QgYmUgY2xvc2VkIGltbWVkaWF0ZWx5IGFmdGVyIGl0IGlzIG9wZW5lZC5cbiAgICAgIC5waXBlKFxuICAgICAgICBmaWx0ZXIoYWN0aXZlID0+IGFjdGl2ZSA9PT0gdGhpcy5fbWVudUl0ZW1JbnN0YW5jZSAmJiAhYWN0aXZlLmRpc2FibGVkKSxcbiAgICAgICAgZGVsYXkoMCwgYXNhcFNjaGVkdWxlcilcbiAgICAgIClcbiAgICAgIC5zdWJzY3JpYmUoKCkgPT4ge1xuICAgICAgICB0aGlzLl9vcGVuZWRCeSA9ICdtb3VzZSc7XG5cbiAgICAgICAgLy8gSWYgdGhlIHNhbWUgbWVudSBpcyB1c2VkIGJldHdlZW4gbXVsdGlwbGUgdHJpZ2dlcnMsIGl0IG1pZ2h0IHN0aWxsIGJlIGFuaW1hdGluZ1xuICAgICAgICAvLyB3aGlsZSB0aGUgbmV3IHRyaWdnZXIgdHJpZXMgdG8gcmUtb3BlbiBpdC4gV2FpdCBmb3IgdGhlIGFuaW1hdGlvbiB0byBmaW5pc2hcbiAgICAgICAgLy8gYmVmb3JlIGRvaW5nIHNvLiBBbHNvIGludGVycnVwdCBpZiB0aGUgdXNlciBtb3ZlcyB0byBhbm90aGVyIGl0ZW0uXG4gICAgICAgIGlmICh0aGlzLm1lbnUgaW5zdGFuY2VvZiBNYXRNZW51ICYmIHRoaXMubWVudS5faXNBbmltYXRpbmcpIHtcbiAgICAgICAgICAvLyBXZSBuZWVkIHRoZSBgZGVsYXkoMClgIGhlcmUgaW4gb3JkZXIgdG8gYXZvaWRcbiAgICAgICAgICAvLyAnY2hhbmdlZCBhZnRlciBjaGVja2VkJyBlcnJvcnMgaW4gc29tZSBjYXNlcy4gU2VlICMxMjE5NC5cbiAgICAgICAgICB0aGlzLm1lbnUuX2FuaW1hdGlvbkRvbmVcbiAgICAgICAgICAgIC5waXBlKHRha2UoMSksIGRlbGF5KDAsIGFzYXBTY2hlZHVsZXIpLCB0YWtlVW50aWwodGhpcy5fcGFyZW50TWVudS5faG92ZXJlZCgpKSlcbiAgICAgICAgICAgIC5zdWJzY3JpYmUoKCkgPT4gdGhpcy5vcGVuTWVudSgpKTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICB0aGlzLm9wZW5NZW51KCk7XG4gICAgICAgIH1cbiAgICAgIH0pO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHBvcnRhbCB0aGF0IHNob3VsZCBiZSBhdHRhY2hlZCB0byB0aGUgb3ZlcmxheS4gKi9cbiAgcHJpdmF0ZSBfZ2V0UG9ydGFsKCk6IFRlbXBsYXRlUG9ydGFsIHtcbiAgICAvLyBOb3RlIHRoYXQgd2UgY2FuIGF2b2lkIHRoaXMgY2hlY2sgYnkga2VlcGluZyB0aGUgcG9ydGFsIG9uIHRoZSBtZW51IHBhbmVsLlxuICAgIC8vIFdoaWxlIGl0IHdvdWxkIGJlIGNsZWFuZXIsIHdlJ2QgaGF2ZSB0byBpbnRyb2R1Y2UgYW5vdGhlciByZXF1aXJlZCBtZXRob2Qgb25cbiAgICAvLyBgTWF0TWVudVBhbmVsYCwgbWFraW5nIGl0IGhhcmRlciB0byBjb25zdW1lLlxuICAgIGlmICghdGhpcy5fcG9ydGFsIHx8IHRoaXMuX3BvcnRhbC50ZW1wbGF0ZVJlZiAhPT0gdGhpcy5tZW51LnRlbXBsYXRlUmVmKSB7XG4gICAgICB0aGlzLl9wb3J0YWwgPSBuZXcgVGVtcGxhdGVQb3J0YWwodGhpcy5tZW51LnRlbXBsYXRlUmVmLCB0aGlzLl92aWV3Q29udGFpbmVyUmVmKTtcbiAgICB9XG5cbiAgICByZXR1cm4gdGhpcy5fcG9ydGFsO1xuICB9XG5cbn1cbiJdfQ==