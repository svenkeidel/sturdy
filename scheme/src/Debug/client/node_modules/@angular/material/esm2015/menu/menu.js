/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { FocusKeyManager } from '@angular/cdk/a11y';
import { coerceBooleanProperty } from '@angular/cdk/coercion';
import { ESCAPE, LEFT_ARROW, RIGHT_ARROW, DOWN_ARROW, UP_ARROW, HOME, END, hasModifierKey, } from '@angular/cdk/keycodes';
import { ChangeDetectionStrategy, Component, ContentChild, ContentChildren, Directive, ElementRef, EventEmitter, Inject, InjectionToken, Input, NgZone, Output, TemplateRef, QueryList, ViewChild, ViewEncapsulation, } from '@angular/core';
import { merge, Subject, Subscription } from 'rxjs';
import { startWith, switchMap, take } from 'rxjs/operators';
import { matMenuAnimations } from './menu-animations';
import { MatMenuContent } from './menu-content';
import { throwMatMenuInvalidPositionX, throwMatMenuInvalidPositionY } from './menu-errors';
import { MatMenuItem } from './menu-item';
import { MAT_MENU_PANEL } from './menu-panel';
/** Injection token to be used to override the default options for `mat-menu`. */
export const MAT_MENU_DEFAULT_OPTIONS = new InjectionToken('mat-menu-default-options', {
    providedIn: 'root',
    factory: MAT_MENU_DEFAULT_OPTIONS_FACTORY
});
/** @docs-private */
export function MAT_MENU_DEFAULT_OPTIONS_FACTORY() {
    return {
        overlapTrigger: false,
        xPosition: 'after',
        yPosition: 'below',
        backdropClass: 'cdk-overlay-transparent-backdrop',
    };
}
/**
 * Start elevation for the menu panel.
 * @docs-private
 */
const MAT_MENU_BASE_ELEVATION = 4;
let menuPanelUid = 0;
/** Base class with all of the `MatMenu` functionality. */
let _MatMenuBase = /** @class */ (() => {
    class _MatMenuBase {
        constructor(_elementRef, _ngZone, _defaultOptions) {
            this._elementRef = _elementRef;
            this._ngZone = _ngZone;
            this._defaultOptions = _defaultOptions;
            this._xPosition = this._defaultOptions.xPosition;
            this._yPosition = this._defaultOptions.yPosition;
            /** Only the direct descendant menu items. */
            this._directDescendantItems = new QueryList();
            /** Subscription to tab events on the menu panel */
            this._tabSubscription = Subscription.EMPTY;
            /** Config object to be passed into the menu's ngClass */
            this._classList = {};
            /** Current state of the panel animation. */
            this._panelAnimationState = 'void';
            /** Emits whenever an animation on the menu completes. */
            this._animationDone = new Subject();
            /** Class to be added to the backdrop element. */
            this.backdropClass = this._defaultOptions.backdropClass;
            this._overlapTrigger = this._defaultOptions.overlapTrigger;
            this._hasBackdrop = this._defaultOptions.hasBackdrop;
            /** Event emitted when the menu is closed. */
            this.closed = new EventEmitter();
            /**
             * Event emitted when the menu is closed.
             * @deprecated Switch to `closed` instead
             * @breaking-change 8.0.0
             */
            this.close = this.closed;
            this.panelId = `mat-menu-panel-${menuPanelUid++}`;
        }
        /** Position of the menu in the X axis. */
        get xPosition() { return this._xPosition; }
        set xPosition(value) {
            if (value !== 'before' && value !== 'after') {
                throwMatMenuInvalidPositionX();
            }
            this._xPosition = value;
            this.setPositionClasses();
        }
        /** Position of the menu in the Y axis. */
        get yPosition() { return this._yPosition; }
        set yPosition(value) {
            if (value !== 'above' && value !== 'below') {
                throwMatMenuInvalidPositionY();
            }
            this._yPosition = value;
            this.setPositionClasses();
        }
        /** Whether the menu should overlap its trigger. */
        get overlapTrigger() { return this._overlapTrigger; }
        set overlapTrigger(value) {
            this._overlapTrigger = coerceBooleanProperty(value);
        }
        /** Whether the menu has a backdrop. */
        get hasBackdrop() { return this._hasBackdrop; }
        set hasBackdrop(value) {
            this._hasBackdrop = coerceBooleanProperty(value);
        }
        /**
         * This method takes classes set on the host mat-menu element and applies them on the
         * menu template that displays in the overlay container.  Otherwise, it's difficult
         * to style the containing menu from outside the component.
         * @param classes list of class names
         */
        set panelClass(classes) {
            const previousPanelClass = this._previousPanelClass;
            if (previousPanelClass && previousPanelClass.length) {
                previousPanelClass.split(' ').forEach((className) => {
                    this._classList[className] = false;
                });
            }
            this._previousPanelClass = classes;
            if (classes && classes.length) {
                classes.split(' ').forEach((className) => {
                    this._classList[className] = true;
                });
                this._elementRef.nativeElement.className = '';
            }
        }
        /**
         * This method takes classes set on the host mat-menu element and applies them on the
         * menu template that displays in the overlay container.  Otherwise, it's difficult
         * to style the containing menu from outside the component.
         * @deprecated Use `panelClass` instead.
         * @breaking-change 8.0.0
         */
        get classList() { return this.panelClass; }
        set classList(classes) { this.panelClass = classes; }
        ngOnInit() {
            this.setPositionClasses();
        }
        ngAfterContentInit() {
            this._updateDirectDescendants();
            this._keyManager = new FocusKeyManager(this._directDescendantItems).withWrap().withTypeAhead();
            this._tabSubscription = this._keyManager.tabOut.subscribe(() => this.closed.emit('tab'));
            // If a user manually (programatically) focuses a menu item, we need to reflect that focus
            // change back to the key manager. Note that we don't need to unsubscribe here because _focused
            // is internal and we know that it gets completed on destroy.
            this._directDescendantItems.changes.pipe(startWith(this._directDescendantItems), switchMap(items => merge(...items.map((item) => item._focused)))).subscribe(focusedItem => this._keyManager.updateActiveItem(focusedItem));
        }
        ngOnDestroy() {
            this._directDescendantItems.destroy();
            this._tabSubscription.unsubscribe();
            this.closed.complete();
        }
        /** Stream that emits whenever the hovered menu item changes. */
        _hovered() {
            // Coerce the `changes` property because Angular types it as `Observable<any>`
            const itemChanges = this._directDescendantItems.changes;
            return itemChanges.pipe(startWith(this._directDescendantItems), switchMap(items => merge(...items.map((item) => item._hovered))));
        }
        /*
         * Registers a menu item with the menu.
         * @docs-private
         * @deprecated No longer being used. To be removed.
         * @breaking-change 9.0.0
         */
        addItem(_item) { }
        /**
         * Removes an item from the menu.
         * @docs-private
         * @deprecated No longer being used. To be removed.
         * @breaking-change 9.0.0
         */
        removeItem(_item) { }
        /** Handle a keyboard event from the menu, delegating to the appropriate action. */
        _handleKeydown(event) {
            const keyCode = event.keyCode;
            const manager = this._keyManager;
            switch (keyCode) {
                case ESCAPE:
                    if (!hasModifierKey(event)) {
                        event.preventDefault();
                        this.closed.emit('keydown');
                    }
                    break;
                case LEFT_ARROW:
                    if (this.parentMenu && this.direction === 'ltr') {
                        this.closed.emit('keydown');
                    }
                    break;
                case RIGHT_ARROW:
                    if (this.parentMenu && this.direction === 'rtl') {
                        this.closed.emit('keydown');
                    }
                    break;
                case HOME:
                case END:
                    if (!hasModifierKey(event)) {
                        keyCode === HOME ? manager.setFirstItemActive() : manager.setLastItemActive();
                        event.preventDefault();
                    }
                    break;
                default:
                    if (keyCode === UP_ARROW || keyCode === DOWN_ARROW) {
                        manager.setFocusOrigin('keyboard');
                    }
                    manager.onKeydown(event);
            }
        }
        /**
         * Focus the first item in the menu.
         * @param origin Action from which the focus originated. Used to set the correct styling.
         */
        focusFirstItem(origin = 'program') {
            // When the content is rendered lazily, it takes a bit before the items are inside the DOM.
            if (this.lazyContent) {
                this._ngZone.onStable.asObservable()
                    .pipe(take(1))
                    .subscribe(() => this._focusFirstItem(origin));
            }
            else {
                this._focusFirstItem(origin);
            }
        }
        /**
         * Actual implementation that focuses the first item. Needs to be separated
         * out so we don't repeat the same logic in the public `focusFirstItem` method.
         */
        _focusFirstItem(origin) {
            const manager = this._keyManager;
            manager.setFocusOrigin(origin).setFirstItemActive();
            // If there's no active item at this point, it means that all the items are disabled.
            // Move focus to the menu panel so keyboard events like Escape still work. Also this will
            // give _some_ feedback to screen readers.
            if (!manager.activeItem && this._directDescendantItems.length) {
                let element = this._directDescendantItems.first._getHostElement().parentElement;
                // Because the `mat-menu` is at the DOM insertion point, not inside the overlay, we don't
                // have a nice way of getting a hold of the menu panel. We can't use a `ViewChild` either
                // because the panel is inside an `ng-template`. We work around it by starting from one of
                // the items and walking up the DOM.
                while (element) {
                    if (element.getAttribute('role') === 'menu') {
                        element.focus();
                        break;
                    }
                    else {
                        element = element.parentElement;
                    }
                }
            }
        }
        /**
         * Resets the active item in the menu. This is used when the menu is opened, allowing
         * the user to start from the first option when pressing the down arrow.
         */
        resetActiveItem() {
            this._keyManager.setActiveItem(-1);
        }
        /**
         * Sets the menu panel elevation.
         * @param depth Number of parent menus that come before the menu.
         */
        setElevation(depth) {
            // The elevation starts at the base and increases by one for each level.
            // Capped at 24 because that's the maximum elevation defined in the Material design spec.
            const elevation = Math.min(MAT_MENU_BASE_ELEVATION + depth, 24);
            const newElevation = `mat-elevation-z${elevation}`;
            const customElevation = Object.keys(this._classList).find(c => c.startsWith('mat-elevation-z'));
            if (!customElevation || customElevation === this._previousElevation) {
                if (this._previousElevation) {
                    this._classList[this._previousElevation] = false;
                }
                this._classList[newElevation] = true;
                this._previousElevation = newElevation;
            }
        }
        /**
         * Adds classes to the menu panel based on its position. Can be used by
         * consumers to add specific styling based on the position.
         * @param posX Position of the menu along the x axis.
         * @param posY Position of the menu along the y axis.
         * @docs-private
         */
        setPositionClasses(posX = this.xPosition, posY = this.yPosition) {
            const classes = this._classList;
            classes['mat-menu-before'] = posX === 'before';
            classes['mat-menu-after'] = posX === 'after';
            classes['mat-menu-above'] = posY === 'above';
            classes['mat-menu-below'] = posY === 'below';
        }
        /** Starts the enter animation. */
        _startAnimation() {
            // @breaking-change 8.0.0 Combine with _resetAnimation.
            this._panelAnimationState = 'enter';
        }
        /** Resets the panel animation to its initial state. */
        _resetAnimation() {
            // @breaking-change 8.0.0 Combine with _startAnimation.
            this._panelAnimationState = 'void';
        }
        /** Callback that is invoked when the panel animation completes. */
        _onAnimationDone(event) {
            this._animationDone.next(event);
            this._isAnimating = false;
        }
        _onAnimationStart(event) {
            this._isAnimating = true;
            // Scroll the content element to the top as soon as the animation starts. This is necessary,
            // because we move focus to the first item while it's still being animated, which can throw
            // the browser off when it determines the scroll position. Alternatively we can move focus
            // when the animation is done, however moving focus asynchronously will interrupt screen
            // readers which are in the process of reading out the menu already. We take the `element`
            // from the `event` since we can't use a `ViewChild` to access the pane.
            if (event.toState === 'enter' && this._keyManager.activeItemIndex === 0) {
                event.element.scrollTop = 0;
            }
        }
        /**
         * Sets up a stream that will keep track of any newly-added menu items and will update the list
         * of direct descendants. We collect the descendants this way, because `_allItems` can include
         * items that are part of child menus, and using a custom way of registering items is unreliable
         * when it comes to maintaining the item order.
         */
        _updateDirectDescendants() {
            this._allItems.changes
                .pipe(startWith(this._allItems))
                .subscribe((items) => {
                this._directDescendantItems.reset(items.filter(item => item._parentMenu === this));
                this._directDescendantItems.notifyOnChanges();
            });
        }
    }
    _MatMenuBase.decorators = [
        { type: Directive }
    ];
    _MatMenuBase.ctorParameters = () => [
        { type: ElementRef },
        { type: NgZone },
        { type: undefined, decorators: [{ type: Inject, args: [MAT_MENU_DEFAULT_OPTIONS,] }] }
    ];
    _MatMenuBase.propDecorators = {
        _allItems: [{ type: ContentChildren, args: [MatMenuItem, { descendants: true },] }],
        backdropClass: [{ type: Input }],
        ariaLabel: [{ type: Input, args: ['aria-label',] }],
        ariaLabelledby: [{ type: Input, args: ['aria-labelledby',] }],
        ariaDescribedby: [{ type: Input, args: ['aria-describedby',] }],
        xPosition: [{ type: Input }],
        yPosition: [{ type: Input }],
        templateRef: [{ type: ViewChild, args: [TemplateRef,] }],
        items: [{ type: ContentChildren, args: [MatMenuItem, { descendants: false },] }],
        lazyContent: [{ type: ContentChild, args: [MatMenuContent,] }],
        overlapTrigger: [{ type: Input }],
        hasBackdrop: [{ type: Input }],
        panelClass: [{ type: Input, args: ['class',] }],
        classList: [{ type: Input }],
        closed: [{ type: Output }],
        close: [{ type: Output }]
    };
    return _MatMenuBase;
})();
export { _MatMenuBase };
/** @docs-private We show the "_MatMenu" class as "MatMenu" in the docs. */
let MatMenu = /** @class */ (() => {
    class MatMenu extends _MatMenuBase {
    }
    MatMenu.decorators = [
        { type: Directive }
    ];
    return MatMenu;
})();
export { MatMenu };
// Note on the weird inheritance setup: we need three classes, because the MDC-based menu has to
// extend `MatMenu`, however keeping a reference to it will cause the inlined template and styles
// to be retained as well. The MDC menu also has to provide itself as a `MatMenu` in order for
// queries and DI to work correctly, while still not referencing the actual menu class.
// Class responsibility is split up as follows:
// * _MatMenuBase - provides all the functionality without any of the Angular metadata.
// * MatMenu - keeps the same name symbol name as the current menu and
// is used as a provider for DI and query purposes.
// * _MatMenu - the actual menu component implementation with the Angular metadata that should
// be tree shaken away for MDC.
/** @docs-public MatMenu */
let _MatMenu = /** @class */ (() => {
    class _MatMenu extends MatMenu {
        constructor(elementRef, ngZone, defaultOptions) {
            super(elementRef, ngZone, defaultOptions);
        }
    }
    _MatMenu.decorators = [
        { type: Component, args: [{
                    selector: 'mat-menu',
                    template: "<ng-template>\n  <div\n    class=\"mat-menu-panel\"\n    [id]=\"panelId\"\n    [ngClass]=\"_classList\"\n    (keydown)=\"_handleKeydown($event)\"\n    (click)=\"closed.emit('click')\"\n    [@transformMenu]=\"_panelAnimationState\"\n    (@transformMenu.start)=\"_onAnimationStart($event)\"\n    (@transformMenu.done)=\"_onAnimationDone($event)\"\n    tabindex=\"-1\"\n    role=\"menu\"\n    [attr.aria-label]=\"ariaLabel || null\"\n    [attr.aria-labelledby]=\"ariaLabelledby || null\"\n    [attr.aria-describedby]=\"ariaDescribedby || null\">\n    <div class=\"mat-menu-content\">\n      <ng-content></ng-content>\n    </div>\n  </div>\n</ng-template>\n",
                    changeDetection: ChangeDetectionStrategy.OnPush,
                    encapsulation: ViewEncapsulation.None,
                    exportAs: 'matMenu',
                    animations: [
                        matMenuAnimations.transformMenu,
                        matMenuAnimations.fadeInItems
                    ],
                    providers: [
                        { provide: MAT_MENU_PANEL, useExisting: MatMenu },
                        { provide: MatMenu, useExisting: _MatMenu }
                    ],
                    styles: [".mat-menu-panel{min-width:112px;max-width:280px;overflow:auto;-webkit-overflow-scrolling:touch;max-height:calc(100vh - 48px);border-radius:4px;outline:0;min-height:64px}.mat-menu-panel.ng-animating{pointer-events:none}.cdk-high-contrast-active .mat-menu-panel{outline:solid 1px}.mat-menu-content:not(:empty){padding-top:8px;padding-bottom:8px}.mat-menu-item{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;cursor:pointer;outline:none;border:none;-webkit-tap-highlight-color:transparent;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;display:block;line-height:48px;height:48px;padding:0 16px;text-align:left;text-decoration:none;max-width:100%;position:relative}.mat-menu-item::-moz-focus-inner{border:0}.mat-menu-item[disabled]{cursor:default}[dir=rtl] .mat-menu-item{text-align:right}.mat-menu-item .mat-icon{margin-right:16px;vertical-align:middle}.mat-menu-item .mat-icon svg{vertical-align:top}[dir=rtl] .mat-menu-item .mat-icon{margin-left:16px;margin-right:0}.mat-menu-item[disabled]{pointer-events:none}.cdk-high-contrast-active .mat-menu-item.cdk-program-focused,.cdk-high-contrast-active .mat-menu-item.cdk-keyboard-focused,.cdk-high-contrast-active .mat-menu-item-highlighted{outline:dotted 1px}.mat-menu-item-submenu-trigger{padding-right:32px}.mat-menu-item-submenu-trigger::after{width:0;height:0;border-style:solid;border-width:5px 0 5px 5px;border-color:transparent transparent transparent currentColor;content:\"\";display:inline-block;position:absolute;top:50%;right:16px;transform:translateY(-50%)}[dir=rtl] .mat-menu-item-submenu-trigger{padding-right:16px;padding-left:32px}[dir=rtl] .mat-menu-item-submenu-trigger::after{right:auto;left:16px;transform:rotateY(180deg) translateY(-50%)}button.mat-menu-item{width:100%}.mat-menu-item .mat-menu-ripple{top:0;left:0;right:0;bottom:0;position:absolute;pointer-events:none}\n"]
                },] }
    ];
    _MatMenu.ctorParameters = () => [
        { type: ElementRef },
        { type: NgZone },
        { type: undefined, decorators: [{ type: Inject, args: [MAT_MENU_DEFAULT_OPTIONS,] }] }
    ];
    return _MatMenu;
})();
export { _MatMenu };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibWVudS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9tZW51L21lbnUudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLGVBQWUsRUFBYyxNQUFNLG1CQUFtQixDQUFDO0FBRS9ELE9BQU8sRUFBZSxxQkFBcUIsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQzFFLE9BQU8sRUFDTCxNQUFNLEVBQ04sVUFBVSxFQUNWLFdBQVcsRUFDWCxVQUFVLEVBQ1YsUUFBUSxFQUNSLElBQUksRUFDSixHQUFHLEVBQ0gsY0FBYyxHQUNmLE1BQU0sdUJBQXVCLENBQUM7QUFDL0IsT0FBTyxFQUVMLHVCQUF1QixFQUN2QixTQUFTLEVBQ1QsWUFBWSxFQUNaLGVBQWUsRUFDZixTQUFTLEVBQ1QsVUFBVSxFQUNWLFlBQVksRUFDWixNQUFNLEVBQ04sY0FBYyxFQUNkLEtBQUssRUFDTCxNQUFNLEVBRU4sTUFBTSxFQUNOLFdBQVcsRUFDWCxTQUFTLEVBQ1QsU0FBUyxFQUNULGlCQUFpQixHQUVsQixNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUMsS0FBSyxFQUFjLE9BQU8sRUFBRSxZQUFZLEVBQUMsTUFBTSxNQUFNLENBQUM7QUFDOUQsT0FBTyxFQUFDLFNBQVMsRUFBRSxTQUFTLEVBQUUsSUFBSSxFQUFDLE1BQU0sZ0JBQWdCLENBQUM7QUFDMUQsT0FBTyxFQUFDLGlCQUFpQixFQUFDLE1BQU0sbUJBQW1CLENBQUM7QUFDcEQsT0FBTyxFQUFDLGNBQWMsRUFBQyxNQUFNLGdCQUFnQixDQUFDO0FBRTlDLE9BQU8sRUFBQyw0QkFBNEIsRUFBRSw0QkFBNEIsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUN6RixPQUFPLEVBQUMsV0FBVyxFQUFDLE1BQU0sYUFBYSxDQUFDO0FBQ3hDLE9BQU8sRUFBQyxjQUFjLEVBQWUsTUFBTSxjQUFjLENBQUM7QUFxQjFELGlGQUFp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Z2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtGb2N1c0tleU1hbmFnZXIsIEZvY3VzT3JpZ2lufSBmcm9tICdAYW5ndWxhci9jZGsvYTExeSc7XG5pbXBvcnQge0RpcmVjdGlvbn0gZnJvbSAnQGFuZ3VsYXIvY2RrL2JpZGknO1xuaW1wb3J0IHtCb29sZWFuSW5wdXQsIGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eX0gZnJvbSAnQGFuZ3VsYXIvY2RrL2NvZXJjaW9uJztcbmltcG9ydCB7XG4gIEVTQ0FQRSxcbiAgTEVGVF9BUlJPVyxcbiAgUklHSFRfQVJST1csXG4gIERPV05fQVJST1csXG4gIFVQX0FSUk9XLFxuICBIT01FLFxuICBFTkQsXG4gIGhhc01vZGlmaWVyS2V5LFxufSBmcm9tICdAYW5ndWxhci9jZGsva2V5Y29kZXMnO1xuaW1wb3J0IHtcbiAgQWZ0ZXJDb250ZW50SW5pdCxcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksXG4gIENvbXBvbmVudCxcbiAgQ29udGVudENoaWxkLFxuICBDb250ZW50Q2hpbGRyZW4sXG4gIERpcmVjdGl2ZSxcbiAgRWxlbWVudFJlZixcbiAgRXZlbnRFbWl0dGVyLFxuICBJbmplY3QsXG4gIEluamVjdGlvblRva2VuLFxuICBJbnB1dCxcbiAgTmdab25lLFxuICBPbkRlc3Ryb3ksXG4gIE91dHB1dCxcbiAgVGVtcGxhdGVSZWYsXG4gIFF1ZXJ5TGlzdCxcbiAgVmlld0NoaWxkLFxuICBWaWV3RW5jYXBzdWxhdGlvbixcbiAgT25Jbml0LFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7bWVyZ2UsIE9ic2VydmFibGUsIFN1YmplY3QsIFN1YnNjcmlwdGlvbn0gZnJvbSAncnhqcyc7XG5pbXBvcnQge3N0YXJ0V2l0aCwgc3dpdGNoTWFwLCB0YWtlfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5pbXBvcnQge21hdE1lbnVBbmltYXRpb25zfSBmcm9tICcuL21lbnUtYW5pbWF0aW9ucyc7XG5pbXBvcnQge01hdE1lbnVDb250ZW50fSBmcm9tICcuL21lbnUtY29udGVudCc7XG5pbXBvcnQge01lbnVQb3NpdGlvblgsIE1lbnVQb3NpdGlvbll9IGZyb20gJy4vbWVudS1wb3NpdGlvbnMnO1xuaW1wb3J0IHt0aHJvd01hdE1lbnVJbnZhbGlkUG9zaXRpb25YLCB0aHJvd01hdE1lbnVJbnZhbGlkUG9zaXRpb25ZfSBmcm9tICcuL21lbnUtZXJyb3JzJztcbmltcG9ydCB7TWF0TWVudUl0ZW19IGZyb20gJy4vbWVudS1pdGVtJztcbmltcG9ydCB7TUFUX01FTlVfUEFORUwsIE1hdE1lbnVQYW5lbH0gZnJvbSAnLi9tZW51LXBhbmVsJztcbmltcG9ydCB7QW5pbWF0aW9uRXZlbnR9IGZyb20gJ0Bhbmd1bGFyL2FuaW1hdGlvbnMnO1xuXG4vKiogRGVmYXVsdCBgbWF0LW1lbnVgIG9wdGlvbnMgdGhhdCBjYW4gYmUgb3ZlcnJpZGRlbi4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgTWF0TWVudURlZmF1bHRPcHRpb25zIHtcbiAgLyoqIFRoZSB4LWF4aXMgcG9zaXRpb24gb2YgdGhlIG1lbnUuICovXG4gIHhQb3NpdGlvbjogTWVudVBvc2l0aW9uWDtcblxuICAvKiogVGhlIHktYXhpcyBwb3NpdGlvbiBvZiB0aGUgbWVudS4gKi9cbiAgeVBvc2l0aW9uOiBNZW51UG9zaXRpb25ZO1xuXG4gIC8qKiBXaGV0aGVyIHRoZSBtZW51IHNob3VsZCBvdmVybGFwIHRoZSBtZW51IHRyaWdnZXIuICovXG4gIG92ZXJsYXBUcmlnZ2VyOiBib29sZWFuO1xuXG4gIC8qKiBDbGFzcyB0byBiZSBhcHBsaWVkIHRvIHRoZSBtZW51J3MgYmFja2Ryb3AuICovXG4gIGJhY2tkcm9wQ2xhc3M6IHN0cmluZztcblxuICAvKiogV2hldGhlciB0aGUgbWVudSBoYXMgYSBiYWNrZHJvcC4gKi9cbiAgaGFzQmFja2Ryb3A/OiBib29sZWFuO1xufVxuXG4vKiogSW5qZWN0aW9uIHRva2VuIHRvIGJlIHVzZWQgdG8gb3ZlcnJpZGUgdGhlIGRlZmF1bHQgb3B0aW9ucyBmb3IgYG1hdC1tZW51YC4gKi9cbmV4cG9ydCBjb25zdCBNQVRfTUVOVV9ERUZBVUxUX09QVElPTlMgPVxuICAgIG5ldyBJbmplY3Rpb25Ub2tlbjxNYXRNZW51RGVmYXVsdE9wdGlvbnM+KCdtYXQtbWVudS1kZWZhdWx0LW9wdGlvbnMnLCB7XG4gICAgICBwcm92aWRlZEluOiAncm9vdCcsXG4gICAgICBmYWN0b3J5OiBNQVRfTUVOVV9ERUZBVUxUX09QVElPTlNfRkFDVE9SWVxuICAgIH0pO1xuXG4vKiogQGRvY3MtcHJpdmF0ZSAqL1xuZXhwb3J0IGZ1bmN0aW9uIE1BVF9NRU5VX0RFRkFVTFRfT1BUSU9OU19GQUNUT1JZKCk6IE1hdE1lbnVEZWZhdWx0T3B0aW9ucyB7XG4gIHJldHVybiB7XG4gICAgb3ZlcmxhcFRyaWdnZXI6IGZhbHNlLFxuICAgIHhQb3NpdGlvbjogJ2FmdGVyJyxcbiAgICB5UG9zaXRpb246ICdiZWxvdycsXG4gICAgYmFja2Ryb3BDbGFzczogJ2Nkay1vdmVybGF5LXRyYW5zcGFyZW50LWJhY2tkcm9wJyxcbiAgfTtcbn1cbi8qKlxuICogU3RhcnQgZWxldmF0aW9uIGZvciB0aGUgbWVudSBwYW5lbC5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuY29uc3QgTUFUX01FTlVfQkFTRV9FTEVWQVRJT04gPSA0O1xuXG5sZXQgbWVudVBhbmVsVWlkID0gMDtcblxuLyoqIEJhc2UgY2xhc3Mgd2l0aCBhbGwgb2YgdGhlIGBNYXRNZW51YCBmdW5jdGlvbmFsaXR5LiAqL1xuQERpcmVjdGl2ZSgpXG4vLyB0c2xpbnQ6ZGlzYWJsZS1uZXh0LWxpbmU6Y2xhc3MtbmFtZVxuZXhwb3J0IGNsYXNzIF9NYXRNZW51QmFzZSBpbXBsZW1lbnRzIEFmdGVyQ29udGVudEluaXQsIE1hdE1lbnVQYW5lbDxNYXRNZW51SXRlbT4sIE9uSW5pdCxcbiAgT25EZXN0cm95IHtcbiAgcHJpdmF0ZSBfa2V5TWFuYWdlcjogRm9jdXNLZXlNYW5hZ2VyPE1hdE1lbnVJdGVtPjtcbiAgcHJpdmF0ZSBfeFBvc2l0aW9uOiBNZW51UG9zaXRpb25YID0gdGhpcy5fZGVmYXVsdE9wdGlvbnMueFBvc2l0aW9uO1xuICBwcml2YXRlIF95UG9zaXRpb246IE1lbnVQb3NpdGlvblkgPSB0aGlzLl9kZWZhdWx0T3B0aW9ucy55UG9zaXRpb247XG4gIHByaXZhdGUgX3ByZXZpb3VzRWxldmF0aW9uOiBzdHJpbmc7XG5cbiAgLyoqIEFsbCBpdGVtcyBpbnNpZGUgdGhlIG1lbnUuIEluY2x1ZGVzIGl0ZW1zIG5lc3RlZCBpbnNpZGUgYW5vdGhlciBtZW51LiAqL1xuICBAQ29udGVudENoaWxkcmVuKE1hdE1lbnVJdGVtLCB7ZGVzY2VuZGFudHM6IHRydWV9KSBfYWxsSXRlbXM6IFF1ZXJ5TGlzdDxNYXRNZW51SXRlbT47XG5cbiAgLyoqIE9ubHkgdGhlIGRpcmVjdCBkZXNjZW5kYW50IG1lbnUgaXRlbXMuICovXG4gIHByaXZhdGUgX2RpcmVjdERlc2NlbmRhbnRJdGVtcyA9IG5ldyBRdWVyeUxpc3Q8TWF0TWVudUl0ZW0+KCk7XG5cbiAgLyoqIFN1YnNjcmlwdGlvbiB0byB0YWIgZXZlbnRzIG9uIHRoZSBtZW51IHBhbmVsICovXG4gIHByaXZhdGUgX3RhYlN1YnNjcmlwdGlvbiA9IFN1YnNjcmlwdGlvbi5FTVBUWTtcblxuICAvKiogQ29uZmlnIG9iamVjdCB0byBiZSBwYXNzZWQgaW50byB0aGUgbWVudSdzIG5nQ2xhc3MgKi9cbiAgX2NsYXNzTGlzdDoge1trZXk6IHN0cmluZ106IGJvb2xlYW59ID0ge307XG5cbiAgLyoqIEN1cnJlbnQgc3RhdGUgb2YgdGhlIHBhbmVsIGFuaW1hdGlvbi4gKi9cbiAgX3BhbmVsQW5pbWF0aW9uU3RhdGU6ICd2b2lkJyB8ICdlbnRlcicgPSAndm9pZCc7XG5cbiAgLyoqIEVtaXRzIHdoZW5ldmVyIGFuIGFuaW1hdGlvbiBvbiB0aGUgbWVudSBjb21wbGV0ZXMuICovXG4gIF9hbmltYXRpb25Eb25lID0gbmV3IFN1YmplY3Q8QW5pbWF0aW9uRXZlbnQ+KCk7XG5cbiAgLyoqIFdoZXRoZXIgdGhlIG1lbnUgaXMgYW5pbWF0aW5nLiAqL1xuICBfaXNBbmltYXRpbmc6IGJvb2xlYW47XG5cbiAgLyoqIFBhcmVudCBtZW51IG9mIHRoZSBjdXJyZW50IG1lbnUgcGFuZWwuICovXG4gIHBhcmVudE1lbnU6IE1hdE1lbnVQYW5lbCB8IHVuZGVmaW5lZDtcblxuICAvKiogTGF5b3V0IGRpcmVjdGlvbiBvZiB0aGUgbWVudS4gKi9cbiAgZGlyZWN0aW9uOiBEaXJlY3Rpb247XG5cbiAgLyoqIENsYXNzIHRvIGJlIGFkZGVkIHRvIHRoZSBiYWNrZHJvcCBlbGVtZW50LiAqL1xuICBASW5wdXQoKSBiYWNrZHJvcENsYXNzOiBzdHJpbmcgPSB0aGlzLl9kZWZhdWx0T3B0aW9ucy5iYWNrZHJvcENsYXNzO1xuXG4gIC8qKiBhcmlhLWxhYmVsIGZvciB0aGUgbWVudSBwYW5lbC4gKi9cbiAgQElucHV0KCdhcmlhLWxhYmVsJykgYXJpYUxhYmVsOiBzdHJpbmc7XG5cbiAgLyoqIGFyaWEtbGFiZWxsZWRieSBmb3IgdGhlIG1lbnUgcGFuZWwuICovXG4gIEBJbnB1dCgnYXJpYS1sYWJlbGxlZGJ5JykgYXJpYUxhYmVsbGVkYnk6IHN0cmluZztcblxuICAvKiogYXJpYS1kZXNjcmliZWRieSBmb3IgdGhlIG1lbnUgcGFuZWwuICovXG4gIEBJbnB1dCgnYXJpYS1kZXNjcmliZWRieScpIGFyaWFEZXNjcmliZWRieTogc3RyaW5nO1xuXG4gIC8qKiBQb3NpdGlvbiBvZiB0aGUgbWVudSBpbiB0aGUgWCBheGlzLiAqL1xuICBASW5wdXQoKVxuICBnZXQgeFBvc2l0aW9uKCk6IE1lbnVQb3NpdGlvblggeyByZXR1cm4gdGhpcy5feFBvc2l0aW9uOyB9XG4gIHNldCB4UG9zaXRpb24odmFsdWU6IE1lbnVQb3NpdGlvblgpIHtcbiAgICBpZiAodmFsdWUgIT09ICdiZWZvcmUnICYmIHZhbHVlICE9PSAnYWZ0ZXInKSB7XG4gICAgICB0aHJvd01hdE1lbnVJbnZhbGlkUG9zaXRpb25YKCk7XG4gICAgfVxuICAgIHRoaXMuX3hQb3NpdGlvbiA9IHZhbHVlO1xuICAgIHRoaXMuc2V0UG9zaXRpb25DbGFzc2VzKCk7XG4gIH1cblxuICAvKiogUG9zaXRpb24gb2YgdGhlIG1lbnUgaW4gdGhlIFkgYXhpcy4gKi9cbiAgQElucHV0KClcbiAgZ2V0IHlQb3NpdGlvbigpOiBNZW51UG9zaXRpb25ZIHsgcmV0dXJuIHRoaXMuX3lQb3NpdGlvbjsgfVxuICBzZXQgeVBvc2l0aW9uKHZhbHVlOiBNZW51UG9zaXRpb25ZKSB7XG4gICAgaWYgKHZhbHVlICE9PSAnYWJvdmUnICYmIHZhbHVlICE9PSAnYmVsb3cnKSB7XG4gICAgICB0aHJvd01hdE1lbnVJbnZhbGlkUG9zaXRpb25ZKCk7XG4gICAgfVxuICAgIHRoaXMuX3lQb3NpdGlvbiA9IHZhbHVlO1xuICAgIHRoaXMuc2V0UG9zaXRpb25DbGFzc2VzKCk7XG4gIH1cblxuICAvKiogQGRvY3MtcHJpdmF0ZSAqL1xuICBAVmlld0NoaWxkKFRlbXBsYXRlUmVmKSB0ZW1wbGF0ZVJlZjogVGVtcGxhdGVSZWY8YW55PjtcblxuICAvKipcbiAgICogTGlzdCBvZiB0aGUgaXRlbXMgaW5zaWRlIG9mIGEgbWVudS5cbiAgICogQGRlcHJlY2F0ZWRcbiAgICogQGJyZWFraW5nLWNoYW5nZSA4LjAuMFxuICAgKi9cbiAgQENvbnRlbnRDaGlsZHJlbihNYXRNZW51SXRlbSwge2Rlc2NlbmRhbnRzOiBmYWxzZX0pIGl0ZW1zOiBRdWVyeUxpc3Q8TWF0TWVudUl0ZW0+O1xuXG4gIC8qKlxuICAgKiBNZW51IGNvbnRlbnQgdGhhdCB3aWxsIGJlIHJlbmRlcmVkIGxhemlseS5cbiAgICogQGRvY3MtcHJpdmF0ZVxuICAgKi9cbiAgQENvbnRlbnRDaGlsZChNYXRNZW51Q29udGVudCkgbGF6eUNvbnRlbnQ6IE1hdE1lbnVDb250ZW50O1xuXG4gIC8qKiBXaGV0aGVyIHRoZSBtZW51IHNob3VsZCBvdmVybGFwIGl0cyB0cmlnZ2VyLiAqL1xuICBASW5wdXQoKVxuICBnZXQgb3ZlcmxhcFRyaWdnZXIoKTogYm9vbGVhbiB7IHJldHVybiB0aGlzLl9vdmVybGFwVHJpZ2dlcjsgfVxuICBzZXQgb3ZlcmxhcFRyaWdnZXIodmFsdWU6IGJvb2xlYW4pIHtcbiAgICB0aGlzLl9vdmVybGFwVHJpZ2dlciA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh2YWx1ZSk7XG4gIH1cbiAgcHJpdmF0ZSBfb3ZlcmxhcFRyaWdnZXI6IGJvb2xlYW4gPSB0aGlzLl9kZWZhdWx0T3B0aW9ucy5vdmVybGFwVHJpZ2dlcjtcblxuICAvKiogV2hldGhlciB0aGUgbWVudSBoYXMgYSBiYWNrZHJvcC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IGhhc0JhY2tkcm9wKCk6IGJvb2xlYW4gfCB1bmRlZmluZWQgeyByZXR1cm4gdGhpcy5faGFzQmFja2Ryb3A7IH1cbiAgc2V0IGhhc0JhY2tkcm9wKHZhbHVlOiBib29sZWFuIHwgdW5kZWZpbmVkKSB7XG4gICAgdGhpcy5faGFzQmFja2Ryb3AgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkodmFsdWUpO1xuICB9XG4gIHByaXZhdGUgX2hhc0JhY2tkcm9wOiBib29sZWFuIHwgdW5kZWZpbmVkID0gdGhpcy5fZGVmYXVsdE9wdGlvbnMuaGFzQmFja2Ryb3A7XG5cbiAgLyoqXG4gICAqIFRoaXMgbWV0aG9kIHRha2VzIGNsYXNzZXMgc2V0IG9uIHRoZSBob3N0IG1hdC1tZW51IGVsZW1lbnQgYW5kIGFwcGxpZXMgdGhlbSBvbiB0aGVcbiAgICogbWVudSB0ZW1wbGF0ZSB0aGF0IGRpc3BsYXlzIGluIHRoZSBvdmVybGF5IGNvbnRhaW5lci4gIE90aGVyd2lzZSwgaXQncyBkaWZmaWN1bHRcbiAgICogdG8gc3R5bGUgdGhlIGNvbnRhaW5pbmcgbWVudSBmcm9tIG91dHNpZGUgdGhlIGNvbXBvbmVudC5cbiAgICogQHBhcmFtIGNsYXNzZXMgbGlzdCBvZiBjbGFzcyBuYW1lc1xuICAgKi9cbiAgQElucHV0KCdjbGFzcycpXG4gIHNldCBwYW5lbENsYXNzKGNsYXNzZXM6IHN0cmluZykge1xuICAgIGNvbnN0IHByZXZpb3VzUGFuZWxDbGFzcyA9IHRoaXMuX3ByZXZpb3VzUGFuZWxDbGFzcztcblxuICAgIGlmIChwcmV2aW91c1BhbmVsQ2xhc3MgJiYgcHJldmlvdXNQYW5lbENsYXNzLmxlbmd0aCkge1xuICAgICAgcHJldmlvdXNQYW5lbENsYXNzLnNwbGl0KCcgJykuZm9yRWFjaCgoY2xhc3NOYW1lOiBzdHJpbmcpID0+IHtcbiAgICAgICAgdGhpcy5fY2xhc3NMaXN0W2NsYXNzTmFtZV0gPSBmYWxzZTtcbiAgICAgIH0pO1xuICAgIH1cblxuICAgIHRoaXMuX3ByZXZpb3VzUGFuZWxDbGFzcyA9IGNsYXNzZXM7XG5cbiAgICBpZiAoY2xhc3NlcyAmJiBjbGFzc2VzLmxlbmd0aCkge1xuICAgICAgY2xhc3Nlcy5zcGxpdCgnICcpLmZvckVhY2goKGNsYXNzTmFtZTogc3RyaW5nKSA9PiB7XG4gICAgICAgIHRoaXMuX2NsYXNzTGlzdFtjbGFzc05hbWVdID0gdHJ1ZTtcbiAgICAgIH0pO1xuXG4gICAgICB0aGlzLl9lbGVtZW50UmVmLm5hdGl2ZUVsZW1lbnQuY2xhc3NOYW1lID0gJyc7XG4gICAgfVxuICB9XG4gIHByaXZhdGUgX3ByZXZpb3VzUGFuZWxDbGFzczogc3RyaW5nO1xuXG4gIC8qKlxuICAgKiBUaGlzIG1ldGhvZCB0YWtlcyBjbGFzc2VzIHNldCBvbiB0aGUgaG9zdCBtYXQtbWVudSBlbGVtZW50IGFuZCBhcHBsaWVzIHRoZW0gb24gdGhlXG4gICAqIG1lbnUgdGVtcGxhdGUgdGhhdCBkaXNwbGF5cyBpbiB0aGUgb3ZlcmxheSBjb250YWluZXIuICBPdGhlcndpc2UsIGl0J3MgZGlmZmljdWx0XG4gICAqIHRvIHN0eWxlIHRoZSBjb250YWluaW5nIG1lbnUgZnJvbSBvdXRzaWRlIHRoZSBjb21wb25lbnQuXG4gICAqIEBkZXByZWNhdGVkIFVzZSBgcGFuZWxDbGFzc2AgaW5zdGVhZC5cbiAgICogQGJyZWFraW5nLWNoYW5nZSA4LjAuMFxuICAgKi9cbiAgQElucHV0KClcbiAgZ2V0IGNsYXNzTGlzdCgpOiBzdHJpbmcgeyByZXR1cm4gdGhpcy5wYW5lbENsYXNzOyB9XG4gIHNldCBjbGFzc0xpc3QoY2xhc3Nlczogc3RyaW5nKSB7IHRoaXMucGFuZWxDbGFzcyA9IGNsYXNzZXM7IH1cblxuICAvKiogRXZlbnQgZW1pdHRlZCB3aGVuIHRoZSBtZW51IGlzIGNsb3NlZC4gKi9cbiAgQE91dHB1dCgpIHJlYWRvbmx5IGNsb3NlZDogRXZlbnRFbWl0dGVyPHZvaWQgfCAnY2xpY2snIHwgJ2tleWRvd24nIHwgJ3RhYic+ID1cbiAgICAgIG5ldyBFdmVudEVtaXR0ZXI8dm9pZCB8ICdjbGljaycgfCAna2V5ZG93bicgfCAndGFiJz4oKTtcblxuICAvKipcbiAgICogRXZlbnQgZW1pdHRlZCB3aGVuIHRoZSBtZW51IGlzIGNsb3NlZC5cbiAgICogQGRlcHJlY2F0ZWQgU3dpdGNoIHRvIGBjbG9zZWRgIGluc3RlYWRcbiAgICogQGJyZWFraW5nLWNoYW5nZSA4LjAuMFxuICAgKi9cbiAgQE91dHB1dCgpIGNsb3NlOiBFdmVudEVtaXR0ZXI8dm9pZCB8ICdjbGljaycgfCAna2V5ZG93bicgfCAndGFiJz4gPSB0aGlzLmNsb3NlZDtcblxuICByZWFkb25seSBwYW5lbElkID0gYG1hdC1tZW51LXBhbmVsLSR7bWVudVBhbmVsVWlkKyt9YDtcblxuICBjb25zdHJ1Y3RvcihcbiAgICBwcml2YXRlIF9lbGVtZW50UmVmOiBFbGVtZW50UmVmPEhUTUxFbGVtZW50PixcbiAgICBwcml2YXRlIF9uZ1pvbmU6IE5nWm9uZSxcbiAgICBASW5qZWN0KE1BVF9NRU5VX0RFRkFVTFRfT1BUSU9OUykgcHJpdmF0ZSBfZGVmYXVsdE9wdGlvbnM6IE1hdE1lbnVEZWZhdWx0T3B0aW9ucykgeyB9XG5cbiAgbmdPbkluaXQoKSB7XG4gICAgdGhpcy5zZXRQb3NpdGlvbkNsYXNzZXMoKTtcbiAgfVxuXG4gIG5nQWZ0ZXJDb250ZW50SW5pdCgpIHtcbiAgICB0aGlzLl91cGRhdGVEaXJlY3REZXNjZW5kYW50cygpO1xuICAgIHRoaXMuX2tleU1hbmFnZXIgPSBuZXcgRm9jdXNLZXlNYW5hZ2VyKHRoaXMuX2RpcmVjdERlc2NlbmRhbnRJdGVtcykud2l0aFdyYXAoKS53aXRoVHlwZUFoZWFkKCk7XG4gICAgdGhpcy5fdGFiU3Vic2NyaXB0aW9uID0gdGhpcy5fa2V5TWFuYWdlci50YWJPdXQuc3Vic2NyaWJlKCgpID0+IHRoaXMuY2xvc2VkLmVtaXQoJ3RhYicpKTtcblxuICAgIC8vIElmIGEgdXNlciBtYW51YWxseSAocHJvZ3JhbWF0aWNhbGx5KSBmb2N1c2VzIGEgbWVudSBpdGVtLCB3ZSBuZWVkIHRvIHJlZmxlY3QgdGhhdCBmb2N1c1xuICAgIC8vIGNoYW5nZSBiYWNrIHRvIHRoZSBrZXkgbWFuYWdlci4gTm90ZSB0aGF0IHdlIGRvbid0IG5lZWQgdG8gdW5zdWJzY3JpYmUgaGVyZSBiZWNhdXNlIF9mb2N1c2VkXG4gICAgLy8gaXMgaW50ZXJuYWwgYW5kIHdlIGtub3cgdGhhdCBpdCBnZXRzIGNvbXBsZXRlZCBvbiBkZXN0cm95LlxuICAgIHRoaXMuX2RpcmVjdERlc2NlbmRhbnRJdGVtcy5jaGFuZ2VzLnBpcGUoXG4gICAgICBzdGFydFdpdGgodGhpcy5fZGlyZWN0RGVzY2VuZGFudEl0ZW1zKSxcbiAgICAgIHN3aXRjaE1hcChpdGVtcyA9PiBtZXJnZTxNYXRNZW51SXRlbT4oLi4uaXRlbXMubWFwKChpdGVtOiBNYXRNZW51SXRlbSkgPT4gaXRlbS5fZm9jdXNlZCkpKVxuICAgICkuc3Vic2NyaWJlKGZvY3VzZWRJdGVtID0+IHRoaXMuX2tleU1hbmFnZXIudXBkYXRlQWN0aXZlSXRlbShmb2N1c2VkSXRlbSkpO1xuICB9XG5cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgdGhpcy5fZGlyZWN0RGVzY2VuZGFudEl0ZW1zLmRlc3Ryb3koKTtcbiAgICB0aGlzLl90YWJTdWJzY3JpcHRpb24udW5zdWJzY3JpYmUoKTtcbiAgICB0aGlzLmNsb3NlZC5jb21wbGV0ZSgpO1xuICB9XG5cbiAgLyoqIFN0cmVhbSB0aGF0IGVtaXRzIHdoZW5ldmVyIHRoZSBob3ZlcmVkIG1lbnUgaXRlbSBjaGFuZ2VzLiAqL1xuICBfaG92ZXJlZCgpOiBPYnNlcnZhYmxlPE1hdE1lbnVJdGVtPiB7XG4gICAgLy8gQ29lcmNlIHRoZSBgY2hhbmdlc2AgcHJvcGVydHkgYmVjYXVzZSBBbmd1bGFyIHR5cGVzIGl0IGFzIGBPYnNlcnZhYmxlPGFueT5gXG4gICAgY29uc3QgaXRlbUNoYW5nZXMgPSB0aGlzLl9kaXJlY3REZXNjZW5kYW50SXRlbXMuY2hhbmdlcyBhcyBPYnNlcnZhYmxlPFF1ZXJ5TGlzdDxNYXRNZW51SXRlbT4+O1xuICAgIHJldHVybiBpdGVtQ2hhbmdlcy5waXBlKFxuICAgICAgc3RhcnRXaXRoKHRoaXMuX2RpcmVjdERlc2NlbmRhbnRJdGVtcyksXG4gICAgICBzd2l0Y2hNYXAoaXRlbXMgPT4gbWVyZ2UoLi4uaXRlbXMubWFwKChpdGVtOiBNYXRNZW51SXRlbSkgPT4gaXRlbS5faG92ZXJlZCkpKVxuICAgICkgYXMgT2JzZXJ2YWJsZTxNYXRNZW51SXRlbT47XG4gIH1cblxuICAvKlxuICAgKiBSZWdpc3RlcnMgYSBtZW51IGl0ZW0gd2l0aCB0aGUgbWVudS5cbiAgICogQGRvY3MtcHJpdmF0ZVxuICAgKiBAZGVwcmVjYXRlZCBObyBsb25nZXIgYmVpbmcgdXNlZC4gVG8gYmUgcmVtb3ZlZC5cbiAgICogQGJyZWFraW5nLWNoYW5nZSA5LjAuMFxuICAgKi9cbiAgYWRkSXRlbShfaXRlbTogTWF0TWVudUl0ZW0pIHt9XG5cbiAgLyoqXG4gICAqIFJlbW92ZXMgYW4gaXRlbSBmcm9tIHRoZSBtZW51LlxuICAgKiBAZG9jcy1wcml2YXRlXG4gICAqIEBkZXByZWNhdGVkIE5vIGxvbmdlciBiZWluZyB1c2VkLiBUbyBiZSByZW1vdmVkLlxuICAgKiBAYnJlYWtpbmctY2hhbmdlIDkuMC4wXG4gICAqL1xuICByZW1vdmVJdGVtKF9pdGVtOiBNYXRNZW51SXRlbSkge31cblxuICAvKiogSGFuZGxlIGEga2V5Ym9hcmQgZXZlbnQgZnJvbSB0aGUgbWVudSwgZGVsZWdhdGluZyB0byB0aGUgYXBwcm9wcmlhdGUgYWN0aW9uLiAqL1xuICBfaGFuZGxlS2V5ZG93bihldmVudDogS2V5Ym9hcmRFdmVudCkge1xuICAgIGNvbnN0IGtleUNvZGUgPSBldmVudC5rZXlDb2RlO1xuICAgIGNvbnN0IG1hbmFnZXIgPSB0aGlzLl9rZXlNYW5hZ2VyO1xuXG4gICAgc3dpdGNoIChrZXlDb2RlKSB7XG4gICAgICBjYXNlIEVTQ0FQRTpcbiAgICAgICAgaWYgKCFoYXNNb2RpZmllcktleShldmVudCkpIHtcbiAgICAgICAgICBldmVudC5wcmV2ZW50RGVmYXVsdCgpO1xuICAgICAgICAgIHRoaXMuY2xvc2VkLmVtaXQoJ2tleWRvd24nKTtcbiAgICAgICAgfVxuICAgICAgYnJlYWs7XG4gICAgICBjYXNlIExFRlRfQVJST1c6XG4gICAgICAgIGlmICh0aGlzLnBhcmVudE1lbnUgJiYgdGhpcy5kaXJlY3Rpb24gPT09ICdsdHInKSB7XG4gICAgICAgICAgdGhpcy5jbG9zZWQuZW1pdCgna2V5ZG93bicpO1xuICAgICAgICB9XG4gICAgICBicmVhaztcbiAgICAgIGNhc2UgUklHSFRfQVJST1c6XG4gICAgICAgIGlmICh0aGlzLnBhcmVudE1lbnUgJiYgdGhpcy5kaXJlY3Rpb24gPT09ICdydGwnKSB7XG4gICAgICAgICAgdGhpcy5jbG9zZWQuZW1pdCgna2V5ZG93bicpO1xuICAgICAgICB9XG4gICAgICBicmVhaztcbiAgICAgIGNhc2UgSE9NRTpcbiAgICAgIGNhc2UgRU5EOlxuICAgICAgICBpZiAoIWhhc01vZGlmaWVyS2V5KGV2ZW50KSkge1xuICAgICAgICAgIGtleUNvZGUgPT09IEhPTUUgPyBtYW5hZ2VyLnNldEZpcnN0SXRlbUFjdGl2ZSgpIDogbWFuYWdlci5zZXRMYXN0SXRlbUFjdGl2ZSgpO1xuICAgICAgICAgIGV2ZW50LnByZXZlbnREZWZhdWx0KCk7XG4gICAgICAgIH1cbiAgICAgIGJyZWFrO1xuICAgICAgZGVmYXVsdDpcbiAgICAgICAgaWYgKGtleUNvZGUgPT09IFVQX0FSUk9XIHx8IGtleUNvZGUgPT09IERPV05fQVJST1cpIHtcbiAgICAgICAgICBtYW5hZ2VyLnNldEZvY3VzT3JpZ2luKCdrZXlib2FyZCcpO1xuICAgICAgICB9XG5cbiAgICAgICAgbWFuYWdlci5vbktleWRvd24oZXZlbnQpO1xuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBGb2N1cyB0aGUgZmlyc3QgaXRlbSBpbiB0aGUgbWVudS5cbiAgICogQHBhcmFtIG9yaWdpbiBBY3Rpb24gZnJvbSB3aGljaCB0aGUgZm9jdXMgb3JpZ2luYXRlZC4gVXNlZCB0byBzZXQgdGhlIGNvcnJlY3Qgc3R5bGluZy5cbiAgICovXG4gIGZvY3VzRmlyc3RJdGVtKG9yaWdpbjogRm9jdXNPcmlnaW4gPSAncHJvZ3JhbScpOiB2b2lkIHtcbiAgICAvLyBXaGVuIHRoZSBjb250ZW50IGlzIHJlbmRlcmVkIGxhemlseSwgaXQgdGFrZXMgYSBiaXQgYmVmb3JlIHRoZSBpdGVtcyBhcmUgaW5zaWRlIHRoZSBET00uXG4gICAgaWYgKHRoaXMubGF6eUNvbnRlbnQpIHtcbiAgICAgIHRoaXMuX25nWm9uZS5vblN0YWJsZS5hc09ic2VydmFibGUoKVxuICAgICAgICAucGlwZSh0YWtlKDEpKVxuICAgICAgICAuc3Vic2NyaWJlKCgpID0+IHRoaXMuX2ZvY3VzRmlyc3RJdGVtKG9yaWdpbikpO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLl9mb2N1c0ZpcnN0SXRlbShvcmlnaW4pO1xuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBBY3R1YWwgaW1wbGVtZW50YXRpb24gdGhhdCBmb2N1c2VzIHRoZSBmaXJzdCBpdGVtLiBOZWVkcyB0byBiZSBzZXBhcmF0ZWRcbiAgICogb3V0IHNvIHdlIGRvbid0IHJlcGVhdCB0aGUgc2FtZSBsb2dpYyBpbiB0aGUgcHVibGljIGBmb2N1c0ZpcnN0SXRlbWAgbWV0aG9kLlxuICAgKi9cbiAgcHJpdmF0ZSBfZm9jdXNGaXJzdEl0ZW0ob3JpZ2luOiBGb2N1c09yaWdpbikge1xuICAgIGNvbnN0IG1hbmFnZXIgPSB0aGlzLl9rZXlNYW5hZ2VyO1xuXG4gICAgbWFuYWdlci5zZXRGb2N1c09yaWdpbihvcmlnaW4pLnNldEZpcnN0SXRlbUFjdGl2ZSgpO1xuXG4gICAgLy8gSWYgdGhlcmUncyBubyBhY3RpdmUgaXRlbSBhdCB0aGlzIHBvaW50LCBpdCBtZWFucyB0aGF0IGFsbCB0aGUgaXRlbXMgYXJlIGRpc2FibGVkLlxuICAgIC8vIE1vdmUgZm9jdXMgdG8gdGhlIG1lbnUgcGFuZWwgc28ga2V5Ym9hcmQgZXZlbnRzIGxpa2UgRXNjYXBlIHN0aWxsIHdvcmsuIEFsc28gdGhpcyB3aWxsXG4gICAgLy8gZ2l2ZSBfc29tZV8gZmVlZGJhY2sgdG8gc2NyZWVuIHJlYWRlcnMuXG4gICAgaWYgKCFtYW5hZ2VyLmFjdGl2ZUl0ZW0gJiYgdGhpcy5fZGlyZWN0RGVzY2VuZGFudEl0ZW1zLmxlbmd0aCkge1xuICAgICAgbGV0IGVsZW1lbnQgPSB0aGlzLl9kaXJlY3REZXNjZW5kYW50SXRlbXMuZmlyc3QuX2dldEhvc3RFbGVtZW50KCkucGFyZW50RWxlbWVudDtcblxuICAgICAgLy8gQmVjYXVzZSB0aGUgYG1hdC1tZW51YCBpcyBhdCB0aGUgRE9NIGluc2VydGlvbiBwb2ludCwgbm90IGluc2lkZSB0aGUgb3ZlcmxheSwgd2UgZG9uJ3RcbiAgICAgIC8vIGhhdmUgYSBuaWNlIHdheSBvZiBnZXR0aW5nIGEgaG9sZCBvZiB0aGUgbWVudSBwYW5lbC4gV2UgY2FuJ3QgdXNlIGEgYFZpZXdDaGlsZGAgZWl0aGVyXG4gICAgICAvLyBiZWNhdXNlIHRoZSBwYW5lbCBpcyBpbnNpZGUgYW4gYG5nLXRlbXBsYXRlYC4gV2Ugd29yayBhcm91bmQgaXQgYnkgc3RhcnRpbmcgZnJvbSBvbmUgb2ZcbiAgICAgIC8vIHRoZSBpdGVtcyBhbmQgd2Fsa2luZyB1cCB0aGUgRE9NLlxuICAgICAgd2hpbGUgKGVsZW1lbnQpIHtcbiAgICAgICAgaWYgKGVsZW1lbnQuZ2V0QXR0cmlidXRlKCdyb2xlJykgPT09ICdtZW51Jykge1xuICAgICAgICAgIGVsZW1lbnQuZm9jdXMoKTtcbiAgICAgICAgICBicmVhaztcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICBlbGVtZW50ID0gZWxlbWVudC5wYXJlbnRFbGVtZW50O1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIFJlc2V0cyB0aGUgYWN0aXZlIGl0ZW0gaW4gdGhlIG1lbnUuIFRoaXMgaXMgdXNlZCB3aGVuIHRoZSBtZW51IGlzIG9wZW5lZCwgYWxsb3dpbmdcbiAgICogdGhlIHVzZXIgdG8gc3RhcnQgZnJvbSB0aGUgZmlyc3Qgb3B0aW9uIHdoZW4gcHJlc3NpbmcgdGhlIGRvd24gYXJyb3cuXG4gICAqL1xuICByZXNldEFjdGl2ZUl0ZW0oKSB7XG4gICAgdGhpcy5fa2V5TWFuYWdlci5zZXRBY3RpdmVJdGVtKC0xKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBTZXRzIHRoZSBtZW51IHBhbmVsIGVsZXZhdGlvbi5cbiAgICogQHBhcmFtIGRlcHRoIE51bWJlciBvZiBwYXJlbnQgbWVudXMgdGhhdCBjb21lIGJlZm9yZSB0aGUgbWVudS5cbiAgICovXG4gIHNldEVsZXZhdGlvbihkZXB0aDogbnVtYmVyKTogdm9pZCB7XG4gICAgLy8gVGhlIGVsZXZhdGlvbiBzdGFydHMgYXQgdGhlIGJhc2UgYW5kIGluY3JlYXNlcyBieSBvbmUgZm9yIGVhY2ggbGV2ZWwuXG4gICAgLy8gQ2FwcGVkIGF0IDI0IGJlY2F1c2UgdGhhdCdzIHRoZSBtYXhpbXVtIGVsZXZhdGlvbiBkZWZpbmVkIGluIHRoZSBNYXRlcmlhbCBkZXNpZ24gc3BlYy5cbiAgICBjb25zdCBlbGV2YXRpb24gPSBNYXRoLm1pbihNQVRfTUVOVV9CQVNFX0VMRVZBVElPTiArIGRlcHRoLCAyNCk7XG4gICAgY29uc3QgbmV3RWxldmF0aW9uID0gYG1hdC1lbGV2YXRpb24teiR7ZWxldmF0aW9ufWA7XG4gICAgY29uc3QgY3VzdG9tRWxldmF0aW9uID0gT2JqZWN0LmtleXModGhpcy5fY2xhc3NMaXN0KS5maW5kKGMgPT4gYy5zdGFydHNXaXRoKCdtYXQtZWxldmF0aW9uLXonKSk7XG5cbiAgICBpZiAoIWN1c3RvbUVsZXZhdGlvbiB8fCBjdXN0b21FbGV2YXRpb24gPT09IHRoaXMuX3ByZXZpb3VzRWxldmF0aW9uKSB7XG4gICAgICBpZiAodGhpcy5fcHJldmlvdXNFbGV2YXRpb24pIHtcbiAgICAgICAgdGhpcy5fY2xhc3NMaXN0W3RoaXMuX3ByZXZpb3VzRWxldmF0aW9uXSA9IGZhbHNlO1xuICAgICAgfVxuXG4gICAgICB0aGlzLl9jbGFzc0xpc3RbbmV3RWxldmF0aW9uXSA9IHRydWU7XG4gICAgICB0aGlzLl9wcmV2aW91c0VsZXZhdGlvbiA9IG5ld0VsZXZhdGlvbjtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogQWRkcyBjbGFzc2VzIHRvIHRoZSBtZW51IHBhbmVsIGJhc2VkIG9uIGl0cyBwb3NpdGlvbi4gQ2FuIGJlIHVzZWQgYnlcbiAgICogY29uc3VtZXJzIHRvIGFkZCBzcGVjaWZpYyBzdHlsaW5nIGJhc2VkIG9uIHRoZSBwb3NpdGlvbi5cbiAgICogQHBhcmFtIHBvc1ggUG9zaXRpb24gb2YgdGhlIG1lbnUgYWxvbmcgdGhlIHggYXhpcy5cbiAgICogQHBhcmFtIHBvc1kgUG9zaXRpb24gb2YgdGhlIG1lbnUgYWxvbmcgdGhlIHkgYXhpcy5cbiAgICogQGRvY3MtcHJpdmF0ZVxuICAgKi9cbiAgc2V0UG9zaXRpb25DbGFzc2VzKHBvc1g6IE1lbnVQb3NpdGlvblggPSB0aGlzLnhQb3NpdGlvbiwgcG9zWTogTWVudVBvc2l0aW9uWSA9IHRoaXMueVBvc2l0aW9uKSB7XG4gICAgY29uc3QgY2xhc3NlcyA9IHRoaXMuX2NsYXNzTGlzdDtcbiAgICBjbGFzc2VzWydtYXQtbWVudS1iZWZvcmUnXSA9IHBvc1ggPT09ICdiZWZvcmUnO1xuICAgIGNsYXNzZXNbJ21hdC1tZW51LWFmdGVyJ10gPSBwb3NYID09PSAnYWZ0ZXInO1xuICAgIGNsYXNzZXNbJ21hdC1tZW51LWFib3ZlJ10gPSBwb3NZID09PSAnYWJvdmUnO1xuICAgIGNsYXNzZXNbJ21hdC1tZW51LWJlbG93J10gPSBwb3NZID09PSAnYmVsb3cnO1xuICB9XG5cbiAgLyoqIFN0YXJ0cyB0aGUgZW50ZXIgYW5pbWF0aW9uLiAqL1xuICBfc3RhcnRBbmltYXRpb24oKSB7XG4gICAgLy8gQGJyZWFraW5nLWNoYW5nZSA4LjAuMCBDb21iaW5lIHdpdGggX3Jlc2V0QW5pbWF0aW9uLlxuICAgIHRoaXMuX3BhbmVsQW5pbWF0aW9uU3RhdGUgPSAnZW50ZXInO1xuICB9XG5cbiAgLyoqIFJlc2V0cyB0aGUgcGFuZWwgYW5pbWF0aW9uIHRvIGl0cyBpbml0aWFsIHN0YXRlLiAqL1xuICBfcmVzZXRBbmltYXRpb24oKSB7XG4gICAgLy8gQGJyZWFraW5nLWNoYW5nZSA4LjAuMCBDb21iaW5lIHdpdGggX3N0YXJ0QW5pbWF0aW9uLlxuICAgIHRoaXMuX3BhbmVsQW5pbWF0aW9uU3RhdGUgPSAndm9pZCc7XG4gIH1cblxuICAvKiogQ2FsbGJhY2sgdGhhdCBpcyBpbnZva2VkIHdoZW4gdGhlIHBhbmVsIGFuaW1hdGlvbiBjb21wbGV0ZXMuICovXG4gIF9vbkFuaW1hdGlvbkRvbmUoZXZlbnQ6IEFuaW1hdGlvbkV2ZW50KSB7XG4gICAgdGhpcy5fYW5pbWF0aW9uRG9uZS5uZXh0KGV2ZW50KTtcbiAgICB0aGlzLl9pc0FuaW1hdGluZyA9IGZhbHNlO1xuICB9XG5cbiAgX29uQW5pbWF0aW9uU3RhcnQoZXZlbnQ6IEFuaW1hdGlvbkV2ZW50KSB7XG4gICAgdGhpcy5faXNBbmltYXRpbmcgPSB0cnVlO1xuXG4gICAgLy8gU2Nyb2xsIHRoZSBjb250ZW50IGVsZW1lbnQgdG8gdGhlIHRvcCBhcyBzb29uIGFzIHRoZSBhbmltYXRpb24gc3RhcnRzLiBUaGlzIGlzIG5lY2Vzc2FyeSxcbiAgICAvLyBiZWNhdXNlIHdlIG1vdmUgZm9jdXMgdG8gdGhlIGZpcnN0IGl0ZW0gd2hpbGUgaXQncyBzdGlsbCBiZWluZyBhbmltYXRlZCwgd2hpY2ggY2FuIHRocm93XG4gICAgLy8gdGhlIGJyb3dzZXIgb2ZmIHdoZW4gaXQgZGV0ZXJtaW5lcyB0aGUgc2Nyb2xsIHBvc2l0aW9uLiBBbHRlcm5hdGl2ZWx5IHdlIGNhbiBtb3ZlIGZvY3VzXG4gICAgLy8gd2hlbiB0aGUgYW5pbWF0aW9uIGlzIGRvbmUsIGhvd2V2ZXIgbW92aW5nIGZvY3VzIGFzeW5jaHJvbm91c2x5IHdpbGwgaW50ZXJydXB0IHNjcmVlblxuICAgIC8vIHJlYWRlcnMgd2hpY2ggYXJlIGluIHRoZSBwcm9jZXNzIG9mIHJlYWRpbmcgb3V0IHRoZSBtZW51IGFscmVhZHkuIFdlIHRha2UgdGhlIGBlbGVtZW50YFxuICAgIC8vIGZyb20gdGhlIGBldmVudGAgc2luY2Ugd2UgY2FuJ3QgdXNlIGEgYFZpZXdDaGlsZGAgdG8gYWNjZXNzIHRoZSBwYW5lLlxuICAgIGlmIChldmVudC50b1N0YXRlID09PSAnZW50ZXInICYmIHRoaXMuX2tleU1hbmFnZXIuYWN0aXZlSXRlbUluZGV4ID09PSAwKSB7XG4gICAgICBldmVudC5lbGVtZW50LnNjcm9sbFRvcCA9IDA7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIFNldHMgdXAgYSBzdHJlYW0gdGhhdCB3aWxsIGtlZXAgdHJhY2sgb2YgYW55IG5ld2x5LWFkZGVkIG1lbnUgaXRlbXMgYW5kIHdpbGwgdXBkYXRlIHRoZSBsaXN0XG4gICAqIG9mIGRpcmVjdCBkZXNjZW5kYW50cy4gV2UgY29sbGVjdCB0aGUgZGVzY2VuZGFudHMgdGhpcyB3YXksIGJlY2F1c2UgYF9hbGxJdGVtc2AgY2FuIGluY2x1ZGVcbiAgICogaXRlbXMgdGhhdCBhcmUgcGFydCBvZiBjaGlsZCBtZW51cywgYW5kIHVzaW5nIGEgY3VzdG9tIHdheSBvZiByZWdpc3RlcmluZyBpdGVtcyBpcyB1bnJlbGlhYmxlXG4gICAqIHdoZW4gaXQgY29tZXMgdG8gbWFpbnRhaW5pbmcgdGhlIGl0ZW0gb3JkZXIuXG4gICAqL1xuICBwcml2YXRlIF91cGRhdGVEaXJlY3REZXNjZW5kYW50cygpIHtcbiAgICB0aGlzLl9hbGxJdGVtcy5jaGFuZ2VzXG4gICAgICAucGlwZShzdGFydFdpdGgodGhpcy5fYWxsSXRlbXMpKVxuICAgICAgLnN1YnNjcmliZSgoaXRlbXM6IFF1ZXJ5TGlzdDxNYXRNZW51SXRlbT4pID0+IHtcbiAgICAgICAgdGhpcy5fZGlyZWN0RGVzY2VuZGFudEl0ZW1zLnJlc2V0KGl0ZW1zLmZpbHRlcihpdGVtID0+IGl0ZW0uX3BhcmVudE1lbnUgPT09IHRoaXMpKTtcbiAgICAgICAgdGhpcy5fZGlyZWN0RGVzY2VuZGFudEl0ZW1zLm5vdGlmeU9uQ2hhbmdlcygpO1xuICAgICAgfSk7XG4gIH1cblxuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfb3ZlcmxhcFRyaWdnZXI6IEJvb2xlYW5JbnB1dDtcbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX2hhc0JhY2tkcm9wOiBCb29sZWFuSW5wdXQ7XG59XG5cbi8qKiBAZG9jcy1wcml2YXRlIFdlIHNob3cgdGhlIFwiX01hdE1lbnVcIiBjbGFzcyBhcyBcIk1hdE1lbnVcIiBpbiB0aGUgZG9jcy4gKi9cbkBEaXJlY3RpdmUoKVxuZXhwb3J0IGNsYXNzIE1hdE1lbnUgZXh0ZW5kcyBfTWF0TWVudUJhc2Uge31cblxuLy8gTm90ZSBvbiB0aGUgd2VpcmQgaW5oZXJpdGFuY2Ugc2V0dXA6IHdlIG5lZWQgdGhyZWUgY2xhc3NlcywgYmVjYXVzZSB0aGUgTURDLWJhc2VkIG1lbnUgaGFzIHRvXG4vLyBleHRlbmQgYE1hdE1lbnVgLCBob3dldmVyIGtlZXBpbmcgYSByZWZlcmVuY2UgdG8gaXQgd2lsbCBjYXVzZSB0aGUgaW5saW5lZCB0ZW1wbGF0ZSBhbmQgc3R5bGVzXG4vLyB0byBiZSByZXRhaW5lZCBhcyB3ZWxsLiBUaGUgTURDIG1lbnUgYWxzbyBoYXMgdG8gcHJvdmlkZSBpdHNlbGYgYXMgYSBgTWF0TWVudWAgaW4gb3JkZXIgZm9yXG4vLyBxdWVyaWVzIGFuZCBESSB0byB3b3JrIGNvcnJlY3RseSwgd2hpbGUgc3RpbGwgbm90IHJlZmVyZW5jaW5nIHRoZSBhY3R1YWwgbWVudSBjbGFzcy5cbi8vIENsYXNzIHJlc3BvbnNpYmlsaXR5IGlzIHNwbGl0IHVwIGFzIGZvbGxvd3M6XG4vLyAqIF9NYXRNZW51QmFzZSAtIHByb3ZpZGVzIGFsbCB0aGUgZnVuY3Rpb25hbGl0eSB3aXRob3V0IGFueSBvZiB0aGUgQW5ndWxhciBtZXRhZGF0YS5cbi8vICogTWF0TWVudSAtIGtlZXBzIHRoZSBzYW1lIG5hbWUgc3ltYm9sIG5hbWUgYXMgdGhlIGN1cnJlbnQgbWVudSBhbmRcbi8vIGlzIHVzZWQgYXMgYSBwcm92aWRlciBmb3IgREkgYW5kIHF1ZXJ5IHB1cnBvc2VzLlxuLy8gKiBfTWF0TWVudSAtIHRoZSBhY3R1YWwgbWVudSBjb21wb25lbnQgaW1wbGVtZW50YXRpb24gd2l0aCB0aGUgQW5ndWxhciBtZXRhZGF0YSB0aGF0IHNob3VsZFxuLy8gYmUgdHJlZSBzaGFrZW4gYXdheSBmb3IgTURDLlxuXG4vKiogQGRvY3MtcHVibGljIE1hdE1lbnUgKi9cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ21hdC1tZW51JyxcbiAgdGVtcGxhdGVVcmw6ICdtZW51Lmh0bWwnLFxuICBzdHlsZVVybHM6IFsnbWVudS5jc3MnXSxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2gsXG4gIGVuY2Fwc3VsYXRpb246IFZpZXdFbmNhcHN1bGF0aW9uLk5vbmUsXG4gIGV4cG9ydEFzOiAnbWF0TWVudScsXG4gIGFuaW1hdGlvbnM6IFtcbiAgICBtYXRNZW51QW5pbWF0aW9ucy50cmFuc2Zvcm1NZW51LFxuICAgIG1hdE1lbnVBbmltYXRpb25zLmZhZGVJbkl0ZW1zXG4gIF0sXG4gIHByb3ZpZGVyczogW1xuICAgIHtwcm92aWRlOiBNQVRfTUVOVV9QQU5FTCwgdXNlRXhpc3Rpbmc6IE1hdE1lbnV9LFxuICAgIHtwcm92aWRlOiBNYXRNZW51LCB1c2VFeGlzdGluZzogX01hdE1lbnV9XG4gIF1cbn0pXG4vLyB0c2xpbnQ6ZGlzYWJsZS1uZXh0LWxpbmU6Y2xhc3MtbmFtZVxuZXhwb3J0IGNsYXNzIF9NYXRNZW51IGV4dGVuZHMgTWF0TWVudSB7XG5cbiAgY29uc3RydWN0b3IoZWxlbWVudFJlZjogRWxlbWVudFJlZjxIVE1MRWxlbWVudD4sIG5nWm9uZTogTmdab25lLFxuICAgICAgQEluamVjdChNQVRfTUVOVV9ERUZBVUxUX09QVElPTlMpIGRlZmF1bHRPcHRpb25zOiBNYXRNZW51RGVmYXVsdE9wdGlvbnMpIHtcbiAgICBzdXBlcihlbGVtZW50UmVmLCBuZ1pvbmUsIGRlZmF1bHRPcHRpb25zKTtcbiAgfVxufVxuIl19