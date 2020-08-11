/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ComponentFactoryResolver, Directive, EventEmitter, NgModule, Output, TemplateRef, ViewContainerRef, Inject, } from '@angular/core';
import { DOCUMENT } from '@angular/common';
import { BasePortalOutlet, TemplatePortal } from './portal';
/**
 * Directive version of a `TemplatePortal`. Because the directive *is* a TemplatePortal,
 * the directive instance itself can be attached to a host, enabling declarative use of portals.
 */
let CdkPortal = /** @class */ (() => {
    class CdkPortal extends TemplatePortal {
        constructor(templateRef, viewContainerRef) {
            super(templateRef, viewContainerRef);
        }
    }
    CdkPortal.decorators = [
        { type: Directive, args: [{
                    selector: '[cdkPortal]',
                    exportAs: 'cdkPortal',
                },] }
    ];
    CdkPortal.ctorParameters = () => [
        { type: TemplateRef },
        { type: ViewContainerRef }
    ];
    return CdkPortal;
})();
export { CdkPortal };
/**
 * @deprecated Use `CdkPortal` instead.
 * @breaking-change 9.0.0
 */
let TemplatePortalDirective = /** @class */ (() => {
    class TemplatePortalDirective extends CdkPortal {
    }
    TemplatePortalDirective.decorators = [
        { type: Directive, args: [{
                    selector: '[cdk-portal], [portal]',
                    exportAs: 'cdkPortal',
                    providers: [{
                            provide: CdkPortal,
                            useExisting: TemplatePortalDirective
                        }]
                },] }
    ];
    return TemplatePortalDirective;
})();
export { TemplatePortalDirective };
/**
 * Directive version of a PortalOutlet. Because the directive *is* a PortalOutlet, portals can be
 * directly attached to it, enabling declarative use.
 *
 * Usage:
 * `<ng-template [cdkPortalOutlet]="greeting"></ng-template>`
 */
let CdkPortalOutlet = /** @class */ (() => {
    class CdkPortalOutlet extends BasePortalOutlet {
        constructor(_componentFactoryResolver, _viewContainerRef, 
        /**
         * @deprecated `_document` parameter to be made required.
         * @breaking-change 9.0.0
         */
        _document) {
            super();
            this._componentFactoryResolver = _componentFactoryResolver;
            this._viewContainerRef = _viewContainerRef;
            /** Whether the portal component is initialized. */
            this._isInitialized = false;
            /** Emits when a portal is attached to the outlet. */
            this.attached = new EventEmitter();
            /**
             * Attaches the given DomPortal to this PortalHost by moving all of the portal content into it.
             * @param portal Portal to be attached.
             * @deprecated To be turned into a method.
             * @breaking-change 10.0.0
             */
            this.attachDomPortal = (portal) => {
                // @breaking-change 9.0.0 Remove check and error once the
                // `_document` constructor parameter is required.
                if (!this._document) {
                    throw Error('Cannot attach DOM portal without _document constructor parameter');
                }
                const element = portal.element;
                if (!element.parentNode) {
                    throw Error('DOM portal content must be attached to a parent node.');
                }
                // Anchor used to save the element's previous position so
                // that we can restore it when the portal is detached.
                const anchorNode = this._document.createComment('dom-portal');
                portal.setAttachedHost(this);
                element.parentNode.insertBefore(anchorNode, element);
                this._getRootNode().appendChild(element);
                super.setDisposeFn(() => {
                    if (anchorNode.parentNode) {
                        anchorNode.parentNode.replaceChild(element, anchorNode);
                    }
                });
            };
            this._document = _document;
        }
        /** Portal associated with the Portal outlet. */
        get portal() {
            return this._attachedPortal;
        }
        set portal(portal) {
            // Ignore the cases where the `portal` is set to a falsy value before the lifecycle hooks have
            // run. This handles the cases where the user might do something like `<div cdkPortalOutlet>`
            // and attach a portal programmatically in the parent component. When Angular does the first CD
            // round, it will fire the setter with empty string, causing the user's content to be cleared.
            if (this.hasAttached() && !portal && !this._isInitialized) {
                return;
            }
            if (this.hasAttached()) {
                super.detach();
            }
            if (portal) {
                super.attach(portal);
            }
            this._attachedPortal = portal;
        }
        /** Component or view reference that is attached to the portal. */
        get attachedRef() {
            return this._attachedRef;
        }
        ngOnInit() {
            this._isInitialized = true;
        }
        ngOnDestroy() {
            super.dispose();
            this._attachedPortal = null;
            this._attachedRef = null;
        }
        /**
         * Attach the given ComponentPortal to this PortalOutlet using the ComponentFactoryResolver.
         *
         * @param portal Portal to be attached to the portal outlet.
         * @returns Reference to the created component.
         */
        attachComponentPortal(portal) {
            portal.setAttachedHost(this);
            // If the portal specifies an origin, use that as the logical location of the component
            // in the application tree. Otherwise use the location of this PortalOutlet.
            const viewContainerRef = portal.viewContainerRef != null ?
                portal.viewContainerRef :
                this._viewContainerRef;
            const resolver = portal.componentFactoryResolver || this._componentFactoryResolver;
            const componentFactory = resolver.resolveComponentFactory(portal.component);
            const ref = viewContainerRef.createComponent(componentFactory, viewContainerRef.length, portal.injector || viewContainerRef.injector);
            // If we're using a view container that's different from the injected one (e.g. when the portal
            // specifies its own) we need to move the component into the outlet, otherwise it'll be rendered
            // inside of the alternate view container.
            if (viewContainerRef !== this._viewContainerRef) {
                this._getRootNode().appendChild(ref.hostView.rootNodes[0]);
            }
            super.setDisposeFn(() => ref.destroy());
            this._attachedPortal = portal;
            this._attachedRef = ref;
            this.attached.emit(ref);
            return ref;
        }
        /**
         * Attach the given TemplatePortal to this PortalHost as an embedded View.
         * @param portal Portal to be attached.
         * @returns Reference to the created embedded view.
         */
        attachTemplatePortal(portal) {
            portal.setAttachedHost(this);
            const viewRef = this._viewContainerRef.createEmbeddedView(portal.templateRef, portal.context);
            super.setDisposeFn(() => this._viewContainerRef.clear());
            this._attachedPortal = portal;
            this._attachedRef = viewRef;
            this.attached.emit(viewRef);
            return viewRef;
        }
        /** Gets the root node of the portal outlet. */
        _getRootNode() {
            const nativeElement = this._viewContainerRef.element.nativeElement;
            // The directive could be set on a template which will result in a comment
            // node being the root. Use the comment's parent node if that is the case.
            return (nativeElement.nodeType === nativeElement.ELEMENT_NODE ?
                nativeElement : nativeElement.parentNode);
        }
    }
    CdkPortalOutlet.decorators = [
        { type: Directive, args: [{
                    selector: '[cdkPortalOutlet]',
                    exportAs: 'cdkPortalOutlet',
                    inputs: ['portal: cdkPortalOutlet']
                },] }
    ];
    CdkPortalOutlet.ctorParameters = () => [
        { type: ComponentFactoryResolver },
        { type: ViewContainerRef },
        { type: undefined, decorators: [{ type: Inject, args: [DOCUMENT,] }] }
    ];
    CdkPortalOutlet.propDecorators = {
        attached: [{ type: Output }]
    };
    return CdkPortalOutlet;
})();
export { CdkPortalOutlet };
/**
 * @deprecated Use `CdkPortalOutlet` instead.
 * @breaking-change 9.0.0
 */
let PortalHostDirective = /** @class */ (() => {
    class PortalHostDirective extends CdkPortalOutlet {
    }
    PortalHostDirective.decorators = [
        { type: Directive, args: [{
                    selector: '[cdkPortalHost], [portalHost]',
                    exportAs: 'cdkPortalHost',
                    inputs: ['portal: cdkPortalHost'],
                    providers: [{
                            provide: CdkPortalOutlet,
                            useExisting: PortalHostDirective
                        }]
                },] }
    ];
    return PortalHostDirective;
})();
export { PortalHostDirective };
let PortalModule = /** @class */ (() => {
    class PortalModule {
    }
    PortalModule.decorators = [
        { type: NgModule, args: [{
                    exports: [CdkPortal, CdkPortalOutlet, TemplatePortalDirective, PortalHostDirective],
                    declarations: [CdkPortal, CdkPortalOutlet, TemplatePortalDirective, PortalHostDirective],
                },] }
    ];
    return PortalModule;
})();
export { PortalModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicG9ydGFsLWRpcmVjdGl2ZXMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL3BvcnRhbC9wb3J0YWwtZGlyZWN0aXZlcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQ0wsd0JBQXdCLEVBRXhCLFNBQVMsRUFFVCxZQUFZLEVBQ1osUUFBUSxFQUdSLE1BQU0sRUFDTixXQUFXLEVBQ1gsZ0JBQWdCLEVBQ2hCLE1BQU0sR0FDUCxNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUMsUUFBUSxFQUFDLE1BQU0saUJBQWlCLENBQUM7QUFDekMsT0FBTyxFQUFDLGdCQUFnQixFQUEyQixjQUFjLEVBQVksTUFBTSxVQUFVLENBQUM7QUFHOUY7OztHQUdHO0FBQ0g7SUFBQSxNQUlhLFNBQVUsU0FBUSxjQUFjO1FBQzNDLFlBQVksV0FBNkIsRUFBRSxnQkFBa0M7WUFDM0UsS0FBSyxDQUFDLFdBQVcsRUFBRSxnQkFBZ0IsQ0FBQyxDQUFDO1FBQ3ZDLENBQUM7OztnQkFQRixTQUFTLFNBQUM7b0JBQ1QsUUFBUSxFQUFFLGFBQWE7b0JBQ3ZCLFFBQVEsRUFBRSxXQUFXO2lCQUN0Qjs7O2dCQWZDLFdBQVc7Z0JBQ1gsZ0JBQWdCOztJQW1CbEIsZ0JBQUM7S0FBQTtTQUpZLFNBQVM7QUFNdEI7OztHQUdHO0FBQ0g7SUFBQSxNQVFhLHVCQUF3QixTQUFRLFNBQVM7OztnQkFSckQsU0FBUyxTQUFDO29CQUNULFFBQVEsRUFBRSx3QkFBd0I7b0JBQ2xDLFFBQVEsRUFBRSxXQUFXO29CQUNyQixTQUFTLEVBQUUsQ0FBQzs0QkFDVixPQUFPLEVBQUUsU0FBUzs0QkFDbEIsV0FBVyxFQUFFLHVCQUF1Qjt5QkFDckMsQ0FBQztpQkFDSDs7SUFDdUQsOEJBQUM7S0FBQTtTQUE1Qyx1QkFBdUI7QUFRcEM7Ozs7OztHQU1HO0FBQ0g7SUFBQSxNQUthLGVBQWdCLFNBQVEsZ0JBQWdC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LGVBQWU7NEJBQ3hCLFdBQVcsRUFBRSxtQkFBbUI7eUJBQ2pDLENBQUM7aUJBQ0g7O0lBQ3lELDBCQUFDO0tBQUE7U0FBOUMsbUJBQW1CO0FBR2hDO0lBQUEsTUFJYSxZQUFZOzs7Z0JBSnhCLFFBQVEsU0FBQztvQkFDUixPQUFPLEVBQUUsQ0FBQyxTQUFTLEVBQUUsZUFBZSxFQUFFLHVCQUF1QixFQUFFLG1CQUFtQixDQUFDO29CQUNuRixZQUFZLEVBQUUsQ0FBQyxTQUFTLEVBQUUsZUFBZSxFQUFFLHVCQUF1QixFQUFFLG1CQUFtQixDQUFDO2lCQUN6Rjs7SUFDMEIsbUJBQUM7S0FBQTtTQUFmLFlBQVkiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtcbiAgQ29tcG9uZW50RmFjdG9yeVJlc29sdmVyLFxuICBDb21wb25lbnRSZWYsXG4gIERpcmVjdGl2ZSxcbiAgRW1iZWRkZWRWaWV3UmVmLFxuICBFdmVudEVtaXR0ZXIsXG4gIE5nTW9kdWxlLFxuICBPbkRlc3Ryb3ksXG4gIE9uSW5pdCxcbiAgT3V0cHV0LFxuICBUZW1wbGF0ZVJlZixcbiAgVmlld0NvbnRhaW5lclJlZixcbiAgSW5qZWN0LFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7RE9DVU1FTlR9IGZyb20gJ0Bhbmd1bGFyL2NvbW1vbic7XG5pbXBvcnQge0Jhc2VQb3J0YWxPdXRsZXQsIENvbXBvbmVudFBvcnRhbCwgUG9ydGFsLCBUZW1wbGF0ZVBvcnRhbCwgRG9tUG9ydGFsfSBmcm9tICcuL3BvcnRhbCc7XG5cblxuLyoqXG4gKiBEaXJlY3RpdmUgdmVyc2lvbiBvZiBhIGBUZW1wbGF0ZVBvcnRhbGAuIEJlY2F1c2UgdGhlIGRpcmVjdGl2ZSAqaXMqIGEgVGVtcGxhdGVQb3J0YWwsXG4gKiB0aGUgZGlyZWN0aXZlIGluc3RhbmNlIGl0c2VsZiBjYW4gYmUgYXR0YWNoZWQgdG8gYSBob3N0LCBlbmFibGluZyBkZWNsYXJhdGl2ZSB1c2Ugb2YgcG9ydGFscy5cbiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnW2Nka1BvcnRhbF0nLFxuICBleHBvcnRBczogJ2Nka1BvcnRhbCcsXG59KVxuZXhwb3J0IGNsYXNzIENka1BvcnRhbCBleHRlbmRzIFRlbXBsYXRlUG9ydGFsIHtcbiAgY29uc3RydWN0b3IodGVtcGxhdGVSZWY6IFRlbXBsYXRlUmVmPGFueT4sIHZpZXdDb250YWluZXJSZWY6IFZpZXdDb250YWluZXJSZWYpIHtcbiAgICBzdXBlcih0ZW1wbGF0ZVJlZiwgdmlld0NvbnRhaW5lclJlZik7XG4gIH1cbn1cblxuLyoqXG4gKiBAZGVwcmVjYXRlZCBVc2UgYENka1BvcnRhbGAgaW5zdGVhZC5cbiAqIEBicmVha2luZy1jaGFuZ2UgOS4wLjBcbiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnW2Nkay1wb3J0YWxdLCBbcG9ydGFsXScsXG4gIGV4cG9ydEFzOiAnY2RrUG9ydGFsJyxcbiAgcHJvdmlkZXJzOiBbe1xuICAgIHByb3ZpZGU6IENka1BvcnRhbCxcbiAgICB1c2VFeGlzdGluZzogVGVtcGxhdGVQb3J0YWxEaXJlY3RpdmVcbiAgfV1cbn0pXG5leHBvcnQgY2xhc3MgVGVtcGxhdGVQb3J0YWxEaXJlY3RpdmUgZXh0ZW5kcyBDZGtQb3J0YWwge31cblxuLyoqXG4gKiBQb3NzaWJsZSBhdHRhY2hlZCByZWZlcmVuY2VzIHRvIHRoZSBDZGtQb3J0YWxPdXRsZXQuXG4gKi9cbmV4cG9ydCB0eXBlIENka1BvcnRhbE91dGxldEF0dGFjaGVkUmVmID0gQ29tcG9uZW50UmVmPGFueT4gfCBFbWJlZGRlZFZpZXdSZWY8YW55PiB8IG51bGw7XG5cblxuLyoqXG4gKiBEaXJlY3RpdmUgdmVyc2lvbiBvZiBhIFBvcnRhbE91dGxldC4gQmVjYXVzZSB0aGUgZGlyZWN0aXZlICppcyogYSBQb3J0YWxPdXRsZXQsIHBvcnRhbHMgY2FuIGJlXG4gKiBkaXJlY3RseSBhdHRhY2hlZCB0byBpdCwgZW5hYmxpbmcgZGVjbGFyYXRpdmUgdXNlLlxuICpcbiAqIFVzYWdlOlxuICogYDxuZy10ZW1wbGF0ZSBbY2RrUG9ydGFsT3V0bGV0XT1cImdyZWV0aW5nXCI+PC9uZy10ZW1wbGF0ZT5gXG4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogJ1tjZGtQb3J0YWxPdXRsZXRdJyxcbiAgZXhwb3J0QXM6ICdjZGtQb3J0YWxPdXRsZXQnLFxuICBpbnB1dHM6IFsncG9ydGFsOiBjZGtQb3J0YWxPdXRsZXQnXVxufSlcbmV4cG9ydCBjbGFzcyBDZGtQb3J0YWxPdXRsZXQgZXh0ZW5kcyBCYXNlUG9ydGFsT3V0bGV0IGltcGxlbWVudHMgT25Jbml0LCBPbkRlc3Ryb3kge1xuICBwcml2YXRlIF9kb2N1bWVudDogRG9jdW1lbnQ7XG5cbiAgLyoqIFdoZXRoZXIgdGhlIHBvcnRhbCBjb21wb25lbnQgaXMgaW5pdGlhbGl6ZWQuICovXG4gIHByaXZhdGUgX2lzSW5pdGlhbGl6ZWQgPSBmYWxzZTtcblxuICAvKiogUmVmZXJlbmNlIHRvIHRoZSBjdXJyZW50bHktYXR0YWNoZWQgY29tcG9uZW50L3ZpZXcgcmVmLiAqL1xuICBwcml2YXRlIF9hdHRhY2hlZFJlZjogQ2RrUG9ydGFsT3V0bGV0QXR0YWNoZWRSZWY7XG5cbiAgY29uc3RydWN0b3IoXG4gICAgICBwcml2YXRlIF9jb21wb25lbnRGYWN0b3J5UmVzb2x2ZXI6IENvbXBvbmVudEZhY3RvcnlSZXNvbHZlcixcbiAgICAgIHByaXZhdGUgX3ZpZXdDb250YWluZXJSZWY6IFZpZXdDb250YWluZXJSZWYsXG5cbiAgICAgIC8qKlxuICAgICAgICogQGRlcHJlY2F0ZWQgYF9kb2N1bWVudGAgcGFyYW1ldGVyIHRvIGJlIG1hZGUgcmVxdWlyZWQuXG4gICAgICAgKiBAYnJlYWtpbmctY2hhbmdlIDkuMC4wXG4gICAgICAgKi9cbiAgICAgIEBJbmplY3QoRE9DVU1FTlQpIF9kb2N1bWVudD86IGFueSkge1xuICAgIHN1cGVyKCk7XG4gICAgdGhpcy5fZG9jdW1lbnQgPSBfZG9jdW1lbnQ7XG4gIH1cblxuICAvKiogUG9ydGFsIGFzc29jaWF0ZWQgd2l0aCB0aGUgUG9ydGFsIG91dGxldC4gKi9cbiAgZ2V0IHBvcnRhbCgpOiBQb3J0YWw8YW55PiB8IG51bGwge1xuICAgIHJldHVybiB0aGlzLl9hdHRhY2hlZFBvcnRhbDtcbiAgfVxuXG4gIHNldCBwb3J0YWwocG9ydGFsOiBQb3J0YWw8YW55PiB8IG51bGwpIHtcbiAgICAvLyBJZ25vcmUgdGhlIGNhc2VzIHdoZXJlIHRoZSBgcG9ydGFsYCBpcyBzZXQgdG8gYSBmYWxzeSB2YWx1ZSBiZWZvcmUgdGhlIGxpZmVjeWNsZSBob29rcyBoYXZlXG4gICAgLy8gcnVuLiBUaGlzIGhhbmRsZXMgdGhlIGNhc2VzIHdoZXJlIHRoZSB1c2VyIG1pZ2h0IGRvIHNvbWV0aGluZyBsaWtlIGA8ZGl2IGNka1BvcnRhbE91dGxldD5gXG4gICAgLy8gYW5kIGF0dGFjaCBhIHBvcnRhbCBwcm9ncmFtbWF0aWNhbGx5IGluIHRoZSBwYXJlbnQgY29tcG9uZW50LiBXaGVuIEFuZ3VsYXIgZG9lcyB0aGUgZmlyc3QgQ0RcbiAgICAvLyByb3VuZCwgaXQgd2lsbCBmaXJlIHRoZSBzZXR0ZXIgd2l0aCBlbXB0eSBzdHJpbmcsIGNhdXNpbmcgdGhlIHVzZXIncyBjb250ZW50IHRvIGJlIGNsZWFyZWQuXG4gICAgaWYgKHRoaXMuaGFzQXR0YWNoZWQoKSAmJiAhcG9ydGFsICYmICF0aGlzLl9pc0luaXRpYWxpemVkKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgaWYgKHRoaXMuaGFzQXR0YWNoZWQoKSkge1xuICAgICAgc3VwZXIuZGV0YWNoKCk7XG4gICAgfVxuXG4gICAgaWYgKHBvcnRhbCkge1xuICAgICAgc3VwZXIuYXR0YWNoKHBvcnRhbCk7XG4gICAgfVxuXG4gICAgdGhpcy5fYXR0YWNoZWRQb3J0YWwgPSBwb3J0YWw7XG4gIH1cblxuICAvKiogRW1pdHMgd2hlbiBhIHBvcnRhbCBpcyBhdHRhY2hlZCB0byB0aGUgb3V0bGV0LiAqL1xuICBAT3V0cHV0KCkgYXR0YWNoZWQ6IEV2ZW50RW1pdHRlcjxDZGtQb3J0YWxPdXRsZXRBdHRhY2hlZFJlZj4gPVxuICAgICAgbmV3IEV2ZW50RW1pdHRlcjxDZGtQb3J0YWxPdXRsZXRBdHRhY2hlZFJlZj4oKTtcblxuICAvKiogQ29tcG9uZW50IG9yIHZpZXcgcmVmZXJlbmNlIHRoYXQgaXMgYXR0YWNoZWQgdG8gdGhlIHBvcnRhbC4gKi9cbiAgZ2V0IGF0dGFjaGVkUmVmKCk6IENka1BvcnRhbE91dGxldEF0dGFjaGVkUmVmIHtcbiAgICByZXR1cm4gdGhpcy5fYXR0YWNoZWRSZWY7XG4gIH1cblxuICBuZ09uSW5pdCgpIHtcbiAgICB0aGlzLl9pc0luaXRpYWxpemVkID0gdHJ1ZTtcbiAgfVxuXG4gIG5nT25EZXN0cm95KCkge1xuICAgIHN1cGVyLmRpc3Bvc2UoKTtcbiAgICB0aGlzLl9hdHRhY2hlZFBvcnRhbCA9IG51bGw7XG4gICAgdGhpcy5fYXR0YWNoZWRSZWYgPSBudWxsO1xuICB9XG5cbiAgLyoqXG4gICAqIEF0dGFjaCB0aGUgZ2l2ZW4gQ29tcG9uZW50UG9ydGFsIHRvIHRoaXMgUG9ydGFsT3V0bGV0IHVzaW5nIHRoZSBDb21wb25lbnRGYWN0b3J5UmVzb2x2ZXIuXG4gICAqXG4gICAqIEBwYXJhbSBwb3J0YWwgUG9ydGFsIHRvIGJlIGF0dGFjaGVkIHRvIHRoZSBwb3J0YWwgb3V0bGV0LlxuICAgKiBAcmV0dXJucyBSZWZlcmVuY2UgdG8gdGhlIGNyZWF0ZWQgY29tcG9uZW50LlxuICAgKi9cbiAgYXR0YWNoQ29tcG9uZW50UG9ydGFsPFQ+KHBvcnRhbDogQ29tcG9uZW50UG9ydGFsPFQ+KTogQ29tcG9uZW50UmVmPFQ+IHtcbiAgICBwb3J0YWwuc2V0QXR0YWNoZWRIb3N0KHRoaXMpO1xuXG4gICAgLy8gSWYgdGhlIHBvcnRhbCBzcGVjaWZpZXMgYW4gb3JpZ2luLCB1c2UgdGhhdCBhcyB0aGUgbG9naWNhbCBsb2NhdGlvbiBvZiB0aGUgY29tcG9uZW50XG4gICAgLy8gaW4gdGhlIGFwcGxpY2F0aW9uIHRyZWUuIE90aGVyd2lzZSB1c2UgdGhlIGxvY2F0aW9uIG9mIHRoaXMgUG9ydGFsT3V0bGV0LlxuICAgIGNvbnN0IHZpZXdDb250YWluZXJSZWYgPSBwb3J0YWwudmlld0NvbnRhaW5lclJlZiAhPSBudWxsID9cbiAgICAgICAgcG9ydGFsLnZpZXdDb250YWluZXJSZWYgOlxuICAgICAgICB0aGlzLl92aWV3Q29udGFpbmVyUmVmO1xuXG4gICAgY29uc3QgcmVzb2x2ZXIgPSBwb3J0YWwuY29tcG9uZW50RmFjdG9yeVJlc29sdmVyIHx8IHRoaXMuX2NvbXBvbmVudEZhY3RvcnlSZXNvbHZlcjtcbiAgICBjb25zdCBjb21wb25lbnRGYWN0b3J5ID0gcmVzb2x2ZXIucmVzb2x2ZUNvbXBvbmVudEZhY3RvcnkocG9ydGFsLmNvbXBvbmVudCk7XG4gICAgY29uc3QgcmVmID0gdmlld0NvbnRhaW5lclJlZi5jcmVhdGVDb21wb25lbnQoXG4gICAgICAgIGNvbXBvbmVudEZhY3RvcnksIHZpZXdDb250YWluZXJSZWYubGVuZ3RoLFxuICAgICAgICBwb3J0YWwuaW5qZWN0b3IgfHwgdmlld0NvbnRhaW5lclJlZi5pbmplY3Rvcik7XG5cbiAgICAvLyBJZiB3ZSdyZSB1c2luZyBhIHZpZXcgY29udGFpbmVyIHRoYXQncyBkaWZmZXJlbnQgZnJvbSB0aGUgaW5qZWN0ZWQgb25lIChlLmcuIHdoZW4gdGhlIHBvcnRhbFxuICAgIC8vIHNwZWNpZmllcyBpdHMgb3duKSB3ZSBuZWVkIHRvIG1vdmUgdGhlIGNvbXBvbmVudCBpbnRvIHRoZSBvdXRsZXQsIG90aGVyd2lzZSBpdCdsbCBiZSByZW5kZXJlZFxuICAgIC8vIGluc2lkZSBvZiB0aGUgYWx0ZXJuYXRlIHZpZXcgY29udGFpbmVyLlxuICAgIGlmICh2aWV3Q29udGFpbmVyUmVmICE9PSB0aGlzLl92aWV3Q29udGFpbmVyUmVmKSB7XG4gICAgICB0aGlzLl9nZXRSb290Tm9kZSgpLmFwcGVuZENoaWxkKChyZWYuaG9zdFZpZXcgYXMgRW1iZWRkZWRWaWV3UmVmPGFueT4pLnJvb3ROb2Rlc1swXSk7XG4gICAgfVxuXG4gICAgc3VwZXIuc2V0RGlzcG9zZUZuKCgpID0+IHJlZi5kZXN0cm95KCkpO1xuICAgIHRoaXMuX2F0dGFjaGVkUG9ydGFsID0gcG9ydGFsO1xuICAgIHRoaXMuX2F0dGFjaGVkUmVmID0gcmVmO1xuICAgIHRoaXMuYXR0YWNoZWQuZW1pdChyZWYpO1xuXG4gICAgcmV0dXJuIHJlZjtcbiAgfVxuXG4gIC8qKlxuICAgKiBBdHRhY2ggdGhlIGdpdmVuIFRlbXBsYXRlUG9ydGFsIHRvIHRoaXMgUG9ydGFsSG9zdCBhcyBhbiBlbWJlZGRlZCBWaWV3LlxuICAgKiBAcGFyYW0gcG9ydGFsIFBvcnRhbCB0byBiZSBhdHRhY2hlZC5cbiAgICogQHJldHVybnMgUmVmZXJlbmNlIHRvIHRoZSBjcmVhdGVkIGVtYmVkZGVkIHZpZXcuXG4gICAqL1xuICBhdHRhY2hUZW1wbGF0ZVBvcnRhbDxDPihwb3J0YWw6IFRlbXBsYXRlUG9ydGFsPEM+KTogRW1iZWRkZWRWaWV3UmVmPEM+IHtcbiAgICBwb3J0YWwuc2V0QXR0YWNoZWRIb3N0KHRoaXMpO1xuICAgIGNvbnN0IHZpZXdSZWYgPSB0aGlzLl92aWV3Q29udGFpbmVyUmVmLmNyZWF0ZUVtYmVkZGVkVmlldyhwb3J0YWwudGVtcGxhdGVSZWYsIHBvcnRhbC5jb250ZXh0KTtcbiAgICBzdXBlci5zZXREaXNwb3NlRm4oKCkgPT4gdGhpcy5fdmlld0NvbnRhaW5lclJlZi5jbGVhcigpKTtcblxuICAgIHRoaXMuX2F0dGFjaGVkUG9ydGFsID0gcG9ydGFsO1xuICAgIHRoaXMuX2F0dGFjaGVkUmVmID0gdmlld1JlZjtcbiAgICB0aGlzLmF0dGFjaGVkLmVtaXQodmlld1JlZik7XG5cbiAgICByZXR1cm4gdmlld1JlZjtcbiAgfVxuXG4gIC8qKlxuICAgKiBBdHRhY2hlcyB0aGUgZ2l2ZW4gRG9tUG9ydGFsIHRvIHRoaXMgUG9ydGFsSG9zdCBieSBtb3ZpbmcgYWxsIG9mIHRoZSBwb3J0YWwgY29udGVudCBpbnRvIGl0LlxuICAgKiBAcGFyYW0gcG9ydGFsIFBvcnRhbCB0byBiZSBhdHRhY2hlZC5cbiAgICogQGRlcHJlY2F0ZWQgVG8gYmUgdHVybmVkIGludG8gYSBtZXRob2QuXG4gICAqIEBicmVha2luZy1jaGFuZ2UgMTAuMC4wXG4gICAqL1xuICBhdHRhY2hEb21Qb3J0YWwgPSAocG9ydGFsOiBEb21Qb3J0YWwpID0+IHtcbiAgICAvLyBAYnJlYWtpbmctY2hhbmdlIDkuMC4wIFJlbW92ZSBjaGVjayBhbmQgZXJyb3Igb25jZSB0aGVcbiAgICAvLyBgX2RvY3VtZW50YCBjb25zdHJ1Y3RvciBwYXJhbWV0ZXIgaXMgcmVxdWlyZWQuXG4gICAgaWYgKCF0aGlzLl9kb2N1bWVudCkge1xuICAgICAgdGhyb3cgRXJyb3IoJ0Nhbm5vdCBhdHRhY2ggRE9NIHBvcnRhbCB3aXRob3V0IF9kb2N1bWVudCBjb25zdHJ1Y3RvciBwYXJhbWV0ZXInKTtcbiAgICB9XG5cbiAgICBjb25zdCBlbGVtZW50ID0gcG9ydGFsLmVsZW1lbnQ7XG4gICAgaWYgKCFlbGVtZW50LnBhcmVudE5vZGUpIHtcbiAgICAgIHRocm93IEVycm9yKCdET00gcG9ydGFsIGNvbnRlbnQgbXVzdCBiZSBhdHRhY2hlZCB0byBhIHBhcmVudCBub2RlLicpO1xuICAgIH1cblxuICAgIC8vIEFuY2hvciB1c2VkIHRvIHNhdmUgdGhlIGVsZW1lbnQncyBwcmV2aW91cyBwb3NpdGlvbiBzb1xuICAgIC8vIHRoYXQgd2UgY2FuIHJlc3RvcmUgaXQgd2hlbiB0aGUgcG9ydGFsIGlzIGRldGFjaGVkLlxuICAgIGNvbnN0IGFuY2hvck5vZGUgPSB0aGlzLl9kb2N1bWVudC5jcmVhdGVDb21tZW50KCdkb20tcG9ydGFsJyk7XG5cbiAgICBwb3J0YWwuc2V0QXR0YWNoZWRIb3N0KHRoaXMpO1xuICAgIGVsZW1lbnQucGFyZW50Tm9kZS5pbnNlcnRCZWZvcmUoYW5jaG9yTm9kZSwgZWxlbWVudCk7XG4gICAgdGhpcy5fZ2V0Um9vdE5vZGUoKS5hcHBlbmRDaGlsZChlbGVtZW50KTtcblxuICAgIHN1cGVyLnNldERpc3Bvc2VGbigoKSA9PiB7XG4gICAgICBpZiAoYW5jaG9yTm9kZS5wYXJlbnROb2RlKSB7XG4gICAgICAgIGFuY2hvck5vZGUucGFyZW50Tm9kZSEucmVwbGFjZUNoaWxkKGVsZW1lbnQsIGFuY2hvck5vZGUpO1xuICAgICAgfVxuICAgIH0pO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHJvb3Qgbm9kZSBvZiB0aGUgcG9ydGFsIG91dGxldC4gKi9cbiAgcHJpdmF0ZSBfZ2V0Um9vdE5vZGUoKTogSFRNTEVsZW1lbnQge1xuICAgIGNvbnN0IG5hdGl2ZUVsZW1lbnQ6IE5vZGUgPSB0aGlzLl92aWV3Q29udGFpbmVyUmVmLmVsZW1lbnQubmF0aXZlRWxlbWVudDtcblxuICAgIC8vIFRoZSBkaXJlY3RpdmUgY291bGQgYmUgc2V0IG9uIGEgdGVtcGxhdGUgd2hpY2ggd2lsbCByZXN1bHQgaW4gYSBjb21tZW50XG4gICAgLy8gbm9kZSBiZWluZyB0aGUgcm9vdC4gVXNlIHRoZSBjb21tZW50J3MgcGFyZW50IG5vZGUgaWYgdGhhdCBpcyB0aGUgY2FzZS5cbiAgICByZXR1cm4gKG5hdGl2ZUVsZW1lbnQubm9kZVR5cGUgPT09IG5hdGl2ZUVsZW1lbnQuRUxFTUVOVF9OT0RFID9cbiAgICAgICAgICAgbmF0aXZlRWxlbWVudCA6IG5hdGl2ZUVsZW1lbnQucGFyZW50Tm9kZSEpIGFzIEhUTUxFbGVtZW50O1xuICB9XG5cbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX3BvcnRhbDogUG9ydGFsPGFueT4gfCBudWxsIHwgdW5kZWZpbmVkIHwgJyc7XG59XG5cbi8qKlxuICogQGRlcHJlY2F0ZWQgVXNlIGBDZGtQb3J0YWxPdXRsZXRgIGluc3RlYWQuXG4gKiBAYnJlYWtpbmctY2hhbmdlIDkuMC4wXG4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogJ1tjZGtQb3J0YWxIb3N0XSwgW3BvcnRhbEhvc3RdJyxcbiAgZXhwb3J0QXM6ICdjZGtQb3J0YWxIb3N0JyxcbiAgaW5wdXRzOiBbJ3BvcnRhbDogY2RrUG9ydGFsSG9zdCddLFxuICBwcm92aWRlcnM6IFt7XG4gICAgcHJvdmlkZTogQ2RrUG9ydGFsT3V0bGV0LFxuICAgIHVzZUV4aXN0aW5nOiBQb3J0YWxIb3N0RGlyZWN0aXZlXG4gIH1dXG59KVxuZXhwb3J0IGNsYXNzIFBvcnRhbEhvc3REaXJlY3RpdmUgZXh0ZW5kcyBDZGtQb3J0YWxPdXRsZXQge31cblxuXG5ATmdNb2R1bGUoe1xuICBleHBvcnRzOiBbQ2RrUG9ydGFsLCBDZGtQb3J0YWxPdXRsZXQsIFRlbXBsYXRlUG9ydGFsRGlyZWN0aXZlLCBQb3J0YWxIb3N0RGlyZWN0aXZlXSxcbiAgZGVjbGFyYXRpb25zOiBbQ2RrUG9ydGFsLCBDZGtQb3J0YWxPdXRsZXQsIFRlbXBsYXRlUG9ydGFsRGlyZWN0aXZlLCBQb3J0YWxIb3N0RGlyZWN0aXZlXSxcbn0pXG5leHBvcnQgY2xhc3MgUG9ydGFsTW9kdWxlIHt9XG4iXX0=