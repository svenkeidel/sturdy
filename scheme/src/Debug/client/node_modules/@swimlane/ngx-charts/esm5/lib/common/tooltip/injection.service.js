import { __decorate, __values } from "tslib";
import { ApplicationRef, ComponentFactoryResolver, ComponentRef, Injectable, Injector, ViewContainerRef, EmbeddedViewRef, Type } from '@angular/core';
import { DomPortalHost, ComponentPortal } from '@angular/cdk/portal';
function isViewContainerRef(x) {
    return x.element;
}
/**
 * Injection service is a helper to append components
 * dynamically to a known location in the DOM, most
 * noteably for dialogs/tooltips appending to body.
 *
 * @export
 */
var InjectionService = /** @class */ (function () {
    function InjectionService(applicationRef, componentFactoryResolver, injector) {
        this.applicationRef = applicationRef;
        this.componentFactoryResolver = componentFactoryResolver;
        this.injector = injector;
    }
    InjectionService_1 = InjectionService;
    /**
     * Sets a default global root view container. This is useful for
     * things like ngUpgrade that doesn't have a ApplicationRef root.
     *
     * @param container
     */
    InjectionService.setGlobalRootViewContainer = function (container) {
        InjectionService_1.globalRootViewContainer = container;
    };
    /**
     * Gets the root view container to inject the component to.
     *
     * @memberOf InjectionService
     */
    InjectionService.prototype.getRootViewContainer = function () {
        if (this._container)
            return this._container;
        if (InjectionService_1.globalRootViewContainer)
            return InjectionService_1.globalRootViewContainer;
        if (this.applicationRef.components.length)
            return this.applicationRef.components[0];
        throw new Error('View Container not found! ngUpgrade needs to manually set this via setRootViewContainer or setGlobalRootViewContainer.');
    };
    /**
     * Overrides the default root view container. This is useful for
     * things like ngUpgrade that doesn't have a ApplicationRef root.
     *
     * @param container
     *
     * @memberOf InjectionService
     */
    InjectionService.prototype.setRootViewContainer = function (container) {
        this._container = container;
    };
    /**
     * Gets the html element for a component ref.
     *
     * @param componentRef
     *
     * @memberOf InjectionService
     */
    InjectionService.prototype.getComponentRootNode = function (component) {
        if (isViewContainerRef(component)) {
            return component.element.nativeElement;
        }
        if (component.hostView && component.hostView.rootNodes.length > 0) {
            return component.hostView.rootNodes[0];
        }
        // the top most component root node has no `hostView`
        return component.location.nativeElement;
    };
    /**
     * Gets the root component container html element.
     *
     * @memberOf InjectionService
     */
    InjectionService.prototype.getRootViewContainerNode = function (component) {
        return this.getComponentRootNode(component);
    };
    /**
     * Projects the bindings onto the component
     *
     * @param component
     * @param options
     *
     * @memberOf InjectionService
     */
    InjectionService.prototype.projectComponentBindings = function (component, bindings) {
        var e_1, _a, e_2, _b;
        if (bindings) {
            if (bindings.inputs !== undefined) {
                var bindingKeys = Object.getOwnPropertyNames(bindings.inputs);
                try {
                    for (var bindingKeys_1 = __values(bindingKeys), bindingKeys_1_1 = bindingKeys_1.next(); !bindingKeys_1_1.done; bindingKeys_1_1 = bindingKeys_1.next()) {
                        var bindingName = bindingKeys_1_1.value;
                        component.instance[bindingName] = bindings.inputs[bindingName];
                    }
                }
                catch (e_1_1) { e_1 = { error: e_1_1 }; }
                finally {
                    try {
                        if (bindingKeys_1_1 && !bindingKeys_1_1.done && (_a = bindingKeys_1.return)) _a.call(bindingKeys_1);
                    }
                    finally { if (e_1) throw e_1.error; }
                }
            }
            if (bindings.outputs !== undefined) {
                var eventKeys = Object.getOwnPropertyNames(bindings.outputs);
                try {
                    for (var eventKeys_1 = __values(eventKeys), eventKeys_1_1 = eventKeys_1.next(); !eventKeys_1_1.done; eventKeys_1_1 = eventKeys_1.next()) {
                        var eventName = eventKeys_1_1.value;
                        component.instance[eventName] = bindings.outputs[eventName];
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (eventKeys_1_1 && !eventKeys_1_1.done && (_b = eventKeys_1.return)) _b.call(eventKeys_1);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
            }
        }
        return component;
    };
    /**
     * Appends a component to a adjacent location
     *
     * @param componentClass
     * @param [options={}]
     * @param [location]
     *
     * @memberOf InjectionService
     */
    InjectionService.prototype.appendComponent = function (componentClass, bindings, location) {
        if (bindings === void 0) { bindings = {}; }
        if (!location)
            location = this.getRootViewContainer();
        var appendLocation = this.getComponentRootNode(location);
        var portalHost = new DomPortalHost(appendLocation, this.componentFactoryResolver, this.applicationRef, this.injector);
        var portal = new ComponentPortal(componentClass);
        var componentRef = portalHost.attach(portal);
        this.projectComponentBindings(componentRef, bindings);
        return componentRef;
    };
    var InjectionService_1;
    InjectionService.globalRootViewContainer = null;
    InjectionService.ctorParameters = function () { return [
        { type: ApplicationRef },
        { type: ComponentFactoryResolver },
        { type: Injector }
    ]; };
    InjectionService = InjectionService_1 = __decorate([
        Injectable()
    ], InjectionService);
    return InjectionService;
}());
export { InjectionService };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5qZWN0aW9uLnNlcnZpY2UuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vdG9vbHRpcC9pbmplY3Rpb24uc2VydmljZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUNMLGNBQWMsRUFDZCx3QkFBd0IsRUFDeEIsWUFBWSxFQUNaLFVBQVUsRUFDVixRQUFRLEVBQ1IsZ0JBQWdCLEVBQ2hCLGVBQWUsRUFDZixJQUFJLEVBQ0wsTUFBTSxlQUFlLENBQUM7QUFDdkIsT0FBTyxFQUFFLGFBQWEsRUFBRSxlQUFlLEVBQUUsTUFBTSxxQkFBcUIsQ0FBQztBQUVyRSxTQUFTLGtCQUFrQixDQUFDLENBQU07SUFDaEMsT0FBTyxDQUFDLENBQUMsT0FBTyxDQUFDO0FBQ25CLENBQUM7QUFFRDs7Ozs7O0dBTUc7QUFFSDtJQWVFLDBCQUNVLGNBQThCLEVBQzlCLHdCQUFrRCxFQUNsRCxRQUFrQjtRQUZsQixtQkFBYyxHQUFkLGNBQWMsQ0FBZ0I7UUFDOUIsNkJBQXdCLEdBQXhCLHdCQUF3QixDQUEwQjtRQUNsRCxhQUFRLEdBQVIsUUFBUSxDQUFVO0lBQ3pCLENBQUM7eUJBbkJPLGdCQUFnQjtJQUczQjs7Ozs7T0FLRztJQUNJLDJDQUEwQixHQUFqQyxVQUFrQyxTQUEyQjtRQUMzRCxrQkFBZ0IsQ0FBQyx1QkFBdUIsR0FBRyxTQUFTLENBQUM7SUFDdkQsQ0FBQztJQVVEOzs7O09BSUc7SUFDSCwrQ0FBb0IsR0FBcEI7UUFDRSxJQUFJLElBQUksQ0FBQyxVQUFVO1lBQUUsT0FBTyxJQUFJLENBQUMsVUFBVSxDQUFDO1FBQzVDLElBQUksa0JBQWdCLENBQUMsdUJBQXVCO1lBQUUsT0FBTyxrQkFBZ0IsQ0FBQyx1QkFBdUIsQ0FBQztRQUU5RixJQUFJLElBQUksQ0FBQyxjQUFjLENBQUMsVUFBVSxDQUFDLE1BQU07WUFBRSxPQUFPLElBQUksQ0FBQyxjQUFjLENBQUMsVUFB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Qzs7SUFqSU0sd0NBQXVCLEdBQXFCLElBQUksQ0FBQzs7Z0JBZTlCLGNBQWM7Z0JBQ0osd0JBQXdCO2dCQUN4QyxRQUFROztJQWxCakIsZ0JBQWdCO1FBRDVCLFVBQVUsRUFBRTtPQUNBLGdCQUFnQixDQW1JNUI7SUFBRCx1QkFBQztDQUFBLEFBbklELElBbUlDO1NBbklZLGdCQUFnQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIEFwcGxpY2F0aW9uUmVmLFxuICBDb21wb25lbnRGYWN0b3J5UmVzb2x2ZXIsXG4gIENvbXBvbmVudFJlZixcbiAgSW5qZWN0YWJsZSxcbiAgSW5qZWN0b3IsXG4gIFZpZXdDb250YWluZXJSZWYsXG4gIEVtYmVkZGVkVmlld1JlZixcbiAgVHlwZVxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IERvbVBvcnRhbEhvc3QsIENvbXBvbmVudFBvcnRhbCB9IGZyb20gJ0Bhbmd1bGFyL2Nkay9wb3J0YWwnO1xuXG5mdW5jdGlvbiBpc1ZpZXdDb250YWluZXJSZWYoeDogYW55KTogeCBpcyBWaWV3Q29udGFpbmVyUmVmIHtcbiAgcmV0dXJuIHguZWxlbWVudDtcbn1cblxuLyoqXG4gKiBJbmplY3Rpb24gc2VydmljZSBpcyBhIGhlbHBlciB0byBhcHBlbmQgY29tcG9uZW50c1xuICogZHluYW1pY2FsbHkgdG8gYSBrbm93biBsb2NhdGlvbiBpbiB0aGUgRE9NLCBtb3N0XG4gKiBub3RlYWJseSBmb3IgZGlhbG9ncy90b29sdGlwcyBhcHBlbmRpbmcgdG8gYm9keS5cbiAqXG4gKiBAZXhwb3J0XG4gKi9cbkBJbmplY3RhYmxlKClcbmV4cG9ydCBjbGFzcyBJbmplY3Rpb25TZXJ2aWNlIHtcbiAgc3RhdGljIGdsb2JhbFJvb3RWaWV3Q29udGFpbmVyOiBWaWV3Q29udGFpbmVyUmVmID0gbnVsbDtcblxuICAvKipcbiAgICogU2V0cyBhIGRlZmF1bHQgZ2xvYmFsIHJvb3QgdmlldyBjb250YWluZXIuIFRoaXMgaXMgdXNlZnVsIGZvclxuICAgKiB0aGluZ3MgbGlrZSBuZ1VwZ3JhZGUgdGhhdCBkb2Vzbid0IGhhdmUgYSBBcHBsaWNhdGlvblJlZiByb290LlxuICAgKlxuICAgKiBAcGFyYW0gY29udGFpbmVyXG4gICAqL1xuICBzdGF0aWMgc2V0R2xvYmFsUm9vdFZpZXdDb250YWluZXIoY29udGFpbmVyOiBWaWV3Q29udGFpbmVyUmVmKTogdm9pZCB7XG4gICAgSW5qZWN0aW9uU2VydmljZS5nbG9iYWxSb290Vmlld0NvbnRhaW5lciA9IGNvbnRhaW5lcjtcbiAgfVxuXG4gIHByaXZhdGUgX2NvbnRhaW5lcjogVmlld0NvbnRhaW5lclJlZjtcblxuICBjb25zdHJ1Y3RvcihcbiAgICBwcml2YXRlIGFwcGxpY2F0aW9uUmVmOiBBcHBsaWNhdGlvblJlZixcbiAgICBwcml2YXRlIGNvbXBvbmVudEZhY3RvcnlSZXNvbHZlcjogQ29tcG9uZW50RmFjdG9yeVJlc29sdmVyLFxuICAgIHByaXZhdGUgaW5qZWN0b3I6IEluamVjdG9yXG4gICkge31cblxuICAvKipcbiAgICogR2V0cyB0aGUgcm9vdCB2aWV3IGNvbnRhaW5lciB0byBpbmplY3QgdGhlIGNvbXBvbmVudCB0by5cbiAgICpcbiAgICogQG1lbWJlck9mIEluamVjdGlvblNlcnZpY2VcbiAgICovXG4gIGdldFJvb3RWaWV3Q29udGFpbmVyKCk6IFZpZXdDb250YWluZXJSZWYgfCBDb21wb25lbnRSZWY8YW55PiB7XG4gICAgaWYgKHRoaXMuX2NvbnRhaW5lcikgcmV0dXJuIHRoaXMuX2NvbnRhaW5lcjtcbiAgICBpZiAoSW5qZWN0aW9uU2VydmljZS5nbG9iYWxSb290Vmlld0NvbnRhaW5lcikgcmV0dXJuIEluamVjdGlvblNlcnZpY2UuZ2xvYmFsUm9vdFZpZXdDb250YWluZXI7XG5cbiAgICBpZiAodGhpcy5hcHBsaWNhdGlvblJlZi5jb21wb25lbnRzLmxlbmd0aCkgcmV0dXJuIHRoaXMuYXBwbGljYXRpb25SZWYuY29tcG9uZW50c1swXTtcblxuICAgIHRocm93IG5ldyBFcnJvcihcbiAgICAgICdWaWV3IENvbnRhaW5lciBub3QgZm91bmQhIG5nVXBncmFkZSBuZWVkcyB0byBtYW51YWxseSBzZXQgdGhpcyB2aWEgc2V0Um9vdFZpZXdDb250YWluZXIgb3Igc2V0R2xvYmFsUm9vdFZpZXdDb250YWluZXIuJ1xuICAgICk7XG4gIH1cblxuICAvKipcbiAgICogT3ZlcnJpZGVzIHRoZSBkZWZhdWx0IHJvb3QgdmlldyBjb250YWluZXIuIFRoaXMgaXMgdXNlZnVsIGZvclxuICAgKiB0aGluZ3MgbGlrZSBuZ1VwZ3JhZGUgdGhhdCBkb2Vzbid0IGhhdmUgYSBBcHBsaWNhdGlvblJlZiByb290LlxuICAgKlxuICAgKiBAcGFyYW0gY29udGFpbmVyXG4gICAqXG4gICAqIEBtZW1iZXJPZiBJbmplY3Rpb25TZXJ2aWNlXG4gICAqL1xuICBzZXRSb290Vmlld0NvbnRhaW5lcihjb250YWluZXI6IFZpZXdDb250YWluZXJSZWYpOiB2b2lkIHtcbiAgICB0aGlzLl9jb250YWluZXIgPSBjb250YWluZXI7XG4gIH1cblxuICAvKipcbiAgICogR2V0cyB0aGUgaHRtbCBlbGVtZW50IGZvciBhIGNvbXBvbmVudCByZWYuXG4gICAqXG4gICAqIEBwYXJhbSBjb21wb25lbnRSZWZcbiAgICpcbiAgICogQG1lbWJlck9mIEluamVjdGlvblNlcnZpY2VcbiAgICovXG4gIGdldENvbXBvbmVudFJvb3ROb2RlKGNvbXBvbmVudDogVmlld0NvbnRhaW5lclJlZiB8IENvbXBvbmVudFJlZjxhbnk+KTogSFRNTEVsZW1lbnQge1xuICAgIGlmIChpc1ZpZXdDb250YWluZXJSZWYoY29tcG9uZW50KSkge1xuICAgICAgcmV0dXJuIGNvbXBvbmVudC5lbGVtZW50Lm5hdGl2ZUVsZW1lbnQ7XG4gICAgfVxuICAgIGlmIChjb21wb25lbnQuaG9zdFZpZXcgJiYgKGNvbXBvbmVudC5ob3N0VmlldyBhcyBFbWJlZGRlZFZpZXdSZWY8YW55Pikucm9vdE5vZGVzLmxlbmd0aCA+IDApIHtcbiAgICAgIHJldHVybiAoY29tcG9uZW50Lmhvc3RWaWV3IGFzIEVtYmVkZGVkVmlld1JlZjxhbnk+KS5yb290Tm9kZXNbMF0gYXMgSFRNTEVsZW1lbnQ7XG4gICAgfVxuXG4gICAgLy8gdGhlIHRvcCBtb3N0IGNvbXBvbmVudCByb290IG5vZGUgaGFzIG5vIGBob3N0Vmlld2BcbiAgICByZXR1cm4gY29tcG9uZW50LmxvY2F0aW9uLm5hdGl2ZUVsZW1lbnQ7XG4gIH1cblxuICAvKipcbiAgICogR2V0cyB0aGUgcm9vdCBjb21wb25lbnQgY29udGFpbmVyIGh0bWwgZWxlbWVudC5cbiAgICpcbiAgICogQG1lbWJlck9mIEluamVjdGlvblNlcnZpY2VcbiAgICovXG4gIGdldFJvb3RWaWV3Q29udGFpbmVyTm9kZShjb21wb25lbnQ6IFZpZXdDb250YWluZXJSZWYgfCBDb21wb25lbnRSZWY8YW55Pik6IEhUTUxFbGVtZW50IHtcbiAgICByZXR1cm4gdGhpcy5nZXRDb21wb25lbnRSb290Tm9kZShjb21wb25lbnQpO1xuICB9XG5cbiAgLyoqXG4gICAqIFByb2plY3RzIHRoZSBiaW5kaW5ncyBvbnRvIHRoZSBjb21wb25lbnRcbiAgICpcbiAgICogQHBhcmFtIGNvbXBvbmVudFxuICAgKiBAcGFyYW0gb3B0aW9uc1xuICAgKlxuICAgKiBAbWVtYmVyT2YgSW5qZWN0aW9uU2VydmljZVxuICAgKi9cbiAgcHJvamVjdENvbXBvbmVudEJpbmRpbmdzKGNvbXBvbmVudDogQ29tcG9uZW50UmVmPGFueT4sIGJpbmRpbmdzOiBhbnkpOiBDb21wb25lbnRSZWY8YW55PiB7XG4gICAgaWYgKGJpbmRpbmdzKSB7XG4gICAgICBpZiAoYmluZGluZ3MuaW5wdXRzICE9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgY29uc3QgYmluZGluZ0tleXMgPSBPYmplY3QuZ2V0T3duUHJvcGVydHlOYW1lcyhiaW5kaW5ncy5pbnB1dHMpO1xuICAgICAgICBmb3IgKGNvbnN0IGJpbmRpbmdOYW1lIG9mIGJpbmRpbmdLZXlzKSB7XG4gICAgICAgICAgY29tcG9uZW50Lmluc3RhbmNlW2JpbmRpbmdOYW1lXSA9IGJpbmRpbmdzLmlucHV0c1tiaW5kaW5nTmFtZV07XG4gICAgICAgIH1cbiAgICAgIH1cblxuICAgICAgaWYgKGJpbmRpbmdzLm91dHB1dHMgIT09IHVuZGVmaW5lZCkge1xuICAgICAgICBjb25zdCBldmVudEtleXMgPSBPYmplY3QuZ2V0T3duUHJvcGVydHlOYW1lcyhiaW5kaW5ncy5vdXRwdXRzKTtcbiAgICAgICAgZm9yIChjb25zdCBldmVudE5hbWUgb2YgZXZlbnRLZXlzKSB7XG4gICAgICAgICAgY29tcG9uZW50Lmluc3RhbmNlW2V2ZW50TmFtZV0gPSBiaW5kaW5ncy5vdXRwdXRzW2V2ZW50TmFtZV07XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gY29tcG9uZW50O1xuICB9XG5cbiAgLyoqXG4gICAqIEFwcGVuZHMgYSBjb21wb25lbnQgdG8gYSBhZGphY2VudCBsb2NhdGlvblxuICAgKlxuICAgKiBAcGFyYW0gY29tcG9uZW50Q2xhc3NcbiAgICogQHBhcmFtIFtvcHRpb25zPXt9XVxuICAgKiBAcGFyYW0gW2xvY2F0aW9uXVxuICAgKlxuICAgKiBAbWVtYmVyT2YgSW5qZWN0aW9uU2VydmljZVxuICAgKi9cbiAgYXBwZW5kQ29tcG9uZW50PFQ+KGNvbXBvbmVudENsYXNzOiBUeXBlPFQ+LCBiaW5kaW5nczogYW55ID0ge30sIGxvY2F0aW9uPzogYW55KTogQ29tcG9uZW50UmVmPGFueT4ge1xuICAgIGlmICghbG9jYXRpb24pIGxvY2F0aW9uID0gdGhpcy5nZXRSb290Vmlld0NvbnRhaW5lcigpO1xuICAgIGNvbnN0IGFwcGVuZExvY2F0aW9uID0gdGhpcy5nZXRDb21wb25lbnRSb290Tm9kZShsb2NhdGlvbik7XG5cbiAgICBjb25zdCBwb3J0YWxIb3N0ID0gbmV3IERvbVBvcnRhbEhvc3QoXG4gICAgICBhcHBlbmRMb2NhdGlvbixcbiAgICAgIHRoaXMuY29tcG9uZW50RmFjdG9yeVJlc29sdmVyLFxuICAgICAgdGhpcy5hcHBsaWNhdGlvblJlZixcbiAgICAgIHRoaXMuaW5qZWN0b3JcbiAgICApO1xuXG4gICAgY29uc3QgcG9ydGFsID0gbmV3IENvbXBvbmVudFBvcnRhbChjb21wb25lbnRDbGFzcyk7XG5cbiAgICBjb25zdCBjb21wb25lbnRSZWYgPSBwb3J0YWxIb3N0LmF0dGFjaChwb3J0YWwpO1xuICAgIHRoaXMucHJvamVjdENvbXBvbmVudEJpbmRpbmdzKGNvbXBvbmVudFJlZiwgYmluZGluZ3MpO1xuICAgIHJldHVybiBjb21wb25lbnRSZWY7XG4gIH1cbn1cbiJdfQ==