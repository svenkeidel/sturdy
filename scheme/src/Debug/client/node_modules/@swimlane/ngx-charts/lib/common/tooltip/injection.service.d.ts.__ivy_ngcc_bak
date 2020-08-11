import { ApplicationRef, ComponentFactoryResolver, ComponentRef, Injector, ViewContainerRef, Type } from '@angular/core';
/**
 * Injection service is a helper to append components
 * dynamically to a known location in the DOM, most
 * noteably for dialogs/tooltips appending to body.
 *
 * @export
 */
export declare class InjectionService {
    private applicationRef;
    private componentFactoryResolver;
    private injector;
    static globalRootViewContainer: ViewContainerRef;
    /**
     * Sets a default global root view container. This is useful for
     * things like ngUpgrade that doesn't have a ApplicationRef root.
     *
     * @param container
     */
    static setGlobalRootViewContainer(container: ViewContainerRef): void;
    private _container;
    constructor(applicationRef: ApplicationRef, componentFactoryResolver: ComponentFactoryResolver, injector: Injector);
    /**
     * Gets the root view container to inject the component to.
     *
     * @memberOf InjectionService
     */
    getRootViewContainer(): ViewContainerRef | ComponentRef<any>;
    /**
     * Overrides the default root view container. This is useful for
     * things like ngUpgrade that doesn't have a ApplicationRef root.
     *
     * @param container
     *
     * @memberOf InjectionService
     */
    setRootViewContainer(container: ViewContainerRef): void;
    /**
     * Gets the html element for a component ref.
     *
     * @param componentRef
     *
     * @memberOf InjectionService
     */
    getComponentRootNode(component: ViewContainerRef | ComponentRef<any>): HTMLElement;
    /**
     * Gets the root component container html element.
     *
     * @memberOf InjectionService
     */
    getRootViewContainerNode(component: ViewContainerRef | ComponentRef<any>): HTMLElement;
    /**
     * Projects the bindings onto the component
     *
     * @param component
     * @param options
     *
     * @memberOf InjectionService
     */
    projectComponentBindings(component: ComponentRef<any>, bindings: any): ComponentRef<any>;
    /**
     * Appends a component to a adjacent location
     *
     * @param componentClass
     * @param [options={}]
     * @param [location]
     *
     * @memberOf InjectionService
     */
    appendComponent<T>(componentClass: Type<T>, bindings?: any, location?: any): ComponentRef<any>;
}
