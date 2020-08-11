/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Injector } from '../di/injector';
import { INJECTOR } from '../di/injector_compatibility';
import { InjectFlags } from '../di/interface/injector';
import { createInjectorWithoutInjectorInstances } from '../di/r3_injector';
import { ComponentFactoryResolver as viewEngine_ComponentFactoryResolver } from '../linker/component_factory_resolver';
import { NgModuleFactory as viewEngine_NgModuleFactory, NgModuleRef as viewEngine_NgModuleRef } from '../linker/ng_module_factory';
import { registerNgModuleType } from '../linker/ng_module_factory_registration';
import { assertDefined } from '../util/assert';
import { stringify } from '../util/stringify';
import { ComponentFactoryResolver } from './component_ref';
import { getNgLocaleIdDef, getNgModuleDef } from './definition';
import { setLocaleId } from './i18n';
import { maybeUnwrapFn } from './util/misc_utils';
export class NgModuleRef extends viewEngine_NgModuleRef {
    constructor(ngModuleType, _parent) {
        super();
        this._parent = _parent;
        // tslint:disable-next-line:require-internal-with-underscore
        this._bootstrapComponents = [];
        this.injector = this;
        this.destroyCbs = [];
        // When bootstrapping a module we have a dependency graph that looks like this:
        // ApplicationRef -> ComponentFactoryResolver -> NgModuleRef. The problem is that if the
        // module being resolved tries to inject the ComponentFactoryResolver, it'll create a
        // circular dependency which will result in a runtime error, because the injector doesn't
        // exist yet. We work around the issue by creating the ComponentFactoryResolver ourselves
        // and providing it, rather than letting the injector resolve it.
        this.componentFactoryResolver = new ComponentFactoryResolver(this);
        const ngModuleDef = getNgModuleDef(ngModuleType);
        ngDevMode &&
            assertDefined(ngModuleDef, `NgModule '${stringify(ngModuleType)}' is not a subtype of 'NgModuleType'.`);
        const ngLocaleIdDef = getNgLocaleIdDef(ngModuleType);
        ngLocaleIdDef && setLocaleId(ngLocaleIdDef);
        this._bootstrapComponents = maybeUnwrapFn(ngModuleDef.bootstrap);
        this._r3Injector = createInjectorWithoutInjectorInstances(ngModuleType, _parent, [
            { provide: viewEngine_NgModuleRef, useValue: this }, {
                provide: viewEngine_ComponentFactoryResolver,
                useValue: this.componentFactoryResolver
            }
        ], stringify(ngModuleType));
        // We need to resolve the injector types separately from the injector creation, because
        // the module might be trying to use this ref in its contructor for DI which will cause a
        // circular error that will eventually error out, because the injector isn't created yet.
        this._r3Injector._resolveInjectorDefTypes();
        this.instance = this.get(ngModuleType);
    }
    get(token, notFoundValue = Injector.THROW_IF_NOT_FOUND, injectFlags = InjectFlags.Default) {
        if (token === Injector || token === viewEngine_NgModuleRef || token === INJECTOR) {
            return this;
        }
        return this._r3Injector.get(token, notFoundValue, injectFlags);
    }
    destroy() {
        ngDevMode && assertDefined(this.destroyCbs, 'NgModule already destroyed');
        const injector = this._r3Injector;
        !injector.destroyed && injector.destroy();
        this.destroyCbs.forEach(fn => fn());
        this.destroyCbs = null;
    }
    onDestroy(callback) {
        ngDevMode && assertDefined(this.destroyCbs, 'NgModule already destroyed');
        this.destroyCbs.push(callback);
    }
}
export class NgModuleFactory extends viewEngine_NgModuleFactory {
    constructor(moduleType) {
        super();
        this.moduleType = moduleType;
        const ngModuleDef = getNgModuleDef(moduleType);
        if (ngModuleDef !== null) {
            // Register the NgModule with Angular's module registry. The location (and hence timing) of
            // this call is critical to ensure this works correctly (modules get registered when expected)
            // without bloating bundles (modules are registered when otherwise not referenced).
            //
            // In View Engine, registration occurs in the .ngfactory.js file as a side effect. This has
            // several practical consequences:
            //
            // - If an .ngfactory file is not imported from, the module won't be registered (and can be
            //   tree shaken).
            // - If an .ngfactory file is imported from, the module will be registered even if an instance
            //   is not actually created (via `create` below).
            // - Since an .ngfactory file in View Engine references the .ngfactory files of the NgModule's
            //   imports,
            //
            // In Ivy, things are a bit different. .ngfactory files still exist for compatibility, but are
            // not a required API to use - there are other ways to obtain an NgModuleFactory for a given
            // NgModule. Thus, relying on a side effect in the .ngfactory file is not sufficient. Instead,
            // the side effect of registration is added here, in the constructor of NgModuleFactory,
            // ensuring no matter how a factory is created, the module is registered correctly.
            //
            // An alternative would be to include the registration side effect inline following the actual
            // NgModule definition. This also has the correct timing, but breaks tree-shaking - modules
            // will be registered and retained even if they're otherwise never referenced.
            registerNgModuleType(moduleType);
        }
    }
    create(parentInjector) {
        return new NgModuleRef(this.moduleType, parentInjector);
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibmdfbW9kdWxlX3JlZi5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2NvcmUvc3JjL3JlbmRlcjMvbmdfbW9kdWxlX3JlZi50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsUUFBUSxFQUFDLE1BQU0sZ0JBQWdCLENBQUM7QUFDeEMsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLDhCQUE4QixDQUFDO0FBQ3RELE9BQU8sRUFBQyxXQUFXLEVBQUMsTUFBTSwwQkFBMEIsQ0FBQztBQUNyRCxPQUFPLEVBQUMsc0NBQXNDLEVBQWEsTUFBTSxtQkFBbUIsQ0FBQztBQUVyRixPQUFPLEVBQUMsd0JBQXdCLElBQUksbUNBQW1DLEVBQUMsTUFBTSxzQ0FBc0MsQ0FBQztBQUNySCxPQUFPLEVBQXNCLGVBQWUsSUFBSSwwQkFBMEIsRUFBRSxXQUFXLElBQUksc0JBQXNCLEVBQUMsTUFBTSw2QkFBNkIsQ0FBQztBQUN0SixPQUFPLEVBQUMsb0JBQW9CLEVBQUMsTUFBTSwwQ0FBMEMsQ0FBQztBQUU5RSxPQUFPLEVBQUMsYUFBYSxFQUFDLE1BQU0sZ0JBQWdCLENBQUM7QUFDN0MsT0FBTyxFQUFDLFNBQVMsRUFBQyxNQUFNLG1CQUFtQixDQUFDO0FBRTVDLE9BQU8sRUFBQyx3QkFBd0IsRUFBQyxNQUFNLGlCQUFpQixDQUFDO0FBQ3pELE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxjQUFjLEVBQUMsTUFBTSxjQUFjLENBQUM7QUFDOUQsT0FBTyxFQUFDLFdBQVcsRUFBQyxNQUFNLFFBQVEsQ0FBQztBQUNuQyxPQUFPLEVBQUMsYUFBYSxFQUFDLE1BQU0sbUJBQW1CLENBQUM7QUFNaEQsTUFBTSxPQUFPLFdBQWUsU0FBUSxzQkFBeUI7SUFpQjNELFlBQVksWUFBcUIsRUFBUyxPQUFzQjtRQUM5RCxLQUFLLEVBQUUsQ0FBQztRQURnQyxZQUFPLEdBQVAsT0FBTyxDQUFl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RUFBRTtZQUNGLDJGQUEyRjtZQUMzRixrQ0FBa0M7WUFDbEMsRUFBRTtZQUNGLDJGQUEyRjtZQUMzRixrQkFBa0I7WUFDbEIsOEZBQThGO1lBQzlGLGtEQUFrRDtZQUNsRCw4RkFBOEY7WUFDOUYsYUFBYTtZQUNiLEVBQUU7WUFDRiw4RkFBOEY7WUFDOUYsNEZBQTRGO1lBQzVGLDhGQUE4RjtZQUM5Rix3RkFBd0Y7WUFDeEYsbUZBQW1GO1lBQ25GLEVBQUU7WUFDRiw4RkFBOEY7WUFDOUYsMkZBQTJGO1lBQzNGLDhFQUE4RTtZQUM5RSxvQkFBb0IsQ0FBQyxVQUEwQixDQUFDLENBQUM7U0FDbEQ7SUFDSCxDQUFDO0lBRUQsTUFBTSxDQUFDLGNBQTZCO1FBQ2xDLE9BQU8sSUFBSSxXQUFXLENBQUMsSUFBSSxDQUFDLFVBQVUsRUFBRSxjQUFjLENBQUMsQ0FBQztJQUMxRCxDQUFDO0NBQ0YiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtJbmplY3Rvcn0gZnJvbSAnLi4vZGkvaW5qZWN0b3InO1xuaW1wb3J0IHtJTkpFQ1RPUn0gZnJvbSAnLi4vZGkvaW5qZWN0b3JfY29tcGF0aWJpbGl0eSc7XG5pbXBvcnQge0luamVjdEZsYWdzfSBmcm9tICcuLi9kaS9pbnRlcmZhY2UvaW5qZWN0b3InO1xuaW1wb3J0IHtjcmVhdGVJbmplY3RvcldpdGhvdXRJbmplY3Rvckluc3RhbmNlcywgUjNJbmplY3Rvcn0gZnJvbSAnLi4vZGkvcjNfaW5qZWN0b3InO1xuaW1wb3J0IHtUeXBlfSBmcm9tICcuLi9pbnRlcmZhY2UvdHlwZSc7XG5pbXBvcnQge0NvbXBvbmVudEZhY3RvcnlSZXNvbHZlciBhcyB2aWV3RW5naW5lX0NvbXBvbmVudEZhY3RvcnlSZXNvbHZlcn0gZnJvbSAnLi4vbGlua2VyL2NvbXBvbmVudF9mYWN0b3J5X3Jlc29sdmVyJztcbmltcG9ydCB7SW50ZXJuYWxOZ01vZHVsZVJlZiwgTmdNb2R1bGVGYWN0b3J5IGFzIHZpZXdFbmdpbmVfTmdNb2R1bGVGYWN0b3J5LCBOZ01vZHVsZVJlZiBhcyB2aWV3RW5naW5lX05nTW9kdWxlUmVmfSBmcm9tICcuLi9saW5rZXIvbmdfbW9kdWxlX2ZhY3RvcnknO1xuaW1wb3J0IHtyZWdpc3Rlck5nTW9kdWxlVHlwZX0gZnJvbSAnLi4vbGlua2VyL25nX21vZHVsZV9mYWN0b3J5X3JlZ2lzdHJhdGlvbic7XG5pbXBvcnQge05nTW9kdWxlRGVmfSBmcm9tICcuLi9tZXRhZGF0YS9uZ19tb2R1bGUnO1xuaW1wb3J0IHthc3NlcnREZWZpbmVkfSBmcm9tICcuLi91dGlsL2Fzc2VydCc7XG5pbXBvcnQge3N0cmluZ2lmeX0gZnJvbSAnLi4vdXRpbC9zdHJpbmdpZnknO1xuXG5pbXBvcnQge0NvbXBvbmVudEZhY3RvcnlSZXNvbHZlcn0gZnJvbSAnLi9jb21wb25lbnRfcmVmJztcbmltcG9ydCB7Z2V0TmdMb2NhbGVJZERlZiwgZ2V0TmdNb2R1bGVEZWZ9IGZyb20gJy4vZGVmaW5pdGlvbic7XG5pbXBvcnQge3NldExvY2FsZUlkfSBmcm9tICcuL2kxOG4nO1xuaW1wb3J0IHttYXliZVVud3JhcEZufSBmcm9tICcuL3V0aWwvbWlzY191dGlscyc7XG5cbmV4cG9ydCBpbnRlcmZhY2UgTmdNb2R1bGVUeXBlPFQgPSBhbnk+IGV4dGVuZHMgVHlwZTxUPiB7XG4gIMm1bW9kOiBOZ01vZHVsZURlZjxUPjtcbn1cblxuZXhwb3J0IGNsYXNzIE5nTW9kdWxlUmVmPFQ+IGV4dGVuZHMgdmlld0VuZ2luZV9OZ01vZHVsZVJlZjxUPiBpbXBsZW1lbnRzIEludGVybmFsTmdNb2R1bGVSZWY8VD4ge1xuICAvLyB0c2xpbnQ6ZGlzYWJsZS1uZXh0LWxpbmU6cmVxdWlyZS1pbnRlcm5hbC13aXRoLXVuZGVyc2NvcmVcbiAgX2Jvb3RzdHJhcENvbXBvbmVudHM6IFR5cGU8YW55PltdID0gW107XG4gIC8vIHRzbGludDpkaXNhYmxlLW5leHQtbGluZTpyZXF1aXJlLWludGVybmFsLXdpdGgtdW5kZXJzY29yZVxuICBfcjNJbmplY3RvcjogUjNJbmplY3RvcjtcbiAgaW5qZWN0b3I6IEluamVjdG9yID0gdGhpcztcbiAgaW5zdGFuY2U6IFQ7XG4gIGRlc3Ryb3lDYnM6ICgoKSA9PiB2b2lkKVtdfG51bGwgPSBbXTtcblxuICAvLyBXaGVuIGJvb3RzdHJhcHBpbmcgYSBtb2R1bGUgd2UgaGF2ZSBhIGRlcGVuZGVuY3kgZ3JhcGggdGhhdCBsb29rcyBsaWtlIHRoaXM6XG4gIC8vIEFwcGxpY2F0aW9uUmVmIC0+IENvbXBvbmVudEZhY3RvcnlSZXNvbHZlciAtPiBOZ01vZHVsZVJlZi4gVGhlIHByb2JsZW0gaXMgdGhhdCBpZiB0aGVcbiAgLy8gbW9kdWxlIGJlaW5nIHJlc29sdmVkIHRyaWVzIHRvIGluamVjdCB0aGUgQ29tcG9uZW50RmFjdG9yeVJlc29sdmVyLCBpdCdsbCBjcmVhdGUgYVxuICAvLyBjaXJjdWxhciBkZXBlbmRlbmN5IHdoaWNoIHdpbGwgcmVzdWx0IGluIGEgcnVudGltZSBlcnJvciwgYmVjYXVzZSB0aGUgaW5qZWN0b3IgZG9lc24ndFxuICAvLyBleGlzdCB5ZXQuIFdlIHdvcmsgYXJvdW5kIHRoZSBpc3N1ZSBieSBjcmVhdGluZyB0aGUgQ29tcG9uZW50RmFjdG9yeVJlc29sdmVyIG91cnNlbHZlc1xuICAvLyBhbmQgcHJvdmlkaW5nIGl0LCByYXRoZXIgdGhhbiBsZXR0aW5nIHRoZSBpbmplY3RvciByZXNvbHZlIGl0LlxuICByZWFkb25seSBjb21wb25lbnRGYWN0b3J5UmVzb2x2ZXI6IENvbXBvbmVudEZhY3RvcnlSZXNvbHZlciA9IG5ldyBDb21wb25lbnRGYWN0b3J5UmVzb2x2ZXIodGhpcyk7XG5cbiAgY29uc3RydWN0b3IobmdNb2R1bGVUeXBlOiBUeXBlPFQ+LCBwdWJsaWMgX3BhcmVudDogSW5qZWN0b3J8bnVsbCkge1xuICAgIHN1cGVyKCk7XG4gICAgY29uc3QgbmdNb2R1bGVEZWYgPSBnZXROZ01vZHVsZURlZihuZ01vZHVsZVR5cGUpO1xuICAgIG5nRGV2TW9kZSAmJlxuICAgICAgICBhc3NlcnREZWZpbmVkKFxuICAgICAgICAgICAgbmdNb2R1bGVEZWYsXG4gICAgICAgICAgICBgTmdNb2R1bGUgJyR7c3RyaW5naWZ5KG5nTW9kdWxlVHlwZSl9JyBpcyBub3QgYSBzdWJ0eXBlIG9mICdOZ01vZHVsZVR5cGUnLmApO1xuXG4gICAgY29uc3QgbmdMb2NhbGVJZERlZiA9IGdldE5nTG9jYWxlSWREZWYobmdNb2R1bGVUeXBlKTtcbiAgICBuZ0xvY2FsZUlkRGVmICYmIHNldExvY2FsZUlkKG5nTG9jYWxlSWREZWYpO1xuICAgIHRoaXMuX2Jvb3RzdHJhcENvbXBvbmVudHMgPSBtYXliZVVud3JhcEZuKG5nTW9kdWxlRGVmIS5ib290c3RyYXApO1xuICAgIHRoaXMuX3IzSW5qZWN0b3IgPSBjcmVhdGVJbmplY3RvcldpdGhvdXRJbmplY3Rvckluc3RhbmNlcyhcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIG5nTW9kdWxlVHlwZSwgX3BhcmVudCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAge3Byb3ZpZGU6IHZpZXdFbmdpbmVfTmdNb2R1bGVSZWYsIHVzZVZhbHVlOiB0aGlzfSwge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHByb3ZpZGU6IHZpZXdFbmdpbmVfQ29tcG9uZW50RmFjdG9yeVJlc29sdmVyLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHVzZVZhbHVlOiB0aGlzLmNvbXBvbmVudEZhY3RvcnlSZXNvbHZlclxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICBdLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgc3RyaW5naWZ5KG5nTW9kdWxlVHlwZSkpIGFzIFIzSW5qZWN0b3I7XG5cbiAgICAvLyBXZSBuZWVkIHRvIHJlc29sdmUgdGhlIGluamVjdG9yIHR5cGVzIHNlcGFyYXRlbHkgZnJvbSB0aGUgaW5qZWN0b3IgY3JlYXRpb24sIGJlY2F1c2VcbiAgICAvLyB0aGUgbW9kdWxlIG1pZ2h0IGJlIHRyeWluZyB0byB1c2UgdGhpcyByZWYgaW4gaXRzIGNvbnRydWN0b3IgZm9yIERJIHdoaWNoIHdpbGwgY2F1c2UgYVxuICAgIC8vIGNpcmN1bGFyIGVycm9yIHRoYXQgd2lsbCBldmVudHVhbGx5IGVycm9yIG91dCwgYmVjYXVzZSB0aGUgaW5qZWN0b3IgaXNuJ3QgY3JlYXRlZCB5ZXQuXG4gICAgdGhpcy5fcjNJbmplY3Rvci5fcmVzb2x2ZUluamVjdG9yRGVmVHlwZXMoKTtcbiAgICB0aGlzLmluc3RhbmNlID0gdGhpcy5nZXQobmdNb2R1bGVUeXBlKTtcbiAgfVxuXG4gIGdldCh0b2tlbjogYW55LCBub3RGb3VuZFZhbHVlOiBhbnkgPSBJbmplY3Rvci5USFJPV19JRl9OT1RfRk9VTkQsXG4gICAgICBpbmplY3RGbGFnczogSW5qZWN0RmxhZ3MgPSBJbmplY3RGbGFncy5EZWZhdWx0KTogYW55IHtcbiAgICBpZiAodG9rZW4gPT09IEluamVjdG9yIHx8IHRva2VuID09PSB2aWV3RW5naW5lX05nTW9kdWxlUmVmIHx8IHRva2VuID09PSBJTkpFQ1RPUikge1xuICAgICAgcmV0dXJuIHRoaXM7XG4gICAgfVxuICAgIHJldHVybiB0aGlzLl9yM0luamVjdG9yLmdldCh0b2tlbiwgbm90Rm91bmRWYWx1ZSwgaW5qZWN0RmxhZ3MpO1xuICB9XG5cbiAgZGVzdHJveSgpOiB2b2lkIHtcbiAgICBuZ0Rldk1vZGUgJiYgYXNzZXJ0RGVmaW5lZCh0aGlzLmRlc3Ryb3lDYnMsICdOZ01vZHVsZSBhbHJlYWR5IGRlc3Ryb3llZCcpO1xuICAgIGNvbnN0IGluamVjdG9yID0gdGhpcy5fcjNJbmplY3RvcjtcbiAgICAhaW5qZWN0b3IuZGVzdHJveWVkICYmIGluamVjdG9yLmRlc3Ryb3koKTtcbiAgICB0aGlzLmRlc3Ryb3lDYnMhLmZvckVhY2goZm4gPT4gZm4oKSk7XG4gICAgdGhpcy5kZXN0cm95Q2JzID0gbnVsbDtcbiAgfVxuICBvbkRlc3Ryb3koY2FsbGJhY2s6ICgpID0+IHZvaWQpOiB2b2lkIHtcbiAgICBuZ0Rldk1vZGUgJiYgYXNzZXJ0RGVmaW5lZCh0aGlzLmRlc3Ryb3lDYnMsICdOZ01vZHVsZSBhbHJlYWR5IGRlc3Ryb3llZCcpO1xuICAgIHRoaXMuZGVzdHJveUNicyEucHVzaChjYWxsYmFjayk7XG4gIH1cbn1cblxuZXhwb3J0IGNsYXNzIE5nTW9kdWxlRmFjdG9yeTxUPiBleHRlbmRzIHZpZXdFbmdpbmVfTmdNb2R1bGVGYWN0b3J5PFQ+IHtcbiAgY29uc3RydWN0b3IocHVibGljIG1vZHVsZVR5cGU6IFR5cGU8VD4pIHtcbiAgICBzdXBlcigpO1xuXG4gICAgY29uc3QgbmdNb2R1bGVEZWYgPSBnZXROZ01vZHVsZURlZihtb2R1bGVUeXBlKTtcbiAgICBpZiAobmdNb2R1bGVEZWYgIT09IG51bGwpIHtcbiAgICAgIC8vIFJlZ2lzdGVyIHRoZSBOZ01vZHVsZSB3aXRoIEFuZ3VsYXIncyBtb2R1bGUgcmVnaXN0cnkuIFRoZSBsb2NhdGlvbiAoYW5kIGhlbmNlIHRpbWluZykgb2ZcbiAgICAgIC8vIHRoaXMgY2FsbCBpcyBjcml0aWNhbCB0byBlbnN1cmUgdGhpcyB3b3JrcyBjb3JyZWN0bHkgKG1vZHVsZXMgZ2V0IHJlZ2lzdGVyZWQgd2hlbiBleHBlY3RlZClcbiAgICAgIC8vIHdpdGhvdXQgYmxvYXRpbmcgYnVuZGxlcyAobW9kdWxlcyBhcmUgcmVnaXN0ZXJlZCB3aGVuIG90aGVyd2lzZSBub3QgcmVmZXJlbmNlZCkuXG4gICAgICAvL1xuICAgICAgLy8gSW4gVmlldyBFbmdpbmUsIHJlZ2lzdHJhdGlvbiBvY2N1cnMgaW4gdGhlIC5uZ2ZhY3RvcnkuanMgZmlsZSBhcyBhIHNpZGUgZWZmZWN0LiBUaGlzIGhhc1xuICAgICAgLy8gc2V2ZXJhbCBwcmFjdGljYWwgY29uc2VxdWVuY2VzOlxuICAgICAgLy9cbiAgICAgIC8vIC0gSWYgYW4gLm5nZmFjdG9yeSBmaWxlIGlzIG5vdCBpbXBvcnRlZCBmcm9tLCB0aGUgbW9kdWxlIHdvbid0IGJlIHJlZ2lzdGVyZWQgKGFuZCBjYW4gYmVcbiAgICAgIC8vICAgdHJlZSBzaGFrZW4pLlxuICAgICAgLy8gLSBJZiBhbiAubmdmYWN0b3J5IGZpbGUgaXMgaW1wb3J0ZWQgZnJvbSwgdGhlIG1vZHVsZSB3aWxsIGJlIHJlZ2lzdGVyZWQgZXZlbiBpZiBhbiBpbnN0YW5jZVxuICAgICAgLy8gICBpcyBub3QgYWN0dWFsbHkgY3JlYXRlZCAodmlhIGBjcmVhdGVgIGJlbG93KS5cbiAgICAgIC8vIC0gU2luY2UgYW4gLm5nZmFjdG9yeSBmaWxlIGluIFZpZXcgRW5naW5lIHJlZmVyZW5jZXMgdGhlIC5uZ2ZhY3RvcnkgZmlsZXMgb2YgdGhlIE5nTW9kdWxlJ3NcbiAgICAgIC8vICAgaW1wb3J0cyxcbiAgICAgIC8vXG4gICAgICAvLyBJbiBJdnksIHRoaW5ncyBhcmUgYSBiaXQgZGlmZmVyZW50LiAubmdmYWN0b3J5IGZpbGVzIHN0aWxsIGV4aXN0IGZvciBjb21wYXRpYmlsaXR5LCBidXQgYXJlXG4gICAgICAvLyBub3QgYSByZXF1aXJlZCBBUEkgdG8gdXNlIC0gdGhlcmUgYXJlIG90aGVyIHdheXMgdG8gb2J0YWluIGFuIE5nTW9kdWxlRmFjdG9yeSBmb3IgYSBnaXZlblxuICAgICAgLy8gTmdNb2R1bGUuIFRodXMsIHJlbHlpbmcgb24gYSBzaWRlIGVmZmVjdCBpbiB0aGUgLm5nZmFjdG9yeSBmaWxlIGlzIG5vdCBzdWZmaWNpZW50LiBJbnN0ZWFkLFxuICAgICAgLy8gdGhlIHNpZGUgZWZmZWN0IG9mIHJlZ2lzdHJhdGlvbiBpcyBhZGRlZCBoZXJlLCBpbiB0aGUgY29uc3RydWN0b3Igb2YgTmdNb2R1bGVGYWN0b3J5LFxuICAgICAgLy8gZW5zdXJpbmcgbm8gbWF0dGVyIGhvdyBhIGZhY3RvcnkgaXMgY3JlYXRlZCwgdGhlIG1vZHVsZSBpcyByZWdpc3RlcmVkIGNvcnJlY3RseS5cbiAgICAgIC8vXG4gICAgICAvLyBBbiBhbHRlcm5hdGl2ZSB3b3VsZCBiZSB0byBpbmNsdWRlIHRoZSByZWdpc3RyYXRpb24gc2lkZSBlZmZlY3QgaW5saW5lIGZvbGxvd2luZyB0aGUgYWN0dWFsXG4gICAgICAvLyBOZ01vZHVsZSBkZWZpbml0aW9uLiBUaGlzIGFsc28gaGFzIHRoZSBjb3JyZWN0IHRpbWluZywgYnV0IGJyZWFrcyB0cmVlLXNoYWtpbmcgLSBtb2R1bGVzXG4gICAgICAvLyB3aWxsIGJlIHJlZ2lzdGVyZWQgYW5kIHJldGFpbmVkIGV2ZW4gaWYgdGhleSdyZSBvdGhlcndpc2UgbmV2ZXIgcmVmZXJlbmNlZC5cbiAgICAgIHJlZ2lzdGVyTmdNb2R1bGVUeXBlKG1vZHVsZVR5cGUgYXMgTmdNb2R1bGVUeXBlKTtcbiAgICB9XG4gIH1cblxuICBjcmVhdGUocGFyZW50SW5qZWN0b3I6IEluamVjdG9yfG51bGwpOiB2aWV3RW5naW5lX05nTW9kdWxlUmVmPFQ+IHtcbiAgICByZXR1cm4gbmV3IE5nTW9kdWxlUmVmKHRoaXMubW9kdWxlVHlwZSwgcGFyZW50SW5qZWN0b3IpO1xuICB9XG59XG4iXX0=