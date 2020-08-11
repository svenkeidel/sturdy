/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { NgModuleRef as viewEngine_NgModuleRef } from '../linker/ng_module_factory';
import { addToArray, removeFromArray } from '../util/array_utils';
import { assertDefined, assertEqual, assertGreaterThan, assertLessThan } from '../util/assert';
import { assertLContainer } from './assert';
import { getParentInjectorLocation, NodeInjector } from './di';
import { addToViewTree, createLContainer, createLView, renderView } from './instructions/shared';
import { CONTAINER_HEADER_OFFSET, VIEW_REFS } from './interfaces/container';
import { isProceduralRenderer } from './interfaces/renderer';
import { isComponentHost, isLContainer, isLView, isRootView } from './interfaces/type_checks';
import { DECLARATION_COMPONENT_VIEW, DECLARATION_LCONTAINER, PARENT, QUERIES, RENDERER, T_HOST, TVIEW } from './interfaces/view';
import { assertNodeOfPossibleTypes } from './node_assert';
import { addRemoveViewFromContainer, appendChild, detachView, getBeforeNodeForView, insertView, nativeInsertBefore, nativeNextSibling, nativeParentNode, removeView } from './node_manipulation';
import { getParentInjectorTNode } from './node_util';
import { getLView, getPreviousOrParentTNode } from './state';
import { getParentInjectorView, hasParentInjector } from './util/injector_utils';
import { getComponentLViewByIndex, getNativeByTNode, unwrapRNode, viewAttachedToContainer } from './util/view_utils';
import { ViewRef } from './view_ref';
/**
 * Creates an ElementRef from the most recent node.
 *
 * @returns The ElementRef instance to use
 */
export function injectElementRef(ElementRefToken) {
    return createElementRef(ElementRefToken, getPreviousOrParentTNode(), getLView());
}
let R3ElementRef;
/**
 * Creates an ElementRef given a node.
 *
 * @param ElementRefToken The ElementRef type
 * @param tNode The node for which you'd like an ElementRef
 * @param view The view to which the node belongs
 * @returns The ElementRef instance to use
 */
export function createElementRef(ElementRefToken, tNode, view) {
    if (!R3ElementRef) {
        R3ElementRef = class ElementRef extends ElementRefToken {
        };
    }
    return new R3ElementRef(getNativeByTNode(tNode, view));
}
let R3TemplateRef;
/**
 * Creates a TemplateRef given a node.
 *
 * @returns The TemplateRef instance to use
 */
export function injectTemplateRef(TemplateRefToken, ElementRefToken) {
    return createTemplateRef(TemplateRefToken, ElementRefToken, getPreviousOrParentTNode(), getLView());
}
/**
 * Creates a TemplateRef and stores it on the injector.
 *
 * @param TemplateRefToken The TemplateRef type
 * @param ElementRefToken The ElementRef type
 * @param hostTNode The node on which a TemplateRef is requested
 * @param hostView The view to which the node belongs
 * @returns The TemplateRef instance or null if we can't create a TemplateRef on a given node type
 */
export function createTemplateRef(TemplateRefToken, ElementRefToken, hostTNode, hostView) {
    if (!R3TemplateRef) {
        R3TemplateRef = class TemplateRef extends TemplateRefToken {
            constructor(_declarationView, _declarationTContainer, elementRef) {
                super();
                this._declarationView = _declarationView;
                this._declarationTContainer = _declarationTContainer;
                this.elementRef = elementRef;
            }
            createEmbeddedView(context) {
                const embeddedTView = this._declarationTContainer.tViews;
                const embeddedLView = createLView(this._declarationView, embeddedTView, context, 16 /* CheckAlways */, null, embeddedTView.node);
                const declarationLContainer = this._declarationView[this._declarationTContainer.index];
                ngDevMode && assertLContainer(declarationLContainer);
                embeddedLView[DECLARATION_LCONTAINER] = declarationLContainer;
                const declarationViewLQueries = this._declarationView[QUERIES];
                if (declarationViewLQueries !== null) {
                    embeddedLView[QUERIES] = declarationViewLQueries.createEmbeddedView(embeddedTView);
                }
                renderView(embeddedTView, embeddedLView, context);
                return new ViewRef(embeddedLView);
            }
        };
    }
    if (hostTNode.type === 0 /* Container */) {
        ngDevMode && assertDefined(hostTNode.tViews, 'TView must be allocated');
        return new R3TemplateRef(hostView, hostTNode, createElementRef(ElementRefToken, hostTNode, hostView));
    }
    else {
        return null;
    }
}
let R3ViewContainerRef;
/**
 * Creates a ViewContainerRef and stores it on the injector. Or, if the ViewContainerRef
 * already exists, retrieves the existing ViewContainerRef.
 *
 * @returns The ViewContainerRef instance to use
 */
export function injectViewContainerRef(ViewContainerRefToken, ElementRefToken) {
    const previousTNode = getPreviousOrParentTNode();
    return createContainerRef(ViewContainerRefToken, ElementRefToken, previousTNode, getLView());
}
/**
 * Creates a ViewContainerRef and stores it on the injector.
 *
 * @param ViewContainerRefToken The ViewContainerRef type
 * @param ElementRefToken The ElementRef type
 * @param hostTNode The node that is requesting a ViewContainerRef
 * @param hostView The view to which the node belongs
 * @returns The ViewContainerRef instance to use
 */
export function createContainerRef(ViewContainerRefToken, ElementRefToken, hostTNode, hostView) {
    if (!R3ViewContainerRef) {
        R3ViewContainerRef = class ViewContainerRef extends ViewContainerRefToken {
            constructor(_lContainer, _hostTNode, _hostView) {
                super();
                this._lContainer = _lContainer;
                this._hostTNode = _hostTNode;
                this._hostView = _hostView;
            }
            get element() {
                return createElementRef(ElementRefToken, this._hostTNode, this._hostView);
            }
            get injector() {
                return new NodeInjector(this._hostTNode, this._hostView);
            }
            /** @deprecated No replacement */
            get parentInjector() {
                const parentLocation = getParentInjectorLocation(this._hostTNode, this._hostView);
                const parentView = getParentInjectorView(parentLocation, this._hostView);
                const parentTNode = getParentInjectorTNode(parentLocation, this._hostView, this._hostTNode);
                return !hasParentInjector(parentLocation) || parentTNode == null ?
                    new NodeInjector(null, this._hostView) :
                    new NodeInjector(parentTNode, parentView);
            }
            clear() {
                while (this.length > 0) {
                    this.remove(this.length - 1);
                }
            }
            get(index) {
                return this._lContainer[VIEW_REFS] !== null && this._lContainer[VIEW_REFS][index] || null;
            }
            get length() {
                return this._lContainer.length - CONTAINER_HEADER_OFFSET;
            }
            createEmbeddedView(templateRef, context, index) {
                const viewRef = templateRef.createEmbeddedView(context || {});
                this.insert(viewRef, index);
                return viewRef;
            }
            createComponent(componentFactory, index, injector, projectableNodes, ngModuleRef) {
                const contextInjector = injector || this.parentInjector;
                if (!ngModuleRef && componentFactory.ngModule == null && contextInjector) {
                    // DO NOT REFACTOR. The code here used to have a `value || undefined` expression
                    // which seems to cause internal google apps to fail. This is documented in the
                    // following internal bug issue: go/b/142967802
                    const result = contextInjector.get(viewEngine_NgModuleRef, null);
                    if (result) {
                        ngModuleRef = result;
                    }
                }
                const componentRef = componentFactory.create(contextInjector, projectableNodes, undefined, ngModuleRef);
                this.insert(componentRef.hostView, index);
                return componentRef;
            }
            insert(viewRef, index) {
                const lView = viewRef._lView;
                const tView = lView[TVIEW];
                if (viewRef.destroyed) {
                    throw new Error('Cannot insert a destroyed View in a ViewContainer!');
                }
                this.allocateContainerIfNeeded();
                if (viewAttachedToContainer(lView)) {
                    // If view is already attached, detach it first so we clean up references appropriately.
                    const prevIdx = this.indexOf(viewRef);
                    // A view might be attached either to this or a different container. The `prevIdx` for
                    // those cases will be:
                    // equal to -1 for views attached to this ViewContainerRef
                    // >= 0 for views attached to a different ViewContainerRef
                    if (prevIdx !== -1) {
                        this.detach(prevIdx);
                    }
                    else {
                        const prevLContainer = lView[PARENT];
                        ngDevMode &&
                            assertEqual(isLContainer(prevLContainer), true, 'An attached view should have its PARENT point to a container.');
                        // We need to re-create a R3ViewContainerRef instance since those are not stored on
                        // LView (nor anywhere else).
                        const prevVCRef = new R3ViewContainerRef(prevLContainer, prevLContainer[T_HOST], prevLContainer[PARENT]);
                        prevVCRef.detach(prevVCRef.indexOf(viewRef));
                    }
                }
                const adjustedIdx = this._adjustIndex(index);
                insertView(tView, lView, this._lContainer, adjustedIdx);
                const beforeNode = getBeforeNodeForView(adjustedIdx, this._lContainer);
                addRemoveViewFromContainer(tView, lView, true, beforeNode);
                viewRef.attachToViewContainerRef(this);
                addToArray(this._lContainer[VIEW_REFS], adjustedIdx, viewRef);
                return viewRef;
            }
            move(viewRef, newIndex) {
                if (viewRef.destroyed) {
                    throw new Error('Cannot move a destroyed View in a ViewContainer!');
                }
                return this.insert(viewRef, newIndex);
            }
            indexOf(viewRef) {
                const viewRefsArr = this._lContainer[VIEW_REFS];
                return viewRefsArr !== null ? viewRefsArr.indexOf(viewRef) : -1;
            }
            remove(index) {
                this.allocateContainerIfNeeded();
                const adjustedIdx = this._adjustIndex(index, -1);
                removeView(this._lContainer, adjustedIdx);
                removeFromArray(this._lContainer[VIEW_REFS], adjustedIdx);
            }
            detach(index) {
                this.allocateContainerIfNeeded();
                const adjustedIdx = this._adjustIndex(index, -1);
                const view = detachView(this._lContainer, adjustedIdx);
                const wasDetached = view && removeFromArray(this._lContainer[VIEW_REFS], adjustedIdx) != null;
                return wasDetached ? new ViewRef(view) : null;
            }
            _adjustIndex(index, shift = 0) {
                if (index == null) {
                    return this.length + shift;
                }
                if (ngDevMode) {
                    assertGreaterThan(index, -1, `ViewRef index must be positive, got ${index}`);
                    // +1 because it's legal to insert at the end.
                    assertLessThan(index, this.length + 1 + shift, 'index');
                }
                return index;
            }
            allocateContainerIfNeeded() {
                if (this._lContainer[VIEW_REFS] === null) {
                    this._lContainer[VIEW_REFS] = [];
                }
            }
        };
    }
    ngDevMode &&
        assertNodeOfPossibleTypes(hostTNode, [0 /* Container */, 3 /* Element */, 4 /* ElementContainer */]);
    let lContainer;
    const slotValue = hostView[hostTNode.index];
    if (isLContainer(slotValue)) {
        // If the host is a container, we don't need to create a new LContainer
        lContainer = slotValue;
    }
    else {
        let commentNode;
        // If the host is an element container, the native host element is guaranteed to be a
        // comment and we can reuse that comment as anchor element for the new LContainer.
        // The comment node in question is already part of the DOM structure so we don't need to append
        // it again.
        if (hostTNode.type === 4 /* ElementContainer */) {
            commentNode = unwrapRNode(slotValue);
        }
        else {
            ngDevMode && ngDevMode.rendererCreateComment++;
            commentNode = hostView[RENDERER].createComment(ngDevMode ? 'container' : '');
            // A `ViewContainerRef` can be injected by the root (topmost / bootstrapped) component. In
            // this case we can't use TView / TNode data structures to insert container's marker node
            // (both a parent of a comment node and the comment node itself are not part of any view). In
            // this specific case we use low-level DOM manipulation to insert container's marker (comment)
            // node.
            if (isRootView(hostView)) {
                const renderer = hostView[RENDERER];
                const hostNative = getNativeByTNode(hostTNode, hostView);
                const parentOfHostNative = nativeParentNode(renderer, hostNative);
                nativeInsertBefore(renderer, parentOfHostNative, commentNode, nativeNextSibling(renderer, hostNative));
            }
            else {
                appendChild(hostView[TVIEW], hostView, commentNode, hostTNode);
            }
        }
        hostView[hostTNode.index] = lContainer =
            createLContainer(slotValue, hostView, commentNode, hostTNode);
        addToViewTree(hostView, lContainer);
    }
    return new R3ViewContainerRef(lContainer, hostTNode, hostView);
}
/** Returns a ChangeDetectorRef (a.k.a. a ViewRef) */
export function injectChangeDetectorRef(isPipe = false) {
    return createViewRef(getPreviousOrParentTNode(), getLView(), isPipe);
}
/**
 * Creates a ViewRef and stores it on the injector as ChangeDetectorRef (public alias).
 *
 * @param tNode The node that is requesting a ChangeDetectorRef
 * @param lView The view to which the node belongs
 * @param isPipe Whether the view is being injected into a pipe.
 * @returns The ChangeDetectorRef to use
 */
function createViewRef(tNode, lView, isPipe) {
    // `isComponentView` will be true for Component and Directives (but not for Pipes).
    // See https://github.com/angular/angular/pull/33072 for proper fix
    const isComponentView = !isPipe && isComponentHost(tNode);
    if (isComponentView) {
        // The LView represents the location where the component is declared.
        // Instead we want the LView for the component View and so we need to look it up.
        const componentView = getComponentLViewByIndex(tNode.index, lView); // look down
        return new ViewRef(componentView, componentView);
    }
    else if (tNode.type === 3 /* Element */ || tNode.type === 0 /* Container */ ||
        tNode.type === 4 /* ElementContainer */ || tNode.type === 5 /* IcuContainer */) {
        // The LView represents the location where the injection is requested from.
        // We need to locate the containing LView (in case where the `lView` is an embedded view)
        const hostComponentView = lView[DECLARATION_COMPONENT_VIEW]; // look up
        return new ViewRef(hostComponentView, lView);
    }
    return null;
}
/** Returns a Renderer2 (or throws when application was bootstrapped with Renderer3) */
function getOrCreateRenderer2(view) {
    const renderer = view[RENDERER];
    if (isProceduralRenderer(renderer)) {
        return renderer;
    }
    else {
        throw new Error('Cannot inject Renderer2 when the application uses Renderer3!');
    }
}
/** Injects a Renderer2 for the current component. */
export function injectRenderer2() {
    // We need the Renderer to be based on the component that it's being injected into, however since
    // DI happens before we've entered its view, `getLView` will return the parent view instead.
    const lView = getLView();
    const tNode = getPreviousOrParentTNode();
    const nodeAtIndex = getComponentLViewByIndex(tNode.index, lView);
    return getOrCreateRenderer2(isLView(nodeAtIndex) ? nodeAtIndex : lView);
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidmlld19lbmdpbmVfY29tcGF0aWJpbGl0eS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2NvcmUvc3JjL3JlbmRlcjMvdmlld19lbmdpbmVfY29tcGF0aWJpbGl0eS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFNSCxPQUFPLEVBQUMsV0FBVyxJQUFJLHNCQUFzQixFQUFDLE1BQU0sNkJBQTZCLENBQUM7QUFLbEYsT0FBTyxFQUFDLFVBQVUsRUFBRSxlQUFlLEVBQUMsTUFBTSxxQkFBcUIsQ0FBQztBQUNoRSxPQUFPLEVBQUMsYUFBYSxFQUFFLFdBQVcsRUFBRSxpQkFBaUIsRUFBRSxjQUFjLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQUU3RixPQUFPLEVBQUMsZ0JBQWdCLEVBQUMsTUFBTSxVQUFVLENBQUM7QUFDMUMsT0FBTyxFQUFDLHlCQUF5QixFQUFFLFlBQVksRUFBQyxNQUFNLE1BQU0sQ0FBQztBQUM3RCxPQUFPLEVBQUMsYUFBYSxFQUFFLGdCQUFnQixFQUFFLFdBQVcsRUFBRSxVQUFVLEVBQUMsTUFBTSx1QkFBdUIsQ0FBQztBQUMvRixPQUFPLEVBQUMsdUJBQXVCLEVBQWMsU0FBUyxFQUFDLE1BQU0sd0JBQXdCLENBQUM7QUFFdEYsT0FBTyxFQUFDLG9CQUFvQixFQUFxQixNQUFNLHVCQUF1QixDQUFDO0FBQy9FLE9BQU8sRUFBQyxlQUFlLEVBQUUsWUFBWSxFQUFFLE9BQU8sRUFBRSxVQUFVLEVBQUMsTUFBTSwwQkFBMEIsQ0FBQztBQUM1RixPQUFPLEVBQUMsMEJBQTBCLEVBQUUsc0JBQXNCLEVBQXFCLE1BQU0sRUFBRSxPQUFPLEVBQUUsUUFBUSxFQUFFLE1BQU0sRUFBRSxLQUFLLEVBQVEsTUFBTSxtQkFBbUIsQ0FBQztBQUN6SixPQUFPLEVBQUMseUJBQXlCLEVBQUMsTUFBTSxlQUFlLENBQUM7QUFDeEQsT0FBTyxFQUFDLDBCQUEwQixFQUFF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LHlGQUF5RjtRQUN6RixNQUFNLGlCQUFpQixHQUFHLEtBQUssQ0FBQywwQkFBMEIsQ0FBQyxDQUFDLENBQUUsVUFBVTtRQUN4RSxPQUFPLElBQUksT0FBTyxDQUFDLGlCQUFpQixFQUFFLEtBQUssQ0FBQyxDQUFDO0tBQzlDO0lBQ0QsT0FBTyxJQUFLLENBQUM7QUFDZixDQUFDO0FBRUQsdUZBQXVGO0FBQ3ZGLFNBQVMsb0JBQW9CLENBQUMsSUFBVztJQUN2QyxNQUFNLFFBQVEsR0FBRyxJQUFJLENBQUMsUUFBUSxDQUFDLENBQUM7SUFDaEMsSUFBSSxvQkFBb0IsQ0FBQyxRQUFRLENBQUMsRUFBRTtRQUNsQyxPQUFPLFFBQXFCLENBQUM7S0FDOUI7U0FBTTtRQUNMLE1BQU0sSUFBSSxLQUFLLENBQUMsOERBQThELENBQUMsQ0FBQztLQUNqRjtBQUNILENBQUM7QUFFRCxxREFBcUQ7QUFDckQsTUFBTSxVQUFVLGVBQWU7SUFDN0IsaUdBQWlHO0lBQ2pHLDRGQUE0RjtJQUM1RixNQUFNLEtBQUssR0FBRyxRQUFRLEVBQUUsQ0FBQztJQUN6QixNQUFNLEtBQUssR0FBRyx3QkFBd0IsRUFBRSxDQUFDO0lBQ3pDLE1BQU0sV0FBVyxHQUFHLHdCQUF3QixDQUFDLEtBQUssQ0FBQyxLQUFLLEVBQUUsS0FBSyxDQUFDLENBQUM7SUFDakUsT0FBTyxvQkFBb0IsQ0FBQyxPQUFPLENBQUMsV0FBVyxDQUFDLENBQUMsQ0FBQyxDQUFDLFdBQVcsQ0FBQyxDQUFDLENBQUMsS0FBSyxDQUFDLENBQUM7QUFDMUUsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0NoYW5nZURldGVjdG9yUmVmIGFzIFZpZXdFbmdpbmVfQ2hhbmdlRGV0ZWN0b3JSZWZ9IGZyb20gJy4uL2NoYW5nZV9kZXRlY3Rpb24vY2hhbmdlX2RldGVjdG9yX3JlZic7XG5pbXBvcnQge0luamVjdG9yfSBmcm9tICcuLi9kaS9pbmplY3Rvcic7XG5pbXBvcnQge0NvbXBvbmVudEZhY3RvcnkgYXMgdmlld0VuZ2luZV9Db21wb25lbnRGYWN0b3J5LCBDb21wb25lbnRSZWYgYXMgdmlld0VuZ2luZV9Db21wb25lbnRSZWZ9IGZyb20gJy4uL2xpbmtlci9jb21wb25lbnRfZmFjdG9yeSc7XG5pbXBvcnQge0VsZW1lbnRSZWYgYXMgVmlld0VuZ2luZV9FbGVtZW50UmVmfSBmcm9tICcuLi9saW5rZXIvZWxlbWVudF9yZWYnO1xuaW1wb3J0IHtOZ01vZHVsZVJlZiBhcyB2aWV3RW5naW5lX05nTW9kdWxlUmVmfSBmcm9tICcuLi9saW5rZXIvbmdfbW9kdWxlX2ZhY3RvcnknO1xuaW1wb3J0IHtUZW1wbGF0ZVJlZiBhcyBWaWV3RW5naW5lX1RlbXBsYXRlUmVmfSBmcm9tICcuLi9saW5rZXIvdGVtcGxhdGVfcmVmJztcbmltcG9ydCB7Vmlld0NvbnRhaW5lclJlZiBhcyBWaWV3RW5naW5lX1ZpZXdDb250YWluZXJSZWZ9IGZyb20gJy4uL2xpbmtlci92aWV3X2NvbnRhaW5lcl9yZWYnO1xuaW1wb3J0IHtFbWJlZGRlZFZpZXdSZWYgYXMgdmlld0VuZ2luZV9FbWJlZGRlZFZpZXdSZWYsIFZpZXdSZWYgYXMgdmlld0VuZ2luZV9WaWV3UmVmfSBmcm9tICcuLi9saW5rZXIvdmlld19yZWYnO1xuaW1wb3J0IHtSZW5kZXJlcjJ9IGZyb20gJy4uL3JlbmRlci9hcGknO1xuaW1wb3J0IHthZGRUb0FycmF5LCByZW1vdmVGcm9tQXJyYXl9IGZyb20gJy4uL3V0aWwvYXJyYXlfdXRpbHMnO1xuaW1wb3J0IHthc3NlcnREZWZpbmVkLCBhc3NlcnRFcXVhbCwgYXNzZXJ0R3JlYXRlclRoYW4sIGFzc2VydExlc3NUaGFufSBmcm9tICcuLi91dGlsL2Fzc2VydCc7XG5cbmltcG9ydCB7YXNzZXJ0TENvbnRhaW5lcn0gZnJvbSAnLi9hc3NlcnQnO1xuaW1wb3J0IHtnZXRQYXJlbnRJbmplY3RvckxvY2F0aW9uLCBOb2RlSW5qZWN0b3J9IGZyb20gJy4vZGknO1xuaW1wb3J0IHthZGRUb1ZpZXdUcmVlLCBjcmVhdGVMQ29udGFpbmVyLCBjcmVhdGVMVmlldywgcmVuZGVyVmlld30gZnJvbSAnLi9pbnN0cnVjdGlvbnMvc2hhcmVkJztcbmltcG9ydCB7Q09OVEFJTkVSX0hFQURFUl9PRkZTRVQsIExDb250YWluZXIsIFZJRVdfUkVGU30gZnJvbSAnLi9pbnRlcmZhY2VzL2NvbnRhaW5lcic7XG5pbXBvcnQge1RDb250YWluZXJOb2RlLCBURGlyZWN0aXZlSG9zdE5vZGUsIFRFbGVtZW50Q29udGFpbmVyTm9kZSwgVEVsZW1lbnROb2RlLCBUTm9kZSwgVE5vZGVUeXBlLCBUVmlld05vZGV9IGZyb20gJy4vaW50ZXJmYWNlcy9ub2RlJztcbmltcG9ydCB7aXNQcm9jZWR1cmFsUmVuZGVyZXIsIFJDb21tZW50LCBSRWxlbWVudH0gZnJvbSAnLi9pbnRlcmZhY2VzL3JlbmRlcmVyJztcbmltcG9ydCB7aXNDb21wb25lbnRIb3N0LCBpc0xDb250YWluZXIsIGlzTFZpZXcsIGlzUm9vdFZpZXd9IGZyb20gJy4vaW50ZXJmYWNlcy90eXBlX2NoZWNrcyc7XG5pbXBvcnQge0RFQ0xBUkFUSU9OX0NPTVBPTkVOVF9WSUVXLCBERUNMQVJBVElPTl9MQ09OVEFJTkVSLCBMVmlldywgTFZpZXdGbGFncywgUEFSRU5ULCBRVUVSSUVTLCBSRU5ERVJFUiwgVF9IT1NULCBUVklFVywgVFZpZXd9IGZyb20gJy4vaW50ZXJmYWNlcy92aWV3JztcbmltcG9ydCB7YXNzZXJ0Tm9kZU9mUG9zc2libGVUeXBlc30gZnJvbSAnLi9ub2RlX2Fzc2VydCc7XG5pbXBvcnQge2FkZFJlbW92ZVZpZXdGcm9tQ29udGFpbmVyLCBhcHBlbmRDaGlsZCwgZGV0YWNoVmlldywgZ2V0QmVmb3JlTm9kZUZvclZpZXcsIGluc2VydFZpZXcsIG5hdGl2ZUluc2VydEJlZm9yZSwgbmF0aXZlTmV4dFNpYmxpbmcsIG5hdGl2ZVBhcmVudE5vZGUsIHJlbW92ZVZpZXd9IGZyb20gJy4vbm9kZV9tYW5pcHVsYXRpb24nO1xuaW1wb3J0IHtnZXRQYXJlbnRJbmplY3RvclROb2RlfSBmcm9tICcuL25vZGVfdXRpbCc7XG5pbXBvcnQge2dldExWaWV3LCBnZXRQcmV2aW91c09yUGFyZW50VE5vZGV9IGZyb20gJy4vc3RhdGUnO1xuaW1wb3J0IHtnZXRQYXJlbnRJbmplY3RvclZpZXcsIGhhc1BhcmVudEluamVjdG9yfSBmcm9tICcuL3V0aWwvaW5qZWN0b3JfdXRpbHMnO1xuaW1wb3J0IHtnZXRDb21wb25lbnRMVmlld0J5SW5kZXgsIGdldE5hdGl2ZUJ5VE5vZGUsIHVud3JhcFJOb2RlLCB2aWV3QXR0YWNoZWRUb0NvbnRhaW5lcn0gZnJvbSAnLi91dGlsL3ZpZXdfdXRpbHMnO1xuaW1wb3J0IHtWaWV3UmVmfSBmcm9tICcuL3ZpZXdfcmVmJztcblxuXG5cbi8qKlxuICogQ3JlYXRlcyBhbiBFbGVtZW50UmVmIGZyb20gdGhlIG1vc3QgcmVjZW50IG5vZGUuXG4gKlxuICogQHJldHVybnMgVGhlIEVsZW1lbnRSZWYgaW5zdGFuY2UgdG8gdXNlXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBpbmplY3RFbGVtZW50UmVmKEVsZW1lbnRSZWZUb2tlbjogdHlwZW9mIFZpZXdFbmdpbmVfRWxlbWVudFJlZik6XG4gICAgVmlld0VuZ2luZV9FbGVtZW50UmVmIHtcbiAgcmV0dXJuIGNyZWF0ZUVsZW1lbnRSZWYoRWxlbWVudFJlZlRva2VuLCBnZXRQcmV2aW91c09yUGFyZW50VE5vZGUoKSwgZ2V0TFZpZXcoKSk7XG59XG5cbmxldCBSM0VsZW1lbnRSZWY6IHtuZXcgKG5hdGl2ZTogUkVsZW1lbnR8UkNvbW1lbnQpOiBWaWV3RW5naW5lX0VsZW1lbnRSZWZ9O1xuXG4vKipcbiAqIENyZWF0ZXMgYW4gRWxlbWVudFJlZiBnaXZlbiBhIG5vZGUuXG4gKlxuICogQHBhcmFtIEVsZW1lbnRSZWZUb2tlbiBUaGUgRWxlbWVudFJlZiB0eXBlXG4gKiBAcGFyYW0gdE5vZGUgVGhlIG5vZGUgZm9yIHdoaWNoIHlvdSdkIGxpa2UgYW4gRWxlbWVudFJlZlxuICogQHBhcmFtIHZpZXcgVGhlIHZpZXcgdG8gd2hpY2ggdGhlIG5vZGUgYmVsb25nc1xuICogQHJldHVybnMgVGhlIEVsZW1lbnRSZWYgaW5zdGFuY2UgdG8gdXNlXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBjcmVhdGVFbGVtZW50UmVmKFxuICAgIEVsZW1lbnRSZWZUb2tlbjogdHlwZW9mIFZpZXdFbmdpbmVfRWxlbWVudFJlZiwgdE5vZGU6IFROb2RlLFxuICAgIHZpZXc6IExWaWV3KTogVmlld0VuZ2luZV9FbGVtZW50UmVmIHtcbiAgaWYgKCFSM0VsZW1lbnRSZWYpIHtcbiAgICBSM0VsZW1lbnRSZWYgPSBjbGFzcyBFbGVtZW50UmVmIGV4dGVuZHMgRWxlbWVudFJlZlRva2VuIHt9O1xuICB9XG4gIHJldHVybiBuZXcgUjNFbGVtZW50UmVmKGdldE5hdGl2ZUJ5VE5vZGUodE5vZGUsIHZpZXcpIGFzIFJFbGVtZW50KTtcbn1cblxubGV0IFIzVGVtcGxhdGVSZWY6IHtcbiAgbmV3IChfZGVjbGFyYXRpb25QYXJlbnRWaWV3OiBMVmlldywgaG9zdFROb2RlOiBUQ29udGFpbmVyTm9kZSwgZWxlbWVudFJlZjogVmlld0VuZ2luZV9FbGVtZW50UmVmKTpcbiAgICAgIFZpZXdFbmdpbmVfVGVtcGxhdGVSZWY8YW55PlxufTtcblxuLyoqXG4gKiBDcmVhdGVzIGEgVGVtcGxhdGVSZWYgZ2l2ZW4gYSBub2RlLlxuICpcbiAqIEByZXR1cm5zIFRoZSBUZW1wbGF0ZVJlZiBpbnN0YW5jZSB0byB1c2VcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGluamVjdFRlbXBsYXRlUmVmPFQ+KFxuICAgIFRlbXBsYXRlUmVmVG9rZW46IHR5cGVvZiBWaWV3RW5naW5lX1RlbXBsYXRlUmVmLFxuICAgIEVsZW1lbnRSZWZUb2tlbjogdHlwZW9mIFZpZXdFbmdpbmVfRWxlbWVudFJlZik6IFZpZXdFbmdpbmVfVGVtcGxhdGVSZWY8VD58bnVsbCB7XG4gIHJldHVybiBjcmVhdGVUZW1wbGF0ZVJlZjxUPihcbiAgICAgIFRlbXBsYXRlUmVmVG9rZW4sIEVsZW1lbnRSZWZUb2tlbiwgZ2V0UHJldmlvdXNPclBhcmVudFROb2RlKCksIGdldExWaWV3KCkpO1xufVxuXG4vKipcbiAqIENyZWF0ZXMgYSBUZW1wbGF0ZVJlZiBhbmQgc3RvcmVzIGl0IG9uIHRoZSBpbmplY3Rvci5cbiAqXG4gKiBAcGFyYW0gVGVtcGxhdGVSZWZUb2tlbiBUaGUgVGVtcGxhdGVSZWYgdHlwZVxuICogQHBhcmFtIEVsZW1lbnRSZWZUb2tlbiBUaGUgRWxlbWVudFJlZiB0eXBlXG4gKiBAcGFyYW0gaG9zdFROb2RlIFRoZSBub2RlIG9uIHdoaWNoIGEgVGVtcGxhdGVSZWYgaXMgcmVxdWVzdGVkXG4gKiBAcGFyYW0gaG9zdFZpZXcgVGhlIHZpZXcgdG8gd2hpY2ggdGhlIG5vZGUgYmVsb25nc1xuICogQHJldHVybnMgVGhlIFRlbXBsYXRlUmVmIGluc3RhbmNlIG9yIG51bGwgaWYgd2UgY2FuJ3QgY3JlYXRlIGEgVGVtcGxhdGVSZWYgb24gYSBnaXZlbiBub2RlIHR5cGVcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGNyZWF0ZVRlbXBsYXRlUmVmPFQ+KFxuICAgIFRlbXBsYXRlUmVmVG9rZW46IHR5cGVvZiBWaWV3RW5naW5lX1RlbXBsYXRlUmVmLCBFbGVtZW50UmVmVG9rZW46IHR5cGVvZiBWaWV3RW5naW5lX0VsZW1lbnRSZWYsXG4gICAgaG9zdFROb2RlOiBUTm9kZSwgaG9zdFZpZXc6IExWaWV3KTogVmlld0VuZ2luZV9UZW1wbGF0ZVJlZjxUPnxudWxsIHtcbiAgaWYgKCFSM1RlbXBsYXRlUmVmKSB7XG4gICAgUjNUZW1wbGF0ZVJlZiA9IGNsYXNzIFRlbXBsYXRlUmVmPFQ+IGV4dGVuZHMgVGVtcGxhdGVSZWZUb2tlbjxUPntcbiAgICAgIGNvbnN0cnVjdG9yKFxuICAgICAgICAgIHByaXZhdGUgX2RlY2xhcmF0aW9uVmlldzogTFZpZXcsIHByaXZhdGUgX2RlY2xhcmF0aW9uVENvbnRhaW5lcjogVENvbnRhaW5lck5vZGUsXG4gICAgICAgICAgcmVhZG9ubHkgZWxlbWVudFJlZjogVmlld0VuZ2luZV9FbGVtZW50UmVmKSB7XG4gICAgICAgIHN1cGVyKCk7XG4gICAgICB9XG5cbiAgICAgIGNyZWF0ZUVtYmVkZGVkVmlldyhjb250ZXh0OiBUKTogdmlld0VuZ2luZV9FbWJlZGRlZFZpZXdSZWY8VD4ge1xuICAgICAgICBjb25zdCBlbWJlZGRlZFRWaWV3ID0gdGhpcy5fZGVjbGFyYXRpb25UQ29udGFpbmVyLnRWaWV3cyBhcyBUVmlldztcbiAgICAgICAgY29uc3QgZW1iZWRkZWRMVmlldyA9IGNyZWF0ZUxWaWV3KFxuICAgICAgICAgICAgdGhpcy5fZGVjbGFyYXRpb25WaWV3LCBlbWJlZGRlZFRWaWV3LCBjb250ZXh0LCBMVmlld0ZsYWdzLkNoZWNrQWx3YXlzLCBudWxsLFxuICAgICAgICAgICAgZW1iZWRkZWRUVmlldy5ub2RlKTtcblxuICAgICAgICBjb25zdCBkZWNsYXJhdGlvbkxDb250YWluZXIgPSB0aGlzLl9kZWNsYXJhdGlvblZpZXdbdGhpcy5fZGVjbGFyYXRpb25UQ29udGFpbmVyLmluZGV4XTtcbiAgICAgICAgbmdEZXZNb2RlICYmIGFzc2VydExDb250YWluZXIoZGVjbGFyYXRpb25MQ29udGFpbmVyKTtcbiAgICAgICAgZW1iZWRkZWRMVmlld1tERUNMQVJBVElPTl9MQ09OVEFJTkVSXSA9IGRlY2xhcmF0aW9uTENvbnRhaW5lcjtcblxuICAgICAgICBjb25zdCBkZWNsYXJhdGlvblZpZXdMUXVlcmllcyA9IHRoaXMuX2RlY2xhcmF0aW9uVmlld1tRVUVSSUVTXTtcbiAgICAgICAgaWYgKGRlY2xhcmF0aW9uVmlld0xRdWVyaWVzICE9PSBudWxsKSB7XG4gICAgICAgICAgZW1iZWRkZWRMVmlld1tRVUVSSUVTXSA9IGRlY2xhcmF0aW9uVmlld0xRdWVyaWVzLmNyZWF0ZUVtYmVkZGVkVmlldyhlbWJlZGRlZFRWaWV3KTtcbiAgICAgICAgfVxuXG4gICAgICAgIHJlbmRlclZpZXcoZW1iZWRkZWRUVmlldywgZW1iZWRkZWRMVmlldywgY29udGV4dCk7XG5cbiAgICAgICAgcmV0dXJuIG5ldyBWaWV3UmVmPFQ+KGVtYmVkZGVkTFZpZXcpO1xuICAgICAgfVxuICAgIH07XG4gIH1cblxuICBpZiAoaG9zdFROb2RlLnR5cGUgPT09IFROb2RlVHlwZS5Db250YWluZXIpIHtcbiAgICBuZ0Rldk1vZGUgJiYgYXNzZXJ0RGVmaW5lZChob3N0VE5vZGUudFZpZXdzLCAnVFZpZXcgbXVzdCBiZSBhbGxvY2F0ZWQnKTtcbiAgICByZXR1cm4gbmV3IFIzVGVtcGxhdGVSZWYoXG4gICAgICAgIGhvc3RWaWV3LCBob3N0VE5vZGUgYXMgVENvbnRhaW5lck5vZGUsXG4gICAgICAgIGNyZWF0ZUVsZW1lbnRSZWYoRWxlbWVudFJlZlRva2VuLCBob3N0VE5vZGUsIGhvc3RWaWV3KSk7XG4gIH0gZWxzZSB7XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cbn1cblxubGV0IFIzVmlld0NvbnRhaW5lclJlZjoge1xuICBuZXcgKFxuICAgICAgbENvbnRhaW5lcjogTENvbnRhaW5lciwgaG9zdFROb2RlOiBURWxlbWVudE5vZGV8VENvbnRhaW5lck5vZGV8VEVsZW1lbnRDb250YWluZXJOb2RlLFxuICAgICAgaG9zdFZpZXc6IExWaWV3KTogVmlld0VuZ2luZV9WaWV3Q29udGFpbmVyUmVmXG59O1xuXG4vKipcbiAqIENyZWF0ZXMgYSBWaWV3Q29udGFpbmVyUmVmIGFuZCBzdG9yZXMgaXQgb24gdGhlIGluamVjdG9yLiBPciwgaWYgdGhlIFZpZXdDb250YWluZXJSZWZcbiAqIGFscmVhZHkgZXhpc3RzLCByZXRyaWV2ZXMgdGhlIGV4aXN0aW5nIFZpZXdDb250YWluZXJSZWYuXG4gKlxuICogQHJldHVybnMgVGhlIFZpZXdDb250YWluZXJSZWYgaW5zdGFuY2UgdG8gdXNlXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBpbmplY3RWaWV3Q29udGFpbmVyUmVmKFxuICAgIFZpZXdDb250YWluZXJSZWZUb2tlbjogdHlwZW9mIFZpZXdFbmdpbmVfVmlld0NvbnRhaW5lclJlZixcbiAgICBFbGVtZW50UmVmVG9rZW46IHR5cGVvZiBWaWV3RW5naW5lX0VsZW1lbnRSZWYpOiBWaWV3RW5naW5lX1ZpZXdDb250YWluZXJSZWYge1xuICBjb25zdCBwcmV2aW91c1ROb2RlID1cbiAgICAgIGdldFByZXZpb3VzT3JQYXJlbnRUTm9kZSgpIGFzIFRFbGVtZW50Tm9kZSB8IFRFbGVtZW50Q29udGFpbmVyTm9kZSB8IFRDb250YWluZXJOb2RlO1xuICByZXR1cm4gY3JlYXRlQ29udGFpbmVyUmVmKFZpZXdDb250YWluZXJSZWZUb2tlbiwgRWxlbWVudFJlZlRva2VuLCBwcmV2aW91c1ROb2RlLCBnZXRMVmlldygpKTtcbn1cblxuLyoqXG4gKiBDcmVhdGVzIGEgVmlld0NvbnRhaW5lclJlZiBhbmQgc3RvcmVzIGl0IG9uIHRoZSBpbmplY3Rvci5cbiAqXG4gKiBAcGFyYW0gVmlld0NvbnRhaW5lclJlZlRva2VuIFRoZSBWaWV3Q29udGFpbmVyUmVmIHR5cGVcbiAqIEBwYXJhbSBFbGVtZW50UmVmVG9rZW4gVGhlIEVsZW1lbnRSZWYgdHlwZVxuICogQHBhcmFtIGhvc3RUTm9kZSBUaGUgbm9kZSB0aGF0IGlzIHJlcXVlc3RpbmcgYSBWaWV3Q29udGFpbmVyUmVmXG4gKiBAcGFyYW0gaG9zdFZpZXcgVGhlIHZpZXcgdG8gd2hpY2ggdGhlIG5vZGUgYmVsb25nc1xuICogQHJldHVybnMgVGhlIFZpZXdDb250YWluZXJSZWYgaW5zdGFuY2UgdG8gdXNlXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBjcmVhdGVDb250YWluZXJSZWYoXG4gICAgVmlld0NvbnRhaW5lclJlZlRva2VuOiB0eXBlb2YgVmlld0VuZ2luZV9WaWV3Q29udGFpbmVyUmVmLFxuICAgIEVsZW1lbnRSZWZUb2tlbjogdHlwZW9mIFZpZXdFbmdpbmVfRWxlbWVudFJlZixcbiAgICBob3N0VE5vZGU6IFRFbGVtZW50Tm9kZXxUQ29udGFpbmVyTm9kZXxURWxlbWVudENvbnRhaW5lck5vZGUsXG4gICAgaG9zdFZpZXc6IExWaWV3KTogVmlld0VuZ2luZV9WaWV3Q29udGFpbmVyUmVmIHtcbiAgaWYgKCFSM1ZpZXdDb250YWluZXJSZWYpIHtcbiAgICBSM1ZpZXdDb250YWluZXJSZWYgPSBjbGFzcyBWaWV3Q29udGFpbmVyUmVmIGV4dGVuZHMgVmlld0NvbnRhaW5lclJlZlRva2VuIHtcbiAgICAgIGNvbnN0cnVjdG9yKFxuICAgICAgICAgIHByaXZhdGUgX2xDb250YWluZXI6IExDb250YWluZXIsXG4gICAgICAgICAgcHJpdmF0ZSBfaG9zdFROb2RlOiBURWxlbWVudE5vZGV8VENvbnRhaW5lck5vZGV8VEVsZW1lbnRDb250YWluZXJOb2RlLFxuICAgICAgICAgIHByaXZhdGUgX2hvc3RWaWV3OiBMVmlldykge1xuICAgICAgICBzdXBlcigpO1xuICAgICAgfVxuXG4gICAgICBnZXQgZWxlbWVudCgpOiBWaWV3RW5naW5lX0VsZW1lbnRSZWYge1xuICAgICAgICByZXR1cm4gY3JlYXRlRWxlbWVudFJlZihFbGVtZW50UmVmVG9rZW4sIHRoaXMuX2hvc3RUTm9kZSwgdGhpcy5faG9zdFZpZXcpO1xuICAgICAgfVxuXG4gICAgICBnZXQgaW5qZWN0b3IoKTogSW5qZWN0b3Ige1xuICAgICAgICByZXR1cm4gbmV3IE5vZGVJbmplY3Rvcih0aGlzLl9ob3N0VE5vZGUsIHRoaXMuX2hvc3RWaWV3KTtcbiAgICAgIH1cblxuICAgICAgLyoqIEBkZXByZWNhdGVkIE5vIHJlcGxhY2VtZW50ICovXG4gICAgICBnZXQgcGFyZW50SW5qZWN0b3IoKTogSW5qZWN0b3Ige1xuICAgICAgICBjb25zdCBwYXJlbnRMb2NhdGlvbiA9IGdldFBhcmVudEluamVjdG9yTG9jYXRpb24odGhpcy5faG9zdFROb2RlLCB0aGlzLl9ob3N0Vmlldyk7XG4gICAgICAgIGNvbnN0IHBhcmVudFZpZXcgPSBnZXRQYXJlbnRJbmplY3RvclZpZXcocGFyZW50TG9jYXRpb24sIHRoaXMuX2hvc3RWaWV3KTtcbiAgICAgICAgY29uc3QgcGFyZW50VE5vZGUgPSBnZXRQYXJlbnRJbmplY3RvclROb2RlKHBhcmVudExvY2F0aW9uLCB0aGlzLl9ob3N0VmlldywgdGhpcy5faG9zdFROb2RlKTtcblxuICAgICAgICByZXR1cm4gIWhhc1BhcmVudEluamVjdG9yKHBhcmVudExvY2F0aW9uKSB8fCBwYXJlbnRUTm9kZSA9PSBudWxsID9cbiAgICAgICAgICAgIG5ldyBOb2RlSW5qZWN0b3IobnVsbCwgdGhpcy5faG9zdFZpZXcpIDpcbiAgICAgICAgICAgIG5ldyBOb2RlSW5qZWN0b3IocGFyZW50VE5vZGUsIHBhcmVudFZpZXcpO1xuICAgICAgfVxuXG4gICAgICBjbGVhcigpOiB2b2lkIHtcbiAgICAgICAgd2hpbGUgKHRoaXMubGVuZ3RoID4gMCkge1xuICAgICAgICAgIHRoaXMucmVtb3ZlKHRoaXMubGVuZ3RoIC0gMSk7XG4gICAgICAgIH1cbiAgICAgIH1cblxuICAgICAgZ2V0KGluZGV4OiBudW1iZXIpOiB2aWV3RW5naW5lX1ZpZXdSZWZ8bnVsbCB7XG4gICAgICAgIHJldHVybiB0aGlzLl9sQ29udGFpbmVyW1ZJRVdfUkVGU10gIT09IG51bGwgJiYgdGhpcy5fbENvbnRhaW5lcltWSUVXX1JFRlNdIVtpbmRleF0gfHwgbnVsbDtcbiAgICAgIH1cblxuICAgICAgZ2V0IGxlbmd0aCgpOiBudW1iZXIge1xuICAgICAgICByZXR1cm4gdGhpcy5fbENvbnRhaW5lci5sZW5ndGggLSBDT05UQUlORVJfSEVBREVSX09GRlNFVDtcbiAgICAgIH1cblxuICAgICAgY3JlYXRlRW1iZWRkZWRWaWV3PEM+KHRlbXBsYXRlUmVmOiBWaWV3RW5naW5lX1RlbXBsYXRlUmVmPEM+LCBjb250ZXh0PzogQywgaW5kZXg/OiBudW1iZXIpOlxuICAgICAgICAgIHZpZXdFbmdpbmVfRW1iZWRkZWRWaWV3UmVmPEM+IHtcbiAgICAgICAgY29uc3Qgdmlld1JlZiA9IHRlbXBsYXRlUmVmLmNyZWF0ZUVtYmVkZGVkVmlldyhjb250ZXh0IHx8IDxhbnk+e30pO1xuICAgICAgICB0aGlzLmluc2VydCh2aWV3UmVmLCBpbmRleCk7XG4gICAgICAgIHJldHVybiB2aWV3UmVmO1xuICAgICAgfVxuXG4gICAgICBjcmVhdGVDb21wb25lbnQ8Qz4oXG4gICAgICAgICAgY29tcG9uZW50RmFjdG9yeTogdmlld0VuZ2luZV9Db21wb25lbnRGYWN0b3J5PEM+LCBpbmRleD86IG51bWJlcnx1bmRlZmluZWQsXG4gICAgICAgICAgaW5qZWN0b3I/OiBJbmplY3Rvcnx1bmRlZmluZWQsIHByb2plY3RhYmxlTm9kZXM/OiBhbnlbXVtdfHVuZGVmaW5lZCxcbiAgICAgICAgICBuZ01vZHVsZVJlZj86IHZpZXdFbmdpbmVfTmdNb2R1bGVSZWY8YW55Pnx1bmRlZmluZWQpOiB2aWV3RW5naW5lX0NvbXBvbmVudFJlZjxDPiB7XG4gICAgICAgIGNvbnN0IGNvbnRleHRJbmplY3RvciA9IGluamVjdG9yIHx8IHRoaXMucGFyZW50SW5qZWN0b3I7XG4gICAgICAgIGlmICghbmdNb2R1bGVSZWYgJiYgKGNvbXBvbmVudEZhY3RvcnkgYXMgYW55KS5uZ01vZHVsZSA9PSBudWxsICYmIGNvbnRleHRJbmplY3Rvcikge1xuICAgICAgICAgIC8vIERPIE5PVCBSRUZBQ1RPUi4gVGhlIGNvZGUgaGVyZSB1c2VkIHRvIGhhdmUgYSBgdmFsdWUgfHwgdW5kZWZpbmVkYCBleHByZXNzaW9uXG4gICAgICAgICAgLy8gd2hpY2ggc2VlbXMgdG8gY2F1c2UgaW50ZXJuYWwgZ29vZ2xlIGFwcHMgdG8gZmFpbC4gVGhpcyBpcyBkb2N1bWVudGVkIGluIHRoZVxuICAgICAgICAgIC8vIGZvbGxvd2luZyBpbnRlcm5hbCBidWcgaXNzdWU6IGdvL2IvMTQyOTY3ODAyXG4gICAgICAgICAgY29uc3QgcmVzdWx0ID0gY29udGV4dEluamVjdG9yLmdldCh2aWV3RW5naW5lX05nTW9kdWxlUmVmLCBudWxsKTtcbiAgICAgICAgICBpZiAocmVzdWx0KSB7XG4gICAgICAgICAgICBuZ01vZHVsZVJlZiA9IHJlc3VsdDtcbiAgICAgICAgICB9XG4gICAgICAgIH1cblxuICAgICAgICBjb25zdCBjb21wb25lbnRSZWYgPVxuICAgICAgICAgICAgY29tcG9uZW50RmFjdG9yeS5jcmVhdGUoY29udGV4dEluamVjdG9yLCBwcm9qZWN0YWJsZU5vZGVzLCB1bmRlZmluZWQsIG5nTW9kdWxlUmVmKTtcbiAgICAgICAgdGhpcy5pbnNlcnQoY29tcG9uZW50UmVmLmhvc3RWaWV3LCBpbmRleCk7XG4gICAgICAgIHJldHVybiBjb21wb25lbnRSZWY7XG4gICAgICB9XG5cbiAgICAgIGluc2VydCh2aWV3UmVmOiB2aWV3RW5naW5lX1ZpZXdSZWYsIGluZGV4PzogbnVtYmVyKTogdmlld0VuZ2luZV9WaWV3UmVmIHtcbiAgICAgICAgY29uc3QgbFZpZXcgPSAodmlld1JlZiBhcyBWaWV3UmVmPGFueT4pLl9sVmlldyE7XG4gICAgICAgIGNvbnN0IHRWaWV3ID0gbFZpZXdbVFZJRVddO1xuXG4gICAgICAgIGlmICh2aWV3UmVmLmRlc3Ryb3llZCkge1xuICAgICAgICAgIHRocm93IG5ldyBFcnJvcignQ2Fubm90IGluc2VydCBhIGRlc3Ryb3llZCBWaWV3IGluIGEgVmlld0NvbnRhaW5lciEnKTtcbiAgICAgICAgfVxuXG4gICAgICAgIHRoaXMuYWxsb2NhdGVDb250YWluZXJJZk5lZWRlZCgpO1xuXG4gICAgICAgIGlmICh2aWV3QXR0YWNoZWRUb0NvbnRhaW5lcihsVmlldykpIHtcbiAgICAgICAgICAvLyBJZiB2aWV3IGlzIGFscmVhZHkgYXR0YWNoZWQsIGRldGFjaCBpdCBmaXJzdCBzbyB3ZSBjbGVhbiB1cCByZWZlcmVuY2VzIGFwcHJvcHJpYXRlbHkuXG5cbiAgICAgICAgICBjb25zdCBwcmV2SWR4ID0gdGhpcy5pbmRleE9mKHZpZXdSZWYpO1xuXG4gICAgICAgICAgLy8gQSB2aWV3IG1pZ2h0IGJlIGF0dGFjaGVkIGVpdGhlciB0byB0aGlzIG9yIGEgZGlmZmVyZW50IGNvbnRhaW5lci4gVGhlIGBwcmV2SWR4YCBmb3JcbiAgICAgICAgICAvLyB0aG9zZSBjYXNlcyB3aWxsIGJlOlxuICAgICAgICAgIC8vIGVxdWFsIHRvIC0xIGZvciB2aWV3cyBhdHRhY2hlZCB0byB0aGlzIFZpZXdDb250YWluZXJSZWZcbiAgICAgICAgICAvLyA+PSAwIGZvciB2aWV3cyBhdHRhY2hlZCB0byBhIGRpZmZlcmVudCBWaWV3Q29udGFpbmVyUmVmXG4gICAgICAgICAgaWYgKHByZXZJZHggIT09IC0xKSB7XG4gICAgICAgICAgICB0aGlzLmRldGFjaChwcmV2SWR4KTtcbiAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgY29uc3QgcHJldkxDb250YWluZXIgPSBsVmlld1tQQVJFTlRdIGFzIExDb250YWluZXI7XG4gICAgICAgICAgICBuZ0Rldk1vZGUgJiZcbiAgICAgICAgICAgICAgICBhc3NlcnRFcXVhbChcbiAgICAgICAgICAgICAgICAgICAgaXNMQ29udGFpbmVyKHByZXZMQ29udGFpbmVyKSwgdHJ1ZSxcbiAgICAgICAgICAgICAgICAgICAgJ0FuIGF0dGFjaGVkIHZpZXcgc2hvdWxkIGhhdmUgaXRzIFBBUkVOVCBwb2ludCB0byBhIGNvbnRhaW5lci4nKTtcblxuXG4gICAgICAgICAgICAvLyBXZSBuZWVkIHRvIHJlLWNyZWF0ZSBhIFIzVmlld0NvbnRhaW5lclJlZiBpbnN0YW5jZSBzaW5jZSB0aG9zZSBhcmUgbm90IHN0b3JlZCBvblxuICAgICAgICAgICAgLy8gTFZpZXcgKG5vciBhbnl3aGVyZSBlbHNlKS5cbiAgICAgICAgICAgIGNvbnN0IHByZXZWQ1JlZiA9IG5ldyBSM1ZpZXdDb250YWluZXJSZWYoXG4gICAgICAgICAgICAgICAgcHJldkxDb250YWluZXIsIHByZXZMQ29udGFpbmVyW1RfSE9TVF0gYXMgVERpcmVjdGl2ZUhvc3ROb2RlLFxuICAgICAgICAgICAgICAgIHByZXZMQ29udGFpbmVyW1BBUkVOVF0pO1xuXG4gICAgICAgICAgICBwcmV2VkNSZWYuZGV0YWNoKHByZXZWQ1JlZi5pbmRleE9mKHZpZXdSZWYpKTtcbiAgICAgICAgICB9XG4gICAgICAgIH1cblxuICAgICAgICBjb25zdCBhZGp1c3RlZElkeCA9IHRoaXMuX2FkanVzdEluZGV4KGluZGV4KTtcbiAgICAgICAgaW5zZXJ0Vmlldyh0VmlldywgbFZpZXcsIHRoaXMuX2xDb250YWluZXIsIGFkanVzdGVkSWR4KTtcblxuICAgICAgICBjb25zdCBiZWZvcmVOb2RlID0gZ2V0QmVmb3JlTm9kZUZvclZpZXcoYWRqdXN0ZWRJZHgsIHRoaXMuX2xDb250YWluZXIpO1xuICAgICAgICBhZGRSZW1vdmVWaWV3RnJvbUNvbnRhaW5lcih0VmlldywgbFZpZXcsIHRydWUsIGJlZm9yZU5vZGUpO1xuXG4gICAgICAgICh2aWV3UmVmIGFzIFZpZXdSZWY8YW55PikuYXR0YWNoVG9WaWV3Q29udGFpbmVyUmVmKHRoaXMpO1xuICAgICAgICBhZGRUb0FycmF5KHRoaXMuX2xDb250YWluZXJbVklFV19SRUZTXSEsIGFkanVzdGVkSWR4LCB2aWV3UmVmKTtcblxuICAgICAgICByZXR1cm4gdmlld1JlZjtcbiAgICAgIH1cblxuICAgICAgbW92ZSh2aWV3UmVmOiB2aWV3RW5naW5lX1ZpZXdSZWYsIG5ld0luZGV4OiBudW1iZXIpOiB2aWV3RW5naW5lX1ZpZXdSZWYge1xuICAgICAgICBpZiAodmlld1JlZi5kZXN0cm95ZWQpIHtcbiAgICAgICAgICB0aHJvdyBuZXcgRXJyb3IoJ0Nhbm5vdCBtb3ZlIGEgZGVzdHJveWVkIFZpZXcgaW4gYSBWaWV3Q29udGFpbmVyIScpO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiB0aGlzLmluc2VydCh2aWV3UmVmLCBuZXdJbmRleCk7XG4gICAgICB9XG5cbiAgICAgIGluZGV4T2Yodmlld1JlZjogdmlld0VuZ2luZV9WaWV3UmVmKTogbnVtYmVyIHtcbiAgICAgICAgY29uc3Qgdmlld1JlZnNBcnIgPSB0aGlzLl9sQ29udGFpbmVyW1ZJRVdfUkVGU107XG4gICAgICAgIHJldHVybiB2aWV3UmVmc0FyciAhPT0gbnVsbCA/IHZpZXdSZWZzQXJyLmluZGV4T2Yodmlld1JlZikgOiAtMTtcbiAgICAgIH1cblxuICAgICAgcmVtb3ZlKGluZGV4PzogbnVtYmVyKTogdm9pZCB7XG4gICAgICAgIHRoaXMuYWxsb2NhdGVDb250YWluZXJJZk5lZWRlZCgpO1xuICAgICAgICBjb25zdCBhZGp1c3RlZElkeCA9IHRoaXMuX2FkanVzdEluZGV4KGluZGV4LCAtMSk7XG4gICAgICAgIHJlbW92ZVZpZXcodGhpcy5fbENvbnRhaW5lciwgYWRqdXN0ZWRJZHgpO1xuICAgICAgICByZW1vdmVGcm9tQXJyYXkodGhpcy5fbENvbnRhaW5lcltWSUVXX1JFRlNdISwgYWRqdXN0ZWRJZHgpO1xuICAgICAgfVxuXG4gICAgICBkZXRhY2goaW5kZXg/OiBudW1iZXIpOiB2aWV3RW5naW5lX1ZpZXdSZWZ8bnVsbCB7XG4gICAgICAgIHRoaXMuYWxsb2NhdGVDb250YWluZXJJZk5lZWRlZCgpO1xuICAgICAgICBjb25zdCBhZGp1c3RlZElkeCA9IHRoaXMuX2FkanVzdEluZGV4KGluZGV4LCAtMSk7XG4gICAgICAgIGNvbnN0IHZpZXcgPSBkZXRhY2hWaWV3KHRoaXMuX2xDb250YWluZXIsIGFkanVzdGVkSWR4KTtcblxuICAgICAgICBjb25zdCB3YXNEZXRhY2hlZCA9XG4gICAgICAgICAgICB2aWV3ICYmIHJlbW92ZUZyb21BcnJheSh0aGlzLl9sQ29udGFpbmVyW1ZJRVdfUkVGU10hLCBhZGp1c3RlZElkeCkgIT0gbnVsbDtcbiAgICAgICAgcmV0dXJuIHdhc0RldGFjaGVkID8gbmV3IFZpZXdSZWYodmlldyEpIDogbnVsbDtcbiAgICAgIH1cblxuICAgICAgcHJpdmF0ZSBfYWRqdXN0SW5kZXgoaW5kZXg/OiBudW1iZXIsIHNoaWZ0OiBudW1iZXIgPSAwKSB7XG4gICAgICAgIGlmIChpbmRleCA9PSBudWxsKSB7XG4gICAgICAgICAgcmV0dXJuIHRoaXMubGVuZ3RoICsgc2hpZnQ7XG4gICAgICAgIH1cbiAgICAgICAgaWYgKG5nRGV2TW9kZSkge1xuICAgICAgICAgIGFzc2VydEdyZWF0ZXJUaGFuKGluZGV4LCAtMSwgYFZpZXdSZWYgaW5kZXggbXVzdCBiZSBwb3NpdGl2ZSwgZ290ICR7aW5kZXh9YCk7XG4gICAgICAgICAgLy8gKzEgYmVjYXVzZSBpdCdzIGxlZ2FsIHRvIGluc2VydCBhdCB0aGUgZW5kLlxuICAgICAgICAgIGFzc2VydExlc3NUaGFuKGluZGV4LCB0aGlzLmxlbmd0aCArIDEgKyBzaGlmdCwgJ2luZGV4Jyk7XG4gICAgICAgIH1cbiAgICAgICAgcmV0dXJuIGluZGV4O1xuICAgICAgfVxuXG4gICAgICBwcml2YXRlIGFsbG9jYXRlQ29udGFpbmVySWZOZWVkZWQoKTogdm9pZCB7XG4gICAgICAgIGlmICh0aGlzLl9sQ29udGFpbmVyW1ZJRVdfUkVGU10gPT09IG51bGwpIHtcbiAgICAgICAgICB0aGlzLl9sQ29udGFpbmVyW1ZJRVdfUkVGU10gPSBbXTtcbiAgICAgICAgfVxuICAgICAgfVxuICAgIH07XG4gIH1cblxuICBuZ0Rldk1vZGUgJiZcbiAgICAgIGFzc2VydE5vZGVPZlBvc3NpYmxlVHlwZXMoXG4gICAgICAgICAgaG9zdFROb2RlLCBbVE5vZGVUeXBlLkNvbnRhaW5lciwgVE5vZGVUeXBlLkVsZW1lbnQsIFROb2RlVHlwZS5FbGVtZW50Q29udGFpbmVyXSk7XG5cbiAgbGV0IGxDb250YWluZXI6IExDb250YWluZXI7XG4gIGNvbnN0IHNsb3RWYWx1ZSA9IGhvc3RWaWV3W2hvc3RUTm9kZS5pbmRleF07XG4gIGlmIChpc0xDb250YWluZXIoc2xvdFZhbHVlKSkge1xuICAgIC8vIElmIHRoZSBob3N0IGlzIGEgY29udGFpbmVyLCB3ZSBkb24ndCBuZWVkIHRvIGNyZWF0ZSBhIG5ldyBMQ29udGFpbmVyXG4gICAgbENvbnRhaW5lciA9IHNsb3RWYWx1ZTtcbiAgfSBlbHNlIHtcbiAgICBsZXQgY29tbWVudE5vZGU6IFJDb21tZW50O1xuICAgIC8vIElmIHRoZSBob3N0IGlzIGFuIGVsZW1lbnQgY29udGFpbmVyLCB0aGUgbmF0aXZlIGhvc3QgZWxlbWVudCBpcyBndWFyYW50ZWVkIHRvIGJlIGFcbiAgICAvLyBjb21tZW50IGFuZCB3ZSBjYW4gcmV1c2UgdGhhdCBjb21tZW50IGFzIGFuY2hvciBlbGVtZW50IGZvciB0aGUgbmV3IExDb250YWluZXIuXG4gICAgLy8gVGhlIGNvbW1lbnQgbm9kZSBpbiBxdWVzdGlvbiBpcyBhbHJlYWR5IHBhcnQgb2YgdGhlIERPTSBzdHJ1Y3R1cmUgc28gd2UgZG9uJ3QgbmVlZCB0byBhcHBlbmRcbiAgICAvLyBpdCBhZ2Fpbi5cbiAgICBpZiAoaG9zdFROb2RlLnR5cGUgPT09IFROb2RlVHlwZS5FbGVtZW50Q29udGFpbmVyKSB7XG4gICAgICBjb21tZW50Tm9kZSA9IHVud3JhcFJOb2RlKHNsb3RWYWx1ZSkgYXMgUkNvbW1lbnQ7XG4gICAgfSBlbHNlIHtcbiAgICAgIG5nRGV2TW9kZSAmJiBuZ0Rldk1vZGUucmVuZGVyZXJDcmVhdGVDb21tZW50Kys7XG4gICAgICBjb21tZW50Tm9kZSA9IGhvc3RWaWV3W1JFTkRFUkVSXS5jcmVhdGVDb21tZW50KG5nRGV2TW9kZSA/ICdjb250YWluZXInIDogJycpO1xuXG4gICAgICAvLyBBIGBWaWV3Q29udGFpbmVyUmVmYCBjYW4gYmUgaW5qZWN0ZWQgYnkgdGhlIHJvb3QgKHRvcG1vc3QgLyBib290c3RyYXBwZWQpIGNvbXBvbmVudC4gSW5cbiAgICAgIC8vIHRoaXMgY2FzZSB3ZSBjYW4ndCB1c2UgVFZpZXcgLyBUTm9kZSBkYXRhIHN0cnVjdHVyZXMgdG8gaW5zZXJ0IGNvbnRhaW5lcidzIG1hcmtlciBub2RlXG4gICAgICAvLyAoYm90aCBhIHBhcmVudCBvZiBhIGNvbW1lbnQgbm9kZSBhbmQgdGhlIGNvbW1lbnQgbm9kZSBpdHNlbGYgYXJlIG5vdCBwYXJ0IG9mIGFueSB2aWV3KS4gSW5cbiAgICAgIC8vIHRoaXMgc3BlY2lmaWMgY2FzZSB3ZSB1c2UgbG93LWxldmVsIERPTSBtYW5pcHVsYXRpb24gdG8gaW5zZXJ0IGNvbnRhaW5lcidzIG1hcmtlciAoY29tbWVudClcbiAgICAgIC8vIG5vZGUuXG4gICAgICBpZiAoaXNSb290Vmlldyhob3N0VmlldykpIHtcbiAgICAgICAgY29uc3QgcmVuZGVyZXIgPSBob3N0Vmlld1tSRU5ERVJFUl07XG4gICAgICAgIGNvbnN0IGhvc3ROYXRpdmUgPSBnZXROYXRpdmVCeVROb2RlKGhvc3RUTm9kZSwgaG9zdFZpZXcpITtcbiAgICAgICAgY29uc3QgcGFyZW50T2ZIb3N0TmF0aXZlID0gbmF0aXZlUGFyZW50Tm9kZShyZW5kZXJlciwgaG9zdE5hdGl2ZSk7XG4gICAgICAgIG5hdGl2ZUluc2VydEJlZm9yZShcbiAgICAgICAgICAgIHJlbmRlcmVyLCBwYXJlbnRPZkhvc3ROYXRpdmUhLCBjb21tZW50Tm9kZSwgbmF0aXZlTmV4dFNpYmxpbmcocmVuZGVyZXIsIGhvc3ROYXRpdmUpKTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIGFwcGVuZENoaWxkKGhvc3RWaWV3W1RWSUVXXSwgaG9zdFZpZXcsIGNvbW1lbnROb2RlLCBob3N0VE5vZGUpO1xuICAgICAgfVxuICAgIH1cblxuICAgIGhvc3RWaWV3W2hvc3RUTm9kZS5pbmRleF0gPSBsQ29udGFpbmVyID1cbiAgICAgICAgY3JlYXRlTENvbnRhaW5lcihzbG90VmFsdWUsIGhvc3RWaWV3LCBjb21tZW50Tm9kZSwgaG9zdFROb2RlKTtcblxuICAgIGFkZFRvVmlld1RyZWUoaG9zdFZpZXcsIGxDb250YWluZXIpO1xuICB9XG5cbiAgcmV0dXJuIG5ldyBSM1ZpZXdDb250YWluZXJSZWYobENvbnRhaW5lciwgaG9zdFROb2RlLCBob3N0Vmlldyk7XG59XG5cblxuLyoqIFJldHVybnMgYSBDaGFuZ2VEZXRlY3RvclJlZiAoYS5rLmEuIGEgVmlld1JlZikgKi9cbmV4cG9ydCBmdW5jdGlvbiBpbmplY3RDaGFuZ2VEZXRlY3RvclJlZihpc1BpcGUgPSBmYWxzZSk6IFZpZXdFbmdpbmVfQ2hhbmdlRGV0ZWN0b3JSZWYge1xuICByZXR1cm4gY3JlYXRlVmlld1JlZihnZXRQcmV2aW91c09yUGFyZW50VE5vZGUoKSwgZ2V0TFZpZXcoKSwgaXNQaXBlKTtcbn1cblxuLyoqXG4gKiBDcmVhdGVzIGEgVmlld1JlZiBhbmQgc3RvcmVzIGl0IG9uIHRoZSBpbmplY3RvciBhcyBDaGFuZ2VEZXRlY3RvclJlZiAocHVibGljIGFsaWFzKS5cbiAqXG4gKiBAcGFyYW0gdE5vZGUgVGhlIG5vZGUgdGhhdCBpcyByZXF1ZXN0aW5nIGEgQ2hhbmdlRGV0ZWN0b3JSZWZcbiAqIEBwYXJhbSBsVmlldyBUaGUgdmlldyB0byB3aGljaCB0aGUgbm9kZSBiZWxvbmdzXG4gKiBAcGFyYW0gaXNQaXBlIFdoZXRoZXIgdGhlIHZpZXcgaXMgYmVpbmcgaW5qZWN0ZWQgaW50byBhIHBpcGUuXG4gKiBAcmV0dXJucyBUaGUgQ2hhbmdlRGV0ZWN0b3JSZWYgdG8gdXNlXG4gKi9cbmZ1bmN0aW9uIGNyZWF0ZVZpZXdSZWYodE5vZGU6IFROb2RlLCBsVmlldzogTFZpZXcsIGlzUGlwZTogYm9vbGVhbik6IFZpZXdFbmdpbmVfQ2hhbmdlRGV0ZWN0b3JSZWYge1xuICAvLyBgaXNDb21wb25lbnRWaWV3YCB3aWxsIGJlIHRydWUgZm9yIENvbXBvbmVudCBhbmQgRGlyZWN0aXZlcyAoYnV0IG5vdCBmb3IgUGlwZXMpLlxuICAvLyBTZWUgaHR0cHM6Ly9naXRodWIuY29tL2FuZ3VsYXIvYW5ndWxhci9wdWxsLzMzMDcyIGZvciBwcm9wZXIgZml4XG4gIGNvbnN0IGlzQ29tcG9uZW50VmlldyA9ICFpc1BpcGUgJiYgaXNDb21wb25lbnRIb3N0KHROb2RlKTtcbiAgaWYgKGlzQ29tcG9uZW50Vmlldykge1xuICAgIC8vIFRoZSBMVmlldyByZXByZXNlbnRzIHRoZSBsb2NhdGlvbiB3aGVyZSB0aGUgY29tcG9uZW50IGlzIGRlY2xhcmVkLlxuICAgIC8vIEluc3RlYWQgd2Ugd2FudCB0aGUgTFZpZXcgZm9yIHRoZSBjb21wb25lbnQgVmlldyBhbmQgc28gd2UgbmVlZCB0byBsb29rIGl0IHVwLlxuICAgIGNvbnN0IGNvbXBvbmVudFZpZXcgPSBnZXRDb21wb25lbnRMVmlld0J5SW5kZXgodE5vZGUuaW5kZXgsIGxWaWV3KTsgIC8vIGxvb2sgZG93blxuICAgIHJldHVybiBuZXcgVmlld1JlZihjb21wb25lbnRWaWV3LCBjb21wb25lbnRWaWV3KTtcbiAgfSBlbHNlIGlmIChcbiAgICAgIHROb2RlLnR5cGUgPT09IFROb2RlVHlwZS5FbGVtZW50IHx8IHROb2RlLnR5cGUgPT09IFROb2RlVHlwZS5Db250YWluZXIgfHxcbiAgICAgIHROb2RlLnR5cGUgPT09IFROb2RlVHlwZS5FbGVtZW50Q29udGFpbmVyIHx8IHROb2RlLnR5cGUgPT09IFROb2RlVHlwZS5JY3VDb250YWluZXIpIHtcbiAgICAvLyBUaGUgTFZpZXcgcmVwcmVzZW50cyB0aGUgbG9jYXRpb24gd2hlcmUgdGhlIGluamVjdGlvbiBpcyByZXF1ZXN0ZWQgZnJvbS5cbiAgICAvLyBXZSBuZWVkIHRvIGxvY2F0ZSB0aGUgY29udGFpbmluZyBMVmlldyAoaW4gY2FzZSB3aGVyZSB0aGUgYGxWaWV3YCBpcyBhbiBlbWJlZGRlZCB2aWV3KVxuICAgIGNvbnN0IGhvc3RDb21wb25lbnRWaWV3ID0gbFZpZXdbREVDTEFSQVRJT05fQ09NUE9ORU5UX1ZJRVddOyAgLy8gbG9vayB1cFxuICAgIHJldHVybiBuZXcgVmlld1JlZihob3N0Q29tcG9uZW50VmlldywgbFZpZXcpO1xuICB9XG4gIHJldHVybiBudWxsITtcbn1cblxuLyoqIFJldHVybnMgYSBSZW5kZXJlcjIgKG9yIHRocm93cyB3aGVuIGFwcGxpY2F0aW9uIHdhcyBib290c3RyYXBwZWQgd2l0aCBSZW5kZXJlcjMpICovXG5mdW5jdGlvbiBnZXRPckNyZWF0ZVJlbmRlcmVyMih2aWV3OiBMVmlldyk6IFJlbmRlcmVyMiB7XG4gIGNvbnN0IHJlbmRlcmVyID0gdmlld1tSRU5ERVJFUl07XG4gIGlmIChpc1Byb2NlZHVyYWxSZW5kZXJlcihyZW5kZXJlcikpIHtcbiAgICByZXR1cm4gcmVuZGVyZXIgYXMgUmVuZGVyZXIyO1xuICB9IGVsc2Uge1xuICAgIHRocm93IG5ldyBFcnJvcignQ2Fubm90IGluamVjdCBSZW5kZXJlcjIgd2hlbiB0aGUgYXBwbGljYXRpb24gdXNlcyBSZW5kZXJlcjMhJyk7XG4gIH1cbn1cblxuLyoqIEluamVjdHMgYSBSZW5kZXJlcjIgZm9yIHRoZSBjdXJyZW50IGNvbXBvbmVudC4gKi9cbmV4cG9ydCBmdW5jdGlvbiBpbmplY3RSZW5kZXJlcjIoKTogUmVuZGVyZXIyIHtcbiAgLy8gV2UgbmVlZCB0aGUgUmVuZGVyZXIgdG8gYmUgYmFzZWQgb24gdGhlIGNvbXBvbmVudCB0aGF0IGl0J3MgYmVpbmcgaW5qZWN0ZWQgaW50bywgaG93ZXZlciBzaW5jZVxuICAvLyBESSBoYXBwZW5zIGJlZm9yZSB3ZSd2ZSBlbnRlcmVkIGl0cyB2aWV3LCBgZ2V0TFZpZXdgIHdpbGwgcmV0dXJuIHRoZSBwYXJlbnQgdmlldyBpbnN0ZWFkLlxuICBjb25zdCBsVmlldyA9IGdldExWaWV3KCk7XG4gIGNvbnN0IHROb2RlID0gZ2V0UHJldmlvdXNPclBhcmVudFROb2RlKCk7XG4gIGNvbnN0IG5vZGVBdEluZGV4ID0gZ2V0Q29tcG9uZW50TFZpZXdCeUluZGV4KHROb2RlLmluZGV4LCBsVmlldyk7XG4gIHJldHVybiBnZXRPckNyZWF0ZVJlbmRlcmVyMihpc0xWaWV3KG5vZGVBdEluZGV4KSA/IG5vZGVBdEluZGV4IDogbFZpZXcpO1xufVxuIl19