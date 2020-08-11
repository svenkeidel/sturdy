/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { coerceArray, coerceBooleanProperty } from '@angular/cdk/coercion';
import { ElementRef, EventEmitter, Input, Output, Optional, Directive, ChangeDetectorRef, SkipSelf, Inject, } from '@angular/core';
import { Directionality } from '@angular/cdk/bidi';
import { ScrollDispatcher } from '@angular/cdk/scrolling';
import { CDK_DROP_LIST } from './drag';
import { CdkDropListGroup } from './drop-list-group';
import { DragDrop } from '../drag-drop';
import { CDK_DRAG_CONFIG } from './config';
import { Subject } from 'rxjs';
import { startWith, takeUntil } from 'rxjs/operators';
/** Counter used to generate unique ids for drop zones. */
let _uniqueIdCounter = 0;
const ɵ0 = undefined;
/** Container that wraps a set of draggable items. */
let CdkDropList = /** @class */ (() => {
    class CdkDropList {
        constructor(
        /** Element that the drop list is attached to. */
        element, dragDrop, _changeDetectorRef, _dir, _group, 
        /**
         * @deprecated _scrollDispatcher parameter to become required.
         * @breaking-change 11.0.0
         */
        _scrollDispatcher, config) {
            this.element = element;
            this._changeDetectorRef = _changeDetectorRef;
            this._dir = _dir;
            this._group = _group;
            this._scrollDispatcher = _scrollDispatcher;
            /** Emits when the list has been destroyed. */
            this._destroyed = new Subject();
            /**
             * Other draggable containers that this container is connected to and into which the
             * container's items can be transferred. Can either be references to other drop containers,
             * or their unique IDs.
             */
            this.connectedTo = [];
            /**
             * Unique ID for the drop zone. Can be used as a reference
             * in the `connectedTo` of another `CdkDropList`.
             */
            this.id = `cdk-drop-list-${_uniqueIdCounter++}`;
            /**
             * Function that is used to determine whether an item
             * is allowed to be moved into a drop container.
             */
            this.enterPredicate = () => true;
            /** Emits when the user drops an item inside the container. */
            this.dropped = new EventEmitter();
            /**
             * Emits when the user has moved a new drag item into this container.
             */
            this.entered = new EventEmitter();
            /**
             * Emits when the user removes an item from the container
             * by dragging it into another container.
             */
            this.exited = new EventEmitter();
            /** Emits as the user is swapping items while actively dragging. */
            this.sorted = new EventEmitter();
            /**
             * Keeps track of the items that are registered with this container. Historically we used to
             * do this with a `ContentChildren` query, however queries don't handle transplanted views very
             * well which means that we can't handle cases like dragging the headers of a `mat-table`
             * correctly. What we do instead is to have the items register themselves with the container
             * and then we sort them based on their position in the DOM.
             */
            this._unsortedItems = new Set();
            this._dropListRef = dragDrop.createDropList(element);
            this._dropListRef.data = this;
            if (config) {
                this._assignDefaults(config);
            }
            this._dropListRef.enterPredicate = (drag, drop) => {
                return this.enterPredicate(drag.data, drop.data);
            };
            this._setupInputSyncSubscription(this._dropListRef);
            this._handleEvents(this._dropListRef);
            CdkDropList._dropLists.push(this);
            if (_group) {
                _group._items.add(this);
            }
        }
        /** Whether starting a dragging sequence from this container is disabled. */
        get disabled() {
            return this._disabled || (!!this._group && this._group.disabled);
        }
        set disabled(value) {
            // Usually we sync the directive and ref state right before dragging starts, in order to have
            // a single point of failure and to avoid having to use setters for everything. `disabled` is
            // a special case, because it can prevent the `beforeStarted` event from firing, which can lock
            // the user in a disabled state, so we also need to sync it as it's being set.
            this._dropListRef.disabled = this._disabled = coerceBooleanProperty(value);
        }
        /** Registers an items with the drop list. */
        addItem(item) {
            this._unsortedItems.add(item);
            if (this._dropListRef.isDragging()) {
                this._syncItemsWithRef();
            }
        }
        /** Removes an item from the drop list. */
        removeItem(item) {
            this._unsortedItems.delete(item);
            if (this._dropListRef.isDragging()) {
                this._syncItemsWithRef();
            }
        }
        /** Gets the registered items in the list, sorted by their position in the DOM. */
        getSortedItems() {
            return Array.from(this._unsortedItems).sort((a, b) => {
                const documentPosition = a._dragRef.getVisibleElement().compareDocumentPosition(b._dragRef.getVisibleElement());
                // `compareDocumentPosition` returns a bitmask so we have to use a bitwise operator.
                // https://developer.mozilla.org/en-US/docs/Web/API/Node/compareDocumentPosition
                // tslint:disable-next-line:no-bitwise
                return documentPosition & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1;
            });
        }
        ngOnDestroy() {
            const index = CdkDropList._dropLists.indexOf(this);
            if (index > -1) {
                CdkDropList._dropLists.splice(index, 1);
            }
            if (this._group) {
                this._group._items.delete(this);
            }
            this._unsortedItems.clear();
            this._dropListRef.dispose();
            this._destroyed.next();
            this._destroyed.complete();
        }
        /** Syncs the inputs of the CdkDropList with the options of the underlying DropListRef. */
        _setupInputSyncSubscription(ref) {
            if (this._dir) {
                this._dir.change
                    .pipe(startWith(this._dir.value), takeUntil(this._destroyed))
                    .subscribe(value => ref.withDirection(value));
            }
            ref.beforeStarted.subscribe(() => {
                const siblings = coerceArray(this.connectedTo).map(drop => {
                    return typeof drop === 'string' ?
                        CdkDropList._dropLists.find(list => list.id === drop) : drop;
                });
                if (this._group) {
                    this._group._items.forEach(drop => {
                        if (siblings.indexOf(drop) === -1) {
                            siblings.push(drop);
                        }
                    });
                }
                // Note that we resolve the scrollable parents here so that we delay the resolution
                // as long as possible, ensuring that the element is in its final place in the DOM.
                // @breaking-change 11.0.0 Remove null check for _scrollDispatcher once it's required.
                if (!this._scrollableParentsResolved && this._scrollDispatcher) {
                    const scrollableParents = this._scrollDispatcher
                        .getAncestorScrollContainers(this.element)
                        .map(scrollable => scrollable.getElementRef().nativeElement);
                    this._dropListRef.withScrollableParents(scrollableParents);
                    // Only do this once since it involves traversing the DOM and the parents
                    // shouldn't be able to change without the drop list being destroyed.
                    this._scrollableParentsResolved = true;
                }
                ref.disabled = this.disabled;
                ref.lockAxis = this.lockAxis;
                ref.sortingDisabled = coerceBooleanProperty(this.sortingDisabled);
                ref.autoScrollDisabled = coerceBooleanProperty(this.autoScrollDisabled);
                ref
                    .connectedTo(siblings.filter(drop => drop && drop !== this).map(list => list._dropListRef))
                    .withOrientation(this.orientation);
            });
        }
        /** Handles events from the underlying DropListRef. */
        _handleEvents(ref) {
            ref.beforeStarted.subscribe(() => {
                this._syncItemsWithRef();
                this._changeDetectorRef.markForCheck();
            });
            ref.entered.subscribe(event => {
                this.entered.emit({
                    container: this,
                    item: event.item.data,
                    currentIndex: event.currentIndex
                });
            });
            ref.exited.subscribe(event => {
                this.exited.emit({
                    container: this,
                    item: event.item.data
                });
                this._changeDetectorRef.markForCheck();
            });
            ref.sorted.subscribe(event => {
                this.sorted.emit({
                    previousIndex: event.previousIndex,
                    currentIndex: event.currentIndex,
                    container: this,
                    item: event.item.data
                });
            });
            ref.dropped.subscribe(event => {
                this.dropped.emit({
                    previousIndex: event.previousIndex,
                    currentIndex: event.currentIndex,
                    previousContainer: event.previousContainer.data,
                    container: event.container.data,
                    item: event.item.data,
                    isPointerOverContainer: event.isPointerOverContainer,
                    distance: event.distance
                });
                // Mark for check since all of these events run outside of change
                // detection and we're not guaranteed for something else to have triggered it.
                this._changeDetectorRef.markForCheck();
            });
        }
        /** Assigns the default input values based on a provided config object. */
        _assignDefaults(config) {
            const { lockAxis, draggingDisabled, sortingDisabled, listAutoScrollDisabled, listOrientation } = config;
            this.disabled = draggingDisabled == null ? false : draggingDisabled;
            this.sortingDisabled = sortingDisabled == null ? false : sortingDisabled;
            this.autoScrollDisabled = listAutoScrollDisabled == null ? false : listAutoScrollDisabled;
            this.orientation = listOrientation || 'vertical';
            if (lockAxis) {
                this.lockAxis = lockAxis;
            }
        }
        /** Syncs up the registered drag items with underlying drop list ref. */
        _syncItemsWithRef() {
            this._dropListRef.withItems(this.getSortedItems().map(item => item._dragRef));
        }
    }
    /** Keeps track of the drop lists that are currently on the page. */
    CdkDropList._dropLists = [];
    CdkDropList.decorators = [
        { type: Directive, args: [{
                    selector: '[cdkDropList], cdk-drop-list',
                    exportAs: 'cdkDropList',
                    providers: [
                        // Prevent child drop lists from picking up the same group as their parent.
                        { provide: CdkDropListGroup, useValue: ɵ0 },
                        { provide: CDK_DROP_LIST, useExisting: CdkDropList },
                    ],
                    host: {
                        'class': 'cdk-drop-list',
                        '[id]': 'id',
                        '[class.cdk-drop-list-disabled]': 'disabled',
                        '[class.cdk-drop-list-dragging]': '_dropListRef.isDragging()',
                        '[class.cdk-drop-list-receiving]': '_dropListRef.isReceiving()',
                    }
                },] }
    ];
    CdkDropList.ctorParameters = () => [
        { type: ElementRef },
        { type: DragDrop },
        { type: ChangeDetectorRef },
        { type: Directionality, decorators: [{ type: Optional }] },
        { type: CdkDropListGroup, decorators: [{ type: Optional }, { type: SkipSelf }] },
        { type: ScrollDispatcher },
        { type: undefined, decorators: [{ type: Optional }, { type: Inject, args: [CDK_DRAG_CONFIG,] }] }
    ];
    CdkDropList.propDecorators = {
        connectedTo: [{ type: Input, args: ['cdkDropListConnectedTo',] }],
        data: [{ type: Input, args: ['cdkDropListData',] }],
        orientation: [{ type: Input, args: ['cdkDropListOrientation',] }],
        id: [{ type: Input }],
        lockAxis: [{ type: Input, args: ['cdkDropListLockAxis',] }],
        disabled: [{ type: Input, args: ['cdkDropListDisabled',] }],
        sortingDisabled: [{ type: Input, args: ['cdkDropListSortingDisabled',] }],
        enterPredicate: [{ type: Input, args: ['cdkDropListEnterPredicate',] }],
        autoScrollDisabled: [{ type: Input, args: ['cdkDropListAutoScrollDisabled',] }],
        dropped: [{ type: Output, args: ['cdkDropListDropped',] }],
        entered: [{ type: Output, args: ['cdkDropListEntered',] }],
        exited: [{ type: Output, args: ['cdkDropListExited',] }],
        sorted: [{ type: Output, args: ['cdkDropListSorted',] }]
    };
    return CdkDropList;
})();
export { CdkDropList };
export { ɵ0 };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZHJvcC1saXN0LmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9kcmFnLWRyb3AvZGlyZWN0aXZlcy9kcm9wLWxpc3QudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFlLFdBQVcsRUFBRSxxQkFBcUIsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQ3ZGLE9BQU8sRUFDTCxVQUFVLEVBQ1YsWUFBWSxFQUNaLEtBQUssRUFFTCxNQUFNLEVBQ04sUUFBUSxFQUNSLFNBQVMsRUFDVCxpQkFBaUIsRUFDakIsUUFBUSxFQUNSLE1BQU0sR0FDUCxNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUMsY0FBYyxFQUFDLE1BQU0sbUJBQW1CLENBQUM7QUFDakQsT0FBTyxFQUFDLGdCQUFnQixFQUFDLE1BQU0sd0JBQXdCLENBQUM7QUFDeEQsT0FBTyxFQUFVLGFBQWEsRUFBQyxNQUFNLFFBQVEsQ0FBQztBQUU5QyxPQUFPLEVBQUMsZ0JBQWdCLEVBQUMsTUFBTSxtQkFBbUIsQ0FBQztBQUduRCxPQUFPLEVBQUMsUUFBUSxFQUFDLE1BQU0sY0FBYyxDQUFDO0FBQ3RDLE9BQU8sRUFBZ0QsZUFBZSxFQUFDLE1BQU0sVUFBVSxDQUFDO0FBQ3hGLE9BQU8sRUFBQyxPQUFPLEVBQUMsTUFBTSxNQUFNLENBQUM7QUFDN0IsT0FBTyxFQUFDLFNBQVMsRUFBRSxTQUFTLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQUVwRCwwREFBMEQ7QUFDMUQsSUFBSSxnQkFBZ0IsR0FBRyxDQUFDLENBQUM7V0FlaUIsU0FBUztBQU5uRCxxREFBcUQ7QUFDckQ7SUFBQSxNQWdCYSxXQUFXO1FBK0Z0QjtRQUNJLGlEQUFpRDtRQUMxQyxPQUFnQyxFQUFFLFFBQWtCLEVBQ25ELGtCQUFxQyxFQUFzQixJQUFxQixFQUN4RCxNQUFzQztRQUV0RTs7O1dBR0c7UUFDSyxpQkFBb0MsRUFDUCxNQUF1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QkFBbUI7O0lBbU43QixrQkFBQztLQUFBO1NBdFNZLFdBQVciLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtCb29sZWFuSW5wdXQsIGNvZXJjZUFycmF5LCBjb2VyY2VCb29sZWFuUHJvcGVydHl9IGZyb20gJ0Bhbmd1bGFyL2Nkay9jb2VyY2lvbic7XG5pbXBvcnQge1xuICBFbGVtZW50UmVmLFxuICBFdmVudEVtaXR0ZXIsXG4gIElucHV0LFxuICBPbkRlc3Ryb3ksXG4gIE91dHB1dCxcbiAgT3B0aW9uYWwsXG4gIERpcmVjdGl2ZSxcbiAgQ2hhbmdlRGV0ZWN0b3JSZWYsXG4gIFNraXBTZWxmLFxuICBJbmplY3QsXG59IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtEaXJlY3Rpb25hbGl0eX0gZnJvbSAnQGFuZ3VsYXIvY2RrL2JpZGknO1xuaW1wb3J0IHtTY3JvbGxEaXNwYXRjaGVyfSBmcm9tICdAYW5ndWxhci9jZGsvc2Nyb2xsaW5nJztcbmltcG9ydCB7Q2RrRHJhZywgQ0RLX0RST1BfTElTVH0gZnJvbSAnLi9kcmFnJztcbmltcG9ydCB7Q2RrRHJhZ0Ryb3AsIENka0RyYWdFbnRlciwgQ2RrRHJhZ0V4aXQsIENka0RyYWdTb3J0RXZlbnR9IGZyb20gJy4uL2RyYWctZXZlbnRzJztcbmltcG9ydCB7Q2RrRHJvcExpc3RHcm91cH0gZnJvbSAnLi9kcm9wLWxpc3QtZ3JvdXAnO1xuaW1wb3J0IHtEcm9wTGlzdFJlZn0gZnJvbSAnLi4vZHJvcC1saXN0LXJlZic7XG5pbXBvcnQge0RyYWdSZWZ9IGZyb20gJy4uL2RyYWctcmVmJztcbmltcG9ydCB7RHJhZ0Ryb3B9IGZyb20gJy4uL2RyYWctZHJvcCc7XG5pbXBvcnQge0Ryb3BMaXN0T3JpZW50YXRpb24sIERyYWdBeGlzLCBEcmFnRHJvcENvbmZpZywgQ0RLX0RSQUdfQ09ORklHfSBmcm9tICcuL2NvbmZpZyc7XG5pbXBvcnQge1N1YmplY3R9IGZyb20gJ3J4anMnO1xuaW1wb3J0IHtzdGFydFdpdGgsIHRha2VVbnRpbH0gZnJvbSAncnhqcy9vcGVyYXRvcnMnO1xuXG4vKiogQ291bnRlciB1c2VkIHRvIGdlbmVyYXRlIHVuaXF1ZSBpZHMgZm9yIGRyb3Agem9uZXMuICovXG5sZXQgX3VuaXF1ZUlkQ291bnRlciA9IDA7XG5cbi8qKlxuICogSW50ZXJuYWwgY29tcGlsZS10aW1lLW9ubHkgcmVwcmVzZW50YXRpb24gb2YgYSBgQ2RrRHJvcExpc3RgLlxuICogVXNlZCB0byBhdm9pZCBjaXJjdWxhciBpbXBvcnQgaXNzdWVzIGJldHdlZW4gdGhlIGBDZGtEcm9wTGlzdGAgYW5kIHRoZSBgQ2RrRHJhZ2AuXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgQ2RrRHJvcExpc3RJbnRlcm5hbCBleHRlbmRzIENka0Ryb3BMaXN0IHt9XG5cbi8qKiBDb250YWluZXIgdGhhdCB3cmFwcyBhIHNldCBvZiBkcmFnZ2FibGUgaXRlbXMuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICdbY2RrRHJvcExpc3RdLCBjZGstZHJvcC1saXN0JyxcbiAgZXhwb3J0QXM6ICdjZGtEcm9wTGlzdCcsXG4gIHByb3ZpZGVyczogW1xuICAgIC8vIFByZXZlbnQgY2hpbGQgZHJvcCBsaXN0cyBmcm9tIHBpY2tpbmcgdXAgdGhlIHNhbWUgZ3JvdXAgYXMgdGhlaXIgcGFyZW50LlxuICAgIHtwcm92aWRlOiBDZGtEcm9wTGlzdEdyb3VwLCB1c2VWYWx1ZTogdW5kZWZpbmVkfSxcbiAgICB7cHJvdmlkZTogQ0RLX0RST1BfTElTVCwgdXNlRXhpc3Rpbmc6IENka0Ryb3BMaXN0fSxcbiAgXSxcbiAgaG9zdDoge1xuICAgICdjbGFzcyc6ICdjZGstZHJvcC1saXN0JyxcbiAgICAnW2lkXSc6ICdpZCcsXG4gICAgJ1tjbGFzcy5jZGstZHJvcC1saXN0LWRpc2FibGVkXSc6ICdkaXNhYmxlZCcsXG4gICAgJ1tjbGFzcy5jZGstZHJvcC1saXN0LWRyYWdnaW5nXSc6ICdfZHJvcExpc3RSZWYuaXNEcmFnZ2luZygpJyxcbiAgICAnW2NsYXNzLmNkay1kcm9wLWxpc3QtcmVjZWl2aW5nXSc6ICdfZHJvcExpc3RSZWYuaXNSZWNlaXZpbmcoKScsXG4gIH1cbn0pXG5leHBvcnQgY2xhc3MgQ2RrRHJvcExpc3Q8VCA9IGFueT4gaW1wbGVtZW50cyBPbkRlc3Ryb3kge1xuICAvKiogRW1pdHMgd2hlbiB0aGUgbGlzdCBoYXMgYmVlbiBkZXN0cm95ZWQuICovXG4gIHByaXZhdGUgX2Rlc3Ryb3llZCA9IG5ldyBTdWJqZWN0PHZvaWQ+KCk7XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGVsZW1lbnQncyBzY3JvbGxhYmxlIHBhcmVudHMgaGF2ZSBiZWVuIHJlc29sdmVkLiAqL1xuICBwcml2YXRlIF9zY3JvbGxhYmxlUGFyZW50c1Jlc29sdmVkOiBib29sZWFuO1xuXG4gIC8qKiBLZWVwcyB0cmFjayBvZiB0aGUgZHJvcCBsaXN0cyB0aGF0IGFyZSBjdXJyZW50bHkgb24gdGhlIHBhZ2UuICovXG4gIHByaXZhdGUgc3RhdGljIF9kcm9wTGlzdHM6IENka0Ryb3BMaXN0W10gPSBbXTtcblxuICAvKiogUmVmZXJlbmNlIHRvIHRoZSB1bmRlcmx5aW5nIGRyb3AgbGlzdCBpbnN0YW5jZS4gKi9cbiAgX2Ryb3BMaXN0UmVmOiBEcm9wTGlzdFJlZjxDZGtEcm9wTGlzdDxUPj47XG5cbiAgLyoqXG4gICAqIE90aGVyIGRyYWdnYWJsZSBjb250YWluZXJzIHRoYXQgdGhpcyBjb250YWluZXIgaXMgY29ubmVjdGVkIHRvIGFuZCBpbnRvIHdoaWNoIHRoZVxuICAgKiBjb250YWluZXIncyBpdGVtcyBjYW4gYmUgdHJhbnNmZXJyZWQuIENhbiBlaXRoZXIgYmUgcmVmZXJlbmNlcyB0byBvdGhlciBkcm9wIGNvbnRhaW5lcnMsXG4gICAqIG9yIHRoZWlyIHVuaXF1ZSBJRHMuXG4gICAqL1xuICBASW5wdXQoJ2Nka0Ryb3BMaXN0Q29ubmVjdGVkVG8nKVxuICBjb25uZWN0ZWRUbzogKENka0Ryb3BMaXN0IHwgc3RyaW5nKVtdIHwgQ2RrRHJvcExpc3QgfCBzdHJpbmcgPSBbXTtcblxuICAvKiogQXJiaXRyYXJ5IGRhdGEgdG8gYXR0YWNoIHRvIHRoaXMgY29udGFpbmVyLiAqL1xuICBASW5wdXQoJ2Nka0Ryb3BMaXN0RGF0YScpIGRhdGE6IFQ7XG5cbiAgLyoqIERpcmVjdGlvbiBpbiB3aGljaCB0aGUgbGlzdCBpcyBvcmllbnRlZC4gKi9cbiAgQElucHV0KCdjZGtEcm9wTGlzdE9yaWVudGF0aW9uJykgb3JpZW50YXRpb246IERyb3BMaXN0T3JpZW50YXRpb247XG5cbiAgLyoqXG4gICAqIFVuaXF1ZSBJRCBmb3IgdGhlIGRyb3Agem9uZS4gQ2FuIGJlIHVzZWQgYXMgYSByZWZlcmVuY2VcbiAgICogaW4gdGhlIGBjb25uZWN0ZWRUb2Agb2YgYW5vdGhlciBgQ2RrRHJvcExpc3RgLlxuICAgKi9cbiAgQElucHV0KCkgaWQ6IHN0cmluZyA9IGBjZGstZHJvcC1saXN0LSR7X3VuaXF1ZUlkQ291bnRlcisrfWA7XG5cbiAgLyoqIExvY2tzIHRoZSBwb3NpdGlvbiBvZiB0aGUgZHJhZ2dhYmxlIGVsZW1lbnRzIGluc2lkZSB0aGUgY29udGFpbmVyIGFsb25nIHRoZSBzcGVjaWZpZWQgYXhpcy4gKi9cbiAgQElucHV0KCdjZGtEcm9wTGlzdExvY2tBeGlzJykgbG9ja0F4aXM6IERyYWdBeGlzO1xuXG4gIC8qKiBXaGV0aGVyIHN0YXJ0aW5nIGEgZHJhZ2dpbmcgc2VxdWVuY2UgZnJvbSB0aGlzIGNvbnRhaW5lciBpcyBkaXNhYmxlZC4gKi9cbiAgQElucHV0KCdjZGtEcm9wTGlzdERpc2FibGVkJylcbiAgZ2V0IGRpc2FibGVkKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLl9kaXNhYmxlZCB8fCAoISF0aGlzLl9ncm91cCAmJiB0aGlzLl9ncm91cC5kaXNhYmxlZCk7XG4gIH1cbiAgc2V0IGRpc2FibGVkKHZhbHVlOiBib29sZWFuKSB7XG4gICAgLy8gVXN1YWxseSB3ZSBzeW5jIHRoZSBkaXJlY3RpdmUgYW5kIHJlZiBzdGF0ZSByaWdodCBiZWZvcmUgZHJhZ2dpbmcgc3RhcnRzLCBpbiBvcmRlciB0byBoYXZlXG4gICAgLy8gYSBzaW5nbGUgcG9pbnQgb2YgZmFpbHVyZSBhbmQgdG8gYXZvaWQgaGF2aW5nIHRvIHVzZSBzZXR0ZXJzIGZvciBldmVyeXRoaW5nLiBgZGlzYWJsZWRgIGlzXG4gICAgLy8gYSBzcGVjaWFsIGNhc2UsIGJlY2F1c2UgaXQgY2FuIHByZXZlbnQgdGhlIGBiZWZvcmVTdGFydGVkYCBldmVudCBmcm9tIGZpcmluZywgd2hpY2ggY2FuIGxvY2tcbiAgICAvLyB0aGUgdXNlciBpbiBhIGRpc2FibGVkIHN0YXRlLCBzbyB3ZSBhbHNvIG5lZWQgdG8gc3luYyBpdCBhcyBpdCdzIGJlaW5nIHNldC5cbiAgICB0aGlzLl9kcm9wTGlzdFJlZi5kaXNhYmxlZCA9IHRoaXMuX2Rpc2FibGVkID0gY29lcmNlQm9vbGVhblByb3BlcnR5KHZhbHVlKTtcbiAgfVxuICBwcml2YXRlIF9kaXNhYmxlZDogYm9vbGVhbjtcblxuICAvKiogV2hldGhlciBzb3J0aW5nIHdpdGhpbiB0aGlzIGRyb3AgbGlzdCBpcyBkaXNhYmxlZC4gKi9cbiAgQElucHV0KCdjZGtEcm9wTGlzdFNvcnRpbmdEaXNhYmxlZCcpXG4gIHNvcnRpbmdEaXNhYmxlZDogYm9vbGVhbjtcblxuICAvKipcbiAgICogRnVuY3Rpb24gdGhhdCBpcyB1c2VkIHRvIGRldGVybWluZSB3aGV0aGVyIGFuIGl0ZW1cbiAgICogaXMgYWxsb3dlZCB0byBiZSBtb3ZlZCBpbnRvIGEgZHJvcCBjb250YWluZXIuXG4gICAqL1xuICBASW5wdXQoJ2Nka0Ryb3BMaXN0RW50ZXJQcmVkaWNhdGUnKVxuICBlbnRlclByZWRpY2F0ZTogKGRyYWc6IENka0RyYWcsIGRyb3A6IENka0Ryb3BMaXN0KSA9PiBib29sZWFuID0gKCkgPT4gdHJ1ZVxuXG4gIC8qKiBXaGV0aGVyIHRvIGF1dG8tc2Nyb2xsIHRoZSB2aWV3IHdoZW4gdGhlIHVzZXIgbW92ZXMgdGhlaXIgcG9pbnRlciBjbG9zZSB0byB0aGUgZWRnZXMuICovXG4gIEBJbnB1dCgnY2RrRHJvcExpc3RBdXRvU2Nyb2xsRGlzYWJsZWQnKVxuICBhdXRvU2Nyb2xsRGlzYWJsZWQ6IGJvb2xlYW47XG5cbiAgLyoqIEVtaXRzIHdoZW4gdGhlIHVzZXIgZHJvcHMgYW4gaXRlbSBpbnNpZGUgdGhlIGNvbnRhaW5lci4gKi9cbiAgQE91dHB1dCgnY2RrRHJvcExpc3REcm9wcGVkJylcbiAgZHJvcHBlZDogRXZlbnRFbWl0dGVyPENka0RyYWdEcm9wPFQsIGFueT4+ID0gbmV3IEV2ZW50RW1pdHRlcjxDZGtEcmFnRHJvcDxULCBhbnk+PigpO1xuXG4gIC8qKlxuICAgKiBFbWl0cyB3aGVuIHRoZSB1c2VyIGhhcyBtb3ZlZCBhIG5ldyBkcmFnIGl0ZW0gaW50byB0aGlzIGNvbnRhaW5lci5cbiAgICovXG4gIEBPdXRwdXQoJ2Nka0Ryb3BMaXN0RW50ZXJlZCcpXG4gIGVudGVyZWQ6IEV2ZW50RW1pdHRlcjxDZGtEcmFnRW50ZXI8VD4+ID0gbmV3IEV2ZW50RW1pdHRlcjxDZGtEcmFnRW50ZXI8VD4+KCk7XG5cbiAgLyoqXG4gICAqIEVtaXRzIHdoZW4gdGhlIHVzZXIgcmVtb3ZlcyBhbiBpdGVtIGZyb20gdGhlIGNvbnRhaW5lclxuICAgKiBieSBkcmFnZ2luZyBpdCBpbnRvIGFub3RoZXIgY29udGFpbmVyLlxuICAgKi9cbiAgQE91dHB1dCgnY2RrRHJvcExpc3RFeGl0ZWQnKVxuICBleGl0ZWQ6IEV2ZW50RW1pdHRlcjxDZGtEcmFnRXhpdDxUPj4gPSBuZXcgRXZlbnRFbWl0dGVyPENka0RyYWdFeGl0PFQ+PigpO1xuXG4gIC8qKiBFbWl0cyBhcyB0aGUgdXNlciBpcyBzd2FwcGluZyBpdGVtcyB3aGlsZSBhY3RpdmVseSBkcmFnZ2luZy4gKi9cbiAgQE91dHB1dCgnY2RrRHJvcExpc3RTb3J0ZWQnKVxuICBzb3J0ZWQ6IEV2ZW50RW1pdHRlcjxDZGtEcmFnU29ydEV2ZW50PFQ+PiA9IG5ldyBFdmVudEVtaXR0ZXI8Q2RrRHJhZ1NvcnRFdmVudDxUPj4oKTtcblxuICAvKipcbiAgICogS2VlcHMgdHJhY2sgb2YgdGhlIGl0ZW1zIHRoYXQgYXJlIHJlZ2lzdGVyZWQgd2l0aCB0aGlzIGNvbnRhaW5lci4gSGlzdG9yaWNhbGx5IHdlIHVzZWQgdG9cbiAgICogZG8gdGhpcyB3aXRoIGEgYENvbnRlbnRDaGlsZHJlbmAgcXVlcnksIGhvd2V2ZXIgcXVlcmllcyBkb24ndCBoYW5kbGUgdHJhbnNwbGFudGVkIHZpZXdzIHZlcnlcbiAgICogd2VsbCB3aGljaCBtZWFucyB0aGF0IHdlIGNhbid0IGhhbmRsZSBjYXNlcyBsaWtlIGRyYWdnaW5nIHRoZSBoZWFkZXJzIG9mIGEgYG1hdC10YWJsZWBcbiAgICogY29ycmVjdGx5LiBXaGF0IHdlIGRvIGluc3RlYWQgaXMgdG8gaGF2ZSB0aGUgaXRlbXMgcmVnaXN0ZXIgdGhlbXNlbHZlcyB3aXRoIHRoZSBjb250YWluZXJcbiAgICogYW5kIHRoZW4gd2Ugc29ydCB0aGVtIGJhc2VkIG9uIHRoZWlyIHBvc2l0aW9uIGluIHRoZSBET00uXG4gICAqL1xuICBwcml2YXRlIF91bnNvcnRlZEl0ZW1zID0gbmV3IFNldDxDZGtEcmFnPigpO1xuXG4gIGNvbnN0cnVjdG9yKFxuICAgICAgLyoqIEVsZW1lbnQgdGhhdCB0aGUgZHJvcCBsaXN0IGlzIGF0dGFjaGVkIHRvLiAqL1xuICAgICAgcHVibGljIGVsZW1lbnQ6IEVsZW1lbnRSZWY8SFRNTEVsZW1lbnQ+LCBkcmFnRHJvcDogRHJhZ0Ryb3AsXG4gICAgICBwcml2YXRlIF9jaGFuZ2VEZXRlY3RvclJlZjogQ2hhbmdlRGV0ZWN0b3JSZWYsIEBPcHRpb25hbCgpIHByaXZhdGUgX2Rpcj86IERpcmVjdGlvbmFsaXR5LFxuICAgICAgQE9wdGlvbmFsKCkgQFNraXBTZWxmKCkgcHJpdmF0ZSBfZ3JvdXA/OiBDZGtEcm9wTGlzdEdyb3VwPENka0Ryb3BMaXN0PixcblxuICAgICAgLyoqXG4gICAgICAgKiBAZGVwcmVjYXRlZCBfc2Nyb2xsRGlzcGF0Y2hlciBwYXJhbWV0ZXIgdG8gYmVjb21lIHJlcXVpcmVkLlxuICAgICAgICogQGJyZWFraW5nLWNoYW5nZSAxMS4wLjBcbiAgICAgICAqL1xuICAgICAgcHJpdmF0ZSBfc2Nyb2xsRGlzcGF0Y2hlcj86IFNjcm9sbERpc3BhdGNoZXIsXG4gICAgICBAT3B0aW9uYWwoKSBASW5qZWN0KENES19EUkFHX0NPTkZJRykgY29uZmlnPzogRHJhZ0Ryb3BDb25maWcpIHtcbiAgICB0aGlzLl9kcm9wTGlzdFJlZiA9IGRyYWdEcm9wLmNyZWF0ZURyb3BMaXN0KGVsZW1lbnQpO1xuICAgIHRoaXMuX2Ryb3BMaXN0UmVmLmRhdGEgPSB0aGlzO1xuXG4gICAgaWYgKGNvbmZpZykge1xuICAgICAgdGhpcy5fYXNzaWduRGVmYXVsdHMoY29uZmlnKTtcbiAgICB9XG5cbiAgICB0aGlzLl9kcm9wTGlzdFJlZi5lbnRlclByZWRpY2F0ZSA9IChkcmFnOiBEcmFnUmVmPENka0RyYWc+LCBkcm9wOiBEcm9wTGlzdFJlZjxDZGtEcm9wTGlzdD4pID0+IHtcbiAgICAgIHJldHVybiB0aGlzLmVudGVyUHJlZGljYXRlKGRyYWcuZGF0YSwgZHJvcC5kYXRhKTtcbiAgICB9O1xuXG4gICAgdGhpcy5fc2V0dXBJbnB1dFN5bmNTdWJzY3JpcHRpb24odGhpcy5fZHJvcExpc3RSZWYpO1xuICAgIHRoaXMuX2hhbmRsZUV2ZW50cyh0aGlzLl9kcm9wTGlzdFJlZik7XG4gICAgQ2RrRHJvcExpc3QuX2Ryb3BMaXN0cy5wdXNoKHRoaXMpO1xuXG4gICAgaWYgKF9ncm91cCkge1xuICAgICAgX2dyb3VwLl9pdGVtcy5hZGQodGhpcyk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFJlZ2lzdGVycyBhbiBpdGVtcyB3aXRoIHRoZSBkcm9wIGxpc3QuICovXG4gIGFkZEl0ZW0oaXRlbTogQ2RrRHJhZyk6IHZvaWQge1xuICAgIHRoaXMuX3Vuc29ydGVkSXRlbXMuYWRkKGl0ZW0pO1xuXG4gICAgaWYgKHRoaXMuX2Ryb3BMaXN0UmVmLmlzRHJhZ2dpbmcoKSkge1xuICAgICAgdGhpcy5fc3luY0l0ZW1zV2l0aFJlZigpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBSZW1vdmVzIGFuIGl0ZW0gZnJvbSB0aGUgZHJvcCBsaXN0LiAqL1xuICByZW1vdmVJdGVtKGl0ZW06IENka0RyYWcpOiB2b2lkIHtcbiAgICB0aGlzLl91bnNvcnRlZEl0ZW1zLmRlbGV0ZShpdGVtKTtcblxuICAgIGlmICh0aGlzLl9kcm9wTGlzdFJlZi5pc0RyYWdnaW5nKCkpIHtcbiAgICAgIHRoaXMuX3N5bmNJdGVtc1dpdGhSZWYoKTtcbiAgICB9XG4gIH1cblxuICAvKiogR2V0cyB0aGUgcmVnaXN0ZXJlZCBpdGVtcyBpbiB0aGUgbGlzdCwgc29ydGVkIGJ5IHRoZWlyIHBvc2l0aW9uIGluIHRoZSBET00uICovXG4gIGdldFNvcnRlZEl0ZW1zKCk6IENka0RyYWdbXSB7XG4gICAgcmV0dXJuIEFycmF5LmZyb20odGhpcy5fdW5zb3J0ZWRJdGVtcykuc29ydCgoYTogQ2RrRHJhZywgYjogQ2RrRHJhZykgPT4ge1xuICAgICAgY29uc3QgZG9jdW1lbnRQb3NpdGlvbiA9XG4gICAgICAgICAgYS5fZHJhZ1JlZi5nZXRWaXNpYmxlRWxlbWVudCgpLmNvbXBhcmVEb2N1bWVudFBvc2l0aW9uKGIuX2RyYWdSZWYuZ2V0VmlzaWJsZUVsZW1lbnQoKSk7XG5cbiAgICAgIC8vIGBjb21wYXJlRG9jdW1lbnRQb3NpdGlvbmAgcmV0dXJucyBhIGJpdG1hc2sgc28gd2UgaGF2ZSB0byB1c2UgYSBiaXR3aXNlIG9wZXJhdG9yLlxuICAgICAgLy8gaHR0cHM6Ly9kZXZlbG9wZXIubW96aWxsYS5vcmcvZW4tVVMvZG9jcy9XZWIvQVBJL05vZGUvY29tcGFyZURvY3VtZW50UG9zaXRpb25cbiAgICAgIC8vIHRzbGludDpkaXNhYmxlLW5leHQtbGluZTpuby1iaXR3aXNlXG4gICAgICByZXR1cm4gZG9jdW1lbnRQb3NpdGlvbiAmIE5vZGUuRE9DVU1FTlRfUE9TSVRJT05fRk9MTE9XSU5HID8gLTEgOiAxO1xuICAgIH0pO1xuICB9XG5cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgY29uc3QgaW5kZXggPSBDZGtEcm9wTGlzdC5fZHJvcExpc3RzLmluZGV4T2YodGhpcyk7XG5cbiAgICBpZiAoaW5kZXggPiAtMSkge1xuICAgICAgQ2RrRHJvcExpc3QuX2Ryb3BMaXN0cy5zcGxpY2UoaW5kZXgsIDEpO1xuICAgIH1cblxuICAgIGlmICh0aGlzLl9ncm91cCkge1xuICAgICAgdGhpcy5fZ3JvdXAuX2l0ZW1zLmRlbGV0ZSh0aGlzKTtcbiAgICB9XG5cbiAgICB0aGlzLl91bnNvcnRlZEl0ZW1zLmNsZWFyKCk7XG4gICAgdGhpcy5fZHJvcExpc3RSZWYuZGlzcG9zZSgpO1xuICAgIHRoaXMuX2Rlc3Ryb3llZC5uZXh0KCk7XG4gICAgdGhpcy5fZGVzdHJveWVkLmNvbXBsZXRlKCk7XG4gIH1cblxuICAvKiogU3luY3MgdGhlIGlucHV0cyBvZiB0aGUgQ2RrRHJvcExpc3Qgd2l0aCB0aGUgb3B0aW9ucyBvZiB0aGUgdW5kZXJseWluZyBEcm9wTGlzdFJlZi4gKi9cbiAgcHJpdmF0ZSBfc2V0dXBJbnB1dFN5bmNTdWJzY3JpcHRpb24ocmVmOiBEcm9wTGlzdFJlZjxDZGtEcm9wTGlzdD4pIHtcbiAgICBpZiAodGhpcy5fZGlyKSB7XG4gICAgICB0aGlzLl9kaXIuY2hhbmdlXG4gICAgICAgIC5waXBlKHN0YXJ0V2l0aCh0aGlzLl9kaXIudmFsdWUpLCB0YWtlVW50aWwodGhpcy5fZGVzdHJveWVkKSlcbiAgICAgICAgLnN1YnNjcmliZSh2YWx1ZSA9PiByZWYud2l0aERpcmVjdGlvbih2YWx1ZSkpO1xuICAgIH1cblxuICAgIHJlZi5iZWZvcmVTdGFydGVkLnN1YnNjcmliZSgoKSA9PiB7XG4gICAgICBjb25zdCBzaWJsaW5ncyA9IGNvZXJjZUFycmF5KHRoaXMuY29ubmVjdGVkVG8pLm1hcChkcm9wID0+IHtcbiAgICAgICAgcmV0dXJuIHR5cGVvZiBkcm9wID09PSAnc3RyaW5nJyA/XG4gICAgICAgICAgICBDZGtEcm9wTGlzdC5fZHJvcExpc3RzLmZpbmQobGlzdCA9PiBsaXN0LmlkID09PSBkcm9wKSEgOiBkcm9wO1xuICAgICAgfSk7XG5cbiAgICAgIGlmICh0aGlzLl9ncm91cCkge1xuICAgICAgICB0aGlzLl9ncm91cC5faXRlbXMuZm9yRWFjaChkcm9wID0+IHtcbiAgICAgICAgICBpZiAoc2libGluZ3MuaW5kZXhPZihkcm9wKSA9PT0gLTEpIHtcbiAgICAgICAgICAgIHNpYmxpbmdzLnB1c2goZHJvcCk7XG4gICAgICAgICAgfVxuICAgICAgICB9KTtcbiAgICAgIH1cblxuICAgICAgLy8gTm90ZSB0aGF0IHdlIHJlc29sdmUgdGhlIHNjcm9sbGFibGUgcGFyZW50cyBoZXJlIHNvIHRoYXQgd2UgZGVsYXkgdGhlIHJlc29sdXRpb25cbiAgICAgIC8vIGFzIGxvbmcgYXMgcG9zc2libGUsIGVuc3VyaW5nIHRoYXQgdGhlIGVsZW1lbnQgaXMgaW4gaXRzIGZpbmFsIHBsYWNlIGluIHRoZSBET00uXG4gICAgICAvLyBAYnJlYWtpbmctY2hhbmdlIDExLjAuMCBSZW1vdmUgbnVsbCBjaGVjayBmb3IgX3Njcm9sbERpc3BhdGNoZXIgb25jZSBpdCdzIHJlcXVpcmVkLlxuICAgICAgaWYgKCF0aGlzLl9zY3JvbGxhYmxlUGFyZW50c1Jlc29sdmVkICYmIHRoaXMuX3Njcm9sbERpc3BhdGNoZXIpIHtcbiAgICAgICAgY29uc3Qgc2Nyb2xsYWJsZVBhcmVudHMgPSB0aGlzLl9zY3JvbGxEaXNwYXRjaGVyXG4gICAgICAgICAgLmdldEFuY2VzdG9yU2Nyb2xsQ29udGFpbmVycyh0aGlzLmVsZW1lbnQpXG4gICAgICAgICAgLm1hcChzY3JvbGxhYmxlID0+IHNjcm9sbGFibGUuZ2V0RWxlbWVudFJlZigpLm5hdGl2ZUVsZW1lbnQpO1xuICAgICAgICB0aGlzLl9kcm9wTGlzdFJlZi53aXRoU2Nyb2xsYWJsZVBhcmVudHMoc2Nyb2xsYWJsZVBhcmVudHMpO1xuXG4gICAgICAgIC8vIE9ubHkgZG8gdGhpcyBvbmNlIHNpbmNlIGl0IGludm9sdmVzIHRyYXZlcnNpbmcgdGhlIERPTSBhbmQgdGhlIHBhcmVudHNcbiAgICAgICAgLy8gc2hvdWxkbid0IGJlIGFibGUgdG8gY2hhbmdlIHdpdGhvdXQgdGhlIGRyb3AgbGlzdCBiZWluZyBkZXN0cm95ZWQuXG4gICAgICAgIHRoaXMuX3Njcm9sbGFibGVQYXJlbnRzUmVzb2x2ZWQgPSB0cnVlO1xuICAgICAgfVxuXG4gICAgICByZWYuZGlzYWJsZWQgPSB0aGlzLmRpc2FibGVkO1xuICAgICAgcmVmLmxvY2tBeGlzID0gdGhpcy5sb2NrQXhpcztcbiAgICAgIHJlZi5zb3J0aW5nRGlzYWJsZWQgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkodGhpcy5zb3J0aW5nRGlzYWJsZWQpO1xuICAgICAgcmVmLmF1dG9TY3JvbGxEaXNhYmxlZCA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh0aGlzLmF1dG9TY3JvbGxEaXNhYmxlZCk7XG4gICAgICByZWZcbiAgICAgICAgLmNvbm5lY3RlZFRvKHNpYmxpbmdzLmZpbHRlcihkcm9wID0+IGRyb3AgJiYgZHJvcCAhPT0gdGhpcykubWFwKGxpc3QgPT4gbGlzdC5fZHJvcExpc3RSZWYpKVxuICAgICAgICAud2l0aE9yaWVudGF0aW9uKHRoaXMub3JpZW50YXRpb24pO1xuICAgIH0pO1xuICB9XG5cbiAgLyoqIEhhbmRsZXMgZXZlbnRzIGZyb20gdGhlIHVuZGVybHlpbmcgRHJvcExpc3RSZWYuICovXG4gIHByaXZhdGUgX2hhbmRsZUV2ZW50cyhyZWY6IERyb3BMaXN0UmVmPENka0Ryb3BMaXN0Pikge1xuICAgIHJlZi5iZWZvcmVTdGFydGVkLnN1YnNjcmliZSgoKSA9PiB7XG4gICAgICB0aGlzLl9zeW5jSXRlbXNXaXRoUmVmKCk7XG4gICAgICB0aGlzLl9jaGFuZ2VEZXRlY3RvclJlZi5tYXJrRm9yQ2hlY2soKTtcbiAgICB9KTtcblxuICAgIHJlZi5lbnRlcmVkLnN1YnNjcmliZShldmVudCA9PiB7XG4gICAgICB0aGlzLmVudGVyZWQuZW1pdCh7XG4gICAgICAgIGNvbnRhaW5lcjogdGhpcyxcbiAgICAgICAgaXRlbTogZXZlbnQuaXRlbS5kYXRhLFxuICAgICAgICBjdXJyZW50SW5kZXg6IGV2ZW50LmN1cnJlbnRJbmRleFxuICAgICAgfSk7XG4gICAgfSk7XG5cbiAgICByZWYuZXhpdGVkLnN1YnNjcmliZShldmVudCA9PiB7XG4gICAgICB0aGlzLmV4aXRlZC5lbWl0KHtcbiAgICAgICAgY29udGFpbmVyOiB0aGlzLFxuICAgICAgICBpdGVtOiBldmVudC5pdGVtLmRhdGFcbiAgICAgIH0pO1xuICAgICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3JSZWYubWFya0ZvckNoZWNrKCk7XG4gICAgfSk7XG5cbiAgICByZWYuc29ydGVkLnN1YnNjcmliZShldmVudCA9PiB7XG4gICAgICB0aGlzLnNvcnRlZC5lbWl0KHtcbiAgICAgICAgcHJldmlvdXNJbmRleDogZXZlbnQucHJldmlvdXNJbmRleCxcbiAgICAgICAgY3VycmVudEluZGV4OiBldmVudC5jdXJyZW50SW5kZXgsXG4gICAgICAgIGNvbnRhaW5lcjogdGhpcyxcbiAgICAgICAgaXRlbTogZXZlbnQuaXRlbS5kYXRhXG4gICAgICB9KTtcbiAgICB9KTtcblxuICAgIHJlZi5kcm9wcGVkLnN1YnNjcmliZShldmVudCA9PiB7XG4gICAgICB0aGlzLmRyb3BwZWQuZW1pdCh7XG4gICAgICAgIHByZXZpb3VzSW5kZXg6IGV2ZW50LnByZXZpb3VzSW5kZXgsXG4gICAgICAgIGN1cnJlbnRJbmRleDogZXZlbnQuY3VycmVudEluZGV4LFxuICAgICAgICBwcmV2aW91c0NvbnRhaW5lcjogZXZlbnQucHJldmlvdXNDb250YWluZXIuZGF0YSxcbiAgICAgICAgY29udGFpbmVyOiBldmVudC5jb250YWluZXIuZGF0YSxcbiAgICAgICAgaXRlbTogZXZlbnQuaXRlbS5kYXRhLFxuICAgICAgICBpc1BvaW50ZXJPdmVyQ29udGFpbmVyOiBldmVudC5pc1BvaW50ZXJPdmVyQ29udGFpbmVyLFxuICAgICAgICBkaXN0YW5jZTogZXZlbnQuZGlzdGFuY2VcbiAgICAgIH0pO1xuXG4gICAgICAvLyBNYXJrIGZvciBjaGVjayBzaW5jZSBhbGwgb2YgdGhlc2UgZXZlbnRzIHJ1biBvdXRzaWRlIG9mIGNoYW5nZVxuICAgICAgLy8gZGV0ZWN0aW9uIGFuZCB3ZSdyZSBub3QgZ3VhcmFudGVlZCBmb3Igc29tZXRoaW5nIGVsc2UgdG8gaGF2ZSB0cmlnZ2VyZWQgaXQuXG4gICAgICB0aGlzLl9jaGFuZ2VEZXRlY3RvclJlZi5tYXJrRm9yQ2hlY2soKTtcbiAgICB9KTtcbiAgfVxuXG4gIC8qKiBBc3NpZ25zIHRoZSBkZWZhdWx0IGlucHV0IHZhbHVlcyBiYXNlZCBvbiBhIHByb3ZpZGVkIGNvbmZpZyBvYmplY3QuICovXG4gIHByaXZhdGUgX2Fzc2lnbkRlZmF1bHRzKGNvbmZpZzogRHJhZ0Ryb3BDb25maWcpIHtcbiAgICBjb25zdCB7XG4gICAgICBsb2NrQXhpcywgZHJhZ2dpbmdEaXNhYmxlZCwgc29ydGluZ0Rpc2FibGVkLCBsaXN0QXV0b1Njcm9sbERpc2FibGVkLCBsaXN0T3JpZW50YXRpb25cbiAgICB9ID0gY29uZmlnO1xuXG4gICAgdGhpcy5kaXNhYmxlZCA9IGRyYWdnaW5nRGlzYWJsZWQgPT0gbnVsbCA/IGZhbHNlIDogZHJhZ2dpbmdEaXNhYmxlZDtcbiAgICB0aGlzLnNvcnRpbmdEaXNhYmxlZCA9IHNvcnRpbmdEaXNhYmxlZCA9PSBudWxsID8gZmFsc2UgOiBzb3J0aW5nRGlzYWJsZWQ7XG4gICAgdGhpcy5hdXRvU2Nyb2xsRGlzYWJsZWQgPSBsaXN0QXV0b1Njcm9sbERpc2FibGVkID09IG51bGwgPyBmYWxzZSA6IGxpc3RBdXRvU2Nyb2xsRGlzYWJsZWQ7XG4gICAgdGhpcy5vcmllbnRhdGlvbiA9IGxpc3RPcmllbnRhdGlvbiB8fCAndmVydGljYWwnO1xuXG4gICAgaWYgKGxvY2tBeGlzKSB7XG4gICAgICB0aGlzLmxvY2tBeGlzID0gbG9ja0F4aXM7XG4gICAgfVxuICB9XG5cbiAgLyoqIFN5bmNzIHVwIHRoZSByZWdpc3RlcmVkIGRyYWcgaXRlbXMgd2l0aCB1bmRlcmx5aW5nIGRyb3AgbGlzdCByZWYuICovXG4gIHByaXZhdGUgX3N5bmNJdGVtc1dpdGhSZWYoKSB7XG4gICAgdGhpcy5fZHJvcExpc3RSZWYud2l0aEl0ZW1zKHRoaXMuZ2V0U29ydGVkSXRlbXMoKS5tYXAoaXRlbSA9PiBpdGVtLl9kcmFnUmVmKSk7XG4gIH1cblxuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfZGlzYWJsZWQ6IEJvb2xlYW5JbnB1dDtcbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX3NvcnRpbmdEaXNhYmxlZDogQm9vbGVhbklucHV0O1xuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfYXV0b1Njcm9sbERpc2FibGVkOiBCb29sZWFuSW5wdXQ7XG59XG4iXX0=