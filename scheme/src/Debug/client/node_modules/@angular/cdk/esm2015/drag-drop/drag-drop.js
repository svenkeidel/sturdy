/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Injectable, Inject, NgZone } from '@angular/core';
import { DOCUMENT } from '@angular/common';
import { ViewportRuler } from '@angular/cdk/scrolling';
import { DragRef } from './drag-ref';
import { DropListRef } from './drop-list-ref';
import { DragDropRegistry } from './drag-drop-registry';
import * as i0 from "@angular/core";
import * as i1 from "@angular/common";
import * as i2 from "@angular/cdk/scrolling";
import * as i3 from "./drag-drop-registry";
/** Default configuration to be used when creating a `DragRef`. */
const DEFAULT_CONFIG = {
    dragStartThreshold: 5,
    pointerDirectionChangeThreshold: 5
};
/**
 * Service that allows for drag-and-drop functionality to be attached to DOM elements.
 */
let DragDrop = /** @class */ (() => {
    class DragDrop {
        constructor(_document, _ngZone, _viewportRuler, _dragDropRegistry) {
            this._document = _document;
            this._ngZone = _ngZone;
            this._viewportRuler = _viewportRuler;
            this._dragDropRegistry = _dragDropRegistry;
        }
        /**
         * Turns an element into a draggable item.
         * @param element Element to which to attach the dragging functionality.
         * @param config Object used to configure the dragging behavior.
         */
        createDrag(element, config = DEFAULT_CONFIG) {
            return new DragRef(element, config, this._document, this._ngZone, this._viewportRuler, this._dragDropRegistry);
        }
        /**
         * Turns an element into a drop list.
         * @param element Element to which to attach the drop list functionality.
         */
        createDropList(element) {
            return new DropListRef(element, this._dragDropRegistry, this._document, this._ngZone, this._viewportRuler);
        }
    }
    DragDrop.ɵprov = i0.ɵɵdefineInjectable({ factory: function DragDrop_Factory() { return new DragDrop(i0.ɵɵinject(i1.DOCUMENT), i0.ɵɵinject(i0.NgZone), i0.ɵɵinject(i2.ViewportRuler), i0.ɵɵinject(i3.DragDropRegistry)); }, token: DragDrop, providedIn: "root" });
    DragDrop.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    DragDrop.ctorParameters = () => [
        { type: undefined, decorators: [{ type: Inject, args: [DOCUMENT,] }] },
        { type: NgZone },
        { type: ViewportRuler },
        { type: DragDropRegistry }
    ];
    return DragDrop;
})();
export { DragDrop };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZHJhZy1kcm9wLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9kcmFnLWRyb3AvZHJhZy1kcm9wLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU8sRUFBQyxVQUFVLEVBQUUsTUFBTSxFQUFFLE1BQU0sRUFBYSxNQUFNLGVBQWUsQ0FBQztBQUNyRSxPQUFPLEVBQUMsUUFBUSxFQUFDLE1BQU0saUJBQWlCLENBQUM7QUFDekMsT0FBTyxFQUFDLGFBQWEsRUFBQyxNQUFNLHdCQUF3QixDQUFDO0FBQ3JELE9BQU8sRUFBQyxPQUFPLEVBQWdCLE1BQU0sWUFBWSxDQUFDO0FBQ2xELE9BQU8sRUFBQyxXQUFXLEVBQUMsTUFBTSxpQkFBaUIsQ0FBQztBQUM1QyxPQUFPLEVBQUMsZ0JBQWdCLEVBQUMsTUFBTSxzQkFBc0IsQ0FBQzs7Ozs7QUFFdEQsa0VBQWtFO0FBQ2xFLE1BQU0sY0FBYyxHQUFHO0lBQ3JCLGtCQUFrQixFQUFFLENBQUM7SUFDckIsK0JBQStCLEVBQUUsQ0FBQztDQUNuQyxDQUFDO0FBRUY7O0dBRUc7QUFDSDtJQUFBLE1BQ2EsUUFBUTtRQUNuQixZQUM0QixTQUFjLEVBQ2hDLE9BQWUsRUFDZixjQUE2QixFQUM3QixpQkFBeUQ7WUFIdkMsY0FBUyxHQUFULFNBQVMsQ0FBSztZQUNoQyxZQUFPLEdBQVAsT0FBTyxDQUFRO1lBQ2YsbUJBQWMsR0FBZCxjQUFjLENBQWU7WUFDN0Isc0JBQWlCLEdBQWpCLGlCQUFpQixDQUF3QztRQUFHLENBQUM7UUFFdkU7Ozs7V0FJRztRQUNILFVBQVUsQ0FBVSxPQUE4QyxFQUNwRCxTQUF3QixjQUFjO1lBRWxELE9BQU8sSUFBSSxPQUFPLENBQUksT0FBTyxFQUFFLE1BQU0sRUFBRSxJQUFJLENBQUMsU0FBUyxFQUFFLElBQUksQ0FBQyxPQUFPLEVBQUUsSUFBSSxDQUFDLGNBQWMsRUFDcEYsSUFBSSxDQUFDLGlCQUFpQixDQUFDLENBQUM7UUFDOUIsQ0FBQztRQUVEOzs7V0FHRztRQUNILGNBQWMsQ0FBVSxPQUE4QztZQUNwRSxPQUFPLElBQUksV0FBVyxDQUFJLE9BQU8sRUFBRSxJQUFJLENBQUMsaUJBQWlCLEVBQUUsSUFBSSxDQUFDLFNBQVMsRUFBRSxJQUFJLENBQUMsT0FBTyxFQUNuRixJQUFJLENBQUMsY0FBYyxDQUFDLENBQUM7UUFDM0IsQ0FBQzs7OztnQkEzQkYsVUFBVSxTQUFDLEVBQUMsVUFBVSxFQUFFLE1BQU0sRUFBQzs7O2dEQUczQixNQUFNLFNBQUMsUUFBUTtnQkFuQlEsTUFBTTtnQkFFMUIsYUFBYTtnQkFHYixnQkFBZ0I7O21CQWJ4QjtLQW9EQztTQTNCWSxRQUFRIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7SW5qZWN0YWJsZSwgSW5qZWN0LCBOZ1pvbmUsIEVsZW1lbnRSZWZ9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtET0NVTUVOVH0gZnJvbSAnQGFuZ3VsYXIvY29tbW9uJztcbmltcG9ydCB7Vmlld3BvcnRSdWxlcn0gZnJvbSAnQGFuZ3VsYXIvY2RrL3Njcm9sbGluZyc7XG5pbXBvcnQge0RyYWdSZWYsIERyYWdSZWZDb25maWd9IGZyb20gJy4vZHJhZy1yZWYnO1xuaW1wb3J0IHtEcm9wTGlzdFJlZn0gZnJvbSAnLi9kcm9wLWxpc3QtcmVmJztcbmltcG9ydCB7RHJhZ0Ryb3BSZWdpc3RyeX0gZnJvbSAnLi9kcmFnLWRyb3AtcmVnaXN0cnknO1xuXG4vKiogRGVmYXVsdCBjb25maWd1cmF0aW9uIHRvIGJlIHVzZWQgd2hlbiBjcmVhdGluZyBhIGBEcmFnUmVmYC4gKi9cbmNvbnN0IERFRkFVTFRfQ09ORklHID0ge1xuICBkcmFnU3RhcnRUaHJlc2hvbGQ6IDUsXG4gIHBvaW50ZXJEaXJlY3Rpb25DaGFuZ2VUaHJlc2hvbGQ6IDVcbn07XG5cbi8qKlxuICogU2VydmljZSB0aGF0IGFsbG93cyBmb3IgZHJhZy1hbmQtZHJvcCBmdW5jdGlvbmFsaXR5IHRvIGJlIGF0dGFjaGVkIHRvIERPTSBlbGVtZW50cy5cbiAqL1xuQEluamVjdGFibGUoe3Byb3ZpZGVkSW46ICdyb290J30pXG5leHBvcnQgY2xhc3MgRHJhZ0Ryb3Age1xuICBjb25zdHJ1Y3RvcihcbiAgICBASW5qZWN0KERPQ1VNRU5UKSBwcml2YXRlIF9kb2N1bWVudDogYW55LFxuICAgIHByaXZhdGUgX25nWm9uZTogTmdab25lLFxuICAgIHByaXZhdGUgX3ZpZXdwb3J0UnVsZXI6IFZpZXdwb3J0UnVsZXIsXG4gICAgcHJpdmF0ZSBfZHJhZ0Ryb3BSZWdpc3RyeTogRHJhZ0Ryb3BSZWdpc3RyeTxEcmFnUmVmLCBEcm9wTGlzdFJlZj4pIHt9XG5cbiAgLyoqXG4gICAqIFR1cm5zIGFuIGVsZW1lbnQgaW50byBhIGRyYWdnYWJsZSBpdGVtLlxuICAgKiBAcGFyYW0gZWxlbWVudCBFbGVtZW50IHRvIHdoaWNoIHRvIGF0dGFjaCB0aGUgZHJhZ2dpbmcgZnVuY3Rpb25hbGl0eS5cbiAgICogQHBhcmFtIGNvbmZpZyBPYmplY3QgdXNlZCB0byBjb25maWd1cmUgdGhlIGRyYWdnaW5nIGJlaGF2aW9yLlxuICAgKi9cbiAgY3JlYXRlRHJhZzxUID0gYW55PihlbGVtZW50OiBFbGVtZW50UmVmPEhUTUxFbGVtZW50PiB8IEhUTUxFbGVtZW50LFxuICAgICAgICAgICAgICAgIGNvbmZpZzogRHJhZ1JlZkNvbmZpZyA9IERFRkFVTFRfQ09ORklHKTogRHJhZ1JlZjxUPiB7XG5cbiAgICByZXR1cm4gbmV3IERyYWdSZWY8VD4oZWxlbWVudCwgY29uZmlnLCB0aGlzLl9kb2N1bWVudCwgdGhpcy5fbmdab25lLCB0aGlzLl92aWV3cG9ydFJ1bGVyLFxuICAgICAgICB0aGlzLl9kcmFnRHJvcFJlZ2lzdHJ5KTtcbiAgfVxuXG4gIC8qKlxuICAgKiBUdXJucyBhbiBlbGVtZW50IGludG8gYSBkcm9wIGxpc3QuXG4gICAqIEBwYXJhbSBlbGVtZW50IEVsZW1lbnQgdG8gd2hpY2ggdG8gYXR0YWNoIHRoZSBkcm9wIGxpc3QgZnVuY3Rpb25hbGl0eS5cbiAgICovXG4gIGNyZWF0ZURyb3BMaXN0PFQgPSBhbnk+KGVsZW1lbnQ6IEVsZW1lbnRSZWY8SFRNTEVsZW1lbnQ+IHwgSFRNTEVsZW1lbnQpOiBEcm9wTGlzdFJlZjxUPiB7XG4gICAgcmV0dXJuIG5ldyBEcm9wTGlzdFJlZjxUPihlbGVtZW50LCB0aGlzLl9kcmFnRHJvcFJlZ2lzdHJ5LCB0aGlzLl9kb2N1bWVudCwgdGhpcy5fbmdab25lLFxuICAgICAgICB0aGlzLl92aWV3cG9ydFJ1bGVyKTtcbiAgfVxufVxuIl19