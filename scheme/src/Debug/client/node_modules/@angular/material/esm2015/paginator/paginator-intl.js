/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Injectable, Optional, SkipSelf } from '@angular/core';
import { Subject } from 'rxjs';
import * as i0 from "@angular/core";
/**
 * To modify the labels and text displayed, create a new instance of MatPaginatorIntl and
 * include it in a custom provider
 */
let MatPaginatorIntl = /** @class */ (() => {
    class MatPaginatorIntl {
        constructor() {
            /**
             * Stream to emit from when labels are changed. Use this to notify components when the labels have
             * changed after initialization.
             */
            this.changes = new Subject();
            /** A label for the page size selector. */
            this.itemsPerPageLabel = 'Items per page:';
            /** A label for the button that increments the current page. */
            this.nextPageLabel = 'Next page';
            /** A label for the button that decrements the current page. */
            this.previousPageLabel = 'Previous page';
            /** A label for the button that moves to the first page. */
            this.firstPageLabel = 'First page';
            /** A label for the button that moves to the last page. */
            this.lastPageLabel = 'Last page';
            /** A label for the range of items within the current page and the length of the whole list. */
            this.getRangeLabel = (page, pageSize, length) => {
                if (length == 0 || pageSize == 0) {
                    return `0 of ${length}`;
                }
                length = Math.max(length, 0);
                const startIndex = page * pageSize;
                // If the start index exceeds the list length, do not try and fix the end index to the end.
                const endIndex = startIndex < length ?
                    Math.min(startIndex + pageSize, length) :
                    startIndex + pageSize;
                return `${startIndex + 1} – ${endIndex} of ${length}`;
            };
        }
    }
    MatPaginatorIntl.ɵprov = i0.ɵɵdefineInjectable({ factory: function MatPaginatorIntl_Factory() { return new MatPaginatorIntl(); }, token: MatPaginatorIntl, providedIn: "root" });
    MatPaginatorIntl.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    return MatPaginatorIntl;
})();
export { MatPaginatorIntl };
/** @docs-private */
export function MAT_PAGINATOR_INTL_PROVIDER_FACTORY(parentIntl) {
    return parentIntl || new MatPaginatorIntl();
}
/** @docs-private */
export const MAT_PAGINATOR_INTL_PROVIDER = {
    // If there is already an MatPaginatorIntl available, use that. Otherwise, provide a new one.
    provide: MatPaginatorIntl,
    deps: [[new Optional(), new SkipSelf(), MatPaginatorIntl]],
    useFactory: MAT_PAGINATOR_INTL_PROVIDER_FACTORY
};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGFnaW5hdG9yLWludGwuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvcGFnaW5hdG9yL3BhZ2luYXRvci1pbnRsLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU8sRUFBQyxVQUFVLEVBQUUsUUFBUSxFQUFFLFFBQVEsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUM3RCxPQUFPLEVBQUMsT0FBTyxFQUFDLE1BQU0sTUFBTSxDQUFDOztBQUc3Qjs7O0dBR0c7QUFDSDtJQUFBLE1BQ2EsZ0JBQWdCO1FBRDdCO1lBRUU7OztlQUdHO1lBQ00sWUFBTyxHQUFrQixJQUFJLE9BQU8sRUFBUSxDQUFDO1lBRXRELDBDQUEwQztZQUMxQyxzQkFBaUIsR0FBVyxpQkFBaUIsQ0FBQztZQUU5QywrREFBK0Q7WUFDL0Qsa0JBQWEsR0FBVyxXQUFXLENBQUM7WUFFcEMsK0RBQStEO1lBQy9ELHNCQUFpQixHQUFXLGVBQWUsQ0FBQztZQUU1QywyREFBMkQ7WUFDM0QsbUJBQWMsR0FBVyxZQUFZLENBQUM7WUFFdEMsMERBQTBEO1lBQzFELGtCQUFhLEdBQVcsV0FBVyxDQUFDO1lBRXBDLCtGQUErRjtZQUMvRixrQkFBYSxHQUFHLENBQUMsSUFBWSxFQUFFLFFBQWdCLEVBQUUsTUFBYyxFQUFFLEVBQUU7Z0JBQ2pFLElBQUksTUFBTSxJQUFJLENBQUMsSUFBSSxRQUFRLElBQUksQ0FBQyxFQUFFO29CQUFFLE9BQU8sUUFBUSxNQUFNLEVBQUUsQ0FBQztpQkFBRTtnQkFFOUQsTUFBTSxHQUFHLElBQUksQ0FBQyxHQUFHLENBQUMsTUFBTSxFQUFFLENBQUMsQ0FBQyxDQUFDO2dCQUU3QixNQUFNLFVBQVUsR0FBRyxJQUFJLEdBQUcsUUFBUSxDQUFDO2dCQUVuQywyRkFBMkY7Z0JBQzNGLE1BQU0sUUFBUSxHQUFHLFVBQVUsR0FBRyxNQUFNLENBQUMsQ0FBQztvQkFDbEMsSUFBSSxDQUFDLEdBQUcsQ0FBQyxVQUFVLEdBQUcsUUFBUSxFQUFFLE1BQU0sQ0FBQyxDQUFDLENBQUM7b0JBQ3pDLFVBQVUsR0FBRyxRQUFRLENBQUM7Z0JBRTFCLE9BQU8sR0FBRyxVQUFVLEdBQUcsQ0FBQyxNQUFNLFFBQVEsT0FBTyxNQUFNLEVBQUUsQ0FBQztZQUN4RCxDQUFDLENBQUE7U0FDRjs7OztnQkF0Q0EsVUFBVSxTQUFDLEVBQUMsVUFBVSxFQUFFLE1BQU0sRUFBQzs7MkJBaEJoQztLQXNEQztTQXJDWSxnQkFBZ0I7QUF1QzdCLG9CQUFvQjtBQUNwQixNQUFNLFVBQVUsbUNBQW1DLENBQUMsVUFBNEI7SUFDOUUsT0FBTyxVQUFVLElBQUksSUFBSSxnQkFBZ0IsRUFBRSxDQUFDO0FBQzlDLENBQUM7QUFFRCxvQkFBb0I7QUFDcEIsTUFBTSxDQUFDLE1BQU0sMkJBQTJCLEdBQUc7SUFDekMsNkZBQTZGO0lBQzdGLE9BQU8sRUFBRSxnQkFBZ0I7SUFDekIsSUFBSSxFQUFFLENBQUMsQ0FBQyxJQUFJLFFBQVEsRUFBRSxFQUFFLElBQUksUUFBUSxFQUFFLEVBQUUsZ0JBQWdCLENBQUMsQ0FBQztJQUMxRCxVQUFVLEVBQUUsbUNBQW1DO0NBQ2hELENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtJbmplY3RhYmxlLCBPcHRpb25hbCwgU2tpcFNlbGZ9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtTdWJqZWN0fSBmcm9tICdyeGpzJztcblxuXG4vKipcbiAqIFRvIG1vZGlmeSB0aGUgbGFiZWxzIGFuZCB0ZXh0IGRpc3BsYXllZCwgY3JlYXRlIGEgbmV3IGluc3RhbmNlIG9mIE1hdFBhZ2luYXRvckludGwgYW5kXG4gKiBpbmNsdWRlIGl0IGluIGEgY3VzdG9tIHByb3ZpZGVyXG4gKi9cbkBJbmplY3RhYmxlKHtwcm92aWRlZEluOiAncm9vdCd9KVxuZXhwb3J0IGNsYXNzIE1hdFBhZ2luYXRvckludGwge1xuICAvKipcbiAgICogU3RyZWFtIHRvIGVtaXQgZnJvbSB3aGVuIGxhYmVscyBhcmUgY2hhbmdlZC4gVXNlIHRoaXMgdG8gbm90aWZ5IGNvbXBvbmVudHMgd2hlbiB0aGUgbGFiZWxzIGhhdmVcbiAgICogY2hhbmdlZCBhZnRlciBpbml0aWFsaXphdGlvbi5cbiAgICovXG4gIHJlYWRvbmx5IGNoYW5nZXM6IFN1YmplY3Q8dm9pZD4gPSBuZXcgU3ViamVjdDx2b2lkPigpO1xuXG4gIC8qKiBBIGxhYmVsIGZvciB0aGUgcGFnZSBzaXplIHNlbGVjdG9yLiAqL1xuICBpdGVtc1BlclBhZ2VMYWJlbDogc3RyaW5nID0gJ0l0ZW1zIHBlciBwYWdlOic7XG5cbiAgLyoqIEEgbGFiZWwgZm9yIHRoZSBidXR0b24gdGhhdCBpbmNyZW1lbnRzIHRoZSBjdXJyZW50IHBhZ2UuICovXG4gIG5leHRQYWdlTGFiZWw6IHN0cmluZyA9ICdOZXh0IHBhZ2UnO1xuXG4gIC8qKiBBIGxhYmVsIGZvciB0aGUgYnV0dG9uIHRoYXQgZGVjcmVtZW50cyB0aGUgY3VycmVudCBwYWdlLiAqL1xuICBwcmV2aW91c1BhZ2VMYWJlbDogc3RyaW5nID0gJ1ByZXZpb3VzIHBhZ2UnO1xuXG4gIC8qKiBBIGxhYmVsIGZvciB0aGUgYnV0dG9uIHRoYXQgbW92ZXMgdG8gdGhlIGZpcnN0IHBhZ2UuICovXG4gIGZpcnN0UGFnZUxhYmVsOiBzdHJpbmcgPSAnRmlyc3QgcGFnZSc7XG5cbiAgLyoqIEEgbGFiZWwgZm9yIHRoZSBidXR0b24gdGhhdCBtb3ZlcyB0byB0aGUgbGFzdCBwYWdlLiAqL1xuICBsYXN0UGFnZUxhYmVsOiBzdHJpbmcgPSAnTGFzdCBwYWdlJztcblxuICAvKiogQSBsYWJlbCBmb3IgdGhlIHJhbmdlIG9mIGl0ZW1zIHdpdGhpbiB0aGUgY3VycmVudCBwYWdlIGFuZCB0aGUgbGVuZ3RoIG9mIHRoZSB3aG9sZSBsaXN0LiAqL1xuICBnZXRSYW5nZUxhYmVsID0gKHBhZ2U6IG51bWJlciwgcGFnZVNpemU6IG51bWJlciwgbGVuZ3RoOiBudW1iZXIpID0+IHtcbiAgICBpZiAobGVuZ3RoID09IDAgfHwgcGFnZVNpemUgPT0gMCkgeyByZXR1cm4gYDAgb2YgJHtsZW5ndGh9YDsgfVxuXG4gICAgbGVuZ3RoID0gTWF0aC5tYXgobGVuZ3RoLCAwKTtcblxuICAgIGNvbnN0IHN0YXJ0SW5kZXggPSBwYWdlICogcGFnZVNpemU7XG5cbiAgICAvLyBJZiB0aGUgc3RhcnQgaW5kZXggZXhjZWVkcyB0aGUgbGlzdCBsZW5ndGgsIGRvIG5vdCB0cnkgYW5kIGZpeCB0aGUgZW5kIGluZGV4IHRvIHRoZSBlbmQuXG4gICAgY29uc3QgZW5kSW5kZXggPSBzdGFydEluZGV4IDwgbGVuZ3RoID9cbiAgICAgICAgTWF0aC5taW4oc3RhcnRJbmRleCArIHBhZ2VTaXplLCBsZW5ndGgpIDpcbiAgICAgICAgc3RhcnRJbmRleCArIHBhZ2VTaXplO1xuXG4gICAgcmV0dXJuIGAke3N0YXJ0SW5kZXggKyAxfSDigJMgJHtlbmRJbmRleH0gb2YgJHtsZW5ndGh9YDtcbiAgfVxufVxuXG4vKiogQGRvY3MtcHJpdmF0ZSAqL1xuZXhwb3J0IGZ1bmN0aW9uIE1BVF9QQUdJTkFUT1JfSU5UTF9QUk9WSURFUl9GQUNUT1JZKHBhcmVudEludGw6IE1hdFBhZ2luYXRvckludGwpIHtcbiAgcmV0dXJuIHBhcmVudEludGwgfHwgbmV3IE1hdFBhZ2luYXRvckludGwoKTtcbn1cblxuLyoqIEBkb2NzLXByaXZhdGUgKi9cbmV4cG9ydCBjb25zdCBNQVRfUEFHSU5BVE9SX0lOVExfUFJPVklERVIgPSB7XG4gIC8vIElmIHRoZXJlIGlzIGFscmVhZHkgYW4gTWF0UGFnaW5hdG9ySW50bCBhdmFpbGFibGUsIHVzZSB0aGF0LiBPdGhlcndpc2UsIHByb3ZpZGUgYSBuZXcgb25lLlxuICBwcm92aWRlOiBNYXRQYWdpbmF0b3JJbnRsLFxuICBkZXBzOiBbW25ldyBPcHRpb25hbCgpLCBuZXcgU2tpcFNlbGYoKSwgTWF0UGFnaW5hdG9ySW50bF1dLFxuICB1c2VGYWN0b3J5OiBNQVRfUEFHSU5BVE9SX0lOVExfUFJPVklERVJfRkFDVE9SWVxufTtcbiJdfQ==