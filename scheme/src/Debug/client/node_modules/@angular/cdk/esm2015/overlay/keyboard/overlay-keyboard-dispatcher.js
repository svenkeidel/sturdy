/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { DOCUMENT } from '@angular/common';
import { Inject, Injectable, Optional, SkipSelf, } from '@angular/core';
import * as i0 from "@angular/core";
import * as i1 from "@angular/common";
/**
 * Service for dispatching keyboard events that land on the body to appropriate overlay ref,
 * if any. It maintains a list of attached overlays to determine best suited overlay based
 * on event target and order of overlay opens.
 */
let OverlayKeyboardDispatcher = /** @class */ (() => {
    class OverlayKeyboardDispatcher {
        constructor(document) {
            /** Currently attached overlays in the order they were attached. */
            this._attachedOverlays = [];
            /** Keyboard event listener that will be attached to the body. */
            this._keydownListener = (event) => {
                const overlays = this._attachedOverlays;
                for (let i = overlays.length - 1; i > -1; i--) {
                    // Dispatch the keydown event to the top overlay which has subscribers to its keydown events.
                    // We want to target the most recent overlay, rather than trying to match where the event came
                    // from, because some components might open an overlay, but keep focus on a trigger element
                    // (e.g. for select and autocomplete). We skip overlays without keydown event subscriptions,
                    // because we don't want overlays that don't handle keyboard events to block the ones below
                    // them that do.
                    if (overlays[i]._keydownEvents.observers.length > 0) {
                        overlays[i]._keydownEvents.next(event);
                        break;
                    }
                }
            };
            this._document = document;
        }
        ngOnDestroy() {
            this._detach();
        }
        /** Add a new overlay to the list of attached overlay refs. */
        add(overlayRef) {
            // Ensure that we don't get the same overlay multiple times.
            this.remove(overlayRef);
            // Lazily start dispatcher once first overlay is added
            if (!this._isAttached) {
                this._document.body.addEventListener('keydown', this._keydownListener);
                this._isAttached = true;
            }
            this._attachedOverlays.push(overlayRef);
        }
        /** Remove an overlay from the list of attached overlay refs. */
        remove(overlayRef) {
            const index = this._attachedOverlays.indexOf(overlayRef);
            if (index > -1) {
                this._attachedOverlays.splice(index, 1);
            }
            // Remove the global listener once there are no more overlays.
            if (this._attachedOverlays.length === 0) {
                this._detach();
            }
        }
        /** Detaches the global keyboard event listener. */
        _detach() {
            if (this._isAttached) {
                this._document.body.removeEventListener('keydown', this._keydownListener);
                this._isAttached = false;
            }
        }
    }
    OverlayKeyboardDispatcher.ɵprov = i0.ɵɵdefineInjectable({ factory: function OverlayKeyboardDispatcher_Factory() { return new OverlayKeyboardDispatcher(i0.ɵɵinject(i1.DOCUMENT)); }, token: OverlayKeyboardDispatcher, providedIn: "root" });
    OverlayKeyboardDispatcher.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    OverlayKeyboardDispatcher.ctorParameters = () => [
        { type: undefined, decorators: [{ type: Inject, args: [DOCUMENT,] }] }
    ];
    return OverlayKeyboardDispatcher;
})();
export { OverlayKeyboardDispatcher };
/** @docs-private @deprecated @breaking-change 8.0.0 */
export function OVERLAY_KEYBOARD_DISPATCHER_PROVIDER_FACTORY(dispatcher, _document) {
    return dispatcher || new OverlayKeyboardDispatcher(_document);
}
/** @docs-private @deprecated @breaking-change 8.0.0 */
export const OVERLAY_KEYBOARD_DISPATCHER_PROVIDER = {
    // If there is already an OverlayKeyboardDispatcher available, use that.
    // Otherwise, provide a new one.
    provide: OverlayKeyboardDispatcher,
    deps: [
        [new Optional(), new SkipSelf(), OverlayKeyboardDispatcher],
        // Coerce to `InjectionToken` so that the `deps` match the "shape"
        // of the type expected by Angular
        DOCUMENT
    ],
    useFactory: OVERLAY_KEYBOARD_DISPATCHER_PROVIDER_FACTORY
};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoib3ZlcmxheS1rZXlib2FyZC1kaXNwYXRjaGVyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9vdmVybGF5L2tleWJvYXJkL292ZXJsYXkta2V5Ym9hcmQtZGlzcGF0Y2hlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsUUFBUSxFQUFDLE1BQU0saUJBQWlCLENBQUM7QUFDekMsT0FBTyxFQUNMLE1BQU0sRUFDTixVQUFVLEVBR1YsUUFBUSxFQUNSLFFBQVEsR0FDVCxNQUFNLGVBQWUsQ0FBQzs7O0FBSXZCOzs7O0dBSUc7QUFDSDtJQUFBLE1BQ2EseUJBQXlCO1FBUXBDLFlBQThCLFFBQWE7WUFOM0MsbUVBQW1FO1lBQ25FLHNCQUFpQixHQUF1QixFQUFFLENBQUM7WUFpRDNDLGlFQUFpRTtZQUN6RCxxQkFBZ0IsR0FBRyxDQUFDLEtBQW9CLEVBQUUsRUFBRTtnQkFDbEQsTUFBTSxRQUFRLEdBQUcsSUFBSSxDQUFDLGlCQUFpQixDQUFDO2dCQUV4QyxLQUFLLElBQUksQ0FBQyxHQUFHLFFBQVEsQ0FBQyxNQUFNLEdBQUcsQ0FBQyxFQUFFLENBQUMsR0FBRyxDQUFDLENBQUMsRUFBRSxDQUFDLEVBQUUsRUFBRTtvQkFDN0MsNkZBQTZGO29CQUM3Riw4RkFBOEY7b0JBQzlGLDJGQUEyRjtvQkFDM0YsNEZBQTRGO29CQUM1RiwyRkFBMkY7b0JBQzNGLGdCQUFnQjtvQkFDaEIsSUFBSSxRQUFRLENBQUMsQ0FBQyxDQUFDLENBQUMsY0FBYyxDQUFDLFNBQVMsQ0FBQyxNQUFNLEdBQUcsQ0FBQyxFQUFFO3dCQUNuRCxRQUFRLENBQUMsQ0FBQyxDQUFDLENBQUMsY0FBYyxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsQ0FBQzt3QkFDdkMsTUFBTTtxQkFDUDtpQkFDRjtZQUNILENBQUMsQ0FBQTtZQTNEQyxJQUFJLENBQUMsU0FBUyxHQUFHLFFBQVEsQ0FBQztRQUM1QixDQUFDO1FBRUQsV0FBVztZQUNULElBQUksQ0FBQyxPQUFPLEVBQUUsQ0FBQztRQUNqQixDQUFDO1FBRUQsOERBQThEO1FBQzlELEdBQUcsQ0FBQyxVQUE0QjtZQUM5Qiw0REFBNEQ7WUFDNUQsSUFBSSxDQUFDLE1BQU0sQ0FBQyxVQUFVLENBQUMsQ0FBQztZQUV4QixzREFBc0Q7WUFDdEQsSUFBSSxDQUFDLElBQUksQ0FBQyxXQUFXLEVBQUU7Z0JBQ3JCLElBQUksQ0FBQyxTQUFTLENBQUMsSUFBSSxDQUFDLGdCQUFnQixDQUFDLFNBQVMsRUFBRSxJQUFJLENBQUMsZ0JBQWdCLENBQUMsQ0FBQztnQkFDdkUsSUFBSSxDQUFDLFdBQVcsR0FBRyxJQUFJLENBQUM7YUFDekI7WUFFRCxJQUFJLENBQUMsaUJBQWlCLENBQUMsSUFBSSxDQUFDLFVBQVUsQ0FBQyxDQUFDO1FBQzFDLENBQUM7UUFFRCxnRUFBZ0U7UUFDaEUsTUFBTSxDQUFDLFVBQTRCO1lBQ2pDLE1BQU0sS0FBSyxHQUFHLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxPQUFPLENBQUMsVUFBVSxDQUFDLENBQUM7WUFFekQsSUFBSSxLQUFLLEdBQUcsQ0FBQyxDQUFDLEVBQUU7Z0JBQ2QsSUFBSSxDQUFDLGlCQUFpQixDQUFDLE1BQU0sQ0FBQyxLQUFLLEVBQUUsQ0FBQyxDQUFDLENBQUM7YUFDekM7WUFFRCw4REFBOEQ7WUFDOUQsSUFBSSxJQUFJLENBQUMsaUJBQWlCLENBQUMsTUFBTSxLQUFLLENBQUMsRUFBRTtnQkFDdkMsSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDO2FBQ2hCO1FBQ0gsQ0FBQztRQUVELG1EQUFtRDtRQUMzQyxPQUFPO1lBQ2IsSUFBSSxJQUFJLENBQUMsV0FBVyxFQUFFO2dCQUNwQixJQUFJLENBQUMsU0FBUyxDQUFDLElBQUksQ0FBQyxtQkFBbUIsQ0FBQyxTQUFTLEVBQUUsSUFBSSxDQUFDLGdCQUFnQixDQUFDLENBQUM7Z0JBQzFFLElBQUksQ0FBQyxXQUFXLEdBQUcsS0FBSyxDQUFDO2FBQzFCO1FBQ0gsQ0FBQzs7OztnQkFuREYsVUFBVSxTQUFDLEVBQUMsVUFBVSxFQUFFLE1BQU0sRUFBQzs7O2dEQVNqQixNQUFNLFNBQUMsUUFBUTs7b0NBbEM5QjtLQStGQztTQXJFWSx5QkFBeUI7QUF3RXRDLHVEQUF1RDtBQUN2RCxNQUFNLFVBQVUsNENBQTRDLENBQ3hELFVBQXFDLEVBQUUsU0FBYztJQUN2RCxPQUFPLFVBQVUsSUFBSSxJQUFJLHlCQUF5QixDQUFDLFNBQVMsQ0FBQyxDQUFDO0FBQ2hFLENBQUM7QUFFRCx1REFBdUQ7QUFDdkQsTUFBTSxDQUFDLE1BQU0sb0NBQW9DLEdBQUc7SUFDbEQsd0VBQXdFO0lBQ3hFLGdDQUFnQztJQUNoQyxPQUFPLEVBQUUseUJBQXlCO0lBQ2xDLElBQUksRUFBRTtRQUNKLENBQUMsSUFBSSxRQUFRLEVBQUUsRUFBRSxJQUFJLFFBQVEsRUFBRSxFQUFFLHlCQUF5QixDQUFDO1FBRTNELGtFQUFrRTtRQUNsRSxrQ0FBa0M7UUFDbEMsUUFBK0I7S0FDaEM7SUFDRCxVQUFVLEVBQUUsNENBQTRDO0NBQ3pELENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtET0NVTUVOVH0gZnJvbSAnQGFuZ3VsYXIvY29tbW9uJztcbmltcG9ydCB7XG4gIEluamVjdCxcbiAgSW5qZWN0YWJsZSxcbiAgSW5qZWN0aW9uVG9rZW4sXG4gIE9uRGVzdHJveSxcbiAgT3B0aW9uYWwsXG4gIFNraXBTZWxmLFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7T3ZlcmxheVJlZmVyZW5jZX0gZnJvbSAnLi4vb3ZlcmxheS1yZWZlcmVuY2UnO1xuXG5cbi8qKlxuICogU2VydmljZSBmb3IgZGlzcGF0Y2hpbmcga2V5Ym9hcmQgZXZlbnRzIHRoYXQgbGFuZCBvbiB0aGUgYm9keSB0byBhcHByb3ByaWF0ZSBvdmVybGF5IHJlZixcbiAqIGlmIGFueS4gSXQgbWFpbnRhaW5zIGEgbGlzdCBvZiBhdHRhY2hlZCBvdmVybGF5cyB0byBkZXRlcm1pbmUgYmVzdCBzdWl0ZWQgb3ZlcmxheSBiYXNlZFxuICogb24gZXZlbnQgdGFyZ2V0IGFuZCBvcmRlciBvZiBvdmVybGF5IG9wZW5zLlxuICovXG5ASW5qZWN0YWJsZSh7cHJvdmlkZWRJbjogJ3Jvb3QnfSlcbmV4cG9ydCBjbGFzcyBPdmVybGF5S2V5Ym9hcmREaXNwYXRjaGVyIGltcGxlbWVudHMgT25EZXN0cm95IHtcblxuICAvKiogQ3VycmVudGx5IGF0dGFjaGVkIG92ZXJsYXlzIGluIHRoZSBvcmRlciB0aGV5IHdlcmUgYXR0YWNoZWQuICovXG4gIF9hdHRhY2hlZE92ZXJsYXlzOiBPdmVybGF5UmVmZXJlbmNlW10gPSBbXTtcblxuICBwcml2YXRlIF9kb2N1bWVudDogRG9jdW1lbnQ7XG4gIHByaXZhdGUgX2lzQXR0YWNoZWQ6IGJvb2xlYW47XG5cbiAgY29uc3RydWN0b3IoQEluamVjdChET0NVTUVOVCkgZG9jdW1lbnQ6IGFueSkge1xuICAgIHRoaXMuX2RvY3VtZW50ID0gZG9jdW1lbnQ7XG4gIH1cblxuICBuZ09uRGVzdHJveSgpIHtcbiAgICB0aGlzLl9kZXRhY2goKTtcbiAgfVxuXG4gIC8qKiBBZGQgYSBuZXcgb3ZlcmxheSB0byB0aGUgbGlzdCBvZiBhdHRhY2hlZCBvdmVybGF5IHJlZnMuICovXG4gIGFkZChvdmVybGF5UmVmOiBPdmVybGF5UmVmZXJlbmNlKTogdm9pZCB7XG4gICAgLy8gRW5zdXJlIHRoYXQgd2UgZG9uJ3QgZ2V0IHRoZSBzYW1lIG92ZXJsYXkgbXVsdGlwbGUgdGltZXMuXG4gICAgdGhpcy5yZW1vdmUob3ZlcmxheVJlZik7XG5cbiAgICAvLyBMYXppbHkgc3RhcnQgZGlzcGF0Y2hlciBvbmNlIGZpcnN0IG92ZXJsYXkgaXMgYWRkZWRcbiAgICBpZiAoIXRoaXMuX2lzQXR0YWNoZWQpIHtcbiAgICAgIHRoaXMuX2RvY3VtZW50LmJvZHkuYWRkRXZlbnRMaXN0ZW5lcigna2V5ZG93bicsIHRoaXMuX2tleWRvd25MaXN0ZW5lcik7XG4gICAgICB0aGlzLl9pc0F0dGFjaGVkID0gdHJ1ZTtcbiAgICB9XG5cbiAgICB0aGlzLl9hdHRhY2hlZE92ZXJsYXlzLnB1c2gob3ZlcmxheVJlZik7XG4gIH1cblxuICAvKiogUmVtb3ZlIGFuIG92ZXJsYXkgZnJvbSB0aGUgbGlzdCBvZiBhdHRhY2hlZCBvdmVybGF5IHJlZnMuICovXG4gIHJlbW92ZShvdmVybGF5UmVmOiBPdmVybGF5UmVmZXJlbmNlKTogdm9pZCB7XG4gICAgY29uc3QgaW5kZXggPSB0aGlzLl9hdHRhY2hlZE92ZXJsYXlzLmluZGV4T2Yob3ZlcmxheVJlZik7XG5cbiAgICBpZiAoaW5kZXggPiAtMSkge1xuICAgICAgdGhpcy5fYXR0YWNoZWRPdmVybGF5cy5zcGxpY2UoaW5kZXgsIDEpO1xuICAgIH1cblxuICAgIC8vIFJlbW92ZSB0aGUgZ2xvYmFsIGxpc3RlbmVyIG9uY2UgdGhlcmUgYXJlIG5vIG1vcmUgb3ZlcmxheXMuXG4gICAgaWYgKHRoaXMuX2F0dGFjaGVkT3ZlcmxheXMubGVuZ3RoID09PSAwKSB7XG4gICAgICB0aGlzLl9kZXRhY2goKTtcbiAgICB9XG4gIH1cblxuICAvKiogRGV0YWNoZXMgdGhlIGdsb2JhbCBrZXlib2FyZCBldmVudCBsaXN0ZW5lci4gKi9cbiAgcHJpdmF0ZSBfZGV0YWNoKCkge1xuICAgIGlmICh0aGlzLl9pc0F0dGFjaGVkKSB7XG4gICAgICB0aGlzLl9kb2N1bWVudC5ib2R5LnJlbW92ZUV2ZW50TGlzdGVuZXIoJ2tleWRvd24nLCB0aGlzLl9rZXlkb3duTGlzdGVuZXIpO1xuICAgICAgdGhpcy5faXNBdHRhY2hlZCA9IGZhbHNlO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBLZXlib2FyZCBldmVudCBsaXN0ZW5lciB0aGF0IHdpbGwgYmUgYXR0YWNoZWQgdG8gdGhlIGJvZHkuICovXG4gIHByaXZhdGUgX2tleWRvd25MaXN0ZW5lciA9IChldmVudDogS2V5Ym9hcmRFdmVudCkgPT4ge1xuICAgIGNvbnN0IG92ZXJsYXlzID0gdGhpcy5fYXR0YWNoZWRPdmVybGF5cztcblxuICAgIGZvciAobGV0IGkgPSBvdmVybGF5cy5sZW5ndGggLSAxOyBpID4gLTE7IGktLSkge1xuICAgICAgLy8gRGlzcGF0Y2ggdGhlIGtleWRvd24gZXZlbnQgdG8gdGhlIHRvcCBvdmVybGF5IHdoaWNoIGhhcyBzdWJzY3JpYmVycyB0byBpdHMga2V5ZG93biBldmVudHMuXG4gICAgICAvLyBXZSB3YW50IHRvIHRhcmdldCB0aGUgbW9zdCByZWNlbnQgb3ZlcmxheSwgcmF0aGVyIHRoYW4gdHJ5aW5nIHRvIG1hdGNoIHdoZXJlIHRoZSBldmVudCBjYW1lXG4gICAgICAvLyBmcm9tLCBiZWNhdXNlIHNvbWUgY29tcG9uZW50cyBtaWdodCBvcGVuIGFuIG92ZXJsYXksIGJ1dCBrZWVwIGZvY3VzIG9uIGEgdHJpZ2dlciBlbGVtZW50XG4gICAgICAvLyAoZS5nLiBmb3Igc2VsZWN0IGFuZCBhdXRvY29tcGxldGUpLiBXZSBza2lwIG92ZXJsYXlzIHdpdGhvdXQga2V5ZG93biBldmVudCBzdWJzY3JpcHRpb25zLFxuICAgICAgLy8gYmVjYXVzZSB3ZSBkb24ndCB3YW50IG92ZXJsYXlzIHRoYXQgZG9uJ3QgaGFuZGxlIGtleWJvYXJkIGV2ZW50cyB0byBibG9jayB0aGUgb25lcyBiZWxvd1xuICAgICAgLy8gdGhlbSB0aGF0IGRvLlxuICAgICAgaWYgKG92ZXJsYXlzW2ldLl9rZXlkb3duRXZlbnRzLm9ic2VydmVycy5sZW5ndGggPiAwKSB7XG4gICAgICAgIG92ZXJsYXlzW2ldLl9rZXlkb3duRXZlbnRzLm5leHQoZXZlbnQpO1xuICAgICAgICBicmVhaztcbiAgICAgIH1cbiAgICB9XG4gIH1cbn1cblxuXG4vKiogQGRvY3MtcHJpdmF0ZSBAZGVwcmVjYXRlZCBAYnJlYWtpbmctY2hhbmdlIDguMC4wICovXG5leHBvcnQgZnVuY3Rpb24gT1ZFUkxBWV9LRVlCT0FSRF9ESVNQQVRDSEVSX1BST1ZJREVSX0ZBQ1RPUlkoXG4gICAgZGlzcGF0Y2hlcjogT3ZlcmxheUtleWJvYXJkRGlzcGF0Y2hlciwgX2RvY3VtZW50OiBhbnkpIHtcbiAgcmV0dXJuIGRpc3BhdGNoZXIgfHwgbmV3IE92ZXJsYXlLZXlib2FyZERpc3BhdGNoZXIoX2RvY3VtZW50KTtcbn1cblxuLyoqIEBkb2NzLXByaXZhdGUgQGRlcHJlY2F0ZWQgQGJyZWFraW5nLWNoYW5nZSA4LjAuMCAqL1xuZXhwb3J0IGNvbnN0IE9WRVJMQVlfS0VZQk9BUkRfRElTUEFUQ0hFUl9QUk9WSURFUiA9IHtcbiAgLy8gSWYgdGhlcmUgaXMgYWxyZWFkeSBhbiBPdmVybGF5S2V5Ym9hcmREaXNwYXRjaGVyIGF2YWlsYWJsZSwgdXNlIHRoYXQuXG4gIC8vIE90aGVyd2lzZSwgcHJvdmlkZSBhIG5ldyBvbmUuXG4gIHByb3ZpZGU6IE92ZXJsYXlLZXlib2FyZERpc3BhdGNoZXIsXG4gIGRlcHM6IFtcbiAgICBbbmV3IE9wdGlvbmFsKCksIG5ldyBTa2lwU2VsZigpLCBPdmVybGF5S2V5Ym9hcmREaXNwYXRjaGVyXSxcblxuICAgIC8vIENvZXJjZSB0byBgSW5qZWN0aW9uVG9rZW5gIHNvIHRoYXQgdGhlIGBkZXBzYCBtYXRjaCB0aGUgXCJzaGFwZVwiXG4gICAgLy8gb2YgdGhlIHR5cGUgZXhwZWN0ZWQgYnkgQW5ndWxhclxuICAgIERPQ1VNRU5UIGFzIEluamVjdGlvblRva2VuPGFueT5cbiAgXSxcbiAgdXNlRmFjdG9yeTogT1ZFUkxBWV9LRVlCT0FSRF9ESVNQQVRDSEVSX1BST1ZJREVSX0ZBQ1RPUllcbn07XG4iXX0=