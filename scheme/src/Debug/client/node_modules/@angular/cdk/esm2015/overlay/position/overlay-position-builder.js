/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Platform } from '@angular/cdk/platform';
import { ViewportRuler } from '@angular/cdk/scrolling';
import { DOCUMENT } from '@angular/common';
import { Inject, Injectable } from '@angular/core';
import { OverlayContainer } from '../overlay-container';
import { ConnectedPositionStrategy } from './connected-position-strategy';
import { FlexibleConnectedPositionStrategy, } from './flexible-connected-position-strategy';
import { GlobalPositionStrategy } from './global-position-strategy';
import * as i0 from "@angular/core";
import * as i1 from "@angular/cdk/scrolling";
import * as i2 from "@angular/common";
import * as i3 from "@angular/cdk/platform";
import * as i4 from "../overlay-container";
/** Builder for overlay position strategy. */
let OverlayPositionBuilder = /** @class */ (() => {
    class OverlayPositionBuilder {
        constructor(_viewportRuler, _document, _platform, _overlayContainer) {
            this._viewportRuler = _viewportRuler;
            this._document = _document;
            this._platform = _platform;
            this._overlayContainer = _overlayContainer;
        }
        /**
         * Creates a global position strategy.
         */
        global() {
            return new GlobalPositionStrategy();
        }
        /**
         * Creates a relative position strategy.
         * @param elementRef
         * @param originPos
         * @param overlayPos
         * @deprecated Use `flexibleConnectedTo` instead.
         * @breaking-change 8.0.0
         */
        connectedTo(elementRef, originPos, overlayPos) {
            return new ConnectedPositionStrategy(originPos, overlayPos, elementRef, this._viewportRuler, this._document, this._platform, this._overlayContainer);
        }
        /**
         * Creates a flexible position strategy.
         * @param origin Origin relative to which to position the overlay.
         */
        flexibleConnectedTo(origin) {
            return new FlexibleConnectedPositionStrategy(origin, this._viewportRuler, this._document, this._platform, this._overlayContainer);
        }
    }
    OverlayPositionBuilder.ɵprov = i0.ɵɵdefineInjectable({ factory: function OverlayPositionBuilder_Factory() { return new OverlayPositionBuilder(i0.ɵɵinject(i1.ViewportRuler), i0.ɵɵinject(i2.DOCUMENT), i0.ɵɵinject(i3.Platform), i0.ɵɵinject(i4.OverlayContainer)); }, token: OverlayPositionBuilder, providedIn: "root" });
    OverlayPositionBuilder.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    OverlayPositionBuilder.ctorParameters = () => [
        { type: ViewportRuler },
        { type: undefined, decorators: [{ type: Inject, args: [DOCUMENT,] }] },
        { type: Platform },
        { type: OverlayContainer }
    ];
    return OverlayPositionBuilder;
})();
export { OverlayPositionBuilder };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoib3ZlcmxheS1wb3NpdGlvbi1idWlsZGVyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9vdmVybGF5L3Bvc2l0aW9uL292ZXJsYXktcG9zaXRpb24tYnVpbGRlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsUUFBUSxFQUFDLE1BQU0sdUJBQXVCLENBQUM7QUFDL0MsT0FBTyxFQUFDLGFBQWEsRUFBQyxNQUFNLHdCQUF3QixDQUFDO0FBQ3JELE9BQU8sRUFBQyxRQUFRLEVBQUMsTUFBTSxpQkFBaUIsQ0FBQztBQUN6QyxPQUFPLEVBQWEsTUFBTSxFQUFFLFVBQVUsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUU3RCxPQUFPLEVBQUMsZ0JBQWdCLEVBQUMsTUFBTSxzQkFBc0IsQ0FBQztBQUd0RCxPQUFPLEVBQUMseUJBQXlCLEVBQUMsTUFBTSwrQkFBK0IsQ0FBQztBQUN4RSxPQUFPLEVBQ0wsaUNBQWlDLEdBRWxDLE1BQU0sd0NBQXdDLENBQUM7QUFDaEQsT0FBTyxFQUFDLHNCQUFzQixFQUFDLE1BQU0sNEJBQTRCLENBQUM7Ozs7OztBQUdsRSw2Q0FBNkM7QUFDN0M7SUFBQSxNQUNhLHNCQUFzQjtRQUNqQyxZQUNZLGNBQTZCLEVBQTRCLFNBQWMsRUFDdkUsU0FBbUIsRUFBVSxpQkFBbUM7WUFEaEUsbUJBQWMsR0FBZCxjQUFjLENBQWU7WUFBNEIsY0FBUyxHQUFULFNBQVMsQ0FBSztZQUN2RSxjQUFTLEdBQVQsU0FBUyxDQUFVO1lBQVUsc0JBQWlCLEdBQWpCLGlCQUFpQixDQUFrQjtRQUFHLENBQUM7UUFFaEY7O1dBRUc7UUFDSCxNQUFNO1lBQ0osT0FBTyxJQUFJLHNCQUFzQixFQUFFLENBQUM7UUFDdEMsQ0FBQztRQUVEOzs7Ozs7O1dBT0c7UUFDSCxXQUFXLENBQ1AsVUFBc0IsRUFDdEIsU0FBbUMsRUFDbkMsVUFBcUM7WUFDdkMsT0FBTyxJQUFJLHlCQUF5QixDQUNoQyxTQUFTLEVBQUUsVUFBVSxFQUFFLFVBQVUsRUFBRSxJQUFJLENBQUMsY0FBYyxFQUFFLElBQUksQ0FBQyxTQUFTLEVBQUUsSUFBSSxDQUFDLFNBQVMsRUFDdEYsSUFBSSxDQUFDLGlCQUFpQixDQUFDLENBQUM7UUFDOUIsQ0FBQztRQUVEOzs7V0FHRztRQUNILG1CQUFtQixDQUFDLE1BQStDO1lBRWpFLE9BQU8sSUFBSSxpQ0FBaUMsQ0FBQyxNQUFNLEVBQUUsSUFBSSxDQUFDLGNBQWMsRUFBRSxJQUFJLENBQUMsU0FBUyxFQUNwRixJQUFJLENBQUMsU0FBUyxFQUFFLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxDQUFDO1FBQzlDLENBQUM7Ozs7Z0JBdENGLFVBQVUsU0FBQyxFQUFDLFVBQVUsRUFBRSxNQUFNLEVBQUM7OztnQkFoQnhCLGFBQWE7Z0RBbUJ5QixNQUFNLFNBQUMsUUFBUTtnQkFwQnJELFFBQVE7Z0JBS1IsZ0JBQWdCOztpQ0FieEI7S0FpRUM7U0F2Q1ksc0JBQXNCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7UGxhdGZvcm19IGZyb20gJ0Bhbmd1bGFyL2Nkay9wbGF0Zm9ybSc7XG5pbXBvcnQge1ZpZXdwb3J0UnVsZXJ9IGZyb20gJ0Bhbmd1bGFyL2Nkay9zY3JvbGxpbmcnO1xuaW1wb3J0IHtET0NVTUVOVH0gZnJvbSAnQGFuZ3VsYXIvY29tbW9uJztcbmltcG9ydCB7RWxlbWVudFJlZiwgSW5qZWN0LCBJbmplY3RhYmxlfSBmcm9tICdAYW5ndWxhci9jb3JlJztcblxuaW1wb3J0IHtPdmVybGF5Q29udGFpbmVyfSBmcm9tICcuLi9vdmVybGF5LWNvbnRhaW5lcic7XG5cbmltcG9ydCB7T3JpZ2luQ29ubmVjdGlvblBvc2l0aW9uLCBPdmVybGF5Q29ubmVjdGlvblBvc2l0aW9ufSBmcm9tICcuL2Nvbm5lY3RlZC1wb3NpdGlvbic7XG5pbXBvcnQge0Nvbm5lY3RlZFBvc2l0aW9uU3RyYXRlZ3l9IGZyb20gJy4vY29ubmVjdGVkLXBvc2l0aW9uLXN0cmF0ZWd5JztcbmltcG9ydCB7XG4gIEZsZXhpYmxlQ29ubmVjdGVkUG9zaXRpb25TdHJhdGVneSxcbiAgRmxleGlibGVDb25uZWN0ZWRQb3NpdGlvblN0cmF0ZWd5T3JpZ2luLFxufSBmcm9tICcuL2ZsZXhpYmxlLWNvbm5lY3RlZC1wb3NpdGlvbi1zdHJhdGVneSc7XG5pbXBvcnQge0dsb2JhbFBvc2l0aW9uU3RyYXRlZ3l9IGZyb20gJy4vZ2xvYmFsLXBvc2l0aW9uLXN0cmF0ZWd5JztcblxuXG4vKiogQnVpbGRlciBmb3Igb3ZlcmxheSBwb3NpdGlvbiBzdHJhdGVneS4gKi9cbkBJbmplY3RhYmxlKHtwcm92aWRlZEluOiAncm9vdCd9KVxuZXhwb3J0IGNsYXNzIE92ZXJsYXlQb3NpdGlvbkJ1aWxkZXIge1xuICBjb25zdHJ1Y3RvcihcbiAgICAgIHByaXZhdGUgX3ZpZXdwb3J0UnVsZXI6IFZpZXdwb3J0UnVsZXIsIEBJbmplY3QoRE9DVU1FTlQpIHByaXZhdGUgX2RvY3VtZW50OiBhbnksXG4gICAgICBwcml2YXRlIF9wbGF0Zm9ybTogUGxhdGZvcm0sIHByaXZhdGUgX292ZXJsYXlDb250YWluZXI6IE92ZXJsYXlDb250YWluZXIpIHt9XG5cbiAgLyoqXG4gICAqIENyZWF0ZXMgYSBnbG9iYWwgcG9zaXRpb24gc3RyYXRlZ3kuXG4gICAqL1xuICBnbG9iYWwoKTogR2xvYmFsUG9zaXRpb25TdHJhdGVneSB7XG4gICAgcmV0dXJuIG5ldyBHbG9iYWxQb3NpdGlvblN0cmF0ZWd5KCk7XG4gIH1cblxuICAvKipcbiAgICogQ3JlYXRlcyBhIHJlbGF0aXZlIHBvc2l0aW9uIHN0cmF0ZWd5LlxuICAgKiBAcGFyYW0gZWxlbWVudFJlZlxuICAgKiBAcGFyYW0gb3JpZ2luUG9zXG4gICAqIEBwYXJhbSBvdmVybGF5UG9zXG4gICAqIEBkZXByZWNhdGVkIFVzZSBgZmxleGlibGVDb25uZWN0ZWRUb2AgaW5zdGVhZC5cbiAgICogQGJyZWFraW5nLWNoYW5nZSA4LjAuMFxuICAgKi9cbiAgY29ubmVjdGVkVG8oXG4gICAgICBlbGVtZW50UmVmOiBFbGVtZW50UmVmLFxuICAgICAgb3JpZ2luUG9zOiBPcmlnaW5Db25uZWN0aW9uUG9zaXRpb24sXG4gICAgICBvdmVybGF5UG9zOiBPdmVybGF5Q29ubmVjdGlvblBvc2l0aW9uKTogQ29ubmVjdGVkUG9zaXRpb25TdHJhdGVneSB7XG4gICAgcmV0dXJuIG5ldyBDb25uZWN0ZWRQb3NpdGlvblN0cmF0ZWd5KFxuICAgICAgICBvcmlnaW5Qb3MsIG92ZXJsYXlQb3MsIGVsZW1lbnRSZWYsIHRoaXMuX3ZpZXdwb3J0UnVsZXIsIHRoaXMuX2RvY3VtZW50LCB0aGlzLl9wbGF0Zm9ybSxcbiAgICAgICAgdGhpcy5fb3ZlcmxheUNvbnRhaW5lcik7XG4gIH1cblxuICAvKipcbiAgICogQ3JlYXRlcyBhIGZsZXhpYmxlIHBvc2l0aW9uIHN0cmF0ZWd5LlxuICAgKiBAcGFyYW0gb3JpZ2luIE9yaWdpbiByZWxhdGl2ZSB0byB3aGljaCB0byBwb3NpdGlvbiB0aGUgb3ZlcmxheS5cbiAgICovXG4gIGZsZXhpYmxlQ29ubmVjdGVkVG8ob3JpZ2luOiBGbGV4aWJsZUNvbm5lY3RlZFBvc2l0aW9uU3RyYXRlZ3lPcmlnaW4pOlxuICAgIEZsZXhpYmxlQ29ubmVjdGVkUG9zaXRpb25TdHJhdGVneSB7XG4gICAgcmV0dXJuIG5ldyBGbGV4aWJsZUNvbm5lY3RlZFBvc2l0aW9uU3RyYXRlZ3kob3JpZ2luLCB0aGlzLl92aWV3cG9ydFJ1bGVyLCB0aGlzLl9kb2N1bWVudCxcbiAgICAgICAgdGhpcy5fcGxhdGZvcm0sIHRoaXMuX292ZXJsYXlDb250YWluZXIpO1xuICB9XG5cbn1cbiJdfQ==