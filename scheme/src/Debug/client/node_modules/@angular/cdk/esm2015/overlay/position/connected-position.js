/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** Horizontal dimension of a connection point on the perimeter of the origin or overlay element. */
import { Optional } from '@angular/core';
/** The points of the origin element and the overlay element to connect. */
export class ConnectionPositionPair {
    constructor(origin, overlay, 
    /** Offset along the X axis. */
    offsetX, 
    /** Offset along the Y axis. */
    offsetY, 
    /** Class(es) to be applied to the panel while this position is active. */
    panelClass) {
        this.offsetX = offsetX;
        this.offsetY = offsetY;
        this.panelClass = panelClass;
        this.originX = origin.originX;
        this.originY = origin.originY;
        this.overlayX = overlay.overlayX;
        this.overlayY = overlay.overlayY;
    }
}
/**
 * Set of properties regarding the position of the origin and overlay relative to the viewport
 * with respect to the containing Scrollable elements.
 *
 * The overlay and origin are clipped if any part of their bounding client rectangle exceeds the
 * bounds of any one of the strategy's Scrollable's bounding client rectangle.
 *
 * The overlay and origin are outside view if there is no overlap between their bounding client
 * rectangle and any one of the strategy's Scrollable's bounding client rectangle.
 *
 *       -----------                    -----------
 *       | outside |                    | clipped |
 *       |  view   |              --------------------------
 *       |         |              |     |         |        |
 *       ----------               |     -----------        |
 *  --------------------------    |                        |
 *  |                        |    |      Scrollable        |
 *  |                        |    |                        |
 *  |                        |     --------------------------
 *  |      Scrollable        |
 *  |                        |
 *  --------------------------
 *
 *  @docs-private
 */
export class ScrollingVisibility {
}
/** The change event emitted by the strategy when a fallback position is used. */
let ConnectedOverlayPositionChange = /** @class */ (() => {
    class ConnectedOverlayPositionChange {
        constructor(
        /** The position used as a result of this change. */
        connectionPair, 
        /** @docs-private */
        scrollableViewProperties) {
            this.connectionPair = connectionPair;
            this.scrollableViewProperties = scrollableViewProperties;
        }
    }
    ConnectedOverlayPositionChange.ctorParameters = () => [
        { type: ConnectionPositionPair },
        { type: ScrollingVisibility, decorators: [{ type: Optional }] }
    ];
    return ConnectedOverlayPositionChange;
})();
export { ConnectedOverlayPositionChange };
/**
 * Validates whether a vertical position property matches the expected values.
 * @param property Name of the property being validated.
 * @param value Value of the property being validated.
 * @docs-private
 */
export function validateVerticalPosition(property, value) {
    if (value !== 'top' && value !== 'bottom' && value !== 'center') {
        throw Error(`ConnectedPosition: Invalid ${property} "${value}". ` +
            `Expected "top", "bottom" or "center".`);
    }
}
/**
 * Validates whether a horizontal position property matches the expected values.
 * @param property Name of the property being validated.
 * @param value Value of the property being validated.
 * @docs-private
 */
export function validateHorizontalPosition(property, value) {
    if (value !== 'start' && value !== 'end' && value !== 'center') {
        throw Error(`ConnectedPosition: Invalid ${property} "${value}". ` +
            `Expected "start", "end" or "center".`);
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY29ubmVjdGVkLXBvc2l0aW9uLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9vdmVybGF5L3Bvc2l0aW9uL2Nvbm5lY3RlZC1wb3NpdGlvbi50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxvR0FBb0c7QUFDcEcsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQW1CdkMsMkVBQTJFO0FBQzNFLE1BQU0sT0FBTyxzQkFBc0I7SUFVakMsWUFDRSxNQUFnQyxFQUNoQyxPQUFrQztJQUNsQywrQkFBK0I7SUFDeEIsT0FBZ0I7SUFDdkIsK0JBQStCO0lBQ3hCLE9BQWdCO0lBQ3ZCLDBFQUEwRTtJQUNuRSxVQUE4QjtRQUo5QixZQUFPLEdBQVAsT0FBTyxDQUFTO1FBRWhCLFlBQU8sR0FBUCxPQUFPLENBQVM7UUFFaEIsZUFBVSxHQUFWLFVBQVUsQ0FBb0I7UUFFckMsSUFBSSxDQUFDLE9BQU8sR0FBRyxNQUFNLENBQUMsT0FBTyxDQUFDO1FBQzlCLElBQUksQ0FBQyxPQUFPLEdBQUcsTUFBTSxDQUFDLE9BQU8sQ0FBQztRQUM5QixJQUFJLENBQUMsUUFBUSxHQUFHLE9BQU8sQ0FBQyxRQUFRLENBQUM7UUFDakMsSUFBSSxDQUFDLFFBQVEsR0FBRyxPQUFPLENBQUMsUUFBUSxDQUFDO0lBQ25DLENBQUM7Q0FDRjtBQUVEOzs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7R0F3Qkc7QUFDSCxNQUFNLE9BQU8sbUJBQW1CO0NBSy9CO0FBRUQsaUZBQWlGO0FBQ2pGO0lBQUEsTUFBYSw4QkFBOEI7UUFDekM7UUFDSSxvREFBb0Q7UUFDN0MsY0FBc0M7UUFDN0Msb0JBQW9CO1FBQ0Qsd0JBQTZDO1lBRnpELG1CQUFjLEdBQWQsY0FBYyxDQUF3QjtZQUUxQiw2QkFBd0IsR0FBeEIsd0JBQXdCLENBQXFCO1FBQUcsQ0FBQzs7O2dCQUY3QyxzQkFBc0I7Z0JBRUEsbUJBQW1CLHVCQUEvRCxRQUFROztJQUNmLHFDQUFDO0tBQUE7U0FOWSw4QkFBOEI7QUFRM0M7Ozs7O0dBS0c7QUFDSCxNQUFNLFVBQVUsd0JBQXdCLENBQUMsUUFBZ0IsRUFBRSxLQUE0QjtJQUNyRixJQUFJLEtBQUssS0FBSyxLQUFLLElBQUksS0FBSyxLQUFLLFFBQVEsSUFBSSxLQUFLLEtBQUssUUFBUSxFQUFFO1FBQy9ELE1BQU0sS0FBSyxDQUFDLDhCQUE4QixRQUFRLEtBQUssS0FBSyxLQUFLO1lBQ3JELHVDQUF1QyxDQUFDLENBQUM7S0FDdEQ7QUFDSCxDQUFDO0FBRUQ7Ozs7O0dBS0c7QUFDSCxNQUFNLFVBQVUsMEJBQTBCLENBQUMsUUFBZ0IsRUFBRSxLQUE4QjtJQUN6RixJQUFJLEtBQUssS0FBSyxPQUFPLElBQUksS0FBSyxLQUFLLEtBQUssSUFBSSxLQUFLLEtBQUssUUFBUSxFQUFFO1FBQzlELE1BQU0sS0FBSyxDQUFDLDhCQUE4QixRQUFRLEtBQUssS0FBSyxLQUFLO1lBQ3JELHNDQUFzQyxDQUFDLENBQUM7S0FDckQ7QUFDSCxDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbi8qKiBIb3Jpem9udGFsIGRpbWVuc2lvbiBvZiBhIGNvbm5lY3Rpb24gcG9pbnQgb24gdGhlIHBlcmltZXRlciBvZiB0aGUgb3JpZ2luIG9yIG92ZXJsYXkgZWxlbWVudC4gKi9cbmltcG9ydCB7T3B0aW9uYWx9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuZXhwb3J0IHR5cGUgSG9yaXpvbnRhbENvbm5lY3Rpb25Qb3MgPSAnc3RhcnQnIHwgJ2NlbnRlcicgfCAnZW5kJztcblxuLyoqIFZlcnRpY2FsIGRpbWVuc2lvbiBvZiBhIGNvbm5lY3Rpb24gcG9pbnQgb24gdGhlIHBlcmltZXRlciBvZiB0aGUgb3JpZ2luIG9yIG92ZXJsYXkgZWxlbWVudC4gKi9cbmV4cG9ydCB0eXBlIFZlcnRpY2FsQ29ubmVjdGlvblBvcyA9ICd0b3AnIHwgJ2NlbnRlcicgfCAnYm90dG9tJztcblxuXG4vKiogQSBjb25uZWN0aW9uIHBvaW50IG9uIHRoZSBvcmlnaW4gZWxlbWVudC4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgT3JpZ2luQ29ubmVjdGlvblBvc2l0aW9uIHtcbiAgb3JpZ2luWDogSG9yaXpvbnRhbENvbm5lY3Rpb25Qb3M7XG4gIG9yaWdpblk6IFZlcnRpY2FsQ29ubmVjdGlvblBvcztcbn1cblxuLyoqIEEgY29ubmVjdGlvbiBwb2ludCBvbiB0aGUgb3ZlcmxheSBlbGVtZW50LiAqL1xuZXhwb3J0IGludGVyZmFjZSBPdmVybGF5Q29ubmVjdGlvblBvc2l0aW9uIHtcbiAgb3ZlcmxheVg6IEhvcml6b250YWxDb25uZWN0aW9uUG9zO1xuICBvdmVybGF5WTogVmVydGljYWxDb25uZWN0aW9uUG9zO1xufVxuXG4vKiogVGhlIHBvaW50cyBvZiB0aGUgb3JpZ2luIGVsZW1lbnQgYW5kIHRoZSBvdmVybGF5IGVsZW1lbnQgdG8gY29ubmVjdC4gKi9cbmV4cG9ydCBjbGFzcyBDb25uZWN0aW9uUG9zaXRpb25QYWlyIHtcbiAgLyoqIFgtYXhpcyBhdHRhY2htZW50IHBvaW50IGZvciBjb25uZWN0ZWQgb3ZlcmxheSBvcmlnaW4uIENhbiBiZSAnc3RhcnQnLCAnZW5kJywgb3IgJ2NlbnRlcicuICovXG4gIG9yaWdpblg6IEhvcml6b250YWxDb25uZWN0aW9uUG9zO1xuICAvKiogWS1heGlzIGF0dGFjaG1lbnQgcG9pbnQgZm9yIGNvbm5lY3RlZCBvdmVybGF5IG9yaWdpbi4gQ2FuIGJlICd0b3AnLCAnYm90dG9tJywgb3IgJ2NlbnRlcicuICovXG4gIG9yaWdpblk6IFZlcnRpY2FsQ29ubmVjdGlvblBvcztcbiAgLyoqIFgtYXhpcyBhdHRhY2htZW50IHBvaW50IGZvciBjb25uZWN0ZWQgb3ZlcmxheS4gQ2FuIGJlICdzdGFydCcsICdlbmQnLCBvciAnY2VudGVyJy4gKi9cbiAgb3ZlcmxheVg6IEhvcml6b250YWxDb25uZWN0aW9uUG9zO1xuICAvKiogWS1heGlzIGF0dGFjaG1lbnQgcG9pbnQgZm9yIGNvbm5lY3RlZCBvdmVybGF5LiBDYW4gYmUgJ3RvcCcsICdib3R0b20nLCBvciAnY2VudGVyJy4gKi9cbiAgb3ZlcmxheVk6IFZlcnRpY2FsQ29ubmVjdGlvblBvcztcblxuICBjb25zdHJ1Y3RvcihcbiAgICBvcmlnaW46IE9yaWdpbkNvbm5lY3Rpb25Qb3NpdGlvbixcbiAgICBvdmVybGF5OiBPdmVybGF5Q29ubmVjdGlvblBvc2l0aW9uLFxuICAgIC8qKiBPZmZzZXQgYWxvbmcgdGhlIFggYXhpcy4gKi9cbiAgICBwdWJsaWMgb2Zmc2V0WD86IG51bWJlcixcbiAgICAvKiogT2Zmc2V0IGFsb25nIHRoZSBZIGF4aXMuICovXG4gICAgcHVibGljIG9mZnNldFk/OiBudW1iZXIsXG4gICAgLyoqIENsYXNzKGVzKSB0byBiZSBhcHBsaWVkIHRvIHRoZSBwYW5lbCB3aGlsZSB0aGlzIHBvc2l0aW9uIGlzIGFjdGl2ZS4gKi9cbiAgICBwdWJsaWMgcGFuZWxDbGFzcz86IHN0cmluZyB8IHN0cmluZ1tdKSB7XG5cbiAgICB0aGlzLm9yaWdpblggPSBvcmlnaW4ub3JpZ2luWDtcbiAgICB0aGlzLm9yaWdpblkgPSBvcmlnaW4ub3JpZ2luWTtcbiAgICB0aGlzLm92ZXJsYXlYID0gb3ZlcmxheS5vdmVybGF5WDtcbiAgICB0aGlzLm92ZXJsYXlZID0gb3ZlcmxheS5vdmVybGF5WTtcbiAgfVxufVxuXG4vKipcbiAqIFNldCBvZiBwcm9wZXJ0aWVzIHJlZ2FyZGluZyB0aGUgcG9zaXRpb24gb2YgdGhlIG9yaWdpbiBhbmQgb3ZlcmxheSByZWxhdGl2ZSB0byB0aGUgdmlld3BvcnRcbiAqIHdpdGggcmVzcGVjdCB0byB0aGUgY29udGFpbmluZyBTY3JvbGxhYmxlIGVsZW1lbnRzLlxuICpcbiAqIFRoZSBvdmVybGF5IGFuZCBvcmlnaW4gYXJlIGNsaXBwZWQgaWYgYW55IHBhcnQgb2YgdGhlaXIgYm91bmRpbmcgY2xpZW50IHJlY3RhbmdsZSBleGNlZWRzIHRoZVxuICogYm91bmRzIG9mIGFueSBvbmUgb2YgdGhlIHN0cmF0ZWd5J3MgU2Nyb2xsYWJsZSdzIGJvdW5kaW5nIGNsaWVudCByZWN0YW5nbGUuXG4gKlxuICogVGhlIG92ZXJsYXkgYW5kIG9yaWdpbiBhcmUgb3V0c2lkZSB2aWV3IGlmIHRoZXJlIGlzIG5vIG92ZXJsYXAgYmV0d2VlbiB0aGVpciBib3VuZGluZyBjbGllbnRcbiAqIHJlY3RhbmdsZSBhbmQgYW55IG9uZSBvZiB0aGUgc3RyYXRlZ3kncyBTY3JvbGxhYmxlJ3MgYm91bmRpbmcgY2xpZW50IHJlY3RhbmdsZS5cbiAqXG4gKiAgICAgICAtLS0tLS0tLS0tLSAgICAgICAgICAgICAgICAgICAgLS0tLS0tLS0tLS1cbiAqICAgICAgIHwgb3V0c2lkZSB8ICAgICAgICAgICAgICAgICAgICB8IGNsaXBwZWQgfFxuICogICAgICAgfCAgdmlldyAgIHwgICAgICAgICAgICAgIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXG4gKiAgICAgICB8ICAgICAgICAgfCAgICAgICAgICAgICAgfCAgICAgfCAgICAgICAgIHwgICAgICAgIHxcbiAqICAgICAgIC0tLS0tLS0tLS0gICAgICAgICAgICAgICB8ICAgICAtLS0tLS0tLS0tLSAgICAgICAgfFxuICogIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tICAgIHwgICAgICAgICAgICAgICAgICAgICAgICB8XG4gKiAgfCAgICAgICAgICAgICAgICAgICAgICAgIHwgICAgfCAgICAgIFNjcm9sbGFibGUgICAgICAgIHxcbiAqICB8ICAgICAgICAgICAgICAgICAgICAgICAgfCAgICB8ICAgICAgICAgICAgICAgICAgICAgICAgfFxuICogIHwgICAgICAgICAgICAgICAgICAgICAgICB8ICAgICAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuICogIHwgICAgICBTY3JvbGxhYmxlICAgICAgICB8XG4gKiAgfCAgICAgICAgICAgICAgICAgICAgICAgIHxcbiAqICAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuICpcbiAqICBAZG9jcy1wcml2YXRlXG4gKi9cbmV4cG9ydCBjbGFzcyBTY3JvbGxpbmdWaXNpYmlsaXR5IHtcbiAgaXNPcmlnaW5DbGlwcGVkOiBib29sZWFuO1xuICBpc09yaWdpbk91dHNpZGVWaWV3OiBib29sZWFuO1xuICBpc092ZXJsYXlDbGlwcGVkOiBib29sZWFuO1xuICBpc092ZXJsYXlPdXRzaWRlVmlldzogYm9vbGVhbjtcbn1cblxuLyoqIFRoZSBjaGFuZ2UgZXZlbnQgZW1pdHRlZCBieSB0aGUgc3RyYXRlZ3kgd2hlbiBhIGZhbGxiYWNrIHBvc2l0aW9uIGlzIHVzZWQuICovXG5leHBvcnQgY2xhc3MgQ29ubmVjdGVkT3ZlcmxheVBvc2l0aW9uQ2hhbmdlIHtcbiAgY29uc3RydWN0b3IoXG4gICAgICAvKiogVGhlIHBvc2l0aW9uIHVzZWQgYXMgYSByZXN1bHQgb2YgdGhpcyBjaGFuZ2UuICovXG4gICAgICBwdWJsaWMgY29ubmVjdGlvblBhaXI6IENvbm5lY3Rpb25Qb3NpdGlvblBhaXIsXG4gICAgICAvKiogQGRvY3MtcHJpdmF0ZSAqL1xuICAgICAgQE9wdGlvbmFsKCkgcHVibGljIHNjcm9sbGFibGVWaWV3UHJvcGVydGllczogU2Nyb2xsaW5nVmlzaWJpbGl0eSkge31cbn1cblxuLyoqXG4gKiBWYWxpZGF0ZXMgd2hldGhlciBhIHZlcnRpY2FsIHBvc2l0aW9uIHByb3BlcnR5IG1hdGNoZXMgdGhlIGV4cGVjdGVkIHZhbHVlcy5cbiAqIEBwYXJhbSBwcm9wZXJ0eSBOYW1lIG9mIHRoZSBwcm9wZXJ0eSBiZWluZyB2YWxpZGF0ZWQuXG4gKiBAcGFyYW0gdmFsdWUgVmFsdWUgb2YgdGhlIHByb3BlcnR5IGJlaW5nIHZhbGlkYXRlZC5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHZhbGlkYXRlVmVydGljYWxQb3NpdGlvbihwcm9wZXJ0eTogc3RyaW5nLCB2YWx1ZTogVmVydGljYWxDb25uZWN0aW9uUG9zKSB7XG4gIGlmICh2YWx1ZSAhPT0gJ3RvcCcgJiYgdmFsdWUgIT09ICdib3R0b20nICYmIHZhbHVlICE9PSAnY2VudGVyJykge1xuICAgIHRocm93IEVycm9yKGBDb25uZWN0ZWRQb3NpdGlvbjogSW52YWxpZCAke3Byb3BlcnR5fSBcIiR7dmFsdWV9XCIuIGAgK1xuICAgICAgICAgICAgICAgIGBFeHBlY3RlZCBcInRvcFwiLCBcImJvdHRvbVwiIG9yIFwiY2VudGVyXCIuYCk7XG4gIH1cbn1cblxuLyoqXG4gKiBWYWxpZGF0ZXMgd2hldGhlciBhIGhvcml6b250YWwgcG9zaXRpb24gcHJvcGVydHkgbWF0Y2hlcyB0aGUgZXhwZWN0ZWQgdmFsdWVzLlxuICogQHBhcmFtIHByb3BlcnR5IE5hbWUgb2YgdGhlIHByb3BlcnR5IGJlaW5nIHZhbGlkYXRlZC5cbiAqIEBwYXJhbSB2YWx1ZSBWYWx1ZSBvZiB0aGUgcHJvcGVydHkgYmVpbmcgdmFsaWRhdGVkLlxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5leHBvcnQgZnVuY3Rpb24gdmFsaWRhdGVIb3Jpem9udGFsUG9zaXRpb24ocHJvcGVydHk6IHN0cmluZywgdmFsdWU6IEhvcml6b250YWxDb25uZWN0aW9uUG9zKSB7XG4gIGlmICh2YWx1ZSAhPT0gJ3N0YXJ0JyAmJiB2YWx1ZSAhPT0gJ2VuZCcgJiYgdmFsdWUgIT09ICdjZW50ZXInKSB7XG4gICAgdGhyb3cgRXJyb3IoYENvbm5lY3RlZFBvc2l0aW9uOiBJbnZhbGlkICR7cHJvcGVydHl9IFwiJHt2YWx1ZX1cIi4gYCArXG4gICAgICAgICAgICAgICAgYEV4cGVjdGVkIFwic3RhcnRcIiwgXCJlbmRcIiBvciBcImNlbnRlclwiLmApO1xuICB9XG59XG4iXX0=