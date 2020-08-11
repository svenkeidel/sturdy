/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ConnectionPositionPair, } from './connected-position';
import { FlexibleConnectedPositionStrategy } from './flexible-connected-position-strategy';
/**
 * A strategy for positioning overlays. Using this strategy, an overlay is given an
 * implicit position relative to some origin element. The relative position is defined in terms of
 * a point on the origin element that is connected to a point on the overlay element. For example,
 * a basic dropdown is connecting the bottom-left corner of the origin to the top-left corner
 * of the overlay.
 * @deprecated Use `FlexibleConnectedPositionStrategy` instead.
 * @breaking-change 8.0.0
 */
export class ConnectedPositionStrategy {
    constructor(originPos, overlayPos, connectedTo, viewportRuler, document, platform, overlayContainer) {
        /** Ordered list of preferred positions, from most to least desirable. */
        this._preferredPositions = [];
        // Since the `ConnectedPositionStrategy` is deprecated and we don't want to maintain
        // the extra logic, we create an instance of the positioning strategy that has some
        // defaults that make it behave as the old position strategy and to which we'll
        // proxy all of the API calls.
        this._positionStrategy = new FlexibleConnectedPositionStrategy(connectedTo, viewportRuler, document, platform, overlayContainer)
            .withFlexibleDimensions(false)
            .withPush(false)
            .withViewportMargin(0);
        this.withFallbackPosition(originPos, overlayPos);
    }
    /** Whether the we're dealing with an RTL context */
    get _isRtl() {
        return this._overlayRef.getDirection() === 'rtl';
    }
    /** Emits an event when the connection point changes. */
    get onPositionChange() {
        return this._positionStrategy.positionChanges;
    }
    /** Ordered list of preferred positions, from most to least desirable. */
    get positions() {
        return this._preferredPositions;
    }
    /** Attach this position strategy to an overlay. */
    attach(overlayRef) {
        this._overlayRef = overlayRef;
        this._positionStrategy.attach(overlayRef);
        if (this._direction) {
            overlayRef.setDirection(this._direction);
            this._direction = null;
        }
    }
    /** Disposes all resources used by the position strategy. */
    dispose() {
        this._positionStrategy.dispose();
    }
    /** @docs-private */
    detach() {
        this._positionStrategy.detach();
    }
    /**
     * Updates the position of the overlay element, using whichever preferred position relative
     * to the origin fits on-screen.
     * @docs-private
     */
    apply() {
        this._positionStrategy.apply();
    }
    /**
     * Re-positions the overlay element with the trigger in its last calculated position,
     * even if a position higher in the "preferred positions" list would now fit. This
     * allows one to re-align the panel without changing the orientation of the panel.
     */
    recalculateLastPosition() {
        this._positionStrategy.reapplyLastPosition();
    }
    /**
     * Sets the list of Scrollable containers that host the origin element so that
     * on reposition we can evaluate if it or the overlay has been clipped or outside view. Every
     * Scrollable must be an ancestor element of the strategy's origin element.
     */
    withScrollableContainers(scrollables) {
        this._positionStrategy.withScrollableContainers(scrollables);
    }
    /**
     * Adds a new preferred fallback position.
     * @param originPos
     * @param overlayPos
     */
    withFallbackPosition(originPos, overlayPos, offsetX, offsetY) {
        const position = new ConnectionPositionPair(originPos, overlayPos, offsetX, offsetY);
        this._preferredPositions.push(position);
        this._positionStrategy.withPositions(this._preferredPositions);
        return this;
    }
    /**
     * Sets the layout direction so the overlay's position can be adjusted to match.
     * @param dir New layout direction.
     */
    withDirection(dir) {
        // Since the direction might be declared before the strategy is attached,
        // we save the value in a temporary property and we'll transfer it to the
        // overlay ref on attachment.
        if (this._overlayRef) {
            this._overlayRef.setDirection(dir);
        }
        else {
            this._direction = dir;
        }
        return this;
    }
    /**
     * Sets an offset for the overlay's connection point on the x-axis
     * @param offset New offset in the X axis.
     */
    withOffsetX(offset) {
        this._positionStrategy.withDefaultOffsetX(offset);
        return this;
    }
    /**
     * Sets an offset for the overlay's connection point on the y-axis
     * @param  offset New offset in the Y axis.
     */
    withOffsetY(offset) {
        this._positionStrategy.withDefaultOffsetY(offset);
        return this;
    }
    /**
     * Sets whether the overlay's position should be locked in after it is positioned
     * initially. When an overlay is locked in, it won't attempt to reposition itself
     * when the position is re-applied (e.g. when the user scrolls away).
     * @param isLocked Whether the overlay should locked in.
     */
    withLockedPosition(isLocked) {
        this._positionStrategy.withLockedPosition(isLocked);
        return this;
    }
    /**
     * Overwrites the current set of positions with an array of new ones.
     * @param positions Position pairs to be set on the strategy.
     */
    withPositions(positions) {
        this._preferredPositions = positions.slice();
        this._positionStrategy.withPositions(this._preferredPositions);
        return this;
    }
    /**
     * Sets the origin element, relative to which to position the overlay.
     * @param origin Reference to the new origin element.
     */
    setOrigin(origin) {
        this._positionStrategy.setOrigin(origin);
        return this;
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY29ubmVjdGVkLXBvc2l0aW9uLXN0cmF0ZWd5LmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9vdmVybGF5L3Bvc2l0aW9uL2Nvbm5lY3RlZC1wb3NpdGlvbi1zdHJhdGVneS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFXSCxPQUFPLEVBRUwsc0JBQXNCLEdBR3ZCLE1BQU0sc0JBQXNCLENBQUM7QUFDOUIsT0FBTyxFQUFDLGlDQUFpQyxFQUFDLE1BQU0sd0NBQXdDLENBQUM7QUFHekY7Ozs7Ozs7O0dBUUc7QUFDSCxNQUFNLE9BQU8seUJBQXlCO0lBeUJwQyxZQUNJLFNBQW1DLEVBQUUsVUFBcUMsRUFDMUUsV0FBb0MsRUFBRSxhQUE0QixFQUFFLFFBQWtCLEVBQ3RGLFFBQWtCLEVBQUUsZ0JBQWtDO1FBWDFELHlFQUF5RTtRQUN6RSx3QkFBbUIsR0FBNkIsRUFBRSxDQUFDO1FBV2pELG9GQUFvRjtRQUNwRixtRkFBbUY7UUFDbkYsK0VBQStFO1FBQy9FLDhCQUE4QjtRQUM5QixJQUFJLENBQUMsaUJBQWlCLEdBQUcsSUFBSSxpQ0FBaUMsQ0FDakMsV0FBVyxFQUFFLGFBQWEsRUFBRSxRQUFRLEVBQUUsUUFBUSxFQUFFLGdCQUFnQixDQUFDO2FBQ2hFLHNCQUFzQixDQUFDLEtBQUssQ0FBQzthQUM3QixRQUFRLENBQUMsS0FBSyxDQUFDO2FBQ2Ysa0JBQWtCLENBQUMsQ0FBQyxDQUFDLENBQUM7UUFFcEQsSUFBSSxDQUFDLG9CQUFvQixDQUFDLFNBQVMsRUFBRSxVQUFVLENBQUMsQ0FBQztJQUNuRCxDQUFDO0lBNUJELG9EQUFvRDtJQUNwRCxJQUFJLE1BQU07UUFDUixPQUFPLElBQUksQ0FBQyxXQUFXLENBQUMsWUFBWSxFQUFFLEtBQUssS0FBSyxDQUFDO0lBQ25ELENBQUM7SUFLRCx3REFBd0Q7SUFDeEQsSUFBSSxnQkFBZ0I7UUFDbEIsT0FBTyxJQUFJLENB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SUFBSSxDQUFDLG1CQUFtQixDQUFDLENBQUM7UUFDL0QsT0FBTyxJQUFJLENBQUM7SUFDZCxDQUFDO0lBRUQ7OztPQUdHO0lBQ0gsU0FBUyxDQUFDLE1BQWtCO1FBQzFCLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxTQUFTLENBQUMsTUFBTSxDQUFDLENBQUM7UUFDekMsT0FBTyxJQUFJLENBQUM7SUFDZCxDQUFDO0NBQ0YiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtEaXJlY3Rpb259IGZyb20gJ0Bhbmd1bGFyL2Nkay9iaWRpJztcbmltcG9ydCB7UGxhdGZvcm19IGZyb20gJ0Bhbmd1bGFyL2Nkay9wbGF0Zm9ybSc7XG5pbXBvcnQge0Nka1Njcm9sbGFibGUsIFZpZXdwb3J0UnVsZXJ9IGZyb20gJ0Bhbmd1bGFyL2Nkay9zY3JvbGxpbmcnO1xuaW1wb3J0IHtFbGVtZW50UmVmfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7T2JzZXJ2YWJsZX0gZnJvbSAncnhqcyc7XG5cbmltcG9ydCB7T3ZlcmxheUNvbnRhaW5lcn0gZnJvbSAnLi4vb3ZlcmxheS1jb250YWluZXInO1xuaW1wb3J0IHtPdmVybGF5UmVmZXJlbmNlfSBmcm9tICcuLi9vdmVybGF5LXJlZmVyZW5jZSc7XG5cbmltcG9ydCB7XG4gIENvbm5lY3RlZE92ZXJsYXlQb3NpdGlvbkNoYW5nZSxcbiAgQ29ubmVjdGlvblBvc2l0aW9uUGFpcixcbiAgT3JpZ2luQ29ubmVjdGlvblBvc2l0aW9uLFxuICBPdmVybGF5Q29ubmVjdGlvblBvc2l0aW9uLFxufSBmcm9tICcuL2Nvbm5lY3RlZC1wb3NpdGlvbic7XG5pbXBvcnQge0ZsZXhpYmxlQ29ubmVjdGVkUG9zaXRpb25TdHJhdGVneX0gZnJvbSAnLi9mbGV4aWJsZS1jb25uZWN0ZWQtcG9zaXRpb24tc3RyYXRlZ3knO1xuaW1wb3J0IHtQb3NpdGlvblN0cmF0ZWd5fSBmcm9tICcuL3Bvc2l0aW9uLXN0cmF0ZWd5JztcblxuLyoqXG4gKiBBIHN0cmF0ZWd5IGZvciBwb3NpdGlvbmluZyBvdmVybGF5cy4gVXNpbmcgdGhpcyBzdHJhdGVneSwgYW4gb3ZlcmxheSBpcyBnaXZlbiBhblxuICogaW1wbGljaXQgcG9zaXRpb24gcmVsYXRpdmUgdG8gc29tZSBvcmlnaW4gZWxlbWVudC4gVGhlIHJlbGF0aXZlIHBvc2l0aW9uIGlzIGRlZmluZWQgaW4gdGVybXMgb2ZcbiAqIGEgcG9pbnQgb24gdGhlIG9yaWdpbiBlbGVtZW50IHRoYXQgaXMgY29ubmVjdGVkIHRvIGEgcG9pbnQgb24gdGhlIG92ZXJsYXkgZWxlbWVudC4gRm9yIGV4YW1wbGUsXG4gKiBhIGJhc2ljIGRyb3Bkb3duIGlzIGNvbm5lY3RpbmcgdGhlIGJvdHRvbS1sZWZ0IGNvcm5lciBvZiB0aGUgb3JpZ2luIHRvIHRoZSB0b3AtbGVmdCBjb3JuZXJcbiAqIG9mIHRoZSBvdmVybGF5LlxuICogQGRlcHJlY2F0ZWQgVXNlIGBGbGV4aWJsZUNvbm5lY3RlZFBvc2l0aW9uU3RyYXRlZ3lgIGluc3RlYWQuXG4gKiBAYnJlYWtpbmctY2hhbmdlIDguMC4wXG4gKi9cbmV4cG9ydCBjbGFzcyBDb25uZWN0ZWRQb3NpdGlvblN0cmF0ZWd5IGltcGxlbWVudHMgUG9zaXRpb25TdHJhdGVneSB7XG4gIC8qKlxuICAgKiBSZWZlcmVuY2UgdG8gdGhlIHVuZGVybHlpbmcgcG9zaXRpb24gc3RyYXRlZ3kgdG8gd2hpY2ggYWxsIHRoZSBBUEkgY2FsbHMgYXJlIHByb3hpZWQuXG4gICAqIEBkb2NzLXByaXZhdGVcbiAgICovXG4gIF9wb3NpdGlvblN0cmF0ZWd5OiBGbGV4aWJsZUNvbm5lY3RlZFBvc2l0aW9uU3RyYXRlZ3k7XG5cbiAgLyoqIFRoZSBvdmVybGF5IHRvIHdoaWNoIHRoaXMgc3RyYXRlZ3kgaXMgYXR0YWNoZWQuICovXG4gIHByaXZhdGUgX292ZXJsYXlSZWY6IE92ZXJsYXlSZWZlcmVuY2U7XG5cbiAgcHJpdmF0ZSBfZGlyZWN0aW9uOiBEaXJlY3Rpb24gfCBudWxsO1xuXG4gIC8qKiBXaGV0aGVyIHRoZSB3ZSdyZSBkZWFsaW5nIHdpdGggYW4gUlRMIGNvbnRleHQgKi9cbiAgZ2V0IF9pc1J0bCgpIHtcbiAgICByZXR1cm4gdGhpcy5fb3ZlcmxheVJlZi5nZXREaXJlY3Rpb24oKSA9PT0gJ3J0bCc7XG4gIH1cblxuICAvKiogT3JkZXJlZCBsaXN0IG9mIHByZWZlcnJlZCBwb3NpdGlvbnMsIGZyb20gbW9zdCB0byBsZWFzdCBkZXNpcmFibGUuICovXG4gIF9wcmVmZXJyZWRQb3NpdGlvbnM6IENvbm5lY3Rpb25Qb3NpdGlvblBhaXJbXSA9IFtdO1xuXG4gIC8qKiBFbWl0cyBhbiBldmVudCB3aGVuIHRoZSBjb25uZWN0aW9uIHBvaW50IGNoYW5nZXMuICovXG4gIGdldCBvblBvc2l0aW9uQ2hhbmdlKCk6IE9ic2VydmFibGU8Q29ubmVjdGVkT3ZlcmxheVBvc2l0aW9uQ2hhbmdlPiB7XG4gICAgcmV0dXJuIHRoaXMuX3Bvc2l0aW9uU3RyYXRlZ3kucG9zaXRpb25DaGFuZ2VzO1xuICB9XG5cbiAgY29uc3RydWN0b3IoXG4gICAgICBvcmlnaW5Qb3M6IE9yaWdpbkNvbm5lY3Rpb25Qb3NpdGlvbiwgb3ZlcmxheVBvczogT3ZlcmxheUNvbm5lY3Rpb25Qb3NpdGlvbixcbiAgICAgIGNvbm5lY3RlZFRvOiBFbGVtZW50UmVmPEhUTUxFbGVtZW50Piwgdmlld3BvcnRSdWxlcjogVmlld3BvcnRSdWxlciwgZG9jdW1lbnQ6IERvY3VtZW50LFxuICAgICAgcGxhdGZvcm06IFBsYXRmb3JtLCBvdmVybGF5Q29udGFpbmVyOiBPdmVybGF5Q29udGFpbmVyKSB7XG4gICAgLy8gU2luY2UgdGhlIGBDb25uZWN0ZWRQb3NpdGlvblN0cmF0ZWd5YCBpcyBkZXByZWNhdGVkIGFuZCB3ZSBkb24ndCB3YW50IHRvIG1haW50YWluXG4gICAgLy8gdGhlIGV4dHJhIGxvZ2ljLCB3ZSBjcmVhdGUgYW4gaW5zdGFuY2Ugb2YgdGhlIHBvc2l0aW9uaW5nIHN0cmF0ZWd5IHRoYXQgaGFzIHNvbWVcbiAgICAvLyBkZWZhdWx0cyB0aGF0IG1ha2UgaXQgYmVoYXZlIGFzIHRoZSBvbGQgcG9zaXRpb24gc3RyYXRlZ3kgYW5kIHRvIHdoaWNoIHdlJ2xsXG4gICAgLy8gcHJveHkgYWxsIG9mIHRoZSBBUEkgY2FsbHMuXG4gICAgdGhpcy5fcG9zaXRpb25TdHJhdGVneSA9IG5ldyBGbGV4aWJsZUNvbm5lY3RlZFBvc2l0aW9uU3RyYXRlZ3koXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBjb25uZWN0ZWRUbywgdmlld3BvcnRSdWxlciwgZG9jdW1lbnQsIHBsYXRmb3JtLCBvdmVybGF5Q29udGFpbmVyKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLndpdGhGbGV4aWJsZURpbWVuc2lvbnMoZmFsc2UpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAud2l0aFB1c2goZmFsc2UpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAud2l0aFZpZXdwb3J0TWFyZ2luKDApO1xuXG4gICAgdGhpcy53aXRoRmFsbGJhY2tQb3NpdGlvbihvcmlnaW5Qb3MsIG92ZXJsYXlQb3MpO1xuICB9XG5cbiAgLyoqIE9yZGVyZWQgbGlzdCBvZiBwcmVmZXJyZWQgcG9zaXRpb25zLCBmcm9tIG1vc3QgdG8gbGVhc3QgZGVzaXJhYmxlLiAqL1xuICBnZXQgcG9zaXRpb25zKCk6IENvbm5lY3Rpb25Qb3NpdGlvblBhaXJbXSB7XG4gICAgcmV0dXJuIHRoaXMuX3ByZWZlcnJlZFBvc2l0aW9ucztcbiAgfVxuXG4gIC8qKiBBdHRhY2ggdGhpcyBwb3NpdGlvbiBzdHJhdGVneSB0byBhbiBvdmVybGF5LiAqL1xuICBhdHRhY2gob3ZlcmxheVJlZjogT3ZlcmxheVJlZmVyZW5jZSk6IHZvaWQge1xuICAgIHRoaXMuX292ZXJsYXlSZWYgPSBvdmVybGF5UmVmO1xuICAgIHRoaXMuX3Bvc2l0aW9uU3RyYXRlZ3kuYXR0YWNoKG92ZXJsYXlSZWYpO1xuXG4gICAgaWYgKHRoaXMuX2RpcmVjdGlvbikge1xuICAgICAgb3ZlcmxheVJlZi5zZXREaXJlY3Rpb24odGhpcy5fZGlyZWN0aW9uKTtcbiAgICAgIHRoaXMuX2RpcmVjdGlvbiA9IG51bGw7XG4gICAgfVxuICB9XG5cbiAgLyoqIERpc3Bvc2VzIGFsbCByZXNvdXJjZXMgdXNlZCBieSB0aGUgcG9zaXRpb24gc3RyYXRlZ3kuICovXG4gIGRpc3Bvc2UoKSB7XG4gICAgdGhpcy5fcG9zaXRpb25TdHJhdGVneS5kaXNwb3NlKCk7XG4gIH1cblxuICAvKiogQGRvY3MtcHJpdmF0ZSAqL1xuICBkZXRhY2goKSB7XG4gICAgdGhpcy5fcG9zaXRpb25TdHJhdGVneS5kZXRhY2goKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBVcGRhdGVzIHRoZSBwb3NpdGlvbiBvZiB0aGUgb3ZlcmxheSBlbGVtZW50LCB1c2luZyB3aGljaGV2ZXIgcHJlZmVycmVkIHBvc2l0aW9uIHJlbGF0aXZlXG4gICAqIHRvIHRoZSBvcmlnaW4gZml0cyBvbi1zY3JlZW4uXG4gICAqIEBkb2NzLXByaXZhdGVcbiAgICovXG4gIGFwcGx5KCk6IHZvaWQge1xuICAgIHRoaXMuX3Bvc2l0aW9uU3RyYXRlZ3kuYXBwbHkoKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZS1wb3NpdGlvbnMgdGhlIG92ZXJsYXkgZWxlbWVudCB3aXRoIHRoZSB0cmlnZ2VyIGluIGl0cyBsYXN0IGNhbGN1bGF0ZWQgcG9zaXRpb24sXG4gICAqIGV2ZW4gaWYgYSBwb3NpdGlvbiBoaWdoZXIgaW4gdGhlIFwicHJlZmVycmVkIHBvc2l0aW9uc1wiIGxpc3Qgd291bGQgbm93IGZpdC4gVGhpc1xuICAgKiBhbGxvd3Mgb25lIHRvIHJlLWFsaWduIHRoZSBwYW5lbCB3aXRob3V0IGNoYW5naW5nIHRoZSBvcmllbnRhdGlvbiBvZiB0aGUgcGFuZWwuXG4gICAqL1xuICByZWNhbGN1bGF0ZUxhc3RQb3NpdGlvbigpOiB2b2lkIHtcbiAgICB0aGlzLl9wb3NpdGlvblN0cmF0ZWd5LnJlYXBwbHlMYXN0UG9zaXRpb24oKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBTZXRzIHRoZSBsaXN0IG9mIFNjcm9sbGFibGUgY29udGFpbmVycyB0aGF0IGhvc3QgdGhlIG9yaWdpbiBlbGVtZW50IHNvIHRoYXRcbiAgICogb24gcmVwb3NpdGlvbiB3ZSBjYW4gZXZhbHVhdGUgaWYgaXQgb3IgdGhlIG92ZXJsYXkgaGFzIGJlZW4gY2xpcHBlZCBvciBvdXRzaWRlIHZpZXcuIEV2ZXJ5XG4gICAqIFNjcm9sbGFibGUgbXVzdCBiZSBhbiBhbmNlc3RvciBlbGVtZW50IG9mIHRoZSBzdHJhdGVneSdzIG9yaWdpbiBlbGVtZW50LlxuICAgKi9cbiAgd2l0aFNjcm9sbGFibGVDb250YWluZXJzKHNjcm9sbGFibGVzOiBDZGtTY3JvbGxhYmxlW10pIHtcbiAgICB0aGlzLl9wb3NpdGlvblN0cmF0ZWd5LndpdGhTY3JvbGxhYmxlQ29udGFpbmVycyhzY3JvbGxhYmxlcyk7XG4gIH1cblxuICAvKipcbiAgICogQWRkcyBhIG5ldyBwcmVmZXJyZWQgZmFsbGJhY2sgcG9zaXRpb24uXG4gICAqIEBwYXJhbSBvcmlnaW5Qb3NcbiAgICogQHBhcmFtIG92ZXJsYXlQb3NcbiAgICovXG4gIHdpdGhGYWxsYmFja1Bvc2l0aW9uKFxuICAgICAgb3JpZ2luUG9zOiBPcmlnaW5Db25uZWN0aW9uUG9zaXRpb24sXG4gICAgICBvdmVybGF5UG9zOiBPdmVybGF5Q29ubmVjdGlvblBvc2l0aW9uLFxuICAgICAgb2Zmc2V0WD86IG51bWJlcixcbiAgICAgIG9mZnNldFk/OiBudW1iZXIpOiB0aGlzIHtcblxuICAgIGNvbnN0IHBvc2l0aW9uID0gbmV3IENvbm5lY3Rpb25Qb3NpdGlvblBhaXIob3JpZ2luUG9zLCBvdmVybGF5UG9zLCBvZmZzZXRYLCBvZmZzZXRZKTtcbiAgICB0aGlzLl9wcmVmZXJyZWRQb3NpdGlvbnMucHVzaChwb3NpdGlvbik7XG4gICAgdGhpcy5fcG9zaXRpb25TdHJhdGVneS53aXRoUG9zaXRpb25zKHRoaXMuX3ByZWZlcnJlZFBvc2l0aW9ucyk7XG4gICAgcmV0dXJuIHRoaXM7XG4gIH1cblxuICAvKipcbiAgICogU2V0cyB0aGUgbGF5b3V0IGRpcmVjdGlvbiBzbyB0aGUgb3ZlcmxheSdzIHBvc2l0aW9uIGNhbiBiZSBhZGp1c3RlZCB0byBtYXRjaC5cbiAgICogQHBhcmFtIGRpciBOZXcgbGF5b3V0IGRpcmVjdGlvbi5cbiAgICovXG4gIHdpdGhEaXJlY3Rpb24oZGlyOiAnbHRyJyB8ICdydGwnKTogdGhpcyB7XG4gICAgLy8gU2luY2UgdGhlIGRpcmVjdGlvbiBtaWdodCBiZSBkZWNsYXJlZCBiZWZvcmUgdGhlIHN0cmF0ZWd5IGlzIGF0dGFjaGVkLFxuICAgIC8vIHdlIHNhdmUgdGhlIHZhbHVlIGluIGEgdGVtcG9yYXJ5IHByb3BlcnR5IGFuZCB3ZSdsbCB0cmFuc2ZlciBpdCB0byB0aGVcbiAgICAvLyBvdmVybGF5IHJlZiBvbiBhdHRhY2htZW50LlxuICAgIGlmICh0aGlzLl9vdmVybGF5UmVmKSB7XG4gICAgICB0aGlzLl9vdmVybGF5UmVmLnNldERpcmVjdGlvbihkaXIpO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLl9kaXJlY3Rpb24gPSBkaXI7XG4gICAgfVxuXG4gICAgcmV0dXJuIHRoaXM7XG4gIH1cblxuICAvKipcbiAgICogU2V0cyBhbiBvZmZzZXQgZm9yIHRoZSBvdmVybGF5J3MgY29ubmVjdGlvbiBwb2ludCBvbiB0aGUgeC1heGlzXG4gICAqIEBwYXJhbSBvZmZzZXQgTmV3IG9mZnNldCBpbiB0aGUgWCBheGlzLlxuICAgKi9cbiAgd2l0aE9mZnNldFgob2Zmc2V0OiBudW1iZXIpOiB0aGlzIHtcbiAgICB0aGlzLl9wb3NpdGlvblN0cmF0ZWd5LndpdGhEZWZhdWx0T2Zmc2V0WChvZmZzZXQpO1xuICAgIHJldHVybiB0aGlzO1xuICB9XG5cbiAgLyoqXG4gICAqIFNldHMgYW4gb2Zmc2V0IGZvciB0aGUgb3ZlcmxheSdzIGNvbm5lY3Rpb24gcG9pbnQgb24gdGhlIHktYXhpc1xuICAgKiBAcGFyYW0gIG9mZnNldCBOZXcgb2Zmc2V0IGluIHRoZSBZIGF4aXMuXG4gICAqL1xuICB3aXRoT2Zmc2V0WShvZmZzZXQ6IG51bWJlcik6IHRoaXMge1xuICAgIHRoaXMuX3Bvc2l0aW9uU3RyYXRlZ3kud2l0aERlZmF1bHRPZmZzZXRZKG9mZnNldCk7XG4gICAgcmV0dXJuIHRoaXM7XG4gIH1cblxuICAvKipcbiAgICogU2V0cyB3aGV0aGVyIHRoZSBvdmVybGF5J3MgcG9zaXRpb24gc2hvdWxkIGJlIGxvY2tlZCBpbiBhZnRlciBpdCBpcyBwb3NpdGlvbmVkXG4gICAqIGluaXRpYWxseS4gV2hlbiBhbiBvdmVybGF5IGlzIGxvY2tlZCBpbiwgaXQgd29uJ3QgYXR0ZW1wdCB0byByZXBvc2l0aW9uIGl0c2VsZlxuICAgKiB3aGVuIHRoZSBwb3NpdGlvbiBpcyByZS1hcHBsaWVkIChlLmcuIHdoZW4gdGhlIHVzZXIgc2Nyb2xscyBhd2F5KS5cbiAgICogQHBhcmFtIGlzTG9ja2VkIFdoZXRoZXIgdGhlIG92ZXJsYXkgc2hvdWxkIGxvY2tlZCBpbi5cbiAgICovXG4gIHdpdGhMb2NrZWRQb3NpdGlvbihpc0xvY2tlZDogYm9vbGVhbik6IHRoaXMge1xuICAgIHRoaXMuX3Bvc2l0aW9uU3RyYXRlZ3kud2l0aExvY2tlZFBvc2l0aW9uKGlzTG9ja2VkKTtcbiAgICByZXR1cm4gdGhpcztcbiAgfVxuXG4gIC8qKlxuICAgKiBPdmVyd3JpdGVzIHRoZSBjdXJyZW50IHNldCBvZiBwb3NpdGlvbnMgd2l0aCBhbiBhcnJheSBvZiBuZXcgb25lcy5cbiAgICogQHBhcmFtIHBvc2l0aW9ucyBQb3NpdGlvbiBwYWlycyB0byBiZSBzZXQgb24gdGhlIHN0cmF0ZWd5LlxuICAgKi9cbiAgd2l0aFBvc2l0aW9ucyhwb3NpdGlvbnM6IENvbm5lY3Rpb25Qb3NpdGlvblBhaXJbXSk6IHRoaXMge1xuICAgIHRoaXMuX3ByZWZlcnJlZFBvc2l0aW9ucyA9IHBvc2l0aW9ucy5zbGljZSgpO1xuICAgIHRoaXMuX3Bvc2l0aW9uU3RyYXRlZ3kud2l0aFBvc2l0aW9ucyh0aGlzLl9wcmVmZXJyZWRQb3NpdGlvbnMpO1xuICAgIHJldHVybiB0aGlzO1xuICB9XG5cbiAgLyoqXG4gICAqIFNldHMgdGhlIG9yaWdpbiBlbGVtZW50LCByZWxhdGl2ZSB0byB3aGljaCB0byBwb3NpdGlvbiB0aGUgb3ZlcmxheS5cbiAgICogQHBhcmFtIG9yaWdpbiBSZWZlcmVuY2UgdG8gdGhlIG5ldyBvcmlnaW4gZWxlbWVudC5cbiAgICovXG4gIHNldE9yaWdpbihvcmlnaW46IEVsZW1lbnRSZWYpOiB0aGlzIHtcbiAgICB0aGlzLl9wb3NpdGlvblN0cmF0ZWd5LnNldE9yaWdpbihvcmlnaW4pO1xuICAgIHJldHVybiB0aGlzO1xuICB9XG59XG4iXX0=