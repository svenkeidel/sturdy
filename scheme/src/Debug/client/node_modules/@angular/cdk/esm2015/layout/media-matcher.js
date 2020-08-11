/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Injectable } from '@angular/core';
import { Platform } from '@angular/cdk/platform';
import * as i0 from "@angular/core";
import * as i1 from "@angular/cdk/platform";
/** Global registry for all dynamically-created, injected media queries. */
const mediaQueriesForWebkitCompatibility = new Set();
/** Style tag that holds all of the dynamically-created media queries. */
let mediaQueryStyleNode;
/** A utility for calling matchMedia queries. */
let MediaMatcher = /** @class */ (() => {
    class MediaMatcher {
        constructor(_platform) {
            this._platform = _platform;
            this._matchMedia = this._platform.isBrowser && window.matchMedia ?
                // matchMedia is bound to the window scope intentionally as it is an illegal invocation to
                // call it from a different scope.
                window.matchMedia.bind(window) :
                noopMatchMedia;
        }
        /**
         * Evaluates the given media query and returns the native MediaQueryList from which results
         * can be retrieved.
         * Confirms the layout engine will trigger for the selector query provided and returns the
         * MediaQueryList for the query provided.
         */
        matchMedia(query) {
            if (this._platform.WEBKIT) {
                createEmptyStyleRule(query);
            }
            return this._matchMedia(query);
        }
    }
    MediaMatcher.ɵprov = i0.ɵɵdefineInjectable({ factory: function MediaMatcher_Factory() { return new MediaMatcher(i0.ɵɵinject(i1.Platform)); }, token: MediaMatcher, providedIn: "root" });
    MediaMatcher.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    MediaMatcher.ctorParameters = () => [
        { type: Platform }
    ];
    return MediaMatcher;
})();
export { MediaMatcher };
/**
 * For Webkit engines that only trigger the MediaQueryListListener when
 * there is at least one CSS selector for the respective media query.
 */
function createEmptyStyleRule(query) {
    if (mediaQueriesForWebkitCompatibility.has(query)) {
        return;
    }
    try {
        if (!mediaQueryStyleNode) {
            mediaQueryStyleNode = document.createElement('style');
            mediaQueryStyleNode.setAttribute('type', 'text/css');
            document.head.appendChild(mediaQueryStyleNode);
        }
        if (mediaQueryStyleNode.sheet) {
            mediaQueryStyleNode.sheet
                .insertRule(`@media ${query} {.fx-query-test{ }}`, 0);
            mediaQueriesForWebkitCompatibility.add(query);
        }
    }
    catch (e) {
        console.error(e);
    }
}
/** No-op matchMedia replacement for non-browser platforms. */
function noopMatchMedia(query) {
    // Use `as any` here to avoid adding additional necessary properties for
    // the noop matcher.
    return {
        matches: query === 'all' || query === '',
        media: query,
        addListener: () => { },
        removeListener: () => { }
    };
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibWVkaWEtbWF0Y2hlci5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9jZGsvbGF5b3V0L21lZGlhLW1hdGNoZXIudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBQ0gsT0FBTyxFQUFDLFVBQVUsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUN6QyxPQUFPLEVBQUMsUUFBUSxFQUFDLE1BQU0sdUJBQXVCLENBQUM7OztBQUUvQywyRUFBMkU7QUFDM0UsTUFBTSxrQ0FBa0MsR0FBZ0IsSUFBSSxHQUFHLEVBQVUsQ0FBQztBQUUxRSx5RUFBeUU7QUFDekUsSUFBSSxtQkFBaUQsQ0FBQztBQUV0RCxnREFBZ0Q7QUFDaEQ7SUFBQSxNQUNhLFlBQVk7UUFJdkIsWUFBb0IsU0FBbUI7WUFBbkIsY0FBUyxHQUFULFNBQVMsQ0FBVTtZQUNyQyxJQUFJLENBQUMsV0FBVyxHQUFHLElBQUksQ0FBQyxTQUFTLENBQUMsU0FBUyxJQUFJLE1BQU0sQ0FBQyxVQUFVLENBQUMsQ0FBQztnQkFDaEUsMEZBQTBGO2dCQUMxRixrQ0FBa0M7Z0JBQ2xDLE1BQU0sQ0FBQyxVQUFVLENBQUMsSUFBSSxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUM7Z0JBQ2hDLGNBQWMsQ0FBQztRQUNuQixDQUFDO1FBRUQ7Ozs7O1dBS0c7UUFDSCxVQUFVLENBQUMsS0FBYTtZQUN0QixJQUFJLElBQUksQ0FBQyxTQUFTLENBQUMsTUFBTSxFQUFFO2dCQUN6QixvQkFBb0IsQ0FBQyxLQUFLLENBQUMsQ0FBQzthQUM3QjtZQUNELE9BQU8sSUFBSSxDQUFDLFdBQVcsQ0FBQyxLQUFLLENBQUMsQ0FBQztRQUNqQyxDQUFDOzs7O2dCQXhCRixVQUFVLFNBQUMsRUFBQyxVQUFVLEVBQUUsTUFBTSxFQUFDOzs7Z0JBVHhCLFFBQVE7O3VCQVJoQjtLQTBDQztTQXhCWSxZQUFZO0FBMEJ6Qjs7O0dBR0c7QUFDSCxTQUFTLG9CQUFvQixDQUFDLEtBQWE7SUFDekMsSUFBSSxrQ0FBa0MsQ0FBQyxHQUFHLENBQUMsS0FBSyxDQUFDLEVBQUU7UUFDakQsT0FBTztLQUNSO0lBRUQsSUFBSTtRQUNGLElBQUksQ0FBQyxtQkFBbUIsRUFBRTtZQUN4QixtQkFBbUIsR0FBRyxRQUFRLENBQUMsYUFBYSxDQUFDLE9BQU8sQ0FBQyxDQUFDO1lBQ3RELG1CQUFtQixDQUFDLFlBQVksQ0FBQyxNQUFNLEVBQUUsVUFBVSxDQUFDLENBQUM7WUFDckQsUUFBUSxDQUFDLElBQUssQ0FBQyxXQUFXLENBQUMsbUJBQW1CLENBQUMsQ0FBQztTQUNqRDtRQUVELElBQUksbUJBQW1CLENBQUMsS0FBSyxFQUFFO1lBQzVCLG1CQUFtQixDQUFDLEtBQXVCO2lCQUN2QyxVQUFVLENBQUMsVUFBVSxLQUFLLHNCQUFzQixFQUFFLENBQUMsQ0FBQyxDQUFDO1lBQzFELGtDQUFrQyxDQUFDLEdBQUcsQ0FBQyxLQUFLLENBQUMsQ0FBQztTQUMvQztLQUNGO0lBQUMsT0FBTyxDQUFDLEVBQUU7UUFDVixPQUFPLENBQUMsS0FBSyxDQUFDLENBQUMsQ0FBQyxDQUFDO0tBQ2xCO0FBQ0gsQ0FBQztBQUVELDhEQUE4RDtBQUM5RCxTQUFTLGNBQWMsQ0FBQyxLQUFhO0lBQ25DLHdFQUF3RTtJQUN4RSxvQkFBb0I7SUFDcEIsT0FBTztRQUNMLE9BQU8sRUFBRSxLQUFLLEtBQUssS0FBSyxJQUFJLEtBQUssS0FBSyxFQUFFO1FBQ3hDLEtBQUssRUFBRSxLQUFLO1FBQ1osV0FBVyxFQUFFLEdBQUcsRUFBRSxHQUFFLENBQUM7UUFDckIsY0FBYyxFQUFFLEdBQUcsRUFBRSxHQUFFLENBQUM7S0FDbEIsQ0FBQztBQUNYLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cbmltcG9ydCB7SW5qZWN0YWJsZX0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQge1BsYXRmb3JtfSBmcm9tICdAYW5ndWxhci9jZGsvcGxhdGZvcm0nO1xuXG4vKiogR2xvYmFsIHJlZ2lzdHJ5IGZvciBhbGwgZHluYW1pY2FsbHktY3JlYXRlZCwgaW5qZWN0ZWQgbWVkaWEgcXVlcmllcy4gKi9cbmNvbnN0IG1lZGlhUXVlcmllc0ZvcldlYmtpdENvbXBhdGliaWxpdHk6IFNldDxzdHJpbmc+ID0gbmV3IFNldDxzdHJpbmc+KCk7XG5cbi8qKiBTdHlsZSB0YWcgdGhhdCBob2xkcyBhbGwgb2YgdGhlIGR5bmFtaWNhbGx5LWNyZWF0ZWQgbWVkaWEgcXVlcmllcy4gKi9cbmxldCBtZWRpYVF1ZXJ5U3R5bGVOb2RlOiBIVE1MU3R5bGVFbGVtZW50IHwgdW5kZWZpbmVkO1xuXG4vKiogQSB1dGlsaXR5IGZvciBjYWxsaW5nIG1hdGNoTWVkaWEgcXVlcmllcy4gKi9cbkBJbmplY3RhYmxlKHtwcm92aWRlZEluOiAncm9vdCd9KVxuZXhwb3J0IGNsYXNzIE1lZGlhTWF0Y2hlciB7XG4gIC8qKiBUaGUgaW50ZXJuYWwgbWF0Y2hNZWRpYSBtZXRob2QgdG8gcmV0dXJuIGJhY2sgYSBNZWRpYVF1ZXJ5TGlzdCBsaWtlIG9iamVjdC4gKi9cbiAgcHJpdmF0ZSBfbWF0Y2hNZWRpYTogKHF1ZXJ5OiBzdHJpbmcpID0+IE1lZGlhUXVlcnlMaXN0O1xuXG4gIGNvbnN0cnVjdG9yKHByaXZhdGUgX3BsYXRmb3JtOiBQbGF0Zm9ybSkge1xuICAgIHRoaXMuX21hdGNoTWVkaWEgPSB0aGlzLl9wbGF0Zm9ybS5pc0Jyb3dzZXIgJiYgd2luZG93Lm1hdGNoTWVkaWEgP1xuICAgICAgLy8gbWF0Y2hNZWRpYSBpcyBib3VuZCB0byB0aGUgd2luZG93IHNjb3BlIGludGVudGlvbmFsbHkgYXMgaXQgaXMgYW4gaWxsZWdhbCBpbnZvY2F0aW9uIHRvXG4gICAgICAvLyBjYWxsIGl0IGZyb20gYSBkaWZmZXJlbnQgc2NvcGUuXG4gICAgICB3aW5kb3cubWF0Y2hNZWRpYS5iaW5kKHdpbmRvdykgOlxuICAgICAgbm9vcE1hdGNoTWVkaWE7XG4gIH1cblxuICAvKipcbiAgICogRXZhbHVhdGVzIHRoZSBnaXZlbiBtZWRpYSBxdWVyeSBhbmQgcmV0dXJucyB0aGUgbmF0aXZlIE1lZGlhUXVlcnlMaXN0IGZyb20gd2hpY2ggcmVzdWx0c1xuICAgKiBjYW4gYmUgcmV0cmlldmVkLlxuICAgKiBDb25maXJtcyB0aGUgbGF5b3V0IGVuZ2luZSB3aWxsIHRyaWdnZXIgZm9yIHRoZSBzZWxlY3RvciBxdWVyeSBwcm92aWRlZCBhbmQgcmV0dXJucyB0aGVcbiAgICogTWVkaWFRdWVyeUxpc3QgZm9yIHRoZSBxdWVyeSBwcm92aWRlZC5cbiAgICovXG4gIG1hdGNoTWVkaWEocXVlcnk6IHN0cmluZyk6IE1lZGlhUXVlcnlMaXN0IHtcbiAgICBpZiAodGhpcy5fcGxhdGZvcm0uV0VCS0lUKSB7XG4gICAgICBjcmVhdGVFbXB0eVN0eWxlUnVsZShxdWVyeSk7XG4gICAgfVxuICAgIHJldHVybiB0aGlzLl9tYXRjaE1lZGlhKHF1ZXJ5KTtcbiAgfVxufVxuXG4vKipcbiAqIEZvciBXZWJraXQgZW5naW5lcyB0aGF0IG9ubHkgdHJpZ2dlciB0aGUgTWVkaWFRdWVyeUxpc3RMaXN0ZW5lciB3aGVuXG4gKiB0aGVyZSBpcyBhdCBsZWFzdCBvbmUgQ1NTIHNlbGVjdG9yIGZvciB0aGUgcmVzcGVjdGl2ZSBtZWRpYSBxdWVyeS5cbiAqL1xuZnVuY3Rpb24gY3JlYXRlRW1wdHlTdHlsZVJ1bGUocXVlcnk6IHN0cmluZykge1xuICBpZiAobWVkaWFRdWVyaWVzRm9yV2Via2l0Q29tcGF0aWJpbGl0eS5oYXMocXVlcnkpKSB7XG4gICAgcmV0dXJuO1xuICB9XG5cbiAgdHJ5IHtcbiAgICBpZiAoIW1lZGlhUXVlcnlTdHlsZU5vZGUpIHtcbiAgICAgIG1lZGlhUXVlcnlTdHlsZU5vZGUgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCdzdHlsZScpO1xuICAgICAgbWVkaWFRdWVyeVN0eWxlTm9kZS5zZXRBdHRyaWJ1dGUoJ3R5cGUnLCAndGV4dC9jc3MnKTtcbiAgICAgIGRvY3VtZW50LmhlYWQhLmFwcGVuZENoaWxkKG1lZGlhUXVlcnlTdHlsZU5vZGUpO1xuICAgIH1cblxuICAgIGlmIChtZWRpYVF1ZXJ5U3R5bGVOb2RlLnNoZWV0KSB7XG4gICAgICAobWVkaWFRdWVyeVN0eWxlTm9kZS5zaGVldCBhcyBDU1NTdHlsZVNoZWV0KVxuICAgICAgICAgIC5pbnNlcnRSdWxlKGBAbWVkaWEgJHtxdWVyeX0gey5meC1xdWVyeS10ZXN0eyB9fWAsIDApO1xuICAgICAgbWVkaWFRdWVyaWVzRm9yV2Via2l0Q29tcGF0aWJpbGl0eS5hZGQocXVlcnkpO1xuICAgIH1cbiAgfSBjYXRjaCAoZSkge1xuICAgIGNvbnNvbGUuZXJyb3IoZSk7XG4gIH1cbn1cblxuLyoqIE5vLW9wIG1hdGNoTWVkaWEgcmVwbGFjZW1lbnQgZm9yIG5vbi1icm93c2VyIHBsYXRmb3Jtcy4gKi9cbmZ1bmN0aW9uIG5vb3BNYXRjaE1lZGlhKHF1ZXJ5OiBzdHJpbmcpOiBNZWRpYVF1ZXJ5TGlzdCB7XG4gIC8vIFVzZSBgYXMgYW55YCBoZXJlIHRvIGF2b2lkIGFkZGluZyBhZGRpdGlvbmFsIG5lY2Vzc2FyeSBwcm9wZXJ0aWVzIGZvclxuICAvLyB0aGUgbm9vcCBtYXRjaGVyLlxuICByZXR1cm4ge1xuICAgIG1hdGNoZXM6IHF1ZXJ5ID09PSAnYWxsJyB8fCBxdWVyeSA9PT0gJycsXG4gICAgbWVkaWE6IHF1ZXJ5LFxuICAgIGFkZExpc3RlbmVyOiAoKSA9PiB7fSxcbiAgICByZW1vdmVMaXN0ZW5lcjogKCkgPT4ge31cbiAgfSBhcyBhbnk7XG59XG4iXX0=