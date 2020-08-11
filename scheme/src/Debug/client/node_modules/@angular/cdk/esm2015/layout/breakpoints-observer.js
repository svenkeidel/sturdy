/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Injectable, NgZone } from '@angular/core';
import { MediaMatcher } from './media-matcher';
import { combineLatest, concat, Observable, Subject } from 'rxjs';
import { debounceTime, map, skip, startWith, take, takeUntil } from 'rxjs/operators';
import { coerceArray } from '@angular/cdk/coercion';
import * as i0 from "@angular/core";
import * as i1 from "./media-matcher";
/** Utility for checking the matching state of @media queries. */
let BreakpointObserver = /** @class */ (() => {
    class BreakpointObserver {
        constructor(_mediaMatcher, _zone) {
            this._mediaMatcher = _mediaMatcher;
            this._zone = _zone;
            /**  A map of all media queries currently being listened for. */
            this._queries = new Map();
            /** A subject for all other observables to takeUntil based on. */
            this._destroySubject = new Subject();
        }
        /** Completes the active subject, signalling to all other observables to complete. */
        ngOnDestroy() {
            this._destroySubject.next();
            this._destroySubject.complete();
        }
        /**
         * Whether one or more media queries match the current viewport size.
         * @param value One or more media queries to check.
         * @returns Whether any of the media queries match.
         */
        isMatched(value) {
            const queries = splitQueries(coerceArray(value));
            return queries.some(mediaQuery => this._registerQuery(mediaQuery).mql.matches);
        }
        /**
         * Gets an observable of results for the given queries that will emit new results for any changes
         * in matching of the given queries.
         * @param value One or more media queries to check.
         * @returns A stream of matches for the given queries.
         */
        observe(value) {
            const queries = splitQueries(coerceArray(value));
            const observables = queries.map(query => this._registerQuery(query).observable);
            let stateObservable = combineLatest(observables);
            // Emit the first state immediately, and then debounce the subsequent emissions.
            stateObservable = concat(stateObservable.pipe(take(1)), stateObservable.pipe(skip(1), debounceTime(0)));
            return stateObservable.pipe(map((breakpointStates) => {
                const response = {
                    matches: false,
                    breakpoints: {},
                };
                breakpointStates.forEach((state) => {
                    response.matches = response.matches || state.matches;
                    response.breakpoints[state.query] = state.matches;
                });
                return response;
            }));
        }
        /** Registers a specific query to be listened for. */
        _registerQuery(query) {
            // Only set up a new MediaQueryList if it is not already being listened for.
            if (this._queries.has(query)) {
                return this._queries.get(query);
            }
            const mql = this._mediaMatcher.matchMedia(query);
            // Create callback for match changes and add it is as a listener.
            const queryObservable = new Observable((observer) => {
                // Listener callback methods are wrapped to be placed back in ngZone. Callbacks must be placed
                // back into the zone because matchMedia is only included in Zone.js by loading the
                // webapis-media-query.js file alongside the zone.js file.  Additionally, some browsers do not
                // have MediaQueryList inherit from EventTarget, which causes inconsistencies in how Zone.js
                // patches it.
                const handler = (e) => this._zone.run(() => observer.next(e));
                mql.addListener(handler);
                return () => {
                    mql.removeListener(handler);
                };
            }).pipe(startWith(mql), map((nextMql) => ({ query, matches: nextMql.matches })), takeUntil(this._destroySubject));
            // Add the MediaQueryList to the set of queries.
            const output = { observable: queryObservable, mql };
            this._queries.set(query, output);
            return output;
        }
    }
    BreakpointObserver.ɵprov = i0.ɵɵdefineInjectable({ factory: function BreakpointObserver_Factory() { return new BreakpointObserver(i0.ɵɵinject(i1.MediaMatcher), i0.ɵɵinject(i0.NgZone)); }, token: BreakpointObserver, providedIn: "root" });
    BreakpointObserver.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    BreakpointObserver.ctorParameters = () => [
        { type: MediaMatcher },
        { type: NgZone }
    ];
    return BreakpointObserver;
})();
export { BreakpointObserver };
/**
 * Split each query string into separate query strings if two queries are provided as comma
 * separated.
 */
function splitQueries(queries) {
    return queries.map((query) => query.split(','))
        .reduce((a1, a2) => a1.concat(a2))
        .map(query => query.trim());
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYnJlYWtwb2ludHMtb2JzZXJ2ZXIuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL2xheW91dC9icmVha3BvaW50cy1vYnNlcnZlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsVUFBVSxFQUFFLE1BQU0sRUFBWSxNQUFNLGVBQWUsQ0FBQztBQUM1RCxPQUFPLEVBQUMsWUFBWSxFQUFDLE1BQU0saUJBQWlCLENBQUM7QUFDN0MsT0FBTyxFQUFDLGFBQWEsRUFBRSxNQUFNLEVBQUUsVUFBVSxFQUFFLE9BQU8sRUFBVyxNQUFNLE1BQU0sQ0FBQztBQUMxRSxPQUFPLEVBQUMsWUFBWSxFQUFFLEdBQUcsRUFBRSxJQUFJLEVBQUUsU0FBUyxFQUFFLElBQUksRUFBRSxTQUFTLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQUNuRixPQUFPLEVBQUMsV0FBVyxFQUFDLE1BQU0sdUJBQXVCLENBQUM7OztBQTZCbEQsaUVBQWlFO0FBQ2pFO0lBQUEsTUFDYSxrQkFBa0I7UUFNN0IsWUFBb0IsYUFBMkIsRUFBVSxLQUFhO1lBQWxELGtCQUFhLEdBQWIsYUFBYSxDQUFjO1lBQVUsVUFBSyxHQUFMLEtBQUssQ0FBUTtZQUx0RSxnRUFBZ0U7WUFDeEQsYUFBUSxHQUFHLElBQUksR0FBRyxFQUFpQixDQUFDO1lBQzVDLGlFQUFpRTtZQUN6RCxvQkFBZSxHQUFHLElBQUksT0FBTyxFQUFRLENBQUM7UUFFMkIsQ0FBQztRQUUxRSxxRkFBcUY7UUFDckYsV0FBVztZQUNULElBQUksQ0FBQyxlQUFlLENBQUMsSUFBSSxFQUFFLENBQUM7WUFDNUIsSUFBSSxDQUFDLGVBQWUsQ0FBQyxRQUFRLEVBQUUsQ0FBQztRQUNsQyxDQUFDO1FBRUQ7Ozs7V0FJRztRQUNILFNBQVMsQ0FBQyxLQUF3QjtZQUNoQyxNQUFNLE9BQU8sR0FBRyxZQUFZLENBQUMsV0FBVyxDQUFDLEtBQUssQ0FB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QUNoRCxNQUFNLE1BQU0sR0FBRyxFQUFDLFVBQVUsRUFBRSxlQUFlLEVBQUUsR0FBRyxFQUFDLENBQUM7WUFDbEQsSUFBSSxDQUFDLFFBQVEsQ0FBQyxHQUFHLENBQUMsS0FBSyxFQUFFLE1BQU0sQ0FBQyxDQUFDO1lBQ2pDLE9BQU8sTUFBTSxDQUFDO1FBQ2hCLENBQUM7Ozs7Z0JBckZGLFVBQVUsU0FBQyxFQUFDLFVBQVUsRUFBRSxNQUFNLEVBQUM7OztnQkFqQ3hCLFlBQVk7Z0JBREEsTUFBTTs7NkJBUjFCO0tBZ0lDO1NBckZZLGtCQUFrQjtBQXVGL0I7OztHQUdHO0FBQ0gsU0FBUyxZQUFZLENBQUMsT0FBaUI7SUFDckMsT0FBTyxPQUFPLENBQUMsR0FBRyxDQUFDLENBQUMsS0FBYSxFQUFFLEVBQUUsQ0FBQyxLQUFLLENBQUMsS0FBSyxDQUFDLEdBQUcsQ0FBQyxDQUFDO1NBQ3hDLE1BQU0sQ0FBQyxDQUFDLEVBQVksRUFBRSxFQUFZLEVBQUUsRUFBRSxDQUFDLEVBQUUsQ0FBQyxNQUFNLENBQUMsRUFBRSxDQUFDLENBQUM7U0FDckQsR0FBRyxDQUFDLEtBQUssQ0FBQyxFQUFFLENBQUMsS0FBSyxDQUFDLElBQUksRUFBRSxDQUFDLENBQUM7QUFDNUMsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0luamVjdGFibGUsIE5nWm9uZSwgT25EZXN0cm95fSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7TWVkaWFNYXRjaGVyfSBmcm9tICcuL21lZGlhLW1hdGNoZXInO1xuaW1wb3J0IHtjb21iaW5lTGF0ZXN0LCBjb25jYXQsIE9ic2VydmFibGUsIFN1YmplY3QsIE9ic2VydmVyfSBmcm9tICdyeGpzJztcbmltcG9ydCB7ZGVib3VuY2VUaW1lLCBtYXAsIHNraXAsIHN0YXJ0V2l0aCwgdGFrZSwgdGFrZVVudGlsfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5pbXBvcnQge2NvZXJjZUFycmF5fSBmcm9tICdAYW5ndWxhci9jZGsvY29lcmNpb24nO1xuXG5cbi8qKiBUaGUgY3VycmVudCBzdGF0ZSBvZiBhIGxheW91dCBicmVha3BvaW50LiAqL1xuZXhwb3J0IGludGVyZmFjZSBCcmVha3BvaW50U3RhdGUge1xuICAvKiogV2hldGhlciB0aGUgYnJlYWtwb2ludCBpcyBjdXJyZW50bHkgbWF0Y2hpbmcuICovXG4gIG1hdGNoZXM6IGJvb2xlYW47XG4gIC8qKlxuICAgKiBBIGtleSBib29sZWFuIHBhaXIgZm9yIGVhY2ggcXVlcnkgcHJvdmlkZWQgdG8gdGhlIG9ic2VydmUgbWV0aG9kLFxuICAgKiB3aXRoIGl0cyBjdXJyZW50IG1hdGNoZWQgc3RhdGUuXG4gICAqL1xuICBicmVha3BvaW50czoge1xuICAgIFtrZXk6IHN0cmluZ106IGJvb2xlYW47XG4gIH07XG59XG5cbi8qKiBUaGUgY3VycmVudCBzdGF0ZSBvZiBhIGxheW91dCBicmVha3BvaW50LiAqL1xuaW50ZXJmYWNlIEludGVybmFsQnJlYWtwb2ludFN0YXRlIHtcbiAgLyoqIFdoZXRoZXIgdGhlIGJyZWFrcG9pbnQgaXMgY3VycmVudGx5IG1hdGNoaW5nLiAqL1xuICBtYXRjaGVzOiBib29sZWFuO1xuICAvKiogVGhlIG1lZGlhIHF1ZXJ5IGJlaW5nIHRvIGJlIG1hdGNoZWQgKi9cbiAgcXVlcnk6IHN0cmluZztcbn1cblxuaW50ZXJmYWNlIFF1ZXJ5IHtcbiAgb2JzZXJ2YWJsZTogT2JzZXJ2YWJsZTxJbnRlcm5hbEJyZWFrcG9pbnRTdGF0ZT47XG4gIG1xbDogTWVkaWFRdWVyeUxpc3Q7XG59XG5cbi8qKiBVdGlsaXR5IGZvciBjaGVja2luZyB0aGUgbWF0Y2hpbmcgc3RhdGUgb2YgQG1lZGlhIHF1ZXJpZXMuICovXG5ASW5qZWN0YWJsZSh7cHJvdmlkZWRJbjogJ3Jvb3QnfSlcbmV4cG9ydCBjbGFzcyBCcmVha3BvaW50T2JzZXJ2ZXIgaW1wbGVtZW50cyBPbkRlc3Ryb3kge1xuICAvKiogIEEgbWFwIG9mIGFsbCBtZWRpYSBxdWVyaWVzIGN1cnJlbnRseSBiZWluZyBsaXN0ZW5lZCBmb3IuICovXG4gIHByaXZhdGUgX3F1ZXJpZXMgPSBuZXcgTWFwPHN0cmluZywgUXVlcnk+KCk7XG4gIC8qKiBBIHN1YmplY3QgZm9yIGFsbCBvdGhlciBvYnNlcnZhYmxlcyB0byB0YWtlVW50aWwgYmFzZWQgb24uICovXG4gIHByaXZhdGUgX2Rlc3Ryb3lTdWJqZWN0ID0gbmV3IFN1YmplY3Q8dm9pZD4oKTtcblxuICBjb25zdHJ1Y3Rvcihwcml2YXRlIF9tZWRpYU1hdGNoZXI6IE1lZGlhTWF0Y2hlciwgcHJpdmF0ZSBfem9uZTogTmdab25lKSB7fVxuXG4gIC8qKiBDb21wbGV0ZXMgdGhlIGFjdGl2ZSBzdWJqZWN0LCBzaWduYWxsaW5nIHRvIGFsbCBvdGhlciBvYnNlcnZhYmxlcyB0byBjb21wbGV0ZS4gKi9cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgdGhpcy5fZGVzdHJveVN1YmplY3QubmV4dCgpO1xuICAgIHRoaXMuX2Rlc3Ryb3lTdWJqZWN0LmNvbXBsZXRlKCk7XG4gIH1cblxuICAvKipcbiAgICogV2hldGhlciBvbmUgb3IgbW9yZSBtZWRpYSBxdWVyaWVzIG1hdGNoIHRoZSBjdXJyZW50IHZpZXdwb3J0IHNpemUuXG4gICAqIEBwYXJhbSB2YWx1ZSBPbmUgb3IgbW9yZSBtZWRpYSBxdWVyaWVzIHRvIGNoZWNrLlxuICAgKiBAcmV0dXJucyBXaGV0aGVyIGFueSBvZiB0aGUgbWVkaWEgcXVlcmllcyBtYXRjaC5cbiAgICovXG4gIGlzTWF0Y2hlZCh2YWx1ZTogc3RyaW5nIHwgc3RyaW5nW10pOiBib29sZWFuIHtcbiAgICBjb25zdCBxdWVyaWVzID0gc3BsaXRRdWVyaWVzKGNvZXJjZUFycmF5KHZhbHVlKSk7XG4gICAgcmV0dXJuIHF1ZXJpZXMuc29tZShtZWRpYVF1ZXJ5ID0+IHRoaXMuX3JlZ2lzdGVyUXVlcnkobWVkaWFRdWVyeSkubXFsLm1hdGNoZXMpO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgYW4gb2JzZXJ2YWJsZSBvZiByZXN1bHRzIGZvciB0aGUgZ2l2ZW4gcXVlcmllcyB0aGF0IHdpbGwgZW1pdCBuZXcgcmVzdWx0cyBmb3IgYW55IGNoYW5nZXNcbiAgICogaW4gbWF0Y2hpbmcgb2YgdGhlIGdpdmVuIHF1ZXJpZXMuXG4gICAqIEBwYXJhbSB2YWx1ZSBPbmUgb3IgbW9yZSBtZWRpYSBxdWVyaWVzIHRvIGNoZWNrLlxuICAgKiBAcmV0dXJucyBBIHN0cmVhbSBvZiBtYXRjaGVzIGZvciB0aGUgZ2l2ZW4gcXVlcmllcy5cbiAgICovXG4gIG9ic2VydmUodmFsdWU6IHN0cmluZyB8IHN0cmluZ1tdKTogT2JzZXJ2YWJsZTxCcmVha3BvaW50U3RhdGU+IHtcbiAgICBjb25zdCBxdWVyaWVzID0gc3BsaXRRdWVyaWVzKGNvZXJjZUFycmF5KHZhbHVlKSk7XG4gICAgY29uc3Qgb2JzZXJ2YWJsZXMgPSBxdWVyaWVzLm1hcChxdWVyeSA9PiB0aGlzLl9yZWdpc3RlclF1ZXJ5KHF1ZXJ5KS5vYnNlcnZhYmxlKTtcblxuICAgIGxldCBzdGF0ZU9ic2VydmFibGUgPSBjb21iaW5lTGF0ZXN0KG9ic2VydmFibGVzKTtcbiAgICAvLyBFbWl0IHRoZSBmaXJzdCBzdGF0ZSBpbW1lZGlhdGVseSwgYW5kIHRoZW4gZGVib3VuY2UgdGhlIHN1YnNlcXVlbnQgZW1pc3Npb25zLlxuICAgIHN0YXRlT2JzZXJ2YWJsZSA9IGNvbmNhdChcbiAgICAgIHN0YXRlT2JzZXJ2YWJsZS5waXBlKHRha2UoMSkpLFxuICAgICAgc3RhdGVPYnNlcnZhYmxlLnBpcGUoc2tpcCgxKSwgZGVib3VuY2VUaW1lKDApKSk7XG4gICAgcmV0dXJuIHN0YXRlT2JzZXJ2YWJsZS5waXBlKG1hcCgoYnJlYWtwb2ludFN0YXRlczogSW50ZXJuYWxCcmVha3BvaW50U3RhdGVbXSkgPT4ge1xuICAgICAgY29uc3QgcmVzcG9uc2U6IEJyZWFrcG9pbnRTdGF0ZSA9IHtcbiAgICAgICAgbWF0Y2hlczogZmFsc2UsXG4gICAgICAgIGJyZWFrcG9pbnRzOiB7fSxcbiAgICAgIH07XG4gICAgICBicmVha3BvaW50U3RhdGVzLmZvckVhY2goKHN0YXRlOiBJbnRlcm5hbEJyZWFrcG9pbnRTdGF0ZSkgPT4ge1xuICAgICAgICByZXNwb25zZS5tYXRjaGVzID0gcmVzcG9uc2UubWF0Y2hlcyB8fCBzdGF0ZS5tYXRjaGVzO1xuICAgICAgICByZXNwb25zZS5icmVha3BvaW50c1tzdGF0ZS5xdWVyeV0gPSBzdGF0ZS5tYXRjaGVzO1xuICAgICAgfSk7XG4gICAgICByZXR1cm4gcmVzcG9uc2U7XG4gICAgfSkpO1xuICB9XG5cbiAgLyoqIFJlZ2lzdGVycyBhIHNwZWNpZmljIHF1ZXJ5IHRvIGJlIGxpc3RlbmVkIGZvci4gKi9cbiAgcHJpdmF0ZSBfcmVnaXN0ZXJRdWVyeShxdWVyeTogc3RyaW5nKTogUXVlcnkge1xuICAgIC8vIE9ubHkgc2V0IHVwIGEgbmV3IE1lZGlhUXVlcnlMaXN0IGlmIGl0IGlzIG5vdCBhbHJlYWR5IGJlaW5nIGxpc3RlbmVkIGZvci5cbiAgICBpZiAodGhpcy5fcXVlcmllcy5oYXMocXVlcnkpKSB7XG4gICAgICByZXR1cm4gdGhpcy5fcXVlcmllcy5nZXQocXVlcnkpITtcbiAgICB9XG5cbiAgICBjb25zdCBtcWw6IE1lZGlhUXVlcnlMaXN0ID0gdGhpcy5fbWVkaWFNYXRjaGVyLm1hdGNoTWVkaWEocXVlcnkpO1xuXG4gICAgLy8gQ3JlYXRlIGNhbGxiYWNrIGZvciBtYXRjaCBjaGFuZ2VzIGFuZCBhZGQgaXQgaXMgYXMgYSBsaXN0ZW5lci5cbiAgICBjb25zdCBxdWVyeU9ic2VydmFibGUgPSBuZXcgT2JzZXJ2YWJsZTxNZWRpYVF1ZXJ5TGlzdD4oKG9ic2VydmVyOiBPYnNlcnZlcjxNZWRpYVF1ZXJ5TGlzdD4pID0+IHtcbiAgICAgIC8vIExpc3RlbmVyIGNhbGxiYWNrIG1ldGhvZHMgYXJlIHdyYXBwZWQgdG8gYmUgcGxhY2VkIGJhY2sgaW4gbmdab25lLiBDYWxsYmFja3MgbXVzdCBiZSBwbGFjZWRcbiAgICAgIC8vIGJhY2sgaW50byB0aGUgem9uZSBiZWNhdXNlIG1hdGNoTWVkaWEgaXMgb25seSBpbmNsdWRlZCBpbiBab25lLmpzIGJ5IGxvYWRpbmcgdGhlXG4gICAgICAvLyB3ZWJhcGlzLW1lZGlhLXF1ZXJ5LmpzIGZpbGUgYWxvbmdzaWRlIHRoZSB6b25lLmpzIGZpbGUuICBBZGRpdGlvbmFsbHksIHNvbWUgYnJvd3NlcnMgZG8gbm90XG4gICAgICAvLyBoYXZlIE1lZGlhUXVlcnlMaXN0IGluaGVyaXQgZnJvbSBFdmVudFRhcmdldCwgd2hpY2ggY2F1c2VzIGluY29uc2lzdGVuY2llcyBpbiBob3cgWm9uZS5qc1xuICAgICAgLy8gcGF0Y2hlcyBpdC5cbiAgICAgIGNvbnN0IGhhbmRsZXIgPSAoZTogYW55KSA9PiB0aGlzLl96b25lLnJ1bigoKSA9PiBvYnNlcnZlci5uZXh0KGUpKTtcbiAgICAgIG1xbC5hZGRMaXN0ZW5lcihoYW5kbGVyKTtcblxuICAgICAgcmV0dXJuICgpID0+IHtcbiAgICAgICAgbXFsLnJlbW92ZUxpc3RlbmVyKGhhbmRsZXIpO1xuICAgICAgfTtcbiAgICB9KS5waXBlKFxuICAgICAgc3RhcnRXaXRoKG1xbCksXG4gICAgICBtYXAoKG5leHRNcWw6IE1lZGlhUXVlcnlMaXN0KSA9PiAoe3F1ZXJ5LCBtYXRjaGVzOiBuZXh0TXFsLm1hdGNoZXN9KSksXG4gICAgICB0YWtlVW50aWwodGhpcy5fZGVzdHJveVN1YmplY3QpXG4gICAgKTtcblxuICAgIC8vIEFkZCB0aGUgTWVkaWFRdWVyeUxpc3QgdG8gdGhlIHNldCBvZiBxdWVyaWVzLlxuICAgIGNvbnN0IG91dHB1dCA9IHtvYnNlcnZhYmxlOiBxdWVyeU9ic2VydmFibGUsIG1xbH07XG4gICAgdGhpcy5fcXVlcmllcy5zZXQocXVlcnksIG91dHB1dCk7XG4gICAgcmV0dXJuIG91dHB1dDtcbiAgfVxufVxuXG4vKipcbiAqIFNwbGl0IGVhY2ggcXVlcnkgc3RyaW5nIGludG8gc2VwYXJhdGUgcXVlcnkgc3RyaW5ncyBpZiB0d28gcXVlcmllcyBhcmUgcHJvdmlkZWQgYXMgY29tbWFcbiAqIHNlcGFyYXRlZC5cbiAqL1xuZnVuY3Rpb24gc3BsaXRRdWVyaWVzKHF1ZXJpZXM6IHN0cmluZ1tdKTogc3RyaW5nW10ge1xuICByZXR1cm4gcXVlcmllcy5tYXAoKHF1ZXJ5OiBzdHJpbmcpID0+IHF1ZXJ5LnNwbGl0KCcsJykpXG4gICAgICAgICAgICAgICAgLnJlZHVjZSgoYTE6IHN0cmluZ1tdLCBhMjogc3RyaW5nW10pID0+IGExLmNvbmNhdChhMikpXG4gICAgICAgICAgICAgICAgLm1hcChxdWVyeSA9PiBxdWVyeS50cmltKCkpO1xufVxuIl19