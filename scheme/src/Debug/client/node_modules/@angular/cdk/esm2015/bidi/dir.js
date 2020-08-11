/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Directive, Output, Input, EventEmitter, } from '@angular/core';
import { Directionality } from './directionality';
/**
 * Directive to listen for changes of direction of part of the DOM.
 *
 * Provides itself as Directionality such that descendant directives only need to ever inject
 * Directionality to get the closest direction.
 */
let Dir = /** @class */ (() => {
    class Dir {
        constructor() {
            /** Normalized direction that accounts for invalid/unsupported values. */
            this._dir = 'ltr';
            /** Whether the `value` has been set to its initial value. */
            this._isInitialized = false;
            /** Event emitted when the direction changes. */
            this.change = new EventEmitter();
        }
        /** @docs-private */
        get dir() { return this._dir; }
        set dir(value) {
            const old = this._dir;
            const normalizedValue = value ? value.toLowerCase() : value;
            this._rawDir = value;
            this._dir = (normalizedValue === 'ltr' || normalizedValue === 'rtl') ? normalizedValue : 'ltr';
            if (old !== this._dir && this._isInitialized) {
                this.change.emit(this._dir);
            }
        }
        /** Current layout direction of the element. */
        get value() { return this.dir; }
        /** Initialize once default value has been set. */
        ngAfterContentInit() {
            this._isInitialized = true;
        }
        ngOnDestroy() {
            this.change.complete();
        }
    }
    Dir.decorators = [
        { type: Directive, args: [{
                    selector: '[dir]',
                    providers: [{ provide: Directionality, useExisting: Dir }],
                    host: { '[attr.dir]': '_rawDir' },
                    exportAs: 'dir',
                },] }
    ];
    Dir.propDecorators = {
        change: [{ type: Output, args: ['dirChange',] }],
        dir: [{ type: Input }]
    };
    return Dir;
})();
export { Dir };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGlyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9iaWRpL2Rpci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQ0wsU0FBUyxFQUNULE1BQU0sRUFDTixLQUFLLEVBQ0wsWUFBWSxHQUdiLE1BQU0sZUFBZSxDQUFDO0FBRXZCLE9BQU8sRUFBWSxjQUFjLEVBQUMsTUFBTSxrQkFBa0IsQ0FBQztBQUUzRDs7Ozs7R0FLRztBQUNIO0lBQUEsTUFNYSxHQUFHO1FBTmhCO1lBT0UseUVBQXlFO1lBQ2pFLFNBQUksR0FBYyxLQUFLLENBQUM7WUFFaEMsNkRBQTZEO1lBQ3JELG1CQUFjLEdBQVksS0FBSyxDQUFDO1lBS3hDLGdEQUFnRDtZQUMzQixXQUFNLEdBQUcsSUFBSSxZQUFZLEVBQWEsQ0FBQztRQTRCOUQsQ0FBQztRQTFCQyxvQkFBb0I7UUFDcEIsSUFDSSxHQUFHLEtBQWdCLE9BQU8sSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUM7UUFDMUMsSUFBSSxHQUFHLENBQUMsS0FBZ0I7WUFDdEIsTUFBTSxHQUFHLEdBQUcsSUFBSSxDQUFDLElBQUksQ0FBQztZQUN0QixNQUFNLGVBQWUsR0FBRyxLQUFLLENBQUMsQ0FBQyxDQUFDLEtBQUssQ0FBQyxXQUFXLEVBQUUsQ0FBQyxDQUFDLENBQUMsS0FBSyxDQUFDO1lBRTVELElBQUksQ0FBQyxPQUFPLEdBQUcsS0FBSyxDQUFDO1lBQ3JCLElBQUksQ0FBQyxJQUFJLEdBQUcsQ0FBQyxlQUFlLEtBQUssS0FBSyxJQUFJLGVBQWUsS0FBSyxLQUFLLENBQUMsQ0FBQyxDQUFDLENBQUMsZUFBZSxDQUFDLENBQUMsQ0FBQyxLQUFLLENBQUM7WUFFL0YsSUFBSSxHQUFHLEtBQUssSUFBSSxDQUFDLElBQUksSUFBSSxJQUFJLENBQUMsY0FBYyxFQUFFO2dCQUM1QyxJQUFJLENBQUMsTUFBTSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUM7YUFDN0I7UUFDSCxDQUFDO1FBRUQsK0NBQStDO1FBQy9DLElBQUksS0FBSyxLQUFnQixPQUFPLElBQUksQ0FBQyxHQUFHLENBQUMsQ0FBQyxDQUFDO1FBRTNDLGtEQUFrRDtRQUNsRCxrQkFBa0I7WUFDaEIsSUFBSSxDQUFDLGNBQWMsR0FBRyxJQUFJLENBQUM7UUFDN0IsQ0FBQztRQUVELFdBQVc7WUFDVCxJQUFJLENBQUMsTUFBTSxDQUFDLFFBQVEsRUFBRSxDQUFDO1FBQ3pCLENBQUM7OztnQkE1Q0YsU0FBUyxTQUFDO29CQUNULFFBQVEsRUFBRSxPQUFPO29CQUNqQixTQUFTLEVBQUUsQ0FBQyxFQUFDLE9BQU8sRUFBRSxjQUFjLEVBQUUsV0FBVyxFQUFFLEdBQUcsRUFBQyxDQUFDO29CQUN4RCxJQUFJLEVBQUUsRUFBQyxZQUFZLEVBQUUsU0FBUyxFQUFDO29CQUMvQixRQUFRLEVBQUUsS0FBSztpQkFDaEI7Ozt5QkFZRSxNQUFNLFNBQUMsV0FBVztzQkFHbEIsS0FBSzs7SUF5QlIsVUFBQztLQUFBO1NBdkNZLEdBQUciLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtcbiAgRGlyZWN0aXZlLFxuICBPdXRwdXQsXG4gIElucHV0LFxuICBFdmVudEVtaXR0ZXIsXG4gIEFmdGVyQ29udGVudEluaXQsXG4gIE9uRGVzdHJveSxcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5cbmltcG9ydCB7RGlyZWN0aW9uLCBEaXJlY3Rpb25hbGl0eX0gZnJvbSAnLi9kaXJlY3Rpb25hbGl0eSc7XG5cbi8qKlxuICogRGlyZWN0aXZlIHRvIGxpc3RlbiBmb3IgY2hhbmdlcyBvZiBkaXJlY3Rpb24gb2YgcGFydCBvZiB0aGUgRE9NLlxuICpcbiAqIFByb3ZpZGVzIGl0c2VsZiBhcyBEaXJlY3Rpb25hbGl0eSBzdWNoIHRoYXQgZGVzY2VuZGFudCBkaXJlY3RpdmVzIG9ubHkgbmVlZCB0byBldmVyIGluamVjdFxuICogRGlyZWN0aW9uYWxpdHkgdG8gZ2V0IHRoZSBjbG9zZXN0IGRpcmVjdGlvbi5cbiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnW2Rpcl0nLFxuICBwcm92aWRlcnM6IFt7cHJvdmlkZTogRGlyZWN0aW9uYWxpdHksIHVzZUV4aXN0aW5nOiBEaXJ9XSxcbiAgaG9zdDogeydbYXR0ci5kaXJdJzogJ19yYXdEaXInfSxcbiAgZXhwb3J0QXM6ICdkaXInLFxufSlcbmV4cG9ydCBjbGFzcyBEaXIgaW1wbGVtZW50cyBEaXJlY3Rpb25hbGl0eSwgQWZ0ZXJDb250ZW50SW5pdCwgT25EZXN0cm95IHtcbiAgLyoqIE5vcm1hbGl6ZWQgZGlyZWN0aW9uIHRoYXQgYWNjb3VudHMgZm9yIGludmFsaWQvdW5zdXBwb3J0ZWQgdmFsdWVzLiAqL1xuICBwcml2YXRlIF9kaXI6IERpcmVjdGlvbiA9ICdsdHInO1xuXG4gIC8qKiBXaGV0aGVyIHRoZSBgdmFsdWVgIGhhcyBiZWVuIHNldCB0byBpdHMgaW5pdGlhbCB2YWx1ZS4gKi9cbiAgcHJpdmF0ZSBfaXNJbml0aWFsaXplZDogYm9vbGVhbiA9IGZhbHNlO1xuXG4gIC8qKiBEaXJlY3Rpb24gYXMgcGFzc2VkIGluIGJ5IHRoZSBjb25zdW1lci4gKi9cbiAgX3Jhd0Rpcjogc3RyaW5nO1xuXG4gIC8qKiBFdmVudCBlbWl0dGVkIHdoZW4gdGhlIGRpcmVjdGlvbiBjaGFuZ2VzLiAqL1xuICBAT3V0cHV0KCdkaXJDaGFuZ2UnKSBjaGFuZ2UgPSBuZXcgRXZlbnRFbWl0dGVyPERpcmVjdGlvbj4oKTtcblxuICAvKiogQGRvY3MtcHJpdmF0ZSAqL1xuICBASW5wdXQoKVxuICBnZXQgZGlyKCk6IERpcmVjdGlvbiB7IHJldHVybiB0aGlzLl9kaXI7IH1cbiAgc2V0IGRpcih2YWx1ZTogRGlyZWN0aW9uKSB7XG4gICAgY29uc3Qgb2xkID0gdGhpcy5fZGlyO1xuICAgIGNvbnN0IG5vcm1hbGl6ZWRWYWx1ZSA9IHZhbHVlID8gdmFsdWUudG9Mb3dlckNhc2UoKSA6IHZhbHVlO1xuXG4gICAgdGhpcy5fcmF3RGlyID0gdmFsdWU7XG4gICAgdGhpcy5fZGlyID0gKG5vcm1hbGl6ZWRWYWx1ZSA9PT0gJ2x0cicgfHwgbm9ybWFsaXplZFZhbHVlID09PSAncnRsJykgPyBub3JtYWxpemVkVmFsdWUgOiAnbHRyJztcblxuICAgIGlmIChvbGQgIT09IHRoaXMuX2RpciAmJiB0aGlzLl9pc0luaXRpYWxpemVkKSB7XG4gICAgICB0aGlzLmNoYW5nZS5lbWl0KHRoaXMuX2Rpcik7XG4gICAgfVxuICB9XG5cbiAgLyoqIEN1cnJlbnQgbGF5b3V0IGRpcmVjdGlvbiBvZiB0aGUgZWxlbWVudC4gKi9cbiAgZ2V0IHZhbHVlKCk6IERpcmVjdGlvbiB7IHJldHVybiB0aGlzLmRpcjsgfVxuXG4gIC8qKiBJbml0aWFsaXplIG9uY2UgZGVmYXVsdCB2YWx1ZSBoYXMgYmVlbiBzZXQuICovXG4gIG5nQWZ0ZXJDb250ZW50SW5pdCgpIHtcbiAgICB0aGlzLl9pc0luaXRpYWxpemVkID0gdHJ1ZTtcbiAgfVxuXG4gIG5nT25EZXN0cm95KCkge1xuICAgIHRoaXMuY2hhbmdlLmNvbXBsZXRlKCk7XG4gIH1cbn1cblxuIl19