/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** Mixin to augment a directive with a `color` property. */
export function mixinColor(base, defaultColor) {
    return class extends base {
        constructor(...args) {
            super(...args);
            // Set the default color that can be specified from the mixin.
            this.color = defaultColor;
        }
        get color() { return this._color; }
        set color(value) {
            const colorPalette = value || defaultColor;
            if (colorPalette !== this._color) {
                if (this._color) {
                    this._elementRef.nativeElement.classList.remove(`mat-${this._color}`);
                }
                if (colorPalette) {
                    this._elementRef.nativeElement.classList.add(`mat-${colorPalette}`);
                }
                this._color = colorPalette;
            }
        }
    };
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY29sb3IuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvY29yZS9jb21tb24tYmVoYXZpb3JzL2NvbG9yLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQXNCSCw0REFBNEQ7QUFDNUQsTUFBTSxVQUFVLFVBQVUsQ0FDdEIsSUFBTyxFQUFFLFlBQTJCO0lBQ3RDLE9BQU8sS0FBTSxTQUFRLElBQUk7UUFtQnZCLFlBQVksR0FBRyxJQUFXO1lBQ3hCLEtBQUssQ0FBQyxHQUFHLElBQUksQ0FBQyxDQUFDO1lBRWYsOERBQThEO1lBQzlELElBQUksQ0FBQyxLQUFLLEdBQUcsWUFBWSxDQUFDO1FBQzVCLENBQUM7UUFyQkQsSUFBSSxLQUFLLEtBQW1CLE9BQU8sSUFBSSxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUM7UUFDakQsSUFBSSxLQUFLLENBQUMsS0FBbUI7WUFDM0IsTUFBTSxZQUFZLEdBQUcsS0FBSyxJQUFJLFlBQVksQ0FBQztZQUUzQyxJQUFJLFlBQVksS0FBSyxJQUFJLENBQUMsTUFBTSxFQUFFO2dCQUNoQyxJQUFJLElBQUksQ0FBQyxNQUFNLEVBQUU7b0JBQ2YsSUFBSSxDQUFDLFdBQVcsQ0FBQyxhQUFhLENBQUMsU0FBUyxDQUFDLE1BQU0sQ0FBQyxPQUFPLElBQUksQ0FBQyxNQUFNLEVBQUUsQ0FBQyxDQUFDO2lCQUN2RTtnQkFDRCxJQUFJLFlBQVksRUFBRTtvQkFDaEIsSUFBSSxDQUFDLFdBQVcsQ0FBQyxhQUFhLENBQUMsU0FBUyxDQUFDLEdBQUcsQ0FBQyxPQUFPLFlBQVksRUFBRSxDQUFDLENBQUM7aUJBQ3JFO2dCQUVELElBQUksQ0FBQyxNQUFNLEdBQUcsWUFBWSxDQUFDO2FBQzVCO1FBQ0gsQ0FBQztLQVFGLENBQUM7QUFDSixDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Q29uc3RydWN0b3J9IGZyb20gJy4vY29uc3RydWN0b3InO1xuaW1wb3J0IHtFbGVtZW50UmVmfSBmcm9tICdAYW5ndWxhci9jb3JlJztcblxuLyoqIEBkb2NzLXByaXZhdGUgKi9cbmV4cG9ydCBpbnRlcmZhY2UgQ2FuQ29sb3Ige1xuICAvKiogVGhlbWUgY29sb3IgcGFsZXR0ZSBmb3IgdGhlIGNvbXBvbmVudC4gKi9cbiAgY29sb3I6IFRoZW1lUGFsZXR0ZTtcbn1cblxuLyoqIEBkb2NzLXByaXZhdGUgKi9cbmV4cG9ydCB0eXBlIENhbkNvbG9yQ3RvciA9IENvbnN0cnVjdG9yPENhbkNvbG9yPjtcblxuLyoqIEBkb2NzLXByaXZhdGUgKi9cbmV4cG9ydCBpbnRlcmZhY2UgSGFzRWxlbWVudFJlZiB7XG4gIF9lbGVtZW50UmVmOiBFbGVtZW50UmVmO1xufVxuXG4vKiogUG9zc2libGUgY29sb3IgcGFsZXR0ZSB2YWx1ZXMuICovXG5leHBvcnQgdHlwZSBUaGVtZVBhbGV0dGUgPSAncHJpbWFyeScgfCAnYWNjZW50JyB8ICd3YXJuJyB8IHVuZGVmaW5lZDtcblxuLyoqIE1peGluIHRvIGF1Z21lbnQgYSBkaXJlY3RpdmUgd2l0aCBhIGBjb2xvcmAgcHJvcGVydHkuICovXG5leHBvcnQgZnVuY3Rpb24gbWl4aW5Db2xvcjxUIGV4dGVuZHMgQ29uc3RydWN0b3I8SGFzRWxlbWVudFJlZj4+KFxuICAgIGJhc2U6IFQsIGRlZmF1bHRDb2xvcj86IFRoZW1lUGFsZXR0ZSk6IENhbkNvbG9yQ3RvciAmIFQge1xuICByZXR1cm4gY2xhc3MgZXh0ZW5kcyBiYXNlIHtcbiAgICBwcml2YXRlIF9jb2xvcjogVGhlbWVQYWxldHRlO1xuXG4gICAgZ2V0IGNvbG9yKCk6IFRoZW1lUGFsZXR0ZSB7IHJldHVybiB0aGlzLl9jb2xvcjsgfVxuICAgIHNldCBjb2xvcih2YWx1ZTogVGhlbWVQYWxldHRlKSB7XG4gICAgICBjb25zdCBjb2xvclBhbGV0dGUgPSB2YWx1ZSB8fCBkZWZhdWx0Q29sb3I7XG5cbiAgICAgIGlmIChjb2xvclBhbGV0dGUgIT09IHRoaXMuX2NvbG9yKSB7XG4gICAgICAgIGlmICh0aGlzLl9jb2xvcikge1xuICAgICAgICAgIHRoaXMuX2VsZW1lbnRSZWYubmF0aXZlRWxlbWVudC5jbGFzc0xpc3QucmVtb3ZlKGBtYXQtJHt0aGlzLl9jb2xvcn1gKTtcbiAgICAgICAgfVxuICAgICAgICBpZiAoY29sb3JQYWxldHRlKSB7XG4gICAgICAgICAgdGhpcy5fZWxlbWVudFJlZi5uYXRpdmVFbGVtZW50LmNsYXNzTGlzdC5hZGQoYG1hdC0ke2NvbG9yUGFsZXR0ZX1gKTtcbiAgICAgICAgfVxuXG4gICAgICAgIHRoaXMuX2NvbG9yID0gY29sb3JQYWxldHRlO1xuICAgICAgfVxuICAgIH1cblxuICAgIGNvbnN0cnVjdG9yKC4uLmFyZ3M6IGFueVtdKSB7XG4gICAgICBzdXBlciguLi5hcmdzKTtcblxuICAgICAgLy8gU2V0IHRoZSBkZWZhdWx0IGNvbG9yIHRoYXQgY2FuIGJlIHNwZWNpZmllZCBmcm9tIHRoZSBtaXhpbi5cbiAgICAgIHRoaXMuY29sb3IgPSBkZWZhdWx0Q29sb3I7XG4gICAgfVxuICB9O1xufVxuXG4iXX0=