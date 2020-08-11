/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Injectable, NgModule } from '@angular/core';
import { MatIconRegistry } from '@angular/material/icon';
import { of as observableOf } from 'rxjs';
// tslint:enable:no-any
/**
 * A null icon registry that must be imported to allow disabling of custom
 * icons.
 */
let FakeMatIconRegistry = /** @class */ (() => {
    class FakeMatIconRegistry {
        addSvgIcon() {
            return this;
        }
        addSvgIconLiteral() {
            return this;
        }
        addSvgIconInNamespace() {
            return this;
        }
        addSvgIconLiteralInNamespace() {
            return this;
        }
        addSvgIconSet() {
            return this;
        }
        addSvgIconSetLiteral() {
            return this;
        }
        addSvgIconSetInNamespace() {
            return this;
        }
        addSvgIconSetLiteralInNamespace() {
            return this;
        }
        registerFontClassAlias() {
            return this;
        }
        classNameForFontAlias(alias) {
            return alias;
        }
        getDefaultFontSetClass() {
            return 'material-icons';
        }
        getSvgIconFromUrl() {
            return observableOf(this._generateEmptySvg());
        }
        getNamedSvgIcon() {
            return observableOf(this._generateEmptySvg());
        }
        setDefaultFontSetClass() {
            return this;
        }
        ngOnDestroy() { }
        _generateEmptySvg() {
            const emptySvg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
            emptySvg.classList.add('fake-testing-svg');
            // Emulate real icon characteristics from `MatIconRegistry` so size remains consistent in tests.
            emptySvg.setAttribute('fit', '');
            emptySvg.setAttribute('height', '100%');
            emptySvg.setAttribute('width', '100%');
            emptySvg.setAttribute('preserveAspectRatio', 'xMidYMid meet');
            emptySvg.setAttribute('focusable', 'false');
            return emptySvg;
        }
    }
    FakeMatIconRegistry.decorators = [
        { type: Injectable }
    ];
    return FakeMatIconRegistry;
})();
export { FakeMatIconRegistry };
/** Import this module in tests to install the null icon registry. */
let MatIconTestingModule = /** @class */ (() => {
    class MatIconTestingModule {
    }
    MatIconTestingModule.decorators = [
        { type: NgModule, args: [{
                    providers: [{ provide: MatIconRegistry, useClass: FakeMatIconRegistry }]
                },] }
    ];
    return MatIconTestingModule;
})();
export { MatIconTestingModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZmFrZS1pY29uLXJlZ2lzdHJ5LmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL2ljb24vdGVzdGluZy9mYWtlLWljb24tcmVnaXN0cnkudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFVBQVUsRUFBRSxRQUFRLEVBQVksTUFBTSxlQUFlLENBQUM7QUFDOUQsT0FBTyxFQUFDLGVBQWUsRUFBQyxNQUFNLHdCQUF3QixDQUFDO0FBQ3ZELE9BQU8sRUFBYSxFQUFFLElBQUksWUFBWSxFQUFDLE1BQU0sTUFBTSxDQUFDO0FBTXBELHVCQUF1QjtBQUV2Qjs7O0dBR0c7QUFDSDtJQUFBLE1BQ2EsbUJBQW1CO1FBQzlCLFVBQVU7WUFDUixPQUFPLElBQUksQ0FBQztRQUNkLENBQUM7UUFFRCxpQkFBaUI7WUFDZixPQUFPLElBQUksQ0FBQztRQUNkLENBQUM7UUFFRCxxQkFBcUI7WUFDbkIsT0FBTyxJQUFJLENBQUM7UUFDZCxDQUFDO1FBRUQsNEJBQTRCO1lBQzFCLE9BQU8sSUFBSSxDQUFDO1FBQ2QsQ0FBQztRQUVELGFBQWE7WUFDWCxPQUFPLElBQUksQ0FBQztRQUNkLENBQUM7UUFFRCxvQkFBb0I7WUFDbEIsT0FBTyxJQUFJLENBQUM7UUFDZCxDQUFDO1FBRUQsd0JBQXdCO1lBQ3RCLE9BQU8sSUFBSSxDQUFDO1FBQ2QsQ0FBQztRQUVELCtCQUErQjtZQUM3QixPQUFPLElBQUksQ0FBQztRQUNkLENBQUM7UUFFRCxzQkFBc0I7WUFDcEIsT0FBTyxJQUFJLENBQUM7UUFDZCxDQUFDO1FBRUQscUJBQXFCLENBQUMsS0FBYTtZQUNqQyxPQUFPLEtBQUssQ0FBQztRQUNmLENBQUM7UUFFRCxzQkFBc0I7WUFDcEIsT0FBTyxnQkFBZ0IsQ0FBQztRQUMxQixDQUFDO1FBRUQsaUJBQWlCO1lBQ2YsT0FBTyxZQUFZLENBQUMsSUFBSSxDQUFDLGlCQUFpQixFQUFFLENBQUMsQ0FBQztRQUNoRCxDQUFDO1FBRUQsZUFBZTtZQUNiLE9BQU8sWUFBWSxDQUFDLElBQUksQ0FBQyxpQkFBaUIsRUFBRSxDQUFDLENBQUM7UUFDaEQsQ0FBQztRQUVELHNCQUFzQjtZQUNwQixPQUFPLElBQUksQ0FBQztRQUNkLENBQUM7UUFFRCxXQUFXLEtBQUssQ0FBQztRQUVULGlCQUFpQjtZQUN2QixNQUFNLFFBQVEsR0FBRyxRQUFRLENBQUMsZUFBZSxDQUFDLDRCQUE0QixFQUFFLEtBQUssQ0FBQyxDQUFDO1lBQy9FLFFBQVEsQ0FBQyxTQUFTLENBQUMsR0FBRyxDQUFDLGtCQUFrQixDQUFDLENBQUM7WUFDM0MsZ0dBQWdHO1lBQ2hHLFFBQVEsQ0FBQyxZQUFZLENBQUMsS0FBSyxFQUFFLEVBQUUsQ0FBQyxDQUFDO1lBQ2pDLFFBQVEsQ0FBQyxZQUFZLENBQUMsUUFBUSxFQUFFLE1BQU0sQ0FBQyxDQUFDO1lBQ3hDLFFBQVEsQ0FBQyxZQUFZLENBQUMsT0FBTyxFQUFFLE1BQU0sQ0FBQyxDQUFDO1lBQ3ZDLFFBQVEsQ0FBQyxZQUFZLENBQUMscUJBQXFCLEVBQUUsZUFBZSxDQUFDLENBQUM7WUFDOUQsUUFBUSxDQUFDLFlBQVksQ0FBQyxXQUFXLEVBQUUsT0FBTyxDQUFDLENBQUM7WUFDNUMsT0FBTyxRQUFRLENBQUM7UUFDbEIsQ0FBQzs7O2dCQXRFRixVQUFVOztJQXVFWCwwQkFBQztLQUFBO1NBdEVZLG1CQUFtQjtBQXdFaEMscUVBQXFFO0FBQ3JFO0lBQUEsTUFHYSxvQkFBb0I7OztnQkFIaEMsUUFBUSxTQUFDO29CQUNSLFNBQVMsRUFBRSxDQUFDLEVBQUMsT0FBTyxFQUFFLGVBQWUsRUFBRSxRQUFRLEVBQUUsbUJBQW1CLEVBQUMsQ0FBQztpQkFDdkU7O0lBRUQsMkJBQUM7S0FBQTtTQURZLG9CQUFvQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0luamVjdGFibGUsIE5nTW9kdWxlLCBPbkRlc3Ryb3l9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtNYXRJY29uUmVnaXN0cnl9IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL2ljb24nO1xuaW1wb3J0IHtPYnNlcnZhYmxlLCBvZiBhcyBvYnNlcnZhYmxlT2Z9IGZyb20gJ3J4anMnO1xuXG4vLyB0c2xpbnQ6ZGlzYWJsZTpuby1hbnkgSW1wb3NzaWJsZSB0byB0ZWxsIHBhcmFtIHR5cGVzLlxudHlwZSBQdWJsaWNBcGk8VD4gPSB7XG4gIFtLIGluIGtleW9mIFRdOiBUW0tdIGV4dGVuZHMgKC4uLng6IGFueVtdKSA9PiBUID8gKC4uLng6IGFueVtdKSA9PiBQdWJsaWNBcGk8VD4gOiBUW0tdXG59O1xuLy8gdHNsaW50OmVuYWJsZTpuby1hbnlcblxuLyoqXG4gKiBBIG51bGwgaWNvbiByZWdpc3RyeSB0aGF0IG11c3QgYmUgaW1wb3J0ZWQgdG8gYWxsb3cgZGlzYWJsaW5nIG9mIGN1c3RvbVxuICogaWNvbnMuXG4gKi9cbkBJbmplY3RhYmxlKClcbmV4cG9ydCBjbGFzcyBGYWtlTWF0SWNvblJlZ2lzdHJ5IGltcGxlbWVudHMgUHVibGljQXBpPE1hdEljb25SZWdpc3RyeT4sIE9uRGVzdHJveSB7XG4gIGFkZFN2Z0ljb24oKTogdGhpcyB7XG4gICAgcmV0dXJuIHRoaXM7XG4gIH1cblxuICBhZGRTdmdJY29uTGl0ZXJhbCgpOiB0aGlzIHtcbiAgICByZXR1cm4gdGhpcztcbiAgfVxuXG4gIGFkZFN2Z0ljb25Jbk5hbWVzcGFjZSgpOiB0aGlzIHtcbiAgICByZXR1cm4gdGhpcztcbiAgfVxuXG4gIGFkZFN2Z0ljb25MaXRlcmFsSW5OYW1lc3BhY2UoKTogdGhpcyB7XG4gICAgcmV0dXJuIHRoaXM7XG4gIH1cblxuICBhZGRTdmdJY29uU2V0KCk6IHRoaXMge1xuICAgIHJldHVybiB0aGlzO1xuICB9XG5cbiAgYWRkU3ZnSWNvblNldExpdGVyYWwoKTogdGhpcyB7XG4gICAgcmV0dXJuIHRoaXM7XG4gIH1cblxuICBhZGRTdmdJY29uU2V0SW5OYW1lc3BhY2UoKTogdGhpcyB7XG4gICAgcmV0dXJuIHRoaXM7XG4gIH1cblxuICBhZGRTdmdJY29uU2V0TGl0ZXJhbEluTmFtZXNwYWNlKCk6IHRoaXMge1xuICAgIHJldHVybiB0aGlzO1xuICB9XG5cbiAgcmVnaXN0ZXJGb250Q2xhc3NBbGlhcygpOiB0aGlzIHtcbiAgICByZXR1cm4gdGhpcztcbiAgfVxuXG4gIGNsYXNzTmFtZUZvckZvbnRBbGlhcyhhbGlhczogc3RyaW5nKTogc3RyaW5nIHtcbiAgICByZXR1cm4gYWxpYXM7XG4gIH1cblxuICBnZXREZWZhdWx0Rm9udFNldENsYXNzKCkge1xuICAgIHJldHVybiAnbWF0ZXJpYWwtaWNvbnMnO1xuICB9XG5cbiAgZ2V0U3ZnSWNvbkZyb21VcmwoKTogT2JzZXJ2YWJsZTxTVkdFbGVtZW50PiB7XG4gICAgcmV0dXJuIG9ic2VydmFibGVPZih0aGlzLl9nZW5lcmF0ZUVtcHR5U3ZnKCkpO1xuICB9XG5cbiAgZ2V0TmFtZWRTdmdJY29uKCk6IE9ic2VydmFibGU8U1ZHRWxlbWVudD4ge1xuICAgIHJldHVybiBvYnNlcnZhYmxlT2YodGhpcy5fZ2VuZXJhdGVFbXB0eVN2ZygpKTtcbiAgfVxuXG4gIHNldERlZmF1bHRGb250U2V0Q2xhc3MoKTogdGhpcyB7XG4gICAgcmV0dXJuIHRoaXM7XG4gIH1cblxuICBuZ09uRGVzdHJveSgpIHsgfVxuXG4gIHByaXZhdGUgX2dlbmVyYXRlRW1wdHlTdmcoKTogU1ZHRWxlbWVudCB7XG4gICAgY29uc3QgZW1wdHlTdmcgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50TlMoJ2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJywgJ3N2ZycpO1xuICAgIGVtcHR5U3ZnLmNsYXNzTGlzdC5hZGQoJ2Zha2UtdGVzdGluZy1zdmcnKTtcbiAgICAvLyBFbXVsYXRlIHJlYWwgaWNvbiBjaGFyYWN0ZXJpc3RpY3MgZnJvbSBgTWF0SWNvblJlZ2lzdHJ5YCBzbyBzaXplIHJlbWFpbnMgY29uc2lzdGVudCBpbiB0ZXN0cy5cbiAgICBlbXB0eVN2Zy5zZXRBdHRyaWJ1dGUoJ2ZpdCcsICcnKTtcbiAgICBlbXB0eVN2Zy5zZXRBdHRyaWJ1dGUoJ2hlaWdodCcsICcxMDAlJyk7XG4gICAgZW1wdHlTdmcuc2V0QXR0cmlidXRlKCd3aWR0aCcsICcxMDAlJyk7XG4gICAgZW1wdHlTdmcuc2V0QXR0cmlidXRlKCdwcmVzZXJ2ZUFzcGVjdFJhdGlvJywgJ3hNaWRZTWlkIG1lZXQnKTtcbiAgICBlbXB0eVN2Zy5zZXRBdHRyaWJ1dGUoJ2ZvY3VzYWJsZScsICdmYWxzZScpO1xuICAgIHJldHVybiBlbXB0eVN2ZztcbiAgfVxufVxuXG4vKiogSW1wb3J0IHRoaXMgbW9kdWxlIGluIHRlc3RzIHRvIGluc3RhbGwgdGhlIG51bGwgaWNvbiByZWdpc3RyeS4gKi9cbkBOZ01vZHVsZSh7XG4gIHByb3ZpZGVyczogW3twcm92aWRlOiBNYXRJY29uUmVnaXN0cnksIHVzZUNsYXNzOiBGYWtlTWF0SWNvblJlZ2lzdHJ5fV1cbn0pXG5leHBvcnQgY2xhc3MgTWF0SWNvblRlc3RpbmdNb2R1bGUge1xufVxuIl19