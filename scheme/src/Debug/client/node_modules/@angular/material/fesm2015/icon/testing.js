import { Injectable, NgModule } from '@angular/core';
import { MatIconRegistry } from '@angular/material/icon';
import { of } from 'rxjs';

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
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
            return of(this._generateEmptySvg());
        }
        getNamedSvgIcon() {
            return of(this._generateEmptySvg());
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

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */

/**
 * Generated bundle index. Do not edit.
 */

export { FakeMatIconRegistry, MatIconTestingModule };
//# sourceMappingURL=testing.js.map
