(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('@angular/core'), require('@angular/material/icon'), require('rxjs')) :
    typeof define === 'function' && define.amd ? define('@angular/material/icon/testing', ['exports', '@angular/core', '@angular/material/icon', 'rxjs'], factory) :
    (global = global || self, factory((global.ng = global.ng || {}, global.ng.material = global.ng.material || {}, global.ng.material.icon = global.ng.material.icon || {}, global.ng.material.icon.testing = {}), global.ng.core, global.ng.material.icon, global.rxjs));
}(this, (function (exports, core, icon, rxjs) { 'use strict';

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
    var FakeMatIconRegistry = /** @class */ (function () {
        function FakeMatIconRegistry() {
        }
        FakeMatIconRegistry.prototype.addSvgIcon = function () {
            return this;
        };
        FakeMatIconRegistry.prototype.addSvgIconLiteral = function () {
            return this;
        };
        FakeMatIconRegistry.prototype.addSvgIconInNamespace = function () {
            return this;
        };
        FakeMatIconRegistry.prototype.addSvgIconLiteralInNamespace = function () {
            return this;
        };
        FakeMatIconRegistry.prototype.addSvgIconSet = function () {
            return this;
        };
        FakeMatIconRegistry.prototype.addSvgIconSetLiteral = function () {
            return this;
        };
        FakeMatIconRegistry.prototype.addSvgIconSetInNamespace = function () {
            return this;
        };
        FakeMatIconRegistry.prototype.addSvgIconSetLiteralInNamespace = function () {
            return this;
        };
        FakeMatIconRegistry.prototype.registerFontClassAlias = function () {
            return this;
        };
        FakeMatIconRegistry.prototype.classNameForFontAlias = function (alias) {
            return alias;
        };
        FakeMatIconRegistry.prototype.getDefaultFontSetClass = function () {
            return 'material-icons';
        };
        FakeMatIconRegistry.prototype.getSvgIconFromUrl = function () {
            return rxjs.of(this._generateEmptySvg());
        };
        FakeMatIconRegistry.prototype.getNamedSvgIcon = function () {
            return rxjs.of(this._generateEmptySvg());
        };
        FakeMatIconRegistry.prototype.setDefaultFontSetClass = function () {
            return this;
        };
        FakeMatIconRegistry.prototype.ngOnDestroy = function () { };
        FakeMatIconRegistry.prototype._generateEmptySvg = function () {
            var emptySvg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
            emptySvg.classList.add('fake-testing-svg');
            // Emulate real icon characteristics from `MatIconRegistry` so size remains consistent in tests.
            emptySvg.setAttribute('fit', '');
            emptySvg.setAttribute('height', '100%');
            emptySvg.setAttribute('width', '100%');
            emptySvg.setAttribute('preserveAspectRatio', 'xMidYMid meet');
            emptySvg.setAttribute('focusable', 'false');
            return emptySvg;
        };
        FakeMatIconRegistry.decorators = [
            { type: core.Injectable }
        ];
        return FakeMatIconRegistry;
    }());
    /** Import this module in tests to install the null icon registry. */
    var MatIconTestingModule = /** @class */ (function () {
        function MatIconTestingModule() {
        }
        MatIconTestingModule.decorators = [
            { type: core.NgModule, args: [{
                        providers: [{ provide: icon.MatIconRegistry, useClass: FakeMatIconRegistry }]
                    },] }
        ];
        return MatIconTestingModule;
    }());

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

    exports.FakeMatIconRegistry = FakeMatIconRegistry;
    exports.MatIconTestingModule = MatIconTestingModule;

    Object.defineProperty(exports, '__esModule', { value: true });

})));
//# sourceMappingURL=material-icon-testing.umd.js.map
