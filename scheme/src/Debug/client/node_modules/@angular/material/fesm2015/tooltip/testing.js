import { __awaiter } from 'tslib';
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** Harness for interacting with a standard mat-tooltip in tests. */
let MatTooltipHarness = /** @class */ (() => {
    class MatTooltipHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._optionalPanel = this.documentRootLocatorFactory().locatorForOptional('.mat-tooltip');
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search
         * for a tooltip trigger with specific attributes.
         * @param options Options for narrowing the search.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatTooltipHarness, options);
        }
        /** Shows the tooltip. */
        show() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hover();
            });
        }
        /** Hides the tooltip. */
        hide() {
            return __awaiter(this, void 0, void 0, function* () {
                const host = yield this.host();
                yield host.mouseAway();
                yield this.forceStabilize(); // Needed in order to flush the `hide` animation.
            });
        }
        /** Gets whether the tooltip is open. */
        isOpen() {
            return __awaiter(this, void 0, void 0, function* () {
                return !!(yield this._optionalPanel());
            });
        }
        /** Gets a promise for the tooltip panel's text. */
        getTooltipText() {
            return __awaiter(this, void 0, void 0, function* () {
                const panel = yield this._optionalPanel();
                return panel ? panel.text() : '';
            });
        }
    }
    MatTooltipHarness.hostSelector = '.mat-tooltip-trigger';
    return MatTooltipHarness;
})();

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */

export { MatTooltipHarness };
//# sourceMappingURL=testing.js.map
