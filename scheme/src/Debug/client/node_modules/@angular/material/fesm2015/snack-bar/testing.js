import { __awaiter } from 'tslib';
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** Harness for interacting with a standard mat-snack-bar in tests. */
let MatSnackBarHarness = /** @class */ (() => {
    class MatSnackBarHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._simpleSnackBar = this.locatorForOptional('.mat-simple-snackbar');
            this._simpleSnackBarMessage = this.locatorFor('.mat-simple-snackbar > span');
            this._simpleSnackBarActionButton = this.locatorForOptional('.mat-simple-snackbar-action > button');
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatSnackBarHarness` that meets
         * certain criteria.
         * @param options Options for filtering which snack bar instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatSnackBarHarness, options);
        }
        /**
         * Gets the role of the snack-bar. The role of a snack-bar is determined based
         * on the ARIA politeness specified in the snack-bar config.
         */
        getRole() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('role');
            });
        }
        /**
         * Whether the snack-bar has an action. Method cannot be used for snack-bar's with custom content.
         */
        hasAction() {
            return __awaiter(this, void 0, void 0, function* () {
                yield this._assertSimpleSnackBar();
                return (yield this._simpleSnackBarActionButton()) !== null;
            });
        }
        /**
         * Gets the description of the snack-bar. Method cannot be used for snack-bar's without action or
         * with custom content.
         */
        getActionDescription() {
            return __awaiter(this, void 0, void 0, function* () {
                yield this._assertSimpleSnackBarWithAction();
                return (yield this._simpleSnackBarActionButton()).text();
            });
        }
        /**
         * Dismisses the snack-bar by clicking the action button. Method cannot be used for snack-bar's
         * without action or with custom content.
         */
        dismissWithAction() {
            return __awaiter(this, void 0, void 0, function* () {
                yield this._assertSimpleSnackBarWithAction();
                yield (yield this._simpleSnackBarActionButton()).click();
            });
        }
        /**
         * Gets the message of the snack-bar. Method cannot be used for snack-bar's with custom content.
         */
        getMessage() {
            return __awaiter(this, void 0, void 0, function* () {
                yield this._assertSimpleSnackBar();
                return (yield this._simpleSnackBarMessage()).text();
            });
        }
        /** Gets whether the snack-bar has been dismissed. */
        isDismissed() {
            return __awaiter(this, void 0, void 0, function* () {
                // We consider the snackbar dismissed if it's not in the DOM. We can assert that the
                // element isn't in the DOM by seeing that its width and height are zero.
                const host = yield this.host();
                const [exit, dimensions] = yield Promise.all([
                    // The snackbar container is marked with the "exit" attribute after it has been dismissed
                    // but before the animation has finished (after which it's removed from the DOM).
                    host.getAttribute('mat-exit'),
                    host.getDimensions(),
                ]);
                return exit != null || (!!dimensions && dimensions.height === 0 && dimensions.width === 0);
            });
        }
        /**
         * Asserts that the current snack-bar does not use custom content. Promise rejects if
         * custom content is used.
         */
        _assertSimpleSnackBar() {
            return __awaiter(this, void 0, void 0, function* () {
                if (!(yield this._isSimpleSnackBar())) {
                    throw Error('Method cannot be used for snack-bar with custom content.');
                }
            });
        }
        /**
         * Asserts that the current snack-bar does not use custom content and has
         * an action defined. Otherwise the promise will reject.
         */
        _assertSimpleSnackBarWithAction() {
            return __awaiter(this, void 0, void 0, function* () {
                yield this._assertSimpleSnackBar();
                if (!(yield this.hasAction())) {
                    throw Error('Method cannot be used for standard snack-bar without action.');
                }
            });
        }
        /** Whether the snack-bar is using the default content template. */
        _isSimpleSnackBar() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._simpleSnackBar()) !== null;
            });
        }
    }
    // Developers can provide a custom component or template for the
    // snackbar. The canonical snack-bar parent is the "MatSnackBarContainer".
    /** The selector for the host element of a `MatSnackBar` instance. */
    MatSnackBarHarness.hostSelector = '.mat-snack-bar-container';
    return MatSnackBarHarness;
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

export { MatSnackBarHarness };
//# sourceMappingURL=testing.js.map
