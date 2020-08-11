import { __awaiter } from 'tslib';
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';

/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/** Harness for interacting with a standard mat-drawer in tests. */
let MatDrawerHarness = /** @class */ (() => {
    class MatDrawerHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatDrawerHarness` that meets
         * certain criteria.
         * @param options Options for filtering which drawer instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatDrawerHarness, options)
                .addOption('position', options.position, (harness, position) => __awaiter(this, void 0, void 0, function* () { return (yield harness.getPosition()) === position; }));
        }
        /** Whether the drawer is open. */
        isOpen() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-drawer-opened');
            });
        }
        /** Gets the position of the drawer inside its container. */
        getPosition() {
            return __awaiter(this, void 0, void 0, function* () {
                const host = yield this.host();
                return (yield host.hasClass('mat-drawer-end')) ? 'end' : 'start';
            });
        }
        /** Gets the mode that the drawer is in. */
        getMode() {
            return __awaiter(this, void 0, void 0, function* () {
                const host = yield this.host();
                if (yield host.hasClass('mat-drawer-push')) {
                    return 'push';
                }
                if (yield host.hasClass('mat-drawer-side')) {
                    return 'side';
                }
                return 'over';
            });
        }
    }
    /** The selector for the host element of a `MatDrawer` instance. */
    MatDrawerHarness.hostSelector = '.mat-drawer';
    return MatDrawerHarness;
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
/** Harness for interacting with a standard mat-sidenav in tests. */
let MatSidenavHarness = /** @class */ (() => {
    class MatSidenavHarness extends MatDrawerHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatSidenavHarness` that meets
         * certain criteria.
         * @param options Options for filtering which sidenav instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatDrawerHarness, options)
                .addOption('position', options.position, (harness, position) => __awaiter(this, void 0, void 0, function* () { return (yield harness.getPosition()) === position; }));
        }
        /** Whether the sidenav is fixed in the viewport. */
        isFixedInViewport() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-sidenav-fixed');
            });
        }
    }
    /** The selector for the host element of a `MatSidenav` instance. */
    MatSidenavHarness.hostSelector = '.mat-sidenav';
    return MatSidenavHarness;
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

export { MatDrawerHarness, MatSidenavHarness };
//# sourceMappingURL=testing.js.map
