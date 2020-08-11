/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
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
export { MatTooltipHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidG9vbHRpcC1oYXJuZXNzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL3Rvb2x0aXAvdGVzdGluZy90b29sdGlwLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBR3hFLG9FQUFvRTtBQUNwRTtJQUFBLE1BQWEsaUJBQWtCLFNBQVEsZ0JBQWdCO1FBQXZEOztZQUNVLG1CQUFjLEdBQUcsSUFBSSxDQUFDLDBCQUEwQixFQUFFLENBQUMsa0JBQWtCLENBQUMsY0FBYyxDQUFDLENBQUM7UUFtQ2hHLENBQUM7UUFoQ0M7Ozs7O1dBS0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQWlDLEVBQUU7WUFDN0MsT0FBTyxJQUFJLGdCQUFnQixDQUFDLGlCQUFpQixFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQzFELENBQUM7UUFFRCx5QkFBeUI7UUFDbkIsSUFBSTs7Z0JBQ1IsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsS0FBSyxFQUFFLENBQUM7WUFDckMsQ0FBQztTQUFBO1FBRUQseUJBQXlCO1FBQ25CLElBQUk7O2dCQUNSLE1BQU0sSUFBSSxHQUFHLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDO2dCQUMvQixNQUFNLElBQUksQ0FBQyxTQUFTLEVBQUUsQ0FBQztnQkFDdkIsTUFBTSxJQUFJLENBQUMsY0FBYyxFQUFFLENBQUMsQ0FBQyxpREFBaUQ7WUFDaEYsQ0FBQztTQUFBO1FBRUQsd0NBQXdDO1FBQ2xDLE1BQU07O2dCQUNWLE9BQU8sQ0FBQyxDQUFDLENBQUMsTUFBTSxJQUFJLENBQUMsY0FBYyxFQUFFLENBQUMsQ0FBQztZQUN6QyxDQUFDO1NBQUE7UUFFRCxtREFBbUQ7UUFDN0MsY0FBYzs7Z0JBQ2xCLE1BQU0sS0FBSyxHQUFHLE1BQU0sSUFBSSxDQUFDLGNBQWMsRUFBRSxDQUFDO2dCQUMxQyxPQUFPLEtBQUssQ0FBQyxDQUFDLENBQUMsS0FBSyxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsQ0FBQyxFQUFFLENBQUM7WUFDbkMsQ0FBQztTQUFBOztJQWpDTSw4QkFBWSxHQUFHLHNCQUFzQixDQUFDO0lBa0MvQyx3QkFBQztLQUFBO1NBcENZLGlCQUFpQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0NvbXBvbmVudEhhcm5lc3MsIEhhcm5lc3NQcmVkaWNhdGV9IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7VG9vbHRpcEhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL3Rvb2x0aXAtaGFybmVzcy1maWx0ZXJzJztcblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBtYXQtdG9vbHRpcCBpbiB0ZXN0cy4gKi9cbmV4cG9ydCBjbGFzcyBNYXRUb29sdGlwSGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICBwcml2YXRlIF9vcHRpb25hbFBhbmVsID0gdGhpcy5kb2N1bWVudFJvb3RMb2NhdG9yRmFjdG9yeSgpLmxvY2F0b3JGb3JPcHRpb25hbCgnLm1hdC10b29sdGlwJyk7XG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC10b29sdGlwLXRyaWdnZXInO1xuXG4gIC8qKlxuICAgKiBHZXRzIGEgYEhhcm5lc3NQcmVkaWNhdGVgIHRoYXQgY2FuIGJlIHVzZWQgdG8gc2VhcmNoXG4gICAqIGZvciBhIHRvb2x0aXAgdHJpZ2dlciB3aXRoIHNwZWNpZmljIGF0dHJpYnV0ZXMuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIG5hcnJvd2luZyB0aGUgc2VhcmNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IFRvb2x0aXBIYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXRUb29sdGlwSGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXRUb29sdGlwSGFybmVzcywgb3B0aW9ucyk7XG4gIH1cblxuICAvKiogU2hvd3MgdGhlIHRvb2x0aXAuICovXG4gIGFzeW5jIHNob3coKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuaG92ZXIoKTtcbiAgfVxuXG4gIC8qKiBIaWRlcyB0aGUgdG9vbHRpcC4gKi9cbiAgYXN5bmMgaGlkZSgpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICBjb25zdCBob3N0ID0gYXdhaXQgdGhpcy5ob3N0KCk7XG4gICAgYXdhaXQgaG9zdC5tb3VzZUF3YXkoKTtcbiAgICBhd2FpdCB0aGlzLmZvcmNlU3RhYmlsaXplKCk7IC8vIE5lZWRlZCBpbiBvcmRlciB0byBmbHVzaCB0aGUgYGhpZGVgIGFuaW1hdGlvbi5cbiAgfVxuXG4gIC8qKiBHZXRzIHdoZXRoZXIgdGhlIHRvb2x0aXAgaXMgb3Blbi4gKi9cbiAgYXN5bmMgaXNPcGVuKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIHJldHVybiAhIShhd2FpdCB0aGlzLl9vcHRpb25hbFBhbmVsKCkpO1xuICB9XG5cbiAgLyoqIEdldHMgYSBwcm9taXNlIGZvciB0aGUgdG9vbHRpcCBwYW5lbCdzIHRleHQuICovXG4gIGFzeW5jIGdldFRvb2x0aXBUZXh0KCk6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgY29uc3QgcGFuZWwgPSBhd2FpdCB0aGlzLl9vcHRpb25hbFBhbmVsKCk7XG4gICAgcmV0dXJuIHBhbmVsID8gcGFuZWwudGV4dCgpIDogJyc7XG4gIH1cbn1cbiJdfQ==