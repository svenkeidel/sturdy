/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
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
export { MatSnackBarHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic25hY2stYmFyLWhhcm5lc3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvc25hY2stYmFyL3Rlc3Rpbmcvc25hY2stYmFyLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBR3hFLHNFQUFzRTtBQUN0RTtJQUFBLE1BQWEsa0JBQW1CLFNBQVEsZ0JBQWdCO1FBQXhEOztZQU1VLG9CQUFlLEdBQUcsSUFBSSxDQUFDLGtCQUFrQixDQUFDLHNCQUFzQixDQUFDLENBQUM7WUFDbEUsMkJBQXNCLEdBQUcsSUFBSSxDQUFDLFVBQVUsQ0FBQyw2QkFBNkIsQ0FBQyxDQUFDO1lBQ3hFLGdDQUEyQixHQUMvQixJQUFJLENBQUMsa0JBQWtCLENBQUMsc0NBQXNDLENBQUMsQ0FBQztRQWdHdEUsQ0FBQztRQTlGQzs7Ozs7V0FLRztRQUNILE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBa0MsRUFBRTtZQUM5QyxPQUFPLElBQUksZ0JBQWdCLENBQUMsa0JBQWtCLEVBQUUsT0FBTyxDQUFDLENBQUM7UUFDM0QsQ0FBQztRQUVEOzs7V0FHRztRQUNHLE9BQU87O2dCQUNYLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFlBQVksQ0FBQyxNQUFNLENBQW1DLENBQUM7WUFDcEYsQ0FBQztTQUFBO1FBRUQ7O1dBRUc7UUFDRyxTQUFTOztnQkFDYixNQUFNLElBQUksQ0FBQyxxQkFBcUIsRUFBRSxDQUFDO2dCQUNuQyxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsMkJBQTJCLEVBQUUsQ0FBQyxLQUFLLElBQUksQ0FBQztZQUM3RCxDQUFDO1NBQUE7UUFFRDs7O1dBR0c7UUFDRyxvQkFBb0I7O2dCQUN4QixNQUFNLElBQUksQ0FBQywrQkFBK0IsRUFBRSxDQUFDO2dCQUM3QyxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsMkJBQTJCLEVBQUUsQ0FBRSxDQUFDLElBQUksRUFBRSxDQUFDO1lBQzVELENBQUM7U0FBQTtRQUdEOzs7V0FHRztRQUNHLGlCQUFpQjs7Z0JBQ3JCLE1BQU0sSUFBSSxDQUFDLCtCQUErQixFQUFFLENBQUM7Z0JBQzdDLE1BQU0sQ0FBQyxNQUFNLElBQUksQ0FBQywyQkFBMkIsRUFBRSxDQUFFLENBQUMsS0FBSyxFQUFFLENBQUM7WUFDNUQsQ0FBQztTQUFBO1FBRUQ7O1dBRUc7UUFDRyxVQUFVOztnQkFDZCxNQUFNLElBQUksQ0FBQyxxQkFBcUIsRUFBRSxDQUFDO2dCQUNuQyxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsc0JBQXNCLEVBQUUsQ0FBQyxDQUFDLElBQUksRUFBRSxDQUFDO1lBQ3RELENBQUM7U0FBQTtRQUVELHFEQUFxRDtRQUMvQyxXQUFXOztnQkFDZixvRkFBb0Y7Z0JBQ3BGLHlFQUF5RTtnQkFFekUsTUFBTSxJQUFJLEdBQUcsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUM7Z0JBQy9CLE1BQU0sQ0FBQyxJQUFJLEVBQUUsVUFBVSxDQUFDLEdBQUcsTUFBTSxPQUFPLENBQUMsR0FBRyxDQUFDO29CQUMzQyx5RkFBeUY7b0JBQ3pGLGlGQUFpRjtvQkFDakYsSUFBSSxDQUFDLFlBQVksQ0FBQyxVQUFVLENBQUM7b0JBQzdCLElBQUksQ0FBQyxhQUFhLEVBQUU7aUJBQ3JCLENBQUMsQ0FBQztnQkFFSCxPQUFPLElBQUksSUFBSSxJQUFJLElBQUksQ0FBQyxDQUFDLENBQUMsVUFBVSxJQUFJLFVBQVUsQ0FBQyxNQUFNLEtBQUssQ0FBQyxJQUFJLFVBQVUsQ0FBQyxLQUFLLEtBQUssQ0FBQyxDQUFDLENBQUM7WUFDN0YsQ0FBQztTQUFBO1FBRUQ7OztXQUdHO1FBQ1cscUJBQXFCOztnQkFDakMsSUFBSSxDQUFDLENBQUEsTUFBTSxJQUFJLENBQUMsaUJBQWlCLEVBQUUsQ0FBQSxFQUFFO29CQUNuQyxNQUFNLEtBQUssQ0FBQywwREFBMEQsQ0FBQyxDQUFDO2lCQUN6RTtZQUNILENBQUM7U0FBQTtRQUVEOzs7V0FHRztRQUNXLCtCQUErQjs7Z0JBQzNDLE1BQU0sSUFBSSxDQUFDLHFCQUFxQixFQUFFLENBQUM7Z0JBQ25DLElBQUksQ0FBQyxDQUFBLE1BQU0sSUFBSSxDQUFDLFNBQVMsRUFBRSxDQUFBLEVBQUU7b0JBQzNCLE1BQU0sS0FBSyxDQUFDLDhEQUE4RCxDQUFDLENBQUM7aUJBQzdFO1lBQ0gsQ0FBQztTQUFBO1FBRUQsbUVBQW1FO1FBQ3JELGlCQUFpQjs7Z0JBQzdCLE9BQU8sQ0FBQSxNQUFNLElBQUksQ0FBQyxlQUFlLEVBQUUsTUFBSyxJQUFJLENBQUM7WUFDL0MsQ0FBQztTQUFBOztJQXZHRCxnRUFBZ0U7SUFDaEUsMEVBQTBFO0lBQzFFLHFFQUFxRTtJQUM5RCwrQkFBWSxHQUFHLDBCQUEwQixDQUFDO0lBcUduRCx5QkFBQztLQUFBO1NBekdZLGtCQUFrQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0NvbXBvbmVudEhhcm5lc3MsIEhhcm5lc3NQcmVkaWNhdGV9IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7U25hY2tCYXJIYXJuZXNzRmlsdGVyc30gZnJvbSAnLi9zbmFjay1iYXItaGFybmVzcy1maWx0ZXJzJztcblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBtYXQtc25hY2stYmFyIGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdFNuYWNrQmFySGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICAvLyBEZXZlbG9wZXJzIGNhbiBwcm92aWRlIGEgY3VzdG9tIGNvbXBvbmVudCBvciB0ZW1wbGF0ZSBmb3IgdGhlXG4gIC8vIHNuYWNrYmFyLiBUaGUgY2Fub25pY2FsIHNuYWNrLWJhciBwYXJlbnQgaXMgdGhlIFwiTWF0U25hY2tCYXJDb250YWluZXJcIi5cbiAgLyoqIFRoZSBzZWxlY3RvciBmb3IgdGhlIGhvc3QgZWxlbWVudCBvZiBhIGBNYXRTbmFja0JhcmAgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1zbmFjay1iYXItY29udGFpbmVyJztcblxuICBwcml2YXRlIF9zaW1wbGVTbmFja0JhciA9IHRoaXMubG9jYXRvckZvck9wdGlvbmFsKCcubWF0LXNpbXBsZS1zbmFja2JhcicpO1xuICBwcml2YXRlIF9zaW1wbGVTbmFja0Jhck1lc3NhZ2UgPSB0aGlzLmxvY2F0b3JGb3IoJy5tYXQtc2ltcGxlLXNuYWNrYmFyID4gc3BhbicpO1xuICBwcml2YXRlIF9zaW1wbGVTbmFja0JhckFjdGlvbkJ1dHRvbiA9XG4gICAgICB0aGlzLmxvY2F0b3JGb3JPcHRpb25hbCgnLm1hdC1zaW1wbGUtc25hY2tiYXItYWN0aW9uID4gYnV0dG9uJyk7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYE1hdFNuYWNrQmFySGFybmVzc2AgdGhhdCBtZWV0c1xuICAgKiBjZXJ0YWluIGNyaXRlcmlhLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBmaWx0ZXJpbmcgd2hpY2ggc25hY2sgYmFyIGluc3RhbmNlcyBhcmUgY29uc2lkZXJlZCBhIG1hdGNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IFNuYWNrQmFySGFybmVzc0ZpbHRlcnMgPSB7fSk6IEhhcm5lc3NQcmVkaWNhdGU8TWF0U25hY2tCYXJIYXJuZXNzPiB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdFNuYWNrQmFySGFybmVzcywgb3B0aW9ucyk7XG4gIH1cblxuICAvKipcbiAgICogR2V0cyB0aGUgcm9sZSBvZiB0aGUgc25hY2stYmFyLiBUaGUgcm9sZSBvZiBhIHNuYWNrLWJhciBpcyBkZXRlcm1pbmVkIGJhc2VkXG4gICAqIG9uIHRoZSBBUklBIHBvbGl0ZW5lc3Mgc3BlY2lmaWVkIGluIHRoZSBzbmFjay1iYXIgY29uZmlnLlxuICAgKi9cbiAgYXN5bmMgZ2V0Um9sZSgpOiBQcm9taXNlPCdhbGVydCd8J3N0YXR1cyd8bnVsbD4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldEF0dHJpYnV0ZSgncm9sZScpIGFzIFByb21pc2U8J2FsZXJ0J3wnc3RhdHVzJ3xudWxsPjtcbiAgfVxuXG4gIC8qKlxuICAgKiBXaGV0aGVyIHRoZSBzbmFjay1iYXIgaGFzIGFuIGFjdGlvbi4gTWV0aG9kIGNhbm5vdCBiZSB1c2VkIGZvciBzbmFjay1iYXIncyB3aXRoIGN1c3RvbSBjb250ZW50LlxuICAgKi9cbiAgYXN5bmMgaGFzQWN0aW9uKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIGF3YWl0IHRoaXMuX2Fzc2VydFNpbXBsZVNuYWNrQmFyKCk7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9zaW1wbGVTbmFja0JhckFjdGlvbkJ1dHRvbigpKSAhPT0gbnVsbDtcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXRzIHRoZSBkZXNjcmlwdGlvbiBvZiB0aGUgc25hY2stYmFyLiBNZXRob2QgY2Fubm90IGJlIHVzZWQgZm9yIHNuYWNrLWJhcidzIHdpdGhvdXQgYWN0aW9uIG9yXG4gICAqIHdpdGggY3VzdG9tIGNvbnRlbnQuXG4gICAqL1xuICBhc3luYyBnZXRBY3Rpb25EZXNjcmlwdGlvbigpOiBQcm9taXNlPHN0cmluZz4ge1xuICAgIGF3YWl0IHRoaXMuX2Fzc2VydFNpbXBsZVNuYWNrQmFyV2l0aEFjdGlvbigpO1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5fc2ltcGxlU25hY2tCYXJBY3Rpb25CdXR0b24oKSkhLnRleHQoKTtcbiAgfVxuXG5cbiAgLyoqXG4gICAqIERpc21pc3NlcyB0aGUgc25hY2stYmFyIGJ5IGNsaWNraW5nIHRoZSBhY3Rpb24gYnV0dG9uLiBNZXRob2QgY2Fubm90IGJlIHVzZWQgZm9yIHNuYWNrLWJhcidzXG4gICAqIHdpdGhvdXQgYWN0aW9uIG9yIHdpdGggY3VzdG9tIGNvbnRlbnQuXG4gICAqL1xuICBhc3luYyBkaXNtaXNzV2l0aEFjdGlvbigpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICBhd2FpdCB0aGlzLl9hc3NlcnRTaW1wbGVTbmFja0JhcldpdGhBY3Rpb24oKTtcbiAgICBhd2FpdCAoYXdhaXQgdGhpcy5fc2ltcGxlU25hY2tCYXJBY3Rpb25CdXR0b24oKSkhLmNsaWNrKCk7XG4gIH1cblxuICAvKipcbiAgICogR2V0cyB0aGUgbWVzc2FnZSBvZiB0aGUgc25hY2stYmFyLiBNZXRob2QgY2Fubm90IGJlIHVzZWQgZm9yIHNuYWNrLWJhcidzIHdpdGggY3VzdG9tIGNvbnRlbnQuXG4gICAqL1xuICBhc3luYyBnZXRNZXNzYWdlKCk6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgYXdhaXQgdGhpcy5fYXNzZXJ0U2ltcGxlU25hY2tCYXIoKTtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuX3NpbXBsZVNuYWNrQmFyTWVzc2FnZSgpKS50ZXh0KCk7XG4gIH1cblxuICAvKiogR2V0cyB3aGV0aGVyIHRoZSBzbmFjay1iYXIgaGFzIGJlZW4gZGlzbWlzc2VkLiAqL1xuICBhc3luYyBpc0Rpc21pc3NlZCgpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICAvLyBXZSBjb25zaWRlciB0aGUgc25hY2tiYXIgZGlzbWlzc2VkIGlmIGl0J3Mgbm90IGluIHRoZSBET00uIFdlIGNhbiBhc3NlcnQgdGhhdCB0aGVcbiAgICAvLyBlbGVtZW50IGlzbid0IGluIHRoZSBET00gYnkgc2VlaW5nIHRoYXQgaXRzIHdpZHRoIGFuZCBoZWlnaHQgYXJlIHplcm8uXG5cbiAgICBjb25zdCBob3N0ID0gYXdhaXQgdGhpcy5ob3N0KCk7XG4gICAgY29uc3QgW2V4aXQsIGRpbWVuc2lvbnNdID0gYXdhaXQgUHJvbWlzZS5hbGwoW1xuICAgICAgLy8gVGhlIHNuYWNrYmFyIGNvbnRhaW5lciBpcyBtYXJrZWQgd2l0aCB0aGUgXCJleGl0XCIgYXR0cmlidXRlIGFmdGVyIGl0IGhhcyBiZWVuIGRpc21pc3NlZFxuICAgICAgLy8gYnV0IGJlZm9yZSB0aGUgYW5pbWF0aW9uIGhhcyBmaW5pc2hlZCAoYWZ0ZXIgd2hpY2ggaXQncyByZW1vdmVkIGZyb20gdGhlIERPTSkuXG4gICAgICBob3N0LmdldEF0dHJpYnV0ZSgnbWF0LWV4aXQnKSxcbiAgICAgIGhvc3QuZ2V0RGltZW5zaW9ucygpLFxuICAgIF0pO1xuXG4gICAgcmV0dXJuIGV4aXQgIT0gbnVsbCB8fCAoISFkaW1lbnNpb25zICYmIGRpbWVuc2lvbnMuaGVpZ2h0ID09PSAwICYmIGRpbWVuc2lvbnMud2lkdGggPT09IDApO1xuICB9XG5cbiAgLyoqXG4gICAqIEFzc2VydHMgdGhhdCB0aGUgY3VycmVudCBzbmFjay1iYXIgZG9lcyBub3QgdXNlIGN1c3RvbSBjb250ZW50LiBQcm9taXNlIHJlamVjdHMgaWZcbiAgICogY3VzdG9tIGNvbnRlbnQgaXMgdXNlZC5cbiAgICovXG4gIHByaXZhdGUgYXN5bmMgX2Fzc2VydFNpbXBsZVNuYWNrQmFyKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIGlmICghYXdhaXQgdGhpcy5faXNTaW1wbGVTbmFja0JhcigpKSB7XG4gICAgICB0aHJvdyBFcnJvcignTWV0aG9kIGNhbm5vdCBiZSB1c2VkIGZvciBzbmFjay1iYXIgd2l0aCBjdXN0b20gY29udGVudC4nKTtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogQXNzZXJ0cyB0aGF0IHRoZSBjdXJyZW50IHNuYWNrLWJhciBkb2VzIG5vdCB1c2UgY3VzdG9tIGNvbnRlbnQgYW5kIGhhc1xuICAgKiBhbiBhY3Rpb24gZGVmaW5lZC4gT3RoZXJ3aXNlIHRoZSBwcm9taXNlIHdpbGwgcmVqZWN0LlxuICAgKi9cbiAgcHJpdmF0ZSBhc3luYyBfYXNzZXJ0U2ltcGxlU25hY2tCYXJXaXRoQWN0aW9uKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIGF3YWl0IHRoaXMuX2Fzc2VydFNpbXBsZVNuYWNrQmFyKCk7XG4gICAgaWYgKCFhd2FpdCB0aGlzLmhhc0FjdGlvbigpKSB7XG4gICAgICB0aHJvdyBFcnJvcignTWV0aG9kIGNhbm5vdCBiZSB1c2VkIGZvciBzdGFuZGFyZCBzbmFjay1iYXIgd2l0aG91dCBhY3Rpb24uJyk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIHNuYWNrLWJhciBpcyB1c2luZyB0aGUgZGVmYXVsdCBjb250ZW50IHRlbXBsYXRlLiAqL1xuICBwcml2YXRlIGFzeW5jIF9pc1NpbXBsZVNuYWNrQmFyKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIHJldHVybiBhd2FpdCB0aGlzLl9zaW1wbGVTbmFja0JhcigpICE9PSBudWxsO1xuICB9XG59XG4iXX0=