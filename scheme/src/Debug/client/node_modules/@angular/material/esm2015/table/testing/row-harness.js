/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MatCellHarness, MatHeaderCellHarness, MatFooterCellHarness } from './cell-harness';
/** Harness for interacting with a standard Angular Material table row. */
let MatRowHarness = /** @class */ (() => {
    class MatRowHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a table row with specific attributes.
         * @param options Options for narrowing the search
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatRowHarness, options);
        }
        /** Gets a list of `MatCellHarness` for all cells in the row. */
        getCells(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatCellHarness.with(filter))();
            });
        }
        /** Gets the text of the cells in the row. */
        getCellTextByIndex(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return getCellTextByIndex(this, filter);
            });
        }
        /** Gets the text inside the row organized by columns. */
        getCellTextByColumnName() {
            return __awaiter(this, void 0, void 0, function* () {
                return getCellTextByColumnName(this);
            });
        }
    }
    /** The selector for the host element of a `MatRowHarness` instance. */
    MatRowHarness.hostSelector = '.mat-row';
    return MatRowHarness;
})();
export { MatRowHarness };
/** Harness for interacting with a standard Angular Material table header row. */
let MatHeaderRowHarness = /** @class */ (() => {
    class MatHeaderRowHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for
         * a table header row with specific attributes.
         * @param options Options for narrowing the search
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatHeaderRowHarness, options);
        }
        /** Gets a list of `MatHeaderCellHarness` for all cells in the row. */
        getCells(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatHeaderCellHarness.with(filter))();
            });
        }
        /** Gets the text of the cells in the header row. */
        getCellTextByIndex(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return getCellTextByIndex(this, filter);
            });
        }
        /** Gets the text inside the header row organized by columns. */
        getCellTextByColumnName() {
            return __awaiter(this, void 0, void 0, function* () {
                return getCellTextByColumnName(this);
            });
        }
    }
    /** The selector for the host element of a `MatHeaderRowHarness` instance. */
    MatHeaderRowHarness.hostSelector = '.mat-header-row';
    return MatHeaderRowHarness;
})();
export { MatHeaderRowHarness };
/** Harness for interacting with a standard Angular Material table footer row. */
let MatFooterRowHarness = /** @class */ (() => {
    class MatFooterRowHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for
         * a table footer row cell with specific attributes.
         * @param options Options for narrowing the search
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatFooterRowHarness, options);
        }
        /** Gets a list of `MatFooterCellHarness` for all cells in the row. */
        getCells(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatFooterCellHarness.with(filter))();
            });
        }
        /** Gets the text of the cells in the footer row. */
        getCellTextByIndex(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return getCellTextByIndex(this, filter);
            });
        }
        /** Gets the text inside the footer row organized by columns. */
        getCellTextByColumnName() {
            return __awaiter(this, void 0, void 0, function* () {
                return getCellTextByColumnName(this);
            });
        }
    }
    /** The selector for the host element of a `MatFooterRowHarness` instance. */
    MatFooterRowHarness.hostSelector = '.mat-footer-row';
    return MatFooterRowHarness;
})();
export { MatFooterRowHarness };
function getCellTextByIndex(harness, filter) {
    return __awaiter(this, void 0, void 0, function* () {
        const cells = yield harness.getCells(filter);
        return Promise.all(cells.map(cell => cell.getText()));
    });
}
function getCellTextByColumnName(harness) {
    return __awaiter(this, void 0, void 0, function* () {
        const output = {};
        const cells = yield harness.getCells();
        const cellsData = yield Promise.all(cells.map(cell => {
            return Promise.all([cell.getColumnName(), cell.getText()]);
        }));
        cellsData.forEach(([columnName, text]) => output[columnName] = text);
        return output;
    });
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicm93LWhhcm5lc3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvdGFibGUvdGVzdGluZy9yb3ctaGFybmVzcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7O0FBRUgsT0FBTyxFQUFDLGdCQUFnQixFQUFFLGdCQUFnQixFQUFDLE1BQU0sc0JBQXNCLENBQUM7QUFFeEUsT0FBTyxFQUFDLGNBQWMsRUFBRSxvQkFBb0IsRUFBRSxvQkFBb0IsRUFBQyxNQUFNLGdCQUFnQixDQUFDO0FBTzFGLDBFQUEwRTtBQUMxRTtJQUFBLE1BQWEsYUFBYyxTQUFRLGdCQUFnQjtRQUlqRDs7OztXQUlHO1FBQ0gsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUE2QixFQUFFO1lBQ3pDLE9BQU8sSUFBSSxnQkFBZ0IsQ0FBQyxhQUFhLEVBQUUsT0FBTyxDQUFDLENBQUM7UUFDdEQsQ0FBQztRQUVELGdFQUFnRTtRQUMxRCxRQUFRLENBQUMsU0FBNkIsRUFBRTs7Z0JBQzVDLE9BQU8sSUFBSSxDQUFDLGFBQWEsQ0FBQyxjQUFjLENBQUMsSUFBSSxDQUFDLE1BQU0sQ0FBQyxDQUFDLEVBQUUsQ0FBQztZQUMzRCxDQUFDO1NBQUE7UUFFRCw2Q0FBNkM7UUFDdkMsa0JBQWtCLENBQUMsU0FBNkIsRUFBRTs7Z0JBQ3RELE9BQU8sa0JBQWtCLENBQUMsSUFBSSxFQUFFLE1BQU0sQ0FBQyxDQUFDO1lBQzFDLENBQUM7U0FBQTtRQUVELHlEQUF5RDtRQUNuRCx1QkFBdUI7O2dCQUMzQixPQUFPLHVCQUF1QixDQUFDLElBQUksQ0FBQyxDQUFDO1lBQ3ZDLENBQUM7U0FBQTs7SUF6QkQsdUVBQXVFO0lBQ2hFLDBCQUFZLEdBQUcsVUFBVSxDQUFDO0lBeUJuQyxvQkFBQztLQUFBO1NBM0JZLGFBQWE7QUE2QjFCLGlGQUFpRjtBQUNqRjtJQUFBLE1BQWEsbUJBQW9CLFNBQVEsZ0JBQWdCO1FBSXZEOzs7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cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtDb21wb25lbnRIYXJuZXNzLCBIYXJuZXNzUHJlZGljYXRlfSBmcm9tICdAYW5ndWxhci9jZGsvdGVzdGluZyc7XG5pbXBvcnQge1Jvd0hhcm5lc3NGaWx0ZXJzLCBDZWxsSGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vdGFibGUtaGFybmVzcy1maWx0ZXJzJztcbmltcG9ydCB7TWF0Q2VsbEhhcm5lc3MsIE1hdEhlYWRlckNlbGxIYXJuZXNzLCBNYXRGb290ZXJDZWxsSGFybmVzc30gZnJvbSAnLi9jZWxsLWhhcm5lc3MnO1xuXG4vKiogVGV4dCBleHRyYWN0ZWQgZnJvbSBhIHRhYmxlIHJvdyBvcmdhbml6ZWQgYnkgY29sdW1ucy4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgTWF0Um93SGFybmVzc0NvbHVtbnNUZXh0IHtcbiAgW2NvbHVtbk5hbWU6IHN0cmluZ106IHN0cmluZztcbn1cblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBBbmd1bGFyIE1hdGVyaWFsIHRhYmxlIHJvdy4gKi9cbmV4cG9ydCBjbGFzcyBNYXRSb3dIYXJuZXNzIGV4dGVuZHMgQ29tcG9uZW50SGFybmVzcyB7XG4gIC8qKiBUaGUgc2VsZWN0b3IgZm9yIHRoZSBob3N0IGVsZW1lbnQgb2YgYSBgTWF0Um93SGFybmVzc2AgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1yb3cnO1xuXG4gIC8qKlxuICAgKiBHZXRzIGEgYEhhcm5lc3NQcmVkaWNhdGVgIHRoYXQgY2FuIGJlIHVzZWQgdG8gc2VhcmNoIGZvciBhIHRhYmxlIHJvdyB3aXRoIHNwZWNpZmljIGF0dHJpYnV0ZXMuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIG5hcnJvd2luZyB0aGUgc2VhcmNoXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogUm93SGFybmVzc0ZpbHRlcnMgPSB7fSk6IEhhcm5lc3NQcmVkaWNhdGU8TWF0Um93SGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXRSb3dIYXJuZXNzLCBvcHRpb25zKTtcbiAgfVxuXG4gIC8qKiBHZXRzIGEgbGlzdCBvZiBgTWF0Q2VsbEhhcm5lc3NgIGZvciBhbGwgY2VsbHMgaW4gdGhlIHJvdy4gKi9cbiAgYXN5bmMgZ2V0Q2VsbHMoZmlsdGVyOiBDZWxsSGFybmVzc0ZpbHRlcnMgPSB7fSk6IFByb21pc2U8TWF0Q2VsbEhhcm5lc3NbXT4ge1xuICAgIHJldHVybiB0aGlzLmxvY2F0b3JGb3JBbGwoTWF0Q2VsbEhhcm5lc3Mud2l0aChmaWx0ZXIpKSgpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHRleHQgb2YgdGhlIGNlbGxzIGluIHRoZSByb3cuICovXG4gIGFzeW5jIGdldENlbGxUZXh0QnlJbmRleChmaWx0ZXI6IENlbGxIYXJuZXNzRmlsdGVycyA9IHt9KTogUHJvbWlzZTxzdHJpbmdbXT4ge1xuICAgIHJldHVybiBnZXRDZWxsVGV4dEJ5SW5kZXgodGhpcywgZmlsdGVyKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSB0ZXh0IGluc2lkZSB0aGUgcm93IG9yZ2FuaXplZCBieSBjb2x1bW5zLiAqL1xuICBhc3luYyBnZXRDZWxsVGV4dEJ5Q29sdW1uTmFtZSgpOiBQcm9taXNlPE1hdFJvd0hhcm5lc3NDb2x1bW5zVGV4dD4ge1xuICAgIHJldHVybiBnZXRDZWxsVGV4dEJ5Q29sdW1uTmFtZSh0aGlzKTtcbiAgfVxufVxuXG4vKiogSGFybmVzcyBmb3IgaW50ZXJhY3Rpbmcgd2l0aCBhIHN0YW5kYXJkIEFuZ3VsYXIgTWF0ZXJpYWwgdGFibGUgaGVhZGVyIHJvdy4gKi9cbmV4cG9ydCBjbGFzcyBNYXRIZWFkZXJSb3dIYXJuZXNzIGV4dGVuZHMgQ29tcG9uZW50SGFybmVzcyB7XG4gIC8qKiBUaGUgc2VsZWN0b3IgZm9yIHRoZSBob3N0IGVsZW1lbnQgb2YgYSBgTWF0SGVhZGVyUm93SGFybmVzc2AgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1oZWFkZXItcm93JztcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3JcbiAgICogYSB0YWJsZSBoZWFkZXIgcm93IHdpdGggc3BlY2lmaWMgYXR0cmlidXRlcy5cbiAgICogQHBhcmFtIG9wdGlvbnMgT3B0aW9ucyBmb3IgbmFycm93aW5nIHRoZSBzZWFyY2hcbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBSb3dIYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXRIZWFkZXJSb3dIYXJuZXNzPiB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdEhlYWRlclJvd0hhcm5lc3MsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqIEdldHMgYSBsaXN0IG9mIGBNYXRIZWFkZXJDZWxsSGFybmVzc2AgZm9yIGFsbCBjZWxscyBpbiB0aGUgcm93LiAqL1xuICBhc3luYyBnZXRDZWxscyhmaWx0ZXI6IENlbGxIYXJuZXNzRmlsdGVycyA9IHt9KTogUHJvbWlzZTxNYXRIZWFkZXJDZWxsSGFybmVzc1tdPiB7XG4gICAgcmV0dXJuIHRoaXMubG9jYXRvckZvckFsbChNYXRIZWFkZXJDZWxsSGFybmVzcy53aXRoKGZpbHRlcikpKCk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgdGV4dCBvZiB0aGUgY2VsbHMgaW4gdGhlIGhlYWRlciByb3cuICovXG4gIGFzeW5jIGdldENlbGxUZXh0QnlJbmRleChmaWx0ZXI6IENlbGxIYXJuZXNzRmlsdGVycyA9IHt9KTogUHJvbWlzZTxzdHJpbmdbXT4ge1xuICAgIHJldHVybiBnZXRDZWxsVGV4dEJ5SW5kZXgodGhpcywgZmlsdGVyKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSB0ZXh0IGluc2lkZSB0aGUgaGVhZGVyIHJvdyBvcmdhbml6ZWQgYnkgY29sdW1ucy4gKi9cbiAgYXN5bmMgZ2V0Q2VsbFRleHRCeUNvbHVtbk5hbWUoKTogUHJvbWlzZTxNYXRSb3dIYXJuZXNzQ29sdW1uc1RleHQ+IHtcbiAgICByZXR1cm4gZ2V0Q2VsbFRleHRCeUNvbHVtbk5hbWUodGhpcyk7XG4gIH1cbn1cblxuXG4vKiogSGFybmVzcyBmb3IgaW50ZXJhY3Rpbmcgd2l0aCBhIHN0YW5kYXJkIEFuZ3VsYXIgTWF0ZXJpYWwgdGFibGUgZm9vdGVyIHJvdy4gKi9cbmV4cG9ydCBjbGFzcyBNYXRGb290ZXJSb3dIYXJuZXNzIGV4dGVuZHMgQ29tcG9uZW50SGFybmVzcyB7XG4gIC8qKiBUaGUgc2VsZWN0b3IgZm9yIHRoZSBob3N0IGVsZW1lbnQgb2YgYSBgTWF0Rm9vdGVyUm93SGFybmVzc2AgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1mb290ZXItcm93JztcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3JcbiAgICogYSB0YWJsZSBmb290ZXIgcm93IGNlbGwgd2l0aCBzcGVjaWZpYyBhdHRyaWJ1dGVzLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBuYXJyb3dpbmcgdGhlIHNlYXJjaFxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IFJvd0hhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdEZvb3RlclJvd0hhcm5lc3M+IHtcbiAgICByZXR1cm4gbmV3IEhhcm5lc3NQcmVkaWNhdGUoTWF0Rm9vdGVyUm93SGFybmVzcywgb3B0aW9ucyk7XG4gIH1cblxuICAvKiogR2V0cyBhIGxpc3Qgb2YgYE1hdEZvb3RlckNlbGxIYXJuZXNzYCBmb3IgYWxsIGNlbGxzIGluIHRoZSByb3cuICovXG4gIGFzeW5jIGdldENlbGxzKGZpbHRlcjogQ2VsbEhhcm5lc3NGaWx0ZXJzID0ge30pOiBQcm9taXNlPE1hdEZvb3RlckNlbGxIYXJuZXNzW10+IHtcbiAgICByZXR1cm4gdGhpcy5sb2NhdG9yRm9yQWxsKE1hdEZvb3RlckNlbGxIYXJuZXNzLndpdGgoZmlsdGVyKSkoKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSB0ZXh0IG9mIHRoZSBjZWxscyBpbiB0aGUgZm9vdGVyIHJvdy4gKi9cbiAgYXN5bmMgZ2V0Q2VsbFRleHRCeUluZGV4KGZpbHRlcjogQ2VsbEhhcm5lc3NGaWx0ZXJzID0ge30pOiBQcm9taXNlPHN0cmluZ1tdPiB7XG4gICAgcmV0dXJuIGdldENlbGxUZXh0QnlJbmRleCh0aGlzLCBmaWx0ZXIpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHRleHQgaW5zaWRlIHRoZSBmb290ZXIgcm93IG9yZ2FuaXplZCBieSBjb2x1bW5zLiAqL1xuICBhc3luYyBnZXRDZWxsVGV4dEJ5Q29sdW1uTmFtZSgpOiBQcm9taXNlPE1hdFJvd0hhcm5lc3NDb2x1bW5zVGV4dD4ge1xuICAgIHJldHVybiBnZXRDZWxsVGV4dEJ5Q29sdW1uTmFtZSh0aGlzKTtcbiAgfVxufVxuXG5cbmFzeW5jIGZ1bmN0aW9uIGdldENlbGxUZXh0QnlJbmRleChoYXJuZXNzOiB7XG4gIGdldENlbGxzOiAoZmlsdGVyPzogQ2VsbEhhcm5lc3NGaWx0ZXJzKSA9PiBQcm9taXNlPE1hdENlbGxIYXJuZXNzW10+XG59LCBmaWx0ZXI6IENlbGxIYXJuZXNzRmlsdGVycyk6IFByb21pc2U8c3RyaW5nW10+IHtcbiAgY29uc3QgY2VsbHMgPSBhd2FpdCBoYXJuZXNzLmdldENlbGxzKGZpbHRlcik7XG4gIHJldHVybiBQcm9taXNlLmFsbChjZWxscy5tYXAoY2VsbCA9PiBjZWxsLmdldFRleHQoKSkpO1xufVxuXG5hc3luYyBmdW5jdGlvbiBnZXRDZWxsVGV4dEJ5Q29sdW1uTmFtZShoYXJuZXNzOiB7XG4gIGdldENlbGxzOiAoKSA9PiBQcm9taXNlPE1hdENlbGxIYXJuZXNzW10+XG59KTogUHJvbWlzZTxNYXRSb3dIYXJuZXNzQ29sdW1uc1RleHQ+IHtcbiAgY29uc3Qgb3V0cHV0OiBNYXRSb3dIYXJuZXNzQ29sdW1uc1RleHQgPSB7fTtcbiAgY29uc3QgY2VsbHMgPSBhd2FpdCBoYXJuZXNzLmdldENlbGxzKCk7XG4gIGNvbnN0IGNlbGxzRGF0YSA9IGF3YWl0IFByb21pc2UuYWxsKGNlbGxzLm1hcChjZWxsID0+IHtcbiAgICByZXR1cm4gUHJvbWlzZS5hbGwoW2NlbGwuZ2V0Q29sdW1uTmFtZSgpLCBjZWxsLmdldFRleHQoKV0pO1xuICB9KSk7XG4gIGNlbGxzRGF0YS5mb3JFYWNoKChbY29sdW1uTmFtZSwgdGV4dF0pID0+IG91dHB1dFtjb2x1bW5OYW1lXSA9IHRleHQpO1xuICByZXR1cm4gb3V0cHV0O1xufVxuIl19