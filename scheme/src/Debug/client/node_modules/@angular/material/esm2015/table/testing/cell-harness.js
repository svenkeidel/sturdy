/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate, } from '@angular/cdk/testing';
/** Harness for interacting with a standard Angular Material table cell. */
let MatCellHarness = /** @class */ (() => {
    class MatCellHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a table cell with specific attributes.
         * @param options Options for narrowing the search
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return getCellPredicate(MatCellHarness, options);
        }
        /** Gets the cell's text. */
        getText() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).text();
            });
        }
        /** Gets the name of the column that the cell belongs to. */
        getColumnName() {
            return __awaiter(this, void 0, void 0, function* () {
                const host = yield this.host();
                const classAttribute = yield host.getAttribute('class');
                if (classAttribute) {
                    const prefix = 'mat-column-';
                    const name = classAttribute.split(' ').map(c => c.trim()).find(c => c.startsWith(prefix));
                    if (name) {
                        return name.split(prefix)[1];
                    }
                }
                throw Error('Could not determine column name of cell.');
            });
        }
    }
    /** The selector for the host element of a `MatCellHarness` instance. */
    MatCellHarness.hostSelector = '.mat-cell';
    return MatCellHarness;
})();
export { MatCellHarness };
/** Harness for interacting with a standard Angular Material table header cell. */
let MatHeaderCellHarness = /** @class */ (() => {
    class MatHeaderCellHarness extends MatCellHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for
         * a table header cell with specific attributes.
         * @param options Options for narrowing the search
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return getCellPredicate(MatHeaderCellHarness, options);
        }
    }
    /** The selector for the host element of a `MatHeaderCellHarness` instance. */
    MatHeaderCellHarness.hostSelector = '.mat-header-cell';
    return MatHeaderCellHarness;
})();
export { MatHeaderCellHarness };
/** Harness for interacting with a standard Angular Material table footer cell. */
let MatFooterCellHarness = /** @class */ (() => {
    class MatFooterCellHarness extends MatCellHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for
         * a table footer cell with specific attributes.
         * @param options Options for narrowing the search
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return getCellPredicate(MatFooterCellHarness, options);
        }
    }
    /** The selector for the host element of a `MatFooterCellHarness` instance. */
    MatFooterCellHarness.hostSelector = '.mat-footer-cell';
    return MatFooterCellHarness;
})();
export { MatFooterCellHarness };
function getCellPredicate(type, options) {
    return new HarnessPredicate(type, options)
        .addOption('text', options.text, (harness, text) => HarnessPredicate.stringMatches(harness.getText(), text))
        .addOption('columnName', options.columnName, (harness, name) => HarnessPredicate.stringMatches(harness.getColumnName(), name));
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2VsbC1oYXJuZXNzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL3RhYmxlL3Rlc3RpbmcvY2VsbC1oYXJuZXNzLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7QUFFSCxPQUFPLEVBQ0wsZ0JBQWdCLEVBQ2hCLGdCQUFnQixHQUVqQixNQUFNLHNCQUFzQixDQUFDO0FBRzlCLDJFQUEyRTtBQUMzRTtJQUFBLE1BQWEsY0FBZSxTQUFRLGdCQUFnQjtRQUlsRDs7OztXQUlHO1FBQ0gsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUE4QixFQUFFO1lBQzFDLE9BQU8sZ0JBQWdCLENBQUMsY0FBYyxFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQ25ELENBQUM7UUFFRCw0QkFBNEI7UUFDdEIsT0FBTzs7Z0JBQ1gsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsSUFBSSxFQUFFLENBQUM7WUFDcEMsQ0FBQztTQUFBO1FBRUQsNERBQTREO1FBQ3RELGFBQWE7O2dCQUNqQixNQUFNLElBQUksR0FBRyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQztnQkFDL0IsTUFBTSxjQUFjLEdBQUcsTUFBTSxJQUFJLENBQUMsWUFBWSxDQUFDLE9BQU8sQ0FBQyxDQUFDO2dCQUV4RCxJQUFJLGNBQWMsRUFBRTtvQkFDbEIsTUFBTSxNQUFNLEdBQUcsYUFBYSxDQUFDO29CQUM3QixNQUFNLElBQUksR0FBRyxjQUFjLENBQUMsS0FBSyxDQUFDLEdBQUcsQ0FBQyxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQyxVQUFVLENBQUMsTUFBTSxDQUFDLENBQUMsQ0FBQztvQkFFMUYsSUFBSSxJQUFJLEVBQUU7d0JBQ1IsT0FBTyxJQUFJLENBQUMsS0FBSyxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDO3FCQUM5QjtpQkFDRjtnQkFFRCxNQUFNLEtBQUssQ0FBQywwQ0FBMEMsQ0FBQyxDQUFDO1lBQzFELENBQUM7U0FBQTs7SUFoQ0Qsd0VBQXdFO0lBQ2pFLDJCQUFZLEdBQUcsV0FBVyxDQUFDO0lBZ0NwQyxxQkFBQztLQUFBO1NBbENZLGNBQWM7QUFvQzNCLGtGQUFrRjtBQUNsRjtJQUFBLE1BQWEsb0JBQXFCLFNBQVEsY0FBYztRQUl0RDs7Ozs7V0FLRztRQUNILE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBOEIsRUFBRTtZQUMxQyxPQUFPLGdCQUFnQixDQUFDLG9CQUFvQixFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQ3pELENBQUM7O0lBWEQsOEVBQThFO0lBQ3ZFLGlDQUFZLEdBQUcsa0JBQWtCLENBQUM7SUFXM0MsMkJBQUM7S0FBQTtTQWJZLG9CQUFvQjtBQWVqQyxrRkFBa0Y7QUFDbEY7SUFBQSxNQUFhLG9CQUFxQixTQUFRLGNBQWM7UUFJdEQ7Ozs7O1dBS0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQThCLEVBQUU7WUFDMUMsT0FBTyxnQkFBZ0IsQ0FBQyxvQkFBb0IsRUFBRSxPQUFPLENBQUMsQ0FBQztRQUN6RCxDQUFDOztJQVhELDhFQUE4RTtJQUN2RSxpQ0FBWSxHQUFHLGtCQUFrQixDQUFDO0lBVzNDLDJCQUFDO0tBQUE7U0FiWSxvQkFBb0I7QUFnQmpDLFNBQVMsZ0JBQWdCLENBQ3ZCLElBQW9DLEVBQ3BDLE9BQTJCO0lBQzNCLE9BQU8sSUFBSSxnQkFBZ0IsQ0FBQyxJQUFJLEVBQUUsT0FBTyxDQUFDO1NBQ3ZDLFNBQVMsQ0FBQyxNQUFNLEVBQUUsT0FBTyxDQUFDLElBQUksRUFDM0IsQ0FBQyxPQUFPLEVBQUUsSUFBSSxFQUFFLEVBQUUsQ0FBQyxnQkFBZ0IsQ0FBQyxhQUFhLENBQUMsT0FBTyxDQUFDLE9BQU8sRUFBRSxFQUFFLElBQUksQ0FBQyxDQUFDO1NBQzlFLFNBQVMsQ0FBQyxZQUFZLEVBQUUsT0FBTyxDQUFDLFVBQVUsRUFDdkMsQ0FBQyxPQUFPLEVBQUUsSUFBSSxFQUFFLEVBQUUsQ0FBQyxnQkFBZ0IsQ0FBQyxhQUFhLENBQUMsT0FBTyxDQUFDLGFBQWEsRUFBRSxFQUFFLElBQUksQ0FBQyxDQUFDLENBQUM7QUFDMUYsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge1xuICBDb21wb25lbnRIYXJuZXNzLFxuICBIYXJuZXNzUHJlZGljYXRlLFxuICBDb21wb25lbnRIYXJuZXNzQ29uc3RydWN0b3IsXG59IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7Q2VsbEhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL3RhYmxlLWhhcm5lc3MtZmlsdGVycyc7XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgQW5ndWxhciBNYXRlcmlhbCB0YWJsZSBjZWxsLiAqL1xuZXhwb3J0IGNsYXNzIE1hdENlbGxIYXJuZXNzIGV4dGVuZHMgQ29tcG9uZW50SGFybmVzcyB7XG4gIC8qKiBUaGUgc2VsZWN0b3IgZm9yIHRoZSBob3N0IGVsZW1lbnQgb2YgYSBgTWF0Q2VsbEhhcm5lc3NgIGluc3RhbmNlLiAqL1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJy5tYXQtY2VsbCc7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgdGFibGUgY2VsbCB3aXRoIHNwZWNpZmljIGF0dHJpYnV0ZXMuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIG5hcnJvd2luZyB0aGUgc2VhcmNoXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogQ2VsbEhhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdENlbGxIYXJuZXNzPiB7XG4gICAgcmV0dXJuIGdldENlbGxQcmVkaWNhdGUoTWF0Q2VsbEhhcm5lc3MsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIGNlbGwncyB0ZXh0LiAqL1xuICBhc3luYyBnZXRUZXh0KCk6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkudGV4dCgpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIG5hbWUgb2YgdGhlIGNvbHVtbiB0aGF0IHRoZSBjZWxsIGJlbG9uZ3MgdG8uICovXG4gIGFzeW5jIGdldENvbHVtbk5hbWUoKTogUHJvbWlzZTxzdHJpbmc+IHtcbiAgICBjb25zdCBob3N0ID0gYXdhaXQgdGhpcy5ob3N0KCk7XG4gICAgY29uc3QgY2xhc3NBdHRyaWJ1dGUgPSBhd2FpdCBob3N0LmdldEF0dHJpYnV0ZSgnY2xhc3MnKTtcblxuICAgIGlmIChjbGFzc0F0dHJpYnV0ZSkge1xuICAgICAgY29uc3QgcHJlZml4ID0gJ21hdC1jb2x1bW4tJztcbiAgICAgIGNvbnN0IG5hbWUgPSBjbGFzc0F0dHJpYnV0ZS5zcGxpdCgnICcpLm1hcChjID0+IGMudHJpbSgpKS5maW5kKGMgPT4gYy5zdGFydHNXaXRoKHByZWZpeCkpO1xuXG4gICAgICBpZiAobmFtZSkge1xuICAgICAgICByZXR1cm4gbmFtZS5zcGxpdChwcmVmaXgpWzFdO1xuICAgICAgfVxuICAgIH1cblxuICAgIHRocm93IEVycm9yKCdDb3VsZCBub3QgZGV0ZXJtaW5lIGNvbHVtbiBuYW1lIG9mIGNlbGwuJyk7XG4gIH1cbn1cblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBBbmd1bGFyIE1hdGVyaWFsIHRhYmxlIGhlYWRlciBjZWxsLiAqL1xuZXhwb3J0IGNsYXNzIE1hdEhlYWRlckNlbGxIYXJuZXNzIGV4dGVuZHMgTWF0Q2VsbEhhcm5lc3Mge1xuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdEhlYWRlckNlbGxIYXJuZXNzYCBpbnN0YW5jZS4gKi9cbiAgc3RhdGljIGhvc3RTZWxlY3RvciA9ICcubWF0LWhlYWRlci1jZWxsJztcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3JcbiAgICogYSB0YWJsZSBoZWFkZXIgY2VsbCB3aXRoIHNwZWNpZmljIGF0dHJpYnV0ZXMuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIG5hcnJvd2luZyB0aGUgc2VhcmNoXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogQ2VsbEhhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdEhlYWRlckNlbGxIYXJuZXNzPiB7XG4gICAgcmV0dXJuIGdldENlbGxQcmVkaWNhdGUoTWF0SGVhZGVyQ2VsbEhhcm5lc3MsIG9wdGlvbnMpO1xuICB9XG59XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgQW5ndWxhciBNYXRlcmlhbCB0YWJsZSBmb290ZXIgY2VsbC4gKi9cbmV4cG9ydCBjbGFzcyBNYXRGb290ZXJDZWxsSGFybmVzcyBleHRlbmRzIE1hdENlbGxIYXJuZXNzIHtcbiAgLyoqIFRoZSBzZWxlY3RvciBmb3IgdGhlIGhvc3QgZWxlbWVudCBvZiBhIGBNYXRGb290ZXJDZWxsSGFybmVzc2AgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1mb290ZXItY2VsbCc7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yXG4gICAqIGEgdGFibGUgZm9vdGVyIGNlbGwgd2l0aCBzcGVjaWZpYyBhdHRyaWJ1dGVzLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBuYXJyb3dpbmcgdGhlIHNlYXJjaFxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IENlbGxIYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXRGb290ZXJDZWxsSGFybmVzcz4ge1xuICAgIHJldHVybiBnZXRDZWxsUHJlZGljYXRlKE1hdEZvb3RlckNlbGxIYXJuZXNzLCBvcHRpb25zKTtcbiAgfVxufVxuXG5cbmZ1bmN0aW9uIGdldENlbGxQcmVkaWNhdGU8VCBleHRlbmRzIE1hdENlbGxIYXJuZXNzPihcbiAgdHlwZTogQ29tcG9uZW50SGFybmVzc0NvbnN0cnVjdG9yPFQ+LFxuICBvcHRpb25zOiBDZWxsSGFybmVzc0ZpbHRlcnMpOiBIYXJuZXNzUHJlZGljYXRlPFQ+IHtcbiAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKHR5cGUsIG9wdGlvbnMpXG4gICAgLmFkZE9wdGlvbigndGV4dCcsIG9wdGlvbnMudGV4dCxcbiAgICAgICAgKGhhcm5lc3MsIHRleHQpID0+IEhhcm5lc3NQcmVkaWNhdGUuc3RyaW5nTWF0Y2hlcyhoYXJuZXNzLmdldFRleHQoKSwgdGV4dCkpXG4gICAgLmFkZE9wdGlvbignY29sdW1uTmFtZScsIG9wdGlvbnMuY29sdW1uTmFtZSxcbiAgICAgICAgKGhhcm5lc3MsIG5hbWUpID0+IEhhcm5lc3NQcmVkaWNhdGUuc3RyaW5nTWF0Y2hlcyhoYXJuZXNzLmdldENvbHVtbk5hbWUoKSwgbmFtZSkpO1xufVxuIl19