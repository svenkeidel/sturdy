/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MatOptionHarness } from './option-harness';
/** Harness for interacting with a `mat-optgroup` in tests. */
let MatOptgroupHarness = /** @class */ (() => {
    class MatOptgroupHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._label = this.locatorFor('.mat-optgroup-label');
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatOptgroupHarness` that meets
         * certain criteria.
         * @param options Options for filtering which option instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatOptgroupHarness, options)
                .addOption('labelText', options.labelText, (harness, title) => __awaiter(this, void 0, void 0, function* () { return HarnessPredicate.stringMatches(yield harness.getLabelText(), title); }));
        }
        /** Gets the option group's label text. */
        getLabelText() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._label()).text();
            });
        }
        /** Gets whether the option group is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-optgroup-disabled');
            });
        }
        /**
         * Gets the options that are inside the group.
         * @param filter Optionally filters which options are included.
         */
        getOptions(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatOptionHarness.with(filter))();
            });
        }
    }
    /** Selector used to locate option group instances. */
    MatOptgroupHarness.hostSelector = '.mat-optgroup';
    return MatOptgroupHarness;
})();
export { MatOptgroupHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoib3B0Z3JvdXAtaGFybmVzcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9jb3JlL3Rlc3Rpbmcvb3B0Z3JvdXAtaGFybmVzcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7O0FBRUgsT0FBTyxFQUFDLGdCQUFnQixFQUFFLGdCQUFnQixFQUFDLE1BQU0sc0JBQXNCLENBQUM7QUFFeEUsT0FBTyxFQUFDLGdCQUFnQixFQUFDLE1BQU0sa0JBQWtCLENBQUM7QUFHbEQsOERBQThEO0FBQzlEO0lBQUEsTUFBYSxrQkFBbUIsU0FBUSxnQkFBZ0I7UUFBeEQ7O1lBR1UsV0FBTSxHQUFHLElBQUksQ0FBQyxVQUFVLENBQUMscUJBQXFCLENBQUMsQ0FBQztRQWdDMUQsQ0FBQztRQTlCQzs7Ozs7V0FLRztRQUNILE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBa0MsRUFBRTtZQUM5QyxPQUFPLElBQUksZ0JBQWdCLENBQUMsa0JBQWtCLEVBQUUsT0FBTyxDQUFDO2lCQUNuRCxTQUFTLENBQUMsV0FBVyxFQUFFLE9BQU8sQ0FBQyxTQUFTLEVBQ3JDLENBQU8sT0FBTyxFQUFFLEtBQUssRUFBRSxFQUFFLGdEQUNyQixPQUFBLGdCQUFnQixDQUFDLGFBQWEsQ0FBQyxNQUFNLE9BQU8sQ0FBQyxZQUFZLEVBQUUsRUFBRSxLQUFLLENBQUMsQ0FBQSxHQUFBLENBQUMsQ0FBQztRQUNuRixDQUFDO1FBRUQsMENBQTBDO1FBQ3BDLFlBQVk7O2dCQUNoQixPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUMsQ0FBQyxJQUFJLEVBQUUsQ0FBQztZQUN0QyxDQUFDO1NBQUE7UUFFRCxpREFBaUQ7UUFDM0MsVUFBVTs7Z0JBQ2QsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsUUFBUSxDQUFDLHVCQUF1QixDQUFDLENBQUM7WUFDL0QsQ0FBQztTQUFBO1FBRUQ7OztXQUdHO1FBQ0csVUFBVSxDQUFDLFNBQStCLEVBQUU7O2dCQUNoRCxPQUFPLElBQUksQ0FBQyxhQUFhLENBQUMsZ0JBQWdCLENBQUMsSUFBSSxDQUFDLE1BQU0sQ0FBQyxDQUFDLEVBQUUsQ0FBQztZQUM3RCxDQUFDO1NBQUE7O0lBakNELHNEQUFzRDtJQUMvQywrQkFBWSxHQUFHLGVBQWUsQ0FBQztJQWlDeEMseUJBQUM7S0FBQTtTQW5DWSxrQkFBa0IiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtDb21wb25lbnRIYXJuZXNzLCBIYXJuZXNzUHJlZGljYXRlfSBmcm9tICdAYW5ndWxhci9jZGsvdGVzdGluZyc7XG5pbXBvcnQge09wdGdyb3VwSGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vb3B0Z3JvdXAtaGFybmVzcy1maWx0ZXJzJztcbmltcG9ydCB7TWF0T3B0aW9uSGFybmVzc30gZnJvbSAnLi9vcHRpb24taGFybmVzcyc7XG5pbXBvcnQge09wdGlvbkhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL29wdGlvbi1oYXJuZXNzLWZpbHRlcnMnO1xuXG4vKiogSGFybmVzcyBmb3IgaW50ZXJhY3Rpbmcgd2l0aCBhIGBtYXQtb3B0Z3JvdXBgIGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdE9wdGdyb3VwSGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICAvKiogU2VsZWN0b3IgdXNlZCB0byBsb2NhdGUgb3B0aW9uIGdyb3VwIGluc3RhbmNlcy4gKi9cbiAgc3RhdGljIGhvc3RTZWxlY3RvciA9ICcubWF0LW9wdGdyb3VwJztcbiAgcHJpdmF0ZSBfbGFiZWwgPSB0aGlzLmxvY2F0b3JGb3IoJy5tYXQtb3B0Z3JvdXAtbGFiZWwnKTtcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3IgYSBgTWF0T3B0Z3JvdXBIYXJuZXNzYCB0aGF0IG1lZXRzXG4gICAqIGNlcnRhaW4gY3JpdGVyaWEuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIGZpbHRlcmluZyB3aGljaCBvcHRpb24gaW5zdGFuY2VzIGFyZSBjb25zaWRlcmVkIGEgbWF0Y2guXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogT3B0Z3JvdXBIYXJuZXNzRmlsdGVycyA9IHt9KSB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdE9wdGdyb3VwSGFybmVzcywgb3B0aW9ucylcbiAgICAgICAgLmFkZE9wdGlvbignbGFiZWxUZXh0Jywgb3B0aW9ucy5sYWJlbFRleHQsXG4gICAgICAgICAgICBhc3luYyAoaGFybmVzcywgdGl0bGUpID0+XG4gICAgICAgICAgICAgICAgSGFybmVzc1ByZWRpY2F0ZS5zdHJpbmdNYXRjaGVzKGF3YWl0IGhhcm5lc3MuZ2V0TGFiZWxUZXh0KCksIHRpdGxlKSk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgb3B0aW9uIGdyb3VwJ3MgbGFiZWwgdGV4dC4gKi9cbiAgYXN5bmMgZ2V0TGFiZWxUZXh0KCk6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9sYWJlbCgpKS50ZXh0KCk7XG4gIH1cblxuICAvKiogR2V0cyB3aGV0aGVyIHRoZSBvcHRpb24gZ3JvdXAgaXMgZGlzYWJsZWQuICovXG4gIGFzeW5jIGlzRGlzYWJsZWQoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuaGFzQ2xhc3MoJ21hdC1vcHRncm91cC1kaXNhYmxlZCcpO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgdGhlIG9wdGlvbnMgdGhhdCBhcmUgaW5zaWRlIHRoZSBncm91cC5cbiAgICogQHBhcmFtIGZpbHRlciBPcHRpb25hbGx5IGZpbHRlcnMgd2hpY2ggb3B0aW9ucyBhcmUgaW5jbHVkZWQuXG4gICAqL1xuICBhc3luYyBnZXRPcHRpb25zKGZpbHRlcjogT3B0aW9uSGFybmVzc0ZpbHRlcnMgPSB7fSk6IFByb21pc2U8TWF0T3B0aW9uSGFybmVzc1tdPiB7XG4gICAgcmV0dXJuIHRoaXMubG9jYXRvckZvckFsbChNYXRPcHRpb25IYXJuZXNzLndpdGgoZmlsdGVyKSkoKTtcbiAgfVxufVxuXG4iXX0=