/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate, TestKey } from '@angular/cdk/testing';
/**
 * Harness for interacting with a standard MatBottomSheet in tests.
 * @dynamic
 */
let MatBottomSheetHarness = /** @class */ (() => {
    class MatBottomSheetHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a bottom sheet with
         * specific attributes.
         * @param options Options for narrowing the search.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatBottomSheetHarness, options);
        }
        /** Gets the value of the bottom sheet's "aria-label" attribute. */
        getAriaLabel() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getAttribute('aria-label');
            });
        }
        /**
         * Dismisses the bottom sheet by pressing escape. Note that this method cannot
         * be used if "disableClose" has been set to true via the config.
         */
        dismiss() {
            return __awaiter(this, void 0, void 0, function* () {
                yield (yield this.host()).sendKeys(TestKey.ESCAPE);
            });
        }
    }
    // Developers can provide a custom component or template for the
    // bottom sheet. The canonical parent is the ".mat-bottom-sheet-container".
    MatBottomSheetHarness.hostSelector = '.mat-bottom-sheet-container';
    return MatBottomSheetHarness;
})();
export { MatBottomSheetHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYm90dG9tLXNoZWV0LWhhcm5lc3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvYm90dG9tLXNoZWV0L3Rlc3RpbmcvYm90dG9tLXNoZWV0LWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBRSxPQUFPLEVBQUMsTUFBTSxzQkFBc0IsQ0FBQztBQUdqRjs7O0dBR0c7QUFDSDtJQUFBLE1BQWEscUJBQXNCLFNBQVEsZ0JBQWdCO1FBS3pEOzs7OztXQUtHO1FBQ0gsTUFBTSxDQUFDLElBQUksQ0FBQyxVQUFxQyxFQUFFO1lBQ2pELE9BQU8sSUFBSSxnQkFBZ0IsQ0FBQyxxQkFBcUIsRUFBRSxPQUFPLENBQUMsQ0FBQztRQUM5RCxDQUFDO1FBRUQsbUVBQW1FO1FBQzdELFlBQVk7O2dCQUNoQixPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxZQUFZLENBQUMsWUFBWSxDQUFDLENBQUM7WUFDeEQsQ0FBQztTQUFBO1FBRUQ7OztXQUdHO1FBQ0csT0FBTzs7Z0JBQ1gsTUFBTSxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FBQyxNQUFNLENBQUMsQ0FBQztZQUNyRCxDQUFDO1NBQUE7O0lBekJELGdFQUFnRTtJQUNoRSwyRUFBMkU7SUFDcEUsa0NBQVksR0FBRyw2QkFBNkIsQ0FBQztJQXdCdEQsNEJBQUM7S0FBQTtTQTNCWSxxQkFBcUIiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtDb21wb25lbnRIYXJuZXNzLCBIYXJuZXNzUHJlZGljYXRlLCBUZXN0S2V5fSBmcm9tICdAYW5ndWxhci9jZGsvdGVzdGluZyc7XG5pbXBvcnQge0JvdHRvbVNoZWV0SGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vYm90dG9tLXNoZWV0LWhhcm5lc3MtZmlsdGVycyc7XG5cbi8qKlxuICogSGFybmVzcyBmb3IgaW50ZXJhY3Rpbmcgd2l0aCBhIHN0YW5kYXJkIE1hdEJvdHRvbVNoZWV0IGluIHRlc3RzLlxuICogQGR5bmFtaWNcbiAqL1xuZXhwb3J0IGNsYXNzIE1hdEJvdHRvbVNoZWV0SGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICAvLyBEZXZlbG9wZXJzIGNhbiBwcm92aWRlIGEgY3VzdG9tIGNvbXBvbmVudCBvciB0ZW1wbGF0ZSBmb3IgdGhlXG4gIC8vIGJvdHRvbSBzaGVldC4gVGhlIGNhbm9uaWNhbCBwYXJlbnQgaXMgdGhlIFwiLm1hdC1ib3R0b20tc2hlZXQtY29udGFpbmVyXCIuXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1ib3R0b20tc2hlZXQtY29udGFpbmVyJztcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3IgYSBib3R0b20gc2hlZXQgd2l0aFxuICAgKiBzcGVjaWZpYyBhdHRyaWJ1dGVzLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBuYXJyb3dpbmcgdGhlIHNlYXJjaC5cbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBCb3R0b21TaGVldEhhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdEJvdHRvbVNoZWV0SGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXRCb3R0b21TaGVldEhhcm5lc3MsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHZhbHVlIG9mIHRoZSBib3R0b20gc2hlZXQncyBcImFyaWEtbGFiZWxcIiBhdHRyaWJ1dGUuICovXG4gIGFzeW5jIGdldEFyaWFMYWJlbCgpOiBQcm9taXNlPHN0cmluZ3xudWxsPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuZ2V0QXR0cmlidXRlKCdhcmlhLWxhYmVsJyk7XG4gIH1cblxuICAvKipcbiAgICogRGlzbWlzc2VzIHRoZSBib3R0b20gc2hlZXQgYnkgcHJlc3NpbmcgZXNjYXBlLiBOb3RlIHRoYXQgdGhpcyBtZXRob2QgY2Fubm90XG4gICAqIGJlIHVzZWQgaWYgXCJkaXNhYmxlQ2xvc2VcIiBoYXMgYmVlbiBzZXQgdG8gdHJ1ZSB2aWEgdGhlIGNvbmZpZy5cbiAgICovXG4gIGFzeW5jIGRpc21pc3MoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgYXdhaXQgKGF3YWl0IHRoaXMuaG9zdCgpKS5zZW5kS2V5cyhUZXN0S2V5LkVTQ0FQRSk7XG4gIH1cbn1cbiJdfQ==