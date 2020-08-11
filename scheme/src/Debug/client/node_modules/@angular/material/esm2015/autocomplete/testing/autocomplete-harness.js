/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { coerceBooleanProperty } from '@angular/cdk/coercion';
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MatOptgroupHarness, MatOptionHarness } from '@angular/material/core/testing';
/** Harness for interacting with a standard mat-autocomplete in tests. */
let MatAutocompleteHarness = /** @class */ (() => {
    class MatAutocompleteHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._documentRootLocator = this.documentRootLocatorFactory();
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatAutocompleteHarness` that meets
         * certain criteria.
         * @param options Options for filtering which autocomplete instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatAutocompleteHarness, options)
                .addOption('value', options.value, (harness, value) => HarnessPredicate.stringMatches(harness.getValue(), value));
        }
        /** Gets the value of the autocomplete input. */
        getValue() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getProperty('value');
            });
        }
        /** Whether the autocomplete input is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                const disabled = (yield this.host()).getAttribute('disabled');
                return coerceBooleanProperty(yield disabled);
            });
        }
        /** Focuses the autocomplete input. */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).focus();
            });
        }
        /** Blurs the autocomplete input. */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).blur();
            });
        }
        /** Enters text into the autocomplete. */
        enterText(value) {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).sendKeys(value);
            });
        }
        /** Gets the options inside the autocomplete panel. */
        getOptions(filters = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this._documentRootLocator.locatorForAll(MatOptionHarness.with(Object.assign(Object.assign({}, filters), { ancestor: yield this._getPanelSelector() })))();
            });
        }
        /** Gets the option groups inside the autocomplete panel. */
        getOptionGroups(filters = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this._documentRootLocator.locatorForAll(MatOptgroupHarness.with(Object.assign(Object.assign({}, filters), { ancestor: yield this._getPanelSelector() })))();
            });
        }
        /** Selects the first option matching the given filters. */
        selectOption(filters) {
            return __awaiter(this, void 0, void 0, function* () {
                yield this.focus(); // Focus the input to make sure the autocomplete panel is shown.
                const options = yield this.getOptions(filters);
                if (!options.length) {
                    throw Error(`Could not find a mat-option matching ${JSON.stringify(filters)}`);
                }
                yield options[0].click();
            });
        }
        /** Whether the autocomplete is open. */
        isOpen() {
            return __awaiter(this, void 0, void 0, function* () {
                const panel = yield this._getPanel();
                return !!panel && (yield panel.hasClass('mat-autocomplete-visible'));
            });
        }
        /** Gets the panel associated with this autocomplete trigger. */
        _getPanel() {
            return __awaiter(this, void 0, void 0, function* () {
                // Technically this is static, but it needs to be in a
                // function, because the autocomplete's panel ID can changed.
                return this._documentRootLocator.locatorForOptional(yield this._getPanelSelector())();
            });
        }
        /** Gets the selector that can be used to find the autocomplete trigger's panel. */
        _getPanelSelector() {
            return __awaiter(this, void 0, void 0, function* () {
                return `#${(yield (yield this.host()).getAttribute('aria-owns'))}`;
            });
        }
    }
    /** The selector for the host element of a `MatAutocomplete` instance. */
    MatAutocompleteHarness.hostSelector = '.mat-autocomplete-trigger';
    return MatAutocompleteHarness;
})();
export { MatAutocompleteHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYXV0b2NvbXBsZXRlLWhhcm5lc3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvYXV0b2NvbXBsZXRlL3Rlc3RpbmcvYXV0b2NvbXBsZXRlLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxxQkFBcUIsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQzVELE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBQ3hFLE9BQU8sRUFDTCxrQkFBa0IsRUFDbEIsZ0JBQWdCLEVBR2pCLE1BQU0sZ0NBQWdDLENBQUM7QUFHeEMseUVBQXlFO0FBQ3pFO0lBQUEsTUFBYSxzQkFBdUIsU0FBUSxnQkFBZ0I7UUFBNUQ7O1lBQ1UseUJBQW9CLEdBQUcsSUFBSSxDQUFDLDBCQUEwQixFQUFFLENBQUM7UUF3Rm5FLENBQUM7UUFuRkM7Ozs7O1dBS0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQXNDLEVBQUU7WUFDbEQsT0FBTyxJQUFJLGdCQUFnQixDQUFDLHNCQUFzQixFQUFFLE9BQU8sQ0FBQztpQkFDdkQsU0FBUyxDQUFDLE9BQU8sRUFBRSxPQUFPLENBQUMsS0FBSyxFQUM3QixDQUFDLE9BQU8sRUFBRSxLQUFLLEVBQUUsRUFBRSxDQUFDLGdCQUFnQixDQUFDLGFBQWEsQ0FBQyxPQUFPLENBQUMsUUFBUSxFQUFFLEVBQUUsS0FBSyxDQUFDLENBQUMsQ0FBQztRQUN6RixDQUFDO1FBRUQsZ0RBQWdEO1FBQzFDLFFBQVE7O2dCQUNaLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFdBQVcsQ0FBQyxPQUFPLENBQUMsQ0FBQztZQUNsRCxDQUFDO1NBQUE7UUFFRCxrREFBa0Q7UUFDNUMsVUFBVTs7Z0JBQ2QsTUFBTSxRQUFRLEdBQUcsQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFlBQVksQ0FBQyxVQUFVLENBQUMsQ0FBQztnQkFDOUQsT0FBTyxxQkFBcUIsQ0FBQyxNQUFNLFFBQVEsQ0FBQyxDQUFDO1lBQy9DLENBQUM7U0FBQTtRQUVELHNDQUFzQztRQUNoQyxLQUFLOztnQkFDVCxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxLQUFLLEVBQUUsQ0FBQztZQUNyQyxDQUFDO1NBQUE7UUFFRCxvQ0FBb0M7UUFDOUIsSUFBSTs7Z0JBQ1IsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsSUFBSSxFQUFFLENBQUM7WUFDcEMsQ0FBQztTQUFBO1FBRUQseUNBQXlDO1FBQ25DLFNBQVMsQ0FBQyxLQUFhOztnQkFDM0IsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxDQUFDO1lBQzdDLENBQUM7U0FBQTtRQUVELHNEQUFzRDtRQUNoRCxVQUFVLENBQUMsVUFBa0QsRUFBRTs7Z0JBRW5FLE9BQU8sSUFBSSxDQUFDLG9CQUFvQixDQUFDLGFBQWEsQ0FBQyxnQkFBZ0IsQ0FBQyxJQUFJLGlDQUMvRCxPQUFPLEtBQ1YsUUFBUSxFQUFFLE1BQU0sSUFBSSxDQUFDLGlCQUFpQixFQUFFLElBQ3hDLENBQUMsRUFBRSxDQUFDO1lBQ1IsQ0FBQztTQUFBO1FBRUQsNERBQTREO1FBQ3RELGVBQWUsQ0FBQyxVQUFvRCxFQUFFOztnQkFFMUUsT0FBTyxJQUFJLENBQUMsb0JBQW9CLENBQUMsYUFBYSxDQUFDLGtCQUFrQixDQUFDLElBQUksaUNBQ2pFLE9BQU8sS0FDVixRQUFRLEVBQUUsTUFBTSxJQUFJLENBQUMsaUJBQWlCLEVBQUUsSUFDeEMsQ0FBQyxFQUFFLENBQUM7WUFDUixDQUFDO1NBQUE7UUFFRCwyREFBMkQ7UUFDckQsWUFBWSxDQUFDLE9BQTZCOztnQkFDOUMsTUFBTSxJQUFJLENBQUMsS0FBSyxFQUFFLENBQUMsQ0FBQyxnRUFBZ0U7Z0JBQ3BGLE1BQU0sT0FBTyxHQUFHLE1BQU0sSUFBSSxDQUFDLFVBQVUsQ0FBQyxPQUFPLENBQUMsQ0FBQztnQkFDL0MsSUFBSSxDQUFDLE9BQU8sQ0FBQyxNQUFNLEVBQUU7b0JBQ25CLE1BQU0sS0FBSyxDQUFDLHdDQUF3QyxJQUFJLENBQUMsU0FBUyxDQUFDLE9BQU8sQ0FBQyxFQUFFLENBQUMsQ0FBQztpQkFDaEY7Z0JBQ0QsTUFBTSxPQUFPLENBQUMsQ0FBQyxDQUFDLENBQUMsS0FBSyxFQUFFLENBQUM7WUFDM0IsQ0FBQztTQUFBO1FBRUQsd0NBQXdDO1FBQ2xDLE1BQU07O2dCQUNWLE1BQU0sS0FBSyxHQUFHLE1BQU0sSUFBSSxDQUFDLFNBQVMsRUFBRSxDQUFDO2dCQUNyQyxPQUFPLENBQUMsQ0FBQyxLQUFLLEtBQUksTUFBTSxLQUFLLENBQUMsUUFBUSxDQUFDLDBCQUEwQixDQUFDLENBQUEsQ0FBQztZQUNyRSxDQUFDO1NBQUE7UUFFRCxnRUFBZ0U7UUFDbEQsU0FBUzs7Z0JBQ3JCLHNEQUFzRDtnQkFDdEQsNkRBQTZEO2dCQUM3RCxPQUFPLElBQUksQ0FBQyxvQkFBb0IsQ0FBQyxrQkFBa0IsQ0FBQyxNQUFNLElBQUksQ0FBQyxpQkFBaUIsRUFBRSxDQUFDLEVBQUUsQ0FBQztZQUN4RixDQUFDO1NBQUE7UUFFRCxtRkFBbUY7UUFDckUsaUJBQWlCOztnQkFDN0IsT0FBTyxJQUFJLENBQUMsTUFBTSxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsWUFBWSxDQUFDLFdBQVcsQ0FBQyxDQUFDLEVBQUUsQ0FBQztZQUNyRSxDQUFDO1NBQUE7O0lBckZELHlFQUF5RTtJQUNsRSxtQ0FBWSxHQUFHLDJCQUEyQixDQUFDO0lBcUZwRCw2QkFBQztLQUFBO1NBekZZLHNCQUFzQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge2NvZXJjZUJvb2xlYW5Qcm9wZXJ0eX0gZnJvbSAnQGFuZ3VsYXIvY2RrL2NvZXJjaW9uJztcbmltcG9ydCB7Q29tcG9uZW50SGFybmVzcywgSGFybmVzc1ByZWRpY2F0ZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3Rlc3RpbmcnO1xuaW1wb3J0IHtcbiAgTWF0T3B0Z3JvdXBIYXJuZXNzLFxuICBNYXRPcHRpb25IYXJuZXNzLFxuICBPcHRncm91cEhhcm5lc3NGaWx0ZXJzLFxuICBPcHRpb25IYXJuZXNzRmlsdGVyc1xufSBmcm9tICdAYW5ndWxhci9tYXRlcmlhbC9jb3JlL3Rlc3RpbmcnO1xuaW1wb3J0IHtBdXRvY29tcGxldGVIYXJuZXNzRmlsdGVyc30gZnJvbSAnLi9hdXRvY29tcGxldGUtaGFybmVzcy1maWx0ZXJzJztcblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBtYXQtYXV0b2NvbXBsZXRlIGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdEF1dG9jb21wbGV0ZUhhcm5lc3MgZXh0ZW5kcyBDb21wb25lbnRIYXJuZXNzIHtcbiAgcHJpdmF0ZSBfZG9jdW1lbnRSb290TG9jYXRvciA9IHRoaXMuZG9jdW1lbnRSb290TG9jYXRvckZhY3RvcnkoKTtcblxuICAvKiogVGhlIHNlbGVjdG9yIGZvciB0aGUgaG9zdCBlbGVtZW50IG9mIGEgYE1hdEF1dG9jb21wbGV0ZWAgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1hdXRvY29tcGxldGUtdHJpZ2dlcic7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYE1hdEF1dG9jb21wbGV0ZUhhcm5lc3NgIHRoYXQgbWVldHNcbiAgICogY2VydGFpbiBjcml0ZXJpYS5cbiAgICogQHBhcmFtIG9wdGlvbnMgT3B0aW9ucyBmb3IgZmlsdGVyaW5nIHdoaWNoIGF1dG9jb21wbGV0ZSBpbnN0YW5jZXMgYXJlIGNvbnNpZGVyZWQgYSBtYXRjaC5cbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBBdXRvY29tcGxldGVIYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXRBdXRvY29tcGxldGVIYXJuZXNzPiB7XG4gICAgcmV0dXJuIG5ldyBIYXJuZXNzUHJlZGljYXRlKE1hdEF1dG9jb21wbGV0ZUhhcm5lc3MsIG9wdGlvbnMpXG4gICAgICAgIC5hZGRPcHRpb24oJ3ZhbHVlJywgb3B0aW9ucy52YWx1ZSxcbiAgICAgICAgICAgIChoYXJuZXNzLCB2YWx1ZSkgPT4gSGFybmVzc1ByZWRpY2F0ZS5zdHJpbmdNYXRjaGVzKGhhcm5lc3MuZ2V0VmFsdWUoKSwgdmFsdWUpKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSB2YWx1ZSBvZiB0aGUgYXV0b2NvbXBsZXRlIGlucHV0LiAqL1xuICBhc3luYyBnZXRWYWx1ZSgpOiBQcm9taXNlPHN0cmluZz4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldFByb3BlcnR5KCd2YWx1ZScpO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGF1dG9jb21wbGV0ZSBpbnB1dCBpcyBkaXNhYmxlZC4gKi9cbiAgYXN5bmMgaXNEaXNhYmxlZCgpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICBjb25zdCBkaXNhYmxlZCA9IChhd2FpdCB0aGlzLmhvc3QoKSkuZ2V0QXR0cmlidXRlKCdkaXNhYmxlZCcpO1xuICAgIHJldHVybiBjb2VyY2VCb29sZWFuUHJvcGVydHkoYXdhaXQgZGlzYWJsZWQpO1xuICB9XG5cbiAgLyoqIEZvY3VzZXMgdGhlIGF1dG9jb21wbGV0ZSBpbnB1dC4gKi9cbiAgYXN5bmMgZm9jdXMoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuZm9jdXMoKTtcbiAgfVxuXG4gIC8qKiBCbHVycyB0aGUgYXV0b2NvbXBsZXRlIGlucHV0LiAqL1xuICBhc3luYyBibHVyKCk6IFByb21pc2U8dm9pZD4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmJsdXIoKTtcbiAgfVxuXG4gIC8qKiBFbnRlcnMgdGV4dCBpbnRvIHRoZSBhdXRvY29tcGxldGUuICovXG4gIGFzeW5jIGVudGVyVGV4dCh2YWx1ZTogc3RyaW5nKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuc2VuZEtleXModmFsdWUpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIG9wdGlvbnMgaW5zaWRlIHRoZSBhdXRvY29tcGxldGUgcGFuZWwuICovXG4gIGFzeW5jIGdldE9wdGlvbnMoZmlsdGVyczogT21pdDxPcHRpb25IYXJuZXNzRmlsdGVycywgJ2FuY2VzdG9yJz4gPSB7fSk6XG4gICAgUHJvbWlzZTxNYXRPcHRpb25IYXJuZXNzW10+IHtcbiAgICByZXR1cm4gdGhpcy5fZG9jdW1lbnRSb290TG9jYXRvci5sb2NhdG9yRm9yQWxsKE1hdE9wdGlvbkhhcm5lc3Mud2l0aCh7XG4gICAgICAuLi5maWx0ZXJzLFxuICAgICAgYW5jZXN0b3I6IGF3YWl0IHRoaXMuX2dldFBhbmVsU2VsZWN0b3IoKVxuICAgIH0pKSgpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIG9wdGlvbiBncm91cHMgaW5zaWRlIHRoZSBhdXRvY29tcGxldGUgcGFuZWwuICovXG4gIGFzeW5jIGdldE9wdGlvbkdyb3VwcyhmaWx0ZXJzOiBPbWl0PE9wdGdyb3VwSGFybmVzc0ZpbHRlcnMsICdhbmNlc3Rvcic+ID0ge30pOlxuICAgIFByb21pc2U8TWF0T3B0Z3JvdXBIYXJuZXNzW10+IHtcbiAgICByZXR1cm4gdGhpcy5fZG9jdW1lbnRSb290TG9jYXRvci5sb2NhdG9yRm9yQWxsKE1hdE9wdGdyb3VwSGFybmVzcy53aXRoKHtcbiAgICAgIC4uLmZpbHRlcnMsXG4gICAgICBhbmNlc3RvcjogYXdhaXQgdGhpcy5fZ2V0UGFuZWxTZWxlY3RvcigpXG4gICAgfSkpKCk7XG4gIH1cblxuICAvKiogU2VsZWN0cyB0aGUgZmlyc3Qgb3B0aW9uIG1hdGNoaW5nIHRoZSBnaXZlbiBmaWx0ZXJzLiAqL1xuICBhc3luYyBzZWxlY3RPcHRpb24oZmlsdGVyczogT3B0aW9uSGFybmVzc0ZpbHRlcnMpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICBhd2FpdCB0aGlzLmZvY3VzKCk7IC8vIEZvY3VzIHRoZSBpbnB1dCB0byBtYWtlIHN1cmUgdGhlIGF1dG9jb21wbGV0ZSBwYW5lbCBpcyBzaG93bi5cbiAgICBjb25zdCBvcHRpb25zID0gYXdhaXQgdGhpcy5nZXRPcHRpb25zKGZpbHRlcnMpO1xuICAgIGlmICghb3B0aW9ucy5sZW5ndGgpIHtcbiAgICAgIHRocm93IEVycm9yKGBDb3VsZCBub3QgZmluZCBhIG1hdC1vcHRpb24gbWF0Y2hpbmcgJHtKU09OLnN0cmluZ2lmeShmaWx0ZXJzKX1gKTtcbiAgICB9XG4gICAgYXdhaXQgb3B0aW9uc1swXS5jbGljaygpO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGF1dG9jb21wbGV0ZSBpcyBvcGVuLiAqL1xuICBhc3luYyBpc09wZW4oKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgY29uc3QgcGFuZWwgPSBhd2FpdCB0aGlzLl9nZXRQYW5lbCgpO1xuICAgIHJldHVybiAhIXBhbmVsICYmIGF3YWl0IHBhbmVsLmhhc0NsYXNzKCdtYXQtYXV0b2NvbXBsZXRlLXZpc2libGUnKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSBwYW5lbCBhc3NvY2lhdGVkIHdpdGggdGhpcyBhdXRvY29tcGxldGUgdHJpZ2dlci4gKi9cbiAgcHJpdmF0ZSBhc3luYyBfZ2V0UGFuZWwoKSB7XG4gICAgLy8gVGVjaG5pY2FsbHkgdGhpcyBpcyBzdGF0aWMsIGJ1dCBpdCBuZWVkcyB0byBiZSBpbiBhXG4gICAgLy8gZnVuY3Rpb24sIGJlY2F1c2UgdGhlIGF1dG9jb21wbGV0ZSdzIHBhbmVsIElEIGNhbiBjaGFuZ2VkLlxuICAgIHJldHVybiB0aGlzLl9kb2N1bWVudFJvb3RMb2NhdG9yLmxvY2F0b3JGb3JPcHRpb25hbChhd2FpdCB0aGlzLl9nZXRQYW5lbFNlbGVjdG9yKCkpKCk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgc2VsZWN0b3IgdGhhdCBjYW4gYmUgdXNlZCB0byBmaW5kIHRoZSBhdXRvY29tcGxldGUgdHJpZ2dlcidzIHBhbmVsLiAqL1xuICBwcml2YXRlIGFzeW5jIF9nZXRQYW5lbFNlbGVjdG9yKCk6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgcmV0dXJuIGAjJHsoYXdhaXQgKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRBdHRyaWJ1dGUoJ2FyaWEtb3ducycpKX1gO1xuICB9XG59XG4iXX0=