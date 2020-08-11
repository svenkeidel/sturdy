/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
/**
 * Harness for interacting with a standard Material badge in tests.
 * @dynamic
 */
let MatBadgeHarness = /** @class */ (() => {
    class MatBadgeHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._badgeElement = this.locatorFor('.mat-badge-content');
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a badge with specific attributes.
         * @param options Options for narrowing the search:
         *   - `text` finds a badge host with a particular text.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatBadgeHarness, options)
                .addOption('text', options.text, (harness, text) => HarnessPredicate.stringMatches(harness.getText(), text));
        }
        /** Gets a promise for the badge text. */
        getText() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._badgeElement()).text();
            });
        }
        /** Gets whether the badge is overlapping the content. */
        isOverlapping() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-badge-overlap');
            });
        }
        /** Gets the position of the badge. */
        getPosition() {
            return __awaiter(this, void 0, void 0, function* () {
                const host = yield this.host();
                let result = '';
                if (yield host.hasClass('mat-badge-above')) {
                    result += 'above';
                }
                else if (yield host.hasClass('mat-badge-below')) {
                    result += 'below';
                }
                if (yield host.hasClass('mat-badge-before')) {
                    result += ' before';
                }
                else if (yield host.hasClass('mat-badge-after')) {
                    result += ' after';
                }
                return result.trim();
            });
        }
        /** Gets the size of the badge. */
        getSize() {
            return __awaiter(this, void 0, void 0, function* () {
                const host = yield this.host();
                if (yield host.hasClass('mat-badge-small')) {
                    return 'small';
                }
                else if (yield host.hasClass('mat-badge-large')) {
                    return 'large';
                }
                return 'medium';
            });
        }
        /** Gets whether the badge is hidden. */
        isHidden() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-badge-hidden');
            });
        }
        /** Gets whether the badge is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-badge-disabled');
            });
        }
    }
    MatBadgeHarness.hostSelector = '.mat-badge';
    return MatBadgeHarness;
})();
export { MatBadgeHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYmFkZ2UtaGFybmVzcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9iYWRnZS90ZXN0aW5nL2JhZGdlLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBS3hFOzs7R0FHRztBQUNIO0lBQUEsTUFBYSxlQUFnQixTQUFRLGdCQUFnQjtRQUFyRDs7WUFlVSxrQkFBYSxHQUFHLElBQUksQ0FBQyxVQUFVLENBQUMsb0JBQW9CLENBQUMsQ0FBQztRQXNEaEUsQ0FBQztRQWxFQzs7Ozs7V0FLRztRQUNILE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBK0IsRUFBRTtZQUMzQyxPQUFPLElBQUksZ0JBQWdCLENBQUMsZUFBZSxFQUFFLE9BQU8sQ0FBQztpQkFDaEQsU0FBUyxDQUFDLE1BQU0sRUFBRSxPQUFPLENBQUMsSUFBSSxFQUMzQixDQUFDLE9BQU8sRUFBRSxJQUFJLEVBQUUsRUFBRSxDQUFDLGdCQUFnQixDQUFDLGFBQWEsQ0FBQyxPQUFPLENBQUMsT0FBTyxFQUFFLEVBQUUsSUFBSSxDQUFDLENBQUMsQ0FBQztRQUN0RixDQUFDO1FBSUQseUNBQXlDO1FBQ25DLE9BQU87O2dCQUNYLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxhQUFhLEVBQUUsQ0FBQyxDQUFDLElBQUksRUFBRSxDQUFDO1lBQzdDLENBQUM7U0FBQTtRQUVELHlEQUF5RDtRQUNuRCxhQUFhOztnQkFDakIsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsUUFBUSxDQUFDLG1CQUFtQixDQUFDLENBQUM7WUFDM0QsQ0FBQztTQUFBO1FBRUQsc0NBQXNDO1FBQ2hDLFdBQVc7O2dCQUNmLE1BQU0sSUFBSSxHQUFHLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDO2dCQUMvQixJQUFJLE1BQU0sR0FBRyxFQUFFLENBQUM7Z0JBRWhCLElBQUksTUFBTSxJQUFJLENBQUMsUUFBUSxDQUFDLGlCQUFpQixDQUFDLEVBQUU7b0JBQzFDLE1BQU0sSUFBSSxPQUFPLENBQUM7aUJBQ25CO3FCQUFNLElBQUksTUFBTSxJQUFJLENBQUMsUUFBUSxDQUFDLGlCQUFpQixDQUFDLEVBQUU7b0JBQ2pELE1BQU0sSUFBSSxPQUFPLENBQUM7aUJBQ25CO2dCQUVELElBQUksTUFBTSxJQUFJLENBQUMsUUFBUSxDQUFDLGtCQUFrQixDQUFDLEVBQUU7b0JBQzNDLE1BQU0sSUFBSSxTQUFTLENBQUM7aUJBQ3JCO3FCQUFNLElBQUksTUFBTSxJQUFJLENBQUMsUUFBUSxDQUFDLGlCQUFpQixDQUFDLEVBQUU7b0JBQ2pELE1BQU0sSUFBSSxRQUFRLENBQUM7aUJBQ3BCO2dCQUVELE9BQU8sTUFBTSxDQUFDLElBQUksRUFBc0IsQ0FBQztZQUMzQyxDQUFDO1NBQUE7UUFFRCxrQ0FBa0M7UUFDNUIsT0FBTzs7Z0JBQ1gsTUFBTSxJQUFJLEdBQUcsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUM7Z0JBRS9CLElBQUksTUFBTSxJQUFJLENBQUMsUUFBUSxDQUFDLGlCQUFpQixDQUFDLEVBQUU7b0JBQzFDLE9BQU8sT0FBTyxDQUFDO2lCQUNoQjtxQkFBTSxJQUFJLE1BQU0sSUFBSSxDQUFDLFFBQVEsQ0FBQyxpQkFBaUIsQ0FBQyxFQUFFO29CQUNqRCxPQUFPLE9BQU8sQ0FBQztpQkFDaEI7Z0JBRUQsT0FBTyxRQUFRLENBQUM7WUFDbEIsQ0FBQztTQUFBO1FBRUQsd0NBQXdDO1FBQ2xDLFFBQVE7O2dCQUNaLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFFBQVEsQ0FBQyxrQkFBa0IsQ0FBQyxDQUFDO1lBQzFELENBQUM7U0FBQTtRQUVELDBDQUEwQztRQUNwQyxVQUFVOztnQkFDZCxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxRQUFRLENBQUMsb0JBQW9CLENBQUMsQ0FBQztZQUM1RCxDQUFDO1NBQUE7O0lBbkVNLDRCQUFZLEdBQUcsWUFBWSxDQUFDO0lBb0VyQyxzQkFBQztLQUFBO1NBckVZLGVBQWUiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtDb21wb25lbnRIYXJuZXNzLCBIYXJuZXNzUHJlZGljYXRlfSBmcm9tICdAYW5ndWxhci9jZGsvdGVzdGluZyc7XG5pbXBvcnQge01hdEJhZGdlUG9zaXRpb24sIE1hdEJhZGdlU2l6ZX0gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvYmFkZ2UnO1xuaW1wb3J0IHtCYWRnZUhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL2JhZGdlLWhhcm5lc3MtZmlsdGVycyc7XG5cblxuLyoqXG4gKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgTWF0ZXJpYWwgYmFkZ2UgaW4gdGVzdHMuXG4gKiBAZHluYW1pY1xuICovXG5leHBvcnQgY2xhc3MgTWF0QmFkZ2VIYXJuZXNzIGV4dGVuZHMgQ29tcG9uZW50SGFybmVzcyB7XG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1iYWRnZSc7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYmFkZ2Ugd2l0aCBzcGVjaWZpYyBhdHRyaWJ1dGVzLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBuYXJyb3dpbmcgdGhlIHNlYXJjaDpcbiAgICogICAtIGB0ZXh0YCBmaW5kcyBhIGJhZGdlIGhvc3Qgd2l0aCBhIHBhcnRpY3VsYXIgdGV4dC5cbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBCYWRnZUhhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdEJhZGdlSGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXRCYWRnZUhhcm5lc3MsIG9wdGlvbnMpXG4gICAgICAgIC5hZGRPcHRpb24oJ3RleHQnLCBvcHRpb25zLnRleHQsXG4gICAgICAgICAgICAoaGFybmVzcywgdGV4dCkgPT4gSGFybmVzc1ByZWRpY2F0ZS5zdHJpbmdNYXRjaGVzKGhhcm5lc3MuZ2V0VGV4dCgpLCB0ZXh0KSk7XG4gIH1cblxuICBwcml2YXRlIF9iYWRnZUVsZW1lbnQgPSB0aGlzLmxvY2F0b3JGb3IoJy5tYXQtYmFkZ2UtY29udGVudCcpO1xuXG4gIC8qKiBHZXRzIGEgcHJvbWlzZSBmb3IgdGhlIGJhZGdlIHRleHQuICovXG4gIGFzeW5jIGdldFRleHQoKTogUHJvbWlzZTxzdHJpbmc+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuX2JhZGdlRWxlbWVudCgpKS50ZXh0KCk7XG4gIH1cblxuICAvKiogR2V0cyB3aGV0aGVyIHRoZSBiYWRnZSBpcyBvdmVybGFwcGluZyB0aGUgY29udGVudC4gKi9cbiAgYXN5bmMgaXNPdmVybGFwcGluZygpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5oYXNDbGFzcygnbWF0LWJhZGdlLW92ZXJsYXAnKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSBwb3NpdGlvbiBvZiB0aGUgYmFkZ2UuICovXG4gIGFzeW5jIGdldFBvc2l0aW9uKCk6IFByb21pc2U8TWF0QmFkZ2VQb3NpdGlvbj4ge1xuICAgIGNvbnN0IGhvc3QgPSBhd2FpdCB0aGlzLmhvc3QoKTtcbiAgICBsZXQgcmVzdWx0ID0gJyc7XG5cbiAgICBpZiAoYXdhaXQgaG9zdC5oYXNDbGFzcygnbWF0LWJhZGdlLWFib3ZlJykpIHtcbiAgICAgIHJlc3VsdCArPSAnYWJvdmUnO1xuICAgIH0gZWxzZSBpZiAoYXdhaXQgaG9zdC5oYXNDbGFzcygnbWF0LWJhZGdlLWJlbG93JykpIHtcbiAgICAgIHJlc3VsdCArPSAnYmVsb3cnO1xuICAgIH1cblxuICAgIGlmIChhd2FpdCBob3N0Lmhhc0NsYXNzKCdtYXQtYmFkZ2UtYmVmb3JlJykpIHtcbiAgICAgIHJlc3VsdCArPSAnIGJlZm9yZSc7XG4gICAgfSBlbHNlIGlmIChhd2FpdCBob3N0Lmhhc0NsYXNzKCdtYXQtYmFkZ2UtYWZ0ZXInKSkge1xuICAgICAgcmVzdWx0ICs9ICcgYWZ0ZXInO1xuICAgIH1cblxuICAgIHJldHVybiByZXN1bHQudHJpbSgpIGFzIE1hdEJhZGdlUG9zaXRpb247XG4gIH1cblxuICAvKiogR2V0cyB0aGUgc2l6ZSBvZiB0aGUgYmFkZ2UuICovXG4gIGFzeW5jIGdldFNpemUoKTogUHJvbWlzZTxNYXRCYWRnZVNpemU+IHtcbiAgICBjb25zdCBob3N0ID0gYXdhaXQgdGhpcy5ob3N0KCk7XG5cbiAgICBpZiAoYXdhaXQgaG9zdC5oYXNDbGFzcygnbWF0LWJhZGdlLXNtYWxsJykpIHtcbiAgICAgIHJldHVybiAnc21hbGwnO1xuICAgIH0gZWxzZSBpZiAoYXdhaXQgaG9zdC5oYXNDbGFzcygnbWF0LWJhZGdlLWxhcmdlJykpIHtcbiAgICAgIHJldHVybiAnbGFyZ2UnO1xuICAgIH1cblxuICAgIHJldHVybiAnbWVkaXVtJztcbiAgfVxuXG4gIC8qKiBHZXRzIHdoZXRoZXIgdGhlIGJhZGdlIGlzIGhpZGRlbi4gKi9cbiAgYXN5bmMgaXNIaWRkZW4oKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuaGFzQ2xhc3MoJ21hdC1iYWRnZS1oaWRkZW4nKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHdoZXRoZXIgdGhlIGJhZGdlIGlzIGRpc2FibGVkLiAqL1xuICBhc3luYyBpc0Rpc2FibGVkKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmhhc0NsYXNzKCdtYXQtYmFkZ2UtZGlzYWJsZWQnKTtcbiAgfVxufVxuIl19