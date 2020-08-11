/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
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
export { MatDrawerHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZHJhd2VyLWhhcm5lc3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvc2lkZW5hdi90ZXN0aW5nL2RyYXdlci1oYXJuZXNzLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7QUFFSCxPQUFPLEVBQUMsZ0JBQWdCLEVBQUUsZ0JBQWdCLEVBQUMsTUFBTSxzQkFBc0IsQ0FBQztBQUd4RSxtRUFBbUU7QUFDbkU7SUFBQSxNQUFhLGdCQUFpQixTQUFRLGdCQUFnQjtRQUlwRDs7Ozs7V0FLRztRQUNILE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBZ0MsRUFBRTtZQUM1QyxPQUFPLElBQUksZ0JBQWdCLENBQUMsZ0JBQWdCLEVBQUUsT0FBTyxDQUFDO2lCQUNqRCxTQUFTLENBQUMsVUFBVSxFQUFFLE9BQU8sQ0FBQyxRQUFRLEVBQ25DLENBQU8sT0FBTyxFQUFFLFFBQVEsRUFBRSxFQUFFLGdEQUFDLE9BQUEsQ0FBQyxNQUFNLE9BQU8sQ0FBQyxXQUFXLEVBQUUsQ0FBQyxLQUFLLFFBQVEsQ0FBQSxHQUFBLENBQUMsQ0FBQztRQUNuRixDQUFDO1FBRUQsa0NBQWtDO1FBQzVCLE1BQU07O2dCQUNWLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFFBQVEsQ0FBQyxtQkFBbUIsQ0FBQyxDQUFDO1lBQzNELENBQUM7U0FBQTtRQUVELDREQUE0RDtRQUN0RCxXQUFXOztnQkFDZixNQUFNLElBQUksR0FBRyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQztnQkFDL0IsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLFFBQVEsQ0FBQyxnQkFBZ0IsQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLEtBQUssQ0FBQyxDQUFDLENBQUMsT0FBTyxDQUFDO1lBQ25FLENBQUM7U0FBQTtRQUVELDJDQUEyQztRQUNyQyxPQUFPOztnQkFDWCxNQUFNLElBQUksR0FBRyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQztnQkFFL0IsSUFBSSxNQUFNLElBQUksQ0FBQyxRQUFRLENBQUMsaUJBQWlCLENBQUMsRUFBRTtvQkFDMUMsT0FBTyxNQUFNLENBQUM7aUJBQ2Y7Z0JBRUQsSUFBSSxNQUFNLElBQUksQ0FBQyxRQUFRLENBQUMsaUJBQWlCLENBQUMsRUFBRTtvQkFDMUMsT0FBTyxNQUFNLENBQUM7aUJBQ2Y7Z0JBRUQsT0FBTyxNQUFNLENBQUM7WUFDaEIsQ0FBQztTQUFBOztJQXZDRCxtRUFBbUU7SUFDNUQsNkJBQVksR0FBRyxhQUFhLENBQUM7SUF1Q3RDLHVCQUFDO0tBQUE7U0F6Q1ksZ0JBQWdCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Q29tcG9uZW50SGFybmVzcywgSGFybmVzc1ByZWRpY2F0ZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3Rlc3RpbmcnO1xuaW1wb3J0IHtEcmF3ZXJIYXJuZXNzRmlsdGVyc30gZnJvbSAnLi9kcmF3ZXItaGFybmVzcy1maWx0ZXJzJztcblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBtYXQtZHJhd2VyIGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdERyYXdlckhhcm5lc3MgZXh0ZW5kcyBDb21wb25lbnRIYXJuZXNzIHtcbiAgLyoqIFRoZSBzZWxlY3RvciBmb3IgdGhlIGhvc3QgZWxlbWVudCBvZiBhIGBNYXREcmF3ZXJgIGluc3RhbmNlLiAqL1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJy5tYXQtZHJhd2VyJztcblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzUHJlZGljYXRlYCB0aGF0IGNhbiBiZSB1c2VkIHRvIHNlYXJjaCBmb3IgYSBgTWF0RHJhd2VySGFybmVzc2AgdGhhdCBtZWV0c1xuICAgKiBjZXJ0YWluIGNyaXRlcmlhLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBmaWx0ZXJpbmcgd2hpY2ggZHJhd2VyIGluc3RhbmNlcyBhcmUgY29uc2lkZXJlZCBhIG1hdGNoLlxuICAgKiBAcmV0dXJuIGEgYEhhcm5lc3NQcmVkaWNhdGVgIGNvbmZpZ3VyZWQgd2l0aCB0aGUgZ2l2ZW4gb3B0aW9ucy5cbiAgICovXG4gIHN0YXRpYyB3aXRoKG9wdGlvbnM6IERyYXdlckhhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdERyYXdlckhhcm5lc3M+IHtcbiAgICByZXR1cm4gbmV3IEhhcm5lc3NQcmVkaWNhdGUoTWF0RHJhd2VySGFybmVzcywgb3B0aW9ucylcbiAgICAgICAgLmFkZE9wdGlvbigncG9zaXRpb24nLCBvcHRpb25zLnBvc2l0aW9uLFxuICAgICAgICAgICAgYXN5bmMgKGhhcm5lc3MsIHBvc2l0aW9uKSA9PiAoYXdhaXQgaGFybmVzcy5nZXRQb3NpdGlvbigpKSA9PT0gcG9zaXRpb24pO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGRyYXdlciBpcyBvcGVuLiAqL1xuICBhc3luYyBpc09wZW4oKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuaGFzQ2xhc3MoJ21hdC1kcmF3ZXItb3BlbmVkJyk7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgcG9zaXRpb24gb2YgdGhlIGRyYXdlciBpbnNpZGUgaXRzIGNvbnRhaW5lci4gKi9cbiAgYXN5bmMgZ2V0UG9zaXRpb24oKTogUHJvbWlzZTwnc3RhcnQnfCdlbmQnPiB7XG4gICAgY29uc3QgaG9zdCA9IGF3YWl0IHRoaXMuaG9zdCgpO1xuICAgIHJldHVybiAoYXdhaXQgaG9zdC5oYXNDbGFzcygnbWF0LWRyYXdlci1lbmQnKSkgPyAnZW5kJyA6ICdzdGFydCc7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgbW9kZSB0aGF0IHRoZSBkcmF3ZXIgaXMgaW4uICovXG4gIGFzeW5jIGdldE1vZGUoKTogUHJvbWlzZTwnb3Zlcid8J3B1c2gnfCdzaWRlJz4ge1xuICAgIGNvbnN0IGhvc3QgPSBhd2FpdCB0aGlzLmhvc3QoKTtcblxuICAgIGlmIChhd2FpdCBob3N0Lmhhc0NsYXNzKCdtYXQtZHJhd2VyLXB1c2gnKSkge1xuICAgICAgcmV0dXJuICdwdXNoJztcbiAgICB9XG5cbiAgICBpZiAoYXdhaXQgaG9zdC5oYXNDbGFzcygnbWF0LWRyYXdlci1zaWRlJykpIHtcbiAgICAgIHJldHVybiAnc2lkZSc7XG4gICAgfVxuXG4gICAgcmV0dXJuICdvdmVyJztcbiAgfVxufVxuIl19