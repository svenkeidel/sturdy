/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
const EXPANSION_PANEL_CONTENT_SELECTOR = '.mat-expansion-panel-content';
/** Harness for interacting with a standard mat-expansion-panel in tests. */
let MatExpansionPanelHarness = /** @class */ (() => {
    class MatExpansionPanelHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            this._header = this.locatorFor('.mat-expansion-panel-header');
            this._title = this.locatorForOptional('.mat-expansion-panel-header-title');
            this._description = this.locatorForOptional('.mat-expansion-panel-header-description');
            this._expansionIndicator = this.locatorForOptional('.mat-expansion-indicator');
            this._content = this.locatorFor(EXPANSION_PANEL_CONTENT_SELECTOR);
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for an expansion-panel
         * with specific attributes.
         * @param options Options for narrowing the search:
         *   - `title` finds an expansion-panel with a specific title text.
         *   - `description` finds an expansion-panel with a specific description text.
         *   - `expanded` finds an expansion-panel that is currently expanded.
         *   - `disabled` finds an expansion-panel that is disabled.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatExpansionPanelHarness, options)
                .addOption('title', options.title, (harness, title) => HarnessPredicate.stringMatches(harness.getTitle(), title))
                .addOption('description', options.description, (harness, description) => HarnessPredicate.stringMatches(harness.getDescription(), description))
                .addOption('content', options.content, (harness, content) => HarnessPredicate.stringMatches(harness.getTextContent(), content))
                .addOption('expanded', options.expanded, (harness, expanded) => __awaiter(this, void 0, void 0, function* () { return (yield harness.isExpanded()) === expanded; }))
                .addOption('disabled', options.disabled, (harness, disabled) => __awaiter(this, void 0, void 0, function* () { return (yield harness.isDisabled()) === disabled; }));
        }
        /** Whether the panel is expanded. */
        isExpanded() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).hasClass('mat-expanded');
            });
        }
        /**
         * Gets the title text of the panel.
         * @returns Title text or `null` if no title is set up.
         */
        getTitle() {
            return __awaiter(this, void 0, void 0, function* () {
                const titleEl = yield this._title();
                return titleEl ? titleEl.text() : null;
            });
        }
        /**
         * Gets the description text of the panel.
         * @returns Description text or `null` if no description is set up.
         */
        getDescription() {
            return __awaiter(this, void 0, void 0, function* () {
                const descriptionEl = yield this._description();
                return descriptionEl ? descriptionEl.text() : null;
            });
        }
        /** Whether the panel is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield (yield this._header()).getAttribute('aria-disabled')) === 'true';
            });
        }
        /**
         * Toggles the expanded state of the panel by clicking on the panel
         * header. This method will not work if the panel is disabled.
         */
        toggle() {
            return __awaiter(this, void 0, void 0, function* () {
                yield (yield this._header()).click();
            });
        }
        /** Expands the expansion panel if collapsed. */
        expand() {
            return __awaiter(this, void 0, void 0, function* () {
                if (!(yield this.isExpanded())) {
                    yield this.toggle();
                }
            });
        }
        /** Collapses the expansion panel if expanded. */
        collapse() {
            return __awaiter(this, void 0, void 0, function* () {
                if (yield this.isExpanded()) {
                    yield this.toggle();
                }
            });
        }
        /** Gets the text content of the panel. */
        getTextContent() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._content()).text();
            });
        }
        /**
         * Gets a `HarnessLoader` that can be used to load harnesses for
         * components within the panel's content area.
         */
        getHarnessLoaderForContent() {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorFactory.harnessLoaderFor(EXPANSION_PANEL_CONTENT_SELECTOR);
            });
        }
        /** Focuses the panel. */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._header()).focus();
            });
        }
        /** Blurs the panel. */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._header()).blur();
            });
        }
        /** Whether the panel has a toggle indicator displayed. */
        hasToggleIndicator() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._expansionIndicator()) !== null;
            });
        }
        /** Gets the position of the toggle indicator. */
        getToggleIndicatorPosition() {
            return __awaiter(this, void 0, void 0, function* () {
                // By default the expansion indicator will show "after" the panel header content.
                if (yield (yield this._header()).hasClass('mat-expansion-toggle-indicator-before')) {
                    return 'before';
                }
                return 'after';
            });
        }
    }
    MatExpansionPanelHarness.hostSelector = '.mat-expansion-panel';
    return MatExpansionPanelHarness;
})();
export { MatExpansionPanelHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZXhwYW5zaW9uLWhhcm5lc3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvZXhwYW5zaW9uL3Rlc3RpbmcvZXhwYW5zaW9uLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBaUIsZ0JBQWdCLEVBQUMsTUFBTSxzQkFBc0IsQ0FBQztBQUd2RixNQUFNLGdDQUFnQyxHQUFHLDhCQUE4QixDQUFDO0FBRXhFLDRFQUE0RTtBQUM1RTtJQUFBLE1BQWEsd0JBQXlCLFNBQVEsZ0JBQWdCO1FBQTlEOztZQUdVLFlBQU8sR0FBRyxJQUFJLENBQUMsVUFBVSxDQUFDLDZCQUE2QixDQUFDLENBQUM7WUFDekQsV0FBTSxHQUFHLElBQUksQ0FBQyxrQkFBa0IsQ0FBQyxtQ0FBbUMsQ0FBQyxDQUFDO1lBQ3RFLGlCQUFZLEdBQUcsSUFBSSxDQUFDLGtCQUFrQixDQUFDLHlDQUF5QyxDQUFDLENBQUM7WUFDbEYsd0JBQW1CLEdBQUcsSUFBSSxDQUFDLGtCQUFrQixDQUFDLDBCQUEwQixDQUFDLENBQUM7WUFDMUUsYUFBUSxHQUFHLElBQUksQ0FBQyxVQUFVLENBQUMsZ0NBQWdDLENBQUMsQ0FBQztRQXVIdkUsQ0FBQztRQXJIQzs7Ozs7Ozs7O1dBU0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQXdDLEVBQUU7WUFFcEQsT0FBTyxJQUFJLGdCQUFnQixDQUFDLHdCQUF3QixFQUFFLE9BQU8sQ0FBQztpQkFDekQsU0FBUyxDQUNOLE9BQU8sRUFBRSxPQUFPLENBQUMsS0FBSyxFQUN0QixDQUFDLE9BQU8sRUFBRSxLQUFLLEVBQUUsRUFBRSxDQUFDLGdCQUFnQixDQUFDLGFBQWEsQ0FBQyxPQUFPLENBQUMsUUFBUSxFQUFFLEVBQUUsS0FBSyxDQUFDLENBQUM7aUJBQ2pGLFNBQVMsQ0FDTixhQUFhLEVBQUUsT0FBTyxD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LENBQUMsQ0FBQyxLQUFLLEVBQUUsQ0FBQztZQUN4QyxDQUFDO1NBQUE7UUFFRCx1QkFBdUI7UUFDakIsSUFBSTs7Z0JBQ1IsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDLENBQUMsSUFBSSxFQUFFLENBQUM7WUFDdkMsQ0FBQztTQUFBO1FBRUQsMERBQTBEO1FBQ3BELGtCQUFrQjs7Z0JBQ3RCLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxtQkFBbUIsRUFBRSxDQUFDLEtBQUssSUFBSSxDQUFDO1lBQ3JELENBQUM7U0FBQTtRQUVELGlEQUFpRDtRQUMzQywwQkFBMEI7O2dCQUM5QixpRkFBaUY7Z0JBQ2pGLElBQUksTUFBTSxDQUFDLE1BQU0sSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDLENBQUMsUUFBUSxDQUFDLHVDQUF1QyxDQUFDLEVBQUU7b0JBQ2xGLE9BQU8sUUFBUSxDQUFDO2lCQUNqQjtnQkFDRCxPQUFPLE9BQU8sQ0FBQztZQUNqQixDQUFDO1NBQUE7O0lBNUhNLHFDQUFZLEdBQUcsc0JBQXNCLENBQUM7SUE2SC9DLCtCQUFDO0tBQUE7U0E5SFksd0JBQXdCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Q29tcG9uZW50SGFybmVzcywgSGFybmVzc0xvYWRlciwgSGFybmVzc1ByZWRpY2F0ZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3Rlc3RpbmcnO1xuaW1wb3J0IHtFeHBhbnNpb25QYW5lbEhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL2V4cGFuc2lvbi1oYXJuZXNzLWZpbHRlcnMnO1xuXG5jb25zdCBFWFBBTlNJT05fUEFORUxfQ09OVEVOVF9TRUxFQ1RPUiA9ICcubWF0LWV4cGFuc2lvbi1wYW5lbC1jb250ZW50JztcblxuLyoqIEhhcm5lc3MgZm9yIGludGVyYWN0aW5nIHdpdGggYSBzdGFuZGFyZCBtYXQtZXhwYW5zaW9uLXBhbmVsIGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdEV4cGFuc2lvblBhbmVsSGFybmVzcyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3Mge1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJy5tYXQtZXhwYW5zaW9uLXBhbmVsJztcblxuICBwcml2YXRlIF9oZWFkZXIgPSB0aGlzLmxvY2F0b3JGb3IoJy5tYXQtZXhwYW5zaW9uLXBhbmVsLWhlYWRlcicpO1xuICBwcml2YXRlIF90aXRsZSA9IHRoaXMubG9jYXRvckZvck9wdGlvbmFsKCcubWF0LWV4cGFuc2lvbi1wYW5lbC1oZWFkZXItdGl0bGUnKTtcbiAgcHJpdmF0ZSBfZGVzY3JpcHRpb24gPSB0aGlzLmxvY2F0b3JGb3JPcHRpb25hbCgnLm1hdC1leHBhbnNpb24tcGFuZWwtaGVhZGVyLWRlc2NyaXB0aW9uJyk7XG4gIHByaXZhdGUgX2V4cGFuc2lvbkluZGljYXRvciA9IHRoaXMubG9jYXRvckZvck9wdGlvbmFsKCcubWF0LWV4cGFuc2lvbi1pbmRpY2F0b3InKTtcbiAgcHJpdmF0ZSBfY29udGVudCA9IHRoaXMubG9jYXRvckZvcihFWFBBTlNJT05fUEFORUxfQ09OVEVOVF9TRUxFQ1RPUik7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGFuIGV4cGFuc2lvbi1wYW5lbFxuICAgKiB3aXRoIHNwZWNpZmljIGF0dHJpYnV0ZXMuXG4gICAqIEBwYXJhbSBvcHRpb25zIE9wdGlvbnMgZm9yIG5hcnJvd2luZyB0aGUgc2VhcmNoOlxuICAgKiAgIC0gYHRpdGxlYCBmaW5kcyBhbiBleHBhbnNpb24tcGFuZWwgd2l0aCBhIHNwZWNpZmljIHRpdGxlIHRleHQuXG4gICAqICAgLSBgZGVzY3JpcHRpb25gIGZpbmRzIGFuIGV4cGFuc2lvbi1wYW5lbCB3aXRoIGEgc3BlY2lmaWMgZGVzY3JpcHRpb24gdGV4dC5cbiAgICogICAtIGBleHBhbmRlZGAgZmluZHMgYW4gZXhwYW5zaW9uLXBhbmVsIHRoYXQgaXMgY3VycmVudGx5IGV4cGFuZGVkLlxuICAgKiAgIC0gYGRpc2FibGVkYCBmaW5kcyBhbiBleHBhbnNpb24tcGFuZWwgdGhhdCBpcyBkaXNhYmxlZC5cbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBFeHBhbnNpb25QYW5lbEhhcm5lc3NGaWx0ZXJzID0ge30pOlxuICAgICAgSGFybmVzc1ByZWRpY2F0ZTxNYXRFeHBhbnNpb25QYW5lbEhhcm5lc3M+IHtcbiAgICByZXR1cm4gbmV3IEhhcm5lc3NQcmVkaWNhdGUoTWF0RXhwYW5zaW9uUGFuZWxIYXJuZXNzLCBvcHRpb25zKVxuICAgICAgICAuYWRkT3B0aW9uKFxuICAgICAgICAgICAgJ3RpdGxlJywgb3B0aW9ucy50aXRsZSxcbiAgICAgICAgICAgIChoYXJuZXNzLCB0aXRsZSkgPT4gSGFybmVzc1ByZWRpY2F0ZS5zdHJpbmdNYXRjaGVzKGhhcm5lc3MuZ2V0VGl0bGUoKSwgdGl0bGUpKVxuICAgICAgICAuYWRkT3B0aW9uKFxuICAgICAgICAgICAgJ2Rlc2NyaXB0aW9uJywgb3B0aW9ucy5kZXNjcmlwdGlvbixcbiAgICAgICAgICAgIChoYXJuZXNzLCBkZXNjcmlwdGlvbikgPT5cbiAgICAgICAgICAgICAgICBIYXJuZXNzUHJlZGljYXRlLnN0cmluZ01hdGNoZXMoaGFybmVzcy5nZXREZXNjcmlwdGlvbigpLCBkZXNjcmlwdGlvbikpXG4gICAgICAgIC5hZGRPcHRpb24oXG4gICAgICAgICAgICAnY29udGVudCcsIG9wdGlvbnMuY29udGVudCxcbiAgICAgICAgICAgIChoYXJuZXNzLCBjb250ZW50KSA9PiBIYXJuZXNzUHJlZGljYXRlLnN0cmluZ01hdGNoZXMoaGFybmVzcy5nZXRUZXh0Q29udGVudCgpLCBjb250ZW50KSlcbiAgICAgICAgLmFkZE9wdGlvbihcbiAgICAgICAgICAgICdleHBhbmRlZCcsIG9wdGlvbnMuZXhwYW5kZWQsXG4gICAgICAgICAgICBhc3luYyAoaGFybmVzcywgZXhwYW5kZWQpID0+IChhd2FpdCBoYXJuZXNzLmlzRXhwYW5kZWQoKSkgPT09IGV4cGFuZGVkKVxuICAgICAgICAuYWRkT3B0aW9uKFxuICAgICAgICAgICAgJ2Rpc2FibGVkJywgb3B0aW9ucy5kaXNhYmxlZCxcbiAgICAgICAgICAgIGFzeW5jIChoYXJuZXNzLCBkaXNhYmxlZCkgPT4gKGF3YWl0IGhhcm5lc3MuaXNEaXNhYmxlZCgpKSA9PT0gZGlzYWJsZWQpO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIHBhbmVsIGlzIGV4cGFuZGVkLiAqL1xuICBhc3luYyBpc0V4cGFuZGVkKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmhhc0NsYXNzKCdtYXQtZXhwYW5kZWQnKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXRzIHRoZSB0aXRsZSB0ZXh0IG9mIHRoZSBwYW5lbC5cbiAgICogQHJldHVybnMgVGl0bGUgdGV4dCBvciBgbnVsbGAgaWYgbm8gdGl0bGUgaXMgc2V0IHVwLlxuICAgKi9cbiAgYXN5bmMgZ2V0VGl0bGUoKTogUHJvbWlzZTxzdHJpbmd8bnVsbD4ge1xuICAgIGNvbnN0IHRpdGxlRWwgPSBhd2FpdCB0aGlzLl90aXRsZSgpO1xuICAgIHJldHVybiB0aXRsZUVsID8gdGl0bGVFbC50ZXh0KCkgOiBudWxsO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgdGhlIGRlc2NyaXB0aW9uIHRleHQgb2YgdGhlIHBhbmVsLlxuICAgKiBAcmV0dXJucyBEZXNjcmlwdGlvbiB0ZXh0IG9yIGBudWxsYCBpZiBubyBkZXNjcmlwdGlvbiBpcyBzZXQgdXAuXG4gICAqL1xuICBhc3luYyBnZXREZXNjcmlwdGlvbigpOiBQcm9taXNlPHN0cmluZ3xudWxsPiB7XG4gICAgY29uc3QgZGVzY3JpcHRpb25FbCA9IGF3YWl0IHRoaXMuX2Rlc2NyaXB0aW9uKCk7XG4gICAgcmV0dXJuIGRlc2NyaXB0aW9uRWwgPyBkZXNjcmlwdGlvbkVsLnRleHQoKSA6IG51bGw7XG4gIH1cblxuICAvKiogV2hldGhlciB0aGUgcGFuZWwgaXMgZGlzYWJsZWQuICovXG4gIGFzeW5jIGlzRGlzYWJsZWQoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIGF3YWl0IChhd2FpdCB0aGlzLl9oZWFkZXIoKSkuZ2V0QXR0cmlidXRlKCdhcmlhLWRpc2FibGVkJykgPT09ICd0cnVlJztcbiAgfVxuXG4gIC8qKlxuICAgKiBUb2dnbGVzIHRoZSBleHBhbmRlZCBzdGF0ZSBvZiB0aGUgcGFuZWwgYnkgY2xpY2tpbmcgb24gdGhlIHBhbmVsXG4gICAqIGhlYWRlci4gVGhpcyBtZXRob2Qgd2lsbCBub3Qgd29yayBpZiB0aGUgcGFuZWwgaXMgZGlzYWJsZWQuXG4gICAqL1xuICBhc3luYyB0b2dnbGUoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgYXdhaXQgKGF3YWl0IHRoaXMuX2hlYWRlcigpKS5jbGljaygpO1xuICB9XG5cbiAgLyoqIEV4cGFuZHMgdGhlIGV4cGFuc2lvbiBwYW5lbCBpZiBjb2xsYXBzZWQuICovXG4gIGFzeW5jIGV4cGFuZCgpOiBQcm9taXNlPHZvaWQ+IHtcbiAgICBpZiAoIWF3YWl0IHRoaXMuaXNFeHBhbmRlZCgpKSB7XG4gICAgICBhd2FpdCB0aGlzLnRvZ2dsZSgpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBDb2xsYXBzZXMgdGhlIGV4cGFuc2lvbiBwYW5lbCBpZiBleHBhbmRlZC4gKi9cbiAgYXN5bmMgY29sbGFwc2UoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgaWYgKGF3YWl0IHRoaXMuaXNFeHBhbmRlZCgpKSB7XG4gICAgICBhd2FpdCB0aGlzLnRvZ2dsZSgpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSB0ZXh0IGNvbnRlbnQgb2YgdGhlIHBhbmVsLiAqL1xuICBhc3luYyBnZXRUZXh0Q29udGVudCgpOiBQcm9taXNlPHN0cmluZz4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5fY29udGVudCgpKS50ZXh0KCk7XG4gIH1cblxuICAvKipcbiAgICogR2V0cyBhIGBIYXJuZXNzTG9hZGVyYCB0aGF0IGNhbiBiZSB1c2VkIHRvIGxvYWQgaGFybmVzc2VzIGZvclxuICAgKiBjb21wb25lbnRzIHdpdGhpbiB0aGUgcGFuZWwncyBjb250ZW50IGFyZWEuXG4gICAqL1xuICBhc3luYyBnZXRIYXJuZXNzTG9hZGVyRm9yQ29udGVudCgpOiBQcm9taXNlPEhhcm5lc3NMb2FkZXI+IHtcbiAgICByZXR1cm4gdGhpcy5sb2NhdG9yRmFjdG9yeS5oYXJuZXNzTG9hZGVyRm9yKEVYUEFOU0lPTl9QQU5FTF9DT05URU5UX1NFTEVDVE9SKTtcbiAgfVxuXG4gIC8qKiBGb2N1c2VzIHRoZSBwYW5lbC4gKi9cbiAgYXN5bmMgZm9jdXMoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9oZWFkZXIoKSkuZm9jdXMoKTtcbiAgfVxuXG4gIC8qKiBCbHVycyB0aGUgcGFuZWwuICovXG4gIGFzeW5jIGJsdXIoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9oZWFkZXIoKSkuYmx1cigpO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIHBhbmVsIGhhcyBhIHRvZ2dsZSBpbmRpY2F0b3IgZGlzcGxheWVkLiAqL1xuICBhc3luYyBoYXNUb2dnbGVJbmRpY2F0b3IoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLl9leHBhbnNpb25JbmRpY2F0b3IoKSkgIT09IG51bGw7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgcG9zaXRpb24gb2YgdGhlIHRvZ2dsZSBpbmRpY2F0b3IuICovXG4gIGFzeW5jIGdldFRvZ2dsZUluZGljYXRvclBvc2l0aW9uKCk6IFByb21pc2U8J2JlZm9yZSd8J2FmdGVyJz4ge1xuICAgIC8vIEJ5IGRlZmF1bHQgdGhlIGV4cGFuc2lvbiBpbmRpY2F0b3Igd2lsbCBzaG93IFwiYWZ0ZXJcIiB0aGUgcGFuZWwgaGVhZGVyIGNvbnRlbnQuXG4gICAgaWYgKGF3YWl0IChhd2FpdCB0aGlzLl9oZWFkZXIoKSkuaGFzQ2xhc3MoJ21hdC1leHBhbnNpb24tdG9nZ2xlLWluZGljYXRvci1iZWZvcmUnKSkge1xuICAgICAgcmV0dXJuICdiZWZvcmUnO1xuICAgIH1cbiAgICByZXR1cm4gJ2FmdGVyJztcbiAgfVxufVxuIl19