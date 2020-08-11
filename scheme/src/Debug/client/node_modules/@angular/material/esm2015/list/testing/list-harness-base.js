/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness } from '@angular/cdk/testing';
import { MatDividerHarness } from '@angular/material/divider/testing';
import { MatSubheaderHarness } from './list-item-harness-base';
/**
 * Shared behavior among the harnesses for the various `MatList` flavors.
 * @template T A constructor type for a list item harness type used by this list harness.
 * @template C The list item harness type that `T` constructs.
 * @template F The filter type used filter list item harness of type `C`.
 * @docs-private
 */
export class MatListHarnessBase extends ComponentHarness {
    /**
     * Gets a list of harnesses representing the items in this list.
     * @param filters Optional filters used to narrow which harnesses are included
     * @return The list of items matching the given filters.
     */
    getItems(filters) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.locatorForAll(this._itemHarness.with(filters))();
        });
    }
    /**
     * Gets a list of `ListSection` representing the list items grouped by subheaders. If the list has
     * no subheaders it is represented as a single `ListSection` with an undefined `heading` property.
     * @param filters Optional filters used to narrow which list item harnesses are included
     * @return The list of items matching the given filters, grouped into sections by subheader.
     */
    getItemsGroupedBySubheader(filters) {
        return __awaiter(this, void 0, void 0, function* () {
            const listSections = [];
            let currentSection = { items: [] };
            const itemsAndSubheaders = yield this.getItemsWithSubheadersAndDividers({ item: filters, divider: false });
            for (const itemOrSubheader of itemsAndSubheaders) {
                if (itemOrSubheader instanceof MatSubheaderHarness) {
                    if (currentSection.heading !== undefined || currentSection.items.length) {
                        listSections.push(currentSection);
                    }
                    currentSection = { heading: yield itemOrSubheader.getText(), items: [] };
                }
                else {
                    currentSection.items.push(itemOrSubheader);
                }
            }
            if (currentSection.heading !== undefined || currentSection.items.length ||
                !listSections.length) {
                listSections.push(currentSection);
            }
            return listSections;
        });
    }
    /**
     * Gets a list of sub-lists representing the list items grouped by dividers. If the list has no
     * dividers it is represented as a list with a single sub-list.
     * @param filters Optional filters used to narrow which list item harnesses are included
     * @return The list of items matching the given filters, grouped into sub-lists by divider.
     */
    getItemsGroupedByDividers(filters) {
        return __awaiter(this, void 0, void 0, function* () {
            const listSections = [[]];
            const itemsAndDividers = yield this.getItemsWithSubheadersAndDividers({ item: filters, subheader: false });
            for (const itemOrDivider of itemsAndDividers) {
                if (itemOrDivider instanceof MatDividerHarness) {
                    listSections.push([]);
                }
                else {
                    listSections[listSections.length - 1].push(itemOrDivider);
                }
            }
            return listSections;
        });
    }
    getItemsWithSubheadersAndDividers(filters = {}) {
        return __awaiter(this, void 0, void 0, function* () {
            const query = [];
            if (filters.item !== false) {
                query.push(this._itemHarness.with(filters.item || {}));
            }
            if (filters.subheader !== false) {
                query.push(MatSubheaderHarness.with(filters.subheader));
            }
            if (filters.divider !== false) {
                query.push(MatDividerHarness.with(filters.divider));
            }
            return this.locatorForAll(...query)();
        });
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibGlzdC1oYXJuZXNzLWJhc2UuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvbGlzdC90ZXN0aW5nL2xpc3QtaGFybmVzcy1iYXNlLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7QUFFSCxPQUFPLEVBQ0wsZ0JBQWdCLEVBR2pCLE1BQU0sc0JBQXNCLENBQUM7QUFDOUIsT0FBTyxFQUF3QixpQkFBaUIsRUFBQyxNQUFNLG1DQUFtQyxDQUFDO0FBRTNGLE9BQU8sRUFBQyxtQkFBbUIsRUFBQyxNQUFNLDBCQUEwQixDQUFDO0FBVzdEOzs7Ozs7R0FNRztBQUNILE1BQU0sT0FBTyxrQkFLUCxTQUFRLGdCQUFnQjtJQUc1Qjs7OztPQUlHO0lBQ0csUUFBUSxDQUFDLE9BQVc7O1lBQ3hCLE9BQU8sSUFBSSxDQUFDLGFBQWEsQ0FBQyxJQUFJLENBQUMsWUFBWSxDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsQ0FBQyxFQUFFLENBQUM7UUFDL0QsQ0FBQztLQUFBO0lBRUQ7Ozs7O09BS0c7SUFDRywwQkFBMEIsQ0FBQyxPQUFXOztZQUMxQyxNQUFNLFlBQVksR0FBRyxFQUFFLENBQUM7WUFDeEIsSUFBSSxjQUFjLEdBQW1CLEVBQUMsS0FBSyxFQUFFLEVBQUUsRUFBQyxDQUFDO1lBQ2pELE1BQU0sa0JBQWtCLEdBQ3BCLE1BQU0sSUFBSSxDQUFDLGlDQUFpQyxDQUFDLEVBQUMsSUFBSSxFQUFFLE9BQU8sRUFBRSxPQUFPLEVBQUUsS0FBSyxFQUFDLENBQUMsQ0FBQztZQUNsRixLQUFLLE1BQU0sZUFBZSxJQUFJLGtCQUFrQixFQUFFO2dCQUNoRCxJQUFJLGVBQWUsWUFBWSxtQkFBbUIsRUFBRTtvQkFDbEQsSUFBSSxjQUFjLENBQUMsT0FBTyxLQUFLLFNBQVMsSUFBSSxjQUFjLENBQUMsS0FBSyxDQUFDLE1BQU0sRUFBRTt3QkFDdkUsWUFBWSxDQUFDLElBQUksQ0FBQyxjQUFjLENBQUMsQ0FBQztxQkFDbkM7b0JBQ0QsY0FBYyxHQUFHLEVBQUMsT0FBTyxFQUFFLE1BQU0sZUFBZSxDQUFDLE9BQU8sRUFBRSxFQUFFLEtBQUssRUFBRSxFQUFFLEVBQUMsQ0FBQztpQkFDeEU7cUJBQU07b0JBQ0wsY0FBYyxDQUFDLEtBQUssQ0FBQyxJQUFJLENBQUMsZUFBZSxDQUFDLENBQUM7aUJBQzVDO2FBQ0Y7WUFDRCxJQUFJLGNBQWMsQ0FBQyxPQUFPLEtBQUssU0FBUyxJQUFJLGNBQWMsQ0FBQyxLQUFLLENBQUMsTUFBTTtnQkFDbkUsQ0FBQyxZQUFZLENBQUMsTUFBTSxFQUFFO2dCQUN4QixZQUFZLENBQUMsSUFBSSxDQUFDLGNBQWMsQ0FBQyxDQUFDO2FBQ25DO1lBQ0QsT0FBTyxZQUFZLENBQUM7UUFDdEIsQ0FBQztLQUFBO0lBRUQ7Ozs7O09BS0c7SUFDRyx5QkFBeUIsQ0FBQyxPQUFXOztZQUN6QyxNQUFNLFlBQVksR0FBVSxDQUFDLEVBQUUsQ0FBQyxDQUFDO1lBQ2pDLE1BQU0sZ0JBQWdCLEdBQ2xCLE1BQU0sSUFBSSxDQUFDLGlDQUFpQyxDQUFDLEVBQUMsSUFBSSxFQUFFLE9BQU8sRUFBRSxTQUFTLEVBQUUsS0FBSyxFQUFDLENBQUMsQ0FBQztZQUNwRixLQUFLLE1BQU0sYUFBYSxJQUFJLGdCQUFnQixFQUFFO2dCQUM1QyxJQUFJLGFBQWEsWUFBWSxpQkFBaUIsRUFBRTtvQkFDOUMsWUFBWSxDQUFDLElBQUksQ0FBQyxFQUFFLENBQUMsQ0FBQztpQkFDdkI7cUJBQU07b0JBQ0wsWUFBWSxDQUFDLFlBQVksQ0FBQyxNQUFNLEdBQUcsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLGFBQWEsQ0FBQyxDQUFDO2lCQUMzRDthQUNGO1lBQ0QsT0FBTyxZQUFZLENBQUM7UUFDdEIsQ0FBQztLQUFBO0lBb0RLLGlDQUFpQyxDQUFDLFVBSXBDLEVBQUU7O1lBQ0osTUFBTSxLQUFLLEdBQUcsRUFBRSxDQUFDO1lBQ2pCLElBQUksT0FBTyxDQUFDLElBQUksS0FBSyxLQUFLLEVBQUU7Z0JBQzFCLEtBQUssQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLFlBQVksQ0FBQyxJQUFJLENBQUMsT0FBTyxDQUFDLElBQUksSUFBSSxFQUFPLENBQUMsQ0FBQyxDQUFDO2FBQzdEO1lBQ0QsSUFBSSxPQUFPLENBQUMsU0FBUyxLQUFLLEtBQUssRUFBRTtnQkFDL0IsS0FBSyxDQUFDLElBQUksQ0FBQyxtQkFBbUIsQ0FBQyxJQUFJLENBQUMsT0FBTyxDQUFDLFNBQVMsQ0FBQyxDQUFDLENBQUM7YUFDekQ7WUFDRCxJQUFJLE9BQU8sQ0FBQyxPQUFPLEtBQUssS0FBSyxFQUFFO2dCQUM3QixLQUFLLENBQUMsSUFBSSxDQUFDLGlCQUFpQixDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsT0FBTyxDQUFDLENBQUMsQ0FBQzthQUNyRDtZQUNELE9BQU8sSUFBSSxDQUFDLGFBQWEsQ0FBQyxHQUFHLEtBQUssQ0FBQyxFQUFFLENBQUM7UUFDeEMsQ0FBQztLQUFBO0NBQ0YiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtcbiAgQ29tcG9uZW50SGFybmVzcyxcbiAgQ29tcG9uZW50SGFybmVzc0NvbnN0cnVjdG9yLFxuICBIYXJuZXNzUHJlZGljYXRlXG59IGZyb20gJ0Bhbmd1bGFyL2Nkay90ZXN0aW5nJztcbmltcG9ydCB7RGl2aWRlckhhcm5lc3NGaWx0ZXJzLCBNYXREaXZpZGVySGFybmVzc30gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvZGl2aWRlci90ZXN0aW5nJztcbmltcG9ydCB7QmFzZUxpc3RJdGVtSGFybmVzc0ZpbHRlcnMsIFN1YmhlYWRlckhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL2xpc3QtaGFybmVzcy1maWx0ZXJzJztcbmltcG9ydCB7TWF0U3ViaGVhZGVySGFybmVzc30gZnJvbSAnLi9saXN0LWl0ZW0taGFybmVzcy1iYXNlJztcblxuLyoqIFJlcHJlc2VudHMgYSBzZWN0aW9uIG9mIGEgbGlzdCBmYWxsaW5nIHVuZGVyIGEgc3BlY2lmaWMgaGVhZGVyLiAqL1xuZXhwb3J0IGludGVyZmFjZSBMaXN0U2VjdGlvbjxJPiB7XG4gIC8qKiBUaGUgaGVhZGluZyBmb3IgdGhpcyBsaXN0IHNlY3Rpb24uIGB1bmRlZmluZWRgIGlmIHRoZXJlIGlzIG5vIGhlYWRpbmcuICovXG4gIGhlYWRpbmc/OiBzdHJpbmc7XG5cbiAgLyoqIFRoZSBpdGVtcyBpbiB0aGlzIGxpc3Qgc2VjdGlvbi4gKi9cbiAgaXRlbXM6IElbXTtcbn1cblxuLyoqXG4gKiBTaGFyZWQgYmVoYXZpb3IgYW1vbmcgdGhlIGhhcm5lc3NlcyBmb3IgdGhlIHZhcmlvdXMgYE1hdExpc3RgIGZsYXZvcnMuXG4gKiBAdGVtcGxhdGUgVCBBIGNvbnN0cnVjdG9yIHR5cGUgZm9yIGEgbGlzdCBpdGVtIGhhcm5lc3MgdHlwZSB1c2VkIGJ5IHRoaXMgbGlzdCBoYXJuZXNzLlxuICogQHRlbXBsYXRlIEMgVGhlIGxpc3QgaXRlbSBoYXJuZXNzIHR5cGUgdGhhdCBgVGAgY29uc3RydWN0cy5cbiAqIEB0ZW1wbGF0ZSBGIFRoZSBmaWx0ZXIgdHlwZSB1c2VkIGZpbHRlciBsaXN0IGl0ZW0gaGFybmVzcyBvZiB0eXBlIGBDYC5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuZXhwb3J0IGNsYXNzIE1hdExpc3RIYXJuZXNzQmFzZVxuICAgIDxcbiAgICAgIFQgZXh0ZW5kcyAoQ29tcG9uZW50SGFybmVzc0NvbnN0cnVjdG9yPEM+ICYge3dpdGg6IChvcHRpb25zPzogRikgPT4gSGFybmVzc1ByZWRpY2F0ZTxDPn0pLFxuICAgICAgQyBleHRlbmRzIENvbXBvbmVudEhhcm5lc3MsXG4gICAgICBGIGV4dGVuZHMgQmFzZUxpc3RJdGVtSGFybmVzc0ZpbHRlcnNcbiAgICA+IGV4dGVuZHMgQ29tcG9uZW50SGFybmVzcyB7XG4gIHByb3RlY3RlZCBfaXRlbUhhcm5lc3M6IFQ7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBsaXN0IG9mIGhhcm5lc3NlcyByZXByZXNlbnRpbmcgdGhlIGl0ZW1zIGluIHRoaXMgbGlzdC5cbiAgICogQHBhcmFtIGZpbHRlcnMgT3B0aW9uYWwgZmlsdGVycyB1c2VkIHRvIG5hcnJvdyB3aGljaCBoYXJuZXNzZXMgYXJlIGluY2x1ZGVkXG4gICAqIEByZXR1cm4gVGhlIGxpc3Qgb2YgaXRlbXMgbWF0Y2hpbmcgdGhlIGdpdmVuIGZpbHRlcnMuXG4gICAqL1xuICBhc3luYyBnZXRJdGVtcyhmaWx0ZXJzPzogRik6IFByb21pc2U8Q1tdPiB7XG4gICAgcmV0dXJuIHRoaXMubG9jYXRvckZvckFsbCh0aGlzLl9pdGVtSGFybmVzcy53aXRoKGZpbHRlcnMpKSgpO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgYSBsaXN0IG9mIGBMaXN0U2VjdGlvbmAgcmVwcmVzZW50aW5nIHRoZSBsaXN0IGl0ZW1zIGdyb3VwZWQgYnkgc3ViaGVhZGVycy4gSWYgdGhlIGxpc3QgaGFzXG4gICAqIG5vIHN1YmhlYWRlcnMgaXQgaXMgcmVwcmVzZW50ZWQgYXMgYSBzaW5nbGUgYExpc3RTZWN0aW9uYCB3aXRoIGFuIHVuZGVmaW5lZCBgaGVhZGluZ2AgcHJvcGVydHkuXG4gICAqIEBwYXJhbSBmaWx0ZXJzIE9wdGlvbmFsIGZpbHRlcnMgdXNlZCB0byBuYXJyb3cgd2hpY2ggbGlzdCBpdGVtIGhhcm5lc3NlcyBhcmUgaW5jbHVkZWRcbiAgICogQHJldHVybiBUaGUgbGlzdCBvZiBpdGVtcyBtYXRjaGluZyB0aGUgZ2l2ZW4gZmlsdGVycywgZ3JvdXBlZCBpbnRvIHNlY3Rpb25zIGJ5IHN1YmhlYWRlci5cbiAgICovXG4gIGFzeW5jIGdldEl0ZW1zR3JvdXBlZEJ5U3ViaGVhZGVyKGZpbHRlcnM/OiBGKTogUHJvbWlzZTxMaXN0U2VjdGlvbjxDPltdPiB7XG4gICAgY29uc3QgbGlzdFNlY3Rpb25zID0gW107XG4gICAgbGV0IGN1cnJlbnRTZWN0aW9uOiBMaXN0U2VjdGlvbjxDPiA9IHtpdGVtczogW119O1xuICAgIGNvbnN0IGl0ZW1zQW5kU3ViaGVhZGVycyA9XG4gICAgICAgIGF3YWl0IHRoaXMuZ2V0SXRlbXNXaXRoU3ViaGVhZGVyc0FuZERpdmlkZXJzKHtpdGVtOiBmaWx0ZXJzLCBkaXZpZGVyOiBmYWxzZX0pO1xuICAgIGZvciAoY29uc3QgaXRlbU9yU3ViaGVhZGVyIG9mIGl0ZW1zQW5kU3ViaGVhZGVycykge1xuICAgICAgaWYgKGl0ZW1PclN1YmhlYWRlciBpbnN0YW5jZW9mIE1hdFN1YmhlYWRlckhhcm5lc3MpIHtcbiAgICAgICAgaWYgKGN1cnJlbnRTZWN0aW9uLmhlYWRpbmcgIT09IHVuZGVmaW5lZCB8fCBjdXJyZW50U2VjdGlvbi5pdGVtcy5sZW5ndGgpIHtcbiAgICAgICAgICBsaXN0U2VjdGlvbnMucHVzaChjdXJyZW50U2VjdGlvbik7XG4gICAgICAgIH1cbiAgICAgICAgY3VycmVudFNlY3Rpb24gPSB7aGVhZGluZzogYXdhaXQgaXRlbU9yU3ViaGVhZGVyLmdldFRleHQoKSwgaXRlbXM6IFtdfTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIGN1cnJlbnRTZWN0aW9uLml0ZW1zLnB1c2goaXRlbU9yU3ViaGVhZGVyKTtcbiAgICAgIH1cbiAgICB9XG4gICAgaWYgKGN1cnJlbnRTZWN0aW9uLmhlYWRpbmcgIT09IHVuZGVmaW5lZCB8fCBjdXJyZW50U2VjdGlvbi5pdGVtcy5sZW5ndGggfHxcbiAgICAgICAgIWxpc3RTZWN0aW9ucy5sZW5ndGgpIHtcbiAgICAgIGxpc3RTZWN0aW9ucy5wdXNoKGN1cnJlbnRTZWN0aW9uKTtcbiAgICB9XG4gICAgcmV0dXJuIGxpc3RTZWN0aW9ucztcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXRzIGEgbGlzdCBvZiBzdWItbGlzdHMgcmVwcmVzZW50aW5nIHRoZSBsaXN0IGl0ZW1zIGdyb3VwZWQgYnkgZGl2aWRlcnMuIElmIHRoZSBsaXN0IGhhcyBub1xuICAgKiBkaXZpZGVycyBpdCBpcyByZXByZXNlbnRlZCBhcyBhIGxpc3Qgd2l0aCBhIHNpbmdsZSBzdWItbGlzdC5cbiAgICogQHBhcmFtIGZpbHRlcnMgT3B0aW9uYWwgZmlsdGVycyB1c2VkIHRvIG5hcnJvdyB3aGljaCBsaXN0IGl0ZW0gaGFybmVzc2VzIGFyZSBpbmNsdWRlZFxuICAgKiBAcmV0dXJuIFRoZSBsaXN0IG9mIGl0ZW1zIG1hdGNoaW5nIHRoZSBnaXZlbiBmaWx0ZXJzLCBncm91cGVkIGludG8gc3ViLWxpc3RzIGJ5IGRpdmlkZXIuXG4gICAqL1xuICBhc3luYyBnZXRJdGVtc0dyb3VwZWRCeURpdmlkZXJzKGZpbHRlcnM/OiBGKTogUHJvbWlzZTxDW11bXT4ge1xuICAgIGNvbnN0IGxpc3RTZWN0aW9uczogQ1tdW10gPSBbW11dO1xuICAgIGNvbnN0IGl0ZW1zQW5kRGl2aWRlcnMgPVxuICAgICAgICBhd2FpdCB0aGlzLmdldEl0ZW1zV2l0aFN1YmhlYWRlcnNBbmREaXZpZGVycyh7aXRlbTogZmlsdGVycywgc3ViaGVhZGVyOiBmYWxzZX0pO1xuICAgIGZvciAoY29uc3QgaXRlbU9yRGl2aWRlciBvZiBpdGVtc0FuZERpdmlkZXJzKSB7XG4gICAgICBpZiAoaXRlbU9yRGl2aWRlciBpbnN0YW5jZW9mIE1hdERpdmlkZXJIYXJuZXNzKSB7XG4gICAgICAgIGxpc3RTZWN0aW9ucy5wdXNoKFtdKTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIGxpc3RTZWN0aW9uc1tsaXN0U2VjdGlvbnMubGVuZ3RoIC0gMV0ucHVzaChpdGVtT3JEaXZpZGVyKTtcbiAgICAgIH1cbiAgICB9XG4gICAgcmV0dXJuIGxpc3RTZWN0aW9ucztcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXRzIGEgbGlzdCBvZiBoYXJuZXNzZXMgcmVwcmVzZW50aW5nIGFsbCBvZiB0aGUgaXRlbXMsIHN1YmhlYWRlcnMsIGFuZCBkaXZpZGVyc1xuICAgKiAoaW4gdGhlIG9yZGVyIHRoZXkgYXBwZWFyIGluIHRoZSBsaXN0KS4gVXNlIGBpbnN0YW5jZW9mYCB0byBjaGVjayB3aGljaCB0eXBlIG9mIGhhcm5lc3MgYSBnaXZlblxuICAgKiBpdGVtIGlzLlxuICAgKiBAcGFyYW0gZmlsdGVycyBPcHRpb25hbCBmaWx0ZXJzIHVzZWQgdG8gbmFycm93IHdoaWNoIGxpc3QgaXRlbXMsIHN1YmhlYWRlcnMsIGFuZCBkaXZpZGVycyBhcmVcbiAgICogICAgIGluY2x1ZGVkLiBBIHZhbHVlIG9mIGBmYWxzZWAgZm9yIHRoZSBgaXRlbWAsIGBzdWJoZWFkZXJgLCBvciBgZGl2aWRlcmAgcHJvcGVydGllcyBpbmRpY2F0ZXNcbiAgICogICAgIHRoYXQgdGhlIHJlc3BlY3RpdmUgaGFybmVzcyB0eXBlIHNob3VsZCBiZSBvbWl0dGVkIGNvbXBsZXRlbHkuXG4gICAqIEByZXR1cm4gVGhlIGxpc3Qgb2YgaGFybmVzc2VzIHJlcHJlc2VudGluZyB0aGUgaXRlbXMsIHN1YmhlYWRlcnMsIGFuZCBkaXZpZGVycyBtYXRjaGluZyB0aGVcbiAgICogICAgIGdpdmVuIGZpbHRlcnMuXG4gICAqL1xuICBnZXRJdGVtc1dpdGhTdWJoZWFkZXJzQW5kRGl2aWRlcnMoZmlsdGVyczoge1xuICAgIGl0ZW06IGZhbHNlLFxuICAgIHN1YmhlYWRlcjogZmFsc2UsXG4gICAgZGl2aWRlcjogZmFsc2VcbiAgfSk6IFByb21pc2U8W10+O1xuICBnZXRJdGVtc1dpdGhTdWJoZWFkZXJzQW5kRGl2aWRlcnMoZmlsdGVyczoge1xuICAgIGl0ZW0/OiBGIHwgZmFsc2UsXG4gICAgc3ViaGVhZGVyOiBmYWxzZSxcbiAgICBkaXZpZGVyOiBmYWxzZVxuICB9KTogUHJvbWlzZTxDW10+O1xuICBnZXRJdGVtc1dpdGhTdWJoZWFkZXJzQW5kRGl2aWRlcnMoZmlsdGVyczoge1xuICAgIGl0ZW06IGZhbHNlLFxuICAgIHN1YmhlYWRlcj86IFN1YmhlYWRlckhhcm5lc3NGaWx0ZXJzIHwgZmFsc2UsXG4gICAgZGl2aWRlcjogZmFsc2VcbiAgfSk6IFByb21pc2U8TWF0U3ViaGVhZGVySGFybmVzc1tdPjtcbiAgZ2V0SXRlbXNXaXRoU3ViaGVhZGVyc0FuZERpdmlkZXJzKGZpbHRlcnM6IHtcbiAgICBpdGVtOiBmYWxzZSxcbiAgICBzdWJoZWFkZXI6IGZhbHNlLFxuICAgIGRpdmlkZXI/OiBEaXZpZGVySGFybmVzc0ZpbHRlcnMgfCBmYWxzZVxuICB9KTogUHJvbWlzZTxNYXREaXZpZGVySGFybmVzc1tdPjtcbiAgZ2V0SXRlbXNXaXRoU3ViaGVhZGVyc0FuZERpdmlkZXJzKGZpbHRlcnM6IHtcbiAgICBpdGVtPzogRiB8IGZhbHNlLFxuICAgIHN1YmhlYWRlcj86IFN1YmhlYWRlckhhcm5lc3NGaWx0ZXJzIHwgZmFsc2UsXG4gICAgZGl2aWRlcjogZmFsc2VcbiAgfSk6IFByb21pc2U8KEMgfCBNYXRTdWJoZWFkZXJIYXJuZXNzKVtdPjtcbiAgZ2V0SXRlbXNXaXRoU3ViaGVhZGVyc0FuZERpdmlkZXJzKGZpbHRlcnM6IHtcbiAgICBpdGVtPzogRiB8IGZhbHNlLFxuICAgIHN1YmhlYWRlcjogZmFsc2UsXG4gICAgZGl2aWRlcj86IGZhbHNlIHwgRGl2aWRlckhhcm5lc3NGaWx0ZXJzXG4gIH0pOiBQcm9taXNlPChDIHwgTWF0RGl2aWRlckhhcm5lc3MpW10+O1xuICBnZXRJdGVtc1dpdGhTdWJoZWFkZXJzQW5kRGl2aWRlcnMoZmlsdGVyczoge1xuICAgIGl0ZW06IGZhbHNlLFxuICAgIHN1YmhlYWRlcj86IGZhbHNlIHwgU3ViaGVhZGVySGFybmVzc0ZpbHRlcnMsXG4gICAgZGl2aWRlcj86IGZhbHNlIHwgRGl2aWRlckhhcm5lc3NGaWx0ZXJzXG4gIH0pOiBQcm9taXNlPChNYXRTdWJoZWFkZXJIYXJuZXNzIHwgTWF0RGl2aWRlckhhcm5lc3MpW10+O1xuICBnZXRJdGVtc1dpdGhTdWJoZWFkZXJzQW5kRGl2aWRlcnMoZmlsdGVycz86IHtcbiAgICBpdGVtPzogRiB8IGZhbHNlLFxuICAgIHN1YmhlYWRlcj86IFN1YmhlYWRlckhhcm5lc3NGaWx0ZXJzIHwgZmFsc2UsXG4gICAgZGl2aWRlcj86IERpdmlkZXJIYXJuZXNzRmlsdGVycyB8IGZhbHNlXG4gIH0pOiBQcm9taXNlPChDIHwgTWF0U3ViaGVhZGVySGFybmVzcyB8IE1hdERpdmlkZXJIYXJuZXNzKVtdPjtcbiAgYXN5bmMgZ2V0SXRlbXNXaXRoU3ViaGVhZGVyc0FuZERpdmlkZXJzKGZpbHRlcnM6IHtcbiAgICBpdGVtPzogRiB8IGZhbHNlLFxuICAgIHN1YmhlYWRlcj86IFN1YmhlYWRlckhhcm5lc3NGaWx0ZXJzIHwgZmFsc2UsXG4gICAgZGl2aWRlcj86IERpdmlkZXJIYXJuZXNzRmlsdGVycyB8IGZhbHNlXG4gIH0gPSB7fSk6IFByb21pc2U8KEMgfCBNYXRTdWJoZWFkZXJIYXJuZXNzIHwgTWF0RGl2aWRlckhhcm5lc3MpW10+IHtcbiAgICBjb25zdCBxdWVyeSA9IFtdO1xuICAgIGlmIChmaWx0ZXJzLml0ZW0gIT09IGZhbHNlKSB7XG4gICAgICBxdWVyeS5wdXNoKHRoaXMuX2l0ZW1IYXJuZXNzLndpdGgoZmlsdGVycy5pdGVtIHx8IHt9IGFzIEYpKTtcbiAgICB9XG4gICAgaWYgKGZpbHRlcnMuc3ViaGVhZGVyICE9PSBmYWxzZSkge1xuICAgICAgcXVlcnkucHVzaChNYXRTdWJoZWFkZXJIYXJuZXNzLndpdGgoZmlsdGVycy5zdWJoZWFkZXIpKTtcbiAgICB9XG4gICAgaWYgKGZpbHRlcnMuZGl2aWRlciAhPT0gZmFsc2UpIHtcbiAgICAgIHF1ZXJ5LnB1c2goTWF0RGl2aWRlckhhcm5lc3Mud2l0aChmaWx0ZXJzLmRpdmlkZXIpKTtcbiAgICB9XG4gICAgcmV0dXJuIHRoaXMubG9jYXRvckZvckFsbCguLi5xdWVyeSkoKTtcbiAgfVxufVxuIl19