/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MatRowHarness, MatHeaderRowHarness, MatFooterRowHarness, } from './row-harness';
/** Harness for interacting with a standard mat-table in tests. */
let MatTableHarness = /** @class */ (() => {
    class MatTableHarness extends ComponentHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a table with specific attributes.
         * @param options Options for narrowing the search
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatTableHarness, options);
        }
        /** Gets all of the header rows in a table. */
        getHeaderRows(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatHeaderRowHarness.with(filter))();
            });
        }
        /** Gets all of the regular data rows in a table. */
        getRows(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatRowHarness.with(filter))();
            });
        }
        /** Gets all of the footer rows in a table. */
        getFooterRows(filter = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return this.locatorForAll(MatFooterRowHarness.with(filter))();
            });
        }
        /** Gets the text inside the entire table organized by rows. */
        getCellTextByIndex() {
            return __awaiter(this, void 0, void 0, function* () {
                const rows = yield this.getRows();
                return Promise.all(rows.map(row => row.getCellTextByIndex()));
            });
        }
        /** Gets the text inside the entire table organized by columns. */
        getCellTextByColumnName() {
            return __awaiter(this, void 0, void 0, function* () {
                const [headerRows, footerRows, dataRows] = yield Promise.all([
                    this.getHeaderRows(),
                    this.getFooterRows(),
                    this.getRows()
                ]);
                const text = {};
                const [headerData, footerData, rowsData] = yield Promise.all([
                    Promise.all(headerRows.map(row => row.getCellTextByColumnName())),
                    Promise.all(footerRows.map(row => row.getCellTextByColumnName())),
                    Promise.all(dataRows.map(row => row.getCellTextByColumnName())),
                ]);
                rowsData.forEach(data => {
                    Object.keys(data).forEach(columnName => {
                        const cellText = data[columnName];
                        if (!text[columnName]) {
                            text[columnName] = {
                                headerText: getCellTextsByColumn(headerData, columnName),
                                footerText: getCellTextsByColumn(footerData, columnName),
                                text: []
                            };
                        }
                        text[columnName].text.push(cellText);
                    });
                });
                return text;
            });
        }
    }
    /** The selector for the host element of a `MatTableHarness` instance. */
    MatTableHarness.hostSelector = '.mat-table';
    return MatTableHarness;
})();
export { MatTableHarness };
/** Extracts the text of cells only under a particular column. */
function getCellTextsByColumn(rowsData, column) {
    const columnTexts = [];
    rowsData.forEach(data => {
        Object.keys(data).forEach(columnName => {
            if (columnName === column) {
                columnTexts.push(data[columnName]);
            }
        });
    });
    return columnTexts;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidGFibGUtaGFybmVzcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC90YWJsZS90ZXN0aW5nL3RhYmxlLWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBRXhFLE9BQU8sRUFDTCxhQUFhLEVBQ2IsbUJBQW1CLEVBQ25CLG1CQUFtQixHQUVwQixNQUFNLGVBQWUsQ0FBQztBQVd2QixrRUFBa0U7QUFDbEU7SUFBQSxNQUFhLGVBQWdCLFNBQVEsZ0JBQWdCO1FBSW5EOzs7O1dBSUc7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQStCLEVBQUU7WUFDM0MsT0FBTyxJQUFJLGdCQUFnQixDQUFDLGVBQWUsRUFBRSxPQUFPLENBQUMsQ0FBQztRQUN4RCxDQUFDO1FBRUQsOENBQThDO1FBQ3hDLGFBQWEsQ0FBQyxTQUE0QixFQUFFOztnQkFDaEQsT0FBTyxJQUFJLENBQUMsYUFBYSxDQUFDLG1CQUFtQixDQUFDLElBQUksQ0FBQyxNQUFNLENBQUMsQ0FBQyxFQUFFLENBQUM7WUFDaEUsQ0FBQztTQUFBO1FBRUQsb0RBQW9EO1FBQzlDLE9BQU8sQ0FBQyxTQUE0QixFQUFFOztnQkFDMUMsT0FBTyxJQUFJLENBQUMsYUFBYSxDQUFDLGFBQWEsQ0FBQyxJQUFJLENBQUMsTUFBTSxDQUFDLENBQUMsRUFBRSxDQUFDO1lBQzFELENBQUM7U0FBQTtRQUVELDhDQUE4QztRQUN4QyxhQUFhLENBQUMsU0FBNEIsRUFBRTs7Z0JBQ2hELE9BQU8sSUFBSSxDQUFDLGFBQWEsQ0FBQyxtQkFBbUIsQ0FBQyxJQUFJLENBQUMsTUFBTSxDQUFDLENBQUMsRUFBRSxDQUFDO1lBQ2hFLENBQUM7U0FBQTtRQUVELCtEQUErRDtRQUN6RCxrQkFBa0I7O2dCQUN0QixNQUFNLElBQUksR0FBRyxNQUFNLElBQUksQ0FBQyxPQUFPLEVBQUUsQ0FBQztnQkFDbEMsT0FBTyxPQUFPLENBQUMsR0FBRyxDQUFDLElBQUksQ0FBQyxHQUFHLENBQUMsR0FBRyxDQUFDLEVBQUUsQ0FBQyxHQUFHLENBQUMsa0JBQWtCLEVBQUUsQ0FBQyxDQUFDLENBQUM7WUFDaEUsQ0FBQztTQUFBO1FBRUQsa0VBQWtFO1FBQzVELHVCQUF1Qjs7Z0JBQzNCLE1BQU0sQ0FBQyxVQUFVLEVBQUUsVUFBVSxFQUFFLFFBQVEsQ0FBQyxHQUFHLE1BQU0sT0FBTyxDQUFDLEdBQUcsQ0FBQztvQkFDM0QsSUFBSSxDQUFDLGFBQWEsRUFBRTtvQkFDcEIsSUFBSSxDQUFDLGFBQWEsRUFBRTtvQkFDcEIsSUFBSSxDQUFDLE9BQU8sRUFBRTtpQkFDZixDQUFDLENBQUM7Z0JBRUgsTUFBTSxJQUFJLEdBQStCLEVBQUUsQ0FBQztnQkFDNUMsTUFBTSxDQUFDLFVBQVUsRUFBRSxVQUFVLEVBQUUsUUFBUSxDQUFDLEdBQUcsTUFBTSxPQUFPLENBQUMsR0FBRyxDQUFDO29CQUMzRCxPQUFPLENBQUMsR0FBRyxDQUFDLFVBQVUsQ0FBQyxHQUFHLENBQUMsR0FBRyxDQUFDLEVBQUUsQ0FBQyxHQUFHLENBQUMsdUJBQXVCLEVBQUUsQ0FBQyxDQUFDO29CQUNqRSxPQUFPLENBQUMsR0FBRyxDQUFDLFVBQVUsQ0FBQyxHQUFHLENBQUMsR0FBRyxDQUFDLEVBQUUsQ0FBQyxHQUFHLENBQUMsdUJBQXVCLEVBQUUsQ0FBQyxDQUFDO29CQUNqRSxPQUFPLENBQUMsR0FBRyxDQUFDLFFBQVEsQ0FBQyxHQUFHLENBQUMsR0FBRyxDQUFDLEVBQUUsQ0FBQyxHQUFHLENBQUMsdUJBQXVCLEVBQUUsQ0FBQyxDQUFDO2lCQUNoRSxDQUFDLENBQUM7Z0JBRUgsUUFBUSxDQUFDLE9BQU8sQ0FBQyxJQUFJLENBQUMsRUFBRTtvQkFDdEIsTUFBTSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQyxPQUFPLENBQUMsVUFBVSxDQUFDLEVBQUU7d0JBQ3JDLE1BQU0sUUFBUSxHQUFHLElBQUksQ0FBQyxVQUFVLENBQUMsQ0FBQzt3QkFFbEMsSUFBSSxDQUFDLElBQUksQ0FBQyxVQUFVLENBQUMsRUFBRTs0QkFDckIsSUFBSSxDQUFDLFVBQVUsQ0FBQyxHQUFHO2dDQUNqQixVQUFVLEVBQUUsb0JBQW9CLENBQUMsVUFBVSxFQUFFLFVBQVUsQ0FBQztnQ0FDeEQsVUFBVSxFQUFFLG9CQUFvQixDQUFDLFVBQVUsRUFBRSxVQUFVLENBQUM7Z0NBQ3hELElBQUksRUFBRSxFQUFFOzZCQUNULENBQUM7eUJBQ0g7d0JBRUQsSUFBSSxDQUFDLFVBQVUsQ0FBQyxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsUUFBUSxDQUFDLENBQUM7b0JBQ3ZDLENBQUMsQ0FBQyxDQUFDO2dCQUNMLENBQUMsQ0FBQyxDQUFDO2dCQUVILE9BQU8sSUFBSSxDQUFDO1lBQ2QsQ0FBQztTQUFBOztJQWpFRCx5RUFBeUU7SUFDbEUsNEJBQVksR0FBRyxZQUFZLENBQUM7SUFpRXJDLHNCQUFDO0tBQUE7U0FuRVksZUFBZTtBQXFFNUIsaUVBQWlFO0FBQ2pFLFNBQVMsb0JBQW9CLENBQUMsUUFBb0MsRUFBRSxNQUFjO0lBQ2hGLE1BQU0sV0FBVyxHQUFhLEVBQUUsQ0FBQztJQUVqQyxRQUFRLENBQUMsT0FBTyxDQUFDLElBQUksQ0FBQyxFQUFFO1FBQ3RCLE1BQU0sQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUMsT0FBTyxDQUFDLFVBQVUsQ0FBQyxFQUFFO1lBQ3JDLElBQUksVUFBVSxLQUFLLE1BQU0sRUFBRTtnQkFDekIsV0FBVyxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsVUFBVSxDQUFDLENBQUMsQ0FBQzthQUNwQztRQUNILENBQUMsQ0FBQyxDQUFDO0lBQ0wsQ0FBQyxDQUFDLENBQUM7SUFFSCxPQUFPLFdBQVcsQ0FBQztBQUNyQixDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Q29tcG9uZW50SGFybmVzcywgSGFybmVzc1ByZWRpY2F0ZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3Rlc3RpbmcnO1xuaW1wb3J0IHtUYWJsZUhhcm5lc3NGaWx0ZXJzLCBSb3dIYXJuZXNzRmlsdGVyc30gZnJvbSAnLi90YWJsZS1oYXJuZXNzLWZpbHRlcnMnO1xuaW1wb3J0IHtcbiAgTWF0Um93SGFybmVzcyxcbiAgTWF0SGVhZGVyUm93SGFybmVzcyxcbiAgTWF0Rm9vdGVyUm93SGFybmVzcyxcbiAgTWF0Um93SGFybmVzc0NvbHVtbnNUZXh0LFxufSBmcm9tICcuL3Jvdy1oYXJuZXNzJztcblxuLyoqIFRleHQgZXh0cmFjdGVkIGZyb20gYSB0YWJsZSBvcmdhbml6ZWQgYnkgY29sdW1ucy4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgTWF0VGFibGVIYXJuZXNzQ29sdW1uc1RleHQge1xuICBbY29sdW1uTmFtZTogc3RyaW5nXToge1xuICAgIHRleHQ6IHN0cmluZ1tdO1xuICAgIGhlYWRlclRleHQ6IHN0cmluZ1tdO1xuICAgIGZvb3RlclRleHQ6IHN0cmluZ1tdO1xuICB9O1xufVxuXG4vKiogSGFybmVzcyBmb3IgaW50ZXJhY3Rpbmcgd2l0aCBhIHN0YW5kYXJkIG1hdC10YWJsZSBpbiB0ZXN0cy4gKi9cbmV4cG9ydCBjbGFzcyBNYXRUYWJsZUhhcm5lc3MgZXh0ZW5kcyBDb21wb25lbnRIYXJuZXNzIHtcbiAgLyoqIFRoZSBzZWxlY3RvciBmb3IgdGhlIGhvc3QgZWxlbWVudCBvZiBhIGBNYXRUYWJsZUhhcm5lc3NgIGluc3RhbmNlLiAqL1xuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJy5tYXQtdGFibGUnO1xuXG4gIC8qKlxuICAgKiBHZXRzIGEgYEhhcm5lc3NQcmVkaWNhdGVgIHRoYXQgY2FuIGJlIHVzZWQgdG8gc2VhcmNoIGZvciBhIHRhYmxlIHdpdGggc3BlY2lmaWMgYXR0cmlidXRlcy5cbiAgICogQHBhcmFtIG9wdGlvbnMgT3B0aW9ucyBmb3IgbmFycm93aW5nIHRoZSBzZWFyY2hcbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBUYWJsZUhhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdFRhYmxlSGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXRUYWJsZUhhcm5lc3MsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqIEdldHMgYWxsIG9mIHRoZSBoZWFkZXIgcm93cyBpbiBhIHRhYmxlLiAqL1xuICBhc3luYyBnZXRIZWFkZXJSb3dzKGZpbHRlcjogUm93SGFybmVzc0ZpbHRlcnMgPSB7fSk6IFByb21pc2U8TWF0SGVhZGVyUm93SGFybmVzc1tdPiB7XG4gICAgcmV0dXJuIHRoaXMubG9jYXRvckZvckFsbChNYXRIZWFkZXJSb3dIYXJuZXNzLndpdGgoZmlsdGVyKSkoKTtcbiAgfVxuXG4gIC8qKiBHZXRzIGFsbCBvZiB0aGUgcmVndWxhciBkYXRhIHJvd3MgaW4gYSB0YWJsZS4gKi9cbiAgYXN5bmMgZ2V0Um93cyhmaWx0ZXI6IFJvd0hhcm5lc3NGaWx0ZXJzID0ge30pOiBQcm9taXNlPE1hdFJvd0hhcm5lc3NbXT4ge1xuICAgIHJldHVybiB0aGlzLmxvY2F0b3JGb3JBbGwoTWF0Um93SGFybmVzcy53aXRoKGZpbHRlcikpKCk7XG4gIH1cblxuICAvKiogR2V0cyBhbGwgb2YgdGhlIGZvb3RlciByb3dzIGluIGEgdGFibGUuICovXG4gIGFzeW5jIGdldEZvb3RlclJvd3MoZmlsdGVyOiBSb3dIYXJuZXNzRmlsdGVycyA9IHt9KTogUHJvbWlzZTxNYXRGb290ZXJSb3dIYXJuZXNzW10+IHtcbiAgICByZXR1cm4gdGhpcy5sb2NhdG9yRm9yQWxsKE1hdEZvb3RlclJvd0hhcm5lc3Mud2l0aChmaWx0ZXIpKSgpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHRleHQgaW5zaWRlIHRoZSBlbnRpcmUgdGFibGUgb3JnYW5pemVkIGJ5IHJvd3MuICovXG4gIGFzeW5jIGdldENlbGxUZXh0QnlJbmRleCgpOiBQcm9taXNlPHN0cmluZ1tdW10+IHtcbiAgICBjb25zdCByb3dzID0gYXdhaXQgdGhpcy5nZXRSb3dzKCk7XG4gICAgcmV0dXJuIFByb21pc2UuYWxsKHJvd3MubWFwKHJvdyA9PiByb3cuZ2V0Q2VsbFRleHRCeUluZGV4KCkpKTtcbiAgfVxuXG4gIC8qKiBHZXRzIHRoZSB0ZXh0IGluc2lkZSB0aGUgZW50aXJlIHRhYmxlIG9yZ2FuaXplZCBieSBjb2x1bW5zLiAqL1xuICBhc3luYyBnZXRDZWxsVGV4dEJ5Q29sdW1uTmFtZSgpOiBQcm9taXNlPE1hdFRhYmxlSGFybmVzc0NvbHVtbnNUZXh0PiB7XG4gICAgY29uc3QgW2hlYWRlclJvd3MsIGZvb3RlclJvd3MsIGRhdGFSb3dzXSA9IGF3YWl0IFByb21pc2UuYWxsKFtcbiAgICAgIHRoaXMuZ2V0SGVhZGVyUm93cygpLFxuICAgICAgdGhpcy5nZXRGb290ZXJSb3dzKCksXG4gICAgICB0aGlzLmdldFJvd3MoKVxuICAgIF0pO1xuXG4gICAgY29uc3QgdGV4dDogTWF0VGFibGVIYXJuZXNzQ29sdW1uc1RleHQgPSB7fTtcbiAgICBjb25zdCBbaGVhZGVyRGF0YSwgZm9vdGVyRGF0YSwgcm93c0RhdGFdID0gYXdhaXQgUHJvbWlzZS5hbGwoW1xuICAgICAgUHJvbWlzZS5hbGwoaGVhZGVyUm93cy5tYXAocm93ID0+IHJvdy5nZXRDZWxsVGV4dEJ5Q29sdW1uTmFtZSgpKSksXG4gICAgICBQcm9taXNlLmFsbChmb290ZXJSb3dzLm1hcChyb3cgPT4gcm93LmdldENlbGxUZXh0QnlDb2x1bW5OYW1lKCkpKSxcbiAgICAgIFByb21pc2UuYWxsKGRhdGFSb3dzLm1hcChyb3cgPT4gcm93LmdldENlbGxUZXh0QnlDb2x1bW5OYW1lKCkpKSxcbiAgICBdKTtcblxuICAgIHJvd3NEYXRhLmZvckVhY2goZGF0YSA9PiB7XG4gICAgICBPYmplY3Qua2V5cyhkYXRhKS5mb3JFYWNoKGNvbHVtbk5hbWUgPT4ge1xuICAgICAgICBjb25zdCBjZWxsVGV4dCA9IGRhdGFbY29sdW1uTmFtZV07XG5cbiAgICAgICAgaWYgKCF0ZXh0W2NvbHVtbk5hbWVdKSB7XG4gICAgICAgICAgdGV4dFtjb2x1bW5OYW1lXSA9IHtcbiAgICAgICAgICAgIGhlYWRlclRleHQ6IGdldENlbGxUZXh0c0J5Q29sdW1uKGhlYWRlckRhdGEsIGNvbHVtbk5hbWUpLFxuICAgICAgICAgICAgZm9vdGVyVGV4dDogZ2V0Q2VsbFRleHRzQnlDb2x1bW4oZm9vdGVyRGF0YSwgY29sdW1uTmFtZSksXG4gICAgICAgICAgICB0ZXh0OiBbXVxuICAgICAgICAgIH07XG4gICAgICAgIH1cblxuICAgICAgICB0ZXh0W2NvbHVtbk5hbWVdLnRleHQucHVzaChjZWxsVGV4dCk7XG4gICAgICB9KTtcbiAgICB9KTtcblxuICAgIHJldHVybiB0ZXh0O1xuICB9XG59XG5cbi8qKiBFeHRyYWN0cyB0aGUgdGV4dCBvZiBjZWxscyBvbmx5IHVuZGVyIGEgcGFydGljdWxhciBjb2x1bW4uICovXG5mdW5jdGlvbiBnZXRDZWxsVGV4dHNCeUNvbHVtbihyb3dzRGF0YTogTWF0Um93SGFybmVzc0NvbHVtbnNUZXh0W10sIGNvbHVtbjogc3RyaW5nKTogc3RyaW5nW10ge1xuICBjb25zdCBjb2x1bW5UZXh0czogc3RyaW5nW10gPSBbXTtcblxuICByb3dzRGF0YS5mb3JFYWNoKGRhdGEgPT4ge1xuICAgIE9iamVjdC5rZXlzKGRhdGEpLmZvckVhY2goY29sdW1uTmFtZSA9PiB7XG4gICAgICBpZiAoY29sdW1uTmFtZSA9PT0gY29sdW1uKSB7XG4gICAgICAgIGNvbHVtblRleHRzLnB1c2goZGF0YVtjb2x1bW5OYW1lXSk7XG4gICAgICB9XG4gICAgfSk7XG4gIH0pO1xuXG4gIHJldHVybiBjb2x1bW5UZXh0cztcbn1cbiJdfQ==