/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { BooleanInput } from '@angular/cdk/coercion';
import { CdkFooterRow, CdkFooterRowDef, CdkHeaderRow, CdkHeaderRowDef, CdkRow, CdkRowDef, CdkNoDataRow } from '@angular/cdk/table';
/**
 * Header row definition for the mat-table.
 * Captures the header row's template and other header properties such as the columns to display.
 */
import * as ɵngcc0 from '@angular/core';
export declare class MatHeaderRowDef extends CdkHeaderRowDef {
    static ngAcceptInputType_sticky: BooleanInput;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatHeaderRowDef, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatHeaderRowDef, "[matHeaderRowDef]", never, { "columns": "matHeaderRowDef"; "sticky": "matHeaderRowDefSticky"; }, {}, never>;
}
/**
 * Footer row definition for the mat-table.
 * Captures the footer row's template and other footer properties such as the columns to display.
 */
export declare class MatFooterRowDef extends CdkFooterRowDef {
    static ngAcceptInputType_sticky: BooleanInput;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatFooterRowDef, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatFooterRowDef, "[matFooterRowDef]", never, { "columns": "matFooterRowDef"; "sticky": "matFooterRowDefSticky"; }, {}, never>;
}
/**
 * Data row definition for the mat-table.
 * Captures the data row's template and other properties such as the columns to display and
 * a when predicate that describes when this row should be used.
 */
export declare class MatRowDef<T> extends CdkRowDef<T> {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatRowDef<any>, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatRowDef<any>, "[matRowDef]", never, { "columns": "matRowDefColumns"; "when": "matRowDefWhen"; }, {}, never>;
}
/** Header template container that contains the cell outlet. Adds the right class and role. */
export declare class MatHeaderRow extends CdkHeaderRow {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatHeaderRow, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatHeaderRow, "mat-header-row, tr[mat-header-row]", ["matHeaderRow"], {}, {}, never, never>;
}
/** Footer template container that contains the cell outlet. Adds the right class and role. */
export declare class MatFooterRow extends CdkFooterRow {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatFooterRow, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatFooterRow, "mat-footer-row, tr[mat-footer-row]", ["matFooterRow"], {}, {}, never, never>;
}
/** Data row template container that contains the cell outlet. Adds the right class and role. */
export declare class MatRow extends CdkRow {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatRow, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatRow, "mat-row, tr[mat-row]", ["matRow"], {}, {}, never, never>;
}
/** Row that can be used to display a message when no data is shown in the table. */
export declare class MatNoDataRow extends CdkNoDataRow {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatNoDataRow, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatNoDataRow, "ng-template[matNoDataRow]", never, {}, {}, never>;
}

//# sourceMappingURL=row.d.ts.map