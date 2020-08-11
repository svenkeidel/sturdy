/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ElementRef, QueryList, AfterContentInit } from '@angular/core';
import { MatLine } from '@angular/material/core';
import { NumberInput } from '@angular/cdk/coercion';
import { MatGridListBase } from './grid-list-base';
import * as ɵngcc0 from '@angular/core';
export declare class MatGridTile {
    private _element;
    _gridList?: MatGridListBase | undefined;
    _rowspan: number;
    _colspan: number;
    constructor(_element: ElementRef<HTMLElement>, _gridList?: MatGridListBase | undefined);
    /** Amount of rows that the grid tile takes up. */
    get rowspan(): number;
    set rowspan(value: number);
    /** Amount of columns that the grid tile takes up. */
    get colspan(): number;
    set colspan(value: number);
    /**
     * Sets the style of the grid-tile element.  Needs to be set manually to avoid
     * "Changed after checked" errors that would occur with HostBinding.
     */
    _setStyle(property: string, value: any): void;
    static ngAcceptInputType_rowspan: NumberInput;
    static ngAcceptInputType_colspan: NumberInput;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatGridTile, [null, { optional: true; }]>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatGridTile, "mat-grid-tile", ["matGridTile"], { "rowspan": "rowspan"; "colspan": "colspan"; }, {}, never, ["*"]>;
}
export declare class MatGridTileText implements AfterContentInit {
    private _element;
    _lines: QueryList<MatLine>;
    constructor(_element: ElementRef<HTMLElement>);
    ngAfterContentInit(): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatGridTileText, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatGridTileText, "mat-grid-tile-header, mat-grid-tile-footer", never, {}, {}, ["_lines"], ["[mat-grid-avatar], [matGridAvatar]", "[mat-line], [matLine]", "*"]>;
}
/**
 * Directive whose purpose is to add the mat- CSS styling to this selector.
 * @docs-private
 */
export declare class MatGridAvatarCssMatStyler {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatGridAvatarCssMatStyler, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatGridAvatarCssMatStyler, "[mat-grid-avatar], [matGridAvatar]", never, {}, {}, never>;
}
/**
 * Directive whose purpose is to add the mat- CSS styling to this selector.
 * @docs-private
 */
export declare class MatGridTileHeaderCssMatStyler {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatGridTileHeaderCssMatStyler, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatGridTileHeaderCssMatStyler, "mat-grid-tile-header", never, {}, {}, never>;
}
/**
 * Directive whose purpose is to add the mat- CSS styling to this selector.
 * @docs-private
 */
export declare class MatGridTileFooterCssMatStyler {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatGridTileFooterCssMatStyler, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatGridTileFooterCssMatStyler, "mat-grid-tile-footer", never, {}, {}, never>;
}

//# sourceMappingURL=grid-tile.d.ts.map