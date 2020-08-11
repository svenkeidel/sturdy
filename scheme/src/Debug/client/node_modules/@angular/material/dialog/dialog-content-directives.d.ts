/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { OnChanges, OnInit, SimpleChanges, ElementRef } from '@angular/core';
import { MatDialog } from './dialog';
import { MatDialogRef } from './dialog-ref';
/**
 * Button that will close the current dialog.
 */
import * as ɵngcc0 from '@angular/core';
export declare class MatDialogClose implements OnInit, OnChanges {
    dialogRef: MatDialogRef<any>;
    private _elementRef;
    private _dialog;
    /** Screenreader label for the button. */
    ariaLabel: string;
    /** Default to "button" to prevents accidental form submits. */
    type: 'submit' | 'button' | 'reset';
    /** Dialog close input. */
    dialogResult: any;
    _matDialogClose: any;
    constructor(dialogRef: MatDialogRef<any>, _elementRef: ElementRef<HTMLElement>, _dialog: MatDialog);
    ngOnInit(): void;
    ngOnChanges(changes: SimpleChanges): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatDialogClose, [{ optional: true; }, null, null]>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatDialogClose, "[mat-dialog-close], [matDialogClose]", ["matDialogClose"], { "type": "type"; "dialogResult": "mat-dialog-close"; "ariaLabel": "aria-label"; "_matDialogClose": "matDialogClose"; }, {}, never>;
}
/**
 * Title of a dialog element. Stays fixed to the top of the dialog when scrolling.
 */
export declare class MatDialogTitle implements OnInit {
    private _dialogRef;
    private _elementRef;
    private _dialog;
    id: string;
    constructor(_dialogRef: MatDialogRef<any>, _elementRef: ElementRef<HTMLElement>, _dialog: MatDialog);
    ngOnInit(): void;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatDialogTitle, [{ optional: true; }, null, null]>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatDialogTitle, "[mat-dialog-title], [matDialogTitle]", ["matDialogTitle"], { "id": "id"; }, {}, never>;
}
/**
 * Scrollable content container of a dialog.
 */
export declare class MatDialogContent {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatDialogContent, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatDialogContent, "[mat-dialog-content], mat-dialog-content, [matDialogContent]", never, {}, {}, never>;
}
/**
 * Container for the bottom action buttons in a dialog.
 * Stays fixed to the bottom when scrolling.
 */
export declare class MatDialogActions {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatDialogActions, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatDialogActions, "[mat-dialog-actions], mat-dialog-actions, [matDialogActions]", never, {}, {}, never>;
}

//# sourceMappingURL=dialog-content-directives.d.ts.map