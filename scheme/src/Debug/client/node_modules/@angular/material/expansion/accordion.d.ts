/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { QueryList, AfterContentInit } from '@angular/core';
import { BooleanInput } from '@angular/cdk/coercion';
import { CdkAccordion } from '@angular/cdk/accordion';
import { MatAccordionBase, MatAccordionDisplayMode, MatAccordionTogglePosition } from './accordion-base';
import { MatExpansionPanelHeader } from './expansion-panel-header';
/**
 * Directive for a Material Design Accordion.
 */
import * as ɵngcc0 from '@angular/core';
export declare class MatAccordion extends CdkAccordion implements MatAccordionBase, AfterContentInit {
    private _keyManager;
    /** Headers belonging to this accordion. */
    private _ownHeaders;
    /** All headers inside the accordion. Includes headers inside nested accordions. */
    _headers: QueryList<MatExpansionPanelHeader>;
    /** Whether the expansion indicator should be hidden. */
    get hideToggle(): boolean;
    set hideToggle(show: boolean);
    private _hideToggle;
    /**
     * Display mode used for all expansion panels in the accordion. Currently two display
     * modes exist:
     *  default - a gutter-like spacing is placed around any expanded panel, placing the expanded
     *     panel at a different elevation from the rest of the accordion.
     *  flat - no spacing is placed around expanded panels, showing all panels at the same
     *     elevation.
     */
    displayMode: MatAccordionDisplayMode;
    /** The position of the expansion indicator. */
    togglePosition: MatAccordionTogglePosition;
    ngAfterContentInit(): void;
    /** Handles keyboard events coming in from the panel headers. */
    _handleHeaderKeydown(event: KeyboardEvent): void;
    _handleHeaderFocus(header: MatExpansionPanelHeader): void;
    static ngAcceptInputType_hideToggle: BooleanInput;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatAccordion, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatAccordion, "mat-accordion", ["matAccordion"], { "multi": "multi"; "displayMode": "displayMode"; "togglePosition": "togglePosition"; "hideToggle": "hideToggle"; }, {}, ["_headers"]>;
}

//# sourceMappingURL=accordion.d.ts.map