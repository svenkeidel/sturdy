/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { BooleanInput } from '@angular/cdk/coercion';
import { CanDisable, CanDisableCtor } from '../common-behaviors/disabled';
/** @docs-private */
import * as ɵngcc0 from '@angular/core';
declare class MatOptgroupBase {
}
declare const _MatOptgroupMixinBase: CanDisableCtor & typeof MatOptgroupBase;
/**
 * Component that is used to group instances of `mat-option`.
 */
export declare class MatOptgroup extends _MatOptgroupMixinBase implements CanDisable {
    /** Label for the option group. */
    label: string;
    /** Unique id for the underlying label. */
    _labelId: string;
    static ngAcceptInputType_disabled: BooleanInput;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatOptgroup, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatOptgroup, "mat-optgroup", ["matOptgroup"], { "disabled": "disabled"; "label": "label"; }, {}, never, ["*", "mat-option, ng-container"]>;
}
export {};

//# sourceMappingURL=optgroup.d.ts.map