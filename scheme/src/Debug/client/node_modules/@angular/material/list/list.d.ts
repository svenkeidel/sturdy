/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { BooleanInput } from '@angular/cdk/coercion';
import { AfterContentInit, ElementRef, QueryList, OnChanges, OnDestroy, ChangeDetectorRef, InjectionToken } from '@angular/core';
import { CanDisable, CanDisableCtor, CanDisableRipple, CanDisableRippleCtor, MatLine } from '@angular/material/core';
import { Subject } from 'rxjs';
/** @docs-private */
import * as ɵngcc0 from '@angular/core';
declare class MatListBase {
}
declare const _MatListMixinBase: CanDisableRippleCtor & CanDisableCtor & typeof MatListBase;
/** @docs-private */
declare class MatListItemBase {
}
declare const _MatListItemMixinBase: CanDisableRippleCtor & typeof MatListItemBase;
/**
 * Injection token that can be used to inject instances of `MatList`. It serves as
 * alternative token to the actual `MatList` class which could cause unnecessary
 * retention of the class and its component metadata.
 */
export declare const MAT_LIST: InjectionToken<MatList>;
/**
 * Injection token that can be used to inject instances of `MatNavList`. It serves as
 * alternative token to the actual `MatNavList` class which could cause unnecessary
 * retention of the class and its component metadata.
 */
export declare const MAT_NAV_LIST: InjectionToken<MatNavList>;
export declare class MatNavList extends _MatListMixinBase implements CanDisable, CanDisableRipple, OnChanges, OnDestroy {
    /** Emits when the state of the list changes. */
    _stateChanges: Subject<void>;
    ngOnChanges(): void;
    ngOnDestroy(): void;
    static ngAcceptInputType_disableRipple: BooleanInput;
    static ngAcceptInputType_disabled: BooleanInput;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatNavList, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatNavList, "mat-nav-list", ["matNavList"], { "disableRipple": "disableRipple"; "disabled": "disabled"; }, {}, never, ["*"]>;
}
export declare class MatList extends _MatListMixinBase implements CanDisable, CanDisableRipple, OnChanges, OnDestroy {
    private _elementRef;
    /** Emits when the state of the list changes. */
    _stateChanges: Subject<void>;
    constructor(_elementRef: ElementRef<HTMLElement>);
    _getListType(): 'list' | 'action-list' | null;
    ngOnChanges(): void;
    ngOnDestroy(): void;
    static ngAcceptInputType_disableRipple: BooleanInput;
    static ngAcceptInputType_disabled: BooleanInput;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatList, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatList, "mat-list, mat-action-list", ["matList"], { "disableRipple": "disableRipple"; "disabled": "disabled"; }, {}, never, ["*"]>;
}
/**
 * Directive whose purpose is to add the mat- CSS styling to this selector.
 * @docs-private
 */
export declare class MatListAvatarCssMatStyler {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatListAvatarCssMatStyler, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatListAvatarCssMatStyler, "[mat-list-avatar], [matListAvatar]", never, {}, {}, never>;
}
/**
 * Directive whose purpose is to add the mat- CSS styling to this selector.
 * @docs-private
 */
export declare class MatListIconCssMatStyler {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatListIconCssMatStyler, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatListIconCssMatStyler, "[mat-list-icon], [matListIcon]", never, {}, {}, never>;
}
/**
 * Directive whose purpose is to add the mat- CSS styling to this selector.
 * @docs-private
 */
export declare class MatListSubheaderCssMatStyler {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatListSubheaderCssMatStyler, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatListSubheaderCssMatStyler, "[mat-subheader], [matSubheader]", never, {}, {}, never>;
}
/** An item within a Material Design list. */
export declare class MatListItem extends _MatListItemMixinBase implements AfterContentInit, CanDisableRipple, OnDestroy {
    private _element;
    private _isInteractiveList;
    private _list?;
    private _destroyed;
    _lines: QueryList<MatLine>;
    _avatar: MatListAvatarCssMatStyler;
    _icon: MatListIconCssMatStyler;
    constructor(_element: ElementRef<HTMLElement>, _changeDetectorRef: ChangeDetectorRef, navList?: MatNavList, list?: MatList);
    /** Whether the option is disabled. */
    get disabled(): boolean;
    set disabled(value: boolean);
    private _disabled;
    ngAfterContentInit(): void;
    ngOnDestroy(): void;
    /** Whether this list item should show a ripple effect when clicked. */
    _isRippleDisabled(): boolean;
    /** Retrieves the DOM element of the component host. */
    _getHostElement(): HTMLElement;
    static ngAcceptInputType_disableRipple: BooleanInput;
    static ngAcceptInputType_disabled: BooleanInput;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatListItem, [null, null, { optional: true; }, { optional: true; }]>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatListItem, "mat-list-item, a[mat-list-item], button[mat-list-item]", ["matListItem"], { "disableRipple": "disableRipple"; "disabled": "disabled"; }, {}, ["_avatar", "_icon", "_lines"], ["[mat-list-avatar], [mat-list-icon], [matListAvatar], [matListIcon]", "[mat-line], [matLine]", "*"]>;
}
export {};

//# sourceMappingURL=list.d.ts.map