/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { CdkNestedTreeNode, CdkTree, CdkTreeNode, CdkTreeNodeDef } from '@angular/cdk/tree';
import { AfterContentInit, ElementRef, IterableDiffers, OnDestroy } from '@angular/core';
import { CanDisable, CanDisableCtor, HasTabIndex, HasTabIndexCtor } from '@angular/material/core';
import { BooleanInput } from '@angular/cdk/coercion';
import * as ɵngcc0 from '@angular/core';
declare const _MatTreeNodeMixinBase: HasTabIndexCtor & CanDisableCtor & typeof CdkTreeNode;
/**
 * Wrapper for the CdkTree node with Material design styles.
 */
export declare class MatTreeNode<T> extends _MatTreeNodeMixinBase<T> implements CanDisable, HasTabIndex {
    protected _elementRef: ElementRef<HTMLElement>;
    protected _tree: CdkTree<T>;
    role: 'treeitem' | 'group';
    constructor(_elementRef: ElementRef<HTMLElement>, _tree: CdkTree<T>, tabIndex: string);
    static ngAcceptInputType_disabled: BooleanInput;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatTreeNode<any>, [null, null, { attribute: "tabindex"; }]>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatTreeNode<any>, "mat-tree-node", ["matTreeNode"], { "disabled": "disabled"; "tabIndex": "tabIndex"; "role": "role"; }, {}, never>;
}
/**
 * Wrapper for the CdkTree node definition with Material design styles.
 */
export declare class MatTreeNodeDef<T> extends CdkTreeNodeDef<T> {
    data: T;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatTreeNodeDef<any>, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatTreeNodeDef<any>, "[matTreeNodeDef]", never, { "when": "matTreeNodeDefWhen"; "data": "matTreeNode"; }, {}, never>;
}
/**
 * Wrapper for the CdkTree nested node with Material design styles.
 */
export declare class MatNestedTreeNode<T> extends CdkNestedTreeNode<T> implements AfterContentInit, OnDestroy {
    protected _elementRef: ElementRef<HTMLElement>;
    protected _tree: CdkTree<T>;
    protected _differs: IterableDiffers;
    node: T;
    /** Whether the node is disabled. */
    get disabled(): any;
    set disabled(value: any);
    private _disabled;
    /** Tabindex for the node. */
    get tabIndex(): number;
    set tabIndex(value: number);
    private _tabIndex;
    constructor(_elementRef: ElementRef<HTMLElement>, _tree: CdkTree<T>, _differs: IterableDiffers, tabIndex: string);
    ngAfterContentInit(): void;
    ngOnDestroy(): void;
    static ngAcceptInputType_disabled: BooleanInput;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatNestedTreeNode<any>, [null, null, null, { attribute: "tabindex"; }]>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatNestedTreeNode<any>, "mat-nested-tree-node", ["matNestedTreeNode"], { "tabIndex": "tabIndex"; "disabled": "disabled"; "node": "matNestedTreeNode"; }, {}, never>;
}
export {};

//# sourceMappingURL=node.d.ts.map