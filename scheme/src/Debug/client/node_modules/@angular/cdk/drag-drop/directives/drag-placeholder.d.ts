/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { TemplateRef } from '@angular/core';
/**
 * Element that will be used as a template for the placeholder of a CdkDrag when
 * it is being dragged. The placeholder is displayed in place of the element being dragged.
 */
import * as ɵngcc0 from '@angular/core';
export declare class CdkDragPlaceholder<T = any> {
    templateRef: TemplateRef<T>;
    /** Context data to be added to the placeholder template instance. */
    data: T;
    constructor(templateRef: TemplateRef<T>);
    static ɵfac: ɵngcc0.ɵɵFactoryDef<CdkDragPlaceholder<any>, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<CdkDragPlaceholder<any>, "ng-template[cdkDragPlaceholder]", never, { "data": "data"; }, {}, never>;
}

//# sourceMappingURL=drag-placeholder.d.ts.map