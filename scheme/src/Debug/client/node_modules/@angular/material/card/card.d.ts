/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/**
 * Content of a card, needed as it's used as a selector in the API.
 * @docs-private
 */
import * as ɵngcc0 from '@angular/core';
export declare class MatCardContent {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardContent, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardContent, "mat-card-content, [mat-card-content], [matCardContent]", never, {}, {}, never>;
}
/**
 * Title of a card, needed as it's used as a selector in the API.
 * @docs-private
 */
export declare class MatCardTitle {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardTitle, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardTitle, "mat-card-title, [mat-card-title], [matCardTitle]", never, {}, {}, never>;
}
/**
 * Sub-title of a card, needed as it's used as a selector in the API.
 * @docs-private
 */
export declare class MatCardSubtitle {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardSubtitle, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardSubtitle, "mat-card-subtitle, [mat-card-subtitle], [matCardSubtitle]", never, {}, {}, never>;
}
/**
 * Action section of a card, needed as it's used as a selector in the API.
 * @docs-private
 */
export declare class MatCardActions {
    /** Position of the actions inside the card. */
    align: 'start' | 'end';
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardActions, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardActions, "mat-card-actions", ["matCardActions"], { "align": "align"; }, {}, never>;
}
/**
 * Footer of a card, needed as it's used as a selector in the API.
 * @docs-private
 */
export declare class MatCardFooter {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardFooter, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardFooter, "mat-card-footer", never, {}, {}, never>;
}
/**
 * Image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
export declare class MatCardImage {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardImage, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardImage, "[mat-card-image], [matCardImage]", never, {}, {}, never>;
}
/**
 * Image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
export declare class MatCardSmImage {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardSmImage, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardSmImage, "[mat-card-sm-image], [matCardImageSmall]", never, {}, {}, never>;
}
/**
 * Image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
export declare class MatCardMdImage {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardMdImage, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardMdImage, "[mat-card-md-image], [matCardImageMedium]", never, {}, {}, never>;
}
/**
 * Image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
export declare class MatCardLgImage {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardLgImage, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardLgImage, "[mat-card-lg-image], [matCardImageLarge]", never, {}, {}, never>;
}
/**
 * Large image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
export declare class MatCardXlImage {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardXlImage, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardXlImage, "[mat-card-xl-image], [matCardImageXLarge]", never, {}, {}, never>;
}
/**
 * Avatar image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
export declare class MatCardAvatar {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardAvatar, never>;
    static ɵdir: ɵngcc0.ɵɵDirectiveDefWithMeta<MatCardAvatar, "[mat-card-avatar], [matCardAvatar]", never, {}, {}, never>;
}
/**
 * A basic content container component that adds the styles of a Material design card.
 *
 * While this component can be used alone, it also provides a number
 * of preset styles for common card sections, including:
 * - mat-card-title
 * - mat-card-subtitle
 * - mat-card-content
 * - mat-card-actions
 * - mat-card-footer
 */
export declare class MatCard {
    _animationMode?: string | undefined;
    constructor(_animationMode?: string | undefined);
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCard, [{ optional: true; }]>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatCard, "mat-card", ["matCard"], {}, {}, never, ["*", "mat-card-footer"]>;
}
/**
 * Component intended to be used within the `<mat-card>` component. It adds styles for a
 * preset header section (i.e. a title, subtitle, and avatar layout).
 * @docs-private
 */
export declare class MatCardHeader {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardHeader, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatCardHeader, "mat-card-header", never, {}, {}, never, ["[mat-card-avatar], [matCardAvatar]", "mat-card-title, mat-card-subtitle,\n      [mat-card-title], [mat-card-subtitle],\n      [matCardTitle], [matCardSubtitle]", "*"]>;
}
/**
 * Component intended to be used within the `<mat-card>` component. It adds styles for a preset
 * layout that groups an image with a title section.
 * @docs-private
 */
export declare class MatCardTitleGroup {
    static ɵfac: ɵngcc0.ɵɵFactoryDef<MatCardTitleGroup, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<MatCardTitleGroup, "mat-card-title-group", never, {}, {}, never, ["mat-card-title, mat-card-subtitle,\n      [mat-card-title], [mat-card-subtitle],\n      [matCardTitle], [matCardSubtitle]", "img", "*"]>;
}

//# sourceMappingURL=card.d.ts.map