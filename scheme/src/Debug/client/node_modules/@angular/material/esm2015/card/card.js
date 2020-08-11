/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Component, ViewEncapsulation, ChangeDetectionStrategy, Directive, Input, Optional, Inject, } from '@angular/core';
import { ANIMATION_MODULE_TYPE } from '@angular/platform-browser/animations';
/**
 * Content of a card, needed as it's used as a selector in the API.
 * @docs-private
 */
let MatCardContent = /** @class */ (() => {
    class MatCardContent {
    }
    MatCardContent.decorators = [
        { type: Directive, args: [{
                    selector: 'mat-card-content, [mat-card-content], [matCardContent]',
                    host: { 'class': 'mat-card-content' }
                },] }
    ];
    return MatCardContent;
})();
export { MatCardContent };
/**
 * Title of a card, needed as it's used as a selector in the API.
 * @docs-private
 */
let MatCardTitle = /** @class */ (() => {
    class MatCardTitle {
    }
    MatCardTitle.decorators = [
        { type: Directive, args: [{
                    selector: `mat-card-title, [mat-card-title], [matCardTitle]`,
                    host: {
                        'class': 'mat-card-title'
                    }
                },] }
    ];
    return MatCardTitle;
})();
export { MatCardTitle };
/**
 * Sub-title of a card, needed as it's used as a selector in the API.
 * @docs-private
 */
let MatCardSubtitle = /** @class */ (() => {
    class MatCardSubtitle {
    }
    MatCardSubtitle.decorators = [
        { type: Directive, args: [{
                    selector: `mat-card-subtitle, [mat-card-subtitle], [matCardSubtitle]`,
                    host: {
                        'class': 'mat-card-subtitle'
                    }
                },] }
    ];
    return MatCardSubtitle;
})();
export { MatCardSubtitle };
/**
 * Action section of a card, needed as it's used as a selector in the API.
 * @docs-private
 */
let MatCardActions = /** @class */ (() => {
    class MatCardActions {
        constructor() {
            /** Position of the actions inside the card. */
            this.align = 'start';
        }
    }
    MatCardActions.decorators = [
        { type: Directive, args: [{
                    selector: 'mat-card-actions',
                    exportAs: 'matCardActions',
                    host: {
                        'class': 'mat-card-actions',
                        '[class.mat-card-actions-align-end]': 'align === "end"',
                    }
                },] }
    ];
    MatCardActions.propDecorators = {
        align: [{ type: Input }]
    };
    return MatCardActions;
})();
export { MatCardActions };
/**
 * Footer of a card, needed as it's used as a selector in the API.
 * @docs-private
 */
let MatCardFooter = /** @class */ (() => {
    class MatCardFooter {
    }
    MatCardFooter.decorators = [
        { type: Directive, args: [{
                    selector: 'mat-card-footer',
                    host: { 'class': 'mat-card-footer' }
                },] }
    ];
    return MatCardFooter;
})();
export { MatCardFooter };
/**
 * Image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
let MatCardImage = /** @class */ (() => {
    class MatCardImage {
    }
    MatCardImage.decorators = [
        { type: Directive, args: [{
                    selector: '[mat-card-image], [matCardImage]',
                    host: { 'class': 'mat-card-image' }
                },] }
    ];
    return MatCardImage;
})();
export { MatCardImage };
/**
 * Image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
let MatCardSmImage = /** @class */ (() => {
    class MatCardSmImage {
    }
    MatCardSmImage.decorators = [
        { type: Directive, args: [{
                    selector: '[mat-card-sm-image], [matCardImageSmall]',
                    host: { 'class': 'mat-card-sm-image' }
                },] }
    ];
    return MatCardSmImage;
})();
export { MatCardSmImage };
/**
 * Image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
let MatCardMdImage = /** @class */ (() => {
    class MatCardMdImage {
    }
    MatCardMdImage.decorators = [
        { type: Directive, args: [{
                    selector: '[mat-card-md-image], [matCardImageMedium]',
                    host: { 'class': 'mat-card-md-image' }
                },] }
    ];
    return MatCardMdImage;
})();
export { MatCardMdImage };
/**
 * Image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
let MatCardLgImage = /** @class */ (() => {
    class MatCardLgImage {
    }
    MatCardLgImage.decorators = [
        { type: Directive, args: [{
                    selector: '[mat-card-lg-image], [matCardImageLarge]',
                    host: { 'class': 'mat-card-lg-image' }
                },] }
    ];
    return MatCardLgImage;
})();
export { MatCardLgImage };
/**
 * Large image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
let MatCardXlImage = /** @class */ (() => {
    class MatCardXlImage {
    }
    MatCardXlImage.decorators = [
        { type: Directive, args: [{
                    selector: '[mat-card-xl-image], [matCardImageXLarge]',
                    host: { 'class': 'mat-card-xl-image' }
                },] }
    ];
    return MatCardXlImage;
})();
export { MatCardXlImage };
/**
 * Avatar image used in a card, needed to add the mat- CSS styling.
 * @docs-private
 */
let MatCardAvatar = /** @class */ (() => {
    class MatCardAvatar {
    }
    MatCardAvatar.decorators = [
        { type: Directive, args: [{
                    selector: '[mat-card-avatar], [matCardAvatar]',
                    host: { 'class': 'mat-card-avatar' }
                },] }
    ];
    return MatCardAvatar;
})();
export { MatCardAvatar };
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
let MatCard = /** @class */ (() => {
    class MatCard {
        // @breaking-change 9.0.0 `_animationMode` parameter to be made required.
        constructor(_animationMode) {
            this._animationMode = _animationMode;
        }
    }
    MatCard.decorators = [
        { type: Component, args: [{
                    selector: 'mat-card',
                    exportAs: 'matCard',
                    template: "<ng-content></ng-content>\n<ng-content select=\"mat-card-footer\"></ng-content>\n",
                    encapsulation: ViewEncapsulation.None,
                    changeDetection: ChangeDetectionStrategy.OnPush,
                    host: {
                        'class': 'mat-card mat-focus-indicator',
                        '[class._mat-animation-noopable]': '_animationMode === "NoopAnimations"',
                    },
                    styles: [".mat-card{transition:box-shadow 280ms cubic-bezier(0.4, 0, 0.2, 1);display:block;position:relative;padding:16px;border-radius:4px}._mat-animation-noopable.mat-card{transition:none;animation:none}.mat-card .mat-divider-horizontal{position:absolute;left:0;width:100%}[dir=rtl] .mat-card .mat-divider-horizontal{left:auto;right:0}.mat-card .mat-divider-horizontal.mat-divider-inset{position:static;margin:0}[dir=rtl] .mat-card .mat-divider-horizontal.mat-divider-inset{margin-right:0}.cdk-high-contrast-active .mat-card{outline:solid 1px}.mat-card-actions,.mat-card-subtitle,.mat-card-content{display:block;margin-bottom:16px}.mat-card-title{display:block;margin-bottom:8px}.mat-card-actions{margin-left:-8px;margin-right:-8px;padding:8px 0}.mat-card-actions-align-end{display:flex;justify-content:flex-end}.mat-card-image{width:calc(100% + 32px);margin:0 -16px 16px -16px}.mat-card-footer{display:block;margin:0 -16px -16px -16px}.mat-card-actions .mat-button,.mat-card-actions .mat-raised-button,.mat-card-actions .mat-stroked-button{margin:0 8px}.mat-card-header{display:flex;flex-direction:row}.mat-card-header .mat-card-title{margin-bottom:12px}.mat-card-header-text{margin:0 16px}.mat-card-avatar{height:40px;width:40px;border-radius:50%;flex-shrink:0;object-fit:cover}.mat-card-title-group{display:flex;justify-content:space-between}.mat-card-sm-image{width:80px;height:80px}.mat-card-md-image{width:112px;height:112px}.mat-card-lg-image{width:152px;height:152px}.mat-card-xl-image{width:240px;height:240px;margin:-8px}.mat-card-title-group>.mat-card-xl-image{margin:-8px 0 8px}@media(max-width: 599px){.mat-card-title-group{margin:0}.mat-card-xl-image{margin-left:0;margin-right:0}}.mat-card>:first-child,.mat-card-content>:first-child{margin-top:0}.mat-card>:last-child:not(.mat-card-footer),.mat-card-content>:last-child:not(.mat-card-footer){margin-bottom:0}.mat-card-image:first-child{margin-top:-16px;border-top-left-radius:inherit;border-top-right-radius:inherit}.mat-card>.mat-card-actions:last-child{margin-bottom:-8px;padding-bottom:0}.mat-card-actions .mat-button:first-child,.mat-card-actions .mat-raised-button:first-child,.mat-card-actions .mat-stroked-button:first-child{margin-left:0;margin-right:0}.mat-card-title:not(:first-child),.mat-card-subtitle:not(:first-child){margin-top:-4px}.mat-card-header .mat-card-subtitle:not(:first-child){margin-top:-8px}.mat-card>.mat-card-xl-image:first-child{margin-top:-8px}.mat-card>.mat-card-xl-image:last-child{margin-bottom:-8px}\n"]
                },] }
    ];
    MatCard.ctorParameters = () => [
        { type: String, decorators: [{ type: Optional }, { type: Inject, args: [ANIMATION_MODULE_TYPE,] }] }
    ];
    return MatCard;
})();
export { MatCard };
/**
 * Component intended to be used within the `<mat-card>` component. It adds styles for a
 * preset header section (i.e. a title, subtitle, and avatar layout).
 * @docs-private
 */
let MatCardHeader = /** @class */ (() => {
    class MatCardHeader {
    }
    MatCardHeader.decorators = [
        { type: Component, args: [{
                    selector: 'mat-card-header',
                    template: "<ng-content select=\"[mat-card-avatar], [matCardAvatar]\"></ng-content>\n<div class=\"mat-card-header-text\">\n  <ng-content\n      select=\"mat-card-title, mat-card-subtitle,\n      [mat-card-title], [mat-card-subtitle],\n      [matCardTitle], [matCardSubtitle]\"></ng-content>\n</div>\n<ng-content></ng-content>\n",
                    encapsulation: ViewEncapsulation.None,
                    changeDetection: ChangeDetectionStrategy.OnPush,
                    host: { 'class': 'mat-card-header' }
                },] }
    ];
    return MatCardHeader;
})();
export { MatCardHeader };
/**
 * Component intended to be used within the `<mat-card>` component. It adds styles for a preset
 * layout that groups an image with a title section.
 * @docs-private
 */
let MatCardTitleGroup = /** @class */ (() => {
    class MatCardTitleGroup {
    }
    MatCardTitleGroup.decorators = [
        { type: Component, args: [{
                    selector: 'mat-card-title-group',
                    template: "<div>\n  <ng-content\n      select=\"mat-card-title, mat-card-subtitle,\n      [mat-card-title], [mat-card-subtitle],\n      [matCardTitle], [matCardSubtitle]\"></ng-content>\n</div>\n<ng-content select=\"img\"></ng-content>\n<ng-content></ng-content>\n",
                    encapsulation: ViewEncapsulation.None,
                    changeDetection: ChangeDetectionStrategy.OnPush,
                    host: { 'class': 'mat-card-title-group' }
                },] }
    ];
    return MatCardTitleGroup;
})();
export { MatCardTitleGroup };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2FyZC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9jYXJkL2NhcmQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUNMLFNBQVMsRUFDVCxpQkFBaUIsRUFDakIsdUJBQXVCLEVBQ3ZCLFNBQVMsRUFDVCxLQUFLLEVBQ0wsUUFBUSxFQUNSLE1BQU0sR0FDUCxNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUMscUJBQXFCLEVBQUMsTUFBTSxzQ0FBc0MsQ0FBQztBQUczRTs7O0dBR0c7QUFDSDtJQUFBLE1BSWEsY0FBYzs7O2dCQUoxQixTQUFTLFNBQUM7b0JBQ1QsUUFBUSxFQUFFLHdEQUF3RDtvQkFDbEUsSUFBSSxFQUFFLEVBQUMsT0FBTyxFQUFFLGtCQUFrQixFQUFDO2lCQUNwQzs7SUFDNEIscUJBQUM7S0FBQTtTQUFqQixjQUFjO0FBRTNCOzs7R0FHRztBQUNIO0lBQUEsTUFNYSxZQUFZOzs7Z0JBTnhCLFNBQVMsU0FBQztvQkFDVCxRQUFRLEVBQUUsa0RBQWtEO29CQUM1RCxJQUFJLEVBQUU7d0JBQ0osT0FBTyxFQUFFLGdCQUFnQjtxQkFDMUI7aUJBQ0Y7O0lBQzBCLG1CQUFDO0tBQUE7U0FBZixZQUFZO0FBRXpCOzs7R0FHRztBQUNIO0lBQUEsTUFNYSxlQUFlOzs7Z0JBTjNCLFNBQVMsU0FBQztvQkFDVCxRQUFRLEVBQUUsMkRBQTJEO29CQUNyRSxJQUFJLEVBQUU7d0JBQ0osT0FBTyxFQUFFLG1CQUFtQjtxQkFDN0I7aUJBQ0Y7O0lBQzZCLHNCQUFDO0tBQUE7U0FBbEIsZUFBZTtBQUU1Qjs7O0dBR0c7QUFDSDtJQUFBLE1BUWEsY0FBYztRQVIzQjtZQVNFLCtDQUErQztZQUN0QyxVQUFLLEdBQW9CLE9BQU8sQ0FBQztRQUM1QyxDQUFDOzs7Z0JBWEEsU0FBUyxTQUFDO29CQUNULFFBQVEsRUFBRSxrQkFBa0I7b0JBQzVCLFFB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RSxpQkFBaUIsRUFBQztpQkFDbkM7O0lBQzJCLG9CQUFDO0tBQUE7U0FBaEIsYUFBYTtBQUcxQjs7OztHQUlHO0FBQ0g7SUFBQSxNQU9hLGlCQUFpQjs7O2dCQVA3QixTQUFTLFNBQUM7b0JBQ1QsUUFBUSxFQUFFLHNCQUFzQjtvQkFDaEMseVFBQW9DO29CQUNwQyxhQUFhLEVBQUUsaUJBQWlCLENBQUMsSUFBSTtvQkFDckMsZUFBZSxFQUFFLHVCQUF1QixDQUFDLE1BQU07b0JBQy9DLElBQUksRUFBRSxFQUFDLE9BQU8sRUFBRSxzQkFBc0IsRUFBQztpQkFDeEM7O0lBQytCLHdCQUFDO0tBQUE7U0FBcEIsaUJBQWlCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7XG4gIENvbXBvbmVudCxcbiAgVmlld0VuY2Fwc3VsYXRpb24sXG4gIENoYW5nZURldGVjdGlvblN0cmF0ZWd5LFxuICBEaXJlY3RpdmUsXG4gIElucHV0LFxuICBPcHRpb25hbCxcbiAgSW5qZWN0LFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7QU5JTUFUSU9OX01PRFVMRV9UWVBFfSBmcm9tICdAYW5ndWxhci9wbGF0Zm9ybS1icm93c2VyL2FuaW1hdGlvbnMnO1xuXG5cbi8qKlxuICogQ29udGVudCBvZiBhIGNhcmQsIG5lZWRlZCBhcyBpdCdzIHVzZWQgYXMgYSBzZWxlY3RvciBpbiB0aGUgQVBJLlxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICdtYXQtY2FyZC1jb250ZW50LCBbbWF0LWNhcmQtY29udGVudF0sIFttYXRDYXJkQ29udGVudF0nLFxuICBob3N0OiB7J2NsYXNzJzogJ21hdC1jYXJkLWNvbnRlbnQnfVxufSlcbmV4cG9ydCBjbGFzcyBNYXRDYXJkQ29udGVudCB7fVxuXG4vKipcbiAqIFRpdGxlIG9mIGEgY2FyZCwgbmVlZGVkIGFzIGl0J3MgdXNlZCBhcyBhIHNlbGVjdG9yIGluIHRoZSBBUEkuXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogYG1hdC1jYXJkLXRpdGxlLCBbbWF0LWNhcmQtdGl0bGVdLCBbbWF0Q2FyZFRpdGxlXWAsXG4gIGhvc3Q6IHtcbiAgICAnY2xhc3MnOiAnbWF0LWNhcmQtdGl0bGUnXG4gIH1cbn0pXG5leHBvcnQgY2xhc3MgTWF0Q2FyZFRpdGxlIHt9XG5cbi8qKlxuICogU3ViLXRpdGxlIG9mIGEgY2FyZCwgbmVlZGVkIGFzIGl0J3MgdXNlZCBhcyBhIHNlbGVjdG9yIGluIHRoZSBBUEkuXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogYG1hdC1jYXJkLXN1YnRpdGxlLCBbbWF0LWNhcmQtc3VidGl0bGVdLCBbbWF0Q2FyZFN1YnRpdGxlXWAsXG4gIGhvc3Q6IHtcbiAgICAnY2xhc3MnOiAnbWF0LWNhcmQtc3VidGl0bGUnXG4gIH1cbn0pXG5leHBvcnQgY2xhc3MgTWF0Q2FyZFN1YnRpdGxlIHt9XG5cbi8qKlxuICogQWN0aW9uIHNlY3Rpb24gb2YgYSBjYXJkLCBuZWVkZWQgYXMgaXQncyB1c2VkIGFzIGEgc2VsZWN0b3IgaW4gdGhlIEFQSS5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnbWF0LWNhcmQtYWN0aW9ucycsXG4gIGV4cG9ydEFzOiAnbWF0Q2FyZEFjdGlvbnMnLFxuICBob3N0OiB7XG4gICAgJ2NsYXNzJzogJ21hdC1jYXJkLWFjdGlvbnMnLFxuICAgICdbY2xhc3MubWF0LWNhcmQtYWN0aW9ucy1hbGlnbi1lbmRdJzogJ2FsaWduID09PSBcImVuZFwiJyxcbiAgfVxufSlcbmV4cG9ydCBjbGFzcyBNYXRDYXJkQWN0aW9ucyB7XG4gIC8qKiBQb3NpdGlvbiBvZiB0aGUgYWN0aW9ucyBpbnNpZGUgdGhlIGNhcmQuICovXG4gIEBJbnB1dCgpIGFsaWduOiAnc3RhcnQnIHwgJ2VuZCcgPSAnc3RhcnQnO1xufVxuXG4vKipcbiAqIEZvb3RlciBvZiBhIGNhcmQsIG5lZWRlZCBhcyBpdCdzIHVzZWQgYXMgYSBzZWxlY3RvciBpbiB0aGUgQVBJLlxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICdtYXQtY2FyZC1mb290ZXInLFxuICBob3N0OiB7J2NsYXNzJzogJ21hdC1jYXJkLWZvb3Rlcid9XG59KVxuZXhwb3J0IGNsYXNzIE1hdENhcmRGb290ZXIge31cblxuLyoqXG4gKiBJbWFnZSB1c2VkIGluIGEgY2FyZCwgbmVlZGVkIHRvIGFkZCB0aGUgbWF0LSBDU1Mgc3R5bGluZy5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnW21hdC1jYXJkLWltYWdlXSwgW21hdENhcmRJbWFnZV0nLFxuICBob3N0OiB7J2NsYXNzJzogJ21hdC1jYXJkLWltYWdlJ31cbn0pXG5leHBvcnQgY2xhc3MgTWF0Q2FyZEltYWdlIHt9XG5cbi8qKlxuICogSW1hZ2UgdXNlZCBpbiBhIGNhcmQsIG5lZWRlZCB0byBhZGQgdGhlIG1hdC0gQ1NTIHN0eWxpbmcuXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogJ1ttYXQtY2FyZC1zbS1pbWFnZV0sIFttYXRDYXJkSW1hZ2VTbWFsbF0nLFxuICBob3N0OiB7J2NsYXNzJzogJ21hdC1jYXJkLXNtLWltYWdlJ31cbn0pXG5leHBvcnQgY2xhc3MgTWF0Q2FyZFNtSW1hZ2Uge31cblxuLyoqXG4gKiBJbWFnZSB1c2VkIGluIGEgY2FyZCwgbmVlZGVkIHRvIGFkZCB0aGUgbWF0LSBDU1Mgc3R5bGluZy5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnW21hdC1jYXJkLW1kLWltYWdlXSwgW21hdENhcmRJbWFnZU1lZGl1bV0nLFxuICBob3N0OiB7J2NsYXNzJzogJ21hdC1jYXJkLW1kLWltYWdlJ31cbn0pXG5leHBvcnQgY2xhc3MgTWF0Q2FyZE1kSW1hZ2Uge31cblxuLyoqXG4gKiBJbWFnZSB1c2VkIGluIGEgY2FyZCwgbmVlZGVkIHRvIGFkZCB0aGUgbWF0LSBDU1Mgc3R5bGluZy5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnW21hdC1jYXJkLWxnLWltYWdlXSwgW21hdENhcmRJbWFnZUxhcmdlXScsXG4gIGhvc3Q6IHsnY2xhc3MnOiAnbWF0LWNhcmQtbGctaW1hZ2UnfVxufSlcbmV4cG9ydCBjbGFzcyBNYXRDYXJkTGdJbWFnZSB7fVxuXG4vKipcbiAqIExhcmdlIGltYWdlIHVzZWQgaW4gYSBjYXJkLCBuZWVkZWQgdG8gYWRkIHRoZSBtYXQtIENTUyBzdHlsaW5nLlxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICdbbWF0LWNhcmQteGwtaW1hZ2VdLCBbbWF0Q2FyZEltYWdlWExhcmdlXScsXG4gIGhvc3Q6IHsnY2xhc3MnOiAnbWF0LWNhcmQteGwtaW1hZ2UnfVxufSlcbmV4cG9ydCBjbGFzcyBNYXRDYXJkWGxJbWFnZSB7fVxuXG4vKipcbiAqIEF2YXRhciBpbWFnZSB1c2VkIGluIGEgY2FyZCwgbmVlZGVkIHRvIGFkZCB0aGUgbWF0LSBDU1Mgc3R5bGluZy5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnW21hdC1jYXJkLWF2YXRhcl0sIFttYXRDYXJkQXZhdGFyXScsXG4gIGhvc3Q6IHsnY2xhc3MnOiAnbWF0LWNhcmQtYXZhdGFyJ31cbn0pXG5leHBvcnQgY2xhc3MgTWF0Q2FyZEF2YXRhciB7fVxuXG5cbi8qKlxuICogQSBiYXNpYyBjb250ZW50IGNvbnRhaW5lciBjb21wb25lbnQgdGhhdCBhZGRzIHRoZSBzdHlsZXMgb2YgYSBNYXRlcmlhbCBkZXNpZ24gY2FyZC5cbiAqXG4gKiBXaGlsZSB0aGlzIGNvbXBvbmVudCBjYW4gYmUgdXNlZCBhbG9uZSwgaXQgYWxzbyBwcm92aWRlcyBhIG51bWJlclxuICogb2YgcHJlc2V0IHN0eWxlcyBmb3IgY29tbW9uIGNhcmQgc2VjdGlvbnMsIGluY2x1ZGluZzpcbiAqIC0gbWF0LWNhcmQtdGl0bGVcbiAqIC0gbWF0LWNhcmQtc3VidGl0bGVcbiAqIC0gbWF0LWNhcmQtY29udGVudFxuICogLSBtYXQtY2FyZC1hY3Rpb25zXG4gKiAtIG1hdC1jYXJkLWZvb3RlclxuICovXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdtYXQtY2FyZCcsXG4gIGV4cG9ydEFzOiAnbWF0Q2FyZCcsXG4gIHRlbXBsYXRlVXJsOiAnY2FyZC5odG1sJyxcbiAgc3R5bGVVcmxzOiBbJ2NhcmQuY3NzJ10sXG4gIGVuY2Fwc3VsYXRpb246IFZpZXdFbmNhcHN1bGF0aW9uLk5vbmUsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoLFxuICBob3N0OiB7XG4gICAgJ2NsYXNzJzogJ21hdC1jYXJkIG1hdC1mb2N1cy1pbmRpY2F0b3InLFxuICAgICdbY2xhc3MuX21hdC1hbmltYXRpb24tbm9vcGFibGVdJzogJ19hbmltYXRpb25Nb2RlID09PSBcIk5vb3BBbmltYXRpb25zXCInLFxuICB9XG59KVxuZXhwb3J0IGNsYXNzIE1hdENhcmQge1xuICAvLyBAYnJlYWtpbmctY2hhbmdlIDkuMC4wIGBfYW5pbWF0aW9uTW9kZWAgcGFyYW1ldGVyIHRvIGJlIG1hZGUgcmVxdWlyZWQuXG4gIGNvbnN0cnVjdG9yKEBPcHRpb25hbCgpIEBJbmplY3QoQU5JTUFUSU9OX01PRFVMRV9UWVBFKSBwdWJsaWMgX2FuaW1hdGlvbk1vZGU/OiBzdHJpbmcpIHt9XG59XG5cblxuLyoqXG4gKiBDb21wb25lbnQgaW50ZW5kZWQgdG8gYmUgdXNlZCB3aXRoaW4gdGhlIGA8bWF0LWNhcmQ+YCBjb21wb25lbnQuIEl0IGFkZHMgc3R5bGVzIGZvciBhXG4gKiBwcmVzZXQgaGVhZGVyIHNlY3Rpb24gKGkuZS4gYSB0aXRsZSwgc3VidGl0bGUsIGFuZCBhdmF0YXIgbGF5b3V0KS5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnbWF0LWNhcmQtaGVhZGVyJyxcbiAgdGVtcGxhdGVVcmw6ICdjYXJkLWhlYWRlci5odG1sJyxcbiAgZW5jYXBzdWxhdGlvbjogVmlld0VuY2Fwc3VsYXRpb24uTm9uZSxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2gsXG4gIGhvc3Q6IHsnY2xhc3MnOiAnbWF0LWNhcmQtaGVhZGVyJ31cbn0pXG5leHBvcnQgY2xhc3MgTWF0Q2FyZEhlYWRlciB7fVxuXG5cbi8qKlxuICogQ29tcG9uZW50IGludGVuZGVkIHRvIGJlIHVzZWQgd2l0aGluIHRoZSBgPG1hdC1jYXJkPmAgY29tcG9uZW50LiBJdCBhZGRzIHN0eWxlcyBmb3IgYSBwcmVzZXRcbiAqIGxheW91dCB0aGF0IGdyb3VwcyBhbiBpbWFnZSB3aXRoIGEgdGl0bGUgc2VjdGlvbi5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnbWF0LWNhcmQtdGl0bGUtZ3JvdXAnLFxuICB0ZW1wbGF0ZVVybDogJ2NhcmQtdGl0bGUtZ3JvdXAuaHRtbCcsXG4gIGVuY2Fwc3VsYXRpb246IFZpZXdFbmNhcHN1bGF0aW9uLk5vbmUsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoLFxuICBob3N0OiB7J2NsYXNzJzogJ21hdC1jYXJkLXRpdGxlLWdyb3VwJ31cbn0pXG5leHBvcnQgY2xhc3MgTWF0Q2FyZFRpdGxlR3JvdXAge31cbiJdfQ==