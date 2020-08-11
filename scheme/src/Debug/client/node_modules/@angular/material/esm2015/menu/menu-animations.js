/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { trigger, state, style, animate, transition, query, group, } from '@angular/animations';
/**
 * Animations used by the mat-menu component.
 * Animation duration and timing values are based on:
 * https://material.io/guidelines/components/menus.html#menus-usage
 * @docs-private
 */
export const matMenuAnimations = {
    /**
     * This animation controls the menu panel's entry and exit from the page.
     *
     * When the menu panel is added to the DOM, it scales in and fades in its border.
     *
     * When the menu panel is removed from the DOM, it simply fades out after a brief
     * delay to display the ripple.
     */
    transformMenu: trigger('transformMenu', [
        state('void', style({
            opacity: 0,
            transform: 'scale(0.8)'
        })),
        transition('void => enter', group([
            query('.mat-menu-content, .mat-mdc-menu-content', animate('100ms linear', style({
                opacity: 1
            }))),
            animate('120ms cubic-bezier(0, 0, 0.2, 1)', style({ transform: 'scale(1)' })),
        ])),
        transition('* => void', animate('100ms 25ms linear', style({ opacity: 0 })))
    ]),
    /**
     * This animation fades in the background color and content of the menu panel
     * after its containing element is scaled in.
     */
    fadeInItems: trigger('fadeInItems', [
        // TODO(crisbeto): this is inside the `transformMenu`
        // now. Remove next time we do breaking changes.
        state('showing', style({ opacity: 1 })),
        transition('void => *', [
            style({ opacity: 0 }),
            animate('400ms 100ms cubic-bezier(0.55, 0, 0.55, 0.2)')
        ])
    ])
};
/**
 * @deprecated
 * @breaking-change 8.0.0
 * @docs-private
 */
export const fadeInItems = matMenuAnimations.fadeInItems;
/**
 * @deprecated
 * @breaking-change 8.0.0
 * @docs-private
 */
export const transformMenu = matMenuAnimations.transformMenu;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibWVudS1hbmltYXRpb25zLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL21lbnUvbWVudS1hbmltYXRpb25zLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU0sRUFDSixPQUFPLEVBQ1AsS0FBSyxFQUNMLEtBQUssRUFDTCxPQUFPLEVBQ1AsVUFBVSxFQUNWLEtBQUssRUFDTCxLQUFLLEdBRU4sTUFBTSxxQkFBcUIsQ0FBQztBQUU3Qjs7Ozs7R0FLRztBQUNILE1BQU0sQ0FBQyxNQUFNLGlCQUFpQixHQUcxQjtJQUNGOzs7Ozs7O09BT0c7SUFDSCxhQUFhLEVBQUUsT0FBTyxDQUFDLGVBQWUsRUFBRTtRQUN0QyxLQUFLLENBQUMsTUFBTSxFQUFFLEtBQUssQ0FBQztZQUNsQixPQUFPLEVBQUUsQ0FBQztZQUNWLFNBQVMsRUFBRSxZQUFZO1NBQ3hCLENBQUMsQ0FBQztRQUNILFVBQVUsQ0FBQyxlQUFlLEVBQUUsS0FBSyxDQUFDO1lBQ2hDLEtBQUssQ0FBQywwQ0FBMEMsRUFBRSxPQUFPLENBQUMsY0FBYyxFQUFFLEtBQUssQ0FBQztnQkFDOUUsT0FBTyxFQUFFLENBQUM7YUFDWCxDQUFDLENBQUMsQ0FBQztZQUNKLE9BQU8sQ0FBQyxrQ0FBa0MsRUFBRSxLQUFLLENBQUMsRUFBQyxTQUFTLEVBQUUsVUFBVSxFQUFDLENBQUMsQ0FBQztTQUM1RSxDQUFDLENBQUM7UUFDSCxVQUFVLENBQUMsV0FBVyxFQUFFLE9BQU8sQ0FBQyxtQkFBbUIsRUFBRSxLQUFLLENBQUMsRUFBQyxPQUFPLEVBQUUsQ0FBQyxFQUFDLENBQUMsQ0FBQyxDQUFDO0tBQzNFLENBQUM7SUFHRjs7O09BR0c7SUFDSCxXQUFXLEVBQUUsT0FBTyxDQUFDLGFBQWEsRUFBRTtRQUNsQyxxREFBcUQ7UUFDckQsZ0RBQWdEO1FBQ2hELEtBQUssQ0FBQyxTQUFTLEVBQUUsS0FBSyxDQUFDLEVBQUMsT0FBTyxFQUFFLENBQUMsRUFBQyxDQUFDLENBQUM7UUFDckMsVUFBVSxDQUFDLFdBQVcsRUFBRTtZQUN0QixLQUFLLENBQUMsRUFBQyxPQUFPLEVBQUUsQ0FBQyxFQUFDLENBQUM7WUFDbkIsT0FBTyxDQUFDLDhDQUE4QyxDQUFDO1NBQ3hELENBQUM7S0FDSCxDQUFDO0NBQ0gsQ0FBQztBQUVGOzs7O0dBSUc7QUFDSCxNQUFNLENBQUMsTUFBTSxXQUFXLEdBQUcsaUJBQWlCLENBQUMsV0FBVyxDQUFDO0FBRXpEOzs7O0dBSUc7QUFDSCxNQUFNLENBQUMsTUFBTSxhQUFhLEdBQUcsaUJBQWlCLENBQUMsYUFBYSxDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydHtcbiAgdHJpZ2dlcixcbiAgc3RhdGUsXG4gIHN0eWxlLFxuICBhbmltYXRlLFxuICB0cmFuc2l0aW9uLFxuICBxdWVyeSxcbiAgZ3JvdXAsXG4gIEFuaW1hdGlvblRyaWdnZXJNZXRhZGF0YSxcbn0gZnJvbSAnQGFuZ3VsYXIvYW5pbWF0aW9ucyc7XG5cbi8qKlxuICogQW5pbWF0aW9ucyB1c2VkIGJ5IHRoZSBtYXQtbWVudSBjb21wb25lbnQuXG4gKiBBbmltYXRpb24gZHVyYXRpb24gYW5kIHRpbWluZyB2YWx1ZXMgYXJlIGJhc2VkIG9uOlxuICogaHR0cHM6Ly9tYXRlcmlhbC5pby9ndWlkZWxpbmVzL2NvbXBvbmVudHMvbWVudXMuaHRtbCNtZW51cy11c2FnZVxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5leHBvcnQgY29uc3QgbWF0TWVudUFuaW1hdGlvbnM6IHtcbiAgcmVhZG9ubHkgdHJhbnNmb3JtTWVudTogQW5pbWF0aW9uVHJpZ2dlck1ldGFkYXRhO1xuICByZWFkb25seSBmYWRlSW5JdGVtczogQW5pbWF0aW9uVHJpZ2dlck1ldGFkYXRhO1xufSA9IHtcbiAgLyoqXG4gICAqIFRoaXMgYW5pbWF0aW9uIGNvbnRyb2xzIHRoZSBtZW51IHBhbmVsJ3MgZW50cnkgYW5kIGV4aXQgZnJvbSB0aGUgcGFnZS5cbiAgICpcbiAgICogV2hlbiB0aGUgbWVudSBwYW5lbCBpcyBhZGRlZCB0byB0aGUgRE9NLCBpdCBzY2FsZXMgaW4gYW5kIGZhZGVzIGluIGl0cyBib3JkZXIuXG4gICAqXG4gICAqIFdoZW4gdGhlIG1lbnUgcGFuZWwgaXMgcmVtb3ZlZCBmcm9tIHRoZSBET00sIGl0IHNpbXBseSBmYWRlcyBvdXQgYWZ0ZXIgYSBicmllZlxuICAgKiBkZWxheSB0byBkaXNwbGF5IHRoZSByaXBwbGUuXG4gICAqL1xuICB0cmFuc2Zvcm1NZW51OiB0cmlnZ2VyKCd0cmFuc2Zvcm1NZW51JywgW1xuICAgIHN0YXRlKCd2b2lkJywgc3R5bGUoe1xuICAgICAgb3BhY2l0eTogMCxcbiAgICAgIHRyYW5zZm9ybTogJ3NjYWxlKDAuOCknXG4gICAgfSkpLFxuICAgIHRyYW5zaXRpb24oJ3ZvaWQgPT4gZW50ZXInLCBncm91cChbXG4gICAgICBxdWVyeSgnLm1hdC1tZW51LWNvbnRlbnQsIC5tYXQtbWRjLW1lbnUtY29udGVudCcsIGFuaW1hdGUoJzEwMG1zIGxpbmVhcicsIHN0eWxlKHtcbiAgICAgICAgb3BhY2l0eTogMVxuICAgICAgfSkpKSxcbiAgICAgIGFuaW1hdGUoJzEyMG1zIGN1YmljLWJlemllcigwLCAwLCAwLjIsIDEpJywgc3R5bGUoe3RyYW5zZm9ybTogJ3NjYWxlKDEpJ30pKSxcbiAgICBdKSksXG4gICAgdHJhbnNpdGlvbignKiA9PiB2b2lkJywgYW5pbWF0ZSgnMTAwbXMgMjVtcyBsaW5lYXInLCBzdHlsZSh7b3BhY2l0eTogMH0pKSlcbiAgXSksXG5cblxuICAvKipcbiAgICogVGhpcyBhbmltYXRpb24gZmFkZXMgaW4gdGhlIGJhY2tncm91bmQgY29sb3IgYW5kIGNvbnRlbnQgb2YgdGhlIG1lbnUgcGFuZWxcbiAgICogYWZ0ZXIgaXRzIGNvbnRhaW5pbmcgZWxlbWVudCBpcyBzY2FsZWQgaW4uXG4gICAqL1xuICBmYWRlSW5JdGVtczogdHJpZ2dlcignZmFkZUluSXRlbXMnLCBbXG4gICAgLy8gVE9ETyhjcmlzYmV0byk6IHRoaXMgaXMgaW5zaWRlIHRoZSBgdHJhbnNmb3JtTWVudWBcbiAgICAvLyBub3cuIFJlbW92ZSBuZXh0IHRpbWUgd2UgZG8gYnJlYWtpbmcgY2hhbmdlcy5cbiAgICBzdGF0ZSgnc2hvd2luZycsIHN0eWxlKHtvcGFjaXR5OiAxfSkpLFxuICAgIHRyYW5zaXRpb24oJ3ZvaWQgPT4gKicsIFtcbiAgICAgIHN0eWxlKHtvcGFjaXR5OiAwfSksXG4gICAgICBhbmltYXRlKCc0MDBtcyAxMDBtcyBjdWJpYy1iZXppZXIoMC41NSwgMCwgMC41NSwgMC4yKScpXG4gICAgXSlcbiAgXSlcbn07XG5cbi8qKlxuICogQGRlcHJlY2F0ZWRcbiAqIEBicmVha2luZy1jaGFuZ2UgOC4wLjBcbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuZXhwb3J0IGNvbnN0IGZhZGVJbkl0ZW1zID0gbWF0TWVudUFuaW1hdGlvbnMuZmFkZUluSXRlbXM7XG5cbi8qKlxuICogQGRlcHJlY2F0ZWRcbiAqIEBicmVha2luZy1jaGFuZ2UgOC4wLjBcbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuZXhwb3J0IGNvbnN0IHRyYW5zZm9ybU1lbnUgPSBtYXRNZW51QW5pbWF0aW9ucy50cmFuc2Zvcm1NZW51O1xuIl19