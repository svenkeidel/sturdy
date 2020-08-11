/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ENTER } from '@angular/cdk/keycodes';
import { NgModule } from '@angular/core';
import { ErrorStateMatcher } from '@angular/material/core';
import { MatChip, MatChipAvatar, MatChipRemove, MatChipTrailingIcon } from './chip';
import { MAT_CHIPS_DEFAULT_OPTIONS } from './chip-default-options';
import { MatChipInput } from './chip-input';
import { MatChipList } from './chip-list';
const CHIP_DECLARATIONS = [
    MatChipList,
    MatChip,
    MatChipInput,
    MatChipRemove,
    MatChipAvatar,
    MatChipTrailingIcon,
];
const ɵ0 = {
    separatorKeyCodes: [ENTER]
};
let MatChipsModule = /** @class */ (() => {
    class MatChipsModule {
    }
    MatChipsModule.decorators = [
        { type: NgModule, args: [{
                    exports: CHIP_DECLARATIONS,
                    declarations: CHIP_DECLARATIONS,
                    providers: [
                        ErrorStateMatcher,
                        {
                            provide: MAT_CHIPS_DEFAULT_OPTIONS,
                            useValue: ɵ0
                        }
                    ]
                },] }
    ];
    return MatChipsModule;
})();
export { MatChipsModule };
export { ɵ0 };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2hpcHMtbW9kdWxlLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL2NoaXBzL2NoaXBzLW1vZHVsZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsS0FBSyxFQUFDLE1BQU0sdUJBQXVCLENBQUM7QUFDNUMsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUN2QyxPQUFPLEVBQUMsaUJBQWlCLEVBQUMsTUFBTSx3QkFBd0IsQ0FBQztBQUN6RCxPQUFPLEVBQUMsT0FBTyxFQUFFLGFBQWEsRUFBRSxhQUFhLEVBQUUsbUJBQW1CLEVBQUMsTUFBTSxRQUFRLENBQUM7QUFDbEYsT0FBTyxFQUFDLHlCQUF5QixFQUF5QixNQUFNLHdCQUF3QixDQUFDO0FBQ3pGLE9BQU8sRUFBQyxZQUFZLEVBQUMsTUFBTSxjQUFjLENBQUM7QUFDMUMsT0FBTyxFQUFDLFdBQVcsRUFBQyxNQUFNLGFBQWEsQ0FBQztBQUV4QyxNQUFNLGlCQUFpQixHQUFHO0lBQ3hCLFdBQVc7SUFDWCxPQUFPO0lBQ1AsWUFBWTtJQUNaLGFBQWE7SUFDYixhQUFhO0lBQ2IsbUJBQW1CO0NBQ3BCLENBQUM7V0FTYztJQUNSLGlCQUFpQixFQUFFLENBQUMsS0FBSyxDQUFDO0NBQ0Q7QUFUakM7SUFBQSxNQWFhLGNBQWM7OztnQkFiMUIsUUFBUSxTQUFDO29CQUNSLE9BQU8sRUFBRSxpQkFBaUI7b0JBQzFCLFlBQVksRUFBRSxpQkFBaUI7b0JBQy9CLFNBQVMsRUFBRTt3QkFDVCxpQkFBaUI7d0JBQ2pCOzRCQUNFLE9BQU8sRUFBRSx5QkFBeUI7NEJBQ2xDLFFBQVEsSUFFbUI7eUJBQzVCO3FCQUNGO2lCQUNGOztJQUM0QixxQkFBQztLQUFBO1NBQWpCLGNBQWMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtFTlRFUn0gZnJvbSAnQGFuZ3VsYXIvY2RrL2tleWNvZGVzJztcbmltcG9ydCB7TmdNb2R1bGV9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtFcnJvclN0YXRlTWF0Y2hlcn0gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvY29yZSc7XG5pbXBvcnQge01hdENoaXAsIE1hdENoaXBBdmF0YXIsIE1hdENoaXBSZW1vdmUsIE1hdENoaXBUcmFpbGluZ0ljb259IGZyb20gJy4vY2hpcCc7XG5pbXBvcnQge01BVF9DSElQU19ERUZBVUxUX09QVElPTlMsIE1hdENoaXBzRGVmYXVsdE9wdGlvbnN9IGZyb20gJy4vY2hpcC1kZWZhdWx0LW9wdGlvbnMnO1xuaW1wb3J0IHtNYXRDaGlwSW5wdXR9IGZyb20gJy4vY2hpcC1pbnB1dCc7XG5pbXBvcnQge01hdENoaXBMaXN0fSBmcm9tICcuL2NoaXAtbGlzdCc7XG5cbmNvbnN0IENISVBfREVDTEFSQVRJT05TID0gW1xuICBNYXRDaGlwTGlzdCxcbiAgTWF0Q2hpcCxcbiAgTWF0Q2hpcElucHV0LFxuICBNYXRDaGlwUmVtb3ZlLFxuICBNYXRDaGlwQXZhdGFyLFxuICBNYXRDaGlwVHJhaWxpbmdJY29uLFxuXTtcblxuQE5nTW9kdWxlKHtcbiAgZXhwb3J0czogQ0hJUF9ERUNMQVJBVElPTlMsXG4gIGRlY2xhcmF0aW9uczogQ0hJUF9ERUNMQVJBVElPTlMsXG4gIHByb3ZpZGVyczogW1xuICAgIEVycm9yU3RhdGVNYXRjaGVyLFxuICAgIHtcbiAgICAgIHByb3ZpZGU6IE1BVF9DSElQU19ERUZBVUxUX09QVElPTlMsXG4gICAgICB1c2VWYWx1ZToge1xuICAgICAgICBzZXBhcmF0b3JLZXlDb2RlczogW0VOVEVSXVxuICAgICAgfSBhcyBNYXRDaGlwc0RlZmF1bHRPcHRpb25zXG4gICAgfVxuICBdXG59KVxuZXhwb3J0IGNsYXNzIE1hdENoaXBzTW9kdWxlIHt9XG4iXX0=