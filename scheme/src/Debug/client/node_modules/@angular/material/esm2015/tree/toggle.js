/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { CdkTreeNodeToggle } from '@angular/cdk/tree';
import { Directive, Input } from '@angular/core';
/**
 * Wrapper for the CdkTree's toggle with Material design styles.
 */
let MatTreeNodeToggle = /** @class */ (() => {
    class MatTreeNodeToggle extends CdkTreeNodeToggle {
        constructor() {
            super(...arguments);
            this.recursive = false;
        }
    }
    MatTreeNodeToggle.decorators = [
        { type: Directive, args: [{
                    selector: '[matTreeNodeToggle]',
                    providers: [{ provide: CdkTreeNodeToggle, useExisting: MatTreeNodeToggle }]
                },] }
    ];
    MatTreeNodeToggle.propDecorators = {
        recursive: [{ type: Input, args: ['matTreeNodeToggleRecursive',] }]
    };
    return MatTreeNodeToggle;
})();
export { MatTreeNodeToggle };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidG9nZ2xlLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL3RyZWUvdG9nZ2xlLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU8sRUFBQyxpQkFBaUIsRUFBQyxNQUFNLG1CQUFtQixDQUFDO0FBQ3BELE9BQU8sRUFBQyxTQUFTLEVBQUUsS0FBSyxFQUFDLE1BQU0sZUFBZSxDQUFDO0FBRS9DOztHQUVHO0FBQ0g7SUFBQSxNQUlhLGlCQUFxQixTQUFRLGlCQUFvQjtRQUo5RDs7WUFLdUMsY0FBUyxHQUFZLEtBQUssQ0FBQztRQUNsRSxDQUFDOzs7Z0JBTkEsU0FBUyxTQUFDO29CQUNULFFBQVEsRUFBRSxxQkFBcUI7b0JBQy9CLFNBQVMsRUFBRSxDQUFDLEVBQUMsT0FBTyxFQUFFLGlCQUFpQixFQUFFLFdBQVcsRUFBRSxpQkFBaUIsRUFBQyxDQUFDO2lCQUMxRTs7OzRCQUVFLEtBQUssU0FBQyw0QkFBNEI7O0lBQ3JDLHdCQUFDO0tBQUE7U0FGWSxpQkFBaUIiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtDZGtUcmVlTm9kZVRvZ2dsZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3RyZWUnO1xuaW1wb3J0IHtEaXJlY3RpdmUsIElucHV0fSBmcm9tICdAYW5ndWxhci9jb3JlJztcblxuLyoqXG4gKiBXcmFwcGVyIGZvciB0aGUgQ2RrVHJlZSdzIHRvZ2dsZSB3aXRoIE1hdGVyaWFsIGRlc2lnbiBzdHlsZXMuXG4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogJ1ttYXRUcmVlTm9kZVRvZ2dsZV0nLFxuICBwcm92aWRlcnM6IFt7cHJvdmlkZTogQ2RrVHJlZU5vZGVUb2dnbGUsIHVzZUV4aXN0aW5nOiBNYXRUcmVlTm9kZVRvZ2dsZX1dXG59KVxuZXhwb3J0IGNsYXNzIE1hdFRyZWVOb2RlVG9nZ2xlPFQ+IGV4dGVuZHMgQ2RrVHJlZU5vZGVUb2dnbGU8VD4ge1xuICBASW5wdXQoJ21hdFRyZWVOb2RlVG9nZ2xlUmVjdXJzaXZlJykgcmVjdXJzaXZlOiBib29sZWFuID0gZmFsc2U7XG59XG4iXX0=