/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Directive } from '@angular/core';
import { CdkPortal } from '@angular/cdk/portal';
/** Used to flag tab labels for use with the portal directive */
let MatTabLabel = /** @class */ (() => {
    class MatTabLabel extends CdkPortal {
    }
    MatTabLabel.decorators = [
        { type: Directive, args: [{
                    selector: '[mat-tab-label], [matTabLabel]',
                },] }
    ];
    return MatTabLabel;
})();
export { MatTabLabel };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidGFiLWxhYmVsLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL3RhYnMvdGFiLWxhYmVsLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU8sRUFBQyxTQUFTLEVBQUMsTUFBTSxlQUFlLENBQUM7QUFDeEMsT0FBTyxFQUFDLFNBQVMsRUFBQyxNQUFNLHFCQUFxQixDQUFDO0FBRTlDLGdFQUFnRTtBQUNoRTtJQUFBLE1BR2EsV0FBWSxTQUFRLFNBQVM7OztnQkFIekMsU0FBUyxTQUFDO29CQUNULFFBQVEsRUFBRSxnQ0FBZ0M7aUJBQzNDOztJQUMyQyxrQkFBQztLQUFBO1NBQWhDLFdBQVciLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtEaXJlY3RpdmV9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtDZGtQb3J0YWx9IGZyb20gJ0Bhbmd1bGFyL2Nkay9wb3J0YWwnO1xuXG4vKiogVXNlZCB0byBmbGFnIHRhYiBsYWJlbHMgZm9yIHVzZSB3aXRoIHRoZSBwb3J0YWwgZGlyZWN0aXZlICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICdbbWF0LXRhYi1sYWJlbF0sIFttYXRUYWJMYWJlbF0nLFxufSlcbmV4cG9ydCBjbGFzcyBNYXRUYWJMYWJlbCBleHRlbmRzIENka1BvcnRhbCB7fVxuIl19