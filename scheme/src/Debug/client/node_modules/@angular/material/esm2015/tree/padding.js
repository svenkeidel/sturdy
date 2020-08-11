/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { CdkTreeNodePadding } from '@angular/cdk/tree';
import { Directive, Input } from '@angular/core';
/**
 * Wrapper for the CdkTree padding with Material design styles.
 */
let MatTreeNodePadding = /** @class */ (() => {
    class MatTreeNodePadding extends CdkTreeNodePadding {
    }
    MatTreeNodePadding.decorators = [
        { type: Directive, args: [{
                    selector: '[matTreeNodePadding]',
                    providers: [{ provide: CdkTreeNodePadding, useExisting: MatTreeNodePadding }]
                },] }
    ];
    MatTreeNodePadding.propDecorators = {
        level: [{ type: Input, args: ['matTreeNodePadding',] }],
        indent: [{ type: Input, args: ['matTreeNodePaddingIndent',] }]
    };
    return MatTreeNodePadding;
})();
export { MatTreeNodePadding };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGFkZGluZy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC90cmVlL3BhZGRpbmcudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBQ0gsT0FBTyxFQUFDLGtCQUFrQixFQUFDLE1BQU0sbUJBQW1CLENBQUM7QUFDckQsT0FBTyxFQUFDLFNBQVMsRUFBRSxLQUFLLEVBQUMsTUFBTSxlQUFlLENBQUM7QUFFL0M7O0dBRUc7QUFDSDtJQUFBLE1BSWEsa0JBQXNCLFNBQVEsa0JBQXFCOzs7Z0JBSi9ELFNBQVMsU0FBQztvQkFDVCxRQUFRLEVBQUUsc0JBQXNCO29CQUNoQyxTQUFTLEVBQUUsQ0FBQyxFQUFDLE9BQU8sRUFBRSxrQkFBa0IsRUFBRSxXQUFXLEVBQUUsa0JBQWtCLEVBQUMsQ0FBQztpQkFDNUU7Ozt3QkFJRSxLQUFLLFNBQUMsb0JBQW9CO3lCQUcxQixLQUFLLFNBQUMsMEJBQTBCOztJQUNuQyx5QkFBQztLQUFBO1NBUFksa0JBQWtCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5pbXBvcnQge0Nka1RyZWVOb2RlUGFkZGluZ30gZnJvbSAnQGFuZ3VsYXIvY2RrL3RyZWUnO1xuaW1wb3J0IHtEaXJlY3RpdmUsIElucHV0fSBmcm9tICdAYW5ndWxhci9jb3JlJztcblxuLyoqXG4gKiBXcmFwcGVyIGZvciB0aGUgQ2RrVHJlZSBwYWRkaW5nIHdpdGggTWF0ZXJpYWwgZGVzaWduIHN0eWxlcy5cbiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnW21hdFRyZWVOb2RlUGFkZGluZ10nLFxuICBwcm92aWRlcnM6IFt7cHJvdmlkZTogQ2RrVHJlZU5vZGVQYWRkaW5nLCB1c2VFeGlzdGluZzogTWF0VHJlZU5vZGVQYWRkaW5nfV1cbn0pXG5leHBvcnQgY2xhc3MgTWF0VHJlZU5vZGVQYWRkaW5nPFQ+IGV4dGVuZHMgQ2RrVHJlZU5vZGVQYWRkaW5nPFQ+IHtcblxuICAvKiogVGhlIGxldmVsIG9mIGRlcHRoIG9mIHRoZSB0cmVlIG5vZGUuIFRoZSBwYWRkaW5nIHdpbGwgYmUgYGxldmVsICogaW5kZW50YCBwaXhlbHMuICovXG4gIEBJbnB1dCgnbWF0VHJlZU5vZGVQYWRkaW5nJykgbGV2ZWw6IG51bWJlcjtcblxuICAvKiogVGhlIGluZGVudCBmb3IgZWFjaCBsZXZlbC4gRGVmYXVsdCBudW1iZXIgNDBweCBmcm9tIG1hdGVyaWFsIGRlc2lnbiBtZW51IHN1Yi1tZW51IHNwZWMuICovXG4gIEBJbnB1dCgnbWF0VHJlZU5vZGVQYWRkaW5nSW5kZW50JykgaW5kZW50OiBudW1iZXI7XG59XG4iXX0=