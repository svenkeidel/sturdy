/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { NgModule, Directive, } from '@angular/core';
import { startWith } from 'rxjs/operators';
import { MatCommonModule } from '../common-behaviors/common-module';
/**
 * Shared directive to count lines inside a text area, such as a list item.
 * Line elements can be extracted with a @ContentChildren(MatLine) query, then
 * counted by checking the query list's length.
 */
let MatLine = /** @class */ (() => {
    class MatLine {
    }
    MatLine.decorators = [
        { type: Directive, args: [{
                    selector: '[mat-line], [matLine]',
                    host: { 'class': 'mat-line' }
                },] }
    ];
    return MatLine;
})();
export { MatLine };
/**
 * Helper that takes a query list of lines and sets the correct class on the host.
 * @docs-private
 */
export function setLines(lines, element, prefix = 'mat') {
    // Note: doesn't need to unsubscribe, because `changes`
    // gets completed by Angular when the view is destroyed.
    lines.changes.pipe(startWith(lines)).subscribe(({ length }) => {
        setClass(element, `${prefix}-2-line`, false);
        setClass(element, `${prefix}-3-line`, false);
        setClass(element, `${prefix}-multi-line`, false);
        if (length === 2 || length === 3) {
            setClass(element, `${prefix}-${length}-line`, true);
        }
        else if (length > 3) {
            setClass(element, `${prefix}-multi-line`, true);
        }
    });
}
/** Adds or removes a class from an element. */
function setClass(element, className, isAdd) {
    const classList = element.nativeElement.classList;
    isAdd ? classList.add(className) : classList.remove(className);
}
/**
 * Helper that takes a query list of lines and sets the correct class on the host.
 * @docs-private
 * @deprecated Use `setLines` instead.
 * @breaking-change 8.0.0
 */
export class MatLineSetter {
    constructor(lines, element) {
        setLines(lines, element);
    }
}
let MatLineModule = /** @class */ (() => {
    class MatLineModule {
    }
    MatLineModule.decorators = [
        { type: NgModule, args: [{
                    imports: [MatCommonModule],
                    exports: [MatLine, MatCommonModule],
                    declarations: [MatLine],
                },] }
    ];
    return MatLineModule;
})();
export { MatLineModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibGluZS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9jb3JlL2xpbmUvbGluZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQ0wsUUFBUSxFQUNSLFNBQVMsR0FHVixNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUMsU0FBUyxFQUFDLE1BQU0sZ0JBQWdCLENBQUM7QUFDekMsT0FBTyxFQUFDLGVBQWUsRUFBQyxNQUFNLG1DQUFtQyxDQUFDO0FBR2xFOzs7O0dBSUc7QUFDSDtJQUFBLE1BSWEsT0FBTzs7O2dCQUpuQixTQUFTLFNBQUM7b0JBQ1QsUUFBUSxFQUFFLHVCQUF1QjtvQkFDakMsSUFBSSxFQUFFLEVBQUMsT0FBTyxFQUFFLFVBQVUsRUFBQztpQkFDNUI7O0lBQ3FCLGNBQUM7S0FBQTtTQUFWLE9BQU87QUFFcEI7OztHQUdHO0FBQ0gsTUFBTSxVQUFVLFFBQVEsQ0FBQyxLQUF5QixFQUFFLE9BQWdDLEVBQzNELE1BQU0sR0FBRyxLQUFLO0lBQ3JDLHVEQUF1RDtJQUN2RCx3REFBd0Q7SUFDeEQsS0FBSyxDQUFDLE9BQU8sQ0FBQyxJQUFJLENBQUMsU0FBUyxDQUFDLEtBQUssQ0FBQyxDQUFDLENBQUMsU0FBUyxDQUFDLENBQUMsRUFBQyxNQUFNLEVBQUMsRUFBRSxFQUFFO1FBQzFELFFBQVEsQ0FBQyxPQUFPLEVBQUUsR0FBRyxNQUFNLFNBQVMsRUFBRSxLQUFLLENBQUMsQ0FBQztRQUM3QyxRQUFRLENBQUMsT0FBTyxFQUFFLEdBQUcsTUFBTSxTQUFTLEVBQUUsS0FBSyxDQUFDLENBQUM7UUFDN0MsUUFBUSxDQUFDLE9BQU8sRUFBRSxHQUFHLE1BQU0sYUFBYSxFQUFFLEtBQUssQ0FBQyxDQUFDO1FBRWpELElBQUksTUFBTSxLQUFLLENBQUMsSUFBSSxNQUFNLEtBQUssQ0FBQyxFQUFFO1lBQ2hDLFFBQVEsQ0FBQyxPQUFPLEVBQUUsR0FBRyxNQUFNLElBQUksTUFBTSxPQUFPLEVBQUUsSUFBSSxDQUFDLENBQUM7U0FDckQ7YUFBTSxJQUFJLE1BQU0sR0FBRyxDQUFDLEVBQUU7WUFDckIsUUFBUSxDQUFDLE9BQU8sRUFBRSxHQUFHLE1BQU0sYUFBYSxFQUFFLElBQUksQ0FBQyxDQUFDO1NBQ2pEO0lBQ0gsQ0FBQyxDQUFDLENBQUM7QUFDTCxDQUFDO0FBRUQsK0NBQStDO0FBQy9DLFNBQVMsUUFBUSxDQUFDLE9BQWdDLEVBQUUsU0FBaUIsRUFBRSxLQUFjO0lBQ25GLE1BQU0sU0FBUyxHQUFHLE9BQU8sQ0FBQyxhQUFhLENBQUMsU0FBUyxDQUFDO0lBQ2xELEtBQUssQ0FBQyxDQUFDLENBQUMsU0FBUyxDQUFDLEdBQUcsQ0FBQyxTQUFTLENBQUMsQ0FBQyxDQUFDLENBQUMsU0FBUyxDQUFDLE1BQU0sQ0FBQyxTQUFTLENBQUMsQ0FBQztBQUNqRSxDQUFDO0FBRUQ7Ozs7O0dBS0c7QUFDSCxNQUFNLE9BQU8sYUFBYTtJQUN4QixZQUFZLEtBQXlCLEVBQUUsT0FBZ0M7UUFDckUsUUFBUSxDQUFDLEtBQUssRUFBRSxPQUFPLENBQUMsQ0FBQztJQUMzQixDQUFDO0NBQ0Y7QUFFRDtJQUFBLE1BS2EsYUFBYTs7O2dCQUx6QixRQUFRLFNBQUM7b0JBQ1IsT0FBTyxFQUFFLENBQUMsZUFBZSxDQUFDO29CQUMxQixPQUFPLEVBQUUsQ0FBQyxPQUFPLEVBQUUsZUFBZSxDQUFDO29CQUNuQyxZQUFZLEVBQUUsQ0FBQyxPQUFPLENBQUM7aUJBQ3hCOztJQUM0QixvQkFBQztLQUFBO1NBQWpCLGFBQWEiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtcbiAgTmdNb2R1bGUsXG4gIERpcmVjdGl2ZSxcbiAgRWxlbWVudFJlZixcbiAgUXVlcnlMaXN0LFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7c3RhcnRXaXRofSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5pbXBvcnQge01hdENvbW1vbk1vZHVsZX0gZnJvbSAnLi4vY29tbW9uLWJlaGF2aW9ycy9jb21tb24tbW9kdWxlJztcblxuXG4vKipcbiAqIFNoYXJlZCBkaXJlY3RpdmUgdG8gY291bnQgbGluZXMgaW5zaWRlIGEgdGV4dCBhcmVhLCBzdWNoIGFzIGEgbGlzdCBpdGVtLlxuICogTGluZSBlbGVtZW50cyBjYW4gYmUgZXh0cmFjdGVkIHdpdGggYSBAQ29udGVudENoaWxkcmVuKE1hdExpbmUpIHF1ZXJ5LCB0aGVuXG4gKiBjb3VudGVkIGJ5IGNoZWNraW5nIHRoZSBxdWVyeSBsaXN0J3MgbGVuZ3RoLlxuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICdbbWF0LWxpbmVdLCBbbWF0TGluZV0nLFxuICBob3N0OiB7J2NsYXNzJzogJ21hdC1saW5lJ31cbn0pXG5leHBvcnQgY2xhc3MgTWF0TGluZSB7fVxuXG4vKipcbiAqIEhlbHBlciB0aGF0IHRha2VzIGEgcXVlcnkgbGlzdCBvZiBsaW5lcyBhbmQgc2V0cyB0aGUgY29ycmVjdCBjbGFzcyBvbiB0aGUgaG9zdC5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHNldExpbmVzKGxpbmVzOiBRdWVyeUxpc3Q8dW5rbm93bj4sIGVsZW1lbnQ6IEVsZW1lbnRSZWY8SFRNTEVsZW1lbnQ+LFxuICAgICAgICAgICAgICAgICAgICAgICAgIHByZWZpeCA9ICdtYXQnKSB7XG4gIC8vIE5vdGU6IGRvZXNuJ3QgbmVlZCB0byB1bnN1YnNjcmliZSwgYmVjYXVzZSBgY2hhbmdlc2BcbiAgLy8gZ2V0cyBjb21wbGV0ZWQgYnkgQW5ndWxhciB3aGVuIHRoZSB2aWV3IGlzIGRlc3Ryb3llZC5cbiAgbGluZXMuY2hhbmdlcy5waXBlKHN0YXJ0V2l0aChsaW5lcykpLnN1YnNjcmliZSgoe2xlbmd0aH0pID0+IHtcbiAgICBzZXRDbGFzcyhlbGVtZW50LCBgJHtwcmVmaXh9LTItbGluZWAsIGZhbHNlKTtcbiAgICBzZXRDbGFzcyhlbGVtZW50LCBgJHtwcmVmaXh9LTMtbGluZWAsIGZhbHNlKTtcbiAgICBzZXRDbGFzcyhlbGVtZW50LCBgJHtwcmVmaXh9LW11bHRpLWxpbmVgLCBmYWxzZSk7XG5cbiAgICBpZiAobGVuZ3RoID09PSAyIHx8IGxlbmd0aCA9PT0gMykge1xuICAgICAgc2V0Q2xhc3MoZWxlbWVudCwgYCR7cHJlZml4fS0ke2xlbmd0aH0tbGluZWAsIHRydWUpO1xuICAgIH0gZWxzZSBpZiAobGVuZ3RoID4gMykge1xuICAgICAgc2V0Q2xhc3MoZWxlbWVudCwgYCR7cHJlZml4fS1tdWx0aS1saW5lYCwgdHJ1ZSk7XG4gICAgfVxuICB9KTtcbn1cblxuLyoqIEFkZHMgb3IgcmVtb3ZlcyBhIGNsYXNzIGZyb20gYW4gZWxlbWVudC4gKi9cbmZ1bmN0aW9uIHNldENsYXNzKGVsZW1lbnQ6IEVsZW1lbnRSZWY8SFRNTEVsZW1lbnQ+LCBjbGFzc05hbWU6IHN0cmluZywgaXNBZGQ6IGJvb2xlYW4pOiB2b2lkIHtcbiAgY29uc3QgY2xhc3NMaXN0ID0gZWxlbWVudC5uYXRpdmVFbGVtZW50LmNsYXNzTGlzdDtcbiAgaXNBZGQgPyBjbGFzc0xpc3QuYWRkKGNsYXNzTmFtZSkgOiBjbGFzc0xpc3QucmVtb3ZlKGNsYXNzTmFtZSk7XG59XG5cbi8qKlxuICogSGVscGVyIHRoYXQgdGFrZXMgYSBxdWVyeSBsaXN0IG9mIGxpbmVzIGFuZCBzZXRzIHRoZSBjb3JyZWN0IGNsYXNzIG9uIHRoZSBob3N0LlxuICogQGRvY3MtcHJpdmF0ZVxuICogQGRlcHJlY2F0ZWQgVXNlIGBzZXRMaW5lc2AgaW5zdGVhZC5cbiAqIEBicmVha2luZy1jaGFuZ2UgOC4wLjBcbiAqL1xuZXhwb3J0IGNsYXNzIE1hdExpbmVTZXR0ZXIge1xuICBjb25zdHJ1Y3RvcihsaW5lczogUXVlcnlMaXN0PE1hdExpbmU+LCBlbGVtZW50OiBFbGVtZW50UmVmPEhUTUxFbGVtZW50Pikge1xuICAgIHNldExpbmVzKGxpbmVzLCBlbGVtZW50KTtcbiAgfVxufVxuXG5ATmdNb2R1bGUoe1xuICBpbXBvcnRzOiBbTWF0Q29tbW9uTW9kdWxlXSxcbiAgZXhwb3J0czogW01hdExpbmUsIE1hdENvbW1vbk1vZHVsZV0sXG4gIGRlY2xhcmF0aW9uczogW01hdExpbmVdLFxufSlcbmV4cG9ydCBjbGFzcyBNYXRMaW5lTW9kdWxlIHsgfVxuIl19