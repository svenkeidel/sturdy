/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/**
 * Configuration for opening a modal dialog with the MatDialog service.
 */
export class MatDialogConfig {
    constructor() {
        /** The ARIA role of the dialog element. */
        this.role = 'dialog';
        /** Custom class for the overlay pane. */
        this.panelClass = '';
        /** Whether the dialog has a backdrop. */
        this.hasBackdrop = true;
        /** Custom class for the backdrop. */
        this.backdropClass = '';
        /** Whether the user can use escape or clicking on the backdrop to close the modal. */
        this.disableClose = false;
        /** Width of the dialog. */
        this.width = '';
        /** Height of the dialog. */
        this.height = '';
        /** Max-width of the dialog. If a number is provided, assumes pixel units. Defaults to 80vw. */
        this.maxWidth = '80vw';
        /** Data being injected into the child component. */
        this.data = null;
        /** ID of the element that describes the dialog. */
        this.ariaDescribedBy = null;
        /** ID of the element that labels the dialog. */
        this.ariaLabelledBy = null;
        /** Aria label to assign to the dialog element. */
        this.ariaLabel = null;
        /** Whether the dialog should focus the first focusable element on open. */
        this.autoFocus = true;
        /**
         * Whether the dialog should restore focus to the
         * previously-focused element, after it's closed.
         */
        this.restoreFocus = true;
        /**
         * Whether the dialog should close when the user goes backwards/forwards in history.
         * Note that this usually doesn't include clicking on links (unless the user is using
         * the `HashLocationStrategy`).
         */
        this.closeOnNavigation = true;
        // TODO(jelbourn): add configuration for lifecycle hooks, ARIA labelling.
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGlhbG9nLWNvbmZpZy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9kaWFsb2cvZGlhbG9nLWNvbmZpZy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUF3Qkg7O0dBRUc7QUFDSCxNQUFNLE9BQU8sZUFBZTtJQUE1QjtRQWFFLDJDQUEyQztRQUMzQyxTQUFJLEdBQWdCLFFBQVEsQ0FBQztRQUU3Qix5Q0FBeUM7UUFDekMsZUFBVSxHQUF1QixFQUFFLENBQUM7UUFFcEMseUNBQXlDO1FBQ3pDLGdCQUFXLEdBQWEsSUFBSSxDQUFDO1FBRTdCLHFDQUFxQztRQUNyQyxrQkFBYSxHQUFZLEVBQUUsQ0FBQztRQUU1QixzRkFBc0Y7UUFDdEYsaUJBQVksR0FBYSxLQUFLLENBQUM7UUFFL0IsMkJBQTJCO1FBQzNCLFVBQUssR0FBWSxFQUFFLENBQUM7UUFFcEIsNEJBQTRCO1FBQzVCLFdBQU0sR0FBWSxFQUFFLENBQUM7UUFRckIsK0ZBQStGO1FBQy9GLGFBQVEsR0FBcUIsTUFBTSxDQUFDO1FBUXBDLG9EQUFvRDtRQUNwRCxTQUFJLEdBQWMsSUFBSSxDQUFDO1FBS3ZCLG1EQUFtRDtRQUNuRCxvQkFBZSxHQUFtQixJQUFJLENBQUM7UUFFdkMsZ0RBQWdEO1FBQ2hELG1CQUFjLEdBQW1CLElBQUksQ0FBQztRQUV0QyxrREFBa0Q7UUFDbEQsY0FBUyxHQUFtQixJQUFJLENBQUM7UUFFakMsMkVBQTJFO1FBQzNFLGNBQVMsR0FBYSxJQUFJLENBQUM7UUFFM0I7OztXQUdHO1FBQ0gsaUJBQVksR0FBYSxJQUFJLENBQUM7UUFLOUI7Ozs7V0FJRztRQUNILHNCQUFpQixHQUFhLElBQUksQ0FBQztRQUtuQyx5RUFBeUU7SUFDM0UsQ0FBQztDQUFBIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Vmlld0NvbnRhaW5lclJlZiwgQ29tcG9uZW50RmFjdG9yeVJlc29sdmVyfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7RGlyZWN0aW9ufSBmcm9tICdAYW5ndWxhci9jZGsvYmlkaSc7XG5pbXBvcnQge1Njcm9sbFN0cmF0ZWd5fSBmcm9tICdAYW5ndWxhci9jZGsvb3ZlcmxheSc7XG5cbi8qKiBWYWxpZCBBUklBIHJvbGVzIGZvciBhIGRpYWxvZyBlbGVtZW50LiAqL1xuZXhwb3J0IHR5cGUgRGlhbG9nUm9sZSA9ICdkaWFsb2cnIHwgJ2FsZXJ0ZGlhbG9nJztcblxuLyoqIFBvc3NpYmxlIG92ZXJyaWRlcyBmb3IgYSBkaWFsb2cncyBwb3NpdGlvbi4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgRGlhbG9nUG9zaXRpb24ge1xuICAvKiogT3ZlcnJpZGUgZm9yIHRoZSBkaWFsb2cncyB0b3AgcG9zaXRpb24uICovXG4gIHRvcD86IHN0cmluZztcblxuICAvKiogT3ZlcnJpZGUgZm9yIHRoZSBkaWFsb2cncyBib3R0b20gcG9zaXRpb24uICovXG4gIGJvdHRvbT86IHN0cmluZztcblxuICAvKiogT3ZlcnJpZGUgZm9yIHRoZSBkaWFsb2cncyBsZWZ0IHBvc2l0aW9uLiAqL1xuICBsZWZ0Pzogc3RyaW5nO1xuXG4gIC8qKiBPdmVycmlkZSBmb3IgdGhlIGRpYWxvZydzIHJpZ2h0IHBvc2l0aW9uLiAqL1xuICByaWdodD86IHN0cmluZztcbn1cblxuLyoqXG4gKiBDb25maWd1cmF0aW9uIGZvciBvcGVuaW5nIGEgbW9kYWwgZGlhbG9nIHdpdGggdGhlIE1hdERpYWxvZyBzZXJ2aWNlLlxuICovXG5leHBvcnQgY2xhc3MgTWF0RGlhbG9nQ29uZmlnPEQgPSBhbnk+IHtcblxuICAvKipcbiAgICogV2hlcmUgdGhlIGF0dGFjaGVkIGNvbXBvbmVudCBzaG91bGQgbGl2ZSBpbiBBbmd1bGFyJ3MgKmxvZ2ljYWwqIGNvbXBvbmVudCB0cmVlLlxuICAgKiBUaGlzIGFmZmVjdHMgd2hhdCBpcyBhdmFpbGFibGUgZm9yIGluamVjdGlvbiBhbmQgdGhlIGNoYW5nZSBkZXRlY3Rpb24gb3JkZXIgZm9yIHRoZVxuICAgKiBjb21wb25lbnQgaW5zdGFudGlhdGVkIGluc2lkZSBvZiB0aGUgZGlhbG9nLiBUaGlzIGRvZXMgbm90IGFmZmVjdCB3aGVyZSB0aGUgZGlhbG9nXG4gICAqIGNvbnRlbnQgd2lsbCBiZSByZW5kZXJlZC5cbiAgICovXG4gIHZpZXdDb250YWluZXJSZWY/OiBWaWV3Q29udGFpbmVyUmVmO1xuXG4gIC8qKiBJRCBmb3IgdGhlIGRpYWxvZy4gSWYgb21pdHRlZCwgYSB1bmlxdWUgb25lIHdpbGwgYmUgZ2VuZXJhdGVkLiAqL1xuICBpZD86IHN0cmluZztcblxuICAvKiogVGhlIEFSSUEgcm9sZSBvZiB0aGUgZGlhbG9nIGVsZW1lbnQuICovXG4gIHJvbGU/OiBEaWFsb2dSb2xlID0gJ2RpYWxvZyc7XG5cbiAgLyoqIEN1c3RvbSBjbGFzcyBmb3IgdGhlIG92ZXJsYXkgcGFuZS4gKi9cbiAgcGFuZWxDbGFzcz86IHN0cmluZyB8IHN0cmluZ1tdID0gJyc7XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGRpYWxvZyBoYXMgYSBiYWNrZHJvcC4gKi9cbiAgaGFzQmFja2Ryb3A/OiBib29sZWFuID0gdHJ1ZTtcblxuICAvKiogQ3VzdG9tIGNsYXNzIGZvciB0aGUgYmFja2Ryb3AuICovXG4gIGJhY2tkcm9wQ2xhc3M/OiBzdHJpbmcgPSAnJztcblxuICAvKiogV2hldGhlciB0aGUgdXNlciBjYW4gdXNlIGVzY2FwZSBvciBjbGlja2luZyBvbiB0aGUgYmFja2Ryb3AgdG8gY2xvc2UgdGhlIG1vZGFsLiAqL1xuICBkaXNhYmxlQ2xvc2U/OiBib29sZWFuID0gZmFsc2U7XG5cbiAgLyoqIFdpZHRoIG9mIHRoZSBkaWFsb2cuICovXG4gIHdpZHRoPzogc3RyaW5nID0gJyc7XG5cbiAgLyoqIEhlaWdodCBvZiB0aGUgZGlhbG9nLiAqL1xuICBoZWlnaHQ/OiBzdHJpbmcgPSAnJztcblxuICAvKiogTWluLXdpZHRoIG9mIHRoZSBkaWFsb2cuIElmIGEgbnVtYmVyIGlzIHByb3ZpZGVkLCBhc3N1bWVzIHBpeGVsIHVuaXRzLiAqL1xuICBtaW5XaWR0aD86IG51bWJlciB8IHN0cmluZztcblxuICAvKiogTWluLWhlaWdodCBvZiB0aGUgZGlhbG9nLiBJZiBhIG51bWJlciBpcyBwcm92aWRlZCwgYXNzdW1lcyBwaXhlbCB1bml0cy4gKi9cbiAgbWluSGVpZ2h0PzogbnVtYmVyIHwgc3RyaW5nO1xuXG4gIC8qKiBNYXgtd2lkdGggb2YgdGhlIGRpYWxvZy4gSWYgYSBudW1iZXIgaXMgcHJvdmlkZWQsIGFzc3VtZXMgcGl4ZWwgdW5pdHMuIERlZmF1bHRzIHRvIDgwdncuICovXG4gIG1heFdpZHRoPzogbnVtYmVyIHwgc3RyaW5nID0gJzgwdncnO1xuXG4gIC8qKiBNYXgtaGVpZ2h0IG9mIHRoZSBkaWFsb2cuIElmIGEgbnVtYmVyIGlzIHByb3ZpZGVkLCBhc3N1bWVzIHBpeGVsIHVuaXRzLiAqL1xuICBtYXhIZWlnaHQ/OiBudW1iZXIgfCBzdHJpbmc7XG5cbiAgLyoqIFBvc2l0aW9uIG92ZXJyaWRlcy4gKi9cbiAgcG9zaXRpb24/OiBEaWFsb2dQb3NpdGlvbjtcblxuICAvKiogRGF0YSBiZWluZyBpbmplY3RlZCBpbnRvIHRoZSBjaGlsZCBjb21wb25lbnQuICovXG4gIGRhdGE/OiBEIHwgbnVsbCA9IG51bGw7XG5cbiAgLyoqIExheW91dCBkaXJlY3Rpb24gZm9yIHRoZSBkaWFsb2cncyBjb250ZW50LiAqL1xuICBkaXJlY3Rpb24/OiBEaXJlY3Rpb247XG5cbiAgLyoqIElEIG9mIHRoZSBlbGVtZW50IHRoYXQgZGVzY3JpYmVzIHRoZSBkaWFsb2cuICovXG4gIGFyaWFEZXNjcmliZWRCeT86IHN0cmluZyB8IG51bGwgPSBudWxsO1xuXG4gIC8qKiBJRCBvZiB0aGUgZWxlbWVudCB0aGF0IGxhYmVscyB0aGUgZGlhbG9nLiAqL1xuICBhcmlhTGFiZWxsZWRCeT86IHN0cmluZyB8IG51bGwgPSBudWxsO1xuXG4gIC8qKiBBcmlhIGxhYmVsIHRvIGFzc2lnbiB0byB0aGUgZGlhbG9nIGVsZW1lbnQuICovXG4gIGFyaWFMYWJlbD86IHN0cmluZyB8IG51bGwgPSBudWxsO1xuXG4gIC8qKiBXaGV0aGVyIHRoZSBkaWFsb2cgc2hvdWxkIGZvY3VzIHRoZSBmaXJzdCBmb2N1c2FibGUgZWxlbWVudCBvbiBvcGVuLiAqL1xuICBhdXRvRm9jdXM/OiBib29sZWFuID0gdHJ1ZTtcblxuICAvKipcbiAgICogV2hldGhlciB0aGUgZGlhbG9nIHNob3VsZCByZXN0b3JlIGZvY3VzIHRvIHRoZVxuICAgKiBwcmV2aW91c2x5LWZvY3VzZWQgZWxlbWVudCwgYWZ0ZXIgaXQncyBjbG9zZWQuXG4gICAqL1xuICByZXN0b3JlRm9jdXM/OiBib29sZWFuID0gdHJ1ZTtcblxuICAvKiogU2Nyb2xsIHN0cmF0ZWd5IHRvIGJlIHVzZWQgZm9yIHRoZSBkaWFsb2cuICovXG4gIHNjcm9sbFN0cmF0ZWd5PzogU2Nyb2xsU3RyYXRlZ3k7XG5cbiAgLyoqXG4gICAqIFdoZXRoZXIgdGhlIGRpYWxvZyBzaG91bGQgY2xvc2Ugd2hlbiB0aGUgdXNlciBnb2VzIGJhY2t3YXJkcy9mb3J3YXJkcyBpbiBoaXN0b3J5LlxuICAgKiBOb3RlIHRoYXQgdGhpcyB1c3VhbGx5IGRvZXNuJ3QgaW5jbHVkZSBjbGlja2luZyBvbiBsaW5rcyAodW5sZXNzIHRoZSB1c2VyIGlzIHVzaW5nXG4gICAqIHRoZSBgSGFzaExvY2F0aW9uU3RyYXRlZ3lgKS5cbiAgICovXG4gIGNsb3NlT25OYXZpZ2F0aW9uPzogYm9vbGVhbiA9IHRydWU7XG5cbiAgLyoqIEFsdGVybmF0ZSBgQ29tcG9uZW50RmFjdG9yeVJlc29sdmVyYCB0byB1c2Ugd2hlbiByZXNvbHZpbmcgdGhlIGFzc29jaWF0ZWQgY29tcG9uZW50LiAqL1xuICBjb21wb25lbnRGYWN0b3J5UmVzb2x2ZXI/OiBDb21wb25lbnRGYWN0b3J5UmVzb2x2ZXI7XG5cbiAgLy8gVE9ETyhqZWxib3Vybik6IGFkZCBjb25maWd1cmF0aW9uIGZvciBsaWZlY3ljbGUgaG9va3MsIEFSSUEgbGFiZWxsaW5nLlxufVxuIl19