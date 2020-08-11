/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/**
 * Throws an exception for the case when menu trigger doesn't have a valid mat-menu instance
 * @docs-private
 */
export function throwMatMenuMissingError() {
    throw Error(`matMenuTriggerFor: must pass in an mat-menu instance.

    Example:
      <mat-menu #menu="matMenu"></mat-menu>
      <button [matMenuTriggerFor]="menu"></button>`);
}
/**
 * Throws an exception for the case when menu's x-position value isn't valid.
 * In other words, it doesn't match 'before' or 'after'.
 * @docs-private
 */
export function throwMatMenuInvalidPositionX() {
    throw Error(`xPosition value must be either 'before' or after'.
      Example: <mat-menu xPosition="before" #menu="matMenu"></mat-menu>`);
}
/**
 * Throws an exception for the case when menu's y-position value isn't valid.
 * In other words, it doesn't match 'above' or 'below'.
 * @docs-private
 */
export function throwMatMenuInvalidPositionY() {
    throw Error(`yPosition value must be either 'above' or below'.
      Example: <mat-menu yPosition="above" #menu="matMenu"></mat-menu>`);
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibWVudS1lcnJvcnMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvbWVudS9tZW51LWVycm9ycy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSDs7O0dBR0c7QUFDSCxNQUFNLFVBQVUsd0JBQXdCO0lBQ3RDLE1BQU0sS0FBSyxDQUFDOzs7O21EQUlxQyxDQUFDLENBQUM7QUFDckQsQ0FBQztBQUVEOzs7O0dBSUc7QUFDSCxNQUFNLFVBQVUsNEJBQTRCO0lBQzFDLE1BQU0sS0FBSyxDQUFDO3dFQUMwRCxDQUFDLENBQUM7QUFDMUUsQ0FBQztBQUVEOzs7O0dBSUc7QUFDSCxNQUFNLFVBQVUsNEJBQTRCO0lBQzFDLE1BQU0sS0FBSyxDQUFDO3VFQUN5RCxDQUFDLENBQUM7QUFDekUsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG4vKipcbiAqIFRocm93cyBhbiBleGNlcHRpb24gZm9yIHRoZSBjYXNlIHdoZW4gbWVudSB0cmlnZ2VyIGRvZXNuJ3QgaGF2ZSBhIHZhbGlkIG1hdC1tZW51IGluc3RhbmNlXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiB0aHJvd01hdE1lbnVNaXNzaW5nRXJyb3IoKSB7XG4gIHRocm93IEVycm9yKGBtYXRNZW51VHJpZ2dlckZvcjogbXVzdCBwYXNzIGluIGFuIG1hdC1tZW51IGluc3RhbmNlLlxuXG4gICAgRXhhbXBsZTpcbiAgICAgIDxtYXQtbWVudSAjbWVudT1cIm1hdE1lbnVcIj48L21hdC1tZW51PlxuICAgICAgPGJ1dHRvbiBbbWF0TWVudVRyaWdnZXJGb3JdPVwibWVudVwiPjwvYnV0dG9uPmApO1xufVxuXG4vKipcbiAqIFRocm93cyBhbiBleGNlcHRpb24gZm9yIHRoZSBjYXNlIHdoZW4gbWVudSdzIHgtcG9zaXRpb24gdmFsdWUgaXNuJ3QgdmFsaWQuXG4gKiBJbiBvdGhlciB3b3JkcywgaXQgZG9lc24ndCBtYXRjaCAnYmVmb3JlJyBvciAnYWZ0ZXInLlxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5leHBvcnQgZnVuY3Rpb24gdGhyb3dNYXRNZW51SW52YWxpZFBvc2l0aW9uWCgpIHtcbiAgdGhyb3cgRXJyb3IoYHhQb3NpdGlvbiB2YWx1ZSBtdXN0IGJlIGVpdGhlciAnYmVmb3JlJyBvciBhZnRlcicuXG4gICAgICBFeGFtcGxlOiA8bWF0LW1lbnUgeFBvc2l0aW9uPVwiYmVmb3JlXCIgI21lbnU9XCJtYXRNZW51XCI+PC9tYXQtbWVudT5gKTtcbn1cblxuLyoqXG4gKiBUaHJvd3MgYW4gZXhjZXB0aW9uIGZvciB0aGUgY2FzZSB3aGVuIG1lbnUncyB5LXBvc2l0aW9uIHZhbHVlIGlzbid0IHZhbGlkLlxuICogSW4gb3RoZXIgd29yZHMsIGl0IGRvZXNuJ3QgbWF0Y2ggJ2Fib3ZlJyBvciAnYmVsb3cnLlxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5leHBvcnQgZnVuY3Rpb24gdGhyb3dNYXRNZW51SW52YWxpZFBvc2l0aW9uWSgpIHtcbiAgdGhyb3cgRXJyb3IoYHlQb3NpdGlvbiB2YWx1ZSBtdXN0IGJlIGVpdGhlciAnYWJvdmUnIG9yIGJlbG93Jy5cbiAgICAgIEV4YW1wbGU6IDxtYXQtbWVudSB5UG9zaXRpb249XCJhYm92ZVwiICNtZW51PVwibWF0TWVudVwiPjwvbWF0LW1lbnU+YCk7XG59XG4iXX0=