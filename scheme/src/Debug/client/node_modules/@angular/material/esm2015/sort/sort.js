/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { coerceBooleanProperty } from '@angular/cdk/coercion';
import { Directive, EventEmitter, Input, isDevMode, Output, } from '@angular/core';
import { mixinDisabled, mixinInitialized, } from '@angular/material/core';
import { Subject } from 'rxjs';
import { getSortDuplicateSortableIdError, getSortHeaderMissingIdError, getSortInvalidDirectionError, } from './sort-errors';
// Boilerplate for applying mixins to MatSort.
/** @docs-private */
class MatSortBase {
}
const _MatSortMixinBase = mixinInitialized(mixinDisabled(MatSortBase));
/** Container for MatSortables to manage the sort state and provide default sort parameters. */
let MatSort = /** @class */ (() => {
    class MatSort extends _MatSortMixinBase {
        constructor() {
            super(...arguments);
            /** Collection of all registered sortables that this directive manages. */
            this.sortables = new Map();
            /** Used to notify any child components listening to state changes. */
            this._stateChanges = new Subject();
            /**
             * The direction to set when an MatSortable is initially sorted.
             * May be overriden by the MatSortable's sort start.
             */
            this.start = 'asc';
            this._direction = '';
            /** Event emitted when the user changes either the active sort or sort direction. */
            this.sortChange = new EventEmitter();
        }
        /** The sort direction of the currently active MatSortable. */
        get direction() { return this._direction; }
        set direction(direction) {
            if (isDevMode() && direction && direction !== 'asc' && direction !== 'desc') {
                throw getSortInvalidDirectionError(direction);
            }
            this._direction = direction;
        }
        /**
         * Whether to disable the user from clearing the sort by finishing the sort direction cycle.
         * May be overriden by the MatSortable's disable clear input.
         */
        get disableClear() { return this._disableClear; }
        set disableClear(v) { this._disableClear = coerceBooleanProperty(v); }
        /**
         * Register function to be used by the contained MatSortables. Adds the MatSortable to the
         * collection of MatSortables.
         */
        register(sortable) {
            if (!sortable.id) {
                throw getSortHeaderMissingIdError();
            }
            if (this.sortables.has(sortable.id)) {
                throw getSortDuplicateSortableIdError(sortable.id);
            }
            this.sortables.set(sortable.id, sortable);
        }
        /**
         * Unregister function to be used by the contained MatSortables. Removes the MatSortable from the
         * collection of contained MatSortables.
         */
        deregister(sortable) {
            this.sortables.delete(sortable.id);
        }
        /** Sets the active sort id and determines the new sort direction. */
        sort(sortable) {
            if (this.active != sortable.id) {
                this.active = sortable.id;
                this.direction = sortable.start ? sortable.start : this.start;
            }
            else {
                this.direction = this.getNextSortDirection(sortable);
            }
            this.sortChange.emit({ active: this.active, direction: this.direction });
        }
        /** Returns the next sort direction of the active sortable, checking for potential overrides. */
        getNextSortDirection(sortable) {
            if (!sortable) {
                return '';
            }
            // Get the sort direction cycle with the potential sortable overrides.
            const disableClear = sortable.disableClear != null ? sortable.disableClear : this.disableClear;
            let sortDirectionCycle = getSortDirectionCycle(sortable.start || this.start, disableClear);
            // Get and return the next direction in the cycle
            let nextDirectionIndex = sortDirectionCycle.indexOf(this.direction) + 1;
            if (nextDirectionIndex >= sortDirectionCycle.length) {
                nextDirectionIndex = 0;
            }
            return sortDirectionCycle[nextDirectionIndex];
        }
        ngOnInit() {
            this._markInitialized();
        }
        ngOnChanges() {
            this._stateChanges.next();
        }
        ngOnDestroy() {
            this._stateChanges.complete();
        }
    }
    MatSort.decorators = [
        { type: Directive, args: [{
                    selector: '[matSort]',
                    exportAs: 'matSort',
                    host: { 'class': 'mat-sort' },
                    inputs: ['disabled: matSortDisabled']
                },] }
    ];
    MatSort.propDecorators = {
        active: [{ type: Input, args: ['matSortActive',] }],
        start: [{ type: Input, args: ['matSortStart',] }],
        direction: [{ type: Input, args: ['matSortDirection',] }],
        disableClear: [{ type: Input, args: ['matSortDisableClear',] }],
        sortChange: [{ type: Output, args: ['matSortChange',] }]
    };
    return MatSort;
})();
export { MatSort };
/** Returns the sort direction cycle to use given the provided parameters of order and clear. */
function getSortDirectionCycle(start, disableClear) {
    let sortOrder = ['asc', 'desc'];
    if (start == 'desc') {
        sortOrder.reverse();
    }
    if (!disableClear) {
        sortOrder.push('');
    }
    return sortOrder;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic29ydC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9zb3J0L3NvcnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFlLHFCQUFxQixFQUFDLE1BQU0sdUJBQXVCLENBQUM7QUFDMUUsT0FBTyxFQUNMLFNBQVMsRUFDVCxZQUFZLEVBQ1osS0FBSyxFQUNMLFNBQVMsRUFJVCxNQUFNLEdBQ1AsTUFBTSxlQUFlLENBQUM7QUFDdkIsT0FBTyxFQUtMLGFBQWEsRUFDYixnQkFBZ0IsR0FDakIsTUFBTSx3QkFBd0IsQ0FBQztBQUNoQyxPQUFPLEVBQUMsT0FBTyxFQUFDLE1BQU0sTUFBTSxDQUFDO0FBRTdCLE9BQU8sRUFDTCwrQkFBK0IsRUFDL0IsMkJBQTJCLEVBQzNCLDRCQUE0QixHQUM3QixNQUFNLGVBQWUsQ0FBQztBQXVCdkIsOENBQThDO0FBQzlDLG9CQUFvQjtBQUNwQixNQUFNLFdBQVc7Q0FBRztBQUNwQixNQUFNLGlCQUFpQixHQUNuQixnQkFBZ0IsQ0FBQyxhQUFhLENBQUMsV0FBVyxDQUFDLENBQUMsQ0FBQztBQUVqRCwrRkFBK0Y7QUFDL0Y7SUFBQSxNQU1hLE9BQVEsU0FBUSxpQkFBaUI7UUFOOUM7O1lBUUUsMEVBQTBFO1lBQzFFLGNBQVMsR0FBRyxJQUFJLEdBQUcsRUFBdUIsQ0FBQztZQUUzQyxzRUFBc0U7WUFDN0Qsa0JBQWEsR0FBRyxJQUFJLE9BQU8sRUFBUSxDQUFDO1lBSzdDOzs7ZUFHRztZQUNvQixVQUFLLEdBQW1CLEtBQUssQ0FBQztZQVc3QyxlQUFVLEdBQWtCLEVBQUUsQ0FBQztZQVd2QyxvRkFBb0Y7WUFDbEQsZUFBVSxHQUF1QixJQUFJLFlBQVksRUFBUSxDQUFDO1FBaUU5RixDQUFDO1FBdEZDLDhEQUE4RDtRQUM5RCxJQUNJLFNBQVMsS0FBb0IsT0FBTyxJQUFJLENBQUMsVUFBVSxDQUFDLENBQUMsQ0FBQztR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QyxhQUFhLENBQUMsSUFBSSxFQUFFLENBQUM7UUFDNUIsQ0FBQztRQUVELFdBQVc7WUFDVCxJQUFJLENBQUMsYUFBYSxDQUFDLFFBQVEsRUFBRSxDQUFDO1FBQ2hDLENBQUM7OztnQkF6R0YsU0FBUyxTQUFDO29CQUNULFFBQVEsRUFBRSxXQUFXO29CQUNyQixRQUFRLEVBQUUsU0FBUztvQkFDbkIsSUFBSSxFQUFFLEVBQUMsT0FBTyxFQUFFLFVBQVUsRUFBQztvQkFDM0IsTUFBTSxFQUFFLENBQUMsMkJBQTJCLENBQUM7aUJBQ3RDOzs7eUJBVUUsS0FBSyxTQUFDLGVBQWU7d0JBTXJCLEtBQUssU0FBQyxjQUFjOzRCQUdwQixLQUFLLFNBQUMsa0JBQWtCOytCQWN4QixLQUFLLFNBQUMscUJBQXFCOzZCQU0zQixNQUFNLFNBQUMsZUFBZTs7SUFpRXpCLGNBQUM7S0FBQTtTQXZHWSxPQUFPO0FBeUdwQixnR0FBZ0c7QUFDaEcsU0FBUyxxQkFBcUIsQ0FBQyxLQUFxQixFQUNyQixZQUFxQjtJQUNsRCxJQUFJLFNBQVMsR0FBb0IsQ0FBQyxLQUFLLEVBQUUsTUFBTSxDQUFDLENBQUM7SUFDakQsSUFBSSxLQUFLLElBQUksTUFBTSxFQUFFO1FBQUUsU0FBUyxDQUFDLE9BQU8sRUFBRSxDQUFDO0tBQUU7SUFDN0MsSUFBSSxDQUFDLFlBQVksRUFBRTtRQUFFLFNBQVMsQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUFDLENBQUM7S0FBRTtJQUUxQyxPQUFPLFNBQVMsQ0FBQztBQUNuQixDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Qm9vbGVhbklucHV0LCBjb2VyY2VCb29sZWFuUHJvcGVydHl9IGZyb20gJ0Bhbmd1bGFyL2Nkay9jb2VyY2lvbic7XG5pbXBvcnQge1xuICBEaXJlY3RpdmUsXG4gIEV2ZW50RW1pdHRlcixcbiAgSW5wdXQsXG4gIGlzRGV2TW9kZSxcbiAgT25DaGFuZ2VzLFxuICBPbkRlc3Ryb3ksXG4gIE9uSW5pdCxcbiAgT3V0cHV0LFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7XG4gIENhbkRpc2FibGUsXG4gIENhbkRpc2FibGVDdG9yLFxuICBIYXNJbml0aWFsaXplZCxcbiAgSGFzSW5pdGlhbGl6ZWRDdG9yLFxuICBtaXhpbkRpc2FibGVkLFxuICBtaXhpbkluaXRpYWxpemVkLFxufSBmcm9tICdAYW5ndWxhci9tYXRlcmlhbC9jb3JlJztcbmltcG9ydCB7U3ViamVjdH0gZnJvbSAncnhqcyc7XG5pbXBvcnQge1NvcnREaXJlY3Rpb259IGZyb20gJy4vc29ydC1kaXJlY3Rpb24nO1xuaW1wb3J0IHtcbiAgZ2V0U29ydER1cGxpY2F0ZVNvcnRhYmxlSWRFcnJvcixcbiAgZ2V0U29ydEhlYWRlck1pc3NpbmdJZEVycm9yLFxuICBnZXRTb3J0SW52YWxpZERpcmVjdGlvbkVycm9yLFxufSBmcm9tICcuL3NvcnQtZXJyb3JzJztcblxuLyoqIEludGVyZmFjZSBmb3IgYSBkaXJlY3RpdmUgdGhhdCBob2xkcyBzb3J0aW5nIHN0YXRlIGNvbnN1bWVkIGJ5IGBNYXRTb3J0SGVhZGVyYC4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgTWF0U29ydGFibGUge1xuICAvKiogVGhlIGlkIG9mIHRoZSBjb2x1bW4gYmVpbmcgc29ydGVkLiAqL1xuICBpZDogc3RyaW5nO1xuXG4gIC8qKiBTdGFydGluZyBzb3J0IGRpcmVjdGlvbi4gKi9cbiAgc3RhcnQ6ICdhc2MnIHwgJ2Rlc2MnO1xuXG4gIC8qKiBXaGV0aGVyIHRvIGRpc2FibGUgY2xlYXJpbmcgdGhlIHNvcnRpbmcgc3RhdGUuICovXG4gIGRpc2FibGVDbGVhcjogYm9vbGVhbjtcbn1cblxuLyoqIFRoZSBjdXJyZW50IHNvcnQgc3RhdGUuICovXG5leHBvcnQgaW50ZXJmYWNlIFNvcnQge1xuICAvKiogVGhlIGlkIG9mIHRoZSBjb2x1bW4gYmVpbmcgc29ydGVkLiAqL1xuICBhY3RpdmU6IHN0cmluZztcblxuICAvKiogVGhlIHNvcnQgZGlyZWN0aW9uLiAqL1xuICBkaXJlY3Rpb246IFNvcnREaXJlY3Rpb247XG59XG5cbi8vIEJvaWxlcnBsYXRlIGZvciBhcHBseWluZyBtaXhpbnMgdG8gTWF0U29ydC5cbi8qKiBAZG9jcy1wcml2YXRlICovXG5jbGFzcyBNYXRTb3J0QmFzZSB7fVxuY29uc3QgX01hdFNvcnRNaXhpbkJhc2U6IEhhc0luaXRpYWxpemVkQ3RvciAmIENhbkRpc2FibGVDdG9yICYgdHlwZW9mIE1hdFNvcnRCYXNlID1cbiAgICBtaXhpbkluaXRpYWxpemVkKG1peGluRGlzYWJsZWQoTWF0U29ydEJhc2UpKTtcblxuLyoqIENvbnRhaW5lciBmb3IgTWF0U29ydGFibGVzIHRvIG1hbmFnZSB0aGUgc29ydCBzdGF0ZSBhbmQgcHJvdmlkZSBkZWZhdWx0IHNvcnQgcGFyYW1ldGVycy4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogJ1ttYXRTb3J0XScsXG4gIGV4cG9ydEFzOiAnbWF0U29ydCcsXG4gIGhvc3Q6IHsnY2xhc3MnOiAnbWF0LXNvcnQnfSxcbiAgaW5wdXRzOiBbJ2Rpc2FibGVkOiBtYXRTb3J0RGlzYWJsZWQnXVxufSlcbmV4cG9ydCBjbGFzcyBNYXRTb3J0IGV4dGVuZHMgX01hdFNvcnRNaXhpbkJhc2VcbiAgICBpbXBsZW1lbnRzIENhbkRpc2FibGUsIEhhc0luaXRpYWxpemVkLCBPbkNoYW5nZXMsIE9uRGVzdHJveSwgT25Jbml0IHtcbiAgLyoqIENvbGxlY3Rpb24gb2YgYWxsIHJlZ2lzdGVyZWQgc29ydGFibGVzIHRoYXQgdGhpcyBkaXJlY3RpdmUgbWFuYWdlcy4gKi9cbiAgc29ydGFibGVzID0gbmV3IE1hcDxzdHJpbmcsIE1hdFNvcnRhYmxlPigpO1xuXG4gIC8qKiBVc2VkIHRvIG5vdGlmeSBhbnkgY2hpbGQgY29tcG9uZW50cyBsaXN0ZW5pbmcgdG8gc3RhdGUgY2hhbmdlcy4gKi9cbiAgcmVhZG9ubHkgX3N0YXRlQ2hhbmdlcyA9IG5ldyBTdWJqZWN0PHZvaWQ+KCk7XG5cbiAgLyoqIFRoZSBpZCBvZiB0aGUgbW9zdCByZWNlbnRseSBzb3J0ZWQgTWF0U29ydGFibGUuICovXG4gIEBJbnB1dCgnbWF0U29ydEFjdGl2ZScpIGFjdGl2ZTogc3RyaW5nO1xuXG4gIC8qKlxuICAgKiBUaGUgZGlyZWN0aW9uIHRvIHNldCB3aGVuIGFuIE1hdFNvcnRhYmxlIGlzIGluaXRpYWxseSBzb3J0ZWQuXG4gICAqIE1heSBiZSBvdmVycmlkZW4gYnkgdGhlIE1hdFNvcnRhYmxlJ3Mgc29ydCBzdGFydC5cbiAgICovXG4gIEBJbnB1dCgnbWF0U29ydFN0YXJ0Jykgc3RhcnQ6ICdhc2MnIHwgJ2Rlc2MnID0gJ2FzYyc7XG5cbiAgLyoqIFRoZSBzb3J0IGRpcmVjdGlvbiBvZiB0aGUgY3VycmVudGx5IGFjdGl2ZSBNYXRTb3J0YWJsZS4gKi9cbiAgQElucHV0KCdtYXRTb3J0RGlyZWN0aW9uJylcbiAgZ2V0IGRpcmVjdGlvbigpOiBTb3J0RGlyZWN0aW9uIHsgcmV0dXJuIHRoaXMuX2RpcmVjdGlvbjsgfVxuICBzZXQgZGlyZWN0aW9uKGRpcmVjdGlvbjogU29ydERpcmVjdGlvbikge1xuICAgIGlmIChpc0Rldk1vZGUoKSAmJiBkaXJlY3Rpb24gJiYgZGlyZWN0aW9uICE9PSAnYXNjJyAmJiBkaXJlY3Rpb24gIT09ICdkZXNjJykge1xuICAgICAgdGhyb3cgZ2V0U29ydEludmFsaWREaXJlY3Rpb25FcnJvcihkaXJlY3Rpb24pO1xuICAgIH1cbiAgICB0aGlzLl9kaXJlY3Rpb24gPSBkaXJlY3Rpb247XG4gIH1cbiAgcHJpdmF0ZSBfZGlyZWN0aW9uOiBTb3J0RGlyZWN0aW9uID0gJyc7XG5cbiAgLyoqXG4gICAqIFdoZXRoZXIgdG8gZGlzYWJsZSB0aGUgdXNlciBmcm9tIGNsZWFyaW5nIHRoZSBzb3J0IGJ5IGZpbmlzaGluZyB0aGUgc29ydCBkaXJlY3Rpb24gY3ljbGUuXG4gICAqIE1heSBiZSBvdmVycmlkZW4gYnkgdGhlIE1hdFNvcnRhYmxlJ3MgZGlzYWJsZSBjbGVhciBpbnB1dC5cbiAgICovXG4gIEBJbnB1dCgnbWF0U29ydERpc2FibGVDbGVhcicpXG4gIGdldCBkaXNhYmxlQ2xlYXIoKTogYm9vbGVhbiB7IHJldHVybiB0aGlzLl9kaXNhYmxlQ2xlYXI7IH1cbiAgc2V0IGRpc2FibGVDbGVhcih2OiBib29sZWFuKSB7IHRoaXMuX2Rpc2FibGVDbGVhciA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh2KTsgfVxuICBwcml2YXRlIF9kaXNhYmxlQ2xlYXI6IGJvb2xlYW47XG5cbiAgLyoqIEV2ZW50IGVtaXR0ZWQgd2hlbiB0aGUgdXNlciBjaGFuZ2VzIGVpdGhlciB0aGUgYWN0aXZlIHNvcnQgb3Igc29ydCBkaXJlY3Rpb24uICovXG4gIEBPdXRwdXQoJ21hdFNvcnRDaGFuZ2UnKSByZWFkb25seSBzb3J0Q2hhbmdlOiBFdmVudEVtaXR0ZXI8U29ydD4gPSBuZXcgRXZlbnRFbWl0dGVyPFNvcnQ+KCk7XG5cbiAgLyoqXG4gICAqIFJlZ2lzdGVyIGZ1bmN0aW9uIHRvIGJlIHVzZWQgYnkgdGhlIGNvbnRhaW5lZCBNYXRTb3J0YWJsZXMuIEFkZHMgdGhlIE1hdFNvcnRhYmxlIHRvIHRoZVxuICAgKiBjb2xsZWN0aW9uIG9mIE1hdFNvcnRhYmxlcy5cbiAgICovXG4gIHJlZ2lzdGVyKHNvcnRhYmxlOiBNYXRTb3J0YWJsZSk6IHZvaWQge1xuICAgIGlmICghc29ydGFibGUuaWQpIHtcbiAgICAgIHRocm93IGdldFNvcnRIZWFkZXJNaXNzaW5nSWRFcnJvcigpO1xuICAgIH1cblxuICAgIGlmICh0aGlzLnNvcnRhYmxlcy5oYXMoc29ydGFibGUuaWQpKSB7XG4gICAgICB0aHJvdyBnZXRTb3J0RHVwbGljYXRlU29ydGFibGVJZEVycm9yKHNvcnRhYmxlLmlkKTtcbiAgICB9XG4gICAgdGhpcy5zb3J0YWJsZXMuc2V0KHNvcnRhYmxlLmlkLCBzb3J0YWJsZSk7XG4gIH1cblxuICAvKipcbiAgICogVW5yZWdpc3RlciBmdW5jdGlvbiB0byBiZSB1c2VkIGJ5IHRoZSBjb250YWluZWQgTWF0U29ydGFibGVzLiBSZW1vdmVzIHRoZSBNYXRTb3J0YWJsZSBmcm9tIHRoZVxuICAgKiBjb2xsZWN0aW9uIG9mIGNvbnRhaW5lZCBNYXRTb3J0YWJsZXMuXG4gICAqL1xuICBkZXJlZ2lzdGVyKHNvcnRhYmxlOiBNYXRTb3J0YWJsZSk6IHZvaWQge1xuICAgIHRoaXMuc29ydGFibGVzLmRlbGV0ZShzb3J0YWJsZS5pZCk7XG4gIH1cblxuICAvKiogU2V0cyB0aGUgYWN0aXZlIHNvcnQgaWQgYW5kIGRldGVybWluZXMgdGhlIG5ldyBzb3J0IGRpcmVjdGlvbi4gKi9cbiAgc29ydChzb3J0YWJsZTogTWF0U29ydGFibGUpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5hY3RpdmUgIT0gc29ydGFibGUuaWQpIHtcbiAgICAgIHRoaXMuYWN0aXZlID0gc29ydGFibGUuaWQ7XG4gICAgICB0aGlzLmRpcmVjdGlvbiA9IHNvcnRhYmxlLnN0YXJ0ID8gc29ydGFibGUuc3RhcnQgOiB0aGlzLnN0YXJ0O1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLmRpcmVjdGlvbiA9IHRoaXMuZ2V0TmV4dFNvcnREaXJlY3Rpb24oc29ydGFibGUpO1xuICAgIH1cblxuICAgIHRoaXMuc29ydENoYW5nZS5lbWl0KHthY3RpdmU6IHRoaXMuYWN0aXZlLCBkaXJlY3Rpb246IHRoaXMuZGlyZWN0aW9ufSk7XG4gIH1cblxuICAvKiogUmV0dXJucyB0aGUgbmV4dCBzb3J0IGRpcmVjdGlvbiBvZiB0aGUgYWN0aXZlIHNvcnRhYmxlLCBjaGVja2luZyBmb3IgcG90ZW50aWFsIG92ZXJyaWRlcy4gKi9cbiAgZ2V0TmV4dFNvcnREaXJlY3Rpb24oc29ydGFibGU6IE1hdFNvcnRhYmxlKTogU29ydERpcmVjdGlvbiB7XG4gICAgaWYgKCFzb3J0YWJsZSkgeyByZXR1cm4gJyc7IH1cblxuICAgIC8vIEdldCB0aGUgc29ydCBkaXJlY3Rpb24gY3ljbGUgd2l0aCB0aGUgcG90ZW50aWFsIHNvcnRhYmxlIG92ZXJyaWRlcy5cbiAgICBjb25zdCBkaXNhYmxlQ2xlYXIgPSBzb3J0YWJsZS5kaXNhYmxlQ2xlYXIgIT0gbnVsbCA/IHNvcnRhYmxlLmRpc2FibGVDbGVhciA6IHRoaXMuZGlzYWJsZUNsZWFyO1xuICAgIGxldCBzb3J0RGlyZWN0aW9uQ3ljbGUgPSBnZXRTb3J0RGlyZWN0aW9uQ3ljbGUoc29ydGFibGUuc3RhcnQgfHwgdGhpcy5zdGFydCwgZGlzYWJsZUNsZWFyKTtcblxuICAgIC8vIEdldCBhbmQgcmV0dXJuIHRoZSBuZXh0IGRpcmVjdGlvbiBpbiB0aGUgY3ljbGVcbiAgICBsZXQgbmV4dERpcmVjdGlvbkluZGV4ID0gc29ydERpcmVjdGlvbkN5Y2xlLmluZGV4T2YodGhpcy5kaXJlY3Rpb24pICsgMTtcbiAgICBpZiAobmV4dERpcmVjdGlvbkluZGV4ID49IHNvcnREaXJlY3Rpb25DeWNsZS5sZW5ndGgpIHsgbmV4dERpcmVjdGlvbkluZGV4ID0gMDsgfVxuICAgIHJldHVybiBzb3J0RGlyZWN0aW9uQ3ljbGVbbmV4dERpcmVjdGlvbkluZGV4XTtcbiAgfVxuXG4gIG5nT25Jbml0KCkge1xuICAgIHRoaXMuX21hcmtJbml0aWFsaXplZCgpO1xuICB9XG5cbiAgbmdPbkNoYW5nZXMoKSB7XG4gICAgdGhpcy5fc3RhdGVDaGFuZ2VzLm5leHQoKTtcbiAgfVxuXG4gIG5nT25EZXN0cm95KCkge1xuICAgIHRoaXMuX3N0YXRlQ2hhbmdlcy5jb21wbGV0ZSgpO1xuICB9XG5cbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX2Rpc2FibGVDbGVhcjogQm9vbGVhbklucHV0O1xuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfZGlzYWJsZWQ6IEJvb2xlYW5JbnB1dDtcbn1cblxuLyoqIFJldHVybnMgdGhlIHNvcnQgZGlyZWN0aW9uIGN5Y2xlIHRvIHVzZSBnaXZlbiB0aGUgcHJvdmlkZWQgcGFyYW1ldGVycyBvZiBvcmRlciBhbmQgY2xlYXIuICovXG5mdW5jdGlvbiBnZXRTb3J0RGlyZWN0aW9uQ3ljbGUoc3RhcnQ6ICdhc2MnIHwgJ2Rlc2MnLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGRpc2FibGVDbGVhcjogYm9vbGVhbik6IFNvcnREaXJlY3Rpb25bXSB7XG4gIGxldCBzb3J0T3JkZXI6IFNvcnREaXJlY3Rpb25bXSA9IFsnYXNjJywgJ2Rlc2MnXTtcbiAgaWYgKHN0YXJ0ID09ICdkZXNjJykgeyBzb3J0T3JkZXIucmV2ZXJzZSgpOyB9XG4gIGlmICghZGlzYWJsZUNsZWFyKSB7IHNvcnRPcmRlci5wdXNoKCcnKTsgfVxuXG4gIHJldHVybiBzb3J0T3JkZXI7XG59XG4iXX0=