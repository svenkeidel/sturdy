/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Directive, ElementRef, Inject, Optional, Input } from '@angular/core';
import { coerceBooleanProperty } from '@angular/cdk/coercion';
import { Subject } from 'rxjs';
import { CDK_DRAG_PARENT } from '../drag-parent';
import { toggleNativeDragInteractions } from '../drag-styling';
/** Handle that can be used to drag and CdkDrag instance. */
let CdkDragHandle = /** @class */ (() => {
    class CdkDragHandle {
        constructor(element, parentDrag) {
            this.element = element;
            /** Emits when the state of the handle has changed. */
            this._stateChanges = new Subject();
            this._disabled = false;
            this._parentDrag = parentDrag;
            toggleNativeDragInteractions(element.nativeElement, false);
        }
        /** Whether starting to drag through this handle is disabled. */
        get disabled() { return this._disabled; }
        set disabled(value) {
            this._disabled = coerceBooleanProperty(value);
            this._stateChanges.next(this);
        }
        ngOnDestroy() {
            this._stateChanges.complete();
        }
    }
    CdkDragHandle.decorators = [
        { type: Directive, args: [{
                    selector: '[cdkDragHandle]',
                    host: {
                        'class': 'cdk-drag-handle'
                    }
                },] }
    ];
    CdkDragHandle.ctorParameters = () => [
        { type: ElementRef },
        { type: undefined, decorators: [{ type: Inject, args: [CDK_DRAG_PARENT,] }, { type: Optional }] }
    ];
    CdkDragHandle.propDecorators = {
        disabled: [{ type: Input, args: ['cdkDragHandleDisabled',] }]
    };
    return CdkDragHandle;
})();
export { CdkDragHandle };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZHJhZy1oYW5kbGUuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL2RyYWctZHJvcC9kaXJlY3RpdmVzL2RyYWctaGFuZGxlLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU8sRUFBQyxTQUFTLEVBQUUsVUFBVSxFQUFFLE1BQU0sRUFBRSxRQUFRLEVBQUUsS0FBSyxFQUFZLE1BQU0sZUFBZSxDQUFDO0FBQ3hGLE9BQU8sRUFBZSxxQkFBcUIsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQzFFLE9BQU8sRUFBQyxPQUFPLEVBQUMsTUFBTSxNQUFNLENBQUM7QUFDN0IsT0FBTyxFQUFDLGVBQWUsRUFBQyxNQUFNLGdCQUFnQixDQUFDO0FBQy9DLE9BQU8sRUFBQyw0QkFBNEIsRUFBQyxNQUFNLGlCQUFpQixDQUFDO0FBRTdELDREQUE0RDtBQUM1RDtJQUFBLE1BTWEsYUFBYTtRQWdCeEIsWUFDUyxPQUFnQyxFQUNGLFVBQWdCO1lBRDlDLFlBQU8sR0FBUCxPQUFPLENBQXlCO1lBYnpDLHNEQUFzRDtZQUN0RCxrQkFBYSxHQUFHLElBQUksT0FBTyxFQUFpQixDQUFDO1lBU3JDLGNBQVMsR0FBRyxLQUFLLENBQUM7WUFNeEIsSUFBSSxDQUFDLFdBQVcsR0FBRyxVQUFVLENBQUM7WUFDOUIsNEJBQTRCLENBQUMsT0FBTyxDQUFDLGFBQWEsRUFBRSxLQUFLLENBQUMsQ0FBQztRQUM3RCxDQUFDO1FBZkQsZ0VBQWdFO1FBQ2hFLElBQ0ksUUFBUSxLQUFjLE9BQU8sSUFBSSxDQUFDLFNBQVMsQ0FBQyxDQUFDLENBQUM7UUFDbEQsSUFBSSxRQUFRLENBQUMsS0FBYztZQUN6QixJQUFJLENBQUMsU0FBUyxHQUFHLHFCQUFxQixDQUFDLEtBQUssQ0FBQyxDQUFDO1lBQzlDLElBQUksQ0FBQyxhQUFhLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO1FBQ2hDLENBQUM7UUFXRCxXQUFXO1lBQ1QsSUFBSSxDQUFDLGFBQWEsQ0FBQyxRQUFRLEVBQUUsQ0FBQztRQUNoQyxDQUFDOzs7Z0JBaENGLFNBQVMsU0FBQztvQkFDVCxRQUFRLEVBQUUsaUJBQWlCO29CQUMzQixJQUFJLEVBQUU7d0JBQ0osT0FBTyxFQUFFLGlCQUFpQjtxQkFDM0I7aUJBQ0Y7OztnQkFaa0IsVUFBVTtnREErQnhCLE1BQU0sU0FBQyxlQUFlLGNBQUcsUUFBUTs7OzJCQVZuQyxLQUFLLFNBQUMsdUJBQXVCOztJQXFCaEMsb0JBQUM7S0FBQTtTQTdCWSxhQUFhIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7RGlyZWN0aXZlLCBFbGVtZW50UmVmLCBJbmplY3QsIE9wdGlvbmFsLCBJbnB1dCwgT25EZXN0cm95fSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7Qm9vbGVhbklucHV0LCBjb2VyY2VCb29sZWFuUHJvcGVydHl9IGZyb20gJ0Bhbmd1bGFyL2Nkay9jb2VyY2lvbic7XG5pbXBvcnQge1N1YmplY3R9IGZyb20gJ3J4anMnO1xuaW1wb3J0IHtDREtfRFJBR19QQVJFTlR9IGZyb20gJy4uL2RyYWctcGFyZW50JztcbmltcG9ydCB7dG9nZ2xlTmF0aXZlRHJhZ0ludGVyYWN0aW9uc30gZnJvbSAnLi4vZHJhZy1zdHlsaW5nJztcblxuLyoqIEhhbmRsZSB0aGF0IGNhbiBiZSB1c2VkIHRvIGRyYWcgYW5kIENka0RyYWcgaW5zdGFuY2UuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICdbY2RrRHJhZ0hhbmRsZV0nLFxuICBob3N0OiB7XG4gICAgJ2NsYXNzJzogJ2Nkay1kcmFnLWhhbmRsZSdcbiAgfVxufSlcbmV4cG9ydCBjbGFzcyBDZGtEcmFnSGFuZGxlIGltcGxlbWVudHMgT25EZXN0cm95IHtcbiAgLyoqIENsb3Nlc3QgcGFyZW50IGRyYWdnYWJsZSBpbnN0YW5jZS4gKi9cbiAgX3BhcmVudERyYWc6IHt9IHwgdW5kZWZpbmVkO1xuXG4gIC8qKiBFbWl0cyB3aGVuIHRoZSBzdGF0ZSBvZiB0aGUgaGFuZGxlIGhhcyBjaGFuZ2VkLiAqL1xuICBfc3RhdGVDaGFuZ2VzID0gbmV3IFN1YmplY3Q8Q2RrRHJhZ0hhbmRsZT4oKTtcblxuICAvKiogV2hldGhlciBzdGFydGluZyB0byBkcmFnIHRocm91Z2ggdGhpcyBoYW5kbGUgaXMgZGlzYWJsZWQuICovXG4gIEBJbnB1dCgnY2RrRHJhZ0hhbmRsZURpc2FibGVkJylcbiAgZ2V0IGRpc2FibGVkKCk6IGJvb2xlYW4geyByZXR1cm4gdGhpcy5fZGlzYWJsZWQ7IH1cbiAgc2V0IGRpc2FibGVkKHZhbHVlOiBib29sZWFuKSB7XG4gICAgdGhpcy5fZGlzYWJsZWQgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkodmFsdWUpO1xuICAgIHRoaXMuX3N0YXRlQ2hhbmdlcy5uZXh0KHRoaXMpO1xuICB9XG4gIHByaXZhdGUgX2Rpc2FibGVkID0gZmFsc2U7XG5cbiAgY29uc3RydWN0b3IoXG4gICAgcHVibGljIGVsZW1lbnQ6IEVsZW1lbnRSZWY8SFRNTEVsZW1lbnQ+LFxuICAgIEBJbmplY3QoQ0RLX0RSQUdfUEFSRU5UKSBAT3B0aW9uYWwoKSBwYXJlbnREcmFnPzogYW55KSB7XG5cbiAgICB0aGlzLl9wYXJlbnREcmFnID0gcGFyZW50RHJhZztcbiAgICB0b2dnbGVOYXRpdmVEcmFnSW50ZXJhY3Rpb25zKGVsZW1lbnQubmF0aXZlRWxlbWVudCwgZmFsc2UpO1xuICB9XG5cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgdGhpcy5fc3RhdGVDaGFuZ2VzLmNvbXBsZXRlKCk7XG4gIH1cblxuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfZGlzYWJsZWQ6IEJvb2xlYW5JbnB1dDtcbn1cbiJdfQ==