/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Platform, normalizePassiveListenerOptions } from '@angular/cdk/platform';
import { Directive, ElementRef, EventEmitter, Injectable, NgZone, Output, } from '@angular/core';
import { coerceElement } from '@angular/cdk/coercion';
import { EMPTY, Subject } from 'rxjs';
import * as i0 from "@angular/core";
import * as i1 from "@angular/cdk/platform";
/** Options to pass to the animationstart listener. */
const listenerOptions = normalizePassiveListenerOptions({ passive: true });
/**
 * An injectable service that can be used to monitor the autofill state of an input.
 * Based on the following blog post:
 * https://medium.com/@brunn/detecting-autofilled-fields-in-javascript-aed598d25da7
 */
let AutofillMonitor = /** @class */ (() => {
    class AutofillMonitor {
        constructor(_platform, _ngZone) {
            this._platform = _platform;
            this._ngZone = _ngZone;
            this._monitoredElements = new Map();
        }
        monitor(elementOrRef) {
            if (!this._platform.isBrowser) {
                return EMPTY;
            }
            const element = coerceElement(elementOrRef);
            const info = this._monitoredElements.get(element);
            if (info) {
                return info.subject.asObservable();
            }
            const result = new Subject();
            const cssClass = 'cdk-text-field-autofilled';
            const listener = ((event) => {
                // Animation events fire on initial element render, we check for the presence of the autofill
                // CSS class to make sure this is a real change in state, not just the initial render before
                // we fire off events.
                if (event.animationName === 'cdk-text-field-autofill-start' &&
                    !element.classList.contains(cssClass)) {
                    element.classList.add(cssClass);
                    this._ngZone.run(() => result.next({ target: event.target, isAutofilled: true }));
                }
                else if (event.animationName === 'cdk-text-field-autofill-end' &&
                    element.classList.contains(cssClass)) {
                    element.classList.remove(cssClass);
                    this._ngZone.run(() => result.next({ target: event.target, isAutofilled: false }));
                }
            });
            this._ngZone.runOutsideAngular(() => {
                element.addEventListener('animationstart', listener, listenerOptions);
                element.classList.add('cdk-text-field-autofill-monitored');
            });
            this._monitoredElements.set(element, {
                subject: result,
                unlisten: () => {
                    element.removeEventListener('animationstart', listener, listenerOptions);
                }
            });
            return result.asObservable();
        }
        stopMonitoring(elementOrRef) {
            const element = coerceElement(elementOrRef);
            const info = this._monitoredElements.get(element);
            if (info) {
                info.unlisten();
                info.subject.complete();
                element.classList.remove('cdk-text-field-autofill-monitored');
                element.classList.remove('cdk-text-field-autofilled');
                this._monitoredElements.delete(element);
            }
        }
        ngOnDestroy() {
            this._monitoredElements.forEach((_info, element) => this.stopMonitoring(element));
        }
    }
    AutofillMonitor.ɵprov = i0.ɵɵdefineInjectable({ factory: function AutofillMonitor_Factory() { return new AutofillMonitor(i0.ɵɵinject(i1.Platform), i0.ɵɵinject(i0.NgZone)); }, token: AutofillMonitor, providedIn: "root" });
    AutofillMonitor.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    AutofillMonitor.ctorParameters = () => [
        { type: Platform },
        { type: NgZone }
    ];
    return AutofillMonitor;
})();
export { AutofillMonitor };
/** A directive that can be used to monitor the autofill state of an input. */
let CdkAutofill = /** @class */ (() => {
    class CdkAutofill {
        constructor(_elementRef, _autofillMonitor) {
            this._elementRef = _elementRef;
            this._autofillMonitor = _autofillMonitor;
            /** Emits when the autofill state of the element changes. */
            this.cdkAutofill = new EventEmitter();
        }
        ngOnInit() {
            this._autofillMonitor
                .monitor(this._elementRef)
                .subscribe(event => this.cdkAutofill.emit(event));
        }
        ngOnDestroy() {
            this._autofillMonitor.stopMonitoring(this._elementRef);
        }
    }
    CdkAutofill.decorators = [
        { type: Directive, args: [{
                    selector: '[cdkAutofill]',
                },] }
    ];
    CdkAutofill.ctorParameters = () => [
        { type: ElementRef },
        { type: AutofillMonitor }
    ];
    CdkAutofill.propDecorators = {
        cdkAutofill: [{ type: Output }]
    };
    return CdkAutofill;
})();
export { CdkAutofill };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYXV0b2ZpbGwuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL3RleHQtZmllbGQvYXV0b2ZpbGwudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFFBQVEsRUFBRSwrQkFBK0IsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQ2hGLE9BQU8sRUFDTCxTQUFTLEVBQ1QsVUFBVSxFQUNWLFlBQVksRUFDWixVQUFVLEVBQ1YsTUFBTSxFQUdOLE1BQU0sR0FDUCxNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUMsYUFBYSxFQUFDLE1BQU0sdUJBQXVCLENBQUM7QUFDcEQsT0FBTyxFQUFDLEtBQUssRUFBYyxPQUFPLEVBQUMsTUFBTSxNQUFNLENBQUM7OztBQW1CaEQsc0RBQXNEO0FBQ3RELE1BQU0sZUFBZSxHQUFHLCtCQUErQixDQUFDLEVBQUMsT0FBTyxFQUFFLElBQUksRUFBQyxDQUFDLENBQUM7QUFHekU7Ozs7R0FJRztBQUNIO0lBQUEsTUFDYSxlQUFlO1FBRzFCLFlBQW9CLFNBQW1CLEVBQVUsT0FBZTtZQUE1QyxjQUFTLEdBQVQsU0FBUyxDQUFVO1lBQVUsWUFBTyxHQUFQLE9BQU8sQ0FBUTtZQUZ4RCx1QkFBa0IsR0FBRyxJQUFJLEdBQUcsRUFBaUMsQ0FBQztRQUVILENBQUM7UUFnQnBFLE9BQU8sQ0FBQyxZQUEyQztZQUNqRCxJQUFJLENBQUMsSUFBSSxDQUFDLFNBQVMsQ0FBQyxTQUFTLEVBQUU7Z0JBQzdCLE9BQU8sS0FBSyxDQUFDO2FBQ2Q7WUFFRCxNQUFNLE9BQU8sR0FBRyxhQUFhLENBQUMsWUFBWSxDQUFDLENBQUM7WUFDNUMsTUFBTSxJQUFJLEdBQUcsSUFBSSxDQUFDLGtCQUFrQixDQUFDLEdBQUcsQ0FBQyxPQUFPLENBQUMsQ0FBQztZQUVsRCxJQUFJLElBQUksRUFBRTtnQkFDUixPQUFPLElBQUksQ0FBQyxPQUFPLENBQUMsWUFB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O2dCQXhDeEIsUUFBUTtnQkFNZCxNQUFNOzswQkFkUjtLQTRJQztTQTNGWSxlQUFlO0FBOEY1Qiw4RUFBOEU7QUFDOUU7SUFBQSxNQUdhLFdBQVc7UUFJdEIsWUFBb0IsV0FBb0MsRUFDcEMsZ0JBQWlDO1lBRGpDLGdCQUFXLEdBQVgsV0FBVyxDQUF5QjtZQUNwQyxxQkFBZ0IsR0FBaEIsZ0JBQWdCLENBQWlCO1lBSnJELDREQUE0RDtZQUNsRCxnQkFBVyxHQUFnQyxJQUFJLFlBQVksRUFBaUIsQ0FBQztRQUcvQixDQUFDO1FBRXpELFFBQVE7WUFDTixJQUFJLENBQUMsZ0JBQWdCO2lCQUNsQixPQUFPLENBQUMsSUFBSSxDQUFDLFdBQVcsQ0FBQztpQkFDekIsU0FBUyxDQUFDLEtBQUssQ0FBQyxFQUFFLENBQUMsSUFBSSxDQUFDLFdBQVcsQ0FBQyxJQUFJLENBQUMsS0FBSyxDQUFDLENBQUMsQ0FBQztRQUN0RCxDQUFDO1FBRUQsV0FBVztZQUNULElBQUksQ0FBQyxnQkFBZ0IsQ0FBQyxjQUFjLENBQUMsSUFBSSxDQUFDLFdBQVcsQ0FBQyxDQUFDO1FBQ3pELENBQUM7OztnQkFsQkYsU0FBUyxTQUFDO29CQUNULFFBQVEsRUFBRSxlQUFlO2lCQUMxQjs7O2dCQXZJQyxVQUFVO2dCQTZJNEIsZUFBZTs7OzhCQUhwRCxNQUFNOztJQWNULGtCQUFDO0tBQUE7U0FoQlksV0FBVyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge1BsYXRmb3JtLCBub3JtYWxpemVQYXNzaXZlTGlzdGVuZXJPcHRpb25zfSBmcm9tICdAYW5ndWxhci9jZGsvcGxhdGZvcm0nO1xuaW1wb3J0IHtcbiAgRGlyZWN0aXZlLFxuICBFbGVtZW50UmVmLFxuICBFdmVudEVtaXR0ZXIsXG4gIEluamVjdGFibGUsXG4gIE5nWm9uZSxcbiAgT25EZXN0cm95LFxuICBPbkluaXQsXG4gIE91dHB1dCxcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQge2NvZXJjZUVsZW1lbnR9IGZyb20gJ0Bhbmd1bGFyL2Nkay9jb2VyY2lvbic7XG5pbXBvcnQge0VNUFRZLCBPYnNlcnZhYmxlLCBTdWJqZWN0fSBmcm9tICdyeGpzJztcblxuXG4vKiogQW4gZXZlbnQgdGhhdCBpcyBlbWl0dGVkIHdoZW4gdGhlIGF1dG9maWxsIHN0YXRlIG9mIGFuIGlucHV0IGNoYW5nZXMuICovXG5leHBvcnQgdHlwZSBBdXRvZmlsbEV2ZW50ID0ge1xuICAvKiogVGhlIGVsZW1lbnQgd2hvc2UgYXV0b2ZpbGwgc3RhdGUgY2hhbmdlcy4gKi9cbiAgdGFyZ2V0OiBFbGVtZW50O1xuICAvKiogV2hldGhlciB0aGUgZWxlbWVudCBpcyBjdXJyZW50bHkgYXV0b2ZpbGxlZC4gKi9cbiAgaXNBdXRvZmlsbGVkOiBib29sZWFuO1xufTtcblxuXG4vKiogVXNlZCB0byB0cmFjayBpbmZvIGFib3V0IGN1cnJlbnRseSBtb25pdG9yZWQgZWxlbWVudHMuICovXG50eXBlIE1vbml0b3JlZEVsZW1lbnRJbmZvID0ge1xuICBzdWJqZWN0OiBTdWJqZWN0PEF1dG9maWxsRXZlbnQ+O1xuICB1bmxpc3RlbjogKCkgPT4gdm9pZDtcbn07XG5cblxuLyoqIE9wdGlvbnMgdG8gcGFzcyB0byB0aGUgYW5pbWF0aW9uc3RhcnQgbGlzdGVuZXIuICovXG5jb25zdCBsaXN0ZW5lck9wdGlvbnMgPSBub3JtYWxpemVQYXNzaXZlTGlzdGVuZXJPcHRpb25zKHtwYXNzaXZlOiB0cnVlfSk7XG5cblxuLyoqXG4gKiBBbiBpbmplY3RhYmxlIHNlcnZpY2UgdGhhdCBjYW4gYmUgdXNlZCB0byBtb25pdG9yIHRoZSBhdXRvZmlsbCBzdGF0ZSBvZiBhbiBpbnB1dC5cbiAqIEJhc2VkIG9uIHRoZSBmb2xsb3dpbmcgYmxvZyBwb3N0OlxuICogaHR0cHM6Ly9tZWRpdW0uY29tL0BicnVubi9kZXRlY3RpbmctYXV0b2ZpbGxlZC1maWVsZHMtaW4tamF2YXNjcmlwdC1hZWQ1OThkMjVkYTdcbiAqL1xuQEluamVjdGFibGUoe3Byb3ZpZGVkSW46ICdyb290J30pXG5leHBvcnQgY2xhc3MgQXV0b2ZpbGxNb25pdG9yIGltcGxlbWVudHMgT25EZXN0cm95IHtcbiAgcHJpdmF0ZSBfbW9uaXRvcmVkRWxlbWVudHMgPSBuZXcgTWFwPEVsZW1lbnQsIE1vbml0b3JlZEVsZW1lbnRJbmZvPigpO1xuXG4gIGNvbnN0cnVjdG9yKHByaXZhdGUgX3BsYXRmb3JtOiBQbGF0Zm9ybSwgcHJpdmF0ZSBfbmdab25lOiBOZ1pvbmUpIHt9XG5cbiAgLyoqXG4gICAqIE1vbml0b3IgZm9yIGNoYW5nZXMgaW4gdGhlIGF1dG9maWxsIHN0YXRlIG9mIHRoZSBnaXZlbiBpbnB1dCBlbGVtZW50LlxuICAgKiBAcGFyYW0gZWxlbWVudCBUaGUgZWxlbWVudCB0byBtb25pdG9yLlxuICAgKiBAcmV0dXJuIEEgc3RyZWFtIG9mIGF1dG9maWxsIHN0YXRlIGNoYW5nZXMuXG4gICAqL1xuICBtb25pdG9yKGVsZW1lbnQ6IEVsZW1lbnQpOiBPYnNlcnZhYmxlPEF1dG9maWxsRXZlbnQ+O1xuXG4gIC8qKlxuICAgKiBNb25pdG9yIGZvciBjaGFuZ2VzIGluIHRoZSBhdXRvZmlsbCBzdGF0ZSBvZiB0aGUgZ2l2ZW4gaW5wdXQgZWxlbWVudC5cbiAgICogQHBhcmFtIGVsZW1lbnQgVGhlIGVsZW1lbnQgdG8gbW9uaXRvci5cbiAgICogQHJldHVybiBBIHN0cmVhbSBvZiBhdXRvZmlsbCBzdGF0ZSBjaGFuZ2VzLlxuICAgKi9cbiAgbW9uaXRvcihlbGVtZW50OiBFbGVtZW50UmVmPEVsZW1lbnQ+KTogT2JzZXJ2YWJsZTxBdXRvZmlsbEV2ZW50PjtcblxuICBtb25pdG9yKGVsZW1lbnRPclJlZjogRWxlbWVudCB8IEVsZW1lbnRSZWY8RWxlbWVudD4pOiBPYnNlcnZhYmxlPEF1dG9maWxsRXZlbnQ+IHtcbiAgICBpZiAoIXRoaXMuX3BsYXRmb3JtLmlzQnJvd3Nlcikge1xuICAgICAgcmV0dXJuIEVNUFRZO1xuICAgIH1cblxuICAgIGNvbnN0IGVsZW1lbnQgPSBjb2VyY2VFbGVtZW50KGVsZW1lbnRPclJlZik7XG4gICAgY29uc3QgaW5mbyA9IHRoaXMuX21vbml0b3JlZEVsZW1lbnRzLmdldChlbGVtZW50KTtcblxuICAgIGlmIChpbmZvKSB7XG4gICAgICByZXR1cm4gaW5mby5zdWJqZWN0LmFzT2JzZXJ2YWJsZSgpO1xuICAgIH1cblxuICAgIGNvbnN0IHJlc3VsdCA9IG5ldyBTdWJqZWN0PEF1dG9maWxsRXZlbnQ+KCk7XG4gICAgY29uc3QgY3NzQ2xhc3MgPSAnY2RrLXRleHQtZmllbGQtYXV0b2ZpbGxlZCc7XG4gICAgY29uc3QgbGlzdGVuZXIgPSAoKGV2ZW50OiBBbmltYXRpb25FdmVudCkgPT4ge1xuICAgICAgLy8gQW5pbWF0aW9uIGV2ZW50cyBmaXJlIG9uIGluaXRpYWwgZWxlbWVudCByZW5kZXIsIHdlIGNoZWNrIGZvciB0aGUgcHJlc2VuY2Ugb2YgdGhlIGF1dG9maWxsXG4gICAgICAvLyBDU1MgY2xhc3MgdG8gbWFrZSBzdXJlIHRoaXMgaXMgYSByZWFsIGNoYW5nZSBpbiBzdGF0ZSwgbm90IGp1c3QgdGhlIGluaXRpYWwgcmVuZGVyIGJlZm9yZVxuICAgICAgLy8gd2UgZmlyZSBvZmYgZXZlbnRzLlxuICAgICAgaWYgKGV2ZW50LmFuaW1hdGlvbk5hbWUgPT09ICdjZGstdGV4dC1maWVsZC1hdXRvZmlsbC1zdGFydCcgJiZcbiAgICAgICAgICAhZWxlbWVudC5jbGFzc0xpc3QuY29udGFpbnMoY3NzQ2xhc3MpKSB7XG4gICAgICAgIGVsZW1lbnQuY2xhc3NMaXN0LmFkZChjc3NDbGFzcyk7XG4gICAgICAgIHRoaXMuX25nWm9uZS5ydW4oKCkgPT4gcmVzdWx0Lm5leHQoe3RhcmdldDogZXZlbnQudGFyZ2V0IGFzIEVsZW1lbnQsIGlzQXV0b2ZpbGxlZDogdHJ1ZX0pKTtcbiAgICAgIH0gZWxzZSBpZiAoZXZlbnQuYW5pbWF0aW9uTmFtZSA9PT0gJ2Nkay10ZXh0LWZpZWxkLWF1dG9maWxsLWVuZCcgJiZcbiAgICAgICAgICBlbGVtZW50LmNsYXNzTGlzdC5jb250YWlucyhjc3NDbGFzcykpIHtcbiAgICAgICAgZWxlbWVudC5jbGFzc0xpc3QucmVtb3ZlKGNzc0NsYXNzKTtcbiAgICAgICAgdGhpcy5fbmdab25lLnJ1bigoKSA9PiByZXN1bHQubmV4dCh7dGFyZ2V0OiBldmVudC50YXJnZXQgYXMgRWxlbWVudCwgaXNBdXRvZmlsbGVkOiBmYWxzZX0pKTtcbiAgICAgIH1cbiAgICB9KSBhcyBFdmVudExpc3RlbmVyT3JFdmVudExpc3RlbmVyT2JqZWN0O1xuXG4gICAgdGhpcy5fbmdab25lLnJ1bk91dHNpZGVBbmd1bGFyKCgpID0+IHtcbiAgICAgIGVsZW1lbnQuYWRkRXZlbnRMaXN0ZW5lcignYW5pbWF0aW9uc3RhcnQnLCBsaXN0ZW5lciwgbGlzdGVuZXJPcHRpb25zKTtcbiAgICAgIGVsZW1lbnQuY2xhc3NMaXN0LmFkZCgnY2RrLXRleHQtZmllbGQtYXV0b2ZpbGwtbW9uaXRvcmVkJyk7XG4gICAgfSk7XG5cbiAgICB0aGlzLl9tb25pdG9yZWRFbGVtZW50cy5zZXQoZWxlbWVudCwge1xuICAgICAgc3ViamVjdDogcmVzdWx0LFxuICAgICAgdW5saXN0ZW46ICgpID0+IHtcbiAgICAgICAgZWxlbWVudC5yZW1vdmVFdmVudExpc3RlbmVyKCdhbmltYXRpb25zdGFydCcsIGxpc3RlbmVyLCBsaXN0ZW5lck9wdGlvbnMpO1xuICAgICAgfVxuICAgIH0pO1xuXG4gICAgcmV0dXJuIHJlc3VsdC5hc09ic2VydmFibGUoKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBTdG9wIG1vbml0b3JpbmcgdGhlIGF1dG9maWxsIHN0YXRlIG9mIHRoZSBnaXZlbiBpbnB1dCBlbGVtZW50LlxuICAgKiBAcGFyYW0gZWxlbWVudCBUaGUgZWxlbWVudCB0byBzdG9wIG1vbml0b3JpbmcuXG4gICAqL1xuICBzdG9wTW9uaXRvcmluZyhlbGVtZW50OiBFbGVtZW50KTogdm9pZDtcblxuICAvKipcbiAgICogU3RvcCBtb25pdG9yaW5nIHRoZSBhdXRvZmlsbCBzdGF0ZSBvZiB0aGUgZ2l2ZW4gaW5wdXQgZWxlbWVudC5cbiAgICogQHBhcmFtIGVsZW1lbnQgVGhlIGVsZW1lbnQgdG8gc3RvcCBtb25pdG9yaW5nLlxuICAgKi9cbiAgc3RvcE1vbml0b3JpbmcoZWxlbWVudDogRWxlbWVudFJlZjxFbGVtZW50Pik6IHZvaWQ7XG5cbiAgc3RvcE1vbml0b3JpbmcoZWxlbWVudE9yUmVmOiBFbGVtZW50IHwgRWxlbWVudFJlZjxFbGVtZW50Pik6IHZvaWQge1xuICAgIGNvbnN0IGVsZW1lbnQgPSBjb2VyY2VFbGVtZW50KGVsZW1lbnRPclJlZik7XG4gICAgY29uc3QgaW5mbyA9IHRoaXMuX21vbml0b3JlZEVsZW1lbnRzLmdldChlbGVtZW50KTtcblxuICAgIGlmIChpbmZvKSB7XG4gICAgICBpbmZvLnVubGlzdGVuKCk7XG4gICAgICBpbmZvLnN1YmplY3QuY29tcGxldGUoKTtcbiAgICAgIGVsZW1lbnQuY2xhc3NMaXN0LnJlbW92ZSgnY2RrLXRleHQtZmllbGQtYXV0b2ZpbGwtbW9uaXRvcmVkJyk7XG4gICAgICBlbGVtZW50LmNsYXNzTGlzdC5yZW1vdmUoJ2Nkay10ZXh0LWZpZWxkLWF1dG9maWxsZWQnKTtcbiAgICAgIHRoaXMuX21vbml0b3JlZEVsZW1lbnRzLmRlbGV0ZShlbGVtZW50KTtcbiAgICB9XG4gIH1cblxuICBuZ09uRGVzdHJveSgpIHtcbiAgICB0aGlzLl9tb25pdG9yZWRFbGVtZW50cy5mb3JFYWNoKChfaW5mbywgZWxlbWVudCkgPT4gdGhpcy5zdG9wTW9uaXRvcmluZyhlbGVtZW50KSk7XG4gIH1cbn1cblxuXG4vKiogQSBkaXJlY3RpdmUgdGhhdCBjYW4gYmUgdXNlZCB0byBtb25pdG9yIHRoZSBhdXRvZmlsbCBzdGF0ZSBvZiBhbiBpbnB1dC4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogJ1tjZGtBdXRvZmlsbF0nLFxufSlcbmV4cG9ydCBjbGFzcyBDZGtBdXRvZmlsbCBpbXBsZW1lbnRzIE9uRGVzdHJveSwgT25Jbml0IHtcbiAgLyoqIEVtaXRzIHdoZW4gdGhlIGF1dG9maWxsIHN0YXRlIG9mIHRoZSBlbGVtZW50IGNoYW5nZXMuICovXG4gIEBPdXRwdXQoKSBjZGtBdXRvZmlsbDogRXZlbnRFbWl0dGVyPEF1dG9maWxsRXZlbnQ+ID0gbmV3IEV2ZW50RW1pdHRlcjxBdXRvZmlsbEV2ZW50PigpO1xuXG4gIGNvbnN0cnVjdG9yKHByaXZhdGUgX2VsZW1lbnRSZWY6IEVsZW1lbnRSZWY8SFRNTEVsZW1lbnQ+LFxuICAgICAgICAgICAgICBwcml2YXRlIF9hdXRvZmlsbE1vbml0b3I6IEF1dG9maWxsTW9uaXRvcikge31cblxuICBuZ09uSW5pdCgpIHtcbiAgICB0aGlzLl9hdXRvZmlsbE1vbml0b3JcbiAgICAgIC5tb25pdG9yKHRoaXMuX2VsZW1lbnRSZWYpXG4gICAgICAuc3Vic2NyaWJlKGV2ZW50ID0+IHRoaXMuY2RrQXV0b2ZpbGwuZW1pdChldmVudCkpO1xuICB9XG5cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgdGhpcy5fYXV0b2ZpbGxNb25pdG9yLnN0b3BNb25pdG9yaW5nKHRoaXMuX2VsZW1lbnRSZWYpO1xuICB9XG59XG4iXX0=