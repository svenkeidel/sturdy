/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { coerceBooleanProperty, coerceNumberProperty } from '@angular/cdk/coercion';
import { Directive, ElementRef, Input, NgZone, HostListener, Optional, Inject, } from '@angular/core';
import { Platform } from '@angular/cdk/platform';
import { auditTime, takeUntil } from 'rxjs/operators';
import { fromEvent, Subject } from 'rxjs';
import { DOCUMENT } from '@angular/common';
/** Directive to automatically resize a textarea to fit its content. */
let CdkTextareaAutosize = /** @class */ (() => {
    class CdkTextareaAutosize {
        constructor(_elementRef, _platform, _ngZone, 
        /** @breaking-change 11.0.0 make document required */
        document) {
            this._elementRef = _elementRef;
            this._platform = _platform;
            this._ngZone = _ngZone;
            this._destroyed = new Subject();
            this._enabled = true;
            /**
             * Value of minRows as of last resize. If the minRows has decreased, the
             * height of the textarea needs to be recomputed to reflect the new minimum. The maxHeight
             * does not have the same problem because it does not affect the textarea's scrollHeight.
             */
            this._previousMinRows = -1;
            this._document = document;
            this._textareaElement = this._elementRef.nativeElement;
            this._measuringClass = _platform.FIREFOX ?
                'cdk-textarea-autosize-measuring-firefox' :
                'cdk-textarea-autosize-measuring';
        }
        /** Minimum amount of rows in the textarea. */
        get minRows() { return this._minRows; }
        set minRows(value) {
            this._minRows = coerceNumberProperty(value);
            this._setMinHeight();
        }
        /** Maximum amount of rows in the textarea. */
        get maxRows() { return this._maxRows; }
        set maxRows(value) {
            this._maxRows = coerceNumberProperty(value);
            this._setMaxHeight();
        }
        /** Whether autosizing is enabled or not */
        get enabled() { return this._enabled; }
        set enabled(value) {
            value = coerceBooleanProperty(value);
            // Only act if the actual value changed. This specifically helps to not run
            // resizeToFitContent too early (i.e. before ngAfterViewInit)
            if (this._enabled !== value) {
                (this._enabled = value) ? this.resizeToFitContent(true) : this.reset();
            }
        }
        /** Sets the minimum height of the textarea as determined by minRows. */
        _setMinHeight() {
            const minHeight = this.minRows && this._cachedLineHeight ?
                `${this.minRows * this._cachedLineHeight}px` : null;
            if (minHeight) {
                this._textareaElement.style.minHeight = minHeight;
            }
        }
        /** Sets the maximum height of the textarea as determined by maxRows. */
        _setMaxHeight() {
            const maxHeight = this.maxRows && this._cachedLineHeight ?
                `${this.maxRows * this._cachedLineHeight}px` : null;
            if (maxHeight) {
                this._textareaElement.style.maxHeight = maxHeight;
            }
        }
        ngAfterViewInit() {
            if (this._platform.isBrowser) {
                // Remember the height which we started with in case autosizing is disabled
                this._initialHeight = this._textareaElement.style.height;
                this.resizeToFitContent();
                this._ngZone.runOutsideAngular(() => {
                    const window = this._getWindow();
                    fromEvent(window, 'resize')
                        .pipe(auditTime(16), takeUntil(this._destroyed))
                        .subscribe(() => this.resizeToFitContent(true));
                });
            }
        }
        ngOnDestroy() {
            this._destroyed.next();
            this._destroyed.complete();
        }
        /**
         * Cache the height of a single-row textarea if it has not already been cached.
         *
         * We need to know how large a single "row" of a textarea is in order to apply minRows and
         * maxRows. For the initial version, we will assume that the height of a single line in the
         * textarea does not ever change.
         */
        _cacheTextareaLineHeight() {
            if (this._cachedLineHeight) {
                return;
            }
            // Use a clone element because we have to override some styles.
            let textareaClone = this._textareaElement.cloneNode(false);
            textareaClone.rows = 1;
            // Use `position: absolute` so that this doesn't cause a browser layout and use
            // `visibility: hidden` so that nothing is rendered. Clear any other styles that
            // would affect the height.
            textareaClone.style.position = 'absolute';
            textareaClone.style.visibility = 'hidden';
            textareaClone.style.border = 'none';
            textareaClone.style.padding = '0';
            textareaClone.style.height = '';
            textareaClone.style.minHeight = '';
            textareaClone.style.maxHeight = '';
            // In Firefox it happens that textarea elements are always bigger than the specified amount
            // of rows. This is because Firefox tries to add extra space for the horizontal scrollbar.
            // As a workaround that removes the extra space for the scrollbar, we can just set overflow
            // to hidden. This ensures that there is no invalid calculation of the line height.
            // See Firefox bug report: https://bugzilla.mozilla.org/show_bug.cgi?id=33654
            textareaClone.style.overflow = 'hidden';
            this._textareaElement.parentNode.appendChild(textareaClone);
            this._cachedLineHeight = textareaClone.clientHeight;
            this._textareaElement.parentNode.removeChild(textareaClone);
            // Min and max heights have to be re-calculated if the cached line height changes
            this._setMinHeight();
            this._setMaxHeight();
        }
        ngDoCheck() {
            if (this._platform.isBrowser) {
                this.resizeToFitContent();
            }
        }
        /**
         * Resize the textarea to fit its content.
         * @param force Whether to force a height recalculation. By default the height will be
         *    recalculated only if the value changed since the last call.
         */
        resizeToFitContent(force = false) {
            // If autosizing is disabled, just skip everything else
            if (!this._enabled) {
                return;
            }
            this._cacheTextareaLineHeight();
            // If we haven't determined the line-height yet, we know we're still hidden and there's no point
            // in checking the height of the textarea.
            if (!this._cachedLineHeight) {
                return;
            }
            const textarea = this._elementRef.nativeElement;
            const value = textarea.value;
            // Only resize if the value or minRows have changed since these calculations can be expensive.
            if (!force && this._minRows === this._previousMinRows && value === this._previousValue) {
                return;
            }
            const placeholderText = textarea.placeholder;
            // Reset the textarea height to auto in order to shrink back to its default size.
            // Also temporarily force overflow:hidden, so scroll bars do not interfere with calculations.
            // Long placeholders that are wider than the textarea width may lead to a bigger scrollHeight
            // value. To ensure that the scrollHeight is not bigger than the content, the placeholders
            // need to be removed temporarily.
            textarea.classList.add(this._measuringClass);
            textarea.placeholder = '';
            // The measuring class includes a 2px padding to workaround an issue with Chrome,
            // so we account for that extra space here by subtracting 4 (2px top + 2px bottom).
            const height = textarea.scrollHeight - 4;
            // Use the scrollHeight to know how large the textarea *would* be if fit its entire value.
            textarea.style.height = `${height}px`;
            textarea.classList.remove(this._measuringClass);
            textarea.placeholder = placeholderText;
            this._ngZone.runOutsideAngular(() => {
                if (typeof requestAnimationFrame !== 'undefined') {
                    requestAnimationFrame(() => this._scrollToCaretPosition(textarea));
                }
                else {
                    setTimeout(() => this._scrollToCaretPosition(textarea));
                }
            });
            this._previousValue = value;
            this._previousMinRows = this._minRows;
        }
        /**
         * Resets the textarea to its original size
         */
        reset() {
            // Do not try to change the textarea, if the initialHeight has not been determined yet
            // This might potentially remove styles when reset() is called before ngAfterViewInit
            if (this._initialHeight !== undefined) {
                this._textareaElement.style.height = this._initialHeight;
            }
        }
        // In Ivy the `host` metadata will be merged, whereas in ViewEngine it is overridden. In order
        // to avoid double event listeners, we need to use `HostListener`. Once Ivy is the default, we
        // can move this back into `host`.
        // tslint:disable:no-host-decorator-in-concrete
        _noopInputHandler() {
            // no-op handler that ensures we're running change detection on input events.
        }
        /** Access injected document if available or fallback to global document reference */
        _getDocument() {
            return this._document || document;
        }
        /** Use defaultView of injected document if available or fallback to global window reference */
        _getWindow() {
            const doc = this._getDocument();
            return doc.defaultView || window;
        }
        /**
         * Scrolls a textarea to the caret position. On Firefox resizing the textarea will
         * prevent it from scrolling to the caret position. We need to re-set the selection
         * in order for it to scroll to the proper position.
         */
        _scrollToCaretPosition(textarea) {
            const { selectionStart, selectionEnd } = textarea;
            const document = this._getDocument();
            // IE will throw an "Unspecified error" if we try to set the selection range after the
            // element has been removed from the DOM. Assert that the directive hasn't been destroyed
            // between the time we requested the animation frame and when it was executed.
            // Also note that we have to assert that the textarea is focused before we set the
            // selection range. Setting the selection range on a non-focused textarea will cause
            // it to receive focus on IE and Edge.
            if (!this._destroyed.isStopped && document.activeElement === textarea) {
                textarea.setSelectionRange(selectionStart, selectionEnd);
            }
        }
    }
    CdkTextareaAutosize.decorators = [
        { type: Directive, args: [{
                    selector: 'textarea[cdkTextareaAutosize]',
                    exportAs: 'cdkTextareaAutosize',
                    host: {
                        'class': 'cdk-textarea-autosize',
                        // Textarea elements that have the directive applied should have a single row by default.
                        // Browsers normally show two rows by default and therefore this limits the minRows binding.
                        'rows': '1',
                    },
                },] }
    ];
    CdkTextareaAutosize.ctorParameters = () => [
        { type: ElementRef },
        { type: Platform },
        { type: NgZone },
        { type: undefined, decorators: [{ type: Optional }, { type: Inject, args: [DOCUMENT,] }] }
    ];
    CdkTextareaAutosize.propDecorators = {
        minRows: [{ type: Input, args: ['cdkAutosizeMinRows',] }],
        maxRows: [{ type: Input, args: ['cdkAutosizeMaxRows',] }],
        enabled: [{ type: Input, args: ['cdkTextareaAutosize',] }],
        _noopInputHandler: [{ type: HostListener, args: ['input',] }]
    };
    return CdkTextareaAutosize;
})();
export { CdkTextareaAutosize };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYXV0b3NpemUuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL3RleHQtZmllbGQvYXV0b3NpemUudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUVMLHFCQUFxQixFQUNyQixvQkFBb0IsRUFFckIsTUFBTSx1QkFBdUIsQ0FBQztBQUMvQixPQUFPLEVBQ0wsU0FBUyxFQUNULFVBQVUsRUFDVixLQUFLLEVBSUwsTUFBTSxFQUNOLFlBQVksRUFDWixRQUFRLEVBQ1IsTUFBTSxHQUNQLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBQyxRQUFRLEVBQUMsTUFBTSx1QkFBdUIsQ0FBQztBQUMvQyxPQUFPLEVBQUMsU0FBUyxFQUFFLFNBQVMsRUFBQyxNQUFNLGdCQUFnQixDQUFDO0FBQ3BELE9BQU8sRUFBQyxTQUFTLEVBQUUsT0FBTyxFQUFDLE1BQU0sTUFBTSxDQUFDO0FBQ3hDLE9BQU8sRUFBQyxRQUFRLEVBQUMsTUFBTSxpQkFBaUIsQ0FBQztBQUV6Qyx1RUFBdUU7QUFDdkU7SUFBQSxNQVVhLG1CQUFtQjtRQXlEOUIsWUFBb0IsV0FBb0MsRUFDcEMsU0FBbUIsRUFDbkIsT0FBZTtRQUN2QixxREFBcUQ7UUFDdkIsUUFBYztZQUpwQyxnQkFBVyxHQUFYLFdBQVcsQ0FBeUI7WUFDcEMsY0FBUyxHQUFULFNBQVMsQ0FBVTtZQUNuQixZQUFPLEdBQVAsT0FBTyxDQUFRO1lBdkRsQixlQUFVLEdBQUcsSUFBSSxPQUFPLEVBQVEsQ0FBQztZQUkxQyxhQUFRLEdBQVksSUFBSSxDQUFDO1lBRWpDOzs7O2VBSUc7WUFDSyxxQkFBZ0IsR0FBVyxDQUFDLENBQUMsQ0FBQztZQStDcEMsSUFBSSxDQUFDLFNBQVMsR0FBRyxRQUFRLENBQUM7WUFFMUIsSUFBSSxDQUFDLGdCQUFnQixHQUFHLElBQUksQ0FBQyxXQUFXLENBQUMsYUFBb0MsQ0FBQztZQUM5RSxJQUFJLENB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c291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7XG4gIEJvb2xlYW5JbnB1dCxcbiAgY29lcmNlQm9vbGVhblByb3BlcnR5LFxuICBjb2VyY2VOdW1iZXJQcm9wZXJ0eSxcbiAgTnVtYmVySW5wdXRcbn0gZnJvbSAnQGFuZ3VsYXIvY2RrL2NvZXJjaW9uJztcbmltcG9ydCB7XG4gIERpcmVjdGl2ZSxcbiAgRWxlbWVudFJlZixcbiAgSW5wdXQsXG4gIEFmdGVyVmlld0luaXQsXG4gIERvQ2hlY2ssXG4gIE9uRGVzdHJveSxcbiAgTmdab25lLFxuICBIb3N0TGlzdGVuZXIsXG4gIE9wdGlvbmFsLFxuICBJbmplY3QsXG59IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtQbGF0Zm9ybX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3BsYXRmb3JtJztcbmltcG9ydCB7YXVkaXRUaW1lLCB0YWtlVW50aWx9IGZyb20gJ3J4anMvb3BlcmF0b3JzJztcbmltcG9ydCB7ZnJvbUV2ZW50LCBTdWJqZWN0fSBmcm9tICdyeGpzJztcbmltcG9ydCB7RE9DVU1FTlR9IGZyb20gJ0Bhbmd1bGFyL2NvbW1vbic7XG5cbi8qKiBEaXJlY3RpdmUgdG8gYXV0b21hdGljYWxseSByZXNpemUgYSB0ZXh0YXJlYSB0byBmaXQgaXRzIGNvbnRlbnQuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICd0ZXh0YXJlYVtjZGtUZXh0YXJlYUF1dG9zaXplXScsXG4gIGV4cG9ydEFzOiAnY2RrVGV4dGFyZWFBdXRvc2l6ZScsXG4gIGhvc3Q6IHtcbiAgICAnY2xhc3MnOiAnY2RrLXRleHRhcmVhLWF1dG9zaXplJyxcbiAgICAvLyBUZXh0YXJlYSBlbGVtZW50cyB0aGF0IGhhdmUgdGhlIGRpcmVjdGl2ZSBhcHBsaWVkIHNob3VsZCBoYXZlIGEgc2luZ2xlIHJvdyBieSBkZWZhdWx0LlxuICAgIC8vIEJyb3dzZXJzIG5vcm1hbGx5IHNob3cgdHdvIHJvd3MgYnkgZGVmYXVsdCBhbmQgdGhlcmVmb3JlIHRoaXMgbGltaXRzIHRoZSBtaW5Sb3dzIGJpbmRpbmcuXG4gICAgJ3Jvd3MnOiAnMScsXG4gIH0sXG59KVxuZXhwb3J0IGNsYXNzIENka1RleHRhcmVhQXV0b3NpemUgaW1wbGVtZW50cyBBZnRlclZpZXdJbml0LCBEb0NoZWNrLCBPbkRlc3Ryb3kge1xuICAvKiogS2VlcCB0cmFjayBvZiB0aGUgcHJldmlvdXMgdGV4dGFyZWEgdmFsdWUgdG8gYXZvaWQgcmVzaXppbmcgd2hlbiB0aGUgdmFsdWUgaGFzbid0IGNoYW5nZWQuICovXG4gIHByaXZhdGUgX3ByZXZpb3VzVmFsdWU/OiBzdHJpbmc7XG4gIHByaXZhdGUgX2luaXRpYWxIZWlnaHQ6IHN0cmluZyB8IHVuZGVmaW5lZDtcbiAgcHJpdmF0ZSByZWFkb25seSBfZGVzdHJveWVkID0gbmV3IFN1YmplY3Q8dm9pZD4oKTtcblxuICBwcml2YXRlIF9taW5Sb3dzOiBudW1iZXI7XG4gIHByaXZhdGUgX21heFJvd3M6IG51bWJlcjtcbiAgcHJpdmF0ZSBfZW5hYmxlZDogYm9vbGVhbiA9IHRydWU7XG5cbiAgLyoqXG4gICAqIFZhbHVlIG9mIG1pblJvd3MgYXMgb2YgbGFzdCByZXNpemUuIElmIHRoZSBtaW5Sb3dzIGhhcyBkZWNyZWFzZWQsIHRoZVxuICAgKiBoZWlnaHQgb2YgdGhlIHRleHRhcmVhIG5lZWRzIHRvIGJlIHJlY29tcHV0ZWQgdG8gcmVmbGVjdCB0aGUgbmV3IG1pbmltdW0uIFRoZSBtYXhIZWlnaHRcbiAgICogZG9lcyBub3QgaGF2ZSB0aGUgc2FtZSBwcm9ibGVtIGJlY2F1c2UgaXQgZG9lcyBub3QgYWZmZWN0IHRoZSB0ZXh0YXJlYSdzIHNjcm9sbEhlaWdodC5cbiAgICovXG4gIHByaXZhdGUgX3ByZXZpb3VzTWluUm93czogbnVtYmVyID0gLTE7XG5cbiAgcHJpdmF0ZSBfdGV4dGFyZWFFbGVtZW50OiBIVE1MVGV4dEFyZWFFbGVtZW50O1xuXG4gIC8qKiBNaW5pbXVtIGFtb3VudCBvZiByb3dzIGluIHRoZSB0ZXh0YXJlYS4gKi9cbiAgQElucHV0KCdjZGtBdXRvc2l6ZU1pblJvd3MnKVxuICBnZXQgbWluUm93cygpOiBudW1iZXIgeyByZXR1cm4gdGhpcy5fbWluUm93czsgfVxuICBzZXQgbWluUm93cyh2YWx1ZTogbnVtYmVyKSB7XG4gICAgdGhpcy5fbWluUm93cyA9IGNvZXJjZU51bWJlclByb3BlcnR5KHZhbHVlKTtcbiAgICB0aGlzLl9zZXRNaW5IZWlnaHQoKTtcbiAgfVxuXG4gIC8qKiBNYXhpbXVtIGFtb3VudCBvZiByb3dzIGluIHRoZSB0ZXh0YXJlYS4gKi9cbiAgQElucHV0KCdjZGtBdXRvc2l6ZU1heFJvd3MnKVxuICBnZXQgbWF4Um93cygpOiBudW1iZXIgeyByZXR1cm4gdGhpcy5fbWF4Um93czsgfVxuICBzZXQgbWF4Um93cyh2YWx1ZTogbnVtYmVyKSB7XG4gICAgdGhpcy5fbWF4Um93cyA9IGNvZXJjZU51bWJlclByb3BlcnR5KHZhbHVlKTtcbiAgICB0aGlzLl9zZXRNYXhIZWlnaHQoKTtcbiAgfVxuXG4gIC8qKiBXaGV0aGVyIGF1dG9zaXppbmcgaXMgZW5hYmxlZCBvciBub3QgKi9cbiAgQElucHV0KCdjZGtUZXh0YXJlYUF1dG9zaXplJylcbiAgZ2V0IGVuYWJsZWQoKTogYm9vbGVhbiB7IHJldHVybiB0aGlzLl9lbmFibGVkOyB9XG4gIHNldCBlbmFibGVkKHZhbHVlOiBib29sZWFuKSB7XG4gICAgdmFsdWUgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkodmFsdWUpO1xuXG4gICAgLy8gT25seSBhY3QgaWYgdGhlIGFjdHVhbCB2YWx1ZSBjaGFuZ2VkLiBUaGlzIHNwZWNpZmljYWxseSBoZWxwcyB0byBub3QgcnVuXG4gICAgLy8gcmVzaXplVG9GaXRDb250ZW50IHRvbyBlYXJseSAoaS5lLiBiZWZvcmUgbmdBZnRlclZpZXdJbml0KVxuICAgIGlmICh0aGlzLl9lbmFibGVkICE9PSB2YWx1ZSkge1xuICAgICAgKHRoaXMuX2VuYWJsZWQgPSB2YWx1ZSkgPyB0aGlzLnJlc2l6ZVRvRml0Q29udGVudCh0cnVlKSA6IHRoaXMucmVzZXQoKTtcbiAgICB9XG4gIH1cblxuICAvKiogQ2FjaGVkIGhlaWdodCBvZiBhIHRleHRhcmVhIHdpdGggYSBzaW5nbGUgcm93LiAqL1xuICBwcml2YXRlIF9jYWNoZWRMaW5lSGVpZ2h0OiBudW1iZXI7XG5cbiAgLyoqIFVzZWQgdG8gcmVmZXJlbmNlIGNvcnJlY3QgZG9jdW1lbnQvd2luZG93ICovXG4gIHByb3RlY3RlZCBfZG9jdW1lbnQ/OiBEb2N1bWVudDtcblxuICAvKiogQ2xhc3MgdGhhdCBzaG91bGQgYmUgYXBwbGllZCB0byB0aGUgdGV4dGFyZWEgd2hpbGUgaXQncyBiZWluZyBtZWFzdXJlZC4gKi9cbiAgcHJpdmF0ZSBfbWVhc3VyaW5nQ2xhc3M6IHN0cmluZztcblxuICBjb25zdHJ1Y3Rvcihwcml2YXRlIF9lbGVtZW50UmVmOiBFbGVtZW50UmVmPEhUTUxFbGVtZW50PixcbiAgICAgICAgICAgICAgcHJpdmF0ZSBfcGxhdGZvcm06IFBsYXRmb3JtLFxuICAgICAgICAgICAgICBwcml2YXRlIF9uZ1pvbmU6IE5nWm9uZSxcbiAgICAgICAgICAgICAgLyoqIEBicmVha2luZy1jaGFuZ2UgMTEuMC4wIG1ha2UgZG9jdW1lbnQgcmVxdWlyZWQgKi9cbiAgICAgICAgICAgICAgQE9wdGlvbmFsKCkgQEluamVjdChET0NVTUVOVCkgZG9jdW1lbnQ/OiBhbnkpIHtcbiAgICB0aGlzLl9kb2N1bWVudCA9IGRvY3VtZW50O1xuXG4gICAgdGhpcy5fdGV4dGFyZWFFbGVtZW50ID0gdGhpcy5fZWxlbWVudFJlZi5uYXRpdmVFbGVtZW50IGFzIEhUTUxUZXh0QXJlYUVsZW1lbnQ7XG4gICAgdGhpcy5fbWVhc3VyaW5nQ2xhc3MgPSBfcGxhdGZvcm0uRklSRUZPWCA/XG4gICAgICAnY2RrLXRleHRhcmVhLWF1dG9zaXplLW1lYXN1cmluZy1maXJlZm94JyA6XG4gICAgICAnY2RrLXRleHRhcmVhLWF1dG9zaXplLW1lYXN1cmluZyc7XG4gIH1cblxuICAvKiogU2V0cyB0aGUgbWluaW11bSBoZWlnaHQgb2YgdGhlIHRleHRhcmVhIGFzIGRldGVybWluZWQgYnkgbWluUm93cy4gKi9cbiAgX3NldE1pbkhlaWdodCgpOiB2b2lkIHtcbiAgICBjb25zdCBtaW5IZWlnaHQgPSB0aGlzLm1pblJvd3MgJiYgdGhpcy5fY2FjaGVkTGluZUhlaWdodCA/XG4gICAgICAgIGAke3RoaXMubWluUm93cyAqIHRoaXMuX2NhY2hlZExpbmVIZWlnaHR9cHhgIDogbnVsbDtcblxuICAgIGlmIChtaW5IZWlnaHQpICB7XG4gICAgICB0aGlzLl90ZXh0YXJlYUVsZW1lbnQuc3R5bGUubWluSGVpZ2h0ID0gbWluSGVpZ2h0O1xuICAgIH1cbiAgfVxuXG4gIC8qKiBTZXRzIHRoZSBtYXhpbXVtIGhlaWdodCBvZiB0aGUgdGV4dGFyZWEgYXMgZGV0ZXJtaW5lZCBieSBtYXhSb3dzLiAqL1xuICBfc2V0TWF4SGVpZ2h0KCk6IHZvaWQge1xuICAgIGNvbnN0IG1heEhlaWdodCA9IHRoaXMubWF4Um93cyAmJiB0aGlzLl9jYWNoZWRMaW5lSGVpZ2h0ID9cbiAgICAgICAgYCR7dGhpcy5tYXhSb3dzICogdGhpcy5fY2FjaGVkTGluZUhlaWdodH1weGAgOiBudWxsO1xuXG4gICAgaWYgKG1heEhlaWdodCkge1xuICAgICAgdGhpcy5fdGV4dGFyZWFFbGVtZW50LnN0eWxlLm1heEhlaWdodCA9IG1heEhlaWdodDtcbiAgICB9XG4gIH1cblxuICBuZ0FmdGVyVmlld0luaXQoKSB7XG4gICAgaWYgKHRoaXMuX3BsYXRmb3JtLmlzQnJvd3Nlcikge1xuICAgICAgLy8gUmVtZW1iZXIgdGhlIGhlaWdodCB3aGljaCB3ZSBzdGFydGVkIHdpdGggaW4gY2FzZSBhdXRvc2l6aW5nIGlzIGRpc2FibGVkXG4gICAgICB0aGlzLl9pbml0aWFsSGVpZ2h0ID0gdGhpcy5fdGV4dGFyZWFFbGVtZW50LnN0eWxlLmhlaWdodDtcblxuICAgICAgdGhpcy5yZXNpemVUb0ZpdENvbnRlbnQoKTtcblxuICAgICAgdGhpcy5fbmdab25lLnJ1bk91dHNpZGVBbmd1bGFyKCgpID0+IHtcbiAgICAgICAgY29uc3Qgd2luZG93ID0gdGhpcy5fZ2V0V2luZG93KCk7XG5cbiAgICAgICAgZnJvbUV2ZW50KHdpbmRvdywgJ3Jlc2l6ZScpXG4gICAgICAgICAgLnBpcGUoYXVkaXRUaW1lKDE2KSwgdGFrZVVudGlsKHRoaXMuX2Rlc3Ryb3llZCkpXG4gICAgICAgICAgLnN1YnNjcmliZSgoKSA9PiB0aGlzLnJlc2l6ZVRvRml0Q29udGVudCh0cnVlKSk7XG4gICAgICB9KTtcbiAgICB9XG4gIH1cblxuICBuZ09uRGVzdHJveSgpIHtcbiAgICB0aGlzLl9kZXN0cm95ZWQubmV4dCgpO1xuICAgIHRoaXMuX2Rlc3Ryb3llZC5jb21wbGV0ZSgpO1xuICB9XG5cbiAgLyoqXG4gICAqIENhY2hlIHRoZSBoZWlnaHQgb2YgYSBzaW5nbGUtcm93IHRleHRhcmVhIGlmIGl0IGhhcyBub3QgYWxyZWFkeSBiZWVuIGNhY2hlZC5cbiAgICpcbiAgICogV2UgbmVlZCB0byBrbm93IGhvdyBsYXJnZSBhIHNpbmdsZSBcInJvd1wiIG9mIGEgdGV4dGFyZWEgaXMgaW4gb3JkZXIgdG8gYXBwbHkgbWluUm93cyBhbmRcbiAgICogbWF4Um93cy4gRm9yIHRoZSBpbml0aWFsIHZlcnNpb24sIHdlIHdpbGwgYXNzdW1lIHRoYXQgdGhlIGhlaWdodCBvZiBhIHNpbmdsZSBsaW5lIGluIHRoZVxuICAgKiB0ZXh0YXJlYSBkb2VzIG5vdCBldmVyIGNoYW5nZS5cbiAgICovXG4gIHByaXZhdGUgX2NhY2hlVGV4dGFyZWFMaW5lSGVpZ2h0KCk6IHZvaWQge1xuICAgIGlmICh0aGlzLl9jYWNoZWRMaW5lSGVpZ2h0KSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgLy8gVXNlIGEgY2xvbmUgZWxlbWVudCBiZWNhdXNlIHdlIGhhdmUgdG8gb3ZlcnJpZGUgc29tZSBzdHlsZXMuXG4gICAgbGV0IHRleHRhcmVhQ2xvbmUgPSB0aGlzLl90ZXh0YXJlYUVsZW1lbnQuY2xvbmVOb2RlKGZhbHNlKSBhcyBIVE1MVGV4dEFyZWFFbGVtZW50O1xuICAgIHRleHRhcmVhQ2xvbmUucm93cyA9IDE7XG5cbiAgICAvLyBVc2UgYHBvc2l0aW9uOiBhYnNvbHV0ZWAgc28gdGhhdCB0aGlzIGRvZXNuJ3QgY2F1c2UgYSBicm93c2VyIGxheW91dCBhbmQgdXNlXG4gICAgLy8gYHZpc2liaWxpdHk6IGhpZGRlbmAgc28gdGhhdCBub3RoaW5nIGlzIHJlbmRlcmVkLiBDbGVhciBhbnkgb3RoZXIgc3R5bGVzIHRoYXRcbiAgICAvLyB3b3VsZCBhZmZlY3QgdGhlIGhlaWdodC5cbiAgICB0ZXh0YXJlYUNsb25lLnN0eWxlLnBvc2l0aW9uID0gJ2Fic29sdXRlJztcbiAgICB0ZXh0YXJlYUNsb25lLnN0eWxlLnZpc2liaWxpdHkgPSAnaGlkZGVuJztcbiAgICB0ZXh0YXJlYUNsb25lLnN0eWxlLmJvcmRlciA9ICdub25lJztcbiAgICB0ZXh0YXJlYUNsb25lLnN0eWxlLnBhZGRpbmcgPSAnMCc7XG4gICAgdGV4dGFyZWFDbG9uZS5zdHlsZS5oZWlnaHQgPSAnJztcbiAgICB0ZXh0YXJlYUNsb25lLnN0eWxlLm1pbkhlaWdodCA9ICcnO1xuICAgIHRleHRhcmVhQ2xvbmUuc3R5bGUubWF4SGVpZ2h0ID0gJyc7XG5cbiAgICAvLyBJbiBGaXJlZm94IGl0IGhhcHBlbnMgdGhhdCB0ZXh0YXJlYSBlbGVtZW50cyBhcmUgYWx3YXlzIGJpZ2dlciB0aGFuIHRoZSBzcGVjaWZpZWQgYW1vdW50XG4gICAgLy8gb2Ygcm93cy4gVGhpcyBpcyBiZWNhdXNlIEZpcmVmb3ggdHJpZXMgdG8gYWRkIGV4dHJhIHNwYWNlIGZvciB0aGUgaG9yaXpvbnRhbCBzY3JvbGxiYXIuXG4gICAgLy8gQXMgYSB3b3JrYXJvdW5kIHRoYXQgcmVtb3ZlcyB0aGUgZXh0cmEgc3BhY2UgZm9yIHRoZSBzY3JvbGxiYXIsIHdlIGNhbiBqdXN0IHNldCBvdmVyZmxvd1xuICAgIC8vIHRvIGhpZGRlbi4gVGhpcyBlbnN1cmVzIHRoYXQgdGhlcmUgaXMgbm8gaW52YWxpZCBjYWxjdWxhdGlvbiBvZiB0aGUgbGluZSBoZWlnaHQuXG4gICAgLy8gU2VlIEZpcmVmb3ggYnVnIHJlcG9ydDogaHR0cHM6Ly9idWd6aWxsYS5tb3ppbGxhLm9yZy9zaG93X2J1Zy5jZ2k/aWQ9MzM2NTRcbiAgICB0ZXh0YXJlYUNsb25lLnN0eWxlLm92ZXJmbG93ID0gJ2hpZGRlbic7XG5cbiAgICB0aGlzLl90ZXh0YXJlYUVsZW1lbnQucGFyZW50Tm9kZSEuYXBwZW5kQ2hpbGQodGV4dGFyZWFDbG9uZSk7XG4gICAgdGhpcy5fY2FjaGVkTGluZUhlaWdodCA9IHRleHRhcmVhQ2xvbmUuY2xpZW50SGVpZ2h0O1xuICAgIHRoaXMuX3RleHRhcmVhRWxlbWVudC5wYXJlbnROb2RlIS5yZW1vdmVDaGlsZCh0ZXh0YXJlYUNsb25lKTtcblxuICAgIC8vIE1pbiBhbmQgbWF4IGhlaWdodHMgaGF2ZSB0byBiZSByZS1jYWxjdWxhdGVkIGlmIHRoZSBjYWNoZWQgbGluZSBoZWlnaHQgY2hhbmdlc1xuICAgIHRoaXMuX3NldE1pbkhlaWdodCgpO1xuICAgIHRoaXMuX3NldE1heEhlaWdodCgpO1xuICB9XG5cbiAgbmdEb0NoZWNrKCkge1xuICAgIGlmICh0aGlzLl9wbGF0Zm9ybS5pc0Jyb3dzZXIpIHtcbiAgICAgIHRoaXMucmVzaXplVG9GaXRDb250ZW50KCk7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIFJlc2l6ZSB0aGUgdGV4dGFyZWEgdG8gZml0IGl0cyBjb250ZW50LlxuICAgKiBAcGFyYW0gZm9yY2UgV2hldGhlciB0byBmb3JjZSBhIGhlaWdodCByZWNhbGN1bGF0aW9uLiBCeSBkZWZhdWx0IHRoZSBoZWlnaHQgd2lsbCBiZVxuICAgKiAgICByZWNhbGN1bGF0ZWQgb25seSBpZiB0aGUgdmFsdWUgY2hhbmdlZCBzaW5jZSB0aGUgbGFzdCBjYWxsLlxuICAgKi9cbiAgcmVzaXplVG9GaXRDb250ZW50KGZvcmNlOiBib29sZWFuID0gZmFsc2UpIHtcbiAgICAvLyBJZiBhdXRvc2l6aW5nIGlzIGRpc2FibGVkLCBqdXN0IHNraXAgZXZlcnl0aGluZyBlbHNlXG4gICAgaWYgKCF0aGlzLl9lbmFibGVkKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgdGhpcy5fY2FjaGVUZXh0YXJlYUxpbmVIZWlnaHQoKTtcblxuICAgIC8vIElmIHdlIGhhdmVuJ3QgZGV0ZXJtaW5lZCB0aGUgbGluZS1oZWlnaHQgeWV0LCB3ZSBrbm93IHdlJ3JlIHN0aWxsIGhpZGRlbiBhbmQgdGhlcmUncyBubyBwb2ludFxuICAgIC8vIGluIGNoZWNraW5nIHRoZSBoZWlnaHQgb2YgdGhlIHRleHRhcmVhLlxuICAgIGlmICghdGhpcy5fY2FjaGVkTGluZUhlaWdodCkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGNvbnN0IHRleHRhcmVhID0gdGhpcy5fZWxlbWVudFJlZi5uYXRpdmVFbGVtZW50IGFzIEhUTUxUZXh0QXJlYUVsZW1lbnQ7XG4gICAgY29uc3QgdmFsdWUgPSB0ZXh0YXJlYS52YWx1ZTtcblxuICAgIC8vIE9ubHkgcmVzaXplIGlmIHRoZSB2YWx1ZSBvciBtaW5Sb3dzIGhhdmUgY2hhbmdlZCBzaW5jZSB0aGVzZSBjYWxjdWxhdGlvbnMgY2FuIGJlIGV4cGVuc2l2ZS5cbiAgICBpZiAoIWZvcmNlICYmIHRoaXMuX21pblJvd3MgPT09IHRoaXMuX3ByZXZpb3VzTWluUm93cyAmJiB2YWx1ZSA9PT0gdGhpcy5fcHJldmlvdXNWYWx1ZSkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGNvbnN0IHBsYWNlaG9sZGVyVGV4dCA9IHRleHRhcmVhLnBsYWNlaG9sZGVyO1xuXG4gICAgLy8gUmVzZXQgdGhlIHRleHRhcmVhIGhlaWdodCB0byBhdXRvIGluIG9yZGVyIHRvIHNocmluayBiYWNrIHRvIGl0cyBkZWZhdWx0IHNpemUuXG4gICAgLy8gQWxzbyB0ZW1wb3JhcmlseSBmb3JjZSBvdmVyZmxvdzpoaWRkZW4sIHNvIHNjcm9sbCBiYXJzIGRvIG5vdCBpbnRlcmZlcmUgd2l0aCBjYWxjdWxhdGlvbnMuXG4gICAgLy8gTG9uZyBwbGFjZWhvbGRlcnMgdGhhdCBhcmUgd2lkZXIgdGhhbiB0aGUgdGV4dGFyZWEgd2lkdGggbWF5IGxlYWQgdG8gYSBiaWdnZXIgc2Nyb2xsSGVpZ2h0XG4gICAgLy8gdmFsdWUuIFRvIGVuc3VyZSB0aGF0IHRoZSBzY3JvbGxIZWlnaHQgaXMgbm90IGJpZ2dlciB0aGFuIHRoZSBjb250ZW50LCB0aGUgcGxhY2Vob2xkZXJzXG4gICAgLy8gbmVlZCB0byBiZSByZW1vdmVkIHRlbXBvcmFyaWx5LlxuICAgIHRleHRhcmVhLmNsYXNzTGlzdC5hZGQodGhpcy5fbWVhc3VyaW5nQ2xhc3MpO1xuICAgIHRleHRhcmVhLnBsYWNlaG9sZGVyID0gJyc7XG5cbiAgICAvLyBUaGUgbWVhc3VyaW5nIGNsYXNzIGluY2x1ZGVzIGEgMnB4IHBhZGRpbmcgdG8gd29ya2Fyb3VuZCBhbiBpc3N1ZSB3aXRoIENocm9tZSxcbiAgICAvLyBzbyB3ZSBhY2NvdW50IGZvciB0aGF0IGV4dHJhIHNwYWNlIGhlcmUgYnkgc3VidHJhY3RpbmcgNCAoMnB4IHRvcCArIDJweCBib3R0b20pLlxuICAgIGNvbnN0IGhlaWdodCA9IHRleHRhcmVhLnNjcm9sbEhlaWdodCAtIDQ7XG5cbiAgICAvLyBVc2UgdGhlIHNjcm9sbEhlaWdodCB0byBrbm93IGhvdyBsYXJnZSB0aGUgdGV4dGFyZWEgKndvdWxkKiBiZSBpZiBmaXQgaXRzIGVudGlyZSB2YWx1ZS5cbiAgICB0ZXh0YXJlYS5zdHlsZS5oZWlnaHQgPSBgJHtoZWlnaHR9cHhgO1xuICAgIHRleHRhcmVhLmNsYXNzTGlzdC5yZW1vdmUodGhpcy5fbWVhc3VyaW5nQ2xhc3MpO1xuICAgIHRleHRhcmVhLnBsYWNlaG9sZGVyID0gcGxhY2Vob2xkZXJUZXh0O1xuXG4gICAgdGhpcy5fbmdab25lLnJ1bk91dHNpZGVBbmd1bGFyKCgpID0+IHtcbiAgICAgIGlmICh0eXBlb2YgcmVxdWVzdEFuaW1hdGlvbkZyYW1lICE9PSAndW5kZWZpbmVkJykge1xuICAgICAgICByZXF1ZXN0QW5pbWF0aW9uRnJhbWUoKCkgPT4gdGhpcy5fc2Nyb2xsVG9DYXJldFBvc2l0aW9uKHRleHRhcmVhKSk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICBzZXRUaW1lb3V0KCgpID0+IHRoaXMuX3Njcm9sbFRvQ2FyZXRQb3NpdGlvbih0ZXh0YXJlYSkpO1xuICAgICAgfVxuICAgIH0pO1xuXG4gICAgdGhpcy5fcHJldmlvdXNWYWx1ZSA9IHZhbHVlO1xuICAgIHRoaXMuX3ByZXZpb3VzTWluUm93cyA9IHRoaXMuX21pblJvd3M7XG4gIH1cblxuICAvKipcbiAgICogUmVzZXRzIHRoZSB0ZXh0YXJlYSB0byBpdHMgb3JpZ2luYWwgc2l6ZVxuICAgKi9cbiAgcmVzZXQoKSB7XG4gICAgLy8gRG8gbm90IHRyeSB0byBjaGFuZ2UgdGhlIHRleHRhcmVhLCBpZiB0aGUgaW5pdGlhbEhlaWdodCBoYXMgbm90IGJlZW4gZGV0ZXJtaW5lZCB5ZXRcbiAgICAvLyBUaGlzIG1pZ2h0IHBvdGVudGlhbGx5IHJlbW92ZSBzdHlsZXMgd2hlbiByZXNldCgpIGlzIGNhbGxlZCBiZWZvcmUgbmdBZnRlclZpZXdJbml0XG4gICAgaWYgKHRoaXMuX2luaXRpYWxIZWlnaHQgIT09IHVuZGVmaW5lZCkge1xuICAgICAgdGhpcy5fdGV4dGFyZWFFbGVtZW50LnN0eWxlLmhlaWdodCA9IHRoaXMuX2luaXRpYWxIZWlnaHQ7XG4gICAgfVxuICB9XG5cbiAgLy8gSW4gSXZ5IHRoZSBgaG9zdGAgbWV0YWRhdGEgd2lsbCBiZSBtZXJnZWQsIHdoZXJlYXMgaW4gVmlld0VuZ2luZSBpdCBpcyBvdmVycmlkZGVuLiBJbiBvcmRlclxuICAvLyB0byBhdm9pZCBkb3VibGUgZXZlbnQgbGlzdGVuZXJzLCB3ZSBuZWVkIHRvIHVzZSBgSG9zdExpc3RlbmVyYC4gT25jZSBJdnkgaXMgdGhlIGRlZmF1bHQsIHdlXG4gIC8vIGNhbiBtb3ZlIHRoaXMgYmFjayBpbnRvIGBob3N0YC5cbiAgLy8gdHNsaW50OmRpc2FibGU6bm8taG9zdC1kZWNvcmF0b3ItaW4tY29uY3JldGVcbiAgQEhvc3RMaXN0ZW5lcignaW5wdXQnKVxuICBfbm9vcElucHV0SGFuZGxlcigpIHtcbiAgICAvLyBuby1vcCBoYW5kbGVyIHRoYXQgZW5zdXJlcyB3ZSdyZSBydW5uaW5nIGNoYW5nZSBkZXRlY3Rpb24gb24gaW5wdXQgZXZlbnRzLlxuICB9XG5cbiAgLyoqIEFjY2VzcyBpbmplY3RlZCBkb2N1bWVudCBpZiBhdmFpbGFibGUgb3IgZmFsbGJhY2sgdG8gZ2xvYmFsIGRvY3VtZW50IHJlZmVyZW5jZSAqL1xuICBwcml2YXRlIF9nZXREb2N1bWVudCgpOiBEb2N1bWVudCB7XG4gICAgcmV0dXJuIHRoaXMuX2RvY3VtZW50IHx8IGRvY3VtZW50O1xuICB9XG5cbiAgLyoqIFVzZSBkZWZhdWx0VmlldyBvZiBpbmplY3RlZCBkb2N1bWVudCBpZiBhdmFpbGFibGUgb3IgZmFsbGJhY2sgdG8gZ2xvYmFsIHdpbmRvdyByZWZlcmVuY2UgKi9cbiAgcHJpdmF0ZSBfZ2V0V2luZG93KCk6IFdpbmRvdyB7XG4gICAgY29uc3QgZG9jID0gdGhpcy5fZ2V0RG9jdW1lbnQoKTtcbiAgICByZXR1cm4gZG9jLmRlZmF1bHRWaWV3IHx8IHdpbmRvdztcbiAgfVxuXG4gIC8qKlxuICAgKiBTY3JvbGxzIGEgdGV4dGFyZWEgdG8gdGhlIGNhcmV0IHBvc2l0aW9uLiBPbiBGaXJlZm94IHJlc2l6aW5nIHRoZSB0ZXh0YXJlYSB3aWxsXG4gICAqIHByZXZlbnQgaXQgZnJvbSBzY3JvbGxpbmcgdG8gdGhlIGNhcmV0IHBvc2l0aW9uLiBXZSBuZWVkIHRvIHJlLXNldCB0aGUgc2VsZWN0aW9uXG4gICAqIGluIG9yZGVyIGZvciBpdCB0byBzY3JvbGwgdG8gdGhlIHByb3BlciBwb3NpdGlvbi5cbiAgICovXG4gIHByaXZhdGUgX3Njcm9sbFRvQ2FyZXRQb3NpdGlvbih0ZXh0YXJlYTogSFRNTFRleHRBcmVhRWxlbWVudCkge1xuICAgIGNvbnN0IHtzZWxlY3Rpb25TdGFydCwgc2VsZWN0aW9uRW5kfSA9IHRleHRhcmVhO1xuICAgIGNvbnN0IGRvY3VtZW50ID0gdGhpcy5fZ2V0RG9jdW1lbnQoKTtcblxuICAgIC8vIElFIHdpbGwgdGhyb3cgYW4gXCJVbnNwZWNpZmllZCBlcnJvclwiIGlmIHdlIHRyeSB0byBzZXQgdGhlIHNlbGVjdGlvbiByYW5nZSBhZnRlciB0aGVcbiAgICAvLyBlbGVtZW50IGhhcyBiZWVuIHJlbW92ZWQgZnJvbSB0aGUgRE9NLiBBc3NlcnQgdGhhdCB0aGUgZGlyZWN0aXZlIGhhc24ndCBiZWVuIGRlc3Ryb3llZFxuICAgIC8vIGJldHdlZW4gdGhlIHRpbWUgd2UgcmVxdWVzdGVkIHRoZSBhbmltYXRpb24gZnJhbWUgYW5kIHdoZW4gaXQgd2FzIGV4ZWN1dGVkLlxuICAgIC8vIEFsc28gbm90ZSB0aGF0IHdlIGhhdmUgdG8gYXNzZXJ0IHRoYXQgdGhlIHRleHRhcmVhIGlzIGZvY3VzZWQgYmVmb3JlIHdlIHNldCB0aGVcbiAgICAvLyBzZWxlY3Rpb24gcmFuZ2UuIFNldHRpbmcgdGhlIHNlbGVjdGlvbiByYW5nZSBvbiBhIG5vbi1mb2N1c2VkIHRleHRhcmVhIHdpbGwgY2F1c2VcbiAgICAvLyBpdCB0byByZWNlaXZlIGZvY3VzIG9uIElFIGFuZCBFZGdlLlxuICAgIGlmICghdGhpcy5fZGVzdHJveWVkLmlzU3RvcHBlZCAmJiBkb2N1bWVudC5hY3RpdmVFbGVtZW50ID09PSB0ZXh0YXJlYSkge1xuICAgICAgdGV4dGFyZWEuc2V0U2VsZWN0aW9uUmFuZ2Uoc2VsZWN0aW9uU3RhcnQsIHNlbGVjdGlvbkVuZCk7XG4gICAgfVxuICB9XG5cbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX21pblJvd3M6IE51bWJlcklucHV0O1xuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfbWF4Um93czogTnVtYmVySW5wdXQ7XG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV9lbmFibGVkOiBCb29sZWFuSW5wdXQ7XG59XG4iXX0=