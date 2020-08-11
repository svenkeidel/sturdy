/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Platform } from '@angular/cdk/platform';
import { Injectable } from '@angular/core';
import * as i0 from "@angular/core";
import * as i1 from "@angular/cdk/platform";
// The InteractivityChecker leans heavily on the ally.js accessibility utilities.
// Methods like `isTabbable` are only covering specific edge-cases for the browsers which are
// supported.
/**
 * Utility for checking the interactivity of an element, such as whether is is focusable or
 * tabbable.
 */
let InteractivityChecker = /** @class */ (() => {
    class InteractivityChecker {
        constructor(_platform) {
            this._platform = _platform;
        }
        /**
         * Gets whether an element is disabled.
         *
         * @param element Element to be checked.
         * @returns Whether the element is disabled.
         */
        isDisabled(element) {
            // This does not capture some cases, such as a non-form control with a disabled attribute or
            // a form control inside of a disabled form, but should capture the most common cases.
            return element.hasAttribute('disabled');
        }
        /**
         * Gets whether an element is visible for the purposes of interactivity.
         *
         * This will capture states like `display: none` and `visibility: hidden`, but not things like
         * being clipped by an `overflow: hidden` parent or being outside the viewport.
         *
         * @returns Whether the element is visible.
         */
        isVisible(element) {
            return hasGeometry(element) && getComputedStyle(element).visibility === 'visible';
        }
        /**
         * Gets whether an element can be reached via Tab key.
         * Assumes that the element has already been checked with isFocusable.
         *
         * @param element Element to be checked.
         * @returns Whether the element is tabbable.
         */
        isTabbable(element) {
            // Nothing is tabbable on the server 😎
            if (!this._platform.isBrowser) {
                return false;
            }
            const frameElement = getFrameElement(getWindow(element));
            if (frameElement) {
                const frameType = frameElement && frameElement.nodeName.toLowerCase();
                // Frame elements inherit their tabindex onto all child elements.
                if (getTabIndexValue(frameElement) === -1) {
                    return false;
                }
                // Webkit and Blink consider anything inside of an <object> element as non-tabbable.
                if ((this._platform.BLINK || this._platform.WEBKIT) && frameType === 'object') {
                    return false;
                }
                // Webkit and Blink disable tabbing to an element inside of an invisible frame.
                if ((this._platform.BLINK || this._platform.WEBKIT) && !this.isVisible(frameElement)) {
                    return false;
                }
            }
            let nodeName = element.nodeName.toLowerCase();
            let tabIndexValue = getTabIndexValue(element);
            if (element.hasAttribute('contenteditable')) {
                return tabIndexValue !== -1;
            }
            if (nodeName === 'iframe') {
                // The frames may be tabbable depending on content, but it's not possibly to reliably
                // investigate the content of the frames.
                return false;
            }
            if (nodeName === 'audio') {
                if (!element.hasAttribute('controls')) {
                    // By default an <audio> element without the controls enabled is not tabbable.
                    return false;
                }
                else if (this._platform.BLINK) {
                    // In Blink <audio controls> elements are always tabbable.
                    return true;
                }
            }
            if (nodeName === 'video') {
                if (!element.hasAttribute('controls') && this._platform.TRIDENT) {
                    // In Trident a <video> element without the controls enabled is not tabbable.
                    return false;
                }
                else if (this._platform.BLINK || this._platform.FIREFOX) {
                    // In Chrome and Firefox <video controls> elements are always tabbable.
                    return true;
                }
            }
            if (nodeName === 'object' && (this._platform.BLINK || this._platform.WEBKIT)) {
                // In all Blink and WebKit based browsers <object> elements are never tabbable.
                return false;
            }
            // In iOS the browser only considers some specific elements as tabbable.
            if (this._platform.WEBKIT && this._platform.IOS && !isPotentiallyTabbableIOS(element)) {
                return false;
            }
            return element.tabIndex >= 0;
        }
        /**
         * Gets whether an element can be focused by the user.
         *
         * @param element Element to be checked.
         * @returns Whether the element is focusable.
         */
        isFocusable(element) {
            // Perform checks in order of left to most expensive.
            // Again, naive approach that does not capture many edge cases and browser quirks.
            return isPotentiallyFocusable(element) && !this.isDisabled(element) && this.isVisible(element);
        }
    }
    InteractivityChecker.ɵprov = i0.ɵɵdefineInjectable({ factory: function InteractivityChecker_Factory() { return new InteractivityChecker(i0.ɵɵinject(i1.Platform)); }, token: InteractivityChecker, providedIn: "root" });
    InteractivityChecker.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    InteractivityChecker.ctorParameters = () => [
        { type: Platform }
    ];
    return InteractivityChecker;
})();
export { InteractivityChecker };
/**
 * Returns the frame element from a window object. Since browsers like MS Edge throw errors if
 * the frameElement property is being accessed from a different host address, this property
 * should be accessed carefully.
 */
function getFrameElement(window) {
    try {
        return window.frameElement;
    }
    catch (_a) {
        return null;
    }
}
/** Checks whether the specified element has any geometry / rectangles. */
function hasGeometry(element) {
    // Use logic from jQuery to check for an invisible element.
    // See https://github.com/jquery/jquery/blob/master/src/css/hiddenVisibleSelectors.js#L12
    return !!(element.offsetWidth || element.offsetHeight ||
        (typeof element.getClientRects === 'function' && element.getClientRects().length));
}
/** Gets whether an element's  */
function isNativeFormElement(element) {
    let nodeName = element.nodeName.toLowerCase();
    return nodeName === 'input' ||
        nodeName === 'select' ||
        nodeName === 'button' ||
        nodeName === 'textarea';
}
/** Gets whether an element is an `<input type="hidden">`. */
function isHiddenInput(element) {
    return isInputElement(element) && element.type == 'hidden';
}
/** Gets whether an element is an anchor that has an href attribute. */
function isAnchorWithHref(element) {
    return isAnchorElement(element) && element.hasAttribute('href');
}
/** Gets whether an element is an input element. */
function isInputElement(element) {
    return element.nodeName.toLowerCase() == 'input';
}
/** Gets whether an element is an anchor element. */
function isAnchorElement(element) {
    return element.nodeName.toLowerCase() == 'a';
}
/** Gets whether an element has a valid tabindex. */
function hasValidTabIndex(element) {
    if (!element.hasAttribute('tabindex') || element.tabIndex === undefined) {
        return false;
    }
    let tabIndex = element.getAttribute('tabindex');
    // IE11 parses tabindex="" as the value "-32768"
    if (tabIndex == '-32768') {
        return false;
    }
    return !!(tabIndex && !isNaN(parseInt(tabIndex, 10)));
}
/**
 * Returns the parsed tabindex from the element attributes instead of returning the
 * evaluated tabindex from the browsers defaults.
 */
function getTabIndexValue(element) {
    if (!hasValidTabIndex(element)) {
        return null;
    }
    // See browser issue in Gecko https://bugzilla.mozilla.org/show_bug.cgi?id=1128054
    const tabIndex = parseInt(element.getAttribute('tabindex') || '', 10);
    return isNaN(tabIndex) ? -1 : tabIndex;
}
/** Checks whether the specified element is potentially tabbable on iOS */
function isPotentiallyTabbableIOS(element) {
    let nodeName = element.nodeName.toLowerCase();
    let inputType = nodeName === 'input' && element.type;
    return inputType === 'text'
        || inputType === 'password'
        || nodeName === 'select'
        || nodeName === 'textarea';
}
/**
 * Gets whether an element is potentially focusable without taking current visible/disabled state
 * into account.
 */
function isPotentiallyFocusable(element) {
    // Inputs are potentially focusable *unless* they're type="hidden".
    if (isHiddenInput(element)) {
        return false;
    }
    return isNativeFormElement(element) ||
        isAnchorWithHref(element) ||
        element.hasAttribute('contenteditable') ||
        hasValidTabIndex(element);
}
/** Gets the parent window of a DOM node with regards of being inside of an iframe. */
function getWindow(node) {
    // ownerDocument is null if `node` itself *is* a document.
    return node.ownerDocument && node.ownerDocument.defaultView || window;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW50ZXJhY3Rpdml0eS1jaGVja2VyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9hMTF5L2ludGVyYWN0aXZpdHktY2hlY2tlci9pbnRlcmFjdGl2aXR5LWNoZWNrZXIudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQy9DLE9BQU8sRUFBQyxVQUFVLEVBQUMsTUFBTSxlQUFlLENBQUM7OztBQUd6QyxpRkFBaUY7QUFDakYsNkZBQTZGO0FBQzdGLGFBQWE7QUFFYjs7O0dBR0c7QUFDSDtJQUFBLE1BQ2Esb0JBQW9CO1FBRS9CLFlBQW9CLFNBQW1CO1lBQW5CLGNBQVMsR0FBVCxTQUFTLENBQVU7UUFBRyxDQUFDO1FBRTNDOzs7OztXQUtHO1FBQ0gsVUFBVSxDQUFDLE9BQW9CO1lBQzdCLDRGQUE0RjtZQUM1RixzRkFBc0Y7WUFDdEYsT0FBTyxPQUFPLENBQUMsWUFBWSxDQUFDLFVBQVUsQ0FBQyxDQUFDO1FBQzFDLENBQUM7UUFFRDs7Ozs7OztXQU9HO1FBQ0gsU0FBUyxDQUFDLE9BQW9CO1lBQzVCLE9BQU8sV0FBVyxDQUFDLE9BQU8sQ0FBQyxJQUFJLGdCQUFnQixDQUFDLE9BQU8sQ0FBQyxDQUFDLFVBQVUsS0FBSyxTQUFTLENBQUM7UUFDcEYsQ0FBQztRQUVEOzs7Ozs7V0FNRztRQUNILFVBQVUsQ0FBQyxPQUFvQjtZQUM3Qix1Q0FBdUM7WUFDdkMsSUFBSSxDQUFDLElBQUksQ0FBQyxTQUFTLENBQUMsU0FBUyxFQUFFO2dCQUM3QixPQUFPLEtBQUssQ0FBQzthQUNkO1lBRUQsTUFBTSxZQUFZLEdBQUcsZUFBZSxDQUFDLFNBQVMsQ0FBQyxPQUFPLENBQUMsQ0FBQyxDQUFDO1lBRXpELElBQUksWUFBWSxFQUFFO2dCQUNoQixNQUFNLFNBQVMsR0FBRyxZQUFZLElBQUksWUFBWSxDQUFD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SUFBSSxTQUFTLEdBQUcsUUFBUSxLQUFLLE9BQU8sSUFBSyxPQUE0QixDQUFDLElBQUksQ0FBQztJQUUzRSxPQUFPLFNBQVMsS0FBSyxNQUFNO1dBQ3BCLFNBQVMsS0FBSyxVQUFVO1dBQ3hCLFFBQVEsS0FBSyxRQUFRO1dBQ3JCLFFBQVEsS0FBSyxVQUFVLENBQUM7QUFDakMsQ0FBQztBQUVEOzs7R0FHRztBQUNILFNBQVMsc0JBQXNCLENBQUMsT0FBb0I7SUFDbEQsbUVBQW1FO0lBQ25FLElBQUksYUFBYSxDQUFDLE9BQU8sQ0FBQyxFQUFFO1FBQzFCLE9BQU8sS0FBSyxDQUFDO0tBQ2Q7SUFFRCxPQUFPLG1CQUFtQixDQUFDLE9BQU8sQ0FBQztRQUMvQixnQkFBZ0IsQ0FBQyxPQUFPLENBQUM7UUFDekIsT0FBTyxDQUFDLFlBQVksQ0FBQyxpQkFBaUIsQ0FBQztRQUN2QyxnQkFBZ0IsQ0FBQyxPQUFPLENBQUMsQ0FBQztBQUNoQyxDQUFDO0FBRUQsc0ZBQXNGO0FBQ3RGLFNBQVMsU0FBUyxDQUFDLElBQWlCO0lBQ2xDLDBEQUEwRDtJQUMxRCxPQUFPLElBQUksQ0FBQyxhQUFhLElBQUksSUFBSSxDQUFDLGFBQWEsQ0FBQyxXQUFXLElBQUksTUFBTSxDQUFDO0FBQ3hFLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtQbGF0Zm9ybX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3BsYXRmb3JtJztcbmltcG9ydCB7SW5qZWN0YWJsZX0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5cblxuLy8gVGhlIEludGVyYWN0aXZpdHlDaGVja2VyIGxlYW5zIGhlYXZpbHkgb24gdGhlIGFsbHkuanMgYWNjZXNzaWJpbGl0eSB1dGlsaXRpZXMuXG4vLyBNZXRob2RzIGxpa2UgYGlzVGFiYmFibGVgIGFyZSBvbmx5IGNvdmVyaW5nIHNwZWNpZmljIGVkZ2UtY2FzZXMgZm9yIHRoZSBicm93c2VycyB3aGljaCBhcmVcbi8vIHN1cHBvcnRlZC5cblxuLyoqXG4gKiBVdGlsaXR5IGZvciBjaGVja2luZyB0aGUgaW50ZXJhY3Rpdml0eSBvZiBhbiBlbGVtZW50LCBzdWNoIGFzIHdoZXRoZXIgaXMgaXMgZm9jdXNhYmxlIG9yXG4gKiB0YWJiYWJsZS5cbiAqL1xuQEluamVjdGFibGUoe3Byb3ZpZGVkSW46ICdyb290J30pXG5leHBvcnQgY2xhc3MgSW50ZXJhY3Rpdml0eUNoZWNrZXIge1xuXG4gIGNvbnN0cnVjdG9yKHByaXZhdGUgX3BsYXRmb3JtOiBQbGF0Zm9ybSkge31cblxuICAvKipcbiAgICogR2V0cyB3aGV0aGVyIGFuIGVsZW1lbnQgaXMgZGlzYWJsZWQuXG4gICAqXG4gICAqIEBwYXJhbSBlbGVtZW50IEVsZW1lbnQgdG8gYmUgY2hlY2tlZC5cbiAgICogQHJldHVybnMgV2hldGhlciB0aGUgZWxlbWVudCBpcyBkaXNhYmxlZC5cbiAgICovXG4gIGlzRGlzYWJsZWQoZWxlbWVudDogSFRNTEVsZW1lbnQpOiBib29sZWFuIHtcbiAgICAvLyBUaGlzIGRvZXMgbm90IGNhcHR1cmUgc29tZSBjYXNlcywgc3VjaCBhcyBhIG5vbi1mb3JtIGNvbnRyb2wgd2l0aCBhIGRpc2FibGVkIGF0dHJpYnV0ZSBvclxuICAgIC8vIGEgZm9ybSBjb250cm9sIGluc2lkZSBvZiBhIGRpc2FibGVkIGZvcm0sIGJ1dCBzaG91bGQgY2FwdHVyZSB0aGUgbW9zdCBjb21tb24gY2FzZXMuXG4gICAgcmV0dXJuIGVsZW1lbnQuaGFzQXR0cmlidXRlKCdkaXNhYmxlZCcpO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgd2hldGhlciBhbiBlbGVtZW50IGlzIHZpc2libGUgZm9yIHRoZSBwdXJwb3NlcyBvZiBpbnRlcmFjdGl2aXR5LlxuICAgKlxuICAgKiBUaGlzIHdpbGwgY2FwdHVyZSBzdGF0ZXMgbGlrZSBgZGlzcGxheTogbm9uZWAgYW5kIGB2aXNpYmlsaXR5OiBoaWRkZW5gLCBidXQgbm90IHRoaW5ncyBsaWtlXG4gICAqIGJlaW5nIGNsaXBwZWQgYnkgYW4gYG92ZXJmbG93OiBoaWRkZW5gIHBhcmVudCBvciBiZWluZyBvdXRzaWRlIHRoZSB2aWV3cG9ydC5cbiAgICpcbiAgICogQHJldHVybnMgV2hldGhlciB0aGUgZWxlbWVudCBpcyB2aXNpYmxlLlxuICAgKi9cbiAgaXNWaXNpYmxlKGVsZW1lbnQ6IEhUTUxFbGVtZW50KTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIGhhc0dlb21ldHJ5KGVsZW1lbnQpICYmIGdldENvbXB1dGVkU3R5bGUoZWxlbWVudCkudmlzaWJpbGl0eSA9PT0gJ3Zpc2libGUnO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgd2hldGhlciBhbiBlbGVtZW50IGNhbiBiZSByZWFjaGVkIHZpYSBUYWIga2V5LlxuICAgKiBBc3N1bWVzIHRoYXQgdGhlIGVsZW1lbnQgaGFzIGFscmVhZHkgYmVlbiBjaGVja2VkIHdpdGggaXNGb2N1c2FibGUuXG4gICAqXG4gICAqIEBwYXJhbSBlbGVtZW50IEVsZW1lbnQgdG8gYmUgY2hlY2tlZC5cbiAgICogQHJldHVybnMgV2hldGhlciB0aGUgZWxlbWVudCBpcyB0YWJiYWJsZS5cbiAgICovXG4gIGlzVGFiYmFibGUoZWxlbWVudDogSFRNTEVsZW1lbnQpOiBib29sZWFuIHtcbiAgICAvLyBOb3RoaW5nIGlzIHRhYmJhYmxlIG9uIHRoZSBzZXJ2ZXIg8J+YjlxuICAgIGlmICghdGhpcy5fcGxhdGZvcm0uaXNCcm93c2VyKSB7XG4gICAgICByZXR1cm4gZmFsc2U7XG4gICAgfVxuXG4gICAgY29uc3QgZnJhbWVFbGVtZW50ID0gZ2V0RnJhbWVFbGVtZW50KGdldFdpbmRvdyhlbGVtZW50KSk7XG5cbiAgICBpZiAoZnJhbWVFbGVtZW50KSB7XG4gICAgICBjb25zdCBmcmFtZVR5cGUgPSBmcmFtZUVsZW1lbnQgJiYgZnJhbWVFbGVtZW50Lm5vZGVOYW1lLnRvTG93ZXJDYXNlKCk7XG5cbiAgICAgIC8vIEZyYW1lIGVsZW1lbnRzIGluaGVyaXQgdGhlaXIgdGFiaW5kZXggb250byBhbGwgY2hpbGQgZWxlbWVudHMuXG4gICAgICBpZiAoZ2V0VGFiSW5kZXhWYWx1ZShmcmFtZUVsZW1lbnQpID09PSAtMSkge1xuICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICB9XG5cbiAgICAgIC8vIFdlYmtpdCBhbmQgQmxpbmsgY29uc2lkZXIgYW55dGhpbmcgaW5zaWRlIG9mIGFuIDxvYmplY3Q+IGVsZW1lbnQgYXMgbm9uLXRhYmJhYmxlLlxuICAgICAgaWYgKCh0aGlzLl9wbGF0Zm9ybS5CTElOSyB8fCB0aGlzLl9wbGF0Zm9ybS5XRUJLSVQpICYmIGZyYW1lVHlwZSA9PT0gJ29iamVjdCcpIHtcbiAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgfVxuXG4gICAgICAvLyBXZWJraXQgYW5kIEJsaW5rIGRpc2FibGUgdGFiYmluZyB0byBhbiBlbGVtZW50IGluc2lkZSBvZiBhbiBpbnZpc2libGUgZnJhbWUuXG4gICAgICBpZiAoKHRoaXMuX3BsYXRmb3JtLkJMSU5LIHx8IHRoaXMuX3BsYXRmb3JtLldFQktJVCkgJiYgIXRoaXMuaXNWaXNpYmxlKGZyYW1lRWxlbWVudCkpIHtcbiAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgfVxuXG4gICAgfVxuXG4gICAgbGV0IG5vZGVOYW1lID0gZWxlbWVudC5ub2RlTmFtZS50b0xvd2VyQ2FzZSgpO1xuICAgIGxldCB0YWJJbmRleFZhbHVlID0gZ2V0VGFiSW5kZXhWYWx1ZShlbGVtZW50KTtcblxuICAgIGlmIChlbGVtZW50Lmhhc0F0dHJpYnV0ZSgnY29udGVudGVkaXRhYmxlJykpIHtcbiAgICAgIHJldHVybiB0YWJJbmRleFZhbHVlICE9PSAtMTtcbiAgICB9XG5cbiAgICBpZiAobm9kZU5hbWUgPT09ICdpZnJhbWUnKSB7XG4gICAgICAvLyBUaGUgZnJhbWVzIG1heSBiZSB0YWJiYWJsZSBkZXBlbmRpbmcgb24gY29udGVudCwgYnV0IGl0J3Mgbm90IHBvc3NpYmx5IHRvIHJlbGlhYmx5XG4gICAgICAvLyBpbnZlc3RpZ2F0ZSB0aGUgY29udGVudCBvZiB0aGUgZnJhbWVzLlxuICAgICAgcmV0dXJuIGZhbHNlO1xuICAgIH1cblxuICAgIGlmIChub2RlTmFtZSA9PT0gJ2F1ZGlvJykge1xuICAgICAgaWYgKCFlbGVtZW50Lmhhc0F0dHJpYnV0ZSgnY29udHJvbHMnKSkge1xuICAgICAgICAvLyBCeSBkZWZhdWx0IGFuIDxhdWRpbz4gZWxlbWVudCB3aXRob3V0IHRoZSBjb250cm9scyBlbmFibGVkIGlzIG5vdCB0YWJiYWJsZS5cbiAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgfSBlbHNlIGlmICh0aGlzLl9wbGF0Zm9ybS5CTElOSykge1xuICAgICAgICAvLyBJbiBCbGluayA8YXVkaW8gY29udHJvbHM+IGVsZW1lbnRzIGFyZSBhbHdheXMgdGFiYmFibGUuXG4gICAgICAgIHJldHVybiB0cnVlO1xuICAgICAgfVxuICAgIH1cblxuICAgIGlmIChub2RlTmFtZSA9PT0gJ3ZpZGVvJykge1xuICAgICAgaWYgKCFlbGVtZW50Lmhhc0F0dHJpYnV0ZSgnY29udHJvbHMnKSAmJiB0aGlzLl9wbGF0Zm9ybS5UUklERU5UKSB7XG4gICAgICAgIC8vIEluIFRyaWRlbnQgYSA8dmlkZW8+IGVsZW1lbnQgd2l0aG91dCB0aGUgY29udHJvbHMgZW5hYmxlZCBpcyBub3QgdGFiYmFibGUuXG4gICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgIH0gZWxzZSBpZiAodGhpcy5fcGxhdGZvcm0uQkxJTksgfHwgdGhpcy5fcGxhdGZvcm0uRklSRUZPWCkge1xuICAgICAgICAvLyBJbiBDaHJvbWUgYW5kIEZpcmVmb3ggPHZpZGVvIGNvbnRyb2xzPiBlbGVtZW50cyBhcmUgYWx3YXlzIHRhYmJhYmxlLlxuICAgICAgICByZXR1cm4gdHJ1ZTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICBpZiAobm9kZU5hbWUgPT09ICdvYmplY3QnICYmICh0aGlzLl9wbGF0Zm9ybS5CTElOSyB8fCB0aGlzLl9wbGF0Zm9ybS5XRUJLSVQpKSB7XG4gICAgICAvLyBJbiBhbGwgQmxpbmsgYW5kIFdlYktpdCBiYXNlZCBicm93c2VycyA8b2JqZWN0PiBlbGVtZW50cyBhcmUgbmV2ZXIgdGFiYmFibGUuXG4gICAgICByZXR1cm4gZmFsc2U7XG4gICAgfVxuXG4gICAgLy8gSW4gaU9TIHRoZSBicm93c2VyIG9ubHkgY29uc2lkZXJzIHNvbWUgc3BlY2lmaWMgZWxlbWVudHMgYXMgdGFiYmFibGUuXG4gICAgaWYgKHRoaXMuX3BsYXRmb3JtLldFQktJVCAmJiB0aGlzLl9wbGF0Zm9ybS5JT1MgJiYgIWlzUG90ZW50aWFsbHlUYWJiYWJsZUlPUyhlbGVtZW50KSkge1xuICAgICAgcmV0dXJuIGZhbHNlO1xuICAgIH1cblxuICAgIHJldHVybiBlbGVtZW50LnRhYkluZGV4ID49IDA7XG4gIH1cblxuICAvKipcbiAgICogR2V0cyB3aGV0aGVyIGFuIGVsZW1lbnQgY2FuIGJlIGZvY3VzZWQgYnkgdGhlIHVzZXIuXG4gICAqXG4gICAqIEBwYXJhbSBlbGVtZW50IEVsZW1lbnQgdG8gYmUgY2hlY2tlZC5cbiAgICogQHJldHVybnMgV2hldGhlciB0aGUgZWxlbWVudCBpcyBmb2N1c2FibGUuXG4gICAqL1xuICBpc0ZvY3VzYWJsZShlbGVtZW50OiBIVE1MRWxlbWVudCk6IGJvb2xlYW4ge1xuICAgIC8vIFBlcmZvcm0gY2hlY2tzIGluIG9yZGVyIG9mIGxlZnQgdG8gbW9zdCBleHBlbnNpdmUuXG4gICAgLy8gQWdhaW4sIG5haXZlIGFwcHJvYWNoIHRoYXQgZG9lcyBub3QgY2FwdHVyZSBtYW55IGVkZ2UgY2FzZXMgYW5kIGJyb3dzZXIgcXVpcmtzLlxuICAgIHJldHVybiBpc1BvdGVudGlhbGx5Rm9jdXNhYmxlKGVsZW1lbnQpICYmICF0aGlzLmlzRGlzYWJsZWQoZWxlbWVudCkgJiYgdGhpcy5pc1Zpc2libGUoZWxlbWVudCk7XG4gIH1cblxufVxuXG4vKipcbiAqIFJldHVybnMgdGhlIGZyYW1lIGVsZW1lbnQgZnJvbSBhIHdpbmRvdyBvYmplY3QuIFNpbmNlIGJyb3dzZXJzIGxpa2UgTVMgRWRnZSB0aHJvdyBlcnJvcnMgaWZcbiAqIHRoZSBmcmFtZUVsZW1lbnQgcHJvcGVydHkgaXMgYmVpbmcgYWNjZXNzZWQgZnJvbSBhIGRpZmZlcmVudCBob3N0IGFkZHJlc3MsIHRoaXMgcHJvcGVydHlcbiAqIHNob3VsZCBiZSBhY2Nlc3NlZCBjYXJlZnVsbHkuXG4gKi9cbmZ1bmN0aW9uIGdldEZyYW1lRWxlbWVudCh3aW5kb3c6IFdpbmRvdykge1xuICB0cnkge1xuICAgIHJldHVybiB3aW5kb3cuZnJhbWVFbGVtZW50IGFzIEhUTUxFbGVtZW50O1xuICB9IGNhdGNoIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxufVxuXG4vKiogQ2hlY2tzIHdoZXRoZXIgdGhlIHNwZWNpZmllZCBlbGVtZW50IGhhcyBhbnkgZ2VvbWV0cnkgLyByZWN0YW5nbGVzLiAqL1xuZnVuY3Rpb24gaGFzR2VvbWV0cnkoZWxlbWVudDogSFRNTEVsZW1lbnQpOiBib29sZWFuIHtcbiAgLy8gVXNlIGxvZ2ljIGZyb20galF1ZXJ5IHRvIGNoZWNrIGZvciBhbiBpbnZpc2libGUgZWxlbWVudC5cbiAgLy8gU2VlIGh0dHBzOi8vZ2l0aHViLmNvbS9qcXVlcnkvanF1ZXJ5L2Jsb2IvbWFzdGVyL3NyYy9jc3MvaGlkZGVuVmlzaWJsZVNlbGVjdG9ycy5qcyNMMTJcbiAgcmV0dXJuICEhKGVsZW1lbnQub2Zmc2V0V2lkdGggfHwgZWxlbWVudC5vZmZzZXRIZWlnaHQgfHxcbiAgICAgICh0eXBlb2YgZWxlbWVudC5nZXRDbGllbnRSZWN0cyA9PT0gJ2Z1bmN0aW9uJyAmJiBlbGVtZW50LmdldENsaWVudFJlY3RzKCkubGVuZ3RoKSk7XG59XG5cbi8qKiBHZXRzIHdoZXRoZXIgYW4gZWxlbWVudCdzICAqL1xuZnVuY3Rpb24gaXNOYXRpdmVGb3JtRWxlbWVudChlbGVtZW50OiBOb2RlKSB7XG4gIGxldCBub2RlTmFtZSA9IGVsZW1lbnQubm9kZU5hbWUudG9Mb3dlckNhc2UoKTtcbiAgcmV0dXJuIG5vZGVOYW1lID09PSAnaW5wdXQnIHx8XG4gICAgICBub2RlTmFtZSA9PT0gJ3NlbGVjdCcgfHxcbiAgICAgIG5vZGVOYW1lID09PSAnYnV0dG9uJyB8fFxuICAgICAgbm9kZU5hbWUgPT09ICd0ZXh0YXJlYSc7XG59XG5cbi8qKiBHZXRzIHdoZXRoZXIgYW4gZWxlbWVudCBpcyBhbiBgPGlucHV0IHR5cGU9XCJoaWRkZW5cIj5gLiAqL1xuZnVuY3Rpb24gaXNIaWRkZW5JbnB1dChlbGVtZW50OiBIVE1MRWxlbWVudCk6IGJvb2xlYW4ge1xuICByZXR1cm4gaXNJbnB1dEVsZW1lbnQoZWxlbWVudCkgJiYgZWxlbWVudC50eXBlID09ICdoaWRkZW4nO1xufVxuXG4vKiogR2V0cyB3aGV0aGVyIGFuIGVsZW1lbnQgaXMgYW4gYW5jaG9yIHRoYXQgaGFzIGFuIGhyZWYgYXR0cmlidXRlLiAqL1xuZnVuY3Rpb24gaXNBbmNob3JXaXRoSHJlZihlbGVtZW50OiBIVE1MRWxlbWVudCk6IGJvb2xlYW4ge1xuICByZXR1cm4gaXNBbmNob3JFbGVtZW50KGVsZW1lbnQpICYmIGVsZW1lbnQuaGFzQXR0cmlidXRlKCdocmVmJyk7XG59XG5cbi8qKiBHZXRzIHdoZXRoZXIgYW4gZWxlbWVudCBpcyBhbiBpbnB1dCBlbGVtZW50LiAqL1xuZnVuY3Rpb24gaXNJbnB1dEVsZW1lbnQoZWxlbWVudDogSFRNTEVsZW1lbnQpOiBlbGVtZW50IGlzIEhUTUxJbnB1dEVsZW1lbnQge1xuICByZXR1cm4gZWxlbWVudC5ub2RlTmFtZS50b0xvd2VyQ2FzZSgpID09ICdpbnB1dCc7XG59XG5cbi8qKiBHZXRzIHdoZXRoZXIgYW4gZWxlbWVudCBpcyBhbiBhbmNob3IgZWxlbWVudC4gKi9cbmZ1bmN0aW9uIGlzQW5jaG9yRWxlbWVudChlbGVtZW50OiBIVE1MRWxlbWVudCk6IGVsZW1lbnQgaXMgSFRNTEFuY2hvckVsZW1lbnQge1xuICByZXR1cm4gZWxlbWVudC5ub2RlTmFtZS50b0xvd2VyQ2FzZSgpID09ICdhJztcbn1cblxuLyoqIEdldHMgd2hldGhlciBhbiBlbGVtZW50IGhhcyBhIHZhbGlkIHRhYmluZGV4LiAqL1xuZnVuY3Rpb24gaGFzVmFsaWRUYWJJbmRleChlbGVtZW50OiBIVE1MRWxlbWVudCk6IGJvb2xlYW4ge1xuICBpZiAoIWVsZW1lbnQuaGFzQXR0cmlidXRlKCd0YWJpbmRleCcpIHx8IGVsZW1lbnQudGFiSW5kZXggPT09IHVuZGVmaW5lZCkge1xuICAgIHJldHVybiBmYWxzZTtcbiAgfVxuXG4gIGxldCB0YWJJbmRleCA9IGVsZW1lbnQuZ2V0QXR0cmlidXRlKCd0YWJpbmRleCcpO1xuXG4gIC8vIElFMTEgcGFyc2VzIHRhYmluZGV4PVwiXCIgYXMgdGhlIHZhbHVlIFwiLTMyNzY4XCJcbiAgaWYgKHRhYkluZGV4ID09ICctMzI3NjgnKSB7XG4gICAgcmV0dXJuIGZhbHNlO1xuICB9XG5cbiAgcmV0dXJuICEhKHRhYkluZGV4ICYmICFpc05hTihwYXJzZUludCh0YWJJbmRleCwgMTApKSk7XG59XG5cbi8qKlxuICogUmV0dXJucyB0aGUgcGFyc2VkIHRhYmluZGV4IGZyb20gdGhlIGVsZW1lbnQgYXR0cmlidXRlcyBpbnN0ZWFkIG9mIHJldHVybmluZyB0aGVcbiAqIGV2YWx1YXRlZCB0YWJpbmRleCBmcm9tIHRoZSBicm93c2VycyBkZWZhdWx0cy5cbiAqL1xuZnVuY3Rpb24gZ2V0VGFiSW5kZXhWYWx1ZShlbGVtZW50OiBIVE1MRWxlbWVudCk6IG51bWJlciB8IG51bGwge1xuICBpZiAoIWhhc1ZhbGlkVGFiSW5kZXgoZWxlbWVudCkpIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuXG4gIC8vIFNlZSBicm93c2VyIGlzc3VlIGluIEdlY2tvIGh0dHBzOi8vYnVnemlsbGEubW96aWxsYS5vcmcvc2hvd19idWcuY2dpP2lkPTExMjgwNTRcbiAgY29uc3QgdGFiSW5kZXggPSBwYXJzZUludChlbGVtZW50LmdldEF0dHJpYnV0ZSgndGFiaW5kZXgnKSB8fCAnJywgMTApO1xuXG4gIHJldHVybiBpc05hTih0YWJJbmRleCkgPyAtMSA6IHRhYkluZGV4O1xufVxuXG4vKiogQ2hlY2tzIHdoZXRoZXIgdGhlIHNwZWNpZmllZCBlbGVtZW50IGlzIHBvdGVudGlhbGx5IHRhYmJhYmxlIG9uIGlPUyAqL1xuZnVuY3Rpb24gaXNQb3RlbnRpYWxseVRhYmJhYmxlSU9TKGVsZW1lbnQ6IEhUTUxFbGVtZW50KTogYm9vbGVhbiB7XG4gIGxldCBub2RlTmFtZSA9IGVsZW1lbnQubm9kZU5hbWUudG9Mb3dlckNhc2UoKTtcbiAgbGV0IGlucHV0VHlwZSA9IG5vZGVOYW1lID09PSAnaW5wdXQnICYmIChlbGVtZW50IGFzIEhUTUxJbnB1dEVsZW1lbnQpLnR5cGU7XG5cbiAgcmV0dXJuIGlucHV0VHlwZSA9PT0gJ3RleHQnXG4gICAgICB8fCBpbnB1dFR5cGUgPT09ICdwYXNzd29yZCdcbiAgICAgIHx8IG5vZGVOYW1lID09PSAnc2VsZWN0J1xuICAgICAgfHwgbm9kZU5hbWUgPT09ICd0ZXh0YXJlYSc7XG59XG5cbi8qKlxuICogR2V0cyB3aGV0aGVyIGFuIGVsZW1lbnQgaXMgcG90ZW50aWFsbHkgZm9jdXNhYmxlIHdpdGhvdXQgdGFraW5nIGN1cnJlbnQgdmlzaWJsZS9kaXNhYmxlZCBzdGF0ZVxuICogaW50byBhY2NvdW50LlxuICovXG5mdW5jdGlvbiBpc1BvdGVudGlhbGx5Rm9jdXNhYmxlKGVsZW1lbnQ6IEhUTUxFbGVtZW50KTogYm9vbGVhbiB7XG4gIC8vIElucHV0cyBhcmUgcG90ZW50aWFsbHkgZm9jdXNhYmxlICp1bmxlc3MqIHRoZXkncmUgdHlwZT1cImhpZGRlblwiLlxuICBpZiAoaXNIaWRkZW5JbnB1dChlbGVtZW50KSkge1xuICAgIHJldHVybiBmYWxzZTtcbiAgfVxuXG4gIHJldHVybiBpc05hdGl2ZUZvcm1FbGVtZW50KGVsZW1lbnQpIHx8XG4gICAgICBpc0FuY2hvcldpdGhIcmVmKGVsZW1lbnQpIHx8XG4gICAgICBlbGVtZW50Lmhhc0F0dHJpYnV0ZSgnY29udGVudGVkaXRhYmxlJykgfHxcbiAgICAgIGhhc1ZhbGlkVGFiSW5kZXgoZWxlbWVudCk7XG59XG5cbi8qKiBHZXRzIHRoZSBwYXJlbnQgd2luZG93IG9mIGEgRE9NIG5vZGUgd2l0aCByZWdhcmRzIG9mIGJlaW5nIGluc2lkZSBvZiBhbiBpZnJhbWUuICovXG5mdW5jdGlvbiBnZXRXaW5kb3cobm9kZTogSFRNTEVsZW1lbnQpOiBXaW5kb3cge1xuICAvLyBvd25lckRvY3VtZW50IGlzIG51bGwgaWYgYG5vZGVgIGl0c2VsZiAqaXMqIGEgZG9jdW1lbnQuXG4gIHJldHVybiBub2RlLm93bmVyRG9jdW1lbnQgJiYgbm9kZS5vd25lckRvY3VtZW50LmRlZmF1bHRWaWV3IHx8IHdpbmRvdztcbn1cbiJdfQ==