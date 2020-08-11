/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { DOCUMENT } from '@angular/common';
import { Inject, Injectable } from '@angular/core';
import { addAriaReferencedId, getAriaReferenceIds, removeAriaReferencedId } from './aria-reference';
import * as i0 from "@angular/core";
import * as i1 from "@angular/common";
/** ID used for the body container where all messages are appended. */
export const MESSAGES_CONTAINER_ID = 'cdk-describedby-message-container';
/** ID prefix used for each created message element. */
export const CDK_DESCRIBEDBY_ID_PREFIX = 'cdk-describedby-message';
/** Attribute given to each host element that is described by a message element. */
export const CDK_DESCRIBEDBY_HOST_ATTRIBUTE = 'cdk-describedby-host';
/** Global incremental identifier for each registered message element. */
let nextId = 0;
/** Global map of all registered message elements that have been placed into the document. */
const messageRegistry = new Map();
/** Container for all registered messages. */
let messagesContainer = null;
/**
 * Utility that creates visually hidden elements with a message content. Useful for elements that
 * want to use aria-describedby to further describe themselves without adding additional visual
 * content.
 */
let AriaDescriber = /** @class */ (() => {
    class AriaDescriber {
        constructor(_document) {
            this._document = _document;
        }
        /**
         * Adds to the host element an aria-describedby reference to a hidden element that contains
         * the message. If the same message has already been registered, then it will reuse the created
         * message element.
         */
        describe(hostElement, message) {
            if (!this._canBeDescribed(hostElement, message)) {
                return;
            }
            if (typeof message !== 'string') {
                // We need to ensure that the element has an ID.
                this._setMessageId(message);
                messageRegistry.set(message, { messageElement: message, referenceCount: 0 });
            }
            else if (!messageRegistry.has(message)) {
                this._createMessageElement(message);
            }
            if (!this._isElementDescribedByMessage(hostElement, message)) {
                this._addMessageReference(hostElement, message);
            }
        }
        /** Removes the host element's aria-describedby reference to the message element. */
        removeDescription(hostElement, message) {
            if (!this._isElementNode(hostElement)) {
                return;
            }
            if (this._isElementDescribedByMessage(hostElement, message)) {
                this._removeMessageReference(hostElement, message);
            }
            // If the message is a string, it means that it's one that we created for the
            // consumer so we can remove it safely, otherwise we should leave it in place.
            if (typeof message === 'string') {
                const registeredMessage = messageRegistry.get(message);
                if (registeredMessage && registeredMessage.referenceCount === 0) {
                    this._deleteMessageElement(message);
                }
            }
            if (messagesContainer && messagesContainer.childNodes.length === 0) {
                this._deleteMessagesContainer();
            }
        }
        /** Unregisters all created message elements and removes the message container. */
        ngOnDestroy() {
            const describedElements = this._document.querySelectorAll(`[${CDK_DESCRIBEDBY_HOST_ATTRIBUTE}]`);
            for (let i = 0; i < describedElements.length; i++) {
                this._removeCdkDescribedByReferenceIds(describedElements[i]);
                describedElements[i].removeAttribute(CDK_DESCRIBEDBY_HOST_ATTRIBUTE);
            }
            if (messagesContainer) {
                this._deleteMessagesContainer();
            }
            messageRegistry.clear();
        }
        /**
         * Creates a new element in the visually hidden message container element with the message
         * as its content and adds it to the message registry.
         */
        _createMessageElement(message) {
            const messageElement = this._document.createElement('div');
            this._setMessageId(messageElement);
            messageElement.textContent = message;
            this._createMessagesContainer();
            messagesContainer.appendChild(messageElement);
            messageRegistry.set(message, { messageElement, referenceCount: 0 });
        }
        /** Assigns a unique ID to an element, if it doesn't have one already. */
        _setMessageId(element) {
            if (!element.id) {
                element.id = `${CDK_DESCRIBEDBY_ID_PREFIX}-${nextId++}`;
            }
        }
        /** Deletes the message element from the global messages container. */
        _deleteMessageElement(message) {
            const registeredMessage = messageRegistry.get(message);
            const messageElement = registeredMessage && registeredMessage.messageElement;
            if (messagesContainer && messageElement) {
                messagesContainer.removeChild(messageElement);
            }
            messageRegistry.delete(message);
        }
        /** Creates the global container for all aria-describedby messages. */
        _createMessagesContainer() {
            if (!messagesContainer) {
                const preExistingContainer = this._document.getElementById(MESSAGES_CONTAINER_ID);
                // When going from the server to the client, we may end up in a situation where there's
                // already a container on the page, but we don't have a reference to it. Clear the
                // old container so we don't get duplicates. Doing this, instead of emptying the previous
                // container, should be slightly faster.
                if (preExistingContainer) {
                    preExistingContainer.parentNode.removeChild(preExistingContainer);
                }
                messagesContainer = this._document.createElement('div');
                messagesContainer.id = MESSAGES_CONTAINER_ID;
                messagesContainer.setAttribute('aria-hidden', 'true');
                messagesContainer.style.display = 'none';
                this._document.body.appendChild(messagesContainer);
            }
        }
        /** Deletes the global messages container. */
        _deleteMessagesContainer() {
            if (messagesContainer && messagesContainer.parentNode) {
                messagesContainer.parentNode.removeChild(messagesContainer);
                messagesContainer = null;
            }
        }
        /** Removes all cdk-describedby messages that are hosted through the element. */
        _removeCdkDescribedByReferenceIds(element) {
            // Remove all aria-describedby reference IDs that are prefixed by CDK_DESCRIBEDBY_ID_PREFIX
            const originalReferenceIds = getAriaReferenceIds(element, 'aria-describedby')
                .filter(id => id.indexOf(CDK_DESCRIBEDBY_ID_PREFIX) != 0);
            element.setAttribute('aria-describedby', originalReferenceIds.join(' '));
        }
        /**
         * Adds a message reference to the element using aria-describedby and increments the registered
         * message's reference count.
         */
        _addMessageReference(element, message) {
            const registeredMessage = messageRegistry.get(message);
            // Add the aria-describedby reference and set the
            // describedby_host attribute to mark the element.
            addAriaReferencedId(element, 'aria-describedby', registeredMessage.messageElement.id);
            element.setAttribute(CDK_DESCRIBEDBY_HOST_ATTRIBUTE, '');
            registeredMessage.referenceCount++;
        }
        /**
         * Removes a message reference from the element using aria-describedby
         * and decrements the registered message's reference count.
         */
        _removeMessageReference(element, message) {
            const registeredMessage = messageRegistry.get(message);
            registeredMessage.referenceCount--;
            removeAriaReferencedId(element, 'aria-describedby', registeredMessage.messageElement.id);
            element.removeAttribute(CDK_DESCRIBEDBY_HOST_ATTRIBUTE);
        }
        /** Returns true if the element has been described by the provided message ID. */
        _isElementDescribedByMessage(element, message) {
            const referenceIds = getAriaReferenceIds(element, 'aria-describedby');
            const registeredMessage = messageRegistry.get(message);
            const messageId = registeredMessage && registeredMessage.messageElement.id;
            return !!messageId && referenceIds.indexOf(messageId) != -1;
        }
        /** Determines whether a message can be described on a particular element. */
        _canBeDescribed(element, message) {
            if (!this._isElementNode(element)) {
                return false;
            }
            if (message && typeof message === 'object') {
                // We'd have to make some assumptions about the description element's text, if the consumer
                // passed in an element. Assume that if an element is passed in, the consumer has verified
                // that it can be used as a description.
                return true;
            }
            const trimmedMessage = message == null ? '' : `${message}`.trim();
            const ariaLabel = element.getAttribute('aria-label');
            // We shouldn't set descriptions if they're exactly the same as the `aria-label` of the
            // element, because screen readers will end up reading out the same text twice in a row.
            return trimmedMessage ? (!ariaLabel || ariaLabel.trim() !== trimmedMessage) : false;
        }
        /** Checks whether a node is an Element node. */
        _isElementNode(element) {
            return element.nodeType === this._document.ELEMENT_NODE;
        }
    }
    AriaDescriber.ɵprov = i0.ɵɵdefineInjectable({ factory: function AriaDescriber_Factory() { return new AriaDescriber(i0.ɵɵinject(i1.DOCUMENT)); }, token: AriaDescriber, providedIn: "root" });
    AriaDescriber.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    AriaDescriber.ctorParameters = () => [
        { type: undefined, decorators: [{ type: Inject, args: [DOCUMENT,] }] }
    ];
    return AriaDescriber;
})();
export { AriaDescriber };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYXJpYS1kZXNjcmliZXIuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL2ExMXkvYXJpYS1kZXNjcmliZXIvYXJpYS1kZXNjcmliZXIudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLGlCQUFpQixDQUFDO0FBQ3pDLE9BQU8sRUFBQyxNQUFNLEVBQUUsVUFBVSxFQUFZLE1BQU0sZUFBZSxDQUFDO0FBQzVELE9BQU8sRUFBQyxtQkFBbUIsRUFBRSxtQkFBbUIsRUFBRSxzQkFBc0IsRUFBQyxNQUFNLGtCQUFrQixDQUFDOzs7QUFlbEcsc0VBQXNFO0FBQ3RFLE1BQU0sQ0FBQyxNQUFNLHFCQUFxQixHQUFHLG1DQUFtQyxDQUFDO0FBRXpFLHVEQUF1RDtBQUN2RCxNQUFNLENBQUMsTUFBTSx5QkFBeUIsR0FBRyx5QkFBeUIsQ0FBQztBQUVuRSxtRkFBbUY7QUFDbkYsTUFBTSxDQUFDLE1BQU0sOEJBQThCLEdBQUcsc0JBQXNCLENBQUM7QUFFckUseUVBQXlFO0FBQ3pFLElBQUksTUFBTSxHQUFHLENBQUMsQ0FBQztBQUVmLDZGQUE2RjtBQUM3RixNQUFNLGVBQWUsR0FBRyxJQUFJLEdBQUcsRUFBeUMsQ0FBQztBQUV6RSw2Q0FBNkM7QUFDN0MsSUFBSSxpQkFBaUIsR0FBdUIsSUFBSSxDQUFDO0FBRWpEOzs7O0dBSUc7QUFDSDtJQUFBLE1BQ2EsYUFBYTtRQUd4QixZQUE4QixTQUFjO1lBQzFDLElBQUksQ0FBQyxTQUFTLEdBQUcsU0FBUyxDQUFDO1FBQzdCLENBQUM7UUFFRDs7OztXQUlHO1FBQ0gsUUFBUSxDQUFDLFdBQW9CLEVBQUUsT0FBMkI7WUFDeEQsSUFBSSxDQUFDLElBQUksQ0FBQyxlQUFlLENBQUMsV0FBVyxFQUFFLE9BQU8sQ0FBQyxFQUFFO2dCQUMvQyxPQUFPO2FBQ1I7WUFFRCxJQUFJLE9BQU8sT0FBTyxL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d0Y7WUFDeEYsT0FBTyxjQUFjLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxTQUFTLElBQUksU0FBUyxDQUFDLElBQUksRUFBRSxLQUFLLGNBQWMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxLQUFLLENBQUM7UUFDdEYsQ0FBQztRQUVELGdEQUFnRDtRQUN4QyxjQUFjLENBQUMsT0FBYTtZQUNsQyxPQUFPLE9BQU8sQ0FBQyxRQUFRLEtBQUssSUFBSSxDQUFDLFNBQVMsQ0FBQyxZQUFZLENBQUM7UUFDMUQsQ0FBQzs7OztnQkF6TUYsVUFBVSxTQUFDLEVBQUMsVUFBVSxFQUFFLE1BQU0sRUFBQzs7O2dEQUlqQixNQUFNLFNBQUMsUUFBUTs7d0JBcEQ5QjtLQTBQQztTQXpNWSxhQUFhIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7RE9DVU1FTlR9IGZyb20gJ0Bhbmd1bGFyL2NvbW1vbic7XG5pbXBvcnQge0luamVjdCwgSW5qZWN0YWJsZSwgT25EZXN0cm95fSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7YWRkQXJpYVJlZmVyZW5jZWRJZCwgZ2V0QXJpYVJlZmVyZW5jZUlkcywgcmVtb3ZlQXJpYVJlZmVyZW5jZWRJZH0gZnJvbSAnLi9hcmlhLXJlZmVyZW5jZSc7XG5cblxuLyoqXG4gKiBJbnRlcmZhY2UgdXNlZCB0byByZWdpc3RlciBtZXNzYWdlIGVsZW1lbnRzIGFuZCBrZWVwIGEgY291bnQgb2YgaG93IG1hbnkgcmVnaXN0cmF0aW9ucyBoYXZlXG4gKiB0aGUgc2FtZSBtZXNzYWdlIGFuZCB0aGUgcmVmZXJlbmNlIHRvIHRoZSBtZXNzYWdlIGVsZW1lbnQgdXNlZCBmb3IgdGhlIGBhcmlhLWRlc2NyaWJlZGJ5YC5cbiAqL1xuZXhwb3J0IGludGVyZmFjZSBSZWdpc3RlcmVkTWVzc2FnZSB7XG4gIC8qKiBUaGUgZWxlbWVudCBjb250YWluaW5nIHRoZSBtZXNzYWdlLiAqL1xuICBtZXNzYWdlRWxlbWVudDogRWxlbWVudDtcblxuICAvKiogVGhlIG51bWJlciBvZiBlbGVtZW50cyB0aGF0IHJlZmVyZW5jZSB0aGlzIG1lc3NhZ2UgZWxlbWVudCB2aWEgYGFyaWEtZGVzY3JpYmVkYnlgLiAqL1xuICByZWZlcmVuY2VDb3VudDogbnVtYmVyO1xufVxuXG4vKiogSUQgdXNlZCBmb3IgdGhlIGJvZHkgY29udGFpbmVyIHdoZXJlIGFsbCBtZXNzYWdlcyBhcmUgYXBwZW5kZWQuICovXG5leHBvcnQgY29uc3QgTUVTU0FHRVNfQ09OVEFJTkVSX0lEID0gJ2Nkay1kZXNjcmliZWRieS1tZXNzYWdlLWNvbnRhaW5lcic7XG5cbi8qKiBJRCBwcmVmaXggdXNlZCBmb3IgZWFjaCBjcmVhdGVkIG1lc3NhZ2UgZWxlbWVudC4gKi9cbmV4cG9ydCBjb25zdCBDREtfREVTQ1JJQkVEQllfSURfUFJFRklYID0gJ2Nkay1kZXNjcmliZWRieS1tZXNzYWdlJztcblxuLyoqIEF0dHJpYnV0ZSBnaXZlbiB0byBlYWNoIGhvc3QgZWxlbWVudCB0aGF0IGlzIGRlc2NyaWJlZCBieSBhIG1lc3NhZ2UgZWxlbWVudC4gKi9cbmV4cG9ydCBjb25zdCBDREtfREVTQ1JJQkVEQllfSE9TVF9BVFRSSUJVVEUgPSAnY2RrLWRlc2NyaWJlZGJ5LWhvc3QnO1xuXG4vKiogR2xvYmFsIGluY3JlbWVudGFsIGlkZW50aWZpZXIgZm9yIGVhY2ggcmVnaXN0ZXJlZCBtZXNzYWdlIGVsZW1lbnQuICovXG5sZXQgbmV4dElkID0gMDtcblxuLyoqIEdsb2JhbCBtYXAgb2YgYWxsIHJlZ2lzdGVyZWQgbWVzc2FnZSBlbGVtZW50cyB0aGF0IGhhdmUgYmVlbiBwbGFjZWQgaW50byB0aGUgZG9jdW1lbnQuICovXG5jb25zdCBtZXNzYWdlUmVnaXN0cnkgPSBuZXcgTWFwPHN0cmluZ3xIVE1MRWxlbWVudCwgUmVnaXN0ZXJlZE1lc3NhZ2U+KCk7XG5cbi8qKiBDb250YWluZXIgZm9yIGFsbCByZWdpc3RlcmVkIG1lc3NhZ2VzLiAqL1xubGV0IG1lc3NhZ2VzQ29udGFpbmVyOiBIVE1MRWxlbWVudCB8IG51bGwgPSBudWxsO1xuXG4vKipcbiAqIFV0aWxpdHkgdGhhdCBjcmVhdGVzIHZpc3VhbGx5IGhpZGRlbiBlbGVtZW50cyB3aXRoIGEgbWVzc2FnZSBjb250ZW50LiBVc2VmdWwgZm9yIGVsZW1lbnRzIHRoYXRcbiAqIHdhbnQgdG8gdXNlIGFyaWEtZGVzY3JpYmVkYnkgdG8gZnVydGhlciBkZXNjcmliZSB0aGVtc2VsdmVzIHdpdGhvdXQgYWRkaW5nIGFkZGl0aW9uYWwgdmlzdWFsXG4gKiBjb250ZW50LlxuICovXG5ASW5qZWN0YWJsZSh7cHJvdmlkZWRJbjogJ3Jvb3QnfSlcbmV4cG9ydCBjbGFzcyBBcmlhRGVzY3JpYmVyIGltcGxlbWVudHMgT25EZXN0cm95IHtcbiAgcHJpdmF0ZSBfZG9jdW1lbnQ6IERvY3VtZW50O1xuXG4gIGNvbnN0cnVjdG9yKEBJbmplY3QoRE9DVU1FTlQpIF9kb2N1bWVudDogYW55KSB7XG4gICAgdGhpcy5fZG9jdW1lbnQgPSBfZG9jdW1lbnQ7XG4gIH1cblxuICAvKipcbiAgICogQWRkcyB0byB0aGUgaG9zdCBlbGVtZW50IGFuIGFyaWEtZGVzY3JpYmVkYnkgcmVmZXJlbmNlIHRvIGEgaGlkZGVuIGVsZW1lbnQgdGhhdCBjb250YWluc1xuICAgKiB0aGUgbWVzc2FnZS4gSWYgdGhlIHNhbWUgbWVzc2FnZSBoYXMgYWxyZWFkeSBiZWVuIHJlZ2lzdGVyZWQsIHRoZW4gaXQgd2lsbCByZXVzZSB0aGUgY3JlYXRlZFxuICAgKiBtZXNzYWdlIGVsZW1lbnQuXG4gICAqL1xuICBkZXNjcmliZShob3N0RWxlbWVudDogRWxlbWVudCwgbWVzc2FnZTogc3RyaW5nfEhUTUxFbGVtZW50KSB7XG4gICAgaWYgKCF0aGlzLl9jYW5CZURlc2NyaWJlZChob3N0RWxlbWVudCwgbWVzc2FnZSkpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICBpZiAodHlwZW9mIG1lc3NhZ2UgIT09ICdzdHJpbmcnKSB7XG4gICAgICAvLyBXZSBuZWVkIHRvIGVuc3VyZSB0aGF0IHRoZSBlbGVtZW50IGhhcyBhbiBJRC5cbiAgICAgIHRoaXMuX3NldE1lc3NhZ2VJZChtZXNzYWdlKTtcbiAgICAgIG1lc3NhZ2VSZWdpc3RyeS5zZXQobWVzc2FnZSwge21lc3NhZ2VFbGVtZW50OiBtZXNzYWdlLCByZWZlcmVuY2VDb3VudDogMH0pO1xuICAgIH0gZWxzZSBpZiAoIW1lc3NhZ2VSZWdpc3RyeS5oYXMobWVzc2FnZSkpIHtcbiAgICAgIHRoaXMuX2NyZWF0ZU1lc3NhZ2VFbGVtZW50KG1lc3NhZ2UpO1xuICAgIH1cblxuICAgIGlmICghdGhpcy5faXNFbGVtZW50RGVzY3JpYmVkQnlNZXNzYWdlKGhvc3RFbGVtZW50LCBtZXNzYWdlKSkge1xuICAgICAgdGhpcy5fYWRkTWVzc2FnZVJlZmVyZW5jZShob3N0RWxlbWVudCwgbWVzc2FnZSk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFJlbW92ZXMgdGhlIGhvc3QgZWxlbWVudCdzIGFyaWEtZGVzY3JpYmVkYnkgcmVmZXJlbmNlIHRvIHRoZSBtZXNzYWdlIGVsZW1lbnQuICovXG4gIHJlbW92ZURlc2NyaXB0aW9uKGhvc3RFbGVtZW50OiBFbGVtZW50LCBtZXNzYWdlOiBzdHJpbmd8SFRNTEVsZW1lbnQpIHtcbiAgICBpZiAoIXRoaXMuX2lzRWxlbWVudE5vZGUoaG9zdEVsZW1lbnQpKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgaWYgKHRoaXMuX2lzRWxlbWVudERlc2NyaWJlZEJ5TWVzc2FnZShob3N0RWxlbWVudCwgbWVzc2FnZSkpIHtcbiAgICAgIHRoaXMuX3JlbW92ZU1lc3NhZ2VSZWZlcmVuY2UoaG9zdEVsZW1lbnQsIG1lc3NhZ2UpO1xuICAgIH1cblxuICAgIC8vIElmIHRoZSBtZXNzYWdlIGlzIGEgc3RyaW5nLCBpdCBtZWFucyB0aGF0IGl0J3Mgb25lIHRoYXQgd2UgY3JlYXRlZCBmb3IgdGhlXG4gICAgLy8gY29uc3VtZXIgc28gd2UgY2FuIHJlbW92ZSBpdCBzYWZlbHksIG90aGVyd2lzZSB3ZSBzaG91bGQgbGVhdmUgaXQgaW4gcGxhY2UuXG4gICAgaWYgKHR5cGVvZiBtZXNzYWdlID09PSAnc3RyaW5nJykge1xuICAgICAgY29uc3QgcmVnaXN0ZXJlZE1lc3NhZ2UgPSBtZXNzYWdlUmVnaXN0cnkuZ2V0KG1lc3NhZ2UpO1xuICAgICAgaWYgKHJlZ2lzdGVyZWRNZXNzYWdlICYmIHJlZ2lzdGVyZWRNZXNzYWdlLnJlZmVyZW5jZUNvdW50ID09PSAwKSB7XG4gICAgICAgIHRoaXMuX2RlbGV0ZU1lc3NhZ2VFbGVtZW50KG1lc3NhZ2UpO1xuICAgICAgfVxuICAgIH1cblxuICAgIGlmIChtZXNzYWdlc0NvbnRhaW5lciAmJiBtZXNzYWdlc0NvbnRhaW5lci5jaGlsZE5vZGVzLmxlbmd0aCA9PT0gMCkge1xuICAgICAgdGhpcy5fZGVsZXRlTWVzc2FnZXNDb250YWluZXIoKTtcbiAgICB9XG4gIH1cblxuICAvKiogVW5yZWdpc3RlcnMgYWxsIGNyZWF0ZWQgbWVzc2FnZSBlbGVtZW50cyBhbmQgcmVtb3ZlcyB0aGUgbWVzc2FnZSBjb250YWluZXIuICovXG4gIG5nT25EZXN0cm95KCkge1xuICAgIGNvbnN0IGRlc2NyaWJlZEVsZW1lbnRzID1cbiAgICAgICAgdGhpcy5fZG9jdW1lbnQucXVlcnlTZWxlY3RvckFsbChgWyR7Q0RLX0RFU0NSSUJFREJZX0hPU1RfQVRUUklCVVRFfV1gKTtcblxuICAgIGZvciAobGV0IGkgPSAwOyBpIDwgZGVzY3JpYmVkRWxlbWVudHMubGVuZ3RoOyBpKyspIHtcbiAgICAgIHRoaXMuX3JlbW92ZUNka0Rlc2NyaWJlZEJ5UmVmZXJlbmNlSWRzKGRlc2NyaWJlZEVsZW1lbnRzW2ldKTtcbiAgICAgIGRlc2NyaWJlZEVsZW1lbnRzW2ldLnJlbW92ZUF0dHJpYnV0ZShDREtfREVTQ1JJQkVEQllfSE9TVF9BVFRSSUJVVEUpO1xuICAgIH1cblxuICAgIGlmIChtZXNzYWdlc0NvbnRhaW5lcikge1xuICAgICAgdGhpcy5fZGVsZXRlTWVzc2FnZXNDb250YWluZXIoKTtcbiAgICB9XG5cbiAgICBtZXNzYWdlUmVnaXN0cnkuY2xlYXIoKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBDcmVhdGVzIGEgbmV3IGVsZW1lbnQgaW4gdGhlIHZpc3VhbGx5IGhpZGRlbiBtZXNzYWdlIGNvbnRhaW5lciBlbGVtZW50IHdpdGggdGhlIG1lc3NhZ2VcbiAgICogYXMgaXRzIGNvbnRlbnQgYW5kIGFkZHMgaXQgdG8gdGhlIG1lc3NhZ2UgcmVnaXN0cnkuXG4gICAqL1xuICBwcml2YXRlIF9jcmVhdGVNZXNzYWdlRWxlbWVudChtZXNzYWdlOiBzdHJpbmcpIHtcbiAgICBjb25zdCBtZXNzYWdlRWxlbWVudCA9IHRoaXMuX2RvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ2RpdicpO1xuICAgIHRoaXMuX3NldE1lc3NhZ2VJZChtZXNzYWdlRWxlbWVudCk7XG4gICAgbWVzc2FnZUVsZW1lbnQudGV4dENvbnRlbnQgPSBtZXNzYWdlO1xuXG4gICAgdGhpcy5fY3JlYXRlTWVzc2FnZXNDb250YWluZXIoKTtcbiAgICBtZXNzYWdlc0NvbnRhaW5lciEuYXBwZW5kQ2hpbGQobWVzc2FnZUVsZW1lbnQpO1xuXG4gICAgbWVzc2FnZVJlZ2lzdHJ5LnNldChtZXNzYWdlLCB7bWVzc2FnZUVsZW1lbnQsIHJlZmVyZW5jZUNvdW50OiAwfSk7XG4gIH1cblxuICAvKiogQXNzaWducyBhIHVuaXF1ZSBJRCB0byBhbiBlbGVtZW50LCBpZiBpdCBkb2Vzbid0IGhhdmUgb25lIGFscmVhZHkuICovXG4gIHByaXZhdGUgX3NldE1lc3NhZ2VJZChlbGVtZW50OiBIVE1MRWxlbWVudCkge1xuICAgIGlmICghZWxlbWVudC5pZCkge1xuICAgICAgZWxlbWVudC5pZCA9IGAke0NES19ERVNDUklCRURCWV9JRF9QUkVGSVh9LSR7bmV4dElkKyt9YDtcbiAgICB9XG4gIH1cblxuICAvKiogRGVsZXRlcyB0aGUgbWVzc2FnZSBlbGVtZW50IGZyb20gdGhlIGdsb2JhbCBtZXNzYWdlcyBjb250YWluZXIuICovXG4gIHByaXZhdGUgX2RlbGV0ZU1lc3NhZ2VFbGVtZW50KG1lc3NhZ2U6IHN0cmluZykge1xuICAgIGNvbnN0IHJlZ2lzdGVyZWRNZXNzYWdlID0gbWVzc2FnZVJlZ2lzdHJ5LmdldChtZXNzYWdlKTtcbiAgICBjb25zdCBtZXNzYWdlRWxlbWVudCA9IHJlZ2lzdGVyZWRNZXNzYWdlICYmIHJlZ2lzdGVyZWRNZXNzYWdlLm1lc3NhZ2VFbGVtZW50O1xuICAgIGlmIChtZXNzYWdlc0NvbnRhaW5lciAmJiBtZXNzYWdlRWxlbWVudCkge1xuICAgICAgbWVzc2FnZXNDb250YWluZXIucmVtb3ZlQ2hpbGQobWVzc2FnZUVsZW1lbnQpO1xuICAgIH1cbiAgICBtZXNzYWdlUmVnaXN0cnkuZGVsZXRlKG1lc3NhZ2UpO1xuICB9XG5cbiAgLyoqIENyZWF0ZXMgdGhlIGdsb2JhbCBjb250YWluZXIgZm9yIGFsbCBhcmlhLWRlc2NyaWJlZGJ5IG1lc3NhZ2VzLiAqL1xuICBwcml2YXRlIF9jcmVhdGVNZXNzYWdlc0NvbnRhaW5lcigpIHtcbiAgICBpZiAoIW1lc3NhZ2VzQ29udGFpbmVyKSB7XG4gICAgICBjb25zdCBwcmVFeGlzdGluZ0NvbnRhaW5lciA9IHRoaXMuX2RvY3VtZW50LmdldEVsZW1lbnRCeUlkKE1FU1NBR0VTX0NPTlRBSU5FUl9JRCk7XG5cbiAgICAgIC8vIFdoZW4gZ29pbmcgZnJvbSB0aGUgc2VydmVyIHRvIHRoZSBjbGllbnQsIHdlIG1heSBlbmQgdXAgaW4gYSBzaXR1YXRpb24gd2hlcmUgdGhlcmUnc1xuICAgICAgLy8gYWxyZWFkeSBhIGNvbnRhaW5lciBvbiB0aGUgcGFnZSwgYnV0IHdlIGRvbid0IGhhdmUgYSByZWZlcmVuY2UgdG8gaXQuIENsZWFyIHRoZVxuICAgICAgLy8gb2xkIGNvbnRhaW5lciBzbyB3ZSBkb24ndCBnZXQgZHVwbGljYXRlcy4gRG9pbmcgdGhpcywgaW5zdGVhZCBvZiBlbXB0eWluZyB0aGUgcHJldmlvdXNcbiAgICAgIC8vIGNvbnRhaW5lciwgc2hvdWxkIGJlIHNsaWdodGx5IGZhc3Rlci5cbiAgICAgIGlmIChwcmVFeGlzdGluZ0NvbnRhaW5lcikge1xuICAgICAgICBwcmVFeGlzdGluZ0NvbnRhaW5lci5wYXJlbnROb2RlIS5yZW1vdmVDaGlsZChwcmVFeGlzdGluZ0NvbnRhaW5lcik7XG4gICAgICB9XG5cbiAgICAgIG1lc3NhZ2VzQ29udGFpbmVyID0gdGhpcy5fZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgnZGl2Jyk7XG4gICAgICBtZXNzYWdlc0NvbnRhaW5lci5pZCA9IE1FU1NBR0VTX0NPTlRBSU5FUl9JRDtcbiAgICAgIG1lc3NhZ2VzQ29udGFpbmVyLnNldEF0dHJpYnV0ZSgnYXJpYS1oaWRkZW4nLCAndHJ1ZScpO1xuICAgICAgbWVzc2FnZXNDb250YWluZXIuc3R5bGUuZGlzcGxheSA9ICdub25lJztcbiAgICAgIHRoaXMuX2RvY3VtZW50LmJvZHkuYXBwZW5kQ2hpbGQobWVzc2FnZXNDb250YWluZXIpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBEZWxldGVzIHRoZSBnbG9iYWwgbWVzc2FnZXMgY29udGFpbmVyLiAqL1xuICBwcml2YXRlIF9kZWxldGVNZXNzYWdlc0NvbnRhaW5lcigpIHtcbiAgICBpZiAobWVzc2FnZXNDb250YWluZXIgJiYgbWVzc2FnZXNDb250YWluZXIucGFyZW50Tm9kZSkge1xuICAgICAgbWVzc2FnZXNDb250YWluZXIucGFyZW50Tm9kZS5yZW1vdmVDaGlsZChtZXNzYWdlc0NvbnRhaW5lcik7XG4gICAgICBtZXNzYWdlc0NvbnRhaW5lciA9IG51bGw7XG4gICAgfVxuICB9XG5cbiAgLyoqIFJlbW92ZXMgYWxsIGNkay1kZXNjcmliZWRieSBtZXNzYWdlcyB0aGF0IGFyZSBob3N0ZWQgdGhyb3VnaCB0aGUgZWxlbWVudC4gKi9cbiAgcHJpdmF0ZSBfcmVtb3ZlQ2RrRGVzY3JpYmVkQnlSZWZlcmVuY2VJZHMoZWxlbWVudDogRWxlbWVudCkge1xuICAgIC8vIFJlbW92ZSBhbGwgYXJpYS1kZXNjcmliZWRieSByZWZlcmVuY2UgSURzIHRoYXQgYXJlIHByZWZpeGVkIGJ5IENES19ERVNDUklCRURCWV9JRF9QUkVGSVhcbiAgICBjb25zdCBvcmlnaW5hbFJlZmVyZW5jZUlkcyA9IGdldEFyaWFSZWZlcmVuY2VJZHMoZWxlbWVudCwgJ2FyaWEtZGVzY3JpYmVkYnknKVxuICAgICAgICAuZmlsdGVyKGlkID0+IGlkLmluZGV4T2YoQ0RLX0RFU0NSSUJFREJZX0lEX1BSRUZJWCkgIT0gMCk7XG4gICAgZWxlbWVudC5zZXRBdHRyaWJ1dGUoJ2FyaWEtZGVzY3JpYmVkYnknLCBvcmlnaW5hbFJlZmVyZW5jZUlkcy5qb2luKCcgJykpO1xuICB9XG5cbiAgLyoqXG4gICAqIEFkZHMgYSBtZXNzYWdlIHJlZmVyZW5jZSB0byB0aGUgZWxlbWVudCB1c2luZyBhcmlhLWRlc2NyaWJlZGJ5IGFuZCBpbmNyZW1lbnRzIHRoZSByZWdpc3RlcmVkXG4gICAqIG1lc3NhZ2UncyByZWZlcmVuY2UgY291bnQuXG4gICAqL1xuICBwcml2YXRlIF9hZGRNZXNzYWdlUmVmZXJlbmNlKGVsZW1lbnQ6IEVsZW1lbnQsIG1lc3NhZ2U6IHN0cmluZ3xIVE1MRWxlbWVudCkge1xuICAgIGNvbnN0IHJlZ2lzdGVyZWRNZXNzYWdlID0gbWVzc2FnZVJlZ2lzdHJ5LmdldChtZXNzYWdlKSE7XG5cbiAgICAvLyBBZGQgdGhlIGFyaWEtZGVzY3JpYmVkYnkgcmVmZXJlbmNlIGFuZCBzZXQgdGhlXG4gICAgLy8gZGVzY3JpYmVkYnlfaG9zdCBhdHRyaWJ1dGUgdG8gbWFyayB0aGUgZWxlbWVudC5cbiAgICBhZGRBcmlhUmVmZXJlbmNlZElkKGVsZW1lbnQsICdhcmlhLWRlc2NyaWJlZGJ5JywgcmVnaXN0ZXJlZE1lc3NhZ2UubWVzc2FnZUVsZW1lbnQuaWQpO1xuICAgIGVsZW1lbnQuc2V0QXR0cmlidXRlKENES19ERVNDUklCRURCWV9IT1NUX0FUVFJJQlVURSwgJycpO1xuXG4gICAgcmVnaXN0ZXJlZE1lc3NhZ2UucmVmZXJlbmNlQ291bnQrKztcbiAgfVxuXG4gIC8qKlxuICAgKiBSZW1vdmVzIGEgbWVzc2FnZSByZWZlcmVuY2UgZnJvbSB0aGUgZWxlbWVudCB1c2luZyBhcmlhLWRlc2NyaWJlZGJ5XG4gICAqIGFuZCBkZWNyZW1lbnRzIHRoZSByZWdpc3RlcmVkIG1lc3NhZ2UncyByZWZlcmVuY2UgY291bnQuXG4gICAqL1xuICBwcml2YXRlIF9yZW1vdmVNZXNzYWdlUmVmZXJlbmNlKGVsZW1lbnQ6IEVsZW1lbnQsIG1lc3NhZ2U6IHN0cmluZ3xIVE1MRWxlbWVudCkge1xuICAgIGNvbnN0IHJlZ2lzdGVyZWRNZXNzYWdlID0gbWVzc2FnZVJlZ2lzdHJ5LmdldChtZXNzYWdlKSE7XG4gICAgcmVnaXN0ZXJlZE1lc3NhZ2UucmVmZXJlbmNlQ291bnQtLTtcblxuICAgIHJlbW92ZUFyaWFSZWZlcmVuY2VkSWQoZWxlbWVudCwgJ2FyaWEtZGVzY3JpYmVkYnknLCByZWdpc3RlcmVkTWVzc2FnZS5tZXNzYWdlRWxlbWVudC5pZCk7XG4gICAgZWxlbWVudC5yZW1vdmVBdHRyaWJ1dGUoQ0RLX0RFU0NSSUJFREJZX0hPU1RfQVRUUklCVVRFKTtcbiAgfVxuXG4gIC8qKiBSZXR1cm5zIHRydWUgaWYgdGhlIGVsZW1lbnQgaGFzIGJlZW4gZGVzY3JpYmVkIGJ5IHRoZSBwcm92aWRlZCBtZXNzYWdlIElELiAqL1xuICBwcml2YXRlIF9pc0VsZW1lbnREZXNjcmliZWRCeU1lc3NhZ2UoZWxlbWVudDogRWxlbWVudCwgbWVzc2FnZTogc3RyaW5nfEhUTUxFbGVtZW50KTogYm9vbGVhbiB7XG4gICAgY29uc3QgcmVmZXJlbmNlSWRzID0gZ2V0QXJpYVJlZmVyZW5jZUlkcyhlbGVtZW50LCAnYXJpYS1kZXNjcmliZWRieScpO1xuICAgIGNvbnN0IHJlZ2lzdGVyZWRNZXNzYWdlID0gbWVzc2FnZVJlZ2lzdHJ5LmdldChtZXNzYWdlKTtcbiAgICBjb25zdCBtZXNzYWdlSWQgPSByZWdpc3RlcmVkTWVzc2FnZSAmJiByZWdpc3RlcmVkTWVzc2FnZS5tZXNzYWdlRWxlbWVudC5pZDtcblxuICAgIHJldHVybiAhIW1lc3NhZ2VJZCAmJiByZWZlcmVuY2VJZHMuaW5kZXhPZihtZXNzYWdlSWQpICE9IC0xO1xuICB9XG5cbiAgLyoqIERldGVybWluZXMgd2hldGhlciBhIG1lc3NhZ2UgY2FuIGJlIGRlc2NyaWJlZCBvbiBhIHBhcnRpY3VsYXIgZWxlbWVudC4gKi9cbiAgcHJpdmF0ZSBfY2FuQmVEZXNjcmliZWQoZWxlbWVudDogRWxlbWVudCwgbWVzc2FnZTogc3RyaW5nfEhUTUxFbGVtZW50fHZvaWQpOiBib29sZWFuIHtcbiAgICBpZiAoIXRoaXMuX2lzRWxlbWVudE5vZGUoZWxlbWVudCkpIHtcbiAgICAgIHJldHVybiBmYWxzZTtcbiAgICB9XG5cbiAgICBpZiAobWVzc2FnZSAmJiB0eXBlb2YgbWVzc2FnZSA9PT0gJ29iamVjdCcpIHtcbiAgICAgIC8vIFdlJ2QgaGF2ZSB0byBtYWtlIHNvbWUgYXNzdW1wdGlvbnMgYWJvdXQgdGhlIGRlc2NyaXB0aW9uIGVsZW1lbnQncyB0ZXh0LCBpZiB0aGUgY29uc3VtZXJcbiAgICAgIC8vIHBhc3NlZCBpbiBhbiBlbGVtZW50LiBBc3N1bWUgdGhhdCBpZiBhbiBlbGVtZW50IGlzIHBhc3NlZCBpbiwgdGhlIGNvbnN1bWVyIGhhcyB2ZXJpZmllZFxuICAgICAgLy8gdGhhdCBpdCBjYW4gYmUgdXNlZCBhcyBhIGRlc2NyaXB0aW9uLlxuICAgICAgcmV0dXJuIHRydWU7XG4gICAgfVxuXG4gICAgY29uc3QgdHJpbW1lZE1lc3NhZ2UgPSBtZXNzYWdlID09IG51bGwgPyAnJyA6IGAke21lc3NhZ2V9YC50cmltKCk7XG4gICAgY29uc3QgYXJpYUxhYmVsID0gZWxlbWVudC5nZXRBdHRyaWJ1dGUoJ2FyaWEtbGFiZWwnKTtcblxuICAgIC8vIFdlIHNob3VsZG4ndCBzZXQgZGVzY3JpcHRpb25zIGlmIHRoZXkncmUgZXhhY3RseSB0aGUgc2FtZSBhcyB0aGUgYGFyaWEtbGFiZWxgIG9mIHRoZVxuICAgIC8vIGVsZW1lbnQsIGJlY2F1c2Ugc2NyZWVuIHJlYWRlcnMgd2lsbCBlbmQgdXAgcmVhZGluZyBvdXQgdGhlIHNhbWUgdGV4dCB0d2ljZSBpbiBhIHJvdy5cbiAgICByZXR1cm4gdHJpbW1lZE1lc3NhZ2UgPyAoIWFyaWFMYWJlbCB8fCBhcmlhTGFiZWwudHJpbSgpICE9PSB0cmltbWVkTWVzc2FnZSkgOiBmYWxzZTtcbiAgfVxuXG4gIC8qKiBDaGVja3Mgd2hldGhlciBhIG5vZGUgaXMgYW4gRWxlbWVudCBub2RlLiAqL1xuICBwcml2YXRlIF9pc0VsZW1lbnROb2RlKGVsZW1lbnQ6IE5vZGUpOiBlbGVtZW50IGlzIEVsZW1lbnQge1xuICAgIHJldHVybiBlbGVtZW50Lm5vZGVUeXBlID09PSB0aGlzLl9kb2N1bWVudC5FTEVNRU5UX05PREU7XG4gIH1cbn1cbiJdfQ==