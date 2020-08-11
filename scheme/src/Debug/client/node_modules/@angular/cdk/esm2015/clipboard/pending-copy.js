/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/**
 * A pending copy-to-clipboard operation.
 *
 * The implementation of copying text to the clipboard modifies the DOM and
 * forces a relayout. This relayout can take too long if the string is large,
 * causing the execCommand('copy') to happen too long after the user clicked.
 * This results in the browser refusing to copy. This object lets the
 * relayout happen in a separate tick from copying by providing a copy function
 * that can be called later.
 *
 * Destroy must be called when no longer in use, regardless of whether `copy` is
 * called.
 */
export class PendingCopy {
    constructor(text, _document) {
        this._document = _document;
        const textarea = this._textarea = this._document.createElement('textarea');
        const styles = textarea.style;
        // Hide the element for display and accessibility. Set an
        // absolute position so the page layout isn't affected.
        styles.opacity = '0';
        styles.position = 'absolute';
        styles.left = styles.top = '-999em';
        textarea.setAttribute('aria-hidden', 'true');
        textarea.value = text;
        this._document.body.appendChild(textarea);
    }
    /** Finishes copying the text. */
    copy() {
        const textarea = this._textarea;
        let successful = false;
        try { // Older browsers could throw if copy is not supported.
            if (textarea) {
                const currentFocus = this._document.activeElement;
                textarea.select();
                textarea.setSelectionRange(0, textarea.value.length);
                successful = this._document.execCommand('copy');
                if (currentFocus) {
                    currentFocus.focus();
                }
            }
        }
        catch (_a) {
            // Discard error.
            // Initial setting of {@code successful} will represent failure here.
        }
        return successful;
    }
    /** Cleans up DOM changes used to perform the copy operation. */
    destroy() {
        const textarea = this._textarea;
        if (textarea) {
            if (textarea.parentNode) {
                textarea.parentNode.removeChild(textarea);
            }
            this._textarea = undefined;
        }
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGVuZGluZy1jb3B5LmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL2Nkay9jbGlwYm9hcmQvcGVuZGluZy1jb3B5LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVIOzs7Ozs7Ozs7Ozs7R0FZRztBQUNILE1BQU0sT0FBTyxXQUFXO0lBR3RCLFlBQVksSUFBWSxFQUFtQixTQUFtQjtRQUFuQixjQUFTLEdBQVQsU0FBUyxDQUFVO1FBQzVELE1BQU0sUUFBUSxHQUFHLElBQUksQ0FBQyxTQUFTLEdBQUcsSUFBSSxDQUFDLFNBQVMsQ0FBQyxhQUFhLENBQUMsVUFBVSxDQUFDLENBQUM7UUFDM0UsTUFBTSxNQUFNLEdBQUcsUUFBUSxDQUFDLEtBQUssQ0FBQztRQUU5Qix5REFBeUQ7UUFDekQsdURBQXVEO1FBQ3ZELE1BQU0sQ0FBQyxPQUFPLEdBQUcsR0FBRyxDQUFDO1FBQ3JCLE1BQU0sQ0FBQyxRQUFRLEdBQUcsVUFBVSxDQUFDO1FBQzdCLE1BQU0sQ0FBQyxJQUFJLEdBQUcsTUFBTSxDQUFDLEdBQUcsR0FBRyxRQUFRLENBQUM7UUFDcEMsUUFBUSxDQUFDLFlBQVksQ0FBQyxhQUFhLEVBQUUsTUFBTSxDQUFDLENBQUM7UUFDN0MsUUFBUSxDQUFDLEtBQUssR0FBRyxJQUFJLENBQUM7UUFDdEIsSUFBSSxDQUFDLFNBQVMsQ0FBQyxJQUFJLENBQUMsV0FBVyxDQUFDLFFBQVEsQ0FBQyxDQUFDO0lBQzVDLENBQUM7SUFFRCxpQ0FBaUM7SUFDakMsSUFBSTtRQUNGLE1BQU0sUUFBUSxHQUFHLElBQUksQ0FBQyxTQUFTLENBQUM7UUFDaEMsSUFBSSxVQUFVLEdBQUcsS0FBSyxDQUFDO1FBRXZCLElBQUksRUFBRyx1REFBdUQ7WUFDNUQsSUFBSSxRQUFRLEVBQUU7Z0JBQ1osTUFBTSxZQUFZLEdBQUcsSUFBSSxDQUFDLFNBQVMsQ0FBQyxhQUF3QyxDQUFDO2dCQUU3RSxRQUFRLENBQUMsTUFBTSxFQUFFLENBQUM7Z0JBQ2xCLFFBQVEsQ0FBQyxpQkFBaUIsQ0FBQyxDQUFDLEVBQUUsUUFBUSxDQUFDLEtBQUssQ0FBQyxNQUFNLENBQUMsQ0FBQztnQkFDckQsVUFBVSxHQUFHLElBQUksQ0FBQyxTQUFTLENBQUMsV0FBVyxDQUFDLE1BQU0sQ0FBQyxDQUFDO2dCQUVoRCxJQUFJLFlBQVksRUFBRTtvQkFDaEIsWUFBWSxDQUFDLEtBQUssRUFBRSxDQUFDO2lCQUN0QjthQUNGO1NBQ0Y7UUFBQyxXQUFNO1lBQ04saUJBQWlCO1lBQ2pCLHFFQUFxRTtTQUN0RTtRQUVELE9BQU8sVUFBVSxDQUFDO0lBQ3BCLENBQUM7SUFFRCxnRUFBZ0U7SUFDaEUsT0FBTztRQUNMLE1BQU0sUUFBUSxHQUFHLElBQUksQ0FBQyxTQUFTLENBQUM7UUFFaEMsSUFBSSxRQUFRLEVBQUU7WUFDWixJQUFJLFFBQVEsQ0FBQyxVQUFVLEVBQUU7Z0JBQ3ZCLFFBQVEsQ0FBQyxVQUFVLENBQUMsV0FBVyxDQUFDLFFBQVEsQ0FBQyxDQUFDO2FBQzNDO1lBRUQsSUFBSSxDQUFDLFNBQVMsR0FBRyxTQUFTLENBQUM7U0FDNUI7SUFDSCxDQUFDO0NBQ0YiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuLyoqXG4gKiBBIHBlbmRpbmcgY29weS10by1jbGlwYm9hcmQgb3BlcmF0aW9uLlxuICpcbiAqIFRoZSBpbXBsZW1lbnRhdGlvbiBvZiBjb3B5aW5nIHRleHQgdG8gdGhlIGNsaXBib2FyZCBtb2RpZmllcyB0aGUgRE9NIGFuZFxuICogZm9yY2VzIGEgcmVsYXlvdXQuIFRoaXMgcmVsYXlvdXQgY2FuIHRha2UgdG9vIGxvbmcgaWYgdGhlIHN0cmluZyBpcyBsYXJnZSxcbiAqIGNhdXNpbmcgdGhlIGV4ZWNDb21tYW5kKCdjb3B5JykgdG8gaGFwcGVuIHRvbyBsb25nIGFmdGVyIHRoZSB1c2VyIGNsaWNrZWQuXG4gKiBUaGlzIHJlc3VsdHMgaW4gdGhlIGJyb3dzZXIgcmVmdXNpbmcgdG8gY29weS4gVGhpcyBvYmplY3QgbGV0cyB0aGVcbiAqIHJlbGF5b3V0IGhhcHBlbiBpbiBhIHNlcGFyYXRlIHRpY2sgZnJvbSBjb3B5aW5nIGJ5IHByb3ZpZGluZyBhIGNvcHkgZnVuY3Rpb25cbiAqIHRoYXQgY2FuIGJlIGNhbGxlZCBsYXRlci5cbiAqXG4gKiBEZXN0cm95IG11c3QgYmUgY2FsbGVkIHdoZW4gbm8gbG9uZ2VyIGluIHVzZSwgcmVnYXJkbGVzcyBvZiB3aGV0aGVyIGBjb3B5YCBpc1xuICogY2FsbGVkLlxuICovXG5leHBvcnQgY2xhc3MgUGVuZGluZ0NvcHkge1xuICBwcml2YXRlIF90ZXh0YXJlYTogSFRNTFRleHRBcmVhRWxlbWVudHx1bmRlZmluZWQ7XG5cbiAgY29uc3RydWN0b3IodGV4dDogc3RyaW5nLCBwcml2YXRlIHJlYWRvbmx5IF9kb2N1bWVudDogRG9jdW1lbnQpIHtcbiAgICBjb25zdCB0ZXh0YXJlYSA9IHRoaXMuX3RleHRhcmVhID0gdGhpcy5fZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgndGV4dGFyZWEnKTtcbiAgICBjb25zdCBzdHlsZXMgPSB0ZXh0YXJlYS5zdHlsZTtcblxuICAgIC8vIEhpZGUgdGhlIGVsZW1lbnQgZm9yIGRpc3BsYXkgYW5kIGFjY2Vzc2liaWxpdHkuIFNldCBhblxuICAgIC8vIGFic29sdXRlIHBvc2l0aW9uIHNvIHRoZSBwYWdlIGxheW91dCBpc24ndCBhZmZlY3RlZC5cbiAgICBzdHlsZXMub3BhY2l0eSA9ICcwJztcbiAgICBzdHlsZXMucG9zaXRpb24gPSAnYWJzb2x1dGUnO1xuICAgIHN0eWxlcy5sZWZ0ID0gc3R5bGVzLnRvcCA9ICctOTk5ZW0nO1xuICAgIHRleHRhcmVhLnNldEF0dHJpYnV0ZSgnYXJpYS1oaWRkZW4nLCAndHJ1ZScpO1xuICAgIHRleHRhcmVhLnZhbHVlID0gdGV4dDtcbiAgICB0aGlzLl9kb2N1bWVudC5ib2R5LmFwcGVuZENoaWxkKHRleHRhcmVhKTtcbiAgfVxuXG4gIC8qKiBGaW5pc2hlcyBjb3B5aW5nIHRoZSB0ZXh0LiAqL1xuICBjb3B5KCk6IGJvb2xlYW4ge1xuICAgIGNvbnN0IHRleHRhcmVhID0gdGhpcy5fdGV4dGFyZWE7XG4gICAgbGV0IHN1Y2Nlc3NmdWwgPSBmYWxzZTtcblxuICAgIHRyeSB7ICAvLyBPbGRlciBicm93c2VycyBjb3VsZCB0aHJvdyBpZiBjb3B5IGlzIG5vdCBzdXBwb3J0ZWQuXG4gICAgICBpZiAodGV4dGFyZWEpIHtcbiAgICAgICAgY29uc3QgY3VycmVudEZvY3VzID0gdGhpcy5fZG9jdW1lbnQuYWN0aXZlRWxlbWVudCBhcyBIVE1MT3JTVkdFbGVtZW50IHwgbnVsbDtcblxuICAgICAgICB0ZXh0YXJlYS5zZWxlY3QoKTtcbiAgICAgICAgdGV4dGFyZWEuc2V0U2VsZWN0aW9uUmFuZ2UoMCwgdGV4dGFyZWEudmFsdWUubGVuZ3RoKTtcbiAgICAgICAgc3VjY2Vzc2Z1bCA9IHRoaXMuX2RvY3VtZW50LmV4ZWNDb21tYW5kKCdjb3B5Jyk7XG5cbiAgICAgICAgaWYgKGN1cnJlbnRGb2N1cykge1xuICAgICAgICAgIGN1cnJlbnRGb2N1cy5mb2N1cygpO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfSBjYXRjaCB7XG4gICAgICAvLyBEaXNjYXJkIGVycm9yLlxuICAgICAgLy8gSW5pdGlhbCBzZXR0aW5nIG9mIHtAY29kZSBzdWNjZXNzZnVsfSB3aWxsIHJlcHJlc2VudCBmYWlsdXJlIGhlcmUuXG4gICAgfVxuXG4gICAgcmV0dXJuIHN1Y2Nlc3NmdWw7XG4gIH1cblxuICAvKiogQ2xlYW5zIHVwIERPTSBjaGFuZ2VzIHVzZWQgdG8gcGVyZm9ybSB0aGUgY29weSBvcGVyYXRpb24uICovXG4gIGRlc3Ryb3koKSB7XG4gICAgY29uc3QgdGV4dGFyZWEgPSB0aGlzLl90ZXh0YXJlYTtcblxuICAgIGlmICh0ZXh0YXJlYSkge1xuICAgICAgaWYgKHRleHRhcmVhLnBhcmVudE5vZGUpIHtcbiAgICAgICAgdGV4dGFyZWEucGFyZW50Tm9kZS5yZW1vdmVDaGlsZCh0ZXh0YXJlYSk7XG4gICAgICB9XG5cbiAgICAgIHRoaXMuX3RleHRhcmVhID0gdW5kZWZpbmVkO1xuICAgIH1cbiAgfVxufVxuIl19