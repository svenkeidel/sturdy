/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Directive, EventEmitter, Input, Output, NgZone, InjectionToken, Inject, Optional, } from '@angular/core';
import { Clipboard } from './clipboard';
/** Injection token that can be used to provide the default options to `CdkCopyToClipboard`. */
export const CKD_COPY_TO_CLIPBOARD_CONFIG = new InjectionToken('CKD_COPY_TO_CLIPBOARD_CONFIG');
/**
 * Provides behavior for a button that when clicked copies content into user's
 * clipboard.
 */
let CdkCopyToClipboard = /** @class */ (() => {
    class CdkCopyToClipboard {
        constructor(_clipboard, _ngZone, config) {
            this._clipboard = _clipboard;
            this._ngZone = _ngZone;
            /** Content to be copied. */
            this.text = '';
            /**
             * How many times to attempt to copy the text. This may be necessary for longer text, because
             * the browser needs time to fill an intermediate textarea element and copy the content.
             */
            this.attempts = 1;
            /**
             * Emits when some text is copied to the clipboard. The
             * emitted value indicates whether copying was successful.
             */
            this.copied = new EventEmitter();
            /** Copies that are currently being attempted. */
            this._pending = new Set();
            if (config && config.attempts != null) {
                this.attempts = config.attempts;
            }
        }
        /** Copies the current text to the clipboard. */
        copy(attempts = this.attempts) {
            if (attempts > 1) {
                let remainingAttempts = attempts;
                const pending = this._clipboard.beginCopy(this.text);
                this._pending.add(pending);
                const attempt = () => {
                    const successful = pending.copy();
                    if (!successful && --remainingAttempts && !this._destroyed) {
                        // We use 1 for the timeout since it's more predictable when flushing in unit tests.
                        this._currentTimeout = this._ngZone.runOutsideAngular(() => setTimeout(attempt, 1));
                    }
                    else {
                        this._currentTimeout = null;
                        this._pending.delete(pending);
                        pending.destroy();
                        this.copied.emit(successful);
                    }
                };
                attempt();
            }
            else {
                this.copied.emit(this._clipboard.copy(this.text));
            }
        }
        ngOnDestroy() {
            if (this._currentTimeout) {
                clearTimeout(this._currentTimeout);
            }
            this._pending.forEach(copy => copy.destroy());
            this._pending.clear();
            this._destroyed = true;
        }
    }
    CdkCopyToClipboard.decorators = [
        { type: Directive, args: [{
                    selector: '[cdkCopyToClipboard]',
                    host: {
                        '(click)': 'copy()',
                    }
                },] }
    ];
    CdkCopyToClipboard.ctorParameters = () => [
        { type: Clipboard },
        { type: NgZone },
        { type: undefined, decorators: [{ type: Optional }, { type: Inject, args: [CKD_COPY_TO_CLIPBOARD_CONFIG,] }] }
    ];
    CdkCopyToClipboard.propDecorators = {
        text: [{ type: Input, args: ['cdkCopyToClipboard',] }],
        attempts: [{ type: Input, args: ['cdkCopyToClipboardAttempts',] }],
        copied: [{ type: Output, args: ['cdkCopyToClipboardCopied',] }]
    };
    return CdkCopyToClipboard;
})();
export { CdkCopyToClipboard };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY29weS10by1jbGlwYm9hcmQuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL2NsaXBib2FyZC9jb3B5LXRvLWNsaXBib2FyZC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQ0wsU0FBUyxFQUNULFlBQVksRUFDWixLQUFLLEVBQ0wsTUFBTSxFQUNOLE1BQU0sRUFDTixjQUFjLEVBQ2QsTUFBTSxFQUNOLFFBQVEsR0FFVCxNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUMsU0FBUyxFQUFDLE1BQU0sYUFBYSxDQUFDO0FBU3RDLCtGQUErRjtBQUMvRixNQUFNLENBQUMsTUFBTSw0QkFBNEIsR0FDckMsSUFBSSxjQUFjLENBQTJCLDhCQUE4QixDQUFDLENBQUM7QUFFakY7OztHQUdHO0FBQ0g7SUFBQSxNQU1hLGtCQUFrQjtRQXlCN0IsWUFDVSxVQUFxQixFQUNyQixPQUFlLEVBQzJCLE1BQWlDO1lBRjNFLGVBQVUsR0FBVixVQUFVLENBQVc7WUFDckIsWUFBTyxHQUFQLE9BQU8sQ0FBUTtZQTFCekIsNEJBQTRCO1lBQ0MsU0FBSSxHQUFXLEVBQUUsQ0FBQztZQUUvQzs7O2VBR0c7WUFDa0MsYUFBUSxHQUFXLENBQUMsQ0FBQztZQUUxRDs7O2VBR0c7WUFDaUMsV0FBTSxHQUFHLElBQUksWUFBWSxFQUFXLENBQUM7WUFFekUsaURBQWlEO1lBQ3pDLGFBQVEsR0FBRyxJQUFJLEdBQUcsRUFBZSxDQUFDO1lBYXhDLElBQUksTUFBTSxJQUFJLE1BQU0sQ0FBQyxRQUFRLElBQUksSUFBSSxFQUFFO2dCQUNyQyxJQUFJLENBQUMsUUFBUSxHQUFHLE1BQU0sQ0FBQyxRQUFRLENBQUM7YUFDakM7UUFDSCxDQUFDO1FBRUQsZ0RBQWdEO1FBQ2hELElBQUksQ0FBQyxXQUFtQixJQUFJLENBQUMsUUFBUTtZQUNuQyxJQUFJLFFBQVEsR0FBRyxDQUFDLEVBQUU7Z0JBQ2hCLElBQUksaUJBQWlCLEdBQUcsUUFBUSxDQUFDO2dCQUNqQyxNQUFNLE9BQU8sR0FBRyxJQUFJLENBQUMsVUFBVSxDQUFDLFNBQVMsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUM7Z0JBQ3JELElBQUksQ0FBQyxRQUFRLENBQUMsR0FBRyxDQUFDLE9BQU8sQ0FBQyxDQUFDO2dCQUUzQixNQUFNLE9BQU8sR0FBRyxHQUFHLEVBQUU7b0JBQ25CLE1BQU0sVUFBVSxHQUFHLE9BQU8sQ0FBQyxJQUFJLEVBQUUsQ0FBQztvQkFDbEMsSUFBSSxDQUFDLFVBQVUsSUFBSSxFQUFFLGlCQUFpQixJQUFJLENBQUMsSUFBSSxDQUFDLFVBQVUsRUFBRTt3QkFDMUQsb0ZBQW9GO3dCQUNwRixJQUFJLENBQUMsZUFBZSxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUMsaUJBQWlCLENBQUMsR0FBRyxFQUFFLENBQUMsVUFBVSxDQUFDLE9BQU8sRUFBRSxDQUFDLENBQUMsQ0FBQyxDQUFDO3FCQUNyRjt5QkFBTTt3QkFDTCxJQUFJLENBQUMsZUFBZSxHQUFHLElBQUksQ0FBQzt3QkFDNUIsSUFBSSxDQUFDLFFBQVEsQ0FBQyxNQUFNLENBQUMsT0FBTyxDQUFDLENBQUM7d0JBQzlCLE9BQU8sQ0FBQyxPQUFPLEVBQUUsQ0FBQzt3QkFDbEIsSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBVSxDQUFDLENBQUM7cUJBQzlCO2dCQUNILENBQUMsQ0FBQztnQkFDRixPQUFPLEVBQUUsQ0FBQzthQUNYO2lCQUFNO2dCQUNMLElBQUksQ0FBQyxNQUFNLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxVQUFVLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDO2FBQ25EO1FBQ0gsQ0FBQztRQUVELFdBQVc7WUFDVCxJQUFJLElBQUksQ0FBQyxlQUFlLEVBQUU7Z0JBQ3hCLFlBQVksQ0FBQyxJQUFJLENBQUMsZUFBZSxDQUFDLENBQUM7YUFDcEM7WUFFRCxJQUFJLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUFDLElBQUksQ0FBQyxPQUFPLEVBQUUsQ0FBQyxDQUFDO1lBQzlDLElBQUksQ0FBQyxRQUFRLENBQUMsS0FBSyxFQUFFLENBQUM7WUFDdEIsSUFBSSxDQUFDLFVBQVUsR0FBRyxJQUFJLENBQUM7UUFDekIsQ0FBQzs7O2dCQTFFRixTQUFTLFNBQUM7b0JBQ1QsUUFBUSxFQUFFLHNCQUFzQjtvQkFDaEMsSUFBSSxFQUFFO3dCQUNKLFNBQVMsRUFBRSxRQUFRO3FCQUNwQjtpQkFDRjs7O2dCQXRCTyxTQUFTO2dCQU5mLE1BQU07Z0RBeURILFFBQVEsWUFBSSxNQUFNLFNBQUMsNEJBQTRCOzs7dUJBMUJqRCxLQUFLLFNBQUMsb0JBQW9COzJCQU0xQixLQUFLLFNBQUMsNEJBQTRCO3lCQU1sQyxNQUFNLFNBQUMsMEJBQTBCOztJQXVEcEMseUJBQUM7S0FBQTtTQXJFWSxrQkFBa0IiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtcbiAgRGlyZWN0aXZlLFxuICBFdmVudEVtaXR0ZXIsXG4gIElucHV0LFxuICBPdXRwdXQsXG4gIE5nWm9uZSxcbiAgSW5qZWN0aW9uVG9rZW4sXG4gIEluamVjdCxcbiAgT3B0aW9uYWwsXG4gIE9uRGVzdHJveSxcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQge0NsaXBib2FyZH0gZnJvbSAnLi9jbGlwYm9hcmQnO1xuaW1wb3J0IHtQZW5kaW5nQ29weX0gZnJvbSAnLi9wZW5kaW5nLWNvcHknO1xuXG4vKiogT2JqZWN0IHRoYXQgY2FuIGJlIHVzZWQgdG8gY29uZmlndXJlIHRoZSBkZWZhdWx0IG9wdGlvbnMgZm9yIGBDZGtDb3B5VG9DbGlwYm9hcmRgLiAqL1xuZXhwb3J0IGludGVyZmFjZSBDZGtDb3B5VG9DbGlwYm9hcmRDb25maWcge1xuICAvKiogRGVmYXVsdCBudW1iZXIgb2YgYXR0ZW1wdHMgdG8gbWFrZSB3aGVuIGNvcHlpbmcgdGV4dCB0byB0aGUgY2xpcGJvYXJkLiAqL1xuICBhdHRlbXB0cz86IG51bWJlcjtcbn1cblxuLyoqIEluamVjdGlvbiB0b2tlbiB0aGF0IGNhbiBiZSB1c2VkIHRvIHByb3ZpZGUgdGhlIGRlZmF1bHQgb3B0aW9ucyB0byBgQ2RrQ29weVRvQ2xpcGJvYXJkYC4gKi9cbmV4cG9ydCBjb25zdCBDS0RfQ09QWV9UT19DTElQQk9BUkRfQ09ORklHID1cbiAgICBuZXcgSW5qZWN0aW9uVG9rZW48Q2RrQ29weVRvQ2xpcGJvYXJkQ29uZmlnPignQ0tEX0NPUFlfVE9fQ0xJUEJPQVJEX0NPTkZJRycpO1xuXG4vKipcbiAqIFByb3ZpZGVzIGJlaGF2aW9yIGZvciBhIGJ1dHRvbiB0aGF0IHdoZW4gY2xpY2tlZCBjb3BpZXMgY29udGVudCBpbnRvIHVzZXInc1xuICogY2xpcGJvYXJkLlxuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICdbY2RrQ29weVRvQ2xpcGJvYXJkXScsXG4gIGhvc3Q6IHtcbiAgICAnKGNsaWNrKSc6ICdjb3B5KCknLFxuICB9XG59KVxuZXhwb3J0IGNsYXNzIENka0NvcHlUb0NsaXBib2FyZCBpbXBsZW1lbnRzIE9uRGVzdHJveSB7XG4gIC8qKiBDb250ZW50IHRvIGJlIGNvcGllZC4gKi9cbiAgQElucHV0KCdjZGtDb3B5VG9DbGlwYm9hcmQnKSB0ZXh0OiBzdHJpbmcgPSAnJztcblxuICAvKipcbiAgICogSG93IG1hbnkgdGltZXMgdG8gYXR0ZW1wdCB0byBjb3B5IHRoZSB0ZXh0LiBUaGlzIG1heSBiZSBuZWNlc3NhcnkgZm9yIGxvbmdlciB0ZXh0LCBiZWNhdXNlXG4gICAqIHRoZSBicm93c2VyIG5lZWRzIHRpbWUgdG8gZmlsbCBhbiBpbnRlcm1lZGlhdGUgdGV4dGFyZWEgZWxlbWVudCBhbmQgY29weSB0aGUgY29udGVudC5cbiAgICovXG4gIEBJbnB1dCgnY2RrQ29weVRvQ2xpcGJvYXJkQXR0ZW1wdHMnKSBhdHRlbXB0czogbnVtYmVyID0gMTtcblxuICAvKipcbiAgICogRW1pdHMgd2hlbiBzb21lIHRleHQgaXMgY29waWVkIHRvIHRoZSBjbGlwYm9hcmQuIFRoZVxuICAgKiBlbWl0dGVkIHZhbHVlIGluZGljYXRlcyB3aGV0aGVyIGNvcHlpbmcgd2FzIHN1Y2Nlc3NmdWwuXG4gICAqL1xuICBAT3V0cHV0KCdjZGtDb3B5VG9DbGlwYm9hcmRDb3BpZWQnKSBjb3BpZWQgPSBuZXcgRXZlbnRFbWl0dGVyPGJvb2xlYW4+KCk7XG5cbiAgLyoqIENvcGllcyB0aGF0IGFyZSBjdXJyZW50bHkgYmVpbmcgYXR0ZW1wdGVkLiAqL1xuICBwcml2YXRlIF9wZW5kaW5nID0gbmV3IFNldDxQZW5kaW5nQ29weT4oKTtcblxuICAvKiogV2hldGhlciB0aGUgZGlyZWN0aXZlIGhhcyBiZWVuIGRlc3Ryb3llZC4gKi9cbiAgcHJpdmF0ZSBfZGVzdHJveWVkOiBib29sZWFuO1xuXG4gIC8qKiBUaW1lb3V0IGZvciB0aGUgY3VycmVudCBjb3B5IGF0dGVtcHQuICovXG4gIHByaXZhdGUgX2N1cnJlbnRUaW1lb3V0OiBhbnk7XG5cbiAgY29uc3RydWN0b3IoXG4gICAgcHJpdmF0ZSBfY2xpcGJvYXJkOiBDbGlwYm9hcmQsXG4gICAgcHJpdmF0ZSBfbmdab25lOiBOZ1pvbmUsXG4gICAgQE9wdGlvbmFsKCkgQEluamVjdChDS0RfQ09QWV9UT19DTElQQk9BUkRfQ09ORklHKSBjb25maWc/OiBDZGtDb3B5VG9DbGlwYm9hcmRDb25maWcpIHtcblxuICAgIGlmIChjb25maWcgJiYgY29uZmlnLmF0dGVtcHRzICE9IG51bGwpIHtcbiAgICAgIHRoaXMuYXR0ZW1wdHMgPSBjb25maWcuYXR0ZW1wdHM7XG4gICAgfVxuICB9XG5cbiAgLyoqIENvcGllcyB0aGUgY3VycmVudCB0ZXh0IHRvIHRoZSBjbGlwYm9hcmQuICovXG4gIGNvcHkoYXR0ZW1wdHM6IG51bWJlciA9IHRoaXMuYXR0ZW1wdHMpOiB2b2lkIHtcbiAgICBpZiAoYXR0ZW1wdHMgPiAxKSB7XG4gICAgICBsZXQgcmVtYWluaW5nQXR0ZW1wdHMgPSBhdHRlbXB0cztcbiAgICAgIGNvbnN0IHBlbmRpbmcgPSB0aGlzLl9jbGlwYm9hcmQuYmVnaW5Db3B5KHRoaXMudGV4dCk7XG4gICAgICB0aGlzLl9wZW5kaW5nLmFkZChwZW5kaW5nKTtcblxuICAgICAgY29uc3QgYXR0ZW1wdCA9ICgpID0+IHtcbiAgICAgICAgY29uc3Qgc3VjY2Vzc2Z1bCA9IHBlbmRpbmcuY29weSgpO1xuICAgICAgICBpZiAoIXN1Y2Nlc3NmdWwgJiYgLS1yZW1haW5pbmdBdHRlbXB0cyAmJiAhdGhpcy5fZGVzdHJveWVkKSB7XG4gICAgICAgICAgLy8gV2UgdXNlIDEgZm9yIHRoZSB0aW1lb3V0IHNpbmNlIGl0J3MgbW9yZSBwcmVkaWN0YWJsZSB3aGVuIGZsdXNoaW5nIGluIHVuaXQgdGVzdHMuXG4gICAgICAgICAgdGhpcy5fY3VycmVudFRpbWVvdXQgPSB0aGlzLl9uZ1pvbmUucnVuT3V0c2lkZUFuZ3VsYXIoKCkgPT4gc2V0VGltZW91dChhdHRlbXB0LCAxKSk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgdGhpcy5fY3VycmVudFRpbWVvdXQgPSBudWxsO1xuICAgICAgICAgIHRoaXMuX3BlbmRpbmcuZGVsZXRlKHBlbmRpbmcpO1xuICAgICAgICAgIHBlbmRpbmcuZGVzdHJveSgpO1xuICAgICAgICAgIHRoaXMuY29waWVkLmVtaXQoc3VjY2Vzc2Z1bCk7XG4gICAgICAgIH1cbiAgICAgIH07XG4gICAgICBhdHRlbXB0KCk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHRoaXMuY29waWVkLmVtaXQodGhpcy5fY2xpcGJvYXJkLmNvcHkodGhpcy50ZXh0KSk7XG4gICAgfVxuICB9XG5cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgaWYgKHRoaXMuX2N1cnJlbnRUaW1lb3V0KSB7XG4gICAgICBjbGVhclRpbWVvdXQodGhpcy5fY3VycmVudFRpbWVvdXQpO1xuICAgIH1cblxuICAgIHRoaXMuX3BlbmRpbmcuZm9yRWFjaChjb3B5ID0+IGNvcHkuZGVzdHJveSgpKTtcbiAgICB0aGlzLl9wZW5kaW5nLmNsZWFyKCk7XG4gICAgdGhpcy5fZGVzdHJveWVkID0gdHJ1ZTtcbiAgfVxufVxuIl19