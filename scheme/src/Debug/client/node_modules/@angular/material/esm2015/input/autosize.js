/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { CdkTextareaAutosize } from '@angular/cdk/text-field';
import { Directive, Input } from '@angular/core';
/**
 * Directive to automatically resize a textarea to fit its content.
 * @deprecated Use `cdkTextareaAutosize` from `@angular/cdk/text-field` instead.
 * @breaking-change 8.0.0
 */
let MatTextareaAutosize = /** @class */ (() => {
    class MatTextareaAutosize extends CdkTextareaAutosize {
        get matAutosizeMinRows() { return this.minRows; }
        set matAutosizeMinRows(value) { this.minRows = value; }
        get matAutosizeMaxRows() { return this.maxRows; }
        set matAutosizeMaxRows(value) { this.maxRows = value; }
        get matAutosize() { return this.enabled; }
        set matAutosize(value) { this.enabled = value; }
        get matTextareaAutosize() { return this.enabled; }
        set matTextareaAutosize(value) { this.enabled = value; }
    }
    MatTextareaAutosize.decorators = [
        { type: Directive, args: [{
                    selector: 'textarea[mat-autosize], textarea[matTextareaAutosize]',
                    exportAs: 'matTextareaAutosize',
                    inputs: ['cdkAutosizeMinRows', 'cdkAutosizeMaxRows'],
                    host: {
                        'class': 'cdk-textarea-autosize mat-autosize',
                        // Textarea elements that have the directive applied should have a single row by default.
                        // Browsers normally show two rows by default and therefore this limits the minRows binding.
                        'rows': '1',
                    },
                },] }
    ];
    MatTextareaAutosize.propDecorators = {
        matAutosizeMinRows: [{ type: Input }],
        matAutosizeMaxRows: [{ type: Input }],
        matAutosize: [{ type: Input, args: ['mat-autosize',] }],
        matTextareaAutosize: [{ type: Input }]
    };
    return MatTextareaAutosize;
})();
export { MatTextareaAutosize };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYXV0b3NpemUuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvaW5wdXQvYXV0b3NpemUudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLG1CQUFtQixFQUFDLE1BQU0seUJBQXlCLENBQUM7QUFDNUQsT0FBTyxFQUFDLFNBQVMsRUFBRSxLQUFLLEVBQUMsTUFBTSxlQUFlLENBQUM7QUFFL0M7Ozs7R0FJRztBQUNIO0lBQUEsTUFXYSxtQkFBb0IsU0FBUSxtQkFBbUI7UUFDMUQsSUFDSSxrQkFBa0IsS0FBYSxPQUFPLElBQUksQ0FBQyxPQUFPLENBQUMsQ0FBQyxDQUFDO1FBQ3pELElBQUksa0JBQWtCLENBQUMsS0FBYSxJQUFJLElBQUksQ0FBQyxPQUFPLEdBQUcsS0FBSyxDQUFDLENBQUMsQ0FBQztRQUUvRCxJQUNJLGtCQUFrQixLQUFhLE9BQU8sSUFBSSxDQUFDLE9BQU8sQ0FBQyxDQUFDLENBQUM7UUFDekQsSUFBSSxrQkFBa0IsQ0FBQyxLQUFhLElBQUksSUFBSSxDQUFDLE9BQU8sR0FBRyxLQUFLLENBQUMsQ0FBQyxDQUFDO1FBRS9ELElBQ0ksV0FBVyxLQUFjLE9BQU8sSUFBSSxDQUFDLE9BQU8sQ0FBQyxDQUFDLENBQUM7UUFDbkQsSUFBSSxXQUFXLENBQUMsS0FBYyxJQUFJLElBQUksQ0FBQyxPQUFPLEdBQUcsS0FBSyxDQUFDLENBQUMsQ0FBQztRQUV6RCxJQUNJLG1CQUFtQixLQUFjLE9BQU8sSUFBSSxDQUFDLE9BQU8sQ0FBQyxDQUFDLENBQUM7UUFDM0QsSUFBSSxtQkFBbUIsQ0FBQyxLQUFjLElBQUksSUFBSSxDQUFDLE9BQU8sR0FBRyxLQUFLLENBQUMsQ0FBQyxDQUFDOzs7Z0JBMUJsRSxTQUFTLFNBQUM7b0JBQ1QsUUFBUSxFQUFFLHVEQUF1RDtvQkFDakUsUUFBUSxFQUFFLHFCQUFxQjtvQkFDL0IsTUFBTSxFQUFFLENBQUMsb0JBQW9CLEVBQUUsb0JBQW9CLENBQUM7b0JBQ3BELElBQUksRUFBRTt3QkFDSixPQUFPLEVBQUUsb0NBQW9DO3dCQUM3Qyx5RkFBeUY7d0JBQ3pGLDRGQUE0Rjt3QkFDNUYsTUFBTSxFQUFFLEdBQUc7cUJBQ1o7aUJBQ0Y7OztxQ0FFRSxLQUFLO3FDQUlMLEtBQUs7OEJBSUwsS0FBSyxTQUFDLGNBQWM7c0NBSXBCLEtBQUs7O0lBR1IsMEJBQUM7S0FBQTtTQWhCWSxtQkFBbUIiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtDZGtUZXh0YXJlYUF1dG9zaXplfSBmcm9tICdAYW5ndWxhci9jZGsvdGV4dC1maWVsZCc7XG5pbXBvcnQge0RpcmVjdGl2ZSwgSW5wdXR9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuXG4vKipcbiAqIERpcmVjdGl2ZSB0byBhdXRvbWF0aWNhbGx5IHJlc2l6ZSBhIHRleHRhcmVhIHRvIGZpdCBpdHMgY29udGVudC5cbiAqIEBkZXByZWNhdGVkIFVzZSBgY2RrVGV4dGFyZWFBdXRvc2l6ZWAgZnJvbSBgQGFuZ3VsYXIvY2RrL3RleHQtZmllbGRgIGluc3RlYWQuXG4gKiBAYnJlYWtpbmctY2hhbmdlIDguMC4wXG4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogJ3RleHRhcmVhW21hdC1hdXRvc2l6ZV0sIHRleHRhcmVhW21hdFRleHRhcmVhQXV0b3NpemVdJyxcbiAgZXhwb3J0QXM6ICdtYXRUZXh0YXJlYUF1dG9zaXplJyxcbiAgaW5wdXRzOiBbJ2Nka0F1dG9zaXplTWluUm93cycsICdjZGtBdXRvc2l6ZU1heFJvd3MnXSxcbiAgaG9zdDoge1xuICAgICdjbGFzcyc6ICdjZGstdGV4dGFyZWEtYXV0b3NpemUgbWF0LWF1dG9zaXplJyxcbiAgICAvLyBUZXh0YXJlYSBlbGVtZW50cyB0aGF0IGhhdmUgdGhlIGRpcmVjdGl2ZSBhcHBsaWVkIHNob3VsZCBoYXZlIGEgc2luZ2xlIHJvdyBieSBkZWZhdWx0LlxuICAgIC8vIEJyb3dzZXJzIG5vcm1hbGx5IHNob3cgdHdvIHJvd3MgYnkgZGVmYXVsdCBhbmQgdGhlcmVmb3JlIHRoaXMgbGltaXRzIHRoZSBtaW5Sb3dzIGJpbmRpbmcuXG4gICAgJ3Jvd3MnOiAnMScsXG4gIH0sXG59KVxuZXhwb3J0IGNsYXNzIE1hdFRleHRhcmVhQXV0b3NpemUgZXh0ZW5kcyBDZGtUZXh0YXJlYUF1dG9zaXplIHtcbiAgQElucHV0KClcbiAgZ2V0IG1hdEF1dG9zaXplTWluUm93cygpOiBudW1iZXIgeyByZXR1cm4gdGhpcy5taW5Sb3dzOyB9XG4gIHNldCBtYXRBdXRvc2l6ZU1pblJvd3ModmFsdWU6IG51bWJlcikgeyB0aGlzLm1pblJvd3MgPSB2YWx1ZTsgfVxuXG4gIEBJbnB1dCgpXG4gIGdldCBtYXRBdXRvc2l6ZU1heFJvd3MoKTogbnVtYmVyIHsgcmV0dXJuIHRoaXMubWF4Um93czsgfVxuICBzZXQgbWF0QXV0b3NpemVNYXhSb3dzKHZhbHVlOiBudW1iZXIpIHsgdGhpcy5tYXhSb3dzID0gdmFsdWU7IH1cblxuICBASW5wdXQoJ21hdC1hdXRvc2l6ZScpXG4gIGdldCBtYXRBdXRvc2l6ZSgpOiBib29sZWFuIHsgcmV0dXJuIHRoaXMuZW5hYmxlZDsgfVxuICBzZXQgbWF0QXV0b3NpemUodmFsdWU6IGJvb2xlYW4pIHsgdGhpcy5lbmFibGVkID0gdmFsdWU7IH1cblxuICBASW5wdXQoKVxuICBnZXQgbWF0VGV4dGFyZWFBdXRvc2l6ZSgpOiBib29sZWFuIHsgcmV0dXJuIHRoaXMuZW5hYmxlZDsgfVxuICBzZXQgbWF0VGV4dGFyZWFBdXRvc2l6ZSh2YWx1ZTogYm9vbGVhbikgeyB0aGlzLmVuYWJsZWQgPSB2YWx1ZTsgfVxufVxuIl19