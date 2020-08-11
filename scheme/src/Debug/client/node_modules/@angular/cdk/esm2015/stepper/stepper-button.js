/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Directive, HostListener, Input } from '@angular/core';
import { CdkStepper } from './stepper';
/** Button that moves to the next step in a stepper workflow. */
let CdkStepperNext = /** @class */ (() => {
    class CdkStepperNext {
        constructor(_stepper) {
            this._stepper = _stepper;
            /** Type of the next button. Defaults to "submit" if not specified. */
            this.type = 'submit';
        }
        // We have to use a `HostListener` here in order to support both Ivy and ViewEngine.
        // In Ivy the `host` bindings will be merged when this class is extended, whereas in
        // ViewEngine they're overwritten.
        // TODO(crisbeto): we move this back into `host` once Ivy is turned on by default.
        // tslint:disable-next-line:no-host-decorator-in-concrete
        _handleClick() {
            this._stepper.next();
        }
    }
    CdkStepperNext.decorators = [
        { type: Directive, args: [{
                    selector: 'button[cdkStepperNext]',
                    host: {
                        '[type]': 'type',
                    }
                },] }
    ];
    CdkStepperNext.ctorParameters = () => [
        { type: CdkStepper }
    ];
    CdkStepperNext.propDecorators = {
        type: [{ type: Input }],
        _handleClick: [{ type: HostListener, args: ['click',] }]
    };
    return CdkStepperNext;
})();
export { CdkStepperNext };
/** Button that moves to the previous step in a stepper workflow. */
let CdkStepperPrevious = /** @class */ (() => {
    class CdkStepperPrevious {
        constructor(_stepper) {
            this._stepper = _stepper;
            /** Type of the previous button. Defaults to "button" if not specified. */
            this.type = 'button';
        }
        // We have to use a `HostListener` here in order to support both Ivy and ViewEngine.
        // In Ivy the `host` bindings will be merged when this class is extended, whereas in
        // ViewEngine they're overwritten.
        // TODO(crisbeto): we move this back into `host` once Ivy is turned on by default.
        // tslint:disable-next-line:no-host-decorator-in-concrete
        _handleClick() {
            this._stepper.previous();
        }
    }
    CdkStepperPrevious.decorators = [
        { type: Directive, args: [{
                    selector: 'button[cdkStepperPrevious]',
                    host: {
                        '[type]': 'type',
                    }
                },] }
    ];
    CdkStepperPrevious.ctorParameters = () => [
        { type: CdkStepper }
    ];
    CdkStepperPrevious.propDecorators = {
        type: [{ type: Input }],
        _handleClick: [{ type: HostListener, args: ['click',] }]
    };
    return CdkStepperPrevious;
})();
export { CdkStepperPrevious };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic3RlcHBlci1idXR0b24uanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvY2RrL3N0ZXBwZXIvc3RlcHBlci1idXR0b24udHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFNBQVMsRUFBRSxZQUFZLEVBQUUsS0FBSyxFQUFDLE1BQU0sZUFBZSxDQUFDO0FBRTdELE9BQU8sRUFBQyxVQUFVLEVBQUMsTUFBTSxXQUFXLENBQUM7QUFFckMsZ0VBQWdFO0FBQ2hFO0lBQUEsTUFNYSxjQUFjO1FBSXpCLFlBQW1CLFFBQW9CO1lBQXBCLGFBQVEsR0FBUixRQUFRLENBQVk7WUFIdkMsc0VBQXNFO1lBQzdELFNBQUksR0FBVyxRQUFRLENBQUM7UUFFUyxDQUFDO1FBRTNDLG9GQUFvRjtRQUNwRixvRkFBb0Y7UUFDcEYsa0NBQWtDO1FBQ2xDLGtGQUFrRjtRQUNsRix5REFBeUQ7UUFFekQsWUFBWTtZQUNWLElBQUksQ0FBQyxRQUFRLENBQUMsSUFBSSxFQUFFLENBQUM7UUFDdkIsQ0FBQzs7O2dCQXBCRixTQUFTLFNBQUM7b0JBQ1QsUUFBUSxFQUFFLHdCQUF3QjtvQkFDbEMsSUFBSSxFQUFFO3dCQUNKLFFBQVEsRUFBRSxNQUFNO3FCQUNqQjtpQkFDRjs7O2dCQVJPLFVBQVU7Ozt1QkFXZixLQUFLOytCQVNMLFlBQVksU0FBQyxPQUFPOztJQUl2QixxQkFBQztLQUFBO1NBZlksY0FBYztBQWlCM0Isb0VBQW9FO0FBQ3BFO0lBQUEsTUFNYSxrQkFBa0I7UUFJN0IsWUFBbUIsUUFBb0I7WUFBcEIsYUFBUSxHQUFSLFFBQVEsQ0FBWTtZQUh2QywwRUFBMEU7WUFDakUsU0FBSSxHQUFXLFFBQVEsQ0FBQztRQUVTLENBQUM7UUFFM0Msb0ZBQW9GO1FBQ3BGLG9GQUFvRjtRQUNwRixrQ0FBa0M7UUFDbEMsa0ZBQWtGO1FBQ2xGLHlEQUF5RDtRQUV6RCxZQUFZO1lBQ1YsSUFBSSxDQUFDLFFBQVEsQ0FBQyxRQUFRLEVBQUUsQ0FBQztRQUMzQixDQUFDOzs7Z0JBcEJGLFNBQVMsU0FBQztvQkFDVCxRQUFRLEVBQUUsNEJBQTRCO29CQUN0QyxJQUFJLEVBQUU7d0JBQ0osUUFBUSxFQUFFLE1BQU07cUJBQ2pCO2lCQUNGOzs7Z0JBaENPLFVBQVU7Ozt1QkFtQ2YsS0FBSzsrQkFTTCxZQUFZLFNBQUMsT0FBTzs7SUFJdkIseUJBQUM7S0FBQTtTQWZZLGtCQUFrQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0RpcmVjdGl2ZSwgSG9zdExpc3RlbmVyLCBJbnB1dH0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5cbmltcG9ydCB7Q2RrU3RlcHBlcn0gZnJvbSAnLi9zdGVwcGVyJztcblxuLyoqIEJ1dHRvbiB0aGF0IG1vdmVzIHRvIHRoZSBuZXh0IHN0ZXAgaW4gYSBzdGVwcGVyIHdvcmtmbG93LiAqL1xuQERpcmVjdGl2ZSh7XG4gIHNlbGVjdG9yOiAnYnV0dG9uW2Nka1N0ZXBwZXJOZXh0XScsXG4gIGhvc3Q6IHtcbiAgICAnW3R5cGVdJzogJ3R5cGUnLFxuICB9XG59KVxuZXhwb3J0IGNsYXNzIENka1N0ZXBwZXJOZXh0IHtcbiAgLyoqIFR5cGUgb2YgdGhlIG5leHQgYnV0dG9uLiBEZWZhdWx0cyB0byBcInN1Ym1pdFwiIGlmIG5vdCBzcGVjaWZpZWQuICovXG4gIEBJbnB1dCgpIHR5cGU6IHN0cmluZyA9ICdzdWJtaXQnO1xuXG4gIGNvbnN0cnVjdG9yKHB1YmxpYyBfc3RlcHBlcjogQ2RrU3RlcHBlcikge31cblxuICAvLyBXZSBoYXZlIHRvIHVzZSBhIGBIb3N0TGlzdGVuZXJgIGhlcmUgaW4gb3JkZXIgdG8gc3VwcG9ydCBib3RoIEl2eSBhbmQgVmlld0VuZ2luZS5cbiAgLy8gSW4gSXZ5IHRoZSBgaG9zdGAgYmluZGluZ3Mgd2lsbCBiZSBtZXJnZWQgd2hlbiB0aGlzIGNsYXNzIGlzIGV4dGVuZGVkLCB3aGVyZWFzIGluXG4gIC8vIFZpZXdFbmdpbmUgdGhleSdyZSBvdmVyd3JpdHRlbi5cbiAgLy8gVE9ETyhjcmlzYmV0byk6IHdlIG1vdmUgdGhpcyBiYWNrIGludG8gYGhvc3RgIG9uY2UgSXZ5IGlzIHR1cm5lZCBvbiBieSBkZWZhdWx0LlxuICAvLyB0c2xpbnQ6ZGlzYWJsZS1uZXh0LWxpbmU6bm8taG9zdC1kZWNvcmF0b3ItaW4tY29uY3JldGVcbiAgQEhvc3RMaXN0ZW5lcignY2xpY2snKVxuICBfaGFuZGxlQ2xpY2soKSB7XG4gICAgdGhpcy5fc3RlcHBlci5uZXh0KCk7XG4gIH1cbn1cblxuLyoqIEJ1dHRvbiB0aGF0IG1vdmVzIHRvIHRoZSBwcmV2aW91cyBzdGVwIGluIGEgc3RlcHBlciB3b3JrZmxvdy4gKi9cbkBEaXJlY3RpdmUoe1xuICBzZWxlY3RvcjogJ2J1dHRvbltjZGtTdGVwcGVyUHJldmlvdXNdJyxcbiAgaG9zdDoge1xuICAgICdbdHlwZV0nOiAndHlwZScsXG4gIH1cbn0pXG5leHBvcnQgY2xhc3MgQ2RrU3RlcHBlclByZXZpb3VzIHtcbiAgLyoqIFR5cGUgb2YgdGhlIHByZXZpb3VzIGJ1dHRvbi4gRGVmYXVsdHMgdG8gXCJidXR0b25cIiBpZiBub3Qgc3BlY2lmaWVkLiAqL1xuICBASW5wdXQoKSB0eXBlOiBzdHJpbmcgPSAnYnV0dG9uJztcblxuICBjb25zdHJ1Y3RvcihwdWJsaWMgX3N0ZXBwZXI6IENka1N0ZXBwZXIpIHt9XG5cbiAgLy8gV2UgaGF2ZSB0byB1c2UgYSBgSG9zdExpc3RlbmVyYCBoZXJlIGluIG9yZGVyIHRvIHN1cHBvcnQgYm90aCBJdnkgYW5kIFZpZXdFbmdpbmUuXG4gIC8vIEluIEl2eSB0aGUgYGhvc3RgIGJpbmRpbmdzIHdpbGwgYmUgbWVyZ2VkIHdoZW4gdGhpcyBjbGFzcyBpcyBleHRlbmRlZCwgd2hlcmVhcyBpblxuICAvLyBWaWV3RW5naW5lIHRoZXkncmUgb3ZlcndyaXR0ZW4uXG4gIC8vIFRPRE8oY3Jpc2JldG8pOiB3ZSBtb3ZlIHRoaXMgYmFjayBpbnRvIGBob3N0YCBvbmNlIEl2eSBpcyB0dXJuZWQgb24gYnkgZGVmYXVsdC5cbiAgLy8gdHNsaW50OmRpc2FibGUtbmV4dC1saW5lOm5vLWhvc3QtZGVjb3JhdG9yLWluLWNvbmNyZXRlXG4gIEBIb3N0TGlzdGVuZXIoJ2NsaWNrJylcbiAgX2hhbmRsZUNsaWNrKCkge1xuICAgIHRoaXMuX3N0ZXBwZXIucHJldmlvdXMoKTtcbiAgfVxufVxuIl19