/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ObserversModule } from '@angular/cdk/observers';
import { NgModule } from '@angular/core';
import { MatCommonModule, MatRippleModule } from '@angular/material/core';
import { MatCheckbox } from './checkbox';
import { MatCheckboxRequiredValidator } from './checkbox-required-validator';
/** This module is used by both original and MDC-based checkbox implementations. */
let _MatCheckboxRequiredValidatorModule = /** @class */ (() => {
    class _MatCheckboxRequiredValidatorModule {
    }
    _MatCheckboxRequiredValidatorModule.decorators = [
        { type: NgModule, args: [{
                    exports: [MatCheckboxRequiredValidator],
                    declarations: [MatCheckboxRequiredValidator],
                },] }
    ];
    return _MatCheckboxRequiredValidatorModule;
})();
export { _MatCheckboxRequiredValidatorModule };
let MatCheckboxModule = /** @class */ (() => {
    class MatCheckboxModule {
    }
    MatCheckboxModule.decorators = [
        { type: NgModule, args: [{
                    imports: [
                        MatRippleModule, MatCommonModule, ObserversModule,
                        _MatCheckboxRequiredValidatorModule
                    ],
                    exports: [MatCheckbox, MatCommonModule, _MatCheckboxRequiredValidatorModule],
                    declarations: [MatCheckbox],
                },] }
    ];
    return MatCheckboxModule;
})();
export { MatCheckboxModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2hlY2tib3gtbW9kdWxlLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL2NoZWNrYm94L2NoZWNrYm94LW1vZHVsZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsZUFBZSxFQUFDLE1BQU0sd0JBQXdCLENBQUM7QUFDdkQsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUN2QyxPQUFPLEVBQUMsZUFBZSxFQUFFLGVBQWUsRUFBQyxNQUFNLHdCQUF3QixDQUFDO0FBQ3hFLE9BQU8sRUFBQyxXQUFXLEVBQUMsTUFBTSxZQUFZLENBQUM7QUFDdkMsT0FBTyxFQUFDLDRCQUE0QixFQUFDLE1BQU0sK0JBQStCLENBQUM7QUFFM0UsbUZBQW1GO0FBQ25GO0lBQUEsTUFLYSxtQ0FBbUM7OztnQkFML0MsUUFBUSxTQUFDO29CQUNSLE9BQU8sRUFBRSxDQUFDLDRCQUE0QixDQUFDO29CQUN2QyxZQUFZLEVBQUUsQ0FBQyw0QkFBNEIsQ0FBQztpQkFDN0M7O0lBR0QsMENBQUM7S0FBQTtTQURZLG1DQUFtQztBQUdoRDtJQUFBLE1BUWEsaUJBQWlCOzs7Z0JBUjdCLFFBQVEsU0FBQztvQkFDUixPQUFPLEVBQUU7d0JBQ1AsZUFBZSxFQUFFLGVBQWUsRUFBRSxlQUFlO3dCQUNqRCxtQ0FBbUM7cUJBQ3BDO29CQUNELE9BQU8sRUFBRSxDQUFDLFdBQVcsRUFBRSxlQUFlLEVBQUUsbUNBQW1DLENBQUM7b0JBQzVFLFlBQVksRUFBRSxDQUFDLFdBQVcsQ0FBQztpQkFDNUI7O0lBRUQsd0JBQUM7S0FBQTtTQURZLGlCQUFpQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge09ic2VydmVyc01vZHVsZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL29ic2VydmVycyc7XG5pbXBvcnQge05nTW9kdWxlfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7TWF0Q29tbW9uTW9kdWxlLCBNYXRSaXBwbGVNb2R1bGV9IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL2NvcmUnO1xuaW1wb3J0IHtNYXRDaGVja2JveH0gZnJvbSAnLi9jaGVja2JveCc7XG5pbXBvcnQge01hdENoZWNrYm94UmVxdWlyZWRWYWxpZGF0b3J9IGZyb20gJy4vY2hlY2tib3gtcmVxdWlyZWQtdmFsaWRhdG9yJztcblxuLyoqIFRoaXMgbW9kdWxlIGlzIHVzZWQgYnkgYm90aCBvcmlnaW5hbCBhbmQgTURDLWJhc2VkIGNoZWNrYm94IGltcGxlbWVudGF0aW9ucy4gKi9cbkBOZ01vZHVsZSh7XG4gIGV4cG9ydHM6IFtNYXRDaGVja2JveFJlcXVpcmVkVmFsaWRhdG9yXSxcbiAgZGVjbGFyYXRpb25zOiBbTWF0Q2hlY2tib3hSZXF1aXJlZFZhbGlkYXRvcl0sXG59KVxuLy8gdHNsaW50OmRpc2FibGUtbmV4dC1saW5lOmNsYXNzLW5hbWVcbmV4cG9ydCBjbGFzcyBfTWF0Q2hlY2tib3hSZXF1aXJlZFZhbGlkYXRvck1vZHVsZSB7XG59XG5cbkBOZ01vZHVsZSh7XG4gIGltcG9ydHM6IFtcbiAgICBNYXRSaXBwbGVNb2R1bGUsIE1hdENvbW1vbk1vZHVsZSwgT2JzZXJ2ZXJzTW9kdWxlLFxuICAgIF9NYXRDaGVja2JveFJlcXVpcmVkVmFsaWRhdG9yTW9kdWxlXG4gIF0sXG4gIGV4cG9ydHM6IFtNYXRDaGVja2JveCwgTWF0Q29tbW9uTW9kdWxlLCBfTWF0Q2hlY2tib3hSZXF1aXJlZFZhbGlkYXRvck1vZHVsZV0sXG4gIGRlY2xhcmF0aW9uczogW01hdENoZWNrYm94XSxcbn0pXG5leHBvcnQgY2xhc3MgTWF0Q2hlY2tib3hNb2R1bGUge1xufVxuIl19