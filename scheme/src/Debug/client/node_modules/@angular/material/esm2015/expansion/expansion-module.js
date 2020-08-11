/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { CdkAccordionModule } from '@angular/cdk/accordion';
import { PortalModule } from '@angular/cdk/portal';
import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { MatAccordion } from './accordion';
import { MatExpansionPanel, MatExpansionPanelActionRow } from './expansion-panel';
import { MatExpansionPanelContent } from './expansion-panel-content';
import { MatExpansionPanelDescription, MatExpansionPanelHeader, MatExpansionPanelTitle, } from './expansion-panel-header';
let MatExpansionModule = /** @class */ (() => {
    class MatExpansionModule {
    }
    MatExpansionModule.decorators = [
        { type: NgModule, args: [{
                    imports: [CommonModule, CdkAccordionModule, PortalModule],
                    exports: [
                        MatAccordion,
                        MatExpansionPanel,
                        MatExpansionPanelActionRow,
                        MatExpansionPanelHeader,
                        MatExpansionPanelTitle,
                        MatExpansionPanelDescription,
                        MatExpansionPanelContent,
                    ],
                    declarations: [
                        MatAccordion,
                        MatExpansionPanel,
                        MatExpansionPanelActionRow,
                        MatExpansionPanelHeader,
                        MatExpansionPanelTitle,
                        MatExpansionPanelDescription,
                        MatExpansionPanelContent,
                    ],
                },] }
    ];
    return MatExpansionModule;
})();
export { MatExpansionModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZXhwYW5zaW9uLW1vZHVsZS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9leHBhbnNpb24vZXhwYW5zaW9uLW1vZHVsZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsa0JBQWtCLEVBQUMsTUFBTSx3QkFBd0IsQ0FBQztBQUMxRCxPQUFPLEVBQUMsWUFBWSxFQUFDLE1BQU0scUJBQXFCLENBQUM7QUFDakQsT0FBTyxFQUFDLFlBQVksRUFBQyxNQUFNLGlCQUFpQixDQUFDO0FBQzdDLE9BQU8sRUFBQyxRQUFRLEVBQUMsTUFBTSxlQUFlLENBQUM7QUFDdkMsT0FBTyxFQUFDLFlBQVksRUFBQyxNQUFNLGFBQWEsQ0FBQztBQUN6QyxPQUFPLEVBQUMsaUJBQWlCLEVBQUUsMEJBQTBCLEVBQUMsTUFBTSxtQkFBbUIsQ0FBQztBQUNoRixPQUFPLEVBQUMsd0JBQXdCLEVBQUMsTUFBTSwyQkFBMkIsQ0FBQztBQUNuRSxPQUFPLEVBQ0wsNEJBQTRCLEVBQzVCLHVCQUF1QixFQUN2QixzQkFBc0IsR0FDdkIsTUFBTSwwQkFBMEIsQ0FBQztBQUdsQztJQUFBLE1BcUJhLGtCQUFrQjs7O2dCQXJCOUIsUUFBUSxTQUFDO29CQUNSLE9BQU8sRUFBRSxDQUFDLFlBQVksRUFBRSxrQkFBa0IsRUFBRSxZQUFZLENBQUM7b0JBQ3pELE9BQU8sRUFBRTt3QkFDUCxZQUFZO3dCQUNaLGlCQUFpQjt3QkFDakIsMEJBQTBCO3dCQUMxQix1QkFBdUI7d0JBQ3ZCLHNCQUFzQjt3QkFDdEIsNEJBQTRCO3dCQUM1Qix3QkFBd0I7cUJBQ3pCO29CQUNELFlBQVksRUFBRTt3QkFDWixZQUFZO3dCQUNaLGlCQUFpQjt3QkFDakIsMEJBQTBCO3dCQUMxQix1QkFBdUI7d0JBQ3ZCLHNCQUFzQjt3QkFDdEIsNEJBQTRCO3dCQUM1Qix3QkFBd0I7cUJBQ3pCO2lCQUNGOztJQUNnQyx5QkFBQztLQUFBO1NBQXJCLGtCQUFrQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0Nka0FjY29yZGlvbk1vZHVsZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL2FjY29yZGlvbic7XG5pbXBvcnQge1BvcnRhbE1vZHVsZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3BvcnRhbCc7XG5pbXBvcnQge0NvbW1vbk1vZHVsZX0gZnJvbSAnQGFuZ3VsYXIvY29tbW9uJztcbmltcG9ydCB7TmdNb2R1bGV9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtNYXRBY2NvcmRpb259IGZyb20gJy4vYWNjb3JkaW9uJztcbmltcG9ydCB7TWF0RXhwYW5zaW9uUGFuZWwsIE1hdEV4cGFuc2lvblBhbmVsQWN0aW9uUm93fSBmcm9tICcuL2V4cGFuc2lvbi1wYW5lbCc7XG5pbXBvcnQge01hdEV4cGFuc2lvblBhbmVsQ29udGVudH0gZnJvbSAnLi9leHBhbnNpb24tcGFuZWwtY29udGVudCc7XG5pbXBvcnQge1xuICBNYXRFeHBhbnNpb25QYW5lbERlc2NyaXB0aW9uLFxuICBNYXRFeHBhbnNpb25QYW5lbEhlYWRlcixcbiAgTWF0RXhwYW5zaW9uUGFuZWxUaXRsZSxcbn0gZnJvbSAnLi9leHBhbnNpb24tcGFuZWwtaGVhZGVyJztcblxuXG5ATmdNb2R1bGUoe1xuICBpbXBvcnRzOiBbQ29tbW9uTW9kdWxlLCBDZGtBY2NvcmRpb25Nb2R1bGUsIFBvcnRhbE1vZHVsZV0sXG4gIGV4cG9ydHM6IFtcbiAgICBNYXRBY2NvcmRpb24sXG4gICAgTWF0RXhwYW5zaW9uUGFuZWwsXG4gICAgTWF0RXhwYW5zaW9uUGFuZWxBY3Rpb25Sb3csXG4gICAgTWF0RXhwYW5zaW9uUGFuZWxIZWFkZXIsXG4gICAgTWF0RXhwYW5zaW9uUGFuZWxUaXRsZSxcbiAgICBNYXRFeHBhbnNpb25QYW5lbERlc2NyaXB0aW9uLFxuICAgIE1hdEV4cGFuc2lvblBhbmVsQ29udGVudCxcbiAgXSxcbiAgZGVjbGFyYXRpb25zOiBbXG4gICAgTWF0QWNjb3JkaW9uLFxuICAgIE1hdEV4cGFuc2lvblBhbmVsLFxuICAgIE1hdEV4cGFuc2lvblBhbmVsQWN0aW9uUm93LFxuICAgIE1hdEV4cGFuc2lvblBhbmVsSGVhZGVyLFxuICAgIE1hdEV4cGFuc2lvblBhbmVsVGl0bGUsXG4gICAgTWF0RXhwYW5zaW9uUGFuZWxEZXNjcmlwdGlvbixcbiAgICBNYXRFeHBhbnNpb25QYW5lbENvbnRlbnQsXG4gIF0sXG59KVxuZXhwb3J0IGNsYXNzIE1hdEV4cGFuc2lvbk1vZHVsZSB7fVxuIl19