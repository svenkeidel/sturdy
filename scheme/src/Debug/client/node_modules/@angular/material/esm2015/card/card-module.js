/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { NgModule } from '@angular/core';
import { MatCommonModule } from '@angular/material/core';
import { MatCard, MatCardActions, MatCardAvatar, MatCardContent, MatCardFooter, MatCardHeader, MatCardImage, MatCardLgImage, MatCardMdImage, MatCardSmImage, MatCardSubtitle, MatCardTitle, MatCardTitleGroup, MatCardXlImage, } from './card';
let MatCardModule = /** @class */ (() => {
    class MatCardModule {
    }
    MatCardModule.decorators = [
        { type: NgModule, args: [{
                    imports: [MatCommonModule],
                    exports: [
                        MatCard,
                        MatCardHeader,
                        MatCardTitleGroup,
                        MatCardContent,
                        MatCardTitle,
                        MatCardSubtitle,
                        MatCardActions,
                        MatCardFooter,
                        MatCardSmImage,
                        MatCardMdImage,
                        MatCardLgImage,
                        MatCardImage,
                        MatCardXlImage,
                        MatCardAvatar,
                        MatCommonModule,
                    ],
                    declarations: [
                        MatCard, MatCardHeader, MatCardTitleGroup, MatCardContent, MatCardTitle, MatCardSubtitle,
                        MatCardActions, MatCardFooter, MatCardSmImage, MatCardMdImage, MatCardLgImage, MatCardImage,
                        MatCardXlImage, MatCardAvatar,
                    ],
                },] }
    ];
    return MatCardModule;
})();
export { MatCardModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2FyZC1tb2R1bGUuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvY2FyZC9jYXJkLW1vZHVsZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7QUFFSCxPQUFPLEVBQUMsUUFBUSxFQUFDLE1BQU0sZUFBZSxDQUFDO0FBQ3ZDLE9BQU8sRUFBQyxlQUFlLEVBQUMsTUFBTSx3QkFBd0IsQ0FBQztBQUN2RCxPQUFPLEVBQ0wsT0FBTyxFQUNQLGNBQWMsRUFDZCxhQUFhLEVBQ2IsY0FBYyxFQUNkLGFBQWEsRUFDYixhQUFhLEVBQ2IsWUFBWSxFQUNaLGNBQWMsRUFDZCxjQUFjLEVBQ2QsY0FBYyxFQUNkLGVBQWUsRUFDZixZQUFZLEVBQ1osaUJBQWlCLEVBQ2pCLGNBQWMsR0FDZixNQUFNLFFBQVEsQ0FBQztBQUdoQjtJQUFBLE1BeUJhLGFBQWE7OztnQkF6QnpCLFFBQVEsU0FBQztvQkFDUixPQUFPLEVBQUUsQ0FBQyxlQUFlLENBQUM7b0JBQzFCLE9BQU8sRUFBRTt3QkFDUCxPQUFPO3dCQUNQLGFBQWE7d0JBQ2IsaUJBQWlCO3dCQUNqQixjQUFjO3dCQUNkLFlBQVk7d0JBQ1osZUFBZTt3QkFDZixjQUFjO3dCQUNkLGFBQWE7d0JBQ2IsY0FBYzt3QkFDZCxjQUFjO3dCQUNkLGNBQWM7d0JBQ2QsWUFBWTt3QkFDWixjQUFjO3dCQUNkLGFBQWE7d0JBQ2IsZUFBZTtxQkFDaEI7b0JBQ0QsWUFBWSxFQUFFO3dCQUNaLE9BQU8sRUFBRSxhQUFhLEVBQUUsaUJBQWlCLEVBQUUsY0FBYyxFQUFFLFlBQVksRUFBRSxlQUFlO3dCQUN4RixjQUFjLEVBQUUsYUFBYSxFQUFFLGNBQWMsRUFBRSxjQUFjLEVBQUUsY0FBYyxFQUFFLFlBQVk7d0JBQzNGLGNBQWMsRUFBRSxhQUFhO3FCQUM5QjtpQkFDRjs7SUFDMkIsb0JBQUM7S0FBQTtTQUFoQixhQUFhIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7TmdNb2R1bGV9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtNYXRDb21tb25Nb2R1bGV9IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL2NvcmUnO1xuaW1wb3J0IHtcbiAgTWF0Q2FyZCxcbiAgTWF0Q2FyZEFjdGlvbnMsXG4gIE1hdENhcmRBdmF0YXIsXG4gIE1hdENhcmRDb250ZW50LFxuICBNYXRDYXJkRm9vdGVyLFxuICBNYXRDYXJkSGVhZGVyLFxuICBNYXRDYXJkSW1hZ2UsXG4gIE1hdENhcmRMZ0ltYWdlLFxuICBNYXRDYXJkTWRJbWFnZSxcbiAgTWF0Q2FyZFNtSW1hZ2UsXG4gIE1hdENhcmRTdWJ0aXRsZSxcbiAgTWF0Q2FyZFRpdGxlLFxuICBNYXRDYXJkVGl0bGVHcm91cCxcbiAgTWF0Q2FyZFhsSW1hZ2UsXG59IGZyb20gJy4vY2FyZCc7XG5cblxuQE5nTW9kdWxlKHtcbiAgaW1wb3J0czogW01hdENvbW1vbk1vZHVsZV0sXG4gIGV4cG9ydHM6IFtcbiAgICBNYXRDYXJkLFxuICAgIE1hdENhcmRIZWFkZXIsXG4gICAgTWF0Q2FyZFRpdGxlR3JvdXAsXG4gICAgTWF0Q2FyZENvbnRlbnQsXG4gICAgTWF0Q2FyZFRpdGxlLFxuICAgIE1hdENhcmRTdWJ0aXRsZSxcbiAgICBNYXRDYXJkQWN0aW9ucyxcbiAgICBNYXRDYXJkRm9vdGVyLFxuICAgIE1hdENhcmRTbUltYWdlLFxuICAgIE1hdENhcmRNZEltYWdlLFxuICAgIE1hdENhcmRMZ0ltYWdlLFxuICAgIE1hdENhcmRJbWFnZSxcbiAgICBNYXRDYXJkWGxJbWFnZSxcbiAgICBNYXRDYXJkQXZhdGFyLFxuICAgIE1hdENvbW1vbk1vZHVsZSxcbiAgXSxcbiAgZGVjbGFyYXRpb25zOiBbXG4gICAgTWF0Q2FyZCwgTWF0Q2FyZEhlYWRlciwgTWF0Q2FyZFRpdGxlR3JvdXAsIE1hdENhcmRDb250ZW50LCBNYXRDYXJkVGl0bGUsIE1hdENhcmRTdWJ0aXRsZSxcbiAgICBNYXRDYXJkQWN0aW9ucywgTWF0Q2FyZEZvb3RlciwgTWF0Q2FyZFNtSW1hZ2UsIE1hdENhcmRNZEltYWdlLCBNYXRDYXJkTGdJbWFnZSwgTWF0Q2FyZEltYWdlLFxuICAgIE1hdENhcmRYbEltYWdlLCBNYXRDYXJkQXZhdGFyLFxuICBdLFxufSlcbmV4cG9ydCBjbGFzcyBNYXRDYXJkTW9kdWxlIHt9XG4iXX0=