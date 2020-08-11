"use strict";
/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.propertyNames = void 0;
const schematics_1 = require("@angular/cdk/schematics");
exports.propertyNames = {
    [schematics_1.TargetVersion.V9]: [
        {
            pr: 'https://github.com/angular/components/pull/17333',
            changes: [
                {
                    replace: 'afterOpen',
                    replaceWith: 'afterOpened',
                    whitelist: { classes: ['MatDialogRef'] }
                },
                {
                    replace: 'beforeClose',
                    replaceWith: 'beforeClosed',
                    whitelist: { classes: ['MatDialogRef'] }
                },
                {
                    replace: 'afterOpen',
                    replaceWith: 'afterOpened',
                    whitelist: { classes: ['MatDialog'] }
                }
            ]
        }
    ],
    [schematics_1.TargetVersion.V6]: [
        {
            pr: 'https://github.com/angular/components/pull/10163',
            changes: [
                { replace: 'change', replaceWith: 'selectionChange', whitelist: { classes: ['MatSelect'] } }, {
                    replace: 'onOpen',
                    replaceWith: 'openedChange.pipe(filter(isOpen => isOpen))',
                    whitelist: { classes: ['MatSelect'] }
                },
                {
                    replace: 'onClose',
                    replaceWith: 'openedChange.pipe(filter(isOpen => !isOpen))',
                    whitelist: { classes: ['MatSelect'] }
                }
            ]
        },
        {
            pr: 'https://github.com/angular/components/pull/10218',
            changes: [{
                    replace: 'align',
                    replaceWith: 'labelPosition',
                    whitelist: { classes: ['MatRadioGroup', 'MatRadioButton'] }
                }]
        },
        {
            pr: 'https://github.com/angular/components/pull/10253',
            changes: [{
                    replace: 'extraClasses',
                    replaceWith: 'panelClass',
                    whitelist: { classes: ['MatSnackBarConfig'] }
                }]
        },
        {
            pr: 'https://github.com/angular/components/pull/10279',
            changes: [
                {
                    replace: 'align',
                    replaceWith: 'position',
                    whitelist: { classes: ['MatDrawer', 'MatSidenav'] }
                },
                {
                    replace: 'onAlignChanged',
                    replaceWith: 'onPositionChanged',
                    whitelist: { classes: ['MatDrawer', 'MatSidenav'] }
                },
                {
                    replace: 'onOpen',
                    replaceWith: 'openedChange.pipe(filter(isOpen => isOpen))',
                    whitelist: { classes: ['MatDrawer', 'MatSidenav'] }
                },
                {
                    replace: 'onClose',
                    replaceWith: 'openedChange.pipe(filter(isOpen => !isOpen))',
                    whitelist: { classes: ['MatDrawer', 'MatSidenav'] }
                }
            ]
        },
        {
            pr: 'https://github.com/angular/components/pull/10293',
            changes: [{
                    replace: 'shouldPlaceholderFloat',
                    replaceWith: 'shouldLabelFloat',
                    whitelist: { classes: ['MatFormFieldControl', 'MatSelect'] }
                }]
        },
        {
            pr: 'https://github.com/angular/components/pull/10294',
            changes: [
                { replace: 'dividerColor', replaceWith: 'color', whitelist: { classes: ['MatFormField'] } }, {
                    replace: 'floatPlaceholder',
                    replaceWith: 'floatLabel',
                    whitelist: { classes: ['MatFormField'] }
                }
            ]
        },
        {
            pr: 'https://github.com/angular/components/pull/10309',
            changes: [
                {
                    replace: 'selectChange',
                    replaceWith: 'selectedTabChange',
                    whitelist: { classes: ['MatTabGroup'] }
                },
                {
                    replace: '_dynamicHeightDeprecated',
                    replaceWith: 'dynamicHeight',
                    whitelist: { classes: ['MatTabGroup'] }
                }
            ]
        },
        {
            pr: 'https://github.com/angular/components/pull/10311',
            changes: [
                { replace: 'destroy', replaceWith: 'destroyed', whitelist: { classes: ['MatChip'] } },
                { replace: 'onRemove', replaceWith: 'removed', whitelist: { classes: ['MatChip'] } }
            ]
        },
        {
            pr: 'https://github.com/angular/components/pull/10342',
            changes: [{ replace: 'align', replaceWith: 'labelPosition', whitelist: { classes: ['MatCheckbox'] } }]
        },
        {
            pr: 'https://github.com/angular/components/pull/10344',
            changes: [{
                    replace: '_positionDeprecated',
                    replaceWith: 'position',
                    whitelist: { classes: ['MatTooltip'] }
                }]
        },
        {
            pr: 'https://github.com/angular/components/pull/10373',
            changes: [
                {
                    replace: '_thumbLabelDeprecated',
                    replaceWith: 'thumbLabel',
                    whitelist: { classes: ['MatSlider'] }
                },
                {
                    replace: '_tickIntervalDeprecated',
                    replaceWith: 'tickInterval',
                    whitelist: { classes: ['MatSlider'] }
                }
            ]
        },
    ]
};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicHJvcGVydHktbmFtZXMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvc2NoZW1hdGljcy9uZy11cGRhdGUvZGF0YS9wcm9wZXJ0eS1uYW1lcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUE7Ozs7OztHQU1HOzs7QUFFSCx3REFBK0Y7QUFFbEYsUUFBQSxhQUFhLEdBQTRDO0lBQ3BFLENBQUMsMEJBQWEsQ0FBQyxFQUFFLENBQUMsRUFBRTtRQUNsQjtZQUNFLEVBQUUsRUFBRSxrREFBa0Q7WUFDdEQsT0FBTyxFQUFFO2dCQUNQO29CQUNFLE9BQU8sRUFBRSxXQUFXO29CQUNwQixXQUFXLEVBQUUsYUFBYTtvQkFDMUIsU0FBUyxFQUFFLEVBQUMsT0FBTyxFQUFFLENBQUMsY0FBYyxDQUFDLEVBQUM7aUJBQ3ZDO2dCQUNEO29CQUNFLE9BQU8sRUFBRSxhQUFhO29CQUN0QixXQUFXLEVBQUUsY0FBYztvQkFDM0IsU0FBUyxFQUFFLEVBQUMsT0FBTyxFQUFFLENBQUMsY0FBYyxDQUFDLEVBQUM7aUJBQ3ZDO2dCQUNEO29CQUNFLE9BQU8sRUFBRSxXQUFXO29CQUNwQixXQUFXLEVBQUUsYUFBYTtvQkFDMUIsU0FBUyxFQUFFLEVBQUMsT0FBTyxFQUFFLENBQUMsV0FBVyxDQUFDLEVBQUM7aUJBQ3BDO2FBQ0Y7U0FDRjtLQUNGO0lBQ0QsQ0FBQywwQkFBYSxDQUFDLEVBQUUsQ0FBQyxFQUFFO1FBQ2xCO1lBQ0UsRUFBRSxFQUFFLGtEQUFrRDtZQUN0RCxPQUFPLEVBQUU7Z0JBQ1AsRUFBQyxPQUFPLEVBQUUsUUFBUSxFQUFFLFdBQVcsRUFBRSxpQkFBaUIsRUFBRSxTQUFTLEVBQUUsRUFBQyxPQUFPLEVBQUUsQ0FBQyxXQUFXLENBQUMsRUFBQyxFQUFDLEVBQUU7b0JBQ3hGLE9BQU8sRUFBRSxRQUFRO29CQUNqQixXQUFXLEVBQUUsNkNBQTZDO29CQUMxRCxTQUFTLEVBQUUsRUFB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QUUsU0FBUyxFQUFFLFdBQVcsRUFBRSxXQUFXLEVBQUUsU0FBUyxFQUFFLEVBQUMsT0FBTyxFQUFFLENBQUMsU0FBUyxDQUFDLEVBQUMsRUFBQztnQkFDakYsRUFBQyxPQUFPLEVBQUUsVUFBVSxFQUFFLFdBQVcsRUFBRSxTQUFTLEVBQUUsU0FBUyxFQUFFLEVBQUMsT0FBTyxFQUFFLENBQUMsU0FBUyxDQUFDLEVBQUMsRUFBQzthQUNqRjtTQUNGO1FBRUQ7WUFDRSxFQUFFLEVBQUUsa0RBQWtEO1lBQ3RELE9BQU8sRUFDSCxDQUFDLEVBQUMsT0FBTyxFQUFFLE9BQU8sRUFBRSxXQUFXLEVBQUUsZUFBZSxFQUFFLFNBQVMsRUFBRSxFQUFDLE9BQU8sRUFBRSxDQUFDLGFBQWEsQ0FBQyxFQUFDLEVBQUMsQ0FBQztTQUM5RjtRQUVEO1lBQ0UsRUFBRSxFQUFFLGtEQUFrRDtZQUN0RCxPQUFPLEVBQUUsQ0FBQztvQkFDUixPQUFPLEVBQUUscUJBQXFCO29CQUM5QixXQUFXLEVBQUUsVUFBVTtvQkFDdkIsU0FBUyxFQUFFLEVBQUMsT0FBTyxFQUFFLENBQUMsWUFBWSxDQUFDLEVBQUM7aUJBQ3JDLENBQUM7U0FDSDtRQUVEO1lBQ0UsRUFBRSxFQUFFLGtEQUFrRDtZQUN0RCxPQUFPLEVBQUU7Z0JBQ1A7b0JBQ0UsT0FBTyxFQUFFLHVCQUF1QjtvQkFDaEMsV0FBVyxFQUFFLFlBQVk7b0JBQ3pCLFNBQVMsRUFBRSxFQUFDLE9BQU8sRUFBRSxDQUFDLFdBQVcsQ0FBQyxFQUFDO2lCQUNwQztnQkFDRDtvQkFDRSxPQUFPLEVBQUUseUJBQXlCO29CQUNsQyxXQUFXLEVBQUUsY0FBYztvQkFDM0IsU0FBUyxFQUFFLEVBQUMsT0FBTyxFQUFFLENBQUMsV0FBVyxDQUFDLEVBQUM7aUJBQ3BDO2FBQ0Y7U0FDRjtLQUNGO0NBQ0YsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge1Byb3BlcnR5TmFtZVVwZ3JhZGVEYXRhLCBUYXJnZXRWZXJzaW9uLCBWZXJzaW9uQ2hhbmdlc30gZnJvbSAnQGFuZ3VsYXIvY2RrL3NjaGVtYXRpY3MnO1xuXG5leHBvcnQgY29uc3QgcHJvcGVydHlOYW1lczogVmVyc2lvbkNoYW5nZXM8UHJvcGVydHlOYW1lVXBncmFkZURhdGE+ID0ge1xuICBbVGFyZ2V0VmVyc2lvbi5WOV06IFtcbiAgICB7XG4gICAgICBwcjogJ2h0dHBzOi8vZ2l0aHViLmNvbS9hbmd1bGFyL2NvbXBvbmVudHMvcHVsbC8xNzMzMycsXG4gICAgICBjaGFuZ2VzOiBbXG4gICAgICAgIHtcbiAgICAgICAgICByZXBsYWNlOiAnYWZ0ZXJPcGVuJyxcbiAgICAgICAgICByZXBsYWNlV2l0aDogJ2FmdGVyT3BlbmVkJyxcbiAgICAgICAgICB3aGl0ZWxpc3Q6IHtjbGFzc2VzOiBbJ01hdERpYWxvZ1JlZiddfVxuICAgICAgICB9LFxuICAgICAgICB7XG4gICAgICAgICAgcmVwbGFjZTogJ2JlZm9yZUNsb3NlJyxcbiAgICAgICAgICByZXBsYWNlV2l0aDogJ2JlZm9yZUNsb3NlZCcsXG4gICAgICAgICAgd2hpdGVsaXN0OiB7Y2xhc3NlczogWydNYXREaWFsb2dSZWYnXX1cbiAgICAgICAgfSxcbiAgICAgICAge1xuICAgICAgICAgIHJlcGxhY2U6ICdhZnRlck9wZW4nLFxuICAgICAgICAgIHJlcGxhY2VXaXRoOiAnYWZ0ZXJPcGVuZWQnLFxuICAgICAgICAgIHdoaXRlbGlzdDoge2NsYXNzZXM6IFsnTWF0RGlhbG9nJ119XG4gICAgICAgIH1cbiAgICAgIF1cbiAgICB9XG4gIF0sXG4gIFtUYXJnZXRWZXJzaW9uLlY2XTogW1xuICAgIHtcbiAgICAgIHByOiAnaHR0cHM6Ly9naXRodWIuY29tL2FuZ3VsYXIvY29tcG9uZW50cy9wdWxsLzEwMTYzJyxcbiAgICAgIGNoYW5nZXM6IFtcbiAgICAgICAge3JlcGxhY2U6ICdjaGFuZ2UnLCByZXBsYWNlV2l0aDogJ3NlbGVjdGlvbkNoYW5nZScsIHdoaXRlbGlzdDoge2NsYXNzZXM6IFsnTWF0U2VsZWN0J119fSwge1xuICAgICAgICAgIHJlcGxhY2U6ICdvbk9wZW4nLFxuICAgICAgICAgIHJlcGxhY2VXaXRoOiAnb3BlbmVkQ2hhbmdlLnBpcGUoZmlsdGVyKGlzT3BlbiA9PiBpc09wZW4pKScsXG4gICAgICAgICAgd2hpdGVsaXN0OiB7Y2xhc3NlczogWydNYXRTZWxlY3QnXX1cbiAgICAgICAgfSxcbiAgICAgICAge1xuICAgICAgICAgIHJlcGxhY2U6ICdvbkNsb3NlJyxcbiAgICAgICAgICByZXBsYWNlV2l0aDogJ29wZW5lZENoYW5nZS5waXBlKGZpbHRlcihpc09wZW4gPT4gIWlzT3BlbikpJyxcbiAgICAgICAgICB3aGl0ZWxpc3Q6IHtjbGFzc2VzOiBbJ01hdFNlbGVjdCddfVxuICAgICAgICB9XG4gICAgICBdXG4gICAgfSxcblxuICAgIHtcbiAgICAgIHByOiAnaHR0cHM6Ly9naXRodWIuY29tL2FuZ3VsYXIvY29tcG9uZW50cy9wdWxsLzEwMjE4JyxcbiAgICAgIGNoYW5nZXM6IFt7XG4gICAgICAgIHJlcGxhY2U6ICdhbGlnbicsXG4gICAgICAgIHJlcGxhY2VXaXRoOiAnbGFiZWxQb3NpdGlvbicsXG4gICAgICAgIHdoaXRlbGlzdDoge2NsYXNzZXM6IFsnTWF0UmFkaW9Hcm91cCcsICdNYXRSYWRpb0J1dHRvbiddfVxuICAgICAgfV1cbiAgICB9LFxuXG4gICAge1xuICAgICAgcHI6ICdodHRwczovL2dpdGh1Yi5jb20vYW5ndWxhci9jb21wb25lbnRzL3B1bGwvMTAyNTMnLFxuICAgICAgY2hhbmdlczogW3tcbiAgICAgICAgcmVwbGFjZTogJ2V4dHJhQ2xhc3NlcycsXG4gICAgICAgIHJlcGxhY2VXaXRoOiAncGFuZWxDbGFzcycsXG4gICAgICAgIHdoaXRlbGlzdDoge2NsYXNzZXM6IFsnTWF0U25hY2tCYXJDb25maWcnXX1cbiAgICAgIH1dXG4gICAgfSxcblxuICAgIHtcbiAgICAgIHByOiAnaHR0cHM6Ly9naXRodWIuY29tL2FuZ3VsYXIvY29tcG9uZW50cy9wdWxsLzEwMjc5JyxcbiAgICAgIGNoYW5nZXM6IFtcbiAgICAgICAge1xuICAgICAgICAgIHJlcGxhY2U6ICdhbGlnbicsXG4gICAgICAgICAgcmVwbGFjZVdpdGg6ICdwb3NpdGlvbicsXG4gICAgICAgICAgd2hpdGVsaXN0OiB7Y2xhc3NlczogWydNYXREcmF3ZXInLCAnTWF0U2lkZW5hdiddfVxuICAgICAgICB9LFxuICAgICAgICB7XG4gICAgICAgICAgcmVwbGFjZTogJ29uQWxpZ25DaGFuZ2VkJyxcbiAgICAgICAgICByZXBsYWNlV2l0aDogJ29uUG9zaXRpb25DaGFuZ2VkJyxcbiAgICAgICAgICB3aGl0ZWxpc3Q6IHtjbGFzc2VzOiBbJ01hdERyYXdlcicsICdNYXRTaWRlbmF2J119XG4gICAgICAgIH0sXG4gICAgICAgIHtcbiAgICAgICAgICByZXBsYWNlOiAnb25PcGVuJyxcbiAgICAgICAgICByZXBsYWNlV2l0aDogJ29wZW5lZENoYW5nZS5waXBlKGZpbHRlcihpc09wZW4gPT4gaXNPcGVuKSknLFxuICAgICAgICAgIHdoaXRlbGlzdDoge2NsYXNzZXM6IFsnTWF0RHJhd2VyJywgJ01hdFNpZGVuYXYnXX1cbiAgICAgICAgfSxcbiAgICAgICAge1xuICAgICAgICAgIHJlcGxhY2U6ICdvbkNsb3NlJyxcbiAgICAgICAgICByZXBsYWNlV2l0aDogJ29wZW5lZENoYW5nZS5waXBlKGZpbHRlcihpc09wZW4gPT4gIWlzT3BlbikpJyxcbiAgICAgICAgICB3aGl0ZWxpc3Q6IHtjbGFzc2VzOiBbJ01hdERyYXdlcicsICdNYXRTaWRlbmF2J119XG4gICAgICAgIH1cbiAgICAgIF1cbiAgICB9LFxuXG4gICAge1xuICAgICAgcHI6ICdodHRwczovL2dpdGh1Yi5jb20vYW5ndWxhci9jb21wb25lbnRzL3B1bGwvMTAyOTMnLFxuICAgICAgY2hhbmdlczogW3tcbiAgICAgICAgcmVwbGFjZTogJ3Nob3VsZFBsYWNlaG9sZGVyRmxvYXQnLFxuICAgICAgICByZXBsYWNlV2l0aDogJ3Nob3VsZExhYmVsRmxvYXQnLFxuICAgICAgICB3aGl0ZWxpc3Q6IHtjbGFzc2VzOiBbJ01hdEZvcm1GaWVsZENvbnRyb2wnLCAnTWF0U2VsZWN0J119XG4gICAgICB9XVxuICAgIH0sXG5cbiAgICB7XG4gICAgICBwcjogJ2h0dHBzOi8vZ2l0aHViLmNvbS9hbmd1bGFyL2NvbXBvbmVudHMvcHVsbC8xMDI5NCcsXG4gICAgICBjaGFuZ2VzOiBbXG4gICAgICAgIHtyZXBsYWNlOiAnZGl2aWRlckNvbG9yJywgcmVwbGFjZVdpdGg6ICdjb2xvcicsIHdoaXRlbGlzdDoge2NsYXNzZXM6IFsnTWF0Rm9ybUZpZWxkJ119fSwge1xuICAgICAgICAgIHJlcGxhY2U6ICdmbG9hdFBsYWNlaG9sZGVyJyxcbiAgICAgICAgICByZXBsYWNlV2l0aDogJ2Zsb2F0TGFiZWwnLFxuICAgICAgICAgIHdoaXRlbGlzdDoge2NsYXNzZXM6IFsnTWF0Rm9ybUZpZWxkJ119XG4gICAgICAgIH1cbiAgICAgIF1cbiAgICB9LFxuXG4gICAge1xuICAgICAgcHI6ICdodHRwczovL2dpdGh1Yi5jb20vYW5ndWxhci9jb21wb25lbnRzL3B1bGwvMTAzMDknLFxuICAgICAgY2hhbmdlczogW1xuICAgICAgICB7XG4gICAgICAgICAgcmVwbGFjZTogJ3NlbGVjdENoYW5nZScsXG4gICAgICAgICAgcmVwbGFjZVdpdGg6ICdzZWxlY3RlZFRhYkNoYW5nZScsXG4gICAgICAgICAgd2hpdGVsaXN0OiB7Y2xhc3NlczogWydNYXRUYWJHcm91cCddfVxuICAgICAgICB9LFxuICAgICAgICB7XG4gICAgICAgICAgcmVwbGFjZTogJ19keW5hbWljSGVpZ2h0RGVwcmVjYXRlZCcsXG4gICAgICAgICAgcmVwbGFjZVdpdGg6ICdkeW5hbWljSGVpZ2h0JyxcbiAgICAgICAgICB3aGl0ZWxpc3Q6IHtjbGFzc2VzOiBbJ01hdFRhYkdyb3VwJ119XG4gICAgICAgIH1cbiAgICAgIF1cbiAgICB9LFxuXG4gICAge1xuICAgICAgcHI6ICdodHRwczovL2dpdGh1Yi5jb20vYW5ndWxhci9jb21wb25lbnRzL3B1bGwvMTAzMTEnLFxuICAgICAgY2hhbmdlczogW1xuICAgICAgICB7cmVwbGFjZTogJ2Rlc3Ryb3knLCByZXBsYWNlV2l0aDogJ2Rlc3Ryb3llZCcsIHdoaXRlbGlzdDoge2NsYXNzZXM6IFsnTWF0Q2hpcCddfX0sXG4gICAgICAgIHtyZXBsYWNlOiAnb25SZW1vdmUnLCByZXBsYWNlV2l0aDogJ3JlbW92ZWQnLCB3aGl0ZWxpc3Q6IHtjbGFzc2VzOiBbJ01hdENoaXAnXX19XG4gICAgICBdXG4gICAgfSxcblxuICAgIHtcbiAgICAgIHByOiAnaHR0cHM6Ly9naXRodWIuY29tL2FuZ3VsYXIvY29tcG9uZW50cy9wdWxsLzEwMzQyJyxcbiAgICAgIGNoYW5nZXM6XG4gICAgICAgICAgW3tyZXBsYWNlOiAnYWxpZ24nLCByZXBsYWNlV2l0aDogJ2xhYmVsUG9zaXRpb24nLCB3aGl0ZWxpc3Q6IHtjbGFzc2VzOiBbJ01hdENoZWNrYm94J119fV1cbiAgICB9LFxuXG4gICAge1xuICAgICAgcHI6ICdodHRwczovL2dpdGh1Yi5jb20vYW5ndWxhci9jb21wb25lbnRzL3B1bGwvMTAzNDQnLFxuICAgICAgY2hhbmdlczogW3tcbiAgICAgICAgcmVwbGFjZTogJ19wb3NpdGlvbkRlcHJlY2F0ZWQnLFxuICAgICAgICByZXBsYWNlV2l0aDogJ3Bvc2l0aW9uJyxcbiAgICAgICAgd2hpdGVsaXN0OiB7Y2xhc3NlczogWydNYXRUb29sdGlwJ119XG4gICAgICB9XVxuICAgIH0sXG5cbiAgICB7XG4gICAgICBwcjogJ2h0dHBzOi8vZ2l0aHViLmNvbS9hbmd1bGFyL2NvbXBvbmVudHMvcHVsbC8xMDM3MycsXG4gICAgICBjaGFuZ2VzOiBbXG4gICAgICAgIHtcbiAgICAgICAgICByZXBsYWNlOiAnX3RodW1iTGFiZWxEZXByZWNhdGVkJyxcbiAgICAgICAgICByZXBsYWNlV2l0aDogJ3RodW1iTGFiZWwnLFxuICAgICAgICAgIHdoaXRlbGlzdDoge2NsYXNzZXM6IFsnTWF0U2xpZGVyJ119XG4gICAgICAgIH0sXG4gICAgICAgIHtcbiAgICAgICAgICByZXBsYWNlOiAnX3RpY2tJbnRlcnZhbERlcHJlY2F0ZWQnLFxuICAgICAgICAgIHJlcGxhY2VXaXRoOiAndGlja0ludGVydmFsJyxcbiAgICAgICAgICB3aGl0ZWxpc3Q6IHtjbGFzc2VzOiBbJ01hdFNsaWRlciddfVxuICAgICAgICB9XG4gICAgICBdXG4gICAgfSxcbiAgXVxufTtcbiJdfQ==