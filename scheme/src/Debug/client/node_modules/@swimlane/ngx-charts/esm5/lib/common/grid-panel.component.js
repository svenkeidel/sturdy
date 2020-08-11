import { __decorate } from "tslib";
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
var GridPanelComponent = /** @class */ (function () {
    function GridPanelComponent() {
    }
    __decorate([
        Input()
    ], GridPanelComponent.prototype, "path", void 0);
    __decorate([
        Input()
    ], GridPanelComponent.prototype, "width", void 0);
    __decorate([
        Input()
    ], GridPanelComponent.prototype, "height", void 0);
    __decorate([
        Input()
    ], GridPanelComponent.prototype, "x", void 0);
    __decorate([
        Input()
    ], GridPanelComponent.prototype, "y", void 0);
    GridPanelComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-grid-panel]',
            template: "\n    <svg:rect [attr.height]=\"height\" [attr.width]=\"width\" [attr.x]=\"x\" [attr.y]=\"y\" stroke=\"none\" class=\"gridpanel\" />\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], GridPanelComponent);
    return GridPanelComponent;
}());
export { GridPanelComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZ3JpZC1wYW5lbC5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vZ3JpZC1wYW5lbC5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFBRSxTQUFTLEVBQUUsS0FBSyxFQUFFLHVCQUF1QixFQUFFLE1BQU0sZUFBZSxDQUFDO0FBUzFFO0lBQUE7SUFNQSxDQUFDO0lBTFU7UUFBUixLQUFLLEVBQUU7b0RBQU07SUFDTDtRQUFSLEtBQUssRUFBRTtxREFBTztJQUNOO1FBQVIsS0FBSyxFQUFFO3NEQUFRO0lBQ1A7UUFBUixLQUFLLEVBQUU7aURBQUc7SUFDRjtRQUFSLEtBQUssRUFBRTtpREFBRztJQUxBLGtCQUFrQjtRQVA5QixTQUFTLENBQUM7WUFDVCxRQUFRLEVBQUUsMEJBQTBCO1lBQ3BDLFFBQVEsRUFBRSwwSUFFVDtZQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO1NBQ2hELENBQUM7T0FDVyxrQkFBa0IsQ0FNOUI7SUFBRCx5QkFBQztDQUFBLEFBTkQsSUFNQztTQU5ZLGtCQUFrQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IENvbXBvbmVudCwgSW5wdXQsIENoYW5nZURldGVjdGlvblN0cmF0ZWd5IH0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ2dbbmd4LWNoYXJ0cy1ncmlkLXBhbmVsXScsXG4gIHRlbXBsYXRlOiBgXG4gICAgPHN2ZzpyZWN0IFthdHRyLmhlaWdodF09XCJoZWlnaHRcIiBbYXR0ci53aWR0aF09XCJ3aWR0aFwiIFthdHRyLnhdPVwieFwiIFthdHRyLnldPVwieVwiIHN0cm9rZT1cIm5vbmVcIiBjbGFzcz1cImdyaWRwYW5lbFwiIC8+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIEdyaWRQYW5lbENvbXBvbmVudCB7XG4gIEBJbnB1dCgpIHBhdGg7XG4gIEBJbnB1dCgpIHdpZHRoO1xuICBASW5wdXQoKSBoZWlnaHQ7XG4gIEBJbnB1dCgpIHg7XG4gIEBJbnB1dCgpIHk7XG59XG4iXX0=