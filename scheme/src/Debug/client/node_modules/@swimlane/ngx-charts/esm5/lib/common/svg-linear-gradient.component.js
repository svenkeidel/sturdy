import { __decorate } from "tslib";
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
var SvgLinearGradientComponent = /** @class */ (function () {
    function SvgLinearGradientComponent() {
        this.orientation = 'vertical';
    }
    SvgLinearGradientComponent.prototype.ngOnChanges = function (changes) {
        this.x1 = '0%';
        this.x2 = '0%';
        this.y1 = '0%';
        this.y2 = '0%';
        if (this.orientation === 'horizontal') {
            this.x2 = '100%';
        }
        else if (this.orientation === 'vertical') {
            this.y1 = '100%';
        }
    };
    __decorate([
        Input()
    ], SvgLinearGradientComponent.prototype, "orientation", void 0);
    __decorate([
        Input()
    ], SvgLinearGradientComponent.prototype, "name", void 0);
    __decorate([
        Input()
    ], SvgLinearGradientComponent.prototype, "stops", void 0);
    SvgLinearGradientComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-svg-linear-gradient]',
            template: "\n    <svg:linearGradient [id]=\"name\" [attr.x1]=\"x1\" [attr.y1]=\"y1\" [attr.x2]=\"x2\" [attr.y2]=\"y2\">\n      <svg:stop\n        *ngFor=\"let stop of stops\"\n        [attr.offset]=\"stop.offset + '%'\"\n        [style.stop-color]=\"stop.color\"\n        [style.stop-opacity]=\"stop.opacity\"\n      />\n    </svg:linearGradient>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], SvgLinearGradientComponent);
    return SvgLinearGradientComponent;
}());
export { SvgLinearGradientComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic3ZnLWxpbmVhci1ncmFkaWVudC5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vc3ZnLWxpbmVhci1ncmFkaWVudC5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFBRSxTQUFTLEVBQUUsS0FBSyxFQUE0Qix1QkFBdUIsRUFBRSxNQUFNLGVBQWUsQ0FBQztBQWdCcEc7SUFBQTtRQUNXLGdCQUFXLEdBQUcsVUFBVSxDQUFDO0lBcUJwQyxDQUFDO0lBWkMsZ0RBQVcsR0FBWCxVQUFZLE9BQXNCO1FBQ2hDLElBQUksQ0FBQyxFQUFFLEdBQUcsSUFBSSxDQUFDO1FBQ2YsSUFBSSxDQUFDLEVBQUUsR0FBRyxJQUFJLENBQUM7UUFDZixJQUFJLENBQUMsRUFBRSxHQUFHLElBQUksQ0FBQztRQUNmLElBQUksQ0FBQyxFQUFFLEdBQUcsSUFBSSxDQUFDO1FBRWYsSUFBSSxJQUFJLENBQUMsV0FBVyxLQUFLLFlBQVksRUFBRTtZQUNyQyxJQUFJLENBQUMsRUFBRSxHQUFHLE1BQU0sQ0FBQztTQUNsQjthQUFNLElBQUksSUFBSSxDQUFDLFdBQVcsS0FBSyxVQUFVLEVBQUU7WUFDMUMsSUFBSSxDQUFDLEVBQUUsR0FBRyxNQUFNLENBQUM7U0FDbEI7SUFDSCxDQUFDO0lBcEJRO1FBQVIsS0FBSyxFQUFFO21FQUEwQjtJQUN6QjtRQUFSLEtBQUssRUFBRTs0REFBTTtJQUNMO1FBQVIsS0FBSyxFQUFFOzZEQUFjO0lBSFgsMEJBQTBCO1FBZHRDLFNBQVMsQ0FBQztZQUNULFFBQVEsRUFBRSxtQ0FBbUM7WUFDN0MsUUFBUSxFQUFFLHFWQVNUO1lBQ0QsZUFBZSxFQUFFLHVCQUF1QixDQUFDLE1BQU07U0FDaEQsQ0FBQztPQUNXLDBCQUEwQixDQXNCdEM7SUFBRCxpQ0FBQztDQUFBLEFBdEJELElBc0JDO1NBdEJZLDBCQUEwQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IENvbXBvbmVudCwgSW5wdXQsIE9uQ2hhbmdlcywgU2ltcGxlQ2hhbmdlcywgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kgfSBmcm9tICdAYW5ndWxhci9jb3JlJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnZ1tuZ3gtY2hhcnRzLXN2Zy1saW5lYXItZ3JhZGllbnRdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOmxpbmVhckdyYWRpZW50IFtpZF09XCJuYW1lXCIgW2F0dHIueDFdPVwieDFcIiBbYXR0ci55MV09XCJ5MVwiIFthdHRyLngyXT1cIngyXCIgW2F0dHIueTJdPVwieTJcIj5cbiAgICAgIDxzdmc6c3RvcFxuICAgICAgICAqbmdGb3I9XCJsZXQgc3RvcCBvZiBzdG9wc1wiXG4gICAgICAgIFthdHRyLm9mZnNldF09XCJzdG9wLm9mZnNldCArICclJ1wiXG4gICAgICAgIFtzdHlsZS5zdG9wLWNvbG9yXT1cInN0b3AuY29sb3JcIlxuICAgICAgICBbc3R5bGUuc3RvcC1vcGFjaXR5XT1cInN0b3Aub3BhY2l0eVwiXG4gICAgICAvPlxuICAgIDwvc3ZnOmxpbmVhckdyYWRpZW50PlxuICBgLFxuICBjaGFuZ2VEZXRlY3Rpb246IENoYW5nZURldGVjdGlvblN0cmF0ZWd5Lk9uUHVzaFxufSlcbmV4cG9ydCBjbGFzcyBTdmdMaW5lYXJHcmFkaWVudENvbXBvbmVudCBpbXBsZW1lbnRzIE9uQ2hhbmdlcyB7XG4gIEBJbnB1dCgpIG9yaWVudGF0aW9uID0gJ3ZlcnRpY2FsJztcbiAgQElucHV0KCkgbmFtZTtcbiAgQElucHV0KCkgc3RvcHM6IGFueVtdO1xuXG4gIHgxOiBhbnk7XG4gIHgyOiBhbnk7XG4gIHkxOiBhbnk7XG4gIHkyOiBhbnk7XG5cbiAgbmdPbkNoYW5nZXMoY2hhbmdlczogU2ltcGxlQ2hhbmdlcyk6IHZvaWQge1xuICAgIHRoaXMueDEgPSAnMCUnO1xuICAgIHRoaXMueDIgPSAnMCUnO1xuICAgIHRoaXMueTEgPSAnMCUnO1xuICAgIHRoaXMueTIgPSAnMCUnO1xuXG4gICAgaWYgKHRoaXMub3JpZW50YXRpb24gPT09ICdob3Jpem9udGFsJykge1xuICAgICAgdGhpcy54MiA9ICcxMDAlJztcbiAgICB9IGVsc2UgaWYgKHRoaXMub3JpZW50YXRpb24gPT09ICd2ZXJ0aWNhbCcpIHtcbiAgICAgIHRoaXMueTEgPSAnMTAwJSc7XG4gICAgfVxuICB9XG59XG4iXX0=