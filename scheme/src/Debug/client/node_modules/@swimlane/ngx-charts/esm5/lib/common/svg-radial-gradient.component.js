import { __decorate } from "tslib";
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
var SvgRadialGradientComponent = /** @class */ (function () {
    function SvgRadialGradientComponent() {
        this.endOpacity = 1;
        this.cx = 0;
        this.cy = 0;
    }
    Object.defineProperty(SvgRadialGradientComponent.prototype, "stops", {
        get: function () {
            return this.stopsInput || this.stopsDefault;
        },
        set: function (value) {
            this.stopsInput = value;
        },
        enumerable: true,
        configurable: true
    });
    SvgRadialGradientComponent.prototype.ngOnChanges = function (changes) {
        this.r = '30%';
        if ('color' in changes || 'startOpacity' in changes || 'endOpacity' in changes) {
            this.stopsDefault = [
                {
                    offset: 0,
                    color: this.color,
                    opacity: this.startOpacity
                },
                {
                    offset: 100,
                    color: this.color,
                    opacity: this.endOpacity
                }
            ];
        }
    };
    __decorate([
        Input()
    ], SvgRadialGradientComponent.prototype, "color", void 0);
    __decorate([
        Input()
    ], SvgRadialGradientComponent.prototype, "name", void 0);
    __decorate([
        Input()
    ], SvgRadialGradientComponent.prototype, "startOpacity", void 0);
    __decorate([
        Input()
    ], SvgRadialGradientComponent.prototype, "endOpacity", void 0);
    __decorate([
        Input()
    ], SvgRadialGradientComponent.prototype, "cx", void 0);
    __decorate([
        Input()
    ], SvgRadialGradientComponent.prototype, "cy", void 0);
    __decorate([
        Input()
    ], SvgRadialGradientComponent.prototype, "stops", null);
    SvgRadialGradientComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-svg-radial-gradient]',
            template: "\n    <svg:radialGradient [id]=\"name\" [attr.cx]=\"cx\" [attr.cy]=\"cy\" [attr.r]=\"r\" gradientUnits=\"userSpaceOnUse\">\n      <svg:stop\n        *ngFor=\"let stop of stops\"\n        [attr.offset]=\"stop.offset + '%'\"\n        [style.stop-color]=\"stop.color\"\n        [style.stop-opacity]=\"stop.opacity\"\n      />\n    </svg:radialGradient>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], SvgRadialGradientComponent);
    return SvgRadialGradientComponent;
}());
export { SvgRadialGradientComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic3ZnLXJhZGlhbC1ncmFkaWVudC5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vc3ZnLXJhZGlhbC1ncmFkaWVudC5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFBRSxTQUFTLEVBQUUsS0FBSyxFQUFhLHVCQUF1QixFQUFpQixNQUFNLGVBQWUsQ0FBQztBQWdCcEc7SUFBQTtRQUlXLGVBQVUsR0FBRyxDQUFDLENBQUM7UUFDZixPQUFFLEdBQVcsQ0FBQyxDQUFDO1FBQ2YsT0FBRSxHQUFXLENBQUMsQ0FBQztJQWlDMUIsQ0FBQztJQTlCQyxzQkFBSSw2Q0FBSzthQUFUO1lBQ0UsT0FBTyxJQUFJLENBQUMsVUFBVSxJQUFJLElBQUksQ0FBQyxZQUFZLENBQUM7UUFDOUMsQ0FBQzthQUVELFVBQVUsS0FBWTtZQUNwQixJQUFJLENBQUMsVUFBVSxHQUFHLEtBQUssQ0FBQztRQUMxQixDQUFDOzs7T0FKQTtJQVdELGdEQUFXLEdBQVgsVUFBWSxPQUFzQjtRQUNoQyxJQUFJLENBQUMsQ0FBQyxHQUFHLEtBQUssQ0FBQztRQUNmLElBQUksT0FBTyxJQUFJLE9BQU8sSUFBSSxjQUFjLElBQUksT0FBTyxJQUFJLFlBQVksSUFBSSxPQUFPLEVBQUU7WUFDOUUsSUFBSSxDQUFDLFlBQVksR0FBRztnQkFDbEI7b0JBQ0UsTUFBTSxFQUFFLENBQUM7b0JBQ1QsS0FBSyxFQUFFLElBQUksQ0FBQyxLQUFLO29CQUNqQixPQUFPLEVBQUUsSUFBSSxDQUFDLFlBQVk7aUJBQzNCO2dCQUNEO29CQUNFLE1BQU0sRUFBRSxHQUFHO29CQUNYLEtBQUssRUFBRSxJQUFJLENBQUMsS0FBSztvQkFDakIsT0FBTyxFQUFFLElBQUksQ0FBQyxVQUFVO2lCQUN6QjthQUNGLENBQUM7U0FDSDtJQUNILENBQUM7SUFyQ1E7UUFBUixLQUFLLEVBQUU7NkRBQWU7SUFDZDtRQUFSLEtBQUssRUFBRTs0REFBYztJQUNiO1FBQVIsS0FBSyxFQUFFO29FQUFzQjtJQUNyQjtRQUFSLEtBQUssRUFBRTtrRUFBZ0I7SUFDZjtRQUFSLEtBQUssRUFBRTswREFBZ0I7SUFDZjtRQUFSLEtBQUssRUFBRTswREFBZ0I7SUFHeEI7UUFEQyxLQUFLLEVBQUU7MkRBR1A7SUFYVSwwQkFBMEI7UUFkdEMsU0FBUyxDQUFDO1lBQ1QsUUFBUSxFQUFFLG1DQUFtQztZQUM3QyxRQUFRLEVBQUUsbVdBU1Q7WUFDRCxlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTtTQUNoRCxDQUFDO09BQ1csMEJBQTBCLENBdUN0QztJQUFELGlDQUFDO0NBQUEsQUF2Q0QsSUF1Q0M7U0F2Q1ksMEJBQTBCIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgQ29tcG9uZW50LCBJbnB1dCwgT25DaGFuZ2VzLCBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSwgU2ltcGxlQ2hhbmdlcyB9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdnW25neC1jaGFydHMtc3ZnLXJhZGlhbC1ncmFkaWVudF0nLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxzdmc6cmFkaWFsR3JhZGllbnQgW2lkXT1cIm5hbWVcIiBbYXR0ci5jeF09XCJjeFwiIFthdHRyLmN5XT1cImN5XCIgW2F0dHIucl09XCJyXCIgZ3JhZGllbnRVbml0cz1cInVzZXJTcGFjZU9uVXNlXCI+XG4gICAgICA8c3ZnOnN0b3BcbiAgICAgICAgKm5nRm9yPVwibGV0IHN0b3Agb2Ygc3RvcHNcIlxuICAgICAgICBbYXR0ci5vZmZzZXRdPVwic3RvcC5vZmZzZXQgKyAnJSdcIlxuICAgICAgICBbc3R5bGUuc3RvcC1jb2xvcl09XCJzdG9wLmNvbG9yXCJcbiAgICAgICAgW3N0eWxlLnN0b3Atb3BhY2l0eV09XCJzdG9wLm9wYWNpdHlcIlxuICAgICAgLz5cbiAgICA8L3N2ZzpyYWRpYWxHcmFkaWVudD5cbiAgYCxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2hcbn0pXG5leHBvcnQgY2xhc3MgU3ZnUmFkaWFsR3JhZGllbnRDb21wb25lbnQgaW1wbGVtZW50cyBPbkNoYW5nZXMge1xuICBASW5wdXQoKSBjb2xvcjogc3RyaW5nO1xuICBASW5wdXQoKSBuYW1lOiBzdHJpbmc7XG4gIEBJbnB1dCgpIHN0YXJ0T3BhY2l0eTogbnVtYmVyO1xuICBASW5wdXQoKSBlbmRPcGFjaXR5ID0gMTtcbiAgQElucHV0KCkgY3g6IG51bWJlciA9IDA7XG4gIEBJbnB1dCgpIGN5OiBudW1iZXIgPSAwO1xuXG4gIEBJbnB1dCgpXG4gIGdldCBzdG9wcygpOiBhbnlbXSB7XG4gICAgcmV0dXJuIHRoaXMuc3RvcHNJbnB1dCB8fCB0aGlzLnN0b3BzRGVmYXVsdDtcbiAgfVxuXG4gIHNldCBzdG9wcyh2YWx1ZTogYW55W10pIHtcbiAgICB0aGlzLnN0b3BzSW5wdXQgPSB2YWx1ZTtcbiAgfVxuXG4gIHI6IHN0cmluZztcblxuICBwcml2YXRlIHN0b3BzSW5wdXQ6IGFueVtdO1xuICBwcml2YXRlIHN0b3BzRGVmYXVsdDogYW55W107XG5cbiAgbmdPbkNoYW5nZXMoY2hhbmdlczogU2ltcGxlQ2hhbmdlcyk6IHZvaWQge1xuICAgIHRoaXMuciA9ICczMCUnO1xuICAgIGlmICgnY29sb3InIGluIGNoYW5nZXMgfHwgJ3N0YXJ0T3BhY2l0eScgaW4gY2hhbmdlcyB8fCAnZW5kT3BhY2l0eScgaW4gY2hhbmdlcykge1xuICAgICAgdGhpcy5zdG9wc0RlZmF1bHQgPSBbXG4gICAgICAgIHtcbiAgICAgICAgICBvZmZzZXQ6IDAsXG4gICAgICAgICAgY29sb3I6IHRoaXMuY29sb3IsXG4gICAgICAgICAgb3BhY2l0eTogdGhpcy5zdGFydE9wYWNpdHlcbiAgICAgICAgfSxcbiAgICAgICAge1xuICAgICAgICAgIG9mZnNldDogMTAwLFxuICAgICAgICAgIGNvbG9yOiB0aGlzLmNvbG9yLFxuICAgICAgICAgIG9wYWNpdHk6IHRoaXMuZW5kT3BhY2l0eVxuICAgICAgICB9XG4gICAgICBdO1xuICAgIH1cbiAgfVxufVxuIl19