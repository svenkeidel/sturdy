import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy } from '@angular/core';
import { formatLabel, escapeLabel } from '../common/label.helper';
var GaugeArcComponent = /** @class */ (function () {
    function GaugeArcComponent() {
        this.isActive = false;
        this.tooltipDisabled = false;
        this.animations = true;
        this.select = new EventEmitter();
        this.activate = new EventEmitter();
        this.deactivate = new EventEmitter();
    }
    GaugeArcComponent.prototype.tooltipText = function (arc) {
        var label = formatLabel(arc.data.name);
        var val;
        if (this.valueFormatting) {
            val = this.valueFormatting(arc.data.value);
        }
        else {
            val = formatLabel(arc.data.value);
        }
        return "\n      <span class=\"tooltip-label\">" + escapeLabel(label) + "</span>\n      <span class=\"tooltip-val\">" + val + "</span>\n    ";
    };
    __decorate([
        Input()
    ], GaugeArcComponent.prototype, "backgroundArc", void 0);
    __decorate([
        Input()
    ], GaugeArcComponent.prototype, "valueArc", void 0);
    __decorate([
        Input()
    ], GaugeArcComponent.prototype, "cornerRadius", void 0);
    __decorate([
        Input()
    ], GaugeArcComponent.prototype, "colors", void 0);
    __decorate([
        Input()
    ], GaugeArcComponent.prototype, "isActive", void 0);
    __decorate([
        Input()
    ], GaugeArcComponent.prototype, "tooltipDisabled", void 0);
    __decorate([
        Input()
    ], GaugeArcComponent.prototype, "valueFormatting", void 0);
    __decorate([
        Input()
    ], GaugeArcComponent.prototype, "tooltipTemplate", void 0);
    __decorate([
        Input()
    ], GaugeArcComponent.prototype, "animations", void 0);
    __decorate([
        Output()
    ], GaugeArcComponent.prototype, "select", void 0);
    __decorate([
        Output()
    ], GaugeArcComponent.prototype, "activate", void 0);
    __decorate([
        Output()
    ], GaugeArcComponent.prototype, "deactivate", void 0);
    GaugeArcComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-gauge-arc]',
            template: "\n    <svg:g\n      ngx-charts-pie-arc\n      class=\"background-arc\"\n      [startAngle]=\"0\"\n      [endAngle]=\"backgroundArc.endAngle\"\n      [innerRadius]=\"backgroundArc.innerRadius\"\n      [outerRadius]=\"backgroundArc.outerRadius\"\n      [cornerRadius]=\"cornerRadius\"\n      [data]=\"backgroundArc.data\"\n      [animate]=\"false\"\n      [pointerEvents]=\"false\"\n    ></svg:g>\n    <svg:g\n      ngx-charts-pie-arc\n      [startAngle]=\"0\"\n      [endAngle]=\"valueArc.endAngle\"\n      [innerRadius]=\"valueArc.innerRadius\"\n      [outerRadius]=\"valueArc.outerRadius\"\n      [cornerRadius]=\"cornerRadius\"\n      [fill]=\"colors.getColor(valueArc.data.name)\"\n      [data]=\"valueArc.data\"\n      [animate]=\"animations\"\n      [isActive]=\"isActive\"\n      (select)=\"select.emit($event)\"\n      (activate)=\"activate.emit($event)\"\n      (deactivate)=\"deactivate.emit($event)\"\n      ngx-tooltip\n      [tooltipDisabled]=\"tooltipDisabled\"\n      [tooltipPlacement]=\"'top'\"\n      [tooltipType]=\"'tooltip'\"\n      [tooltipTitle]=\"tooltipTemplate ? undefined : tooltipText(valueArc)\"\n      [tooltipTemplate]=\"tooltipTemplate\"\n      [tooltipContext]=\"valueArc.data\"\n    ></svg:g>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], GaugeArcComponent);
    return GaugeArcComponent;
}());
export { GaugeArcComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZ2F1Z2UtYXJjLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2dhdWdlL2dhdWdlLWFyYy5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFBRSxTQUFTLEVBQUUsS0FBSyxFQUFFLE1BQU0sRUFBRSxZQUFZLEVBQUUsdUJBQXVCLEVBQWUsTUFBTSxlQUFlLENBQUM7QUFDN0csT0FBTyxFQUFFLFdBQVcsRUFBRSxXQUFXLEVBQUUsTUFBTSx3QkFBd0IsQ0FBQztBQTJDbEU7SUFBQTtRQUtXLGFBQVEsR0FBWSxLQUFLLENBQUM7UUFDMUIsb0JBQWUsR0FBWSxLQUFLLENBQUM7UUFHakMsZUFBVSxHQUFZLElBQUksQ0FBQztRQUUxQixXQUFNLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUM1QixhQUFRLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUM5QixlQUFVLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztJQWlCNUMsQ0FBQztJQWZDLHVDQUFXLEdBQVgsVUFBWSxHQUFHO1FBQ2IsSUFBTSxLQUFLLEdBQUcsV0FBVyxDQUFDLEdBQUcsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUM7UUFDekMsSUFBSSxHQUFHLENBQUM7UUFFUixJQUFJLElBQUksQ0FBQyxlQUFlLEVBQUU7WUFDeEIsR0FBRyxHQUFHLElBQUksQ0FBQyxlQUFlLENBQUMsR0FBRyxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsQ0FBQztTQUM1QzthQUFNO1lBQ0wsR0FBRyxHQUFHLFdBQVcsQ0FBQyxHQUFHLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxDQUFDO1NBQ25DO1FBRUQsT0FBTywyQ0FDeUIsV0FBVyxDQUFDLEtBQUssQ0FBQyxtREFDcEIsR0FBRyxrQkFDaEMsQ0FBQztJQUNKLENBQUM7SUE1QlE7UUFBUixLQUFLLEVBQUU7NERBQW9CO0lBQ25CO1FBQVIsS0FBSyxFQUFFO3VEQUFlO0lBQ2Q7UUFBUixLQUFLLEVBQUU7MkRBQW1CO0lBQ2xCO1FBQVIsS0FBSyxFQUFFO3FEQUFxQjtJQUNwQjtRQUFSLEtBQUssRUFBRTt1REFBMkI7SUFDMUI7UUFBUixLQUFLLEVBQUU7OERBQWtDO0lBQ2pDO1FBQVIsS0FBSyxFQUFFOzhEQUF5QztJQUN4QztRQUFSLEtBQUssRUFBRTs4REFBbUM7SUFDbEM7UUFBUixLQUFLLEVBQUU7eURBQTRCO0lBRTFCO1FBQVQsTUFBTSxFQUFFO3FEQUE2QjtJQUM1QjtRQUFULE1BQU0sRUFBRTt1REFBK0I7SUFDOUI7UUFBVCxNQUFNLEVBQUU7eURBQWlDO0lBYi9CLGlCQUFpQjtRQXhDN0IsU0FBUyxDQUFDO1lBQ1QsUUFBUSxFQUFFLHlCQUF5QjtZQUNuQyxRQUFRLEVBQUUsK3NDQW1DVDtZQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO1NBQ2hELENBQUM7T0FDVyxpQkFBaUIsQ0E4QjdCO0lBQUQsd0JBQUM7Q0FBQSxBQTlCRCxJQThCQztTQTlCWSxpQkFBaUIiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgeyBDb21wb25lbnQsIElucHV0LCBPdXRwdXQsIEV2ZW50RW1pdHRlciwgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksIFRlbXBsYXRlUmVmIH0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBmb3JtYXRMYWJlbCwgZXNjYXBlTGFiZWwgfSBmcm9tICcuLi9jb21tb24vbGFiZWwuaGVscGVyJztcbmltcG9ydCB7IENvbG9ySGVscGVyIH0gZnJvbSAnLi4vY29tbW9uL2NvbG9yLmhlbHBlcic7XG5cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ2dbbmd4LWNoYXJ0cy1nYXVnZS1hcmNdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOmdcbiAgICAgIG5neC1jaGFydHMtcGllLWFyY1xuICAgICAgY2xhc3M9XCJiYWNrZ3JvdW5kLWFyY1wiXG4gICAgICBbc3RhcnRBbmdsZV09XCIwXCJcbiAgICAgIFtlbmRBbmdsZV09XCJiYWNrZ3JvdW5kQXJjLmVuZEFuZ2xlXCJcbiAgICAgIFtpbm5lclJhZGl1c109XCJiYWNrZ3JvdW5kQXJjLmlubmVyUmFkaXVzXCJcbiAgICAgIFtvdXRlclJhZGl1c109XCJiYWNrZ3JvdW5kQXJjLm91dGVyUmFkaXVzXCJcbiAgICAgIFtjb3JuZXJSYWRpdXNdPVwiY29ybmVyUmFkaXVzXCJcbiAgICAgIFtkYXRhXT1cImJhY2tncm91bmRBcmMuZGF0YVwiXG4gICAgICBbYW5pbWF0ZV09XCJmYWxzZVwiXG4gICAgICBbcG9pbnRlckV2ZW50c109XCJmYWxzZVwiXG4gICAgPjwvc3ZnOmc+XG4gICAgPHN2ZzpnXG4gICAgICBuZ3gtY2hhcnRzLXBpZS1hcmNcbiAgICAgIFtzdGFydEFuZ2xlXT1cIjBcIlxuICAgICAgW2VuZEFuZ2xlXT1cInZhbHVlQXJjLmVuZEFuZ2xlXCJcbiAgICAgIFtpbm5lclJhZGl1c109XCJ2YWx1ZUFyYy5pbm5lclJhZGl1c1wiXG4gICAgICBbb3V0ZXJSYWRpdXNdPVwidmFsdWVBcmMub3V0ZXJSYWRpdXNcIlxuICAgICAgW2Nvcm5lclJhZGl1c109XCJjb3JuZXJSYWRpdXNcIlxuICAgICAgW2ZpbGxdPVwiY29sb3JzLmdldENvbG9yKHZhbHVlQXJjLmRhdGEubmFtZSlcIlxuICAgICAgW2RhdGFdPVwidmFsdWVBcmMuZGF0YVwiXG4gICAgICBbYW5pbWF0ZV09XCJhbmltYXRpb25zXCJcbiAgICAgIFtpc0FjdGl2ZV09XCJpc0FjdGl2ZVwiXG4gICAgICAoc2VsZWN0KT1cInNlbGVjdC5lbWl0KCRldmVudClcIlxuICAgICAgKGFjdGl2YXRlKT1cImFjdGl2YXRlLmVtaXQoJGV2ZW50KVwiXG4gICAgICAoZGVhY3RpdmF0ZSk9XCJkZWFjdGl2YXRlLmVtaXQoJGV2ZW50KVwiXG4gICAgICBuZ3gtdG9vbHRpcFxuICAgICAgW3Rvb2x0aXBEaXNhYmxlZF09XCJ0b29sdGlwRGlzYWJsZWRcIlxuICAgICAgW3Rvb2x0aXBQbGFjZW1lbnRdPVwiJ3RvcCdcIlxuICAgICAgW3Rvb2x0aXBUeXBlXT1cIid0b29sdGlwJ1wiXG4gICAgICBbdG9vbHRpcFRpdGxlXT1cInRvb2x0aXBUZW1wbGF0ZSA/IHVuZGVmaW5lZCA6IHRvb2x0aXBUZXh0KHZhbHVlQXJjKVwiXG4gICAgICBbdG9vbHRpcFRlbXBsYXRlXT1cInRvb2x0aXBUZW1wbGF0ZVwiXG4gICAgICBbdG9vbHRpcENvbnRleHRdPVwidmFsdWVBcmMuZGF0YVwiXG4gICAgPjwvc3ZnOmc+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIEdhdWdlQXJjQ29tcG9uZW50IHtcbiAgQElucHV0KCkgYmFja2dyb3VuZEFyYzogYW55O1xuICBASW5wdXQoKSB2YWx1ZUFyYzogYW55O1xuICBASW5wdXQoKSBjb3JuZXJSYWRpdXM6IGFueTtcbiAgQElucHV0KCkgY29sb3JzOiBDb2xvckhlbHBlcjtcbiAgQElucHV0KCkgaXNBY3RpdmU6IGJvb2xlYW4gPSBmYWxzZTtcbiAgQElucHV0KCkgdG9vbHRpcERpc2FibGVkOiBib29sZWFuID0gZmFsc2U7XG4gIEBJbnB1dCgpIHZhbHVlRm9ybWF0dGluZzogKHZhbHVlOiBhbnkpID0+IHN0cmluZztcbiAgQElucHV0KCkgdG9vbHRpcFRlbXBsYXRlOiBUZW1wbGF0ZVJlZjxhbnk+O1xuICBASW5wdXQoKSBhbmltYXRpb25zOiBib29sZWFuID0gdHJ1ZTtcblxuICBAT3V0cHV0KCkgc2VsZWN0ID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuICBAT3V0cHV0KCkgYWN0aXZhdGUgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBkZWFjdGl2YXRlID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuXG4gIHRvb2x0aXBUZXh0KGFyYyk6IHN0cmluZyB7XG4gICAgY29uc3QgbGFiZWwgPSBmb3JtYXRMYWJlbChhcmMuZGF0YS5uYW1lKTtcbiAgICBsZXQgdmFsO1xuXG4gICAgaWYgKHRoaXMudmFsdWVGb3JtYXR0aW5nKSB7XG4gICAgICB2YWwgPSB0aGlzLnZhbHVlRm9ybWF0dGluZyhhcmMuZGF0YS52YWx1ZSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHZhbCA9IGZvcm1hdExhYmVsKGFyYy5kYXRhLnZhbHVlKTtcbiAgICB9XG5cbiAgICByZXR1cm4gYFxuICAgICAgPHNwYW4gY2xhc3M9XCJ0b29sdGlwLWxhYmVsXCI+JHtlc2NhcGVMYWJlbChsYWJlbCl9PC9zcGFuPlxuICAgICAgPHNwYW4gY2xhc3M9XCJ0b29sdGlwLXZhbFwiPiR7dmFsfTwvc3Bhbj5cbiAgICBgO1xuICB9XG59XG4iXX0=