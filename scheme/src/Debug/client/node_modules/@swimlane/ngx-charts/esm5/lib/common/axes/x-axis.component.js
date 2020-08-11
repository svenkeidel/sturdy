import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ViewChild, ChangeDetectionStrategy } from '@angular/core';
import { XAxisTicksComponent } from './x-axis-ticks.component';
var XAxisComponent = /** @class */ (function () {
    function XAxisComponent() {
        this.rotateTicks = true;
        this.showGridLines = false;
        this.xOrient = 'bottom';
        this.xAxisOffset = 0;
        this.dimensionsChanged = new EventEmitter();
        this.xAxisClassName = 'x axis';
        this.labelOffset = 0;
        this.fill = 'none';
        this.stroke = 'stroke';
        this.tickStroke = '#ccc';
        this.strokeWidth = 'none';
        this.padding = 5;
    }
    XAxisComponent.prototype.ngOnChanges = function (changes) {
        this.update();
    };
    XAxisComponent.prototype.update = function () {
        this.transform = "translate(0," + (this.xAxisOffset + this.padding + this.dims.height) + ")";
        if (typeof this.xAxisTickCount !== 'undefined') {
            this.tickArguments = [this.xAxisTickCount];
        }
    };
    XAxisComponent.prototype.emitTicksHeight = function (_a) {
        var _this = this;
        var height = _a.height;
        var newLabelOffset = height + 25 + 5;
        if (newLabelOffset !== this.labelOffset) {
            this.labelOffset = newLabelOffset;
            setTimeout(function () {
                _this.dimensionsChanged.emit({ height: height });
            }, 0);
        }
    };
    __decorate([
        Input()
    ], XAxisComponent.prototype, "xScale", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "dims", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "trimTicks", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "rotateTicks", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "maxTickLength", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "tickFormatting", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "showGridLines", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "showLabel", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "labelText", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "ticks", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "xAxisTickInterval", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "xAxisTickCount", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "xOrient", void 0);
    __decorate([
        Input()
    ], XAxisComponent.prototype, "xAxisOffset", void 0);
    __decorate([
        Output()
    ], XAxisComponent.prototype, "dimensionsChanged", void 0);
    __decorate([
        ViewChild(XAxisTicksComponent)
    ], XAxisComponent.prototype, "ticksComponent", void 0);
    XAxisComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-x-axis]',
            template: "\n    <svg:g [attr.class]=\"xAxisClassName\" [attr.transform]=\"transform\">\n      <svg:g\n        ngx-charts-x-axis-ticks\n        *ngIf=\"xScale\"\n        [trimTicks]=\"trimTicks\"\n        [rotateTicks]=\"rotateTicks\"\n        [maxTickLength]=\"maxTickLength\"\n        [tickFormatting]=\"tickFormatting\"\n        [tickArguments]=\"tickArguments\"\n        [tickStroke]=\"tickStroke\"\n        [scale]=\"xScale\"\n        [orient]=\"xOrient\"\n        [showGridLines]=\"showGridLines\"\n        [gridLineHeight]=\"dims.height\"\n        [width]=\"dims.width\"\n        [tickValues]=\"ticks\"\n        (dimensionsChanged)=\"emitTicksHeight($event)\"\n      />\n      <svg:g\n        ngx-charts-axis-label\n        *ngIf=\"showLabel\"\n        [label]=\"labelText\"\n        [offset]=\"labelOffset\"\n        [orient]=\"'bottom'\"\n        [height]=\"dims.height\"\n        [width]=\"dims.width\"\n      ></svg:g>\n    </svg:g>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], XAxisComponent);
    return XAxisComponent;
}());
export { XAxisComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoieC1heGlzLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi9heGVzL3gtYXhpcy5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFDTCxTQUFTLEVBQ1QsS0FBSyxFQUVMLE1BQU0sRUFDTixZQUFZLEVBRVosU0FBUyxFQUNULHVCQUF1QixFQUN4QixNQUFNLGVBQWUsQ0FBQztBQUV2QixPQUFPLEVBQUUsbUJBQW1CLEVBQUUsTUFBTSwwQkFBMEIsQ0FBQztBQW9DL0Q7SUFBQTtRQUlXLGdCQUFXLEdBQVksSUFBSSxDQUFDO1FBRzVCLGtCQUFhLEdBQUcsS0FBSyxDQUFDO1FBTXRCLFlBQU8sR0FBVyxRQUFRLENBQUM7UUFDM0IsZ0JBQVcsR0FBVyxDQUFDLENBQUM7UUFFdkIsc0JBQWlCLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUVqRCxtQkFBYyxHQUFXLFFBQVEsQ0FBQztRQUlsQyxnQkFBVyxHQUFXLENBQUMsQ0FBQztRQUN4QixTQUFJLEdBQVcsTUFBTSxDQUFDO1FBQ3RCLFdBQU0sR0FBVyxRQUFRLENBQUM7UUFDMUIsZUFBVSxHQUFXLE1BQU0sQ0FBQztRQUM1QixnQkFBVyxHQUFXLE1BQU0sQ0FBQztRQUM3QixZQUFPLEdBQVcsQ0FBQyxDQUFDO0lBeUJ0QixDQUFDO0lBckJDLG9DQUFXLEdBQVgsVUFBWSxPQUFzQjtRQUNoQyxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUM7SUFDaEIsQ0FBQztJQUVELCtCQUFNLEdBQU47UUFDRSxJQUFJLENBQUMsU0FBUyxHQUFHLGtCQUFlLElBQUksQ0FBQyxXQUFXLEdBQUcsSUFBSSxDQUFDLE9BQU8sR0FBRyxJQUFJLENBQUMsSUFBSSxDQUFDLE1BQU0sT0FBRyxDQUFDO1FBRXRGLElBQUksT0FBTyxJQUFJLENBQUMsY0FBYyxLQUFLLFdBQVcsRUFBRTtZQUM5QyxJQUFJLENBQUMsYUFBYSxHQUFHLENBQUMsSUFBSSxDQUFDLGNBQWMsQ0FBQyxDQUFDO1NBQzVDO0lBQ0gsQ0FBQztJQUVELHdDQUFlLEdBQWYsVUFBZ0IsRUFBVTtRQUExQixpQkFRQztZQVJpQixrQkFBTTtRQUN0QixJQUFNLGNBQWMsR0FBRyxNQUFNLEdBQUcsRUFBRSxHQUFHLENBQUMsQ0FBQztRQUN2QyxJQUFJLGNBQWMsS0FBSyxJQUFJLENBQUMsV0FBVyxFQUFFO1lBQ3ZDLElBQUksQ0FBQyxXQUFXLEdBQUcsY0FBYyxDQUFDO1lBQ2xDLFVBQVUsQ0FBQztnQkFDVCxLQUFJLENBQUMsaUJBQWlCLENBQUMsSUFBSSxDQUFDLEVBQUUsTUFBTSxRQUFBLEVBQUUsQ0FBQyxDQUFDO1lBQzFDLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQztTQUNQO0lBQ0gsQ0FBQztJQWxEUTtRQUFSLEtBQUssRUFBRTtrREFBUTtJQUNQO1FBQVIsS0FBSyxFQUFFO2dEQUFNO0lBQ0w7UUFBUixLQUFLLEVBQUU7cURBQW9CO0lBQ25CO1FBQVIsS0FBSyxFQUFFO3VEQUE2QjtJQUM1QjtRQUFSLEtBQUssRUFBRTt5REFBdUI7SUFDdEI7UUFBUixLQUFLLEVBQUU7MERBQWdCO0lBQ2Y7UUFBUixLQUFLLEVBQUU7eURBQXVCO0lBQ3RCO1FBQVIsS0FBSyxFQUFFO3FEQUFXO0lBQ1Y7UUFBUixLQUFLLEVBQUU7cURBQVc7SUFDVjtRQUFSLEtBQUssRUFBRTtpREFBYztJQUNiO1FBQVIsS0FBSyxFQUFFOzZEQUFtQjtJQUNsQjtRQUFSLEtBQUssRUFBRTswREFBcUI7SUFDcEI7UUFBUixLQUFLLEVBQUU7bURBQTRCO0lBQzNCO1FBQVIsS0FBSyxFQUFFO3VEQUF5QjtJQUV2QjtRQUFULE1BQU0sRUFBRTs2REFBd0M7SUFhakI7UUFBL0IsU0FBUyxDQUFDLG1CQUFtQixDQUFDOzBEQUFxQztJQTdCekQsY0FBYztRQWxDMUIsU0FBUyxDQUFDO1lBQ1QsUUFBUSxFQUFFLHNCQUFzQjtZQUNoQyxRQUFRLEVBQUUsMDZCQTZCVDtZQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO1NBQ2hELENBQUM7T0FDVyxjQUFjLENBb0QxQjtJQUFELHFCQUFDO0NBQUEsQUFwREQsSUFvREM7U0FwRFksY0FBYyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIENvbXBvbmVudCxcbiAgSW5wdXQsXG4gIFNpbXBsZUNoYW5nZXMsXG4gIE91dHB1dCxcbiAgRXZlbnRFbWl0dGVyLFxuICBPbkNoYW5nZXMsXG4gIFZpZXdDaGlsZCxcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3lcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5cbmltcG9ydCB7IFhBeGlzVGlja3NDb21wb25lbnQgfSBmcm9tICcuL3gtYXhpcy10aWNrcy5jb21wb25lbnQnO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdnW25neC1jaGFydHMteC1heGlzXScsXG4gIHRlbXBsYXRlOiBgXG4gICAgPHN2ZzpnIFthdHRyLmNsYXNzXT1cInhBeGlzQ2xhc3NOYW1lXCIgW2F0dHIudHJhbnNmb3JtXT1cInRyYW5zZm9ybVwiPlxuICAgICAgPHN2ZzpnXG4gICAgICAgIG5neC1jaGFydHMteC1heGlzLXRpY2tzXG4gICAgICAgICpuZ0lmPVwieFNjYWxlXCJcbiAgICAgICAgW3RyaW1UaWNrc109XCJ0cmltVGlja3NcIlxuICAgICAgICBbcm90YXRlVGlja3NdPVwicm90YXRlVGlja3NcIlxuICAgICAgICBbbWF4VGlja0xlbmd0aF09XCJtYXhUaWNrTGVuZ3RoXCJcbiAgICAgICAgW3RpY2tGb3JtYXR0aW5nXT1cInRpY2tGb3JtYXR0aW5nXCJcbiAgICAgICAgW3RpY2tBcmd1bWVudHNdPVwidGlja0FyZ3VtZW50c1wiXG4gICAgICAgIFt0aWNrU3Ryb2tlXT1cInRpY2tTdHJva2VcIlxuICAgICAgICBbc2NhbGVdPVwieFNjYWxlXCJcbiAgICAgICAgW29yaWVudF09XCJ4T3JpZW50XCJcbiAgICAgICAgW3Nob3dHcmlkTGluZXNdPVwic2hvd0dyaWRMaW5lc1wiXG4gICAgICAgIFtncmlkTGluZUhlaWdodF09XCJkaW1zLmhlaWdodFwiXG4gICAgICAgIFt3aWR0aF09XCJkaW1zLndpZHRoXCJcbiAgICAgICAgW3RpY2tWYWx1ZXNdPVwidGlja3NcIlxuICAgICAgICAoZGltZW5zaW9uc0NoYW5nZWQpPVwiZW1pdFRpY2tzSGVpZ2h0KCRldmVudClcIlxuICAgICAgLz5cbiAgICAgIDxzdmc6Z1xuICAgICAgICBuZ3gtY2hhcnRzLWF4aXMtbGFiZWxcbiAgICAgICAgKm5nSWY9XCJzaG93TGFiZWxcIlxuICAgICAgICBbbGFiZWxdPVwibGFiZWxUZXh0XCJcbiAgICAgICAgW29mZnNldF09XCJsYWJlbE9mZnNldFwiXG4gICAgICAgIFtvcmllbnRdPVwiJ2JvdHRvbSdcIlxuICAgICAgICBbaGVpZ2h0XT1cImRpbXMuaGVpZ2h0XCJcbiAgICAgICAgW3dpZHRoXT1cImRpbXMud2lkdGhcIlxuICAgICAgPjwvc3ZnOmc+XG4gICAgPC9zdmc6Zz5cbiAgYCxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2hcbn0pXG5leHBvcnQgY2xhc3MgWEF4aXNDb21wb25lbnQgaW1wbGVtZW50cyBPbkNoYW5nZXMge1xuICBASW5wdXQoKSB4U2NhbGU7XG4gIEBJbnB1dCgpIGRpbXM7XG4gIEBJbnB1dCgpIHRyaW1UaWNrczogYm9vbGVhbjtcbiAgQElucHV0KCkgcm90YXRlVGlja3M6IGJvb2xlYW4gPSB0cnVlO1xuICBASW5wdXQoKSBtYXhUaWNrTGVuZ3RoOiBudW1iZXI7XG4gIEBJbnB1dCgpIHRpY2tGb3JtYXR0aW5nO1xuICBASW5wdXQoKSBzaG93R3JpZExpbmVzID0gZmFsc2U7XG4gIEBJbnB1dCgpIHNob3dMYWJlbDtcbiAgQElucHV0KCkgbGFiZWxUZXh0O1xuICBASW5wdXQoKSB0aWNrczogYW55W107XG4gIEBJbnB1dCgpIHhBeGlzVGlja0ludGVydmFsO1xuICBASW5wdXQoKSB4QXhpc1RpY2tDb3VudDogYW55O1xuICBASW5wdXQoKSB4T3JpZW50OiBzdHJpbmcgPSAnYm90dG9tJztcbiAgQElucHV0KCkgeEF4aXNPZmZzZXQ6IG51bWJlciA9IDA7XG5cbiAgQE91dHB1dCgpIGRpbWVuc2lvbnNDaGFuZ2VkID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuXG4gIHhBeGlzQ2xhc3NOYW1lOiBzdHJpbmcgPSAneCBheGlzJztcblxuICB0aWNrQXJndW1lbnRzOiBhbnk7XG4gIHRyYW5zZm9ybTogYW55O1xuICBsYWJlbE9mZnNldDogbnVtYmVyID0gMDtcbiAgZmlsbDogc3RyaW5nID0gJ25vbmUnO1xuICBzdHJva2U6IHN0cmluZyA9ICdzdHJva2UnO1xuICB0aWNrU3Ryb2tlOiBzdHJpbmcgPSAnI2NjYyc7XG4gIHN0cm9rZVdpZHRoOiBzdHJpbmcgPSAnbm9uZSc7XG4gIHBhZGRpbmc6IG51bWJlciA9IDU7XG5cbiAgQFZpZXdDaGlsZChYQXhpc1RpY2tzQ29tcG9uZW50KSB0aWNrc0NvbXBvbmVudDogWEF4aXNUaWNrc0NvbXBvbmVudDtcblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKTogdm9pZCB7XG4gICAgdGhpcy51cGRhdGUoKTtcbiAgfVxuXG4gIHVwZGF0ZSgpOiB2b2lkIHtcbiAgICB0aGlzLnRyYW5zZm9ybSA9IGB0cmFuc2xhdGUoMCwke3RoaXMueEF4aXNPZmZzZXQgKyB0aGlzLnBhZGRpbmcgKyB0aGlzLmRpbXMuaGVpZ2h0fSlgO1xuXG4gICAgaWYgKHR5cGVvZiB0aGlzLnhBeGlzVGlja0NvdW50ICE9PSAndW5kZWZpbmVkJykge1xuICAgICAgdGhpcy50aWNrQXJndW1lbnRzID0gW3RoaXMueEF4aXNUaWNrQ291bnRdO1xuICAgIH1cbiAgfVxuXG4gIGVtaXRUaWNrc0hlaWdodCh7IGhlaWdodCB9KTogdm9pZCB7XG4gICAgY29uc3QgbmV3TGFiZWxPZmZzZXQgPSBoZWlnaHQgKyAyNSArIDU7XG4gICAgaWYgKG5ld0xhYmVsT2Zmc2V0ICE9PSB0aGlzLmxhYmVsT2Zmc2V0KSB7XG4gICAgICB0aGlzLmxhYmVsT2Zmc2V0ID0gbmV3TGFiZWxPZmZzZXQ7XG4gICAgICBzZXRUaW1lb3V0KCgpID0+IHtcbiAgICAgICAgdGhpcy5kaW1lbnNpb25zQ2hhbmdlZC5lbWl0KHsgaGVpZ2h0IH0pO1xuICAgICAgfSwgMCk7XG4gICAgfVxuICB9XG59XG4iXX0=