import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy } from '@angular/core';
import { trigger, style, animate, transition } from '@angular/animations';
import { formatLabel, escapeLabel } from '../common/label.helper';
var BubbleSeriesComponent = /** @class */ (function () {
    function BubbleSeriesComponent() {
        this.tooltipDisabled = false;
        this.select = new EventEmitter();
        this.activate = new EventEmitter();
        this.deactivate = new EventEmitter();
    }
    BubbleSeriesComponent.prototype.ngOnChanges = function (changes) {
        this.update();
    };
    BubbleSeriesComponent.prototype.update = function () {
        this.circles = this.getCircles();
    };
    BubbleSeriesComponent.prototype.getCircles = function () {
        var _this = this;
        var seriesName = this.data.name;
        return this.data.series
            .map(function (d, i) {
            if (typeof d.y !== 'undefined' && typeof d.x !== 'undefined') {
                var y = d.y;
                var x = d.x;
                var r = d.r;
                var radius = _this.rScale(r || 1);
                var tooltipLabel = formatLabel(d.name);
                var cx = _this.xScaleType === 'linear' ? _this.xScale(Number(x)) : _this.xScale(x);
                var cy = _this.yScaleType === 'linear' ? _this.yScale(Number(y)) : _this.yScale(y);
                var color = _this.colors.scaleType === 'linear' ? _this.colors.getColor(r) : _this.colors.getColor(seriesName);
                var isActive = !_this.activeEntries.length ? true : _this.isActive({ name: seriesName });
                var opacity = isActive ? 1 : 0.3;
                var data = Object.assign({}, d, {
                    series: seriesName,
                    name: d.name,
                    value: d.y,
                    x: d.x,
                    radius: d.r
                });
                return {
                    data: data,
                    x: x,
                    y: y,
                    r: r,
                    classNames: ["circle-data-" + i],
                    value: y,
                    label: x,
                    cx: cx,
                    cy: cy,
                    radius: radius,
                    tooltipLabel: tooltipLabel,
                    color: color,
                    opacity: opacity,
                    seriesName: seriesName,
                    isActive: isActive,
                    transform: "translate(" + cx + "," + cy + ")"
                };
            }
        })
            .filter(function (circle) { return circle !== undefined; });
    };
    BubbleSeriesComponent.prototype.getTooltipText = function (circle) {
        var hasRadius = typeof circle.r !== 'undefined';
        var hasTooltipLabel = circle.tooltipLabel && circle.tooltipLabel.length;
        var hasSeriesName = circle.seriesName && circle.seriesName.length;
        var radiusValue = hasRadius ? formatLabel(circle.r) : '';
        var xAxisLabel = this.xAxisLabel && this.xAxisLabel !== '' ? this.xAxisLabel + ":" : '';
        var yAxisLabel = this.yAxisLabel && this.yAxisLabel !== '' ? this.yAxisLabel + ":" : '';
        var x = formatLabel(circle.x);
        var y = formatLabel(circle.y);
        var name = hasSeriesName && hasTooltipLabel
            ? circle.seriesName + " \u2022 " + circle.tooltipLabel
            : circle.seriesName + circle.tooltipLabel;
        var tooltipTitle = hasSeriesName || hasTooltipLabel ? "<span class=\"tooltip-label\">" + escapeLabel(name) + "</span>" : '';
        return "\n      " + tooltipTitle + "\n      <span class=\"tooltip-label\">\n        <label>" + escapeLabel(xAxisLabel) + "</label> " + escapeLabel(x) + "<br />\n        <label>" + escapeLabel(yAxisLabel) + "</label> " + escapeLabel(y) + "\n      </span>\n      <span class=\"tooltip-val\">\n        " + escapeLabel(radiusValue) + "\n      </span>\n    ";
    };
    BubbleSeriesComponent.prototype.onClick = function (data) {
        this.select.emit(data);
    };
    BubbleSeriesComponent.prototype.isActive = function (entry) {
        if (!this.activeEntries)
            return false;
        var item = this.activeEntries.find(function (d) {
            return entry.name === d.name;
        });
        return item !== undefined;
    };
    BubbleSeriesComponent.prototype.isVisible = function (circle) {
        if (this.activeEntries.length > 0) {
            return this.isActive({ name: circle.seriesName });
        }
        return circle.opacity !== 0;
    };
    BubbleSeriesComponent.prototype.activateCircle = function (circle) {
        circle.barVisible = true;
        this.activate.emit({ name: this.data.name });
    };
    BubbleSeriesComponent.prototype.deactivateCircle = function (circle) {
        circle.barVisible = false;
        this.deactivate.emit({ name: this.data.name });
    };
    BubbleSeriesComponent.prototype.trackBy = function (index, circle) {
        return circle.data.series + " " + circle.data.name;
    };
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "data", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "xScale", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "yScale", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "rScale", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "xScaleType", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "yScaleType", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "colors", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "visibleValue", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "activeEntries", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "xAxisLabel", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "yAxisLabel", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "tooltipDisabled", void 0);
    __decorate([
        Input()
    ], BubbleSeriesComponent.prototype, "tooltipTemplate", void 0);
    __decorate([
        Output()
    ], BubbleSeriesComponent.prototype, "select", void 0);
    __decorate([
        Output()
    ], BubbleSeriesComponent.prototype, "activate", void 0);
    __decorate([
        Output()
    ], BubbleSeriesComponent.prototype, "deactivate", void 0);
    BubbleSeriesComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-bubble-series]',
            template: "\n    <svg:g *ngFor=\"let circle of circles; trackBy: trackBy\">\n      <svg:g [attr.transform]=\"circle.transform\">\n        <svg:g\n          ngx-charts-circle\n          [@animationState]=\"'active'\"\n          class=\"circle\"\n          [cx]=\"0\"\n          [cy]=\"0\"\n          [r]=\"circle.radius\"\n          [fill]=\"circle.color\"\n          [style.opacity]=\"circle.opacity\"\n          [class.active]=\"circle.isActive\"\n          [pointerEvents]=\"'all'\"\n          [data]=\"circle.value\"\n          [classNames]=\"circle.classNames\"\n          (select)=\"onClick(circle.data)\"\n          (activate)=\"activateCircle(circle)\"\n          (deactivate)=\"deactivateCircle(circle)\"\n          ngx-tooltip\n          [tooltipDisabled]=\"tooltipDisabled\"\n          [tooltipPlacement]=\"'top'\"\n          [tooltipType]=\"'tooltip'\"\n          [tooltipTitle]=\"tooltipTemplate ? undefined : getTooltipText(circle)\"\n          [tooltipTemplate]=\"tooltipTemplate\"\n          [tooltipContext]=\"circle.data\"\n        />\n      </svg:g>\n    </svg:g>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush,
            animations: [
                trigger('animationState', [
                    transition(':enter', [
                        style({
                            opacity: 0,
                            transform: 'scale(0)'
                        }),
                        animate(250, style({ opacity: 1, transform: 'scale(1)' }))
                    ])
                ])
            ]
        })
    ], BubbleSeriesComponent);
    return BubbleSeriesComponent;
}());
export { BubbleSeriesComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYnViYmxlLXNlcmllcy5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9idWJibGUtY2hhcnQvYnViYmxlLXNlcmllcy5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFDTCxTQUFTLEVBQ1QsS0FBSyxFQUNMLE1BQU0sRUFFTixZQUFZLEVBRVosdUJBQXVCLEVBRXhCLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBRSxPQUFPLEVBQUUsS0FBSyxFQUFFLE9BQU8sRUFBRSxVQUFVLEVBQUUsTUFBTSxxQkFBcUIsQ0FBQztBQUMxRSxPQUFPLEVBQUUsV0FBVyxFQUFFLFdBQVcsRUFBRSxNQUFNLHdCQUF3QixDQUFDO0FBK0NsRTtJQUFBO1FBWVcsb0JBQWUsR0FBWSxLQUFLLENBQUM7UUFHaEMsV0FBTSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFDNUIsYUFBUSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFDOUIsZUFBVSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7SUErSDVDLENBQUM7SUExSEMsMkNBQVcsR0FBWCxVQUFZLE9BQXNCO1FBQ2hDLElBQUksQ0FBQyxNQUFNLEVBQUUsQ0FBQztJQUNoQixDQUFDO0lBRUQsc0NBQU0sR0FBTjtRQUNFLElBQUksQ0FBQyxPQUFPLEdBQUcsSUFBSSxDQUFDLFVBQVUsRUFBRSxDQUFDO0lBQ25DLENBQUM7SUFFRCwwQ0FBVSxHQUFWO1FBQUEsaUJBa0RDO1FBakRDLElBQU0sVUFBVSxHQUFHLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDO1FBRWxDLE9BQU8sSUFBSSxDQUFDLElBQUksQ0FBQyxNQUFNO2FBQ3BCLEdBQUcsQ0FBQyxVQUFDLENBQUMsRUFBRSxDQUFDO1lBQ1IsSUFBSSxPQUFPLENBQUMsQ0FBQyxDQUFDLEtBQUssV0FBVyxJQUFJLE9BQU8sQ0FBQyxDQUFDLENB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Q0FBQyxDQUFDO3FCQUMzRCxDQUFDO2lCQUNILENBQUM7YUFDSDtTQUNGLENBQUM7T0FDVyxxQkFBcUIsQ0FnSmpDO0lBQUQsNEJBQUM7Q0FBQSxBQWhKRCxJQWdKQztTQWhKWSxxQkFBcUIiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQge1xuICBDb21wb25lbnQsXG4gIElucHV0LFxuICBPdXRwdXQsXG4gIFNpbXBsZUNoYW5nZXMsXG4gIEV2ZW50RW1pdHRlcixcbiAgT25DaGFuZ2VzLFxuICBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSxcbiAgVGVtcGxhdGVSZWZcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyB0cmlnZ2VyLCBzdHlsZSwgYW5pbWF0ZSwgdHJhbnNpdGlvbiB9IGZyb20gJ0Bhbmd1bGFyL2FuaW1hdGlvbnMnO1xuaW1wb3J0IHsgZm9ybWF0TGFiZWwsIGVzY2FwZUxhYmVsIH0gZnJvbSAnLi4vY29tbW9uL2xhYmVsLmhlbHBlcic7XG5cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ2dbbmd4LWNoYXJ0cy1idWJibGUtc2VyaWVzXScsXG4gIHRlbXBsYXRlOiBgXG4gICAgPHN2ZzpnICpuZ0Zvcj1cImxldCBjaXJjbGUgb2YgY2lyY2xlczsgdHJhY2tCeTogdHJhY2tCeVwiPlxuICAgICAgPHN2ZzpnIFthdHRyLnRyYW5zZm9ybV09XCJjaXJjbGUudHJhbnNmb3JtXCI+XG4gICAgICAgIDxzdmc6Z1xuICAgICAgICAgIG5neC1jaGFydHMtY2lyY2xlXG4gICAgICAgICAgW0BhbmltYXRpb25TdGF0ZV09XCInYWN0aXZlJ1wiXG4gICAgICAgICAgY2xhc3M9XCJjaXJjbGVcIlxuICAgICAgICAgIFtjeF09XCIwXCJcbiAgICAgICAgICBbY3ldPVwiMFwiXG4gICAgICAgICAgW3JdPVwiY2lyY2xlLnJhZGl1c1wiXG4gICAgICAgICAgW2ZpbGxdPVwiY2lyY2xlLmNvbG9yXCJcbiAgICAgICAgICBbc3R5bGUub3BhY2l0eV09XCJjaXJjbGUub3BhY2l0eVwiXG4gICAgICAgICAgW2NsYXNzLmFjdGl2ZV09XCJjaXJjbGUuaXNBY3RpdmVcIlxuICAgICAgICAgIFtwb2ludGVyRXZlbnRzXT1cIidhbGwnXCJcbiAgICAgICAgICBbZGF0YV09XCJjaXJjbGUudmFsdWVcIlxuICAgICAgICAgIFtjbGFzc05hbWVzXT1cImNpcmNsZS5jbGFzc05hbWVzXCJcbiAgICAgICAgICAoc2VsZWN0KT1cIm9uQ2xpY2soY2lyY2xlLmRhdGEpXCJcbiAgICAgICAgICAoYWN0aXZhdGUpPVwiYWN0aXZhdGVDaXJjbGUoY2lyY2xlKVwiXG4gICAgICAgICAgKGRlYWN0aXZhdGUpPVwiZGVhY3RpdmF0ZUNpcmNsZShjaXJjbGUpXCJcbiAgICAgICAgICBuZ3gtdG9vbHRpcFxuICAgICAgICAgIFt0b29sdGlwRGlzYWJsZWRdPVwidG9vbHRpcERpc2FibGVkXCJcbiAgICAgICAgICBbdG9vbHRpcFBsYWNlbWVudF09XCIndG9wJ1wiXG4gICAgICAgICAgW3Rvb2x0aXBUeXBlXT1cIid0b29sdGlwJ1wiXG4gICAgICAgICAgW3Rvb2x0aXBUaXRsZV09XCJ0b29sdGlwVGVtcGxhdGUgPyB1bmRlZmluZWQgOiBnZXRUb29sdGlwVGV4dChjaXJjbGUpXCJcbiAgICAgICAgICBbdG9vbHRpcFRlbXBsYXRlXT1cInRvb2x0aXBUZW1wbGF0ZVwiXG4gICAgICAgICAgW3Rvb2x0aXBDb250ZXh0XT1cImNpcmNsZS5kYXRhXCJcbiAgICAgICAgLz5cbiAgICAgIDwvc3ZnOmc+XG4gICAgPC9zdmc6Zz5cbiAgYCxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2gsXG4gIGFuaW1hdGlvbnM6IFtcbiAgICB0cmlnZ2VyKCdhbmltYXRpb25TdGF0ZScsIFtcbiAgICAgIHRyYW5zaXRpb24oJzplbnRlcicsIFtcbiAgICAgICAgc3R5bGUoe1xuICAgICAgICAgIG9wYWNpdHk6IDAsXG4gICAgICAgICAgdHJhbnNmb3JtOiAnc2NhbGUoMCknXG4gICAgICAgIH0pLFxuICAgICAgICBhbmltYXRlKDI1MCwgc3R5bGUoeyBvcGFjaXR5OiAxLCB0cmFuc2Zvcm06ICdzY2FsZSgxKScgfSkpXG4gICAgICBdKVxuICAgIF0pXG4gIF1cbn0pXG5leHBvcnQgY2xhc3MgQnViYmxlU2VyaWVzQ29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgZGF0YTtcbiAgQElucHV0KCkgeFNjYWxlO1xuICBASW5wdXQoKSB5U2NhbGU7XG4gIEBJbnB1dCgpIHJTY2FsZTtcbiAgQElucHV0KCkgeFNjYWxlVHlwZTtcbiAgQElucHV0KCkgeVNjYWxlVHlwZTtcbiAgQElucHV0KCkgY29sb3JzO1xuICBASW5wdXQoKSB2aXNpYmxlVmFsdWU7XG4gIEBJbnB1dCgpIGFjdGl2ZUVudHJpZXM6IGFueVtdO1xuICBASW5wdXQoKSB4QXhpc0xhYmVsOiBzdHJpbmc7XG4gIEBJbnB1dCgpIHlBeGlzTGFiZWw6IHN0cmluZztcbiAgQElucHV0KCkgdG9vbHRpcERpc2FibGVkOiBib29sZWFuID0gZmFsc2U7XG4gIEBJbnB1dCgpIHRvb2x0aXBUZW1wbGF0ZTogVGVtcGxhdGVSZWY8YW55PjtcblxuICBAT3V0cHV0KCkgc2VsZWN0ID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuICBAT3V0cHV0KCkgYWN0aXZhdGUgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBkZWFjdGl2YXRlID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuXG4gIGFyZWFQYXRoOiBhbnk7XG4gIGNpcmNsZXM6IGFueVtdO1xuXG4gIG5nT25DaGFuZ2VzKGNoYW5nZXM6IFNpbXBsZUNoYW5nZXMpOiB2b2lkIHtcbiAgICB0aGlzLnVwZGF0ZSgpO1xuICB9XG5cbiAgdXBkYXRlKCk6IHZvaWQge1xuICAgIHRoaXMuY2lyY2xlcyA9IHRoaXMuZ2V0Q2lyY2xlcygpO1xuICB9XG5cbiAgZ2V0Q2lyY2xlcygpOiBhbnlbXSB7XG4gICAgY29uc3Qgc2VyaWVzTmFtZSA9IHRoaXMuZGF0YS5uYW1lO1xuXG4gICAgcmV0dXJuIHRoaXMuZGF0YS5zZXJpZXNcbiAgICAgIC5tYXAoKGQsIGkpID0+IHtcbiAgICAgICAgaWYgKHR5cGVvZiBkLnkgIT09ICd1bmRlZmluZWQnICYmIHR5cGVvZiBkLnggIT09ICd1bmRlZmluZWQnKSB7XG4gICAgICAgICAgY29uc3QgeSA9IGQueTtcbiAgICAgICAgICBjb25zdCB4ID0gZC54O1xuICAgICAgICAgIGNvbnN0IHIgPSBkLnI7XG5cbiAgICAgICAgICBjb25zdCByYWRpdXMgPSB0aGlzLnJTY2FsZShyIHx8IDEpO1xuICAgICAgICAgIGNvbnN0IHRvb2x0aXBMYWJlbCA9IGZvcm1hdExhYmVsKGQubmFtZSk7XG5cbiAgICAgICAgICBjb25zdCBjeCA9IHRoaXMueFNjYWxlVHlwZSA9PT0gJ2xpbmVhcicgPyB0aGlzLnhTY2FsZShOdW1iZXIoeCkpIDogdGhpcy54U2NhbGUoeCk7XG4gICAgICAgICAgY29uc3QgY3kgPSB0aGlzLnlTY2FsZVR5cGUgPT09ICdsaW5lYXInID8gdGhpcy55U2NhbGUoTnVtYmVyKHkpKSA6IHRoaXMueVNjYWxlKHkpO1xuXG4gICAgICAgICAgY29uc3QgY29sb3IgPSB0aGlzLmNvbG9ycy5zY2FsZVR5cGUgPT09ICdsaW5lYXInID8gdGhpcy5jb2xvcnMuZ2V0Q29sb3IocikgOiB0aGlzLmNvbG9ycy5nZXRDb2xvcihzZXJpZXNOYW1lKTtcblxuICAgICAgICAgIGNvbnN0IGlzQWN0aXZlID0gIXRoaXMuYWN0aXZlRW50cmllcy5sZW5ndGggPyB0cnVlIDogdGhpcy5pc0FjdGl2ZSh7IG5hbWU6IHNlcmllc05hbWUgfSk7XG4gICAgICAgICAgY29uc3Qgb3BhY2l0eSA9IGlzQWN0aXZlID8gMSA6IDAuMztcblxuICAgICAgICAgIGNvbnN0IGRhdGEgPSBPYmplY3QuYXNzaWduKHt9LCBkLCB7XG4gICAgICAgICAgICBzZXJpZXM6IHNlcmllc05hbWUsXG4gICAgICAgICAgICBuYW1lOiBkLm5hbWUsXG4gICAgICAgICAgICB2YWx1ZTogZC55LFxuICAgICAgICAgICAgeDogZC54LFxuICAgICAgICAgICAgcmFkaXVzOiBkLnJcbiAgICAgICAgICB9KTtcblxuICAgICAgICAgIHJldHVybiB7XG4gICAgICAgICAgICBkYXRhLFxuICAgICAgICAgICAgeCxcbiAgICAgICAgICAgIHksXG4gICAgICAgICAgICByLFxuICAgICAgICAgICAgY2xhc3NOYW1lczogW2BjaXJjbGUtZGF0YS0ke2l9YF0sXG4gICAgICAgICAgICB2YWx1ZTogeSxcbiAgICAgICAgICAgIGxhYmVsOiB4LFxuICAgICAgICAgICAgY3gsXG4gICAgICAgICAgICBjeSxcbiAgICAgICAgICAgIHJhZGl1cyxcbiAgICAgICAgICAgIHRvb2x0aXBMYWJlbCxcbiAgICAgICAgICAgIGNvbG9yLFxuICAgICAgICAgICAgb3BhY2l0eSxcbiAgICAgICAgICAgIHNlcmllc05hbWUsXG4gICAgICAgICAgICBpc0FjdGl2ZSxcbiAgICAgICAgICAgIHRyYW5zZm9ybTogYHRyYW5zbGF0ZSgke2N4fSwke2N5fSlgXG4gICAgICAgICAgfTtcbiAgICAgICAgfVxuICAgICAgfSlcbiAgICAgIC5maWx0ZXIoY2lyY2xlID0+IGNpcmNsZSAhPT0gdW5kZWZpbmVkKTtcbiAgfVxuXG4gIGdldFRvb2x0aXBUZXh0KGNpcmNsZSk6IHN0cmluZyB7XG4gICAgY29uc3QgaGFzUmFkaXVzID0gdHlwZW9mIGNpcmNsZS5yICE9PSAndW5kZWZpbmVkJztcbiAgICBjb25zdCBoYXNUb29sdGlwTGFiZWwgPSBjaXJjbGUudG9vbHRpcExhYmVsICYmIGNpcmNsZS50b29sdGlwTGFiZWwubGVuZ3RoO1xuICAgIGNvbnN0IGhhc1Nlcmllc05hbWUgPSBjaXJjbGUuc2VyaWVzTmFtZSAmJiBjaXJjbGUuc2VyaWVzTmFtZS5sZW5ndGg7XG5cbiAgICBjb25zdCByYWRpdXNWYWx1ZSA9IGhhc1JhZGl1cyA/IGZvcm1hdExhYmVsKGNpcmNsZS5yKSA6ICcnO1xuICAgIGNvbnN0IHhBeGlzTGFiZWwgPSB0aGlzLnhBeGlzTGFiZWwgJiYgdGhpcy54QXhpc0xhYmVsICE9PSAnJyA/IGAke3RoaXMueEF4aXNMYWJlbH06YCA6ICcnO1xuICAgIGNvbnN0IHlBeGlzTGFiZWwgPSB0aGlzLnlBeGlzTGFiZWwgJiYgdGhpcy55QXhpc0xhYmVsICE9PSAnJyA/IGAke3RoaXMueUF4aXNMYWJlbH06YCA6ICcnO1xuICAgIGNvbnN0IHggPSBmb3JtYXRMYWJlbChjaXJjbGUueCk7XG4gICAgY29uc3QgeSA9IGZvcm1hdExhYmVsKGNpcmNsZS55KTtcbiAgICBjb25zdCBuYW1lID1cbiAgICAgIGhhc1Nlcmllc05hbWUgJiYgaGFzVG9vbHRpcExhYmVsXG4gICAgICAgID8gYCR7Y2lyY2xlLnNlcmllc05hbWV9IOKAoiAke2NpcmNsZS50b29sdGlwTGFiZWx9YFxuICAgICAgICA6IGNpcmNsZS5zZXJpZXNOYW1lICsgY2lyY2xlLnRvb2x0aXBMYWJlbDtcbiAgICBjb25zdCB0b29sdGlwVGl0bGUgPVxuICAgICAgaGFzU2VyaWVzTmFtZSB8fCBoYXNUb29sdGlwTGFiZWwgPyBgPHNwYW4gY2xhc3M9XCJ0b29sdGlwLWxhYmVsXCI+JHtlc2NhcGVMYWJlbChuYW1lKX08L3NwYW4+YCA6ICcnO1xuXG4gICAgcmV0dXJuIGBcbiAgICAgICR7dG9vbHRpcFRpdGxlfVxuICAgICAgPHNwYW4gY2xhc3M9XCJ0b29sdGlwLWxhYmVsXCI+XG4gICAgICAgIDxsYWJlbD4ke2VzY2FwZUxhYmVsKHhBeGlzTGFiZWwpfTwvbGFiZWw+ICR7ZXNjYXBlTGFiZWwoeCl9PGJyIC8+XG4gICAgICAgIDxsYWJlbD4ke2VzY2FwZUxhYmVsKHlBeGlzTGFiZWwpfTwvbGFiZWw+ICR7ZXNjYXBlTGFiZWwoeSl9XG4gICAgICA8L3NwYW4+XG4gICAgICA8c3BhbiBjbGFzcz1cInRvb2x0aXAtdmFsXCI+XG4gICAgICAgICR7ZXNjYXBlTGFiZWwocmFkaXVzVmFsdWUpfVxuICAgICAgPC9zcGFuPlxuICAgIGA7XG4gIH1cblxuICBvbkNsaWNrKGRhdGEpOiB2b2lkIHtcbiAgICB0aGlzLnNlbGVjdC5lbWl0KGRhdGEpO1xuICB9XG5cbiAgaXNBY3RpdmUoZW50cnkpOiBib29sZWFuIHtcbiAgICBpZiAoIXRoaXMuYWN0aXZlRW50cmllcykgcmV0dXJuIGZhbHNlO1xuICAgIGNvbnN0IGl0ZW0gPSB0aGlzLmFjdGl2ZUVudHJpZXMuZmluZChkID0+IHtcbiAgICAgIHJldHVybiBlbnRyeS5uYW1lID09PSBkLm5hbWU7XG4gICAgfSk7XG4gICAgcmV0dXJuIGl0ZW0gIT09IHVuZGVmaW5lZDtcbiAgfVxuXG4gIGlzVmlzaWJsZShjaXJjbGUpOiBib29sZWFuIHtcbiAgICBpZiAodGhpcy5hY3RpdmVFbnRyaWVzLmxlbmd0aCA+IDApIHtcbiAgICAgIHJldHVybiB0aGlzLmlzQWN0aXZlKHsgbmFtZTogY2lyY2xlLnNlcmllc05hbWUgfSk7XG4gICAgfVxuXG4gICAgcmV0dXJuIGNpcmNsZS5vcGFjaXR5ICE9PSAwO1xuICB9XG5cbiAgYWN0aXZhdGVDaXJjbGUoY2lyY2xlKTogdm9pZCB7XG4gICAgY2lyY2xlLmJhclZpc2libGUgPSB0cnVlO1xuICAgIHRoaXMuYWN0aXZhdGUuZW1pdCh7IG5hbWU6IHRoaXMuZGF0YS5uYW1lIH0pO1xuICB9XG5cbiAgZGVhY3RpdmF0ZUNpcmNsZShjaXJjbGUpOiB2b2lkIHtcbiAgICBjaXJjbGUuYmFyVmlzaWJsZSA9IGZhbHNlO1xuICAgIHRoaXMuZGVhY3RpdmF0ZS5lbWl0KHsgbmFtZTogdGhpcy5kYXRhLm5hbWUgfSk7XG4gIH1cblxuICB0cmFja0J5KGluZGV4LCBjaXJjbGUpOiBzdHJpbmcge1xuICAgIHJldHVybiBgJHtjaXJjbGUuZGF0YS5zZXJpZXN9ICR7Y2lyY2xlLmRhdGEubmFtZX1gO1xuICB9XG59XG4iXX0=