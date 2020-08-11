import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy } from '@angular/core';
import { escapeLabel } from '../common/label.helper';
var TreeMapCellSeriesComponent = /** @class */ (function () {
    function TreeMapCellSeriesComponent() {
        this.gradient = false;
        this.tooltipDisabled = false;
        this.animations = true;
        this.select = new EventEmitter();
    }
    TreeMapCellSeriesComponent.prototype.ngOnChanges = function (changes) {
        this.cells = this.getCells();
    };
    TreeMapCellSeriesComponent.prototype.getCells = function () {
        var _this = this;
        return this.data.children
            .filter(function (d) {
            return d.depth === 1;
        })
            .map(function (d, index) {
            var label = d.id;
            return {
                data: d.data,
                x: d.x0,
                y: d.y0,
                width: d.x1 - d.x0,
                height: d.y1 - d.y0,
                fill: _this.colors.getColor(label),
                label: label,
                value: d.value,
                valueType: d.valueType
            };
        });
    };
    TreeMapCellSeriesComponent.prototype.getTooltipText = function (_a) {
        var label = _a.label, value = _a.value;
        return "\n      <span class=\"tooltip-label\">" + escapeLabel(label) + "</span>\n      <span class=\"tooltip-val\">" + value.toLocaleString() + "</span>\n    ";
    };
    TreeMapCellSeriesComponent.prototype.onClick = function (data) {
        this.select.emit(data);
    };
    TreeMapCellSeriesComponent.prototype.trackBy = function (index, item) {
        return item.label;
    };
    __decorate([
        Input()
    ], TreeMapCellSeriesComponent.prototype, "data", void 0);
    __decorate([
        Input()
    ], TreeMapCellSeriesComponent.prototype, "dims", void 0);
    __decorate([
        Input()
    ], TreeMapCellSeriesComponent.prototype, "colors", void 0);
    __decorate([
        Input()
    ], TreeMapCellSeriesComponent.prototype, "valueFormatting", void 0);
    __decorate([
        Input()
    ], TreeMapCellSeriesComponent.prototype, "labelFormatting", void 0);
    __decorate([
        Input()
    ], TreeMapCellSeriesComponent.prototype, "gradient", void 0);
    __decorate([
        Input()
    ], TreeMapCellSeriesComponent.prototype, "tooltipDisabled", void 0);
    __decorate([
        Input()
    ], TreeMapCellSeriesComponent.prototype, "tooltipTemplate", void 0);
    __decorate([
        Input()
    ], TreeMapCellSeriesComponent.prototype, "animations", void 0);
    __decorate([
        Output()
    ], TreeMapCellSeriesComponent.prototype, "select", void 0);
    TreeMapCellSeriesComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-tree-map-cell-series]',
            template: "\n    <svg:g\n      ngx-charts-tree-map-cell\n      *ngFor=\"let c of cells; trackBy: trackBy\"\n      [data]=\"c.data\"\n      [x]=\"c.x\"\n      [y]=\"c.y\"\n      [width]=\"c.width\"\n      [height]=\"c.height\"\n      [fill]=\"c.fill\"\n      [label]=\"c.label\"\n      [value]=\"c.value\"\n      [valueType]=\"c.valueType\"\n      [valueFormatting]=\"valueFormatting\"\n      [labelFormatting]=\"labelFormatting\"\n      [gradient]=\"gradient\"\n      [animations]=\"animations\"\n      (select)=\"onClick($event)\"\n      ngx-tooltip\n      [tooltipDisabled]=\"tooltipDisabled\"\n      [tooltipPlacement]=\"'top'\"\n      [tooltipType]=\"'tooltip'\"\n      [tooltipTitle]=\"tooltipTemplate ? undefined : getTooltipText(c)\"\n      [tooltipTemplate]=\"tooltipTemplate\"\n      [tooltipContext]=\"c.data\"\n    ></svg:g>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], TreeMapCellSeriesComponent);
    return TreeMapCellSeriesComponent;
}());
export { TreeMapCellSeriesComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidHJlZS1tYXAtY2VsbC1zZXJpZXMuY29tcG9uZW50LmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvdHJlZS1tYXAvdHJlZS1tYXAtY2VsbC1zZXJpZXMuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQ0wsU0FBUyxFQUVULEtBQUssRUFDTCxNQUFNLEVBRU4sWUFBWSxFQUNaLHVCQUF1QixFQUV4QixNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUUsV0FBVyxFQUFFLE1BQU0sd0JBQXdCLENBQUM7QUFpQ3JEO0lBQUE7UUFNVyxhQUFRLEdBQVksS0FBSyxDQUFDO1FBQzFCLG9CQUFlLEdBQVksS0FBSyxDQUFDO1FBRWpDLGVBQVUsR0FBWSxJQUFJLENBQUM7UUFFMUIsV0FBTSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7SUE0Q3hDLENBQUM7SUF4Q0MsZ0RBQVcsR0FBWCxVQUFZLE9BQXNCO1FBQ2hDLElBQUksQ0FBQyxLQUFLLEdBQUcsSUFBSSxDQUFDLFFBQVEsRUFBRSxDQUFDO0lBQy9CLENBQUM7SUFFRCw2Q0FBUSxHQUFSO1FBQUEsaUJBb0JDO1FBbkJDLE9BQU8sSUFBSSxDQUFDLElBQUksQ0FBQyxRQUFRO2FBQ3RCLE1BQU0sQ0FBQyxVQUFBLENBQUM7WUFDUCxPQUFPLENBQUMsQ0FBQyxLQUFLLEtBQUssQ0FBQyxDQUFDO1FBQ3ZCLENBQUMsQ0FBQzthQUNELEdBQUcsQ0FBQyxVQUFDLENBQUMsRUFBRSxLQUFLO1lBQ1osSUFBTSxLQUFLLEdBQUcsQ0FBQyxDQUFDLEVBQUUsQ0FBQztZQUVuQixPQUFPO2dCQUNMLElBQUksRUFBRSxDQUFDLENBQUMsSUFBSTtnQkFDWixDQUFDLEVBQUUsQ0FBQyxDQUFDLEVBQUU7Z0JBQ1AsQ0FBQyxFQUFFLENBQUMsQ0FBQyxFQUFFO2dCQUNQLEtBQUssRUFBRSxDQUFDLENBQUMsRUFBRSxHQUFHLENBQUMsQ0FBQyxFQUFFO2dCQUNsQixNQUFNLEVBQUUsQ0FBQyxDQUFDLEVBQUUsR0FBRyxDQUFDLENBQUMsRUFBRTtnQkFDbkIsSUFBSSxFQUFFLEtBQUksQ0FBQyxNQUFNLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQztnQkFDakMsS0FBSyxPQUFBO2dCQUNMLEtBQUssRUFBRSxDQUFDLENBQUMsS0FBSztnQkFDZCxTQUFTLEVBQUUsQ0FBQyxDQUFDLFNBQVM7YUFDdkIsQ0FBQztRQUNKLENBQUMsQ0FBQyxDQUFDO0lBQ1AsQ0FBQztJQUVELG1EQUFjLEdBQWQsVUFBZSxFQUFnQjtZQUFkLGdCQUFLLEVBQUUsZ0JBQUs7UUFDM0IsT0FBTywyQ0FDeUIsV0FBVyxDQUFDLEtBQUssQ0FBQyxtREFDcEIsS0FBSyxDQUFDLGNBQWMsRUFBRSxrQkFDbkQsQ0FBQztJQUNKLENBQUM7SUFFRCw0Q0FBTyxHQUFQLFVBQVEsSUFBSTtRQUNWLElBQUksQ0FBQyxNQUFNLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQ3pCLENBQUM7SUFFRCw0Q0FBTyxHQUFQLFVBQVEsS0FBSyxFQUFFLElBQUk7UUFDakIsT0FBTyxJQUFJLENBQUMsS0FBSyxDQUFDO0lBQ3BCLENBQUM7SUFyRFE7UUFBUixLQUFLLEVBQUU7NERBQU07SUFDTDtRQUFSLEtBQUssRUFBRTs0REFBTTtJQUNMO1FBQVIsS0FBSyxFQUFFOzhEQUFRO0lBQ1A7UUFBUixLQUFLLEVBQUU7dUVBQXNCO0lBQ3JCO1FBQVIsS0FBSyxFQUFFO3VFQUFzQjtJQUNyQjtRQUFSLEtBQUssRUFBRTtnRUFBMkI7SUFDMUI7UUFBUixLQUFLLEVBQUU7dUVBQWtDO0lBQ2pDO1FBQVIsS0FBSyxFQUFFO3VFQUFtQztJQUNsQztRQUFSLEtBQUssRUFBRTtrRUFBNEI7SUFFMUI7UUFBVCxNQUFNLEVBQUU7OERBQTZCO0lBWDNCLDBCQUEwQjtRQS9CdEMsU0FBUyxDQUFDO1lBQ1QsUUFBUSxFQUFFLG9DQUFvQztZQUM5QyxRQUFRLEVBQUUsOHpCQTBCVDtZQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO1NBQ2hELENBQUM7T0FDVywwQkFBMEIsQ0F1RHRDO0lBQUQsaUNBQUM7Q0FBQSxBQXZERCxJQXVEQztTQXZEWSwwQkFBMEIiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQge1xuICBDb21wb25lbnQsXG4gIE9uQ2hhbmdlcyxcbiAgSW5wdXQsXG4gIE91dHB1dCxcbiAgU2ltcGxlQ2hhbmdlcyxcbiAgRXZlbnRFbWl0dGVyLFxuICBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSxcbiAgVGVtcGxhdGVSZWZcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBlc2NhcGVMYWJlbCB9IGZyb20gJy4uL2NvbW1vbi9sYWJlbC5oZWxwZXInO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdnW25neC1jaGFydHMtdHJlZS1tYXAtY2VsbC1zZXJpZXNdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOmdcbiAgICAgIG5neC1jaGFydHMtdHJlZS1tYXAtY2VsbFxuICAgICAgKm5nRm9yPVwibGV0IGMgb2YgY2VsbHM7IHRyYWNrQnk6IHRyYWNrQnlcIlxuICAgICAgW2RhdGFdPVwiYy5kYXRhXCJcbiAgICAgIFt4XT1cImMueFwiXG4gICAgICBbeV09XCJjLnlcIlxuICAgICAgW3dpZHRoXT1cImMud2lkdGhcIlxuICAgICAgW2hlaWdodF09XCJjLmhlaWdodFwiXG4gICAgICBbZmlsbF09XCJjLmZpbGxcIlxuICAgICAgW2xhYmVsXT1cImMubGFiZWxcIlxuICAgICAgW3ZhbHVlXT1cImMudmFsdWVcIlxuICAgICAgW3ZhbHVlVHlwZV09XCJjLnZhbHVlVHlwZVwiXG4gICAgICBbdmFsdWVGb3JtYXR0aW5nXT1cInZhbHVlRm9ybWF0dGluZ1wiXG4gICAgICBbbGFiZWxGb3JtYXR0aW5nXT1cImxhYmVsRm9ybWF0dGluZ1wiXG4gICAgICBbZ3JhZGllbnRdPVwiZ3JhZGllbnRcIlxuICAgICAgW2FuaW1hdGlvbnNdPVwiYW5pbWF0aW9uc1wiXG4gICAgICAoc2VsZWN0KT1cIm9uQ2xpY2soJGV2ZW50KVwiXG4gICAgICBuZ3gtdG9vbHRpcFxuICAgICAgW3Rvb2x0aXBEaXNhYmxlZF09XCJ0b29sdGlwRGlzYWJsZWRcIlxuICAgICAgW3Rvb2x0aXBQbGFjZW1lbnRdPVwiJ3RvcCdcIlxuICAgICAgW3Rvb2x0aXBUeXBlXT1cIid0b29sdGlwJ1wiXG4gICAgICBbdG9vbHRpcFRpdGxlXT1cInRvb2x0aXBUZW1wbGF0ZSA/IHVuZGVmaW5lZCA6IGdldFRvb2x0aXBUZXh0KGMpXCJcbiAgICAgIFt0b29sdGlwVGVtcGxhdGVdPVwidG9vbHRpcFRlbXBsYXRlXCJcbiAgICAgIFt0b29sdGlwQ29udGV4dF09XCJjLmRhdGFcIlxuICAgID48L3N2ZzpnPlxuICBgLFxuICBjaGFuZ2VEZXRlY3Rpb246IENoYW5nZURldGVjdGlvblN0cmF0ZWd5Lk9uUHVzaFxufSlcbmV4cG9ydCBjbGFzcyBUcmVlTWFwQ2VsbFNlcmllc0NvbXBvbmVudCBpbXBsZW1lbnRzIE9uQ2hhbmdlcyB7XG4gIEBJbnB1dCgpIGRhdGE7XG4gIEBJbnB1dCgpIGRpbXM7XG4gIEBJbnB1dCgpIGNvbG9ycztcbiAgQElucHV0KCkgdmFsdWVGb3JtYXR0aW5nOiBhbnk7XG4gIEBJbnB1dCgpIGxhYmVsRm9ybWF0dGluZzogYW55O1xuICBASW5wdXQoKSBncmFkaWVudDogYm9vbGVhbiA9IGZhbHNlO1xuICBASW5wdXQoKSB0b29sdGlwRGlzYWJsZWQ6IGJvb2xlYW4gPSBmYWxzZTtcbiAgQElucHV0KCkgdG9vbHRpcFRlbXBsYXRlOiBUZW1wbGF0ZVJlZjxhbnk+O1xuICBASW5wdXQoKSBhbmltYXRpb25zOiBib29sZWFuID0gdHJ1ZTtcblxuICBAT3V0cHV0KCkgc2VsZWN0ID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuXG4gIGNlbGxzOiBhbnlbXTtcblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKTogdm9pZCB7XG4gICAgdGhpcy5jZWxscyA9IHRoaXMuZ2V0Q2VsbHMoKTtcbiAgfVxuXG4gIGdldENlbGxzKCk6IGFueVtdIHtcbiAgICByZXR1cm4gdGhpcy5kYXRhLmNoaWxkcmVuXG4gICAgICAuZmlsdGVyKGQgPT4ge1xuICAgICAgICByZXR1cm4gZC5kZXB0aCA9PT0gMTtcbiAgICAgIH0pXG4gICAgICAubWFwKChkLCBpbmRleCkgPT4ge1xuICAgICAgICBjb25zdCBsYWJlbCA9IGQuaWQ7XG5cbiAgICAgICAgcmV0dXJuIHtcbiAgICAgICAgICBkYXRhOiBkLmRhdGEsXG4gICAgICAgICAgeDogZC54MCxcbiAgICAgICAgICB5OiBkLnkwLFxuICAgICAgICAgIHdpZHRoOiBkLngxIC0gZC54MCxcbiAgICAgICAgICBoZWlnaHQ6IGQueTEgLSBkLnkwLFxuICAgICAgICAgIGZpbGw6IHRoaXMuY29sb3JzLmdldENvbG9yKGxhYmVsKSxcbiAgICAgICAgICBsYWJlbCxcbiAgICAgICAgICB2YWx1ZTogZC52YWx1ZSxcbiAgICAgICAgICB2YWx1ZVR5cGU6IGQudmFsdWVUeXBlXG4gICAgICAgIH07XG4gICAgICB9KTtcbiAgfVxuXG4gIGdldFRvb2x0aXBUZXh0KHsgbGFiZWwsIHZhbHVlIH0pOiBzdHJpbmcge1xuICAgIHJldHVybiBgXG4gICAgICA8c3BhbiBjbGFzcz1cInRvb2x0aXAtbGFiZWxcIj4ke2VzY2FwZUxhYmVsKGxhYmVsKX08L3NwYW4+XG4gICAgICA8c3BhbiBjbGFzcz1cInRvb2x0aXAtdmFsXCI+JHt2YWx1ZS50b0xvY2FsZVN0cmluZygpfTwvc3Bhbj5cbiAgICBgO1xuICB9XG5cbiAgb25DbGljayhkYXRhKTogdm9pZCB7XG4gICAgdGhpcy5zZWxlY3QuZW1pdChkYXRhKTtcbiAgfVxuXG4gIHRyYWNrQnkoaW5kZXgsIGl0ZW0pOiBzdHJpbmcge1xuICAgIHJldHVybiBpdGVtLmxhYmVsO1xuICB9XG59XG4iXX0=