import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy } from '@angular/core';
import { invertColor } from '../utils/color-utils';
var CardSeriesComponent = /** @class */ (function () {
    function CardSeriesComponent() {
        this.innerPadding = 15;
        this.emptyColor = 'rgba(0, 0, 0, 0)';
        this.animations = true;
        this.select = new EventEmitter();
    }
    CardSeriesComponent.prototype.ngOnChanges = function (changes) {
        this.update();
    };
    CardSeriesComponent.prototype.update = function () {
        if (this.data.length > 2) {
            var valueFormatting_1 = this.valueFormatting || (function (card) { return card.value.toLocaleString(); });
            var sortedLengths = this.data
                .map(function (d) {
                var hasValue = d && d.data && typeof d.data.value !== 'undefined' && d.data.value !== null;
                return hasValue
                    ? valueFormatting_1({
                        data: d.data,
                        label: d ? d.data.name : '',
                        value: d && d.data ? d.data.value : ''
                    }).length
                    : 0;
            })
                .sort(function (a, b) { return b - a; });
            var idx = Math.ceil(this.data.length / 2);
            this.medianSize = sortedLengths[idx];
        }
        var cards = this.getCards();
        this.cards = cards.filter(function (d) { return d.data.value !== null; });
        this.emptySlots = cards.filter(function (d) { return d.data.value === null; });
    };
    CardSeriesComponent.prototype.getCards = function () {
        var _this = this;
        var yPadding = typeof this.innerPadding === 'number' ? this.innerPadding : this.innerPadding[0] + this.innerPadding[2];
        var xPadding = typeof this.innerPadding === 'number' ? this.innerPadding : this.innerPadding[1] + this.innerPadding[3];
        return this.data.map(function (d, index) {
            var label = d.data.name;
            if (label && label.constructor.name === 'Date') {
                label = label.toLocaleDateString();
            }
            else {
                label = label ? label.toLocaleString() : label;
            }
            var value = d.data.value;
            var valueColor = label ? _this.colors.getColor(label) : _this.emptyColor;
            var color = _this.cardColor || valueColor || '#000';
            return {
                x: d.x,
                y: d.y,
                width: d.width - xPadding,
                height: d.height - yPadding,
                color: color,
                bandColor: _this.bandColor || valueColor,
                textColor: _this.textColor || invertColor(color),
                label: label,
                data: d.data,
                tooltipText: label + ": " + value
            };
        });
    };
    CardSeriesComponent.prototype.trackBy = function (index, card) {
        return card.label;
    };
    CardSeriesComponent.prototype.onClick = function (data) {
        this.select.emit(data);
    };
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "data", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "slots", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "dims", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "colors", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "innerPadding", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "cardColor", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "bandColor", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "emptyColor", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "textColor", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "valueFormatting", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "labelFormatting", void 0);
    __decorate([
        Input()
    ], CardSeriesComponent.prototype, "animations", void 0);
    __decorate([
        Output()
    ], CardSeriesComponent.prototype, "select", void 0);
    CardSeriesComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-card-series]',
            template: "\n    <svg:rect\n      *ngFor=\"let c of emptySlots; trackBy: trackBy\"\n      class=\"card-empty\"\n      [attr.x]=\"c.x\"\n      [attr.y]=\"c.y\"\n      [style.fill]=\"emptyColor\"\n      [attr.width]=\"c.width\"\n      [attr.height]=\"c.height\"\n      rx=\"3\"\n      ry=\"3\"\n    />\n    <svg:g\n      ngx-charts-card\n      *ngFor=\"let c of cards; trackBy: trackBy\"\n      [x]=\"c.x\"\n      [y]=\"c.y\"\n      [width]=\"c.width\"\n      [height]=\"c.height\"\n      [color]=\"c.color\"\n      [bandColor]=\"c.bandColor\"\n      [textColor]=\"c.textColor\"\n      [data]=\"c.data\"\n      [label]=\"c.label\"\n      [medianSize]=\"medianSize\"\n      [valueFormatting]=\"valueFormatting\"\n      [labelFormatting]=\"labelFormatting\"\n      [animations]=\"animations\"\n      (select)=\"onClick($event)\"\n    />\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], CardSeriesComponent);
    return CardSeriesComponent;
}());
export { CardSeriesComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2FyZC1zZXJpZXMuY29tcG9uZW50LmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvbnVtYmVyLWNhcmQvY2FyZC1zZXJpZXMuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQ0wsU0FBUyxFQUNULEtBQUssRUFDTCxNQUFNLEVBQ04sWUFBWSxFQUdaLHVCQUF1QixFQUN4QixNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUUsV0FBVyxFQUFFLE1BQU0sc0JBQXNCLENBQUM7QUFnRG5EO0lBQUE7UUFLVyxpQkFBWSxHQUFHLEVBQUUsQ0FBQztRQUlsQixlQUFVLEdBQUcsa0JBQWtCLENBQUM7UUFJaEMsZUFBVSxHQUFZLElBQUksQ0FBQztRQUUxQixXQUFNLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztJQTBFeEMsQ0FBQztJQXBFQyx5Q0FBVyxHQUFYLFVBQVksT0FBc0I7UUFDaEMsSUFBSSxDQUFDLE1BQU0sRUFBRSxDQUFDO0lBQ2hCLENBQUM7SUFFRCxvQ0FBTSxHQUFOO1FBQ0UsSUFBSSxJQUFJLENBQUMsSUFBSSxDQUFDLE1BQU0sR0FBRyxDQUFDLEVBQUU7WUFDeEIsSUFBTSxpQkFBZSxHQUFHLElBQUksQ0FBQyxlQUFlLElBQUksQ0FBQyxVQUFBLElBQUksSUFBSSxPQUFBLElBQUksQ0FBQyxLQUFLLENBQUMsY0FBYyxFQUFFLEVBQTNCLENBQTJCLENBQUMsQ0FBQztZQUV0RixJQUFNLGFBQWEsR0FBRyxJQUFJLENBQUMsSUFBSTtpQkFDNUIsR0FBRyxDQUFDLFVBQUEsQ0FBQztnQkFDSixJQUFNLFFBQVEsR0FBRyxDQUFDLElBQUksQ0FBQyxDQUFDLElBQUksSUFBSSxPQUFPLENBQUMsQ0FBQyxJQUFJLENBQUMsS0FBSyxLQUFLLFdBQVcsSUFBSSxDQUFDLENBQUMsSUFBSSxDQUFDLEtBQUssS0FBSyxJQUFJLENBQUM7Z0JBQzdGLE9BQU8s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QyxRQUFRLEVBQUUsMnpCQThCVDtZQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO1NBQ2hELENBQUM7T0FDVyxtQkFBbUIsQ0F5Ri9CO0lBQUQsMEJBQUM7Q0FBQSxBQXpGRCxJQXlGQztTQXpGWSxtQkFBbUIiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQge1xuICBDb21wb25lbnQsXG4gIElucHV0LFxuICBPdXRwdXQsXG4gIEV2ZW50RW1pdHRlcixcbiAgT25DaGFuZ2VzLFxuICBTaW1wbGVDaGFuZ2VzLFxuICBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneVxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IGludmVydENvbG9yIH0gZnJvbSAnLi4vdXRpbHMvY29sb3ItdXRpbHMnO1xuXG5leHBvcnQgaW50ZXJmYWNlIENhcmRNb2RlbCB7XG4gIHg7XG4gIHk7XG4gIHdpZHRoOiBudW1iZXI7XG4gIGhlaWdodDogbnVtYmVyO1xuICBjb2xvcjogc3RyaW5nO1xuICBsYWJlbDogc3RyaW5nO1xuICBkYXRhO1xuICB0b29sdGlwVGV4dDogc3RyaW5nO1xufVxuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdnW25neC1jaGFydHMtY2FyZC1zZXJpZXNdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOnJlY3RcbiAgICAgICpuZ0Zvcj1cImxldCBjIG9mIGVtcHR5U2xvdHM7IHRyYWNrQnk6IHRyYWNrQnlcIlxuICAgICAgY2xhc3M9XCJjYXJkLWVtcHR5XCJcbiAgICAgIFthdHRyLnhdPVwiYy54XCJcbiAgICAgIFthdHRyLnldPVwiYy55XCJcbiAgICAgIFtzdHlsZS5maWxsXT1cImVtcHR5Q29sb3JcIlxuICAgICAgW2F0dHIud2lkdGhdPVwiYy53aWR0aFwiXG4gICAgICBbYXR0ci5oZWlnaHRdPVwiYy5oZWlnaHRcIlxuICAgICAgcng9XCIzXCJcbiAgICAgIHJ5PVwiM1wiXG4gICAgLz5cbiAgICA8c3ZnOmdcbiAgICAgIG5neC1jaGFydHMtY2FyZFxuICAgICAgKm5nRm9yPVwibGV0IGMgb2YgY2FyZHM7IHRyYWNrQnk6IHRyYWNrQnlcIlxuICAgICAgW3hdPVwiYy54XCJcbiAgICAgIFt5XT1cImMueVwiXG4gICAgICBbd2lkdGhdPVwiYy53aWR0aFwiXG4gICAgICBbaGVpZ2h0XT1cImMuaGVpZ2h0XCJcbiAgICAgIFtjb2xvcl09XCJjLmNvbG9yXCJcbiAgICAgIFtiYW5kQ29sb3JdPVwiYy5iYW5kQ29sb3JcIlxuICAgICAgW3RleHRDb2xvcl09XCJjLnRleHRDb2xvclwiXG4gICAgICBbZGF0YV09XCJjLmRhdGFcIlxuICAgICAgW2xhYmVsXT1cImMubGFiZWxcIlxuICAgICAgW21lZGlhblNpemVdPVwibWVkaWFuU2l6ZVwiXG4gICAgICBbdmFsdWVGb3JtYXR0aW5nXT1cInZhbHVlRm9ybWF0dGluZ1wiXG4gICAgICBbbGFiZWxGb3JtYXR0aW5nXT1cImxhYmVsRm9ybWF0dGluZ1wiXG4gICAgICBbYW5pbWF0aW9uc109XCJhbmltYXRpb25zXCJcbiAgICAgIChzZWxlY3QpPVwib25DbGljaygkZXZlbnQpXCJcbiAgICAvPlxuICBgLFxuICBjaGFuZ2VEZXRlY3Rpb246IENoYW5nZURldGVjdGlvblN0cmF0ZWd5Lk9uUHVzaFxufSlcbmV4cG9ydCBjbGFzcyBDYXJkU2VyaWVzQ29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgZGF0YTogYW55W107XG4gIEBJbnB1dCgpIHNsb3RzOiBhbnlbXTtcbiAgQElucHV0KCkgZGltcztcbiAgQElucHV0KCkgY29sb3JzO1xuICBASW5wdXQoKSBpbm5lclBhZGRpbmcgPSAxNTtcblxuICBASW5wdXQoKSBjYXJkQ29sb3I7XG4gIEBJbnB1dCgpIGJhbmRDb2xvcjtcbiAgQElucHV0KCkgZW1wdHlDb2xvciA9ICdyZ2JhKDAsIDAsIDAsIDApJztcbiAgQElucHV0KCkgdGV4dENvbG9yO1xuICBASW5wdXQoKSB2YWx1ZUZvcm1hdHRpbmc6IGFueTtcbiAgQElucHV0KCkgbGFiZWxGb3JtYXR0aW5nOiBhbnk7XG4gIEBJbnB1dCgpIGFuaW1hdGlvbnM6IGJvb2xlYW4gPSB0cnVlO1xuXG4gIEBPdXRwdXQoKSBzZWxlY3QgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgY2FyZHM6IENhcmRNb2RlbFtdO1xuICBlbXB0eVNsb3RzOiBhbnlbXTtcbiAgbWVkaWFuU2l6ZTogbnVtYmVyO1xuXG4gIG5nT25DaGFuZ2VzKGNoYW5nZXM6IFNpbXBsZUNoYW5nZXMpOiB2b2lkIHtcbiAgICB0aGlzLnVwZGF0ZSgpO1xuICB9XG5cbiAgdXBkYXRlKCk6IHZvaWQge1xuICAgIGlmICh0aGlzLmRhdGEubGVuZ3RoID4gMikge1xuICAgICAgY29uc3QgdmFsdWVGb3JtYXR0aW5nID0gdGhpcy52YWx1ZUZvcm1hdHRpbmcgfHwgKGNhcmQgPT4gY2FyZC52YWx1ZS50b0xvY2FsZVN0cmluZygpKTtcblxuICAgICAgY29uc3Qgc29ydGVkTGVuZ3RocyA9IHRoaXMuZGF0YVxuICAgICAgICAubWFwKGQgPT4ge1xuICAgICAgICAgIGNvbnN0IGhhc1ZhbHVlID0gZCAmJiBkLmRhdGEgJiYgdHlwZW9mIGQuZGF0YS52YWx1ZSAhPT0gJ3VuZGVmaW5lZCcgJiYgZC5kYXRhLnZhbHVlICE9PSBudWxsO1xuICAgICAgICAgIHJldHVybiBoYXNWYWx1ZVxuICAgICAgICAgICAgPyB2YWx1ZUZvcm1hdHRpbmcoe1xuICAgICAgICAgICAgICAgIGRhdGE6IGQuZGF0YSxcbiAgICAgICAgICAgICAgICBsYWJlbDogZCA/IGQuZGF0YS5uYW1lIDogJycsXG4gICAgICAgICAgICAgICAgdmFsdWU6IGQgJiYgZC5kYXRhID8gZC5kYXRhLnZhbHVlIDogJydcbiAgICAgICAgICAgICAgfSkubGVuZ3RoXG4gICAgICAgICAgICA6IDA7XG4gICAgICAgIH0pXG4gICAgICAgIC5zb3J0KChhLCBiKSA9PiBiIC0gYSk7XG4gICAgICBjb25zdCBpZHggPSBNYXRoLmNlaWwodGhpcy5kYXRhLmxlbmd0aCAvIDIpO1xuICAgICAgdGhpcy5tZWRpYW5TaXplID0gc29ydGVkTGVuZ3Roc1tpZHhdO1xuICAgIH1cblxuICAgIGNvbnN0IGNhcmRzID0gdGhpcy5nZXRDYXJkcygpO1xuICAgIHRoaXMuY2FyZHMgPSBjYXJkcy5maWx0ZXIoZCA9PiBkLmRhdGEudmFsdWUgIT09IG51bGwpO1xuICAgIHRoaXMuZW1wdHlTbG90cyA9IGNhcmRzLmZpbHRlcihkID0+IGQuZGF0YS52YWx1ZSA9PT0gbnVsbCk7XG4gIH1cblxuICBnZXRDYXJkcygpOiBhbnlbXSB7XG4gICAgY29uc3QgeVBhZGRpbmcgPVxuICAgICAgdHlwZW9mIHRoaXMuaW5uZXJQYWRkaW5nID09PSAnbnVtYmVyJyA/IHRoaXMuaW5uZXJQYWRkaW5nIDogdGhpcy5pbm5lclBhZGRpbmdbMF0gKyB0aGlzLmlubmVyUGFkZGluZ1syXTtcbiAgICBjb25zdCB4UGFkZGluZyA9XG4gICAgICB0eXBlb2YgdGhpcy5pbm5lclBhZGRpbmcgPT09ICdudW1iZXInID8gdGhpcy5pbm5lclBhZGRpbmcgOiB0aGlzLmlubmVyUGFkZGluZ1sxXSArIHRoaXMuaW5uZXJQYWRkaW5nWzNdO1xuXG4gICAgcmV0dXJuIHRoaXMuZGF0YS5tYXAoKGQsIGluZGV4KSA9PiB7XG4gICAgICBsZXQgbGFiZWwgPSBkLmRhdGEubmFtZTtcbiAgICAgIGlmIChsYWJlbCAmJiBsYWJlbC5jb25zdHJ1Y3Rvci5uYW1lID09PSAnRGF0ZScpIHtcbiAgICAgICAgbGFiZWwgPSBsYWJlbC50b0xvY2FsZURhdGVTdHJpbmcoKTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIGxhYmVsID0gbGFiZWwgPyBsYWJlbC50b0xvY2FsZVN0cmluZygpIDogbGFiZWw7XG4gICAgICB9XG5cbiAgICAgIGNvbnN0IHZhbHVlID0gZC5kYXRhLnZhbHVlO1xuICAgICAgY29uc3QgdmFsdWVDb2xvciA9IGxhYmVsID8gdGhpcy5jb2xvcnMuZ2V0Q29sb3IobGFiZWwpIDogdGhpcy5lbXB0eUNvbG9yO1xuICAgICAgY29uc3QgY29sb3IgPSB0aGlzLmNhcmRDb2xvciB8fCB2YWx1ZUNvbG9yIHx8ICcjMDAwJztcbiAgICAgIHJldHVybiB7XG4gICAgICAgIHg6IGQueCxcbiAgICAgICAgeTogZC55LFxuICAgICAgICB3aWR0aDogZC53aWR0aCAtIHhQYWRkaW5nLFxuICAgICAgICBoZWlnaHQ6IGQuaGVpZ2h0IC0geVBhZGRpbmcsXG4gICAgICAgIGNvbG9yLFxuICAgICAgICBiYW5kQ29sb3I6IHRoaXMuYmFuZENvbG9yIHx8IHZhbHVlQ29sb3IsXG4gICAgICAgIHRleHRDb2xvcjogdGhpcy50ZXh0Q29sb3IgfHwgaW52ZXJ0Q29sb3IoY29sb3IpLFxuICAgICAgICBsYWJlbCxcbiAgICAgICAgZGF0YTogZC5kYXRhLFxuICAgICAgICB0b29sdGlwVGV4dDogYCR7bGFiZWx9OiAke3ZhbHVlfWBcbiAgICAgIH07XG4gICAgfSk7XG4gIH1cblxuICB0cmFja0J5KGluZGV4LCBjYXJkKTogc3RyaW5nIHtcbiAgICByZXR1cm4gY2FyZC5sYWJlbDtcbiAgfVxuXG4gIG9uQ2xpY2soZGF0YSk6IHZvaWQge1xuICAgIHRoaXMuc2VsZWN0LmVtaXQoZGF0YSk7XG4gIH1cbn1cbiJdfQ==