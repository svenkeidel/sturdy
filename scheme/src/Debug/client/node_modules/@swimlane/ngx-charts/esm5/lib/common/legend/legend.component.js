import { __decorate, __values } from "tslib";
import { Component, Input, ChangeDetectionStrategy, Output, EventEmitter, SimpleChanges, OnChanges, ChangeDetectorRef, ViewEncapsulation } from '@angular/core';
import { formatLabel } from '../label.helper';
var LegendComponent = /** @class */ (function () {
    function LegendComponent(cd) {
        this.cd = cd;
        this.horizontal = false;
        this.labelClick = new EventEmitter();
        this.labelActivate = new EventEmitter();
        this.labelDeactivate = new EventEmitter();
        this.legendEntries = [];
    }
    LegendComponent.prototype.ngOnChanges = function (changes) {
        this.update();
    };
    LegendComponent.prototype.update = function () {
        this.cd.markForCheck();
        this.legendEntries = this.getLegendEntries();
    };
    LegendComponent.prototype.getLegendEntries = function () {
        var e_1, _a;
        var items = [];
        var _loop_1 = function (label) {
            var formattedLabel = formatLabel(label);
            var idx = items.findIndex(function (i) {
                return i.label === formattedLabel;
            });
            if (idx === -1) {
                items.push({
                    label: label,
                    formattedLabel: formattedLabel,
                    color: this_1.colors.getColor(label)
                });
            }
        };
        var this_1 = this;
        try {
            for (var _b = __values(this.data), _c = _b.next(); !_c.done; _c = _b.next()) {
                var label = _c.value;
                _loop_1(label);
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return items;
    };
    LegendComponent.prototype.isActive = function (entry) {
        if (!this.activeEntries)
            return false;
        var item = this.activeEntries.find(function (d) {
            return entry.label === d.name;
        });
        return item !== undefined;
    };
    LegendComponent.prototype.activate = function (item) {
        this.labelActivate.emit(item);
    };
    LegendComponent.prototype.deactivate = function (item) {
        this.labelDeactivate.emit(item);
    };
    LegendComponent.prototype.trackBy = function (index, item) {
        return item.label;
    };
    LegendComponent.ctorParameters = function () { return [
        { type: ChangeDetectorRef }
    ]; };
    __decorate([
        Input()
    ], LegendComponent.prototype, "data", void 0);
    __decorate([
        Input()
    ], LegendComponent.prototype, "title", void 0);
    __decorate([
        Input()
    ], LegendComponent.prototype, "colors", void 0);
    __decorate([
        Input()
    ], LegendComponent.prototype, "height", void 0);
    __decorate([
        Input()
    ], LegendComponent.prototype, "width", void 0);
    __decorate([
        Input()
    ], LegendComponent.prototype, "activeEntries", void 0);
    __decorate([
        Input()
    ], LegendComponent.prototype, "horizontal", void 0);
    __decorate([
        Output()
    ], LegendComponent.prototype, "labelClick", void 0);
    __decorate([
        Output()
    ], LegendComponent.prototype, "labelActivate", void 0);
    __decorate([
        Output()
    ], LegendComponent.prototype, "labelDeactivate", void 0);
    LegendComponent = __decorate([
        Component({
            selector: 'ngx-charts-legend',
            template: "\n    <div [style.width.px]=\"width\">\n      <header class=\"legend-title\" *ngIf=\"title?.length > 0\">\n        <span class=\"legend-title-text\">{{ title }}</span>\n      </header>\n      <div class=\"legend-wrap\">\n        <ul class=\"legend-labels\" [class.horizontal-legend]=\"horizontal\" [style.max-height.px]=\"height - 45\">\n          <li *ngFor=\"let entry of legendEntries; trackBy: trackBy\" class=\"legend-label\">\n            <ngx-charts-legend-entry\n              [label]=\"entry.label\"\n              [formattedLabel]=\"entry.formattedLabel\"\n              [color]=\"entry.color\"\n              [isActive]=\"isActive(entry)\"\n              (select)=\"labelClick.emit($event)\"\n              (activate)=\"activate($event)\"\n              (deactivate)=\"deactivate($event)\"\n            >\n            </ngx-charts-legend-entry>\n          </li>\n        </ul>\n      </div>\n    </div>\n  ",
            encapsulation: ViewEncapsulation.None,
            changeDetection: ChangeDetectionStrategy.OnPush,
            styles: [".chart-legend{display:inline-block;padding:0;width:auto!important}.chart-legend .legend-title{white-space:nowrap;overflow:hidden;margin-left:10px;margin-bottom:5px;font-size:14px;font-weight:700}.chart-legend li,.chart-legend ul{padding:0;margin:0;list-style:none}.chart-legend .horizontal-legend li{display:inline-block}.chart-legend .legend-wrap{width:calc(100% - 10px)}.chart-legend .legend-labels{line-height:85%;list-style:none;text-align:left;float:left;width:100%;border-radius:3px;overflow-y:auto;overflow-x:hidden;white-space:nowrap;background:rgba(0,0,0,.05)}.chart-legend .legend-label{cursor:pointer;font-size:90%;margin:8px;color:#afb7c8}.chart-legend .legend-label:hover{color:#000;-webkit-transition:.2s;transition:.2s}.chart-legend .legend-label .active .legend-label-text{color:#000}.chart-legend .legend-label-color{display:inline-block;height:15px;width:15px;margin-right:5px;color:#5b646b;border-radius:3px}.chart-legend .legend-label-text{display:inline-block;vertical-align:top;line-height:15px;font-size:12px;width:calc(100% - 20px);text-overflow:ellipsis;white-space:nowrap;overflow:hidden}.chart-legend .legend-title-text{vertical-align:bottom;display:inline-block;line-height:16px;overflow:hidden;white-space:nowrap;text-overflow:ellipsis}"]
        })
    ], LegendComponent);
    return LegendComponent;
}());
export { LegendComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibGVnZW5kLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi9sZWdlbmQvbGVnZW5kLmNvbXBvbmVudC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUNMLFNBQVMsRUFDVCxLQUFLLEVBQ0wsdUJBQXVCLEVBQ3ZCLE1BQU0sRUFDTixZQUFZLEVBQ1osYUFBYSxFQUNiLFNBQVMsRUFDVCxpQkFBaUIsRUFDakIsaUJBQWlCLEVBQ2xCLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBRSxXQUFXLEVBQUUsTUFBTSxpQkFBaUIsQ0FBQztBQStCOUM7SUFlRSx5QkFBb0IsRUFBcUI7UUFBckIsT0FBRSxHQUFGLEVBQUUsQ0FBbUI7UUFSaEMsZUFBVSxHQUFHLEtBQUssQ0FBQztRQUVsQixlQUFVLEdBQXNCLElBQUksWUFBWSxFQUFFLENBQUM7UUFDbkQsa0JBQWEsR0FBc0IsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUN0RCxvQkFBZSxHQUFzQixJQUFJLFlBQVksRUFBRSxDQUFDO1FBRWxFLGtCQUFhLEdBQVUsRUFBRSxDQUFDO0lBRWtCLENBQUM7SUFFN0MscUNBQVcsR0FBWCxVQUFZLE9BQXNCO1FBQ2hDLElBQUksQ0FBQyxNQUFNLEVBQUUsQ0FBQztJQUNoQixDQUFDO0lBRUQsZ0NBQU0sR0FBTjtRQUNFLElBQUksQ0FBQyxFQUFFLENBQUMsWUFBWSxFQUFFLENBQUM7UUFDdkIsSUFBSSxDQUFDLGFBQWEsR0FBRyxJQUFJLENBQUMsZ0JBQWdCLEVBQUUsQ0FBQztJQUMvQyxDQUFDO0lBRUQsMENBQWdCLEdBQWhCOztRQUNFLElBQU0sS0FBSyxHQUFHLEVBQUUsQ0FBQztnQ0FFTixLQUFLO1lBQ2QsSUFBTSxjQUFjLEdBQUcsV0FBVyxDQUFDLEtBQUssQ0FBQyxDQUFDO1lBRTFDLElBQU0sR0FBRyxHQUFHLEtBQUssQ0FBQyxTQUFTLENBQUMsVUFBQSxDQUFDO2dCQUMzQixPQUFPLENBQUMsQ0FBQyxLQUFLLEtBQUssY0FBYyxDQUFDO1lBQ3BDLENBQUMsQ0FBQyxDQUFDO1lBRUgsSUFBSSxHQUFHLEtBQUssQ0FBQyxDQUFDLEVBQUU7Z0JBQ2QsS0FBSyxDQUFDLElBQUksQ0FBQztvQkFDVCxLQUFLLE9BQUE7b0JBQ0wsY0FBYyxnQkFBQTtvQkFDZCxLQUFLLEVBQUUsT0FBSyxNQUFNLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQztpQkFDbkMsQ0FBQyxDQUFDO2FBQ0o7Ozs7WUFiSCxLQUFvQixJQUFBLEtBQUEsU0FBQSxJQUFJLENBQUMsSUFBSSxDQUFBLGdCQUFBO2dCQUF4QixJQUFNLEtBQUssV0FBQTt3QkFBTCxLQUFLO2FBY2Y7Ozs7Ozs7OztRQUVELE9BQU8sS0FBSyxDQUFDO0lBQ2YsQ0FBQztJQUVELGtDQUFRLEdBQVIsVUFBUyxLQUFLO1FBQ1osSUFBSSxDQUFDLElBQUksQ0FBQyxhQUFhO1lBQUUsT0FBTyxLQUFLLENBQUM7UUFDdEMsSUFBTSxJQUFJLEdBQUcsSUFBSSxDQUFDLGFBQWEsQ0FBQyxJQUFJLENBQUMsVUFBQSxDQUFDO1lBQ3BDLE9BQU8sS0FBSyxDQUFDLEtBQUssS0FBSyxDQUFDLENBQUMsSUFBSSxDQUFDO1FBQ2hDLENBQUMsQ0FBQyxDQUFDO1FBQ0gsT0FBTyxJQUFJLEtBQUssU0FBUyxDQUFDO0lBQzVCLENBQUM7SUFFRCxrQ0FBUSxHQUFSLFVBQVMsSUFBSTtRQUNYLElBQUksQ0FBQyxhQUFhLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQ2hDLENBQUM7SUFFRCxvQ0FBVSxHQUFWLFVBQVcsSUFBSTtRQUNiLElBQUksQ0FBQyxlQUFlLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQ2xDLENBQUM7SUFFRCxpQ0FBTyxHQUFQLFVBQVEsS0FBSyxFQUFFLElBQUk7UUFDakIsT0FBTyxJQUFJLENBQUMsS0FBSyxDQUFDO0lBQ3BCLENBQUM7O2dCQW5EdUIsaUJBQWlCOztJQWRoQztRQUFSLEtBQUssRUFBRTtpREFBTTtJQUNMO1FBQVIsS0FBSyxFQUFFO2tEQUFPO0lBQ047UUFBUixLQUFLLEVBQUU7bURBQVE7SUFDUDtRQUFSLEtBQUssRUFBRTttREFBUTtJQUNQO1FBQVIsS0FBSyxFQUFFO2tEQUFPO0lBQ047UUFBUixLQUFLLEVBQUU7MERBQWU7SUFDZDtRQUFSLEtBQUssRUFBRTt1REFBb0I7SUFFbEI7UUFBVCxNQUFNLEVBQUU7dURBQW9EO0lBQ25EO1FBQVQsTUFBTSxFQUFFOzBEQUF1RDtJQUN0RDtRQUFULE1BQU0sRUFBRTs0REFBeUQ7SUFYdkQsZUFBZTtRQTdCM0IsU0FBUyxDQUFDO1lBQ1QsUUFBUSxFQUFFLG1CQUFtQjtZQUM3QixRQUFRLEVBQUUsdTVCQXNCVDtZQUVELGFBQWEsRUFBRSxpQkFBaUIsQ0FBQyxJQUFJO1lBQ3JDLGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNOztTQUNoRCxDQUFDO09BQ1csZUFBZSxDQW1FM0I7SUFBRCxzQkFBQztDQUFBLEFBbkVELElBbUVDO1NBbkVZLGVBQWUiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQge1xuICBDb21wb25lbnQsXG4gIElucHV0LFxuICBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSxcbiAgT3V0cHV0LFxuICBFdmVudEVtaXR0ZXIsXG4gIFNpbXBsZUNoYW5nZXMsXG4gIE9uQ2hhbmdlcyxcbiAgQ2hhbmdlRGV0ZWN0b3JSZWYsXG4gIFZpZXdFbmNhcHN1bGF0aW9uXG59IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHsgZm9ybWF0TGFiZWwgfSBmcm9tICcuLi9sYWJlbC5oZWxwZXInO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICduZ3gtY2hhcnRzLWxlZ2VuZCcsXG4gIHRlbXBsYXRlOiBgXG4gICAgPGRpdiBbc3R5bGUud2lkdGgucHhdPVwid2lkdGhcIj5cbiAgICAgIDxoZWFkZXIgY2xhc3M9XCJsZWdlbmQtdGl0bGVcIiAqbmdJZj1cInRpdGxlPy5sZW5ndGggPiAwXCI+XG4gICAgICAgIDxzcGFuIGNsYXNzPVwibGVnZW5kLXRpdGxlLXRleHRcIj57eyB0aXRsZSB9fTwvc3Bhbj5cbiAgICAgIDwvaGVhZGVyPlxuICAgICAgPGRpdiBjbGFzcz1cImxlZ2VuZC13cmFwXCI+XG4gICAgICAgIDx1bCBjbGFzcz1cImxlZ2VuZC1sYWJlbHNcIiBbY2xhc3MuaG9yaXpvbnRhbC1sZWdlbmRdPVwiaG9yaXpvbnRhbFwiIFtzdHlsZS5tYXgtaGVpZ2h0LnB4XT1cImhlaWdodCAtIDQ1XCI+XG4gICAgICAgICAgPGxpICpuZ0Zvcj1cImxldCBlbnRyeSBvZiBsZWdlbmRFbnRyaWVzOyB0cmFja0J5OiB0cmFja0J5XCIgY2xhc3M9XCJsZWdlbmQtbGFiZWxcIj5cbiAgICAgICAgICAgIDxuZ3gtY2hhcnRzLWxlZ2VuZC1lbnRyeVxuICAgICAgICAgICAgICBbbGFiZWxdPVwiZW50cnkubGFiZWxcIlxuICAgICAgICAgICAgICBbZm9ybWF0dGVkTGFiZWxdPVwiZW50cnkuZm9ybWF0dGVkTGFiZWxcIlxuICAgICAgICAgICAgICBbY29sb3JdPVwiZW50cnkuY29sb3JcIlxuICAgICAgICAgICAgICBbaXNBY3RpdmVdPVwiaXNBY3RpdmUoZW50cnkpXCJcbiAgICAgICAgICAgICAgKHNlbGVjdCk9XCJsYWJlbENsaWNrLmVtaXQoJGV2ZW50KVwiXG4gICAgICAgICAgICAgIChhY3RpdmF0ZSk9XCJhY3RpdmF0ZSgkZXZlbnQpXCJcbiAgICAgICAgICAgICAgKGRlYWN0aXZhdGUpPVwiZGVhY3RpdmF0ZSgkZXZlbnQpXCJcbiAgICAgICAgICAgID5cbiAgICAgICAgICAgIDwvbmd4LWNoYXJ0cy1sZWdlbmQtZW50cnk+XG4gICAgICAgICAgPC9saT5cbiAgICAgICAgPC91bD5cbiAgICAgIDwvZGl2PlxuICAgIDwvZGl2PlxuICBgLFxuICBzdHlsZVVybHM6IFsnLi9sZWdlbmQuY29tcG9uZW50LnNjc3MnXSxcbiAgZW5jYXBzdWxhdGlvbjogVmlld0VuY2Fwc3VsYXRpb24uTm9uZSxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2hcbn0pXG5leHBvcnQgY2xhc3MgTGVnZW5kQ29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgZGF0YTtcbiAgQElucHV0KCkgdGl0bGU7XG4gIEBJbnB1dCgpIGNvbG9ycztcbiAgQElucHV0KCkgaGVpZ2h0O1xuICBASW5wdXQoKSB3aWR0aDtcbiAgQElucHV0KCkgYWN0aXZlRW50cmllcztcbiAgQElucHV0KCkgaG9yaXpvbnRhbCA9IGZhbHNlO1xuXG4gIEBPdXRwdXQoKSBsYWJlbENsaWNrOiBFdmVudEVtaXR0ZXI8YW55PiA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcbiAgQE91dHB1dCgpIGxhYmVsQWN0aXZhdGU6IEV2ZW50RW1pdHRlcjxhbnk+ID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuICBAT3V0cHV0KCkgbGFiZWxEZWFjdGl2YXRlOiBFdmVudEVtaXR0ZXI8YW55PiA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcblxuICBsZWdlbmRFbnRyaWVzOiBhbnlbXSA9IFtdO1xuXG4gIGNvbnN0cnVjdG9yKHByaXZhdGUgY2Q6IENoYW5nZURldGVjdG9yUmVmKSB7fVxuXG4gIG5nT25DaGFuZ2VzKGNoYW5nZXM6IFNpbXBsZUNoYW5nZXMpOiB2b2lkIHtcbiAgICB0aGlzLnVwZGF0ZSgpO1xuICB9XG5cbiAgdXBkYXRlKCk6IHZvaWQge1xuICAgIHRoaXMuY2QubWFya0ZvckNoZWNrKCk7XG4gICAgdGhpcy5sZWdlbmRFbnRyaWVzID0gdGhpcy5nZXRMZWdlbmRFbnRyaWVzKCk7XG4gIH1cblxuICBnZXRMZWdlbmRFbnRyaWVzKCk6IGFueVtdIHtcbiAgICBjb25zdCBpdGVtcyA9IFtdO1xuXG4gICAgZm9yIChjb25zdCBsYWJlbCBvZiB0aGlzLmRhdGEpIHtcbiAgICAgIGNvbnN0IGZvcm1hdHRlZExhYmVsID0gZm9ybWF0TGFiZWwobGFiZWwpO1xuXG4gICAgICBjb25zdCBpZHggPSBpdGVtcy5maW5kSW5kZXgoaSA9PiB7XG4gICAgICAgIHJldHVybiBpLmxhYmVsID09PSBmb3JtYXR0ZWRMYWJlbDtcbiAgICAgIH0pO1xuXG4gICAgICBpZiAoaWR4ID09PSAtMSkge1xuICAgICAgICBpdGVtcy5wdXNoKHtcbiAgICAgICAgICBsYWJlbCxcbiAgICAgICAgICBmb3JtYXR0ZWRMYWJlbCxcbiAgICAgICAgICBjb2xvcjogdGhpcy5jb2xvcnMuZ2V0Q29sb3IobGFiZWwpXG4gICAgICAgIH0pO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiBpdGVtcztcbiAgfVxuXG4gIGlzQWN0aXZlKGVudHJ5KTogYm9vbGVhbiB7XG4gICAgaWYgKCF0aGlzLmFjdGl2ZUVudHJpZXMpIHJldHVybiBmYWxzZTtcbiAgICBjb25zdCBpdGVtID0gdGhpcy5hY3RpdmVFbnRyaWVzLmZpbmQoZCA9PiB7XG4gICAgICByZXR1cm4gZW50cnkubGFiZWwgPT09IGQubmFtZTtcbiAgICB9KTtcbiAgICByZXR1cm4gaXRlbSAhPT0gdW5kZWZpbmVkO1xuICB9XG5cbiAgYWN0aXZhdGUoaXRlbSkge1xuICAgIHRoaXMubGFiZWxBY3RpdmF0ZS5lbWl0KGl0ZW0pO1xuICB9XG5cbiAgZGVhY3RpdmF0ZShpdGVtKSB7XG4gICAgdGhpcy5sYWJlbERlYWN0aXZhdGUuZW1pdChpdGVtKTtcbiAgfVxuXG4gIHRyYWNrQnkoaW5kZXgsIGl0ZW0pOiBzdHJpbmcge1xuICAgIHJldHVybiBpdGVtLmxhYmVsO1xuICB9XG59XG4iXX0=