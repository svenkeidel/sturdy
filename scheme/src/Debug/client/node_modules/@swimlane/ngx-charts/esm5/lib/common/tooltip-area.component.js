import { __decorate, __values } from "tslib";
import { Component, Input, Output, EventEmitter, ViewChild, ChangeDetectionStrategy } from '@angular/core';
import { trigger, style, animate, transition } from '@angular/animations';
import { createMouseEvent } from '../events';
var TooltipArea = /** @class */ (function () {
    function TooltipArea() {
        this.anchorOpacity = 0;
        this.anchorPos = -1;
        this.anchorValues = [];
        this.showPercentage = false;
        this.tooltipDisabled = false;
        this.hover = new EventEmitter();
    }
    TooltipArea.prototype.getValues = function (xVal) {
        var e_1, _a;
        var results = [];
        try {
            for (var _b = __values(this.results), _c = _b.next(); !_c.done; _c = _b.next()) {
                var group = _c.value;
                var item = group.series.find(function (d) { return d.name.toString() === xVal.toString(); });
                var groupName = group.name;
                if (groupName instanceof Date) {
                    groupName = groupName.toLocaleDateString();
                }
                if (item) {
                    var label = item.name;
                    var val = item.value;
                    if (this.showPercentage) {
                        val = (item.d1 - item.d0).toFixed(2) + '%';
                    }
                    var color = void 0;
                    if (this.colors.scaleType === 'linear') {
                        var v = val;
                        if (item.d1) {
                            v = item.d1;
                        }
                        color = this.colors.getColor(v);
                    }
                    else {
                        color = this.colors.getColor(group.name);
                    }
                    var data = Object.assign({}, item, {
                        value: val,
                        name: label,
                        series: groupName,
                        min: item.min,
                        max: item.max,
                        color: color
                    });
                    results.push(data);
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return results;
    };
    TooltipArea.prototype.mouseMove = function (event) {
        var xPos = event.pageX - event.target.getBoundingClientRect().left;
        var closestIndex = this.findClosestPointIndex(xPos);
        var closestPoint = this.xSet[closestIndex];
        this.anchorPos = this.xScale(closestPoint);
        this.anchorPos = Math.max(0, this.anchorPos);
        this.anchorPos = Math.min(this.dims.width, this.anchorPos);
        this.anchorValues = this.getValues(closestPoint);
        if (this.anchorPos !== this.lastAnchorPos) {
            var ev = createMouseEvent('mouseleave');
            this.tooltipAnchor.nativeElement.dispatchEvent(ev);
            this.anchorOpacity = 0.7;
            this.hover.emit({
                value: closestPoint
            });
            this.showTooltip();
            this.lastAnchorPos = this.anchorPos;
        }
    };
    TooltipArea.prototype.findClosestPointIndex = function (xPos) {
        var minIndex = 0;
        var maxIndex = this.xSet.length - 1;
        var minDiff = Number.MAX_VALUE;
        var closestIndex = 0;
        while (minIndex <= maxIndex) {
            var currentIndex = ((minIndex + maxIndex) / 2) | 0;
            var currentElement = this.xScale(this.xSet[currentIndex]);
            var curDiff = Math.abs(currentElement - xPos);
            if (curDiff < minDiff) {
                minDiff = curDiff;
                closestIndex = currentIndex;
            }
            if (currentElement < xPos) {
                minIndex = currentIndex + 1;
            }
            else if (currentElement > xPos) {
                maxIndex = currentIndex - 1;
            }
            else {
                minDiff = 0;
                closestIndex = currentIndex;
                break;
            }
        }
        return closestIndex;
    };
    TooltipArea.prototype.showTooltip = function () {
        var event = createMouseEvent('mouseenter');
        this.tooltipAnchor.nativeElement.dispatchEvent(event);
    };
    TooltipArea.prototype.hideTooltip = function () {
        var event = createMouseEvent('mouseleave');
        this.tooltipAnchor.nativeElement.dispatchEvent(event);
        this.anchorOpacity = 0;
        this.lastAnchorPos = -1;
    };
    TooltipArea.prototype.getToolTipText = function (tooltipItem) {
        var result = '';
        if (tooltipItem.series !== undefined) {
            result += tooltipItem.series;
        }
        else {
            result += '???';
        }
        result += ': ';
        if (tooltipItem.value !== undefined) {
            result += tooltipItem.value.toLocaleString();
        }
        if (tooltipItem.min !== undefined || tooltipItem.max !== undefined) {
            result += ' (';
            if (tooltipItem.min !== undefined) {
                if (tooltipItem.max === undefined) {
                    result += '≥';
                }
                result += tooltipItem.min.toLocaleString();
                if (tooltipItem.max !== undefined) {
                    result += ' - ';
                }
            }
            else if (tooltipItem.max !== undefined) {
                result += '≤';
            }
            if (tooltipItem.max !== undefined) {
                result += tooltipItem.max.toLocaleString();
            }
            result += ')';
        }
        return result;
    };
    __decorate([
        Input()
    ], TooltipArea.prototype, "dims", void 0);
    __decorate([
        Input()
    ], TooltipArea.prototype, "xSet", void 0);
    __decorate([
        Input()
    ], TooltipArea.prototype, "xScale", void 0);
    __decorate([
        Input()
    ], TooltipArea.prototype, "yScale", void 0);
    __decorate([
        Input()
    ], TooltipArea.prototype, "results", void 0);
    __decorate([
        Input()
    ], TooltipArea.prototype, "colors", void 0);
    __decorate([
        Input()
    ], TooltipArea.prototype, "showPercentage", void 0);
    __decorate([
        Input()
    ], TooltipArea.prototype, "tooltipDisabled", void 0);
    __decorate([
        Input()
    ], TooltipArea.prototype, "tooltipTemplate", void 0);
    __decorate([
        Output()
    ], TooltipArea.prototype, "hover", void 0);
    __decorate([
        ViewChild('tooltipAnchor', { static: false })
    ], TooltipArea.prototype, "tooltipAnchor", void 0);
    TooltipArea = __decorate([
        Component({
            selector: 'g[ngx-charts-tooltip-area]',
            template: "\n    <svg:g>\n      <svg:rect\n        class=\"tooltip-area\"\n        [attr.x]=\"0\"\n        y=\"0\"\n        [attr.width]=\"dims.width\"\n        [attr.height]=\"dims.height\"\n        style=\"opacity: 0; cursor: 'auto';\"\n        (mousemove)=\"mouseMove($event)\"\n        (mouseleave)=\"hideTooltip()\"\n      />\n      <ng-template #defaultTooltipTemplate let-model=\"model\">\n        <xhtml:div class=\"area-tooltip-container\">\n          <xhtml:div *ngFor=\"let tooltipItem of model\" class=\"tooltip-item\">\n            <xhtml:span class=\"tooltip-item-color\" [style.background-color]=\"tooltipItem.color\"></xhtml:span>\n            {{ getToolTipText(tooltipItem) }}\n          </xhtml:div>\n        </xhtml:div>\n      </ng-template>\n      <svg:rect\n        #tooltipAnchor\n        [@animationState]=\"anchorOpacity !== 0 ? 'active' : 'inactive'\"\n        class=\"tooltip-anchor\"\n        [attr.x]=\"anchorPos\"\n        y=\"0\"\n        [attr.width]=\"1\"\n        [attr.height]=\"dims.height\"\n        [style.opacity]=\"anchorOpacity\"\n        [style.pointer-events]=\"'none'\"\n        ngx-tooltip\n        [tooltipDisabled]=\"tooltipDisabled\"\n        [tooltipPlacement]=\"'right'\"\n        [tooltipType]=\"'tooltip'\"\n        [tooltipSpacing]=\"15\"\n        [tooltipTemplate]=\"tooltipTemplate ? tooltipTemplate : defaultTooltipTemplate\"\n        [tooltipContext]=\"anchorValues\"\n        [tooltipImmediateExit]=\"true\"\n      />\n    </svg:g>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush,
            animations: [
                trigger('animationState', [
                    transition('inactive => active', [
                        style({
                            opacity: 0
                        }),
                        animate(250, style({ opacity: 0.7 }))
                    ]),
                    transition('active => inactive', [
                        style({
                            opacity: 0.7
                        }),
                        animate(250, style({ opacity: 0 }))
                    ])
                ])
            ]
        })
    ], TooltipArea);
    return TooltipArea;
}());
export { TooltipArea };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidG9vbHRpcC1hcmVhLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi90b29sdGlwLWFyZWEuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQUUsU0FBUyxFQUFFLEtBQUssRUFBRSxNQUFNLEVBQUUsWUFBWSxFQUFFLFNBQVMsRUFBRSx1QkFBdUIsRUFBZSxNQUFNLGVBQWUsQ0FBQztBQUN4SCxPQUFPLEVBQUUsT0FBTyxFQUFFLEtBQUssRUFBRSxPQUFPLEVBQUUsVUFBVSxFQUFFLE1BQU0scUJBQXFCLENBQUM7QUFDMUUsT0FBTyxFQUFFLGdCQUFnQixFQUFFLE1BQU0sV0FBVyxDQUFDO0FBK0Q3QztJQUFBO1FBQ0Usa0JBQWEsR0FBVyxDQUFDLENBQUM7UUFDMUIsY0FBUyxHQUFXLENBQUMsQ0FBQyxDQUFDO1FBQ3ZCLGlCQUFZLEdBQVUsRUFBRSxDQUFDO1FBU2hCLG1CQUFjLEdBQVksS0FBSyxDQUFDO1FBQ2hDLG9CQUFlLEdBQVksS0FBSyxDQUFDO1FBR2hDLFVBQUssR0FBRyxJQUFJLFlBQVksRUFBRSxDQUFDO0lBZ0p2QyxDQUFDO0lBNUlDLCtCQUFTLEdBQVQsVUFBVSxJQUFJOztRQUNaLElBQU0sT0FBTyxHQUFHLEVBQUUsQ0FBQzs7WUFFbkIsS0FBb0IsSUFBQSxLQUFBLFNBQUEsSUFBSSxDQUFDLE9BQU8sQ0FBQSxnQkFBQSw0QkFBRTtnQkFBN0IsSUFBTSxLQUFLLFdBQUE7Z0JBQ2QsSUFBTSxJQUFJLEdBQUcsS0FBSyxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBQSxDQUFDLElBQUksT0FBQSxDQUFDLENBQUMsSUFBSSxDQUFDLFFBQVEsRUFBRSxLQUFLLElBQUksQ0FBQyxRQUFRLEVBQUUsRUFBckMsQ0FBcUMsQ0FBQyxDQUFDO2dCQUMzRSxJQUFJLFNBQVMsR0FBRyxLQUFLLENBQUMsSUFBSSxDQUFD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QUFFLEdBQUcsRUFBRSxDQUFDLENBQUM7cUJBQ3RDLENBQUM7b0JBQ0YsVUFBVSxDQUFDLG9CQUFvQixFQUFFO3dCQUMvQixLQUFLLENBQUM7NEJBQ0osT0FBTyxFQUFFLEdBQUc7eUJBQ2IsQ0FBQzt3QkFDRixPQUFPLENBQUMsR0FBRyxFQUFFLEtBQUssQ0FBQyxFQUFFLE9BQU8sRUFBRSxDQUFDLEVBQUUsQ0FBQyxDQUFDO3FCQUNwQyxDQUFDO2lCQUNILENBQUM7YUFDSDtTQUNGLENBQUM7T0FDVyxXQUFXLENBZ0t2QjtJQUFELGtCQUFDO0NBQUEsQUFoS0QsSUFnS0M7U0FoS1ksV0FBVyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IENvbXBvbmVudCwgSW5wdXQsIE91dHB1dCwgRXZlbnRFbWl0dGVyLCBWaWV3Q2hpbGQsIENoYW5nZURldGVjdGlvblN0cmF0ZWd5LCBUZW1wbGF0ZVJlZiB9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHsgdHJpZ2dlciwgc3R5bGUsIGFuaW1hdGUsIHRyYW5zaXRpb24gfSBmcm9tICdAYW5ndWxhci9hbmltYXRpb25zJztcbmltcG9ydCB7IGNyZWF0ZU1vdXNlRXZlbnQgfSBmcm9tICcuLi9ldmVudHMnO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdnW25neC1jaGFydHMtdG9vbHRpcC1hcmVhXScsXG4gIHRlbXBsYXRlOiBgXG4gICAgPHN2ZzpnPlxuICAgICAgPHN2ZzpyZWN0XG4gICAgICAgIGNsYXNzPVwidG9vbHRpcC1hcmVhXCJcbiAgICAgICAgW2F0dHIueF09XCIwXCJcbiAgICAgICAgeT1cIjBcIlxuICAgICAgICBbYXR0ci53aWR0aF09XCJkaW1zLndpZHRoXCJcbiAgICAgICAgW2F0dHIuaGVpZ2h0XT1cImRpbXMuaGVpZ2h0XCJcbiAgICAgICAgc3R5bGU9XCJvcGFjaXR5OiAwOyBjdXJzb3I6ICdhdXRvJztcIlxuICAgICAgICAobW91c2Vtb3ZlKT1cIm1vdXNlTW92ZSgkZXZlbnQpXCJcbiAgICAgICAgKG1vdXNlbGVhdmUpPVwiaGlkZVRvb2x0aXAoKVwiXG4gICAgICAvPlxuICAgICAgPG5nLXRlbXBsYXRlICNkZWZhdWx0VG9vbHRpcFRlbXBsYXRlIGxldC1tb2RlbD1cIm1vZGVsXCI+XG4gICAgICAgIDx4aHRtbDpkaXYgY2xhc3M9XCJhcmVhLXRvb2x0aXAtY29udGFpbmVyXCI+XG4gICAgICAgICAgPHhodG1sOmRpdiAqbmdGb3I9XCJsZXQgdG9vbHRpcEl0ZW0gb2YgbW9kZWxcIiBjbGFzcz1cInRvb2x0aXAtaXRlbVwiPlxuICAgICAgICAgICAgPHhodG1sOnNwYW4gY2xhc3M9XCJ0b29sdGlwLWl0ZW0tY29sb3JcIiBbc3R5bGUuYmFja2dyb3VuZC1jb2xvcl09XCJ0b29sdGlwSXRlbS5jb2xvclwiPjwveGh0bWw6c3Bhbj5cbiAgICAgICAgICAgIHt7IGdldFRvb2xUaXBUZXh0KHRvb2x0aXBJdGVtKSB9fVxuICAgICAgICAgIDwveGh0bWw6ZGl2PlxuICAgICAgICA8L3hodG1sOmRpdj5cbiAgICAgIDwvbmctdGVtcGxhdGU+XG4gICAgICA8c3ZnOnJlY3RcbiAgICAgICAgI3Rvb2x0aXBBbmNob3JcbiAgICAgICAgW0BhbmltYXRpb25TdGF0ZV09XCJhbmNob3JPcGFjaXR5ICE9PSAwID8gJ2FjdGl2ZScgOiAnaW5hY3RpdmUnXCJcbiAgICAgICAgY2xhc3M9XCJ0b29sdGlwLWFuY2hvclwiXG4gICAgICAgIFthdHRyLnhdPVwiYW5jaG9yUG9zXCJcbiAgICAgICAgeT1cIjBcIlxuICAgICAgICBbYXR0ci53aWR0aF09XCIxXCJcbiAgICAgICAgW2F0dHIuaGVpZ2h0XT1cImRpbXMuaGVpZ2h0XCJcbiAgICAgICAgW3N0eWxlLm9wYWNpdHldPVwiYW5jaG9yT3BhY2l0eVwiXG4gICAgICAgIFtzdHlsZS5wb2ludGVyLWV2ZW50c109XCInbm9uZSdcIlxuICAgICAgICBuZ3gtdG9vbHRpcFxuICAgICAgICBbdG9vbHRpcERpc2FibGVkXT1cInRvb2x0aXBEaXNhYmxlZFwiXG4gICAgICAgIFt0b29sdGlwUGxhY2VtZW50XT1cIidyaWdodCdcIlxuICAgICAgICBbdG9vbHRpcFR5cGVdPVwiJ3Rvb2x0aXAnXCJcbiAgICAgICAgW3Rvb2x0aXBTcGFjaW5nXT1cIjE1XCJcbiAgICAgICAgW3Rvb2x0aXBUZW1wbGF0ZV09XCJ0b29sdGlwVGVtcGxhdGUgPyB0b29sdGlwVGVtcGxhdGUgOiBkZWZhdWx0VG9vbHRpcFRlbXBsYXRlXCJcbiAgICAgICAgW3Rvb2x0aXBDb250ZXh0XT1cImFuY2hvclZhbHVlc1wiXG4gICAgICAgIFt0b29sdGlwSW1tZWRpYXRlRXhpdF09XCJ0cnVlXCJcbiAgICAgIC8+XG4gICAgPC9zdmc6Zz5cbiAgYCxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2gsXG4gIGFuaW1hdGlvbnM6IFtcbiAgICB0cmlnZ2VyKCdhbmltYXRpb25TdGF0ZScsIFtcbiAgICAgIHRyYW5zaXRpb24oJ2luYWN0aXZlID0+IGFjdGl2ZScsIFtcbiAgICAgICAgc3R5bGUoe1xuICAgICAgICAgIG9wYWNpdHk6IDBcbiAgICAgICAgfSksXG4gICAgICAgIGFuaW1hdGUoMjUwLCBzdHlsZSh7IG9wYWNpdHk6IDAuNyB9KSlcbiAgICAgIF0pLFxuICAgICAgdHJhbnNpdGlvbignYWN0aXZlID0+IGluYWN0aXZlJywgW1xuICAgICAgICBzdHlsZSh7XG4gICAgICAgICAgb3BhY2l0eTogMC43XG4gICAgICAgIH0pLFxuICAgICAgICBhbmltYXRlKDI1MCwgc3R5bGUoeyBvcGFjaXR5OiAwIH0pKVxuICAgICAgXSlcbiAgICBdKVxuICBdXG59KVxuZXhwb3J0IGNsYXNzIFRvb2x0aXBBcmVhIHtcbiAgYW5jaG9yT3BhY2l0eTogbnVtYmVyID0gMDtcbiAgYW5jaG9yUG9zOiBudW1iZXIgPSAtMTtcbiAgYW5jaG9yVmFsdWVzOiBhbnlbXSA9IFtdO1xuICBsYXN0QW5jaG9yUG9zOiBudW1iZXI7XG5cbiAgQElucHV0KCkgZGltcztcbiAgQElucHV0KCkgeFNldDtcbiAgQElucHV0KCkgeFNjYWxlO1xuICBASW5wdXQoKSB5U2NhbGU7XG4gIEBJbnB1dCgpIHJlc3VsdHM7XG4gIEBJbnB1dCgpIGNvbG9ycztcbiAgQElucHV0KCkgc2hvd1BlcmNlbnRhZ2U6IGJvb2xlYW4gPSBmYWxzZTtcbiAgQElucHV0KCkgdG9vbHRpcERpc2FibGVkOiBib29sZWFuID0gZmFsc2U7XG4gIEBJbnB1dCgpIHRvb2x0aXBUZW1wbGF0ZTogVGVtcGxhdGVSZWY8YW55PjtcblxuICBAT3V0cHV0KCkgaG92ZXIgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgQFZpZXdDaGlsZCgndG9vbHRpcEFuY2hvcicsIHsgc3RhdGljOiBmYWxzZSB9KSB0b29sdGlwQW5jaG9yO1xuXG4gIGdldFZhbHVlcyh4VmFsKTogYW55W10ge1xuICAgIGNvbnN0IHJlc3VsdHMgPSBbXTtcblxuICAgIGZvciAoY29uc3QgZ3JvdXAgb2YgdGhpcy5yZXN1bHRzKSB7XG4gICAgICBjb25zdCBpdGVtID0gZ3JvdXAuc2VyaWVzLmZpbmQoZCA9PiBkLm5hbWUudG9TdHJpbmcoKSA9PT0geFZhbC50b1N0cmluZygpKTtcbiAgICAgIGxldCBncm91cE5hbWUgPSBncm91cC5uYW1lO1xuICAgICAgaWYgKGdyb3VwTmFtZSBpbnN0YW5jZW9mIERhdGUpIHtcbiAgICAgICAgZ3JvdXBOYW1lID0gZ3JvdXBOYW1lLnRvTG9jYWxlRGF0ZVN0cmluZygpO1xuICAgICAgfVxuXG4gICAgICBpZiAoaXRlbSkge1xuICAgICAgICBjb25zdCBsYWJlbCA9IGl0ZW0ubmFtZTtcbiAgICAgICAgbGV0IHZhbCA9IGl0ZW0udmFsdWU7XG4gICAgICAgIGlmICh0aGlzLnNob3dQZXJjZW50YWdlKSB7XG4gICAgICAgICAgdmFsID0gKGl0ZW0uZDEgLSBpdGVtLmQwKS50b0ZpeGVkKDIpICsgJyUnO1xuICAgICAgICB9XG4gICAgICAgIGxldCBjb2xvcjtcbiAgICAgICAgaWYgKHRoaXMuY29sb3JzLnNjYWxlVHlwZSA9PT0gJ2xpbmVhcicpIHtcbiAgICAgICAgICBsZXQgdiA9IHZhbDtcbiAgICAgICAgICBpZiAoaXRlbS5kMSkge1xuICAgICAgICAgICAgdiA9IGl0ZW0uZDE7XG4gICAgICAgICAgfVxuICAgICAgICAgIGNvbG9yID0gdGhpcy5jb2xvcnMuZ2V0Q29sb3Iodik7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgY29sb3IgPSB0aGlzLmNvbG9ycy5nZXRDb2xvcihncm91cC5uYW1lKTtcbiAgICAgICAgfVxuXG4gICAgICAgIGNvbnN0IGRhdGEgPSBPYmplY3QuYXNzaWduKHt9LCBpdGVtLCB7XG4gICAgICAgICAgdmFsdWU6IHZhbCxcbiAgICAgICAgICBuYW1lOiBsYWJlbCxcbiAgICAgICAgICBzZXJpZXM6IGdyb3VwTmFtZSxcbiAgICAgICAgICBtaW46IGl0ZW0ubWluLFxuICAgICAgICAgIG1heDogaXRlbS5tYXgsXG4gICAgICAgICAgY29sb3JcbiAgICAgICAgfSk7XG5cbiAgICAgICAgcmVzdWx0cy5wdXNoKGRhdGEpO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiByZXN1bHRzO1xuICB9XG5cbiAgbW91c2VNb3ZlKGV2ZW50KSB7XG4gICAgY29uc3QgeFBvcyA9IGV2ZW50LnBhZ2VYIC0gZXZlbnQudGFyZ2V0LmdldEJvdW5kaW5nQ2xpZW50UmVjdCgpLmxlZnQ7XG5cbiAgICBjb25zdCBjbG9zZXN0SW5kZXggPSB0aGlzLmZpbmRDbG9zZXN0UG9pbnRJbmRleCh4UG9zKTtcbiAgICBjb25zdCBjbG9zZXN0UG9pbnQgPSB0aGlzLnhTZXRbY2xvc2VzdEluZGV4XTtcbiAgICB0aGlzLmFuY2hvclBvcyA9IHRoaXMueFNjYWxlKGNsb3Nlc3RQb2ludCk7XG4gICAgdGhpcy5hbmNob3JQb3MgPSBNYXRoLm1heCgwLCB0aGlzLmFuY2hvclBvcyk7XG4gICAgdGhpcy5hbmNob3JQb3MgPSBNYXRoLm1pbih0aGlzLmRpbXMud2lkdGgsIHRoaXMuYW5jaG9yUG9zKTtcblxuICAgIHRoaXMuYW5jaG9yVmFsdWVzID0gdGhpcy5nZXRWYWx1ZXMoY2xvc2VzdFBvaW50KTtcbiAgICBpZiAodGhpcy5hbmNob3JQb3MgIT09IHRoaXMubGFzdEFuY2hvclBvcykge1xuICAgICAgY29uc3QgZXYgPSBjcmVhdGVNb3VzZUV2ZW50KCdtb3VzZWxlYXZlJyk7XG4gICAgICB0aGlzLnRvb2x0aXBBbmNob3IubmF0aXZlRWxlbWVudC5kaXNwYXRjaEV2ZW50KGV2KTtcbiAgICAgIHRoaXMuYW5jaG9yT3BhY2l0eSA9IDAuNztcbiAgICAgIHRoaXMuaG92ZXIuZW1pdCh7XG4gICAgICAgIHZhbHVlOiBjbG9zZXN0UG9pbnRcbiAgICAgIH0pO1xuICAgICAgdGhpcy5zaG93VG9vbHRpcCgpO1xuXG4gICAgICB0aGlzLmxhc3RBbmNob3JQb3MgPSB0aGlzLmFuY2hvclBvcztcbiAgICB9XG4gIH1cblxuICBmaW5kQ2xvc2VzdFBvaW50SW5kZXgoeFBvcykge1xuICAgIGxldCBtaW5JbmRleCA9IDA7XG4gICAgbGV0IG1heEluZGV4ID0gdGhpcy54U2V0Lmxlbmd0aCAtIDE7XG4gICAgbGV0IG1pbkRpZmYgPSBOdW1iZXIuTUFYX1ZBTFVFO1xuICAgIGxldCBjbG9zZXN0SW5kZXggPSAwO1xuXG4gICAgd2hpbGUgKG1pbkluZGV4IDw9IG1heEluZGV4KSB7XG4gICAgICBjb25zdCBjdXJyZW50SW5kZXggPSAoKG1pbkluZGV4ICsgbWF4SW5kZXgpIC8gMikgfCAwO1xuICAgICAgY29uc3QgY3VycmVudEVsZW1lbnQgPSB0aGlzLnhTY2FsZSh0aGlzLnhTZXRbY3VycmVudEluZGV4XSk7XG5cbiAgICAgIGNvbnN0IGN1ckRpZmYgPSBNYXRoLmFicyhjdXJyZW50RWxlbWVudCAtIHhQb3MpO1xuXG4gICAgICBpZiAoY3VyRGlmZiA8IG1pbkRpZmYpIHtcbiAgICAgICAgbWluRGlmZiA9IGN1ckRpZmY7XG4gICAgICAgIGNsb3Nlc3RJbmRleCA9IGN1cnJlbnRJbmRleDtcbiAgICAgIH1cblxuICAgICAgaWYgKGN1cnJlbnRFbGVtZW50IDwgeFBvcykge1xuICAgICAgICBtaW5JbmRleCA9IGN1cnJlbnRJbmRleCArIDE7XG4gICAgICB9IGVsc2UgaWYgKGN1cnJlbnRFbGVtZW50ID4geFBvcykge1xuICAgICAgICBtYXhJbmRleCA9IGN1cnJlbnRJbmRleCAtIDE7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICBtaW5EaWZmID0gMDtcbiAgICAgICAgY2xvc2VzdEluZGV4ID0gY3VycmVudEluZGV4O1xuICAgICAgICBicmVhaztcbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gY2xvc2VzdEluZGV4O1xuICB9XG5cbiAgc2hvd1Rvb2x0aXAoKTogdm9pZCB7XG4gICAgY29uc3QgZXZlbnQgPSBjcmVhdGVNb3VzZUV2ZW50KCdtb3VzZWVudGVyJyk7XG4gICAgdGhpcy50b29sdGlwQW5jaG9yLm5hdGl2ZUVsZW1lbnQuZGlzcGF0Y2hFdmVudChldmVudCk7XG4gIH1cblxuICBoaWRlVG9vbHRpcCgpOiB2b2lkIHtcbiAgICBjb25zdCBldmVudCA9IGNyZWF0ZU1vdXNlRXZlbnQoJ21vdXNlbGVhdmUnKTtcbiAgICB0aGlzLnRvb2x0aXBBbmNob3IubmF0aXZlRWxlbWVudC5kaXNwYXRjaEV2ZW50KGV2ZW50KTtcbiAgICB0aGlzLmFuY2hvck9wYWNpdHkgPSAwO1xuICAgIHRoaXMubGFzdEFuY2hvclBvcyA9IC0xO1xuICB9XG5cbiAgZ2V0VG9vbFRpcFRleHQodG9vbHRpcEl0ZW06IGFueSk6IHN0cmluZyB7XG4gICAgbGV0IHJlc3VsdDogc3RyaW5nID0gJyc7XG4gICAgaWYgKHRvb2x0aXBJdGVtLnNlcmllcyAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICByZXN1bHQgKz0gdG9vbHRpcEl0ZW0uc2VyaWVzO1xuICAgIH0gZWxzZSB7XG4gICAgICByZXN1bHQgKz0gJz8/Pyc7XG4gICAgfVxuICAgIHJlc3VsdCArPSAnOiAnO1xuICAgIGlmICh0b29sdGlwSXRlbS52YWx1ZSAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICByZXN1bHQgKz0gdG9vbHRpcEl0ZW0udmFsdWUudG9Mb2NhbGVTdHJpbmcoKTtcbiAgICB9XG4gICAgaWYgKHRvb2x0aXBJdGVtLm1pbiAhPT0gdW5kZWZpbmVkIHx8IHRvb2x0aXBJdGVtLm1heCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICByZXN1bHQgKz0gJyAoJztcbiAgICAgIGlmICh0b29sdGlwSXRlbS5taW4gIT09IHVuZGVmaW5lZCkge1xuICAgICAgICBpZiAodG9vbHRpcEl0ZW0ubWF4ID09PSB1bmRlZmluZWQpIHtcbiAgICAgICAgICByZXN1bHQgKz0gJ+KJpSc7XG4gICAgICAgIH1cbiAgICAgICAgcmVzdWx0ICs9IHRvb2x0aXBJdGVtLm1pbi50b0xvY2FsZVN0cmluZygpO1xuICAgICAgICBpZiAodG9vbHRpcEl0ZW0ubWF4ICE9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgICByZXN1bHQgKz0gJyAtICc7XG4gICAgICAgIH1cbiAgICAgIH0gZWxzZSBpZiAodG9vbHRpcEl0ZW0ubWF4ICE9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgcmVzdWx0ICs9ICfiiaQnO1xuICAgICAgfVxuICAgICAgaWYgKHRvb2x0aXBJdGVtLm1heCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICAgIHJlc3VsdCArPSB0b29sdGlwSXRlbS5tYXgudG9Mb2NhbGVTdHJpbmcoKTtcbiAgICAgIH1cbiAgICAgIHJlc3VsdCArPSAnKSc7XG4gICAgfVxuICAgIHJldHVybiByZXN1bHQ7XG4gIH1cbn1cbiJdfQ==