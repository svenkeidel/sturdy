import { __decorate } from "tslib";
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
import { line } from 'd3-shape';
var GaugeAxisComponent = /** @class */ (function () {
    function GaugeAxisComponent() {
        this.rotate = '';
    }
    GaugeAxisComponent.prototype.ngOnChanges = function (changes) {
        this.update();
    };
    GaugeAxisComponent.prototype.update = function () {
        this.rotationAngle = -90 + this.startAngle;
        this.rotate = "rotate(" + this.rotationAngle + ")";
        this.ticks = this.getTicks();
    };
    GaugeAxisComponent.prototype.getTicks = function () {
        var bigTickSegment = this.angleSpan / this.bigSegments;
        var smallTickSegment = bigTickSegment / this.smallSegments;
        var tickLength = 20;
        var ticks = {
            big: [],
            small: []
        };
        var startDistance = this.radius + 10;
        var textDist = startDistance + tickLength + 10;
        for (var i = 0; i <= this.bigSegments; i++) {
            var angleDeg = i * bigTickSegment;
            var angle = (angleDeg * Math.PI) / 180;
            var textAnchor = this.getTextAnchor(angleDeg);
            var skip = false;
            if (i === 0 && this.angleSpan === 360) {
                skip = true;
            }
            if (!skip) {
                var text = Number.parseFloat(this.valueScale.invert(angleDeg).toString()).toLocaleString();
                if (this.tickFormatting) {
                    text = this.tickFormatting(text);
                }
                ticks.big.push({
                    line: this.getTickPath(startDistance, tickLength, angle),
                    textAnchor: textAnchor,
                    text: text,
                    textTransform: "\n            translate(" + textDist * Math.cos(angle) + ", " + textDist * Math.sin(angle) + ") rotate(" + -this.rotationAngle + ")\n          "
                });
            }
            if (i === this.bigSegments) {
                continue;
            }
            for (var j = 1; j <= this.smallSegments; j++) {
                var smallAngleDeg = angleDeg + j * smallTickSegment;
                var smallAngle = (smallAngleDeg * Math.PI) / 180;
                ticks.small.push({
                    line: this.getTickPath(startDistance, tickLength / 2, smallAngle)
                });
            }
        }
        return ticks;
    };
    GaugeAxisComponent.prototype.getTextAnchor = function (angle) {
        // [0, 45] = 'middle';
        // [46, 135] = 'start';
        // [136, 225] = 'middle';
        // [226, 315] = 'end';
        angle = (this.startAngle + angle) % 360;
        var textAnchor = 'middle';
        if (angle > 45 && angle <= 135) {
            textAnchor = 'start';
        }
        else if (angle > 225 && angle <= 315) {
            textAnchor = 'end';
        }
        return textAnchor;
    };
    GaugeAxisComponent.prototype.getTickPath = function (startDistance, tickLength, angle) {
        var y1 = startDistance * Math.sin(angle);
        var y2 = (startDistance + tickLength) * Math.sin(angle);
        var x1 = startDistance * Math.cos(angle);
        var x2 = (startDistance + tickLength) * Math.cos(angle);
        var points = [
            { x: x1, y: y1 },
            { x: x2, y: y2 }
        ];
        var lineGenerator = line()
            .x(function (d) { return d.x; })
            .y(function (d) { return d.y; });
        return lineGenerator(points);
    };
    __decorate([
        Input()
    ], GaugeAxisComponent.prototype, "bigSegments", void 0);
    __decorate([
        Input()
    ], GaugeAxisComponent.prototype, "smallSegments", void 0);
    __decorate([
        Input()
    ], GaugeAxisComponent.prototype, "min", void 0);
    __decorate([
        Input()
    ], GaugeAxisComponent.prototype, "max", void 0);
    __decorate([
        Input()
    ], GaugeAxisComponent.prototype, "angleSpan", void 0);
    __decorate([
        Input()
    ], GaugeAxisComponent.prototype, "startAngle", void 0);
    __decorate([
        Input()
    ], GaugeAxisComponent.prototype, "radius", void 0);
    __decorate([
        Input()
    ], GaugeAxisComponent.prototype, "valueScale", void 0);
    __decorate([
        Input()
    ], GaugeAxisComponent.prototype, "tickFormatting", void 0);
    GaugeAxisComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-gauge-axis]',
            template: "\n    <svg:g [attr.transform]=\"rotate\">\n      <svg:g *ngFor=\"let tick of ticks.big\" class=\"gauge-tick gauge-tick-large\">\n        <svg:path [attr.d]=\"tick.line\" />\n      </svg:g>\n      <svg:g *ngFor=\"let tick of ticks.big\" class=\"gauge-tick gauge-tick-large\">\n        <svg:text\n          [style.textAnchor]=\"tick.textAnchor\"\n          [attr.transform]=\"tick.textTransform\"\n          alignment-baseline=\"central\"\n        >\n          {{ tick.text }}\n        </svg:text>\n      </svg:g>\n      <svg:g *ngFor=\"let tick of ticks.small\" class=\"gauge-tick gauge-tick-small\">\n        <svg:path [attr.d]=\"tick.line\" />\n      </svg:g>\n    </svg:g>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], GaugeAxisComponent);
    return GaugeAxisComponent;
}());
export { GaugeAxisComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZ2F1Z2UtYXhpcy5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9nYXVnZS9nYXVnZS1heGlzLmNvbXBvbmVudC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUFFLFNBQVMsRUFBRSxLQUFLLEVBQTRCLHVCQUF1QixFQUFFLE1BQU0sZUFBZSxDQUFDO0FBQ3BHLE9BQU8sRUFBRSxJQUFJLEVBQUUsTUFBTSxVQUFVLENBQUM7QUF5QmhDO0lBQUE7UUFhRSxXQUFNLEdBQVcsRUFBRSxDQUFDO0lBa0d0QixDQUFDO0lBaEdDLHdDQUFXLEdBQVgsVUFBWSxPQUFzQjtRQUNoQyxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUM7SUFDaEIsQ0FBQztJQUVELG1DQUFNLEdBQU47UUFDRSxJQUFJLENBQUMsYUFBYSxHQUFHLENBQUMsRUFBRSxHQUFHLElBQUksQ0FBQyxVQUFVLENBQUM7UUFDM0MsSUFBSSxDQUFDLE1BQU0sR0FBRyxZQUFVLElBQUksQ0FBQyxhQUFhLE1BQUcsQ0FBQztRQUM5QyxJQUFJLENBQUMsS0FBSyxHQUFHLElBQUksQ0FBQyxRQUFRLEVBQUUsQ0FBQztJQUMvQixDQUFDO0lBRUQscUNBQVEsR0FBUjtRQUNFLElBQU0sY0FBYyxHQUFHLElBQUksQ0FBQyxTQUFTLEdBQUcsSUFBSSxDQUFDLFdBQVcsQ0FBQztRQUN6RCxJQUFNLGdCQUFnQixHQUFHLGNBQWMsR0FBRyxJQUFJLENBQUMsYUFBYSxDQUFDO1FBQzdELElBQU0sVUFBVSxHQUFHLEVBQUUsQ0FBQztRQUN0QixJQUFNLEtBQUssR0FBRztZQUNaLEdBQUcsRUFBRSxFQUFFO1lBQ1AsS0FBSyxFQUFFLEVBQUU7U0FDVixDQUFDO1FBRUYsSUFBTSxhQUFhLEdBQUcsSUFBSSxDQUFDLE1BQU0sR0FBRyxFQUFFLENBQUM7UUFDdkMsSUFBTSxRQUFRLEdBQUcsYUFBYSxHQUFHLFVBQVUsR0FBRyxFQUFFLENBQUM7UUFFakQsS0FBSyxJQUFJLENBQUMsR0FBRyxDQUFDLEVBQUUsQ0FBQyxJQUFJLElBQUksQ0FBQyxXQUFXLEVBQUUsQ0FBQyxFQUFFLEVBQUU7WUFDMUMsSUFBTSxRQUFRLEdBQUcsQ0FBQyxHQUFHLGNBQWMsQ0FBQztZQUNwQyxJQUFNLEtBQUssR0FBRyxDQUFDLFFBQVEsR0FBRyxJQUFJLENBQUMsRUFBRSxDQUFDLEdBQUcsR0FBRyxDQUFDO1lBRXpDLElBQU0sVUFBVSxHQUFHLElBQUksQ0FBQyxhQUFhLENBQUMsUUFBUSxDQUFDLENBQUM7WUFFaEQsSUFBSSxJQUFJLEdBQUcsS0FBSyxDQUFDO1lBQ2pCLElBQUksQ0FBQyxLQUFLLENBQUMsSUFBSSxJQUFJLENBQUMsU0FBUyxLQUFLLEdBQUcsRUFBRTtnQkFDckMsSUFBSSxHQUFHLElBQUksQ0FBQzthQUNiO1lBRUQsSUFBSSxDQUFDLElBQUksRUFBRTtnQkFDVCxJQUFJLElBQUksR0FBRyxNQUFNLENBQUMsVUFBVSxDQUFDLElBQUksQ0FBQyxVQUFVLENBQUMsTUFBTSxDQUFDLFFBQVEsQ0FBQyxDQUFDLFFBQVEsRUFBRSxDQUFDLENBQUMsY0FBYyxFQUFFLENBQUM7Z0JBQzNGLElBQUksSUFBSSxDQUFDLGNBQWMsRUFBRTtvQkFDdkIsSUFBSSxHQUFHLElBQUksQ0FBQyxjQUFjLENBQUMsSUFBSSxDQUFDLENBQUM7aUJBQ2xDO2dCQUNELEtBQUssQ0FBQyxHQUFHLENBQUMsSUFBSSxDQUFDO29CQUNiLElBQUksRUFBRSxJQUFJLENBQUMsV0FBVyxDQUFDLGFBQWEsRUFBRSxVQUFVLEVBQUUsS0FBSyxDQUFDO29CQUN4RCxVQUFVLFlBQUE7b0JBQ1YsSUFBSSxNQUFBO29CQUNKLGFBQWEsRUFBRSw2QkFDRCxRQUFRLEdBQUcsSUFBSSxDQUFDLEdBQUcsQ0FBQyxLQUFLLENBQUMsVUFBSyxRQUFRLEdBQUcsSUFBSSxDQUFDLEdBQUcsQ0FBQyxLQUFLLENBQUMsaUJBQVksQ0FBQyxJQUFJLENBQUMsYUFBYSxrQkFDckc7aUJBQ0YsQ0FBQyxDQUFDO2FBQ0o7WUFFRCxJQUFJLENBQUMsS0FBSyxJQUFJLENBQUMsV0FBVyxFQUFFO2dCQUMxQixTQUFTO2FBQ1Y7WUFFRCxLQUFLLElBQUksQ0FBQyxHQUFHLENBQUMsRUFBRSxDQUFDLElBQUksSUFBSSxDQUFDLGFBQWEsRUFBRSxDQUFDLEVBQUUsRUFBRTtnQkFDNUMsSUFBTSxhQUFhLEdBQUcsUUFBUSxHQUFHLENBQUMsR0FBRyxnQkFBZ0IsQ0FBQztnQkFDdEQsSUFBTSxVQUFVLEdBQUcsQ0FBQyxhQUFhLEdBQUcsSUFBSSxDQUFDLEVBQUUsQ0FBQyxHQUFHLEdBQUcsQ0FBQztnQkFFbkQsS0FBSyxDQUFDLEtBQUssQ0FBQyxJQUFJLENBQUM7b0JBQ2YsSUFBSSxFQUFFLElBQUksQ0FBQyxXQUFXLENBQUMsYUFBYSxFQUFFLFVBQVUsR0FBRyxDQUFDLEVBQUUsVUFBVSxDQUFDO2lCQUNsRSxDQUFDLENBQUM7YUFDSjtTQUNGO1FBRUQsT0FBTyxLQUFLLENBQUM7SUFDZixDQUFDO0lBRUQsMENBQWEsR0FBYixVQUFjLEtBQUs7UUFDakIsc0JBQXNCO1FBQ3RCLHVCQUF1QjtRQUN2Qix5QkFBeUI7UUFDekIsc0JBQXNCO1FBRXRCLEtBQUssR0FBRyxDQUFDLElBQUksQ0FBQyxVQUFVLEdBQUcsS0FBSyxDQUFDLEdBQUcsR0FBRyxDQUFDO1FBQ3hDLElBQUksVUFBVSxHQUFHLFFBQVEsQ0FBQztRQUMxQixJQUFJLEtBQUssR0FBRyxFQUFFLElBQUksS0FBSyxJQUFJLEdBQUcsRUFBRTtZQUM5QixVQUFVLEdBQUcsT0FBTyxDQUFDO1NBQ3RCO2FBQU0sSUFBSSxLQUFLLEdBQUcsR0FBRyxJQUFJLEtBQUssSUFBSSxHQUFHLEVBQUU7WUFDdEMsVUFBVSxHQUFHLEtBQUssQ0FBQztTQUNwQjtRQUNELE9BQU8sVUFBVSxDQUFDO0lBQ3BCLENBQUM7SUFFRCx3Q0FBVyxHQUFYLFVBQVksYUFBYSxFQUFFLFVBQVUsRUFBRSxLQUFLO1FBQzFDLElBQU0sRUFBRSxHQUFHLGFBQWEsR0FBRyxJQUFJLENBQUMsR0FBRyxDQUFDLEtBQUssQ0FBQyxDQUFDO1FBQzNDLElBQU0sRUFBRSxHQUFHLENBQUMsYUFBYSxHQUFHLFVBQVUsQ0FBQyxHQUFHLElBQUksQ0FBQyxHQUFHLENBQUMsS0FBSyxDQUFDLENBQUM7UUFDMUQsSUFBTSxFQUFFLEdBQUcsYUFBYSxHQUFHLElBQUksQ0FBQyxHQUFHLENBQUMsS0FBSyxDQUFDLENBQUM7UUFDM0MsSUFBTSxFQUFFLEdBQUcsQ0FBQyxhQUFhLEdBQUcsVUFBVSxDQUFDLEdBQUcsSUFBSSxDQUFDLEdBQUcsQ0FBQyxLQUFLLENBQUMsQ0FBQztRQUUxRCxJQUFNLE1BQU0sR0FBRztZQUNiLEVBQUUsQ0FBQyxFQUFFLEVBQUUsRUFBRSxDQUFDLEVBQUUsRUFBRSxFQUFFO1lBQ2hCLEVBQUUsQ0FBQyxFQUFFLEVBQUUsRUFBRSxDQUFDLEVBQUUsRUFBRSxFQUFFO1NBQ2pCLENBQUM7UUFDRixJQUFNLGFBQWEsR0FBRyxJQUFJLEVBQU87YUFDOUIsQ0FBQyxDQUFDLFVBQUEsQ0FBQyxJQUFJLE9BQUEsQ0FBQyxDQUFDLENBQUMsRUFBSCxDQUFHLENBQUM7YUFDWCxDQUFDLENBQUMsVUFBQSxDQUFDLElBQUksT0FBQSxDQUFDLENBQUMsQ0FBQyxFQUFILENBQUcsQ0FBQyxDQUFDO1FBQ2YsT0FBTyxhQUFhLENBQUMsTUFBTSxDQUFDLENBQUM7SUFDL0IsQ0FBQztJQTdHUTtRQUFSLEtBQUssRUFBRTsyREFBa0I7SUFDakI7UUFBUixLQUFLLEVBQUU7NkRBQW9CO0lBQ25CO1FBQVIsS0FBSyxFQUFFO21EQUFVO0lBQ1Q7UUFBUixLQUFLLEVBQUU7bURBQVU7SUFDVDtRQUFSLEtBQUssRUFBRTt5REFBbUI7SUFDbEI7UUFBUixLQUFLLEVBQUU7MERBQW9CO0lBQ25CO1FBQVIsS0FBSyxFQUFFO3NEQUFhO0lBQ1o7UUFBUixLQUFLLEVBQUU7MERBQWlCO0lBQ2hCO1FBQVIsS0FBSyxFQUFFOzhEQUFxQjtJQVRsQixrQkFBa0I7UUF2QjlCLFNBQVMsQ0FBQztZQUNULFFBQVEsRUFBRSwwQkFBMEI7WUFDcEMsUUFBUSxFQUFFLHdxQkFrQlQ7WUFDRCxlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTtTQUNoRCxDQUFDO09BQ1csa0JBQWtCLENBK0c5QjtJQUFELHlCQUFDO0NBQUEsQUEvR0QsSUErR0M7U0EvR1ksa0JBQWtCIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgQ29tcG9uZW50LCBJbnB1dCwgT25DaGFuZ2VzLCBTaW1wbGVDaGFuZ2VzLCBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSB9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHsgbGluZSB9IGZyb20gJ2QzLXNoYXBlJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnZ1tuZ3gtY2hhcnRzLWdhdWdlLWF4aXNdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOmcgW2F0dHIudHJhbnNmb3JtXT1cInJvdGF0ZVwiPlxuICAgICAgPHN2ZzpnICpuZ0Zvcj1cImxldCB0aWNrIG9mIHRpY2tzLmJpZ1wiIGNsYXNzPVwiZ2F1Z2UtdGljayBnYXVnZS10aWNrLWxhcmdlXCI+XG4gICAgICAgIDxzdmc6cGF0aCBbYXR0ci5kXT1cInRpY2subGluZVwiIC8+XG4gICAgICA8L3N2ZzpnPlxuICAgICAgPHN2ZzpnICpuZ0Zvcj1cImxldCB0aWNrIG9mIHRpY2tzLmJpZ1wiIGNsYXNzPVwiZ2F1Z2UtdGljayBnYXVnZS10aWNrLWxhcmdlXCI+XG4gICAgICAgIDxzdmc6dGV4dFxuICAgICAgICAgIFtzdHlsZS50ZXh0QW5jaG9yXT1cInRpY2sudGV4dEFuY2hvclwiXG4gICAgICAgICAgW2F0dHIudHJhbnNmb3JtXT1cInRpY2sudGV4dFRyYW5zZm9ybVwiXG4gICAgICAgICAgYWxpZ25tZW50LWJhc2VsaW5lPVwiY2VudHJhbFwiXG4gICAgICAgID5cbiAgICAgICAgICB7eyB0aWNrLnRleHQgfX1cbiAgICAgICAgPC9zdmc6dGV4dD5cbiAgICAgIDwvc3ZnOmc+XG4gICAgICA8c3ZnOmcgKm5nRm9yPVwibGV0IHRpY2sgb2YgdGlja3Muc21hbGxcIiBjbGFzcz1cImdhdWdlLXRpY2sgZ2F1Z2UtdGljay1zbWFsbFwiPlxuICAgICAgICA8c3ZnOnBhdGggW2F0dHIuZF09XCJ0aWNrLmxpbmVcIiAvPlxuICAgICAgPC9zdmc6Zz5cbiAgICA8L3N2ZzpnPlxuICBgLFxuICBjaGFuZ2VEZXRlY3Rpb246IENoYW5nZURldGVjdGlvblN0cmF0ZWd5Lk9uUHVzaFxufSlcbmV4cG9ydCBjbGFzcyBHYXVnZUF4aXNDb21wb25lbnQgaW1wbGVtZW50cyBPbkNoYW5nZXMge1xuICBASW5wdXQoKSBiaWdTZWdtZW50czogYW55O1xuICBASW5wdXQoKSBzbWFsbFNlZ21lbnRzOiBhbnk7XG4gIEBJbnB1dCgpIG1pbjogYW55O1xuICBASW5wdXQoKSBtYXg6IGFueTtcbiAgQElucHV0KCkgYW5nbGVTcGFuOiBudW1iZXI7XG4gIEBJbnB1dCgpIHN0YXJ0QW5nbGU6IG51bWJlcjtcbiAgQElucHV0KCkgcmFkaXVzOiBhbnk7XG4gIEBJbnB1dCgpIHZhbHVlU2NhbGU6IGFueTtcbiAgQElucHV0KCkgdGlja0Zvcm1hdHRpbmc6IGFueTtcblxuICB0aWNrczogYW55O1xuICByb3RhdGlvbkFuZ2xlOiBudW1iZXI7XG4gIHJvdGF0ZTogc3RyaW5nID0gJyc7XG5cbiAgbmdPbkNoYW5nZXMoY2hhbmdlczogU2ltcGxlQ2hhbmdlcykge1xuICAgIHRoaXMudXBkYXRlKCk7XG4gIH1cblxuICB1cGRhdGUoKTogdm9pZCB7XG4gICAgdGhpcy5yb3RhdGlvbkFuZ2xlID0gLTkwICsgdGhpcy5zdGFydEFuZ2xlO1xuICAgIHRoaXMucm90YXRlID0gYHJvdGF0ZSgke3RoaXMucm90YXRpb25BbmdsZX0pYDtcbiAgICB0aGlzLnRpY2tzID0gdGhpcy5nZXRUaWNrcygpO1xuICB9XG5cbiAgZ2V0VGlja3MoKTogYW55IHtcbiAgICBjb25zdCBiaWdUaWNrU2VnbWVudCA9IHRoaXMuYW5nbGVTcGFuIC8gdGhpcy5iaWdTZWdtZW50cztcbiAgICBjb25zdCBzbWFsbFRpY2tTZWdtZW50ID0gYmlnVGlja1NlZ21lbnQgLyB0aGlzLnNtYWxsU2VnbWVudHM7XG4gICAgY29uc3QgdGlja0xlbmd0aCA9IDIwO1xuICAgIGNvbnN0IHRpY2tzID0ge1xuICAgICAgYmlnOiBbXSxcbiAgICAgIHNtYWxsOiBbXVxuICAgIH07XG5cbiAgICBjb25zdCBzdGFydERpc3RhbmNlID0gdGhpcy5yYWRpdXMgKyAxMDtcbiAgICBjb25zdCB0ZXh0RGlzdCA9IHN0YXJ0RGlzdGFuY2UgKyB0aWNrTGVuZ3RoICsgMTA7XG5cbiAgICBmb3IgKGxldCBpID0gMDsgaSA8PSB0aGlzLmJpZ1NlZ21lbnRzOyBpKyspIHtcbiAgICAgIGNvbnN0IGFuZ2xlRGVnID0gaSAqIGJpZ1RpY2tTZWdtZW50O1xuICAgICAgY29uc3QgYW5nbGUgPSAoYW5nbGVEZWcgKiBNYXRoLlBJKSAvIDE4MDtcblxuICAgICAgY29uc3QgdGV4dEFuY2hvciA9IHRoaXMuZ2V0VGV4dEFuY2hvcihhbmdsZURlZyk7XG5cbiAgICAgIGxldCBza2lwID0gZmFsc2U7XG4gICAgICBpZiAoaSA9PT0gMCAmJiB0aGlzLmFuZ2xlU3BhbiA9PT0gMzYwKSB7XG4gICAgICAgIHNraXAgPSB0cnVlO1xuICAgICAgfVxuXG4gICAgICBpZiAoIXNraXApIHtcbiAgICAgICAgbGV0IHRleHQgPSBOdW1iZXIucGFyc2VGbG9hdCh0aGlzLnZhbHVlU2NhbGUuaW52ZXJ0KGFuZ2xlRGVnKS50b1N0cmluZygpKS50b0xvY2FsZVN0cmluZygpO1xuICAgICAgICBpZiAodGhpcy50aWNrRm9ybWF0dGluZykge1xuICAgICAgICAgIHRleHQgPSB0aGlzLnRpY2tGb3JtYXR0aW5nKHRleHQpO1xuICAgICAgICB9XG4gICAgICAgIHRpY2tzLmJpZy5wdXNoKHtcbiAgICAgICAgICBsaW5lOiB0aGlzLmdldFRpY2tQYXRoKHN0YXJ0RGlzdGFuY2UsIHRpY2tMZW5ndGgsIGFuZ2xlKSxcbiAgICAgICAgICB0ZXh0QW5jaG9yLFxuICAgICAgICAgIHRleHQsXG4gICAgICAgICAgdGV4dFRyYW5zZm9ybTogYFxuICAgICAgICAgICAgdHJhbnNsYXRlKCR7dGV4dERpc3QgKiBNYXRoLmNvcyhhbmdsZSl9LCAke3RleHREaXN0ICogTWF0aC5zaW4oYW5nbGUpfSkgcm90YXRlKCR7LXRoaXMucm90YXRpb25BbmdsZX0pXG4gICAgICAgICAgYFxuICAgICAgICB9KTtcbiAgICAgIH1cblxuICAgICAgaWYgKGkgPT09IHRoaXMuYmlnU2VnbWVudHMpIHtcbiAgICAgICAgY29udGludWU7XG4gICAgICB9XG5cbiAgICAgIGZvciAobGV0IGogPSAxOyBqIDw9IHRoaXMuc21hbGxTZWdtZW50czsgaisrKSB7XG4gICAgICAgIGNvbnN0IHNtYWxsQW5nbGVEZWcgPSBhbmdsZURlZyArIGogKiBzbWFsbFRpY2tTZWdtZW50O1xuICAgICAgICBjb25zdCBzbWFsbEFuZ2xlID0gKHNtYWxsQW5nbGVEZWcgKiBNYXRoLlBJKSAvIDE4MDtcblxuICAgICAgICB0aWNrcy5zbWFsbC5wdXNoKHtcbiAgICAgICAgICBsaW5lOiB0aGlzLmdldFRpY2tQYXRoKHN0YXJ0RGlzdGFuY2UsIHRpY2tMZW5ndGggLyAyLCBzbWFsbEFuZ2xlKVxuICAgICAgICB9KTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gdGlja3M7XG4gIH1cblxuICBnZXRUZXh0QW5jaG9yKGFuZ2xlKSB7XG4gICAgLy8gWzAsIDQ1XSA9ICdtaWRkbGUnO1xuICAgIC8vIFs0NiwgMTM1XSA9ICdzdGFydCc7XG4gICAgLy8gWzEzNiwgMjI1XSA9ICdtaWRkbGUnO1xuICAgIC8vIFsyMjYsIDMxNV0gPSAnZW5kJztcblxuICAgIGFuZ2xlID0gKHRoaXMuc3RhcnRBbmdsZSArIGFuZ2xlKSAlIDM2MDtcbiAgICBsZXQgdGV4dEFuY2hvciA9ICdtaWRkbGUnO1xuICAgIGlmIChhbmdsZSA+IDQ1ICYmIGFuZ2xlIDw9IDEzNSkge1xuICAgICAgdGV4dEFuY2hvciA9ICdzdGFydCc7XG4gICAgfSBlbHNlIGlmIChhbmdsZSA+IDIyNSAmJiBhbmdsZSA8PSAzMTUpIHtcbiAgICAgIHRleHRBbmNob3IgPSAnZW5kJztcbiAgICB9XG4gICAgcmV0dXJuIHRleHRBbmNob3I7XG4gIH1cblxuICBnZXRUaWNrUGF0aChzdGFydERpc3RhbmNlLCB0aWNrTGVuZ3RoLCBhbmdsZSk6IGFueSB7XG4gICAgY29uc3QgeTEgPSBzdGFydERpc3RhbmNlICogTWF0aC5zaW4oYW5nbGUpO1xuICAgIGNvbnN0IHkyID0gKHN0YXJ0RGlzdGFuY2UgKyB0aWNrTGVuZ3RoKSAqIE1hdGguc2luKGFuZ2xlKTtcbiAgICBjb25zdCB4MSA9IHN0YXJ0RGlzdGFuY2UgKiBNYXRoLmNvcyhhbmdsZSk7XG4gICAgY29uc3QgeDIgPSAoc3RhcnREaXN0YW5jZSArIHRpY2tMZW5ndGgpICogTWF0aC5jb3MoYW5nbGUpO1xuXG4gICAgY29uc3QgcG9pbnRzID0gW1xuICAgICAgeyB4OiB4MSwgeTogeTEgfSxcbiAgICAgIHsgeDogeDIsIHk6IHkyIH1cbiAgICBdO1xuICAgIGNvbnN0IGxpbmVHZW5lcmF0b3IgPSBsaW5lPGFueT4oKVxuICAgICAgLngoZCA9PiBkLngpXG4gICAgICAueShkID0+IGQueSk7XG4gICAgcmV0dXJuIGxpbmVHZW5lcmF0b3IocG9pbnRzKTtcbiAgfVxufVxuIl19