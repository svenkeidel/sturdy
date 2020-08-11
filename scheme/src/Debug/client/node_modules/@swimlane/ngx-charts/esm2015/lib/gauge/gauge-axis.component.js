import { __decorate } from "tslib";
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
import { line } from 'd3-shape';
let GaugeAxisComponent = class GaugeAxisComponent {
    constructor() {
        this.rotate = '';
    }
    ngOnChanges(changes) {
        this.update();
    }
    update() {
        this.rotationAngle = -90 + this.startAngle;
        this.rotate = `rotate(${this.rotationAngle})`;
        this.ticks = this.getTicks();
    }
    getTicks() {
        const bigTickSegment = this.angleSpan / this.bigSegments;
        const smallTickSegment = bigTickSegment / this.smallSegments;
        const tickLength = 20;
        const ticks = {
            big: [],
            small: []
        };
        const startDistance = this.radius + 10;
        const textDist = startDistance + tickLength + 10;
        for (let i = 0; i <= this.bigSegments; i++) {
            const angleDeg = i * bigTickSegment;
            const angle = (angleDeg * Math.PI) / 180;
            const textAnchor = this.getTextAnchor(angleDeg);
            let skip = false;
            if (i === 0 && this.angleSpan === 360) {
                skip = true;
            }
            if (!skip) {
                let text = Number.parseFloat(this.valueScale.invert(angleDeg).toString()).toLocaleString();
                if (this.tickFormatting) {
                    text = this.tickFormatting(text);
                }
                ticks.big.push({
                    line: this.getTickPath(startDistance, tickLength, angle),
                    textAnchor,
                    text,
                    textTransform: `
            translate(${textDist * Math.cos(angle)}, ${textDist * Math.sin(angle)}) rotate(${-this.rotationAngle})
          `
                });
            }
            if (i === this.bigSegments) {
                continue;
            }
            for (let j = 1; j <= this.smallSegments; j++) {
                const smallAngleDeg = angleDeg + j * smallTickSegment;
                const smallAngle = (smallAngleDeg * Math.PI) / 180;
                ticks.small.push({
                    line: this.getTickPath(startDistance, tickLength / 2, smallAngle)
                });
            }
        }
        return ticks;
    }
    getTextAnchor(angle) {
        // [0, 45] = 'middle';
        // [46, 135] = 'start';
        // [136, 225] = 'middle';
        // [226, 315] = 'end';
        angle = (this.startAngle + angle) % 360;
        let textAnchor = 'middle';
        if (angle > 45 && angle <= 135) {
            textAnchor = 'start';
        }
        else if (angle > 225 && angle <= 315) {
            textAnchor = 'end';
        }
        return textAnchor;
    }
    getTickPath(startDistance, tickLength, angle) {
        const y1 = startDistance * Math.sin(angle);
        const y2 = (startDistance + tickLength) * Math.sin(angle);
        const x1 = startDistance * Math.cos(angle);
        const x2 = (startDistance + tickLength) * Math.cos(angle);
        const points = [
            { x: x1, y: y1 },
            { x: x2, y: y2 }
        ];
        const lineGenerator = line()
            .x(d => d.x)
            .y(d => d.y);
        return lineGenerator(points);
    }
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
        template: `
    <svg:g [attr.transform]="rotate">
      <svg:g *ngFor="let tick of ticks.big" class="gauge-tick gauge-tick-large">
        <svg:path [attr.d]="tick.line" />
      </svg:g>
      <svg:g *ngFor="let tick of ticks.big" class="gauge-tick gauge-tick-large">
        <svg:text
          [style.textAnchor]="tick.textAnchor"
          [attr.transform]="tick.textTransform"
          alignment-baseline="central"
        >
          {{ tick.text }}
        </svg:text>
      </svg:g>
      <svg:g *ngFor="let tick of ticks.small" class="gauge-tick gauge-tick-small">
        <svg:path [attr.d]="tick.line" />
      </svg:g>
    </svg:g>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush
    })
], GaugeAxisComponent);
export { GaugeAxisComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZ2F1Z2UtYXhpcy5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9nYXVnZS9nYXVnZS1heGlzLmNvbXBvbmVudC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUFFLFNBQVMsRUFBRSxLQUFLLEVBQTRCLHVCQUF1QixFQUFFLE1BQU0sZUFBZSxDQUFDO0FBQ3BHLE9BQU8sRUFBRSxJQUFJLEVBQUUsTUFBTSxVQUFVLENBQUM7QUF5QmhDLElBQWEsa0JBQWtCLEdBQS9CLE1BQWEsa0JBQWtCO0lBQS9CO1FBYUUsV0FBTSxHQUFXLEVBQUUsQ0FBQztJQWtHdEIsQ0FBQztJQWhHQyxXQUFXLENBQUMsT0FBc0I7UUFDaEMsSUFBSSxDQUFDLE1BQU0sRUFBRSxDQUFDO0lBQ2hCLENBQUM7SUFFRCxNQUFNO1FBQ0osSUFBSSxDQUFDLGFBQWEsR0FBRyxDQUFDLEVBQUUsR0FBRyxJQUFJLENBQUMsVUFBVSxDQUFDO1FBQzNDLElBQUksQ0FBQyxNQUFNLEdBQUcsVUFBVSxJQUFJLENBQUMsYUFBYSxHQUFHLENBQUM7UUFDOUMsSUFBSSxDQUFDLEtBQUssR0FBRyxJQUFJLENBQUMsUUFBUSxFQUFFLENBQUM7SUFDL0IsQ0FBQztJQUVELFFBQVE7UUFDTixNQUFNLGNBQWMsR0FBRyxJQUFJLENBQUMsU0FBUyxHQUFHLElBQUksQ0FBQyxXQUFXLENBQUM7UUFDekQsTUFBTSxnQkFBZ0IsR0FBRyxjQUFjLEdBQUcsSUFBSSxDQUFDLGFBQWEsQ0FBQztRQUM3RCxNQUFNLFVBQVUsR0FBRyxFQUFFLENBQUM7UUFDdEIsTUFBTSxLQUFLLEdBQUc7WUFDWixHQUFHLEVBQUUsRUFBRTtZQUNQLEtBQUssRUFBRSxFQUFFO1NBQ1YsQ0FBQztRQUVGLE1BQU0sYUFBYSxHQUFHLElBQUksQ0FBQyxNQUFNLEdBQUcsRUFBRSxDQUFDO1FBQ3ZDLE1BQU0sUUFBUSxHQUFHLGFBQWEsR0FB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USxFQUFFOzs7Ozs7Ozs7Ozs7Ozs7Ozs7R0FrQlQ7UUFDRCxlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTtLQUNoRCxDQUFDO0dBQ1csa0JBQWtCLENBK0c5QjtTQS9HWSxrQkFBa0IiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgeyBDb21wb25lbnQsIElucHV0LCBPbkNoYW5nZXMsIFNpbXBsZUNoYW5nZXMsIENoYW5nZURldGVjdGlvblN0cmF0ZWd5IH0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBsaW5lIH0gZnJvbSAnZDMtc2hhcGUnO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdnW25neC1jaGFydHMtZ2F1Z2UtYXhpc10nLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxzdmc6ZyBbYXR0ci50cmFuc2Zvcm1dPVwicm90YXRlXCI+XG4gICAgICA8c3ZnOmcgKm5nRm9yPVwibGV0IHRpY2sgb2YgdGlja3MuYmlnXCIgY2xhc3M9XCJnYXVnZS10aWNrIGdhdWdlLXRpY2stbGFyZ2VcIj5cbiAgICAgICAgPHN2ZzpwYXRoIFthdHRyLmRdPVwidGljay5saW5lXCIgLz5cbiAgICAgIDwvc3ZnOmc+XG4gICAgICA8c3ZnOmcgKm5nRm9yPVwibGV0IHRpY2sgb2YgdGlja3MuYmlnXCIgY2xhc3M9XCJnYXVnZS10aWNrIGdhdWdlLXRpY2stbGFyZ2VcIj5cbiAgICAgICAgPHN2Zzp0ZXh0XG4gICAgICAgICAgW3N0eWxlLnRleHRBbmNob3JdPVwidGljay50ZXh0QW5jaG9yXCJcbiAgICAgICAgICBbYXR0ci50cmFuc2Zvcm1dPVwidGljay50ZXh0VHJhbnNmb3JtXCJcbiAgICAgICAgICBhbGlnbm1lbnQtYmFzZWxpbmU9XCJjZW50cmFsXCJcbiAgICAgICAgPlxuICAgICAgICAgIHt7IHRpY2sudGV4dCB9fVxuICAgICAgICA8L3N2Zzp0ZXh0PlxuICAgICAgPC9zdmc6Zz5cbiAgICAgIDxzdmc6ZyAqbmdGb3I9XCJsZXQgdGljayBvZiB0aWNrcy5zbWFsbFwiIGNsYXNzPVwiZ2F1Z2UtdGljayBnYXVnZS10aWNrLXNtYWxsXCI+XG4gICAgICAgIDxzdmc6cGF0aCBbYXR0ci5kXT1cInRpY2subGluZVwiIC8+XG4gICAgICA8L3N2ZzpnPlxuICAgIDwvc3ZnOmc+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIEdhdWdlQXhpc0NvbXBvbmVudCBpbXBsZW1lbnRzIE9uQ2hhbmdlcyB7XG4gIEBJbnB1dCgpIGJpZ1NlZ21lbnRzOiBhbnk7XG4gIEBJbnB1dCgpIHNtYWxsU2VnbWVudHM6IGFueTtcbiAgQElucHV0KCkgbWluOiBhbnk7XG4gIEBJbnB1dCgpIG1heDogYW55O1xuICBASW5wdXQoKSBhbmdsZVNwYW46IG51bWJlcjtcbiAgQElucHV0KCkgc3RhcnRBbmdsZTogbnVtYmVyO1xuICBASW5wdXQoKSByYWRpdXM6IGFueTtcbiAgQElucHV0KCkgdmFsdWVTY2FsZTogYW55O1xuICBASW5wdXQoKSB0aWNrRm9ybWF0dGluZzogYW55O1xuXG4gIHRpY2tzOiBhbnk7XG4gIHJvdGF0aW9uQW5nbGU6IG51bWJlcjtcbiAgcm90YXRlOiBzdHJpbmcgPSAnJztcblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKSB7XG4gICAgdGhpcy51cGRhdGUoKTtcbiAgfVxuXG4gIHVwZGF0ZSgpOiB2b2lkIHtcbiAgICB0aGlzLnJvdGF0aW9uQW5nbGUgPSAtOTAgKyB0aGlzLnN0YXJ0QW5nbGU7XG4gICAgdGhpcy5yb3RhdGUgPSBgcm90YXRlKCR7dGhpcy5yb3RhdGlvbkFuZ2xlfSlgO1xuICAgIHRoaXMudGlja3MgPSB0aGlzLmdldFRpY2tzKCk7XG4gIH1cblxuICBnZXRUaWNrcygpOiBhbnkge1xuICAgIGNvbnN0IGJpZ1RpY2tTZWdtZW50ID0gdGhpcy5hbmdsZVNwYW4gLyB0aGlzLmJpZ1NlZ21lbnRzO1xuICAgIGNvbnN0IHNtYWxsVGlja1NlZ21lbnQgPSBiaWdUaWNrU2VnbWVudCAvIHRoaXMuc21hbGxTZWdtZW50cztcbiAgICBjb25zdCB0aWNrTGVuZ3RoID0gMjA7XG4gICAgY29uc3QgdGlja3MgPSB7XG4gICAgICBiaWc6IFtdLFxuICAgICAgc21hbGw6IFtdXG4gICAgfTtcblxuICAgIGNvbnN0IHN0YXJ0RGlzdGFuY2UgPSB0aGlzLnJhZGl1cyArIDEwO1xuICAgIGNvbnN0IHRleHREaXN0ID0gc3RhcnREaXN0YW5jZSArIHRpY2tMZW5ndGggKyAxMDtcblxuICAgIGZvciAobGV0IGkgPSAwOyBpIDw9IHRoaXMuYmlnU2VnbWVudHM7IGkrKykge1xuICAgICAgY29uc3QgYW5nbGVEZWcgPSBpICogYmlnVGlja1NlZ21lbnQ7XG4gICAgICBjb25zdCBhbmdsZSA9IChhbmdsZURlZyAqIE1hdGguUEkpIC8gMTgwO1xuXG4gICAgICBjb25zdCB0ZXh0QW5jaG9yID0gdGhpcy5nZXRUZXh0QW5jaG9yKGFuZ2xlRGVnKTtcblxuICAgICAgbGV0IHNraXAgPSBmYWxzZTtcbiAgICAgIGlmIChpID09PSAwICYmIHRoaXMuYW5nbGVTcGFuID09PSAzNjApIHtcbiAgICAgICAgc2tpcCA9IHRydWU7XG4gICAgICB9XG5cbiAgICAgIGlmICghc2tpcCkge1xuICAgICAgICBsZXQgdGV4dCA9IE51bWJlci5wYXJzZUZsb2F0KHRoaXMudmFsdWVTY2FsZS5pbnZlcnQoYW5nbGVEZWcpLnRvU3RyaW5nKCkpLnRvTG9jYWxlU3RyaW5nKCk7XG4gICAgICAgIGlmICh0aGlzLnRpY2tGb3JtYXR0aW5nKSB7XG4gICAgICAgICAgdGV4dCA9IHRoaXMudGlja0Zvcm1hdHRpbmcodGV4dCk7XG4gICAgICAgIH1cbiAgICAgICAgdGlja3MuYmlnLnB1c2goe1xuICAgICAgICAgIGxpbmU6IHRoaXMuZ2V0VGlja1BhdGgoc3RhcnREaXN0YW5jZSwgdGlja0xlbmd0aCwgYW5nbGUpLFxuICAgICAgICAgIHRleHRBbmNob3IsXG4gICAgICAgICAgdGV4dCxcbiAgICAgICAgICB0ZXh0VHJhbnNmb3JtOiBgXG4gICAgICAgICAgICB0cmFuc2xhdGUoJHt0ZXh0RGlzdCAqIE1hdGguY29zKGFuZ2xlKX0sICR7dGV4dERpc3QgKiBNYXRoLnNpbihhbmdsZSl9KSByb3RhdGUoJHstdGhpcy5yb3RhdGlvbkFuZ2xlfSlcbiAgICAgICAgICBgXG4gICAgICAgIH0pO1xuICAgICAgfVxuXG4gICAgICBpZiAoaSA9PT0gdGhpcy5iaWdTZWdtZW50cykge1xuICAgICAgICBjb250aW51ZTtcbiAgICAgIH1cblxuICAgICAgZm9yIChsZXQgaiA9IDE7IGogPD0gdGhpcy5zbWFsbFNlZ21lbnRzOyBqKyspIHtcbiAgICAgICAgY29uc3Qgc21hbGxBbmdsZURlZyA9IGFuZ2xlRGVnICsgaiAqIHNtYWxsVGlja1NlZ21lbnQ7XG4gICAgICAgIGNvbnN0IHNtYWxsQW5nbGUgPSAoc21hbGxBbmdsZURlZyAqIE1hdGguUEkpIC8gMTgwO1xuXG4gICAgICAgIHRpY2tzLnNtYWxsLnB1c2goe1xuICAgICAgICAgIGxpbmU6IHRoaXMuZ2V0VGlja1BhdGgoc3RhcnREaXN0YW5jZSwgdGlja0xlbmd0aCAvIDIsIHNtYWxsQW5nbGUpXG4gICAgICAgIH0pO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiB0aWNrcztcbiAgfVxuXG4gIGdldFRleHRBbmNob3IoYW5nbGUpIHtcbiAgICAvLyBbMCwgNDVdID0gJ21pZGRsZSc7XG4gICAgLy8gWzQ2LCAxMzVdID0gJ3N0YXJ0JztcbiAgICAvLyBbMTM2LCAyMjVdID0gJ21pZGRsZSc7XG4gICAgLy8gWzIyNiwgMzE1XSA9ICdlbmQnO1xuXG4gICAgYW5nbGUgPSAodGhpcy5zdGFydEFuZ2xlICsgYW5nbGUpICUgMzYwO1xuICAgIGxldCB0ZXh0QW5jaG9yID0gJ21pZGRsZSc7XG4gICAgaWYgKGFuZ2xlID4gNDUgJiYgYW5nbGUgPD0gMTM1KSB7XG4gICAgICB0ZXh0QW5jaG9yID0gJ3N0YXJ0JztcbiAgICB9IGVsc2UgaWYgKGFuZ2xlID4gMjI1ICYmIGFuZ2xlIDw9IDMxNSkge1xuICAgICAgdGV4dEFuY2hvciA9ICdlbmQnO1xuICAgIH1cbiAgICByZXR1cm4gdGV4dEFuY2hvcjtcbiAgfVxuXG4gIGdldFRpY2tQYXRoKHN0YXJ0RGlzdGFuY2UsIHRpY2tMZW5ndGgsIGFuZ2xlKTogYW55IHtcbiAgICBjb25zdCB5MSA9IHN0YXJ0RGlzdGFuY2UgKiBNYXRoLnNpbihhbmdsZSk7XG4gICAgY29uc3QgeTIgPSAoc3RhcnREaXN0YW5jZSArIHRpY2tMZW5ndGgpICogTWF0aC5zaW4oYW5nbGUpO1xuICAgIGNvbnN0IHgxID0gc3RhcnREaXN0YW5jZSAqIE1hdGguY29zKGFuZ2xlKTtcbiAgICBjb25zdCB4MiA9IChzdGFydERpc3RhbmNlICsgdGlja0xlbmd0aCkgKiBNYXRoLmNvcyhhbmdsZSk7XG5cbiAgICBjb25zdCBwb2ludHMgPSBbXG4gICAgICB7IHg6IHgxLCB5OiB5MSB9LFxuICAgICAgeyB4OiB4MiwgeTogeTIgfVxuICAgIF07XG4gICAgY29uc3QgbGluZUdlbmVyYXRvciA9IGxpbmU8YW55PigpXG4gICAgICAueChkID0+IGQueClcbiAgICAgIC55KGQgPT4gZC55KTtcbiAgICByZXR1cm4gbGluZUdlbmVyYXRvcihwb2ludHMpO1xuICB9XG59XG4iXX0=