import { __decorate } from "tslib";
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
import { arc } from 'd3-shape';
import { trimLabel } from '../common/trim-label.helper';
let PieLabelComponent = class PieLabelComponent {
    constructor() {
        this.animations = true;
        this.labelTrim = true;
        this.labelTrimSize = 10;
        this.isIE = /(edge|msie|trident)/i.test(navigator.userAgent);
        this.trimLabel = trimLabel;
    }
    ngOnChanges(changes) {
        this.update();
    }
    update() {
        let startRadius = this.radius;
        if (this.explodeSlices) {
            startRadius = (this.radius * this.value) / this.max;
        }
        const innerArc = arc()
            .innerRadius(startRadius)
            .outerRadius(startRadius);
        // Calculate innerPos then scale outer position to match label position
        const innerPos = innerArc.centroid(this.data);
        let scale = this.data.pos[1] / innerPos[1];
        if (this.data.pos[1] === 0 || innerPos[1] === 0) {
            scale = 1;
        }
        const outerPos = [scale * innerPos[0], scale * innerPos[1]];
        this.line = `M${innerPos}L${outerPos}L${this.data.pos}`;
    }
    get textX() {
        return this.data.pos[0];
    }
    get textY() {
        return this.data.pos[1];
    }
    get styleTransform() {
        return this.isIE ? null : `translate3d(${this.textX}px,${this.textY}px, 0)`;
    }
    get attrTransform() {
        return !this.isIE ? null : `translate(${this.textX},${this.textY})`;
    }
    get textTransition() {
        return this.isIE || !this.animations ? null : 'transform 0.75s';
    }
    textAnchor() {
        return this.midAngle(this.data) < Math.PI ? 'start' : 'end';
    }
    midAngle(d) {
        return d.startAngle + (d.endAngle - d.startAngle) / 2;
    }
};
__decorate([
    Input()
], PieLabelComponent.prototype, "data", void 0);
__decorate([
    Input()
], PieLabelComponent.prototype, "radius", void 0);
__decorate([
    Input()
], PieLabelComponent.prototype, "label", void 0);
__decorate([
    Input()
], PieLabelComponent.prototype, "color", void 0);
__decorate([
    Input()
], PieLabelComponent.prototype, "max", void 0);
__decorate([
    Input()
], PieLabelComponent.prototype, "value", void 0);
__decorate([
    Input()
], PieLabelComponent.prototype, "explodeSlices", void 0);
__decorate([
    Input()
], PieLabelComponent.prototype, "animations", void 0);
__decorate([
    Input()
], PieLabelComponent.prototype, "labelTrim", void 0);
__decorate([
    Input()
], PieLabelComponent.prototype, "labelTrimSize", void 0);
PieLabelComponent = __decorate([
    Component({
        selector: 'g[ngx-charts-pie-label]',
        template: `
    <title>{{ label }}</title>
    <svg:g [attr.transform]="attrTransform" [style.transform]="styleTransform" [style.transition]="textTransition">
      <svg:text
        class="pie-label"
        [class.animation]="animations"
        dy=".35em"
        [style.textAnchor]="textAnchor()"
        [style.shapeRendering]="'crispEdges'"
      >
        {{ labelTrim ? trimLabel(label, labelTrimSize) : label }}
      </svg:text>
    </svg:g>
    <svg:path
      [attr.d]="line"
      [attr.stroke]="color"
      fill="none"
      class="pie-label-line line"
      [class.animation]="animations"
    ></svg:path>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush
    })
], PieLabelComponent);
export { PieLabelComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGllLWxhYmVsLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL3BpZS1jaGFydC9waWUtbGFiZWwuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQUUsU0FBUyxFQUFFLEtBQUssRUFBNEIsdUJBQXVCLEVBQUUsTUFBTSxlQUFlLENBQUM7QUFDcEcsT0FBTyxFQUFFLEdBQUcsRUFBRSxNQUFNLFVBQVUsQ0FBQztBQUUvQixPQUFPLEVBQUUsU0FBUyxFQUFFLE1BQU0sNkJBQTZCLENBQUM7QUEyQnhELElBQWEsaUJBQWlCLEdBQTlCLE1BQWEsaUJBQWlCO0lBaUI1QjtRQVRTLGVBQVUsR0FBWSxJQUFJLENBQUM7UUFDM0IsY0FBUyxHQUFZLElBQUksQ0FBQztRQUMxQixrQkFBYSxHQUFXLEVBQUUsQ0FBQztRQUtuQixTQUFJLEdBQUcsc0JBQXNCLENBQUMsSUFBSSxDQUFDLFNBQVMsQ0FBQyxTQUFTLENBQUMsQ0FBQztRQUd2RSxJQUFJLENBQUMsU0FBUyxHQUFHLFNBQVMsQ0FBQztJQUM3QixDQUFDO0lBRUQsV0FBVyxDQUFDLE9BQXNCO1FBQ2hDLElBQUksQ0FBQyxNQUFNLEVBQUUsQ0FBQztJQUNoQixDQUFDO0lBRUQsTUFBTTtRQUNKLElBQUksV0FBVyxHQUFHLElBQUksQ0FBQyxNQUFNLENBQUM7UUFDOUIsSUFBSSxJQUFJLENBQUMsYUFBYSxFQUFFO1lBQ3RCLFdBQVcsR0FBRyxDQUFDLElBQUksQ0FBQyxNQUFNLEdBQUcsSUFBSSxDQUFDLEtBQUssQ0FBQyxHQUFHLElBQUksQ0FBQyxHQUFHLENBQUM7U0FDckQ7UUFFRCxNQUFNLFFBQVEsR0FBRyxHQUFHLEVBQUU7YUFDbkIsV0FBVyxDQUFDLFdBQVcsQ0FBQzthQUN4QixXQUFXLENBQUMsV0FBVyxDQUFDLENBQUM7UUFFNUIsdUVBQXVFO1FBQ3ZFLE1BQU0sUUFBUSxHQUFHLFFBQVEsQ0FBQyxRQUFRLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO1FBRTlDLElBQUksS0FBSyxHQUFHLElBQUksQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLENBQUMsQ0FBQyxHQUFHLFFBQVEsQ0FBQyxDQUFDLENBQUMsQ0FBQztRQUMzQyxJQUFJLElBQUksQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLENBQUMsQ0FBQyxLQUFLLENBQUMsSUFBSSxRQUFRLENBQUMsQ0FBQyxDQUFDLEtBQUssQ0FBQyxFQUFFO1lBQy9DLEtBQUssR0FBRyxDQUFDLENBQUM7U0FDWDtRQUNELE1BQU0sUUFBUSxHQUFHLENBQUMsS0FBSyxHQUFHLFFBQVEsQ0FBQyxDQUFDLENBQUMsRUFBRSxLQUFLLEdBQUcsUUFBUSxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUM7UUFFNUQsSUFBSSxDQUFDLElBQUksR0FBRyxJQUFJLFFBQVEsSUFBSSxRQUFRLElBQUksSUFBSSxDQUFDLElBQUksQ0FBQyxHQUFHLEVBQUUsQ0FBQztJQUMxRCxDQUFDO0lBRUQsSUFBSSxLQUFLO1FBQ1AsT0FBTyxJQUFJLENBQUMsSUFBSSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsQ0FBQztJQUMxQixDQUFDO0lBRUQsSUFBSSxLQUFLO1FBQ1AsT0FBTyxJQUFJLENBQUMsSUFBSSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsQ0FBQztJQUMxQixDQUFDO0lBRUQsSUFBSSxjQUFjO1FBQ2hCLE9BQU8sSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQyxlQUFlLElBQUksQ0FBQyxLQUFLLE1BQU0sSUFBSSxDQUFDLEtBQUssUUFBUSxDQUFDO0lBQzlFLENBQUM7SUFFRCxJQUFJLGFBQWE7UUFDZixPQUFPLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQyxhQUFhLElBQUksQ0FBQyxLQUFLLElBQUksSUFBSSxDQUFDLEtBQUssR0FBRyxDQUFDO0lBQ3RFLENBQUM7SUFFRCxJQUFJLGNBQWM7UUFDaEIsT0FBTyxJQUFJLENBQUMsSUFBSSxJQUFJLENBQUMsSUFBSSxDQUFDLFVBQVUsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQyxpQkFBaUIsQ0FBQztJQUNsRSxDQUFDO0lBRUQsVUFBVTtRQUNSLE9BQU8sSUFBSSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLEdBQUcsSUFBSSxDQUFDLEVBQUUsQ0FBQyxDQUFDLENBQUMsT0FBTyxDQUFDLENBQUMsQ0FBQyxLQUFLLENBQUM7SUFDOUQsQ0FBQztJQUVELFFBQVEsQ0FBQyxDQUFDO1FBQ1IsT0FBTyxDQUFDLENBQUMsVUFBVSxHQUFHLENBQUMsQ0FBQyxDQUFDLFFBQVEsR0FBRyxDQUFDLENBQUMsVUFBVSxDQUFDLEdBQUcsQ0FBQyxDQUFDO0lBQ3hELENBQUM7Q0FDRixDQUFBO0FBekVVO0lBQVIsS0FBSyxFQUFFOytDQUFNO0FBQ0w7SUFBUixLQUFLLEVBQUU7aURBQVE7QUFDUDtJQUFSLEtBQUssRUFBRTtnREFBTztBQUNOO0lBQVIsS0FBSyxFQUFFO2dEQUFPO0FBQ047SUFBUixLQUFLLEVBQUU7OENBQUs7QUFDSjtJQUFSLEtBQUssRUFBRTtnREFBTztBQUNOO0lBQVIsS0FBSyxFQUFFO3dEQUFlO0FBQ2Q7SUFBUixLQUFLLEVBQUU7cURBQTRCO0FBQzNCO0lBQVIsS0FBSyxFQUFFO29EQUEyQjtBQUMxQjtJQUFSLEtBQUssRUFBRTt3REFBNEI7QUFWekIsaUJBQWlCO0lBekI3QixTQUFTLENBQUM7UUFDVCxRQUFRLEVBQUUseUJBQXlCO1FBQ25DLFFBQVEsRUFBRTs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7R0FvQlQ7UUFDRCxlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTtLQUNoRCxDQUFDO0dBQ1csaUJBQWlCLENBMEU3QjtTQTFFWSxpQkFBaUIiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgeyBDb21wb25lbnQsIElucHV0LCBPbkNoYW5nZXMsIFNpbXBsZUNoYW5nZXMsIENoYW5nZURldGVjdGlvblN0cmF0ZWd5IH0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBhcmMgfSBmcm9tICdkMy1zaGFwZSc7XG5cbmltcG9ydCB7IHRyaW1MYWJlbCB9IGZyb20gJy4uL2NvbW1vbi90cmltLWxhYmVsLmhlbHBlcic7XG5cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ2dbbmd4LWNoYXJ0cy1waWUtbGFiZWxdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8dGl0bGU+e3sgbGFiZWwgfX08L3RpdGxlPlxuICAgIDxzdmc6ZyBbYXR0ci50cmFuc2Zvcm1dPVwiYXR0clRyYW5zZm9ybVwiIFtzdHlsZS50cmFuc2Zvcm1dPVwic3R5bGVUcmFuc2Zvcm1cIiBbc3R5bGUudHJhbnNpdGlvbl09XCJ0ZXh0VHJhbnNpdGlvblwiPlxuICAgICAgPHN2Zzp0ZXh0XG4gICAgICAgIGNsYXNzPVwicGllLWxhYmVsXCJcbiAgICAgICAgW2NsYXNzLmFuaW1hdGlvbl09XCJhbmltYXRpb25zXCJcbiAgICAgICAgZHk9XCIuMzVlbVwiXG4gICAgICAgIFtzdHlsZS50ZXh0QW5jaG9yXT1cInRleHRBbmNob3IoKVwiXG4gICAgICAgIFtzdHlsZS5zaGFwZVJlbmRlcmluZ109XCInY3Jpc3BFZGdlcydcIlxuICAgICAgPlxuICAgICAgICB7eyBsYWJlbFRyaW0gPyB0cmltTGFiZWwobGFiZWwsIGxhYmVsVHJpbVNpemUpIDogbGFiZWwgfX1cbiAgICAgIDwvc3ZnOnRleHQ+XG4gICAgPC9zdmc6Zz5cbiAgICA8c3ZnOnBhdGhcbiAgICAgIFthdHRyLmRdPVwibGluZVwiXG4gICAgICBbYXR0ci5zdHJva2VdPVwiY29sb3JcIlxuICAgICAgZmlsbD1cIm5vbmVcIlxuICAgICAgY2xhc3M9XCJwaWUtbGFiZWwtbGluZSBsaW5lXCJcbiAgICAgIFtjbGFzcy5hbmltYXRpb25dPVwiYW5pbWF0aW9uc1wiXG4gICAgPjwvc3ZnOnBhdGg+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIFBpZUxhYmVsQ29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgZGF0YTtcbiAgQElucHV0KCkgcmFkaXVzO1xuICBASW5wdXQoKSBsYWJlbDtcbiAgQElucHV0KCkgY29sb3I7XG4gIEBJbnB1dCgpIG1heDtcbiAgQElucHV0KCkgdmFsdWU7XG4gIEBJbnB1dCgpIGV4cGxvZGVTbGljZXM7XG4gIEBJbnB1dCgpIGFuaW1hdGlvbnM6IGJvb2xlYW4gPSB0cnVlO1xuICBASW5wdXQoKSBsYWJlbFRyaW06IGJvb2xlYW4gPSB0cnVlO1xuICBASW5wdXQoKSBsYWJlbFRyaW1TaXplOiBudW1iZXIgPSAxMDtcblxuICB0cmltTGFiZWw6IChsYWJlbDogc3RyaW5nLCBtYXg/OiBudW1iZXIpID0+IHN0cmluZztcbiAgbGluZTogc3RyaW5nO1xuXG4gIHByaXZhdGUgcmVhZG9ubHkgaXNJRSA9IC8oZWRnZXxtc2llfHRyaWRlbnQpL2kudGVzdChuYXZpZ2F0b3IudXNlckFnZW50KTtcblxuICBjb25zdHJ1Y3RvcigpIHtcbiAgICB0aGlzLnRyaW1MYWJlbCA9IHRyaW1MYWJlbDtcbiAgfVxuXG4gIG5nT25DaGFuZ2VzKGNoYW5nZXM6IFNpbXBsZUNoYW5nZXMpOiB2b2lkIHtcbiAgICB0aGlzLnVwZGF0ZSgpO1xuICB9XG5cbiAgdXBkYXRlKCk6IHZvaWQge1xuICAgIGxldCBzdGFydFJhZGl1cyA9IHRoaXMucmFkaXVzO1xuICAgIGlmICh0aGlzLmV4cGxvZGVTbGljZXMpIHtcbiAgICAgIHN0YXJ0UmFkaXVzID0gKHRoaXMucmFkaXVzICogdGhpcy52YWx1ZSkgLyB0aGlzLm1heDtcbiAgICB9XG5cbiAgICBjb25zdCBpbm5lckFyYyA9IGFyYygpXG4gICAgICAuaW5uZXJSYWRpdXMoc3RhcnRSYWRpdXMpXG4gICAgICAub3V0ZXJSYWRpdXMoc3RhcnRSYWRpdXMpO1xuXG4gICAgLy8gQ2FsY3VsYXRlIGlubmVyUG9zIHRoZW4gc2NhbGUgb3V0ZXIgcG9zaXRpb24gdG8gbWF0Y2ggbGFiZWwgcG9zaXRpb25cbiAgICBjb25zdCBpbm5lclBvcyA9IGlubmVyQXJjLmNlbnRyb2lkKHRoaXMuZGF0YSk7XG5cbiAgICBsZXQgc2NhbGUgPSB0aGlzLmRhdGEucG9zWzFdIC8gaW5uZXJQb3NbMV07XG4gICAgaWYgKHRoaXMuZGF0YS5wb3NbMV0gPT09IDAgfHwgaW5uZXJQb3NbMV0gPT09IDApIHtcbiAgICAgIHNjYWxlID0gMTtcbiAgICB9XG4gICAgY29uc3Qgb3V0ZXJQb3MgPSBbc2NhbGUgKiBpbm5lclBvc1swXSwgc2NhbGUgKiBpbm5lclBvc1sxXV07XG5cbiAgICB0aGlzLmxpbmUgPSBgTSR7aW5uZXJQb3N9TCR7b3V0ZXJQb3N9TCR7dGhpcy5kYXRhLnBvc31gO1xuICB9XG5cbiAgZ2V0IHRleHRYKCk6IG51bWJlciB7XG4gICAgcmV0dXJuIHRoaXMuZGF0YS5wb3NbMF07XG4gIH1cblxuICBnZXQgdGV4dFkoKTogbnVtYmVyIHtcbiAgICByZXR1cm4gdGhpcy5kYXRhLnBvc1sxXTtcbiAgfVxuXG4gIGdldCBzdHlsZVRyYW5zZm9ybSgpOiBzdHJpbmcge1xuICAgIHJldHVybiB0aGlzLmlzSUUgPyBudWxsIDogYHRyYW5zbGF0ZTNkKCR7dGhpcy50ZXh0WH1weCwke3RoaXMudGV4dFl9cHgsIDApYDtcbiAgfVxuXG4gIGdldCBhdHRyVHJhbnNmb3JtKCk6IHN0cmluZyB7XG4gICAgcmV0dXJuICF0aGlzLmlzSUUgPyBudWxsIDogYHRyYW5zbGF0ZSgke3RoaXMudGV4dFh9LCR7dGhpcy50ZXh0WX0pYDtcbiAgfVxuXG4gIGdldCB0ZXh0VHJhbnNpdGlvbigpOiBzdHJpbmcge1xuICAgIHJldHVybiB0aGlzLmlzSUUgfHwgIXRoaXMuYW5pbWF0aW9ucyA/IG51bGwgOiAndHJhbnNmb3JtIDAuNzVzJztcbiAgfVxuXG4gIHRleHRBbmNob3IoKTogYW55IHtcbiAgICByZXR1cm4gdGhpcy5taWRBbmdsZSh0aGlzLmRhdGEpIDwgTWF0aC5QSSA/ICdzdGFydCcgOiAnZW5kJztcbiAgfVxuXG4gIG1pZEFuZ2xlKGQpOiBudW1iZXIge1xuICAgIHJldHVybiBkLnN0YXJ0QW5nbGUgKyAoZC5lbmRBbmdsZSAtIGQuc3RhcnRBbmdsZSkgLyAyO1xuICB9XG59XG4iXX0=