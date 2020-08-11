import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ElementRef, OnChanges, SimpleChanges, ChangeDetectionStrategy } from '@angular/core';
import { pie } from 'd3-shape';
let PieGridSeriesComponent = class PieGridSeriesComponent {
    constructor(element) {
        this.innerRadius = 70;
        this.outerRadius = 80;
        this.animations = true;
        this.select = new EventEmitter();
        this.activate = new EventEmitter();
        this.deactivate = new EventEmitter();
        this.element = element.nativeElement;
    }
    ngOnChanges(changes) {
        this.update();
    }
    update() {
        this.layout = pie()
            .value(d => d.data.value)
            .sort(null);
        this.arcs = this.getArcs();
    }
    getArcs() {
        return this.layout(this.data).map((arc, index) => {
            const label = arc.data.data.name;
            const other = arc.data.data.other;
            if (index === 0) {
                arc.startAngle = 0;
            }
            const color = this.colors(label);
            return {
                data: arc.data.data,
                class: 'arc ' + 'arc' + index,
                fill: color,
                startAngle: other ? 0 : arc.startAngle,
                endAngle: arc.endAngle,
                animate: this.animations && !other,
                pointerEvents: !other
            };
        });
    }
    onClick(data) {
        this.select.emit(this.data[0].data);
    }
    trackBy(index, item) {
        return item.data.name;
    }
    label(arc) {
        return arc.data.name;
    }
    color(arc) {
        return this.colors(this.label(arc));
    }
};
PieGridSeriesComponent.ctorParameters = () => [
    { type: ElementRef }
];
__decorate([
    Input()
], PieGridSeriesComponent.prototype, "colors", void 0);
__decorate([
    Input()
], PieGridSeriesComponent.prototype, "data", void 0);
__decorate([
    Input()
], PieGridSeriesComponent.prototype, "innerRadius", void 0);
__decorate([
    Input()
], PieGridSeriesComponent.prototype, "outerRadius", void 0);
__decorate([
    Input()
], PieGridSeriesComponent.prototype, "animations", void 0);
__decorate([
    Output()
], PieGridSeriesComponent.prototype, "select", void 0);
__decorate([
    Output()
], PieGridSeriesComponent.prototype, "activate", void 0);
__decorate([
    Output()
], PieGridSeriesComponent.prototype, "deactivate", void 0);
PieGridSeriesComponent = __decorate([
    Component({
        selector: 'g[ngx-charts-pie-grid-series]',
        template: `
    <svg:g class="pie-grid-arcs">
      <svg:g
        ngx-charts-pie-arc
        *ngFor="let arc of arcs; trackBy: trackBy"
        [attr.class]="arc.class"
        [startAngle]="arc.startAngle"
        [endAngle]="arc.endAngle"
        [innerRadius]="innerRadius"
        [outerRadius]="outerRadius"
        [fill]="color(arc)"
        [value]="arc.data.value"
        [data]="arc.data"
        [gradient]="false"
        [pointerEvents]="arc.pointerEvents"
        [animate]="arc.animate"
        (select)="onClick($event)"
        (activate)="activate.emit($event)"
        (deactivate)="deactivate.emit($event)"
      ></svg:g>
    </svg:g>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush
    })
], PieGridSeriesComponent);
export { PieGridSeriesComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGllLWdyaWQtc2VyaWVzLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL3BpZS1jaGFydC9waWUtZ3JpZC1zZXJpZXMuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQ0wsU0FBUyxFQUNULEtBQUssRUFDTCxNQUFNLEVBQ04sWUFBWSxFQUNaLFVBQVUsRUFDVixTQUFTLEVBQ1QsYUFBYSxFQUNiLHVCQUF1QixFQUN4QixNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUUsR0FBRyxFQUFFLE1BQU0sVUFBVSxDQUFDO0FBNEIvQixJQUFhLHNCQUFzQixHQUFuQyxNQUFhLHNCQUFzQjtJQWVqQyxZQUFZLE9BQW1CO1FBWnRCLGdCQUFXLEdBQUcsRUFBRSxDQUFDO1FBQ2pCLGdCQUFXLEdBQUcsRUFBRSxDQUFDO1FBQ2pCLGVBQVUsR0FBWSxJQUFJLENBQUM7UUFFMUIsV0FBTSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFDNUIsYUFBUSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFDOUIsZUFBVSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFPeEMsSUFBSSxDQUFDLE9BQU8sR0FBRyxPQUFPLENBQUMsYUFBYSxDQUFDO0lBQ3ZDLENBQUM7SUFFRCxXQUFXLENBQUMsT0FBc0I7UUFDaEMsSUFBSSxDQUFDLE1BQU0sRUFBRSxDQUFDO0lBQ2hCLENBQUM7SUFFRCxNQUFNO1FBQ0osSUFBSSxDQUFDLE1BQU0sR0FBRyxHQUFHLEVBQVk7YUFDMUIsS0FBSyxDQUFDLENBQUMsQ0FBQyxFQUFFLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUM7YUFDeEIsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO1FBRWQsSUFBSSxDQUFDLElBQUksR0FBRyxJQUFJLENBQUMsT0FBTyxFQUFFLENBQUM7SUFDN0IsQ0FBQztJQUVELE9BQU87UUFDTCxPQUFPLElBQUksQ0FBQyxNQUFNLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDLEdBQUcsQ0FBQyxDQUFDLEdBQUcsRUFBRSxLQUFLLEVBQUUsRUFBRTtZQUMvQyxNQUFNLEtBQUssR0FBRyxHQUFHLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUM7WUFDakMsTUFBTSxLQUFLLEdBQUcsR0FBRyxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsS0FBSyxDQUFDO1lBRWxDLElBQUksS0FBSyxLQUFLLENBQUMsRUFBRTtnQkFDZixHQUFHLENBQUMsVUFBVSxHQUFHLENBQUMsQ0FBQzthQUNwQjtZQUVELE1BQU0sS0FBSyxHQUFHLElBQUksQ0FBQyxNQUFNLENBQUMsS0FBSyxDQUFDLENBQUM7WUFDakMsT0FBTztnQkFDTCxJQUFJLEVBQUUsR0FBRyxDQUFDLElBQUksQ0FBQyxJQUFJO2dCQUNuQixLQUFLLEVBQUUsTUFBTSxHQUFHLEtBQUssR0FBRyxLQUFLO2dCQUM3QixJQUFJLEVBQUUsS0FBSztnQkFDWCxVQUFVLEVBQUUsS0FBSyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLEdBQUcsQ0FBQyxVQUFVO2dCQUN0QyxRQUFRLEVBQUUsR0FBRyxDQUFDLFFBQVE7Z0JBQ3RCLE9BQU8sRUFBRSxJQUFJLENBQUMsVUFBVSxJQUFJLENBQUMsS0FBSztnQkFDbEMsYUFBYSxFQUFFLENBQUMsS0FBSzthQUN0QixDQUFDO1FBQ0osQ0FBQyxDQUFDLENBQUM7SUFDTCxDQUFDO0lBRUQsT0FBTyxDQUFDLElBQUk7UUFDVixJQUFJLENBQUMsTUFBTSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQ3RDLENBQUM7SUFFRCxPQUFPLENBQUMsS0FBSyxFQUFFLElBQUk7UUFDakIsT0FBTyxJQUFJLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQztJQUN4QixDQUFDO0lBRUQsS0FBSyxDQUFDLEdBQUc7UUFDUCxPQUFPLEdBQUcsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDO0lBQ3ZCLENBQUM7SUFFRCxLQUFLLENBQUMsR0FBRztRQUNQLE9BQU8sSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsS0FBSyxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUM7SUFDdEMsQ0FBQztDQUNGLENBQUE7O1lBckRzQixVQUFVOztBQWR0QjtJQUFSLEtBQUssRUFBRTtzREFBUTtBQUNQO0lBQVIsS0FBSyxFQUFFO29EQUFNO0FBQ0w7SUFBUixLQUFLLEVBQUU7MkRBQWtCO0FBQ2pCO0lBQVIsS0FBSyxFQUFFOzJEQUFrQjtBQUNqQjtJQUFSLEtBQUssRUFBRTswREFBNEI7QUFFMUI7SUFBVCxNQUFNLEVBQUU7c0RBQTZCO0FBQzVCO0lBQVQsTUFBTSxFQUFFO3dEQUErQjtBQUM5QjtJQUFULE1BQU0sRUFBRTswREFBaUM7QUFUL0Isc0JBQXNCO0lBMUJsQyxTQUFTLENBQUM7UUFDVCxRQUFRLEVBQUUsK0JBQStCO1FBQ3pDLFFBQVEsRUFBRTs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7O0dBcUJUO1FBQ0QsZUFBZSxFQUFFLHVCQUF1QixDQUFDLE1BQU07S0FDaEQsQ0FBQztHQUNXLHNCQUFzQixDQW9FbEM7U0FwRVksc0JBQXNCIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHtcbiAgQ29tcG9uZW50LFxuICBJbnB1dCxcbiAgT3V0cHV0LFxuICBFdmVudEVtaXR0ZXIsXG4gIEVsZW1lbnRSZWYsXG4gIE9uQ2hhbmdlcyxcbiAgU2ltcGxlQ2hhbmdlcyxcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3lcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBwaWUgfSBmcm9tICdkMy1zaGFwZSc7XG5cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ2dbbmd4LWNoYXJ0cy1waWUtZ3JpZC1zZXJpZXNdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOmcgY2xhc3M9XCJwaWUtZ3JpZC1hcmNzXCI+XG4gICAgICA8c3ZnOmdcbiAgICAgICAgbmd4LWNoYXJ0cy1waWUtYXJjXG4gICAgICAgICpuZ0Zvcj1cImxldCBhcmMgb2YgYXJjczsgdHJhY2tCeTogdHJhY2tCeVwiXG4gICAgICAgIFthdHRyLmNsYXNzXT1cImFyYy5jbGFzc1wiXG4gICAgICAgIFtzdGFydEFuZ2xlXT1cImFyYy5zdGFydEFuZ2xlXCJcbiAgICAgICAgW2VuZEFuZ2xlXT1cImFyYy5lbmRBbmdsZVwiXG4gICAgICAgIFtpbm5lclJhZGl1c109XCJpbm5lclJhZGl1c1wiXG4gICAgICAgIFtvdXRlclJhZGl1c109XCJvdXRlclJhZGl1c1wiXG4gICAgICAgIFtmaWxsXT1cImNvbG9yKGFyYylcIlxuICAgICAgICBbdmFsdWVdPVwiYXJjLmRhdGEudmFsdWVcIlxuICAgICAgICBbZGF0YV09XCJhcmMuZGF0YVwiXG4gICAgICAgIFtncmFkaWVudF09XCJmYWxzZVwiXG4gICAgICAgIFtwb2ludGVyRXZlbnRzXT1cImFyYy5wb2ludGVyRXZlbnRzXCJcbiAgICAgICAgW2FuaW1hdGVdPVwiYXJjLmFuaW1hdGVcIlxuICAgICAgICAoc2VsZWN0KT1cIm9uQ2xpY2soJGV2ZW50KVwiXG4gICAgICAgIChhY3RpdmF0ZSk9XCJhY3RpdmF0ZS5lbWl0KCRldmVudClcIlxuICAgICAgICAoZGVhY3RpdmF0ZSk9XCJkZWFjdGl2YXRlLmVtaXQoJGV2ZW50KVwiXG4gICAgICA+PC9zdmc6Zz5cbiAgICA8L3N2ZzpnPlxuICBgLFxuICBjaGFuZ2VEZXRlY3Rpb246IENoYW5nZURldGVjdGlvblN0cmF0ZWd5Lk9uUHVzaFxufSlcbmV4cG9ydCBjbGFzcyBQaWVHcmlkU2VyaWVzQ29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgY29sb3JzO1xuICBASW5wdXQoKSBkYXRhO1xuICBASW5wdXQoKSBpbm5lclJhZGl1cyA9IDcwO1xuICBASW5wdXQoKSBvdXRlclJhZGl1cyA9IDgwO1xuICBASW5wdXQoKSBhbmltYXRpb25zOiBib29sZWFuID0gdHJ1ZTtcblxuICBAT3V0cHV0KCkgc2VsZWN0ID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuICBAT3V0cHV0KCkgYWN0aXZhdGUgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBkZWFjdGl2YXRlID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuXG4gIGVsZW1lbnQ6IEhUTUxFbGVtZW50O1xuICBsYXlvdXQ6IGFueTtcbiAgYXJjczogYW55O1xuXG4gIGNvbnN0cnVjdG9yKGVsZW1lbnQ6IEVsZW1lbnRSZWYpIHtcbiAgICB0aGlzLmVsZW1lbnQgPSBlbGVtZW50Lm5hdGl2ZUVsZW1lbnQ7XG4gIH1cblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKTogdm9pZCB7XG4gICAgdGhpcy51cGRhdGUoKTtcbiAgfVxuXG4gIHVwZGF0ZSgpOiB2b2lkIHtcbiAgICB0aGlzLmxheW91dCA9IHBpZTxhbnksIGFueT4oKVxuICAgICAgLnZhbHVlKGQgPT4gZC5kYXRhLnZhbHVlKVxuICAgICAgLnNvcnQobnVsbCk7XG5cbiAgICB0aGlzLmFyY3MgPSB0aGlzLmdldEFyY3MoKTtcbiAgfVxuXG4gIGdldEFyY3MoKTogYW55W10ge1xuICAgIHJldHVybiB0aGlzLmxheW91dCh0aGlzLmRhdGEpLm1hcCgoYXJjLCBpbmRleCkgPT4ge1xuICAgICAgY29uc3QgbGFiZWwgPSBhcmMuZGF0YS5kYXRhLm5hbWU7XG4gICAgICBjb25zdCBvdGhlciA9IGFyYy5kYXRhLmRhdGEub3RoZXI7XG5cbiAgICAgIGlmIChpbmRleCA9PT0gMCkge1xuICAgICAgICBhcmMuc3RhcnRBbmdsZSA9IDA7XG4gICAgICB9XG5cbiAgICAgIGNvbnN0IGNvbG9yID0gdGhpcy5jb2xvcnMobGFiZWwpO1xuICAgICAgcmV0dXJuIHtcbiAgICAgICAgZGF0YTogYXJjLmRhdGEuZGF0YSxcbiAgICAgICAgY2xhc3M6ICdhcmMgJyArICdhcmMnICsgaW5kZXgsXG4gICAgICAgIGZpbGw6IGNvbG9yLFxuICAgICAgICBzdGFydEFuZ2xlOiBvdGhlciA/IDAgOiBhcmMuc3RhcnRBbmdsZSxcbiAgICAgICAgZW5kQW5nbGU6IGFyYy5lbmRBbmdsZSxcbiAgICAgICAgYW5pbWF0ZTogdGhpcy5hbmltYXRpb25zICYmICFvdGhlcixcbiAgICAgICAgcG9pbnRlckV2ZW50czogIW90aGVyXG4gICAgICB9O1xuICAgIH0pO1xuICB9XG5cbiAgb25DbGljayhkYXRhKTogdm9pZCB7XG4gICAgdGhpcy5zZWxlY3QuZW1pdCh0aGlzLmRhdGFbMF0uZGF0YSk7XG4gIH1cblxuICB0cmFja0J5KGluZGV4LCBpdGVtKTogc3RyaW5nIHtcbiAgICByZXR1cm4gaXRlbS5kYXRhLm5hbWU7XG4gIH1cblxuICBsYWJlbChhcmMpOiBzdHJpbmcge1xuICAgIHJldHVybiBhcmMuZGF0YS5uYW1lO1xuICB9XG5cbiAgY29sb3IoYXJjKTogYW55IHtcbiAgICByZXR1cm4gdGhpcy5jb2xvcnModGhpcy5sYWJlbChhcmMpKTtcbiAgfVxufVxuIl19