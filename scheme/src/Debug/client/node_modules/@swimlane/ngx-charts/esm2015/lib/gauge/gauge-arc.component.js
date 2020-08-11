import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy } from '@angular/core';
import { formatLabel, escapeLabel } from '../common/label.helper';
let GaugeArcComponent = class GaugeArcComponent {
    constructor() {
        this.isActive = false;
        this.tooltipDisabled = false;
        this.animations = true;
        this.select = new EventEmitter();
        this.activate = new EventEmitter();
        this.deactivate = new EventEmitter();
    }
    tooltipText(arc) {
        const label = formatLabel(arc.data.name);
        let val;
        if (this.valueFormatting) {
            val = this.valueFormatting(arc.data.value);
        }
        else {
            val = formatLabel(arc.data.value);
        }
        return `
      <span class="tooltip-label">${escapeLabel(label)}</span>
      <span class="tooltip-val">${val}</span>
    `;
    }
};
__decorate([
    Input()
], GaugeArcComponent.prototype, "backgroundArc", void 0);
__decorate([
    Input()
], GaugeArcComponent.prototype, "valueArc", void 0);
__decorate([
    Input()
], GaugeArcComponent.prototype, "cornerRadius", void 0);
__decorate([
    Input()
], GaugeArcComponent.prototype, "colors", void 0);
__decorate([
    Input()
], GaugeArcComponent.prototype, "isActive", void 0);
__decorate([
    Input()
], GaugeArcComponent.prototype, "tooltipDisabled", void 0);
__decorate([
    Input()
], GaugeArcComponent.prototype, "valueFormatting", void 0);
__decorate([
    Input()
], GaugeArcComponent.prototype, "tooltipTemplate", void 0);
__decorate([
    Input()
], GaugeArcComponent.prototype, "animations", void 0);
__decorate([
    Output()
], GaugeArcComponent.prototype, "select", void 0);
__decorate([
    Output()
], GaugeArcComponent.prototype, "activate", void 0);
__decorate([
    Output()
], GaugeArcComponent.prototype, "deactivate", void 0);
GaugeArcComponent = __decorate([
    Component({
        selector: 'g[ngx-charts-gauge-arc]',
        template: `
    <svg:g
      ngx-charts-pie-arc
      class="background-arc"
      [startAngle]="0"
      [endAngle]="backgroundArc.endAngle"
      [innerRadius]="backgroundArc.innerRadius"
      [outerRadius]="backgroundArc.outerRadius"
      [cornerRadius]="cornerRadius"
      [data]="backgroundArc.data"
      [animate]="false"
      [pointerEvents]="false"
    ></svg:g>
    <svg:g
      ngx-charts-pie-arc
      [startAngle]="0"
      [endAngle]="valueArc.endAngle"
      [innerRadius]="valueArc.innerRadius"
      [outerRadius]="valueArc.outerRadius"
      [cornerRadius]="cornerRadius"
      [fill]="colors.getColor(valueArc.data.name)"
      [data]="valueArc.data"
      [animate]="animations"
      [isActive]="isActive"
      (select)="select.emit($event)"
      (activate)="activate.emit($event)"
      (deactivate)="deactivate.emit($event)"
      ngx-tooltip
      [tooltipDisabled]="tooltipDisabled"
      [tooltipPlacement]="'top'"
      [tooltipType]="'tooltip'"
      [tooltipTitle]="tooltipTemplate ? undefined : tooltipText(valueArc)"
      [tooltipTemplate]="tooltipTemplate"
      [tooltipContext]="valueArc.data"
    ></svg:g>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush
    })
], GaugeArcComponent);
export { GaugeArcComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZ2F1Z2UtYXJjLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2dhdWdlL2dhdWdlLWFyYy5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFBRSxTQUFTLEVBQUUsS0FBSyxFQUFFLE1BQU0sRUFBRSxZQUFZLEVBQUUsdUJBQXVCLEVBQWUsTUFBTSxlQUFlLENBQUM7QUFDN0csT0FBTyxFQUFFLFdBQVcsRUFBRSxXQUFXLEVBQUUsTUFBTSx3QkFBd0IsQ0FBQztBQTJDbEUsSUFBYSxpQkFBaUIsR0FBOUIsTUFBYSxpQkFBaUI7SUFBOUI7UUFLVyxhQUFRLEdBQVksS0FBSyxDQUFDO1FBQzFCLG9CQUFlLEdBQVksS0FBSyxDQUFDO1FBR2pDLGVBQVUsR0FBWSxJQUFJLENBQUM7UUFFMUIsV0FBTSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFDNUIsYUFBUSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFDOUIsZUFBVSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7SUFpQjVDLENBQUM7SUFmQyxXQUFXLENBQUMsR0FBRztRQUNiLE1BQU0sS0FBSyxHQUFHLFdBQVcsQ0FBQyxHQUFHLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO1FBQ3pDLElBQUksR0FBRyxDQUFDO1FBRVIsSUFBSSxJQUFJLENBQUMsZUFBZSxFQUFFO1lBQ3hCLEdBQUcsR0FBRyxJQUFJLENBQUMsZUFBZSxDQUFDLEdBQUcsQ0FBQyxJQUFJLENBQUMsS0FBSyxDQUFDLENBQUM7U0FDNUM7YUFBTTtZQUNMLEdBQUcsR0FBRyxXQUFXLENBQUMsR0FBRyxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsQ0FBQztTQUNuQztRQUVELE9BQU87b0NBQ3lCLFdBQVcsQ0FBQyxLQUFLLENBQUM7a0NBQ3BCLEdBQUc7S0FDaEMsQ0FBQztJQUNKLENBQUM7Q0FDRixDQUFBO0FBN0JVO0lBQVIsS0FBSyxFQUFFO3dEQUFvQjtBQUNuQjtJQUFSLEtBQUssRUFBRTttREFBZTtBQUNkO0lBQVIsS0FBSyxFQUFFO3VEQUFtQjtBQUNsQjtJQUFSLEtBQUssRUFBRTtpREFBcUI7QUFDcEI7SUFBUixLQUFLLEVBQUU7bURBQTJCO0FBQzFCO0lBQVIsS0FBSyxFQUFFOzBEQUFrQztBQUNqQztJQUFSLEtBQUssRUFBRTswREFBeUM7QUFDeEM7SUFBUixLQUFLLEVBQUU7MERBQW1DO0FBQ2xDO0lBQVIsS0FBSyxFQUFFO3FEQUE0QjtBQUUxQjtJQUFULE1BQU0sRUFBRTtpREFBNkI7QUFDNUI7SUFBVCxNQUFNLEVBQUU7bURBQStCO0FBQzlCO0lBQVQsTUFBTSxFQUFFO3FEQUFpQztBQWIvQixpQkFBaUI7SUF4QzdCLFNBQVMsQ0FBQztRQUNULFFBQVEsRUFBRSx5QkFBeUI7UUFDbkMsUUFBUSxFQUFFOzs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7OztHQW1DVDtRQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO0tBQ2hELENBQUM7R0FDVyxpQkFBaUIsQ0E4QjdCO1NBOUJZLGlCQUFpQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IENvbXBvbmVudCwgSW5wdXQsIE91dHB1dCwgRXZlbnRFbWl0dGVyLCBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSwgVGVtcGxhdGVSZWYgfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IGZvcm1hdExhYmVsLCBlc2NhcGVMYWJlbCB9IGZyb20gJy4uL2NvbW1vbi9sYWJlbC5oZWxwZXInO1xuaW1wb3J0IHsgQ29sb3JIZWxwZXIgfSBmcm9tICcuLi9jb21tb24vY29sb3IuaGVscGVyJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnZ1tuZ3gtY2hhcnRzLWdhdWdlLWFyY10nLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxzdmc6Z1xuICAgICAgbmd4LWNoYXJ0cy1waWUtYXJjXG4gICAgICBjbGFzcz1cImJhY2tncm91bmQtYXJjXCJcbiAgICAgIFtzdGFydEFuZ2xlXT1cIjBcIlxuICAgICAgW2VuZEFuZ2xlXT1cImJhY2tncm91bmRBcmMuZW5kQW5nbGVcIlxuICAgICAgW2lubmVyUmFkaXVzXT1cImJhY2tncm91bmRBcmMuaW5uZXJSYWRpdXNcIlxuICAgICAgW291dGVyUmFkaXVzXT1cImJhY2tncm91bmRBcmMub3V0ZXJSYWRpdXNcIlxuICAgICAgW2Nvcm5lclJhZGl1c109XCJjb3JuZXJSYWRpdXNcIlxuICAgICAgW2RhdGFdPVwiYmFja2dyb3VuZEFyYy5kYXRhXCJcbiAgICAgIFthbmltYXRlXT1cImZhbHNlXCJcbiAgICAgIFtwb2ludGVyRXZlbnRzXT1cImZhbHNlXCJcbiAgICA+PC9zdmc6Zz5cbiAgICA8c3ZnOmdcbiAgICAgIG5neC1jaGFydHMtcGllLWFyY1xuICAgICAgW3N0YXJ0QW5nbGVdPVwiMFwiXG4gICAgICBbZW5kQW5nbGVdPVwidmFsdWVBcmMuZW5kQW5nbGVcIlxuICAgICAgW2lubmVyUmFkaXVzXT1cInZhbHVlQXJjLmlubmVyUmFkaXVzXCJcbiAgICAgIFtvdXRlclJhZGl1c109XCJ2YWx1ZUFyYy5vdXRlclJhZGl1c1wiXG4gICAgICBbY29ybmVyUmFkaXVzXT1cImNvcm5lclJhZGl1c1wiXG4gICAgICBbZmlsbF09XCJjb2xvcnMuZ2V0Q29sb3IodmFsdWVBcmMuZGF0YS5uYW1lKVwiXG4gICAgICBbZGF0YV09XCJ2YWx1ZUFyYy5kYXRhXCJcbiAgICAgIFthbmltYXRlXT1cImFuaW1hdGlvbnNcIlxuICAgICAgW2lzQWN0aXZlXT1cImlzQWN0aXZlXCJcbiAgICAgIChzZWxlY3QpPVwic2VsZWN0LmVtaXQoJGV2ZW50KVwiXG4gICAgICAoYWN0aXZhdGUpPVwiYWN0aXZhdGUuZW1pdCgkZXZlbnQpXCJcbiAgICAgIChkZWFjdGl2YXRlKT1cImRlYWN0aXZhdGUuZW1pdCgkZXZlbnQpXCJcbiAgICAgIG5neC10b29sdGlwXG4gICAgICBbdG9vbHRpcERpc2FibGVkXT1cInRvb2x0aXBEaXNhYmxlZFwiXG4gICAgICBbdG9vbHRpcFBsYWNlbWVudF09XCIndG9wJ1wiXG4gICAgICBbdG9vbHRpcFR5cGVdPVwiJ3Rvb2x0aXAnXCJcbiAgICAgIFt0b29sdGlwVGl0bGVdPVwidG9vbHRpcFRlbXBsYXRlID8gdW5kZWZpbmVkIDogdG9vbHRpcFRleHQodmFsdWVBcmMpXCJcbiAgICAgIFt0b29sdGlwVGVtcGxhdGVdPVwidG9vbHRpcFRlbXBsYXRlXCJcbiAgICAgIFt0b29sdGlwQ29udGV4dF09XCJ2YWx1ZUFyYy5kYXRhXCJcbiAgICA+PC9zdmc6Zz5cbiAgYCxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2hcbn0pXG5leHBvcnQgY2xhc3MgR2F1Z2VBcmNDb21wb25lbnQge1xuICBASW5wdXQoKSBiYWNrZ3JvdW5kQXJjOiBhbnk7XG4gIEBJbnB1dCgpIHZhbHVlQXJjOiBhbnk7XG4gIEBJbnB1dCgpIGNvcm5lclJhZGl1czogYW55O1xuICBASW5wdXQoKSBjb2xvcnM6IENvbG9ySGVscGVyO1xuICBASW5wdXQoKSBpc0FjdGl2ZTogYm9vbGVhbiA9IGZhbHNlO1xuICBASW5wdXQoKSB0b29sdGlwRGlzYWJsZWQ6IGJvb2xlYW4gPSBmYWxzZTtcbiAgQElucHV0KCkgdmFsdWVGb3JtYXR0aW5nOiAodmFsdWU6IGFueSkgPT4gc3RyaW5nO1xuICBASW5wdXQoKSB0b29sdGlwVGVtcGxhdGU6IFRlbXBsYXRlUmVmPGFueT47XG4gIEBJbnB1dCgpIGFuaW1hdGlvbnM6IGJvb2xlYW4gPSB0cnVlO1xuXG4gIEBPdXRwdXQoKSBzZWxlY3QgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBhY3RpdmF0ZSA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcbiAgQE91dHB1dCgpIGRlYWN0aXZhdGUgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgdG9vbHRpcFRleHQoYXJjKTogc3RyaW5nIHtcbiAgICBjb25zdCBsYWJlbCA9IGZvcm1hdExhYmVsKGFyYy5kYXRhLm5hbWUpO1xuICAgIGxldCB2YWw7XG5cbiAgICBpZiAodGhpcy52YWx1ZUZvcm1hdHRpbmcpIHtcbiAgICAgIHZhbCA9IHRoaXMudmFsdWVGb3JtYXR0aW5nKGFyYy5kYXRhLnZhbHVlKTtcbiAgICB9IGVsc2Uge1xuICAgICAgdmFsID0gZm9ybWF0TGFiZWwoYXJjLmRhdGEudmFsdWUpO1xuICAgIH1cblxuICAgIHJldHVybiBgXG4gICAgICA8c3BhbiBjbGFzcz1cInRvb2x0aXAtbGFiZWxcIj4ke2VzY2FwZUxhYmVsKGxhYmVsKX08L3NwYW4+XG4gICAgICA8c3BhbiBjbGFzcz1cInRvb2x0aXAtdmFsXCI+JHt2YWx9PC9zcGFuPlxuICAgIGA7XG4gIH1cbn1cbiJdfQ==