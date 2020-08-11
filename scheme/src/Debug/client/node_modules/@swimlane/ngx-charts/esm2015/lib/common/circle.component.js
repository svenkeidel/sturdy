import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy, HostListener } from '@angular/core';
let CircleComponent = class CircleComponent {
    constructor() {
        this.select = new EventEmitter();
        this.activate = new EventEmitter();
        this.deactivate = new EventEmitter();
    }
    onClick() {
        this.select.emit(this.data);
    }
    onMouseEnter() {
        this.activate.emit(this.data);
    }
    onMouseLeave() {
        this.deactivate.emit(this.data);
    }
    ngOnChanges(changes) {
        this.classNames = Array.isArray(this.classNames) ? this.classNames.join(' ') : '';
        this.classNames += 'circle';
    }
};
__decorate([
    Input()
], CircleComponent.prototype, "cx", void 0);
__decorate([
    Input()
], CircleComponent.prototype, "cy", void 0);
__decorate([
    Input()
], CircleComponent.prototype, "r", void 0);
__decorate([
    Input()
], CircleComponent.prototype, "fill", void 0);
__decorate([
    Input()
], CircleComponent.prototype, "stroke", void 0);
__decorate([
    Input()
], CircleComponent.prototype, "data", void 0);
__decorate([
    Input()
], CircleComponent.prototype, "classNames", void 0);
__decorate([
    Input()
], CircleComponent.prototype, "circleOpacity", void 0);
__decorate([
    Input()
], CircleComponent.prototype, "pointerEvents", void 0);
__decorate([
    Output()
], CircleComponent.prototype, "select", void 0);
__decorate([
    Output()
], CircleComponent.prototype, "activate", void 0);
__decorate([
    Output()
], CircleComponent.prototype, "deactivate", void 0);
__decorate([
    HostListener('click')
], CircleComponent.prototype, "onClick", null);
__decorate([
    HostListener('mouseenter')
], CircleComponent.prototype, "onMouseEnter", null);
__decorate([
    HostListener('mouseleave')
], CircleComponent.prototype, "onMouseLeave", null);
CircleComponent = __decorate([
    Component({
        selector: 'g[ngx-charts-circle]',
        template: `
    <svg:circle
      [attr.cx]="cx"
      [attr.cy]="cy"
      [attr.r]="r"
      [attr.fill]="fill"
      [attr.stroke]="stroke"
      [attr.opacity]="circleOpacity"
      [attr.class]="classNames"
      [attr.pointer-events]="pointerEvents"
    />
  `,
        changeDetection: ChangeDetectionStrategy.OnPush
    })
], CircleComponent);
export { CircleComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2lyY2xlLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi9jaXJjbGUuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQ0wsU0FBUyxFQUNULEtBQUssRUFFTCxNQUFNLEVBQ04sWUFBWSxFQUVaLHVCQUF1QixFQUN2QixZQUFZLEVBQ2IsTUFBTSxlQUFlLENBQUM7QUFrQnZCLElBQWEsZUFBZSxHQUE1QixNQUFhLGVBQWU7SUFBNUI7UUFXWSxXQUFNLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUM1QixhQUFRLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUM5QixlQUFVLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztJQXFCNUMsQ0FBQztJQWxCQyxPQUFPO1FBQ0wsSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQzlCLENBQUM7SUFHRCxZQUFZO1FBQ1YsSUFBSSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQ2hDLENBQUM7SUFHRCxZQUFZO1FBQ1YsSUFBSSxDQUFDLFVBQVUsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQ2xDLENBQUM7SUFFRCxXQUFXLENBQUMsT0FBc0I7UUFDaEMsSUFBSSxDQUFDLFVBQVUsR0FBRyxLQUFLLENBQUMsT0FBTyxDQUFDLElBQUksQ0FBQyxVQUFVLENBQUMsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLFVBQVUsQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLENBQUMsQ0FBQyxDQUFDLEVBQUUsQ0FBQztRQUNsRixJQUFJLENBQUMsVUFBVSxJQUFJLFFBQVEsQ0FBQztJQUM5QixDQUFDO0NBQ0YsQ0FBQTtBQWpDVTtJQUFSLEtBQUssRUFBRTsyQ0FBSTtBQUNIO0lBQVIsS0FBSyxFQUFFOzJDQUFJO0FBQ0g7SUFBUixLQUFLLEVBQUU7MENBQUc7QUFDRjtJQUFSLEtBQUssRUFBRTs2Q0FBTTtBQUNMO0lBQVIsS0FBSyxFQUFFOytDQUFRO0FBQ1A7SUFBUixLQUFLLEVBQUU7NkNBQU07QUFDTDtJQUFSLEtBQUssRUFBRTttREFBWTtBQUNYO0lBQVIsS0FBSyxFQUFFO3NEQUFlO0FBQ2Q7SUFBUixLQUFLLEVBQUU7c0RBQWU7QUFFYjtJQUFULE1BQU0sRUFBRTsrQ0FBNkI7QUFDNUI7SUFBVCxNQUFNLEVBQUU7aURBQStCO0FBQzlCO0lBQVQsTUFBTSxFQUFFO21EQUFpQztBQUcxQztJQURDLFlBQVksQ0FBQyxPQUFPLENBQUM7OENBR3JCO0FBR0Q7SUFEQyxZQUFZLENBQUMsWUFBWSxDQUFDO21EQUcxQjtBQUdEO0lBREMsWUFBWSxDQUFDLFlBQVksQ0FBQzttREFHMUI7QUE1QlUsZUFBZTtJQWhCM0IsU0FBUyxDQUFDO1FBQ1QsUUFBUSxFQUFFLHNCQUFzQjtRQUNoQyxRQUFRLEVBQUU7Ozs7Ozs7Ozs7O0dBV1Q7UUFDRCxlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTtLQUNoRCxDQUFDO0dBQ1csZUFBZSxDQWtDM0I7U0FsQ1ksZUFBZSIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIENvbXBvbmVudCxcbiAgSW5wdXQsXG4gIFNpbXBsZUNoYW5nZXMsXG4gIE91dHB1dCxcbiAgRXZlbnRFbWl0dGVyLFxuICBPbkNoYW5nZXMsXG4gIENoYW5nZURldGVjdGlvblN0cmF0ZWd5LFxuICBIb3N0TGlzdGVuZXJcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ2dbbmd4LWNoYXJ0cy1jaXJjbGVdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOmNpcmNsZVxuICAgICAgW2F0dHIuY3hdPVwiY3hcIlxuICAgICAgW2F0dHIuY3ldPVwiY3lcIlxuICAgICAgW2F0dHIucl09XCJyXCJcbiAgICAgIFthdHRyLmZpbGxdPVwiZmlsbFwiXG4gICAgICBbYXR0ci5zdHJva2VdPVwic3Ryb2tlXCJcbiAgICAgIFthdHRyLm9wYWNpdHldPVwiY2lyY2xlT3BhY2l0eVwiXG4gICAgICBbYXR0ci5jbGFzc109XCJjbGFzc05hbWVzXCJcbiAgICAgIFthdHRyLnBvaW50ZXItZXZlbnRzXT1cInBvaW50ZXJFdmVudHNcIlxuICAgIC8+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIENpcmNsZUNvbXBvbmVudCBpbXBsZW1lbnRzIE9uQ2hhbmdlcyB7XG4gIEBJbnB1dCgpIGN4O1xuICBASW5wdXQoKSBjeTtcbiAgQElucHV0KCkgcjtcbiAgQElucHV0KCkgZmlsbDtcbiAgQElucHV0KCkgc3Ryb2tlO1xuICBASW5wdXQoKSBkYXRhO1xuICBASW5wdXQoKSBjbGFzc05hbWVzO1xuICBASW5wdXQoKSBjaXJjbGVPcGFjaXR5O1xuICBASW5wdXQoKSBwb2ludGVyRXZlbnRzO1xuXG4gIEBPdXRwdXQoKSBzZWxlY3QgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBhY3RpdmF0ZSA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcbiAgQE91dHB1dCgpIGRlYWN0aXZhdGUgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgQEhvc3RMaXN0ZW5lcignY2xpY2snKVxuICBvbkNsaWNrKCkge1xuICAgIHRoaXMuc2VsZWN0LmVtaXQodGhpcy5kYXRhKTtcbiAgfVxuXG4gIEBIb3N0TGlzdGVuZXIoJ21vdXNlZW50ZXInKVxuICBvbk1vdXNlRW50ZXIoKTogdm9pZCB7XG4gICAgdGhpcy5hY3RpdmF0ZS5lbWl0KHRoaXMuZGF0YSk7XG4gIH1cblxuICBASG9zdExpc3RlbmVyKCdtb3VzZWxlYXZlJylcbiAgb25Nb3VzZUxlYXZlKCk6IHZvaWQge1xuICAgIHRoaXMuZGVhY3RpdmF0ZS5lbWl0KHRoaXMuZGF0YSk7XG4gIH1cblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKTogdm9pZCB7XG4gICAgdGhpcy5jbGFzc05hbWVzID0gQXJyYXkuaXNBcnJheSh0aGlzLmNsYXNzTmFtZXMpID8gdGhpcy5jbGFzc05hbWVzLmpvaW4oJyAnKSA6ICcnO1xuICAgIHRoaXMuY2xhc3NOYW1lcyArPSAnY2lyY2xlJztcbiAgfVxufVxuIl19