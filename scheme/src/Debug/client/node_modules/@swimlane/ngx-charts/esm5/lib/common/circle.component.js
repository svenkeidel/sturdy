import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy, HostListener } from '@angular/core';
var CircleComponent = /** @class */ (function () {
    function CircleComponent() {
        this.select = new EventEmitter();
        this.activate = new EventEmitter();
        this.deactivate = new EventEmitter();
    }
    CircleComponent.prototype.onClick = function () {
        this.select.emit(this.data);
    };
    CircleComponent.prototype.onMouseEnter = function () {
        this.activate.emit(this.data);
    };
    CircleComponent.prototype.onMouseLeave = function () {
        this.deactivate.emit(this.data);
    };
    CircleComponent.prototype.ngOnChanges = function (changes) {
        this.classNames = Array.isArray(this.classNames) ? this.classNames.join(' ') : '';
        this.classNames += 'circle';
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
            template: "\n    <svg:circle\n      [attr.cx]=\"cx\"\n      [attr.cy]=\"cy\"\n      [attr.r]=\"r\"\n      [attr.fill]=\"fill\"\n      [attr.stroke]=\"stroke\"\n      [attr.opacity]=\"circleOpacity\"\n      [attr.class]=\"classNames\"\n      [attr.pointer-events]=\"pointerEvents\"\n    />\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], CircleComponent);
    return CircleComponent;
}());
export { CircleComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2lyY2xlLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi9jaXJjbGUuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQ0wsU0FBUyxFQUNULEtBQUssRUFFTCxNQUFNLEVBQ04sWUFBWSxFQUVaLHVCQUF1QixFQUN2QixZQUFZLEVBQ2IsTUFBTSxlQUFlLENBQUM7QUFrQnZCO0lBQUE7UUFXWSxXQUFNLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUM1QixhQUFRLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUM5QixlQUFVLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztJQXFCNUMsQ0FBQztJQWxCQyxpQ0FBTyxHQUFQO1FBQ0UsSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQzlCLENBQUM7SUFHRCxzQ0FBWSxHQUFaO1FBQ0UsSUFBSSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQ2hDLENBQUM7SUFHRCxzQ0FBWSxHQUFaO1FBQ0UsSUFBSSxDQUFDLFVBQVUsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQ2xDLENBQUM7SUFFRCxxQ0FBVyxHQUFYLFVBQVksT0FBc0I7UUFDaEMsSUFBSSxDQUFDLFVBQVUsR0FBRyxLQUFLLENBQUMsT0FBTyxDQUFDLElBQUksQ0FBQyxVQUFVLENBQUMsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLFVBQVUsQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLENBQUMsQ0FBQyxDQUFDLEVBQUUsQ0FBQztRQUNsRixJQUFJLENBQUMsVUFBVSxJQUFJLFFBQVEsQ0FBQztJQUM5QixDQUFDO0lBaENRO1FBQVIsS0FBSyxFQUFFOytDQUFJO0lBQ0g7UUFBUixLQUFLLEVBQUU7K0NBQUk7SUFDSDtRQUFSLEtBQUssRUFBRTs4Q0FBRztJQUNGO1FBQVIsS0FBSyxFQUFFO2lEQUFNO0lBQ0w7UUFBUixLQUFLLEVBQUU7bURBQVE7SUFDUDtRQUFSLEtBQUssRUFBRTtpREFBTTtJQUNMO1FBQVIsS0FBSyxFQUFFO3VEQUFZO0lBQ1g7UUFBUixLQUFLLEVBQUU7MERBQWU7SUFDZDtRQUFSLEtBQUssRUFBRTswREFBZTtJQUViO1FBQVQsTUFBTSxFQUFFO21EQUE2QjtJQUM1QjtRQUFULE1BQU0sRUFBRTtxREFBK0I7SUFDOUI7UUFBVCxNQUFNLEVBQUU7dURBQWlDO0lBRzFDO1FBREMsWUFBWSxDQUFDLE9BQU8sQ0FBQztrREFHckI7SUFHRDtRQURDLFlBQVksQ0FBQyxZQUFZLENBQUM7dURBRzFCO0lBR0Q7UUFEQyxZQUFZLENBQUMsWUFBWSxDQUFDO3VEQUcxQjtJQTVCVSxlQUFlO1FBaEIzQixTQUFTLENBQUM7WUFDVCxRQUFRLEVBQUUsc0JBQXNCO1lBQ2hDLFFBQVEsRUFBRSwyUkFXVDtZQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO1NBQ2hELENBQUM7T0FDVyxlQUFlLENBa0MzQjtJQUFELHNCQUFDO0NBQUEsQUFsQ0QsSUFrQ0M7U0FsQ1ksZUFBZSIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIENvbXBvbmVudCxcbiAgSW5wdXQsXG4gIFNpbXBsZUNoYW5nZXMsXG4gIE91dHB1dCxcbiAgRXZlbnRFbWl0dGVyLFxuICBPbkNoYW5nZXMsXG4gIENoYW5nZURldGVjdGlvblN0cmF0ZWd5LFxuICBIb3N0TGlzdGVuZXJcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ2dbbmd4LWNoYXJ0cy1jaXJjbGVdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOmNpcmNsZVxuICAgICAgW2F0dHIuY3hdPVwiY3hcIlxuICAgICAgW2F0dHIuY3ldPVwiY3lcIlxuICAgICAgW2F0dHIucl09XCJyXCJcbiAgICAgIFthdHRyLmZpbGxdPVwiZmlsbFwiXG4gICAgICBbYXR0ci5zdHJva2VdPVwic3Ryb2tlXCJcbiAgICAgIFthdHRyLm9wYWNpdHldPVwiY2lyY2xlT3BhY2l0eVwiXG4gICAgICBbYXR0ci5jbGFzc109XCJjbGFzc05hbWVzXCJcbiAgICAgIFthdHRyLnBvaW50ZXItZXZlbnRzXT1cInBvaW50ZXJFdmVudHNcIlxuICAgIC8+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIENpcmNsZUNvbXBvbmVudCBpbXBsZW1lbnRzIE9uQ2hhbmdlcyB7XG4gIEBJbnB1dCgpIGN4O1xuICBASW5wdXQoKSBjeTtcbiAgQElucHV0KCkgcjtcbiAgQElucHV0KCkgZmlsbDtcbiAgQElucHV0KCkgc3Ryb2tlO1xuICBASW5wdXQoKSBkYXRhO1xuICBASW5wdXQoKSBjbGFzc05hbWVzO1xuICBASW5wdXQoKSBjaXJjbGVPcGFjaXR5O1xuICBASW5wdXQoKSBwb2ludGVyRXZlbnRzO1xuXG4gIEBPdXRwdXQoKSBzZWxlY3QgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBhY3RpdmF0ZSA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcbiAgQE91dHB1dCgpIGRlYWN0aXZhdGUgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgQEhvc3RMaXN0ZW5lcignY2xpY2snKVxuICBvbkNsaWNrKCkge1xuICAgIHRoaXMuc2VsZWN0LmVtaXQodGhpcy5kYXRhKTtcbiAgfVxuXG4gIEBIb3N0TGlzdGVuZXIoJ21vdXNlZW50ZXInKVxuICBvbk1vdXNlRW50ZXIoKTogdm9pZCB7XG4gICAgdGhpcy5hY3RpdmF0ZS5lbWl0KHRoaXMuZGF0YSk7XG4gIH1cblxuICBASG9zdExpc3RlbmVyKCdtb3VzZWxlYXZlJylcbiAgb25Nb3VzZUxlYXZlKCk6IHZvaWQge1xuICAgIHRoaXMuZGVhY3RpdmF0ZS5lbWl0KHRoaXMuZGF0YSk7XG4gIH1cblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKTogdm9pZCB7XG4gICAgdGhpcy5jbGFzc05hbWVzID0gQXJyYXkuaXNBcnJheSh0aGlzLmNsYXNzTmFtZXMpID8gdGhpcy5jbGFzc05hbWVzLmpvaW4oJyAnKSA6ICcnO1xuICAgIHRoaXMuY2xhc3NOYW1lcyArPSAnY2lyY2xlJztcbiAgfVxufVxuIl19