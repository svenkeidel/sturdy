import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ElementRef, OnChanges, SimpleChanges, ChangeDetectionStrategy } from '@angular/core';
import { pie } from 'd3-shape';
var PieGridSeriesComponent = /** @class */ (function () {
    function PieGridSeriesComponent(element) {
        this.innerRadius = 70;
        this.outerRadius = 80;
        this.animations = true;
        this.select = new EventEmitter();
        this.activate = new EventEmitter();
        this.deactivate = new EventEmitter();
        this.element = element.nativeElement;
    }
    PieGridSeriesComponent.prototype.ngOnChanges = function (changes) {
        this.update();
    };
    PieGridSeriesComponent.prototype.update = function () {
        this.layout = pie()
            .value(function (d) { return d.data.value; })
            .sort(null);
        this.arcs = this.getArcs();
    };
    PieGridSeriesComponent.prototype.getArcs = function () {
        var _this = this;
        return this.layout(this.data).map(function (arc, index) {
            var label = arc.data.data.name;
            var other = arc.data.data.other;
            if (index === 0) {
                arc.startAngle = 0;
            }
            var color = _this.colors(label);
            return {
                data: arc.data.data,
                class: 'arc ' + 'arc' + index,
                fill: color,
                startAngle: other ? 0 : arc.startAngle,
                endAngle: arc.endAngle,
                animate: _this.animations && !other,
                pointerEvents: !other
            };
        });
    };
    PieGridSeriesComponent.prototype.onClick = function (data) {
        this.select.emit(this.data[0].data);
    };
    PieGridSeriesComponent.prototype.trackBy = function (index, item) {
        return item.data.name;
    };
    PieGridSeriesComponent.prototype.label = function (arc) {
        return arc.data.name;
    };
    PieGridSeriesComponent.prototype.color = function (arc) {
        return this.colors(this.label(arc));
    };
    PieGridSeriesComponent.ctorParameters = function () { return [
        { type: ElementRef }
    ]; };
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
            template: "\n    <svg:g class=\"pie-grid-arcs\">\n      <svg:g\n        ngx-charts-pie-arc\n        *ngFor=\"let arc of arcs; trackBy: trackBy\"\n        [attr.class]=\"arc.class\"\n        [startAngle]=\"arc.startAngle\"\n        [endAngle]=\"arc.endAngle\"\n        [innerRadius]=\"innerRadius\"\n        [outerRadius]=\"outerRadius\"\n        [fill]=\"color(arc)\"\n        [value]=\"arc.data.value\"\n        [data]=\"arc.data\"\n        [gradient]=\"false\"\n        [pointerEvents]=\"arc.pointerEvents\"\n        [animate]=\"arc.animate\"\n        (select)=\"onClick($event)\"\n        (activate)=\"activate.emit($event)\"\n        (deactivate)=\"deactivate.emit($event)\"\n      ></svg:g>\n    </svg:g>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], PieGridSeriesComponent);
    return PieGridSeriesComponent;
}());
export { PieGridSeriesComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGllLWdyaWQtc2VyaWVzLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL3BpZS1jaGFydC9waWUtZ3JpZC1zZXJpZXMuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQ0wsU0FBUyxFQUNULEtBQUssRUFDTCxNQUFNLEVBQ04sWUFBWSxFQUNaLFVBQVUsRUFDVixTQUFTLEVBQ1QsYUFBYSxFQUNiLHVCQUF1QixFQUN4QixNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUUsR0FBRyxFQUFFLE1BQU0sVUFBVSxDQUFDO0FBNEIvQjtJQWVFLGdDQUFZLE9BQW1CO1FBWnRCLGdCQUFXLEdBQUcsRUFBRSxDQUFDO1FBQ2pCLGdCQUFXLEdBQUcsRUFBRSxDQUFDO1FBQ2pCLGVBQVUsR0FBWSxJQUFJLENBQUM7UUFFMUIsV0FBTSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFDNUIsYUFBUSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFDOUIsZUFBVSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFPeEMsSUFBSSxDQUFDLE9BQU8sR0FBRyxPQUFPLENBQUMsYUFBYSxDQUFDO0lBQ3ZDLENBQUM7SUFFRCw0Q0FBVyxHQUFYLFVBQVksT0FBc0I7UUFDaEMsSUFBSSxDQUFDLE1BQU0sRUFBRSxDQUFDO0lBQ2hCLENBQUM7SUFFRCx1Q0FBTSxHQUFOO1FBQ0UsSUFBSSxDQUFDLE1BQU0sR0FBRyxHQUFHLEVBQVk7YUFDMUIsS0FBSyxDQUFDLFVBQUEsQ0FBQyxJQUFJLE9BQUEsQ0FBQyxDQUFDLElBQUksQ0FBQyxLQUFLLEVBQVosQ0FBWSxDQUFDO2FBQ3hCLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQztRQUVkLElBQUksQ0FBQyxJQUFJLEdBQUcsSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDO0lBQzdCLENBQUM7SUFFRCx3Q0FBTyxHQUFQO1FBQUEsaUJBb0JDO1FBbkJDLE9BQU8sSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUMsR0FBRyxDQUFDLFVBQUMsR0FBRyxFQUFFLEtBQUs7WUFDM0MsSUFBTSxLQUFLLEdBQUcsR0FBRyxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDO1lBQ2pDLElBQU0sS0FBSyxHQUFHLEdBQUcsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQztZQUVsQyxJQUFJLEtBQUssS0FBSyxDQUFDLEVBQUU7Z0JBQ2YsR0FBRyxDQUFDLFVBQVUsR0FBRyxDQUFDLENBQUM7YUFDcEI7WUFFRCxJQUFNLEtBQUssR0FBRyxLQUFJLENBQUMsTUFBTSxDQUFDLEtBQUssQ0FBQyxDQUFDO1lBQ2pDLE9BQU87Z0JBQ0wsSUFBSSxFQUFFLEdBQUcsQ0FBQyxJQUFJLENBQUMsSUFBSTtnQkFDbkIsS0FBSyxFQUFFLE1BQU0sR0FBRyxLQUFLLEdBQUcsS0FBSztnQkFDN0IsSUFBSSxFQUFFLEtBQUs7Z0JBQ1gsVUFBVSxFQUFFLEtBQUssQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxHQUFHLENBQUMsVUFBVTtnQkFDdEMsUUFBUSxFQUFFLEdBQUcsQ0FBQyxRQUFRO2dCQUN0QixPQUFPLEVBQUUsS0FBSSxDQUFDLFVBQVUsSUFBSSxDQUFDLEtBQUs7Z0JBQ2xDLGFBQWEsRUFBRSxDQUFDLEtBQUs7YUFDdEIsQ0FBQztRQUNKLENBQUMsQ0FBQyxDQUFDO0lBQ0wsQ0FBQztJQUVELHdDQUFPLEdBQVAsVUFBUSxJQUFJO1FBQ1YsSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsQ0FBQztJQUN0QyxDQUFDO0lBRUQsd0NBQU8sR0FBUCxVQUFRLEtBQUssRUFBRSxJQUFJO1FBQ2pCLE9BQU8sSUFBSSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUM7SUFDeEIsQ0FBQztJQUVELHNDQUFLLEdBQUwsVUFBTSxHQUFHO1FBQ1AsT0FBTyxHQUFHLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQztJQUN2QixDQUFDO0lBRUQsc0NBQUssR0FBTCxVQUFNLEdBQUc7UUFDUCxPQUFPLElBQUksQ0FBQyxNQUFNLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxHQUFHLENBQUMsQ0FBQyxDQUFDO0lBQ3RDLENBQUM7O2dCQXBEb0IsVUFBVTs7SUFkdEI7UUFBUixLQUFLLEVBQUU7MERBQVE7SUFDUDtRQUFSLEtBQUssRUFBRTt3REFBTTtJQUNMO1FBQVIsS0FBSyxFQUFFOytEQUFrQjtJQUNqQjtRQUFSLEtBQUssRUFBRTsrREFBa0I7SUFDakI7UUFBUixLQUFLLEVBQUU7OERBQTRCO0lBRTFCO1FBQVQsTUFBTSxFQUFFOzBEQUE2QjtJQUM1QjtRQUFULE1BQU0sRUFBRTs0REFBK0I7SUFDOUI7UUFBVCxNQUFNLEVBQUU7OERBQWlDO0lBVC9CLHNCQUFzQjtRQTFCbEMsU0FBUyxDQUFDO1lBQ1QsUUFBUSxFQUFFLCtCQUErQjtZQUN6QyxRQUFRLEVBQUUsZ3NCQXFCVDtZQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO1NBQ2hELENBQUM7T0FDVyxzQkFBc0IsQ0FvRWxDO0lBQUQsNkJBQUM7Q0FBQSxBQXBFRCxJQW9FQztTQXBFWSxzQkFBc0IiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQge1xuICBDb21wb25lbnQsXG4gIElucHV0LFxuICBPdXRwdXQsXG4gIEV2ZW50RW1pdHRlcixcbiAgRWxlbWVudFJlZixcbiAgT25DaGFuZ2VzLFxuICBTaW1wbGVDaGFuZ2VzLFxuICBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneVxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IHBpZSB9IGZyb20gJ2QzLXNoYXBlJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnZ1tuZ3gtY2hhcnRzLXBpZS1ncmlkLXNlcmllc10nLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxzdmc6ZyBjbGFzcz1cInBpZS1ncmlkLWFyY3NcIj5cbiAgICAgIDxzdmc6Z1xuICAgICAgICBuZ3gtY2hhcnRzLXBpZS1hcmNcbiAgICAgICAgKm5nRm9yPVwibGV0IGFyYyBvZiBhcmNzOyB0cmFja0J5OiB0cmFja0J5XCJcbiAgICAgICAgW2F0dHIuY2xhc3NdPVwiYXJjLmNsYXNzXCJcbiAgICAgICAgW3N0YXJ0QW5nbGVdPVwiYXJjLnN0YXJ0QW5nbGVcIlxuICAgICAgICBbZW5kQW5nbGVdPVwiYXJjLmVuZEFuZ2xlXCJcbiAgICAgICAgW2lubmVyUmFkaXVzXT1cImlubmVyUmFkaXVzXCJcbiAgICAgICAgW291dGVyUmFkaXVzXT1cIm91dGVyUmFkaXVzXCJcbiAgICAgICAgW2ZpbGxdPVwiY29sb3IoYXJjKVwiXG4gICAgICAgIFt2YWx1ZV09XCJhcmMuZGF0YS52YWx1ZVwiXG4gICAgICAgIFtkYXRhXT1cImFyYy5kYXRhXCJcbiAgICAgICAgW2dyYWRpZW50XT1cImZhbHNlXCJcbiAgICAgICAgW3BvaW50ZXJFdmVudHNdPVwiYXJjLnBvaW50ZXJFdmVudHNcIlxuICAgICAgICBbYW5pbWF0ZV09XCJhcmMuYW5pbWF0ZVwiXG4gICAgICAgIChzZWxlY3QpPVwib25DbGljaygkZXZlbnQpXCJcbiAgICAgICAgKGFjdGl2YXRlKT1cImFjdGl2YXRlLmVtaXQoJGV2ZW50KVwiXG4gICAgICAgIChkZWFjdGl2YXRlKT1cImRlYWN0aXZhdGUuZW1pdCgkZXZlbnQpXCJcbiAgICAgID48L3N2ZzpnPlxuICAgIDwvc3ZnOmc+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIFBpZUdyaWRTZXJpZXNDb21wb25lbnQgaW1wbGVtZW50cyBPbkNoYW5nZXMge1xuICBASW5wdXQoKSBjb2xvcnM7XG4gIEBJbnB1dCgpIGRhdGE7XG4gIEBJbnB1dCgpIGlubmVyUmFkaXVzID0gNzA7XG4gIEBJbnB1dCgpIG91dGVyUmFkaXVzID0gODA7XG4gIEBJbnB1dCgpIGFuaW1hdGlvbnM6IGJvb2xlYW4gPSB0cnVlO1xuXG4gIEBPdXRwdXQoKSBzZWxlY3QgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBhY3RpdmF0ZSA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcbiAgQE91dHB1dCgpIGRlYWN0aXZhdGUgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgZWxlbWVudDogSFRNTEVsZW1lbnQ7XG4gIGxheW91dDogYW55O1xuICBhcmNzOiBhbnk7XG5cbiAgY29uc3RydWN0b3IoZWxlbWVudDogRWxlbWVudFJlZikge1xuICAgIHRoaXMuZWxlbWVudCA9IGVsZW1lbnQubmF0aXZlRWxlbWVudDtcbiAgfVxuXG4gIG5nT25DaGFuZ2VzKGNoYW5nZXM6IFNpbXBsZUNoYW5nZXMpOiB2b2lkIHtcbiAgICB0aGlzLnVwZGF0ZSgpO1xuICB9XG5cbiAgdXBkYXRlKCk6IHZvaWQge1xuICAgIHRoaXMubGF5b3V0ID0gcGllPGFueSwgYW55PigpXG4gICAgICAudmFsdWUoZCA9PiBkLmRhdGEudmFsdWUpXG4gICAgICAuc29ydChudWxsKTtcblxuICAgIHRoaXMuYXJjcyA9IHRoaXMuZ2V0QXJjcygpO1xuICB9XG5cbiAgZ2V0QXJjcygpOiBhbnlbXSB7XG4gICAgcmV0dXJuIHRoaXMubGF5b3V0KHRoaXMuZGF0YSkubWFwKChhcmMsIGluZGV4KSA9PiB7XG4gICAgICBjb25zdCBsYWJlbCA9IGFyYy5kYXRhLmRhdGEubmFtZTtcbiAgICAgIGNvbnN0IG90aGVyID0gYXJjLmRhdGEuZGF0YS5vdGhlcjtcblxuICAgICAgaWYgKGluZGV4ID09PSAwKSB7XG4gICAgICAgIGFyYy5zdGFydEFuZ2xlID0gMDtcbiAgICAgIH1cblxuICAgICAgY29uc3QgY29sb3IgPSB0aGlzLmNvbG9ycyhsYWJlbCk7XG4gICAgICByZXR1cm4ge1xuICAgICAgICBkYXRhOiBhcmMuZGF0YS5kYXRhLFxuICAgICAgICBjbGFzczogJ2FyYyAnICsgJ2FyYycgKyBpbmRleCxcbiAgICAgICAgZmlsbDogY29sb3IsXG4gICAgICAgIHN0YXJ0QW5nbGU6IG90aGVyID8gMCA6IGFyYy5zdGFydEFuZ2xlLFxuICAgICAgICBlbmRBbmdsZTogYXJjLmVuZEFuZ2xlLFxuICAgICAgICBhbmltYXRlOiB0aGlzLmFuaW1hdGlvbnMgJiYgIW90aGVyLFxuICAgICAgICBwb2ludGVyRXZlbnRzOiAhb3RoZXJcbiAgICAgIH07XG4gICAgfSk7XG4gIH1cblxuICBvbkNsaWNrKGRhdGEpOiB2b2lkIHtcbiAgICB0aGlzLnNlbGVjdC5lbWl0KHRoaXMuZGF0YVswXS5kYXRhKTtcbiAgfVxuXG4gIHRyYWNrQnkoaW5kZXgsIGl0ZW0pOiBzdHJpbmcge1xuICAgIHJldHVybiBpdGVtLmRhdGEubmFtZTtcbiAgfVxuXG4gIGxhYmVsKGFyYyk6IHN0cmluZyB7XG4gICAgcmV0dXJuIGFyYy5kYXRhLm5hbWU7XG4gIH1cblxuICBjb2xvcihhcmMpOiBhbnkge1xuICAgIHJldHVybiB0aGlzLmNvbG9ycyh0aGlzLmxhYmVsKGFyYykpO1xuICB9XG59XG4iXX0=