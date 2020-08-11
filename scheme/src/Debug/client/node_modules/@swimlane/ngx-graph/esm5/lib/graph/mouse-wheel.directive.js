import { __decorate, __metadata } from "tslib";
import { Directive, Output, HostListener, EventEmitter } from '@angular/core';
/**
 * Mousewheel directive
 * https://github.com/SodhanaLibrary/angular2-examples/blob/master/app/mouseWheelDirective/mousewheel.directive.ts
 *
 * @export
 */
// tslint:disable-next-line: directive-selector
var MouseWheelDirective = /** @class */ (function () {
    function MouseWheelDirective() {
        this.mouseWheelUp = new EventEmitter();
        this.mouseWheelDown = new EventEmitter();
    }
    MouseWheelDirective.prototype.onMouseWheelChrome = function (event) {
        this.mouseWheelFunc(event);
    };
    MouseWheelDirective.prototype.onMouseWheelFirefox = function (event) {
        this.mouseWheelFunc(event);
    };
    MouseWheelDirective.prototype.onWheel = function (event) {
        this.mouseWheelFunc(event);
    };
    MouseWheelDirective.prototype.onMouseWheelIE = function (event) {
        this.mouseWheelFunc(event);
    };
    MouseWheelDirective.prototype.mouseWheelFunc = function (event) {
        if (window.event) {
            event = window.event;
        }
        var delta = Math.max(-1, Math.min(1, event.wheelDelta || -event.detail || event.deltaY || event.deltaX));
        // Firefox don't have native support for wheel event, as a result delta values are reverse
        var isWheelMouseUp = event.wheelDelta ? delta > 0 : delta < 0;
        var isWheelMouseDown = event.wheelDelta ? delta < 0 : delta > 0;
        if (isWheelMouseUp) {
            this.mouseWheelUp.emit(event);
        }
        else if (isWheelMouseDown) {
            this.mouseWheelDown.emit(event);
        }
        // for IE
        event.returnValue = false;
        // for Chrome and Firefox
        if (event.preventDefault) {
            event.preventDefault();
        }
    };
    __decorate([
        Output(),
        __metadata("design:type", Object)
    ], MouseWheelDirective.prototype, "mouseWheelUp", void 0);
    __decorate([
        Output(),
        __metadata("design:type", Object)
    ], MouseWheelDirective.prototype, "mouseWheelDown", void 0);
    __decorate([
        HostListener('mousewheel', ['$event']),
        __metadata("design:type", Function),
        __metadata("design:paramtypes", [Object]),
        __metadata("design:returntype", void 0)
    ], MouseWheelDirective.prototype, "onMouseWheelChrome", null);
    __decorate([
        HostListener('DOMMouseScroll', ['$event']),
        __metadata("design:type", Function),
        __metadata("design:paramtypes", [Object]),
        __metadata("design:returntype", void 0)
    ], MouseWheelDirective.prototype, "onMouseWheelFirefox", null);
    __decorate([
        HostListener('wheel', ['$event']),
        __metadata("design:type", Function),
        __metadata("design:paramtypes", [Object]),
        __metadata("design:returntype", void 0)
    ], MouseWheelDirective.prototype, "onWheel", null);
    __decorate([
        HostListener('onmousewheel', ['$event']),
        __metadata("design:type", Function),
        __metadata("design:paramtypes", [Object]),
        __metadata("design:returntype", void 0)
    ], MouseWheelDirective.prototype, "onMouseWheelIE", null);
    MouseWheelDirective = __decorate([
        Directive({ selector: '[mouseWheel]' })
    ], MouseWheelDirective);
    return MouseWheelDirective;
}());
export { MouseWheelDirective };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibW91c2Utd2hlZWwuZGlyZWN0aXZlLmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1ncmFwaC8iLCJzb3VyY2VzIjpbImxpYi9ncmFwaC9tb3VzZS13aGVlbC5kaXJlY3RpdmUudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFBRSxTQUFTLEVBQUUsTUFBTSxFQUFFLFlBQVksRUFBRSxZQUFZLEVBQUUsTUFBTSxlQUFlLENBQUM7QUFFOUU7Ozs7O0dBS0c7QUFDSCwrQ0FBK0M7QUFFL0M7SUFBQTtRQUVFLGlCQUFZLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUVsQyxtQkFBYyxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7SUE2Q3RDLENBQUM7SUExQ0MsZ0RBQWtCLEdBQWxCLFVBQW1CLEtBQVU7UUFDM0IsSUFBSSxDQUFDLGNBQWMsQ0FBQyxLQUFLLENBQUMsQ0FBQztJQUM3QixDQUFDO0lBR0QsaURBQW1CLEdBQW5CLFVBQW9CLEtBQVU7UUFDNUIsSUFBSSxDQUFDLGNBQWMsQ0FBQyxLQUFLLENBQUMsQ0FBQztJQUM3QixDQUFDO0lBR0QscUNBQU8sR0FBUCxVQUFRLEtBQVU7UUFDaEIsSUFBSSxDQUFDLGNBQWMsQ0FBQyxLQUFLLENBQUMsQ0FBQztJQUM3QixDQUFDO0lBR0QsNENBQWMsR0FBZCxVQUFlLEtBQVU7UUFDdkIsSUFBSSxDQUFDLGNBQWMsQ0FBQyxLQUFLLENBQUMsQ0FBQztJQUM3QixDQUFDO0lBRUQsNENBQWMsR0FBZCxVQUFlLEtBQVU7UUFDdkIsSUFBSSxNQUFNLENBQUMsS0FBSyxFQUFFO1lBQ2hCLEtBQUssR0FBRyxNQUFNLENBQUMsS0FBSyxDQUFDO1NBQ3RCO1FBRUQsSUFBTSxLQUFLLEdBQVcsSUFBSSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsRUFBRSxJQUFJLENBQUMsR0FBRyxDQUFDLENBQUMsRUFBRSxLQUFLLENBQUMsVUFBVSxJQUFJLENBQUMsS0FBSyxDQUFDLE1BQU0sSUFBSSxLQUFLLENBQUMsTUFBTSxJQUFJLEtBQUssQ0FBQyxNQUFNLENBQUMsQ0FBQyxDQUFDO1FBQ25ILDBGQUEwRjtRQUMxRixJQUFNLGNBQWMsR0FBWSxLQUFLLENBQUMsVUFBVSxDQUFDLENBQUMsQ0FBQyxLQUFLLEdBQUcsQ0FBQyxDQUFDLENBQUMsQ0FBQyxLQUFLLEdBQUcsQ0FBQyxDQUFDO1FBQ3pFLElBQU0sZ0JBQWdCLEdBQVksS0FBSyxDQUFDLFVBQVUsQ0FBQyxDQUFDLENBQUMsS0FBSyxHQUFHLENBQUMsQ0FBQyxDQUFDLENBQUMsS0FBSyxHQUFHLENBQUMsQ0FBQztRQUMzRSxJQUFJLGNBQWMsRUFBRTtZQUNsQixJQUFJLENBQUMsWUFBWSxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsQ0FBQztTQUMvQjthQUFNLElBQUksZ0JBQWdCLEVBQUU7WUFDM0IsSUFBSSxDQUFDLGNBQWMsQ0FBQyxJQUFJLENBQUMsS0FBSyxDQUFDLENBQUM7U0FDakM7UUFFRCxTQUFTO1FBQ1QsS0FBSyxDQUFDLFdBQVcsR0FBRyxLQUFLLENBQUM7UUFFMUIseUJBQXlCO1FBQ3pCLElBQUksS0FBSyxDQUFDLGNBQWMsRUFBRTtZQUN4QixLQUFLLENBQUMsY0FBYyxFQUFFLENBQUM7U0FDeEI7SUFDSCxDQUFDO0lBOUNEO1FBREMsTUFBTSxFQUFFOzs2REFDeUI7SUFFbEM7UUFEQyxNQUFNLEVBQUU7OytEQUMyQjtJQUdwQztRQURDLFlBQVksQ0FBQyxZQUFZLEVBQUUsQ0FBQyxRQUFRLENBQUMsQ0FBQzs7OztpRUFHdEM7SUFHRDtRQURDLFlBQVksQ0FBQyxnQkFBZ0IsRUFBRSxDQUFDLFFBQVEsQ0FBQyxDQUFDOzs7O2tFQUcxQztJQUdEO1FBREMsWUFBWSxDQUFDLE9BQU8sRUFBRSxDQUFDLFFBQVEsQ0FBQyxDQUFDOzs7O3NEQUdqQztJQUdEO1FBREMsWUFBWSxDQUFDLGNBQWMsRUFBRSxDQUFDLFFBQVEsQ0FBQyxDQUFDOzs7OzZEQUd4QztJQXhCVSxtQkFBbUI7UUFEL0IsU0FBUyxDQUFDLEVBQUUsUUFBUSxFQUFFLGNBQWMsRUFBRSxDQUFDO09BQzNCLG1CQUFtQixDQWlEL0I7SUFBRCwwQkFBQztDQUFBLEFBakRELElBaURDO1NBakRZLG1CQUFtQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IERpcmVjdGl2ZSwgT3V0cHV0LCBIb3N0TGlzdGVuZXIsIEV2ZW50RW1pdHRlciB9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuXG4vKipcbiAqIE1vdXNld2hlZWwgZGlyZWN0aXZlXG4gKiBodHRwczovL2dpdGh1Yi5jb20vU29kaGFuYUxpYnJhcnkvYW5ndWxhcjItZXhhbXBsZXMvYmxvYi9tYXN0ZXIvYXBwL21vdXNlV2hlZWxEaXJlY3RpdmUvbW91c2V3aGVlbC5kaXJlY3RpdmUudHNcbiAqXG4gKiBAZXhwb3J0XG4gKi9cbi8vIHRzbGludDpkaXNhYmxlLW5leHQtbGluZTogZGlyZWN0aXZlLXNlbGVjdG9yXG5ARGlyZWN0aXZlKHsgc2VsZWN0b3I6ICdbbW91c2VXaGVlbF0nIH0pXG5leHBvcnQgY2xhc3MgTW91c2VXaGVlbERpcmVjdGl2ZSB7XG4gIEBPdXRwdXQoKVxuICBtb3VzZVdoZWVsVXAgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKVxuICBtb3VzZVdoZWVsRG93biA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcblxuICBASG9zdExpc3RlbmVyKCdtb3VzZXdoZWVsJywgWyckZXZlbnQnXSlcbiAgb25Nb3VzZVdoZWVsQ2hyb21lKGV2ZW50OiBhbnkpOiB2b2lkIHtcbiAgICB0aGlzLm1vdXNlV2hlZWxGdW5jKGV2ZW50KTtcbiAgfVxuXG4gIEBIb3N0TGlzdGVuZXIoJ0RPTU1vdXNlU2Nyb2xsJywgWyckZXZlbnQnXSlcbiAgb25Nb3VzZVdoZWVsRmlyZWZveChldmVudDogYW55KTogdm9pZCB7XG4gICAgdGhpcy5tb3VzZVdoZWVsRnVuYyhldmVudCk7XG4gIH1cblxuICBASG9zdExpc3RlbmVyKCd3aGVlbCcsIFsnJGV2ZW50J10pXG4gIG9uV2hlZWwoZXZlbnQ6IGFueSk6IHZvaWQge1xuICAgIHRoaXMubW91c2VXaGVlbEZ1bmMoZXZlbnQpO1xuICB9XG5cbiAgQEhvc3RMaXN0ZW5lcignb25tb3VzZXdoZWVsJywgWyckZXZlbnQnXSlcbiAgb25Nb3VzZVdoZWVsSUUoZXZlbnQ6IGFueSk6IHZvaWQge1xuICAgIHRoaXMubW91c2VXaGVlbEZ1bmMoZXZlbnQpO1xuICB9XG5cbiAgbW91c2VXaGVlbEZ1bmMoZXZlbnQ6IGFueSk6IHZvaWQge1xuICAgIGlmICh3aW5kb3cuZXZlbnQpIHtcbiAgICAgIGV2ZW50ID0gd2luZG93LmV2ZW50O1xuICAgIH1cblxuICAgIGNvbnN0IGRlbHRhOiBudW1iZXIgPSBNYXRoLm1heCgtMSwgTWF0aC5taW4oMSwgZXZlbnQud2hlZWxEZWx0YSB8fCAtZXZlbnQuZGV0YWlsIHx8IGV2ZW50LmRlbHRhWSB8fCBldmVudC5kZWx0YVgpKTtcbiAgICAvLyBGaXJlZm94IGRvbid0IGhhdmUgbmF0aXZlIHN1cHBvcnQgZm9yIHdoZWVsIGV2ZW50LCBhcyBhIHJlc3VsdCBkZWx0YSB2YWx1ZXMgYXJlIHJldmVyc2VcbiAgICBjb25zdCBpc1doZWVsTW91c2VVcDogYm9vbGVhbiA9IGV2ZW50LndoZWVsRGVsdGEgPyBkZWx0YSA+IDAgOiBkZWx0YSA8IDA7XG4gICAgY29uc3QgaXNXaGVlbE1vdXNlRG93bjogYm9vbGVhbiA9IGV2ZW50LndoZWVsRGVsdGEgPyBkZWx0YSA8IDAgOiBkZWx0YSA+IDA7XG4gICAgaWYgKGlzV2hlZWxNb3VzZVVwKSB7XG4gICAgICB0aGlzLm1vdXNlV2hlZWxVcC5lbWl0KGV2ZW50KTtcbiAgICB9IGVsc2UgaWYgKGlzV2hlZWxNb3VzZURvd24pIHtcbiAgICAgIHRoaXMubW91c2VXaGVlbERvd24uZW1pdChldmVudCk7XG4gICAgfVxuXG4gICAgLy8gZm9yIElFXG4gICAgZXZlbnQucmV0dXJuVmFsdWUgPSBmYWxzZTtcblxuICAgIC8vIGZvciBDaHJvbWUgYW5kIEZpcmVmb3hcbiAgICBpZiAoZXZlbnQucHJldmVudERlZmF1bHQpIHtcbiAgICAgIGV2ZW50LnByZXZlbnREZWZhdWx0KCk7XG4gICAgfVxuICB9XG59XG4iXX0=