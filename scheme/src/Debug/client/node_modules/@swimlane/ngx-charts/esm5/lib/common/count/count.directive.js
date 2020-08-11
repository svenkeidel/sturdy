import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectorRef, OnDestroy, ElementRef } from '@angular/core';
import { count, decimalChecker } from './count.helper';
/**
 * Count up component
 *
 * Loosely inspired by:
 *  - https://github.com/izupet/angular2-counto
 *  - https://inorganik.github.io/countUp.js/
 *
 * @export
 */
var CountUpDirective = /** @class */ (function () {
    function CountUpDirective(cd, element) {
        this.cd = cd;
        this.countDuration = 1;
        this.countPrefix = '';
        this.countSuffix = '';
        this.countChange = new EventEmitter();
        this.countFinish = new EventEmitter();
        this.value = '';
        this._countDecimals = 0;
        this._countTo = 0;
        this._countFrom = 0;
        this.nativeElement = element.nativeElement;
    }
    Object.defineProperty(CountUpDirective.prototype, "countDecimals", {
        get: function () {
            if (this._countDecimals)
                return this._countDecimals;
            return decimalChecker(this.countTo);
        },
        set: function (val) {
            this._countDecimals = val;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(CountUpDirective.prototype, "countTo", {
        get: function () {
            return this._countTo;
        },
        set: function (val) {
            this._countTo = parseFloat(val);
            this.start();
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(CountUpDirective.prototype, "countFrom", {
        get: function () {
            return this._countFrom;
        },
        set: function (val) {
            this._countFrom = parseFloat(val);
            this.start();
        },
        enumerable: true,
        configurable: true
    });
    CountUpDirective.prototype.ngOnDestroy = function () {
        cancelAnimationFrame(this.animationReq);
    };
    CountUpDirective.prototype.start = function () {
        var _this = this;
        cancelAnimationFrame(this.animationReq);
        var valueFormatting = this.valueFormatting || (function (value) { return "" + _this.countPrefix + value.toLocaleString() + _this.countSuffix; });
        var callback = function (_a) {
            var value = _a.value, progress = _a.progress, finished = _a.finished;
            _this.value = valueFormatting(value);
            _this.cd.markForCheck();
            if (!finished)
                _this.countChange.emit({ value: _this.value, progress: progress });
            if (finished)
                _this.countFinish.emit({ value: _this.value, progress: progress });
        };
        this.animationReq = count(this.countFrom, this.countTo, this.countDecimals, this.countDuration, callback);
    };
    CountUpDirective.ctorParameters = function () { return [
        { type: ChangeDetectorRef },
        { type: ElementRef }
    ]; };
    __decorate([
        Input()
    ], CountUpDirective.prototype, "countDuration", void 0);
    __decorate([
        Input()
    ], CountUpDirective.prototype, "countPrefix", void 0);
    __decorate([
        Input()
    ], CountUpDirective.prototype, "countSuffix", void 0);
    __decorate([
        Input()
    ], CountUpDirective.prototype, "valueFormatting", void 0);
    __decorate([
        Input()
    ], CountUpDirective.prototype, "countDecimals", null);
    __decorate([
        Input()
    ], CountUpDirective.prototype, "countTo", null);
    __decorate([
        Input()
    ], CountUpDirective.prototype, "countFrom", null);
    __decorate([
        Output()
    ], CountUpDirective.prototype, "countChange", void 0);
    __decorate([
        Output()
    ], CountUpDirective.prototype, "countFinish", void 0);
    CountUpDirective = __decorate([
        Component({
            selector: '[ngx-charts-count-up]',
            template: "\n    {{ value }}\n  "
        })
    ], CountUpDirective);
    return CountUpDirective;
}());
export { CountUpDirective };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY291bnQuZGlyZWN0aXZlLmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvY29tbW9uL2NvdW50L2NvdW50LmRpcmVjdGl2ZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUFFLFNBQVMsRUFBRSxLQUFLLEVBQUUsTUFBTSxFQUFFLFlBQVksRUFBRSxpQkFBaUIsRUFBRSxTQUFTLEVBQUUsVUFBVSxFQUFFLE1BQU0sZUFBZSxDQUFDO0FBQ2pILE9BQU8sRUFBRSxLQUFLLEVBQUUsY0FBYyxFQUFFLE1BQU0sZ0JBQWdCLENBQUM7QUFFdkQ7Ozs7Ozs7O0dBUUc7QUFPSDtJQWtERSwwQkFBb0IsRUFBcUIsRUFBRSxPQUFtQjtRQUExQyxPQUFFLEdBQUYsRUFBRSxDQUFtQjtRQWpEaEMsa0JBQWEsR0FBVyxDQUFDLENBQUM7UUFDMUIsZ0JBQVcsR0FBVyxFQUFFLENBQUM7UUFDekIsZ0JBQVcsR0FBVyxFQUFFLENBQUM7UUFpQ3hCLGdCQUFXLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUNqQyxnQkFBVyxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFJM0MsVUFBSyxHQUFRLEVBQUUsQ0FBQztRQUtSLG1CQUFjLEdBQVcsQ0FBQyxDQUFDO1FBQzNCLGFBQVEsR0FBVyxDQUFDLENBQUM7UUFDckIsZUFBVSxHQUFXLENBQUMsQ0FBQztRQUc3QixJQUFJLENBQUMsYUFBYSxHQUFHLE9BQU8sQ0FBQyxhQUFhLENBQUM7SUFDN0MsQ0FBQztJQTdDRCxzQkFBSSwyQ0FBYTthQUlqQjtZQUNFLElBQUksSUFBSSxDQUFDLGNBQWM7Z0JBQUUsT0FBTyxJQUFJLENBQUMsY0FBYyxDQUFDO1lBQ3BELE9BQU8sY0FBYyxDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsQ0FBQztRQUN0QyxDQUFDO2FBUEQsVUFBa0IsR0FBVztZQUMzQixJQUFJLENBQUMsY0FBYyxHQUFHLEdBQUcsQ0FBQztRQUM1QixDQUFDOzs7T0FBQTtJQVFELHNCQUFJLHFDQUFPO2FBS1g7WUFDRSxPQUFPLElBQUksQ0FBQyxRQUFRLENBQUM7UUFDdkIsQ0FBQzthQVBELFVBQVksR0FBRztZQUNiLElBQUksQ0FBQyxRQUFRLEdBQUcsVUFBVSxDQUFDLEdBQUcsQ0FBQyxDQUFDO1lBQ2hDLElBQUksQ0FBQyxLQUFLLEVBQUUsQ0FBQztRQUNmLENBQUM7OztPQUFBO0lBT0Qsc0JBQUksdUNBQVM7YUFLYjtZQUNFLE9BQU8sSUFBSSxDQUFDLFVBQVUsQ0FBQztRQUN6QixDQUFDO2FBUEQsVUFBYyxHQUFHO1lBQ2YsSUFBSSxDQUFDLFVBQVUsR0FBRyxVQUFVLENBQUMsR0FBRyxDQUFDLENBQUM7WUFDbEMsSUFBSSxDQUFDLEtBQUssRUFBRSxDQUFDO1FBQ2YsQ0FBQzs7O09BQUE7SUF3QkQsc0NBQVcsR0FBWDtRQUNFLG9CQUFvQixDQUFDLElBQUksQ0FBQyxZQUFZLENBQUMsQ0FBQztJQUMxQyxDQUFDO0lBRUQsZ0NBQUssR0FBTDtRQUFBLGlCQWNDO1FBYkMsb0JBQW9CLENBQUMsSUFBSSxDQUFDLFlBQVksQ0FBQyxDQUFDO1FBRXhDLElBQU0sZUFBZSxHQUNuQixJQUFJLENBQUMsZUFBZSxJQUFJLENBQUMsVUFBQSxLQUFLLElBQUksT0FBQSxLQUFHLEtBQUksQ0FBQyxXQUFXLEdBQUcsS0FBSyxDQUFDLGNBQWMsRUFBRSxHQUFHLEtBQUksQ0FBQyxXQUFhLEVBQWpFLENBQWlFLENBQUMsQ0FBQztRQUV2RyxJQUFNLFFBQVEsR0FBRyxVQUFDLEVBQTZCO2dCQUEzQixnQkFBSyxFQUFFLHNCQUFRLEVBQUUsc0JBQVE7WUFDM0MsS0FBSSxDQUFDLEtBQUssR0FBRyxlQUFlLENBQUMsS0FBSyxDQUFDLENBQUM7WUFDcEMsS0FBSSxDQUFDLEVBQUUsQ0FBQyxZQUFZLEVBQUUsQ0FBQztZQUN2QixJQUFJLENBQUMsUUFBUTtnQkFBRSxLQUFJLENBQUMsV0FBVyxDQUFDLElBQUksQ0FBQyxFQUFFLEtBQUssRUFBRSxLQUFJLENBQUMsS0FBSyxFQUFFLFFBQVEsVUFBQSxFQUFFLENBQUMsQ0FBQztZQUN0RSxJQUFJLFFBQVE7Z0JBQUUsS0FBSSxDQUFDLFdBQVcsQ0FBQyxJQUFJLENBQUMsRUFBRSxLQUFLLEVBQUUsS0FBSSxDQUFDLEtBQUssRUFBRSxRQUFRLFVBQUEsRUFBRSxDQUFDLENBQUM7UUFDdkUsQ0FBQyxDQUFDO1FBRUYsSUFBSSxDQUFDLFlBQVksR0FBRyxLQUFLLENBQUMsSUFBSSxDQUFDLFNBQVMsRUFBRSxJQUFJLENBQUMsT0FBTyxFQUFFLElBQUksQ0FBQyxhQUFhLEVBQUUsSUFBSSxDQUFDLGFBQWEsRUFBRSxRQUFRLENBQUMsQ0FBQztJQUM1RyxDQUFDOztnQkF0QnVCLGlCQUFpQjtnQkFBVyxVQUFVOztJQWpEckQ7UUFBUixLQUFLLEVBQUU7MkRBQTJCO0lBQzFCO1FBQVIsS0FBSyxFQUFFO3lEQUEwQjtJQUN6QjtRQUFSLEtBQUssRUFBRTt5REFBMEI7SUFDekI7UUFBUixLQUFLLEVBQUU7NkRBQXNCO0lBRzlCO1FBREMsS0FBSyxFQUFFO3lEQUdQO0lBUUQ7UUFEQyxLQUFLLEVBQUU7bURBSVA7SUFPRDtRQURDLEtBQUssRUFBRTtxREFJUDtJQU1TO1FBQVQsTUFBTSxFQUFFO3lEQUFrQztJQUNqQztRQUFULE1BQU0sRUFBRTt5REFBa0M7SUFyQ2hDLGdCQUFnQjtRQU41QixTQUFTLENBQUM7WUFDVCxRQUFRLEVBQUUsdUJBQXVCO1lBQ2pDLFFBQVEsRUFBRSx1QkFFVDtTQUNGLENBQUM7T0FDVyxnQkFBZ0IsQ0F5RTVCO0lBQUQsdUJBQUM7Q0FBQSxBQXpFRCxJQXlFQztTQXpFWSxnQkFBZ0IiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgeyBDb21wb25lbnQsIElucHV0LCBPdXRwdXQsIEV2ZW50RW1pdHRlciwgQ2hhbmdlRGV0ZWN0b3JSZWYsIE9uRGVzdHJveSwgRWxlbWVudFJlZiB9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHsgY291bnQsIGRlY2ltYWxDaGVja2VyIH0gZnJvbSAnLi9jb3VudC5oZWxwZXInO1xuXG4vKipcbiAqIENvdW50IHVwIGNvbXBvbmVudFxuICpcbiAqIExvb3NlbHkgaW5zcGlyZWQgYnk6XG4gKiAgLSBodHRwczovL2dpdGh1Yi5jb20vaXp1cGV0L2FuZ3VsYXIyLWNvdW50b1xuICogIC0gaHR0cHM6Ly9pbm9yZ2FuaWsuZ2l0aHViLmlvL2NvdW50VXAuanMvXG4gKlxuICogQGV4cG9ydFxuICovXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdbbmd4LWNoYXJ0cy1jb3VudC11cF0nLFxuICB0ZW1wbGF0ZTogYFxuICAgIHt7IHZhbHVlIH19XG4gIGBcbn0pXG5leHBvcnQgY2xhc3MgQ291bnRVcERpcmVjdGl2ZSBpbXBsZW1lbnRzIE9uRGVzdHJveSB7XG4gIEBJbnB1dCgpIGNvdW50RHVyYXRpb246IG51bWJlciA9IDE7XG4gIEBJbnB1dCgpIGNvdW50UHJlZml4OiBzdHJpbmcgPSAnJztcbiAgQElucHV0KCkgY291bnRTdWZmaXg6IHN0cmluZyA9ICcnO1xuICBASW5wdXQoKSB2YWx1ZUZvcm1hdHRpbmc6IGFueTtcblxuICBASW5wdXQoKVxuICBzZXQgY291bnREZWNpbWFscyh2YWw6IG51bWJlcikge1xuICAgIHRoaXMuX2NvdW50RGVjaW1hbHMgPSB2YWw7XG4gIH1cblxuICBnZXQgY291bnREZWNpbWFscygpOiBudW1iZXIge1xuICAgIGlmICh0aGlzLl9jb3VudERlY2ltYWxzKSByZXR1cm4gdGhpcy5fY291bnREZWNpbWFscztcbiAgICByZXR1cm4gZGVjaW1hbENoZWNrZXIodGhpcy5jb3VudFRvKTtcbiAgfVxuXG4gIEBJbnB1dCgpXG4gIHNldCBjb3VudFRvKHZhbCkge1xuICAgIHRoaXMuX2NvdW50VG8gPSBwYXJzZUZsb2F0KHZhbCk7XG4gICAgdGhpcy5zdGFydCgpO1xuICB9XG5cbiAgZ2V0IGNvdW50VG8oKTogYW55IHtcbiAgICByZXR1cm4gdGhpcy5fY291bnRUbztcbiAgfVxuXG4gIEBJbnB1dCgpXG4gIHNldCBjb3VudEZyb20odmFsKSB7XG4gICAgdGhpcy5fY291bnRGcm9tID0gcGFyc2VGbG9hdCh2YWwpO1xuICAgIHRoaXMuc3RhcnQoKTtcbiAgfVxuXG4gIGdldCBjb3VudEZyb20oKTogYW55IHtcbiAgICByZXR1cm4gdGhpcy5fY291bnRGcm9tO1xuICB9XG5cbiAgQE91dHB1dCgpIGNvdW50Q2hhbmdlID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuICBAT3V0cHV0KCkgY291bnRGaW5pc2ggPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgbmF0aXZlRWxlbWVudDogYW55O1xuXG4gIHZhbHVlOiBhbnkgPSAnJztcbiAgZm9ybWF0dGVkVmFsdWU6IHN0cmluZztcblxuICBwcml2YXRlIGFuaW1hdGlvblJlcTogYW55O1xuXG4gIHByaXZhdGUgX2NvdW50RGVjaW1hbHM6IG51bWJlciA9IDA7XG4gIHByaXZhdGUgX2NvdW50VG86IG51bWJlciA9IDA7XG4gIHByaXZhdGUgX2NvdW50RnJvbTogbnVtYmVyID0gMDtcblxuICBjb25zdHJ1Y3Rvcihwcml2YXRlIGNkOiBDaGFuZ2VEZXRlY3RvclJlZiwgZWxlbWVudDogRWxlbWVudFJlZikge1xuICAgIHRoaXMubmF0aXZlRWxlbWVudCA9IGVsZW1lbnQubmF0aXZlRWxlbWVudDtcbiAgfVxuXG4gIG5nT25EZXN0cm95KCk6IHZvaWQge1xuICAgIGNhbmNlbEFuaW1hdGlvbkZyYW1lKHRoaXMuYW5pbWF0aW9uUmVxKTtcbiAgfVxuXG4gIHN0YXJ0KCk6IHZvaWQge1xuICAgIGNhbmNlbEFuaW1hdGlvbkZyYW1lKHRoaXMuYW5pbWF0aW9uUmVxKTtcblxuICAgIGNvbnN0IHZhbHVlRm9ybWF0dGluZyA9XG4gICAgICB0aGlzLnZhbHVlRm9ybWF0dGluZyB8fCAodmFsdWUgPT4gYCR7dGhpcy5jb3VudFByZWZpeH0ke3ZhbHVlLnRvTG9jYWxlU3RyaW5nKCl9JHt0aGlzLmNvdW50U3VmZml4fWApO1xuXG4gICAgY29uc3QgY2FsbGJhY2sgPSAoeyB2YWx1ZSwgcHJvZ3Jlc3MsIGZpbmlzaGVkIH0pID0+IHtcbiAgICAgIHRoaXMudmFsdWUgPSB2YWx1ZUZvcm1hdHRpbmcodmFsdWUpO1xuICAgICAgdGhpcy5jZC5tYXJrRm9yQ2hlY2soKTtcbiAgICAgIGlmICghZmluaXNoZWQpIHRoaXMuY291bnRDaGFuZ2UuZW1pdCh7IHZhbHVlOiB0aGlzLnZhbHVlLCBwcm9ncmVzcyB9KTtcbiAgICAgIGlmIChmaW5pc2hlZCkgdGhpcy5jb3VudEZpbmlzaC5lbWl0KHsgdmFsdWU6IHRoaXMudmFsdWUsIHByb2dyZXNzIH0pO1xuICAgIH07XG5cbiAgICB0aGlzLmFuaW1hdGlvblJlcSA9IGNvdW50KHRoaXMuY291bnRGcm9tLCB0aGlzLmNvdW50VG8sIHRoaXMuY291bnREZWNpbWFscywgdGhpcy5jb3VudER1cmF0aW9uLCBjYWxsYmFjayk7XG4gIH1cbn1cbiJdfQ==