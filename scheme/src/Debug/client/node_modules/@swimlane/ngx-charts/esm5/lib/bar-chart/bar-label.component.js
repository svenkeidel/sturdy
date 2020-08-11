import { __decorate } from "tslib";
import { Component, Input, OnChanges, SimpleChanges, ChangeDetectionStrategy, ElementRef, Output, EventEmitter } from '@angular/core';
import { formatLabel } from '../common/label.helper';
var BarLabelComponent = /** @class */ (function () {
    function BarLabelComponent(element) {
        this.dimensionsChanged = new EventEmitter();
        this.horizontalPadding = 2;
        this.verticalPadding = 5;
        this.element = element.nativeElement;
    }
    BarLabelComponent.prototype.ngOnChanges = function (changes) {
        this.update();
    };
    BarLabelComponent.prototype.getSize = function () {
        var h = this.element.getBoundingClientRect().height;
        var w = this.element.getBoundingClientRect().width;
        return { height: h, width: w, negative: this.value < 0 };
    };
    BarLabelComponent.prototype.ngAfterViewInit = function () {
        this.dimensionsChanged.emit(this.getSize());
    };
    BarLabelComponent.prototype.update = function () {
        if (this.valueFormatting) {
            this.formatedValue = this.valueFormatting(this.value);
        }
        else {
            this.formatedValue = formatLabel(this.value);
        }
        if (this.orientation === 'horizontal') {
            this.x = this.barX + this.barWidth;
            // if the value is negative then it's on the left of the x0.
            // we need to put the data label in front of the bar
            if (this.value < 0) {
                this.x = this.x - this.horizontalPadding;
                this.textAnchor = 'end';
            }
            else {
                this.x = this.x + this.horizontalPadding;
                this.textAnchor = 'start';
            }
            this.y = this.barY + this.barHeight / 2;
        }
        else {
            // orientation must be "vertical"
            this.x = this.barX + this.barWidth / 2;
            this.y = this.barY + this.barHeight;
            if (this.value < 0) {
                this.y = this.y + this.verticalPadding;
                this.textAnchor = 'end';
            }
            else {
                this.y = this.y - this.verticalPadding;
                this.textAnchor = 'start';
            }
            this.transform = "rotate(-45, " + this.x + " , " + this.y + ")";
        }
    };
    BarLabelComponent.ctorParameters = function () { return [
        { type: ElementRef }
    ]; };
    __decorate([
        Input()
    ], BarLabelComponent.prototype, "value", void 0);
    __decorate([
        Input()
    ], BarLabelComponent.prototype, "valueFormatting", void 0);
    __decorate([
        Input()
    ], BarLabelComponent.prototype, "barX", void 0);
    __decorate([
        Input()
    ], BarLabelComponent.prototype, "barY", void 0);
    __decorate([
        Input()
    ], BarLabelComponent.prototype, "barWidth", void 0);
    __decorate([
        Input()
    ], BarLabelComponent.prototype, "barHeight", void 0);
    __decorate([
        Input()
    ], BarLabelComponent.prototype, "orientation", void 0);
    __decorate([
        Output()
    ], BarLabelComponent.prototype, "dimensionsChanged", void 0);
    BarLabelComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-bar-label]',
            template: "\n    <svg:text\n      class=\"textDataLabel\"\n      alignment-baseline=\"middle\"\n      [attr.text-anchor]=\"textAnchor\"\n      [attr.transform]=\"transform\"\n      [attr.x]=\"x\"\n      [attr.y]=\"y\"\n    >\n      {{ formatedValue }}\n    </svg:text>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush,
            styles: [".textDataLabel{font-size:11px}"]
        })
    ], BarLabelComponent);
    return BarLabelComponent;
}());
export { BarLabelComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYmFyLWxhYmVsLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2Jhci1jaGFydC9iYXItbGFiZWwuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQ0wsU0FBUyxFQUNULEtBQUssRUFDTCxTQUFTLEVBQ1QsYUFBYSxFQUNiLHVCQUF1QixFQUN2QixVQUFVLEVBQ1YsTUFBTSxFQUNOLFlBQVksRUFDYixNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUUsV0FBVyxFQUFFLE1BQU0sd0JBQXdCLENBQUM7QUFtQnJEO0lBb0JFLDJCQUFZLE9BQW1CO1FBWHJCLHNCQUFpQixHQUFzQixJQUFJLFlBQVksRUFBRSxDQUFDO1FBS3BFLHNCQUFpQixHQUFXLENBQUMsQ0FBQztRQUM5QixvQkFBZSxHQUFXLENBQUMsQ0FBQztRQU0xQixJQUFJLENBQUMsT0FBTyxHQUFHLE9BQU8sQ0FBQyxhQUFhLENBQUM7SUFDdkMsQ0FBQztJQUVELHVDQUFXLEdBQVgsVUFBWSxPQUFzQjtRQUNoQyxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUM7SUFDaEIsQ0FBQztJQUVELG1DQUFPLEdBQVA7UUFDRSxJQUFNLENBQUMsR0FBRyxJQUFJLENBQUMsT0FBTyxDQUFDLHFCQUFxQixFQUFFLENBQUMsTUFBTSxDQUFDO1FBQ3RELElBQU0sQ0FBQyxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUMscUJBQXFCLEVBQUUsQ0FBQyxLQUFLLENBQUM7UUFDckQsT0FBTyxFQUFFLE1BQU0sRUFBRSxDQUFDLEVBQUUsS0FBSyxFQUFFLENBQUMsRUFBRSxRQUFRLEVBQUUsSUFBSSxDQUFDLEtBQUssR0FBRyxDQUFDLEVBQUUsQ0FBQztJQUMzRCxDQUFDO0lBRUQsMkNBQWUsR0FBZjtRQUNFLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDLENBQUM7SUFDOUMsQ0FBQztJQUVELGtDQUFNLEdBQU47UUFDRSxJQUFJLElBQUksQ0FBQyxlQUFlLEVBQUU7WUFDeEIsSUFBSSxDQUFDLGFBQWEsR0FBRyxJQUFJLENBQUMsZUFBZSxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsQ0FBQztTQUN2RDthQUFNO1lBQ0wsSUFBSSxDQUFDLGFBQWEsR0FBRyxXQUFXLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxDQUFDO1NBQzlDO1FBRUQsSUFBSSxJQUFJLENBQUMsV0FBVyxLQUFLLFlBQVksRUFBRTtZQUNyQyxJQUFJLENBQUMsQ0FBQyxHQUFHLElBQUksQ0FBQyxJQUFJLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQztZQUNuQyw0REFBNEQ7WUFDNUQsb0RBQW9EO1lBQ3BELElBQUksSUFBSSxDQUFDLEtBQUssR0FBRyxDQUFDLEVBQUU7Z0JBQ2xCLElBQUksQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsaUJBQWlCLENBQUM7Z0JBQ3pDLElBQUksQ0FBQyxVQUFVLEdBQUcsS0FBSyxDQUFDO2FBQ3pCO2lCQUFNO2dCQUNMLElBQUksQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsaUJBQWlCLENBQUM7Z0JBQ3pDLElBQUksQ0FBQyxVQUFVLEdBQUcsT0FBTyxDQUFDO2FBQzNCO1lBQ0QsSUFBSSxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsSUFBSSxHQUFHLElBQUksQ0FBQyxTQUFTLEdBQUcsQ0FBQyxDQUFDO1NBQ3pDO2FBQU07WUFDTCxpQ0FBaUM7WUFDakMsSUFBSSxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsSUFBSSxHQUFHLElBQUksQ0FBQyxRQUFRLEdBQUcsQ0FBQyxDQUFDO1lBQ3ZDLElBQUksQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLElBQUksR0FBRyxJQUFJLENBQUMsU0FBUyxDQUFDO1lBRXBDLElBQUksSUFBSSxDQUFDLEtBQUssR0FBRyxDQUFDLEVBQUU7Z0JBQ2xCLElBQUksQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsZUFBZSxDQUFDO2dCQUN2QyxJQUFJLENBQUMsVUFBVSxHQUFHLEtBQUssQ0FBQzthQUN6QjtpQkFBTTtnQkFDTCxJQUFJLENBQUMsQ0FBQyxHQUFHLElBQUksQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLGVBQWUsQ0FBQztnQkFDdkMsSUFBSSxDQUFDLFVBQVUsR0FBRyxPQUFPLENBQUM7YUFDM0I7WUFDRCxJQUFJLENBQUMsU0FBUyxHQUFHLGlCQUFlLElBQUksQ0FBQyxDQUFDLFdBQU0sSUFBSSxDQUFDLENBQUMsTUFBRyxDQUFDO1NBQ3ZEO0lBQ0gsQ0FBQzs7Z0JBbkRvQixVQUFVOztJQW5CdEI7UUFBUixLQUFLLEVBQUU7b0RBQU87SUFDTjtRQUFSLEtBQUssRUFBRTs4REFBc0I7SUFDckI7UUFBUixLQUFLLEVBQUU7bURBQU07SUFDTDtRQUFSLEtBQUssRUFBRTttREFBTTtJQUNMO1FBQVIsS0FBSyxFQUFFO3VEQUFVO0lBQ1Q7UUFBUixLQUFLLEVBQUU7d0RBQVc7SUFDVjtRQUFSLEtBQUssRUFBRTswREFBYTtJQUVYO1FBQVQsTUFBTSxFQUFFO2dFQUEyRDtJQVR6RCxpQkFBaUI7UUFqQjdCLFNBQVMsQ0FBQztZQUNULFFBQVEsRUFBRSx5QkFBeUI7WUFDbkMsUUFBUSxFQUFFLHVRQVdUO1lBRUQsZUFBZSxFQUFFLHVCQUF1QixDQUFDLE1BQU07O1NBQ2hELENBQUM7T0FDVyxpQkFBaUIsQ0F3RTdCO0lBQUQsd0JBQUM7Q0FBQSxBQXhFRCxJQXdFQztTQXhFWSxpQkFBaUIiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQge1xuICBDb21wb25lbnQsXG4gIElucHV0LFxuICBPbkNoYW5nZXMsXG4gIFNpbXBsZUNoYW5nZXMsXG4gIENoYW5nZURldGVjdGlvblN0cmF0ZWd5LFxuICBFbGVtZW50UmVmLFxuICBPdXRwdXQsXG4gIEV2ZW50RW1pdHRlclxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IGZvcm1hdExhYmVsIH0gZnJvbSAnLi4vY29tbW9uL2xhYmVsLmhlbHBlcic7XG5cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ2dbbmd4LWNoYXJ0cy1iYXItbGFiZWxdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOnRleHRcbiAgICAgIGNsYXNzPVwidGV4dERhdGFMYWJlbFwiXG4gICAgICBhbGlnbm1lbnQtYmFzZWxpbmU9XCJtaWRkbGVcIlxuICAgICAgW2F0dHIudGV4dC1hbmNob3JdPVwidGV4dEFuY2hvclwiXG4gICAgICBbYXR0ci50cmFuc2Zvcm1dPVwidHJhbnNmb3JtXCJcbiAgICAgIFthdHRyLnhdPVwieFwiXG4gICAgICBbYXR0ci55XT1cInlcIlxuICAgID5cbiAgICAgIHt7IGZvcm1hdGVkVmFsdWUgfX1cbiAgICA8L3N2Zzp0ZXh0PlxuICBgLFxuICBzdHlsZVVybHM6IFsnLi9iYXItbGFiZWwuY29tcG9uZW50LnNjc3MnXSxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2hcbn0pXG5leHBvcnQgY2xhc3MgQmFyTGFiZWxDb21wb25lbnQgaW1wbGVtZW50cyBPbkNoYW5nZXMge1xuICBASW5wdXQoKSB2YWx1ZTtcbiAgQElucHV0KCkgdmFsdWVGb3JtYXR0aW5nOiBhbnk7XG4gIEBJbnB1dCgpIGJhclg7XG4gIEBJbnB1dCgpIGJhclk7XG4gIEBJbnB1dCgpIGJhcldpZHRoO1xuICBASW5wdXQoKSBiYXJIZWlnaHQ7XG4gIEBJbnB1dCgpIG9yaWVudGF0aW9uO1xuXG4gIEBPdXRwdXQoKSBkaW1lbnNpb25zQ2hhbmdlZDogRXZlbnRFbWl0dGVyPGFueT4gPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgZWxlbWVudDogYW55O1xuICB4OiBudW1iZXI7XG4gIHk6IG51bWJlcjtcbiAgaG9yaXpvbnRhbFBhZGRpbmc6IG51bWJlciA9IDI7XG4gIHZlcnRpY2FsUGFkZGluZzogbnVtYmVyID0gNTtcbiAgZm9ybWF0ZWRWYWx1ZTogc3RyaW5nO1xuICB0cmFuc2Zvcm06IHN0cmluZztcbiAgdGV4dEFuY2hvcjogc3RyaW5nO1xuXG4gIGNvbnN0cnVjdG9yKGVsZW1lbnQ6IEVsZW1lbnRSZWYpIHtcbiAgICB0aGlzLmVsZW1lbnQgPSBlbGVtZW50Lm5hdGl2ZUVsZW1lbnQ7XG4gIH1cblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKTogdm9pZCB7XG4gICAgdGhpcy51cGRhdGUoKTtcbiAgfVxuXG4gIGdldFNpemUoKTogYW55IHtcbiAgICBjb25zdCBoID0gdGhpcy5lbGVtZW50LmdldEJvdW5kaW5nQ2xpZW50UmVjdCgpLmhlaWdodDtcbiAgICBjb25zdCB3ID0gdGhpcy5lbGVtZW50LmdldEJvdW5kaW5nQ2xpZW50UmVjdCgpLndpZHRoO1xuICAgIHJldHVybiB7IGhlaWdodDogaCwgd2lkdGg6IHcsIG5lZ2F0aXZlOiB0aGlzLnZhbHVlIDwgMCB9O1xuICB9XG5cbiAgbmdBZnRlclZpZXdJbml0KCkge1xuICAgIHRoaXMuZGltZW5zaW9uc0NoYW5nZWQuZW1pdCh0aGlzLmdldFNpemUoKSk7XG4gIH1cblxuICB1cGRhdGUoKTogdm9pZCB7XG4gICAgaWYgKHRoaXMudmFsdWVGb3JtYXR0aW5nKSB7XG4gICAgICB0aGlzLmZvcm1hdGVkVmFsdWUgPSB0aGlzLnZhbHVlRm9ybWF0dGluZyh0aGlzLnZhbHVlKTtcbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5mb3JtYXRlZFZhbHVlID0gZm9ybWF0TGFiZWwodGhpcy52YWx1ZSk7XG4gICAgfVxuXG4gICAgaWYgKHRoaXMub3JpZW50YXRpb24gPT09ICdob3Jpem9udGFsJykge1xuICAgICAgdGhpcy54ID0gdGhpcy5iYXJYICsgdGhpcy5iYXJXaWR0aDtcbiAgICAgIC8vIGlmIHRoZSB2YWx1ZSBpcyBuZWdhdGl2ZSB0aGVuIGl0J3Mgb24gdGhlIGxlZnQgb2YgdGhlIHgwLlxuICAgICAgLy8gd2UgbmVlZCB0byBwdXQgdGhlIGRhdGEgbGFiZWwgaW4gZnJvbnQgb2YgdGhlIGJhclxuICAgICAgaWYgKHRoaXMudmFsdWUgPCAwKSB7XG4gICAgICAgIHRoaXMueCA9IHRoaXMueCAtIHRoaXMuaG9yaXpvbnRhbFBhZGRpbmc7XG4gICAgICAgIHRoaXMudGV4dEFuY2hvciA9ICdlbmQnO1xuICAgICAgfSBlbHNlIHtcbiAgICAgICAgdGhpcy54ID0gdGhpcy54ICsgdGhpcy5ob3Jpem9udGFsUGFkZGluZztcbiAgICAgICAgdGhpcy50ZXh0QW5jaG9yID0gJ3N0YXJ0JztcbiAgICAgIH1cbiAgICAgIHRoaXMueSA9IHRoaXMuYmFyWSArIHRoaXMuYmFySGVpZ2h0IC8gMjtcbiAgICB9IGVsc2Uge1xuICAgICAgLy8gb3JpZW50YXRpb24gbXVzdCBiZSBcInZlcnRpY2FsXCJcbiAgICAgIHRoaXMueCA9IHRoaXMuYmFyWCArIHRoaXMuYmFyV2lkdGggLyAyO1xuICAgICAgdGhpcy55ID0gdGhpcy5iYXJZICsgdGhpcy5iYXJIZWlnaHQ7XG5cbiAgICAgIGlmICh0aGlzLnZhbHVlIDwgMCkge1xuICAgICAgICB0aGlzLnkgPSB0aGlzLnkgKyB0aGlzLnZlcnRpY2FsUGFkZGluZztcbiAgICAgICAgdGhpcy50ZXh0QW5jaG9yID0gJ2VuZCc7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICB0aGlzLnkgPSB0aGlzLnkgLSB0aGlzLnZlcnRpY2FsUGFkZGluZztcbiAgICAgICAgdGhpcy50ZXh0QW5jaG9yID0gJ3N0YXJ0JztcbiAgICAgIH1cbiAgICAgIHRoaXMudHJhbnNmb3JtID0gYHJvdGF0ZSgtNDUsICR7dGhpcy54fSAsICR7dGhpcy55fSlgO1xuICAgIH1cbiAgfVxufVxuIl19