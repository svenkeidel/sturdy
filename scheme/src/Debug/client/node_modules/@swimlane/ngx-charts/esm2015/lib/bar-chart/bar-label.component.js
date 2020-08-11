import { __decorate } from "tslib";
import { Component, Input, OnChanges, SimpleChanges, ChangeDetectionStrategy, ElementRef, Output, EventEmitter } from '@angular/core';
import { formatLabel } from '../common/label.helper';
let BarLabelComponent = class BarLabelComponent {
    constructor(element) {
        this.dimensionsChanged = new EventEmitter();
        this.horizontalPadding = 2;
        this.verticalPadding = 5;
        this.element = element.nativeElement;
    }
    ngOnChanges(changes) {
        this.update();
    }
    getSize() {
        const h = this.element.getBoundingClientRect().height;
        const w = this.element.getBoundingClientRect().width;
        return { height: h, width: w, negative: this.value < 0 };
    }
    ngAfterViewInit() {
        this.dimensionsChanged.emit(this.getSize());
    }
    update() {
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
            this.transform = `rotate(-45, ${this.x} , ${this.y})`;
        }
    }
};
BarLabelComponent.ctorParameters = () => [
    { type: ElementRef }
];
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
        template: `
    <svg:text
      class="textDataLabel"
      alignment-baseline="middle"
      [attr.text-anchor]="textAnchor"
      [attr.transform]="transform"
      [attr.x]="x"
      [attr.y]="y"
    >
      {{ formatedValue }}
    </svg:text>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush,
        styles: [".textDataLabel{font-size:11px}"]
    })
], BarLabelComponent);
export { BarLabelComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYmFyLWxhYmVsLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2Jhci1jaGFydC9iYXItbGFiZWwuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQ0wsU0FBUyxFQUNULEtBQUssRUFDTCxTQUFTLEVBQ1QsYUFBYSxFQUNiLHVCQUF1QixFQUN2QixVQUFVLEVBQ1YsTUFBTSxFQUNOLFlBQVksRUFDYixNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUUsV0FBVyxFQUFFLE1BQU0sd0JBQXdCLENBQUM7QUFtQnJELElBQWEsaUJBQWlCLEdBQTlCLE1BQWEsaUJBQWlCO0lBb0I1QixZQUFZLE9BQW1CO1FBWHJCLHNCQUFpQixHQUFzQixJQUFJLFlBQVksRUFBRSxDQUFDO1FBS3BFLHNCQUFpQixHQUFXLENBQUMsQ0FBQztRQUM5QixvQkFBZSxHQUFXLENBQUMsQ0FBQztRQU0xQixJQUFJLENBQUMsT0FBTyxHQUFHLE9BQU8sQ0FBQyxhQUFhLENBQUM7SUFDdkMsQ0FBQztJQUVELFdBQVcsQ0FBQyxPQUFzQjtRQUNoQyxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUM7SUFDaEIsQ0FBQztJQUVELE9BQU87UUFDTCxNQUFNLENBQUMsR0FBRyxJQUFJLENBQUMsT0FBTyxDQUFDLHFCQUFxQixFQUFFLENBQUMsTUFBTSxDQUFDO1FBQ3RELE1BQU0sQ0FBQyxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUMscUJBQXFCLEVBQUUsQ0FBQyxLQUFLLENBQUM7UUFDckQsT0FBTyxFQUFFLE1BQU0sRUFBRSxDQUFDLEVBQUUsS0FBSyxFQUFFLENBQUMsRUFBRSxRQUFRLEVBQUUsSUFBSSxDQUFDLEtBQUssR0FBRyxDQUFDLEVBQUUsQ0FBQztJQUMzRCxDQUFDO0lBRUQsZUFBZTtRQUNiLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDLENBQUM7SUFDOUMsQ0FBQztJQUVELE1BQU07UUFDSixJQUFJLElBQUksQ0FBQyxlQUFlLEVBQUU7WUFDeEIsSUFBSSxDQUFDLGFBQWEsR0FBRyxJQUFJLENBQUMsZUFBZSxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsQ0FBQztTQUN2RDthQUFNO1lBQ0wsSUFBSSxDQUFDLGFBQWEsR0FBRyxXQUFXLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxDQUFDO1NBQzlDO1FBRUQsSUFBSSxJQUFJLENBQUMsV0FBVyxLQUFLLFlBQVksRUFBRTtZQUNyQyxJQUFJLENBQUMsQ0FBQyxHQUFHLElBQUksQ0FBQyxJQUFJLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQztZQUNuQyw0REFBNEQ7WUFDNUQsb0RBQW9EO1lBQ3BELElBQUksSUFBSSxDQUFDLEtBQUssR0FBRyxDQUFDLEVBQUU7Z0JBQ2xCLElBQUksQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsaUJBQWlCLENBQUM7Z0JBQ3pDLElBQUksQ0FBQyxVQUFVLEdBQUcsS0FBSyxDQUFDO2FBQ3pCO2lCQUFNO2dCQUNMLElBQUksQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsaUJBQWlCLENBQUM7Z0JBQ3pDLElBQUksQ0FBQyxVQUFVLEdBQUcsT0FBTyxDQUFDO2FBQzNCO1lBQ0QsSUFBSSxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsSUFBSSxHQUFHLElBQUksQ0FBQyxTQUFTLEdBQUcsQ0FBQyxDQUFDO1NBQ3pDO2FBQU07WUFDTCxpQ0FBaUM7WUFDakMsSUFBSSxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsSUFBSSxHQUFHLElBQUksQ0FBQyxRQUFRLEdBQUcsQ0FBQyxDQUFDO1lBQ3ZDLElBQUksQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLElBQUksR0FBRyxJQUFJLENBQUMsU0FBUyxDQUFDO1lBRXBDLElBQUksSUFBSSxDQUFDLEtBQUssR0FBRyxDQUFDLEVBQUU7Z0JBQ2xCLElBQUksQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsZUFBZSxDQUFDO2dCQUN2QyxJQUFJLENBQUMsVUFBVSxHQUFHLEtBQUssQ0FBQzthQUN6QjtpQkFBTTtnQkFDTCxJQUFJLENBQUMsQ0FBQyxHQUFHLElBQUksQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLGVBQWUsQ0FBQztnQkFDdkMsSUFBSSxDQUFDLFVBQVUsR0FBRyxPQUFPLENBQUM7YUFDM0I7WUFDRCxJQUFJLENBQUMsU0FBUyxHQUFHLGVBQWUsSUFBSSxDQUFDLENBQUMsTUFBTSxJQUFJLENBQUMsQ0FBQyxHQUFHLENBQUM7U0FDdkQ7SUFDSCxDQUFDO0NBQ0YsQ0FBQTs7WUFwRHNCLFVBQVU7O0FBbkJ0QjtJQUFSLEtBQUssRUFBRTtnREFBTztBQUNOO0lBQVIsS0FBSyxFQUFFOzBEQUFzQjtBQUNyQjtJQUFSLEtBQUssRUFBRTsrQ0FBTTtBQUNMO0lBQVIsS0FBSyxFQUFFOytDQUFNO0FBQ0w7SUFBUixLQUFLLEVBQUU7bURBQVU7QUFDVDtJQUFSLEtBQUssRUFBRTtvREFBVztBQUNWO0lBQVIsS0FBSyxFQUFFO3NEQUFhO0FBRVg7SUFBVCxNQUFNLEVBQUU7NERBQTJEO0FBVHpELGlCQUFpQjtJQWpCN0IsU0FBUyxDQUFDO1FBQ1QsUUFBUSxFQUFFLHlCQUF5QjtRQUNuQyxRQUFRLEVBQUU7Ozs7Ozs7Ozs7O0dBV1Q7UUFFRCxlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTs7S0FDaEQsQ0FBQztHQUNXLGlCQUFpQixDQXdFN0I7U0F4RVksaUJBQWlCIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHtcbiAgQ29tcG9uZW50LFxuICBJbnB1dCxcbiAgT25DaGFuZ2VzLFxuICBTaW1wbGVDaGFuZ2VzLFxuICBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSxcbiAgRWxlbWVudFJlZixcbiAgT3V0cHV0LFxuICBFdmVudEVtaXR0ZXJcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBmb3JtYXRMYWJlbCB9IGZyb20gJy4uL2NvbW1vbi9sYWJlbC5oZWxwZXInO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdnW25neC1jaGFydHMtYmFyLWxhYmVsXScsXG4gIHRlbXBsYXRlOiBgXG4gICAgPHN2Zzp0ZXh0XG4gICAgICBjbGFzcz1cInRleHREYXRhTGFiZWxcIlxuICAgICAgYWxpZ25tZW50LWJhc2VsaW5lPVwibWlkZGxlXCJcbiAgICAgIFthdHRyLnRleHQtYW5jaG9yXT1cInRleHRBbmNob3JcIlxuICAgICAgW2F0dHIudHJhbnNmb3JtXT1cInRyYW5zZm9ybVwiXG4gICAgICBbYXR0ci54XT1cInhcIlxuICAgICAgW2F0dHIueV09XCJ5XCJcbiAgICA+XG4gICAgICB7eyBmb3JtYXRlZFZhbHVlIH19XG4gICAgPC9zdmc6dGV4dD5cbiAgYCxcbiAgc3R5bGVVcmxzOiBbJy4vYmFyLWxhYmVsLmNvbXBvbmVudC5zY3NzJ10sXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIEJhckxhYmVsQ29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgdmFsdWU7XG4gIEBJbnB1dCgpIHZhbHVlRm9ybWF0dGluZzogYW55O1xuICBASW5wdXQoKSBiYXJYO1xuICBASW5wdXQoKSBiYXJZO1xuICBASW5wdXQoKSBiYXJXaWR0aDtcbiAgQElucHV0KCkgYmFySGVpZ2h0O1xuICBASW5wdXQoKSBvcmllbnRhdGlvbjtcblxuICBAT3V0cHV0KCkgZGltZW5zaW9uc0NoYW5nZWQ6IEV2ZW50RW1pdHRlcjxhbnk+ID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuXG4gIGVsZW1lbnQ6IGFueTtcbiAgeDogbnVtYmVyO1xuICB5OiBudW1iZXI7XG4gIGhvcml6b250YWxQYWRkaW5nOiBudW1iZXIgPSAyO1xuICB2ZXJ0aWNhbFBhZGRpbmc6IG51bWJlciA9IDU7XG4gIGZvcm1hdGVkVmFsdWU6IHN0cmluZztcbiAgdHJhbnNmb3JtOiBzdHJpbmc7XG4gIHRleHRBbmNob3I6IHN0cmluZztcblxuICBjb25zdHJ1Y3RvcihlbGVtZW50OiBFbGVtZW50UmVmKSB7XG4gICAgdGhpcy5lbGVtZW50ID0gZWxlbWVudC5uYXRpdmVFbGVtZW50O1xuICB9XG5cbiAgbmdPbkNoYW5nZXMoY2hhbmdlczogU2ltcGxlQ2hhbmdlcyk6IHZvaWQge1xuICAgIHRoaXMudXBkYXRlKCk7XG4gIH1cblxuICBnZXRTaXplKCk6IGFueSB7XG4gICAgY29uc3QgaCA9IHRoaXMuZWxlbWVudC5nZXRCb3VuZGluZ0NsaWVudFJlY3QoKS5oZWlnaHQ7XG4gICAgY29uc3QgdyA9IHRoaXMuZWxlbWVudC5nZXRCb3VuZGluZ0NsaWVudFJlY3QoKS53aWR0aDtcbiAgICByZXR1cm4geyBoZWlnaHQ6IGgsIHdpZHRoOiB3LCBuZWdhdGl2ZTogdGhpcy52YWx1ZSA8IDAgfTtcbiAgfVxuXG4gIG5nQWZ0ZXJWaWV3SW5pdCgpIHtcbiAgICB0aGlzLmRpbWVuc2lvbnNDaGFuZ2VkLmVtaXQodGhpcy5nZXRTaXplKCkpO1xuICB9XG5cbiAgdXBkYXRlKCk6IHZvaWQge1xuICAgIGlmICh0aGlzLnZhbHVlRm9ybWF0dGluZykge1xuICAgICAgdGhpcy5mb3JtYXRlZFZhbHVlID0gdGhpcy52YWx1ZUZvcm1hdHRpbmcodGhpcy52YWx1ZSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHRoaXMuZm9ybWF0ZWRWYWx1ZSA9IGZvcm1hdExhYmVsKHRoaXMudmFsdWUpO1xuICAgIH1cblxuICAgIGlmICh0aGlzLm9yaWVudGF0aW9uID09PSAnaG9yaXpvbnRhbCcpIHtcbiAgICAgIHRoaXMueCA9IHRoaXMuYmFyWCArIHRoaXMuYmFyV2lkdGg7XG4gICAgICAvLyBpZiB0aGUgdmFsdWUgaXMgbmVnYXRpdmUgdGhlbiBpdCdzIG9uIHRoZSBsZWZ0IG9mIHRoZSB4MC5cbiAgICAgIC8vIHdlIG5lZWQgdG8gcHV0IHRoZSBkYXRhIGxhYmVsIGluIGZyb250IG9mIHRoZSBiYXJcbiAgICAgIGlmICh0aGlzLnZhbHVlIDwgMCkge1xuICAgICAgICB0aGlzLnggPSB0aGlzLnggLSB0aGlzLmhvcml6b250YWxQYWRkaW5nO1xuICAgICAgICB0aGlzLnRleHRBbmNob3IgPSAnZW5kJztcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIHRoaXMueCA9IHRoaXMueCArIHRoaXMuaG9yaXpvbnRhbFBhZGRpbmc7XG4gICAgICAgIHRoaXMudGV4dEFuY2hvciA9ICdzdGFydCc7XG4gICAgICB9XG4gICAgICB0aGlzLnkgPSB0aGlzLmJhclkgKyB0aGlzLmJhckhlaWdodCAvIDI7XG4gICAgfSBlbHNlIHtcbiAgICAgIC8vIG9yaWVudGF0aW9uIG11c3QgYmUgXCJ2ZXJ0aWNhbFwiXG4gICAgICB0aGlzLnggPSB0aGlzLmJhclggKyB0aGlzLmJhcldpZHRoIC8gMjtcbiAgICAgIHRoaXMueSA9IHRoaXMuYmFyWSArIHRoaXMuYmFySGVpZ2h0O1xuXG4gICAgICBpZiAodGhpcy52YWx1ZSA8IDApIHtcbiAgICAgICAgdGhpcy55ID0gdGhpcy55ICsgdGhpcy52ZXJ0aWNhbFBhZGRpbmc7XG4gICAgICAgIHRoaXMudGV4dEFuY2hvciA9ICdlbmQnO1xuICAgICAgfSBlbHNlIHtcbiAgICAgICAgdGhpcy55ID0gdGhpcy55IC0gdGhpcy52ZXJ0aWNhbFBhZGRpbmc7XG4gICAgICAgIHRoaXMudGV4dEFuY2hvciA9ICdzdGFydCc7XG4gICAgICB9XG4gICAgICB0aGlzLnRyYW5zZm9ybSA9IGByb3RhdGUoLTQ1LCAke3RoaXMueH0gLCAke3RoaXMueX0pYDtcbiAgICB9XG4gIH1cbn1cbiJdfQ==