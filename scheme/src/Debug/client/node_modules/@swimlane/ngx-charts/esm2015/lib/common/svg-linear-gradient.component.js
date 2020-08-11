import { __decorate } from "tslib";
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
let SvgLinearGradientComponent = class SvgLinearGradientComponent {
    constructor() {
        this.orientation = 'vertical';
    }
    ngOnChanges(changes) {
        this.x1 = '0%';
        this.x2 = '0%';
        this.y1 = '0%';
        this.y2 = '0%';
        if (this.orientation === 'horizontal') {
            this.x2 = '100%';
        }
        else if (this.orientation === 'vertical') {
            this.y1 = '100%';
        }
    }
};
__decorate([
    Input()
], SvgLinearGradientComponent.prototype, "orientation", void 0);
__decorate([
    Input()
], SvgLinearGradientComponent.prototype, "name", void 0);
__decorate([
    Input()
], SvgLinearGradientComponent.prototype, "stops", void 0);
SvgLinearGradientComponent = __decorate([
    Component({
        selector: 'g[ngx-charts-svg-linear-gradient]',
        template: `
    <svg:linearGradient [id]="name" [attr.x1]="x1" [attr.y1]="y1" [attr.x2]="x2" [attr.y2]="y2">
      <svg:stop
        *ngFor="let stop of stops"
        [attr.offset]="stop.offset + '%'"
        [style.stop-color]="stop.color"
        [style.stop-opacity]="stop.opacity"
      />
    </svg:linearGradient>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush
    })
], SvgLinearGradientComponent);
export { SvgLinearGradientComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic3ZnLWxpbmVhci1ncmFkaWVudC5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vc3ZnLWxpbmVhci1ncmFkaWVudC5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFBRSxTQUFTLEVBQUUsS0FBSyxFQUE0Qix1QkFBdUIsRUFBRSxNQUFNLGVBQWUsQ0FBQztBQWdCcEcsSUFBYSwwQkFBMEIsR0FBdkMsTUFBYSwwQkFBMEI7SUFBdkM7UUFDVyxnQkFBVyxHQUFHLFVBQVUsQ0FBQztJQXFCcEMsQ0FBQztJQVpDLFdBQVcsQ0FBQyxPQUFzQjtRQUNoQyxJQUFJLENBQUMsRUFBRSxHQUFHLElBQUksQ0FBQztRQUNmLElBQUksQ0FBQyxFQUFFLEdBQUcsSUFBSSxDQUFDO1FBQ2YsSUFBSSxDQUFDLEVBQUUsR0FBRyxJQUFJLENBQUM7UUFDZixJQUFJLENBQUMsRUFBRSxHQUFHLElBQUksQ0FBQztRQUVmLElBQUksSUFBSSxDQUFDLFdBQVcsS0FBSyxZQUFZLEVBQUU7WUFDckMsSUFBSSxDQUFDLEVBQUUsR0FBRyxNQUFNLENBQUM7U0FDbEI7YUFBTSxJQUFJLElBQUksQ0FBQyxXQUFXLEtBQUssVUFBVSxFQUFFO1lBQzFDLElBQUksQ0FBQyxFQUFFLEdBQUcsTUFBTSxDQUFDO1NBQ2xCO0lBQ0gsQ0FBQztDQUNGLENBQUE7QUFyQlU7SUFBUixLQUFLLEVBQUU7K0RBQTBCO0FBQ3pCO0lBQVIsS0FBSyxFQUFFO3dEQUFNO0FBQ0w7SUFBUixLQUFLLEVBQUU7eURBQWM7QUFIWCwwQkFBMEI7SUFkdEMsU0FBUyxDQUFDO1FBQ1QsUUFBUSxFQUFFLG1DQUFtQztRQUM3QyxRQUFRLEVBQUU7Ozs7Ozs7OztHQVNUO1FBQ0QsZUFBZSxFQUFFLHVCQUF1QixDQUFDLE1BQU07S0FDaEQsQ0FBQztHQUNXLDBCQUEwQixDQXNCdEM7U0F0QlksMEJBQTBCIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgQ29tcG9uZW50LCBJbnB1dCwgT25DaGFuZ2VzLCBTaW1wbGVDaGFuZ2VzLCBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSB9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdnW25neC1jaGFydHMtc3ZnLWxpbmVhci1ncmFkaWVudF0nLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxzdmc6bGluZWFyR3JhZGllbnQgW2lkXT1cIm5hbWVcIiBbYXR0ci54MV09XCJ4MVwiIFthdHRyLnkxXT1cInkxXCIgW2F0dHIueDJdPVwieDJcIiBbYXR0ci55Ml09XCJ5MlwiPlxuICAgICAgPHN2ZzpzdG9wXG4gICAgICAgICpuZ0Zvcj1cImxldCBzdG9wIG9mIHN0b3BzXCJcbiAgICAgICAgW2F0dHIub2Zmc2V0XT1cInN0b3Aub2Zmc2V0ICsgJyUnXCJcbiAgICAgICAgW3N0eWxlLnN0b3AtY29sb3JdPVwic3RvcC5jb2xvclwiXG4gICAgICAgIFtzdHlsZS5zdG9wLW9wYWNpdHldPVwic3RvcC5vcGFjaXR5XCJcbiAgICAgIC8+XG4gICAgPC9zdmc6bGluZWFyR3JhZGllbnQ+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIFN2Z0xpbmVhckdyYWRpZW50Q29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgb3JpZW50YXRpb24gPSAndmVydGljYWwnO1xuICBASW5wdXQoKSBuYW1lO1xuICBASW5wdXQoKSBzdG9wczogYW55W107XG5cbiAgeDE6IGFueTtcbiAgeDI6IGFueTtcbiAgeTE6IGFueTtcbiAgeTI6IGFueTtcblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKTogdm9pZCB7XG4gICAgdGhpcy54MSA9ICcwJSc7XG4gICAgdGhpcy54MiA9ICcwJSc7XG4gICAgdGhpcy55MSA9ICcwJSc7XG4gICAgdGhpcy55MiA9ICcwJSc7XG5cbiAgICBpZiAodGhpcy5vcmllbnRhdGlvbiA9PT0gJ2hvcml6b250YWwnKSB7XG4gICAgICB0aGlzLngyID0gJzEwMCUnO1xuICAgIH0gZWxzZSBpZiAodGhpcy5vcmllbnRhdGlvbiA9PT0gJ3ZlcnRpY2FsJykge1xuICAgICAgdGhpcy55MSA9ICcxMDAlJztcbiAgICB9XG4gIH1cbn1cbiJdfQ==