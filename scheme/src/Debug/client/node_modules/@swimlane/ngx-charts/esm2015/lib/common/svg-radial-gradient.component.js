import { __decorate } from "tslib";
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
let SvgRadialGradientComponent = class SvgRadialGradientComponent {
    constructor() {
        this.endOpacity = 1;
        this.cx = 0;
        this.cy = 0;
    }
    get stops() {
        return this.stopsInput || this.stopsDefault;
    }
    set stops(value) {
        this.stopsInput = value;
    }
    ngOnChanges(changes) {
        this.r = '30%';
        if ('color' in changes || 'startOpacity' in changes || 'endOpacity' in changes) {
            this.stopsDefault = [
                {
                    offset: 0,
                    color: this.color,
                    opacity: this.startOpacity
                },
                {
                    offset: 100,
                    color: this.color,
                    opacity: this.endOpacity
                }
            ];
        }
    }
};
__decorate([
    Input()
], SvgRadialGradientComponent.prototype, "color", void 0);
__decorate([
    Input()
], SvgRadialGradientComponent.prototype, "name", void 0);
__decorate([
    Input()
], SvgRadialGradientComponent.prototype, "startOpacity", void 0);
__decorate([
    Input()
], SvgRadialGradientComponent.prototype, "endOpacity", void 0);
__decorate([
    Input()
], SvgRadialGradientComponent.prototype, "cx", void 0);
__decorate([
    Input()
], SvgRadialGradientComponent.prototype, "cy", void 0);
__decorate([
    Input()
], SvgRadialGradientComponent.prototype, "stops", null);
SvgRadialGradientComponent = __decorate([
    Component({
        selector: 'g[ngx-charts-svg-radial-gradient]',
        template: `
    <svg:radialGradient [id]="name" [attr.cx]="cx" [attr.cy]="cy" [attr.r]="r" gradientUnits="userSpaceOnUse">
      <svg:stop
        *ngFor="let stop of stops"
        [attr.offset]="stop.offset + '%'"
        [style.stop-color]="stop.color"
        [style.stop-opacity]="stop.opacity"
      />
    </svg:radialGradient>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush
    })
], SvgRadialGradientComponent);
export { SvgRadialGradientComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic3ZnLXJhZGlhbC1ncmFkaWVudC5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vc3ZnLXJhZGlhbC1ncmFkaWVudC5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFBRSxTQUFTLEVBQUUsS0FBSyxFQUFhLHVCQUF1QixFQUFpQixNQUFNLGVBQWUsQ0FBQztBQWdCcEcsSUFBYSwwQkFBMEIsR0FBdkMsTUFBYSwwQkFBMEI7SUFBdkM7UUFJVyxlQUFVLEdBQUcsQ0FBQyxDQUFDO1FBQ2YsT0FBRSxHQUFXLENBQUMsQ0FBQztRQUNmLE9BQUUsR0FBVyxDQUFDLENBQUM7SUFpQzFCLENBQUM7SUE5QkMsSUFBSSxLQUFLO1FBQ1AsT0FBTyxJQUFJLENBQUMsVUFBVSxJQUFJLElBQUksQ0FBQyxZQUFZLENBQUM7SUFDOUMsQ0FBQztJQUVELElBQUksS0FBSyxDQUFDLEtBQVk7UUFDcEIsSUFBSSxDQUFDLFVBQVUsR0FBRyxLQUFLLENBQUM7SUFDMUIsQ0FBQztJQU9ELFdBQVcsQ0FBQyxPQUFzQjtRQUNoQyxJQUFJLENBQUMsQ0FBQyxHQUFHLEtBQUssQ0FBQztRQUNmLElBQUksT0FBTyxJQUFJLE9BQU8sSUFBSSxjQUFjLElBQUksT0FBTyxJQUFJLFlBQVksSUFBSSxPQUFPLEVBQUU7WUFDOUUsSUFBSSxDQUFDLFlBQVksR0FBRztnQkFDbEI7b0JBQ0UsTUFBTSxFQUFFLENBQUM7b0JBQ1QsS0FBSyxFQUFFLElBQUksQ0FBQyxLQUFLO29CQUNqQixPQUFPLEVBQUUsSUFBSSxDQUFDLFlBQVk7aUJBQzNCO2dCQUNEO29CQUNFLE1BQU0sRUFBRSxHQUFHO29CQUNYLEtBQUssRUFBRSxJQUFJLENBQUMsS0FBSztvQkFDakIsT0FBTyxFQUFFLElBQUksQ0FBQyxVQUFVO2lCQUN6QjthQUNGLENBQUM7U0FDSDtJQUNILENBQUM7Q0FDRixDQUFBO0FBdENVO0lBQVIsS0FBSyxFQUFFO3lEQUFlO0FBQ2Q7SUFBUixLQUFLLEVBQUU7d0RBQWM7QUFDYjtJQUFSLEtBQUssRUFBRTtnRUFBc0I7QUFDckI7SUFBUixLQUFLLEVBQUU7OERBQWdCO0FBQ2Y7SUFBUixLQUFLLEVBQUU7c0RBQWdCO0FBQ2Y7SUFBUixLQUFLLEVBQUU7c0RBQWdCO0FBR3hCO0lBREMsS0FBSyxFQUFFO3VEQUdQO0FBWFUsMEJBQTBCO0lBZHRDLFNBQVMsQ0FBQztRQUNULFFBQVEsRUFBRSxtQ0FBbUM7UUFDN0MsUUFBUSxFQUFFOzs7Ozs7Ozs7R0FTVDtRQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO0tBQ2hELENBQUM7R0FDVywwQkFBMEIsQ0F1Q3RDO1NBdkNZLDBCQUEwQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IENvbXBvbmVudCwgSW5wdXQsIE9uQ2hhbmdlcywgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksIFNpbXBsZUNoYW5nZXMgfSBmcm9tICdAYW5ndWxhci9jb3JlJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnZ1tuZ3gtY2hhcnRzLXN2Zy1yYWRpYWwtZ3JhZGllbnRdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOnJhZGlhbEdyYWRpZW50IFtpZF09XCJuYW1lXCIgW2F0dHIuY3hdPVwiY3hcIiBbYXR0ci5jeV09XCJjeVwiIFthdHRyLnJdPVwiclwiIGdyYWRpZW50VW5pdHM9XCJ1c2VyU3BhY2VPblVzZVwiPlxuICAgICAgPHN2ZzpzdG9wXG4gICAgICAgICpuZ0Zvcj1cImxldCBzdG9wIG9mIHN0b3BzXCJcbiAgICAgICAgW2F0dHIub2Zmc2V0XT1cInN0b3Aub2Zmc2V0ICsgJyUnXCJcbiAgICAgICAgW3N0eWxlLnN0b3AtY29sb3JdPVwic3RvcC5jb2xvclwiXG4gICAgICAgIFtzdHlsZS5zdG9wLW9wYWNpdHldPVwic3RvcC5vcGFjaXR5XCJcbiAgICAgIC8+XG4gICAgPC9zdmc6cmFkaWFsR3JhZGllbnQ+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIFN2Z1JhZGlhbEdyYWRpZW50Q29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgY29sb3I6IHN0cmluZztcbiAgQElucHV0KCkgbmFtZTogc3RyaW5nO1xuICBASW5wdXQoKSBzdGFydE9wYWNpdHk6IG51bWJlcjtcbiAgQElucHV0KCkgZW5kT3BhY2l0eSA9IDE7XG4gIEBJbnB1dCgpIGN4OiBudW1iZXIgPSAwO1xuICBASW5wdXQoKSBjeTogbnVtYmVyID0gMDtcblxuICBASW5wdXQoKVxuICBnZXQgc3RvcHMoKTogYW55W10ge1xuICAgIHJldHVybiB0aGlzLnN0b3BzSW5wdXQgfHwgdGhpcy5zdG9wc0RlZmF1bHQ7XG4gIH1cblxuICBzZXQgc3RvcHModmFsdWU6IGFueVtdKSB7XG4gICAgdGhpcy5zdG9wc0lucHV0ID0gdmFsdWU7XG4gIH1cblxuICByOiBzdHJpbmc7XG5cbiAgcHJpdmF0ZSBzdG9wc0lucHV0OiBhbnlbXTtcbiAgcHJpdmF0ZSBzdG9wc0RlZmF1bHQ6IGFueVtdO1xuXG4gIG5nT25DaGFuZ2VzKGNoYW5nZXM6IFNpbXBsZUNoYW5nZXMpOiB2b2lkIHtcbiAgICB0aGlzLnIgPSAnMzAlJztcbiAgICBpZiAoJ2NvbG9yJyBpbiBjaGFuZ2VzIHx8ICdzdGFydE9wYWNpdHknIGluIGNoYW5nZXMgfHwgJ2VuZE9wYWNpdHknIGluIGNoYW5nZXMpIHtcbiAgICAgIHRoaXMuc3RvcHNEZWZhdWx0ID0gW1xuICAgICAgICB7XG4gICAgICAgICAgb2Zmc2V0OiAwLFxuICAgICAgICAgIGNvbG9yOiB0aGlzLmNvbG9yLFxuICAgICAgICAgIG9wYWNpdHk6IHRoaXMuc3RhcnRPcGFjaXR5XG4gICAgICAgIH0sXG4gICAgICAgIHtcbiAgICAgICAgICBvZmZzZXQ6IDEwMCxcbiAgICAgICAgICBjb2xvcjogdGhpcy5jb2xvcixcbiAgICAgICAgICBvcGFjaXR5OiB0aGlzLmVuZE9wYWNpdHlcbiAgICAgICAgfVxuICAgICAgXTtcbiAgICB9XG4gIH1cbn1cbiJdfQ==