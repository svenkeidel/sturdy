import { __decorate } from "tslib";
import { Component, Input, ChangeDetectionStrategy, ViewEncapsulation } from '@angular/core';
import { DomSanitizer } from '@angular/platform-browser';
var ScaleLegendComponent = /** @class */ (function () {
    function ScaleLegendComponent(sanitizer) {
        this.sanitizer = sanitizer;
        this.horizontal = false;
    }
    ScaleLegendComponent.prototype.ngOnChanges = function (changes) {
        var gradientValues = this.gradientString(this.colors.range(), this.colors.domain());
        var direction = this.horizontal ? 'right' : 'bottom';
        this.gradient = this.sanitizer.bypassSecurityTrustStyle("linear-gradient(to " + direction + ", " + gradientValues + ")");
    };
    /**
     * Generates the string used in the gradient stylesheet properties
     * @param colors array of colors
     * @param splits array of splits on a scale of (0, 1)
     */
    ScaleLegendComponent.prototype.gradientString = function (colors, splits) {
        // add the 100%
        splits.push(1);
        var pairs = [];
        colors.reverse().forEach(function (c, i) {
            pairs.push(c + " " + Math.round(splits[i] * 100) + "%");
        });
        return pairs.join(', ');
    };
    ScaleLegendComponent.ctorParameters = function () { return [
        { type: DomSanitizer }
    ]; };
    __decorate([
        Input()
    ], ScaleLegendComponent.prototype, "valueRange", void 0);
    __decorate([
        Input()
    ], ScaleLegendComponent.prototype, "colors", void 0);
    __decorate([
        Input()
    ], ScaleLegendComponent.prototype, "height", void 0);
    __decorate([
        Input()
    ], ScaleLegendComponent.prototype, "width", void 0);
    __decorate([
        Input()
    ], ScaleLegendComponent.prototype, "horizontal", void 0);
    ScaleLegendComponent = __decorate([
        Component({
            selector: 'ngx-charts-scale-legend',
            template: "\n    <div\n      class=\"scale-legend\"\n      [class.horizontal-legend]=\"horizontal\"\n      [style.height.px]=\"horizontal ? undefined : height\"\n      [style.width.px]=\"width\"\n    >\n      <div class=\"scale-legend-label\">\n        <span>{{ valueRange[1].toLocaleString() }}</span>\n      </div>\n      <div class=\"scale-legend-wrap\" [style.background]=\"gradient\"></div>\n      <div class=\"scale-legend-label\">\n        <span>{{ valueRange[0].toLocaleString() }}</span>\n      </div>\n    </div>\n  ",
            encapsulation: ViewEncapsulation.None,
            changeDetection: ChangeDetectionStrategy.OnPush,
            styles: [".chart-legend{display:inline-block;padding:0;width:auto!important}.chart-legend .scale-legend{text-align:center;display:-webkit-box;display:flex;-webkit-box-orient:vertical;-webkit-box-direction:normal;flex-direction:column}.chart-legend .scale-legend-wrap{display:inline-block;-webkit-box-flex:1;flex:1;width:30px;border-radius:5px;margin:0 auto}.chart-legend .scale-legend-label{font-size:12px}.chart-legend .horizontal-legend.scale-legend{-webkit-box-orient:horizontal;-webkit-box-direction:normal;flex-direction:row}.chart-legend .horizontal-legend .scale-legend-wrap{width:auto;height:30px;margin:0 16px}"]
        })
    ], ScaleLegendComponent);
    return ScaleLegendComponent;
}());
export { ScaleLegendComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic2NhbGUtbGVnZW5kLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi9sZWdlbmQvc2NhbGUtbGVnZW5kLmNvbXBvbmVudC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUFFLFNBQVMsRUFBRSxLQUFLLEVBQWEsdUJBQXVCLEVBQWlCLGlCQUFpQixFQUFFLE1BQU0sZUFBZSxDQUFDO0FBQ3ZILE9BQU8sRUFBRSxZQUFZLEVBQUUsTUFBTSwyQkFBMkIsQ0FBQztBQXdCekQ7SUFTRSw4QkFBb0IsU0FBdUI7UUFBdkIsY0FBUyxHQUFULFNBQVMsQ0FBYztRQUpsQyxlQUFVLEdBQUcsS0FBSyxDQUFDO0lBSWtCLENBQUM7SUFFL0MsMENBQVcsR0FBWCxVQUFZLE9BQXNCO1FBQ2hDLElBQU0sY0FBYyxHQUFHLElBQUksQ0FBQyxjQUFjLENBQUMsSUFBSSxDQUFDLE1BQU0sQ0FBQyxLQUFLLEVBQUUsRUFBRSxJQUFJLENBQUMsTUFBTSxDQUFDLE1BQU0sRUFBRSxDQUFDLENBQUM7UUFDdEYsSUFBTSxTQUFTLEdBQUcsSUFBSSxDQUFDLFVBQVUsQ0FBQyxDQUFDLENBQUMsT0FBTyxDQUFDLENBQUMsQ0FBQyxRQUFRLENBQUM7UUFDdkQsSUFBSSxDQUFDLFFBQVEsR0FBRyxJQUFJLENBQUMsU0FBUyxDQUFDLHdCQUF3QixDQUFDLHdCQUFzQixTQUFTLFVBQUssY0FBYyxNQUFHLENBQUMsQ0FBQztJQUNqSCxDQUFDO0lBRUQ7Ozs7T0FJRztJQUNILDZDQUFjLEdBQWQsVUFBZSxNQUFNLEVBQUUsTUFBTTtRQUMzQixlQUFlO1FBQ2YsTUFBTSxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsQ0FBQztRQUNmLElBQU0sS0FBSyxHQUFHLEVBQUUsQ0FBQztRQUNqQixNQUFNLENBQUMsT0FBTyxFQUFFLENBQUMsT0FBTyxDQUFDLFVBQUMsQ0FBQyxFQUFFLENBQUM7WUFDNUIsS0FBSyxDQUFDLElBQUksQ0FBSSxDQUFDLFNBQUksSUFBSSxDQUFDLEtBQUssQ0FBQyxNQUFNLENBQUMsQ0FBQyxDQUFDLEdBQUcsR0FBRyxDQUFDLE1BQUcsQ0FBQyxDQUFDO1FBQ3JELENBQUMsQ0FBQyxDQUFDO1FBRUgsT0FBTyxLQUFLLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQzFCLENBQUM7O2dCQXRCOEIsWUFBWTs7SUFSbEM7UUFBUixLQUFLLEVBQUU7NERBQVk7SUFDWDtRQUFSLEtBQUssRUFBRTt3REFBUTtJQUNQO1FBQVIsS0FBSyxFQUFFO3dEQUFRO0lBQ1A7UUFBUixLQUFLLEVBQUU7dURBQU87SUFDTjtRQUFSLEtBQUssRUFBRTs0REFBb0I7SUFMakIsb0JBQW9CO1FBdEJoQyxTQUFTLENBQUM7WUFDVCxRQUFRLEVBQUUseUJBQXlCO1lBQ25DLFFBQVEsRUFBRSxxZ0JBZVQ7WUFFRCxhQUFhLEVBQUUsaUJBQWlCLENBQUMsSUFBSTtZQUNyQyxlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTs7U0FDaEQsQ0FBQztPQUNXLG9CQUFvQixDQWdDaEM7SUFBRCwyQkFBQztDQUFBLEFBaENELElBZ0NDO1NBaENZLG9CQUFvQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IENvbXBvbmVudCwgSW5wdXQsIE9uQ2hhbmdlcywgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksIFNpbXBsZUNoYW5nZXMsIFZpZXdFbmNhcHN1bGF0aW9uIH0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBEb21TYW5pdGl6ZXIgfSBmcm9tICdAYW5ndWxhci9wbGF0Zm9ybS1icm93c2VyJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnbmd4LWNoYXJ0cy1zY2FsZS1sZWdlbmQnLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxkaXZcbiAgICAgIGNsYXNzPVwic2NhbGUtbGVnZW5kXCJcbiAgICAgIFtjbGFzcy5ob3Jpem9udGFsLWxlZ2VuZF09XCJob3Jpem9udGFsXCJcbiAgICAgIFtzdHlsZS5oZWlnaHQucHhdPVwiaG9yaXpvbnRhbCA/IHVuZGVmaW5lZCA6IGhlaWdodFwiXG4gICAgICBbc3R5bGUud2lkdGgucHhdPVwid2lkdGhcIlxuICAgID5cbiAgICAgIDxkaXYgY2xhc3M9XCJzY2FsZS1sZWdlbmQtbGFiZWxcIj5cbiAgICAgICAgPHNwYW4+e3sgdmFsdWVSYW5nZVsxXS50b0xvY2FsZVN0cmluZygpIH19PC9zcGFuPlxuICAgICAgPC9kaXY+XG4gICAgICA8ZGl2IGNsYXNzPVwic2NhbGUtbGVnZW5kLXdyYXBcIiBbc3R5bGUuYmFja2dyb3VuZF09XCJncmFkaWVudFwiPjwvZGl2PlxuICAgICAgPGRpdiBjbGFzcz1cInNjYWxlLWxlZ2VuZC1sYWJlbFwiPlxuICAgICAgICA8c3Bhbj57eyB2YWx1ZVJhbmdlWzBdLnRvTG9jYWxlU3RyaW5nKCkgfX08L3NwYW4+XG4gICAgICA8L2Rpdj5cbiAgICA8L2Rpdj5cbiAgYCxcbiAgc3R5bGVVcmxzOiBbJy4vc2NhbGUtbGVnZW5kLmNvbXBvbmVudC5zY3NzJ10sXG4gIGVuY2Fwc3VsYXRpb246IFZpZXdFbmNhcHN1bGF0aW9uLk5vbmUsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIFNjYWxlTGVnZW5kQ29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgdmFsdWVSYW5nZTtcbiAgQElucHV0KCkgY29sb3JzO1xuICBASW5wdXQoKSBoZWlnaHQ7XG4gIEBJbnB1dCgpIHdpZHRoO1xuICBASW5wdXQoKSBob3Jpem9udGFsID0gZmFsc2U7XG5cbiAgZ3JhZGllbnQ6IGFueTtcblxuICBjb25zdHJ1Y3Rvcihwcml2YXRlIHNhbml0aXplcjogRG9tU2FuaXRpemVyKSB7fVxuXG4gIG5nT25DaGFuZ2VzKGNoYW5nZXM6IFNpbXBsZUNoYW5nZXMpOiB2b2lkIHtcbiAgICBjb25zdCBncmFkaWVudFZhbHVlcyA9IHRoaXMuZ3JhZGllbnRTdHJpbmcodGhpcy5jb2xvcnMucmFuZ2UoKSwgdGhpcy5jb2xvcnMuZG9tYWluKCkpO1xuICAgIGNvbnN0IGRpcmVjdGlvbiA9IHRoaXMuaG9yaXpvbnRhbCA/ICdyaWdodCcgOiAnYm90dG9tJztcbiAgICB0aGlzLmdyYWRpZW50ID0gdGhpcy5zYW5pdGl6ZXIuYnlwYXNzU2VjdXJpdHlUcnVzdFN0eWxlKGBsaW5lYXItZ3JhZGllbnQodG8gJHtkaXJlY3Rpb259LCAke2dyYWRpZW50VmFsdWVzfSlgKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBHZW5lcmF0ZXMgdGhlIHN0cmluZyB1c2VkIGluIHRoZSBncmFkaWVudCBzdHlsZXNoZWV0IHByb3BlcnRpZXNcbiAgICogQHBhcmFtIGNvbG9ycyBhcnJheSBvZiBjb2xvcnNcbiAgICogQHBhcmFtIHNwbGl0cyBhcnJheSBvZiBzcGxpdHMgb24gYSBzY2FsZSBvZiAoMCwgMSlcbiAgICovXG4gIGdyYWRpZW50U3RyaW5nKGNvbG9ycywgc3BsaXRzKTogc3RyaW5nIHtcbiAgICAvLyBhZGQgdGhlIDEwMCVcbiAgICBzcGxpdHMucHVzaCgxKTtcbiAgICBjb25zdCBwYWlycyA9IFtdO1xuICAgIGNvbG9ycy5yZXZlcnNlKCkuZm9yRWFjaCgoYywgaSkgPT4ge1xuICAgICAgcGFpcnMucHVzaChgJHtjfSAke01hdGgucm91bmQoc3BsaXRzW2ldICogMTAwKX0lYCk7XG4gICAgfSk7XG5cbiAgICByZXR1cm4gcGFpcnMuam9pbignLCAnKTtcbiAgfVxufVxuIl19