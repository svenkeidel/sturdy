import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ViewChild, ChangeDetectionStrategy } from '@angular/core';
import { YAxisTicksComponent } from './y-axis-ticks.component';
let YAxisComponent = class YAxisComponent {
    constructor() {
        this.showGridLines = false;
        this.yOrient = 'left';
        this.yAxisOffset = 0;
        this.dimensionsChanged = new EventEmitter();
        this.yAxisClassName = 'y axis';
        this.labelOffset = 15;
        this.fill = 'none';
        this.stroke = '#CCC';
        this.tickStroke = '#CCC';
        this.strokeWidth = 1;
        this.padding = 5;
    }
    ngOnChanges(changes) {
        this.update();
    }
    update() {
        this.offset = -(this.yAxisOffset + this.padding);
        if (this.yOrient === 'right') {
            this.labelOffset = 65;
            this.transform = `translate(${this.offset + this.dims.width} , 0)`;
        }
        else {
            this.offset = this.offset;
            this.transform = `translate(${this.offset} , 0)`;
        }
        if (this.yAxisTickCount !== undefined) {
            this.tickArguments = [this.yAxisTickCount];
        }
    }
    emitTicksWidth({ width }) {
        if (width !== this.labelOffset && this.yOrient === 'right') {
            this.labelOffset = width + this.labelOffset;
            setTimeout(() => {
                this.dimensionsChanged.emit({ width });
            }, 0);
        }
        else if (width !== this.labelOffset) {
            this.labelOffset = width;
            setTimeout(() => {
                this.dimensionsChanged.emit({ width });
            }, 0);
        }
    }
};
__decorate([
    Input()
], YAxisComponent.prototype, "yScale", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "dims", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "trimTicks", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "maxTickLength", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "tickFormatting", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "ticks", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "showGridLines", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "showLabel", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "labelText", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "yAxisTickInterval", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "yAxisTickCount", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "yOrient", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "referenceLines", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "showRefLines", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "showRefLabels", void 0);
__decorate([
    Input()
], YAxisComponent.prototype, "yAxisOffset", void 0);
__decorate([
    Output()
], YAxisComponent.prototype, "dimensionsChanged", void 0);
__decorate([
    ViewChild(YAxisTicksComponent)
], YAxisComponent.prototype, "ticksComponent", void 0);
YAxisComponent = __decorate([
    Component({
        selector: 'g[ngx-charts-y-axis]',
        template: `
    <svg:g [attr.class]="yAxisClassName" [attr.transform]="transform">
      <svg:g
        ngx-charts-y-axis-ticks
        *ngIf="yScale"
        [trimTicks]="trimTicks"
        [maxTickLength]="maxTickLength"
        [tickFormatting]="tickFormatting"
        [tickArguments]="tickArguments"
        [tickValues]="ticks"
        [tickStroke]="tickStroke"
        [scale]="yScale"
        [orient]="yOrient"
        [showGridLines]="showGridLines"
        [gridLineWidth]="dims.width"
        [referenceLines]="referenceLines"
        [showRefLines]="showRefLines"
        [showRefLabels]="showRefLabels"
        [height]="dims.height"
        (dimensionsChanged)="emitTicksWidth($event)"
      />

      <svg:g
        ngx-charts-axis-label
        *ngIf="showLabel"
        [label]="labelText"
        [offset]="labelOffset"
        [orient]="yOrient"
        [height]="dims.height"
        [width]="dims.width"
      ></svg:g>
    </svg:g>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush
    })
], YAxisComponent);
export { YAxisComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoieS1heGlzLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi9heGVzL3ktYXhpcy5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFDTCxTQUFTLEVBQ1QsS0FBSyxFQUNMLE1BQU0sRUFDTixZQUFZLEVBRVosU0FBUyxFQUVULHVCQUF1QixFQUN4QixNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUUsbUJBQW1CLEVBQUUsTUFBTSwwQkFBMEIsQ0FBQztBQXVDL0QsSUFBYSxjQUFjLEdBQTNCLE1BQWEsY0FBYztJQUEzQjtRQU9XLGtCQUFhLEdBQUcsS0FBSyxDQUFDO1FBS3RCLFlBQU8sR0FBVyxNQUFNLENBQUM7UUFJekIsZ0JBQVcsR0FBVyxDQUFDLENBQUM7UUFDdkIsc0JBQWlCLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUVqRCxtQkFBYyxHQUFXLFFBQVEsQ0FBQztRQUlsQyxnQkFBVyxHQUFXLEVBQUUsQ0FBQztRQUN6QixTQUFJLEdBQVcsTUFBTSxDQUFDO1FBQ3RCLFdBQU0sR0FBVyxNQUFNLENBQUM7UUFDeEIsZUFBVSxHQUFXLE1BQU0sQ0FBQztRQUM1QixnQkFBVyxHQUFXLENBQUMsQ0FBQztRQUN4QixZQUFPLEdBQVcsQ0FBQyxDQUFDO0lBb0N0QixDQUFDO0lBaENDLFdBQVcsQ0FBQyxPQUFzQjtRQUNoQyxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUM7SUFDaEIsQ0FBQztJQUVELE1BQU07UUFDSixJQUFJLENBQUMsTUFBTSxHQUFHLENBQUMsQ0FBQyxJQUFJLENBQUMsV0FBVyxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUMsQ0FBQztRQUNqRCxJQUFJLElBQUksQ0FBQyxPQUFPLEtBQUssT0FBTyxFQUFFO1lBQzVCLElBQUksQ0FBQyxXQUFXLEdBQUcsRUFBRSxDQUFDO1lBQ3RCLElBQUksQ0FBQyxTQUFTLEdBQUcsYUFBYSxJQUFJLENBQUMsTUFBTSxHQUFHLElBQUksQ0FBQyxJQUFJLENBQUMsS0FBSyxPQUFPLENBQUM7U0FDcEU7YUFBTTtZQUNMLElBQUksQ0FBQyxNQUFNLEdBQUcsSUFBSSxDQUFDLE1BQU0sQ0FBQztZQUMxQixJQUFJLENBQUMsU0FBUyxHQUFHLGFBQWEsSUFBSSxDQUFDLE1BQU0sT0FBTyxDQUFDO1NBQ2xEO1FBRUQsSUFBSSxJQUFJLENBQUMsY0FBYyxLQUFLLFNBQVMsRUFBRTtZQUNyQyxJQUFJLENBQUMsYUFBYSxHQUFHLENBQUMsSUFBSSxDQUFDLGNBQWMsQ0FBQyxDQUFDO1NBQzVDO0lBQ0gsQ0FBQztJQUVELGNBQWMsQ0FBQyxFQUFFLEtBQUssRUFBRTtRQUN0QixJQUFJLEtBQUssS0FBSyxJQUFJLENBQUMsV0FBVyxJQUFJLElBQUksQ0FBQyxPQUFPLEtBQUssT0FBTyxFQUFFO1lBQzFELElBQUksQ0FBQyxXQUFXLEdBQUcsS0FBSyxHQUFHLElBQUksQ0FBQyxXQUFXLENBQUM7WUFDNUMsVUFBVSxDQUFDLEdBQUcsRUFBRTtnQkFDZCxJQUFJLENBQUMsaUJBQWlCLENBQUMsSUFBSSxDQUFDLEVBQUUsS0FBSyxFQUFFLENBQUMsQ0FBQztZQUN6QyxDQUFDLEVBQUUsQ0FBQyxDQUFDLENBQUM7U0FDUDthQUFNLElBQUksS0FBSyxLQUFLLElBQUksQ0FBQyxXQUFXLEVBQUU7WUFDckMsSUFBSSxDQUFDLFdBQVcsR0FBRyxLQUFLLENBQUM7WUFDekIsVUFBVSxDQUFDLEdBQUcsRUFBRTtnQkFDZCxJQUFJLENBQUMsaUJBQWlCLENBQUMsSUFBSSxDQUFDLEVBQUUsS0FBSyxFQUFFLENBQUMsQ0FBQztZQUN6QyxDQUFDLEVBQUUsQ0FBQyxDQUFDLENBQUM7U0FDUDtJQUNILENBQUM7Q0FDRixDQUFBO0FBL0RVO0lBQVIsS0FBSyxFQUFFOzhDQUFRO0FBQ1A7SUFBUixLQUFLLEVBQUU7NENBQU07QUFDTDtJQUFSLEtBQUssRUFBRTtpREFBb0I7QUFDbkI7SUFBUixLQUFLLEVBQUU7cURBQXVCO0FBQ3RCO0lBQVIsS0FBSyxFQUFFO3NEQUFnQjtBQUNmO0lBQVIsS0FBSyxFQUFFOzZDQUFjO0FBQ2I7SUFBUixLQUFLLEVBQUU7cURBQXVCO0FBQ3RCO0lBQVIsS0FBSyxFQUFFO2lEQUFXO0FBQ1Y7SUFBUixLQUFLLEVBQUU7aURBQVc7QUFDVjtJQUFSLEtBQUssRUFBRTt5REFBbUI7QUFDbEI7SUFBUixLQUFLLEVBQUU7c0RBQXFCO0FBQ3BCO0lBQVIsS0FBSyxFQUFFOytDQUEwQjtBQUN6QjtJQUFSLEtBQUssRUFBRTtzREFBZ0I7QUFDZjtJQUFSLEtBQUssRUFBRTtvREFBYztBQUNiO0lBQVIsS0FBSyxFQUFFO3FEQUFlO0FBQ2Q7SUFBUixLQUFLLEVBQUU7bURBQXlCO0FBQ3ZCO0lBQVQsTUFBTSxFQUFFO3lEQUF3QztBQWFqQjtJQUEvQixTQUFTLENBQUMsbUJBQW1CLENBQUM7c0RBQXFDO0FBOUJ6RCxjQUFjO0lBckMxQixTQUFTLENBQUM7UUFDVCxRQUFRLEVBQUUsc0JBQXNCO1FBQ2hDLFFBQVEsRUFBRTs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7R0FnQ1Q7UUFDRCxlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTtLQUNoRCxDQUFDO0dBQ1csY0FBYyxDQWdFMUI7U0FoRVksY0FBYyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIENvbXBvbmVudCxcbiAgSW5wdXQsXG4gIE91dHB1dCxcbiAgRXZlbnRFbWl0dGVyLFxuICBPbkNoYW5nZXMsXG4gIFZpZXdDaGlsZCxcbiAgU2ltcGxlQ2hhbmdlcyxcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3lcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBZQXhpc1RpY2tzQ29tcG9uZW50IH0gZnJvbSAnLi95LWF4aXMtdGlja3MuY29tcG9uZW50JztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnZ1tuZ3gtY2hhcnRzLXktYXhpc10nLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxzdmc6ZyBbYXR0ci5jbGFzc109XCJ5QXhpc0NsYXNzTmFtZVwiIFthdHRyLnRyYW5zZm9ybV09XCJ0cmFuc2Zvcm1cIj5cbiAgICAgIDxzdmc6Z1xuICAgICAgICBuZ3gtY2hhcnRzLXktYXhpcy10aWNrc1xuICAgICAgICAqbmdJZj1cInlTY2FsZVwiXG4gICAgICAgIFt0cmltVGlja3NdPVwidHJpbVRpY2tzXCJcbiAgICAgICAgW21heFRpY2tMZW5ndGhdPVwibWF4VGlja0xlbmd0aFwiXG4gICAgICAgIFt0aWNrRm9ybWF0dGluZ109XCJ0aWNrRm9ybWF0dGluZ1wiXG4gICAgICAgIFt0aWNrQXJndW1lbnRzXT1cInRpY2tBcmd1bWVudHNcIlxuICAgICAgICBbdGlja1ZhbHVlc109XCJ0aWNrc1wiXG4gICAgICAgIFt0aWNrU3Ryb2tlXT1cInRpY2tTdHJva2VcIlxuICAgICAgICBbc2NhbGVdPVwieVNjYWxlXCJcbiAgICAgICAgW29yaWVudF09XCJ5T3JpZW50XCJcbiAgICAgICAgW3Nob3dHcmlkTGluZXNdPVwic2hvd0dyaWRMaW5lc1wiXG4gICAgICAgIFtncmlkTGluZVdpZHRoXT1cImRpbXMud2lkdGhcIlxuICAgICAgICBbcmVmZXJlbmNlTGluZXNdPVwicmVmZXJlbmNlTGluZXNcIlxuICAgICAgICBbc2hvd1JlZkxpbmVzXT1cInNob3dSZWZMaW5lc1wiXG4gICAgICAgIFtzaG93UmVmTGFiZWxzXT1cInNob3dSZWZMYWJlbHNcIlxuICAgICAgICBbaGVpZ2h0XT1cImRpbXMuaGVpZ2h0XCJcbiAgICAgICAgKGRpbWVuc2lvbnNDaGFuZ2VkKT1cImVtaXRUaWNrc1dpZHRoKCRldmVudClcIlxuICAgICAgLz5cblxuICAgICAgPHN2ZzpnXG4gICAgICAgIG5neC1jaGFydHMtYXhpcy1sYWJlbFxuICAgICAgICAqbmdJZj1cInNob3dMYWJlbFwiXG4gICAgICAgIFtsYWJlbF09XCJsYWJlbFRleHRcIlxuICAgICAgICBbb2Zmc2V0XT1cImxhYmVsT2Zmc2V0XCJcbiAgICAgICAgW29yaWVudF09XCJ5T3JpZW50XCJcbiAgICAgICAgW2hlaWdodF09XCJkaW1zLmhlaWdodFwiXG4gICAgICAgIFt3aWR0aF09XCJkaW1zLndpZHRoXCJcbiAgICAgID48L3N2ZzpnPlxuICAgIDwvc3ZnOmc+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIFlBeGlzQ29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgeVNjYWxlO1xuICBASW5wdXQoKSBkaW1zO1xuICBASW5wdXQoKSB0cmltVGlja3M6IGJvb2xlYW47XG4gIEBJbnB1dCgpIG1heFRpY2tMZW5ndGg6IG51bWJlcjtcbiAgQElucHV0KCkgdGlja0Zvcm1hdHRpbmc7XG4gIEBJbnB1dCgpIHRpY2tzOiBhbnlbXTtcbiAgQElucHV0KCkgc2hvd0dyaWRMaW5lcyA9IGZhbHNlO1xuICBASW5wdXQoKSBzaG93TGFiZWw7XG4gIEBJbnB1dCgpIGxhYmVsVGV4dDtcbiAgQElucHV0KCkgeUF4aXNUaWNrSW50ZXJ2YWw7XG4gIEBJbnB1dCgpIHlBeGlzVGlja0NvdW50OiBhbnk7XG4gIEBJbnB1dCgpIHlPcmllbnQ6IHN0cmluZyA9ICdsZWZ0JztcbiAgQElucHV0KCkgcmVmZXJlbmNlTGluZXM7XG4gIEBJbnB1dCgpIHNob3dSZWZMaW5lcztcbiAgQElucHV0KCkgc2hvd1JlZkxhYmVscztcbiAgQElucHV0KCkgeUF4aXNPZmZzZXQ6IG51bWJlciA9IDA7XG4gIEBPdXRwdXQoKSBkaW1lbnNpb25zQ2hhbmdlZCA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcblxuICB5QXhpc0NsYXNzTmFtZTogc3RyaW5nID0gJ3kgYXhpcyc7XG4gIHRpY2tBcmd1bWVudHM6IGFueTtcbiAgb2Zmc2V0OiBhbnk7XG4gIHRyYW5zZm9ybTogYW55O1xuICBsYWJlbE9mZnNldDogbnVtYmVyID0gMTU7XG4gIGZpbGw6IHN0cmluZyA9ICdub25lJztcbiAgc3Ryb2tlOiBzdHJpbmcgPSAnI0NDQyc7XG4gIHRpY2tTdHJva2U6IHN0cmluZyA9ICcjQ0NDJztcbiAgc3Ryb2tlV2lkdGg6IG51bWJlciA9IDE7XG4gIHBhZGRpbmc6IG51bWJlciA9IDU7XG5cbiAgQFZpZXdDaGlsZChZQXhpc1RpY2tzQ29tcG9uZW50KSB0aWNrc0NvbXBvbmVudDogWUF4aXNUaWNrc0NvbXBvbmVudDtcblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKTogdm9pZCB7XG4gICAgdGhpcy51cGRhdGUoKTtcbiAgfVxuXG4gIHVwZGF0ZSgpOiB2b2lkIHtcbiAgICB0aGlzLm9mZnNldCA9IC0odGhpcy55QXhpc09mZnNldCArIHRoaXMucGFkZGluZyk7XG4gICAgaWYgKHRoaXMueU9yaWVudCA9PT0gJ3JpZ2h0Jykge1xuICAgICAgdGhpcy5sYWJlbE9mZnNldCA9IDY1O1xuICAgICAgdGhpcy50cmFuc2Zvcm0gPSBgdHJhbnNsYXRlKCR7dGhpcy5vZmZzZXQgKyB0aGlzLmRpbXMud2lkdGh9ICwgMClgO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLm9mZnNldCA9IHRoaXMub2Zmc2V0O1xuICAgICAgdGhpcy50cmFuc2Zvcm0gPSBgdHJhbnNsYXRlKCR7dGhpcy5vZmZzZXR9ICwgMClgO1xuICAgIH1cblxuICAgIGlmICh0aGlzLnlBeGlzVGlja0NvdW50ICE9PSB1bmRlZmluZWQpIHtcbiAgICAgIHRoaXMudGlja0FyZ3VtZW50cyA9IFt0aGlzLnlBeGlzVGlja0NvdW50XTtcbiAgICB9XG4gIH1cblxuICBlbWl0VGlja3NXaWR0aCh7IHdpZHRoIH0pOiB2b2lkIHtcbiAgICBpZiAod2lkdGggIT09IHRoaXMubGFiZWxPZmZzZXQgJiYgdGhpcy55T3JpZW50ID09PSAncmlnaHQnKSB7XG4gICAgICB0aGlzLmxhYmVsT2Zmc2V0ID0gd2lkdGggKyB0aGlzLmxhYmVsT2Zmc2V0O1xuICAgICAgc2V0VGltZW91dCgoKSA9PiB7XG4gICAgICAgIHRoaXMuZGltZW5zaW9uc0NoYW5nZWQuZW1pdCh7IHdpZHRoIH0pO1xuICAgICAgfSwgMCk7XG4gICAgfSBlbHNlIGlmICh3aWR0aCAhPT0gdGhpcy5sYWJlbE9mZnNldCkge1xuICAgICAgdGhpcy5sYWJlbE9mZnNldCA9IHdpZHRoO1xuICAgICAgc2V0VGltZW91dCgoKSA9PiB7XG4gICAgICAgIHRoaXMuZGltZW5zaW9uc0NoYW5nZWQuZW1pdCh7IHdpZHRoIH0pO1xuICAgICAgfSwgMCk7XG4gICAgfVxuICB9XG59XG4iXX0=