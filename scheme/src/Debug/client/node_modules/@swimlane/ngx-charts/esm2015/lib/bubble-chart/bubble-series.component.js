import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy } from '@angular/core';
import { trigger, style, animate, transition } from '@angular/animations';
import { formatLabel, escapeLabel } from '../common/label.helper';
let BubbleSeriesComponent = class BubbleSeriesComponent {
    constructor() {
        this.tooltipDisabled = false;
        this.select = new EventEmitter();
        this.activate = new EventEmitter();
        this.deactivate = new EventEmitter();
    }
    ngOnChanges(changes) {
        this.update();
    }
    update() {
        this.circles = this.getCircles();
    }
    getCircles() {
        const seriesName = this.data.name;
        return this.data.series
            .map((d, i) => {
            if (typeof d.y !== 'undefined' && typeof d.x !== 'undefined') {
                const y = d.y;
                const x = d.x;
                const r = d.r;
                const radius = this.rScale(r || 1);
                const tooltipLabel = formatLabel(d.name);
                const cx = this.xScaleType === 'linear' ? this.xScale(Number(x)) : this.xScale(x);
                const cy = this.yScaleType === 'linear' ? this.yScale(Number(y)) : this.yScale(y);
                const color = this.colors.scaleType === 'linear' ? this.colors.getColor(r) : this.colors.getColor(seriesName);
                const isActive = !this.activeEntries.length ? true : this.isActive({ name: seriesName });
                const opacity = isActive ? 1 : 0.3;
                const data = Object.assign({}, d, {
                    series: seriesName,
                    name: d.name,
                    value: d.y,
                    x: d.x,
                    radius: d.r
                });
                return {
                    data,
                    x,
                    y,
                    r,
                    classNames: [`circle-data-${i}`],
                    value: y,
                    label: x,
                    cx,
                    cy,
                    radius,
                    tooltipLabel,
                    color,
                    opacity,
                    seriesName,
                    isActive,
                    transform: `translate(${cx},${cy})`
                };
            }
        })
            .filter(circle => circle !== undefined);
    }
    getTooltipText(circle) {
        const hasRadius = typeof circle.r !== 'undefined';
        const hasTooltipLabel = circle.tooltipLabel && circle.tooltipLabel.length;
        const hasSeriesName = circle.seriesName && circle.seriesName.length;
        const radiusValue = hasRadius ? formatLabel(circle.r) : '';
        const xAxisLabel = this.xAxisLabel && this.xAxisLabel !== '' ? `${this.xAxisLabel}:` : '';
        const yAxisLabel = this.yAxisLabel && this.yAxisLabel !== '' ? `${this.yAxisLabel}:` : '';
        const x = formatLabel(circle.x);
        const y = formatLabel(circle.y);
        const name = hasSeriesName && hasTooltipLabel
            ? `${circle.seriesName} • ${circle.tooltipLabel}`
            : circle.seriesName + circle.tooltipLabel;
        const tooltipTitle = hasSeriesName || hasTooltipLabel ? `<span class="tooltip-label">${escapeLabel(name)}</span>` : '';
        return `
      ${tooltipTitle}
      <span class="tooltip-label">
        <label>${escapeLabel(xAxisLabel)}</label> ${escapeLabel(x)}<br />
        <label>${escapeLabel(yAxisLabel)}</label> ${escapeLabel(y)}
      </span>
      <span class="tooltip-val">
        ${escapeLabel(radiusValue)}
      </span>
    `;
    }
    onClick(data) {
        this.select.emit(data);
    }
    isActive(entry) {
        if (!this.activeEntries)
            return false;
        const item = this.activeEntries.find(d => {
            return entry.name === d.name;
        });
        return item !== undefined;
    }
    isVisible(circle) {
        if (this.activeEntries.length > 0) {
            return this.isActive({ name: circle.seriesName });
        }
        return circle.opacity !== 0;
    }
    activateCircle(circle) {
        circle.barVisible = true;
        this.activate.emit({ name: this.data.name });
    }
    deactivateCircle(circle) {
        circle.barVisible = false;
        this.deactivate.emit({ name: this.data.name });
    }
    trackBy(index, circle) {
        return `${circle.data.series} ${circle.data.name}`;
    }
};
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "data", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "xScale", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "yScale", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "rScale", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "xScaleType", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "yScaleType", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "colors", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "visibleValue", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "activeEntries", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "xAxisLabel", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "yAxisLabel", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "tooltipDisabled", void 0);
__decorate([
    Input()
], BubbleSeriesComponent.prototype, "tooltipTemplate", void 0);
__decorate([
    Output()
], BubbleSeriesComponent.prototype, "select", void 0);
__decorate([
    Output()
], BubbleSeriesComponent.prototype, "activate", void 0);
__decorate([
    Output()
], BubbleSeriesComponent.prototype, "deactivate", void 0);
BubbleSeriesComponent = __decorate([
    Component({
        selector: 'g[ngx-charts-bubble-series]',
        template: `
    <svg:g *ngFor="let circle of circles; trackBy: trackBy">
      <svg:g [attr.transform]="circle.transform">
        <svg:g
          ngx-charts-circle
          [@animationState]="'active'"
          class="circle"
          [cx]="0"
          [cy]="0"
          [r]="circle.radius"
          [fill]="circle.color"
          [style.opacity]="circle.opacity"
          [class.active]="circle.isActive"
          [pointerEvents]="'all'"
          [data]="circle.value"
          [classNames]="circle.classNames"
          (select)="onClick(circle.data)"
          (activate)="activateCircle(circle)"
          (deactivate)="deactivateCircle(circle)"
          ngx-tooltip
          [tooltipDisabled]="tooltipDisabled"
          [tooltipPlacement]="'top'"
          [tooltipType]="'tooltip'"
          [tooltipTitle]="tooltipTemplate ? undefined : getTooltipText(circle)"
          [tooltipTemplate]="tooltipTemplate"
          [tooltipContext]="circle.data"
        />
      </svg:g>
    </svg:g>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush,
        animations: [
            trigger('animationState', [
                transition(':enter', [
                    style({
                        opacity: 0,
                        transform: 'scale(0)'
                    }),
                    animate(250, style({ opacity: 1, transform: 'scale(1)' }))
                ])
            ])
        ]
    })
], BubbleSeriesComponent);
export { BubbleSeriesComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYnViYmxlLXNlcmllcy5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9idWJibGUtY2hhcnQvYnViYmxlLXNlcmllcy5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFDTCxTQUFTLEVBQ1QsS0FBSyxFQUNMLE1BQU0sRUFFTixZQUFZLEVBRVosdUJBQXVCLEVBRXhCLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBRSxPQUFPLEVBQUUsS0FBSyxFQUFFLE9BQU8sRUFBRSxVQUFVLEVBQUUsTUFBTSxxQkFBcUIsQ0FBQztBQUMxRSxPQUFPLEVBQUUsV0FBVyxFQUFFLFdBQVcsRUFBRSxNQUFNLHdCQUF3QixDQUFDO0FBK0NsRSxJQUFhLHFCQUFxQixHQUFsQyxNQUFhLHFCQUFxQjtJQUFsQztRQVlXLG9CQUFlLEdBQVksS0FBSyxDQUFDO1FBR2hDLFdBQU0sR0FBRyxJQUFJLFlBQVksRUFBRSxDQUFDO1FBQzVCLGFBQVEsR0FBRyxJQUFJLFlBQVksRUFBRSxDQUFDO1FBQzlCLGVBQVUsR0FBRyxJQUFJLFlBQVksRUFBRSxDQUFDO0lBK0g1QyxDQUFDO0lBMUhDLFdBQVcsQ0FBQyxPQUFzQjtRQUNoQyxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUM7SUFDaEIsQ0FBQztJQUVELE1BQU07UUFDSixJQUFJLENBQUMsT0FBTyxHQUFHLElBQUksQ0FBQyxVQUFVLEVBQUUsQ0FBQztJQUNuQyxDQUFDO0lBRUQsVUFBVTtRQUNSLE1BQU0sVUFBVSxHQUFHLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDO1FBRWxDLE9BQU8sSUFBSSxDQUFDLElBQUksQ0FBQyxNQUFNO2FBQ3BCLEdBQUcsQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLEVBQUUsRUFBRTtZQUNaLElBQUksT0FBTyxDQUFDLENBQUMsQ0FBQyxLQUFLLFdBQVcsSUFBSSxPQUFPLENBQUMsQ0FBQyxDQUFDLEtBQUssV0FBVyxFQUFFO2dCQUM1RCxNQUFNLENBQUMsR0FBRyxDQUFDLENBQUMsQ0FBQyxDQUFDO2dCQUNkLE1BQU0sQ0FBQyxHQUFHLENBQUMsQ0FBQyxDQUFDLENBQUM7Z0JBQ2QsTUFBTSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsQ0FBQztnQkFFZCxNQUFNLE1BQU0sR0FBRyxJQUFJLENBQUMsTUFBTSxDQUFDLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQztnQkFDbkMsTUFBTSxZQUFZLEdBQUcsV0FBVyxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsQ0FBQztnQkFFekMsTUFBTSxFQUFFLEdBQUcsSUFBSSxDQUFDLFVBQVUsS0FBSyxRQUFRLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxNQUFNLENBQUMsTUFBTSxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxNQUFNLENBQUMsQ0FBQyxDQUFDLENBQUM7Z0JBQ2xGLE1BQU0sRUFBRSxHQUFHLElBQUksQ0FBQyxVQUFVLEtBQUssUUFBUSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsTUFBTSxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsTUFBTSxDQUFDLENBQUMsQ0FBQyxDQUFDO2dCQUVsRixNQUFNLEtBQUssR0FBRyxJQUFJLENBQUMsTUFBTSxDQUFDLFNBQVMsS0FBSyxRQUFRLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxNQUFNLENBQUMsUUFBUSxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsTUFBTSxDQUFDLFFBQVEsQ0FBQyxVQUFVLENBQUMsQ0FBQztnQkFFOUcsTUFBTSxRQUFRLEdBQUcsQ0FBQyxJQUFJLENBQUMsYUFBYSxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsUUFBUSxDQUFDLEVBQUUsSUFBSSxFQUFFLFVBQVUsRUFBRSxDQUFDLENBQUM7Z0JBQ3pGLE1BQU0sT0FBTyxHQUFHLFFBQVEsQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxHQUFHLENBQUM7Z0JBRW5DLE1BQU0sSUFBSSxHQUFHLE1BQU0sQ0FBQyxNQUFNLENBQUMsRUFBRSxFQUFFLENBQUMsRUFBRTtvQkFDaEMsTUFBTSxFQUFFLFVBQVU7b0JBQ2xCLElBQUksRUFBRSxDQUFDLENBQUMsSUFBSTtvQkFDWixLQUFLLEVBQUUsQ0FBQyxDQUFDLENBQUM7b0JBQ1YsQ0FBQyxFQUFFLENBQUMsQ0FBQyxDQUFDO29CQUNOLE1BQU0sRUFBRSxDQUFDLENBQUMsQ0FBQztpQkFDWixDQUFDLENBQUM7Z0JBRUgsT0FBTztvQkFDTCxJQUFJO29CQUNKLENBQUM7b0JBQ0QsQ0FBQztvQkFDRCxDQUFDO29CQUNELFVBQVUsRUFBRSxDQUFDLGVBQWUsQ0FBQyxFQUFFLENBQUM7b0JBQ2hDLEtBQUssRUFBRSxDQUFDO29CQUNSLEtBQUssRUFBRSxDQUFDO29CQUNSLEVBQUU7b0JBQ0YsRUFBRTtvQkFDRixNQUFNO29CQUNOLFlBQVk7b0JBQ1osS0FBSztvQkFDTCxPQUFPO29CQUNQLFVBQVU7b0JBQ1YsUUFBUTtvQkFDUixTQUFTLEVBQUUsYUFBYSxFQUFFLElBQUksRUFBRSxHQUFHO2lCQUNwQyxDQUFDO2FBQ0g7UUFDSCxDQUFDLENBQUM7YUFDRCxNQUFNLENBQUMsTUFBTSxDQUFDLEVBQUUsQ0FBQyxNQUFNLEtBQUssU0FBUyxDQUFDLENBQUM7SUFDNUMsQ0FBQztJQUVELGNBQWMsQ0FBQyxNQUFNO1FBQ25CLE1BQU0sU0FBUyxHQUFHLE9BQU8sTUFBTSxDQUFDLENBQUMsS0FBSyxXQUFXLENBQUM7UUFDbEQsTUFBTSxlQUFlLEdBQUcsTUFBTSxDQUFDLFlBQVksSUFBSSxNQUFNLENBQUMsWUFBWSxDQUFDLE1BQU0sQ0FBQztRQUMxRSxNQUFNLGFBQWEsR0FBRyxNQUFNLENBQUMsVUFBVSxJQUFJLE1BQU0sQ0FBQyxVQUFVLENBQUMsTUFBTSxDQUFDO1FBRXBFLE1BQU0sV0FBVyxHQUFHLFNBQVMsQ0FBQyxDQUFDLENBQUMsV0FBVyxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDO1FBQzNELE1BQU0sVUFBVSxHQUFHLElBQUksQ0FBQyxVQUFVLElBQUksSUFBSSxDQUFDLFVBQVUsS0FBSyxFQUFFLENBQUMsQ0FBQyxDQUFDLEdBQUcsSUFBSSxDQUFDLFVBQVUsR0FBRyxDQUFDLENBQUMsQ0FBQyxFQUFFLENBQUM7UUFDMUYsTUFBTSxVQUFVLEdBQUcsSUFBSSxDQUFDLFVBQVUsSUFBSSxJQUFJLENBQUMsVUFBVSxLQUFLLEVBQUUsQ0FBQyxDQUFDLENBQUMsR0FBRyxJQUFJLENBQUMsVUFBVSxHQUFHLENBQUMsQ0FBQyxDQUFDLEVBQUUsQ0FBQztRQUMxRixNQUFNLENBQUMsR0FBRyxXQUFXLENBQUMsTUFBTSxDQUFDLENBQUMsQ0FBQyxDQUFDO1FBQ2hDLE1BQU0sQ0FBQyxHQUFHLFdBQVcsQ0FBQyxNQUFNLENBQUMsQ0FBQyxDQUFDLENBQUM7UUFDaEMsTUFBTSxJQUFJLEdBQ1IsYUFBYSxJQUFJLGVBQWU7WUFDOUIsQ0FBQyxDQUFDLEdBQUcsTUFBTSxDQUFDLFVBQVUsTUFBTSxNQUFNLENBQUMsWUFBWSxFQUFFO1lBQ2pELENBQUMsQ0FBQyxNQUFNLENBQUMsVUFBVSxHQUFHLE1BQU0sQ0FBQyxZQUFZLENBQUM7UUFDOUMsTUFBTSxZQUFZLEdBQ2hCLGFBQWEsSUFBSSxlQUFlLENBQUMsQ0FBQyxDQUFDLCtCQUErQixXQUFXLENBQUMsSUFBSSxDQUFDLFNBQVMsQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDO1FBRXBHLE9BQU87UUFDSCxZQUFZOztpQkFFSCxXQUFXLENBQUMsVUFBVSxDQUFDLFlBQVksV0FBVyxDQUFDLENBQUMsQ0FBQztpQkFDakQsV0FBVyxDQUFDLFVBQVUsQ0FBQyxZQUFZLFdBQVcsQ0FBQyxDQUFDLENBQUM7OztVQUd4RCxXQUFXLENBQUMsV0FBVyxDQUFDOztLQUU3QixDQUFDO0lBQ0osQ0FBQztJQUVELE9BQU8sQ0FBQyxJQUFJO1FBQ1YsSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUM7SUFDekIsQ0FBQztJQUVELFFBQVEsQ0FBQyxLQUFLO1FBQ1osSUFBSSxDQUFDLElBQUksQ0FBQyxhQUFhO1lBQUUsT0FBTyxLQUFLLENBQUM7UUFDdEMsTUFBTSxJQUFJLEdBQUcsSUFBSSxDQUFDLGFBQWEsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLEVBQUU7WUFDdkMsT0FBTyxLQUFLLENBQUMsSUFBSSxLQUFLLENBQUMsQ0FBQyxJQUFJLENBQUM7UUFDL0IsQ0FBQyxDQUFDLENBQUM7UUFDSCxPQUFPLElBQUksS0FBSyxTQUFTLENBQUM7SUFDNUIsQ0FBQztJQUVELFNBQVMsQ0FBQyxNQUFNO1FBQ2QsSUFBSSxJQUFJLENBQUMsYUFBYSxDQUFDLE1BQU0sR0FBRyxDQUFDLEVBQUU7WUFDakMsT0FBTyxJQUFJLENBQUMsUUFBUSxDQUFDLEVBQUUsSUFBSSxFQUFFLE1BQU0sQ0FBQyxVQUFVLEVBQUUsQ0FBQyxDQUFDO1NBQ25EO1FBRUQsT0FBTyxNQUFNLENBQUMsT0FBTyxLQUFLLENBQUMsQ0FBQztJQUM5QixDQUFDO0lBRUQsY0FBYyxDQUFDLE1BQU07UUFDbkIsTUFBTSxDQUFDLFVBQVUsR0FBRyxJQUFJLENBQUM7UUFDekIsSUFBSSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsRUFBRSxJQUFJLEVBQUUsSUFBSSxDQUFDLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDO0lBQy9DLENBQUM7SUFFRCxnQkFBZ0IsQ0FBQyxNQUFNO1FBQ3JCLE1BQU0sQ0FBQyxVQUFVLEdBQUcsS0FBSyxDQUFDO1FBQzFCLElBQUksQ0FBQyxVQUFVLENBQUMsSUFBSSxDQUFDLEVBQUUsSUFBSSxFQUFFLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQztJQUNqRCxDQUFDO0lBRUQsT0FBTyxDQUFDLEtBQUssRUFBRSxNQUFNO1FBQ25CLE9BQU8sR0FBRyxNQUFNLENBQUMsSUFBSSxDQUFDLE1BQU0sSUFBSSxNQUFNLENBQUMsSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDO0lBQ3JELENBQUM7Q0FDRixDQUFBO0FBL0lVO0lBQVIsS0FBSyxFQUFFO21EQUFNO0FBQ0w7SUFBUixLQUFLLEVBQUU7cURBQVE7QUFDUDtJQUFSLEtBQUssRUFBRTtxREFBUTtBQUNQO0lBQVIsS0FBSyxFQUFFO3FEQUFRO0FBQ1A7SUFBUixLQUFLLEVBQUU7eURBQVk7QUFDWDtJQUFSLEtBQUssRUFBRTt5REFBWTtBQUNYO0lBQVIsS0FBSyxFQUFFO3FEQUFRO0FBQ1A7SUFBUixLQUFLLEVBQUU7MkRBQWM7QUFDYjtJQUFSLEtBQUssRUFBRTs0REFBc0I7QUFDckI7SUFBUixLQUFLLEVBQUU7eURBQW9CO0FBQ25CO0lBQVIsS0FBSyxFQUFFO3lEQUFvQjtBQUNuQjtJQUFSLEtBQUssRUFBRTs4REFBa0M7QUFDakM7SUFBUixLQUFLLEVBQUU7OERBQW1DO0FBRWpDO0lBQVQsTUFBTSxFQUFFO3FEQUE2QjtBQUM1QjtJQUFULE1BQU0sRUFBRTt1REFBK0I7QUFDOUI7SUFBVCxNQUFNLEVBQUU7eURBQWlDO0FBakIvQixxQkFBcUI7SUE3Q2pDLFNBQVMsQ0FBQztRQUNULFFBQVEsRUFBRSw2QkFBNkI7UUFDdkMsUUFBUSxFQUFFOzs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7OztHQTZCVDtRQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO1FBQy9DLFVBQVUsRUFBRTtZQUNWLE9BQU8sQ0FBQyxnQkFBZ0IsRUFBRTtnQkFDeEIsVUFBVSxDQUFDLFFBQVEsRUFBRTtvQkFDbkIsS0FBSyxDQUFDO3dCQUNKLE9BQU8sRUFBRSxDQUFDO3dCQUNWLFNBQVMsRUFBRSxVQUFVO3FCQUN0QixDQUFDO29CQUNGLE9BQU8sQ0FBQyxHQUFHLEVBQUUsS0FBSyxDQUFDLEVBQUUsT0FBTyxFQUFFLENBQUMsRUFBRSxTQUFTLEVBQUUsVUFBVSxFQUFFLENBQUMsQ0FBQztpQkFDM0QsQ0FBQzthQUNILENBQUM7U0FDSDtLQUNGLENBQUM7R0FDVyxxQkFBcUIsQ0FnSmpDO1NBaEpZLHFCQUFxQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIENvbXBvbmVudCxcbiAgSW5wdXQsXG4gIE91dHB1dCxcbiAgU2ltcGxlQ2hhbmdlcyxcbiAgRXZlbnRFbWl0dGVyLFxuICBPbkNoYW5nZXMsXG4gIENoYW5nZURldGVjdGlvblN0cmF0ZWd5LFxuICBUZW1wbGF0ZVJlZlxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IHRyaWdnZXIsIHN0eWxlLCBhbmltYXRlLCB0cmFuc2l0aW9uIH0gZnJvbSAnQGFuZ3VsYXIvYW5pbWF0aW9ucyc7XG5pbXBvcnQgeyBmb3JtYXRMYWJlbCwgZXNjYXBlTGFiZWwgfSBmcm9tICcuLi9jb21tb24vbGFiZWwuaGVscGVyJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnZ1tuZ3gtY2hhcnRzLWJ1YmJsZS1zZXJpZXNdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8c3ZnOmcgKm5nRm9yPVwibGV0IGNpcmNsZSBvZiBjaXJjbGVzOyB0cmFja0J5OiB0cmFja0J5XCI+XG4gICAgICA8c3ZnOmcgW2F0dHIudHJhbnNmb3JtXT1cImNpcmNsZS50cmFuc2Zvcm1cIj5cbiAgICAgICAgPHN2ZzpnXG4gICAgICAgICAgbmd4LWNoYXJ0cy1jaXJjbGVcbiAgICAgICAgICBbQGFuaW1hdGlvblN0YXRlXT1cIidhY3RpdmUnXCJcbiAgICAgICAgICBjbGFzcz1cImNpcmNsZVwiXG4gICAgICAgICAgW2N4XT1cIjBcIlxuICAgICAgICAgIFtjeV09XCIwXCJcbiAgICAgICAgICBbcl09XCJjaXJjbGUucmFkaXVzXCJcbiAgICAgICAgICBbZmlsbF09XCJjaXJjbGUuY29sb3JcIlxuICAgICAgICAgIFtzdHlsZS5vcGFjaXR5XT1cImNpcmNsZS5vcGFjaXR5XCJcbiAgICAgICAgICBbY2xhc3MuYWN0aXZlXT1cImNpcmNsZS5pc0FjdGl2ZVwiXG4gICAgICAgICAgW3BvaW50ZXJFdmVudHNdPVwiJ2FsbCdcIlxuICAgICAgICAgIFtkYXRhXT1cImNpcmNsZS52YWx1ZVwiXG4gICAgICAgICAgW2NsYXNzTmFtZXNdPVwiY2lyY2xlLmNsYXNzTmFtZXNcIlxuICAgICAgICAgIChzZWxlY3QpPVwib25DbGljayhjaXJjbGUuZGF0YSlcIlxuICAgICAgICAgIChhY3RpdmF0ZSk9XCJhY3RpdmF0ZUNpcmNsZShjaXJjbGUpXCJcbiAgICAgICAgICAoZGVhY3RpdmF0ZSk9XCJkZWFjdGl2YXRlQ2lyY2xlKGNpcmNsZSlcIlxuICAgICAgICAgIG5neC10b29sdGlwXG4gICAgICAgICAgW3Rvb2x0aXBEaXNhYmxlZF09XCJ0b29sdGlwRGlzYWJsZWRcIlxuICAgICAgICAgIFt0b29sdGlwUGxhY2VtZW50XT1cIid0b3AnXCJcbiAgICAgICAgICBbdG9vbHRpcFR5cGVdPVwiJ3Rvb2x0aXAnXCJcbiAgICAgICAgICBbdG9vbHRpcFRpdGxlXT1cInRvb2x0aXBUZW1wbGF0ZSA/IHVuZGVmaW5lZCA6IGdldFRvb2x0aXBUZXh0KGNpcmNsZSlcIlxuICAgICAgICAgIFt0b29sdGlwVGVtcGxhdGVdPVwidG9vbHRpcFRlbXBsYXRlXCJcbiAgICAgICAgICBbdG9vbHRpcENvbnRleHRdPVwiY2lyY2xlLmRhdGFcIlxuICAgICAgICAvPlxuICAgICAgPC9zdmc6Zz5cbiAgICA8L3N2ZzpnPlxuICBgLFxuICBjaGFuZ2VEZXRlY3Rpb246IENoYW5nZURldGVjdGlvblN0cmF0ZWd5Lk9uUHVzaCxcbiAgYW5pbWF0aW9uczogW1xuICAgIHRyaWdnZXIoJ2FuaW1hdGlvblN0YXRlJywgW1xuICAgICAgdHJhbnNpdGlvbignOmVudGVyJywgW1xuICAgICAgICBzdHlsZSh7XG4gICAgICAgICAgb3BhY2l0eTogMCxcbiAgICAgICAgICB0cmFuc2Zvcm06ICdzY2FsZSgwKSdcbiAgICAgICAgfSksXG4gICAgICAgIGFuaW1hdGUoMjUwLCBzdHlsZSh7IG9wYWNpdHk6IDEsIHRyYW5zZm9ybTogJ3NjYWxlKDEpJyB9KSlcbiAgICAgIF0pXG4gICAgXSlcbiAgXVxufSlcbmV4cG9ydCBjbGFzcyBCdWJibGVTZXJpZXNDb21wb25lbnQgaW1wbGVtZW50cyBPbkNoYW5nZXMge1xuICBASW5wdXQoKSBkYXRhO1xuICBASW5wdXQoKSB4U2NhbGU7XG4gIEBJbnB1dCgpIHlTY2FsZTtcbiAgQElucHV0KCkgclNjYWxlO1xuICBASW5wdXQoKSB4U2NhbGVUeXBlO1xuICBASW5wdXQoKSB5U2NhbGVUeXBlO1xuICBASW5wdXQoKSBjb2xvcnM7XG4gIEBJbnB1dCgpIHZpc2libGVWYWx1ZTtcbiAgQElucHV0KCkgYWN0aXZlRW50cmllczogYW55W107XG4gIEBJbnB1dCgpIHhBeGlzTGFiZWw6IHN0cmluZztcbiAgQElucHV0KCkgeUF4aXNMYWJlbDogc3RyaW5nO1xuICBASW5wdXQoKSB0b29sdGlwRGlzYWJsZWQ6IGJvb2xlYW4gPSBmYWxzZTtcbiAgQElucHV0KCkgdG9vbHRpcFRlbXBsYXRlOiBUZW1wbGF0ZVJlZjxhbnk+O1xuXG4gIEBPdXRwdXQoKSBzZWxlY3QgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBhY3RpdmF0ZSA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcbiAgQE91dHB1dCgpIGRlYWN0aXZhdGUgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgYXJlYVBhdGg6IGFueTtcbiAgY2lyY2xlczogYW55W107XG5cbiAgbmdPbkNoYW5nZXMoY2hhbmdlczogU2ltcGxlQ2hhbmdlcyk6IHZvaWQge1xuICAgIHRoaXMudXBkYXRlKCk7XG4gIH1cblxuICB1cGRhdGUoKTogdm9pZCB7XG4gICAgdGhpcy5jaXJjbGVzID0gdGhpcy5nZXRDaXJjbGVzKCk7XG4gIH1cblxuICBnZXRDaXJjbGVzKCk6IGFueVtdIHtcbiAgICBjb25zdCBzZXJpZXNOYW1lID0gdGhpcy5kYXRhLm5hbWU7XG5cbiAgICByZXR1cm4gdGhpcy5kYXRhLnNlcmllc1xuICAgICAgLm1hcCgoZCwgaSkgPT4ge1xuICAgICAgICBpZiAodHlwZW9mIGQueSAhPT0gJ3VuZGVmaW5lZCcgJiYgdHlwZW9mIGQueCAhPT0gJ3VuZGVmaW5lZCcpIHtcbiAgICAgICAgICBjb25zdCB5ID0gZC55O1xuICAgICAgICAgIGNvbnN0IHggPSBkLng7XG4gICAgICAgICAgY29uc3QgciA9IGQucjtcblxuICAgICAgICAgIGNvbnN0IHJhZGl1cyA9IHRoaXMuclNjYWxlKHIgfHwgMSk7XG4gICAgICAgICAgY29uc3QgdG9vbHRpcExhYmVsID0gZm9ybWF0TGFiZWwoZC5uYW1lKTtcblxuICAgICAgICAgIGNvbnN0IGN4ID0gdGhpcy54U2NhbGVUeXBlID09PSAnbGluZWFyJyA/IHRoaXMueFNjYWxlKE51bWJlcih4KSkgOiB0aGlzLnhTY2FsZSh4KTtcbiAgICAgICAgICBjb25zdCBjeSA9IHRoaXMueVNjYWxlVHlwZSA9PT0gJ2xpbmVhcicgPyB0aGlzLnlTY2FsZShOdW1iZXIoeSkpIDogdGhpcy55U2NhbGUoeSk7XG5cbiAgICAgICAgICBjb25zdCBjb2xvciA9IHRoaXMuY29sb3JzLnNjYWxlVHlwZSA9PT0gJ2xpbmVhcicgPyB0aGlzLmNvbG9ycy5nZXRDb2xvcihyKSA6IHRoaXMuY29sb3JzLmdldENvbG9yKHNlcmllc05hbWUpO1xuXG4gICAgICAgICAgY29uc3QgaXNBY3RpdmUgPSAhdGhpcy5hY3RpdmVFbnRyaWVzLmxlbmd0aCA/IHRydWUgOiB0aGlzLmlzQWN0aXZlKHsgbmFtZTogc2VyaWVzTmFtZSB9KTtcbiAgICAgICAgICBjb25zdCBvcGFjaXR5ID0gaXNBY3RpdmUgPyAxIDogMC4zO1xuXG4gICAgICAgICAgY29uc3QgZGF0YSA9IE9iamVjdC5hc3NpZ24oe30sIGQsIHtcbiAgICAgICAgICAgIHNlcmllczogc2VyaWVzTmFtZSxcbiAgICAgICAgICAgIG5hbWU6IGQubmFtZSxcbiAgICAgICAgICAgIHZhbHVlOiBkLnksXG4gICAgICAgICAgICB4OiBkLngsXG4gICAgICAgICAgICByYWRpdXM6IGQuclxuICAgICAgICAgIH0pO1xuXG4gICAgICAgICAgcmV0dXJuIHtcbiAgICAgICAgICAgIGRhdGEsXG4gICAgICAgICAgICB4LFxuICAgICAgICAgICAgeSxcbiAgICAgICAgICAgIHIsXG4gICAgICAgICAgICBjbGFzc05hbWVzOiBbYGNpcmNsZS1kYXRhLSR7aX1gXSxcbiAgICAgICAgICAgIHZhbHVlOiB5LFxuICAgICAgICAgICAgbGFiZWw6IHgsXG4gICAgICAgICAgICBjeCxcbiAgICAgICAgICAgIGN5LFxuICAgICAgICAgICAgcmFkaXVzLFxuICAgICAgICAgICAgdG9vbHRpcExhYmVsLFxuICAgICAgICAgICAgY29sb3IsXG4gICAgICAgICAgICBvcGFjaXR5LFxuICAgICAgICAgICAgc2VyaWVzTmFtZSxcbiAgICAgICAgICAgIGlzQWN0aXZlLFxuICAgICAgICAgICAgdHJhbnNmb3JtOiBgdHJhbnNsYXRlKCR7Y3h9LCR7Y3l9KWBcbiAgICAgICAgICB9O1xuICAgICAgICB9XG4gICAgICB9KVxuICAgICAgLmZpbHRlcihjaXJjbGUgPT4gY2lyY2xlICE9PSB1bmRlZmluZWQpO1xuICB9XG5cbiAgZ2V0VG9vbHRpcFRleHQoY2lyY2xlKTogc3RyaW5nIHtcbiAgICBjb25zdCBoYXNSYWRpdXMgPSB0eXBlb2YgY2lyY2xlLnIgIT09ICd1bmRlZmluZWQnO1xuICAgIGNvbnN0IGhhc1Rvb2x0aXBMYWJlbCA9IGNpcmNsZS50b29sdGlwTGFiZWwgJiYgY2lyY2xlLnRvb2x0aXBMYWJlbC5sZW5ndGg7XG4gICAgY29uc3QgaGFzU2VyaWVzTmFtZSA9IGNpcmNsZS5zZXJpZXNOYW1lICYmIGNpcmNsZS5zZXJpZXNOYW1lLmxlbmd0aDtcblxuICAgIGNvbnN0IHJhZGl1c1ZhbHVlID0gaGFzUmFkaXVzID8gZm9ybWF0TGFiZWwoY2lyY2xlLnIpIDogJyc7XG4gICAgY29uc3QgeEF4aXNMYWJlbCA9IHRoaXMueEF4aXNMYWJlbCAmJiB0aGlzLnhBeGlzTGFiZWwgIT09ICcnID8gYCR7dGhpcy54QXhpc0xhYmVsfTpgIDogJyc7XG4gICAgY29uc3QgeUF4aXNMYWJlbCA9IHRoaXMueUF4aXNMYWJlbCAmJiB0aGlzLnlBeGlzTGFiZWwgIT09ICcnID8gYCR7dGhpcy55QXhpc0xhYmVsfTpgIDogJyc7XG4gICAgY29uc3QgeCA9IGZvcm1hdExhYmVsKGNpcmNsZS54KTtcbiAgICBjb25zdCB5ID0gZm9ybWF0TGFiZWwoY2lyY2xlLnkpO1xuICAgIGNvbnN0IG5hbWUgPVxuICAgICAgaGFzU2VyaWVzTmFtZSAmJiBoYXNUb29sdGlwTGFiZWxcbiAgICAgICAgPyBgJHtjaXJjbGUuc2VyaWVzTmFtZX0g4oCiICR7Y2lyY2xlLnRvb2x0aXBMYWJlbH1gXG4gICAgICAgIDogY2lyY2xlLnNlcmllc05hbWUgKyBjaXJjbGUudG9vbHRpcExhYmVsO1xuICAgIGNvbnN0IHRvb2x0aXBUaXRsZSA9XG4gICAgICBoYXNTZXJpZXNOYW1lIHx8IGhhc1Rvb2x0aXBMYWJlbCA/IGA8c3BhbiBjbGFzcz1cInRvb2x0aXAtbGFiZWxcIj4ke2VzY2FwZUxhYmVsKG5hbWUpfTwvc3Bhbj5gIDogJyc7XG5cbiAgICByZXR1cm4gYFxuICAgICAgJHt0b29sdGlwVGl0bGV9XG4gICAgICA8c3BhbiBjbGFzcz1cInRvb2x0aXAtbGFiZWxcIj5cbiAgICAgICAgPGxhYmVsPiR7ZXNjYXBlTGFiZWwoeEF4aXNMYWJlbCl9PC9sYWJlbD4gJHtlc2NhcGVMYWJlbCh4KX08YnIgLz5cbiAgICAgICAgPGxhYmVsPiR7ZXNjYXBlTGFiZWwoeUF4aXNMYWJlbCl9PC9sYWJlbD4gJHtlc2NhcGVMYWJlbCh5KX1cbiAgICAgIDwvc3Bhbj5cbiAgICAgIDxzcGFuIGNsYXNzPVwidG9vbHRpcC12YWxcIj5cbiAgICAgICAgJHtlc2NhcGVMYWJlbChyYWRpdXNWYWx1ZSl9XG4gICAgICA8L3NwYW4+XG4gICAgYDtcbiAgfVxuXG4gIG9uQ2xpY2soZGF0YSk6IHZvaWQge1xuICAgIHRoaXMuc2VsZWN0LmVtaXQoZGF0YSk7XG4gIH1cblxuICBpc0FjdGl2ZShlbnRyeSk6IGJvb2xlYW4ge1xuICAgIGlmICghdGhpcy5hY3RpdmVFbnRyaWVzKSByZXR1cm4gZmFsc2U7XG4gICAgY29uc3QgaXRlbSA9IHRoaXMuYWN0aXZlRW50cmllcy5maW5kKGQgPT4ge1xuICAgICAgcmV0dXJuIGVudHJ5Lm5hbWUgPT09IGQubmFtZTtcbiAgICB9KTtcbiAgICByZXR1cm4gaXRlbSAhPT0gdW5kZWZpbmVkO1xuICB9XG5cbiAgaXNWaXNpYmxlKGNpcmNsZSk6IGJvb2xlYW4ge1xuICAgIGlmICh0aGlzLmFjdGl2ZUVudHJpZXMubGVuZ3RoID4gMCkge1xuICAgICAgcmV0dXJuIHRoaXMuaXNBY3RpdmUoeyBuYW1lOiBjaXJjbGUuc2VyaWVzTmFtZSB9KTtcbiAgICB9XG5cbiAgICByZXR1cm4gY2lyY2xlLm9wYWNpdHkgIT09IDA7XG4gIH1cblxuICBhY3RpdmF0ZUNpcmNsZShjaXJjbGUpOiB2b2lkIHtcbiAgICBjaXJjbGUuYmFyVmlzaWJsZSA9IHRydWU7XG4gICAgdGhpcy5hY3RpdmF0ZS5lbWl0KHsgbmFtZTogdGhpcy5kYXRhLm5hbWUgfSk7XG4gIH1cblxuICBkZWFjdGl2YXRlQ2lyY2xlKGNpcmNsZSk6IHZvaWQge1xuICAgIGNpcmNsZS5iYXJWaXNpYmxlID0gZmFsc2U7XG4gICAgdGhpcy5kZWFjdGl2YXRlLmVtaXQoeyBuYW1lOiB0aGlzLmRhdGEubmFtZSB9KTtcbiAgfVxuXG4gIHRyYWNrQnkoaW5kZXgsIGNpcmNsZSk6IHN0cmluZyB7XG4gICAgcmV0dXJuIGAke2NpcmNsZS5kYXRhLnNlcmllc30gJHtjaXJjbGUuZGF0YS5uYW1lfWA7XG4gIH1cbn1cbiJdfQ==