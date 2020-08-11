import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ViewChild, ChangeDetectionStrategy } from '@angular/core';
import { trigger, style, animate, transition } from '@angular/animations';
import { createMouseEvent } from '../events';
let TooltipArea = class TooltipArea {
    constructor() {
        this.anchorOpacity = 0;
        this.anchorPos = -1;
        this.anchorValues = [];
        this.showPercentage = false;
        this.tooltipDisabled = false;
        this.hover = new EventEmitter();
    }
    getValues(xVal) {
        const results = [];
        for (const group of this.results) {
            const item = group.series.find(d => d.name.toString() === xVal.toString());
            let groupName = group.name;
            if (groupName instanceof Date) {
                groupName = groupName.toLocaleDateString();
            }
            if (item) {
                const label = item.name;
                let val = item.value;
                if (this.showPercentage) {
                    val = (item.d1 - item.d0).toFixed(2) + '%';
                }
                let color;
                if (this.colors.scaleType === 'linear') {
                    let v = val;
                    if (item.d1) {
                        v = item.d1;
                    }
                    color = this.colors.getColor(v);
                }
                else {
                    color = this.colors.getColor(group.name);
                }
                const data = Object.assign({}, item, {
                    value: val,
                    name: label,
                    series: groupName,
                    min: item.min,
                    max: item.max,
                    color
                });
                results.push(data);
            }
        }
        return results;
    }
    mouseMove(event) {
        const xPos = event.pageX - event.target.getBoundingClientRect().left;
        const closestIndex = this.findClosestPointIndex(xPos);
        const closestPoint = this.xSet[closestIndex];
        this.anchorPos = this.xScale(closestPoint);
        this.anchorPos = Math.max(0, this.anchorPos);
        this.anchorPos = Math.min(this.dims.width, this.anchorPos);
        this.anchorValues = this.getValues(closestPoint);
        if (this.anchorPos !== this.lastAnchorPos) {
            const ev = createMouseEvent('mouseleave');
            this.tooltipAnchor.nativeElement.dispatchEvent(ev);
            this.anchorOpacity = 0.7;
            this.hover.emit({
                value: closestPoint
            });
            this.showTooltip();
            this.lastAnchorPos = this.anchorPos;
        }
    }
    findClosestPointIndex(xPos) {
        let minIndex = 0;
        let maxIndex = this.xSet.length - 1;
        let minDiff = Number.MAX_VALUE;
        let closestIndex = 0;
        while (minIndex <= maxIndex) {
            const currentIndex = ((minIndex + maxIndex) / 2) | 0;
            const currentElement = this.xScale(this.xSet[currentIndex]);
            const curDiff = Math.abs(currentElement - xPos);
            if (curDiff < minDiff) {
                minDiff = curDiff;
                closestIndex = currentIndex;
            }
            if (currentElement < xPos) {
                minIndex = currentIndex + 1;
            }
            else if (currentElement > xPos) {
                maxIndex = currentIndex - 1;
            }
            else {
                minDiff = 0;
                closestIndex = currentIndex;
                break;
            }
        }
        return closestIndex;
    }
    showTooltip() {
        const event = createMouseEvent('mouseenter');
        this.tooltipAnchor.nativeElement.dispatchEvent(event);
    }
    hideTooltip() {
        const event = createMouseEvent('mouseleave');
        this.tooltipAnchor.nativeElement.dispatchEvent(event);
        this.anchorOpacity = 0;
        this.lastAnchorPos = -1;
    }
    getToolTipText(tooltipItem) {
        let result = '';
        if (tooltipItem.series !== undefined) {
            result += tooltipItem.series;
        }
        else {
            result += '???';
        }
        result += ': ';
        if (tooltipItem.value !== undefined) {
            result += tooltipItem.value.toLocaleString();
        }
        if (tooltipItem.min !== undefined || tooltipItem.max !== undefined) {
            result += ' (';
            if (tooltipItem.min !== undefined) {
                if (tooltipItem.max === undefined) {
                    result += '≥';
                }
                result += tooltipItem.min.toLocaleString();
                if (tooltipItem.max !== undefined) {
                    result += ' - ';
                }
            }
            else if (tooltipItem.max !== undefined) {
                result += '≤';
            }
            if (tooltipItem.max !== undefined) {
                result += tooltipItem.max.toLocaleString();
            }
            result += ')';
        }
        return result;
    }
};
__decorate([
    Input()
], TooltipArea.prototype, "dims", void 0);
__decorate([
    Input()
], TooltipArea.prototype, "xSet", void 0);
__decorate([
    Input()
], TooltipArea.prototype, "xScale", void 0);
__decorate([
    Input()
], TooltipArea.prototype, "yScale", void 0);
__decorate([
    Input()
], TooltipArea.prototype, "results", void 0);
__decorate([
    Input()
], TooltipArea.prototype, "colors", void 0);
__decorate([
    Input()
], TooltipArea.prototype, "showPercentage", void 0);
__decorate([
    Input()
], TooltipArea.prototype, "tooltipDisabled", void 0);
__decorate([
    Input()
], TooltipArea.prototype, "tooltipTemplate", void 0);
__decorate([
    Output()
], TooltipArea.prototype, "hover", void 0);
__decorate([
    ViewChild('tooltipAnchor', { static: false })
], TooltipArea.prototype, "tooltipAnchor", void 0);
TooltipArea = __decorate([
    Component({
        selector: 'g[ngx-charts-tooltip-area]',
        template: `
    <svg:g>
      <svg:rect
        class="tooltip-area"
        [attr.x]="0"
        y="0"
        [attr.width]="dims.width"
        [attr.height]="dims.height"
        style="opacity: 0; cursor: 'auto';"
        (mousemove)="mouseMove($event)"
        (mouseleave)="hideTooltip()"
      />
      <ng-template #defaultTooltipTemplate let-model="model">
        <xhtml:div class="area-tooltip-container">
          <xhtml:div *ngFor="let tooltipItem of model" class="tooltip-item">
            <xhtml:span class="tooltip-item-color" [style.background-color]="tooltipItem.color"></xhtml:span>
            {{ getToolTipText(tooltipItem) }}
          </xhtml:div>
        </xhtml:div>
      </ng-template>
      <svg:rect
        #tooltipAnchor
        [@animationState]="anchorOpacity !== 0 ? 'active' : 'inactive'"
        class="tooltip-anchor"
        [attr.x]="anchorPos"
        y="0"
        [attr.width]="1"
        [attr.height]="dims.height"
        [style.opacity]="anchorOpacity"
        [style.pointer-events]="'none'"
        ngx-tooltip
        [tooltipDisabled]="tooltipDisabled"
        [tooltipPlacement]="'right'"
        [tooltipType]="'tooltip'"
        [tooltipSpacing]="15"
        [tooltipTemplate]="tooltipTemplate ? tooltipTemplate : defaultTooltipTemplate"
        [tooltipContext]="anchorValues"
        [tooltipImmediateExit]="true"
      />
    </svg:g>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush,
        animations: [
            trigger('animationState', [
                transition('inactive => active', [
                    style({
                        opacity: 0
                    }),
                    animate(250, style({ opacity: 0.7 }))
                ]),
                transition('active => inactive', [
                    style({
                        opacity: 0.7
                    }),
                    animate(250, style({ opacity: 0 }))
                ])
            ])
        ]
    })
], TooltipArea);
export { TooltipArea };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidG9vbHRpcC1hcmVhLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi90b29sdGlwLWFyZWEuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQUUsU0FBUyxFQUFFLEtBQUssRUFBRSxNQUFNLEVBQUUsWUFBWSxFQUFFLFNBQVMsRUFBRSx1QkFBdUIsRUFBZSxNQUFNLGVBQWUsQ0FBQztBQUN4SCxPQUFPLEVBQUUsT0FBTyxFQUFFLEtBQUssRUFBRSxPQUFPLEVBQUUsVUFBVSxFQUFFLE1BQU0scUJBQXFCLENBQUM7QUFDMUUsT0FBTyxFQUFFLGdCQUFnQixFQUFFLE1BQU0sV0FBVyxDQUFDO0FBK0Q3QyxJQUFhLFdBQVcsR0FBeEIsTUFBYSxXQUFXO0lBQXhCO1FBQ0Usa0JBQWEsR0FBVyxDQUFDLENBQUM7UUFDMUIsY0FBUyxHQUFXLENBQUMsQ0FBQyxDQUFDO1FBQ3ZCLGlCQUFZLEdBQVUsRUFBRSxDQUFDO1FBU2hCLG1CQUFjLEdBQVksS0FBSyxDQUFDO1FBQ2hDLG9CQUFlLEdBQVksS0FBSyxDQUFDO1FBR2hDLFVBQUssR0FBRyxJQUFJLFlBQVksRUFBRSxDQUFDO0lBZ0p2QyxDQUFDO0lBNUlDLFNBQVMsQ0FBQyxJQUFJO1FBQ1osTUFBTSxPQUFPLEdBQUcsRUFBRSxDQUFDO1FBRW5CLEtBQUssTUFBTSxLQUFLLElBQUksSUFBSSxDQUFDLE9BQU8sRUFBRTtZQUNoQyxNQUFNLElBQUksR0FBRyxLQUFLLENBQUMsTUFBTSxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsUUFBUSxFQUFFLEtBQUssSUFBSSxDQUFDLFFBQVEsRUFBRSxDQUFDLENBQUM7WUFDM0UsSUFBSSxTQUFTLEdBQUcsS0FBSyxDQUFDLElBQUksQ0FBQztZQUMzQixJQUFJLFNBQVMsWUFBWSxJQUFJLEVB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Z0JBQ0YsVUFBVSxDQUFDLG9CQUFvQixFQUFFO29CQUMvQixLQUFLLENBQUM7d0JBQ0osT0FBTyxFQUFFLEdBQUc7cUJBQ2IsQ0FBQztvQkFDRixPQUFPLENBQUMsR0FBRyxFQUFFLEtBQUssQ0FBQyxFQUFFLE9BQU8sRUFBRSxDQUFDLEVBQUUsQ0FBQyxDQUFDO2lCQUNwQyxDQUFDO2FBQ0gsQ0FBQztTQUNIO0tBQ0YsQ0FBQztHQUNXLFdBQVcsQ0FnS3ZCO1NBaEtZLFdBQVciLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgeyBDb21wb25lbnQsIElucHV0LCBPdXRwdXQsIEV2ZW50RW1pdHRlciwgVmlld0NoaWxkLCBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSwgVGVtcGxhdGVSZWYgfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IHRyaWdnZXIsIHN0eWxlLCBhbmltYXRlLCB0cmFuc2l0aW9uIH0gZnJvbSAnQGFuZ3VsYXIvYW5pbWF0aW9ucyc7XG5pbXBvcnQgeyBjcmVhdGVNb3VzZUV2ZW50IH0gZnJvbSAnLi4vZXZlbnRzJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnZ1tuZ3gtY2hhcnRzLXRvb2x0aXAtYXJlYV0nLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxzdmc6Zz5cbiAgICAgIDxzdmc6cmVjdFxuICAgICAgICBjbGFzcz1cInRvb2x0aXAtYXJlYVwiXG4gICAgICAgIFthdHRyLnhdPVwiMFwiXG4gICAgICAgIHk9XCIwXCJcbiAgICAgICAgW2F0dHIud2lkdGhdPVwiZGltcy53aWR0aFwiXG4gICAgICAgIFthdHRyLmhlaWdodF09XCJkaW1zLmhlaWdodFwiXG4gICAgICAgIHN0eWxlPVwib3BhY2l0eTogMDsgY3Vyc29yOiAnYXV0byc7XCJcbiAgICAgICAgKG1vdXNlbW92ZSk9XCJtb3VzZU1vdmUoJGV2ZW50KVwiXG4gICAgICAgIChtb3VzZWxlYXZlKT1cImhpZGVUb29sdGlwKClcIlxuICAgICAgLz5cbiAgICAgIDxuZy10ZW1wbGF0ZSAjZGVmYXVsdFRvb2x0aXBUZW1wbGF0ZSBsZXQtbW9kZWw9XCJtb2RlbFwiPlxuICAgICAgICA8eGh0bWw6ZGl2IGNsYXNzPVwiYXJlYS10b29sdGlwLWNvbnRhaW5lclwiPlxuICAgICAgICAgIDx4aHRtbDpkaXYgKm5nRm9yPVwibGV0IHRvb2x0aXBJdGVtIG9mIG1vZGVsXCIgY2xhc3M9XCJ0b29sdGlwLWl0ZW1cIj5cbiAgICAgICAgICAgIDx4aHRtbDpzcGFuIGNsYXNzPVwidG9vbHRpcC1pdGVtLWNvbG9yXCIgW3N0eWxlLmJhY2tncm91bmQtY29sb3JdPVwidG9vbHRpcEl0ZW0uY29sb3JcIj48L3hodG1sOnNwYW4+XG4gICAgICAgICAgICB7eyBnZXRUb29sVGlwVGV4dCh0b29sdGlwSXRlbSkgfX1cbiAgICAgICAgICA8L3hodG1sOmRpdj5cbiAgICAgICAgPC94aHRtbDpkaXY+XG4gICAgICA8L25nLXRlbXBsYXRlPlxuICAgICAgPHN2ZzpyZWN0XG4gICAgICAgICN0b29sdGlwQW5jaG9yXG4gICAgICAgIFtAYW5pbWF0aW9uU3RhdGVdPVwiYW5jaG9yT3BhY2l0eSAhPT0gMCA/ICdhY3RpdmUnIDogJ2luYWN0aXZlJ1wiXG4gICAgICAgIGNsYXNzPVwidG9vbHRpcC1hbmNob3JcIlxuICAgICAgICBbYXR0ci54XT1cImFuY2hvclBvc1wiXG4gICAgICAgIHk9XCIwXCJcbiAgICAgICAgW2F0dHIud2lkdGhdPVwiMVwiXG4gICAgICAgIFthdHRyLmhlaWdodF09XCJkaW1zLmhlaWdodFwiXG4gICAgICAgIFtzdHlsZS5vcGFjaXR5XT1cImFuY2hvck9wYWNpdHlcIlxuICAgICAgICBbc3R5bGUucG9pbnRlci1ldmVudHNdPVwiJ25vbmUnXCJcbiAgICAgICAgbmd4LXRvb2x0aXBcbiAgICAgICAgW3Rvb2x0aXBEaXNhYmxlZF09XCJ0b29sdGlwRGlzYWJsZWRcIlxuICAgICAgICBbdG9vbHRpcFBsYWNlbWVudF09XCIncmlnaHQnXCJcbiAgICAgICAgW3Rvb2x0aXBUeXBlXT1cIid0b29sdGlwJ1wiXG4gICAgICAgIFt0b29sdGlwU3BhY2luZ109XCIxNVwiXG4gICAgICAgIFt0b29sdGlwVGVtcGxhdGVdPVwidG9vbHRpcFRlbXBsYXRlID8gdG9vbHRpcFRlbXBsYXRlIDogZGVmYXVsdFRvb2x0aXBUZW1wbGF0ZVwiXG4gICAgICAgIFt0b29sdGlwQ29udGV4dF09XCJhbmNob3JWYWx1ZXNcIlxuICAgICAgICBbdG9vbHRpcEltbWVkaWF0ZUV4aXRdPVwidHJ1ZVwiXG4gICAgICAvPlxuICAgIDwvc3ZnOmc+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoLFxuICBhbmltYXRpb25zOiBbXG4gICAgdHJpZ2dlcignYW5pbWF0aW9uU3RhdGUnLCBbXG4gICAgICB0cmFuc2l0aW9uKCdpbmFjdGl2ZSA9PiBhY3RpdmUnLCBbXG4gICAgICAgIHN0eWxlKHtcbiAgICAgICAgICBvcGFjaXR5OiAwXG4gICAgICAgIH0pLFxuICAgICAgICBhbmltYXRlKDI1MCwgc3R5bGUoeyBvcGFjaXR5OiAwLjcgfSkpXG4gICAgICBdKSxcbiAgICAgIHRyYW5zaXRpb24oJ2FjdGl2ZSA9PiBpbmFjdGl2ZScsIFtcbiAgICAgICAgc3R5bGUoe1xuICAgICAgICAgIG9wYWNpdHk6IDAuN1xuICAgICAgICB9KSxcbiAgICAgICAgYW5pbWF0ZSgyNTAsIHN0eWxlKHsgb3BhY2l0eTogMCB9KSlcbiAgICAgIF0pXG4gICAgXSlcbiAgXVxufSlcbmV4cG9ydCBjbGFzcyBUb29sdGlwQXJlYSB7XG4gIGFuY2hvck9wYWNpdHk6IG51bWJlciA9IDA7XG4gIGFuY2hvclBvczogbnVtYmVyID0gLTE7XG4gIGFuY2hvclZhbHVlczogYW55W10gPSBbXTtcbiAgbGFzdEFuY2hvclBvczogbnVtYmVyO1xuXG4gIEBJbnB1dCgpIGRpbXM7XG4gIEBJbnB1dCgpIHhTZXQ7XG4gIEBJbnB1dCgpIHhTY2FsZTtcbiAgQElucHV0KCkgeVNjYWxlO1xuICBASW5wdXQoKSByZXN1bHRzO1xuICBASW5wdXQoKSBjb2xvcnM7XG4gIEBJbnB1dCgpIHNob3dQZXJjZW50YWdlOiBib29sZWFuID0gZmFsc2U7XG4gIEBJbnB1dCgpIHRvb2x0aXBEaXNhYmxlZDogYm9vbGVhbiA9IGZhbHNlO1xuICBASW5wdXQoKSB0b29sdGlwVGVtcGxhdGU6IFRlbXBsYXRlUmVmPGFueT47XG5cbiAgQE91dHB1dCgpIGhvdmVyID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuXG4gIEBWaWV3Q2hpbGQoJ3Rvb2x0aXBBbmNob3InLCB7IHN0YXRpYzogZmFsc2UgfSkgdG9vbHRpcEFuY2hvcjtcblxuICBnZXRWYWx1ZXMoeFZhbCk6IGFueVtdIHtcbiAgICBjb25zdCByZXN1bHRzID0gW107XG5cbiAgICBmb3IgKGNvbnN0IGdyb3VwIG9mIHRoaXMucmVzdWx0cykge1xuICAgICAgY29uc3QgaXRlbSA9IGdyb3VwLnNlcmllcy5maW5kKGQgPT4gZC5uYW1lLnRvU3RyaW5nKCkgPT09IHhWYWwudG9TdHJpbmcoKSk7XG4gICAgICBsZXQgZ3JvdXBOYW1lID0gZ3JvdXAubmFtZTtcbiAgICAgIGlmIChncm91cE5hbWUgaW5zdGFuY2VvZiBEYXRlKSB7XG4gICAgICAgIGdyb3VwTmFtZSA9IGdyb3VwTmFtZS50b0xvY2FsZURhdGVTdHJpbmcoKTtcbiAgICAgIH1cblxuICAgICAgaWYgKGl0ZW0pIHtcbiAgICAgICAgY29uc3QgbGFiZWwgPSBpdGVtLm5hbWU7XG4gICAgICAgIGxldCB2YWwgPSBpdGVtLnZhbHVlO1xuICAgICAgICBpZiAodGhpcy5zaG93UGVyY2VudGFnZSkge1xuICAgICAgICAgIHZhbCA9IChpdGVtLmQxIC0gaXRlbS5kMCkudG9GaXhlZCgyKSArICclJztcbiAgICAgICAgfVxuICAgICAgICBsZXQgY29sb3I7XG4gICAgICAgIGlmICh0aGlzLmNvbG9ycy5zY2FsZVR5cGUgPT09ICdsaW5lYXInKSB7XG4gICAgICAgICAgbGV0IHYgPSB2YWw7XG4gICAgICAgICAgaWYgKGl0ZW0uZDEpIHtcbiAgICAgICAgICAgIHYgPSBpdGVtLmQxO1xuICAgICAgICAgIH1cbiAgICAgICAgICBjb2xvciA9IHRoaXMuY29sb3JzLmdldENvbG9yKHYpO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgIGNvbG9yID0gdGhpcy5jb2xvcnMuZ2V0Q29sb3IoZ3JvdXAubmFtZSk7XG4gICAgICAgIH1cblxuICAgICAgICBjb25zdCBkYXRhID0gT2JqZWN0LmFzc2lnbih7fSwgaXRlbSwge1xuICAgICAgICAgIHZhbHVlOiB2YWwsXG4gICAgICAgICAgbmFtZTogbGFiZWwsXG4gICAgICAgICAgc2VyaWVzOiBncm91cE5hbWUsXG4gICAgICAgICAgbWluOiBpdGVtLm1pbixcbiAgICAgICAgICBtYXg6IGl0ZW0ubWF4LFxuICAgICAgICAgIGNvbG9yXG4gICAgICAgIH0pO1xuXG4gICAgICAgIHJlc3VsdHMucHVzaChkYXRhKTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gcmVzdWx0cztcbiAgfVxuXG4gIG1vdXNlTW92ZShldmVudCkge1xuICAgIGNvbnN0IHhQb3MgPSBldmVudC5wYWdlWCAtIGV2ZW50LnRhcmdldC5nZXRCb3VuZGluZ0NsaWVudFJlY3QoKS5sZWZ0O1xuXG4gICAgY29uc3QgY2xvc2VzdEluZGV4ID0gdGhpcy5maW5kQ2xvc2VzdFBvaW50SW5kZXgoeFBvcyk7XG4gICAgY29uc3QgY2xvc2VzdFBvaW50ID0gdGhpcy54U2V0W2Nsb3Nlc3RJbmRleF07XG4gICAgdGhpcy5hbmNob3JQb3MgPSB0aGlzLnhTY2FsZShjbG9zZXN0UG9pbnQpO1xuICAgIHRoaXMuYW5jaG9yUG9zID0gTWF0aC5tYXgoMCwgdGhpcy5hbmNob3JQb3MpO1xuICAgIHRoaXMuYW5jaG9yUG9zID0gTWF0aC5taW4odGhpcy5kaW1zLndpZHRoLCB0aGlzLmFuY2hvclBvcyk7XG5cbiAgICB0aGlzLmFuY2hvclZhbHVlcyA9IHRoaXMuZ2V0VmFsdWVzKGNsb3Nlc3RQb2ludCk7XG4gICAgaWYgKHRoaXMuYW5jaG9yUG9zICE9PSB0aGlzLmxhc3RBbmNob3JQb3MpIHtcbiAgICAgIGNvbnN0IGV2ID0gY3JlYXRlTW91c2VFdmVudCgnbW91c2VsZWF2ZScpO1xuICAgICAgdGhpcy50b29sdGlwQW5jaG9yLm5hdGl2ZUVsZW1lbnQuZGlzcGF0Y2hFdmVudChldik7XG4gICAgICB0aGlzLmFuY2hvck9wYWNpdHkgPSAwLjc7XG4gICAgICB0aGlzLmhvdmVyLmVtaXQoe1xuICAgICAgICB2YWx1ZTogY2xvc2VzdFBvaW50XG4gICAgICB9KTtcbiAgICAgIHRoaXMuc2hvd1Rvb2x0aXAoKTtcblxuICAgICAgdGhpcy5sYXN0QW5jaG9yUG9zID0gdGhpcy5hbmNob3JQb3M7XG4gICAgfVxuICB9XG5cbiAgZmluZENsb3Nlc3RQb2ludEluZGV4KHhQb3MpIHtcbiAgICBsZXQgbWluSW5kZXggPSAwO1xuICAgIGxldCBtYXhJbmRleCA9IHRoaXMueFNldC5sZW5ndGggLSAxO1xuICAgIGxldCBtaW5EaWZmID0gTnVtYmVyLk1BWF9WQUxVRTtcbiAgICBsZXQgY2xvc2VzdEluZGV4ID0gMDtcblxuICAgIHdoaWxlIChtaW5JbmRleCA8PSBtYXhJbmRleCkge1xuICAgICAgY29uc3QgY3VycmVudEluZGV4ID0gKChtaW5JbmRleCArIG1heEluZGV4KSAvIDIpIHwgMDtcbiAgICAgIGNvbnN0IGN1cnJlbnRFbGVtZW50ID0gdGhpcy54U2NhbGUodGhpcy54U2V0W2N1cnJlbnRJbmRleF0pO1xuXG4gICAgICBjb25zdCBjdXJEaWZmID0gTWF0aC5hYnMoY3VycmVudEVsZW1lbnQgLSB4UG9zKTtcblxuICAgICAgaWYgKGN1ckRpZmYgPCBtaW5EaWZmKSB7XG4gICAgICAgIG1pbkRpZmYgPSBjdXJEaWZmO1xuICAgICAgICBjbG9zZXN0SW5kZXggPSBjdXJyZW50SW5kZXg7XG4gICAgICB9XG5cbiAgICAgIGlmIChjdXJyZW50RWxlbWVudCA8IHhQb3MpIHtcbiAgICAgICAgbWluSW5kZXggPSBjdXJyZW50SW5kZXggKyAxO1xuICAgICAgfSBlbHNlIGlmIChjdXJyZW50RWxlbWVudCA+IHhQb3MpIHtcbiAgICAgICAgbWF4SW5kZXggPSBjdXJyZW50SW5kZXggLSAxO1xuICAgICAgfSBlbHNlIHtcbiAgICAgICAgbWluRGlmZiA9IDA7XG4gICAgICAgIGNsb3Nlc3RJbmRleCA9IGN1cnJlbnRJbmRleDtcbiAgICAgICAgYnJlYWs7XG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIGNsb3Nlc3RJbmRleDtcbiAgfVxuXG4gIHNob3dUb29sdGlwKCk6IHZvaWQge1xuICAgIGNvbnN0IGV2ZW50ID0gY3JlYXRlTW91c2VFdmVudCgnbW91c2VlbnRlcicpO1xuICAgIHRoaXMudG9vbHRpcEFuY2hvci5uYXRpdmVFbGVtZW50LmRpc3BhdGNoRXZlbnQoZXZlbnQpO1xuICB9XG5cbiAgaGlkZVRvb2x0aXAoKTogdm9pZCB7XG4gICAgY29uc3QgZXZlbnQgPSBjcmVhdGVNb3VzZUV2ZW50KCdtb3VzZWxlYXZlJyk7XG4gICAgdGhpcy50b29sdGlwQW5jaG9yLm5hdGl2ZUVsZW1lbnQuZGlzcGF0Y2hFdmVudChldmVudCk7XG4gICAgdGhpcy5hbmNob3JPcGFjaXR5ID0gMDtcbiAgICB0aGlzLmxhc3RBbmNob3JQb3MgPSAtMTtcbiAgfVxuXG4gIGdldFRvb2xUaXBUZXh0KHRvb2x0aXBJdGVtOiBhbnkpOiBzdHJpbmcge1xuICAgIGxldCByZXN1bHQ6IHN0cmluZyA9ICcnO1xuICAgIGlmICh0b29sdGlwSXRlbS5zZXJpZXMgIT09IHVuZGVmaW5lZCkge1xuICAgICAgcmVzdWx0ICs9IHRvb2x0aXBJdGVtLnNlcmllcztcbiAgICB9IGVsc2Uge1xuICAgICAgcmVzdWx0ICs9ICc/Pz8nO1xuICAgIH1cbiAgICByZXN1bHQgKz0gJzogJztcbiAgICBpZiAodG9vbHRpcEl0ZW0udmFsdWUgIT09IHVuZGVmaW5lZCkge1xuICAgICAgcmVzdWx0ICs9IHRvb2x0aXBJdGVtLnZhbHVlLnRvTG9jYWxlU3RyaW5nKCk7XG4gICAgfVxuICAgIGlmICh0b29sdGlwSXRlbS5taW4gIT09IHVuZGVmaW5lZCB8fCB0b29sdGlwSXRlbS5tYXggIT09IHVuZGVmaW5lZCkge1xuICAgICAgcmVzdWx0ICs9ICcgKCc7XG4gICAgICBpZiAodG9vbHRpcEl0ZW0ubWluICE9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgaWYgKHRvb2x0aXBJdGVtLm1heCA9PT0gdW5kZWZpbmVkKSB7XG4gICAgICAgICAgcmVzdWx0ICs9ICfiiaUnO1xuICAgICAgICB9XG4gICAgICAgIHJlc3VsdCArPSB0b29sdGlwSXRlbS5taW4udG9Mb2NhbGVTdHJpbmcoKTtcbiAgICAgICAgaWYgKHRvb2x0aXBJdGVtLm1heCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICAgICAgcmVzdWx0ICs9ICcgLSAnO1xuICAgICAgICB9XG4gICAgICB9IGVsc2UgaWYgKHRvb2x0aXBJdGVtLm1heCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICAgIHJlc3VsdCArPSAn4omkJztcbiAgICAgIH1cbiAgICAgIGlmICh0b29sdGlwSXRlbS5tYXggIT09IHVuZGVmaW5lZCkge1xuICAgICAgICByZXN1bHQgKz0gdG9vbHRpcEl0ZW0ubWF4LnRvTG9jYWxlU3RyaW5nKCk7XG4gICAgICB9XG4gICAgICByZXN1bHQgKz0gJyknO1xuICAgIH1cbiAgICByZXR1cm4gcmVzdWx0O1xuICB9XG59XG4iXX0=