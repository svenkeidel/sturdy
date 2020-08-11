import { __decorate } from "tslib";
import { ChangeDetectionStrategy, Component, EventEmitter, Input, Output, ViewEncapsulation } from '@angular/core';
import { trimLabel } from '../trim-label.helper';
import { formatLabel } from '../label.helper';
let AdvancedLegendComponent = class AdvancedLegendComponent {
    constructor() {
        this.label = 'Total';
        this.animations = true;
        this.select = new EventEmitter();
        this.activate = new EventEmitter();
        this.deactivate = new EventEmitter();
        this.legendItems = [];
        this.labelFormatting = label => label;
        this.percentageFormatting = percentage => percentage;
        this.defaultValueFormatting = value => value.toLocaleString();
    }
    ngOnChanges(changes) {
        this.update();
    }
    getTotal() {
        return this.data.map(d => d.value).reduce((sum, d) => sum + d, 0);
    }
    update() {
        this.total = this.getTotal();
        this.roundedTotal = this.total;
        this.legendItems = this.getLegendItems();
    }
    getLegendItems() {
        return this.data.map(d => {
            const label = formatLabel(d.name);
            const value = d.value;
            const color = this.colors.getColor(label);
            const percentage = this.total > 0 ? (value / this.total) * 100 : 0;
            const formattedLabel = typeof this.labelFormatting === 'function' ? this.labelFormatting(label) : label;
            return {
                _value: value,
                data: d,
                value,
                color,
                label: formattedLabel,
                displayLabel: trimLabel(formattedLabel, 20),
                origialLabel: d.name,
                percentage: this.percentageFormatting ? this.percentageFormatting(percentage) : percentage.toLocaleString()
            };
        });
    }
    trackBy(item) {
        return item.formattedLabel;
    }
};
__decorate([
    Input()
], AdvancedLegendComponent.prototype, "width", void 0);
__decorate([
    Input()
], AdvancedLegendComponent.prototype, "data", void 0);
__decorate([
    Input()
], AdvancedLegendComponent.prototype, "colors", void 0);
__decorate([
    Input()
], AdvancedLegendComponent.prototype, "label", void 0);
__decorate([
    Input()
], AdvancedLegendComponent.prototype, "animations", void 0);
__decorate([
    Output()
], AdvancedLegendComponent.prototype, "select", void 0);
__decorate([
    Output()
], AdvancedLegendComponent.prototype, "activate", void 0);
__decorate([
    Output()
], AdvancedLegendComponent.prototype, "deactivate", void 0);
__decorate([
    Input()
], AdvancedLegendComponent.prototype, "valueFormatting", void 0);
__decorate([
    Input()
], AdvancedLegendComponent.prototype, "labelFormatting", void 0);
__decorate([
    Input()
], AdvancedLegendComponent.prototype, "percentageFormatting", void 0);
AdvancedLegendComponent = __decorate([
    Component({
        selector: 'ngx-charts-advanced-legend',
        template: `
    <div class="advanced-pie-legend" [style.width.px]="width">
      <div
        *ngIf="animations"
        class="total-value"
        ngx-charts-count-up
        [countTo]="roundedTotal"
        [valueFormatting]="valueFormatting"
      ></div>
      <div class="total-value" *ngIf="!animations">
        {{ valueFormatting ? valueFormatting(roundedTotal) : defaultValueFormatting(roundedTotal) }}
      </div>
      <div class="total-label">
        {{ label }}
      </div>
      <div class="legend-items-container">
        <div class="legend-items">
          <div
            *ngFor="let legendItem of legendItems; trackBy: trackBy"
            tabindex="-1"
            class="legend-item"
            (mouseenter)="activate.emit(legendItem.data)"
            (mouseleave)="deactivate.emit(legendItem.data)"
            (click)="select.emit(legendItem.data)"
          >
            <div class="item-color" [style.border-left-color]="legendItem.color"></div>
            <div
              *ngIf="animations"
              class="item-value"
              ngx-charts-count-up
              [countTo]="legendItem._value"
              [valueFormatting]="valueFormatting"
            ></div>
            <div *ngIf="!animations" class="item-value">
              {{ valueFormatting ? valueFormatting(legendItem.value) : defaultValueFormatting(legendItem.value) }}
            </div>
            <div class="item-label">{{ legendItem.displayLabel }}</div>
            <div
              *ngIf="animations"
              class="item-percent"
              ngx-charts-count-up
              [countTo]="legendItem.percentage"
              [countSuffix]="'%'"
            ></div>
            <div *ngIf="!animations" class="item-percent">{{ legendItem.percentage.toLocaleString() }}%</div>
          </div>
        </div>
      </div>
    </div>
  `,
        encapsulation: ViewEncapsulation.None,
        changeDetection: ChangeDetectionStrategy.OnPush,
        styles: [".advanced-pie-legend{float:left;position:relative;top:50%;-webkit-transform:translate(0,-50%);transform:translate(0,-50%)}.advanced-pie-legend .total-value{font-size:36px}.advanced-pie-legend .total-label{font-size:24px;margin-bottom:19px}.advanced-pie-legend .legend-items-container{width:100%}.advanced-pie-legend .legend-items-container .legend-items{white-space:nowrap;overflow:auto}.advanced-pie-legend .legend-items-container .legend-items .legend-item{margin-right:20px;display:inline-block;cursor:pointer}.advanced-pie-legend .legend-items-container .legend-items .legend-item:focus{outline:0}.advanced-pie-legend .legend-items-container .legend-items .legend-item:hover{color:#000;-webkit-transition:.2s;transition:.2s}.advanced-pie-legend .legend-items-container .legend-items .legend-item .item-value{font-size:24px;margin-top:-6px;margin-left:11px}.advanced-pie-legend .legend-items-container .legend-items .legend-item .item-label{font-size:14px;opacity:.7;margin-left:11px;margin-top:-6px}.advanced-pie-legend .legend-items-container .legend-items .legend-item .item-percent{font-size:24px;opacity:.7;margin-left:11px}.advanced-pie-legend .legend-items-container .legend-items .legend-item .item-color{border-left:4px solid;width:4px;height:42px;float:left;margin-right:7px}"]
    })
], AdvancedLegendComponent);
export { AdvancedLegendComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYWR2YW5jZWQtbGVnZW5kLmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi9sZWdlbmQvYWR2YW5jZWQtbGVnZW5kLmNvbXBvbmVudC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUNMLHVCQUF1QixFQUN2QixTQUFTLEVBQ1QsWUFBWSxFQUNaLEtBQUssRUFFTCxNQUFNLEVBRU4saUJBQWlCLEVBQ2xCLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBRSxTQUFTLEVBQUUsTUFBTSxzQkFBc0IsQ0FBQztBQUNqRCxPQUFPLEVBQUUsV0FBVyxFQUFFLE1BQU0saUJBQWlCLENBQUM7QUEwRDlDLElBQWEsdUJBQXVCLEdBQXBDLE1BQWEsdUJBQXVCO0lBQXBDO1FBSVcsVUFBSyxHQUFXLE9BQU8sQ0FBQztRQUN4QixlQUFVLEdBQVksSUFBSSxDQUFDO1FBRTFCLFdBQU0sR0FBc0IsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUMvQyxhQUFRLEdBQXNCLElBQUksWUFBWSxFQUFFLENBQUM7UUFDakQsZUFBVSxHQUFzQixJQUFJLFlBQVksRUFBRSxDQUFDO1FBRTdELGdCQUFXLEdBQVUsRUFBRSxDQUFDO1FBS2Ysb0JBQWUsR0FBMkIsS0FBSyxDQUFDLEVBQUUsQ0FBQyxLQUFLLENBQUM7UUFDekQseUJBQW9CLEdBQTJCLFVBQVUsQ0FBQyxFQUFFLENBQUMsVUFBVSxDQUFDO1FBRWpGLDJCQUFzQixHQUEyQixLQUFLLENBQUMsRUFBRSxDQUFDLEtBQUssQ0FBQyxjQUFjLEVBQUUsQ0FBQztJQXlDbkYsQ0FBQztJQXZDQyxXQUFXLENBQUMsT0FBc0I7UUFDaEMsSUFBSSxDQUFDLE1BQU0sRUFBRSxDQUFDO0lBQ2hCLENBQUM7SUFFRCxRQUFRO1FBQ04sT0FBTyxJQUFJLENBQUMsSUFBSSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQyxLQUFLLENBQUMsQ0FBQyxNQUFNLENBQUMsQ0FBQyxHQUFHLEVBQUUsQ0FBQyxFQUFFLEVBQUUsQ0FBQyxHQUFHLEdBQUcsQ0FBQyxFQUFFLENBQUMsQ0FBQyxDQUFDO0lBQ3BFLENBQUM7SUFFRCxNQUFNO1FBQ0osSUFBSSxDQUFDLEtBQUssR0FBRyxJQUFJLENBQUMsUUFBUSxFQUFFLENBQUM7UUFDN0IsSUFBSSxDQUFDLFlBQVksR0FBRyxJQUFJLENBQUMsS0FBSyxDQUFDO1FBRS9CLElBQUksQ0FBQyxXQUFXLEdBQUcsSUFBSSxDQUFDLGNBQWMsRUFBRSxDQUFDO0lBQzNDLENBQUM7SUFFRCxjQUFjO1FBQ1osT0FBTyxJQUFJLENBQUMsSUFBSSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsRUFBRTtZQUN2QixNQUFNLEtBQUssR0FBRyxXQUFXLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxDQUFDO1lBQ2xDLE1BQU0sS0FBSyxHQUFHLENBQUMsQ0FBQyxLQUFLLENBQUM7WUFDdEIsTUFBTSxLQUFLLEdBQUcsSUFBSSxDQUFDLE1BQU0sQ0FBQyxRQUFRLENBQUMsS0FBSyxDQUFDLENBQUM7WUFDMUMsTUFBTSxVQUFVLEdBQUcsSUFBSSxDQUFDLEtBQUssR0FBRyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsS0FBSyxHQUFHLElBQUksQ0FBQyxLQUFLLENBQUMsR0FBRyxHQUFHLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQztZQUNuRSxNQUFNLGNBQWMsR0FBRyxPQUFPLElBQUksQ0FBQyxlQUFlLEtBQUssVUFBVSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsZUFBZSxDQUFDLEtBQUssQ0FBQyxDQUFDLENBQUMsQ0FBQyxLQUFLLENBQUM7WUFFeEcsT0FBTztnQkFDTCxNQUFNLEVBQUUsS0FBSztnQkFDYixJQUFJLEVBQUUsQ0FBQztnQkFDUCxLQUFLO2dCQUNMLEtBQUs7Z0JBQ0wsS0FBSyxFQUFFLGNBQWM7Z0JBQ3JCLFlBQVksRUFBRSxTQUFTLENBQUMsY0FBYyxFQUFFLEVBQUUsQ0FBQztnQkFDM0MsWUFBWSxFQUFFLENBQUMsQ0FBQyxJQUFJO2dCQUNwQixVQUFVLEVBQUUsSUFBSSxDQUFDLG9CQUFvQixDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsb0JBQW9CLENBQUMsVUFBVSxDQUFDLENBQUMsQ0FBQyxDQUFDLFVBQVUsQ0FBQyxjQUFjLEVBQUU7YUFDNUcsQ0FBQztRQUNKLENBQUMsQ0FBQyxDQUFDO0lBQ0wsQ0FBQztJQUVELE9BQU8sQ0FBQyxJQUFJO1FBQ1YsT0FBTyxJQUFJLENBQUMsY0FBYyxDQUFDO0lBQzdCLENBQUM7Q0FDRixDQUFBO0FBM0RVO0lBQVIsS0FBSyxFQUFFO3NEQUFlO0FBQ2Q7SUFBUixLQUFLLEVBQUU7cURBQU07QUFDTDtJQUFSLEtBQUssRUFBRTt1REFBUTtBQUNQO0lBQVIsS0FBSyxFQUFFO3NEQUF5QjtBQUN4QjtJQUFSLEtBQUssRUFBRTsyREFBNEI7QUFFMUI7SUFBVCxNQUFNLEVBQUU7dURBQWdEO0FBQy9DO0lBQVQsTUFBTSxFQUFFO3lEQUFrRDtBQUNqRDtJQUFULE1BQU0sRUFBRTsyREFBb0Q7QUFNcEQ7SUFBUixLQUFLLEVBQUU7Z0VBQXlDO0FBQ3hDO0lBQVIsS0FBSyxFQUFFO2dFQUEwRDtBQUN6RDtJQUFSLEtBQUssRUFBRTtxRUFBeUU7QUFqQnRFLHVCQUF1QjtJQXhEbkMsU0FBUyxDQUFDO1FBQ1QsUUFBUSxFQUFFLDRCQUE0QjtRQUN0QyxRQUFRLEVBQUU7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7R0FpRFQ7UUFFRCxhQUFhLEVBQUUsaUJBQWlCLENBQUMsSUFBSTtRQUNyQyxlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTs7S0FDaEQsQ0FBQztHQUNXLHVCQUF1QixDQTREbkM7U0E1RFksdUJBQXVCIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHtcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksXG4gIENvbXBvbmVudCxcbiAgRXZlbnRFbWl0dGVyLFxuICBJbnB1dCxcbiAgT25DaGFuZ2VzLFxuICBPdXRwdXQsXG4gIFNpbXBsZUNoYW5nZXMsXG4gIFZpZXdFbmNhcHN1bGF0aW9uXG59IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHsgdHJpbUxhYmVsIH0gZnJvbSAnLi4vdHJpbS1sYWJlbC5oZWxwZXInO1xuaW1wb3J0IHsgZm9ybWF0TGFiZWwgfSBmcm9tICcuLi9sYWJlbC5oZWxwZXInO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICduZ3gtY2hhcnRzLWFkdmFuY2VkLWxlZ2VuZCcsXG4gIHRlbXBsYXRlOiBgXG4gICAgPGRpdiBjbGFzcz1cImFkdmFuY2VkLXBpZS1sZWdlbmRcIiBbc3R5bGUud2lkdGgucHhdPVwid2lkdGhcIj5cbiAgICAgIDxkaXZcbiAgICAgICAgKm5nSWY9XCJhbmltYXRpb25zXCJcbiAgICAgICAgY2xhc3M9XCJ0b3RhbC12YWx1ZVwiXG4gICAgICAgIG5neC1jaGFydHMtY291bnQtdXBcbiAgICAgICAgW2NvdW50VG9dPVwicm91bmRlZFRvdGFsXCJcbiAgICAgICAgW3ZhbHVlRm9ybWF0dGluZ109XCJ2YWx1ZUZvcm1hdHRpbmdcIlxuICAgICAgPjwvZGl2PlxuICAgICAgPGRpdiBjbGFzcz1cInRvdGFsLXZhbHVlXCIgKm5nSWY9XCIhYW5pbWF0aW9uc1wiPlxuICAgICAgICB7eyB2YWx1ZUZvcm1hdHRpbmcgPyB2YWx1ZUZvcm1hdHRpbmcocm91bmRlZFRvdGFsKSA6IGRlZmF1bHRWYWx1ZUZvcm1hdHRpbmcocm91bmRlZFRvdGFsKSB9fVxuICAgICAgPC9kaXY+XG4gICAgICA8ZGl2IGNsYXNzPVwidG90YWwtbGFiZWxcIj5cbiAgICAgICAge3sgbGFiZWwgfX1cbiAgICAgIDwvZGl2PlxuICAgICAgPGRpdiBjbGFzcz1cImxlZ2VuZC1pdGVtcy1jb250YWluZXJcIj5cbiAgICAgICAgPGRpdiBjbGFzcz1cImxlZ2VuZC1pdGVtc1wiPlxuICAgICAgICAgIDxkaXZcbiAgICAgICAgICAgICpuZ0Zvcj1cImxldCBsZWdlbmRJdGVtIG9mIGxlZ2VuZEl0ZW1zOyB0cmFja0J5OiB0cmFja0J5XCJcbiAgICAgICAgICAgIHRhYmluZGV4PVwiLTFcIlxuICAgICAgICAgICAgY2xhc3M9XCJsZWdlbmQtaXRlbVwiXG4gICAgICAgICAgICAobW91c2VlbnRlcik9XCJhY3RpdmF0ZS5lbWl0KGxlZ2VuZEl0ZW0uZGF0YSlcIlxuICAgICAgICAgICAgKG1vdXNlbGVhdmUpPVwiZGVhY3RpdmF0ZS5lbWl0KGxlZ2VuZEl0ZW0uZGF0YSlcIlxuICAgICAgICAgICAgKGNsaWNrKT1cInNlbGVjdC5lbWl0KGxlZ2VuZEl0ZW0uZGF0YSlcIlxuICAgICAgICAgID5cbiAgICAgICAgICAgIDxkaXYgY2xhc3M9XCJpdGVtLWNvbG9yXCIgW3N0eWxlLmJvcmRlci1sZWZ0LWNvbG9yXT1cImxlZ2VuZEl0ZW0uY29sb3JcIj48L2Rpdj5cbiAgICAgICAgICAgIDxkaXZcbiAgICAgICAgICAgICAgKm5nSWY9XCJhbmltYXRpb25zXCJcbiAgICAgICAgICAgICAgY2xhc3M9XCJpdGVtLXZhbHVlXCJcbiAgICAgICAgICAgICAgbmd4LWNoYXJ0cy1jb3VudC11cFxuICAgICAgICAgICAgICBbY291bnRUb109XCJsZWdlbmRJdGVtLl92YWx1ZVwiXG4gICAgICAgICAgICAgIFt2YWx1ZUZvcm1hdHRpbmddPVwidmFsdWVGb3JtYXR0aW5nXCJcbiAgICAgICAgICAgID48L2Rpdj5cbiAgICAgICAgICAgIDxkaXYgKm5nSWY9XCIhYW5pbWF0aW9uc1wiIGNsYXNzPVwiaXRlbS12YWx1ZVwiPlxuICAgICAgICAgICAgICB7eyB2YWx1ZUZvcm1hdHRpbmcgPyB2YWx1ZUZvcm1hdHRpbmcobGVnZW5kSXRlbS52YWx1ZSkgOiBkZWZhdWx0VmFsdWVGb3JtYXR0aW5nKGxlZ2VuZEl0ZW0udmFsdWUpIH19XG4gICAgICAgICAgICA8L2Rpdj5cbiAgICAgICAgICAgIDxkaXYgY2xhc3M9XCJpdGVtLWxhYmVsXCI+e3sgbGVnZW5kSXRlbS5kaXNwbGF5TGFiZWwgfX08L2Rpdj5cbiAgICAgICAgICAgIDxkaXZcbiAgICAgICAgICAgICAgKm5nSWY9XCJhbmltYXRpb25zXCJcbiAgICAgICAgICAgICAgY2xhc3M9XCJpdGVtLXBlcmNlbnRcIlxuICAgICAgICAgICAgICBuZ3gtY2hhcnRzLWNvdW50LXVwXG4gICAgICAgICAgICAgIFtjb3VudFRvXT1cImxlZ2VuZEl0ZW0ucGVyY2VudGFnZVwiXG4gICAgICAgICAgICAgIFtjb3VudFN1ZmZpeF09XCInJSdcIlxuICAgICAgICAgICAgPjwvZGl2PlxuICAgICAgICAgICAgPGRpdiAqbmdJZj1cIiFhbmltYXRpb25zXCIgY2xhc3M9XCJpdGVtLXBlcmNlbnRcIj57eyBsZWdlbmRJdGVtLnBlcmNlbnRhZ2UudG9Mb2NhbGVTdHJpbmcoKSB9fSU8L2Rpdj5cbiAgICAgICAgICA8L2Rpdj5cbiAgICAgICAgPC9kaXY+XG4gICAgICA8L2Rpdj5cbiAgICA8L2Rpdj5cbiAgYCxcbiAgc3R5bGVVcmxzOiBbJy4vYWR2YW5jZWQtbGVnZW5kLmNvbXBvbmVudC5zY3NzJ10sXG4gIGVuY2Fwc3VsYXRpb246IFZpZXdFbmNhcHN1bGF0aW9uLk5vbmUsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIEFkdmFuY2VkTGVnZW5kQ29tcG9uZW50IGltcGxlbWVudHMgT25DaGFuZ2VzIHtcbiAgQElucHV0KCkgd2lkdGg6IG51bWJlcjtcbiAgQElucHV0KCkgZGF0YTtcbiAgQElucHV0KCkgY29sb3JzO1xuICBASW5wdXQoKSBsYWJlbDogc3RyaW5nID0gJ1RvdGFsJztcbiAgQElucHV0KCkgYW5pbWF0aW9uczogYm9vbGVhbiA9IHRydWU7XG5cbiAgQE91dHB1dCgpIHNlbGVjdDogRXZlbnRFbWl0dGVyPGFueT4gPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBhY3RpdmF0ZTogRXZlbnRFbWl0dGVyPGFueT4gPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBkZWFjdGl2YXRlOiBFdmVudEVtaXR0ZXI8YW55PiA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcblxuICBsZWdlbmRJdGVtczogYW55W10gPSBbXTtcbiAgdG90YWw6IG51bWJlcjtcbiAgcm91bmRlZFRvdGFsOiBudW1iZXI7XG5cbiAgQElucHV0KCkgdmFsdWVGb3JtYXR0aW5nOiAodmFsdWU6IG51bWJlcikgPT4gYW55O1xuICBASW5wdXQoKSBsYWJlbEZvcm1hdHRpbmc6ICh2YWx1ZTogc3RyaW5nKSA9PiBhbnkgPSBsYWJlbCA9PiBsYWJlbDtcbiAgQElucHV0KCkgcGVyY2VudGFnZUZvcm1hdHRpbmc6ICh2YWx1ZTogbnVtYmVyKSA9PiBhbnkgPSBwZXJjZW50YWdlID0+IHBlcmNlbnRhZ2U7XG5cbiAgZGVmYXVsdFZhbHVlRm9ybWF0dGluZzogKHZhbHVlOiBudW1iZXIpID0+IGFueSA9IHZhbHVlID0+IHZhbHVlLnRvTG9jYWxlU3RyaW5nKCk7XG5cbiAgbmdPbkNoYW5nZXMoY2hhbmdlczogU2ltcGxlQ2hhbmdlcyk6IHZvaWQge1xuICAgIHRoaXMudXBkYXRlKCk7XG4gIH1cblxuICBnZXRUb3RhbCgpOiBudW1iZXIge1xuICAgIHJldHVybiB0aGlzLmRhdGEubWFwKGQgPT4gZC52YWx1ZSkucmVkdWNlKChzdW0sIGQpID0+IHN1bSArIGQsIDApO1xuICB9XG5cbiAgdXBkYXRlKCk6IHZvaWQge1xuICAgIHRoaXMudG90YWwgPSB0aGlzLmdldFRvdGFsKCk7XG4gICAgdGhpcy5yb3VuZGVkVG90YWwgPSB0aGlzLnRvdGFsO1xuXG4gICAgdGhpcy5sZWdlbmRJdGVtcyA9IHRoaXMuZ2V0TGVnZW5kSXRlbXMoKTtcbiAgfVxuXG4gIGdldExlZ2VuZEl0ZW1zKCk6IGFueSB7XG4gICAgcmV0dXJuIHRoaXMuZGF0YS5tYXAoZCA9PiB7XG4gICAgICBjb25zdCBsYWJlbCA9IGZvcm1hdExhYmVsKGQubmFtZSk7XG4gICAgICBjb25zdCB2YWx1ZSA9IGQudmFsdWU7XG4gICAgICBjb25zdCBjb2xvciA9IHRoaXMuY29sb3JzLmdldENvbG9yKGxhYmVsKTtcbiAgICAgIGNvbnN0IHBlcmNlbnRhZ2UgPSB0aGlzLnRvdGFsID4gMCA/ICh2YWx1ZSAvIHRoaXMudG90YWwpICogMTAwIDogMDtcbiAgICAgIGNvbnN0IGZvcm1hdHRlZExhYmVsID0gdHlwZW9mIHRoaXMubGFiZWxGb3JtYXR0aW5nID09PSAnZnVuY3Rpb24nID8gdGhpcy5sYWJlbEZvcm1hdHRpbmcobGFiZWwpIDogbGFiZWw7XG5cbiAgICAgIHJldHVybiB7XG4gICAgICAgIF92YWx1ZTogdmFsdWUsXG4gICAgICAgIGRhdGE6IGQsXG4gICAgICAgIHZhbHVlLFxuICAgICAgICBjb2xvcixcbiAgICAgICAgbGFiZWw6IGZvcm1hdHRlZExhYmVsLFxuICAgICAgICBkaXNwbGF5TGFiZWw6IHRyaW1MYWJlbChmb3JtYXR0ZWRMYWJlbCwgMjApLFxuICAgICAgICBvcmlnaWFsTGFiZWw6IGQubmFtZSxcbiAgICAgICAgcGVyY2VudGFnZTogdGhpcy5wZXJjZW50YWdlRm9ybWF0dGluZyA/IHRoaXMucGVyY2VudGFnZUZvcm1hdHRpbmcocGVyY2VudGFnZSkgOiBwZXJjZW50YWdlLnRvTG9jYWxlU3RyaW5nKClcbiAgICAgIH07XG4gICAgfSk7XG4gIH1cblxuICB0cmFja0J5KGl0ZW0pIHtcbiAgICByZXR1cm4gaXRlbS5mb3JtYXR0ZWRMYWJlbDtcbiAgfVxufVxuIl19