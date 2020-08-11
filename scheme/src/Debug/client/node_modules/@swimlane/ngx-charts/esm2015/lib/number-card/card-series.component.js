import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy } from '@angular/core';
import { invertColor } from '../utils/color-utils';
let CardSeriesComponent = class CardSeriesComponent {
    constructor() {
        this.innerPadding = 15;
        this.emptyColor = 'rgba(0, 0, 0, 0)';
        this.animations = true;
        this.select = new EventEmitter();
    }
    ngOnChanges(changes) {
        this.update();
    }
    update() {
        if (this.data.length > 2) {
            const valueFormatting = this.valueFormatting || (card => card.value.toLocaleString());
            const sortedLengths = this.data
                .map(d => {
                const hasValue = d && d.data && typeof d.data.value !== 'undefined' && d.data.value !== null;
                return hasValue
                    ? valueFormatting({
                        data: d.data,
                        label: d ? d.data.name : '',
                        value: d && d.data ? d.data.value : ''
                    }).length
                    : 0;
            })
                .sort((a, b) => b - a);
            const idx = Math.ceil(this.data.length / 2);
            this.medianSize = sortedLengths[idx];
        }
        const cards = this.getCards();
        this.cards = cards.filter(d => d.data.value !== null);
        this.emptySlots = cards.filter(d => d.data.value === null);
    }
    getCards() {
        const yPadding = typeof this.innerPadding === 'number' ? this.innerPadding : this.innerPadding[0] + this.innerPadding[2];
        const xPadding = typeof this.innerPadding === 'number' ? this.innerPadding : this.innerPadding[1] + this.innerPadding[3];
        return this.data.map((d, index) => {
            let label = d.data.name;
            if (label && label.constructor.name === 'Date') {
                label = label.toLocaleDateString();
            }
            else {
                label = label ? label.toLocaleString() : label;
            }
            const value = d.data.value;
            const valueColor = label ? this.colors.getColor(label) : this.emptyColor;
            const color = this.cardColor || valueColor || '#000';
            return {
                x: d.x,
                y: d.y,
                width: d.width - xPadding,
                height: d.height - yPadding,
                color,
                bandColor: this.bandColor || valueColor,
                textColor: this.textColor || invertColor(color),
                label,
                data: d.data,
                tooltipText: `${label}: ${value}`
            };
        });
    }
    trackBy(index, card) {
        return card.label;
    }
    onClick(data) {
        this.select.emit(data);
    }
};
__decorate([
    Input()
], CardSeriesComponent.prototype, "data", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "slots", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "dims", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "colors", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "innerPadding", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "cardColor", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "bandColor", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "emptyColor", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "textColor", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "valueFormatting", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "labelFormatting", void 0);
__decorate([
    Input()
], CardSeriesComponent.prototype, "animations", void 0);
__decorate([
    Output()
], CardSeriesComponent.prototype, "select", void 0);
CardSeriesComponent = __decorate([
    Component({
        selector: 'g[ngx-charts-card-series]',
        template: `
    <svg:rect
      *ngFor="let c of emptySlots; trackBy: trackBy"
      class="card-empty"
      [attr.x]="c.x"
      [attr.y]="c.y"
      [style.fill]="emptyColor"
      [attr.width]="c.width"
      [attr.height]="c.height"
      rx="3"
      ry="3"
    />
    <svg:g
      ngx-charts-card
      *ngFor="let c of cards; trackBy: trackBy"
      [x]="c.x"
      [y]="c.y"
      [width]="c.width"
      [height]="c.height"
      [color]="c.color"
      [bandColor]="c.bandColor"
      [textColor]="c.textColor"
      [data]="c.data"
      [label]="c.label"
      [medianSize]="medianSize"
      [valueFormatting]="valueFormatting"
      [labelFormatting]="labelFormatting"
      [animations]="animations"
      (select)="onClick($event)"
    />
  `,
        changeDetection: ChangeDetectionStrategy.OnPush
    })
], CardSeriesComponent);
export { CardSeriesComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2FyZC1zZXJpZXMuY29tcG9uZW50LmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvbnVtYmVyLWNhcmQvY2FyZC1zZXJpZXMuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQ0wsU0FBUyxFQUNULEtBQUssRUFDTCxNQUFNLEVBQ04sWUFBWSxFQUdaLHVCQUF1QixFQUN4QixNQUFNLGVBQWUsQ0FBQztBQUN2QixPQUFPLEVBQUUsV0FBVyxFQUFFLE1BQU0sc0JBQXNCLENBQUM7QUFnRG5ELElBQWEsbUJBQW1CLEdBQWhDLE1BQWEsbUJBQW1CO0lBQWhDO1FBS1csaUJBQVksR0FBRyxFQUFFLENBQUM7UUFJbEIsZUFBVSxHQUFHLGtCQUFrQixDQUFDO1FBSWhDLGVBQVUsR0FBWSxJQUFJLENBQUM7UUFFMUIsV0FBTSxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7SUEwRXhDLENBQUM7SUFwRUMsV0FBVyxDQUFDLE9BQXNCO1FBQ2hDLElBQUksQ0FBQyxNQUFNLEVBQUUsQ0FBQztJQUNoQixDQUFDO0lBRUQsTUFBTTtRQUNKLElBQUksSUFBSSxDQUFDLElBQUksQ0FBQyxNQUFNLEdBQUcsQ0FBQyxFQUFFO1lBQ3hCLE1BQU0sZUFBZSxHQUFHLElBQUksQ0FBQyxlQUFlLElBQUksQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsY0FBYyxFQUFFLENBQUMsQ0FBQztZQUV0RixNQUFNLGFBQWEsR0FBRyxJQUFJLENBQUMsSUFBSTtpQkFDNUIsR0FBRyxDQUFDLENBQUMsQ0FBQyxFQUFFO2dCQUNQLE1BQU0sUUFBUSxHQUFHLENBQUMsSUFBSSxDQUFDLENBQUMsSUFBSSxJQUFJLE9BQU8sQ0FBQyxDQUFDLElBQUksQ0FBQyxLQUFLLEtBQUssV0FBVyxJQUFJLENBQUMsQ0FBQyxJQUFJLENBQUMsS0FBSyxLQUFLLElBQUksQ0FBQztn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Ozs7Ozs7Ozs7OztHQThCVDtRQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO0tBQ2hELENBQUM7R0FDVyxtQkFBbUIsQ0F5Ri9CO1NBekZZLG1CQUFtQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIENvbXBvbmVudCxcbiAgSW5wdXQsXG4gIE91dHB1dCxcbiAgRXZlbnRFbWl0dGVyLFxuICBPbkNoYW5nZXMsXG4gIFNpbXBsZUNoYW5nZXMsXG4gIENoYW5nZURldGVjdGlvblN0cmF0ZWd5XG59IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHsgaW52ZXJ0Q29sb3IgfSBmcm9tICcuLi91dGlscy9jb2xvci11dGlscyc7XG5cbmV4cG9ydCBpbnRlcmZhY2UgQ2FyZE1vZGVsIHtcbiAgeDtcbiAgeTtcbiAgd2lkdGg6IG51bWJlcjtcbiAgaGVpZ2h0OiBudW1iZXI7XG4gIGNvbG9yOiBzdHJpbmc7XG4gIGxhYmVsOiBzdHJpbmc7XG4gIGRhdGE7XG4gIHRvb2x0aXBUZXh0OiBzdHJpbmc7XG59XG5cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ2dbbmd4LWNoYXJ0cy1jYXJkLXNlcmllc10nLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxzdmc6cmVjdFxuICAgICAgKm5nRm9yPVwibGV0IGMgb2YgZW1wdHlTbG90czsgdHJhY2tCeTogdHJhY2tCeVwiXG4gICAgICBjbGFzcz1cImNhcmQtZW1wdHlcIlxuICAgICAgW2F0dHIueF09XCJjLnhcIlxuICAgICAgW2F0dHIueV09XCJjLnlcIlxuICAgICAgW3N0eWxlLmZpbGxdPVwiZW1wdHlDb2xvclwiXG4gICAgICBbYXR0ci53aWR0aF09XCJjLndpZHRoXCJcbiAgICAgIFthdHRyLmhlaWdodF09XCJjLmhlaWdodFwiXG4gICAgICByeD1cIjNcIlxuICAgICAgcnk9XCIzXCJcbiAgICAvPlxuICAgIDxzdmc6Z1xuICAgICAgbmd4LWNoYXJ0cy1jYXJkXG4gICAgICAqbmdGb3I9XCJsZXQgYyBvZiBjYXJkczsgdHJhY2tCeTogdHJhY2tCeVwiXG4gICAgICBbeF09XCJjLnhcIlxuICAgICAgW3ldPVwiYy55XCJcbiAgICAgIFt3aWR0aF09XCJjLndpZHRoXCJcbiAgICAgIFtoZWlnaHRdPVwiYy5oZWlnaHRcIlxuICAgICAgW2NvbG9yXT1cImMuY29sb3JcIlxuICAgICAgW2JhbmRDb2xvcl09XCJjLmJhbmRDb2xvclwiXG4gICAgICBbdGV4dENvbG9yXT1cImMudGV4dENvbG9yXCJcbiAgICAgIFtkYXRhXT1cImMuZGF0YVwiXG4gICAgICBbbGFiZWxdPVwiYy5sYWJlbFwiXG4gICAgICBbbWVkaWFuU2l6ZV09XCJtZWRpYW5TaXplXCJcbiAgICAgIFt2YWx1ZUZvcm1hdHRpbmddPVwidmFsdWVGb3JtYXR0aW5nXCJcbiAgICAgIFtsYWJlbEZvcm1hdHRpbmddPVwibGFiZWxGb3JtYXR0aW5nXCJcbiAgICAgIFthbmltYXRpb25zXT1cImFuaW1hdGlvbnNcIlxuICAgICAgKHNlbGVjdCk9XCJvbkNsaWNrKCRldmVudClcIlxuICAgIC8+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIENhcmRTZXJpZXNDb21wb25lbnQgaW1wbGVtZW50cyBPbkNoYW5nZXMge1xuICBASW5wdXQoKSBkYXRhOiBhbnlbXTtcbiAgQElucHV0KCkgc2xvdHM6IGFueVtdO1xuICBASW5wdXQoKSBkaW1zO1xuICBASW5wdXQoKSBjb2xvcnM7XG4gIEBJbnB1dCgpIGlubmVyUGFkZGluZyA9IDE1O1xuXG4gIEBJbnB1dCgpIGNhcmRDb2xvcjtcbiAgQElucHV0KCkgYmFuZENvbG9yO1xuICBASW5wdXQoKSBlbXB0eUNvbG9yID0gJ3JnYmEoMCwgMCwgMCwgMCknO1xuICBASW5wdXQoKSB0ZXh0Q29sb3I7XG4gIEBJbnB1dCgpIHZhbHVlRm9ybWF0dGluZzogYW55O1xuICBASW5wdXQoKSBsYWJlbEZvcm1hdHRpbmc6IGFueTtcbiAgQElucHV0KCkgYW5pbWF0aW9uczogYm9vbGVhbiA9IHRydWU7XG5cbiAgQE91dHB1dCgpIHNlbGVjdCA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcblxuICBjYXJkczogQ2FyZE1vZGVsW107XG4gIGVtcHR5U2xvdHM6IGFueVtdO1xuICBtZWRpYW5TaXplOiBudW1iZXI7XG5cbiAgbmdPbkNoYW5nZXMoY2hhbmdlczogU2ltcGxlQ2hhbmdlcyk6IHZvaWQge1xuICAgIHRoaXMudXBkYXRlKCk7XG4gIH1cblxuICB1cGRhdGUoKTogdm9pZCB7XG4gICAgaWYgKHRoaXMuZGF0YS5sZW5ndGggPiAyKSB7XG4gICAgICBjb25zdCB2YWx1ZUZvcm1hdHRpbmcgPSB0aGlzLnZhbHVlRm9ybWF0dGluZyB8fCAoY2FyZCA9PiBjYXJkLnZhbHVlLnRvTG9jYWxlU3RyaW5nKCkpO1xuXG4gICAgICBjb25zdCBzb3J0ZWRMZW5ndGhzID0gdGhpcy5kYXRhXG4gICAgICAgIC5tYXAoZCA9PiB7XG4gICAgICAgICAgY29uc3QgaGFzVmFsdWUgPSBkICYmIGQuZGF0YSAmJiB0eXBlb2YgZC5kYXRhLnZhbHVlICE9PSAndW5kZWZpbmVkJyAmJiBkLmRhdGEudmFsdWUgIT09IG51bGw7XG4gICAgICAgICAgcmV0dXJuIGhhc1ZhbHVlXG4gICAgICAgICAgICA/IHZhbHVlRm9ybWF0dGluZyh7XG4gICAgICAgICAgICAgICAgZGF0YTogZC5kYXRhLFxuICAgICAgICAgICAgICAgIGxhYmVsOiBkID8gZC5kYXRhLm5hbWUgOiAnJyxcbiAgICAgICAgICAgICAgICB2YWx1ZTogZCAmJiBkLmRhdGEgPyBkLmRhdGEudmFsdWUgOiAnJ1xuICAgICAgICAgICAgICB9KS5sZW5ndGhcbiAgICAgICAgICAgIDogMDtcbiAgICAgICAgfSlcbiAgICAgICAgLnNvcnQoKGEsIGIpID0+IGIgLSBhKTtcbiAgICAgIGNvbnN0IGlkeCA9IE1hdGguY2VpbCh0aGlzLmRhdGEubGVuZ3RoIC8gMik7XG4gICAgICB0aGlzLm1lZGlhblNpemUgPSBzb3J0ZWRMZW5ndGhzW2lkeF07XG4gICAgfVxuXG4gICAgY29uc3QgY2FyZHMgPSB0aGlzLmdldENhcmRzKCk7XG4gICAgdGhpcy5jYXJkcyA9IGNhcmRzLmZpbHRlcihkID0+IGQuZGF0YS52YWx1ZSAhPT0gbnVsbCk7XG4gICAgdGhpcy5lbXB0eVNsb3RzID0gY2FyZHMuZmlsdGVyKGQgPT4gZC5kYXRhLnZhbHVlID09PSBudWxsKTtcbiAgfVxuXG4gIGdldENhcmRzKCk6IGFueVtdIHtcbiAgICBjb25zdCB5UGFkZGluZyA9XG4gICAgICB0eXBlb2YgdGhpcy5pbm5lclBhZGRpbmcgPT09ICdudW1iZXInID8gdGhpcy5pbm5lclBhZGRpbmcgOiB0aGlzLmlubmVyUGFkZGluZ1swXSArIHRoaXMuaW5uZXJQYWRkaW5nWzJdO1xuICAgIGNvbnN0IHhQYWRkaW5nID1cbiAgICAgIHR5cGVvZiB0aGlzLmlubmVyUGFkZGluZyA9PT0gJ251bWJlcicgPyB0aGlzLmlubmVyUGFkZGluZyA6IHRoaXMuaW5uZXJQYWRkaW5nWzFdICsgdGhpcy5pbm5lclBhZGRpbmdbM107XG5cbiAgICByZXR1cm4gdGhpcy5kYXRhLm1hcCgoZCwgaW5kZXgpID0+IHtcbiAgICAgIGxldCBsYWJlbCA9IGQuZGF0YS5uYW1lO1xuICAgICAgaWYgKGxhYmVsICYmIGxhYmVsLmNvbnN0cnVjdG9yLm5hbWUgPT09ICdEYXRlJykge1xuICAgICAgICBsYWJlbCA9IGxhYmVsLnRvTG9jYWxlRGF0ZVN0cmluZygpO1xuICAgICAgfSBlbHNlIHtcbiAgICAgICAgbGFiZWwgPSBsYWJlbCA/IGxhYmVsLnRvTG9jYWxlU3RyaW5nKCkgOiBsYWJlbDtcbiAgICAgIH1cblxuICAgICAgY29uc3QgdmFsdWUgPSBkLmRhdGEudmFsdWU7XG4gICAgICBjb25zdCB2YWx1ZUNvbG9yID0gbGFiZWwgPyB0aGlzLmNvbG9ycy5nZXRDb2xvcihsYWJlbCkgOiB0aGlzLmVtcHR5Q29sb3I7XG4gICAgICBjb25zdCBjb2xvciA9IHRoaXMuY2FyZENvbG9yIHx8IHZhbHVlQ29sb3IgfHwgJyMwMDAnO1xuICAgICAgcmV0dXJuIHtcbiAgICAgICAgeDogZC54LFxuICAgICAgICB5OiBkLnksXG4gICAgICAgIHdpZHRoOiBkLndpZHRoIC0geFBhZGRpbmcsXG4gICAgICAgIGhlaWdodDogZC5oZWlnaHQgLSB5UGFkZGluZyxcbiAgICAgICAgY29sb3IsXG4gICAgICAgIGJhbmRDb2xvcjogdGhpcy5iYW5kQ29sb3IgfHwgdmFsdWVDb2xvcixcbiAgICAgICAgdGV4dENvbG9yOiB0aGlzLnRleHRDb2xvciB8fCBpbnZlcnRDb2xvcihjb2xvciksXG4gICAgICAgIGxhYmVsLFxuICAgICAgICBkYXRhOiBkLmRhdGEsXG4gICAgICAgIHRvb2x0aXBUZXh0OiBgJHtsYWJlbH06ICR7dmFsdWV9YFxuICAgICAgfTtcbiAgICB9KTtcbiAgfVxuXG4gIHRyYWNrQnkoaW5kZXgsIGNhcmQpOiBzdHJpbmcge1xuICAgIHJldHVybiBjYXJkLmxhYmVsO1xuICB9XG5cbiAgb25DbGljayhkYXRhKTogdm9pZCB7XG4gICAgdGhpcy5zZWxlY3QuZW1pdChkYXRhKTtcbiAgfVxufVxuIl19