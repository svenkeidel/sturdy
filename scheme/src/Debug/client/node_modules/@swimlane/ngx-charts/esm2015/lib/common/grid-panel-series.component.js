import { __decorate } from "tslib";
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
let GridPanelSeriesComponent = class GridPanelSeriesComponent {
    ngOnChanges(changes) {
        this.update();
    }
    update() {
        this.gridPanels = this.getGridPanels();
    }
    getGridPanels() {
        return this.data.map(d => {
            let offset;
            let width;
            let height;
            let x;
            let y;
            let className = 'odd';
            if (this.orient === 'vertical') {
                const position = this.xScale(d.name);
                const positionIndex = Number.parseInt((position / this.xScale.step()).toString(), 10);
                if (positionIndex % 2 === 1) {
                    className = 'even';
                }
                offset = this.xScale.bandwidth() * this.xScale.paddingInner();
                width = this.xScale.bandwidth() + offset;
                height = this.dims.height;
                x = this.xScale(d.name) - offset / 2;
                y = 0;
            }
            else if (this.orient === 'horizontal') {
                const position = this.yScale(d.name);
                const positionIndex = Number.parseInt((position / this.yScale.step()).toString(), 10);
                if (positionIndex % 2 === 1) {
                    className = 'even';
                }
                offset = this.yScale.bandwidth() * this.yScale.paddingInner();
                width = this.dims.width;
                height = this.yScale.bandwidth() + offset;
                x = 0;
                y = this.yScale(d.name) - offset / 2;
            }
            return {
                name: d.name,
                class: className,
                height,
                width,
                x,
                y
            };
        });
    }
};
__decorate([
    Input()
], GridPanelSeriesComponent.prototype, "data", void 0);
__decorate([
    Input()
], GridPanelSeriesComponent.prototype, "dims", void 0);
__decorate([
    Input()
], GridPanelSeriesComponent.prototype, "xScale", void 0);
__decorate([
    Input()
], GridPanelSeriesComponent.prototype, "yScale", void 0);
__decorate([
    Input()
], GridPanelSeriesComponent.prototype, "orient", void 0);
GridPanelSeriesComponent = __decorate([
    Component({
        selector: 'g[ngx-charts-grid-panel-series]',
        template: `
    <svg:g
      ngx-charts-grid-panel
      *ngFor="let gridPanel of gridPanels"
      [height]="gridPanel.height"
      [width]="gridPanel.width"
      [x]="gridPanel.x"
      [y]="gridPanel.y"
      [class.grid-panel]="true"
      [class.odd]="gridPanel.class === 'odd'"
      [class.even]="gridPanel.class === 'even'"
    ></svg:g>
  `,
        changeDetection: ChangeDetectionStrategy.OnPush
    })
], GridPanelSeriesComponent);
export { GridPanelSeriesComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZ3JpZC1wYW5lbC1zZXJpZXMuY29tcG9uZW50LmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvY29tbW9uL2dyaWQtcGFuZWwtc2VyaWVzLmNvbXBvbmVudC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUFFLFNBQVMsRUFBaUIsS0FBSyxFQUFhLHVCQUF1QixFQUFFLE1BQU0sZUFBZSxDQUFDO0FBbUJwRyxJQUFhLHdCQUF3QixHQUFyQyxNQUFhLHdCQUF3QjtJQWtCbkMsV0FBVyxDQUFDLE9BQXNCO1FBQ2hDLElBQUksQ0FBQyxNQUFNLEVBQUUsQ0FBQztJQUNoQixDQUFDO0lBRUQsTUFBTTtRQUNKLElBQUksQ0FBQyxVQUFVLEdBQUcsSUFBSSxDQUFDLGFBQWEsRUFBRSxDQUFDO0lBQ3pDLENBQUM7SUFFRCxhQUFhO1FBQ1gsT0FBTyxJQUFJLENBQUMsSUFBSSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsRUFBRTtZQUN2QixJQUFJLE1BQU0sQ0FBQztZQUNYLElBQUksS0FBSyxDQUFDO1lBQ1YsSUFBSSxNQUFNLENBQUM7WUFDWCxJQUFJLENBQUMsQ0FBQztZQUNOLElBQUksQ0FBQyxDQUFDO1lBQ04sSUFBSSxTQUFTLEdBQUcsS0FBSyxDQUFDO1lBRXRCLElBQUksSUFBSSxDQUFDLE1BQU0sS0FBSyxVQUFVLEVBQUU7Z0JBQzlCLE1BQU0sUUFBUSxHQUFXLElBQUksQ0FBQyxNQUFNLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxDQUFDO2dCQUM3QyxNQUFNLGFBQWEsR0FBRyxNQUFNLENBQUMsUUFBUSxDQUFDLENBQUMsUUFBUSxHQUFHLElBQUksQ0FBQyxNQUFNLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxRQUFRLEVBQUUsRUFBRSxFQUFFLENBQUMsQ0FBQztnQkFFdEYsSUFBSSxhQUFhLEdBQUcsQ0FBQyxLQUFLLENBQUMsRUFBRTtvQkFDM0IsU0FBUyxHQUFHLE1BQU0sQ0FBQztpQkFDcEI7Z0JBQ0QsTUFBTSxHQUFHLElBQUksQ0FBQyxNQUFNLENBQUMsU0FBUyxFQUFFLEdBQUcsSUFBSSxDQUFDLE1BQU0sQ0FBQyxZQUFZLEVBQUUsQ0FBQztnQkFDOUQsS0FBSyxHQUFHLElBQUksQ0FBQyxNQUFNLENBQUMsU0FBUyxFQUFFLEdBQUcsTUFBTSxDQUFDO2dCQUN6QyxNQUFNLEdBQUcsSUFBSSxDQUFDLElBQUksQ0FBQyxNQUFNLENBQUM7Z0JBQzFCLENBQUMsR0FBRyxJQUFJLENBQUMsTUFBTSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsR0FBRyxNQUFNLEdBQUcsQ0FBQyxDQUFDO2dCQUNyQyxDQUFDLEdBQUcsQ0FBQyxDQUFDO2FBQ1A7aUJBQU0sSUFBSSxJQUFJLENBQUMsTUFBTSxLQUFLLFlBQVksRUFBRTtnQkFDdkMsTUFBTSxRQUFRLEdBQUcsSUFBSSxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLENBQUM7Z0JBQ3JDLE1BQU0sYUFBYSxHQUFHLE1BQU0sQ0FBQyxRQUFRLENBQUMsQ0FBQyxRQUFRLEdBQUcsSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFFBQVEsRUFBRSxFQUFFLEVBQUUsQ0FBQyxDQUFDO2dCQUV0RixJQUFJLGFBQWEsR0FBRyxDQUFDLEtBQUssQ0FBQyxFQUFFO29CQUMzQixTQUFTLEdBQUcsTUFBTSxDQUFDO2lCQUNwQjtnQkFDRCxNQUFNLEdBQUcsSUFBSSxDQUFDLE1BQU0sQ0FBQyxTQUFTLEVBQUUsR0FBRyxJQUFJLENBQUMsTUFBTSxDQUFDLFlBQVksRUFBRSxDQUFDO2dCQUU5RCxLQUFLLEdBQUcsSUFBSSxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUM7Z0JBQ3hCLE1BQU0sR0FBRyxJQUFJLENBQUMsTUFBTSxDQUFDLFNBQVMsRUFBRSxHQUFHLE1BQU0sQ0FBQztnQkFDMUMsQ0FBQyxHQUFHLENBQUMsQ0FBQztnQkFDTixDQUFDLEdBQUcsSUFBSSxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLEdBQUcsTUFBTSxHQUFHLENBQUMsQ0FBQzthQUN0QztZQUVELE9BQU87Z0JBQ0wsSUFBSSxFQUFFLENBQUMsQ0FBQyxJQUFJO2dCQUNaLEtBQUssRUFBRSxTQUFTO2dCQUNoQixNQUFNO2dCQUNOLEtBQUs7Z0JBQ0wsQ0FBQztnQkFDRCxDQUFDO2FBQ0YsQ0FBQztRQUNKLENBQUMsQ0FBQyxDQUFDO0lBQ0wsQ0FBQztDQUNGLENBQUE7QUFwRUM7SUFEQyxLQUFLLEVBQUU7c0RBQ0g7QUFHTDtJQURDLEtBQUssRUFBRTtzREFDSDtBQUdMO0lBREMsS0FBSyxFQUFFO3dEQUNEO0FBR1A7SUFEQyxLQUFLLEVBQUU7d0RBQ0Q7QUFHUDtJQURDLEtBQUssRUFBRTt3REFDRDtBQWhCSSx3QkFBd0I7SUFqQnBDLFNBQVMsQ0FBQztRQUNULFFBQVEsRUFBRSxpQ0FBaUM7UUFDM0MsUUFBUSxFQUFFOzs7Ozs7Ozs7Ozs7R0FZVDtRQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO0tBQ2hELENBQUM7R0FDVyx3QkFBd0IsQ0F3RXBDO1NBeEVZLHdCQUF3QiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IENvbXBvbmVudCwgU2ltcGxlQ2hhbmdlcywgSW5wdXQsIE9uQ2hhbmdlcywgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kgfSBmcm9tICdAYW5ndWxhci9jb3JlJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnZ1tuZ3gtY2hhcnRzLWdyaWQtcGFuZWwtc2VyaWVzXScsXG4gIHRlbXBsYXRlOiBgXG4gICAgPHN2ZzpnXG4gICAgICBuZ3gtY2hhcnRzLWdyaWQtcGFuZWxcbiAgICAgICpuZ0Zvcj1cImxldCBncmlkUGFuZWwgb2YgZ3JpZFBhbmVsc1wiXG4gICAgICBbaGVpZ2h0XT1cImdyaWRQYW5lbC5oZWlnaHRcIlxuICAgICAgW3dpZHRoXT1cImdyaWRQYW5lbC53aWR0aFwiXG4gICAgICBbeF09XCJncmlkUGFuZWwueFwiXG4gICAgICBbeV09XCJncmlkUGFuZWwueVwiXG4gICAgICBbY2xhc3MuZ3JpZC1wYW5lbF09XCJ0cnVlXCJcbiAgICAgIFtjbGFzcy5vZGRdPVwiZ3JpZFBhbmVsLmNsYXNzID09PSAnb2RkJ1wiXG4gICAgICBbY2xhc3MuZXZlbl09XCJncmlkUGFuZWwuY2xhc3MgPT09ICdldmVuJ1wiXG4gICAgPjwvc3ZnOmc+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIEdyaWRQYW5lbFNlcmllc0NvbXBvbmVudCBpbXBsZW1lbnRzIE9uQ2hhbmdlcyB7XG4gIGdyaWRQYW5lbHM6IGFueVtdO1xuXG4gIEBJbnB1dCgpXG4gIGRhdGE7XG5cbiAgQElucHV0KClcbiAgZGltcztcblxuICBASW5wdXQoKVxuICB4U2NhbGU7XG5cbiAgQElucHV0KClcbiAgeVNjYWxlO1xuXG4gIEBJbnB1dCgpXG4gIG9yaWVudDtcblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKTogdm9pZCB7XG4gICAgdGhpcy51cGRhdGUoKTtcbiAgfVxuXG4gIHVwZGF0ZSgpOiB2b2lkIHtcbiAgICB0aGlzLmdyaWRQYW5lbHMgPSB0aGlzLmdldEdyaWRQYW5lbHMoKTtcbiAgfVxuXG4gIGdldEdyaWRQYW5lbHMoKTogYW55W10ge1xuICAgIHJldHVybiB0aGlzLmRhdGEubWFwKGQgPT4ge1xuICAgICAgbGV0IG9mZnNldDtcbiAgICAgIGxldCB3aWR0aDtcbiAgICAgIGxldCBoZWlnaHQ7XG4gICAgICBsZXQgeDtcbiAgICAgIGxldCB5O1xuICAgICAgbGV0IGNsYXNzTmFtZSA9ICdvZGQnO1xuXG4gICAgICBpZiAodGhpcy5vcmllbnQgPT09ICd2ZXJ0aWNhbCcpIHtcbiAgICAgICAgY29uc3QgcG9zaXRpb246IG51bWJlciA9IHRoaXMueFNjYWxlKGQubmFtZSk7XG4gICAgICAgIGNvbnN0IHBvc2l0aW9uSW5kZXggPSBOdW1iZXIucGFyc2VJbnQoKHBvc2l0aW9uIC8gdGhpcy54U2NhbGUuc3RlcCgpKS50b1N0cmluZygpLCAxMCk7XG5cbiAgICAgICAgaWYgKHBvc2l0aW9uSW5kZXggJSAyID09PSAxKSB7XG4gICAgICAgICAgY2xhc3NOYW1lID0gJ2V2ZW4nO1xuICAgICAgICB9XG4gICAgICAgIG9mZnNldCA9IHRoaXMueFNjYWxlLmJhbmR3aWR0aCgpICogdGhpcy54U2NhbGUucGFkZGluZ0lubmVyKCk7XG4gICAgICAgIHdpZHRoID0gdGhpcy54U2NhbGUuYmFuZHdpZHRoKCkgKyBvZmZzZXQ7XG4gICAgICAgIGhlaWdodCA9IHRoaXMuZGltcy5oZWlnaHQ7XG4gICAgICAgIHggPSB0aGlzLnhTY2FsZShkLm5hbWUpIC0gb2Zmc2V0IC8gMjtcbiAgICAgICAgeSA9IDA7XG4gICAgICB9IGVsc2UgaWYgKHRoaXMub3JpZW50ID09PSAnaG9yaXpvbnRhbCcpIHtcbiAgICAgICAgY29uc3QgcG9zaXRpb24gPSB0aGlzLnlTY2FsZShkLm5hbWUpO1xuICAgICAgICBjb25zdCBwb3NpdGlvbkluZGV4ID0gTnVtYmVyLnBhcnNlSW50KChwb3NpdGlvbiAvIHRoaXMueVNjYWxlLnN0ZXAoKSkudG9TdHJpbmcoKSwgMTApO1xuXG4gICAgICAgIGlmIChwb3NpdGlvbkluZGV4ICUgMiA9PT0gMSkge1xuICAgICAgICAgIGNsYXNzTmFtZSA9ICdldmVuJztcbiAgICAgICAgfVxuICAgICAgICBvZmZzZXQgPSB0aGlzLnlTY2FsZS5iYW5kd2lkdGgoKSAqIHRoaXMueVNjYWxlLnBhZGRpbmdJbm5lcigpO1xuXG4gICAgICAgIHdpZHRoID0gdGhpcy5kaW1zLndpZHRoO1xuICAgICAgICBoZWlnaHQgPSB0aGlzLnlTY2FsZS5iYW5kd2lkdGgoKSArIG9mZnNldDtcbiAgICAgICAgeCA9IDA7XG4gICAgICAgIHkgPSB0aGlzLnlTY2FsZShkLm5hbWUpIC0gb2Zmc2V0IC8gMjtcbiAgICAgIH1cblxuICAgICAgcmV0dXJuIHtcbiAgICAgICAgbmFtZTogZC5uYW1lLFxuICAgICAgICBjbGFzczogY2xhc3NOYW1lLFxuICAgICAgICBoZWlnaHQsXG4gICAgICAgIHdpZHRoLFxuICAgICAgICB4LFxuICAgICAgICB5XG4gICAgICB9O1xuICAgIH0pO1xuICB9XG59XG4iXX0=