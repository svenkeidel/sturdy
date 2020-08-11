import { __decorate } from "tslib";
import { Component, Input, Output, ChangeDetectionStrategy, HostListener, EventEmitter } from '@angular/core';
var LegendEntryComponent = /** @class */ (function () {
    function LegendEntryComponent() {
        this.isActive = false;
        this.select = new EventEmitter();
        this.activate = new EventEmitter();
        this.deactivate = new EventEmitter();
        this.toggle = new EventEmitter();
    }
    Object.defineProperty(LegendEntryComponent.prototype, "trimmedLabel", {
        get: function () {
            return this.formattedLabel || '(empty)';
        },
        enumerable: true,
        configurable: true
    });
    LegendEntryComponent.prototype.onMouseEnter = function () {
        this.activate.emit({ name: this.label });
    };
    LegendEntryComponent.prototype.onMouseLeave = function () {
        this.deactivate.emit({ name: this.label });
    };
    __decorate([
        Input()
    ], LegendEntryComponent.prototype, "color", void 0);
    __decorate([
        Input()
    ], LegendEntryComponent.prototype, "label", void 0);
    __decorate([
        Input()
    ], LegendEntryComponent.prototype, "formattedLabel", void 0);
    __decorate([
        Input()
    ], LegendEntryComponent.prototype, "isActive", void 0);
    __decorate([
        Output()
    ], LegendEntryComponent.prototype, "select", void 0);
    __decorate([
        Output()
    ], LegendEntryComponent.prototype, "activate", void 0);
    __decorate([
        Output()
    ], LegendEntryComponent.prototype, "deactivate", void 0);
    __decorate([
        Output()
    ], LegendEntryComponent.prototype, "toggle", void 0);
    __decorate([
        HostListener('mouseenter')
    ], LegendEntryComponent.prototype, "onMouseEnter", null);
    __decorate([
        HostListener('mouseleave')
    ], LegendEntryComponent.prototype, "onMouseLeave", null);
    LegendEntryComponent = __decorate([
        Component({
            selector: 'ngx-charts-legend-entry',
            template: "\n    <span [title]=\"formattedLabel\" tabindex=\"-1\" [class.active]=\"isActive\" (click)=\"select.emit(formattedLabel)\">\n      <span class=\"legend-label-color\" [style.background-color]=\"color\" (click)=\"toggle.emit(formattedLabel)\"> </span>\n      <span class=\"legend-label-text\">\n        {{ trimmedLabel }}\n      </span>\n    </span>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], LegendEntryComponent);
    return LegendEntryComponent;
}());
export { LegendEntryComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibGVnZW5kLWVudHJ5LmNvbXBvbmVudC5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi9sZWdlbmQvbGVnZW5kLWVudHJ5LmNvbXBvbmVudC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUFFLFNBQVMsRUFBRSxLQUFLLEVBQUUsTUFBTSxFQUFFLHVCQUF1QixFQUFFLFlBQVksRUFBRSxZQUFZLEVBQUUsTUFBTSxlQUFlLENBQUM7QUFjOUc7SUFBQTtRQUlXLGFBQVEsR0FBWSxLQUFLLENBQUM7UUFFekIsV0FBTSxHQUFzQixJQUFJLFlBQVksRUFBRSxDQUFDO1FBQy9DLGFBQVEsR0FBc0IsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUNqRCxlQUFVLEdBQXNCLElBQUksWUFBWSxFQUFFLENBQUM7UUFDbkQsV0FBTSxHQUFzQixJQUFJLFlBQVksRUFBRSxDQUFDO0lBZTNELENBQUM7SUFiQyxzQkFBSSw4Q0FBWTthQUFoQjtZQUNFLE9BQU8sSUFBSSxDQUFDLGNBQWMsSUFBSSxTQUFTLENBQUM7UUFDMUMsQ0FBQzs7O09BQUE7SUFHRCwyQ0FBWSxHQUFaO1FBQ0UsSUFBSSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsRUFBRSxJQUFJLEVBQUUsSUFBSSxDQUFDLEtBQUssRUFBRSxDQUFDLENBQUM7SUFDM0MsQ0FBQztJQUdELDJDQUFZLEdBQVo7UUFDRSxJQUFJLENBQUMsVUFBVSxDQUFDLElBQUksQ0FBQyxFQUFFLElBQUksRUFBRSxJQUFJLENBQUMsS0FBSyxFQUFFLENBQUMsQ0FBQztJQUM3QyxDQUFDO0lBdEJRO1FBQVIsS0FBSyxFQUFFO3VEQUFlO0lBQ2Q7UUFBUixLQUFLLEVBQUU7dURBQVk7SUFDWDtRQUFSLEtBQUssRUFBRTtnRUFBd0I7SUFDdkI7UUFBUixLQUFLLEVBQUU7MERBQTJCO0lBRXpCO1FBQVQsTUFBTSxFQUFFO3dEQUFnRDtJQUMvQztRQUFULE1BQU0sRUFBRTswREFBa0Q7SUFDakQ7UUFBVCxNQUFNLEVBQUU7NERBQW9EO0lBQ25EO1FBQVQsTUFBTSxFQUFFO3dEQUFnRDtJQU96RDtRQURDLFlBQVksQ0FBQyxZQUFZLENBQUM7NERBRzFCO0lBR0Q7UUFEQyxZQUFZLENBQUMsWUFBWSxDQUFDOzREQUcxQjtJQXZCVSxvQkFBb0I7UUFaaEMsU0FBUyxDQUFDO1lBQ1QsUUFBUSxFQUFFLHlCQUF5QjtZQUNuQyxRQUFRLEVBQUUsaVdBT1Q7WUFDRCxlQUFlLEVBQUUsdUJBQXVCLENBQUMsTUFBTTtTQUNoRCxDQUFDO09BQ1csb0JBQW9CLENBd0JoQztJQUFELDJCQUFDO0NBQUEsQUF4QkQsSUF3QkM7U0F4Qlksb0JBQW9CIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgQ29tcG9uZW50LCBJbnB1dCwgT3V0cHV0LCBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneSwgSG9zdExpc3RlbmVyLCBFdmVudEVtaXR0ZXIgfSBmcm9tICdAYW5ndWxhci9jb3JlJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnbmd4LWNoYXJ0cy1sZWdlbmQtZW50cnknLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxzcGFuIFt0aXRsZV09XCJmb3JtYXR0ZWRMYWJlbFwiIHRhYmluZGV4PVwiLTFcIiBbY2xhc3MuYWN0aXZlXT1cImlzQWN0aXZlXCIgKGNsaWNrKT1cInNlbGVjdC5lbWl0KGZvcm1hdHRlZExhYmVsKVwiPlxuICAgICAgPHNwYW4gY2xhc3M9XCJsZWdlbmQtbGFiZWwtY29sb3JcIiBbc3R5bGUuYmFja2dyb3VuZC1jb2xvcl09XCJjb2xvclwiIChjbGljayk9XCJ0b2dnbGUuZW1pdChmb3JtYXR0ZWRMYWJlbClcIj4gPC9zcGFuPlxuICAgICAgPHNwYW4gY2xhc3M9XCJsZWdlbmQtbGFiZWwtdGV4dFwiPlxuICAgICAgICB7eyB0cmltbWVkTGFiZWwgfX1cbiAgICAgIDwvc3Bhbj5cbiAgICA8L3NwYW4+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIExlZ2VuZEVudHJ5Q29tcG9uZW50IHtcbiAgQElucHV0KCkgY29sb3I6IHN0cmluZztcbiAgQElucHV0KCkgbGFiZWw6IGFueTtcbiAgQElucHV0KCkgZm9ybWF0dGVkTGFiZWw6IHN0cmluZztcbiAgQElucHV0KCkgaXNBY3RpdmU6IGJvb2xlYW4gPSBmYWxzZTtcblxuICBAT3V0cHV0KCkgc2VsZWN0OiBFdmVudEVtaXR0ZXI8YW55PiA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcbiAgQE91dHB1dCgpIGFjdGl2YXRlOiBFdmVudEVtaXR0ZXI8YW55PiA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcbiAgQE91dHB1dCgpIGRlYWN0aXZhdGU6IEV2ZW50RW1pdHRlcjxhbnk+ID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuICBAT3V0cHV0KCkgdG9nZ2xlOiBFdmVudEVtaXR0ZXI8YW55PiA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcblxuICBnZXQgdHJpbW1lZExhYmVsKCk6IHN0cmluZyB7XG4gICAgcmV0dXJuIHRoaXMuZm9ybWF0dGVkTGFiZWwgfHwgJyhlbXB0eSknO1xuICB9XG5cbiAgQEhvc3RMaXN0ZW5lcignbW91c2VlbnRlcicpXG4gIG9uTW91c2VFbnRlcigpOiB2b2lkIHtcbiAgICB0aGlzLmFjdGl2YXRlLmVtaXQoeyBuYW1lOiB0aGlzLmxhYmVsIH0pO1xuICB9XG5cbiAgQEhvc3RMaXN0ZW5lcignbW91c2VsZWF2ZScpXG4gIG9uTW91c2VMZWF2ZSgpOiB2b2lkIHtcbiAgICB0aGlzLmRlYWN0aXZhdGUuZW1pdCh7IG5hbWU6IHRoaXMubGFiZWwgfSk7XG4gIH1cbn1cbiJdfQ==