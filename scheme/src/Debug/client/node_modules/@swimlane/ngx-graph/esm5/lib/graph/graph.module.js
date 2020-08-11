import { __decorate } from "tslib";
import { NgModule } from '@angular/core';
import { GraphComponent } from './graph.component';
import { ChartCommonModule } from '@swimlane/ngx-charts';
import { MouseWheelDirective } from './mouse-wheel.directive';
import { LayoutService } from './layouts/layout.service';
export { GraphComponent };
var GraphModule = /** @class */ (function () {
    function GraphModule() {
    }
    GraphModule = __decorate([
        NgModule({
            imports: [ChartCommonModule],
            declarations: [GraphComponent, MouseWheelDirective],
            exports: [GraphComponent, MouseWheelDirective],
            providers: [LayoutService]
        })
    ], GraphModule);
    return GraphModule;
}());
export { GraphModule };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZ3JhcGgubW9kdWxlLmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1ncmFwaC8iLCJzb3VyY2VzIjpbImxpYi9ncmFwaC9ncmFwaC5tb2R1bGUudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFBRSxRQUFRLEVBQUUsTUFBTSxlQUFlLENBQUM7QUFDekMsT0FBTyxFQUFFLGNBQWMsRUFBRSxNQUFNLG1CQUFtQixDQUFDO0FBQ25ELE9BQU8sRUFBRSxpQkFBaUIsRUFBRSxNQUFNLHNCQUFzQixDQUFDO0FBQ3pELE9BQU8sRUFBRSxtQkFBbUIsRUFBRSxNQUFNLHlCQUF5QixDQUFDO0FBQzlELE9BQU8sRUFBRSxhQUFhLEVBQUUsTUFBTSwwQkFBMEIsQ0FBQztBQUN6RCxPQUFPLEVBQUUsY0FBYyxFQUFFLENBQUM7QUFRMUI7SUFBQTtJQUEwQixDQUFDO0lBQWQsV0FBVztRQU52QixRQUFRLENBQUM7WUFDUixPQUFPLEVBQUUsQ0FBQyxpQkFBaUIsQ0FBQztZQUM1QixZQUFZLEVBQUUsQ0FBQyxjQUFjLEVBQUUsbUJBQW1CLENBQUM7WUFDbkQsT0FBTyxFQUFFLENBQUMsY0FBYyxFQUFFLG1CQUFtQixDQUFDO1lBQzlDLFNBQVMsRUFBRSxDQUFDLGFBQWEsQ0FBQztTQUMzQixDQUFDO09BQ1csV0FBVyxDQUFHO0lBQUQsa0JBQUM7Q0FBQSxBQUEzQixJQUEyQjtTQUFkLFdBQVciLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgeyBOZ01vZHVsZSB9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHsgR3JhcGhDb21wb25lbnQgfSBmcm9tICcuL2dyYXBoLmNvbXBvbmVudCc7XG5pbXBvcnQgeyBDaGFydENvbW1vbk1vZHVsZSB9IGZyb20gJ0Bzd2ltbGFuZS9uZ3gtY2hhcnRzJztcbmltcG9ydCB7IE1vdXNlV2hlZWxEaXJlY3RpdmUgfSBmcm9tICcuL21vdXNlLXdoZWVsLmRpcmVjdGl2ZSc7XG5pbXBvcnQgeyBMYXlvdXRTZXJ2aWNlIH0gZnJvbSAnLi9sYXlvdXRzL2xheW91dC5zZXJ2aWNlJztcbmV4cG9ydCB7IEdyYXBoQ29tcG9uZW50IH07XG5cbkBOZ01vZHVsZSh7XG4gIGltcG9ydHM6IFtDaGFydENvbW1vbk1vZHVsZV0sXG4gIGRlY2xhcmF0aW9uczogW0dyYXBoQ29tcG9uZW50LCBNb3VzZVdoZWVsRGlyZWN0aXZlXSxcbiAgZXhwb3J0czogW0dyYXBoQ29tcG9uZW50LCBNb3VzZVdoZWVsRGlyZWN0aXZlXSxcbiAgcHJvdmlkZXJzOiBbTGF5b3V0U2VydmljZV1cbn0pXG5leHBvcnQgY2xhc3MgR3JhcGhNb2R1bGUge31cbiJdfQ==