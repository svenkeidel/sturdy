import { __decorate, __extends } from "tslib";
import { Injectable } from '@angular/core';
import { InjectionService } from './injection.service';
import { TooltipContentComponent } from './tooltip.component';
import { InjectionRegisteryService } from './injection-registery.service';
var TooltipService = /** @class */ (function (_super) {
    __extends(TooltipService, _super);
    function TooltipService(injectionService) {
        var _this = _super.call(this, injectionService) || this;
        _this.type = TooltipContentComponent;
        return _this;
    }
    TooltipService.ctorParameters = function () { return [
        { type: InjectionService }
    ]; };
    TooltipService = __decorate([
        Injectable()
    ], TooltipService);
    return TooltipService;
}(InjectionRegisteryService));
export { TooltipService };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidG9vbHRpcC5zZXJ2aWNlLmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvY29tbW9uL3Rvb2x0aXAvdG9vbHRpcC5zZXJ2aWNlLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQUUsVUFBVSxFQUFFLE1BQU0sZUFBZSxDQUFDO0FBQzNDLE9BQU8sRUFBRSxnQkFBZ0IsRUFBRSxNQUFNLHFCQUFxQixDQUFDO0FBQ3ZELE9BQU8sRUFBRSx1QkFBdUIsRUFBRSxNQUFNLHFCQUFxQixDQUFDO0FBQzlELE9BQU8sRUFBRSx5QkFBeUIsRUFBRSxNQUFNLCtCQUErQixDQUFDO0FBRTFFO0lBQW9DLGtDQUFrRDtJQUdwRix3QkFBWSxnQkFBa0M7UUFBOUMsWUFDRSxrQkFBTSxnQkFBZ0IsQ0FBQyxTQUN4QjtRQUpELFVBQUksR0FBUSx1QkFBdUIsQ0FBQzs7SUFJcEMsQ0FBQzs7Z0JBRjZCLGdCQUFnQjs7SUFIbkMsY0FBYztRQUQxQixVQUFVLEVBQUU7T0FDQSxjQUFjLENBTTFCO0lBQUQscUJBQUM7Q0FBQSxBQU5ELENBQW9DLHlCQUF5QixHQU01RDtTQU5ZLGNBQWMiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgeyBJbmplY3RhYmxlIH0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyBJbmplY3Rpb25TZXJ2aWNlIH0gZnJvbSAnLi9pbmplY3Rpb24uc2VydmljZSc7XG5pbXBvcnQgeyBUb29sdGlwQ29udGVudENvbXBvbmVudCB9IGZyb20gJy4vdG9vbHRpcC5jb21wb25lbnQnO1xuaW1wb3J0IHsgSW5qZWN0aW9uUmVnaXN0ZXJ5U2VydmljZSB9IGZyb20gJy4vaW5qZWN0aW9uLXJlZ2lzdGVyeS5zZXJ2aWNlJztcbkBJbmplY3RhYmxlKClcbmV4cG9ydCBjbGFzcyBUb29sdGlwU2VydmljZSBleHRlbmRzIEluamVjdGlvblJlZ2lzdGVyeVNlcnZpY2U8VG9vbHRpcENvbnRlbnRDb21wb25lbnQ+IHtcbiAgdHlwZTogYW55ID0gVG9vbHRpcENvbnRlbnRDb21wb25lbnQ7XG5cbiAgY29uc3RydWN0b3IoaW5qZWN0aW9uU2VydmljZTogSW5qZWN0aW9uU2VydmljZSkge1xuICAgIHN1cGVyKGluamVjdGlvblNlcnZpY2UpO1xuICB9XG59XG4iXX0=