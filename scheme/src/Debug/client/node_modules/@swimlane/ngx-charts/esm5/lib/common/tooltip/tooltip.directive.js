import { __decorate } from "tslib";
import { Directive, Input, Output, EventEmitter, HostListener, ViewContainerRef, Renderer2, OnDestroy } from '@angular/core';
import { PlacementTypes } from './position';
import { StyleTypes } from './style.type';
import { AlignmentTypes } from './alignment.type';
import { ShowTypes } from './show.type';
import { TooltipService } from './tooltip.service';
var TooltipDirective = /** @class */ (function () {
    function TooltipDirective(tooltipService, viewContainerRef, renderer) {
        this.tooltipService = tooltipService;
        this.viewContainerRef = viewContainerRef;
        this.renderer = renderer;
        this.tooltipCssClass = '';
        this.tooltipTitle = '';
        this.tooltipAppendToBody = true;
        this.tooltipSpacing = 10;
        this.tooltipDisabled = false;
        this.tooltipShowCaret = true;
        this.tooltipPlacement = PlacementTypes.top;
        this.tooltipAlignment = AlignmentTypes.center;
        this.tooltipType = StyleTypes.popover;
        this.tooltipCloseOnClickOutside = true;
        this.tooltipCloseOnMouseLeave = true;
        this.tooltipHideTimeout = 300;
        this.tooltipShowTimeout = 100;
        this.tooltipShowEvent = ShowTypes.all;
        this.tooltipImmediateExit = false;
        this.show = new EventEmitter();
        this.hide = new EventEmitter();
    }
    Object.defineProperty(TooltipDirective.prototype, "listensForFocus", {
        get: function () {
            return this.tooltipShowEvent === ShowTypes.all || this.tooltipShowEvent === ShowTypes.focus;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TooltipDirective.prototype, "listensForHover", {
        get: function () {
            return this.tooltipShowEvent === ShowTypes.all || this.tooltipShowEvent === ShowTypes.mouseover;
        },
        enumerable: true,
        configurable: true
    });
    TooltipDirective.prototype.ngOnDestroy = function () {
        this.hideTooltip(true);
    };
    TooltipDirective.prototype.onFocus = function () {
        if (this.listensForFocus) {
            this.showTooltip();
        }
    };
    TooltipDirective.prototype.onBlur = function () {
        if (this.listensForFocus) {
            this.hideTooltip(true);
        }
    };
    TooltipDirective.prototype.onMouseEnter = function () {
        if (this.listensForHover) {
            this.showTooltip();
        }
    };
    TooltipDirective.prototype.onMouseLeave = function (target) {
        if (this.listensForHover && this.tooltipCloseOnMouseLeave) {
            clearTimeout(this.timeout);
            if (this.component) {
                var contentDom = this.component.instance.element.nativeElement;
                var contains = contentDom.contains(target);
                if (contains)
                    return;
            }
            this.hideTooltip(this.tooltipImmediateExit);
        }
    };
    TooltipDirective.prototype.onMouseClick = function () {
        if (this.listensForHover) {
            this.hideTooltip(true);
        }
    };
    TooltipDirective.prototype.showTooltip = function (immediate) {
        var _this = this;
        if (this.component || this.tooltipDisabled)
            return;
        var time = immediate ? 0 : this.tooltipShowTimeout;
        clearTimeout(this.timeout);
        this.timeout = setTimeout(function () {
            _this.tooltipService.destroyAll();
            var options = _this.createBoundOptions();
            _this.component = _this.tooltipService.create(options);
            // add a tiny timeout to avoid event re-triggers
            setTimeout(function () {
                if (_this.component) {
                    _this.addHideListeners(_this.component.instance.element.nativeElement);
                }
            }, 10);
            _this.show.emit(true);
        }, time);
    };
    TooltipDirective.prototype.addHideListeners = function (tooltip) {
        var _this = this;
        // on mouse enter, cancel the hide triggered by the leave
        this.mouseEnterContentEvent = this.renderer.listen(tooltip, 'mouseenter', function () {
            clearTimeout(_this.timeout);
        });
        // content mouse leave listener
        if (this.tooltipCloseOnMouseLeave) {
            this.mouseLeaveContentEvent = this.renderer.listen(tooltip, 'mouseleave', function () {
                _this.hideTooltip(_this.tooltipImmediateExit);
            });
        }
        // content close on click outside
        if (this.tooltipCloseOnClickOutside) {
            this.documentClickEvent = this.renderer.listen(document, 'click', function (event) {
                var contains = tooltip.contains(event.target);
                if (!contains)
                    _this.hideTooltip();
            });
        }
    };
    TooltipDirective.prototype.hideTooltip = function (immediate) {
        var _this = this;
        if (immediate === void 0) { immediate = false; }
        if (!this.component)
            return;
        var destroyFn = function () {
            // remove events
            if (_this.mouseLeaveContentEvent)
                _this.mouseLeaveContentEvent();
            if (_this.mouseEnterContentEvent)
                _this.mouseEnterContentEvent();
            if (_this.documentClickEvent)
                _this.documentClickEvent();
            // emit events
            _this.hide.emit(true);
            // destroy component
            _this.tooltipService.destroy(_this.component);
            _this.component = undefined;
        };
        clearTimeout(this.timeout);
        if (!immediate) {
            this.timeout = setTimeout(destroyFn, this.tooltipHideTimeout);
        }
        else {
            destroyFn();
        }
    };
    TooltipDirective.prototype.createBoundOptions = function () {
        return {
            title: this.tooltipTitle,
            template: this.tooltipTemplate,
            host: this.viewContainerRef.element,
            placement: this.tooltipPlacement,
            alignment: this.tooltipAlignment,
            type: this.tooltipType,
            showCaret: this.tooltipShowCaret,
            cssClass: this.tooltipCssClass,
            spacing: this.tooltipSpacing,
            context: this.tooltipContext
        };
    };
    TooltipDirective.ctorParameters = function () { return [
        { type: TooltipService },
        { type: ViewContainerRef },
        { type: Renderer2 }
    ]; };
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipCssClass", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipTitle", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipAppendToBody", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipSpacing", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipDisabled", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipShowCaret", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipPlacement", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipAlignment", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipType", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipCloseOnClickOutside", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipCloseOnMouseLeave", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipHideTimeout", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipShowTimeout", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipTemplate", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipShowEvent", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipContext", void 0);
    __decorate([
        Input()
    ], TooltipDirective.prototype, "tooltipImmediateExit", void 0);
    __decorate([
        Output()
    ], TooltipDirective.prototype, "show", void 0);
    __decorate([
        Output()
    ], TooltipDirective.prototype, "hide", void 0);
    __decorate([
        HostListener('focusin')
    ], TooltipDirective.prototype, "onFocus", null);
    __decorate([
        HostListener('blur')
    ], TooltipDirective.prototype, "onBlur", null);
    __decorate([
        HostListener('mouseenter')
    ], TooltipDirective.prototype, "onMouseEnter", null);
    __decorate([
        HostListener('mouseleave', ['$event.target'])
    ], TooltipDirective.prototype, "onMouseLeave", null);
    __decorate([
        HostListener('click')
    ], TooltipDirective.prototype, "onMouseClick", null);
    TooltipDirective = __decorate([
        Directive({ selector: '[ngx-tooltip]' })
    ], TooltipDirective);
    return TooltipDirective;
}());
export { TooltipDirective };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidG9vbHRpcC5kaXJlY3RpdmUuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vdG9vbHRpcC90b29sdGlwLmRpcmVjdGl2ZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUNMLFNBQVMsRUFDVCxLQUFLLEVBQ0wsTUFBTSxFQUNOLFlBQVksRUFDWixZQUFZLEVBQ1osZ0JBQWdCLEVBQ2hCLFNBQVMsRUFDVCxTQUFTLEVBQ1YsTUFBTSxlQUFlLENBQUM7QUFFdkIsT0FBTyxFQUFFLGNBQWMsRUFBRSxNQUFNLFlBQVksQ0FBQztBQUM1QyxPQUFPLEVBQUUsVUFBVSxFQUFFLE1BQU0sY0FBYyxDQUFDO0FBQzFDLE9BQU8sRUFBRSxjQUFjLEVBQUUsTUFBTSxrQkFBa0IsQ0FBQztBQUNsRCxPQUFPLEVBQUUsU0FBUyxFQUFFLE1BQU0sYUFBYSxDQUFDO0FBRXhDLE9BQU8sRUFBRSxjQUFjLEVBQUUsTUFBTSxtQkFBbUIsQ0FBQztBQUduRDtJQW9DRSwwQkFDVSxjQUE4QixFQUM5QixnQkFBa0MsRUFDbEMsUUFBbUI7UUFGbkIsbUJBQWMsR0FBZCxjQUFjLENBQWdCO1FBQzlCLHFCQUFnQixHQUFoQixnQkFBZ0IsQ0FBa0I7UUFDbEMsYUFBUSxHQUFSLFFBQVEsQ0FBVztRQXRDcEIsb0JBQWUsR0FBVyxFQUFFLENBQUM7UUFDN0IsaUJBQVksR0FBVyxFQUFFLENBQUM7UUFDMUIsd0JBQW1CLEdBQVksSUFBSSxDQUFDO1FBQ3BDLG1CQUFjLEdBQVcsRUFBRSxDQUFDO1FBQzVCLG9CQUFlLEdBQVksS0FBSyxDQUFDO1FBQ2pDLHFCQUFnQixHQUFZLElBQUksQ0FBQztRQUNqQyxxQkFBZ0IsR0FBbUIsY0FBYyxDQUFDLEdBQUcsQ0FBQztRQUN0RCxxQkFBZ0IsR0FBbUIsY0FBYyxDQUFDLE1BQU0sQ0FBQztRQUN6RCxnQkFBVyxHQUFlLFVBQVUsQ0FBQyxPQUFPLENBQUM7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SzFCO0lBR0Q7UUFEQyxZQUFZLENBQUMsWUFBWSxFQUFFLENBQUMsZUFBZSxDQUFDLENBQUM7d0RBYTdDO0lBR0Q7UUFEQyxZQUFZLENBQUMsT0FBTyxDQUFDO3dEQUtyQjtJQXZGVSxnQkFBZ0I7UUFENUIsU0FBUyxDQUFDLEVBQUUsUUFBUSxFQUFFLGVBQWUsRUFBRSxDQUFDO09BQzVCLGdCQUFnQixDQTZLNUI7SUFBRCx1QkFBQztDQUFBLEFBN0tELElBNktDO1NBN0tZLGdCQUFnQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIERpcmVjdGl2ZSxcbiAgSW5wdXQsXG4gIE91dHB1dCxcbiAgRXZlbnRFbWl0dGVyLFxuICBIb3N0TGlzdGVuZXIsXG4gIFZpZXdDb250YWluZXJSZWYsXG4gIFJlbmRlcmVyMixcbiAgT25EZXN0cm95XG59IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuXG5pbXBvcnQgeyBQbGFjZW1lbnRUeXBlcyB9IGZyb20gJy4vcG9zaXRpb24nO1xuaW1wb3J0IHsgU3R5bGVUeXBlcyB9IGZyb20gJy4vc3R5bGUudHlwZSc7XG5pbXBvcnQgeyBBbGlnbm1lbnRUeXBlcyB9IGZyb20gJy4vYWxpZ25tZW50LnR5cGUnO1xuaW1wb3J0IHsgU2hvd1R5cGVzIH0gZnJvbSAnLi9zaG93LnR5cGUnO1xuXG5pbXBvcnQgeyBUb29sdGlwU2VydmljZSB9IGZyb20gJy4vdG9vbHRpcC5zZXJ2aWNlJztcblxuQERpcmVjdGl2ZSh7IHNlbGVjdG9yOiAnW25neC10b29sdGlwXScgfSlcbmV4cG9ydCBjbGFzcyBUb29sdGlwRGlyZWN0aXZlIGltcGxlbWVudHMgT25EZXN0cm95IHtcbiAgQElucHV0KCkgdG9vbHRpcENzc0NsYXNzOiBzdHJpbmcgPSAnJztcbiAgQElucHV0KCkgdG9vbHRpcFRpdGxlOiBzdHJpbmcgPSAnJztcbiAgQElucHV0KCkgdG9vbHRpcEFwcGVuZFRvQm9keTogYm9vbGVhbiA9IHRydWU7XG4gIEBJbnB1dCgpIHRvb2x0aXBTcGFjaW5nOiBudW1iZXIgPSAxMDtcbiAgQElucHV0KCkgdG9vbHRpcERpc2FibGVkOiBib29sZWFuID0gZmFsc2U7XG4gIEBJbnB1dCgpIHRvb2x0aXBTaG93Q2FyZXQ6IGJvb2xlYW4gPSB0cnVlO1xuICBASW5wdXQoKSB0b29sdGlwUGxhY2VtZW50OiBQbGFjZW1lbnRUeXBlcyA9IFBsYWNlbWVudFR5cGVzLnRvcDtcbiAgQElucHV0KCkgdG9vbHRpcEFsaWdubWVudDogQWxpZ25tZW50VHlwZXMgPSBBbGlnbm1lbnRUeXBlcy5jZW50ZXI7XG4gIEBJbnB1dCgpIHRvb2x0aXBUeXBlOiBTdHlsZVR5cGVzID0gU3R5bGVUeXBlcy5wb3BvdmVyO1xuICBASW5wdXQoKSB0b29sdGlwQ2xvc2VPbkNsaWNrT3V0c2lkZTogYm9vbGVhbiA9IHRydWU7XG4gIEBJbnB1dCgpIHRvb2x0aXBDbG9zZU9uTW91c2VMZWF2ZTogYm9vbGVhbiA9IHRydWU7XG4gIEBJbnB1dCgpIHRvb2x0aXBIaWRlVGltZW91dDogbnVtYmVyID0gMzAwO1xuICBASW5wdXQoKSB0b29sdGlwU2hvd1RpbWVvdXQ6IG51bWJlciA9IDEwMDtcbiAgQElucHV0KCkgdG9vbHRpcFRlbXBsYXRlOiBhbnk7XG4gIEBJbnB1dCgpIHRvb2x0aXBTaG93RXZlbnQ6IFNob3dUeXBlcyA9IFNob3dUeXBlcy5hbGw7XG4gIEBJbnB1dCgpIHRvb2x0aXBDb250ZXh0OiBhbnk7XG4gIEBJbnB1dCgpIHRvb2x0aXBJbW1lZGlhdGVFeGl0OiBib29sZWFuID0gZmFsc2U7XG5cbiAgQE91dHB1dCgpIHNob3cgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG4gIEBPdXRwdXQoKSBoaWRlID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuXG4gIHByaXZhdGUgZ2V0IGxpc3RlbnNGb3JGb2N1cygpOiBib29sZWFuIHtcbiAgICByZXR1cm4gdGhpcy50b29sdGlwU2hvd0V2ZW50ID09PSBTaG93VHlwZXMuYWxsIHx8IHRoaXMudG9vbHRpcFNob3dFdmVudCA9PT0gU2hvd1R5cGVzLmZvY3VzO1xuICB9XG5cbiAgcHJpdmF0ZSBnZXQgbGlzdGVuc0ZvckhvdmVyKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLnRvb2x0aXBTaG93RXZlbnQgPT09IFNob3dUeXBlcy5hbGwgfHwgdGhpcy50b29sdGlwU2hvd0V2ZW50ID09PSBTaG93VHlwZXMubW91c2VvdmVyO1xuICB9XG5cbiAgcHJpdmF0ZSBjb21wb25lbnQ6IGFueTtcbiAgcHJpdmF0ZSB0aW1lb3V0OiBhbnk7XG4gIHByaXZhdGUgbW91c2VMZWF2ZUNvbnRlbnRFdmVudDogYW55O1xuICBwcml2YXRlIG1vdXNlRW50ZXJDb250ZW50RXZlbnQ6IGFueTtcbiAgcHJpdmF0ZSBkb2N1bWVudENsaWNrRXZlbnQ6IGFueTtcblxuICBjb25zdHJ1Y3RvcihcbiAgICBwcml2YXRlIHRvb2x0aXBTZXJ2aWNlOiBUb29sdGlwU2VydmljZSxcbiAgICBwcml2YXRlIHZpZXdDb250YWluZXJSZWY6IFZpZXdDb250YWluZXJSZWYsXG4gICAgcHJpdmF0ZSByZW5kZXJlcjogUmVuZGVyZXIyXG4gICkge31cblxuICBuZ09uRGVzdHJveSgpOiB2b2lkIHtcbiAgICB0aGlzLmhpZGVUb29sdGlwKHRydWUpO1xuICB9XG5cbiAgQEhvc3RMaXN0ZW5lcignZm9jdXNpbicpXG4gIG9uRm9jdXMoKTogdm9pZCB7XG4gICAgaWYgKHRoaXMubGlzdGVuc0ZvckZvY3VzKSB7XG4gICAgICB0aGlzLnNob3dUb29sdGlwKCk7XG4gICAgfVxuICB9XG5cbiAgQEhvc3RMaXN0ZW5lcignYmx1cicpXG4gIG9uQmx1cigpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5saXN0ZW5zRm9yRm9jdXMpIHtcbiAgICAgIHRoaXMuaGlkZVRvb2x0aXAodHJ1ZSk7XG4gICAgfVxuICB9XG5cbiAgQEhvc3RMaXN0ZW5lcignbW91c2VlbnRlcicpXG4gIG9uTW91c2VFbnRlcigpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5saXN0ZW5zRm9ySG92ZXIpIHtcbiAgICAgIHRoaXMuc2hvd1Rvb2x0aXAoKTtcbiAgICB9XG4gIH1cblxuICBASG9zdExpc3RlbmVyKCdtb3VzZWxlYXZlJywgWyckZXZlbnQudGFyZ2V0J10pXG4gIG9uTW91c2VMZWF2ZSh0YXJnZXQpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5saXN0ZW5zRm9ySG92ZXIgJiYgdGhpcy50b29sdGlwQ2xvc2VPbk1vdXNlTGVhdmUpIHtcbiAgICAgIGNsZWFyVGltZW91dCh0aGlzLnRpbWVvdXQpO1xuXG4gICAgICBpZiAodGhpcy5jb21wb25lbnQpIHtcbiAgICAgICAgY29uc3QgY29udGVudERvbSA9IHRoaXMuY29tcG9uZW50Lmluc3RhbmNlLmVsZW1lbnQubmF0aXZlRWxlbWVudDtcbiAgICAgICAgY29uc3QgY29udGFpbnMgPSBjb250ZW50RG9tLmNvbnRhaW5zKHRhcmdldCk7XG4gICAgICAgIGlmIChjb250YWlucykgcmV0dXJuO1xuICAgICAgfVxuXG4gICAgICB0aGlzLmhpZGVUb29sdGlwKHRoaXMudG9vbHRpcEltbWVkaWF0ZUV4aXQpO1xuICAgIH1cbiAgfVxuXG4gIEBIb3N0TGlzdGVuZXIoJ2NsaWNrJylcbiAgb25Nb3VzZUNsaWNrKCkge1xuICAgIGlmICh0aGlzLmxpc3RlbnNGb3JIb3Zlcikge1xuICAgICAgdGhpcy5oaWRlVG9vbHRpcCh0cnVlKTtcbiAgICB9XG4gIH1cblxuICBzaG93VG9vbHRpcChpbW1lZGlhdGU/OiBib29sZWFuKTogdm9pZCB7XG4gICAgaWYgKHRoaXMuY29tcG9uZW50IHx8IHRoaXMudG9vbHRpcERpc2FibGVkKSByZXR1cm47XG5cbiAgICBjb25zdCB0aW1lID0gaW1tZWRpYXRlID8gMCA6IHRoaXMudG9vbHRpcFNob3dUaW1lb3V0O1xuXG4gICAgY2xlYXJUaW1lb3V0KHRoaXMudGltZW91dCk7XG4gICAgdGhpcy50aW1lb3V0ID0gc2V0VGltZW91dCgoKSA9PiB7XG4gICAgICB0aGlzLnRvb2x0aXBTZXJ2aWNlLmRlc3Ryb3lBbGwoKTtcblxuICAgICAgY29uc3Qgb3B0aW9ucyA9IHRoaXMuY3JlYXRlQm91bmRPcHRpb25zKCk7XG4gICAgICB0aGlzLmNvbXBvbmVudCA9IHRoaXMudG9vbHRpcFNlcnZpY2UuY3JlYXRlKG9wdGlvbnMpO1xuXG4gICAgICAvLyBhZGQgYSB0aW55IHRpbWVvdXQgdG8gYXZvaWQgZXZlbnQgcmUtdHJpZ2dlcnNcbiAgICAgIHNldFRpbWVvdXQoKCkgPT4ge1xuICAgICAgICBpZiAodGhpcy5jb21wb25lbnQpIHtcbiAgICAgICAgICB0aGlzLmFkZEhpZGVMaXN0ZW5lcnModGhpcy5jb21wb25lbnQuaW5zdGFuY2UuZWxlbWVudC5uYXRpdmVFbGVtZW50KTtcbiAgICAgICAgfVxuICAgICAgfSwgMTApO1xuXG4gICAgICB0aGlzLnNob3cuZW1pdCh0cnVlKTtcbiAgICB9LCB0aW1lKTtcbiAgfVxuXG4gIGFkZEhpZGVMaXN0ZW5lcnModG9vbHRpcCk6IHZvaWQge1xuICAgIC8vIG9uIG1vdXNlIGVudGVyLCBjYW5jZWwgdGhlIGhpZGUgdHJpZ2dlcmVkIGJ5IHRoZSBsZWF2ZVxuICAgIHRoaXMubW91c2VFbnRlckNvbnRlbnRFdmVudCA9IHRoaXMucmVuZGVyZXIubGlzdGVuKHRvb2x0aXAsICdtb3VzZWVudGVyJywgKCkgPT4ge1xuICAgICAgY2xlYXJUaW1lb3V0KHRoaXMudGltZW91dCk7XG4gICAgfSk7XG5cbiAgICAvLyBjb250ZW50IG1vdXNlIGxlYXZlIGxpc3RlbmVyXG4gICAgaWYgKHRoaXMudG9vbHRpcENsb3NlT25Nb3VzZUxlYXZlKSB7XG4gICAgICB0aGlzLm1vdXNlTGVhdmVDb250ZW50RXZlbnQgPSB0aGlzLnJlbmRlcmVyLmxpc3Rlbih0b29sdGlwLCAnbW91c2VsZWF2ZScsICgpID0+IHtcbiAgICAgICAgdGhpcy5oaWRlVG9vbHRpcCh0aGlzLnRvb2x0aXBJbW1lZGlhdGVFeGl0KTtcbiAgICAgIH0pO1xuICAgIH1cblxuICAgIC8vIGNvbnRlbnQgY2xvc2Ugb24gY2xpY2sgb3V0c2lkZVxuICAgIGlmICh0aGlzLnRvb2x0aXBDbG9zZU9uQ2xpY2tPdXRzaWRlKSB7XG4gICAgICB0aGlzLmRvY3VtZW50Q2xpY2tFdmVudCA9IHRoaXMucmVuZGVyZXIubGlzdGVuKGRvY3VtZW50LCAnY2xpY2snLCBldmVudCA9PiB7XG4gICAgICAgIGNvbnN0IGNvbnRhaW5zID0gdG9vbHRpcC5jb250YWlucyhldmVudC50YXJnZXQpO1xuICAgICAgICBpZiAoIWNvbnRhaW5zKSB0aGlzLmhpZGVUb29sdGlwKCk7XG4gICAgICB9KTtcbiAgICB9XG4gIH1cblxuICBoaWRlVG9vbHRpcChpbW1lZGlhdGU6IGJvb2xlYW4gPSBmYWxzZSk6IHZvaWQge1xuICAgIGlmICghdGhpcy5jb21wb25lbnQpIHJldHVybjtcblxuICAgIGNvbnN0IGRlc3Ryb3lGbiA9ICgpID0+IHtcbiAgICAgIC8vIHJlbW92ZSBldmVudHNcbiAgICAgIGlmICh0aGlzLm1vdXNlTGVhdmVDb250ZW50RXZlbnQpIHRoaXMubW91c2VMZWF2ZUNvbnRlbnRFdmVudCgpO1xuICAgICAgaWYgKHRoaXMubW91c2VFbnRlckNvbnRlbnRFdmVudCkgdGhpcy5tb3VzZUVudGVyQ29udGVudEV2ZW50KCk7XG4gICAgICBpZiAodGhpcy5kb2N1bWVudENsaWNrRXZlbnQpIHRoaXMuZG9jdW1lbnRDbGlja0V2ZW50KCk7XG5cbiAgICAgIC8vIGVtaXQgZXZlbnRzXG4gICAgICB0aGlzLmhpZGUuZW1pdCh0cnVlKTtcblxuICAgICAgLy8gZGVzdHJveSBjb21wb25lbnRcbiAgICAgIHRoaXMudG9vbHRpcFNlcnZpY2UuZGVzdHJveSh0aGlzLmNvbXBvbmVudCk7XG4gICAgICB0aGlzLmNvbXBvbmVudCA9IHVuZGVmaW5lZDtcbiAgICB9O1xuXG4gICAgY2xlYXJUaW1lb3V0KHRoaXMudGltZW91dCk7XG4gICAgaWYgKCFpbW1lZGlhdGUpIHtcbiAgICAgIHRoaXMudGltZW91dCA9IHNldFRpbWVvdXQoZGVzdHJveUZuLCB0aGlzLnRvb2x0aXBIaWRlVGltZW91dCk7XG4gICAgfSBlbHNlIHtcbiAgICAgIGRlc3Ryb3lGbigpO1xuICAgIH1cbiAgfVxuXG4gIHByaXZhdGUgY3JlYXRlQm91bmRPcHRpb25zKCk6IGFueSB7XG4gICAgcmV0dXJuIHtcbiAgICAgIHRpdGxlOiB0aGlzLnRvb2x0aXBUaXRsZSxcbiAgICAgIHRlbXBsYXRlOiB0aGlzLnRvb2x0aXBUZW1wbGF0ZSxcbiAgICAgIGhvc3Q6IHRoaXMudmlld0NvbnRhaW5lclJlZi5lbGVtZW50LFxuICAgICAgcGxhY2VtZW50OiB0aGlzLnRvb2x0aXBQbGFjZW1lbnQsXG4gICAgICBhbGlnbm1lbnQ6IHRoaXMudG9vbHRpcEFsaWdubWVudCxcbiAgICAgIHR5cGU6IHRoaXMudG9vbHRpcFR5cGUsXG4gICAgICBzaG93Q2FyZXQ6IHRoaXMudG9vbHRpcFNob3dDYXJldCxcbiAgICAgIGNzc0NsYXNzOiB0aGlzLnRvb2x0aXBDc3NDbGFzcyxcbiAgICAgIHNwYWNpbmc6IHRoaXMudG9vbHRpcFNwYWNpbmcsXG4gICAgICBjb250ZXh0OiB0aGlzLnRvb2x0aXBDb250ZXh0XG4gICAgfTtcbiAgfVxufVxuIl19