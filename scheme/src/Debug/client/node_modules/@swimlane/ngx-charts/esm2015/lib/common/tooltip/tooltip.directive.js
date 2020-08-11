import { __decorate } from "tslib";
import { Directive, Input, Output, EventEmitter, HostListener, ViewContainerRef, Renderer2, OnDestroy } from '@angular/core';
import { PlacementTypes } from './position';
import { StyleTypes } from './style.type';
import { AlignmentTypes } from './alignment.type';
import { ShowTypes } from './show.type';
import { TooltipService } from './tooltip.service';
let TooltipDirective = class TooltipDirective {
    constructor(tooltipService, viewContainerRef, renderer) {
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
    get listensForFocus() {
        return this.tooltipShowEvent === ShowTypes.all || this.tooltipShowEvent === ShowTypes.focus;
    }
    get listensForHover() {
        return this.tooltipShowEvent === ShowTypes.all || this.tooltipShowEvent === ShowTypes.mouseover;
    }
    ngOnDestroy() {
        this.hideTooltip(true);
    }
    onFocus() {
        if (this.listensForFocus) {
            this.showTooltip();
        }
    }
    onBlur() {
        if (this.listensForFocus) {
            this.hideTooltip(true);
        }
    }
    onMouseEnter() {
        if (this.listensForHover) {
            this.showTooltip();
        }
    }
    onMouseLeave(target) {
        if (this.listensForHover && this.tooltipCloseOnMouseLeave) {
            clearTimeout(this.timeout);
            if (this.component) {
                const contentDom = this.component.instance.element.nativeElement;
                const contains = contentDom.contains(target);
                if (contains)
                    return;
            }
            this.hideTooltip(this.tooltipImmediateExit);
        }
    }
    onMouseClick() {
        if (this.listensForHover) {
            this.hideTooltip(true);
        }
    }
    showTooltip(immediate) {
        if (this.component || this.tooltipDisabled)
            return;
        const time = immediate ? 0 : this.tooltipShowTimeout;
        clearTimeout(this.timeout);
        this.timeout = setTimeout(() => {
            this.tooltipService.destroyAll();
            const options = this.createBoundOptions();
            this.component = this.tooltipService.create(options);
            // add a tiny timeout to avoid event re-triggers
            setTimeout(() => {
                if (this.component) {
                    this.addHideListeners(this.component.instance.element.nativeElement);
                }
            }, 10);
            this.show.emit(true);
        }, time);
    }
    addHideListeners(tooltip) {
        // on mouse enter, cancel the hide triggered by the leave
        this.mouseEnterContentEvent = this.renderer.listen(tooltip, 'mouseenter', () => {
            clearTimeout(this.timeout);
        });
        // content mouse leave listener
        if (this.tooltipCloseOnMouseLeave) {
            this.mouseLeaveContentEvent = this.renderer.listen(tooltip, 'mouseleave', () => {
                this.hideTooltip(this.tooltipImmediateExit);
            });
        }
        // content close on click outside
        if (this.tooltipCloseOnClickOutside) {
            this.documentClickEvent = this.renderer.listen(document, 'click', event => {
                const contains = tooltip.contains(event.target);
                if (!contains)
                    this.hideTooltip();
            });
        }
    }
    hideTooltip(immediate = false) {
        if (!this.component)
            return;
        const destroyFn = () => {
            // remove events
            if (this.mouseLeaveContentEvent)
                this.mouseLeaveContentEvent();
            if (this.mouseEnterContentEvent)
                this.mouseEnterContentEvent();
            if (this.documentClickEvent)
                this.documentClickEvent();
            // emit events
            this.hide.emit(true);
            // destroy component
            this.tooltipService.destroy(this.component);
            this.component = undefined;
        };
        clearTimeout(this.timeout);
        if (!immediate) {
            this.timeout = setTimeout(destroyFn, this.tooltipHideTimeout);
        }
        else {
            destroyFn();
        }
    }
    createBoundOptions() {
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
    }
};
TooltipDirective.ctorParameters = () => [
    { type: TooltipService },
    { type: ViewContainerRef },
    { type: Renderer2 }
];
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
export { TooltipDirective };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidG9vbHRpcC5kaXJlY3RpdmUuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vdG9vbHRpcC90b29sdGlwLmRpcmVjdGl2ZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUNMLFNBQVMsRUFDVCxLQUFLLEVBQ0wsTUFBTSxFQUNOLFlBQVksRUFDWixZQUFZLEVBQ1osZ0JBQWdCLEVBQ2hCLFNBQVMsRUFDVCxTQUFTLEVBQ1YsTUFBTSxlQUFlLENBQUM7QUFFdkIsT0FBTyxFQUFFLGNBQWMsRUFBRSxNQUFNLFlBQVksQ0FBQztBQUM1QyxPQUFPLEVBQUUsVUFBVSxFQUFFLE1BQU0sY0FBYyxDQUFDO0FBQzFDLE9BQU8sRUFBRSxjQUFjLEVBQUUsTUFBTSxrQkFBa0IsQ0FBQztBQUNsRCxPQUFPLEVBQUUsU0FBUyxFQUFFLE1BQU0sYUFBYSxDQUFDO0FBRXhDLE9BQU8sRUFBRSxjQUFjLEVBQUUsTUFBTSxtQkFBbUIsQ0FBQztBQUduRCxJQUFhLGdCQUFnQixHQUE3QixNQUFhLGdCQUFnQjtJQW9DM0IsWUFDVSxjQUE4QixFQUM5QixnQkFBa0MsRUFDbEMsUUFBbUI7UUFGbkIsbUJBQWMsR0FBZCxjQUFjLENBQWdCO1FBQzlCLHFCQUFnQixHQUFoQixnQkFBZ0IsQ0FBa0I7UUFDbEMsYUFBUSxHQUFSLFFBQVEsQ0FBVztRQXRDcEIsb0JBQWUsR0FBVyxFQUFFLENBQUM7UUFDN0IsaUJBQVksR0FBVyxFQUFFLENBQUM7UUFDMUIsd0JBQW1CLEdBQVksSUFBSSxDQUFDO1FBQ3BDLG1CQUFjLEdBQVcsRUFBRSxDQUFDO1FBQzVCLG9CQUFlLEdBQVksS0FBSyxDQUFDO1FBQ2pDLHFCQUFnQixHQUFZLElBQUksQ0FBQztRQUNqQyxxQkFBZ0IsR0FBbUIsY0FBYyxDQUFDLEdBQUcsQ0FBQztRQUN0RCxxQkFBZ0IsR0FBbUIsY0FBYyxDQUFDLE1BQU0sQ0FBQztRQUN6RCxn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b0RBYTdDO0FBR0Q7SUFEQyxZQUFZLENBQUMsT0FBTyxDQUFDO29EQUtyQjtBQXZGVSxnQkFBZ0I7SUFENUIsU0FBUyxDQUFDLEVBQUUsUUFBUSxFQUFFLGVBQWUsRUFBRSxDQUFDO0dBQzVCLGdCQUFnQixDQTZLNUI7U0E3S1ksZ0JBQWdCIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHtcbiAgRGlyZWN0aXZlLFxuICBJbnB1dCxcbiAgT3V0cHV0LFxuICBFdmVudEVtaXR0ZXIsXG4gIEhvc3RMaXN0ZW5lcixcbiAgVmlld0NvbnRhaW5lclJlZixcbiAgUmVuZGVyZXIyLFxuICBPbkRlc3Ryb3lcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5cbmltcG9ydCB7IFBsYWNlbWVudFR5cGVzIH0gZnJvbSAnLi9wb3NpdGlvbic7XG5pbXBvcnQgeyBTdHlsZVR5cGVzIH0gZnJvbSAnLi9zdHlsZS50eXBlJztcbmltcG9ydCB7IEFsaWdubWVudFR5cGVzIH0gZnJvbSAnLi9hbGlnbm1lbnQudHlwZSc7XG5pbXBvcnQgeyBTaG93VHlwZXMgfSBmcm9tICcuL3Nob3cudHlwZSc7XG5cbmltcG9ydCB7IFRvb2x0aXBTZXJ2aWNlIH0gZnJvbSAnLi90b29sdGlwLnNlcnZpY2UnO1xuXG5ARGlyZWN0aXZlKHsgc2VsZWN0b3I6ICdbbmd4LXRvb2x0aXBdJyB9KVxuZXhwb3J0IGNsYXNzIFRvb2x0aXBEaXJlY3RpdmUgaW1wbGVtZW50cyBPbkRlc3Ryb3kge1xuICBASW5wdXQoKSB0b29sdGlwQ3NzQ2xhc3M6IHN0cmluZyA9ICcnO1xuICBASW5wdXQoKSB0b29sdGlwVGl0bGU6IHN0cmluZyA9ICcnO1xuICBASW5wdXQoKSB0b29sdGlwQXBwZW5kVG9Cb2R5OiBib29sZWFuID0gdHJ1ZTtcbiAgQElucHV0KCkgdG9vbHRpcFNwYWNpbmc6IG51bWJlciA9IDEwO1xuICBASW5wdXQoKSB0b29sdGlwRGlzYWJsZWQ6IGJvb2xlYW4gPSBmYWxzZTtcbiAgQElucHV0KCkgdG9vbHRpcFNob3dDYXJldDogYm9vbGVhbiA9IHRydWU7XG4gIEBJbnB1dCgpIHRvb2x0aXBQbGFjZW1lbnQ6IFBsYWNlbWVudFR5cGVzID0gUGxhY2VtZW50VHlwZXMudG9wO1xuICBASW5wdXQoKSB0b29sdGlwQWxpZ25tZW50OiBBbGlnbm1lbnRUeXBlcyA9IEFsaWdubWVudFR5cGVzLmNlbnRlcjtcbiAgQElucHV0KCkgdG9vbHRpcFR5cGU6IFN0eWxlVHlwZXMgPSBTdHlsZVR5cGVzLnBvcG92ZXI7XG4gIEBJbnB1dCgpIHRvb2x0aXBDbG9zZU9uQ2xpY2tPdXRzaWRlOiBib29sZWFuID0gdHJ1ZTtcbiAgQElucHV0KCkgdG9vbHRpcENsb3NlT25Nb3VzZUxlYXZlOiBib29sZWFuID0gdHJ1ZTtcbiAgQElucHV0KCkgdG9vbHRpcEhpZGVUaW1lb3V0OiBudW1iZXIgPSAzMDA7XG4gIEBJbnB1dCgpIHRvb2x0aXBTaG93VGltZW91dDogbnVtYmVyID0gMTAwO1xuICBASW5wdXQoKSB0b29sdGlwVGVtcGxhdGU6IGFueTtcbiAgQElucHV0KCkgdG9vbHRpcFNob3dFdmVudDogU2hvd1R5cGVzID0gU2hvd1R5cGVzLmFsbDtcbiAgQElucHV0KCkgdG9vbHRpcENvbnRleHQ6IGFueTtcbiAgQElucHV0KCkgdG9vbHRpcEltbWVkaWF0ZUV4aXQ6IGJvb2xlYW4gPSBmYWxzZTtcblxuICBAT3V0cHV0KCkgc2hvdyA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcbiAgQE91dHB1dCgpIGhpZGUgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgcHJpdmF0ZSBnZXQgbGlzdGVuc0ZvckZvY3VzKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLnRvb2x0aXBTaG93RXZlbnQgPT09IFNob3dUeXBlcy5hbGwgfHwgdGhpcy50b29sdGlwU2hvd0V2ZW50ID09PSBTaG93VHlwZXMuZm9jdXM7XG4gIH1cblxuICBwcml2YXRlIGdldCBsaXN0ZW5zRm9ySG92ZXIoKTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIHRoaXMudG9vbHRpcFNob3dFdmVudCA9PT0gU2hvd1R5cGVzLmFsbCB8fCB0aGlzLnRvb2x0aXBTaG93RXZlbnQgPT09IFNob3dUeXBlcy5tb3VzZW92ZXI7XG4gIH1cblxuICBwcml2YXRlIGNvbXBvbmVudDogYW55O1xuICBwcml2YXRlIHRpbWVvdXQ6IGFueTtcbiAgcHJpdmF0ZSBtb3VzZUxlYXZlQ29udGVudEV2ZW50OiBhbnk7XG4gIHByaXZhdGUgbW91c2VFbnRlckNvbnRlbnRFdmVudDogYW55O1xuICBwcml2YXRlIGRvY3VtZW50Q2xpY2tFdmVudDogYW55O1xuXG4gIGNvbnN0cnVjdG9yKFxuICAgIHByaXZhdGUgdG9vbHRpcFNlcnZpY2U6IFRvb2x0aXBTZXJ2aWNlLFxuICAgIHByaXZhdGUgdmlld0NvbnRhaW5lclJlZjogVmlld0NvbnRhaW5lclJlZixcbiAgICBwcml2YXRlIHJlbmRlcmVyOiBSZW5kZXJlcjJcbiAgKSB7fVxuXG4gIG5nT25EZXN0cm95KCk6IHZvaWQge1xuICAgIHRoaXMuaGlkZVRvb2x0aXAodHJ1ZSk7XG4gIH1cblxuICBASG9zdExpc3RlbmVyKCdmb2N1c2luJylcbiAgb25Gb2N1cygpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5saXN0ZW5zRm9yRm9jdXMpIHtcbiAgICAgIHRoaXMuc2hvd1Rvb2x0aXAoKTtcbiAgICB9XG4gIH1cblxuICBASG9zdExpc3RlbmVyKCdibHVyJylcbiAgb25CbHVyKCk6IHZvaWQge1xuICAgIGlmICh0aGlzLmxpc3RlbnNGb3JGb2N1cykge1xuICAgICAgdGhpcy5oaWRlVG9vbHRpcCh0cnVlKTtcbiAgICB9XG4gIH1cblxuICBASG9zdExpc3RlbmVyKCdtb3VzZWVudGVyJylcbiAgb25Nb3VzZUVudGVyKCk6IHZvaWQge1xuICAgIGlmICh0aGlzLmxpc3RlbnNGb3JIb3Zlcikge1xuICAgICAgdGhpcy5zaG93VG9vbHRpcCgpO1xuICAgIH1cbiAgfVxuXG4gIEBIb3N0TGlzdGVuZXIoJ21vdXNlbGVhdmUnLCBbJyRldmVudC50YXJnZXQnXSlcbiAgb25Nb3VzZUxlYXZlKHRhcmdldCk6IHZvaWQge1xuICAgIGlmICh0aGlzLmxpc3RlbnNGb3JIb3ZlciAmJiB0aGlzLnRvb2x0aXBDbG9zZU9uTW91c2VMZWF2ZSkge1xuICAgICAgY2xlYXJUaW1lb3V0KHRoaXMudGltZW91dCk7XG5cbiAgICAgIGlmICh0aGlzLmNvbXBvbmVudCkge1xuICAgICAgICBjb25zdCBjb250ZW50RG9tID0gdGhpcy5jb21wb25lbnQuaW5zdGFuY2UuZWxlbWVudC5uYXRpdmVFbGVtZW50O1xuICAgICAgICBjb25zdCBjb250YWlucyA9IGNvbnRlbnREb20uY29udGFpbnModGFyZ2V0KTtcbiAgICAgICAgaWYgKGNvbnRhaW5zKSByZXR1cm47XG4gICAgICB9XG5cbiAgICAgIHRoaXMuaGlkZVRvb2x0aXAodGhpcy50b29sdGlwSW1tZWRpYXRlRXhpdCk7XG4gICAgfVxuICB9XG5cbiAgQEhvc3RMaXN0ZW5lcignY2xpY2snKVxuICBvbk1vdXNlQ2xpY2soKSB7XG4gICAgaWYgKHRoaXMubGlzdGVuc0ZvckhvdmVyKSB7XG4gICAgICB0aGlzLmhpZGVUb29sdGlwKHRydWUpO1xuICAgIH1cbiAgfVxuXG4gIHNob3dUb29sdGlwKGltbWVkaWF0ZT86IGJvb2xlYW4pOiB2b2lkIHtcbiAgICBpZiAodGhpcy5jb21wb25lbnQgfHwgdGhpcy50b29sdGlwRGlzYWJsZWQpIHJldHVybjtcblxuICAgIGNvbnN0IHRpbWUgPSBpbW1lZGlhdGUgPyAwIDogdGhpcy50b29sdGlwU2hvd1RpbWVvdXQ7XG5cbiAgICBjbGVhclRpbWVvdXQodGhpcy50aW1lb3V0KTtcbiAgICB0aGlzLnRpbWVvdXQgPSBzZXRUaW1lb3V0KCgpID0+IHtcbiAgICAgIHRoaXMudG9vbHRpcFNlcnZpY2UuZGVzdHJveUFsbCgpO1xuXG4gICAgICBjb25zdCBvcHRpb25zID0gdGhpcy5jcmVhdGVCb3VuZE9wdGlvbnMoKTtcbiAgICAgIHRoaXMuY29tcG9uZW50ID0gdGhpcy50b29sdGlwU2VydmljZS5jcmVhdGUob3B0aW9ucyk7XG5cbiAgICAgIC8vIGFkZCBhIHRpbnkgdGltZW91dCB0byBhdm9pZCBldmVudCByZS10cmlnZ2Vyc1xuICAgICAgc2V0VGltZW91dCgoKSA9PiB7XG4gICAgICAgIGlmICh0aGlzLmNvbXBvbmVudCkge1xuICAgICAgICAgIHRoaXMuYWRkSGlkZUxpc3RlbmVycyh0aGlzLmNvbXBvbmVudC5pbnN0YW5jZS5lbGVtZW50Lm5hdGl2ZUVsZW1lbnQpO1xuICAgICAgICB9XG4gICAgICB9LCAxMCk7XG5cbiAgICAgIHRoaXMuc2hvdy5lbWl0KHRydWUpO1xuICAgIH0sIHRpbWUpO1xuICB9XG5cbiAgYWRkSGlkZUxpc3RlbmVycyh0b29sdGlwKTogdm9pZCB7XG4gICAgLy8gb24gbW91c2UgZW50ZXIsIGNhbmNlbCB0aGUgaGlkZSB0cmlnZ2VyZWQgYnkgdGhlIGxlYXZlXG4gICAgdGhpcy5tb3VzZUVudGVyQ29udGVudEV2ZW50ID0gdGhpcy5yZW5kZXJlci5saXN0ZW4odG9vbHRpcCwgJ21vdXNlZW50ZXInLCAoKSA9PiB7XG4gICAgICBjbGVhclRpbWVvdXQodGhpcy50aW1lb3V0KTtcbiAgICB9KTtcblxuICAgIC8vIGNvbnRlbnQgbW91c2UgbGVhdmUgbGlzdGVuZXJcbiAgICBpZiAodGhpcy50b29sdGlwQ2xvc2VPbk1vdXNlTGVhdmUpIHtcbiAgICAgIHRoaXMubW91c2VMZWF2ZUNvbnRlbnRFdmVudCA9IHRoaXMucmVuZGVyZXIubGlzdGVuKHRvb2x0aXAsICdtb3VzZWxlYXZlJywgKCkgPT4ge1xuICAgICAgICB0aGlzLmhpZGVUb29sdGlwKHRoaXMudG9vbHRpcEltbWVkaWF0ZUV4aXQpO1xuICAgICAgfSk7XG4gICAgfVxuXG4gICAgLy8gY29udGVudCBjbG9zZSBvbiBjbGljayBvdXRzaWRlXG4gICAgaWYgKHRoaXMudG9vbHRpcENsb3NlT25DbGlja091dHNpZGUpIHtcbiAgICAgIHRoaXMuZG9jdW1lbnRDbGlja0V2ZW50ID0gdGhpcy5yZW5kZXJlci5saXN0ZW4oZG9jdW1lbnQsICdjbGljaycsIGV2ZW50ID0+IHtcbiAgICAgICAgY29uc3QgY29udGFpbnMgPSB0b29sdGlwLmNvbnRhaW5zKGV2ZW50LnRhcmdldCk7XG4gICAgICAgIGlmICghY29udGFpbnMpIHRoaXMuaGlkZVRvb2x0aXAoKTtcbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuXG4gIGhpZGVUb29sdGlwKGltbWVkaWF0ZTogYm9vbGVhbiA9IGZhbHNlKTogdm9pZCB7XG4gICAgaWYgKCF0aGlzLmNvbXBvbmVudCkgcmV0dXJuO1xuXG4gICAgY29uc3QgZGVzdHJveUZuID0gKCkgPT4ge1xuICAgICAgLy8gcmVtb3ZlIGV2ZW50c1xuICAgICAgaWYgKHRoaXMubW91c2VMZWF2ZUNvbnRlbnRFdmVudCkgdGhpcy5tb3VzZUxlYXZlQ29udGVudEV2ZW50KCk7XG4gICAgICBpZiAodGhpcy5tb3VzZUVudGVyQ29udGVudEV2ZW50KSB0aGlzLm1vdXNlRW50ZXJDb250ZW50RXZlbnQoKTtcbiAgICAgIGlmICh0aGlzLmRvY3VtZW50Q2xpY2tFdmVudCkgdGhpcy5kb2N1bWVudENsaWNrRXZlbnQoKTtcblxuICAgICAgLy8gZW1pdCBldmVudHNcbiAgICAgIHRoaXMuaGlkZS5lbWl0KHRydWUpO1xuXG4gICAgICAvLyBkZXN0cm95IGNvbXBvbmVudFxuICAgICAgdGhpcy50b29sdGlwU2VydmljZS5kZXN0cm95KHRoaXMuY29tcG9uZW50KTtcbiAgICAgIHRoaXMuY29tcG9uZW50ID0gdW5kZWZpbmVkO1xuICAgIH07XG5cbiAgICBjbGVhclRpbWVvdXQodGhpcy50aW1lb3V0KTtcbiAgICBpZiAoIWltbWVkaWF0ZSkge1xuICAgICAgdGhpcy50aW1lb3V0ID0gc2V0VGltZW91dChkZXN0cm95Rm4sIHRoaXMudG9vbHRpcEhpZGVUaW1lb3V0KTtcbiAgICB9IGVsc2Uge1xuICAgICAgZGVzdHJveUZuKCk7XG4gICAgfVxuICB9XG5cbiAgcHJpdmF0ZSBjcmVhdGVCb3VuZE9wdGlvbnMoKTogYW55IHtcbiAgICByZXR1cm4ge1xuICAgICAgdGl0bGU6IHRoaXMudG9vbHRpcFRpdGxlLFxuICAgICAgdGVtcGxhdGU6IHRoaXMudG9vbHRpcFRlbXBsYXRlLFxuICAgICAgaG9zdDogdGhpcy52aWV3Q29udGFpbmVyUmVmLmVsZW1lbnQsXG4gICAgICBwbGFjZW1lbnQ6IHRoaXMudG9vbHRpcFBsYWNlbWVudCxcbiAgICAgIGFsaWdubWVudDogdGhpcy50b29sdGlwQWxpZ25tZW50LFxuICAgICAgdHlwZTogdGhpcy50b29sdGlwVHlwZSxcbiAgICAgIHNob3dDYXJldDogdGhpcy50b29sdGlwU2hvd0NhcmV0LFxuICAgICAgY3NzQ2xhc3M6IHRoaXMudG9vbHRpcENzc0NsYXNzLFxuICAgICAgc3BhY2luZzogdGhpcy50b29sdGlwU3BhY2luZyxcbiAgICAgIGNvbnRleHQ6IHRoaXMudG9vbHRpcENvbnRleHRcbiAgICB9O1xuICB9XG59XG4iXX0=