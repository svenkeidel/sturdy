import { __decorate } from "tslib";
import { Input, Component, ElementRef, AfterViewInit, ViewEncapsulation, HostListener, ViewChild, HostBinding, Renderer2 } from '@angular/core';
import { throttleable } from '../../utils/throttle';
import { PositionHelper } from './position';
var TooltipContentComponent = /** @class */ (function () {
    function TooltipContentComponent(element, renderer) {
        this.element = element;
        this.renderer = renderer;
    }
    Object.defineProperty(TooltipContentComponent.prototype, "cssClasses", {
        get: function () {
            var clz = 'ngx-charts-tooltip-content';
            clz += " position-" + this.placement;
            clz += " type-" + this.type;
            clz += " " + this.cssClass;
            return clz;
        },
        enumerable: true,
        configurable: true
    });
    TooltipContentComponent.prototype.ngAfterViewInit = function () {
        setTimeout(this.position.bind(this));
    };
    TooltipContentComponent.prototype.position = function () {
        var _this = this;
        var nativeElm = this.element.nativeElement;
        var hostDim = this.host.nativeElement.getBoundingClientRect();
        // if no dims were found, never show
        if (!hostDim.height && !hostDim.width)
            return;
        var elmDim = nativeElm.getBoundingClientRect();
        this.checkFlip(hostDim, elmDim);
        this.positionContent(nativeElm, hostDim, elmDim);
        if (this.showCaret) {
            this.positionCaret(hostDim, elmDim);
        }
        // animate its entry
        setTimeout(function () { return _this.renderer.addClass(nativeElm, 'animate'); }, 1);
    };
    TooltipContentComponent.prototype.positionContent = function (nativeElm, hostDim, elmDim) {
        var _a = PositionHelper.positionContent(this.placement, elmDim, hostDim, this.spacing, this.alignment), top = _a.top, left = _a.left;
        this.renderer.setStyle(nativeElm, 'top', top + "px");
        this.renderer.setStyle(nativeElm, 'left', left + "px");
    };
    TooltipContentComponent.prototype.positionCaret = function (hostDim, elmDim) {
        var caretElm = this.caretElm.nativeElement;
        var caretDimensions = caretElm.getBoundingClientRect();
        var _a = PositionHelper.positionCaret(this.placement, elmDim, hostDim, caretDimensions, this.alignment), top = _a.top, left = _a.left;
        this.renderer.setStyle(caretElm, 'top', top + "px");
        this.renderer.setStyle(caretElm, 'left', left + "px");
    };
    TooltipContentComponent.prototype.checkFlip = function (hostDim, elmDim) {
        this.placement = PositionHelper.determinePlacement(this.placement, elmDim, hostDim, this.spacing);
    };
    TooltipContentComponent.prototype.onWindowResize = function () {
        this.position();
    };
    TooltipContentComponent.ctorParameters = function () { return [
        { type: ElementRef },
        { type: Renderer2 }
    ]; };
    __decorate([
        Input()
    ], TooltipContentComponent.prototype, "host", void 0);
    __decorate([
        Input()
    ], TooltipContentComponent.prototype, "showCaret", void 0);
    __decorate([
        Input()
    ], TooltipContentComponent.prototype, "type", void 0);
    __decorate([
        Input()
    ], TooltipContentComponent.prototype, "placement", void 0);
    __decorate([
        Input()
    ], TooltipContentComponent.prototype, "alignment", void 0);
    __decorate([
        Input()
    ], TooltipContentComponent.prototype, "spacing", void 0);
    __decorate([
        Input()
    ], TooltipContentComponent.prototype, "cssClass", void 0);
    __decorate([
        Input()
    ], TooltipContentComponent.prototype, "title", void 0);
    __decorate([
        Input()
    ], TooltipContentComponent.prototype, "template", void 0);
    __decorate([
        Input()
    ], TooltipContentComponent.prototype, "context", void 0);
    __decorate([
        ViewChild('caretElm')
    ], TooltipContentComponent.prototype, "caretElm", void 0);
    __decorate([
        HostBinding('class')
    ], TooltipContentComponent.prototype, "cssClasses", null);
    __decorate([
        HostListener('window:resize'),
        throttleable(100)
    ], TooltipContentComponent.prototype, "onWindowResize", null);
    TooltipContentComponent = __decorate([
        Component({
            selector: 'ngx-tooltip-content',
            template: "\n    <div>\n      <span #caretElm [hidden]=\"!showCaret\" class=\"tooltip-caret position-{{ this.placement }}\"> </span>\n      <div class=\"tooltip-content\">\n        <span *ngIf=\"!title\">\n          <ng-template [ngTemplateOutlet]=\"template\" [ngTemplateOutletContext]=\"{ model: context }\"> </ng-template>\n        </span>\n        <span *ngIf=\"title\" [innerHTML]=\"title\"> </span>\n      </div>\n    </div>\n  ",
            encapsulation: ViewEncapsulation.None,
            styles: [".ngx-charts-tooltip-content{position:fixed;border-radius:3px;z-index:5000;display:block;font-weight:400;opacity:0;pointer-events:none!important}.ngx-charts-tooltip-content.type-popover{background:#fff;color:#060709;border:1px solid #72809b;box-shadow:0 1px 3px 0 rgba(0,0,0,.2),0 1px 1px 0 rgba(0,0,0,.14),0 2px 1px -1px rgba(0,0,0,.12);font-size:13px;padding:4px}.ngx-charts-tooltip-content.type-popover .tooltip-caret{position:absolute;z-index:5001;width:0;height:0}.ngx-charts-tooltip-content.type-popover .tooltip-caret.position-left{border-top:7px solid transparent;border-bottom:7px solid transparent;border-left:7px solid #fff}.ngx-charts-tooltip-content.type-popover .tooltip-caret.position-top{border-left:7px solid transparent;border-right:7px solid transparent;border-top:7px solid #fff}.ngx-charts-tooltip-content.type-popover .tooltip-caret.position-right{border-top:7px solid transparent;border-bottom:7px solid transparent;border-right:7px solid #fff}.ngx-charts-tooltip-content.type-popover .tooltip-caret.position-bottom{border-left:7px solid transparent;border-right:7px solid transparent;border-bottom:7px solid #fff}.ngx-charts-tooltip-content.type-tooltip{color:#fff;background:rgba(0,0,0,.75);font-size:12px;padding:0 10px;text-align:center;pointer-events:auto}.ngx-charts-tooltip-content.type-tooltip .tooltip-caret.position-left{border-top:7px solid transparent;border-bottom:7px solid transparent;border-left:7px solid rgba(0,0,0,.75)}.ngx-charts-tooltip-content.type-tooltip .tooltip-caret.position-top{border-left:7px solid transparent;border-right:7px solid transparent;border-top:7px solid rgba(0,0,0,.75)}.ngx-charts-tooltip-content.type-tooltip .tooltip-caret.position-right{border-top:7px solid transparent;border-bottom:7px solid transparent;border-right:7px solid rgba(0,0,0,.75)}.ngx-charts-tooltip-content.type-tooltip .tooltip-caret.position-bottom{border-left:7px solid transparent;border-right:7px solid transparent;border-bottom:7px solid rgba(0,0,0,.75)}.ngx-charts-tooltip-content .tooltip-label{display:block;line-height:1em;padding:8px 5px 5px;font-size:1em}.ngx-charts-tooltip-content .tooltip-val{display:block;font-size:1.3em;line-height:1em;padding:0 5px 8px}.ngx-charts-tooltip-content .tooltip-caret{position:absolute;z-index:5001;width:0;height:0}.ngx-charts-tooltip-content.position-right{-webkit-transform:translate3d(10px,0,0);transform:translate3d(10px,0,0)}.ngx-charts-tooltip-content.position-left{-webkit-transform:translate3d(-10px,0,0);transform:translate3d(-10px,0,0)}.ngx-charts-tooltip-content.position-top{-webkit-transform:translate3d(0,-10px,0);transform:translate3d(0,-10px,0)}.ngx-charts-tooltip-content.position-bottom{-webkit-transform:translate3d(0,10px,0);transform:translate3d(0,10px,0)}.ngx-charts-tooltip-content.animate{opacity:1;-webkit-transition:opacity .3s,-webkit-transform .3s;transition:opacity .3s,transform .3s,-webkit-transform .3s;-webkit-transform:translate3d(0,0,0);transform:translate3d(0,0,0);pointer-events:auto}.area-tooltip-container{padding:5px 0;pointer-events:none}.tooltip-item{text-align:left;line-height:1.2em;padding:5px 0}.tooltip-item .tooltip-item-color{display:inline-block;height:12px;width:12px;margin-right:5px;color:#5b646b;border-radius:3px}"]
        })
    ], TooltipContentComponent);
    return TooltipContentComponent;
}());
export { TooltipContentComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidG9vbHRpcC5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vdG9vbHRpcC90b29sdGlwLmNvbXBvbmVudC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUNMLEtBQUssRUFDTCxTQUFTLEVBQ1QsVUFBVSxFQUNWLGFBQWEsRUFDYixpQkFBaUIsRUFDakIsWUFBWSxFQUNaLFNBQVMsRUFDVCxXQUFXLEVBQ1gsU0FBUyxFQUNWLE1BQU0sZUFBZSxDQUFDO0FBRXZCLE9BQU8sRUFBRSxZQUFZLEVBQUUsTUFBTSxzQkFBc0IsQ0FBQztBQUNwRCxPQUFPLEVBQUUsY0FBYyxFQUFrQixNQUFNLFlBQVksQ0FBQztBQXFCNUQ7SUF1QkUsaUNBQW1CLE9BQW1CLEVBQVUsUUFBbUI7UUFBaEQsWUFBTyxHQUFQLE9BQU8sQ0FBWTtRQUFVLGFBQVEsR0FBUixRQUFRLENBQVc7SUFBRyxDQUFDO0lBUnZFLHNCQUFJLCtDQUFVO2FBQWQ7WUFDRSxJQUFJLEdBQUcsR0FBRyw0QkFBNEIsQ0FBQztZQUN2QyxHQUFHLElBQUksZUFBYSxJQUFJLENBQUMsU0FBVyxDQUFDO1lBQ3JDLEdBQUcsSUFBSSxXQUFTLElBQUksQ0FBQyxJQUFNLENBQUM7WUFDNUIsR0FBRyxJQUFJLE1BQUksSUFBSSxDQUFDLFFBQVUsQ0FBQztZQUMzQixPQUFPLEdBQUcsQ0FBQztRQUNiLENBQUM7OztPQUFBO0lBSUQsaURBQWUsR0FBZjtRQUNFLFVBQVUsQ0FBQyxJQUFJLENBQUMsUUFBUSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDO0lBQ3ZDLENBQUM7SUFFRCwwQ0FBUSxHQUFSO1FBQUEsaUJBaUJDO1FBaEJDLElBQU0sU0FBUyxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUMsYUFBYSxDQUFDO1FBQzdDLElBQU0sT0FBTyxHQUFHLElBQUksQ0FBQyxJQUFJLENBQUMsYUFB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LHlhQVVUO1lBQ0QsYUFBYSxFQUFFLGlCQUFpQixDQUFDLElBQUk7O1NBRXRDLENBQUM7T0FDVyx1QkFBdUIsQ0ErRW5DO0lBQUQsOEJBQUM7Q0FBQSxBQS9FRCxJQStFQztTQS9FWSx1QkFBdUIiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQge1xuICBJbnB1dCxcbiAgQ29tcG9uZW50LFxuICBFbGVtZW50UmVmLFxuICBBZnRlclZpZXdJbml0LFxuICBWaWV3RW5jYXBzdWxhdGlvbixcbiAgSG9zdExpc3RlbmVyLFxuICBWaWV3Q2hpbGQsXG4gIEhvc3RCaW5kaW5nLFxuICBSZW5kZXJlcjJcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5cbmltcG9ydCB7IHRocm90dGxlYWJsZSB9IGZyb20gJy4uLy4uL3V0aWxzL3Rocm90dGxlJztcbmltcG9ydCB7IFBvc2l0aW9uSGVscGVyLCBQbGFjZW1lbnRUeXBlcyB9IGZyb20gJy4vcG9zaXRpb24nO1xuXG5pbXBvcnQgeyBTdHlsZVR5cGVzIH0gZnJvbSAnLi9zdHlsZS50eXBlJztcbmltcG9ydCB7IEFsaWdubWVudFR5cGVzIH0gZnJvbSAnLi9hbGlnbm1lbnQudHlwZSc7XG5cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ25neC10b29sdGlwLWNvbnRlbnQnLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxkaXY+XG4gICAgICA8c3BhbiAjY2FyZXRFbG0gW2hpZGRlbl09XCIhc2hvd0NhcmV0XCIgY2xhc3M9XCJ0b29sdGlwLWNhcmV0IHBvc2l0aW9uLXt7IHRoaXMucGxhY2VtZW50IH19XCI+IDwvc3Bhbj5cbiAgICAgIDxkaXYgY2xhc3M9XCJ0b29sdGlwLWNvbnRlbnRcIj5cbiAgICAgICAgPHNwYW4gKm5nSWY9XCIhdGl0bGVcIj5cbiAgICAgICAgICA8bmctdGVtcGxhdGUgW25nVGVtcGxhdGVPdXRsZXRdPVwidGVtcGxhdGVcIiBbbmdUZW1wbGF0ZU91dGxldENvbnRleHRdPVwieyBtb2RlbDogY29udGV4dCB9XCI+IDwvbmctdGVtcGxhdGU+XG4gICAgICAgIDwvc3Bhbj5cbiAgICAgICAgPHNwYW4gKm5nSWY9XCJ0aXRsZVwiIFtpbm5lckhUTUxdPVwidGl0bGVcIj4gPC9zcGFuPlxuICAgICAgPC9kaXY+XG4gICAgPC9kaXY+XG4gIGAsXG4gIGVuY2Fwc3VsYXRpb246IFZpZXdFbmNhcHN1bGF0aW9uLk5vbmUsXG4gIHN0eWxlVXJsczogWycuL3Rvb2x0aXAuY29tcG9uZW50LnNjc3MnXVxufSlcbmV4cG9ydCBjbGFzcyBUb29sdGlwQ29udGVudENvbXBvbmVudCBpbXBsZW1lbnRzIEFmdGVyVmlld0luaXQge1xuICBASW5wdXQoKSBob3N0OiBhbnk7XG4gIEBJbnB1dCgpIHNob3dDYXJldDogYm9vbGVhbjtcbiAgQElucHV0KCkgdHlwZTogU3R5bGVUeXBlcztcbiAgQElucHV0KCkgcGxhY2VtZW50OiBQbGFjZW1lbnRUeXBlcztcbiAgQElucHV0KCkgYWxpZ25tZW50OiBBbGlnbm1lbnRUeXBlcztcbiAgQElucHV0KCkgc3BhY2luZzogbnVtYmVyO1xuICBASW5wdXQoKSBjc3NDbGFzczogc3RyaW5nO1xuICBASW5wdXQoKSB0aXRsZTogc3RyaW5nO1xuICBASW5wdXQoKSB0ZW1wbGF0ZTogYW55O1xuICBASW5wdXQoKSBjb250ZXh0OiBhbnk7XG5cbiAgQFZpZXdDaGlsZCgnY2FyZXRFbG0nKSBjYXJldEVsbTtcblxuICBASG9zdEJpbmRpbmcoJ2NsYXNzJylcbiAgZ2V0IGNzc0NsYXNzZXMoKTogc3RyaW5nIHtcbiAgICBsZXQgY2x6ID0gJ25neC1jaGFydHMtdG9vbHRpcC1jb250ZW50JztcbiAgICBjbHogKz0gYCBwb3NpdGlvbi0ke3RoaXMucGxhY2VtZW50fWA7XG4gICAgY2x6ICs9IGAgdHlwZS0ke3RoaXMudHlwZX1gO1xuICAgIGNseiArPSBgICR7dGhpcy5jc3NDbGFzc31gO1xuICAgIHJldHVybiBjbHo7XG4gIH1cblxuICBjb25zdHJ1Y3RvcihwdWJsaWMgZWxlbWVudDogRWxlbWVudFJlZiwgcHJpdmF0ZSByZW5kZXJlcjogUmVuZGVyZXIyKSB7fVxuXG4gIG5nQWZ0ZXJWaWV3SW5pdCgpOiB2b2lkIHtcbiAgICBzZXRUaW1lb3V0KHRoaXMucG9zaXRpb24uYmluZCh0aGlzKSk7XG4gIH1cblxuICBwb3NpdGlvbigpOiB2b2lkIHtcbiAgICBjb25zdCBuYXRpdmVFbG0gPSB0aGlzLmVsZW1lbnQubmF0aXZlRWxlbWVudDtcbiAgICBjb25zdCBob3N0RGltID0gdGhpcy5ob3N0Lm5hdGl2ZUVsZW1lbnQuZ2V0Qm91bmRpbmdDbGllbnRSZWN0KCk7XG5cbiAgICAvLyBpZiBubyBkaW1zIHdlcmUgZm91bmQsIG5ldmVyIHNob3dcbiAgICBpZiAoIWhvc3REaW0uaGVpZ2h0ICYmICFob3N0RGltLndpZHRoKSByZXR1cm47XG5cbiAgICBjb25zdCBlbG1EaW0gPSBuYXRpdmVFbG0uZ2V0Qm91bmRpbmdDbGllbnRSZWN0KCk7XG4gICAgdGhpcy5jaGVja0ZsaXAoaG9zdERpbSwgZWxtRGltKTtcbiAgICB0aGlzLnBvc2l0aW9uQ29udGVudChuYXRpdmVFbG0sIGhvc3REaW0sIGVsbURpbSk7XG5cbiAgICBpZiAodGhpcy5zaG93Q2FyZXQpIHtcbiAgICAgIHRoaXMucG9zaXRpb25DYXJldChob3N0RGltLCBlbG1EaW0pO1xuICAgIH1cblxuICAgIC8vIGFuaW1hdGUgaXRzIGVudHJ5XG4gICAgc2V0VGltZW91dCgoKSA9PiB0aGlzLnJlbmRlcmVyLmFkZENsYXNzKG5hdGl2ZUVsbSwgJ2FuaW1hdGUnKSwgMSk7XG4gIH1cblxuICBwb3NpdGlvbkNvbnRlbnQobmF0aXZlRWxtLCBob3N0RGltLCBlbG1EaW0pOiB2b2lkIHtcbiAgICBjb25zdCB7IHRvcCwgbGVmdCB9ID0gUG9zaXRpb25IZWxwZXIucG9zaXRpb25Db250ZW50KHRoaXMucGxhY2VtZW50LCBlbG1EaW0sIGhvc3REaW0sIHRoaXMuc3BhY2luZywgdGhpcy5hbGlnbm1lbnQpO1xuXG4gICAgdGhpcy5yZW5kZXJlci5zZXRTdHlsZShuYXRpdmVFbG0sICd0b3AnLCBgJHt0b3B9cHhgKTtcbiAgICB0aGlzLnJlbmRlcmVyLnNldFN0eWxlKG5hdGl2ZUVsbSwgJ2xlZnQnLCBgJHtsZWZ0fXB4YCk7XG4gIH1cblxuICBwb3NpdGlvbkNhcmV0KGhvc3REaW0sIGVsbURpbSk6IHZvaWQge1xuICAgIGNvbnN0IGNhcmV0RWxtID0gdGhpcy5jYXJldEVsbS5uYXRpdmVFbGVtZW50O1xuICAgIGNvbnN0IGNhcmV0RGltZW5zaW9ucyA9IGNhcmV0RWxtLmdldEJvdW5kaW5nQ2xpZW50UmVjdCgpO1xuICAgIGNvbnN0IHsgdG9wLCBsZWZ0IH0gPSBQb3NpdGlvbkhlbHBlci5wb3NpdGlvbkNhcmV0KFxuICAgICAgdGhpcy5wbGFjZW1lbnQsXG4gICAgICBlbG1EaW0sXG4gICAgICBob3N0RGltLFxuICAgICAgY2FyZXREaW1lbnNpb25zLFxuICAgICAgdGhpcy5hbGlnbm1lbnRcbiAgICApO1xuXG4gICAgdGhpcy5yZW5kZXJlci5zZXRTdHlsZShjYXJldEVsbSwgJ3RvcCcsIGAke3RvcH1weGApO1xuICAgIHRoaXMucmVuZGVyZXIuc2V0U3R5bGUoY2FyZXRFbG0sICdsZWZ0JywgYCR7bGVmdH1weGApO1xuICB9XG5cbiAgY2hlY2tGbGlwKGhvc3REaW0sIGVsbURpbSk6IHZvaWQge1xuICAgIHRoaXMucGxhY2VtZW50ID0gUG9zaXRpb25IZWxwZXIuZGV0ZXJtaW5lUGxhY2VtZW50KHRoaXMucGxhY2VtZW50LCBlbG1EaW0sIGhvc3REaW0sIHRoaXMuc3BhY2luZyk7XG4gIH1cblxuICBASG9zdExpc3RlbmVyKCd3aW5kb3c6cmVzaXplJylcbiAgQHRocm90dGxlYWJsZSgxMDApXG4gIG9uV2luZG93UmVzaXplKCk6IHZvaWQge1xuICAgIHRoaXMucG9zaXRpb24oKTtcbiAgfVxufVxuIl19