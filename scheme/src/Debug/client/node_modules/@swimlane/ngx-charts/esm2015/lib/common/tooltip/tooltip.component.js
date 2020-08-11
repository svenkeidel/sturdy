import { __decorate } from "tslib";
import { Input, Component, ElementRef, AfterViewInit, ViewEncapsulation, HostListener, ViewChild, HostBinding, Renderer2 } from '@angular/core';
import { throttleable } from '../../utils/throttle';
import { PositionHelper } from './position';
let TooltipContentComponent = class TooltipContentComponent {
    constructor(element, renderer) {
        this.element = element;
        this.renderer = renderer;
    }
    get cssClasses() {
        let clz = 'ngx-charts-tooltip-content';
        clz += ` position-${this.placement}`;
        clz += ` type-${this.type}`;
        clz += ` ${this.cssClass}`;
        return clz;
    }
    ngAfterViewInit() {
        setTimeout(this.position.bind(this));
    }
    position() {
        const nativeElm = this.element.nativeElement;
        const hostDim = this.host.nativeElement.getBoundingClientRect();
        // if no dims were found, never show
        if (!hostDim.height && !hostDim.width)
            return;
        const elmDim = nativeElm.getBoundingClientRect();
        this.checkFlip(hostDim, elmDim);
        this.positionContent(nativeElm, hostDim, elmDim);
        if (this.showCaret) {
            this.positionCaret(hostDim, elmDim);
        }
        // animate its entry
        setTimeout(() => this.renderer.addClass(nativeElm, 'animate'), 1);
    }
    positionContent(nativeElm, hostDim, elmDim) {
        const { top, left } = PositionHelper.positionContent(this.placement, elmDim, hostDim, this.spacing, this.alignment);
        this.renderer.setStyle(nativeElm, 'top', `${top}px`);
        this.renderer.setStyle(nativeElm, 'left', `${left}px`);
    }
    positionCaret(hostDim, elmDim) {
        const caretElm = this.caretElm.nativeElement;
        const caretDimensions = caretElm.getBoundingClientRect();
        const { top, left } = PositionHelper.positionCaret(this.placement, elmDim, hostDim, caretDimensions, this.alignment);
        this.renderer.setStyle(caretElm, 'top', `${top}px`);
        this.renderer.setStyle(caretElm, 'left', `${left}px`);
    }
    checkFlip(hostDim, elmDim) {
        this.placement = PositionHelper.determinePlacement(this.placement, elmDim, hostDim, this.spacing);
    }
    onWindowResize() {
        this.position();
    }
};
TooltipContentComponent.ctorParameters = () => [
    { type: ElementRef },
    { type: Renderer2 }
];
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
        template: `
    <div>
      <span #caretElm [hidden]="!showCaret" class="tooltip-caret position-{{ this.placement }}"> </span>
      <div class="tooltip-content">
        <span *ngIf="!title">
          <ng-template [ngTemplateOutlet]="template" [ngTemplateOutletContext]="{ model: context }"> </ng-template>
        </span>
        <span *ngIf="title" [innerHTML]="title"> </span>
      </div>
    </div>
  `,
        encapsulation: ViewEncapsulation.None,
        styles: [".ngx-charts-tooltip-content{position:fixed;border-radius:3px;z-index:5000;display:block;font-weight:400;opacity:0;pointer-events:none!important}.ngx-charts-tooltip-content.type-popover{background:#fff;color:#060709;border:1px solid #72809b;box-shadow:0 1px 3px 0 rgba(0,0,0,.2),0 1px 1px 0 rgba(0,0,0,.14),0 2px 1px -1px rgba(0,0,0,.12);font-size:13px;padding:4px}.ngx-charts-tooltip-content.type-popover .tooltip-caret{position:absolute;z-index:5001;width:0;height:0}.ngx-charts-tooltip-content.type-popover .tooltip-caret.position-left{border-top:7px solid transparent;border-bottom:7px solid transparent;border-left:7px solid #fff}.ngx-charts-tooltip-content.type-popover .tooltip-caret.position-top{border-left:7px solid transparent;border-right:7px solid transparent;border-top:7px solid #fff}.ngx-charts-tooltip-content.type-popover .tooltip-caret.position-right{border-top:7px solid transparent;border-bottom:7px solid transparent;border-right:7px solid #fff}.ngx-charts-tooltip-content.type-popover .tooltip-caret.position-bottom{border-left:7px solid transparent;border-right:7px solid transparent;border-bottom:7px solid #fff}.ngx-charts-tooltip-content.type-tooltip{color:#fff;background:rgba(0,0,0,.75);font-size:12px;padding:0 10px;text-align:center;pointer-events:auto}.ngx-charts-tooltip-content.type-tooltip .tooltip-caret.position-left{border-top:7px solid transparent;border-bottom:7px solid transparent;border-left:7px solid rgba(0,0,0,.75)}.ngx-charts-tooltip-content.type-tooltip .tooltip-caret.position-top{border-left:7px solid transparent;border-right:7px solid transparent;border-top:7px solid rgba(0,0,0,.75)}.ngx-charts-tooltip-content.type-tooltip .tooltip-caret.position-right{border-top:7px solid transparent;border-bottom:7px solid transparent;border-right:7px solid rgba(0,0,0,.75)}.ngx-charts-tooltip-content.type-tooltip .tooltip-caret.position-bottom{border-left:7px solid transparent;border-right:7px solid transparent;border-bottom:7px solid rgba(0,0,0,.75)}.ngx-charts-tooltip-content .tooltip-label{display:block;line-height:1em;padding:8px 5px 5px;font-size:1em}.ngx-charts-tooltip-content .tooltip-val{display:block;font-size:1.3em;line-height:1em;padding:0 5px 8px}.ngx-charts-tooltip-content .tooltip-caret{position:absolute;z-index:5001;width:0;height:0}.ngx-charts-tooltip-content.position-right{-webkit-transform:translate3d(10px,0,0);transform:translate3d(10px,0,0)}.ngx-charts-tooltip-content.position-left{-webkit-transform:translate3d(-10px,0,0);transform:translate3d(-10px,0,0)}.ngx-charts-tooltip-content.position-top{-webkit-transform:translate3d(0,-10px,0);transform:translate3d(0,-10px,0)}.ngx-charts-tooltip-content.position-bottom{-webkit-transform:translate3d(0,10px,0);transform:translate3d(0,10px,0)}.ngx-charts-tooltip-content.animate{opacity:1;-webkit-transition:opacity .3s,-webkit-transform .3s;transition:opacity .3s,transform .3s,-webkit-transform .3s;-webkit-transform:translate3d(0,0,0);transform:translate3d(0,0,0);pointer-events:auto}.area-tooltip-container{padding:5px 0;pointer-events:none}.tooltip-item{text-align:left;line-height:1.2em;padding:5px 0}.tooltip-item .tooltip-item-color{display:inline-block;height:12px;width:12px;margin-right:5px;color:#5b646b;border-radius:3px}"]
    })
], TooltipContentComponent);
export { TooltipContentComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidG9vbHRpcC5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vdG9vbHRpcC90b29sdGlwLmNvbXBvbmVudC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUNMLEtBQUssRUFDTCxTQUFTLEVBQ1QsVUFBVSxFQUNWLGFBQWEsRUFDYixpQkFBaUIsRUFDakIsWUFBWSxFQUNaLFNBQVMsRUFDVCxXQUFXLEVBQ1gsU0FBUyxFQUNWLE1BQU0sZUFBZSxDQUFDO0FBRXZCLE9BQU8sRUFBRSxZQUFZLEVBQUUsTUFBTSxzQkFBc0IsQ0FBQztBQUNwRCxPQUFPLEVBQUUsY0FBYyxFQUFrQixNQUFNLFlBQVksQ0FBQztBQXFCNUQsSUFBYSx1QkFBdUIsR0FBcEMsTUFBYSx1QkFBdUI7SUF1QmxDLFlBQW1CLE9BQW1CLEVBQVUsUUFBbUI7UUFBaEQsWUFBTyxHQUFQLE9BQU8sQ0FBWTtRQUFVLGFBQVEsR0FBUixRQUFRLENBQVc7SUFBRyxDQUFDO0lBUnZFLElBQUksVUFBVTtRQUNaLElBQUksR0FBRyxHQUFHLDRCQUE0QixDQUFDO1FBQ3ZDLEdBQUcsSUFBSSxhQUFhLElBQUksQ0FBQyxTQUFTLEVBQUUsQ0FBQztRQUNyQyxHQUFHLElBQUksU0FBUyxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUM7UUFDNUIsR0FBRyxJQUFJLElBQUksSUFBSSxDQUFDLFFBQVEsRUFBRSxDQUFDO1FBQzNCLE9BQU8sR0FBRyxDQUFDO0lBQ2IsQ0FBQztJQUlELGVBQWU7UUFDYixVQUFVLENBQUMsSUFBSSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQztJQUN2QyxDQUFDO0lBRUQsUUFBUTtRQUNOLE1BQU0sU0FBUyxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUMsYUFBYSxDQUFDO1FBQzdDLE1BQU0sT0FBTyxHQUFHLElBQUksQ0FBQyxJQUFJLENB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dEIsU0FBUyxDQUFDLFVBQVUsQ0FBQzt5REFBVTtBQUdoQztJQURDLFdBQVcsQ0FBQyxPQUFPLENBQUM7eURBT3BCO0FBdUREO0lBRkMsWUFBWSxDQUFDLGVBQWUsQ0FBQztJQUM3QixZQUFZLENBQUMsR0FBRyxDQUFDOzZEQUdqQjtBQTlFVSx1QkFBdUI7SUFoQm5DLFNBQVMsQ0FBQztRQUNULFFBQVEsRUFBRSxxQkFBcUI7UUFDL0IsUUFBUSxFQUFFOzs7Ozs7Ozs7O0dBVVQ7UUFDRCxhQUFhLEVBQUUsaUJBQWlCLENBQUMsSUFBSTs7S0FFdEMsQ0FBQztHQUNXLHVCQUF1QixDQStFbkM7U0EvRVksdUJBQXVCIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHtcbiAgSW5wdXQsXG4gIENvbXBvbmVudCxcbiAgRWxlbWVudFJlZixcbiAgQWZ0ZXJWaWV3SW5pdCxcbiAgVmlld0VuY2Fwc3VsYXRpb24sXG4gIEhvc3RMaXN0ZW5lcixcbiAgVmlld0NoaWxkLFxuICBIb3N0QmluZGluZyxcbiAgUmVuZGVyZXIyXG59IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuXG5pbXBvcnQgeyB0aHJvdHRsZWFibGUgfSBmcm9tICcuLi8uLi91dGlscy90aHJvdHRsZSc7XG5pbXBvcnQgeyBQb3NpdGlvbkhlbHBlciwgUGxhY2VtZW50VHlwZXMgfSBmcm9tICcuL3Bvc2l0aW9uJztcblxuaW1wb3J0IHsgU3R5bGVUeXBlcyB9IGZyb20gJy4vc3R5bGUudHlwZSc7XG5pbXBvcnQgeyBBbGlnbm1lbnRUeXBlcyB9IGZyb20gJy4vYWxpZ25tZW50LnR5cGUnO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICduZ3gtdG9vbHRpcC1jb250ZW50JyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8ZGl2PlxuICAgICAgPHNwYW4gI2NhcmV0RWxtIFtoaWRkZW5dPVwiIXNob3dDYXJldFwiIGNsYXNzPVwidG9vbHRpcC1jYXJldCBwb3NpdGlvbi17eyB0aGlzLnBsYWNlbWVudCB9fVwiPiA8L3NwYW4+XG4gICAgICA8ZGl2IGNsYXNzPVwidG9vbHRpcC1jb250ZW50XCI+XG4gICAgICAgIDxzcGFuICpuZ0lmPVwiIXRpdGxlXCI+XG4gICAgICAgICAgPG5nLXRlbXBsYXRlIFtuZ1RlbXBsYXRlT3V0bGV0XT1cInRlbXBsYXRlXCIgW25nVGVtcGxhdGVPdXRsZXRDb250ZXh0XT1cInsgbW9kZWw6IGNvbnRleHQgfVwiPiA8L25nLXRlbXBsYXRlPlxuICAgICAgICA8L3NwYW4+XG4gICAgICAgIDxzcGFuICpuZ0lmPVwidGl0bGVcIiBbaW5uZXJIVE1MXT1cInRpdGxlXCI+IDwvc3Bhbj5cbiAgICAgIDwvZGl2PlxuICAgIDwvZGl2PlxuICBgLFxuICBlbmNhcHN1bGF0aW9uOiBWaWV3RW5jYXBzdWxhdGlvbi5Ob25lLFxuICBzdHlsZVVybHM6IFsnLi90b29sdGlwLmNvbXBvbmVudC5zY3NzJ11cbn0pXG5leHBvcnQgY2xhc3MgVG9vbHRpcENvbnRlbnRDb21wb25lbnQgaW1wbGVtZW50cyBBZnRlclZpZXdJbml0IHtcbiAgQElucHV0KCkgaG9zdDogYW55O1xuICBASW5wdXQoKSBzaG93Q2FyZXQ6IGJvb2xlYW47XG4gIEBJbnB1dCgpIHR5cGU6IFN0eWxlVHlwZXM7XG4gIEBJbnB1dCgpIHBsYWNlbWVudDogUGxhY2VtZW50VHlwZXM7XG4gIEBJbnB1dCgpIGFsaWdubWVudDogQWxpZ25tZW50VHlwZXM7XG4gIEBJbnB1dCgpIHNwYWNpbmc6IG51bWJlcjtcbiAgQElucHV0KCkgY3NzQ2xhc3M6IHN0cmluZztcbiAgQElucHV0KCkgdGl0bGU6IHN0cmluZztcbiAgQElucHV0KCkgdGVtcGxhdGU6IGFueTtcbiAgQElucHV0KCkgY29udGV4dDogYW55O1xuXG4gIEBWaWV3Q2hpbGQoJ2NhcmV0RWxtJykgY2FyZXRFbG07XG5cbiAgQEhvc3RCaW5kaW5nKCdjbGFzcycpXG4gIGdldCBjc3NDbGFzc2VzKCk6IHN0cmluZyB7XG4gICAgbGV0IGNseiA9ICduZ3gtY2hhcnRzLXRvb2x0aXAtY29udGVudCc7XG4gICAgY2x6ICs9IGAgcG9zaXRpb24tJHt0aGlzLnBsYWNlbWVudH1gO1xuICAgIGNseiArPSBgIHR5cGUtJHt0aGlzLnR5cGV9YDtcbiAgICBjbHogKz0gYCAke3RoaXMuY3NzQ2xhc3N9YDtcbiAgICByZXR1cm4gY2x6O1xuICB9XG5cbiAgY29uc3RydWN0b3IocHVibGljIGVsZW1lbnQ6IEVsZW1lbnRSZWYsIHByaXZhdGUgcmVuZGVyZXI6IFJlbmRlcmVyMikge31cblxuICBuZ0FmdGVyVmlld0luaXQoKTogdm9pZCB7XG4gICAgc2V0VGltZW91dCh0aGlzLnBvc2l0aW9uLmJpbmQodGhpcykpO1xuICB9XG5cbiAgcG9zaXRpb24oKTogdm9pZCB7XG4gICAgY29uc3QgbmF0aXZlRWxtID0gdGhpcy5lbGVtZW50Lm5hdGl2ZUVsZW1lbnQ7XG4gICAgY29uc3QgaG9zdERpbSA9IHRoaXMuaG9zdC5uYXRpdmVFbGVtZW50LmdldEJvdW5kaW5nQ2xpZW50UmVjdCgpO1xuXG4gICAgLy8gaWYgbm8gZGltcyB3ZXJlIGZvdW5kLCBuZXZlciBzaG93XG4gICAgaWYgKCFob3N0RGltLmhlaWdodCAmJiAhaG9zdERpbS53aWR0aCkgcmV0dXJuO1xuXG4gICAgY29uc3QgZWxtRGltID0gbmF0aXZlRWxtLmdldEJvdW5kaW5nQ2xpZW50UmVjdCgpO1xuICAgIHRoaXMuY2hlY2tGbGlwKGhvc3REaW0sIGVsbURpbSk7XG4gICAgdGhpcy5wb3NpdGlvbkNvbnRlbnQobmF0aXZlRWxtLCBob3N0RGltLCBlbG1EaW0pO1xuXG4gICAgaWYgKHRoaXMuc2hvd0NhcmV0KSB7XG4gICAgICB0aGlzLnBvc2l0aW9uQ2FyZXQoaG9zdERpbSwgZWxtRGltKTtcbiAgICB9XG5cbiAgICAvLyBhbmltYXRlIGl0cyBlbnRyeVxuICAgIHNldFRpbWVvdXQoKCkgPT4gdGhpcy5yZW5kZXJlci5hZGRDbGFzcyhuYXRpdmVFbG0sICdhbmltYXRlJyksIDEpO1xuICB9XG5cbiAgcG9zaXRpb25Db250ZW50KG5hdGl2ZUVsbSwgaG9zdERpbSwgZWxtRGltKTogdm9pZCB7XG4gICAgY29uc3QgeyB0b3AsIGxlZnQgfSA9IFBvc2l0aW9uSGVscGVyLnBvc2l0aW9uQ29udGVudCh0aGlzLnBsYWNlbWVudCwgZWxtRGltLCBob3N0RGltLCB0aGlzLnNwYWNpbmcsIHRoaXMuYWxpZ25tZW50KTtcblxuICAgIHRoaXMucmVuZGVyZXIuc2V0U3R5bGUobmF0aXZlRWxtLCAndG9wJywgYCR7dG9wfXB4YCk7XG4gICAgdGhpcy5yZW5kZXJlci5zZXRTdHlsZShuYXRpdmVFbG0sICdsZWZ0JywgYCR7bGVmdH1weGApO1xuICB9XG5cbiAgcG9zaXRpb25DYXJldChob3N0RGltLCBlbG1EaW0pOiB2b2lkIHtcbiAgICBjb25zdCBjYXJldEVsbSA9IHRoaXMuY2FyZXRFbG0ubmF0aXZlRWxlbWVudDtcbiAgICBjb25zdCBjYXJldERpbWVuc2lvbnMgPSBjYXJldEVsbS5nZXRCb3VuZGluZ0NsaWVudFJlY3QoKTtcbiAgICBjb25zdCB7IHRvcCwgbGVmdCB9ID0gUG9zaXRpb25IZWxwZXIucG9zaXRpb25DYXJldChcbiAgICAgIHRoaXMucGxhY2VtZW50LFxuICAgICAgZWxtRGltLFxuICAgICAgaG9zdERpbSxcbiAgICAgIGNhcmV0RGltZW5zaW9ucyxcbiAgICAgIHRoaXMuYWxpZ25tZW50XG4gICAgKTtcblxuICAgIHRoaXMucmVuZGVyZXIuc2V0U3R5bGUoY2FyZXRFbG0sICd0b3AnLCBgJHt0b3B9cHhgKTtcbiAgICB0aGlzLnJlbmRlcmVyLnNldFN0eWxlKGNhcmV0RWxtLCAnbGVmdCcsIGAke2xlZnR9cHhgKTtcbiAgfVxuXG4gIGNoZWNrRmxpcChob3N0RGltLCBlbG1EaW0pOiB2b2lkIHtcbiAgICB0aGlzLnBsYWNlbWVudCA9IFBvc2l0aW9uSGVscGVyLmRldGVybWluZVBsYWNlbWVudCh0aGlzLnBsYWNlbWVudCwgZWxtRGltLCBob3N0RGltLCB0aGlzLnNwYWNpbmcpO1xuICB9XG5cbiAgQEhvc3RMaXN0ZW5lcignd2luZG93OnJlc2l6ZScpXG4gIEB0aHJvdHRsZWFibGUoMTAwKVxuICBvbldpbmRvd1Jlc2l6ZSgpOiB2b2lkIHtcbiAgICB0aGlzLnBvc2l0aW9uKCk7XG4gIH1cbn1cbiJdfQ==