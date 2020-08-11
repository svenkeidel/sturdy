import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ElementRef, SimpleChanges, OnChanges, ViewChild, ChangeDetectionStrategy, ChangeDetectorRef, NgZone, OnDestroy } from '@angular/core';
import { trimLabel } from '../common/trim-label.helper';
import { roundedRect } from '../common/shape.helper';
import { escapeLabel } from '../common/label.helper';
import { decimalChecker, count } from '../common/count/count.helper';
var CardComponent = /** @class */ (function () {
    function CardComponent(element, cd, zone) {
        this.cd = cd;
        this.zone = zone;
        this.animations = true;
        this.select = new EventEmitter();
        this.value = '';
        this.textFontSize = 12;
        this.textTransform = '';
        this.initialized = false;
        this.bandHeight = 10;
        this.textPadding = [10, 20, 5, 20];
        this.labelFontSize = 15;
        this.element = element.nativeElement;
    }
    CardComponent.prototype.ngOnChanges = function (changes) {
        this.update();
    };
    CardComponent.prototype.ngOnDestroy = function () {
        cancelAnimationFrame(this.animationReq);
    };
    CardComponent.prototype.update = function () {
        var _this = this;
        this.zone.run(function () {
            var hasValue = _this.data && typeof _this.data.value !== 'undefined';
            var valueFormatting = _this.valueFormatting || (function (card) { return card.value.toLocaleString(); });
            var labelFormatting = _this.labelFormatting || (function (card) { return escapeLabel(trimLabel(card.label, 55)); });
            _this.transform = "translate(" + _this.x + " , " + _this.y + ")";
            _this.textWidth = Math.max(0, _this.width) - _this.textPadding[1] - _this.textPadding[3];
            _this.cardWidth = Math.max(0, _this.width);
            _this.cardHeight = Math.max(0, _this.height);
            _this.label = _this.label ? _this.label : _this.data.name;
            var cardData = {
                label: _this.label,
                data: _this.data,
                value: _this.data.value
            };
            _this.formattedLabel = labelFormatting(cardData);
            _this.transformBand = "translate(0 , " + (_this.cardHeight - _this.bandHeight) + ")";
            var value = hasValue ? valueFormatting(cardData) : '';
            _this.value = _this.paddedValue(value);
            _this.setPadding();
            _this.bandPath = roundedRect(0, 0, _this.cardWidth, _this.bandHeight, 3, [false, false, true, true]);
            setTimeout(function () {
                _this.scaleText();
                _this.value = value;
                if (hasValue && !_this.initialized) {
                    setTimeout(function () { return _this.startCount(); }, 20);
                }
            }, 8);
        });
    };
    CardComponent.prototype.paddedValue = function (value) {
        if (this.medianSize && this.medianSize > value.length) {
            value += '\u2007'.repeat(this.medianSize - value.length);
        }
        return value;
    };
    CardComponent.prototype.startCount = function () {
        var _this = this;
        if (!this.initialized && this.animations) {
            cancelAnimationFrame(this.animationReq);
            var val_1 = this.data.value;
            var decs = decimalChecker(val_1);
            var valueFormatting_1 = this.valueFormatting || (function (card) { return card.value.toLocaleString(); });
            var callback = function (_a) {
                var value = _a.value, finished = _a.finished;
                _this.zone.run(function () {
                    value = finished ? val_1 : value;
                    _this.value = valueFormatting_1({ label: _this.label, data: _this.data, value: value });
                    if (!finished) {
                        _this.value = _this.paddedValue(_this.value);
                    }
                    _this.cd.markForCheck();
                });
            };
            this.animationReq = count(0, val_1, decs, 1, callback);
            this.initialized = true;
        }
    };
    CardComponent.prototype.scaleText = function () {
        var _this = this;
        this.zone.run(function () {
            var _a = _this.textEl.nativeElement.getBoundingClientRect(), width = _a.width, height = _a.height;
            if (width === 0 || height === 0) {
                return;
            }
            var textPadding = (_this.textPadding[1] = _this.textPadding[3] = _this.cardWidth / 8);
            var availableWidth = _this.cardWidth - 2 * textPadding;
            var availableHeight = _this.cardHeight / 3;
            var resizeScale = Math.min(availableWidth / width, availableHeight / height);
            _this.textFontSize = Math.floor(_this.textFontSize * resizeScale);
            _this.labelFontSize = Math.min(_this.textFontSize, 15);
            _this.setPadding();
            _this.cd.markForCheck();
        });
    };
    CardComponent.prototype.setPadding = function () {
        this.textPadding[1] = this.textPadding[3] = this.cardWidth / 8;
        var padding = this.cardHeight / 2;
        this.textPadding[0] = padding - this.textFontSize - this.labelFontSize / 2;
        this.textPadding[2] = padding - this.labelFontSize;
    };
    CardComponent.prototype.onClick = function () {
        this.select.emit(this.data);
    };
    CardComponent.ctorParameters = function () { return [
        { type: ElementRef },
        { type: ChangeDetectorRef },
        { type: NgZone }
    ]; };
    __decorate([
        Input()
    ], CardComponent.prototype, "color", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "bandColor", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "textColor", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "x", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "y", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "width", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "height", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "label", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "data", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "medianSize", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "valueFormatting", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "labelFormatting", void 0);
    __decorate([
        Input()
    ], CardComponent.prototype, "animations", void 0);
    __decorate([
        Output()
    ], CardComponent.prototype, "select", void 0);
    __decorate([
        ViewChild('textEl', { static: false })
    ], CardComponent.prototype, "textEl", void 0);
    CardComponent = __decorate([
        Component({
            selector: 'g[ngx-charts-card]',
            template: "\n    <svg:g [attr.transform]=\"transform\" class=\"cell\" (click)=\"onClick()\">\n      <svg:rect class=\"card\" [style.fill]=\"color\" [attr.width]=\"cardWidth\" [attr.height]=\"cardHeight\" rx=\"3\" ry=\"3\" />\n      <svg:path\n        *ngIf=\"bandColor && bandColor !== color\"\n        class=\"card-band\"\n        [attr.fill]=\"bandColor\"\n        [attr.transform]=\"transformBand\"\n        stroke=\"none\"\n        [attr.d]=\"bandPath\"\n      />\n      <title>{{ label }}</title>\n      <svg:foreignObject\n        class=\"trimmed-label\"\n        x=\"5\"\n        [attr.x]=\"textPadding[3]\"\n        [attr.y]=\"cardHeight - textPadding[2]\"\n        [attr.width]=\"textWidth\"\n        [attr.height]=\"labelFontSize + textPadding[2]\"\n        alignment-baseline=\"hanging\"\n      >\n        <xhtml:p\n          [style.color]=\"textColor\"\n          [style.fontSize.px]=\"labelFontSize\"\n          [style.lineHeight.px]=\"labelFontSize\"\n          [innerHTML]=\"formattedLabel\"\n        >\n        </xhtml:p>\n      </svg:foreignObject>\n      <svg:text\n        #textEl\n        class=\"value-text\"\n        [attr.x]=\"textPadding[3]\"\n        [attr.y]=\"textPadding[0]\"\n        [style.fill]=\"textColor\"\n        text-anchor=\"start\"\n        alignment-baseline=\"hanging\"\n        [style.font-size.pt]=\"textFontSize\"\n      >\n        {{ value }}\n      </svg:text>\n    </svg:g>\n  ",
            changeDetection: ChangeDetectionStrategy.OnPush
        })
    ], CardComponent);
    return CardComponent;
}());
export { CardComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2FyZC5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9udW1iZXItY2FyZC9jYXJkLmNvbXBvbmVudC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUNMLFNBQVMsRUFDVCxLQUFLLEVBQ0wsTUFBTSxFQUNOLFlBQVksRUFDWixVQUFVLEVBQ1YsYUFBYSxFQUNiLFNBQVMsRUFDVCxTQUFTLEVBQ1QsdUJBQXVCLEVBQ3ZCLGlCQUFpQixFQUNqQixNQUFNLEVBQ04sU0FBUyxFQUNWLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBRSxTQUFTLEVBQUUsTUFBTSw2QkFBNkIsQ0FBQztBQUN4RCxPQUFPLEVBQUUsV0FBVyxFQUFFLE1BQU0sd0JBQXdCLENBQUM7QUFDckQsT0FBTyxFQUFFLFdBQVcsRUFBRSxNQUFNLHdCQUF3QixDQUFDO0FBQ3JELE9BQU8sRUFBRSxjQUFjLEVBQUUsS0FBSyxFQUFFLE1BQU0sOEJBQThCLENBQUM7QUFpRHJFO0lBdUNFLHVCQUFZLE9BQW1CLEVBQVUsRUFBcUIsRUFBVSxJQUFZO1FBQTNDLE9BQUUsR0FBRixFQUFFLENBQW1CO1FBQVUsU0FBSSxHQUFKLElBQUksQ0FBUTtRQXpCM0UsZUFBVSxHQUFZLElBQUksQ0FBQztRQUUxQixXQUFNLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUt0QyxVQUFLLEdBQVcsRUFBRSxDQUFDO1FBTW5CLGlCQUFZLEdBQVcsRUFBRSxDQUFDO1FBQzFCLGtCQUFhLEdBQVcsRUFBRSxDQUFDO1FBQzNCLGdCQUFXLEdBQVksS0FBSyxDQUFDO1FBRzdCLGVBQVUsR0FBVyxFQUFFLENBQUM7UUFFeEIsZ0JBQVcsR0FBRyxDQUFDLEVBQUUsRUFBRSxFQUFFLEVBQUUsQ0FBQyxFQUFFLEVBQUUsQ0FBQyxDQUFDO1FBQzlCLGtCQUFhLEdBQUcsRUFBRSxDQUFDO1FBS2pCLElBQUksQ0FBQyxPQUFP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Q0Y7UUFBUixLQUFLLEVBQUU7NENBQUc7SUFDRjtRQUFSLEtBQUssRUFBRTtnREFBTztJQUNOO1FBQVIsS0FBSyxFQUFFO2lEQUFRO0lBQ1A7UUFBUixLQUFLLEVBQUU7Z0RBQU87SUFDTjtRQUFSLEtBQUssRUFBRTsrQ0FBTTtJQUNMO1FBQVIsS0FBSyxFQUFFO3FEQUFvQjtJQUNuQjtRQUFSLEtBQUssRUFBRTswREFBc0I7SUFDckI7UUFBUixLQUFLLEVBQUU7MERBQXNCO0lBQ3JCO1FBQVIsS0FBSyxFQUFFO3FEQUE0QjtJQUUxQjtRQUFULE1BQU0sRUFBRTtpREFBNkI7SUFFRTtRQUF2QyxTQUFTLENBQUMsUUFBUSxFQUFFLEVBQUUsTUFBTSxFQUFFLEtBQUssRUFBRSxDQUFDO2lEQUFvQjtJQWxCaEQsYUFBYTtRQS9DekIsU0FBUyxDQUFDO1lBQ1QsUUFBUSxFQUFFLG9CQUFvQjtZQUM5QixRQUFRLEVBQUUsdTRDQTBDVDtZQUNELGVBQWUsRUFBRSx1QkFBdUIsQ0FBQyxNQUFNO1NBQ2hELENBQUM7T0FDVyxhQUFhLENBd0p6QjtJQUFELG9CQUFDO0NBQUEsQUF4SkQsSUF3SkM7U0F4SlksYUFBYSIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIENvbXBvbmVudCxcbiAgSW5wdXQsXG4gIE91dHB1dCxcbiAgRXZlbnRFbWl0dGVyLFxuICBFbGVtZW50UmVmLFxuICBTaW1wbGVDaGFuZ2VzLFxuICBPbkNoYW5nZXMsXG4gIFZpZXdDaGlsZCxcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksXG4gIENoYW5nZURldGVjdG9yUmVmLFxuICBOZ1pvbmUsXG4gIE9uRGVzdHJveVxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IHRyaW1MYWJlbCB9IGZyb20gJy4uL2NvbW1vbi90cmltLWxhYmVsLmhlbHBlcic7XG5pbXBvcnQgeyByb3VuZGVkUmVjdCB9IGZyb20gJy4uL2NvbW1vbi9zaGFwZS5oZWxwZXInO1xuaW1wb3J0IHsgZXNjYXBlTGFiZWwgfSBmcm9tICcuLi9jb21tb24vbGFiZWwuaGVscGVyJztcbmltcG9ydCB7IGRlY2ltYWxDaGVja2VyLCBjb3VudCB9IGZyb20gJy4uL2NvbW1vbi9jb3VudC9jb3VudC5oZWxwZXInO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdnW25neC1jaGFydHMtY2FyZF0nLFxuICB0ZW1wbGF0ZTogYFxuICAgIDxzdmc6ZyBbYXR0ci50cmFuc2Zvcm1dPVwidHJhbnNmb3JtXCIgY2xhc3M9XCJjZWxsXCIgKGNsaWNrKT1cIm9uQ2xpY2soKVwiPlxuICAgICAgPHN2ZzpyZWN0IGNsYXNzPVwiY2FyZFwiIFtzdHlsZS5maWxsXT1cImNvbG9yXCIgW2F0dHIud2lkdGhdPVwiY2FyZFdpZHRoXCIgW2F0dHIuaGVpZ2h0XT1cImNhcmRIZWlnaHRcIiByeD1cIjNcIiByeT1cIjNcIiAvPlxuICAgICAgPHN2ZzpwYXRoXG4gICAgICAgICpuZ0lmPVwiYmFuZENvbG9yICYmIGJhbmRDb2xvciAhPT0gY29sb3JcIlxuICAgICAgICBjbGFzcz1cImNhcmQtYmFuZFwiXG4gICAgICAgIFthdHRyLmZpbGxdPVwiYmFuZENvbG9yXCJcbiAgICAgICAgW2F0dHIudHJhbnNmb3JtXT1cInRyYW5zZm9ybUJhbmRcIlxuICAgICAgICBzdHJva2U9XCJub25lXCJcbiAgICAgICAgW2F0dHIuZF09XCJiYW5kUGF0aFwiXG4gICAgICAvPlxuICAgICAgPHRpdGxlPnt7IGxhYmVsIH19PC90aXRsZT5cbiAgICAgIDxzdmc6Zm9yZWlnbk9iamVjdFxuICAgICAgICBjbGFzcz1cInRyaW1tZWQtbGFiZWxcIlxuICAgICAgICB4PVwiNVwiXG4gICAgICAgIFthdHRyLnhdPVwidGV4dFBhZGRpbmdbM11cIlxuICAgICAgICBbYXR0ci55XT1cImNhcmRIZWlnaHQgLSB0ZXh0UGFkZGluZ1syXVwiXG4gICAgICAgIFthdHRyLndpZHRoXT1cInRleHRXaWR0aFwiXG4gICAgICAgIFthdHRyLmhlaWdodF09XCJsYWJlbEZvbnRTaXplICsgdGV4dFBhZGRpbmdbMl1cIlxuICAgICAgICBhbGlnbm1lbnQtYmFzZWxpbmU9XCJoYW5naW5nXCJcbiAgICAgID5cbiAgICAgICAgPHhodG1sOnBcbiAgICAgICAgICBbc3R5bGUuY29sb3JdPVwidGV4dENvbG9yXCJcbiAgICAgICAgICBbc3R5bGUuZm9udFNpemUucHhdPVwibGFiZWxGb250U2l6ZVwiXG4gICAgICAgICAgW3N0eWxlLmxpbmVIZWlnaHQucHhdPVwibGFiZWxGb250U2l6ZVwiXG4gICAgICAgICAgW2lubmVySFRNTF09XCJmb3JtYXR0ZWRMYWJlbFwiXG4gICAgICAgID5cbiAgICAgICAgPC94aHRtbDpwPlxuICAgICAgPC9zdmc6Zm9yZWlnbk9iamVjdD5cbiAgICAgIDxzdmc6dGV4dFxuICAgICAgICAjdGV4dEVsXG4gICAgICAgIGNsYXNzPVwidmFsdWUtdGV4dFwiXG4gICAgICAgIFthdHRyLnhdPVwidGV4dFBhZGRpbmdbM11cIlxuICAgICAgICBbYXR0ci55XT1cInRleHRQYWRkaW5nWzBdXCJcbiAgICAgICAgW3N0eWxlLmZpbGxdPVwidGV4dENvbG9yXCJcbiAgICAgICAgdGV4dC1hbmNob3I9XCJzdGFydFwiXG4gICAgICAgIGFsaWdubWVudC1iYXNlbGluZT1cImhhbmdpbmdcIlxuICAgICAgICBbc3R5bGUuZm9udC1zaXplLnB0XT1cInRleHRGb250U2l6ZVwiXG4gICAgICA+XG4gICAgICAgIHt7IHZhbHVlIH19XG4gICAgICA8L3N2Zzp0ZXh0PlxuICAgIDwvc3ZnOmc+XG4gIGAsXG4gIGNoYW5nZURldGVjdGlvbjogQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3kuT25QdXNoXG59KVxuZXhwb3J0IGNsYXNzIENhcmRDb21wb25lbnQgaW1wbGVtZW50cyBPbkNoYW5nZXMsIE9uRGVzdHJveSB7XG4gIEBJbnB1dCgpIGNvbG9yO1xuICBASW5wdXQoKSBiYW5kQ29sb3I7XG4gIEBJbnB1dCgpIHRleHRDb2xvcjtcblxuICBASW5wdXQoKSB4O1xuICBASW5wdXQoKSB5O1xuICBASW5wdXQoKSB3aWR0aDtcbiAgQElucHV0KCkgaGVpZ2h0O1xuICBASW5wdXQoKSBsYWJlbDtcbiAgQElucHV0KCkgZGF0YTtcbiAgQElucHV0KCkgbWVkaWFuU2l6ZTogbnVtYmVyO1xuICBASW5wdXQoKSB2YWx1ZUZvcm1hdHRpbmc6IGFueTtcbiAgQElucHV0KCkgbGFiZWxGb3JtYXR0aW5nOiBhbnk7XG4gIEBJbnB1dCgpIGFuaW1hdGlvbnM6IGJvb2xlYW4gPSB0cnVlO1xuXG4gIEBPdXRwdXQoKSBzZWxlY3QgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgQFZpZXdDaGlsZCgndGV4dEVsJywgeyBzdGF0aWM6IGZhbHNlIH0pIHRleHRFbDogRWxlbWVudFJlZjtcblxuICBlbGVtZW50OiBIVE1MRWxlbWVudDtcbiAgdmFsdWU6IHN0cmluZyA9ICcnO1xuICB0cmFuc2Zvcm06IHN0cmluZztcbiAgZm9ybWF0dGVkTGFiZWw6IHN0cmluZztcbiAgY2FyZFdpZHRoOiBudW1iZXI7XG4gIGNhcmRIZWlnaHQ6IG51bWJlcjtcbiAgdGV4dFdpZHRoOiBudW1iZXI7XG4gIHRleHRGb250U2l6ZTogbnVtYmVyID0gMTI7XG4gIHRleHRUcmFuc2Zvcm06IHN0cmluZyA9ICcnO1xuICBpbml0aWFsaXplZDogYm9vbGVhbiA9IGZhbHNlO1xuICBhbmltYXRpb25SZXE6IGFueTtcblxuICBiYW5kSGVpZ2h0OiBudW1iZXIgPSAxMDtcbiAgdHJhbnNmb3JtQmFuZDogc3RyaW5nO1xuICB0ZXh0UGFkZGluZyA9IFsxMCwgMjAsIDUsIDIwXTtcbiAgbGFiZWxGb250U2l6ZSA9IDE1O1xuXG4gIGJhbmRQYXRoOiBzdHJpbmc7XG5cbiAgY29uc3RydWN0b3IoZWxlbWVudDogRWxlbWVudFJlZiwgcHJpdmF0ZSBjZDogQ2hhbmdlRGV0ZWN0b3JSZWYsIHByaXZhdGUgem9uZTogTmdab25lKSB7XG4gICAgdGhpcy5lbGVtZW50ID0gZWxlbWVudC5uYXRpdmVFbGVtZW50O1xuICB9XG5cbiAgbmdPbkNoYW5nZXMoY2hhbmdlczogU2ltcGxlQ2hhbmdlcyk6IHZvaWQge1xuICAgIHRoaXMudXBkYXRlKCk7XG4gIH1cblxuICBuZ09uRGVzdHJveSgpOiB2b2lkIHtcbiAgICBjYW5jZWxBbmltYXRpb25GcmFtZSh0aGlzLmFuaW1hdGlvblJlcSk7XG4gIH1cblxuICB1cGRhdGUoKTogdm9pZCB7XG4gICAgdGhpcy56b25lLnJ1bigoKSA9PiB7XG4gICAgICBjb25zdCBoYXNWYWx1ZSA9IHRoaXMuZGF0YSAmJiB0eXBlb2YgdGhpcy5kYXRhLnZhbHVlICE9PSAndW5kZWZpbmVkJztcbiAgICAgIGNvbnN0IHZhbHVlRm9ybWF0dGluZyA9IHRoaXMudmFsdWVGb3JtYXR0aW5nIHx8IChjYXJkID0+IGNhcmQudmFsdWUudG9Mb2NhbGVTdHJpbmcoKSk7XG4gICAgICBjb25zdCBsYWJlbEZvcm1hdHRpbmcgPSB0aGlzLmxhYmVsRm9ybWF0dGluZyB8fCAoY2FyZCA9PiBlc2NhcGVMYWJlbCh0cmltTGFiZWwoY2FyZC5sYWJlbCwgNTUpKSk7XG5cbiAgICAgIHRoaXMudHJhbnNmb3JtID0gYHRyYW5zbGF0ZSgke3RoaXMueH0gLCAke3RoaXMueX0pYDtcblxuICAgICAgdGhpcy50ZXh0V2lkdGggPSBNYXRoLm1heCgwLCB0aGlzLndpZHRoKSAtIHRoaXMudGV4dFBhZGRpbmdbMV0gLSB0aGlzLnRleHRQYWRkaW5nWzNdO1xuICAgICAgdGhpcy5jYXJkV2lkdGggPSBNYXRoLm1heCgwLCB0aGlzLndpZHRoKTtcbiAgICAgIHRoaXMuY2FyZEhlaWdodCA9IE1hdGgubWF4KDAsIHRoaXMuaGVpZ2h0KTtcblxuICAgICAgdGhpcy5sYWJlbCA9IHRoaXMubGFiZWwgPyB0aGlzLmxhYmVsIDogdGhpcy5kYXRhLm5hbWU7XG5cbiAgICAgIGNvbnN0IGNhcmREYXRhID0ge1xuICAgICAgICBsYWJlbDogdGhpcy5sYWJlbCxcbiAgICAgICAgZGF0YTogdGhpcy5kYXRhLFxuICAgICAgICB2YWx1ZTogdGhpcy5kYXRhLnZhbHVlXG4gICAgICB9O1xuXG4gICAgICB0aGlzLmZvcm1hdHRlZExhYmVsID0gbGFiZWxGb3JtYXR0aW5nKGNhcmREYXRhKTtcbiAgICAgIHRoaXMudHJhbnNmb3JtQmFuZCA9IGB0cmFuc2xhdGUoMCAsICR7dGhpcy5jYXJkSGVpZ2h0IC0gdGhpcy5iYW5kSGVpZ2h0fSlgO1xuXG4gICAgICBjb25zdCB2YWx1ZSA9IGhhc1ZhbHVlID8gdmFsdWVGb3JtYXR0aW5nKGNhcmREYXRhKSA6ICcnO1xuXG4gICAgICB0aGlzLnZhbHVlID0gdGhpcy5wYWRkZWRWYWx1ZSh2YWx1ZSk7XG4gICAgICB0aGlzLnNldFBhZGRpbmcoKTtcblxuICAgICAgdGhpcy5iYW5kUGF0aCA9IHJvdW5kZWRSZWN0KDAsIDAsIHRoaXMuY2FyZFdpZHRoLCB0aGlzLmJhbmRIZWlnaHQsIDMsIFtmYWxzZSwgZmFsc2UsIHRydWUsIHRydWVdKTtcblxuICAgICAgc2V0VGltZW91dCgoKSA9PiB7XG4gICAgICAgIHRoaXMuc2NhbGVUZXh0KCk7XG4gICAgICAgIHRoaXMudmFsdWUgPSB2YWx1ZTtcbiAgICAgICAgaWYgKGhhc1ZhbHVlICYmICF0aGlzLmluaXRpYWxpemVkKSB7XG4gICAgICAgICAgc2V0VGltZW91dCgoKSA9PiB0aGlzLnN0YXJ0Q291bnQoKSwgMjApO1xuICAgICAgICB9XG4gICAgICB9LCA4KTtcbiAgICB9KTtcbiAgfVxuXG4gIHBhZGRlZFZhbHVlKHZhbHVlOiBzdHJpbmcpIHtcbiAgICBpZiAodGhpcy5tZWRpYW5TaXplICYmIHRoaXMubWVkaWFuU2l6ZSA+IHZhbHVlLmxlbmd0aCkge1xuICAgICAgdmFsdWUgKz0gJ1xcdTIwMDcnLnJlcGVhdCh0aGlzLm1lZGlhblNpemUgLSB2YWx1ZS5sZW5ndGgpO1xuICAgIH1cbiAgICByZXR1cm4gdmFsdWU7XG4gIH1cblxuICBzdGFydENvdW50KCk6IHZvaWQge1xuICAgIGlmICghdGhpcy5pbml0aWFsaXplZCAmJiB0aGlzLmFuaW1hdGlvbnMpIHtcbiAgICAgIGNhbmNlbEFuaW1hdGlvbkZyYW1lKHRoaXMuYW5pbWF0aW9uUmVxKTtcblxuICAgICAgY29uc3QgdmFsID0gdGhpcy5kYXRhLnZhbHVlO1xuICAgICAgY29uc3QgZGVjcyA9IGRlY2ltYWxDaGVja2VyKHZhbCk7XG4gICAgICBjb25zdCB2YWx1ZUZvcm1hdHRpbmcgPSB0aGlzLnZhbHVlRm9ybWF0dGluZyB8fCAoY2FyZCA9PiBjYXJkLnZhbHVlLnRvTG9jYWxlU3RyaW5nKCkpO1xuXG4gICAgICBjb25zdCBjYWxsYmFjayA9ICh7IHZhbHVlLCBmaW5pc2hlZCB9KSA9PiB7XG4gICAgICAgIHRoaXMuem9uZS5ydW4oKCkgPT4ge1xuICAgICAgICAgIHZhbHVlID0gZmluaXNoZWQgPyB2YWwgOiB2YWx1ZTtcbiAgICAgICAgICB0aGlzLnZhbHVlID0gdmFsdWVGb3JtYXR0aW5nKHsgbGFiZWw6IHRoaXMubGFiZWwsIGRhdGE6IHRoaXMuZGF0YSwgdmFsdWUgfSk7XG4gICAgICAgICAgaWYgKCFmaW5pc2hlZCkge1xuICAgICAgICAgICAgdGhpcy52YWx1ZSA9IHRoaXMucGFkZGVkVmFsdWUodGhpcy52YWx1ZSk7XG4gICAgICAgICAgfVxuICAgICAgICAgIHRoaXMuY2QubWFya0ZvckNoZWNrKCk7XG4gICAgICAgIH0pO1xuICAgICAgfTtcblxuICAgICAgdGhpcy5hbmltYXRpb25SZXEgPSBjb3VudCgwLCB2YWwsIGRlY3MsIDEsIGNhbGxiYWNrKTtcbiAgICAgIHRoaXMuaW5pdGlhbGl6ZWQgPSB0cnVlO1xuICAgIH1cbiAgfVxuXG4gIHNjYWxlVGV4dCgpOiB2b2lkIHtcbiAgICB0aGlzLnpvbmUucnVuKCgpID0+IHtcbiAgICAgIGNvbnN0IHsgd2lkdGgsIGhlaWdodCB9ID0gdGhpcy50ZXh0RWwubmF0aXZlRWxlbWVudC5nZXRCb3VuZGluZ0NsaWVudFJlY3QoKTtcbiAgICAgIGlmICh3aWR0aCA9PT0gMCB8fCBoZWlnaHQgPT09IDApIHtcbiAgICAgICAgcmV0dXJuO1xuICAgICAgfVxuXG4gICAgICBjb25zdCB0ZXh0UGFkZGluZyA9ICh0aGlzLnRleHRQYWRkaW5nWzFdID0gdGhpcy50ZXh0UGFkZGluZ1szXSA9IHRoaXMuY2FyZFdpZHRoIC8gOCk7XG4gICAgICBjb25zdCBhdmFpbGFibGVXaWR0aCA9IHRoaXMuY2FyZFdpZHRoIC0gMiAqIHRleHRQYWRkaW5nO1xuICAgICAgY29uc3QgYXZhaWxhYmxlSGVpZ2h0ID0gdGhpcy5jYXJkSGVpZ2h0IC8gMztcblxuICAgICAgY29uc3QgcmVzaXplU2NhbGUgPSBNYXRoLm1pbihhdmFpbGFibGVXaWR0aCAvIHdpZHRoLCBhdmFpbGFibGVIZWlnaHQgLyBoZWlnaHQpO1xuICAgICAgdGhpcy50ZXh0Rm9udFNpemUgPSBNYXRoLmZsb29yKHRoaXMudGV4dEZvbnRTaXplICogcmVzaXplU2NhbGUpO1xuICAgICAgdGhpcy5sYWJlbEZvbnRTaXplID0gTWF0aC5taW4odGhpcy50ZXh0Rm9udFNpemUsIDE1KTtcblxuICAgICAgdGhpcy5zZXRQYWRkaW5nKCk7XG4gICAgICB0aGlzLmNkLm1hcmtGb3JDaGVjaygpO1xuICAgIH0pO1xuICB9XG5cbiAgc2V0UGFkZGluZygpIHtcbiAgICB0aGlzLnRleHRQYWRkaW5nWzFdID0gdGhpcy50ZXh0UGFkZGluZ1szXSA9IHRoaXMuY2FyZFdpZHRoIC8gODtcbiAgICBjb25zdCBwYWRkaW5nID0gdGhpcy5jYXJkSGVpZ2h0IC8gMjtcbiAgICB0aGlzLnRleHRQYWRkaW5nWzBdID0gcGFkZGluZyAtIHRoaXMudGV4dEZvbnRTaXplIC0gdGhpcy5sYWJlbEZvbnRTaXplIC8gMjtcbiAgICB0aGlzLnRleHRQYWRkaW5nWzJdID0gcGFkZGluZyAtIHRoaXMubGFiZWxGb250U2l6ZTtcbiAgfVxuXG4gIG9uQ2xpY2soKTogdm9pZCB7XG4gICAgdGhpcy5zZWxlY3QuZW1pdCh0aGlzLmRhdGEpO1xuICB9XG59XG4iXX0=