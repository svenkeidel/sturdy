import { __decorate } from "tslib";
import { Component, Input, Output, EventEmitter, ChangeDetectorRef, OnDestroy, ElementRef } from '@angular/core';
import { count, decimalChecker } from './count.helper';
/**
 * Count up component
 *
 * Loosely inspired by:
 *  - https://github.com/izupet/angular2-counto
 *  - https://inorganik.github.io/countUp.js/
 *
 * @export
 */
let CountUpDirective = class CountUpDirective {
    constructor(cd, element) {
        this.cd = cd;
        this.countDuration = 1;
        this.countPrefix = '';
        this.countSuffix = '';
        this.countChange = new EventEmitter();
        this.countFinish = new EventEmitter();
        this.value = '';
        this._countDecimals = 0;
        this._countTo = 0;
        this._countFrom = 0;
        this.nativeElement = element.nativeElement;
    }
    set countDecimals(val) {
        this._countDecimals = val;
    }
    get countDecimals() {
        if (this._countDecimals)
            return this._countDecimals;
        return decimalChecker(this.countTo);
    }
    set countTo(val) {
        this._countTo = parseFloat(val);
        this.start();
    }
    get countTo() {
        return this._countTo;
    }
    set countFrom(val) {
        this._countFrom = parseFloat(val);
        this.start();
    }
    get countFrom() {
        return this._countFrom;
    }
    ngOnDestroy() {
        cancelAnimationFrame(this.animationReq);
    }
    start() {
        cancelAnimationFrame(this.animationReq);
        const valueFormatting = this.valueFormatting || (value => `${this.countPrefix}${value.toLocaleString()}${this.countSuffix}`);
        const callback = ({ value, progress, finished }) => {
            this.value = valueFormatting(value);
            this.cd.markForCheck();
            if (!finished)
                this.countChange.emit({ value: this.value, progress });
            if (finished)
                this.countFinish.emit({ value: this.value, progress });
        };
        this.animationReq = count(this.countFrom, this.countTo, this.countDecimals, this.countDuration, callback);
    }
};
CountUpDirective.ctorParameters = () => [
    { type: ChangeDetectorRef },
    { type: ElementRef }
];
__decorate([
    Input()
], CountUpDirective.prototype, "countDuration", void 0);
__decorate([
    Input()
], CountUpDirective.prototype, "countPrefix", void 0);
__decorate([
    Input()
], CountUpDirective.prototype, "countSuffix", void 0);
__decorate([
    Input()
], CountUpDirective.prototype, "valueFormatting", void 0);
__decorate([
    Input()
], CountUpDirective.prototype, "countDecimals", null);
__decorate([
    Input()
], CountUpDirective.prototype, "countTo", null);
__decorate([
    Input()
], CountUpDirective.prototype, "countFrom", null);
__decorate([
    Output()
], CountUpDirective.prototype, "countChange", void 0);
__decorate([
    Output()
], CountUpDirective.prototype, "countFinish", void 0);
CountUpDirective = __decorate([
    Component({
        selector: '[ngx-charts-count-up]',
        template: `
    {{ value }}
  `
    })
], CountUpDirective);
export { CountUpDirective };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY291bnQuZGlyZWN0aXZlLmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvY29tbW9uL2NvdW50L2NvdW50LmRpcmVjdGl2ZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUFFLFNBQVMsRUFBRSxLQUFLLEVBQUUsTUFBTSxFQUFFLFlBQVksRUFBRSxpQkFBaUIsRUFBRSxTQUFTLEVBQUUsVUFBVSxFQUFFLE1BQU0sZUFBZSxDQUFDO0FBQ2pILE9BQU8sRUFBRSxLQUFLLEVBQUUsY0FBYyxFQUFFLE1BQU0sZ0JBQWdCLENBQUM7QUFFdkQ7Ozs7Ozs7O0dBUUc7QUFPSCxJQUFhLGdCQUFnQixHQUE3QixNQUFhLGdCQUFnQjtJQWtEM0IsWUFBb0IsRUFBcUIsRUFBRSxPQUFtQjtRQUExQyxPQUFFLEdBQUYsRUFBRSxDQUFtQjtRQWpEaEMsa0JBQWEsR0FBVyxDQUFDLENBQUM7UUFDMUIsZ0JBQVcsR0FBVyxFQUFFLENBQUM7UUFDekIsZ0JBQVcsR0FBVyxFQUFFLENBQUM7UUFpQ3hCLGdCQUFXLEdBQUcsSUFBSSxZQUFZLEVBQUUsQ0FBQztRQUNqQyxnQkFBVyxHQUFHLElBQUksWUFBWSxFQUFFLENBQUM7UUFJM0MsVUFBSyxHQUFRLEVBQUUsQ0FBQztRQUtSLG1CQUFjLEdBQVcsQ0FBQyxDQUFDO1FBQzNCLGFBQVEsR0FBVyxDQUFDLENBQUM7UUFDckIsZUFBVSxHQUFXLENBQUMsQ0FBQztRQUc3QixJQUFJLENBQUMsYUFBYSxHQUFHLE9BQU8sQ0FBQyxhQUFhLENBQUM7SUFDN0MsQ0FBQztJQTdDRCxJQUFJLGFBQWEsQ0FBQyxHQUFXO1FBQzNCLElBQUksQ0FBQyxjQUFjLEdBQUcsR0FBRyxDQUFDO0lBQzVCLENBQUM7SUFFRCxJQUFJLGFBQWE7UUFDZixJQUFJLElBQUksQ0FBQyxjQUFjO1lBQUUsT0FBTyxJQUFJLENBQUMsY0FBYyxDQUFDO1FBQ3BELE9BQU8sY0FBYyxDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsQ0FBQztJQUN0QyxDQUFDO0lBR0QsSUFBSSxPQUFPLENBQUMsR0FBRztRQUNiLElBQUksQ0FBQyxRQUFRLEdBQUcsVUFBVSxDQUFDLEdBQUcsQ0FBQyxDQUFDO1FBQ2hDLElBQUksQ0FBQyxLQUFLLEVBQUUsQ0FBQztJQUNmLENBQUM7SUFFRCxJQUFJLE9BQU87UUFDVCxPQUFPLElBQUksQ0FBQyxRQUFRLENBQUM7SUFDdkIsQ0FBQztJQUdELElBQUksU0FBUyxDQUFDLEdBQUc7UUFDZixJQUFJLENBQUMsVUFBVSxHQUFHLFVBQVUsQ0FBQyxHQUFHLENBQUMsQ0FBQztRQUNsQyxJQUFJLENBQUMsS0FBSyxFQUFFLENBQUM7SUFDZixDQUFDO0lBRUQsSUFBSSxTQUFTO1FBQ1gsT0FBTyxJQUFJLENBQUMsVUFBVSxDQUFDO0lBQ3pCLENBQUM7SUFvQkQsV0FBVztRQUNULG9CQUFvQixDQUFDLElBQUksQ0FBQyxZQUFZLENBQUMsQ0FBQztJQUMxQyxDQUFDO0lBRUQsS0FBSztRQUNILG9CQUFvQixDQUFDLElBQUksQ0FBQyxZQUFZLENBQUMsQ0FBQztRQUV4QyxNQUFNLGVBQWUsR0FDbkIsSUFBSSxDQUFDLGVBQWUsSUFBSSxDQUFDLEtBQUssQ0FBQyxFQUFFLENBQUMsR0FBRyxJQUFJLENBQUMsV0FBVyxHQUFHLEtBQUssQ0FBQyxjQUFjLEVBQUUsR0FBRyxJQUFJLENBQUMsV0FBVyxFQUFFLENBQUMsQ0FBQztRQUV2RyxNQUFNLFFBQVEsR0FBRyxDQUFDLEVBQUUsS0FBSyxFQUFFLFFBQVEsRUFBRSxRQUFRLEVBQUUsRUFBRSxFQUFFO1lBQ2pELElBQUksQ0FBQyxLQUFLLEdBQUcsZUFBZSxDQUFDLEtBQUssQ0FBQyxDQUFDO1lBQ3BDLElBQUksQ0FBQyxFQUFFLENBQUMsWUFBWSxFQUFFLENBQUM7WUFDdkIsSUFBSSxDQUFDLFFBQVE7Z0JBQUUsSUFBSSxDQUFDLFdBQVcsQ0FBQyxJQUFJLENBQUMsRUFBRSxLQUFLLEVBQUUsSUFBSSxDQUFDLEtBQUssRUFBRSxRQUFRLEVBQUUsQ0FBQyxDQUFDO1lBQ3RFLElBQUksUUFBUTtnQkFBRSxJQUFJLENBQUMsV0FBVyxDQUFDLElBQUksQ0FBQyxFQUFFLEtBQUssRUFBRSxJQUFJLENBQUMsS0FBSyxFQUFFLFFBQVEsRUFBRSxDQUFDLENBQUM7UUFDdkUsQ0FBQyxDQUFDO1FBRUYsSUFBSSxDQUFDLFlBQVksR0FBRyxLQUFLLENBQUMsSUFBSSxDQUFDLFNBQVMsRUFBRSxJQUFJLENBQUMsT0FBTyxFQUFFLElBQUksQ0FBQyxhQUFhLEVBQUUsSUFBSSxDQUFDLGFBQWEsRUFBRSxRQUFRLENBQUMsQ0FBQztJQUM1RyxDQUFDO0NBQ0YsQ0FBQTs7WUF2QnlCLGlCQUFpQjtZQUFXLFVBQVU7O0FBakRyRDtJQUFSLEtBQUssRUFBRTt1REFBMkI7QUFDMUI7SUFBUixLQUFLLEVBQUU7cURBQTBCO0FBQ3pCO0lBQVIsS0FBSyxFQUFFO3FEQUEwQjtBQUN6QjtJQUFSLEtBQUssRUFBRTt5REFBc0I7QUFHOUI7SUFEQyxLQUFLLEVBQUU7cURBR1A7QUFRRDtJQURDLEtBQUssRUFBRTsrQ0FJUDtBQU9EO0lBREMsS0FBSyxFQUFFO2lEQUlQO0FBTVM7SUFBVCxNQUFNLEVBQUU7cURBQWtDO0FBQ2pDO0lBQVQsTUFBTSxFQUFFO3FEQUFrQztBQXJDaEMsZ0JBQWdCO0lBTjVCLFNBQVMsQ0FBQztRQUNULFFBQVEsRUFBRSx1QkFBdUI7UUFDakMsUUFBUSxFQUFFOztHQUVUO0tBQ0YsQ0FBQztHQUNXLGdCQUFnQixDQXlFNUI7U0F6RVksZ0JBQWdCIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgQ29tcG9uZW50LCBJbnB1dCwgT3V0cHV0LCBFdmVudEVtaXR0ZXIsIENoYW5nZURldGVjdG9yUmVmLCBPbkRlc3Ryb3ksIEVsZW1lbnRSZWYgfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7IGNvdW50LCBkZWNpbWFsQ2hlY2tlciB9IGZyb20gJy4vY291bnQuaGVscGVyJztcblxuLyoqXG4gKiBDb3VudCB1cCBjb21wb25lbnRcbiAqXG4gKiBMb29zZWx5IGluc3BpcmVkIGJ5OlxuICogIC0gaHR0cHM6Ly9naXRodWIuY29tL2l6dXBldC9hbmd1bGFyMi1jb3VudG9cbiAqICAtIGh0dHBzOi8vaW5vcmdhbmlrLmdpdGh1Yi5pby9jb3VudFVwLmpzL1xuICpcbiAqIEBleHBvcnRcbiAqL1xuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnW25neC1jaGFydHMtY291bnQtdXBdJyxcbiAgdGVtcGxhdGU6IGBcbiAgICB7eyB2YWx1ZSB9fVxuICBgXG59KVxuZXhwb3J0IGNsYXNzIENvdW50VXBEaXJlY3RpdmUgaW1wbGVtZW50cyBPbkRlc3Ryb3kge1xuICBASW5wdXQoKSBjb3VudER1cmF0aW9uOiBudW1iZXIgPSAxO1xuICBASW5wdXQoKSBjb3VudFByZWZpeDogc3RyaW5nID0gJyc7XG4gIEBJbnB1dCgpIGNvdW50U3VmZml4OiBzdHJpbmcgPSAnJztcbiAgQElucHV0KCkgdmFsdWVGb3JtYXR0aW5nOiBhbnk7XG5cbiAgQElucHV0KClcbiAgc2V0IGNvdW50RGVjaW1hbHModmFsOiBudW1iZXIpIHtcbiAgICB0aGlzLl9jb3VudERlY2ltYWxzID0gdmFsO1xuICB9XG5cbiAgZ2V0IGNvdW50RGVjaW1hbHMoKTogbnVtYmVyIHtcbiAgICBpZiAodGhpcy5fY291bnREZWNpbWFscykgcmV0dXJuIHRoaXMuX2NvdW50RGVjaW1hbHM7XG4gICAgcmV0dXJuIGRlY2ltYWxDaGVja2VyKHRoaXMuY291bnRUbyk7XG4gIH1cblxuICBASW5wdXQoKVxuICBzZXQgY291bnRUbyh2YWwpIHtcbiAgICB0aGlzLl9jb3VudFRvID0gcGFyc2VGbG9hdCh2YWwpO1xuICAgIHRoaXMuc3RhcnQoKTtcbiAgfVxuXG4gIGdldCBjb3VudFRvKCk6IGFueSB7XG4gICAgcmV0dXJuIHRoaXMuX2NvdW50VG87XG4gIH1cblxuICBASW5wdXQoKVxuICBzZXQgY291bnRGcm9tKHZhbCkge1xuICAgIHRoaXMuX2NvdW50RnJvbSA9IHBhcnNlRmxvYXQodmFsKTtcbiAgICB0aGlzLnN0YXJ0KCk7XG4gIH1cblxuICBnZXQgY291bnRGcm9tKCk6IGFueSB7XG4gICAgcmV0dXJuIHRoaXMuX2NvdW50RnJvbTtcbiAgfVxuXG4gIEBPdXRwdXQoKSBjb3VudENoYW5nZSA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcbiAgQE91dHB1dCgpIGNvdW50RmluaXNoID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuXG4gIG5hdGl2ZUVsZW1lbnQ6IGFueTtcblxuICB2YWx1ZTogYW55ID0gJyc7XG4gIGZvcm1hdHRlZFZhbHVlOiBzdHJpbmc7XG5cbiAgcHJpdmF0ZSBhbmltYXRpb25SZXE6IGFueTtcblxuICBwcml2YXRlIF9jb3VudERlY2ltYWxzOiBudW1iZXIgPSAwO1xuICBwcml2YXRlIF9jb3VudFRvOiBudW1iZXIgPSAwO1xuICBwcml2YXRlIF9jb3VudEZyb206IG51bWJlciA9IDA7XG5cbiAgY29uc3RydWN0b3IocHJpdmF0ZSBjZDogQ2hhbmdlRGV0ZWN0b3JSZWYsIGVsZW1lbnQ6IEVsZW1lbnRSZWYpIHtcbiAgICB0aGlzLm5hdGl2ZUVsZW1lbnQgPSBlbGVtZW50Lm5hdGl2ZUVsZW1lbnQ7XG4gIH1cblxuICBuZ09uRGVzdHJveSgpOiB2b2lkIHtcbiAgICBjYW5jZWxBbmltYXRpb25GcmFtZSh0aGlzLmFuaW1hdGlvblJlcSk7XG4gIH1cblxuICBzdGFydCgpOiB2b2lkIHtcbiAgICBjYW5jZWxBbmltYXRpb25GcmFtZSh0aGlzLmFuaW1hdGlvblJlcSk7XG5cbiAgICBjb25zdCB2YWx1ZUZvcm1hdHRpbmcgPVxuICAgICAgdGhpcy52YWx1ZUZvcm1hdHRpbmcgfHwgKHZhbHVlID0+IGAke3RoaXMuY291bnRQcmVmaXh9JHt2YWx1ZS50b0xvY2FsZVN0cmluZygpfSR7dGhpcy5jb3VudFN1ZmZpeH1gKTtcblxuICAgIGNvbnN0IGNhbGxiYWNrID0gKHsgdmFsdWUsIHByb2dyZXNzLCBmaW5pc2hlZCB9KSA9PiB7XG4gICAgICB0aGlzLnZhbHVlID0gdmFsdWVGb3JtYXR0aW5nKHZhbHVlKTtcbiAgICAgIHRoaXMuY2QubWFya0ZvckNoZWNrKCk7XG4gICAgICBpZiAoIWZpbmlzaGVkKSB0aGlzLmNvdW50Q2hhbmdlLmVtaXQoeyB2YWx1ZTogdGhpcy52YWx1ZSwgcHJvZ3Jlc3MgfSk7XG4gICAgICBpZiAoZmluaXNoZWQpIHRoaXMuY291bnRGaW5pc2guZW1pdCh7IHZhbHVlOiB0aGlzLnZhbHVlLCBwcm9ncmVzcyB9KTtcbiAgICB9O1xuXG4gICAgdGhpcy5hbmltYXRpb25SZXEgPSBjb3VudCh0aGlzLmNvdW50RnJvbSwgdGhpcy5jb3VudFRvLCB0aGlzLmNvdW50RGVjaW1hbHMsIHRoaXMuY291bnREdXJhdGlvbiwgY2FsbGJhY2spO1xuICB9XG59XG4iXX0=