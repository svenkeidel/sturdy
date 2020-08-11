import { __decorate } from "tslib";
import { ElementRef, NgZone, ChangeDetectorRef, Component, Input, Output, EventEmitter, AfterViewInit, OnDestroy, OnChanges, SimpleChanges } from '@angular/core';
import { fromEvent as observableFromEvent } from 'rxjs';
import { debounceTime } from 'rxjs/operators';
import { VisibilityObserver } from '../utils/visibility-observer';
let BaseChartComponent = class BaseChartComponent {
    constructor(chartElement, zone, cd) {
        this.chartElement = chartElement;
        this.zone = zone;
        this.cd = cd;
        this.scheme = 'cool';
        this.schemeType = 'ordinal';
        this.animations = true;
        this.select = new EventEmitter();
    }
    ngAfterViewInit() {
        this.bindWindowResizeEvent();
        // listen for visibility of the element for hidden by default scenario
        this.visibilityObserver = new VisibilityObserver(this.chartElement, this.zone);
        this.visibilityObserver.visible.subscribe(this.update.bind(this));
    }
    ngOnDestroy() {
        this.unbindEvents();
        if (this.visibilityObserver) {
            this.visibilityObserver.visible.unsubscribe();
            this.visibilityObserver.destroy();
        }
    }
    ngOnChanges(changes) {
        this.update();
    }
    update() {
        if (this.results) {
            this.results = this.cloneData(this.results);
        }
        else {
            this.results = [];
        }
        if (this.view) {
            this.width = this.view[0];
            this.height = this.view[1];
        }
        else {
            const dims = this.getContainerDims();
            if (dims) {
                this.width = dims.width;
                this.height = dims.height;
            }
        }
        // default values if width or height are 0 or undefined
        if (!this.width) {
            this.width = 600;
        }
        if (!this.height) {
            this.height = 400;
        }
        this.width = Math.floor(this.width);
        this.height = Math.floor(this.height);
        if (this.cd) {
            this.cd.markForCheck();
        }
    }
    getContainerDims() {
        let width;
        let height;
        const hostElem = this.chartElement.nativeElement;
        if (hostElem.parentNode !== null) {
            // Get the container dimensions
            const dims = hostElem.parentNode.getBoundingClientRect();
            width = dims.width;
            height = dims.height;
        }
        if (width && height) {
            return { width, height };
        }
        return null;
    }
    /**
     * Converts all date objects that appear as name
     * into formatted date strings
     */
    formatDates() {
        for (let i = 0; i < this.results.length; i++) {
            const g = this.results[i];
            g.label = g.name;
            if (g.label instanceof Date) {
                g.label = g.label.toLocaleDateString();
            }
            if (g.series) {
                for (let j = 0; j < g.series.length; j++) {
                    const d = g.series[j];
                    d.label = d.name;
                    if (d.label instanceof Date) {
                        d.label = d.label.toLocaleDateString();
                    }
                }
            }
        }
    }
    unbindEvents() {
        if (this.resizeSubscription) {
            this.resizeSubscription.unsubscribe();
        }
    }
    bindWindowResizeEvent() {
        const source = observableFromEvent(window, 'resize');
        const subscription = source.pipe(debounceTime(200)).subscribe(e => {
            this.update();
            if (this.cd) {
                this.cd.markForCheck();
            }
        });
        this.resizeSubscription = subscription;
    }
    /**
     * Clones the data into a new object
     *
     * @memberOf BaseChart
     */
    cloneData(data) {
        const results = [];
        for (const item of data) {
            const copy = {
                name: item['name']
            };
            if (item['value'] !== undefined) {
                copy['value'] = item['value'];
            }
            if (item['series'] !== undefined) {
                copy['series'] = [];
                for (const seriesItem of item['series']) {
                    const seriesItemCopy = Object.assign({}, seriesItem);
                    copy['series'].push(seriesItemCopy);
                }
            }
            if (item['extra'] !== undefined) {
                copy['extra'] = JSON.parse(JSON.stringify(item['extra']));
            }
            results.push(copy);
        }
        return results;
    }
};
BaseChartComponent.ctorParameters = () => [
    { type: ElementRef },
    { type: NgZone },
    { type: ChangeDetectorRef }
];
__decorate([
    Input()
], BaseChartComponent.prototype, "results", void 0);
__decorate([
    Input()
], BaseChartComponent.prototype, "view", void 0);
__decorate([
    Input()
], BaseChartComponent.prototype, "scheme", void 0);
__decorate([
    Input()
], BaseChartComponent.prototype, "schemeType", void 0);
__decorate([
    Input()
], BaseChartComponent.prototype, "customColors", void 0);
__decorate([
    Input()
], BaseChartComponent.prototype, "animations", void 0);
__decorate([
    Output()
], BaseChartComponent.prototype, "select", void 0);
BaseChartComponent = __decorate([
    Component({
        selector: 'base-chart',
        template: `
    <div></div>
  `
    })
], BaseChartComponent);
export { BaseChartComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYmFzZS1jaGFydC5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vYmFzZS1jaGFydC5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFDTCxVQUFVLEVBQ1YsTUFBTSxFQUNOLGlCQUFpQixFQUNqQixTQUFTLEVBQ1QsS0FBSyxFQUNMLE1BQU0sRUFDTixZQUFZLEVBQ1osYUFBYSxFQUNiLFNBQVMsRUFDVCxTQUFTLEVBQ1QsYUFBYSxFQUNkLE1BQU0sZUFBZSxDQUFDO0FBRXZCLE9BQU8sRUFBRSxTQUFTLElBQUksbUJBQW1CLEVBQUUsTUFBTSxNQUFNLENBQUM7QUFDeEQsT0FBTyxFQUFFLFlBQVksRUFBRSxNQUFNLGdCQUFnQixDQUFDO0FBQzlDLE9BQU8sRUFBRSxrQkFBa0IsRUFBRSxNQUFNLDhCQUE4QixDQUFDO0FBUWxFLElBQWEsa0JBQWtCLEdBQS9CLE1BQWEsa0JBQWtCO0lBZTdCLFlBQXNCLFlBQXdCLEVBQVksSUFBWSxFQUFZLEVBQXFCO1FBQWpGLGlCQUFZLEdBQVosWUFBWSxDQUFZO1FBQVksU0FBSSxHQUFKLElBQUksQ0FBUTtRQUFZLE9BQUUsR0FBRixFQUFFLENBQW1CO1FBWjlGLFdBQU0sR0FBUSxNQUFNLENBQUM7UUFDckIsZUFBVSxHQUFXLFNBQVMsQ0FBQztRQUUvQixlQUFVLEdBQVksSUFBSSxDQUFDO1FBRTFCLFdBQU0sR0FBRyxJQUFJLFlBQVksRUFBRSxDQUFDO0lBT29FLENBQUM7SUFFM0csZUFBZTtRQUNiLElBQUksQ0FBQyxxQkFBcUIsRUFBRSxDQUFDO1FBRTdCLHNFQUFzRTtRQUN0RSxJQUFJLENBQUMsa0JBQWtCLEdBQUcsSUFBSSxrQkFBa0IsQ0FBQyxJQUFJLENBQUMsWUFBWSxFQUFFLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQztRQUMvRSxJQUFJLENBQUMsa0JBQWtCLENBQUMsT0FB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QUFFO21EQUFjO0FBQ2I7SUFBUixLQUFLLEVBQUU7Z0RBQXdCO0FBQ3ZCO0lBQVIsS0FBSyxFQUFFO2tEQUFzQjtBQUNyQjtJQUFSLEtBQUssRUFBRTtzREFBZ0M7QUFDL0I7SUFBUixLQUFLLEVBQUU7d0RBQW1CO0FBQ2xCO0lBQVIsS0FBSyxFQUFFO3NEQUE0QjtBQUUxQjtJQUFULE1BQU0sRUFBRTtrREFBNkI7QUFSM0Isa0JBQWtCO0lBTjlCLFNBQVMsQ0FBQztRQUNULFFBQVEsRUFBRSxZQUFZO1FBQ3RCLFFBQVEsRUFBRTs7R0FFVDtLQUNGLENBQUM7R0FDVyxrQkFBa0IsQ0FzSzlCO1NBdEtZLGtCQUFrQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIEVsZW1lbnRSZWYsXG4gIE5nWm9uZSxcbiAgQ2hhbmdlRGV0ZWN0b3JSZWYsXG4gIENvbXBvbmVudCxcbiAgSW5wdXQsXG4gIE91dHB1dCxcbiAgRXZlbnRFbWl0dGVyLFxuICBBZnRlclZpZXdJbml0LFxuICBPbkRlc3Ryb3ksXG4gIE9uQ2hhbmdlcyxcbiAgU2ltcGxlQ2hhbmdlc1xufSBmcm9tICdAYW5ndWxhci9jb3JlJztcblxuaW1wb3J0IHsgZnJvbUV2ZW50IGFzIG9ic2VydmFibGVGcm9tRXZlbnQgfSBmcm9tICdyeGpzJztcbmltcG9ydCB7IGRlYm91bmNlVGltZSB9IGZyb20gJ3J4anMvb3BlcmF0b3JzJztcbmltcG9ydCB7IFZpc2liaWxpdHlPYnNlcnZlciB9IGZyb20gJy4uL3V0aWxzL3Zpc2liaWxpdHktb2JzZXJ2ZXInO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICdiYXNlLWNoYXJ0JyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8ZGl2PjwvZGl2PlxuICBgXG59KVxuZXhwb3J0IGNsYXNzIEJhc2VDaGFydENvbXBvbmVudCBpbXBsZW1lbnRzIE9uQ2hhbmdlcywgQWZ0ZXJWaWV3SW5pdCwgT25EZXN0cm95IHtcbiAgQElucHV0KCkgcmVzdWx0czogYW55O1xuICBASW5wdXQoKSB2aWV3OiBbbnVtYmVyLCBudW1iZXJdO1xuICBASW5wdXQoKSBzY2hlbWU6IGFueSA9ICdjb29sJztcbiAgQElucHV0KCkgc2NoZW1lVHlwZTogc3RyaW5nID0gJ29yZGluYWwnO1xuICBASW5wdXQoKSBjdXN0b21Db2xvcnM6IGFueTtcbiAgQElucHV0KCkgYW5pbWF0aW9uczogYm9vbGVhbiA9IHRydWU7XG5cbiAgQE91dHB1dCgpIHNlbGVjdCA9IG5ldyBFdmVudEVtaXR0ZXIoKTtcblxuICB3aWR0aDogbnVtYmVyO1xuICBoZWlnaHQ6IG51bWJlcjtcbiAgcmVzaXplU3Vic2NyaXB0aW9uOiBhbnk7XG4gIHZpc2liaWxpdHlPYnNlcnZlcjogVmlzaWJpbGl0eU9ic2VydmVyO1xuXG4gIGNvbnN0cnVjdG9yKHByb3RlY3RlZCBjaGFydEVsZW1lbnQ6IEVsZW1lbnRSZWYsIHByb3RlY3RlZCB6b25lOiBOZ1pvbmUsIHByb3RlY3RlZCBjZDogQ2hhbmdlRGV0ZWN0b3JSZWYpIHt9XG5cbiAgbmdBZnRlclZpZXdJbml0KCk6IHZvaWQge1xuICAgIHRoaXMuYmluZFdpbmRvd1Jlc2l6ZUV2ZW50KCk7XG5cbiAgICAvLyBsaXN0ZW4gZm9yIHZpc2liaWxpdHkgb2YgdGhlIGVsZW1lbnQgZm9yIGhpZGRlbiBieSBkZWZhdWx0IHNjZW5hcmlvXG4gICAgdGhpcy52aXNpYmlsaXR5T2JzZXJ2ZXIgPSBuZXcgVmlzaWJpbGl0eU9ic2VydmVyKHRoaXMuY2hhcnRFbGVtZW50LCB0aGlzLnpvbmUpO1xuICAgIHRoaXMudmlzaWJpbGl0eU9ic2VydmVyLnZpc2libGUuc3Vic2NyaWJlKHRoaXMudXBkYXRlLmJpbmQodGhpcykpO1xuICB9XG5cbiAgbmdPbkRlc3Ryb3koKTogdm9pZCB7XG4gICAgdGhpcy51bmJpbmRFdmVudHMoKTtcbiAgICBpZiAodGhpcy52aXNpYmlsaXR5T2JzZXJ2ZXIpIHtcbiAgICAgIHRoaXMudmlzaWJpbGl0eU9ic2VydmVyLnZpc2libGUudW5zdWJzY3JpYmUoKTtcbiAgICAgIHRoaXMudmlzaWJpbGl0eU9ic2VydmVyLmRlc3Ryb3koKTtcbiAgICB9XG4gIH1cblxuICBuZ09uQ2hhbmdlcyhjaGFuZ2VzOiBTaW1wbGVDaGFuZ2VzKTogdm9pZCB7XG4gICAgdGhpcy51cGRhdGUoKTtcbiAgfVxuXG4gIHVwZGF0ZSgpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5yZXN1bHRzKSB7XG4gICAgICB0aGlzLnJlc3VsdHMgPSB0aGlzLmNsb25lRGF0YSh0aGlzLnJlc3VsdHMpO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLnJlc3VsdHMgPSBbXTtcbiAgICB9XG5cbiAgICBpZiAodGhpcy52aWV3KSB7XG4gICAgICB0aGlzLndpZHRoID0gdGhpcy52aWV3WzBdO1xuICAgICAgdGhpcy5oZWlnaHQgPSB0aGlzLnZpZXdbMV07XG4gICAgfSBlbHNlIHtcbiAgICAgIGNvbnN0IGRpbXMgPSB0aGlzLmdldENvbnRhaW5lckRpbXMoKTtcbiAgICAgIGlmIChkaW1zKSB7XG4gICAgICAgIHRoaXMud2lkdGggPSBkaW1zLndpZHRoO1xuICAgICAgICB0aGlzLmhlaWdodCA9IGRpbXMuaGVpZ2h0O1xuICAgICAgfVxuICAgIH1cblxuICAgIC8vIGRlZmF1bHQgdmFsdWVzIGlmIHdpZHRoIG9yIGhlaWdodCBhcmUgMCBvciB1bmRlZmluZWRcbiAgICBpZiAoIXRoaXMud2lkdGgpIHtcbiAgICAgIHRoaXMud2lkdGggPSA2MDA7XG4gICAgfVxuXG4gICAgaWYgKCF0aGlzLmhlaWdodCkge1xuICAgICAgdGhpcy5oZWlnaHQgPSA0MDA7XG4gICAgfVxuXG4gICAgdGhpcy53aWR0aCA9IE1hdGguZmxvb3IodGhpcy53aWR0aCk7XG4gICAgdGhpcy5oZWlnaHQgPSBNYXRoLmZsb29yKHRoaXMuaGVpZ2h0KTtcblxuICAgIGlmICh0aGlzLmNkKSB7XG4gICAgICB0aGlzLmNkLm1hcmtGb3JDaGVjaygpO1xuICAgIH1cbiAgfVxuXG4gIGdldENvbnRhaW5lckRpbXMoKTogYW55IHtcbiAgICBsZXQgd2lkdGg7XG4gICAgbGV0IGhlaWdodDtcbiAgICBjb25zdCBob3N0RWxlbSA9IHRoaXMuY2hhcnRFbGVtZW50Lm5hdGl2ZUVsZW1lbnQ7XG5cbiAgICBpZiAoaG9zdEVsZW0ucGFyZW50Tm9kZSAhPT0gbnVsbCkge1xuICAgICAgLy8gR2V0IHRoZSBjb250YWluZXIgZGltZW5zaW9uc1xuICAgICAgY29uc3QgZGltcyA9IGhvc3RFbGVtLnBhcmVudE5vZGUuZ2V0Qm91bmRpbmdDbGllbnRSZWN0KCk7XG4gICAgICB3aWR0aCA9IGRpbXMud2lkdGg7XG4gICAgICBoZWlnaHQgPSBkaW1zLmhlaWdodDtcbiAgICB9XG5cbiAgICBpZiAod2lkdGggJiYgaGVpZ2h0KSB7XG4gICAgICByZXR1cm4geyB3aWR0aCwgaGVpZ2h0IH07XG4gICAgfVxuXG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuICAvKipcbiAgICogQ29udmVydHMgYWxsIGRhdGUgb2JqZWN0cyB0aGF0IGFwcGVhciBhcyBuYW1lXG4gICAqIGludG8gZm9ybWF0dGVkIGRhdGUgc3RyaW5nc1xuICAgKi9cbiAgZm9ybWF0RGF0ZXMoKTogdm9pZCB7XG4gICAgZm9yIChsZXQgaSA9IDA7IGkgPCB0aGlzLnJlc3VsdHMubGVuZ3RoOyBpKyspIHtcbiAgICAgIGNvbnN0IGcgPSB0aGlzLnJlc3VsdHNbaV07XG4gICAgICBnLmxhYmVsID0gZy5uYW1lO1xuICAgICAgaWYgKGcubGFiZWwgaW5zdGFuY2VvZiBEYXRlKSB7XG4gICAgICAgIGcubGFiZWwgPSBnLmxhYmVsLnRvTG9jYWxlRGF0ZVN0cmluZygpO1xuICAgICAgfVxuXG4gICAgICBpZiAoZy5zZXJpZXMpIHtcbiAgICAgICAgZm9yIChsZXQgaiA9IDA7IGogPCBnLnNlcmllcy5sZW5ndGg7IGorKykge1xuICAgICAgICAgIGNvbnN0IGQgPSBnLnNlcmllc1tqXTtcbiAgICAgICAgICBkLmxhYmVsID0gZC5uYW1lO1xuICAgICAgICAgIGlmIChkLmxhYmVsIGluc3RhbmNlb2YgRGF0ZSkge1xuICAgICAgICAgICAgZC5sYWJlbCA9IGQubGFiZWwudG9Mb2NhbGVEYXRlU3RyaW5nKCk7XG4gICAgICAgICAgfVxuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgcHJvdGVjdGVkIHVuYmluZEV2ZW50cygpOiB2b2lkIHtcbiAgICBpZiAodGhpcy5yZXNpemVTdWJzY3JpcHRpb24pIHtcbiAgICAgIHRoaXMucmVzaXplU3Vic2NyaXB0aW9uLnVuc3Vic2NyaWJlKCk7XG4gICAgfVxuICB9XG5cbiAgcHJpdmF0ZSBiaW5kV2luZG93UmVzaXplRXZlbnQoKTogdm9pZCB7XG4gICAgY29uc3Qgc291cmNlID0gb2JzZXJ2YWJsZUZyb21FdmVudCh3aW5kb3csICdyZXNpemUnKTtcbiAgICBjb25zdCBzdWJzY3JpcHRpb24gPSBzb3VyY2UucGlwZShkZWJvdW5jZVRpbWUoMjAwKSkuc3Vic2NyaWJlKGUgPT4ge1xuICAgICAgdGhpcy51cGRhdGUoKTtcbiAgICAgIGlmICh0aGlzLmNkKSB7XG4gICAgICAgIHRoaXMuY2QubWFya0ZvckNoZWNrKCk7XG4gICAgICB9XG4gICAgfSk7XG4gICAgdGhpcy5yZXNpemVTdWJzY3JpcHRpb24gPSBzdWJzY3JpcHRpb247XG4gIH1cblxuICAvKipcbiAgICogQ2xvbmVzIHRoZSBkYXRhIGludG8gYSBuZXcgb2JqZWN0XG4gICAqXG4gICAqIEBtZW1iZXJPZiBCYXNlQ2hhcnRcbiAgICovXG4gIHByaXZhdGUgY2xvbmVEYXRhKGRhdGEpOiBhbnkge1xuICAgIGNvbnN0IHJlc3VsdHMgPSBbXTtcblxuICAgIGZvciAoY29uc3QgaXRlbSBvZiBkYXRhKSB7XG4gICAgICBjb25zdCBjb3B5ID0ge1xuICAgICAgICBuYW1lOiBpdGVtWyduYW1lJ11cbiAgICAgIH07XG5cbiAgICAgIGlmIChpdGVtWyd2YWx1ZSddICE9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgY29weVsndmFsdWUnXSA9IGl0ZW1bJ3ZhbHVlJ107XG4gICAgICB9XG5cbiAgICAgIGlmIChpdGVtWydzZXJpZXMnXSAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICAgIGNvcHlbJ3NlcmllcyddID0gW107XG4gICAgICAgIGZvciAoY29uc3Qgc2VyaWVzSXRlbSBvZiBpdGVtWydzZXJpZXMnXSkge1xuICAgICAgICAgIGNvbnN0IHNlcmllc0l0ZW1Db3B5ID0gT2JqZWN0LmFzc2lnbih7fSwgc2VyaWVzSXRlbSk7XG4gICAgICAgICAgY29weVsnc2VyaWVzJ10ucHVzaChzZXJpZXNJdGVtQ29weSk7XG4gICAgICAgIH1cbiAgICAgIH1cblxuICAgICAgaWYgKGl0ZW1bJ2V4dHJhJ10gIT09IHVuZGVmaW5lZCkge1xuICAgICAgICBjb3B5WydleHRyYSddID0gSlNPTi5wYXJzZShKU09OLnN0cmluZ2lmeShpdGVtWydleHRyYSddKSk7XG4gICAgICB9XG5cbiAgICAgIHJlc3VsdHMucHVzaChjb3B5KTtcbiAgICB9XG5cbiAgICByZXR1cm4gcmVzdWx0cztcbiAgfVxufVxuIl19