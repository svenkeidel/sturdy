import { __decorate, __values } from "tslib";
import { ElementRef, NgZone, ChangeDetectorRef, Component, Input, Output, EventEmitter, AfterViewInit, OnDestroy, OnChanges, SimpleChanges } from '@angular/core';
import { fromEvent as observableFromEvent } from 'rxjs';
import { debounceTime } from 'rxjs/operators';
import { VisibilityObserver } from '../utils/visibility-observer';
var BaseChartComponent = /** @class */ (function () {
    function BaseChartComponent(chartElement, zone, cd) {
        this.chartElement = chartElement;
        this.zone = zone;
        this.cd = cd;
        this.scheme = 'cool';
        this.schemeType = 'ordinal';
        this.animations = true;
        this.select = new EventEmitter();
    }
    BaseChartComponent.prototype.ngAfterViewInit = function () {
        this.bindWindowResizeEvent();
        // listen for visibility of the element for hidden by default scenario
        this.visibilityObserver = new VisibilityObserver(this.chartElement, this.zone);
        this.visibilityObserver.visible.subscribe(this.update.bind(this));
    };
    BaseChartComponent.prototype.ngOnDestroy = function () {
        this.unbindEvents();
        if (this.visibilityObserver) {
            this.visibilityObserver.visible.unsubscribe();
            this.visibilityObserver.destroy();
        }
    };
    BaseChartComponent.prototype.ngOnChanges = function (changes) {
        this.update();
    };
    BaseChartComponent.prototype.update = function () {
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
            var dims = this.getContainerDims();
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
    };
    BaseChartComponent.prototype.getContainerDims = function () {
        var width;
        var height;
        var hostElem = this.chartElement.nativeElement;
        if (hostElem.parentNode !== null) {
            // Get the container dimensions
            var dims = hostElem.parentNode.getBoundingClientRect();
            width = dims.width;
            height = dims.height;
        }
        if (width && height) {
            return { width: width, height: height };
        }
        return null;
    };
    /**
     * Converts all date objects that appear as name
     * into formatted date strings
     */
    BaseChartComponent.prototype.formatDates = function () {
        for (var i = 0; i < this.results.length; i++) {
            var g = this.results[i];
            g.label = g.name;
            if (g.label instanceof Date) {
                g.label = g.label.toLocaleDateString();
            }
            if (g.series) {
                for (var j = 0; j < g.series.length; j++) {
                    var d = g.series[j];
                    d.label = d.name;
                    if (d.label instanceof Date) {
                        d.label = d.label.toLocaleDateString();
                    }
                }
            }
        }
    };
    BaseChartComponent.prototype.unbindEvents = function () {
        if (this.resizeSubscription) {
            this.resizeSubscription.unsubscribe();
        }
    };
    BaseChartComponent.prototype.bindWindowResizeEvent = function () {
        var _this = this;
        var source = observableFromEvent(window, 'resize');
        var subscription = source.pipe(debounceTime(200)).subscribe(function (e) {
            _this.update();
            if (_this.cd) {
                _this.cd.markForCheck();
            }
        });
        this.resizeSubscription = subscription;
    };
    /**
     * Clones the data into a new object
     *
     * @memberOf BaseChart
     */
    BaseChartComponent.prototype.cloneData = function (data) {
        var e_1, _a, e_2, _b;
        var results = [];
        try {
            for (var data_1 = __values(data), data_1_1 = data_1.next(); !data_1_1.done; data_1_1 = data_1.next()) {
                var item = data_1_1.value;
                var copy = {
                    name: item['name']
                };
                if (item['value'] !== undefined) {
                    copy['value'] = item['value'];
                }
                if (item['series'] !== undefined) {
                    copy['series'] = [];
                    try {
                        for (var _c = (e_2 = void 0, __values(item['series'])), _d = _c.next(); !_d.done; _d = _c.next()) {
                            var seriesItem = _d.value;
                            var seriesItemCopy = Object.assign({}, seriesItem);
                            copy['series'].push(seriesItemCopy);
                        }
                    }
                    catch (e_2_1) { e_2 = { error: e_2_1 }; }
                    finally {
                        try {
                            if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
                        }
                        finally { if (e_2) throw e_2.error; }
                    }
                }
                if (item['extra'] !== undefined) {
                    copy['extra'] = JSON.parse(JSON.stringify(item['extra']));
                }
                results.push(copy);
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (data_1_1 && !data_1_1.done && (_a = data_1.return)) _a.call(data_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return results;
    };
    BaseChartComponent.ctorParameters = function () { return [
        { type: ElementRef },
        { type: NgZone },
        { type: ChangeDetectorRef }
    ]; };
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
            template: "\n    <div></div>\n  "
        })
    ], BaseChartComponent);
    return BaseChartComponent;
}());
export { BaseChartComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYmFzZS1jaGFydC5jb21wb25lbnQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vYmFzZS1jaGFydC5jb21wb25lbnQudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFDTCxVQUFVLEVBQ1YsTUFBTSxFQUNOLGlCQUFpQixFQUNqQixTQUFTLEVBQ1QsS0FBSyxFQUNMLE1BQU0sRUFDTixZQUFZLEVBQ1osYUFBYSxFQUNiLFNBQVMsRUFDVCxTQUFTLEVBQ1QsYUFBYSxFQUNkLE1BQU0sZUFBZSxDQUFDO0FBRXZCLE9BQU8sRUFBRSxTQUFTLElBQUksbUJBQW1CLEVBQUUsTUFBTSxNQUFNLENBQUM7QUFDeEQsT0FBTyxFQUFFLFlBQVksRUFBRSxNQUFNLGdCQUFnQixDQUFDO0FBQzlDLE9BQU8sRUFBRSxrQkFBa0IsRUFBRSxNQUFNLDhCQUE4QixDQUFDO0FBUWxFO0lBZUUsNEJBQXNCLFlBQXdCLEVBQVksSUFBWSxFQUFZLEVBQXFCO1FBQWpGLGlCQUFZLEdBQVosWUFBWSxDQUFZO1FBQVksU0FBSSxHQUFKLElBQUksQ0FBUTtRQUFZLE9BQUUsR0FBRixFQUFFLENBQW1CO1FBWjlGLFdBQU0sR0FBUSxNQUFNLENBQUM7UUFDckIsZUFBVSxHQUFXLFNBQVMsQ0FBQztRQUUvQixlQUFVLEdBQVksSUFBSSxDQUFDO1FBRTFCLFdBQU0sR0FBRyxJQUFJLFlBQVksRUFBRSxDQUFDO0lBT29FLENBQUM7SUFFM0csNENBQWUsR0FBZjtRQUNFLElBQUksQ0FBQyxxQkFBcUIsRUFBRSxDQUFDO1FBRTdCLHNFQUFzRTtRQUN0RSxJQUFJLENBQUMsa0JBQWtCLEdBQUcsSUFBSSxrQkFBa0IsQ0FBQyxJQUFJLENBQUMsWUFBWSxFQUFFLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQztRQUMvRSxJQUFJLENBQUMsa0JBQWtCLENBQUMsT0FBTyxDQUFDLFNBQVMsQ0FBQyxJQUFJLENBQUMsTUFBTSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDO0lBQ3BFLENBQUM7SUFFRCx3Q0FBVyxHQUFYO1FBQ0UsSUFBSSxDQUFDLFlBQVksRUFBRSxDQUFDO1FBQ3BCLElBQUksSUFBSSxDQUFDLGtCQUFrQixFQUFFO1lBQzNCLElBQUksQ0FBQyxrQkFBa0IsQ0FBQyxPQUFPLENBQUMsV0FBVyxFQUFFLENBQUM7WUFDOUMsSUFBSSxDQUFDLGtCQUFrQixDQUFDLE9BQU8sRUFBRSxDQUFDO1NBQ25DO0lBQ0gsQ0FBQztJQUVELHdDQUFXLEdBQVgsVUFBWSxPQUFzQjtRQUNoQyxJQUFJLENBQUMsTUFBTSxFQUFFLENBQUM7SUFDaEIsQ0FBQztJQUVELG1DQUFNLEdBQU47UUFDRSxJQUFJLElBQUksQ0FBQyxPQUFPLEVBQUU7WUFDaEIsSUFBSSxDQUFDLE9BQU8sR0FBRyxJQUFJLENBQUMsU0FBUyxDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsQ0FBQztTQUM3QzthQUFNO1lBQ0wsSUFBSSxDQUFDLE9BQU8sR0FBRyxFQUFFLENBQUM7U0FDbkI7UUFFRCxJQUFJLElBQUksQ0FBQyxJQUFJLEVBQUU7WUFDYixJQUFJLENBQUMsS0FBSyxHQUFHLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLENBQUM7WUFDMUIsSUFBSSxDQUFDLE1BQU0sR0FBRyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQyxDQUFDO1NBQzVCO2FBQU07WUFDTCxJQUFNLElBQUksR0FBRyxJQUFJLENBQUMsZ0JBQWdCLEVBQUUsQ0FBQztZQUNyQyxJQUFJLElBQUksRUFBRTtnQkFDUixJQUFJLENBQUMsS0FBSyxHQUFHLElBQUksQ0FBQyxLQUFLLENBQUM7Z0JBQ3hCLElBQUksQ0FBQyxNQUFNLEdBQUcsSUFBSSxDQUFDLE1BQU0sQ0FBQzthQUMzQjtTQUNGO1FBRUQsdURBQXVEO1FBQ3ZELElBQUksQ0FBQyxJQUFJLENBQUMsS0FBSyxFQUFFO1lBQ2YsSUFBSSxDQUFDLEtBQUssR0FBRyxHQUFHLENBQUM7U0FDbEI7UUFFRCxJQUFJLENBQUMsSUFBSSxDQUFDLE1BQU0sRUFBRTtZQUNoQixJQUFJLENBQUMsTUFBTSxHQUFHLEdBQUcsQ0FBQztTQUNuQjtRQUVELElBQUksQ0FBQyxLQUFLLEdBQUcsSUFBSSxDQUFDLEtBQUssQ0FBQyxJQUFJLENBQUMsS0FBSyxDQUFDLENBQUM7UUFDcEMsSUFBSSxDQUFDLE1BQU0sR0FBRyxJQUFJLENBQUMsS0FBSyxDQUFDLElBQUksQ0FBQyxNQUFNLENBQUMsQ0FBQztRQUV0QyxJQUFJLElBQUksQ0FBQyxFQUFFLEVBQUU7WUFDWCxJQUFJLENBQUMsRUFBRSxDQUFDLFlBQVksRUFBRSxDQUFDO1NBQ3hCO0lBQ0gsQ0FBQztJQUVELDZDQUFnQixHQUFoQjtRQUNFLElBQUksS0FBSyxDQUFDO1FBQ1YsSUFBSSxNQUFNLENBQUM7UUFDWCxJQUFNLFFBQVEsR0FBRyxJQUFJLENBQUMsWUFBWSxDQUFDLGFBQWEsQ0FBQztRQUVqRCxJQUFJLFFBQVEsQ0FBQyxVQUFVLEtBQUssSUFBSSxFQUFFO1lBQ2hDLCtCQUErQjtZQUMvQixJQUFNLElBQUksR0FBRyxRQUFRLENBQUMsVUFBVSxDQUFDLHFCQUFxQixFQUFFLENBQUM7WUFDekQsS0FBSyxHQUFHLElBQUksQ0FBQyxLQUFLLENBQUM7WUFDbkIsTUFBTSxHQUFHLElBQUksQ0FBQyxNQUFNLENBQUM7U0FDdEI7UUFFRCxJQUFJLEtBQUssSUFBSSxNQUFNLEVBQUU7WUFDbkIsT0FBTyxFQUFFLEtBQUssT0FBQSxFQUFFLE1BQU0sUUFBQSxFQUFFLENBQUM7U0FDMUI7UUFFRCxPQUFPLElBQUksQ0FBQztJQUNkLENBQUM7SUFFRDs7O09BR0c7SUFDSCx3Q0FBVyxHQUFYO1FBQ0UsS0FBSyxJQUFJLENBQUMsR0FBRyxDQUFDLEVBQUUsQ0FBQyxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUMsTUFBTSxFQUFFLENBQUMsRUFBRSxFQUFFO1lBQzVDLElBQU0sQ0FBQyxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUMsQ0FBQyxDQUFDLENBQUM7WUFDMUIsQ0FBQyxDQUFDLEtBQUssR0FBRyxDQUFDLENBQUMsSUFBSSxDQUFDO1lBQ2pCLElBQUksQ0FBQyxDQUFDLEtBQUssWUFBWSxJQUFJLEVBQUU7Z0JBQzNCLENBQUMsQ0FBQyxLQUFLLEdBQUcsQ0FBQyxDQUFDLEtBQUssQ0FBQyxrQkFBa0IsRUFBRSxDQUFDO2FBQ3hDO1lBRUQsSUFBSSxDQUFDLENBQUMsTUFBTSxFQUFFO2dCQUNaLEtBQUssSUFBSSxDQUFDLEdBQUcsQ0FBQyxFQUFFLENBQUMsR0FBRyxDQUFDLENBQUMsTUFBTSxDQUFDLE1BQU0sRUFBRSxDQUFDLEVBQUUsRUFBRTtvQkFDeEMsSUFBTSxDQUFDLEdBQUcsQ0FBQyxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUMsQ0FBQztvQkFDdEIsQ0FBQyxDQUFDLEtBQUssR0FBRyxDQUFDLENBQUMsSUFBSSxDQUFDO29CQUNqQixJQUFJLENBQUMsQ0FBQyxLQUFLLFlBQVksSUFBSSxFQUFFO3dCQUMzQixDQUFDLENBQUMsS0FBSyxHQUFHLENBQUMsQ0FBQyxLQUFLLENBQUMsa0JBQWtCLEVBQUUsQ0FBQztxQkFDeEM7aUJBQ0Y7YUFDRjtTQUNGO0lBQ0gsQ0FBQztJQUVTLHlDQUFZLEdBQXRCO1FBQ0UsSUFBSSxJQUFJLENBQUMsa0JBQWtCLEVBQUU7WUFDM0IsSUFBSSxDQUFDLGtCQUFrQixDQUFDLFdBQVcsRUFBRSxDQUFDO1NBQ3ZDO0lBQ0gsQ0FBQztJQUVPLGtEQUFxQixHQUE3QjtRQUFBLGlCQVNDO1FBUkMsSUFBTSxNQUFNLEdBQUcsbUJBQW1CLENBQUMsTUFBTSxFQUFFLFFBQVEsQ0FBQyxDQUFDO1FBQ3JELElBQU0sWUFBWSxHQUFHLE1BQU0sQ0FBQyxJQUFJLENBQUMsWUFBWSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsU0FBUyxDQUFDLFVBQUEsQ0FBQztZQUM3RCxLQUFJLENBQUMsTUFBTSxFQUFFLENBQUM7WUFDZCxJQUFJLEtBQUksQ0FBQyxFQUFFLEVBQUU7Z0JBQ1gsS0FBSSxDQUFDLEVBQUUsQ0FBQyxZQUFZLEVBQUUsQ0FBQzthQUN4QjtRQUNILENBQUMsQ0FBQyxDQUFDO1FBQ0gsSUFBSSxDQUFDLGtCQUFrQixHQUFHLFlBQVksQ0FBQztJQUN6QyxDQUFDO0lBRUQ7Ozs7T0FJRztJQUNLLHNDQUFTLEdBQWpCLFVBQWtCLElBQUk7O1FBQ3BCLElBQU0sT0FBTyxHQUFHLEVBQUUsQ0FBQzs7WUFFbkIsS0FBbUIsSUFBQSxTQUFBLFNBQUEsSUFBSSxDQUFBLDBCQUFBLDRDQUFFO2dCQUFwQixJQUFNLElBQUksaUJBQUE7Z0JBQ2IsSUFBTSxJQUFJLEdBQUc7b0JBQ1gsSUFBSSxFQUFFLElBQUksQ0FBQyxNQUFNLENBQUM7aUJBQ25CLENBQUM7Z0JBRUYsSUFBSSxJQUFJLENBQUMsT0FBTyxDQUFDLEtBQUssU0FBUyxFQUFFO29CQUMvQixJQUFJLENBQUMsT0FBTyxDQUFDLEdBQUcsSUFBSSxDQUFDLE9BQU8sQ0FBQyxDQUFDO2lCQUMvQjtnQkFFRCxJQUFJLElBQUksQ0FBQyxRQUFRLENBQUMsS0FBSyxTQUFTLEVBQUU7b0JBQ2hDLElBQUksQ0FBQyxRQUFRLENBQUMsR0FBRyxFQUFFLENBQUM7O3dCQUNwQixLQUF5QixJQUFBLG9CQUFBLFNBQUEsSUFBSSxDQUFDLFFBQVEsQ0FBQyxDQUFBLENBQUEsZ0JBQUEsNEJBQUU7NEJBQXBDLElBQU0sVUFBVSxXQUFBOzRCQUNuQixJQUFNLGNBQWMsR0FBRyxNQUFNLENBQUMsTUFBTSxDQUFDLEVBQUUsRUFBRSxVQUFVLENBQUMsQ0FBQzs0QkFDckQsSUFBSSxDQUFDLFFBQVEsQ0FBQyxDQUFDLElBQUksQ0FBQyxjQUFjLENBQUMsQ0FBQzt5QkFDckM7Ozs7Ozs7OztpQkFDRjtnQkFFRCxJQUFJLElBQUksQ0FBQyxPQUFPLENBQUMsS0FBSyxTQUFTLEVBQUU7b0JBQy9CLElBQUksQ0FBQyxPQUFPLENBQUMsR0FBRyxJQUFJLENBQUMsS0FBSyxDQUFDLElBQUksQ0FBQyxTQUFTLENBQUMsSUFBSSxDQUFDLE9BQU8sQ0FBQyxDQUFDLENBQUMsQ0FBQztpQkFDM0Q7Z0JBRUQsT0FBTyxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQzthQUNwQjs7Ozs7Ozs7O1FBRUQsT0FBTyxPQUFPLENBQUM7SUFDakIsQ0FBQzs7Z0JBdEptQyxVQUFVO2dCQUFrQixNQUFNO2dCQUFnQixpQkFBaUI7O0lBZDlGO1FBQVIsS0FBSyxFQUFFO3VEQUFjO0lBQ2I7UUFBUixLQUFLLEVBQUU7b0RBQXdCO0lBQ3ZCO1FBQVIsS0FBSyxFQUFFO3NEQUFzQjtJQUNyQjtRQUFSLEtBQUssRUFBRTswREFBZ0M7SUFDL0I7UUFBUixLQUFLLEVBQUU7NERBQW1CO0lBQ2xCO1FBQVIsS0FBSyxFQUFFOzBEQUE0QjtJQUUxQjtRQUFULE1BQU0sRUFBRTtzREFBNkI7SUFSM0Isa0JBQWtCO1FBTjlCLFNBQVMsQ0FBQztZQUNULFFBQVEsRUFBRSxZQUFZO1lBQ3RCLFFBQVEsRUFBRSx1QkFFVDtTQUNGLENBQUM7T0FDVyxrQkFBa0IsQ0FzSzlCO0lBQUQseUJBQUM7Q0FBQSxBQXRLRCxJQXNLQztTQXRLWSxrQkFBa0IiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQge1xuICBFbGVtZW50UmVmLFxuICBOZ1pvbmUsXG4gIENoYW5nZURldGVjdG9yUmVmLFxuICBDb21wb25lbnQsXG4gIElucHV0LFxuICBPdXRwdXQsXG4gIEV2ZW50RW1pdHRlcixcbiAgQWZ0ZXJWaWV3SW5pdCxcbiAgT25EZXN0cm95LFxuICBPbkNoYW5nZXMsXG4gIFNpbXBsZUNoYW5nZXNcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5cbmltcG9ydCB7IGZyb21FdmVudCBhcyBvYnNlcnZhYmxlRnJvbUV2ZW50IH0gZnJvbSAncnhqcyc7XG5pbXBvcnQgeyBkZWJvdW5jZVRpbWUgfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5pbXBvcnQgeyBWaXNpYmlsaXR5T2JzZXJ2ZXIgfSBmcm9tICcuLi91dGlscy92aXNpYmlsaXR5LW9ic2VydmVyJztcblxuQENvbXBvbmVudCh7XG4gIHNlbGVjdG9yOiAnYmFzZS1jaGFydCcsXG4gIHRlbXBsYXRlOiBgXG4gICAgPGRpdj48L2Rpdj5cbiAgYFxufSlcbmV4cG9ydCBjbGFzcyBCYXNlQ2hhcnRDb21wb25lbnQgaW1wbGVtZW50cyBPbkNoYW5nZXMsIEFmdGVyVmlld0luaXQsIE9uRGVzdHJveSB7XG4gIEBJbnB1dCgpIHJlc3VsdHM6IGFueTtcbiAgQElucHV0KCkgdmlldzogW251bWJlciwgbnVtYmVyXTtcbiAgQElucHV0KCkgc2NoZW1lOiBhbnkgPSAnY29vbCc7XG4gIEBJbnB1dCgpIHNjaGVtZVR5cGU6IHN0cmluZyA9ICdvcmRpbmFsJztcbiAgQElucHV0KCkgY3VzdG9tQ29sb3JzOiBhbnk7XG4gIEBJbnB1dCgpIGFuaW1hdGlvbnM6IGJvb2xlYW4gPSB0cnVlO1xuXG4gIEBPdXRwdXQoKSBzZWxlY3QgPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgd2lkdGg6IG51bWJlcjtcbiAgaGVpZ2h0OiBudW1iZXI7XG4gIHJlc2l6ZVN1YnNjcmlwdGlvbjogYW55O1xuICB2aXNpYmlsaXR5T2JzZXJ2ZXI6IFZpc2liaWxpdHlPYnNlcnZlcjtcblxuICBjb25zdHJ1Y3Rvcihwcm90ZWN0ZWQgY2hhcnRFbGVtZW50OiBFbGVtZW50UmVmLCBwcm90ZWN0ZWQgem9uZTogTmdab25lLCBwcm90ZWN0ZWQgY2Q6IENoYW5nZURldGVjdG9yUmVmKSB7fVxuXG4gIG5nQWZ0ZXJWaWV3SW5pdCgpOiB2b2lkIHtcbiAgICB0aGlzLmJpbmRXaW5kb3dSZXNpemVFdmVudCgpO1xuXG4gICAgLy8gbGlzdGVuIGZvciB2aXNpYmlsaXR5IG9mIHRoZSBlbGVtZW50IGZvciBoaWRkZW4gYnkgZGVmYXVsdCBzY2VuYXJpb1xuICAgIHRoaXMudmlzaWJpbGl0eU9ic2VydmVyID0gbmV3IFZpc2liaWxpdHlPYnNlcnZlcih0aGlzLmNoYXJ0RWxlbWVudCwgdGhpcy56b25lKTtcbiAgICB0aGlzLnZpc2liaWxpdHlPYnNlcnZlci52aXNpYmxlLnN1YnNjcmliZSh0aGlzLnVwZGF0ZS5iaW5kKHRoaXMpKTtcbiAgfVxuXG4gIG5nT25EZXN0cm95KCk6IHZvaWQge1xuICAgIHRoaXMudW5iaW5kRXZlbnRzKCk7XG4gICAgaWYgKHRoaXMudmlzaWJpbGl0eU9ic2VydmVyKSB7XG4gICAgICB0aGlzLnZpc2liaWxpdHlPYnNlcnZlci52aXNpYmxlLnVuc3Vic2NyaWJlKCk7XG4gICAgICB0aGlzLnZpc2liaWxpdHlPYnNlcnZlci5kZXN0cm95KCk7XG4gICAgfVxuICB9XG5cbiAgbmdPbkNoYW5nZXMoY2hhbmdlczogU2ltcGxlQ2hhbmdlcyk6IHZvaWQge1xuICAgIHRoaXMudXBkYXRlKCk7XG4gIH1cblxuICB1cGRhdGUoKTogdm9pZCB7XG4gICAgaWYgKHRoaXMucmVzdWx0cykge1xuICAgICAgdGhpcy5yZXN1bHRzID0gdGhpcy5jbG9uZURhdGEodGhpcy5yZXN1bHRzKTtcbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5yZXN1bHRzID0gW107XG4gICAgfVxuXG4gICAgaWYgKHRoaXMudmlldykge1xuICAgICAgdGhpcy53aWR0aCA9IHRoaXMudmlld1swXTtcbiAgICAgIHRoaXMuaGVpZ2h0ID0gdGhpcy52aWV3WzFdO1xuICAgIH0gZWxzZSB7XG4gICAgICBjb25zdCBkaW1zID0gdGhpcy5nZXRDb250YWluZXJEaW1zKCk7XG4gICAgICBpZiAoZGltcykge1xuICAgICAgICB0aGlzLndpZHRoID0gZGltcy53aWR0aDtcbiAgICAgICAgdGhpcy5oZWlnaHQgPSBkaW1zLmhlaWdodDtcbiAgICAgIH1cbiAgICB9XG5cbiAgICAvLyBkZWZhdWx0IHZhbHVlcyBpZiB3aWR0aCBvciBoZWlnaHQgYXJlIDAgb3IgdW5kZWZpbmVkXG4gICAgaWYgKCF0aGlzLndpZHRoKSB7XG4gICAgICB0aGlzLndpZHRoID0gNjAwO1xuICAgIH1cblxuICAgIGlmICghdGhpcy5oZWlnaHQpIHtcbiAgICAgIHRoaXMuaGVpZ2h0ID0gNDAwO1xuICAgIH1cblxuICAgIHRoaXMud2lkdGggPSBNYXRoLmZsb29yKHRoaXMud2lkdGgpO1xuICAgIHRoaXMuaGVpZ2h0ID0gTWF0aC5mbG9vcih0aGlzLmhlaWdodCk7XG5cbiAgICBpZiAodGhpcy5jZCkge1xuICAgICAgdGhpcy5jZC5tYXJrRm9yQ2hlY2soKTtcbiAgICB9XG4gIH1cblxuICBnZXRDb250YWluZXJEaW1zKCk6IGFueSB7XG4gICAgbGV0IHdpZHRoO1xuICAgIGxldCBoZWlnaHQ7XG4gICAgY29uc3QgaG9zdEVsZW0gPSB0aGlzLmNoYXJ0RWxlbWVudC5uYXRpdmVFbGVtZW50O1xuXG4gICAgaWYgKGhvc3RFbGVtLnBhcmVudE5vZGUgIT09IG51bGwpIHtcbiAgICAgIC8vIEdldCB0aGUgY29udGFpbmVyIGRpbWVuc2lvbnNcbiAgICAgIGNvbnN0IGRpbXMgPSBob3N0RWxlbS5wYXJlbnROb2RlLmdldEJvdW5kaW5nQ2xpZW50UmVjdCgpO1xuICAgICAgd2lkdGggPSBkaW1zLndpZHRoO1xuICAgICAgaGVpZ2h0ID0gZGltcy5oZWlnaHQ7XG4gICAgfVxuXG4gICAgaWYgKHdpZHRoICYmIGhlaWdodCkge1xuICAgICAgcmV0dXJuIHsgd2lkdGgsIGhlaWdodCB9O1xuICAgIH1cblxuICAgIHJldHVybiBudWxsO1xuICB9XG5cbiAgLyoqXG4gICAqIENvbnZlcnRzIGFsbCBkYXRlIG9iamVjdHMgdGhhdCBhcHBlYXIgYXMgbmFtZVxuICAgKiBpbnRvIGZvcm1hdHRlZCBkYXRlIHN0cmluZ3NcbiAgICovXG4gIGZvcm1hdERhdGVzKCk6IHZvaWQge1xuICAgIGZvciAobGV0IGkgPSAwOyBpIDwgdGhpcy5yZXN1bHRzLmxlbmd0aDsgaSsrKSB7XG4gICAgICBjb25zdCBnID0gdGhpcy5yZXN1bHRzW2ldO1xuICAgICAgZy5sYWJlbCA9IGcubmFtZTtcbiAgICAgIGlmIChnLmxhYmVsIGluc3RhbmNlb2YgRGF0ZSkge1xuICAgICAgICBnLmxhYmVsID0gZy5sYWJlbC50b0xvY2FsZURhdGVTdHJpbmcoKTtcbiAgICAgIH1cblxuICAgICAgaWYgKGcuc2VyaWVzKSB7XG4gICAgICAgIGZvciAobGV0IGogPSAwOyBqIDwgZy5zZXJpZXMubGVuZ3RoOyBqKyspIHtcbiAgICAgICAgICBjb25zdCBkID0gZy5zZXJpZXNbal07XG4gICAgICAgICAgZC5sYWJlbCA9IGQubmFtZTtcbiAgICAgICAgICBpZiAoZC5sYWJlbCBpbnN0YW5jZW9mIERhdGUpIHtcbiAgICAgICAgICAgIGQubGFiZWwgPSBkLmxhYmVsLnRvTG9jYWxlRGF0ZVN0cmluZygpO1xuICAgICAgICAgIH1cbiAgICAgICAgfVxuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIHByb3RlY3RlZCB1bmJpbmRFdmVudHMoKTogdm9pZCB7XG4gICAgaWYgKHRoaXMucmVzaXplU3Vic2NyaXB0aW9uKSB7XG4gICAgICB0aGlzLnJlc2l6ZVN1YnNjcmlwdGlvbi51bnN1YnNjcmliZSgpO1xuICAgIH1cbiAgfVxuXG4gIHByaXZhdGUgYmluZFdpbmRvd1Jlc2l6ZUV2ZW50KCk6IHZvaWQge1xuICAgIGNvbnN0IHNvdXJjZSA9IG9ic2VydmFibGVGcm9tRXZlbnQod2luZG93LCAncmVzaXplJyk7XG4gICAgY29uc3Qgc3Vic2NyaXB0aW9uID0gc291cmNlLnBpcGUoZGVib3VuY2VUaW1lKDIwMCkpLnN1YnNjcmliZShlID0+IHtcbiAgICAgIHRoaXMudXBkYXRlKCk7XG4gICAgICBpZiAodGhpcy5jZCkge1xuICAgICAgICB0aGlzLmNkLm1hcmtGb3JDaGVjaygpO1xuICAgICAgfVxuICAgIH0pO1xuICAgIHRoaXMucmVzaXplU3Vic2NyaXB0aW9uID0gc3Vic2NyaXB0aW9uO1xuICB9XG5cbiAgLyoqXG4gICAqIENsb25lcyB0aGUgZGF0YSBpbnRvIGEgbmV3IG9iamVjdFxuICAgKlxuICAgKiBAbWVtYmVyT2YgQmFzZUNoYXJ0XG4gICAqL1xuICBwcml2YXRlIGNsb25lRGF0YShkYXRhKTogYW55IHtcbiAgICBjb25zdCByZXN1bHRzID0gW107XG5cbiAgICBmb3IgKGNvbnN0IGl0ZW0gb2YgZGF0YSkge1xuICAgICAgY29uc3QgY29weSA9IHtcbiAgICAgICAgbmFtZTogaXRlbVsnbmFtZSddXG4gICAgICB9O1xuXG4gICAgICBpZiAoaXRlbVsndmFsdWUnXSAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICAgIGNvcHlbJ3ZhbHVlJ10gPSBpdGVtWyd2YWx1ZSddO1xuICAgICAgfVxuXG4gICAgICBpZiAoaXRlbVsnc2VyaWVzJ10gIT09IHVuZGVmaW5lZCkge1xuICAgICAgICBjb3B5WydzZXJpZXMnXSA9IFtdO1xuICAgICAgICBmb3IgKGNvbnN0IHNlcmllc0l0ZW0gb2YgaXRlbVsnc2VyaWVzJ10pIHtcbiAgICAgICAgICBjb25zdCBzZXJpZXNJdGVtQ29weSA9IE9iamVjdC5hc3NpZ24oe30sIHNlcmllc0l0ZW0pO1xuICAgICAgICAgIGNvcHlbJ3NlcmllcyddLnB1c2goc2VyaWVzSXRlbUNvcHkpO1xuICAgICAgICB9XG4gICAgICB9XG5cbiAgICAgIGlmIChpdGVtWydleHRyYSddICE9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgY29weVsnZXh0cmEnXSA9IEpTT04ucGFyc2UoSlNPTi5zdHJpbmdpZnkoaXRlbVsnZXh0cmEnXSkpO1xuICAgICAgfVxuXG4gICAgICByZXN1bHRzLnB1c2goY29weSk7XG4gICAgfVxuXG4gICAgcmV0dXJuIHJlc3VsdHM7XG4gIH1cbn1cbiJdfQ==