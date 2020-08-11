import { __assign, __decorate, __extends, __read, __spread, __values } from "tslib";
import { Component, Input, Output, EventEmitter, ViewEncapsulation, ChangeDetectionStrategy, ContentChild } from '@angular/core';
import { trigger, style, animate, transition } from '@angular/animations';
import { scaleLinear, scaleTime, scalePoint } from 'd3-scale';
import { curveCardinalClosed } from 'd3-shape';
import { calculateViewDimensions } from '../common/view-dimensions.helper';
import { ColorHelper } from '../common/color.helper';
import { BaseChartComponent } from '../common/base-chart.component';
import { getScaleType } from '../common/domain.helper';
import { isDate } from '../utils/types';
var twoPI = 2 * Math.PI;
var PolarChartComponent = /** @class */ (function (_super) {
    __extends(PolarChartComponent, _super);
    function PolarChartComponent() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.legendTitle = 'Legend';
        _this.legendPosition = 'right';
        _this.showGridLines = true;
        _this.curve = curveCardinalClosed;
        _this.activeEntries = [];
        _this.rangeFillOpacity = 0.15;
        _this.trimYAxisTicks = true;
        _this.maxYAxisTickLength = 16;
        _this.roundDomains = false;
        _this.tooltipDisabled = false;
        _this.showSeriesOnHover = true;
        _this.gradient = false;
        _this.yAxisMinScale = 0;
        _this.labelTrim = true;
        _this.labelTrimSize = 10;
        _this.activate = new EventEmitter();
        _this.deactivate = new EventEmitter();
        _this.margin = [10, 20, 10, 20];
        _this.xAxisHeight = 0;
        _this.yAxisWidth = 0;
        return _this;
    }
    PolarChartComponent.prototype.update = function () {
        _super.prototype.update.call(this);
        this.setDims();
        this.setScales();
        this.setColors();
        this.legendOptions = this.getLegendOptions();
        this.setTicks();
    };
    PolarChartComponent.prototype.setDims = function () {
        this.dims = calculateViewDimensions({
            width: this.width,
            height: this.height,
            margins: this.margin,
            showXAxis: this.xAxis,
            showYAxis: this.yAxis,
            xAxisHeight: this.xAxisHeight,
            yAxisWidth: this.yAxisWidth,
            showXLabel: this.showXAxisLabel,
            showYLabel: this.showYAxisLabel,
            showLegend: this.legend,
            legendType: this.schemeType,
            legendPosition: this.legendPosition
        });
        var halfWidth = Math.floor(this.dims.width / 2);
        var halfHeight = Math.floor(this.dims.height / 2);
        var outerRadius = (this.outerRadius = Math.min(halfHeight / 1.5, halfWidth / 1.5));
        var yOffset = Math.max(0, halfHeight - outerRadius);
        this.yAxisDims = __assign(__assign({}, this.dims), { width: halfWidth });
        this.transform = "translate(" + this.dims.xOffset + ", " + this.margin[0] + ")";
        this.transformYAxis = "translate(0, " + yOffset + ")";
        this.labelOffset = this.dims.height + 40;
        this.transformPlot = "translate(" + halfWidth + ", " + halfHeight + ")";
    };
    PolarChartComponent.prototype.setScales = function () {
        var xValues = this.getXValues();
        this.scaleType = getScaleType(xValues);
        this.xDomain = this.filteredDomain || this.getXDomain(xValues);
        this.yDomain = this.getYDomain();
        this.seriesDomain = this.getSeriesDomain();
        this.xScale = this.getXScale(this.xDomain, twoPI);
        this.yScale = this.getYScale(this.yDomain, this.outerRadius);
        this.yAxisScale = this.getYScale(this.yDomain.reverse(), this.outerRadius);
    };
    PolarChartComponent.prototype.setTicks = function () {
        var _this = this;
        var tickFormat;
        if (this.xAxisTickFormatting) {
            tickFormat = this.xAxisTickFormatting;
        }
        else if (this.xScale.tickFormat) {
            tickFormat = this.xScale.tickFormat.apply(this.xScale, [5]);
        }
        else {
            tickFormat = function (d) {
                if (isDate(d)) {
                    return d.toLocaleDateString();
                }
                return d.toLocaleString();
            };
        }
        var outerRadius = this.outerRadius;
        var s = 1.1;
        this.thetaTicks = this.xDomain.map(function (d) {
            var startAngle = _this.xScale(d);
            var dd = s * outerRadius * (startAngle > Math.PI ? -1 : 1);
            var label = tickFormat(d);
            var startPos = [outerRadius * Math.sin(startAngle), -outerRadius * Math.cos(startAngle)];
            var pos = [dd, s * startPos[1]];
            return {
                innerRadius: 0,
                outerRadius: outerRadius,
                startAngle: startAngle,
                endAngle: startAngle,
                value: outerRadius,
                label: label,
                startPos: startPos,
                pos: pos
            };
        });
        var minDistance = 10;
        /* from pie chart, abstract out -*/
        for (var i = 0; i < this.thetaTicks.length - 1; i++) {
            var a = this.thetaTicks[i];
            for (var j = i + 1; j < this.thetaTicks.length; j++) {
                var b = this.thetaTicks[j];
                // if they're on the same side
                if (b.pos[0] * a.pos[0] > 0) {
                    // if they're overlapping
                    var o = minDistance - Math.abs(b.pos[1] - a.pos[1]);
                    if (o > 0) {
                        // push the second up or down
                        b.pos[1] += Math.sign(b.pos[0]) * o;
                    }
                }
            }
        }
        this.radiusTicks = this.yAxisScale.ticks(Math.floor(this.dims.height / 50)).map(function (d) { return _this.yScale(d); });
    };
    PolarChartComponent.prototype.getXValues = function () {
        var e_1, _a, e_2, _b;
        var values = [];
        try {
            for (var _c = __values(this.results), _d = _c.next(); !_d.done; _d = _c.next()) {
                var results = _d.value;
                try {
                    for (var _e = (e_2 = void 0, __values(results.series)), _f = _e.next(); !_f.done; _f = _e.next()) {
                        var d = _f.value;
                        if (!values.includes(d.name)) {
                            values.push(d.name);
                        }
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return values;
    };
    PolarChartComponent.prototype.getXDomain = function (values) {
        if (values === void 0) { values = this.getXValues(); }
        if (this.scaleType === 'time') {
            var min = Math.min.apply(Math, __spread(values));
            var max = Math.max.apply(Math, __spread(values));
            return [min, max];
        }
        else if (this.scaleType === 'linear') {
            values = values.map(function (v) { return Number(v); });
            var min = Math.min.apply(Math, __spread(values));
            var max = Math.max.apply(Math, __spread(values));
            return [min, max];
        }
        return values;
    };
    PolarChartComponent.prototype.getYValues = function () {
        var e_3, _a, e_4, _b;
        var domain = [];
        try {
            for (var _c = __values(this.results), _d = _c.next(); !_d.done; _d = _c.next()) {
                var results = _d.value;
                try {
                    for (var _e = (e_4 = void 0, __values(results.series)), _f = _e.next(); !_f.done; _f = _e.next()) {
                        var d = _f.value;
                        if (domain.indexOf(d.value) < 0) {
                            domain.push(d.value);
                        }
                        if (d.min !== undefined) {
                            if (domain.indexOf(d.min) < 0) {
                                domain.push(d.min);
                            }
                        }
                        if (d.max !== undefined) {
                            if (domain.indexOf(d.max) < 0) {
                                domain.push(d.max);
                            }
                        }
                    }
                }
                catch (e_4_1) { e_4 = { error: e_4_1 }; }
                finally {
                    try {
                        if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
                    }
                    finally { if (e_4) throw e_4.error; }
                }
            }
        }
        catch (e_3_1) { e_3 = { error: e_3_1 }; }
        finally {
            try {
                if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
            }
            finally { if (e_3) throw e_3.error; }
        }
        return domain;
    };
    PolarChartComponent.prototype.getYDomain = function (domain) {
        if (domain === void 0) { domain = this.getYValues(); }
        var min = Math.min.apply(Math, __spread(domain));
        var max = Math.max.apply(Math, __spread([this.yAxisMinScale], domain));
        min = Math.max(0, min);
        if (!this.autoScale) {
            min = Math.min(0, min);
        }
        return [min, max];
    };
    PolarChartComponent.prototype.getSeriesDomain = function () {
        return this.results.map(function (d) { return d.name; });
    };
    PolarChartComponent.prototype.getXScale = function (domain, width) {
        switch (this.scaleType) {
            case 'time':
                return scaleTime()
                    .range([0, width])
                    .domain(domain);
            case 'linear':
                var scale = scaleLinear()
                    .range([0, width])
                    .domain(domain);
                return this.roundDomains ? scale.nice() : scale;
            default:
                return scalePoint()
                    .range([0, width - twoPI / domain.length])
                    .padding(0)
                    .domain(domain);
        }
    };
    PolarChartComponent.prototype.getYScale = function (domain, height) {
        var scale = scaleLinear()
            .range([0, height])
            .domain(domain);
        return this.roundDomains ? scale.nice() : scale;
    };
    PolarChartComponent.prototype.onClick = function (data, series) {
        if (series) {
            data.series = series.name;
        }
        this.select.emit(data);
    };
    PolarChartComponent.prototype.setColors = function () {
        var domain = this.schemeType === 'ordinal' ? this.seriesDomain : this.yDomain.reverse();
        this.colors = new ColorHelper(this.scheme, this.schemeType, domain, this.customColors);
    };
    PolarChartComponent.prototype.getLegendOptions = function () {
        if (this.schemeType === 'ordinal') {
            return {
                scaleType: this.schemeType,
                colors: this.colors,
                domain: this.seriesDomain,
                title: this.legendTitle,
                position: this.legendPosition
            };
        }
        return {
            scaleType: this.schemeType,
            colors: this.colors.scale,
            domain: this.yDomain,
            title: undefined,
            position: this.legendPosition
        };
    };
    PolarChartComponent.prototype.updateYAxisWidth = function (_a) {
        var width = _a.width;
        this.yAxisWidth = width;
        this.update();
    };
    PolarChartComponent.prototype.updateXAxisHeight = function (_a) {
        var height = _a.height;
        this.xAxisHeight = height;
        this.update();
    };
    PolarChartComponent.prototype.onActivate = function (item) {
        var idx = this.activeEntries.findIndex(function (d) {
            return d.name === item.name && d.value === item.value;
        });
        if (idx > -1) {
            return;
        }
        this.activeEntries = this.showSeriesOnHover ? __spread([item], this.activeEntries) : this.activeEntries;
        this.activate.emit({ value: item, entries: this.activeEntries });
    };
    PolarChartComponent.prototype.onDeactivate = function (item) {
        var idx = this.activeEntries.findIndex(function (d) {
            return d.name === item.name && d.value === item.value;
        });
        this.activeEntries.splice(idx, 1);
        this.activeEntries = __spread(this.activeEntries);
        this.deactivate.emit({ value: item, entries: this.activeEntries });
    };
    PolarChartComponent.prototype.deactivateAll = function () {
        var e_5, _a;
        this.activeEntries = __spread(this.activeEntries);
        try {
            for (var _b = __values(this.activeEntries), _c = _b.next(); !_c.done; _c = _b.next()) {
                var entry = _c.value;
                this.deactivate.emit({ value: entry, entries: [] });
            }
        }
        catch (e_5_1) { e_5 = { error: e_5_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_5) throw e_5.error; }
        }
        this.activeEntries = [];
    };
    PolarChartComponent.prototype.trackBy = function (index, item) {
        return item.name;
    };
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "legend", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "legendTitle", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "legendPosition", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "xAxis", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "yAxis", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "showXAxisLabel", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "showYAxisLabel", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "xAxisLabel", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "yAxisLabel", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "autoScale", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "showGridLines", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "curve", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "activeEntries", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "schemeType", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "rangeFillOpacity", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "trimYAxisTicks", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "maxYAxisTickLength", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "xAxisTickFormatting", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "yAxisTickFormatting", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "roundDomains", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "tooltipDisabled", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "showSeriesOnHover", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "gradient", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "yAxisMinScale", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "labelTrim", void 0);
    __decorate([
        Input()
    ], PolarChartComponent.prototype, "labelTrimSize", void 0);
    __decorate([
        Output()
    ], PolarChartComponent.prototype, "activate", void 0);
    __decorate([
        Output()
    ], PolarChartComponent.prototype, "deactivate", void 0);
    __decorate([
        ContentChild('tooltipTemplate')
    ], PolarChartComponent.prototype, "tooltipTemplate", void 0);
    PolarChartComponent = __decorate([
        Component({
            selector: 'ngx-charts-polar-chart',
            template: "\n    <ngx-charts-chart\n      [view]=\"[width, height]\"\n      [showLegend]=\"legend\"\n      [legendOptions]=\"legendOptions\"\n      [activeEntries]=\"activeEntries\"\n      [animations]=\"animations\"\n      (legendLabelClick)=\"onClick($event)\"\n      (legendLabelActivate)=\"onActivate($event)\"\n      (legendLabelDeactivate)=\"onDeactivate($event)\"\n    >\n      <svg:g class=\"polar-chart chart\" [attr.transform]=\"transform\">\n        <svg:g [attr.transform]=\"transformPlot\">\n          <svg:circle class=\"polar-chart-background\" cx=\"0\" cy=\"0\" [attr.r]=\"this.outerRadius\" />\n          <svg:g *ngIf=\"showGridLines\">\n            <svg:circle\n              *ngFor=\"let r of radiusTicks\"\n              class=\"gridline-path radial-gridline-path\"\n              cx=\"0\"\n              cy=\"0\"\n              [attr.r]=\"r\"\n            />\n          </svg:g>\n          <svg:g *ngIf=\"xAxis\">\n            <svg:g\n              ngx-charts-pie-label\n              *ngFor=\"let tick of thetaTicks\"\n              [data]=\"tick\"\n              [radius]=\"outerRadius\"\n              [label]=\"tick.label\"\n              [max]=\"outerRadius\"\n              [value]=\"showGridLines ? 1 : outerRadius\"\n              [explodeSlices]=\"true\"\n              [animations]=\"animations\"\n              [labelTrim]=\"labelTrim\"\n              [labelTrimSize]=\"labelTrimSize\"\n            ></svg:g>\n          </svg:g>\n        </svg:g>\n        <svg:g\n          ngx-charts-y-axis\n          [attr.transform]=\"transformYAxis\"\n          *ngIf=\"yAxis\"\n          [yScale]=\"yAxisScale\"\n          [dims]=\"yAxisDims\"\n          [showGridLines]=\"showGridLines\"\n          [showLabel]=\"showYAxisLabel\"\n          [labelText]=\"yAxisLabel\"\n          [trimTicks]=\"trimYAxisTicks\"\n          [maxTickLength]=\"maxYAxisTickLength\"\n          [tickFormatting]=\"yAxisTickFormatting\"\n          (dimensionsChanged)=\"updateYAxisWidth($event)\"\n        ></svg:g>\n        <svg:g\n          ngx-charts-axis-label\n          *ngIf=\"xAxis && showXAxisLabel\"\n          [label]=\"xAxisLabel\"\n          [offset]=\"labelOffset\"\n          [orient]=\"'bottom'\"\n          [height]=\"dims.height\"\n          [width]=\"dims.width\"\n        ></svg:g>\n        <svg:g [attr.transform]=\"transformPlot\">\n          <svg:g *ngFor=\"let series of results; trackBy: trackBy\" [@animationState]=\"'active'\">\n            <svg:g\n              ngx-charts-polar-series\n              [gradient]=\"gradient\"\n              [xScale]=\"xScale\"\n              [yScale]=\"yScale\"\n              [colors]=\"colors\"\n              [data]=\"series\"\n              [activeEntries]=\"activeEntries\"\n              [scaleType]=\"scaleType\"\n              [curve]=\"curve\"\n              [rangeFillOpacity]=\"rangeFillOpacity\"\n              [animations]=\"animations\"\n              [tooltipDisabled]=\"tooltipDisabled\"\n              [tooltipTemplate]=\"tooltipTemplate\"\n              (select)=\"onClick($event)\"\n              (activate)=\"onActivate($event)\"\n              (deactivate)=\"onDeactivate($event)\"\n            />\n          </svg:g>\n        </svg:g>\n      </svg:g>\n    </ngx-charts-chart>\n  ",
            encapsulation: ViewEncapsulation.None,
            changeDetection: ChangeDetectionStrategy.OnPush,
            animations: [
                trigger('animationState', [
                    transition(':leave', [
                        style({
                            opacity: 1
                        }),
                        animate(500, style({
                            opacity: 0
                        }))
                    ])
                ])
            ],
            styles: [".ngx-charts{float:left;overflow:visible}.ngx-charts .arc,.ngx-charts .bar,.ngx-charts .circle{cursor:pointer}.ngx-charts .arc.active,.ngx-charts .arc:hover,.ngx-charts .bar.active,.ngx-charts .bar:hover,.ngx-charts .card.active,.ngx-charts .card:hover,.ngx-charts .cell.active,.ngx-charts .cell:hover{opacity:.8;-webkit-transition:opacity .1s ease-in-out;transition:opacity .1s ease-in-out}.ngx-charts .arc:focus,.ngx-charts .bar:focus,.ngx-charts .card:focus,.ngx-charts .cell:focus{outline:0}.ngx-charts .arc.hidden,.ngx-charts .bar.hidden,.ngx-charts .card.hidden,.ngx-charts .cell.hidden{display:none}.ngx-charts g:focus{outline:0}.ngx-charts .area-series.inactive,.ngx-charts .line-series-range.inactive,.ngx-charts .line-series.inactive,.ngx-charts .polar-series-area.inactive,.ngx-charts .polar-series-path.inactive{-webkit-transition:opacity .1s ease-in-out;transition:opacity .1s ease-in-out;opacity:.2}.ngx-charts .line-highlight{display:none}.ngx-charts .line-highlight.active{display:block}.ngx-charts .area{opacity:.6}.ngx-charts .circle:hover{cursor:pointer}.ngx-charts .label{font-size:12px;font-weight:400}.ngx-charts .tooltip-anchor{fill:#000}.ngx-charts .gridline-path{stroke:#ddd;stroke-width:1;fill:none}.ngx-charts .refline-path{stroke:#a8b2c7;stroke-width:1;stroke-dasharray:5;stroke-dashoffset:5}.ngx-charts .refline-label{font-size:9px}.ngx-charts .reference-area{fill-opacity:.05;fill:#000}.ngx-charts .gridline-path-dotted{stroke:#ddd;stroke-width:1;fill:none;stroke-dasharray:1,20;stroke-dashoffset:3}.ngx-charts .grid-panel rect{fill:none}.ngx-charts .grid-panel.odd rect{fill:rgba(0,0,0,.05)}", ".pie-label{font-size:11px}.pie-label.animation{-webkit-animation:750ms ease-in fadeIn;animation:750ms ease-in fadeIn}@-webkit-keyframes fadeIn{from{opacity:0}to{opacity:1}}@keyframes fadeIn{from{opacity:0}to{opacity:1}}.pie-label-line{stroke-dasharray:100%}.pie-label-line.animation{-webkit-animation:3s linear drawOut;animation:3s linear drawOut;-webkit-transition:d 750ms;transition:d 750ms}@-webkit-keyframes drawOut{from{stroke-dashoffset:100%}to{stroke-dashoffset:0}}@keyframes drawOut{from{stroke-dashoffset:100%}to{stroke-dashoffset:0}}", ".polar-chart .polar-chart-background{fill:none}.polar-chart .radial-gridline-path{stroke-dasharray:10 10;fill:none}.polar-chart .pie-label-line{stroke:#2f3646}.polar-charts-series .polar-series-area,.polar-series-path{pointer-events:none}"]
        })
    ], PolarChartComponent);
    return PolarChartComponent;
}(BaseChartComponent));
export { PolarChartComponent };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicG9sYXItY2hhcnQuY29tcG9uZW50LmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvcG9sYXItY2hhcnQvcG9sYXItY2hhcnQuY29tcG9uZW50LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxPQUFPLEVBQ0wsU0FBUyxFQUNULEtBQUssRUFDTCxNQUFNLEVBQ04sWUFBWSxFQUNaLGlCQUFpQixFQUNqQix1QkFBdUIsRUFDdkIsWUFBWSxFQUViLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBRSxPQUFPLEVBQUUsS0FBSyxFQUFFLE9BQU8sRUFBRSxVQUFVLEVBQUUsTUFBTSxxQkFBcUIsQ0FBQztBQUMxRSxPQUFPLEVBQUUsV0FBVyxFQUFFLFNBQVMsRUFBRSxVQUFVLEVBQUUsTUFBTSxVQUFVLENBQUM7QUFDOUQsT0FBTyxFQUFFLG1CQUFtQixFQUFFLE1BQU0sVUFBVSxDQUFDO0FBRS9DLE9BQU8sRUFBRSx1QkFBdUIsRUFBa0IsTUFBTSxrQ0FBa0MsQ0FBQztBQUMzRixPQUFPLEVBQUUsV0FBVyxFQUFFLE1BQU0sd0JBQXdCLENBQUM7QUFDckQsT0FBTyxFQUFFLGtCQUFrQixFQUFFLE1BQU0sZ0NBQWdDLENBQUM7QUFDcEUsT0FBTyxFQUFFLFlBQVksRUFBRSxNQUFNLHlCQUF5QixDQUFDO0FBQ3ZELE9BQU8sRUFBRSxNQUFNLEVBQUUsTUFBTSxnQkFBZ0IsQ0FBQztBQUV4QyxJQUFNLEtBQUssR0FBRyxDQUFDLEdBQUcsSUFBSSxDQUFDLEVBQUUsQ0FBQztBQWtIMUI7SUFBeUMsdUNBQWtCO0lBQTNEO1FBQUEscUVBd1ZDO1FBdFZVLGlCQUFXLEdBQVcsUUFBUSxDQUFDO1FBQy9CLG9CQUFjLEdBQVcsT0FBTyxDQUFDO1FBUWpDLG1CQUFhLEdBQVksSUFBSSxDQUFDO1FBQzlCLFdBQUssR0FBUSxtQkFBbUIsQ0FBQztRQUNqQyxtQkFBYSxHQUFVLEVBQUUsQ0FBQztRQUUx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QUU7eURBQTJCO0lBQzFCO1FBQVIsS0FBSyxFQUFFOzhEQUEyQjtJQUMxQjtRQUFSLEtBQUssRUFBRTswREFBMkI7SUFDMUI7UUFBUixLQUFLLEVBQUU7OERBQTRCO0lBRTFCO1FBQVQsTUFBTSxFQUFFO3lEQUFrRDtJQUNqRDtRQUFULE1BQU0sRUFBRTsyREFBb0Q7SUFFNUI7UUFBaEMsWUFBWSxDQUFDLGlCQUFpQixDQUFDO2dFQUFtQztJQS9CeEQsbUJBQW1CO1FBaEgvQixTQUFTLENBQUM7WUFDVCxRQUFRLEVBQUUsd0JBQXdCO1lBQ2xDLFFBQVEsRUFBRSxrckdBc0ZUO1lBTUQsYUFBYSxFQUFFLGlCQUFpQixDQUFDLElBQUk7WUFDckMsZUFBZSxFQUFFLHVCQUF1QixDQUFDLE1BQU07WUFDL0MsVUFBVSxFQUFFO2dCQUNWLE9BQU8sQ0FBQyxnQkFBZ0IsRUFBRTtvQkFDeEIsVUFBVSxDQUFDLFFBQVEsRUFBRTt3QkFDbkIsS0FBSyxDQUFDOzRCQUNKLE9BQU8sRUFBRSxDQUFDO3lCQUNYLENBQUM7d0JBQ0YsT0FBTyxDQUNMLEdBQUcsRUFDSCxLQUFLLENBQUM7NEJBQ0osT0FBTyxFQUFFLENBQUM7eUJBQ1gsQ0FBQyxDQUNIO3FCQUNGLENBQUM7aUJBQ0gsQ0FBQzthQUNIOztTQUNGLENBQUM7T0FDVyxtQkFBbUIsQ0F3Vi9CO0lBQUQsMEJBQUM7Q0FBQSxBQXhWRCxDQUF5QyxrQkFBa0IsR0F3VjFEO1NBeFZZLG1CQUFtQiIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIENvbXBvbmVudCxcbiAgSW5wdXQsXG4gIE91dHB1dCxcbiAgRXZlbnRFbWl0dGVyLFxuICBWaWV3RW5jYXBzdWxhdGlvbixcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksXG4gIENvbnRlbnRDaGlsZCxcbiAgVGVtcGxhdGVSZWZcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgeyB0cmlnZ2VyLCBzdHlsZSwgYW5pbWF0ZSwgdHJhbnNpdGlvbiB9IGZyb20gJ0Bhbmd1bGFyL2FuaW1hdGlvbnMnO1xuaW1wb3J0IHsgc2NhbGVMaW5lYXIsIHNjYWxlVGltZSwgc2NhbGVQb2ludCB9IGZyb20gJ2QzLXNjYWxlJztcbmltcG9ydCB7IGN1cnZlQ2FyZGluYWxDbG9zZWQgfSBmcm9tICdkMy1zaGFwZSc7XG5cbmltcG9ydCB7IGNhbGN1bGF0ZVZpZXdEaW1lbnNpb25zLCBWaWV3RGltZW5zaW9ucyB9IGZyb20gJy4uL2NvbW1vbi92aWV3LWRpbWVuc2lvbnMuaGVscGVyJztcbmltcG9ydCB7IENvbG9ySGVscGVyIH0gZnJvbSAnLi4vY29tbW9uL2NvbG9yLmhlbHBlcic7XG5pbXBvcnQgeyBCYXNlQ2hhcnRDb21wb25lbnQgfSBmcm9tICcuLi9jb21tb24vYmFzZS1jaGFydC5jb21wb25lbnQnO1xuaW1wb3J0IHsgZ2V0U2NhbGVUeXBlIH0gZnJvbSAnLi4vY29tbW9uL2RvbWFpbi5oZWxwZXInO1xuaW1wb3J0IHsgaXNEYXRlIH0gZnJvbSAnLi4vdXRpbHMvdHlwZXMnO1xuXG5jb25zdCB0d29QSSA9IDIgKiBNYXRoLlBJO1xuXG5AQ29tcG9uZW50KHtcbiAgc2VsZWN0b3I6ICduZ3gtY2hhcnRzLXBvbGFyLWNoYXJ0JyxcbiAgdGVtcGxhdGU6IGBcbiAgICA8bmd4LWNoYXJ0cy1jaGFydFxuICAgICAgW3ZpZXddPVwiW3dpZHRoLCBoZWlnaHRdXCJcbiAgICAgIFtzaG93TGVnZW5kXT1cImxlZ2VuZFwiXG4gICAgICBbbGVnZW5kT3B0aW9uc109XCJsZWdlbmRPcHRpb25zXCJcbiAgICAgIFthY3RpdmVFbnRyaWVzXT1cImFjdGl2ZUVudHJpZXNcIlxuICAgICAgW2FuaW1hdGlvbnNdPVwiYW5pbWF0aW9uc1wiXG4gICAgICAobGVnZW5kTGFiZWxDbGljayk9XCJvbkNsaWNrKCRldmVudClcIlxuICAgICAgKGxlZ2VuZExhYmVsQWN0aXZhdGUpPVwib25BY3RpdmF0ZSgkZXZlbnQpXCJcbiAgICAgIChsZWdlbmRMYWJlbERlYWN0aXZhdGUpPVwib25EZWFjdGl2YXRlKCRldmVudClcIlxuICAgID5cbiAgICAgIDxzdmc6ZyBjbGFzcz1cInBvbGFyLWNoYXJ0IGNoYXJ0XCIgW2F0dHIudHJhbnNmb3JtXT1cInRyYW5zZm9ybVwiPlxuICAgICAgICA8c3ZnOmcgW2F0dHIudHJhbnNmb3JtXT1cInRyYW5zZm9ybVBsb3RcIj5cbiAgICAgICAgICA8c3ZnOmNpcmNsZSBjbGFzcz1cInBvbGFyLWNoYXJ0LWJhY2tncm91bmRcIiBjeD1cIjBcIiBjeT1cIjBcIiBbYXR0ci5yXT1cInRoaXMub3V0ZXJSYWRpdXNcIiAvPlxuICAgICAgICAgIDxzdmc6ZyAqbmdJZj1cInNob3dHcmlkTGluZXNcIj5cbiAgICAgICAgICAgIDxzdmc6Y2lyY2xlXG4gICAgICAgICAgICAgICpuZ0Zvcj1cImxldCByIG9mIHJhZGl1c1RpY2tzXCJcbiAgICAgICAgICAgICAgY2xhc3M9XCJncmlkbGluZS1wYXRoIHJhZGlhbC1ncmlkbGluZS1wYXRoXCJcbiAgICAgICAgICAgICAgY3g9XCIwXCJcbiAgICAgICAgICAgICAgY3k9XCIwXCJcbiAgICAgICAgICAgICAgW2F0dHIucl09XCJyXCJcbiAgICAgICAgICAgIC8+XG4gICAgICAgICAgPC9zdmc6Zz5cbiAgICAgICAgICA8c3ZnOmcgKm5nSWY9XCJ4QXhpc1wiPlxuICAgICAgICAgICAgPHN2ZzpnXG4gICAgICAgICAgICAgIG5neC1jaGFydHMtcGllLWxhYmVsXG4gICAgICAgICAgICAgICpuZ0Zvcj1cImxldCB0aWNrIG9mIHRoZXRhVGlja3NcIlxuICAgICAgICAgICAgICBbZGF0YV09XCJ0aWNrXCJcbiAgICAgICAgICAgICAgW3JhZGl1c109XCJvdXRlclJhZGl1c1wiXG4gICAgICAgICAgICAgIFtsYWJlbF09XCJ0aWNrLmxhYmVsXCJcbiAgICAgICAgICAgICAgW21heF09XCJvdXRlclJhZGl1c1wiXG4gICAgICAgICAgICAgIFt2YWx1ZV09XCJzaG93R3JpZExpbmVzID8gMSA6IG91dGVyUmFkaXVzXCJcbiAgICAgICAgICAgICAgW2V4cGxvZGVTbGljZXNdPVwidHJ1ZVwiXG4gICAgICAgICAgICAgIFthbmltYXRpb25zXT1cImFuaW1hdGlvbnNcIlxuICAgICAgICAgICAgICBbbGFiZWxUcmltXT1cImxhYmVsVHJpbVwiXG4gICAgICAgICAgICAgIFtsYWJlbFRyaW1TaXplXT1cImxhYmVsVHJpbVNpemVcIlxuICAgICAgICAgICAgPjwvc3ZnOmc+XG4gICAgICAgICAgPC9zdmc6Zz5cbiAgICAgICAgPC9zdmc6Zz5cbiAgICAgICAgPHN2ZzpnXG4gICAgICAgICAgbmd4LWNoYXJ0cy15LWF4aXNcbiAgICAgICAgICBbYXR0ci50cmFuc2Zvcm1dPVwidHJhbnNmb3JtWUF4aXNcIlxuICAgICAgICAgICpuZ0lmPVwieUF4aXNcIlxuICAgICAgICAgIFt5U2NhbGVdPVwieUF4aXNTY2FsZVwiXG4gICAgICAgICAgW2RpbXNdPVwieUF4aXNEaW1zXCJcbiAgICAgICAgICBbc2hvd0dyaWRMaW5lc109XCJzaG93R3JpZExpbmVzXCJcbiAgICAgICAgICBbc2hvd0xhYmVsXT1cInNob3dZQXhpc0xhYmVsXCJcbiAgICAgICAgICBbbGFiZWxUZXh0XT1cInlBeGlzTGFiZWxcIlxuICAgICAgICAgIFt0cmltVGlja3NdPVwidHJpbVlBeGlzVGlja3NcIlxuICAgICAgICAgIFttYXhUaWNrTGVuZ3RoXT1cIm1heFlBeGlzVGlja0xlbmd0aFwiXG4gICAgICAgICAgW3RpY2tGb3JtYXR0aW5nXT1cInlBeGlzVGlja0Zvcm1hdHRpbmdcIlxuICAgICAgICAgIChkaW1lbnNpb25zQ2hhbmdlZCk9XCJ1cGRhdGVZQXhpc1dpZHRoKCRldmVudClcIlxuICAgICAgICA+PC9zdmc6Zz5cbiAgICAgICAgPHN2ZzpnXG4gICAgICAgICAgbmd4LWNoYXJ0cy1heGlzLWxhYmVsXG4gICAgICAgICAgKm5nSWY9XCJ4QXhpcyAmJiBzaG93WEF4aXNMYWJlbFwiXG4gICAgICAgICAgW2xhYmVsXT1cInhBeGlzTGFiZWxcIlxuICAgICAgICAgIFtvZmZzZXRdPVwibGFiZWxPZmZzZXRcIlxuICAgICAgICAgIFtvcmllbnRdPVwiJ2JvdHRvbSdcIlxuICAgICAgICAgIFtoZWlnaHRdPVwiZGltcy5oZWlnaHRcIlxuICAgICAgICAgIFt3aWR0aF09XCJkaW1zLndpZHRoXCJcbiAgICAgICAgPjwvc3ZnOmc+XG4gICAgICAgIDxzdmc6ZyBbYXR0ci50cmFuc2Zvcm1dPVwidHJhbnNmb3JtUGxvdFwiPlxuICAgICAgICAgIDxzdmc6ZyAqbmdGb3I9XCJsZXQgc2VyaWVzIG9mIHJlc3VsdHM7IHRyYWNrQnk6IHRyYWNrQnlcIiBbQGFuaW1hdGlvblN0YXRlXT1cIidhY3RpdmUnXCI+XG4gICAgICAgICAgICA8c3ZnOmdcbiAgICAgICAgICAgICAgbmd4LWNoYXJ0cy1wb2xhci1zZXJpZXNcbiAgICAgICAgICAgICAgW2dyYWRpZW50XT1cImdyYWRpZW50XCJcbiAgICAgICAgICAgICAgW3hTY2FsZV09XCJ4U2NhbGVcIlxuICAgICAgICAgICAgICBbeVNjYWxlXT1cInlTY2FsZVwiXG4gICAgICAgICAgICAgIFtjb2xvcnNdPVwiY29sb3JzXCJcbiAgICAgICAgICAgICAgW2RhdGFdPVwic2VyaWVzXCJcbiAgICAgICAgICAgICAgW2FjdGl2ZUVudHJpZXNdPVwiYWN0aXZlRW50cmllc1wiXG4gICAgICAgICAgICAgIFtzY2FsZVR5cGVdPVwic2NhbGVUeXBlXCJcbiAgICAgICAgICAgICAgW2N1cnZlXT1cImN1cnZlXCJcbiAgICAgICAgICAgICAgW3JhbmdlRmlsbE9wYWNpdHldPVwicmFuZ2VGaWxsT3BhY2l0eVwiXG4gICAgICAgICAgICAgIFthbmltYXRpb25zXT1cImFuaW1hdGlvbnNcIlxuICAgICAgICAgICAgICBbdG9vbHRpcERpc2FibGVkXT1cInRvb2x0aXBEaXNhYmxlZFwiXG4gICAgICAgICAgICAgIFt0b29sdGlwVGVtcGxhdGVdPVwidG9vbHRpcFRlbXBsYXRlXCJcbiAgICAgICAgICAgICAgKHNlbGVjdCk9XCJvbkNsaWNrKCRldmVudClcIlxuICAgICAgICAgICAgICAoYWN0aXZhdGUpPVwib25BY3RpdmF0ZSgkZXZlbnQpXCJcbiAgICAgICAgICAgICAgKGRlYWN0aXZhdGUpPVwib25EZWFjdGl2YXRlKCRldmVudClcIlxuICAgICAgICAgICAgLz5cbiAgICAgICAgICA8L3N2ZzpnPlxuICAgICAgICA8L3N2ZzpnPlxuICAgICAgPC9zdmc6Zz5cbiAgICA8L25neC1jaGFydHMtY2hhcnQ+XG4gIGAsXG4gIHN0eWxlVXJsczogW1xuICAgICcuLi9jb21tb24vYmFzZS1jaGFydC5jb21wb25lbnQuc2NzcycsXG4gICAgJy4uL3BpZS1jaGFydC9waWUtY2hhcnQuY29tcG9uZW50LnNjc3MnLFxuICAgICcuL3BvbGFyLWNoYXJ0LmNvbXBvbmVudC5zY3NzJ1xuICBdLFxuICBlbmNhcHN1bGF0aW9uOiBWaWV3RW5jYXBzdWxhdGlvbi5Ob25lLFxuICBjaGFuZ2VEZXRlY3Rpb246IENoYW5nZURldGVjdGlvblN0cmF0ZWd5Lk9uUHVzaCxcbiAgYW5pbWF0aW9uczogW1xuICAgIHRyaWdnZXIoJ2FuaW1hdGlvblN0YXRlJywgW1xuICAgICAgdHJhbnNpdGlvbignOmxlYXZlJywgW1xuICAgICAgICBzdHlsZSh7XG4gICAgICAgICAgb3BhY2l0eTogMVxuICAgICAgICB9KSxcbiAgICAgICAgYW5pbWF0ZShcbiAgICAgICAgICA1MDAsXG4gICAgICAgICAgc3R5bGUoe1xuICAgICAgICAgICAgb3BhY2l0eTogMFxuICAgICAgICAgIH0pXG4gICAgICAgIClcbiAgICAgIF0pXG4gICAgXSlcbiAgXVxufSlcbmV4cG9ydCBjbGFzcyBQb2xhckNoYXJ0Q29tcG9uZW50IGV4dGVuZHMgQmFzZUNoYXJ0Q29tcG9uZW50IHtcbiAgQElucHV0KCkgbGVnZW5kOiBib29sZWFuO1xuICBASW5wdXQoKSBsZWdlbmRUaXRsZTogc3RyaW5nID0gJ0xlZ2VuZCc7XG4gIEBJbnB1dCgpIGxlZ2VuZFBvc2l0aW9uOiBzdHJpbmcgPSAncmlnaHQnO1xuICBASW5wdXQoKSB4QXhpczogYm9vbGVhbjtcbiAgQElucHV0KCkgeUF4aXM6IGJvb2xlYW47XG4gIEBJbnB1dCgpIHNob3dYQXhpc0xhYmVsOiBib29sZWFuO1xuICBASW5wdXQoKSBzaG93WUF4aXNMYWJlbDogYm9vbGVhbjtcbiAgQElucHV0KCkgeEF4aXNMYWJlbDogc3RyaW5nO1xuICBASW5wdXQoKSB5QXhpc0xhYmVsOiBzdHJpbmc7XG4gIEBJbnB1dCgpIGF1dG9TY2FsZTogYm9vbGVhbjtcbiAgQElucHV0KCkgc2hvd0dyaWRMaW5lczogYm9vbGVhbiA9IHRydWU7XG4gIEBJbnB1dCgpIGN1cnZlOiBhbnkgPSBjdXJ2ZUNhcmRpbmFsQ2xvc2VkO1xuICBASW5wdXQoKSBhY3RpdmVFbnRyaWVzOiBhbnlbXSA9IFtdO1xuICBASW5wdXQoKSBzY2hlbWVUeXBlOiBzdHJpbmc7XG4gIEBJbnB1dCgpIHJhbmdlRmlsbE9wYWNpdHk6IG51bWJlciA9IDAuMTU7XG4gIEBJbnB1dCgpIHRyaW1ZQXhpc1RpY2tzOiBib29sZWFuID0gdHJ1ZTtcbiAgQElucHV0KCkgbWF4WUF4aXNUaWNrTGVuZ3RoOiBudW1iZXIgPSAxNjtcbiAgQElucHV0KCkgeEF4aXNUaWNrRm9ybWF0dGluZzogKG86IGFueSkgPT4gYW55O1xuICBASW5wdXQoKSB5QXhpc1RpY2tGb3JtYXR0aW5nOiAobzogYW55KSA9PiBhbnk7XG4gIEBJbnB1dCgpIHJvdW5kRG9tYWluczogYm9vbGVhbiA9IGZhbHNlO1xuICBASW5wdXQoKSB0b29sdGlwRGlzYWJsZWQ6IGJvb2xlYW4gPSBmYWxzZTtcbiAgQElucHV0KCkgc2hvd1Nlcmllc09uSG92ZXI6IGJvb2xlYW4gPSB0cnVlO1xuICBASW5wdXQoKSBncmFkaWVudDogYm9vbGVhbiA9IGZhbHNlO1xuICBASW5wdXQoKSB5QXhpc01pblNjYWxlOiBudW1iZXIgPSAwO1xuICBASW5wdXQoKSBsYWJlbFRyaW06IGJvb2xlYW4gPSB0cnVlO1xuICBASW5wdXQoKSBsYWJlbFRyaW1TaXplOiBudW1iZXIgPSAxMDtcblxuICBAT3V0cHV0KCkgYWN0aXZhdGU6IEV2ZW50RW1pdHRlcjxhbnk+ID0gbmV3IEV2ZW50RW1pdHRlcigpO1xuICBAT3V0cHV0KCkgZGVhY3RpdmF0ZTogRXZlbnRFbWl0dGVyPGFueT4gPSBuZXcgRXZlbnRFbWl0dGVyKCk7XG5cbiAgQENvbnRlbnRDaGlsZCgndG9vbHRpcFRlbXBsYXRlJykgdG9vbHRpcFRlbXBsYXRlOiBUZW1wbGF0ZVJlZjxhbnk+O1xuXG4gIGRpbXM6IFZpZXdEaW1lbnNpb25zO1xuICB5QXhpc0RpbXM6IFZpZXdEaW1lbnNpb25zO1xuICBsYWJlbE9mZnNldDogbnVtYmVyO1xuICB4RG9tYWluOiBhbnk7XG4gIHlEb21haW46IGFueTtcbiAgc2VyaWVzRG9tYWluOiBhbnk7XG4gIHlTY2FsZTogYW55OyAvLyAtPiByU2NhbGVcbiAgeFNjYWxlOiBhbnk7IC8vIC0+IHRTY2FsZVxuICB5QXhpc1NjYWxlOiBhbnk7IC8vIC0+IHlTY2FsZVxuICBjb2xvcnM6IENvbG9ySGVscGVyO1xuICBzY2FsZVR5cGU6IHN0cmluZztcbiAgdHJhbnNmb3JtOiBzdHJpbmc7XG4gIHRyYW5zZm9ybVBsb3Q6IHN0cmluZztcbiAgdHJhbnNmb3JtWUF4aXM6IHN0cmluZztcbiAgdHJhbnNmb3JtWEF4aXM6IHN0cmluZztcbiAgc2VyaWVzOiBhbnk7IC8vID8/P1xuICBtYXJnaW4gPSBbMTAsIDIwLCAxMCwgMjBdO1xuICB4QXhpc0hlaWdodDogbnVtYmVyID0gMDtcbiAgeUF4aXNXaWR0aDogbnVtYmVyID0gMDtcbiAgZmlsdGVyZWREb21haW46IGFueTtcbiAgbGVnZW5kT3B0aW9uczogYW55O1xuICB0aGV0YVRpY2tzOiBhbnlbXTtcbiAgcmFkaXVzVGlja3M6IG51bWJlcltdO1xuICBvdXRlclJhZGl1czogbnVtYmVyO1xuXG4gIHVwZGF0ZSgpOiB2b2lkIHtcbiAgICBzdXBlci51cGRhdGUoKTtcblxuICAgIHRoaXMuc2V0RGltcygpO1xuXG4gICAgdGhpcy5zZXRTY2FsZXMoKTtcbiAgICB0aGlzLnNldENvbG9ycygpO1xuICAgIHRoaXMubGVnZW5kT3B0aW9ucyA9IHRoaXMuZ2V0TGVnZW5kT3B0aW9ucygpO1xuXG4gICAgdGhpcy5zZXRUaWNrcygpO1xuICB9XG5cbiAgc2V0RGltcygpIHtcbiAgICB0aGlzLmRpbXMgPSBjYWxjdWxhdGVWaWV3RGltZW5zaW9ucyh7XG4gICAgICB3aWR0aDogdGhpcy53aWR0aCxcbiAgICAgIGhlaWdodDogdGhpcy5oZWlnaHQsXG4gICAgICBtYXJnaW5zOiB0aGlzLm1hcmdpbixcbiAgICAgIHNob3dYQXhpczogdGhpcy54QXhpcyxcbiAgICAgIHNob3dZQXhpczogdGhpcy55QXhpcyxcbiAgICAgIHhBeGlzSGVpZ2h0OiB0aGlzLnhBeGlzSGVpZ2h0LFxuICAgICAgeUF4aXNXaWR0aDogdGhpcy55QXhpc1dpZHRoLFxuICAgICAgc2hvd1hMYWJlbDogdGhpcy5zaG93WEF4aXNMYWJlbCxcbiAgICAgIHNob3dZTGFiZWw6IHRoaXMuc2hvd1lBeGlzTGFiZWwsXG4gICAgICBzaG93TGVnZW5kOiB0aGlzLmxlZ2VuZCxcbiAgICAgIGxlZ2VuZFR5cGU6IHRoaXMuc2NoZW1lVHlwZSxcbiAgICAgIGxlZ2VuZFBvc2l0aW9uOiB0aGlzLmxlZ2VuZFBvc2l0aW9uXG4gICAgfSk7XG5cbiAgICBjb25zdCBoYWxmV2lkdGggPSBNYXRoLmZsb29yKHRoaXMuZGltcy53aWR0aCAvIDIpO1xuICAgIGNvbnN0IGhhbGZIZWlnaHQgPSBNYXRoLmZsb29yKHRoaXMuZGltcy5oZWlnaHQgLyAyKTtcblxuICAgIGNvbnN0IG91dGVyUmFkaXVzID0gKHRoaXMub3V0ZXJSYWRpdXMgPSBNYXRoLm1pbihoYWxmSGVpZ2h0IC8gMS41LCBoYWxmV2lkdGggLyAxLjUpKTtcblxuICAgIGNvbnN0IHlPZmZzZXQgPSBNYXRoLm1heCgwLCBoYWxmSGVpZ2h0IC0gb3V0ZXJSYWRpdXMpO1xuXG4gICAgdGhpcy55QXhpc0RpbXMgPSB7XG4gICAgICAuLi50aGlzLmRpbXMsXG4gICAgICB3aWR0aDogaGFsZldpZHRoXG4gICAgfTtcblxuICAgIHRoaXMudHJhbnNmb3JtID0gYHRyYW5zbGF0ZSgke3RoaXMuZGltcy54T2Zmc2V0fSwgJHt0aGlzLm1hcmdpblswXX0pYDtcbiAgICB0aGlzLnRyYW5zZm9ybVlBeGlzID0gYHRyYW5zbGF0ZSgwLCAke3lPZmZzZXR9KWA7XG4gICAgdGhpcy5sYWJlbE9mZnNldCA9IHRoaXMuZGltcy5oZWlnaHQgKyA0MDtcbiAgICB0aGlzLnRyYW5zZm9ybVBsb3QgPSBgdHJhbnNsYXRlKCR7aGFsZldpZHRofSwgJHtoYWxmSGVpZ2h0fSlgO1xuICB9XG5cbiAgc2V0U2NhbGVzKCkge1xuICAgIGNvbnN0IHhWYWx1ZXMgPSB0aGlzLmdldFhWYWx1ZXMoKTtcbiAgICB0aGlzLnNjYWxlVHlwZSA9IGdldFNjYWxlVHlwZSh4VmFsdWVzKTtcbiAgICB0aGlzLnhEb21haW4gPSB0aGlzLmZpbHRlcmVkRG9tYWluIHx8IHRoaXMuZ2V0WERvbWFpbih4VmFsdWVzKTtcblxuICAgIHRoaXMueURvbWFpbiA9IHRoaXMuZ2V0WURvbWFpbigpO1xuICAgIHRoaXMuc2VyaWVzRG9tYWluID0gdGhpcy5nZXRTZXJpZXNEb21haW4oKTtcblxuICAgIHRoaXMueFNjYWxlID0gdGhpcy5nZXRYU2NhbGUodGhpcy54RG9tYWluLCB0d29QSSk7XG4gICAgdGhpcy55U2NhbGUgPSB0aGlzLmdldFlTY2FsZSh0aGlzLnlEb21haW4sIHRoaXMub3V0ZXJSYWRpdXMpO1xuICAgIHRoaXMueUF4aXNTY2FsZSA9IHRoaXMuZ2V0WVNjYWxlKHRoaXMueURvbWFpbi5yZXZlcnNlKCksIHRoaXMub3V0ZXJSYWRpdXMpO1xuICB9XG5cbiAgc2V0VGlja3MoKSB7XG4gICAgbGV0IHRpY2tGb3JtYXQ7XG4gICAgaWYgKHRoaXMueEF4aXNUaWNrRm9ybWF0dGluZykge1xuICAgICAgdGlja0Zvcm1hdCA9IHRoaXMueEF4aXNUaWNrRm9ybWF0dGluZztcbiAgICB9IGVsc2UgaWYgKHRoaXMueFNjYWxlLnRpY2tGb3JtYXQpIHtcbiAgICAgIHRpY2tGb3JtYXQgPSB0aGlzLnhTY2FsZS50aWNrRm9ybWF0LmFwcGx5KHRoaXMueFNjYWxlLCBbNV0pO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aWNrRm9ybWF0ID0gZCA9PiB7XG4gICAgICAgIGlmIChpc0RhdGUoZCkpIHtcbiAgICAgICAgICByZXR1cm4gZC50b0xvY2FsZURhdGVTdHJpbmcoKTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gZC50b0xvY2FsZVN0cmluZygpO1xuICAgICAgfTtcbiAgICB9XG5cbiAgICBjb25zdCBvdXRlclJhZGl1cyA9IHRoaXMub3V0ZXJSYWRpdXM7XG4gICAgY29uc3QgcyA9IDEuMTtcblxuICAgIHRoaXMudGhldGFUaWNrcyA9IHRoaXMueERvbWFpbi5tYXAoZCA9PiB7XG4gICAgICBjb25zdCBzdGFydEFuZ2xlID0gdGhpcy54U2NhbGUoZCk7XG4gICAgICBjb25zdCBkZCA9IHMgKiBvdXRlclJhZGl1cyAqIChzdGFydEFuZ2xlID4gTWF0aC5QSSA/IC0xIDogMSk7XG4gICAgICBjb25zdCBsYWJlbCA9IHRpY2tGb3JtYXQoZCk7XG5cbiAgICAgIGNvbnN0IHN0YXJ0UG9zID0gW291dGVyUmFkaXVzICogTWF0aC5zaW4oc3RhcnRBbmdsZSksIC1vdXRlclJhZGl1cyAqIE1hdGguY29zKHN0YXJ0QW5nbGUpXTtcbiAgICAgIGNvbnN0IHBvcyA9IFtkZCwgcyAqIHN0YXJ0UG9zWzFdXTtcbiAgICAgIHJldHVybiB7XG4gICAgICAgIGlubmVyUmFkaXVzOiAwLFxuICAgICAgICBvdXRlclJhZGl1cyxcbiAgICAgICAgc3RhcnRBbmdsZSxcbiAgICAgICAgZW5kQW5nbGU6IHN0YXJ0QW5nbGUsXG4gICAgICAgIHZhbHVlOiBvdXRlclJhZGl1cyxcbiAgICAgICAgbGFiZWwsXG4gICAgICAgIHN0YXJ0UG9zLFxuICAgICAgICBwb3NcbiAgICAgIH07XG4gICAgfSk7XG5cbiAgICBjb25zdCBtaW5EaXN0YW5jZSA9IDEwO1xuXG4gICAgLyogZnJvbSBwaWUgY2hhcnQsIGFic3RyYWN0IG91dCAtKi9cbiAgICBmb3IgKGxldCBpID0gMDsgaSA8IHRoaXMudGhldGFUaWNrcy5sZW5ndGggLSAxOyBpKyspIHtcbiAgICAgIGNvbnN0IGEgPSB0aGlzLnRoZXRhVGlja3NbaV07XG5cbiAgICAgIGZvciAobGV0IGogPSBpICsgMTsgaiA8IHRoaXMudGhldGFUaWNrcy5sZW5ndGg7IGorKykge1xuICAgICAgICBjb25zdCBiID0gdGhpcy50aGV0YVRpY2tzW2pdO1xuICAgICAgICAvLyBpZiB0aGV5J3JlIG9uIHRoZSBzYW1lIHNpZGVcbiAgICAgICAgaWYgKGIucG9zWzBdICogYS5wb3NbMF0gPiAwKSB7XG4gICAgICAgICAgLy8gaWYgdGhleSdyZSBvdmVybGFwcGluZ1xuICAgICAgICAgIGNvbnN0IG8gPSBtaW5EaXN0YW5jZSAtIE1hdGguYWJzKGIucG9zWzFdIC0gYS5wb3NbMV0pO1xuICAgICAgICAgIGlmIChvID4gMCkge1xuICAgICAgICAgICAgLy8gcHVzaCB0aGUgc2Vjb25kIHVwIG9yIGRvd25cbiAgICAgICAgICAgIGIucG9zWzFdICs9IE1hdGguc2lnbihiLnBvc1swXSkgKiBvO1xuICAgICAgICAgIH1cbiAgICAgICAgfVxuICAgICAgfVxuICAgIH1cblxuICAgIHRoaXMucmFkaXVzVGlja3MgPSB0aGlzLnlBeGlzU2NhbGUudGlja3MoTWF0aC5mbG9vcih0aGlzLmRpbXMuaGVpZ2h0IC8gNTApKS5tYXAoZCA9PiB0aGlzLnlTY2FsZShkKSk7XG4gIH1cblxuICBnZXRYVmFsdWVzKCk6IGFueVtdIHtcbiAgICBjb25zdCB2YWx1ZXMgPSBbXTtcbiAgICBmb3IgKGNvbnN0IHJlc3VsdHMgb2YgdGhpcy5yZXN1bHRzKSB7XG4gICAgICBmb3IgKGNvbnN0IGQgb2YgcmVzdWx0cy5zZXJpZXMpIHtcbiAgICAgICAgaWYgKCF2YWx1ZXMuaW5jbHVkZXMoZC5uYW1lKSkge1xuICAgICAgICAgIHZhbHVlcy5wdXNoKGQubmFtZSk7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG4gICAgcmV0dXJuIHZhbHVlcztcbiAgfVxuXG4gIGdldFhEb21haW4odmFsdWVzID0gdGhpcy5nZXRYVmFsdWVzKCkpOiBhbnlbXSB7XG4gICAgaWYgKHRoaXMuc2NhbGVUeXBlID09PSAndGltZScpIHtcbiAgICAgIGNvbnN0IG1pbiA9IE1hdGgubWluKC4uLnZhbHVlcyk7XG4gICAgICBjb25zdCBtYXggPSBNYXRoLm1heCguLi52YWx1ZXMpO1xuICAgICAgcmV0dXJuIFttaW4sIG1heF07XG4gICAgfSBlbHNlIGlmICh0aGlzLnNjYWxlVHlwZSA9PT0gJ2xpbmVhcicpIHtcbiAgICAgIHZhbHVlcyA9IHZhbHVlcy5tYXAodiA9PiBOdW1iZXIodikpO1xuICAgICAgY29uc3QgbWluID0gTWF0aC5taW4oLi4udmFsdWVzKTtcbiAgICAgIGNvbnN0IG1heCA9IE1hdGgubWF4KC4uLnZhbHVlcyk7XG4gICAgICByZXR1cm4gW21pbiwgbWF4XTtcbiAgICB9XG4gICAgcmV0dXJuIHZhbHVlcztcbiAgfVxuXG4gIGdldFlWYWx1ZXMoKTogYW55W10ge1xuICAgIGNvbnN0IGRvbWFpbiA9IFtdO1xuXG4gICAgZm9yIChjb25zdCByZXN1bHRzIG9mIHRoaXMucmVzdWx0cykge1xuICAgICAgZm9yIChjb25zdCBkIG9mIHJlc3VsdHMuc2VyaWVzKSB7XG4gICAgICAgIGlmIChkb21haW4uaW5kZXhPZihkLnZhbHVlKSA8IDApIHtcbiAgICAgICAgICBkb21haW4ucHVzaChkLnZhbHVlKTtcbiAgICAgICAgfVxuICAgICAgICBpZiAoZC5taW4gIT09IHVuZGVmaW5lZCkge1xuICAgICAgICAgIGlmIChkb21haW4uaW5kZXhPZihkLm1pbikgPCAwKSB7XG4gICAgICAgICAgICBkb21haW4ucHVzaChkLm1pbik7XG4gICAgICAgICAgfVxuICAgICAgICB9XG4gICAgICAgIGlmIChkLm1heCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICAgICAgaWYgKGRvbWFpbi5pbmRleE9mKGQubWF4KSA8IDApIHtcbiAgICAgICAgICAgIGRvbWFpbi5wdXNoKGQubWF4KTtcbiAgICAgICAgICB9XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG4gICAgcmV0dXJuIGRvbWFpbjtcbiAgfVxuXG4gIGdldFlEb21haW4oZG9tYWluID0gdGhpcy5nZXRZVmFsdWVzKCkpOiBhbnlbXSB7XG4gICAgbGV0IG1pbiA9IE1hdGgubWluKC4uLmRvbWFpbik7XG4gICAgY29uc3QgbWF4ID0gTWF0aC5tYXgodGhpcy55QXhpc01pblNjYWxlLCAuLi5kb21haW4pO1xuXG4gICAgbWluID0gTWF0aC5tYXgoMCwgbWluKTtcbiAgICBpZiAoIXRoaXMuYXV0b1NjYWxlKSB7XG4gICAgICBtaW4gPSBNYXRoLm1pbigwLCBtaW4pO1xuICAgIH1cblxuICAgIHJldHVybiBbbWluLCBtYXhdO1xuICB9XG5cbiAgZ2V0U2VyaWVzRG9tYWluKCk6IGFueVtdIHtcbiAgICByZXR1cm4gdGhpcy5yZXN1bHRzLm1hcChkID0+IGQubmFtZSk7XG4gIH1cblxuICBnZXRYU2NhbGUoZG9tYWluLCB3aWR0aCk6IGFueSB7XG4gICAgc3dpdGNoICh0aGlzLnNjYWxlVHlwZSkge1xuICAgICAgY2FzZSAndGltZSc6XG4gICAgICAgIHJldHVybiBzY2FsZVRpbWUoKVxuICAgICAgICAgIC5yYW5nZShbMCwgd2lkdGhdKVxuICAgICAgICAgIC5kb21haW4oZG9tYWluKTtcbiAgICAgIGNhc2UgJ2xpbmVhcic6XG4gICAgICAgIGNvbnN0IHNjYWxlID0gc2NhbGVMaW5lYXIoKVxuICAgICAgICAgIC5yYW5nZShbMCwgd2lkdGhdKVxuICAgICAgICAgIC5kb21haW4oZG9tYWluKTtcbiAgICAgICAgcmV0dXJuIHRoaXMucm91bmREb21haW5zID8gc2NhbGUubmljZSgpIDogc2NhbGU7XG4gICAgICBkZWZhdWx0OlxuICAgICAgICByZXR1cm4gc2NhbGVQb2ludCgpXG4gICAgICAgICAgLnJhbmdlKFswLCB3aWR0aCAtIHR3b1BJIC8gZG9tYWluLmxlbmd0aF0pXG4gICAgICAgICAgLnBhZGRpbmcoMClcbiAgICAgICAgICAuZG9tYWluKGRvbWFpbik7XG4gICAgfVxuICB9XG5cbiAgZ2V0WVNjYWxlKGRvbWFpbiwgaGVpZ2h0KTogYW55IHtcbiAgICBjb25zdCBzY2FsZSA9IHNjYWxlTGluZWFyKClcbiAgICAgIC5yYW5nZShbMCwgaGVpZ2h0XSlcbiAgICAgIC5kb21haW4oZG9tYWluKTtcblxuICAgIHJldHVybiB0aGlzLnJvdW5kRG9tYWlucyA/IHNjYWxlLm5pY2UoKSA6IHNjYWxlO1xuICB9XG5cbiAgb25DbGljayhkYXRhLCBzZXJpZXM/KTogdm9pZCB7XG4gICAgaWYgKHNlcmllcykge1xuICAgICAgZGF0YS5zZXJpZXMgPSBzZXJpZXMubmFtZTtcbiAgICB9XG5cbiAgICB0aGlzLnNlbGVjdC5lbWl0KGRhdGEpO1xuICB9XG5cbiAgc2V0Q29sb3JzKCk6IHZvaWQge1xuICAgIGNvbnN0IGRvbWFpbiA9IHRoaXMuc2NoZW1lVHlwZSA9PT0gJ29yZGluYWwnID8gdGhpcy5zZXJpZXNEb21haW4gOiB0aGlzLnlEb21haW4ucmV2ZXJzZSgpO1xuICAgIHRoaXMuY29sb3JzID0gbmV3IENvbG9ySGVscGVyKHRoaXMuc2NoZW1lLCB0aGlzLnNjaGVtZVR5cGUsIGRvbWFpbiwgdGhpcy5jdXN0b21Db2xvcnMpO1xuICB9XG5cbiAgZ2V0TGVnZW5kT3B0aW9ucygpIHtcbiAgICBpZiAodGhpcy5zY2hlbWVUeXBlID09PSAnb3JkaW5hbCcpIHtcbiAgICAgIHJldHVybiB7XG4gICAgICAgIHNjYWxlVHlwZTogdGhpcy5zY2hlbWVUeXBlLFxuICAgICAgICBjb2xvcnM6IHRoaXMuY29sb3JzLFxuICAgICAgICBkb21haW46IHRoaXMuc2VyaWVzRG9tYWluLFxuICAgICAgICB0aXRsZTogdGhpcy5sZWdlbmRUaXRsZSxcbiAgICAgICAgcG9zaXRpb246IHRoaXMubGVnZW5kUG9zaXRpb25cbiAgICAgIH07XG4gICAgfVxuICAgIHJldHVybiB7XG4gICAgICBzY2FsZVR5cGU6IHRoaXMuc2NoZW1lVHlwZSxcbiAgICAgIGNvbG9yczogdGhpcy5jb2xvcnMuc2NhbGUsXG4gICAgICBkb21haW46IHRoaXMueURvbWFpbixcbiAgICAgIHRpdGxlOiB1bmRlZmluZWQsXG4gICAgICBwb3NpdGlvbjogdGhpcy5sZWdlbmRQb3NpdGlvblxuICAgIH07XG4gIH1cblxuICB1cGRhdGVZQXhpc1dpZHRoKHsgd2lkdGggfSk6IHZvaWQge1xuICAgIHRoaXMueUF4aXNXaWR0aCA9IHdpZHRoO1xuICAgIHRoaXMudXBkYXRlKCk7XG4gIH1cblxuICB1cGRhdGVYQXhpc0hlaWdodCh7IGhlaWdodCB9KTogdm9pZCB7XG4gICAgdGhpcy54QXhpc0hlaWdodCA9IGhlaWdodDtcbiAgICB0aGlzLnVwZGF0ZSgpO1xuICB9XG5cbiAgb25BY3RpdmF0ZShpdGVtKSB7XG4gICAgY29uc3QgaWR4ID0gdGhpcy5hY3RpdmVFbnRyaWVzLmZpbmRJbmRleChkID0+IHtcbiAgICAgIHJldHVybiBkLm5hbWUgPT09IGl0ZW0ubmFtZSAmJiBkLnZhbHVlID09PSBpdGVtLnZhbHVlO1xuICAgIH0pO1xuICAgIGlmIChpZHggPiAtMSkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgICB0aGlzLmFjdGl2ZUVudHJpZXMgPSB0aGlzLnNob3dTZXJpZXNPbkhvdmVyID8gW2l0ZW0sIC4uLnRoaXMuYWN0aXZlRW50cmllc10gOiB0aGlzLmFjdGl2ZUVudHJpZXM7XG4gICAgdGhpcy5hY3RpdmF0ZS5lbWl0KHsgdmFsdWU6IGl0ZW0sIGVudHJpZXM6IHRoaXMuYWN0aXZlRW50cmllcyB9KTtcbiAgfVxuXG4gIG9uRGVhY3RpdmF0ZShpdGVtKSB7XG4gICAgY29uc3QgaWR4ID0gdGhpcy5hY3RpdmVFbnRyaWVzLmZpbmRJbmRleChkID0+IHtcbiAgICAgIHJldHVybiBkLm5hbWUgPT09IGl0ZW0ubmFtZSAmJiBkLnZhbHVlID09PSBpdGVtLnZhbHVlO1xuICAgIH0pO1xuXG4gICAgdGhpcy5hY3RpdmVFbnRyaWVzLnNwbGljZShpZHgsIDEpO1xuICAgIHRoaXMuYWN0aXZlRW50cmllcyA9IFsuLi50aGlzLmFjdGl2ZUVudHJpZXNdO1xuXG4gICAgdGhpcy5kZWFjdGl2YXRlLmVtaXQoeyB2YWx1ZTogaXRlbSwgZW50cmllczogdGhpcy5hY3RpdmVFbnRyaWVzIH0pO1xuICB9XG5cbiAgZGVhY3RpdmF0ZUFsbCgpIHtcbiAgICB0aGlzLmFjdGl2ZUVudHJpZXMgPSBbLi4udGhpcy5hY3RpdmVFbnRyaWVzXTtcbiAgICBmb3IgKGNvbnN0IGVudHJ5IG9mIHRoaXMuYWN0aXZlRW50cmllcykge1xuICAgICAgdGhpcy5kZWFjdGl2YXRlLmVtaXQoeyB2YWx1ZTogZW50cnksIGVudHJpZXM6IFtdIH0pO1xuICAgIH1cbiAgICB0aGlzLmFjdGl2ZUVudHJpZXMgPSBbXTtcbiAgfVxuXG4gIHRyYWNrQnkoaW5kZXgsIGl0ZW0pIHtcbiAgICByZXR1cm4gaXRlbS5uYW1lO1xuICB9XG59XG4iXX0=