import { __read, __spread, __values } from "tslib";
import { range } from 'd3-array';
import { scaleBand, scaleLinear, scaleOrdinal, scaleQuantile } from 'd3-scale';
import { colorSets } from '../utils/color-sets';
var ColorHelper = /** @class */ (function () {
    function ColorHelper(scheme, type, domain, customColors) {
        if (typeof scheme === 'string') {
            scheme = colorSets.find(function (cs) {
                return cs.name === scheme;
            });
        }
        this.colorDomain = scheme.domain;
        this.scaleType = type;
        this.domain = domain;
        this.customColors = customColors;
        this.scale = this.generateColorScheme(scheme, type, this.domain);
    }
    ColorHelper.prototype.generateColorScheme = function (scheme, type, domain) {
        if (typeof scheme === 'string') {
            scheme = colorSets.find(function (cs) {
                return cs.name === scheme;
            });
        }
        var colorScale;
        if (type === 'quantile') {
            colorScale = scaleQuantile()
                .range(scheme.domain)
                .domain(domain);
        }
        else if (type === 'ordinal') {
            colorScale = scaleOrdinal()
                .range(scheme.domain)
                .domain(domain);
        }
        else if (type === 'linear') {
            // linear schemes must have at least 2 colors
            var colorDomain = __spread(scheme.domain);
            if (colorDomain.length === 1) {
                colorDomain.push(colorDomain[0]);
                this.colorDomain = colorDomain;
            }
            var points = range(0, 1, 1.0 / colorDomain.length);
            colorScale = scaleLinear()
                .domain(points)
                .range(colorDomain);
        }
        return colorScale;
    };
    ColorHelper.prototype.getColor = function (value) {
        if (value === undefined || value === null) {
            throw new Error('Value can not be null');
        }
        if (this.scaleType === 'linear') {
            var valueScale = scaleLinear()
                .domain(this.domain)
                .range([0, 1]);
            return this.scale(valueScale(value));
        }
        else {
            if (typeof this.customColors === 'function') {
                return this.customColors(value);
            }
            var formattedValue_1 = value.toString();
            var found = void 0; // todo type customColors
            if (this.customColors && this.customColors.length > 0) {
                found = this.customColors.find(function (mapping) {
                    return mapping.name.toLowerCase() === formattedValue_1.toLowerCase();
                });
            }
            if (found) {
                return found.value;
            }
            else {
                return this.scale(value);
            }
        }
    };
    ColorHelper.prototype.getLinearGradientStops = function (value, start) {
        var e_1, _a;
        if (start === undefined) {
            start = this.domain[0];
        }
        var valueScale = scaleLinear()
            .domain(this.domain)
            .range([0, 1]);
        var colorValueScale = scaleBand()
            .domain(this.colorDomain)
            .range([0, 1]);
        var endColor = this.getColor(value);
        // generate the stops
        var startVal = valueScale(start);
        var startColor = this.getColor(start);
        var endVal = valueScale(value);
        var i = 1;
        var currentVal = startVal;
        var stops = [];
        stops.push({
            color: startColor,
            offset: startVal,
            originalOffset: startVal,
            opacity: 1
        });
        while (currentVal < endVal && i < this.colorDomain.length) {
            var color = this.colorDomain[i];
            var offset = colorValueScale(color);
            if (offset <= startVal) {
                i++;
                continue;
            }
            if (offset.toFixed(4) >= (endVal - colorValueScale.bandwidth()).toFixed(4)) {
                break;
            }
            stops.push({
                color: color,
                offset: offset,
                opacity: 1
            });
            currentVal = offset;
            i++;
        }
        if (stops[stops.length - 1].offset < 100) {
            stops.push({
                color: endColor,
                offset: endVal,
                opacity: 1
            });
        }
        if (endVal === startVal) {
            stops[0].offset = 0;
            stops[1].offset = 100;
        }
        else {
            // normalize the offsets into percentages
            if (stops[stops.length - 1].offset !== 100) {
                try {
                    for (var stops_1 = __values(stops), stops_1_1 = stops_1.next(); !stops_1_1.done; stops_1_1 = stops_1.next()) {
                        var s = stops_1_1.value;
                        s.offset = ((s.offset - startVal) / (endVal - startVal)) * 100;
                    }
                }
                catch (e_1_1) { e_1 = { error: e_1_1 }; }
                finally {
                    try {
                        if (stops_1_1 && !stops_1_1.done && (_a = stops_1.return)) _a.call(stops_1);
                    }
                    finally { if (e_1) throw e_1.error; }
                }
            }
        }
        return stops;
    };
    return ColorHelper;
}());
export { ColorHelper };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY29sb3IuaGVscGVyLmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvY29tbW9uL2NvbG9yLmhlbHBlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsT0FBTyxFQUFFLEtBQUssRUFBRSxNQUFNLFVBQVUsQ0FBQztBQUNqQyxPQUFPLEVBQUUsU0FBUyxFQUFFLFdBQVcsRUFBRSxZQUFZLEVBQUUsYUFBYSxFQUFFLE1BQU0sVUFBVSxDQUFDO0FBRS9FLE9BQU8sRUFBRSxTQUFTLEVBQUUsTUFBTSxxQkFBcUIsQ0FBQztBQUVoRDtJQU9FLHFCQUFZLE1BQU0sRUFBRSxJQUFJLEVBQUUsTUFBTSxFQUFFLFlBQWE7UUFDN0MsSUFBSSxPQUFPLE1BQU0sS0FBSyxRQUFRLEVBQUU7WUFDOUIsTUFBTSxHQUFHLFNBQVMsQ0FBQyxJQUFJLENBQUMsVUFBQSxFQUFFO2dCQUN4QixPQUFPLEVBQUUsQ0FBQyxJQUFJLEtBQUssTUFBTSxDQUFDO1lBQzVCLENBQUMsQ0FBQyxDQUFDO1NBQ0o7UUFDRCxJQUFJLENBQUMsV0FBVyxHQUFHLE1BQU0sQ0FBQyxNQUFNLENBQUM7UUFDakMsSUFBSSxDQUFDLFNBQVMsR0FBRyxJQUFJLENBQUM7UUFDdEIsSUFBSSxDQUFDLE1BQU0sR0FBRyxNQUFNLENBQUM7UUFDckIsSUFBSSxDQUFDLFlBQVksR0FBRyxZQUFZLENBQUM7UUFFakMsSUFBSSxDQUFDLEtBQUssR0FBRyxJQUFJLENBQUMsbUJBQW1CLENBQUMsTUFBTSxFQUFFLElBQUksRUFBRSxJQUFJLENBQUMsTUFBTSxDQUFDLENBQUM7SUFDbkUsQ0FBQztJQUVELHlDQUFtQixHQUFuQixVQUFvQixNQUFNLEVBQUUsSUFBSSxFQUFFLE1BQU07UUFDdEMsSUFBSSxPQUFPLE1BQU0sS0FBSyxRQUFRLEVBQUU7WUFDOUIsTUFBTSxHQUFHLFNBQVMsQ0FBQyxJQUFJLENBQUMsVUFBQSxFQUFFO2dCQUN4QixPQUFP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Ozs7OzthQUNGO1NBQ0Y7UUFFRCxPQUFPLEtBQUssQ0FBQztJQUNmLENBQUM7SUFDSCxrQkFBQztBQUFELENBQUMsQUE5SkQsSUE4SkMiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgeyByYW5nZSB9IGZyb20gJ2QzLWFycmF5JztcbmltcG9ydCB7IHNjYWxlQmFuZCwgc2NhbGVMaW5lYXIsIHNjYWxlT3JkaW5hbCwgc2NhbGVRdWFudGlsZSB9IGZyb20gJ2QzLXNjYWxlJztcblxuaW1wb3J0IHsgY29sb3JTZXRzIH0gZnJvbSAnLi4vdXRpbHMvY29sb3Itc2V0cyc7XG5cbmV4cG9ydCBjbGFzcyBDb2xvckhlbHBlciB7XG4gIHNjYWxlOiBhbnk7XG4gIHNjYWxlVHlwZTogYW55O1xuICBjb2xvckRvbWFpbjogYW55W107XG4gIGRvbWFpbjogYW55O1xuICBjdXN0b21Db2xvcnM6IGFueTtcblxuICBjb25zdHJ1Y3RvcihzY2hlbWUsIHR5cGUsIGRvbWFpbiwgY3VzdG9tQ29sb3JzPykge1xuICAgIGlmICh0eXBlb2Ygc2NoZW1lID09PSAnc3RyaW5nJykge1xuICAgICAgc2NoZW1lID0gY29sb3JTZXRzLmZpbmQoY3MgPT4ge1xuICAgICAgICByZXR1cm4gY3MubmFtZSA9PT0gc2NoZW1lO1xuICAgICAgfSk7XG4gICAgfVxuICAgIHRoaXMuY29sb3JEb21haW4gPSBzY2hlbWUuZG9tYWluO1xuICAgIHRoaXMuc2NhbGVUeXBlID0gdHlwZTtcbiAgICB0aGlzLmRvbWFpbiA9IGRvbWFpbjtcbiAgICB0aGlzLmN1c3RvbUNvbG9ycyA9IGN1c3RvbUNvbG9ycztcblxuICAgIHRoaXMuc2NhbGUgPSB0aGlzLmdlbmVyYXRlQ29sb3JTY2hlbWUoc2NoZW1lLCB0eXBlLCB0aGlzLmRvbWFpbik7XG4gIH1cblxuICBnZW5lcmF0ZUNvbG9yU2NoZW1lKHNjaGVtZSwgdHlwZSwgZG9tYWluKSB7XG4gICAgaWYgKHR5cGVvZiBzY2hlbWUgPT09ICdzdHJpbmcnKSB7XG4gICAgICBzY2hlbWUgPSBjb2xvclNldHMuZmluZChjcyA9PiB7XG4gICAgICAgIHJldHVybiBjcy5uYW1lID09PSBzY2hlbWU7XG4gICAgICB9KTtcbiAgICB9XG4gICAgbGV0IGNvbG9yU2NhbGU7XG4gICAgaWYgKHR5cGUgPT09ICdxdWFudGlsZScpIHtcbiAgICAgIGNvbG9yU2NhbGUgPSBzY2FsZVF1YW50aWxlKClcbiAgICAgICAgLnJhbmdlKHNjaGVtZS5kb21haW4pXG4gICAgICAgIC5kb21haW4oZG9tYWluKTtcbiAgICB9IGVsc2UgaWYgKHR5cGUgPT09ICdvcmRpbmFsJykge1xuICAgICAgY29sb3JTY2FsZSA9IHNjYWxlT3JkaW5hbCgpXG4gICAgICAgIC5yYW5nZShzY2hlbWUuZG9tYWluKVxuICAgICAgICAuZG9tYWluKGRvbWFpbik7XG4gICAgfSBlbHNlIGlmICh0eXBlID09PSAnbGluZWFyJykge1xuICAgICAgLy8gbGluZWFyIHNjaGVtZXMgbXVzdCBoYXZlIGF0IGxlYXN0IDIgY29sb3JzXG4gICAgICBjb25zdCBjb2xvckRvbWFpbiA9IFsuLi5zY2hlbWUuZG9tYWluXTtcbiAgICAgIGlmIChjb2xvckRvbWFpbi5sZW5ndGggPT09IDEpIHtcbiAgICAgICAgY29sb3JEb21haW4ucHVzaChjb2xvckRvbWFpblswXSk7XG4gICAgICAgIHRoaXMuY29sb3JEb21haW4gPSBjb2xvckRvbWFpbjtcbiAgICAgIH1cblxuICAgICAgY29uc3QgcG9pbnRzID0gcmFuZ2UoMCwgMSwgMS4wIC8gY29sb3JEb21haW4ubGVuZ3RoKTtcbiAgICAgIGNvbG9yU2NhbGUgPSBzY2FsZUxpbmVhcigpXG4gICAgICAgIC5kb21haW4ocG9pbnRzKVxuICAgICAgICAucmFuZ2UoY29sb3JEb21haW4pO1xuICAgIH1cblxuICAgIHJldHVybiBjb2xvclNjYWxlO1xuICB9XG5cbiAgZ2V0Q29sb3IodmFsdWUpIHtcbiAgICBpZiAodmFsdWUgPT09IHVuZGVmaW5lZCB8fCB2YWx1ZSA9PT0gbnVsbCkge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKCdWYWx1ZSBjYW4gbm90IGJlIG51bGwnKTtcbiAgICB9XG4gICAgaWYgKHRoaXMuc2NhbGVUeXBlID09PSAnbGluZWFyJykge1xuICAgICAgY29uc3QgdmFsdWVTY2FsZSA9IHNjYWxlTGluZWFyKClcbiAgICAgICAgLmRvbWFpbih0aGlzLmRvbWFpbilcbiAgICAgICAgLnJhbmdlKFswLCAxXSk7XG5cbiAgICAgIHJldHVybiB0aGlzLnNjYWxlKHZhbHVlU2NhbGUodmFsdWUpKTtcbiAgICB9IGVsc2Uge1xuICAgICAgaWYgKHR5cGVvZiB0aGlzLmN1c3RvbUNvbG9ycyA9PT0gJ2Z1bmN0aW9uJykge1xuICAgICAgICByZXR1cm4gdGhpcy5jdXN0b21Db2xvcnModmFsdWUpO1xuICAgICAgfVxuXG4gICAgICBjb25zdCBmb3JtYXR0ZWRWYWx1ZSA9IHZhbHVlLnRvU3RyaW5nKCk7XG4gICAgICBsZXQgZm91bmQ6IGFueTsgLy8gdG9kbyB0eXBlIGN1c3RvbUNvbG9yc1xuICAgICAgaWYgKHRoaXMuY3VzdG9tQ29sb3JzICYmIHRoaXMuY3VzdG9tQ29sb3JzLmxlbmd0aCA+IDApIHtcbiAgICAgICAgZm91bmQgPSB0aGlzLmN1c3RvbUNvbG9ycy5maW5kKG1hcHBpbmcgPT4ge1xuICAgICAgICAgIHJldHVybiBtYXBwaW5nLm5hbWUudG9Mb3dlckNhc2UoKSA9PT0gZm9ybWF0dGVkVmFsdWUudG9Mb3dlckNhc2UoKTtcbiAgICAgICAgfSk7XG4gICAgICB9XG5cbiAgICAgIGlmIChmb3VuZCkge1xuICAgICAgICByZXR1cm4gZm91bmQudmFsdWU7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICByZXR1cm4gdGhpcy5zY2FsZSh2YWx1ZSk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgZ2V0TGluZWFyR3JhZGllbnRTdG9wcyh2YWx1ZSwgc3RhcnQpIHtcbiAgICBpZiAoc3RhcnQgPT09IHVuZGVmaW5lZCkge1xuICAgICAgc3RhcnQgPSB0aGlzLmRvbWFpblswXTtcbiAgICB9XG5cbiAgICBjb25zdCB2YWx1ZVNjYWxlID0gc2NhbGVMaW5lYXIoKVxuICAgICAgLmRvbWFpbih0aGlzLmRvbWFpbilcbiAgICAgIC5yYW5nZShbMCwgMV0pO1xuXG4gICAgY29uc3QgY29sb3JWYWx1ZVNjYWxlID0gc2NhbGVCYW5kKClcbiAgICAgIC5kb21haW4odGhpcy5jb2xvckRvbWFpbilcbiAgICAgIC5yYW5nZShbMCwgMV0pO1xuXG4gICAgY29uc3QgZW5kQ29sb3IgPSB0aGlzLmdldENvbG9yKHZhbHVlKTtcblxuICAgIC8vIGdlbmVyYXRlIHRoZSBzdG9wc1xuICAgIGNvbnN0IHN0YXJ0VmFsID0gdmFsdWVTY2FsZShzdGFydCk7XG4gICAgY29uc3Qgc3RhcnRDb2xvciA9IHRoaXMuZ2V0Q29sb3Ioc3RhcnQpO1xuXG4gICAgY29uc3QgZW5kVmFsID0gdmFsdWVTY2FsZSh2YWx1ZSk7XG4gICAgbGV0IGkgPSAxO1xuICAgIGxldCBjdXJyZW50VmFsID0gc3RhcnRWYWw7XG4gICAgY29uc3Qgc3RvcHMgPSBbXTtcblxuICAgIHN0b3BzLnB1c2goe1xuICAgICAgY29sb3I6IHN0YXJ0Q29sb3IsXG4gICAgICBvZmZzZXQ6IHN0YXJ0VmFsLFxuICAgICAgb3JpZ2luYWxPZmZzZXQ6IHN0YXJ0VmFsLFxuICAgICAgb3BhY2l0eTogMVxuICAgIH0pO1xuXG4gICAgd2hpbGUgKGN1cnJlbnRWYWwgPCBlbmRWYWwgJiYgaSA8IHRoaXMuY29sb3JEb21haW4ubGVuZ3RoKSB7XG4gICAgICBjb25zdCBjb2xvciA9IHRoaXMuY29sb3JEb21haW5baV07XG4gICAgICBjb25zdCBvZmZzZXQgPSBjb2xvclZhbHVlU2NhbGUoY29sb3IpO1xuICAgICAgaWYgKG9mZnNldCA8PSBzdGFydFZhbCkge1xuICAgICAgICBpKys7XG4gICAgICAgIGNvbnRpbnVlO1xuICAgICAgfVxuXG4gICAgICBpZiAob2Zmc2V0LnRvRml4ZWQoNCkgPj0gKGVuZFZhbCAtIGNvbG9yVmFsdWVTY2FsZS5iYW5kd2lkdGgoKSkudG9GaXhlZCg0KSkge1xuICAgICAgICBicmVhaztcbiAgICAgIH1cblxuICAgICAgc3RvcHMucHVzaCh7XG4gICAgICAgIGNvbG9yLFxuICAgICAgICBvZmZzZXQsXG4gICAgICAgIG9wYWNpdHk6IDFcbiAgICAgIH0pO1xuICAgICAgY3VycmVudFZhbCA9IG9mZnNldDtcbiAgICAgIGkrKztcbiAgICB9XG5cbiAgICBpZiAoc3RvcHNbc3RvcHMubGVuZ3RoIC0gMV0ub2Zmc2V0IDwgMTAwKSB7XG4gICAgICBzdG9wcy5wdXNoKHtcbiAgICAgICAgY29sb3I6IGVuZENvbG9yLFxuICAgICAgICBvZmZzZXQ6IGVuZFZhbCxcbiAgICAgICAgb3BhY2l0eTogMVxuICAgICAgfSk7XG4gICAgfVxuXG4gICAgaWYgKGVuZFZhbCA9PT0gc3RhcnRWYWwpIHtcbiAgICAgIHN0b3BzWzBdLm9mZnNldCA9IDA7XG4gICAgICBzdG9wc1sxXS5vZmZzZXQgPSAxMDA7XG4gICAgfSBlbHNlIHtcbiAgICAgIC8vIG5vcm1hbGl6ZSB0aGUgb2Zmc2V0cyBpbnRvIHBlcmNlbnRhZ2VzXG4gICAgICBpZiAoc3RvcHNbc3RvcHMubGVuZ3RoIC0gMV0ub2Zmc2V0ICE9PSAxMDApIHtcbiAgICAgICAgZm9yIChjb25zdCBzIG9mIHN0b3BzKSB7XG4gICAgICAgICAgcy5vZmZzZXQgPSAoKHMub2Zmc2V0IC0gc3RhcnRWYWwpIC8gKGVuZFZhbCAtIHN0YXJ0VmFsKSkgKiAxMDA7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gc3RvcHM7XG4gIH1cbn1cbiJdfQ==