export function calculateViewDimensions(_a) {
    var width = _a.width, height = _a.height, margins = _a.margins, _b = _a.showXAxis, showXAxis = _b === void 0 ? false : _b, _c = _a.showYAxis, showYAxis = _c === void 0 ? false : _c, _d = _a.xAxisHeight, xAxisHeight = _d === void 0 ? 0 : _d, _e = _a.yAxisWidth, yAxisWidth = _e === void 0 ? 0 : _e, _f = _a.showXLabel, showXLabel = _f === void 0 ? false : _f, _g = _a.showYLabel, showYLabel = _g === void 0 ? false : _g, _h = _a.showLegend, showLegend = _h === void 0 ? false : _h, _j = _a.legendType, legendType = _j === void 0 ? 'ordinal' : _j, _k = _a.legendPosition, legendPosition = _k === void 0 ? 'right' : _k, _l = _a.columns, columns = _l === void 0 ? 12 : _l;
    var xOffset = margins[3];
    var chartWidth = width;
    var chartHeight = height - margins[0] - margins[2];
    if (showLegend && legendPosition === 'right') {
        if (legendType === 'ordinal') {
            columns -= 2;
        }
        else {
            columns -= 1;
        }
    }
    chartWidth = (chartWidth * columns) / 12;
    chartWidth = chartWidth - margins[1] - margins[3];
    if (showXAxis) {
        chartHeight -= 5;
        chartHeight -= xAxisHeight;
        if (showXLabel) {
            // text height + spacing between axis label and tick labels
            var offset = 25 + 5;
            chartHeight -= offset;
        }
    }
    if (showYAxis) {
        chartWidth -= 5;
        chartWidth -= yAxisWidth;
        xOffset += yAxisWidth;
        xOffset += 10;
        if (showYLabel) {
            // text height + spacing between axis label and tick labels
            var offset = 25 + 5;
            chartWidth -= offset;
            xOffset += offset;
        }
    }
    chartWidth = Math.max(0, chartWidth);
    chartHeight = Math.max(0, chartHeight);
    return {
        width: Math.floor(chartWidth),
        height: Math.floor(chartHeight),
        xOffset: Math.floor(xOffset)
    };
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidmlldy1kaW1lbnNpb25zLmhlbHBlci5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi92aWV3LWRpbWVuc2lvbnMuaGVscGVyLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQU1BLE1BQU0sVUFBVSx1QkFBdUIsQ0FBQyxFQWN2QztRQWJDLGdCQUFLLEVBQ0wsa0JBQU0sRUFDTixvQkFBTyxFQUNQLGlCQUFpQixFQUFqQixzQ0FBaUIsRUFDakIsaUJBQWlCLEVBQWpCLHNDQUFpQixFQUNqQixtQkFBZSxFQUFmLG9DQUFlLEVBQ2Ysa0JBQWMsRUFBZCxtQ0FBYyxFQUNkLGtCQUFrQixFQUFsQix1Q0FBa0IsRUFDbEIsa0JBQWtCLEVBQWxCLHVDQUFrQixFQUNsQixrQkFBa0IsRUFBbEIsdUNBQWtCLEVBQ2xCLGtCQUFzQixFQUF0QiwyQ0FBc0IsRUFDdEIsc0JBQXdCLEVBQXhCLDZDQUF3QixFQUN4QixlQUFZLEVBQVosaUNBQVk7SUFFWixJQUFJLE9BQU8sR0FBRyxPQUFPLENBQUMsQ0FBQyxDQUFDLENBQUM7SUFDekIsSUFBSSxVQUFVLEdBQUcsS0FBSyxDQUFDO0lBQ3ZCLElBQUksV0FBVyxHQUFHLE1BQU0sR0FBRyxPQUFPLENBQUMsQ0FBQyxDQUFDLEdBQUcsT0FBTyxDQUFDLENBQUMsQ0FBQyxDQUFDO0lBRW5ELElBQUksVUFBVSxJQUFJLGNBQWMsS0FBSyxPQUFPLEVBQUU7UUFDNUMsSUFBSSxVQUFVLEtBQUssU0FBUyxFQUFFO1lBQzVCLE9BQU8sSUFBSSxDQUFDLENBQUM7U0FDZDthQUFNO1lBQ0wsT0FBTyxJQUFJLENBQUMsQ0FBQztTQUNkO0tBQ0Y7SUFFRCxVQUFVLEdBQUcsQ0FBQyxVQUFVLEdBQUcsT0FBTyxDQUFDLEdBQUcsRUFBRSxDQUFDO0lBRXpDLFVBQVUsR0FBRyxVQUFVLEdBQUcsT0FBTyxDQUFDLENBQUMsQ0FBQyxHQUFHLE9BQU8sQ0FBQyxDQUFDLENBQUMsQ0FBQztJQUVsRCxJQUFJLFNBQVMsRUFBRTtRQUNiLFdBQVcsSUFBSSxDQUFDLENBQUM7UUFDakIsV0FBVyxJQUFJLFdBQVcsQ0FBQztRQUUzQixJQUFJLFVBQVUsRUFBRTtZQUNkLDJEQUEyRDtZQUMzRCxJQUFNLE1BQU0sR0FBRyxFQUFFLEdBQUcsQ0FBQyxDQUFDO1lBQ3RCLFdBQVcsSUFBSSxNQUFNLENBQUM7U0FDdkI7S0FDRjtJQUVELElBQUksU0FBUyxFQUFFO1FBQ2IsVUFBVSxJQUFJLENBQUMsQ0FBQztRQUNoQixVQUFVLElBQUksVUFBVSxDQUFDO1FBQ3pCLE9BQU8sSUFBSSxVQUFVLENBQUM7UUFDdEIsT0FBTyxJQUFJLEVBQUUsQ0FBQztRQUVkLElBQUksVUFBVSxFQUFFO1lBQ2QsMkRBQTJEO1lBQzNELElBQU0sTUFBTSxHQUFHLEVBQUUsR0FBRyxDQUFDLENBQUM7WUFDdEIsVUFBVSxJQUFJLE1BQU0sQ0FBQztZQUNyQixPQUFPLElBQUksTUFBTSxDQUFDO1NBQ25CO0tBQ0Y7SUFFRCxVQUFVLEdBQUcsSUFBSSxDQUFDLEdBQUcsQ0FBQyxDQUFDLEVBQUUsVUFBVSxDQUFDLENBQUM7SUFDckMsV0FBVyxHQUFHLElBQUksQ0FBQyxHQUFHLENBQUMsQ0FBQyxFQUFFLFdBQVcsQ0FBQyxDQUFDO0lBRXZDLE9BQU87UUFDTCxLQUFLLEVBQUUsSUFBSSxDQUFDLEtBQUssQ0FBQyxVQUFVLENBQUM7UUFDN0IsTUFBTSxFQUFFLElBQUksQ0FBQyxLQUFLLENBQUMsV0FBVyxDQUFDO1FBQy9CLE9BQU8sRUFBRSxJQUFJLENBQUMsS0FBSyxDQUFDLE9BQU8sQ0FBQztLQUM3QixDQUFDO0FBQ0osQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbImV4cG9ydCBpbnRlcmZhY2UgVmlld0RpbWVuc2lvbnMge1xuICB3aWR0aDogbnVtYmVyO1xuICBoZWlnaHQ6IG51bWJlcjtcbiAgeE9mZnNldDogbnVtYmVyO1xufVxuXG5leHBvcnQgZnVuY3Rpb24gY2FsY3VsYXRlVmlld0RpbWVuc2lvbnMoe1xuICB3aWR0aCxcbiAgaGVpZ2h0LFxuICBtYXJnaW5zLFxuICBzaG93WEF4aXMgPSBmYWxzZSxcbiAgc2hvd1lBeGlzID0gZmFsc2UsXG4gIHhBeGlzSGVpZ2h0ID0gMCxcbiAgeUF4aXNXaWR0aCA9IDAsXG4gIHNob3dYTGFiZWwgPSBmYWxzZSxcbiAgc2hvd1lMYWJlbCA9IGZhbHNlLFxuICBzaG93TGVnZW5kID0gZmFsc2UsXG4gIGxlZ2VuZFR5cGUgPSAnb3JkaW5hbCcsXG4gIGxlZ2VuZFBvc2l0aW9uID0gJ3JpZ2h0JyxcbiAgY29sdW1ucyA9IDEyXG59KTogVmlld0RpbWVuc2lvbnMge1xuICBsZXQgeE9mZnNldCA9IG1hcmdpbnNbM107XG4gIGxldCBjaGFydFdpZHRoID0gd2lkdGg7XG4gIGxldCBjaGFydEhlaWdodCA9IGhlaWdodCAtIG1hcmdpbnNbMF0gLSBtYXJnaW5zWzJdO1xuXG4gIGlmIChzaG93TGVnZW5kICYmIGxlZ2VuZFBvc2l0aW9uID09PSAncmlnaHQnKSB7XG4gICAgaWYgKGxlZ2VuZFR5cGUgPT09ICdvcmRpbmFsJykge1xuICAgICAgY29sdW1ucyAtPSAyO1xuICAgIH0gZWxzZSB7XG4gICAgICBjb2x1bW5zIC09IDE7XG4gICAgfVxuICB9XG5cbiAgY2hhcnRXaWR0aCA9IChjaGFydFdpZHRoICogY29sdW1ucykgLyAxMjtcblxuICBjaGFydFdpZHRoID0gY2hhcnRXaWR0aCAtIG1hcmdpbnNbMV0gLSBtYXJnaW5zWzNdO1xuXG4gIGlmIChzaG93WEF4aXMpIHtcbiAgICBjaGFydEhlaWdodCAtPSA1O1xuICAgIGNoYXJ0SGVpZ2h0IC09IHhBeGlzSGVpZ2h0O1xuXG4gICAgaWYgKHNob3dYTGFiZWwpIHtcbiAgICAgIC8vIHRleHQgaGVpZ2h0ICsgc3BhY2luZyBiZXR3ZWVuIGF4aXMgbGFiZWwgYW5kIHRpY2sgbGFiZWxzXG4gICAgICBjb25zdCBvZmZzZXQgPSAyNSArIDU7XG4gICAgICBjaGFydEhlaWdodCAtPSBvZmZzZXQ7XG4gICAgfVxuICB9XG5cbiAgaWYgKHNob3dZQXhpcykge1xuICAgIGNoYXJ0V2lkdGggLT0gNTtcbiAgICBjaGFydFdpZHRoIC09IHlBeGlzV2lkdGg7XG4gICAgeE9mZnNldCArPSB5QXhpc1dpZHRoO1xuICAgIHhPZmZzZXQgKz0gMTA7XG5cbiAgICBpZiAoc2hvd1lMYWJlbCkge1xuICAgICAgLy8gdGV4dCBoZWlnaHQgKyBzcGFjaW5nIGJldHdlZW4gYXhpcyBsYWJlbCBhbmQgdGljayBsYWJlbHNcbiAgICAgIGNvbnN0IG9mZnNldCA9IDI1ICsgNTtcbiAgICAgIGNoYXJ0V2lkdGggLT0gb2Zmc2V0O1xuICAgICAgeE9mZnNldCArPSBvZmZzZXQ7XG4gICAgfVxuICB9XG5cbiAgY2hhcnRXaWR0aCA9IE1hdGgubWF4KDAsIGNoYXJ0V2lkdGgpO1xuICBjaGFydEhlaWdodCA9IE1hdGgubWF4KDAsIGNoYXJ0SGVpZ2h0KTtcblxuICByZXR1cm4ge1xuICAgIHdpZHRoOiBNYXRoLmZsb29yKGNoYXJ0V2lkdGgpLFxuICAgIGhlaWdodDogTWF0aC5mbG9vcihjaGFydEhlaWdodCksXG4gICAgeE9mZnNldDogTWF0aC5mbG9vcih4T2Zmc2V0KVxuICB9O1xufVxuIl19