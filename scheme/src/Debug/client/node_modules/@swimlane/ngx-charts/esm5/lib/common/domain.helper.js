import { __values } from "tslib";
/**
 * Based on the data, return an array with unique values.
 *
 * @export
 * @returns array
 */
export function getUniqueXDomainValues(results) {
    var e_1, _a, e_2, _b;
    var valueSet = new Set();
    try {
        for (var results_1 = __values(results), results_1_1 = results_1.next(); !results_1_1.done; results_1_1 = results_1.next()) {
            var result = results_1_1.value;
            try {
                for (var _c = (e_2 = void 0, __values(result.series)), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var d = _d.value;
                    valueSet.add(d.name);
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
    }
    catch (e_1_1) { e_1 = { error: e_1_1 }; }
    finally {
        try {
            if (results_1_1 && !results_1_1.done && (_a = results_1.return)) _a.call(results_1);
        }
        finally { if (e_1) throw e_1.error; }
    }
    return Array.from(valueSet);
}
/**
 * Get the scaleType of enumerable of values.
 * @returns  'time', 'linear' or 'ordinal'
 */
export function getScaleType(values, checkDateType) {
    if (checkDateType === void 0) { checkDateType = true; }
    if (checkDateType) {
        var allDates = values.every(function (value) { return value instanceof Date; });
        if (allDates) {
            return 'time';
        }
    }
    var allNumbers = values.every(function (value) { return typeof value === 'number'; });
    if (allNumbers) {
        return 'linear';
    }
    return 'ordinal';
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZG9tYWluLmhlbHBlci5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi9kb21haW4uaGVscGVyLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQTs7Ozs7R0FLRztBQUNILE1BQU0sVUFBVSxzQkFBc0IsQ0FBQyxPQUFjOztJQUNuRCxJQUFNLFFBQVEsR0FBRyxJQUFJLEdBQUcsRUFBRSxDQUFDOztRQUMzQixLQUFxQixJQUFBLFlBQUEsU0FBQSxPQUFPLENBQUEsZ0NBQUEscURBQUU7WUFBekIsSUFBTSxNQUFNLG9CQUFBOztnQkFDZixLQUFnQixJQUFBLG9CQUFBLFNBQUEsTUFBTSxDQUFDLE1BQU0sQ0FBQSxDQUFBLGdCQUFBLDRCQUFFO29CQUExQixJQUFNLENBQUMsV0FBQTtvQkFDVixRQUFRLENBQUMsR0FBRyxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsQ0FBQztpQkFDdEI7Ozs7Ozs7OztTQUNGOzs7Ozs7Ozs7SUFDRCxPQUFPLEtBQUssQ0FBQyxJQUFJLENBQUMsUUFBUSxDQUFDLENBQUM7QUFDOUIsQ0FBQztBQUVEOzs7R0FHRztBQUNILE1BQU0sVUFBVSxZQUFZLENBQUMsTUFBYSxFQUFFLGFBQW9CO0lBQXBCLDhCQUFBLEVBQUEsb0JBQW9CO0lBQzlELElBQUksYUFBYSxFQUFFO1FBQ2pCLElBQU0sUUFBUSxHQUFHLE1BQU0sQ0FBQyxLQUFLLENBQUMsVUFBQSxLQUFLLElBQUksT0FBQSxLQUFLLFlBQVksSUFBSSxFQUFyQixDQUFxQixDQUFDLENBQUM7UUFDOUQsSUFBSSxRQUFRLEVBQUU7WUFDWixPQUFPLE1BQU0sQ0FBQztTQUNmO0tBQ0Y7SUFFRCxJQUFNLFVBQVUsR0FBRyxNQUFNLENBQUMsS0FBSyxDQUFDLFVBQUEsS0FBSyxJQUFJLE9BQUEsT0FBTyxLQUFLLEtBQUssUUFBUSxFQUF6QixDQUF5QixDQUFDLENBQUM7SUFDcEUsSUFBSSxVQUFVLEVBQUU7UUFDZCxPQUFPLFFBQVEsQ0FBQztLQUNqQjtJQUVELE9BQU8sU0FBUyxDQUFDO0FBQ25CLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEJhc2VkIG9uIHRoZSBkYXRhLCByZXR1cm4gYW4gYXJyYXkgd2l0aCB1bmlxdWUgdmFsdWVzLlxuICpcbiAqIEBleHBvcnRcbiAqIEByZXR1cm5zIGFycmF5XG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRVbmlxdWVYRG9tYWluVmFsdWVzKHJlc3VsdHM6IGFueVtdKTogYW55W10ge1xuICBjb25zdCB2YWx1ZVNldCA9IG5ldyBTZXQoKTtcbiAgZm9yIChjb25zdCByZXN1bHQgb2YgcmVzdWx0cykge1xuICAgIGZvciAoY29uc3QgZCBvZiByZXN1bHQuc2VyaWVzKSB7XG4gICAgICB2YWx1ZVNldC5hZGQoZC5uYW1lKTtcbiAgICB9XG4gIH1cbiAgcmV0dXJuIEFycmF5LmZyb20odmFsdWVTZXQpO1xufVxuXG4vKipcbiAqIEdldCB0aGUgc2NhbGVUeXBlIG9mIGVudW1lcmFibGUgb2YgdmFsdWVzLlxuICogQHJldHVybnMgICd0aW1lJywgJ2xpbmVhcicgb3IgJ29yZGluYWwnXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRTY2FsZVR5cGUodmFsdWVzOiBhbnlbXSwgY2hlY2tEYXRlVHlwZSA9IHRydWUpOiBzdHJpbmcge1xuICBpZiAoY2hlY2tEYXRlVHlwZSkge1xuICAgIGNvbnN0IGFsbERhdGVzID0gdmFsdWVzLmV2ZXJ5KHZhbHVlID0+IHZhbHVlIGluc3RhbmNlb2YgRGF0ZSk7XG4gICAgaWYgKGFsbERhdGVzKSB7XG4gICAgICByZXR1cm4gJ3RpbWUnO1xuICAgIH1cbiAgfVxuXG4gIGNvbnN0IGFsbE51bWJlcnMgPSB2YWx1ZXMuZXZlcnkodmFsdWUgPT4gdHlwZW9mIHZhbHVlID09PSAnbnVtYmVyJyk7XG4gIGlmIChhbGxOdW1iZXJzKSB7XG4gICAgcmV0dXJuICdsaW5lYXInO1xuICB9XG5cbiAgcmV0dXJuICdvcmRpbmFsJztcbn1cbiJdfQ==