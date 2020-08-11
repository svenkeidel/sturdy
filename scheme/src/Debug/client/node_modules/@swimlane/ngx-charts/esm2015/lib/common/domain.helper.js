/**
 * Based on the data, return an array with unique values.
 *
 * @export
 * @returns array
 */
export function getUniqueXDomainValues(results) {
    const valueSet = new Set();
    for (const result of results) {
        for (const d of result.series) {
            valueSet.add(d.name);
        }
    }
    return Array.from(valueSet);
}
/**
 * Get the scaleType of enumerable of values.
 * @returns  'time', 'linear' or 'ordinal'
 */
export function getScaleType(values, checkDateType = true) {
    if (checkDateType) {
        const allDates = values.every(value => value instanceof Date);
        if (allDates) {
            return 'time';
        }
    }
    const allNumbers = values.every(value => typeof value === 'number');
    if (allNumbers) {
        return 'linear';
    }
    return 'ordinal';
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZG9tYWluLmhlbHBlci5qcyIsInNvdXJjZVJvb3QiOiJuZzovL0Bzd2ltbGFuZS9uZ3gtY2hhcnRzLyIsInNvdXJjZXMiOlsibGliL2NvbW1vbi9kb21haW4uaGVscGVyLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7OztHQUtHO0FBQ0gsTUFBTSxVQUFVLHNCQUFzQixDQUFDLE9BQWM7SUFDbkQsTUFBTSxRQUFRLEdBQUcsSUFBSSxHQUFHLEVBQUUsQ0FBQztJQUMzQixLQUFLLE1BQU0sTUFBTSxJQUFJLE9BQU8sRUFBRTtRQUM1QixLQUFLLE1BQU0sQ0FBQyxJQUFJLE1BQU0sQ0FBQyxNQUFNLEVBQUU7WUFDN0IsUUFBUSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLENBQUM7U0FDdEI7S0FDRjtJQUNELE9BQU8sS0FBSyxDQUFDLElBQUksQ0FBQyxRQUFRLENBQUMsQ0FBQztBQUM5QixDQUFDO0FBRUQ7OztHQUdHO0FBQ0gsTUFBTSxVQUFVLFlBQVksQ0FBQyxNQUFhLEVBQUUsYUFBYSxHQUFHLElBQUk7SUFDOUQsSUFBSSxhQUFhLEVBQUU7UUFDakIsTUFBTSxRQUFRLEdBQUcsTUFBTSxDQUFDLEtBQUssQ0FBQyxLQUFLLENBQUMsRUFBRSxDQUFDLEtBQUssWUFBWSxJQUFJLENBQUMsQ0FBQztRQUM5RCxJQUFJLFFBQVEsRUFBRTtZQUNaLE9BQU8sTUFBTSxDQUFDO1NBQ2Y7S0FDRjtJQUVELE1BQU0sVUFBVSxHQUFHLE1BQU0sQ0FBQyxLQUFLLENBQUMsS0FBSyxDQUFDLEVBQUUsQ0FBQyxPQUFPLEtBQUssS0FBSyxRQUFRLENBQUMsQ0FBQztJQUNwRSxJQUFJLFVBQVUsRUFBRTtRQUNkLE9BQU8sUUFBUSxDQUFDO0tBQ2pCO0lBRUQsT0FBTyxTQUFTLENBQUM7QUFDbkIsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQmFzZWQgb24gdGhlIGRhdGEsIHJldHVybiBhbiBhcnJheSB3aXRoIHVuaXF1ZSB2YWx1ZXMuXG4gKlxuICogQGV4cG9ydFxuICogQHJldHVybnMgYXJyYXlcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGdldFVuaXF1ZVhEb21haW5WYWx1ZXMocmVzdWx0czogYW55W10pOiBhbnlbXSB7XG4gIGNvbnN0IHZhbHVlU2V0ID0gbmV3IFNldCgpO1xuICBmb3IgKGNvbnN0IHJlc3VsdCBvZiByZXN1bHRzKSB7XG4gICAgZm9yIChjb25zdCBkIG9mIHJlc3VsdC5zZXJpZXMpIHtcbiAgICAgIHZhbHVlU2V0LmFkZChkLm5hbWUpO1xuICAgIH1cbiAgfVxuICByZXR1cm4gQXJyYXkuZnJvbSh2YWx1ZVNldCk7XG59XG5cbi8qKlxuICogR2V0IHRoZSBzY2FsZVR5cGUgb2YgZW51bWVyYWJsZSBvZiB2YWx1ZXMuXG4gKiBAcmV0dXJucyAgJ3RpbWUnLCAnbGluZWFyJyBvciAnb3JkaW5hbCdcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGdldFNjYWxlVHlwZSh2YWx1ZXM6IGFueVtdLCBjaGVja0RhdGVUeXBlID0gdHJ1ZSk6IHN0cmluZyB7XG4gIGlmIChjaGVja0RhdGVUeXBlKSB7XG4gICAgY29uc3QgYWxsRGF0ZXMgPSB2YWx1ZXMuZXZlcnkodmFsdWUgPT4gdmFsdWUgaW5zdGFuY2VvZiBEYXRlKTtcbiAgICBpZiAoYWxsRGF0ZXMpIHtcbiAgICAgIHJldHVybiAndGltZSc7XG4gICAgfVxuICB9XG5cbiAgY29uc3QgYWxsTnVtYmVycyA9IHZhbHVlcy5ldmVyeSh2YWx1ZSA9PiB0eXBlb2YgdmFsdWUgPT09ICdudW1iZXInKTtcbiAgaWYgKGFsbE51bWJlcnMpIHtcbiAgICByZXR1cm4gJ2xpbmVhcic7XG4gIH1cblxuICByZXR1cm4gJ29yZGluYWwnO1xufVxuIl19