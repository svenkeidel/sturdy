/**
 * Formats a label given a date, number or string.
 *
 * @export
 */
export function formatLabel(label) {
    if (label instanceof Date) {
        label = label.toLocaleDateString();
    }
    else {
        label = label.toLocaleString();
    }
    return label;
}
/**
 * Escapes a label.
 *
 * @export
 */
export function escapeLabel(label) {
    return label.toLocaleString().replace(/[&'`"<>]/g, function (match) {
        return {
            '&': '&amp;',
            // tslint:disable-next-line: quotemark
            "'": '&#x27;',
            '`': '&#x60;',
            '"': '&quot;',
            '<': '&lt;',
            '>': '&gt;'
        }[match];
    });
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibGFiZWwuaGVscGVyLmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvY29tbW9uL2xhYmVsLmhlbHBlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7OztHQUlHO0FBQ0gsTUFBTSxVQUFVLFdBQVcsQ0FBQyxLQUFVO0lBQ3BDLElBQUksS0FBSyxZQUFZLElBQUksRUFBRTtRQUN6QixLQUFLLEdBQUcsS0FBSyxDQUFDLGtCQUFrQixFQUFFLENBQUM7S0FDcEM7U0FBTTtRQUNMLEtBQUssR0FBRyxLQUFLLENBQUMsY0FBYyxFQUFFLENBQUM7S0FDaEM7SUFFRCxPQUFPLEtBQUssQ0FBQztBQUNmLENBQUM7QUFFRDs7OztHQUlHO0FBQ0gsTUFBTSxVQUFVLFdBQVcsQ0FBQyxLQUFVO0lBQ3BDLE9BQU8sS0FBSyxDQUFDLGNBQWMsRUFBRSxDQUFDLE9BQU8sQ0FBQyxXQUFXLEVBQUUsVUFBQSxLQUFLO1FBQ3RELE9BQU87WUFDTCxHQUFHLEVBQUUsT0FBTztZQUNaLHNDQUFzQztZQUN0QyxHQUFHLEVBQUUsUUFBUTtZQUNiLEdBQUcsRUFBRSxRQUFRO1lBQ2IsR0FBRyxFQUFFLFFBQVE7WUFDYixHQUFHLEVBQUUsTUFBTTtZQUNYLEdBQUcsRUFBRSxNQUFNO1NBQ1osQ0FBQyxLQUFLLENBQUMsQ0FBQztJQUNYLENBQUMsQ0FBQyxDQUFDO0FBQ0wsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogRm9ybWF0cyBhIGxhYmVsIGdpdmVuIGEgZGF0ZSwgbnVtYmVyIG9yIHN0cmluZy5cbiAqXG4gKiBAZXhwb3J0XG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBmb3JtYXRMYWJlbChsYWJlbDogYW55KTogc3RyaW5nIHtcbiAgaWYgKGxhYmVsIGluc3RhbmNlb2YgRGF0ZSkge1xuICAgIGxhYmVsID0gbGFiZWwudG9Mb2NhbGVEYXRlU3RyaW5nKCk7XG4gIH0gZWxzZSB7XG4gICAgbGFiZWwgPSBsYWJlbC50b0xvY2FsZVN0cmluZygpO1xuICB9XG5cbiAgcmV0dXJuIGxhYmVsO1xufVxuXG4vKipcbiAqIEVzY2FwZXMgYSBsYWJlbC5cbiAqXG4gKiBAZXhwb3J0XG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBlc2NhcGVMYWJlbChsYWJlbDogYW55KTogc3RyaW5nIHtcbiAgcmV0dXJuIGxhYmVsLnRvTG9jYWxlU3RyaW5nKCkucmVwbGFjZSgvWyYnYFwiPD5dL2csIG1hdGNoID0+IHtcbiAgICByZXR1cm4ge1xuICAgICAgJyYnOiAnJmFtcDsnLFxuICAgICAgLy8gdHNsaW50OmRpc2FibGUtbmV4dC1saW5lOiBxdW90ZW1hcmtcbiAgICAgIFwiJ1wiOiAnJiN4Mjc7JyxcbiAgICAgICdgJzogJyYjeDYwOycsXG4gICAgICAnXCInOiAnJnF1b3Q7JyxcbiAgICAgICc8JzogJyZsdDsnLFxuICAgICAgJz4nOiAnJmd0OydcbiAgICB9W21hdGNoXTtcbiAgfSk7XG59XG4iXX0=