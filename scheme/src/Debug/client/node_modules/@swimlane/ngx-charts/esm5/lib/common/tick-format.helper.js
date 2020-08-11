import { timeFormat } from 'd3-time-format';
export function tickFormat(fieldType, groupByType) {
    return function (label) {
        if (label === 'No Value' || label === 'Other') {
            return label;
        }
        if (fieldType === 'date' && groupByType === 'groupBy') {
            var formatter = timeFormat('MM/DD/YYYY');
            return formatter(label);
        }
        return label.toString();
    };
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidGljay1mb3JtYXQuaGVscGVyLmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvY29tbW9uL3RpY2stZm9ybWF0LmhlbHBlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQSxPQUFPLEVBQUUsVUFBVSxFQUFFLE1BQU0sZ0JBQWdCLENBQUM7QUFFNUMsTUFBTSxVQUFVLFVBQVUsQ0FBQyxTQUFTLEVBQUUsV0FBVztJQUMvQyxPQUFPLFVBQVMsS0FBYTtRQUMzQixJQUFJLEtBQUssS0FBSyxVQUFVLElBQUksS0FBSyxLQUFLLE9BQU8sRUFBRTtZQUM3QyxPQUFPLEtBQUssQ0FBQztTQUNkO1FBQ0QsSUFBSSxTQUFTLEtBQUssTUFBTSxJQUFJLFdBQVcsS0FBSyxTQUFTLEVBQUU7WUFDckQsSUFBTSxTQUFTLEdBQUcsVUFBVSxDQUFDLFlBQVksQ0FBQyxDQUFDO1lBQzNDLE9BQU8sU0FBUyxDQUFNLEtBQUssQ0FBQyxDQUFDO1NBQzlCO1FBRUQsT0FBTyxLQUFLLENBQUMsUUFBUSxFQUFFLENBQUM7SUFDMUIsQ0FBQyxDQUFDO0FBQ0osQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IHRpbWVGb3JtYXQgfSBmcm9tICdkMy10aW1lLWZvcm1hdCc7XG5cbmV4cG9ydCBmdW5jdGlvbiB0aWNrRm9ybWF0KGZpZWxkVHlwZSwgZ3JvdXBCeVR5cGUpOiAobGFiZWw6IHN0cmluZykgPT4gc3RyaW5nIHtcbiAgcmV0dXJuIGZ1bmN0aW9uKGxhYmVsOiBzdHJpbmcpOiBzdHJpbmcge1xuICAgIGlmIChsYWJlbCA9PT0gJ05vIFZhbHVlJyB8fCBsYWJlbCA9PT0gJ090aGVyJykge1xuICAgICAgcmV0dXJuIGxhYmVsO1xuICAgIH1cbiAgICBpZiAoZmllbGRUeXBlID09PSAnZGF0ZScgJiYgZ3JvdXBCeVR5cGUgPT09ICdncm91cEJ5Jykge1xuICAgICAgY29uc3QgZm9ybWF0dGVyID0gdGltZUZvcm1hdCgnTU0vREQvWVlZWScpO1xuICAgICAgcmV0dXJuIGZvcm1hdHRlcig8YW55PmxhYmVsKTtcbiAgICB9XG5cbiAgICByZXR1cm4gbGFiZWwudG9TdHJpbmcoKTtcbiAgfTtcbn1cbiJdfQ==