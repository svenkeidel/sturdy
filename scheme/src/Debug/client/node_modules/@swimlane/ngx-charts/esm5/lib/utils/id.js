var cache = {};
/**
 * Generates a short id.
 *
 * Description:
 *   A 4-character alphanumeric sequence (364 = 1.6 million)
 *   This should only be used for JavaScript specific models.
 *   http://stackoverflow.com/questions/6248666/how-to-generate-short-uid-like-ax4j9z-in-js
 *
 *   Example: `ebgf`
 */
export function id() {
    var newId = ('0000' + ((Math.random() * Math.pow(36, 4)) << 0).toString(36)).slice(-4);
    // append a 'a' because neo gets mad
    newId = "a" + newId;
    // ensure not already used
    if (!cache[newId]) {
        cache[newId] = true;
        return newId;
    }
    return id();
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaWQuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi91dGlscy9pZC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQSxJQUFNLEtBQUssR0FBRyxFQUFFLENBQUM7QUFFakI7Ozs7Ozs7OztHQVNHO0FBQ0gsTUFBTSxVQUFVLEVBQUU7SUFDaEIsSUFBSSxLQUFLLEdBQUcsQ0FBQyxNQUFNLEdBQUcsQ0FBQyxDQUFDLElBQUksQ0FBQyxNQUFNLEVBQUUsR0FBRyxJQUFJLENBQUMsR0FBRyxDQUFDLEVBQUUsRUFBRSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLFFBQVEsQ0FBQyxFQUFFLENBQUMsQ0FBQyxDQUFDLEtBQUssQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDO0lBRXZGLG9DQUFvQztJQUNwQyxLQUFLLEdBQUcsTUFBSSxLQUFPLENBQUM7SUFFcEIsMEJBQTBCO0lBQzFCLElBQUksQ0FBQyxLQUFLLENBQUMsS0FBSyxDQUFDLEVBQUU7UUFDakIsS0FBSyxDQUFDLEtBQUssQ0FBQyxHQUFHLElBQUksQ0FBQztRQUNwQixPQUFPLEtBQUssQ0FBQztLQUNkO0lBRUQsT0FBTyxFQUFFLEVBQUUsQ0FBQztBQUNkLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyJjb25zdCBjYWNoZSA9IHt9O1xuXG4vKipcbiAqIEdlbmVyYXRlcyBhIHNob3J0IGlkLlxuICpcbiAqIERlc2NyaXB0aW9uOlxuICogICBBIDQtY2hhcmFjdGVyIGFscGhhbnVtZXJpYyBzZXF1ZW5jZSAoMzY0ID0gMS42IG1pbGxpb24pXG4gKiAgIFRoaXMgc2hvdWxkIG9ubHkgYmUgdXNlZCBmb3IgSmF2YVNjcmlwdCBzcGVjaWZpYyBtb2RlbHMuXG4gKiAgIGh0dHA6Ly9zdGFja292ZXJmbG93LmNvbS9xdWVzdGlvbnMvNjI0ODY2Ni9ob3ctdG8tZ2VuZXJhdGUtc2hvcnQtdWlkLWxpa2UtYXg0ajl6LWluLWpzXG4gKlxuICogICBFeGFtcGxlOiBgZWJnZmBcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGlkKCk6IHN0cmluZyB7XG4gIGxldCBuZXdJZCA9ICgnMDAwMCcgKyAoKE1hdGgucmFuZG9tKCkgKiBNYXRoLnBvdygzNiwgNCkpIDw8IDApLnRvU3RyaW5nKDM2KSkuc2xpY2UoLTQpO1xuXG4gIC8vIGFwcGVuZCBhICdhJyBiZWNhdXNlIG5lbyBnZXRzIG1hZFxuICBuZXdJZCA9IGBhJHtuZXdJZH1gO1xuXG4gIC8vIGVuc3VyZSBub3QgYWxyZWFkeSB1c2VkXG4gIGlmICghY2FjaGVbbmV3SWRdKSB7XG4gICAgY2FjaGVbbmV3SWRdID0gdHJ1ZTtcbiAgICByZXR1cm4gbmV3SWQ7XG4gIH1cblxuICByZXR1cm4gaWQoKTtcbn1cbiJdfQ==