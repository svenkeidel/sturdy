/**
 * Converts a hex to RGB
 *
 * @export
 */
export declare function hexToRgb(value: string): any;
/**
 * Accepts a color (string) and returns a inverted hex color (string)
 * http://stackoverflow.com/questions/9600295/automatically-change-text-color-to-assure-readability
 *
 * @export
 */
export declare function invertColor(value: string): string;
/**
 * Given a rgb, it will darken/lighten
 * http://stackoverflow.com/questions/5560248/programmatically-lighten-or-darken-a-hex-color-or-rgb-and-blend-colors
 *
 * @export
 * @param \{ r, g, b }
 */
export declare function shadeRGBColor({ r, g, b }: {
    r: any;
    g: any;
    b: any;
}, percent: any): string;
