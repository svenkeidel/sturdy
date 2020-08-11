/**
 * Position helper for the popover directive.
 *
 * @export
 */
export declare class PositionHelper {
    /**
     * Calculate vertical alignment position
     *
     * @memberOf PositionHelper
     */
    static calculateVerticalAlignment(elDimensions: any, popoverDimensions: any, alignment: any): number;
    /**
     * Calculate vertical caret position
     *
     * @memberOf PositionHelper
     */
    static calculateVerticalCaret(elDimensions: any, popoverDimensions: any, caretDimensions: any, alignment: any): number;
    /**
     * Calculate horz alignment position
     *
     * @memberOf PositionHelper
     */
    static calculateHorizontalAlignment(elDimensions: any, popoverDimensions: any, alignment: any): number;
    /**
     * Calculate horz caret position
     *
     * @memberOf PositionHelper
     */
    static calculateHorizontalCaret(elDimensions: any, popoverDimensions: any, caretDimensions: any, alignment: any): number;
    /**
     * Checks if the element's position should be flipped
     *
     * @memberOf PositionHelper
     */
    static shouldFlip(elDimensions: any, popoverDimensions: any, placement: any, spacing: any): boolean;
    /**
     * Position caret
     *
     * @memberOf PositionHelper
     */
    static positionCaret(placement: any, elmDim: any, hostDim: any, caretDimensions: any, alignment: any): any;
    /**
     * Position content
     *
     * @memberOf PositionHelper
     */
    static positionContent(placement: any, elmDim: any, hostDim: any, spacing: any, alignment: any): any;
    /**
     * Determine placement based on flip
     *
     * @memberOf PositionHelper
     */
    static determinePlacement(placement: any, elmDim: any, hostDim: any, spacing: any): any;
}
