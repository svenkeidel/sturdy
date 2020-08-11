import { PlacementTypes } from './placement.type';
const caretOffset = 7;
function verticalPosition(elDimensions, popoverDimensions, alignment) {
    if (alignment === 'top') {
        return elDimensions.top - caretOffset;
    }
    if (alignment === 'bottom') {
        return elDimensions.top + elDimensions.height - popoverDimensions.height + caretOffset;
    }
    if (alignment === 'center') {
        return elDimensions.top + elDimensions.height / 2 - popoverDimensions.height / 2;
    }
    return undefined;
}
function horizontalPosition(elDimensions, popoverDimensions, alignment) {
    if (alignment === 'left') {
        return elDimensions.left - caretOffset;
    }
    if (alignment === 'right') {
        return elDimensions.left + elDimensions.width - popoverDimensions.width + caretOffset;
    }
    if (alignment === 'center') {
        return elDimensions.left + elDimensions.width / 2 - popoverDimensions.width / 2;
    }
    return undefined;
}
/**
 * Position helper for the popover directive.
 *
 * @export
 */
export class PositionHelper {
    /**
     * Calculate vertical alignment position
     *
     * @memberOf PositionHelper
     */
    static calculateVerticalAlignment(elDimensions, popoverDimensions, alignment) {
        let result = verticalPosition(elDimensions, popoverDimensions, alignment);
        if (result + popoverDimensions.height > window.innerHeight) {
            result = window.innerHeight - popoverDimensions.height;
        }
        return result;
    }
    /**
     * Calculate vertical caret position
     *
     * @memberOf PositionHelper
     */
    static calculateVerticalCaret(elDimensions, popoverDimensions, caretDimensions, alignment) {
        let result;
        if (alignment === 'top') {
            result = elDimensions.height / 2 - caretDimensions.height / 2 + caretOffset;
        }
        if (alignment === 'bottom') {
            result = popoverDimensions.height - elDimensions.height / 2 - caretDimensions.height / 2 - caretOffset;
        }
        if (alignment === 'center') {
            result = popoverDimensions.height / 2 - caretDimensions.height / 2;
        }
        const popoverPosition = verticalPosition(elDimensions, popoverDimensions, alignment);
        if (popoverPosition + popoverDimensions.height > window.innerHeight) {
            result += popoverPosition + popoverDimensions.height - window.innerHeight;
        }
        return result;
    }
    /**
     * Calculate horz alignment position
     *
     * @memberOf PositionHelper
     */
    static calculateHorizontalAlignment(elDimensions, popoverDimensions, alignment) {
        let result = horizontalPosition(elDimensions, popoverDimensions, alignment);
        if (result + popoverDimensions.width > window.innerWidth) {
            result = window.innerWidth - popoverDimensions.width;
        }
        return result;
    }
    /**
     * Calculate horz caret position
     *
     * @memberOf PositionHelper
     */
    static calculateHorizontalCaret(elDimensions, popoverDimensions, caretDimensions, alignment) {
        let result;
        if (alignment === 'left') {
            result = elDimensions.width / 2 - caretDimensions.width / 2 + caretOffset;
        }
        if (alignment === 'right') {
            result = popoverDimensions.width - elDimensions.width / 2 - caretDimensions.width / 2 - caretOffset;
        }
        if (alignment === 'center') {
            result = popoverDimensions.width / 2 - caretDimensions.width / 2;
        }
        const popoverPosition = horizontalPosition(elDimensions, popoverDimensions, alignment);
        if (popoverPosition + popoverDimensions.width > window.innerWidth) {
            result += popoverPosition + popoverDimensions.width - window.innerWidth;
        }
        return result;
    }
    /**
     * Checks if the element's position should be flipped
     *
     * @memberOf PositionHelper
     */
    static shouldFlip(elDimensions, popoverDimensions, placement, spacing) {
        let flip = false;
        if (placement === 'right') {
            if (elDimensions.left + elDimensions.width + popoverDimensions.width + spacing > window.innerWidth) {
                flip = true;
            }
        }
        if (placement === 'left') {
            if (elDimensions.left - popoverDimensions.width - spacing < 0) {
                flip = true;
            }
        }
        if (placement === 'top') {
            if (elDimensions.top - popoverDimensions.height - spacing < 0) {
                flip = true;
            }
        }
        if (placement === 'bottom') {
            if (elDimensions.top + elDimensions.height + popoverDimensions.height + spacing > window.innerHeight) {
                flip = true;
            }
        }
        return flip;
    }
    /**
     * Position caret
     *
     * @memberOf PositionHelper
     */
    static positionCaret(placement, elmDim, hostDim, caretDimensions, alignment) {
        let top = 0;
        let left = 0;
        if (placement === PlacementTypes.right) {
            left = -7;
            top = PositionHelper.calculateVerticalCaret(hostDim, elmDim, caretDimensions, alignment);
        }
        else if (placement === PlacementTypes.left) {
            left = elmDim.width;
            top = PositionHelper.calculateVerticalCaret(hostDim, elmDim, caretDimensions, alignment);
        }
        else if (placement === PlacementTypes.top) {
            top = elmDim.height;
            left = PositionHelper.calculateHorizontalCaret(hostDim, elmDim, caretDimensions, alignment);
        }
        else if (placement === PlacementTypes.bottom) {
            top = -7;
            left = PositionHelper.calculateHorizontalCaret(hostDim, elmDim, caretDimensions, alignment);
        }
        return { top, left };
    }
    /**
     * Position content
     *
     * @memberOf PositionHelper
     */
    static positionContent(placement, elmDim, hostDim, spacing, alignment) {
        let top = 0;
        let left = 0;
        if (placement === PlacementTypes.right) {
            left = hostDim.left + hostDim.width + spacing;
            top = PositionHelper.calculateVerticalAlignment(hostDim, elmDim, alignment);
        }
        else if (placement === PlacementTypes.left) {
            left = hostDim.left - elmDim.width - spacing;
            top = PositionHelper.calculateVerticalAlignment(hostDim, elmDim, alignment);
        }
        else if (placement === PlacementTypes.top) {
            top = hostDim.top - elmDim.height - spacing;
            left = PositionHelper.calculateHorizontalAlignment(hostDim, elmDim, alignment);
        }
        else if (placement === PlacementTypes.bottom) {
            top = hostDim.top + hostDim.height + spacing;
            left = PositionHelper.calculateHorizontalAlignment(hostDim, elmDim, alignment);
        }
        return { top, left };
    }
    /**
     * Determine placement based on flip
     *
     * @memberOf PositionHelper
     */
    static determinePlacement(placement, elmDim, hostDim, spacing) {
        const shouldFlip = PositionHelper.shouldFlip(hostDim, elmDim, placement, spacing);
        if (shouldFlip) {
            if (placement === PlacementTypes.right) {
                return PlacementTypes.left;
            }
            else if (placement === PlacementTypes.left) {
                return PlacementTypes.right;
            }
            else if (placement === PlacementTypes.top) {
                return PlacementTypes.bottom;
            }
            else if (placement === PlacementTypes.bottom) {
                return PlacementTypes.top;
            }
        }
        return placement;
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicG9zaXRpb24uanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWNoYXJ0cy8iLCJzb3VyY2VzIjpbImxpYi9jb21tb24vdG9vbHRpcC9wb3NpdGlvbi9wb3NpdGlvbi50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQSxPQUFPLEVBQUUsY0FBYyxFQUFFLE1BQU0sa0JBQWtCLENBQUM7QUFFbEQsTUFBTSxXQUFXLEdBQUcsQ0FBQyxDQUFDO0FBRXRCLFNBQVMsZ0JBQWdCLENBQUMsWUFBWSxFQUFFLGlCQUFpQixFQUFFLFNBQVM7SUFDbEUsSUFBSSxTQUFTLEtBQUssS0FBSyxFQUFFO1FBQ3ZCLE9BQU8sWUFBWSxDQUFDLEdBQUcsR0FBRyxXQUFXLENBQUM7S0FDdkM7SUFFRCxJQUFJLFNBQVMsS0FBSyxRQUFRLEVBQUU7UUFDMUIsT0FBTyxZQUFZLENBQUMsR0FBRyxHQUFHLFlBQVksQ0FBQyxNQUFNLEdBQUcsaUJBQWlCLENBQUMsTUFBTSxHQUFHLFdBQVcsQ0FBQztLQUN4RjtJQUVELElBQUksU0FBUyxLQUFLLFFBQVEsRUFBRTtRQUMxQixPQUFPLFlBQVksQ0FBQyxHQUFHLEdBQUcsWUFBWSxDQUFDLE1BQU0sR0FBRyxDQUFDLEdBQUcsaUJBQWlCLENBQUMsTUFBTSxHQUFHLENBQUMsQ0FBQztLQUNsRjtJQUVELE9BQU8sU0FBUyxDQUFDO0FBQ25CLENBQUM7QUFFRCxTQUFTLGtCQUFrQixDQUFDLFlBQVksRUFBRSxpQkFBaUIsRUFBRSxTQUFTO0lBQ3BFLElBQUksU0FBUyxLQUFLLE1BQU0sRUFBRTtRQUN4QixPQUFPLFlBQVksQ0FBQyxJQUFJLEdBQUcsV0FBVyxDQUFDO0tBQ3hDO0lBRUQsSUFBSSxTQUFTLEtBQUssT0FBTyxFQUFFO1FBQ3pCLE9BQU8sWUFBWSxDQUFDLElBQUksR0FBRyxZQUFZLENBQUMsS0FBSyxHQUFHLGlCQUFpQixDQUFDLEtBQUssR0FBRyxXQUFXLENBQUM7S0FDdkY7SUFFRCxJQUFJLFNBQVMsS0FBSyxR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SSxTQUFTLEtBQUssY0FBYyxDQUFDLE1BQU0sRUFBRTtZQUM5QyxHQUFHLEdBQUcsT0FBTyxDQUFDLEdBQUcsR0FBRyxPQUFPLENBQUMsTUFBTSxHQUFHLE9BQU8sQ0FBQztZQUM3QyxJQUFJLEdBQUcsY0FBYyxDQUFDLDRCQUE0QixDQUFDLE9BQU8sRUFBRSxNQUFNLEVBQUUsU0FBUyxDQUFDLENBQUM7U0FDaEY7UUFFRCxPQUFPLEVBQUUsR0FBRyxFQUFFLElBQUksRUFBRSxDQUFDO0lBQ3ZCLENBQUM7SUFFRDs7OztPQUlHO0lBQ0gsTUFBTSxDQUFDLGtCQUFrQixDQUFDLFNBQVMsRUFBRSxNQUFNLEVBQUUsT0FBTyxFQUFFLE9BQU87UUFDM0QsTUFBTSxVQUFVLEdBQUcsY0FBYyxDQUFDLFVBQVUsQ0FBQyxPQUFPLEVBQUUsTUFBTSxFQUFFLFNBQVMsRUFBRSxPQUFPLENBQUMsQ0FBQztRQUVsRixJQUFJLFVBQVUsRUFBRTtZQUNkLElBQUksU0FBUyxLQUFLLGNBQWMsQ0FBQyxLQUFLLEVBQUU7Z0JBQ3RDLE9BQU8sY0FBYyxDQUFDLElBQUksQ0FBQzthQUM1QjtpQkFBTSxJQUFJLFNBQVMsS0FBSyxjQUFjLENBQUMsSUFBSSxFQUFFO2dCQUM1QyxPQUFPLGNBQWMsQ0FBQyxLQUFLLENBQUM7YUFDN0I7aUJBQU0sSUFBSSxTQUFTLEtBQUssY0FBYyxDQUFDLEdBQUcsRUFBRTtnQkFDM0MsT0FBTyxjQUFjLENBQUMsTUFBTSxDQUFDO2FBQzlCO2lCQUFNLElBQUksU0FBUyxLQUFLLGNBQWMsQ0FBQyxNQUFNLEVBQUU7Z0JBQzlDLE9BQU8sY0FBYyxDQUFDLEdBQUcsQ0FBQzthQUMzQjtTQUNGO1FBRUQsT0FBTyxTQUFTLENBQUM7SUFDbkIsQ0FBQztDQUNGIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgUGxhY2VtZW50VHlwZXMgfSBmcm9tICcuL3BsYWNlbWVudC50eXBlJztcblxuY29uc3QgY2FyZXRPZmZzZXQgPSA3O1xuXG5mdW5jdGlvbiB2ZXJ0aWNhbFBvc2l0aW9uKGVsRGltZW5zaW9ucywgcG9wb3ZlckRpbWVuc2lvbnMsIGFsaWdubWVudCkge1xuICBpZiAoYWxpZ25tZW50ID09PSAndG9wJykge1xuICAgIHJldHVybiBlbERpbWVuc2lvbnMudG9wIC0gY2FyZXRPZmZzZXQ7XG4gIH1cblxuICBpZiAoYWxpZ25tZW50ID09PSAnYm90dG9tJykge1xuICAgIHJldHVybiBlbERpbWVuc2lvbnMudG9wICsgZWxEaW1lbnNpb25zLmhlaWdodCAtIHBvcG92ZXJEaW1lbnNpb25zLmhlaWdodCArIGNhcmV0T2Zmc2V0O1xuICB9XG5cbiAgaWYgKGFsaWdubWVudCA9PT0gJ2NlbnRlcicpIHtcbiAgICByZXR1cm4gZWxEaW1lbnNpb25zLnRvcCArIGVsRGltZW5zaW9ucy5oZWlnaHQgLyAyIC0gcG9wb3ZlckRpbWVuc2lvbnMuaGVpZ2h0IC8gMjtcbiAgfVxuXG4gIHJldHVybiB1bmRlZmluZWQ7XG59XG5cbmZ1bmN0aW9uIGhvcml6b250YWxQb3NpdGlvbihlbERpbWVuc2lvbnMsIHBvcG92ZXJEaW1lbnNpb25zLCBhbGlnbm1lbnQpIHtcbiAgaWYgKGFsaWdubWVudCA9PT0gJ2xlZnQnKSB7XG4gICAgcmV0dXJuIGVsRGltZW5zaW9ucy5sZWZ0IC0gY2FyZXRPZmZzZXQ7XG4gIH1cblxuICBpZiAoYWxpZ25tZW50ID09PSAncmlnaHQnKSB7XG4gICAgcmV0dXJuIGVsRGltZW5zaW9ucy5sZWZ0ICsgZWxEaW1lbnNpb25zLndpZHRoIC0gcG9wb3ZlckRpbWVuc2lvbnMud2lkdGggKyBjYXJldE9mZnNldDtcbiAgfVxuXG4gIGlmIChhbGlnbm1lbnQgPT09ICdjZW50ZXInKSB7XG4gICAgcmV0dXJuIGVsRGltZW5zaW9ucy5sZWZ0ICsgZWxEaW1lbnNpb25zLndpZHRoIC8gMiAtIHBvcG92ZXJEaW1lbnNpb25zLndpZHRoIC8gMjtcbiAgfVxuXG4gIHJldHVybiB1bmRlZmluZWQ7XG59XG5cbi8qKlxuICogUG9zaXRpb24gaGVscGVyIGZvciB0aGUgcG9wb3ZlciBkaXJlY3RpdmUuXG4gKlxuICogQGV4cG9ydFxuICovXG5leHBvcnQgY2xhc3MgUG9zaXRpb25IZWxwZXIge1xuICAvKipcbiAgICogQ2FsY3VsYXRlIHZlcnRpY2FsIGFsaWdubWVudCBwb3NpdGlvblxuICAgKlxuICAgKiBAbWVtYmVyT2YgUG9zaXRpb25IZWxwZXJcbiAgICovXG4gIHN0YXRpYyBjYWxjdWxhdGVWZXJ0aWNhbEFsaWdubWVudChlbERpbWVuc2lvbnMsIHBvcG92ZXJEaW1lbnNpb25zLCBhbGlnbm1lbnQpOiBudW1iZXIge1xuICAgIGxldCByZXN1bHQgPSB2ZXJ0aWNhbFBvc2l0aW9uKGVsRGltZW5zaW9ucywgcG9wb3ZlckRpbWVuc2lvbnMsIGFsaWdubWVudCk7XG5cbiAgICBpZiAocmVzdWx0ICsgcG9wb3ZlckRpbWVuc2lvbnMuaGVpZ2h0ID4gd2luZG93LmlubmVySGVpZ2h0KSB7XG4gICAgICByZXN1bHQgPSB3aW5kb3cuaW5uZXJIZWlnaHQgLSBwb3BvdmVyRGltZW5zaW9ucy5oZWlnaHQ7XG4gICAgfVxuXG4gICAgcmV0dXJuIHJlc3VsdDtcbiAgfVxuXG4gIC8qKlxuICAgKiBDYWxjdWxhdGUgdmVydGljYWwgY2FyZXQgcG9zaXRpb25cbiAgICpcbiAgICogQG1lbWJlck9mIFBvc2l0aW9uSGVscGVyXG4gICAqL1xuICBzdGF0aWMgY2FsY3VsYXRlVmVydGljYWxDYXJldChlbERpbWVuc2lvbnMsIHBvcG92ZXJEaW1lbnNpb25zLCBjYXJldERpbWVuc2lvbnMsIGFsaWdubWVudCk6IG51bWJlciB7XG4gICAgbGV0IHJlc3VsdDtcblxuICAgIGlmIChhbGlnbm1lbnQgPT09ICd0b3AnKSB7XG4gICAgICByZXN1bHQgPSBlbERpbWVuc2lvbnMuaGVpZ2h0IC8gMiAtIGNhcmV0RGltZW5zaW9ucy5oZWlnaHQgLyAyICsgY2FyZXRPZmZzZXQ7XG4gICAgfVxuXG4gICAgaWYgKGFsaWdubWVudCA9PT0gJ2JvdHRvbScpIHtcbiAgICAgIHJlc3VsdCA9IHBvcG92ZXJEaW1lbnNpb25zLmhlaWdodCAtIGVsRGltZW5zaW9ucy5oZWlnaHQgLyAyIC0gY2FyZXREaW1lbnNpb25zLmhlaWdodCAvIDIgLSBjYXJldE9mZnNldDtcbiAgICB9XG5cbiAgICBpZiAoYWxpZ25tZW50ID09PSAnY2VudGVyJykge1xuICAgICAgcmVzdWx0ID0gcG9wb3ZlckRpbWVuc2lvbnMuaGVpZ2h0IC8gMiAtIGNhcmV0RGltZW5zaW9ucy5oZWlnaHQgLyAyO1xuICAgIH1cblxuICAgIGNvbnN0IHBvcG92ZXJQb3NpdGlvbiA9IHZlcnRpY2FsUG9zaXRpb24oZWxEaW1lbnNpb25zLCBwb3BvdmVyRGltZW5zaW9ucywgYWxpZ25tZW50KTtcbiAgICBpZiAocG9wb3ZlclBvc2l0aW9uICsgcG9wb3ZlckRpbWVuc2lvbnMuaGVpZ2h0ID4gd2luZG93LmlubmVySGVpZ2h0KSB7XG4gICAgICByZXN1bHQgKz0gcG9wb3ZlclBvc2l0aW9uICsgcG9wb3ZlckRpbWVuc2lvbnMuaGVpZ2h0IC0gd2luZG93LmlubmVySGVpZ2h0O1xuICAgIH1cblxuICAgIHJldHVybiByZXN1bHQ7XG4gIH1cblxuICAvKipcbiAgICogQ2FsY3VsYXRlIGhvcnogYWxpZ25tZW50IHBvc2l0aW9uXG4gICAqXG4gICAqIEBtZW1iZXJPZiBQb3NpdGlvbkhlbHBlclxuICAgKi9cbiAgc3RhdGljIGNhbGN1bGF0ZUhvcml6b250YWxBbGlnbm1lbnQoZWxEaW1lbnNpb25zLCBwb3BvdmVyRGltZW5zaW9ucywgYWxpZ25tZW50KTogbnVtYmVyIHtcbiAgICBsZXQgcmVzdWx0ID0gaG9yaXpvbnRhbFBvc2l0aW9uKGVsRGltZW5zaW9ucywgcG9wb3ZlckRpbWVuc2lvbnMsIGFsaWdubWVudCk7XG5cbiAgICBpZiAocmVzdWx0ICsgcG9wb3ZlckRpbWVuc2lvbnMud2lkdGggPiB3aW5kb3cuaW5uZXJXaWR0aCkge1xuICAgICAgcmVzdWx0ID0gd2luZG93LmlubmVyV2lkdGggLSBwb3BvdmVyRGltZW5zaW9ucy53aWR0aDtcbiAgICB9XG5cbiAgICByZXR1cm4gcmVzdWx0O1xuICB9XG5cbiAgLyoqXG4gICAqIENhbGN1bGF0ZSBob3J6IGNhcmV0IHBvc2l0aW9uXG4gICAqXG4gICAqIEBtZW1iZXJPZiBQb3NpdGlvbkhlbHBlclxuICAgKi9cbiAgc3RhdGljIGNhbGN1bGF0ZUhvcml6b250YWxDYXJldChlbERpbWVuc2lvbnMsIHBvcG92ZXJEaW1lbnNpb25zLCBjYXJldERpbWVuc2lvbnMsIGFsaWdubWVudCk6IG51bWJlciB7XG4gICAgbGV0IHJlc3VsdDtcblxuICAgIGlmIChhbGlnbm1lbnQgPT09ICdsZWZ0Jykge1xuICAgICAgcmVzdWx0ID0gZWxEaW1lbnNpb25zLndpZHRoIC8gMiAtIGNhcmV0RGltZW5zaW9ucy53aWR0aCAvIDIgKyBjYXJldE9mZnNldDtcbiAgICB9XG5cbiAgICBpZiAoYWxpZ25tZW50ID09PSAncmlnaHQnKSB7XG4gICAgICByZXN1bHQgPSBwb3BvdmVyRGltZW5zaW9ucy53aWR0aCAtIGVsRGltZW5zaW9ucy53aWR0aCAvIDIgLSBjYXJldERpbWVuc2lvbnMud2lkdGggLyAyIC0gY2FyZXRPZmZzZXQ7XG4gICAgfVxuXG4gICAgaWYgKGFsaWdubWVudCA9PT0gJ2NlbnRlcicpIHtcbiAgICAgIHJlc3VsdCA9IHBvcG92ZXJEaW1lbnNpb25zLndpZHRoIC8gMiAtIGNhcmV0RGltZW5zaW9ucy53aWR0aCAvIDI7XG4gICAgfVxuXG4gICAgY29uc3QgcG9wb3ZlclBvc2l0aW9uID0gaG9yaXpvbnRhbFBvc2l0aW9uKGVsRGltZW5zaW9ucywgcG9wb3ZlckRpbWVuc2lvbnMsIGFsaWdubWVudCk7XG4gICAgaWYgKHBvcG92ZXJQb3NpdGlvbiArIHBvcG92ZXJEaW1lbnNpb25zLndpZHRoID4gd2luZG93LmlubmVyV2lkdGgpIHtcbiAgICAgIHJlc3VsdCArPSBwb3BvdmVyUG9zaXRpb24gKyBwb3BvdmVyRGltZW5zaW9ucy53aWR0aCAtIHdpbmRvdy5pbm5lcldpZHRoO1xuICAgIH1cblxuICAgIHJldHVybiByZXN1bHQ7XG4gIH1cblxuICAvKipcbiAgICogQ2hlY2tzIGlmIHRoZSBlbGVtZW50J3MgcG9zaXRpb24gc2hvdWxkIGJlIGZsaXBwZWRcbiAgICpcbiAgICogQG1lbWJlck9mIFBvc2l0aW9uSGVscGVyXG4gICAqL1xuICBzdGF0aWMgc2hvdWxkRmxpcChlbERpbWVuc2lvbnMsIHBvcG92ZXJEaW1lbnNpb25zLCBwbGFjZW1lbnQsIHNwYWNpbmcpOiBib29sZWFuIHtcbiAgICBsZXQgZmxpcCA9IGZhbHNlO1xuXG4gICAgaWYgKHBsYWNlbWVudCA9PT0gJ3JpZ2h0Jykge1xuICAgICAgaWYgKGVsRGltZW5zaW9ucy5sZWZ0ICsgZWxEaW1lbnNpb25zLndpZHRoICsgcG9wb3ZlckRpbWVuc2lvbnMud2lkdGggKyBzcGFjaW5nID4gd2luZG93LmlubmVyV2lkdGgpIHtcbiAgICAgICAgZmxpcCA9IHRydWU7XG4gICAgICB9XG4gICAgfVxuXG4gICAgaWYgKHBsYWNlbWVudCA9PT0gJ2xlZnQnKSB7XG4gICAgICBpZiAoZWxEaW1lbnNpb25zLmxlZnQgLSBwb3BvdmVyRGltZW5zaW9ucy53aWR0aCAtIHNwYWNpbmcgPCAwKSB7XG4gICAgICAgIGZsaXAgPSB0cnVlO1xuICAgICAgfVxuICAgIH1cblxuICAgIGlmIChwbGFjZW1lbnQgPT09ICd0b3AnKSB7XG4gICAgICBpZiAoZWxEaW1lbnNpb25zLnRvcCAtIHBvcG92ZXJEaW1lbnNpb25zLmhlaWdodCAtIHNwYWNpbmcgPCAwKSB7XG4gICAgICAgIGZsaXAgPSB0cnVlO1xuICAgICAgfVxuICAgIH1cblxuICAgIGlmIChwbGFjZW1lbnQgPT09ICdib3R0b20nKSB7XG4gICAgICBpZiAoZWxEaW1lbnNpb25zLnRvcCArIGVsRGltZW5zaW9ucy5oZWlnaHQgKyBwb3BvdmVyRGltZW5zaW9ucy5oZWlnaHQgKyBzcGFjaW5nID4gd2luZG93LmlubmVySGVpZ2h0KSB7XG4gICAgICAgIGZsaXAgPSB0cnVlO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiBmbGlwO1xuICB9XG5cbiAgLyoqXG4gICAqIFBvc2l0aW9uIGNhcmV0XG4gICAqXG4gICAqIEBtZW1iZXJPZiBQb3NpdGlvbkhlbHBlclxuICAgKi9cbiAgc3RhdGljIHBvc2l0aW9uQ2FyZXQocGxhY2VtZW50LCBlbG1EaW0sIGhvc3REaW0sIGNhcmV0RGltZW5zaW9ucywgYWxpZ25tZW50KTogYW55IHtcbiAgICBsZXQgdG9wID0gMDtcbiAgICBsZXQgbGVmdCA9IDA7XG5cbiAgICBpZiAocGxhY2VtZW50ID09PSBQbGFjZW1lbnRUeXBlcy5yaWdodCkge1xuICAgICAgbGVmdCA9IC03O1xuICAgICAgdG9wID0gUG9zaXRpb25IZWxwZXIuY2FsY3VsYXRlVmVydGljYWxDYXJldChob3N0RGltLCBlbG1EaW0sIGNhcmV0RGltZW5zaW9ucywgYWxpZ25tZW50KTtcbiAgICB9IGVsc2UgaWYgKHBsYWNlbWVudCA9PT0gUGxhY2VtZW50VHlwZXMubGVmdCkge1xuICAgICAgbGVmdCA9IGVsbURpbS53aWR0aDtcbiAgICAgIHRvcCA9IFBvc2l0aW9uSGVscGVyLmNhbGN1bGF0ZVZlcnRpY2FsQ2FyZXQoaG9zdERpbSwgZWxtRGltLCBjYXJldERpbWVuc2lvbnMsIGFsaWdubWVudCk7XG4gICAgfSBlbHNlIGlmIChwbGFjZW1lbnQgPT09IFBsYWNlbWVudFR5cGVzLnRvcCkge1xuICAgICAgdG9wID0gZWxtRGltLmhlaWdodDtcbiAgICAgIGxlZnQgPSBQb3NpdGlvbkhlbHBlci5jYWxjdWxhdGVIb3Jpem9udGFsQ2FyZXQoaG9zdERpbSwgZWxtRGltLCBjYXJldERpbWVuc2lvbnMsIGFsaWdubWVudCk7XG4gICAgfSBlbHNlIGlmIChwbGFjZW1lbnQgPT09IFBsYWNlbWVudFR5cGVzLmJvdHRvbSkge1xuICAgICAgdG9wID0gLTc7XG4gICAgICBsZWZ0ID0gUG9zaXRpb25IZWxwZXIuY2FsY3VsYXRlSG9yaXpvbnRhbENhcmV0KGhvc3REaW0sIGVsbURpbSwgY2FyZXREaW1lbnNpb25zLCBhbGlnbm1lbnQpO1xuICAgIH1cblxuICAgIHJldHVybiB7IHRvcCwgbGVmdCB9O1xuICB9XG5cbiAgLyoqXG4gICAqIFBvc2l0aW9uIGNvbnRlbnRcbiAgICpcbiAgICogQG1lbWJlck9mIFBvc2l0aW9uSGVscGVyXG4gICAqL1xuICBzdGF0aWMgcG9zaXRpb25Db250ZW50KHBsYWNlbWVudCwgZWxtRGltLCBob3N0RGltLCBzcGFjaW5nLCBhbGlnbm1lbnQpOiBhbnkge1xuICAgIGxldCB0b3AgPSAwO1xuICAgIGxldCBsZWZ0ID0gMDtcblxuICAgIGlmIChwbGFjZW1lbnQgPT09IFBsYWNlbWVudFR5cGVzLnJpZ2h0KSB7XG4gICAgICBsZWZ0ID0gaG9zdERpbS5sZWZ0ICsgaG9zdERpbS53aWR0aCArIHNwYWNpbmc7XG4gICAgICB0b3AgPSBQb3NpdGlvbkhlbHBlci5jYWxjdWxhdGVWZXJ0aWNhbEFsaWdubWVudChob3N0RGltLCBlbG1EaW0sIGFsaWdubWVudCk7XG4gICAgfSBlbHNlIGlmIChwbGFjZW1lbnQgPT09IFBsYWNlbWVudFR5cGVzLmxlZnQpIHtcbiAgICAgIGxlZnQgPSBob3N0RGltLmxlZnQgLSBlbG1EaW0ud2lkdGggLSBzcGFjaW5nO1xuICAgICAgdG9wID0gUG9zaXRpb25IZWxwZXIuY2FsY3VsYXRlVmVydGljYWxBbGlnbm1lbnQoaG9zdERpbSwgZWxtRGltLCBhbGlnbm1lbnQpO1xuICAgIH0gZWxzZSBpZiAocGxhY2VtZW50ID09PSBQbGFjZW1lbnRUeXBlcy50b3ApIHtcbiAgICAgIHRvcCA9IGhvc3REaW0udG9wIC0gZWxtRGltLmhlaWdodCAtIHNwYWNpbmc7XG4gICAgICBsZWZ0ID0gUG9zaXRpb25IZWxwZXIuY2FsY3VsYXRlSG9yaXpvbnRhbEFsaWdubWVudChob3N0RGltLCBlbG1EaW0sIGFsaWdubWVudCk7XG4gICAgfSBlbHNlIGlmIChwbGFjZW1lbnQgPT09IFBsYWNlbWVudFR5cGVzLmJvdHRvbSkge1xuICAgICAgdG9wID0gaG9zdERpbS50b3AgKyBob3N0RGltLmhlaWdodCArIHNwYWNpbmc7XG4gICAgICBsZWZ0ID0gUG9zaXRpb25IZWxwZXIuY2FsY3VsYXRlSG9yaXpvbnRhbEFsaWdubWVudChob3N0RGltLCBlbG1EaW0sIGFsaWdubWVudCk7XG4gICAgfVxuXG4gICAgcmV0dXJuIHsgdG9wLCBsZWZ0IH07XG4gIH1cblxuICAvKipcbiAgICogRGV0ZXJtaW5lIHBsYWNlbWVudCBiYXNlZCBvbiBmbGlwXG4gICAqXG4gICAqIEBtZW1iZXJPZiBQb3NpdGlvbkhlbHBlclxuICAgKi9cbiAgc3RhdGljIGRldGVybWluZVBsYWNlbWVudChwbGFjZW1lbnQsIGVsbURpbSwgaG9zdERpbSwgc3BhY2luZyk6IGFueSB7XG4gICAgY29uc3Qgc2hvdWxkRmxpcCA9IFBvc2l0aW9uSGVscGVyLnNob3VsZEZsaXAoaG9zdERpbSwgZWxtRGltLCBwbGFjZW1lbnQsIHNwYWNpbmcpO1xuXG4gICAgaWYgKHNob3VsZEZsaXApIHtcbiAgICAgIGlmIChwbGFjZW1lbnQgPT09IFBsYWNlbWVudFR5cGVzLnJpZ2h0KSB7XG4gICAgICAgIHJldHVybiBQbGFjZW1lbnRUeXBlcy5sZWZ0O1xuICAgICAgfSBlbHNlIGlmIChwbGFjZW1lbnQgPT09IFBsYWNlbWVudFR5cGVzLmxlZnQpIHtcbiAgICAgICAgcmV0dXJuIFBsYWNlbWVudFR5cGVzLnJpZ2h0O1xuICAgICAgfSBlbHNlIGlmIChwbGFjZW1lbnQgPT09IFBsYWNlbWVudFR5cGVzLnRvcCkge1xuICAgICAgICByZXR1cm4gUGxhY2VtZW50VHlwZXMuYm90dG9tO1xuICAgICAgfSBlbHNlIGlmIChwbGFjZW1lbnQgPT09IFBsYWNlbWVudFR5cGVzLmJvdHRvbSkge1xuICAgICAgICByZXR1cm4gUGxhY2VtZW50VHlwZXMudG9wO1xuICAgICAgfVxuICAgIH1cblxuICAgIHJldHVybiBwbGFjZW1lbnQ7XG4gIH1cbn1cbiJdfQ==