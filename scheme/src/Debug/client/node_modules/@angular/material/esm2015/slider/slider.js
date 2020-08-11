/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { FocusMonitor } from '@angular/cdk/a11y';
import { Directionality } from '@angular/cdk/bidi';
import { coerceBooleanProperty, coerceNumberProperty } from '@angular/cdk/coercion';
import { DOWN_ARROW, END, HOME, LEFT_ARROW, PAGE_DOWN, PAGE_UP, RIGHT_ARROW, UP_ARROW, hasModifierKey, } from '@angular/cdk/keycodes';
import { Attribute, ChangeDetectionStrategy, ChangeDetectorRef, Component, ElementRef, EventEmitter, forwardRef, Inject, Input, Optional, Output, ViewChild, ViewEncapsulation, NgZone, } from '@angular/core';
import { NG_VALUE_ACCESSOR } from '@angular/forms';
import { mixinColor, mixinDisabled, mixinTabIndex, } from '@angular/material/core';
import { ANIMATION_MODULE_TYPE } from '@angular/platform-browser/animations';
import { normalizePassiveListenerOptions } from '@angular/cdk/platform';
import { DOCUMENT } from '@angular/common';
import { Subscription } from 'rxjs';
const activeEventOptions = normalizePassiveListenerOptions({ passive: false });
/**
 * Visually, a 30px separation between tick marks looks best. This is very subjective but it is
 * the default separation we chose.
 */
const MIN_AUTO_TICK_SEPARATION = 30;
/** The thumb gap size for a disabled slider. */
const DISABLED_THUMB_GAP = 7;
/** The thumb gap size for a non-active slider at its minimum value. */
const MIN_VALUE_NONACTIVE_THUMB_GAP = 7;
/** The thumb gap size for an active slider at its minimum value. */
const MIN_VALUE_ACTIVE_THUMB_GAP = 10;
/**
 * Provider Expression that allows mat-slider to register as a ControlValueAccessor.
 * This allows it to support [(ngModel)] and [formControl].
 * @docs-private
 */
export const MAT_SLIDER_VALUE_ACCESSOR = {
    provide: NG_VALUE_ACCESSOR,
    useExisting: forwardRef(() => MatSlider),
    multi: true
};
/** A simple change event emitted by the MatSlider component. */
export class MatSliderChange {
}
// Boilerplate for applying mixins to MatSlider.
/** @docs-private */
class MatSliderBase {
    constructor(_elementRef) {
        this._elementRef = _elementRef;
    }
}
const _MatSliderMixinBase = mixinTabIndex(mixinColor(mixinDisabled(MatSliderBase), 'accent'));
/**
 * Allows users to select from a range of values by moving the slider thumb. It is similar in
 * behavior to the native `<input type="range">` element.
 */
let MatSlider = /** @class */ (() => {
    class MatSlider extends _MatSliderMixinBase {
        constructor(elementRef, _focusMonitor, _changeDetectorRef, _dir, tabIndex, _ngZone, _document, _animationMode) {
            super(elementRef);
            this._focusMonitor = _focusMonitor;
            this._changeDetectorRef = _changeDetectorRef;
            this._dir = _dir;
            this._ngZone = _ngZone;
            this._animationMode = _animationMode;
            this._invert = false;
            this._max = 100;
            this._min = 0;
            this._step = 1;
            this._thumbLabel = false;
            this._tickInterval = 0;
            this._value = null;
            this._vertical = false;
            /** Event emitted when the slider value has changed. */
            this.change = new EventEmitter();
            /** Event emitted when the slider thumb moves. */
            this.input = new EventEmitter();
            /**
             * Emits when the raw value of the slider changes. This is here primarily
             * to facilitate the two-way binding for the `value` input.
             * @docs-private
             */
            this.valueChange = new EventEmitter();
            /** onTouch function registered via registerOnTouch (ControlValueAccessor). */
            this.onTouched = () => { };
            this._percent = 0;
            /**
             * Whether or not the thumb is sliding.
             * Used to determine if there should be a transition for the thumb and fill track.
             */
            this._isSliding = false;
            /**
             * Whether or not the slider is active (clicked or sliding).
             * Used to shrink and grow the thumb as according to the Material Design spec.
             */
            this._isActive = false;
            /** The size of a tick interval as a percentage of the size of the track. */
            this._tickIntervalPercent = 0;
            /** The dimensions of the slider. */
            this._sliderDimensions = null;
            this._controlValueAccessorChangeFn = () => { };
            /** Subscription to the Directionality change EventEmitter. */
            this._dirChangeSubscription = Subscription.EMPTY;
            /** Called when the user has put their pointer down on the slider. */
            this._pointerDown = (event) => {
                // Don't do anything if the slider is disabled or the
                // user is using anything other than the main mouse button.
                if (this.disabled || this._isSliding || (!isTouchEvent(event) && event.button !== 0)) {
                    return;
                }
                this._ngZone.run(() => {
                    const oldValue = this.value;
                    const pointerPosition = getPointerPositionOnPage(event);
                    this._isSliding = true;
                    this._lastPointerEvent = event;
                    event.preventDefault();
                    this._focusHostElement();
                    this._onMouseenter(); // Simulate mouseenter in case this is a mobile device.
                    this._bindGlobalEvents(event);
                    this._focusHostElement();
                    this._updateValueFromPosition(pointerPosition);
                    this._valueOnSlideStart = this.value;
                    this._pointerPositionOnStart = pointerPosition;
                    // Emit a change and input event if the value changed.
                    if (oldValue != this.value) {
                        this._emitInputEvent();
                        this._emitChangeEvent();
                    }
                });
            };
            /**
             * Called when the user has moved their pointer after
             * starting to drag. Bound on the document level.
             */
            this._pointerMove = (event) => {
                if (this._isSliding) {
                    // Prevent the slide from selecting anything else.
                    event.preventDefault();
                    const oldValue = this.value;
                    this._lastPointerEvent = event;
                    this._updateValueFromPosition(getPointerPositionOnPage(event));
                    // Native range elements always emit `input` events when the value changed while sliding.
                    if (oldValue != this.value) {
                        this._emitInputEvent();
                    }
                }
            };
            /** Called when the user has lifted their pointer. Bound on the document level. */
            this._pointerUp = (event) => {
                if (this._isSliding) {
                    const pointerPositionOnStart = this._pointerPositionOnStart;
                    const currentPointerPosition = getPointerPositionOnPage(event);
                    event.preventDefault();
                    this._removeGlobalEvents();
                    this._valueOnSlideStart = this._pointerPositionOnStart = this._lastPointerEvent = null;
                    this._isSliding = false;
                    if (this._valueOnSlideStart != this.value && !this.disabled &&
                        pointerPositionOnStart && (pointerPositionOnStart.x !== currentPointerPosition.x ||
                        pointerPositionOnStart.y !== currentPointerPosition.y)) {
                        this._emitChangeEvent();
                    }
                }
            };
            /** Called when the window has lost focus. */
            this._windowBlur = () => {
                // If the window is blurred while dragging we need to stop dragging because the
                // browser won't dispatch the `mouseup` and `touchend` events anymore.
                if (this._lastPointerEvent) {
                    this._pointerUp(this._lastPointerEvent);
                }
            };
            this._document = _document;
            this.tabIndex = parseInt(tabIndex) || 0;
            _ngZone.runOutsideAngular(() => {
                const element = elementRef.nativeElement;
                element.addEventListener('mousedown', this._pointerDown, activeEventOptions);
                element.addEventListener('touchstart', this._pointerDown, activeEventOptions);
            });
        }
        /** Whether the slider is inverted. */
        get invert() { return this._invert; }
        set invert(value) {
            this._invert = coerceBooleanProperty(value);
        }
        /** The maximum value that the slider can have. */
        get max() { return this._max; }
        set max(v) {
            this._max = coerceNumberProperty(v, this._max);
            this._percent = this._calculatePercentage(this._value);
            // Since this also modifies the percentage, we need to let the change detection know.
            this._changeDetectorRef.markForCheck();
        }
        /** The minimum value that the slider can have. */
        get min() { return this._min; }
        set min(v) {
            this._min = coerceNumberProperty(v, this._min);
            // If the value wasn't explicitly set by the user, set it to the min.
            if (this._value === null) {
                this.value = this._min;
            }
            this._percent = this._calculatePercentage(this._value);
            // Since this also modifies the percentage, we need to let the change detection know.
            this._changeDetectorRef.markForCheck();
        }
        /** The values at which the thumb will snap. */
        get step() { return this._step; }
        set step(v) {
            this._step = coerceNumberProperty(v, this._step);
            if (this._step % 1 !== 0) {
                this._roundToDecimal = this._step.toString().split('.').pop().length;
            }
            // Since this could modify the label, we need to notify the change detection.
            this._changeDetectorRef.markForCheck();
        }
        /** Whether or not to show the thumb label. */
        get thumbLabel() { return this._thumbLabel; }
        set thumbLabel(value) { this._thumbLabel = coerceBooleanProperty(value); }
        /**
         * How often to show ticks. Relative to the step so that a tick always appears on a step.
         * Ex: Tick interval of 4 with a step of 3 will draw a tick every 4 steps (every 12 values).
         */
        get tickInterval() { return this._tickInterval; }
        set tickInterval(value) {
            if (value === 'auto') {
                this._tickInterval = 'auto';
            }
            else if (typeof value === 'number' || typeof value === 'string') {
                this._tickInterval = coerceNumberProperty(value, this._tickInterval);
            }
            else {
                this._tickInterval = 0;
            }
        }
        /** Value of the slider. */
        get value() {
            // If the value needs to be read and it is still uninitialized, initialize it to the min.
            if (this._value === null) {
                this.value = this._min;
            }
            return this._value;
        }
        set value(v) {
            if (v !== this._value) {
                let value = coerceNumberProperty(v);
                // While incrementing by a decimal we can end up with values like 33.300000000000004.
                // Truncate it to ensure that it matches the label and to make it easier to work with.
                if (this._roundToDecimal) {
                    value = parseFloat(value.toFixed(this._roundToDecimal));
                }
                this._value = value;
                this._percent = this._calculatePercentage(this._value);
                // Since this also modifies the percentage, we need to let the change detection know.
                this._changeDetectorRef.markForCheck();
            }
        }
        /** Whether the slider is vertical. */
        get vertical() { return this._vertical; }
        set vertical(value) {
            this._vertical = coerceBooleanProperty(value);
        }
        /** The value to be used for display purposes. */
        get displayValue() {
            if (this.displayWith) {
                // Value is never null but since setters and getters cannot have
                // different types, the value getter is also typed to return null.
                return this.displayWith(this.value);
            }
            // Note that this could be improved further by rounding something like 0.999 to 1 or
            // 0.899 to 0.9, however it is very performance sensitive, because it gets called on
            // every change detection cycle.
            if (this._roundToDecimal && this.value && this.value % 1 !== 0) {
                return this.value.toFixed(this._roundToDecimal);
            }
            return this.value || 0;
        }
        /** set focus to the host element */
        focus(options) {
            this._focusHostElement(options);
        }
        /** blur the host element */
        blur() {
            this._blurHostElement();
        }
        /** The percentage of the slider that coincides with the value. */
        get percent() { return this._clamp(this._percent); }
        /**
         * Whether the axis of the slider is inverted.
         * (i.e. whether moving the thumb in the positive x or y direction decreases the slider's value).
         */
        get _invertAxis() {
            // Standard non-inverted mode for a vertical slider should be dragging the thumb from bottom to
            // top. However from a y-axis standpoint this is inverted.
            return this.vertical ? !this.invert : this.invert;
        }
        /** Whether the slider is at its minimum value. */
        get _isMinValue() {
            return this.percent === 0;
        }
        /**
         * The amount of space to leave between the slider thumb and the track fill & track background
         * elements.
         */
        get _thumbGap() {
            if (this.disabled) {
                return DISABLED_THUMB_GAP;
            }
            if (this._isMinValue && !this.thumbLabel) {
                return this._isActive ? MIN_VALUE_ACTIVE_THUMB_GAP : MIN_VALUE_NONACTIVE_THUMB_GAP;
            }
            return 0;
        }
        /** CSS styles for the track background element. */
        get _trackBackgroundStyles() {
            const axis = this.vertical ? 'Y' : 'X';
            const scale = this.vertical ? `1, ${1 - this.percent}, 1` : `${1 - this.percent}, 1, 1`;
            const sign = this._shouldInvertMouseCoords() ? '-' : '';
            return {
                // scale3d avoids some rendering issues in Chrome. See #12071.
                transform: `translate${axis}(${sign}${this._thumbGap}px) scale3d(${scale})`
            };
        }
        /** CSS styles for the track fill element. */
        get _trackFillStyles() {
            const percent = this.percent;
            const axis = this.vertical ? 'Y' : 'X';
            const scale = this.vertical ? `1, ${percent}, 1` : `${percent}, 1, 1`;
            const sign = this._shouldInvertMouseCoords() ? '' : '-';
            return {
                // scale3d avoids some rendering issues in Chrome. See #12071.
                transform: `translate${axis}(${sign}${this._thumbGap}px) scale3d(${scale})`,
                // iOS Safari has a bug where it won't re-render elements which start of as `scale(0)` until
                // something forces a style recalculation on it. Since we'll end up with `scale(0)` when
                // the value of the slider is 0, we can easily get into this situation. We force a
                // recalculation by changing the element's `display` when it goes from 0 to any other value.
                display: percent === 0 ? 'none' : ''
            };
        }
        /** CSS styles for the ticks container element. */
        get _ticksContainerStyles() {
            let axis = this.vertical ? 'Y' : 'X';
            // For a horizontal slider in RTL languages we push the ticks container off the left edge
            // instead of the right edge to avoid causing a horizontal scrollbar to appear.
            let sign = !this.vertical && this._getDirection() == 'rtl' ? '' : '-';
            let offset = this._tickIntervalPercent / 2 * 100;
            return {
                'transform': `translate${axis}(${sign}${offset}%)`
            };
        }
        /** CSS styles for the ticks element. */
        get _ticksStyles() {
            let tickSize = this._tickIntervalPercent * 100;
            let backgroundSize = this.vertical ? `2px ${tickSize}%` : `${tickSize}% 2px`;
            let axis = this.vertical ? 'Y' : 'X';
            // Depending on the direction we pushed the ticks container, push the ticks the opposite
            // direction to re-center them but clip off the end edge. In RTL languages we need to flip the
            // ticks 180 degrees so we're really cutting off the end edge abd not the start.
            let sign = !this.vertical && this._getDirection() == 'rtl' ? '-' : '';
            let rotate = !this.vertical && this._getDirection() == 'rtl' ? ' rotate(180deg)' : '';
            let styles = {
                'backgroundSize': backgroundSize,
                // Without translateZ ticks sometimes jitter as the slider moves on Chrome & Firefox.
                'transform': `translateZ(0) translate${axis}(${sign}${tickSize / 2}%)${rotate}`
            };
            if (this._isMinValue && this._thumbGap) {
                let side;
                if (this.vertical) {
                    side = this._invertAxis ? 'Bottom' : 'Top';
                }
                else {
                    side = this._invertAxis ? 'Right' : 'Left';
                }
                styles[`padding${side}`] = `${this._thumbGap}px`;
            }
            return styles;
        }
        get _thumbContainerStyles() {
            let axis = this.vertical ? 'Y' : 'X';
            // For a horizontal slider in RTL languages we push the thumb container off the left edge
            // instead of the right edge to avoid causing a horizontal scrollbar to appear.
            let invertOffset = (this._getDirection() == 'rtl' && !this.vertical) ? !this._invertAxis : this._invertAxis;
            let offset = (invertOffset ? this.percent : 1 - this.percent) * 100;
            return {
                'transform': `translate${axis}(-${offset}%)`
            };
        }
        /**
         * Whether mouse events should be converted to a slider position by calculating their distance
         * from the right or bottom edge of the slider as opposed to the top or left.
         */
        _shouldInvertMouseCoords() {
            return (this._getDirection() == 'rtl' && !this.vertical) ? !this._invertAxis : this._invertAxis;
        }
        /** The language direction for this slider element. */
        _getDirection() {
            return (this._dir && this._dir.value == 'rtl') ? 'rtl' : 'ltr';
        }
        ngAfterViewInit() {
            this._focusMonitor
                .monitor(this._elementRef, true)
                .subscribe((origin) => {
                this._isActive = !!origin && origin !== 'keyboard';
                this._changeDetectorRef.detectChanges();
            });
            if (this._dir) {
                this._dirChangeSubscription = this._dir.change.subscribe(() => {
                    this._changeDetectorRef.markForCheck();
                });
            }
        }
        ngOnDestroy() {
            const element = this._elementRef.nativeElement;
            element.removeEventListener('mousedown', this._pointerDown, activeEventOptions);
            element.removeEventListener('touchstart', this._pointerDown, activeEventOptions);
            this._lastPointerEvent = null;
            this._removeGlobalEvents();
            this._focusMonitor.stopMonitoring(this._elementRef);
            this._dirChangeSubscription.unsubscribe();
        }
        _onMouseenter() {
            if (this.disabled) {
                return;
            }
            // We save the dimensions of the slider here so we can use them to update the spacing of the
            // ticks and determine where on the slider click and slide events happen.
            this._sliderDimensions = this._getSliderDimensions();
            this._updateTickIntervalPercent();
        }
        _onFocus() {
            // We save the dimensions of the slider here so we can use them to update the spacing of the
            // ticks and determine where on the slider click and slide events happen.
            this._sliderDimensions = this._getSliderDimensions();
            this._updateTickIntervalPercent();
        }
        _onBlur() {
            this.onTouched();
        }
        _onKeydown(event) {
            if (this.disabled || hasModifierKey(event)) {
                return;
            }
            const oldValue = this.value;
            switch (event.keyCode) {
                case PAGE_UP:
                    this._increment(10);
                    break;
                case PAGE_DOWN:
                    this._increment(-10);
                    break;
                case END:
                    this.value = this.max;
                    break;
                case HOME:
                    this.value = this.min;
                    break;
                case LEFT_ARROW:
                    // NOTE: For a sighted user it would make more sense that when they press an arrow key on an
                    // inverted slider the thumb moves in that direction. However for a blind user, nothing
                    // about the slider indicates that it is inverted. They will expect left to be decrement,
                    // regardless of how it appears on the screen. For speakers ofRTL languages, they probably
                    // expect left to mean increment. Therefore we flip the meaning of the side arrow keys for
                    // RTL. For inverted sliders we prefer a good a11y experience to having it "look right" for
                    // sighted users, therefore we do not swap the meaning.
                    this._increment(this._getDirection() == 'rtl' ? 1 : -1);
                    break;
                case UP_ARROW:
                    this._increment(1);
                    break;
                case RIGHT_ARROW:
                    // See comment on LEFT_ARROW about the conditions under which we flip the meaning.
                    this._increment(this._getDirection() == 'rtl' ? -1 : 1);
                    break;
                case DOWN_ARROW:
                    this._increment(-1);
                    break;
                default:
                    // Return if the key is not one that we explicitly handle to avoid calling preventDefault on
                    // it.
                    return;
            }
            if (oldValue != this.value) {
                this._emitInputEvent();
                this._emitChangeEvent();
            }
            this._isSliding = true;
            event.preventDefault();
        }
        _onKeyup() {
            this._isSliding = false;
        }
        /** Use defaultView of injected document if available or fallback to global window reference */
        _getWindow() {
            return this._document.defaultView || window;
        }
        /**
         * Binds our global move and end events. They're bound at the document level and only while
         * dragging so that the user doesn't have to keep their pointer exactly over the slider
         * as they're swiping across the screen.
         */
        _bindGlobalEvents(triggerEvent) {
            // Note that we bind the events to the `document`, because it allows us to capture
            // drag cancel events where the user's pointer is outside the browser window.
            const document = this._document;
            const isTouch = isTouchEvent(triggerEvent);
            const moveEventName = isTouch ? 'touchmove' : 'mousemove';
            const endEventName = isTouch ? 'touchend' : 'mouseup';
            document.addEventListener(moveEventName, this._pointerMove, activeEventOptions);
            document.addEventListener(endEventName, this._pointerUp, activeEventOptions);
            if (isTouch) {
                document.addEventListener('touchcancel', this._pointerUp, activeEventOptions);
            }
            const window = this._getWindow();
            if (typeof window !== 'undefined' && window) {
                window.addEventListener('blur', this._windowBlur);
            }
        }
        /** Removes any global event listeners that we may have added. */
        _removeGlobalEvents() {
            const document = this._document;
            document.removeEventListener('mousemove', this._pointerMove, activeEventOptions);
            document.removeEventListener('mouseup', this._pointerUp, activeEventOptions);
            document.removeEventListener('touchmove', this._pointerMove, activeEventOptions);
            document.removeEventListener('touchend', this._pointerUp, activeEventOptions);
            document.removeEventListener('touchcancel', this._pointerUp, activeEventOptions);
            const window = this._getWindow();
            if (typeof window !== 'undefined' && window) {
                window.removeEventListener('blur', this._windowBlur);
            }
        }
        /** Increments the slider by the given number of steps (negative number decrements). */
        _increment(numSteps) {
            this.value = this._clamp((this.value || 0) + this.step * numSteps, this.min, this.max);
        }
        /** Calculate the new value from the new physical location. The value will always be snapped. */
        _updateValueFromPosition(pos) {
            if (!this._sliderDimensions) {
                return;
            }
            let offset = this.vertical ? this._sliderDimensions.top : this._sliderDimensions.left;
            let size = this.vertical ? this._sliderDimensions.height : this._sliderDimensions.width;
            let posComponent = this.vertical ? pos.y : pos.x;
            // The exact value is calculated from the event and used to find the closest snap value.
            let percent = this._clamp((posComponent - offset) / size);
            if (this._shouldInvertMouseCoords()) {
                percent = 1 - percent;
            }
            // Since the steps may not divide cleanly into the max value, if the user
            // slid to 0 or 100 percent, we jump to the min/max value. This approach
            // is slightly more intuitive than using `Math.ceil` below, because it
            // follows the user's pointer closer.
            if (percent === 0) {
                this.value = this.min;
            }
            else if (percent === 1) {
                this.value = this.max;
            }
            else {
                const exactValue = this._calculateValue(percent);
                // This calculation finds the closest step by finding the closest
                // whole number divisible by the step relative to the min.
                const closestValue = Math.round((exactValue - this.min) / this.step) * this.step + this.min;
                // The value needs to snap to the min and max.
                this.value = this._clamp(closestValue, this.min, this.max);
            }
        }
        /** Emits a change event if the current value is different from the last emitted value. */
        _emitChangeEvent() {
            this._controlValueAccessorChangeFn(this.value);
            this.valueChange.emit(this.value);
            this.change.emit(this._createChangeEvent());
        }
        /** Emits an input event when the current value is different from the last emitted value. */
        _emitInputEvent() {
            this.input.emit(this._createChangeEvent());
        }
        /** Updates the amount of space between ticks as a percentage of the width of the slider. */
        _updateTickIntervalPercent() {
            if (!this.tickInterval || !this._sliderDimensions) {
                return;
            }
            if (this.tickInterval == 'auto') {
                let trackSize = this.vertical ? this._sliderDimensions.height : this._sliderDimensions.width;
                let pixelsPerStep = trackSize * this.step / (this.max - this.min);
                let stepsPerTick = Math.ceil(MIN_AUTO_TICK_SEPARATION / pixelsPerStep);
                let pixelsPerTick = stepsPerTick * this.step;
                this._tickIntervalPercent = pixelsPerTick / trackSize;
            }
            else {
                this._tickIntervalPercent = this.tickInterval * this.step / (this.max - this.min);
            }
        }
        /** Creates a slider change object from the specified value. */
        _createChangeEvent(value = this.value) {
            let event = new MatSliderChange();
            event.source = this;
            event.value = value;
            return event;
        }
        /** Calculates the percentage of the slider that a value is. */
        _calculatePercentage(value) {
            return ((value || 0) - this.min) / (this.max - this.min);
        }
        /** Calculates the value a percentage of the slider corresponds to. */
        _calculateValue(percentage) {
            return this.min + percentage * (this.max - this.min);
        }
        /** Return a number between two numbers. */
        _clamp(value, min = 0, max = 1) {
            return Math.max(min, Math.min(value, max));
        }
        /**
         * Get the bounding client rect of the slider track element.
         * The track is used rather than the native element to ignore the extra space that the thumb can
         * take up.
         */
        _getSliderDimensions() {
            return this._sliderWrapper ? this._sliderWrapper.nativeElement.getBoundingClientRect() : null;
        }
        /**
         * Focuses the native element.
         * Currently only used to allow a blur event to fire but will be used with keyboard input later.
         */
        _focusHostElement(options) {
            this._elementRef.nativeElement.focus(options);
        }
        /** Blurs the native element. */
        _blurHostElement() {
            this._elementRef.nativeElement.blur();
        }
        /**
         * Sets the model value. Implemented as part of ControlValueAccessor.
         * @param value
         */
        writeValue(value) {
            this.value = value;
        }
        /**
         * Registers a callback to be triggered when the value has changed.
         * Implemented as part of ControlValueAccessor.
         * @param fn Callback to be registered.
         */
        registerOnChange(fn) {
            this._controlValueAccessorChangeFn = fn;
        }
        /**
         * Registers a callback to be triggered when the component is touched.
         * Implemented as part of ControlValueAccessor.
         * @param fn Callback to be registered.
         */
        registerOnTouched(fn) {
            this.onTouched = fn;
        }
        /**
         * Sets whether the component should be disabled.
         * Implemented as part of ControlValueAccessor.
         * @param isDisabled
         */
        setDisabledState(isDisabled) {
            this.disabled = isDisabled;
        }
    }
    MatSlider.decorators = [
        { type: Component, args: [{
                    selector: 'mat-slider',
                    exportAs: 'matSlider',
                    providers: [MAT_SLIDER_VALUE_ACCESSOR],
                    host: {
                        '(focus)': '_onFocus()',
                        '(blur)': '_onBlur()',
                        '(keydown)': '_onKeydown($event)',
                        '(keyup)': '_onKeyup()',
                        '(mouseenter)': '_onMouseenter()',
                        // On Safari starting to slide temporarily triggers text selection mode which
                        // show the wrong cursor. We prevent it by stopping the `selectstart` event.
                        '(selectstart)': '$event.preventDefault()',
                        'class': 'mat-slider mat-focus-indicator',
                        'role': 'slider',
                        '[tabIndex]': 'tabIndex',
                        '[attr.aria-disabled]': 'disabled',
                        '[attr.aria-valuemax]': 'max',
                        '[attr.aria-valuemin]': 'min',
                        '[attr.aria-valuenow]': 'value',
                        '[attr.aria-orientation]': 'vertical ? "vertical" : "horizontal"',
                        '[class.mat-slider-disabled]': 'disabled',
                        '[class.mat-slider-has-ticks]': 'tickInterval',
                        '[class.mat-slider-horizontal]': '!vertical',
                        '[class.mat-slider-axis-inverted]': '_invertAxis',
                        // Class binding which is only used by the test harness as there is no other
                        // way for the harness to detect if mouse coordinates need to be inverted.
                        '[class.mat-slider-invert-mouse-coords]': '_shouldInvertMouseCoords()',
                        '[class.mat-slider-sliding]': '_isSliding',
                        '[class.mat-slider-thumb-label-showing]': 'thumbLabel',
                        '[class.mat-slider-vertical]': 'vertical',
                        '[class.mat-slider-min-value]': '_isMinValue',
                        '[class.mat-slider-hide-last-tick]': 'disabled || _isMinValue && _thumbGap && _invertAxis',
                        '[class._mat-animation-noopable]': '_animationMode === "NoopAnimations"',
                    },
                    template: "<div class=\"mat-slider-wrapper\" #sliderWrapper>\n  <div class=\"mat-slider-track-wrapper\">\n    <div class=\"mat-slider-track-background\" [ngStyle]=\"_trackBackgroundStyles\"></div>\n    <div class=\"mat-slider-track-fill\" [ngStyle]=\"_trackFillStyles\"></div>\n  </div>\n  <div class=\"mat-slider-ticks-container\" [ngStyle]=\"_ticksContainerStyles\">\n    <div class=\"mat-slider-ticks\" [ngStyle]=\"_ticksStyles\"></div>\n  </div>\n  <div class=\"mat-slider-thumb-container\" [ngStyle]=\"_thumbContainerStyles\">\n    <div class=\"mat-slider-focus-ring\"></div>\n    <div class=\"mat-slider-thumb\"></div>\n    <div class=\"mat-slider-thumb-label\">\n      <span class=\"mat-slider-thumb-label-text\">{{displayValue}}</span>\n    </div>\n  </div>\n</div>\n",
                    inputs: ['disabled', 'color', 'tabIndex'],
                    encapsulation: ViewEncapsulation.None,
                    changeDetection: ChangeDetectionStrategy.OnPush,
                    styles: [".mat-slider{display:inline-block;position:relative;box-sizing:border-box;padding:8px;outline:none;vertical-align:middle}.mat-slider:not(.mat-slider-disabled):active,.mat-slider.mat-slider-sliding:not(.mat-slider-disabled){cursor:-webkit-grabbing;cursor:grabbing}.mat-slider-wrapper{position:absolute}.mat-slider-track-wrapper{position:absolute;top:0;left:0;overflow:hidden}.mat-slider-track-fill{position:absolute;transform-origin:0 0;transition:transform 400ms cubic-bezier(0.25, 0.8, 0.25, 1),background-color 400ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-slider-track-background{position:absolute;transform-origin:100% 100%;transition:transform 400ms cubic-bezier(0.25, 0.8, 0.25, 1),background-color 400ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-slider-ticks-container{position:absolute;left:0;top:0;overflow:hidden}.mat-slider-ticks{background-repeat:repeat;background-clip:content-box;box-sizing:border-box;opacity:0;transition:opacity 400ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-slider-thumb-container{position:absolute;z-index:1;transition:transform 400ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-slider-focus-ring{position:absolute;width:30px;height:30px;border-radius:50%;transform:scale(0);opacity:0;transition:transform 400ms cubic-bezier(0.25, 0.8, 0.25, 1),background-color 400ms cubic-bezier(0.25, 0.8, 0.25, 1),opacity 400ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-slider.cdk-keyboard-focused .mat-slider-focus-ring,.mat-slider.cdk-program-focused .mat-slider-focus-ring{transform:scale(1);opacity:1}.mat-slider:not(.mat-slider-disabled):not(.mat-slider-sliding) .mat-slider-thumb-label,.mat-slider:not(.mat-slider-disabled):not(.mat-slider-sliding) .mat-slider-thumb{cursor:-webkit-grab;cursor:grab}.mat-slider-thumb{position:absolute;right:-10px;bottom:-10px;box-sizing:border-box;width:20px;height:20px;border:3px solid transparent;border-radius:50%;transform:scale(0.7);transition:transform 400ms cubic-bezier(0.25, 0.8, 0.25, 1),background-color 400ms cubic-bezier(0.25, 0.8, 0.25, 1),border-color 400ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-slider-thumb-label{display:none;align-items:center;justify-content:center;position:absolute;width:28px;height:28px;border-radius:50%;transition:transform 400ms cubic-bezier(0.25, 0.8, 0.25, 1),border-radius 400ms cubic-bezier(0.25, 0.8, 0.25, 1),background-color 400ms cubic-bezier(0.25, 0.8, 0.25, 1)}.cdk-high-contrast-active .mat-slider-thumb-label{outline:solid 1px}.mat-slider-thumb-label-text{z-index:1;opacity:0;transition:opacity 400ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-slider-sliding .mat-slider-track-fill,.mat-slider-sliding .mat-slider-track-background,.mat-slider-sliding .mat-slider-thumb-container{transition-duration:0ms}.mat-slider-has-ticks .mat-slider-wrapper::after{content:\"\";position:absolute;border-width:0;border-style:solid;opacity:0;transition:opacity 400ms cubic-bezier(0.25, 0.8, 0.25, 1)}.mat-slider-has-ticks.cdk-focused:not(.mat-slider-hide-last-tick) .mat-slider-wrapper::after,.mat-slider-has-ticks:hover:not(.mat-slider-hide-last-tick) .mat-slider-wrapper::after{opacity:1}.mat-slider-has-ticks.cdk-focused:not(.mat-slider-disabled) .mat-slider-ticks,.mat-slider-has-ticks:hover:not(.mat-slider-disabled) .mat-slider-ticks{opacity:1}.mat-slider-thumb-label-showing .mat-slider-focus-ring{display:none}.mat-slider-thumb-label-showing .mat-slider-thumb-label{display:flex}.mat-slider-axis-inverted .mat-slider-track-fill{transform-origin:100% 100%}.mat-slider-axis-inverted .mat-slider-track-background{transform-origin:0 0}.mat-slider:not(.mat-slider-disabled).cdk-focused.mat-slider-thumb-label-showing .mat-slider-thumb{transform:scale(0)}.mat-slider:not(.mat-slider-disabled).cdk-focused .mat-slider-thumb-label{border-radius:50% 50% 0}.mat-slider:not(.mat-slider-disabled).cdk-focused .mat-slider-thumb-label-text{opacity:1}.mat-slider:not(.mat-slider-disabled).cdk-mouse-focused .mat-slider-thumb,.mat-slider:not(.mat-slider-disabled).cdk-touch-focused .mat-slider-thumb,.mat-slider:not(.mat-slider-disabled).cdk-program-focused .mat-slider-thumb{border-width:2px;transform:scale(1)}.mat-slider-disabled .mat-slider-focus-ring{transform:scale(0);opacity:0}.mat-slider-disabled .mat-slider-thumb{border-width:4px;transform:scale(0.5)}.mat-slider-disabled .mat-slider-thumb-label{display:none}.mat-slider-horizontal{height:48px;min-width:128px}.mat-slider-horizontal .mat-slider-wrapper{height:2px;top:23px;left:8px;right:8px}.mat-slider-horizontal .mat-slider-wrapper::after{height:2px;border-left-width:2px;right:0;top:0}.mat-slider-horizontal .mat-slider-track-wrapper{height:2px;width:100%}.mat-slider-horizontal .mat-slider-track-fill{height:2px;width:100%;transform:scaleX(0)}.mat-slider-horizontal .mat-slider-track-background{height:2px;width:100%;transform:scaleX(1)}.mat-slider-horizontal .mat-slider-ticks-container{height:2px;width:100%}.cdk-high-contrast-active .mat-slider-horizontal .mat-slider-ticks-container{height:0;outline:solid 2px;top:1px}.mat-slider-horizontal .mat-slider-ticks{height:2px;width:100%}.mat-slider-horizontal .mat-slider-thumb-container{width:100%;height:0;top:50%}.mat-slider-horizontal .mat-slider-focus-ring{top:-15px;right:-15px}.mat-slider-horizontal .mat-slider-thumb-label{right:-14px;top:-40px;transform:translateY(26px) scale(0.01) rotate(45deg)}.mat-slider-horizontal .mat-slider-thumb-label-text{transform:rotate(-45deg)}.mat-slider-horizontal.cdk-focused .mat-slider-thumb-label{transform:rotate(45deg)}.cdk-high-contrast-active .mat-slider-horizontal.cdk-focused .mat-slider-thumb-label,.cdk-high-contrast-active .mat-slider-horizontal.cdk-focused .mat-slider-thumb-label-text{transform:none}.mat-slider-vertical{width:48px;min-height:128px}.mat-slider-vertical .mat-slider-wrapper{width:2px;top:8px;bottom:8px;left:23px}.mat-slider-vertical .mat-slider-wrapper::after{width:2px;border-top-width:2px;bottom:0;left:0}.mat-slider-vertical .mat-slider-track-wrapper{height:100%;width:2px}.mat-slider-vertical .mat-slider-track-fill{height:100%;width:2px;transform:scaleY(0)}.mat-slider-vertical .mat-slider-track-background{height:100%;width:2px;transform:scaleY(1)}.mat-slider-vertical .mat-slider-ticks-container{width:2px;height:100%}.cdk-high-contrast-active .mat-slider-vertical .mat-slider-ticks-container{width:0;outline:solid 2px;left:1px}.mat-slider-vertical .mat-slider-focus-ring{bottom:-15px;left:-15px}.mat-slider-vertical .mat-slider-ticks{width:2px;height:100%}.mat-slider-vertical .mat-slider-thumb-container{height:100%;width:0;left:50%}.mat-slider-vertical .mat-slider-thumb{-webkit-backface-visibility:hidden;backface-visibility:hidden}.mat-slider-vertical .mat-slider-thumb-label{bottom:-14px;left:-40px;transform:translateX(26px) scale(0.01) rotate(-45deg)}.mat-slider-vertical .mat-slider-thumb-label-text{transform:rotate(45deg)}.mat-slider-vertical.cdk-focused .mat-slider-thumb-label{transform:rotate(-45deg)}[dir=rtl] .mat-slider-wrapper::after{left:0;right:auto}[dir=rtl] .mat-slider-horizontal .mat-slider-track-fill{transform-origin:100% 100%}[dir=rtl] .mat-slider-horizontal .mat-slider-track-background{transform-origin:0 0}[dir=rtl] .mat-slider-horizontal.mat-slider-axis-inverted .mat-slider-track-fill{transform-origin:0 0}[dir=rtl] .mat-slider-horizontal.mat-slider-axis-inverted .mat-slider-track-background{transform-origin:100% 100%}.mat-slider._mat-animation-noopable .mat-slider-track-fill,.mat-slider._mat-animation-noopable .mat-slider-track-background,.mat-slider._mat-animation-noopable .mat-slider-ticks,.mat-slider._mat-animation-noopable .mat-slider-thumb-container,.mat-slider._mat-animation-noopable .mat-slider-focus-ring,.mat-slider._mat-animation-noopable .mat-slider-thumb,.mat-slider._mat-animation-noopable .mat-slider-thumb-label,.mat-slider._mat-animation-noopable .mat-slider-thumb-label-text,.mat-slider._mat-animation-noopable .mat-slider-has-ticks .mat-slider-wrapper::after{transition:none}\n"]
                },] }
    ];
    MatSlider.ctorParameters = () => [
        { type: ElementRef },
        { type: FocusMonitor },
        { type: ChangeDetectorRef },
        { type: Directionality, decorators: [{ type: Optional }] },
        { type: String, decorators: [{ type: Attribute, args: ['tabindex',] }] },
        { type: NgZone },
        { type: undefined, decorators: [{ type: Inject, args: [DOCUMENT,] }] },
        { type: String, decorators: [{ type: Optional }, { type: Inject, args: [ANIMATION_MODULE_TYPE,] }] }
    ];
    MatSlider.propDecorators = {
        invert: [{ type: Input }],
        max: [{ type: Input }],
        min: [{ type: Input }],
        step: [{ type: Input }],
        thumbLabel: [{ type: Input }],
        tickInterval: [{ type: Input }],
        value: [{ type: Input }],
        displayWith: [{ type: Input }],
        vertical: [{ type: Input }],
        change: [{ type: Output }],
        input: [{ type: Output }],
        valueChange: [{ type: Output }],
        _sliderWrapper: [{ type: ViewChild, args: ['sliderWrapper',] }]
    };
    return MatSlider;
})();
export { MatSlider };
/** Returns whether an event is a touch event. */
function isTouchEvent(event) {
    // This function is called for every pixel that the user has dragged so we need it to be
    // as fast as possible. Since we only bind mouse events and touch events, we can assume
    // that if the event's name starts with `t`, it's a touch event.
    return event.type[0] === 't';
}
/** Gets the coordinates of a touch or mouse event relative to the viewport. */
function getPointerPositionOnPage(event) {
    // `touches` will be empty for start/end events so we have to fall back to `changedTouches`.
    const point = isTouchEvent(event) ? (event.touches[0] || event.changedTouches[0]) : event;
    return { x: point.clientX, y: point.clientY };
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic2xpZGVyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL3NsaWRlci9zbGlkZXIudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFlBQVksRUFBYyxNQUFNLG1CQUFtQixDQUFDO0FBQzVELE9BQU8sRUFBQyxjQUFjLEVBQUMsTUFBTSxtQkFBbUIsQ0FBQztBQUNqRCxPQUFPLEVBRUwscUJBQXFCLEVBQ3JCLG9CQUFvQixFQUVyQixNQUFNLHVCQUF1QixDQUFDO0FBQy9CLE9BQU8sRUFDTCxVQUFVLEVBQ1YsR0FBRyxFQUNILElBQUksRUFDSixVQUFVLEVBQ1YsU0FBUyxFQUNULE9BQU8sRUFDUCxXQUFXLEVBQ1gsUUFBUSxFQUNSLGNBQWMsR0FDZixNQUFNLHVCQUF1QixDQUFDO0FBQy9CLE9BQU8sRUFDTCxTQUFTLEVBQ1QsdUJBQXVCLEVBQ3ZCLGlCQUFpQixFQUNqQixTQUFTLEVBQ1QsVUFBVSxFQUNWLFlBQVksRUFDWixVQUFVLEVBQ1YsTUFBTSxFQUNOLEtBQUssRUFFTCxRQUFRLEVBQ1IsTUFBTSxFQUNOLFNBQVMsRUFDVCxpQkFBaUIsRUFDakIsTUFBTSxHQUVQLE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBdUIsaUJBQWlCLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQUN2RSxPQUFPLEVBT0wsVUFBVSxFQUNWLGFBQWEsRUFDYixhQUFhLEdBQ2QsTUFBTSx3QkFBd0IsQ0FBQztBQUNoQyxPQUFPLEVBQUMscUJBQXFCLEVBQUMsTUFBTSxzQ0FBc0MsQ0FBQztBQUMzRSxPQUFPLEVBQUMsK0JBQStCLEVBQUMsTUFBTSx1QkFBdUIsQ0FBQztBQUN0RSxPQUFPLEVBQUMsUUFBUSxFQUFDLE1BQU0saUJBQWlCLENBQUM7QUFDekMsT0FBTyxFQUFDLFlBQVksRUFBQyxNQUFNLE1BQU0sQ0FBQztBQUVsQyxNQUFNLGtCQUFrQixHQUFH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UyxTQUFDLFVBQVU7Z0JBeGNqQyxNQUFNO2dEQTBjTyxNQUFNLFNBQUMsUUFBUTs2Q0FDZixRQUFRLFlBQUksTUFBTSxTQUFDLHFCQUFxQjs7O3lCQXJWcEQsS0FBSztzQkFRTCxLQUFLO3NCQVlMLEtBQUs7dUJBaUJMLEtBQUs7NkJBZUwsS0FBSzsrQkFTTCxLQUFLO3dCQWNMLEtBQUs7OEJBZ0NMLEtBQUs7MkJBR0wsS0FBSzt5QkFRTCxNQUFNO3dCQUdOLE1BQU07OEJBT04sTUFBTTtpQ0F5TE4sU0FBUyxTQUFDLGVBQWU7O0lBK2E1QixnQkFBQztLQUFBO1NBM3VCWSxTQUFTO0FBNnVCdEIsaURBQWlEO0FBQ2pELFNBQVMsWUFBWSxDQUFDLEtBQThCO0lBQ2xELHdGQUF3RjtJQUN4Rix1RkFBdUY7SUFDdkYsZ0VBQWdFO0lBQ2hFLE9BQU8sS0FBSyxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsS0FBSyxHQUFHLENBQUM7QUFDL0IsQ0FBQztBQUVELCtFQUErRTtBQUMvRSxTQUFTLHdCQUF3QixDQUFDLEtBQThCO0lBQzlELDRGQUE0RjtJQUM1RixNQUFNLEtBQUssR0FBRyxZQUFZLENBQUMsS0FBSyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsS0FBSyxDQUFDLE9BQU8sQ0FBQyxDQUFDLENBQUMsSUFBSSxLQUFLLENBQUMsY0FBYyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLEtBQUssQ0FBQztJQUMxRixPQUFPLEVBQUMsQ0FBQyxFQUFFLEtBQUssQ0FBQyxPQUFPLEVBQUUsQ0FBQyxFQUFFLEtBQUssQ0FBQyxPQUFPLEVBQUMsQ0FBQztBQUM5QyxDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Rm9jdXNNb25pdG9yLCBGb2N1c09yaWdpbn0gZnJvbSAnQGFuZ3VsYXIvY2RrL2ExMXknO1xuaW1wb3J0IHtEaXJlY3Rpb25hbGl0eX0gZnJvbSAnQGFuZ3VsYXIvY2RrL2JpZGknO1xuaW1wb3J0IHtcbiAgQm9vbGVhbklucHV0LFxuICBjb2VyY2VCb29sZWFuUHJvcGVydHksXG4gIGNvZXJjZU51bWJlclByb3BlcnR5LFxuICBOdW1iZXJJbnB1dFxufSBmcm9tICdAYW5ndWxhci9jZGsvY29lcmNpb24nO1xuaW1wb3J0IHtcbiAgRE9XTl9BUlJPVyxcbiAgRU5ELFxuICBIT01FLFxuICBMRUZUX0FSUk9XLFxuICBQQUdFX0RPV04sXG4gIFBBR0VfVVAsXG4gIFJJR0hUX0FSUk9XLFxuICBVUF9BUlJPVyxcbiAgaGFzTW9kaWZpZXJLZXksXG59IGZyb20gJ0Bhbmd1bGFyL2Nkay9rZXljb2Rlcyc7XG5pbXBvcnQge1xuICBBdHRyaWJ1dGUsXG4gIENoYW5nZURldGVjdGlvblN0cmF0ZWd5LFxuICBDaGFuZ2VEZXRlY3RvclJlZixcbiAgQ29tcG9uZW50LFxuICBFbGVtZW50UmVmLFxuICBFdmVudEVtaXR0ZXIsXG4gIGZvcndhcmRSZWYsXG4gIEluamVjdCxcbiAgSW5wdXQsXG4gIE9uRGVzdHJveSxcbiAgT3B0aW9uYWwsXG4gIE91dHB1dCxcbiAgVmlld0NoaWxkLFxuICBWaWV3RW5jYXBzdWxhdGlvbixcbiAgTmdab25lLFxuICBBZnRlclZpZXdJbml0LFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7Q29udHJvbFZhbHVlQWNjZXNzb3IsIE5HX1ZBTFVFX0FDQ0VTU09SfSBmcm9tICdAYW5ndWxhci9mb3Jtcyc7XG5pbXBvcnQge1xuICBDYW5Db2xvcixcbiAgQ2FuQ29sb3JDdG9yLFxuICBDYW5EaXNhYmxlLFxuICBDYW5EaXNhYmxlQ3RvcixcbiAgSGFzVGFiSW5kZXgsXG4gIEhhc1RhYkluZGV4Q3RvcixcbiAgbWl4aW5Db2xvcixcbiAgbWl4aW5EaXNhYmxlZCxcbiAgbWl4aW5UYWJJbmRleCxcbn0gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvY29yZSc7XG5pbXBvcnQge0FOSU1BVElPTl9NT0RVTEVfVFlQRX0gZnJvbSAnQGFuZ3VsYXIvcGxhdGZvcm0tYnJvd3Nlci9hbmltYXRpb25zJztcbmltcG9ydCB7bm9ybWFsaXplUGFzc2l2ZUxpc3RlbmVyT3B0aW9uc30gZnJvbSAnQGFuZ3VsYXIvY2RrL3BsYXRmb3JtJztcbmltcG9ydCB7RE9DVU1FTlR9IGZyb20gJ0Bhbmd1bGFyL2NvbW1vbic7XG5pbXBvcnQge1N1YnNjcmlwdGlvbn0gZnJvbSAncnhqcyc7XG5cbmNvbnN0IGFjdGl2ZUV2ZW50T3B0aW9ucyA9IG5vcm1hbGl6ZVBhc3NpdmVMaXN0ZW5lck9wdGlvbnMoe3Bhc3NpdmU6IGZhbHNlfSk7XG5cbi8qKlxuICogVmlzdWFsbHksIGEgMzBweCBzZXBhcmF0aW9uIGJldHdlZW4gdGljayBtYXJrcyBsb29rcyBiZXN0LiBUaGlzIGlzIHZlcnkgc3ViamVjdGl2ZSBidXQgaXQgaXNcbiAqIHRoZSBkZWZhdWx0IHNlcGFyYXRpb24gd2UgY2hvc2UuXG4gKi9cbmNvbnN0IE1JTl9BVVRPX1RJQ0tfU0VQQVJBVElPTiA9IDMwO1xuXG4vKiogVGhlIHRodW1iIGdhcCBzaXplIGZvciBhIGRpc2FibGVkIHNsaWRlci4gKi9cbmNvbnN0IERJU0FCTEVEX1RIVU1CX0dBUCA9IDc7XG5cbi8qKiBUaGUgdGh1bWIgZ2FwIHNpemUgZm9yIGEgbm9uLWFjdGl2ZSBzbGlkZXIgYXQgaXRzIG1pbmltdW0gdmFsdWUuICovXG5jb25zdCBNSU5fVkFMVUVfTk9OQUNUSVZFX1RIVU1CX0dBUCA9IDc7XG5cbi8qKiBUaGUgdGh1bWIgZ2FwIHNpemUgZm9yIGFuIGFjdGl2ZSBzbGlkZXIgYXQgaXRzIG1pbmltdW0gdmFsdWUuICovXG5jb25zdCBNSU5fVkFMVUVfQUNUSVZFX1RIVU1CX0dBUCA9IDEwO1xuXG4vKipcbiAqIFByb3ZpZGVyIEV4cHJlc3Npb24gdGhhdCBhbGxvd3MgbWF0LXNsaWRlciB0byByZWdpc3RlciBhcyBhIENvbnRyb2xWYWx1ZUFjY2Vzc29yLlxuICogVGhpcyBhbGxvd3MgaXQgdG8gc3VwcG9ydCBbKG5nTW9kZWwpXSBhbmQgW2Zvcm1Db250cm9sXS5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuZXhwb3J0IGNvbnN0IE1BVF9TTElERVJfVkFMVUVfQUNDRVNTT1I6IGFueSA9IHtcbiAgcHJvdmlkZTogTkdfVkFMVUVfQUNDRVNTT1IsXG4gIHVzZUV4aXN0aW5nOiBmb3J3YXJkUmVmKCgpID0+IE1hdFNsaWRlciksXG4gIG11bHRpOiB0cnVlXG59O1xuXG4vKiogQSBzaW1wbGUgY2hhbmdlIGV2ZW50IGVtaXR0ZWQgYnkgdGhlIE1hdFNsaWRlciBjb21wb25lbnQuICovXG5leHBvcnQgY2xhc3MgTWF0U2xpZGVyQ2hhbmdlIHtcbiAgLyoqIFRoZSBNYXRTbGlkZXIgdGhhdCBjaGFuZ2VkLiAqL1xuICBzb3VyY2U6IE1hdFNsaWRlcjtcblxuICAvKiogVGhlIG5ldyB2YWx1ZSBvZiB0aGUgc291cmNlIHNsaWRlci4gKi9cbiAgdmFsdWU6IG51bWJlciB8IG51bGw7XG59XG5cbi8vIEJvaWxlcnBsYXRlIGZvciBhcHBseWluZyBtaXhpbnMgdG8gTWF0U2xpZGVyLlxuLyoqIEBkb2NzLXByaXZhdGUgKi9cbmNsYXNzIE1hdFNsaWRlckJhc2Uge1xuICBjb25zdHJ1Y3RvcihwdWJsaWMgX2VsZW1lbnRSZWY6IEVsZW1lbnRSZWYpIHt9XG59XG5jb25zdCBfTWF0U2xpZGVyTWl4aW5CYXNlOlxuICAgIEhhc1RhYkluZGV4Q3RvciAmXG4gICAgQ2FuQ29sb3JDdG9yICZcbiAgICBDYW5EaXNhYmxlQ3RvciAmXG4gICAgdHlwZW9mIE1hdFNsaWRlckJhc2UgPVxuICAgICAgICBtaXhpblRhYkluZGV4KG1peGluQ29sb3IobWl4aW5EaXNhYmxlZChNYXRTbGlkZXJCYXNlKSwgJ2FjY2VudCcpKTtcblxuLyoqXG4gKiBBbGxvd3MgdXNlcnMgdG8gc2VsZWN0IGZyb20gYSByYW5nZSBvZiB2YWx1ZXMgYnkgbW92aW5nIHRoZSBzbGlkZXIgdGh1bWIuIEl0IGlzIHNpbWlsYXIgaW5cbiAqIGJlaGF2aW9yIHRvIHRoZSBuYXRpdmUgYDxpbnB1dCB0eXBlPVwicmFuZ2VcIj5gIGVsZW1lbnQuXG4gKi9cbkBDb21wb25lbnQoe1xuICBzZWxlY3RvcjogJ21hdC1zbGlkZXInLFxuICBleHBvcnRBczogJ21hdFNsaWRlcicsXG4gIHByb3ZpZGVyczogW01BVF9TTElERVJfVkFMVUVfQUNDRVNTT1JdLFxuICBob3N0OiB7XG4gICAgJyhmb2N1cyknOiAnX29uRm9jdXMoKScsXG4gICAgJyhibHVyKSc6ICdfb25CbHVyKCknLFxuICAgICcoa2V5ZG93biknOiAnX29uS2V5ZG93bigkZXZlbnQpJyxcbiAgICAnKGtleXVwKSc6ICdfb25LZXl1cCgpJyxcbiAgICAnKG1vdXNlZW50ZXIpJzogJ19vbk1vdXNlZW50ZXIoKScsXG5cbiAgICAvLyBPbiBTYWZhcmkgc3RhcnRpbmcgdG8gc2xpZGUgdGVtcG9yYXJpbHkgdHJpZ2dlcnMgdGV4dCBzZWxlY3Rpb24gbW9kZSB3aGljaFxuICAgIC8vIHNob3cgdGhlIHdyb25nIGN1cnNvci4gV2UgcHJldmVudCBpdCBieSBzdG9wcGluZyB0aGUgYHNlbGVjdHN0YXJ0YCBldmVudC5cbiAgICAnKHNlbGVjdHN0YXJ0KSc6ICckZXZlbnQucHJldmVudERlZmF1bHQoKScsXG4gICAgJ2NsYXNzJzogJ21hdC1zbGlkZXIgbWF0LWZvY3VzLWluZGljYXRvcicsXG4gICAgJ3JvbGUnOiAnc2xpZGVyJyxcbiAgICAnW3RhYkluZGV4XSc6ICd0YWJJbmRleCcsXG4gICAgJ1thdHRyLmFyaWEtZGlzYWJsZWRdJzogJ2Rpc2FibGVkJyxcbiAgICAnW2F0dHIuYXJpYS12YWx1ZW1heF0nOiAnbWF4JyxcbiAgICAnW2F0dHIuYXJpYS12YWx1ZW1pbl0nOiAnbWluJyxcbiAgICAnW2F0dHIuYXJpYS12YWx1ZW5vd10nOiAndmFsdWUnLFxuICAgICdbYXR0ci5hcmlhLW9yaWVudGF0aW9uXSc6ICd2ZXJ0aWNhbCA/IFwidmVydGljYWxcIiA6IFwiaG9yaXpvbnRhbFwiJyxcbiAgICAnW2NsYXNzLm1hdC1zbGlkZXItZGlzYWJsZWRdJzogJ2Rpc2FibGVkJyxcbiAgICAnW2NsYXNzLm1hdC1zbGlkZXItaGFzLXRpY2tzXSc6ICd0aWNrSW50ZXJ2YWwnLFxuICAgICdbY2xhc3MubWF0LXNsaWRlci1ob3Jpem9udGFsXSc6ICchdmVydGljYWwnLFxuICAgICdbY2xhc3MubWF0LXNsaWRlci1heGlzLWludmVydGVkXSc6ICdfaW52ZXJ0QXhpcycsXG4gICAgLy8gQ2xhc3MgYmluZGluZyB3aGljaCBpcyBvbmx5IHVzZWQgYnkgdGhlIHRlc3QgaGFybmVzcyBhcyB0aGVyZSBpcyBubyBvdGhlclxuICAgIC8vIHdheSBmb3IgdGhlIGhhcm5lc3MgdG8gZGV0ZWN0IGlmIG1vdXNlIGNvb3JkaW5hdGVzIG5lZWQgdG8gYmUgaW52ZXJ0ZWQuXG4gICAgJ1tjbGFzcy5tYXQtc2xpZGVyLWludmVydC1tb3VzZS1jb29yZHNdJzogJ19zaG91bGRJbnZlcnRNb3VzZUNvb3JkcygpJyxcbiAgICAnW2NsYXNzLm1hdC1zbGlkZXItc2xpZGluZ10nOiAnX2lzU2xpZGluZycsXG4gICAgJ1tjbGFzcy5tYXQtc2xpZGVyLXRodW1iLWxhYmVsLXNob3dpbmddJzogJ3RodW1iTGFiZWwnLFxuICAgICdbY2xhc3MubWF0LXNsaWRlci12ZXJ0aWNhbF0nOiAndmVydGljYWwnLFxuICAgICdbY2xhc3MubWF0LXNsaWRlci1taW4tdmFsdWVdJzogJ19pc01pblZhbHVlJyxcbiAgICAnW2NsYXNzLm1hdC1zbGlkZXItaGlkZS1sYXN0LXRpY2tdJzogJ2Rpc2FibGVkIHx8IF9pc01pblZhbHVlICYmIF90aHVtYkdhcCAmJiBfaW52ZXJ0QXhpcycsXG4gICAgJ1tjbGFzcy5fbWF0LWFuaW1hdGlvbi1ub29wYWJsZV0nOiAnX2FuaW1hdGlvbk1vZGUgPT09IFwiTm9vcEFuaW1hdGlvbnNcIicsXG4gIH0sXG4gIHRlbXBsYXRlVXJsOiAnc2xpZGVyLmh0bWwnLFxuICBzdHlsZVVybHM6IFsnc2xpZGVyLmNzcyddLFxuICBpbnB1dHM6IFsnZGlzYWJsZWQnLCAnY29sb3InLCAndGFiSW5kZXgnXSxcbiAgZW5jYXBzdWxhdGlvbjogVmlld0VuY2Fwc3VsYXRpb24uTm9uZSxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2gsXG59KVxuZXhwb3J0IGNsYXNzIE1hdFNsaWRlciBleHRlbmRzIF9NYXRTbGlkZXJNaXhpbkJhc2VcbiAgICBpbXBsZW1lbnRzIENvbnRyb2xWYWx1ZUFjY2Vzc29yLCBPbkRlc3Ryb3ksIENhbkRpc2FibGUsIENhbkNvbG9yLCBBZnRlclZpZXdJbml0LCBIYXNUYWJJbmRleCB7XG4gIC8qKiBXaGV0aGVyIHRoZSBzbGlkZXIgaXMgaW52ZXJ0ZWQuICovXG4gIEBJbnB1dCgpXG4gIGdldCBpbnZlcnQoKTogYm9vbGVhbiB7IHJldHVybiB0aGlzLl9pbnZlcnQ7IH1cbiAgc2V0IGludmVydCh2YWx1ZTogYm9vbGVhbikge1xuICAgIHRoaXMuX2ludmVydCA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh2YWx1ZSk7XG4gIH1cbiAgcHJpdmF0ZSBfaW52ZXJ0ID0gZmFsc2U7XG5cbiAgLyoqIFRoZSBtYXhpbXVtIHZhbHVlIHRoYXQgdGhlIHNsaWRlciBjYW4gaGF2ZS4gKi9cbiAgQElucHV0KClcbiAgZ2V0IG1heCgpOiBudW1iZXIgeyByZXR1cm4gdGhpcy5fbWF4OyB9XG4gIHNldCBtYXgodjogbnVtYmVyKSB7XG4gICAgdGhpcy5fbWF4ID0gY29lcmNlTnVtYmVyUHJvcGVydHkodiwgdGhpcy5fbWF4KTtcbiAgICB0aGlzLl9wZXJjZW50ID0gdGhpcy5fY2FsY3VsYXRlUGVyY2VudGFnZSh0aGlzLl92YWx1ZSk7XG5cbiAgICAvLyBTaW5jZSB0aGlzIGFsc28gbW9kaWZpZXMgdGhlIHBlcmNlbnRhZ2UsIHdlIG5lZWQgdG8gbGV0IHRoZSBjaGFuZ2UgZGV0ZWN0aW9uIGtub3cuXG4gICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3JSZWYubWFya0ZvckNoZWNrKCk7XG4gIH1cbiAgcHJpdmF0ZSBfbWF4OiBudW1iZXIgPSAxMDA7XG5cbiAgLyoqIFRoZSBtaW5pbXVtIHZhbHVlIHRoYXQgdGhlIHNsaWRlciBjYW4gaGF2ZS4gKi9cbiAgQElucHV0KClcbiAgZ2V0IG1pbigpOiBudW1iZXIgeyByZXR1cm4gdGhpcy5fbWluOyB9XG4gIHNldCBtaW4odjogbnVtYmVyKSB7XG4gICAgdGhpcy5fbWluID0gY29lcmNlTnVtYmVyUHJvcGVydHkodiwgdGhpcy5fbWluKTtcblxuICAgIC8vIElmIHRoZSB2YWx1ZSB3YXNuJ3QgZXhwbGljaXRseSBzZXQgYnkgdGhlIHVzZXIsIHNldCBpdCB0byB0aGUgbWluLlxuICAgIGlmICh0aGlzLl92YWx1ZSA9PT0gbnVsbCkge1xuICAgICAgdGhpcy52YWx1ZSA9IHRoaXMuX21pbjtcbiAgICB9XG4gICAgdGhpcy5fcGVyY2VudCA9IHRoaXMuX2NhbGN1bGF0ZVBlcmNlbnRhZ2UodGhpcy5fdmFsdWUpO1xuXG4gICAgLy8gU2luY2UgdGhpcyBhbHNvIG1vZGlmaWVzIHRoZSBwZXJjZW50YWdlLCB3ZSBuZWVkIHRvIGxldCB0aGUgY2hhbmdlIGRldGVjdGlvbiBrbm93LlxuICAgIHRoaXMuX2NoYW5nZURldGVjdG9yUmVmLm1hcmtGb3JDaGVjaygpO1xuICB9XG4gIHByaXZhdGUgX21pbjogbnVtYmVyID0gMDtcblxuICAvKiogVGhlIHZhbHVlcyBhdCB3aGljaCB0aGUgdGh1bWIgd2lsbCBzbmFwLiAqL1xuICBASW5wdXQoKVxuICBnZXQgc3RlcCgpOiBudW1iZXIgeyByZXR1cm4gdGhpcy5fc3RlcDsgfVxuICBzZXQgc3RlcCh2OiBudW1iZXIpIHtcbiAgICB0aGlzLl9zdGVwID0gY29lcmNlTnVtYmVyUHJvcGVydHkodiwgdGhpcy5fc3RlcCk7XG5cbiAgICBpZiAodGhpcy5fc3RlcCAlIDEgIT09IDApIHtcbiAgICAgIHRoaXMuX3JvdW5kVG9EZWNpbWFsID0gdGhpcy5fc3RlcC50b1N0cmluZygpLnNwbGl0KCcuJykucG9wKCkhLmxlbmd0aDtcbiAgICB9XG5cbiAgICAvLyBTaW5jZSB0aGlzIGNvdWxkIG1vZGlmeSB0aGUgbGFiZWwsIHdlIG5lZWQgdG8gbm90aWZ5IHRoZSBjaGFuZ2UgZGV0ZWN0aW9uLlxuICAgIHRoaXMuX2NoYW5nZURldGVjdG9yUmVmLm1hcmtGb3JDaGVjaygpO1xuICB9XG4gIHByaXZhdGUgX3N0ZXA6IG51bWJlciA9IDE7XG5cbiAgLyoqIFdoZXRoZXIgb3Igbm90IHRvIHNob3cgdGhlIHRodW1iIGxhYmVsLiAqL1xuICBASW5wdXQoKVxuICBnZXQgdGh1bWJMYWJlbCgpOiBib29sZWFuIHsgcmV0dXJuIHRoaXMuX3RodW1iTGFiZWw7IH1cbiAgc2V0IHRodW1iTGFiZWwodmFsdWU6IGJvb2xlYW4pIHsgdGhpcy5fdGh1bWJMYWJlbCA9IGNvZXJjZUJvb2xlYW5Qcm9wZXJ0eSh2YWx1ZSk7IH1cbiAgcHJpdmF0ZSBfdGh1bWJMYWJlbDogYm9vbGVhbiA9IGZhbHNlO1xuXG4gIC8qKlxuICAgKiBIb3cgb2Z0ZW4gdG8gc2hvdyB0aWNrcy4gUmVsYXRpdmUgdG8gdGhlIHN0ZXAgc28gdGhhdCBhIHRpY2sgYWx3YXlzIGFwcGVhcnMgb24gYSBzdGVwLlxuICAgKiBFeDogVGljayBpbnRlcnZhbCBvZiA0IHdpdGggYSBzdGVwIG9mIDMgd2lsbCBkcmF3IGEgdGljayBldmVyeSA0IHN0ZXBzIChldmVyeSAxMiB2YWx1ZXMpLlxuICAgKi9cbiAgQElucHV0KClcbiAgZ2V0IHRpY2tJbnRlcnZhbCgpIHsgcmV0dXJuIHRoaXMuX3RpY2tJbnRlcnZhbDsgfVxuICBzZXQgdGlja0ludGVydmFsKHZhbHVlOiAnYXV0bycgfCBudW1iZXIpIHtcbiAgICBpZiAodmFsdWUgPT09ICdhdXRvJykge1xuICAgICAgdGhpcy5fdGlja0ludGVydmFsID0gJ2F1dG8nO1xuICAgIH0gZWxzZSBpZiAodHlwZW9mIHZhbHVlID09PSAnbnVtYmVyJyB8fCB0eXBlb2YgdmFsdWUgPT09ICdzdHJpbmcnKSB7XG4gICAgICB0aGlzLl90aWNrSW50ZXJ2YWwgPSBjb2VyY2VOdW1iZXJQcm9wZXJ0eSh2YWx1ZSwgdGhpcy5fdGlja0ludGVydmFsIGFzIG51bWJlcik7XG4gICAgfSBlbHNlIHtcbiAgICAgIHRoaXMuX3RpY2tJbnRlcnZhbCA9IDA7XG4gICAgfVxuICB9XG4gIHByaXZhdGUgX3RpY2tJbnRlcnZhbDogJ2F1dG8nIHwgbnVtYmVyID0gMDtcblxuICAvKiogVmFsdWUgb2YgdGhlIHNsaWRlci4gKi9cbiAgQElucHV0KClcbiAgZ2V0IHZhbHVlKCk6IG51bWJlciB8IG51bGwge1xuICAgIC8vIElmIHRoZSB2YWx1ZSBuZWVkcyB0byBiZSByZWFkIGFuZCBpdCBpcyBzdGlsbCB1bmluaXRpYWxpemVkLCBpbml0aWFsaXplIGl0IHRvIHRoZSBtaW4uXG4gICAgaWYgKHRoaXMuX3ZhbHVlID09PSBudWxsKSB7XG4gICAgICB0aGlzLnZhbHVlID0gdGhpcy5fbWluO1xuICAgIH1cbiAgICByZXR1cm4gdGhpcy5fdmFsdWU7XG4gIH1cbiAgc2V0IHZhbHVlKHY6IG51bWJlciB8IG51bGwpIHtcbiAgICBpZiAodiAhPT0gdGhpcy5fdmFsdWUpIHtcbiAgICAgIGxldCB2YWx1ZSA9IGNvZXJjZU51bWJlclByb3BlcnR5KHYpO1xuXG4gICAgICAvLyBXaGlsZSBpbmNyZW1lbnRpbmcgYnkgYSBkZWNpbWFsIHdlIGNhbiBlbmQgdXAgd2l0aCB2YWx1ZXMgbGlrZSAzMy4zMDAwMDAwMDAwMDAwMDQuXG4gICAgICAvLyBUcnVuY2F0ZSBpdCB0byBlbnN1cmUgdGhhdCBpdCBtYXRjaGVzIHRoZSBsYWJlbCBhbmQgdG8gbWFrZSBpdCBlYXNpZXIgdG8gd29yayB3aXRoLlxuICAgICAgaWYgKHRoaXMuX3JvdW5kVG9EZWNpbWFsKSB7XG4gICAgICAgIHZhbHVlID0gcGFyc2VGbG9hdCh2YWx1ZS50b0ZpeGVkKHRoaXMuX3JvdW5kVG9EZWNpbWFsKSk7XG4gICAgICB9XG5cbiAgICAgIHRoaXMuX3ZhbHVlID0gdmFsdWU7XG4gICAgICB0aGlzLl9wZXJjZW50ID0gdGhpcy5fY2FsY3VsYXRlUGVyY2VudGFnZSh0aGlzLl92YWx1ZSk7XG5cbiAgICAgIC8vIFNpbmNlIHRoaXMgYWxzbyBtb2RpZmllcyB0aGUgcGVyY2VudGFnZSwgd2UgbmVlZCB0byBsZXQgdGhlIGNoYW5nZSBkZXRlY3Rpb24ga25vdy5cbiAgICAgIHRoaXMuX2NoYW5nZURldGVjdG9yUmVmLm1hcmtGb3JDaGVjaygpO1xuICAgIH1cbiAgfVxuICBwcml2YXRlIF92YWx1ZTogbnVtYmVyIHwgbnVsbCA9IG51bGw7XG5cbiAgLyoqXG4gICAqIEZ1bmN0aW9uIHRoYXQgd2lsbCBiZSB1c2VkIHRvIGZvcm1hdCB0aGUgdmFsdWUgYmVmb3JlIGl0IGlzIGRpc3BsYXllZFxuICAgKiBpbiB0aGUgdGh1bWIgbGFiZWwuIENhbiBiZSB1c2VkIHRvIGZvcm1hdCB2ZXJ5IGxhcmdlIG51bWJlciBpbiBvcmRlclxuICAgKiBmb3IgdGhlbSB0byBmaXQgaW50byB0aGUgc2xpZGVyIHRodW1iLlxuICAgKi9cbiAgQElucHV0KCkgZGlzcGxheVdpdGg6ICh2YWx1ZTogbnVtYmVyKSA9PiBzdHJpbmcgfCBudW1iZXI7XG5cbiAgLyoqIFdoZXRoZXIgdGhlIHNsaWRlciBpcyB2ZXJ0aWNhbC4gKi9cbiAgQElucHV0KClcbiAgZ2V0IHZlcnRpY2FsKCk6IGJvb2xlYW4geyByZXR1cm4gdGhpcy5fdmVydGljYWw7IH1cbiAgc2V0IHZlcnRpY2FsKHZhbHVlOiBib29sZWFuKSB7XG4gICAgdGhpcy5fdmVydGljYWwgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkodmFsdWUpO1xuICB9XG4gIHByaXZhdGUgX3ZlcnRpY2FsID0gZmFsc2U7XG5cbiAgLyoqIEV2ZW50IGVtaXR0ZWQgd2hlbiB0aGUgc2xpZGVyIHZhbHVlIGhhcyBjaGFuZ2VkLiAqL1xuICBAT3V0cHV0KCkgcmVhZG9ubHkgY2hhbmdlOiBFdmVudEVtaXR0ZXI8TWF0U2xpZGVyQ2hhbmdlPiA9IG5ldyBFdmVudEVtaXR0ZXI8TWF0U2xpZGVyQ2hhbmdlPigpO1xuXG4gIC8qKiBFdmVudCBlbWl0dGVkIHdoZW4gdGhlIHNsaWRlciB0aHVtYiBtb3Zlcy4gKi9cbiAgQE91dHB1dCgpIHJlYWRvbmx5IGlucHV0OiBFdmVudEVtaXR0ZXI8TWF0U2xpZGVyQ2hhbmdlPiA9IG5ldyBFdmVudEVtaXR0ZXI8TWF0U2xpZGVyQ2hhbmdlPigpO1xuXG4gIC8qKlxuICAgKiBFbWl0cyB3aGVuIHRoZSByYXcgdmFsdWUgb2YgdGhlIHNsaWRlciBjaGFuZ2VzLiBUaGlzIGlzIGhlcmUgcHJpbWFyaWx5XG4gICAqIHRvIGZhY2lsaXRhdGUgdGhlIHR3by13YXkgYmluZGluZyBmb3IgdGhlIGB2YWx1ZWAgaW5wdXQuXG4gICAqIEBkb2NzLXByaXZhdGVcbiAgICovXG4gIEBPdXRwdXQoKSByZWFkb25seSB2YWx1ZUNoYW5nZTogRXZlbnRFbWl0dGVyPG51bWJlciB8IG51bGw+ID0gbmV3IEV2ZW50RW1pdHRlcjxudW1iZXIgfCBudWxsPigpO1xuXG4gIC8qKiBUaGUgdmFsdWUgdG8gYmUgdXNlZCBmb3IgZGlzcGxheSBwdXJwb3Nlcy4gKi9cbiAgZ2V0IGRpc3BsYXlWYWx1ZSgpOiBzdHJpbmcgfCBudW1iZXIge1xuICAgIGlmICh0aGlzLmRpc3BsYXlXaXRoKSB7XG4gICAgICAvLyBWYWx1ZSBpcyBuZXZlciBudWxsIGJ1dCBzaW5jZSBzZXR0ZXJzIGFuZCBnZXR0ZXJzIGNhbm5vdCBoYXZlXG4gICAgICAvLyBkaWZmZXJlbnQgdHlwZXMsIHRoZSB2YWx1ZSBnZXR0ZXIgaXMgYWxzbyB0eXBlZCB0byByZXR1cm4gbnVsbC5cbiAgICAgIHJldHVybiB0aGlzLmRpc3BsYXlXaXRoKHRoaXMudmFsdWUhKTtcbiAgICB9XG5cbiAgICAvLyBOb3RlIHRoYXQgdGhpcyBjb3VsZCBiZSBpbXByb3ZlZCBmdXJ0aGVyIGJ5IHJvdW5kaW5nIHNvbWV0aGluZyBsaWtlIDAuOTk5IHRvIDEgb3JcbiAgICAvLyAwLjg5OSB0byAwLjksIGhvd2V2ZXIgaXQgaXMgdmVyeSBwZXJmb3JtYW5jZSBzZW5zaXRpdmUsIGJlY2F1c2UgaXQgZ2V0cyBjYWxsZWQgb25cbiAgICAvLyBldmVyeSBjaGFuZ2UgZGV0ZWN0aW9uIGN5Y2xlLlxuICAgIGlmICh0aGlzLl9yb3VuZFRvRGVjaW1hbCAmJiB0aGlzLnZhbHVlICYmIHRoaXMudmFsdWUgJSAxICE9PSAwKSB7XG4gICAgICByZXR1cm4gdGhpcy52YWx1ZS50b0ZpeGVkKHRoaXMuX3JvdW5kVG9EZWNpbWFsKTtcbiAgICB9XG5cbiAgICByZXR1cm4gdGhpcy52YWx1ZSB8fCAwO1xuICB9XG5cbiAgLyoqIHNldCBmb2N1cyB0byB0aGUgaG9zdCBlbGVtZW50ICovXG4gIGZvY3VzKG9wdGlvbnM/OiBGb2N1c09wdGlvbnMpIHtcbiAgICB0aGlzLl9mb2N1c0hvc3RFbGVtZW50KG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqIGJsdXIgdGhlIGhvc3QgZWxlbWVudCAqL1xuICBibHVyKCkge1xuICAgIHRoaXMuX2JsdXJIb3N0RWxlbWVudCgpO1xuICB9XG5cbiAgLyoqIG9uVG91Y2ggZnVuY3Rpb24gcmVnaXN0ZXJlZCB2aWEgcmVnaXN0ZXJPblRvdWNoIChDb250cm9sVmFsdWVBY2Nlc3NvcikuICovXG4gIG9uVG91Y2hlZDogKCkgPT4gYW55ID0gKCkgPT4ge307XG5cbiAgLyoqIFRoZSBwZXJjZW50YWdlIG9mIHRoZSBzbGlkZXIgdGhhdCBjb2luY2lkZXMgd2l0aCB0aGUgdmFsdWUuICovXG4gIGdldCBwZXJjZW50KCk6IG51bWJlciB7IHJldHVybiB0aGlzLl9jbGFtcCh0aGlzLl9wZXJjZW50KTsgfVxuICBwcml2YXRlIF9wZXJjZW50OiBudW1iZXIgPSAwO1xuXG4gIC8qKlxuICAgKiBXaGV0aGVyIG9yIG5vdCB0aGUgdGh1bWIgaXMgc2xpZGluZy5cbiAgICogVXNlZCB0byBkZXRlcm1pbmUgaWYgdGhlcmUgc2hvdWxkIGJlIGEgdHJhbnNpdGlvbiBmb3IgdGhlIHRodW1iIGFuZCBmaWxsIHRyYWNrLlxuICAgKi9cbiAgX2lzU2xpZGluZzogYm9vbGVhbiA9IGZhbHNlO1xuXG4gIC8qKlxuICAgKiBXaGV0aGVyIG9yIG5vdCB0aGUgc2xpZGVyIGlzIGFjdGl2ZSAoY2xpY2tlZCBvciBzbGlkaW5nKS5cbiAgICogVXNlZCB0byBzaHJpbmsgYW5kIGdyb3cgdGhlIHRodW1iIGFzIGFjY29yZGluZyB0byB0aGUgTWF0ZXJpYWwgRGVzaWduIHNwZWMuXG4gICAqL1xuICBfaXNBY3RpdmU6IGJvb2xlYW4gPSBmYWxzZTtcblxuICAvKipcbiAgICogV2hldGhlciB0aGUgYXhpcyBvZiB0aGUgc2xpZGVyIGlzIGludmVydGVkLlxuICAgKiAoaS5lLiB3aGV0aGVyIG1vdmluZyB0aGUgdGh1bWIgaW4gdGhlIHBvc2l0aXZlIHggb3IgeSBkaXJlY3Rpb24gZGVjcmVhc2VzIHRoZSBzbGlkZXIncyB2YWx1ZSkuXG4gICAqL1xuICBnZXQgX2ludmVydEF4aXMoKSB7XG4gICAgLy8gU3RhbmRhcmQgbm9uLWludmVydGVkIG1vZGUgZm9yIGEgdmVydGljYWwgc2xpZGVyIHNob3VsZCBiZSBkcmFnZ2luZyB0aGUgdGh1bWIgZnJvbSBib3R0b20gdG9cbiAgICAvLyB0b3AuIEhvd2V2ZXIgZnJvbSBhIHktYXhpcyBzdGFuZHBvaW50IHRoaXMgaXMgaW52ZXJ0ZWQuXG4gICAgcmV0dXJuIHRoaXMudmVydGljYWwgPyAhdGhpcy5pbnZlcnQgOiB0aGlzLmludmVydDtcbiAgfVxuXG5cbiAgLyoqIFdoZXRoZXIgdGhlIHNsaWRlciBpcyBhdCBpdHMgbWluaW11bSB2YWx1ZS4gKi9cbiAgZ2V0IF9pc01pblZhbHVlKCkge1xuICAgIHJldHVybiB0aGlzLnBlcmNlbnQgPT09IDA7XG4gIH1cblxuICAvKipcbiAgICogVGhlIGFtb3VudCBvZiBzcGFjZSB0byBsZWF2ZSBiZXR3ZWVuIHRoZSBzbGlkZXIgdGh1bWIgYW5kIHRoZSB0cmFjayBmaWxsICYgdHJhY2sgYmFja2dyb3VuZFxuICAgKiBlbGVtZW50cy5cbiAgICovXG4gIGdldCBfdGh1bWJHYXAoKSB7XG4gICAgaWYgKHRoaXMuZGlzYWJsZWQpIHtcbiAgICAgIHJldHVybiBESVNBQkxFRF9USFVNQl9HQVA7XG4gICAgfVxuICAgIGlmICh0aGlzLl9pc01pblZhbHVlICYmICF0aGlzLnRodW1iTGFiZWwpIHtcbiAgICAgIHJldHVybiB0aGlzLl9pc0FjdGl2ZSA/IE1JTl9WQUxVRV9BQ1RJVkVfVEhVTUJfR0FQIDogTUlOX1ZBTFVFX05PTkFDVElWRV9USFVNQl9HQVA7XG4gICAgfVxuICAgIHJldHVybiAwO1xuICB9XG5cbiAgLyoqIENTUyBzdHlsZXMgZm9yIHRoZSB0cmFjayBiYWNrZ3JvdW5kIGVsZW1lbnQuICovXG4gIGdldCBfdHJhY2tCYWNrZ3JvdW5kU3R5bGVzKCk6IHsgW2tleTogc3RyaW5nXTogc3RyaW5nIH0ge1xuICAgIGNvbnN0IGF4aXMgPSB0aGlzLnZlcnRpY2FsID8gJ1knIDogJ1gnO1xuICAgIGNvbnN0IHNjYWxlID0gdGhpcy52ZXJ0aWNhbCA/IGAxLCAkezEgLSB0aGlzLnBlcmNlbnR9LCAxYCA6IGAkezEgLSB0aGlzLnBlcmNlbnR9LCAxLCAxYDtcbiAgICBjb25zdCBzaWduID0gdGhpcy5fc2hvdWxkSW52ZXJ0TW91c2VDb29yZHMoKSA/ICctJyA6ICcnO1xuXG4gICAgcmV0dXJuIHtcbiAgICAgIC8vIHNjYWxlM2QgYXZvaWRzIHNvbWUgcmVuZGVyaW5nIGlzc3VlcyBpbiBDaHJvbWUuIFNlZSAjMTIwNzEuXG4gICAgICB0cmFuc2Zvcm06IGB0cmFuc2xhdGUke2F4aXN9KCR7c2lnbn0ke3RoaXMuX3RodW1iR2FwfXB4KSBzY2FsZTNkKCR7c2NhbGV9KWBcbiAgICB9O1xuICB9XG5cbiAgLyoqIENTUyBzdHlsZXMgZm9yIHRoZSB0cmFjayBmaWxsIGVsZW1lbnQuICovXG4gIGdldCBfdHJhY2tGaWxsU3R5bGVzKCk6IHsgW2tleTogc3RyaW5nXTogc3RyaW5nIH0ge1xuICAgIGNvbnN0IHBlcmNlbnQgPSB0aGlzLnBlcmNlbnQ7XG4gICAgY29uc3QgYXhpcyA9IHRoaXMudmVydGljYWwgPyAnWScgOiAnWCc7XG4gICAgY29uc3Qgc2NhbGUgPSB0aGlzLnZlcnRpY2FsID8gYDEsICR7cGVyY2VudH0sIDFgIDogYCR7cGVyY2VudH0sIDEsIDFgO1xuICAgIGNvbnN0IHNpZ24gPSB0aGlzLl9zaG91bGRJbnZlcnRNb3VzZUNvb3JkcygpID8gJycgOiAnLSc7XG5cbiAgICByZXR1cm4ge1xuICAgICAgLy8gc2NhbGUzZCBhdm9pZHMgc29tZSByZW5kZXJpbmcgaXNzdWVzIGluIENocm9tZS4gU2VlICMxMjA3MS5cbiAgICAgIHRyYW5zZm9ybTogYHRyYW5zbGF0ZSR7YXhpc30oJHtzaWdufSR7dGhpcy5fdGh1bWJHYXB9cHgpIHNjYWxlM2QoJHtzY2FsZX0pYCxcbiAgICAgIC8vIGlPUyBTYWZhcmkgaGFzIGEgYnVnIHdoZXJlIGl0IHdvbid0IHJlLXJlbmRlciBlbGVtZW50cyB3aGljaCBzdGFydCBvZiBhcyBgc2NhbGUoMClgIHVudGlsXG4gICAgICAvLyBzb21ldGhpbmcgZm9yY2VzIGEgc3R5bGUgcmVjYWxjdWxhdGlvbiBvbiBpdC4gU2luY2Ugd2UnbGwgZW5kIHVwIHdpdGggYHNjYWxlKDApYCB3aGVuXG4gICAgICAvLyB0aGUgdmFsdWUgb2YgdGhlIHNsaWRlciBpcyAwLCB3ZSBjYW4gZWFzaWx5IGdldCBpbnRvIHRoaXMgc2l0dWF0aW9uLiBXZSBmb3JjZSBhXG4gICAgICAvLyByZWNhbGN1bGF0aW9uIGJ5IGNoYW5naW5nIHRoZSBlbGVtZW50J3MgYGRpc3BsYXlgIHdoZW4gaXQgZ29lcyBmcm9tIDAgdG8gYW55IG90aGVyIHZhbHVlLlxuICAgICAgZGlzcGxheTogcGVyY2VudCA9PT0gMCA/ICdub25lJyA6ICcnXG4gICAgfTtcbiAgfVxuXG4gIC8qKiBDU1Mgc3R5bGVzIGZvciB0aGUgdGlja3MgY29udGFpbmVyIGVsZW1lbnQuICovXG4gIGdldCBfdGlja3NDb250YWluZXJTdHlsZXMoKTogeyBba2V5OiBzdHJpbmddOiBzdHJpbmcgfSB7XG4gICAgbGV0IGF4aXMgPSB0aGlzLnZlcnRpY2FsID8gJ1knIDogJ1gnO1xuICAgIC8vIEZvciBhIGhvcml6b250YWwgc2xpZGVyIGluIFJUTCBsYW5ndWFnZXMgd2UgcHVzaCB0aGUgdGlja3MgY29udGFpbmVyIG9mZiB0aGUgbGVmdCBlZGdlXG4gICAgLy8gaW5zdGVhZCBvZiB0aGUgcmlnaHQgZWRnZSB0byBhdm9pZCBjYXVzaW5nIGEgaG9yaXpvbnRhbCBzY3JvbGxiYXIgdG8gYXBwZWFyLlxuICAgIGxldCBzaWduID0gIXRoaXMudmVydGljYWwgJiYgdGhpcy5fZ2V0RGlyZWN0aW9uKCkgPT0gJ3J0bCcgPyAnJyA6ICctJztcbiAgICBsZXQgb2Zmc2V0ID0gdGhpcy5fdGlja0ludGVydmFsUGVyY2VudCAvIDIgKiAxMDA7XG4gICAgcmV0dXJuIHtcbiAgICAgICd0cmFuc2Zvcm0nOiBgdHJhbnNsYXRlJHtheGlzfSgke3NpZ259JHtvZmZzZXR9JSlgXG4gICAgfTtcbiAgfVxuXG4gIC8qKiBDU1Mgc3R5bGVzIGZvciB0aGUgdGlja3MgZWxlbWVudC4gKi9cbiAgZ2V0IF90aWNrc1N0eWxlcygpOiB7IFtrZXk6IHN0cmluZ106IHN0cmluZyB9IHtcbiAgICBsZXQgdGlja1NpemUgPSB0aGlzLl90aWNrSW50ZXJ2YWxQZXJjZW50ICogMTAwO1xuICAgIGxldCBiYWNrZ3JvdW5kU2l6ZSA9IHRoaXMudmVydGljYWwgPyBgMnB4ICR7dGlja1NpemV9JWAgOiBgJHt0aWNrU2l6ZX0lIDJweGA7XG4gICAgbGV0IGF4aXMgPSB0aGlzLnZlcnRpY2FsID8gJ1knIDogJ1gnO1xuICAgIC8vIERlcGVuZGluZyBvbiB0aGUgZGlyZWN0aW9uIHdlIHB1c2hlZCB0aGUgdGlja3MgY29udGFpbmVyLCBwdXNoIHRoZSB0aWNrcyB0aGUgb3Bwb3NpdGVcbiAgICAvLyBkaXJlY3Rpb24gdG8gcmUtY2VudGVyIHRoZW0gYnV0IGNsaXAgb2ZmIHRoZSBlbmQgZWRnZS4gSW4gUlRMIGxhbmd1YWdlcyB3ZSBuZWVkIHRvIGZsaXAgdGhlXG4gICAgLy8gdGlja3MgMTgwIGRlZ3JlZXMgc28gd2UncmUgcmVhbGx5IGN1dHRpbmcgb2ZmIHRoZSBlbmQgZWRnZSBhYmQgbm90IHRoZSBzdGFydC5cbiAgICBsZXQgc2lnbiA9ICF0aGlzLnZlcnRpY2FsICYmIHRoaXMuX2dldERpcmVjdGlvbigpID09ICdydGwnID8gJy0nIDogJyc7XG4gICAgbGV0IHJvdGF0ZSA9ICF0aGlzLnZlcnRpY2FsICYmIHRoaXMuX2dldERpcmVjdGlvbigpID09ICdydGwnID8gJyByb3RhdGUoMTgwZGVnKScgOiAnJztcbiAgICBsZXQgc3R5bGVzOiB7IFtrZXk6IHN0cmluZ106IHN0cmluZyB9ID0ge1xuICAgICAgJ2JhY2tncm91bmRTaXplJzogYmFja2dyb3VuZFNpemUsXG4gICAgICAvLyBXaXRob3V0IHRyYW5zbGF0ZVogdGlja3Mgc29tZXRpbWVzIGppdHRlciBhcyB0aGUgc2xpZGVyIG1vdmVzIG9uIENocm9tZSAmIEZpcmVmb3guXG4gICAgICAndHJhbnNmb3JtJzogYHRyYW5zbGF0ZVooMCkgdHJhbnNsYXRlJHtheGlzfSgke3NpZ259JHt0aWNrU2l6ZSAvIDJ9JSkke3JvdGF0ZX1gXG4gICAgfTtcblxuICAgIGlmICh0aGlzLl9pc01pblZhbHVlICYmIHRoaXMuX3RodW1iR2FwKSB7XG4gICAgICBsZXQgc2lkZTogc3RyaW5nO1xuXG4gICAgICBpZiAodGhpcy52ZXJ0aWNhbCkge1xuICAgICAgICBzaWRlID0gdGhpcy5faW52ZXJ0QXhpcyA/ICdCb3R0b20nIDogJ1RvcCc7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICBzaWRlID0gdGhpcy5faW52ZXJ0QXhpcyA/ICdSaWdodCcgOiAnTGVmdCc7XG4gICAgICB9XG5cbiAgICAgIHN0eWxlc1tgcGFkZGluZyR7c2lkZX1gXSA9IGAke3RoaXMuX3RodW1iR2FwfXB4YDtcbiAgICB9XG5cbiAgICByZXR1cm4gc3R5bGVzO1xuICB9XG5cbiAgZ2V0IF90aHVtYkNvbnRhaW5lclN0eWxlcygpOiB7IFtrZXk6IHN0cmluZ106IHN0cmluZyB9IHtcbiAgICBsZXQgYXhpcyA9IHRoaXMudmVydGljYWwgPyAnWScgOiAnWCc7XG4gICAgLy8gRm9yIGEgaG9yaXpvbnRhbCBzbGlkZXIgaW4gUlRMIGxhbmd1YWdlcyB3ZSBwdXNoIHRoZSB0aHVtYiBjb250YWluZXIgb2ZmIHRoZSBsZWZ0IGVkZ2VcbiAgICAvLyBpbnN0ZWFkIG9mIHRoZSByaWdodCBlZGdlIHRvIGF2b2lkIGNhdXNpbmcgYSBob3Jpem9udGFsIHNjcm9sbGJhciB0byBhcHBlYXIuXG4gICAgbGV0IGludmVydE9mZnNldCA9XG4gICAgICAgICh0aGlzLl9nZXREaXJlY3Rpb24oKSA9PSAncnRsJyAmJiAhdGhpcy52ZXJ0aWNhbCkgPyAhdGhpcy5faW52ZXJ0QXhpcyA6IHRoaXMuX2ludmVydEF4aXM7XG4gICAgbGV0IG9mZnNldCA9IChpbnZlcnRPZmZzZXQgPyB0aGlzLnBlcmNlbnQgOiAxIC0gdGhpcy5wZXJjZW50KSAqIDEwMDtcbiAgICByZXR1cm4ge1xuICAgICAgJ3RyYW5zZm9ybSc6IGB0cmFuc2xhdGUke2F4aXN9KC0ke29mZnNldH0lKWBcbiAgICB9O1xuICB9XG5cbiAgLyoqIFRoZSBzaXplIG9mIGEgdGljayBpbnRlcnZhbCBhcyBhIHBlcmNlbnRhZ2Ugb2YgdGhlIHNpemUgb2YgdGhlIHRyYWNrLiAqL1xuICBwcml2YXRlIF90aWNrSW50ZXJ2YWxQZXJjZW50OiBudW1iZXIgPSAwO1xuXG4gIC8qKiBUaGUgZGltZW5zaW9ucyBvZiB0aGUgc2xpZGVyLiAqL1xuICBwcml2YXRlIF9zbGlkZXJEaW1lbnNpb25zOiBDbGllbnRSZWN0IHwgbnVsbCA9IG51bGw7XG5cbiAgcHJpdmF0ZSBfY29udHJvbFZhbHVlQWNjZXNzb3JDaGFuZ2VGbjogKHZhbHVlOiBhbnkpID0+IHZvaWQgPSAoKSA9PiB7fTtcblxuICAvKiogRGVjaW1hbCBwbGFjZXMgdG8gcm91bmQgdG8sIGJhc2VkIG9uIHRoZSBzdGVwIGFtb3VudC4gKi9cbiAgcHJpdmF0ZSBfcm91bmRUb0RlY2ltYWw6IG51bWJlcjtcblxuICAvKiogU3Vic2NyaXB0aW9uIHRvIHRoZSBEaXJlY3Rpb25hbGl0eSBjaGFuZ2UgRXZlbnRFbWl0dGVyLiAqL1xuICBwcml2YXRlIF9kaXJDaGFuZ2VTdWJzY3JpcHRpb24gPSBTdWJzY3JpcHRpb24uRU1QVFk7XG5cbiAgLyoqIFRoZSB2YWx1ZSBvZiB0aGUgc2xpZGVyIHdoZW4gdGhlIHNsaWRlIHN0YXJ0IGV2ZW50IGZpcmVzLiAqL1xuICBwcml2YXRlIF92YWx1ZU9uU2xpZGVTdGFydDogbnVtYmVyIHwgbnVsbDtcblxuICAvKiogUG9zaXRpb24gb2YgdGhlIHBvaW50ZXIgd2hlbiB0aGUgZHJhZ2dpbmcgc3RhcnRlZC4gKi9cbiAgcHJpdmF0ZSBfcG9pbnRlclBvc2l0aW9uT25TdGFydDoge3g6IG51bWJlciwgeTogbnVtYmVyfSB8IG51bGw7XG5cbiAgLyoqIFJlZmVyZW5jZSB0byB0aGUgaW5uZXIgc2xpZGVyIHdyYXBwZXIgZWxlbWVudC4gKi9cbiAgQFZpZXdDaGlsZCgnc2xpZGVyV3JhcHBlcicpIHByaXZhdGUgX3NsaWRlcldyYXBwZXI6IEVsZW1lbnRSZWY7XG5cbiAgLyoqXG4gICAqIFdoZXRoZXIgbW91c2UgZXZlbnRzIHNob3VsZCBiZSBjb252ZXJ0ZWQgdG8gYSBzbGlkZXIgcG9zaXRpb24gYnkgY2FsY3VsYXRpbmcgdGhlaXIgZGlzdGFuY2VcbiAgICogZnJvbSB0aGUgcmlnaHQgb3IgYm90dG9tIGVkZ2Ugb2YgdGhlIHNsaWRlciBhcyBvcHBvc2VkIHRvIHRoZSB0b3Agb3IgbGVmdC5cbiAgICovXG4gIF9zaG91bGRJbnZlcnRNb3VzZUNvb3JkcygpIHtcbiAgICByZXR1cm4gKHRoaXMuX2dldERpcmVjdGlvbigpID09ICdydGwnICYmICF0aGlzLnZlcnRpY2FsKSA/ICF0aGlzLl9pbnZlcnRBeGlzIDogdGhpcy5faW52ZXJ0QXhpcztcbiAgfVxuXG4gIC8qKiBUaGUgbGFuZ3VhZ2UgZGlyZWN0aW9uIGZvciB0aGlzIHNsaWRlciBlbGVtZW50LiAqL1xuICBwcml2YXRlIF9nZXREaXJlY3Rpb24oKSB7XG4gICAgcmV0dXJuICh0aGlzLl9kaXIgJiYgdGhpcy5fZGlyLnZhbHVlID09ICdydGwnKSA/ICdydGwnIDogJ2x0cic7XG4gIH1cblxuICAvKiogS2VlcHMgdHJhY2sgb2YgdGhlIGxhc3QgcG9pbnRlciBldmVudCB0aGF0IHdhcyBjYXB0dXJlZCBieSB0aGUgc2xpZGVyLiAqL1xuICBwcml2YXRlIF9sYXN0UG9pbnRlckV2ZW50OiBNb3VzZUV2ZW50IHwgVG91Y2hFdmVudCB8IG51bGw7XG5cbiAgLyoqIFVzZWQgdG8gc3Vic2NyaWJlIHRvIGdsb2JhbCBtb3ZlIGFuZCBlbmQgZXZlbnRzICovXG4gIHByb3RlY3RlZCBfZG9jdW1lbnQ6IERvY3VtZW50O1xuXG4gIGNvbnN0cnVjdG9yKGVsZW1lbnRSZWY6IEVsZW1lbnRSZWYsXG4gICAgICAgICAgICAgIHByaXZhdGUgX2ZvY3VzTW9uaXRvcjogRm9jdXNNb25pdG9yLFxuICAgICAgICAgICAgICBwcml2YXRlIF9jaGFuZ2VEZXRlY3RvclJlZjogQ2hhbmdlRGV0ZWN0b3JSZWYsXG4gICAgICAgICAgICAgIEBPcHRpb25hbCgpIHByaXZhdGUgX2RpcjogRGlyZWN0aW9uYWxpdHksXG4gICAgICAgICAgICAgIEBBdHRyaWJ1dGUoJ3RhYmluZGV4JykgdGFiSW5kZXg6IHN0cmluZyxcbiAgICAgICAgICAgICAgcHJpdmF0ZSBfbmdab25lOiBOZ1pvbmUsXG4gICAgICAgICAgICAgIEBJbmplY3QoRE9DVU1FTlQpIF9kb2N1bWVudDogYW55LFxuICAgICAgICAgICAgICBAT3B0aW9uYWwoKSBASW5qZWN0KEFOSU1BVElPTl9NT0RVTEVfVFlQRSkgcHVibGljIF9hbmltYXRpb25Nb2RlPzogc3RyaW5nKSB7XG4gICAgc3VwZXIoZWxlbWVudFJlZik7XG4gICAgdGhpcy5fZG9jdW1lbnQgPSBfZG9jdW1lbnQ7XG4gICAgdGhpcy50YWJJbmRleCA9IHBhcnNlSW50KHRhYkluZGV4KSB8fCAwO1xuXG4gICAgX25nWm9uZS5ydW5PdXRzaWRlQW5ndWxhcigoKSA9PiB7XG4gICAgICBjb25zdCBlbGVtZW50ID0gZWxlbWVudFJlZi5uYXRpdmVFbGVtZW50O1xuICAgICAgZWxlbWVudC5hZGRFdmVudExpc3RlbmVyKCdtb3VzZWRvd24nLCB0aGlzLl9wb2ludGVyRG93biwgYWN0aXZlRXZlbnRPcHRpb25zKTtcbiAgICAgIGVsZW1lbnQuYWRkRXZlbnRMaXN0ZW5lcigndG91Y2hzdGFydCcsIHRoaXMuX3BvaW50ZXJEb3duLCBhY3RpdmVFdmVudE9wdGlvbnMpO1xuICAgIH0pO1xuICB9XG5cbiAgbmdBZnRlclZpZXdJbml0KCkge1xuICAgIHRoaXMuX2ZvY3VzTW9uaXRvclxuICAgICAgICAubW9uaXRvcih0aGlzLl9lbGVtZW50UmVmLCB0cnVlKVxuICAgICAgICAuc3Vic2NyaWJlKChvcmlnaW46IEZvY3VzT3JpZ2luKSA9PiB7XG4gICAgICAgICAgdGhpcy5faXNBY3RpdmUgPSAhIW9yaWdpbiAmJiBvcmlnaW4gIT09ICdrZXlib2FyZCc7XG4gICAgICAgICAgdGhpcy5fY2hhbmdlRGV0ZWN0b3JSZWYuZGV0ZWN0Q2hhbmdlcygpO1xuICAgICAgICB9KTtcbiAgICBpZiAodGhpcy5fZGlyKSB7XG4gICAgICB0aGlzLl9kaXJDaGFuZ2VTdWJzY3JpcHRpb24gPSB0aGlzLl9kaXIuY2hhbmdlLnN1YnNjcmliZSgoKSA9PiB7XG4gICAgICAgIHRoaXMuX2NoYW5nZURldGVjdG9yUmVmLm1hcmtGb3JDaGVjaygpO1xuICAgICAgfSk7XG4gICAgfVxuICB9XG5cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgY29uc3QgZWxlbWVudCA9IHRoaXMuX2VsZW1lbnRSZWYubmF0aXZlRWxlbWVudDtcbiAgICBlbGVtZW50LnJlbW92ZUV2ZW50TGlzdGVuZXIoJ21vdXNlZG93bicsIHRoaXMuX3BvaW50ZXJEb3duLCBhY3RpdmVFdmVudE9wdGlvbnMpO1xuICAgIGVsZW1lbnQucmVtb3ZlRXZlbnRMaXN0ZW5lcigndG91Y2hzdGFydCcsIHRoaXMuX3BvaW50ZXJEb3duLCBhY3RpdmVFdmVudE9wdGlvbnMpO1xuICAgIHRoaXMuX2xhc3RQb2ludGVyRXZlbnQgPSBudWxsO1xuICAgIHRoaXMuX3JlbW92ZUdsb2JhbEV2ZW50cygpO1xuICAgIHRoaXMuX2ZvY3VzTW9uaXRvci5zdG9wTW9uaXRvcmluZyh0aGlzLl9lbGVtZW50UmVmKTtcbiAgICB0aGlzLl9kaXJDaGFuZ2VTdWJzY3JpcHRpb24udW5zdWJzY3JpYmUoKTtcbiAgfVxuXG4gIF9vbk1vdXNlZW50ZXIoKSB7XG4gICAgaWYgKHRoaXMuZGlzYWJsZWQpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICAvLyBXZSBzYXZlIHRoZSBkaW1lbnNpb25zIG9mIHRoZSBzbGlkZXIgaGVyZSBzbyB3ZSBjYW4gdXNlIHRoZW0gdG8gdXBkYXRlIHRoZSBzcGFjaW5nIG9mIHRoZVxuICAgIC8vIHRpY2tzIGFuZCBkZXRlcm1pbmUgd2hlcmUgb24gdGhlIHNsaWRlciBjbGljayBhbmQgc2xpZGUgZXZlbnRzIGhhcHBlbi5cbiAgICB0aGlzLl9zbGlkZXJEaW1lbnNpb25zID0gdGhpcy5fZ2V0U2xpZGVyRGltZW5zaW9ucygpO1xuICAgIHRoaXMuX3VwZGF0ZVRpY2tJbnRlcnZhbFBlcmNlbnQoKTtcbiAgfVxuXG4gIF9vbkZvY3VzKCkge1xuICAgIC8vIFdlIHNhdmUgdGhlIGRpbWVuc2lvbnMgb2YgdGhlIHNsaWRlciBoZXJlIHNvIHdlIGNhbiB1c2UgdGhlbSB0byB1cGRhdGUgdGhlIHNwYWNpbmcgb2YgdGhlXG4gICAgLy8gdGlja3MgYW5kIGRldGVybWluZSB3aGVyZSBvbiB0aGUgc2xpZGVyIGNsaWNrIGFuZCBzbGlkZSBldmVudHMgaGFwcGVuLlxuICAgIHRoaXMuX3NsaWRlckRpbWVuc2lvbnMgPSB0aGlzLl9nZXRTbGlkZXJEaW1lbnNpb25zKCk7XG4gICAgdGhpcy5fdXBkYXRlVGlja0ludGVydmFsUGVyY2VudCgpO1xuICB9XG5cbiAgX29uQmx1cigpIHtcbiAgICB0aGlzLm9uVG91Y2hlZCgpO1xuICB9XG5cbiAgX29uS2V5ZG93bihldmVudDogS2V5Ym9hcmRFdmVudCkge1xuICAgIGlmICh0aGlzLmRpc2FibGVkIHx8IGhhc01vZGlmaWVyS2V5KGV2ZW50KSkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGNvbnN0IG9sZFZhbHVlID0gdGhpcy52YWx1ZTtcblxuICAgIHN3aXRjaCAoZXZlbnQua2V5Q29kZSkge1xuICAgICAgY2FzZSBQQUdFX1VQOlxuICAgICAgICB0aGlzLl9pbmNyZW1lbnQoMTApO1xuICAgICAgICBicmVhaztcbiAgICAgIGNhc2UgUEFHRV9ET1dOOlxuICAgICAgICB0aGlzLl9pbmNyZW1lbnQoLTEwKTtcbiAgICAgICAgYnJlYWs7XG4gICAgICBjYXNlIEVORDpcbiAgICAgICAgdGhpcy52YWx1ZSA9IHRoaXMubWF4O1xuICAgICAgICBicmVhaztcbiAgICAgIGNhc2UgSE9NRTpcbiAgICAgICAgdGhpcy52YWx1ZSA9IHRoaXMubWluO1xuICAgICAgICBicmVhaztcbiAgICAgIGNhc2UgTEVGVF9BUlJPVzpcbiAgICAgICAgLy8gTk9URTogRm9yIGEgc2lnaHRlZCB1c2VyIGl0IHdvdWxkIG1ha2UgbW9yZSBzZW5zZSB0aGF0IHdoZW4gdGhleSBwcmVzcyBhbiBhcnJvdyBrZXkgb24gYW5cbiAgICAgICAgLy8gaW52ZXJ0ZWQgc2xpZGVyIHRoZSB0aHVtYiBtb3ZlcyBpbiB0aGF0IGRpcmVjdGlvbi4gSG93ZXZlciBmb3IgYSBibGluZCB1c2VyLCBub3RoaW5nXG4gICAgICAgIC8vIGFib3V0IHRoZSBzbGlkZXIgaW5kaWNhdGVzIHRoYXQgaXQgaXMgaW52ZXJ0ZWQuIFRoZXkgd2lsbCBleHBlY3QgbGVmdCB0byBiZSBkZWNyZW1lbnQsXG4gICAgICAgIC8vIHJlZ2FyZGxlc3Mgb2YgaG93IGl0IGFwcGVhcnMgb24gdGhlIHNjcmVlbi4gRm9yIHNwZWFrZXJzIG9mUlRMIGxhbmd1YWdlcywgdGhleSBwcm9iYWJseVxuICAgICAgICAvLyBleHBlY3QgbGVmdCB0byBtZWFuIGluY3JlbWVudC4gVGhlcmVmb3JlIHdlIGZsaXAgdGhlIG1lYW5pbmcgb2YgdGhlIHNpZGUgYXJyb3cga2V5cyBmb3JcbiAgICAgICAgLy8gUlRMLiBGb3IgaW52ZXJ0ZWQgc2xpZGVycyB3ZSBwcmVmZXIgYSBnb29kIGExMXkgZXhwZXJpZW5jZSB0byBoYXZpbmcgaXQgXCJsb29rIHJpZ2h0XCIgZm9yXG4gICAgICAgIC8vIHNpZ2h0ZWQgdXNlcnMsIHRoZXJlZm9yZSB3ZSBkbyBub3Qgc3dhcCB0aGUgbWVhbmluZy5cbiAgICAgICAgdGhpcy5faW5jcmVtZW50KHRoaXMuX2dldERpcmVjdGlvbigpID09ICdydGwnID8gMSA6IC0xKTtcbiAgICAgICAgYnJlYWs7XG4gICAgICBjYXNlIFVQX0FSUk9XOlxuICAgICAgICB0aGlzLl9pbmNyZW1lbnQoMSk7XG4gICAgICAgIGJyZWFrO1xuICAgICAgY2FzZSBSSUdIVF9BUlJPVzpcbiAgICAgICAgLy8gU2VlIGNvbW1lbnQgb24gTEVGVF9BUlJPVyBhYm91dCB0aGUgY29uZGl0aW9ucyB1bmRlciB3aGljaCB3ZSBmbGlwIHRoZSBtZWFuaW5nLlxuICAgICAgICB0aGlzLl9pbmNyZW1lbnQodGhpcy5fZ2V0RGlyZWN0aW9uKCkgPT0gJ3J0bCcgPyAtMSA6IDEpO1xuICAgICAgICBicmVhaztcbiAgICAgIGNhc2UgRE9XTl9BUlJPVzpcbiAgICAgICAgdGhpcy5faW5jcmVtZW50KC0xKTtcbiAgICAgICAgYnJlYWs7XG4gICAgICBkZWZhdWx0OlxuICAgICAgICAvLyBSZXR1cm4gaWYgdGhlIGtleSBpcyBub3Qgb25lIHRoYXQgd2UgZXhwbGljaXRseSBoYW5kbGUgdG8gYXZvaWQgY2FsbGluZyBwcmV2ZW50RGVmYXVsdCBvblxuICAgICAgICAvLyBpdC5cbiAgICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGlmIChvbGRWYWx1ZSAhPSB0aGlzLnZhbHVlKSB7XG4gICAgICB0aGlzLl9lbWl0SW5wdXRFdmVudCgpO1xuICAgICAgdGhpcy5fZW1pdENoYW5nZUV2ZW50KCk7XG4gICAgfVxuXG4gICAgdGhpcy5faXNTbGlkaW5nID0gdHJ1ZTtcbiAgICBldmVudC5wcmV2ZW50RGVmYXVsdCgpO1xuICB9XG5cbiAgX29uS2V5dXAoKSB7XG4gICAgdGhpcy5faXNTbGlkaW5nID0gZmFsc2U7XG4gIH1cblxuICAvKiogQ2FsbGVkIHdoZW4gdGhlIHVzZXIgaGFzIHB1dCB0aGVpciBwb2ludGVyIGRvd24gb24gdGhlIHNsaWRlci4gKi9cbiAgcHJpdmF0ZSBfcG9pbnRlckRvd24gPSAoZXZlbnQ6IFRvdWNoRXZlbnQgfCBNb3VzZUV2ZW50KSA9PiB7XG4gICAgLy8gRG9uJ3QgZG8gYW55dGhpbmcgaWYgdGhlIHNsaWRlciBpcyBkaXNhYmxlZCBvciB0aGVcbiAgICAvLyB1c2VyIGlzIHVzaW5nIGFueXRoaW5nIG90aGVyIHRoYW4gdGhlIG1haW4gbW91c2UgYnV0dG9uLlxuICAgIGlmICh0aGlzLmRpc2FibGVkIHx8IHRoaXMuX2lzU2xpZGluZyB8fCAoIWlzVG91Y2hFdmVudChldmVudCkgJiYgZXZlbnQuYnV0dG9uICE9PSAwKSkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIHRoaXMuX25nWm9uZS5ydW4oKCkgPT4ge1xuICAgICAgY29uc3Qgb2xkVmFsdWUgPSB0aGlzLnZhbHVlO1xuICAgICAgY29uc3QgcG9pbnRlclBvc2l0aW9uID0gZ2V0UG9pbnRlclBvc2l0aW9uT25QYWdlKGV2ZW50KTtcbiAgICAgIHRoaXMuX2lzU2xpZGluZyA9IHRydWU7XG4gICAgICB0aGlzLl9sYXN0UG9pbnRlckV2ZW50ID0gZXZlbnQ7XG4gICAgICBldmVudC5wcmV2ZW50RGVmYXVsdCgpO1xuICAgICAgdGhpcy5fZm9jdXNIb3N0RWxlbWVudCgpO1xuICAgICAgdGhpcy5fb25Nb3VzZWVudGVyKCk7IC8vIFNpbXVsYXRlIG1vdXNlZW50ZXIgaW4gY2FzZSB0aGlzIGlzIGEgbW9iaWxlIGRldmljZS5cbiAgICAgIHRoaXMuX2JpbmRHbG9iYWxFdmVudHMoZXZlbnQpO1xuICAgICAgdGhpcy5fZm9jdXNIb3N0RWxlbWVudCgpO1xuICAgICAgdGhpcy5fdXBkYXRlVmFsdWVGcm9tUG9zaXRpb24ocG9pbnRlclBvc2l0aW9uKTtcbiAgICAgIHRoaXMuX3ZhbHVlT25TbGlkZVN0YXJ0ID0gdGhpcy52YWx1ZTtcbiAgICAgIHRoaXMuX3BvaW50ZXJQb3NpdGlvbk9uU3RhcnQgPSBwb2ludGVyUG9zaXRpb247XG5cbiAgICAgIC8vIEVtaXQgYSBjaGFuZ2UgYW5kIGlucHV0IGV2ZW50IGlmIHRoZSB2YWx1ZSBjaGFuZ2VkLlxuICAgICAgaWYgKG9sZFZhbHVlICE9IHRoaXMudmFsdWUpIHtcbiAgICAgICAgdGhpcy5fZW1pdElucHV0RXZlbnQoKTtcbiAgICAgICAgdGhpcy5fZW1pdENoYW5nZUV2ZW50KCk7XG4gICAgICB9XG4gICAgfSk7XG4gIH1cblxuICAvKipcbiAgICogQ2FsbGVkIHdoZW4gdGhlIHVzZXIgaGFzIG1vdmVkIHRoZWlyIHBvaW50ZXIgYWZ0ZXJcbiAgICogc3RhcnRpbmcgdG8gZHJhZy4gQm91bmQgb24gdGhlIGRvY3VtZW50IGxldmVsLlxuICAgKi9cbiAgcHJpdmF0ZSBfcG9pbnRlck1vdmUgPSAoZXZlbnQ6IFRvdWNoRXZlbnQgfCBNb3VzZUV2ZW50KSA9PiB7XG4gICAgaWYgKHRoaXMuX2lzU2xpZGluZykge1xuICAgICAgLy8gUHJldmVudCB0aGUgc2xpZGUgZnJvbSBzZWxlY3RpbmcgYW55dGhpbmcgZWxzZS5cbiAgICAgIGV2ZW50LnByZXZlbnREZWZhdWx0KCk7XG4gICAgICBjb25zdCBvbGRWYWx1ZSA9IHRoaXMudmFsdWU7XG4gICAgICB0aGlzLl9sYXN0UG9pbnRlckV2ZW50ID0gZXZlbnQ7XG4gICAgICB0aGlzLl91cGRhdGVWYWx1ZUZyb21Qb3NpdGlvbihnZXRQb2ludGVyUG9zaXRpb25PblBhZ2UoZXZlbnQpKTtcblxuICAgICAgLy8gTmF0aXZlIHJhbmdlIGVsZW1lbnRzIGFsd2F5cyBlbWl0IGBpbnB1dGAgZXZlbnRzIHdoZW4gdGhlIHZhbHVlIGNoYW5nZWQgd2hpbGUgc2xpZGluZy5cbiAgICAgIGlmIChvbGRWYWx1ZSAhPSB0aGlzLnZhbHVlKSB7XG4gICAgICAgIHRoaXMuX2VtaXRJbnB1dEV2ZW50KCk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgLyoqIENhbGxlZCB3aGVuIHRoZSB1c2VyIGhhcyBsaWZ0ZWQgdGhlaXIgcG9pbnRlci4gQm91bmQgb24gdGhlIGRvY3VtZW50IGxldmVsLiAqL1xuICBwcml2YXRlIF9wb2ludGVyVXAgPSAoZXZlbnQ6IFRvdWNoRXZlbnQgfCBNb3VzZUV2ZW50KSA9PiB7XG4gICAgaWYgKHRoaXMuX2lzU2xpZGluZykge1xuICAgICAgY29uc3QgcG9pbnRlclBvc2l0aW9uT25TdGFydCA9IHRoaXMuX3BvaW50ZXJQb3NpdGlvbk9uU3RhcnQ7XG4gICAgICBjb25zdCBjdXJyZW50UG9pbnRlclBvc2l0aW9uID0gZ2V0UG9pbnRlclBvc2l0aW9uT25QYWdlKGV2ZW50KTtcblxuICAgICAgZXZlbnQucHJldmVudERlZmF1bHQoKTtcbiAgICAgIHRoaXMuX3JlbW92ZUdsb2JhbEV2ZW50cygpO1xuICAgICAgdGhpcy5fdmFsdWVPblNsaWRlU3RhcnQgPSB0aGlzLl9wb2ludGVyUG9zaXRpb25PblN0YXJ0ID0gdGhpcy5fbGFzdFBvaW50ZXJFdmVudCA9IG51bGw7XG4gICAgICB0aGlzLl9pc1NsaWRpbmcgPSBmYWxzZTtcblxuICAgICAgaWYgKHRoaXMuX3ZhbHVlT25TbGlkZVN0YXJ0ICE9IHRoaXMudmFsdWUgJiYgIXRoaXMuZGlzYWJsZWQgJiZcbiAgICAgICAgICBwb2ludGVyUG9zaXRpb25PblN0YXJ0ICYmIChwb2ludGVyUG9zaXRpb25PblN0YXJ0LnggIT09IGN1cnJlbnRQb2ludGVyUG9zaXRpb24ueCB8fFxuICAgICAgICAgIHBvaW50ZXJQb3NpdGlvbk9uU3RhcnQueSAhPT0gY3VycmVudFBvaW50ZXJQb3NpdGlvbi55KSkge1xuICAgICAgICB0aGlzLl9lbWl0Q2hhbmdlRXZlbnQoKTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICAvKiogQ2FsbGVkIHdoZW4gdGhlIHdpbmRvdyBoYXMgbG9zdCBmb2N1cy4gKi9cbiAgcHJpdmF0ZSBfd2luZG93Qmx1ciA9ICgpID0+IHtcbiAgICAvLyBJZiB0aGUgd2luZG93IGlzIGJsdXJyZWQgd2hpbGUgZHJhZ2dpbmcgd2UgbmVlZCB0byBzdG9wIGRyYWdnaW5nIGJlY2F1c2UgdGhlXG4gICAgLy8gYnJvd3NlciB3b24ndCBkaXNwYXRjaCB0aGUgYG1vdXNldXBgIGFuZCBgdG91Y2hlbmRgIGV2ZW50cyBhbnltb3JlLlxuICAgIGlmICh0aGlzLl9sYXN0UG9pbnRlckV2ZW50KSB7XG4gICAgICB0aGlzLl9wb2ludGVyVXAodGhpcy5fbGFzdFBvaW50ZXJFdmVudCk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFVzZSBkZWZhdWx0VmlldyBvZiBpbmplY3RlZCBkb2N1bWVudCBpZiBhdmFpbGFibGUgb3IgZmFsbGJhY2sgdG8gZ2xvYmFsIHdpbmRvdyByZWZlcmVuY2UgKi9cbiAgcHJpdmF0ZSBfZ2V0V2luZG93KCk6IFdpbmRvdyB7XG4gICAgcmV0dXJuIHRoaXMuX2RvY3VtZW50LmRlZmF1bHRWaWV3IHx8IHdpbmRvdztcbiAgfVxuXG4gIC8qKlxuICAgKiBCaW5kcyBvdXIgZ2xvYmFsIG1vdmUgYW5kIGVuZCBldmVudHMuIFRoZXkncmUgYm91bmQgYXQgdGhlIGRvY3VtZW50IGxldmVsIGFuZCBvbmx5IHdoaWxlXG4gICAqIGRyYWdnaW5nIHNvIHRoYXQgdGhlIHVzZXIgZG9lc24ndCBoYXZlIHRvIGtlZXAgdGhlaXIgcG9pbnRlciBleGFjdGx5IG92ZXIgdGhlIHNsaWRlclxuICAgKiBhcyB0aGV5J3JlIHN3aXBpbmcgYWNyb3NzIHRoZSBzY3JlZW4uXG4gICAqL1xuICBwcml2YXRlIF9iaW5kR2xvYmFsRXZlbnRzKHRyaWdnZXJFdmVudDogVG91Y2hFdmVudCB8IE1vdXNlRXZlbnQpIHtcbiAgICAvLyBOb3RlIHRoYXQgd2UgYmluZCB0aGUgZXZlbnRzIHRvIHRoZSBgZG9jdW1lbnRgLCBiZWNhdXNlIGl0IGFsbG93cyB1cyB0byBjYXB0dXJlXG4gICAgLy8gZHJhZyBjYW5jZWwgZXZlbnRzIHdoZXJlIHRoZSB1c2VyJ3MgcG9pbnRlciBpcyBvdXRzaWRlIHRoZSBicm93c2VyIHdpbmRvdy5cbiAgICBjb25zdCBkb2N1bWVudCA9IHRoaXMuX2RvY3VtZW50O1xuICAgIGNvbnN0IGlzVG91Y2ggPSBpc1RvdWNoRXZlbnQodHJpZ2dlckV2ZW50KTtcbiAgICBjb25zdCBtb3ZlRXZlbnROYW1lID0gaXNUb3VjaCA/ICd0b3VjaG1vdmUnIDogJ21vdXNlbW92ZSc7XG4gICAgY29uc3QgZW5kRXZlbnROYW1lID0gaXNUb3VjaCA/ICd0b3VjaGVuZCcgOiAnbW91c2V1cCc7XG4gICAgZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcihtb3ZlRXZlbnROYW1lLCB0aGlzLl9wb2ludGVyTW92ZSwgYWN0aXZlRXZlbnRPcHRpb25zKTtcbiAgICBkb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKGVuZEV2ZW50TmFtZSwgdGhpcy5fcG9pbnRlclVwLCBhY3RpdmVFdmVudE9wdGlvbnMpO1xuXG4gICAgaWYgKGlzVG91Y2gpIHtcbiAgICAgIGRvY3VtZW50LmFkZEV2ZW50TGlzdGVuZXIoJ3RvdWNoY2FuY2VsJywgdGhpcy5fcG9pbnRlclVwLCBhY3RpdmVFdmVudE9wdGlvbnMpO1xuICAgIH1cblxuICAgIGNvbnN0IHdpbmRvdyA9IHRoaXMuX2dldFdpbmRvdygpO1xuXG4gICAgaWYgKHR5cGVvZiB3aW5kb3cgIT09ICd1bmRlZmluZWQnICYmIHdpbmRvdykge1xuICAgICAgd2luZG93LmFkZEV2ZW50TGlzdGVuZXIoJ2JsdXInLCB0aGlzLl93aW5kb3dCbHVyKTtcbiAgICB9XG4gIH1cblxuICAvKiogUmVtb3ZlcyBhbnkgZ2xvYmFsIGV2ZW50IGxpc3RlbmVycyB0aGF0IHdlIG1heSBoYXZlIGFkZGVkLiAqL1xuICBwcml2YXRlIF9yZW1vdmVHbG9iYWxFdmVudHMoKSB7XG4gICAgY29uc3QgZG9jdW1lbnQgPSB0aGlzLl9kb2N1bWVudDtcbiAgICBkb2N1bWVudC5yZW1vdmVFdmVudExpc3RlbmVyKCdtb3VzZW1vdmUnLCB0aGlzLl9wb2ludGVyTW92ZSwgYWN0aXZlRXZlbnRPcHRpb25zKTtcbiAgICBkb2N1bWVudC5yZW1vdmVFdmVudExpc3RlbmVyKCdtb3VzZXVwJywgdGhpcy5fcG9pbnRlclVwLCBhY3RpdmVFdmVudE9wdGlvbnMpO1xuICAgIGRvY3VtZW50LnJlbW92ZUV2ZW50TGlzdGVuZXIoJ3RvdWNobW92ZScsIHRoaXMuX3BvaW50ZXJNb3ZlLCBhY3RpdmVFdmVudE9wdGlvbnMpO1xuICAgIGRvY3VtZW50LnJlbW92ZUV2ZW50TGlzdGVuZXIoJ3RvdWNoZW5kJywgdGhpcy5fcG9pbnRlclVwLCBhY3RpdmVFdmVudE9wdGlvbnMpO1xuICAgIGRvY3VtZW50LnJlbW92ZUV2ZW50TGlzdGVuZXIoJ3RvdWNoY2FuY2VsJywgdGhpcy5fcG9pbnRlclVwLCBhY3RpdmVFdmVudE9wdGlvbnMpO1xuXG4gICAgY29uc3Qgd2luZG93ID0gdGhpcy5fZ2V0V2luZG93KCk7XG5cbiAgICBpZiAodHlwZW9mIHdpbmRvdyAhPT0gJ3VuZGVmaW5lZCcgJiYgd2luZG93KSB7XG4gICAgICB3aW5kb3cucmVtb3ZlRXZlbnRMaXN0ZW5lcignYmx1cicsIHRoaXMuX3dpbmRvd0JsdXIpO1xuICAgIH1cbiAgfVxuXG4gIC8qKiBJbmNyZW1lbnRzIHRoZSBzbGlkZXIgYnkgdGhlIGdpdmVuIG51bWJlciBvZiBzdGVwcyAobmVnYXRpdmUgbnVtYmVyIGRlY3JlbWVudHMpLiAqL1xuICBwcml2YXRlIF9pbmNyZW1lbnQobnVtU3RlcHM6IG51bWJlcikge1xuICAgIHRoaXMudmFsdWUgPSB0aGlzLl9jbGFtcCgodGhpcy52YWx1ZSB8fCAwKSArIHRoaXMuc3RlcCAqIG51bVN0ZXBzLCB0aGlzLm1pbiwgdGhpcy5tYXgpO1xuICB9XG5cbiAgLyoqIENhbGN1bGF0ZSB0aGUgbmV3IHZhbHVlIGZyb20gdGhlIG5ldyBwaHlzaWNhbCBsb2NhdGlvbi4gVGhlIHZhbHVlIHdpbGwgYWx3YXlzIGJlIHNuYXBwZWQuICovXG4gIHByaXZhdGUgX3VwZGF0ZVZhbHVlRnJvbVBvc2l0aW9uKHBvczoge3g6IG51bWJlciwgeTogbnVtYmVyfSkge1xuICAgIGlmICghdGhpcy5fc2xpZGVyRGltZW5zaW9ucykge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGxldCBvZmZzZXQgPSB0aGlzLnZlcnRpY2FsID8gdGhpcy5fc2xpZGVyRGltZW5zaW9ucy50b3AgOiB0aGlzLl9zbGlkZXJEaW1lbnNpb25zLmxlZnQ7XG4gICAgbGV0IHNpemUgPSB0aGlzLnZlcnRpY2FsID8gdGhpcy5fc2xpZGVyRGltZW5zaW9ucy5oZWlnaHQgOiB0aGlzLl9zbGlkZXJEaW1lbnNpb25zLndpZHRoO1xuICAgIGxldCBwb3NDb21wb25lbnQgPSB0aGlzLnZlcnRpY2FsID8gcG9zLnkgOiBwb3MueDtcblxuICAgIC8vIFRoZSBleGFjdCB2YWx1ZSBpcyBjYWxjdWxhdGVkIGZyb20gdGhlIGV2ZW50IGFuZCB1c2VkIHRvIGZpbmQgdGhlIGNsb3Nlc3Qgc25hcCB2YWx1ZS5cbiAgICBsZXQgcGVyY2VudCA9IHRoaXMuX2NsYW1wKChwb3NDb21wb25lbnQgLSBvZmZzZXQpIC8gc2l6ZSk7XG5cbiAgICBpZiAodGhpcy5fc2hvdWxkSW52ZXJ0TW91c2VDb29yZHMoKSkge1xuICAgICAgcGVyY2VudCA9IDEgLSBwZXJjZW50O1xuICAgIH1cblxuICAgIC8vIFNpbmNlIHRoZSBzdGVwcyBtYXkgbm90IGRpdmlkZSBjbGVhbmx5IGludG8gdGhlIG1heCB2YWx1ZSwgaWYgdGhlIHVzZXJcbiAgICAvLyBzbGlkIHRvIDAgb3IgMTAwIHBlcmNlbnQsIHdlIGp1bXAgdG8gdGhlIG1pbi9tYXggdmFsdWUuIFRoaXMgYXBwcm9hY2hcbiAgICAvLyBpcyBzbGlnaHRseSBtb3JlIGludHVpdGl2ZSB0aGFuIHVzaW5nIGBNYXRoLmNlaWxgIGJlbG93LCBiZWNhdXNlIGl0XG4gICAgLy8gZm9sbG93cyB0aGUgdXNlcidzIHBvaW50ZXIgY2xvc2VyLlxuICAgIGlmIChwZXJjZW50ID09PSAwKSB7XG4gICAgICB0aGlzLnZhbHVlID0gdGhpcy5taW47XG4gICAgfSBlbHNlIGlmIChwZXJjZW50ID09PSAxKSB7XG4gICAgICB0aGlzLnZhbHVlID0gdGhpcy5tYXg7XG4gICAgfSBlbHNlIHtcbiAgICAgIGNvbnN0IGV4YWN0VmFsdWUgPSB0aGlzLl9jYWxjdWxhdGVWYWx1ZShwZXJjZW50KTtcblxuICAgICAgLy8gVGhpcyBjYWxjdWxhdGlvbiBmaW5kcyB0aGUgY2xvc2VzdCBzdGVwIGJ5IGZpbmRpbmcgdGhlIGNsb3Nlc3RcbiAgICAgIC8vIHdob2xlIG51bWJlciBkaXZpc2libGUgYnkgdGhlIHN0ZXAgcmVsYXRpdmUgdG8gdGhlIG1pbi5cbiAgICAgIGNvbnN0IGNsb3Nlc3RWYWx1ZSA9IE1hdGgucm91bmQoKGV4YWN0VmFsdWUgLSB0aGlzLm1pbikgLyB0aGlzLnN0ZXApICogdGhpcy5zdGVwICsgdGhpcy5taW47XG5cbiAgICAgIC8vIFRoZSB2YWx1ZSBuZWVkcyB0byBzbmFwIHRvIHRoZSBtaW4gYW5kIG1heC5cbiAgICAgIHRoaXMudmFsdWUgPSB0aGlzLl9jbGFtcChjbG9zZXN0VmFsdWUsIHRoaXMubWluLCB0aGlzLm1heCk7XG4gICAgfVxuICB9XG5cbiAgLyoqIEVtaXRzIGEgY2hhbmdlIGV2ZW50IGlmIHRoZSBjdXJyZW50IHZhbHVlIGlzIGRpZmZlcmVudCBmcm9tIHRoZSBsYXN0IGVtaXR0ZWQgdmFsdWUuICovXG4gIHByaXZhdGUgX2VtaXRDaGFuZ2VFdmVudCgpIHtcbiAgICB0aGlzLl9jb250cm9sVmFsdWVBY2Nlc3NvckNoYW5nZUZuKHRoaXMudmFsdWUpO1xuICAgIHRoaXMudmFsdWVDaGFuZ2UuZW1pdCh0aGlzLnZhbHVlKTtcbiAgICB0aGlzLmNoYW5nZS5lbWl0KHRoaXMuX2NyZWF0ZUNoYW5nZUV2ZW50KCkpO1xuICB9XG5cbiAgLyoqIEVtaXRzIGFuIGlucHV0IGV2ZW50IHdoZW4gdGhlIGN1cnJlbnQgdmFsdWUgaXMgZGlmZmVyZW50IGZyb20gdGhlIGxhc3QgZW1pdHRlZCB2YWx1ZS4gKi9cbiAgcHJpdmF0ZSBfZW1pdElucHV0RXZlbnQoKSB7XG4gICAgdGhpcy5pbnB1dC5lbWl0KHRoaXMuX2NyZWF0ZUNoYW5nZUV2ZW50KCkpO1xuICB9XG5cbiAgLyoqIFVwZGF0ZXMgdGhlIGFtb3VudCBvZiBzcGFjZSBiZXR3ZWVuIHRpY2tzIGFzIGEgcGVyY2VudGFnZSBvZiB0aGUgd2lkdGggb2YgdGhlIHNsaWRlci4gKi9cbiAgcHJpdmF0ZSBfdXBkYXRlVGlja0ludGVydmFsUGVyY2VudCgpIHtcbiAgICBpZiAoIXRoaXMudGlja0ludGVydmFsIHx8ICF0aGlzLl9zbGlkZXJEaW1lbnNpb25zKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgaWYgKHRoaXMudGlja0ludGVydmFsID09ICdhdXRvJykge1xuICAgICAgbGV0IHRyYWNrU2l6ZSA9IHRoaXMudmVydGljYWwgPyB0aGlzLl9zbGlkZXJEaW1lbnNpb25zLmhlaWdodCA6IHRoaXMuX3NsaWRlckRpbWVuc2lvbnMud2lkdGg7XG4gICAgICBsZXQgcGl4ZWxzUGVyU3RlcCA9IHRyYWNrU2l6ZSAqIHRoaXMuc3RlcCAvICh0aGlzLm1heCAtIHRoaXMubWluKTtcbiAgICAgIGxldCBzdGVwc1BlclRpY2sgPSBNYXRoLmNlaWwoTUlOX0FVVE9fVElDS19TRVBBUkFUSU9OIC8gcGl4ZWxzUGVyU3RlcCk7XG4gICAgICBsZXQgcGl4ZWxzUGVyVGljayA9IHN0ZXBzUGVyVGljayAqIHRoaXMuc3RlcDtcbiAgICAgIHRoaXMuX3RpY2tJbnRlcnZhbFBlcmNlbnQgPSBwaXhlbHNQZXJUaWNrIC8gdHJhY2tTaXplO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLl90aWNrSW50ZXJ2YWxQZXJjZW50ID0gdGhpcy50aWNrSW50ZXJ2YWwgKiB0aGlzLnN0ZXAgLyAodGhpcy5tYXggLSB0aGlzLm1pbik7XG4gICAgfVxuICB9XG5cbiAgLyoqIENyZWF0ZXMgYSBzbGlkZXIgY2hhbmdlIG9iamVjdCBmcm9tIHRoZSBzcGVjaWZpZWQgdmFsdWUuICovXG4gIHByaXZhdGUgX2NyZWF0ZUNoYW5nZUV2ZW50KHZhbHVlID0gdGhpcy52YWx1ZSk6IE1hdFNsaWRlckNoYW5nZSB7XG4gICAgbGV0IGV2ZW50ID0gbmV3IE1hdFNsaWRlckNoYW5nZSgpO1xuXG4gICAgZXZlbnQuc291cmNlID0gdGhpcztcbiAgICBldmVudC52YWx1ZSA9IHZhbHVlO1xuXG4gICAgcmV0dXJuIGV2ZW50O1xuICB9XG5cbiAgLyoqIENhbGN1bGF0ZXMgdGhlIHBlcmNlbnRhZ2Ugb2YgdGhlIHNsaWRlciB0aGF0IGEgdmFsdWUgaXMuICovXG4gIHByaXZhdGUgX2NhbGN1bGF0ZVBlcmNlbnRhZ2UodmFsdWU6IG51bWJlciB8IG51bGwpIHtcbiAgICByZXR1cm4gKCh2YWx1ZSB8fCAwKSAtIHRoaXMubWluKSAvICh0aGlzLm1heCAtIHRoaXMubWluKTtcbiAgfVxuXG4gIC8qKiBDYWxjdWxhdGVzIHRoZSB2YWx1ZSBhIHBlcmNlbnRhZ2Ugb2YgdGhlIHNsaWRlciBjb3JyZXNwb25kcyB0by4gKi9cbiAgcHJpdmF0ZSBfY2FsY3VsYXRlVmFsdWUocGVyY2VudGFnZTogbnVtYmVyKSB7XG4gICAgcmV0dXJuIHRoaXMubWluICsgcGVyY2VudGFnZSAqICh0aGlzLm1heCAtIHRoaXMubWluKTtcbiAgfVxuXG4gIC8qKiBSZXR1cm4gYSBudW1iZXIgYmV0d2VlbiB0d28gbnVtYmVycy4gKi9cbiAgcHJpdmF0ZSBfY2xhbXAodmFsdWU6IG51bWJlciwgbWluID0gMCwgbWF4ID0gMSkge1xuICAgIHJldHVybiBNYXRoLm1heChtaW4sIE1hdGgubWluKHZhbHVlLCBtYXgpKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXQgdGhlIGJvdW5kaW5nIGNsaWVudCByZWN0IG9mIHRoZSBzbGlkZXIgdHJhY2sgZWxlbWVudC5cbiAgICogVGhlIHRyYWNrIGlzIHVzZWQgcmF0aGVyIHRoYW4gdGhlIG5hdGl2ZSBlbGVtZW50IHRvIGlnbm9yZSB0aGUgZXh0cmEgc3BhY2UgdGhhdCB0aGUgdGh1bWIgY2FuXG4gICAqIHRha2UgdXAuXG4gICAqL1xuICBwcml2YXRlIF9nZXRTbGlkZXJEaW1lbnNpb25zKCkge1xuICAgIHJldHVybiB0aGlzLl9zbGlkZXJXcmFwcGVyID8gdGhpcy5fc2xpZGVyV3JhcHBlci5uYXRpdmVFbGVtZW50LmdldEJvdW5kaW5nQ2xpZW50UmVjdCgpIDogbnVsbDtcbiAgfVxuXG4gIC8qKlxuICAgKiBGb2N1c2VzIHRoZSBuYXRpdmUgZWxlbWVudC5cbiAgICogQ3VycmVudGx5IG9ubHkgdXNlZCB0byBhbGxvdyBhIGJsdXIgZXZlbnQgdG8gZmlyZSBidXQgd2lsbCBiZSB1c2VkIHdpdGgga2V5Ym9hcmQgaW5wdXQgbGF0ZXIuXG4gICAqL1xuICBwcml2YXRlIF9mb2N1c0hvc3RFbGVtZW50KG9wdGlvbnM/OiBGb2N1c09wdGlvbnMpIHtcbiAgICB0aGlzLl9lbGVtZW50UmVmLm5hdGl2ZUVsZW1lbnQuZm9jdXMob3B0aW9ucyk7XG4gIH1cblxuICAvKiogQmx1cnMgdGhlIG5hdGl2ZSBlbGVtZW50LiAqL1xuICBwcml2YXRlIF9ibHVySG9zdEVsZW1lbnQoKSB7XG4gICAgdGhpcy5fZWxlbWVudFJlZi5uYXRpdmVFbGVtZW50LmJsdXIoKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBTZXRzIHRoZSBtb2RlbCB2YWx1ZS4gSW1wbGVtZW50ZWQgYXMgcGFydCBvZiBDb250cm9sVmFsdWVBY2Nlc3Nvci5cbiAgICogQHBhcmFtIHZhbHVlXG4gICAqL1xuICB3cml0ZVZhbHVlKHZhbHVlOiBhbnkpIHtcbiAgICB0aGlzLnZhbHVlID0gdmFsdWU7XG4gIH1cblxuICAvKipcbiAgICogUmVnaXN0ZXJzIGEgY2FsbGJhY2sgdG8gYmUgdHJpZ2dlcmVkIHdoZW4gdGhlIHZhbHVlIGhhcyBjaGFuZ2VkLlxuICAgKiBJbXBsZW1lbnRlZCBhcyBwYXJ0IG9mIENvbnRyb2xWYWx1ZUFjY2Vzc29yLlxuICAgKiBAcGFyYW0gZm4gQ2FsbGJhY2sgdG8gYmUgcmVnaXN0ZXJlZC5cbiAgICovXG4gIHJlZ2lzdGVyT25DaGFuZ2UoZm46ICh2YWx1ZTogYW55KSA9PiB2b2lkKSB7XG4gICAgdGhpcy5fY29udHJvbFZhbHVlQWNjZXNzb3JDaGFuZ2VGbiA9IGZuO1xuICB9XG5cbiAgLyoqXG4gICAqIFJlZ2lzdGVycyBhIGNhbGxiYWNrIHRvIGJlIHRyaWdnZXJlZCB3aGVuIHRoZSBjb21wb25lbnQgaXMgdG91Y2hlZC5cbiAgICogSW1wbGVtZW50ZWQgYXMgcGFydCBvZiBDb250cm9sVmFsdWVBY2Nlc3Nvci5cbiAgICogQHBhcmFtIGZuIENhbGxiYWNrIHRvIGJlIHJlZ2lzdGVyZWQuXG4gICAqL1xuICByZWdpc3Rlck9uVG91Y2hlZChmbjogYW55KSB7XG4gICAgdGhpcy5vblRvdWNoZWQgPSBmbjtcbiAgfVxuXG4gIC8qKlxuICAgKiBTZXRzIHdoZXRoZXIgdGhlIGNvbXBvbmVudCBzaG91bGQgYmUgZGlzYWJsZWQuXG4gICAqIEltcGxlbWVudGVkIGFzIHBhcnQgb2YgQ29udHJvbFZhbHVlQWNjZXNzb3IuXG4gICAqIEBwYXJhbSBpc0Rpc2FibGVkXG4gICAqL1xuICBzZXREaXNhYmxlZFN0YXRlKGlzRGlzYWJsZWQ6IGJvb2xlYW4pIHtcbiAgICB0aGlzLmRpc2FibGVkID0gaXNEaXNhYmxlZDtcbiAgfVxuXG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV9pbnZlcnQ6IEJvb2xlYW5JbnB1dDtcbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX21heDogTnVtYmVySW5wdXQ7XG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV9taW46IE51bWJlcklucHV0O1xuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfc3RlcDogTnVtYmVySW5wdXQ7XG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV90aHVtYkxhYmVsOiBCb29sZWFuSW5wdXQ7XG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV90aWNrSW50ZXJ2YWw6IE51bWJlcklucHV0O1xuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfdmFsdWU6IE51bWJlcklucHV0O1xuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfdmVydGljYWw6IEJvb2xlYW5JbnB1dDtcbiAgc3RhdGljIG5nQWNjZXB0SW5wdXRUeXBlX2Rpc2FibGVkOiBCb29sZWFuSW5wdXQ7XG59XG5cbi8qKiBSZXR1cm5zIHdoZXRoZXIgYW4gZXZlbnQgaXMgYSB0b3VjaCBldmVudC4gKi9cbmZ1bmN0aW9uIGlzVG91Y2hFdmVudChldmVudDogTW91c2VFdmVudCB8IFRvdWNoRXZlbnQpOiBldmVudCBpcyBUb3VjaEV2ZW50IHtcbiAgLy8gVGhpcyBmdW5jdGlvbiBpcyBjYWxsZWQgZm9yIGV2ZXJ5IHBpeGVsIHRoYXQgdGhlIHVzZXIgaGFzIGRyYWdnZWQgc28gd2UgbmVlZCBpdCB0byBiZVxuICAvLyBhcyBmYXN0IGFzIHBvc3NpYmxlLiBTaW5jZSB3ZSBvbmx5IGJpbmQgbW91c2UgZXZlbnRzIGFuZCB0b3VjaCBldmVudHMsIHdlIGNhbiBhc3N1bWVcbiAgLy8gdGhhdCBpZiB0aGUgZXZlbnQncyBuYW1lIHN0YXJ0cyB3aXRoIGB0YCwgaXQncyBhIHRvdWNoIGV2ZW50LlxuICByZXR1cm4gZXZlbnQudHlwZVswXSA9PT0gJ3QnO1xufVxuXG4vKiogR2V0cyB0aGUgY29vcmRpbmF0ZXMgb2YgYSB0b3VjaCBvciBtb3VzZSBldmVudCByZWxhdGl2ZSB0byB0aGUgdmlld3BvcnQuICovXG5mdW5jdGlvbiBnZXRQb2ludGVyUG9zaXRpb25PblBhZ2UoZXZlbnQ6IE1vdXNlRXZlbnQgfCBUb3VjaEV2ZW50KSB7XG4gIC8vIGB0b3VjaGVzYCB3aWxsIGJlIGVtcHR5IGZvciBzdGFydC9lbmQgZXZlbnRzIHNvIHdlIGhhdmUgdG8gZmFsbCBiYWNrIHRvIGBjaGFuZ2VkVG91Y2hlc2AuXG4gIGNvbnN0IHBvaW50ID0gaXNUb3VjaEV2ZW50KGV2ZW50KSA/IChldmVudC50b3VjaGVzWzBdIHx8IGV2ZW50LmNoYW5nZWRUb3VjaGVzWzBdKSA6IGV2ZW50O1xuICByZXR1cm4ge3g6IHBvaW50LmNsaWVudFgsIHk6IHBvaW50LmNsaWVudFl9O1xufVxuIl19