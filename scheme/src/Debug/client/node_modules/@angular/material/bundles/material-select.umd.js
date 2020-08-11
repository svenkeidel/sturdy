(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('@angular/cdk/overlay'), require('@angular/common'), require('@angular/core'), require('@angular/material/core'), require('@angular/material/form-field'), require('@angular/cdk/scrolling'), require('@angular/cdk/a11y'), require('@angular/cdk/bidi'), require('@angular/cdk/coercion'), require('@angular/cdk/collections'), require('@angular/cdk/keycodes'), require('@angular/forms'), require('rxjs'), require('rxjs/operators'), require('@angular/animations')) :
    typeof define === 'function' && define.amd ? define('@angular/material/select', ['exports', '@angular/cdk/overlay', '@angular/common', '@angular/core', '@angular/material/core', '@angular/material/form-field', '@angular/cdk/scrolling', '@angular/cdk/a11y', '@angular/cdk/bidi', '@angular/cdk/coercion', '@angular/cdk/collections', '@angular/cdk/keycodes', '@angular/forms', 'rxjs', 'rxjs/operators', '@angular/animations'], factory) :
    (global = global || self, factory((global.ng = global.ng || {}, global.ng.material = global.ng.material || {}, global.ng.material.select = {}), global.ng.cdk.overlay, global.ng.common, global.ng.core, global.ng.material.core, global.ng.material.formField, global.ng.cdk.scrolling, global.ng.cdk.a11y, global.ng.cdk.bidi, global.ng.cdk.coercion, global.ng.cdk.collections, global.ng.cdk.keycodes, global.ng.forms, global.rxjs, global.rxjs.operators, global.ng.animations));
}(this, (function (exports, overlay, common, core, core$1, formField, scrolling, a11y, bidi, coercion, collections, keycodes, forms, rxjs, operators, animations) { 'use strict';

    /*! *****************************************************************************
    Copyright (c) Microsoft Corporation.

    Permission to use, copy, modify, and/or distribute this software for any
    purpose with or without fee is hereby granted.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
    REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
    AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
    INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
    LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
    OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
    PERFORMANCE OF THIS SOFTWARE.
    ***************************************************************************** */
    /* global Reflect, Promise */

    var extendStatics = function(d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };

    function __extends(d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    }

    var __assign = function() {
        __assign = Object.assign || function __assign(t) {
            for (var s, i = 1, n = arguments.length; i < n; i++) {
                s = arguments[i];
                for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p)) t[p] = s[p];
            }
            return t;
        };
        return __assign.apply(this, arguments);
    };

    function __rest(s, e) {
        var t = {};
        for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
            t[p] = s[p];
        if (s != null && typeof Object.getOwnPropertySymbols === "function")
            for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
                if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                    t[p[i]] = s[p[i]];
            }
        return t;
    }

    function __decorate(decorators, target, key, desc) {
        var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
        if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
        else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
        return c > 3 && r && Object.defineProperty(target, key, r), r;
    }

    function __param(paramIndex, decorator) {
        return function (target, key) { decorator(target, key, paramIndex); }
    }

    function __metadata(metadataKey, metadataValue) {
        if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(metadataKey, metadataValue);
    }

    function __awaiter(thisArg, _arguments, P, generator) {
        function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
        return new (P || (P = Promise))(function (resolve, reject) {
            function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
            function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
            function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
            step((generator = generator.apply(thisArg, _arguments || [])).next());
        });
    }

    function __generator(thisArg, body) {
        var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
        return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
        function verb(n) { return function (v) { return step([n, v]); }; }
        function step(op) {
            if (f) throw new TypeError("Generator is already executing.");
            while (_) try {
                if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
                if (y = 0, t) op = [op[0] & 2, t.value];
                switch (op[0]) {
                    case 0: case 1: t = op; break;
                    case 4: _.label++; return { value: op[1], done: false };
                    case 5: _.label++; y = op[1]; op = [0]; continue;
                    case 7: op = _.ops.pop(); _.trys.pop(); continue;
                    default:
                        if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                        if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                        if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                        if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                        if (t[2]) _.ops.pop();
                        _.trys.pop(); continue;
                }
                op = body.call(thisArg, _);
            } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
            if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
        }
    }

    var __createBinding = Object.create ? (function(o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
    }) : (function(o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
    });

    function __exportStar(m, exports) {
        for (var p in m) if (p !== "default" && !exports.hasOwnProperty(p)) __createBinding(exports, m, p);
    }

    function __values(o) {
        var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
        if (m) return m.call(o);
        if (o && typeof o.length === "number") return {
            next: function () {
                if (o && i >= o.length) o = void 0;
                return { value: o && o[i++], done: !o };
            }
        };
        throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
    }

    function __read(o, n) {
        var m = typeof Symbol === "function" && o[Symbol.iterator];
        if (!m) return o;
        var i = m.call(o), r, ar = [], e;
        try {
            while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
        }
        catch (error) { e = { error: error }; }
        finally {
            try {
                if (r && !r.done && (m = i["return"])) m.call(i);
            }
            finally { if (e) throw e.error; }
        }
        return ar;
    }

    function __spread() {
        for (var ar = [], i = 0; i < arguments.length; i++)
            ar = ar.concat(__read(arguments[i]));
        return ar;
    }

    function __spreadArrays() {
        for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
        for (var r = Array(s), k = 0, i = 0; i < il; i++)
            for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
                r[k] = a[j];
        return r;
    };

    function __await(v) {
        return this instanceof __await ? (this.v = v, this) : new __await(v);
    }

    function __asyncGenerator(thisArg, _arguments, generator) {
        if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
        var g = generator.apply(thisArg, _arguments || []), i, q = [];
        return i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i;
        function verb(n) { if (g[n]) i[n] = function (v) { return new Promise(function (a, b) { q.push([n, v, a, b]) > 1 || resume(n, v); }); }; }
        function resume(n, v) { try { step(g[n](v)); } catch (e) { settle(q[0][3], e); } }
        function step(r) { r.value instanceof __await ? Promise.resolve(r.value.v).then(fulfill, reject) : settle(q[0][2], r); }
        function fulfill(value) { resume("next", value); }
        function reject(value) { resume("throw", value); }
        function settle(f, v) { if (f(v), q.shift(), q.length) resume(q[0][0], q[0][1]); }
    }

    function __asyncDelegator(o) {
        var i, p;
        return i = {}, verb("next"), verb("throw", function (e) { throw e; }), verb("return"), i[Symbol.iterator] = function () { return this; }, i;
        function verb(n, f) { i[n] = o[n] ? function (v) { return (p = !p) ? { value: __await(o[n](v)), done: n === "return" } : f ? f(v) : v; } : f; }
    }

    function __asyncValues(o) {
        if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
        var m = o[Symbol.asyncIterator], i;
        return m ? m.call(o) : (o = typeof __values === "function" ? __values(o) : o[Symbol.iterator](), i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i);
        function verb(n) { i[n] = o[n] && function (v) { return new Promise(function (resolve, reject) { v = o[n](v), settle(resolve, reject, v.done, v.value); }); }; }
        function settle(resolve, reject, d, v) { Promise.resolve(v).then(function(v) { resolve({ value: v, done: d }); }, reject); }
    }

    function __makeTemplateObject(cooked, raw) {
        if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
        return cooked;
    };

    var __setModuleDefault = Object.create ? (function(o, v) {
        Object.defineProperty(o, "default", { enumerable: true, value: v });
    }) : function(o, v) {
        o["default"] = v;
    };

    function __importStar(mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
        __setModuleDefault(result, mod);
        return result;
    }

    function __importDefault(mod) {
        return (mod && mod.__esModule) ? mod : { default: mod };
    }

    function __classPrivateFieldGet(receiver, privateMap) {
        if (!privateMap.has(receiver)) {
            throw new TypeError("attempted to get private field on non-instance");
        }
        return privateMap.get(receiver);
    }

    function __classPrivateFieldSet(receiver, privateMap, value) {
        if (!privateMap.has(receiver)) {
            throw new TypeError("attempted to set private field on non-instance");
        }
        privateMap.set(receiver, value);
        return value;
    }

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /**
     * The following are all the animations for the mat-select component, with each
     * const containing the metadata for one animation.
     *
     * The values below match the implementation of the AngularJS Material mat-select animation.
     * @docs-private
     */
    var matSelectAnimations = {
        /**
         * This animation ensures the select's overlay panel animation (transformPanel) is called when
         * closing the select.
         * This is needed due to https://github.com/angular/angular/issues/23302
         */
        transformPanelWrap: animations.trigger('transformPanelWrap', [
            animations.transition('* => void', animations.query('@transformPanel', [animations.animateChild()], { optional: true }))
        ]),
        /**
         * This animation transforms the select's overlay panel on and off the page.
         *
         * When the panel is attached to the DOM, it expands its width by the amount of padding, scales it
         * up to 100% on the Y axis, fades in its border, and translates slightly up and to the
         * side to ensure the option text correctly overlaps the trigger text.
         *
         * When the panel is removed from the DOM, it simply fades out linearly.
         */
        transformPanel: animations.trigger('transformPanel', [
            animations.state('void', animations.style({
                transform: 'scaleY(0.8)',
                minWidth: '100%',
                opacity: 0
            })),
            animations.state('showing', animations.style({
                opacity: 1,
                minWidth: 'calc(100% + 32px)',
                transform: 'scaleY(1)'
            })),
            animations.state('showing-multiple', animations.style({
                opacity: 1,
                minWidth: 'calc(100% + 64px)',
                transform: 'scaleY(1)'
            })),
            animations.transition('void => *', animations.animate('120ms cubic-bezier(0, 0, 0.2, 1)')),
            animations.transition('* => void', animations.animate('100ms 25ms linear', animations.style({ opacity: 0 })))
        ])
    };

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /**
     * Returns an exception to be thrown when attempting to change a select's `multiple` option
     * after initialization.
     * @docs-private
     */
    function getMatSelectDynamicMultipleError() {
        return Error('Cannot change `multiple` mode of select after initialization.');
    }
    /**
     * Returns an exception to be thrown when attempting to assign a non-array value to a select
     * in `multiple` mode. Note that `undefined` and `null` are still valid values to allow for
     * resetting the value.
     * @docs-private
     */
    function getMatSelectNonArrayValueError() {
        return Error('Value must be an array in multiple-selection mode.');
    }
    /**
     * Returns an exception to be thrown when assigning a non-function value to the comparator
     * used to determine if a value corresponds to an option. Note that whether the function
     * actually takes two values and returns a boolean is not checked.
     */
    function getMatSelectNonFunctionValueError() {
        return Error('`compareWith` must be a function.');
    }

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    var nextUniqueId = 0;
    /**
     * The following style constants are necessary to save here in order
     * to properly calculate the alignment of the selected option over
     * the trigger element.
     */
    /** The max height of the select's overlay panel */
    var SELECT_PANEL_MAX_HEIGHT = 256;
    /** The panel's padding on the x-axis */
    var SELECT_PANEL_PADDING_X = 16;
    /** The panel's x axis padding if it is indented (e.g. there is an option group). */
    var SELECT_PANEL_INDENT_PADDING_X = SELECT_PANEL_PADDING_X * 2;
    /** The height of the select items in `em` units. */
    var SELECT_ITEM_HEIGHT_EM = 3;
    // TODO(josephperrott): Revert to a constant after 2018 spec updates are fully merged.
    /**
     * Distance between the panel edge and the option text in
     * multi-selection mode.
     *
     * Calculated as:
     * (SELECT_PANEL_PADDING_X * 1.5) + 16 = 40
     * The padding is multiplied by 1.5 because the checkbox's margin is half the padding.
     * The checkbox width is 16px.
     */
    var SELECT_MULTIPLE_PANEL_PADDING_X = SELECT_PANEL_PADDING_X * 1.5 + 16;
    /**
     * The select panel will only "fit" inside the viewport if it is positioned at
     * this value or more away from the viewport boundary.
     */
    var SELECT_PANEL_VIEWPORT_PADDING = 8;
    /** Injection token that determines the scroll handling while a select is open. */
    var MAT_SELECT_SCROLL_STRATEGY = new core.InjectionToken('mat-select-scroll-strategy');
    /** @docs-private */
    function MAT_SELECT_SCROLL_STRATEGY_PROVIDER_FACTORY(overlay) {
        return function () { return overlay.scrollStrategies.reposition(); };
    }
    /** Injection token that can be used to provide the default options the select module. */
    var MAT_SELECT_CONFIG = new core.InjectionToken('MAT_SELECT_CONFIG');
    /** @docs-private */
    var MAT_SELECT_SCROLL_STRATEGY_PROVIDER = {
        provide: MAT_SELECT_SCROLL_STRATEGY,
        deps: [overlay.Overlay],
        useFactory: MAT_SELECT_SCROLL_STRATEGY_PROVIDER_FACTORY,
    };
    /** Change event object that is emitted when the select value has changed. */
    var MatSelectChange = /** @class */ (function () {
        function MatSelectChange(
        /** Reference to the select that emitted the change event. */
        source, 
        /** Current value of the select that emitted the event. */
        value) {
            this.source = source;
            this.value = value;
        }
        return MatSelectChange;
    }());
    // Boilerplate for applying mixins to MatSelect.
    /** @docs-private */
    var MatSelectBase = /** @class */ (function () {
        function MatSelectBase(_elementRef, _defaultErrorStateMatcher, _parentForm, _parentFormGroup, ngControl) {
            this._elementRef = _elementRef;
            this._defaultErrorStateMatcher = _defaultErrorStateMatcher;
            this._parentForm = _parentForm;
            this._parentFormGroup = _parentFormGroup;
            this.ngControl = ngControl;
        }
        return MatSelectBase;
    }());
    var _MatSelectMixinBase = core$1.mixinDisableRipple(core$1.mixinTabIndex(core$1.mixinDisabled(core$1.mixinErrorState(MatSelectBase))));
    /**
     * Allows the user to customize the trigger that is displayed when the select has a value.
     */
    var MatSelectTrigger = /** @class */ (function () {
        function MatSelectTrigger() {
        }
        MatSelectTrigger.decorators = [
            { type: core.Directive, args: [{
                        selector: 'mat-select-trigger'
                    },] }
        ];
        return MatSelectTrigger;
    }());
    var MatSelect = /** @class */ (function (_super) {
        __extends(MatSelect, _super);
        function MatSelect(_viewportRuler, _changeDetectorRef, _ngZone, _defaultErrorStateMatcher, elementRef, _dir, _parentForm, _parentFormGroup, _parentFormField, ngControl, tabIndex, scrollStrategyFactory, _liveAnnouncer, defaults) {
            var _this = _super.call(this, elementRef, _defaultErrorStateMatcher, _parentForm, _parentFormGroup, ngControl) || this;
            _this._viewportRuler = _viewportRuler;
            _this._changeDetectorRef = _changeDetectorRef;
            _this._ngZone = _ngZone;
            _this._dir = _dir;
            _this._parentFormField = _parentFormField;
            _this.ngControl = ngControl;
            _this._liveAnnouncer = _liveAnnouncer;
            /** Whether or not the overlay panel is open. */
            _this._panelOpen = false;
            /** Whether filling out the select is required in the form. */
            _this._required = false;
            /** The scroll position of the overlay panel, calculated to center the selected option. */
            _this._scrollTop = 0;
            /** Whether the component is in multiple selection mode. */
            _this._multiple = false;
            /** Comparison function to specify which option is displayed. Defaults to object equality. */
            _this._compareWith = function (o1, o2) { return o1 === o2; };
            /** Unique id for this input. */
            _this._uid = "mat-select-" + nextUniqueId++;
            /** Emits whenever the component is destroyed. */
            _this._destroy = new rxjs.Subject();
            /** The cached font-size of the trigger element. */
            _this._triggerFontSize = 0;
            /** `View -> model callback called when value changes` */
            _this._onChange = function () { };
            /** `View -> model callback called when select has been touched` */
            _this._onTouched = function () { };
            /** The IDs of child options to be passed to the aria-owns attribute. */
            _this._optionIds = '';
            /** The value of the select panel's transform-origin property. */
            _this._transformOrigin = 'top';
            /** Emits when the panel element is finished transforming in. */
            _this._panelDoneAnimatingStream = new rxjs.Subject();
            /**
             * The y-offset of the overlay panel in relation to the trigger's top start corner.
             * This must be adjusted to align the selected option text over the trigger text.
             * when the panel opens. Will change based on the y-position of the selected option.
             */
            _this._offsetY = 0;
            /**
             * This position config ensures that the top "start" corner of the overlay
             * is aligned with with the top "start" of the origin by default (overlapping
             * the trigger completely). If the panel cannot fit below the trigger, it
             * will fall back to a position above the trigger.
             */
            _this._positions = [
                {
                    originX: 'start',
                    originY: 'top',
                    overlayX: 'start',
                    overlayY: 'top',
                },
                {
                    originX: 'start',
                    originY: 'bottom',
                    overlayX: 'start',
                    overlayY: 'bottom',
                },
            ];
            /** Whether the component is disabling centering of the active option over the trigger. */
            _this._disableOptionCentering = false;
            _this._focused = false;
            /** A name for this control that can be used by `mat-form-field`. */
            _this.controlType = 'mat-select';
            /** Aria label of the select. If not specified, the placeholder will be used as label. */
            _this.ariaLabel = '';
            /** Combined stream of all of the child options' change events. */
            _this.optionSelectionChanges = rxjs.defer(function () {
                var options = _this.options;
                if (options) {
                    return options.changes.pipe(operators.startWith(options), operators.switchMap(function () { return rxjs.merge.apply(void 0, __spread(options.map(function (option) { return option.onSelectionChange; }))); }));
                }
                return _this._ngZone.onStable
                    .asObservable()
                    .pipe(operators.take(1), operators.switchMap(function () { return _this.optionSelectionChanges; }));
            });
            /** Event emitted when the select panel has been toggled. */
            _this.openedChange = new core.EventEmitter();
            /** Event emitted when the select has been opened. */
            _this._openedStream = _this.openedChange.pipe(operators.filter(function (o) { return o; }), operators.map(function () { }));
            /** Event emitted when the select has been closed. */
            _this._closedStream = _this.openedChange.pipe(operators.filter(function (o) { return !o; }), operators.map(function () { }));
            /** Event emitted when the selected value has been changed by the user. */
            _this.selectionChange = new core.EventEmitter();
            /**
             * Event that emits whenever the raw value of the select changes. This is here primarily
             * to facilitate the two-way binding for the `value` input.
             * @docs-private
             */
            _this.valueChange = new core.EventEmitter();
            if (_this.ngControl) {
                // Note: we provide the value accessor through here, instead of
                // the `providers` to avoid running into a circular import.
                _this.ngControl.valueAccessor = _this;
            }
            _this._scrollStrategyFactory = scrollStrategyFactory;
            _this._scrollStrategy = _this._scrollStrategyFactory();
            _this.tabIndex = parseInt(tabIndex) || 0;
            // Force setter to be called in case id was not specified.
            _this.id = _this.id;
            if (defaults) {
                if (defaults.disableOptionCentering != null) {
                    _this.disableOptionCentering = defaults.disableOptionCentering;
                }
                if (defaults.typeaheadDebounceInterval != null) {
                    _this.typeaheadDebounceInterval = defaults.typeaheadDebounceInterval;
                }
            }
            return _this;
        }
        Object.defineProperty(MatSelect.prototype, "focused", {
            /** Whether the select is focused. */
            get: function () {
                return this._focused || this._panelOpen;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatSelect.prototype, "placeholder", {
            /** Placeholder to be shown if no value has been selected. */
            get: function () { return this._placeholder; },
            set: function (value) {
                this._placeholder = value;
                this.stateChanges.next();
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatSelect.prototype, "required", {
            /** Whether the component is required. */
            get: function () { return this._required; },
            set: function (value) {
                this._required = coercion.coerceBooleanProperty(value);
                this.stateChanges.next();
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatSelect.prototype, "multiple", {
            /** Whether the user should be allowed to select multiple options. */
            get: function () { return this._multiple; },
            set: function (value) {
                if (this._selectionModel) {
                    throw getMatSelectDynamicMultipleError();
                }
                this._multiple = coercion.coerceBooleanProperty(value);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatSelect.prototype, "disableOptionCentering", {
            /** Whether to center the active option over the trigger. */
            get: function () { return this._disableOptionCentering; },
            set: function (value) {
                this._disableOptionCentering = coercion.coerceBooleanProperty(value);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatSelect.prototype, "compareWith", {
            /**
             * Function to compare the option values with the selected values. The first argument
             * is a value from an option. The second is a value from the selection. A boolean
             * should be returned.
             */
            get: function () { return this._compareWith; },
            set: function (fn) {
                if (typeof fn !== 'function') {
                    throw getMatSelectNonFunctionValueError();
                }
                this._compareWith = fn;
                if (this._selectionModel) {
                    // A different comparator means the selection could change.
                    this._initializeSelection();
                }
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatSelect.prototype, "value", {
            /** Value of the select control. */
            get: function () { return this._value; },
            set: function (newValue) {
                if (newValue !== this._value) {
                    this.writeValue(newValue);
                    this._value = newValue;
                }
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatSelect.prototype, "typeaheadDebounceInterval", {
            /** Time to wait in milliseconds after the last keystroke before moving focus to an item. */
            get: function () { return this._typeaheadDebounceInterval; },
            set: function (value) {
                this._typeaheadDebounceInterval = coercion.coerceNumberProperty(value);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatSelect.prototype, "id", {
            /** Unique id of the element. */
            get: function () { return this._id; },
            set: function (value) {
                this._id = value || this._uid;
                this.stateChanges.next();
            },
            enumerable: false,
            configurable: true
        });
        MatSelect.prototype.ngOnInit = function () {
            var _this = this;
            this._selectionModel = new collections.SelectionModel(this.multiple);
            this.stateChanges.next();
            // We need `distinctUntilChanged` here, because some browsers will
            // fire the animation end event twice for the same animation. See:
            // https://github.com/angular/angular/issues/24084
            this._panelDoneAnimatingStream
                .pipe(operators.distinctUntilChanged(), operators.takeUntil(this._destroy))
                .subscribe(function () {
                if (_this.panelOpen) {
                    _this._scrollTop = 0;
                    _this.openedChange.emit(true);
                }
                else {
                    _this.openedChange.emit(false);
                    _this.overlayDir.offsetX = 0;
                    _this._changeDetectorRef.markForCheck();
                }
            });
            this._viewportRuler.change()
                .pipe(operators.takeUntil(this._destroy))
                .subscribe(function () {
                if (_this._panelOpen) {
                    _this._triggerRect = _this.trigger.nativeElement.getBoundingClientRect();
                    _this._changeDetectorRef.markForCheck();
                }
            });
        };
        MatSelect.prototype.ngAfterContentInit = function () {
            var _this = this;
            this._initKeyManager();
            this._selectionModel.changed.pipe(operators.takeUntil(this._destroy)).subscribe(function (event) {
                event.added.forEach(function (option) { return option.select(); });
                event.removed.forEach(function (option) { return option.deselect(); });
            });
            this.options.changes.pipe(operators.startWith(null), operators.takeUntil(this._destroy)).subscribe(function () {
                _this._resetOptions();
                _this._initializeSelection();
            });
        };
        MatSelect.prototype.ngDoCheck = function () {
            if (this.ngControl) {
                this.updateErrorState();
            }
        };
        MatSelect.prototype.ngOnChanges = function (changes) {
            // Updating the disabled state is handled by `mixinDisabled`, but we need to additionally let
            // the parent form field know to run change detection when the disabled state changes.
            if (changes['disabled']) {
                this.stateChanges.next();
            }
            if (changes['typeaheadDebounceInterval'] && this._keyManager) {
                this._keyManager.withTypeAhead(this._typeaheadDebounceInterval);
            }
        };
        MatSelect.prototype.ngOnDestroy = function () {
            this._destroy.next();
            this._destroy.complete();
            this.stateChanges.complete();
        };
        /** Toggles the overlay panel open or closed. */
        MatSelect.prototype.toggle = function () {
            this.panelOpen ? this.close() : this.open();
        };
        /** Opens the overlay panel. */
        MatSelect.prototype.open = function () {
            var _this = this;
            if (this.disabled || !this.options || !this.options.length || this._panelOpen) {
                return;
            }
            this._triggerRect = this.trigger.nativeElement.getBoundingClientRect();
            // Note: The computed font-size will be a string pixel value (e.g. "16px").
            // `parseInt` ignores the trailing 'px' and converts this to a number.
            this._triggerFontSize = parseInt(getComputedStyle(this.trigger.nativeElement).fontSize || '0');
            this._panelOpen = true;
            this._keyManager.withHorizontalOrientation(null);
            this._calculateOverlayPosition();
            this._highlightCorrectOption();
            this._changeDetectorRef.markForCheck();
            // Set the font size on the panel element once it exists.
            this._ngZone.onStable.asObservable().pipe(operators.take(1)).subscribe(function () {
                if (_this._triggerFontSize && _this.overlayDir.overlayRef &&
                    _this.overlayDir.overlayRef.overlayElement) {
                    _this.overlayDir.overlayRef.overlayElement.style.fontSize = _this._triggerFontSize + "px";
                }
            });
        };
        /** Closes the overlay panel and focuses the host element. */
        MatSelect.prototype.close = function () {
            if (this._panelOpen) {
                this._panelOpen = false;
                this._keyManager.withHorizontalOrientation(this._isRtl() ? 'rtl' : 'ltr');
                this._changeDetectorRef.markForCheck();
                this._onTouched();
            }
        };
        /**
         * Sets the select's value. Part of the ControlValueAccessor interface
         * required to integrate with Angular's core forms API.
         *
         * @param value New value to be written to the model.
         */
        MatSelect.prototype.writeValue = function (value) {
            if (this.options) {
                this._setSelectionByValue(value);
            }
        };
        /**
         * Saves a callback function to be invoked when the select's value
         * changes from user input. Part of the ControlValueAccessor interface
         * required to integrate with Angular's core forms API.
         *
         * @param fn Callback to be triggered when the value changes.
         */
        MatSelect.prototype.registerOnChange = function (fn) {
            this._onChange = fn;
        };
        /**
         * Saves a callback function to be invoked when the select is blurred
         * by the user. Part of the ControlValueAccessor interface required
         * to integrate with Angular's core forms API.
         *
         * @param fn Callback to be triggered when the component has been touched.
         */
        MatSelect.prototype.registerOnTouched = function (fn) {
            this._onTouched = fn;
        };
        /**
         * Disables the select. Part of the ControlValueAccessor interface required
         * to integrate with Angular's core forms API.
         *
         * @param isDisabled Sets whether the component is disabled.
         */
        MatSelect.prototype.setDisabledState = function (isDisabled) {
            this.disabled = isDisabled;
            this._changeDetectorRef.markForCheck();
            this.stateChanges.next();
        };
        Object.defineProperty(MatSelect.prototype, "panelOpen", {
            /** Whether or not the overlay panel is open. */
            get: function () {
                return this._panelOpen;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatSelect.prototype, "selected", {
            /** The currently selected option. */
            get: function () {
                return this.multiple ? this._selectionModel.selected : this._selectionModel.selected[0];
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatSelect.prototype, "triggerValue", {
            /** The value displayed in the trigger. */
            get: function () {
                if (this.empty) {
                    return '';
                }
                if (this._multiple) {
                    var selectedOptions = this._selectionModel.selected.map(function (option) { return option.viewValue; });
                    if (this._isRtl()) {
                        selectedOptions.reverse();
                    }
                    // TODO(crisbeto): delimiter should be configurable for proper localization.
                    return selectedOptions.join(', ');
                }
                return this._selectionModel.selected[0].viewValue;
            },
            enumerable: false,
            configurable: true
        });
        /** Whether the element is in RTL mode. */
        MatSelect.prototype._isRtl = function () {
            return this._dir ? this._dir.value === 'rtl' : false;
        };
        /** Handles all keydown events on the select. */
        MatSelect.prototype._handleKeydown = function (event) {
            if (!this.disabled) {
                this.panelOpen ? this._handleOpenKeydown(event) : this._handleClosedKeydown(event);
            }
        };
        /** Handles keyboard events while the select is closed. */
        MatSelect.prototype._handleClosedKeydown = function (event) {
            var keyCode = event.keyCode;
            var isArrowKey = keyCode === keycodes.DOWN_ARROW || keyCode === keycodes.UP_ARROW ||
                keyCode === keycodes.LEFT_ARROW || keyCode === keycodes.RIGHT_ARROW;
            var isOpenKey = keyCode === keycodes.ENTER || keyCode === keycodes.SPACE;
            var manager = this._keyManager;
            // Open the select on ALT + arrow key to match the native <select>
            if (!manager.isTyping() && (isOpenKey && !keycodes.hasModifierKey(event)) ||
                ((this.multiple || event.altKey) && isArrowKey)) {
                event.preventDefault(); // prevents the page from scrolling down when pressing space
                this.open();
            }
            else if (!this.multiple) {
                var previouslySelectedOption = this.selected;
                if (keyCode === keycodes.HOME || keyCode === keycodes.END) {
                    keyCode === keycodes.HOME ? manager.setFirstItemActive() : manager.setLastItemActive();
                    event.preventDefault();
                }
                else {
                    manager.onKeydown(event);
                }
                var selectedOption = this.selected;
                // Since the value has changed, we need to announce it ourselves.
                if (selectedOption && previouslySelectedOption !== selectedOption) {
                    // We set a duration on the live announcement, because we want the live element to be
                    // cleared after a while so that users can't navigate to it using the arrow keys.
                    this._liveAnnouncer.announce(selectedOption.viewValue, 10000);
                }
            }
        };
        /** Handles keyboard events when the selected is open. */
        MatSelect.prototype._handleOpenKeydown = function (event) {
            var manager = this._keyManager;
            var keyCode = event.keyCode;
            var isArrowKey = keyCode === keycodes.DOWN_ARROW || keyCode === keycodes.UP_ARROW;
            var isTyping = manager.isTyping();
            if (keyCode === keycodes.HOME || keyCode === keycodes.END) {
                event.preventDefault();
                keyCode === keycodes.HOME ? manager.setFirstItemActive() : manager.setLastItemActive();
            }
            else if (isArrowKey && event.altKey) {
                // Close the select on ALT + arrow key to match the native <select>
                event.preventDefault();
                this.close();
                // Don't do anything in this case if the user is typing,
                // because the typing sequence can include the space key.
            }
            else if (!isTyping && (keyCode === keycodes.ENTER || keyCode === keycodes.SPACE) && manager.activeItem &&
                !keycodes.hasModifierKey(event)) {
                event.preventDefault();
                manager.activeItem._selectViaInteraction();
            }
            else if (!isTyping && this._multiple && keyCode === keycodes.A && event.ctrlKey) {
                event.preventDefault();
                var hasDeselectedOptions_1 = this.options.some(function (opt) { return !opt.disabled && !opt.selected; });
                this.options.forEach(function (option) {
                    if (!option.disabled) {
                        hasDeselectedOptions_1 ? option.select() : option.deselect();
                    }
                });
            }
            else {
                var previouslyFocusedIndex = manager.activeItemIndex;
                manager.onKeydown(event);
                if (this._multiple && isArrowKey && event.shiftKey && manager.activeItem &&
                    manager.activeItemIndex !== previouslyFocusedIndex) {
                    manager.activeItem._selectViaInteraction();
                }
            }
        };
        MatSelect.prototype._onFocus = function () {
            if (!this.disabled) {
                this._focused = true;
                this.stateChanges.next();
            }
        };
        /**
         * Calls the touched callback only if the panel is closed. Otherwise, the trigger will
         * "blur" to the panel when it opens, causing a false positive.
         */
        MatSelect.prototype._onBlur = function () {
            this._focused = false;
            if (!this.disabled && !this.panelOpen) {
                this._onTouched();
                this._changeDetectorRef.markForCheck();
                this.stateChanges.next();
            }
        };
        /**
         * Callback that is invoked when the overlay panel has been attached.
         */
        MatSelect.prototype._onAttached = function () {
            var _this = this;
            this.overlayDir.positionChange.pipe(operators.take(1)).subscribe(function () {
                _this._changeDetectorRef.detectChanges();
                _this._calculateOverlayOffsetX();
                _this.panel.nativeElement.scrollTop = _this._scrollTop;
            });
        };
        /** Returns the theme to be used on the panel. */
        MatSelect.prototype._getPanelTheme = function () {
            return this._parentFormField ? "mat-" + this._parentFormField.color : '';
        };
        Object.defineProperty(MatSelect.prototype, "empty", {
            /** Whether the select has a value. */
            get: function () {
                return !this._selectionModel || this._selectionModel.isEmpty();
            },
            enumerable: false,
            configurable: true
        });
        MatSelect.prototype._initializeSelection = function () {
            var _this = this;
            // Defer setting the value in order to avoid the "Expression
            // has changed after it was checked" errors from Angular.
            Promise.resolve().then(function () {
                _this._setSelectionByValue(_this.ngControl ? _this.ngControl.value : _this._value);
                _this.stateChanges.next();
            });
        };
        /**
         * Sets the selected option based on a value. If no option can be
         * found with the designated value, the select trigger is cleared.
         */
        MatSelect.prototype._setSelectionByValue = function (value) {
            var _this = this;
            if (this.multiple && value) {
                if (!Array.isArray(value)) {
                    throw getMatSelectNonArrayValueError();
                }
                this._selectionModel.clear();
                value.forEach(function (currentValue) { return _this._selectValue(currentValue); });
                this._sortValues();
            }
            else {
                this._selectionModel.clear();
                var correspondingOption = this._selectValue(value);
                // Shift focus to the active item. Note that we shouldn't do this in multiple
                // mode, because we don't know what option the user interacted with last.
                if (correspondingOption) {
                    this._keyManager.setActiveItem(correspondingOption);
                }
                else if (!this.panelOpen) {
                    // Otherwise reset the highlighted option. Note that we only want to do this while
                    // closed, because doing it while open can shift the user's focus unnecessarily.
                    this._keyManager.setActiveItem(-1);
                }
            }
            this._changeDetectorRef.markForCheck();
        };
        /**
         * Finds and selects and option based on its value.
         * @returns Option that has the corresponding value.
         */
        MatSelect.prototype._selectValue = function (value) {
            var _this = this;
            var correspondingOption = this.options.find(function (option) {
                try {
                    // Treat null as a special reset value.
                    return option.value != null && _this._compareWith(option.value, value);
                }
                catch (error) {
                    if (core.isDevMode()) {
                        // Notify developers of errors in their comparator.
                        console.warn(error);
                    }
                    return false;
                }
            });
            if (correspondingOption) {
                this._selectionModel.select(correspondingOption);
            }
            return correspondingOption;
        };
        /** Sets up a key manager to listen to keyboard events on the overlay panel. */
        MatSelect.prototype._initKeyManager = function () {
            var _this = this;
            this._keyManager = new a11y.ActiveDescendantKeyManager(this.options)
                .withTypeAhead(this._typeaheadDebounceInterval)
                .withVerticalOrientation()
                .withHorizontalOrientation(this._isRtl() ? 'rtl' : 'ltr')
                .withAllowedModifierKeys(['shiftKey']);
            this._keyManager.tabOut.pipe(operators.takeUntil(this._destroy)).subscribe(function () {
                if (_this.panelOpen) {
                    // Select the active item when tabbing away. This is consistent with how the native
                    // select behaves. Note that we only want to do this in single selection mode.
                    if (!_this.multiple && _this._keyManager.activeItem) {
                        _this._keyManager.activeItem._selectViaInteraction();
                    }
                    // Restore focus to the trigger before closing. Ensures that the focus
                    // position won't be lost if the user got focus into the overlay.
                    _this.focus();
                    _this.close();
                }
            });
            this._keyManager.change.pipe(operators.takeUntil(this._destroy)).subscribe(function () {
                if (_this._panelOpen && _this.panel) {
                    _this._scrollActiveOptionIntoView();
                }
                else if (!_this._panelOpen && !_this.multiple && _this._keyManager.activeItem) {
                    _this._keyManager.activeItem._selectViaInteraction();
                }
            });
        };
        /** Drops current option subscriptions and IDs and resets from scratch. */
        MatSelect.prototype._resetOptions = function () {
            var _this = this;
            var changedOrDestroyed = rxjs.merge(this.options.changes, this._destroy);
            this.optionSelectionChanges.pipe(operators.takeUntil(changedOrDestroyed)).subscribe(function (event) {
                _this._onSelect(event.source, event.isUserInput);
                if (event.isUserInput && !_this.multiple && _this._panelOpen) {
                    _this.close();
                    _this.focus();
                }
            });
            // Listen to changes in the internal state of the options and react accordingly.
            // Handles cases like the labels of the selected options changing.
            rxjs.merge.apply(void 0, __spread(this.options.map(function (option) { return option._stateChanges; }))).pipe(operators.takeUntil(changedOrDestroyed))
                .subscribe(function () {
                _this._changeDetectorRef.markForCheck();
                _this.stateChanges.next();
            });
            this._setOptionIds();
        };
        /** Invoked when an option is clicked. */
        MatSelect.prototype._onSelect = function (option, isUserInput) {
            var wasSelected = this._selectionModel.isSelected(option);
            if (option.value == null && !this._multiple) {
                option.deselect();
                this._selectionModel.clear();
                this._propagateChanges(option.value);
            }
            else {
                if (wasSelected !== option.selected) {
                    option.selected ? this._selectionModel.select(option) :
                        this._selectionModel.deselect(option);
                }
                if (isUserInput) {
                    this._keyManager.setActiveItem(option);
                }
                if (this.multiple) {
                    this._sortValues();
                    if (isUserInput) {
                        // In case the user selected the option with their mouse, we
                        // want to restore focus back to the trigger, in order to
                        // prevent the select keyboard controls from clashing with
                        // the ones from `mat-option`.
                        this.focus();
                    }
                }
            }
            if (wasSelected !== this._selectionModel.isSelected(option)) {
                this._propagateChanges();
            }
            this.stateChanges.next();
        };
        /** Sorts the selected values in the selected based on their order in the panel. */
        MatSelect.prototype._sortValues = function () {
            var _this = this;
            if (this.multiple) {
                var options_1 = this.options.toArray();
                this._selectionModel.sort(function (a, b) {
                    return _this.sortComparator ? _this.sortComparator(a, b, options_1) :
                        options_1.indexOf(a) - options_1.indexOf(b);
                });
                this.stateChanges.next();
            }
        };
        /** Emits change event to set the model value. */
        MatSelect.prototype._propagateChanges = function (fallbackValue) {
            var valueToEmit = null;
            if (this.multiple) {
                valueToEmit = this.selected.map(function (option) { return option.value; });
            }
            else {
                valueToEmit = this.selected ? this.selected.value : fallbackValue;
            }
            this._value = valueToEmit;
            this.valueChange.emit(valueToEmit);
            this._onChange(valueToEmit);
            this.selectionChange.emit(new MatSelectChange(this, valueToEmit));
            this._changeDetectorRef.markForCheck();
        };
        /** Records option IDs to pass to the aria-owns property. */
        MatSelect.prototype._setOptionIds = function () {
            this._optionIds = this.options.map(function (option) { return option.id; }).join(' ');
        };
        /**
         * Highlights the selected item. If no option is selected, it will highlight
         * the first item instead.
         */
        MatSelect.prototype._highlightCorrectOption = function () {
            if (this._keyManager) {
                if (this.empty) {
                    this._keyManager.setFirstItemActive();
                }
                else {
                    this._keyManager.setActiveItem(this._selectionModel.selected[0]);
                }
            }
        };
        /** Scrolls the active option into view. */
        MatSelect.prototype._scrollActiveOptionIntoView = function () {
            var activeOptionIndex = this._keyManager.activeItemIndex || 0;
            var labelCount = core$1._countGroupLabelsBeforeOption(activeOptionIndex, this.options, this.optionGroups);
            this.panel.nativeElement.scrollTop = core$1._getOptionScrollPosition(activeOptionIndex + labelCount, this._getItemHeight(), this.panel.nativeElement.scrollTop, SELECT_PANEL_MAX_HEIGHT);
        };
        /** Focuses the select element. */
        MatSelect.prototype.focus = function (options) {
            this._elementRef.nativeElement.focus(options);
        };
        /** Gets the index of the provided option in the option list. */
        MatSelect.prototype._getOptionIndex = function (option) {
            return this.options.reduce(function (result, current, index) {
                if (result !== undefined) {
                    return result;
                }
                return option === current ? index : undefined;
            }, undefined);
        };
        /** Calculates the scroll position and x- and y-offsets of the overlay panel. */
        MatSelect.prototype._calculateOverlayPosition = function () {
            var itemHeight = this._getItemHeight();
            var items = this._getItemCount();
            var panelHeight = Math.min(items * itemHeight, SELECT_PANEL_MAX_HEIGHT);
            var scrollContainerHeight = items * itemHeight;
            // The farthest the panel can be scrolled before it hits the bottom
            var maxScroll = scrollContainerHeight - panelHeight;
            // If no value is selected we open the popup to the first item.
            var selectedOptionOffset = this.empty ? 0 : this._getOptionIndex(this._selectionModel.selected[0]);
            selectedOptionOffset += core$1._countGroupLabelsBeforeOption(selectedOptionOffset, this.options, this.optionGroups);
            // We must maintain a scroll buffer so the selected option will be scrolled to the
            // center of the overlay panel rather than the top.
            var scrollBuffer = panelHeight / 2;
            this._scrollTop = this._calculateOverlayScroll(selectedOptionOffset, scrollBuffer, maxScroll);
            this._offsetY = this._calculateOverlayOffsetY(selectedOptionOffset, scrollBuffer, maxScroll);
            this._checkOverlayWithinViewport(maxScroll);
        };
        /**
         * Calculates the scroll position of the select's overlay panel.
         *
         * Attempts to center the selected option in the panel. If the option is
         * too high or too low in the panel to be scrolled to the center, it clamps the
         * scroll position to the min or max scroll positions respectively.
         */
        MatSelect.prototype._calculateOverlayScroll = function (selectedIndex, scrollBuffer, maxScroll) {
            var itemHeight = this._getItemHeight();
            var optionOffsetFromScrollTop = itemHeight * selectedIndex;
            var halfOptionHeight = itemHeight / 2;
            // Starts at the optionOffsetFromScrollTop, which scrolls the option to the top of the
            // scroll container, then subtracts the scroll buffer to scroll the option down to
            // the center of the overlay panel. Half the option height must be re-added to the
            // scrollTop so the option is centered based on its middle, not its top edge.
            var optimalScrollPosition = optionOffsetFromScrollTop - scrollBuffer + halfOptionHeight;
            return Math.min(Math.max(0, optimalScrollPosition), maxScroll);
        };
        /** Returns the aria-label of the select component. */
        MatSelect.prototype._getAriaLabel = function () {
            // If an ariaLabelledby value has been set by the consumer, the select should not overwrite the
            // `aria-labelledby` value by setting the ariaLabel to the placeholder.
            return this.ariaLabelledby ? null : this.ariaLabel || this.placeholder;
        };
        /** Returns the aria-labelledby of the select component. */
        MatSelect.prototype._getAriaLabelledby = function () {
            if (this.ariaLabelledby) {
                return this.ariaLabelledby;
            }
            // Note: we use `_getAriaLabel` here, because we want to check whether there's a
            // computed label. `this.ariaLabel` is only the user-specified label.
            if (!this._parentFormField || !this._parentFormField._hasFloatingLabel() ||
                this._getAriaLabel()) {
                return null;
            }
            return this._parentFormField._labelId || null;
        };
        /** Determines the `aria-activedescendant` to be set on the host. */
        MatSelect.prototype._getAriaActiveDescendant = function () {
            if (this.panelOpen && this._keyManager && this._keyManager.activeItem) {
                return this._keyManager.activeItem.id;
            }
            return null;
        };
        /**
         * Sets the x-offset of the overlay panel in relation to the trigger's top start corner.
         * This must be adjusted to align the selected option text over the trigger text when
         * the panel opens. Will change based on LTR or RTL text direction. Note that the offset
         * can't be calculated until the panel has been attached, because we need to know the
         * content width in order to constrain the panel within the viewport.
         */
        MatSelect.prototype._calculateOverlayOffsetX = function () {
            var overlayRect = this.overlayDir.overlayRef.overlayElement.getBoundingClientRect();
            var viewportSize = this._viewportRuler.getViewportSize();
            var isRtl = this._isRtl();
            var paddingWidth = this.multiple ? SELECT_MULTIPLE_PANEL_PADDING_X + SELECT_PANEL_PADDING_X :
                SELECT_PANEL_PADDING_X * 2;
            var offsetX;
            // Adjust the offset, depending on the option padding.
            if (this.multiple) {
                offsetX = SELECT_MULTIPLE_PANEL_PADDING_X;
            }
            else {
                var selected = this._selectionModel.selected[0] || this.options.first;
                offsetX = selected && selected.group ? SELECT_PANEL_INDENT_PADDING_X : SELECT_PANEL_PADDING_X;
            }
            // Invert the offset in LTR.
            if (!isRtl) {
                offsetX *= -1;
            }
            // Determine how much the select overflows on each side.
            var leftOverflow = 0 - (overlayRect.left + offsetX - (isRtl ? paddingWidth : 0));
            var rightOverflow = overlayRect.right + offsetX - viewportSize.width
                + (isRtl ? 0 : paddingWidth);
            // If the element overflows on either side, reduce the offset to allow it to fit.
            if (leftOverflow > 0) {
                offsetX += leftOverflow + SELECT_PANEL_VIEWPORT_PADDING;
            }
            else if (rightOverflow > 0) {
                offsetX -= rightOverflow + SELECT_PANEL_VIEWPORT_PADDING;
            }
            // Set the offset directly in order to avoid having to go through change detection and
            // potentially triggering "changed after it was checked" errors. Round the value to avoid
            // blurry content in some browsers.
            this.overlayDir.offsetX = Math.round(offsetX);
            this.overlayDir.overlayRef.updatePosition();
        };
        /**
         * Calculates the y-offset of the select's overlay panel in relation to the
         * top start corner of the trigger. It has to be adjusted in order for the
         * selected option to be aligned over the trigger when the panel opens.
         */
        MatSelect.prototype._calculateOverlayOffsetY = function (selectedIndex, scrollBuffer, maxScroll) {
            var itemHeight = this._getItemHeight();
            var optionHeightAdjustment = (itemHeight - this._triggerRect.height) / 2;
            var maxOptionsDisplayed = Math.floor(SELECT_PANEL_MAX_HEIGHT / itemHeight);
            var optionOffsetFromPanelTop;
            // Disable offset if requested by user by returning 0 as value to offset
            if (this._disableOptionCentering) {
                return 0;
            }
            if (this._scrollTop === 0) {
                optionOffsetFromPanelTop = selectedIndex * itemHeight;
            }
            else if (this._scrollTop === maxScroll) {
                var firstDisplayedIndex = this._getItemCount() - maxOptionsDisplayed;
                var selectedDisplayIndex = selectedIndex - firstDisplayedIndex;
                // The first item is partially out of the viewport. Therefore we need to calculate what
                // portion of it is shown in the viewport and account for it in our offset.
                var partialItemHeight = itemHeight - (this._getItemCount() * itemHeight - SELECT_PANEL_MAX_HEIGHT) % itemHeight;
                // Because the panel height is longer than the height of the options alone,
                // there is always extra padding at the top or bottom of the panel. When
                // scrolled to the very bottom, this padding is at the top of the panel and
                // must be added to the offset.
                optionOffsetFromPanelTop = selectedDisplayIndex * itemHeight + partialItemHeight;
            }
            else {
                // If the option was scrolled to the middle of the panel using a scroll buffer,
                // its offset will be the scroll buffer minus the half height that was added to
                // center it.
                optionOffsetFromPanelTop = scrollBuffer - itemHeight / 2;
            }
            // The final offset is the option's offset from the top, adjusted for the height difference,
            // multiplied by -1 to ensure that the overlay moves in the correct direction up the page.
            // The value is rounded to prevent some browsers from blurring the content.
            return Math.round(optionOffsetFromPanelTop * -1 - optionHeightAdjustment);
        };
        /**
         * Checks that the attempted overlay position will fit within the viewport.
         * If it will not fit, tries to adjust the scroll position and the associated
         * y-offset so the panel can open fully on-screen. If it still won't fit,
         * sets the offset back to 0 to allow the fallback position to take over.
         */
        MatSelect.prototype._checkOverlayWithinViewport = function (maxScroll) {
            var itemHeight = this._getItemHeight();
            var viewportSize = this._viewportRuler.getViewportSize();
            var topSpaceAvailable = this._triggerRect.top - SELECT_PANEL_VIEWPORT_PADDING;
            var bottomSpaceAvailable = viewportSize.height - this._triggerRect.bottom - SELECT_PANEL_VIEWPORT_PADDING;
            var panelHeightTop = Math.abs(this._offsetY);
            var totalPanelHeight = Math.min(this._getItemCount() * itemHeight, SELECT_PANEL_MAX_HEIGHT);
            var panelHeightBottom = totalPanelHeight - panelHeightTop - this._triggerRect.height;
            if (panelHeightBottom > bottomSpaceAvailable) {
                this._adjustPanelUp(panelHeightBottom, bottomSpaceAvailable);
            }
            else if (panelHeightTop > topSpaceAvailable) {
                this._adjustPanelDown(panelHeightTop, topSpaceAvailable, maxScroll);
            }
            else {
                this._transformOrigin = this._getOriginBasedOnOption();
            }
        };
        /** Adjusts the overlay panel up to fit in the viewport. */
        MatSelect.prototype._adjustPanelUp = function (panelHeightBottom, bottomSpaceAvailable) {
            // Browsers ignore fractional scroll offsets, so we need to round.
            var distanceBelowViewport = Math.round(panelHeightBottom - bottomSpaceAvailable);
            // Scrolls the panel up by the distance it was extending past the boundary, then
            // adjusts the offset by that amount to move the panel up into the viewport.
            this._scrollTop -= distanceBelowViewport;
            this._offsetY -= distanceBelowViewport;
            this._transformOrigin = this._getOriginBasedOnOption();
            // If the panel is scrolled to the very top, it won't be able to fit the panel
            // by scrolling, so set the offset to 0 to allow the fallback position to take
            // effect.
            if (this._scrollTop <= 0) {
                this._scrollTop = 0;
                this._offsetY = 0;
                this._transformOrigin = "50% bottom 0px";
            }
        };
        /** Adjusts the overlay panel down to fit in the viewport. */
        MatSelect.prototype._adjustPanelDown = function (panelHeightTop, topSpaceAvailable, maxScroll) {
            // Browsers ignore fractional scroll offsets, so we need to round.
            var distanceAboveViewport = Math.round(panelHeightTop - topSpaceAvailable);
            // Scrolls the panel down by the distance it was extending past the boundary, then
            // adjusts the offset by that amount to move the panel down into the viewport.
            this._scrollTop += distanceAboveViewport;
            this._offsetY += distanceAboveViewport;
            this._transformOrigin = this._getOriginBasedOnOption();
            // If the panel is scrolled to the very bottom, it won't be able to fit the
            // panel by scrolling, so set the offset to 0 to allow the fallback position
            // to take effect.
            if (this._scrollTop >= maxScroll) {
                this._scrollTop = maxScroll;
                this._offsetY = 0;
                this._transformOrigin = "50% top 0px";
                return;
            }
        };
        /** Sets the transform origin point based on the selected option. */
        MatSelect.prototype._getOriginBasedOnOption = function () {
            var itemHeight = this._getItemHeight();
            var optionHeightAdjustment = (itemHeight - this._triggerRect.height) / 2;
            var originY = Math.abs(this._offsetY) - optionHeightAdjustment + itemHeight / 2;
            return "50% " + originY + "px 0px";
        };
        /** Calculates the amount of items in the select. This includes options and group labels. */
        MatSelect.prototype._getItemCount = function () {
            return this.options.length + this.optionGroups.length;
        };
        /** Calculates the height of the select's options. */
        MatSelect.prototype._getItemHeight = function () {
            return this._triggerFontSize * SELECT_ITEM_HEIGHT_EM;
        };
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        MatSelect.prototype.setDescribedByIds = function (ids) {
            this._ariaDescribedby = ids.join(' ');
        };
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        MatSelect.prototype.onContainerClick = function () {
            this.focus();
            this.open();
        };
        Object.defineProperty(MatSelect.prototype, "shouldLabelFloat", {
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            get: function () {
                return this._panelOpen || !this.empty;
            },
            enumerable: false,
            configurable: true
        });
        MatSelect.decorators = [
            { type: core.Component, args: [{
                        selector: 'mat-select',
                        exportAs: 'matSelect',
                        template: "<div cdk-overlay-origin\n     class=\"mat-select-trigger\"\n     aria-hidden=\"true\"\n     (click)=\"toggle()\"\n     #origin=\"cdkOverlayOrigin\"\n     #trigger>\n  <div class=\"mat-select-value\" [ngSwitch]=\"empty\">\n    <span class=\"mat-select-placeholder\" *ngSwitchCase=\"true\">{{placeholder || '\\u00A0'}}</span>\n    <span class=\"mat-select-value-text\" *ngSwitchCase=\"false\" [ngSwitch]=\"!!customTrigger\">\n      <span *ngSwitchDefault>{{triggerValue || '\\u00A0'}}</span>\n      <ng-content select=\"mat-select-trigger\" *ngSwitchCase=\"true\"></ng-content>\n    </span>\n  </div>\n\n  <div class=\"mat-select-arrow-wrapper\"><div class=\"mat-select-arrow\"></div></div>\n</div>\n\n<ng-template\n  cdk-connected-overlay\n  cdkConnectedOverlayLockPosition\n  cdkConnectedOverlayHasBackdrop\n  cdkConnectedOverlayBackdropClass=\"cdk-overlay-transparent-backdrop\"\n  [cdkConnectedOverlayScrollStrategy]=\"_scrollStrategy\"\n  [cdkConnectedOverlayOrigin]=\"origin\"\n  [cdkConnectedOverlayOpen]=\"panelOpen\"\n  [cdkConnectedOverlayPositions]=\"_positions\"\n  [cdkConnectedOverlayMinWidth]=\"_triggerRect?.width\"\n  [cdkConnectedOverlayOffsetY]=\"_offsetY\"\n  (backdropClick)=\"close()\"\n  (attach)=\"_onAttached()\"\n  (detach)=\"close()\">\n  <div class=\"mat-select-panel-wrap\" [@transformPanelWrap]>\n    <div\n      #panel\n      [attr.id]=\"id + '-panel'\"\n      class=\"mat-select-panel {{ _getPanelTheme() }}\"\n      [ngClass]=\"panelClass\"\n      [@transformPanel]=\"multiple ? 'showing-multiple' : 'showing'\"\n      (@transformPanel.done)=\"_panelDoneAnimatingStream.next($event.toState)\"\n      [style.transformOrigin]=\"_transformOrigin\"\n      [style.font-size.px]=\"_triggerFontSize\"\n      (keydown)=\"_handleKeydown($event)\">\n      <ng-content></ng-content>\n    </div>\n  </div>\n</ng-template>\n",
                        inputs: ['disabled', 'disableRipple', 'tabIndex'],
                        encapsulation: core.ViewEncapsulation.None,
                        changeDetection: core.ChangeDetectionStrategy.OnPush,
                        host: {
                            'role': 'listbox',
                            '[attr.id]': 'id',
                            '[attr.tabindex]': 'tabIndex',
                            '[attr.aria-label]': '_getAriaLabel()',
                            '[attr.aria-labelledby]': '_getAriaLabelledby()',
                            '[attr.aria-required]': 'required.toString()',
                            '[attr.aria-disabled]': 'disabled.toString()',
                            '[attr.aria-invalid]': 'errorState',
                            '[attr.aria-owns]': 'panelOpen ? _optionIds : null',
                            '[attr.aria-multiselectable]': 'multiple',
                            '[attr.aria-describedby]': '_ariaDescribedby || null',
                            '[attr.aria-activedescendant]': '_getAriaActiveDescendant()',
                            '[class.mat-select-disabled]': 'disabled',
                            '[class.mat-select-invalid]': 'errorState',
                            '[class.mat-select-required]': 'required',
                            '[class.mat-select-empty]': 'empty',
                            'class': 'mat-select',
                            '(keydown)': '_handleKeydown($event)',
                            '(focus)': '_onFocus()',
                            '(blur)': '_onBlur()',
                        },
                        animations: [
                            matSelectAnimations.transformPanelWrap,
                            matSelectAnimations.transformPanel
                        ],
                        providers: [
                            { provide: formField.MatFormFieldControl, useExisting: MatSelect },
                            { provide: core$1.MAT_OPTION_PARENT_COMPONENT, useExisting: MatSelect }
                        ],
                        styles: [".mat-select{display:inline-block;width:100%;outline:none}.mat-select-trigger{display:inline-table;cursor:pointer;position:relative;box-sizing:border-box}.mat-select-disabled .mat-select-trigger{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;cursor:default}.mat-select-value{display:table-cell;max-width:0;width:100%;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.mat-select-value-text{white-space:nowrap;overflow:hidden;text-overflow:ellipsis}.mat-select-arrow-wrapper{display:table-cell;vertical-align:middle}.mat-form-field-appearance-fill .mat-select-arrow-wrapper{transform:translateY(-50%)}.mat-form-field-appearance-outline .mat-select-arrow-wrapper{transform:translateY(-25%)}.mat-form-field-appearance-standard.mat-form-field-has-label .mat-select:not(.mat-select-empty) .mat-select-arrow-wrapper{transform:translateY(-50%)}.mat-form-field-appearance-standard .mat-select.mat-select-empty .mat-select-arrow-wrapper{transition:transform 400ms cubic-bezier(0.25, 0.8, 0.25, 1)}._mat-animation-noopable.mat-form-field-appearance-standard .mat-select.mat-select-empty .mat-select-arrow-wrapper{transition:none}.mat-select-arrow{width:0;height:0;border-left:5px solid transparent;border-right:5px solid transparent;border-top:5px solid;margin:0 4px}.mat-select-panel-wrap{flex-basis:100%}.mat-select-panel{min-width:112px;max-width:280px;overflow:auto;-webkit-overflow-scrolling:touch;padding-top:0;padding-bottom:0;max-height:256px;min-width:100%;border-radius:4px}.cdk-high-contrast-active .mat-select-panel{outline:solid 1px}.mat-select-panel .mat-optgroup-label,.mat-select-panel .mat-option{font-size:inherit;line-height:3em;height:3em}.mat-form-field-type-mat-select:not(.mat-form-field-disabled) .mat-form-field-flex{cursor:pointer}.mat-form-field-type-mat-select .mat-form-field-label{width:calc(100% - 18px)}.mat-select-placeholder{transition:color 400ms 133.3333333333ms cubic-bezier(0.25, 0.8, 0.25, 1)}._mat-animation-noopable .mat-select-placeholder{transition:none}.mat-form-field-hide-placeholder .mat-select-placeholder{color:transparent;-webkit-text-fill-color:transparent;transition:none;display:block}\n"]
                    },] }
        ];
        MatSelect.ctorParameters = function () { return [
            { type: scrolling.ViewportRuler },
            { type: core.ChangeDetectorRef },
            { type: core.NgZone },
            { type: core$1.ErrorStateMatcher },
            { type: core.ElementRef },
            { type: bidi.Directionality, decorators: [{ type: core.Optional }] },
            { type: forms.NgForm, decorators: [{ type: core.Optional }] },
            { type: forms.FormGroupDirective, decorators: [{ type: core.Optional }] },
            { type: formField.MatFormField, decorators: [{ type: core.Optional }, { type: core.Inject, args: [formField.MAT_FORM_FIELD,] }] },
            { type: forms.NgControl, decorators: [{ type: core.Self }, { type: core.Optional }] },
            { type: String, decorators: [{ type: core.Attribute, args: ['tabindex',] }] },
            { type: undefined, decorators: [{ type: core.Inject, args: [MAT_SELECT_SCROLL_STRATEGY,] }] },
            { type: a11y.LiveAnnouncer },
            { type: undefined, decorators: [{ type: core.Optional }, { type: core.Inject, args: [MAT_SELECT_CONFIG,] }] }
        ]; };
        MatSelect.propDecorators = {
            trigger: [{ type: core.ViewChild, args: ['trigger',] }],
            panel: [{ type: core.ViewChild, args: ['panel',] }],
            overlayDir: [{ type: core.ViewChild, args: [overlay.CdkConnectedOverlay,] }],
            options: [{ type: core.ContentChildren, args: [core$1.MatOption, { descendants: true },] }],
            optionGroups: [{ type: core.ContentChildren, args: [core$1.MatOptgroup, { descendants: true },] }],
            panelClass: [{ type: core.Input }],
            customTrigger: [{ type: core.ContentChild, args: [MatSelectTrigger,] }],
            placeholder: [{ type: core.Input }],
            required: [{ type: core.Input }],
            multiple: [{ type: core.Input }],
            disableOptionCentering: [{ type: core.Input }],
            compareWith: [{ type: core.Input }],
            value: [{ type: core.Input }],
            ariaLabel: [{ type: core.Input, args: ['aria-label',] }],
            ariaLabelledby: [{ type: core.Input, args: ['aria-labelledby',] }],
            errorStateMatcher: [{ type: core.Input }],
            typeaheadDebounceInterval: [{ type: core.Input }],
            sortComparator: [{ type: core.Input }],
            id: [{ type: core.Input }],
            openedChange: [{ type: core.Output }],
            _openedStream: [{ type: core.Output, args: ['opened',] }],
            _closedStream: [{ type: core.Output, args: ['closed',] }],
            selectionChange: [{ type: core.Output }],
            valueChange: [{ type: core.Output }]
        };
        return MatSelect;
    }(_MatSelectMixinBase));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    var MatSelectModule = /** @class */ (function () {
        function MatSelectModule() {
        }
        MatSelectModule.decorators = [
            { type: core.NgModule, args: [{
                        imports: [
                            common.CommonModule,
                            overlay.OverlayModule,
                            core$1.MatOptionModule,
                            core$1.MatCommonModule,
                        ],
                        exports: [
                            scrolling.CdkScrollableModule,
                            formField.MatFormFieldModule,
                            MatSelect,
                            MatSelectTrigger,
                            core$1.MatOptionModule,
                            core$1.MatCommonModule
                        ],
                        declarations: [MatSelect, MatSelectTrigger],
                        providers: [MAT_SELECT_SCROLL_STRATEGY_PROVIDER]
                    },] }
        ];
        return MatSelectModule;
    }());

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */

    /**
     * Generated bundle index. Do not edit.
     */

    exports.MAT_SELECT_CONFIG = MAT_SELECT_CONFIG;
    exports.MAT_SELECT_SCROLL_STRATEGY = MAT_SELECT_SCROLL_STRATEGY;
    exports.MAT_SELECT_SCROLL_STRATEGY_PROVIDER = MAT_SELECT_SCROLL_STRATEGY_PROVIDER;
    exports.MAT_SELECT_SCROLL_STRATEGY_PROVIDER_FACTORY = MAT_SELECT_SCROLL_STRATEGY_PROVIDER_FACTORY;
    exports.MatSelect = MatSelect;
    exports.MatSelectChange = MatSelectChange;
    exports.MatSelectModule = MatSelectModule;
    exports.MatSelectTrigger = MatSelectTrigger;
    exports.SELECT_ITEM_HEIGHT_EM = SELECT_ITEM_HEIGHT_EM;
    exports.SELECT_MULTIPLE_PANEL_PADDING_X = SELECT_MULTIPLE_PANEL_PADDING_X;
    exports.SELECT_PANEL_INDENT_PADDING_X = SELECT_PANEL_INDENT_PADDING_X;
    exports.SELECT_PANEL_MAX_HEIGHT = SELECT_PANEL_MAX_HEIGHT;
    exports.SELECT_PANEL_PADDING_X = SELECT_PANEL_PADDING_X;
    exports.SELECT_PANEL_VIEWPORT_PADDING = SELECT_PANEL_VIEWPORT_PADDING;
    exports.matSelectAnimations = matSelectAnimations;

    Object.defineProperty(exports, '__esModule', { value: true });

})));
//# sourceMappingURL=material-select.umd.js.map
