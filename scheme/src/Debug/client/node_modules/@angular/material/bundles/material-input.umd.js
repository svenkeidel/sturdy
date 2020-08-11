(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('@angular/cdk/text-field'), require('@angular/core'), require('@angular/cdk/coercion'), require('@angular/cdk/platform'), require('@angular/forms'), require('@angular/material/core'), require('@angular/material/form-field'), require('rxjs')) :
    typeof define === 'function' && define.amd ? define('@angular/material/input', ['exports', '@angular/cdk/text-field', '@angular/core', '@angular/cdk/coercion', '@angular/cdk/platform', '@angular/forms', '@angular/material/core', '@angular/material/form-field', 'rxjs'], factory) :
    (global = global || self, factory((global.ng = global.ng || {}, global.ng.material = global.ng.material || {}, global.ng.material.input = {}), global.ng.cdk.textField, global.ng.core, global.ng.cdk.coercion, global.ng.cdk.platform, global.ng.forms, global.ng.material.core, global.ng.material.formField, global.rxjs));
}(this, (function (exports, textField, core, coercion, platform, forms, core$1, formField, rxjs) { 'use strict';

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
     * Directive to automatically resize a textarea to fit its content.
     * @deprecated Use `cdkTextareaAutosize` from `@angular/cdk/text-field` instead.
     * @breaking-change 8.0.0
     */
    var MatTextareaAutosize = /** @class */ (function (_super) {
        __extends(MatTextareaAutosize, _super);
        function MatTextareaAutosize() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        Object.defineProperty(MatTextareaAutosize.prototype, "matAutosizeMinRows", {
            get: function () { return this.minRows; },
            set: function (value) { this.minRows = value; },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatTextareaAutosize.prototype, "matAutosizeMaxRows", {
            get: function () { return this.maxRows; },
            set: function (value) { this.maxRows = value; },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatTextareaAutosize.prototype, "matAutosize", {
            get: function () { return this.enabled; },
            set: function (value) { this.enabled = value; },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatTextareaAutosize.prototype, "matTextareaAutosize", {
            get: function () { return this.enabled; },
            set: function (value) { this.enabled = value; },
            enumerable: false,
            configurable: true
        });
        MatTextareaAutosize.decorators = [
            { type: core.Directive, args: [{
                        selector: 'textarea[mat-autosize], textarea[matTextareaAutosize]',
                        exportAs: 'matTextareaAutosize',
                        inputs: ['cdkAutosizeMinRows', 'cdkAutosizeMaxRows'],
                        host: {
                            'class': 'cdk-textarea-autosize mat-autosize',
                            // Textarea elements that have the directive applied should have a single row by default.
                            // Browsers normally show two rows by default and therefore this limits the minRows binding.
                            'rows': '1',
                        },
                    },] }
        ];
        MatTextareaAutosize.propDecorators = {
            matAutosizeMinRows: [{ type: core.Input }],
            matAutosizeMaxRows: [{ type: core.Input }],
            matAutosize: [{ type: core.Input, args: ['mat-autosize',] }],
            matTextareaAutosize: [{ type: core.Input }]
        };
        return MatTextareaAutosize;
    }(textField.CdkTextareaAutosize));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /** @docs-private */
    function getMatInputUnsupportedTypeError(type) {
        return Error("Input type \"" + type + "\" isn't supported by matInput.");
    }

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /**
     * This token is used to inject the object whose value should be set into `MatInput`. If none is
     * provided, the native `HTMLInputElement` is used. Directives like `MatDatepickerInput` can provide
     * themselves for this token, in order to make `MatInput` delegate the getting and setting of the
     * value to them.
     */
    var MAT_INPUT_VALUE_ACCESSOR = new core.InjectionToken('MAT_INPUT_VALUE_ACCESSOR');

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    // Invalid input type. Using one of these will throw an MatInputUnsupportedTypeError.
    var MAT_INPUT_INVALID_TYPES = [
        'button',
        'checkbox',
        'file',
        'hidden',
        'image',
        'radio',
        'range',
        'reset',
        'submit'
    ];
    var nextUniqueId = 0;
    // Boilerplate for applying mixins to MatInput.
    /** @docs-private */
    var MatInputBase = /** @class */ (function () {
        function MatInputBase(_defaultErrorStateMatcher, _parentForm, _parentFormGroup, 
        /** @docs-private */
        ngControl) {
            this._defaultErrorStateMatcher = _defaultErrorStateMatcher;
            this._parentForm = _parentForm;
            this._parentFormGroup = _parentFormGroup;
            this.ngControl = ngControl;
        }
        return MatInputBase;
    }());
    var _MatInputMixinBase = core$1.mixinErrorState(MatInputBase);
    /** Directive that allows a native input to work inside a `MatFormField`. */
    var MatInput = /** @class */ (function (_super) {
        __extends(MatInput, _super);
        function MatInput(_elementRef, _platform, 
        /** @docs-private */
        ngControl, _parentForm, _parentFormGroup, _defaultErrorStateMatcher, inputValueAccessor, _autofillMonitor, ngZone) {
            var _this = _super.call(this, _defaultErrorStateMatcher, _parentForm, _parentFormGroup, ngControl) || this;
            _this._elementRef = _elementRef;
            _this._platform = _platform;
            _this.ngControl = ngControl;
            _this._autofillMonitor = _autofillMonitor;
            _this._uid = "mat-input-" + nextUniqueId++;
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            _this.focused = false;
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            _this.stateChanges = new rxjs.Subject();
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            _this.controlType = 'mat-input';
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            _this.autofilled = false;
            _this._disabled = false;
            _this._required = false;
            _this._type = 'text';
            _this._readonly = false;
            _this._neverEmptyInputTypes = [
                'date',
                'datetime',
                'datetime-local',
                'month',
                'time',
                'week'
            ].filter(function (t) { return platform.getSupportedInputTypes().has(t); });
            var element = _this._elementRef.nativeElement;
            var nodeName = element.nodeName.toLowerCase();
            // If no input value accessor was explicitly specified, use the element as the input value
            // accessor.
            _this._inputValueAccessor = inputValueAccessor || element;
            _this._previousNativeValue = _this.value;
            // Force setter to be called in case id was not specified.
            _this.id = _this.id;
            // On some versions of iOS the caret gets stuck in the wrong place when holding down the delete
            // key. In order to get around this we need to "jiggle" the caret loose. Since this bug only
            // exists on iOS, we only bother to install the listener on iOS.
            if (_platform.IOS) {
                ngZone.runOutsideAngular(function () {
                    _elementRef.nativeElement.addEventListener('keyup', function (event) {
                        var el = event.target;
                        if (!el.value && !el.selectionStart && !el.selectionEnd) {
                            // Note: Just setting `0, 0` doesn't fix the issue. Setting
                            // `1, 1` fixes it for the first time that you type text and
                            // then hold delete. Toggling to `1, 1` and then back to
                            // `0, 0` seems to completely fix it.
                            el.setSelectionRange(1, 1);
                            el.setSelectionRange(0, 0);
                        }
                    });
                });
            }
            _this._isServer = !_this._platform.isBrowser;
            _this._isNativeSelect = nodeName === 'select';
            _this._isTextarea = nodeName === 'textarea';
            if (_this._isNativeSelect) {
                _this.controlType = element.multiple ? 'mat-native-select-multiple' :
                    'mat-native-select';
            }
            return _this;
        }
        Object.defineProperty(MatInput.prototype, "disabled", {
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            get: function () {
                if (this.ngControl && this.ngControl.disabled !== null) {
                    return this.ngControl.disabled;
                }
                return this._disabled;
            },
            set: function (value) {
                this._disabled = coercion.coerceBooleanProperty(value);
                // Browsers may not fire the blur event if the input is disabled too quickly.
                // Reset from here to ensure that the element doesn't become stuck.
                if (this.focused) {
                    this.focused = false;
                    this.stateChanges.next();
                }
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatInput.prototype, "id", {
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            get: function () { return this._id; },
            set: function (value) { this._id = value || this._uid; },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatInput.prototype, "required", {
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            get: function () { return this._required; },
            set: function (value) { this._required = coercion.coerceBooleanProperty(value); },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatInput.prototype, "type", {
            /** Input type of the element. */
            get: function () { return this._type; },
            set: function (value) {
                this._type = value || 'text';
                this._validateType();
                // When using Angular inputs, developers are no longer able to set the properties on the native
                // input element. To ensure that bindings for `type` work, we need to sync the setter
                // with the native property. Textarea elements don't support the type property or attribute.
                if (!this._isTextarea && platform.getSupportedInputTypes().has(this._type)) {
                    this._elementRef.nativeElement.type = this._type;
                }
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatInput.prototype, "value", {
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            get: function () { return this._inputValueAccessor.value; },
            set: function (value) {
                if (value !== this.value) {
                    this._inputValueAccessor.value = value;
                    this.stateChanges.next();
                }
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatInput.prototype, "readonly", {
            /** Whether the element is readonly. */
            get: function () { return this._readonly; },
            set: function (value) { this._readonly = coercion.coerceBooleanProperty(value); },
            enumerable: false,
            configurable: true
        });
        MatInput.prototype.ngAfterViewInit = function () {
            var _this = this;
            if (this._platform.isBrowser) {
                this._autofillMonitor.monitor(this._elementRef.nativeElement).subscribe(function (event) {
                    _this.autofilled = event.isAutofilled;
                    _this.stateChanges.next();
                });
            }
        };
        MatInput.prototype.ngOnChanges = function () {
            this.stateChanges.next();
        };
        MatInput.prototype.ngOnDestroy = function () {
            this.stateChanges.complete();
            if (this._platform.isBrowser) {
                this._autofillMonitor.stopMonitoring(this._elementRef.nativeElement);
            }
        };
        MatInput.prototype.ngDoCheck = function () {
            if (this.ngControl) {
                // We need to re-evaluate this on every change detection cycle, because there are some
                // error triggers that we can't subscribe to (e.g. parent form submissions). This means
                // that whatever logic is in here has to be super lean or we risk destroying the performance.
                this.updateErrorState();
            }
            // We need to dirty-check the native element's value, because there are some cases where
            // we won't be notified when it changes (e.g. the consumer isn't using forms or they're
            // updating the value using `emitEvent: false`).
            this._dirtyCheckNativeValue();
        };
        /** Focuses the input. */
        MatInput.prototype.focus = function (options) {
            this._elementRef.nativeElement.focus(options);
        };
        // We have to use a `HostListener` here in order to support both Ivy and ViewEngine.
        // In Ivy the `host` bindings will be merged when this class is extended, whereas in
        // ViewEngine they're overwritten.
        // TODO(crisbeto): we move this back into `host` once Ivy is turned on by default.
        /** Callback for the cases where the focused state of the input changes. */
        // tslint:disable:no-host-decorator-in-concrete
        // tslint:enable:no-host-decorator-in-concrete
        MatInput.prototype._focusChanged = function (isFocused) {
            if (isFocused !== this.focused && (!this.readonly || !isFocused)) {
                this.focused = isFocused;
                this.stateChanges.next();
            }
        };
        // We have to use a `HostListener` here in order to support both Ivy and ViewEngine.
        // In Ivy the `host` bindings will be merged when this class is extended, whereas in
        // ViewEngine they're overwritten.
        // TODO(crisbeto): we move this back into `host` once Ivy is turned on by default.
        // tslint:disable-next-line:no-host-decorator-in-concrete
        MatInput.prototype._onInput = function () {
            // This is a noop function and is used to let Angular know whenever the value changes.
            // Angular will run a new change detection each time the `input` event has been dispatched.
            // It's necessary that Angular recognizes the value change, because when floatingLabel
            // is set to false and Angular forms aren't used, the placeholder won't recognize the
            // value changes and will not disappear.
            // Listening to the input event wouldn't be necessary when the input is using the
            // FormsModule or ReactiveFormsModule, because Angular forms also listens to input events.
        };
        /** Does some manual dirty checking on the native input `value` property. */
        MatInput.prototype._dirtyCheckNativeValue = function () {
            var newValue = this._elementRef.nativeElement.value;
            if (this._previousNativeValue !== newValue) {
                this._previousNativeValue = newValue;
                this.stateChanges.next();
            }
        };
        /** Make sure the input is a supported type. */
        MatInput.prototype._validateType = function () {
            if (MAT_INPUT_INVALID_TYPES.indexOf(this._type) > -1) {
                throw getMatInputUnsupportedTypeError(this._type);
            }
        };
        /** Checks whether the input type is one of the types that are never empty. */
        MatInput.prototype._isNeverEmpty = function () {
            return this._neverEmptyInputTypes.indexOf(this._type) > -1;
        };
        /** Checks whether the input is invalid based on the native validation. */
        MatInput.prototype._isBadInput = function () {
            // The `validity` property won't be present on platform-server.
            var validity = this._elementRef.nativeElement.validity;
            return validity && validity.badInput;
        };
        Object.defineProperty(MatInput.prototype, "empty", {
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            get: function () {
                return !this._isNeverEmpty() && !this._elementRef.nativeElement.value && !this._isBadInput() &&
                    !this.autofilled;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatInput.prototype, "shouldLabelFloat", {
            /**
             * Implemented as part of MatFormFieldControl.
             * @docs-private
             */
            get: function () {
                if (this._isNativeSelect) {
                    // For a single-selection `<select>`, the label should float when the selected option has
                    // a non-empty display value. For a `<select multiple>`, the label *always* floats to avoid
                    // overlapping the label with the options.
                    var selectElement = this._elementRef.nativeElement;
                    var firstOption = selectElement.options[0];
                    // On most browsers the `selectedIndex` will always be 0, however on IE and Edge it'll be
                    // -1 if the `value` is set to something, that isn't in the list of options, at a later point.
                    return this.focused || selectElement.multiple || !this.empty ||
                        !!(selectElement.selectedIndex > -1 && firstOption && firstOption.label);
                }
                else {
                    return this.focused || !this.empty;
                }
            },
            enumerable: false,
            configurable: true
        });
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        MatInput.prototype.setDescribedByIds = function (ids) {
            this._ariaDescribedby = ids.join(' ');
        };
        /**
         * Implemented as part of MatFormFieldControl.
         * @docs-private
         */
        MatInput.prototype.onContainerClick = function () {
            // Do not re-focus the input element if the element is already focused. Otherwise it can happen
            // that someone clicks on a time input and the cursor resets to the "hours" field while the
            // "minutes" field was actually clicked. See: https://github.com/angular/components/issues/12849
            if (!this.focused) {
                this.focus();
            }
        };
        MatInput.decorators = [
            { type: core.Directive, args: [{
                        selector: "input[matInput], textarea[matInput], select[matNativeControl],\n      input[matNativeControl], textarea[matNativeControl]",
                        exportAs: 'matInput',
                        host: {
                            /**
                             * @breaking-change 8.0.0 remove .mat-form-field-autofill-control in favor of AutofillMonitor.
                             */
                            'class': 'mat-input-element mat-form-field-autofill-control',
                            '[class.mat-input-server]': '_isServer',
                            // Native input properties that are overwritten by Angular inputs need to be synced with
                            // the native input element. Otherwise property bindings for those don't work.
                            '[attr.id]': 'id',
                            '[attr.placeholder]': 'placeholder',
                            '[disabled]': 'disabled',
                            '[required]': 'required',
                            '[attr.readonly]': 'readonly && !_isNativeSelect || null',
                            '[attr.aria-describedby]': '_ariaDescribedby || null',
                            '[attr.aria-invalid]': 'errorState',
                            '[attr.aria-required]': 'required.toString()',
                        },
                        providers: [{ provide: formField.MatFormFieldControl, useExisting: MatInput }],
                    },] }
        ];
        MatInput.ctorParameters = function () { return [
            { type: core.ElementRef },
            { type: platform.Platform },
            { type: forms.NgControl, decorators: [{ type: core.Optional }, { type: core.Self }] },
            { type: forms.NgForm, decorators: [{ type: core.Optional }] },
            { type: forms.FormGroupDirective, decorators: [{ type: core.Optional }] },
            { type: core$1.ErrorStateMatcher },
            { type: undefined, decorators: [{ type: core.Optional }, { type: core.Self }, { type: core.Inject, args: [MAT_INPUT_VALUE_ACCESSOR,] }] },
            { type: textField.AutofillMonitor },
            { type: core.NgZone }
        ]; };
        MatInput.propDecorators = {
            disabled: [{ type: core.Input }],
            id: [{ type: core.Input }],
            placeholder: [{ type: core.Input }],
            required: [{ type: core.Input }],
            type: [{ type: core.Input }],
            errorStateMatcher: [{ type: core.Input }],
            value: [{ type: core.Input }],
            readonly: [{ type: core.Input }],
            _focusChanged: [{ type: core.HostListener, args: ['focus', ['true'],] }, { type: core.HostListener, args: ['blur', ['false'],] }],
            _onInput: [{ type: core.HostListener, args: ['input',] }]
        };
        return MatInput;
    }(_MatInputMixinBase));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    var MatInputModule = /** @class */ (function () {
        function MatInputModule() {
        }
        MatInputModule.decorators = [
            { type: core.NgModule, args: [{
                        declarations: [MatInput, MatTextareaAutosize],
                        imports: [
                            textField.TextFieldModule,
                            formField.MatFormFieldModule,
                        ],
                        exports: [
                            textField.TextFieldModule,
                            // We re-export the `MatFormFieldModule` since `MatInput` will almost always
                            // be used together with `MatFormField`.
                            formField.MatFormFieldModule,
                            MatInput,
                            MatTextareaAutosize,
                        ],
                        providers: [core$1.ErrorStateMatcher],
                    },] }
        ];
        return MatInputModule;
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

    exports.MAT_INPUT_VALUE_ACCESSOR = MAT_INPUT_VALUE_ACCESSOR;
    exports.MatInput = MatInput;
    exports.MatInputModule = MatInputModule;
    exports.MatTextareaAutosize = MatTextareaAutosize;
    exports.getMatInputUnsupportedTypeError = getMatInputUnsupportedTypeError;

    Object.defineProperty(exports, '__esModule', { value: true });

})));
//# sourceMappingURL=material-input.umd.js.map
