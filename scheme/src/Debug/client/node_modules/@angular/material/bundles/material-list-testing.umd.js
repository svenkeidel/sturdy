(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('@angular/cdk/testing'), require('@angular/material/divider/testing')) :
    typeof define === 'function' && define.amd ? define('@angular/material/list/testing', ['exports', '@angular/cdk/testing', '@angular/material/divider/testing'], factory) :
    (global = global || self, factory((global.ng = global.ng || {}, global.ng.material = global.ng.material || {}, global.ng.material.list = global.ng.material.list || {}, global.ng.material.list.testing = {}), global.ng.cdk.testing, global.ng.material.divider.testing));
}(this, (function (exports, testing, testing$1) { 'use strict';

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
     * Gets a `HarnessPredicate` that applies the given `BaseListItemHarnessFilters` to the given
     * list item harness.
     * @template H The type of list item harness to create a predicate for.
     * @param harnessType A constructor for a list item harness.
     * @param options An instance of `BaseListItemHarnessFilters` to apply.
     * @return A `HarnessPredicate` for the given harness type with the given options applied.
     */
    function getListItemPredicate(harnessType, options) {
        return new testing.HarnessPredicate(harnessType, options)
            .addOption('text', options.text, function (harness, text) { return testing.HarnessPredicate.stringMatches(harness.getText(), text); });
    }
    /** Harness for interacting with a list subheader. */
    var MatSubheaderHarness = /** @class */ (function (_super) {
        __extends(MatSubheaderHarness, _super);
        function MatSubheaderHarness() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        MatSubheaderHarness.with = function (options) {
            if (options === void 0) { options = {}; }
            return new testing.HarnessPredicate(MatSubheaderHarness, options)
                .addOption('text', options.text, function (harness, text) { return testing.HarnessPredicate.stringMatches(harness.getText(), text); });
        };
        /** Gets the full text content of the list item (including text from any font icons). */
        MatSubheaderHarness.prototype.getText = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).text()];
                    }
                });
            });
        };
        MatSubheaderHarness.hostSelector = '[mat-subheader], [matSubheader]';
        return MatSubheaderHarness;
    }(testing.ComponentHarness));
    /**
     * Shared behavior among the harnesses for the various `MatListItem` flavors.
     * @docs-private
     */
    var MatListItemHarnessBase = /** @class */ (function (_super) {
        __extends(MatListItemHarnessBase, _super);
        function MatListItemHarnessBase() {
            var _this = _super !== null && _super.apply(this, arguments) || this;
            _this._lines = _this.locatorForAll('[mat-line], [matLine]');
            _this._avatar = _this.locatorForOptional('[mat-list-avatar], [matListAvatar]');
            _this._icon = _this.locatorForOptional('[mat-list-icon], [matListIcon]');
            return _this;
        }
        /** Gets the full text content of the list item (including text from any font icons). */
        MatListItemHarnessBase.prototype.getText = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).text()];
                    }
                });
            });
        };
        /** Gets the lines of text (`mat-line` elements) in this nav list item. */
        MatListItemHarnessBase.prototype.getLinesText = function () {
            return __awaiter(this, void 0, void 0, function () {
                var _a, _b;
                return __generator(this, function (_c) {
                    switch (_c.label) {
                        case 0:
                            _b = (_a = Promise).all;
                            return [4 /*yield*/, this._lines()];
                        case 1: return [2 /*return*/, _b.apply(_a, [(_c.sent()).map(function (l) { return l.text(); })])];
                    }
                });
            });
        };
        /** Whether this list item has an avatar. */
        MatListItemHarnessBase.prototype.hasAvatar = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this._avatar()];
                        case 1: return [2 /*return*/, !!(_a.sent())];
                    }
                });
            });
        };
        /** Whether this list item has an icon. */
        MatListItemHarnessBase.prototype.hasIcon = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this._icon()];
                        case 1: return [2 /*return*/, !!(_a.sent())];
                    }
                });
            });
        };
        /** Gets a `HarnessLoader` used to get harnesses within the list item's content. */
        MatListItemHarnessBase.prototype.getHarnessLoaderForContent = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    return [2 /*return*/, this.locatorFactory.harnessLoaderFor('.mat-list-item-content')];
                });
            });
        };
        return MatListItemHarnessBase;
    }(testing.ComponentHarness));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /**
     * Shared behavior among the harnesses for the various `MatList` flavors.
     * @template T A constructor type for a list item harness type used by this list harness.
     * @template C The list item harness type that `T` constructs.
     * @template F The filter type used filter list item harness of type `C`.
     * @docs-private
     */
    var MatListHarnessBase = /** @class */ (function (_super) {
        __extends(MatListHarnessBase, _super);
        function MatListHarnessBase() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        /**
         * Gets a list of harnesses representing the items in this list.
         * @param filters Optional filters used to narrow which harnesses are included
         * @return The list of items matching the given filters.
         */
        MatListHarnessBase.prototype.getItems = function (filters) {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    return [2 /*return*/, this.locatorForAll(this._itemHarness.with(filters))()];
                });
            });
        };
        /**
         * Gets a list of `ListSection` representing the list items grouped by subheaders. If the list has
         * no subheaders it is represented as a single `ListSection` with an undefined `heading` property.
         * @param filters Optional filters used to narrow which list item harnesses are included
         * @return The list of items matching the given filters, grouped into sections by subheader.
         */
        MatListHarnessBase.prototype.getItemsGroupedBySubheader = function (filters) {
            return __awaiter(this, void 0, void 0, function () {
                var listSections, currentSection, itemsAndSubheaders, itemsAndSubheaders_1, itemsAndSubheaders_1_1, itemOrSubheader, _a, e_1_1;
                var e_1, _b;
                return __generator(this, function (_c) {
                    switch (_c.label) {
                        case 0:
                            listSections = [];
                            currentSection = { items: [] };
                            return [4 /*yield*/, this.getItemsWithSubheadersAndDividers({ item: filters, divider: false })];
                        case 1:
                            itemsAndSubheaders = _c.sent();
                            _c.label = 2;
                        case 2:
                            _c.trys.push([2, 8, 9, 10]);
                            itemsAndSubheaders_1 = __values(itemsAndSubheaders), itemsAndSubheaders_1_1 = itemsAndSubheaders_1.next();
                            _c.label = 3;
                        case 3:
                            if (!!itemsAndSubheaders_1_1.done) return [3 /*break*/, 7];
                            itemOrSubheader = itemsAndSubheaders_1_1.value;
                            if (!(itemOrSubheader instanceof MatSubheaderHarness)) return [3 /*break*/, 5];
                            if (currentSection.heading !== undefined || currentSection.items.length) {
                                listSections.push(currentSection);
                            }
                            _a = {};
                            return [4 /*yield*/, itemOrSubheader.getText()];
                        case 4:
                            currentSection = (_a.heading = _c.sent(), _a.items = [], _a);
                            return [3 /*break*/, 6];
                        case 5:
                            currentSection.items.push(itemOrSubheader);
                            _c.label = 6;
                        case 6:
                            itemsAndSubheaders_1_1 = itemsAndSubheaders_1.next();
                            return [3 /*break*/, 3];
                        case 7: return [3 /*break*/, 10];
                        case 8:
                            e_1_1 = _c.sent();
                            e_1 = { error: e_1_1 };
                            return [3 /*break*/, 10];
                        case 9:
                            try {
                                if (itemsAndSubheaders_1_1 && !itemsAndSubheaders_1_1.done && (_b = itemsAndSubheaders_1.return)) _b.call(itemsAndSubheaders_1);
                            }
                            finally { if (e_1) throw e_1.error; }
                            return [7 /*endfinally*/];
                        case 10:
                            if (currentSection.heading !== undefined || currentSection.items.length ||
                                !listSections.length) {
                                listSections.push(currentSection);
                            }
                            return [2 /*return*/, listSections];
                    }
                });
            });
        };
        /**
         * Gets a list of sub-lists representing the list items grouped by dividers. If the list has no
         * dividers it is represented as a list with a single sub-list.
         * @param filters Optional filters used to narrow which list item harnesses are included
         * @return The list of items matching the given filters, grouped into sub-lists by divider.
         */
        MatListHarnessBase.prototype.getItemsGroupedByDividers = function (filters) {
            return __awaiter(this, void 0, void 0, function () {
                var listSections, itemsAndDividers, itemsAndDividers_1, itemsAndDividers_1_1, itemOrDivider;
                var e_2, _a;
                return __generator(this, function (_b) {
                    switch (_b.label) {
                        case 0:
                            listSections = [[]];
                            return [4 /*yield*/, this.getItemsWithSubheadersAndDividers({ item: filters, subheader: false })];
                        case 1:
                            itemsAndDividers = _b.sent();
                            try {
                                for (itemsAndDividers_1 = __values(itemsAndDividers), itemsAndDividers_1_1 = itemsAndDividers_1.next(); !itemsAndDividers_1_1.done; itemsAndDividers_1_1 = itemsAndDividers_1.next()) {
                                    itemOrDivider = itemsAndDividers_1_1.value;
                                    if (itemOrDivider instanceof testing$1.MatDividerHarness) {
                                        listSections.push([]);
                                    }
                                    else {
                                        listSections[listSections.length - 1].push(itemOrDivider);
                                    }
                                }
                            }
                            catch (e_2_1) { e_2 = { error: e_2_1 }; }
                            finally {
                                try {
                                    if (itemsAndDividers_1_1 && !itemsAndDividers_1_1.done && (_a = itemsAndDividers_1.return)) _a.call(itemsAndDividers_1);
                                }
                                finally { if (e_2) throw e_2.error; }
                            }
                            return [2 /*return*/, listSections];
                    }
                });
            });
        };
        MatListHarnessBase.prototype.getItemsWithSubheadersAndDividers = function (filters) {
            if (filters === void 0) { filters = {}; }
            return __awaiter(this, void 0, void 0, function () {
                var query;
                return __generator(this, function (_a) {
                    query = [];
                    if (filters.item !== false) {
                        query.push(this._itemHarness.with(filters.item || {}));
                    }
                    if (filters.subheader !== false) {
                        query.push(MatSubheaderHarness.with(filters.subheader));
                    }
                    if (filters.divider !== false) {
                        query.push(testing$1.MatDividerHarness.with(filters.divider));
                    }
                    return [2 /*return*/, this.locatorForAll.apply(this, __spread(query))()];
                });
            });
        };
        return MatListHarnessBase;
    }(testing.ComponentHarness));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /** Harness for interacting with a standard mat-action-list in tests. */
    var MatActionListHarness = /** @class */ (function (_super) {
        __extends(MatActionListHarness, _super);
        function MatActionListHarness() {
            var _this = _super !== null && _super.apply(this, arguments) || this;
            _this._itemHarness = MatActionListItemHarness;
            return _this;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatActionListHarness` that meets
         * certain criteria.
         * @param options Options for filtering which action list instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        MatActionListHarness.with = function (options) {
            if (options === void 0) { options = {}; }
            return new testing.HarnessPredicate(MatActionListHarness, options);
        };
        /** The selector for the host element of a `MatActionList` instance. */
        MatActionListHarness.hostSelector = 'mat-action-list';
        return MatActionListHarness;
    }(MatListHarnessBase));
    /** Harness for interacting with an action list item. */
    var MatActionListItemHarness = /** @class */ (function (_super) {
        __extends(MatActionListItemHarness, _super);
        function MatActionListItemHarness() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatActionListItemHarness` that
         * meets certain criteria.
         * @param options Options for filtering which action list item instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        MatActionListItemHarness.with = function (options) {
            if (options === void 0) { options = {}; }
            return getListItemPredicate(MatActionListItemHarness, options);
        };
        /** Clicks on the action list item. */
        MatActionListItemHarness.prototype.click = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).click()];
                    }
                });
            });
        };
        /** Focuses the action list item. */
        MatActionListItemHarness.prototype.focus = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).focus()];
                    }
                });
            });
        };
        /** Blurs the action list item. */
        MatActionListItemHarness.prototype.blur = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).blur()];
                    }
                });
            });
        };
        /** The selector for the host element of a `MatListItem` instance. */
        MatActionListItemHarness.hostSelector = ['mat-list-item', 'a[mat-list-item]', 'button[mat-list-item]']
            .map(function (selector) { return MatActionListHarness.hostSelector + " " + selector; })
            .join(',');
        return MatActionListItemHarness;
    }(MatListItemHarnessBase));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /** Harness for interacting with a standard mat-list in tests. */
    var MatListHarness = /** @class */ (function (_super) {
        __extends(MatListHarness, _super);
        function MatListHarness() {
            var _this = _super !== null && _super.apply(this, arguments) || this;
            _this._itemHarness = MatListItemHarness;
            return _this;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatListHarness` that meets certain
         * criteria.
         * @param options Options for filtering which list instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        MatListHarness.with = function (options) {
            if (options === void 0) { options = {}; }
            return new testing.HarnessPredicate(MatListHarness, options);
        };
        /** The selector for the host element of a `MatList` instance. */
        MatListHarness.hostSelector = 'mat-list';
        return MatListHarness;
    }(MatListHarnessBase));
    /** Harness for interacting with a list item. */
    var MatListItemHarness = /** @class */ (function (_super) {
        __extends(MatListItemHarness, _super);
        function MatListItemHarness() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatListItemHarness` that meets
         * certain criteria.
         * @param options Options for filtering which list item instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        MatListItemHarness.with = function (options) {
            if (options === void 0) { options = {}; }
            return getListItemPredicate(MatListItemHarness, options);
        };
        /** The selector for the host element of a `MatListItem` instance. */
        MatListItemHarness.hostSelector = ['mat-list-item', 'a[mat-list-item]', 'button[mat-list-item]']
            .map(function (selector) { return MatListHarness.hostSelector + " " + selector; })
            .join(',');
        return MatListItemHarness;
    }(MatListItemHarnessBase));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /** Harness for interacting with a standard mat-nav-list in tests. */
    var MatNavListHarness = /** @class */ (function (_super) {
        __extends(MatNavListHarness, _super);
        function MatNavListHarness() {
            var _this = _super !== null && _super.apply(this, arguments) || this;
            _this._itemHarness = MatNavListItemHarness;
            return _this;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatNavListHarness` that meets
         * certain criteria.
         * @param options Options for filtering which nav list instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        MatNavListHarness.with = function (options) {
            if (options === void 0) { options = {}; }
            return new testing.HarnessPredicate(MatNavListHarness, options);
        };
        /** The selector for the host element of a `MatNavList` instance. */
        MatNavListHarness.hostSelector = 'mat-nav-list';
        return MatNavListHarness;
    }(MatListHarnessBase));
    /** Harness for interacting with a nav list item. */
    var MatNavListItemHarness = /** @class */ (function (_super) {
        __extends(MatNavListItemHarness, _super);
        function MatNavListItemHarness() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatNavListItemHarness` that
         * meets certain criteria.
         * @param options Options for filtering which nav list item instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        MatNavListItemHarness.with = function (options) {
            var _this = this;
            if (options === void 0) { options = {}; }
            return getListItemPredicate(MatNavListItemHarness, options)
                .addOption('href', options.href, function (harness, href) { return __awaiter(_this, void 0, void 0, function () { return __generator(this, function (_a) {
                return [2 /*return*/, testing.HarnessPredicate.stringMatches(harness.getHref(), href)];
            }); }); });
        };
        /** Gets the href for this nav list item. */
        MatNavListItemHarness.prototype.getHref = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).getAttribute('href')];
                    }
                });
            });
        };
        /** Clicks on the nav list item. */
        MatNavListItemHarness.prototype.click = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).click()];
                    }
                });
            });
        };
        /** Focuses the nav list item. */
        MatNavListItemHarness.prototype.focus = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).focus()];
                    }
                });
            });
        };
        /** Blurs the nav list item. */
        MatNavListItemHarness.prototype.blur = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).blur()];
                    }
                });
            });
        };
        /** The selector for the host element of a `MatListItem` instance. */
        MatNavListItemHarness.hostSelector = ['mat-list-item', 'a[mat-list-item]', 'button[mat-list-item]']
            .map(function (selector) { return MatNavListHarness.hostSelector + " " + selector; })
            .join(',');
        return MatNavListItemHarness;
    }(MatListItemHarnessBase));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /** Harness for interacting with a standard mat-selection-list in tests. */
    var MatSelectionListHarness = /** @class */ (function (_super) {
        __extends(MatSelectionListHarness, _super);
        function MatSelectionListHarness() {
            var _this = _super !== null && _super.apply(this, arguments) || this;
            _this._itemHarness = MatListOptionHarness;
            return _this;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatSelectionListHarness` that meets
         * certain criteria.
         * @param options Options for filtering which selection list instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        MatSelectionListHarness.with = function (options) {
            if (options === void 0) { options = {}; }
            return new testing.HarnessPredicate(MatSelectionListHarness, options);
        };
        /** Whether the selection list is disabled. */
        MatSelectionListHarness.prototype.isDisabled = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [4 /*yield*/, (_a.sent()).getAttribute('aria-disabled')];
                        case 2: return [2 /*return*/, (_a.sent()) === 'true'];
                    }
                });
            });
        };
        /**
         * Selects all items matching any of the given filters.
         * @param filters Filters that specify which items should be selected.
         */
        MatSelectionListHarness.prototype.selectItems = function () {
            var filters = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                filters[_i] = arguments[_i];
            }
            return __awaiter(this, void 0, void 0, function () {
                var items;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this._getItems(filters)];
                        case 1:
                            items = _a.sent();
                            return [4 /*yield*/, Promise.all(items.map(function (item) { return item.select(); }))];
                        case 2:
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        };
        /**
         * Deselects all items matching any of the given filters.
         * @param filters Filters that specify which items should be deselected.
         */
        MatSelectionListHarness.prototype.deselectItems = function () {
            var filters = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                filters[_i] = arguments[_i];
            }
            return __awaiter(this, void 0, void 0, function () {
                var items;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this._getItems(filters)];
                        case 1:
                            items = _a.sent();
                            return [4 /*yield*/, Promise.all(items.map(function (item) { return item.deselect(); }))];
                        case 2:
                            _a.sent();
                            return [2 /*return*/];
                    }
                });
            });
        };
        /** Gets all items matching the given list of filters. */
        MatSelectionListHarness.prototype._getItems = function (filters) {
            return __awaiter(this, void 0, void 0, function () {
                var _a, _b, _c;
                var _d;
                var _this = this;
                return __generator(this, function (_e) {
                    switch (_e.label) {
                        case 0:
                            if (!filters.length) {
                                return [2 /*return*/, this.getItems()];
                            }
                            _b = (_a = (_d = []).concat).apply;
                            _c = [_d];
                            return [4 /*yield*/, Promise.all(filters.map(function (filter) { return _this.locatorForAll(MatListOptionHarness.with(filter))(); }))];
                        case 1: return [2 /*return*/, _b.apply(_a, _c.concat([__spread.apply(void 0, [_e.sent()])]))];
                    }
                });
            });
        };
        /** The selector for the host element of a `MatSelectionList` instance. */
        MatSelectionListHarness.hostSelector = 'mat-selection-list';
        return MatSelectionListHarness;
    }(MatListHarnessBase));
    /** Harness for interacting with a list option. */
    var MatListOptionHarness = /** @class */ (function (_super) {
        __extends(MatListOptionHarness, _super);
        function MatListOptionHarness() {
            var _this = _super !== null && _super.apply(this, arguments) || this;
            _this._itemContent = _this.locatorFor('.mat-list-item-content');
            return _this;
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatListOptionHarness` that
         * meets certain criteria.
         * @param options Options for filtering which list option instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        MatListOptionHarness.with = function (options) {
            var _this = this;
            if (options === void 0) { options = {}; }
            return getListItemPredicate(MatListOptionHarness, options)
                .addOption('is selected', options.selected, function (harness, selected) { return __awaiter(_this, void 0, void 0, function () { return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, harness.isSelected()];
                    case 1: return [2 /*return*/, (_a.sent()) === selected];
                }
            }); }); });
        };
        /** Gets the position of the checkbox relative to the list option content. */
        MatListOptionHarness.prototype.getCheckboxPosition = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this._itemContent()];
                        case 1: return [4 /*yield*/, (_a.sent()).hasClass('mat-list-item-content-reverse')];
                        case 2: return [2 /*return*/, (_a.sent()) ?
                                'after' : 'before'];
                    }
                });
            });
        };
        /** Whether the list option is selected. */
        MatListOptionHarness.prototype.isSelected = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [4 /*yield*/, (_a.sent()).getAttribute('aria-selected')];
                        case 2: return [2 /*return*/, (_a.sent()) === 'true'];
                    }
                });
            });
        };
        /** Whether the list option is disabled. */
        MatListOptionHarness.prototype.isDisabled = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [4 /*yield*/, (_a.sent()).getAttribute('aria-disabled')];
                        case 2: return [2 /*return*/, (_a.sent()) === 'true'];
                    }
                });
            });
        };
        /** Focuses the list option. */
        MatListOptionHarness.prototype.focus = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).focus()];
                    }
                });
            });
        };
        /** Blurs the list option. */
        MatListOptionHarness.prototype.blur = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).blur()];
                    }
                });
            });
        };
        /** Toggles the checked state of the checkbox. */
        MatListOptionHarness.prototype.toggle = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.host()];
                        case 1: return [2 /*return*/, (_a.sent()).click()];
                    }
                });
            });
        };
        /**
         * Puts the list option in a checked state by toggling it if it is currently unchecked, or doing
         * nothing if it is already checked.
         */
        MatListOptionHarness.prototype.select = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.isSelected()];
                        case 1:
                            if (!(_a.sent())) {
                                return [2 /*return*/, this.toggle()];
                            }
                            return [2 /*return*/];
                    }
                });
            });
        };
        /**
         * Puts the list option in an unchecked state by toggling it if it is currently checked, or doing
         * nothing if it is already unchecked.
         */
        MatListOptionHarness.prototype.deselect = function () {
            return __awaiter(this, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0: return [4 /*yield*/, this.isSelected()];
                        case 1:
                            if (_a.sent()) {
                                return [2 /*return*/, this.toggle()];
                            }
                            return [2 /*return*/];
                    }
                });
            });
        };
        /** The selector for the host element of a `MatListOption` instance. */
        MatListOptionHarness.hostSelector = 'mat-list-option';
        return MatListOptionHarness;
    }(MatListItemHarnessBase));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */

    exports.MatActionListHarness = MatActionListHarness;
    exports.MatActionListItemHarness = MatActionListItemHarness;
    exports.MatListHarness = MatListHarness;
    exports.MatListItemHarness = MatListItemHarness;
    exports.MatListOptionHarness = MatListOptionHarness;
    exports.MatNavListHarness = MatNavListHarness;
    exports.MatNavListItemHarness = MatNavListItemHarness;
    exports.MatSelectionListHarness = MatSelectionListHarness;

    Object.defineProperty(exports, '__esModule', { value: true });

})));
//# sourceMappingURL=material-list-testing.umd.js.map
