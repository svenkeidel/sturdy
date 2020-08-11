(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('@angular/cdk/tree'), require('@angular/core'), require('@angular/material/core'), require('@angular/cdk/coercion'), require('@angular/cdk/collections'), require('rxjs'), require('rxjs/operators')) :
    typeof define === 'function' && define.amd ? define('@angular/material/tree', ['exports', '@angular/cdk/tree', '@angular/core', '@angular/material/core', '@angular/cdk/coercion', '@angular/cdk/collections', 'rxjs', 'rxjs/operators'], factory) :
    (global = global || self, factory((global.ng = global.ng || {}, global.ng.material = global.ng.material || {}, global.ng.material.tree = {}), global.ng.cdk.tree, global.ng.core, global.ng.material.core, global.ng.cdk.coercion, global.ng.cdk.collections, global.rxjs, global.rxjs.operators));
}(this, (function (exports, tree, core, core$1, coercion, collections, rxjs, operators) { 'use strict';

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
    var _MatTreeNodeMixinBase = core$1.mixinTabIndex(core$1.mixinDisabled(tree.CdkTreeNode));
    /**
     * Wrapper for the CdkTree node with Material design styles.
     */
    var MatTreeNode = /** @class */ (function (_super) {
        __extends(MatTreeNode, _super);
        function MatTreeNode(_elementRef, _tree, tabIndex) {
            var _this = _super.call(this, _elementRef, _tree) || this;
            _this._elementRef = _elementRef;
            _this._tree = _tree;
            _this.role = 'treeitem';
            _this.tabIndex = Number(tabIndex) || 0;
            return _this;
        }
        MatTreeNode.decorators = [
            { type: core.Directive, args: [{
                        selector: 'mat-tree-node',
                        exportAs: 'matTreeNode',
                        inputs: ['disabled', 'tabIndex'],
                        host: {
                            '[attr.aria-expanded]': 'isExpanded',
                            '[attr.aria-level]': 'role === "treeitem" ? level : null',
                            '[attr.role]': 'role',
                            'class': 'mat-tree-node'
                        },
                        providers: [{ provide: tree.CdkTreeNode, useExisting: MatTreeNode }]
                    },] }
        ];
        MatTreeNode.ctorParameters = function () { return [
            { type: core.ElementRef },
            { type: tree.CdkTree },
            { type: String, decorators: [{ type: core.Attribute, args: ['tabindex',] }] }
        ]; };
        MatTreeNode.propDecorators = {
            role: [{ type: core.Input }]
        };
        return MatTreeNode;
    }(_MatTreeNodeMixinBase));
    /**
     * Wrapper for the CdkTree node definition with Material design styles.
     */
    var MatTreeNodeDef = /** @class */ (function (_super) {
        __extends(MatTreeNodeDef, _super);
        function MatTreeNodeDef() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        MatTreeNodeDef.decorators = [
            { type: core.Directive, args: [{
                        selector: '[matTreeNodeDef]',
                        inputs: [
                            'when: matTreeNodeDefWhen'
                        ],
                        providers: [{ provide: tree.CdkTreeNodeDef, useExisting: MatTreeNodeDef }]
                    },] }
        ];
        MatTreeNodeDef.propDecorators = {
            data: [{ type: core.Input, args: ['matTreeNode',] }]
        };
        return MatTreeNodeDef;
    }(tree.CdkTreeNodeDef));
    /**
     * Wrapper for the CdkTree nested node with Material design styles.
     */
    var MatNestedTreeNode = /** @class */ (function (_super) {
        __extends(MatNestedTreeNode, _super);
        function MatNestedTreeNode(_elementRef, _tree, _differs, tabIndex) {
            var _this = _super.call(this, _elementRef, _tree, _differs) || this;
            _this._elementRef = _elementRef;
            _this._tree = _tree;
            _this._differs = _differs;
            _this._disabled = false;
            _this.tabIndex = Number(tabIndex) || 0;
            return _this;
        }
        Object.defineProperty(MatNestedTreeNode.prototype, "disabled", {
            /** Whether the node is disabled. */
            get: function () { return this._disabled; },
            set: function (value) { this._disabled = coercion.coerceBooleanProperty(value); },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(MatNestedTreeNode.prototype, "tabIndex", {
            /** Tabindex for the node. */
            get: function () { return this.disabled ? -1 : this._tabIndex; },
            set: function (value) {
                // If the specified tabIndex value is null or undefined, fall back to the default value.
                this._tabIndex = value != null ? value : 0;
            },
            enumerable: false,
            configurable: true
        });
        // This is a workaround for https://github.com/angular/angular/issues/23091
        // In aot mode, the lifecycle hooks from parent class are not called.
        // TODO(tinayuangao): Remove when the angular issue #23091 is fixed
        MatNestedTreeNode.prototype.ngAfterContentInit = function () {
            _super.prototype.ngAfterContentInit.call(this);
        };
        MatNestedTreeNode.prototype.ngOnDestroy = function () {
            _super.prototype.ngOnDestroy.call(this);
        };
        MatNestedTreeNode.decorators = [
            { type: core.Directive, args: [{
                        selector: 'mat-nested-tree-node',
                        exportAs: 'matNestedTreeNode',
                        host: {
                            '[attr.aria-expanded]': 'isExpanded',
                            '[attr.role]': 'role',
                            'class': 'mat-nested-tree-node',
                        },
                        providers: [
                            { provide: tree.CdkNestedTreeNode, useExisting: MatNestedTreeNode },
                            { provide: tree.CdkTreeNode, useExisting: MatNestedTreeNode },
                            { provide: tree.CDK_TREE_NODE_OUTLET_NODE, useExisting: MatNestedTreeNode }
                        ]
                    },] }
        ];
        MatNestedTreeNode.ctorParameters = function () { return [
            { type: core.ElementRef },
            { type: tree.CdkTree },
            { type: core.IterableDiffers },
            { type: String, decorators: [{ type: core.Attribute, args: ['tabindex',] }] }
        ]; };
        MatNestedTreeNode.propDecorators = {
            node: [{ type: core.Input, args: ['matNestedTreeNode',] }],
            disabled: [{ type: core.Input }],
            tabIndex: [{ type: core.Input }]
        };
        return MatNestedTreeNode;
    }(tree.CdkNestedTreeNode));

    /**
     * Wrapper for the CdkTree padding with Material design styles.
     */
    var MatTreeNodePadding = /** @class */ (function (_super) {
        __extends(MatTreeNodePadding, _super);
        function MatTreeNodePadding() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        MatTreeNodePadding.decorators = [
            { type: core.Directive, args: [{
                        selector: '[matTreeNodePadding]',
                        providers: [{ provide: tree.CdkTreeNodePadding, useExisting: MatTreeNodePadding }]
                    },] }
        ];
        MatTreeNodePadding.propDecorators = {
            level: [{ type: core.Input, args: ['matTreeNodePadding',] }],
            indent: [{ type: core.Input, args: ['matTreeNodePaddingIndent',] }]
        };
        return MatTreeNodePadding;
    }(tree.CdkTreeNodePadding));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /**
     * Outlet for nested CdkNode. Put `[matTreeNodeOutlet]` on a tag to place children dataNodes
     * inside the outlet.
     */
    var MatTreeNodeOutlet = /** @class */ (function () {
        function MatTreeNodeOutlet(viewContainer, _node) {
            this.viewContainer = viewContainer;
            this._node = _node;
        }
        MatTreeNodeOutlet.decorators = [
            { type: core.Directive, args: [{
                        selector: '[matTreeNodeOutlet]',
                        providers: [{
                                provide: tree.CdkTreeNodeOutlet,
                                useExisting: MatTreeNodeOutlet
                            }]
                    },] }
        ];
        MatTreeNodeOutlet.ctorParameters = function () { return [
            { type: core.ViewContainerRef },
            { type: undefined, decorators: [{ type: core.Inject, args: [tree.CDK_TREE_NODE_OUTLET_NODE,] }, { type: core.Optional }] }
        ]; };
        return MatTreeNodeOutlet;
    }());

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /**
     * Wrapper for the CdkTable with Material design styles.
     */
    var MatTree = /** @class */ (function (_super) {
        __extends(MatTree, _super);
        function MatTree() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        MatTree.decorators = [
            { type: core.Component, args: [{
                        selector: 'mat-tree',
                        exportAs: 'matTree',
                        template: "<ng-container matTreeNodeOutlet></ng-container>",
                        host: {
                            'class': 'mat-tree',
                            'role': 'tree',
                        },
                        encapsulation: core.ViewEncapsulation.None,
                        // See note on CdkTree for explanation on why this uses the default change detection strategy.
                        // tslint:disable-next-line:validate-decorators
                        changeDetection: core.ChangeDetectionStrategy.Default,
                        providers: [{ provide: tree.CdkTree, useExisting: MatTree }],
                        styles: [".mat-tree{display:block}.mat-tree-node{display:flex;align-items:center;flex:1;word-wrap:break-word}.mat-nested-tree-node{border-bottom-width:0}\n"]
                    },] }
        ];
        MatTree.propDecorators = {
            _nodeOutlet: [{ type: core.ViewChild, args: [MatTreeNodeOutlet, { static: true },] }]
        };
        return MatTree;
    }(tree.CdkTree));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /**
     * Wrapper for the CdkTree's toggle with Material design styles.
     */
    var MatTreeNodeToggle = /** @class */ (function (_super) {
        __extends(MatTreeNodeToggle, _super);
        function MatTreeNodeToggle() {
            var _this = _super !== null && _super.apply(this, arguments) || this;
            _this.recursive = false;
            return _this;
        }
        MatTreeNodeToggle.decorators = [
            { type: core.Directive, args: [{
                        selector: '[matTreeNodeToggle]',
                        providers: [{ provide: tree.CdkTreeNodeToggle, useExisting: MatTreeNodeToggle }]
                    },] }
        ];
        MatTreeNodeToggle.propDecorators = {
            recursive: [{ type: core.Input, args: ['matTreeNodeToggleRecursive',] }]
        };
        return MatTreeNodeToggle;
    }(tree.CdkTreeNodeToggle));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    var MAT_TREE_DIRECTIVES = [
        MatNestedTreeNode,
        MatTreeNodeDef,
        MatTreeNodePadding,
        MatTreeNodeToggle,
        MatTree,
        MatTreeNode,
        MatTreeNodeOutlet
    ];
    var MatTreeModule = /** @class */ (function () {
        function MatTreeModule() {
        }
        MatTreeModule.decorators = [
            { type: core.NgModule, args: [{
                        imports: [tree.CdkTreeModule, core$1.MatCommonModule],
                        exports: [core$1.MatCommonModule, MAT_TREE_DIRECTIVES],
                        declarations: MAT_TREE_DIRECTIVES,
                    },] }
        ];
        return MatTreeModule;
    }());

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /**
     * Tree flattener to convert a normal type of node to node with children & level information.
     * Transform nested nodes of type `T` to flattened nodes of type `F`.
     *
     * For example, the input data of type `T` is nested, and contains its children data:
     *   SomeNode: {
     *     key: 'Fruits',
     *     children: [
     *       NodeOne: {
     *         key: 'Apple',
     *       },
     *       NodeTwo: {
     *        key: 'Pear',
     *      }
     *    ]
     *  }
     *  After flattener flatten the tree, the structure will become
     *  SomeNode: {
     *    key: 'Fruits',
     *    expandable: true,
     *    level: 1
     *  },
     *  NodeOne: {
     *    key: 'Apple',
     *    expandable: false,
     *    level: 2
     *  },
     *  NodeTwo: {
     *   key: 'Pear',
     *   expandable: false,
     *   level: 2
     * }
     * and the output flattened type is `F` with additional information.
     */
    var MatTreeFlattener = /** @class */ (function () {
        function MatTreeFlattener(transformFunction, getLevel, isExpandable, getChildren) {
            this.transformFunction = transformFunction;
            this.getLevel = getLevel;
            this.isExpandable = isExpandable;
            this.getChildren = getChildren;
        }
        MatTreeFlattener.prototype._flattenNode = function (node, level, resultNodes, parentMap) {
            var _this = this;
            var flatNode = this.transformFunction(node, level);
            resultNodes.push(flatNode);
            if (this.isExpandable(flatNode)) {
                var childrenNodes = this.getChildren(node);
                if (childrenNodes) {
                    if (Array.isArray(childrenNodes)) {
                        this._flattenChildren(childrenNodes, level, resultNodes, parentMap);
                    }
                    else {
                        childrenNodes.pipe(operators.take(1)).subscribe(function (children) {
                            _this._flattenChildren(children, level, resultNodes, parentMap);
                        });
                    }
                }
            }
            return resultNodes;
        };
        MatTreeFlattener.prototype._flattenChildren = function (children, level, resultNodes, parentMap) {
            var _this = this;
            children.forEach(function (child, index) {
                var childParentMap = parentMap.slice();
                childParentMap.push(index != children.length - 1);
                _this._flattenNode(child, level + 1, resultNodes, childParentMap);
            });
        };
        /**
         * Flatten a list of node type T to flattened version of node F.
         * Please note that type T may be nested, and the length of `structuredData` may be different
         * from that of returned list `F[]`.
         */
        MatTreeFlattener.prototype.flattenNodes = function (structuredData) {
            var _this = this;
            var resultNodes = [];
            structuredData.forEach(function (node) { return _this._flattenNode(node, 0, resultNodes, []); });
            return resultNodes;
        };
        /**
         * Expand flattened node with current expansion status.
         * The returned list may have different length.
         */
        MatTreeFlattener.prototype.expandFlattenedNodes = function (nodes, treeControl) {
            var _this = this;
            var results = [];
            var currentExpand = [];
            currentExpand[0] = true;
            nodes.forEach(function (node) {
                var expand = true;
                for (var i = 0; i <= _this.getLevel(node); i++) {
                    expand = expand && currentExpand[i];
                }
                if (expand) {
                    results.push(node);
                }
                if (_this.isExpandable(node)) {
                    currentExpand[_this.getLevel(node) + 1] = treeControl.isExpanded(node);
                }
            });
            return results;
        };
        return MatTreeFlattener;
    }());
    /**
     * Data source for flat tree.
     * The data source need to handle expansion/collapsion of the tree node and change the data feed
     * to `MatTree`.
     * The nested tree nodes of type `T` are flattened through `MatTreeFlattener`, and converted
     * to type `F` for `MatTree` to consume.
     */
    var MatTreeFlatDataSource = /** @class */ (function (_super) {
        __extends(MatTreeFlatDataSource, _super);
        function MatTreeFlatDataSource(_treeControl, _treeFlattener, initialData) {
            if (initialData === void 0) { initialData = []; }
            var _this = _super.call(this) || this;
            _this._treeControl = _treeControl;
            _this._treeFlattener = _treeFlattener;
            _this._flattenedData = new rxjs.BehaviorSubject([]);
            _this._expandedData = new rxjs.BehaviorSubject([]);
            _this._data = new rxjs.BehaviorSubject(initialData);
            return _this;
        }
        Object.defineProperty(MatTreeFlatDataSource.prototype, "data", {
            get: function () { return this._data.value; },
            set: function (value) {
                this._data.next(value);
                this._flattenedData.next(this._treeFlattener.flattenNodes(this.data));
                this._treeControl.dataNodes = this._flattenedData.value;
            },
            enumerable: false,
            configurable: true
        });
        MatTreeFlatDataSource.prototype.connect = function (collectionViewer) {
            var _this = this;
            var changes = [
                collectionViewer.viewChange,
                this._treeControl.expansionModel.changed,
                this._flattenedData
            ];
            return rxjs.merge.apply(void 0, __spread(changes)).pipe(operators.map(function () {
                _this._expandedData.next(_this._treeFlattener.expandFlattenedNodes(_this._flattenedData.value, _this._treeControl));
                return _this._expandedData.value;
            }));
        };
        MatTreeFlatDataSource.prototype.disconnect = function () {
            // no op
        };
        return MatTreeFlatDataSource;
    }(collections.DataSource));

    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    /**
     * Data source for nested tree.
     *
     * The data source for nested tree doesn't have to consider node flattener, or the way to expand
     * or collapse. The expansion/collapsion will be handled by TreeControl and each non-leaf node.
     */
    var MatTreeNestedDataSource = /** @class */ (function (_super) {
        __extends(MatTreeNestedDataSource, _super);
        function MatTreeNestedDataSource() {
            var _this = _super !== null && _super.apply(this, arguments) || this;
            _this._data = new rxjs.BehaviorSubject([]);
            return _this;
        }
        Object.defineProperty(MatTreeNestedDataSource.prototype, "data", {
            /**
             * Data for the nested tree
             */
            get: function () { return this._data.value; },
            set: function (value) { this._data.next(value); },
            enumerable: false,
            configurable: true
        });
        MatTreeNestedDataSource.prototype.connect = function (collectionViewer) {
            var _this = this;
            return rxjs.merge.apply(void 0, __spread([collectionViewer.viewChange, this._data])).pipe(operators.map(function () {
                return _this.data;
            }));
        };
        MatTreeNestedDataSource.prototype.disconnect = function () {
            // no op
        };
        return MatTreeNestedDataSource;
    }(collections.DataSource));

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

    exports.MatNestedTreeNode = MatNestedTreeNode;
    exports.MatTree = MatTree;
    exports.MatTreeFlatDataSource = MatTreeFlatDataSource;
    exports.MatTreeFlattener = MatTreeFlattener;
    exports.MatTreeModule = MatTreeModule;
    exports.MatTreeNestedDataSource = MatTreeNestedDataSource;
    exports.MatTreeNode = MatTreeNode;
    exports.MatTreeNodeDef = MatTreeNodeDef;
    exports.MatTreeNodeOutlet = MatTreeNodeOutlet;
    exports.MatTreeNodePadding = MatTreeNodePadding;
    exports.MatTreeNodeToggle = MatTreeNodeToggle;

    Object.defineProperty(exports, '__esModule', { value: true });

})));
//# sourceMappingURL=material-tree.umd.js.map
