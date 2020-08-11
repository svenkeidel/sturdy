/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define("@angular/compiler/src/aot/static_reflector", ["require", "exports", "tslib", "@angular/compiler/src/compile_metadata", "@angular/compiler/src/core", "@angular/compiler/src/util", "@angular/compiler/src/aot/formatted_error", "@angular/compiler/src/aot/static_symbol"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.StaticReflector = void 0;
    var tslib_1 = require("tslib");
    var compile_metadata_1 = require("@angular/compiler/src/compile_metadata");
    var core_1 = require("@angular/compiler/src/core");
    var util_1 = require("@angular/compiler/src/util");
    var formatted_error_1 = require("@angular/compiler/src/aot/formatted_error");
    var static_symbol_1 = require("@angular/compiler/src/aot/static_symbol");
    var ANGULAR_CORE = '@angular/core';
    var ANGULAR_ROUTER = '@angular/router';
    var HIDDEN_KEY = /^\$.*\$$/;
    var IGNORE = {
        __symbolic: 'ignore'
    };
    var USE_VALUE = 'useValue';
    var PROVIDE = 'provide';
    var REFERENCE_SET = new Set([USE_VALUE, 'useFactory', 'data', 'id', 'loadChildren']);
    var TYPEGUARD_POSTFIX = 'TypeGuard';
    var USE_IF = 'UseIf';
    function shouldIgnore(value) {
        return value && value.__symbolic == 'ignore';
    }
    /**
     * A static reflector implements enough of the Reflector API that is necessary to compile
     * templates statically.
     */
    var StaticReflector = /** @class */ (function () {
        function StaticReflector(summaryResolver, symbolResolver, knownMetadataClasses, knownMetadataFunctions, errorRecorder) {
            var _this = this;
            if (knownMetadataClasses === void 0) { knownMetadataClasses = []; }
            if (knownMetadataFunctions === void 0) { knownMetadataFunctions = []; }
            this.summaryResolver = summaryResolver;
            this.symbolResolver = symbolResolver;
            this.errorRecorder = errorRecorder;
            this.annotationCache = new Map();
            this.shallowAnnotationCache = new Map();
            this.propertyCache = new Map();
            this.parameterCache = new Map();
            this.methodCache = new Map();
            this.staticCache = new Map();
            this.conversionMap = new Map();
            this.resolvedExternalReferences = new Map();
            this.annotationForParentClassWithSummaryKind = new Map();
            this.initializeConversionMap();
            knownMetadataClasses.forEach(function (kc) { return _this._registerDecoratorOrConstructor(_this.getStaticSymbol(kc.filePath, kc.name), kc.ctor); });
            knownMetadataFunctions.forEach(function (kf) { return _this._registerFunction(_this.getStaticSymbol(kf.filePath, kf.name), kf.fn); });
            this.annotationForParentClassWithSummaryKind.set(compile_metadata_1.CompileSummaryKind.Directive, [core_1.createDirective, core_1.createComponent]);
            this.annotationForParentClassWithSummaryKind.set(compile_metadata_1.CompileSummaryKind.Pipe, [core_1.createPipe]);
            this.annotationForParentClassWithSummaryKind.set(compile_metadata_1.CompileSummaryKind.NgModule, [core_1.createNgModule]);
            this.annotationForParentClassWithSummaryKind.set(compile_metadata_1.CompileSummaryKind.Injectable, [core_1.createInjectable, core_1.createPipe, core_1.createDirective, core_1.createComponent, core_1.createNgModule]);
        }
        StaticReflector.prototype.componentModuleUrl = function (typeOrFunc) {
            var staticSymbol = this.findSymbolDeclaration(typeOrFunc);
            return this.symbolResolver.getResourcePath(staticSymbol);
        };
        /**
         * Invalidate the specified `symbols` on program change.
         * @param symbols
         */
        StaticReflector.prototype.invalidateSymbols = function (symbols) {
            var e_1, _a;
            try {
                for (var symbols_1 = tslib_1.__values(symbols), symbols_1_1 = symbols_1.next(); !symbols_1_1.done; symbols_1_1 = symbols_1.next()) {
                    var symbol = symbols_1_1.value;
                    this.annotationCache.delete(symbol);
                    this.shallowAnnotationCache.delete(symbol);
                    this.propertyCache.delete(symbol);
                    this.parameterCache.delete(symbol);
                    this.methodCache.delete(symbol);
                    this.staticCache.delete(symbol);
                    this.conversionMap.delete(symbol);
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (symbols_1_1 && !symbols_1_1.done && (_a = symbols_1.return)) _a.call(symbols_1);
                }
                finally { if (e_1) throw e_1.error; }
            }
        };
        StaticReflector.prototype.resolveExternalReference = function (ref, containingFile) {
            var key = undefined;
            if (!containingFile) {
                key = ref.moduleName + ":" + ref.name;
                var declarationSymbol_1 = this.resolvedExternalReferences.get(key);
                if (declarationSymbol_1)
                    return declarationSymbol_1;
            }
            var refSymbol = this.symbolResolver.getSymbolByModule(ref.moduleName, ref.name, containingFile);
            var declarationSymbol = this.findSymbolDeclaration(refSymbol);
            if (!containingFile) {
                this.symbolResolver.recordModuleNameForFileName(refSymbol.filePath, ref.moduleName);
                this.symbolResolver.recordImportAs(declarationSymbol, refSymbol);
            }
            if (key) {
                this.resolvedExternalReferences.set(key, declarationSymbol);
            }
            return declarationSymbol;
        };
        StaticReflector.prototype.findDeclaration = function (moduleUrl, name, containingFile) {
            return this.findSymbolDeclaration(this.symbolResolver.getSymbolByModule(moduleUrl, name, containingFile));
        };
        StaticReflector.prototype.tryFindDeclaration = function (moduleUrl, name, containingFile) {
            var _this = this;
            return this.symbolResolver.ignoreErrorsFor(function () { return _this.findDeclaration(moduleUrl, name, containingFile); });
        };
        StaticReflector.prototype.findSymbolDeclaration = function (symbol) {
            var resolvedSymbol = this.symbolResolver.resolveSymbol(symbol);
            if (resolvedSymbol) {
                var resolvedMetadata = resolvedSymbol.metadata;
                if (resolvedMetadata && resolvedMetadata.__symbolic === 'resolved') {
                    resolvedMetadata = resolvedMetadata.symbol;
                }
                if (resolvedMetadata instanceof static_symbol_1.StaticSymbol) {
                    return this.findSymbolDeclaration(resolvedSymbol.metadata);
                }
            }
            return symbol;
        };
        StaticReflector.prototype.tryAnnotations = function (type) {
            var originalRecorder = this.errorRecorder;
            this.errorRecorder = function (error, fileName) { };
            try {
                return this.annotations(type);
            }
            finally {
                this.errorRecorder = originalRecorder;
            }
        };
        StaticReflector.prototype.annotations = function (type) {
            var _this = this;
            return this._annotations(type, function (type, decorators) { return _this.simplify(type, decorators); }, this.annotationCache);
        };
        StaticReflector.prototype.shallowAnnotations = function (type) {
            var _this = this;
            return this._annotations(type, function (type, decorators) { return _this.simplify(type, decorators, true); }, this.shallowAnnotationCache);
        };
        StaticReflector.prototype._annotations = function (type, simplify, annotationCache) {
            var annotations = annotationCache.get(type);
            if (!annotations) {
                annotations = [];
                var classMetadata = this.getTypeMetadata(type);
                var parentType = this.findParentType(type, classMetadata);
                if (parentType) {
                    var parentAnnotations = this.annotations(parentType);
                    annotations.push.apply(annotations, tslib_1.__spread(parentAnnotations));
                }
                var ownAnnotations_1 = [];
                if (classMetadata['decorators']) {
                    ownAnnotations_1 = simplify(type, classMetadata['decorators']);
                    if (ownAnnotations_1) {
                        annotations.push.apply(annotations, tslib_1.__spread(ownAnnotations_1));
                    }
                }
                if (parentType && !this.summaryResolver.isLibraryFile(type.filePath) &&
                    this.summaryResolver.isLibraryFile(parentType.filePath)) {
                    var summary = this.summaryResolver.resolveSummary(parentType);
                    if (summary && summary.type) {
                        var requiredAnnotationTypes = this.annotationForParentClassWithSummaryKind.get(summary.type.summaryKind);
                        var typeHasRequiredAnnotation = requiredAnnotationTypes.some(function (requiredType) { return ownAnnotations_1.some(function (ann) { return requiredType.isTypeOf(ann); }); });
                        if (!typeHasRequiredAnnotation) {
                            this.reportError(formatMetadataError(metadataError("Class " + type.name + " in " + type.filePath + " extends from a " + compile_metadata_1.CompileSummaryKind[summary.type.summaryKind] + " in another compilation unit without duplicating the decorator", 
                            /* summary */ undefined, "Please add a " + requiredAnnotationTypes.map(function (type) { return type.ngMetadataName; })
                                .join(' or ') + " decorator to the class"), type), type);
                        }
                    }
                }
                annotationCache.set(type, annotations.filter(function (ann) { return !!ann; }));
            }
            return annotations;
        };
        StaticReflector.prototype.propMetadata = function (type) {
            var _this = this;
            var propMetadata = this.propertyCache.get(type);
            if (!propMetadata) {
                var classMetadata = this.getTypeMetadata(type);
                propMetadata = {};
                var parentType = this.findParentType(type, classMetadata);
                if (parentType) {
                    var parentPropMetadata_1 = this.propMetadata(parentType);
                    Object.keys(parentPropMetadata_1).forEach(function (parentProp) {
                        propMetadata[parentProp] = parentPropMetadata_1[parentProp];
                    });
                }
                var members_1 = classMetadata['members'] || {};
                Object.keys(members_1).forEach(function (propName) {
                    var propData = members_1[propName];
                    var prop = propData
                        .find(function (a) { return a['__symbolic'] == 'property' || a['__symbolic'] == 'method'; });
                    var decorators = [];
                    if (propMetadata[propName]) {
                        decorators.push.apply(decorators, tslib_1.__spread(propMetadata[propName]));
                    }
                    propMetadata[propName] = decorators;
                    if (prop && prop['decorators']) {
                        decorators.push.apply(decorators, tslib_1.__spread(_this.simplify(type, prop['decorators'])));
                    }
                });
                this.propertyCache.set(type, propMetadata);
            }
            return propMetadata;
        };
        StaticReflector.prototype.parameters = function (type) {
            var _this = this;
            if (!(type instanceof static_symbol_1.StaticSymbol)) {
                this.reportError(new Error("parameters received " + JSON.stringify(type) + " which is not a StaticSymbol"), type);
                return [];
            }
            try {
                var parameters_1 = this.parameterCache.get(type);
                if (!parameters_1) {
                    var classMetadata = this.getTypeMetadata(type);
                    var parentType = this.findParentType(type, classMetadata);
                    var members = classMetadata ? classMetadata['members'] : null;
                    var ctorData = members ? members['__ctor__'] : null;
                    if (ctorData) {
                        var ctor = ctorData.find(function (a) { return a['__symbolic'] == 'constructor'; });
                        var rawParameterTypes = ctor['parameters'] || [];
                        var parameterDecorators_1 = this.simplify(type, ctor['parameterDecorators'] || []);
                        parameters_1 = [];
                        rawParameterTypes.forEach(function (rawParamType, index) {
                            var nestedResult = [];
                            var paramType = _this.trySimplify(type, rawParamType);
                            if (paramType)
                                nestedResult.push(paramType);
                            var decorators = parameterDecorators_1 ? parameterDecorators_1[index] : null;
                            if (decorators) {
                                nestedResult.push.apply(nestedResult, tslib_1.__spread(decorators));
                            }
                            parameters_1.push(nestedResult);
                        });
                    }
                    else if (parentType) {
                        parameters_1 = this.parameters(parentType);
                    }
                    if (!parameters_1) {
                        parameters_1 = [];
                    }
                    this.parameterCache.set(type, parameters_1);
                }
                return parameters_1;
            }
            catch (e) {
                console.error("Failed on type " + JSON.stringify(type) + " with error " + e);
                throw e;
            }
        };
        StaticReflector.prototype._methodNames = function (type) {
            var methodNames = this.methodCache.get(type);
            if (!methodNames) {
                var classMetadata = this.getTypeMetadata(type);
                methodNames = {};
                var parentType = this.findParentType(type, classMetadata);
                if (parentType) {
                    var parentMethodNames_1 = this._methodNames(parentType);
                    Object.keys(parentMethodNames_1).forEach(function (parentProp) {
                        methodNames[parentProp] = parentMethodNames_1[parentProp];
                    });
                }
                var members_2 = classMetadata['members'] || {};
                Object.keys(members_2).forEach(function (propName) {
                    var propData = members_2[propName];
                    var isMethod = propData.some(function (a) { return a['__symbolic'] == 'method'; });
                    methodNames[propName] = methodNames[propName] || isMethod;
                });
                this.methodCache.set(type, methodNames);
            }
            return methodNames;
        };
        StaticReflector.prototype._staticMembers = function (type) {
            var staticMembers = this.staticCache.get(type);
            if (!staticMembers) {
                var classMetadata = this.getTypeMetadata(type);
                var staticMemberData = classMetadata['statics'] || {};
                staticMembers = Object.keys(staticMemberData);
                this.staticCache.set(type, staticMembers);
            }
            return staticMembers;
        };
        StaticReflector.prototype.findParentType = function (type, classMetadata) {
            var parentType = this.trySimplify(type, classMetadata['extends']);
            if (parentType instanceof static_symbol_1.StaticSymbol) {
                return parentType;
            }
        };
        StaticReflector.prototype.hasLifecycleHook = function (type, lcProperty) {
            if (!(type instanceof static_symbol_1.StaticSymbol)) {
                this.reportError(new Error("hasLifecycleHook received " + JSON.stringify(type) + " which is not a StaticSymbol"), type);
            }
            try {
                return !!this._methodNames(type)[lcProperty];
            }
            catch (e) {
                console.error("Failed on type " + JSON.stringify(type) + " with error " + e);
                throw e;
            }
        };
        StaticReflector.prototype.guards = function (type) {
            var e_2, _a;
            if (!(type instanceof static_symbol_1.StaticSymbol)) {
                this.reportError(new Error("guards received " + JSON.stringify(type) + " which is not a StaticSymbol"), type);
                return {};
            }
            var staticMembers = this._staticMembers(type);
            var result = {};
            try {
                for (var staticMembers_1 = tslib_1.__values(staticMembers), staticMembers_1_1 = staticMembers_1.next(); !staticMembers_1_1.done; staticMembers_1_1 = staticMembers_1.next()) {
                    var name_1 = staticMembers_1_1.value;
                    if (name_1.endsWith(TYPEGUARD_POSTFIX)) {
                        var property = name_1.substr(0, name_1.length - TYPEGUARD_POSTFIX.length);
                        var value = void 0;
                        if (property.endsWith(USE_IF)) {
                            property = name_1.substr(0, property.length - USE_IF.length);
                            value = USE_IF;
                        }
                        else {
                            value = this.getStaticSymbol(type.filePath, type.name, [name_1]);
                        }
                        result[property] = value;
                    }
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (staticMembers_1_1 && !staticMembers_1_1.done && (_a = staticMembers_1.return)) _a.call(staticMembers_1);
                }
                finally { if (e_2) throw e_2.error; }
            }
            return result;
        };
        StaticReflector.prototype._registerDecoratorOrConstructor = function (type, ctor) {
            this.conversionMap.set(type, function (context, args) { return new (ctor.bind.apply(ctor, tslib_1.__spread([void 0], args)))(); });
        };
        StaticReflector.prototype._registerFunction = function (type, fn) {
            this.conversionMap.set(type, function (context, args) { return fn.apply(undefined, args); });
        };
        StaticReflector.prototype.initializeConversionMap = function () {
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Injectable'), core_1.createInjectable);
            this.injectionToken = this.findDeclaration(ANGULAR_CORE, 'InjectionToken');
            this.opaqueToken = this.findDeclaration(ANGULAR_CORE, 'OpaqueToken');
            this.ROUTES = this.tryFindDeclaration(ANGULAR_ROUTER, 'ROUTES');
            this.ANALYZE_FOR_ENTRY_COMPONENTS =
                this.findDeclaration(ANGULAR_CORE, 'ANALYZE_FOR_ENTRY_COMPONENTS');
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Host'), core_1.createHost);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Self'), core_1.createSelf);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'SkipSelf'), core_1.createSkipSelf);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Inject'), core_1.createInject);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Optional'), core_1.createOptional);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Attribute'), core_1.createAttribute);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'ContentChild'), core_1.createContentChild);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'ContentChildren'), core_1.createContentChildren);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'ViewChild'), core_1.createViewChild);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'ViewChildren'), core_1.createViewChildren);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Input'), core_1.createInput);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Output'), core_1.createOutput);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Pipe'), core_1.createPipe);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'HostBinding'), core_1.createHostBinding);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'HostListener'), core_1.createHostListener);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Directive'), core_1.createDirective);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Component'), core_1.createComponent);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'NgModule'), core_1.createNgModule);
            // Note: Some metadata classes can be used directly with Provider.deps.
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Host'), core_1.createHost);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Self'), core_1.createSelf);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'SkipSelf'), core_1.createSkipSelf);
            this._registerDecoratorOrConstructor(this.findDeclaration(ANGULAR_CORE, 'Optional'), core_1.createOptional);
        };
        /**
         * getStaticSymbol produces a Type whose metadata is known but whose implementation is not loaded.
         * All types passed to the StaticResolver should be pseudo-types returned by this method.
         *
         * @param declarationFile the absolute path of the file where the symbol is declared
         * @param name the name of the type.
         */
        StaticReflector.prototype.getStaticSymbol = function (declarationFile, name, members) {
            return this.symbolResolver.getStaticSymbol(declarationFile, name, members);
        };
        /**
         * Simplify but discard any errors
         */
        StaticReflector.prototype.trySimplify = function (context, value) {
            var originalRecorder = this.errorRecorder;
            this.errorRecorder = function (error, fileName) { };
            var result = this.simplify(context, value);
            this.errorRecorder = originalRecorder;
            return result;
        };
        /** @internal */
        StaticReflector.prototype.simplify = function (context, value, lazy) {
            if (lazy === void 0) { lazy = false; }
            var self = this;
            var scope = BindingScope.empty;
            var calling = new Map();
            var rootContext = context;
            function simplifyInContext(context, value, depth, references) {
                function resolveReferenceValue(staticSymbol) {
                    var resolvedSymbol = self.symbolResolver.resolveSymbol(staticSymbol);
                    return resolvedSymbol ? resolvedSymbol.metadata : null;
                }
                function simplifyEagerly(value) {
                    return simplifyInContext(context, value, depth, 0);
                }
                function simplifyLazily(value) {
                    return simplifyInContext(context, value, depth, references + 1);
                }
                function simplifyNested(nestedContext, value) {
                    if (nestedContext === context) {
                        // If the context hasn't changed let the exception propagate unmodified.
                        return simplifyInContext(nestedContext, value, depth + 1, references);
                    }
                    try {
                        return simplifyInContext(nestedContext, value, depth + 1, references);
                    }
                    catch (e) {
                        if (isMetadataError(e)) {
                            // Propagate the message text up but add a message to the chain that explains how we got
                            // here.
                            // e.chain implies e.symbol
                            var summaryMsg = e.chain ? 'references \'' + e.symbol.name + '\'' : errorSummary(e);
                            var summary = "'" + nestedContext.name + "' " + summaryMsg;
                            var chain = { message: summary, position: e.position, next: e.chain };
                            // TODO(chuckj): retrieve the position information indirectly from the collectors node
                            // map if the metadata is from a .ts file.
                            self.error({
                                message: e.message,
                                advise: e.advise,
                                context: e.context,
                                chain: chain,
                                symbol: nestedContext
                            }, context);
                        }
                        else {
                            // It is probably an internal error.
                            throw e;
                        }
                    }
                }
                function simplifyCall(functionSymbol, targetFunction, args, targetExpression) {
                    if (targetFunction && targetFunction['__symbolic'] == 'function') {
                        if (calling.get(functionSymbol)) {
                            self.error({
                                message: 'Recursion is not supported',
                                summary: "called '" + functionSymbol.name + "' recursively",
                                value: targetFunction
                            }, functionSymbol);
                        }
                        try {
                            var value_1 = targetFunction['value'];
                            if (value_1 && (depth != 0 || value_1.__symbolic != 'error')) {
                                var parameters = targetFunction['parameters'];
                                var defaults = targetFunction.defaults;
                                args = args.map(function (arg) { return simplifyNested(context, arg); })
                                    .map(function (arg) { return shouldIgnore(arg) ? undefined : arg; });
                                if (defaults && defaults.length > args.length) {
                                    args.push.apply(args, tslib_1.__spread(defaults.slice(args.length).map(function (value) { return simplify(value); })));
                                }
                                calling.set(functionSymbol, true);
                                var functionScope = BindingScope.build();
                                for (var i = 0; i < parameters.length; i++) {
                                    functionScope.define(parameters[i], args[i]);
                                }
                                var oldScope = scope;
                                var result_1;
                                try {
                                    scope = functionScope.done();
                                    result_1 = simplifyNested(functionSymbol, value_1);
                                }
                                finally {
                                    scope = oldScope;
                                }
                                return result_1;
                            }
                        }
                        finally {
                            calling.delete(functionSymbol);
                        }
                    }
                    if (depth === 0) {
                        // If depth is 0 we are evaluating the top level expression that is describing element
                        // decorator. In this case, it is a decorator we don't understand, such as a custom
                        // non-angular decorator, and we should just ignore it.
                        return IGNORE;
                    }
                    var position = undefined;
                    if (targetExpression && targetExpression.__symbolic == 'resolved') {
                        var line = targetExpression.line;
                        var character = targetExpression.character;
                        var fileName = targetExpression.fileName;
                        if (fileName != null && line != null && character != null) {
                            position = { fileName: fileName, line: line, column: character };
                        }
                    }
                    self.error({
                        message: FUNCTION_CALL_NOT_SUPPORTED,
                        context: functionSymbol,
                        value: targetFunction,
                        position: position
                    }, context);
                }
                function simplify(expression) {
                    var e_3, _a, e_4, _b;
                    if (isPrimitive(expression)) {
                        return expression;
                    }
                    if (Array.isArray(expression)) {
                        var result_2 = [];
                        try {
                            for (var _c = tslib_1.__values(expression), _d = _c.next(); !_d.done; _d = _c.next()) {
                                var item = _d.value;
                                // Check for a spread expression
                                if (item && item.__symbolic === 'spread') {
                                    // We call with references as 0 because we require the actual value and cannot
                                    // tolerate a reference here.
                                    var spreadArray = simplifyEagerly(item.expression);
                                    if (Array.isArray(spreadArray)) {
                                        try {
                                            for (var spreadArray_1 = (e_4 = void 0, tslib_1.__values(spreadArray)), spreadArray_1_1 = spreadArray_1.next(); !spreadArray_1_1.done; spreadArray_1_1 = spreadArray_1.next()) {
                                                var spreadItem = spreadArray_1_1.value;
                                                result_2.push(spreadItem);
                                            }
                                        }
                                        catch (e_4_1) { e_4 = { error: e_4_1 }; }
                                        finally {
                                            try {
                                                if (spreadArray_1_1 && !spreadArray_1_1.done && (_b = spreadArray_1.return)) _b.call(spreadArray_1);
                                            }
                                            finally { if (e_4) throw e_4.error; }
                                        }
                                        continue;
                                    }
                                }
                                var value_2 = simplify(item);
                                if (shouldIgnore(value_2)) {
                                    continue;
                                }
                                result_2.push(value_2);
                            }
                        }
                        catch (e_3_1) { e_3 = { error: e_3_1 }; }
                        finally {
                            try {
                                if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                            }
                            finally { if (e_3) throw e_3.error; }
                        }
                        return result_2;
                    }
                    if (expression instanceof static_symbol_1.StaticSymbol) {
                        // Stop simplification at builtin symbols or if we are in a reference context and
                        // the symbol doesn't have members.
                        if (expression === self.injectionToken || self.conversionMap.has(expression) ||
                            (references > 0 && !expression.members.length)) {
                            return expression;
                        }
                        else {
                            var staticSymbol = expression;
                            var declarationValue = resolveReferenceValue(staticSymbol);
                            if (declarationValue != null) {
                                return simplifyNested(staticSymbol, declarationValue);
                            }
                            else {
                                return staticSymbol;
                            }
                        }
                    }
                    if (expression) {
                        if (expression['__symbolic']) {
                            var staticSymbol = void 0;
                            switch (expression['__symbolic']) {
                                case 'binop':
                                    var left = simplify(expression['left']);
                                    if (shouldIgnore(left))
                                        return left;
                                    var right = simplify(expression['right']);
                                    if (shouldIgnore(right))
                                        return right;
                                    switch (expression['operator']) {
                                        case '&&':
                                            return left && right;
                                        case '||':
                                            return left || right;
                                        case '|':
                                            return left | right;
                                        case '^':
                                            return left ^ right;
                                        case '&':
                                            return left & right;
                                        case '==':
                                            return left == right;
                                        case '!=':
                                            return left != right;
                                        case '===':
                                            return left === right;
                                        case '!==':
                                            return left !== right;
                                        case '<':
                                            return left < right;
                                        case '>':
                                            return left > right;
                                        case '<=':
                                            return left <= right;
                                        case '>=':
                                            return left >= right;
                                        case '<<':
                                            return left << right;
                                        case '>>':
                                            return left >> right;
                                        case '+':
                                            return left + right;
                                        case '-':
                                            return left - right;
                                        case '*':
                                            return left * right;
                                        case '/':
                                            return left / right;
                                        case '%':
                                            return left % right;
                                    }
                                    return null;
                                case 'if':
                                    var condition = simplify(expression['condition']);
                                    return condition ? simplify(expression['thenExpression']) :
                                        simplify(expression['elseExpression']);
                                case 'pre':
                                    var operand = simplify(expression['operand']);
                                    if (shouldIgnore(operand))
                                        return operand;
                                    switch (expression['operator']) {
                                        case '+':
                                            return operand;
                                        case '-':
                                            return -operand;
                                        case '!':
                                            return !operand;
                                        case '~':
                                            return ~operand;
                                    }
                                    return null;
                                case 'index':
                                    var indexTarget = simplifyEagerly(expression['expression']);
                                    var index = simplifyEagerly(expression['index']);
                                    if (indexTarget && isPrimitive(index))
                                        return indexTarget[index];
                                    return null;
                                case 'select':
                                    var member = expression['member'];
                                    var selectContext = context;
                                    var selectTarget = simplify(expression['expression']);
                                    if (selectTarget instanceof static_symbol_1.StaticSymbol) {
                                        var members = selectTarget.members.concat(member);
                                        selectContext =
                                            self.getStaticSymbol(selectTarget.filePath, selectTarget.name, members);
                                        var declarationValue = resolveReferenceValue(selectContext);
                                        if (declarationValue != null) {
                                            return simplifyNested(selectContext, declarationValue);
                                        }
                                        else {
                                            return selectContext;
                                        }
                                    }
                                    if (selectTarget && isPrimitive(member))
                                        return simplifyNested(selectContext, selectTarget[member]);
                                    return null;
                                case 'reference':
                                    // Note: This only has to deal with variable references, as symbol references have
                                    // been converted into 'resolved'
                                    // in the StaticSymbolResolver.
                                    var name_2 = expression['name'];
                                    var localValue = scope.resolve(name_2);
                                    if (localValue != BindingScope.missing) {
                                        return localValue;
                                    }
                                    break;
                                case 'resolved':
                                    try {
                                        return simplify(expression.symbol);
                                    }
                                    catch (e) {
                                        // If an error is reported evaluating the symbol record the position of the
                                        // reference in the error so it can
                                        // be reported in the error message generated from the exception.
                                        if (isMetadataError(e) && expression.fileName != null &&
                                            expression.line != null && expression.character != null) {
                                            e.position = {
                                                fileName: expression.fileName,
                                                line: expression.line,
                                                column: expression.character
                                            };
                                        }
                                        throw e;
                                    }
                                case 'class':
                                    return context;
                                case 'function':
                                    return context;
                                case 'new':
                                case 'call':
                                    // Determine if the function is a built-in conversion
                                    staticSymbol = simplifyInContext(context, expression['expression'], depth + 1, /* references */ 0);
                                    if (staticSymbol instanceof static_symbol_1.StaticSymbol) {
                                        if (staticSymbol === self.injectionToken || staticSymbol === self.opaqueToken) {
                                            // if somebody calls new InjectionToken, don't create an InjectionToken,
                                            // but rather return the symbol to which the InjectionToken is assigned to.
                                            // OpaqueToken is supported too as it is required by the language service to
                                            // support v4 and prior versions of Angular.
                                            return context;
                                        }
                                        var argExpressions = expression['arguments'] || [];
                                        var converter = self.conversionMap.get(staticSymbol);
                                        if (converter) {
                                            var args = argExpressions.map(function (arg) { return simplifyNested(context, arg); })
                                                .map(function (arg) { return shouldIgnore(arg) ? undefined : arg; });
                                            return converter(context, args);
                                        }
                                        else {
                                            // Determine if the function is one we can simplify.
                                            var targetFunction = resolveReferenceValue(staticSymbol);
                                            return simplifyCall(staticSymbol, targetFunction, argExpressions, expression['expression']);
                                        }
                                    }
                                    return IGNORE;
                                case 'error':
                                    var message = expression.message;
                                    if (expression['line'] != null) {
                                        self.error({
                                            message: message,
                                            context: expression.context,
                                            value: expression,
                                            position: {
                                                fileName: expression['fileName'],
                                                line: expression['line'],
                                                column: expression['character']
                                            }
                                        }, context);
                                    }
                                    else {
                                        self.error({ message: message, context: expression.context }, context);
                                    }
                                    return IGNORE;
                                case 'ignore':
                                    return expression;
                            }
                            return null;
                        }
                        return mapStringMap(expression, function (value, name) {
                            if (REFERENCE_SET.has(name)) {
                                if (name === USE_VALUE && PROVIDE in expression) {
                                    // If this is a provider expression, check for special tokens that need the value
                                    // during analysis.
                                    var provide = simplify(expression.provide);
                                    if (provide === self.ROUTES || provide == self.ANALYZE_FOR_ENTRY_COMPONENTS) {
                                        return simplify(value);
                                    }
                                }
                                return simplifyLazily(value);
                            }
                            return simplify(value);
                        });
                    }
                    return IGNORE;
                }
                return simplify(value);
            }
            var result;
            try {
                result = simplifyInContext(context, value, 0, lazy ? 1 : 0);
            }
            catch (e) {
                if (this.errorRecorder) {
                    this.reportError(e, context);
                }
                else {
                    throw formatMetadataError(e, context);
                }
            }
            if (shouldIgnore(result)) {
                return undefined;
            }
            return result;
        };
        StaticReflector.prototype.getTypeMetadata = function (type) {
            var resolvedSymbol = this.symbolResolver.resolveSymbol(type);
            return resolvedSymbol && resolvedSymbol.metadata ? resolvedSymbol.metadata :
                { __symbolic: 'class' };
        };
        StaticReflector.prototype.reportError = function (error, context, path) {
            if (this.errorRecorder) {
                this.errorRecorder(formatMetadataError(error, context), (context && context.filePath) || path);
            }
            else {
                throw error;
            }
        };
        StaticReflector.prototype.error = function (_a, reportingContext) {
            var message = _a.message, summary = _a.summary, advise = _a.advise, position = _a.position, context = _a.context, value = _a.value, symbol = _a.symbol, chain = _a.chain;
            this.reportError(metadataError(message, summary, advise, position, symbol, context, chain), reportingContext);
        };
        return StaticReflector;
    }());
    exports.StaticReflector = StaticReflector;
    var METADATA_ERROR = 'ngMetadataError';
    function metadataError(message, summary, advise, position, symbol, context, chain) {
        var error = util_1.syntaxError(message);
        error[METADATA_ERROR] = true;
        if (advise)
            error.advise = advise;
        if (position)
            error.position = position;
        if (summary)
            error.summary = summary;
        if (context)
            error.context = context;
        if (chain)
            error.chain = chain;
        if (symbol)
            error.symbol = symbol;
        return error;
    }
    function isMetadataError(error) {
        return !!error[METADATA_ERROR];
    }
    var REFERENCE_TO_NONEXPORTED_CLASS = 'Reference to non-exported class';
    var VARIABLE_NOT_INITIALIZED = 'Variable not initialized';
    var DESTRUCTURE_NOT_SUPPORTED = 'Destructuring not supported';
    var COULD_NOT_RESOLVE_TYPE = 'Could not resolve type';
    var FUNCTION_CALL_NOT_SUPPORTED = 'Function call not supported';
    var REFERENCE_TO_LOCAL_SYMBOL = 'Reference to a local symbol';
    var LAMBDA_NOT_SUPPORTED = 'Lambda not supported';
    function expandedMessage(message, context) {
        switch (message) {
            case REFERENCE_TO_NONEXPORTED_CLASS:
                if (context && context.className) {
                    return "References to a non-exported class are not supported in decorators but " + context.className + " was referenced.";
                }
                break;
            case VARIABLE_NOT_INITIALIZED:
                return 'Only initialized variables and constants can be referenced in decorators because the value of this variable is needed by the template compiler';
            case DESTRUCTURE_NOT_SUPPORTED:
                return 'Referencing an exported destructured variable or constant is not supported in decorators and this value is needed by the template compiler';
            case COULD_NOT_RESOLVE_TYPE:
                if (context && context.typeName) {
                    return "Could not resolve type " + context.typeName;
                }
                break;
            case FUNCTION_CALL_NOT_SUPPORTED:
                if (context && context.name) {
                    return "Function calls are not supported in decorators but '" + context.name + "' was called";
                }
                return 'Function calls are not supported in decorators';
            case REFERENCE_TO_LOCAL_SYMBOL:
                if (context && context.name) {
                    return "Reference to a local (non-exported) symbols are not supported in decorators but '" + context.name + "' was referenced";
                }
                break;
            case LAMBDA_NOT_SUPPORTED:
                return "Function expressions are not supported in decorators";
        }
        return message;
    }
    function messageAdvise(message, context) {
        switch (message) {
            case REFERENCE_TO_NONEXPORTED_CLASS:
                if (context && context.className) {
                    return "Consider exporting '" + context.className + "'";
                }
                break;
            case DESTRUCTURE_NOT_SUPPORTED:
                return 'Consider simplifying to avoid destructuring';
            case REFERENCE_TO_LOCAL_SYMBOL:
                if (context && context.name) {
                    return "Consider exporting '" + context.name + "'";
                }
                break;
            case LAMBDA_NOT_SUPPORTED:
                return "Consider changing the function expression into an exported function";
        }
        return undefined;
    }
    function errorSummary(error) {
        if (error.summary) {
            return error.summary;
        }
        switch (error.message) {
            case REFERENCE_TO_NONEXPORTED_CLASS:
                if (error.context && error.context.className) {
                    return "references non-exported class " + error.context.className;
                }
                break;
            case VARIABLE_NOT_INITIALIZED:
                return 'is not initialized';
            case DESTRUCTURE_NOT_SUPPORTED:
                return 'is a destructured variable';
            case COULD_NOT_RESOLVE_TYPE:
                return 'could not be resolved';
            case FUNCTION_CALL_NOT_SUPPORTED:
                if (error.context && error.context.name) {
                    return "calls '" + error.context.name + "'";
                }
                return "calls a function";
            case REFERENCE_TO_LOCAL_SYMBOL:
                if (error.context && error.context.name) {
                    return "references local variable " + error.context.name;
                }
                return "references a local variable";
        }
        return 'contains the error';
    }
    function mapStringMap(input, transform) {
        if (!input)
            return {};
        var result = {};
        Object.keys(input).forEach(function (key) {
            var value = transform(input[key], key);
            if (!shouldIgnore(value)) {
                if (HIDDEN_KEY.test(key)) {
                    Object.defineProperty(result, key, { enumerable: false, configurable: true, value: value });
                }
                else {
                    result[key] = value;
                }
            }
        });
        return result;
    }
    function isPrimitive(o) {
        return o === null || (typeof o !== 'function' && typeof o !== 'object');
    }
    var BindingScope = /** @class */ (function () {
        function BindingScope() {
        }
        BindingScope.build = function () {
            var current = new Map();
            return {
                define: function (name, value) {
                    current.set(name, value);
                    return this;
                },
                done: function () {
                    return current.size > 0 ? new PopulatedScope(current) : BindingScope.empty;
                }
            };
        };
        BindingScope.missing = {};
        BindingScope.empty = { resolve: function (name) { return BindingScope.missing; } };
        return BindingScope;
    }());
    var PopulatedScope = /** @class */ (function (_super) {
        tslib_1.__extends(PopulatedScope, _super);
        function PopulatedScope(bindings) {
            var _this = _super.call(this) || this;
            _this.bindings = bindings;
            return _this;
        }
        PopulatedScope.prototype.resolve = function (name) {
            return this.bindings.has(name) ? this.bindings.get(name) : BindingScope.missing;
        };
        return PopulatedScope;
    }(BindingScope));
    function formatMetadataMessageChain(chain, advise) {
        var expanded = expandedMessage(chain.message, chain.context);
        var nesting = chain.symbol ? " in '" + chain.symbol.name + "'" : '';
        var message = "" + expanded + nesting;
        var position = chain.position;
        var next = chain.next ?
            formatMetadataMessageChain(chain.next, advise) :
            advise ? { message: advise } : undefined;
        return { message: message, position: position, next: next ? [next] : undefined };
    }
    function formatMetadataError(e, context) {
        if (isMetadataError(e)) {
            // Produce a formatted version of the and leaving enough information in the original error
            // to recover the formatting information to eventually produce a diagnostic error message.
            var position = e.position;
            var chain = {
                message: "Error during template compile of '" + context.name + "'",
                position: position,
                next: { message: e.message, next: e.chain, context: e.context, symbol: e.symbol }
            };
            var advise = e.advise || messageAdvise(e.message, e.context);
            return formatted_error_1.formattedError(formatMetadataMessageChain(chain, advise));
        }
        return e;
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic3RhdGljX3JlZmxlY3Rvci5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2NvbXBpbGVyL3NyYy9hb3Qvc3RhdGljX3JlZmxlY3Rvci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7O0lBRUgsMkVBQXVEO0lBRXZELG1EQUFpVztJQUdqVyxtREFBb0M7SUFFcEMsNkVBQXdFO0lBQ3hFLHlFQUE2QztJQUc3QyxJQUFNLFlBQVksR0FBRyxlQUFlLENBQUM7SUFDckMsSUFBTSxjQUFjLEdBQUcsaUJBQWlCLENBQUM7SUFFekMsSUFBTSxVQUFVLEdBQUcsVUFBVSxDQUFDO0lBRTlCLElBQU0sTUFBTSxHQUFHO1FBQ2IsVUFBVSxFQUFFLFFBQVE7S0FDckIsQ0FBQztJQUVGLElBQU0sU0FBUyxHQUFHLFVBQVUsQ0FBQztJQUM3QixJQUFNLE9BQU8sR0FBRyxTQUFTLENBQUM7SUFDMUIsSUFBTSxhQUFhLEdBQUcsSUFBSSxHQUFHLENBQUMsQ0FBQyxTQUFTLEVBQUUsWUFBWSxFQUFFLE1BQU0sRUFBRSxJQUFJLEVBQUUsY0FBYyxDQUFDLENBQUMsQ0FBQztJQUN2RixJQUFNLGlCQUFpQixHQUFHLFdBQVcsQ0FBQztJQUN0QyxJQUFNLE1BQU0sR0FBRyxPQUFPLENBQUM7SUFFdkIsU0FBUyxZQUFZLENBQUMsS0FBVTtRQUM5QixPQUFPLEtBQUssSUFBSSxLQUFLLENBQUMsVUFBVSxJQUFJLFFBQVEsQ0FBQztJQUMvQyxDQUFDO0lBRUQ7OztPQUdHO0lBQ0g7UUFvQkUseUJBQ1ksZUFBOEMsRUFDOUMsY0FBb0MsRUFDNUMsb0JBQXdFLEVBQ3hFLHNCQUF3RSxFQUNoRSxhQUF1RDtZQUxuRSxpQkFtQkM7WUFoQkcscUNBQUEsRUFBQSx5QkFBd0U7WUFDeEUsdUNBQUEsRUFBQSwyQkFBd0U7WUFIaEUsb0JBQWUsR0FBZixlQUFlLENBQStCO1lBQzlDLG1CQUFj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QUM7SUFDWCxDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Q29tcGlsZVN1bW1hcnlLaW5kfSBmcm9tICcuLi9jb21waWxlX21ldGFkYXRhJztcbmltcG9ydCB7Q29tcGlsZVJlZmxlY3Rvcn0gZnJvbSAnLi4vY29tcGlsZV9yZWZsZWN0b3InO1xuaW1wb3J0IHtjcmVhdGVBdHRyaWJ1dGUsIGNyZWF0ZUNvbXBvbmVudCwgY3JlYXRlQ29udGVudENoaWxkLCBjcmVhdGVDb250ZW50Q2hpbGRyZW4sIGNyZWF0ZURpcmVjdGl2ZSwgY3JlYXRlSG9zdCwgY3JlYXRlSG9zdEJpbmRpbmcsIGNyZWF0ZUhvc3RMaXN0ZW5lciwgY3JlYXRlSW5qZWN0LCBjcmVhdGVJbmplY3RhYmxlLCBjcmVhdGVJbnB1dCwgY3JlYXRlTmdNb2R1bGUsIGNyZWF0ZU9wdGlvbmFsLCBjcmVhdGVPdXRwdXQsIGNyZWF0ZVBpcGUsIGNyZWF0ZVNlbGYsIGNyZWF0ZVNraXBTZWxmLCBjcmVhdGVWaWV3Q2hpbGQsIGNyZWF0ZVZpZXdDaGlsZHJlbiwgTWV0YWRhdGFGYWN0b3J5fSBmcm9tICcuLi9jb3JlJztcbmltcG9ydCAqIGFzIG8gZnJvbSAnLi4vb3V0cHV0L291dHB1dF9hc3QnO1xuaW1wb3J0IHtTdW1tYXJ5UmVzb2x2ZXJ9IGZyb20gJy4uL3N1bW1hcnlfcmVzb2x2ZXInO1xuaW1wb3J0IHtzeW50YXhFcnJvcn0gZnJvbSAnLi4vdXRpbCc7XG5cbmltcG9ydCB7Zm9ybWF0dGVkRXJyb3IsIEZvcm1hdHRlZE1lc3NhZ2VDaGFpbn0gZnJvbSAnLi9mb3JtYXR0ZWRfZXJyb3InO1xuaW1wb3J0IHtTdGF0aWNTeW1ib2x9IGZyb20gJy4vc3RhdGljX3N5bWJvbCc7XG5pbXBvcnQge1N0YXRpY1N5bWJvbFJlc29sdmVyfSBmcm9tICcuL3N0YXRpY19zeW1ib2xfcmVzb2x2ZXInO1xuXG5jb25zdCBBTkdVTEFSX0NPUkUgPSAnQGFuZ3VsYXIvY29yZSc7XG5jb25zdCBBTkdVTEFSX1JPVVRFUiA9ICdAYW5ndWxhci9yb3V0ZXInO1xuXG5jb25zdCBISURERU5fS0VZID0gL15cXCQuKlxcJCQvO1xuXG5jb25zdCBJR05PUkUgPSB7XG4gIF9fc3ltYm9saWM6ICdpZ25vcmUnXG59O1xuXG5jb25zdCBVU0VfVkFMVUUgPSAndXNlVmFsdWUnO1xuY29uc3QgUFJPVklERSA9ICdwcm92aWRlJztcbmNvbnN0IFJFRkVSRU5DRV9TRVQgPSBuZXcgU2V0KFtVU0VfVkFMVUUsICd1c2VGYWN0b3J5JywgJ2RhdGEnLCAnaWQnLCAnbG9hZENoaWxkcmVuJ10pO1xuY29uc3QgVFlQRUdVQVJEX1BPU1RGSVggPSAnVHlwZUd1YXJkJztcbmNvbnN0IFVTRV9JRiA9ICdVc2VJZic7XG5cbmZ1bmN0aW9uIHNob3VsZElnbm9yZSh2YWx1ZTogYW55KTogYm9vbGVhbiB7XG4gIHJldHVybiB2YWx1ZSAmJiB2YWx1ZS5fX3N5bWJvbGljID09ICdpZ25vcmUnO1xufVxuXG4vKipcbiAqIEEgc3RhdGljIHJlZmxlY3RvciBpbXBsZW1lbnRzIGVub3VnaCBvZiB0aGUgUmVmbGVjdG9yIEFQSSB0aGF0IGlzIG5lY2Vzc2FyeSB0byBjb21waWxlXG4gKiB0ZW1wbGF0ZXMgc3RhdGljYWxseS5cbiAqL1xuZXhwb3J0IGNsYXNzIFN0YXRpY1JlZmxlY3RvciBpbXBsZW1lbnRzIENvbXBpbGVSZWZsZWN0b3Ige1xuICBwcml2YXRlIGFubm90YXRpb25DYWNoZSA9IG5ldyBNYXA8U3RhdGljU3ltYm9sLCBhbnlbXT4oKTtcbiAgcHJpdmF0ZSBzaGFsbG93QW5ub3RhdGlvbkNhY2hlID0gbmV3IE1hcDxTdGF0aWNTeW1ib2wsIGFueVtdPigpO1xuICBwcml2YXRlIHByb3BlcnR5Q2FjaGUgPSBuZXcgTWFwPFN0YXRpY1N5bWJvbCwge1trZXk6IHN0cmluZ106IGFueVtdfT4oKTtcbiAgcHJpdmF0ZSBwYXJhbWV0ZXJDYWNoZSA9IG5ldyBNYXA8U3RhdGljU3ltYm9sLCBhbnlbXT4oKTtcbiAgcHJpdmF0ZSBtZXRob2RDYWNoZSA9IG5ldyBNYXA8U3RhdGljU3ltYm9sLCB7W2tleTogc3RyaW5nXTogYm9vbGVhbn0+KCk7XG4gIHByaXZhdGUgc3RhdGljQ2FjaGUgPSBuZXcgTWFwPFN0YXRpY1N5bWJvbCwgc3RyaW5nW10+KCk7XG4gIHByaXZhdGUgY29udmVyc2lvbk1hcCA9IG5ldyBNYXA8U3RhdGljU3ltYm9sLCAoY29udGV4dDogU3RhdGljU3ltYm9sLCBhcmdzOiBhbnlbXSkgPT4gYW55PigpO1xuICBwcml2YXRlIHJlc29sdmVkRXh0ZXJuYWxSZWZlcmVuY2VzID0gbmV3IE1hcDxzdHJpbmcsIFN0YXRpY1N5bWJvbD4oKTtcbiAgLy8gVE9ETyhpc3N1ZS8yNDU3MSk6IHJlbW92ZSAnIScuXG4gIHByaXZhdGUgaW5qZWN0aW9uVG9rZW4hOiBTdGF0aWNTeW1ib2w7XG4gIC8vIFRPRE8oaXNzdWUvMjQ1NzEpOiByZW1vdmUgJyEnLlxuICBwcml2YXRlIG9wYXF1ZVRva2VuITogU3RhdGljU3ltYm9sO1xuICAvLyBUT0RPKGlzc3VlLzI0NTcxKTogcmVtb3ZlICchJy5cbiAgUk9VVEVTITogU3RhdGljU3ltYm9sO1xuICAvLyBUT0RPKGlzc3VlLzI0NTcxKTogcmVtb3ZlICchJy5cbiAgcHJpdmF0ZSBBTkFMWVpFX0ZPUl9FTlRSWV9DT01QT05FTlRTITogU3RhdGljU3ltYm9sO1xuICBwcml2YXRlIGFubm90YXRpb25Gb3JQYXJlbnRDbGFzc1dpdGhTdW1tYXJ5S2luZCA9XG4gICAgICBuZXcgTWFwPENvbXBpbGVTdW1tYXJ5S2luZCwgTWV0YWRhdGFGYWN0b3J5PGFueT5bXT4oKTtcblxuICBjb25zdHJ1Y3RvcihcbiAgICAgIHByaXZhdGUgc3VtbWFyeVJlc29sdmVyOiBTdW1tYXJ5UmVzb2x2ZXI8U3RhdGljU3ltYm9sPixcbiAgICAgIHByaXZhdGUgc3ltYm9sUmVzb2x2ZXI6IFN0YXRpY1N5bWJvbFJlc29sdmVyLFxuICAgICAga25vd25NZXRhZGF0YUNsYXNzZXM6IHtuYW1lOiBzdHJpbmcsIGZpbGVQYXRoOiBzdHJpbmcsIGN0b3I6IGFueX1bXSA9IFtdLFxuICAgICAga25vd25NZXRhZGF0YUZ1bmN0aW9uczoge25hbWU6IHN0cmluZywgZmlsZVBhdGg6IHN0cmluZywgZm46IGFueX1bXSA9IFtdLFxuICAgICAgcHJpdmF0ZSBlcnJvclJlY29yZGVyPzogKGVycm9yOiBhbnksIGZpbGVOYW1lPzogc3RyaW5nKSA9PiB2b2lkKSB7XG4gICAgdGhpcy5pbml0aWFsaXplQ29udmVyc2lvbk1hcCgpO1xuICAgIGtub3duTWV0YWRhdGFDbGFzc2VzLmZvckVhY2goXG4gICAgICAgIChrYykgPT4gdGhpcy5fcmVnaXN0ZXJEZWNvcmF0b3JPckNvbnN0cnVjdG9yKFxuICAgICAgICAgICAgdGhpcy5nZXRTdGF0aWNTeW1ib2woa2MuZmlsZVBhdGgsIGtjLm5hbWUpLCBrYy5jdG9yKSk7XG4gICAga25vd25NZXRhZGF0YUZ1bmN0aW9ucy5mb3JFYWNoKFxuICAgICAgICAoa2YpID0+IHRoaXMuX3JlZ2lzdGVyRnVuY3Rpb24odGhpcy5nZXRTdGF0aWNTeW1ib2woa2YuZmlsZVBhdGgsIGtmLm5hbWUpLCBrZi5mbikpO1xuICAgIHRoaXMuYW5ub3RhdGlvbkZvclBhcmVudENsYXNzV2l0aFN1bW1hcnlLaW5kLnNldChcbiAgICAgICAgQ29tcGlsZVN1bW1hcnlLaW5kLkRpcmVjdGl2ZSwgW2NyZWF0ZURpcmVjdGl2ZSwgY3JlYXRlQ29tcG9uZW50XSk7XG4gICAgdGhpcy5hbm5vdGF0aW9uRm9yUGFyZW50Q2xhc3NXaXRoU3VtbWFyeUtpbmQuc2V0KENvbXBpbGVTdW1tYXJ5S2luZC5QaXBlLCBbY3JlYXRlUGlwZV0pO1xuICAgIHRoaXMuYW5ub3RhdGlvbkZvclBhcmVudENsYXNzV2l0aFN1bW1hcnlLaW5kLnNldChDb21waWxlU3VtbWFyeUtpbmQuTmdNb2R1bGUsIFtjcmVhdGVOZ01vZHVsZV0pO1xuICAgIHRoaXMuYW5ub3RhdGlvbkZvclBhcmVudENsYXNzV2l0aFN1bW1hcnlLaW5kLnNldChcbiAgICAgICAgQ29tcGlsZVN1bW1hcnlLaW5kLkluamVjdGFibGUsXG4gICAgICAgIFtjcmVhdGVJbmplY3RhYmxlLCBjcmVhdGVQaXBlLCBjcmVhdGVEaXJlY3RpdmUsIGNyZWF0ZUNvbXBvbmVudCwgY3JlYXRlTmdNb2R1bGVdKTtcbiAgfVxuXG4gIGNvbXBvbmVudE1vZHVsZVVybCh0eXBlT3JGdW5jOiBTdGF0aWNTeW1ib2wpOiBzdHJpbmcge1xuICAgIGNvbnN0IHN0YXRpY1N5bWJvbCA9IHRoaXMuZmluZFN5bWJvbERlY2xhcmF0aW9uKHR5cGVPckZ1bmMpO1xuICAgIHJldHVybiB0aGlzLnN5bWJvbFJlc29sdmVyLmdldFJlc291cmNlUGF0aChzdGF0aWNTeW1ib2wpO1xuICB9XG5cbiAgLyoqXG4gICAqIEludmFsaWRhdGUgdGhlIHNwZWNpZmllZCBgc3ltYm9sc2Agb24gcHJvZ3JhbSBjaGFuZ2UuXG4gICAqIEBwYXJhbSBzeW1ib2xzXG4gICAqL1xuICBpbnZhbGlkYXRlU3ltYm9scyhzeW1ib2xzOiBTdGF0aWNTeW1ib2xbXSkge1xuICAgIGZvciAoY29uc3Qgc3ltYm9sIG9mIHN5bWJvbHMpIHtcbiAgICAgIHRoaXMuYW5ub3RhdGlvbkNhY2hlLmRlbGV0ZShzeW1ib2wpO1xuICAgICAgdGhpcy5zaGFsbG93QW5ub3RhdGlvbkNhY2hlLmRlbGV0ZShzeW1ib2wpO1xuICAgICAgdGhpcy5wcm9wZXJ0eUNhY2hlLmRlbGV0ZShzeW1ib2wpO1xuICAgICAgdGhpcy5wYXJhbWV0ZXJDYWNoZS5kZWxldGUoc3ltYm9sKTtcbiAgICAgIHRoaXMubWV0aG9kQ2FjaGUuZGVsZXRlKHN5bWJvbCk7XG4gICAgICB0aGlzLnN0YXRpY0NhY2hlLmRlbGV0ZShzeW1ib2wpO1xuICAgICAgdGhpcy5jb252ZXJzaW9uTWFwLmRlbGV0ZShzeW1ib2wpO1xuICAgIH1cbiAgfVxuXG4gIHJlc29sdmVFeHRlcm5hbFJlZmVyZW5jZShyZWY6IG8uRXh0ZXJuYWxSZWZlcmVuY2UsIGNvbnRhaW5pbmdGaWxlPzogc3RyaW5nKTogU3RhdGljU3ltYm9sIHtcbiAgICBsZXQga2V5OiBzdHJpbmd8dW5kZWZpbmVkID0gdW5kZWZpbmVkO1xuICAgIGlmICghY29udGFpbmluZ0ZpbGUpIHtcbiAgICAgIGtleSA9IGAke3JlZi5tb2R1bGVOYW1lfToke3JlZi5uYW1lfWA7XG4gICAgICBjb25zdCBkZWNsYXJhdGlvblN5bWJvbCA9IHRoaXMucmVzb2x2ZWRFeHRlcm5hbFJlZmVyZW5jZXMuZ2V0KGtleSk7XG4gICAgICBpZiAoZGVjbGFyYXRpb25TeW1ib2wpIHJldHVybiBkZWNsYXJhdGlvblN5bWJvbDtcbiAgICB9XG4gICAgY29uc3QgcmVmU3ltYm9sID1cbiAgICAgICAgdGhpcy5zeW1ib2xSZXNvbHZlci5nZXRTeW1ib2xCeU1vZHVsZShyZWYubW9kdWxlTmFtZSEsIHJlZi5uYW1lISwgY29udGFpbmluZ0ZpbGUpO1xuICAgIGNvbnN0IGRlY2xhcmF0aW9uU3ltYm9sID0gdGhpcy5maW5kU3ltYm9sRGVjbGFyYXRpb24ocmVmU3ltYm9sKTtcbiAgICBpZiAoIWNvbnRhaW5pbmdGaWxlKSB7XG4gICAgICB0aGlzLnN5bWJvbFJlc29sdmVyLnJlY29yZE1vZHVsZU5hbWVGb3JGaWxlTmFtZShyZWZTeW1ib2wuZmlsZVBhdGgsIHJlZi5tb2R1bGVOYW1lISk7XG4gICAgICB0aGlzLnN5bWJvbFJlc29sdmVyLnJlY29yZEltcG9ydEFzKGRlY2xhcmF0aW9uU3ltYm9sLCByZWZTeW1ib2wpO1xuICAgIH1cbiAgICBpZiAoa2V5KSB7XG4gICAgICB0aGlzLnJlc29sdmVkRXh0ZXJuYWxSZWZlcmVuY2VzLnNldChrZXksIGRlY2xhcmF0aW9uU3ltYm9sKTtcbiAgICB9XG4gICAgcmV0dXJuIGRlY2xhcmF0aW9uU3ltYm9sO1xuICB9XG5cbiAgZmluZERlY2xhcmF0aW9uKG1vZHVsZVVybDogc3RyaW5nLCBuYW1lOiBzdHJpbmcsIGNvbnRhaW5pbmdGaWxlPzogc3RyaW5nKTogU3RhdGljU3ltYm9sIHtcbiAgICByZXR1cm4gdGhpcy5maW5kU3ltYm9sRGVjbGFyYXRpb24oXG4gICAgICAgIHRoaXMuc3ltYm9sUmVzb2x2ZXIuZ2V0U3ltYm9sQnlNb2R1bGUobW9kdWxlVXJsLCBuYW1lLCBjb250YWluaW5nRmlsZSkpO1xuICB9XG5cbiAgdHJ5RmluZERlY2xhcmF0aW9uKG1vZHVsZVVybDogc3RyaW5nLCBuYW1lOiBzdHJpbmcsIGNvbnRhaW5pbmdGaWxlPzogc3RyaW5nKTogU3RhdGljU3ltYm9sIHtcbiAgICByZXR1cm4gdGhpcy5zeW1ib2xSZXNvbHZlci5pZ25vcmVFcnJvcnNGb3IoXG4gICAgICAgICgpID0+IHRoaXMuZmluZERlY2xhcmF0aW9uKG1vZHVsZVVybCwgbmFtZSwgY29udGFpbmluZ0ZpbGUpKTtcbiAgfVxuXG4gIGZpbmRTeW1ib2xEZWNsYXJhdGlvbihzeW1ib2w6IFN0YXRpY1N5bWJvbCk6IFN0YXRpY1N5bWJvbCB7XG4gICAgY29uc3QgcmVzb2x2ZWRTeW1ib2wgPSB0aGlzLnN5bWJvbFJlc29sdmVyLnJlc29sdmVTeW1ib2woc3ltYm9sKTtcbiAgICBpZiAocmVzb2x2ZWRTeW1ib2wpIHtcbiAgICAgIGxldCByZXNvbHZlZE1ldGFkYXRhID0gcmVzb2x2ZWRTeW1ib2wubWV0YWRhdGE7XG4gICAgICBpZiAocmVzb2x2ZWRNZXRhZGF0YSAmJiByZXNvbHZlZE1ldGFkYXRhLl9fc3ltYm9saWMgPT09ICdyZXNvbHZlZCcpIHtcbiAgICAgICAgcmVzb2x2ZWRNZXRhZGF0YSA9IHJlc29sdmVkTWV0YWRhdGEuc3ltYm9sO1xuICAgICAgfVxuICAgICAgaWYgKHJlc29sdmVkTWV0YWRhdGEgaW5zdGFuY2VvZiBTdGF0aWNTeW1ib2wpIHtcbiAgICAgICAgcmV0dXJuIHRoaXMuZmluZFN5bWJvbERlY2xhcmF0aW9uKHJlc29sdmVkU3ltYm9sLm1ldGFkYXRhKTtcbiAgICAgIH1cbiAgICB9XG4gICAgcmV0dXJuIHN5bWJvbDtcbiAgfVxuXG4gIHB1YmxpYyB0cnlBbm5vdGF0aW9ucyh0eXBlOiBTdGF0aWNTeW1ib2wpOiBhbnlbXSB7XG4gICAgY29uc3Qgb3JpZ2luYWxSZWNvcmRlciA9IHRoaXMuZXJyb3JSZWNvcmRlcjtcbiAgICB0aGlzLmVycm9yUmVjb3JkZXIgPSAoZXJyb3I6IGFueSwgZmlsZU5hbWU/OiBzdHJpbmcpID0+IHt9O1xuICAgIHRyeSB7XG4gICAgICByZXR1cm4gdGhpcy5hbm5vdGF0aW9ucyh0eXBlKTtcbiAgICB9IGZpbmFsbHkge1xuICAgICAgdGhpcy5lcnJvclJlY29yZGVyID0gb3JpZ2luYWxSZWNvcmRlcjtcbiAgICB9XG4gIH1cblxuICBwdWJsaWMgYW5ub3RhdGlvbnModHlwZTogU3RhdGljU3ltYm9sKTogYW55W10ge1xuICAgIHJldHVybiB0aGlzLl9hbm5vdGF0aW9ucyhcbiAgICAgICAgdHlwZSwgKHR5cGU6IFN0YXRpY1N5bWJvbCwgZGVjb3JhdG9yczogYW55KSA9PiB0aGlzLnNpbXBsaWZ5KHR5cGUsIGRlY29yYXRvcnMpLFxuICAgICAgICB0aGlzLmFubm90YXRpb25DYWNoZSk7XG4gIH1cblxuICBwdWJsaWMgc2hhbGxvd0Fubm90YXRpb25zKHR5cGU6IFN0YXRpY1N5bWJvbCk6IGFueVtdIHtcbiAgICByZXR1cm4gdGhpcy5fYW5ub3RhdGlvbnMoXG4gICAgICAgIHR5cGUsICh0eXBlOiBTdGF0aWNTeW1ib2wsIGRlY29yYXRvcnM6IGFueSkgPT4gdGhpcy5zaW1wbGlmeSh0eXBlLCBkZWNvcmF0b3JzLCB0cnVlKSxcbiAgICAgICAgdGhpcy5zaGFsbG93QW5ub3RhdGlvbkNhY2hlKTtcbiAgfVxuXG4gIHByaXZhdGUgX2Fubm90YXRpb25zKFxuICAgICAgdHlwZTogU3RhdGljU3ltYm9sLCBzaW1wbGlmeTogKHR5cGU6IFN0YXRpY1N5bWJvbCwgZGVjb3JhdG9yczogYW55KSA9PiBhbnksXG4gICAgICBhbm5vdGF0aW9uQ2FjaGU6IE1hcDxTdGF0aWNTeW1ib2wsIGFueVtdPik6IGFueVtdIHtcbiAgICBsZXQgYW5ub3RhdGlvbnMgPSBhbm5vdGF0aW9uQ2FjaGUuZ2V0KHR5cGUpO1xuICAgIGlmICghYW5ub3RhdGlvbnMpIHtcbiAgICAgIGFubm90YXRpb25zID0gW107XG4gICAgICBjb25zdCBjbGFzc01ldGFkYXRhID0gdGhpcy5nZXRUeXBlTWV0YWRhdGEodHlwZSk7XG4gICAgICBjb25zdCBwYXJlbnRUeXBlID0gdGhpcy5maW5kUGFyZW50VHlwZSh0eXBlLCBjbGFzc01ldGFkYXRhKTtcbiAgICAgIGlmIChwYXJlbnRUeXBlKSB7XG4gICAgICAgIGNvbnN0IHBhcmVudEFubm90YXRpb25zID0gdGhpcy5hbm5vdGF0aW9ucyhwYXJlbnRUeXBlKTtcbiAgICAgICAgYW5ub3RhdGlvbnMucHVzaCguLi5wYXJlbnRBbm5vdGF0aW9ucyk7XG4gICAgICB9XG4gICAgICBsZXQgb3duQW5ub3RhdGlvbnM6IGFueVtdID0gW107XG4gICAgICBpZiAoY2xhc3NNZXRhZGF0YVsnZGVjb3JhdG9ycyddKSB7XG4gICAgICAgIG93bkFubm90YXRpb25zID0gc2ltcGxpZnkodHlwZSwgY2xhc3NNZXRhZGF0YVsnZGVjb3JhdG9ycyddKTtcbiAgICAgICAgaWYgKG93bkFubm90YXRpb25zKSB7XG4gICAgICAgICAgYW5ub3RhdGlvbnMucHVzaCguLi5vd25Bbm5vdGF0aW9ucyk7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICAgIGlmIChwYXJlbnRUeXBlICYmICF0aGlzLnN1bW1hcnlSZXNvbHZlci5pc0xpYnJhcnlGaWxlKHR5cGUuZmlsZVBhdGgpICYmXG4gICAgICAgICAgdGhpcy5zdW1tYXJ5UmVzb2x2ZXIuaXNMaWJyYXJ5RmlsZShwYXJlbnRUeXBlLmZpbGVQYXRoKSkge1xuICAgICAgICBjb25zdCBzdW1tYXJ5ID0gdGhpcy5zdW1tYXJ5UmVzb2x2ZXIucmVzb2x2ZVN1bW1hcnkocGFyZW50VHlwZSk7XG4gICAgICAgIGlmIChzdW1tYXJ5ICYmIHN1bW1hcnkudHlwZSkge1xuICAgICAgICAgIGNvbnN0IHJlcXVpcmVkQW5ub3RhdGlvblR5cGVzID1cbiAgICAgICAgICAgICAgdGhpcy5hbm5vdGF0aW9uRm9yUGFyZW50Q2xhc3NXaXRoU3VtbWFyeUtpbmQuZ2V0KHN1bW1hcnkudHlwZS5zdW1tYXJ5S2luZCEpITtcbiAgICAgICAgICBjb25zdCB0eXBlSGFzUmVxdWlyZWRBbm5vdGF0aW9uID0gcmVxdWlyZWRBbm5vdGF0aW9uVHlwZXMuc29tZShcbiAgICAgICAgICAgICAgKHJlcXVpcmVkVHlwZSkgPT4gb3duQW5ub3RhdGlvbnMuc29tZShhbm4gPT4gcmVxdWlyZWRUeXBlLmlzVHlwZU9mKGFubikpKTtcbiAgICAgICAgICBpZiAoIXR5cGVIYXNSZXF1aXJlZEFubm90YXRpb24pIHtcbiAgICAgICAgICAgIHRoaXMucmVwb3J0RXJyb3IoXG4gICAgICAgICAgICAgICAgZm9ybWF0TWV0YWRhdGFFcnJvcihcbiAgICAgICAgICAgICAgICAgICAgbWV0YWRhdGFFcnJvcihcbiAgICAgICAgICAgICAgICAgICAgICAgIGBDbGFzcyAke3R5cGUubmFtZX0gaW4gJHt0eXBlLmZpbGVQYXRofSBleHRlbmRzIGZyb20gYSAke1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIENvbXBpbGVTdW1tYXJ5S2luZFtzdW1tYXJ5LnR5cGUuc3VtbWFyeUtpbmQhXG4gICAgICAgICAgICBdfSBpbiBhbm90aGVyIGNvbXBpbGF0aW9uIHVuaXQgd2l0aG91dCBkdXBsaWNhdGluZyB0aGUgZGVjb3JhdG9yYCxcbiAgICAgICAgICAgICAgICAgICAgICAgIC8qIHN1bW1hcnkgKi8gdW5kZWZpbmVkLFxuICAgICAgICAgICAgICAgICAgICAgICAgYFBsZWFzZSBhZGQgYSAke1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJlcXVpcmVkQW5ub3RhdGlvblR5cGVzLm1hcCgodHlwZSkgPT4gdHlwZS5uZ01ldGFkYXRhTmFtZSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLmpvaW4oJyBvciAnKX0gZGVjb3JhdG9yIHRvIHRoZSBjbGFzc2ApLFxuICAgICAgICAgICAgICAgICAgICB0eXBlKSxcbiAgICAgICAgICAgICAgICB0eXBlKTtcbiAgICAgICAgICB9XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICAgIGFubm90YXRpb25DYWNoZS5zZXQodHlwZSwgYW5ub3RhdGlvbnMuZmlsdGVyKGFubiA9PiAhIWFubikpO1xuICAgIH1cbiAgICByZXR1cm4gYW5ub3RhdGlvbnM7XG4gIH1cblxuICBwdWJsaWMgcHJvcE1ldGFkYXRhKHR5cGU6IFN0YXRpY1N5bWJvbCk6IHtba2V5OiBzdHJpbmddOiBhbnlbXX0ge1xuICAgIGxldCBwcm9wTWV0YWRhdGEgPSB0aGlzLnByb3BlcnR5Q2FjaGUuZ2V0KHR5cGUpO1xuICAgIGlmICghcHJvcE1ldGFkYXRhKSB7XG4gICAgICBjb25zdCBjbGFzc01ldGFkYXRhID0gdGhpcy5nZXRUeXBlTWV0YWRhdGEodHlwZSk7XG4gICAgICBwcm9wTWV0YWRhdGEgPSB7fTtcbiAgICAgIGNvbnN0IHBhcmVudFR5cGUgPSB0aGlzLmZpbmRQYXJlbnRUeXBlKHR5cGUsIGNsYXNzTWV0YWRhdGEpO1xuICAgICAgaWYgKHBhcmVudFR5cGUpIHtcbiAgICAgICAgY29uc3QgcGFyZW50UHJvcE1ldGFkYXRhID0gdGhpcy5wcm9wTWV0YWRhdGEocGFyZW50VHlwZSk7XG4gICAgICAgIE9iamVjdC5rZXlzKHBhcmVudFByb3BNZXRhZGF0YSkuZm9yRWFjaCgocGFyZW50UHJvcCkgPT4ge1xuICAgICAgICAgIHByb3BNZXRhZGF0YSFbcGFyZW50UHJvcF0gPSBwYXJlbnRQcm9wTWV0YWRhdGFbcGFyZW50UHJvcF07XG4gICAgICAgIH0pO1xuICAgICAgfVxuXG4gICAgICBjb25zdCBtZW1iZXJzID0gY2xhc3NNZXRhZGF0YVsnbWVtYmVycyddIHx8IHt9O1xuICAgICAgT2JqZWN0LmtleXMobWVtYmVycykuZm9yRWFjaCgocHJvcE5hbWUpID0+IHtcbiAgICAgICAgY29uc3QgcHJvcERhdGEgPSBtZW1iZXJzW3Byb3BOYW1lXTtcbiAgICAgICAgY29uc3QgcHJvcCA9ICg8YW55W10+cHJvcERhdGEpXG4gICAgICAgICAgICAgICAgICAgICAgICAgLmZpbmQoYSA9PiBhWydfX3N5bWJvbGljJ10gPT0gJ3Byb3BlcnR5JyB8fCBhWydfX3N5bWJvbGljJ10gPT0gJ21ldGhvZCcpO1xuICAgICAgICBjb25zdCBkZWNvcmF0b3JzOiBhbnlbXSA9IFtdO1xuICAgICAgICBpZiAocHJvcE1ldGFkYXRhIVtwcm9wTmFtZV0pIHtcbiAgICAgICAgICBkZWNvcmF0b3JzLnB1c2goLi4ucHJvcE1ldGFkYXRhIVtwcm9wTmFtZV0pO1xuICAgICAgICB9XG4gICAgICAgIHByb3BNZXRhZGF0YSFbcHJvcE5hbWVdID0gZGVjb3JhdG9ycztcbiAgICAgICAgaWYgKHByb3AgJiYgcHJvcFsnZGVjb3JhdG9ycyddKSB7XG4gICAgICAgICAgZGVjb3JhdG9ycy5wdXNoKC4uLnRoaXMuc2ltcGxpZnkodHlwZSwgcHJvcFsnZGVjb3JhdG9ycyddKSk7XG4gICAgICAgIH1cbiAgICAgIH0pO1xuICAgICAgdGhpcy5wcm9wZXJ0eUNhY2hlLnNldCh0eXBlLCBwcm9wTWV0YWRhdGEpO1xuICAgIH1cbiAgICByZXR1cm4gcHJvcE1ldGFkYXRhO1xuICB9XG5cbiAgcHVibGljIHBhcmFtZXRlcnModHlwZTogU3RhdGljU3ltYm9sKTogYW55W10ge1xuICAgIGlmICghKHR5cGUgaW5zdGFuY2VvZiBTdGF0aWNTeW1ib2wpKSB7XG4gICAgICB0aGlzLnJlcG9ydEVycm9yKFxuICAgICAgICAgIG5ldyBFcnJvcihgcGFyYW1ldGVycyByZWNlaXZlZCAke0pTT04uc3RyaW5naWZ5KHR5cGUpfSB3aGljaCBpcyBub3QgYSBTdGF0aWNTeW1ib2xgKSxcbiAgICAgICAgICB0eXBlKTtcbiAgICAgIHJldHVybiBbXTtcbiAgICB9XG4gICAgdHJ5IHtcbiAgICAgIGxldCBwYXJhbWV0ZXJzID0gdGhpcy5wYXJhbWV0ZXJDYWNoZS5nZXQodHlwZSk7XG4gICAgICBpZiAoIXBhcmFtZXRlcnMpIHtcbiAgICAgICAgY29uc3QgY2xhc3NNZXRhZGF0YSA9IHRoaXMuZ2V0VHlwZU1ldGFkYXRhKHR5cGUpO1xuICAgICAgICBjb25zdCBwYXJlbnRUeXBlID0gdGhpcy5maW5kUGFyZW50VHlwZSh0eXBlLCBjbGFzc01ldGFkYXRhKTtcbiAgICAgICAgY29uc3QgbWVtYmVycyA9IGNsYXNzTWV0YWRhdGEgPyBjbGFzc01ldGFkYXRhWydtZW1iZXJzJ10gOiBudWxsO1xuICAgICAgICBjb25zdCBjdG9yRGF0YSA9IG1lbWJlcnMgPyBtZW1iZXJzWydfX2N0b3JfXyddIDogbnVsbDtcbiAgICAgICAgaWYgKGN0b3JEYXRhKSB7XG4gICAgICAgICAgY29uc3QgY3RvciA9ICg8YW55W10+Y3RvckRhdGEpLmZpbmQoYSA9PiBhWydfX3N5bWJvbGljJ10gPT0gJ2NvbnN0cnVjdG9yJyk7XG4gICAgICAgICAgY29uc3QgcmF3UGFyYW1ldGVyVHlwZXMgPSA8YW55W10+Y3RvclsncGFyYW1ldGVycyddIHx8IFtdO1xuICAgICAgICAgIGNvbnN0IHBhcmFtZXRlckRlY29yYXRvcnMgPSA8YW55W10+dGhpcy5zaW1wbGlmeSh0eXBlLCBjdG9yWydwYXJhbWV0ZXJEZWNvcmF0b3JzJ10gfHwgW10pO1xuICAgICAgICAgIHBhcmFtZXRlcnMgPSBbXTtcbiAgICAgICAgICByYXdQYXJhbWV0ZXJUeXBlcy5mb3JFYWNoKChyYXdQYXJhbVR5cGUsIGluZGV4KSA9PiB7XG4gICAgICAgICAgICBjb25zdCBuZXN0ZWRSZXN1bHQ6IGFueVtdID0gW107XG4gICAgICAgICAgICBjb25zdCBwYXJhbVR5cGUgPSB0aGlzLnRyeVNpbXBsaWZ5KHR5cGUsIHJhd1BhcmFtVHlwZSk7XG4gICAgICAgICAgICBpZiAocGFyYW1UeXBlKSBuZXN0ZWRSZXN1bHQucHVzaChwYXJhbVR5cGUpO1xuICAgICAgICAgICAgY29uc3QgZGVjb3JhdG9ycyA9IHBhcmFtZXRlckRlY29yYXRvcnMgPyBwYXJhbWV0ZXJEZWNvcmF0b3JzW2luZGV4XSA6IG51bGw7XG4gICAgICAgICAgICBpZiAoZGVjb3JhdG9ycykge1xuICAgICAgICAgICAgICBuZXN0ZWRSZXN1bHQucHVzaCguLi5kZWNvcmF0b3JzKTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHBhcmFtZXRlcnMhLnB1c2gobmVzdGVkUmVzdWx0KTtcbiAgICAgICAgICB9KTtcbiAgICAgICAgfSBlbHNlIGlmIChwYXJlbnRUeXBlKSB7XG4gICAgICAgICAgcGFyYW1ldGVycyA9IHRoaXMucGFyYW1ldGVycyhwYXJlbnRUeXBlKTtcbiAgICAgICAgfVxuICAgICAgICBpZiAoIXBhcmFtZXRlcnMpIHtcbiAgICAgICAgICBwYXJhbWV0ZXJzID0gW107XG4gICAgICAgIH1cbiAgICAgICAgdGhpcy5wYXJhbWV0ZXJDYWNoZS5zZXQodHlwZSwgcGFyYW1ldGVycyk7XG4gICAgICB9XG4gICAgICByZXR1cm4gcGFyYW1ldGVycztcbiAgICB9IGNhdGNoIChlKSB7XG4gICAgICBjb25zb2xlLmVycm9yKGBGYWlsZWQgb24gdHlwZSAke0pTT04uc3RyaW5naWZ5KHR5cGUpfSB3aXRoIGVycm9yICR7ZX1gKTtcbiAgICAgIHRocm93IGU7XG4gICAgfVxuICB9XG5cbiAgcHJpdmF0ZSBfbWV0aG9kTmFtZXModHlwZTogYW55KToge1trZXk6IHN0cmluZ106IGJvb2xlYW59IHtcbiAgICBsZXQgbWV0aG9kTmFtZXMgPSB0aGlzLm1ldGhvZENhY2hlLmdldCh0eXBlKTtcbiAgICBpZiAoIW1ldGhvZE5hbWVzKSB7XG4gICAgICBjb25zdCBjbGFzc01ldGFkYXRhID0gdGhpcy5nZXRUeXBlTWV0YWRhdGEodHlwZSk7XG4gICAgICBtZXRob2ROYW1lcyA9IHt9O1xuICAgICAgY29uc3QgcGFyZW50VHlwZSA9IHRoaXMuZmluZFBhcmVudFR5cGUodHlwZSwgY2xhc3NNZXRhZGF0YSk7XG4gICAgICBpZiAocGFyZW50VHlwZSkge1xuICAgICAgICBjb25zdCBwYXJlbnRNZXRob2ROYW1lcyA9IHRoaXMuX21ldGhvZE5hbWVzKHBhcmVudFR5cGUpO1xuICAgICAgICBPYmplY3Qua2V5cyhwYXJlbnRNZXRob2ROYW1lcykuZm9yRWFjaCgocGFyZW50UHJvcCkgPT4ge1xuICAgICAgICAgIG1ldGhvZE5hbWVzIVtwYXJlbnRQcm9wXSA9IHBhcmVudE1ldGhvZE5hbWVzW3BhcmVudFByb3BdO1xuICAgICAgICB9KTtcbiAgICAgIH1cblxuICAgICAgY29uc3QgbWVtYmVycyA9IGNsYXNzTWV0YWRhdGFbJ21lbWJlcnMnXSB8fCB7fTtcbiAgICAgIE9iamVjdC5rZXlzKG1lbWJlcnMpLmZvckVhY2goKHByb3BOYW1lKSA9PiB7XG4gICAgICAgIGNvbnN0IHByb3BEYXRhID0gbWVtYmVyc1twcm9wTmFtZV07XG4gICAgICAgIGNvbnN0IGlzTWV0aG9kID0gKDxhbnlbXT5wcm9wRGF0YSkuc29tZShhID0+IGFbJ19fc3ltYm9saWMnXSA9PSAnbWV0aG9kJyk7XG4gICAgICAgIG1ldGhvZE5hbWVzIVtwcm9wTmFtZV0gPSBtZXRob2ROYW1lcyFbcHJvcE5hbWVdIHx8IGlzTWV0aG9kO1xuICAgICAgfSk7XG4gICAgICB0aGlzLm1ldGhvZENhY2hlLnNldCh0eXBlLCBtZXRob2ROYW1lcyk7XG4gICAgfVxuICAgIHJldHVybiBtZXRob2ROYW1lcztcbiAgfVxuXG4gIHByaXZhdGUgX3N0YXRpY01lbWJlcnModHlwZTogU3RhdGljU3ltYm9sKTogc3RyaW5nW10ge1xuICAgIGxldCBzdGF0aWNNZW1iZXJzID0gdGhpcy5zdGF0aWNDYWNoZS5nZXQodHlwZSk7XG4gICAgaWYgKCFzdGF0aWNNZW1iZXJzKSB7XG4gICAgICBjb25zdCBjbGFzc01ldGFkYXRhID0gdGhpcy5nZXRUeXBlTWV0YWRhdGEodHlwZSk7XG4gICAgICBjb25zdCBzdGF0aWNNZW1iZXJEYXRhID0gY2xhc3NNZXRhZGF0YVsnc3RhdGljcyddIHx8IHt9O1xuICAgICAgc3RhdGljTWVtYmVycyA9IE9iamVjdC5rZXlzKHN0YXRpY01lbWJlckRhdGEpO1xuICAgICAgdGhpcy5zdGF0aWNDYWNoZS5zZXQodHlwZSwgc3RhdGljTWVtYmVycyk7XG4gICAgfVxuICAgIHJldHVybiBzdGF0aWNNZW1iZXJzO1xuICB9XG5cblxuICBwcml2YXRlIGZpbmRQYXJlbnRUeXBlKHR5cGU6IFN0YXRpY1N5bWJvbCwgY2xhc3NNZXRhZGF0YTogYW55KTogU3RhdGljU3ltYm9sfHVuZGVmaW5lZCB7XG4gICAgY29uc3QgcGFyZW50VHlwZSA9IHRoaXMudHJ5U2ltcGxpZnkodHlwZSwgY2xhc3NNZXRhZGF0YVsnZXh0ZW5kcyddKTtcbiAgICBpZiAocGFyZW50VHlwZSBpbnN0YW5jZW9mIFN0YXRpY1N5bWJvbCkge1xuICAgICAgcmV0dXJuIHBhcmVudFR5cGU7XG4gICAgfVxuICB9XG5cbiAgaGFzTGlmZWN5Y2xlSG9vayh0eXBlOiBhbnksIGxjUHJvcGVydHk6IHN0cmluZyk6IGJvb2xlYW4ge1xuICAgIGlmICghKHR5cGUgaW5zdGFuY2VvZiBTdGF0aWNTeW1ib2wpKSB7XG4gICAgICB0aGlzLnJlcG9ydEVycm9yKFxuICAgICAgICAgIG5ldyBFcnJvcihcbiAgICAgICAgICAgICAgYGhhc0xpZmVjeWNsZUhvb2sgcmVjZWl2ZWQgJHtKU09OLnN0cmluZ2lmeSh0eXBlKX0gd2hpY2ggaXMgbm90IGEgU3RhdGljU3ltYm9sYCksXG4gICAgICAgICAgdHlwZSk7XG4gICAgfVxuICAgIHRyeSB7XG4gICAgICByZXR1cm4gISF0aGlzLl9tZXRob2ROYW1lcyh0eXBlKVtsY1Byb3BlcnR5XTtcbiAgICB9IGNhdGNoIChlKSB7XG4gICAgICBjb25zb2xlLmVycm9yKGBGYWlsZWQgb24gdHlwZSAke0pTT04uc3RyaW5naWZ5KHR5cGUpfSB3aXRoIGVycm9yICR7ZX1gKTtcbiAgICAgIHRocm93IGU7XG4gICAgfVxuICB9XG5cbiAgZ3VhcmRzKHR5cGU6IGFueSk6IHtba2V5OiBzdHJpbmddOiBTdGF0aWNTeW1ib2x9IHtcbiAgICBpZiAoISh0eXBlIGluc3RhbmNlb2YgU3RhdGljU3ltYm9sKSkge1xuICAgICAgdGhpcy5yZXBvcnRFcnJvcihcbiAgICAgICAgICBuZXcgRXJyb3IoYGd1YXJkcyByZWNlaXZlZCAke0pTT04uc3RyaW5naWZ5KHR5cGUpfSB3aGljaCBpcyBub3QgYSBTdGF0aWNTeW1ib2xgKSwgdHlwZSk7XG4gICAgICByZXR1cm4ge307XG4gICAgfVxuICAgIGNvbnN0IHN0YXRpY01lbWJlcnMgPSB0aGlzLl9zdGF0aWNNZW1iZXJzKHR5cGUpO1xuICAgIGNvbnN0IHJlc3VsdDoge1trZXk6IHN0cmluZ106IFN0YXRpY1N5bWJvbH0gPSB7fTtcbiAgICBmb3IgKGxldCBuYW1lIG9mIHN0YXRpY01lbWJlcnMpIHtcbiAgICAgIGlmIChuYW1lLmVuZHNXaXRoKFRZUEVHVUFSRF9QT1NURklYKSkge1xuICAgICAgICBsZXQgcHJvcGVydHkgPSBuYW1lLnN1YnN0cigwLCBuYW1lLmxlbmd0aCAtIFRZUEVHVUFSRF9QT1NURklYLmxlbmd0aCk7XG4gICAgICAgIGxldCB2YWx1ZTogYW55O1xuICAgICAgICBpZiAocHJvcGVydHkuZW5kc1dpdGgoVVNFX0lGKSkge1xuICAgICAgICAgIHByb3BlcnR5ID0gbmFtZS5zdWJzdHIoMCwgcHJvcGVydHkubGVuZ3RoIC0gVVNFX0lGLmxlbmd0aCk7XG4gICAgICAgICAgdmFsdWUgPSBVU0VfSUY7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgdmFsdWUgPSB0aGlzLmdldFN0YXRpY1N5bWJvbCh0eXBlLmZpbGVQYXRoLCB0eXBlLm5hbWUsIFtuYW1lXSk7XG4gICAgICAgIH1cbiAgICAgICAgcmVzdWx0W3Byb3BlcnR5XSA9IHZhbHVlO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gcmVzdWx0O1xuICB9XG5cbiAgcHJpdmF0ZSBfcmVnaXN0ZXJEZWNvcmF0b3JPckNvbnN0cnVjdG9yKHR5cGU6IFN0YXRpY1N5bWJvbCwgY3RvcjogYW55KTogdm9pZCB7XG4gICAgdGhpcy5jb252ZXJzaW9uTWFwLnNldCh0eXBlLCAoY29udGV4dDogU3RhdGljU3ltYm9sLCBhcmdzOiBhbnlbXSkgPT4gbmV3IGN0b3IoLi4uYXJncykpO1xuICB9XG5cbiAgcHJpdmF0ZSBfcmVnaXN0ZXJGdW5jdGlvbih0eXBlOiBTdGF0aWNTeW1ib2wsIGZuOiBhbnkpOiB2b2lkIHtcbiAgICB0aGlzLmNvbnZlcnNpb25NYXAuc2V0KHR5cGUsIChjb250ZXh0OiBTdGF0aWNTeW1ib2wsIGFyZ3M6IGFueVtdKSA9PiBmbi5hcHBseSh1bmRlZmluZWQsIGFyZ3MpKTtcbiAgfVxuXG4gIHByaXZhdGUgaW5pdGlhbGl6ZUNvbnZlcnNpb25NYXAoKTogdm9pZCB7XG4gICAgdGhpcy5fcmVnaXN0ZXJEZWNvcmF0b3JPckNvbnN0cnVjdG9yKFxuICAgICAgICB0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdJbmplY3RhYmxlJyksIGNyZWF0ZUluamVjdGFibGUpO1xuICAgIHRoaXMuaW5qZWN0aW9uVG9rZW4gPSB0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdJbmplY3Rpb25Ub2tlbicpO1xuICAgIHRoaXMub3BhcXVlVG9rZW4gPSB0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdPcGFxdWVUb2tlbicpO1xuICAgIHRoaXMuUk9VVEVTID0gdGhpcy50cnlGaW5kRGVjbGFyYXRpb24oQU5HVUxBUl9ST1VURVIsICdST1VURVMnKTtcbiAgICB0aGlzLkFOQUxZWkVfRk9SX0VOVFJZX0NPTVBPTkVOVFMgPVxuICAgICAgICB0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdBTkFMWVpFX0ZPUl9FTlRSWV9DT01QT05FTlRTJyk7XG5cbiAgICB0aGlzLl9yZWdpc3RlckRlY29yYXRvck9yQ29uc3RydWN0b3IodGhpcy5maW5kRGVjbGFyYXRpb24oQU5HVUxBUl9DT1JFLCAnSG9zdCcpLCBjcmVhdGVIb3N0KTtcbiAgICB0aGlzLl9yZWdpc3RlckRlY29yYXRvck9yQ29uc3RydWN0b3IodGhpcy5maW5kRGVjbGFyYXRpb24oQU5HVUxBUl9DT1JFLCAnU2VsZicpLCBjcmVhdGVTZWxmKTtcbiAgICB0aGlzLl9yZWdpc3RlckRlY29yYXRvck9yQ29uc3RydWN0b3IoXG4gICAgICAgIHRoaXMuZmluZERlY2xhcmF0aW9uKEFOR1VMQVJfQ09SRSwgJ1NraXBTZWxmJyksIGNyZWF0ZVNraXBTZWxmKTtcbiAgICB0aGlzLl9yZWdpc3RlckRlY29yYXRvck9yQ29uc3RydWN0b3IoXG4gICAgICAgIHRoaXMuZmluZERlY2xhcmF0aW9uKEFOR1VMQVJfQ09SRSwgJ0luamVjdCcpLCBjcmVhdGVJbmplY3QpO1xuICAgIHRoaXMuX3JlZ2lzdGVyRGVjb3JhdG9yT3JDb25zdHJ1Y3RvcihcbiAgICAgICAgdGhpcy5maW5kRGVjbGFyYXRpb24oQU5HVUxBUl9DT1JFLCAnT3B0aW9uYWwnKSwgY3JlYXRlT3B0aW9uYWwpO1xuICAgIHRoaXMuX3JlZ2lzdGVyRGVjb3JhdG9yT3JDb25zdHJ1Y3RvcihcbiAgICAgICAgdGhpcy5maW5kRGVjbGFyYXRpb24oQU5HVUxBUl9DT1JFLCAnQXR0cmlidXRlJyksIGNyZWF0ZUF0dHJpYnV0ZSk7XG4gICAgdGhpcy5fcmVnaXN0ZXJEZWNvcmF0b3JPckNvbnN0cnVjdG9yKFxuICAgICAgICB0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdDb250ZW50Q2hpbGQnKSwgY3JlYXRlQ29udGVudENoaWxkKTtcbiAgICB0aGlzLl9yZWdpc3RlckRlY29yYXRvck9yQ29uc3RydWN0b3IoXG4gICAgICAgIHRoaXMuZmluZERlY2xhcmF0aW9uKEFOR1VMQVJfQ09SRSwgJ0NvbnRlbnRDaGlsZHJlbicpLCBjcmVhdGVDb250ZW50Q2hpbGRyZW4pO1xuICAgIHRoaXMuX3JlZ2lzdGVyRGVjb3JhdG9yT3JDb25zdHJ1Y3RvcihcbiAgICAgICAgdGhpcy5maW5kRGVjbGFyYXRpb24oQU5HVUxBUl9DT1JFLCAnVmlld0NoaWxkJyksIGNyZWF0ZVZpZXdDaGlsZCk7XG4gICAgdGhpcy5fcmVnaXN0ZXJEZWNvcmF0b3JPckNvbnN0cnVjdG9yKFxuICAgICAgICB0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdWaWV3Q2hpbGRyZW4nKSwgY3JlYXRlVmlld0NoaWxkcmVuKTtcbiAgICB0aGlzLl9yZWdpc3RlckRlY29yYXRvck9yQ29uc3RydWN0b3IodGhpcy5maW5kRGVjbGFyYXRpb24oQU5HVUxBUl9DT1JFLCAnSW5wdXQnKSwgY3JlYXRlSW5wdXQpO1xuICAgIHRoaXMuX3JlZ2lzdGVyRGVjb3JhdG9yT3JDb25zdHJ1Y3RvcihcbiAgICAgICAgdGhpcy5maW5kRGVjbGFyYXRpb24oQU5HVUxBUl9DT1JFLCAnT3V0cHV0JyksIGNyZWF0ZU91dHB1dCk7XG4gICAgdGhpcy5fcmVnaXN0ZXJEZWNvcmF0b3JPckNvbnN0cnVjdG9yKHRoaXMuZmluZERlY2xhcmF0aW9uKEFOR1VMQVJfQ09SRSwgJ1BpcGUnKSwgY3JlYXRlUGlwZSk7XG4gICAgdGhpcy5fcmVnaXN0ZXJEZWNvcmF0b3JPckNvbnN0cnVjdG9yKFxuICAgICAgICB0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdIb3N0QmluZGluZycpLCBjcmVhdGVIb3N0QmluZGluZyk7XG4gICAgdGhpcy5fcmVnaXN0ZXJEZWNvcmF0b3JPckNvbnN0cnVjdG9yKFxuICAgICAgICB0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdIb3N0TGlzdGVuZXInKSwgY3JlYXRlSG9zdExpc3RlbmVyKTtcbiAgICB0aGlzLl9yZWdpc3RlckRlY29yYXRvck9yQ29uc3RydWN0b3IoXG4gICAgICAgIHRoaXMuZmluZERlY2xhcmF0aW9uKEFOR1VMQVJfQ09SRSwgJ0RpcmVjdGl2ZScpLCBjcmVhdGVEaXJlY3RpdmUpO1xuICAgIHRoaXMuX3JlZ2lzdGVyRGVjb3JhdG9yT3JDb25zdHJ1Y3RvcihcbiAgICAgICAgdGhpcy5maW5kRGVjbGFyYXRpb24oQU5HVUxBUl9DT1JFLCAnQ29tcG9uZW50JyksIGNyZWF0ZUNvbXBvbmVudCk7XG4gICAgdGhpcy5fcmVnaXN0ZXJEZWNvcmF0b3JPckNvbnN0cnVjdG9yKFxuICAgICAgICB0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdOZ01vZHVsZScpLCBjcmVhdGVOZ01vZHVsZSk7XG5cbiAgICAvLyBOb3RlOiBTb21lIG1ldGFkYXRhIGNsYXNzZXMgY2FuIGJlIHVzZWQgZGlyZWN0bHkgd2l0aCBQcm92aWRlci5kZXBzLlxuICAgIHRoaXMuX3JlZ2lzdGVyRGVjb3JhdG9yT3JDb25zdHJ1Y3Rvcih0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdIb3N0JyksIGNyZWF0ZUhvc3QpO1xuICAgIHRoaXMuX3JlZ2lzdGVyRGVjb3JhdG9yT3JDb25zdHJ1Y3Rvcih0aGlzLmZpbmREZWNsYXJhdGlvbihBTkdVTEFSX0NPUkUsICdTZWxmJyksIGNyZWF0ZVNlbGYpO1xuICAgIHRoaXMuX3JlZ2lzdGVyRGVjb3JhdG9yT3JDb25zdHJ1Y3RvcihcbiAgICAgICAgdGhpcy5maW5kRGVjbGFyYXRpb24oQU5HVUxBUl9DT1JFLCAnU2tpcFNlbGYnKSwgY3JlYXRlU2tpcFNlbGYpO1xuICAgIHRoaXMuX3JlZ2lzdGVyRGVjb3JhdG9yT3JDb25zdHJ1Y3RvcihcbiAgICAgICAgdGhpcy5maW5kRGVjbGFyYXRpb24oQU5HVUxBUl9DT1JFLCAnT3B0aW9uYWwnKSwgY3JlYXRlT3B0aW9uYWwpO1xuICB9XG5cbiAgLyoqXG4gICAqIGdldFN0YXRpY1N5bWJvbCBwcm9kdWNlcyBhIFR5cGUgd2hvc2UgbWV0YWRhdGEgaXMga25vd24gYnV0IHdob3NlIGltcGxlbWVudGF0aW9uIGlzIG5vdCBsb2FkZWQuXG4gICAqIEFsbCB0eXBlcyBwYXNzZWQgdG8gdGhlIFN0YXRpY1Jlc29sdmVyIHNob3VsZCBiZSBwc2V1ZG8tdHlwZXMgcmV0dXJuZWQgYnkgdGhpcyBtZXRob2QuXG4gICAqXG4gICAqIEBwYXJhbSBkZWNsYXJhdGlvbkZpbGUgdGhlIGFic29sdXRlIHBhdGggb2YgdGhlIGZpbGUgd2hlcmUgdGhlIHN5bWJvbCBpcyBkZWNsYXJlZFxuICAgKiBAcGFyYW0gbmFtZSB0aGUgbmFtZSBvZiB0aGUgdHlwZS5cbiAgICovXG4gIGdldFN0YXRpY1N5bWJvbChkZWNsYXJhdGlvbkZpbGU6IHN0cmluZywgbmFtZTogc3RyaW5nLCBtZW1iZXJzPzogc3RyaW5nW10pOiBTdGF0aWNTeW1ib2wge1xuICAgIHJldHVybiB0aGlzLnN5bWJvbFJlc29sdmVyLmdldFN0YXRpY1N5bWJvbChkZWNsYXJhdGlvbkZpbGUsIG5hbWUsIG1lbWJlcnMpO1xuICB9XG5cbiAgLyoqXG4gICAqIFNpbXBsaWZ5IGJ1dCBkaXNjYXJkIGFueSBlcnJvcnNcbiAgICovXG4gIHByaXZhdGUgdHJ5U2ltcGxpZnkoY29udGV4dDogU3RhdGljU3ltYm9sLCB2YWx1ZTogYW55KTogYW55IHtcbiAgICBjb25zdCBvcmlnaW5hbFJlY29yZGVyID0gdGhpcy5lcnJvclJlY29yZGVyO1xuICAgIHRoaXMuZXJyb3JSZWNvcmRlciA9IChlcnJvcjogYW55LCBmaWxlTmFtZT86IHN0cmluZykgPT4ge307XG4gICAgY29uc3QgcmVzdWx0ID0gdGhpcy5zaW1wbGlmeShjb250ZXh0LCB2YWx1ZSk7XG4gICAgdGhpcy5lcnJvclJlY29yZGVyID0gb3JpZ2luYWxSZWNvcmRlcjtcbiAgICByZXR1cm4gcmVzdWx0O1xuICB9XG5cbiAgLyoqIEBpbnRlcm5hbCAqL1xuICBwdWJsaWMgc2ltcGxpZnkoY29udGV4dDogU3RhdGljU3ltYm9sLCB2YWx1ZTogYW55LCBsYXp5OiBib29sZWFuID0gZmFsc2UpOiBhbnkge1xuICAgIGNvbnN0IHNlbGYgPSB0aGlzO1xuICAgIGxldCBzY29wZSA9IEJpbmRpbmdTY29wZS5lbXB0eTtcbiAgICBjb25zdCBjYWxsaW5nID0gbmV3IE1hcDxTdGF0aWNTeW1ib2wsIGJvb2xlYW4+KCk7XG4gICAgY29uc3Qgcm9vdENvbnRleHQgPSBjb250ZXh0O1xuXG4gICAgZnVuY3Rpb24gc2ltcGxpZnlJbkNvbnRleHQoXG4gICAgICAgIGNvbnRleHQ6IFN0YXRpY1N5bWJvbCwgdmFsdWU6IGFueSwgZGVwdGg6IG51bWJlciwgcmVmZXJlbmNlczogbnVtYmVyKTogYW55IHtcbiAgICAgIGZ1bmN0aW9uIHJlc29sdmVSZWZlcmVuY2VWYWx1ZShzdGF0aWNTeW1ib2w6IFN0YXRpY1N5bWJvbCk6IGFueSB7XG4gICAgICAgIGNvbnN0IHJlc29sdmVkU3ltYm9sID0gc2VsZi5zeW1ib2xSZXNvbHZlci5yZXNvbHZlU3ltYm9sKHN0YXRpY1N5bWJvbCk7XG4gICAgICAgIHJldHVybiByZXNvbHZlZFN5bWJvbCA/IHJlc29sdmVkU3ltYm9sLm1ldGFkYXRhIDogbnVsbDtcbiAgICAgIH1cblxuICAgICAgZnVuY3Rpb24gc2ltcGxpZnlFYWdlcmx5KHZhbHVlOiBhbnkpOiBhbnkge1xuICAgICAgICByZXR1cm4gc2ltcGxpZnlJbkNvbnRleHQoY29udGV4dCwgdmFsdWUsIGRlcHRoLCAwKTtcbiAgICAgIH1cblxuICAgICAgZnVuY3Rpb24gc2ltcGxpZnlMYXppbHkodmFsdWU6IGFueSk6IGFueSB7XG4gICAgICAgIHJldHVybiBzaW1wbGlmeUluQ29udGV4dChjb250ZXh0LCB2YWx1ZSwgZGVwdGgsIHJlZmVyZW5jZXMgKyAxKTtcbiAgICAgIH1cblxuICAgICAgZnVuY3Rpb24gc2ltcGxpZnlOZXN0ZWQobmVzdGVkQ29udGV4dDogU3RhdGljU3ltYm9sLCB2YWx1ZTogYW55KTogYW55IHtcbiAgICAgICAgaWYgKG5lc3RlZENvbnRleHQgPT09IGNvbnRleHQpIHtcbiAgICAgICAgICAvLyBJZiB0aGUgY29udGV4dCBoYXNuJ3QgY2hhbmdlZCBsZXQgdGhlIGV4Y2VwdGlvbiBwcm9wYWdhdGUgdW5tb2RpZmllZC5cbiAgICAgICAgICByZXR1cm4gc2ltcGxpZnlJbkNvbnRleHQobmVzdGVkQ29udGV4dCwgdmFsdWUsIGRlcHRoICsgMSwgcmVmZXJlbmNlcyk7XG4gICAgICAgIH1cbiAgICAgICAgdHJ5IHtcbiAgICAgICAgICByZXR1cm4gc2ltcGxpZnlJbkNvbnRleHQobmVzdGVkQ29udGV4dCwgdmFsdWUsIGRlcHRoICsgMSwgcmVmZXJlbmNlcyk7XG4gICAgICAgIH0gY2F0Y2ggKGUpIHtcbiAgICAgICAgICBpZiAoaXNNZXRhZGF0YUVycm9yKGUpKSB7XG4gICAgICAgICAgICAvLyBQcm9wYWdhdGUgdGhlIG1lc3NhZ2UgdGV4dCB1cCBidXQgYWRkIGEgbWVzc2FnZSB0byB0aGUgY2hhaW4gdGhhdCBleHBsYWlucyBob3cgd2UgZ290XG4gICAgICAgICAgICAvLyBoZXJlLlxuICAgICAgICAgICAgLy8gZS5jaGFpbiBpbXBsaWVzIGUuc3ltYm9sXG4gICAgICAgICAgICBjb25zdCBzdW1tYXJ5TXNnID0gZS5jaGFpbiA/ICdyZWZlcmVuY2VzIFxcJycgKyBlLnN5bWJvbCEubmFtZSArICdcXCcnIDogZXJyb3JTdW1tYXJ5KGUpO1xuICAgICAgICAgICAgY29uc3Qgc3VtbWFyeSA9IGAnJHtuZXN0ZWRDb250ZXh0Lm5hbWV9JyAke3N1bW1hcnlNc2d9YDtcbiAgICAgICAgICAgIGNvbnN0IGNoYWluID0ge21lc3NhZ2U6IHN1bW1hcnksIHBvc2l0aW9uOiBlLnBvc2l0aW9uLCBuZXh0OiBlLmNoYWlufTtcbiAgICAgICAgICAgIC8vIFRPRE8oY2h1Y2tqKTogcmV0cmlldmUgdGhlIHBvc2l0aW9uIGluZm9ybWF0aW9uIGluZGlyZWN0bHkgZnJvbSB0aGUgY29sbGVjdG9ycyBub2RlXG4gICAgICAgICAgICAvLyBtYXAgaWYgdGhlIG1ldGFkYXRhIGlzIGZyb20gYSAudHMgZmlsZS5cbiAgICAgICAgICAgIHNlbGYuZXJyb3IoXG4gICAgICAgICAgICAgICAge1xuICAgICAgICAgICAgICAgICAgbWVzc2FnZTogZS5tZXNzYWdlLFxuICAgICAgICAgICAgICAgICAgYWR2aXNlOiBlLmFkdmlzZSxcbiAgICAgICAgICAgICAgICAgIGNvbnRleHQ6IGUuY29udGV4dCxcbiAgICAgICAgICAgICAgICAgIGNoYWluLFxuICAgICAgICAgICAgICAgICAgc3ltYm9sOiBuZXN0ZWRDb250ZXh0XG4gICAgICAgICAgICAgICAgfSxcbiAgICAgICAgICAgICAgICBjb250ZXh0KTtcbiAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgLy8gSXQgaXMgcHJvYmFibHkgYW4gaW50ZXJuYWwgZXJyb3IuXG4gICAgICAgICAgICB0aHJvdyBlO1xuICAgICAgICAgIH1cbiAgICAgICAgfVxuICAgICAgfVxuXG4gICAgICBmdW5jdGlvbiBzaW1wbGlmeUNhbGwoXG4gICAgICAgICAgZnVuY3Rpb25TeW1ib2w6IFN0YXRpY1N5bWJvbCwgdGFyZ2V0RnVuY3Rpb246IGFueSwgYXJnczogYW55W10sIHRhcmdldEV4cHJlc3Npb246IGFueSkge1xuICAgICAgICBpZiAodGFyZ2V0RnVuY3Rpb24gJiYgdGFyZ2V0RnVuY3Rpb25bJ19fc3ltYm9saWMnXSA9PSAnZnVuY3Rpb24nKSB7XG4gICAgICAgICAgaWYgKGNhbGxpbmcuZ2V0KGZ1bmN0aW9uU3ltYm9sKSkge1xuICAgICAgICAgICAgc2VsZi5lcnJvcihcbiAgICAgICAgICAgICAgICB7XG4gICAgICAgICAgICAgICAgICBtZXNzYWdlOiAnUmVjdXJzaW9uIGlzIG5vdCBzdXBwb3J0ZWQnLFxuICAgICAgICAgICAgICAgICAgc3VtbWFyeTogYGNhbGxlZCAnJHtmdW5jdGlvblN5bWJvbC5uYW1lfScgcmVjdXJzaXZlbHlgLFxuICAgICAgICAgICAgICAgICAgdmFsdWU6IHRhcmdldEZ1bmN0aW9uXG4gICAgICAgICAgICAgICAgfSxcbiAgICAgICAgICAgICAgICBmdW5jdGlvblN5bWJvbCk7XG4gICAgICAgICAgfVxuICAgICAgICAgIHRyeSB7XG4gICAgICAgICAgICBjb25zdCB2YWx1ZSA9IHRhcmdldEZ1bmN0aW9uWyd2YWx1ZSddO1xuICAgICAgICAgICAgaWYgKHZhbHVlICYmIChkZXB0aCAhPSAwIHx8IHZhbHVlLl9fc3ltYm9saWMgIT0gJ2Vycm9yJykpIHtcbiAgICAgICAgICAgICAgY29uc3QgcGFyYW1ldGVyczogc3RyaW5nW10gPSB0YXJnZXRGdW5jdGlvblsncGFyYW1ldGVycyddO1xuICAgICAgICAgICAgICBjb25zdCBkZWZhdWx0czogYW55W10gPSB0YXJnZXRGdW5jdGlvbi5kZWZhdWx0cztcbiAgICAgICAgICAgICAgYXJncyA9IGFyZ3MubWFwKGFyZyA9PiBzaW1wbGlmeU5lc3RlZChjb250ZXh0LCBhcmcpKVxuICAgICAgICAgICAgICAgICAgICAgICAgIC5tYXAoYXJnID0+IHNob3VsZElnbm9yZShhcmcpID8gdW5kZWZpbmVkIDogYXJnKTtcbiAgICAgICAgICAgICAgaWYgKGRlZmF1bHRzICYmIGRlZmF1bHRzLmxlbmd0aCA+IGFyZ3MubGVuZ3RoKSB7XG4gICAgICAgICAgICAgICAgYXJncy5wdXNoKC4uLmRlZmF1bHRzLnNsaWNlKGFyZ3MubGVuZ3RoKS5tYXAoKHZhbHVlOiBhbnkpID0+IHNpbXBsaWZ5KHZhbHVlKSkpO1xuICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgIGNhbGxpbmcuc2V0KGZ1bmN0aW9uU3ltYm9sLCB0cnVlKTtcbiAgICAgICAgICAgICAgY29uc3QgZnVuY3Rpb25TY29wZSA9IEJpbmRpbmdTY29wZS5idWlsZCgpO1xuICAgICAgICAgICAgICBmb3IgKGxldCBpID0gMDsgaSA8IHBhcmFtZXRlcnMubGVuZ3RoOyBpKyspIHtcbiAgICAgICAgICAgICAgICBmdW5jdGlvblNjb3BlLmRlZmluZShwYXJhbWV0ZXJzW2ldLCBhcmdzW2ldKTtcbiAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICBjb25zdCBvbGRTY29wZSA9IHNjb3BlO1xuICAgICAgICAgICAgICBsZXQgcmVzdWx0OiBhbnk7XG4gICAgICAgICAgICAgIHRyeSB7XG4gICAgICAgICAgICAgICAgc2NvcGUgPSBmdW5jdGlvblNjb3BlLmRvbmUoKTtcbiAgICAgICAgICAgICAgICByZXN1bHQgPSBzaW1wbGlmeU5lc3RlZChmdW5jdGlvblN5bWJvbCwgdmFsdWUpO1xuICAgICAgICAgICAgICB9IGZpbmFsbHkge1xuICAgICAgICAgICAgICAgIHNjb3BlID0gb2xkU2NvcGU7XG4gICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgcmV0dXJuIHJlc3VsdDtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICB9IGZpbmFsbHkge1xuICAgICAgICAgICAgY2FsbGluZy5kZWxldGUoZnVuY3Rpb25TeW1ib2wpO1xuICAgICAgICAgIH1cbiAgICAgICAgfVxuXG4gICAgICAgIGlmIChkZXB0aCA9PT0gMCkge1xuICAgICAgICAgIC8vIElmIGRlcHRoIGlzIDAgd2UgYXJlIGV2YWx1YXRpbmcgdGhlIHRvcCBsZXZlbCBleHByZXNzaW9uIHRoYXQgaXMgZGVzY3JpYmluZyBlbGVtZW50XG4gICAgICAgICAgLy8gZGVjb3JhdG9yLiBJbiB0aGlzIGNhc2UsIGl0IGlzIGEgZGVjb3JhdG9yIHdlIGRvbid0IHVuZGVyc3RhbmQsIHN1Y2ggYXMgYSBjdXN0b21cbiAgICAgICAgICAvLyBub24tYW5ndWxhciBkZWNvcmF0b3IsIGFuZCB3ZSBzaG91bGQganVzdCBpZ25vcmUgaXQuXG4gICAgICAgICAgcmV0dXJuIElHTk9SRTtcbiAgICAgICAgfVxuICAgICAgICBsZXQgcG9zaXRpb246IFBvc2l0aW9ufHVuZGVmaW5lZCA9IHVuZGVmaW5lZDtcbiAgICAgICAgaWYgKHRhcmdldEV4cHJlc3Npb24gJiYgdGFyZ2V0RXhwcmVzc2lvbi5fX3N5bWJvbGljID09ICdyZXNvbHZlZCcpIHtcbiAgICAgICAgICBjb25zdCBsaW5lID0gdGFyZ2V0RXhwcmVzc2lvbi5saW5lO1xuICAgICAgICAgIGNvbnN0IGNoYXJhY3RlciA9IHRhcmdldEV4cHJlc3Npb24uY2hhcmFjdGVyO1xuICAgICAgICAgIGNvbnN0IGZpbGVOYW1lID0gdGFyZ2V0RXhwcmVzc2lvbi5maWxlTmFtZTtcbiAgICAgICAgICBpZiAoZmlsZU5hbWUgIT0gbnVsbCAmJiBsaW5lICE9IG51bGwgJiYgY2hhcmFjdGVyICE9IG51bGwpIHtcbiAgICAgICAgICAgIHBvc2l0aW9uID0ge2ZpbGVOYW1lLCBsaW5lLCBjb2x1bW46IGNoYXJhY3Rlcn07XG4gICAgICAgICAgfVxuICAgICAgICB9XG4gICAgICAgIHNlbGYuZXJyb3IoXG4gICAgICAgICAgICB7XG4gICAgICAgICAgICAgIG1lc3NhZ2U6IEZVTkNUSU9OX0NBTExfTk9UX1NVUFBPUlRFRCxcbiAgICAgICAgICAgICAgY29udGV4dDogZnVuY3Rpb25TeW1ib2wsXG4gICAgICAgICAgICAgIHZhbHVlOiB0YXJnZXRGdW5jdGlvbixcbiAgICAgICAgICAgICAgcG9zaXRpb25cbiAgICAgICAgICAgIH0sXG4gICAgICAgICAgICBjb250ZXh0KTtcbiAgICAgIH1cblxuICAgICAgZnVuY3Rpb24gc2ltcGxpZnkoZXhwcmVzc2lvbjogYW55KTogYW55IHtcbiAgICAgICAgaWYgKGlzUHJpbWl0aXZlKGV4cHJlc3Npb24pKSB7XG4gICAgICAgICAgcmV0dXJuIGV4cHJlc3Npb247XG4gICAgICAgIH1cbiAgICAgICAgaWYgKEFycmF5LmlzQXJyYXkoZXhwcmVzc2lvbikpIHtcbiAgICAgICAgICBjb25zdCByZXN1bHQ6IGFueVtdID0gW107XG4gICAgICAgICAgZm9yIChjb25zdCBpdGVtIG9mICg8YW55PmV4cHJlc3Npb24pKSB7XG4gICAgICAgICAgICAvLyBDaGVjayBmb3IgYSBzcHJlYWQgZXhwcmVzc2lvblxuICAgICAgICAgICAgaWYgKGl0ZW0gJiYgaXRlbS5fX3N5bWJvbGljID09PSAnc3ByZWFkJykge1xuICAgICAgICAgICAgICAvLyBXZSBjYWxsIHdpdGggcmVmZXJlbmNlcyBhcyAwIGJlY2F1c2Ugd2UgcmVxdWlyZSB0aGUgYWN0dWFsIHZhbHVlIGFuZCBjYW5ub3RcbiAgICAgICAgICAgICAgLy8gdG9sZXJhdGUgYSByZWZlcmVuY2UgaGVyZS5cbiAgICAgICAgICAgICAgY29uc3Qgc3ByZWFkQXJyYXkgPSBzaW1wbGlmeUVhZ2VybHkoaXRlbS5leHByZXNzaW9uKTtcbiAgICAgICAgICAgICAgaWYgKEFycmF5LmlzQXJyYXkoc3ByZWFkQXJyYXkpKSB7XG4gICAgICAgICAgICAgICAgZm9yIChjb25zdCBzcHJlYWRJdGVtIG9mIHNwcmVhZEFycmF5KSB7XG4gICAgICAgICAgICAgICAgICByZXN1bHQucHVzaChzcHJlYWRJdGVtKTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgY29udGludWU7XG4gICAgICAgICAgICAgIH1cbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIGNvbnN0IHZhbHVlID0gc2ltcGxpZnkoaXRlbSk7XG4gICAgICAgICAgICBpZiAoc2hvdWxkSWdub3JlKHZhbHVlKSkge1xuICAgICAgICAgICAgICBjb250aW51ZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJlc3VsdC5wdXNoKHZhbHVlKTtcbiAgICAgICAgICB9XG4gICAgICAgICAgcmV0dXJuIHJlc3VsdDtcbiAgICAgICAgfVxuICAgICAgICBpZiAoZXhwcmVzc2lvbiBpbnN0YW5jZW9mIFN0YXRpY1N5bWJvbCkge1xuICAgICAgICAgIC8vIFN0b3Agc2ltcGxpZmljYXRpb24gYXQgYnVpbHRpbiBzeW1ib2xzIG9yIGlmIHdlIGFyZSBpbiBhIHJlZmVyZW5jZSBjb250ZXh0IGFuZFxuICAgICAgICAgIC8vIHRoZSBzeW1ib2wgZG9lc24ndCBoYXZlIG1lbWJlcnMuXG4gICAgICAgICAgaWYgKGV4cHJlc3Npb24gPT09IHNlbGYuaW5qZWN0aW9uVG9rZW4gfHwgc2VsZi5jb252ZXJzaW9uTWFwLmhhcyhleHByZXNzaW9uKSB8fFxuICAgICAgICAgICAgICAocmVmZXJlbmNlcyA+IDAgJiYgIWV4cHJlc3Npb24ubWVtYmVycy5sZW5ndGgpKSB7XG4gICAgICAgICAgICByZXR1cm4gZXhwcmVzc2lvbjtcbiAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgY29uc3Qgc3RhdGljU3ltYm9sID0gZXhwcmVzc2lvbjtcbiAgICAgICAgICAgIGNvbnN0IGRlY2xhcmF0aW9uVmFsdWUgPSByZXNvbHZlUmVmZXJlbmNlVmFsdWUoc3RhdGljU3ltYm9sKTtcbiAgICAgICAgICAgIGlmIChkZWNsYXJhdGlvblZhbHVlICE9IG51bGwpIHtcbiAgICAgICAgICAgICAgcmV0dXJuIHNpbXBsaWZ5TmVzdGVkKHN0YXRpY1N5bWJvbCwgZGVjbGFyYXRpb25WYWx1ZSk7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICByZXR1cm4gc3RhdGljU3ltYm9sO1xuICAgICAgICAgICAgfVxuICAgICAgICAgIH1cbiAgICAgICAgfVxuICAgICAgICBpZiAoZXhwcmVzc2lvbikge1xuICAgICAgICAgIGlmIChleHByZXNzaW9uWydfX3N5bWJvbGljJ10pIHtcbiAgICAgICAgICAgIGxldCBzdGF0aWNTeW1ib2w6IFN0YXRpY1N5bWJvbDtcbiAgICAgICAgICAgIHN3aXRjaCAoZXhwcmVzc2lvblsnX19zeW1ib2xpYyddKSB7XG4gICAgICAgICAgICAgIGNhc2UgJ2Jpbm9wJzpcbiAgICAgICAgICAgICAgICBsZXQgbGVmdCA9IHNpbXBsaWZ5KGV4cHJlc3Npb25bJ2xlZnQnXSk7XG4gICAgICAgICAgICAgICAgaWYgKHNob3VsZElnbm9yZShsZWZ0KSkgcmV0dXJuIGxlZnQ7XG4gICAgICAgICAgICAgICAgbGV0IHJpZ2h0ID0gc2ltcGxpZnkoZXhwcmVzc2lvblsncmlnaHQnXSk7XG4gICAgICAgICAgICAgICAgaWYgKHNob3VsZElnbm9yZShyaWdodCkpIHJldHVybiByaWdodDtcbiAgICAgICAgICAgICAgICBzd2l0Y2ggKGV4cHJlc3Npb25bJ29wZXJhdG9yJ10pIHtcbiAgICAgICAgICAgICAgICAgIGNhc2UgJyYmJzpcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIGxlZnQgJiYgcmlnaHQ7XG4gICAgICAgICAgICAgICAgICBjYXNlICd8fCc6XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiBsZWZ0IHx8IHJpZ2h0O1xuICAgICAgICAgICAgICAgICAgY2FzZSAnfCc6XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiBsZWZ0IHwgcmlnaHQ7XG4gICAgICAgICAgICAgICAgICBjYXNlICdeJzpcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIGxlZnQgXiByaWdodDtcbiAgICAgICAgICAgICAgICAgIGNhc2UgJyYnOlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gbGVmdCAmIHJpZ2h0O1xuICAgICAgICAgICAgICAgICAgY2FzZSAnPT0nOlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gbGVmdCA9PSByaWdodDtcbiAgICAgICAgICAgICAgICAgIGNhc2UgJyE9JzpcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIGxlZnQgIT0gcmlnaHQ7XG4gICAgICAgICAgICAgICAgICBjYXNlICc9PT0nOlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gbGVmdCA9PT0gcmlnaHQ7XG4gICAgICAgICAgICAgICAgICBjYXNlICchPT0nOlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gbGVmdCAhPT0gcmlnaHQ7XG4gICAgICAgICAgICAgICAgICBjYXNlICc8JzpcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIGxlZnQgPCByaWdodDtcbiAgICAgICAgICAgICAgICAgIGNhc2UgJz4nOlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gbGVmdCA+IHJpZ2h0O1xuICAgICAgICAgICAgICAgICAgY2FzZSAnPD0nOlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gbGVmdCA8PSByaWdodDtcbiAgICAgICAgICAgICAgICAgIGNhc2UgJz49JzpcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIGxlZnQgPj0gcmlnaHQ7XG4gICAgICAgICAgICAgICAgICBjYXNlICc8PCc6XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiBsZWZ0IDw8IHJpZ2h0O1xuICAgICAgICAgICAgICAgICAgY2FzZSAnPj4nOlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gbGVmdCA+PiByaWdodDtcbiAgICAgICAgICAgICAgICAgIGNhc2UgJysnOlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gbGVmdCArIHJpZ2h0O1xuICAgICAgICAgICAgICAgICAgY2FzZSAnLSc6XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiBsZWZ0IC0gcmlnaHQ7XG4gICAgICAgICAgICAgICAgICBjYXNlICcqJzpcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIGxlZnQgKiByaWdodDtcbiAgICAgICAgICAgICAgICAgIGNhc2UgJy8nOlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gbGVmdCAvIHJpZ2h0O1xuICAgICAgICAgICAgICAgICAgY2FzZSAnJSc6XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiBsZWZ0ICUgcmlnaHQ7XG4gICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgIHJldHVybiBudWxsO1xuICAgICAgICAgICAgICBjYXNlICdpZic6XG4gICAgICAgICAgICAgICAgbGV0IGNvbmRpdGlvbiA9IHNpbXBsaWZ5KGV4cHJlc3Npb25bJ2NvbmRpdGlvbiddKTtcbiAgICAgICAgICAgICAgICByZXR1cm4gY29uZGl0aW9uID8gc2ltcGxpZnkoZXhwcmVzc2lvblsndGhlbkV4cHJlc3Npb24nXSkgOlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzaW1wbGlmeShleHByZXNzaW9uWydlbHNlRXhwcmVzc2lvbiddKTtcbiAgICAgICAgICAgICAgY2FzZSAncHJlJzpcbiAgICAgICAgICAgICAgICBsZXQgb3BlcmFuZCA9IHNpbXBsaWZ5KGV4cHJlc3Npb25bJ29wZXJhbmQnXSk7XG4gICAgICAgICAgICAgICAgaWYgKHNob3VsZElnbm9yZShvcGVyYW5kKSkgcmV0dXJuIG9wZXJhbmQ7XG4gICAgICAgICAgICAgICAgc3dpdGNoIChleHByZXNzaW9uWydvcGVyYXRvciddKSB7XG4gICAgICAgICAgICAgICAgICBjYXNlICcrJzpcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIG9wZXJhbmQ7XG4gICAgICAgICAgICAgICAgICBjYXNlICctJzpcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIC1vcGVyYW5kO1xuICAgICAgICAgICAgICAgICAgY2FzZSAnISc6XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiAhb3BlcmFuZDtcbiAgICAgICAgICAgICAgICAgIGNhc2UgJ34nOlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gfm9wZXJhbmQ7XG4gICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgIHJldHVybiBudWxsO1xuICAgICAgICAgICAgICBjYXNlICdpbmRleCc6XG4gICAgICAgICAgICAgICAgbGV0IGluZGV4VGFyZ2V0ID0gc2ltcGxpZnlFYWdlcmx5KGV4cHJlc3Npb25bJ2V4cHJlc3Npb24nXSk7XG4gICAgICAgICAgICAgICAgbGV0IGluZGV4ID0gc2ltcGxpZnlFYWdlcmx5KGV4cHJlc3Npb25bJ2luZGV4J10pO1xuICAgICAgICAgICAgICAgIGlmIChpbmRleFRhcmdldCAmJiBpc1ByaW1pdGl2ZShpbmRleCkpIHJldHVybiBpbmRleFRhcmdldFtpbmRleF07XG4gICAgICAgICAgICAgICAgcmV0dXJuIG51bGw7XG4gICAgICAgICAgICAgIGNhc2UgJ3NlbGVjdCc6XG4gICAgICAgICAgICAgICAgY29uc3QgbWVtYmVyID0gZXhwcmVzc2lvblsnbWVtYmVyJ107XG4gICAgICAgICAgICAgICAgbGV0IHNlbGVjdENvbnRleHQgPSBjb250ZXh0O1xuICAgICAgICAgICAgICAgIGxldCBzZWxlY3RUYXJnZXQgPSBzaW1wbGlmeShleHByZXNzaW9uWydleHByZXNzaW9uJ10pO1xuICAgICAgICAgICAgICAgIGlmIChzZWxlY3RUYXJnZXQgaW5zdGFuY2VvZiBTdGF0aWNTeW1ib2wpIHtcbiAgICAgICAgICAgICAgICAgIGNvbnN0IG1lbWJlcnMgPSBzZWxlY3RUYXJnZXQubWVtYmVycy5jb25jYXQobWVtYmVyKTtcbiAgICAgICAgICAgICAgICAgIHNlbGVjdENvbnRleHQgPVxuICAgICAgICAgICAgICAgICAgICAgIHNlbGYuZ2V0U3RhdGljU3ltYm9sKHNlbGVjdFRhcmdldC5maWxlUGF0aCwgc2VsZWN0VGFyZ2V0Lm5hbWUsIG1lbWJlcnMpO1xuICAgICAgICAgICAgICAgICAgY29uc3QgZGVjbGFyYXRpb25WYWx1ZSA9IHJlc29sdmVSZWZlcmVuY2VWYWx1ZShzZWxlY3RDb250ZXh0KTtcbiAgICAgICAgICAgICAgICAgIGlmIChkZWNsYXJhdGlvblZhbHVlICE9IG51bGwpIHtcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIHNpbXBsaWZ5TmVzdGVkKHNlbGVjdENvbnRleHQsIGRlY2xhcmF0aW9uVmFsdWUpO1xuICAgICAgICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIHNlbGVjdENvbnRleHQ7XG4gICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgIGlmIChzZWxlY3RUYXJnZXQgJiYgaXNQcmltaXRpdmUobWVtYmVyKSlcbiAgICAgICAgICAgICAgICAgIHJldHVybiBzaW1wbGlmeU5lc3RlZChzZWxlY3RDb250ZXh0LCBzZWxlY3RUYXJnZXRbbWVtYmVyXSk7XG4gICAgICAgICAgICAgICAgcmV0dXJuIG51bGw7XG4gICAgICAgICAgICAgIGNhc2UgJ3JlZmVyZW5jZSc6XG4gICAgICAgICAgICAgICAgLy8gTm90ZTogVGhpcyBvbmx5IGhhcyB0byBkZWFsIHdpdGggdmFyaWFibGUgcmVmZXJlbmNlcywgYXMgc3ltYm9sIHJlZmVyZW5jZXMgaGF2ZVxuICAgICAgICAgICAgICAgIC8vIGJlZW4gY29udmVydGVkIGludG8gJ3Jlc29sdmVkJ1xuICAgICAgICAgICAgICAgIC8vIGluIHRoZSBTdGF0aWNTeW1ib2xSZXNvbHZlci5cbiAgICAgICAgICAgICAgICBjb25zdCBuYW1lOiBzdHJpbmcgPSBleHByZXNzaW9uWyduYW1lJ107XG4gICAgICAgICAgICAgICAgY29uc3QgbG9jYWxWYWx1ZSA9IHNjb3BlLnJlc29sdmUobmFtZSk7XG4gICAgICAgICAgICAgICAgaWYgKGxvY2FsVmFsdWUgIT0gQmluZGluZ1Njb3BlLm1pc3NpbmcpIHtcbiAgICAgICAgICAgICAgICAgIHJldHVybiBsb2NhbFZhbHVlO1xuICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICBicmVhaztcbiAgICAgICAgICAgICAgY2FzZSAncmVzb2x2ZWQnOlxuICAgICAgICAgICAgICAgIHRyeSB7XG4gICAgICAgICAgICAgICAgICByZXR1cm4gc2ltcGxpZnkoZXhwcmVzc2lvbi5zeW1ib2wpO1xuICAgICAgICAgICAgICAgIH0gY2F0Y2ggKGUpIHtcbiAgICAgICAgICAgICAgICAgIC8vIElmIGFuIGVycm9yIGlzIHJlcG9ydGVkIGV2YWx1YXRpbmcgdGhlIHN5bWJvbCByZWNvcmQgdGhlIHBvc2l0aW9uIG9mIHRoZVxuICAgICAgICAgICAgICAgICAgLy8gcmVmZXJlbmNlIGluIHRoZSBlcnJvciBzbyBpdCBjYW5cbiAgICAgICAgICAgICAgICAgIC8vIGJlIHJlcG9ydGVkIGluIHRoZSBlcnJvciBtZXNzYWdlIGdlbmVyYXRlZCBmcm9tIHRoZSBleGNlcHRpb24uXG4gICAgICAgICAgICAgICAgICBpZiAoaXNNZXRhZGF0YUVycm9yKGUpICYmIGV4cHJlc3Npb24uZmlsZU5hbWUgIT0gbnVsbCAmJlxuICAgICAgICAgICAgICAgICAgICAgIGV4cHJlc3Npb24ubGluZSAhPSBudWxsICYmIGV4cHJlc3Npb24uY2hhcmFjdGVyICE9IG51bGwpIHtcbiAgICAgICAgICAgICAgICAgICAgZS5wb3NpdGlvbiA9IHtcbiAgICAgICAgICAgICAgICAgICAgICBmaWxlTmFtZTogZXhwcmVzc2lvbi5maWxlTmFtZSxcbiAgICAgICAgICAgICAgICAgICAgICBsaW5lOiBleHByZXNzaW9uLmxpbmUsXG4gICAgICAgICAgICAgICAgICAgICAgY29sdW1uOiBleHByZXNzaW9uLmNoYXJhY3RlclxuICAgICAgICAgICAgICAgICAgICB9O1xuICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgdGhyb3cgZTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgIGNhc2UgJ2NsYXNzJzpcbiAgICAgICAgICAgICAgICByZXR1cm4gY29udGV4dDtcbiAgICAgICAgICAgICAgY2FzZSAnZnVuY3Rpb24nOlxuICAgICAgICAgICAgICAgIHJldHVybiBjb250ZXh0O1xuICAgICAgICAgICAgICBjYXNlICduZXcnOlxuICAgICAgICAgICAgICBjYXNlICdjYWxsJzpcbiAgICAgICAgICAgICAgICAvLyBEZXRlcm1pbmUgaWYgdGhlIGZ1bmN0aW9uIGlzIGEgYnVpbHQtaW4gY29udmVyc2lvblxuICAgICAgICAgICAgICAgIHN0YXRpY1N5bWJvbCA9IHNpbXBsaWZ5SW5Db250ZXh0KFxuICAgICAgICAgICAgICAgICAgICBjb250ZXh0LCBleHByZXNzaW9uWydleHByZXNzaW9uJ10sIGRlcHRoICsgMSwgLyogcmVmZXJlbmNlcyAqLyAwKTtcbiAgICAgICAgICAgICAgICBpZiAoc3RhdGljU3ltYm9sIGluc3RhbmNlb2YgU3RhdGljU3ltYm9sKSB7XG4gICAgICAgICAgICAgICAgICBpZiAoc3RhdGljU3ltYm9sID09PSBzZWxmLmluamVjdGlvblRva2VuIHx8IHN0YXRpY1N5bWJvbCA9PT0gc2VsZi5vcGFxdWVUb2tlbikge1xuICAgICAgICAgICAgICAgICAgICAvLyBpZiBzb21lYm9keSBjYWxscyBuZXcgSW5qZWN0aW9uVG9rZW4sIGRvbid0IGNyZWF0ZSBhbiBJbmplY3Rpb25Ub2tlbixcbiAgICAgICAgICAgICAgICAgICAgLy8gYnV0IHJhdGhlciByZXR1cm4gdGhlIHN5bWJvbCB0byB3aGljaCB0aGUgSW5qZWN0aW9uVG9rZW4gaXMgYXNzaWduZWQgdG8uXG5cbiAgICAgICAgICAgICAgICAgICAgLy8gT3BhcXVlVG9rZW4gaXMgc3VwcG9ydGVkIHRvbyBhcyBpdCBpcyByZXF1aXJlZCBieSB0aGUgbGFuZ3VhZ2Ugc2VydmljZSB0b1xuICAgICAgICAgICAgICAgICAgICAvLyBzdXBwb3J0IHY0IGFuZCBwcmlvciB2ZXJzaW9ucyBvZiBBbmd1bGFyLlxuICAgICAgICAgICAgICAgICAgICByZXR1cm4gY29udGV4dDtcbiAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgIGNvbnN0IGFyZ0V4cHJlc3Npb25zOiBhbnlbXSA9IGV4cHJlc3Npb25bJ2FyZ3VtZW50cyddIHx8IFtdO1xuICAgICAgICAgICAgICAgICAgbGV0IGNvbnZlcnRlciA9IHNlbGYuY29udmVyc2lvbk1hcC5nZXQoc3RhdGljU3ltYm9sKTtcbiAgICAgICAgICAgICAgICAgIGlmIChjb252ZXJ0ZXIpIHtcbiAgICAgICAgICAgICAgICAgICAgY29uc3QgYXJncyA9IGFyZ0V4cHJlc3Npb25zLm1hcChhcmcgPT4gc2ltcGxpZnlOZXN0ZWQoY29udGV4dCwgYXJnKSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAubWFwKGFyZyA9PiBzaG91bGRJZ25vcmUoYXJnKSA/IHVuZGVmaW5lZCA6IGFyZyk7XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiBjb252ZXJ0ZXIoY29udGV4dCwgYXJncyk7XG4gICAgICAgICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgICAgICAvLyBEZXRlcm1pbmUgaWYgdGhlIGZ1bmN0aW9uIGlzIG9uZSB3ZSBjYW4gc2ltcGxpZnkuXG4gICAgICAgICAgICAgICAgICAgIGNvbnN0IHRhcmdldEZ1bmN0aW9uID0gcmVzb2x2ZVJlZmVyZW5jZVZhbHVlKHN0YXRpY1N5bWJvbCk7XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiBzaW1wbGlmeUNhbGwoXG4gICAgICAgICAgICAgICAgICAgICAgICBzdGF0aWNTeW1ib2wsIHRhcmdldEZ1bmN0aW9uLCBhcmdFeHByZXNzaW9ucywgZXhwcmVzc2lvblsnZXhwcmVzc2lvbiddKTtcbiAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgcmV0dXJuIElHTk9SRTtcbiAgICAgICAgICAgICAgY2FzZSAnZXJyb3InOlxuICAgICAgICAgICAgICAgIGxldCBtZXNzYWdlID0gZXhwcmVzc2lvbi5tZXNzYWdlO1xuICAgICAgICAgICAgICAgIGlmIChleHByZXNzaW9uWydsaW5lJ10gIT0gbnVsbCkge1xuICAgICAgICAgICAgICAgICAgc2VsZi5lcnJvcihcbiAgICAgICAgICAgICAgICAgICAgICB7XG4gICAgICAgICAgICAgICAgICAgICAgICBtZXNzYWdlLFxuICAgICAgICAgICAgICAgICAgICAgICAgY29udGV4dDogZXhwcmVzc2lvbi5jb250ZXh0LFxuICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6IGV4cHJlc3Npb24sXG4gICAgICAgICAgICAgICAgICAgICAgICBwb3NpdGlvbjoge1xuICAgICAgICAgICAgICAgICAgICAgICAgICBmaWxlTmFtZTogZXhwcmVzc2lvblsnZmlsZU5hbWUnXSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgbGluZTogZXhwcmVzc2lvblsnbGluZSddLFxuICAgICAgICAgICAgICAgICAgICAgICAgICBjb2x1bW46IGV4cHJlc3Npb25bJ2NoYXJhY3RlciddXG4gICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgfSxcbiAgICAgICAgICAgICAgICAgICAgICBjb250ZXh0KTtcbiAgICAgICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgICAgc2VsZi5lcnJvcih7bWVzc2FnZSwgY29udGV4dDogZXhwcmVzc2lvbi5jb250ZXh0fSwgY29udGV4dCk7XG4gICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgIHJldHVybiBJR05PUkU7XG4gICAgICAgICAgICAgIGNhc2UgJ2lnbm9yZSc6XG4gICAgICAgICAgICAgICAgcmV0dXJuIGV4cHJlc3Npb247XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gbnVsbDtcbiAgICAgICAgICB9XG4gICAgICAgICAgcmV0dXJuIG1hcFN0cmluZ01hcChleHByZXNzaW9uLCAodmFsdWUsIG5hbWUpID0+IHtcbiAgICAgICAgICAgIGlmIChSRUZFUkVOQ0VfU0VULmhhcyhuYW1lKSkge1xuICAgICAgICAgICAgICBpZiAobmFtZSA9PT0gVVNFX1ZBTFVFICYmIFBST1ZJREUgaW4gZXhwcmVzc2lvbikge1xuICAgICAgICAgICAgICAgIC8vIElmIHRoaXMgaXMgYSBwcm92aWRlciBleHByZXNzaW9uLCBjaGVjayBmb3Igc3BlY2lhbCB0b2tlbnMgdGhhdCBuZWVkIHRoZSB2YWx1ZVxuICAgICAgICAgICAgICAgIC8vIGR1cmluZyBhbmFseXNpcy5cbiAgICAgICAgICAgICAgICBjb25zdCBwcm92aWRlID0gc2ltcGxpZnkoZXhwcmVzc2lvbi5wcm92aWRlKTtcbiAgICAgICAgICAgICAgICBpZiAocHJvdmlkZSA9PT0gc2VsZi5ST1VURVMgfHwgcHJvdmlkZSA9PSBzZWxmLkFOQUxZWkVfRk9SX0VOVFJZX0NPTVBPTkVOVFMpIHtcbiAgICAgICAgICAgICAgICAgIHJldHVybiBzaW1wbGlmeSh2YWx1ZSk7XG4gICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgIHJldHVybiBzaW1wbGlmeUxhemlseSh2YWx1ZSk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gc2ltcGxpZnkodmFsdWUpO1xuICAgICAgICAgIH0pO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiBJR05PUkU7XG4gICAgICB9XG5cbiAgICAgIHJldHVybiBzaW1wbGlmeSh2YWx1ZSk7XG4gICAgfVxuXG4gICAgbGV0IHJlc3VsdDogYW55O1xuICAgIHRyeSB7XG4gICAgICByZXN1bHQgPSBzaW1wbGlmeUluQ29udGV4dChjb250ZXh0LCB2YWx1ZSwgMCwgbGF6eSA/IDEgOiAwKTtcbiAgICB9IGNhdGNoIChlKSB7XG4gICAgICBpZiAodGhpcy5lcnJvclJlY29yZGVyKSB7XG4gICAgICAgIHRoaXMucmVwb3J0RXJyb3IoZSwgY29udGV4dCk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICB0aHJvdyBmb3JtYXRNZXRhZGF0YUVycm9yKGUsIGNvbnRleHQpO1xuICAgICAgfVxuICAgIH1cbiAgICBpZiAoc2hvdWxkSWdub3JlKHJlc3VsdCkpIHtcbiAgICAgIHJldHVybiB1bmRlZmluZWQ7XG4gICAgfVxuICAgIHJldHVybiByZXN1bHQ7XG4gIH1cblxuICBwcml2YXRlIGdldFR5cGVNZXRhZGF0YSh0eXBlOiBTdGF0aWNTeW1ib2wpOiB7W2tleTogc3RyaW5nXTogYW55fSB7XG4gICAgY29uc3QgcmVzb2x2ZWRTeW1ib2wgPSB0aGlzLnN5bWJvbFJlc29sdmVyLnJlc29sdmVTeW1ib2wodHlwZSk7XG4gICAgcmV0dXJuIHJlc29sdmVkU3ltYm9sICYmIHJlc29sdmVkU3ltYm9sLm1ldGFkYXRhID8gcmVzb2x2ZWRTeW1ib2wubWV0YWRhdGEgOlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHtfX3N5bWJvbGljOiAnY2xhc3MnfTtcbiAgfVxuXG4gIHByaXZhdGUgcmVwb3J0RXJyb3IoZXJyb3I6IEVycm9yLCBjb250ZXh0OiBTdGF0aWNTeW1ib2wsIHBhdGg/OiBzdHJpbmcpIHtcbiAgICBpZiAodGhpcy5lcnJvclJlY29yZGVyKSB7XG4gICAgICB0aGlzLmVycm9yUmVjb3JkZXIoXG4gICAgICAgICAgZm9ybWF0TWV0YWRhdGFFcnJvcihlcnJvciwgY29udGV4dCksIChjb250ZXh0ICYmIGNvbnRleHQuZmlsZVBhdGgpIHx8IHBhdGgpO1xuICAgIH0gZWxzZSB7XG4gICAgICB0aHJvdyBlcnJvcjtcbiAgICB9XG4gIH1cblxuICBwcml2YXRlIGVycm9yKFxuICAgICAge21lc3NhZ2UsIHN1bW1hcnksIGFkdmlzZSwgcG9zaXRpb24sIGNvbnRleHQsIHZhbHVlLCBzeW1ib2wsIGNoYWlufToge1xuICAgICAgICBtZXNzYWdlOiBzdHJpbmcsXG4gICAgICAgIHN1bW1hcnk/OiBzdHJpbmcsXG4gICAgICAgIGFkdmlzZT86IHN0cmluZyxcbiAgICAgICAgcG9zaXRpb24/OiBQb3NpdGlvbixcbiAgICAgICAgY29udGV4dD86IGFueSxcbiAgICAgICAgdmFsdWU/OiBhbnksXG4gICAgICAgIHN5bWJvbD86IFN0YXRpY1N5bWJvbCxcbiAgICAgICAgY2hhaW4/OiBNZXRhZGF0YU1lc3NhZ2VDaGFpblxuICAgICAgfSxcbiAgICAgIHJlcG9ydGluZ0NvbnRleHQ6IFN0YXRpY1N5bWJvbCkge1xuICAgIHRoaXMucmVwb3J0RXJyb3IoXG4gICAgICAgIG1ldGFkYXRhRXJyb3IobWVzc2FnZSwgc3VtbWFyeSwgYWR2aXNlLCBwb3NpdGlvbiwgc3ltYm9sLCBjb250ZXh0LCBjaGFpbiksXG4gICAgICAgIHJlcG9ydGluZ0NvbnRleHQpO1xuICB9XG59XG5cbmludGVyZmFjZSBQb3NpdGlvbiB7XG4gIGZpbGVOYW1lOiBzdHJpbmc7XG4gIGxpbmU6IG51bWJlcjtcbiAgY29sdW1uOiBudW1iZXI7XG59XG5cbmludGVyZmFjZSBNZXRhZGF0YU1lc3NhZ2VDaGFpbiB7XG4gIG1lc3NhZ2U6IHN0cmluZztcbiAgc3VtbWFyeT86IHN0cmluZztcbiAgcG9zaXRpb24/OiBQb3NpdGlvbjtcbiAgY29udGV4dD86IGFueTtcbiAgc3ltYm9sPzogU3RhdGljU3ltYm9sO1xuICBuZXh0PzogTWV0YWRhdGFNZXNzYWdlQ2hhaW47XG59XG5cbnR5cGUgTWV0YWRhdGFFcnJvciA9IEVycm9yJntcbiAgcG9zaXRpb24/OiBQb3NpdGlvbjtcbiAgYWR2aXNlPzogc3RyaW5nO1xuICBzdW1tYXJ5Pzogc3RyaW5nO1xuICBjb250ZXh0PzogYW55O1xuICBzeW1ib2w/OiBTdGF0aWNTeW1ib2w7XG4gIGNoYWluPzogTWV0YWRhdGFNZXNzYWdlQ2hhaW47XG59O1xuXG5jb25zdCBNRVRBREFUQV9FUlJPUiA9ICduZ01ldGFkYXRhRXJyb3InO1xuXG5mdW5jdGlvbiBtZXRhZGF0YUVycm9yKFxuICAgIG1lc3NhZ2U6IHN0cmluZywgc3VtbWFyeT86IHN0cmluZywgYWR2aXNlPzogc3RyaW5nLCBwb3NpdGlvbj86IFBvc2l0aW9uLCBzeW1ib2w/OiBTdGF0aWNTeW1ib2wsXG4gICAgY29udGV4dD86IGFueSwgY2hhaW4/OiBNZXRhZGF0YU1lc3NhZ2VDaGFpbik6IE1ldGFkYXRhRXJyb3Ige1xuICBjb25zdCBlcnJvciA9IHN5bnRheEVycm9yKG1lc3NhZ2UpIGFzIE1ldGFkYXRhRXJyb3I7XG4gIChlcnJvciBhcyBhbnkpW01FVEFEQVRBX0VSUk9SXSA9IHRydWU7XG4gIGlmIChhZHZpc2UpIGVycm9yLmFkdmlzZSA9IGFkdmlzZTtcbiAgaWYgKHBvc2l0aW9uKSBlcnJvci5wb3NpdGlvbiA9IHBvc2l0aW9uO1xuICBpZiAoc3VtbWFyeSkgZXJyb3Iuc3VtbWFyeSA9IHN1bW1hcnk7XG4gIGlmIChjb250ZXh0KSBlcnJvci5jb250ZXh0ID0gY29udGV4dDtcbiAgaWYgKGNoYWluKSBlcnJvci5jaGFpbiA9IGNoYWluO1xuICBpZiAoc3ltYm9sKSBlcnJvci5zeW1ib2wgPSBzeW1ib2w7XG4gIHJldHVybiBlcnJvcjtcbn1cblxuZnVuY3Rpb24gaXNNZXRhZGF0YUVycm9yKGVycm9yOiBFcnJvcik6IGVycm9yIGlzIE1ldGFkYXRhRXJyb3Ige1xuICByZXR1cm4gISEoZXJyb3IgYXMgYW55KVtNRVRBREFUQV9FUlJPUl07XG59XG5cbmNvbnN0IFJFRkVSRU5DRV9UT19OT05FWFBPUlRFRF9DTEFTUyA9ICdSZWZlcmVuY2UgdG8gbm9uLWV4cG9ydGVkIGNsYXNzJztcbmNvbnN0IFZBUklBQkxFX05PVF9JTklUSUFMSVpFRCA9ICdWYXJpYWJsZSBub3QgaW5pdGlhbGl6ZWQnO1xuY29uc3QgREVTVFJVQ1RVUkVfTk9UX1NVUFBPUlRFRCA9ICdEZXN0cnVjdHVyaW5nIG5vdCBzdXBwb3J0ZWQnO1xuY29uc3QgQ09VTERfTk9UX1JFU09MVkVfVFlQRSA9ICdDb3VsZCBub3QgcmVzb2x2ZSB0eXBlJztcbmNvbnN0IEZVTkNUSU9OX0NBTExfTk9UX1NVUFBPUlRFRCA9ICdGdW5jdGlvbiBjYWxsIG5vdCBzdXBwb3J0ZWQnO1xuY29uc3QgUkVGRVJFTkNFX1RPX0xPQ0FMX1NZTUJPTCA9ICdSZWZlcmVuY2UgdG8gYSBsb2NhbCBzeW1ib2wnO1xuY29uc3QgTEFNQkRBX05PVF9TVVBQT1JURUQgPSAnTGFtYmRhIG5vdCBzdXBwb3J0ZWQnO1xuXG5mdW5jdGlvbiBleHBhbmRlZE1lc3NhZ2UobWVzc2FnZTogc3RyaW5nLCBjb250ZXh0OiBhbnkpOiBzdHJpbmcge1xuICBzd2l0Y2ggKG1lc3NhZ2UpIHtcbiAgICBjYXNlIFJFRkVSRU5DRV9UT19OT05FWFBPUlRFRF9DTEFTUzpcbiAgICAgIGlmIChjb250ZXh0ICYmIGNvbnRleHQuY2xhc3NOYW1lKSB7XG4gICAgICAgIHJldHVybiBgUmVmZXJlbmNlcyB0byBhIG5vbi1leHBvcnRlZCBjbGFzcyBhcmUgbm90IHN1cHBvcnRlZCBpbiBkZWNvcmF0b3JzIGJ1dCAke1xuICAgICAgICAgICAgY29udGV4dC5jbGFzc05hbWV9IHdhcyByZWZlcmVuY2VkLmA7XG4gICAgICB9XG4gICAgICBicmVhaztcbiAgICBjYXNlIFZBUklBQkxFX05PVF9JTklUSUFMSVpFRDpcbiAgICAgIHJldHVybiAnT25seSBpbml0aWFsaXplZCB2YXJpYWJsZXMgYW5kIGNvbnN0YW50cyBjYW4gYmUgcmVmZXJlbmNlZCBpbiBkZWNvcmF0b3JzIGJlY2F1c2UgdGhlIHZhbHVlIG9mIHRoaXMgdmFyaWFibGUgaXMgbmVlZGVkIGJ5IHRoZSB0ZW1wbGF0ZSBjb21waWxlcic7XG4gICAgY2FzZSBERVNUUlVDVFVSRV9OT1RfU1VQUE9SVEVEOlxuICAgICAgcmV0dXJuICdSZWZlcmVuY2luZyBhbiBleHBvcnRlZCBkZXN0cnVjdHVyZWQgdmFyaWFibGUgb3IgY29uc3RhbnQgaXMgbm90IHN1cHBvcnRlZCBpbiBkZWNvcmF0b3JzIGFuZCB0aGlzIHZhbHVlIGlzIG5lZWRlZCBieSB0aGUgdGVtcGxhdGUgY29tcGlsZXInO1xuICAgIGNhc2UgQ09VTERfTk9UX1JFU09MVkVfVFlQRTpcbiAgICAgIGlmIChjb250ZXh0ICYmIGNvbnRleHQudHlwZU5hbWUpIHtcbiAgICAgICAgcmV0dXJuIGBDb3VsZCBub3QgcmVzb2x2ZSB0eXBlICR7Y29udGV4dC50eXBlTmFtZX1gO1xuICAgICAgfVxuICAgICAgYnJlYWs7XG4gICAgY2FzZSBGVU5DVElPTl9DQUxMX05PVF9TVVBQT1JURUQ6XG4gICAgICBpZiAoY29udGV4dCAmJiBjb250ZXh0Lm5hbWUpIHtcbiAgICAgICAgcmV0dXJuIGBGdW5jdGlvbiBjYWxscyBhcmUgbm90IHN1cHBvcnRlZCBpbiBkZWNvcmF0b3JzIGJ1dCAnJHtjb250ZXh0Lm5hbWV9JyB3YXMgY2FsbGVkYDtcbiAgICAgIH1cbiAgICAgIHJldHVybiAnRnVuY3Rpb24gY2FsbHMgYXJlIG5vdCBzdXBwb3J0ZWQgaW4gZGVjb3JhdG9ycyc7XG4gICAgY2FzZSBSRUZFUkVOQ0VfVE9fTE9DQUxfU1lNQk9MOlxuICAgICAgaWYgKGNvbnRleHQgJiYgY29udGV4dC5uYW1lKSB7XG4gICAgICAgIHJldHVybiBgUmVmZXJlbmNlIHRvIGEgbG9jYWwgKG5vbi1leHBvcnRlZCkgc3ltYm9scyBhcmUgbm90IHN1cHBvcnRlZCBpbiBkZWNvcmF0b3JzIGJ1dCAnJHtcbiAgICAgICAgICAgIGNvbnRleHQubmFtZX0nIHdhcyByZWZlcmVuY2VkYDtcbiAgICAgIH1cbiAgICAgIGJyZWFrO1xuICAgIGNhc2UgTEFNQkRBX05PVF9TVVBQT1JURUQ6XG4gICAgICByZXR1cm4gYEZ1bmN0aW9uIGV4cHJlc3Npb25zIGFyZSBub3Qgc3VwcG9ydGVkIGluIGRlY29yYXRvcnNgO1xuICB9XG4gIHJldHVybiBtZXNzYWdlO1xufVxuXG5mdW5jdGlvbiBtZXNzYWdlQWR2aXNlKG1lc3NhZ2U6IHN0cmluZywgY29udGV4dDogYW55KTogc3RyaW5nfHVuZGVmaW5lZCB7XG4gIHN3aXRjaCAobWVzc2FnZSkge1xuICAgIGNhc2UgUkVGRVJFTkNFX1RPX05PTkVYUE9SVEVEX0NMQVNTOlxuICAgICAgaWYgKGNvbnRleHQgJiYgY29udGV4dC5jbGFzc05hbWUpIHtcbiAgICAgICAgcmV0dXJuIGBDb25zaWRlciBleHBvcnRpbmcgJyR7Y29udGV4dC5jbGFzc05hbWV9J2A7XG4gICAgICB9XG4gICAgICBicmVhaztcbiAgICBjYXNlIERFU1RSVUNUVVJFX05PVF9TVVBQT1JURUQ6XG4gICAgICByZXR1cm4gJ0NvbnNpZGVyIHNpbXBsaWZ5aW5nIHRvIGF2b2lkIGRlc3RydWN0dXJpbmcnO1xuICAgIGNhc2UgUkVGRVJFTkNFX1RPX0xPQ0FMX1NZTUJPTDpcbiAgICAgIGlmIChjb250ZXh0ICYmIGNvbnRleHQubmFtZSkge1xuICAgICAgICByZXR1cm4gYENvbnNpZGVyIGV4cG9ydGluZyAnJHtjb250ZXh0Lm5hbWV9J2A7XG4gICAgICB9XG4gICAgICBicmVhaztcbiAgICBjYXNlIExBTUJEQV9OT1RfU1VQUE9SVEVEOlxuICAgICAgcmV0dXJuIGBDb25zaWRlciBjaGFuZ2luZyB0aGUgZnVuY3Rpb24gZXhwcmVzc2lvbiBpbnRvIGFuIGV4cG9ydGVkIGZ1bmN0aW9uYDtcbiAgfVxuICByZXR1cm4gdW5kZWZpbmVkO1xufVxuXG5mdW5jdGlvbiBlcnJvclN1bW1hcnkoZXJyb3I6IE1ldGFkYXRhRXJyb3IpOiBzdHJpbmcge1xuICBpZiAoZXJyb3Iuc3VtbWFyeSkge1xuICAgIHJldHVybiBlcnJvci5zdW1tYXJ5O1xuICB9XG4gIHN3aXRjaCAoZXJyb3IubWVzc2FnZSkge1xuICAgIGNhc2UgUkVGRVJFTkNFX1RPX05PTkVYUE9SVEVEX0NMQVNTOlxuICAgICAgaWYgKGVycm9yLmNvbnRleHQgJiYgZXJyb3IuY29udGV4dC5jbGFzc05hbWUpIHtcbiAgICAgICAgcmV0dXJuIGByZWZlcmVuY2VzIG5vbi1leHBvcnRlZCBjbGFzcyAke2Vycm9yLmNvbnRleHQuY2xhc3NOYW1lfWA7XG4gICAgICB9XG4gICAgICBicmVhaztcbiAgICBjYXNlIFZBUklBQkxFX05PVF9JTklUSUFMSVpFRDpcbiAgICAgIHJldHVybiAnaXMgbm90IGluaXRpYWxpemVkJztcbiAgICBjYXNlIERFU1RSVUNUVVJFX05PVF9TVVBQT1JURUQ6XG4gICAgICByZXR1cm4gJ2lzIGEgZGVzdHJ1Y3R1cmVkIHZhcmlhYmxlJztcbiAgICBjYXNlIENPVUxEX05PVF9SRVNPTFZFX1RZUEU6XG4gICAgICByZXR1cm4gJ2NvdWxkIG5vdCBiZSByZXNvbHZlZCc7XG4gICAgY2FzZSBGVU5DVElPTl9DQUxMX05PVF9TVVBQT1JURUQ6XG4gICAgICBpZiAoZXJyb3IuY29udGV4dCAmJiBlcnJvci5jb250ZXh0Lm5hbWUpIHtcbiAgICAgICAgcmV0dXJuIGBjYWxscyAnJHtlcnJvci5jb250ZXh0Lm5hbWV9J2A7XG4gICAgICB9XG4gICAgICByZXR1cm4gYGNhbGxzIGEgZnVuY3Rpb25gO1xuICAgIGNhc2UgUkVGRVJFTkNFX1RPX0xPQ0FMX1NZTUJPTDpcbiAgICAgIGlmIChlcnJvci5jb250ZXh0ICYmIGVycm9yLmNvbnRleHQubmFtZSkge1xuICAgICAgICByZXR1cm4gYHJlZmVyZW5jZXMgbG9jYWwgdmFyaWFibGUgJHtlcnJvci5jb250ZXh0Lm5hbWV9YDtcbiAgICAgIH1cbiAgICAgIHJldHVybiBgcmVmZXJlbmNlcyBhIGxvY2FsIHZhcmlhYmxlYDtcbiAgfVxuICByZXR1cm4gJ2NvbnRhaW5zIHRoZSBlcnJvcic7XG59XG5cbmZ1bmN0aW9uIG1hcFN0cmluZ01hcChpbnB1dDoge1trZXk6IHN0cmluZ106IGFueX0sIHRyYW5zZm9ybTogKHZhbHVlOiBhbnksIGtleTogc3RyaW5nKSA9PiBhbnkpOlxuICAgIHtba2V5OiBzdHJpbmddOiBhbnl9IHtcbiAgaWYgKCFpbnB1dCkgcmV0dXJuIHt9O1xuICBjb25zdCByZXN1bHQ6IHtba2V5OiBzdHJpbmddOiBhbnl9ID0ge307XG4gIE9iamVjdC5rZXlzKGlucHV0KS5mb3JFYWNoKChrZXkpID0+IHtcbiAgICBjb25zdCB2YWx1ZSA9IHRyYW5zZm9ybShpbnB1dFtrZXldLCBrZXkpO1xuICAgIGlmICghc2hvdWxkSWdub3JlKHZhbHVlKSkge1xuICAgICAgaWYgKEhJRERFTl9LRVkudGVzdChrZXkpKSB7XG4gICAgICAgIE9iamVjdC5kZWZpbmVQcm9wZXJ0eShyZXN1bHQsIGtleSwge2VudW1lcmFibGU6IGZhbHNlLCBjb25maWd1cmFibGU6IHRydWUsIHZhbHVlOiB2YWx1ZX0pO1xuICAgICAgfSBlbHNlIHtcbiAgICAgICAgcmVzdWx0W2tleV0gPSB2YWx1ZTtcbiAgICAgIH1cbiAgICB9XG4gIH0pO1xuICByZXR1cm4gcmVzdWx0O1xufVxuXG5mdW5jdGlvbiBpc1ByaW1pdGl2ZShvOiBhbnkpOiBib29sZWFuIHtcbiAgcmV0dXJuIG8gPT09IG51bGwgfHwgKHR5cGVvZiBvICE9PSAnZnVuY3Rpb24nICYmIHR5cGVvZiBvICE9PSAnb2JqZWN0Jyk7XG59XG5cbmludGVyZmFjZSBCaW5kaW5nU2NvcGVCdWlsZGVyIHtcbiAgZGVmaW5lKG5hbWU6IHN0cmluZywgdmFsdWU6IGFueSk6IEJpbmRpbmdTY29wZUJ1aWxkZXI7XG4gIGRvbmUoKTogQmluZGluZ1Njb3BlO1xufVxuXG5hYnN0cmFjdCBjbGFzcyBCaW5kaW5nU2NvcGUge1xuICBhYnN0cmFjdCByZXNvbHZlKG5hbWU6IHN0cmluZyk6IGFueTtcbiAgcHVibGljIHN0YXRpYyBtaXNzaW5nID0ge307XG4gIHB1YmxpYyBzdGF0aWMgZW1wdHk6IEJpbmRpbmdTY29wZSA9IHtyZXNvbHZlOiBuYW1lID0+IEJpbmRpbmdTY29wZS5taXNzaW5nfTtcblxuICBwdWJsaWMgc3RhdGljIGJ1aWxkKCk6IEJpbmRpbmdTY29wZUJ1aWxkZXIge1xuICAgIGNvbnN0IGN1cnJlbnQgPSBuZXcgTWFwPHN0cmluZywgYW55PigpO1xuICAgIHJldHVybiB7XG4gICAgICBkZWZpbmU6IGZ1bmN0aW9uKG5hbWUsIHZhbHVlKSB7XG4gICAgICAgIGN1cnJlbnQuc2V0KG5hbWUsIHZhbHVlKTtcbiAgICAgICAgcmV0dXJuIHRoaXM7XG4gICAgICB9LFxuICAgICAgZG9uZTogZnVuY3Rpb24oKSB7XG4gICAgICAgIHJldHVybiBjdXJyZW50LnNpemUgPiAwID8gbmV3IFBvcHVsYXRlZFNjb3BlKGN1cnJlbnQpIDogQmluZGluZ1Njb3BlLmVtcHR5O1xuICAgICAgfVxuICAgIH07XG4gIH1cbn1cblxuY2xhc3MgUG9wdWxhdGVkU2NvcGUgZXh0ZW5kcyBCaW5kaW5nU2NvcGUge1xuICBjb25zdHJ1Y3Rvcihwcml2YXRlIGJpbmRpbmdzOiBNYXA8c3RyaW5nLCBhbnk+KSB7XG4gICAgc3VwZXIoKTtcbiAgfVxuXG4gIHJlc29sdmUobmFtZTogc3RyaW5nKTogYW55IHtcbiAgICByZXR1cm4gdGhpcy5iaW5kaW5ncy5oYXMobmFtZSkgPyB0aGlzLmJpbmRpbmdzLmdldChuYW1lKSA6IEJpbmRpbmdTY29wZS5taXNzaW5nO1xuICB9XG59XG5cbmZ1bmN0aW9uIGZvcm1hdE1ldGFkYXRhTWVzc2FnZUNoYWluKFxuICAgIGNoYWluOiBNZXRhZGF0YU1lc3NhZ2VDaGFpbiwgYWR2aXNlOiBzdHJpbmd8dW5kZWZpbmVkKTogRm9ybWF0dGVkTWVzc2FnZUNoYWluIHtcbiAgY29uc3QgZXhwYW5kZWQgPSBleHBhbmRlZE1lc3NhZ2UoY2hhaW4ubWVzc2FnZSwgY2hhaW4uY29udGV4dCk7XG4gIGNvbnN0IG5lc3RpbmcgPSBjaGFpbi5zeW1ib2wgPyBgIGluICcke2NoYWluLnN5bWJvbC5uYW1lfSdgIDogJyc7XG4gIGNvbnN0IG1lc3NhZ2UgPSBgJHtleHBhbmRlZH0ke25lc3Rpbmd9YDtcbiAgY29uc3QgcG9zaXRpb24gPSBjaGFpbi5wb3NpdGlvbjtcbiAgY29uc3QgbmV4dDogRm9ybWF0dGVkTWVzc2FnZUNoYWlufHVuZGVmaW5lZCA9IGNoYWluLm5leHQgP1xuICAgICAgZm9ybWF0TWV0YWRhdGFNZXNzYWdlQ2hhaW4oY2hhaW4ubmV4dCwgYWR2aXNlKSA6XG4gICAgICBhZHZpc2UgPyB7bWVzc2FnZTogYWR2aXNlfSA6IHVuZGVmaW5lZDtcbiAgcmV0dXJuIHttZXNzYWdlLCBwb3NpdGlvbiwgbmV4dDogbmV4dCA/IFtuZXh0XSA6IHVuZGVmaW5lZH07XG59XG5cbmZ1bmN0aW9uIGZvcm1hdE1ldGFkYXRhRXJyb3IoZTogRXJyb3IsIGNvbnRleHQ6IFN0YXRpY1N5bWJvbCk6IEVycm9yIHtcbiAgaWYgKGlzTWV0YWRhdGFFcnJvcihlKSkge1xuICAgIC8vIFByb2R1Y2UgYSBmb3JtYXR0ZWQgdmVyc2lvbiBvZiB0aGUgYW5kIGxlYXZpbmcgZW5vdWdoIGluZm9ybWF0aW9uIGluIHRoZSBvcmlnaW5hbCBlcnJvclxuICAgIC8vIHRvIHJlY292ZXIgdGhlIGZvcm1hdHRpbmcgaW5mb3JtYXRpb24gdG8gZXZlbnR1YWxseSBwcm9kdWNlIGEgZGlhZ25vc3RpYyBlcnJvciBtZXNzYWdlLlxuICAgIGNvbnN0IHBvc2l0aW9uID0gZS5wb3NpdGlvbjtcbiAgICBjb25zdCBjaGFpbjogTWV0YWRhdGFNZXNzYWdlQ2hhaW4gPSB7XG4gICAgICBtZXNzYWdlOiBgRXJyb3IgZHVyaW5nIHRlbXBsYXRlIGNvbXBpbGUgb2YgJyR7Y29udGV4dC5uYW1lfSdgLFxuICAgICAgcG9zaXRpb246IHBvc2l0aW9uLFxuICAgICAgbmV4dDoge21lc3NhZ2U6IGUubWVzc2FnZSwgbmV4dDogZS5jaGFpbiwgY29udGV4dDogZS5jb250ZXh0LCBzeW1ib2w6IGUuc3ltYm9sfVxuICAgIH07XG4gICAgY29uc3QgYWR2aXNlID0gZS5hZHZpc2UgfHwgbWVzc2FnZUFkdmlzZShlLm1lc3NhZ2UsIGUuY29udGV4dCk7XG4gICAgcmV0dXJuIGZvcm1hdHRlZEVycm9yKGZvcm1hdE1ldGFkYXRhTWVzc2FnZUNoYWluKGNoYWluLCBhZHZpc2UpKTtcbiAgfVxuICByZXR1cm4gZTtcbn1cbiJdfQ==