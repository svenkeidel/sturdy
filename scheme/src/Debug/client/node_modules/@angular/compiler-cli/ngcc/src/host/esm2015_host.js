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
        define("@angular/compiler-cli/ngcc/src/host/esm2015_host", ["require", "exports", "tslib", "typescript", "@angular/compiler-cli/src/ngtsc/file_system", "@angular/compiler-cli/src/ngtsc/reflection", "@angular/compiler-cli/ngcc/src/analysis/util", "@angular/compiler-cli/ngcc/src/utils", "@angular/compiler-cli/ngcc/src/host/ngcc_host", "@angular/compiler-cli/ngcc/src/host/utils"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.getClassDeclarationFromInnerDeclaration = exports.getContainingStatement = exports.skipClassAliases = exports.getPropertyValueFromSymbol = exports.isMemberDecorateCall = exports.isClassDecorateCall = exports.isAssignment = exports.getIifeBody = exports.isAssignmentStatement = exports.Esm2015ReflectionHost = exports.CONSTRUCTOR_PARAMS = exports.CONSTRUCTOR = exports.PROP_DECORATORS = exports.DECORATORS = void 0;
    var tslib_1 = require("tslib");
    var ts = require("typescript");
    var file_system_1 = require("@angular/compiler-cli/src/ngtsc/file_system");
    var reflection_1 = require("@angular/compiler-cli/src/ngtsc/reflection");
    var util_1 = require("@angular/compiler-cli/ngcc/src/analysis/util");
    var utils_1 = require("@angular/compiler-cli/ngcc/src/utils");
    var ngcc_host_1 = require("@angular/compiler-cli/ngcc/src/host/ngcc_host");
    var utils_2 = require("@angular/compiler-cli/ngcc/src/host/utils");
    exports.DECORATORS = 'decorators';
    exports.PROP_DECORATORS = 'propDecorators';
    exports.CONSTRUCTOR = '__constructor';
    exports.CONSTRUCTOR_PARAMS = 'ctorParameters';
    /**
     * Esm2015 packages contain ECMAScript 2015 classes, etc.
     * Decorators are defined via static properties on the class. For example:
     *
     * ```
     * class SomeDirective {
     * }
     * SomeDirective.decorators = [
     *   { type: Directive, args: [{ selector: '[someDirective]' },] }
     * ];
     * SomeDirective.ctorParameters = () => [
     *   { type: ViewContainerRef, },
     *   { type: TemplateRef, },
     *   { type: undefined, decorators: [{ type: Inject, args: [INJECTED_TOKEN,] },] },
     * ];
     * SomeDirective.propDecorators = {
     *   "input1": [{ type: Input },],
     *   "input2": [{ type: Input },],
     * };
     * ```
     *
     * * Classes are decorated if they have a static property called `decorators`.
     * * Members are decorated if there is a matching key on a static property
     *   called `propDecorators`.
     * * Constructor parameters decorators are found on an object returned from
     *   a static method called `ctorParameters`.
     */
    var Esm2015ReflectionHost = /** @class */ (function (_super) {
        tslib_1.__extends(Esm2015ReflectionHost, _super);
        function Esm2015ReflectionHost(logger, isCore, src, dts) {
            if (dts === void 0) { dts = null; }
            var _this = _super.call(this, src.program.getTypeChecker()) || this;
            _this.logger = logger;
            _this.isCore = isCore;
            _this.src = src;
            _this.dts = dts;
            /**
             * A mapping from source declarations to typings declarations, which are both publicly exported.
             *
             * There should be one entry for every public export visible from the root file of the source
             * tree. Note that by definition the key and value declarations will not be in the same TS
             * program.
             */
            _this.publicDtsDeclarationMap = null;
            /**
             * A mapping from source declarations to typings declarations, which are not publicly exported.
             *
             * This mapping is a best guess between declarations that happen to be exported from their file by
             * the same name in both the source and the dts file. Note that by definition the key and value
             * declarations will not be in the same TS program.
             */
            _this.privateDtsDeclarationMap = null;
            /**
             * The set of source files that have already been preprocessed.
             */
            _this.preprocessedSourceFiles = new Set();
            /**
             * In ES2015, class declarations may have been down-leveled into variable declarations,
             * initialized using a class expression. In certain scenarios, an additional variable
             * is introduced that represents the class so that results in code such as:
             *
             * ```
             * let MyClass_1; let MyClass = MyClass_1 = class MyClass {};
             * ```
             *
             * This map tracks those aliased variables to their original identifier, i.e. the key
             * corresponds with the declaration of `MyClass_1` and its value becomes the `MyClass` identifier
             * of the variable declaration.
             *
             * This map is populated during the preprocessing of each source file.
             */
            _this.aliasedClassDeclarations = new Map();
            /**
             * Caches the information of the decorators on a class, as the work involved with extracting
             * decorators is complex and frequently used.
             *
             * This map is lazily populated during the first call to `acquireDecoratorInfo` for a given class.
             */
            _this.decoratorCache = new Map();
            return _this;
        }
        /**
         * Find a symbol for a node that we think is a class.
         * Classes should have a `name` identifier, because they may need to be referenced in other parts
         * of the program.
         *
         * In ES2015, a class may be declared using a variable declaration of the following structures:
         *
         * ```
         * var MyClass = MyClass_1 = class MyClass {};
         * ```
         *
         * or
         *
         * ```
         * var MyClass = MyClass_1 = (() => { class MyClass {} ... return MyClass; })()
         * ```
         *
         * Here, the intermediate `MyClass_1` assignment is optional. In the above example, the
         * `class MyClass {}` node is returned as declaration of `MyClass`.
         *
         * @param declaration the declaration node whose symbol we are finding.
         * @returns the symbol for the node or `undefined` if it is not a "class" or has no symbol.
         */
        Esm2015ReflectionHost.prototype.getClassSymbol = function (declaration) {
            var symbol = this.getClassSymbolFromOuterDeclaration(declaration);
            if (symbol !== undefined) {
                return symbol;
            }
            if (declaration.parent !== undefined && reflection_1.isNamedVariableDeclaration(declaration.parent)) {
                var variableValue = this.getVariableValue(declaration.parent);
                if (variableValue !== null) {
                    declaration = variableValue;
                }
            }
            return this.getClassSymbolFromInnerDeclaration(declaration);
        };
        /**
         * Examine a declaration (for example, of a class or function) and return metadata about any
         * decorators present on the declaration.
         *
         * @param declaration a TypeScript `ts.Declaration` node representing the class or function over
         * which to reflect. For example, if the intent is to reflect the decorators of a class and the
         * source is in ES6 format, this will be a `ts.ClassDeclaration` node. If the source is in ES5
         * format, this might be a `ts.VariableDeclaration` as classes in ES5 are represented as the
         * result of an IIFE execution.
         *
         * @returns an array of `Decorator` metadata if decorators are present on the declaration, or
         * `null` if either no decorators were present or if the declaration is not of a decoratable type.
         */
        Esm2015ReflectionHost.prototype.getDecoratorsOfDeclaration = function (declaration) {
            var symbol = this.getClassSymbol(declaration);
            if (!symbol) {
                return null;
            }
            return this.getDecoratorsOfSymbol(symbol);
        };
        /**
         * Examine a declaration which should be of a class, and return metadata about the members of the
         * class.
         *
         * @param clazz a `ClassDeclaration` representing the class over which to reflect.
         *
         * @returns an array of `ClassMember` metadata representing the members of the class.
         *
         * @throws if `declaration` does not resolve to a class declaration.
         */
        Esm2015ReflectionHost.prototype.getMembersOfClass = function (clazz) {
            var classSymbol = this.getClassSymbol(clazz);
            if (!classSymbol) {
                throw new Error("Attempted to get members of a non-class: \"" + clazz.getText() + "\"");
            }
            return this.getMembersOfSymbol(classSymbol);
        };
        /**
         * Reflect over the constructor of a class and return metadata about its parameters.
         *
         * This method only looks at the constructor of a class directly and not at any inherited
         * constructors.
         *
         * @param clazz a `ClassDeclaration` representing the class over which to reflect.
         *
         * @returns an array of `Parameter` metadata representing the parameters of the constructor, if
         * a constructor exists. If the constructor exists and has 0 parameters, this array will be empty.
         * If the class has no constructor, this method returns `null`.
         *
         * @throws if `declaration` does not resolve to a class declaration.
         */
        Esm2015ReflectionHost.prototype.getConstructorParameters = function (clazz) {
            var classSymbol = this.getClassSymbol(clazz);
            if (!classSymbol) {
                throw new Error("Attempted to get constructor parameters of a non-class: \"" + clazz.getText() + "\"");
            }
            var parameterNodes = this.getConstructorParameterDeclarations(classSymbol);
            if (parameterNodes) {
                return this.getConstructorParamInfo(classSymbol, parameterNodes);
            }
            return null;
        };
        Esm2015ReflectionHost.prototype.getBaseClassExpression = function (clazz) {
            // First try getting the base class from an ES2015 class declaration
            var superBaseClassIdentifier = _super.prototype.getBaseClassExpression.call(this, clazz);
            if (superBaseClassIdentifier) {
                return superBaseClassIdentifier;
            }
            // That didn't work so now try getting it from the "inner" declaration.
            var classSymbol = this.getClassSymbol(clazz);
            if (classSymbol === undefined ||
                !isNamedDeclaration(classSymbol.implementation.valueDeclaration)) {
                return null;
            }
            return _super.prototype.getBaseClassExpression.call(this, classSymbol.implementation.valueDeclaration);
        };
        Esm2015ReflectionHost.prototype.getInternalNameOfClass = function (clazz) {
            var classSymbol = this.getClassSymbol(clazz);
            if (classSymbol === undefined) {
                throw new Error("getInternalNameOfClass() called on a non-class: expected " + clazz.name.text + " to be a class declaration.");
            }
            return this.getNameFromClassSymbolDeclaration(classSymbol, classSymbol.implementation.valueDeclaration);
        };
        Esm2015ReflectionHost.prototype.getAdjacentNameOfClass = function (clazz) {
            var classSymbol = this.getClassSymbol(clazz);
            if (classSymbol === undefined) {
                throw new Error("getAdjacentNameOfClass() called on a non-class: expected " + clazz.name.text + " to be a class declaration.");
            }
            if (classSymbol.adjacent !== undefined) {
                return this.getNameFromClassSymbolDeclaration(classSymbol, classSymbol.adjacent.valueDeclaration);
            }
            else {
                return this.getNameFromClassSymbolDeclaration(classSymbol, classSymbol.implementation.valueDeclaration);
            }
        };
        Esm2015ReflectionHost.prototype.getNameFromClassSymbolDeclaration = function (classSymbol, declaration) {
            if (declaration === undefined) {
                throw new Error("getInternalNameOfClass() called on a class with an undefined internal declaration. External class name: " + classSymbol.name + "; internal class name: " + classSymbol.implementation.name + ".");
            }
            if (!isNamedDeclaration(declaration)) {
                throw new Error("getInternalNameOfClass() called on a class with an anonymous inner declaration: expected a name on:\n" + declaration.getText());
            }
            return declaration.name;
        };
        /**
         * Check whether the given node actually represents a class.
         */
        Esm2015ReflectionHost.prototype.isClass = function (node) {
            return _super.prototype.isClass.call(this, node) || this.getClassSymbol(node) !== undefined;
        };
        /**
         * Trace an identifier to its declaration, if possible.
         *
         * This method attempts to resolve the declaration of the given identifier, tracing back through
         * imports and re-exports until the original declaration statement is found. A `Declaration`
         * object is returned if the original declaration is found, or `null` is returned otherwise.
         *
         * In ES2015, we need to account for identifiers that refer to aliased class declarations such as
         * `MyClass_1`. Since such declarations are only available within the module itself, we need to
         * find the original class declaration, e.g. `MyClass`, that is associated with the aliased one.
         *
         * @param id a TypeScript `ts.Identifier` to trace back to a declaration.
         *
         * @returns metadata about the `Declaration` if the original declaration is found, or `null`
         * otherwise.
         */
        Esm2015ReflectionHost.prototype.getDeclarationOfIdentifier = function (id) {
            var superDeclaration = _super.prototype.getDeclarationOfIdentifier.call(this, id);
            // If no declaration was found or it's an inline declaration, return as is.
            if (superDeclaration === null || superDeclaration.node === null) {
                return superDeclaration;
            }
            // If the declaration already has traits assigned to it, return as is.
            if (superDeclaration.known !== null || superDeclaration.identity !== null) {
                return superDeclaration;
            }
            var declarationNode = superDeclaration.node;
            if (reflection_1.isNamedVariableDeclaration(superDeclaration.node) && !isTopLevel(superDeclaration.node)) {
                var variableValue = this.getVariableValue(superDeclaration.node);
                if (variableValue !== null && ts.isClassExpression(variableValue)) {
                    declarationNode = getContainingStatement(variableValue);
                }
            }
            var outerClassNode = getClassDeclarationFromInnerDeclaration(declarationNode);
            var declaration = outerClassNode !== null ?
                this.getDeclarationOfIdentifier(outerClassNode.name) :
                superDeclaration;
            if (declaration === null || declaration.node === null || declaration.known !== null) {
                return declaration;
            }
            // The identifier may have been of an additional class assignment such as `MyClass_1` that was
            // present as alias for `MyClass`. If so, resolve such aliases to their original declaration.
            var aliasedIdentifier = this.resolveAliasedClassIdentifier(declaration.node);
            if (aliasedIdentifier !== null) {
                return this.getDeclarationOfIdentifier(aliasedIdentifier);
            }
            // Variable declarations may represent an enum declaration, so attempt to resolve its members.
            if (ts.isVariableDeclaration(declaration.node)) {
                var enumMembers = this.resolveEnumMembers(declaration.node);
                if (enumMembers !== null) {
                    declaration.identity = { kind: 0 /* DownleveledEnum */, enumMembers: enumMembers };
                }
            }
            return declaration;
        };
        /**
         * Gets all decorators of the given class symbol. Any decorator that have been synthetically
         * injected by a migration will not be present in the returned collection.
         */
        Esm2015ReflectionHost.prototype.getDecoratorsOfSymbol = function (symbol) {
            var classDecorators = this.acquireDecoratorInfo(symbol).classDecorators;
            if (classDecorators === null) {
                return null;
            }
            // Return a clone of the array to prevent consumers from mutating the cache.
            return Array.from(classDecorators);
        };
        /**
         * Search the given module for variable declarations in which the initializer
         * is an identifier marked with the `PRE_R3_MARKER`.
         * @param module the module in which to search for switchable declarations.
         * @returns an array of variable declarations that match.
         */
        Esm2015ReflectionHost.prototype.getSwitchableDeclarations = function (module) {
            // Don't bother to walk the AST if the marker is not found in the text
            return module.getText().indexOf(ngcc_host_1.PRE_R3_MARKER) >= 0 ?
                utils_1.findAll(module, ngcc_host_1.isSwitchableVariableDeclaration) :
                [];
        };
        Esm2015ReflectionHost.prototype.getVariableValue = function (declaration) {
            var value = _super.prototype.getVariableValue.call(this, declaration);
            if (value) {
                return value;
            }
            // We have a variable declaration that has no initializer. For example:
            //
            // ```
            // var HttpClientXsrfModule_1;
            // ```
            //
            // So look for the special scenario where the variable is being assigned in
            // a nearby statement to the return value of a call to `__decorate`.
            // Then find the 2nd argument of that call, the "target", which will be the
            // actual class identifier. For example:
            //
            // ```
            // HttpClientXsrfModule = HttpClientXsrfModule_1 = tslib_1.__decorate([
            //   NgModule({
            //     providers: [],
            //   })
            // ], HttpClientXsrfModule);
            // ```
            //
            // And finally, find the declaration of the identifier in that argument.
            // Note also that the assignment can occur within another assignment.
            //
            var block = declaration.parent.parent.parent;
            var symbol = this.checker.getSymbolAtLocation(declaration.name);
            if (symbol && (ts.isBlock(block) || ts.isSourceFile(block))) {
                var decorateCall = this.findDecoratedVariableValue(block, symbol);
                var target = decorateCall && decorateCall.arguments[1];
                if (target && ts.isIdentifier(target)) {
                    var targetSymbol = this.checker.getSymbolAtLocation(target);
                    var targetDeclaration = targetSymbol && targetSymbol.valueDeclaration;
                    if (targetDeclaration) {
                        if (ts.isClassDeclaration(targetDeclaration) ||
                            ts.isFunctionDeclaration(targetDeclaration)) {
                            // The target is just a function or class declaration
                            // so return its identifier as the variable value.
                            return targetDeclaration.name || null;
                        }
                        else if (ts.isVariableDeclaration(targetDeclaration)) {
                            // The target is a variable declaration, so find the far right expression,
                            // in the case of multiple assignments (e.g. `var1 = var2 = value`).
                            var targetValue = targetDeclaration.initializer;
                            while (targetValue && isAssignment(targetValue)) {
                                targetValue = targetValue.right;
                            }
                            if (targetValue) {
                                return targetValue;
                            }
                        }
                    }
                }
            }
            return null;
        };
        /**
         * Find all top-level class symbols in the given file.
         * @param sourceFile The source file to search for classes.
         * @returns An array of class symbols.
         */
        Esm2015ReflectionHost.prototype.findClassSymbols = function (sourceFile) {
            var _this = this;
            var classes = [];
            this.getModuleStatements(sourceFile).forEach(function (statement) {
                if (ts.isVariableStatement(statement)) {
                    statement.declarationList.declarations.forEach(function (declaration) {
                        var classSymbol = _this.getClassSymbol(declaration);
                        if (classSymbol) {
                            classes.push(classSymbol);
                        }
                    });
                }
                else if (ts.isClassDeclaration(statement)) {
                    var classSymbol = _this.getClassSymbol(statement);
                    if (classSymbol) {
                        classes.push(classSymbol);
                    }
                }
            });
            return classes;
        };
        /**
         * Get the number of generic type parameters of a given class.
         *
         * @param clazz a `ClassDeclaration` representing the class over which to reflect.
         *
         * @returns the number of type parameters of the class, if known, or `null` if the declaration
         * is not a class or has an unknown number of type parameters.
         */
        Esm2015ReflectionHost.prototype.getGenericArityOfClass = function (clazz) {
            var dtsDeclaration = this.getDtsDeclaration(clazz);
            if (dtsDeclaration && ts.isClassDeclaration(dtsDeclaration)) {
                return dtsDeclaration.typeParameters ? dtsDeclaration.typeParameters.length : 0;
            }
            return null;
        };
        /**
         * Take an exported declaration of a class (maybe down-leveled to a variable) and look up the
         * declaration of its type in a separate .d.ts tree.
         *
         * This function is allowed to return `null` if the current compilation unit does not have a
         * separate .d.ts tree. When compiling TypeScript code this is always the case, since .d.ts files
         * are produced only during the emit of such a compilation. When compiling .js code, however,
         * there is frequently a parallel .d.ts tree which this method exposes.
         *
         * Note that the `ts.ClassDeclaration` returned from this function may not be from the same
         * `ts.Program` as the input declaration.
         */
        Esm2015ReflectionHost.prototype.getDtsDeclaration = function (declaration) {
            if (this.dts === null) {
                return null;
            }
            if (!isNamedDeclaration(declaration)) {
                throw new Error("Cannot get the dts file for a declaration that has no name: " + declaration.getText() + " in " + declaration.getSourceFile().fileName);
            }
            // Try to retrieve the dts declaration from the public map
            if (this.publicDtsDeclarationMap === null) {
                this.publicDtsDeclarationMap = this.computePublicDtsDeclarationMap(this.src, this.dts);
            }
            if (this.publicDtsDeclarationMap.has(declaration)) {
                return this.publicDtsDeclarationMap.get(declaration);
            }
            // No public export, try the private map
            if (this.privateDtsDeclarationMap === null) {
                this.privateDtsDeclarationMap = this.computePrivateDtsDeclarationMap(this.src, this.dts);
            }
            if (this.privateDtsDeclarationMap.has(declaration)) {
                return this.privateDtsDeclarationMap.get(declaration);
            }
            // No declaration found at all
            return null;
        };
        Esm2015ReflectionHost.prototype.getEndOfClass = function (classSymbol) {
            var implementation = classSymbol.implementation;
            var last = implementation.valueDeclaration;
            var implementationStatement = getContainingStatement(last);
            if (implementationStatement === null)
                return last;
            var container = implementationStatement.parent;
            if (ts.isBlock(container)) {
                // Assume that the implementation is inside an IIFE
                var returnStatementIndex = container.statements.findIndex(ts.isReturnStatement);
                if (returnStatementIndex === -1) {
                    throw new Error("Compiled class wrapper IIFE does not have a return statement: " + classSymbol.name + " in " + classSymbol.declaration.valueDeclaration.getSourceFile().fileName);
                }
                // Return the statement before the IIFE return statement
                last = container.statements[returnStatementIndex - 1];
            }
            else if (ts.isSourceFile(container)) {
                // If there are static members on this class then find the last one
                if (implementation.exports !== undefined) {
                    implementation.exports.forEach(function (exportSymbol) {
                        if (exportSymbol.valueDeclaration === undefined) {
                            return;
                        }
                        var exportStatement = getContainingStatement(exportSymbol.valueDeclaration);
                        if (exportStatement !== null && last.getEnd() < exportStatement.getEnd()) {
                            last = exportStatement;
                        }
                    });
                }
                // If there are helper calls for this class then find the last one
                var helpers = this.getHelperCallsForClass(classSymbol, ['__decorate', '__extends', '__param', '__metadata']);
                helpers.forEach(function (helper) {
                    var helperStatement = getContainingStatement(helper);
                    if (helperStatement !== null && last.getEnd() < helperStatement.getEnd()) {
                        last = helperStatement;
                    }
                });
            }
            return last;
        };
        /**
         * Check whether a `Declaration` corresponds with a known declaration, such as `Object`, and set
         * its `known` property to the appropriate `KnownDeclaration`.
         *
         * @param decl The `Declaration` to check.
         * @return The passed in `Declaration` (potentially enhanced with a `KnownDeclaration`).
         */
        Esm2015ReflectionHost.prototype.detectKnownDeclaration = function (decl) {
            if (decl.known === null && this.isJavaScriptObjectDeclaration(decl)) {
                // If the identifier resolves to the global JavaScript `Object`, update the declaration to
                // denote it as the known `JsGlobalObject` declaration.
                decl.known = reflection_1.KnownDeclaration.JsGlobalObject;
            }
            return decl;
        };
        ///////////// Protected Helpers /////////////
        /**
         * A class may be declared as a top level class declaration:
         *
         * ```
         * class OuterClass { ... }
         * ```
         *
         * or in a variable declaration to a class expression:
         *
         * ```
         * var OuterClass = ClassAlias = class InnerClass {};
         * ```
         *
         * or in a variable declaration to an IIFE containing a class declaration
         *
         * ```
         * var OuterClass = ClassAlias = (() => {
         *   class InnerClass {}
         *   ...
         *   return InnerClass;
         * })()
         * ```
         *
         * or in a variable declaration to an IIFE containing a function declaration
         *
         * ```
         * var OuterClass = ClassAlias = (() => {
         *   function InnerClass() {}
         *   ...
         *   return InnerClass;
         * })()
         * ```
         *
         * This method returns an `NgccClassSymbol` when provided with one of these cases.
         *
         * @param declaration the declaration whose symbol we are finding.
         * @returns the symbol for the class or `undefined` if `declaration` does not represent an outer
         *     declaration of a class.
         */
        Esm2015ReflectionHost.prototype.getClassSymbolFromOuterDeclaration = function (declaration) {
            // Return a class symbol without an inner declaration if it is a regular "top level" class
            if (reflection_1.isNamedClassDeclaration(declaration) && isTopLevel(declaration)) {
                return this.createClassSymbol(declaration, null);
            }
            // Otherwise, an outer class declaration must be an initialized variable declaration:
            if (!isInitializedVariableClassDeclaration(declaration)) {
                return undefined;
            }
            var innerDeclaration = getInnerClassDeclaration(skipClassAliases(declaration));
            if (innerDeclaration !== null) {
                return this.createClassSymbol(declaration, innerDeclaration);
            }
            return undefined;
        };
        /**
         * In ES2015, a class may be declared using a variable declaration of the following structures:
         *
         * ```
         * let MyClass = MyClass_1 = class MyClass {};
         * ```
         *
         * or
         *
         * ```
         * let MyClass = MyClass_1 = (() => { class MyClass {} ... return MyClass; })()
         * ```
         *
         * or
         *
         * ```
         * let MyClass = MyClass_1 = (() => { let MyClass = class MyClass {}; ... return MyClass; })()
         * ```
         *
         * This method extracts the `NgccClassSymbol` for `MyClass` when provided with the
         * `class MyClass {}` declaration node. When the `var MyClass` node or any other node is given,
         * this method will return undefined instead.
         *
         * @param declaration the declaration whose symbol we are finding.
         * @returns the symbol for the node or `undefined` if it does not represent an inner declaration
         * of a class.
         */
        Esm2015ReflectionHost.prototype.getClassSymbolFromInnerDeclaration = function (declaration) {
            var outerDeclaration = undefined;
            if (ts.isClassExpression(declaration) && utils_1.hasNameIdentifier(declaration)) {
                // Handle `let MyClass = MyClass_1 = class MyClass {};`
                outerDeclaration = getFarLeftHandSideOfAssignment(declaration);
                // Handle this being in an IIFE
                if (outerDeclaration !== undefined && !isTopLevel(outerDeclaration)) {
                    outerDeclaration = getContainingVariableDeclaration(outerDeclaration);
                }
            }
            else if (reflection_1.isNamedClassDeclaration(declaration)) {
                // Handle `class MyClass {}` statement
                if (isTopLevel(declaration)) {
                    // At the top level
                    outerDeclaration = declaration;
                }
                else {
                    // Or inside an IIFE
                    outerDeclaration = getContainingVariableDeclaration(declaration);
                }
            }
            if (outerDeclaration === undefined || !utils_1.hasNameIdentifier(outerDeclaration)) {
                return undefined;
            }
            return this.createClassSymbol(outerDeclaration, declaration);
        };
        /**
         * Creates an `NgccClassSymbol` from an outer and inner declaration. If a class only has an outer
         * declaration, the "implementation" symbol of the created `NgccClassSymbol` will be set equal to
         * the "declaration" symbol.
         *
         * @param outerDeclaration The outer declaration node of the class.
         * @param innerDeclaration The inner declaration node of the class, or undefined if no inner
         * declaration is present.
         * @returns the `NgccClassSymbol` representing the class, or undefined if a `ts.Symbol` for any of
         * the declarations could not be resolved.
         */
        Esm2015ReflectionHost.prototype.createClassSymbol = function (outerDeclaration, innerDeclaration) {
            var declarationSymbol = this.checker.getSymbolAtLocation(outerDeclaration.name);
            if (declarationSymbol === undefined) {
                return undefined;
            }
            var implementationSymbol = declarationSymbol;
            if (innerDeclaration !== null && isNamedDeclaration(innerDeclaration)) {
                implementationSymbol = this.checker.getSymbolAtLocation(innerDeclaration.name);
            }
            if (implementationSymbol === undefined) {
                return undefined;
            }
            var classSymbol = {
                name: declarationSymbol.name,
                declaration: declarationSymbol,
                implementation: implementationSymbol,
            };
            var adjacent = this.getAdjacentSymbol(declarationSymbol, implementationSymbol);
            if (adjacent !== null) {
                classSymbol.adjacent = adjacent;
            }
            return classSymbol;
        };
        Esm2015ReflectionHost.prototype.getAdjacentSymbol = function (declarationSymbol, implementationSymbol) {
            if (declarationSymbol === implementationSymbol) {
                return undefined;
            }
            var innerDeclaration = implementationSymbol.valueDeclaration;
            if (!ts.isClassExpression(innerDeclaration) && !ts.isFunctionExpression(innerDeclaration)) {
                return undefined;
            }
            // Deal with the inner class looking like this inside an IIFE:
            // `let MyClass = class MyClass {};` or `var MyClass = function MyClass() {};`
            var adjacentDeclaration = getFarLeftHandSideOfAssignment(innerDeclaration);
            if (adjacentDeclaration === undefined || !reflection_1.isNamedVariableDeclaration(adjacentDeclaration)) {
                return undefined;
            }
            var adjacentSymbol = this.checker.getSymbolAtLocation(adjacentDeclaration.name);
            if (adjacentSymbol === declarationSymbol || adjacentSymbol === implementationSymbol) {
                return undefined;
            }
            return adjacentSymbol;
        };
        /**
         * Resolve a `ts.Symbol` to its declaration and detect whether it corresponds with a known
         * declaration.
         */
        Esm2015ReflectionHost.prototype.getDeclarationOfSymbol = function (symbol, originalId) {
            var declaration = _super.prototype.getDeclarationOfSymbol.call(this, symbol, originalId);
            if (declaration === null) {
                return null;
            }
            return this.detectKnownDeclaration(declaration);
        };
        /**
         * Finds the identifier of the actual class declaration for a potentially aliased declaration of a
         * class.
         *
         * If the given declaration is for an alias of a class, this function will determine an identifier
         * to the original declaration that represents this class.
         *
         * @param declaration The declaration to resolve.
         * @returns The original identifier that the given class declaration resolves to, or `undefined`
         * if the declaration does not represent an aliased class.
         */
        Esm2015ReflectionHost.prototype.resolveAliasedClassIdentifier = function (declaration) {
            this.ensurePreprocessed(declaration.getSourceFile());
            return this.aliasedClassDeclarations.has(declaration) ?
                this.aliasedClassDeclarations.get(declaration) :
                null;
        };
        /**
         * Ensures that the source file that `node` is part of has been preprocessed.
         *
         * During preprocessing, all statements in the source file will be visited such that certain
         * processing steps can be done up-front and cached for subsequent usages.
         *
         * @param sourceFile The source file that needs to have gone through preprocessing.
         */
        Esm2015ReflectionHost.prototype.ensurePreprocessed = function (sourceFile) {
            var e_1, _a;
            if (!this.preprocessedSourceFiles.has(sourceFile)) {
                this.preprocessedSourceFiles.add(sourceFile);
                try {
                    for (var _b = tslib_1.__values(this.getModuleStatements(sourceFile)), _c = _b.next(); !_c.done; _c = _b.next()) {
                        var statement = _c.value;
                        this.preprocessStatement(statement);
                    }
                }
                catch (e_1_1) { e_1 = { error: e_1_1 }; }
                finally {
                    try {
                        if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                    }
                    finally { if (e_1) throw e_1.error; }
                }
            }
        };
        /**
         * Analyzes the given statement to see if it corresponds with a variable declaration like
         * `let MyClass = MyClass_1 = class MyClass {};`. If so, the declaration of `MyClass_1`
         * is associated with the `MyClass` identifier.
         *
         * @param statement The statement that needs to be preprocessed.
         */
        Esm2015ReflectionHost.prototype.preprocessStatement = function (statement) {
            if (!ts.isVariableStatement(statement)) {
                return;
            }
            var declarations = statement.declarationList.declarations;
            if (declarations.length !== 1) {
                return;
            }
            var declaration = declarations[0];
            var initializer = declaration.initializer;
            if (!ts.isIdentifier(declaration.name) || !initializer || !isAssignment(initializer) ||
                !ts.isIdentifier(initializer.left) || !this.isClass(declaration)) {
                return;
            }
            var aliasedIdentifier = initializer.left;
            var aliasedDeclaration = this.getDeclarationOfIdentifier(aliasedIdentifier);
            if (aliasedDeclaration === null || aliasedDeclaration.node === null) {
                throw new Error("Unable to locate declaration of " + aliasedIdentifier.text + " in \"" + statement.getText() + "\"");
            }
            this.aliasedClassDeclarations.set(aliasedDeclaration.node, declaration.name);
        };
        /**
         * Get the top level statements for a module.
         *
         * In ES5 and ES2015 this is just the top level statements of the file.
         * @param sourceFile The module whose statements we want.
         * @returns An array of top level statements for the given module.
         */
        Esm2015ReflectionHost.prototype.getModuleStatements = function (sourceFile) {
            return Array.from(sourceFile.statements);
        };
        /**
         * Walk the AST looking for an assignment to the specified symbol.
         * @param node The current node we are searching.
         * @returns an expression that represents the value of the variable, or undefined if none can be
         * found.
         */
        Esm2015ReflectionHost.prototype.findDecoratedVariableValue = function (node, symbol) {
            var _this = this;
            if (!node) {
                return null;
            }
            if (ts.isBinaryExpression(node) && node.operatorToken.kind === ts.SyntaxKind.EqualsToken) {
                var left = node.left;
                var right = node.right;
                if (ts.isIdentifier(left) && this.checker.getSymbolAtLocation(left) === symbol) {
                    return (ts.isCallExpression(right) && getCalleeName(right) === '__decorate') ? right : null;
                }
                return this.findDecoratedVariableValue(right, symbol);
            }
            return node.forEachChild(function (node) { return _this.findDecoratedVariableValue(node, symbol); }) || null;
        };
        /**
         * Try to retrieve the symbol of a static property on a class.
         *
         * In some cases, a static property can either be set on the inner (implementation or adjacent)
         * declaration inside the class' IIFE, or it can be set on the outer variable declaration.
         * Therefore, the host checks all places, first looking up the property on the inner symbols, and
         * if the property is not found it will fall back to looking up the property on the outer symbol.
         *
         * @param symbol the class whose property we are interested in.
         * @param propertyName the name of static property.
         * @returns the symbol if it is found or `undefined` if not.
         */
        Esm2015ReflectionHost.prototype.getStaticProperty = function (symbol, propertyName) {
            var _a, _b, _c, _d;
            return ((_a = symbol.implementation.exports) === null || _a === void 0 ? void 0 : _a.get(propertyName)) || ((_c = (_b = symbol.adjacent) === null || _b === void 0 ? void 0 : _b.exports) === null || _c === void 0 ? void 0 : _c.get(propertyName)) || ((_d = symbol.declaration.exports) === null || _d === void 0 ? void 0 : _d.get(propertyName));
        };
        /**
         * This is the main entry-point for obtaining information on the decorators of a given class. This
         * information is computed either from static properties if present, or using `tslib.__decorate`
         * helper calls otherwise. The computed result is cached per class.
         *
         * @param classSymbol the class for which decorators should be acquired.
         * @returns all information of the decorators on the class.
         */
        Esm2015ReflectionHost.prototype.acquireDecoratorInfo = function (classSymbol) {
            var decl = classSymbol.declaration.valueDeclaration;
            if (this.decoratorCache.has(decl)) {
                return this.decoratorCache.get(decl);
            }
            // Extract decorators from static properties and `__decorate` helper calls, then merge them
            // together where the information from the static properties is preferred.
            var staticProps = this.computeDecoratorInfoFromStaticProperties(classSymbol);
            var helperCalls = this.computeDecoratorInfoFromHelperCalls(classSymbol);
            var decoratorInfo = {
                classDecorators: staticProps.classDecorators || helperCalls.classDecorators,
                memberDecorators: staticProps.memberDecorators || helperCalls.memberDecorators,
                constructorParamInfo: staticProps.constructorParamInfo || helperCalls.constructorParamInfo,
            };
            this.decoratorCache.set(decl, decoratorInfo);
            return decoratorInfo;
        };
        /**
         * Attempts to compute decorator information from static properties "decorators", "propDecorators"
         * and "ctorParameters" on the class. If neither of these static properties is present the
         * library is likely not compiled using tsickle for usage with Closure compiler, in which case
         * `null` is returned.
         *
         * @param classSymbol The class symbol to compute the decorators information for.
         * @returns All information on the decorators as extracted from static properties, or `null` if
         * none of the static properties exist.
         */
        Esm2015ReflectionHost.prototype.computeDecoratorInfoFromStaticProperties = function (classSymbol) {
            var classDecorators = null;
            var memberDecorators = null;
            var constructorParamInfo = null;
            var decoratorsProperty = this.getStaticProperty(classSymbol, exports.DECORATORS);
            if (decoratorsProperty !== undefined) {
                classDecorators = this.getClassDecoratorsFromStaticProperty(decoratorsProperty);
            }
            var propDecoratorsProperty = this.getStaticProperty(classSymbol, exports.PROP_DECORATORS);
            if (propDecoratorsProperty !== undefined) {
                memberDecorators = this.getMemberDecoratorsFromStaticProperty(propDecoratorsProperty);
            }
            var constructorParamsProperty = this.getStaticProperty(classSymbol, exports.CONSTRUCTOR_PARAMS);
            if (constructorParamsProperty !== undefined) {
                constructorParamInfo = this.getParamInfoFromStaticProperty(constructorParamsProperty);
            }
            return { classDecorators: classDecorators, memberDecorators: memberDecorators, constructorParamInfo: constructorParamInfo };
        };
        /**
         * Get all class decorators for the given class, where the decorators are declared
         * via a static property. For example:
         *
         * ```
         * class SomeDirective {}
         * SomeDirective.decorators = [
         *   { type: Directive, args: [{ selector: '[someDirective]' },] }
         * ];
         * ```
         *
         * @param decoratorsSymbol the property containing the decorators we want to get.
         * @returns an array of decorators or null if none where found.
         */
        Esm2015ReflectionHost.prototype.getClassDecoratorsFromStaticProperty = function (decoratorsSymbol) {
            var _this = this;
            var decoratorsIdentifier = decoratorsSymbol.valueDeclaration;
            if (decoratorsIdentifier && decoratorsIdentifier.parent) {
                if (ts.isBinaryExpression(decoratorsIdentifier.parent) &&
                    decoratorsIdentifier.parent.operatorToken.kind === ts.SyntaxKind.EqualsToken) {
                    // AST of the array of decorator values
                    var decoratorsArray = decoratorsIdentifier.parent.right;
                    return this.reflectDecorators(decoratorsArray)
                        .filter(function (decorator) { return _this.isFromCore(decorator); });
                }
            }
            return null;
        };
        /**
         * Examine a symbol which should be of a class, and return metadata about its members.
         *
         * @param symbol the `ClassSymbol` representing the class over which to reflect.
         * @returns an array of `ClassMember` metadata representing the members of the class.
         */
        Esm2015ReflectionHost.prototype.getMembersOfSymbol = function (symbol) {
            var _this = this;
            var members = [];
            // The decorators map contains all the properties that are decorated
            var memberDecorators = this.acquireDecoratorInfo(symbol).memberDecorators;
            // Make a copy of the decorators as successfully reflected members delete themselves from the
            // map, so that any leftovers can be easily dealt with.
            var decoratorsMap = new Map(memberDecorators);
            // The member map contains all the method (instance and static); and any instance properties
            // that are initialized in the class.
            if (symbol.implementation.members) {
                symbol.implementation.members.forEach(function (value, key) {
                    var decorators = decoratorsMap.get(key);
                    var reflectedMembers = _this.reflectMembers(value, decorators);
                    if (reflectedMembers) {
                        decoratorsMap.delete(key);
                        members.push.apply(members, tslib_1.__spread(reflectedMembers));
                    }
                });
            }
            // The static property map contains all the static properties
            if (symbol.implementation.exports) {
                symbol.implementation.exports.forEach(function (value, key) {
                    var decorators = decoratorsMap.get(key);
                    var reflectedMembers = _this.reflectMembers(value, decorators, true);
                    if (reflectedMembers) {
                        decoratorsMap.delete(key);
                        members.push.apply(members, tslib_1.__spread(reflectedMembers));
                    }
                });
            }
            // If this class was declared as a VariableDeclaration then it may have static properties
            // attached to the variable rather than the class itself
            // For example:
            // ```
            // let MyClass = class MyClass {
            //   // no static properties here!
            // }
            // MyClass.staticProperty = ...;
            // ```
            if (ts.isVariableDeclaration(symbol.declaration.valueDeclaration)) {
                if (symbol.declaration.exports) {
                    symbol.declaration.exports.forEach(function (value, key) {
                        var decorators = decoratorsMap.get(key);
                        var reflectedMembers = _this.reflectMembers(value, decorators, true);
                        if (reflectedMembers) {
                            decoratorsMap.delete(key);
                            members.push.apply(members, tslib_1.__spread(reflectedMembers));
                        }
                    });
                }
            }
            // If this class was declared as a VariableDeclaration inside an IIFE, then it may have static
            // properties attached to the variable rather than the class itself.
            //
            // For example:
            // ```
            // let OuterClass = (() => {
            //   let AdjacentClass = class InternalClass {
            //     // no static properties here!
            //   }
            //   AdjacentClass.staticProperty = ...;
            // })();
            // ```
            if (symbol.adjacent !== undefined) {
                if (ts.isVariableDeclaration(symbol.adjacent.valueDeclaration)) {
                    if (symbol.adjacent.exports !== undefined) {
                        symbol.adjacent.exports.forEach(function (value, key) {
                            var decorators = decoratorsMap.get(key);
                            var reflectedMembers = _this.reflectMembers(value, decorators, true);
                            if (reflectedMembers) {
                                decoratorsMap.delete(key);
                                members.push.apply(members, tslib_1.__spread(reflectedMembers));
                            }
                        });
                    }
                }
            }
            // Deal with any decorated properties that were not initialized in the class
            decoratorsMap.forEach(function (value, key) {
                members.push({
                    implementation: null,
                    decorators: value,
                    isStatic: false,
                    kind: reflection_1.ClassMemberKind.Property,
                    name: key,
                    nameNode: null,
                    node: null,
                    type: null,
                    value: null
                });
            });
            return members;
        };
        /**
         * Member decorators may be declared as static properties of the class:
         *
         * ```
         * SomeDirective.propDecorators = {
         *   "ngForOf": [{ type: Input },],
         *   "ngForTrackBy": [{ type: Input },],
         *   "ngForTemplate": [{ type: Input },],
         * };
         * ```
         *
         * @param decoratorsProperty the class whose member decorators we are interested in.
         * @returns a map whose keys are the name of the members and whose values are collections of
         * decorators for the given member.
         */
        Esm2015ReflectionHost.prototype.getMemberDecoratorsFromStaticProperty = function (decoratorsProperty) {
            var _this = this;
            var memberDecorators = new Map();
            // Symbol of the identifier for `SomeDirective.propDecorators`.
            var propDecoratorsMap = getPropertyValueFromSymbol(decoratorsProperty);
            if (propDecoratorsMap && ts.isObjectLiteralExpression(propDecoratorsMap)) {
                var propertiesMap = reflection_1.reflectObjectLiteral(propDecoratorsMap);
                propertiesMap.forEach(function (value, name) {
                    var decorators = _this.reflectDecorators(value).filter(function (decorator) { return _this.isFromCore(decorator); });
                    if (decorators.length) {
                        memberDecorators.set(name, decorators);
                    }
                });
            }
            return memberDecorators;
        };
        /**
         * For a given class symbol, collects all decorator information from tslib helper methods, as
         * generated by TypeScript into emitted JavaScript files.
         *
         * Class decorators are extracted from calls to `tslib.__decorate` that look as follows:
         *
         * ```
         * let SomeDirective = class SomeDirective {}
         * SomeDirective = __decorate([
         *   Directive({ selector: '[someDirective]' }),
         * ], SomeDirective);
         * ```
         *
         * The extraction of member decorators is similar, with the distinction that its 2nd and 3rd
         * argument correspond with a "prototype" target and the name of the member to which the
         * decorators apply.
         *
         * ```
         * __decorate([
         *     Input(),
         *     __metadata("design:type", String)
         * ], SomeDirective.prototype, "input1", void 0);
         * ```
         *
         * @param classSymbol The class symbol for which decorators should be extracted.
         * @returns All information on the decorators of the class.
         */
        Esm2015ReflectionHost.prototype.computeDecoratorInfoFromHelperCalls = function (classSymbol) {
            var e_2, _a, e_3, _b, e_4, _c;
            var _this = this;
            var classDecorators = null;
            var memberDecorators = new Map();
            var constructorParamInfo = [];
            var getConstructorParamInfo = function (index) {
                var param = constructorParamInfo[index];
                if (param === undefined) {
                    param = constructorParamInfo[index] = { decorators: null, typeExpression: null };
                }
                return param;
            };
            // All relevant information can be extracted from calls to `__decorate`, obtain these first.
            // Note that although the helper calls are retrieved using the class symbol, the result may
            // contain helper calls corresponding with unrelated classes. Therefore, each helper call still
            // has to be checked to actually correspond with the class symbol.
            var helperCalls = this.getHelperCallsForClass(classSymbol, ['__decorate']);
            var outerDeclaration = classSymbol.declaration.valueDeclaration;
            var innerDeclaration = classSymbol.implementation.valueDeclaration;
            var adjacentDeclaration = this.getAdjacentNameOfClass((classSymbol.declaration.valueDeclaration)).parent;
            var matchesClass = function (identifier) {
                var decl = _this.getDeclarationOfIdentifier(identifier);
                return decl !== null &&
                    (decl.node === adjacentDeclaration || decl.node === outerDeclaration ||
                        decl.node === innerDeclaration);
            };
            try {
                for (var helperCalls_1 = tslib_1.__values(helperCalls), helperCalls_1_1 = helperCalls_1.next(); !helperCalls_1_1.done; helperCalls_1_1 = helperCalls_1.next()) {
                    var helperCall = helperCalls_1_1.value;
                    if (isClassDecorateCall(helperCall, matchesClass)) {
                        // This `__decorate` call is targeting the class itself.
                        var helperArgs = helperCall.arguments[0];
                        try {
                            for (var _d = (e_3 = void 0, tslib_1.__values(helperArgs.elements)), _e = _d.next(); !_e.done; _e = _d.next()) {
                                var element = _e.value;
                                var entry = this.reflectDecorateHelperEntry(element);
                                if (entry === null) {
                                    continue;
                                }
                                if (entry.type === 'decorator') {
                                    // The helper arg was reflected to represent an actual decorator
                                    if (this.isFromCore(entry.decorator)) {
                                        (classDecorators || (classDecorators = [])).push(entry.decorator);
                                    }
                                }
                                else if (entry.type === 'param:decorators') {
                                    // The helper arg represents a decorator for a parameter. Since it's applied to the
                                    // class, it corresponds with a constructor parameter of the class.
                                    var param = getConstructorParamInfo(entry.index);
                                    (param.decorators || (param.decorators = [])).push(entry.decorator);
                                }
                                else if (entry.type === 'params') {
                                    // The helper arg represents the types of the parameters. Since it's applied to the
                                    // class, it corresponds with the constructor parameters of the class.
                                    entry.types.forEach(function (type, index) { return getConstructorParamInfo(index).typeExpression = type; });
                                }
                            }
                        }
                        catch (e_3_1) { e_3 = { error: e_3_1 }; }
                        finally {
                            try {
                                if (_e && !_e.done && (_b = _d.return)) _b.call(_d);
                            }
                            finally { if (e_3) throw e_3.error; }
                        }
                    }
                    else if (isMemberDecorateCall(helperCall, matchesClass)) {
                        // The `__decorate` call is targeting a member of the class
                        var helperArgs = helperCall.arguments[0];
                        var memberName = helperCall.arguments[2].text;
                        try {
                            for (var _f = (e_4 = void 0, tslib_1.__values(helperArgs.elements)), _g = _f.next(); !_g.done; _g = _f.next()) {
                                var element = _g.value;
                                var entry = this.reflectDecorateHelperEntry(element);
                                if (entry === null) {
                                    continue;
                                }
                                if (entry.type === 'decorator') {
                                    // The helper arg was reflected to represent an actual decorator.
                                    if (this.isFromCore(entry.decorator)) {
                                        var decorators = memberDecorators.has(memberName) ? memberDecorators.get(memberName) : [];
                                        decorators.push(entry.decorator);
                                        memberDecorators.set(memberName, decorators);
                                    }
                                }
                                else {
                                    // Information on decorated parameters is not interesting for ngcc, so it's ignored.
                                }
                            }
                        }
                        catch (e_4_1) { e_4 = { error: e_4_1 }; }
                        finally {
                            try {
                                if (_g && !_g.done && (_c = _f.return)) _c.call(_f);
                            }
                            finally { if (e_4) throw e_4.error; }
                        }
                    }
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (helperCalls_1_1 && !helperCalls_1_1.done && (_a = helperCalls_1.return)) _a.call(helperCalls_1);
                }
                finally { if (e_2) throw e_2.error; }
            }
            return { classDecorators: classDecorators, memberDecorators: memberDecorators, constructorParamInfo: constructorParamInfo };
        };
        /**
         * Extract the details of an entry within a `__decorate` helper call. For example, given the
         * following code:
         *
         * ```
         * __decorate([
         *   Directive({ selector: '[someDirective]' }),
         *   tslib_1.__param(2, Inject(INJECTED_TOKEN)),
         *   tslib_1.__metadata("design:paramtypes", [ViewContainerRef, TemplateRef, String])
         * ], SomeDirective);
         * ```
         *
         * it can be seen that there are calls to regular decorators (the `Directive`) and calls into
         * `tslib` functions which have been inserted by TypeScript. Therefore, this function classifies
         * a call to correspond with
         *   1. a real decorator like `Directive` above, or
         *   2. a decorated parameter, corresponding with `__param` calls from `tslib`, or
         *   3. the type information of parameters, corresponding with `__metadata` call from `tslib`
         *
         * @param expression the expression that needs to be reflected into a `DecorateHelperEntry`
         * @returns an object that indicates which of the three categories the call represents, together
         * with the reflected information of the call, or null if the call is not a valid decorate call.
         */
        Esm2015ReflectionHost.prototype.reflectDecorateHelperEntry = function (expression) {
            // We only care about those elements that are actual calls
            if (!ts.isCallExpression(expression)) {
                return null;
            }
            var call = expression;
            var helperName = getCalleeName(call);
            if (helperName === '__metadata') {
                // This is a `tslib.__metadata` call, reflect to arguments into a `ParameterTypes` object
                // if the metadata key is "design:paramtypes".
                var key = call.arguments[0];
                if (key === undefined || !ts.isStringLiteral(key) || key.text !== 'design:paramtypes') {
                    return null;
                }
                var value = call.arguments[1];
                if (value === undefined || !ts.isArrayLiteralExpression(value)) {
                    return null;
                }
                return {
                    type: 'params',
                    types: Array.from(value.elements),
                };
            }
            if (helperName === '__param') {
                // This is a `tslib.__param` call that is reflected into a `ParameterDecorators` object.
                var indexArg = call.arguments[0];
                var index = indexArg && ts.isNumericLiteral(indexArg) ? parseInt(indexArg.text, 10) : NaN;
                if (isNaN(index)) {
                    return null;
                }
                var decoratorCall = call.arguments[1];
                if (decoratorCall === undefined || !ts.isCallExpression(decoratorCall)) {
                    return null;
                }
                var decorator_1 = this.reflectDecoratorCall(decoratorCall);
                if (decorator_1 === null) {
                    return null;
                }
                return {
                    type: 'param:decorators',
                    index: index,
                    decorator: decorator_1,
                };
            }
            // Otherwise attempt to reflect it as a regular decorator.
            var decorator = this.reflectDecoratorCall(call);
            if (decorator === null) {
                return null;
            }
            return {
                type: 'decorator',
                decorator: decorator,
            };
        };
        Esm2015ReflectionHost.prototype.reflectDecoratorCall = function (call) {
            var decoratorExpression = call.expression;
            if (!reflection_1.isDecoratorIdentifier(decoratorExpression)) {
                return null;
            }
            // We found a decorator!
            var decoratorIdentifier = ts.isIdentifier(decoratorExpression) ? decoratorExpression : decoratorExpression.name;
            return {
                name: decoratorIdentifier.text,
                identifier: decoratorExpression,
                import: this.getImportOfIdentifier(decoratorIdentifier),
                node: call,
                args: Array.from(call.arguments),
            };
        };
        /**
         * Check the given statement to see if it is a call to any of the specified helper functions or
         * null if not found.
         *
         * Matching statements will look like:  `tslib_1.__decorate(...);`.
         * @param statement the statement that may contain the call.
         * @param helperNames the names of the helper we are looking for.
         * @returns the node that corresponds to the `__decorate(...)` call or null if the statement
         * does not match.
         */
        Esm2015ReflectionHost.prototype.getHelperCall = function (statement, helperNames) {
            if ((ts.isExpressionStatement(statement) || ts.isReturnStatement(statement)) &&
                statement.expression) {
                var expression = statement.expression;
                while (isAssignment(expression)) {
                    expression = expression.right;
                }
                if (ts.isCallExpression(expression)) {
                    var calleeName = getCalleeName(expression);
                    if (calleeName !== null && helperNames.includes(calleeName)) {
                        return expression;
                    }
                }
            }
            return null;
        };
        /**
         * Reflect over the given array node and extract decorator information from each element.
         *
         * This is used for decorators that are defined in static properties. For example:
         *
         * ```
         * SomeDirective.decorators = [
         *   { type: Directive, args: [{ selector: '[someDirective]' },] }
         * ];
         * ```
         *
         * @param decoratorsArray an expression that contains decorator information.
         * @returns an array of decorator info that was reflected from the array node.
         */
        Esm2015ReflectionHost.prototype.reflectDecorators = function (decoratorsArray) {
            var _this = this;
            var decorators = [];
            if (ts.isArrayLiteralExpression(decoratorsArray)) {
                // Add each decorator that is imported from `@angular/core` into the `decorators` array
                decoratorsArray.elements.forEach(function (node) {
                    // If the decorator is not an object literal expression then we are not interested
                    if (ts.isObjectLiteralExpression(node)) {
                        // We are only interested in objects of the form: `{ type: DecoratorType, args: [...] }`
                        var decorator = reflection_1.reflectObjectLiteral(node);
                        // Is the value of the `type` property an identifier?
                        if (decorator.has('type')) {
                            var decoratorType = decorator.get('type');
                            if (reflection_1.isDecoratorIdentifier(decoratorType)) {
                                var decoratorIdentifier = ts.isIdentifier(decoratorType) ? decoratorType : decoratorType.name;
                                decorators.push({
                                    name: decoratorIdentifier.text,
                                    identifier: decoratorType,
                                    import: _this.getImportOfIdentifier(decoratorIdentifier),
                                    node: node,
                                    args: getDecoratorArgs(node),
                                });
                            }
                        }
                    }
                });
            }
            return decorators;
        };
        /**
         * Reflect over a symbol and extract the member information, combining it with the
         * provided decorator information, and whether it is a static member.
         *
         * A single symbol may represent multiple class members in the case of accessors;
         * an equally named getter/setter accessor pair is combined into a single symbol.
         * When the symbol is recognized as representing an accessor, its declarations are
         * analyzed such that both the setter and getter accessor are returned as separate
         * class members.
         *
         * One difference wrt the TypeScript host is that in ES2015, we cannot see which
         * accessor originally had any decorators applied to them, as decorators are applied
         * to the property descriptor in general, not a specific accessor. If an accessor
         * has both a setter and getter, any decorators are only attached to the setter member.
         *
         * @param symbol the symbol for the member to reflect over.
         * @param decorators an array of decorators associated with the member.
         * @param isStatic true if this member is static, false if it is an instance property.
         * @returns the reflected member information, or null if the symbol is not a member.
         */
        Esm2015ReflectionHost.prototype.reflectMembers = function (symbol, decorators, isStatic) {
            if (symbol.flags & ts.SymbolFlags.Accessor) {
                var members = [];
                var setter = symbol.declarations && symbol.declarations.find(ts.isSetAccessor);
                var getter = symbol.declarations && symbol.declarations.find(ts.isGetAccessor);
                var setterMember = setter && this.reflectMember(setter, reflection_1.ClassMemberKind.Setter, decorators, isStatic);
                if (setterMember) {
                    members.push(setterMember);
                    // Prevent attaching the decorators to a potential getter. In ES2015, we can't tell where
                    // the decorators were originally attached to, however we only want to attach them to a
                    // single `ClassMember` as otherwise ngtsc would handle the same decorators twice.
                    decorators = undefined;
                }
                var getterMember = getter && this.reflectMember(getter, reflection_1.ClassMemberKind.Getter, decorators, isStatic);
                if (getterMember) {
                    members.push(getterMember);
                }
                return members;
            }
            var kind = null;
            if (symbol.flags & ts.SymbolFlags.Method) {
                kind = reflection_1.ClassMemberKind.Method;
            }
            else if (symbol.flags & ts.SymbolFlags.Property) {
                kind = reflection_1.ClassMemberKind.Property;
            }
            var node = symbol.valueDeclaration || symbol.declarations && symbol.declarations[0];
            if (!node) {
                // If the symbol has been imported from a TypeScript typings file then the compiler
                // may pass the `prototype` symbol as an export of the class.
                // But this has no declaration. In this case we just quietly ignore it.
                return null;
            }
            var member = this.reflectMember(node, kind, decorators, isStatic);
            if (!member) {
                return null;
            }
            return [member];
        };
        /**
         * Reflect over a symbol and extract the member information, combining it with the
         * provided decorator information, and whether it is a static member.
         * @param node the declaration node for the member to reflect over.
         * @param kind the assumed kind of the member, may become more accurate during reflection.
         * @param decorators an array of decorators associated with the member.
         * @param isStatic true if this member is static, false if it is an instance property.
         * @returns the reflected member information, or null if the symbol is not a member.
         */
        Esm2015ReflectionHost.prototype.reflectMember = function (node, kind, decorators, isStatic) {
            var value = null;
            var name = null;
            var nameNode = null;
            if (!isClassMemberType(node)) {
                return null;
            }
            if (isStatic && isPropertyAccess(node)) {
                name = node.name.text;
                value = kind === reflection_1.ClassMemberKind.Property ? node.parent.right : null;
            }
            else if (isThisAssignment(node)) {
                kind = reflection_1.ClassMemberKind.Property;
                name = node.left.name.text;
                value = node.right;
                isStatic = false;
            }
            else if (ts.isConstructorDeclaration(node)) {
                kind = reflection_1.ClassMemberKind.Constructor;
                name = 'constructor';
                isStatic = false;
            }
            if (kind === null) {
                this.logger.warn("Unknown member type: \"" + node.getText());
                return null;
            }
            if (!name) {
                if (isNamedDeclaration(node)) {
                    name = node.name.text;
                    nameNode = node.name;
                }
                else {
                    return null;
                }
            }
            // If we have still not determined if this is a static or instance member then
            // look for the `static` keyword on the declaration
            if (isStatic === undefined) {
                isStatic = node.modifiers !== undefined &&
                    node.modifiers.some(function (mod) { return mod.kind === ts.SyntaxKind.StaticKeyword; });
            }
            var type = node.type || null;
            return {
                node: node,
                implementation: node,
                kind: kind,
                type: type,
                name: name,
                nameNode: nameNode,
                value: value,
                isStatic: isStatic,
                decorators: decorators || []
            };
        };
        /**
         * Find the declarations of the constructor parameters of a class identified by its symbol.
         * @param classSymbol the class whose parameters we want to find.
         * @returns an array of `ts.ParameterDeclaration` objects representing each of the parameters in
         * the class's constructor or null if there is no constructor.
         */
        Esm2015ReflectionHost.prototype.getConstructorParameterDeclarations = function (classSymbol) {
            var members = classSymbol.implementation.members;
            if (members && members.has(exports.CONSTRUCTOR)) {
                var constructorSymbol = members.get(exports.CONSTRUCTOR);
                // For some reason the constructor does not have a `valueDeclaration` ?!?
                var constructor = constructorSymbol.declarations &&
                    constructorSymbol.declarations[0];
                if (!constructor) {
                    return [];
                }
                if (constructor.parameters.length > 0) {
                    return Array.from(constructor.parameters);
                }
                if (isSynthesizedConstructor(constructor)) {
                    return null;
                }
                return [];
            }
            return null;
        };
        /**
         * Get the parameter decorators of a class constructor.
         *
         * @param classSymbol the class whose parameter info we want to get.
         * @param parameterNodes the array of TypeScript parameter nodes for this class's constructor.
         * @returns an array of constructor parameter info objects.
         */
        Esm2015ReflectionHost.prototype.getConstructorParamInfo = function (classSymbol, parameterNodes) {
            var _this = this;
            var constructorParamInfo = this.acquireDecoratorInfo(classSymbol).constructorParamInfo;
            return parameterNodes.map(function (node, index) {
                var _a = constructorParamInfo[index] ?
                    constructorParamInfo[index] :
                    { decorators: null, typeExpression: null }, decorators = _a.decorators, typeExpression = _a.typeExpression;
                var nameNode = node.name;
                var typeValueReference = null;
                if (typeExpression !== null) {
                    // `typeExpression` is an expression in a "type" context. Resolve it to a declared value.
                    // Either it's a reference to an imported type, or a type declared locally. Distinguish the
                    // two cases with `getDeclarationOfExpression`.
                    var decl = _this.getDeclarationOfExpression(typeExpression);
                    if (decl !== null && decl.node !== null && decl.viaModule !== null &&
                        isNamedDeclaration(decl.node)) {
                        typeValueReference = {
                            local: false,
                            valueDeclaration: decl.node,
                            moduleName: decl.viaModule,
                            importedName: decl.node.name.text,
                            nestedPath: null,
                        };
                    }
                    else {
                        typeValueReference = {
                            local: true,
                            expression: typeExpression,
                            defaultImportStatement: null,
                        };
                    }
                }
                return {
                    name: utils_1.getNameText(nameNode),
                    nameNode: nameNode,
                    typeValueReference: typeValueReference,
                    typeNode: null,
                    decorators: decorators
                };
            });
        };
        /**
         * Get the parameter type and decorators for the constructor of a class,
         * where the information is stored on a static property of the class.
         *
         * Note that in ESM2015, the property is defined an array, or by an arrow function that returns
         * an array, of decorator and type information.
         *
         * For example,
         *
         * ```
         * SomeDirective.ctorParameters = () => [
         *   {type: ViewContainerRef},
         *   {type: TemplateRef},
         *   {type: undefined, decorators: [{ type: Inject, args: [INJECTED_TOKEN]}]},
         * ];
         * ```
         *
         * or
         *
         * ```
         * SomeDirective.ctorParameters = [
         *   {type: ViewContainerRef},
         *   {type: TemplateRef},
         *   {type: undefined, decorators: [{type: Inject, args: [INJECTED_TOKEN]}]},
         * ];
         * ```
         *
         * @param paramDecoratorsProperty the property that holds the parameter info we want to get.
         * @returns an array of objects containing the type and decorators for each parameter.
         */
        Esm2015ReflectionHost.prototype.getParamInfoFromStaticProperty = function (paramDecoratorsProperty) {
            var _this = this;
            var paramDecorators = getPropertyValueFromSymbol(paramDecoratorsProperty);
            if (paramDecorators) {
                // The decorators array may be wrapped in an arrow function. If so unwrap it.
                var container = ts.isArrowFunction(paramDecorators) ? paramDecorators.body : paramDecorators;
                if (ts.isArrayLiteralExpression(container)) {
                    var elements = container.elements;
                    return elements
                        .map(function (element) {
                        return ts.isObjectLiteralExpression(element) ? reflection_1.reflectObjectLiteral(element) : null;
                    })
                        .map(function (paramInfo) {
                        var typeExpression = paramInfo && paramInfo.has('type') ? paramInfo.get('type') : null;
                        var decoratorInfo = paramInfo && paramInfo.has('decorators') ? paramInfo.get('decorators') : null;
                        var decorators = decoratorInfo &&
                            _this.reflectDecorators(decoratorInfo)
                                .filter(function (decorator) { return _this.isFromCore(decorator); });
                        return { typeExpression: typeExpression, decorators: decorators };
                    });
                }
                else if (paramDecorators !== undefined) {
                    this.logger.warn('Invalid constructor parameter decorator in ' +
                        paramDecorators.getSourceFile().fileName + ':\n', paramDecorators.getText());
                }
            }
            return null;
        };
        /**
         * Search statements related to the given class for calls to the specified helper.
         * @param classSymbol the class whose helper calls we are interested in.
         * @param helperNames the names of the helpers (e.g. `__decorate`) whose calls we are interested
         * in.
         * @returns an array of CallExpression nodes for each matching helper call.
         */
        Esm2015ReflectionHost.prototype.getHelperCallsForClass = function (classSymbol, helperNames) {
            var _this = this;
            return this.getStatementsForClass(classSymbol)
                .map(function (statement) { return _this.getHelperCall(statement, helperNames); })
                .filter(utils_1.isDefined);
        };
        /**
         * Find statements related to the given class that may contain calls to a helper.
         *
         * In ESM2015 code the helper calls are in the top level module, so we have to consider
         * all the statements in the module.
         *
         * @param classSymbol the class whose helper calls we are interested in.
         * @returns an array of statements that may contain helper calls.
         */
        Esm2015ReflectionHost.prototype.getStatementsForClass = function (classSymbol) {
            var classNode = classSymbol.implementation.valueDeclaration;
            if (isTopLevel(classNode)) {
                return this.getModuleStatements(classNode.getSourceFile());
            }
            var statement = getContainingStatement(classNode);
            if (ts.isBlock(statement.parent)) {
                return Array.from(statement.parent.statements);
            }
            // We should never arrive here
            throw new Error("Unable to find adjacent statements for " + classSymbol.name);
        };
        /**
         * Test whether a decorator was imported from `@angular/core`.
         *
         * Is the decorator:
         * * externally imported from `@angular/core`?
         * * the current hosted program is actually `@angular/core` and
         *   - relatively internally imported; or
         *   - not imported, from the current file.
         *
         * @param decorator the decorator to test.
         */
        Esm2015ReflectionHost.prototype.isFromCore = function (decorator) {
            if (this.isCore) {
                return !decorator.import || /^\./.test(decorator.import.from);
            }
            else {
                return !!decorator.import && decorator.import.from === '@angular/core';
            }
        };
        /**
         * Create a mapping between the public exports in a src program and the public exports of a dts
         * program.
         *
         * @param src the program bundle containing the source files.
         * @param dts the program bundle containing the typings files.
         * @returns a map of source declarations to typings declarations.
         */
        Esm2015ReflectionHost.prototype.computePublicDtsDeclarationMap = function (src, dts) {
            var declarationMap = new Map();
            var dtsDeclarationMap = new Map();
            var rootDts = getRootFileOrFail(dts);
            this.collectDtsExportedDeclarations(dtsDeclarationMap, rootDts, dts.program.getTypeChecker());
            var rootSrc = getRootFileOrFail(src);
            this.collectSrcExportedDeclarations(declarationMap, dtsDeclarationMap, rootSrc);
            return declarationMap;
        };
        /**
         * Create a mapping between the "private" exports in a src program and the "private" exports of a
         * dts program. These exports may be exported from individual files in the src or dts programs,
         * but not exported from the root file (i.e publicly from the entry-point).
         *
         * This mapping is a "best guess" since we cannot guarantee that two declarations that happen to
         * be exported from a file with the same name are actually equivalent. But this is a reasonable
         * estimate for the purposes of ngcc.
         *
         * @param src the program bundle containing the source files.
         * @param dts the program bundle containing the typings files.
         * @returns a map of source declarations to typings declarations.
         */
        Esm2015ReflectionHost.prototype.computePrivateDtsDeclarationMap = function (src, dts) {
            var e_5, _a, e_6, _b;
            var declarationMap = new Map();
            var dtsDeclarationMap = new Map();
            var typeChecker = dts.program.getTypeChecker();
            var dtsFiles = getNonRootPackageFiles(dts);
            try {
                for (var dtsFiles_1 = tslib_1.__values(dtsFiles), dtsFiles_1_1 = dtsFiles_1.next(); !dtsFiles_1_1.done; dtsFiles_1_1 = dtsFiles_1.next()) {
                    var dtsFile = dtsFiles_1_1.value;
                    this.collectDtsExportedDeclarations(dtsDeclarationMap, dtsFile, typeChecker);
                }
            }
            catch (e_5_1) { e_5 = { error: e_5_1 }; }
            finally {
                try {
                    if (dtsFiles_1_1 && !dtsFiles_1_1.done && (_a = dtsFiles_1.return)) _a.call(dtsFiles_1);
                }
                finally { if (e_5) throw e_5.error; }
            }
            var srcFiles = getNonRootPackageFiles(src);
            try {
                for (var srcFiles_1 = tslib_1.__values(srcFiles), srcFiles_1_1 = srcFiles_1.next(); !srcFiles_1_1.done; srcFiles_1_1 = srcFiles_1.next()) {
                    var srcFile = srcFiles_1_1.value;
                    this.collectSrcExportedDeclarations(declarationMap, dtsDeclarationMap, srcFile);
                }
            }
            catch (e_6_1) { e_6 = { error: e_6_1 }; }
            finally {
                try {
                    if (srcFiles_1_1 && !srcFiles_1_1.done && (_b = srcFiles_1.return)) _b.call(srcFiles_1);
                }
                finally { if (e_6) throw e_6.error; }
            }
            return declarationMap;
        };
        /**
         * Collect mappings between names of exported declarations in a file and its actual declaration.
         *
         * Any new mappings are added to the `dtsDeclarationMap`.
         */
        Esm2015ReflectionHost.prototype.collectDtsExportedDeclarations = function (dtsDeclarationMap, srcFile, checker) {
            var srcModule = srcFile && checker.getSymbolAtLocation(srcFile);
            var moduleExports = srcModule && checker.getExportsOfModule(srcModule);
            if (moduleExports) {
                moduleExports.forEach(function (exportedSymbol) {
                    var name = exportedSymbol.name;
                    if (exportedSymbol.flags & ts.SymbolFlags.Alias) {
                        exportedSymbol = checker.getAliasedSymbol(exportedSymbol);
                    }
                    var declaration = exportedSymbol.valueDeclaration;
                    if (declaration && !dtsDeclarationMap.has(name)) {
                        dtsDeclarationMap.set(name, declaration);
                    }
                });
            }
        };
        Esm2015ReflectionHost.prototype.collectSrcExportedDeclarations = function (declarationMap, dtsDeclarationMap, srcFile) {
            var e_7, _a;
            var fileExports = this.getExportsOfModule(srcFile);
            if (fileExports !== null) {
                try {
                    for (var fileExports_1 = tslib_1.__values(fileExports), fileExports_1_1 = fileExports_1.next(); !fileExports_1_1.done; fileExports_1_1 = fileExports_1.next()) {
                        var _b = tslib_1.__read(fileExports_1_1.value, 2), exportName = _b[0], declaration = _b[1].node;
                        if (declaration !== null && dtsDeclarationMap.has(exportName)) {
                            declarationMap.set(declaration, dtsDeclarationMap.get(exportName));
                        }
                    }
                }
                catch (e_7_1) { e_7 = { error: e_7_1 }; }
                finally {
                    try {
                        if (fileExports_1_1 && !fileExports_1_1.done && (_a = fileExports_1.return)) _a.call(fileExports_1);
                    }
                    finally { if (e_7) throw e_7.error; }
                }
            }
        };
        Esm2015ReflectionHost.prototype.getDeclarationOfExpression = function (expression) {
            if (ts.isIdentifier(expression)) {
                return this.getDeclarationOfIdentifier(expression);
            }
            if (!ts.isPropertyAccessExpression(expression) || !ts.isIdentifier(expression.expression)) {
                return null;
            }
            var namespaceDecl = this.getDeclarationOfIdentifier(expression.expression);
            if (!namespaceDecl || namespaceDecl.node === null || !ts.isSourceFile(namespaceDecl.node)) {
                return null;
            }
            var namespaceExports = this.getExportsOfModule(namespaceDecl.node);
            if (namespaceExports === null) {
                return null;
            }
            if (!namespaceExports.has(expression.name.text)) {
                return null;
            }
            var exportDecl = namespaceExports.get(expression.name.text);
            return tslib_1.__assign(tslib_1.__assign({}, exportDecl), { viaModule: namespaceDecl.viaModule });
        };
        /** Checks if the specified declaration resolves to the known JavaScript global `Object`. */
        Esm2015ReflectionHost.prototype.isJavaScriptObjectDeclaration = function (decl) {
            if (decl.node === null) {
                return false;
            }
            var node = decl.node;
            // The default TypeScript library types the global `Object` variable through
            // a variable declaration with a type reference resolving to `ObjectConstructor`.
            if (!ts.isVariableDeclaration(node) || !ts.isIdentifier(node.name) ||
                node.name.text !== 'Object' || node.type === undefined) {
                return false;
            }
            var typeNode = node.type;
            // If the variable declaration does not have a type resolving to `ObjectConstructor`,
            // we cannot guarantee that the declaration resolves to the global `Object` variable.
            if (!ts.isTypeReferenceNode(typeNode) || !ts.isIdentifier(typeNode.typeName) ||
                typeNode.typeName.text !== 'ObjectConstructor') {
                return false;
            }
            // Finally, check if the type definition for `Object` originates from a default library
            // definition file. This requires default types to be enabled for the host program.
            return this.src.program.isSourceFileDefaultLibrary(node.getSourceFile());
        };
        /**
         * In JavaScript, enum declarations are emitted as a regular variable declaration followed by an
         * IIFE in which the enum members are assigned.
         *
         *   export var Enum;
         *   (function (Enum) {
         *     Enum["a"] = "A";
         *     Enum["b"] = "B";
         *   })(Enum || (Enum = {}));
         *
         * @param declaration A variable declaration that may represent an enum
         * @returns An array of enum members if the variable declaration is followed by an IIFE that
         * declares the enum members, or null otherwise.
         */
        Esm2015ReflectionHost.prototype.resolveEnumMembers = function (declaration) {
            // Initialized variables don't represent enum declarations.
            if (declaration.initializer !== undefined)
                return null;
            var variableStmt = declaration.parent.parent;
            if (!ts.isVariableStatement(variableStmt))
                return null;
            var block = variableStmt.parent;
            if (!ts.isBlock(block) && !ts.isSourceFile(block))
                return null;
            var declarationIndex = block.statements.findIndex(function (statement) { return statement === variableStmt; });
            if (declarationIndex === -1 || declarationIndex === block.statements.length - 1)
                return null;
            var subsequentStmt = block.statements[declarationIndex + 1];
            if (!ts.isExpressionStatement(subsequentStmt))
                return null;
            var iife = utils_2.stripParentheses(subsequentStmt.expression);
            if (!ts.isCallExpression(iife) || !isEnumDeclarationIife(iife))
                return null;
            var fn = utils_2.stripParentheses(iife.expression);
            if (!ts.isFunctionExpression(fn))
                return null;
            return this.reflectEnumMembers(fn);
        };
        /**
         * Attempts to extract all `EnumMember`s from a function that is according to the JavaScript emit
         * format for enums:
         *
         *   function (Enum) {
         *     Enum["MemberA"] = "a";
         *     Enum["MemberB"] = "b";
         *   }
         *
         * @param fn The function expression that is assumed to contain enum members.
         * @returns All enum members if the function is according to the correct syntax, null otherwise.
         */
        Esm2015ReflectionHost.prototype.reflectEnumMembers = function (fn) {
            var e_8, _a;
            if (fn.parameters.length !== 1)
                return null;
            var enumName = fn.parameters[0].name;
            if (!ts.isIdentifier(enumName))
                return null;
            var enumMembers = [];
            try {
                for (var _b = tslib_1.__values(fn.body.statements), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var statement = _c.value;
                    var enumMember = this.reflectEnumMember(enumName, statement);
                    if (enumMember === null) {
                        return null;
                    }
                    enumMembers.push(enumMember);
                }
            }
            catch (e_8_1) { e_8 = { error: e_8_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                }
                finally { if (e_8) throw e_8.error; }
            }
            return enumMembers;
        };
        /**
         * Attempts to extract a single `EnumMember` from a statement in the following syntax:
         *
         *   Enum["MemberA"] = "a";
         *
         * or, for enum member with numeric values:
         *
         *   Enum[Enum["MemberA"] = 0] = "MemberA";
         *
         * @param enumName The identifier of the enum that the members should be set on.
         * @param statement The statement to inspect.
         * @returns An `EnumMember` if the statement is according to the expected syntax, null otherwise.
         */
        Esm2015ReflectionHost.prototype.reflectEnumMember = function (enumName, statement) {
            if (!ts.isExpressionStatement(statement))
                return null;
            var expression = statement.expression;
            // Check for the `Enum[X] = Y;` case.
            if (!isEnumAssignment(enumName, expression)) {
                return null;
            }
            var assignment = reflectEnumAssignment(expression);
            if (assignment != null) {
                return assignment;
            }
            // Check for the `Enum[Enum[X] = Y] = ...;` case.
            var innerExpression = expression.left.argumentExpression;
            if (!isEnumAssignment(enumName, innerExpression)) {
                return null;
            }
            return reflectEnumAssignment(innerExpression);
        };
        return Esm2015ReflectionHost;
    }(reflection_1.TypeScriptReflectionHost));
    exports.Esm2015ReflectionHost = Esm2015ReflectionHost;
    ///////////// Exported Helpers /////////////
    /**
     * Checks whether the iife has the following call signature:
     *
     *   (Enum || (Enum = {})
     *
     * Note that the `Enum` identifier is not checked, as it could also be something
     * like `exports.Enum`. Instead, only the structure of binary operators is checked.
     *
     * @param iife The call expression to check.
     * @returns true if the iife has a call signature that corresponds with a potential
     * enum declaration.
     */
    function isEnumDeclarationIife(iife) {
        if (iife.arguments.length !== 1)
            return false;
        var arg = iife.arguments[0];
        if (!ts.isBinaryExpression(arg) || arg.operatorToken.kind !== ts.SyntaxKind.BarBarToken ||
            !ts.isParenthesizedExpression(arg.right)) {
            return false;
        }
        var right = arg.right.expression;
        if (!ts.isBinaryExpression(right) || right.operatorToken.kind !== ts.SyntaxKind.EqualsToken) {
            return false;
        }
        if (!ts.isObjectLiteralExpression(right.right) || right.right.properties.length !== 0) {
            return false;
        }
        return true;
    }
    /**
     * Checks whether the expression looks like an enum member assignment targeting `Enum`:
     *
     *   Enum[X] = Y;
     *
     * Here, X and Y can be any expression.
     *
     * @param enumName The identifier of the enum that the members should be set on.
     * @param expression The expression that should be checked to conform to the above form.
     * @returns true if the expression is of the correct form, false otherwise.
     */
    function isEnumAssignment(enumName, expression) {
        if (!ts.isBinaryExpression(expression) ||
            expression.operatorToken.kind !== ts.SyntaxKind.EqualsToken ||
            !ts.isElementAccessExpression(expression.left)) {
            return false;
        }
        // Verify that the outer assignment corresponds with the enum declaration.
        var enumIdentifier = expression.left.expression;
        return ts.isIdentifier(enumIdentifier) && enumIdentifier.text === enumName.text;
    }
    /**
     * Attempts to create an `EnumMember` from an expression that is believed to represent an enum
     * assignment.
     *
     * @param expression The expression that is believed to be an enum assignment.
     * @returns An `EnumMember` or null if the expression did not represent an enum member after all.
     */
    function reflectEnumAssignment(expression) {
        var memberName = expression.left.argumentExpression;
        if (!ts.isPropertyName(memberName))
            return null;
        return { name: memberName, initializer: expression.right };
    }
    /**
     * Test whether a statement node is an assignment statement.
     * @param statement the statement to test.
     */
    function isAssignmentStatement(statement) {
        return ts.isExpressionStatement(statement) && isAssignment(statement.expression) &&
            ts.isIdentifier(statement.expression.left);
    }
    exports.isAssignmentStatement = isAssignmentStatement;
    /**
     * Parse the `expression` that is believed to be an IIFE and return the AST node that corresponds to
     * the body of the IIFE.
     *
     * The expression may be wrapped in parentheses, which are stripped off.
     *
     * If the IIFE is an arrow function then its body could be a `ts.Expression` rather than a
     * `ts.FunctionBody`.
     *
     * @param expression the expression to parse.
     * @returns the `ts.Expression` or `ts.FunctionBody` that holds the body of the IIFE or `undefined`
     *     if the `expression` did not have the correct shape.
     */
    function getIifeBody(expression) {
        var call = utils_2.stripParentheses(expression);
        if (!ts.isCallExpression(call)) {
            return undefined;
        }
        var fn = utils_2.stripParentheses(call.expression);
        if (!ts.isFunctionExpression(fn) && !ts.isArrowFunction(fn)) {
            return undefined;
        }
        return fn.body;
    }
    exports.getIifeBody = getIifeBody;
    /**
     * Returns true if the `node` is an assignment of the form `a = b`.
     *
     * @param node The AST node to check.
     */
    function isAssignment(node) {
        return ts.isBinaryExpression(node) && node.operatorToken.kind === ts.SyntaxKind.EqualsToken;
    }
    exports.isAssignment = isAssignment;
    /**
     * Tests whether the provided call expression targets a class, by verifying its arguments are
     * according to the following form:
     *
     * ```
     * __decorate([], SomeDirective);
     * ```
     *
     * @param call the call expression that is tested to represent a class decorator call.
     * @param matches predicate function to test whether the call is associated with the desired class.
     */
    function isClassDecorateCall(call, matches) {
        var helperArgs = call.arguments[0];
        if (helperArgs === undefined || !ts.isArrayLiteralExpression(helperArgs)) {
            return false;
        }
        var target = call.arguments[1];
        return target !== undefined && ts.isIdentifier(target) && matches(target);
    }
    exports.isClassDecorateCall = isClassDecorateCall;
    /**
     * Tests whether the provided call expression targets a member of the class, by verifying its
     * arguments are according to the following form:
     *
     * ```
     * __decorate([], SomeDirective.prototype, "member", void 0);
     * ```
     *
     * @param call the call expression that is tested to represent a member decorator call.
     * @param matches predicate function to test whether the call is associated with the desired class.
     */
    function isMemberDecorateCall(call, matches) {
        var helperArgs = call.arguments[0];
        if (helperArgs === undefined || !ts.isArrayLiteralExpression(helperArgs)) {
            return false;
        }
        var target = call.arguments[1];
        if (target === undefined || !ts.isPropertyAccessExpression(target) ||
            !ts.isIdentifier(target.expression) || !matches(target.expression) ||
            target.name.text !== 'prototype') {
            return false;
        }
        var memberName = call.arguments[2];
        return memberName !== undefined && ts.isStringLiteral(memberName);
    }
    exports.isMemberDecorateCall = isMemberDecorateCall;
    /**
     * Helper method to extract the value of a property given the property's "symbol",
     * which is actually the symbol of the identifier of the property.
     */
    function getPropertyValueFromSymbol(propSymbol) {
        var propIdentifier = propSymbol.valueDeclaration;
        var parent = propIdentifier && propIdentifier.parent;
        return parent && ts.isBinaryExpression(parent) ? parent.right : undefined;
    }
    exports.getPropertyValueFromSymbol = getPropertyValueFromSymbol;
    /**
     * A callee could be one of: `__decorate(...)` or `tslib_1.__decorate`.
     */
    function getCalleeName(call) {
        if (ts.isIdentifier(call.expression)) {
            return utils_1.stripDollarSuffix(call.expression.text);
        }
        if (ts.isPropertyAccessExpression(call.expression)) {
            return utils_1.stripDollarSuffix(call.expression.name.text);
        }
        return null;
    }
    function isInitializedVariableClassDeclaration(node) {
        return reflection_1.isNamedVariableDeclaration(node) && node.initializer !== undefined;
    }
    /**
     * Handle a variable declaration of the form
     *
     * ```
     * var MyClass = alias1 = alias2 = <<declaration>>
     * ```
     *
     * @node the LHS of a variable declaration.
     * @returns the original AST node or the RHS of a series of assignments in a variable
     *     declaration.
     */
    function skipClassAliases(node) {
        var expression = node.initializer;
        while (isAssignment(expression)) {
            expression = expression.right;
        }
        return expression;
    }
    exports.skipClassAliases = skipClassAliases;
    /**
     * This expression could either be a class expression
     *
     * ```
     * class MyClass {};
     * ```
     *
     * or an IIFE wrapped class expression
     *
     * ```
     * (() => {
     *   class MyClass {}
     *   ...
     *   return MyClass;
     * })()
     * ```
     *
     * or an IIFE wrapped aliased class expression
     *
     * ```
     * (() => {
     *   let MyClass = class MyClass {}
     *   ...
     *   return MyClass;
     * })()
     * ```
     *
     * or an IFFE wrapped ES5 class function
     *
     * ```
     * (function () {
     *  function MyClass() {}
     *  ...
     *  return MyClass
     * })()
     * ```
     *
     * @param expression the node that represents the class whose declaration we are finding.
     * @returns the declaration of the class or `null` if it is not a "class".
     */
    function getInnerClassDeclaration(expression) {
        var e_9, _a, e_10, _b;
        if (ts.isClassExpression(expression) && utils_1.hasNameIdentifier(expression)) {
            return expression;
        }
        var iifeBody = getIifeBody(expression);
        if (iifeBody === undefined) {
            return null;
        }
        if (!ts.isBlock(iifeBody)) {
            // Handle the fat arrow expression case: `() => ClassExpression`
            return ts.isClassExpression(iifeBody) && isNamedDeclaration(iifeBody) ? iifeBody : null;
        }
        else {
            try {
                // Handle the case of a normal or fat-arrow function with a body.
                // Return the first ClassDeclaration/VariableDeclaration inside the body
                for (var _c = tslib_1.__values(iifeBody.statements), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var statement = _d.value;
                    if (reflection_1.isNamedClassDeclaration(statement) || reflection_1.isNamedFunctionDeclaration(statement)) {
                        return statement;
                    }
                    if (ts.isVariableStatement(statement)) {
                        try {
                            for (var _e = (e_10 = void 0, tslib_1.__values(statement.declarationList.declarations)), _f = _e.next(); !_f.done; _f = _e.next()) {
                                var declaration = _f.value;
                                if (isInitializedVariableClassDeclaration(declaration)) {
                                    var expression_1 = skipClassAliases(declaration);
                                    if (ts.isClassExpression(expression_1) && utils_1.hasNameIdentifier(expression_1)) {
                                        return expression_1;
                                    }
                                }
                            }
                        }
                        catch (e_10_1) { e_10 = { error: e_10_1 }; }
                        finally {
                            try {
                                if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
                            }
                            finally { if (e_10) throw e_10.error; }
                        }
                    }
                }
            }
            catch (e_9_1) { e_9 = { error: e_9_1 }; }
            finally {
                try {
                    if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                }
                finally { if (e_9) throw e_9.error; }
            }
        }
        return null;
    }
    function getDecoratorArgs(node) {
        // The arguments of a decorator are held in the `args` property of its declaration object.
        var argsProperty = node.properties.filter(ts.isPropertyAssignment)
            .find(function (property) { return utils_1.getNameText(property.name) === 'args'; });
        var argsExpression = argsProperty && argsProperty.initializer;
        return argsExpression && ts.isArrayLiteralExpression(argsExpression) ?
            Array.from(argsExpression.elements) :
            [];
    }
    function isPropertyAccess(node) {
        return !!node.parent && ts.isBinaryExpression(node.parent) && ts.isPropertyAccessExpression(node);
    }
    function isThisAssignment(node) {
        return ts.isBinaryExpression(node) && ts.isPropertyAccessExpression(node.left) &&
            node.left.expression.kind === ts.SyntaxKind.ThisKeyword;
    }
    function isNamedDeclaration(node) {
        var anyNode = node;
        return !!anyNode.name && ts.isIdentifier(anyNode.name);
    }
    function isClassMemberType(node) {
        return (ts.isClassElement(node) || isPropertyAccess(node) || ts.isBinaryExpression(node)) &&
            // Additionally, ensure `node` is not an index signature, for example on an abstract class:
            // `abstract class Foo { [key: string]: any; }`
            !ts.isIndexSignatureDeclaration(node);
    }
    /**
     * Attempt to resolve the variable declaration that the given declaration is assigned to.
     * For example, for the following code:
     *
     * ```
     * var MyClass = MyClass_1 = class MyClass {};
     * ```
     *
     * or
     *
     * ```
     * var MyClass = MyClass_1 = (() => {
     *   class MyClass {}
     *   ...
     *   return MyClass;
     * })()
      ```
     *
     * and the provided declaration being `class MyClass {}`, this will return the `var MyClass`
     * declaration.
     *
     * @param declaration The declaration for which any variable declaration should be obtained.
     * @returns the outer variable declaration if found, undefined otherwise.
     */
    function getFarLeftHandSideOfAssignment(declaration) {
        var node = declaration.parent;
        // Detect an intermediary variable assignment and skip over it.
        if (isAssignment(node) && ts.isIdentifier(node.left)) {
            node = node.parent;
        }
        return ts.isVariableDeclaration(node) ? node : undefined;
    }
    function getContainingVariableDeclaration(node) {
        node = node.parent;
        while (node !== undefined) {
            if (reflection_1.isNamedVariableDeclaration(node)) {
                return node;
            }
            node = node.parent;
        }
        return undefined;
    }
    /**
     * A constructor function may have been "synthesized" by TypeScript during JavaScript emit,
     * in the case no user-defined constructor exists and e.g. property initializers are used.
     * Those initializers need to be emitted into a constructor in JavaScript, so the TypeScript
     * compiler generates a synthetic constructor.
     *
     * We need to identify such constructors as ngcc needs to be able to tell if a class did
     * originally have a constructor in the TypeScript source. When a class has a superclass,
     * a synthesized constructor must not be considered as a user-defined constructor as that
     * prevents a base factory call from being created by ngtsc, resulting in a factory function
     * that does not inject the dependencies of the superclass. Hence, we identify a default
     * synthesized super call in the constructor body, according to the structure that TypeScript
     * emits during JavaScript emit:
     * https://github.com/Microsoft/TypeScript/blob/v3.2.2/src/compiler/transformers/ts.ts#L1068-L1082
     *
     * @param constructor a constructor function to test
     * @returns true if the constructor appears to have been synthesized
     */
    function isSynthesizedConstructor(constructor) {
        if (!constructor.body)
            return false;
        var firstStatement = constructor.body.statements[0];
        if (!firstStatement || !ts.isExpressionStatement(firstStatement))
            return false;
        return isSynthesizedSuperCall(firstStatement.expression);
    }
    /**
     * Tests whether the expression appears to have been synthesized by TypeScript, i.e. whether
     * it is of the following form:
     *
     * ```
     * super(...arguments);
     * ```
     *
     * @param expression the expression that is to be tested
     * @returns true if the expression appears to be a synthesized super call
     */
    function isSynthesizedSuperCall(expression) {
        if (!ts.isCallExpression(expression))
            return false;
        if (expression.expression.kind !== ts.SyntaxKind.SuperKeyword)
            return false;
        if (expression.arguments.length !== 1)
            return false;
        var argument = expression.arguments[0];
        return ts.isSpreadElement(argument) && ts.isIdentifier(argument.expression) &&
            argument.expression.text === 'arguments';
    }
    /**
     * Find the statement that contains the given node
     * @param node a node whose containing statement we wish to find
     */
    function getContainingStatement(node) {
        while (node.parent) {
            if (ts.isBlock(node.parent) || ts.isSourceFile(node.parent)) {
                break;
            }
            node = node.parent;
        }
        return node;
    }
    exports.getContainingStatement = getContainingStatement;
    function getRootFileOrFail(bundle) {
        var rootFile = bundle.program.getSourceFile(bundle.path);
        if (rootFile === undefined) {
            throw new Error("The given rootPath " + rootFile + " is not a file of the program.");
        }
        return rootFile;
    }
    function getNonRootPackageFiles(bundle) {
        var rootFile = bundle.program.getSourceFile(bundle.path);
        return bundle.program.getSourceFiles().filter(function (f) { return (f !== rootFile) && util_1.isWithinPackage(bundle.package, file_system_1.absoluteFromSourceFile(f)); });
    }
    function isTopLevel(node) {
        while (node = node.parent) {
            if (ts.isBlock(node)) {
                return false;
            }
        }
        return true;
    }
    /**
     * Get the actual (outer) declaration of a class.
     *
     * Sometimes, the implementation of a class is an expression that is hidden inside an IIFE and
     * returned to be assigned to a variable outside the IIFE, which is what the rest of the program
     * interacts with.
     *
     * Given the inner declaration, we want to get to the declaration of the outer variable that
     * represents the class.
     *
     * @param node a node that could be the inner declaration inside an IIFE.
     * @returns the outer variable declaration or `null` if it is not a "class".
     */
    function getClassDeclarationFromInnerDeclaration(node) {
        if (ts.isFunctionDeclaration(node) || ts.isClassDeclaration(node) ||
            ts.isVariableStatement(node)) {
            // It might be the function expression inside the IIFE. We need to go 5 levels up...
            // - IIFE body.
            var outerNode = node.parent;
            if (!outerNode || !ts.isBlock(outerNode))
                return null;
            // - IIFE function expression.
            outerNode = outerNode.parent;
            if (!outerNode || (!ts.isFunctionExpression(outerNode) && !ts.isArrowFunction(outerNode))) {
                return null;
            }
            outerNode = outerNode.parent;
            // - Parenthesis inside IIFE.
            if (outerNode && ts.isParenthesizedExpression(outerNode))
                outerNode = outerNode.parent;
            // - IIFE call expression.
            if (!outerNode || !ts.isCallExpression(outerNode))
                return null;
            outerNode = outerNode.parent;
            // - Parenthesis around IIFE.
            if (outerNode && ts.isParenthesizedExpression(outerNode))
                outerNode = outerNode.parent;
            // - Outer variable declaration.
            if (!outerNode || !ts.isVariableDeclaration(outerNode))
                return null;
            // Finally, ensure that the variable declaration has a `name` identifier.
            return utils_1.hasNameIdentifier(outerNode) ? outerNode : null;
        }
        return null;
    }
    exports.getClassDeclarationFromInnerDeclaration = getClassDeclarationFromInnerDeclaration;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZXNtMjAxNV9ob3N0LmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vLi4vcGFja2FnZXMvY29tcGlsZXItY2xpL25nY2Mvc3JjL2hvc3QvZXNtMjAxNV9ob3N0LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7Ozs7Ozs7Ozs7Ozs7SUFFSCwrQkFBaUM7SUFDakMsMkVBQXNFO0lBRXRFLHlFQUFzVztJQUN0VyxxRUFBaUQ7SUFHakQsOERBQStGO0lBRS9GLDJFQUE0SjtJQUM1SixtRUFBeUM7SUFFNUIsUUFBQSxVQUFVLEdBQUcsWUFBMkIsQ0FBQztJQUN6QyxRQUFBLGVBQWUsR0FBRyxnQkFBK0IsQ0FBQztJQUNsRCxRQUFBLFdBQVcsR0FBRyxlQUE4QixDQUFDO0lBQzdDLFFBQUEsa0JBQWtCLEdBQUcsZ0JBQStCLENBQUM7SUFFbEU7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7O09BMEJHO0lBQ0g7UUFBMkMsaURBQXdCO1FBZ0RqRSwrQkFDYyxNQUFjLEVBQVksTUFBZSxFQUFZLEdBQWtCLEVBQ3ZFLEdBQThCO1lBQTlCLG9CQUFBLEVBQUEsVUFBOEI7WUFGNUMsWUFHRSxrQkFBTSxHQUFHLENBQUMsT0FBTyxDQUFDLGNBQWMsRUFBRSxDQUFDLFNBQ3BDO1lBSGEsWUFBTSxHQUFOLE1BQU0sQ0FBUTtZQUFZLFlBQU0sR0FBTixNQUFNLENBQVM7WUFBWSxTQUFHLEdBQUgsR0FBRyxDQUFlO1lBQ3ZFLFNBQUcsR0FBSCxHQUFHLENBQTJCO1lBakQ1Qzs7Ozs7O2VBTUc7WUFDTyw2QkFBdUIsR0FBNkMsSUFBSSxDQUFDO1lBQ25GOzs7Ozs7ZUFNRztZQUNPLDhCQUF3QixHQUE2QyxJQUFJLENBQUM7WUFFcEY7O2VBRUc7WUFDTyw2QkFBdUIsR0FBRyxJQUFJLEdBQUcsRUFBaUIsQ0FBQztZQUU3RDs7Ozs7Ozs7Ozs7Ozs7ZUFjRztZQUNPLDhCQUF3QixHQUFHLElBQUksR0FBRyxFQUFp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QUFTLElBQUksRUFBRSxDQUFDLHlCQUF5QixDQUFDLFNBQVMsQ0FBQztnQkFBRSxTQUFTLEdBQUcsU0FBUyxDQUFDLE1BQU0sQ0FBQztZQUV2RixnQ0FBZ0M7WUFDaEMsSUFBSSxDQUFDLFNBQVMsSUFBSSxDQUFDLEVBQUUsQ0FBQyxxQkFBcUIsQ0FBQyxTQUFTLENBQUM7Z0JBQUUsT0FBTyxJQUFJLENBQUM7WUFFcEUseUVBQXlFO1lBQ3pFLE9BQU8seUJBQWlCLENBQUMsU0FBUyxDQUFDLENBQUMsQ0FBQyxDQUFDLFNBQVMsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDO1NBQ3hEO1FBRUQsT0FBTyxJQUFJLENBQUM7SUFDZCxDQUFDO0lBbkNELDBGQW1DQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQgKiBhcyB0cyBmcm9tICd0eXBlc2NyaXB0JztcbmltcG9ydCB7YWJzb2x1dGVGcm9tU291cmNlRmlsZX0gZnJvbSAnLi4vLi4vLi4vc3JjL25ndHNjL2ZpbGVfc3lzdGVtJztcblxuaW1wb3J0IHtDbGFzc0RlY2xhcmF0aW9uLCBDbGFzc01lbWJlciwgQ2xhc3NNZW1iZXJLaW5kLCBDdG9yUGFyYW1ldGVyLCBEZWNsYXJhdGlvbiwgRGVjb3JhdG9yLCBFbnVtTWVtYmVyLCBpc0RlY29yYXRvcklkZW50aWZpZXIsIGlzTmFtZWRDbGFzc0RlY2xhcmF0aW9uLCBpc05hbWVkRnVuY3Rpb25EZWNsYXJhdGlvbiwgaXNOYW1lZFZhcmlhYmxlRGVjbGFyYXRpb24sIEtub3duRGVjbGFyYXRpb24sIHJlZmxlY3RPYmplY3RMaXRlcmFsLCBTcGVjaWFsRGVjbGFyYXRpb25LaW5kLCBUeXBlU2NyaXB0UmVmbGVjdGlvbkhvc3QsIFR5cGVWYWx1ZVJlZmVyZW5jZX0gZnJvbSAnLi4vLi4vLi4vc3JjL25ndHNjL3JlZmxlY3Rpb24nO1xuaW1wb3J0IHtpc1dpdGhpblBhY2thZ2V9IGZyb20gJy4uL2FuYWx5c2lzL3V0aWwnO1xuaW1wb3J0IHtMb2dnZXJ9IGZyb20gJy4uL2xvZ2dpbmcvbG9nZ2VyJztcbmltcG9ydCB7QnVuZGxlUHJvZ3JhbX0gZnJvbSAnLi4vcGFja2FnZXMvYnVuZGxlX3Byb2dyYW0nO1xuaW1wb3J0IHtmaW5kQWxsLCBnZXROYW1lVGV4dCwgaGFzTmFtZUlkZW50aWZpZXIsIGlzRGVmaW5lZCwgc3RyaXBEb2xsYXJTdWZmaXh9IGZyb20gJy4uL3V0aWxzJztcblxuaW1wb3J0IHtDbGFzc1N5bWJvbCwgaXNTd2l0Y2hhYmxlVmFyaWFibGVEZWNsYXJhdGlvbiwgTmdjY0NsYXNzU3ltYm9sLCBOZ2NjUmVmbGVjdGlvbkhvc3QsIFBSRV9SM19NQVJLRVIsIFN3aXRjaGFibGVWYXJpYWJsZURlY2xhcmF0aW9ufSBmcm9tICcuL25nY2NfaG9zdCc7XG5pbXBvcnQge3N0cmlwUGFyZW50aGVzZXN9IGZyb20gJy4vdXRpbHMnO1xuXG5leHBvcnQgY29uc3QgREVDT1JBVE9SUyA9ICdkZWNvcmF0b3JzJyBhcyB0cy5fX1N0cmluZztcbmV4cG9ydCBjb25zdCBQUk9QX0RFQ09SQVRPUlMgPSAncHJvcERlY29yYXRvcnMnIGFzIHRzLl9fU3RyaW5nO1xuZXhwb3J0IGNvbnN0IENPTlNUUlVDVE9SID0gJ19fY29uc3RydWN0b3InIGFzIHRzLl9fU3RyaW5nO1xuZXhwb3J0IGNvbnN0IENPTlNUUlVDVE9SX1BBUkFNUyA9ICdjdG9yUGFyYW1ldGVycycgYXMgdHMuX19TdHJpbmc7XG5cbi8qKlxuICogRXNtMjAxNSBwYWNrYWdlcyBjb250YWluIEVDTUFTY3JpcHQgMjAxNSBjbGFzc2VzLCBldGMuXG4gKiBEZWNvcmF0b3JzIGFyZSBkZWZpbmVkIHZpYSBzdGF0aWMgcHJvcGVydGllcyBvbiB0aGUgY2xhc3MuIEZvciBleGFtcGxlOlxuICpcbiAqIGBgYFxuICogY2xhc3MgU29tZURpcmVjdGl2ZSB7XG4gKiB9XG4gKiBTb21lRGlyZWN0aXZlLmRlY29yYXRvcnMgPSBbXG4gKiAgIHsgdHlwZTogRGlyZWN0aXZlLCBhcmdzOiBbeyBzZWxlY3RvcjogJ1tzb21lRGlyZWN0aXZlXScgfSxdIH1cbiAqIF07XG4gKiBTb21lRGlyZWN0aXZlLmN0b3JQYXJhbWV0ZXJzID0gKCkgPT4gW1xuICogICB7IHR5cGU6IFZpZXdDb250YWluZXJSZWYsIH0sXG4gKiAgIHsgdHlwZTogVGVtcGxhdGVSZWYsIH0sXG4gKiAgIHsgdHlwZTogdW5kZWZpbmVkLCBkZWNvcmF0b3JzOiBbeyB0eXBlOiBJbmplY3QsIGFyZ3M6IFtJTkpFQ1RFRF9UT0tFTixdIH0sXSB9LFxuICogXTtcbiAqIFNvbWVEaXJlY3RpdmUucHJvcERlY29yYXRvcnMgPSB7XG4gKiAgIFwiaW5wdXQxXCI6IFt7IHR5cGU6IElucHV0IH0sXSxcbiAqICAgXCJpbnB1dDJcIjogW3sgdHlwZTogSW5wdXQgfSxdLFxuICogfTtcbiAqIGBgYFxuICpcbiAqICogQ2xhc3NlcyBhcmUgZGVjb3JhdGVkIGlmIHRoZXkgaGF2ZSBhIHN0YXRpYyBwcm9wZXJ0eSBjYWxsZWQgYGRlY29yYXRvcnNgLlxuICogKiBNZW1iZXJzIGFyZSBkZWNvcmF0ZWQgaWYgdGhlcmUgaXMgYSBtYXRjaGluZyBrZXkgb24gYSBzdGF0aWMgcHJvcGVydHlcbiAqICAgY2FsbGVkIGBwcm9wRGVjb3JhdG9yc2AuXG4gKiAqIENvbnN0cnVjdG9yIHBhcmFtZXRlcnMgZGVjb3JhdG9ycyBhcmUgZm91bmQgb24gYW4gb2JqZWN0IHJldHVybmVkIGZyb21cbiAqICAgYSBzdGF0aWMgbWV0aG9kIGNhbGxlZCBgY3RvclBhcmFtZXRlcnNgLlxuICovXG5leHBvcnQgY2xhc3MgRXNtMjAxNVJlZmxlY3Rpb25Ib3N0IGV4dGVuZHMgVHlwZVNjcmlwdFJlZmxlY3Rpb25Ib3N0IGltcGxlbWVudHMgTmdjY1JlZmxlY3Rpb25Ib3N0IHtcbiAgLyoqXG4gICAqIEEgbWFwcGluZyBmcm9tIHNvdXJjZSBkZWNsYXJhdGlvbnMgdG8gdHlwaW5ncyBkZWNsYXJhdGlvbnMsIHdoaWNoIGFyZSBib3RoIHB1YmxpY2x5IGV4cG9ydGVkLlxuICAgKlxuICAgKiBUaGVyZSBzaG91bGQgYmUgb25lIGVudHJ5IGZvciBldmVyeSBwdWJsaWMgZXhwb3J0IHZpc2libGUgZnJvbSB0aGUgcm9vdCBmaWxlIG9mIHRoZSBzb3VyY2VcbiAgICogdHJlZS4gTm90ZSB0aGF0IGJ5IGRlZmluaXRpb24gdGhlIGtleSBhbmQgdmFsdWUgZGVjbGFyYXRpb25zIHdpbGwgbm90IGJlIGluIHRoZSBzYW1lIFRTXG4gICAqIHByb2dyYW0uXG4gICAqL1xuICBwcm90ZWN0ZWQgcHVibGljRHRzRGVjbGFyYXRpb25NYXA6IE1hcDx0cy5EZWNsYXJhdGlvbiwgdHMuRGVjbGFyYXRpb24+fG51bGwgPSBudWxsO1xuICAvKipcbiAgICogQSBtYXBwaW5nIGZyb20gc291cmNlIGRlY2xhcmF0aW9ucyB0byB0eXBpbmdzIGRlY2xhcmF0aW9ucywgd2hpY2ggYXJlIG5vdCBwdWJsaWNseSBleHBvcnRlZC5cbiAgICpcbiAgICogVGhpcyBtYXBwaW5nIGlzIGEgYmVzdCBndWVzcyBiZXR3ZWVuIGRlY2xhcmF0aW9ucyB0aGF0IGhhcHBlbiB0byBiZSBleHBvcnRlZCBmcm9tIHRoZWlyIGZpbGUgYnlcbiAgICogdGhlIHNhbWUgbmFtZSBpbiBib3RoIHRoZSBzb3VyY2UgYW5kIHRoZSBkdHMgZmlsZS4gTm90ZSB0aGF0IGJ5IGRlZmluaXRpb24gdGhlIGtleSBhbmQgdmFsdWVcbiAgICogZGVjbGFyYXRpb25zIHdpbGwgbm90IGJlIGluIHRoZSBzYW1lIFRTIHByb2dyYW0uXG4gICAqL1xuICBwcm90ZWN0ZWQgcHJpdmF0ZUR0c0RlY2xhcmF0aW9uTWFwOiBNYXA8dHMuRGVjbGFyYXRpb24sIHRzLkRlY2xhcmF0aW9uPnxudWxsID0gbnVsbDtcblxuICAvKipcbiAgICogVGhlIHNldCBvZiBzb3VyY2UgZmlsZXMgdGhhdCBoYXZlIGFscmVhZHkgYmVlbiBwcmVwcm9jZXNzZWQuXG4gICAqL1xuICBwcm90ZWN0ZWQgcHJlcHJvY2Vzc2VkU291cmNlRmlsZXMgPSBuZXcgU2V0PHRzLlNvdXJjZUZpbGU+KCk7XG5cbiAgLyoqXG4gICAqIEluIEVTMjAxNSwgY2xhc3MgZGVjbGFyYXRpb25zIG1heSBoYXZlIGJlZW4gZG93bi1sZXZlbGVkIGludG8gdmFyaWFibGUgZGVjbGFyYXRpb25zLFxuICAgKiBpbml0aWFsaXplZCB1c2luZyBhIGNsYXNzIGV4cHJlc3Npb24uIEluIGNlcnRhaW4gc2NlbmFyaW9zLCBhbiBhZGRpdGlvbmFsIHZhcmlhYmxlXG4gICAqIGlzIGludHJvZHVjZWQgdGhhdCByZXByZXNlbnRzIHRoZSBjbGFzcyBzbyB0aGF0IHJlc3VsdHMgaW4gY29kZSBzdWNoIGFzOlxuICAgKlxuICAgKiBgYGBcbiAgICogbGV0IE15Q2xhc3NfMTsgbGV0IE15Q2xhc3MgPSBNeUNsYXNzXzEgPSBjbGFzcyBNeUNsYXNzIHt9O1xuICAgKiBgYGBcbiAgICpcbiAgICogVGhpcyBtYXAgdHJhY2tzIHRob3NlIGFsaWFzZWQgdmFyaWFibGVzIHRvIHRoZWlyIG9yaWdpbmFsIGlkZW50aWZpZXIsIGkuZS4gdGhlIGtleVxuICAgKiBjb3JyZXNwb25kcyB3aXRoIHRoZSBkZWNsYXJhdGlvbiBvZiBgTXlDbGFzc18xYCBhbmQgaXRzIHZhbHVlIGJlY29tZXMgdGhlIGBNeUNsYXNzYCBpZGVudGlmaWVyXG4gICAqIG9mIHRoZSB2YXJpYWJsZSBkZWNsYXJhdGlvbi5cbiAgICpcbiAgICogVGhpcyBtYXAgaXMgcG9wdWxhdGVkIGR1cmluZyB0aGUgcHJlcHJvY2Vzc2luZyBvZiBlYWNoIHNvdXJjZSBmaWxlLlxuICAgKi9cbiAgcHJvdGVjdGVkIGFsaWFzZWRDbGFzc0RlY2xhcmF0aW9ucyA9IG5ldyBNYXA8dHMuRGVjbGFyYXRpb24sIHRzLklkZW50aWZpZXI+KCk7XG5cbiAgLyoqXG4gICAqIENhY2hlcyB0aGUgaW5mb3JtYXRpb24gb2YgdGhlIGRlY29yYXRvcnMgb24gYSBjbGFzcywgYXMgdGhlIHdvcmsgaW52b2x2ZWQgd2l0aCBleHRyYWN0aW5nXG4gICAqIGRlY29yYXRvcnMgaXMgY29tcGxleCBhbmQgZnJlcXVlbnRseSB1c2VkLlxuICAgKlxuICAgKiBUaGlzIG1hcCBpcyBsYXppbHkgcG9wdWxhdGVkIGR1cmluZyB0aGUgZmlyc3QgY2FsbCB0byBgYWNxdWlyZURlY29yYXRvckluZm9gIGZvciBhIGdpdmVuIGNsYXNzLlxuICAgKi9cbiAgcHJvdGVjdGVkIGRlY29yYXRvckNhY2hlID0gbmV3IE1hcDxDbGFzc0RlY2xhcmF0aW9uLCBEZWNvcmF0b3JJbmZvPigpO1xuXG4gIGNvbnN0cnVjdG9yKFxuICAgICAgcHJvdGVjdGVkIGxvZ2dlcjogTG9nZ2VyLCBwcm90ZWN0ZWQgaXNDb3JlOiBib29sZWFuLCBwcm90ZWN0ZWQgc3JjOiBCdW5kbGVQcm9ncmFtLFxuICAgICAgcHJvdGVjdGVkIGR0czogQnVuZGxlUHJvZ3JhbXxudWxsID0gbnVsbCkge1xuICAgIHN1cGVyKHNyYy5wcm9ncmFtLmdldFR5cGVDaGVja2VyKCkpO1xuICB9XG5cbiAgLyoqXG4gICAqIEZpbmQgYSBzeW1ib2wgZm9yIGEgbm9kZSB0aGF0IHdlIHRoaW5rIGlzIGEgY2xhc3MuXG4gICAqIENsYXNzZXMgc2hvdWxkIGhhdmUgYSBgbmFtZWAgaWRlbnRpZmllciwgYmVjYXVzZSB0aGV5IG1heSBuZWVkIHRvIGJlIHJlZmVyZW5jZWQgaW4gb3RoZXIgcGFydHNcbiAgICogb2YgdGhlIHByb2dyYW0uXG4gICAqXG4gICAqIEluIEVTMjAxNSwgYSBjbGFzcyBtYXkgYmUgZGVjbGFyZWQgdXNpbmcgYSB2YXJpYWJsZSBkZWNsYXJhdGlvbiBvZiB0aGUgZm9sbG93aW5nIHN0cnVjdHVyZXM6XG4gICAqXG4gICAqIGBgYFxuICAgKiB2YXIgTXlDbGFzcyA9IE15Q2xhc3NfMSA9IGNsYXNzIE15Q2xhc3Mge307XG4gICAqIGBgYFxuICAgKlxuICAgKiBvclxuICAgKlxuICAgKiBgYGBcbiAgICogdmFyIE15Q2xhc3MgPSBNeUNsYXNzXzEgPSAoKCkgPT4geyBjbGFzcyBNeUNsYXNzIHt9IC4uLiByZXR1cm4gTXlDbGFzczsgfSkoKVxuICAgKiBgYGBcbiAgICpcbiAgICogSGVyZSwgdGhlIGludGVybWVkaWF0ZSBgTXlDbGFzc18xYCBhc3NpZ25tZW50IGlzIG9wdGlvbmFsLiBJbiB0aGUgYWJvdmUgZXhhbXBsZSwgdGhlXG4gICAqIGBjbGFzcyBNeUNsYXNzIHt9YCBub2RlIGlzIHJldHVybmVkIGFzIGRlY2xhcmF0aW9uIG9mIGBNeUNsYXNzYC5cbiAgICpcbiAgICogQHBhcmFtIGRlY2xhcmF0aW9uIHRoZSBkZWNsYXJhdGlvbiBub2RlIHdob3NlIHN5bWJvbCB3ZSBhcmUgZmluZGluZy5cbiAgICogQHJldHVybnMgdGhlIHN5bWJvbCBmb3IgdGhlIG5vZGUgb3IgYHVuZGVmaW5lZGAgaWYgaXQgaXMgbm90IGEgXCJjbGFzc1wiIG9yIGhhcyBubyBzeW1ib2wuXG4gICAqL1xuICBnZXRDbGFzc1N5bWJvbChkZWNsYXJhdGlvbjogdHMuTm9kZSk6IE5nY2NDbGFzc1N5bWJvbHx1bmRlZmluZWQge1xuICAgIGNvbnN0IHN5bWJvbCA9IHRoaXMuZ2V0Q2xhc3NTeW1ib2xGcm9tT3V0ZXJEZWNsYXJhdGlvbihkZWNsYXJhdGlvbik7XG4gICAgaWYgKHN5bWJvbCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICByZXR1cm4gc3ltYm9sO1xuICAgIH1cblxuICAgIGlmIChkZWNsYXJhdGlvbi5wYXJlbnQgIT09IHVuZGVmaW5lZCAmJiBpc05hbWVkVmFyaWFibGVEZWNsYXJhdGlvbihkZWNsYXJhdGlvbi5wYXJlbnQpKSB7XG4gICAgICBjb25zdCB2YXJpYWJsZVZhbHVlID0gdGhpcy5nZXRWYXJpYWJsZVZhbHVlKGRlY2xhcmF0aW9uLnBhcmVudCk7XG4gICAgICBpZiAodmFyaWFibGVWYWx1ZSAhPT0gbnVsbCkge1xuICAgICAgICBkZWNsYXJhdGlvbiA9IHZhcmlhYmxlVmFsdWU7XG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIHRoaXMuZ2V0Q2xhc3NTeW1ib2xGcm9tSW5uZXJEZWNsYXJhdGlvbihkZWNsYXJhdGlvbik7XG4gIH1cblxuICAvKipcbiAgICogRXhhbWluZSBhIGRlY2xhcmF0aW9uIChmb3IgZXhhbXBsZSwgb2YgYSBjbGFzcyBvciBmdW5jdGlvbikgYW5kIHJldHVybiBtZXRhZGF0YSBhYm91dCBhbnlcbiAgICogZGVjb3JhdG9ycyBwcmVzZW50IG9uIHRoZSBkZWNsYXJhdGlvbi5cbiAgICpcbiAgICogQHBhcmFtIGRlY2xhcmF0aW9uIGEgVHlwZVNjcmlwdCBgdHMuRGVjbGFyYXRpb25gIG5vZGUgcmVwcmVzZW50aW5nIHRoZSBjbGFzcyBvciBmdW5jdGlvbiBvdmVyXG4gICAqIHdoaWNoIHRvIHJlZmxlY3QuIEZvciBleGFtcGxlLCBpZiB0aGUgaW50ZW50IGlzIHRvIHJlZmxlY3QgdGhlIGRlY29yYXRvcnMgb2YgYSBjbGFzcyBhbmQgdGhlXG4gICAqIHNvdXJjZSBpcyBpbiBFUzYgZm9ybWF0LCB0aGlzIHdpbGwgYmUgYSBgdHMuQ2xhc3NEZWNsYXJhdGlvbmAgbm9kZS4gSWYgdGhlIHNvdXJjZSBpcyBpbiBFUzVcbiAgICogZm9ybWF0LCB0aGlzIG1pZ2h0IGJlIGEgYHRzLlZhcmlhYmxlRGVjbGFyYXRpb25gIGFzIGNsYXNzZXMgaW4gRVM1IGFyZSByZXByZXNlbnRlZCBhcyB0aGVcbiAgICogcmVzdWx0IG9mIGFuIElJRkUgZXhlY3V0aW9uLlxuICAgKlxuICAgKiBAcmV0dXJucyBhbiBhcnJheSBvZiBgRGVjb3JhdG9yYCBtZXRhZGF0YSBpZiBkZWNvcmF0b3JzIGFyZSBwcmVzZW50IG9uIHRoZSBkZWNsYXJhdGlvbiwgb3JcbiAgICogYG51bGxgIGlmIGVpdGhlciBubyBkZWNvcmF0b3JzIHdlcmUgcHJlc2VudCBvciBpZiB0aGUgZGVjbGFyYXRpb24gaXMgbm90IG9mIGEgZGVjb3JhdGFibGUgdHlwZS5cbiAgICovXG4gIGdldERlY29yYXRvcnNPZkRlY2xhcmF0aW9uKGRlY2xhcmF0aW9uOiB0cy5EZWNsYXJhdGlvbik6IERlY29yYXRvcltdfG51bGwge1xuICAgIGNvbnN0IHN5bWJvbCA9IHRoaXMuZ2V0Q2xhc3NTeW1ib2woZGVjbGFyYXRpb24pO1xuICAgIGlmICghc3ltYm9sKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG4gICAgcmV0dXJuIHRoaXMuZ2V0RGVjb3JhdG9yc09mU3ltYm9sKHN5bWJvbCk7XG4gIH1cblxuICAvKipcbiAgICogRXhhbWluZSBhIGRlY2xhcmF0aW9uIHdoaWNoIHNob3VsZCBiZSBvZiBhIGNsYXNzLCBhbmQgcmV0dXJuIG1ldGFkYXRhIGFib3V0IHRoZSBtZW1iZXJzIG9mIHRoZVxuICAgKiBjbGFzcy5cbiAgICpcbiAgICogQHBhcmFtIGNsYXp6IGEgYENsYXNzRGVjbGFyYXRpb25gIHJlcHJlc2VudGluZyB0aGUgY2xhc3Mgb3ZlciB3aGljaCB0byByZWZsZWN0LlxuICAgKlxuICAgKiBAcmV0dXJucyBhbiBhcnJheSBvZiBgQ2xhc3NNZW1iZXJgIG1ldGFkYXRhIHJlcHJlc2VudGluZyB0aGUgbWVtYmVycyBvZiB0aGUgY2xhc3MuXG4gICAqXG4gICAqIEB0aHJvd3MgaWYgYGRlY2xhcmF0aW9uYCBkb2VzIG5vdCByZXNvbHZlIHRvIGEgY2xhc3MgZGVjbGFyYXRpb24uXG4gICAqL1xuICBnZXRNZW1iZXJzT2ZDbGFzcyhjbGF6ejogQ2xhc3NEZWNsYXJhdGlvbik6IENsYXNzTWVtYmVyW10ge1xuICAgIGNvbnN0IGNsYXNzU3ltYm9sID0gdGhpcy5nZXRDbGFzc1N5bWJvbChjbGF6eik7XG4gICAgaWYgKCFjbGFzc1N5bWJvbCkge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKGBBdHRlbXB0ZWQgdG8gZ2V0IG1lbWJlcnMgb2YgYSBub24tY2xhc3M6IFwiJHtjbGF6ei5nZXRUZXh0KCl9XCJgKTtcbiAgICB9XG5cbiAgICByZXR1cm4gdGhpcy5nZXRNZW1iZXJzT2ZTeW1ib2woY2xhc3NTeW1ib2wpO1xuICB9XG5cbiAgLyoqXG4gICAqIFJlZmxlY3Qgb3ZlciB0aGUgY29uc3RydWN0b3Igb2YgYSBjbGFzcyBhbmQgcmV0dXJuIG1ldGFkYXRhIGFib3V0IGl0cyBwYXJhbWV0ZXJzLlxuICAgKlxuICAgKiBUaGlzIG1ldGhvZCBvbmx5IGxvb2tzIGF0IHRoZSBjb25zdHJ1Y3RvciBvZiBhIGNsYXNzIGRpcmVjdGx5IGFuZCBub3QgYXQgYW55IGluaGVyaXRlZFxuICAgKiBjb25zdHJ1Y3RvcnMuXG4gICAqXG4gICAqIEBwYXJhbSBjbGF6eiBhIGBDbGFzc0RlY2xhcmF0aW9uYCByZXByZXNlbnRpbmcgdGhlIGNsYXNzIG92ZXIgd2hpY2ggdG8gcmVmbGVjdC5cbiAgICpcbiAgICogQHJldHVybnMgYW4gYXJyYXkgb2YgYFBhcmFtZXRlcmAgbWV0YWRhdGEgcmVwcmVzZW50aW5nIHRoZSBwYXJhbWV0ZXJzIG9mIHRoZSBjb25zdHJ1Y3RvciwgaWZcbiAgICogYSBjb25zdHJ1Y3RvciBleGlzdHMuIElmIHRoZSBjb25zdHJ1Y3RvciBleGlzdHMgYW5kIGhhcyAwIHBhcmFtZXRlcnMsIHRoaXMgYXJyYXkgd2lsbCBiZSBlbXB0eS5cbiAgICogSWYgdGhlIGNsYXNzIGhhcyBubyBjb25zdHJ1Y3RvciwgdGhpcyBtZXRob2QgcmV0dXJucyBgbnVsbGAuXG4gICAqXG4gICAqIEB0aHJvd3MgaWYgYGRlY2xhcmF0aW9uYCBkb2VzIG5vdCByZXNvbHZlIHRvIGEgY2xhc3MgZGVjbGFyYXRpb24uXG4gICAqL1xuICBnZXRDb25zdHJ1Y3RvclBhcmFtZXRlcnMoY2xheno6IENsYXNzRGVjbGFyYXRpb24pOiBDdG9yUGFyYW1ldGVyW118bnVsbCB7XG4gICAgY29uc3QgY2xhc3NTeW1ib2wgPSB0aGlzLmdldENsYXNzU3ltYm9sKGNsYXp6KTtcbiAgICBpZiAoIWNsYXNzU3ltYm9sKSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoXG4gICAgICAgICAgYEF0dGVtcHRlZCB0byBnZXQgY29uc3RydWN0b3IgcGFyYW1ldGVycyBvZiBhIG5vbi1jbGFzczogXCIke2NsYXp6LmdldFRleHQoKX1cImApO1xuICAgIH1cbiAgICBjb25zdCBwYXJhbWV0ZXJOb2RlcyA9IHRoaXMuZ2V0Q29uc3RydWN0b3JQYXJhbWV0ZXJEZWNsYXJhdGlvbnMoY2xhc3NTeW1ib2wpO1xuICAgIGlmIChwYXJhbWV0ZXJOb2Rlcykge1xuICAgICAgcmV0dXJuIHRoaXMuZ2V0Q29uc3RydWN0b3JQYXJhbUluZm8oY2xhc3NTeW1ib2wsIHBhcmFtZXRlck5vZGVzKTtcbiAgICB9XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuICBnZXRCYXNlQ2xhc3NFeHByZXNzaW9uKGNsYXp6OiBDbGFzc0RlY2xhcmF0aW9uKTogdHMuRXhwcmVzc2lvbnxudWxsIHtcbiAgICAvLyBGaXJzdCB0cnkgZ2V0dGluZyB0aGUgYmFzZSBjbGFzcyBmcm9tIGFuIEVTMjAxNSBjbGFzcyBkZWNsYXJhdGlvblxuICAgIGNvbnN0IHN1cGVyQmFzZUNsYXNzSWRlbnRpZmllciA9IHN1cGVyLmdldEJhc2VDbGFzc0V4cHJlc3Npb24oY2xhenopO1xuICAgIGlmIChzdXBlckJhc2VDbGFzc0lkZW50aWZpZXIpIHtcbiAgICAgIHJldHVybiBzdXBlckJhc2VDbGFzc0lkZW50aWZpZXI7XG4gICAgfVxuXG4gICAgLy8gVGhhdCBkaWRuJ3Qgd29yayBzbyBub3cgdHJ5IGdldHRpbmcgaXQgZnJvbSB0aGUgXCJpbm5lclwiIGRlY2xhcmF0aW9uLlxuICAgIGNvbnN0IGNsYXNzU3ltYm9sID0gdGhpcy5nZXRDbGFzc1N5bWJvbChjbGF6eik7XG4gICAgaWYgKGNsYXNzU3ltYm9sID09PSB1bmRlZmluZWQgfHxcbiAgICAgICAgIWlzTmFtZWREZWNsYXJhdGlvbihjbGFzc1N5bWJvbC5pbXBsZW1lbnRhdGlvbi52YWx1ZURlY2xhcmF0aW9uKSkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIHJldHVybiBzdXBlci5nZXRCYXNlQ2xhc3NFeHByZXNzaW9uKGNsYXNzU3ltYm9sLmltcGxlbWVudGF0aW9uLnZhbHVlRGVjbGFyYXRpb24pO1xuICB9XG5cbiAgZ2V0SW50ZXJuYWxOYW1lT2ZDbGFzcyhjbGF6ejogQ2xhc3NEZWNsYXJhdGlvbik6IHRzLklkZW50aWZpZXIge1xuICAgIGNvbnN0IGNsYXNzU3ltYm9sID0gdGhpcy5nZXRDbGFzc1N5bWJvbChjbGF6eik7XG4gICAgaWYgKGNsYXNzU3ltYm9sID09PSB1bmRlZmluZWQpIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihgZ2V0SW50ZXJuYWxOYW1lT2ZDbGFzcygpIGNhbGxlZCBvbiBhIG5vbi1jbGFzczogZXhwZWN0ZWQgJHtcbiAgICAgICAgICBjbGF6ei5uYW1lLnRleHR9IHRvIGJlIGEgY2xhc3MgZGVjbGFyYXRpb24uYCk7XG4gICAgfVxuICAgIHJldHVybiB0aGlzLmdldE5hbWVGcm9tQ2xhc3NTeW1ib2xEZWNsYXJhdGlvbihcbiAgICAgICAgY2xhc3NTeW1ib2wsIGNsYXNzU3ltYm9sLmltcGxlbWVudGF0aW9uLnZhbHVlRGVjbGFyYXRpb24pO1xuICB9XG5cbiAgZ2V0QWRqYWNlbnROYW1lT2ZDbGFzcyhjbGF6ejogQ2xhc3NEZWNsYXJhdGlvbik6IHRzLklkZW50aWZpZXIge1xuICAgIGNvbnN0IGNsYXNzU3ltYm9sID0gdGhpcy5nZXRDbGFzc1N5bWJvbChjbGF6eik7XG4gICAgaWYgKGNsYXNzU3ltYm9sID09PSB1bmRlZmluZWQpIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihgZ2V0QWRqYWNlbnROYW1lT2ZDbGFzcygpIGNhbGxlZCBvbiBhIG5vbi1jbGFzczogZXhwZWN0ZWQgJHtcbiAgICAgICAgICBjbGF6ei5uYW1lLnRleHR9IHRvIGJlIGEgY2xhc3MgZGVjbGFyYXRpb24uYCk7XG4gICAgfVxuXG4gICAgaWYgKGNsYXNzU3ltYm9sLmFkamFjZW50ICE9PSB1bmRlZmluZWQpIHtcbiAgICAgIHJldHVybiB0aGlzLmdldE5hbWVGcm9tQ2xhc3NTeW1ib2xEZWNsYXJhdGlvbihcbiAgICAgICAgICBjbGFzc1N5bWJvbCwgY2xhc3NTeW1ib2wuYWRqYWNlbnQudmFsdWVEZWNsYXJhdGlvbik7XG4gICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiB0aGlzLmdldE5hbWVGcm9tQ2xhc3NTeW1ib2xEZWNsYXJhdGlvbihcbiAgICAgICAgICBjbGFzc1N5bWJvbCwgY2xhc3NTeW1ib2wuaW1wbGVtZW50YXRpb24udmFsdWVEZWNsYXJhdGlvbik7XG4gICAgfVxuICB9XG5cbiAgcHJpdmF0ZSBnZXROYW1lRnJvbUNsYXNzU3ltYm9sRGVjbGFyYXRpb24oXG4gICAgICBjbGFzc1N5bWJvbDogTmdjY0NsYXNzU3ltYm9sLCBkZWNsYXJhdGlvbjogdHMuRGVjbGFyYXRpb24pOiB0cy5JZGVudGlmaWVyIHtcbiAgICBpZiAoZGVjbGFyYXRpb24gPT09IHVuZGVmaW5lZCkge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKFxuICAgICAgICAgIGBnZXRJbnRlcm5hbE5hbWVPZkNsYXNzKCkgY2FsbGVkIG9uIGEgY2xhc3Mgd2l0aCBhbiB1bmRlZmluZWQgaW50ZXJuYWwgZGVjbGFyYXRpb24uIEV4dGVybmFsIGNsYXNzIG5hbWU6ICR7XG4gICAgICAgICAgICAgIGNsYXNzU3ltYm9sLm5hbWV9OyBpbnRlcm5hbCBjbGFzcyBuYW1lOiAke2NsYXNzU3ltYm9sLmltcGxlbWVudGF0aW9uLm5hbWV9LmApO1xuICAgIH1cbiAgICBpZiAoIWlzTmFtZWREZWNsYXJhdGlvbihkZWNsYXJhdGlvbikpIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihcbiAgICAgICAgICBgZ2V0SW50ZXJuYWxOYW1lT2ZDbGFzcygpIGNhbGxlZCBvbiBhIGNsYXNzIHdpdGggYW4gYW5vbnltb3VzIGlubmVyIGRlY2xhcmF0aW9uOiBleHBlY3RlZCBhIG5hbWUgb246XFxuJHtcbiAgICAgICAgICAgICAgZGVjbGFyYXRpb24uZ2V0VGV4dCgpfWApO1xuICAgIH1cbiAgICByZXR1cm4gZGVjbGFyYXRpb24ubmFtZTtcbiAgfVxuXG4gIC8qKlxuICAgKiBDaGVjayB3aGV0aGVyIHRoZSBnaXZlbiBub2RlIGFjdHVhbGx5IHJlcHJlc2VudHMgYSBjbGFzcy5cbiAgICovXG4gIGlzQ2xhc3Mobm9kZTogdHMuTm9kZSk6IG5vZGUgaXMgQ2xhc3NEZWNsYXJhdGlvbiB7XG4gICAgcmV0dXJuIHN1cGVyLmlzQ2xhc3Mobm9kZSkgfHwgdGhpcy5nZXRDbGFzc1N5bWJvbChub2RlKSAhPT0gdW5kZWZpbmVkO1xuICB9XG5cbiAgLyoqXG4gICAqIFRyYWNlIGFuIGlkZW50aWZpZXIgdG8gaXRzIGRlY2xhcmF0aW9uLCBpZiBwb3NzaWJsZS5cbiAgICpcbiAgICogVGhpcyBtZXRob2QgYXR0ZW1wdHMgdG8gcmVzb2x2ZSB0aGUgZGVjbGFyYXRpb24gb2YgdGhlIGdpdmVuIGlkZW50aWZpZXIsIHRyYWNpbmcgYmFjayB0aHJvdWdoXG4gICAqIGltcG9ydHMgYW5kIHJlLWV4cG9ydHMgdW50aWwgdGhlIG9yaWdpbmFsIGRlY2xhcmF0aW9uIHN0YXRlbWVudCBpcyBmb3VuZC4gQSBgRGVjbGFyYXRpb25gXG4gICAqIG9iamVjdCBpcyByZXR1cm5lZCBpZiB0aGUgb3JpZ2luYWwgZGVjbGFyYXRpb24gaXMgZm91bmQsIG9yIGBudWxsYCBpcyByZXR1cm5lZCBvdGhlcndpc2UuXG4gICAqXG4gICAqIEluIEVTMjAxNSwgd2UgbmVlZCB0byBhY2NvdW50IGZvciBpZGVudGlmaWVycyB0aGF0IHJlZmVyIHRvIGFsaWFzZWQgY2xhc3MgZGVjbGFyYXRpb25zIHN1Y2ggYXNcbiAgICogYE15Q2xhc3NfMWAuIFNpbmNlIHN1Y2ggZGVjbGFyYXRpb25zIGFyZSBvbmx5IGF2YWlsYWJsZSB3aXRoaW4gdGhlIG1vZHVsZSBpdHNlbGYsIHdlIG5lZWQgdG9cbiAgICogZmluZCB0aGUgb3JpZ2luYWwgY2xhc3MgZGVjbGFyYXRpb24sIGUuZy4gYE15Q2xhc3NgLCB0aGF0IGlzIGFzc29jaWF0ZWQgd2l0aCB0aGUgYWxpYXNlZCBvbmUuXG4gICAqXG4gICAqIEBwYXJhbSBpZCBhIFR5cGVTY3JpcHQgYHRzLklkZW50aWZpZXJgIHRvIHRyYWNlIGJhY2sgdG8gYSBkZWNsYXJhdGlvbi5cbiAgICpcbiAgICogQHJldHVybnMgbWV0YWRhdGEgYWJvdXQgdGhlIGBEZWNsYXJhdGlvbmAgaWYgdGhlIG9yaWdpbmFsIGRlY2xhcmF0aW9uIGlzIGZvdW5kLCBvciBgbnVsbGBcbiAgICogb3RoZXJ3aXNlLlxuICAgKi9cbiAgZ2V0RGVjbGFyYXRpb25PZklkZW50aWZpZXIoaWQ6IHRzLklkZW50aWZpZXIpOiBEZWNsYXJhdGlvbnxudWxsIHtcbiAgICBjb25zdCBzdXBlckRlY2xhcmF0aW9uID0gc3VwZXIuZ2V0RGVjbGFyYXRpb25PZklkZW50aWZpZXIoaWQpO1xuXG4gICAgLy8gSWYgbm8gZGVjbGFyYXRpb24gd2FzIGZvdW5kIG9yIGl0J3MgYW4gaW5saW5lIGRlY2xhcmF0aW9uLCByZXR1cm4gYXMgaXMuXG4gICAgaWYgKHN1cGVyRGVjbGFyYXRpb24gPT09IG51bGwgfHwgc3VwZXJEZWNsYXJhdGlvbi5ub2RlID09PSBudWxsKSB7XG4gICAgICByZXR1cm4gc3VwZXJEZWNsYXJhdGlvbjtcbiAgICB9XG5cbiAgICAvLyBJZiB0aGUgZGVjbGFyYXRpb24gYWxyZWFkeSBoYXMgdHJhaXRzIGFzc2lnbmVkIHRvIGl0LCByZXR1cm4gYXMgaXMuXG4gICAgaWYgKHN1cGVyRGVjbGFyYXRpb24ua25vd24gIT09IG51bGwgfHwgc3VwZXJEZWNsYXJhdGlvbi5pZGVudGl0eSAhPT0gbnVsbCkge1xuICAgICAgcmV0dXJuIHN1cGVyRGVjbGFyYXRpb247XG4gICAgfVxuICAgIGxldCBkZWNsYXJhdGlvbk5vZGU6IHRzLk5vZGUgPSBzdXBlckRlY2xhcmF0aW9uLm5vZGU7XG4gICAgaWYgKGlzTmFtZWRWYXJpYWJsZURlY2xhcmF0aW9uKHN1cGVyRGVjbGFyYXRpb24ubm9kZSkgJiYgIWlzVG9wTGV2ZWwoc3VwZXJEZWNsYXJhdGlvbi5ub2RlKSkge1xuICAgICAgY29uc3QgdmFyaWFibGVWYWx1ZSA9IHRoaXMuZ2V0VmFyaWFibGVWYWx1ZShzdXBlckRlY2xhcmF0aW9uLm5vZGUpO1xuICAgICAgaWYgKHZhcmlhYmxlVmFsdWUgIT09IG51bGwgJiYgdHMuaXNDbGFzc0V4cHJlc3Npb24odmFyaWFibGVWYWx1ZSkpIHtcbiAgICAgICAgZGVjbGFyYXRpb25Ob2RlID0gZ2V0Q29udGFpbmluZ1N0YXRlbWVudCh2YXJpYWJsZVZhbHVlKTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICBjb25zdCBvdXRlckNsYXNzTm9kZSA9IGdldENsYXNzRGVjbGFyYXRpb25Gcm9tSW5uZXJEZWNsYXJhdGlvbihkZWNsYXJhdGlvbk5vZGUpO1xuICAgIGNvbnN0IGRlY2xhcmF0aW9uID0gb3V0ZXJDbGFzc05vZGUgIT09IG51bGwgP1xuICAgICAgICB0aGlzLmdldERlY2xhcmF0aW9uT2ZJZGVudGlmaWVyKG91dGVyQ2xhc3NOb2RlLm5hbWUpIDpcbiAgICAgICAgc3VwZXJEZWNsYXJhdGlvbjtcbiAgICBpZiAoZGVjbGFyYXRpb24gPT09IG51bGwgfHwgZGVjbGFyYXRpb24ubm9kZSA9PT0gbnVsbCB8fCBkZWNsYXJhdGlvbi5rbm93biAhPT0gbnVsbCkge1xuICAgICAgcmV0dXJuIGRlY2xhcmF0aW9uO1xuICAgIH1cblxuICAgIC8vIFRoZSBpZGVudGlmaWVyIG1heSBoYXZlIGJlZW4gb2YgYW4gYWRkaXRpb25hbCBjbGFzcyBhc3NpZ25tZW50IHN1Y2ggYXMgYE15Q2xhc3NfMWAgdGhhdCB3YXNcbiAgICAvLyBwcmVzZW50IGFzIGFsaWFzIGZvciBgTXlDbGFzc2AuIElmIHNvLCByZXNvbHZlIHN1Y2ggYWxpYXNlcyB0byB0aGVpciBvcmlnaW5hbCBkZWNsYXJhdGlvbi5cbiAgICBjb25zdCBhbGlhc2VkSWRlbnRpZmllciA9IHRoaXMucmVzb2x2ZUFsaWFzZWRDbGFzc0lkZW50aWZpZXIoZGVjbGFyYXRpb24ubm9kZSk7XG4gICAgaWYgKGFsaWFzZWRJZGVudGlmaWVyICE9PSBudWxsKSB7XG4gICAgICByZXR1cm4gdGhpcy5nZXREZWNsYXJhdGlvbk9mSWRlbnRpZmllcihhbGlhc2VkSWRlbnRpZmllcik7XG4gICAgfVxuXG4gICAgLy8gVmFyaWFibGUgZGVjbGFyYXRpb25zIG1heSByZXByZXNlbnQgYW4gZW51bSBkZWNsYXJhdGlvbiwgc28gYXR0ZW1wdCB0byByZXNvbHZlIGl0cyBtZW1iZXJzLlxuICAgIGlmICh0cy5pc1ZhcmlhYmxlRGVjbGFyYXRpb24oZGVjbGFyYXRpb24ubm9kZSkpIHtcbiAgICAgIGNvbnN0IGVudW1NZW1iZXJzID0gdGhpcy5yZXNvbHZlRW51bU1lbWJlcnMoZGVjbGFyYXRpb24ubm9kZSk7XG4gICAgICBpZiAoZW51bU1lbWJlcnMgIT09IG51bGwpIHtcbiAgICAgICAgZGVjbGFyYXRpb24uaWRlbnRpdHkgPSB7a2luZDogU3BlY2lhbERlY2xhcmF0aW9uS2luZC5Eb3dubGV2ZWxlZEVudW0sIGVudW1NZW1iZXJzfTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gZGVjbGFyYXRpb247XG4gIH1cblxuICAvKipcbiAgICogR2V0cyBhbGwgZGVjb3JhdG9ycyBvZiB0aGUgZ2l2ZW4gY2xhc3Mgc3ltYm9sLiBBbnkgZGVjb3JhdG9yIHRoYXQgaGF2ZSBiZWVuIHN5bnRoZXRpY2FsbHlcbiAgICogaW5qZWN0ZWQgYnkgYSBtaWdyYXRpb24gd2lsbCBub3QgYmUgcHJlc2VudCBpbiB0aGUgcmV0dXJuZWQgY29sbGVjdGlvbi5cbiAgICovXG4gIGdldERlY29yYXRvcnNPZlN5bWJvbChzeW1ib2w6IE5nY2NDbGFzc1N5bWJvbCk6IERlY29yYXRvcltdfG51bGwge1xuICAgIGNvbnN0IHtjbGFzc0RlY29yYXRvcnN9ID0gdGhpcy5hY3F1aXJlRGVjb3JhdG9ySW5mbyhzeW1ib2wpO1xuICAgIGlmIChjbGFzc0RlY29yYXRvcnMgPT09IG51bGwpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIC8vIFJldHVybiBhIGNsb25lIG9mIHRoZSBhcnJheSB0byBwcmV2ZW50IGNvbnN1bWVycyBmcm9tIG11dGF0aW5nIHRoZSBjYWNoZS5cbiAgICByZXR1cm4gQXJyYXkuZnJvbShjbGFzc0RlY29yYXRvcnMpO1xuICB9XG5cbiAgLyoqXG4gICAqIFNlYXJjaCB0aGUgZ2l2ZW4gbW9kdWxlIGZvciB2YXJpYWJsZSBkZWNsYXJhdGlvbnMgaW4gd2hpY2ggdGhlIGluaXRpYWxpemVyXG4gICAqIGlzIGFuIGlkZW50aWZpZXIgbWFya2VkIHdpdGggdGhlIGBQUkVfUjNfTUFSS0VSYC5cbiAgICogQHBhcmFtIG1vZHVsZSB0aGUgbW9kdWxlIGluIHdoaWNoIHRvIHNlYXJjaCBmb3Igc3dpdGNoYWJsZSBkZWNsYXJhdGlvbnMuXG4gICAqIEByZXR1cm5zIGFuIGFycmF5IG9mIHZhcmlhYmxlIGRlY2xhcmF0aW9ucyB0aGF0IG1hdGNoLlxuICAgKi9cbiAgZ2V0U3dpdGNoYWJsZURlY2xhcmF0aW9ucyhtb2R1bGU6IHRzLk5vZGUpOiBTd2l0Y2hhYmxlVmFyaWFibGVEZWNsYXJhdGlvbltdIHtcbiAgICAvLyBEb24ndCBib3RoZXIgdG8gd2FsayB0aGUgQVNUIGlmIHRoZSBtYXJrZXIgaXMgbm90IGZvdW5kIGluIHRoZSB0ZXh0XG4gICAgcmV0dXJuIG1vZHVsZS5nZXRUZXh0KCkuaW5kZXhPZihQUkVfUjNfTUFSS0VSKSA+PSAwID9cbiAgICAgICAgZmluZEFsbChtb2R1bGUsIGlzU3dpdGNoYWJsZVZhcmlhYmxlRGVjbGFyYXRpb24pIDpcbiAgICAgICAgW107XG4gIH1cblxuICBnZXRWYXJpYWJsZVZhbHVlKGRlY2xhcmF0aW9uOiB0cy5WYXJpYWJsZURlY2xhcmF0aW9uKTogdHMuRXhwcmVzc2lvbnxudWxsIHtcbiAgICBjb25zdCB2YWx1ZSA9IHN1cGVyLmdldFZhcmlhYmxlVmFsdWUoZGVjbGFyYXRpb24pO1xuICAgIGlmICh2YWx1ZSkge1xuICAgICAgcmV0dXJuIHZhbHVlO1xuICAgIH1cblxuICAgIC8vIFdlIGhhdmUgYSB2YXJpYWJsZSBkZWNsYXJhdGlvbiB0aGF0IGhhcyBubyBpbml0aWFsaXplci4gRm9yIGV4YW1wbGU6XG4gICAgLy9cbiAgICAvLyBgYGBcbiAgICAvLyB2YXIgSHR0cENsaWVudFhzcmZNb2R1bGVfMTtcbiAgICAvLyBgYGBcbiAgICAvL1xuICAgIC8vIFNvIGxvb2sgZm9yIHRoZSBzcGVjaWFsIHNjZW5hcmlvIHdoZXJlIHRoZSB2YXJpYWJsZSBpcyBiZWluZyBhc3NpZ25lZCBpblxuICAgIC8vIGEgbmVhcmJ5IHN0YXRlbWVudCB0byB0aGUgcmV0dXJuIHZhbHVlIG9mIGEgY2FsbCB0byBgX19kZWNvcmF0ZWAuXG4gICAgLy8gVGhlbiBmaW5kIHRoZSAybmQgYXJndW1lbnQgb2YgdGhhdCBjYWxsLCB0aGUgXCJ0YXJnZXRcIiwgd2hpY2ggd2lsbCBiZSB0aGVcbiAgICAvLyBhY3R1YWwgY2xhc3MgaWRlbnRpZmllci4gRm9yIGV4YW1wbGU6XG4gICAgLy9cbiAgICAvLyBgYGBcbiAgICAvLyBIdHRwQ2xpZW50WHNyZk1vZHVsZSA9IEh0dHBDbGllbnRYc3JmTW9kdWxlXzEgPSB0c2xpYl8xLl9fZGVjb3JhdGUoW1xuICAgIC8vICAgTmdNb2R1bGUoe1xuICAgIC8vICAgICBwcm92aWRlcnM6IFtdLFxuICAgIC8vICAgfSlcbiAgICAvLyBdLCBIdHRwQ2xpZW50WHNyZk1vZHVsZSk7XG4gICAgLy8gYGBgXG4gICAgLy9cbiAgICAvLyBBbmQgZmluYWxseSwgZmluZCB0aGUgZGVjbGFyYXRpb24gb2YgdGhlIGlkZW50aWZpZXIgaW4gdGhhdCBhcmd1bWVudC5cbiAgICAvLyBOb3RlIGFsc28gdGhhdCB0aGUgYXNzaWdubWVudCBjYW4gb2NjdXIgd2l0aGluIGFub3RoZXIgYXNzaWdubWVudC5cbiAgICAvL1xuICAgIGNvbnN0IGJsb2NrID0gZGVjbGFyYXRpb24ucGFyZW50LnBhcmVudC5wYXJlbnQ7XG4gICAgY29uc3Qgc3ltYm9sID0gdGhpcy5jaGVja2VyLmdldFN5bWJvbEF0TG9jYXRpb24oZGVjbGFyYXRpb24ubmFtZSk7XG4gICAgaWYgKHN5bWJvbCAmJiAodHMuaXNCbG9jayhibG9jaykgfHwgdHMuaXNTb3VyY2VGaWxlKGJsb2NrKSkpIHtcbiAgICAgIGNvbnN0IGRlY29yYXRlQ2FsbCA9IHRoaXMuZmluZERlY29yYXRlZFZhcmlhYmxlVmFsdWUoYmxvY2ssIHN5bWJvbCk7XG4gICAgICBjb25zdCB0YXJnZXQgPSBkZWNvcmF0ZUNhbGwgJiYgZGVjb3JhdGVDYWxsLmFyZ3VtZW50c1sxXTtcbiAgICAgIGlmICh0YXJnZXQgJiYgdHMuaXNJZGVudGlmaWVyKHRhcmdldCkpIHtcbiAgICAgICAgY29uc3QgdGFyZ2V0U3ltYm9sID0gdGhpcy5jaGVja2VyLmdldFN5bWJvbEF0TG9jYXRpb24odGFyZ2V0KTtcbiAgICAgICAgY29uc3QgdGFyZ2V0RGVjbGFyYXRpb24gPSB0YXJnZXRTeW1ib2wgJiYgdGFyZ2V0U3ltYm9sLnZhbHVlRGVjbGFyYXRpb247XG4gICAgICAgIGlmICh0YXJnZXREZWNsYXJhdGlvbikge1xuICAgICAgICAgIGlmICh0cy5pc0NsYXNzRGVjbGFyYXRpb24odGFyZ2V0RGVjbGFyYXRpb24pIHx8XG4gICAgICAgICAgICAgIHRzLmlzRnVuY3Rpb25EZWNsYXJhdGlvbih0YXJnZXREZWNsYXJhdGlvbikpIHtcbiAgICAgICAgICAgIC8vIFRoZSB0YXJnZXQgaXMganVzdCBhIGZ1bmN0aW9uIG9yIGNsYXNzIGRlY2xhcmF0aW9uXG4gICAgICAgICAgICAvLyBzbyByZXR1cm4gaXRzIGlkZW50aWZpZXIgYXMgdGhlIHZhcmlhYmxlIHZhbHVlLlxuICAgICAgICAgICAgcmV0dXJuIHRhcmdldERlY2xhcmF0aW9uLm5hbWUgfHwgbnVsbDtcbiAgICAgICAgICB9IGVsc2UgaWYgKHRzLmlzVmFyaWFibGVEZWNsYXJhdGlvbih0YXJnZXREZWNsYXJhdGlvbikpIHtcbiAgICAgICAgICAgIC8vIFRoZSB0YXJnZXQgaXMgYSB2YXJpYWJsZSBkZWNsYXJhdGlvbiwgc28gZmluZCB0aGUgZmFyIHJpZ2h0IGV4cHJlc3Npb24sXG4gICAgICAgICAgICAvLyBpbiB0aGUgY2FzZSBvZiBtdWx0aXBsZSBhc3NpZ25tZW50cyAoZS5nLiBgdmFyMSA9IHZhcjIgPSB2YWx1ZWApLlxuICAgICAgICAgICAgbGV0IHRhcmdldFZhbHVlID0gdGFyZ2V0RGVjbGFyYXRpb24uaW5pdGlhbGl6ZXI7XG4gICAgICAgICAgICB3aGlsZSAodGFyZ2V0VmFsdWUgJiYgaXNBc3NpZ25tZW50KHRhcmdldFZhbHVlKSkge1xuICAgICAgICAgICAgICB0YXJnZXRWYWx1ZSA9IHRhcmdldFZhbHVlLnJpZ2h0O1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgaWYgKHRhcmdldFZhbHVlKSB7XG4gICAgICAgICAgICAgIHJldHVybiB0YXJnZXRWYWx1ZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICB9XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuICAvKipcbiAgICogRmluZCBhbGwgdG9wLWxldmVsIGNsYXNzIHN5bWJvbHMgaW4gdGhlIGdpdmVuIGZpbGUuXG4gICAqIEBwYXJhbSBzb3VyY2VGaWxlIFRoZSBzb3VyY2UgZmlsZSB0byBzZWFyY2ggZm9yIGNsYXNzZXMuXG4gICAqIEByZXR1cm5zIEFuIGFycmF5IG9mIGNsYXNzIHN5bWJvbHMuXG4gICAqL1xuICBmaW5kQ2xhc3NTeW1ib2xzKHNvdXJjZUZpbGU6IHRzLlNvdXJjZUZpbGUpOiBOZ2NjQ2xhc3NTeW1ib2xbXSB7XG4gICAgY29uc3QgY2xhc3NlczogTmdjY0NsYXNzU3ltYm9sW10gPSBbXTtcbiAgICB0aGlzLmdldE1vZHVsZVN0YXRlbWVudHMoc291cmNlRmlsZSkuZm9yRWFjaChzdGF0ZW1lbnQgPT4ge1xuICAgICAgaWYgKHRzLmlzVmFyaWFibGVTdGF0ZW1lbnQoc3RhdGVtZW50KSkge1xuICAgICAgICBzdGF0ZW1lbnQuZGVjbGFyYXRpb25MaXN0LmRlY2xhcmF0aW9ucy5mb3JFYWNoKGRlY2xhcmF0aW9uID0+IHtcbiAgICAgICAgICBjb25zdCBjbGFzc1N5bWJvbCA9IHRoaXMuZ2V0Q2xhc3NTeW1ib2woZGVjbGFyYXRpb24pO1xuICAgICAgICAgIGlmIChjbGFzc1N5bWJvbCkge1xuICAgICAgICAgICAgY2xhc3Nlcy5wdXNoKGNsYXNzU3ltYm9sKTtcbiAgICAgICAgICB9XG4gICAgICAgIH0pO1xuICAgICAgfSBlbHNlIGlmICh0cy5pc0NsYXNzRGVjbGFyYXRpb24oc3RhdGVtZW50KSkge1xuICAgICAgICBjb25zdCBjbGFzc1N5bWJvbCA9IHRoaXMuZ2V0Q2xhc3NTeW1ib2woc3RhdGVtZW50KTtcbiAgICAgICAgaWYgKGNsYXNzU3ltYm9sKSB7XG4gICAgICAgICAgY2xhc3Nlcy5wdXNoKGNsYXNzU3ltYm9sKTtcbiAgICAgICAgfVxuICAgICAgfVxuICAgIH0pO1xuICAgIHJldHVybiBjbGFzc2VzO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldCB0aGUgbnVtYmVyIG9mIGdlbmVyaWMgdHlwZSBwYXJhbWV0ZXJzIG9mIGEgZ2l2ZW4gY2xhc3MuXG4gICAqXG4gICAqIEBwYXJhbSBjbGF6eiBhIGBDbGFzc0RlY2xhcmF0aW9uYCByZXByZXNlbnRpbmcgdGhlIGNsYXNzIG92ZXIgd2hpY2ggdG8gcmVmbGVjdC5cbiAgICpcbiAgICogQHJldHVybnMgdGhlIG51bWJlciBvZiB0eXBlIHBhcmFtZXRlcnMgb2YgdGhlIGNsYXNzLCBpZiBrbm93biwgb3IgYG51bGxgIGlmIHRoZSBkZWNsYXJhdGlvblxuICAgKiBpcyBub3QgYSBjbGFzcyBvciBoYXMgYW4gdW5rbm93biBudW1iZXIgb2YgdHlwZSBwYXJhbWV0ZXJzLlxuICAgKi9cbiAgZ2V0R2VuZXJpY0FyaXR5T2ZDbGFzcyhjbGF6ejogQ2xhc3NEZWNsYXJhdGlvbik6IG51bWJlcnxudWxsIHtcbiAgICBjb25zdCBkdHNEZWNsYXJhdGlvbiA9IHRoaXMuZ2V0RHRzRGVjbGFyYXRpb24oY2xhenopO1xuICAgIGlmIChkdHNEZWNsYXJhdGlvbiAmJiB0cy5pc0NsYXNzRGVjbGFyYXRpb24oZHRzRGVjbGFyYXRpb24pKSB7XG4gICAgICByZXR1cm4gZHRzRGVjbGFyYXRpb24udHlwZVBhcmFtZXRlcnMgPyBkdHNEZWNsYXJhdGlvbi50eXBlUGFyYW1ldGVycy5sZW5ndGggOiAwO1xuICAgIH1cbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuXG4gIC8qKlxuICAgKiBUYWtlIGFuIGV4cG9ydGVkIGRlY2xhcmF0aW9uIG9mIGEgY2xhc3MgKG1heWJlIGRvd24tbGV2ZWxlZCB0byBhIHZhcmlhYmxlKSBhbmQgbG9vayB1cCB0aGVcbiAgICogZGVjbGFyYXRpb24gb2YgaXRzIHR5cGUgaW4gYSBzZXBhcmF0ZSAuZC50cyB0cmVlLlxuICAgKlxuICAgKiBUaGlzIGZ1bmN0aW9uIGlzIGFsbG93ZWQgdG8gcmV0dXJuIGBudWxsYCBpZiB0aGUgY3VycmVudCBjb21waWxhdGlvbiB1bml0IGRvZXMgbm90IGhhdmUgYVxuICAgKiBzZXBhcmF0ZSAuZC50cyB0cmVlLiBXaGVuIGNvbXBpbGluZyBUeXBlU2NyaXB0IGNvZGUgdGhpcyBpcyBhbHdheXMgdGhlIGNhc2UsIHNpbmNlIC5kLnRzIGZpbGVzXG4gICAqIGFyZSBwcm9kdWNlZCBvbmx5IGR1cmluZyB0aGUgZW1pdCBvZiBzdWNoIGEgY29tcGlsYXRpb24uIFdoZW4gY29tcGlsaW5nIC5qcyBjb2RlLCBob3dldmVyLFxuICAgKiB0aGVyZSBpcyBmcmVxdWVudGx5IGEgcGFyYWxsZWwgLmQudHMgdHJlZSB3aGljaCB0aGlzIG1ldGhvZCBleHBvc2VzLlxuICAgKlxuICAgKiBOb3RlIHRoYXQgdGhlIGB0cy5DbGFzc0RlY2xhcmF0aW9uYCByZXR1cm5lZCBmcm9tIHRoaXMgZnVuY3Rpb24gbWF5IG5vdCBiZSBmcm9tIHRoZSBzYW1lXG4gICAqIGB0cy5Qcm9ncmFtYCBhcyB0aGUgaW5wdXQgZGVjbGFyYXRpb24uXG4gICAqL1xuICBnZXREdHNEZWNsYXJhdGlvbihkZWNsYXJhdGlvbjogdHMuRGVjbGFyYXRpb24pOiB0cy5EZWNsYXJhdGlvbnxudWxsIHtcbiAgICBpZiAodGhpcy5kdHMgPT09IG51bGwpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cbiAgICBpZiAoIWlzTmFtZWREZWNsYXJhdGlvbihkZWNsYXJhdGlvbikpIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihgQ2Fubm90IGdldCB0aGUgZHRzIGZpbGUgZm9yIGEgZGVjbGFyYXRpb24gdGhhdCBoYXMgbm8gbmFtZTogJHtcbiAgICAgICAgICBkZWNsYXJhdGlvbi5nZXRUZXh0KCl9IGluICR7ZGVjbGFyYXRpb24uZ2V0U291cmNlRmlsZSgpLmZpbGVOYW1lfWApO1xuICAgIH1cblxuICAgIC8vIFRyeSB0byByZXRyaWV2ZSB0aGUgZHRzIGRlY2xhcmF0aW9uIGZyb20gdGhlIHB1YmxpYyBtYXBcbiAgICBpZiAodGhpcy5wdWJsaWNEdHNEZWNsYXJhdGlvbk1hcCA9PT0gbnVsbCkge1xuICAgICAgdGhpcy5wdWJsaWNEdHNEZWNsYXJhdGlvbk1hcCA9IHRoaXMuY29tcHV0ZVB1YmxpY0R0c0RlY2xhcmF0aW9uTWFwKHRoaXMuc3JjLCB0aGlzLmR0cyk7XG4gICAgfVxuICAgIGlmICh0aGlzLnB1YmxpY0R0c0RlY2xhcmF0aW9uTWFwLmhhcyhkZWNsYXJhdGlvbikpIHtcbiAgICAgIHJldHVybiB0aGlzLnB1YmxpY0R0c0RlY2xhcmF0aW9uTWFwLmdldChkZWNsYXJhdGlvbikhO1xuICAgIH1cblxuICAgIC8vIE5vIHB1YmxpYyBleHBvcnQsIHRyeSB0aGUgcHJpdmF0ZSBtYXBcbiAgICBpZiAodGhpcy5wcml2YXRlRHRzRGVjbGFyYXRpb25NYXAgPT09IG51bGwpIHtcbiAgICAgIHRoaXMucHJpdmF0ZUR0c0RlY2xhcmF0aW9uTWFwID0gdGhpcy5jb21wdXRlUHJpdmF0ZUR0c0RlY2xhcmF0aW9uTWFwKHRoaXMuc3JjLCB0aGlzLmR0cyk7XG4gICAgfVxuICAgIGlmICh0aGlzLnByaXZhdGVEdHNEZWNsYXJhdGlvbk1hcC5oYXMoZGVjbGFyYXRpb24pKSB7XG4gICAgICByZXR1cm4gdGhpcy5wcml2YXRlRHRzRGVjbGFyYXRpb25NYXAuZ2V0KGRlY2xhcmF0aW9uKSE7XG4gICAgfVxuXG4gICAgLy8gTm8gZGVjbGFyYXRpb24gZm91bmQgYXQgYWxsXG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuICBnZXRFbmRPZkNsYXNzKGNsYXNzU3ltYm9sOiBOZ2NjQ2xhc3NTeW1ib2wpOiB0cy5Ob2RlIHtcbiAgICBjb25zdCBpbXBsZW1lbnRhdGlvbiA9IGNsYXNzU3ltYm9sLmltcGxlbWVudGF0aW9uO1xuICAgIGxldCBsYXN0OiB0cy5Ob2RlID0gaW1wbGVtZW50YXRpb24udmFsdWVEZWNsYXJhdGlvbjtcbiAgICBjb25zdCBpbXBsZW1lbnRhdGlvblN0YXRlbWVudCA9IGdldENvbnRhaW5pbmdTdGF0ZW1lbnQobGFzdCk7XG4gICAgaWYgKGltcGxlbWVudGF0aW9uU3RhdGVtZW50ID09PSBudWxsKSByZXR1cm4gbGFzdDtcblxuICAgIGNvbnN0IGNvbnRhaW5lciA9IGltcGxlbWVudGF0aW9uU3RhdGVtZW50LnBhcmVudDtcbiAgICBpZiAodHMuaXNCbG9jayhjb250YWluZXIpKSB7XG4gICAgICAvLyBBc3N1bWUgdGhhdCB0aGUgaW1wbGVtZW50YXRpb24gaXMgaW5zaWRlIGFuIElJRkVcbiAgICAgIGNvbnN0IHJldHVyblN0YXRlbWVudEluZGV4ID0gY29udGFpbmVyLnN0YXRlbWVudHMuZmluZEluZGV4KHRzLmlzUmV0dXJuU3RhdGVtZW50KTtcbiAgICAgIGlmIChyZXR1cm5TdGF0ZW1lbnRJbmRleCA9PT0gLTEpIHtcbiAgICAgICAgdGhyb3cgbmV3IEVycm9yKFxuICAgICAgICAgICAgYENvbXBpbGVkIGNsYXNzIHdyYXBwZXIgSUlGRSBkb2VzIG5vdCBoYXZlIGEgcmV0dXJuIHN0YXRlbWVudDogJHtjbGFzc1N5bWJvbC5uYW1lfSBpbiAke1xuICAgICAgICAgICAgICAgIGNsYXNzU3ltYm9sLmRlY2xhcmF0aW9uLnZhbHVlRGVjbGFyYXRpb24uZ2V0U291cmNlRmlsZSgpLmZpbGVOYW1lfWApO1xuICAgICAgfVxuXG4gICAgICAvLyBSZXR1cm4gdGhlIHN0YXRlbWVudCBiZWZvcmUgdGhlIElJRkUgcmV0dXJuIHN0YXRlbWVudFxuICAgICAgbGFzdCA9IGNvbnRhaW5lci5zdGF0ZW1lbnRzW3JldHVyblN0YXRlbWVudEluZGV4IC0gMV07XG4gICAgfSBlbHNlIGlmICh0cy5pc1NvdXJjZUZpbGUoY29udGFpbmVyKSkge1xuICAgICAgLy8gSWYgdGhlcmUgYXJlIHN0YXRpYyBtZW1iZXJzIG9uIHRoaXMgY2xhc3MgdGhlbiBmaW5kIHRoZSBsYXN0IG9uZVxuICAgICAgaWYgKGltcGxlbWVudGF0aW9uLmV4cG9ydHMgIT09IHVuZGVmaW5lZCkge1xuICAgICAgICBpbXBsZW1lbnRhdGlvbi5leHBvcnRzLmZvckVhY2goZXhwb3J0U3ltYm9sID0+IHtcbiAgICAgICAgICBpZiAoZXhwb3J0U3ltYm9sLnZhbHVlRGVjbGFyYXRpb24gPT09IHVuZGVmaW5lZCkge1xuICAgICAgICAgICAgcmV0dXJuO1xuICAgICAgICAgIH1cbiAgICAgICAgICBjb25zdCBleHBvcnRTdGF0ZW1lbnQgPSBnZXRDb250YWluaW5nU3RhdGVtZW50KGV4cG9ydFN5bWJvbC52YWx1ZURlY2xhcmF0aW9uKTtcbiAgICAgICAgICBpZiAoZXhwb3J0U3RhdGVtZW50ICE9PSBudWxsICYmIGxhc3QuZ2V0RW5kKCkgPCBleHBvcnRTdGF0ZW1lbnQuZ2V0RW5kKCkpIHtcbiAgICAgICAgICAgIGxhc3QgPSBleHBvcnRTdGF0ZW1lbnQ7XG4gICAgICAgICAgfVxuICAgICAgICB9KTtcbiAgICAgIH1cblxuICAgICAgLy8gSWYgdGhlcmUgYXJlIGhlbHBlciBjYWxscyBmb3IgdGhpcyBjbGFzcyB0aGVuIGZpbmQgdGhlIGxhc3Qgb25lXG4gICAgICBjb25zdCBoZWxwZXJzID0gdGhpcy5nZXRIZWxwZXJDYWxsc0ZvckNsYXNzKFxuICAgICAgICAgIGNsYXNzU3ltYm9sLCBbJ19fZGVjb3JhdGUnLCAnX19leHRlbmRzJywgJ19fcGFyYW0nLCAnX19tZXRhZGF0YSddKTtcbiAgICAgIGhlbHBlcnMuZm9yRWFjaChoZWxwZXIgPT4ge1xuICAgICAgICBjb25zdCBoZWxwZXJTdGF0ZW1lbnQgPSBnZXRDb250YWluaW5nU3RhdGVtZW50KGhlbHBlcik7XG4gICAgICAgIGlmIChoZWxwZXJTdGF0ZW1lbnQgIT09IG51bGwgJiYgbGFzdC5nZXRFbmQoKSA8IGhlbHBlclN0YXRlbWVudC5nZXRFbmQoKSkge1xuICAgICAgICAgIGxhc3QgPSBoZWxwZXJTdGF0ZW1lbnQ7XG4gICAgICAgIH1cbiAgICAgIH0pO1xuICAgIH1cbiAgICByZXR1cm4gbGFzdDtcbiAgfVxuXG4gIC8qKlxuICAgKiBDaGVjayB3aGV0aGVyIGEgYERlY2xhcmF0aW9uYCBjb3JyZXNwb25kcyB3aXRoIGEga25vd24gZGVjbGFyYXRpb24sIHN1Y2ggYXMgYE9iamVjdGAsIGFuZCBzZXRcbiAgICogaXRzIGBrbm93bmAgcHJvcGVydHkgdG8gdGhlIGFwcHJvcHJpYXRlIGBLbm93bkRlY2xhcmF0aW9uYC5cbiAgICpcbiAgICogQHBhcmFtIGRlY2wgVGhlIGBEZWNsYXJhdGlvbmAgdG8gY2hlY2suXG4gICAqIEByZXR1cm4gVGhlIHBhc3NlZCBpbiBgRGVjbGFyYXRpb25gIChwb3RlbnRpYWxseSBlbmhhbmNlZCB3aXRoIGEgYEtub3duRGVjbGFyYXRpb25gKS5cbiAgICovXG4gIGRldGVjdEtub3duRGVjbGFyYXRpb248VCBleHRlbmRzIERlY2xhcmF0aW9uPihkZWNsOiBUKTogVCB7XG4gICAgaWYgKGRlY2wua25vd24gPT09IG51bGwgJiYgdGhpcy5pc0phdmFTY3JpcHRPYmplY3REZWNsYXJhdGlvbihkZWNsKSkge1xuICAgICAgLy8gSWYgdGhlIGlkZW50aWZpZXIgcmVzb2x2ZXMgdG8gdGhlIGdsb2JhbCBKYXZhU2NyaXB0IGBPYmplY3RgLCB1cGRhdGUgdGhlIGRlY2xhcmF0aW9uIHRvXG4gICAgICAvLyBkZW5vdGUgaXQgYXMgdGhlIGtub3duIGBKc0dsb2JhbE9iamVjdGAgZGVjbGFyYXRpb24uXG4gICAgICBkZWNsLmtub3duID0gS25vd25EZWNsYXJhdGlvbi5Kc0dsb2JhbE9iamVjdDtcbiAgICB9XG4gICAgcmV0dXJuIGRlY2w7XG4gIH1cblxuXG4gIC8vLy8vLy8vLy8vLy8gUHJvdGVjdGVkIEhlbHBlcnMgLy8vLy8vLy8vLy8vL1xuXG4gIC8qKlxuICAgKiBBIGNsYXNzIG1heSBiZSBkZWNsYXJlZCBhcyBhIHRvcCBsZXZlbCBjbGFzcyBkZWNsYXJhdGlvbjpcbiAgICpcbiAgICogYGBgXG4gICAqIGNsYXNzIE91dGVyQ2xhc3MgeyAuLi4gfVxuICAgKiBgYGBcbiAgICpcbiAgICogb3IgaW4gYSB2YXJpYWJsZSBkZWNsYXJhdGlvbiB0byBhIGNsYXNzIGV4cHJlc3Npb246XG4gICAqXG4gICAqIGBgYFxuICAgKiB2YXIgT3V0ZXJDbGFzcyA9IENsYXNzQWxpYXMgPSBjbGFzcyBJbm5lckNsYXNzIHt9O1xuICAgKiBgYGBcbiAgICpcbiAgICogb3IgaW4gYSB2YXJpYWJsZSBkZWNsYXJhdGlvbiB0byBhbiBJSUZFIGNvbnRhaW5pbmcgYSBjbGFzcyBkZWNsYXJhdGlvblxuICAgKlxuICAgKiBgYGBcbiAgICogdmFyIE91dGVyQ2xhc3MgPSBDbGFzc0FsaWFzID0gKCgpID0+IHtcbiAgICogICBjbGFzcyBJbm5lckNsYXNzIHt9XG4gICAqICAgLi4uXG4gICAqICAgcmV0dXJuIElubmVyQ2xhc3M7XG4gICAqIH0pKClcbiAgICogYGBgXG4gICAqXG4gICAqIG9yIGluIGEgdmFyaWFibGUgZGVjbGFyYXRpb24gdG8gYW4gSUlGRSBjb250YWluaW5nIGEgZnVuY3Rpb24gZGVjbGFyYXRpb25cbiAgICpcbiAgICogYGBgXG4gICAqIHZhciBPdXRlckNsYXNzID0gQ2xhc3NBbGlhcyA9ICgoKSA9PiB7XG4gICAqICAgZnVuY3Rpb24gSW5uZXJDbGFzcygpIHt9XG4gICAqICAgLi4uXG4gICAqICAgcmV0dXJuIElubmVyQ2xhc3M7XG4gICAqIH0pKClcbiAgICogYGBgXG4gICAqXG4gICAqIFRoaXMgbWV0aG9kIHJldHVybnMgYW4gYE5nY2NDbGFzc1N5bWJvbGAgd2hlbiBwcm92aWRlZCB3aXRoIG9uZSBvZiB0aGVzZSBjYXNlcy5cbiAgICpcbiAgICogQHBhcmFtIGRlY2xhcmF0aW9uIHRoZSBkZWNsYXJhdGlvbiB3aG9zZSBzeW1ib2wgd2UgYXJlIGZpbmRpbmcuXG4gICAqIEByZXR1cm5zIHRoZSBzeW1ib2wgZm9yIHRoZSBjbGFzcyBvciBgdW5kZWZpbmVkYCBpZiBgZGVjbGFyYXRpb25gIGRvZXMgbm90IHJlcHJlc2VudCBhbiBvdXRlclxuICAgKiAgICAgZGVjbGFyYXRpb24gb2YgYSBjbGFzcy5cbiAgICovXG4gIHByb3RlY3RlZCBnZXRDbGFzc1N5bWJvbEZyb21PdXRlckRlY2xhcmF0aW9uKGRlY2xhcmF0aW9uOiB0cy5Ob2RlKTogTmdjY0NsYXNzU3ltYm9sfHVuZGVmaW5lZCB7XG4gICAgLy8gUmV0dXJuIGEgY2xhc3Mgc3ltYm9sIHdpdGhvdXQgYW4gaW5uZXIgZGVjbGFyYXRpb24gaWYgaXQgaXMgYSByZWd1bGFyIFwidG9wIGxldmVsXCIgY2xhc3NcbiAgICBpZiAoaXNOYW1lZENsYXNzRGVjbGFyYXRpb24oZGVjbGFyYXRpb24pICYmIGlzVG9wTGV2ZWwoZGVjbGFyYXRpb24pKSB7XG4gICAgICByZXR1cm4gdGhpcy5jcmVhdGVDbGFzc1N5bWJvbChkZWNsYXJhdGlvbiwgbnVsbCk7XG4gICAgfVxuXG4gICAgLy8gT3RoZXJ3aXNlLCBhbiBvdXRlciBjbGFzcyBkZWNsYXJhdGlvbiBtdXN0IGJlIGFuIGluaXRpYWxpemVkIHZhcmlhYmxlIGRlY2xhcmF0aW9uOlxuICAgIGlmICghaXNJbml0aWFsaXplZFZhcmlhYmxlQ2xhc3NEZWNsYXJhdGlvbihkZWNsYXJhdGlvbikpIHtcbiAgICAgIHJldHVybiB1bmRlZmluZWQ7XG4gICAgfVxuXG4gICAgY29uc3QgaW5uZXJEZWNsYXJhdGlvbiA9IGdldElubmVyQ2xhc3NEZWNsYXJhdGlvbihza2lwQ2xhc3NBbGlhc2VzKGRlY2xhcmF0aW9uKSk7XG4gICAgaWYgKGlubmVyRGVjbGFyYXRpb24gIT09IG51bGwpIHtcbiAgICAgIHJldHVybiB0aGlzLmNyZWF0ZUNsYXNzU3ltYm9sKGRlY2xhcmF0aW9uLCBpbm5lckRlY2xhcmF0aW9uKTtcbiAgICB9XG5cblxuICAgIHJldHVybiB1bmRlZmluZWQ7XG4gIH1cblxuICAvKipcbiAgICogSW4gRVMyMDE1LCBhIGNsYXNzIG1heSBiZSBkZWNsYXJlZCB1c2luZyBhIHZhcmlhYmxlIGRlY2xhcmF0aW9uIG9mIHRoZSBmb2xsb3dpbmcgc3RydWN0dXJlczpcbiAgICpcbiAgICogYGBgXG4gICAqIGxldCBNeUNsYXNzID0gTXlDbGFzc18xID0gY2xhc3MgTXlDbGFzcyB7fTtcbiAgICogYGBgXG4gICAqXG4gICAqIG9yXG4gICAqXG4gICAqIGBgYFxuICAgKiBsZXQgTXlDbGFzcyA9IE15Q2xhc3NfMSA9ICgoKSA9PiB7IGNsYXNzIE15Q2xhc3Mge30gLi4uIHJldHVybiBNeUNsYXNzOyB9KSgpXG4gICAqIGBgYFxuICAgKlxuICAgKiBvclxuICAgKlxuICAgKiBgYGBcbiAgICogbGV0IE15Q2xhc3MgPSBNeUNsYXNzXzEgPSAoKCkgPT4geyBsZXQgTXlDbGFzcyA9IGNsYXNzIE15Q2xhc3Mge307IC4uLiByZXR1cm4gTXlDbGFzczsgfSkoKVxuICAgKiBgYGBcbiAgICpcbiAgICogVGhpcyBtZXRob2QgZXh0cmFjdHMgdGhlIGBOZ2NjQ2xhc3NTeW1ib2xgIGZvciBgTXlDbGFzc2Agd2hlbiBwcm92aWRlZCB3aXRoIHRoZVxuICAgKiBgY2xhc3MgTXlDbGFzcyB7fWAgZGVjbGFyYXRpb24gbm9kZS4gV2hlbiB0aGUgYHZhciBNeUNsYXNzYCBub2RlIG9yIGFueSBvdGhlciBub2RlIGlzIGdpdmVuLFxuICAgKiB0aGlzIG1ldGhvZCB3aWxsIHJldHVybiB1bmRlZmluZWQgaW5zdGVhZC5cbiAgICpcbiAgICogQHBhcmFtIGRlY2xhcmF0aW9uIHRoZSBkZWNsYXJhdGlvbiB3aG9zZSBzeW1ib2wgd2UgYXJlIGZpbmRpbmcuXG4gICAqIEByZXR1cm5zIHRoZSBzeW1ib2wgZm9yIHRoZSBub2RlIG9yIGB1bmRlZmluZWRgIGlmIGl0IGRvZXMgbm90IHJlcHJlc2VudCBhbiBpbm5lciBkZWNsYXJhdGlvblxuICAgKiBvZiBhIGNsYXNzLlxuICAgKi9cbiAgcHJvdGVjdGVkIGdldENsYXNzU3ltYm9sRnJvbUlubmVyRGVjbGFyYXRpb24oZGVjbGFyYXRpb246IHRzLk5vZGUpOiBOZ2NjQ2xhc3NTeW1ib2x8dW5kZWZpbmVkIHtcbiAgICBsZXQgb3V0ZXJEZWNsYXJhdGlvbjogdHMuQ2xhc3NEZWNsYXJhdGlvbnx0cy5WYXJpYWJsZURlY2xhcmF0aW9ufHVuZGVmaW5lZCA9IHVuZGVmaW5lZDtcblxuICAgIGlmICh0cy5pc0NsYXNzRXhwcmVzc2lvbihkZWNsYXJhdGlvbikgJiYgaGFzTmFtZUlkZW50aWZpZXIoZGVjbGFyYXRpb24pKSB7XG4gICAgICAvLyBIYW5kbGUgYGxldCBNeUNsYXNzID0gTXlDbGFzc18xID0gY2xhc3MgTXlDbGFzcyB7fTtgXG4gICAgICBvdXRlckRlY2xhcmF0aW9uID0gZ2V0RmFyTGVmdEhhbmRTaWRlT2ZBc3NpZ25tZW50KGRlY2xhcmF0aW9uKTtcblxuICAgICAgLy8gSGFuZGxlIHRoaXMgYmVpbmcgaW4gYW4gSUlGRVxuICAgICAgaWYgKG91dGVyRGVjbGFyYXRpb24gIT09IHVuZGVmaW5lZCAmJiAhaXNUb3BMZXZlbChvdXRlckRlY2xhcmF0aW9uKSkge1xuICAgICAgICBvdXRlckRlY2xhcmF0aW9uID0gZ2V0Q29udGFpbmluZ1ZhcmlhYmxlRGVjbGFyYXRpb24ob3V0ZXJEZWNsYXJhdGlvbik7XG4gICAgICB9XG4gICAgfSBlbHNlIGlmIChpc05hbWVkQ2xhc3NEZWNsYXJhdGlvbihkZWNsYXJhdGlvbikpIHtcbiAgICAgIC8vIEhhbmRsZSBgY2xhc3MgTXlDbGFzcyB7fWAgc3RhdGVtZW50XG4gICAgICBpZiAoaXNUb3BMZXZlbChkZWNsYXJhdGlvbikpIHtcbiAgICAgICAgLy8gQXQgdGhlIHRvcCBsZXZlbFxuICAgICAgICBvdXRlckRlY2xhcmF0aW9uID0gZGVjbGFyYXRpb247XG4gICAgICB9IGVsc2Uge1xuICAgICAgICAvLyBPciBpbnNpZGUgYW4gSUlGRVxuICAgICAgICBvdXRlckRlY2xhcmF0aW9uID0gZ2V0Q29udGFpbmluZ1ZhcmlhYmxlRGVjbGFyYXRpb24oZGVjbGFyYXRpb24pO1xuICAgICAgfVxuICAgIH1cblxuICAgIGlmIChvdXRlckRlY2xhcmF0aW9uID09PSB1bmRlZmluZWQgfHwgIWhhc05hbWVJZGVudGlmaWVyKG91dGVyRGVjbGFyYXRpb24pKSB7XG4gICAgICByZXR1cm4gdW5kZWZpbmVkO1xuICAgIH1cblxuICAgIHJldHVybiB0aGlzLmNyZWF0ZUNsYXNzU3ltYm9sKG91dGVyRGVjbGFyYXRpb24sIGRlY2xhcmF0aW9uKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBDcmVhdGVzIGFuIGBOZ2NjQ2xhc3NTeW1ib2xgIGZyb20gYW4gb3V0ZXIgYW5kIGlubmVyIGRlY2xhcmF0aW9uLiBJZiBhIGNsYXNzIG9ubHkgaGFzIGFuIG91dGVyXG4gICAqIGRlY2xhcmF0aW9uLCB0aGUgXCJpbXBsZW1lbnRhdGlvblwiIHN5bWJvbCBvZiB0aGUgY3JlYXRlZCBgTmdjY0NsYXNzU3ltYm9sYCB3aWxsIGJlIHNldCBlcXVhbCB0b1xuICAgKiB0aGUgXCJkZWNsYXJhdGlvblwiIHN5bWJvbC5cbiAgICpcbiAgICogQHBhcmFtIG91dGVyRGVjbGFyYXRpb24gVGhlIG91dGVyIGRlY2xhcmF0aW9uIG5vZGUgb2YgdGhlIGNsYXNzLlxuICAgKiBAcGFyYW0gaW5uZXJEZWNsYXJhdGlvbiBUaGUgaW5uZXIgZGVjbGFyYXRpb24gbm9kZSBvZiB0aGUgY2xhc3MsIG9yIHVuZGVmaW5lZCBpZiBubyBpbm5lclxuICAgKiBkZWNsYXJhdGlvbiBpcyBwcmVzZW50LlxuICAgKiBAcmV0dXJucyB0aGUgYE5nY2NDbGFzc1N5bWJvbGAgcmVwcmVzZW50aW5nIHRoZSBjbGFzcywgb3IgdW5kZWZpbmVkIGlmIGEgYHRzLlN5bWJvbGAgZm9yIGFueSBvZlxuICAgKiB0aGUgZGVjbGFyYXRpb25zIGNvdWxkIG5vdCBiZSByZXNvbHZlZC5cbiAgICovXG4gIHByb3RlY3RlZCBjcmVhdGVDbGFzc1N5bWJvbChvdXRlckRlY2xhcmF0aW9uOiBDbGFzc0RlY2xhcmF0aW9uLCBpbm5lckRlY2xhcmF0aW9uOiB0cy5Ob2RlfG51bGwpOlxuICAgICAgTmdjY0NsYXNzU3ltYm9sfHVuZGVmaW5lZCB7XG4gICAgY29uc3QgZGVjbGFyYXRpb25TeW1ib2wgPVxuICAgICAgICB0aGlzLmNoZWNrZXIuZ2V0U3ltYm9sQXRMb2NhdGlvbihvdXRlckRlY2xhcmF0aW9uLm5hbWUpIGFzIENsYXNzU3ltYm9sIHwgdW5kZWZpbmVkO1xuICAgIGlmIChkZWNsYXJhdGlvblN5bWJvbCA9PT0gdW5kZWZpbmVkKSB7XG4gICAgICByZXR1cm4gdW5kZWZpbmVkO1xuICAgIH1cblxuICAgIGxldCBpbXBsZW1lbnRhdGlvblN5bWJvbCA9IGRlY2xhcmF0aW9uU3ltYm9sO1xuICAgIGlmIChpbm5lckRlY2xhcmF0aW9uICE9PSBudWxsICYmIGlzTmFtZWREZWNsYXJhdGlvbihpbm5lckRlY2xhcmF0aW9uKSkge1xuICAgICAgaW1wbGVtZW50YXRpb25TeW1ib2wgPSB0aGlzLmNoZWNrZXIuZ2V0U3ltYm9sQXRMb2NhdGlvbihpbm5lckRlY2xhcmF0aW9uLm5hbWUpIGFzIENsYXNzU3ltYm9sO1xuICAgIH1cblxuICAgIGlmIChpbXBsZW1lbnRhdGlvblN5bWJvbCA9PT0gdW5kZWZpbmVkKSB7XG4gICAgICByZXR1cm4gdW5kZWZpbmVkO1xuICAgIH1cblxuICAgIGNvbnN0IGNsYXNzU3ltYm9sOiBOZ2NjQ2xhc3NTeW1ib2wgPSB7XG4gICAgICBuYW1lOiBkZWNsYXJhdGlvblN5bWJvbC5uYW1lLFxuICAgICAgZGVjbGFyYXRpb246IGRlY2xhcmF0aW9uU3ltYm9sLFxuICAgICAgaW1wbGVtZW50YXRpb246IGltcGxlbWVudGF0aW9uU3ltYm9sLFxuICAgIH07XG5cbiAgICBsZXQgYWRqYWNlbnQgPSB0aGlzLmdldEFkamFjZW50U3ltYm9sKGRlY2xhcmF0aW9uU3ltYm9sLCBpbXBsZW1lbnRhdGlvblN5bWJvbCk7XG4gICAgaWYgKGFkamFjZW50ICE9PSBudWxsKSB7XG4gICAgICBjbGFzc1N5bWJvbC5hZGphY2VudCA9IGFkamFjZW50O1xuICAgIH1cblxuICAgIHJldHVybiBjbGFzc1N5bWJvbDtcbiAgfVxuXG4gIHByaXZhdGUgZ2V0QWRqYWNlbnRTeW1ib2woZGVjbGFyYXRpb25TeW1ib2w6IENsYXNzU3ltYm9sLCBpbXBsZW1lbnRhdGlvblN5bWJvbDogQ2xhc3NTeW1ib2wpOlxuICAgICAgQ2xhc3NTeW1ib2x8dW5kZWZpbmVkIHtcbiAgICBpZiAoZGVjbGFyYXRpb25TeW1ib2wgPT09IGltcGxlbWVudGF0aW9uU3ltYm9sKSB7XG4gICAgICByZXR1cm4gdW5kZWZpbmVkO1xuICAgIH1cbiAgICBjb25zdCBpbm5lckRlY2xhcmF0aW9uID0gaW1wbGVtZW50YXRpb25TeW1ib2wudmFsdWVEZWNsYXJhdGlvbjtcbiAgICBpZiAoIXRzLmlzQ2xhc3NFeHByZXNzaW9uKGlubmVyRGVjbGFyYXRpb24pICYmICF0cy5pc0Z1bmN0aW9uRXhwcmVzc2lvbihpbm5lckRlY2xhcmF0aW9uKSkge1xuICAgICAgcmV0dXJuIHVuZGVmaW5lZDtcbiAgICB9XG4gICAgLy8gRGVhbCB3aXRoIHRoZSBpbm5lciBjbGFzcyBsb29raW5nIGxpa2UgdGhpcyBpbnNpZGUgYW4gSUlGRTpcbiAgICAvLyBgbGV0IE15Q2xhc3MgPSBjbGFzcyBNeUNsYXNzIHt9O2Agb3IgYHZhciBNeUNsYXNzID0gZnVuY3Rpb24gTXlDbGFzcygpIHt9O2BcbiAgICBjb25zdCBhZGphY2VudERlY2xhcmF0aW9uID0gZ2V0RmFyTGVmdEhhbmRTaWRlT2ZBc3NpZ25tZW50KGlubmVyRGVjbGFyYXRpb24pO1xuICAgIGlmIChhZGphY2VudERlY2xhcmF0aW9uID09PSB1bmRlZmluZWQgfHwgIWlzTmFtZWRWYXJpYWJsZURlY2xhcmF0aW9uKGFkamFjZW50RGVjbGFyYXRpb24pKSB7XG4gICAgICByZXR1cm4gdW5kZWZpbmVkO1xuICAgIH1cbiAgICBjb25zdCBhZGphY2VudFN5bWJvbCA9XG4gICAgICAgIHRoaXMuY2hlY2tlci5nZXRTeW1ib2xBdExvY2F0aW9uKGFkamFjZW50RGVjbGFyYXRpb24ubmFtZSkgYXMgQ2xhc3NTeW1ib2w7XG4gICAgaWYgKGFkamFjZW50U3ltYm9sID09PSBkZWNsYXJhdGlvblN5bWJvbCB8fCBhZGphY2VudFN5bWJvbCA9PT0gaW1wbGVtZW50YXRpb25TeW1ib2wpIHtcbiAgICAgIHJldHVybiB1bmRlZmluZWQ7XG4gICAgfVxuICAgIHJldHVybiBhZGphY2VudFN5bWJvbDtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZXNvbHZlIGEgYHRzLlN5bWJvbGAgdG8gaXRzIGRlY2xhcmF0aW9uIGFuZCBkZXRlY3Qgd2hldGhlciBpdCBjb3JyZXNwb25kcyB3aXRoIGEga25vd25cbiAgICogZGVjbGFyYXRpb24uXG4gICAqL1xuICBwcm90ZWN0ZWQgZ2V0RGVjbGFyYXRpb25PZlN5bWJvbChzeW1ib2w6IHRzLlN5bWJvbCwgb3JpZ2luYWxJZDogdHMuSWRlbnRpZmllcnxudWxsKTogRGVjbGFyYXRpb25cbiAgICAgIHxudWxsIHtcbiAgICBjb25zdCBkZWNsYXJhdGlvbiA9IHN1cGVyLmdldERlY2xhcmF0aW9uT2ZTeW1ib2woc3ltYm9sLCBvcmlnaW5hbElkKTtcbiAgICBpZiAoZGVjbGFyYXRpb24gPT09IG51bGwpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cbiAgICByZXR1cm4gdGhpcy5kZXRlY3RLbm93bkRlY2xhcmF0aW9uKGRlY2xhcmF0aW9uKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBGaW5kcyB0aGUgaWRlbnRpZmllciBvZiB0aGUgYWN0dWFsIGNsYXNzIGRlY2xhcmF0aW9uIGZvciBhIHBvdGVudGlhbGx5IGFsaWFzZWQgZGVjbGFyYXRpb24gb2YgYVxuICAgKiBjbGFzcy5cbiAgICpcbiAgICogSWYgdGhlIGdpdmVuIGRlY2xhcmF0aW9uIGlzIGZvciBhbiBhbGlhcyBvZiBhIGNsYXNzLCB0aGlzIGZ1bmN0aW9uIHdpbGwgZGV0ZXJtaW5lIGFuIGlkZW50aWZpZXJcbiAgICogdG8gdGhlIG9yaWdpbmFsIGRlY2xhcmF0aW9uIHRoYXQgcmVwcmVzZW50cyB0aGlzIGNsYXNzLlxuICAgKlxuICAgKiBAcGFyYW0gZGVjbGFyYXRpb24gVGhlIGRlY2xhcmF0aW9uIHRvIHJlc29sdmUuXG4gICAqIEByZXR1cm5zIFRoZSBvcmlnaW5hbCBpZGVudGlmaWVyIHRoYXQgdGhlIGdpdmVuIGNsYXNzIGRlY2xhcmF0aW9uIHJlc29sdmVzIHRvLCBvciBgdW5kZWZpbmVkYFxuICAgKiBpZiB0aGUgZGVjbGFyYXRpb24gZG9lcyBub3QgcmVwcmVzZW50IGFuIGFsaWFzZWQgY2xhc3MuXG4gICAqL1xuICBwcm90ZWN0ZWQgcmVzb2x2ZUFsaWFzZWRDbGFzc0lkZW50aWZpZXIoZGVjbGFyYXRpb246IHRzLkRlY2xhcmF0aW9uKTogdHMuSWRlbnRpZmllcnxudWxsIHtcbiAgICB0aGlzLmVuc3VyZVByZXByb2Nlc3NlZChkZWNsYXJhdGlvbi5nZXRTb3VyY2VGaWxlKCkpO1xuICAgIHJldHVybiB0aGlzLmFsaWFzZWRDbGFzc0RlY2xhcmF0aW9ucy5oYXMoZGVjbGFyYXRpb24pID9cbiAgICAgICAgdGhpcy5hbGlhc2VkQ2xhc3NEZWNsYXJhdGlvbnMuZ2V0KGRlY2xhcmF0aW9uKSEgOlxuICAgICAgICBudWxsO1xuICB9XG5cbiAgLyoqXG4gICAqIEVuc3VyZXMgdGhhdCB0aGUgc291cmNlIGZpbGUgdGhhdCBgbm9kZWAgaXMgcGFydCBvZiBoYXMgYmVlbiBwcmVwcm9jZXNzZWQuXG4gICAqXG4gICAqIER1cmluZyBwcmVwcm9jZXNzaW5nLCBhbGwgc3RhdGVtZW50cyBpbiB0aGUgc291cmNlIGZpbGUgd2lsbCBiZSB2aXNpdGVkIHN1Y2ggdGhhdCBjZXJ0YWluXG4gICAqIHByb2Nlc3Npbmcgc3RlcHMgY2FuIGJlIGRvbmUgdXAtZnJvbnQgYW5kIGNhY2hlZCBmb3Igc3Vic2VxdWVudCB1c2FnZXMuXG4gICAqXG4gICAqIEBwYXJhbSBzb3VyY2VGaWxlIFRoZSBzb3VyY2UgZmlsZSB0aGF0IG5lZWRzIHRvIGhhdmUgZ29uZSB0aHJvdWdoIHByZXByb2Nlc3NpbmcuXG4gICAqL1xuICBwcm90ZWN0ZWQgZW5zdXJlUHJlcHJvY2Vzc2VkKHNvdXJjZUZpbGU6IHRzLlNvdXJjZUZpbGUpOiB2b2lkIHtcbiAgICBpZiAoIXRoaXMucHJlcHJvY2Vzc2VkU291cmNlRmlsZXMuaGFzKHNvdXJjZUZpbGUpKSB7XG4gICAgICB0aGlzLnByZXByb2Nlc3NlZFNvdXJjZUZpbGVzLmFkZChzb3VyY2VGaWxlKTtcblxuICAgICAgZm9yIChjb25zdCBzdGF0ZW1lbnQgb2YgdGhpcy5nZXRNb2R1bGVTdGF0ZW1lbnRzKHNvdXJjZUZpbGUpKSB7XG4gICAgICAgIHRoaXMucHJlcHJvY2Vzc1N0YXRlbWVudChzdGF0ZW1lbnQpO1xuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBBbmFseXplcyB0aGUgZ2l2ZW4gc3RhdGVtZW50IHRvIHNlZSBpZiBpdCBjb3JyZXNwb25kcyB3aXRoIGEgdmFyaWFibGUgZGVjbGFyYXRpb24gbGlrZVxuICAgKiBgbGV0IE15Q2xhc3MgPSBNeUNsYXNzXzEgPSBjbGFzcyBNeUNsYXNzIHt9O2AuIElmIHNvLCB0aGUgZGVjbGFyYXRpb24gb2YgYE15Q2xhc3NfMWBcbiAgICogaXMgYXNzb2NpYXRlZCB3aXRoIHRoZSBgTXlDbGFzc2AgaWRlbnRpZmllci5cbiAgICpcbiAgICogQHBhcmFtIHN0YXRlbWVudCBUaGUgc3RhdGVtZW50IHRoYXQgbmVlZHMgdG8gYmUgcHJlcHJvY2Vzc2VkLlxuICAgKi9cbiAgcHJvdGVjdGVkIHByZXByb2Nlc3NTdGF0ZW1lbnQoc3RhdGVtZW50OiB0cy5TdGF0ZW1lbnQpOiB2b2lkIHtcbiAgICBpZiAoIXRzLmlzVmFyaWFibGVTdGF0ZW1lbnQoc3RhdGVtZW50KSkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGNvbnN0IGRlY2xhcmF0aW9ucyA9IHN0YXRlbWVudC5kZWNsYXJhdGlvbkxpc3QuZGVjbGFyYXRpb25zO1xuICAgIGlmIChkZWNsYXJhdGlvbnMubGVuZ3RoICE9PSAxKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgY29uc3QgZGVjbGFyYXRpb24gPSBkZWNsYXJhdGlvbnNbMF07XG4gICAgY29uc3QgaW5pdGlhbGl6ZXIgPSBkZWNsYXJhdGlvbi5pbml0aWFsaXplcjtcbiAgICBpZiAoIXRzLmlzSWRlbnRpZmllcihkZWNsYXJhdGlvbi5uYW1lKSB8fCAhaW5pdGlhbGl6ZXIgfHwgIWlzQXNzaWdubWVudChpbml0aWFsaXplcikgfHxcbiAgICAgICAgIXRzLmlzSWRlbnRpZmllcihpbml0aWFsaXplci5sZWZ0KSB8fCAhdGhpcy5pc0NsYXNzKGRlY2xhcmF0aW9uKSkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGNvbnN0IGFsaWFzZWRJZGVudGlmaWVyID0gaW5pdGlhbGl6ZXIubGVmdDtcblxuICAgIGNvbnN0IGFsaWFzZWREZWNsYXJhdGlvbiA9IHRoaXMuZ2V0RGVjbGFyYXRpb25PZklkZW50aWZpZXIoYWxpYXNlZElkZW50aWZpZXIpO1xuICAgIGlmIChhbGlhc2VkRGVjbGFyYXRpb24gPT09IG51bGwgfHwgYWxpYXNlZERlY2xhcmF0aW9uLm5vZGUgPT09IG51bGwpIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihcbiAgICAgICAgICBgVW5hYmxlIHRvIGxvY2F0ZSBkZWNsYXJhdGlvbiBvZiAke2FsaWFzZWRJZGVudGlmaWVyLnRleHR9IGluIFwiJHtzdGF0ZW1lbnQuZ2V0VGV4dCgpfVwiYCk7XG4gICAgfVxuICAgIHRoaXMuYWxpYXNlZENsYXNzRGVjbGFyYXRpb25zLnNldChhbGlhc2VkRGVjbGFyYXRpb24ubm9kZSwgZGVjbGFyYXRpb24ubmFtZSk7XG4gIH1cblxuICAvKipcbiAgICogR2V0IHRoZSB0b3AgbGV2ZWwgc3RhdGVtZW50cyBmb3IgYSBtb2R1bGUuXG4gICAqXG4gICAqIEluIEVTNSBhbmQgRVMyMDE1IHRoaXMgaXMganVzdCB0aGUgdG9wIGxldmVsIHN0YXRlbWVudHMgb2YgdGhlIGZpbGUuXG4gICAqIEBwYXJhbSBzb3VyY2VGaWxlIFRoZSBtb2R1bGUgd2hvc2Ugc3RhdGVtZW50cyB3ZSB3YW50LlxuICAgKiBAcmV0dXJucyBBbiBhcnJheSBvZiB0b3AgbGV2ZWwgc3RhdGVtZW50cyBmb3IgdGhlIGdpdmVuIG1vZHVsZS5cbiAgICovXG4gIHByb3RlY3RlZCBnZXRNb2R1bGVTdGF0ZW1lbnRzKHNvdXJjZUZpbGU6IHRzLlNvdXJjZUZpbGUpOiB0cy5TdGF0ZW1lbnRbXSB7XG4gICAgcmV0dXJuIEFycmF5LmZyb20oc291cmNlRmlsZS5zdGF0ZW1lbnRzKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBXYWxrIHRoZSBBU1QgbG9va2luZyBmb3IgYW4gYXNzaWdubWVudCB0byB0aGUgc3BlY2lmaWVkIHN5bWJvbC5cbiAgICogQHBhcmFtIG5vZGUgVGhlIGN1cnJlbnQgbm9kZSB3ZSBhcmUgc2VhcmNoaW5nLlxuICAgKiBAcmV0dXJucyBhbiBleHByZXNzaW9uIHRoYXQgcmVwcmVzZW50cyB0aGUgdmFsdWUgb2YgdGhlIHZhcmlhYmxlLCBvciB1bmRlZmluZWQgaWYgbm9uZSBjYW4gYmVcbiAgICogZm91bmQuXG4gICAqL1xuICBwcm90ZWN0ZWQgZmluZERlY29yYXRlZFZhcmlhYmxlVmFsdWUobm9kZTogdHMuTm9kZXx1bmRlZmluZWQsIHN5bWJvbDogdHMuU3ltYm9sKTpcbiAgICAgIHRzLkNhbGxFeHByZXNzaW9ufG51bGwge1xuICAgIGlmICghbm9kZSkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIGlmICh0cy5pc0JpbmFyeUV4cHJlc3Npb24obm9kZSkgJiYgbm9kZS5vcGVyYXRvclRva2VuLmtpbmQgPT09IHRzLlN5bnRheEtpbmQuRXF1YWxzVG9rZW4pIHtcbiAgICAgIGNvbnN0IGxlZnQgPSBub2RlLmxlZnQ7XG4gICAgICBjb25zdCByaWdodCA9IG5vZGUucmlnaHQ7XG4gICAgICBpZiAodHMuaXNJZGVudGlmaWVyKGxlZnQpICYmIHRoaXMuY2hlY2tlci5nZXRTeW1ib2xBdExvY2F0aW9uKGxlZnQpID09PSBzeW1ib2wpIHtcbiAgICAgICAgcmV0dXJuICh0cy5pc0NhbGxFeHByZXNzaW9uKHJpZ2h0KSAmJiBnZXRDYWxsZWVOYW1lKHJpZ2h0KSA9PT0gJ19fZGVjb3JhdGUnKSA/IHJpZ2h0IDogbnVsbDtcbiAgICAgIH1cbiAgICAgIHJldHVybiB0aGlzLmZpbmREZWNvcmF0ZWRWYXJpYWJsZVZhbHVlKHJpZ2h0LCBzeW1ib2wpO1xuICAgIH1cbiAgICByZXR1cm4gbm9kZS5mb3JFYWNoQ2hpbGQobm9kZSA9PiB0aGlzLmZpbmREZWNvcmF0ZWRWYXJpYWJsZVZhbHVlKG5vZGUsIHN5bWJvbCkpIHx8IG51bGw7XG4gIH1cblxuICAvKipcbiAgICogVHJ5IHRvIHJldHJpZXZlIHRoZSBzeW1ib2wgb2YgYSBzdGF0aWMgcHJvcGVydHkgb24gYSBjbGFzcy5cbiAgICpcbiAgICogSW4gc29tZSBjYXNlcywgYSBzdGF0aWMgcHJvcGVydHkgY2FuIGVpdGhlciBiZSBzZXQgb24gdGhlIGlubmVyIChpbXBsZW1lbnRhdGlvbiBvciBhZGphY2VudClcbiAgICogZGVjbGFyYXRpb24gaW5zaWRlIHRoZSBjbGFzcycgSUlGRSwgb3IgaXQgY2FuIGJlIHNldCBvbiB0aGUgb3V0ZXIgdmFyaWFibGUgZGVjbGFyYXRpb24uXG4gICAqIFRoZXJlZm9yZSwgdGhlIGhvc3QgY2hlY2tzIGFsbCBwbGFjZXMsIGZpcnN0IGxvb2tpbmcgdXAgdGhlIHByb3BlcnR5IG9uIHRoZSBpbm5lciBzeW1ib2xzLCBhbmRcbiAgICogaWYgdGhlIHByb3BlcnR5IGlzIG5vdCBmb3VuZCBpdCB3aWxsIGZhbGwgYmFjayB0byBsb29raW5nIHVwIHRoZSBwcm9wZXJ0eSBvbiB0aGUgb3V0ZXIgc3ltYm9sLlxuICAgKlxuICAgKiBAcGFyYW0gc3ltYm9sIHRoZSBjbGFzcyB3aG9zZSBwcm9wZXJ0eSB3ZSBhcmUgaW50ZXJlc3RlZCBpbi5cbiAgICogQHBhcmFtIHByb3BlcnR5TmFtZSB0aGUgbmFtZSBvZiBzdGF0aWMgcHJvcGVydHkuXG4gICAqIEByZXR1cm5zIHRoZSBzeW1ib2wgaWYgaXQgaXMgZm91bmQgb3IgYHVuZGVmaW5lZGAgaWYgbm90LlxuICAgKi9cbiAgcHJvdGVjdGVkIGdldFN0YXRpY1Byb3BlcnR5KHN5bWJvbDogTmdjY0NsYXNzU3ltYm9sLCBwcm9wZXJ0eU5hbWU6IHRzLl9fU3RyaW5nKTogdHMuU3ltYm9sXG4gICAgICB8dW5kZWZpbmVkIHtcbiAgICByZXR1cm4gc3ltYm9sLmltcGxlbWVudGF0aW9uLmV4cG9ydHM/LmdldChwcm9wZXJ0eU5hbWUpIHx8XG4gICAgICAgIHN5bWJvbC5hZGphY2VudD8uZXhwb3J0cz8uZ2V0KHByb3BlcnR5TmFtZSkgfHxcbiAgICAgICAgc3ltYm9sLmRlY2xhcmF0aW9uLmV4cG9ydHM/LmdldChwcm9wZXJ0eU5hbWUpO1xuICB9XG5cbiAgLyoqXG4gICAqIFRoaXMgaXMgdGhlIG1haW4gZW50cnktcG9pbnQgZm9yIG9idGFpbmluZyBpbmZvcm1hdGlvbiBvbiB0aGUgZGVjb3JhdG9ycyBvZiBhIGdpdmVuIGNsYXNzLiBUaGlzXG4gICAqIGluZm9ybWF0aW9uIGlzIGNvbXB1dGVkIGVpdGhlciBmcm9tIHN0YXRpYyBwcm9wZXJ0aWVzIGlmIHByZXNlbnQsIG9yIHVzaW5nIGB0c2xpYi5fX2RlY29yYXRlYFxuICAgKiBoZWxwZXIgY2FsbHMgb3RoZXJ3aXNlLiBUaGUgY29tcHV0ZWQgcmVzdWx0IGlzIGNhY2hlZCBwZXIgY2xhc3MuXG4gICAqXG4gICAqIEBwYXJhbSBjbGFzc1N5bWJvbCB0aGUgY2xhc3MgZm9yIHdoaWNoIGRlY29yYXRvcnMgc2hvdWxkIGJlIGFjcXVpcmVkLlxuICAgKiBAcmV0dXJucyBhbGwgaW5mb3JtYXRpb24gb2YgdGhlIGRlY29yYXRvcnMgb24gdGhlIGNsYXNzLlxuICAgKi9cbiAgcHJvdGVjdGVkIGFjcXVpcmVEZWNvcmF0b3JJbmZvKGNsYXNzU3ltYm9sOiBOZ2NjQ2xhc3NTeW1ib2wpOiBEZWNvcmF0b3JJbmZvIHtcbiAgICBjb25zdCBkZWNsID0gY2xhc3NTeW1ib2wuZGVjbGFyYXRpb24udmFsdWVEZWNsYXJhdGlvbjtcbiAgICBpZiAodGhpcy5kZWNvcmF0b3JDYWNoZS5oYXMoZGVjbCkpIHtcbiAgICAgIHJldHVybiB0aGlzLmRlY29yYXRvckNhY2hlLmdldChkZWNsKSE7XG4gICAgfVxuXG4gICAgLy8gRXh0cmFjdCBkZWNvcmF0b3JzIGZyb20gc3RhdGljIHByb3BlcnRpZXMgYW5kIGBfX2RlY29yYXRlYCBoZWxwZXIgY2FsbHMsIHRoZW4gbWVyZ2UgdGhlbVxuICAgIC8vIHRvZ2V0aGVyIHdoZXJlIHRoZSBpbmZvcm1hdGlvbiBmcm9tIHRoZSBzdGF0aWMgcHJvcGVydGllcyBpcyBwcmVmZXJyZWQuXG4gICAgY29uc3Qgc3RhdGljUHJvcHMgPSB0aGlzLmNvbXB1dGVEZWNvcmF0b3JJbmZvRnJvbVN0YXRpY1Byb3BlcnRpZXMoY2xhc3NTeW1ib2wpO1xuICAgIGNvbnN0IGhlbHBlckNhbGxzID0gdGhpcy5jb21wdXRlRGVjb3JhdG9ySW5mb0Zyb21IZWxwZXJDYWxscyhjbGFzc1N5bWJvbCk7XG5cbiAgICBjb25zdCBkZWNvcmF0b3JJbmZvOiBEZWNvcmF0b3JJbmZvID0ge1xuICAgICAgY2xhc3NEZWNvcmF0b3JzOiBzdGF0aWNQcm9wcy5jbGFzc0RlY29yYXRvcnMgfHwgaGVscGVyQ2FsbHMuY2xhc3NEZWNvcmF0b3JzLFxuICAgICAgbWVtYmVyRGVjb3JhdG9yczogc3RhdGljUHJvcHMubWVtYmVyRGVjb3JhdG9ycyB8fCBoZWxwZXJDYWxscy5tZW1iZXJEZWNvcmF0b3JzLFxuICAgICAgY29uc3RydWN0b3JQYXJhbUluZm86IHN0YXRpY1Byb3BzLmNvbnN0cnVjdG9yUGFyYW1JbmZvIHx8IGhlbHBlckNhbGxzLmNvbnN0cnVjdG9yUGFyYW1JbmZvLFxuICAgIH07XG5cbiAgICB0aGlzLmRlY29yYXRvckNhY2hlLnNldChkZWNsLCBkZWNvcmF0b3JJbmZvKTtcbiAgICByZXR1cm4gZGVjb3JhdG9ySW5mbztcbiAgfVxuXG4gIC8qKlxuICAgKiBBdHRlbXB0cyB0byBjb21wdXRlIGRlY29yYXRvciBpbmZvcm1hdGlvbiBmcm9tIHN0YXRpYyBwcm9wZXJ0aWVzIFwiZGVjb3JhdG9yc1wiLCBcInByb3BEZWNvcmF0b3JzXCJcbiAgICogYW5kIFwiY3RvclBhcmFtZXRlcnNcIiBvbiB0aGUgY2xhc3MuIElmIG5laXRoZXIgb2YgdGhlc2Ugc3RhdGljIHByb3BlcnRpZXMgaXMgcHJlc2VudCB0aGVcbiAgICogbGlicmFyeSBpcyBsaWtlbHkgbm90IGNvbXBpbGVkIHVzaW5nIHRzaWNrbGUgZm9yIHVzYWdlIHdpdGggQ2xvc3VyZSBjb21waWxlciwgaW4gd2hpY2ggY2FzZVxuICAgKiBgbnVsbGAgaXMgcmV0dXJuZWQuXG4gICAqXG4gICAqIEBwYXJhbSBjbGFzc1N5bWJvbCBUaGUgY2xhc3Mgc3ltYm9sIHRvIGNvbXB1dGUgdGhlIGRlY29yYXRvcnMgaW5mb3JtYXRpb24gZm9yLlxuICAgKiBAcmV0dXJucyBBbGwgaW5mb3JtYXRpb24gb24gdGhlIGRlY29yYXRvcnMgYXMgZXh0cmFjdGVkIGZyb20gc3RhdGljIHByb3BlcnRpZXMsIG9yIGBudWxsYCBpZlxuICAgKiBub25lIG9mIHRoZSBzdGF0aWMgcHJvcGVydGllcyBleGlzdC5cbiAgICovXG4gIHByb3RlY3RlZCBjb21wdXRlRGVjb3JhdG9ySW5mb0Zyb21TdGF0aWNQcm9wZXJ0aWVzKGNsYXNzU3ltYm9sOiBOZ2NjQ2xhc3NTeW1ib2wpOiB7XG4gICAgY2xhc3NEZWNvcmF0b3JzOiBEZWNvcmF0b3JbXXxudWxsOyBtZW1iZXJEZWNvcmF0b3JzOiBNYXA8c3RyaW5nLCBEZWNvcmF0b3JbXT58IG51bGw7XG4gICAgY29uc3RydWN0b3JQYXJhbUluZm86IFBhcmFtSW5mb1tdIHwgbnVsbDtcbiAgfSB7XG4gICAgbGV0IGNsYXNzRGVjb3JhdG9yczogRGVjb3JhdG9yW118bnVsbCA9IG51bGw7XG4gICAgbGV0IG1lbWJlckRlY29yYXRvcnM6IE1hcDxzdHJpbmcsIERlY29yYXRvcltdPnxudWxsID0gbnVsbDtcbiAgICBsZXQgY29uc3RydWN0b3JQYXJhbUluZm86IFBhcmFtSW5mb1tdfG51bGwgPSBudWxsO1xuXG4gICAgY29uc3QgZGVjb3JhdG9yc1Byb3BlcnR5ID0gdGhpcy5nZXRTdGF0aWNQcm9wZXJ0eShjbGFzc1N5bWJvbCwgREVDT1JBVE9SUyk7XG4gICAgaWYgKGRlY29yYXRvcnNQcm9wZXJ0eSAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICBjbGFzc0RlY29yYXRvcnMgPSB0aGlzLmdldENsYXNzRGVjb3JhdG9yc0Zyb21TdGF0aWNQcm9wZXJ0eShkZWNvcmF0b3JzUHJvcGVydHkpO1xuICAgIH1cblxuICAgIGNvbnN0IHByb3BEZWNvcmF0b3JzUHJvcGVydHkgPSB0aGlzLmdldFN0YXRpY1Byb3BlcnR5KGNsYXNzU3ltYm9sLCBQUk9QX0RFQ09SQVRPUlMpO1xuICAgIGlmIChwcm9wRGVjb3JhdG9yc1Byb3BlcnR5ICE9PSB1bmRlZmluZWQpIHtcbiAgICAgIG1lbWJlckRlY29yYXRvcnMgPSB0aGlzLmdldE1lbWJlckRlY29yYXRvcnNGcm9tU3RhdGljUHJvcGVydHkocHJvcERlY29yYXRvcnNQcm9wZXJ0eSk7XG4gICAgfVxuXG4gICAgY29uc3QgY29uc3RydWN0b3JQYXJhbXNQcm9wZXJ0eSA9IHRoaXMuZ2V0U3RhdGljUHJvcGVydHkoY2xhc3NTeW1ib2wsIENPTlNUUlVDVE9SX1BBUkFNUyk7XG4gICAgaWYgKGNvbnN0cnVjdG9yUGFyYW1zUHJvcGVydHkgIT09IHVuZGVmaW5lZCkge1xuICAgICAgY29uc3RydWN0b3JQYXJhbUluZm8gPSB0aGlzLmdldFBhcmFtSW5mb0Zyb21TdGF0aWNQcm9wZXJ0eShjb25zdHJ1Y3RvclBhcmFtc1Byb3BlcnR5KTtcbiAgICB9XG5cbiAgICByZXR1cm4ge2NsYXNzRGVjb3JhdG9ycywgbWVtYmVyRGVjb3JhdG9ycywgY29uc3RydWN0b3JQYXJhbUluZm99O1xuICB9XG5cbiAgLyoqXG4gICAqIEdldCBhbGwgY2xhc3MgZGVjb3JhdG9ycyBmb3IgdGhlIGdpdmVuIGNsYXNzLCB3aGVyZSB0aGUgZGVjb3JhdG9ycyBhcmUgZGVjbGFyZWRcbiAgICogdmlhIGEgc3RhdGljIHByb3BlcnR5LiBGb3IgZXhhbXBsZTpcbiAgICpcbiAgICogYGBgXG4gICAqIGNsYXNzIFNvbWVEaXJlY3RpdmUge31cbiAgICogU29tZURpcmVjdGl2ZS5kZWNvcmF0b3JzID0gW1xuICAgKiAgIHsgdHlwZTogRGlyZWN0aXZlLCBhcmdzOiBbeyBzZWxlY3RvcjogJ1tzb21lRGlyZWN0aXZlXScgfSxdIH1cbiAgICogXTtcbiAgICogYGBgXG4gICAqXG4gICAqIEBwYXJhbSBkZWNvcmF0b3JzU3ltYm9sIHRoZSBwcm9wZXJ0eSBjb250YWluaW5nIHRoZSBkZWNvcmF0b3JzIHdlIHdhbnQgdG8gZ2V0LlxuICAgKiBAcmV0dXJucyBhbiBhcnJheSBvZiBkZWNvcmF0b3JzIG9yIG51bGwgaWYgbm9uZSB3aGVyZSBmb3VuZC5cbiAgICovXG4gIHByb3RlY3RlZCBnZXRDbGFzc0RlY29yYXRvcnNGcm9tU3RhdGljUHJvcGVydHkoZGVjb3JhdG9yc1N5bWJvbDogdHMuU3ltYm9sKTogRGVjb3JhdG9yW118bnVsbCB7XG4gICAgY29uc3QgZGVjb3JhdG9yc0lkZW50aWZpZXIgPSBkZWNvcmF0b3JzU3ltYm9sLnZhbHVlRGVjbGFyYXRpb247XG4gICAgaWYgKGRlY29yYXRvcnNJZGVudGlmaWVyICYmIGRlY29yYXRvcnNJZGVudGlmaWVyLnBhcmVudCkge1xuICAgICAgaWYgKHRzLmlzQmluYXJ5RXhwcmVzc2lvbihkZWNvcmF0b3JzSWRlbnRpZmllci5wYXJlbnQpICYmXG4gICAgICAgICAgZGVjb3JhdG9yc0lkZW50aWZpZXIucGFyZW50Lm9wZXJhdG9yVG9rZW4ua2luZCA9PT0gdHMuU3ludGF4S2luZC5FcXVhbHNUb2tlbikge1xuICAgICAgICAvLyBBU1Qgb2YgdGhlIGFycmF5IG9mIGRlY29yYXRvciB2YWx1ZXNcbiAgICAgICAgY29uc3QgZGVjb3JhdG9yc0FycmF5ID0gZGVjb3JhdG9yc0lkZW50aWZpZXIucGFyZW50LnJpZ2h0O1xuICAgICAgICByZXR1cm4gdGhpcy5yZWZsZWN0RGVjb3JhdG9ycyhkZWNvcmF0b3JzQXJyYXkpXG4gICAgICAgICAgICAuZmlsdGVyKGRlY29yYXRvciA9PiB0aGlzLmlzRnJvbUNvcmUoZGVjb3JhdG9yKSk7XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiBudWxsO1xuICB9XG5cbiAgLyoqXG4gICAqIEV4YW1pbmUgYSBzeW1ib2wgd2hpY2ggc2hvdWxkIGJlIG9mIGEgY2xhc3MsIGFuZCByZXR1cm4gbWV0YWRhdGEgYWJvdXQgaXRzIG1lbWJlcnMuXG4gICAqXG4gICAqIEBwYXJhbSBzeW1ib2wgdGhlIGBDbGFzc1N5bWJvbGAgcmVwcmVzZW50aW5nIHRoZSBjbGFzcyBvdmVyIHdoaWNoIHRvIHJlZmxlY3QuXG4gICAqIEByZXR1cm5zIGFuIGFycmF5IG9mIGBDbGFzc01lbWJlcmAgbWV0YWRhdGEgcmVwcmVzZW50aW5nIHRoZSBtZW1iZXJzIG9mIHRoZSBjbGFzcy5cbiAgICovXG4gIHByb3RlY3RlZCBnZXRNZW1iZXJzT2ZTeW1ib2woc3ltYm9sOiBOZ2NjQ2xhc3NTeW1ib2wpOiBDbGFzc01lbWJlcltdIHtcbiAgICBjb25zdCBtZW1iZXJzOiBDbGFzc01lbWJlcltdID0gW107XG5cbiAgICAvLyBUaGUgZGVjb3JhdG9ycyBtYXAgY29udGFpbnMgYWxsIHRoZSBwcm9wZXJ0aWVzIHRoYXQgYXJlIGRlY29yYXRlZFxuICAgIGNvbnN0IHttZW1iZXJEZWNvcmF0b3JzfSA9IHRoaXMuYWNxdWlyZURlY29yYXRvckluZm8oc3ltYm9sKTtcblxuICAgIC8vIE1ha2UgYSBjb3B5IG9mIHRoZSBkZWNvcmF0b3JzIGFzIHN1Y2Nlc3NmdWxseSByZWZsZWN0ZWQgbWVtYmVycyBkZWxldGUgdGhlbXNlbHZlcyBmcm9tIHRoZVxuICAgIC8vIG1hcCwgc28gdGhhdCBhbnkgbGVmdG92ZXJzIGNhbiBiZSBlYXNpbHkgZGVhbHQgd2l0aC5cbiAgICBjb25zdCBkZWNvcmF0b3JzTWFwID0gbmV3IE1hcChtZW1iZXJEZWNvcmF0b3JzKTtcblxuICAgIC8vIFRoZSBtZW1iZXIgbWFwIGNvbnRhaW5zIGFsbCB0aGUgbWV0aG9kIChpbnN0YW5jZSBhbmQgc3RhdGljKTsgYW5kIGFueSBpbnN0YW5jZSBwcm9wZXJ0aWVzXG4gICAgLy8gdGhhdCBhcmUgaW5pdGlhbGl6ZWQgaW4gdGhlIGNsYXNzLlxuICAgIGlmIChzeW1ib2wuaW1wbGVtZW50YXRpb24ubWVtYmVycykge1xuICAgICAgc3ltYm9sLmltcGxlbWVudGF0aW9uLm1lbWJlcnMuZm9yRWFjaCgodmFsdWUsIGtleSkgPT4ge1xuICAgICAgICBjb25zdCBkZWNvcmF0b3JzID0gZGVjb3JhdG9yc01hcC5nZXQoa2V5IGFzIHN0cmluZyk7XG4gICAgICAgIGNvbnN0IHJlZmxlY3RlZE1lbWJlcnMgPSB0aGlzLnJlZmxlY3RNZW1iZXJzKHZhbHVlLCBkZWNvcmF0b3JzKTtcbiAgICAgICAgaWYgKHJlZmxlY3RlZE1lbWJlcnMpIHtcbiAgICAgICAgICBkZWNvcmF0b3JzTWFwLmRlbGV0ZShrZXkgYXMgc3RyaW5nKTtcbiAgICAgICAgICBtZW1iZXJzLnB1c2goLi4ucmVmbGVjdGVkTWVtYmVycyk7XG4gICAgICAgIH1cbiAgICAgIH0pO1xuICAgIH1cblxuICAgIC8vIFRoZSBzdGF0aWMgcHJvcGVydHkgbWFwIGNvbnRhaW5zIGFsbCB0aGUgc3RhdGljIHByb3BlcnRpZXNcbiAgICBpZiAoc3ltYm9sLmltcGxlbWVudGF0aW9uLmV4cG9ydHMpIHtcbiAgICAgIHN5bWJvbC5pbXBsZW1lbnRhdGlvbi5leHBvcnRzLmZvckVhY2goKHZhbHVlLCBrZXkpID0+IHtcbiAgICAgICAgY29uc3QgZGVjb3JhdG9ycyA9IGRlY29yYXRvcnNNYXAuZ2V0KGtleSBhcyBzdHJpbmcpO1xuICAgICAgICBjb25zdCByZWZsZWN0ZWRNZW1iZXJzID0gdGhpcy5yZWZsZWN0TWVtYmVycyh2YWx1ZSwgZGVjb3JhdG9ycywgdHJ1ZSk7XG4gICAgICAgIGlmIChyZWZsZWN0ZWRNZW1iZXJzKSB7XG4gICAgICAgICAgZGVjb3JhdG9yc01hcC5kZWxldGUoa2V5IGFzIHN0cmluZyk7XG4gICAgICAgICAgbWVtYmVycy5wdXNoKC4uLnJlZmxlY3RlZE1lbWJlcnMpO1xuICAgICAgICB9XG4gICAgICB9KTtcbiAgICB9XG5cbiAgICAvLyBJZiB0aGlzIGNsYXNzIHdhcyBkZWNsYXJlZCBhcyBhIFZhcmlhYmxlRGVjbGFyYXRpb24gdGhlbiBpdCBtYXkgaGF2ZSBzdGF0aWMgcHJvcGVydGllc1xuICAgIC8vIGF0dGFjaGVkIHRvIHRoZSB2YXJpYWJsZSByYXRoZXIgdGhhbiB0aGUgY2xhc3MgaXRzZWxmXG4gICAgLy8gRm9yIGV4YW1wbGU6XG4gICAgLy8gYGBgXG4gICAgLy8gbGV0IE15Q2xhc3MgPSBjbGFzcyBNeUNsYXNzIHtcbiAgICAvLyAgIC8vIG5vIHN0YXRpYyBwcm9wZXJ0aWVzIGhlcmUhXG4gICAgLy8gfVxuICAgIC8vIE15Q2xhc3Muc3RhdGljUHJvcGVydHkgPSAuLi47XG4gICAgLy8gYGBgXG4gICAgaWYgKHRzLmlzVmFyaWFibGVEZWNsYXJhdGlvbihzeW1ib2wuZGVjbGFyYXRpb24udmFsdWVEZWNsYXJhdGlvbikpIHtcbiAgICAgIGlmIChzeW1ib2wuZGVjbGFyYXRpb24uZXhwb3J0cykge1xuICAgICAgICBzeW1ib2wuZGVjbGFyYXRpb24uZXhwb3J0cy5mb3JFYWNoKCh2YWx1ZSwga2V5KSA9PiB7XG4gICAgICAgICAgY29uc3QgZGVjb3JhdG9ycyA9IGRlY29yYXRvcnNNYXAuZ2V0KGtleSBhcyBzdHJpbmcpO1xuICAgICAgICAgIGNvbnN0IHJlZmxlY3RlZE1lbWJlcnMgPSB0aGlzLnJlZmxlY3RNZW1iZXJzKHZhbHVlLCBkZWNvcmF0b3JzLCB0cnVlKTtcbiAgICAgICAgICBpZiAocmVmbGVjdGVkTWVtYmVycykge1xuICAgICAgICAgICAgZGVjb3JhdG9yc01hcC5kZWxldGUoa2V5IGFzIHN0cmluZyk7XG4gICAgICAgICAgICBtZW1iZXJzLnB1c2goLi4ucmVmbGVjdGVkTWVtYmVycyk7XG4gICAgICAgICAgfVxuICAgICAgICB9KTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICAvLyBJZiB0aGlzIGNsYXNzIHdhcyBkZWNsYXJlZCBhcyBhIFZhcmlhYmxlRGVjbGFyYXRpb24gaW5zaWRlIGFuIElJRkUsIHRoZW4gaXQgbWF5IGhhdmUgc3RhdGljXG4gICAgLy8gcHJvcGVydGllcyBhdHRhY2hlZCB0byB0aGUgdmFyaWFibGUgcmF0aGVyIHRoYW4gdGhlIGNsYXNzIGl0c2VsZi5cbiAgICAvL1xuICAgIC8vIEZvciBleGFtcGxlOlxuICAgIC8vIGBgYFxuICAgIC8vIGxldCBPdXRlckNsYXNzID0gKCgpID0+IHtcbiAgICAvLyAgIGxldCBBZGphY2VudENsYXNzID0gY2xhc3MgSW50ZXJuYWxDbGFzcyB7XG4gICAgLy8gICAgIC8vIG5vIHN0YXRpYyBwcm9wZXJ0aWVzIGhlcmUhXG4gICAgLy8gICB9XG4gICAgLy8gICBBZGphY2VudENsYXNzLnN0YXRpY1Byb3BlcnR5ID0gLi4uO1xuICAgIC8vIH0pKCk7XG4gICAgLy8gYGBgXG4gICAgaWYgKHN5bWJvbC5hZGphY2VudCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICBpZiAodHMuaXNWYXJpYWJsZURlY2xhcmF0aW9uKHN5bWJvbC5hZGphY2VudC52YWx1ZURlY2xhcmF0aW9uKSkge1xuICAgICAgICBpZiAoc3ltYm9sLmFkamFjZW50LmV4cG9ydHMgIT09IHVuZGVmaW5lZCkge1xuICAgICAgICAgIHN5bWJvbC5hZGphY2VudC5leHBvcnRzLmZvckVhY2goKHZhbHVlLCBrZXkpID0+IHtcbiAgICAgICAgICAgIGNvbnN0IGRlY29yYXRvcnMgPSBkZWNvcmF0b3JzTWFwLmdldChrZXkgYXMgc3RyaW5nKTtcbiAgICAgICAgICAgIGNvbnN0IHJlZmxlY3RlZE1lbWJlcnMgPSB0aGlzLnJlZmxlY3RNZW1iZXJzKHZhbHVlLCBkZWNvcmF0b3JzLCB0cnVlKTtcbiAgICAgICAgICAgIGlmIChyZWZsZWN0ZWRNZW1iZXJzKSB7XG4gICAgICAgICAgICAgIGRlY29yYXRvcnNNYXAuZGVsZXRlKGtleSBhcyBzdHJpbmcpO1xuICAgICAgICAgICAgICBtZW1iZXJzLnB1c2goLi4ucmVmbGVjdGVkTWVtYmVycyk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG5cbiAgICAvLyBEZWFsIHdpdGggYW55IGRlY29yYXRlZCBwcm9wZXJ0aWVzIHRoYXQgd2VyZSBub3QgaW5pdGlhbGl6ZWQgaW4gdGhlIGNsYXNzXG4gICAgZGVjb3JhdG9yc01hcC5mb3JFYWNoKCh2YWx1ZSwga2V5KSA9PiB7XG4gICAgICBtZW1iZXJzLnB1c2goe1xuICAgICAgICBpbXBsZW1lbnRhdGlvbjogbnVsbCxcbiAgICAgICAgZGVjb3JhdG9yczogdmFsdWUsXG4gICAgICAgIGlzU3RhdGljOiBmYWxzZSxcbiAgICAgICAga2luZDogQ2xhc3NNZW1iZXJLaW5kLlByb3BlcnR5LFxuICAgICAgICBuYW1lOiBrZXksXG4gICAgICAgIG5hbWVOb2RlOiBudWxsLFxuICAgICAgICBub2RlOiBudWxsLFxuICAgICAgICB0eXBlOiBudWxsLFxuICAgICAgICB2YWx1ZTogbnVsbFxuICAgICAgfSk7XG4gICAgfSk7XG5cbiAgICByZXR1cm4gbWVtYmVycztcbiAgfVxuXG4gIC8qKlxuICAgKiBNZW1iZXIgZGVjb3JhdG9ycyBtYXkgYmUgZGVjbGFyZWQgYXMgc3RhdGljIHByb3BlcnRpZXMgb2YgdGhlIGNsYXNzOlxuICAgKlxuICAgKiBgYGBcbiAgICogU29tZURpcmVjdGl2ZS5wcm9wRGVjb3JhdG9ycyA9IHtcbiAgICogICBcIm5nRm9yT2ZcIjogW3sgdHlwZTogSW5wdXQgfSxdLFxuICAgKiAgIFwibmdGb3JUcmFja0J5XCI6IFt7IHR5cGU6IElucHV0IH0sXSxcbiAgICogICBcIm5nRm9yVGVtcGxhdGVcIjogW3sgdHlwZTogSW5wdXQgfSxdLFxuICAgKiB9O1xuICAgKiBgYGBcbiAgICpcbiAgICogQHBhcmFtIGRlY29yYXRvcnNQcm9wZXJ0eSB0aGUgY2xhc3Mgd2hvc2UgbWVtYmVyIGRlY29yYXRvcnMgd2UgYXJlIGludGVyZXN0ZWQgaW4uXG4gICAqIEByZXR1cm5zIGEgbWFwIHdob3NlIGtleXMgYXJlIHRoZSBuYW1lIG9mIHRoZSBtZW1iZXJzIGFuZCB3aG9zZSB2YWx1ZXMgYXJlIGNvbGxlY3Rpb25zIG9mXG4gICAqIGRlY29yYXRvcnMgZm9yIHRoZSBnaXZlbiBtZW1iZXIuXG4gICAqL1xuICBwcm90ZWN0ZWQgZ2V0TWVtYmVyRGVjb3JhdG9yc0Zyb21TdGF0aWNQcm9wZXJ0eShkZWNvcmF0b3JzUHJvcGVydHk6IHRzLlN5bWJvbCk6XG4gICAgICBNYXA8c3RyaW5nLCBEZWNvcmF0b3JbXT4ge1xuICAgIGNvbnN0IG1lbWJlckRlY29yYXRvcnMgPSBuZXcgTWFwPHN0cmluZywgRGVjb3JhdG9yW10+KCk7XG4gICAgLy8gU3ltYm9sIG9mIHRoZSBpZGVudGlmaWVyIGZvciBgU29tZURpcmVjdGl2ZS5wcm9wRGVjb3JhdG9yc2AuXG4gICAgY29uc3QgcHJvcERlY29yYXRvcnNNYXAgPSBnZXRQcm9wZXJ0eVZhbHVlRnJvbVN5bWJvbChkZWNvcmF0b3JzUHJvcGVydHkpO1xuICAgIGlmIChwcm9wRGVjb3JhdG9yc01hcCAmJiB0cy5pc09iamVjdExpdGVyYWxFeHByZXNzaW9uKHByb3BEZWNvcmF0b3JzTWFwKSkge1xuICAgICAgY29uc3QgcHJvcGVydGllc01hcCA9IHJlZmxlY3RPYmplY3RMaXRlcmFsKHByb3BEZWNvcmF0b3JzTWFwKTtcbiAgICAgIHByb3BlcnRpZXNNYXAuZm9yRWFjaCgodmFsdWUsIG5hbWUpID0+IHtcbiAgICAgICAgY29uc3QgZGVjb3JhdG9ycyA9XG4gICAgICAgICAgICB0aGlzLnJlZmxlY3REZWNvcmF0b3JzKHZhbHVlKS5maWx0ZXIoZGVjb3JhdG9yID0+IHRoaXMuaXNGcm9tQ29yZShkZWNvcmF0b3IpKTtcbiAgICAgICAgaWYgKGRlY29yYXRvcnMubGVuZ3RoKSB7XG4gICAgICAgICAgbWVtYmVyRGVjb3JhdG9ycy5zZXQobmFtZSwgZGVjb3JhdG9ycyk7XG4gICAgICAgIH1cbiAgICAgIH0pO1xuICAgIH1cbiAgICByZXR1cm4gbWVtYmVyRGVjb3JhdG9ycztcbiAgfVxuXG4gIC8qKlxuICAgKiBGb3IgYSBnaXZlbiBjbGFzcyBzeW1ib2wsIGNvbGxlY3RzIGFsbCBkZWNvcmF0b3IgaW5mb3JtYXRpb24gZnJvbSB0c2xpYiBoZWxwZXIgbWV0aG9kcywgYXNcbiAgICogZ2VuZXJhdGVkIGJ5IFR5cGVTY3JpcHQgaW50byBlbWl0dGVkIEphdmFTY3JpcHQgZmlsZXMuXG4gICAqXG4gICAqIENsYXNzIGRlY29yYXRvcnMgYXJlIGV4dHJhY3RlZCBmcm9tIGNhbGxzIHRvIGB0c2xpYi5fX2RlY29yYXRlYCB0aGF0IGxvb2sgYXMgZm9sbG93czpcbiAgICpcbiAgICogYGBgXG4gICAqIGxldCBTb21lRGlyZWN0aXZlID0gY2xhc3MgU29tZURpcmVjdGl2ZSB7fVxuICAgKiBTb21lRGlyZWN0aXZlID0gX19kZWNvcmF0ZShbXG4gICAqICAgRGlyZWN0aXZlKHsgc2VsZWN0b3I6ICdbc29tZURpcmVjdGl2ZV0nIH0pLFxuICAgKiBdLCBTb21lRGlyZWN0aXZlKTtcbiAgICogYGBgXG4gICAqXG4gICAqIFRoZSBleHRyYWN0aW9uIG9mIG1lbWJlciBkZWNvcmF0b3JzIGlzIHNpbWlsYXIsIHdpdGggdGhlIGRpc3RpbmN0aW9uIHRoYXQgaXRzIDJuZCBhbmQgM3JkXG4gICAqIGFyZ3VtZW50IGNvcnJlc3BvbmQgd2l0aCBhIFwicHJvdG90eXBlXCIgdGFyZ2V0IGFuZCB0aGUgbmFtZSBvZiB0aGUgbWVtYmVyIHRvIHdoaWNoIHRoZVxuICAgKiBkZWNvcmF0b3JzIGFwcGx5LlxuICAgKlxuICAgKiBgYGBcbiAgICogX19kZWNvcmF0ZShbXG4gICAqICAgICBJbnB1dCgpLFxuICAgKiAgICAgX19tZXRhZGF0YShcImRlc2lnbjp0eXBlXCIsIFN0cmluZylcbiAgICogXSwgU29tZURpcmVjdGl2ZS5wcm90b3R5cGUsIFwiaW5wdXQxXCIsIHZvaWQgMCk7XG4gICAqIGBgYFxuICAgKlxuICAgKiBAcGFyYW0gY2xhc3NTeW1ib2wgVGhlIGNsYXNzIHN5bWJvbCBmb3Igd2hpY2ggZGVjb3JhdG9ycyBzaG91bGQgYmUgZXh0cmFjdGVkLlxuICAgKiBAcmV0dXJucyBBbGwgaW5mb3JtYXRpb24gb24gdGhlIGRlY29yYXRvcnMgb2YgdGhlIGNsYXNzLlxuICAgKi9cbiAgcHJvdGVjdGVkIGNvbXB1dGVEZWNvcmF0b3JJbmZvRnJvbUhlbHBlckNhbGxzKGNsYXNzU3ltYm9sOiBOZ2NjQ2xhc3NTeW1ib2wpOiBEZWNvcmF0b3JJbmZvIHtcbiAgICBsZXQgY2xhc3NEZWNvcmF0b3JzOiBEZWNvcmF0b3JbXXxudWxsID0gbnVsbDtcbiAgICBjb25zdCBtZW1iZXJEZWNvcmF0b3JzID0gbmV3IE1hcDxzdHJpbmcsIERlY29yYXRvcltdPigpO1xuICAgIGNvbnN0IGNvbnN0cnVjdG9yUGFyYW1JbmZvOiBQYXJhbUluZm9bXSA9IFtdO1xuXG4gICAgY29uc3QgZ2V0Q29uc3RydWN0b3JQYXJhbUluZm8gPSAoaW5kZXg6IG51bWJlcikgPT4ge1xuICAgICAgbGV0IHBhcmFtID0gY29uc3RydWN0b3JQYXJhbUluZm9baW5kZXhdO1xuICAgICAgaWYgKHBhcmFtID09PSB1bmRlZmluZWQpIHtcbiAgICAgICAgcGFyYW0gPSBjb25zdHJ1Y3RvclBhcmFtSW5mb1tpbmRleF0gPSB7ZGVjb3JhdG9yczogbnVsbCwgdHlwZUV4cHJlc3Npb246IG51bGx9O1xuICAgICAgfVxuICAgICAgcmV0dXJuIHBhcmFtO1xuICAgIH07XG5cbiAgICAvLyBBbGwgcmVsZXZhbnQgaW5mb3JtYXRpb24gY2FuIGJlIGV4dHJhY3RlZCBmcm9tIGNhbGxzIHRvIGBfX2RlY29yYXRlYCwgb2J0YWluIHRoZXNlIGZpcnN0LlxuICAgIC8vIE5vdGUgdGhhdCBhbHRob3VnaCB0aGUgaGVscGVyIGNhbGxzIGFyZSByZXRyaWV2ZWQgdXNpbmcgdGhlIGNsYXNzIHN5bWJvbCwgdGhlIHJlc3VsdCBtYXlcbiAgICAvLyBjb250YWluIGhlbHBlciBjYWxscyBjb3JyZXNwb25kaW5nIHdpdGggdW5yZWxhdGVkIGNsYXNzZXMuIFRoZXJlZm9yZSwgZWFjaCBoZWxwZXIgY2FsbCBzdGlsbFxuICAgIC8vIGhhcyB0byBiZSBjaGVja2VkIHRvIGFjdHVhbGx5IGNvcnJlc3BvbmQgd2l0aCB0aGUgY2xhc3Mgc3ltYm9sLlxuICAgIGNvbnN0IGhlbHBlckNhbGxzID0gdGhpcy5nZXRIZWxwZXJDYWxsc0ZvckNsYXNzKGNsYXNzU3ltYm9sLCBbJ19fZGVjb3JhdGUnXSk7XG5cbiAgICBjb25zdCBvdXRlckRlY2xhcmF0aW9uID0gY2xhc3NTeW1ib2wuZGVjbGFyYXRpb24udmFsdWVEZWNsYXJhdGlvbjtcbiAgICBjb25zdCBpbm5lckRlY2xhcmF0aW9uID0gY2xhc3NTeW1ib2wuaW1wbGVtZW50YXRpb24udmFsdWVEZWNsYXJhdGlvbjtcbiAgICBjb25zdCBhZGphY2VudERlY2xhcmF0aW9uID1cbiAgICAgICAgdGhpcy5nZXRBZGphY2VudE5hbWVPZkNsYXNzKChjbGFzc1N5bWJvbC5kZWNsYXJhdGlvbi52YWx1ZURlY2xhcmF0aW9uKSkucGFyZW50O1xuICAgIGNvbnN0IG1hdGNoZXNDbGFzcyA9IChpZGVudGlmaWVyOiB0cy5JZGVudGlmaWVyKSA9PiB7XG4gICAgICBjb25zdCBkZWNsID0gdGhpcy5nZXREZWNsYXJhdGlvbk9mSWRlbnRpZmllcihpZGVudGlmaWVyKTtcbiAgICAgIHJldHVybiBkZWNsICE9PSBudWxsICYmXG4gICAgICAgICAgKGRlY2wubm9kZSA9PT0gYWRqYWNlbnREZWNsYXJhdGlvbiB8fCBkZWNsLm5vZGUgPT09IG91dGVyRGVjbGFyYXRpb24gfHxcbiAgICAgICAgICAgZGVjbC5ub2RlID09PSBpbm5lckRlY2xhcmF0aW9uKTtcbiAgICB9O1xuXG4gICAgZm9yIChjb25zdCBoZWxwZXJDYWxsIG9mIGhlbHBlckNhbGxzKSB7XG4gICAgICBpZiAoaXNDbGFzc0RlY29yYXRlQ2FsbChoZWxwZXJDYWxsLCBtYXRjaGVzQ2xhc3MpKSB7XG4gICAgICAgIC8vIFRoaXMgYF9fZGVjb3JhdGVgIGNhbGwgaXMgdGFyZ2V0aW5nIHRoZSBjbGFzcyBpdHNlbGYuXG4gICAgICAgIGNvbnN0IGhlbHBlckFyZ3MgPSBoZWxwZXJDYWxsLmFyZ3VtZW50c1swXTtcblxuICAgICAgICBmb3IgKGNvbnN0IGVsZW1lbnQgb2YgaGVscGVyQXJncy5lbGVtZW50cykge1xuICAgICAgICAgIGNvbnN0IGVudHJ5ID0gdGhpcy5yZWZsZWN0RGVjb3JhdGVIZWxwZXJFbnRyeShlbGVtZW50KTtcbiAgICAgICAgICBpZiAoZW50cnkgPT09IG51bGwpIHtcbiAgICAgICAgICAgIGNvbnRpbnVlO1xuICAgICAgICAgIH1cblxuICAgICAgICAgIGlmIChlbnRyeS50eXBlID09PSAnZGVjb3JhdG9yJykge1xuICAgICAgICAgICAgLy8gVGhlIGhlbHBlciBhcmcgd2FzIHJlZmxlY3RlZCB0byByZXByZXNlbnQgYW4gYWN0dWFsIGRlY29yYXRvclxuICAgICAgICAgICAgaWYgKHRoaXMuaXNGcm9tQ29yZShlbnRyeS5kZWNvcmF0b3IpKSB7XG4gICAgICAgICAgICAgIChjbGFzc0RlY29yYXRvcnMgfHwgKGNsYXNzRGVjb3JhdG9ycyA9IFtdKSkucHVzaChlbnRyeS5kZWNvcmF0b3IpO1xuICAgICAgICAgICAgfVxuICAgICAgICAgIH0gZWxzZSBpZiAoZW50cnkudHlwZSA9PT0gJ3BhcmFtOmRlY29yYXRvcnMnKSB7XG4gICAgICAgICAgICAvLyBUaGUgaGVscGVyIGFyZyByZXByZXNlbnRzIGEgZGVjb3JhdG9yIGZvciBhIHBhcmFtZXRlci4gU2luY2UgaXQncyBhcHBsaWVkIHRvIHRoZVxuICAgICAgICAgICAgLy8gY2xhc3MsIGl0IGNvcnJlc3BvbmRzIHdpdGggYSBjb25zdHJ1Y3RvciBwYXJhbWV0ZXIgb2YgdGhlIGNsYXNzLlxuICAgICAgICAgICAgY29uc3QgcGFyYW0gPSBnZXRDb25zdHJ1Y3RvclBhcmFtSW5mbyhlbnRyeS5pbmRleCk7XG4gICAgICAgICAgICAocGFyYW0uZGVjb3JhdG9ycyB8fCAocGFyYW0uZGVjb3JhdG9ycyA9IFtdKSkucHVzaChlbnRyeS5kZWNvcmF0b3IpO1xuICAgICAgICAgIH0gZWxzZSBpZiAoZW50cnkudHlwZSA9PT0gJ3BhcmFtcycpIHtcbiAgICAgICAgICAgIC8vIFRoZSBoZWxwZXIgYXJnIHJlcHJlc2VudHMgdGhlIHR5cGVzIG9mIHRoZSBwYXJhbWV0ZXJzLiBTaW5jZSBpdCdzIGFwcGxpZWQgdG8gdGhlXG4gICAgICAgICAgICAvLyBjbGFzcywgaXQgY29ycmVzcG9uZHMgd2l0aCB0aGUgY29uc3RydWN0b3IgcGFyYW1ldGVycyBvZiB0aGUgY2xhc3MuXG4gICAgICAgICAgICBlbnRyeS50eXBlcy5mb3JFYWNoKFxuICAgICAgICAgICAgICAgICh0eXBlLCBpbmRleCkgPT4gZ2V0Q29uc3RydWN0b3JQYXJhbUluZm8oaW5kZXgpLnR5cGVFeHByZXNzaW9uID0gdHlwZSk7XG4gICAgICAgICAgfVxuICAgICAgICB9XG4gICAgICB9IGVsc2UgaWYgKGlzTWVtYmVyRGVjb3JhdGVDYWxsKGhlbHBlckNhbGwsIG1hdGNoZXNDbGFzcykpIHtcbiAgICAgICAgLy8gVGhlIGBfX2RlY29yYXRlYCBjYWxsIGlzIHRhcmdldGluZyBhIG1lbWJlciBvZiB0aGUgY2xhc3NcbiAgICAgICAgY29uc3QgaGVscGVyQXJncyA9IGhlbHBlckNhbGwuYXJndW1lbnRzWzBdO1xuICAgICAgICBjb25zdCBtZW1iZXJOYW1lID0gaGVscGVyQ2FsbC5hcmd1bWVudHNbMl0udGV4dDtcblxuICAgICAgICBmb3IgKGNvbnN0IGVsZW1lbnQgb2YgaGVscGVyQXJncy5lbGVtZW50cykge1xuICAgICAgICAgIGNvbnN0IGVudHJ5ID0gdGhpcy5yZWZsZWN0RGVjb3JhdGVIZWxwZXJFbnRyeShlbGVtZW50KTtcbiAgICAgICAgICBpZiAoZW50cnkgPT09IG51bGwpIHtcbiAgICAgICAgICAgIGNvbnRpbnVlO1xuICAgICAgICAgIH1cblxuICAgICAgICAgIGlmIChlbnRyeS50eXBlID09PSAnZGVjb3JhdG9yJykge1xuICAgICAgICAgICAgLy8gVGhlIGhlbHBlciBhcmcgd2FzIHJlZmxlY3RlZCB0byByZXByZXNlbnQgYW4gYWN0dWFsIGRlY29yYXRvci5cbiAgICAgICAgICAgIGlmICh0aGlzLmlzRnJvbUNvcmUoZW50cnkuZGVjb3JhdG9yKSkge1xuICAgICAgICAgICAgICBjb25zdCBkZWNvcmF0b3JzID1cbiAgICAgICAgICAgICAgICAgIG1lbWJlckRlY29yYXRvcnMuaGFzKG1lbWJlck5hbWUpID8gbWVtYmVyRGVjb3JhdG9ycy5nZXQobWVtYmVyTmFtZSkhIDogW107XG4gICAgICAgICAgICAgIGRlY29yYXRvcnMucHVzaChlbnRyeS5kZWNvcmF0b3IpO1xuICAgICAgICAgICAgICBtZW1iZXJEZWNvcmF0b3JzLnNldChtZW1iZXJOYW1lLCBkZWNvcmF0b3JzKTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgLy8gSW5mb3JtYXRpb24gb24gZGVjb3JhdGVkIHBhcmFtZXRlcnMgaXMgbm90IGludGVyZXN0aW5nIGZvciBuZ2NjLCBzbyBpdCdzIGlnbm9yZWQuXG4gICAgICAgICAgfVxuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIHtjbGFzc0RlY29yYXRvcnMsIG1lbWJlckRlY29yYXRvcnMsIGNvbnN0cnVjdG9yUGFyYW1JbmZvfTtcbiAgfVxuXG4gIC8qKlxuICAgKiBFeHRyYWN0IHRoZSBkZXRhaWxzIG9mIGFuIGVudHJ5IHdpdGhpbiBhIGBfX2RlY29yYXRlYCBoZWxwZXIgY2FsbC4gRm9yIGV4YW1wbGUsIGdpdmVuIHRoZVxuICAgKiBmb2xsb3dpbmcgY29kZTpcbiAgICpcbiAgICogYGBgXG4gICAqIF9fZGVjb3JhdGUoW1xuICAgKiAgIERpcmVjdGl2ZSh7IHNlbGVjdG9yOiAnW3NvbWVEaXJlY3RpdmVdJyB9KSxcbiAgICogICB0c2xpYl8xLl9fcGFyYW0oMiwgSW5qZWN0KElOSkVDVEVEX1RPS0VOKSksXG4gICAqICAgdHNsaWJfMS5fX21ldGFkYXRhKFwiZGVzaWduOnBhcmFtdHlwZXNcIiwgW1ZpZXdDb250YWluZXJSZWYsIFRlbXBsYXRlUmVmLCBTdHJpbmddKVxuICAgKiBdLCBTb21lRGlyZWN0aXZlKTtcbiAgICogYGBgXG4gICAqXG4gICAqIGl0IGNhbiBiZSBzZWVuIHRoYXQgdGhlcmUgYXJlIGNhbGxzIHRvIHJlZ3VsYXIgZGVjb3JhdG9ycyAodGhlIGBEaXJlY3RpdmVgKSBhbmQgY2FsbHMgaW50b1xuICAgKiBgdHNsaWJgIGZ1bmN0aW9ucyB3aGljaCBoYXZlIGJlZW4gaW5zZXJ0ZWQgYnkgVHlwZVNjcmlwdC4gVGhlcmVmb3JlLCB0aGlzIGZ1bmN0aW9uIGNsYXNzaWZpZXNcbiAgICogYSBjYWxsIHRvIGNvcnJlc3BvbmQgd2l0aFxuICAgKiAgIDEuIGEgcmVhbCBkZWNvcmF0b3IgbGlrZSBgRGlyZWN0aXZlYCBhYm92ZSwgb3JcbiAgICogICAyLiBhIGRlY29yYXRlZCBwYXJhbWV0ZXIsIGNvcnJlc3BvbmRpbmcgd2l0aCBgX19wYXJhbWAgY2FsbHMgZnJvbSBgdHNsaWJgLCBvclxuICAgKiAgIDMuIHRoZSB0eXBlIGluZm9ybWF0aW9uIG9mIHBhcmFtZXRlcnMsIGNvcnJlc3BvbmRpbmcgd2l0aCBgX19tZXRhZGF0YWAgY2FsbCBmcm9tIGB0c2xpYmBcbiAgICpcbiAgICogQHBhcmFtIGV4cHJlc3Npb24gdGhlIGV4cHJlc3Npb24gdGhhdCBuZWVkcyB0byBiZSByZWZsZWN0ZWQgaW50byBhIGBEZWNvcmF0ZUhlbHBlckVudHJ5YFxuICAgKiBAcmV0dXJucyBhbiBvYmplY3QgdGhhdCBpbmRpY2F0ZXMgd2hpY2ggb2YgdGhlIHRocmVlIGNhdGVnb3JpZXMgdGhlIGNhbGwgcmVwcmVzZW50cywgdG9nZXRoZXJcbiAgICogd2l0aCB0aGUgcmVmbGVjdGVkIGluZm9ybWF0aW9uIG9mIHRoZSBjYWxsLCBvciBudWxsIGlmIHRoZSBjYWxsIGlzIG5vdCBhIHZhbGlkIGRlY29yYXRlIGNhbGwuXG4gICAqL1xuICBwcm90ZWN0ZWQgcmVmbGVjdERlY29yYXRlSGVscGVyRW50cnkoZXhwcmVzc2lvbjogdHMuRXhwcmVzc2lvbik6IERlY29yYXRlSGVscGVyRW50cnl8bnVsbCB7XG4gICAgLy8gV2Ugb25seSBjYXJlIGFib3V0IHRob3NlIGVsZW1lbnRzIHRoYXQgYXJlIGFjdHVhbCBjYWxsc1xuICAgIGlmICghdHMuaXNDYWxsRXhwcmVzc2lvbihleHByZXNzaW9uKSkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIGNvbnN0IGNhbGwgPSBleHByZXNzaW9uO1xuXG4gICAgY29uc3QgaGVscGVyTmFtZSA9IGdldENhbGxlZU5hbWUoY2FsbCk7XG4gICAgaWYgKGhlbHBlck5hbWUgPT09ICdfX21ldGFkYXRhJykge1xuICAgICAgLy8gVGhpcyBpcyBhIGB0c2xpYi5fX21ldGFkYXRhYCBjYWxsLCByZWZsZWN0IHRvIGFyZ3VtZW50cyBpbnRvIGEgYFBhcmFtZXRlclR5cGVzYCBvYmplY3RcbiAgICAgIC8vIGlmIHRoZSBtZXRhZGF0YSBrZXkgaXMgXCJkZXNpZ246cGFyYW10eXBlc1wiLlxuICAgICAgY29uc3Qga2V5ID0gY2FsbC5hcmd1bWVudHNbMF07XG4gICAgICBpZiAoa2V5ID09PSB1bmRlZmluZWQgfHwgIXRzLmlzU3RyaW5nTGl0ZXJhbChrZXkpIHx8IGtleS50ZXh0ICE9PSAnZGVzaWduOnBhcmFtdHlwZXMnKSB7XG4gICAgICAgIHJldHVybiBudWxsO1xuICAgICAgfVxuXG4gICAgICBjb25zdCB2YWx1ZSA9IGNhbGwuYXJndW1lbnRzWzFdO1xuICAgICAgaWYgKHZhbHVlID09PSB1bmRlZmluZWQgfHwgIXRzLmlzQXJyYXlMaXRlcmFsRXhwcmVzc2lvbih2YWx1ZSkpIHtcbiAgICAgICAgcmV0dXJuIG51bGw7XG4gICAgICB9XG5cbiAgICAgIHJldHVybiB7XG4gICAgICAgIHR5cGU6ICdwYXJhbXMnLFxuICAgICAgICB0eXBlczogQXJyYXkuZnJvbSh2YWx1ZS5lbGVtZW50cyksXG4gICAgICB9O1xuICAgIH1cblxuICAgIGlmIChoZWxwZXJOYW1lID09PSAnX19wYXJhbScpIHtcbiAgICAgIC8vIFRoaXMgaXMgYSBgdHNsaWIuX19wYXJhbWAgY2FsbCB0aGF0IGlzIHJlZmxlY3RlZCBpbnRvIGEgYFBhcmFtZXRlckRlY29yYXRvcnNgIG9iamVjdC5cbiAgICAgIGNvbnN0IGluZGV4QXJnID0gY2FsbC5hcmd1bWVudHNbMF07XG4gICAgICBjb25zdCBpbmRleCA9IGluZGV4QXJnICYmIHRzLmlzTnVtZXJpY0xpdGVyYWwoaW5kZXhBcmcpID8gcGFyc2VJbnQoaW5kZXhBcmcudGV4dCwgMTApIDogTmFOO1xuICAgICAgaWYgKGlzTmFOKGluZGV4KSkge1xuICAgICAgICByZXR1cm4gbnVsbDtcbiAgICAgIH1cblxuICAgICAgY29uc3QgZGVjb3JhdG9yQ2FsbCA9IGNhbGwuYXJndW1lbnRzWzFdO1xuICAgICAgaWYgKGRlY29yYXRvckNhbGwgPT09IHVuZGVmaW5lZCB8fCAhdHMuaXNDYWxsRXhwcmVzc2lvbihkZWNvcmF0b3JDYWxsKSkge1xuICAgICAgICByZXR1cm4gbnVsbDtcbiAgICAgIH1cblxuICAgICAgY29uc3QgZGVjb3JhdG9yID0gdGhpcy5yZWZsZWN0RGVjb3JhdG9yQ2FsbChkZWNvcmF0b3JDYWxsKTtcbiAgICAgIGlmIChkZWNvcmF0b3IgPT09IG51bGwpIHtcbiAgICAgICAgcmV0dXJuIG51bGw7XG4gICAgICB9XG5cbiAgICAgIHJldHVybiB7XG4gICAgICAgIHR5cGU6ICdwYXJhbTpkZWNvcmF0b3JzJyxcbiAgICAgICAgaW5kZXgsXG4gICAgICAgIGRlY29yYXRvcixcbiAgICAgIH07XG4gICAgfVxuXG4gICAgLy8gT3RoZXJ3aXNlIGF0dGVtcHQgdG8gcmVmbGVjdCBpdCBhcyBhIHJlZ3VsYXIgZGVjb3JhdG9yLlxuICAgIGNvbnN0IGRlY29yYXRvciA9IHRoaXMucmVmbGVjdERlY29yYXRvckNhbGwoY2FsbCk7XG4gICAgaWYgKGRlY29yYXRvciA9PT0gbnVsbCkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIHJldHVybiB7XG4gICAgICB0eXBlOiAnZGVjb3JhdG9yJyxcbiAgICAgIGRlY29yYXRvcixcbiAgICB9O1xuICB9XG5cbiAgcHJvdGVjdGVkIHJlZmxlY3REZWNvcmF0b3JDYWxsKGNhbGw6IHRzLkNhbGxFeHByZXNzaW9uKTogRGVjb3JhdG9yfG51bGwge1xuICAgIGNvbnN0IGRlY29yYXRvckV4cHJlc3Npb24gPSBjYWxsLmV4cHJlc3Npb247XG4gICAgaWYgKCFpc0RlY29yYXRvcklkZW50aWZpZXIoZGVjb3JhdG9yRXhwcmVzc2lvbikpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIC8vIFdlIGZvdW5kIGEgZGVjb3JhdG9yIVxuICAgIGNvbnN0IGRlY29yYXRvcklkZW50aWZpZXIgPVxuICAgICAgICB0cy5pc0lkZW50aWZpZXIoZGVjb3JhdG9yRXhwcmVzc2lvbikgPyBkZWNvcmF0b3JFeHByZXNzaW9uIDogZGVjb3JhdG9yRXhwcmVzc2lvbi5uYW1lO1xuXG4gICAgcmV0dXJuIHtcbiAgICAgIG5hbWU6IGRlY29yYXRvcklkZW50aWZpZXIudGV4dCxcbiAgICAgIGlkZW50aWZpZXI6IGRlY29yYXRvckV4cHJlc3Npb24sXG4gICAgICBpbXBvcnQ6IHRoaXMuZ2V0SW1wb3J0T2ZJZGVudGlmaWVyKGRlY29yYXRvcklkZW50aWZpZXIpLFxuICAgICAgbm9kZTogY2FsbCxcbiAgICAgIGFyZ3M6IEFycmF5LmZyb20oY2FsbC5hcmd1bWVudHMpLFxuICAgIH07XG4gIH1cblxuICAvKipcbiAgICogQ2hlY2sgdGhlIGdpdmVuIHN0YXRlbWVudCB0byBzZWUgaWYgaXQgaXMgYSBjYWxsIHRvIGFueSBvZiB0aGUgc3BlY2lmaWVkIGhlbHBlciBmdW5jdGlvbnMgb3JcbiAgICogbnVsbCBpZiBub3QgZm91bmQuXG4gICAqXG4gICAqIE1hdGNoaW5nIHN0YXRlbWVudHMgd2lsbCBsb29rIGxpa2U6ICBgdHNsaWJfMS5fX2RlY29yYXRlKC4uLik7YC5cbiAgICogQHBhcmFtIHN0YXRlbWVudCB0aGUgc3RhdGVtZW50IHRoYXQgbWF5IGNvbnRhaW4gdGhlIGNhbGwuXG4gICAqIEBwYXJhbSBoZWxwZXJOYW1lcyB0aGUgbmFtZXMgb2YgdGhlIGhlbHBlciB3ZSBhcmUgbG9va2luZyBmb3IuXG4gICAqIEByZXR1cm5zIHRoZSBub2RlIHRoYXQgY29ycmVzcG9uZHMgdG8gdGhlIGBfX2RlY29yYXRlKC4uLilgIGNhbGwgb3IgbnVsbCBpZiB0aGUgc3RhdGVtZW50XG4gICAqIGRvZXMgbm90IG1hdGNoLlxuICAgKi9cbiAgcHJvdGVjdGVkIGdldEhlbHBlckNhbGwoc3RhdGVtZW50OiB0cy5TdGF0ZW1lbnQsIGhlbHBlck5hbWVzOiBzdHJpbmdbXSk6IHRzLkNhbGxFeHByZXNzaW9ufG51bGwge1xuICAgIGlmICgodHMuaXNFeHByZXNzaW9uU3RhdGVtZW50KHN0YXRlbWVudCkgfHwgdHMuaXNSZXR1cm5TdGF0ZW1lbnQoc3RhdGVtZW50KSkgJiZcbiAgICAgICAgc3RhdGVtZW50LmV4cHJlc3Npb24pIHtcbiAgICAgIGxldCBleHByZXNzaW9uID0gc3RhdGVtZW50LmV4cHJlc3Npb247XG4gICAgICB3aGlsZSAoaXNBc3NpZ25tZW50KGV4cHJlc3Npb24pKSB7XG4gICAgICAgIGV4cHJlc3Npb24gPSBleHByZXNzaW9uLnJpZ2h0O1xuICAgICAgfVxuICAgICAgaWYgKHRzLmlzQ2FsbEV4cHJlc3Npb24oZXhwcmVzc2lvbikpIHtcbiAgICAgICAgY29uc3QgY2FsbGVlTmFtZSA9IGdldENhbGxlZU5hbWUoZXhwcmVzc2lvbik7XG4gICAgICAgIGlmIChjYWxsZWVOYW1lICE9PSBudWxsICYmIGhlbHBlck5hbWVzLmluY2x1ZGVzKGNhbGxlZU5hbWUpKSB7XG4gICAgICAgICAgcmV0dXJuIGV4cHJlc3Npb247XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuXG4gIC8qKlxuICAgKiBSZWZsZWN0IG92ZXIgdGhlIGdpdmVuIGFycmF5IG5vZGUgYW5kIGV4dHJhY3QgZGVjb3JhdG9yIGluZm9ybWF0aW9uIGZyb20gZWFjaCBlbGVtZW50LlxuICAgKlxuICAgKiBUaGlzIGlzIHVzZWQgZm9yIGRlY29yYXRvcnMgdGhhdCBhcmUgZGVmaW5lZCBpbiBzdGF0aWMgcHJvcGVydGllcy4gRm9yIGV4YW1wbGU6XG4gICAqXG4gICAqIGBgYFxuICAgKiBTb21lRGlyZWN0aXZlLmRlY29yYXRvcnMgPSBbXG4gICAqICAgeyB0eXBlOiBEaXJlY3RpdmUsIGFyZ3M6IFt7IHNlbGVjdG9yOiAnW3NvbWVEaXJlY3RpdmVdJyB9LF0gfVxuICAgKiBdO1xuICAgKiBgYGBcbiAgICpcbiAgICogQHBhcmFtIGRlY29yYXRvcnNBcnJheSBhbiBleHByZXNzaW9uIHRoYXQgY29udGFpbnMgZGVjb3JhdG9yIGluZm9ybWF0aW9uLlxuICAgKiBAcmV0dXJucyBhbiBhcnJheSBvZiBkZWNvcmF0b3IgaW5mbyB0aGF0IHdhcyByZWZsZWN0ZWQgZnJvbSB0aGUgYXJyYXkgbm9kZS5cbiAgICovXG4gIHByb3RlY3RlZCByZWZsZWN0RGVjb3JhdG9ycyhkZWNvcmF0b3JzQXJyYXk6IHRzLkV4cHJlc3Npb24pOiBEZWNvcmF0b3JbXSB7XG4gICAgY29uc3QgZGVjb3JhdG9yczogRGVjb3JhdG9yW10gPSBbXTtcblxuICAgIGlmICh0cy5pc0FycmF5TGl0ZXJhbEV4cHJlc3Npb24oZGVjb3JhdG9yc0FycmF5KSkge1xuICAgICAgLy8gQWRkIGVhY2ggZGVjb3JhdG9yIHRoYXQgaXMgaW1wb3J0ZWQgZnJvbSBgQGFuZ3VsYXIvY29yZWAgaW50byB0aGUgYGRlY29yYXRvcnNgIGFycmF5XG4gICAgICBkZWNvcmF0b3JzQXJyYXkuZWxlbWVudHMuZm9yRWFjaChub2RlID0+IHtcbiAgICAgICAgLy8gSWYgdGhlIGRlY29yYXRvciBpcyBub3QgYW4gb2JqZWN0IGxpdGVyYWwgZXhwcmVzc2lvbiB0aGVuIHdlIGFyZSBub3QgaW50ZXJlc3RlZFxuICAgICAgICBpZiAodHMuaXNPYmplY3RMaXRlcmFsRXhwcmVzc2lvbihub2RlKSkge1xuICAgICAgICAgIC8vIFdlIGFyZSBvbmx5IGludGVyZXN0ZWQgaW4gb2JqZWN0cyBvZiB0aGUgZm9ybTogYHsgdHlwZTogRGVjb3JhdG9yVHlwZSwgYXJnczogWy4uLl0gfWBcbiAgICAgICAgICBjb25zdCBkZWNvcmF0b3IgPSByZWZsZWN0T2JqZWN0TGl0ZXJhbChub2RlKTtcblxuICAgICAgICAgIC8vIElzIHRoZSB2YWx1ZSBvZiB0aGUgYHR5cGVgIHByb3BlcnR5IGFuIGlkZW50aWZpZXI/XG4gICAgICAgICAgaWYgKGRlY29yYXRvci5oYXMoJ3R5cGUnKSkge1xuICAgICAgICAgICAgbGV0IGRlY29yYXRvclR5cGUgPSBkZWNvcmF0b3IuZ2V0KCd0eXBlJykhO1xuICAgICAgICAgICAgaWYgKGlzRGVjb3JhdG9ySWRlbnRpZmllcihkZWNvcmF0b3JUeXBlKSkge1xuICAgICAgICAgICAgICBjb25zdCBkZWNvcmF0b3JJZGVudGlmaWVyID1cbiAgICAgICAgICAgICAgICAgIHRzLmlzSWRlbnRpZmllcihkZWNvcmF0b3JUeXBlKSA/IGRlY29yYXRvclR5cGUgOiBkZWNvcmF0b3JUeXBlLm5hbWU7XG4gICAgICAgICAgICAgIGRlY29yYXRvcnMucHVzaCh7XG4gICAgICAgICAgICAgICAgbmFtZTogZGVjb3JhdG9ySWRlbnRpZmllci50ZXh0LFxuICAgICAgICAgICAgICAgIGlkZW50aWZpZXI6IGRlY29yYXRvclR5cGUsXG4gICAgICAgICAgICAgICAgaW1wb3J0OiB0aGlzLmdldEltcG9ydE9mSWRlbnRpZmllcihkZWNvcmF0b3JJZGVudGlmaWVyKSxcbiAgICAgICAgICAgICAgICBub2RlLFxuICAgICAgICAgICAgICAgIGFyZ3M6IGdldERlY29yYXRvckFyZ3Mobm9kZSksXG4gICAgICAgICAgICAgIH0pO1xuICAgICAgICAgICAgfVxuICAgICAgICAgIH1cbiAgICAgICAgfVxuICAgICAgfSk7XG4gICAgfVxuICAgIHJldHVybiBkZWNvcmF0b3JzO1xuICB9XG5cbiAgLyoqXG4gICAqIFJlZmxlY3Qgb3ZlciBhIHN5bWJvbCBhbmQgZXh0cmFjdCB0aGUgbWVtYmVyIGluZm9ybWF0aW9uLCBjb21iaW5pbmcgaXQgd2l0aCB0aGVcbiAgICogcHJvdmlkZWQgZGVjb3JhdG9yIGluZm9ybWF0aW9uLCBhbmQgd2hldGhlciBpdCBpcyBhIHN0YXRpYyBtZW1iZXIuXG4gICAqXG4gICAqIEEgc2luZ2xlIHN5bWJvbCBtYXkgcmVwcmVzZW50IG11bHRpcGxlIGNsYXNzIG1lbWJlcnMgaW4gdGhlIGNhc2Ugb2YgYWNjZXNzb3JzO1xuICAgKiBhbiBlcXVhbGx5IG5hbWVkIGdldHRlci9zZXR0ZXIgYWNjZXNzb3IgcGFpciBpcyBjb21iaW5lZCBpbnRvIGEgc2luZ2xlIHN5bWJvbC5cbiAgICogV2hlbiB0aGUgc3ltYm9sIGlzIHJlY29nbml6ZWQgYXMgcmVwcmVzZW50aW5nIGFuIGFjY2Vzc29yLCBpdHMgZGVjbGFyYXRpb25zIGFyZVxuICAgKiBhbmFseXplZCBzdWNoIHRoYXQgYm90aCB0aGUgc2V0dGVyIGFuZCBnZXR0ZXIgYWNjZXNzb3IgYXJlIHJldHVybmVkIGFzIHNlcGFyYXRlXG4gICAqIGNsYXNzIG1lbWJlcnMuXG4gICAqXG4gICAqIE9uZSBkaWZmZXJlbmNlIHdydCB0aGUgVHlwZVNjcmlwdCBob3N0IGlzIHRoYXQgaW4gRVMyMDE1LCB3ZSBjYW5ub3Qgc2VlIHdoaWNoXG4gICAqIGFjY2Vzc29yIG9yaWdpbmFsbHkgaGFkIGFueSBkZWNvcmF0b3JzIGFwcGxpZWQgdG8gdGhlbSwgYXMgZGVjb3JhdG9ycyBhcmUgYXBwbGllZFxuICAgKiB0byB0aGUgcHJvcGVydHkgZGVzY3JpcHRvciBpbiBnZW5lcmFsLCBub3QgYSBzcGVjaWZpYyBhY2Nlc3Nvci4gSWYgYW4gYWNjZXNzb3JcbiAgICogaGFzIGJvdGggYSBzZXR0ZXIgYW5kIGdldHRlciwgYW55IGRlY29yYXRvcnMgYXJlIG9ubHkgYXR0YWNoZWQgdG8gdGhlIHNldHRlciBtZW1iZXIuXG4gICAqXG4gICAqIEBwYXJhbSBzeW1ib2wgdGhlIHN5bWJvbCBmb3IgdGhlIG1lbWJlciB0byByZWZsZWN0IG92ZXIuXG4gICAqIEBwYXJhbSBkZWNvcmF0b3JzIGFuIGFycmF5IG9mIGRlY29yYXRvcnMgYXNzb2NpYXRlZCB3aXRoIHRoZSBtZW1iZXIuXG4gICAqIEBwYXJhbSBpc1N0YXRpYyB0cnVlIGlmIHRoaXMgbWVtYmVyIGlzIHN0YXRpYywgZmFsc2UgaWYgaXQgaXMgYW4gaW5zdGFuY2UgcHJvcGVydHkuXG4gICAqIEByZXR1cm5zIHRoZSByZWZsZWN0ZWQgbWVtYmVyIGluZm9ybWF0aW9uLCBvciBudWxsIGlmIHRoZSBzeW1ib2wgaXMgbm90IGEgbWVtYmVyLlxuICAgKi9cbiAgcHJvdGVjdGVkIHJlZmxlY3RNZW1iZXJzKHN5bWJvbDogdHMuU3ltYm9sLCBkZWNvcmF0b3JzPzogRGVjb3JhdG9yW10sIGlzU3RhdGljPzogYm9vbGVhbik6XG4gICAgICBDbGFzc01lbWJlcltdfG51bGwge1xuICAgIGlmIChzeW1ib2wuZmxhZ3MgJiB0cy5TeW1ib2xGbGFncy5BY2Nlc3Nvcikge1xuICAgICAgY29uc3QgbWVtYmVyczogQ2xhc3NNZW1iZXJbXSA9IFtdO1xuICAgICAgY29uc3Qgc2V0dGVyID0gc3ltYm9sLmRlY2xhcmF0aW9ucyAmJiBzeW1ib2wuZGVjbGFyYXRpb25zLmZpbmQodHMuaXNTZXRBY2Nlc3Nvcik7XG4gICAgICBjb25zdCBnZXR0ZXIgPSBzeW1ib2wuZGVjbGFyYXRpb25zICYmIHN5bWJvbC5kZWNsYXJhdGlvbnMuZmluZCh0cy5pc0dldEFjY2Vzc29yKTtcblxuICAgICAgY29uc3Qgc2V0dGVyTWVtYmVyID1cbiAgICAgICAgICBzZXR0ZXIgJiYgdGhpcy5yZWZsZWN0TWVtYmVyKHNldHRlciwgQ2xhc3NNZW1iZXJLaW5kLlNldHRlciwgZGVjb3JhdG9ycywgaXNTdGF0aWMpO1xuICAgICAgaWYgKHNldHRlck1lbWJlcikge1xuICAgICAgICBtZW1iZXJzLnB1c2goc2V0dGVyTWVtYmVyKTtcblxuICAgICAgICAvLyBQcmV2ZW50IGF0dGFjaGluZyB0aGUgZGVjb3JhdG9ycyB0byBhIHBvdGVudGlhbCBnZXR0ZXIuIEluIEVTMjAxNSwgd2UgY2FuJ3QgdGVsbCB3aGVyZVxuICAgICAgICAvLyB0aGUgZGVjb3JhdG9ycyB3ZXJlIG9yaWdpbmFsbHkgYXR0YWNoZWQgdG8sIGhvd2V2ZXIgd2Ugb25seSB3YW50IHRvIGF0dGFjaCB0aGVtIHRvIGFcbiAgICAgICAgLy8gc2luZ2xlIGBDbGFzc01lbWJlcmAgYXMgb3RoZXJ3aXNlIG5ndHNjIHdvdWxkIGhhbmRsZSB0aGUgc2FtZSBkZWNvcmF0b3JzIHR3aWNlLlxuICAgICAgICBkZWNvcmF0b3JzID0gdW5kZWZpbmVkO1xuICAgICAgfVxuXG4gICAgICBjb25zdCBnZXR0ZXJNZW1iZXIgPVxuICAgICAgICAgIGdldHRlciAmJiB0aGlzLnJlZmxlY3RNZW1iZXIoZ2V0dGVyLCBDbGFzc01lbWJlcktpbmQuR2V0dGVyLCBkZWNvcmF0b3JzLCBpc1N0YXRpYyk7XG4gICAgICBpZiAoZ2V0dGVyTWVtYmVyKSB7XG4gICAgICAgIG1lbWJlcnMucHVzaChnZXR0ZXJNZW1iZXIpO1xuICAgICAgfVxuXG4gICAgICByZXR1cm4gbWVtYmVycztcbiAgICB9XG5cbiAgICBsZXQga2luZDogQ2xhc3NNZW1iZXJLaW5kfG51bGwgPSBudWxsO1xuICAgIGlmIChzeW1ib2wuZmxhZ3MgJiB0cy5TeW1ib2xGbGFncy5NZXRob2QpIHtcbiAgICAgIGtpbmQgPSBDbGFzc01lbWJlcktpbmQuTWV0aG9kO1xuICAgIH0gZWxzZSBpZiAoc3ltYm9sLmZsYWdzICYgdHMuU3ltYm9sRmxhZ3MuUHJvcGVydHkpIHtcbiAgICAgIGtpbmQgPSBDbGFzc01lbWJlcktpbmQuUHJvcGVydHk7XG4gICAgfVxuXG4gICAgY29uc3Qgbm9kZSA9IHN5bWJvbC52YWx1ZURlY2xhcmF0aW9uIHx8IHN5bWJvbC5kZWNsYXJhdGlvbnMgJiYgc3ltYm9sLmRlY2xhcmF0aW9uc1swXTtcbiAgICBpZiAoIW5vZGUpIHtcbiAgICAgIC8vIElmIHRoZSBzeW1ib2wgaGFzIGJlZW4gaW1wb3J0ZWQgZnJvbSBhIFR5cGVTY3JpcHQgdHlwaW5ncyBmaWxlIHRoZW4gdGhlIGNvbXBpbGVyXG4gICAgICAvLyBtYXkgcGFzcyB0aGUgYHByb3RvdHlwZWAgc3ltYm9sIGFzIGFuIGV4cG9ydCBvZiB0aGUgY2xhc3MuXG4gICAgICAvLyBCdXQgdGhpcyBoYXMgbm8gZGVjbGFyYXRpb24uIEluIHRoaXMgY2FzZSB3ZSBqdXN0IHF1aWV0bHkgaWdub3JlIGl0LlxuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuXG4gICAgY29uc3QgbWVtYmVyID0gdGhpcy5yZWZsZWN0TWVtYmVyKG5vZGUsIGtpbmQsIGRlY29yYXRvcnMsIGlzU3RhdGljKTtcbiAgICBpZiAoIW1lbWJlcikge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuXG4gICAgcmV0dXJuIFttZW1iZXJdO1xuICB9XG5cbiAgLyoqXG4gICAqIFJlZmxlY3Qgb3ZlciBhIHN5bWJvbCBhbmQgZXh0cmFjdCB0aGUgbWVtYmVyIGluZm9ybWF0aW9uLCBjb21iaW5pbmcgaXQgd2l0aCB0aGVcbiAgICogcHJvdmlkZWQgZGVjb3JhdG9yIGluZm9ybWF0aW9uLCBhbmQgd2hldGhlciBpdCBpcyBhIHN0YXRpYyBtZW1iZXIuXG4gICAqIEBwYXJhbSBub2RlIHRoZSBkZWNsYXJhdGlvbiBub2RlIGZvciB0aGUgbWVtYmVyIHRvIHJlZmxlY3Qgb3Zlci5cbiAgICogQHBhcmFtIGtpbmQgdGhlIGFzc3VtZWQga2luZCBvZiB0aGUgbWVtYmVyLCBtYXkgYmVjb21lIG1vcmUgYWNjdXJhdGUgZHVyaW5nIHJlZmxlY3Rpb24uXG4gICAqIEBwYXJhbSBkZWNvcmF0b3JzIGFuIGFycmF5IG9mIGRlY29yYXRvcnMgYXNzb2NpYXRlZCB3aXRoIHRoZSBtZW1iZXIuXG4gICAqIEBwYXJhbSBpc1N0YXRpYyB0cnVlIGlmIHRoaXMgbWVtYmVyIGlzIHN0YXRpYywgZmFsc2UgaWYgaXQgaXMgYW4gaW5zdGFuY2UgcHJvcGVydHkuXG4gICAqIEByZXR1cm5zIHRoZSByZWZsZWN0ZWQgbWVtYmVyIGluZm9ybWF0aW9uLCBvciBudWxsIGlmIHRoZSBzeW1ib2wgaXMgbm90IGEgbWVtYmVyLlxuICAgKi9cbiAgcHJvdGVjdGVkIHJlZmxlY3RNZW1iZXIoXG4gICAgICBub2RlOiB0cy5EZWNsYXJhdGlvbiwga2luZDogQ2xhc3NNZW1iZXJLaW5kfG51bGwsIGRlY29yYXRvcnM/OiBEZWNvcmF0b3JbXSxcbiAgICAgIGlzU3RhdGljPzogYm9vbGVhbik6IENsYXNzTWVtYmVyfG51bGwge1xuICAgIGxldCB2YWx1ZTogdHMuRXhwcmVzc2lvbnxudWxsID0gbnVsbDtcbiAgICBsZXQgbmFtZTogc3RyaW5nfG51bGwgPSBudWxsO1xuICAgIGxldCBuYW1lTm9kZTogdHMuSWRlbnRpZmllcnxudWxsID0gbnVsbDtcblxuICAgIGlmICghaXNDbGFzc01lbWJlclR5cGUobm9kZSkpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIGlmIChpc1N0YXRpYyAmJiBpc1Byb3BlcnR5QWNjZXNzKG5vZGUpKSB7XG4gICAgICBuYW1lID0gbm9kZS5uYW1lLnRleHQ7XG4gICAgICB2YWx1ZSA9IGtpbmQgPT09IENsYXNzTWVtYmVyS2luZC5Qcm9wZXJ0eSA/IG5vZGUucGFyZW50LnJpZ2h0IDogbnVsbDtcbiAgICB9IGVsc2UgaWYgKGlzVGhpc0Fzc2lnbm1lbnQobm9kZSkpIHtcbiAgICAgIGtpbmQgPSBDbGFzc01lbWJlcktpbmQuUHJvcGVydHk7XG4gICAgICBuYW1lID0gbm9kZS5sZWZ0Lm5hbWUudGV4dDtcbiAgICAgIHZhbHVlID0gbm9kZS5yaWdodDtcbiAgICAgIGlzU3RhdGljID0gZmFsc2U7XG4gICAgfSBlbHNlIGlmICh0cy5pc0NvbnN0cnVjdG9yRGVjbGFyYXRpb24obm9kZSkpIHtcbiAgICAgIGtpbmQgPSBDbGFzc01lbWJlcktpbmQuQ29uc3RydWN0b3I7XG4gICAgICBuYW1lID0gJ2NvbnN0cnVjdG9yJztcbiAgICAgIGlzU3RhdGljID0gZmFsc2U7XG4gICAgfVxuXG4gICAgaWYgKGtpbmQgPT09IG51bGwpIHtcbiAgICAgIHRoaXMubG9nZ2VyLndhcm4oYFVua25vd24gbWVtYmVyIHR5cGU6IFwiJHtub2RlLmdldFRleHQoKX1gKTtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIGlmICghbmFtZSkge1xuICAgICAgaWYgKGlzTmFtZWREZWNsYXJhdGlvbihub2RlKSkge1xuICAgICAgICBuYW1lID0gbm9kZS5uYW1lLnRleHQ7XG4gICAgICAgIG5hbWVOb2RlID0gbm9kZS5uYW1lO1xuICAgICAgfSBlbHNlIHtcbiAgICAgICAgcmV0dXJuIG51bGw7XG4gICAgICB9XG4gICAgfVxuXG4gICAgLy8gSWYgd2UgaGF2ZSBzdGlsbCBub3QgZGV0ZXJtaW5lZCBpZiB0aGlzIGlzIGEgc3RhdGljIG9yIGluc3RhbmNlIG1lbWJlciB0aGVuXG4gICAgLy8gbG9vayBmb3IgdGhlIGBzdGF0aWNgIGtleXdvcmQgb24gdGhlIGRlY2xhcmF0aW9uXG4gICAgaWYgKGlzU3RhdGljID09PSB1bmRlZmluZWQpIHtcbiAgICAgIGlzU3RhdGljID0gbm9kZS5tb2RpZmllcnMgIT09IHVuZGVmaW5lZCAmJlxuICAgICAgICAgIG5vZGUubW9kaWZpZXJzLnNvbWUobW9kID0+IG1vZC5raW5kID09PSB0cy5TeW50YXhLaW5kLlN0YXRpY0tleXdvcmQpO1xuICAgIH1cblxuICAgIGNvbnN0IHR5cGU6IHRzLlR5cGVOb2RlID0gKG5vZGUgYXMgYW55KS50eXBlIHx8IG51bGw7XG4gICAgcmV0dXJuIHtcbiAgICAgIG5vZGUsXG4gICAgICBpbXBsZW1lbnRhdGlvbjogbm9kZSxcbiAgICAgIGtpbmQsXG4gICAgICB0eXBlLFxuICAgICAgbmFtZSxcbiAgICAgIG5hbWVOb2RlLFxuICAgICAgdmFsdWUsXG4gICAgICBpc1N0YXRpYyxcbiAgICAgIGRlY29yYXRvcnM6IGRlY29yYXRvcnMgfHwgW11cbiAgICB9O1xuICB9XG5cbiAgLyoqXG4gICAqIEZpbmQgdGhlIGRlY2xhcmF0aW9ucyBvZiB0aGUgY29uc3RydWN0b3IgcGFyYW1ldGVycyBvZiBhIGNsYXNzIGlkZW50aWZpZWQgYnkgaXRzIHN5bWJvbC5cbiAgICogQHBhcmFtIGNsYXNzU3ltYm9sIHRoZSBjbGFzcyB3aG9zZSBwYXJhbWV0ZXJzIHdlIHdhbnQgdG8gZmluZC5cbiAgICogQHJldHVybnMgYW4gYXJyYXkgb2YgYHRzLlBhcmFtZXRlckRlY2xhcmF0aW9uYCBvYmplY3RzIHJlcHJlc2VudGluZyBlYWNoIG9mIHRoZSBwYXJhbWV0ZXJzIGluXG4gICAqIHRoZSBjbGFzcydzIGNvbnN0cnVjdG9yIG9yIG51bGwgaWYgdGhlcmUgaXMgbm8gY29uc3RydWN0b3IuXG4gICAqL1xuICBwcm90ZWN0ZWQgZ2V0Q29uc3RydWN0b3JQYXJhbWV0ZXJEZWNsYXJhdGlvbnMoY2xhc3NTeW1ib2w6IE5nY2NDbGFzc1N5bWJvbCk6XG4gICAgICB0cy5QYXJhbWV0ZXJEZWNsYXJhdGlvbltdfG51bGwge1xuICAgIGNvbnN0IG1lbWJlcnMgPSBjbGFzc1N5bWJvbC5pbXBsZW1lbnRhdGlvbi5tZW1iZXJzO1xuICAgIGlmIChtZW1iZXJzICYmIG1lbWJlcnMuaGFzKENPTlNUUlVDVE9SKSkge1xuICAgICAgY29uc3QgY29uc3RydWN0b3JTeW1ib2wgPSBtZW1iZXJzLmdldChDT05TVFJVQ1RPUikhO1xuICAgICAgLy8gRm9yIHNvbWUgcmVhc29uIHRoZSBjb25zdHJ1Y3RvciBkb2VzIG5vdCBoYXZlIGEgYHZhbHVlRGVjbGFyYXRpb25gID8hP1xuICAgICAgY29uc3QgY29uc3RydWN0b3IgPSBjb25zdHJ1Y3RvclN5bWJvbC5kZWNsYXJhdGlvbnMgJiZcbiAgICAgICAgICBjb25zdHJ1Y3RvclN5bWJvbC5kZWNsYXJhdGlvbnNbMF0gYXMgdHMuQ29uc3RydWN0b3JEZWNsYXJhdGlvbiB8IHVuZGVmaW5lZDtcbiAgICAgIGlmICghY29uc3RydWN0b3IpIHtcbiAgICAgICAgcmV0dXJuIFtdO1xuICAgICAgfVxuICAgICAgaWYgKGNvbnN0cnVjdG9yLnBhcmFtZXRlcnMubGVuZ3RoID4gMCkge1xuICAgICAgICByZXR1cm4gQXJyYXkuZnJvbShjb25zdHJ1Y3Rvci5wYXJhbWV0ZXJzKTtcbiAgICAgIH1cbiAgICAgIGlmIChpc1N5bnRoZXNpemVkQ29uc3RydWN0b3IoY29uc3RydWN0b3IpKSB7XG4gICAgICAgIHJldHVybiBudWxsO1xuICAgICAgfVxuICAgICAgcmV0dXJuIFtdO1xuICAgIH1cbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXQgdGhlIHBhcmFtZXRlciBkZWNvcmF0b3JzIG9mIGEgY2xhc3MgY29uc3RydWN0b3IuXG4gICAqXG4gICAqIEBwYXJhbSBjbGFzc1N5bWJvbCB0aGUgY2xhc3Mgd2hvc2UgcGFyYW1ldGVyIGluZm8gd2Ugd2FudCB0byBnZXQuXG4gICAqIEBwYXJhbSBwYXJhbWV0ZXJOb2RlcyB0aGUgYXJyYXkgb2YgVHlwZVNjcmlwdCBwYXJhbWV0ZXIgbm9kZXMgZm9yIHRoaXMgY2xhc3MncyBjb25zdHJ1Y3Rvci5cbiAgICogQHJldHVybnMgYW4gYXJyYXkgb2YgY29uc3RydWN0b3IgcGFyYW1ldGVyIGluZm8gb2JqZWN0cy5cbiAgICovXG4gIHByb3RlY3RlZCBnZXRDb25zdHJ1Y3RvclBhcmFtSW5mbyhcbiAgICAgIGNsYXNzU3ltYm9sOiBOZ2NjQ2xhc3NTeW1ib2wsIHBhcmFtZXRlck5vZGVzOiB0cy5QYXJhbWV0ZXJEZWNsYXJhdGlvbltdKTogQ3RvclBhcmFtZXRlcltdIHtcbiAgICBjb25zdCB7Y29uc3RydWN0b3JQYXJhbUluZm99ID0gdGhpcy5hY3F1aXJlRGVjb3JhdG9ySW5mbyhjbGFzc1N5bWJvbCk7XG5cbiAgICByZXR1cm4gcGFyYW1ldGVyTm9kZXMubWFwKChub2RlLCBpbmRleCkgPT4ge1xuICAgICAgY29uc3Qge2RlY29yYXRvcnMsIHR5cGVFeHByZXNzaW9ufSA9IGNvbnN0cnVjdG9yUGFyYW1JbmZvW2luZGV4XSA/XG4gICAgICAgICAgY29uc3RydWN0b3JQYXJhbUluZm9baW5kZXhdIDpcbiAgICAgICAgICB7ZGVjb3JhdG9yczogbnVsbCwgdHlwZUV4cHJlc3Npb246IG51bGx9O1xuICAgICAgY29uc3QgbmFtZU5vZGUgPSBub2RlLm5hbWU7XG5cbiAgICAgIGxldCB0eXBlVmFsdWVSZWZlcmVuY2U6IFR5cGVWYWx1ZVJlZmVyZW5jZXxudWxsID0gbnVsbDtcbiAgICAgIGlmICh0eXBlRXhwcmVzc2lvbiAhPT0gbnVsbCkge1xuICAgICAgICAvLyBgdHlwZUV4cHJlc3Npb25gIGlzIGFuIGV4cHJlc3Npb24gaW4gYSBcInR5cGVcIiBjb250ZXh0LiBSZXNvbHZlIGl0IHRvIGEgZGVjbGFyZWQgdmFsdWUuXG4gICAgICAgIC8vIEVpdGhlciBpdCdzIGEgcmVmZXJlbmNlIHRvIGFuIGltcG9ydGVkIHR5cGUsIG9yIGEgdHlwZSBkZWNsYXJlZCBsb2NhbGx5LiBEaXN0aW5ndWlzaCB0aGVcbiAgICAgICAgLy8gdHdvIGNhc2VzIHdpdGggYGdldERlY2xhcmF0aW9uT2ZFeHByZXNzaW9uYC5cbiAgICAgICAgY29uc3QgZGVjbCA9IHRoaXMuZ2V0RGVjbGFyYXRpb25PZkV4cHJlc3Npb24odHlwZUV4cHJlc3Npb24pO1xuICAgICAgICBpZiAoZGVjbCAhPT0gbnVsbCAmJiBkZWNsLm5vZGUgIT09IG51bGwgJiYgZGVjbC52aWFNb2R1bGUgIT09IG51bGwgJiZcbiAgICAgICAgICAgIGlzTmFtZWREZWNsYXJhdGlvbihkZWNsLm5vZGUpKSB7XG4gICAgICAgICAgdHlwZVZhbHVlUmVmZXJlbmNlID0ge1xuICAgICAgICAgICAgbG9jYWw6IGZhbHNlLFxuICAgICAgICAgICAgdmFsdWVEZWNsYXJhdGlvbjogZGVjbC5ub2RlLFxuICAgICAgICAgICAgbW9kdWxlTmFtZTogZGVjbC52aWFNb2R1bGUsXG4gICAgICAgICAgICBpbXBvcnRlZE5hbWU6IGRlY2wubm9kZS5uYW1lLnRleHQsXG4gICAgICAgICAgICBuZXN0ZWRQYXRoOiBudWxsLFxuICAgICAgICAgIH07XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgdHlwZVZhbHVlUmVmZXJlbmNlID0ge1xuICAgICAgICAgICAgbG9jYWw6IHRydWUsXG4gICAgICAgICAgICBleHByZXNzaW9uOiB0eXBlRXhwcmVzc2lvbixcbiAgICAgICAgICAgIGRlZmF1bHRJbXBvcnRTdGF0ZW1lbnQ6IG51bGwsXG4gICAgICAgICAgfTtcbiAgICAgICAgfVxuICAgICAgfVxuXG4gICAgICByZXR1cm4ge1xuICAgICAgICBuYW1lOiBnZXROYW1lVGV4dChuYW1lTm9kZSksXG4gICAgICAgIG5hbWVOb2RlLFxuICAgICAgICB0eXBlVmFsdWVSZWZlcmVuY2UsXG4gICAgICAgIHR5cGVOb2RlOiBudWxsLFxuICAgICAgICBkZWNvcmF0b3JzXG4gICAgICB9O1xuICAgIH0pO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldCB0aGUgcGFyYW1ldGVyIHR5cGUgYW5kIGRlY29yYXRvcnMgZm9yIHRoZSBjb25zdHJ1Y3RvciBvZiBhIGNsYXNzLFxuICAgKiB3aGVyZSB0aGUgaW5mb3JtYXRpb24gaXMgc3RvcmVkIG9uIGEgc3RhdGljIHByb3BlcnR5IG9mIHRoZSBjbGFzcy5cbiAgICpcbiAgICogTm90ZSB0aGF0IGluIEVTTTIwMTUsIHRoZSBwcm9wZXJ0eSBpcyBkZWZpbmVkIGFuIGFycmF5LCBvciBieSBhbiBhcnJvdyBmdW5jdGlvbiB0aGF0IHJldHVybnNcbiAgICogYW4gYXJyYXksIG9mIGRlY29yYXRvciBhbmQgdHlwZSBpbmZvcm1hdGlvbi5cbiAgICpcbiAgICogRm9yIGV4YW1wbGUsXG4gICAqXG4gICAqIGBgYFxuICAgKiBTb21lRGlyZWN0aXZlLmN0b3JQYXJhbWV0ZXJzID0gKCkgPT4gW1xuICAgKiAgIHt0eXBlOiBWaWV3Q29udGFpbmVyUmVmfSxcbiAgICogICB7dHlwZTogVGVtcGxhdGVSZWZ9LFxuICAgKiAgIHt0eXBlOiB1bmRlZmluZWQsIGRlY29yYXRvcnM6IFt7IHR5cGU6IEluamVjdCwgYXJnczogW0lOSkVDVEVEX1RPS0VOXX1dfSxcbiAgICogXTtcbiAgICogYGBgXG4gICAqXG4gICAqIG9yXG4gICAqXG4gICAqIGBgYFxuICAgKiBTb21lRGlyZWN0aXZlLmN0b3JQYXJhbWV0ZXJzID0gW1xuICAgKiAgIHt0eXBlOiBWaWV3Q29udGFpbmVyUmVmfSxcbiAgICogICB7dHlwZTogVGVtcGxhdGVSZWZ9LFxuICAgKiAgIHt0eXBlOiB1bmRlZmluZWQsIGRlY29yYXRvcnM6IFt7dHlwZTogSW5qZWN0LCBhcmdzOiBbSU5KRUNURURfVE9LRU5dfV19LFxuICAgKiBdO1xuICAgKiBgYGBcbiAgICpcbiAgICogQHBhcmFtIHBhcmFtRGVjb3JhdG9yc1Byb3BlcnR5IHRoZSBwcm9wZXJ0eSB0aGF0IGhvbGRzIHRoZSBwYXJhbWV0ZXIgaW5mbyB3ZSB3YW50IHRvIGdldC5cbiAgICogQHJldHVybnMgYW4gYXJyYXkgb2Ygb2JqZWN0cyBjb250YWluaW5nIHRoZSB0eXBlIGFuZCBkZWNvcmF0b3JzIGZvciBlYWNoIHBhcmFtZXRlci5cbiAgICovXG4gIHByb3RlY3RlZCBnZXRQYXJhbUluZm9Gcm9tU3RhdGljUHJvcGVydHkocGFyYW1EZWNvcmF0b3JzUHJvcGVydHk6IHRzLlN5bWJvbCk6IFBhcmFtSW5mb1tdfG51bGwge1xuICAgIGNvbnN0IHBhcmFtRGVjb3JhdG9ycyA9IGdldFByb3BlcnR5VmFsdWVGcm9tU3ltYm9sKHBhcmFtRGVjb3JhdG9yc1Byb3BlcnR5KTtcbiAgICBpZiAocGFyYW1EZWNvcmF0b3JzKSB7XG4gICAgICAvLyBUaGUgZGVjb3JhdG9ycyBhcnJheSBtYXkgYmUgd3JhcHBlZCBpbiBhbiBhcnJvdyBmdW5jdGlvbi4gSWYgc28gdW53cmFwIGl0LlxuICAgICAgY29uc3QgY29udGFpbmVyID1cbiAgICAgICAgICB0cy5pc0Fycm93RnVuY3Rpb24ocGFyYW1EZWNvcmF0b3JzKSA/IHBhcmFtRGVjb3JhdG9ycy5ib2R5IDogcGFyYW1EZWNvcmF0b3JzO1xuICAgICAgaWYgKHRzLmlzQXJyYXlMaXRlcmFsRXhwcmVzc2lvbihjb250YWluZXIpKSB7XG4gICAgICAgIGNvbnN0IGVsZW1lbnRzID0gY29udGFpbmVyLmVsZW1lbnRzO1xuICAgICAgICByZXR1cm4gZWxlbWVudHNcbiAgICAgICAgICAgIC5tYXAoXG4gICAgICAgICAgICAgICAgZWxlbWVudCA9PlxuICAgICAgICAgICAgICAgICAgICB0cy5pc09iamVjdExpdGVyYWxFeHByZXNzaW9uKGVsZW1lbnQpID8gcmVmbGVjdE9iamVjdExpdGVyYWwoZWxlbWVudCkgOiBudWxsKVxuICAgICAgICAgICAgLm1hcChwYXJhbUluZm8gPT4ge1xuICAgICAgICAgICAgICBjb25zdCB0eXBlRXhwcmVzc2lvbiA9XG4gICAgICAgICAgICAgICAgICBwYXJhbUluZm8gJiYgcGFyYW1JbmZvLmhhcygndHlwZScpID8gcGFyYW1JbmZvLmdldCgndHlwZScpISA6IG51bGw7XG4gICAgICAgICAgICAgIGNvbnN0IGRlY29yYXRvckluZm8gPVxuICAgICAgICAgICAgICAgICAgcGFyYW1JbmZvICYmIHBhcmFtSW5mby5oYXMoJ2RlY29yYXRvcnMnKSA/IHBhcmFtSW5mby5nZXQoJ2RlY29yYXRvcnMnKSEgOiBudWxsO1xuICAgICAgICAgICAgICBjb25zdCBkZWNvcmF0b3JzID0gZGVjb3JhdG9ySW5mbyAmJlxuICAgICAgICAgICAgICAgICAgdGhpcy5yZWZsZWN0RGVjb3JhdG9ycyhkZWNvcmF0b3JJbmZvKVxuICAgICAgICAgICAgICAgICAgICAgIC5maWx0ZXIoZGVjb3JhdG9yID0+IHRoaXMuaXNGcm9tQ29yZShkZWNvcmF0b3IpKTtcbiAgICAgICAgICAgICAgcmV0dXJuIHt0eXBlRXhwcmVzc2lvbiwgZGVjb3JhdG9yc307XG4gICAgICAgICAgICB9KTtcbiAgICAgIH0gZWxzZSBpZiAocGFyYW1EZWNvcmF0b3JzICE9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgdGhpcy5sb2dnZXIud2FybihcbiAgICAgICAgICAgICdJbnZhbGlkIGNvbnN0cnVjdG9yIHBhcmFtZXRlciBkZWNvcmF0b3IgaW4gJyArXG4gICAgICAgICAgICAgICAgcGFyYW1EZWNvcmF0b3JzLmdldFNvdXJjZUZpbGUoKS5maWxlTmFtZSArICc6XFxuJyxcbiAgICAgICAgICAgIHBhcmFtRGVjb3JhdG9ycy5nZXRUZXh0KCkpO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuXG4gIC8qKlxuICAgKiBTZWFyY2ggc3RhdGVtZW50cyByZWxhdGVkIHRvIHRoZSBnaXZlbiBjbGFzcyBmb3IgY2FsbHMgdG8gdGhlIHNwZWNpZmllZCBoZWxwZXIuXG4gICAqIEBwYXJhbSBjbGFzc1N5bWJvbCB0aGUgY2xhc3Mgd2hvc2UgaGVscGVyIGNhbGxzIHdlIGFyZSBpbnRlcmVzdGVkIGluLlxuICAgKiBAcGFyYW0gaGVscGVyTmFtZXMgdGhlIG5hbWVzIG9mIHRoZSBoZWxwZXJzIChlLmcuIGBfX2RlY29yYXRlYCkgd2hvc2UgY2FsbHMgd2UgYXJlIGludGVyZXN0ZWRcbiAgICogaW4uXG4gICAqIEByZXR1cm5zIGFuIGFycmF5IG9mIENhbGxFeHByZXNzaW9uIG5vZGVzIGZvciBlYWNoIG1hdGNoaW5nIGhlbHBlciBjYWxsLlxuICAgKi9cbiAgcHJvdGVjdGVkIGdldEhlbHBlckNhbGxzRm9yQ2xhc3MoY2xhc3NTeW1ib2w6IE5nY2NDbGFzc1N5bWJvbCwgaGVscGVyTmFtZXM6IHN0cmluZ1tdKTpcbiAgICAgIHRzLkNhbGxFeHByZXNzaW9uW10ge1xuICAgIHJldHVybiB0aGlzLmdldFN0YXRlbWVudHNGb3JDbGFzcyhjbGFzc1N5bWJvbClcbiAgICAgICAgLm1hcChzdGF0ZW1lbnQgPT4gdGhpcy5nZXRIZWxwZXJDYWxsKHN0YXRlbWVudCwgaGVscGVyTmFtZXMpKVxuICAgICAgICAuZmlsdGVyKGlzRGVmaW5lZCk7XG4gIH1cblxuICAvKipcbiAgICogRmluZCBzdGF0ZW1lbnRzIHJlbGF0ZWQgdG8gdGhlIGdpdmVuIGNsYXNzIHRoYXQgbWF5IGNvbnRhaW4gY2FsbHMgdG8gYSBoZWxwZXIuXG4gICAqXG4gICAqIEluIEVTTTIwMTUgY29kZSB0aGUgaGVscGVyIGNhbGxzIGFyZSBpbiB0aGUgdG9wIGxldmVsIG1vZHVsZSwgc28gd2UgaGF2ZSB0byBjb25zaWRlclxuICAgKiBhbGwgdGhlIHN0YXRlbWVudHMgaW4gdGhlIG1vZHVsZS5cbiAgICpcbiAgICogQHBhcmFtIGNsYXNzU3ltYm9sIHRoZSBjbGFzcyB3aG9zZSBoZWxwZXIgY2FsbHMgd2UgYXJlIGludGVyZXN0ZWQgaW4uXG4gICAqIEByZXR1cm5zIGFuIGFycmF5IG9mIHN0YXRlbWVudHMgdGhhdCBtYXkgY29udGFpbiBoZWxwZXIgY2FsbHMuXG4gICAqL1xuICBwcm90ZWN0ZWQgZ2V0U3RhdGVtZW50c0ZvckNsYXNzKGNsYXNzU3ltYm9sOiBOZ2NjQ2xhc3NTeW1ib2wpOiB0cy5TdGF0ZW1lbnRbXSB7XG4gICAgY29uc3QgY2xhc3NOb2RlID0gY2xhc3NTeW1ib2wuaW1wbGVtZW50YXRpb24udmFsdWVEZWNsYXJhdGlvbjtcbiAgICBpZiAoaXNUb3BMZXZlbChjbGFzc05vZGUpKSB7XG4gICAgICByZXR1cm4gdGhpcy5nZXRNb2R1bGVTdGF0ZW1lbnRzKGNsYXNzTm9kZS5nZXRTb3VyY2VGaWxlKCkpO1xuICAgIH1cbiAgICBjb25zdCBzdGF0ZW1lbnQgPSBnZXRDb250YWluaW5nU3RhdGVtZW50KGNsYXNzTm9kZSk7XG4gICAgaWYgKHRzLmlzQmxvY2soc3RhdGVtZW50LnBhcmVudCkpIHtcbiAgICAgIHJldHVybiBBcnJheS5mcm9tKHN0YXRlbWVudC5wYXJlbnQuc3RhdGVtZW50cyk7XG4gICAgfVxuICAgIC8vIFdlIHNob3VsZCBuZXZlciBhcnJpdmUgaGVyZVxuICAgIHRocm93IG5ldyBFcnJvcihgVW5hYmxlIHRvIGZpbmQgYWRqYWNlbnQgc3RhdGVtZW50cyBmb3IgJHtjbGFzc1N5bWJvbC5uYW1lfWApO1xuICB9XG5cbiAgLyoqXG4gICAqIFRlc3Qgd2hldGhlciBhIGRlY29yYXRvciB3YXMgaW1wb3J0ZWQgZnJvbSBgQGFuZ3VsYXIvY29yZWAuXG4gICAqXG4gICAqIElzIHRoZSBkZWNvcmF0b3I6XG4gICAqICogZXh0ZXJuYWxseSBpbXBvcnRlZCBmcm9tIGBAYW5ndWxhci9jb3JlYD9cbiAgICogKiB0aGUgY3VycmVudCBob3N0ZWQgcHJvZ3JhbSBpcyBhY3R1YWxseSBgQGFuZ3VsYXIvY29yZWAgYW5kXG4gICAqICAgLSByZWxhdGl2ZWx5IGludGVybmFsbHkgaW1wb3J0ZWQ7IG9yXG4gICAqICAgLSBub3QgaW1wb3J0ZWQsIGZyb20gdGhlIGN1cnJlbnQgZmlsZS5cbiAgICpcbiAgICogQHBhcmFtIGRlY29yYXRvciB0aGUgZGVjb3JhdG9yIHRvIHRlc3QuXG4gICAqL1xuICBwcm90ZWN0ZWQgaXNGcm9tQ29yZShkZWNvcmF0b3I6IERlY29yYXRvcik6IGJvb2xlYW4ge1xuICAgIGlmICh0aGlzLmlzQ29yZSkge1xuICAgICAgcmV0dXJuICFkZWNvcmF0b3IuaW1wb3J0IHx8IC9eXFwuLy50ZXN0KGRlY29yYXRvci5pbXBvcnQuZnJvbSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiAhIWRlY29yYXRvci5pbXBvcnQgJiYgZGVjb3JhdG9yLmltcG9ydC5mcm9tID09PSAnQGFuZ3VsYXIvY29yZSc7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIENyZWF0ZSBhIG1hcHBpbmcgYmV0d2VlbiB0aGUgcHVibGljIGV4cG9ydHMgaW4gYSBzcmMgcHJvZ3JhbSBhbmQgdGhlIHB1YmxpYyBleHBvcnRzIG9mIGEgZHRzXG4gICAqIHByb2dyYW0uXG4gICAqXG4gICAqIEBwYXJhbSBzcmMgdGhlIHByb2dyYW0gYnVuZGxlIGNvbnRhaW5pbmcgdGhlIHNvdXJjZSBmaWxlcy5cbiAgICogQHBhcmFtIGR0cyB0aGUgcHJvZ3JhbSBidW5kbGUgY29udGFpbmluZyB0aGUgdHlwaW5ncyBmaWxlcy5cbiAgICogQHJldHVybnMgYSBtYXAgb2Ygc291cmNlIGRlY2xhcmF0aW9ucyB0byB0eXBpbmdzIGRlY2xhcmF0aW9ucy5cbiAgICovXG4gIHByb3RlY3RlZCBjb21wdXRlUHVibGljRHRzRGVjbGFyYXRpb25NYXAoc3JjOiBCdW5kbGVQcm9ncmFtLCBkdHM6IEJ1bmRsZVByb2dyYW0pOlxuICAgICAgTWFwPHRzLkRlY2xhcmF0aW9uLCB0cy5EZWNsYXJhdGlvbj4ge1xuICAgIGNvbnN0IGRlY2xhcmF0aW9uTWFwID0gbmV3IE1hcDx0cy5EZWNsYXJhdGlvbiwgdHMuRGVjbGFyYXRpb24+KCk7XG4gICAgY29uc3QgZHRzRGVjbGFyYXRpb25NYXAgPSBuZXcgTWFwPHN0cmluZywgdHMuRGVjbGFyYXRpb24+KCk7XG4gICAgY29uc3Qgcm9vdER0cyA9IGdldFJvb3RGaWxlT3JGYWlsKGR0cyk7XG4gICAgdGhpcy5jb2xsZWN0RHRzRXhwb3J0ZWREZWNsYXJhdGlvbnMoZHRzRGVjbGFyYXRpb25NYXAsIHJvb3REdHMsIGR0cy5wcm9ncmFtLmdldFR5cGVDaGVja2VyKCkpO1xuICAgIGNvbnN0IHJvb3RTcmMgPSBnZXRSb290RmlsZU9yRmFpbChzcmMpO1xuICAgIHRoaXMuY29sbGVjdFNyY0V4cG9ydGVkRGVjbGFyYXRpb25zKGRlY2xhcmF0aW9uTWFwLCBkdHNEZWNsYXJhdGlvbk1hcCwgcm9vdFNyYyk7XG4gICAgcmV0dXJuIGRlY2xhcmF0aW9uTWFwO1xuICB9XG5cbiAgLyoqXG4gICAqIENyZWF0ZSBhIG1hcHBpbmcgYmV0d2VlbiB0aGUgXCJwcml2YXRlXCIgZXhwb3J0cyBpbiBhIHNyYyBwcm9ncmFtIGFuZCB0aGUgXCJwcml2YXRlXCIgZXhwb3J0cyBvZiBhXG4gICAqIGR0cyBwcm9ncmFtLiBUaGVzZSBleHBvcnRzIG1heSBiZSBleHBvcnRlZCBmcm9tIGluZGl2aWR1YWwgZmlsZXMgaW4gdGhlIHNyYyBvciBkdHMgcHJvZ3JhbXMsXG4gICAqIGJ1dCBub3QgZXhwb3J0ZWQgZnJvbSB0aGUgcm9vdCBmaWxlIChpLmUgcHVibGljbHkgZnJvbSB0aGUgZW50cnktcG9pbnQpLlxuICAgKlxuICAgKiBUaGlzIG1hcHBpbmcgaXMgYSBcImJlc3QgZ3Vlc3NcIiBzaW5jZSB3ZSBjYW5ub3QgZ3VhcmFudGVlIHRoYXQgdHdvIGRlY2xhcmF0aW9ucyB0aGF0IGhhcHBlbiB0b1xuICAgKiBiZSBleHBvcnRlZCBmcm9tIGEgZmlsZSB3aXRoIHRoZSBzYW1lIG5hbWUgYXJlIGFjdHVhbGx5IGVxdWl2YWxlbnQuIEJ1dCB0aGlzIGlzIGEgcmVhc29uYWJsZVxuICAgKiBlc3RpbWF0ZSBmb3IgdGhlIHB1cnBvc2VzIG9mIG5nY2MuXG4gICAqXG4gICAqIEBwYXJhbSBzcmMgdGhlIHByb2dyYW0gYnVuZGxlIGNvbnRhaW5pbmcgdGhlIHNvdXJjZSBmaWxlcy5cbiAgICogQHBhcmFtIGR0cyB0aGUgcHJvZ3JhbSBidW5kbGUgY29udGFpbmluZyB0aGUgdHlwaW5ncyBmaWxlcy5cbiAgICogQHJldHVybnMgYSBtYXAgb2Ygc291cmNlIGRlY2xhcmF0aW9ucyB0byB0eXBpbmdzIGRlY2xhcmF0aW9ucy5cbiAgICovXG4gIHByb3RlY3RlZCBjb21wdXRlUHJpdmF0ZUR0c0RlY2xhcmF0aW9uTWFwKHNyYzogQnVuZGxlUHJvZ3JhbSwgZHRzOiBCdW5kbGVQcm9ncmFtKTpcbiAgICAgIE1hcDx0cy5EZWNsYXJhdGlvbiwgdHMuRGVjbGFyYXRpb24+IHtcbiAgICBjb25zdCBkZWNsYXJhdGlvbk1hcCA9IG5ldyBNYXA8dHMuRGVjbGFyYXRpb24sIHRzLkRlY2xhcmF0aW9uPigpO1xuICAgIGNvbnN0IGR0c0RlY2xhcmF0aW9uTWFwID0gbmV3IE1hcDxzdHJpbmcsIHRzLkRlY2xhcmF0aW9uPigpO1xuICAgIGNvbnN0IHR5cGVDaGVja2VyID0gZHRzLnByb2dyYW0uZ2V0VHlwZUNoZWNrZXIoKTtcblxuICAgIGNvbnN0IGR0c0ZpbGVzID0gZ2V0Tm9uUm9vdFBhY2thZ2VGaWxlcyhkdHMpO1xuICAgIGZvciAoY29uc3QgZHRzRmlsZSBvZiBkdHNGaWxlcykge1xuICAgICAgdGhpcy5jb2xsZWN0RHRzRXhwb3J0ZWREZWNsYXJhdGlvbnMoZHRzRGVjbGFyYXRpb25NYXAsIGR0c0ZpbGUsIHR5cGVDaGVja2VyKTtcbiAgICB9XG5cbiAgICBjb25zdCBzcmNGaWxlcyA9IGdldE5vblJvb3RQYWNrYWdlRmlsZXMoc3JjKTtcbiAgICBmb3IgKGNvbnN0IHNyY0ZpbGUgb2Ygc3JjRmlsZXMpIHtcbiAgICAgIHRoaXMuY29sbGVjdFNyY0V4cG9ydGVkRGVjbGFyYXRpb25zKGRlY2xhcmF0aW9uTWFwLCBkdHNEZWNsYXJhdGlvbk1hcCwgc3JjRmlsZSk7XG4gICAgfVxuICAgIHJldHVybiBkZWNsYXJhdGlvbk1hcDtcbiAgfVxuXG4gIC8qKlxuICAgKiBDb2xsZWN0IG1hcHBpbmdzIGJldHdlZW4gbmFtZXMgb2YgZXhwb3J0ZWQgZGVjbGFyYXRpb25zIGluIGEgZmlsZSBhbmQgaXRzIGFjdHVhbCBkZWNsYXJhdGlvbi5cbiAgICpcbiAgICogQW55IG5ldyBtYXBwaW5ncyBhcmUgYWRkZWQgdG8gdGhlIGBkdHNEZWNsYXJhdGlvbk1hcGAuXG4gICAqL1xuICBwcm90ZWN0ZWQgY29sbGVjdER0c0V4cG9ydGVkRGVjbGFyYXRpb25zKFxuICAgICAgZHRzRGVjbGFyYXRpb25NYXA6IE1hcDxzdHJpbmcsIHRzLkRlY2xhcmF0aW9uPiwgc3JjRmlsZTogdHMuU291cmNlRmlsZSxcbiAgICAgIGNoZWNrZXI6IHRzLlR5cGVDaGVja2VyKTogdm9pZCB7XG4gICAgY29uc3Qgc3JjTW9kdWxlID0gc3JjRmlsZSAmJiBjaGVja2VyLmdldFN5bWJvbEF0TG9jYXRpb24oc3JjRmlsZSk7XG4gICAgY29uc3QgbW9kdWxlRXhwb3J0cyA9IHNyY01vZHVsZSAmJiBjaGVja2VyLmdldEV4cG9ydHNPZk1vZHVsZShzcmNNb2R1bGUpO1xuICAgIGlmIChtb2R1bGVFeHBvcnRzKSB7XG4gICAgICBtb2R1bGVFeHBvcnRzLmZvckVhY2goZXhwb3J0ZWRTeW1ib2wgPT4ge1xuICAgICAgICBjb25zdCBuYW1lID0gZXhwb3J0ZWRTeW1ib2wubmFtZTtcbiAgICAgICAgaWYgKGV4cG9ydGVkU3ltYm9sLmZsYWdzICYgdHMuU3ltYm9sRmxhZ3MuQWxpYXMpIHtcbiAgICAgICAgICBleHBvcnRlZFN5bWJvbCA9IGNoZWNrZXIuZ2V0QWxpYXNlZFN5bWJvbChleHBvcnRlZFN5bWJvbCk7XG4gICAgICAgIH1cbiAgICAgICAgY29uc3QgZGVjbGFyYXRpb24gPSBleHBvcnRlZFN5bWJvbC52YWx1ZURlY2xhcmF0aW9uO1xuICAgICAgICBpZiAoZGVjbGFyYXRpb24gJiYgIWR0c0RlY2xhcmF0aW9uTWFwLmhhcyhuYW1lKSkge1xuICAgICAgICAgIGR0c0RlY2xhcmF0aW9uTWFwLnNldChuYW1lLCBkZWNsYXJhdGlvbik7XG4gICAgICAgIH1cbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuXG5cbiAgcHJvdGVjdGVkIGNvbGxlY3RTcmNFeHBvcnRlZERlY2xhcmF0aW9ucyhcbiAgICAgIGRlY2xhcmF0aW9uTWFwOiBNYXA8dHMuRGVjbGFyYXRpb24sIHRzLkRlY2xhcmF0aW9uPixcbiAgICAgIGR0c0RlY2xhcmF0aW9uTWFwOiBNYXA8c3RyaW5nLCB0cy5EZWNsYXJhdGlvbj4sIHNyY0ZpbGU6IHRzLlNvdXJjZUZpbGUpOiB2b2lkIHtcbiAgICBjb25zdCBmaWxlRXhwb3J0cyA9IHRoaXMuZ2V0RXhwb3J0c09mTW9kdWxlKHNyY0ZpbGUpO1xuICAgIGlmIChmaWxlRXhwb3J0cyAhPT0gbnVsbCkge1xuICAgICAgZm9yIChjb25zdCBbZXhwb3J0TmFtZSwge25vZGU6IGRlY2xhcmF0aW9ufV0gb2YgZmlsZUV4cG9ydHMpIHtcbiAgICAgICAgaWYgKGRlY2xhcmF0aW9uICE9PSBudWxsICYmIGR0c0RlY2xhcmF0aW9uTWFwLmhhcyhleHBvcnROYW1lKSkge1xuICAgICAgICAgIGRlY2xhcmF0aW9uTWFwLnNldChkZWNsYXJhdGlvbiwgZHRzRGVjbGFyYXRpb25NYXAuZ2V0KGV4cG9ydE5hbWUpISk7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICBwcm90ZWN0ZWQgZ2V0RGVjbGFyYXRpb25PZkV4cHJlc3Npb24oZXhwcmVzc2lvbjogdHMuRXhwcmVzc2lvbik6IERlY2xhcmF0aW9ufG51bGwge1xuICAgIGlmICh0cy5pc0lkZW50aWZpZXIoZXhwcmVzc2lvbikpIHtcbiAgICAgIHJldHVybiB0aGlzLmdldERlY2xhcmF0aW9uT2ZJZGVudGlmaWVyKGV4cHJlc3Npb24pO1xuICAgIH1cblxuICAgIGlmICghdHMuaXNQcm9wZXJ0eUFjY2Vzc0V4cHJlc3Npb24oZXhwcmVzc2lvbikgfHwgIXRzLmlzSWRlbnRpZmllcihleHByZXNzaW9uLmV4cHJlc3Npb24pKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG5cbiAgICBjb25zdCBuYW1lc3BhY2VEZWNsID0gdGhpcy5nZXREZWNsYXJhdGlvbk9mSWRlbnRpZmllcihleHByZXNzaW9uLmV4cHJlc3Npb24pO1xuICAgIGlmICghbmFtZXNwYWNlRGVjbCB8fCBuYW1lc3BhY2VEZWNsLm5vZGUgPT09IG51bGwgfHwgIXRzLmlzU291cmNlRmlsZShuYW1lc3BhY2VEZWNsLm5vZGUpKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG5cbiAgICBjb25zdCBuYW1lc3BhY2VFeHBvcnRzID0gdGhpcy5nZXRFeHBvcnRzT2ZNb2R1bGUobmFtZXNwYWNlRGVjbC5ub2RlKTtcbiAgICBpZiAobmFtZXNwYWNlRXhwb3J0cyA9PT0gbnVsbCkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuXG4gICAgaWYgKCFuYW1lc3BhY2VFeHBvcnRzLmhhcyhleHByZXNzaW9uLm5hbWUudGV4dCkpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIGNvbnN0IGV4cG9ydERlY2wgPSBuYW1lc3BhY2VFeHBvcnRzLmdldChleHByZXNzaW9uLm5hbWUudGV4dCkhO1xuICAgIHJldHVybiB7Li4uZXhwb3J0RGVjbCwgdmlhTW9kdWxlOiBuYW1lc3BhY2VEZWNsLnZpYU1vZHVsZX07XG4gIH1cblxuICAvKiogQ2hlY2tzIGlmIHRoZSBzcGVjaWZpZWQgZGVjbGFyYXRpb24gcmVzb2x2ZXMgdG8gdGhlIGtub3duIEphdmFTY3JpcHQgZ2xvYmFsIGBPYmplY3RgLiAqL1xuICBwcm90ZWN0ZWQgaXNKYXZhU2NyaXB0T2JqZWN0RGVjbGFyYXRpb24oZGVjbDogRGVjbGFyYXRpb24pOiBib29sZWFuIHtcbiAgICBpZiAoZGVjbC5ub2RlID09PSBudWxsKSB7XG4gICAgICByZXR1cm4gZmFsc2U7XG4gICAgfVxuICAgIGNvbnN0IG5vZGUgPSBkZWNsLm5vZGU7XG4gICAgLy8gVGhlIGRlZmF1bHQgVHlwZVNjcmlwdCBsaWJyYXJ5IHR5cGVzIHRoZSBnbG9iYWwgYE9iamVjdGAgdmFyaWFibGUgdGhyb3VnaFxuICAgIC8vIGEgdmFyaWFibGUgZGVjbGFyYXRpb24gd2l0aCBhIHR5cGUgcmVmZXJlbmNlIHJlc29sdmluZyB0byBgT2JqZWN0Q29uc3RydWN0b3JgLlxuICAgIGlmICghdHMuaXNWYXJpYWJsZURlY2xhcmF0aW9uKG5vZGUpIHx8ICF0cy5pc0lkZW50aWZpZXIobm9kZS5uYW1lKSB8fFxuICAgICAgICBub2RlLm5hbWUudGV4dCAhPT0gJ09iamVjdCcgfHwgbm9kZS50eXBlID09PSB1bmRlZmluZWQpIHtcbiAgICAgIHJldHVybiBmYWxzZTtcbiAgICB9XG4gICAgY29uc3QgdHlwZU5vZGUgPSBub2RlLnR5cGU7XG4gICAgLy8gSWYgdGhlIHZhcmlhYmxlIGRlY2xhcmF0aW9uIGRvZXMgbm90IGhhdmUgYSB0eXBlIHJlc29sdmluZyB0byBgT2JqZWN0Q29uc3RydWN0b3JgLFxuICAgIC8vIHdlIGNhbm5vdCBndWFyYW50ZWUgdGhhdCB0aGUgZGVjbGFyYXRpb24gcmVzb2x2ZXMgdG8gdGhlIGdsb2JhbCBgT2JqZWN0YCB2YXJpYWJsZS5cbiAgICBpZiAoIXRzLmlzVHlwZVJlZmVyZW5jZU5vZGUodHlwZU5vZGUpIHx8ICF0cy5pc0lkZW50aWZpZXIodHlwZU5vZGUudHlwZU5hbWUpIHx8XG4gICAgICAgIHR5cGVOb2RlLnR5cGVOYW1lLnRleHQgIT09ICdPYmplY3RDb25zdHJ1Y3RvcicpIHtcbiAgICAgIHJldHVybiBmYWxzZTtcbiAgICB9XG4gICAgLy8gRmluYWxseSwgY2hlY2sgaWYgdGhlIHR5cGUgZGVmaW5pdGlvbiBmb3IgYE9iamVjdGAgb3JpZ2luYXRlcyBmcm9tIGEgZGVmYXVsdCBsaWJyYXJ5XG4gICAgLy8gZGVmaW5pdGlvbiBmaWxlLiBUaGlzIHJlcXVpcmVzIGRlZmF1bHQgdHlwZXMgdG8gYmUgZW5hYmxlZCBmb3IgdGhlIGhvc3QgcHJvZ3JhbS5cbiAgICByZXR1cm4gdGhpcy5zcmMucHJvZ3JhbS5pc1NvdXJjZUZpbGVEZWZhdWx0TGlicmFyeShub2RlLmdldFNvdXJjZUZpbGUoKSk7XG4gIH1cblxuICAvKipcbiAgICogSW4gSmF2YVNjcmlwdCwgZW51bSBkZWNsYXJhdGlvbnMgYXJlIGVtaXR0ZWQgYXMgYSByZWd1bGFyIHZhcmlhYmxlIGRlY2xhcmF0aW9uIGZvbGxvd2VkIGJ5IGFuXG4gICAqIElJRkUgaW4gd2hpY2ggdGhlIGVudW0gbWVtYmVycyBhcmUgYXNzaWduZWQuXG4gICAqXG4gICAqICAgZXhwb3J0IHZhciBFbnVtO1xuICAgKiAgIChmdW5jdGlvbiAoRW51bSkge1xuICAgKiAgICAgRW51bVtcImFcIl0gPSBcIkFcIjtcbiAgICogICAgIEVudW1bXCJiXCJdID0gXCJCXCI7XG4gICAqICAgfSkoRW51bSB8fCAoRW51bSA9IHt9KSk7XG4gICAqXG4gICAqIEBwYXJhbSBkZWNsYXJhdGlvbiBBIHZhcmlhYmxlIGRlY2xhcmF0aW9uIHRoYXQgbWF5IHJlcHJlc2VudCBhbiBlbnVtXG4gICAqIEByZXR1cm5zIEFuIGFycmF5IG9mIGVudW0gbWVtYmVycyBpZiB0aGUgdmFyaWFibGUgZGVjbGFyYXRpb24gaXMgZm9sbG93ZWQgYnkgYW4gSUlGRSB0aGF0XG4gICAqIGRlY2xhcmVzIHRoZSBlbnVtIG1lbWJlcnMsIG9yIG51bGwgb3RoZXJ3aXNlLlxuICAgKi9cbiAgcHJvdGVjdGVkIHJlc29sdmVFbnVtTWVtYmVycyhkZWNsYXJhdGlvbjogdHMuVmFyaWFibGVEZWNsYXJhdGlvbik6IEVudW1NZW1iZXJbXXxudWxsIHtcbiAgICAvLyBJbml0aWFsaXplZCB2YXJpYWJsZXMgZG9uJ3QgcmVwcmVzZW50IGVudW0gZGVjbGFyYXRpb25zLlxuICAgIGlmIChkZWNsYXJhdGlvbi5pbml0aWFsaXplciAhPT0gdW5kZWZpbmVkKSByZXR1cm4gbnVsbDtcblxuICAgIGNvbnN0IHZhcmlhYmxlU3RtdCA9IGRlY2xhcmF0aW9uLnBhcmVudC5wYXJlbnQ7XG4gICAgaWYgKCF0cy5pc1ZhcmlhYmxlU3RhdGVtZW50KHZhcmlhYmxlU3RtdCkpIHJldHVybiBudWxsO1xuXG4gICAgY29uc3QgYmxvY2sgPSB2YXJpYWJsZVN0bXQucGFyZW50O1xuICAgIGlmICghdHMuaXNCbG9jayhibG9jaykgJiYgIXRzLmlzU291cmNlRmlsZShibG9jaykpIHJldHVybiBudWxsO1xuXG4gICAgY29uc3QgZGVjbGFyYXRpb25JbmRleCA9IGJsb2NrLnN0YXRlbWVudHMuZmluZEluZGV4KHN0YXRlbWVudCA9PiBzdGF0ZW1lbnQgPT09IHZhcmlhYmxlU3RtdCk7XG4gICAgaWYgKGRlY2xhcmF0aW9uSW5kZXggPT09IC0xIHx8IGRlY2xhcmF0aW9uSW5kZXggPT09IGJsb2NrLnN0YXRlbWVudHMubGVuZ3RoIC0gMSkgcmV0dXJuIG51bGw7XG5cbiAgICBjb25zdCBzdWJzZXF1ZW50U3RtdCA9IGJsb2NrLnN0YXRlbWVudHNbZGVjbGFyYXRpb25JbmRleCArIDFdO1xuICAgIGlmICghdHMuaXNFeHByZXNzaW9uU3RhdGVtZW50KHN1YnNlcXVlbnRTdG10KSkgcmV0dXJuIG51bGw7XG5cbiAgICBjb25zdCBpaWZlID0gc3RyaXBQYXJlbnRoZXNlcyhzdWJzZXF1ZW50U3RtdC5leHByZXNzaW9uKTtcbiAgICBpZiAoIXRzLmlzQ2FsbEV4cHJlc3Npb24oaWlmZSkgfHwgIWlzRW51bURlY2xhcmF0aW9uSWlmZShpaWZlKSkgcmV0dXJuIG51bGw7XG5cbiAgICBjb25zdCBmbiA9IHN0cmlwUGFyZW50aGVzZXMoaWlmZS5leHByZXNzaW9uKTtcbiAgICBpZiAoIXRzLmlzRnVuY3Rpb25FeHByZXNzaW9uKGZuKSkgcmV0dXJuIG51bGw7XG5cbiAgICByZXR1cm4gdGhpcy5yZWZsZWN0RW51bU1lbWJlcnMoZm4pO1xuICB9XG5cbiAgLyoqXG4gICAqIEF0dGVtcHRzIHRvIGV4dHJhY3QgYWxsIGBFbnVtTWVtYmVyYHMgZnJvbSBhIGZ1bmN0aW9uIHRoYXQgaXMgYWNjb3JkaW5nIHRvIHRoZSBKYXZhU2NyaXB0IGVtaXRcbiAgICogZm9ybWF0IGZvciBlbnVtczpcbiAgICpcbiAgICogICBmdW5jdGlvbiAoRW51bSkge1xuICAgKiAgICAgRW51bVtcIk1lbWJlckFcIl0gPSBcImFcIjtcbiAgICogICAgIEVudW1bXCJNZW1iZXJCXCJdID0gXCJiXCI7XG4gICAqICAgfVxuICAgKlxuICAgKiBAcGFyYW0gZm4gVGhlIGZ1bmN0aW9uIGV4cHJlc3Npb24gdGhhdCBpcyBhc3N1bWVkIHRvIGNvbnRhaW4gZW51bSBtZW1iZXJzLlxuICAgKiBAcmV0dXJucyBBbGwgZW51bSBtZW1iZXJzIGlmIHRoZSBmdW5jdGlvbiBpcyBhY2NvcmRpbmcgdG8gdGhlIGNvcnJlY3Qgc3ludGF4LCBudWxsIG90aGVyd2lzZS5cbiAgICovXG4gIHByaXZhdGUgcmVmbGVjdEVudW1NZW1iZXJzKGZuOiB0cy5GdW5jdGlvbkV4cHJlc3Npb24pOiBFbnVtTWVtYmVyW118bnVsbCB7XG4gICAgaWYgKGZuLnBhcmFtZXRlcnMubGVuZ3RoICE9PSAxKSByZXR1cm4gbnVsbDtcblxuICAgIGNvbnN0IGVudW1OYW1lID0gZm4ucGFyYW1ldGVyc1swXS5uYW1lO1xuICAgIGlmICghdHMuaXNJZGVudGlmaWVyKGVudW1OYW1lKSkgcmV0dXJuIG51bGw7XG5cbiAgICBjb25zdCBlbnVtTWVtYmVyczogRW51bU1lbWJlcltdID0gW107XG4gICAgZm9yIChjb25zdCBzdGF0ZW1lbnQgb2YgZm4uYm9keS5zdGF0ZW1lbnRzKSB7XG4gICAgICBjb25zdCBlbnVtTWVtYmVyID0gdGhpcy5yZWZsZWN0RW51bU1lbWJlcihlbnVtTmFtZSwgc3RhdGVtZW50KTtcbiAgICAgIGlmIChlbnVtTWVtYmVyID09PSBudWxsKSB7XG4gICAgICAgIHJldHVybiBudWxsO1xuICAgICAgfVxuICAgICAgZW51bU1lbWJlcnMucHVzaChlbnVtTWVtYmVyKTtcbiAgICB9XG4gICAgcmV0dXJuIGVudW1NZW1iZXJzO1xuICB9XG5cbiAgLyoqXG4gICAqIEF0dGVtcHRzIHRvIGV4dHJhY3QgYSBzaW5nbGUgYEVudW1NZW1iZXJgIGZyb20gYSBzdGF0ZW1lbnQgaW4gdGhlIGZvbGxvd2luZyBzeW50YXg6XG4gICAqXG4gICAqICAgRW51bVtcIk1lbWJlckFcIl0gPSBcImFcIjtcbiAgICpcbiAgICogb3IsIGZvciBlbnVtIG1lbWJlciB3aXRoIG51bWVyaWMgdmFsdWVzOlxuICAgKlxuICAgKiAgIEVudW1bRW51bVtcIk1lbWJlckFcIl0gPSAwXSA9IFwiTWVtYmVyQVwiO1xuICAgKlxuICAgKiBAcGFyYW0gZW51bU5hbWUgVGhlIGlkZW50aWZpZXIgb2YgdGhlIGVudW0gdGhhdCB0aGUgbWVtYmVycyBzaG91bGQgYmUgc2V0IG9uLlxuICAgKiBAcGFyYW0gc3RhdGVtZW50IFRoZSBzdGF0ZW1lbnQgdG8gaW5zcGVjdC5cbiAgICogQHJldHVybnMgQW4gYEVudW1NZW1iZXJgIGlmIHRoZSBzdGF0ZW1lbnQgaXMgYWNjb3JkaW5nIHRvIHRoZSBleHBlY3RlZCBzeW50YXgsIG51bGwgb3RoZXJ3aXNlLlxuICAgKi9cbiAgcHJvdGVjdGVkIHJlZmxlY3RFbnVtTWVtYmVyKGVudW1OYW1lOiB0cy5JZGVudGlmaWVyLCBzdGF0ZW1lbnQ6IHRzLlN0YXRlbWVudCk6IEVudW1NZW1iZXJ8bnVsbCB7XG4gICAgaWYgKCF0cy5pc0V4cHJlc3Npb25TdGF0ZW1lbnQoc3RhdGVtZW50KSkgcmV0dXJuIG51bGw7XG5cbiAgICBjb25zdCBleHByZXNzaW9uID0gc3RhdGVtZW50LmV4cHJlc3Npb247XG5cbiAgICAvLyBDaGVjayBmb3IgdGhlIGBFbnVtW1hdID0gWTtgIGNhc2UuXG4gICAgaWYgKCFpc0VudW1Bc3NpZ25tZW50KGVudW1OYW1lLCBleHByZXNzaW9uKSkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIGNvbnN0IGFzc2lnbm1lbnQgPSByZWZsZWN0RW51bUFzc2lnbm1lbnQoZXhwcmVzc2lvbik7XG4gICAgaWYgKGFzc2lnbm1lbnQgIT0gbnVsbCkge1xuICAgICAgcmV0dXJuIGFzc2lnbm1lbnQ7XG4gICAgfVxuXG4gICAgLy8gQ2hlY2sgZm9yIHRoZSBgRW51bVtFbnVtW1hdID0gWV0gPSAuLi47YCBjYXNlLlxuICAgIGNvbnN0IGlubmVyRXhwcmVzc2lvbiA9IGV4cHJlc3Npb24ubGVmdC5hcmd1bWVudEV4cHJlc3Npb247XG4gICAgaWYgKCFpc0VudW1Bc3NpZ25tZW50KGVudW1OYW1lLCBpbm5lckV4cHJlc3Npb24pKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG4gICAgcmV0dXJuIHJlZmxlY3RFbnVtQXNzaWdubWVudChpbm5lckV4cHJlc3Npb24pO1xuICB9XG59XG5cbi8vLy8vLy8vLy8vLy8gRXhwb3J0ZWQgSGVscGVycyAvLy8vLy8vLy8vLy8vXG5cbi8qKlxuICogQ2hlY2tzIHdoZXRoZXIgdGhlIGlpZmUgaGFzIHRoZSBmb2xsb3dpbmcgY2FsbCBzaWduYXR1cmU6XG4gKlxuICogICAoRW51bSB8fCAoRW51bSA9IHt9KVxuICpcbiAqIE5vdGUgdGhhdCB0aGUgYEVudW1gIGlkZW50aWZpZXIgaXMgbm90IGNoZWNrZWQsIGFzIGl0IGNvdWxkIGFsc28gYmUgc29tZXRoaW5nXG4gKiBsaWtlIGBleHBvcnRzLkVudW1gLiBJbnN0ZWFkLCBvbmx5IHRoZSBzdHJ1Y3R1cmUgb2YgYmluYXJ5IG9wZXJhdG9ycyBpcyBjaGVja2VkLlxuICpcbiAqIEBwYXJhbSBpaWZlIFRoZSBjYWxsIGV4cHJlc3Npb24gdG8gY2hlY2suXG4gKiBAcmV0dXJucyB0cnVlIGlmIHRoZSBpaWZlIGhhcyBhIGNhbGwgc2lnbmF0dXJlIHRoYXQgY29ycmVzcG9uZHMgd2l0aCBhIHBvdGVudGlhbFxuICogZW51bSBkZWNsYXJhdGlvbi5cbiAqL1xuZnVuY3Rpb24gaXNFbnVtRGVjbGFyYXRpb25JaWZlKGlpZmU6IHRzLkNhbGxFeHByZXNzaW9uKTogYm9vbGVhbiB7XG4gIGlmIChpaWZlLmFyZ3VtZW50cy5sZW5ndGggIT09IDEpIHJldHVybiBmYWxzZTtcblxuICBjb25zdCBhcmcgPSBpaWZlLmFyZ3VtZW50c1swXTtcbiAgaWYgKCF0cy5pc0JpbmFyeUV4cHJlc3Npb24oYXJnKSB8fCBhcmcub3BlcmF0b3JUb2tlbi5raW5kICE9PSB0cy5TeW50YXhLaW5kLkJhckJhclRva2VuIHx8XG4gICAgICAhdHMuaXNQYXJlbnRoZXNpemVkRXhwcmVzc2lvbihhcmcucmlnaHQpKSB7XG4gICAgcmV0dXJuIGZhbHNlO1xuICB9XG5cbiAgY29uc3QgcmlnaHQgPSBhcmcucmlnaHQuZXhwcmVzc2lvbjtcbiAgaWYgKCF0cy5pc0JpbmFyeUV4cHJlc3Npb24ocmlnaHQpIHx8IHJpZ2h0Lm9wZXJhdG9yVG9rZW4ua2luZCAhPT0gdHMuU3ludGF4S2luZC5FcXVhbHNUb2tlbikge1xuICAgIHJldHVybiBmYWxzZTtcbiAgfVxuXG4gIGlmICghdHMuaXNPYmplY3RMaXRlcmFsRXhwcmVzc2lvbihyaWdodC5yaWdodCkgfHwgcmlnaHQucmlnaHQucHJvcGVydGllcy5sZW5ndGggIT09IDApIHtcbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cblxuICByZXR1cm4gdHJ1ZTtcbn1cblxuLyoqXG4gKiBBbiBlbnVtIG1lbWJlciBhc3NpZ25tZW50IHRoYXQgbG9va3MgbGlrZSBgRW51bVtYXSA9IFk7YC5cbiAqL1xuZXhwb3J0IHR5cGUgRW51bU1lbWJlckFzc2lnbm1lbnQgPSB0cy5CaW5hcnlFeHByZXNzaW9uJntsZWZ0OiB0cy5FbGVtZW50QWNjZXNzRXhwcmVzc2lvbn07XG5cbi8qKlxuICogQ2hlY2tzIHdoZXRoZXIgdGhlIGV4cHJlc3Npb24gbG9va3MgbGlrZSBhbiBlbnVtIG1lbWJlciBhc3NpZ25tZW50IHRhcmdldGluZyBgRW51bWA6XG4gKlxuICogICBFbnVtW1hdID0gWTtcbiAqXG4gKiBIZXJlLCBYIGFuZCBZIGNhbiBiZSBhbnkgZXhwcmVzc2lvbi5cbiAqXG4gKiBAcGFyYW0gZW51bU5hbWUgVGhlIGlkZW50aWZpZXIgb2YgdGhlIGVudW0gdGhhdCB0aGUgbWVtYmVycyBzaG91bGQgYmUgc2V0IG9uLlxuICogQHBhcmFtIGV4cHJlc3Npb24gVGhlIGV4cHJlc3Npb24gdGhhdCBzaG91bGQgYmUgY2hlY2tlZCB0byBjb25mb3JtIHRvIHRoZSBhYm92ZSBmb3JtLlxuICogQHJldHVybnMgdHJ1ZSBpZiB0aGUgZXhwcmVzc2lvbiBpcyBvZiB0aGUgY29ycmVjdCBmb3JtLCBmYWxzZSBvdGhlcndpc2UuXG4gKi9cbmZ1bmN0aW9uIGlzRW51bUFzc2lnbm1lbnQoXG4gICAgZW51bU5hbWU6IHRzLklkZW50aWZpZXIsIGV4cHJlc3Npb246IHRzLkV4cHJlc3Npb24pOiBleHByZXNzaW9uIGlzIEVudW1NZW1iZXJBc3NpZ25tZW50IHtcbiAgaWYgKCF0cy5pc0JpbmFyeUV4cHJlc3Npb24oZXhwcmVzc2lvbikgfHxcbiAgICAgIGV4cHJlc3Npb24ub3BlcmF0b3JUb2tlbi5raW5kICE9PSB0cy5TeW50YXhLaW5kLkVxdWFsc1Rva2VuIHx8XG4gICAgICAhdHMuaXNFbGVtZW50QWNjZXNzRXhwcmVzc2lvbihleHByZXNzaW9uLmxlZnQpKSB7XG4gICAgcmV0dXJuIGZhbHNlO1xuICB9XG5cbiAgLy8gVmVyaWZ5IHRoYXQgdGhlIG91dGVyIGFzc2lnbm1lbnQgY29ycmVzcG9uZHMgd2l0aCB0aGUgZW51bSBkZWNsYXJhdGlvbi5cbiAgY29uc3QgZW51bUlkZW50aWZpZXIgPSBleHByZXNzaW9uLmxlZnQuZXhwcmVzc2lvbjtcbiAgcmV0dXJuIHRzLmlzSWRlbnRpZmllcihlbnVtSWRlbnRpZmllcikgJiYgZW51bUlkZW50aWZpZXIudGV4dCA9PT0gZW51bU5hbWUudGV4dDtcbn1cblxuLyoqXG4gKiBBdHRlbXB0cyB0byBjcmVhdGUgYW4gYEVudW1NZW1iZXJgIGZyb20gYW4gZXhwcmVzc2lvbiB0aGF0IGlzIGJlbGlldmVkIHRvIHJlcHJlc2VudCBhbiBlbnVtXG4gKiBhc3NpZ25tZW50LlxuICpcbiAqIEBwYXJhbSBleHByZXNzaW9uIFRoZSBleHByZXNzaW9uIHRoYXQgaXMgYmVsaWV2ZWQgdG8gYmUgYW4gZW51bSBhc3NpZ25tZW50LlxuICogQHJldHVybnMgQW4gYEVudW1NZW1iZXJgIG9yIG51bGwgaWYgdGhlIGV4cHJlc3Npb24gZGlkIG5vdCByZXByZXNlbnQgYW4gZW51bSBtZW1iZXIgYWZ0ZXIgYWxsLlxuICovXG5mdW5jdGlvbiByZWZsZWN0RW51bUFzc2lnbm1lbnQoZXhwcmVzc2lvbjogRW51bU1lbWJlckFzc2lnbm1lbnQpOiBFbnVtTWVtYmVyfG51bGwge1xuICBjb25zdCBtZW1iZXJOYW1lID0gZXhwcmVzc2lvbi5sZWZ0LmFyZ3VtZW50RXhwcmVzc2lvbjtcbiAgaWYgKCF0cy5pc1Byb3BlcnR5TmFtZShtZW1iZXJOYW1lKSkgcmV0dXJuIG51bGw7XG5cbiAgcmV0dXJuIHtuYW1lOiBtZW1iZXJOYW1lLCBpbml0aWFsaXplcjogZXhwcmVzc2lvbi5yaWdodH07XG59XG5cbmV4cG9ydCB0eXBlIFBhcmFtSW5mbyA9IHtcbiAgZGVjb3JhdG9yczogRGVjb3JhdG9yW118bnVsbCxcbiAgdHlwZUV4cHJlc3Npb246IHRzLkV4cHJlc3Npb258bnVsbFxufTtcblxuLyoqXG4gKiBSZXByZXNlbnRzIGEgY2FsbCB0byBgdHNsaWIuX19tZXRhZGF0YWAgYXMgcHJlc2VudCBpbiBgdHNsaWIuX19kZWNvcmF0ZWAgY2FsbHMuIFRoaXMgaXMgYVxuICogc3ludGhldGljIGRlY29yYXRvciBpbnNlcnRlZCBieSBUeXBlU2NyaXB0IHRoYXQgY29udGFpbnMgcmVmbGVjdGlvbiBpbmZvcm1hdGlvbiBhYm91dCB0aGVcbiAqIHRhcmdldCBvZiB0aGUgZGVjb3JhdG9yLCBpLmUuIHRoZSBjbGFzcyBvciBwcm9wZXJ0eS5cbiAqL1xuZXhwb3J0IGludGVyZmFjZSBQYXJhbWV0ZXJUeXBlcyB7XG4gIHR5cGU6ICdwYXJhbXMnO1xuICB0eXBlczogdHMuRXhwcmVzc2lvbltdO1xufVxuXG4vKipcbiAqIFJlcHJlc2VudHMgYSBjYWxsIHRvIGB0c2xpYi5fX3BhcmFtYCBhcyBwcmVzZW50IGluIGB0c2xpYi5fX2RlY29yYXRlYCBjYWxscy4gVGhpcyBjb250YWluc1xuICogaW5mb3JtYXRpb24gb24gYW55IGRlY29yYXRvcnMgd2VyZSBhcHBsaWVkIHRvIGEgY2VydGFpbiBwYXJhbWV0ZXIuXG4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgUGFyYW1ldGVyRGVjb3JhdG9ycyB7XG4gIHR5cGU6ICdwYXJhbTpkZWNvcmF0b3JzJztcbiAgaW5kZXg6IG51bWJlcjtcbiAgZGVjb3JhdG9yOiBEZWNvcmF0b3I7XG59XG5cbi8qKlxuICogUmVwcmVzZW50cyBhIGNhbGwgdG8gYSBkZWNvcmF0b3IgYXMgaXQgd2FzIHByZXNlbnQgaW4gdGhlIG9yaWdpbmFsIHNvdXJjZSBjb2RlLCBhcyBwcmVzZW50IGluXG4gKiBgdHNsaWIuX19kZWNvcmF0ZWAgY2FsbHMuXG4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgRGVjb3JhdG9yQ2FsbCB7XG4gIHR5cGU6ICdkZWNvcmF0b3InO1xuICBkZWNvcmF0b3I6IERlY29yYXRvcjtcbn1cblxuLyoqXG4gKiBSZXByZXNlbnRzIHRoZSBkaWZmZXJlbnQga2luZHMgb2YgZGVjb3JhdGUgaGVscGVycyB0aGF0IG1heSBiZSBwcmVzZW50IGFzIGZpcnN0IGFyZ3VtZW50IHRvXG4gKiBgdHNsaWIuX19kZWNvcmF0ZWAsIGFzIGZvbGxvd3M6XG4gKlxuICogYGBgXG4gKiBfX2RlY29yYXRlKFtcbiAqICAgRGlyZWN0aXZlKHsgc2VsZWN0b3I6ICdbc29tZURpcmVjdGl2ZV0nIH0pLFxuICogICB0c2xpYl8xLl9fcGFyYW0oMiwgSW5qZWN0KElOSkVDVEVEX1RPS0VOKSksXG4gKiAgIHRzbGliXzEuX19tZXRhZGF0YShcImRlc2lnbjpwYXJhbXR5cGVzXCIsIFtWaWV3Q29udGFpbmVyUmVmLCBUZW1wbGF0ZVJlZiwgU3RyaW5nXSlcbiAqIF0sIFNvbWVEaXJlY3RpdmUpO1xuICogYGBgXG4gKi9cbmV4cG9ydCB0eXBlIERlY29yYXRlSGVscGVyRW50cnkgPSBQYXJhbWV0ZXJUeXBlc3xQYXJhbWV0ZXJEZWNvcmF0b3JzfERlY29yYXRvckNhbGw7XG5cbi8qKlxuICogVGhlIHJlY29yZGVkIGRlY29yYXRvciBpbmZvcm1hdGlvbiBvZiBhIHNpbmdsZSBjbGFzcy4gVGhpcyBpbmZvcm1hdGlvbiBpcyBjYWNoZWQgaW4gdGhlIGhvc3QuXG4gKi9cbmludGVyZmFjZSBEZWNvcmF0b3JJbmZvIHtcbiAgLyoqXG4gICAqIEFsbCBkZWNvcmF0b3JzIHRoYXQgd2VyZSBwcmVzZW50IG9uIHRoZSBjbGFzcy4gSWYgbm8gZGVjb3JhdG9ycyB3ZXJlIHByZXNlbnQsIHRoaXMgaXMgYG51bGxgXG4gICAqL1xuICBjbGFzc0RlY29yYXRvcnM6IERlY29yYXRvcltdfG51bGw7XG5cbiAgLyoqXG4gICAqIEFsbCBkZWNvcmF0b3JzIHBlciBtZW1iZXIgb2YgdGhlIGNsYXNzIHRoZXkgd2VyZSBwcmVzZW50IG9uLlxuICAgKi9cbiAgbWVtYmVyRGVjb3JhdG9yczogTWFwPHN0cmluZywgRGVjb3JhdG9yW10+O1xuXG4gIC8qKlxuICAgKiBSZXByZXNlbnRzIHRoZSBjb25zdHJ1Y3RvciBwYXJhbWV0ZXIgaW5mb3JtYXRpb24sIHN1Y2ggYXMgdGhlIHR5cGUgb2YgYSBwYXJhbWV0ZXIgYW5kIGFsbFxuICAgKiBkZWNvcmF0b3JzIGZvciBhIGNlcnRhaW4gcGFyYW1ldGVyLiBJbmRpY2VzIGluIHRoaXMgYXJyYXkgY29ycmVzcG9uZCB3aXRoIHRoZSBwYXJhbWV0ZXInc1xuICAgKiBpbmRleCBpbiB0aGUgY29uc3RydWN0b3IuIE5vdGUgdGhhdCB0aGlzIGFycmF5IG1heSBiZSBzcGFyc2UsIGkuZS4gY2VydGFpbiBjb25zdHJ1Y3RvclxuICAgKiBwYXJhbWV0ZXJzIG1heSBub3QgaGF2ZSBhbnkgaW5mbyByZWNvcmRlZC5cbiAgICovXG4gIGNvbnN0cnVjdG9yUGFyYW1JbmZvOiBQYXJhbUluZm9bXTtcbn1cblxuLyoqXG4gKiBBIHN0YXRlbWVudCBub2RlIHRoYXQgcmVwcmVzZW50cyBhbiBhc3NpZ25tZW50LlxuICovXG5leHBvcnQgdHlwZSBBc3NpZ25tZW50U3RhdGVtZW50ID1cbiAgICB0cy5FeHByZXNzaW9uU3RhdGVtZW50JntleHByZXNzaW9uOiB7bGVmdDogdHMuSWRlbnRpZmllciwgcmlnaHQ6IHRzLkV4cHJlc3Npb259fTtcblxuLyoqXG4gKiBUZXN0IHdoZXRoZXIgYSBzdGF0ZW1lbnQgbm9kZSBpcyBhbiBhc3NpZ25tZW50IHN0YXRlbWVudC5cbiAqIEBwYXJhbSBzdGF0ZW1lbnQgdGhlIHN0YXRlbWVudCB0byB0ZXN0LlxuICovXG5leHBvcnQgZnVuY3Rpb24gaXNBc3NpZ25tZW50U3RhdGVtZW50KHN0YXRlbWVudDogdHMuU3RhdGVtZW50KTogc3RhdGVtZW50IGlzIEFzc2lnbm1lbnRTdGF0ZW1lbnQge1xuICByZXR1cm4gdHMuaXNFeHByZXNzaW9uU3RhdGVtZW50KHN0YXRlbWVudCkgJiYgaXNBc3NpZ25tZW50KHN0YXRlbWVudC5leHByZXNzaW9uKSAmJlxuICAgICAgdHMuaXNJZGVudGlmaWVyKHN0YXRlbWVudC5leHByZXNzaW9uLmxlZnQpO1xufVxuXG4vKipcbiAqIFBhcnNlIHRoZSBgZXhwcmVzc2lvbmAgdGhhdCBpcyBiZWxpZXZlZCB0byBiZSBhbiBJSUZFIGFuZCByZXR1cm4gdGhlIEFTVCBub2RlIHRoYXQgY29ycmVzcG9uZHMgdG9cbiAqIHRoZSBib2R5IG9mIHRoZSBJSUZFLlxuICpcbiAqIFRoZSBleHByZXNzaW9uIG1heSBiZSB3cmFwcGVkIGluIHBhcmVudGhlc2VzLCB3aGljaCBhcmUgc3RyaXBwZWQgb2ZmLlxuICpcbiAqIElmIHRoZSBJSUZFIGlzIGFuIGFycm93IGZ1bmN0aW9uIHRoZW4gaXRzIGJvZHkgY291bGQgYmUgYSBgdHMuRXhwcmVzc2lvbmAgcmF0aGVyIHRoYW4gYVxuICogYHRzLkZ1bmN0aW9uQm9keWAuXG4gKlxuICogQHBhcmFtIGV4cHJlc3Npb24gdGhlIGV4cHJlc3Npb24gdG8gcGFyc2UuXG4gKiBAcmV0dXJucyB0aGUgYHRzLkV4cHJlc3Npb25gIG9yIGB0cy5GdW5jdGlvbkJvZHlgIHRoYXQgaG9sZHMgdGhlIGJvZHkgb2YgdGhlIElJRkUgb3IgYHVuZGVmaW5lZGBcbiAqICAgICBpZiB0aGUgYGV4cHJlc3Npb25gIGRpZCBub3QgaGF2ZSB0aGUgY29ycmVjdCBzaGFwZS5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGdldElpZmVCb2R5KGV4cHJlc3Npb246IHRzLkV4cHJlc3Npb24pOiB0cy5Db25jaXNlQm9keXx1bmRlZmluZWQge1xuICBjb25zdCBjYWxsID0gc3RyaXBQYXJlbnRoZXNlcyhleHByZXNzaW9uKTtcbiAgaWYgKCF0cy5pc0NhbGxFeHByZXNzaW9uKGNhbGwpKSB7XG4gICAgcmV0dXJuIHVuZGVmaW5lZDtcbiAgfVxuXG4gIGNvbnN0IGZuID0gc3RyaXBQYXJlbnRoZXNlcyhjYWxsLmV4cHJlc3Npb24pO1xuICBpZiAoIXRzLmlzRnVuY3Rpb25FeHByZXNzaW9uKGZuKSAmJiAhdHMuaXNBcnJvd0Z1bmN0aW9uKGZuKSkge1xuICAgIHJldHVybiB1bmRlZmluZWQ7XG4gIH1cblxuICByZXR1cm4gZm4uYm9keTtcbn1cblxuLyoqXG4gKiBSZXR1cm5zIHRydWUgaWYgdGhlIGBub2RlYCBpcyBhbiBhc3NpZ25tZW50IG9mIHRoZSBmb3JtIGBhID0gYmAuXG4gKlxuICogQHBhcmFtIG5vZGUgVGhlIEFTVCBub2RlIHRvIGNoZWNrLlxuICovXG5leHBvcnQgZnVuY3Rpb24gaXNBc3NpZ25tZW50KG5vZGU6IHRzLk5vZGUpOiBub2RlIGlzIHRzLkFzc2lnbm1lbnRFeHByZXNzaW9uPHRzLkVxdWFsc1Rva2VuPiB7XG4gIHJldHVybiB0cy5pc0JpbmFyeUV4cHJlc3Npb24obm9kZSkgJiYgbm9kZS5vcGVyYXRvclRva2VuLmtpbmQgPT09IHRzLlN5bnRheEtpbmQuRXF1YWxzVG9rZW47XG59XG5cbi8qKlxuICogVGVzdHMgd2hldGhlciB0aGUgcHJvdmlkZWQgY2FsbCBleHByZXNzaW9uIHRhcmdldHMgYSBjbGFzcywgYnkgdmVyaWZ5aW5nIGl0cyBhcmd1bWVudHMgYXJlXG4gKiBhY2NvcmRpbmcgdG8gdGhlIGZvbGxvd2luZyBmb3JtOlxuICpcbiAqIGBgYFxuICogX19kZWNvcmF0ZShbXSwgU29tZURpcmVjdGl2ZSk7XG4gKiBgYGBcbiAqXG4gKiBAcGFyYW0gY2FsbCB0aGUgY2FsbCBleHByZXNzaW9uIHRoYXQgaXMgdGVzdGVkIHRvIHJlcHJlc2VudCBhIGNsYXNzIGRlY29yYXRvciBjYWxsLlxuICogQHBhcmFtIG1hdGNoZXMgcHJlZGljYXRlIGZ1bmN0aW9uIHRvIHRlc3Qgd2hldGhlciB0aGUgY2FsbCBpcyBhc3NvY2lhdGVkIHdpdGggdGhlIGRlc2lyZWQgY2xhc3MuXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBpc0NsYXNzRGVjb3JhdGVDYWxsKFxuICAgIGNhbGw6IHRzLkNhbGxFeHByZXNzaW9uLCBtYXRjaGVzOiAoaWRlbnRpZmllcjogdHMuSWRlbnRpZmllcikgPT4gYm9vbGVhbik6XG4gICAgY2FsbCBpcyB0cy5DYWxsRXhwcmVzc2lvbiZ7YXJndW1lbnRzOiBbdHMuQXJyYXlMaXRlcmFsRXhwcmVzc2lvbiwgdHMuRXhwcmVzc2lvbl19IHtcbiAgY29uc3QgaGVscGVyQXJncyA9IGNhbGwuYXJndW1lbnRzWzBdO1xuICBpZiAoaGVscGVyQXJncyA9PT0gdW5kZWZpbmVkIHx8ICF0cy5pc0FycmF5TGl0ZXJhbEV4cHJlc3Npb24oaGVscGVyQXJncykpIHtcbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cblxuICBjb25zdCB0YXJnZXQgPSBjYWxsLmFyZ3VtZW50c1sxXTtcbiAgcmV0dXJuIHRhcmdldCAhPT0gdW5kZWZpbmVkICYmIHRzLmlzSWRlbnRpZmllcih0YXJnZXQpICYmIG1hdGNoZXModGFyZ2V0KTtcbn1cblxuLyoqXG4gKiBUZXN0cyB3aGV0aGVyIHRoZSBwcm92aWRlZCBjYWxsIGV4cHJlc3Npb24gdGFyZ2V0cyBhIG1lbWJlciBvZiB0aGUgY2xhc3MsIGJ5IHZlcmlmeWluZyBpdHNcbiAqIGFyZ3VtZW50cyBhcmUgYWNjb3JkaW5nIHRvIHRoZSBmb2xsb3dpbmcgZm9ybTpcbiAqXG4gKiBgYGBcbiAqIF9fZGVjb3JhdGUoW10sIFNvbWVEaXJlY3RpdmUucHJvdG90eXBlLCBcIm1lbWJlclwiLCB2b2lkIDApO1xuICogYGBgXG4gKlxuICogQHBhcmFtIGNhbGwgdGhlIGNhbGwgZXhwcmVzc2lvbiB0aGF0IGlzIHRlc3RlZCB0byByZXByZXNlbnQgYSBtZW1iZXIgZGVjb3JhdG9yIGNhbGwuXG4gKiBAcGFyYW0gbWF0Y2hlcyBwcmVkaWNhdGUgZnVuY3Rpb24gdG8gdGVzdCB3aGV0aGVyIHRoZSBjYWxsIGlzIGFzc29jaWF0ZWQgd2l0aCB0aGUgZGVzaXJlZCBjbGFzcy5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGlzTWVtYmVyRGVjb3JhdGVDYWxsKFxuICAgIGNhbGw6IHRzLkNhbGxFeHByZXNzaW9uLCBtYXRjaGVzOiAoaWRlbnRpZmllcjogdHMuSWRlbnRpZmllcikgPT4gYm9vbGVhbik6XG4gICAgY2FsbCBpcyB0cy5DYWxsRXhwcmVzc2lvbiZcbiAgICB7YXJndW1lbnRzOiBbdHMuQXJyYXlMaXRlcmFsRXhwcmVzc2lvbiwgdHMuU3RyaW5nTGl0ZXJhbCwgdHMuU3RyaW5nTGl0ZXJhbF19IHtcbiAgY29uc3QgaGVscGVyQXJncyA9IGNhbGwuYXJndW1lbnRzWzBdO1xuICBpZiAoaGVscGVyQXJncyA9PT0gdW5kZWZpbmVkIHx8ICF0cy5pc0FycmF5TGl0ZXJhbEV4cHJlc3Npb24oaGVscGVyQXJncykpIHtcbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cblxuICBjb25zdCB0YXJnZXQgPSBjYWxsLmFyZ3VtZW50c1sxXTtcbiAgaWYgKHRhcmdldCA9PT0gdW5kZWZpbmVkIHx8ICF0cy5pc1Byb3BlcnR5QWNjZXNzRXhwcmVzc2lvbih0YXJnZXQpIHx8XG4gICAgICAhdHMuaXNJZGVudGlmaWVyKHRhcmdldC5leHByZXNzaW9uKSB8fCAhbWF0Y2hlcyh0YXJnZXQuZXhwcmVzc2lvbikgfHxcbiAgICAgIHRhcmdldC5uYW1lLnRleHQgIT09ICdwcm90b3R5cGUnKSB7XG4gICAgcmV0dXJuIGZhbHNlO1xuICB9XG5cbiAgY29uc3QgbWVtYmVyTmFtZSA9IGNhbGwuYXJndW1lbnRzWzJdO1xuICByZXR1cm4gbWVtYmVyTmFtZSAhPT0gdW5kZWZpbmVkICYmIHRzLmlzU3RyaW5nTGl0ZXJhbChtZW1iZXJOYW1lKTtcbn1cblxuLyoqXG4gKiBIZWxwZXIgbWV0aG9kIHRvIGV4dHJhY3QgdGhlIHZhbHVlIG9mIGEgcHJvcGVydHkgZ2l2ZW4gdGhlIHByb3BlcnR5J3MgXCJzeW1ib2xcIixcbiAqIHdoaWNoIGlzIGFjdHVhbGx5IHRoZSBzeW1ib2wgb2YgdGhlIGlkZW50aWZpZXIgb2YgdGhlIHByb3BlcnR5LlxuICovXG5leHBvcnQgZnVuY3Rpb24gZ2V0UHJvcGVydHlWYWx1ZUZyb21TeW1ib2wocHJvcFN5bWJvbDogdHMuU3ltYm9sKTogdHMuRXhwcmVzc2lvbnx1bmRlZmluZWQge1xuICBjb25zdCBwcm9wSWRlbnRpZmllciA9IHByb3BTeW1ib2wudmFsdWVEZWNsYXJhdGlvbjtcbiAgY29uc3QgcGFyZW50ID0gcHJvcElkZW50aWZpZXIgJiYgcHJvcElkZW50aWZpZXIucGFyZW50O1xuICByZXR1cm4gcGFyZW50ICYmIHRzLmlzQmluYXJ5RXhwcmVzc2lvbihwYXJlbnQpID8gcGFyZW50LnJpZ2h0IDogdW5kZWZpbmVkO1xufVxuXG4vKipcbiAqIEEgY2FsbGVlIGNvdWxkIGJlIG9uZSBvZjogYF9fZGVjb3JhdGUoLi4uKWAgb3IgYHRzbGliXzEuX19kZWNvcmF0ZWAuXG4gKi9cbmZ1bmN0aW9uIGdldENhbGxlZU5hbWUoY2FsbDogdHMuQ2FsbEV4cHJlc3Npb24pOiBzdHJpbmd8bnVsbCB7XG4gIGlmICh0cy5pc0lkZW50aWZpZXIoY2FsbC5leHByZXNzaW9uKSkge1xuICAgIHJldHVybiBzdHJpcERvbGxhclN1ZmZpeChjYWxsLmV4cHJlc3Npb24udGV4dCk7XG4gIH1cbiAgaWYgKHRzLmlzUHJvcGVydHlBY2Nlc3NFeHByZXNzaW9uKGNhbGwuZXhwcmVzc2lvbikpIHtcbiAgICByZXR1cm4gc3RyaXBEb2xsYXJTdWZmaXgoY2FsbC5leHByZXNzaW9uLm5hbWUudGV4dCk7XG4gIH1cbiAgcmV0dXJuIG51bGw7XG59XG5cbi8vLy8vLy8vLy8vLy8gSW50ZXJuYWwgSGVscGVycyAvLy8vLy8vLy8vLy8vXG5cbnR5cGUgSW5pdGlhbGl6ZWRWYXJpYWJsZUNsYXNzRGVjbGFyYXRpb24gPVxuICAgIENsYXNzRGVjbGFyYXRpb248dHMuVmFyaWFibGVEZWNsYXJhdGlvbj4me2luaXRpYWxpemVyOiB0cy5FeHByZXNzaW9ufTtcblxuZnVuY3Rpb24gaXNJbml0aWFsaXplZFZhcmlhYmxlQ2xhc3NEZWNsYXJhdGlvbihub2RlOiB0cy5Ob2RlKTpcbiAgICBub2RlIGlzIEluaXRpYWxpemVkVmFyaWFibGVDbGFzc0RlY2xhcmF0aW9uIHtcbiAgcmV0dXJuIGlzTmFtZWRWYXJpYWJsZURlY2xhcmF0aW9uKG5vZGUpICYmIG5vZGUuaW5pdGlhbGl6ZXIgIT09IHVuZGVmaW5lZDtcbn1cbi8qKlxuICogSGFuZGxlIGEgdmFyaWFibGUgZGVjbGFyYXRpb24gb2YgdGhlIGZvcm1cbiAqXG4gKiBgYGBcbiAqIHZhciBNeUNsYXNzID0gYWxpYXMxID0gYWxpYXMyID0gPDxkZWNsYXJhdGlvbj4+XG4gKiBgYGBcbiAqXG4gKiBAbm9kZSB0aGUgTEhTIG9mIGEgdmFyaWFibGUgZGVjbGFyYXRpb24uXG4gKiBAcmV0dXJucyB0aGUgb3JpZ2luYWwgQVNUIG5vZGUgb3IgdGhlIFJIUyBvZiBhIHNlcmllcyBvZiBhc3NpZ25tZW50cyBpbiBhIHZhcmlhYmxlXG4gKiAgICAgZGVjbGFyYXRpb24uXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBza2lwQ2xhc3NBbGlhc2VzKG5vZGU6IEluaXRpYWxpemVkVmFyaWFibGVDbGFzc0RlY2xhcmF0aW9uKTogdHMuRXhwcmVzc2lvbiB7XG4gIGxldCBleHByZXNzaW9uID0gbm9kZS5pbml0aWFsaXplcjtcbiAgd2hpbGUgKGlzQXNzaWdubWVudChleHByZXNzaW9uKSkge1xuICAgIGV4cHJlc3Npb24gPSBleHByZXNzaW9uLnJpZ2h0O1xuICB9XG4gIHJldHVybiBleHByZXNzaW9uO1xufVxuXG4vKipcbiAqIFRoaXMgZXhwcmVzc2lvbiBjb3VsZCBlaXRoZXIgYmUgYSBjbGFzcyBleHByZXNzaW9uXG4gKlxuICogYGBgXG4gKiBjbGFzcyBNeUNsYXNzIHt9O1xuICogYGBgXG4gKlxuICogb3IgYW4gSUlGRSB3cmFwcGVkIGNsYXNzIGV4cHJlc3Npb25cbiAqXG4gKiBgYGBcbiAqICgoKSA9PiB7XG4gKiAgIGNsYXNzIE15Q2xhc3Mge31cbiAqICAgLi4uXG4gKiAgIHJldHVybiBNeUNsYXNzO1xuICogfSkoKVxuICogYGBgXG4gKlxuICogb3IgYW4gSUlGRSB3cmFwcGVkIGFsaWFzZWQgY2xhc3MgZXhwcmVzc2lvblxuICpcbiAqIGBgYFxuICogKCgpID0+IHtcbiAqICAgbGV0IE15Q2xhc3MgPSBjbGFzcyBNeUNsYXNzIHt9XG4gKiAgIC4uLlxuICogICByZXR1cm4gTXlDbGFzcztcbiAqIH0pKClcbiAqIGBgYFxuICpcbiAqIG9yIGFuIElGRkUgd3JhcHBlZCBFUzUgY2xhc3MgZnVuY3Rpb25cbiAqXG4gKiBgYGBcbiAqIChmdW5jdGlvbiAoKSB7XG4gKiAgZnVuY3Rpb24gTXlDbGFzcygpIHt9XG4gKiAgLi4uXG4gKiAgcmV0dXJuIE15Q2xhc3NcbiAqIH0pKClcbiAqIGBgYFxuICpcbiAqIEBwYXJhbSBleHByZXNzaW9uIHRoZSBub2RlIHRoYXQgcmVwcmVzZW50cyB0aGUgY2xhc3Mgd2hvc2UgZGVjbGFyYXRpb24gd2UgYXJlIGZpbmRpbmcuXG4gKiBAcmV0dXJucyB0aGUgZGVjbGFyYXRpb24gb2YgdGhlIGNsYXNzIG9yIGBudWxsYCBpZiBpdCBpcyBub3QgYSBcImNsYXNzXCIuXG4gKi9cbmZ1bmN0aW9uIGdldElubmVyQ2xhc3NEZWNsYXJhdGlvbihleHByZXNzaW9uOiB0cy5FeHByZXNzaW9uKTpcbiAgICBDbGFzc0RlY2xhcmF0aW9uPHRzLkNsYXNzRXhwcmVzc2lvbnx0cy5DbGFzc0RlY2xhcmF0aW9ufHRzLkZ1bmN0aW9uRGVjbGFyYXRpb24+fG51bGwge1xuICBpZiAodHMuaXNDbGFzc0V4cHJlc3Npb24oZXhwcmVzc2lvbikgJiYgaGFzTmFtZUlkZW50aWZpZXIoZXhwcmVzc2lvbikpIHtcbiAgICByZXR1cm4gZXhwcmVzc2lvbjtcbiAgfVxuXG4gIGNvbnN0IGlpZmVCb2R5ID0gZ2V0SWlmZUJvZHkoZXhwcmVzc2lvbik7XG4gIGlmIChpaWZlQm9keSA9PT0gdW5kZWZpbmVkKSB7XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuICBpZiAoIXRzLmlzQmxvY2soaWlmZUJvZHkpKSB7XG4gICAgLy8gSGFuZGxlIHRoZSBmYXQgYXJyb3cgZXhwcmVzc2lvbiBjYXNlOiBgKCkgPT4gQ2xhc3NFeHByZXNzaW9uYFxuICAgIHJldHVybiB0cy5pc0NsYXNzRXhwcmVzc2lvbihpaWZlQm9keSkgJiYgaXNOYW1lZERlY2xhcmF0aW9uKGlpZmVCb2R5KSA/IGlpZmVCb2R5IDogbnVsbDtcbiAgfSBlbHNlIHtcbiAgICAvLyBIYW5kbGUgdGhlIGNhc2Ugb2YgYSBub3JtYWwgb3IgZmF0LWFycm93IGZ1bmN0aW9uIHdpdGggYSBib2R5LlxuICAgIC8vIFJldHVybiB0aGUgZmlyc3QgQ2xhc3NEZWNsYXJhdGlvbi9WYXJpYWJsZURlY2xhcmF0aW9uIGluc2lkZSB0aGUgYm9keVxuICAgIGZvciAoY29uc3Qgc3RhdGVtZW50IG9mIGlpZmVCb2R5LnN0YXRlbWVudHMpIHtcbiAgICAgIGlmIChpc05hbWVkQ2xhc3NEZWNsYXJhdGlvbihzdGF0ZW1lbnQpIHx8IGlzTmFtZWRGdW5jdGlvbkRlY2xhcmF0aW9uKHN0YXRlbWVudCkpIHtcbiAgICAgICAgcmV0dXJuIHN0YXRlbWVudDtcbiAgICAgIH1cbiAgICAgIGlmICh0cy5pc1ZhcmlhYmxlU3RhdGVtZW50KHN0YXRlbWVudCkpIHtcbiAgICAgICAgZm9yIChjb25zdCBkZWNsYXJhdGlvbiBvZiBzdGF0ZW1lbnQuZGVjbGFyYXRpb25MaXN0LmRlY2xhcmF0aW9ucykge1xuICAgICAgICAgIGlmIChpc0luaXRpYWxpemVkVmFyaWFibGVDbGFzc0RlY2xhcmF0aW9uKGRlY2xhcmF0aW9uKSkge1xuICAgICAgICAgICAgY29uc3QgZXhwcmVzc2lvbiA9IHNraXBDbGFzc0FsaWFzZXMoZGVjbGFyYXRpb24pO1xuICAgICAgICAgICAgaWYgKHRzLmlzQ2xhc3NFeHByZXNzaW9uKGV4cHJlc3Npb24pICYmIGhhc05hbWVJZGVudGlmaWVyKGV4cHJlc3Npb24pKSB7XG4gICAgICAgICAgICAgIHJldHVybiBleHByZXNzaW9uO1xuICAgICAgICAgICAgfVxuICAgICAgICAgIH1cbiAgICAgICAgfVxuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIHJldHVybiBudWxsO1xufVxuXG5mdW5jdGlvbiBnZXREZWNvcmF0b3JBcmdzKG5vZGU6IHRzLk9iamVjdExpdGVyYWxFeHByZXNzaW9uKTogdHMuRXhwcmVzc2lvbltdIHtcbiAgLy8gVGhlIGFyZ3VtZW50cyBvZiBhIGRlY29yYXRvciBhcmUgaGVsZCBpbiB0aGUgYGFyZ3NgIHByb3BlcnR5IG9mIGl0cyBkZWNsYXJhdGlvbiBvYmplY3QuXG4gIGNvbnN0IGFyZ3NQcm9wZXJ0eSA9IG5vZGUucHJvcGVydGllcy5maWx0ZXIodHMuaXNQcm9wZXJ0eUFzc2lnbm1lbnQpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAuZmluZChwcm9wZXJ0eSA9PiBnZXROYW1lVGV4dChwcm9wZXJ0eS5uYW1lKSA9PT0gJ2FyZ3MnKTtcbiAgY29uc3QgYXJnc0V4cHJlc3Npb24gPSBhcmdzUHJvcGVydHkgJiYgYXJnc1Byb3BlcnR5LmluaXRpYWxpemVyO1xuICByZXR1cm4gYXJnc0V4cHJlc3Npb24gJiYgdHMuaXNBcnJheUxpdGVyYWxFeHByZXNzaW9uKGFyZ3NFeHByZXNzaW9uKSA/XG4gICAgICBBcnJheS5mcm9tKGFyZ3NFeHByZXNzaW9uLmVsZW1lbnRzKSA6XG4gICAgICBbXTtcbn1cblxuZnVuY3Rpb24gaXNQcm9wZXJ0eUFjY2Vzcyhub2RlOiB0cy5Ob2RlKTogbm9kZSBpcyB0cy5Qcm9wZXJ0eUFjY2Vzc0V4cHJlc3Npb24mXG4gICAge3BhcmVudDogdHMuQmluYXJ5RXhwcmVzc2lvbn0ge1xuICByZXR1cm4gISFub2RlLnBhcmVudCAmJiB0cy5pc0JpbmFyeUV4cHJlc3Npb24obm9kZS5wYXJlbnQpICYmIHRzLmlzUHJvcGVydHlBY2Nlc3NFeHByZXNzaW9uKG5vZGUpO1xufVxuXG5mdW5jdGlvbiBpc1RoaXNBc3NpZ25tZW50KG5vZGU6IHRzLkRlY2xhcmF0aW9uKTogbm9kZSBpcyB0cy5CaW5hcnlFeHByZXNzaW9uJlxuICAgIHtsZWZ0OiB0cy5Qcm9wZXJ0eUFjY2Vzc0V4cHJlc3Npb259IHtcbiAgcmV0dXJuIHRzLmlzQmluYXJ5RXhwcmVzc2lvbihub2RlKSAmJiB0cy5pc1Byb3BlcnR5QWNjZXNzRXhwcmVzc2lvbihub2RlLmxlZnQpICYmXG4gICAgICBub2RlLmxlZnQuZXhwcmVzc2lvbi5raW5kID09PSB0cy5TeW50YXhLaW5kLlRoaXNLZXl3b3JkO1xufVxuXG5mdW5jdGlvbiBpc05hbWVkRGVjbGFyYXRpb24obm9kZTogdHMuTm9kZSk6IG5vZGUgaXMgdHMuTmFtZWREZWNsYXJhdGlvbiZ7bmFtZTogdHMuSWRlbnRpZmllcn0ge1xuICBjb25zdCBhbnlOb2RlOiBhbnkgPSBub2RlO1xuICByZXR1cm4gISFhbnlOb2RlLm5hbWUgJiYgdHMuaXNJZGVudGlmaWVyKGFueU5vZGUubmFtZSk7XG59XG5cblxuZnVuY3Rpb24gaXNDbGFzc01lbWJlclR5cGUobm9kZTogdHMuRGVjbGFyYXRpb24pOiBub2RlIGlzIHRzLkNsYXNzRWxlbWVudHxcbiAgICB0cy5Qcm9wZXJ0eUFjY2Vzc0V4cHJlc3Npb258dHMuQmluYXJ5RXhwcmVzc2lvbiB7XG4gIHJldHVybiAodHMuaXNDbGFzc0VsZW1lbnQobm9kZSkgfHwgaXNQcm9wZXJ0eUFjY2Vzcyhub2RlKSB8fCB0cy5pc0JpbmFyeUV4cHJlc3Npb24obm9kZSkpICYmXG4gICAgICAvLyBBZGRpdGlvbmFsbHksIGVuc3VyZSBgbm9kZWAgaXMgbm90IGFuIGluZGV4IHNpZ25hdHVyZSwgZm9yIGV4YW1wbGUgb24gYW4gYWJzdHJhY3QgY2xhc3M6XG4gICAgICAvLyBgYWJzdHJhY3QgY2xhc3MgRm9vIHsgW2tleTogc3RyaW5nXTogYW55OyB9YFxuICAgICAgIXRzLmlzSW5kZXhTaWduYXR1cmVEZWNsYXJhdGlvbihub2RlKTtcbn1cblxuLyoqXG4gKiBBdHRlbXB0IHRvIHJlc29sdmUgdGhlIHZhcmlhYmxlIGRlY2xhcmF0aW9uIHRoYXQgdGhlIGdpdmVuIGRlY2xhcmF0aW9uIGlzIGFzc2lnbmVkIHRvLlxuICogRm9yIGV4YW1wbGUsIGZvciB0aGUgZm9sbG93aW5nIGNvZGU6XG4gKlxuICogYGBgXG4gKiB2YXIgTXlDbGFzcyA9IE15Q2xhc3NfMSA9IGNsYXNzIE15Q2xhc3Mge307XG4gKiBgYGBcbiAqXG4gKiBvclxuICpcbiAqIGBgYFxuICogdmFyIE15Q2xhc3MgPSBNeUNsYXNzXzEgPSAoKCkgPT4ge1xuICogICBjbGFzcyBNeUNsYXNzIHt9XG4gKiAgIC4uLlxuICogICByZXR1cm4gTXlDbGFzcztcbiAqIH0pKClcbiAgYGBgXG4gKlxuICogYW5kIHRoZSBwcm92aWRlZCBkZWNsYXJhdGlvbiBiZWluZyBgY2xhc3MgTXlDbGFzcyB7fWAsIHRoaXMgd2lsbCByZXR1cm4gdGhlIGB2YXIgTXlDbGFzc2BcbiAqIGRlY2xhcmF0aW9uLlxuICpcbiAqIEBwYXJhbSBkZWNsYXJhdGlvbiBUaGUgZGVjbGFyYXRpb24gZm9yIHdoaWNoIGFueSB2YXJpYWJsZSBkZWNsYXJhdGlvbiBzaG91bGQgYmUgb2J0YWluZWQuXG4gKiBAcmV0dXJucyB0aGUgb3V0ZXIgdmFyaWFibGUgZGVjbGFyYXRpb24gaWYgZm91bmQsIHVuZGVmaW5lZCBvdGhlcndpc2UuXG4gKi9cbmZ1bmN0aW9uIGdldEZhckxlZnRIYW5kU2lkZU9mQXNzaWdubWVudChkZWNsYXJhdGlvbjogdHMuRGVjbGFyYXRpb24pOiB0cy5WYXJpYWJsZURlY2xhcmF0aW9ufFxuICAgIHVuZGVmaW5lZCB7XG4gIGxldCBub2RlID0gZGVjbGFyYXRpb24ucGFyZW50O1xuXG4gIC8vIERldGVjdCBhbiBpbnRlcm1lZGlhcnkgdmFyaWFibGUgYXNzaWdubWVudCBhbmQgc2tpcCBvdmVyIGl0LlxuICBpZiAoaXNBc3NpZ25tZW50KG5vZGUpICYmIHRzLmlzSWRlbnRpZmllcihub2RlLmxlZnQpKSB7XG4gICAgbm9kZSA9IG5vZGUucGFyZW50O1xuICB9XG5cbiAgcmV0dXJuIHRzLmlzVmFyaWFibGVEZWNsYXJhdGlvbihub2RlKSA/IG5vZGUgOiB1bmRlZmluZWQ7XG59XG5cbmZ1bmN0aW9uIGdldENvbnRhaW5pbmdWYXJpYWJsZURlY2xhcmF0aW9uKG5vZGU6IHRzLk5vZGUpOiBDbGFzc0RlY2xhcmF0aW9uPHRzLlZhcmlhYmxlRGVjbGFyYXRpb24+fFxuICAgIHVuZGVmaW5lZCB7XG4gIG5vZGUgPSBub2RlLnBhcmVudDtcbiAgd2hpbGUgKG5vZGUgIT09IHVuZGVmaW5lZCkge1xuICAgIGlmIChpc05hbWVkVmFyaWFibGVEZWNsYXJhdGlvbihub2RlKSkge1xuICAgICAgcmV0dXJuIG5vZGU7XG4gICAgfVxuICAgIG5vZGUgPSBub2RlLnBhcmVudDtcbiAgfVxuICByZXR1cm4gdW5kZWZpbmVkO1xufVxuXG4vKipcbiAqIEEgY29uc3RydWN0b3IgZnVuY3Rpb24gbWF5IGhhdmUgYmVlbiBcInN5bnRoZXNpemVkXCIgYnkgVHlwZVNjcmlwdCBkdXJpbmcgSmF2YVNjcmlwdCBlbWl0LFxuICogaW4gdGhlIGNhc2Ugbm8gdXNlci1kZWZpbmVkIGNvbnN0cnVjdG9yIGV4aXN0cyBhbmQgZS5nLiBwcm9wZXJ0eSBpbml0aWFsaXplcnMgYXJlIHVzZWQuXG4gKiBUaG9zZSBpbml0aWFsaXplcnMgbmVlZCB0byBiZSBlbWl0dGVkIGludG8gYSBjb25zdHJ1Y3RvciBpbiBKYXZhU2NyaXB0LCBzbyB0aGUgVHlwZVNjcmlwdFxuICogY29tcGlsZXIgZ2VuZXJhdGVzIGEgc3ludGhldGljIGNvbnN0cnVjdG9yLlxuICpcbiAqIFdlIG5lZWQgdG8gaWRlbnRpZnkgc3VjaCBjb25zdHJ1Y3RvcnMgYXMgbmdjYyBuZWVkcyB0byBiZSBhYmxlIHRvIHRlbGwgaWYgYSBjbGFzcyBkaWRcbiAqIG9yaWdpbmFsbHkgaGF2ZSBhIGNvbnN0cnVjdG9yIGluIHRoZSBUeXBlU2NyaXB0IHNvdXJjZS4gV2hlbiBhIGNsYXNzIGhhcyBhIHN1cGVyY2xhc3MsXG4gKiBhIHN5bnRoZXNpemVkIGNvbnN0cnVjdG9yIG11c3Qgbm90IGJlIGNvbnNpZGVyZWQgYXMgYSB1c2VyLWRlZmluZWQgY29uc3RydWN0b3IgYXMgdGhhdFxuICogcHJldmVudHMgYSBiYXNlIGZhY3RvcnkgY2FsbCBmcm9tIGJlaW5nIGNyZWF0ZWQgYnkgbmd0c2MsIHJlc3VsdGluZyBpbiBhIGZhY3RvcnkgZnVuY3Rpb25cbiAqIHRoYXQgZG9lcyBub3QgaW5qZWN0IHRoZSBkZXBlbmRlbmNpZXMgb2YgdGhlIHN1cGVyY2xhc3MuIEhlbmNlLCB3ZSBpZGVudGlmeSBhIGRlZmF1bHRcbiAqIHN5bnRoZXNpemVkIHN1cGVyIGNhbGwgaW4gdGhlIGNvbnN0cnVjdG9yIGJvZHksIGFjY29yZGluZyB0byB0aGUgc3RydWN0dXJlIHRoYXQgVHlwZVNjcmlwdFxuICogZW1pdHMgZHVyaW5nIEphdmFTY3JpcHQgZW1pdDpcbiAqIGh0dHBzOi8vZ2l0aHViLmNvbS9NaWNyb3NvZnQvVHlwZVNjcmlwdC9ibG9iL3YzLjIuMi9zcmMvY29tcGlsZXIvdHJhbnNmb3JtZXJzL3RzLnRzI0wxMDY4LUwxMDgyXG4gKlxuICogQHBhcmFtIGNvbnN0cnVjdG9yIGEgY29uc3RydWN0b3IgZnVuY3Rpb24gdG8gdGVzdFxuICogQHJldHVybnMgdHJ1ZSBpZiB0aGUgY29uc3RydWN0b3IgYXBwZWFycyB0byBoYXZlIGJlZW4gc3ludGhlc2l6ZWRcbiAqL1xuZnVuY3Rpb24gaXNTeW50aGVzaXplZENvbnN0cnVjdG9yKGNvbnN0cnVjdG9yOiB0cy5Db25zdHJ1Y3RvckRlY2xhcmF0aW9uKTogYm9vbGVhbiB7XG4gIGlmICghY29uc3RydWN0b3IuYm9keSkgcmV0dXJuIGZhbHNlO1xuXG4gIGNvbnN0IGZpcnN0U3RhdGVtZW50ID0gY29uc3RydWN0b3IuYm9keS5zdGF0ZW1lbnRzWzBdO1xuICBpZiAoIWZpcnN0U3RhdGVtZW50IHx8ICF0cy5pc0V4cHJlc3Npb25TdGF0ZW1lbnQoZmlyc3RTdGF0ZW1lbnQpKSByZXR1cm4gZmFsc2U7XG5cbiAgcmV0dXJuIGlzU3ludGhlc2l6ZWRTdXBlckNhbGwoZmlyc3RTdGF0ZW1lbnQuZXhwcmVzc2lvbik7XG59XG5cbi8qKlxuICogVGVzdHMgd2hldGhlciB0aGUgZXhwcmVzc2lvbiBhcHBlYXJzIHRvIGhhdmUgYmVlbiBzeW50aGVzaXplZCBieSBUeXBlU2NyaXB0LCBpLmUuIHdoZXRoZXJcbiAqIGl0IGlzIG9mIHRoZSBmb2xsb3dpbmcgZm9ybTpcbiAqXG4gKiBgYGBcbiAqIHN1cGVyKC4uLmFyZ3VtZW50cyk7XG4gKiBgYGBcbiAqXG4gKiBAcGFyYW0gZXhwcmVzc2lvbiB0aGUgZXhwcmVzc2lvbiB0aGF0IGlzIHRvIGJlIHRlc3RlZFxuICogQHJldHVybnMgdHJ1ZSBpZiB0aGUgZXhwcmVzc2lvbiBhcHBlYXJzIHRvIGJlIGEgc3ludGhlc2l6ZWQgc3VwZXIgY2FsbFxuICovXG5mdW5jdGlvbiBpc1N5bnRoZXNpemVkU3VwZXJDYWxsKGV4cHJlc3Npb246IHRzLkV4cHJlc3Npb24pOiBib29sZWFuIHtcbiAgaWYgKCF0cy5pc0NhbGxFeHByZXNzaW9uKGV4cHJlc3Npb24pKSByZXR1cm4gZmFsc2U7XG4gIGlmIChleHByZXNzaW9uLmV4cHJlc3Npb24ua2luZCAhPT0gdHMuU3ludGF4S2luZC5TdXBlcktleXdvcmQpIHJldHVybiBmYWxzZTtcbiAgaWYgKGV4cHJlc3Npb24uYXJndW1lbnRzLmxlbmd0aCAhPT0gMSkgcmV0dXJuIGZhbHNlO1xuXG4gIGNvbnN0IGFyZ3VtZW50ID0gZXhwcmVzc2lvbi5hcmd1bWVudHNbMF07XG4gIHJldHVybiB0cy5pc1NwcmVhZEVsZW1lbnQoYXJndW1lbnQpICYmIHRzLmlzSWRlbnRpZmllcihhcmd1bWVudC5leHByZXNzaW9uKSAmJlxuICAgICAgYXJndW1lbnQuZXhwcmVzc2lvbi50ZXh0ID09PSAnYXJndW1lbnRzJztcbn1cblxuLyoqXG4gKiBGaW5kIHRoZSBzdGF0ZW1lbnQgdGhhdCBjb250YWlucyB0aGUgZ2l2ZW4gbm9kZVxuICogQHBhcmFtIG5vZGUgYSBub2RlIHdob3NlIGNvbnRhaW5pbmcgc3RhdGVtZW50IHdlIHdpc2ggdG8gZmluZFxuICovXG5leHBvcnQgZnVuY3Rpb24gZ2V0Q29udGFpbmluZ1N0YXRlbWVudChub2RlOiB0cy5Ob2RlKTogdHMuU3RhdGVtZW50IHtcbiAgd2hpbGUgKG5vZGUucGFyZW50KSB7XG4gICAgaWYgKHRzLmlzQmxvY2sobm9kZS5wYXJlbnQpIHx8IHRzLmlzU291cmNlRmlsZShub2RlLnBhcmVudCkpIHtcbiAgICAgIGJyZWFrO1xuICAgIH1cbiAgICBub2RlID0gbm9kZS5wYXJlbnQ7XG4gIH1cbiAgcmV0dXJuIG5vZGUgYXMgdHMuU3RhdGVtZW50O1xufVxuXG5mdW5jdGlvbiBnZXRSb290RmlsZU9yRmFpbChidW5kbGU6IEJ1bmRsZVByb2dyYW0pOiB0cy5Tb3VyY2VGaWxlIHtcbiAgY29uc3Qgcm9vdEZpbGUgPSBidW5kbGUucHJvZ3JhbS5nZXRTb3VyY2VGaWxlKGJ1bmRsZS5wYXRoKTtcbiAgaWYgKHJvb3RGaWxlID09PSB1bmRlZmluZWQpIHtcbiAgICB0aHJvdyBuZXcgRXJyb3IoYFRoZSBnaXZlbiByb290UGF0aCAke3Jvb3RGaWxlfSBpcyBub3QgYSBmaWxlIG9mIHRoZSBwcm9ncmFtLmApO1xuICB9XG4gIHJldHVybiByb290RmlsZTtcbn1cblxuZnVuY3Rpb24gZ2V0Tm9uUm9vdFBhY2thZ2VGaWxlcyhidW5kbGU6IEJ1bmRsZVByb2dyYW0pOiB0cy5Tb3VyY2VGaWxlW10ge1xuICBjb25zdCByb290RmlsZSA9IGJ1bmRsZS5wcm9ncmFtLmdldFNvdXJjZUZpbGUoYnVuZGxlLnBhdGgpO1xuICByZXR1cm4gYnVuZGxlLnByb2dyYW0uZ2V0U291cmNlRmlsZXMoKS5maWx0ZXIoXG4gICAgICBmID0+IChmICE9PSByb290RmlsZSkgJiYgaXNXaXRoaW5QYWNrYWdlKGJ1bmRsZS5wYWNrYWdlLCBhYnNvbHV0ZUZyb21Tb3VyY2VGaWxlKGYpKSk7XG59XG5cbmZ1bmN0aW9uIGlzVG9wTGV2ZWwobm9kZTogdHMuTm9kZSk6IGJvb2xlYW4ge1xuICB3aGlsZSAobm9kZSA9IG5vZGUucGFyZW50KSB7XG4gICAgaWYgKHRzLmlzQmxvY2sobm9kZSkpIHtcbiAgICAgIHJldHVybiBmYWxzZTtcbiAgICB9XG4gIH1cbiAgcmV0dXJuIHRydWU7XG59XG5cbi8qKlxuICogR2V0IHRoZSBhY3R1YWwgKG91dGVyKSBkZWNsYXJhdGlvbiBvZiBhIGNsYXNzLlxuICpcbiAqIFNvbWV0aW1lcywgdGhlIGltcGxlbWVudGF0aW9uIG9mIGEgY2xhc3MgaXMgYW4gZXhwcmVzc2lvbiB0aGF0IGlzIGhpZGRlbiBpbnNpZGUgYW4gSUlGRSBhbmRcbiAqIHJldHVybmVkIHRvIGJlIGFzc2lnbmVkIHRvIGEgdmFyaWFibGUgb3V0c2lkZSB0aGUgSUlGRSwgd2hpY2ggaXMgd2hhdCB0aGUgcmVzdCBvZiB0aGUgcHJvZ3JhbVxuICogaW50ZXJhY3RzIHdpdGguXG4gKlxuICogR2l2ZW4gdGhlIGlubmVyIGRlY2xhcmF0aW9uLCB3ZSB3YW50IHRvIGdldCB0byB0aGUgZGVjbGFyYXRpb24gb2YgdGhlIG91dGVyIHZhcmlhYmxlIHRoYXRcbiAqIHJlcHJlc2VudHMgdGhlIGNsYXNzLlxuICpcbiAqIEBwYXJhbSBub2RlIGEgbm9kZSB0aGF0IGNvdWxkIGJlIHRoZSBpbm5lciBkZWNsYXJhdGlvbiBpbnNpZGUgYW4gSUlGRS5cbiAqIEByZXR1cm5zIHRoZSBvdXRlciB2YXJpYWJsZSBkZWNsYXJhdGlvbiBvciBgbnVsbGAgaWYgaXQgaXMgbm90IGEgXCJjbGFzc1wiLlxuICovXG5leHBvcnQgZnVuY3Rpb24gZ2V0Q2xhc3NEZWNsYXJhdGlvbkZyb21Jbm5lckRlY2xhcmF0aW9uKG5vZGU6IHRzLk5vZGUpOlxuICAgIENsYXNzRGVjbGFyYXRpb248dHMuVmFyaWFibGVEZWNsYXJhdGlvbj58bnVsbCB7XG4gIGlmICh0cy5pc0Z1bmN0aW9uRGVjbGFyYXRpb24obm9kZSkgfHwgdHMuaXNDbGFzc0RlY2xhcmF0aW9uKG5vZGUpIHx8XG4gICAgICB0cy5pc1ZhcmlhYmxlU3RhdGVtZW50KG5vZGUpKSB7XG4gICAgLy8gSXQgbWlnaHQgYmUgdGhlIGZ1bmN0aW9uIGV4cHJlc3Npb24gaW5zaWRlIHRoZSBJSUZFLiBXZSBuZWVkIHRvIGdvIDUgbGV2ZWxzIHVwLi4uXG5cbiAgICAvLyAtIElJRkUgYm9keS5cbiAgICBsZXQgb3V0ZXJOb2RlID0gbm9kZS5wYXJlbnQ7XG4gICAgaWYgKCFvdXRlck5vZGUgfHwgIXRzLmlzQmxvY2sob3V0ZXJOb2RlKSkgcmV0dXJuIG51bGw7XG5cbiAgICAvLyAtIElJRkUgZnVuY3Rpb24gZXhwcmVzc2lvbi5cbiAgICBvdXRlck5vZGUgPSBvdXRlck5vZGUucGFyZW50O1xuICAgIGlmICghb3V0ZXJOb2RlIHx8ICghdHMuaXNGdW5jdGlvbkV4cHJlc3Npb24ob3V0ZXJOb2RlKSAmJiAhdHMuaXNBcnJvd0Z1bmN0aW9uKG91dGVyTm9kZSkpKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG4gICAgb3V0ZXJOb2RlID0gb3V0ZXJOb2RlLnBhcmVudDtcblxuICAgIC8vIC0gUGFyZW50aGVzaXMgaW5zaWRlIElJRkUuXG4gICAgaWYgKG91dGVyTm9kZSAmJiB0cy5pc1BhcmVudGhlc2l6ZWRFeHByZXNzaW9uKG91dGVyTm9kZSkpIG91dGVyTm9kZSA9IG91dGVyTm9kZS5wYXJlbnQ7XG5cbiAgICAvLyAtIElJRkUgY2FsbCBleHByZXNzaW9uLlxuICAgIGlmICghb3V0ZXJOb2RlIHx8ICF0cy5pc0NhbGxFeHByZXNzaW9uKG91dGVyTm9kZSkpIHJldHVybiBudWxsO1xuICAgIG91dGVyTm9kZSA9IG91dGVyTm9kZS5wYXJlbnQ7XG5cbiAgICAvLyAtIFBhcmVudGhlc2lzIGFyb3VuZCBJSUZFLlxuICAgIGlmIChvdXRlck5vZGUgJiYgdHMuaXNQYXJlbnRoZXNpemVkRXhwcmVzc2lvbihvdXRlck5vZGUpKSBvdXRlck5vZGUgPSBvdXRlck5vZGUucGFyZW50O1xuXG4gICAgLy8gLSBPdXRlciB2YXJpYWJsZSBkZWNsYXJhdGlvbi5cbiAgICBpZiAoIW91dGVyTm9kZSB8fCAhdHMuaXNWYXJpYWJsZURlY2xhcmF0aW9uKG91dGVyTm9kZSkpIHJldHVybiBudWxsO1xuXG4gICAgLy8gRmluYWxseSwgZW5zdXJlIHRoYXQgdGhlIHZhcmlhYmxlIGRlY2xhcmF0aW9uIGhhcyBhIGBuYW1lYCBpZGVudGlmaWVyLlxuICAgIHJldHVybiBoYXNOYW1lSWRlbnRpZmllcihvdXRlck5vZGUpID8gb3V0ZXJOb2RlIDogbnVsbDtcbiAgfVxuXG4gIHJldHVybiBudWxsO1xufVxuIl19