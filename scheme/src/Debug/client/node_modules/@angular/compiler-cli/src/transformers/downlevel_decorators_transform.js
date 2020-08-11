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
        define("@angular/compiler-cli/src/transformers/downlevel_decorators_transform", ["require", "exports", "tslib", "typescript", "@angular/compiler-cli/src/transformers/patch_alias_reference_resolution"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.getDownlevelDecoratorsTransform = void 0;
    var tslib_1 = require("tslib");
    var ts = require("typescript");
    var patch_alias_reference_resolution_1 = require("@angular/compiler-cli/src/transformers/patch_alias_reference_resolution");
    /**
     * Whether a given decorator should be treated as an Angular decorator.
     * Either it's used in @angular/core, or it's imported from there.
     */
    function isAngularDecorator(decorator, isCore) {
        return isCore || (decorator.import !== null && decorator.import.from === '@angular/core');
    }
    /*
     #####################################################################
      Code below has been extracted from the tsickle decorator downlevel transformer
      and a few local modifications have been applied:
    
        1. Tsickle by default processed all decorators that had the `@Annotation` JSDoc.
           We modified the transform to only be concerned with known Angular decorators.
        2. Tsickle by default added `@nocollapse` to all generated `ctorParameters` properties.
           We only do this when `annotateForClosureCompiler` is enabled.
        3. Tsickle does not handle union types for dependency injection. i.e. if a injected type
           is denoted with `@Optional`, the actual type could be set to `T | null`.
           See: https://github.com/angular/angular-cli/commit/826803d0736b807867caff9f8903e508970ad5e4.
        4. Tsickle relied on `emitDecoratorMetadata` to be set to `true`. This is due to a limitation
           in TypeScript transformers that never has been fixed. We were able to work around this
           limitation so that `emitDecoratorMetadata` doesn't need to be specified.
           See: `patchAliasReferenceResolution` for more details.
    
      Here is a link to the tsickle revision on which this transformer is based:
      https://github.com/angular/tsickle/blob/fae06becb1570f491806060d83f29f2d50c43cdd/src/decorator_downlevel_transformer.ts
     #####################################################################
    */
    /**
     * Creates the AST for the decorator field type annotation, which has the form
     *     { type: Function, args?: any[] }[]
     */
    function createDecoratorInvocationType() {
        var typeElements = [];
        typeElements.push(ts.createPropertySignature(undefined, 'type', undefined, ts.createTypeReferenceNode(ts.createIdentifier('Function'), undefined), undefined));
        typeElements.push(ts.createPropertySignature(undefined, 'args', ts.createToken(ts.SyntaxKind.QuestionToken), ts.createArrayTypeNode(ts.createKeywordTypeNode(ts.SyntaxKind.AnyKeyword)), undefined));
        return ts.createArrayTypeNode(ts.createTypeLiteralNode(typeElements));
    }
    /**
     * Extracts the type of the decorator (the function or expression invoked), as well as all the
     * arguments passed to the decorator. Returns an AST with the form:
     *
     *     // For @decorator(arg1, arg2)
     *     { type: decorator, args: [arg1, arg2] }
     */
    function extractMetadataFromSingleDecorator(decorator, diagnostics) {
        var e_1, _a;
        var metadataProperties = [];
        var expr = decorator.expression;
        switch (expr.kind) {
            case ts.SyntaxKind.Identifier:
                // The decorator was a plain @Foo.
                metadataProperties.push(ts.createPropertyAssignment('type', expr));
                break;
            case ts.SyntaxKind.CallExpression:
                // The decorator was a call, like @Foo(bar).
                var call = expr;
                metadataProperties.push(ts.createPropertyAssignment('type', call.expression));
                if (call.arguments.length) {
                    var args = [];
                    try {
                        for (var _b = tslib_1.__values(call.arguments), _c = _b.next(); !_c.done; _c = _b.next()) {
                            var arg = _c.value;
                            args.push(arg);
                        }
                    }
                    catch (e_1_1) { e_1 = { error: e_1_1 }; }
                    finally {
                        try {
                            if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                        }
                        finally { if (e_1) throw e_1.error; }
                    }
                    var argsArrayLiteral = ts.createArrayLiteral(args);
                    argsArrayLiteral.elements.hasTrailingComma = true;
                    metadataProperties.push(ts.createPropertyAssignment('args', argsArrayLiteral));
                }
                break;
            default:
                diagnostics.push({
                    file: decorator.getSourceFile(),
                    start: decorator.getStart(),
                    length: decorator.getEnd() - decorator.getStart(),
                    messageText: ts.SyntaxKind[decorator.kind] + " not implemented in gathering decorator metadata.",
                    category: ts.DiagnosticCategory.Error,
                    code: 0,
                });
                break;
        }
        return ts.createObjectLiteral(metadataProperties);
    }
    /**
     * Takes a list of decorator metadata object ASTs and produces an AST for a
     * static class property of an array of those metadata objects.
     */
    function createDecoratorClassProperty(decoratorList) {
        var modifier = ts.createToken(ts.SyntaxKind.StaticKeyword);
        var type = createDecoratorInvocationType();
        var initializer = ts.createArrayLiteral(decoratorList, true);
        // NB: the .decorators property does not get a @nocollapse property. There is
        // no good reason why - it means .decorators is not runtime accessible if you
        // compile with collapse properties, whereas propDecorators is, which doesn't
        // follow any stringent logic. However this has been the case previously, and
        // adding it back in leads to substantial code size increases as Closure fails
        // to tree shake these props without @nocollapse.
        return ts.createProperty(undefined, [modifier], 'decorators', undefined, type, initializer);
    }
    /**
     * Creates the AST for the 'ctorParameters' field type annotation:
     *   () => ({ type: any, decorators?: {type: Function, args?: any[]}[] }|null)[]
     */
    function createCtorParametersClassPropertyType() {
        // Sorry about this. Try reading just the string literals below.
        var typeElements = [];
        typeElements.push(ts.createPropertySignature(undefined, 'type', undefined, ts.createTypeReferenceNode(ts.createIdentifier('any'), undefined), undefined));
        typeElements.push(ts.createPropertySignature(undefined, 'decorators', ts.createToken(ts.SyntaxKind.QuestionToken), ts.createArrayTypeNode(ts.createTypeLiteralNode([
            ts.createPropertySignature(undefined, 'type', undefined, ts.createTypeReferenceNode(ts.createIdentifier('Function'), undefined), undefined),
            ts.createPropertySignature(undefined, 'args', ts.createToken(ts.SyntaxKind.QuestionToken), ts.createArrayTypeNode(ts.createTypeReferenceNode(ts.createIdentifier('any'), undefined)), undefined),
        ])), undefined));
        return ts.createFunctionTypeNode(undefined, [], ts.createArrayTypeNode(ts.createUnionTypeNode([ts.createTypeLiteralNode(typeElements), ts.createNull()])));
    }
    /**
     * Sets a Closure \@nocollapse synthetic comment on the given node. This prevents Closure Compiler
     * from collapsing the apparently static property, which would make it impossible to find for code
     * trying to detect it at runtime.
     */
    function addNoCollapseComment(n) {
        ts.setSyntheticLeadingComments(n, [{
                kind: ts.SyntaxKind.MultiLineCommentTrivia,
                text: '* @nocollapse ',
                pos: -1,
                end: -1,
                hasTrailingNewLine: true
            }]);
    }
    /**
     * createCtorParametersClassProperty creates a static 'ctorParameters' property containing
     * downleveled decorator information.
     *
     * The property contains an arrow function that returns an array of object literals of the shape:
     *     static ctorParameters = () => [{
     *       type: SomeClass|undefined,  // the type of the param that's decorated, if it's a value.
     *       decorators: [{
     *         type: DecoratorFn,  // the type of the decorator that's invoked.
     *         args: [ARGS],       // the arguments passed to the decorator.
     *       }]
     *     }];
     */
    function createCtorParametersClassProperty(diagnostics, entityNameToExpression, ctorParameters, isClosureCompilerEnabled) {
        var e_2, _a, e_3, _b;
        var params = [];
        try {
            for (var ctorParameters_1 = tslib_1.__values(ctorParameters), ctorParameters_1_1 = ctorParameters_1.next(); !ctorParameters_1_1.done; ctorParameters_1_1 = ctorParameters_1.next()) {
                var ctorParam = ctorParameters_1_1.value;
                if (!ctorParam.type && ctorParam.decorators.length === 0) {
                    params.push(ts.createNull());
                    continue;
                }
                var paramType = ctorParam.type ?
                    typeReferenceToExpression(entityNameToExpression, ctorParam.type) :
                    undefined;
                var members = [ts.createPropertyAssignment('type', paramType || ts.createIdentifier('undefined'))];
                var decorators = [];
                try {
                    for (var _c = (e_3 = void 0, tslib_1.__values(ctorParam.decorators)), _d = _c.next(); !_d.done; _d = _c.next()) {
                        var deco = _d.value;
                        decorators.push(extractMetadataFromSingleDecorator(deco, diagnostics));
                    }
                }
                catch (e_3_1) { e_3 = { error: e_3_1 }; }
                finally {
                    try {
                        if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
                    }
                    finally { if (e_3) throw e_3.error; }
                }
                if (decorators.length) {
                    members.push(ts.createPropertyAssignment('decorators', ts.createArrayLiteral(decorators)));
                }
                params.push(ts.createObjectLiteral(members));
            }
        }
        catch (e_2_1) { e_2 = { error: e_2_1 }; }
        finally {
            try {
                if (ctorParameters_1_1 && !ctorParameters_1_1.done && (_a = ctorParameters_1.return)) _a.call(ctorParameters_1);
            }
            finally { if (e_2) throw e_2.error; }
        }
        var initializer = ts.createArrowFunction(undefined, undefined, [], undefined, ts.createToken(ts.SyntaxKind.EqualsGreaterThanToken), ts.createArrayLiteral(params, true));
        var type = createCtorParametersClassPropertyType();
        var ctorProp = ts.createProperty(undefined, [ts.createToken(ts.SyntaxKind.StaticKeyword)], 'ctorParameters', undefined, type, initializer);
        if (isClosureCompilerEnabled) {
            addNoCollapseComment(ctorProp);
        }
        return ctorProp;
    }
    /**
     * createPropDecoratorsClassProperty creates a static 'propDecorators' property containing type
     * information for every property that has a decorator applied.
     *
     *     static propDecorators: {[key: string]: {type: Function, args?: any[]}[]} = {
     *       propA: [{type: MyDecorator, args: [1, 2]}, ...],
     *       ...
     *     };
     */
    function createPropDecoratorsClassProperty(diagnostics, properties) {
        var e_4, _a;
        //  `static propDecorators: {[key: string]: ` + {type: Function, args?: any[]}[] + `} = {\n`);
        var entries = [];
        try {
            for (var _b = tslib_1.__values(properties.entries()), _c = _b.next(); !_c.done; _c = _b.next()) {
                var _d = tslib_1.__read(_c.value, 2), name = _d[0], decorators = _d[1];
                entries.push(ts.createPropertyAssignment(name, ts.createArrayLiteral(decorators.map(function (deco) { return extractMetadataFromSingleDecorator(deco, diagnostics); }))));
            }
        }
        catch (e_4_1) { e_4 = { error: e_4_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_4) throw e_4.error; }
        }
        var initializer = ts.createObjectLiteral(entries, true);
        var type = ts.createTypeLiteralNode([ts.createIndexSignature(undefined, undefined, [ts.createParameter(undefined, undefined, undefined, 'key', undefined, ts.createTypeReferenceNode('string', undefined), undefined)], createDecoratorInvocationType())]);
        return ts.createProperty(undefined, [ts.createToken(ts.SyntaxKind.StaticKeyword)], 'propDecorators', undefined, type, initializer);
    }
    /**
     * Returns an expression representing the (potentially) value part for the given node.
     *
     * This is a partial re-implementation of TypeScript's serializeTypeReferenceNode. This is a
     * workaround for https://github.com/Microsoft/TypeScript/issues/17516 (serializeTypeReferenceNode
     * not being exposed). In practice this implementation is sufficient for Angular's use of type
     * metadata.
     */
    function typeReferenceToExpression(entityNameToExpression, node) {
        var kind = node.kind;
        if (ts.isLiteralTypeNode(node)) {
            // Treat literal types like their base type (boolean, string, number).
            kind = node.literal.kind;
        }
        switch (kind) {
            case ts.SyntaxKind.FunctionType:
            case ts.SyntaxKind.ConstructorType:
                return ts.createIdentifier('Function');
            case ts.SyntaxKind.ArrayType:
            case ts.SyntaxKind.TupleType:
                return ts.createIdentifier('Array');
            case ts.SyntaxKind.TypePredicate:
            case ts.SyntaxKind.TrueKeyword:
            case ts.SyntaxKind.FalseKeyword:
            case ts.SyntaxKind.BooleanKeyword:
                return ts.createIdentifier('Boolean');
            case ts.SyntaxKind.StringLiteral:
            case ts.SyntaxKind.StringKeyword:
                return ts.createIdentifier('String');
            case ts.SyntaxKind.ObjectKeyword:
                return ts.createIdentifier('Object');
            case ts.SyntaxKind.NumberKeyword:
            case ts.SyntaxKind.NumericLiteral:
                return ts.createIdentifier('Number');
            case ts.SyntaxKind.TypeReference:
                var typeRef = node;
                // Ignore any generic types, just return the base type.
                return entityNameToExpression(typeRef.typeName);
            case ts.SyntaxKind.UnionType:
                var childTypeNodes = node.types.filter(function (t) { return t.kind !== ts.SyntaxKind.NullKeyword; });
                return childTypeNodes.length === 1 ?
                    typeReferenceToExpression(entityNameToExpression, childTypeNodes[0]) :
                    undefined;
            default:
                return undefined;
        }
    }
    /**
     * Returns true if the given symbol refers to a value (as distinct from a type).
     *
     * Expands aliases, which is important for the case where
     *   import * as x from 'some-module';
     * and x is now a value (the module object).
     */
    function symbolIsValue(tc, sym) {
        if (sym.flags & ts.SymbolFlags.Alias)
            sym = tc.getAliasedSymbol(sym);
        return (sym.flags & ts.SymbolFlags.Value) !== 0;
    }
    /**
     * Gets a transformer for downleveling Angular decorators.
     * @param typeChecker Reference to the program's type checker.
     * @param host Reflection host that is used for determining decorators.
     * @param diagnostics List which will be populated with diagnostics if any.
     * @param isCore Whether the current TypeScript program is for the `@angular/core` package.
     * @param isClosureCompilerEnabled Whether closure annotations need to be added where needed.
     * @param skipClassDecorators Whether class decorators should be skipped from downleveling.
     *   This is useful for JIT mode where class decorators should be preserved as they could rely
     *   on immediate execution. e.g. downleveling `@Injectable` means that the injectable factory
     *   is not created, and injecting the token will not work. If this decorator would not be
     *   downleveled, the `Injectable` decorator will execute immediately on file load, and
     *   Angular will generate the corresponding injectable factory.
     */
    function getDownlevelDecoratorsTransform(typeChecker, host, diagnostics, isCore, isClosureCompilerEnabled, skipClassDecorators) {
        return function (context) {
            var referencedParameterTypes = new Set();
            /**
             * Converts an EntityName (from a type annotation) to an expression (accessing a value).
             *
             * For a given qualified name, this walks depth first to find the leftmost identifier,
             * and then converts the path into a property access that can be used as expression.
             */
            function entityNameToExpression(name) {
                var symbol = typeChecker.getSymbolAtLocation(name);
                // Check if the entity name references a symbol that is an actual value. If it is not, it
                // cannot be referenced by an expression, so return undefined.
                if (!symbol || !symbolIsValue(typeChecker, symbol) || !symbol.declarations ||
                    symbol.declarations.length === 0) {
                    return undefined;
                }
                // If we deal with a qualified name, build up a property access expression
                // that could be used in the JavaScript output.
                if (ts.isQualifiedName(name)) {
                    var containerExpr = entityNameToExpression(name.left);
                    if (containerExpr === undefined) {
                        return undefined;
                    }
                    return ts.createPropertyAccess(containerExpr, name.right);
                }
                var decl = symbol.declarations[0];
                // If the given entity name has been resolved to an alias import declaration,
                // ensure that the alias declaration is not elided by TypeScript, and use its
                // name identifier to reference it at runtime.
                if (patch_alias_reference_resolution_1.isAliasImportDeclaration(decl)) {
                    referencedParameterTypes.add(decl);
                    // If the entity name resolves to an alias import declaration, we reference the
                    // entity based on the alias import name. This ensures that TypeScript properly
                    // resolves the link to the import. Cloning the original entity name identifier
                    // could lead to an incorrect resolution at local scope. e.g. Consider the following
                    // snippet: `constructor(Dep: Dep) {}`. In such a case, the local `Dep` identifier
                    // would resolve to the actual parameter name, and not to the desired import.
                    // This happens because the entity name identifier symbol is internally considered
                    // as type-only and therefore TypeScript tries to resolve it as value manually.
                    // We can help TypeScript and avoid this non-reliable resolution by using an identifier
                    // that is not type-only and is directly linked to the import alias declaration.
                    if (decl.name !== undefined) {
                        return ts.getMutableClone(decl.name);
                    }
                }
                // Clone the original entity name identifier so that it can be used to reference
                // its value at runtime. This is used when the identifier is resolving to a file
                // local declaration (otherwise it would resolve to an alias import declaration).
                return ts.getMutableClone(name);
            }
            /**
             * Transforms a class element. Returns a three tuple of name, transformed element, and
             * decorators found. Returns an undefined name if there are no decorators to lower on the
             * element, or the element has an exotic name.
             */
            function transformClassElement(element) {
                var e_5, _a;
                element = ts.visitEachChild(element, decoratorDownlevelVisitor, context);
                var decoratorsToKeep = [];
                var toLower = [];
                var decorators = host.getDecoratorsOfDeclaration(element) || [];
                try {
                    for (var decorators_1 = tslib_1.__values(decorators), decorators_1_1 = decorators_1.next(); !decorators_1_1.done; decorators_1_1 = decorators_1.next()) {
                        var decorator = decorators_1_1.value;
                        // We only deal with concrete nodes in TypeScript sources, so we don't
                        // need to handle synthetically created decorators.
                        var decoratorNode = decorator.node;
                        if (!isAngularDecorator(decorator, isCore)) {
                            decoratorsToKeep.push(decoratorNode);
                            continue;
                        }
                        toLower.push(decoratorNode);
                    }
                }
                catch (e_5_1) { e_5 = { error: e_5_1 }; }
                finally {
                    try {
                        if (decorators_1_1 && !decorators_1_1.done && (_a = decorators_1.return)) _a.call(decorators_1);
                    }
                    finally { if (e_5) throw e_5.error; }
                }
                if (!toLower.length)
                    return [undefined, element, []];
                if (!element.name || !ts.isIdentifier(element.name)) {
                    // Method has a weird name, e.g.
                    //   [Symbol.foo]() {...}
                    diagnostics.push({
                        file: element.getSourceFile(),
                        start: element.getStart(),
                        length: element.getEnd() - element.getStart(),
                        messageText: "Cannot process decorators for class element with non-analyzable name.",
                        category: ts.DiagnosticCategory.Error,
                        code: 0,
                    });
                    return [undefined, element, []];
                }
                var name = element.name.text;
                var mutable = ts.getMutableClone(element);
                mutable.decorators = decoratorsToKeep.length ?
                    ts.setTextRange(ts.createNodeArray(decoratorsToKeep), mutable.decorators) :
                    undefined;
                return [name, mutable, toLower];
            }
            /**
             * Transforms a constructor. Returns the transformed constructor and the list of parameter
             * information collected, consisting of decorators and optional type.
             */
            function transformConstructor(ctor) {
                var e_6, _a, e_7, _b;
                ctor = ts.visitEachChild(ctor, decoratorDownlevelVisitor, context);
                var newParameters = [];
                var oldParameters = ts.visitParameterList(ctor.parameters, decoratorDownlevelVisitor, context);
                var parametersInfo = [];
                try {
                    for (var oldParameters_1 = tslib_1.__values(oldParameters), oldParameters_1_1 = oldParameters_1.next(); !oldParameters_1_1.done; oldParameters_1_1 = oldParameters_1.next()) {
                        var param = oldParameters_1_1.value;
                        var decoratorsToKeep = [];
                        var paramInfo = { decorators: [], type: null };
                        var decorators = host.getDecoratorsOfDeclaration(param) || [];
                        try {
                            for (var decorators_2 = (e_7 = void 0, tslib_1.__values(decorators)), decorators_2_1 = decorators_2.next(); !decorators_2_1.done; decorators_2_1 = decorators_2.next()) {
                                var decorator = decorators_2_1.value;
                                // We only deal with concrete nodes in TypeScript sources, so we don't
                                // need to handle synthetically created decorators.
                                var decoratorNode = decorator.node;
                                if (!isAngularDecorator(decorator, isCore)) {
                                    decoratorsToKeep.push(decoratorNode);
                                    continue;
                                }
                                paramInfo.decorators.push(decoratorNode);
                            }
                        }
                        catch (e_7_1) { e_7 = { error: e_7_1 }; }
                        finally {
                            try {
                                if (decorators_2_1 && !decorators_2_1.done && (_b = decorators_2.return)) _b.call(decorators_2);
                            }
                            finally { if (e_7) throw e_7.error; }
                        }
                        if (param.type) {
                            // param has a type provided, e.g. "foo: Bar".
                            // The type will be emitted as a value expression in entityNameToExpression, which takes
                            // care not to emit anything for types that cannot be expressed as a value (e.g.
                            // interfaces).
                            paramInfo.type = param.type;
                        }
                        parametersInfo.push(paramInfo);
                        var newParam = ts.updateParameter(param, 
                        // Must pass 'undefined' to avoid emitting decorator metadata.
                        decoratorsToKeep.length ? decoratorsToKeep : undefined, param.modifiers, param.dotDotDotToken, param.name, param.questionToken, param.type, param.initializer);
                        newParameters.push(newParam);
                    }
                }
                catch (e_6_1) { e_6 = { error: e_6_1 }; }
                finally {
                    try {
                        if (oldParameters_1_1 && !oldParameters_1_1.done && (_a = oldParameters_1.return)) _a.call(oldParameters_1);
                    }
                    finally { if (e_6) throw e_6.error; }
                }
                var updated = ts.updateConstructor(ctor, ctor.decorators, ctor.modifiers, newParameters, ts.visitFunctionBody(ctor.body, decoratorDownlevelVisitor, context));
                return [updated, parametersInfo];
            }
            /**
             * Transforms a single class declaration:
             * - dispatches to strip decorators on members
             * - converts decorators on the class to annotations
             * - creates a ctorParameters property
             * - creates a propDecorators property
             */
            function transformClassDeclaration(classDecl) {
                var e_8, _a, e_9, _b;
                classDecl = ts.getMutableClone(classDecl);
                var newMembers = [];
                var decoratedProperties = new Map();
                var classParameters = null;
                try {
                    for (var _c = tslib_1.__values(classDecl.members), _d = _c.next(); !_d.done; _d = _c.next()) {
                        var member = _d.value;
                        switch (member.kind) {
                            case ts.SyntaxKind.PropertyDeclaration:
                            case ts.SyntaxKind.GetAccessor:
                            case ts.SyntaxKind.SetAccessor:
                            case ts.SyntaxKind.MethodDeclaration: {
                                var _e = tslib_1.__read(transformClassElement(member), 3), name = _e[0], newMember = _e[1], decorators_4 = _e[2];
                                newMembers.push(newMember);
                                if (name)
                                    decoratedProperties.set(name, decorators_4);
                                continue;
                            }
                            case ts.SyntaxKind.Constructor: {
                                var ctor = member;
                                if (!ctor.body)
                                    break;
                                var _f = tslib_1.__read(transformConstructor(member), 2), newMember = _f[0], parametersInfo = _f[1];
                                classParameters = parametersInfo;
                                newMembers.push(newMember);
                                continue;
                            }
                            default:
                                break;
                        }
                        newMembers.push(ts.visitEachChild(member, decoratorDownlevelVisitor, context));
                    }
                }
                catch (e_8_1) { e_8 = { error: e_8_1 }; }
                finally {
                    try {
                        if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                    }
                    finally { if (e_8) throw e_8.error; }
                }
                var decorators = host.getDecoratorsOfDeclaration(classDecl) || [];
                var hasAngularDecorator = false;
                var decoratorsToLower = [];
                var decoratorsToKeep = [];
                try {
                    for (var decorators_3 = tslib_1.__values(decorators), decorators_3_1 = decorators_3.next(); !decorators_3_1.done; decorators_3_1 = decorators_3.next()) {
                        var decorator = decorators_3_1.value;
                        // We only deal with concrete nodes in TypeScript sources, so we don't
                        // need to handle synthetically created decorators.
                        var decoratorNode = decorator.node;
                        var isNgDecorator = isAngularDecorator(decorator, isCore);
                        // Keep track if we come across an Angular class decorator. This is used
                        // for to determine whether constructor parameters should be captured or not.
                        if (isNgDecorator) {
                            hasAngularDecorator = true;
                        }
                        if (isNgDecorator && !skipClassDecorators) {
                            decoratorsToLower.push(extractMetadataFromSingleDecorator(decoratorNode, diagnostics));
                        }
                        else {
                            decoratorsToKeep.push(decoratorNode);
                        }
                    }
                }
                catch (e_9_1) { e_9 = { error: e_9_1 }; }
                finally {
                    try {
                        if (decorators_3_1 && !decorators_3_1.done && (_b = decorators_3.return)) _b.call(decorators_3);
                    }
                    finally { if (e_9) throw e_9.error; }
                }
                var newClassDeclaration = ts.getMutableClone(classDecl);
                if (decoratorsToLower.length) {
                    newMembers.push(createDecoratorClassProperty(decoratorsToLower));
                }
                if (classParameters) {
                    if (hasAngularDecorator || classParameters.some(function (p) { return !!p.decorators.length; })) {
                        // Capture constructor parameters if the class has Angular decorator applied,
                        // or if any of the parameters has decorators applied directly.
                        newMembers.push(createCtorParametersClassProperty(diagnostics, entityNameToExpression, classParameters, isClosureCompilerEnabled));
                    }
                }
                if (decoratedProperties.size) {
                    newMembers.push(createPropDecoratorsClassProperty(diagnostics, decoratedProperties));
                }
                newClassDeclaration.members = ts.setTextRange(ts.createNodeArray(newMembers, newClassDeclaration.members.hasTrailingComma), classDecl.members);
                newClassDeclaration.decorators =
                    decoratorsToKeep.length ? ts.createNodeArray(decoratorsToKeep) : undefined;
                return newClassDeclaration;
            }
            /**
             * Transformer visitor that looks for Angular decorators and replaces them with
             * downleveled static properties. Also collects constructor type metadata for
             * class declaration that are decorated with an Angular decorator.
             */
            function decoratorDownlevelVisitor(node) {
                if (ts.isClassDeclaration(node)) {
                    return transformClassDeclaration(node);
                }
                return ts.visitEachChild(node, decoratorDownlevelVisitor, context);
            }
            return function (sf) {
                // Ensure that referenced type symbols are not elided by TypeScript. Imports for
                // such parameter type symbols previously could be type-only, but now might be also
                // used in the `ctorParameters` static property as a value. We want to make sure
                // that TypeScript does not elide imports for such type references. Read more
                // about this in the description for `patchAliasReferenceResolution`.
                patch_alias_reference_resolution_1.patchAliasReferenceResolutionOrDie(context, referencedParameterTypes);
                // Downlevel decorators and constructor parameter types. We will keep track of all
                // referenced constructor parameter types so that we can instruct TypeScript to
                // not elide their imports if they previously were only type-only.
                return ts.visitEachChild(sf, decoratorDownlevelVisitor, context);
            };
        };
    }
    exports.getDownlevelDecoratorsTransform = getDownlevelDecoratorsTransform;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZG93bmxldmVsX2RlY29yYXRvcnNfdHJhbnNmb3JtLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vcGFja2FnZXMvY29tcGlsZXItY2xpL3NyYy90cmFuc2Zvcm1lcnMvZG93bmxldmVsX2RlY29yYXRvcnNfdHJhbnNmb3JtLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7Ozs7Ozs7Ozs7Ozs7SUFFSCwrQkFBaUM7SUFFakMsNEhBQWdIO0lBRWhIOzs7T0FHRztJQUNILFNBQVMsa0JBQWtCLENBQUMsU0FBb0IsRUFBRSxNQUFlO1FBQy9ELE9BQU8sTUFBTSxJQUFJLENBQUMsU0FBUyxDQUFDLE1BQU0sS0FBSyxJQUFJLElBQUksU0FBUyxDQUFDLE1BQU0sQ0FBQyxJQUFJLEtBQUssZUFBZSxDQUFDLENBQUM7SUFDNUYsQ0FBQztJQUVEOzs7Ozs7Ozs7Ozs7Ozs7Ozs7OztNQW9CRTtJQUVGOzs7T0FHRztJQUNILFNBQVMsNkJBQTZCO1FBQ3BDLElBQU0sWUFBWSxHQUFxQixFQUFFLENBQUM7UUFDMUMsWUFBWSxDQUFDLElBQUksQ0FBQyxFQUFFLENBQUMsdUJBQXVCLENBQ3hDLFNBQVMsRUFBRSxNQUFNLEVBQUUsU0FBUyxFQUM1QixFQUFFLENBQUMsdUJBQXVCLENBQUMsRUFBRSxDQUFDLGdCQUFnQixDQUFDLFVBQVUsQ0FBQyxFQUFFLFNBQVMsQ0FBQyxFQUFFLFNBQVMsQ0FBQyxDQUFDLENBQUM7UUFDeEYsWUFBWSxDQUFDLElBQUksQ0FBQyxFQUFFLENBQUMsdUJBQXVCLENBQ3hDLFNBQVMsRUFBRSxNQUFNLEVBQUUsRUFBRSxDQUFDLFdBQVcsQ0FBQyxFQUFFLENBQUMsVUFBVSxDQUFDLGFBQWEsQ0FBQyxFQUM5RCxFQUFFLENBQUMsbUJBQW1CLENBQUMsRUFBRSxDQUFDLHFCQUFxQixDQUFDLEVBQUUsQ0FBQyxVQUFVLENBQUMsVUFBVSxDQUFDLENBQUMsRUFBRSxTQUFTLENB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T0FBTyxDQUFDLENBQUM7WUFDckUsQ0FBQztZQUVELE9BQU8sVUFBQyxFQUFpQjtnQkFDdkIsZ0ZBQWdGO2dCQUNoRixtRkFBbUY7Z0JBQ25GLGdGQUFnRjtnQkFDaEYsNkVBQTZFO2dCQUM3RSxxRUFBcUU7Z0JBQ3JFLHFFQUFrQyxDQUFDLE9BQU8sRUFBRSx3QkFBd0IsQ0FBQyxDQUFDO2dCQUN0RSxrRkFBa0Y7Z0JBQ2xGLCtFQUErRTtnQkFDL0Usa0VBQWtFO2dCQUNsRSxPQUFPLEVBQUUsQ0FBQyxjQUFjLENBQUMsRUFBRSxFQUFFLHlCQUF5QixFQUFFLE9BQU8sQ0FBQyxDQUFDO1lBQ25FLENBQUMsQ0FBQztRQUNKLENBQUMsQ0FBQztJQUNKLENBQUM7SUFyUUQsMEVBcVFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCAqIGFzIHRzIGZyb20gJ3R5cGVzY3JpcHQnO1xuaW1wb3J0IHtEZWNvcmF0b3IsIFJlZmxlY3Rpb25Ib3N0fSBmcm9tICcuLi9uZ3RzYy9yZWZsZWN0aW9uJztcbmltcG9ydCB7aXNBbGlhc0ltcG9ydERlY2xhcmF0aW9uLCBwYXRjaEFsaWFzUmVmZXJlbmNlUmVzb2x1dGlvbk9yRGllfSBmcm9tICcuL3BhdGNoX2FsaWFzX3JlZmVyZW5jZV9yZXNvbHV0aW9uJztcblxuLyoqXG4gKiBXaGV0aGVyIGEgZ2l2ZW4gZGVjb3JhdG9yIHNob3VsZCBiZSB0cmVhdGVkIGFzIGFuIEFuZ3VsYXIgZGVjb3JhdG9yLlxuICogRWl0aGVyIGl0J3MgdXNlZCBpbiBAYW5ndWxhci9jb3JlLCBvciBpdCdzIGltcG9ydGVkIGZyb20gdGhlcmUuXG4gKi9cbmZ1bmN0aW9uIGlzQW5ndWxhckRlY29yYXRvcihkZWNvcmF0b3I6IERlY29yYXRvciwgaXNDb3JlOiBib29sZWFuKTogYm9vbGVhbiB7XG4gIHJldHVybiBpc0NvcmUgfHwgKGRlY29yYXRvci5pbXBvcnQgIT09IG51bGwgJiYgZGVjb3JhdG9yLmltcG9ydC5mcm9tID09PSAnQGFuZ3VsYXIvY29yZScpO1xufVxuXG4vKlxuICMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjI1xuICBDb2RlIGJlbG93IGhhcyBiZWVuIGV4dHJhY3RlZCBmcm9tIHRoZSB0c2lja2xlIGRlY29yYXRvciBkb3dubGV2ZWwgdHJhbnNmb3JtZXJcbiAgYW5kIGEgZmV3IGxvY2FsIG1vZGlmaWNhdGlvbnMgaGF2ZSBiZWVuIGFwcGxpZWQ6XG5cbiAgICAxLiBUc2lja2xlIGJ5IGRlZmF1bHQgcHJvY2Vzc2VkIGFsbCBkZWNvcmF0b3JzIHRoYXQgaGFkIHRoZSBgQEFubm90YXRpb25gIEpTRG9jLlxuICAgICAgIFdlIG1vZGlmaWVkIHRoZSB0cmFuc2Zvcm0gdG8gb25seSBiZSBjb25jZXJuZWQgd2l0aCBrbm93biBBbmd1bGFyIGRlY29yYXRvcnMuXG4gICAgMi4gVHNpY2tsZSBieSBkZWZhdWx0IGFkZGVkIGBAbm9jb2xsYXBzZWAgdG8gYWxsIGdlbmVyYXRlZCBgY3RvclBhcmFtZXRlcnNgIHByb3BlcnRpZXMuXG4gICAgICAgV2Ugb25seSBkbyB0aGlzIHdoZW4gYGFubm90YXRlRm9yQ2xvc3VyZUNvbXBpbGVyYCBpcyBlbmFibGVkLlxuICAgIDMuIFRzaWNrbGUgZG9lcyBub3QgaGFuZGxlIHVuaW9uIHR5cGVzIGZvciBkZXBlbmRlbmN5IGluamVjdGlvbi4gaS5lLiBpZiBhIGluamVjdGVkIHR5cGVcbiAgICAgICBpcyBkZW5vdGVkIHdpdGggYEBPcHRpb25hbGAsIHRoZSBhY3R1YWwgdHlwZSBjb3VsZCBiZSBzZXQgdG8gYFQgfCBudWxsYC5cbiAgICAgICBTZWU6IGh0dHBzOi8vZ2l0aHViLmNvbS9hbmd1bGFyL2FuZ3VsYXItY2xpL2NvbW1pdC84MjY4MDNkMDczNmI4MDc4NjdjYWZmOWY4OTAzZTUwODk3MGFkNWU0LlxuICAgIDQuIFRzaWNrbGUgcmVsaWVkIG9uIGBlbWl0RGVjb3JhdG9yTWV0YWRhdGFgIHRvIGJlIHNldCB0byBgdHJ1ZWAuIFRoaXMgaXMgZHVlIHRvIGEgbGltaXRhdGlvblxuICAgICAgIGluIFR5cGVTY3JpcHQgdHJhbnNmb3JtZXJzIHRoYXQgbmV2ZXIgaGFzIGJlZW4gZml4ZWQuIFdlIHdlcmUgYWJsZSB0byB3b3JrIGFyb3VuZCB0aGlzXG4gICAgICAgbGltaXRhdGlvbiBzbyB0aGF0IGBlbWl0RGVjb3JhdG9yTWV0YWRhdGFgIGRvZXNuJ3QgbmVlZCB0byBiZSBzcGVjaWZpZWQuXG4gICAgICAgU2VlOiBgcGF0Y2hBbGlhc1JlZmVyZW5jZVJlc29sdXRpb25gIGZvciBtb3JlIGRldGFpbHMuXG5cbiAgSGVyZSBpcyBhIGxpbmsgdG8gdGhlIHRzaWNrbGUgcmV2aXNpb24gb24gd2hpY2ggdGhpcyB0cmFuc2Zvcm1lciBpcyBiYXNlZDpcbiAgaHR0cHM6Ly9naXRodWIuY29tL2FuZ3VsYXIvdHNpY2tsZS9ibG9iL2ZhZTA2YmVjYjE1NzBmNDkxODA2MDYwZDgzZjI5ZjJkNTBjNDNjZGQvc3JjL2RlY29yYXRvcl9kb3dubGV2ZWxfdHJhbnNmb3JtZXIudHNcbiAjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyNcbiovXG5cbi8qKlxuICogQ3JlYXRlcyB0aGUgQVNUIGZvciB0aGUgZGVjb3JhdG9yIGZpZWxkIHR5cGUgYW5ub3RhdGlvbiwgd2hpY2ggaGFzIHRoZSBmb3JtXG4gKiAgICAgeyB0eXBlOiBGdW5jdGlvbiwgYXJncz86IGFueVtdIH1bXVxuICovXG5mdW5jdGlvbiBjcmVhdGVEZWNvcmF0b3JJbnZvY2F0aW9uVHlwZSgpOiB0cy5UeXBlTm9kZSB7XG4gIGNvbnN0IHR5cGVFbGVtZW50czogdHMuVHlwZUVsZW1lbnRbXSA9IFtdO1xuICB0eXBlRWxlbWVudHMucHVzaCh0cy5jcmVhdGVQcm9wZXJ0eVNpZ25hdHVyZShcbiAgICAgIHVuZGVmaW5lZCwgJ3R5cGUnLCB1bmRlZmluZWQsXG4gICAgICB0cy5jcmVhdGVUeXBlUmVmZXJlbmNlTm9kZSh0cy5jcmVhdGVJZGVudGlmaWVyKCdGdW5jdGlvbicpLCB1bmRlZmluZWQpLCB1bmRlZmluZWQpKTtcbiAgdHlwZUVsZW1lbnRzLnB1c2godHMuY3JlYXRlUHJvcGVydHlTaWduYXR1cmUoXG4gICAgICB1bmRlZmluZWQsICdhcmdzJywgdHMuY3JlYXRlVG9rZW4odHMuU3ludGF4S2luZC5RdWVzdGlvblRva2VuKSxcbiAgICAgIHRzLmNyZWF0ZUFycmF5VHlwZU5vZGUodHMuY3JlYXRlS2V5d29yZFR5cGVOb2RlKHRzLlN5bnRheEtpbmQuQW55S2V5d29yZCkpLCB1bmRlZmluZWQpKTtcbiAgcmV0dXJuIHRzLmNyZWF0ZUFycmF5VHlwZU5vZGUodHMuY3JlYXRlVHlwZUxpdGVyYWxOb2RlKHR5cGVFbGVtZW50cykpO1xufVxuXG4vKipcbiAqIEV4dHJhY3RzIHRoZSB0eXBlIG9mIHRoZSBkZWNvcmF0b3IgKHRoZSBmdW5jdGlvbiBvciBleHByZXNzaW9uIGludm9rZWQpLCBhcyB3ZWxsIGFzIGFsbCB0aGVcbiAqIGFyZ3VtZW50cyBwYXNzZWQgdG8gdGhlIGRlY29yYXRvci4gUmV0dXJucyBhbiBBU1Qgd2l0aCB0aGUgZm9ybTpcbiAqXG4gKiAgICAgLy8gRm9yIEBkZWNvcmF0b3IoYXJnMSwgYXJnMilcbiAqICAgICB7IHR5cGU6IGRlY29yYXRvciwgYXJnczogW2FyZzEsIGFyZzJdIH1cbiAqL1xuZnVuY3Rpb24gZXh0cmFjdE1ldGFkYXRhRnJvbVNpbmdsZURlY29yYXRvcihcbiAgICBkZWNvcmF0b3I6IHRzLkRlY29yYXRvciwgZGlhZ25vc3RpY3M6IHRzLkRpYWdub3N0aWNbXSk6IHRzLk9iamVjdExpdGVyYWxFeHByZXNzaW9uIHtcbiAgY29uc3QgbWV0YWRhdGFQcm9wZXJ0aWVzOiB0cy5PYmplY3RMaXRlcmFsRWxlbWVudExpa2VbXSA9IFtdO1xuICBjb25zdCBleHByID0gZGVjb3JhdG9yLmV4cHJlc3Npb247XG4gIHN3aXRjaCAoZXhwci5raW5kKSB7XG4gICAgY2FzZSB0cy5TeW50YXhLaW5kLklkZW50aWZpZXI6XG4gICAgICAvLyBUaGUgZGVjb3JhdG9yIHdhcyBhIHBsYWluIEBGb28uXG4gICAgICBtZXRhZGF0YVByb3BlcnRpZXMucHVzaCh0cy5jcmVhdGVQcm9wZXJ0eUFzc2lnbm1lbnQoJ3R5cGUnLCBleHByKSk7XG4gICAgICBicmVhaztcbiAgICBjYXNlIHRzLlN5bnRheEtpbmQuQ2FsbEV4cHJlc3Npb246XG4gICAgICAvLyBUaGUgZGVjb3JhdG9yIHdhcyBhIGNhbGwsIGxpa2UgQEZvbyhiYXIpLlxuICAgICAgY29uc3QgY2FsbCA9IGV4cHIgYXMgdHMuQ2FsbEV4cHJlc3Npb247XG4gICAgICBtZXRhZGF0YVByb3BlcnRpZXMucHVzaCh0cy5jcmVhdGVQcm9wZXJ0eUFzc2lnbm1lbnQoJ3R5cGUnLCBjYWxsLmV4cHJlc3Npb24pKTtcbiAgICAgIGlmIChjYWxsLmFyZ3VtZW50cy5sZW5ndGgpIHtcbiAgICAgICAgY29uc3QgYXJnczogdHMuRXhwcmVzc2lvbltdID0gW107XG4gICAgICAgIGZvciAoY29uc3QgYXJnIG9mIGNhbGwuYXJndW1lbnRzKSB7XG4gICAgICAgICAgYXJncy5wdXNoKGFyZyk7XG4gICAgICAgIH1cbiAgICAgICAgY29uc3QgYXJnc0FycmF5TGl0ZXJhbCA9IHRzLmNyZWF0ZUFycmF5TGl0ZXJhbChhcmdzKTtcbiAgICAgICAgYXJnc0FycmF5TGl0ZXJhbC5lbGVtZW50cy5oYXNUcmFpbGluZ0NvbW1hID0gdHJ1ZTtcbiAgICAgICAgbWV0YWRhdGFQcm9wZXJ0aWVzLnB1c2godHMuY3JlYXRlUHJvcGVydHlBc3NpZ25tZW50KCdhcmdzJywgYXJnc0FycmF5TGl0ZXJhbCkpO1xuICAgICAgfVxuICAgICAgYnJlYWs7XG4gICAgZGVmYXVsdDpcbiAgICAgIGRpYWdub3N0aWNzLnB1c2goe1xuICAgICAgICBmaWxlOiBkZWNvcmF0b3IuZ2V0U291cmNlRmlsZSgpLFxuICAgICAgICBzdGFydDogZGVjb3JhdG9yLmdldFN0YXJ0KCksXG4gICAgICAgIGxlbmd0aDogZGVjb3JhdG9yLmdldEVuZCgpIC0gZGVjb3JhdG9yLmdldFN0YXJ0KCksXG4gICAgICAgIG1lc3NhZ2VUZXh0OlxuICAgICAgICAgICAgYCR7dHMuU3ludGF4S2luZFtkZWNvcmF0b3Iua2luZF19IG5vdCBpbXBsZW1lbnRlZCBpbiBnYXRoZXJpbmcgZGVjb3JhdG9yIG1ldGFkYXRhLmAsXG4gICAgICAgIGNhdGVnb3J5OiB0cy5EaWFnbm9zdGljQ2F0ZWdvcnkuRXJyb3IsXG4gICAgICAgIGNvZGU6IDAsXG4gICAgICB9KTtcbiAgICAgIGJyZWFrO1xuICB9XG4gIHJldHVybiB0cy5jcmVhdGVPYmplY3RMaXRlcmFsKG1ldGFkYXRhUHJvcGVydGllcyk7XG59XG5cbi8qKlxuICogVGFrZXMgYSBsaXN0IG9mIGRlY29yYXRvciBtZXRhZGF0YSBvYmplY3QgQVNUcyBhbmQgcHJvZHVjZXMgYW4gQVNUIGZvciBhXG4gKiBzdGF0aWMgY2xhc3MgcHJvcGVydHkgb2YgYW4gYXJyYXkgb2YgdGhvc2UgbWV0YWRhdGEgb2JqZWN0cy5cbiAqL1xuZnVuY3Rpb24gY3JlYXRlRGVjb3JhdG9yQ2xhc3NQcm9wZXJ0eShkZWNvcmF0b3JMaXN0OiB0cy5PYmplY3RMaXRlcmFsRXhwcmVzc2lvbltdKSB7XG4gIGNvbnN0IG1vZGlmaWVyID0gdHMuY3JlYXRlVG9rZW4odHMuU3ludGF4S2luZC5TdGF0aWNLZXl3b3JkKTtcbiAgY29uc3QgdHlwZSA9IGNyZWF0ZURlY29yYXRvckludm9jYXRpb25UeXBlKCk7XG4gIGNvbnN0IGluaXRpYWxpemVyID0gdHMuY3JlYXRlQXJyYXlMaXRlcmFsKGRlY29yYXRvckxpc3QsIHRydWUpO1xuICAvLyBOQjogdGhlIC5kZWNvcmF0b3JzIHByb3BlcnR5IGRvZXMgbm90IGdldCBhIEBub2NvbGxhcHNlIHByb3BlcnR5LiBUaGVyZSBpc1xuICAvLyBubyBnb29kIHJlYXNvbiB3aHkgLSBpdCBtZWFucyAuZGVjb3JhdG9ycyBpcyBub3QgcnVudGltZSBhY2Nlc3NpYmxlIGlmIHlvdVxuICAvLyBjb21waWxlIHdpdGggY29sbGFwc2UgcHJvcGVydGllcywgd2hlcmVhcyBwcm9wRGVjb3JhdG9ycyBpcywgd2hpY2ggZG9lc24ndFxuICAvLyBmb2xsb3cgYW55IHN0cmluZ2VudCBsb2dpYy4gSG93ZXZlciB0aGlzIGhhcyBiZWVuIHRoZSBjYXNlIHByZXZpb3VzbHksIGFuZFxuICAvLyBhZGRpbmcgaXQgYmFjayBpbiBsZWFkcyB0byBzdWJzdGFudGlhbCBjb2RlIHNpemUgaW5jcmVhc2VzIGFzIENsb3N1cmUgZmFpbHNcbiAgLy8gdG8gdHJlZSBzaGFrZSB0aGVzZSBwcm9wcyB3aXRob3V0IEBub2NvbGxhcHNlLlxuICByZXR1cm4gdHMuY3JlYXRlUHJvcGVydHkodW5kZWZpbmVkLCBbbW9kaWZpZXJdLCAnZGVjb3JhdG9ycycsIHVuZGVmaW5lZCwgdHlwZSwgaW5pdGlhbGl6ZXIpO1xufVxuXG4vKipcbiAqIENyZWF0ZXMgdGhlIEFTVCBmb3IgdGhlICdjdG9yUGFyYW1ldGVycycgZmllbGQgdHlwZSBhbm5vdGF0aW9uOlxuICogICAoKSA9PiAoeyB0eXBlOiBhbnksIGRlY29yYXRvcnM/OiB7dHlwZTogRnVuY3Rpb24sIGFyZ3M/OiBhbnlbXX1bXSB9fG51bGwpW11cbiAqL1xuZnVuY3Rpb24gY3JlYXRlQ3RvclBhcmFtZXRlcnNDbGFzc1Byb3BlcnR5VHlwZSgpOiB0cy5UeXBlTm9kZSB7XG4gIC8vIFNvcnJ5IGFib3V0IHRoaXMuIFRyeSByZWFkaW5nIGp1c3QgdGhlIHN0cmluZyBsaXRlcmFscyBiZWxvdy5cbiAgY29uc3QgdHlwZUVsZW1lbnRzOiB0cy5UeXBlRWxlbWVudFtdID0gW107XG4gIHR5cGVFbGVtZW50cy5wdXNoKHRzLmNyZWF0ZVByb3BlcnR5U2lnbmF0dXJlKFxuICAgICAgdW5kZWZpbmVkLCAndHlwZScsIHVuZGVmaW5lZCxcbiAgICAgIHRzLmNyZWF0ZVR5cGVSZWZlcmVuY2VOb2RlKHRzLmNyZWF0ZUlkZW50aWZpZXIoJ2FueScpLCB1bmRlZmluZWQpLCB1bmRlZmluZWQpKTtcbiAgdHlwZUVsZW1lbnRzLnB1c2godHMuY3JlYXRlUHJvcGVydHlTaWduYXR1cmUoXG4gICAgICB1bmRlZmluZWQsICdkZWNvcmF0b3JzJywgdHMuY3JlYXRlVG9rZW4odHMuU3ludGF4S2luZC5RdWVzdGlvblRva2VuKSxcbiAgICAgIHRzLmNyZWF0ZUFycmF5VHlwZU5vZGUodHMuY3JlYXRlVHlwZUxpdGVyYWxOb2RlKFtcbiAgICAgICAgdHMuY3JlYXRlUHJvcGVydHlTaWduYXR1cmUoXG4gICAgICAgICAgICB1bmRlZmluZWQsICd0eXBlJywgdW5kZWZpbmVkLFxuICAgICAgICAgICAgdHMuY3JlYXRlVHlwZVJlZmVyZW5jZU5vZGUodHMuY3JlYXRlSWRlbnRpZmllcignRnVuY3Rpb24nKSwgdW5kZWZpbmVkKSwgdW5kZWZpbmVkKSxcbiAgICAgICAgdHMuY3JlYXRlUHJvcGVydHlTaWduYXR1cmUoXG4gICAgICAgICAgICB1bmRlZmluZWQsICdhcmdzJywgdHMuY3JlYXRlVG9rZW4odHMuU3ludGF4S2luZC5RdWVzdGlvblRva2VuKSxcbiAgICAgICAgICAgIHRzLmNyZWF0ZUFycmF5VHlwZU5vZGUoXG4gICAgICAgICAgICAgICAgdHMuY3JlYXRlVHlwZVJlZmVyZW5jZU5vZGUodHMuY3JlYXRlSWRlbnRpZmllcignYW55JyksIHVuZGVmaW5lZCkpLFxuICAgICAgICAgICAgdW5kZWZpbmVkKSxcbiAgICAgIF0pKSxcbiAgICAgIHVuZGVmaW5lZCkpO1xuICByZXR1cm4gdHMuY3JlYXRlRnVuY3Rpb25UeXBlTm9kZShcbiAgICAgIHVuZGVmaW5lZCwgW10sXG4gICAgICB0cy5jcmVhdGVBcnJheVR5cGVOb2RlKFxuICAgICAgICAgIHRzLmNyZWF0ZVVuaW9uVHlwZU5vZGUoW3RzLmNyZWF0ZVR5cGVMaXRlcmFsTm9kZSh0eXBlRWxlbWVudHMpLCB0cy5jcmVhdGVOdWxsKCldKSkpO1xufVxuXG4vKipcbiAqIFNldHMgYSBDbG9zdXJlIFxcQG5vY29sbGFwc2Ugc3ludGhldGljIGNvbW1lbnQgb24gdGhlIGdpdmVuIG5vZGUuIFRoaXMgcHJldmVudHMgQ2xvc3VyZSBDb21waWxlclxuICogZnJvbSBjb2xsYXBzaW5nIHRoZSBhcHBhcmVudGx5IHN0YXRpYyBwcm9wZXJ0eSwgd2hpY2ggd291bGQgbWFrZSBpdCBpbXBvc3NpYmxlIHRvIGZpbmQgZm9yIGNvZGVcbiAqIHRyeWluZyB0byBkZXRlY3QgaXQgYXQgcnVudGltZS5cbiAqL1xuZnVuY3Rpb24gYWRkTm9Db2xsYXBzZUNvbW1lbnQobjogdHMuTm9kZSkge1xuICB0cy5zZXRTeW50aGV0aWNMZWFkaW5nQ29tbWVudHMobiwgW3tcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAga2luZDogdHMuU3ludGF4S2luZC5NdWx0aUxpbmVDb21tZW50VHJpdmlhLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB0ZXh0OiAnKiBAbm9jb2xsYXBzZSAnLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwb3M6IC0xLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBlbmQ6IC0xLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBoYXNUcmFpbGluZ05ld0xpbmU6IHRydWVcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1dKTtcbn1cblxuLyoqXG4gKiBjcmVhdGVDdG9yUGFyYW1ldGVyc0NsYXNzUHJvcGVydHkgY3JlYXRlcyBhIHN0YXRpYyAnY3RvclBhcmFtZXRlcnMnIHByb3BlcnR5IGNvbnRhaW5pbmdcbiAqIGRvd25sZXZlbGVkIGRlY29yYXRvciBpbmZvcm1hdGlvbi5cbiAqXG4gKiBUaGUgcHJvcGVydHkgY29udGFpbnMgYW4gYXJyb3cgZnVuY3Rpb24gdGhhdCByZXR1cm5zIGFuIGFycmF5IG9mIG9iamVjdCBsaXRlcmFscyBvZiB0aGUgc2hhcGU6XG4gKiAgICAgc3RhdGljIGN0b3JQYXJhbWV0ZXJzID0gKCkgPT4gW3tcbiAqICAgICAgIHR5cGU6IFNvbWVDbGFzc3x1bmRlZmluZWQsICAvLyB0aGUgdHlwZSBvZiB0aGUgcGFyYW0gdGhhdCdzIGRlY29yYXRlZCwgaWYgaXQncyBhIHZhbHVlLlxuICogICAgICAgZGVjb3JhdG9yczogW3tcbiAqICAgICAgICAgdHlwZTogRGVjb3JhdG9yRm4sICAvLyB0aGUgdHlwZSBvZiB0aGUgZGVjb3JhdG9yIHRoYXQncyBpbnZva2VkLlxuICogICAgICAgICBhcmdzOiBbQVJHU10sICAgICAgIC8vIHRoZSBhcmd1bWVudHMgcGFzc2VkIHRvIHRoZSBkZWNvcmF0b3IuXG4gKiAgICAgICB9XVxuICogICAgIH1dO1xuICovXG5mdW5jdGlvbiBjcmVhdGVDdG9yUGFyYW1ldGVyc0NsYXNzUHJvcGVydHkoXG4gICAgZGlhZ25vc3RpY3M6IHRzLkRpYWdub3N0aWNbXSxcbiAgICBlbnRpdHlOYW1lVG9FeHByZXNzaW9uOiAobjogdHMuRW50aXR5TmFtZSkgPT4gdHMuRXhwcmVzc2lvbiB8IHVuZGVmaW5lZCxcbiAgICBjdG9yUGFyYW1ldGVyczogUGFyYW1ldGVyRGVjb3JhdGlvbkluZm9bXSxcbiAgICBpc0Nsb3N1cmVDb21waWxlckVuYWJsZWQ6IGJvb2xlYW4pOiB0cy5Qcm9wZXJ0eURlY2xhcmF0aW9uIHtcbiAgY29uc3QgcGFyYW1zOiB0cy5FeHByZXNzaW9uW10gPSBbXTtcblxuICBmb3IgKGNvbnN0IGN0b3JQYXJhbSBvZiBjdG9yUGFyYW1ldGVycykge1xuICAgIGlmICghY3RvclBhcmFtLnR5cGUgJiYgY3RvclBhcmFtLmRlY29yYXRvcnMubGVuZ3RoID09PSAwKSB7XG4gICAgICBwYXJhbXMucHVzaCh0cy5jcmVhdGVOdWxsKCkpO1xuICAgICAgY29udGludWU7XG4gICAgfVxuXG4gICAgY29uc3QgcGFyYW1UeXBlID0gY3RvclBhcmFtLnR5cGUgP1xuICAgICAgICB0eXBlUmVmZXJlbmNlVG9FeHByZXNzaW9uKGVudGl0eU5hbWVUb0V4cHJlc3Npb24sIGN0b3JQYXJhbS50eXBlKSA6XG4gICAgICAgIHVuZGVmaW5lZDtcbiAgICBjb25zdCBtZW1iZXJzID1cbiAgICAgICAgW3RzLmNyZWF0ZVByb3BlcnR5QXNzaWdubWVudCgndHlwZScsIHBhcmFtVHlwZSB8fCB0cy5jcmVhdGVJZGVudGlmaWVyKCd1bmRlZmluZWQnKSldO1xuXG4gICAgY29uc3QgZGVjb3JhdG9yczogdHMuT2JqZWN0TGl0ZXJhbEV4cHJlc3Npb25bXSA9IFtdO1xuICAgIGZvciAoY29uc3QgZGVjbyBvZiBjdG9yUGFyYW0uZGVjb3JhdG9ycykge1xuICAgICAgZGVjb3JhdG9ycy5wdXNoKGV4dHJhY3RNZXRhZGF0YUZyb21TaW5nbGVEZWNvcmF0b3IoZGVjbywgZGlhZ25vc3RpY3MpKTtcbiAgICB9XG4gICAgaWYgKGRlY29yYXRvcnMubGVuZ3RoKSB7XG4gICAgICBtZW1iZXJzLnB1c2godHMuY3JlYXRlUHJvcGVydHlBc3NpZ25tZW50KCdkZWNvcmF0b3JzJywgdHMuY3JlYXRlQXJyYXlMaXRlcmFsKGRlY29yYXRvcnMpKSk7XG4gICAgfVxuICAgIHBhcmFtcy5wdXNoKHRzLmNyZWF0ZU9iamVjdExpdGVyYWwobWVtYmVycykpO1xuICB9XG5cbiAgY29uc3QgaW5pdGlhbGl6ZXIgPSB0cy5jcmVhdGVBcnJvd0Z1bmN0aW9uKFxuICAgICAgdW5kZWZpbmVkLCB1bmRlZmluZWQsIFtdLCB1bmRlZmluZWQsIHRzLmNyZWF0ZVRva2VuKHRzLlN5bnRheEtpbmQuRXF1YWxzR3JlYXRlclRoYW5Ub2tlbiksXG4gICAgICB0cy5jcmVhdGVBcnJheUxpdGVyYWwocGFyYW1zLCB0cnVlKSk7XG4gIGNvbnN0IHR5cGUgPSBjcmVhdGVDdG9yUGFyYW1ldGVyc0NsYXNzUHJvcGVydHlUeXBlKCk7XG4gIGNvbnN0IGN0b3JQcm9wID0gdHMuY3JlYXRlUHJvcGVydHkoXG4gICAgICB1bmRlZmluZWQsIFt0cy5jcmVhdGVUb2tlbih0cy5TeW50YXhLaW5kLlN0YXRpY0tleXdvcmQpXSwgJ2N0b3JQYXJhbWV0ZXJzJywgdW5kZWZpbmVkLCB0eXBlLFxuICAgICAgaW5pdGlhbGl6ZXIpO1xuICBpZiAoaXNDbG9zdXJlQ29tcGlsZXJFbmFibGVkKSB7XG4gICAgYWRkTm9Db2xsYXBzZUNvbW1lbnQoY3RvclByb3ApO1xuICB9XG4gIHJldHVybiBjdG9yUHJvcDtcbn1cblxuLyoqXG4gKiBjcmVhdGVQcm9wRGVjb3JhdG9yc0NsYXNzUHJvcGVydHkgY3JlYXRlcyBhIHN0YXRpYyAncHJvcERlY29yYXRvcnMnIHByb3BlcnR5IGNvbnRhaW5pbmcgdHlwZVxuICogaW5mb3JtYXRpb24gZm9yIGV2ZXJ5IHByb3BlcnR5IHRoYXQgaGFzIGEgZGVjb3JhdG9yIGFwcGxpZWQuXG4gKlxuICogICAgIHN0YXRpYyBwcm9wRGVjb3JhdG9yczoge1trZXk6IHN0cmluZ106IHt0eXBlOiBGdW5jdGlvbiwgYXJncz86IGFueVtdfVtdfSA9IHtcbiAqICAgICAgIHByb3BBOiBbe3R5cGU6IE15RGVjb3JhdG9yLCBhcmdzOiBbMSwgMl19LCAuLi5dLFxuICogICAgICAgLi4uXG4gKiAgICAgfTtcbiAqL1xuZnVuY3Rpb24gY3JlYXRlUHJvcERlY29yYXRvcnNDbGFzc1Byb3BlcnR5KFxuICAgIGRpYWdub3N0aWNzOiB0cy5EaWFnbm9zdGljW10sIHByb3BlcnRpZXM6IE1hcDxzdHJpbmcsIHRzLkRlY29yYXRvcltdPik6IHRzLlByb3BlcnR5RGVjbGFyYXRpb24ge1xuICAvLyAgYHN0YXRpYyBwcm9wRGVjb3JhdG9yczoge1trZXk6IHN0cmluZ106IGAgKyB7dHlwZTogRnVuY3Rpb24sIGFyZ3M/OiBhbnlbXX1bXSArIGB9ID0ge1xcbmApO1xuICBjb25zdCBlbnRyaWVzOiB0cy5PYmplY3RMaXRlcmFsRWxlbWVudExpa2VbXSA9IFtdO1xuICBmb3IgKGNvbnN0IFtuYW1lLCBkZWNvcmF0b3JzXSBvZiBwcm9wZXJ0aWVzLmVudHJpZXMoKSkge1xuICAgIGVudHJpZXMucHVzaCh0cy5jcmVhdGVQcm9wZXJ0eUFzc2lnbm1lbnQoXG4gICAgICAgIG5hbWUsXG4gICAgICAgIHRzLmNyZWF0ZUFycmF5TGl0ZXJhbChcbiAgICAgICAgICAgIGRlY29yYXRvcnMubWFwKGRlY28gPT4gZXh0cmFjdE1ldGFkYXRhRnJvbVNpbmdsZURlY29yYXRvcihkZWNvLCBkaWFnbm9zdGljcykpKSkpO1xuICB9XG4gIGNvbnN0IGluaXRpYWxpemVyID0gdHMuY3JlYXRlT2JqZWN0TGl0ZXJhbChlbnRyaWVzLCB0cnVlKTtcbiAgY29uc3QgdHlwZSA9IHRzLmNyZWF0ZVR5cGVMaXRlcmFsTm9kZShbdHMuY3JlYXRlSW5kZXhTaWduYXR1cmUoXG4gICAgICB1bmRlZmluZWQsIHVuZGVmaW5lZCwgW3RzLmNyZWF0ZVBhcmFtZXRlcihcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdW5kZWZpbmVkLCB1bmRlZmluZWQsIHVuZGVmaW5lZCwgJ2tleScsIHVuZGVmaW5lZCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdHMuY3JlYXRlVHlwZVJlZmVyZW5jZU5vZGUoJ3N0cmluZycsIHVuZGVmaW5lZCksIHVuZGVmaW5lZCldLFxuICAgICAgY3JlYXRlRGVjb3JhdG9ySW52b2NhdGlvblR5cGUoKSldKTtcbiAgcmV0dXJuIHRzLmNyZWF0ZVByb3BlcnR5KFxuICAgICAgdW5kZWZpbmVkLCBbdHMuY3JlYXRlVG9rZW4odHMuU3ludGF4S2luZC5TdGF0aWNLZXl3b3JkKV0sICdwcm9wRGVjb3JhdG9ycycsIHVuZGVmaW5lZCwgdHlwZSxcbiAgICAgIGluaXRpYWxpemVyKTtcbn1cblxuLyoqXG4gKiBSZXR1cm5zIGFuIGV4cHJlc3Npb24gcmVwcmVzZW50aW5nIHRoZSAocG90ZW50aWFsbHkpIHZhbHVlIHBhcnQgZm9yIHRoZSBnaXZlbiBub2RlLlxuICpcbiAqIFRoaXMgaXMgYSBwYXJ0aWFsIHJlLWltcGxlbWVudGF0aW9uIG9mIFR5cGVTY3JpcHQncyBzZXJpYWxpemVUeXBlUmVmZXJlbmNlTm9kZS4gVGhpcyBpcyBhXG4gKiB3b3JrYXJvdW5kIGZvciBodHRwczovL2dpdGh1Yi5jb20vTWljcm9zb2Z0L1R5cGVTY3JpcHQvaXNzdWVzLzE3NTE2IChzZXJpYWxpemVUeXBlUmVmZXJlbmNlTm9kZVxuICogbm90IGJlaW5nIGV4cG9zZWQpLiBJbiBwcmFjdGljZSB0aGlzIGltcGxlbWVudGF0aW9uIGlzIHN1ZmZpY2llbnQgZm9yIEFuZ3VsYXIncyB1c2Ugb2YgdHlwZVxuICogbWV0YWRhdGEuXG4gKi9cbmZ1bmN0aW9uIHR5cGVSZWZlcmVuY2VUb0V4cHJlc3Npb24oXG4gICAgZW50aXR5TmFtZVRvRXhwcmVzc2lvbjogKG46IHRzLkVudGl0eU5hbWUpID0+IHRzLkV4cHJlc3Npb24gfCB1bmRlZmluZWQsXG4gICAgbm9kZTogdHMuVHlwZU5vZGUpOiB0cy5FeHByZXNzaW9ufHVuZGVmaW5lZCB7XG4gIGxldCBraW5kID0gbm9kZS5raW5kO1xuICBpZiAodHMuaXNMaXRlcmFsVHlwZU5vZGUobm9kZSkpIHtcbiAgICAvLyBUcmVhdCBsaXRlcmFsIHR5cGVzIGxpa2UgdGhlaXIgYmFzZSB0eXBlIChib29sZWFuLCBzdHJpbmcsIG51bWJlcikuXG4gICAga2luZCA9IG5vZGUubGl0ZXJhbC5raW5kO1xuICB9XG4gIHN3aXRjaCAoa2luZCkge1xuICAgIGNhc2UgdHMuU3ludGF4S2luZC5GdW5jdGlvblR5cGU6XG4gICAgY2FzZSB0cy5TeW50YXhLaW5kLkNvbnN0cnVjdG9yVHlwZTpcbiAgICAgIHJldHVybiB0cy5jcmVhdGVJZGVudGlmaWVyKCdGdW5jdGlvbicpO1xuICAgIGNhc2UgdHMuU3ludGF4S2luZC5BcnJheVR5cGU6XG4gICAgY2FzZSB0cy5TeW50YXhLaW5kLlR1cGxlVHlwZTpcbiAgICAgIHJldHVybiB0cy5jcmVhdGVJZGVudGlmaWVyKCdBcnJheScpO1xuICAgIGNhc2UgdHMuU3ludGF4S2luZC5UeXBlUHJlZGljYXRlOlxuICAgIGNhc2UgdHMuU3ludGF4S2luZC5UcnVlS2V5d29yZDpcbiAgICBjYXNlIHRzLlN5bnRheEtpbmQuRmFsc2VLZXl3b3JkOlxuICAgIGNhc2UgdHMuU3ludGF4S2luZC5Cb29sZWFuS2V5d29yZDpcbiAgICAgIHJldHVybiB0cy5jcmVhdGVJZGVudGlmaWVyKCdCb29sZWFuJyk7XG4gICAgY2FzZSB0cy5TeW50YXhLaW5kLlN0cmluZ0xpdGVyYWw6XG4gICAgY2FzZSB0cy5TeW50YXhLaW5kLlN0cmluZ0tleXdvcmQ6XG4gICAgICByZXR1cm4gdHMuY3JlYXRlSWRlbnRpZmllcignU3RyaW5nJyk7XG4gICAgY2FzZSB0cy5TeW50YXhLaW5kLk9iamVjdEtleXdvcmQ6XG4gICAgICByZXR1cm4gdHMuY3JlYXRlSWRlbnRpZmllcignT2JqZWN0Jyk7XG4gICAgY2FzZSB0cy5TeW50YXhLaW5kLk51bWJlcktleXdvcmQ6XG4gICAgY2FzZSB0cy5TeW50YXhLaW5kLk51bWVyaWNMaXRlcmFsOlxuICAgICAgcmV0dXJuIHRzLmNyZWF0ZUlkZW50aWZpZXIoJ051bWJlcicpO1xuICAgIGNhc2UgdHMuU3ludGF4S2luZC5UeXBlUmVmZXJlbmNlOlxuICAgICAgY29uc3QgdHlwZVJlZiA9IG5vZGUgYXMgdHMuVHlwZVJlZmVyZW5jZU5vZGU7XG4gICAgICAvLyBJZ25vcmUgYW55IGdlbmVyaWMgdHlwZXMsIGp1c3QgcmV0dXJuIHRoZSBiYXNlIHR5cGUuXG4gICAgICByZXR1cm4gZW50aXR5TmFtZVRvRXhwcmVzc2lvbih0eXBlUmVmLnR5cGVOYW1lKTtcbiAgICBjYXNlIHRzLlN5bnRheEtpbmQuVW5pb25UeXBlOlxuICAgICAgY29uc3QgY2hpbGRUeXBlTm9kZXMgPVxuICAgICAgICAgIChub2RlIGFzIHRzLlVuaW9uVHlwZU5vZGUpLnR5cGVzLmZpbHRlcih0ID0+IHQua2luZCAhPT0gdHMuU3ludGF4S2luZC5OdWxsS2V5d29yZCk7XG4gICAgICByZXR1cm4gY2hpbGRUeXBlTm9kZXMubGVuZ3RoID09PSAxID9cbiAgICAgICAgICB0eXBlUmVmZXJlbmNlVG9FeHByZXNzaW9uKGVudGl0eU5hbWVUb0V4cHJlc3Npb24sIGNoaWxkVHlwZU5vZGVzWzBdKSA6XG4gICAgICAgICAgdW5kZWZpbmVkO1xuICAgIGRlZmF1bHQ6XG4gICAgICByZXR1cm4gdW5kZWZpbmVkO1xuICB9XG59XG5cbi8qKlxuICogUmV0dXJucyB0cnVlIGlmIHRoZSBnaXZlbiBzeW1ib2wgcmVmZXJzIHRvIGEgdmFsdWUgKGFzIGRpc3RpbmN0IGZyb20gYSB0eXBlKS5cbiAqXG4gKiBFeHBhbmRzIGFsaWFzZXMsIHdoaWNoIGlzIGltcG9ydGFudCBmb3IgdGhlIGNhc2Ugd2hlcmVcbiAqICAgaW1wb3J0ICogYXMgeCBmcm9tICdzb21lLW1vZHVsZSc7XG4gKiBhbmQgeCBpcyBub3cgYSB2YWx1ZSAodGhlIG1vZHVsZSBvYmplY3QpLlxuICovXG5mdW5jdGlvbiBzeW1ib2xJc1ZhbHVlKHRjOiB0cy5UeXBlQ2hlY2tlciwgc3ltOiB0cy5TeW1ib2wpOiBib29sZWFuIHtcbiAgaWYgKHN5bS5mbGFncyAmIHRzLlN5bWJvbEZsYWdzLkFsaWFzKSBzeW0gPSB0Yy5nZXRBbGlhc2VkU3ltYm9sKHN5bSk7XG4gIHJldHVybiAoc3ltLmZsYWdzICYgdHMuU3ltYm9sRmxhZ3MuVmFsdWUpICE9PSAwO1xufVxuXG4vKiogUGFyYW1ldGVyRGVjb3JhdGlvbkluZm8gZGVzY3JpYmVzIHRoZSBpbmZvcm1hdGlvbiBmb3IgYSBzaW5nbGUgY29uc3RydWN0b3IgcGFyYW1ldGVyLiAqL1xuaW50ZXJmYWNlIFBhcmFtZXRlckRlY29yYXRpb25JbmZvIHtcbiAgLyoqXG4gICAqIFRoZSB0eXBlIGRlY2xhcmF0aW9uIGZvciB0aGUgcGFyYW1ldGVyLiBPbmx5IHNldCBpZiB0aGUgdHlwZSBpcyBhIHZhbHVlIChlLmcuIGEgY2xhc3MsIG5vdCBhblxuICAgKiBpbnRlcmZhY2UpLlxuICAgKi9cbiAgdHlwZTogdHMuVHlwZU5vZGV8bnVsbDtcbiAgLyoqIFRoZSBsaXN0IG9mIGRlY29yYXRvcnMgZm91bmQgb24gdGhlIHBhcmFtZXRlciwgbnVsbCBpZiBub25lLiAqL1xuICBkZWNvcmF0b3JzOiB0cy5EZWNvcmF0b3JbXTtcbn1cblxuLyoqXG4gKiBHZXRzIGEgdHJhbnNmb3JtZXIgZm9yIGRvd25sZXZlbGluZyBBbmd1bGFyIGRlY29yYXRvcnMuXG4gKiBAcGFyYW0gdHlwZUNoZWNrZXIgUmVmZXJlbmNlIHRvIHRoZSBwcm9ncmFtJ3MgdHlwZSBjaGVja2VyLlxuICogQHBhcmFtIGhvc3QgUmVmbGVjdGlvbiBob3N0IHRoYXQgaXMgdXNlZCBmb3IgZGV0ZXJtaW5pbmcgZGVjb3JhdG9ycy5cbiAqIEBwYXJhbSBkaWFnbm9zdGljcyBMaXN0IHdoaWNoIHdpbGwgYmUgcG9wdWxhdGVkIHdpdGggZGlhZ25vc3RpY3MgaWYgYW55LlxuICogQHBhcmFtIGlzQ29yZSBXaGV0aGVyIHRoZSBjdXJyZW50IFR5cGVTY3JpcHQgcHJvZ3JhbSBpcyBmb3IgdGhlIGBAYW5ndWxhci9jb3JlYCBwYWNrYWdlLlxuICogQHBhcmFtIGlzQ2xvc3VyZUNvbXBpbGVyRW5hYmxlZCBXaGV0aGVyIGNsb3N1cmUgYW5ub3RhdGlvbnMgbmVlZCB0byBiZSBhZGRlZCB3aGVyZSBuZWVkZWQuXG4gKiBAcGFyYW0gc2tpcENsYXNzRGVjb3JhdG9ycyBXaGV0aGVyIGNsYXNzIGRlY29yYXRvcnMgc2hvdWxkIGJlIHNraXBwZWQgZnJvbSBkb3dubGV2ZWxpbmcuXG4gKiAgIFRoaXMgaXMgdXNlZnVsIGZvciBKSVQgbW9kZSB3aGVyZSBjbGFzcyBkZWNvcmF0b3JzIHNob3VsZCBiZSBwcmVzZXJ2ZWQgYXMgdGhleSBjb3VsZCByZWx5XG4gKiAgIG9uIGltbWVkaWF0ZSBleGVjdXRpb24uIGUuZy4gZG93bmxldmVsaW5nIGBASW5qZWN0YWJsZWAgbWVhbnMgdGhhdCB0aGUgaW5qZWN0YWJsZSBmYWN0b3J5XG4gKiAgIGlzIG5vdCBjcmVhdGVkLCBhbmQgaW5qZWN0aW5nIHRoZSB0b2tlbiB3aWxsIG5vdCB3b3JrLiBJZiB0aGlzIGRlY29yYXRvciB3b3VsZCBub3QgYmVcbiAqICAgZG93bmxldmVsZWQsIHRoZSBgSW5qZWN0YWJsZWAgZGVjb3JhdG9yIHdpbGwgZXhlY3V0ZSBpbW1lZGlhdGVseSBvbiBmaWxlIGxvYWQsIGFuZFxuICogICBBbmd1bGFyIHdpbGwgZ2VuZXJhdGUgdGhlIGNvcnJlc3BvbmRpbmcgaW5qZWN0YWJsZSBmYWN0b3J5LlxuICovXG5leHBvcnQgZnVuY3Rpb24gZ2V0RG93bmxldmVsRGVjb3JhdG9yc1RyYW5zZm9ybShcbiAgICB0eXBlQ2hlY2tlcjogdHMuVHlwZUNoZWNrZXIsIGhvc3Q6IFJlZmxlY3Rpb25Ib3N0LCBkaWFnbm9zdGljczogdHMuRGlhZ25vc3RpY1tdLFxuICAgIGlzQ29yZTogYm9vbGVhbiwgaXNDbG9zdXJlQ29tcGlsZXJFbmFibGVkOiBib29sZWFuLFxuICAgIHNraXBDbGFzc0RlY29yYXRvcnM6IGJvb2xlYW4pOiB0cy5UcmFuc2Zvcm1lckZhY3Rvcnk8dHMuU291cmNlRmlsZT4ge1xuICByZXR1cm4gKGNvbnRleHQ6IHRzLlRyYW5zZm9ybWF0aW9uQ29udGV4dCkgPT4ge1xuICAgIGxldCByZWZlcmVuY2VkUGFyYW1ldGVyVHlwZXMgPSBuZXcgU2V0PHRzLkRlY2xhcmF0aW9uPigpO1xuXG4gICAgLyoqXG4gICAgICogQ29udmVydHMgYW4gRW50aXR5TmFtZSAoZnJvbSBhIHR5cGUgYW5ub3RhdGlvbikgdG8gYW4gZXhwcmVzc2lvbiAoYWNjZXNzaW5nIGEgdmFsdWUpLlxuICAgICAqXG4gICAgICogRm9yIGEgZ2l2ZW4gcXVhbGlmaWVkIG5hbWUsIHRoaXMgd2Fsa3MgZGVwdGggZmlyc3QgdG8gZmluZCB0aGUgbGVmdG1vc3QgaWRlbnRpZmllcixcbiAgICAgKiBhbmQgdGhlbiBjb252ZXJ0cyB0aGUgcGF0aCBpbnRvIGEgcHJvcGVydHkgYWNjZXNzIHRoYXQgY2FuIGJlIHVzZWQgYXMgZXhwcmVzc2lvbi5cbiAgICAgKi9cbiAgICBmdW5jdGlvbiBlbnRpdHlOYW1lVG9FeHByZXNzaW9uKG5hbWU6IHRzLkVudGl0eU5hbWUpOiB0cy5FeHByZXNzaW9ufHVuZGVmaW5lZCB7XG4gICAgICBjb25zdCBzeW1ib2wgPSB0eXBlQ2hlY2tlci5nZXRTeW1ib2xBdExvY2F0aW9uKG5hbWUpO1xuICAgICAgLy8gQ2hlY2sgaWYgdGhlIGVudGl0eSBuYW1lIHJlZmVyZW5jZXMgYSBzeW1ib2wgdGhhdCBpcyBhbiBhY3R1YWwgdmFsdWUuIElmIGl0IGlzIG5vdCwgaXRcbiAgICAgIC8vIGNhbm5vdCBiZSByZWZlcmVuY2VkIGJ5IGFuIGV4cHJlc3Npb24sIHNvIHJldHVybiB1bmRlZmluZWQuXG4gICAgICBpZiAoIXN5bWJvbCB8fCAhc3ltYm9sSXNWYWx1ZSh0eXBlQ2hlY2tlciwgc3ltYm9sKSB8fCAhc3ltYm9sLmRlY2xhcmF0aW9ucyB8fFxuICAgICAgICAgIHN5bWJvbC5kZWNsYXJhdGlvbnMubGVuZ3RoID09PSAwKSB7XG4gICAgICAgIHJldHVybiB1bmRlZmluZWQ7XG4gICAgICB9XG4gICAgICAvLyBJZiB3ZSBkZWFsIHdpdGggYSBxdWFsaWZpZWQgbmFtZSwgYnVpbGQgdXAgYSBwcm9wZXJ0eSBhY2Nlc3MgZXhwcmVzc2lvblxuICAgICAgLy8gdGhhdCBjb3VsZCBiZSB1c2VkIGluIHRoZSBKYXZhU2NyaXB0IG91dHB1dC5cbiAgICAgIGlmICh0cy5pc1F1YWxpZmllZE5hbWUobmFtZSkpIHtcbiAgICAgICAgY29uc3QgY29udGFpbmVyRXhwciA9IGVudGl0eU5hbWVUb0V4cHJlc3Npb24obmFtZS5sZWZ0KTtcbiAgICAgICAgaWYgKGNvbnRhaW5lckV4cHIgPT09IHVuZGVmaW5lZCkge1xuICAgICAgICAgIHJldHVybiB1bmRlZmluZWQ7XG4gICAgICAgIH1cbiAgICAgICAgcmV0dXJuIHRzLmNyZWF0ZVByb3BlcnR5QWNjZXNzKGNvbnRhaW5lckV4cHIsIG5hbWUucmlnaHQpO1xuICAgICAgfVxuICAgICAgY29uc3QgZGVjbCA9IHN5bWJvbC5kZWNsYXJhdGlvbnNbMF07XG4gICAgICAvLyBJZiB0aGUgZ2l2ZW4gZW50aXR5IG5hbWUgaGFzIGJlZW4gcmVzb2x2ZWQgdG8gYW4gYWxpYXMgaW1wb3J0IGRlY2xhcmF0aW9uLFxuICAgICAgLy8gZW5zdXJlIHRoYXQgdGhlIGFsaWFzIGRlY2xhcmF0aW9uIGlzIG5vdCBlbGlkZWQgYnkgVHlwZVNjcmlwdCwgYW5kIHVzZSBpdHNcbiAgICAgIC8vIG5hbWUgaWRlbnRpZmllciB0byByZWZlcmVuY2UgaXQgYXQgcnVudGltZS5cbiAgICAgIGlmIChpc0FsaWFzSW1wb3J0RGVjbGFyYXRpb24oZGVjbCkpIHtcbiAgICAgICAgcmVmZXJlbmNlZFBhcmFtZXRlclR5cGVzLmFkZChkZWNsKTtcbiAgICAgICAgLy8gSWYgdGhlIGVudGl0eSBuYW1lIHJlc29sdmVzIHRvIGFuIGFsaWFzIGltcG9ydCBkZWNsYXJhdGlvbiwgd2UgcmVmZXJlbmNlIHRoZVxuICAgICAgICAvLyBlbnRpdHkgYmFzZWQgb24gdGhlIGFsaWFzIGltcG9ydCBuYW1lLiBUaGlzIGVuc3VyZXMgdGhhdCBUeXBlU2NyaXB0IHByb3Blcmx5XG4gICAgICAgIC8vIHJlc29sdmVzIHRoZSBsaW5rIHRvIHRoZSBpbXBvcnQuIENsb25pbmcgdGhlIG9yaWdpbmFsIGVudGl0eSBuYW1lIGlkZW50aWZpZXJcbiAgICAgICAgLy8gY291bGQgbGVhZCB0byBhbiBpbmNvcnJlY3QgcmVzb2x1dGlvbiBhdCBsb2NhbCBzY29wZS4gZS5nLiBDb25zaWRlciB0aGUgZm9sbG93aW5nXG4gICAgICAgIC8vIHNuaXBwZXQ6IGBjb25zdHJ1Y3RvcihEZXA6IERlcCkge31gLiBJbiBzdWNoIGEgY2FzZSwgdGhlIGxvY2FsIGBEZXBgIGlkZW50aWZpZXJcbiAgICAgICAgLy8gd291bGQgcmVzb2x2ZSB0byB0aGUgYWN0dWFsIHBhcmFtZXRlciBuYW1lLCBhbmQgbm90IHRvIHRoZSBkZXNpcmVkIGltcG9ydC5cbiAgICAgICAgLy8gVGhpcyBoYXBwZW5zIGJlY2F1c2UgdGhlIGVudGl0eSBuYW1lIGlkZW50aWZpZXIgc3ltYm9sIGlzIGludGVybmFsbHkgY29uc2lkZXJlZFxuICAgICAgICAvLyBhcyB0eXBlLW9ubHkgYW5kIHRoZXJlZm9yZSBUeXBlU2NyaXB0IHRyaWVzIHRvIHJlc29sdmUgaXQgYXMgdmFsdWUgbWFudWFsbHkuXG4gICAgICAgIC8vIFdlIGNhbiBoZWxwIFR5cGVTY3JpcHQgYW5kIGF2b2lkIHRoaXMgbm9uLXJlbGlhYmxlIHJlc29sdXRpb24gYnkgdXNpbmcgYW4gaWRlbnRpZmllclxuICAgICAgICAvLyB0aGF0IGlzIG5vdCB0eXBlLW9ubHkgYW5kIGlzIGRpcmVjdGx5IGxpbmtlZCB0byB0aGUgaW1wb3J0IGFsaWFzIGRlY2xhcmF0aW9uLlxuICAgICAgICBpZiAoZGVjbC5uYW1lICE9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgICByZXR1cm4gdHMuZ2V0TXV0YWJsZUNsb25lKGRlY2wubmFtZSk7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICAgIC8vIENsb25lIHRoZSBvcmlnaW5hbCBlbnRpdHkgbmFtZSBpZGVudGlmaWVyIHNvIHRoYXQgaXQgY2FuIGJlIHVzZWQgdG8gcmVmZXJlbmNlXG4gICAgICAvLyBpdHMgdmFsdWUgYXQgcnVudGltZS4gVGhpcyBpcyB1c2VkIHdoZW4gdGhlIGlkZW50aWZpZXIgaXMgcmVzb2x2aW5nIHRvIGEgZmlsZVxuICAgICAgLy8gbG9jYWwgZGVjbGFyYXRpb24gKG90aGVyd2lzZSBpdCB3b3VsZCByZXNvbHZlIHRvIGFuIGFsaWFzIGltcG9ydCBkZWNsYXJhdGlvbikuXG4gICAgICByZXR1cm4gdHMuZ2V0TXV0YWJsZUNsb25lKG5hbWUpO1xuICAgIH1cblxuICAgIC8qKlxuICAgICAqIFRyYW5zZm9ybXMgYSBjbGFzcyBlbGVtZW50LiBSZXR1cm5zIGEgdGhyZWUgdHVwbGUgb2YgbmFtZSwgdHJhbnNmb3JtZWQgZWxlbWVudCwgYW5kXG4gICAgICogZGVjb3JhdG9ycyBmb3VuZC4gUmV0dXJucyBhbiB1bmRlZmluZWQgbmFtZSBpZiB0aGVyZSBhcmUgbm8gZGVjb3JhdG9ycyB0byBsb3dlciBvbiB0aGVcbiAgICAgKiBlbGVtZW50LCBvciB0aGUgZWxlbWVudCBoYXMgYW4gZXhvdGljIG5hbWUuXG4gICAgICovXG4gICAgZnVuY3Rpb24gdHJhbnNmb3JtQ2xhc3NFbGVtZW50KGVsZW1lbnQ6IHRzLkNsYXNzRWxlbWVudCk6XG4gICAgICAgIFtzdHJpbmd8dW5kZWZpbmVkLCB0cy5DbGFzc0VsZW1lbnQsIHRzLkRlY29yYXRvcltdXSB7XG4gICAgICBlbGVtZW50ID0gdHMudmlzaXRFYWNoQ2hpbGQoZWxlbWVudCwgZGVjb3JhdG9yRG93bmxldmVsVmlzaXRvciwgY29udGV4dCk7XG4gICAgICBjb25zdCBkZWNvcmF0b3JzVG9LZWVwOiB0cy5EZWNvcmF0b3JbXSA9IFtdO1xuICAgICAgY29uc3QgdG9Mb3dlcjogdHMuRGVjb3JhdG9yW10gPSBbXTtcbiAgICAgIGNvbnN0IGRlY29yYXRvcnMgPSBob3N0LmdldERlY29yYXRvcnNPZkRlY2xhcmF0aW9uKGVsZW1lbnQpIHx8IFtdO1xuICAgICAgZm9yIChjb25zdCBkZWNvcmF0b3Igb2YgZGVjb3JhdG9ycykge1xuICAgICAgICAvLyBXZSBvbmx5IGRlYWwgd2l0aCBjb25jcmV0ZSBub2RlcyBpbiBUeXBlU2NyaXB0IHNvdXJjZXMsIHNvIHdlIGRvbid0XG4gICAgICAgIC8vIG5lZWQgdG8gaGFuZGxlIHN5bnRoZXRpY2FsbHkgY3JlYXRlZCBkZWNvcmF0b3JzLlxuICAgICAgICBjb25zdCBkZWNvcmF0b3JOb2RlID0gZGVjb3JhdG9yLm5vZGUhIGFzIHRzLkRlY29yYXRvcjtcbiAgICAgICAgaWYgKCFpc0FuZ3VsYXJEZWNvcmF0b3IoZGVjb3JhdG9yLCBpc0NvcmUpKSB7XG4gICAgICAgICAgZGVjb3JhdG9yc1RvS2VlcC5wdXNoKGRlY29yYXRvck5vZGUpO1xuICAgICAgICAgIGNvbnRpbnVlO1xuICAgICAgICB9XG4gICAgICAgIHRvTG93ZXIucHVzaChkZWNvcmF0b3JOb2RlKTtcbiAgICAgIH1cbiAgICAgIGlmICghdG9Mb3dlci5sZW5ndGgpIHJldHVybiBbdW5kZWZpbmVkLCBlbGVtZW50LCBbXV07XG5cbiAgICAgIGlmICghZWxlbWVudC5uYW1lIHx8ICF0cy5pc0lkZW50aWZpZXIoZWxlbWVudC5uYW1lKSkge1xuICAgICAgICAvLyBNZXRob2QgaGFzIGEgd2VpcmQgbmFtZSwgZS5nLlxuICAgICAgICAvLyAgIFtTeW1ib2wuZm9vXSgpIHsuLi59XG4gICAgICAgIGRpYWdub3N0aWNzLnB1c2goe1xuICAgICAgICAgIGZpbGU6IGVsZW1lbnQuZ2V0U291cmNlRmlsZSgpLFxuICAgICAgICAgIHN0YXJ0OiBlbGVtZW50LmdldFN0YXJ0KCksXG4gICAgICAgICAgbGVuZ3RoOiBlbGVtZW50LmdldEVuZCgpIC0gZWxlbWVudC5nZXRTdGFydCgpLFxuICAgICAgICAgIG1lc3NhZ2VUZXh0OiBgQ2Fubm90IHByb2Nlc3MgZGVjb3JhdG9ycyBmb3IgY2xhc3MgZWxlbWVudCB3aXRoIG5vbi1hbmFseXphYmxlIG5hbWUuYCxcbiAgICAgICAgICBjYXRlZ29yeTogdHMuRGlhZ25vc3RpY0NhdGVnb3J5LkVycm9yLFxuICAgICAgICAgIGNvZGU6IDAsXG4gICAgICAgIH0pO1xuICAgICAgICByZXR1cm4gW3VuZGVmaW5lZCwgZWxlbWVudCwgW11dO1xuICAgICAgfVxuXG4gICAgICBjb25zdCBuYW1lID0gKGVsZW1lbnQubmFtZSBhcyB0cy5JZGVudGlmaWVyKS50ZXh0O1xuICAgICAgY29uc3QgbXV0YWJsZSA9IHRzLmdldE11dGFibGVDbG9uZShlbGVtZW50KTtcbiAgICAgIG11dGFibGUuZGVjb3JhdG9ycyA9IGRlY29yYXRvcnNUb0tlZXAubGVuZ3RoID9cbiAgICAgICAgICB0cy5zZXRUZXh0UmFuZ2UodHMuY3JlYXRlTm9kZUFycmF5KGRlY29yYXRvcnNUb0tlZXApLCBtdXRhYmxlLmRlY29yYXRvcnMpIDpcbiAgICAgICAgICB1bmRlZmluZWQ7XG4gICAgICByZXR1cm4gW25hbWUsIG11dGFibGUsIHRvTG93ZXJdO1xuICAgIH1cblxuICAgIC8qKlxuICAgICAqIFRyYW5zZm9ybXMgYSBjb25zdHJ1Y3Rvci4gUmV0dXJucyB0aGUgdHJhbnNmb3JtZWQgY29uc3RydWN0b3IgYW5kIHRoZSBsaXN0IG9mIHBhcmFtZXRlclxuICAgICAqIGluZm9ybWF0aW9uIGNvbGxlY3RlZCwgY29uc2lzdGluZyBvZiBkZWNvcmF0b3JzIGFuZCBvcHRpb25hbCB0eXBlLlxuICAgICAqL1xuICAgIGZ1bmN0aW9uIHRyYW5zZm9ybUNvbnN0cnVjdG9yKGN0b3I6IHRzLkNvbnN0cnVjdG9yRGVjbGFyYXRpb24pOlxuICAgICAgICBbdHMuQ29uc3RydWN0b3JEZWNsYXJhdGlvbiwgUGFyYW1ldGVyRGVjb3JhdGlvbkluZm9bXV0ge1xuICAgICAgY3RvciA9IHRzLnZpc2l0RWFjaENoaWxkKGN0b3IsIGRlY29yYXRvckRvd25sZXZlbFZpc2l0b3IsIGNvbnRleHQpO1xuXG4gICAgICBjb25zdCBuZXdQYXJhbWV0ZXJzOiB0cy5QYXJhbWV0ZXJEZWNsYXJhdGlvbltdID0gW107XG4gICAgICBjb25zdCBvbGRQYXJhbWV0ZXJzID1cbiAgICAgICAgICB0cy52aXNpdFBhcmFtZXRlckxpc3QoY3Rvci5wYXJhbWV0ZXJzLCBkZWNvcmF0b3JEb3dubGV2ZWxWaXNpdG9yLCBjb250ZXh0KTtcbiAgICAgIGNvbnN0IHBhcmFtZXRlcnNJbmZvOiBQYXJhbWV0ZXJEZWNvcmF0aW9uSW5mb1tdID0gW107XG4gICAgICBmb3IgKGNvbnN0IHBhcmFtIG9mIG9sZFBhcmFtZXRlcnMpIHtcbiAgICAgICAgY29uc3QgZGVjb3JhdG9yc1RvS2VlcDogdHMuRGVjb3JhdG9yW10gPSBbXTtcbiAgICAgICAgY29uc3QgcGFyYW1JbmZvOiBQYXJhbWV0ZXJEZWNvcmF0aW9uSW5mbyA9IHtkZWNvcmF0b3JzOiBbXSwgdHlwZTogbnVsbH07XG4gICAgICAgIGNvbnN0IGRlY29yYXRvcnMgPSBob3N0LmdldERlY29yYXRvcnNPZkRlY2xhcmF0aW9uKHBhcmFtKSB8fCBbXTtcblxuICAgICAgICBmb3IgKGNvbnN0IGRlY29yYXRvciBvZiBkZWNvcmF0b3JzKSB7XG4gICAgICAgICAgLy8gV2Ugb25seSBkZWFsIHdpdGggY29uY3JldGUgbm9kZXMgaW4gVHlwZVNjcmlwdCBzb3VyY2VzLCBzbyB3ZSBkb24ndFxuICAgICAgICAgIC8vIG5lZWQgdG8gaGFuZGxlIHN5bnRoZXRpY2FsbHkgY3JlYXRlZCBkZWNvcmF0b3JzLlxuICAgICAgICAgIGNvbnN0IGRlY29yYXRvck5vZGUgPSBkZWNvcmF0b3Iubm9kZSEgYXMgdHMuRGVjb3JhdG9yO1xuICAgICAgICAgIGlmICghaXNBbmd1bGFyRGVjb3JhdG9yKGRlY29yYXRvciwgaXNDb3JlKSkge1xuICAgICAgICAgICAgZGVjb3JhdG9yc1RvS2VlcC5wdXNoKGRlY29yYXRvck5vZGUpO1xuICAgICAgICAgICAgY29udGludWU7XG4gICAgICAgICAgfVxuICAgICAgICAgIHBhcmFtSW5mbyEuZGVjb3JhdG9ycy5wdXNoKGRlY29yYXRvck5vZGUpO1xuICAgICAgICB9XG4gICAgICAgIGlmIChwYXJhbS50eXBlKSB7XG4gICAgICAgICAgLy8gcGFyYW0gaGFzIGEgdHlwZSBwcm92aWRlZCwgZS5nLiBcImZvbzogQmFyXCIuXG4gICAgICAgICAgLy8gVGhlIHR5cGUgd2lsbCBiZSBlbWl0dGVkIGFzIGEgdmFsdWUgZXhwcmVzc2lvbiBpbiBlbnRpdHlOYW1lVG9FeHByZXNzaW9uLCB3aGljaCB0YWtlc1xuICAgICAgICAgIC8vIGNhcmUgbm90IHRvIGVtaXQgYW55dGhpbmcgZm9yIHR5cGVzIHRoYXQgY2Fubm90IGJlIGV4cHJlc3NlZCBhcyBhIHZhbHVlIChlLmcuXG4gICAgICAgICAgLy8gaW50ZXJmYWNlcykuXG4gICAgICAgICAgcGFyYW1JbmZvIS50eXBlID0gcGFyYW0udHlwZTtcbiAgICAgICAgfVxuICAgICAgICBwYXJhbWV0ZXJzSW5mby5wdXNoKHBhcmFtSW5mbyk7XG4gICAgICAgIGNvbnN0IG5ld1BhcmFtID0gdHMudXBkYXRlUGFyYW1ldGVyKFxuICAgICAgICAgICAgcGFyYW0sXG4gICAgICAgICAgICAvLyBNdXN0IHBhc3MgJ3VuZGVmaW5lZCcgdG8gYXZvaWQgZW1pdHRpbmcgZGVjb3JhdG9yIG1ldGFkYXRhLlxuICAgICAgICAgICAgZGVjb3JhdG9yc1RvS2VlcC5sZW5ndGggPyBkZWNvcmF0b3JzVG9LZWVwIDogdW5kZWZpbmVkLCBwYXJhbS5tb2RpZmllcnMsXG4gICAgICAgICAgICBwYXJhbS5kb3REb3REb3RUb2tlbiwgcGFyYW0ubmFtZSwgcGFyYW0ucXVlc3Rpb25Ub2tlbiwgcGFyYW0udHlwZSwgcGFyYW0uaW5pdGlhbGl6ZXIpO1xuICAgICAgICBuZXdQYXJhbWV0ZXJzLnB1c2gobmV3UGFyYW0pO1xuICAgICAgfVxuICAgICAgY29uc3QgdXBkYXRlZCA9IHRzLnVwZGF0ZUNvbnN0cnVjdG9yKFxuICAgICAgICAgIGN0b3IsIGN0b3IuZGVjb3JhdG9ycywgY3Rvci5tb2RpZmllcnMsIG5ld1BhcmFtZXRlcnMsXG4gICAgICAgICAgdHMudmlzaXRGdW5jdGlvbkJvZHkoY3Rvci5ib2R5LCBkZWNvcmF0b3JEb3dubGV2ZWxWaXNpdG9yLCBjb250ZXh0KSk7XG4gICAgICByZXR1cm4gW3VwZGF0ZWQsIHBhcmFtZXRlcnNJbmZvXTtcbiAgICB9XG5cbiAgICAvKipcbiAgICAgKiBUcmFuc2Zvcm1zIGEgc2luZ2xlIGNsYXNzIGRlY2xhcmF0aW9uOlxuICAgICAqIC0gZGlzcGF0Y2hlcyB0byBzdHJpcCBkZWNvcmF0b3JzIG9uIG1lbWJlcnNcbiAgICAgKiAtIGNvbnZlcnRzIGRlY29yYXRvcnMgb24gdGhlIGNsYXNzIHRvIGFubm90YXRpb25zXG4gICAgICogLSBjcmVhdGVzIGEgY3RvclBhcmFtZXRlcnMgcHJvcGVydHlcbiAgICAgKiAtIGNyZWF0ZXMgYSBwcm9wRGVjb3JhdG9ycyBwcm9wZXJ0eVxuICAgICAqL1xuICAgIGZ1bmN0aW9uIHRyYW5zZm9ybUNsYXNzRGVjbGFyYXRpb24oY2xhc3NEZWNsOiB0cy5DbGFzc0RlY2xhcmF0aW9uKTogdHMuQ2xhc3NEZWNsYXJhdGlvbiB7XG4gICAgICBjbGFzc0RlY2wgPSB0cy5nZXRNdXRhYmxlQ2xvbmUoY2xhc3NEZWNsKTtcblxuICAgICAgY29uc3QgbmV3TWVtYmVyczogdHMuQ2xhc3NFbGVtZW50W10gPSBbXTtcbiAgICAgIGNvbnN0IGRlY29yYXRlZFByb3BlcnRpZXMgPSBuZXcgTWFwPHN0cmluZywgdHMuRGVjb3JhdG9yW10+KCk7XG4gICAgICBsZXQgY2xhc3NQYXJhbWV0ZXJzOiBQYXJhbWV0ZXJEZWNvcmF0aW9uSW5mb1tdfG51bGwgPSBudWxsO1xuXG4gICAgICBmb3IgKGNvbnN0IG1lbWJlciBvZiBjbGFzc0RlY2wubWVtYmVycykge1xuICAgICAgICBzd2l0Y2ggKG1lbWJlci5raW5kKSB7XG4gICAgICAgICAgY2FzZSB0cy5TeW50YXhLaW5kLlByb3BlcnR5RGVjbGFyYXRpb246XG4gICAgICAgICAgY2FzZSB0cy5TeW50YXhLaW5kLkdldEFjY2Vzc29yOlxuICAgICAgICAgIGNhc2UgdHMuU3ludGF4S2luZC5TZXRBY2Nlc3NvcjpcbiAgICAgICAgICBjYXNlIHRzLlN5bnRheEtpbmQuTWV0aG9kRGVjbGFyYXRpb246IHtcbiAgICAgICAgICAgIGNvbnN0IFtuYW1lLCBuZXdNZW1iZXIsIGRlY29yYXRvcnNdID0gdHJhbnNmb3JtQ2xhc3NFbGVtZW50KG1lbWJlcik7XG4gICAgICAgICAgICBuZXdNZW1iZXJzLnB1c2gobmV3TWVtYmVyKTtcbiAgICAgICAgICAgIGlmIChuYW1lKSBkZWNvcmF0ZWRQcm9wZXJ0aWVzLnNldChuYW1lLCBkZWNvcmF0b3JzKTtcbiAgICAgICAgICAgIGNvbnRpbnVlO1xuICAgICAgICAgIH1cbiAgICAgICAgICBjYXNlIHRzLlN5bnRheEtpbmQuQ29uc3RydWN0b3I6IHtcbiAgICAgICAgICAgIGNvbnN0IGN0b3IgPSBtZW1iZXIgYXMgdHMuQ29uc3RydWN0b3JEZWNsYXJhdGlvbjtcbiAgICAgICAgICAgIGlmICghY3Rvci5ib2R5KSBicmVhaztcbiAgICAgICAgICAgIGNvbnN0IFtuZXdNZW1iZXIsIHBhcmFtZXRlcnNJbmZvXSA9XG4gICAgICAgICAgICAgICAgdHJhbnNmb3JtQ29uc3RydWN0b3IobWVtYmVyIGFzIHRzLkNvbnN0cnVjdG9yRGVjbGFyYXRpb24pO1xuICAgICAgICAgICAgY2xhc3NQYXJhbWV0ZXJzID0gcGFyYW1ldGVyc0luZm87XG4gICAgICAgICAgICBuZXdNZW1iZXJzLnB1c2gobmV3TWVtYmVyKTtcbiAgICAgICAgICAgIGNvbnRpbnVlO1xuICAgICAgICAgIH1cbiAgICAgICAgICBkZWZhdWx0OlxuICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgIH1cbiAgICAgICAgbmV3TWVtYmVycy5wdXNoKHRzLnZpc2l0RWFjaENoaWxkKG1lbWJlciwgZGVjb3JhdG9yRG93bmxldmVsVmlzaXRvciwgY29udGV4dCkpO1xuICAgICAgfVxuICAgICAgY29uc3QgZGVjb3JhdG9ycyA9IGhvc3QuZ2V0RGVjb3JhdG9yc09mRGVjbGFyYXRpb24oY2xhc3NEZWNsKSB8fCBbXTtcblxuICAgICAgbGV0IGhhc0FuZ3VsYXJEZWNvcmF0b3IgPSBmYWxzZTtcbiAgICAgIGNvbnN0IGRlY29yYXRvcnNUb0xvd2VyID0gW107XG4gICAgICBjb25zdCBkZWNvcmF0b3JzVG9LZWVwOiB0cy5EZWNvcmF0b3JbXSA9IFtdO1xuICAgICAgZm9yIChjb25zdCBkZWNvcmF0b3Igb2YgZGVjb3JhdG9ycykge1xuICAgICAgICAvLyBXZSBvbmx5IGRlYWwgd2l0aCBjb25jcmV0ZSBub2RlcyBpbiBUeXBlU2NyaXB0IHNvdXJjZXMsIHNvIHdlIGRvbid0XG4gICAgICAgIC8vIG5lZWQgdG8gaGFuZGxlIHN5bnRoZXRpY2FsbHkgY3JlYXRlZCBkZWNvcmF0b3JzLlxuICAgICAgICBjb25zdCBkZWNvcmF0b3JOb2RlID0gZGVjb3JhdG9yLm5vZGUhIGFzIHRzLkRlY29yYXRvcjtcbiAgICAgICAgY29uc3QgaXNOZ0RlY29yYXRvciA9IGlzQW5ndWxhckRlY29yYXRvcihkZWNvcmF0b3IsIGlzQ29yZSk7XG5cbiAgICAgICAgLy8gS2VlcCB0cmFjayBpZiB3ZSBjb21lIGFjcm9zcyBhbiBBbmd1bGFyIGNsYXNzIGRlY29yYXRvci4gVGhpcyBpcyB1c2VkXG4gICAgICAgIC8vIGZvciB0byBkZXRlcm1pbmUgd2hldGhlciBjb25zdHJ1Y3RvciBwYXJhbWV0ZXJzIHNob3VsZCBiZSBjYXB0dXJlZCBvciBub3QuXG4gICAgICAgIGlmIChpc05nRGVjb3JhdG9yKSB7XG4gICAgICAgICAgaGFzQW5ndWxhckRlY29yYXRvciA9IHRydWU7XG4gICAgICAgIH1cblxuICAgICAgICBpZiAoaXNOZ0RlY29yYXRvciAmJiAhc2tpcENsYXNzRGVjb3JhdG9ycykge1xuICAgICAgICAgIGRlY29yYXRvcnNUb0xvd2VyLnB1c2goZXh0cmFjdE1ldGFkYXRhRnJvbVNpbmdsZURlY29yYXRvcihkZWNvcmF0b3JOb2RlLCBkaWFnbm9zdGljcykpO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgIGRlY29yYXRvcnNUb0tlZXAucHVzaChkZWNvcmF0b3JOb2RlKTtcbiAgICAgICAgfVxuICAgICAgfVxuXG4gICAgICBjb25zdCBuZXdDbGFzc0RlY2xhcmF0aW9uID0gdHMuZ2V0TXV0YWJsZUNsb25lKGNsYXNzRGVjbCk7XG5cbiAgICAgIGlmIChkZWNvcmF0b3JzVG9Mb3dlci5sZW5ndGgpIHtcbiAgICAgICAgbmV3TWVtYmVycy5wdXNoKGNyZWF0ZURlY29yYXRvckNsYXNzUHJvcGVydHkoZGVjb3JhdG9yc1RvTG93ZXIpKTtcbiAgICAgIH1cbiAgICAgIGlmIChjbGFzc1BhcmFtZXRlcnMpIHtcbiAgICAgICAgaWYgKGhhc0FuZ3VsYXJEZWNvcmF0b3IgfHwgY2xhc3NQYXJhbWV0ZXJzLnNvbWUocCA9PiAhIXAuZGVjb3JhdG9ycy5sZW5ndGgpKSB7XG4gICAgICAgICAgLy8gQ2FwdHVyZSBjb25zdHJ1Y3RvciBwYXJhbWV0ZXJzIGlmIHRoZSBjbGFzcyBoYXMgQW5ndWxhciBkZWNvcmF0b3IgYXBwbGllZCxcbiAgICAgICAgICAvLyBvciBpZiBhbnkgb2YgdGhlIHBhcmFtZXRlcnMgaGFzIGRlY29yYXRvcnMgYXBwbGllZCBkaXJlY3RseS5cbiAgICAgICAgICBuZXdNZW1iZXJzLnB1c2goY3JlYXRlQ3RvclBhcmFtZXRlcnNDbGFzc1Byb3BlcnR5KFxuICAgICAgICAgICAgICBkaWFnbm9zdGljcywgZW50aXR5TmFtZVRvRXhwcmVzc2lvbiwgY2xhc3NQYXJhbWV0ZXJzLCBpc0Nsb3N1cmVDb21waWxlckVuYWJsZWQpKTtcbiAgICAgICAgfVxuICAgICAgfVxuICAgICAgaWYgKGRlY29yYXRlZFByb3BlcnRpZXMuc2l6ZSkge1xuICAgICAgICBuZXdNZW1iZXJzLnB1c2goY3JlYXRlUHJvcERlY29yYXRvcnNDbGFzc1Byb3BlcnR5KGRpYWdub3N0aWNzLCBkZWNvcmF0ZWRQcm9wZXJ0aWVzKSk7XG4gICAgICB9XG4gICAgICBuZXdDbGFzc0RlY2xhcmF0aW9uLm1lbWJlcnMgPSB0cy5zZXRUZXh0UmFuZ2UoXG4gICAgICAgICAgdHMuY3JlYXRlTm9kZUFycmF5KG5ld01lbWJlcnMsIG5ld0NsYXNzRGVjbGFyYXRpb24ubWVtYmVycy5oYXNUcmFpbGluZ0NvbW1hKSxcbiAgICAgICAgICBjbGFzc0RlY2wubWVtYmVycyk7XG4gICAgICBuZXdDbGFzc0RlY2xhcmF0aW9uLmRlY29yYXRvcnMgPVxuICAgICAgICAgIGRlY29yYXRvcnNUb0tlZXAubGVuZ3RoID8gdHMuY3JlYXRlTm9kZUFycmF5KGRlY29yYXRvcnNUb0tlZXApIDogdW5kZWZpbmVkO1xuICAgICAgcmV0dXJuIG5ld0NsYXNzRGVjbGFyYXRpb247XG4gICAgfVxuXG4gICAgLyoqXG4gICAgICogVHJhbnNmb3JtZXIgdmlzaXRvciB0aGF0IGxvb2tzIGZvciBBbmd1bGFyIGRlY29yYXRvcnMgYW5kIHJlcGxhY2VzIHRoZW0gd2l0aFxuICAgICAqIGRvd25sZXZlbGVkIHN0YXRpYyBwcm9wZXJ0aWVzLiBBbHNvIGNvbGxlY3RzIGNvbnN0cnVjdG9yIHR5cGUgbWV0YWRhdGEgZm9yXG4gICAgICogY2xhc3MgZGVjbGFyYXRpb24gdGhhdCBhcmUgZGVjb3JhdGVkIHdpdGggYW4gQW5ndWxhciBkZWNvcmF0b3IuXG4gICAgICovXG4gICAgZnVuY3Rpb24gZGVjb3JhdG9yRG93bmxldmVsVmlzaXRvcihub2RlOiB0cy5Ob2RlKTogdHMuTm9kZSB7XG4gICAgICBpZiAodHMuaXNDbGFzc0RlY2xhcmF0aW9uKG5vZGUpKSB7XG4gICAgICAgIHJldHVybiB0cmFuc2Zvcm1DbGFzc0RlY2xhcmF0aW9uKG5vZGUpO1xuICAgICAgfVxuICAgICAgcmV0dXJuIHRzLnZpc2l0RWFjaENoaWxkKG5vZGUsIGRlY29yYXRvckRvd25sZXZlbFZpc2l0b3IsIGNvbnRleHQpO1xuICAgIH1cblxuICAgIHJldHVybiAoc2Y6IHRzLlNvdXJjZUZpbGUpID0+IHtcbiAgICAgIC8vIEVuc3VyZSB0aGF0IHJlZmVyZW5jZWQgdHlwZSBzeW1ib2xzIGFyZSBub3QgZWxpZGVkIGJ5IFR5cGVTY3JpcHQuIEltcG9ydHMgZm9yXG4gICAgICAvLyBzdWNoIHBhcmFtZXRlciB0eXBlIHN5bWJvbHMgcHJldmlvdXNseSBjb3VsZCBiZSB0eXBlLW9ubHksIGJ1dCBub3cgbWlnaHQgYmUgYWxzb1xuICAgICAgLy8gdXNlZCBpbiB0aGUgYGN0b3JQYXJhbWV0ZXJzYCBzdGF0aWMgcHJvcGVydHkgYXMgYSB2YWx1ZS4gV2Ugd2FudCB0byBtYWtlIHN1cmVcbiAgICAgIC8vIHRoYXQgVHlwZVNjcmlwdCBkb2VzIG5vdCBlbGlkZSBpbXBvcnRzIGZvciBzdWNoIHR5cGUgcmVmZXJlbmNlcy4gUmVhZCBtb3JlXG4gICAgICAvLyBhYm91dCB0aGlzIGluIHRoZSBkZXNjcmlwdGlvbiBmb3IgYHBhdGNoQWxpYXNSZWZlcmVuY2VSZXNvbHV0aW9uYC5cbiAgICAgIHBhdGNoQWxpYXNSZWZlcmVuY2VSZXNvbHV0aW9uT3JEaWUoY29udGV4dCwgcmVmZXJlbmNlZFBhcmFtZXRlclR5cGVzKTtcbiAgICAgIC8vIERvd25sZXZlbCBkZWNvcmF0b3JzIGFuZCBjb25zdHJ1Y3RvciBwYXJhbWV0ZXIgdHlwZXMuIFdlIHdpbGwga2VlcCB0cmFjayBvZiBhbGxcbiAgICAgIC8vIHJlZmVyZW5jZWQgY29uc3RydWN0b3IgcGFyYW1ldGVyIHR5cGVzIHNvIHRoYXQgd2UgY2FuIGluc3RydWN0IFR5cGVTY3JpcHQgdG9cbiAgICAgIC8vIG5vdCBlbGlkZSB0aGVpciBpbXBvcnRzIGlmIHRoZXkgcHJldmlvdXNseSB3ZXJlIG9ubHkgdHlwZS1vbmx5LlxuICAgICAgcmV0dXJuIHRzLnZpc2l0RWFjaENoaWxkKHNmLCBkZWNvcmF0b3JEb3dubGV2ZWxWaXNpdG9yLCBjb250ZXh0KTtcbiAgICB9O1xuICB9O1xufVxuIl19