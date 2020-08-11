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
        define("@angular/compiler-cli/src/ngtsc/annotations/src/util", ["require", "exports", "tslib", "@angular/compiler", "typescript", "@angular/compiler-cli/src/ngtsc/diagnostics", "@angular/compiler-cli/src/ngtsc/imports", "@angular/compiler-cli/src/ngtsc/reflection"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.createSourceSpan = exports.wrapTypeReference = exports.resolveProvidersRequiringFactory = exports.makeDuplicateDeclarationError = exports.wrapFunctionExpressionsInParens = exports.readBaseClass = exports.isWrappedTsNodeExpr = exports.isExpressionForwardReference = exports.combineResolvers = exports.forwardRefResolver = exports.unwrapForwardRef = exports.unwrapExpression = exports.isAngularDecorator = exports.findAngularDecorator = exports.isAngularCoreReference = exports.isAngularCore = exports.toR3Reference = exports.validateConstructorDependencies = exports.getValidConstructorDependencies = exports.unwrapConstructorDependencies = exports.valueReferenceToExpression = exports.getConstructorDependencies = exports.ConstructorDepErrorKind = void 0;
    var tslib_1 = require("tslib");
    var compiler_1 = require("@angular/compiler");
    var ts = require("typescript");
    var diagnostics_1 = require("@angular/compiler-cli/src/ngtsc/diagnostics");
    var imports_1 = require("@angular/compiler-cli/src/ngtsc/imports");
    var reflection_1 = require("@angular/compiler-cli/src/ngtsc/reflection");
    var ConstructorDepErrorKind;
    (function (ConstructorDepErrorKind) {
        ConstructorDepErrorKind[ConstructorDepErrorKind["NO_SUITABLE_TOKEN"] = 0] = "NO_SUITABLE_TOKEN";
    })(ConstructorDepErrorKind = exports.ConstructorDepErrorKind || (exports.ConstructorDepErrorKind = {}));
    function getConstructorDependencies(clazz, reflector, defaultImportRecorder, isCore) {
        var deps = [];
        var errors = [];
        var ctorParams = reflector.getConstructorParameters(clazz);
        if (ctorParams === null) {
            if (reflector.hasBaseClass(clazz)) {
                return null;
            }
            else {
                ctorParams = [];
            }
        }
        ctorParams.forEach(function (param, idx) {
            var token = valueReferenceToExpression(param.typeValueReference, defaultImportRecorder);
            var attribute = null;
            var optional = false, self = false, skipSelf = false, host = false;
            var resolved = compiler_1.R3ResolvedDependencyType.Token;
            (param.decorators || []).filter(function (dec) { return isCore || isAngularCore(dec); }).forEach(function (dec) {
                var name = isCore || dec.import === null ? dec.name : dec.import.name;
                if (name === 'Inject') {
                    if (dec.args === null || dec.args.length !== 1) {
                        throw new diagnostics_1.FatalDiagnosticError(diagnostics_1.ErrorCode.DECORATOR_ARITY_WRONG, reflection_1.Decorator.nodeForError(dec), "Unexpected number of arguments to @Inject().");
                    }
                    token = new compiler_1.WrappedNodeExpr(dec.args[0]);
                }
                else if (name === 'Optional') {
                    optional = true;
                }
                else if (name === 'SkipSelf') {
                    skipSelf = true;
                }
                else if (name === 'Self') {
                    self = true;
                }
                else if (name === 'Host') {
                    host = true;
                }
                else if (name === 'Attribute') {
                    if (dec.args === null || dec.args.length !== 1) {
                        throw new diagnostics_1.FatalDiagnosticError(diagnostics_1.ErrorCode.DECORATOR_ARITY_WRONG, reflection_1.Decorator.nodeForError(dec), "Unexpected number of arguments to @Attribute().");
                    }
                    var attributeName = dec.args[0];
                    token = new compiler_1.WrappedNodeExpr(attributeName);
                    if (ts.isStringLiteralLike(attributeName)) {
                        attribute = new compiler_1.LiteralExpr(attributeName.text);
                    }
                    else {
                        attribute = new compiler_1.WrappedNodeExpr(ts.createKeywordTypeNode(ts.SyntaxKind.UnknownKeyword));
                    }
                    resolved = compiler_1.R3ResolvedDependencyType.Attribute;
                }
                else {
                    throw new diagnostics_1.FatalDiagnosticError(diagnostics_1.ErrorCode.DECORATOR_UNEXPECTED, reflection_1.Decorator.nodeForError(dec), "Unexpected decorator " + name + " on parameter.");
                }
            });
            if (token instanceof compiler_1.ExternalExpr && token.value.name === 'ChangeDetectorRef' &&
                token.value.moduleName === '@angular/core') {
                resolved = compiler_1.R3ResolvedDependencyType.ChangeDetectorRef;
            }
            if (token === null) {
                errors.push({
                    index: idx,
                    kind: ConstructorDepErrorKind.NO_SUITABLE_TOKEN,
                    param: param,
                });
            }
            else {
                deps.push({ token: token, attribute: attribute, optional: optional, self: self, skipSelf: skipSelf, host: host, resolved: resolved });
            }
        });
        if (errors.length === 0) {
            return { deps: deps };
        }
        else {
            return { deps: null, errors: errors };
        }
    }
    exports.getConstructorDependencies = getConstructorDependencies;
    function valueReferenceToExpression(valueRef, defaultImportRecorder) {
        var e_1, _a;
        if (valueRef === null) {
            return null;
        }
        else if (valueRef.local) {
            if (defaultImportRecorder !== null && valueRef.defaultImportStatement !== null &&
                ts.isIdentifier(valueRef.expression)) {
                defaultImportRecorder.recordImportedIdentifier(valueRef.expression, valueRef.defaultImportStatement);
            }
            return new compiler_1.WrappedNodeExpr(valueRef.expression);
        }
        else {
            // TODO(alxhub): this cast is necessary because the g3 typescript version doesn't narrow here.
            var ref = valueRef;
            var importExpr = new compiler_1.ExternalExpr({ moduleName: ref.moduleName, name: ref.importedName });
            if (ref.nestedPath !== null) {
                try {
                    for (var _b = tslib_1.__values(ref.nestedPath), _c = _b.next(); !_c.done; _c = _b.next()) {
                        var property = _c.value;
                        importExpr = new compiler_1.ReadPropExpr(importExpr, property);
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
            return importExpr;
        }
    }
    exports.valueReferenceToExpression = valueReferenceToExpression;
    /**
     * Convert `ConstructorDeps` into the `R3DependencyMetadata` array for those deps if they're valid,
     * or into an `'invalid'` signal if they're not.
     *
     * This is a companion function to `validateConstructorDependencies` which accepts invalid deps.
     */
    function unwrapConstructorDependencies(deps) {
        if (deps === null) {
            return null;
        }
        else if (deps.deps !== null) {
            // These constructor dependencies are valid.
            return deps.deps;
        }
        else {
            // These deps are invalid.
            return 'invalid';
        }
    }
    exports.unwrapConstructorDependencies = unwrapConstructorDependencies;
    function getValidConstructorDependencies(clazz, reflector, defaultImportRecorder, isCore) {
        return validateConstructorDependencies(clazz, getConstructorDependencies(clazz, reflector, defaultImportRecorder, isCore));
    }
    exports.getValidConstructorDependencies = getValidConstructorDependencies;
    /**
     * Validate that `ConstructorDeps` does not have any invalid dependencies and convert them into the
     * `R3DependencyMetadata` array if so, or raise a diagnostic if some deps are invalid.
     *
     * This is a companion function to `unwrapConstructorDependencies` which does not accept invalid
     * deps.
     */
    function validateConstructorDependencies(clazz, deps) {
        if (deps === null) {
            return null;
        }
        else if (deps.deps !== null) {
            return deps.deps;
        }
        else {
            // TODO(alxhub): this cast is necessary because the g3 typescript version doesn't narrow here.
            var _a = deps.errors[0], param = _a.param, index = _a.index;
            // There is at least one error.
            throw new diagnostics_1.FatalDiagnosticError(diagnostics_1.ErrorCode.PARAM_MISSING_TOKEN, param.nameNode, "No suitable injection token for parameter '" + (param.name || index) + "' of class '" + clazz.name.text + "'.\n" +
                (param.typeNode !== null ? "Found " + param.typeNode.getText() :
                    'no type or decorator'));
        }
    }
    exports.validateConstructorDependencies = validateConstructorDependencies;
    function toR3Reference(valueRef, typeRef, valueContext, typeContext, refEmitter) {
        var value = refEmitter.emit(valueRef, valueContext);
        var type = refEmitter.emit(typeRef, typeContext, imports_1.ImportFlags.ForceNewImport | imports_1.ImportFlags.AllowTypeImports);
        if (value === null || type === null) {
            throw new Error("Could not refer to " + ts.SyntaxKind[valueRef.node.kind]);
        }
        return { value: value, type: type };
    }
    exports.toR3Reference = toR3Reference;
    function isAngularCore(decorator) {
        return decorator.import !== null && decorator.import.from === '@angular/core';
    }
    exports.isAngularCore = isAngularCore;
    function isAngularCoreReference(reference, symbolName) {
        return reference.ownedByModuleGuess === '@angular/core' && reference.debugName === symbolName;
    }
    exports.isAngularCoreReference = isAngularCoreReference;
    function findAngularDecorator(decorators, name, isCore) {
        return decorators.find(function (decorator) { return isAngularDecorator(decorator, name, isCore); });
    }
    exports.findAngularDecorator = findAngularDecorator;
    function isAngularDecorator(decorator, name, isCore) {
        if (isCore) {
            return decorator.name === name;
        }
        else if (isAngularCore(decorator)) {
            return decorator.import.name === name;
        }
        return false;
    }
    exports.isAngularDecorator = isAngularDecorator;
    /**
     * Unwrap a `ts.Expression`, removing outer type-casts or parentheses until the expression is in its
     * lowest level form.
     *
     * For example, the expression "(foo as Type)" unwraps to "foo".
     */
    function unwrapExpression(node) {
        while (ts.isAsExpression(node) || ts.isParenthesizedExpression(node)) {
            node = node.expression;
        }
        return node;
    }
    exports.unwrapExpression = unwrapExpression;
    function expandForwardRef(arg) {
        arg = unwrapExpression(arg);
        if (!ts.isArrowFunction(arg) && !ts.isFunctionExpression(arg)) {
            return null;
        }
        var body = arg.body;
        // Either the body is a ts.Expression directly, or a block with a single return statement.
        if (ts.isBlock(body)) {
            // Block body - look for a single return statement.
            if (body.statements.length !== 1) {
                return null;
            }
            var stmt = body.statements[0];
            if (!ts.isReturnStatement(stmt) || stmt.expression === undefined) {
                return null;
            }
            return stmt.expression;
        }
        else {
            // Shorthand body - return as an expression.
            return body;
        }
    }
    /**
     * Possibly resolve a forwardRef() expression into the inner value.
     *
     * @param node the forwardRef() expression to resolve
     * @param reflector a ReflectionHost
     * @returns the resolved expression, if the original expression was a forwardRef(), or the original
     * expression otherwise
     */
    function unwrapForwardRef(node, reflector) {
        node = unwrapExpression(node);
        if (!ts.isCallExpression(node) || node.arguments.length !== 1) {
            return node;
        }
        var fn = ts.isPropertyAccessExpression(node.expression) ? node.expression.name : node.expression;
        if (!ts.isIdentifier(fn)) {
            return node;
        }
        var expr = expandForwardRef(node.arguments[0]);
        if (expr === null) {
            return node;
        }
        var imp = reflector.getImportOfIdentifier(fn);
        if (imp === null || imp.from !== '@angular/core' || imp.name !== 'forwardRef') {
            return node;
        }
        else {
            return expr;
        }
    }
    exports.unwrapForwardRef = unwrapForwardRef;
    /**
     * A foreign function resolver for `staticallyResolve` which unwraps forwardRef() expressions.
     *
     * @param ref a Reference to the declaration of the function being called (which might be
     * forwardRef)
     * @param args the arguments to the invocation of the forwardRef expression
     * @returns an unwrapped argument if `ref` pointed to forwardRef, or null otherwise
     */
    function forwardRefResolver(ref, args) {
        if (!isAngularCoreReference(ref, 'forwardRef') || args.length !== 1) {
            return null;
        }
        return expandForwardRef(args[0]);
    }
    exports.forwardRefResolver = forwardRefResolver;
    /**
     * Combines an array of resolver functions into a one.
     * @param resolvers Resolvers to be combined.
     */
    function combineResolvers(resolvers) {
        return function (ref, args) {
            var e_2, _a;
            try {
                for (var resolvers_1 = tslib_1.__values(resolvers), resolvers_1_1 = resolvers_1.next(); !resolvers_1_1.done; resolvers_1_1 = resolvers_1.next()) {
                    var resolver = resolvers_1_1.value;
                    var resolved = resolver(ref, args);
                    if (resolved !== null) {
                        return resolved;
                    }
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (resolvers_1_1 && !resolvers_1_1.done && (_a = resolvers_1.return)) _a.call(resolvers_1);
                }
                finally { if (e_2) throw e_2.error; }
            }
            return null;
        };
    }
    exports.combineResolvers = combineResolvers;
    function isExpressionForwardReference(expr, context, contextSource) {
        if (isWrappedTsNodeExpr(expr)) {
            var node = ts.getOriginalNode(expr.node);
            return node.getSourceFile() === contextSource && context.pos < node.pos;
        }
        else {
            return false;
        }
    }
    exports.isExpressionForwardReference = isExpressionForwardReference;
    function isWrappedTsNodeExpr(expr) {
        return expr instanceof compiler_1.WrappedNodeExpr;
    }
    exports.isWrappedTsNodeExpr = isWrappedTsNodeExpr;
    function readBaseClass(node, reflector, evaluator) {
        var baseExpression = reflector.getBaseClassExpression(node);
        if (baseExpression !== null) {
            var baseClass = evaluator.evaluate(baseExpression);
            if (baseClass instanceof imports_1.Reference && reflector.isClass(baseClass.node)) {
                return baseClass;
            }
            else {
                return 'dynamic';
            }
        }
        return null;
    }
    exports.readBaseClass = readBaseClass;
    var parensWrapperTransformerFactory = function (context) {
        var visitor = function (node) {
            var visited = ts.visitEachChild(node, visitor, context);
            if (ts.isArrowFunction(visited) || ts.isFunctionExpression(visited)) {
                return ts.createParen(visited);
            }
            return visited;
        };
        return function (node) { return ts.visitEachChild(node, visitor, context); };
    };
    /**
     * Wraps all functions in a given expression in parentheses. This is needed to avoid problems
     * where Tsickle annotations added between analyse and transform phases in Angular may trigger
     * automatic semicolon insertion, e.g. if a function is the expression in a `return` statement.
     * More
     * info can be found in Tsickle source code here:
     * https://github.com/angular/tsickle/blob/d7974262571c8a17d684e5ba07680e1b1993afdd/src/jsdoc_transformer.ts#L1021
     *
     * @param expression Expression where functions should be wrapped in parentheses
     */
    function wrapFunctionExpressionsInParens(expression) {
        return ts.transform(expression, [parensWrapperTransformerFactory]).transformed[0];
    }
    exports.wrapFunctionExpressionsInParens = wrapFunctionExpressionsInParens;
    /**
     * Create a `ts.Diagnostic` which indicates the given class is part of the declarations of two or
     * more NgModules.
     *
     * The resulting `ts.Diagnostic` will have a context entry for each NgModule showing the point where
     * the directive/pipe exists in its `declarations` (if possible).
     */
    function makeDuplicateDeclarationError(node, data, kind) {
        var e_3, _a;
        var context = [];
        try {
            for (var data_1 = tslib_1.__values(data), data_1_1 = data_1.next(); !data_1_1.done; data_1_1 = data_1.next()) {
                var decl = data_1_1.value;
                if (decl.rawDeclarations === null) {
                    continue;
                }
                // Try to find the reference to the declaration within the declarations array, to hang the
                // error there. If it can't be found, fall back on using the NgModule's name.
                var contextNode = decl.ref.getOriginForDiagnostics(decl.rawDeclarations, decl.ngModule.name);
                context.push({
                    node: contextNode,
                    messageText: "'" + node.name.text + "' is listed in the declarations of the NgModule '" + decl.ngModule.name.text + "'.",
                });
            }
        }
        catch (e_3_1) { e_3 = { error: e_3_1 }; }
        finally {
            try {
                if (data_1_1 && !data_1_1.done && (_a = data_1.return)) _a.call(data_1);
            }
            finally { if (e_3) throw e_3.error; }
        }
        // Finally, produce the diagnostic.
        return diagnostics_1.makeDiagnostic(diagnostics_1.ErrorCode.NGMODULE_DECLARATION_NOT_UNIQUE, node.name, "The " + kind + " '" + node.name.text + "' is declared by more than one NgModule.", context);
    }
    exports.makeDuplicateDeclarationError = makeDuplicateDeclarationError;
    /**
     * Resolves the given `rawProviders` into `ClassDeclarations` and returns
     * a set containing those that are known to require a factory definition.
     * @param rawProviders Expression that declared the providers array in the source.
     */
    function resolveProvidersRequiringFactory(rawProviders, reflector, evaluator) {
        var providers = new Set();
        var resolvedProviders = evaluator.evaluate(rawProviders);
        if (!Array.isArray(resolvedProviders)) {
            return providers;
        }
        resolvedProviders.forEach(function processProviders(provider) {
            var tokenClass = null;
            if (Array.isArray(provider)) {
                // If we ran into an array, recurse into it until we've resolve all the classes.
                provider.forEach(processProviders);
            }
            else if (provider instanceof imports_1.Reference) {
                tokenClass = provider;
            }
            else if (provider instanceof Map && provider.has('useClass') && !provider.has('deps')) {
                var useExisting = provider.get('useClass');
                if (useExisting instanceof imports_1.Reference) {
                    tokenClass = useExisting;
                }
            }
            if (tokenClass !== null && reflector.isClass(tokenClass.node)) {
                var constructorParameters = reflector.getConstructorParameters(tokenClass.node);
                // Note that we only want to capture providers with a non-trivial constructor,
                // because they're the ones that might be using DI and need to be decorated.
                if (constructorParameters !== null && constructorParameters.length > 0) {
                    providers.add(tokenClass);
                }
            }
        });
        return providers;
    }
    exports.resolveProvidersRequiringFactory = resolveProvidersRequiringFactory;
    /**
     * Create an R3Reference for a class.
     *
     * The `value` is the exported declaration of the class from its source file.
     * The `type` is an expression that would be used by ngcc in the typings (.d.ts) files.
     */
    function wrapTypeReference(reflector, clazz) {
        var dtsClass = reflector.getDtsDeclaration(clazz);
        var value = new compiler_1.WrappedNodeExpr(clazz.name);
        var type = dtsClass !== null && reflection_1.isNamedClassDeclaration(dtsClass) ?
            new compiler_1.WrappedNodeExpr(dtsClass.name) :
            value;
        return { value: value, type: type };
    }
    exports.wrapTypeReference = wrapTypeReference;
    /** Creates a ParseSourceSpan for a TypeScript node. */
    function createSourceSpan(node) {
        var sf = node.getSourceFile();
        var _a = tslib_1.__read([node.getStart(), node.getEnd()], 2), startOffset = _a[0], endOffset = _a[1];
        var _b = sf.getLineAndCharacterOfPosition(startOffset), startLine = _b.line, startCol = _b.character;
        var _c = sf.getLineAndCharacterOfPosition(endOffset), endLine = _c.line, endCol = _c.character;
        var parseSf = new compiler_1.ParseSourceFile(sf.getFullText(), sf.fileName);
        // +1 because values are zero-indexed.
        return new compiler_1.ParseSourceSpan(new compiler_1.ParseLocation(parseSf, startOffset, startLine + 1, startCol + 1), new compiler_1.ParseLocation(parseSf, endOffset, endLine + 1, endCol + 1));
    }
    exports.createSourceSpan = createSourceSpan;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidXRpbC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2NvbXBpbGVyLWNsaS9zcmMvbmd0c2MvYW5ub3RhdGlvbnMvc3JjL3V0aWwudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOzs7Ozs7Ozs7Ozs7OztJQUVILDhDQUFxTjtJQUNyTiwrQkFBaUM7SUFFakMsMkVBQWtGO0lBQ2xGLG1FQUE4RjtJQUU5Rix5RUFBaUo7SUFHakosSUFBWSx1QkFFWDtJQUZELFdBQVksdUJBQXVCO1FBQ2pDLCtGQUFpQixDQUFBO0lBQ25CLENBQUMsRUFGVyx1QkFBdUIsR0FBdkIsK0JBQXVCLEtBQXZCLCtCQUF1QixRQUVsQztJQWVELFNBQWdCLDBCQUEwQixDQUN0QyxLQUF1QixFQUFFLFNBQXlCLEVBQ2xELHFCQUE0QyxFQUFFLE1BQWU7UUFDL0QsSUFBTSxJQUFJLEdBQTJCLEVBQUUsQ0FBQztRQUN4QyxJQUFNLE1BQU0sR0FBMEIsRUFBRSxDQUFDO1FBQ3pDLElBQUksVUFBVSxHQUFHLFNBQVMsQ0FBQyx3QkFBd0IsQ0FBQyxLQUFLLENBQUMsQ0FBQztRQUMzRCxJQUFJLFVBQVUsS0FBSyxJQUFJLEVBQUU7WUFDdkIsSUFBSSxTQUFTLENBQUMsWUFBWSxDQUFDLEtBQUssQ0FBQyxFQUFFO2dCQUNqQyxPQUFPLElBQUksQ0FBQzthQUNiO2lCQUFNO2dCQUNMLFVBQVUsR0FBRyxFQUFFLENBQUM7YUFDakI7U0FDRjtRQUNELFVBQVUsQ0FBQyxPQUFPLENBQUMsVUFBQyxLQUFLLEVBQUUsR0FBRztZQUM1QixJQUFJLEtBQUssR0FBRywwQkFBMEIsQ0FBQyxLQUFLLENBQUMsa0JBQWtCLEVBQUUscUJBQXFCLENBQUMsQ0FBQztZQUN4RixJQUFJLFNBQVMsR0FBb0IsSUFBSSxDQUFDO1lBQ3RDLElBQUksUUFBUSxHQUFHLEtBQUssRUFBRSxJQUFJLEdBQUcsS0FBSyxFQUFFLFFB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Q0FBQyxPQUFPLEVBQUUsU0FBUyxFQUFFLE9BQU8sR0FBRyxDQUFDLEVBQUUsTUFBTSxHQUFHLENBQUMsQ0FBQyxDQUFDLENBQUM7SUFDdEUsQ0FBQztJQVhELDRDQVdDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7RXhwcmVzc2lvbiwgRXh0ZXJuYWxFeHByLCBMaXRlcmFsRXhwciwgUGFyc2VMb2NhdGlvbiwgUGFyc2VTb3VyY2VGaWxlLCBQYXJzZVNvdXJjZVNwYW4sIFIzRGVwZW5kZW5jeU1ldGFkYXRhLCBSM1JlZmVyZW5jZSwgUjNSZXNvbHZlZERlcGVuZGVuY3lUeXBlLCBSZWFkUHJvcEV4cHIsIFdyYXBwZWROb2RlRXhwcn0gZnJvbSAnQGFuZ3VsYXIvY29tcGlsZXInO1xuaW1wb3J0ICogYXMgdHMgZnJvbSAndHlwZXNjcmlwdCc7XG5cbmltcG9ydCB7RXJyb3JDb2RlLCBGYXRhbERpYWdub3N0aWNFcnJvciwgbWFrZURpYWdub3N0aWN9IGZyb20gJy4uLy4uL2RpYWdub3N0aWNzJztcbmltcG9ydCB7RGVmYXVsdEltcG9ydFJlY29yZGVyLCBJbXBvcnRGbGFncywgUmVmZXJlbmNlLCBSZWZlcmVuY2VFbWl0dGVyfSBmcm9tICcuLi8uLi9pbXBvcnRzJztcbmltcG9ydCB7Rm9yZWlnbkZ1bmN0aW9uUmVzb2x2ZXIsIFBhcnRpYWxFdmFsdWF0b3J9IGZyb20gJy4uLy4uL3BhcnRpYWxfZXZhbHVhdG9yJztcbmltcG9ydCB7Q2xhc3NEZWNsYXJhdGlvbiwgQ3RvclBhcmFtZXRlciwgRGVjb3JhdG9yLCBJbXBvcnQsIGlzTmFtZWRDbGFzc0RlY2xhcmF0aW9uLCBSZWZsZWN0aW9uSG9zdCwgVHlwZVZhbHVlUmVmZXJlbmNlfSBmcm9tICcuLi8uLi9yZWZsZWN0aW9uJztcbmltcG9ydCB7RGVjbGFyYXRpb25EYXRhfSBmcm9tICcuLi8uLi9zY29wZSc7XG5cbmV4cG9ydCBlbnVtIENvbnN0cnVjdG9yRGVwRXJyb3JLaW5kIHtcbiAgTk9fU1VJVEFCTEVfVE9LRU4sXG59XG5cbmV4cG9ydCB0eXBlIENvbnN0cnVjdG9yRGVwcyA9IHtcbiAgZGVwczogUjNEZXBlbmRlbmN5TWV0YWRhdGFbXTtcbn18e1xuICBkZXBzOiBudWxsO1xuICBlcnJvcnM6IENvbnN0cnVjdG9yRGVwRXJyb3JbXTtcbn07XG5cbmV4cG9ydCBpbnRlcmZhY2UgQ29uc3RydWN0b3JEZXBFcnJvciB7XG4gIGluZGV4OiBudW1iZXI7XG4gIHBhcmFtOiBDdG9yUGFyYW1ldGVyO1xuICBraW5kOiBDb25zdHJ1Y3RvckRlcEVycm9yS2luZDtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGdldENvbnN0cnVjdG9yRGVwZW5kZW5jaWVzKFxuICAgIGNsYXp6OiBDbGFzc0RlY2xhcmF0aW9uLCByZWZsZWN0b3I6IFJlZmxlY3Rpb25Ib3N0LFxuICAgIGRlZmF1bHRJbXBvcnRSZWNvcmRlcjogRGVmYXVsdEltcG9ydFJlY29yZGVyLCBpc0NvcmU6IGJvb2xlYW4pOiBDb25zdHJ1Y3RvckRlcHN8bnVsbCB7XG4gIGNvbnN0IGRlcHM6IFIzRGVwZW5kZW5jeU1ldGFkYXRhW10gPSBbXTtcbiAgY29uc3QgZXJyb3JzOiBDb25zdHJ1Y3RvckRlcEVycm9yW10gPSBbXTtcbiAgbGV0IGN0b3JQYXJhbXMgPSByZWZsZWN0b3IuZ2V0Q29uc3RydWN0b3JQYXJhbWV0ZXJzKGNsYXp6KTtcbiAgaWYgKGN0b3JQYXJhbXMgPT09IG51bGwpIHtcbiAgICBpZiAocmVmbGVjdG9yLmhhc0Jhc2VDbGFzcyhjbGF6eikpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH0gZWxzZSB7XG4gICAgICBjdG9yUGFyYW1zID0gW107XG4gICAgfVxuICB9XG4gIGN0b3JQYXJhbXMuZm9yRWFjaCgocGFyYW0sIGlkeCkgPT4ge1xuICAgIGxldCB0b2tlbiA9IHZhbHVlUmVmZXJlbmNlVG9FeHByZXNzaW9uKHBhcmFtLnR5cGVWYWx1ZVJlZmVyZW5jZSwgZGVmYXVsdEltcG9ydFJlY29yZGVyKTtcbiAgICBsZXQgYXR0cmlidXRlOiBFeHByZXNzaW9ufG51bGwgPSBudWxsO1xuICAgIGxldCBvcHRpb25hbCA9IGZhbHNlLCBzZWxmID0gZmFsc2UsIHNraXBTZWxmID0gZmFsc2UsIGhvc3QgPSBmYWxzZTtcbiAgICBsZXQgcmVzb2x2ZWQgPSBSM1Jlc29sdmVkRGVwZW5kZW5jeVR5cGUuVG9rZW47XG5cbiAgICAocGFyYW0uZGVjb3JhdG9ycyB8fCBbXSkuZmlsdGVyKGRlYyA9PiBpc0NvcmUgfHwgaXNBbmd1bGFyQ29yZShkZWMpKS5mb3JFYWNoKGRlYyA9PiB7XG4gICAgICBjb25zdCBuYW1lID0gaXNDb3JlIHx8IGRlYy5pbXBvcnQgPT09IG51bGwgPyBkZWMubmFtZSA6IGRlYy5pbXBvcnQhLm5hbWU7XG4gICAgICBpZiAobmFtZSA9PT0gJ0luamVjdCcpIHtcbiAgICAgICAgaWYgKGRlYy5hcmdzID09PSBudWxsIHx8IGRlYy5hcmdzLmxlbmd0aCAhPT0gMSkge1xuICAgICAgICAgIHRocm93IG5ldyBGYXRhbERpYWdub3N0aWNFcnJvcihcbiAgICAgICAgICAgICAgRXJyb3JDb2RlLkRFQ09SQVRPUl9BUklUWV9XUk9ORywgRGVjb3JhdG9yLm5vZGVGb3JFcnJvcihkZWMpLFxuICAgICAgICAgICAgICBgVW5leHBlY3RlZCBudW1iZXIgb2YgYXJndW1lbnRzIHRvIEBJbmplY3QoKS5gKTtcbiAgICAgICAgfVxuICAgICAgICB0b2tlbiA9IG5ldyBXcmFwcGVkTm9kZUV4cHIoZGVjLmFyZ3NbMF0pO1xuICAgICAgfSBlbHNlIGlmIChuYW1lID09PSAnT3B0aW9uYWwnKSB7XG4gICAgICAgIG9wdGlvbmFsID0gdHJ1ZTtcbiAgICAgIH0gZWxzZSBpZiAobmFtZSA9PT0gJ1NraXBTZWxmJykge1xuICAgICAgICBza2lwU2VsZiA9IHRydWU7XG4gICAgICB9IGVsc2UgaWYgKG5hbWUgPT09ICdTZWxmJykge1xuICAgICAgICBzZWxmID0gdHJ1ZTtcbiAgICAgIH0gZWxzZSBpZiAobmFtZSA9PT0gJ0hvc3QnKSB7XG4gICAgICAgIGhvc3QgPSB0cnVlO1xuICAgICAgfSBlbHNlIGlmIChuYW1lID09PSAnQXR0cmlidXRlJykge1xuICAgICAgICBpZiAoZGVjLmFyZ3MgPT09IG51bGwgfHwgZGVjLmFyZ3MubGVuZ3RoICE9PSAxKSB7XG4gICAgICAgICAgdGhyb3cgbmV3IEZhdGFsRGlhZ25vc3RpY0Vycm9yKFxuICAgICAgICAgICAgICBFcnJvckNvZGUuREVDT1JBVE9SX0FSSVRZX1dST05HLCBEZWNvcmF0b3Iubm9kZUZvckVycm9yKGRlYyksXG4gICAgICAgICAgICAgIGBVbmV4cGVjdGVkIG51bWJlciBvZiBhcmd1bWVudHMgdG8gQEF0dHJpYnV0ZSgpLmApO1xuICAgICAgICB9XG4gICAgICAgIGNvbnN0IGF0dHJpYnV0ZU5hbWUgPSBkZWMuYXJnc1swXTtcbiAgICAgICAgdG9rZW4gPSBuZXcgV3JhcHBlZE5vZGVFeHByKGF0dHJpYnV0ZU5hbWUpO1xuICAgICAgICBpZiAodHMuaXNTdHJpbmdMaXRlcmFsTGlrZShhdHRyaWJ1dGVOYW1lKSkge1xuICAgICAgICAgIGF0dHJpYnV0ZSA9IG5ldyBMaXRlcmFsRXhwcihhdHRyaWJ1dGVOYW1lLnRleHQpO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgIGF0dHJpYnV0ZSA9IG5ldyBXcmFwcGVkTm9kZUV4cHIodHMuY3JlYXRlS2V5d29yZFR5cGVOb2RlKHRzLlN5bnRheEtpbmQuVW5rbm93bktleXdvcmQpKTtcbiAgICAgICAgfVxuICAgICAgICByZXNvbHZlZCA9IFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZS5BdHRyaWJ1dGU7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICB0aHJvdyBuZXcgRmF0YWxEaWFnbm9zdGljRXJyb3IoXG4gICAgICAgICAgICBFcnJvckNvZGUuREVDT1JBVE9SX1VORVhQRUNURUQsIERlY29yYXRvci5ub2RlRm9yRXJyb3IoZGVjKSxcbiAgICAgICAgICAgIGBVbmV4cGVjdGVkIGRlY29yYXRvciAke25hbWV9IG9uIHBhcmFtZXRlci5gKTtcbiAgICAgIH1cbiAgICB9KTtcblxuICAgIGlmICh0b2tlbiBpbnN0YW5jZW9mIEV4dGVybmFsRXhwciAmJiB0b2tlbi52YWx1ZS5uYW1lID09PSAnQ2hhbmdlRGV0ZWN0b3JSZWYnICYmXG4gICAgICAgIHRva2VuLnZhbHVlLm1vZHVsZU5hbWUgPT09ICdAYW5ndWxhci9jb3JlJykge1xuICAgICAgcmVzb2x2ZWQgPSBSM1Jlc29sdmVkRGVwZW5kZW5jeVR5cGUuQ2hhbmdlRGV0ZWN0b3JSZWY7XG4gICAgfVxuICAgIGlmICh0b2tlbiA9PT0gbnVsbCkge1xuICAgICAgZXJyb3JzLnB1c2goe1xuICAgICAgICBpbmRleDogaWR4LFxuICAgICAgICBraW5kOiBDb25zdHJ1Y3RvckRlcEVycm9yS2luZC5OT19TVUlUQUJMRV9UT0tFTixcbiAgICAgICAgcGFyYW0sXG4gICAgICB9KTtcbiAgICB9IGVsc2Uge1xuICAgICAgZGVwcy5wdXNoKHt0b2tlbiwgYXR0cmlidXRlLCBvcHRpb25hbCwgc2VsZiwgc2tpcFNlbGYsIGhvc3QsIHJlc29sdmVkfSk7XG4gICAgfVxuICB9KTtcbiAgaWYgKGVycm9ycy5sZW5ndGggPT09IDApIHtcbiAgICByZXR1cm4ge2RlcHN9O1xuICB9IGVsc2Uge1xuICAgIHJldHVybiB7ZGVwczogbnVsbCwgZXJyb3JzfTtcbiAgfVxufVxuXG4vKipcbiAqIENvbnZlcnQgYSBgVHlwZVZhbHVlUmVmZXJlbmNlYCB0byBhbiBgRXhwcmVzc2lvbmAgd2hpY2ggcmVmZXJzIHRvIHRoZSB0eXBlIGFzIGEgdmFsdWUuXG4gKlxuICogTG9jYWwgcmVmZXJlbmNlcyBhcmUgY29udmVydGVkIHRvIGEgYFdyYXBwZWROb2RlRXhwcmAgb2YgdGhlIFR5cGVTY3JpcHQgZXhwcmVzc2lvbiwgYW5kIG5vbi1sb2NhbFxuICogcmVmZXJlbmNlcyBhcmUgY29udmVydGVkIHRvIGFuIGBFeHRlcm5hbEV4cHJgLiBOb3RlIHRoYXQgdGhpcyBpcyBvbmx5IHZhbGlkIGluIHRoZSBjb250ZXh0IG9mIHRoZVxuICogZmlsZSBpbiB3aGljaCB0aGUgYFR5cGVWYWx1ZVJlZmVyZW5jZWAgb3JpZ2luYXRlZC5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHZhbHVlUmVmZXJlbmNlVG9FeHByZXNzaW9uKFxuICAgIHZhbHVlUmVmOiBUeXBlVmFsdWVSZWZlcmVuY2UsIGRlZmF1bHRJbXBvcnRSZWNvcmRlcjogRGVmYXVsdEltcG9ydFJlY29yZGVyKTogRXhwcmVzc2lvbjtcbmV4cG9ydCBmdW5jdGlvbiB2YWx1ZVJlZmVyZW5jZVRvRXhwcmVzc2lvbihcbiAgICB2YWx1ZVJlZjogbnVsbCwgZGVmYXVsdEltcG9ydFJlY29yZGVyOiBEZWZhdWx0SW1wb3J0UmVjb3JkZXIpOiBudWxsO1xuZXhwb3J0IGZ1bmN0aW9uIHZhbHVlUmVmZXJlbmNlVG9FeHByZXNzaW9uKFxuICAgIHZhbHVlUmVmOiBUeXBlVmFsdWVSZWZlcmVuY2V8bnVsbCwgZGVmYXVsdEltcG9ydFJlY29yZGVyOiBEZWZhdWx0SW1wb3J0UmVjb3JkZXIpOiBFeHByZXNzaW9ufFxuICAgIG51bGw7XG5leHBvcnQgZnVuY3Rpb24gdmFsdWVSZWZlcmVuY2VUb0V4cHJlc3Npb24oXG4gICAgdmFsdWVSZWY6IFR5cGVWYWx1ZVJlZmVyZW5jZXxudWxsLCBkZWZhdWx0SW1wb3J0UmVjb3JkZXI6IERlZmF1bHRJbXBvcnRSZWNvcmRlcik6IEV4cHJlc3Npb258XG4gICAgbnVsbCB7XG4gIGlmICh2YWx1ZVJlZiA9PT0gbnVsbCkge1xuICAgIHJldHVybiBudWxsO1xuICB9IGVsc2UgaWYgKHZhbHVlUmVmLmxvY2FsKSB7XG4gICAgaWYgKGRlZmF1bHRJbXBvcnRSZWNvcmRlciAhPT0gbnVsbCAmJiB2YWx1ZVJlZi5kZWZhdWx0SW1wb3J0U3RhdGVtZW50ICE9PSBudWxsICYmXG4gICAgICAgIHRzLmlzSWRlbnRpZmllcih2YWx1ZVJlZi5leHByZXNzaW9uKSkge1xuICAgICAgZGVmYXVsdEltcG9ydFJlY29yZGVyLnJlY29yZEltcG9ydGVkSWRlbnRpZmllcihcbiAgICAgICAgICB2YWx1ZVJlZi5leHByZXNzaW9uLCB2YWx1ZVJlZi5kZWZhdWx0SW1wb3J0U3RhdGVtZW50KTtcbiAgICB9XG4gICAgcmV0dXJuIG5ldyBXcmFwcGVkTm9kZUV4cHIodmFsdWVSZWYuZXhwcmVzc2lvbik7XG4gIH0gZWxzZSB7XG4gICAgLy8gVE9ETyhhbHhodWIpOiB0aGlzIGNhc3QgaXMgbmVjZXNzYXJ5IGJlY2F1c2UgdGhlIGczIHR5cGVzY3JpcHQgdmVyc2lvbiBkb2Vzbid0IG5hcnJvdyBoZXJlLlxuICAgIGNvbnN0IHJlZiA9IHZhbHVlUmVmIGFzIHtcbiAgICAgIG1vZHVsZU5hbWU6IHN0cmluZztcbiAgICAgIGltcG9ydGVkTmFtZTogc3RyaW5nO1xuICAgICAgbmVzdGVkUGF0aDogc3RyaW5nW118bnVsbDtcbiAgICB9O1xuICAgIGxldCBpbXBvcnRFeHByOiBFeHByZXNzaW9uID1cbiAgICAgICAgbmV3IEV4dGVybmFsRXhwcih7bW9kdWxlTmFtZTogcmVmLm1vZHVsZU5hbWUsIG5hbWU6IHJlZi5pbXBvcnRlZE5hbWV9KTtcbiAgICBpZiAocmVmLm5lc3RlZFBhdGggIT09IG51bGwpIHtcbiAgICAgIGZvciAoY29uc3QgcHJvcGVydHkgb2YgcmVmLm5lc3RlZFBhdGgpIHtcbiAgICAgICAgaW1wb3J0RXhwciA9IG5ldyBSZWFkUHJvcEV4cHIoaW1wb3J0RXhwciwgcHJvcGVydHkpO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gaW1wb3J0RXhwcjtcbiAgfVxufVxuXG4vKipcbiAqIENvbnZlcnQgYENvbnN0cnVjdG9yRGVwc2AgaW50byB0aGUgYFIzRGVwZW5kZW5jeU1ldGFkYXRhYCBhcnJheSBmb3IgdGhvc2UgZGVwcyBpZiB0aGV5J3JlIHZhbGlkLFxuICogb3IgaW50byBhbiBgJ2ludmFsaWQnYCBzaWduYWwgaWYgdGhleSdyZSBub3QuXG4gKlxuICogVGhpcyBpcyBhIGNvbXBhbmlvbiBmdW5jdGlvbiB0byBgdmFsaWRhdGVDb25zdHJ1Y3RvckRlcGVuZGVuY2llc2Agd2hpY2ggYWNjZXB0cyBpbnZhbGlkIGRlcHMuXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiB1bndyYXBDb25zdHJ1Y3RvckRlcGVuZGVuY2llcyhkZXBzOiBDb25zdHJ1Y3RvckRlcHN8bnVsbCk6IFIzRGVwZW5kZW5jeU1ldGFkYXRhW118XG4gICAgJ2ludmFsaWQnfG51bGwge1xuICBpZiAoZGVwcyA9PT0gbnVsbCkge1xuICAgIHJldHVybiBudWxsO1xuICB9IGVsc2UgaWYgKGRlcHMuZGVwcyAhPT0gbnVsbCkge1xuICAgIC8vIFRoZXNlIGNvbnN0cnVjdG9yIGRlcGVuZGVuY2llcyBhcmUgdmFsaWQuXG4gICAgcmV0dXJuIGRlcHMuZGVwcztcbiAgfSBlbHNlIHtcbiAgICAvLyBUaGVzZSBkZXBzIGFyZSBpbnZhbGlkLlxuICAgIHJldHVybiAnaW52YWxpZCc7XG4gIH1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGdldFZhbGlkQ29uc3RydWN0b3JEZXBlbmRlbmNpZXMoXG4gICAgY2xheno6IENsYXNzRGVjbGFyYXRpb24sIHJlZmxlY3RvcjogUmVmbGVjdGlvbkhvc3QsXG4gICAgZGVmYXVsdEltcG9ydFJlY29yZGVyOiBEZWZhdWx0SW1wb3J0UmVjb3JkZXIsIGlzQ29yZTogYm9vbGVhbik6IFIzRGVwZW5kZW5jeU1ldGFkYXRhW118bnVsbCB7XG4gIHJldHVybiB2YWxpZGF0ZUNvbnN0cnVjdG9yRGVwZW5kZW5jaWVzKFxuICAgICAgY2xhenosIGdldENvbnN0cnVjdG9yRGVwZW5kZW5jaWVzKGNsYXp6LCByZWZsZWN0b3IsIGRlZmF1bHRJbXBvcnRSZWNvcmRlciwgaXNDb3JlKSk7XG59XG5cbi8qKlxuICogVmFsaWRhdGUgdGhhdCBgQ29uc3RydWN0b3JEZXBzYCBkb2VzIG5vdCBoYXZlIGFueSBpbnZhbGlkIGRlcGVuZGVuY2llcyBhbmQgY29udmVydCB0aGVtIGludG8gdGhlXG4gKiBgUjNEZXBlbmRlbmN5TWV0YWRhdGFgIGFycmF5IGlmIHNvLCBvciByYWlzZSBhIGRpYWdub3N0aWMgaWYgc29tZSBkZXBzIGFyZSBpbnZhbGlkLlxuICpcbiAqIFRoaXMgaXMgYSBjb21wYW5pb24gZnVuY3Rpb24gdG8gYHVud3JhcENvbnN0cnVjdG9yRGVwZW5kZW5jaWVzYCB3aGljaCBkb2VzIG5vdCBhY2NlcHQgaW52YWxpZFxuICogZGVwcy5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHZhbGlkYXRlQ29uc3RydWN0b3JEZXBlbmRlbmNpZXMoXG4gICAgY2xheno6IENsYXNzRGVjbGFyYXRpb24sIGRlcHM6IENvbnN0cnVjdG9yRGVwc3xudWxsKTogUjNEZXBlbmRlbmN5TWV0YWRhdGFbXXxudWxsIHtcbiAgaWYgKGRlcHMgPT09IG51bGwpIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfSBlbHNlIGlmIChkZXBzLmRlcHMgIT09IG51bGwpIHtcbiAgICByZXR1cm4gZGVwcy5kZXBzO1xuICB9IGVsc2Uge1xuICAgIC8vIFRPRE8oYWx4aHViKTogdGhpcyBjYXN0IGlzIG5lY2Vzc2FyeSBiZWNhdXNlIHRoZSBnMyB0eXBlc2NyaXB0IHZlcnNpb24gZG9lc24ndCBuYXJyb3cgaGVyZS5cbiAgICBjb25zdCB7cGFyYW0sIGluZGV4fSA9IChkZXBzIGFzIHtlcnJvcnM6IENvbnN0cnVjdG9yRGVwRXJyb3JbXX0pLmVycm9yc1swXTtcbiAgICAvLyBUaGVyZSBpcyBhdCBsZWFzdCBvbmUgZXJyb3IuXG4gICAgdGhyb3cgbmV3IEZhdGFsRGlhZ25vc3RpY0Vycm9yKFxuICAgICAgICBFcnJvckNvZGUuUEFSQU1fTUlTU0lOR19UT0tFTiwgcGFyYW0ubmFtZU5vZGUsXG4gICAgICAgIGBObyBzdWl0YWJsZSBpbmplY3Rpb24gdG9rZW4gZm9yIHBhcmFtZXRlciAnJHtwYXJhbS5uYW1lIHx8IGluZGV4fScgb2YgY2xhc3MgJyR7XG4gICAgICAgICAgICBjbGF6ei5uYW1lLnRleHR9Jy5cXG5gICtcbiAgICAgICAgICAgIChwYXJhbS50eXBlTm9kZSAhPT0gbnVsbCA/IGBGb3VuZCAke3BhcmFtLnR5cGVOb2RlLmdldFRleHQoKX1gIDpcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICdubyB0eXBlIG9yIGRlY29yYXRvcicpKTtcbiAgfVxufVxuXG5leHBvcnQgZnVuY3Rpb24gdG9SM1JlZmVyZW5jZShcbiAgICB2YWx1ZVJlZjogUmVmZXJlbmNlLCB0eXBlUmVmOiBSZWZlcmVuY2UsIHZhbHVlQ29udGV4dDogdHMuU291cmNlRmlsZSxcbiAgICB0eXBlQ29udGV4dDogdHMuU291cmNlRmlsZSwgcmVmRW1pdHRlcjogUmVmZXJlbmNlRW1pdHRlcik6IFIzUmVmZXJlbmNlIHtcbiAgY29uc3QgdmFsdWUgPSByZWZFbWl0dGVyLmVtaXQodmFsdWVSZWYsIHZhbHVlQ29udGV4dCk7XG4gIGNvbnN0IHR5cGUgPSByZWZFbWl0dGVyLmVtaXQoXG4gICAgICB0eXBlUmVmLCB0eXBlQ29udGV4dCwgSW1wb3J0RmxhZ3MuRm9yY2VOZXdJbXBvcnQgfCBJbXBvcnRGbGFncy5BbGxvd1R5cGVJbXBvcnRzKTtcbiAgaWYgKHZhbHVlID09PSBudWxsIHx8IHR5cGUgPT09IG51bGwpIHtcbiAgICB0aHJvdyBuZXcgRXJyb3IoYENvdWxkIG5vdCByZWZlciB0byAke3RzLlN5bnRheEtpbmRbdmFsdWVSZWYubm9kZS5raW5kXX1gKTtcbiAgfVxuICByZXR1cm4ge3ZhbHVlLCB0eXBlfTtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGlzQW5ndWxhckNvcmUoZGVjb3JhdG9yOiBEZWNvcmF0b3IpOiBkZWNvcmF0b3IgaXMgRGVjb3JhdG9yJntpbXBvcnQ6IEltcG9ydH0ge1xuICByZXR1cm4gZGVjb3JhdG9yLmltcG9ydCAhPT0gbnVsbCAmJiBkZWNvcmF0b3IuaW1wb3J0LmZyb20gPT09ICdAYW5ndWxhci9jb3JlJztcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGlzQW5ndWxhckNvcmVSZWZlcmVuY2UocmVmZXJlbmNlOiBSZWZlcmVuY2UsIHN5bWJvbE5hbWU6IHN0cmluZyk6IGJvb2xlYW4ge1xuICByZXR1cm4gcmVmZXJlbmNlLm93bmVkQnlNb2R1bGVHdWVzcyA9PT0gJ0Bhbmd1bGFyL2NvcmUnICYmIHJlZmVyZW5jZS5kZWJ1Z05hbWUgPT09IHN5bWJvbE5hbWU7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBmaW5kQW5ndWxhckRlY29yYXRvcihcbiAgICBkZWNvcmF0b3JzOiBEZWNvcmF0b3JbXSwgbmFtZTogc3RyaW5nLCBpc0NvcmU6IGJvb2xlYW4pOiBEZWNvcmF0b3J8dW5kZWZpbmVkIHtcbiAgcmV0dXJuIGRlY29yYXRvcnMuZmluZChkZWNvcmF0b3IgPT4gaXNBbmd1bGFyRGVjb3JhdG9yKGRlY29yYXRvciwgbmFtZSwgaXNDb3JlKSk7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBpc0FuZ3VsYXJEZWNvcmF0b3IoZGVjb3JhdG9yOiBEZWNvcmF0b3IsIG5hbWU6IHN0cmluZywgaXNDb3JlOiBib29sZWFuKTogYm9vbGVhbiB7XG4gIGlmIChpc0NvcmUpIHtcbiAgICByZXR1cm4gZGVjb3JhdG9yLm5hbWUgPT09IG5hbWU7XG4gIH0gZWxzZSBpZiAoaXNBbmd1bGFyQ29yZShkZWNvcmF0b3IpKSB7XG4gICAgcmV0dXJuIGRlY29yYXRvci5pbXBvcnQubmFtZSA9PT0gbmFtZTtcbiAgfVxuICByZXR1cm4gZmFsc2U7XG59XG5cbi8qKlxuICogVW53cmFwIGEgYHRzLkV4cHJlc3Npb25gLCByZW1vdmluZyBvdXRlciB0eXBlLWNhc3RzIG9yIHBhcmVudGhlc2VzIHVudGlsIHRoZSBleHByZXNzaW9uIGlzIGluIGl0c1xuICogbG93ZXN0IGxldmVsIGZvcm0uXG4gKlxuICogRm9yIGV4YW1wbGUsIHRoZSBleHByZXNzaW9uIFwiKGZvbyBhcyBUeXBlKVwiIHVud3JhcHMgdG8gXCJmb29cIi5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHVud3JhcEV4cHJlc3Npb24obm9kZTogdHMuRXhwcmVzc2lvbik6IHRzLkV4cHJlc3Npb24ge1xuICB3aGlsZSAodHMuaXNBc0V4cHJlc3Npb24obm9kZSkgfHwgdHMuaXNQYXJlbnRoZXNpemVkRXhwcmVzc2lvbihub2RlKSkge1xuICAgIG5vZGUgPSBub2RlLmV4cHJlc3Npb247XG4gIH1cbiAgcmV0dXJuIG5vZGU7XG59XG5cbmZ1bmN0aW9uIGV4cGFuZEZvcndhcmRSZWYoYXJnOiB0cy5FeHByZXNzaW9uKTogdHMuRXhwcmVzc2lvbnxudWxsIHtcbiAgYXJnID0gdW53cmFwRXhwcmVzc2lvbihhcmcpO1xuICBpZiAoIXRzLmlzQXJyb3dGdW5jdGlvbihhcmcpICYmICF0cy5pc0Z1bmN0aW9uRXhwcmVzc2lvbihhcmcpKSB7XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuICBjb25zdCBib2R5ID0gYXJnLmJvZHk7XG4gIC8vIEVpdGhlciB0aGUgYm9keSBpcyBhIHRzLkV4cHJlc3Npb24gZGlyZWN0bHksIG9yIGEgYmxvY2sgd2l0aCBhIHNpbmdsZSByZXR1cm4gc3RhdGVtZW50LlxuICBpZiAodHMuaXNCbG9jayhib2R5KSkge1xuICAgIC8vIEJsb2NrIGJvZHkgLSBsb29rIGZvciBhIHNpbmdsZSByZXR1cm4gc3RhdGVtZW50LlxuICAgIGlmIChib2R5LnN0YXRlbWVudHMubGVuZ3RoICE9PSAxKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG4gICAgY29uc3Qgc3RtdCA9IGJvZHkuc3RhdGVtZW50c1swXTtcbiAgICBpZiAoIXRzLmlzUmV0dXJuU3RhdGVtZW50KHN0bXQpIHx8IHN0bXQuZXhwcmVzc2lvbiA9PT0gdW5kZWZpbmVkKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG4gICAgcmV0dXJuIHN0bXQuZXhwcmVzc2lvbjtcbiAgfSBlbHNlIHtcbiAgICAvLyBTaG9ydGhhbmQgYm9keSAtIHJldHVybiBhcyBhbiBleHByZXNzaW9uLlxuICAgIHJldHVybiBib2R5O1xuICB9XG59XG5cbi8qKlxuICogUG9zc2libHkgcmVzb2x2ZSBhIGZvcndhcmRSZWYoKSBleHByZXNzaW9uIGludG8gdGhlIGlubmVyIHZhbHVlLlxuICpcbiAqIEBwYXJhbSBub2RlIHRoZSBmb3J3YXJkUmVmKCkgZXhwcmVzc2lvbiB0byByZXNvbHZlXG4gKiBAcGFyYW0gcmVmbGVjdG9yIGEgUmVmbGVjdGlvbkhvc3RcbiAqIEByZXR1cm5zIHRoZSByZXNvbHZlZCBleHByZXNzaW9uLCBpZiB0aGUgb3JpZ2luYWwgZXhwcmVzc2lvbiB3YXMgYSBmb3J3YXJkUmVmKCksIG9yIHRoZSBvcmlnaW5hbFxuICogZXhwcmVzc2lvbiBvdGhlcndpc2VcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHVud3JhcEZvcndhcmRSZWYobm9kZTogdHMuRXhwcmVzc2lvbiwgcmVmbGVjdG9yOiBSZWZsZWN0aW9uSG9zdCk6IHRzLkV4cHJlc3Npb24ge1xuICBub2RlID0gdW53cmFwRXhwcmVzc2lvbihub2RlKTtcbiAgaWYgKCF0cy5pc0NhbGxFeHByZXNzaW9uKG5vZGUpIHx8IG5vZGUuYXJndW1lbnRzLmxlbmd0aCAhPT0gMSkge1xuICAgIHJldHVybiBub2RlO1xuICB9XG5cbiAgY29uc3QgZm4gPVxuICAgICAgdHMuaXNQcm9wZXJ0eUFjY2Vzc0V4cHJlc3Npb24obm9kZS5leHByZXNzaW9uKSA/IG5vZGUuZXhwcmVzc2lvbi5uYW1lIDogbm9kZS5leHByZXNzaW9uO1xuICBpZiAoIXRzLmlzSWRlbnRpZmllcihmbikpIHtcbiAgICByZXR1cm4gbm9kZTtcbiAgfVxuXG4gIGNvbnN0IGV4cHIgPSBleHBhbmRGb3J3YXJkUmVmKG5vZGUuYXJndW1lbnRzWzBdKTtcbiAgaWYgKGV4cHIgPT09IG51bGwpIHtcbiAgICByZXR1cm4gbm9kZTtcbiAgfVxuICBjb25zdCBpbXAgPSByZWZsZWN0b3IuZ2V0SW1wb3J0T2ZJZGVudGlmaWVyKGZuKTtcbiAgaWYgKGltcCA9PT0gbnVsbCB8fCBpbXAuZnJvbSAhPT0gJ0Bhbmd1bGFyL2NvcmUnIHx8IGltcC5uYW1lICE9PSAnZm9yd2FyZFJlZicpIHtcbiAgICByZXR1cm4gbm9kZTtcbiAgfSBlbHNlIHtcbiAgICByZXR1cm4gZXhwcjtcbiAgfVxufVxuXG4vKipcbiAqIEEgZm9yZWlnbiBmdW5jdGlvbiByZXNvbHZlciBmb3IgYHN0YXRpY2FsbHlSZXNvbHZlYCB3aGljaCB1bndyYXBzIGZvcndhcmRSZWYoKSBleHByZXNzaW9ucy5cbiAqXG4gKiBAcGFyYW0gcmVmIGEgUmVmZXJlbmNlIHRvIHRoZSBkZWNsYXJhdGlvbiBvZiB0aGUgZnVuY3Rpb24gYmVpbmcgY2FsbGVkICh3aGljaCBtaWdodCBiZVxuICogZm9yd2FyZFJlZilcbiAqIEBwYXJhbSBhcmdzIHRoZSBhcmd1bWVudHMgdG8gdGhlIGludm9jYXRpb24gb2YgdGhlIGZvcndhcmRSZWYgZXhwcmVzc2lvblxuICogQHJldHVybnMgYW4gdW53cmFwcGVkIGFyZ3VtZW50IGlmIGByZWZgIHBvaW50ZWQgdG8gZm9yd2FyZFJlZiwgb3IgbnVsbCBvdGhlcndpc2VcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGZvcndhcmRSZWZSZXNvbHZlcihcbiAgICByZWY6IFJlZmVyZW5jZTx0cy5GdW5jdGlvbkRlY2xhcmF0aW9ufHRzLk1ldGhvZERlY2xhcmF0aW9ufHRzLkZ1bmN0aW9uRXhwcmVzc2lvbj4sXG4gICAgYXJnczogUmVhZG9ubHlBcnJheTx0cy5FeHByZXNzaW9uPik6IHRzLkV4cHJlc3Npb258bnVsbCB7XG4gIGlmICghaXNBbmd1bGFyQ29yZVJlZmVyZW5jZShyZWYsICdmb3J3YXJkUmVmJykgfHwgYXJncy5sZW5ndGggIT09IDEpIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuICByZXR1cm4gZXhwYW5kRm9yd2FyZFJlZihhcmdzWzBdKTtcbn1cblxuLyoqXG4gKiBDb21iaW5lcyBhbiBhcnJheSBvZiByZXNvbHZlciBmdW5jdGlvbnMgaW50byBhIG9uZS5cbiAqIEBwYXJhbSByZXNvbHZlcnMgUmVzb2x2ZXJzIHRvIGJlIGNvbWJpbmVkLlxuICovXG5leHBvcnQgZnVuY3Rpb24gY29tYmluZVJlc29sdmVycyhyZXNvbHZlcnM6IEZvcmVpZ25GdW5jdGlvblJlc29sdmVyW10pOiBGb3JlaWduRnVuY3Rpb25SZXNvbHZlciB7XG4gIHJldHVybiAocmVmOiBSZWZlcmVuY2U8dHMuRnVuY3Rpb25EZWNsYXJhdGlvbnx0cy5NZXRob2REZWNsYXJhdGlvbnx0cy5GdW5jdGlvbkV4cHJlc3Npb24+LFxuICAgICAgICAgIGFyZ3M6IFJlYWRvbmx5QXJyYXk8dHMuRXhwcmVzc2lvbj4pOiB0cy5FeHByZXNzaW9ufG51bGwgPT4ge1xuICAgIGZvciAoY29uc3QgcmVzb2x2ZXIgb2YgcmVzb2x2ZXJzKSB7XG4gICAgICBjb25zdCByZXNvbHZlZCA9IHJlc29sdmVyKHJlZiwgYXJncyk7XG4gICAgICBpZiAocmVzb2x2ZWQgIT09IG51bGwpIHtcbiAgICAgICAgcmV0dXJuIHJlc29sdmVkO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gbnVsbDtcbiAgfTtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGlzRXhwcmVzc2lvbkZvcndhcmRSZWZlcmVuY2UoXG4gICAgZXhwcjogRXhwcmVzc2lvbiwgY29udGV4dDogdHMuTm9kZSwgY29udGV4dFNvdXJjZTogdHMuU291cmNlRmlsZSk6IGJvb2xlYW4ge1xuICBpZiAoaXNXcmFwcGVkVHNOb2RlRXhwcihleHByKSkge1xuICAgIGNvbnN0IG5vZGUgPSB0cy5nZXRPcmlnaW5hbE5vZGUoZXhwci5ub2RlKTtcbiAgICByZXR1cm4gbm9kZS5nZXRTb3VyY2VGaWxlKCkgPT09IGNvbnRleHRTb3VyY2UgJiYgY29udGV4dC5wb3MgPCBub2RlLnBvcztcbiAgfSBlbHNlIHtcbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGlzV3JhcHBlZFRzTm9kZUV4cHIoZXhwcjogRXhwcmVzc2lvbik6IGV4cHIgaXMgV3JhcHBlZE5vZGVFeHByPHRzLk5vZGU+IHtcbiAgcmV0dXJuIGV4cHIgaW5zdGFuY2VvZiBXcmFwcGVkTm9kZUV4cHI7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiByZWFkQmFzZUNsYXNzKFxuICAgIG5vZGU6IENsYXNzRGVjbGFyYXRpb24sIHJlZmxlY3RvcjogUmVmbGVjdGlvbkhvc3QsXG4gICAgZXZhbHVhdG9yOiBQYXJ0aWFsRXZhbHVhdG9yKTogUmVmZXJlbmNlPENsYXNzRGVjbGFyYXRpb24+fCdkeW5hbWljJ3xudWxsIHtcbiAgY29uc3QgYmFzZUV4cHJlc3Npb24gPSByZWZsZWN0b3IuZ2V0QmFzZUNsYXNzRXhwcmVzc2lvbihub2RlKTtcbiAgaWYgKGJhc2VFeHByZXNzaW9uICE9PSBudWxsKSB7XG4gICAgY29uc3QgYmFzZUNsYXNzID0gZXZhbHVhdG9yLmV2YWx1YXRlKGJhc2VFeHByZXNzaW9uKTtcbiAgICBpZiAoYmFzZUNsYXNzIGluc3RhbmNlb2YgUmVmZXJlbmNlICYmIHJlZmxlY3Rvci5pc0NsYXNzKGJhc2VDbGFzcy5ub2RlKSkge1xuICAgICAgcmV0dXJuIGJhc2VDbGFzcyBhcyBSZWZlcmVuY2U8Q2xhc3NEZWNsYXJhdGlvbj47XG4gICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiAnZHluYW1pYyc7XG4gICAgfVxuICB9XG5cbiAgcmV0dXJuIG51bGw7XG59XG5cbmNvbnN0IHBhcmVuc1dyYXBwZXJUcmFuc2Zvcm1lckZhY3Rvcnk6IHRzLlRyYW5zZm9ybWVyRmFjdG9yeTx0cy5FeHByZXNzaW9uPiA9XG4gICAgKGNvbnRleHQ6IHRzLlRyYW5zZm9ybWF0aW9uQ29udGV4dCkgPT4ge1xuICAgICAgY29uc3QgdmlzaXRvcjogdHMuVmlzaXRvciA9IChub2RlOiB0cy5Ob2RlKTogdHMuTm9kZSA9PiB7XG4gICAgICAgIGNvbnN0IHZpc2l0ZWQgPSB0cy52aXNpdEVhY2hDaGlsZChub2RlLCB2aXNpdG9yLCBjb250ZXh0KTtcbiAgICAgICAgaWYgKHRzLmlzQXJyb3dGdW5jdGlvbih2aXNpdGVkKSB8fCB0cy5pc0Z1bmN0aW9uRXhwcmVzc2lvbih2aXNpdGVkKSkge1xuICAgICAgICAgIHJldHVybiB0cy5jcmVhdGVQYXJlbih2aXNpdGVkKTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gdmlzaXRlZDtcbiAgICAgIH07XG4gICAgICByZXR1cm4gKG5vZGU6IHRzLkV4cHJlc3Npb24pID0+IHRzLnZpc2l0RWFjaENoaWxkKG5vZGUsIHZpc2l0b3IsIGNvbnRleHQpO1xuICAgIH07XG5cbi8qKlxuICogV3JhcHMgYWxsIGZ1bmN0aW9ucyBpbiBhIGdpdmVuIGV4cHJlc3Npb24gaW4gcGFyZW50aGVzZXMuIFRoaXMgaXMgbmVlZGVkIHRvIGF2b2lkIHByb2JsZW1zXG4gKiB3aGVyZSBUc2lja2xlIGFubm90YXRpb25zIGFkZGVkIGJldHdlZW4gYW5hbHlzZSBhbmQgdHJhbnNmb3JtIHBoYXNlcyBpbiBBbmd1bGFyIG1heSB0cmlnZ2VyXG4gKiBhdXRvbWF0aWMgc2VtaWNvbG9uIGluc2VydGlvbiwgZS5nLiBpZiBhIGZ1bmN0aW9uIGlzIHRoZSBleHByZXNzaW9uIGluIGEgYHJldHVybmAgc3RhdGVtZW50LlxuICogTW9yZVxuICogaW5mbyBjYW4gYmUgZm91bmQgaW4gVHNpY2tsZSBzb3VyY2UgY29kZSBoZXJlOlxuICogaHR0cHM6Ly9naXRodWIuY29tL2FuZ3VsYXIvdHNpY2tsZS9ibG9iL2Q3OTc0MjYyNTcxYzhhMTdkNjg0ZTViYTA3NjgwZTFiMTk5M2FmZGQvc3JjL2pzZG9jX3RyYW5zZm9ybWVyLnRzI0wxMDIxXG4gKlxuICogQHBhcmFtIGV4cHJlc3Npb24gRXhwcmVzc2lvbiB3aGVyZSBmdW5jdGlvbnMgc2hvdWxkIGJlIHdyYXBwZWQgaW4gcGFyZW50aGVzZXNcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHdyYXBGdW5jdGlvbkV4cHJlc3Npb25zSW5QYXJlbnMoZXhwcmVzc2lvbjogdHMuRXhwcmVzc2lvbik6IHRzLkV4cHJlc3Npb24ge1xuICByZXR1cm4gdHMudHJhbnNmb3JtKGV4cHJlc3Npb24sIFtwYXJlbnNXcmFwcGVyVHJhbnNmb3JtZXJGYWN0b3J5XSkudHJhbnNmb3JtZWRbMF07XG59XG5cbi8qKlxuICogQ3JlYXRlIGEgYHRzLkRpYWdub3N0aWNgIHdoaWNoIGluZGljYXRlcyB0aGUgZ2l2ZW4gY2xhc3MgaXMgcGFydCBvZiB0aGUgZGVjbGFyYXRpb25zIG9mIHR3byBvclxuICogbW9yZSBOZ01vZHVsZXMuXG4gKlxuICogVGhlIHJlc3VsdGluZyBgdHMuRGlhZ25vc3RpY2Agd2lsbCBoYXZlIGEgY29udGV4dCBlbnRyeSBmb3IgZWFjaCBOZ01vZHVsZSBzaG93aW5nIHRoZSBwb2ludCB3aGVyZVxuICogdGhlIGRpcmVjdGl2ZS9waXBlIGV4aXN0cyBpbiBpdHMgYGRlY2xhcmF0aW9uc2AgKGlmIHBvc3NpYmxlKS5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIG1ha2VEdXBsaWNhdGVEZWNsYXJhdGlvbkVycm9yKFxuICAgIG5vZGU6IENsYXNzRGVjbGFyYXRpb24sIGRhdGE6IERlY2xhcmF0aW9uRGF0YVtdLCBraW5kOiBzdHJpbmcpOiB0cy5EaWFnbm9zdGljIHtcbiAgY29uc3QgY29udGV4dDoge25vZGU6IHRzLk5vZGU7IG1lc3NhZ2VUZXh0OiBzdHJpbmc7fVtdID0gW107XG4gIGZvciAoY29uc3QgZGVjbCBvZiBkYXRhKSB7XG4gICAgaWYgKGRlY2wucmF3RGVjbGFyYXRpb25zID09PSBudWxsKSB7XG4gICAgICBjb250aW51ZTtcbiAgICB9XG4gICAgLy8gVHJ5IHRvIGZpbmQgdGhlIHJlZmVyZW5jZSB0byB0aGUgZGVjbGFyYXRpb24gd2l0aGluIHRoZSBkZWNsYXJhdGlvbnMgYXJyYXksIHRvIGhhbmcgdGhlXG4gICAgLy8gZXJyb3IgdGhlcmUuIElmIGl0IGNhbid0IGJlIGZvdW5kLCBmYWxsIGJhY2sgb24gdXNpbmcgdGhlIE5nTW9kdWxlJ3MgbmFtZS5cbiAgICBjb25zdCBjb250ZXh0Tm9kZSA9IGRlY2wucmVmLmdldE9yaWdpbkZvckRpYWdub3N0aWNzKGRlY2wucmF3RGVjbGFyYXRpb25zLCBkZWNsLm5nTW9kdWxlLm5hbWUpO1xuICAgIGNvbnRleHQucHVzaCh7XG4gICAgICBub2RlOiBjb250ZXh0Tm9kZSxcbiAgICAgIG1lc3NhZ2VUZXh0OiBgJyR7bm9kZS5uYW1lLnRleHR9JyBpcyBsaXN0ZWQgaW4gdGhlIGRlY2xhcmF0aW9ucyBvZiB0aGUgTmdNb2R1bGUgJyR7XG4gICAgICAgICAgZGVjbC5uZ01vZHVsZS5uYW1lLnRleHR9Jy5gLFxuICAgIH0pO1xuICB9XG5cbiAgLy8gRmluYWxseSwgcHJvZHVjZSB0aGUgZGlhZ25vc3RpYy5cbiAgcmV0dXJuIG1ha2VEaWFnbm9zdGljKFxuICAgICAgRXJyb3JDb2RlLk5HTU9EVUxFX0RFQ0xBUkFUSU9OX05PVF9VTklRVUUsIG5vZGUubmFtZSxcbiAgICAgIGBUaGUgJHtraW5kfSAnJHtub2RlLm5hbWUudGV4dH0nIGlzIGRlY2xhcmVkIGJ5IG1vcmUgdGhhbiBvbmUgTmdNb2R1bGUuYCwgY29udGV4dCk7XG59XG5cbi8qKlxuICogUmVzb2x2ZXMgdGhlIGdpdmVuIGByYXdQcm92aWRlcnNgIGludG8gYENsYXNzRGVjbGFyYXRpb25zYCBhbmQgcmV0dXJuc1xuICogYSBzZXQgY29udGFpbmluZyB0aG9zZSB0aGF0IGFyZSBrbm93biB0byByZXF1aXJlIGEgZmFjdG9yeSBkZWZpbml0aW9uLlxuICogQHBhcmFtIHJhd1Byb3ZpZGVycyBFeHByZXNzaW9uIHRoYXQgZGVjbGFyZWQgdGhlIHByb3ZpZGVycyBhcnJheSBpbiB0aGUgc291cmNlLlxuICovXG5leHBvcnQgZnVuY3Rpb24gcmVzb2x2ZVByb3ZpZGVyc1JlcXVpcmluZ0ZhY3RvcnkoXG4gICAgcmF3UHJvdmlkZXJzOiB0cy5FeHByZXNzaW9uLCByZWZsZWN0b3I6IFJlZmxlY3Rpb25Ib3N0LFxuICAgIGV2YWx1YXRvcjogUGFydGlhbEV2YWx1YXRvcik6IFNldDxSZWZlcmVuY2U8Q2xhc3NEZWNsYXJhdGlvbj4+IHtcbiAgY29uc3QgcHJvdmlkZXJzID0gbmV3IFNldDxSZWZlcmVuY2U8Q2xhc3NEZWNsYXJhdGlvbj4+KCk7XG4gIGNvbnN0IHJlc29sdmVkUHJvdmlkZXJzID0gZXZhbHVhdG9yLmV2YWx1YXRlKHJhd1Byb3ZpZGVycyk7XG5cbiAgaWYgKCFBcnJheS5pc0FycmF5KHJlc29sdmVkUHJvdmlkZXJzKSkge1xuICAgIHJldHVybiBwcm92aWRlcnM7XG4gIH1cblxuICByZXNvbHZlZFByb3ZpZGVycy5mb3JFYWNoKGZ1bmN0aW9uIHByb2Nlc3NQcm92aWRlcnMocHJvdmlkZXIpIHtcbiAgICBsZXQgdG9rZW5DbGFzczogUmVmZXJlbmNlfG51bGwgPSBudWxsO1xuXG4gICAgaWYgKEFycmF5LmlzQXJyYXkocHJvdmlkZXIpKSB7XG4gICAgICAvLyBJZiB3ZSByYW4gaW50byBhbiBhcnJheSwgcmVjdXJzZSBpbnRvIGl0IHVudGlsIHdlJ3ZlIHJlc29sdmUgYWxsIHRoZSBjbGFzc2VzLlxuICAgICAgcHJvdmlkZXIuZm9yRWFjaChwcm9jZXNzUHJvdmlkZXJzKTtcbiAgICB9IGVsc2UgaWYgKHByb3ZpZGVyIGluc3RhbmNlb2YgUmVmZXJlbmNlKSB7XG4gICAgICB0b2tlbkNsYXNzID0gcHJvdmlkZXI7XG4gICAgfSBlbHNlIGlmIChwcm92aWRlciBpbnN0YW5jZW9mIE1hcCAmJiBwcm92aWRlci5oYXMoJ3VzZUNsYXNzJykgJiYgIXByb3ZpZGVyLmhhcygnZGVwcycpKSB7XG4gICAgICBjb25zdCB1c2VFeGlzdGluZyA9IHByb3ZpZGVyLmdldCgndXNlQ2xhc3MnKSE7XG4gICAgICBpZiAodXNlRXhpc3RpbmcgaW5zdGFuY2VvZiBSZWZlcmVuY2UpIHtcbiAgICAgICAgdG9rZW5DbGFzcyA9IHVzZUV4aXN0aW5nO1xuICAgICAgfVxuICAgIH1cblxuICAgIGlmICh0b2tlbkNsYXNzICE9PSBudWxsICYmIHJlZmxlY3Rvci5pc0NsYXNzKHRva2VuQ2xhc3Mubm9kZSkpIHtcbiAgICAgIGNvbnN0IGNvbnN0cnVjdG9yUGFyYW1ldGVycyA9IHJlZmxlY3Rvci5nZXRDb25zdHJ1Y3RvclBhcmFtZXRlcnModG9rZW5DbGFzcy5ub2RlKTtcblxuICAgICAgLy8gTm90ZSB0aGF0IHdlIG9ubHkgd2FudCB0byBjYXB0dXJlIHByb3ZpZGVycyB3aXRoIGEgbm9uLXRyaXZpYWwgY29uc3RydWN0b3IsXG4gICAgICAvLyBiZWNhdXNlIHRoZXkncmUgdGhlIG9uZXMgdGhhdCBtaWdodCBiZSB1c2luZyBESSBhbmQgbmVlZCB0byBiZSBkZWNvcmF0ZWQuXG4gICAgICBpZiAoY29uc3RydWN0b3JQYXJhbWV0ZXJzICE9PSBudWxsICYmIGNvbnN0cnVjdG9yUGFyYW1ldGVycy5sZW5ndGggPiAwKSB7XG4gICAgICAgIHByb3ZpZGVycy5hZGQodG9rZW5DbGFzcyBhcyBSZWZlcmVuY2U8Q2xhc3NEZWNsYXJhdGlvbj4pO1xuICAgICAgfVxuICAgIH1cbiAgfSk7XG5cbiAgcmV0dXJuIHByb3ZpZGVycztcbn1cblxuLyoqXG4gKiBDcmVhdGUgYW4gUjNSZWZlcmVuY2UgZm9yIGEgY2xhc3MuXG4gKlxuICogVGhlIGB2YWx1ZWAgaXMgdGhlIGV4cG9ydGVkIGRlY2xhcmF0aW9uIG9mIHRoZSBjbGFzcyBmcm9tIGl0cyBzb3VyY2UgZmlsZS5cbiAqIFRoZSBgdHlwZWAgaXMgYW4gZXhwcmVzc2lvbiB0aGF0IHdvdWxkIGJlIHVzZWQgYnkgbmdjYyBpbiB0aGUgdHlwaW5ncyAoLmQudHMpIGZpbGVzLlxuICovXG5leHBvcnQgZnVuY3Rpb24gd3JhcFR5cGVSZWZlcmVuY2UocmVmbGVjdG9yOiBSZWZsZWN0aW9uSG9zdCwgY2xheno6IENsYXNzRGVjbGFyYXRpb24pOiBSM1JlZmVyZW5jZSB7XG4gIGNvbnN0IGR0c0NsYXNzID0gcmVmbGVjdG9yLmdldER0c0RlY2xhcmF0aW9uKGNsYXp6KTtcbiAgY29uc3QgdmFsdWUgPSBuZXcgV3JhcHBlZE5vZGVFeHByKGNsYXp6Lm5hbWUpO1xuICBjb25zdCB0eXBlID0gZHRzQ2xhc3MgIT09IG51bGwgJiYgaXNOYW1lZENsYXNzRGVjbGFyYXRpb24oZHRzQ2xhc3MpID9cbiAgICAgIG5ldyBXcmFwcGVkTm9kZUV4cHIoZHRzQ2xhc3MubmFtZSkgOlxuICAgICAgdmFsdWU7XG4gIHJldHVybiB7dmFsdWUsIHR5cGV9O1xufVxuXG4vKiogQ3JlYXRlcyBhIFBhcnNlU291cmNlU3BhbiBmb3IgYSBUeXBlU2NyaXB0IG5vZGUuICovXG5leHBvcnQgZnVuY3Rpb24gY3JlYXRlU291cmNlU3Bhbihub2RlOiB0cy5Ob2RlKTogUGFyc2VTb3VyY2VTcGFuIHtcbiAgY29uc3Qgc2YgPSBub2RlLmdldFNvdXJjZUZpbGUoKTtcbiAgY29uc3QgW3N0YXJ0T2Zmc2V0LCBlbmRPZmZzZXRdID0gW25vZGUuZ2V0U3RhcnQoKSwgbm9kZS5nZXRFbmQoKV07XG4gIGNvbnN0IHtsaW5lOiBzdGFydExpbmUsIGNoYXJhY3Rlcjogc3RhcnRDb2x9ID0gc2YuZ2V0TGluZUFuZENoYXJhY3Rlck9mUG9zaXRpb24oc3RhcnRPZmZzZXQpO1xuICBjb25zdCB7bGluZTogZW5kTGluZSwgY2hhcmFjdGVyOiBlbmRDb2x9ID0gc2YuZ2V0TGluZUFuZENoYXJhY3Rlck9mUG9zaXRpb24oZW5kT2Zmc2V0KTtcbiAgY29uc3QgcGFyc2VTZiA9IG5ldyBQYXJzZVNvdXJjZUZpbGUoc2YuZ2V0RnVsbFRleHQoKSwgc2YuZmlsZU5hbWUpO1xuXG4gIC8vICsxIGJlY2F1c2UgdmFsdWVzIGFyZSB6ZXJvLWluZGV4ZWQuXG4gIHJldHVybiBuZXcgUGFyc2VTb3VyY2VTcGFuKFxuICAgICAgbmV3IFBhcnNlTG9jYXRpb24ocGFyc2VTZiwgc3RhcnRPZmZzZXQsIHN0YXJ0TGluZSArIDEsIHN0YXJ0Q29sICsgMSksXG4gICAgICBuZXcgUGFyc2VMb2NhdGlvbihwYXJzZVNmLCBlbmRPZmZzZXQsIGVuZExpbmUgKyAxLCBlbmRDb2wgKyAxKSk7XG59XG4iXX0=