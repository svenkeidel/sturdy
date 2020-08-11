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
        define("@angular/compiler/src/render3/r3_factory", ["require", "exports", "tslib", "@angular/compiler/src/aot/static_symbol", "@angular/compiler/src/compile_metadata", "@angular/compiler/src/identifiers", "@angular/compiler/src/output/output_ast", "@angular/compiler/src/render3/r3_identifiers", "@angular/compiler/src/render3/util", "@angular/compiler/src/render3/view/util"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.dependenciesFromGlobalMetadata = exports.compileFactoryFunction = exports.R3ResolvedDependencyType = exports.R3FactoryTarget = exports.R3FactoryDelegateType = void 0;
    var tslib_1 = require("tslib");
    var static_symbol_1 = require("@angular/compiler/src/aot/static_symbol");
    var compile_metadata_1 = require("@angular/compiler/src/compile_metadata");
    var identifiers_1 = require("@angular/compiler/src/identifiers");
    var o = require("@angular/compiler/src/output/output_ast");
    var r3_identifiers_1 = require("@angular/compiler/src/render3/r3_identifiers");
    var util_1 = require("@angular/compiler/src/render3/util");
    var util_2 = require("@angular/compiler/src/render3/view/util");
    var R3FactoryDelegateType;
    (function (R3FactoryDelegateType) {
        R3FactoryDelegateType[R3FactoryDelegateType["Class"] = 0] = "Class";
        R3FactoryDelegateType[R3FactoryDelegateType["Function"] = 1] = "Function";
        R3FactoryDelegateType[R3FactoryDelegateType["Factory"] = 2] = "Factory";
    })(R3FactoryDelegateType = exports.R3FactoryDelegateType || (exports.R3FactoryDelegateType = {}));
    var R3FactoryTarget;
    (function (R3FactoryTarget) {
        R3FactoryTarget[R3FactoryTarget["Directive"] = 0] = "Directive";
        R3FactoryTarget[R3FactoryTarget["Component"] = 1] = "Component";
        R3FactoryTarget[R3FactoryTarget["Injectable"] = 2] = "Injectable";
        R3FactoryTarget[R3FactoryTarget["Pipe"] = 3] = "Pipe";
        R3FactoryTarget[R3FactoryTarget["NgModule"] = 4] = "NgModule";
    })(R3FactoryTarget = exports.R3FactoryTarget || (exports.R3FactoryTarget = {}));
    /**
     * Resolved type of a dependency.
     *
     * Occasionally, dependencies will have special significance which is known statically. In that
     * case the `R3ResolvedDependencyType` informs the factory generator that a particular dependency
     * should be generated specially (usually by calling a special injection function instead of the
     * standard one).
     */
    var R3ResolvedDependencyType;
    (function (R3ResolvedDependencyType) {
        /**
         * A normal token dependency.
         */
        R3ResolvedDependencyType[R3ResolvedDependencyType["Token"] = 0] = "Token";
        /**
         * The dependency is for an attribute.
         *
         * The token expression is a string representing the attribute name.
         */
        R3ResolvedDependencyType[R3ResolvedDependencyType["Attribute"] = 1] = "Attribute";
        /**
         * Injecting the `ChangeDetectorRef` token. Needs special handling when injected into a pipe.
         */
        R3ResolvedDependencyType[R3ResolvedDependencyType["ChangeDetectorRef"] = 2] = "ChangeDetectorRef";
        /**
         * An invalid dependency (no token could be determined). An error should be thrown at runtime.
         */
        R3ResolvedDependencyType[R3ResolvedDependencyType["Invalid"] = 3] = "Invalid";
    })(R3ResolvedDependencyType = exports.R3ResolvedDependencyType || (exports.R3ResolvedDependencyType = {}));
    /**
     * Construct a factory function expression for the given `R3FactoryMetadata`.
     */
    function compileFactoryFunction(meta) {
        var t = o.variable('t');
        var statements = [];
        var ctorDepsType = o.NONE_TYPE;
        // The type to instantiate via constructor invocation. If there is no delegated factory, meaning
        // this type is always created by constructor invocation, then this is the type-to-create
        // parameter provided by the user (t) if specified, or the current type if not. If there is a
        // delegated factory (which is used to create the current type) then this is only the type-to-
        // create parameter (t).
        var typeForCtor = !isDelegatedMetadata(meta) ?
            new o.BinaryOperatorExpr(o.BinaryOperator.Or, t, meta.internalType) :
            t;
        var ctorExpr = null;
        if (meta.deps !== null) {
            // There is a constructor (either explicitly or implicitly defined).
            if (meta.deps !== 'invalid') {
                ctorExpr = new o.InstantiateExpr(typeForCtor, injectDependencies(meta.deps, meta.injectFn, meta.target === R3FactoryTarget.Pipe));
                ctorDepsType = createCtorDepsType(meta.deps);
            }
        }
        else {
            var baseFactory = o.variable("\u0275" + meta.name + "_BaseFactory");
            var getInheritedFactory = o.importExpr(r3_identifiers_1.Identifiers.getInheritedFactory);
            var baseFactoryStmt = baseFactory.set(getInheritedFactory.callFn([meta.internalType]))
                .toDeclStmt(o.INFERRED_TYPE, [o.StmtModifier.Exported, o.StmtModifier.Final]);
            statements.push(baseFactoryStmt);
            // There is no constructor, use the base class' factory to construct typeForCtor.
            ctorExpr = baseFactory.callFn([typeForCtor]);
        }
        var ctorExprFinal = ctorExpr;
        var body = [];
        var retExpr = null;
        function makeConditionalFactory(nonCtorExpr) {
            var r = o.variable('r');
            body.push(r.set(o.NULL_EXPR).toDeclStmt());
            var ctorStmt = null;
            if (ctorExprFinal !== null) {
                ctorStmt = r.set(ctorExprFinal).toStmt();
            }
            else {
                ctorStmt = o.importExpr(r3_identifiers_1.Identifiers.invalidFactory).callFn([]).toStmt();
            }
            body.push(o.ifStmt(t, [ctorStmt], [r.set(nonCtorExpr).toStmt()]));
            return r;
        }
        if (isDelegatedMetadata(meta) && meta.delegateType === R3FactoryDelegateType.Factory) {
            var delegateFactory = o.variable("\u0275" + meta.name + "_BaseFactory");
            var getFactoryOf = o.importExpr(r3_identifiers_1.Identifiers.getFactoryOf);
            if (meta.delegate.isEquivalent(meta.internalType)) {
                throw new Error("Illegal state: compiling factory that delegates to itself");
            }
            var delegateFactoryStmt = delegateFactory.set(getFactoryOf.callFn([meta.delegate])).toDeclStmt(o.INFERRED_TYPE, [
                o.StmtModifier.Exported, o.StmtModifier.Final
            ]);
            statements.push(delegateFactoryStmt);
            retExpr = makeConditionalFactory(delegateFactory.callFn([]));
        }
        else if (isDelegatedMetadata(meta)) {
            // This type is created with a delegated factory. If a type parameter is not specified, call
            // the factory instead.
            var delegateArgs = injectDependencies(meta.delegateDeps, meta.injectFn, meta.target === R3FactoryTarget.Pipe);
            // Either call `new delegate(...)` or `delegate(...)` depending on meta.delegateType.
            var factoryExpr = new (meta.delegateType === R3FactoryDelegateType.Class ?
                o.InstantiateExpr :
                o.InvokeFunctionExpr)(meta.delegate, delegateArgs);
            retExpr = makeConditionalFactory(factoryExpr);
        }
        else if (isExpressionFactoryMetadata(meta)) {
            // TODO(alxhub): decide whether to lower the value here or in the caller
            retExpr = makeConditionalFactory(meta.expression);
        }
        else {
            retExpr = ctorExpr;
        }
        if (retExpr !== null) {
            body.push(new o.ReturnStatement(retExpr));
        }
        else {
            body.push(o.importExpr(r3_identifiers_1.Identifiers.invalidFactory).callFn([]).toStmt());
        }
        return {
            factory: o.fn([new o.FnParam('t', o.DYNAMIC_TYPE)], body, o.INFERRED_TYPE, undefined, meta.name + "_Factory"),
            statements: statements,
            type: o.expressionType(o.importExpr(r3_identifiers_1.Identifiers.FactoryDef, [util_1.typeWithParameters(meta.type.type, meta.typeArgumentCount), ctorDepsType]))
        };
    }
    exports.compileFactoryFunction = compileFactoryFunction;
    function injectDependencies(deps, injectFn, isPipe) {
        return deps.map(function (dep, index) { return compileInjectDependency(dep, injectFn, isPipe, index); });
    }
    function compileInjectDependency(dep, injectFn, isPipe, index) {
        // Interpret the dependency according to its resolved type.
        switch (dep.resolved) {
            case R3ResolvedDependencyType.Token:
            case R3ResolvedDependencyType.ChangeDetectorRef:
                // Build up the injection flags according to the metadata.
                var flags = 0 /* Default */ | (dep.self ? 2 /* Self */ : 0) |
                    (dep.skipSelf ? 4 /* SkipSelf */ : 0) | (dep.host ? 1 /* Host */ : 0) |
                    (dep.optional ? 8 /* Optional */ : 0);
                // If this dependency is optional or otherwise has non-default flags, then additional
                // parameters describing how to inject the dependency must be passed to the inject function
                // that's being used.
                var flagsParam = (flags !== 0 /* Default */ || dep.optional) ? o.literal(flags) : null;
                // We have a separate instruction for injecting ChangeDetectorRef into a pipe.
                if (isPipe && dep.resolved === R3ResolvedDependencyType.ChangeDetectorRef) {
                    return o.importExpr(r3_identifiers_1.Identifiers.injectPipeChangeDetectorRef).callFn(flagsParam ? [flagsParam] : []);
                }
                // Build up the arguments to the injectFn call.
                var injectArgs = [dep.token];
                if (flagsParam) {
                    injectArgs.push(flagsParam);
                }
                return o.importExpr(injectFn).callFn(injectArgs);
            case R3ResolvedDependencyType.Attribute:
                // In the case of attributes, the attribute name in question is given as the token.
                return o.importExpr(r3_identifiers_1.Identifiers.injectAttribute).callFn([dep.token]);
            case R3ResolvedDependencyType.Invalid:
                return o.importExpr(r3_identifiers_1.Identifiers.invalidFactoryDep).callFn([o.literal(index)]);
            default:
                return util_2.unsupported("Unknown R3ResolvedDependencyType: " + R3ResolvedDependencyType[dep.resolved]);
        }
    }
    function createCtorDepsType(deps) {
        var hasTypes = false;
        var attributeTypes = deps.map(function (dep) {
            var type = createCtorDepType(dep);
            if (type !== null) {
                hasTypes = true;
                return type;
            }
            else {
                return o.literal(null);
            }
        });
        if (hasTypes) {
            return o.expressionType(o.literalArr(attributeTypes));
        }
        else {
            return o.NONE_TYPE;
        }
    }
    function createCtorDepType(dep) {
        var entries = [];
        if (dep.resolved === R3ResolvedDependencyType.Attribute) {
            if (dep.attribute !== null) {
                entries.push({ key: 'attribute', value: dep.attribute, quoted: false });
            }
        }
        if (dep.optional) {
            entries.push({ key: 'optional', value: o.literal(true), quoted: false });
        }
        if (dep.host) {
            entries.push({ key: 'host', value: o.literal(true), quoted: false });
        }
        if (dep.self) {
            entries.push({ key: 'self', value: o.literal(true), quoted: false });
        }
        if (dep.skipSelf) {
            entries.push({ key: 'skipSelf', value: o.literal(true), quoted: false });
        }
        return entries.length > 0 ? o.literalMap(entries) : null;
    }
    /**
     * A helper function useful for extracting `R3DependencyMetadata` from a Render2
     * `CompileTypeMetadata` instance.
     */
    function dependenciesFromGlobalMetadata(type, outputCtx, reflector) {
        var e_1, _a;
        // Use the `CompileReflector` to look up references to some well-known Angular types. These will
        // be compared with the token to statically determine whether the token has significance to
        // Angular, and set the correct `R3ResolvedDependencyType` as a result.
        var injectorRef = reflector.resolveExternalReference(identifiers_1.Identifiers.Injector);
        // Iterate through the type's DI dependencies and produce `R3DependencyMetadata` for each of them.
        var deps = [];
        try {
            for (var _b = tslib_1.__values(type.diDeps), _c = _b.next(); !_c.done; _c = _b.next()) {
                var dependency = _c.value;
                if (dependency.token) {
                    var tokenRef = compile_metadata_1.tokenReference(dependency.token);
                    var resolved = dependency.isAttribute ?
                        R3ResolvedDependencyType.Attribute :
                        R3ResolvedDependencyType.Token;
                    // In the case of most dependencies, the token will be a reference to a type. Sometimes,
                    // however, it can be a string, in the case of older Angular code or @Attribute injection.
                    var token = tokenRef instanceof static_symbol_1.StaticSymbol ? outputCtx.importExpr(tokenRef) : o.literal(tokenRef);
                    // Construct the dependency.
                    deps.push({
                        token: token,
                        attribute: null,
                        resolved: resolved,
                        host: !!dependency.isHost,
                        optional: !!dependency.isOptional,
                        self: !!dependency.isSelf,
                        skipSelf: !!dependency.isSkipSelf,
                    });
                }
                else {
                    util_2.unsupported('dependency without a token');
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return deps;
    }
    exports.dependenciesFromGlobalMetadata = dependenciesFromGlobalMetadata;
    function isDelegatedMetadata(meta) {
        return meta.delegateType !== undefined;
    }
    function isExpressionFactoryMetadata(meta) {
        return meta.expression !== undefined;
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicjNfZmFjdG9yeS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2NvbXBpbGVyL3NyYy9yZW5kZXIzL3IzX2ZhY3RvcnkudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOzs7Ozs7Ozs7Ozs7OztJQUVILHlFQUFrRDtJQUNsRCwyRUFBd0U7SUFHeEUsaUVBQTJDO0lBQzNDLDJEQUEwQztJQUMxQywrRUFBNEQ7SUFHNUQsMkRBQXVEO0lBQ3ZELGdFQUF3QztJQW9EeEMsSUFBWSxxQkFJWDtJQUpELFdBQVkscUJBQXFCO1FBQy9CLG1FQUFLLENBQUE7UUFDTCx5RUFBUSxDQUFBO1FBQ1IsdUVBQU8sQ0FBQTtJQUNULENBQUMsRUFKVyxxQkFBcUIsR0FBckIsNkJBQXFCLEtBQXJCLDZCQUFxQixRQUloQztJQW9CRCxJQUFZLGVBTVg7SUFORCxXQUFZLGVBQWU7UUFDekIsK0RBQWEsQ0FBQTtRQUNiLCtEQUFhLENBQUE7UUFDYixpRUFBYyxDQUFBO1FBQ2QscURBQVEsQ0FBQTtRQUNSLDZEQUFZLENBQUE7SUFDZCxDQUFDLEVBTlcsZUFBZSxHQUFmLHVCQUFlLEtBQWYsdUJBQWUsUUFNMUI7SUFFRDs7Ozs7OztPQU9HO0lBQ0gsSUFBWSx3QkFzQlg7SUF0QkQsV0FBWSx3QkFBd0I7UUFDbEM7O1dBRUc7UUFDSCx5RUFBUyxDQUFBO1FBRVQ7Ozs7V0FJRztRQUNILGlGQUFhLENBQUE7UUFFYjs7V0FFRztRQUNILGlHQUFxQixDQUFBO1FBRXJCOztXQUVHO1FBQ0gsNkVBQVcsQ0FBQTtJQUNiLENBQUMsRUF0Qlcsd0JBQXdCLEdBQXhCLGdDQUF3QixLQUF4QixnQ0FBd0IsUUFzQm5DO0lBbUREOztPQUVHO0lBQ0gsU0FBZ0Isc0JBQXNCLENBQUMsSUFBdUI7UUFDNUQsSUFBTSxDQUFDLEdBQUcsQ0FBQyxDQUFDLFFBQVEsQ0FBQyxHQUFHLENBQUMsQ0FBQztRQUMxQixJQUFNLFVB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ICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7U3RhdGljU3ltYm9sfSBmcm9tICcuLi9hb3Qvc3RhdGljX3N5bWJvbCc7XG5pbXBvcnQge0NvbXBpbGVUeXBlTWV0YWRhdGEsIHRva2VuUmVmZXJlbmNlfSBmcm9tICcuLi9jb21waWxlX21ldGFkYXRhJztcbmltcG9ydCB7Q29tcGlsZVJlZmxlY3Rvcn0gZnJvbSAnLi4vY29tcGlsZV9yZWZsZWN0b3InO1xuaW1wb3J0IHtJbmplY3RGbGFnc30gZnJvbSAnLi4vY29yZSc7XG5pbXBvcnQge0lkZW50aWZpZXJzfSBmcm9tICcuLi9pZGVudGlmaWVycyc7XG5pbXBvcnQgKiBhcyBvIGZyb20gJy4uL291dHB1dC9vdXRwdXRfYXN0JztcbmltcG9ydCB7SWRlbnRpZmllcnMgYXMgUjN9IGZyb20gJy4uL3JlbmRlcjMvcjNfaWRlbnRpZmllcnMnO1xuaW1wb3J0IHtPdXRwdXRDb250ZXh0fSBmcm9tICcuLi91dGlsJztcblxuaW1wb3J0IHtSM1JlZmVyZW5jZSwgdHlwZVdpdGhQYXJhbWV0ZXJzfSBmcm9tICcuL3V0aWwnO1xuaW1wb3J0IHt1bnN1cHBvcnRlZH0gZnJvbSAnLi92aWV3L3V0aWwnO1xuXG5cblxuLyoqXG4gKiBNZXRhZGF0YSByZXF1aXJlZCBieSB0aGUgZmFjdG9yeSBnZW5lcmF0b3IgdG8gZ2VuZXJhdGUgYSBgZmFjdG9yeWAgZnVuY3Rpb24gZm9yIGEgdHlwZS5cbiAqL1xuZXhwb3J0IGludGVyZmFjZSBSM0NvbnN0cnVjdG9yRmFjdG9yeU1ldGFkYXRhIHtcbiAgLyoqXG4gICAqIFN0cmluZyBuYW1lIG9mIHRoZSB0eXBlIGJlaW5nIGdlbmVyYXRlZCAodXNlZCB0byBuYW1lIHRoZSBmYWN0b3J5IGZ1bmN0aW9uKS5cbiAgICovXG4gIG5hbWU6IHN0cmluZztcblxuICAvKipcbiAgICogQW4gZXhwcmVzc2lvbiByZXByZXNlbnRpbmcgdGhlIGludGVyZmFjZSB0eXBlIGJlaW5nIGNvbnN0cnVjdGVkLlxuICAgKi9cbiAgdHlwZTogUjNSZWZlcmVuY2U7XG5cbiAgLyoqXG4gICAqIEFuIGV4cHJlc3Npb24gcmVwcmVzZW50aW5nIHRoZSBjb25zdHJ1Y3RvciB0eXBlLCBpbnRlbmRlZCBmb3IgdXNlIHdpdGhpbiBhIGNsYXNzIGRlZmluaXRpb25cbiAgICogaXRzZWxmLlxuICAgKlxuICAgKiBUaGlzIGNhbiBkaWZmZXIgZnJvbSB0aGUgb3V0ZXIgYHR5cGVgIGlmIHRoZSBjbGFzcyBpcyBiZWluZyBjb21waWxlZCBieSBuZ2NjIGFuZCBpcyBpbnNpZGVcbiAgICogYW4gSUlGRSBzdHJ1Y3R1cmUgdGhhdCB1c2VzIGEgZGlmZmVyZW50IG5hbWUgaW50ZXJuYWxseS5cbiAgICovXG4gIGludGVybmFsVHlwZTogby5FeHByZXNzaW9uO1xuXG4gIC8qKiBOdW1iZXIgb2YgYXJndW1lbnRzIGZvciB0aGUgYHR5cGVgLiAqL1xuICB0eXBlQXJndW1lbnRDb3VudDogbnVtYmVyO1xuXG4gIC8qKlxuICAgKiBSZWdhcmRsZXNzIG9mIHdoZXRoZXIgYGZuT3JDbGFzc2AgaXMgYSBjb25zdHJ1Y3RvciBmdW5jdGlvbiBvciBhIHVzZXItZGVmaW5lZCBmYWN0b3J5LCBpdFxuICAgKiBtYXkgaGF2ZSAwIG9yIG1vcmUgcGFyYW1ldGVycywgd2hpY2ggd2lsbCBiZSBpbmplY3RlZCBhY2NvcmRpbmcgdG8gdGhlIGBSM0RlcGVuZGVuY3lNZXRhZGF0YWBcbiAgICogZm9yIHRob3NlIHBhcmFtZXRlcnMuIElmIHRoaXMgaXMgYG51bGxgLCB0aGVuIHRoZSB0eXBlJ3MgY29uc3RydWN0b3IgaXMgbm9uZXhpc3RlbnQgYW5kIHdpbGxcbiAgICogYmUgaW5oZXJpdGVkIGZyb20gYGZuT3JDbGFzc2Agd2hpY2ggaXMgaW50ZXJwcmV0ZWQgYXMgdGhlIGN1cnJlbnQgdHlwZS4gSWYgdGhpcyBpcyBgJ2ludmFsaWQnYCxcbiAgICogdGhlbiBvbmUgb3IgbW9yZSBvZiB0aGUgcGFyYW1ldGVycyB3YXNuJ3QgcmVzb2x2YWJsZSBhbmQgYW55IGF0dGVtcHQgdG8gdXNlIHRoZXNlIGRlcHMgd2lsbFxuICAgKiByZXN1bHQgaW4gYSBydW50aW1lIGVycm9yLlxuICAgKi9cbiAgZGVwczogUjNEZXBlbmRlbmN5TWV0YWRhdGFbXXwnaW52YWxpZCd8bnVsbDtcblxuICAvKipcbiAgICogQW4gZXhwcmVzc2lvbiBmb3IgdGhlIGZ1bmN0aW9uIHdoaWNoIHdpbGwgYmUgdXNlZCB0byBpbmplY3QgZGVwZW5kZW5jaWVzLiBUaGUgQVBJIG9mIHRoaXNcbiAgICogZnVuY3Rpb24gY291bGQgYmUgZGlmZmVyZW50LCBhbmQgb3RoZXIgb3B0aW9ucyBjb250cm9sIGhvdyBpdCB3aWxsIGJlIGludm9rZWQuXG4gICAqL1xuICBpbmplY3RGbjogby5FeHRlcm5hbFJlZmVyZW5jZTtcblxuICAvKipcbiAgICogVHlwZSBvZiB0aGUgdGFyZ2V0IGJlaW5nIGNyZWF0ZWQgYnkgdGhlIGZhY3RvcnkuXG4gICAqL1xuICB0YXJnZXQ6IFIzRmFjdG9yeVRhcmdldDtcbn1cblxuZXhwb3J0IGVudW0gUjNGYWN0b3J5RGVsZWdhdGVUeXBlIHtcbiAgQ2xhc3MsXG4gIEZ1bmN0aW9uLFxuICBGYWN0b3J5LFxufVxuXG5leHBvcnQgaW50ZXJmYWNlIFIzRGVsZWdhdGVkRmFjdG9yeU1ldGFkYXRhIGV4dGVuZHMgUjNDb25zdHJ1Y3RvckZhY3RvcnlNZXRhZGF0YSB7XG4gIGRlbGVnYXRlOiBvLkV4cHJlc3Npb247XG4gIGRlbGVnYXRlVHlwZTogUjNGYWN0b3J5RGVsZWdhdGVUeXBlLkZhY3Rvcnk7XG59XG5cbmV4cG9ydCBpbnRlcmZhY2UgUjNEZWxlZ2F0ZWRGbk9yQ2xhc3NNZXRhZGF0YSBleHRlbmRzIFIzQ29uc3RydWN0b3JGYWN0b3J5TWV0YWRhdGEge1xuICBkZWxlZ2F0ZTogby5FeHByZXNzaW9uO1xuICBkZWxlZ2F0ZVR5cGU6IFIzRmFjdG9yeURlbGVnYXRlVHlwZS5DbGFzc3xSM0ZhY3RvcnlEZWxlZ2F0ZVR5cGUuRnVuY3Rpb247XG4gIGRlbGVnYXRlRGVwczogUjNEZXBlbmRlbmN5TWV0YWRhdGFbXTtcbn1cblxuZXhwb3J0IGludGVyZmFjZSBSM0V4cHJlc3Npb25GYWN0b3J5TWV0YWRhdGEgZXh0ZW5kcyBSM0NvbnN0cnVjdG9yRmFjdG9yeU1ldGFkYXRhIHtcbiAgZXhwcmVzc2lvbjogby5FeHByZXNzaW9uO1xufVxuXG5leHBvcnQgdHlwZSBSM0ZhY3RvcnlNZXRhZGF0YSA9IFIzQ29uc3RydWN0b3JGYWN0b3J5TWV0YWRhdGF8UjNEZWxlZ2F0ZWRGYWN0b3J5TWV0YWRhdGF8XG4gICAgUjNEZWxlZ2F0ZWRGbk9yQ2xhc3NNZXRhZGF0YXxSM0V4cHJlc3Npb25GYWN0b3J5TWV0YWRhdGE7XG5cbmV4cG9ydCBlbnVtIFIzRmFjdG9yeVRhcmdldCB7XG4gIERpcmVjdGl2ZSA9IDAsXG4gIENvbXBvbmVudCA9IDEsXG4gIEluamVjdGFibGUgPSAyLFxuICBQaXBlID0gMyxcbiAgTmdNb2R1bGUgPSA0LFxufVxuXG4vKipcbiAqIFJlc29sdmVkIHR5cGUgb2YgYSBkZXBlbmRlbmN5LlxuICpcbiAqIE9jY2FzaW9uYWxseSwgZGVwZW5kZW5jaWVzIHdpbGwgaGF2ZSBzcGVjaWFsIHNpZ25pZmljYW5jZSB3aGljaCBpcyBrbm93biBzdGF0aWNhbGx5LiBJbiB0aGF0XG4gKiBjYXNlIHRoZSBgUjNSZXNvbHZlZERlcGVuZGVuY3lUeXBlYCBpbmZvcm1zIHRoZSBmYWN0b3J5IGdlbmVyYXRvciB0aGF0IGEgcGFydGljdWxhciBkZXBlbmRlbmN5XG4gKiBzaG91bGQgYmUgZ2VuZXJhdGVkIHNwZWNpYWxseSAodXN1YWxseSBieSBjYWxsaW5nIGEgc3BlY2lhbCBpbmplY3Rpb24gZnVuY3Rpb24gaW5zdGVhZCBvZiB0aGVcbiAqIHN0YW5kYXJkIG9uZSkuXG4gKi9cbmV4cG9ydCBlbnVtIFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZSB7XG4gIC8qKlxuICAgKiBBIG5vcm1hbCB0b2tlbiBkZXBlbmRlbmN5LlxuICAgKi9cbiAgVG9rZW4gPSAwLFxuXG4gIC8qKlxuICAgKiBUaGUgZGVwZW5kZW5jeSBpcyBmb3IgYW4gYXR0cmlidXRlLlxuICAgKlxuICAgKiBUaGUgdG9rZW4gZXhwcmVzc2lvbiBpcyBhIHN0cmluZyByZXByZXNlbnRpbmcgdGhlIGF0dHJpYnV0ZSBuYW1lLlxuICAgKi9cbiAgQXR0cmlidXRlID0gMSxcblxuICAvKipcbiAgICogSW5qZWN0aW5nIHRoZSBgQ2hhbmdlRGV0ZWN0b3JSZWZgIHRva2VuLiBOZWVkcyBzcGVjaWFsIGhhbmRsaW5nIHdoZW4gaW5qZWN0ZWQgaW50byBhIHBpcGUuXG4gICAqL1xuICBDaGFuZ2VEZXRlY3RvclJlZiA9IDIsXG5cbiAgLyoqXG4gICAqIEFuIGludmFsaWQgZGVwZW5kZW5jeSAobm8gdG9rZW4gY291bGQgYmUgZGV0ZXJtaW5lZCkuIEFuIGVycm9yIHNob3VsZCBiZSB0aHJvd24gYXQgcnVudGltZS5cbiAgICovXG4gIEludmFsaWQgPSAzLFxufVxuXG4vKipcbiAqIE1ldGFkYXRhIHJlcHJlc2VudGluZyBhIHNpbmdsZSBkZXBlbmRlbmN5IHRvIGJlIGluamVjdGVkIGludG8gYSBjb25zdHJ1Y3RvciBvciBmdW5jdGlvbiBjYWxsLlxuICovXG5leHBvcnQgaW50ZXJmYWNlIFIzRGVwZW5kZW5jeU1ldGFkYXRhIHtcbiAgLyoqXG4gICAqIEFuIGV4cHJlc3Npb24gcmVwcmVzZW50aW5nIHRoZSB0b2tlbiBvciB2YWx1ZSB0byBiZSBpbmplY3RlZC5cbiAgICovXG4gIHRva2VuOiBvLkV4cHJlc3Npb247XG5cbiAgLyoqXG4gICAqIElmIGFuIEBBdHRyaWJ1dGUgZGVjb3JhdG9yIGlzIHByZXNlbnQsIHRoaXMgaXMgdGhlIGxpdGVyYWwgdHlwZSBvZiB0aGUgYXR0cmlidXRlIG5hbWUsIG9yXG4gICAqIHRoZSB1bmtub3duIHR5cGUgaWYgbm8gbGl0ZXJhbCB0eXBlIGlzIGF2YWlsYWJsZSAoZS5nLiB0aGUgYXR0cmlidXRlIG5hbWUgaXMgYW4gZXhwcmVzc2lvbikuXG4gICAqIFdpbGwgYmUgbnVsbCBvdGhlcndpc2UuXG4gICAqL1xuICBhdHRyaWJ1dGU6IG8uRXhwcmVzc2lvbnxudWxsO1xuXG4gIC8qKlxuICAgKiBBbiBlbnVtIGluZGljYXRpbmcgd2hldGhlciB0aGlzIGRlcGVuZGVuY3kgaGFzIHNwZWNpYWwgbWVhbmluZyB0byBBbmd1bGFyIGFuZCBuZWVkcyB0byBiZVxuICAgKiBpbmplY3RlZCBzcGVjaWFsbHkuXG4gICAqL1xuICByZXNvbHZlZDogUjNSZXNvbHZlZERlcGVuZGVuY3lUeXBlO1xuXG4gIC8qKlxuICAgKiBXaGV0aGVyIHRoZSBkZXBlbmRlbmN5IGhhcyBhbiBASG9zdCBxdWFsaWZpZXIuXG4gICAqL1xuICBob3N0OiBib29sZWFuO1xuXG4gIC8qKlxuICAgKiBXaGV0aGVyIHRoZSBkZXBlbmRlbmN5IGhhcyBhbiBAT3B0aW9uYWwgcXVhbGlmaWVyLlxuICAgKi9cbiAgb3B0aW9uYWw6IGJvb2xlYW47XG5cbiAgLyoqXG4gICAqIFdoZXRoZXIgdGhlIGRlcGVuZGVuY3kgaGFzIGFuIEBTZWxmIHF1YWxpZmllci5cbiAgICovXG4gIHNlbGY6IGJvb2xlYW47XG5cbiAgLyoqXG4gICAqIFdoZXRoZXIgdGhlIGRlcGVuZGVuY3kgaGFzIGFuIEBTa2lwU2VsZiBxdWFsaWZpZXIuXG4gICAqL1xuICBza2lwU2VsZjogYm9vbGVhbjtcbn1cblxuZXhwb3J0IGludGVyZmFjZSBSM0ZhY3RvcnlGbiB7XG4gIGZhY3Rvcnk6IG8uRXhwcmVzc2lvbjtcbiAgc3RhdGVtZW50czogby5TdGF0ZW1lbnRbXTtcbiAgdHlwZTogby5FeHByZXNzaW9uVHlwZTtcbn1cblxuLyoqXG4gKiBDb25zdHJ1Y3QgYSBmYWN0b3J5IGZ1bmN0aW9uIGV4cHJlc3Npb24gZm9yIHRoZSBnaXZlbiBgUjNGYWN0b3J5TWV0YWRhdGFgLlxuICovXG5leHBvcnQgZnVuY3Rpb24gY29tcGlsZUZhY3RvcnlGdW5jdGlvbihtZXRhOiBSM0ZhY3RvcnlNZXRhZGF0YSk6IFIzRmFjdG9yeUZuIHtcbiAgY29uc3QgdCA9IG8udmFyaWFibGUoJ3QnKTtcbiAgY29uc3Qgc3RhdGVtZW50czogby5TdGF0ZW1lbnRbXSA9IFtdO1xuICBsZXQgY3RvckRlcHNUeXBlOiBvLlR5cGUgPSBvLk5PTkVfVFlQRTtcblxuICAvLyBUaGUgdHlwZSB0byBpbnN0YW50aWF0ZSB2aWEgY29uc3RydWN0b3IgaW52b2NhdGlvbi4gSWYgdGhlcmUgaXMgbm8gZGVsZWdhdGVkIGZhY3RvcnksIG1lYW5pbmdcbiAgLy8gdGhpcyB0eXBlIGlzIGFsd2F5cyBjcmVhdGVkIGJ5IGNvbnN0cnVjdG9yIGludm9jYXRpb24sIHRoZW4gdGhpcyBpcyB0aGUgdHlwZS10by1jcmVhdGVcbiAgLy8gcGFyYW1ldGVyIHByb3ZpZGVkIGJ5IHRoZSB1c2VyICh0KSBpZiBzcGVjaWZpZWQsIG9yIHRoZSBjdXJyZW50IHR5cGUgaWYgbm90LiBJZiB0aGVyZSBpcyBhXG4gIC8vIGRlbGVnYXRlZCBmYWN0b3J5ICh3aGljaCBpcyB1c2VkIHRvIGNyZWF0ZSB0aGUgY3VycmVudCB0eXBlKSB0aGVuIHRoaXMgaXMgb25seSB0aGUgdHlwZS10by1cbiAgLy8gY3JlYXRlIHBhcmFtZXRlciAodCkuXG4gIGNvbnN0IHR5cGVGb3JDdG9yID0gIWlzRGVsZWdhdGVkTWV0YWRhdGEobWV0YSkgP1xuICAgICAgbmV3IG8uQmluYXJ5T3BlcmF0b3JFeHByKG8uQmluYXJ5T3BlcmF0b3IuT3IsIHQsIG1ldGEuaW50ZXJuYWxUeXBlKSA6XG4gICAgICB0O1xuXG4gIGxldCBjdG9yRXhwcjogby5FeHByZXNzaW9ufG51bGwgPSBudWxsO1xuICBpZiAobWV0YS5kZXBzICE9PSBudWxsKSB7XG4gICAgLy8gVGhlcmUgaXMgYSBjb25zdHJ1Y3RvciAoZWl0aGVyIGV4cGxpY2l0bHkgb3IgaW1wbGljaXRseSBkZWZpbmVkKS5cbiAgICBpZiAobWV0YS5kZXBzICE9PSAnaW52YWxpZCcpIHtcbiAgICAgIGN0b3JFeHByID0gbmV3IG8uSW5zdGFudGlhdGVFeHByKFxuICAgICAgICAgIHR5cGVGb3JDdG9yLFxuICAgICAgICAgIGluamVjdERlcGVuZGVuY2llcyhtZXRhLmRlcHMsIG1ldGEuaW5qZWN0Rm4sIG1ldGEudGFyZ2V0ID09PSBSM0ZhY3RvcnlUYXJnZXQuUGlwZSkpO1xuXG4gICAgICBjdG9yRGVwc1R5cGUgPSBjcmVhdGVDdG9yRGVwc1R5cGUobWV0YS5kZXBzKTtcbiAgICB9XG4gIH0gZWxzZSB7XG4gICAgY29uc3QgYmFzZUZhY3RvcnkgPSBvLnZhcmlhYmxlKGDJtSR7bWV0YS5uYW1lfV9CYXNlRmFjdG9yeWApO1xuICAgIGNvbnN0IGdldEluaGVyaXRlZEZhY3RvcnkgPSBvLmltcG9ydEV4cHIoUjMuZ2V0SW5oZXJpdGVkRmFjdG9yeSk7XG4gICAgY29uc3QgYmFzZUZhY3RvcnlTdG10ID1cbiAgICAgICAgYmFzZUZhY3Rvcnkuc2V0KGdldEluaGVyaXRlZEZhY3RvcnkuY2FsbEZuKFttZXRhLmludGVybmFsVHlwZV0pKVxuICAgICAgICAgICAgLnRvRGVjbFN0bXQoby5JTkZFUlJFRF9UWVBFLCBbby5TdG10TW9kaWZpZXIuRXhwb3J0ZWQsIG8uU3RtdE1vZGlmaWVyLkZpbmFsXSk7XG4gICAgc3RhdGVtZW50cy5wdXNoKGJhc2VGYWN0b3J5U3RtdCk7XG5cbiAgICAvLyBUaGVyZSBpcyBubyBjb25zdHJ1Y3RvciwgdXNlIHRoZSBiYXNlIGNsYXNzJyBmYWN0b3J5IHRvIGNvbnN0cnVjdCB0eXBlRm9yQ3Rvci5cbiAgICBjdG9yRXhwciA9IGJhc2VGYWN0b3J5LmNhbGxGbihbdHlwZUZvckN0b3JdKTtcbiAgfVxuICBjb25zdCBjdG9yRXhwckZpbmFsID0gY3RvckV4cHI7XG5cbiAgY29uc3QgYm9keTogby5TdGF0ZW1lbnRbXSA9IFtdO1xuICBsZXQgcmV0RXhwcjogby5FeHByZXNzaW9ufG51bGwgPSBudWxsO1xuXG4gIGZ1bmN0aW9uIG1ha2VDb25kaXRpb25hbEZhY3Rvcnkobm9uQ3RvckV4cHI6IG8uRXhwcmVzc2lvbik6IG8uUmVhZFZhckV4cHIge1xuICAgIGNvbnN0IHIgPSBvLnZhcmlhYmxlKCdyJyk7XG4gICAgYm9keS5wdXNoKHIuc2V0KG8uTlVMTF9FWFBSKS50b0RlY2xTdG10KCkpO1xuICAgIGxldCBjdG9yU3RtdDogby5TdGF0ZW1lbnR8bnVsbCA9IG51bGw7XG4gICAgaWYgKGN0b3JFeHByRmluYWwgIT09IG51bGwpIHtcbiAgICAgIGN0b3JTdG10ID0gci5zZXQoY3RvckV4cHJGaW5hbCkudG9TdG10KCk7XG4gICAgfSBlbHNlIHtcbiAgICAgIGN0b3JTdG10ID0gby5pbXBvcnRFeHByKFIzLmludmFsaWRGYWN0b3J5KS5jYWxsRm4oW10pLnRvU3RtdCgpO1xuICAgIH1cbiAgICBib2R5LnB1c2goby5pZlN0bXQodCwgW2N0b3JTdG10XSwgW3Iuc2V0KG5vbkN0b3JFeHByKS50b1N0bXQoKV0pKTtcbiAgICByZXR1cm4gcjtcbiAgfVxuXG4gIGlmIChpc0RlbGVnYXRlZE1ldGFkYXRhKG1ldGEpICYmIG1ldGEuZGVsZWdhdGVUeXBlID09PSBSM0ZhY3RvcnlEZWxlZ2F0ZVR5cGUuRmFjdG9yeSkge1xuICAgIGNvbnN0IGRlbGVnYXRlRmFjdG9yeSA9IG8udmFyaWFibGUoYMm1JHttZXRhLm5hbWV9X0Jhc2VGYWN0b3J5YCk7XG4gICAgY29uc3QgZ2V0RmFjdG9yeU9mID0gby5pbXBvcnRFeHByKFIzLmdldEZhY3RvcnlPZik7XG4gICAgaWYgKG1ldGEuZGVsZWdhdGUuaXNFcXVpdmFsZW50KG1ldGEuaW50ZXJuYWxUeXBlKSkge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKGBJbGxlZ2FsIHN0YXRlOiBjb21waWxpbmcgZmFjdG9yeSB0aGF0IGRlbGVnYXRlcyB0byBpdHNlbGZgKTtcbiAgICB9XG4gICAgY29uc3QgZGVsZWdhdGVGYWN0b3J5U3RtdCA9XG4gICAgICAgIGRlbGVnYXRlRmFjdG9yeS5zZXQoZ2V0RmFjdG9yeU9mLmNhbGxGbihbbWV0YS5kZWxlZ2F0ZV0pKS50b0RlY2xTdG10KG8uSU5GRVJSRURfVFlQRSwgW1xuICAgICAgICAgIG8uU3RtdE1vZGlmaWVyLkV4cG9ydGVkLCBvLlN0bXRNb2RpZmllci5GaW5hbFxuICAgICAgICBdKTtcblxuICAgIHN0YXRlbWVudHMucHVzaChkZWxlZ2F0ZUZhY3RvcnlTdG10KTtcbiAgICByZXRFeHByID0gbWFrZUNvbmRpdGlvbmFsRmFjdG9yeShkZWxlZ2F0ZUZhY3RvcnkuY2FsbEZuKFtdKSk7XG4gIH0gZWxzZSBpZiAoaXNEZWxlZ2F0ZWRNZXRhZGF0YShtZXRhKSkge1xuICAgIC8vIFRoaXMgdHlwZSBpcyBjcmVhdGVkIHdpdGggYSBkZWxlZ2F0ZWQgZmFjdG9yeS4gSWYgYSB0eXBlIHBhcmFtZXRlciBpcyBub3Qgc3BlY2lmaWVkLCBjYWxsXG4gICAgLy8gdGhlIGZhY3RvcnkgaW5zdGVhZC5cbiAgICBjb25zdCBkZWxlZ2F0ZUFyZ3MgPVxuICAgICAgICBpbmplY3REZXBlbmRlbmNpZXMobWV0YS5kZWxlZ2F0ZURlcHMsIG1ldGEuaW5qZWN0Rm4sIG1ldGEudGFyZ2V0ID09PSBSM0ZhY3RvcnlUYXJnZXQuUGlwZSk7XG4gICAgLy8gRWl0aGVyIGNhbGwgYG5ldyBkZWxlZ2F0ZSguLi4pYCBvciBgZGVsZWdhdGUoLi4uKWAgZGVwZW5kaW5nIG9uIG1ldGEuZGVsZWdhdGVUeXBlLlxuICAgIGNvbnN0IGZhY3RvcnlFeHByID0gbmV3IChcbiAgICAgICAgbWV0YS5kZWxlZ2F0ZVR5cGUgPT09IFIzRmFjdG9yeURlbGVnYXRlVHlwZS5DbGFzcyA/XG4gICAgICAgICAgICBvLkluc3RhbnRpYXRlRXhwciA6XG4gICAgICAgICAgICBvLkludm9rZUZ1bmN0aW9uRXhwcikobWV0YS5kZWxlZ2F0ZSwgZGVsZWdhdGVBcmdzKTtcbiAgICByZXRFeHByID0gbWFrZUNvbmRpdGlvbmFsRmFjdG9yeShmYWN0b3J5RXhwcik7XG4gIH0gZWxzZSBpZiAoaXNFeHByZXNzaW9uRmFjdG9yeU1ldGFkYXRhKG1ldGEpKSB7XG4gICAgLy8gVE9ETyhhbHhodWIpOiBkZWNpZGUgd2hldGhlciB0byBsb3dlciB0aGUgdmFsdWUgaGVyZSBvciBpbiB0aGUgY2FsbGVyXG4gICAgcmV0RXhwciA9IG1ha2VDb25kaXRpb25hbEZhY3RvcnkobWV0YS5leHByZXNzaW9uKTtcbiAgfSBlbHNlIHtcbiAgICByZXRFeHByID0gY3RvckV4cHI7XG4gIH1cblxuICBpZiAocmV0RXhwciAhPT0gbnVsbCkge1xuICAgIGJvZHkucHVzaChuZXcgby5SZXR1cm5TdGF0ZW1lbnQocmV0RXhwcikpO1xuICB9IGVsc2Uge1xuICAgIGJvZHkucHVzaChvLmltcG9ydEV4cHIoUjMuaW52YWxpZEZhY3RvcnkpLmNhbGxGbihbXSkudG9TdG10KCkpO1xuICB9XG5cbiAgcmV0dXJuIHtcbiAgICBmYWN0b3J5OiBvLmZuKFxuICAgICAgICBbbmV3IG8uRm5QYXJhbSgndCcsIG8uRFlOQU1JQ19UWVBFKV0sIGJvZHksIG8uSU5GRVJSRURfVFlQRSwgdW5kZWZpbmVkLFxuICAgICAgICBgJHttZXRhLm5hbWV9X0ZhY3RvcnlgKSxcbiAgICBzdGF0ZW1lbnRzLFxuICAgIHR5cGU6IG8uZXhwcmVzc2lvblR5cGUoby5pbXBvcnRFeHByKFxuICAgICAgICBSMy5GYWN0b3J5RGVmLCBbdHlwZVdpdGhQYXJhbWV0ZXJzKG1ldGEudHlwZS50eXBlLCBtZXRhLnR5cGVBcmd1bWVudENvdW50KSwgY3RvckRlcHNUeXBlXSkpXG4gIH07XG59XG5cbmZ1bmN0aW9uIGluamVjdERlcGVuZGVuY2llcyhcbiAgICBkZXBzOiBSM0RlcGVuZGVuY3lNZXRhZGF0YVtdLCBpbmplY3RGbjogby5FeHRlcm5hbFJlZmVyZW5jZSwgaXNQaXBlOiBib29sZWFuKTogby5FeHByZXNzaW9uW10ge1xuICByZXR1cm4gZGVwcy5tYXAoKGRlcCwgaW5kZXgpID0+IGNvbXBpbGVJbmplY3REZXBlbmRlbmN5KGRlcCwgaW5qZWN0Rm4sIGlzUGlwZSwgaW5kZXgpKTtcbn1cblxuZnVuY3Rpb24gY29tcGlsZUluamVjdERlcGVuZGVuY3koXG4gICAgZGVwOiBSM0RlcGVuZGVuY3lNZXRhZGF0YSwgaW5qZWN0Rm46IG8uRXh0ZXJuYWxSZWZlcmVuY2UsIGlzUGlwZTogYm9vbGVhbixcbiAgICBpbmRleDogbnVtYmVyKTogby5FeHByZXNzaW9uIHtcbiAgLy8gSW50ZXJwcmV0IHRoZSBkZXBlbmRlbmN5IGFjY29yZGluZyB0byBpdHMgcmVzb2x2ZWQgdHlwZS5cbiAgc3dpdGNoIChkZXAucmVzb2x2ZWQpIHtcbiAgICBjYXNlIFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZS5Ub2tlbjpcbiAgICBjYXNlIFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZS5DaGFuZ2VEZXRlY3RvclJlZjpcbiAgICAgIC8vIEJ1aWxkIHVwIHRoZSBpbmplY3Rpb24gZmxhZ3MgYWNjb3JkaW5nIHRvIHRoZSBtZXRhZGF0YS5cbiAgICAgIGNvbnN0IGZsYWdzID0gSW5qZWN0RmxhZ3MuRGVmYXVsdCB8IChkZXAuc2VsZiA/IEluamVjdEZsYWdzLlNlbGYgOiAwKSB8XG4gICAgICAgICAgKGRlcC5za2lwU2VsZiA/IEluamVjdEZsYWdzLlNraXBTZWxmIDogMCkgfCAoZGVwLmhvc3QgPyBJbmplY3RGbGFncy5Ib3N0IDogMCkgfFxuICAgICAgICAgIChkZXAub3B0aW9uYWwgPyBJbmplY3RGbGFncy5PcHRpb25hbCA6IDApO1xuXG4gICAgICAvLyBJZiB0aGlzIGRlcGVuZGVuY3kgaXMgb3B0aW9uYWwgb3Igb3RoZXJ3aXNlIGhhcyBub24tZGVmYXVsdCBmbGFncywgdGhlbiBhZGRpdGlvbmFsXG4gICAgICAvLyBwYXJhbWV0ZXJzIGRlc2NyaWJpbmcgaG93IHRvIGluamVjdCB0aGUgZGVwZW5kZW5jeSBtdXN0IGJlIHBhc3NlZCB0byB0aGUgaW5qZWN0IGZ1bmN0aW9uXG4gICAgICAvLyB0aGF0J3MgYmVpbmcgdXNlZC5cbiAgICAgIGxldCBmbGFnc1BhcmFtOiBvLkxpdGVyYWxFeHByfG51bGwgPVxuICAgICAgICAgIChmbGFncyAhPT0gSW5qZWN0RmxhZ3MuRGVmYXVsdCB8fCBkZXAub3B0aW9uYWwpID8gby5saXRlcmFsKGZsYWdzKSA6IG51bGw7XG5cbiAgICAgIC8vIFdlIGhhdmUgYSBzZXBhcmF0ZSBpbnN0cnVjdGlvbiBmb3IgaW5qZWN0aW5nIENoYW5nZURldGVjdG9yUmVmIGludG8gYSBwaXBlLlxuICAgICAgaWYgKGlzUGlwZSAmJiBkZXAucmVzb2x2ZWQgPT09IFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZS5DaGFuZ2VEZXRlY3RvclJlZikge1xuICAgICAgICByZXR1cm4gby5pbXBvcnRFeHByKFIzLmluamVjdFBpcGVDaGFuZ2VEZXRlY3RvclJlZikuY2FsbEZuKGZsYWdzUGFyYW0gPyBbZmxhZ3NQYXJhbV0gOiBbXSk7XG4gICAgICB9XG5cbiAgICAgIC8vIEJ1aWxkIHVwIHRoZSBhcmd1bWVudHMgdG8gdGhlIGluamVjdEZuIGNhbGwuXG4gICAgICBjb25zdCBpbmplY3RBcmdzID0gW2RlcC50b2tlbl07XG4gICAgICBpZiAoZmxhZ3NQYXJhbSkge1xuICAgICAgICBpbmplY3RBcmdzLnB1c2goZmxhZ3NQYXJhbSk7XG4gICAgICB9XG4gICAgICByZXR1cm4gby5pbXBvcnRFeHByKGluamVjdEZuKS5jYWxsRm4oaW5qZWN0QXJncyk7XG4gICAgY2FzZSBSM1Jlc29sdmVkRGVwZW5kZW5jeVR5cGUuQXR0cmlidXRlOlxuICAgICAgLy8gSW4gdGhlIGNhc2Ugb2YgYXR0cmlidXRlcywgdGhlIGF0dHJpYnV0ZSBuYW1lIGluIHF1ZXN0aW9uIGlzIGdpdmVuIGFzIHRoZSB0b2tlbi5cbiAgICAgIHJldHVybiBvLmltcG9ydEV4cHIoUjMuaW5qZWN0QXR0cmlidXRlKS5jYWxsRm4oW2RlcC50b2tlbl0pO1xuICAgIGNhc2UgUjNSZXNvbHZlZERlcGVuZGVuY3lUeXBlLkludmFsaWQ6XG4gICAgICByZXR1cm4gby5pbXBvcnRFeHByKFIzLmludmFsaWRGYWN0b3J5RGVwKS5jYWxsRm4oW28ubGl0ZXJhbChpbmRleCldKTtcbiAgICBkZWZhdWx0OlxuICAgICAgcmV0dXJuIHVuc3VwcG9ydGVkKFxuICAgICAgICAgIGBVbmtub3duIFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZTogJHtSM1Jlc29sdmVkRGVwZW5kZW5jeVR5cGVbZGVwLnJlc29sdmVkXX1gKTtcbiAgfVxufVxuXG5mdW5jdGlvbiBjcmVhdGVDdG9yRGVwc1R5cGUoZGVwczogUjNEZXBlbmRlbmN5TWV0YWRhdGFbXSk6IG8uVHlwZSB7XG4gIGxldCBoYXNUeXBlcyA9IGZhbHNlO1xuICBjb25zdCBhdHRyaWJ1dGVUeXBlcyA9IGRlcHMubWFwKGRlcCA9PiB7XG4gICAgY29uc3QgdHlwZSA9IGNyZWF0ZUN0b3JEZXBUeXBlKGRlcCk7XG4gICAgaWYgKHR5cGUgIT09IG51bGwpIHtcbiAgICAgIGhhc1R5cGVzID0gdHJ1ZTtcbiAgICAgIHJldHVybiB0eXBlO1xuICAgIH0gZWxzZSB7XG4gICAgICByZXR1cm4gby5saXRlcmFsKG51bGwpO1xuICAgIH1cbiAgfSk7XG5cbiAgaWYgKGhhc1R5cGVzKSB7XG4gICAgcmV0dXJuIG8uZXhwcmVzc2lvblR5cGUoby5saXRlcmFsQXJyKGF0dHJpYnV0ZVR5cGVzKSk7XG4gIH0gZWxzZSB7XG4gICAgcmV0dXJuIG8uTk9ORV9UWVBFO1xuICB9XG59XG5cbmZ1bmN0aW9uIGNyZWF0ZUN0b3JEZXBUeXBlKGRlcDogUjNEZXBlbmRlbmN5TWV0YWRhdGEpOiBvLkxpdGVyYWxNYXBFeHByfG51bGwge1xuICBjb25zdCBlbnRyaWVzOiB7a2V5OiBzdHJpbmcsIHF1b3RlZDogYm9vbGVhbiwgdmFsdWU6IG8uRXhwcmVzc2lvbn1bXSA9IFtdO1xuXG4gIGlmIChkZXAucmVzb2x2ZWQgPT09IFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZS5BdHRyaWJ1dGUpIHtcbiAgICBpZiAoZGVwLmF0dHJpYnV0ZSAhPT0gbnVsbCkge1xuICAgICAgZW50cmllcy5wdXNoKHtrZXk6ICdhdHRyaWJ1dGUnLCB2YWx1ZTogZGVwLmF0dHJpYnV0ZSwgcXVvdGVkOiBmYWxzZX0pO1xuICAgIH1cbiAgfVxuICBpZiAoZGVwLm9wdGlvbmFsKSB7XG4gICAgZW50cmllcy5wdXNoKHtrZXk6ICdvcHRpb25hbCcsIHZhbHVlOiBvLmxpdGVyYWwodHJ1ZSksIHF1b3RlZDogZmFsc2V9KTtcbiAgfVxuICBpZiAoZGVwLmhvc3QpIHtcbiAgICBlbnRyaWVzLnB1c2goe2tleTogJ2hvc3QnLCB2YWx1ZTogby5saXRlcmFsKHRydWUpLCBxdW90ZWQ6IGZhbHNlfSk7XG4gIH1cbiAgaWYgKGRlcC5zZWxmKSB7XG4gICAgZW50cmllcy5wdXNoKHtrZXk6ICdzZWxmJywgdmFsdWU6IG8ubGl0ZXJhbCh0cnVlKSwgcXVvdGVkOiBmYWxzZX0pO1xuICB9XG4gIGlmIChkZXAuc2tpcFNlbGYpIHtcbiAgICBlbnRyaWVzLnB1c2goe2tleTogJ3NraXBTZWxmJywgdmFsdWU6IG8ubGl0ZXJhbCh0cnVlKSwgcXVvdGVkOiBmYWxzZX0pO1xuICB9XG5cbiAgcmV0dXJuIGVudHJpZXMubGVuZ3RoID4gMCA/IG8ubGl0ZXJhbE1hcChlbnRyaWVzKSA6IG51bGw7XG59XG5cbi8qKlxuICogQSBoZWxwZXIgZnVuY3Rpb24gdXNlZnVsIGZvciBleHRyYWN0aW5nIGBSM0RlcGVuZGVuY3lNZXRhZGF0YWAgZnJvbSBhIFJlbmRlcjJcbiAqIGBDb21waWxlVHlwZU1ldGFkYXRhYCBpbnN0YW5jZS5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGRlcGVuZGVuY2llc0Zyb21HbG9iYWxNZXRhZGF0YShcbiAgICB0eXBlOiBDb21waWxlVHlwZU1ldGFkYXRhLCBvdXRwdXRDdHg6IE91dHB1dENvbnRleHQsXG4gICAgcmVmbGVjdG9yOiBDb21waWxlUmVmbGVjdG9yKTogUjNEZXBlbmRlbmN5TWV0YWRhdGFbXSB7XG4gIC8vIFVzZSB0aGUgYENvbXBpbGVSZWZsZWN0b3JgIHRvIGxvb2sgdXAgcmVmZXJlbmNlcyB0byBzb21lIHdlbGwta25vd24gQW5ndWxhciB0eXBlcy4gVGhlc2Ugd2lsbFxuICAvLyBiZSBjb21wYXJlZCB3aXRoIHRoZSB0b2tlbiB0byBzdGF0aWNhbGx5IGRldGVybWluZSB3aGV0aGVyIHRoZSB0b2tlbiBoYXMgc2lnbmlmaWNhbmNlIHRvXG4gIC8vIEFuZ3VsYXIsIGFuZCBzZXQgdGhlIGNvcnJlY3QgYFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZWAgYXMgYSByZXN1bHQuXG4gIGNvbnN0IGluamVjdG9yUmVmID0gcmVmbGVjdG9yLnJlc29sdmVFeHRlcm5hbFJlZmVyZW5jZShJZGVudGlmaWVycy5JbmplY3Rvcik7XG5cbiAgLy8gSXRlcmF0ZSB0aHJvdWdoIHRoZSB0eXBlJ3MgREkgZGVwZW5kZW5jaWVzIGFuZCBwcm9kdWNlIGBSM0RlcGVuZGVuY3lNZXRhZGF0YWAgZm9yIGVhY2ggb2YgdGhlbS5cbiAgY29uc3QgZGVwczogUjNEZXBlbmRlbmN5TWV0YWRhdGFbXSA9IFtdO1xuICBmb3IgKGxldCBkZXBlbmRlbmN5IG9mIHR5cGUuZGlEZXBzKSB7XG4gICAgaWYgKGRlcGVuZGVuY3kudG9rZW4pIHtcbiAgICAgIGNvbnN0IHRva2VuUmVmID0gdG9rZW5SZWZlcmVuY2UoZGVwZW5kZW5jeS50b2tlbik7XG4gICAgICBsZXQgcmVzb2x2ZWQ6IFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZSA9IGRlcGVuZGVuY3kuaXNBdHRyaWJ1dGUgP1xuICAgICAgICAgIFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZS5BdHRyaWJ1dGUgOlxuICAgICAgICAgIFIzUmVzb2x2ZWREZXBlbmRlbmN5VHlwZS5Ub2tlbjtcblxuICAgICAgLy8gSW4gdGhlIGNhc2Ugb2YgbW9zdCBkZXBlbmRlbmNpZXMsIHRoZSB0b2tlbiB3aWxsIGJlIGEgcmVmZXJlbmNlIHRvIGEgdHlwZS4gU29tZXRpbWVzLFxuICAgICAgLy8gaG93ZXZlciwgaXQgY2FuIGJlIGEgc3RyaW5nLCBpbiB0aGUgY2FzZSBvZiBvbGRlciBBbmd1bGFyIGNvZGUgb3IgQEF0dHJpYnV0ZSBpbmplY3Rpb24uXG4gICAgICBjb25zdCB0b2tlbiA9XG4gICAgICAgICAgdG9rZW5SZWYgaW5zdGFuY2VvZiBTdGF0aWNTeW1ib2wgPyBvdXRwdXRDdHguaW1wb3J0RXhwcih0b2tlblJlZikgOiBvLmxpdGVyYWwodG9rZW5SZWYpO1xuXG4gICAgICAvLyBDb25zdHJ1Y3QgdGhlIGRlcGVuZGVuY3kuXG4gICAgICBkZXBzLnB1c2goe1xuICAgICAgICB0b2tlbixcbiAgICAgICAgYXR0cmlidXRlOiBudWxsLFxuICAgICAgICByZXNvbHZlZCxcbiAgICAgICAgaG9zdDogISFkZXBlbmRlbmN5LmlzSG9zdCxcbiAgICAgICAgb3B0aW9uYWw6ICEhZGVwZW5kZW5jeS5pc09wdGlvbmFsLFxuICAgICAgICBzZWxmOiAhIWRlcGVuZGVuY3kuaXNTZWxmLFxuICAgICAgICBza2lwU2VsZjogISFkZXBlbmRlbmN5LmlzU2tpcFNlbGYsXG4gICAgICB9KTtcbiAgICB9IGVsc2Uge1xuICAgICAgdW5zdXBwb3J0ZWQoJ2RlcGVuZGVuY3kgd2l0aG91dCBhIHRva2VuJyk7XG4gICAgfVxuICB9XG5cbiAgcmV0dXJuIGRlcHM7XG59XG5cbmZ1bmN0aW9uIGlzRGVsZWdhdGVkTWV0YWRhdGEobWV0YTogUjNGYWN0b3J5TWV0YWRhdGEpOiBtZXRhIGlzIFIzRGVsZWdhdGVkRmFjdG9yeU1ldGFkYXRhfFxuICAgIFIzRGVsZWdhdGVkRm5PckNsYXNzTWV0YWRhdGEge1xuICByZXR1cm4gKG1ldGEgYXMgYW55KS5kZWxlZ2F0ZVR5cGUgIT09IHVuZGVmaW5lZDtcbn1cblxuZnVuY3Rpb24gaXNFeHByZXNzaW9uRmFjdG9yeU1ldGFkYXRhKG1ldGE6IFIzRmFjdG9yeU1ldGFkYXRhKTogbWV0YSBpcyBSM0V4cHJlc3Npb25GYWN0b3J5TWV0YWRhdGEge1xuICByZXR1cm4gKG1ldGEgYXMgYW55KS5leHByZXNzaW9uICE9PSB1bmRlZmluZWQ7XG59XG4iXX0=