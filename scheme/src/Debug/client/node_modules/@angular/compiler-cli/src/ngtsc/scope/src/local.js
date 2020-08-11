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
        define("@angular/compiler-cli/src/ngtsc/scope/src/local", ["require", "exports", "tslib", "@angular/compiler", "typescript", "@angular/compiler-cli/src/ngtsc/diagnostics", "@angular/compiler-cli/src/ngtsc/util/src/typescript"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.LocalModuleScopeRegistry = void 0;
    var tslib_1 = require("tslib");
    var compiler_1 = require("@angular/compiler");
    var ts = require("typescript");
    var diagnostics_1 = require("@angular/compiler-cli/src/ngtsc/diagnostics");
    var typescript_1 = require("@angular/compiler-cli/src/ngtsc/util/src/typescript");
    /**
     * A registry which collects information about NgModules, Directives, Components, and Pipes which
     * are local (declared in the ts.Program being compiled), and can produce `LocalModuleScope`s
     * which summarize the compilation scope of a component.
     *
     * This class implements the logic of NgModule declarations, imports, and exports and can produce,
     * for a given component, the set of directives and pipes which are "visible" in that component's
     * template.
     *
     * The `LocalModuleScopeRegistry` has two "modes" of operation. During analysis, data for each
     * individual NgModule, Directive, Component, and Pipe is added to the registry. No attempt is made
     * to traverse or validate the NgModule graph (imports, exports, etc). After analysis, one of
     * `getScopeOfModule` or `getScopeForComponent` can be called, which traverses the NgModule graph
     * and applies the NgModule logic to generate a `LocalModuleScope`, the full scope for the given
     * module or component.
     *
     * The `LocalModuleScopeRegistry` is also capable of producing `ts.Diagnostic` errors when Angular
     * semantics are violated.
     */
    var LocalModuleScopeRegistry = /** @class */ (function () {
        function LocalModuleScopeRegistry(localReader, dependencyScopeReader, refEmitter, aliasingHost) {
            this.localReader = localReader;
            this.dependencyScopeReader = dependencyScopeReader;
            this.refEmitter = refEmitter;
            this.aliasingHost = aliasingHost;
            /**
             * Tracks whether the registry has been asked to produce scopes for a module or component. Once
             * this is true, the registry cannot accept registrations of new directives/pipes/modules as it
             * would invalidate the cached scope data.
             */
            this.sealed = false;
            /**
             * A map of components from the current compilation unit to the NgModule which declared them.
             *
             * As components and directives are not distinguished at the NgModule level, this map may also
             * contain directives. This doesn't cause any problems but isn't useful as there is no concept of
             * a directive's compilation scope.
             */
            this.declarationToModule = new Map();
            /**
             * This maps from the directive/pipe class to a map of data for each NgModule that declares the
             * directive/pipe. This data is needed to produce an error for the given class.
             */
            this.duplicateDeclarations = new Map();
            this.moduleToRef = new Map();
            /**
             * A cache of calculated `LocalModuleScope`s for each NgModule declared in the current program.
             *
             * A value of `undefined` indicates the scope was invalid and produced errors (therefore,
             * diagnostics should exist in the `scopeErrors` map).
             */
            this.cache = new Map();
            /**
             * Tracks whether a given component requires "remote scoping".
             *
             * Remote scoping is when the set of directives which apply to a given component is set in the
             * NgModule's file instead of directly on the component def (which is sometimes needed to get
             * around cyclic import issues). This is not used in calculation of `LocalModuleScope`s, but is
             * tracked here for convenience.
             */
            this.remoteScoping = new Set();
            /**
             * Tracks errors accumulated in the processing of scopes for each module declaration.
             */
            this.scopeErrors = new Map();
            /**
             * Tracks which NgModules are unreliable due to errors within their declarations.
             *
             * This provides a unified view of which modules have errors, across all of the different
             * diagnostic categories that can be produced. Theoretically this can be inferred from the other
             * properties of this class, but is tracked explicitly to simplify the logic.
             */
            this.taintedModules = new Set();
        }
        /**
         * Add an NgModule's data to the registry.
         */
        LocalModuleScopeRegistry.prototype.registerNgModuleMetadata = function (data) {
            var e_1, _a;
            this.assertCollecting();
            var ngModule = data.ref.node;
            this.moduleToRef.set(data.ref.node, data.ref);
            try {
                // Iterate over the module's declarations, and add them to declarationToModule. If duplicates
                // are found, they're instead tracked in duplicateDeclarations.
                for (var _b = tslib_1.__values(data.declarations), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var decl = _c.value;
                    this.registerDeclarationOfModule(ngModule, decl, data.rawDeclarations);
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                }
                finally { if (e_1) throw e_1.error; }
            }
        };
        LocalModuleScopeRegistry.prototype.registerDirectiveMetadata = function (directive) { };
        LocalModuleScopeRegistry.prototype.registerPipeMetadata = function (pipe) { };
        LocalModuleScopeRegistry.prototype.getScopeForComponent = function (clazz) {
            var scope = !this.declarationToModule.has(clazz) ?
                null :
                this.getScopeOfModule(this.declarationToModule.get(clazz).ngModule);
            return scope;
        };
        /**
         * If `node` is declared in more than one NgModule (duplicate declaration), then get the
         * `DeclarationData` for each offending declaration.
         *
         * Ordinarily a class is only declared in one NgModule, in which case this function returns
         * `null`.
         */
        LocalModuleScopeRegistry.prototype.getDuplicateDeclarations = function (node) {
            if (!this.duplicateDeclarations.has(node)) {
                return null;
            }
            return Array.from(this.duplicateDeclarations.get(node).values());
        };
        /**
         * Collects registered data for a module and its directives/pipes and convert it into a full
         * `LocalModuleScope`.
         *
         * This method implements the logic of NgModule imports and exports. It returns the
         * `LocalModuleScope` for the given NgModule if one can be produced, `null` if no scope was ever
         * defined, or the string `'error'` if the scope contained errors.
         */
        LocalModuleScopeRegistry.prototype.getScopeOfModule = function (clazz) {
            var scope = this.moduleToRef.has(clazz) ?
                this.getScopeOfModuleReference(this.moduleToRef.get(clazz)) :
                null;
            // If the NgModule class is marked as tainted, consider it an error.
            if (this.taintedModules.has(clazz)) {
                return 'error';
            }
            // Translate undefined -> 'error'.
            return scope !== undefined ? scope : 'error';
        };
        /**
         * Retrieves any `ts.Diagnostic`s produced during the calculation of the `LocalModuleScope` for
         * the given NgModule, or `null` if no errors were present.
         */
        LocalModuleScopeRegistry.prototype.getDiagnosticsOfModule = function (clazz) {
            // Required to ensure the errors are populated for the given class. If it has been processed
            // before, this will be a no-op due to the scope cache.
            this.getScopeOfModule(clazz);
            if (this.scopeErrors.has(clazz)) {
                return this.scopeErrors.get(clazz);
            }
            else {
                return null;
            }
        };
        /**
         * Returns a collection of the compilation scope for each registered declaration.
         */
        LocalModuleScopeRegistry.prototype.getCompilationScopes = function () {
            var _this = this;
            var scopes = [];
            this.declarationToModule.forEach(function (declData, declaration) {
                var scope = _this.getScopeOfModule(declData.ngModule);
                if (scope !== null && scope !== 'error') {
                    scopes.push(tslib_1.__assign({ declaration: declaration, ngModule: declData.ngModule }, scope.compilation));
                }
            });
            return scopes;
        };
        LocalModuleScopeRegistry.prototype.registerDeclarationOfModule = function (ngModule, decl, rawDeclarations) {
            var declData = {
                ngModule: ngModule,
                ref: decl,
                rawDeclarations: rawDeclarations,
            };
            // First, check for duplicate declarations of the same directive/pipe.
            if (this.duplicateDeclarations.has(decl.node)) {
                // This directive/pipe has already been identified as being duplicated. Add this module to the
                // map of modules for which a duplicate declaration exists.
                this.duplicateDeclarations.get(decl.node).set(ngModule, declData);
            }
            else if (this.declarationToModule.has(decl.node) &&
                this.declarationToModule.get(decl.node).ngModule !== ngModule) {
                // This directive/pipe is already registered as declared in another module. Mark it as a
                // duplicate instead.
                var duplicateDeclMap = new Map();
                var firstDeclData = this.declarationToModule.get(decl.node);
                // Mark both modules as tainted, since their declarations are missing a component.
                this.taintedModules.add(firstDeclData.ngModule);
                this.taintedModules.add(ngModule);
                // Being detected as a duplicate means there are two NgModules (for now) which declare this
                // directive/pipe. Add both of them to the duplicate tracking map.
                duplicateDeclMap.set(firstDeclData.ngModule, firstDeclData);
                duplicateDeclMap.set(ngModule, declData);
                this.duplicateDeclarations.set(decl.node, duplicateDeclMap);
                // Remove the directive/pipe from `declarationToModule` as it's a duplicate declaration, and
                // therefore not valid.
                this.declarationToModule.delete(decl.node);
            }
            else {
                // This is the first declaration of this directive/pipe, so map it.
                this.declarationToModule.set(decl.node, declData);
            }
        };
        /**
         * Implementation of `getScopeOfModule` which accepts a reference to a class and differentiates
         * between:
         *
         * * no scope being available (returns `null`)
         * * a scope being produced with errors (returns `undefined`).
         */
        LocalModuleScopeRegistry.prototype.getScopeOfModuleReference = function (ref) {
            var e_2, _a, e_3, _b, e_4, _c, e_5, _d, e_6, _e, e_7, _f, e_8, _g, e_9, _h, e_10, _j;
            if (this.cache.has(ref.node)) {
                return this.cache.get(ref.node);
            }
            // Seal the registry to protect the integrity of the `LocalModuleScope` cache.
            this.sealed = true;
            // `ref` should be an NgModule previously added to the registry. If not, a scope for it
            // cannot be produced.
            var ngModule = this.localReader.getNgModuleMetadata(ref);
            if (ngModule === null) {
                this.cache.set(ref.node, null);
                return null;
            }
            // Modules which contributed to the compilation scope of this module.
            var compilationModules = new Set([ngModule.ref.node]);
            // Modules which contributed to the export scope of this module.
            var exportedModules = new Set([ngModule.ref.node]);
            // Errors produced during computation of the scope are recorded here. At the end, if this array
            // isn't empty then `undefined` will be cached and returned to indicate this scope is invalid.
            var diagnostics = [];
            // At this point, the goal is to produce two distinct transitive sets:
            // - the directives and pipes which are visible to components declared in the NgModule.
            // - the directives and pipes which are exported to any NgModules which import this one.
            // Directives and pipes in the compilation scope.
            var compilationDirectives = new Map();
            var compilationPipes = new Map();
            var declared = new Set();
            // Directives and pipes exported to any importing NgModules.
            var exportDirectives = new Map();
            var exportPipes = new Map();
            try {
                // The algorithm is as follows:
                // 1) Add all of the directives/pipes from each NgModule imported into the current one to the
                //    compilation scope.
                // 2) Add directives/pipes declared in the NgModule to the compilation scope. At this point, the
                //    compilation scope is complete.
                // 3) For each entry in the NgModule's exports:
                //    a) Attempt to resolve it as an NgModule with its own exported directives/pipes. If it is
                //       one, add them to the export scope of this NgModule.
                //    b) Otherwise, it should be a class in the compilation scope of this NgModule. If it is,
                //       add it to the export scope.
                //    c) If it's neither an NgModule nor a directive/pipe in the compilation scope, then this
                //       is an error.
                // 1) process imports.
                for (var _k = tslib_1.__values(ngModule.imports), _l = _k.next(); !_l.done; _l = _k.next()) {
                    var decl = _l.value;
                    var importScope = this.getExportedScope(decl, diagnostics, ref.node, 'import');
                    if (importScope === null) {
                        // An import wasn't an NgModule, so record an error.
                        diagnostics.push(invalidRef(ref.node, decl, 'import'));
                        continue;
                    }
                    else if (importScope === undefined) {
                        // An import was an NgModule but contained errors of its own. Record this as an error too,
                        // because this scope is always going to be incorrect if one of its imports could not be
                        // read.
                        diagnostics.push(invalidTransitiveNgModuleRef(ref.node, decl, 'import'));
                        continue;
                    }
                    try {
                        for (var _m = (e_3 = void 0, tslib_1.__values(importScope.exported.directives)), _o = _m.next(); !_o.done; _o = _m.next()) {
                            var directive = _o.value;
                            compilationDirectives.set(directive.ref.node, directive);
                        }
                    }
                    catch (e_3_1) { e_3 = { error: e_3_1 }; }
                    finally {
                        try {
                            if (_o && !_o.done && (_b = _m.return)) _b.call(_m);
                        }
                        finally { if (e_3) throw e_3.error; }
                    }
                    try {
                        for (var _p = (e_4 = void 0, tslib_1.__values(importScope.exported.pipes)), _q = _p.next(); !_q.done; _q = _p.next()) {
                            var pipe = _q.value;
                            compilationPipes.set(pipe.ref.node, pipe);
                        }
                    }
                    catch (e_4_1) { e_4 = { error: e_4_1 }; }
                    finally {
                        try {
                            if (_q && !_q.done && (_c = _p.return)) _c.call(_p);
                        }
                        finally { if (e_4) throw e_4.error; }
                    }
                    try {
                        for (var _r = (e_5 = void 0, tslib_1.__values(importScope.exported.ngModules)), _s = _r.next(); !_s.done; _s = _r.next()) {
                            var importedModule = _s.value;
                            compilationModules.add(importedModule);
                        }
                    }
                    catch (e_5_1) { e_5 = { error: e_5_1 }; }
                    finally {
                        try {
                            if (_s && !_s.done && (_d = _r.return)) _d.call(_r);
                        }
                        finally { if (e_5) throw e_5.error; }
                    }
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (_l && !_l.done && (_a = _k.return)) _a.call(_k);
                }
                finally { if (e_2) throw e_2.error; }
            }
            try {
                // 2) add declarations.
                for (var _t = tslib_1.__values(ngModule.declarations), _u = _t.next(); !_u.done; _u = _t.next()) {
                    var decl = _u.value;
                    var directive = this.localReader.getDirectiveMetadata(decl);
                    var pipe = this.localReader.getPipeMetadata(decl);
                    if (directive !== null) {
                        compilationDirectives.set(decl.node, tslib_1.__assign(tslib_1.__assign({}, directive), { ref: decl }));
                    }
                    else if (pipe !== null) {
                        compilationPipes.set(decl.node, tslib_1.__assign(tslib_1.__assign({}, pipe), { ref: decl }));
                    }
                    else {
                        this.taintedModules.add(ngModule.ref.node);
                        var errorNode = decl.getOriginForDiagnostics(ngModule.rawDeclarations);
                        diagnostics.push(diagnostics_1.makeDiagnostic(diagnostics_1.ErrorCode.NGMODULE_INVALID_DECLARATION, errorNode, "The class '" + decl.node.name.text + "' is listed in the declarations " +
                            ("of the NgModule '" + ngModule.ref.node.name
                                .text + "', but is not a directive, a component, or a pipe. ") +
                            "Either remove it from the NgModule's declarations, or add an appropriate Angular decorator.", [{ node: decl.node.name, messageText: "'" + decl.node.name.text + "' is declared here." }]));
                        continue;
                    }
                    declared.add(decl.node);
                }
            }
            catch (e_6_1) { e_6 = { error: e_6_1 }; }
            finally {
                try {
                    if (_u && !_u.done && (_e = _t.return)) _e.call(_t);
                }
                finally { if (e_6) throw e_6.error; }
            }
            try {
                // 3) process exports.
                // Exports can contain modules, components, or directives. They're processed differently.
                // Modules are straightforward. Directives and pipes from exported modules are added to the
                // export maps. Directives/pipes are different - they might be exports of declared types or
                // imported types.
                for (var _v = tslib_1.__values(ngModule.exports), _w = _v.next(); !_w.done; _w = _v.next()) {
                    var decl = _w.value;
                    // Attempt to resolve decl as an NgModule.
                    var importScope = this.getExportedScope(decl, diagnostics, ref.node, 'export');
                    if (importScope === undefined) {
                        // An export was an NgModule but contained errors of its own. Record this as an error too,
                        // because this scope is always going to be incorrect if one of its exports could not be
                        // read.
                        diagnostics.push(invalidTransitiveNgModuleRef(ref.node, decl, 'export'));
                        continue;
                    }
                    else if (importScope !== null) {
                        try {
                            // decl is an NgModule.
                            for (var _x = (e_8 = void 0, tslib_1.__values(importScope.exported.directives)), _y = _x.next(); !_y.done; _y = _x.next()) {
                                var directive = _y.value;
                                exportDirectives.set(directive.ref.node, directive);
                            }
                        }
                        catch (e_8_1) { e_8 = { error: e_8_1 }; }
                        finally {
                            try {
                                if (_y && !_y.done && (_g = _x.return)) _g.call(_x);
                            }
                            finally { if (e_8) throw e_8.error; }
                        }
                        try {
                            for (var _z = (e_9 = void 0, tslib_1.__values(importScope.exported.pipes)), _0 = _z.next(); !_0.done; _0 = _z.next()) {
                                var pipe = _0.value;
                                exportPipes.set(pipe.ref.node, pipe);
                            }
                        }
                        catch (e_9_1) { e_9 = { error: e_9_1 }; }
                        finally {
                            try {
                                if (_0 && !_0.done && (_h = _z.return)) _h.call(_z);
                            }
                            finally { if (e_9) throw e_9.error; }
                        }
                        try {
                            for (var _1 = (e_10 = void 0, tslib_1.__values(importScope.exported.ngModules)), _2 = _1.next(); !_2.done; _2 = _1.next()) {
                                var exportedModule = _2.value;
                                exportedModules.add(exportedModule);
                            }
                        }
                        catch (e_10_1) { e_10 = { error: e_10_1 }; }
                        finally {
                            try {
                                if (_2 && !_2.done && (_j = _1.return)) _j.call(_1);
                            }
                            finally { if (e_10) throw e_10.error; }
                        }
                    }
                    else if (compilationDirectives.has(decl.node)) {
                        // decl is a directive or component in the compilation scope of this NgModule.
                        var directive = compilationDirectives.get(decl.node);
                        exportDirectives.set(decl.node, directive);
                    }
                    else if (compilationPipes.has(decl.node)) {
                        // decl is a pipe in the compilation scope of this NgModule.
                        var pipe = compilationPipes.get(decl.node);
                        exportPipes.set(decl.node, pipe);
                    }
                    else {
                        // decl is an unknown export.
                        if (this.localReader.getDirectiveMetadata(decl) !== null ||
                            this.localReader.getPipeMetadata(decl) !== null) {
                            diagnostics.push(invalidReexport(ref.node, decl));
                        }
                        else {
                            diagnostics.push(invalidRef(ref.node, decl, 'export'));
                        }
                        continue;
                    }
                }
            }
            catch (e_7_1) { e_7 = { error: e_7_1 }; }
            finally {
                try {
                    if (_w && !_w.done && (_f = _v.return)) _f.call(_v);
                }
                finally { if (e_7) throw e_7.error; }
            }
            var exported = {
                directives: Array.from(exportDirectives.values()),
                pipes: Array.from(exportPipes.values()),
                ngModules: Array.from(exportedModules),
            };
            var reexports = this.getReexports(ngModule, ref, declared, exported, diagnostics);
            // Check if this scope had any errors during production.
            if (diagnostics.length > 0) {
                // Cache undefined, to mark the fact that the scope is invalid.
                this.cache.set(ref.node, undefined);
                // Save the errors for retrieval.
                this.scopeErrors.set(ref.node, diagnostics);
                // Mark this module as being tainted.
                this.taintedModules.add(ref.node);
                return undefined;
            }
            // Finally, produce the `LocalModuleScope` with both the compilation and export scopes.
            var scope = {
                compilation: {
                    directives: Array.from(compilationDirectives.values()),
                    pipes: Array.from(compilationPipes.values()),
                    ngModules: Array.from(compilationModules),
                },
                exported: exported,
                reexports: reexports,
                schemas: ngModule.schemas,
            };
            this.cache.set(ref.node, scope);
            return scope;
        };
        /**
         * Check whether a component requires remote scoping.
         */
        LocalModuleScopeRegistry.prototype.getRequiresRemoteScope = function (node) {
            return this.remoteScoping.has(node);
        };
        /**
         * Set a component as requiring remote scoping.
         */
        LocalModuleScopeRegistry.prototype.setComponentAsRequiringRemoteScoping = function (node) {
            this.remoteScoping.add(node);
        };
        /**
         * Look up the `ExportScope` of a given `Reference` to an NgModule.
         *
         * The NgModule in question may be declared locally in the current ts.Program, or it may be
         * declared in a .d.ts file.
         *
         * @returns `null` if no scope could be found, or `undefined` if an invalid scope
         * was found.
         *
         * May also contribute diagnostics of its own by adding to the given `diagnostics`
         * array parameter.
         */
        LocalModuleScopeRegistry.prototype.getExportedScope = function (ref, diagnostics, ownerForErrors, type) {
            if (ref.node.getSourceFile().isDeclarationFile) {
                // The NgModule is declared in a .d.ts file. Resolve it with the `DependencyScopeReader`.
                if (!ts.isClassDeclaration(ref.node)) {
                    // The NgModule is in a .d.ts file but is not declared as a ts.ClassDeclaration. This is an
                    // error in the .d.ts metadata.
                    var code = type === 'import' ? diagnostics_1.ErrorCode.NGMODULE_INVALID_IMPORT :
                        diagnostics_1.ErrorCode.NGMODULE_INVALID_EXPORT;
                    diagnostics.push(diagnostics_1.makeDiagnostic(code, typescript_1.identifierOfNode(ref.node) || ref.node, "Appears in the NgModule." + type + "s of " + typescript_1.nodeNameForError(ownerForErrors) + ", but could not be resolved to an NgModule"));
                    return undefined;
                }
                return this.dependencyScopeReader.resolve(ref);
            }
            else {
                // The NgModule is declared locally in the current program. Resolve it from the registry.
                return this.getScopeOfModuleReference(ref);
            }
        };
        LocalModuleScopeRegistry.prototype.getReexports = function (ngModule, ref, declared, exported, diagnostics) {
            var e_11, _a, e_12, _b;
            var _this = this;
            var reexports = null;
            var sourceFile = ref.node.getSourceFile();
            if (this.aliasingHost === null) {
                return null;
            }
            reexports = [];
            // Track re-exports by symbol name, to produce diagnostics if two alias re-exports would share
            // the same name.
            var reexportMap = new Map();
            // Alias ngModuleRef added for readability below.
            var ngModuleRef = ref;
            var addReexport = function (exportRef) {
                if (exportRef.node.getSourceFile() === sourceFile) {
                    return;
                }
                var isReExport = !declared.has(exportRef.node);
                var exportName = _this.aliasingHost.maybeAliasSymbolAs(exportRef, sourceFile, ngModule.ref.node.name.text, isReExport);
                if (exportName === null) {
                    return;
                }
                if (!reexportMap.has(exportName)) {
                    if (exportRef.alias && exportRef.alias instanceof compiler_1.ExternalExpr) {
                        reexports.push({
                            fromModule: exportRef.alias.value.moduleName,
                            symbolName: exportRef.alias.value.name,
                            asAlias: exportName,
                        });
                    }
                    else {
                        var expr = _this.refEmitter.emit(exportRef.cloneWithNoIdentifiers(), sourceFile);
                        if (!(expr instanceof compiler_1.ExternalExpr) || expr.value.moduleName === null ||
                            expr.value.name === null) {
                            throw new Error('Expected ExternalExpr');
                        }
                        reexports.push({
                            fromModule: expr.value.moduleName,
                            symbolName: expr.value.name,
                            asAlias: exportName,
                        });
                    }
                    reexportMap.set(exportName, exportRef);
                }
                else {
                    // Another re-export already used this name. Produce a diagnostic.
                    var prevRef = reexportMap.get(exportName);
                    diagnostics.push(reexportCollision(ngModuleRef.node, prevRef, exportRef));
                }
            };
            try {
                for (var _c = tslib_1.__values(exported.directives), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var ref_1 = _d.value.ref;
                    addReexport(ref_1);
                }
            }
            catch (e_11_1) { e_11 = { error: e_11_1 }; }
            finally {
                try {
                    if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                }
                finally { if (e_11) throw e_11.error; }
            }
            try {
                for (var _e = tslib_1.__values(exported.pipes), _f = _e.next(); !_f.done; _f = _e.next()) {
                    var ref_2 = _f.value.ref;
                    addReexport(ref_2);
                }
            }
            catch (e_12_1) { e_12 = { error: e_12_1 }; }
            finally {
                try {
                    if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
                }
                finally { if (e_12) throw e_12.error; }
            }
            return reexports;
        };
        LocalModuleScopeRegistry.prototype.assertCollecting = function () {
            if (this.sealed) {
                throw new Error("Assertion: LocalModuleScopeRegistry is not COLLECTING");
            }
        };
        return LocalModuleScopeRegistry;
    }());
    exports.LocalModuleScopeRegistry = LocalModuleScopeRegistry;
    /**
     * Produce a `ts.Diagnostic` for an invalid import or export from an NgModule.
     */
    function invalidRef(clazz, decl, type) {
        var code = type === 'import' ? diagnostics_1.ErrorCode.NGMODULE_INVALID_IMPORT : diagnostics_1.ErrorCode.NGMODULE_INVALID_EXPORT;
        var resolveTarget = type === 'import' ? 'NgModule' : 'NgModule, Component, Directive, or Pipe';
        var message = "Appears in the NgModule." + type + "s of " + typescript_1.nodeNameForError(clazz) + ", but could not be resolved to an " + resolveTarget + " class." +
            '\n\n';
        var library = decl.ownedByModuleGuess !== null ? " (" + decl.ownedByModuleGuess + ")" : '';
        var sf = decl.node.getSourceFile();
        // Provide extra context to the error for the user.
        if (!sf.isDeclarationFile) {
            // This is a file in the user's program.
            var annotationType = type === 'import' ? '@NgModule' : 'Angular';
            message += "Is it missing an " + annotationType + " annotation?";
        }
        else if (sf.fileName.indexOf('node_modules') !== -1) {
            // This file comes from a third-party library in node_modules.
            message +=
                "This likely means that the library" + library + " which declares " + decl.debugName + " has not " +
                    'been processed correctly by ngcc, or is not compatible with Angular Ivy. Check if a ' +
                    'newer version of the library is available, and update if so. Also consider checking ' +
                    'with the library\'s authors to see if the library is expected to be compatible with Ivy.';
        }
        else {
            // This is a monorepo style local dependency. Unfortunately these are too different to really
            // offer much more advice than this.
            message += "This likely means that the dependency" + library + " which declares " + decl.debugName + " has not been processed correctly by ngcc.";
        }
        return diagnostics_1.makeDiagnostic(code, typescript_1.identifierOfNode(decl.node) || decl.node, message);
    }
    /**
     * Produce a `ts.Diagnostic` for an import or export which itself has errors.
     */
    function invalidTransitiveNgModuleRef(clazz, decl, type) {
        var code = type === 'import' ? diagnostics_1.ErrorCode.NGMODULE_INVALID_IMPORT : diagnostics_1.ErrorCode.NGMODULE_INVALID_EXPORT;
        return diagnostics_1.makeDiagnostic(code, typescript_1.identifierOfNode(decl.node) || decl.node, "Appears in the NgModule." + type + "s of " + typescript_1.nodeNameForError(clazz) + ", but itself has errors");
    }
    /**
     * Produce a `ts.Diagnostic` for an exported directive or pipe which was not declared or imported
     * by the NgModule in question.
     */
    function invalidReexport(clazz, decl) {
        return diagnostics_1.makeDiagnostic(diagnostics_1.ErrorCode.NGMODULE_INVALID_REEXPORT, typescript_1.identifierOfNode(decl.node) || decl.node, "Present in the NgModule.exports of " + typescript_1.nodeNameForError(clazz) + " but neither declared nor imported");
    }
    /**
     * Produce a `ts.Diagnostic` for a collision in re-export names between two directives/pipes.
     */
    function reexportCollision(module, refA, refB) {
        var childMessageText = "This directive/pipe is part of the exports of '" + module.name.text + "' and shares the same name as another exported directive/pipe.";
        return diagnostics_1.makeDiagnostic(diagnostics_1.ErrorCode.NGMODULE_REEXPORT_NAME_COLLISION, module.name, ("\n    There was a name collision between two classes named '" + refA.node.name.text + "', which are both part of the exports of '" + module.name.text + "'.\n\n    Angular generates re-exports of an NgModule's exported directives/pipes from the module's source file in certain cases, using the declared name of the class. If two classes of the same name are exported, this automatic naming does not work.\n\n    To fix this problem please re-export one or both classes directly from this file.\n  ").trim(), [
            { node: refA.node.name, messageText: childMessageText },
            { node: refB.node.name, messageText: childMessageText },
        ]);
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibG9jYWwuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9jb21waWxlci1jbGkvc3JjL25ndHNjL3Njb3BlL3NyYy9sb2NhbC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7O0lBRUgsOENBQStEO0lBQy9ELCtCQUFpQztJQUVqQywyRUFBNEQ7SUFJNUQsa0ZBQTZFO0lBNEI3RTs7Ozs7Ozs7Ozs7Ozs7Ozs7O09Ba0JHO0lBQ0g7UUEwREUsa0NBQ1ksV0FBMkIsRUFBVSxxQkFBNkMsRUFDbEYsVUFBNEIsRUFBVSxZQUErQjtZQURyRSxnQkFBVyxHQUFYLFdBQVcsQ0FBZ0I7WUFBVSwwQkFBcUIsR0FBckIscUJBQXFCLENBQXdCO1lBQ2xGLGVBQVUsR0FBVixVQUFVLENBQWtCO1lBQVUsaUJBQVksR0FBWixZQUFZLENBQW1CO1lBM0RqRjs7OztlQUlHO1lBQ0ssV0FBTSxHQUFHLEtBQUssQ0FBQztZQUV2Qjs7Ozs7O2VBTUc7WUFDSyx3QkFBbUIsR0FBRyxJQUFJLEdBQUcsRUFBcUMsQ0FBQztZQUUzRTs7O2VBR0c7WUFDSywwQkFBcUIsR0FDekIsSUFBSSxHQUFHLEVBQTRELENBQUM7WUFFaEUsZ0JBQVcsR0FBRyxJQUFJLEdBQUcsRUFBaUQsQ0FBQztZQUUvRTs7Ozs7ZUFLRztZQUNLLFVBQUssR0FBRyxJQUFJLEdBQUcsRUFBcUQsQ0FBQztZQUU3RTs7Ozs7OztlQU9HO1lBQ0ssa0JBQWEsR0FBRyxJQUFJLEdBQUcsRUFBb0IsQ0FBQztZQUVwRDs7ZUFFRztZQUNLLGdCQUFXLEdBQUcsSUFBSSxHQUFHLEVBQXFDLENBQUM7WUFFbkU7Ozs7OztlQU1HO1lBQ0ssbUJBQWMsR0FBRyxJQUFJLEdBQUcsRUFBb0IsQ0FBQztRQUkrQixDQUFDO1FBRXJGOztXQUVHO1FBQ0gsMkRBQXdCLEdBQXhCLFVBQXlCLElBQWtCOztZQUN6QyxJQUFJLENB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QUFJLEVBQUUsSUFBSSxDQUFDLElBQUksQ0FBQyxJQUFJLEVBQUUsV0FBVyxFQUFFLGdCQUFnQixFQUFDO1lBQ3JELEVBQUMsSUFBSSxFQUFFLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxFQUFFLFdBQVcsRUFBRSxnQkFBZ0IsRUFBQztTQUN0RCxDQUFDLENBQUM7SUFDVCxDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7RXh0ZXJuYWxFeHByLCBTY2hlbWFNZXRhZGF0YX0gZnJvbSAnQGFuZ3VsYXIvY29tcGlsZXInO1xuaW1wb3J0ICogYXMgdHMgZnJvbSAndHlwZXNjcmlwdCc7XG5cbmltcG9ydCB7RXJyb3JDb2RlLCBtYWtlRGlhZ25vc3RpY30gZnJvbSAnLi4vLi4vZGlhZ25vc3RpY3MnO1xuaW1wb3J0IHtBbGlhc2luZ0hvc3QsIFJlZXhwb3J0LCBSZWZlcmVuY2UsIFJlZmVyZW5jZUVtaXR0ZXJ9IGZyb20gJy4uLy4uL2ltcG9ydHMnO1xuaW1wb3J0IHtEaXJlY3RpdmVNZXRhLCBNZXRhZGF0YVJlYWRlciwgTWV0YWRhdGFSZWdpc3RyeSwgTmdNb2R1bGVNZXRhLCBQaXBlTWV0YX0gZnJvbSAnLi4vLi4vbWV0YWRhdGEnO1xuaW1wb3J0IHtDbGFzc0RlY2xhcmF0aW9ufSBmcm9tICcuLi8uLi9yZWZsZWN0aW9uJztcbmltcG9ydCB7aWRlbnRpZmllck9mTm9kZSwgbm9kZU5hbWVGb3JFcnJvcn0gZnJvbSAnLi4vLi4vdXRpbC9zcmMvdHlwZXNjcmlwdCc7XG5cbmltcG9ydCB7RXhwb3J0U2NvcGUsIFNjb3BlRGF0YX0gZnJvbSAnLi9hcGknO1xuaW1wb3J0IHtDb21wb25lbnRTY29wZVJlYWRlcn0gZnJvbSAnLi9jb21wb25lbnRfc2NvcGUnO1xuaW1wb3J0IHtEdHNNb2R1bGVTY29wZVJlc29sdmVyfSBmcm9tICcuL2RlcGVuZGVuY3knO1xuXG5leHBvcnQgaW50ZXJmYWNlIExvY2FsTmdNb2R1bGVEYXRhIHtcbiAgZGVjbGFyYXRpb25zOiBSZWZlcmVuY2U8Q2xhc3NEZWNsYXJhdGlvbj5bXTtcbiAgaW1wb3J0czogUmVmZXJlbmNlPENsYXNzRGVjbGFyYXRpb24+W107XG4gIGV4cG9ydHM6IFJlZmVyZW5jZTxDbGFzc0RlY2xhcmF0aW9uPltdO1xufVxuXG5leHBvcnQgaW50ZXJmYWNlIExvY2FsTW9kdWxlU2NvcGUgZXh0ZW5kcyBFeHBvcnRTY29wZSB7XG4gIGNvbXBpbGF0aW9uOiBTY29wZURhdGE7XG4gIHJlZXhwb3J0czogUmVleHBvcnRbXXxudWxsO1xuICBzY2hlbWFzOiBTY2hlbWFNZXRhZGF0YVtdO1xufVxuXG4vKipcbiAqIEluZm9ybWF0aW9uIGFib3V0IHRoZSBjb21waWxhdGlvbiBzY29wZSBvZiBhIHJlZ2lzdGVyZWQgZGVjbGFyYXRpb24uXG4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgQ29tcGlsYXRpb25TY29wZSBleHRlbmRzIFNjb3BlRGF0YSB7XG4gIC8qKiBUaGUgZGVjbGFyYXRpb24gd2hvc2UgY29tcGlsYXRpb24gc2NvcGUgaXMgZGVzY3JpYmVkIGhlcmUuICovXG4gIGRlY2xhcmF0aW9uOiBDbGFzc0RlY2xhcmF0aW9uO1xuICAvKiogVGhlIGRlY2xhcmF0aW9uIG9mIHRoZSBOZ01vZHVsZSB0aGF0IGRlY2xhcmVzIHRoaXMgYGRlY2xhcmF0aW9uYC4gKi9cbiAgbmdNb2R1bGU6IENsYXNzRGVjbGFyYXRpb247XG59XG5cbi8qKlxuICogQSByZWdpc3RyeSB3aGljaCBjb2xsZWN0cyBpbmZvcm1hdGlvbiBhYm91dCBOZ01vZHVsZXMsIERpcmVjdGl2ZXMsIENvbXBvbmVudHMsIGFuZCBQaXBlcyB3aGljaFxuICogYXJlIGxvY2FsIChkZWNsYXJlZCBpbiB0aGUgdHMuUHJvZ3JhbSBiZWluZyBjb21waWxlZCksIGFuZCBjYW4gcHJvZHVjZSBgTG9jYWxNb2R1bGVTY29wZWBzXG4gKiB3aGljaCBzdW1tYXJpemUgdGhlIGNvbXBpbGF0aW9uIHNjb3BlIG9mIGEgY29tcG9uZW50LlxuICpcbiAqIFRoaXMgY2xhc3MgaW1wbGVtZW50cyB0aGUgbG9naWMgb2YgTmdNb2R1bGUgZGVjbGFyYXRpb25zLCBpbXBvcnRzLCBhbmQgZXhwb3J0cyBhbmQgY2FuIHByb2R1Y2UsXG4gKiBmb3IgYSBnaXZlbiBjb21wb25lbnQsIHRoZSBzZXQgb2YgZGlyZWN0aXZlcyBhbmQgcGlwZXMgd2hpY2ggYXJlIFwidmlzaWJsZVwiIGluIHRoYXQgY29tcG9uZW50J3NcbiAqIHRlbXBsYXRlLlxuICpcbiAqIFRoZSBgTG9jYWxNb2R1bGVTY29wZVJlZ2lzdHJ5YCBoYXMgdHdvIFwibW9kZXNcIiBvZiBvcGVyYXRpb24uIER1cmluZyBhbmFseXNpcywgZGF0YSBmb3IgZWFjaFxuICogaW5kaXZpZHVhbCBOZ01vZHVsZSwgRGlyZWN0aXZlLCBDb21wb25lbnQsIGFuZCBQaXBlIGlzIGFkZGVkIHRvIHRoZSByZWdpc3RyeS4gTm8gYXR0ZW1wdCBpcyBtYWRlXG4gKiB0byB0cmF2ZXJzZSBvciB2YWxpZGF0ZSB0aGUgTmdNb2R1bGUgZ3JhcGggKGltcG9ydHMsIGV4cG9ydHMsIGV0YykuIEFmdGVyIGFuYWx5c2lzLCBvbmUgb2ZcbiAqIGBnZXRTY29wZU9mTW9kdWxlYCBvciBgZ2V0U2NvcGVGb3JDb21wb25lbnRgIGNhbiBiZSBjYWxsZWQsIHdoaWNoIHRyYXZlcnNlcyB0aGUgTmdNb2R1bGUgZ3JhcGhcbiAqIGFuZCBhcHBsaWVzIHRoZSBOZ01vZHVsZSBsb2dpYyB0byBnZW5lcmF0ZSBhIGBMb2NhbE1vZHVsZVNjb3BlYCwgdGhlIGZ1bGwgc2NvcGUgZm9yIHRoZSBnaXZlblxuICogbW9kdWxlIG9yIGNvbXBvbmVudC5cbiAqXG4gKiBUaGUgYExvY2FsTW9kdWxlU2NvcGVSZWdpc3RyeWAgaXMgYWxzbyBjYXBhYmxlIG9mIHByb2R1Y2luZyBgdHMuRGlhZ25vc3RpY2AgZXJyb3JzIHdoZW4gQW5ndWxhclxuICogc2VtYW50aWNzIGFyZSB2aW9sYXRlZC5cbiAqL1xuZXhwb3J0IGNsYXNzIExvY2FsTW9kdWxlU2NvcGVSZWdpc3RyeSBpbXBsZW1lbnRzIE1ldGFkYXRhUmVnaXN0cnksIENvbXBvbmVudFNjb3BlUmVhZGVyIHtcbiAgLyoqXG4gICAqIFRyYWNrcyB3aGV0aGVyIHRoZSByZWdpc3RyeSBoYXMgYmVlbiBhc2tlZCB0byBwcm9kdWNlIHNjb3BlcyBmb3IgYSBtb2R1bGUgb3IgY29tcG9uZW50LiBPbmNlXG4gICAqIHRoaXMgaXMgdHJ1ZSwgdGhlIHJlZ2lzdHJ5IGNhbm5vdCBhY2NlcHQgcmVnaXN0cmF0aW9ucyBvZiBuZXcgZGlyZWN0aXZlcy9waXBlcy9tb2R1bGVzIGFzIGl0XG4gICAqIHdvdWxkIGludmFsaWRhdGUgdGhlIGNhY2hlZCBzY29wZSBkYXRhLlxuICAgKi9cbiAgcHJpdmF0ZSBzZWFsZWQgPSBmYWxzZTtcblxuICAvKipcbiAgICogQSBtYXAgb2YgY29tcG9uZW50cyBmcm9tIHRoZSBjdXJyZW50IGNvbXBpbGF0aW9uIHVuaXQgdG8gdGhlIE5nTW9kdWxlIHdoaWNoIGRlY2xhcmVkIHRoZW0uXG4gICAqXG4gICAqIEFzIGNvbXBvbmVudHMgYW5kIGRpcmVjdGl2ZXMgYXJlIG5vdCBkaXN0aW5ndWlzaGVkIGF0IHRoZSBOZ01vZHVsZSBsZXZlbCwgdGhpcyBtYXAgbWF5IGFsc29cbiAgICogY29udGFpbiBkaXJlY3RpdmVzLiBUaGlzIGRvZXNuJ3QgY2F1c2UgYW55IHByb2JsZW1zIGJ1dCBpc24ndCB1c2VmdWwgYXMgdGhlcmUgaXMgbm8gY29uY2VwdCBvZlxuICAgKiBhIGRpcmVjdGl2ZSdzIGNvbXBpbGF0aW9uIHNjb3BlLlxuICAgKi9cbiAgcHJpdmF0ZSBkZWNsYXJhdGlvblRvTW9kdWxlID0gbmV3IE1hcDxDbGFzc0RlY2xhcmF0aW9uLCBEZWNsYXJhdGlvbkRhdGE+KCk7XG5cbiAgLyoqXG4gICAqIFRoaXMgbWFwcyBmcm9tIHRoZSBkaXJlY3RpdmUvcGlwZSBjbGFzcyB0byBhIG1hcCBvZiBkYXRhIGZvciBlYWNoIE5nTW9kdWxlIHRoYXQgZGVjbGFyZXMgdGhlXG4gICAqIGRpcmVjdGl2ZS9waXBlLiBUaGlzIGRhdGEgaXMgbmVlZGVkIHRvIHByb2R1Y2UgYW4gZXJyb3IgZm9yIHRoZSBnaXZlbiBjbGFzcy5cbiAgICovXG4gIHByaXZhdGUgZHVwbGljYXRlRGVjbGFyYXRpb25zID1cbiAgICAgIG5ldyBNYXA8Q2xhc3NEZWNsYXJhdGlvbiwgTWFwPENsYXNzRGVjbGFyYXRpb24sIERlY2xhcmF0aW9uRGF0YT4+KCk7XG5cbiAgcHJpdmF0ZSBtb2R1bGVUb1JlZiA9IG5ldyBNYXA8Q2xhc3NEZWNsYXJhdGlvbiwgUmVmZXJlbmNlPENsYXNzRGVjbGFyYXRpb24+PigpO1xuXG4gIC8qKlxuICAgKiBBIGNhY2hlIG9mIGNhbGN1bGF0ZWQgYExvY2FsTW9kdWxlU2NvcGVgcyBmb3IgZWFjaCBOZ01vZHVsZSBkZWNsYXJlZCBpbiB0aGUgY3VycmVudCBwcm9ncmFtLlxuICAgKlxuICAgKiBBIHZhbHVlIG9mIGB1bmRlZmluZWRgIGluZGljYXRlcyB0aGUgc2NvcGUgd2FzIGludmFsaWQgYW5kIHByb2R1Y2VkIGVycm9ycyAodGhlcmVmb3JlLFxuICAgKiBkaWFnbm9zdGljcyBzaG91bGQgZXhpc3QgaW4gdGhlIGBzY29wZUVycm9yc2AgbWFwKS5cbiAgICovXG4gIHByaXZhdGUgY2FjaGUgPSBuZXcgTWFwPENsYXNzRGVjbGFyYXRpb24sIExvY2FsTW9kdWxlU2NvcGV8dW5kZWZpbmVkfG51bGw+KCk7XG5cbiAgLyoqXG4gICAqIFRyYWNrcyB3aGV0aGVyIGEgZ2l2ZW4gY29tcG9uZW50IHJlcXVpcmVzIFwicmVtb3RlIHNjb3BpbmdcIi5cbiAgICpcbiAgICogUmVtb3RlIHNjb3BpbmcgaXMgd2hlbiB0aGUgc2V0IG9mIGRpcmVjdGl2ZXMgd2hpY2ggYXBwbHkgdG8gYSBnaXZlbiBjb21wb25lbnQgaXMgc2V0IGluIHRoZVxuICAgKiBOZ01vZHVsZSdzIGZpbGUgaW5zdGVhZCBvZiBkaXJlY3RseSBvbiB0aGUgY29tcG9uZW50IGRlZiAod2hpY2ggaXMgc29tZXRpbWVzIG5lZWRlZCB0byBnZXRcbiAgICogYXJvdW5kIGN5Y2xpYyBpbXBvcnQgaXNzdWVzKS4gVGhpcyBpcyBub3QgdXNlZCBpbiBjYWxjdWxhdGlvbiBvZiBgTG9jYWxNb2R1bGVTY29wZWBzLCBidXQgaXNcbiAgICogdHJhY2tlZCBoZXJlIGZvciBjb252ZW5pZW5jZS5cbiAgICovXG4gIHByaXZhdGUgcmVtb3RlU2NvcGluZyA9IG5ldyBTZXQ8Q2xhc3NEZWNsYXJhdGlvbj4oKTtcblxuICAvKipcbiAgICogVHJhY2tzIGVycm9ycyBhY2N1bXVsYXRlZCBpbiB0aGUgcHJvY2Vzc2luZyBvZiBzY29wZXMgZm9yIGVhY2ggbW9kdWxlIGRlY2xhcmF0aW9uLlxuICAgKi9cbiAgcHJpdmF0ZSBzY29wZUVycm9ycyA9IG5ldyBNYXA8Q2xhc3NEZWNsYXJhdGlvbiwgdHMuRGlhZ25vc3RpY1tdPigpO1xuXG4gIC8qKlxuICAgKiBUcmFja3Mgd2hpY2ggTmdNb2R1bGVzIGFyZSB1bnJlbGlhYmxlIGR1ZSB0byBlcnJvcnMgd2l0aGluIHRoZWlyIGRlY2xhcmF0aW9ucy5cbiAgICpcbiAgICogVGhpcyBwcm92aWRlcyBhIHVuaWZpZWQgdmlldyBvZiB3aGljaCBtb2R1bGVzIGhhdmUgZXJyb3JzLCBhY3Jvc3MgYWxsIG9mIHRoZSBkaWZmZXJlbnRcbiAgICogZGlhZ25vc3RpYyBjYXRlZ29yaWVzIHRoYXQgY2FuIGJlIHByb2R1Y2VkLiBUaGVvcmV0aWNhbGx5IHRoaXMgY2FuIGJlIGluZmVycmVkIGZyb20gdGhlIG90aGVyXG4gICAqIHByb3BlcnRpZXMgb2YgdGhpcyBjbGFzcywgYnV0IGlzIHRyYWNrZWQgZXhwbGljaXRseSB0byBzaW1wbGlmeSB0aGUgbG9naWMuXG4gICAqL1xuICBwcml2YXRlIHRhaW50ZWRNb2R1bGVzID0gbmV3IFNldDxDbGFzc0RlY2xhcmF0aW9uPigpO1xuXG4gIGNvbnN0cnVjdG9yKFxuICAgICAgcHJpdmF0ZSBsb2NhbFJlYWRlcjogTWV0YWRhdGFSZWFkZXIsIHByaXZhdGUgZGVwZW5kZW5jeVNjb3BlUmVhZGVyOiBEdHNNb2R1bGVTY29wZVJlc29sdmVyLFxuICAgICAgcHJpdmF0ZSByZWZFbWl0dGVyOiBSZWZlcmVuY2VFbWl0dGVyLCBwcml2YXRlIGFsaWFzaW5nSG9zdDogQWxpYXNpbmdIb3N0fG51bGwpIHt9XG5cbiAgLyoqXG4gICAqIEFkZCBhbiBOZ01vZHVsZSdzIGRhdGEgdG8gdGhlIHJlZ2lzdHJ5LlxuICAgKi9cbiAgcmVnaXN0ZXJOZ01vZHVsZU1ldGFkYXRhKGRhdGE6IE5nTW9kdWxlTWV0YSk6IHZvaWQge1xuICAgIHRoaXMuYXNzZXJ0Q29sbGVjdGluZygpO1xuICAgIGNvbnN0IG5nTW9kdWxlID0gZGF0YS5yZWYubm9kZTtcbiAgICB0aGlzLm1vZHVsZVRvUmVmLnNldChkYXRhLnJlZi5ub2RlLCBkYXRhLnJlZik7XG4gICAgLy8gSXRlcmF0ZSBvdmVyIHRoZSBtb2R1bGUncyBkZWNsYXJhdGlvbnMsIGFuZCBhZGQgdGhlbSB0byBkZWNsYXJhdGlvblRvTW9kdWxlLiBJZiBkdXBsaWNhdGVzXG4gICAgLy8gYXJlIGZvdW5kLCB0aGV5J3JlIGluc3RlYWQgdHJhY2tlZCBpbiBkdXBsaWNhdGVEZWNsYXJhdGlvbnMuXG4gICAgZm9yIChjb25zdCBkZWNsIG9mIGRhdGEuZGVjbGFyYXRpb25zKSB7XG4gICAgICB0aGlzLnJlZ2lzdGVyRGVjbGFyYXRpb25PZk1vZHVsZShuZ01vZHVsZSwgZGVjbCwgZGF0YS5yYXdEZWNsYXJhdGlvbnMpO1xuICAgIH1cbiAgfVxuXG4gIHJlZ2lzdGVyRGlyZWN0aXZlTWV0YWRhdGEoZGlyZWN0aXZlOiBEaXJlY3RpdmVNZXRhKTogdm9pZCB7fVxuXG4gIHJlZ2lzdGVyUGlwZU1ldGFkYXRhKHBpcGU6IFBpcGVNZXRhKTogdm9pZCB7fVxuXG4gIGdldFNjb3BlRm9yQ29tcG9uZW50KGNsYXp6OiBDbGFzc0RlY2xhcmF0aW9uKTogTG9jYWxNb2R1bGVTY29wZXxudWxsfCdlcnJvcicge1xuICAgIGNvbnN0IHNjb3BlID0gIXRoaXMuZGVjbGFyYXRpb25Ub01vZHVsZS5oYXMoY2xhenopID9cbiAgICAgICAgbnVsbCA6XG4gICAgICAgIHRoaXMuZ2V0U2NvcGVPZk1vZHVsZSh0aGlzLmRlY2xhcmF0aW9uVG9Nb2R1bGUuZ2V0KGNsYXp6KSEubmdNb2R1bGUpO1xuICAgIHJldHVybiBzY29wZTtcbiAgfVxuXG4gIC8qKlxuICAgKiBJZiBgbm9kZWAgaXMgZGVjbGFyZWQgaW4gbW9yZSB0aGFuIG9uZSBOZ01vZHVsZSAoZHVwbGljYXRlIGRlY2xhcmF0aW9uKSwgdGhlbiBnZXQgdGhlXG4gICAqIGBEZWNsYXJhdGlvbkRhdGFgIGZvciBlYWNoIG9mZmVuZGluZyBkZWNsYXJhdGlvbi5cbiAgICpcbiAgICogT3JkaW5hcmlseSBhIGNsYXNzIGlzIG9ubHkgZGVjbGFyZWQgaW4gb25lIE5nTW9kdWxlLCBpbiB3aGljaCBjYXNlIHRoaXMgZnVuY3Rpb24gcmV0dXJuc1xuICAgKiBgbnVsbGAuXG4gICAqL1xuICBnZXREdXBsaWNhdGVEZWNsYXJhdGlvbnMobm9kZTogQ2xhc3NEZWNsYXJhdGlvbik6IERlY2xhcmF0aW9uRGF0YVtdfG51bGwge1xuICAgIGlmICghdGhpcy5kdXBsaWNhdGVEZWNsYXJhdGlvbnMuaGFzKG5vZGUpKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG5cbiAgICByZXR1cm4gQXJyYXkuZnJvbSh0aGlzLmR1cGxpY2F0ZURlY2xhcmF0aW9ucy5nZXQobm9kZSkhLnZhbHVlcygpKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBDb2xsZWN0cyByZWdpc3RlcmVkIGRhdGEgZm9yIGEgbW9kdWxlIGFuZCBpdHMgZGlyZWN0aXZlcy9waXBlcyBhbmQgY29udmVydCBpdCBpbnRvIGEgZnVsbFxuICAgKiBgTG9jYWxNb2R1bGVTY29wZWAuXG4gICAqXG4gICAqIFRoaXMgbWV0aG9kIGltcGxlbWVudHMgdGhlIGxvZ2ljIG9mIE5nTW9kdWxlIGltcG9ydHMgYW5kIGV4cG9ydHMuIEl0IHJldHVybnMgdGhlXG4gICAqIGBMb2NhbE1vZHVsZVNjb3BlYCBmb3IgdGhlIGdpdmVuIE5nTW9kdWxlIGlmIG9uZSBjYW4gYmUgcHJvZHVjZWQsIGBudWxsYCBpZiBubyBzY29wZSB3YXMgZXZlclxuICAgKiBkZWZpbmVkLCBvciB0aGUgc3RyaW5nIGAnZXJyb3InYCBpZiB0aGUgc2NvcGUgY29udGFpbmVkIGVycm9ycy5cbiAgICovXG4gIGdldFNjb3BlT2ZNb2R1bGUoY2xheno6IENsYXNzRGVjbGFyYXRpb24pOiBMb2NhbE1vZHVsZVNjb3BlfCdlcnJvcid8bnVsbCB7XG4gICAgY29uc3Qgc2NvcGUgPSB0aGlzLm1vZHVsZVRvUmVmLmhhcyhjbGF6eikgP1xuICAgICAgICB0aGlzLmdldFNjb3BlT2ZNb2R1bGVSZWZlcmVuY2UodGhpcy5tb2R1bGVUb1JlZi5nZXQoY2xhenopISkgOlxuICAgICAgICBudWxsO1xuICAgIC8vIElmIHRoZSBOZ01vZHVsZSBjbGFzcyBpcyBtYXJrZWQgYXMgdGFpbnRlZCwgY29uc2lkZXIgaXQgYW4gZXJyb3IuXG4gICAgaWYgKHRoaXMudGFpbnRlZE1vZHVsZXMuaGFzKGNsYXp6KSkge1xuICAgICAgcmV0dXJuICdlcnJvcic7XG4gICAgfVxuXG4gICAgLy8gVHJhbnNsYXRlIHVuZGVmaW5lZCAtPiAnZXJyb3InLlxuICAgIHJldHVybiBzY29wZSAhPT0gdW5kZWZpbmVkID8gc2NvcGUgOiAnZXJyb3InO1xuICB9XG5cbiAgLyoqXG4gICAqIFJldHJpZXZlcyBhbnkgYHRzLkRpYWdub3N0aWNgcyBwcm9kdWNlZCBkdXJpbmcgdGhlIGNhbGN1bGF0aW9uIG9mIHRoZSBgTG9jYWxNb2R1bGVTY29wZWAgZm9yXG4gICAqIHRoZSBnaXZlbiBOZ01vZHVsZSwgb3IgYG51bGxgIGlmIG5vIGVycm9ycyB3ZXJlIHByZXNlbnQuXG4gICAqL1xuICBnZXREaWFnbm9zdGljc09mTW9kdWxlKGNsYXp6OiBDbGFzc0RlY2xhcmF0aW9uKTogdHMuRGlhZ25vc3RpY1tdfG51bGwge1xuICAgIC8vIFJlcXVpcmVkIHRvIGVuc3VyZSB0aGUgZXJyb3JzIGFyZSBwb3B1bGF0ZWQgZm9yIHRoZSBnaXZlbiBjbGFzcy4gSWYgaXQgaGFzIGJlZW4gcHJvY2Vzc2VkXG4gICAgLy8gYmVmb3JlLCB0aGlzIHdpbGwgYmUgYSBuby1vcCBkdWUgdG8gdGhlIHNjb3BlIGNhY2hlLlxuICAgIHRoaXMuZ2V0U2NvcGVPZk1vZHVsZShjbGF6eik7XG5cbiAgICBpZiAodGhpcy5zY29wZUVycm9ycy5oYXMoY2xhenopKSB7XG4gICAgICByZXR1cm4gdGhpcy5zY29wZUVycm9ycy5nZXQoY2xhenopITtcbiAgICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIFJldHVybnMgYSBjb2xsZWN0aW9uIG9mIHRoZSBjb21waWxhdGlvbiBzY29wZSBmb3IgZWFjaCByZWdpc3RlcmVkIGRlY2xhcmF0aW9uLlxuICAgKi9cbiAgZ2V0Q29tcGlsYXRpb25TY29wZXMoKTogQ29tcGlsYXRpb25TY29wZVtdIHtcbiAgICBjb25zdCBzY29wZXM6IENvbXBpbGF0aW9uU2NvcGVbXSA9IFtdO1xuICAgIHRoaXMuZGVjbGFyYXRpb25Ub01vZHVsZS5mb3JFYWNoKChkZWNsRGF0YSwgZGVjbGFyYXRpb24pID0+IHtcbiAgICAgIGNvbnN0IHNjb3BlID0gdGhpcy5nZXRTY29wZU9mTW9kdWxlKGRlY2xEYXRhLm5nTW9kdWxlKTtcbiAgICAgIGlmIChzY29wZSAhPT0gbnVsbCAmJiBzY29wZSAhPT0gJ2Vycm9yJykge1xuICAgICAgICBzY29wZXMucHVzaCh7ZGVjbGFyYXRpb24sIG5nTW9kdWxlOiBkZWNsRGF0YS5uZ01vZHVsZSwgLi4uc2NvcGUuY29tcGlsYXRpb259KTtcbiAgICAgIH1cbiAgICB9KTtcbiAgICByZXR1cm4gc2NvcGVzO1xuICB9XG5cbiAgcHJpdmF0ZSByZWdpc3RlckRlY2xhcmF0aW9uT2ZNb2R1bGUoXG4gICAgICBuZ01vZHVsZTogQ2xhc3NEZWNsYXJhdGlvbiwgZGVjbDogUmVmZXJlbmNlPENsYXNzRGVjbGFyYXRpb24+LFxuICAgICAgcmF3RGVjbGFyYXRpb25zOiB0cy5FeHByZXNzaW9ufG51bGwpOiB2b2lkIHtcbiAgICBjb25zdCBkZWNsRGF0YTogRGVjbGFyYXRpb25EYXRhID0ge1xuICAgICAgbmdNb2R1bGUsXG4gICAgICByZWY6IGRlY2wsXG4gICAgICByYXdEZWNsYXJhdGlvbnMsXG4gICAgfTtcblxuICAgIC8vIEZpcnN0LCBjaGVjayBmb3IgZHVwbGljYXRlIGRlY2xhcmF0aW9ucyBvZiB0aGUgc2FtZSBkaXJlY3RpdmUvcGlwZS5cbiAgICBpZiAodGhpcy5kdXBsaWNhdGVEZWNsYXJhdGlvbnMuaGFzKGRlY2wubm9kZSkpIHtcbiAgICAgIC8vIFRoaXMgZGlyZWN0aXZlL3BpcGUgaGFzIGFscmVhZHkgYmVlbiBpZGVudGlmaWVkIGFzIGJlaW5nIGR1cGxpY2F0ZWQuIEFkZCB0aGlzIG1vZHVsZSB0byB0aGVcbiAgICAgIC8vIG1hcCBvZiBtb2R1bGVzIGZvciB3aGljaCBhIGR1cGxpY2F0ZSBkZWNsYXJhdGlvbiBleGlzdHMuXG4gICAgICB0aGlzLmR1cGxpY2F0ZURlY2xhcmF0aW9ucy5nZXQoZGVjbC5ub2RlKSEuc2V0KG5nTW9kdWxlLCBkZWNsRGF0YSk7XG4gICAgfSBlbHNlIGlmIChcbiAgICAgICAgdGhpcy5kZWNsYXJhdGlvblRvTW9kdWxlLmhhcyhkZWNsLm5vZGUpICYmXG4gICAgICAgIHRoaXMuZGVjbGFyYXRpb25Ub01vZHVsZS5nZXQoZGVjbC5ub2RlKSEubmdNb2R1bGUgIT09IG5nTW9kdWxlKSB7XG4gICAgICAvLyBUaGlzIGRpcmVjdGl2ZS9waXBlIGlzIGFscmVhZHkgcmVnaXN0ZXJlZCBhcyBkZWNsYXJlZCBpbiBhbm90aGVyIG1vZHVsZS4gTWFyayBpdCBhcyBhXG4gICAgICAvLyBkdXBsaWNhdGUgaW5zdGVhZC5cbiAgICAgIGNvbnN0IGR1cGxpY2F0ZURlY2xNYXAgPSBuZXcgTWFwPENsYXNzRGVjbGFyYXRpb24sIERlY2xhcmF0aW9uRGF0YT4oKTtcbiAgICAgIGNvbnN0IGZpcnN0RGVjbERhdGEgPSB0aGlzLmRlY2xhcmF0aW9uVG9Nb2R1bGUuZ2V0KGRlY2wubm9kZSkhO1xuXG4gICAgICAvLyBNYXJrIGJvdGggbW9kdWxlcyBhcyB0YWludGVkLCBzaW5jZSB0aGVpciBkZWNsYXJhdGlvbnMgYXJlIG1pc3NpbmcgYSBjb21wb25lbnQuXG4gICAgICB0aGlzLnRhaW50ZWRNb2R1bGVzLmFkZChmaXJzdERlY2xEYXRhLm5nTW9kdWxlKTtcbiAgICAgIHRoaXMudGFpbnRlZE1vZHVsZXMuYWRkKG5nTW9kdWxlKTtcblxuICAgICAgLy8gQmVpbmcgZGV0ZWN0ZWQgYXMgYSBkdXBsaWNhdGUgbWVhbnMgdGhlcmUgYXJlIHR3byBOZ01vZHVsZXMgKGZvciBub3cpIHdoaWNoIGRlY2xhcmUgdGhpc1xuICAgICAgLy8gZGlyZWN0aXZlL3BpcGUuIEFkZCBib3RoIG9mIHRoZW0gdG8gdGhlIGR1cGxpY2F0ZSB0cmFja2luZyBtYXAuXG4gICAgICBkdXBsaWNhdGVEZWNsTWFwLnNldChmaXJzdERlY2xEYXRhLm5nTW9kdWxlLCBmaXJzdERlY2xEYXRhKTtcbiAgICAgIGR1cGxpY2F0ZURlY2xNYXAuc2V0KG5nTW9kdWxlLCBkZWNsRGF0YSk7XG4gICAgICB0aGlzLmR1cGxpY2F0ZURlY2xhcmF0aW9ucy5zZXQoZGVjbC5ub2RlLCBkdXBsaWNhdGVEZWNsTWFwKTtcblxuICAgICAgLy8gUmVtb3ZlIHRoZSBkaXJlY3RpdmUvcGlwZSBmcm9tIGBkZWNsYXJhdGlvblRvTW9kdWxlYCBhcyBpdCdzIGEgZHVwbGljYXRlIGRlY2xhcmF0aW9uLCBhbmRcbiAgICAgIC8vIHRoZXJlZm9yZSBub3QgdmFsaWQuXG4gICAgICB0aGlzLmRlY2xhcmF0aW9uVG9Nb2R1bGUuZGVsZXRlKGRlY2wubm9kZSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIC8vIFRoaXMgaXMgdGhlIGZpcnN0IGRlY2xhcmF0aW9uIG9mIHRoaXMgZGlyZWN0aXZlL3BpcGUsIHNvIG1hcCBpdC5cbiAgICAgIHRoaXMuZGVjbGFyYXRpb25Ub01vZHVsZS5zZXQoZGVjbC5ub2RlLCBkZWNsRGF0YSk7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIEltcGxlbWVudGF0aW9uIG9mIGBnZXRTY29wZU9mTW9kdWxlYCB3aGljaCBhY2NlcHRzIGEgcmVmZXJlbmNlIHRvIGEgY2xhc3MgYW5kIGRpZmZlcmVudGlhdGVzXG4gICAqIGJldHdlZW46XG4gICAqXG4gICAqICogbm8gc2NvcGUgYmVpbmcgYXZhaWxhYmxlIChyZXR1cm5zIGBudWxsYClcbiAgICogKiBhIHNjb3BlIGJlaW5nIHByb2R1Y2VkIHdpdGggZXJyb3JzIChyZXR1cm5zIGB1bmRlZmluZWRgKS5cbiAgICovXG4gIHByaXZhdGUgZ2V0U2NvcGVPZk1vZHVsZVJlZmVyZW5jZShyZWY6IFJlZmVyZW5jZTxDbGFzc0RlY2xhcmF0aW9uPik6IExvY2FsTW9kdWxlU2NvcGV8bnVsbFxuICAgICAgfHVuZGVmaW5lZCB7XG4gICAgaWYgKHRoaXMuY2FjaGUuaGFzKHJlZi5ub2RlKSkge1xuICAgICAgcmV0dXJuIHRoaXMuY2FjaGUuZ2V0KHJlZi5ub2RlKTtcbiAgICB9XG5cbiAgICAvLyBTZWFsIHRoZSByZWdpc3RyeSB0byBwcm90ZWN0IHRoZSBpbnRlZ3JpdHkgb2YgdGhlIGBMb2NhbE1vZHVsZVNjb3BlYCBjYWNoZS5cbiAgICB0aGlzLnNlYWxlZCA9IHRydWU7XG5cbiAgICAvLyBgcmVmYCBzaG91bGQgYmUgYW4gTmdNb2R1bGUgcHJldmlvdXNseSBhZGRlZCB0byB0aGUgcmVnaXN0cnkuIElmIG5vdCwgYSBzY29wZSBmb3IgaXRcbiAgICAvLyBjYW5ub3QgYmUgcHJvZHVjZWQuXG4gICAgY29uc3QgbmdNb2R1bGUgPSB0aGlzLmxvY2FsUmVhZGVyLmdldE5nTW9kdWxlTWV0YWRhdGEocmVmKTtcbiAgICBpZiAobmdNb2R1bGUgPT09IG51bGwpIHtcbiAgICAgIHRoaXMuY2FjaGUuc2V0KHJlZi5ub2RlLCBudWxsKTtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIC8vIE1vZHVsZXMgd2hpY2ggY29udHJpYnV0ZWQgdG8gdGhlIGNvbXBpbGF0aW9uIHNjb3BlIG9mIHRoaXMgbW9kdWxlLlxuICAgIGNvbnN0IGNvbXBpbGF0aW9uTW9kdWxlcyA9IG5ldyBTZXQ8Q2xhc3NEZWNsYXJhdGlvbj4oW25nTW9kdWxlLnJlZi5ub2RlXSk7XG4gICAgLy8gTW9kdWxlcyB3aGljaCBjb250cmlidXRlZCB0byB0aGUgZXhwb3J0IHNjb3BlIG9mIHRoaXMgbW9kdWxlLlxuICAgIGNvbnN0IGV4cG9ydGVkTW9kdWxlcyA9IG5ldyBTZXQ8Q2xhc3NEZWNsYXJhdGlvbj4oW25nTW9kdWxlLnJlZi5ub2RlXSk7XG5cbiAgICAvLyBFcnJvcnMgcHJvZHVjZWQgZHVyaW5nIGNvbXB1dGF0aW9uIG9mIHRoZSBzY29wZSBhcmUgcmVjb3JkZWQgaGVyZS4gQXQgdGhlIGVuZCwgaWYgdGhpcyBhcnJheVxuICAgIC8vIGlzbid0IGVtcHR5IHRoZW4gYHVuZGVmaW5lZGAgd2lsbCBiZSBjYWNoZWQgYW5kIHJldHVybmVkIHRvIGluZGljYXRlIHRoaXMgc2NvcGUgaXMgaW52YWxpZC5cbiAgICBjb25zdCBkaWFnbm9zdGljczogdHMuRGlhZ25vc3RpY1tdID0gW107XG5cbiAgICAvLyBBdCB0aGlzIHBvaW50LCB0aGUgZ29hbCBpcyB0byBwcm9kdWNlIHR3byBkaXN0aW5jdCB0cmFuc2l0aXZlIHNldHM6XG4gICAgLy8gLSB0aGUgZGlyZWN0aXZlcyBhbmQgcGlwZXMgd2hpY2ggYXJlIHZpc2libGUgdG8gY29tcG9uZW50cyBkZWNsYXJlZCBpbiB0aGUgTmdNb2R1bGUuXG4gICAgLy8gLSB0aGUgZGlyZWN0aXZlcyBhbmQgcGlwZXMgd2hpY2ggYXJlIGV4cG9ydGVkIHRvIGFueSBOZ01vZHVsZXMgd2hpY2ggaW1wb3J0IHRoaXMgb25lLlxuXG4gICAgLy8gRGlyZWN0aXZlcyBhbmQgcGlwZXMgaW4gdGhlIGNvbXBpbGF0aW9uIHNjb3BlLlxuICAgIGNvbnN0IGNvbXBpbGF0aW9uRGlyZWN0aXZlcyA9IG5ldyBNYXA8dHMuRGVjbGFyYXRpb24sIERpcmVjdGl2ZU1ldGE+KCk7XG4gICAgY29uc3QgY29tcGlsYXRpb25QaXBlcyA9IG5ldyBNYXA8dHMuRGVjbGFyYXRpb24sIFBpcGVNZXRhPigpO1xuXG4gICAgY29uc3QgZGVjbGFyZWQgPSBuZXcgU2V0PHRzLkRlY2xhcmF0aW9uPigpO1xuXG4gICAgLy8gRGlyZWN0aXZlcyBhbmQgcGlwZXMgZXhwb3J0ZWQgdG8gYW55IGltcG9ydGluZyBOZ01vZHVsZXMuXG4gICAgY29uc3QgZXhwb3J0RGlyZWN0aXZlcyA9IG5ldyBNYXA8dHMuRGVjbGFyYXRpb24sIERpcmVjdGl2ZU1ldGE+KCk7XG4gICAgY29uc3QgZXhwb3J0UGlwZXMgPSBuZXcgTWFwPHRzLkRlY2xhcmF0aW9uLCBQaXBlTWV0YT4oKTtcblxuICAgIC8vIFRoZSBhbGdvcml0aG0gaXMgYXMgZm9sbG93czpcbiAgICAvLyAxKSBBZGQgYWxsIG9mIHRoZSBkaXJlY3RpdmVzL3BpcGVzIGZyb20gZWFjaCBOZ01vZHVsZSBpbXBvcnRlZCBpbnRvIHRoZSBjdXJyZW50IG9uZSB0byB0aGVcbiAgICAvLyAgICBjb21waWxhdGlvbiBzY29wZS5cbiAgICAvLyAyKSBBZGQgZGlyZWN0aXZlcy9waXBlcyBkZWNsYXJlZCBpbiB0aGUgTmdNb2R1bGUgdG8gdGhlIGNvbXBpbGF0aW9uIHNjb3BlLiBBdCB0aGlzIHBvaW50LCB0aGVcbiAgICAvLyAgICBjb21waWxhdGlvbiBzY29wZSBpcyBjb21wbGV0ZS5cbiAgICAvLyAzKSBGb3IgZWFjaCBlbnRyeSBpbiB0aGUgTmdNb2R1bGUncyBleHBvcnRzOlxuICAgIC8vICAgIGEpIEF0dGVtcHQgdG8gcmVzb2x2ZSBpdCBhcyBhbiBOZ01vZHVsZSB3aXRoIGl0cyBvd24gZXhwb3J0ZWQgZGlyZWN0aXZlcy9waXBlcy4gSWYgaXQgaXNcbiAgICAvLyAgICAgICBvbmUsIGFkZCB0aGVtIHRvIHRoZSBleHBvcnQgc2NvcGUgb2YgdGhpcyBOZ01vZHVsZS5cbiAgICAvLyAgICBiKSBPdGhlcndpc2UsIGl0IHNob3VsZCBiZSBhIGNsYXNzIGluIHRoZSBjb21waWxhdGlvbiBzY29wZSBvZiB0aGlzIE5nTW9kdWxlLiBJZiBpdCBpcyxcbiAgICAvLyAgICAgICBhZGQgaXQgdG8gdGhlIGV4cG9ydCBzY29wZS5cbiAgICAvLyAgICBjKSBJZiBpdCdzIG5laXRoZXIgYW4gTmdNb2R1bGUgbm9yIGEgZGlyZWN0aXZlL3BpcGUgaW4gdGhlIGNvbXBpbGF0aW9uIHNjb3BlLCB0aGVuIHRoaXNcbiAgICAvLyAgICAgICBpcyBhbiBlcnJvci5cblxuICAgIC8vIDEpIHByb2Nlc3MgaW1wb3J0cy5cbiAgICBmb3IgKGNvbnN0IGRlY2wgb2YgbmdNb2R1bGUuaW1wb3J0cykge1xuICAgICAgY29uc3QgaW1wb3J0U2NvcGUgPSB0aGlzLmdldEV4cG9ydGVkU2NvcGUoZGVjbCwgZGlhZ25vc3RpY3MsIHJlZi5ub2RlLCAnaW1wb3J0Jyk7XG4gICAgICBpZiAoaW1wb3J0U2NvcGUgPT09IG51bGwpIHtcbiAgICAgICAgLy8gQW4gaW1wb3J0IHdhc24ndCBhbiBOZ01vZHVsZSwgc28gcmVjb3JkIGFuIGVycm9yLlxuICAgICAgICBkaWFnbm9zdGljcy5wdXNoKGludmFsaWRSZWYocmVmLm5vZGUsIGRlY2wsICdpbXBvcnQnKSk7XG4gICAgICAgIGNvbnRpbnVlO1xuICAgICAgfSBlbHNlIGlmIChpbXBvcnRTY29wZSA9PT0gdW5kZWZpbmVkKSB7XG4gICAgICAgIC8vIEFuIGltcG9ydCB3YXMgYW4gTmdNb2R1bGUgYnV0IGNvbnRhaW5lZCBlcnJvcnMgb2YgaXRzIG93bi4gUmVjb3JkIHRoaXMgYXMgYW4gZXJyb3IgdG9vLFxuICAgICAgICAvLyBiZWNhdXNlIHRoaXMgc2NvcGUgaXMgYWx3YXlzIGdvaW5nIHRvIGJlIGluY29ycmVjdCBpZiBvbmUgb2YgaXRzIGltcG9ydHMgY291bGQgbm90IGJlXG4gICAgICAgIC8vIHJlYWQuXG4gICAgICAgIGRpYWdub3N0aWNzLnB1c2goaW52YWxpZFRyYW5zaXRpdmVOZ01vZHVsZVJlZihyZWYubm9kZSwgZGVjbCwgJ2ltcG9ydCcpKTtcbiAgICAgICAgY29udGludWU7XG4gICAgICB9XG4gICAgICBmb3IgKGNvbnN0IGRpcmVjdGl2ZSBvZiBpbXBvcnRTY29wZS5leHBvcnRlZC5kaXJlY3RpdmVzKSB7XG4gICAgICAgIGNvbXBpbGF0aW9uRGlyZWN0aXZlcy5zZXQoZGlyZWN0aXZlLnJlZi5ub2RlLCBkaXJlY3RpdmUpO1xuICAgICAgfVxuICAgICAgZm9yIChjb25zdCBwaXBlIG9mIGltcG9ydFNjb3BlLmV4cG9ydGVkLnBpcGVzKSB7XG4gICAgICAgIGNvbXBpbGF0aW9uUGlwZXMuc2V0KHBpcGUucmVmLm5vZGUsIHBpcGUpO1xuICAgICAgfVxuICAgICAgZm9yIChjb25zdCBpbXBvcnRlZE1vZHVsZSBvZiBpbXBvcnRTY29wZS5leHBvcnRlZC5uZ01vZHVsZXMpIHtcbiAgICAgICAgY29tcGlsYXRpb25Nb2R1bGVzLmFkZChpbXBvcnRlZE1vZHVsZSk7XG4gICAgICB9XG4gICAgfVxuXG4gICAgLy8gMikgYWRkIGRlY2xhcmF0aW9ucy5cbiAgICBmb3IgKGNvbnN0IGRlY2wgb2YgbmdNb2R1bGUuZGVjbGFyYXRpb25zKSB7XG4gICAgICBjb25zdCBkaXJlY3RpdmUgPSB0aGlzLmxvY2FsUmVhZGVyLmdldERpcmVjdGl2ZU1ldGFkYXRhKGRlY2wpO1xuICAgICAgY29uc3QgcGlwZSA9IHRoaXMubG9jYWxSZWFkZXIuZ2V0UGlwZU1ldGFkYXRhKGRlY2wpO1xuICAgICAgaWYgKGRpcmVjdGl2ZSAhPT0gbnVsbCkge1xuICAgICAgICBjb21waWxhdGlvbkRpcmVjdGl2ZXMuc2V0KGRlY2wubm9kZSwgey4uLmRpcmVjdGl2ZSwgcmVmOiBkZWNsfSk7XG4gICAgICB9IGVsc2UgaWYgKHBpcGUgIT09IG51bGwpIHtcbiAgICAgICAgY29tcGlsYXRpb25QaXBlcy5zZXQoZGVjbC5ub2RlLCB7Li4ucGlwZSwgcmVmOiBkZWNsfSk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICB0aGlzLnRhaW50ZWRNb2R1bGVzLmFkZChuZ01vZHVsZS5yZWYubm9kZSk7XG5cbiAgICAgICAgY29uc3QgZXJyb3JOb2RlID0gZGVjbC5nZXRPcmlnaW5Gb3JEaWFnbm9zdGljcyhuZ01vZHVsZS5yYXdEZWNsYXJhdGlvbnMhKTtcbiAgICAgICAgZGlhZ25vc3RpY3MucHVzaChtYWtlRGlhZ25vc3RpYyhcbiAgICAgICAgICAgIEVycm9yQ29kZS5OR01PRFVMRV9JTlZBTElEX0RFQ0xBUkFUSU9OLCBlcnJvck5vZGUsXG4gICAgICAgICAgICBgVGhlIGNsYXNzICcke2RlY2wubm9kZS5uYW1lLnRleHR9JyBpcyBsaXN0ZWQgaW4gdGhlIGRlY2xhcmF0aW9ucyBgICtcbiAgICAgICAgICAgICAgICBgb2YgdGhlIE5nTW9kdWxlICcke1xuICAgICAgICAgICAgICAgICAgICBuZ01vZHVsZS5yZWYubm9kZS5uYW1lXG4gICAgICAgICAgICAgICAgICAgICAgICAudGV4dH0nLCBidXQgaXMgbm90IGEgZGlyZWN0aXZlLCBhIGNvbXBvbmVudCwgb3IgYSBwaXBlLiBgICtcbiAgICAgICAgICAgICAgICBgRWl0aGVyIHJlbW92ZSBpdCBmcm9tIHRoZSBOZ01vZHVsZSdzIGRlY2xhcmF0aW9ucywgb3IgYWRkIGFuIGFwcHJvcHJpYXRlIEFuZ3VsYXIgZGVjb3JhdG9yLmAsXG4gICAgICAgICAgICBbe25vZGU6IGRlY2wubm9kZS5uYW1lLCBtZXNzYWdlVGV4dDogYCcke2RlY2wubm9kZS5uYW1lLnRleHR9JyBpcyBkZWNsYXJlZCBoZXJlLmB9XSkpO1xuICAgICAgICBjb250aW51ZTtcbiAgICAgIH1cblxuICAgICAgZGVjbGFyZWQuYWRkKGRlY2wubm9kZSk7XG4gICAgfVxuXG4gICAgLy8gMykgcHJvY2VzcyBleHBvcnRzLlxuICAgIC8vIEV4cG9ydHMgY2FuIGNvbnRhaW4gbW9kdWxlcywgY29tcG9uZW50cywgb3IgZGlyZWN0aXZlcy4gVGhleSdyZSBwcm9jZXNzZWQgZGlmZmVyZW50bHkuXG4gICAgLy8gTW9kdWxlcyBhcmUgc3RyYWlnaHRmb3J3YXJkLiBEaXJlY3RpdmVzIGFuZCBwaXBlcyBmcm9tIGV4cG9ydGVkIG1vZHVsZXMgYXJlIGFkZGVkIHRvIHRoZVxuICAgIC8vIGV4cG9ydCBtYXBzLiBEaXJlY3RpdmVzL3BpcGVzIGFyZSBkaWZmZXJlbnQgLSB0aGV5IG1pZ2h0IGJlIGV4cG9ydHMgb2YgZGVjbGFyZWQgdHlwZXMgb3JcbiAgICAvLyBpbXBvcnRlZCB0eXBlcy5cbiAgICBmb3IgKGNvbnN0IGRlY2wgb2YgbmdNb2R1bGUuZXhwb3J0cykge1xuICAgICAgLy8gQXR0ZW1wdCB0byByZXNvbHZlIGRlY2wgYXMgYW4gTmdNb2R1bGUuXG4gICAgICBjb25zdCBpbXBvcnRTY29wZSA9IHRoaXMuZ2V0RXhwb3J0ZWRTY29wZShkZWNsLCBkaWFnbm9zdGljcywgcmVmLm5vZGUsICdleHBvcnQnKTtcbiAgICAgIGlmIChpbXBvcnRTY29wZSA9PT0gdW5kZWZpbmVkKSB7XG4gICAgICAgIC8vIEFuIGV4cG9ydCB3YXMgYW4gTmdNb2R1bGUgYnV0IGNvbnRhaW5lZCBlcnJvcnMgb2YgaXRzIG93bi4gUmVjb3JkIHRoaXMgYXMgYW4gZXJyb3IgdG9vLFxuICAgICAgICAvLyBiZWNhdXNlIHRoaXMgc2NvcGUgaXMgYWx3YXlzIGdvaW5nIHRvIGJlIGluY29ycmVjdCBpZiBvbmUgb2YgaXRzIGV4cG9ydHMgY291bGQgbm90IGJlXG4gICAgICAgIC8vIHJlYWQuXG4gICAgICAgIGRpYWdub3N0aWNzLnB1c2goaW52YWxpZFRyYW5zaXRpdmVOZ01vZHVsZVJlZihyZWYubm9kZSwgZGVjbCwgJ2V4cG9ydCcpKTtcbiAgICAgICAgY29udGludWU7XG4gICAgICB9IGVsc2UgaWYgKGltcG9ydFNjb3BlICE9PSBudWxsKSB7XG4gICAgICAgIC8vIGRlY2wgaXMgYW4gTmdNb2R1bGUuXG4gICAgICAgIGZvciAoY29uc3QgZGlyZWN0aXZlIG9mIGltcG9ydFNjb3BlLmV4cG9ydGVkLmRpcmVjdGl2ZXMpIHtcbiAgICAgICAgICBleHBvcnREaXJlY3RpdmVzLnNldChkaXJlY3RpdmUucmVmLm5vZGUsIGRpcmVjdGl2ZSk7XG4gICAgICAgIH1cbiAgICAgICAgZm9yIChjb25zdCBwaXBlIG9mIGltcG9ydFNjb3BlLmV4cG9ydGVkLnBpcGVzKSB7XG4gICAgICAgICAgZXhwb3J0UGlwZXMuc2V0KHBpcGUucmVmLm5vZGUsIHBpcGUpO1xuICAgICAgICB9XG4gICAgICAgIGZvciAoY29uc3QgZXhwb3J0ZWRNb2R1bGUgb2YgaW1wb3J0U2NvcGUuZXhwb3J0ZWQubmdNb2R1bGVzKSB7XG4gICAgICAgICAgZXhwb3J0ZWRNb2R1bGVzLmFkZChleHBvcnRlZE1vZHVsZSk7XG4gICAgICAgIH1cbiAgICAgIH0gZWxzZSBpZiAoY29tcGlsYXRpb25EaXJlY3RpdmVzLmhhcyhkZWNsLm5vZGUpKSB7XG4gICAgICAgIC8vIGRlY2wgaXMgYSBkaXJlY3RpdmUgb3IgY29tcG9uZW50IGluIHRoZSBjb21waWxhdGlvbiBzY29wZSBvZiB0aGlzIE5nTW9kdWxlLlxuICAgICAgICBjb25zdCBkaXJlY3RpdmUgPSBjb21waWxhdGlvbkRpcmVjdGl2ZXMuZ2V0KGRlY2wubm9kZSkhO1xuICAgICAgICBleHBvcnREaXJlY3RpdmVzLnNldChkZWNsLm5vZGUsIGRpcmVjdGl2ZSk7XG4gICAgICB9IGVsc2UgaWYgKGNvbXBpbGF0aW9uUGlwZXMuaGFzKGRlY2wubm9kZSkpIHtcbiAgICAgICAgLy8gZGVjbCBpcyBhIHBpcGUgaW4gdGhlIGNvbXBpbGF0aW9uIHNjb3BlIG9mIHRoaXMgTmdNb2R1bGUuXG4gICAgICAgIGNvbnN0IHBpcGUgPSBjb21waWxhdGlvblBpcGVzLmdldChkZWNsLm5vZGUpITtcbiAgICAgICAgZXhwb3J0UGlwZXMuc2V0KGRlY2wubm9kZSwgcGlwZSk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICAvLyBkZWNsIGlzIGFuIHVua25vd24gZXhwb3J0LlxuICAgICAgICBpZiAodGhpcy5sb2NhbFJlYWRlci5nZXREaXJlY3RpdmVNZXRhZGF0YShkZWNsKSAhPT0gbnVsbCB8fFxuICAgICAgICAgICAgdGhpcy5sb2NhbFJlYWRlci5nZXRQaXBlTWV0YWRhdGEoZGVjbCkgIT09IG51bGwpIHtcbiAgICAgICAgICBkaWFnbm9zdGljcy5wdXNoKGludmFsaWRSZWV4cG9ydChyZWYubm9kZSwgZGVjbCkpO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgIGRpYWdub3N0aWNzLnB1c2goaW52YWxpZFJlZihyZWYubm9kZSwgZGVjbCwgJ2V4cG9ydCcpKTtcbiAgICAgICAgfVxuICAgICAgICBjb250aW51ZTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICBjb25zdCBleHBvcnRlZCA9IHtcbiAgICAgIGRpcmVjdGl2ZXM6IEFycmF5LmZyb20oZXhwb3J0RGlyZWN0aXZlcy52YWx1ZXMoKSksXG4gICAgICBwaXBlczogQXJyYXkuZnJvbShleHBvcnRQaXBlcy52YWx1ZXMoKSksXG4gICAgICBuZ01vZHVsZXM6IEFycmF5LmZyb20oZXhwb3J0ZWRNb2R1bGVzKSxcbiAgICB9O1xuXG4gICAgY29uc3QgcmVleHBvcnRzID0gdGhpcy5nZXRSZWV4cG9ydHMobmdNb2R1bGUsIHJlZiwgZGVjbGFyZWQsIGV4cG9ydGVkLCBkaWFnbm9zdGljcyk7XG5cbiAgICAvLyBDaGVjayBpZiB0aGlzIHNjb3BlIGhhZCBhbnkgZXJyb3JzIGR1cmluZyBwcm9kdWN0aW9uLlxuICAgIGlmIChkaWFnbm9zdGljcy5sZW5ndGggPiAwKSB7XG4gICAgICAvLyBDYWNoZSB1bmRlZmluZWQsIHRvIG1hcmsgdGhlIGZhY3QgdGhhdCB0aGUgc2NvcGUgaXMgaW52YWxpZC5cbiAgICAgIHRoaXMuY2FjaGUuc2V0KHJlZi5ub2RlLCB1bmRlZmluZWQpO1xuXG4gICAgICAvLyBTYXZlIHRoZSBlcnJvcnMgZm9yIHJldHJpZXZhbC5cbiAgICAgIHRoaXMuc2NvcGVFcnJvcnMuc2V0KHJlZi5ub2RlLCBkaWFnbm9zdGljcyk7XG5cbiAgICAgIC8vIE1hcmsgdGhpcyBtb2R1bGUgYXMgYmVpbmcgdGFpbnRlZC5cbiAgICAgIHRoaXMudGFpbnRlZE1vZHVsZXMuYWRkKHJlZi5ub2RlKTtcbiAgICAgIHJldHVybiB1bmRlZmluZWQ7XG4gICAgfVxuXG4gICAgLy8gRmluYWxseSwgcHJvZHVjZSB0aGUgYExvY2FsTW9kdWxlU2NvcGVgIHdpdGggYm90aCB0aGUgY29tcGlsYXRpb24gYW5kIGV4cG9ydCBzY29wZXMuXG4gICAgY29uc3Qgc2NvcGUgPSB7XG4gICAgICBjb21waWxhdGlvbjoge1xuICAgICAgICBkaXJlY3RpdmVzOiBBcnJheS5mcm9tKGNvbXBpbGF0aW9uRGlyZWN0aXZlcy52YWx1ZXMoKSksXG4gICAgICAgIHBpcGVzOiBBcnJheS5mcm9tKGNvbXBpbGF0aW9uUGlwZXMudmFsdWVzKCkpLFxuICAgICAgICBuZ01vZHVsZXM6IEFycmF5LmZyb20oY29tcGlsYXRpb25Nb2R1bGVzKSxcbiAgICAgIH0sXG4gICAgICBleHBvcnRlZCxcbiAgICAgIHJlZXhwb3J0cyxcbiAgICAgIHNjaGVtYXM6IG5nTW9kdWxlLnNjaGVtYXMsXG4gICAgfTtcbiAgICB0aGlzLmNhY2hlLnNldChyZWYubm9kZSwgc2NvcGUpO1xuICAgIHJldHVybiBzY29wZTtcbiAgfVxuXG4gIC8qKlxuICAgKiBDaGVjayB3aGV0aGVyIGEgY29tcG9uZW50IHJlcXVpcmVzIHJlbW90ZSBzY29waW5nLlxuICAgKi9cbiAgZ2V0UmVxdWlyZXNSZW1vdGVTY29wZShub2RlOiBDbGFzc0RlY2xhcmF0aW9uKTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIHRoaXMucmVtb3RlU2NvcGluZy5oYXMobm9kZSk7XG4gIH1cblxuICAvKipcbiAgICogU2V0IGEgY29tcG9uZW50IGFzIHJlcXVpcmluZyByZW1vdGUgc2NvcGluZy5cbiAgICovXG4gIHNldENvbXBvbmVudEFzUmVxdWlyaW5nUmVtb3RlU2NvcGluZyhub2RlOiBDbGFzc0RlY2xhcmF0aW9uKTogdm9pZCB7XG4gICAgdGhpcy5yZW1vdGVTY29waW5nLmFkZChub2RlKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBMb29rIHVwIHRoZSBgRXhwb3J0U2NvcGVgIG9mIGEgZ2l2ZW4gYFJlZmVyZW5jZWAgdG8gYW4gTmdNb2R1bGUuXG4gICAqXG4gICAqIFRoZSBOZ01vZHVsZSBpbiBxdWVzdGlvbiBtYXkgYmUgZGVjbGFyZWQgbG9jYWxseSBpbiB0aGUgY3VycmVudCB0cy5Qcm9ncmFtLCBvciBpdCBtYXkgYmVcbiAgICogZGVjbGFyZWQgaW4gYSAuZC50cyBmaWxlLlxuICAgKlxuICAgKiBAcmV0dXJucyBgbnVsbGAgaWYgbm8gc2NvcGUgY291bGQgYmUgZm91bmQsIG9yIGB1bmRlZmluZWRgIGlmIGFuIGludmFsaWQgc2NvcGVcbiAgICogd2FzIGZvdW5kLlxuICAgKlxuICAgKiBNYXkgYWxzbyBjb250cmlidXRlIGRpYWdub3N0aWNzIG9mIGl0cyBvd24gYnkgYWRkaW5nIHRvIHRoZSBnaXZlbiBgZGlhZ25vc3RpY3NgXG4gICAqIGFycmF5IHBhcmFtZXRlci5cbiAgICovXG4gIHByaXZhdGUgZ2V0RXhwb3J0ZWRTY29wZShcbiAgICAgIHJlZjogUmVmZXJlbmNlPENsYXNzRGVjbGFyYXRpb24+LCBkaWFnbm9zdGljczogdHMuRGlhZ25vc3RpY1tdLFxuICAgICAgb3duZXJGb3JFcnJvcnM6IHRzLkRlY2xhcmF0aW9uLCB0eXBlOiAnaW1wb3J0J3wnZXhwb3J0Jyk6IEV4cG9ydFNjb3BlfG51bGx8dW5kZWZpbmVkIHtcbiAgICBpZiAocmVmLm5vZGUuZ2V0U291cmNlRmlsZSgpLmlzRGVjbGFyYXRpb25GaWxlKSB7XG4gICAgICAvLyBUaGUgTmdNb2R1bGUgaXMgZGVjbGFyZWQgaW4gYSAuZC50cyBmaWxlLiBSZXNvbHZlIGl0IHdpdGggdGhlIGBEZXBlbmRlbmN5U2NvcGVSZWFkZXJgLlxuICAgICAgaWYgKCF0cy5pc0NsYXNzRGVjbGFyYXRpb24ocmVmLm5vZGUpKSB7XG4gICAgICAgIC8vIFRoZSBOZ01vZHVsZSBpcyBpbiBhIC5kLnRzIGZpbGUgYnV0IGlzIG5vdCBkZWNsYXJlZCBhcyBhIHRzLkNsYXNzRGVjbGFyYXRpb24uIFRoaXMgaXMgYW5cbiAgICAgICAgLy8gZXJyb3IgaW4gdGhlIC5kLnRzIG1ldGFkYXRhLlxuICAgICAgICBjb25zdCBjb2RlID0gdHlwZSA9PT0gJ2ltcG9ydCcgPyBFcnJvckNvZGUuTkdNT0RVTEVfSU5WQUxJRF9JTVBPUlQgOlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBFcnJvckNvZGUuTkdNT0RVTEVfSU5WQUxJRF9FWFBPUlQ7XG4gICAgICAgIGRpYWdub3N0aWNzLnB1c2gobWFrZURpYWdub3N0aWMoXG4gICAgICAgICAgICBjb2RlLCBpZGVudGlmaWVyT2ZOb2RlKHJlZi5ub2RlKSB8fCByZWYubm9kZSxcbiAgICAgICAgICAgIGBBcHBlYXJzIGluIHRoZSBOZ01vZHVsZS4ke3R5cGV9cyBvZiAke1xuICAgICAgICAgICAgICAgIG5vZGVOYW1lRm9yRXJyb3Iob3duZXJGb3JFcnJvcnMpfSwgYnV0IGNvdWxkIG5vdCBiZSByZXNvbHZlZCB0byBhbiBOZ01vZHVsZWApKTtcbiAgICAgICAgcmV0dXJuIHVuZGVmaW5lZDtcbiAgICAgIH1cbiAgICAgIHJldHVybiB0aGlzLmRlcGVuZGVuY3lTY29wZVJlYWRlci5yZXNvbHZlKHJlZik7XG4gICAgfSBlbHNlIHtcbiAgICAgIC8vIFRoZSBOZ01vZHVsZSBpcyBkZWNsYXJlZCBsb2NhbGx5IGluIHRoZSBjdXJyZW50IHByb2dyYW0uIFJlc29sdmUgaXQgZnJvbSB0aGUgcmVnaXN0cnkuXG4gICAgICByZXR1cm4gdGhpcy5nZXRTY29wZU9mTW9kdWxlUmVmZXJlbmNlKHJlZik7XG4gICAgfVxuICB9XG5cbiAgcHJpdmF0ZSBnZXRSZWV4cG9ydHMoXG4gICAgICBuZ01vZHVsZTogTmdNb2R1bGVNZXRhLCByZWY6IFJlZmVyZW5jZTxDbGFzc0RlY2xhcmF0aW9uPiwgZGVjbGFyZWQ6IFNldDx0cy5EZWNsYXJhdGlvbj4sXG4gICAgICBleHBvcnRlZDoge2RpcmVjdGl2ZXM6IERpcmVjdGl2ZU1ldGFbXSwgcGlwZXM6IFBpcGVNZXRhW119LFxuICAgICAgZGlhZ25vc3RpY3M6IHRzLkRpYWdub3N0aWNbXSk6IFJlZXhwb3J0W118bnVsbCB7XG4gICAgbGV0IHJlZXhwb3J0czogUmVleHBvcnRbXXxudWxsID0gbnVsbDtcbiAgICBjb25zdCBzb3VyY2VGaWxlID0gcmVmLm5vZGUuZ2V0U291cmNlRmlsZSgpO1xuICAgIGlmICh0aGlzLmFsaWFzaW5nSG9zdCA9PT0gbnVsbCkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIHJlZXhwb3J0cyA9IFtdO1xuICAgIC8vIFRyYWNrIHJlLWV4cG9ydHMgYnkgc3ltYm9sIG5hbWUsIHRvIHByb2R1Y2UgZGlhZ25vc3RpY3MgaWYgdHdvIGFsaWFzIHJlLWV4cG9ydHMgd291bGQgc2hhcmVcbiAgICAvLyB0aGUgc2FtZSBuYW1lLlxuICAgIGNvbnN0IHJlZXhwb3J0TWFwID0gbmV3IE1hcDxzdHJpbmcsIFJlZmVyZW5jZTxDbGFzc0RlY2xhcmF0aW9uPj4oKTtcbiAgICAvLyBBbGlhcyBuZ01vZHVsZVJlZiBhZGRlZCBmb3IgcmVhZGFiaWxpdHkgYmVsb3cuXG4gICAgY29uc3QgbmdNb2R1bGVSZWYgPSByZWY7XG4gICAgY29uc3QgYWRkUmVleHBvcnQgPSAoZXhwb3J0UmVmOiBSZWZlcmVuY2U8Q2xhc3NEZWNsYXJhdGlvbj4pID0+IHtcbiAgICAgIGlmIChleHBvcnRSZWYubm9kZS5nZXRTb3VyY2VGaWxlKCkgPT09IHNvdXJjZUZpbGUpIHtcbiAgICAgICAgcmV0dXJuO1xuICAgICAgfVxuICAgICAgY29uc3QgaXNSZUV4cG9ydCA9ICFkZWNsYXJlZC5oYXMoZXhwb3J0UmVmLm5vZGUpO1xuICAgICAgY29uc3QgZXhwb3J0TmFtZSA9IHRoaXMuYWxpYXNpbmdIb3N0IS5tYXliZUFsaWFzU3ltYm9sQXMoXG4gICAgICAgICAgZXhwb3J0UmVmLCBzb3VyY2VGaWxlLCBuZ01vZHVsZS5yZWYubm9kZS5uYW1lLnRleHQsIGlzUmVFeHBvcnQpO1xuICAgICAgaWYgKGV4cG9ydE5hbWUgPT09IG51bGwpIHtcbiAgICAgICAgcmV0dXJuO1xuICAgICAgfVxuICAgICAgaWYgKCFyZWV4cG9ydE1hcC5oYXMoZXhwb3J0TmFtZSkpIHtcbiAgICAgICAgaWYgKGV4cG9ydFJlZi5hbGlhcyAmJiBleHBvcnRSZWYuYWxpYXMgaW5zdGFuY2VvZiBFeHRlcm5hbEV4cHIpIHtcbiAgICAgICAgICByZWV4cG9ydHMhLnB1c2goe1xuICAgICAgICAgICAgZnJvbU1vZHVsZTogZXhwb3J0UmVmLmFsaWFzLnZhbHVlLm1vZHVsZU5hbWUhLFxuICAgICAgICAgICAgc3ltYm9sTmFtZTogZXhwb3J0UmVmLmFsaWFzLnZhbHVlLm5hbWUhLFxuICAgICAgICAgICAgYXNBbGlhczogZXhwb3J0TmFtZSxcbiAgICAgICAgICB9KTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICBjb25zdCBleHByID0gdGhpcy5yZWZFbWl0dGVyLmVtaXQoZXhwb3J0UmVmLmNsb25lV2l0aE5vSWRlbnRpZmllcnMoKSwgc291cmNlRmlsZSk7XG4gICAgICAgICAgaWYgKCEoZXhwciBpbnN0YW5jZW9mIEV4dGVybmFsRXhwcikgfHwgZXhwci52YWx1ZS5tb2R1bGVOYW1lID09PSBudWxsIHx8XG4gICAgICAgICAgICAgIGV4cHIudmFsdWUubmFtZSA9PT0gbnVsbCkge1xuICAgICAgICAgICAgdGhyb3cgbmV3IEVycm9yKCdFeHBlY3RlZCBFeHRlcm5hbEV4cHInKTtcbiAgICAgICAgICB9XG4gICAgICAgICAgcmVleHBvcnRzIS5wdXNoKHtcbiAgICAgICAgICAgIGZyb21Nb2R1bGU6IGV4cHIudmFsdWUubW9kdWxlTmFtZSxcbiAgICAgICAgICAgIHN5bWJvbE5hbWU6IGV4cHIudmFsdWUubmFtZSxcbiAgICAgICAgICAgIGFzQWxpYXM6IGV4cG9ydE5hbWUsXG4gICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICAgICAgcmVleHBvcnRNYXAuc2V0KGV4cG9ydE5hbWUsIGV4cG9ydFJlZik7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICAvLyBBbm90aGVyIHJlLWV4cG9ydCBhbHJlYWR5IHVzZWQgdGhpcyBuYW1lLiBQcm9kdWNlIGEgZGlhZ25vc3RpYy5cbiAgICAgICAgY29uc3QgcHJldlJlZiA9IHJlZXhwb3J0TWFwLmdldChleHBvcnROYW1lKSE7XG4gICAgICAgIGRpYWdub3N0aWNzLnB1c2gocmVleHBvcnRDb2xsaXNpb24obmdNb2R1bGVSZWYubm9kZSwgcHJldlJlZiwgZXhwb3J0UmVmKSk7XG4gICAgICB9XG4gICAgfTtcbiAgICBmb3IgKGNvbnN0IHtyZWZ9IG9mIGV4cG9ydGVkLmRpcmVjdGl2ZXMpIHtcbiAgICAgIGFkZFJlZXhwb3J0KHJlZik7XG4gICAgfVxuICAgIGZvciAoY29uc3Qge3JlZn0gb2YgZXhwb3J0ZWQucGlwZXMpIHtcbiAgICAgIGFkZFJlZXhwb3J0KHJlZik7XG4gICAgfVxuICAgIHJldHVybiByZWV4cG9ydHM7XG4gIH1cblxuICBwcml2YXRlIGFzc2VydENvbGxlY3RpbmcoKTogdm9pZCB7XG4gICAgaWYgKHRoaXMuc2VhbGVkKSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoYEFzc2VydGlvbjogTG9jYWxNb2R1bGVTY29wZVJlZ2lzdHJ5IGlzIG5vdCBDT0xMRUNUSU5HYCk7XG4gICAgfVxuICB9XG59XG5cbi8qKlxuICogUHJvZHVjZSBhIGB0cy5EaWFnbm9zdGljYCBmb3IgYW4gaW52YWxpZCBpbXBvcnQgb3IgZXhwb3J0IGZyb20gYW4gTmdNb2R1bGUuXG4gKi9cbmZ1bmN0aW9uIGludmFsaWRSZWYoXG4gICAgY2xheno6IHRzLkRlY2xhcmF0aW9uLCBkZWNsOiBSZWZlcmVuY2U8dHMuRGVjbGFyYXRpb24+LFxuICAgIHR5cGU6ICdpbXBvcnQnfCdleHBvcnQnKTogdHMuRGlhZ25vc3RpYyB7XG4gIGNvbnN0IGNvZGUgPVxuICAgICAgdHlwZSA9PT0gJ2ltcG9ydCcgPyBFcnJvckNvZGUuTkdNT0RVTEVfSU5WQUxJRF9JTVBPUlQgOiBFcnJvckNvZGUuTkdNT0RVTEVfSU5WQUxJRF9FWFBPUlQ7XG4gIGNvbnN0IHJlc29sdmVUYXJnZXQgPSB0eXBlID09PSAnaW1wb3J0JyA/ICdOZ01vZHVsZScgOiAnTmdNb2R1bGUsIENvbXBvbmVudCwgRGlyZWN0aXZlLCBvciBQaXBlJztcbiAgbGV0IG1lc3NhZ2UgPVxuICAgICAgYEFwcGVhcnMgaW4gdGhlIE5nTW9kdWxlLiR7dHlwZX1zIG9mICR7XG4gICAgICAgICAgbm9kZU5hbWVGb3JFcnJvcihjbGF6eil9LCBidXQgY291bGQgbm90IGJlIHJlc29sdmVkIHRvIGFuICR7cmVzb2x2ZVRhcmdldH0gY2xhc3MuYCArXG4gICAgICAnXFxuXFxuJztcbiAgY29uc3QgbGlicmFyeSA9IGRlY2wub3duZWRCeU1vZHVsZUd1ZXNzICE9PSBudWxsID8gYCAoJHtkZWNsLm93bmVkQnlNb2R1bGVHdWVzc30pYCA6ICcnO1xuICBjb25zdCBzZiA9IGRlY2wubm9kZS5nZXRTb3VyY2VGaWxlKCk7XG5cbiAgLy8gUHJvdmlkZSBleHRyYSBjb250ZXh0IHRvIHRoZSBlcnJvciBmb3IgdGhlIHVzZXIuXG4gIGlmICghc2YuaXNEZWNsYXJhdGlvbkZpbGUpIHtcbiAgICAvLyBUaGlzIGlzIGEgZmlsZSBpbiB0aGUgdXNlcidzIHByb2dyYW0uXG4gICAgY29uc3QgYW5ub3RhdGlvblR5cGUgPSB0eXBlID09PSAnaW1wb3J0JyA/ICdATmdNb2R1bGUnIDogJ0FuZ3VsYXInO1xuICAgIG1lc3NhZ2UgKz0gYElzIGl0IG1pc3NpbmcgYW4gJHthbm5vdGF0aW9uVHlwZX0gYW5ub3RhdGlvbj9gO1xuICB9IGVsc2UgaWYgKHNmLmZpbGVOYW1lLmluZGV4T2YoJ25vZGVfbW9kdWxlcycpICE9PSAtMSkge1xuICAgIC8vIFRoaXMgZmlsZSBjb21lcyBmcm9tIGEgdGhpcmQtcGFydHkgbGlicmFyeSBpbiBub2RlX21vZHVsZXMuXG4gICAgbWVzc2FnZSArPVxuICAgICAgICBgVGhpcyBsaWtlbHkgbWVhbnMgdGhhdCB0aGUgbGlicmFyeSR7bGlicmFyeX0gd2hpY2ggZGVjbGFyZXMgJHtkZWNsLmRlYnVnTmFtZX0gaGFzIG5vdCBgICtcbiAgICAgICAgJ2JlZW4gcHJvY2Vzc2VkIGNvcnJlY3RseSBieSBuZ2NjLCBvciBpcyBub3QgY29tcGF0aWJsZSB3aXRoIEFuZ3VsYXIgSXZ5LiBDaGVjayBpZiBhICcgK1xuICAgICAgICAnbmV3ZXIgdmVyc2lvbiBvZiB0aGUgbGlicmFyeSBpcyBhdmFpbGFibGUsIGFuZCB1cGRhdGUgaWYgc28uIEFsc28gY29uc2lkZXIgY2hlY2tpbmcgJyArXG4gICAgICAgICd3aXRoIHRoZSBsaWJyYXJ5XFwncyBhdXRob3JzIHRvIHNlZSBpZiB0aGUgbGlicmFyeSBpcyBleHBlY3RlZCB0byBiZSBjb21wYXRpYmxlIHdpdGggSXZ5Lic7XG4gIH0gZWxzZSB7XG4gICAgLy8gVGhpcyBpcyBhIG1vbm9yZXBvIHN0eWxlIGxvY2FsIGRlcGVuZGVuY3kuIFVuZm9ydHVuYXRlbHkgdGhlc2UgYXJlIHRvbyBkaWZmZXJlbnQgdG8gcmVhbGx5XG4gICAgLy8gb2ZmZXIgbXVjaCBtb3JlwqBhZHZpY2UgdGhhbiB0aGlzLlxuICAgIG1lc3NhZ2UgKz0gYFRoaXMgbGlrZWx5IG1lYW5zIHRoYXQgdGhlIGRlcGVuZGVuY3kke2xpYnJhcnl9IHdoaWNoIGRlY2xhcmVzICR7XG4gICAgICAgIGRlY2wuZGVidWdOYW1lfSBoYXMgbm90IGJlZW4gcHJvY2Vzc2VkIGNvcnJlY3RseSBieSBuZ2NjLmA7XG4gIH1cblxuICByZXR1cm4gbWFrZURpYWdub3N0aWMoY29kZSwgaWRlbnRpZmllck9mTm9kZShkZWNsLm5vZGUpIHx8IGRlY2wubm9kZSwgbWVzc2FnZSk7XG59XG5cbi8qKlxuICogUHJvZHVjZSBhIGB0cy5EaWFnbm9zdGljYCBmb3IgYW4gaW1wb3J0IG9yIGV4cG9ydCB3aGljaCBpdHNlbGYgaGFzIGVycm9ycy5cbiAqL1xuZnVuY3Rpb24gaW52YWxpZFRyYW5zaXRpdmVOZ01vZHVsZVJlZihcbiAgICBjbGF6ejogdHMuRGVjbGFyYXRpb24sIGRlY2w6IFJlZmVyZW5jZTx0cy5EZWNsYXJhdGlvbj4sXG4gICAgdHlwZTogJ2ltcG9ydCd8J2V4cG9ydCcpOiB0cy5EaWFnbm9zdGljIHtcbiAgY29uc3QgY29kZSA9XG4gICAgICB0eXBlID09PSAnaW1wb3J0JyA/IEVycm9yQ29kZS5OR01PRFVMRV9JTlZBTElEX0lNUE9SVCA6IEVycm9yQ29kZS5OR01PRFVMRV9JTlZBTElEX0VYUE9SVDtcbiAgcmV0dXJuIG1ha2VEaWFnbm9zdGljKFxuICAgICAgY29kZSwgaWRlbnRpZmllck9mTm9kZShkZWNsLm5vZGUpIHx8IGRlY2wubm9kZSxcbiAgICAgIGBBcHBlYXJzIGluIHRoZSBOZ01vZHVsZS4ke3R5cGV9cyBvZiAke25vZGVOYW1lRm9yRXJyb3IoY2xhenopfSwgYnV0IGl0c2VsZiBoYXMgZXJyb3JzYCk7XG59XG5cbi8qKlxuICogUHJvZHVjZSBhIGB0cy5EaWFnbm9zdGljYCBmb3IgYW4gZXhwb3J0ZWQgZGlyZWN0aXZlIG9yIHBpcGUgd2hpY2ggd2FzIG5vdCBkZWNsYXJlZCBvciBpbXBvcnRlZFxuICogYnkgdGhlIE5nTW9kdWxlIGluIHF1ZXN0aW9uLlxuICovXG5mdW5jdGlvbiBpbnZhbGlkUmVleHBvcnQoY2xheno6IHRzLkRlY2xhcmF0aW9uLCBkZWNsOiBSZWZlcmVuY2U8dHMuRGVjbGFyYXRpb24+KTogdHMuRGlhZ25vc3RpYyB7XG4gIHJldHVybiBtYWtlRGlhZ25vc3RpYyhcbiAgICAgIEVycm9yQ29kZS5OR01PRFVMRV9JTlZBTElEX1JFRVhQT1JULCBpZGVudGlmaWVyT2ZOb2RlKGRlY2wubm9kZSkgfHwgZGVjbC5ub2RlLFxuICAgICAgYFByZXNlbnQgaW4gdGhlIE5nTW9kdWxlLmV4cG9ydHMgb2YgJHtcbiAgICAgICAgICBub2RlTmFtZUZvckVycm9yKGNsYXp6KX0gYnV0IG5laXRoZXIgZGVjbGFyZWQgbm9yIGltcG9ydGVkYCk7XG59XG5cbi8qKlxuICogUHJvZHVjZSBhIGB0cy5EaWFnbm9zdGljYCBmb3IgYSBjb2xsaXNpb24gaW4gcmUtZXhwb3J0IG5hbWVzIGJldHdlZW4gdHdvIGRpcmVjdGl2ZXMvcGlwZXMuXG4gKi9cbmZ1bmN0aW9uIHJlZXhwb3J0Q29sbGlzaW9uKFxuICAgIG1vZHVsZTogQ2xhc3NEZWNsYXJhdGlvbiwgcmVmQTogUmVmZXJlbmNlPENsYXNzRGVjbGFyYXRpb24+LFxuICAgIHJlZkI6IFJlZmVyZW5jZTxDbGFzc0RlY2xhcmF0aW9uPik6IHRzLkRpYWdub3N0aWMge1xuICBjb25zdCBjaGlsZE1lc3NhZ2VUZXh0ID0gYFRoaXMgZGlyZWN0aXZlL3BpcGUgaXMgcGFydCBvZiB0aGUgZXhwb3J0cyBvZiAnJHtcbiAgICAgIG1vZHVsZS5uYW1lLnRleHR9JyBhbmQgc2hhcmVzIHRoZSBzYW1lIG5hbWUgYXMgYW5vdGhlciBleHBvcnRlZCBkaXJlY3RpdmUvcGlwZS5gO1xuICByZXR1cm4gbWFrZURpYWdub3N0aWMoXG4gICAgICBFcnJvckNvZGUuTkdNT0RVTEVfUkVFWFBPUlRfTkFNRV9DT0xMSVNJT04sIG1vZHVsZS5uYW1lLFxuICAgICAgYFxuICAgIFRoZXJlIHdhcyBhIG5hbWUgY29sbGlzaW9uIGJldHdlZW4gdHdvIGNsYXNzZXMgbmFtZWQgJyR7XG4gICAgICAgICAgcmVmQS5ub2RlLm5hbWUudGV4dH0nLCB3aGljaCBhcmUgYm90aCBwYXJ0IG9mIHRoZSBleHBvcnRzIG9mICcke21vZHVsZS5uYW1lLnRleHR9Jy5cblxuICAgIEFuZ3VsYXIgZ2VuZXJhdGVzIHJlLWV4cG9ydHMgb2YgYW4gTmdNb2R1bGUncyBleHBvcnRlZCBkaXJlY3RpdmVzL3BpcGVzIGZyb20gdGhlIG1vZHVsZSdzIHNvdXJjZSBmaWxlIGluIGNlcnRhaW4gY2FzZXMsIHVzaW5nIHRoZSBkZWNsYXJlZCBuYW1lIG9mIHRoZSBjbGFzcy4gSWYgdHdvIGNsYXNzZXMgb2YgdGhlIHNhbWUgbmFtZSBhcmUgZXhwb3J0ZWQsIHRoaXMgYXV0b21hdGljIG5hbWluZyBkb2VzIG5vdCB3b3JrLlxuXG4gICAgVG8gZml4IHRoaXMgcHJvYmxlbSBwbGVhc2UgcmUtZXhwb3J0IG9uZSBvciBib3RoIGNsYXNzZXMgZGlyZWN0bHkgZnJvbSB0aGlzIGZpbGUuXG4gIGAudHJpbSgpLFxuICAgICAgW1xuICAgICAgICB7bm9kZTogcmVmQS5ub2RlLm5hbWUsIG1lc3NhZ2VUZXh0OiBjaGlsZE1lc3NhZ2VUZXh0fSxcbiAgICAgICAge25vZGU6IHJlZkIubm9kZS5uYW1lLCBtZXNzYWdlVGV4dDogY2hpbGRNZXNzYWdlVGV4dH0sXG4gICAgICBdKTtcbn1cblxuZXhwb3J0IGludGVyZmFjZSBEZWNsYXJhdGlvbkRhdGEge1xuICBuZ01vZHVsZTogQ2xhc3NEZWNsYXJhdGlvbjtcbiAgcmVmOiBSZWZlcmVuY2U7XG4gIHJhd0RlY2xhcmF0aW9uczogdHMuRXhwcmVzc2lvbnxudWxsO1xufVxuIl19