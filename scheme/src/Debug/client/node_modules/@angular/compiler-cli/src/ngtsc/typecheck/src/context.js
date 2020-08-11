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
        define("@angular/compiler-cli/src/ngtsc/typecheck/src/context", ["require", "exports", "tslib", "typescript", "@angular/compiler-cli/src/ngtsc/file_system", "@angular/compiler-cli/src/ngtsc/imports", "@angular/compiler-cli/src/ngtsc/translator", "@angular/compiler-cli/src/ngtsc/typecheck/src/dom", "@angular/compiler-cli/src/ngtsc/typecheck/src/environment", "@angular/compiler-cli/src/ngtsc/typecheck/src/oob", "@angular/compiler-cli/src/ngtsc/typecheck/src/shim", "@angular/compiler-cli/src/ngtsc/typecheck/src/source", "@angular/compiler-cli/src/ngtsc/typecheck/src/type_check_block", "@angular/compiler-cli/src/ngtsc/typecheck/src/type_check_file", "@angular/compiler-cli/src/ngtsc/typecheck/src/type_constructor"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.TypeCheckContext = void 0;
    var tslib_1 = require("tslib");
    var ts = require("typescript");
    var file_system_1 = require("@angular/compiler-cli/src/ngtsc/file_system");
    var imports_1 = require("@angular/compiler-cli/src/ngtsc/imports");
    var translator_1 = require("@angular/compiler-cli/src/ngtsc/translator");
    var dom_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/dom");
    var environment_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/environment");
    var oob_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/oob");
    var shim_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/shim");
    var source_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/source");
    var type_check_block_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/type_check_block");
    var type_check_file_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/type_check_file");
    var type_constructor_1 = require("@angular/compiler-cli/src/ngtsc/typecheck/src/type_constructor");
    /**
     * A template type checking context for a program.
     *
     * The `TypeCheckContext` allows registration of components and their templates which need to be
     * type checked.
     */
    var TypeCheckContext = /** @class */ (function () {
        function TypeCheckContext(config, compilerHost, refEmitter, reflector) {
            this.config = config;
            this.compilerHost = compilerHost;
            this.refEmitter = refEmitter;
            this.reflector = reflector;
            this.fileMap = new Map();
            /**
             * A `Map` of `ts.SourceFile`s that the context has seen to the operations (additions of methods
             * or type-check blocks) that need to be eventually performed on that file.
             */
            this.opMap = new Map();
            /**
             * Tracks when an a particular class has a pending type constructor patching operation already
             * queued.
             */
            this.typeCtorPending = new Set();
            /**
             * Map of data for file paths which was adopted from a prior compilation.
             *
             * This data allows the `TypeCheckContext` to generate a `TypeCheckRequest` which can interpret
             * diagnostics from type-checking shims included in the prior compilation.
             */
            this.adoptedFiles = new Map();
        }
        /**
         * Record the `FileTypeCheckingData` from a previous program that's associated with a particular
         * source file.
         */
        TypeCheckContext.prototype.adoptPriorResults = function (sf, data) {
            this.adoptedFiles.set(file_system_1.absoluteFromSourceFile(sf), data);
        };
        /**
         * Record a template for the given component `node`, with a `SelectorMatcher` for directive
         * matching.
         *
         * @param node class of the node being recorded.
         * @param template AST nodes of the template being recorded.
         * @param matcher `SelectorMatcher` which tracks directives that are in scope for this template.
         */
        TypeCheckContext.prototype.addTemplate = function (ref, boundTarget, pipes, schemas, sourceMapping, file) {
            var e_1, _a;
            var fileData = this.dataForFile(ref.node.getSourceFile());
            var id = fileData.sourceManager.captureSource(sourceMapping, file);
            try {
                // Get all of the directives used in the template and record type constructors for all of them.
                for (var _b = tslib_1.__values(boundTarget.getUsedDirectives()), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var dir = _c.value;
                    var dirRef = dir.ref;
                    var dirNode = dirRef.node;
                    if (type_constructor_1.requiresInlineTypeCtor(dirNode, this.reflector)) {
                        // Add a type constructor operation for the directive.
                        this.addInlineTypeCtor(fileData, dirNode.getSourceFile(), dirRef, {
                            fnName: 'ngTypeCtor',
                            // The constructor should have a body if the directive comes from a .ts file, but not if
                            // it comes from a .d.ts file. .d.ts declarations don't have bodies.
                            body: !dirNode.getSourceFile().isDeclarationFile,
                            fields: {
                                inputs: Object.keys(dir.inputs),
                                outputs: Object.keys(dir.outputs),
                                // TODO(alxhub): support queries
                                queries: dir.queries,
                            },
                            coercedInputFields: dir.coercedInputFields,
                        });
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
            var tcbMetadata = { id: id, boundTarget: boundTarget, pipes: pipes, schemas: schemas };
            if (type_check_block_1.requiresInlineTypeCheckBlock(ref.node)) {
                // This class didn't meet the requirements for external type checking, so generate an inline
                // TCB for the class.
                this.addInlineTypeCheckBlock(fileData, ref, tcbMetadata);
            }
            else {
                // The class can be type-checked externally as normal.
                fileData.typeCheckFile.addTypeCheckBlock(ref, tcbMetadata, fileData.domSchemaChecker, fileData.oobRecorder);
            }
        };
        /**
         * Record a type constructor for the given `node` with the given `ctorMetadata`.
         */
        TypeCheckContext.prototype.addInlineTypeCtor = function (fileData, sf, ref, ctorMeta) {
            if (this.typeCtorPending.has(ref.node)) {
                return;
            }
            this.typeCtorPending.add(ref.node);
            // Lazily construct the operation map.
            if (!this.opMap.has(sf)) {
                this.opMap.set(sf, []);
            }
            var ops = this.opMap.get(sf);
            // Push a `TypeCtorOp` into the operation queue for the source file.
            ops.push(new TypeCtorOp(ref, ctorMeta));
            fileData.hasInlines = true;
        };
        /**
         * Transform a `ts.SourceFile` into a version that includes type checking code.
         *
         * If this particular `ts.SourceFile` requires changes, the text representing its new contents
         * will be returned. Otherwise, a `null` return indicates no changes were necessary.
         */
        TypeCheckContext.prototype.transform = function (sf) {
            var _this = this;
            // If there are no operations pending for this particular file, return `null` to indicate no
            // changes.
            if (!this.opMap.has(sf)) {
                return null;
            }
            // Imports may need to be added to the file to support type-checking of directives used in the
            // template within it.
            var importManager = new translator_1.ImportManager(new imports_1.NoopImportRewriter(), '_i');
            // Each Op has a splitPoint index into the text where it needs to be inserted. Split the
            // original source text into chunks at these split points, where code will be inserted between
            // the chunks.
            var ops = this.opMap.get(sf).sort(orderOps);
            var textParts = splitStringAtPoints(sf.text, ops.map(function (op) { return op.splitPoint; }));
            // Use a `ts.Printer` to generate source code.
            var printer = ts.createPrinter({ omitTrailingSemicolon: true });
            // Begin with the intial section of the code text.
            var code = textParts[0];
            // Process each operation and use the printer to generate source code for it, inserting it into
            // the source code in between the original chunks.
            ops.forEach(function (op, idx) {
                var text = op.execute(importManager, sf, _this.refEmitter, printer);
                code += '\n\n' + text + textParts[idx + 1];
            });
            // Write out the imports that need to be added to the beginning of the file.
            var imports = importManager.getAllImports(sf.fileName)
                .map(function (i) { return "import * as " + i.qualifier + " from '" + i.specifier + "';"; })
                .join('\n');
            code = imports + '\n' + code;
            return code;
        };
        TypeCheckContext.prototype.finalize = function () {
            var e_2, _a, e_3, _b, e_4, _c;
            // First, build the map of updates to source files.
            var updates = new Map();
            try {
                for (var _d = tslib_1.__values(this.opMap.keys()), _e = _d.next(); !_e.done; _e = _d.next()) {
                    var originalSf = _e.value;
                    var newText = this.transform(originalSf);
                    if (newText !== null) {
                        updates.set(file_system_1.absoluteFromSourceFile(originalSf), newText);
                    }
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (_e && !_e.done && (_a = _d.return)) _a.call(_d);
                }
                finally { if (e_2) throw e_2.error; }
            }
            var results = {
                updates: updates,
                perFileData: new Map(),
            };
            try {
                for (var _f = tslib_1.__values(this.fileMap.entries()), _g = _f.next(); !_g.done; _g = _f.next()) {
                    var _h = tslib_1.__read(_g.value, 2), sfPath = _h[0], fileData = _h[1];
                    updates.set(fileData.typeCheckFile.fileName, fileData.typeCheckFile.render());
                    results.perFileData.set(sfPath, {
                        genesisDiagnostics: tslib_1.__spread(fileData.domSchemaChecker.diagnostics, fileData.oobRecorder.diagnostics),
                        hasInlines: fileData.hasInlines,
                        sourceResolver: fileData.sourceManager,
                        typeCheckFile: fileData.typeCheckFile.fileName,
                    });
                }
            }
            catch (e_3_1) { e_3 = { error: e_3_1 }; }
            finally {
                try {
                    if (_g && !_g.done && (_b = _f.return)) _b.call(_f);
                }
                finally { if (e_3) throw e_3.error; }
            }
            try {
                for (var _j = tslib_1.__values(this.adoptedFiles.entries()), _k = _j.next(); !_k.done; _k = _j.next()) {
                    var _l = tslib_1.__read(_k.value, 2), sfPath = _l[0], fileData = _l[1];
                    results.perFileData.set(sfPath, fileData);
                }
            }
            catch (e_4_1) { e_4 = { error: e_4_1 }; }
            finally {
                try {
                    if (_k && !_k.done && (_c = _j.return)) _c.call(_j);
                }
                finally { if (e_4) throw e_4.error; }
            }
            return results;
        };
        TypeCheckContext.prototype.addInlineTypeCheckBlock = function (fileData, ref, tcbMeta) {
            var sf = ref.node.getSourceFile();
            if (!this.opMap.has(sf)) {
                this.opMap.set(sf, []);
            }
            var ops = this.opMap.get(sf);
            ops.push(new TcbOp(ref, tcbMeta, this.config, this.reflector, fileData.domSchemaChecker, fileData.oobRecorder));
            fileData.hasInlines = true;
        };
        TypeCheckContext.prototype.dataForFile = function (sf) {
            var sfPath = file_system_1.absoluteFromSourceFile(sf);
            if (!this.fileMap.has(sfPath)) {
                var sourceManager = new source_1.TemplateSourceManager();
                var data = {
                    domSchemaChecker: new dom_1.RegistryDomSchemaChecker(sourceManager),
                    oobRecorder: new oob_1.OutOfBandDiagnosticRecorderImpl(sourceManager),
                    typeCheckFile: new type_check_file_1.TypeCheckFile(shim_1.TypeCheckShimGenerator.shimFor(sfPath), this.config, this.refEmitter, this.reflector, this.compilerHost),
                    hasInlines: false,
                    sourceManager: sourceManager,
                };
                this.fileMap.set(sfPath, data);
            }
            return this.fileMap.get(sfPath);
        };
        return TypeCheckContext;
    }());
    exports.TypeCheckContext = TypeCheckContext;
    /**
     * A type check block operation which produces type check code for a particular component.
     */
    var TcbOp = /** @class */ (function () {
        function TcbOp(ref, meta, config, reflector, domSchemaChecker, oobRecorder) {
            this.ref = ref;
            this.meta = meta;
            this.config = config;
            this.reflector = reflector;
            this.domSchemaChecker = domSchemaChecker;
            this.oobRecorder = oobRecorder;
        }
        Object.defineProperty(TcbOp.prototype, "splitPoint", {
            /**
             * Type check blocks are inserted immediately after the end of the component class.
             */
            get: function () {
                return this.ref.node.end + 1;
            },
            enumerable: false,
            configurable: true
        });
        TcbOp.prototype.execute = function (im, sf, refEmitter, printer) {
            var env = new environment_1.Environment(this.config, im, refEmitter, this.reflector, sf);
            var fnName = ts.createIdentifier("_tcb_" + this.ref.node.pos);
            var fn = type_check_block_1.generateTypeCheckBlock(env, this.ref, fnName, this.meta, this.domSchemaChecker, this.oobRecorder);
            return printer.printNode(ts.EmitHint.Unspecified, fn, sf);
        };
        return TcbOp;
    }());
    /**
     * A type constructor operation which produces type constructor code for a particular directive.
     */
    var TypeCtorOp = /** @class */ (function () {
        function TypeCtorOp(ref, meta) {
            this.ref = ref;
            this.meta = meta;
        }
        Object.defineProperty(TypeCtorOp.prototype, "splitPoint", {
            /**
             * Type constructor operations are inserted immediately before the end of the directive class.
             */
            get: function () {
                return this.ref.node.end - 1;
            },
            enumerable: false,
            configurable: true
        });
        TypeCtorOp.prototype.execute = function (im, sf, refEmitter, printer) {
            var tcb = type_constructor_1.generateInlineTypeCtor(this.ref.node, this.meta);
            return printer.printNode(ts.EmitHint.Unspecified, tcb, sf);
        };
        return TypeCtorOp;
    }());
    /**
     * Compare two operations and return their split point ordering.
     */
    function orderOps(op1, op2) {
        return op1.splitPoint - op2.splitPoint;
    }
    /**
     * Split a string into chunks at any number of split points.
     */
    function splitStringAtPoints(str, points) {
        var splits = [];
        var start = 0;
        for (var i = 0; i < points.length; i++) {
            var point = points[i];
            splits.push(str.substring(start, point));
            start = point;
        }
        splits.push(str.substring(start));
        return splits;
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY29udGV4dC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2NvbXBpbGVyLWNsaS9zcmMvbmd0c2MvdHlwZWNoZWNrL3NyYy9jb250ZXh0LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7Ozs7Ozs7Ozs7Ozs7SUFHSCwrQkFBaUM7SUFFakMsMkVBQXlFO0lBQ3pFLG1FQUE4RTtJQUU5RSx5RUFBK0M7SUFJL0MseUVBQWlFO0lBQ2pFLHlGQUEwQztJQUMxQyx5RUFBbUY7SUFDbkYsMkVBQThDO0lBQzlDLCtFQUErQztJQUMvQyxtR0FBd0Y7SUFDeEYsaUdBQWdEO0lBQ2hELG1HQUFrRjtJQWdGbEY7Ozs7O09BS0c7SUFDSDtRQUdFLDBCQUNZLE1BQTBCLEVBQzFCLFlBQTJELEVBQzNELFVBQTRCLEVBQVUsU0FBeUI7WUFGL0QsV0FBTSxHQUFOLE1BQU0sQ0FBb0I7WUFDMUIsaUJBQVksR0FBWixZQUFZLENBQStDO1lBQzNELGVBQVUsR0FBVixVQUFVLENBQWtCO1lBQVUsY0FBUyxHQUFULFNBQVMsQ0FBZ0I7WUFMbkUsWUFBTyxHQUFHLElBQUksR0FBRyxFQUErQyxDQUFDO1lBT3pFOzs7ZUFHRztZQUNLLFVBQUssR0FBRyxJQUFJLEdBQUcsRUFBdUIsQ0FBQztZQUUvQzs7O2VBR0c7WUFDSyxvQkFBZSxHQUFHLElBQUksR0FBRyxFQUF1QixDQUFDO1lBRXpEOzs7OztlQUtHO1lBQ0ssaUJBQVksR0FBRyxJQUFJLEdBQUcsRUFBd0MsQ0FBQztRQXBCTyxDQUFDO1FBc0IvRTs7O1dBR0c7UUFDSCw0Q0FBaUIsR0FBakIsVUFBa0IsRUFBaUIsRUFBRSxJQUEwQjtZQUM3RCxJQUFJLENBQUMsWUFBWSxDQUFDLEdBQUcsQ0FBQyxvQ0FBc0IsQ0FBQyxFQUFFLENBQUMsRUFBRSxJQUFJLENBQUMsQ0FBQztRQUMxRCxDQUFDO1FBRUQ7Ozs7Ozs7V0FPRztRQUNILHND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eEIsTUFBTSxDQUFDLElBQUksQ0FBQyxHQUFHLENBQUMsU0FBUyxDQUFDLEtBQUssRUFBRSxLQUFLLENBQUMsQ0FBQyxDQUFDO1lBQ3pDLEtBQUssR0FBRyxLQUFLLENBQUM7U0FDZjtRQUNELE1BQU0sQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLFNBQVMsQ0FBQyxLQUFLLENBQUMsQ0FBQyxDQUFDO1FBQ2xDLE9BQU8sTUFBTSxDQUFDO0lBQ2hCLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtCb3VuZFRhcmdldCwgUGFyc2VTb3VyY2VGaWxlLCBTY2hlbWFNZXRhZGF0YX0gZnJvbSAnQGFuZ3VsYXIvY29tcGlsZXInO1xuaW1wb3J0ICogYXMgdHMgZnJvbSAndHlwZXNjcmlwdCc7XG5cbmltcG9ydCB7YWJzb2x1dGVGcm9tU291cmNlRmlsZSwgQWJzb2x1dGVGc1BhdGh9IGZyb20gJy4uLy4uL2ZpbGVfc3lzdGVtJztcbmltcG9ydCB7Tm9vcEltcG9ydFJld3JpdGVyLCBSZWZlcmVuY2UsIFJlZmVyZW5jZUVtaXR0ZXJ9IGZyb20gJy4uLy4uL2ltcG9ydHMnO1xuaW1wb3J0IHtDbGFzc0RlY2xhcmF0aW9uLCBSZWZsZWN0aW9uSG9zdH0gZnJvbSAnLi4vLi4vcmVmbGVjdGlvbic7XG5pbXBvcnQge0ltcG9ydE1hbmFnZXJ9IGZyb20gJy4uLy4uL3RyYW5zbGF0b3InO1xuXG5pbXBvcnQge1RlbXBsYXRlU291cmNlTWFwcGluZywgVHlwZUNoZWNrYWJsZURpcmVjdGl2ZU1ldGEsIFR5cGVDaGVja0Jsb2NrTWV0YWRhdGEsIFR5cGVDaGVja2luZ0NvbmZpZywgVHlwZUN0b3JNZXRhZGF0YX0gZnJvbSAnLi9hcGknO1xuaW1wb3J0IHtUZW1wbGF0ZVNvdXJjZVJlc29sdmVyfSBmcm9tICcuL2RpYWdub3N0aWNzJztcbmltcG9ydCB7RG9tU2NoZW1hQ2hlY2tlciwgUmVnaXN0cnlEb21TY2hlbWFDaGVja2VyfSBmcm9tICcuL2RvbSc7XG5pbXBvcnQge0Vudmlyb25tZW50fSBmcm9tICcuL2Vudmlyb25tZW50JztcbmltcG9ydCB7T3V0T2ZCYW5kRGlhZ25vc3RpY1JlY29yZGVyLCBPdXRPZkJhbmREaWFnbm9zdGljUmVjb3JkZXJJbXBsfSBmcm9tICcuL29vYic7XG5pbXBvcnQge1R5cGVDaGVja1NoaW1HZW5lcmF0b3J9IGZyb20gJy4vc2hpbSc7XG5pbXBvcnQge1RlbXBsYXRlU291cmNlTWFuYWdlcn0gZnJvbSAnLi9zb3VyY2UnO1xuaW1wb3J0IHtnZW5lcmF0ZVR5cGVDaGVja0Jsb2NrLCByZXF1aXJlc0lubGluZVR5cGVDaGVja0Jsb2NrfSBmcm9tICcuL3R5cGVfY2hlY2tfYmxvY2snO1xuaW1wb3J0IHtUeXBlQ2hlY2tGaWxlfSBmcm9tICcuL3R5cGVfY2hlY2tfZmlsZSc7XG5pbXBvcnQge2dlbmVyYXRlSW5saW5lVHlwZUN0b3IsIHJlcXVpcmVzSW5saW5lVHlwZUN0b3J9IGZyb20gJy4vdHlwZV9jb25zdHJ1Y3Rvcic7XG5cbi8qKlxuICogQ29tcGxldGUgdHlwZS1jaGVja2luZyBjb2RlIGdlbmVyYXRlZCBmb3IgdGhlIHVzZXIncyBwcm9ncmFtLCByZWFkeSBmb3IgaW5wdXQgaW50byB0aGVcbiAqIHR5cGUtY2hlY2tpbmcgZW5naW5lLlxuICovXG5leHBvcnQgaW50ZXJmYWNlIFR5cGVDaGVja1JlcXVlc3Qge1xuICAvKipcbiAgICogTWFwIG9mIHNvdXJjZSBmaWxlbmFtZXMgdG8gbmV3IGNvbnRlbnRzIGZvciB0aG9zZSBmaWxlcy5cbiAgICpcbiAgICogVGhpcyBpbmNsdWRlcyBib3RoIGNvbnRlbnRzIG9mIHR5cGUtY2hlY2tpbmcgc2hpbSBmaWxlcywgYXMgd2VsbCBhcyBjaGFuZ2VzIHRvIGFueSB1c2VyIGZpbGVzXG4gICAqIHdoaWNoIG5lZWRlZCB0byBiZSBtYWRlIHRvIHN1cHBvcnQgdGVtcGxhdGUgdHlwZS1jaGVja2luZy5cbiAgICovXG4gIHVwZGF0ZXM6IE1hcDxBYnNvbHV0ZUZzUGF0aCwgc3RyaW5nPjtcblxuICAvKipcbiAgICogTWFwIGNvbnRhaW5pbmcgYWRkaXRpb25hbCBkYXRhIGZvciBlYWNoIHR5cGUtY2hlY2tpbmcgc2hpbSB0aGF0IGlzIHJlcXVpcmVkIHRvIHN1cHBvcnRcbiAgICogZ2VuZXJhdGlvbiBvZiBkaWFnbm9zdGljcy5cbiAgICovXG4gIHBlckZpbGVEYXRhOiBNYXA8QWJzb2x1dGVGc1BhdGgsIEZpbGVUeXBlQ2hlY2tpbmdEYXRhPjtcbn1cblxuLyoqXG4gKiBEYXRhIGZvciBhIHR5cGUtY2hlY2tpbmcgc2hpbSB3aGljaCBpcyByZXF1aXJlZCB0byBzdXBwb3J0IGdlbmVyYXRpb24gb2YgZGlhZ25vc3RpY3MuXG4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgRmlsZVR5cGVDaGVja2luZ0RhdGEge1xuICAvKipcbiAgICogV2hldGhlciB0aGUgdHlwZS1jaGVja2luZyBzaGltIHJlcXVpcmVkIGFueSBpbmxpbmUgY2hhbmdlcyB0byB0aGUgb3JpZ2luYWwgZmlsZSwgd2hpY2ggYWZmZWN0c1xuICAgKiB3aGV0aGVyIHRoZSBzaGltIGNhbiBiZSByZXVzZWQuXG4gICAqL1xuICBoYXNJbmxpbmVzOiBib29sZWFuO1xuXG4gIC8qKlxuICAgKiBTb3VyY2UgbWFwcGluZyBpbmZvcm1hdGlvbiBmb3IgbWFwcGluZyBkaWFnbm9zdGljcyBiYWNrIHRvIHRoZSBvcmlnaW5hbCB0ZW1wbGF0ZS5cbiAgICovXG4gIHNvdXJjZVJlc29sdmVyOiBUZW1wbGF0ZVNvdXJjZVJlc29sdmVyO1xuXG4gIC8qKlxuICAgKiBBbnkgYHRzLkRpYWdub3N0aWNgcyB3aGljaCB3ZXJlIHByb2R1Y2VkIGR1cmluZyB0aGUgZ2VuZXJhdGlvbiBvZiB0aGlzIHNoaW0uXG4gICAqXG4gICAqIFNvbWUgZGlhZ25vc3RpY3MgYXJlIHByb2R1Y2VkIGR1cmluZyBjcmVhdGlvbiB0aW1lIGFuZCBhcmUgdHJhY2tlZCBoZXJlLlxuICAgKi9cbiAgZ2VuZXNpc0RpYWdub3N0aWNzOiB0cy5EaWFnbm9zdGljW107XG5cbiAgLyoqXG4gICAqIFBhdGggdG8gdGhlIHNoaW0gZmlsZS5cbiAgICovXG4gIHR5cGVDaGVja0ZpbGU6IEFic29sdXRlRnNQYXRoO1xufVxuXG4vKipcbiAqIERhdGEgZm9yIGEgdHlwZS1jaGVja2luZyBzaGltIHdoaWNoIGlzIHN0aWxsIGhhdmluZyBpdHMgY29kZSBnZW5lcmF0ZWQuXG4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgUGVuZGluZ0ZpbGVUeXBlQ2hlY2tpbmdEYXRhIHtcbiAgLyoqXG4gICAqIFdoZXRoZXIgYW55IGlubGluZSBjb2RlIGhhcyBiZWVuIHJlcXVpcmVkIGJ5IHRoZSBzaGltIHlldC5cbiAgICovXG4gIGhhc0lubGluZXM6IGJvb2xlYW47XG5cbiAgLyoqXG4gICAqIGBUZW1wbGF0ZVNvdXJjZU1hbmFnZXJgIGJlaW5nIHVzZWQgdG8gdHJhY2sgc291cmNlIG1hcHBpbmcgaW5mb3JtYXRpb24gZm9yIHRoaXMgc2hpbS5cbiAgICovXG4gIHNvdXJjZU1hbmFnZXI6IFRlbXBsYXRlU291cmNlTWFuYWdlcjtcblxuICAvKipcbiAgICogUmVjb3JkZXIgZm9yIG91dC1vZi1iYW5kIGRpYWdub3N0aWNzIHdoaWNoIGFyZSByYWlzZWQgZHVyaW5nIGdlbmVyYXRpb24uXG4gICAqL1xuICBvb2JSZWNvcmRlcjogT3V0T2ZCYW5kRGlhZ25vc3RpY1JlY29yZGVyO1xuXG4gIC8qKlxuICAgKiBUaGUgYERvbVNjaGVtYUNoZWNrZXJgIGluIHVzZSBmb3IgdGhpcyB0ZW1wbGF0ZSwgd2hpY2ggcmVjb3JkcyBhbnkgc2NoZW1hLXJlbGF0ZWQgZGlhZ25vc3RpY3MuXG4gICAqL1xuICBkb21TY2hlbWFDaGVja2VyOiBEb21TY2hlbWFDaGVja2VyO1xuXG4gIC8qKlxuICAgKiBQYXRoIHRvIHRoZSBzaGltIGZpbGUuXG4gICAqL1xuICB0eXBlQ2hlY2tGaWxlOiBUeXBlQ2hlY2tGaWxlO1xufVxuXG4vKipcbiAqIEEgdGVtcGxhdGUgdHlwZSBjaGVja2luZyBjb250ZXh0IGZvciBhIHByb2dyYW0uXG4gKlxuICogVGhlIGBUeXBlQ2hlY2tDb250ZXh0YCBhbGxvd3MgcmVnaXN0cmF0aW9uIG9mIGNvbXBvbmVudHMgYW5kIHRoZWlyIHRlbXBsYXRlcyB3aGljaCBuZWVkIHRvIGJlXG4gKiB0eXBlIGNoZWNrZWQuXG4gKi9cbmV4cG9ydCBjbGFzcyBUeXBlQ2hlY2tDb250ZXh0IHtcbiAgcHJpdmF0ZSBmaWxlTWFwID0gbmV3IE1hcDxBYnNvbHV0ZUZzUGF0aCwgUGVuZGluZ0ZpbGVUeXBlQ2hlY2tpbmdEYXRhPigpO1xuXG4gIGNvbnN0cnVjdG9yKFxuICAgICAgcHJpdmF0ZSBjb25maWc6IFR5cGVDaGVja2luZ0NvbmZpZyxcbiAgICAgIHByaXZhdGUgY29tcGlsZXJIb3N0OiBQaWNrPHRzLkNvbXBpbGVySG9zdCwgJ2dldENhbm9uaWNhbEZpbGVOYW1lJz4sXG4gICAgICBwcml2YXRlIHJlZkVtaXR0ZXI6IFJlZmVyZW5jZUVtaXR0ZXIsIHByaXZhdGUgcmVmbGVjdG9yOiBSZWZsZWN0aW9uSG9zdCkge31cblxuICAvKipcbiAgICogQSBgTWFwYCBvZiBgdHMuU291cmNlRmlsZWBzIHRoYXQgdGhlIGNvbnRleHQgaGFzIHNlZW4gdG8gdGhlIG9wZXJhdGlvbnMgKGFkZGl0aW9ucyBvZiBtZXRob2RzXG4gICAqIG9yIHR5cGUtY2hlY2sgYmxvY2tzKSB0aGF0IG5lZWQgdG8gYmUgZXZlbnR1YWxseSBwZXJmb3JtZWQgb24gdGhhdCBmaWxlLlxuICAgKi9cbiAgcHJpdmF0ZSBvcE1hcCA9IG5ldyBNYXA8dHMuU291cmNlRmlsZSwgT3BbXT4oKTtcblxuICAvKipcbiAgICogVHJhY2tzIHdoZW4gYW4gYSBwYXJ0aWN1bGFyIGNsYXNzIGhhcyBhIHBlbmRpbmcgdHlwZSBjb25zdHJ1Y3RvciBwYXRjaGluZyBvcGVyYXRpb24gYWxyZWFkeVxuICAgKiBxdWV1ZWQuXG4gICAqL1xuICBwcml2YXRlIHR5cGVDdG9yUGVuZGluZyA9IG5ldyBTZXQ8dHMuQ2xhc3NEZWNsYXJhdGlvbj4oKTtcblxuICAvKipcbiAgICogTWFwIG9mIGRhdGEgZm9yIGZpbGUgcGF0aHMgd2hpY2ggd2FzIGFkb3B0ZWQgZnJvbSBhIHByaW9yIGNvbXBpbGF0aW9uLlxuICAgKlxuICAgKiBUaGlzIGRhdGEgYWxsb3dzIHRoZSBgVHlwZUNoZWNrQ29udGV4dGAgdG8gZ2VuZXJhdGUgYSBgVHlwZUNoZWNrUmVxdWVzdGAgd2hpY2ggY2FuIGludGVycHJldFxuICAgKiBkaWFnbm9zdGljcyBmcm9tIHR5cGUtY2hlY2tpbmcgc2hpbXMgaW5jbHVkZWQgaW4gdGhlIHByaW9yIGNvbXBpbGF0aW9uLlxuICAgKi9cbiAgcHJpdmF0ZSBhZG9wdGVkRmlsZXMgPSBuZXcgTWFwPEFic29sdXRlRnNQYXRoLCBGaWxlVHlwZUNoZWNraW5nRGF0YT4oKTtcblxuICAvKipcbiAgICogUmVjb3JkIHRoZSBgRmlsZVR5cGVDaGVja2luZ0RhdGFgIGZyb20gYSBwcmV2aW91cyBwcm9ncmFtIHRoYXQncyBhc3NvY2lhdGVkIHdpdGggYSBwYXJ0aWN1bGFyXG4gICAqIHNvdXJjZSBmaWxlLlxuICAgKi9cbiAgYWRvcHRQcmlvclJlc3VsdHMoc2Y6IHRzLlNvdXJjZUZpbGUsIGRhdGE6IEZpbGVUeXBlQ2hlY2tpbmdEYXRhKTogdm9pZCB7XG4gICAgdGhpcy5hZG9wdGVkRmlsZXMuc2V0KGFic29sdXRlRnJvbVNvdXJjZUZpbGUoc2YpLCBkYXRhKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZWNvcmQgYSB0ZW1wbGF0ZSBmb3IgdGhlIGdpdmVuIGNvbXBvbmVudCBgbm9kZWAsIHdpdGggYSBgU2VsZWN0b3JNYXRjaGVyYCBmb3IgZGlyZWN0aXZlXG4gICAqIG1hdGNoaW5nLlxuICAgKlxuICAgKiBAcGFyYW0gbm9kZSBjbGFzcyBvZiB0aGUgbm9kZSBiZWluZyByZWNvcmRlZC5cbiAgICogQHBhcmFtIHRlbXBsYXRlIEFTVCBub2RlcyBvZiB0aGUgdGVtcGxhdGUgYmVpbmcgcmVjb3JkZWQuXG4gICAqIEBwYXJhbSBtYXRjaGVyIGBTZWxlY3Rvck1hdGNoZXJgIHdoaWNoIHRyYWNrcyBkaXJlY3RpdmVzIHRoYXQgYXJlIGluIHNjb3BlIGZvciB0aGlzIHRlbXBsYXRlLlxuICAgKi9cbiAgYWRkVGVtcGxhdGUoXG4gICAgICByZWY6IFJlZmVyZW5jZTxDbGFzc0RlY2xhcmF0aW9uPHRzLkNsYXNzRGVjbGFyYXRpb24+PixcbiAgICAgIGJvdW5kVGFyZ2V0OiBCb3VuZFRhcmdldDxUeXBlQ2hlY2thYmxlRGlyZWN0aXZlTWV0YT4sXG4gICAgICBwaXBlczogTWFwPHN0cmluZywgUmVmZXJlbmNlPENsYXNzRGVjbGFyYXRpb248dHMuQ2xhc3NEZWNsYXJhdGlvbj4+PixcbiAgICAgIHNjaGVtYXM6IFNjaGVtYU1ldGFkYXRhW10sIHNvdXJjZU1hcHBpbmc6IFRlbXBsYXRlU291cmNlTWFwcGluZyxcbiAgICAgIGZpbGU6IFBhcnNlU291cmNlRmlsZSk6IHZvaWQge1xuICAgIGNvbnN0IGZpbGVEYXRhID0gdGhpcy5kYXRhRm9yRmlsZShyZWYubm9kZS5nZXRTb3VyY2VGaWxlKCkpO1xuXG4gICAgY29uc3QgaWQgPSBmaWxlRGF0YS5zb3VyY2VNYW5hZ2VyLmNhcHR1cmVTb3VyY2Uoc291cmNlTWFwcGluZywgZmlsZSk7XG4gICAgLy8gR2V0IGFsbCBvZiB0aGUgZGlyZWN0aXZlcyB1c2VkIGluIHRoZSB0ZW1wbGF0ZSBhbmQgcmVjb3JkIHR5cGUgY29uc3RydWN0b3JzIGZvciBhbGwgb2YgdGhlbS5cbiAgICBmb3IgKGNvbnN0IGRpciBvZiBib3VuZFRhcmdldC5nZXRVc2VkRGlyZWN0aXZlcygpKSB7XG4gICAgICBjb25zdCBkaXJSZWYgPSBkaXIucmVmIGFzIFJlZmVyZW5jZTxDbGFzc0RlY2xhcmF0aW9uPHRzLkNsYXNzRGVjbGFyYXRpb24+PjtcbiAgICAgIGNvbnN0IGRpck5vZGUgPSBkaXJSZWYubm9kZTtcbiAgICAgIGlmIChyZXF1aXJlc0lubGluZVR5cGVDdG9yKGRpck5vZGUsIHRoaXMucmVmbGVjdG9yKSkge1xuICAgICAgICAvLyBBZGQgYSB0eXBlIGNvbnN0cnVjdG9yIG9wZXJhdGlvbiBmb3IgdGhlIGRpcmVjdGl2ZS5cbiAgICAgICAgdGhpcy5hZGRJbmxpbmVUeXBlQ3RvcihmaWxlRGF0YSwgZGlyTm9kZS5nZXRTb3VyY2VGaWxlKCksIGRpclJlZiwge1xuICAgICAgICAgIGZuTmFtZTogJ25nVHlwZUN0b3InLFxuICAgICAgICAgIC8vIFRoZSBjb25zdHJ1Y3RvciBzaG91bGQgaGF2ZSBhIGJvZHkgaWYgdGhlIGRpcmVjdGl2ZSBjb21lcyBmcm9tIGEgLnRzIGZpbGUsIGJ1dCBub3QgaWZcbiAgICAgICAgICAvLyBpdCBjb21lcyBmcm9tIGEgLmQudHMgZmlsZS4gLmQudHMgZGVjbGFyYXRpb25zIGRvbid0IGhhdmUgYm9kaWVzLlxuICAgICAgICAgIGJvZHk6ICFkaXJOb2RlLmdldFNvdXJjZUZpbGUoKS5pc0RlY2xhcmF0aW9uRmlsZSxcbiAgICAgICAgICBmaWVsZHM6IHtcbiAgICAgICAgICAgIGlucHV0czogT2JqZWN0LmtleXMoZGlyLmlucHV0cyksXG4gICAgICAgICAgICBvdXRwdXRzOiBPYmplY3Qua2V5cyhkaXIub3V0cHV0cyksXG4gICAgICAgICAgICAvLyBUT0RPKGFseGh1Yik6IHN1cHBvcnQgcXVlcmllc1xuICAgICAgICAgICAgcXVlcmllczogZGlyLnF1ZXJpZXMsXG4gICAgICAgICAgfSxcbiAgICAgICAgICBjb2VyY2VkSW5wdXRGaWVsZHM6IGRpci5jb2VyY2VkSW5wdXRGaWVsZHMsXG4gICAgICAgIH0pO1xuICAgICAgfVxuICAgIH1cblxuICAgIGNvbnN0IHRjYk1ldGFkYXRhOiBUeXBlQ2hlY2tCbG9ja01ldGFkYXRhID0ge2lkLCBib3VuZFRhcmdldCwgcGlwZXMsIHNjaGVtYXN9O1xuICAgIGlmIChyZXF1aXJlc0lubGluZVR5cGVDaGVja0Jsb2NrKHJlZi5ub2RlKSkge1xuICAgICAgLy8gVGhpcyBjbGFzcyBkaWRuJ3QgbWVldCB0aGUgcmVxdWlyZW1lbnRzIGZvciBleHRlcm5hbCB0eXBlIGNoZWNraW5nLCBzbyBnZW5lcmF0ZSBhbiBpbmxpbmVcbiAgICAgIC8vIFRDQiBmb3IgdGhlIGNsYXNzLlxuICAgICAgdGhpcy5hZGRJbmxpbmVUeXBlQ2hlY2tCbG9jayhmaWxlRGF0YSwgcmVmLCB0Y2JNZXRhZGF0YSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIC8vIFRoZSBjbGFzcyBjYW4gYmUgdHlwZS1jaGVja2VkIGV4dGVybmFsbHkgYXMgbm9ybWFsLlxuICAgICAgZmlsZURhdGEudHlwZUNoZWNrRmlsZS5hZGRUeXBlQ2hlY2tCbG9jayhcbiAgICAgICAgICByZWYsIHRjYk1ldGFkYXRhLCBmaWxlRGF0YS5kb21TY2hlbWFDaGVja2VyLCBmaWxlRGF0YS5vb2JSZWNvcmRlcik7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIFJlY29yZCBhIHR5cGUgY29uc3RydWN0b3IgZm9yIHRoZSBnaXZlbiBgbm9kZWAgd2l0aCB0aGUgZ2l2ZW4gYGN0b3JNZXRhZGF0YWAuXG4gICAqL1xuICBhZGRJbmxpbmVUeXBlQ3RvcihcbiAgICAgIGZpbGVEYXRhOiBQZW5kaW5nRmlsZVR5cGVDaGVja2luZ0RhdGEsIHNmOiB0cy5Tb3VyY2VGaWxlLFxuICAgICAgcmVmOiBSZWZlcmVuY2U8Q2xhc3NEZWNsYXJhdGlvbjx0cy5DbGFzc0RlY2xhcmF0aW9uPj4sIGN0b3JNZXRhOiBUeXBlQ3Rvck1ldGFkYXRhKTogdm9pZCB7XG4gICAgaWYgKHRoaXMudHlwZUN0b3JQZW5kaW5nLmhhcyhyZWYubm9kZSkpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG4gICAgdGhpcy50eXBlQ3RvclBlbmRpbmcuYWRkKHJlZi5ub2RlKTtcblxuICAgIC8vIExhemlseSBjb25zdHJ1Y3QgdGhlIG9wZXJhdGlvbiBtYXAuXG4gICAgaWYgKCF0aGlzLm9wTWFwLmhhcyhzZikpIHtcbiAgICAgIHRoaXMub3BNYXAuc2V0KHNmLCBbXSk7XG4gICAgfVxuICAgIGNvbnN0IG9wcyA9IHRoaXMub3BNYXAuZ2V0KHNmKSE7XG5cbiAgICAvLyBQdXNoIGEgYFR5cGVDdG9yT3BgIGludG8gdGhlIG9wZXJhdGlvbiBxdWV1ZSBmb3IgdGhlIHNvdXJjZSBmaWxlLlxuICAgIG9wcy5wdXNoKG5ldyBUeXBlQ3Rvck9wKHJlZiwgY3Rvck1ldGEpKTtcbiAgICBmaWxlRGF0YS5oYXNJbmxpbmVzID0gdHJ1ZTtcbiAgfVxuXG4gIC8qKlxuICAgKiBUcmFuc2Zvcm0gYSBgdHMuU291cmNlRmlsZWAgaW50byBhIHZlcnNpb24gdGhhdCBpbmNsdWRlcyB0eXBlIGNoZWNraW5nIGNvZGUuXG4gICAqXG4gICAqIElmIHRoaXMgcGFydGljdWxhciBgdHMuU291cmNlRmlsZWAgcmVxdWlyZXMgY2hhbmdlcywgdGhlIHRleHQgcmVwcmVzZW50aW5nIGl0cyBuZXcgY29udGVudHNcbiAgICogd2lsbCBiZSByZXR1cm5lZC4gT3RoZXJ3aXNlLCBhIGBudWxsYCByZXR1cm4gaW5kaWNhdGVzIG5vIGNoYW5nZXMgd2VyZSBuZWNlc3NhcnkuXG4gICAqL1xuICB0cmFuc2Zvcm0oc2Y6IHRzLlNvdXJjZUZpbGUpOiBzdHJpbmd8bnVsbCB7XG4gICAgLy8gSWYgdGhlcmUgYXJlIG5vIG9wZXJhdGlvbnMgcGVuZGluZyBmb3IgdGhpcyBwYXJ0aWN1bGFyIGZpbGUsIHJldHVybiBgbnVsbGAgdG8gaW5kaWNhdGUgbm9cbiAgICAvLyBjaGFuZ2VzLlxuICAgIGlmICghdGhpcy5vcE1hcC5oYXMoc2YpKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG5cbiAgICAvLyBJbXBvcnRzIG1heSBuZWVkIHRvIGJlIGFkZGVkIHRvIHRoZSBmaWxlIHRvIHN1cHBvcnQgdHlwZS1jaGVja2luZyBvZiBkaXJlY3RpdmVzIHVzZWQgaW4gdGhlXG4gICAgLy8gdGVtcGxhdGUgd2l0aGluIGl0LlxuICAgIGNvbnN0IGltcG9ydE1hbmFnZXIgPSBuZXcgSW1wb3J0TWFuYWdlcihuZXcgTm9vcEltcG9ydFJld3JpdGVyKCksICdfaScpO1xuXG4gICAgLy8gRWFjaCBPcCBoYXMgYSBzcGxpdFBvaW50IGluZGV4IGludG8gdGhlIHRleHQgd2hlcmUgaXQgbmVlZHMgdG8gYmUgaW5zZXJ0ZWQuIFNwbGl0IHRoZVxuICAgIC8vIG9yaWdpbmFsIHNvdXJjZSB0ZXh0IGludG8gY2h1bmtzIGF0IHRoZXNlIHNwbGl0IHBvaW50cywgd2hlcmUgY29kZSB3aWxsIGJlIGluc2VydGVkIGJldHdlZW5cbiAgICAvLyB0aGUgY2h1bmtzLlxuICAgIGNvbnN0IG9wcyA9IHRoaXMub3BNYXAuZ2V0KHNmKSEuc29ydChvcmRlck9wcyk7XG4gICAgY29uc3QgdGV4dFBhcnRzID0gc3BsaXRTdHJpbmdBdFBvaW50cyhzZi50ZXh0LCBvcHMubWFwKG9wID0+IG9wLnNwbGl0UG9pbnQpKTtcblxuICAgIC8vIFVzZSBhIGB0cy5QcmludGVyYCB0byBnZW5lcmF0ZSBzb3VyY2UgY29kZS5cbiAgICBjb25zdCBwcmludGVyID0gdHMuY3JlYXRlUHJpbnRlcih7b21pdFRyYWlsaW5nU2VtaWNvbG9uOiB0cnVlfSk7XG5cbiAgICAvLyBCZWdpbiB3aXRoIHRoZSBpbnRpYWwgc2VjdGlvbiBvZiB0aGUgY29kZSB0ZXh0LlxuICAgIGxldCBjb2RlID0gdGV4dFBhcnRzWzBdO1xuXG4gICAgLy8gUHJvY2VzcyBlYWNoIG9wZXJhdGlvbiBhbmQgdXNlIHRoZSBwcmludGVyIHRvIGdlbmVyYXRlIHNvdXJjZSBjb2RlIGZvciBpdCwgaW5zZXJ0aW5nIGl0IGludG9cbiAgICAvLyB0aGUgc291cmNlIGNvZGUgaW4gYmV0d2VlbiB0aGUgb3JpZ2luYWwgY2h1bmtzLlxuICAgIG9wcy5mb3JFYWNoKChvcCwgaWR4KSA9PiB7XG4gICAgICBjb25zdCB0ZXh0ID0gb3AuZXhlY3V0ZShpbXBvcnRNYW5hZ2VyLCBzZiwgdGhpcy5yZWZFbWl0dGVyLCBwcmludGVyKTtcbiAgICAgIGNvZGUgKz0gJ1xcblxcbicgKyB0ZXh0ICsgdGV4dFBhcnRzW2lkeCArIDFdO1xuICAgIH0pO1xuXG4gICAgLy8gV3JpdGUgb3V0IHRoZSBpbXBvcnRzIHRoYXQgbmVlZCB0byBiZSBhZGRlZCB0byB0aGUgYmVnaW5uaW5nIG9mIHRoZSBmaWxlLlxuICAgIGxldCBpbXBvcnRzID0gaW1wb3J0TWFuYWdlci5nZXRBbGxJbXBvcnRzKHNmLmZpbGVOYW1lKVxuICAgICAgICAgICAgICAgICAgICAgIC5tYXAoaSA9PiBgaW1wb3J0ICogYXMgJHtpLnF1YWxpZmllcn0gZnJvbSAnJHtpLnNwZWNpZmllcn0nO2ApXG4gICAgICAgICAgICAgICAgICAgICAgLmpvaW4oJ1xcbicpO1xuICAgIGNvZGUgPSBpbXBvcnRzICsgJ1xcbicgKyBjb2RlO1xuXG4gICAgcmV0dXJuIGNvZGU7XG4gIH1cblxuICBmaW5hbGl6ZSgpOiBUeXBlQ2hlY2tSZXF1ZXN0IHtcbiAgICAvLyBGaXJzdCwgYnVpbGQgdGhlIG1hcCBvZiB1cGRhdGVzIHRvIHNvdXJjZSBmaWxlcy5cbiAgICBjb25zdCB1cGRhdGVzID0gbmV3IE1hcDxBYnNvbHV0ZUZzUGF0aCwgc3RyaW5nPigpO1xuICAgIGZvciAoY29uc3Qgb3JpZ2luYWxTZiBvZiB0aGlzLm9wTWFwLmtleXMoKSkge1xuICAgICAgY29uc3QgbmV3VGV4dCA9IHRoaXMudHJhbnNmb3JtKG9yaWdpbmFsU2YpO1xuICAgICAgaWYgKG5ld1RleHQgIT09IG51bGwpIHtcbiAgICAgICAgdXBkYXRlcy5zZXQoYWJzb2x1dGVGcm9tU291cmNlRmlsZShvcmlnaW5hbFNmKSwgbmV3VGV4dCk7XG4gICAgICB9XG4gICAgfVxuXG4gICAgY29uc3QgcmVzdWx0czogVHlwZUNoZWNrUmVxdWVzdCA9IHtcbiAgICAgIHVwZGF0ZXM6IHVwZGF0ZXMsXG4gICAgICBwZXJGaWxlRGF0YTogbmV3IE1hcDxBYnNvbHV0ZUZzUGF0aCwgRmlsZVR5cGVDaGVja2luZ0RhdGE+KCksXG4gICAgfTtcblxuICAgIGZvciAoY29uc3QgW3NmUGF0aCwgZmlsZURhdGFdIG9mIHRoaXMuZmlsZU1hcC5lbnRyaWVzKCkpIHtcbiAgICAgIHVwZGF0ZXMuc2V0KGZpbGVEYXRhLnR5cGVDaGVja0ZpbGUuZmlsZU5hbWUsIGZpbGVEYXRhLnR5cGVDaGVja0ZpbGUucmVuZGVyKCkpO1xuICAgICAgcmVzdWx0cy5wZXJGaWxlRGF0YS5zZXQoc2ZQYXRoLCB7XG4gICAgICAgIGdlbmVzaXNEaWFnbm9zdGljczogW1xuICAgICAgICAgIC4uLmZpbGVEYXRhLmRvbVNjaGVtYUNoZWNrZXIuZGlhZ25vc3RpY3MsXG4gICAgICAgICAgLi4uZmlsZURhdGEub29iUmVjb3JkZXIuZGlhZ25vc3RpY3MsXG4gICAgICAgIF0sXG4gICAgICAgIGhhc0lubGluZXM6IGZpbGVEYXRhLmhhc0lubGluZXMsXG4gICAgICAgIHNvdXJjZVJlc29sdmVyOiBmaWxlRGF0YS5zb3VyY2VNYW5hZ2VyLFxuICAgICAgICB0eXBlQ2hlY2tGaWxlOiBmaWxlRGF0YS50eXBlQ2hlY2tGaWxlLmZpbGVOYW1lLFxuICAgICAgfSk7XG4gICAgfVxuXG4gICAgZm9yIChjb25zdCBbc2ZQYXRoLCBmaWxlRGF0YV0gb2YgdGhpcy5hZG9wdGVkRmlsZXMuZW50cmllcygpKSB7XG4gICAgICByZXN1bHRzLnBlckZpbGVEYXRhLnNldChzZlBhdGgsIGZpbGVEYXRhKTtcbiAgICB9XG5cbiAgICByZXR1cm4gcmVzdWx0cztcbiAgfVxuXG4gIHByaXZhdGUgYWRkSW5saW5lVHlwZUNoZWNrQmxvY2soXG4gICAgICBmaWxlRGF0YTogUGVuZGluZ0ZpbGVUeXBlQ2hlY2tpbmdEYXRhLCByZWY6IFJlZmVyZW5jZTxDbGFzc0RlY2xhcmF0aW9uPHRzLkNsYXNzRGVjbGFyYXRpb24+PixcbiAgICAgIHRjYk1ldGE6IFR5cGVDaGVja0Jsb2NrTWV0YWRhdGEpOiB2b2lkIHtcbiAgICBjb25zdCBzZiA9IHJlZi5ub2RlLmdldFNvdXJjZUZpbGUoKTtcbiAgICBpZiAoIXRoaXMub3BNYXAuaGFzKHNmKSkge1xuICAgICAgdGhpcy5vcE1hcC5zZXQoc2YsIFtdKTtcbiAgICB9XG4gICAgY29uc3Qgb3BzID0gdGhpcy5vcE1hcC5nZXQoc2YpITtcbiAgICBvcHMucHVzaChuZXcgVGNiT3AoXG4gICAgICAgIHJlZiwgdGNiTWV0YSwgdGhpcy5jb25maWcsIHRoaXMucmVmbGVjdG9yLCBmaWxlRGF0YS5kb21TY2hlbWFDaGVja2VyLFxuICAgICAgICBmaWxlRGF0YS5vb2JSZWNvcmRlcikpO1xuICAgIGZpbGVEYXRhLmhhc0lubGluZXMgPSB0cnVlO1xuICB9XG5cbiAgcHJpdmF0ZSBkYXRhRm9yRmlsZShzZjogdHMuU291cmNlRmlsZSk6IFBlbmRpbmdGaWxlVHlwZUNoZWNraW5nRGF0YSB7XG4gICAgY29uc3Qgc2ZQYXRoID0gYWJzb2x1dGVGcm9tU291cmNlRmlsZShzZik7XG5cbiAgICBpZiAoIXRoaXMuZmlsZU1hcC5oYXMoc2ZQYXRoKSkge1xuICAgICAgY29uc3Qgc291cmNlTWFuYWdlciA9IG5ldyBUZW1wbGF0ZVNvdXJjZU1hbmFnZXIoKTtcbiAgICAgIGNvbnN0IGRhdGE6IFBlbmRpbmdGaWxlVHlwZUNoZWNraW5nRGF0YSA9IHtcbiAgICAgICAgZG9tU2NoZW1hQ2hlY2tlcjogbmV3IFJlZ2lzdHJ5RG9tU2NoZW1hQ2hlY2tlcihzb3VyY2VNYW5hZ2VyKSxcbiAgICAgICAgb29iUmVjb3JkZXI6IG5ldyBPdXRPZkJhbmREaWFnbm9zdGljUmVjb3JkZXJJbXBsKHNvdXJjZU1hbmFnZXIpLFxuICAgICAgICB0eXBlQ2hlY2tGaWxlOiBuZXcgVHlwZUNoZWNrRmlsZShcbiAgICAgICAgICAgIFR5cGVDaGVja1NoaW1HZW5lcmF0b3Iuc2hpbUZvcihzZlBhdGgpLCB0aGlzLmNvbmZpZywgdGhpcy5yZWZFbWl0dGVyLCB0aGlzLnJlZmxlY3RvcixcbiAgICAgICAgICAgIHRoaXMuY29tcGlsZXJIb3N0KSxcbiAgICAgICAgaGFzSW5saW5lczogZmFsc2UsXG4gICAgICAgIHNvdXJjZU1hbmFnZXIsXG4gICAgICB9O1xuICAgICAgdGhpcy5maWxlTWFwLnNldChzZlBhdGgsIGRhdGEpO1xuICAgIH1cblxuICAgIHJldHVybiB0aGlzLmZpbGVNYXAuZ2V0KHNmUGF0aCkhO1xuICB9XG59XG5cbi8qKlxuICogQSBjb2RlIGdlbmVyYXRpb24gb3BlcmF0aW9uIHRoYXQgbmVlZHMgdG8gaGFwcGVuIHdpdGhpbiBhIGdpdmVuIHNvdXJjZSBmaWxlLlxuICovXG5pbnRlcmZhY2UgT3Age1xuICAvKipcbiAgICogVGhlIG5vZGUgaW4gdGhlIGZpbGUgd2hpY2ggd2lsbCBoYXZlIGNvZGUgZ2VuZXJhdGVkIGZvciBpdC5cbiAgICovXG4gIHJlYWRvbmx5IHJlZjogUmVmZXJlbmNlPENsYXNzRGVjbGFyYXRpb248dHMuQ2xhc3NEZWNsYXJhdGlvbj4+O1xuXG4gIC8qKlxuICAgKiBJbmRleCBpbnRvIHRoZSBzb3VyY2UgdGV4dCB3aGVyZSB0aGUgY29kZSBnZW5lcmF0ZWQgYnkgdGhlIG9wZXJhdGlvbiBzaG91bGQgYmUgaW5zZXJ0ZWQuXG4gICAqL1xuICByZWFkb25seSBzcGxpdFBvaW50OiBudW1iZXI7XG5cbiAgLyoqXG4gICAqIEV4ZWN1dGUgdGhlIG9wZXJhdGlvbiBhbmQgcmV0dXJuIHRoZSBnZW5lcmF0ZWQgY29kZSBhcyB0ZXh0LlxuICAgKi9cbiAgZXhlY3V0ZShpbTogSW1wb3J0TWFuYWdlciwgc2Y6IHRzLlNvdXJjZUZpbGUsIHJlZkVtaXR0ZXI6IFJlZmVyZW5jZUVtaXR0ZXIsIHByaW50ZXI6IHRzLlByaW50ZXIpOlxuICAgICAgc3RyaW5nO1xufVxuXG4vKipcbiAqIEEgdHlwZSBjaGVjayBibG9jayBvcGVyYXRpb24gd2hpY2ggcHJvZHVjZXMgdHlwZSBjaGVjayBjb2RlIGZvciBhIHBhcnRpY3VsYXIgY29tcG9uZW50LlxuICovXG5jbGFzcyBUY2JPcCBpbXBsZW1lbnRzIE9wIHtcbiAgY29uc3RydWN0b3IoXG4gICAgICByZWFkb25seSByZWY6IFJlZmVyZW5jZTxDbGFzc0RlY2xhcmF0aW9uPHRzLkNsYXNzRGVjbGFyYXRpb24+PixcbiAgICAgIHJlYWRvbmx5IG1ldGE6IFR5cGVDaGVja0Jsb2NrTWV0YWRhdGEsIHJlYWRvbmx5IGNvbmZpZzogVHlwZUNoZWNraW5nQ29uZmlnLFxuICAgICAgcmVhZG9ubHkgcmVmbGVjdG9yOiBSZWZsZWN0aW9uSG9zdCwgcmVhZG9ubHkgZG9tU2NoZW1hQ2hlY2tlcjogRG9tU2NoZW1hQ2hlY2tlcixcbiAgICAgIHJlYWRvbmx5IG9vYlJlY29yZGVyOiBPdXRPZkJhbmREaWFnbm9zdGljUmVjb3JkZXIpIHt9XG5cbiAgLyoqXG4gICAqIFR5cGUgY2hlY2sgYmxvY2tzIGFyZSBpbnNlcnRlZCBpbW1lZGlhdGVseSBhZnRlciB0aGUgZW5kIG9mIHRoZSBjb21wb25lbnQgY2xhc3MuXG4gICAqL1xuICBnZXQgc3BsaXRQb2ludCgpOiBudW1iZXIge1xuICAgIHJldHVybiB0aGlzLnJlZi5ub2RlLmVuZCArIDE7XG4gIH1cblxuICBleGVjdXRlKGltOiBJbXBvcnRNYW5hZ2VyLCBzZjogdHMuU291cmNlRmlsZSwgcmVmRW1pdHRlcjogUmVmZXJlbmNlRW1pdHRlciwgcHJpbnRlcjogdHMuUHJpbnRlcik6XG4gICAgICBzdHJpbmcge1xuICAgIGNvbnN0IGVudiA9IG5ldyBFbnZpcm9ubWVudCh0aGlzLmNvbmZpZywgaW0sIHJlZkVtaXR0ZXIsIHRoaXMucmVmbGVjdG9yLCBzZik7XG4gICAgY29uc3QgZm5OYW1lID0gdHMuY3JlYXRlSWRlbnRpZmllcihgX3RjYl8ke3RoaXMucmVmLm5vZGUucG9zfWApO1xuICAgIGNvbnN0IGZuID0gZ2VuZXJhdGVUeXBlQ2hlY2tCbG9jayhcbiAgICAgICAgZW52LCB0aGlzLnJlZiwgZm5OYW1lLCB0aGlzLm1ldGEsIHRoaXMuZG9tU2NoZW1hQ2hlY2tlciwgdGhpcy5vb2JSZWNvcmRlcik7XG4gICAgcmV0dXJuIHByaW50ZXIucHJpbnROb2RlKHRzLkVtaXRIaW50LlVuc3BlY2lmaWVkLCBmbiwgc2YpO1xuICB9XG59XG5cbi8qKlxuICogQSB0eXBlIGNvbnN0cnVjdG9yIG9wZXJhdGlvbiB3aGljaCBwcm9kdWNlcyB0eXBlIGNvbnN0cnVjdG9yIGNvZGUgZm9yIGEgcGFydGljdWxhciBkaXJlY3RpdmUuXG4gKi9cbmNsYXNzIFR5cGVDdG9yT3AgaW1wbGVtZW50cyBPcCB7XG4gIGNvbnN0cnVjdG9yKFxuICAgICAgcmVhZG9ubHkgcmVmOiBSZWZlcmVuY2U8Q2xhc3NEZWNsYXJhdGlvbjx0cy5DbGFzc0RlY2xhcmF0aW9uPj4sXG4gICAgICByZWFkb25seSBtZXRhOiBUeXBlQ3Rvck1ldGFkYXRhKSB7fVxuXG4gIC8qKlxuICAgKiBUeXBlIGNvbnN0cnVjdG9yIG9wZXJhdGlvbnMgYXJlIGluc2VydGVkIGltbWVkaWF0ZWx5IGJlZm9yZSB0aGUgZW5kIG9mIHRoZSBkaXJlY3RpdmUgY2xhc3MuXG4gICAqL1xuICBnZXQgc3BsaXRQb2ludCgpOiBudW1iZXIge1xuICAgIHJldHVybiB0aGlzLnJlZi5ub2RlLmVuZCAtIDE7XG4gIH1cblxuICBleGVjdXRlKGltOiBJbXBvcnRNYW5hZ2VyLCBzZjogdHMuU291cmNlRmlsZSwgcmVmRW1pdHRlcjogUmVmZXJlbmNlRW1pdHRlciwgcHJpbnRlcjogdHMuUHJpbnRlcik6XG4gICAgICBzdHJpbmcge1xuICAgIGNvbnN0IHRjYiA9IGdlbmVyYXRlSW5saW5lVHlwZUN0b3IodGhpcy5yZWYubm9kZSwgdGhpcy5tZXRhKTtcbiAgICByZXR1cm4gcHJpbnRlci5wcmludE5vZGUodHMuRW1pdEhpbnQuVW5zcGVjaWZpZWQsIHRjYiwgc2YpO1xuICB9XG59XG5cbi8qKlxuICogQ29tcGFyZSB0d28gb3BlcmF0aW9ucyBhbmQgcmV0dXJuIHRoZWlyIHNwbGl0IHBvaW50IG9yZGVyaW5nLlxuICovXG5mdW5jdGlvbiBvcmRlck9wcyhvcDE6IE9wLCBvcDI6IE9wKTogbnVtYmVyIHtcbiAgcmV0dXJuIG9wMS5zcGxpdFBvaW50IC0gb3AyLnNwbGl0UG9pbnQ7XG59XG5cbi8qKlxuICogU3BsaXQgYSBzdHJpbmcgaW50byBjaHVua3MgYXQgYW55IG51bWJlciBvZiBzcGxpdCBwb2ludHMuXG4gKi9cbmZ1bmN0aW9uIHNwbGl0U3RyaW5nQXRQb2ludHMoc3RyOiBzdHJpbmcsIHBvaW50czogbnVtYmVyW10pOiBzdHJpbmdbXSB7XG4gIGNvbnN0IHNwbGl0czogc3RyaW5nW10gPSBbXTtcbiAgbGV0IHN0YXJ0ID0gMDtcbiAgZm9yIChsZXQgaSA9IDA7IGkgPCBwb2ludHMubGVuZ3RoOyBpKyspIHtcbiAgICBjb25zdCBwb2ludCA9IHBvaW50c1tpXTtcbiAgICBzcGxpdHMucHVzaChzdHIuc3Vic3RyaW5nKHN0YXJ0LCBwb2ludCkpO1xuICAgIHN0YXJ0ID0gcG9pbnQ7XG4gIH1cbiAgc3BsaXRzLnB1c2goc3RyLnN1YnN0cmluZyhzdGFydCkpO1xuICByZXR1cm4gc3BsaXRzO1xufVxuIl19