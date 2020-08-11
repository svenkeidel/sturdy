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
        define("@angular/compiler-cli/src/ngtsc/reflection/src/type_to_value", ["require", "exports", "tslib", "typescript"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.typeNodeToValueExpr = exports.typeToValue = void 0;
    var tslib_1 = require("tslib");
    var ts = require("typescript");
    /**
     * Potentially convert a `ts.TypeNode` to a `TypeValueReference`, which indicates how to use the
     * type given in the `ts.TypeNode` in a value position.
     *
     * This can return `null` if the `typeNode` is `null`, if it does not refer to a symbol with a value
     * declaration, or if it is not possible to statically understand.
     */
    function typeToValue(typeNode, checker) {
        // It's not possible to get a value expression if the parameter doesn't even have a type.
        if (typeNode === null || !ts.isTypeReferenceNode(typeNode)) {
            return null;
        }
        var symbols = resolveTypeSymbols(typeNode, checker);
        if (symbols === null) {
            return null;
        }
        var local = symbols.local, decl = symbols.decl;
        // It's only valid to convert a type reference to a value reference if the type actually
        // has a value declaration associated with it.
        if (decl.valueDeclaration === undefined) {
            return null;
        }
        // The type points to a valid value declaration. Rewrite the TypeReference into an
        // Expression which references the value pointed to by the TypeReference, if possible.
        // Look at the local `ts.Symbol`'s declarations and see if it comes from an import
        // statement. If so, extract the module specifier and the name of the imported type.
        var firstDecl = local.declarations && local.declarations[0];
        if (firstDecl !== undefined) {
            if (ts.isImportClause(firstDecl) && firstDecl.name !== undefined) {
                // This is a default import.
                //   import Foo from 'foo';
                return {
                    local: true,
                    // Copying the name here ensures the generated references will be correctly transformed
                    // along with the import.
                    expression: ts.updateIdentifier(firstDecl.name),
                    defaultImportStatement: firstDecl.parent,
                };
            }
            else if (ts.isImportSpecifier(firstDecl)) {
                // The symbol was imported by name
                //   import {Foo} from 'foo';
                // or
                //   import {Foo as Bar} from 'foo';
                // Determine the name to import (`Foo`) from the import specifier, as the symbol names of
                // the imported type could refer to a local alias (like `Bar` in the example above).
                var importedName = (firstDecl.propertyName || firstDecl.name).text;
                // The first symbol name refers to the local name, which is replaced by `importedName` above.
                // Any remaining symbol names make up the complete path to the value.
                var _a = tslib_1.__read(symbols.symbolNames), _localName = _a[0], nestedPath = _a.slice(1);
                var moduleName = extractModuleName(firstDecl.parent.parent.parent);
                return {
                    local: false,
                    valueDeclaration: decl.valueDeclaration,
                    moduleName: moduleName,
                    importedName: importedName,
                    nestedPath: nestedPath
                };
            }
            else if (ts.isNamespaceImport(firstDecl)) {
                // The import is a namespace import
                //   import * as Foo from 'foo';
                if (symbols.symbolNames.length === 1) {
                    // The type refers to the namespace itself, which cannot be represented as a value.
                    return null;
                }
                // The first symbol name refers to the local name of the namespace, which is is discarded
                // as a new namespace import will be generated. This is followed by the symbol name that needs
                // to be imported and any remaining names that constitute the complete path to the value.
                var _b = tslib_1.__read(symbols.symbolNames), _ns = _b[0], importedName = _b[1], nestedPath = _b.slice(2);
                var moduleName = extractModuleName(firstDecl.parent.parent);
                return {
                    local: false,
                    valueDeclaration: decl.valueDeclaration,
                    moduleName: moduleName,
                    importedName: importedName,
                    nestedPath: nestedPath
                };
            }
        }
        // If the type is not imported, the type reference can be converted into an expression as is.
        var expression = typeNodeToValueExpr(typeNode);
        if (expression !== null) {
            return {
                local: true,
                expression: expression,
                defaultImportStatement: null,
            };
        }
        else {
            return null;
        }
    }
    exports.typeToValue = typeToValue;
    /**
     * Attempt to extract a `ts.Expression` that's equivalent to a `ts.TypeNode`, as the two have
     * different AST shapes but can reference the same symbols.
     *
     * This will return `null` if an equivalent expression cannot be constructed.
     */
    function typeNodeToValueExpr(node) {
        if (ts.isTypeReferenceNode(node)) {
            return entityNameToValue(node.typeName);
        }
        else {
            return null;
        }
    }
    exports.typeNodeToValueExpr = typeNodeToValueExpr;
    /**
     * Resolve a `TypeReference` node to the `ts.Symbol`s for both its declaration and its local source.
     *
     * In the event that the `TypeReference` refers to a locally declared symbol, these will be the
     * same. If the `TypeReference` refers to an imported symbol, then `decl` will be the fully resolved
     * `ts.Symbol` of the referenced symbol. `local` will be the `ts.Symbol` of the `ts.Identifier`
     * which points to the import statement by which the symbol was imported.
     *
     * All symbol names that make up the type reference are returned left-to-right into the
     * `symbolNames` array, which is guaranteed to include at least one entry.
     */
    function resolveTypeSymbols(typeRef, checker) {
        var typeName = typeRef.typeName;
        // typeRefSymbol is the ts.Symbol of the entire type reference.
        var typeRefSymbol = checker.getSymbolAtLocation(typeName);
        if (typeRefSymbol === undefined) {
            return null;
        }
        // `local` is the `ts.Symbol` for the local `ts.Identifier` for the type.
        // If the type is actually locally declared or is imported by name, for example:
        //   import {Foo} from './foo';
        // then it'll be the same as `typeRefSymbol`.
        //
        // If the type is imported via a namespace import, for example:
        //   import * as foo from './foo';
        // and then referenced as:
        //   constructor(f: foo.Foo)
        // then `local` will be the `ts.Symbol` of `foo`, whereas `typeRefSymbol` will be the `ts.Symbol`
        // of `foo.Foo`. This allows tracking of the import behind whatever type reference exists.
        var local = typeRefSymbol;
        // Destructure a name like `foo.X.Y.Z` as follows:
        // - in `leftMost`, the `ts.Identifier` of the left-most name (`foo`) in the qualified name.
        //   This identifier is used to resolve the `ts.Symbol` for `local`.
        // - in `symbolNames`, all names involved in the qualified path, or a single symbol name if the
        //   type is not qualified.
        var leftMost = typeName;
        var symbolNames = [];
        while (ts.isQualifiedName(leftMost)) {
            symbolNames.unshift(leftMost.right.text);
            leftMost = leftMost.left;
        }
        symbolNames.unshift(leftMost.text);
        if (leftMost !== typeName) {
            var localTmp = checker.getSymbolAtLocation(leftMost);
            if (localTmp !== undefined) {
                local = localTmp;
            }
        }
        // De-alias the top-level type reference symbol to get the symbol of the actual declaration.
        var decl = typeRefSymbol;
        if (typeRefSymbol.flags & ts.SymbolFlags.Alias) {
            decl = checker.getAliasedSymbol(typeRefSymbol);
        }
        return { local: local, decl: decl, symbolNames: symbolNames };
    }
    function entityNameToValue(node) {
        if (ts.isQualifiedName(node)) {
            var left = entityNameToValue(node.left);
            return left !== null ? ts.createPropertyAccess(left, node.right) : null;
        }
        else if (ts.isIdentifier(node)) {
            return ts.getMutableClone(node);
        }
        else {
            return null;
        }
    }
    function extractModuleName(node) {
        if (!ts.isStringLiteral(node.moduleSpecifier)) {
            throw new Error('not a module specifier');
        }
        return node.moduleSpecifier.text;
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidHlwZV90b192YWx1ZS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2NvbXBpbGVyLWNsaS9zcmMvbmd0c2MvcmVmbGVjdGlvbi9zcmMvdHlwZV90b192YWx1ZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7O0lBRUgsK0JBQWlDO0lBSWpDOzs7Ozs7T0FNRztJQUNILFNBQWdCLFdBQVcsQ0FDdkIsUUFBMEIsRUFBRSxPQUF1QjtRQUNyRCx5RkFBeUY7UUFDekYsSUFBSSxRQUFRLEtBQUssSUFBSSxJQUFJLENBQUMsRUFBRSxDQUFDLG1CQUFtQixDQUFDLFFBQVEsQ0FBQyxFQUFFO1lBQzFELE9BQU8sSUFBSSxDQUFDO1NBQ2I7UUFFRCxJQUFNLE9BQU8sR0FBRyxrQkFBa0IsQ0FBQyxRQUFRLEVBQUUsT0FBTyxDQUFDLENBQUM7UUFDdEQsSUFBSSxPQUFPLEtBQUssSUFBSSxFQUFFO1lBQ3BCLE9BQU8sSUFBSSxDQUFDO1NBQ2I7UUFFTSxJQUFBLEtBQUssR0FBVSxPQUFPLE1BQWpCLEVBQUUsSUFBSSxHQUFJLE9BQU8sS0FBWCxDQUFZO1FBQzlCLHdGQUF3RjtRQUN4Riw4Q0FBOEM7UUFDOUMsSUFBSSxJQUFJLENBQUMsZ0JBQWdCLEtBQUssU0FBUyxFQUFFO1lBQ3ZDLE9BQU8sSUFBSSxDQUFDO1NBQ2I7UUFFRCxrRkFBa0Y7UUFDbEYsc0ZBQXNGO1FBRXRGLGtGQUFrRjtRQUNsRixvRkFBb0Y7UUFDcEYsSUFBTSxTQUFTLEdBQUcsS0FBSyxDQUFDLFlBQVksSUFBSSxLQUFLLENBQUMsWUFBWSxDQUFDLENBQUMsQ0FBQyxDQUFDO1FBQzlELElBQUksU0FBUyxLQUFLLFNBQVMsRUFBRTtZQUMzQixJQUFJLEVBQUUsQ0FBQyxjQUFjLENBQUMsU0FBUyxDQUFDLElBQUksU0FBUyxDQUFDLElBQUksS0FBSyxTQUFTLEVBQUU7Z0JBQ2hF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QztTQUNoRDtRQUNELE9BQU8sRUFBQyxLQUFLLE9BQUEsRUFBRSxJQUFJLE1BQUEsRUFBRSxXQUFXLGFBQUEsRUFBQyxDQUFDO0lBQ3BDLENBQUM7SUFFRCxTQUFTLGlCQUFpQixDQUFDLElBQW1CO1FBQzVDLElBQUksRUFBRSxDQUFDLGVBQWUsQ0FBQyxJQUFJLENBQUMsRUFBRTtZQUM1QixJQUFNLElBQUksR0FBRyxpQkFBaUIsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUM7WUFDMUMsT0FBTyxJQUFJLEtBQUssSUFBSSxDQUFDLENBQUMsQ0FBQyxFQUFFLENBQUMsb0JBQW9CLENBQUMsSUFBSSxFQUFFLElBQUksQ0FBQyxLQUFLLENBQUMsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDO1NBQ3pFO2FBQU0sSUFBSSxFQUFFLENBQUMsWUFBWSxDQUFDLElBQUksQ0FBQyxFQUFFO1lBQ2hDLE9BQU8sRUFBRSxDQUFDLGVBQWUsQ0FBQyxJQUFJLENBQUMsQ0FBQztTQUNqQzthQUFNO1lBQ0wsT0FBTyxJQUFJLENBQUM7U0FDYjtJQUNILENBQUM7SUFFRCxTQUFTLGlCQUFpQixDQUFDLElBQTBCO1FBQ25ELElBQUksQ0FBQyxFQUFFLENBQUMsZUFBZSxDQUFDLElBQUksQ0FBQyxlQUFlLENBQUMsRUFBRTtZQUM3QyxNQUFNLElBQUksS0FBSyxDQUFDLHdCQUF3QixDQUFDLENBQUM7U0FDM0M7UUFDRCxPQUFPLElBQUksQ0FBQyxlQUFlLENBQUMsSUFBSSxDQUFDO0lBQ25DLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0ICogYXMgdHMgZnJvbSAndHlwZXNjcmlwdCc7XG5cbmltcG9ydCB7VHlwZVZhbHVlUmVmZXJlbmNlfSBmcm9tICcuL2hvc3QnO1xuXG4vKipcbiAqIFBvdGVudGlhbGx5IGNvbnZlcnQgYSBgdHMuVHlwZU5vZGVgIHRvIGEgYFR5cGVWYWx1ZVJlZmVyZW5jZWAsIHdoaWNoIGluZGljYXRlcyBob3cgdG8gdXNlIHRoZVxuICogdHlwZSBnaXZlbiBpbiB0aGUgYHRzLlR5cGVOb2RlYCBpbiBhIHZhbHVlIHBvc2l0aW9uLlxuICpcbiAqIFRoaXMgY2FuIHJldHVybiBgbnVsbGAgaWYgdGhlIGB0eXBlTm9kZWAgaXMgYG51bGxgLCBpZiBpdCBkb2VzIG5vdCByZWZlciB0byBhIHN5bWJvbCB3aXRoIGEgdmFsdWVcbiAqIGRlY2xhcmF0aW9uLCBvciBpZiBpdCBpcyBub3QgcG9zc2libGUgdG8gc3RhdGljYWxseSB1bmRlcnN0YW5kLlxuICovXG5leHBvcnQgZnVuY3Rpb24gdHlwZVRvVmFsdWUoXG4gICAgdHlwZU5vZGU6IHRzLlR5cGVOb2RlfG51bGwsIGNoZWNrZXI6IHRzLlR5cGVDaGVja2VyKTogVHlwZVZhbHVlUmVmZXJlbmNlfG51bGwge1xuICAvLyBJdCdzIG5vdCBwb3NzaWJsZSB0byBnZXQgYSB2YWx1ZSBleHByZXNzaW9uIGlmIHRoZSBwYXJhbWV0ZXIgZG9lc24ndCBldmVuIGhhdmUgYSB0eXBlLlxuICBpZiAodHlwZU5vZGUgPT09IG51bGwgfHwgIXRzLmlzVHlwZVJlZmVyZW5jZU5vZGUodHlwZU5vZGUpKSB7XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuICBjb25zdCBzeW1ib2xzID0gcmVzb2x2ZVR5cGVTeW1ib2xzKHR5cGVOb2RlLCBjaGVja2VyKTtcbiAgaWYgKHN5bWJvbHMgPT09IG51bGwpIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuXG4gIGNvbnN0IHtsb2NhbCwgZGVjbH0gPSBzeW1ib2xzO1xuICAvLyBJdCdzIG9ubHkgdmFsaWQgdG8gY29udmVydCBhIHR5cGUgcmVmZXJlbmNlIHRvIGEgdmFsdWUgcmVmZXJlbmNlIGlmIHRoZSB0eXBlIGFjdHVhbGx5XG4gIC8vIGhhcyBhIHZhbHVlIGRlY2xhcmF0aW9uIGFzc29jaWF0ZWQgd2l0aCBpdC5cbiAgaWYgKGRlY2wudmFsdWVEZWNsYXJhdGlvbiA9PT0gdW5kZWZpbmVkKSB7XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuICAvLyBUaGUgdHlwZSBwb2ludHMgdG8gYSB2YWxpZCB2YWx1ZSBkZWNsYXJhdGlvbi4gUmV3cml0ZSB0aGUgVHlwZVJlZmVyZW5jZSBpbnRvIGFuXG4gIC8vIEV4cHJlc3Npb24gd2hpY2ggcmVmZXJlbmNlcyB0aGUgdmFsdWUgcG9pbnRlZCB0byBieSB0aGUgVHlwZVJlZmVyZW5jZSwgaWYgcG9zc2libGUuXG5cbiAgLy8gTG9vayBhdCB0aGUgbG9jYWwgYHRzLlN5bWJvbGAncyBkZWNsYXJhdGlvbnMgYW5kIHNlZSBpZiBpdCBjb21lcyBmcm9tIGFuIGltcG9ydFxuICAvLyBzdGF0ZW1lbnQuIElmIHNvLCBleHRyYWN0IHRoZSBtb2R1bGUgc3BlY2lmaWVyIGFuZCB0aGUgbmFtZSBvZiB0aGUgaW1wb3J0ZWQgdHlwZS5cbiAgY29uc3QgZmlyc3REZWNsID0gbG9jYWwuZGVjbGFyYXRpb25zICYmIGxvY2FsLmRlY2xhcmF0aW9uc1swXTtcbiAgaWYgKGZpcnN0RGVjbCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgaWYgKHRzLmlzSW1wb3J0Q2xhdXNlKGZpcnN0RGVjbCkgJiYgZmlyc3REZWNsLm5hbWUgIT09IHVuZGVmaW5lZCkge1xuICAgICAgLy8gVGhpcyBpcyBhIGRlZmF1bHQgaW1wb3J0LlxuICAgICAgLy8gICBpbXBvcnQgRm9vIGZyb20gJ2Zvbyc7XG5cbiAgICAgIHJldHVybiB7XG4gICAgICAgIGxvY2FsOiB0cnVlLFxuICAgICAgICAvLyBDb3B5aW5nIHRoZSBuYW1lIGhlcmUgZW5zdXJlcyB0aGUgZ2VuZXJhdGVkIHJlZmVyZW5jZXMgd2lsbCBiZSBjb3JyZWN0bHkgdHJhbnNmb3JtZWRcbiAgICAgICAgLy8gYWxvbmcgd2l0aCB0aGUgaW1wb3J0LlxuICAgICAgICBleHByZXNzaW9uOiB0cy51cGRhdGVJZGVudGlmaWVyKGZpcnN0RGVjbC5uYW1lKSxcbiAgICAgICAgZGVmYXVsdEltcG9ydFN0YXRlbWVudDogZmlyc3REZWNsLnBhcmVudCxcbiAgICAgIH07XG4gICAgfSBlbHNlIGlmICh0cy5pc0ltcG9ydFNwZWNpZmllcihmaXJzdERlY2wpKSB7XG4gICAgICAvLyBUaGUgc3ltYm9sIHdhcyBpbXBvcnRlZCBieSBuYW1lXG4gICAgICAvLyAgIGltcG9ydCB7Rm9vfSBmcm9tICdmb28nO1xuICAgICAgLy8gb3JcbiAgICAgIC8vICAgaW1wb3J0IHtGb28gYXMgQmFyfSBmcm9tICdmb28nO1xuXG4gICAgICAvLyBEZXRlcm1pbmUgdGhlIG5hbWUgdG8gaW1wb3J0IChgRm9vYCkgZnJvbSB0aGUgaW1wb3J0IHNwZWNpZmllciwgYXMgdGhlIHN5bWJvbCBuYW1lcyBvZlxuICAgICAgLy8gdGhlIGltcG9ydGVkIHR5cGUgY291bGQgcmVmZXIgdG8gYSBsb2NhbCBhbGlhcyAobGlrZSBgQmFyYCBpbiB0aGUgZXhhbXBsZSBhYm92ZSkuXG4gICAgICBjb25zdCBpbXBvcnRlZE5hbWUgPSAoZmlyc3REZWNsLnByb3BlcnR5TmFtZSB8fCBmaXJzdERlY2wubmFtZSkudGV4dDtcblxuICAgICAgLy8gVGhlIGZpcnN0IHN5bWJvbCBuYW1lIHJlZmVycyB0byB0aGUgbG9jYWwgbmFtZSwgd2hpY2ggaXMgcmVwbGFjZWQgYnkgYGltcG9ydGVkTmFtZWAgYWJvdmUuXG4gICAgICAvLyBBbnkgcmVtYWluaW5nIHN5bWJvbCBuYW1lcyBtYWtlIHVwIHRoZSBjb21wbGV0ZSBwYXRoIHRvIHRoZSB2YWx1ZS5cbiAgICAgIGNvbnN0IFtfbG9jYWxOYW1lLCAuLi5uZXN0ZWRQYXRoXSA9IHN5bWJvbHMuc3ltYm9sTmFtZXM7XG5cbiAgICAgIGNvbnN0IG1vZHVsZU5hbWUgPSBleHRyYWN0TW9kdWxlTmFtZShmaXJzdERlY2wucGFyZW50LnBhcmVudC5wYXJlbnQpO1xuICAgICAgcmV0dXJuIHtcbiAgICAgICAgbG9jYWw6IGZhbHNlLFxuICAgICAgICB2YWx1ZURlY2xhcmF0aW9uOiBkZWNsLnZhbHVlRGVjbGFyYXRpb24sXG4gICAgICAgIG1vZHVsZU5hbWUsXG4gICAgICAgIGltcG9ydGVkTmFtZSxcbiAgICAgICAgbmVzdGVkUGF0aFxuICAgICAgfTtcbiAgICB9IGVsc2UgaWYgKHRzLmlzTmFtZXNwYWNlSW1wb3J0KGZpcnN0RGVjbCkpIHtcbiAgICAgIC8vIFRoZSBpbXBvcnQgaXMgYSBuYW1lc3BhY2UgaW1wb3J0XG4gICAgICAvLyAgIGltcG9ydCAqIGFzIEZvbyBmcm9tICdmb28nO1xuXG4gICAgICBpZiAoc3ltYm9scy5zeW1ib2xOYW1lcy5sZW5ndGggPT09IDEpIHtcbiAgICAgICAgLy8gVGhlIHR5cGUgcmVmZXJzIHRvIHRoZSBuYW1lc3BhY2UgaXRzZWxmLCB3aGljaCBjYW5ub3QgYmUgcmVwcmVzZW50ZWQgYXMgYSB2YWx1ZS5cbiAgICAgICAgcmV0dXJuIG51bGw7XG4gICAgICB9XG5cbiAgICAgIC8vIFRoZSBmaXJzdCBzeW1ib2wgbmFtZSByZWZlcnMgdG8gdGhlIGxvY2FsIG5hbWUgb2YgdGhlIG5hbWVzcGFjZSwgd2hpY2ggaXMgaXMgZGlzY2FyZGVkXG4gICAgICAvLyBhcyBhIG5ldyBuYW1lc3BhY2UgaW1wb3J0IHdpbGwgYmUgZ2VuZXJhdGVkLiBUaGlzIGlzIGZvbGxvd2VkIGJ5IHRoZSBzeW1ib2wgbmFtZSB0aGF0IG5lZWRzXG4gICAgICAvLyB0byBiZSBpbXBvcnRlZCBhbmQgYW55IHJlbWFpbmluZyBuYW1lcyB0aGF0IGNvbnN0aXR1dGUgdGhlIGNvbXBsZXRlIHBhdGggdG8gdGhlIHZhbHVlLlxuICAgICAgY29uc3QgW19ucywgaW1wb3J0ZWROYW1lLCAuLi5uZXN0ZWRQYXRoXSA9IHN5bWJvbHMuc3ltYm9sTmFtZXM7XG5cbiAgICAgIGNvbnN0IG1vZHVsZU5hbWUgPSBleHRyYWN0TW9kdWxlTmFtZShmaXJzdERlY2wucGFyZW50LnBhcmVudCk7XG4gICAgICByZXR1cm4ge1xuICAgICAgICBsb2NhbDogZmFsc2UsXG4gICAgICAgIHZhbHVlRGVjbGFyYXRpb246IGRlY2wudmFsdWVEZWNsYXJhdGlvbixcbiAgICAgICAgbW9kdWxlTmFtZSxcbiAgICAgICAgaW1wb3J0ZWROYW1lLFxuICAgICAgICBuZXN0ZWRQYXRoXG4gICAgICB9O1xuICAgIH1cbiAgfVxuXG4gIC8vIElmIHRoZSB0eXBlIGlzIG5vdCBpbXBvcnRlZCwgdGhlIHR5cGUgcmVmZXJlbmNlIGNhbiBiZSBjb252ZXJ0ZWQgaW50byBhbiBleHByZXNzaW9uIGFzIGlzLlxuICBjb25zdCBleHByZXNzaW9uID0gdHlwZU5vZGVUb1ZhbHVlRXhwcih0eXBlTm9kZSk7XG4gIGlmIChleHByZXNzaW9uICE9PSBudWxsKSB7XG4gICAgcmV0dXJuIHtcbiAgICAgIGxvY2FsOiB0cnVlLFxuICAgICAgZXhwcmVzc2lvbixcbiAgICAgIGRlZmF1bHRJbXBvcnRTdGF0ZW1lbnQ6IG51bGwsXG4gICAgfTtcbiAgfSBlbHNlIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxufVxuXG4vKipcbiAqIEF0dGVtcHQgdG8gZXh0cmFjdCBhIGB0cy5FeHByZXNzaW9uYCB0aGF0J3MgZXF1aXZhbGVudCB0byBhIGB0cy5UeXBlTm9kZWAsIGFzIHRoZSB0d28gaGF2ZVxuICogZGlmZmVyZW50IEFTVCBzaGFwZXMgYnV0IGNhbiByZWZlcmVuY2UgdGhlIHNhbWUgc3ltYm9scy5cbiAqXG4gKiBUaGlzIHdpbGwgcmV0dXJuIGBudWxsYCBpZiBhbiBlcXVpdmFsZW50IGV4cHJlc3Npb24gY2Fubm90IGJlIGNvbnN0cnVjdGVkLlxuICovXG5leHBvcnQgZnVuY3Rpb24gdHlwZU5vZGVUb1ZhbHVlRXhwcihub2RlOiB0cy5UeXBlTm9kZSk6IHRzLkV4cHJlc3Npb258bnVsbCB7XG4gIGlmICh0cy5pc1R5cGVSZWZlcmVuY2VOb2RlKG5vZGUpKSB7XG4gICAgcmV0dXJuIGVudGl0eU5hbWVUb1ZhbHVlKG5vZGUudHlwZU5hbWUpO1xuICB9IGVsc2Uge1xuICAgIHJldHVybiBudWxsO1xuICB9XG59XG5cbi8qKlxuICogUmVzb2x2ZSBhIGBUeXBlUmVmZXJlbmNlYCBub2RlIHRvIHRoZSBgdHMuU3ltYm9sYHMgZm9yIGJvdGggaXRzIGRlY2xhcmF0aW9uIGFuZCBpdHMgbG9jYWwgc291cmNlLlxuICpcbiAqIEluIHRoZSBldmVudCB0aGF0IHRoZSBgVHlwZVJlZmVyZW5jZWAgcmVmZXJzIHRvIGEgbG9jYWxseSBkZWNsYXJlZCBzeW1ib2wsIHRoZXNlIHdpbGwgYmUgdGhlXG4gKiBzYW1lLiBJZiB0aGUgYFR5cGVSZWZlcmVuY2VgIHJlZmVycyB0byBhbiBpbXBvcnRlZCBzeW1ib2wsIHRoZW4gYGRlY2xgIHdpbGwgYmUgdGhlIGZ1bGx5IHJlc29sdmVkXG4gKiBgdHMuU3ltYm9sYCBvZiB0aGUgcmVmZXJlbmNlZCBzeW1ib2wuIGBsb2NhbGAgd2lsbCBiZSB0aGUgYHRzLlN5bWJvbGAgb2YgdGhlIGB0cy5JZGVudGlmaWVyYFxuICogd2hpY2ggcG9pbnRzIHRvIHRoZSBpbXBvcnQgc3RhdGVtZW50IGJ5IHdoaWNoIHRoZSBzeW1ib2wgd2FzIGltcG9ydGVkLlxuICpcbiAqIEFsbCBzeW1ib2wgbmFtZXMgdGhhdCBtYWtlIHVwIHRoZSB0eXBlIHJlZmVyZW5jZSBhcmUgcmV0dXJuZWQgbGVmdC10by1yaWdodCBpbnRvIHRoZVxuICogYHN5bWJvbE5hbWVzYCBhcnJheSwgd2hpY2ggaXMgZ3VhcmFudGVlZCB0byBpbmNsdWRlIGF0IGxlYXN0IG9uZSBlbnRyeS5cbiAqL1xuZnVuY3Rpb24gcmVzb2x2ZVR5cGVTeW1ib2xzKHR5cGVSZWY6IHRzLlR5cGVSZWZlcmVuY2VOb2RlLCBjaGVja2VyOiB0cy5UeXBlQ2hlY2tlcik6XG4gICAge2xvY2FsOiB0cy5TeW1ib2wsIGRlY2w6IHRzLlN5bWJvbCwgc3ltYm9sTmFtZXM6IHN0cmluZ1tdfXxudWxsIHtcbiAgY29uc3QgdHlwZU5hbWUgPSB0eXBlUmVmLnR5cGVOYW1lO1xuICAvLyB0eXBlUmVmU3ltYm9sIGlzIHRoZSB0cy5TeW1ib2wgb2YgdGhlIGVudGlyZSB0eXBlIHJlZmVyZW5jZS5cbiAgY29uc3QgdHlwZVJlZlN5bWJvbDogdHMuU3ltYm9sfHVuZGVmaW5lZCA9IGNoZWNrZXIuZ2V0U3ltYm9sQXRMb2NhdGlvbih0eXBlTmFtZSk7XG4gIGlmICh0eXBlUmVmU3ltYm9sID09PSB1bmRlZmluZWQpIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuXG4gIC8vIGBsb2NhbGAgaXMgdGhlIGB0cy5TeW1ib2xgIGZvciB0aGUgbG9jYWwgYHRzLklkZW50aWZpZXJgIGZvciB0aGUgdHlwZS5cbiAgLy8gSWYgdGhlIHR5cGUgaXMgYWN0dWFsbHkgbG9jYWxseSBkZWNsYXJlZCBvciBpcyBpbXBvcnRlZCBieSBuYW1lLCBmb3IgZXhhbXBsZTpcbiAgLy8gICBpbXBvcnQge0Zvb30gZnJvbSAnLi9mb28nO1xuICAvLyB0aGVuIGl0J2xsIGJlIHRoZSBzYW1lIGFzIGB0eXBlUmVmU3ltYm9sYC5cbiAgLy9cbiAgLy8gSWYgdGhlIHR5cGUgaXMgaW1wb3J0ZWQgdmlhIGEgbmFtZXNwYWNlIGltcG9ydCwgZm9yIGV4YW1wbGU6XG4gIC8vICAgaW1wb3J0ICogYXMgZm9vIGZyb20gJy4vZm9vJztcbiAgLy8gYW5kIHRoZW4gcmVmZXJlbmNlZCBhczpcbiAgLy8gICBjb25zdHJ1Y3RvcihmOiBmb28uRm9vKVxuICAvLyB0aGVuIGBsb2NhbGAgd2lsbCBiZSB0aGUgYHRzLlN5bWJvbGAgb2YgYGZvb2AsIHdoZXJlYXMgYHR5cGVSZWZTeW1ib2xgIHdpbGwgYmUgdGhlIGB0cy5TeW1ib2xgXG4gIC8vIG9mIGBmb28uRm9vYC4gVGhpcyBhbGxvd3MgdHJhY2tpbmcgb2YgdGhlIGltcG9ydCBiZWhpbmQgd2hhdGV2ZXIgdHlwZSByZWZlcmVuY2UgZXhpc3RzLlxuICBsZXQgbG9jYWwgPSB0eXBlUmVmU3ltYm9sO1xuXG4gIC8vIERlc3RydWN0dXJlIGEgbmFtZSBsaWtlIGBmb28uWC5ZLlpgIGFzIGZvbGxvd3M6XG4gIC8vIC0gaW4gYGxlZnRNb3N0YCwgdGhlIGB0cy5JZGVudGlmaWVyYCBvZiB0aGUgbGVmdC1tb3N0IG5hbWUgKGBmb29gKSBpbiB0aGUgcXVhbGlmaWVkIG5hbWUuXG4gIC8vICAgVGhpcyBpZGVudGlmaWVyIGlzIHVzZWQgdG8gcmVzb2x2ZSB0aGUgYHRzLlN5bWJvbGAgZm9yIGBsb2NhbGAuXG4gIC8vIC0gaW4gYHN5bWJvbE5hbWVzYCwgYWxsIG5hbWVzIGludm9sdmVkIGluIHRoZSBxdWFsaWZpZWQgcGF0aCwgb3IgYSBzaW5nbGUgc3ltYm9sIG5hbWUgaWYgdGhlXG4gIC8vICAgdHlwZSBpcyBub3QgcXVhbGlmaWVkLlxuICBsZXQgbGVmdE1vc3QgPSB0eXBlTmFtZTtcbiAgY29uc3Qgc3ltYm9sTmFtZXM6IHN0cmluZ1tdID0gW107XG4gIHdoaWxlICh0cy5pc1F1YWxpZmllZE5hbWUobGVmdE1vc3QpKSB7XG4gICAgc3ltYm9sTmFtZXMudW5zaGlmdChsZWZ0TW9zdC5yaWdodC50ZXh0KTtcbiAgICBsZWZ0TW9zdCA9IGxlZnRNb3N0LmxlZnQ7XG4gIH1cbiAgc3ltYm9sTmFtZXMudW5zaGlmdChsZWZ0TW9zdC50ZXh0KTtcblxuICBpZiAobGVmdE1vc3QgIT09IHR5cGVOYW1lKSB7XG4gICAgY29uc3QgbG9jYWxUbXAgPSBjaGVja2VyLmdldFN5bWJvbEF0TG9jYXRpb24obGVmdE1vc3QpO1xuICAgIGlmIChsb2NhbFRtcCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICBsb2NhbCA9IGxvY2FsVG1wO1xuICAgIH1cbiAgfVxuXG4gIC8vIERlLWFsaWFzIHRoZSB0b3AtbGV2ZWwgdHlwZSByZWZlcmVuY2Ugc3ltYm9sIHRvIGdldCB0aGUgc3ltYm9sIG9mIHRoZSBhY3R1YWwgZGVjbGFyYXRpb24uXG4gIGxldCBkZWNsID0gdHlwZVJlZlN5bWJvbDtcbiAgaWYgKHR5cGVSZWZTeW1ib2wuZmxhZ3MgJiB0cy5TeW1ib2xGbGFncy5BbGlhcykge1xuICAgIGRlY2wgPSBjaGVja2VyLmdldEFsaWFzZWRTeW1ib2wodHlwZVJlZlN5bWJvbCk7XG4gIH1cbiAgcmV0dXJuIHtsb2NhbCwgZGVjbCwgc3ltYm9sTmFtZXN9O1xufVxuXG5mdW5jdGlvbiBlbnRpdHlOYW1lVG9WYWx1ZShub2RlOiB0cy5FbnRpdHlOYW1lKTogdHMuRXhwcmVzc2lvbnxudWxsIHtcbiAgaWYgKHRzLmlzUXVhbGlmaWVkTmFtZShub2RlKSkge1xuICAgIGNvbnN0IGxlZnQgPSBlbnRpdHlOYW1lVG9WYWx1ZShub2RlLmxlZnQpO1xuICAgIHJldHVybiBsZWZ0ICE9PSBudWxsID8gdHMuY3JlYXRlUHJvcGVydHlBY2Nlc3MobGVmdCwgbm9kZS5yaWdodCkgOiBudWxsO1xuICB9IGVsc2UgaWYgKHRzLmlzSWRlbnRpZmllcihub2RlKSkge1xuICAgIHJldHVybiB0cy5nZXRNdXRhYmxlQ2xvbmUobm9kZSk7XG4gIH0gZWxzZSB7XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cbn1cblxuZnVuY3Rpb24gZXh0cmFjdE1vZHVsZU5hbWUobm9kZTogdHMuSW1wb3J0RGVjbGFyYXRpb24pOiBzdHJpbmcge1xuICBpZiAoIXRzLmlzU3RyaW5nTGl0ZXJhbChub2RlLm1vZHVsZVNwZWNpZmllcikpIHtcbiAgICB0aHJvdyBuZXcgRXJyb3IoJ25vdCBhIG1vZHVsZSBzcGVjaWZpZXInKTtcbiAgfVxuICByZXR1cm4gbm9kZS5tb2R1bGVTcGVjaWZpZXIudGV4dDtcbn1cbiJdfQ==