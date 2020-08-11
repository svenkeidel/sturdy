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
        define("@angular/compiler-cli/src/ngtsc/annotations/src/diagnostics", ["require", "exports", "tslib", "@angular/compiler-cli/src/ngtsc/diagnostics", "@angular/compiler-cli/src/ngtsc/imports", "@angular/compiler-cli/src/ngtsc/annotations/src/util"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.checkInheritanceOfDirective = exports.getUndecoratedClassWithAngularFeaturesDiagnostic = exports.getDirectiveDiagnostics = exports.getProviderDiagnostics = void 0;
    var tslib_1 = require("tslib");
    var diagnostics_1 = require("@angular/compiler-cli/src/ngtsc/diagnostics");
    var imports_1 = require("@angular/compiler-cli/src/ngtsc/imports");
    var util_1 = require("@angular/compiler-cli/src/ngtsc/annotations/src/util");
    /**
     * Gets the diagnostics for a set of provider classes.
     * @param providerClasses Classes that should be checked.
     * @param providersDeclaration Node that declares the providers array.
     * @param registry Registry that keeps track of the registered injectable classes.
     */
    function getProviderDiagnostics(providerClasses, providersDeclaration, registry) {
        var e_1, _a;
        var diagnostics = [];
        try {
            for (var providerClasses_1 = tslib_1.__values(providerClasses), providerClasses_1_1 = providerClasses_1.next(); !providerClasses_1_1.done; providerClasses_1_1 = providerClasses_1.next()) {
                var provider = providerClasses_1_1.value;
                if (registry.isInjectable(provider.node)) {
                    continue;
                }
                var contextNode = provider.getOriginForDiagnostics(providersDeclaration);
                diagnostics.push(diagnostics_1.makeDiagnostic(diagnostics_1.ErrorCode.UNDECORATED_PROVIDER, contextNode, "The class '" + provider.node.name
                    .text + "' cannot be created via dependency injection, as it does not have an Angular decorator. This will result in an error at runtime.\n\nEither add the @Injectable() decorator to '" + provider.node.name
                    .text + "', or configure a different provider (such as a provider with 'useFactory').\n", [{ node: provider.node, messageText: "'" + provider.node.name.text + "' is declared here." }]));
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (providerClasses_1_1 && !providerClasses_1_1.done && (_a = providerClasses_1.return)) _a.call(providerClasses_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return diagnostics;
    }
    exports.getProviderDiagnostics = getProviderDiagnostics;
    function getDirectiveDiagnostics(node, reader, evaluator, reflector, scopeRegistry, kind) {
        var diagnostics = [];
        var addDiagnostics = function (more) {
            if (more === null) {
                return;
            }
            else if (diagnostics === null) {
                diagnostics = Array.isArray(more) ? more : [more];
            }
            else if (Array.isArray(more)) {
                diagnostics.push.apply(diagnostics, tslib_1.__spread(more));
            }
            else {
                diagnostics.push(more);
            }
        };
        var duplicateDeclarations = scopeRegistry.getDuplicateDeclarations(node);
        if (duplicateDeclarations !== null) {
            addDiagnostics(util_1.makeDuplicateDeclarationError(node, duplicateDeclarations, kind));
        }
        addDiagnostics(checkInheritanceOfDirective(node, reader, reflector, evaluator));
        return diagnostics;
    }
    exports.getDirectiveDiagnostics = getDirectiveDiagnostics;
    function getUndecoratedClassWithAngularFeaturesDiagnostic(node) {
        return diagnostics_1.makeDiagnostic(diagnostics_1.ErrorCode.UNDECORATED_CLASS_USING_ANGULAR_FEATURES, node.name, "Class is using Angular features but is not decorated. Please add an explicit " +
            "Angular decorator.");
    }
    exports.getUndecoratedClassWithAngularFeaturesDiagnostic = getUndecoratedClassWithAngularFeaturesDiagnostic;
    function checkInheritanceOfDirective(node, reader, reflector, evaluator) {
        if (!reflector.isClass(node) || reflector.getConstructorParameters(node) !== null) {
            // We should skip nodes that aren't classes. If a constructor exists, then no base class
            // definition is required on the runtime side - it's legal to inherit from any class.
            return null;
        }
        // The extends clause is an expression which can be as dynamic as the user wants. Try to
        // evaluate it, but fall back on ignoring the clause if it can't be understood. This is a View
        // Engine compatibility hack: View Engine ignores 'extends' expressions that it cannot understand.
        var baseClass = util_1.readBaseClass(node, reflector, evaluator);
        while (baseClass !== null) {
            if (baseClass === 'dynamic') {
                return null;
            }
            // We can skip the base class if it has metadata.
            var baseClassMeta = reader.getDirectiveMetadata(baseClass);
            if (baseClassMeta !== null) {
                return null;
            }
            // If the base class has a blank constructor we can skip it since it can't be using DI.
            var baseClassConstructorParams = reflector.getConstructorParameters(baseClass.node);
            var newParentClass = util_1.readBaseClass(baseClass.node, reflector, evaluator);
            if (baseClassConstructorParams !== null && baseClassConstructorParams.length > 0) {
                // This class has a non-trivial constructor, that's an error!
                return getInheritedUndecoratedCtorDiagnostic(node, baseClass, reader);
            }
            else if (baseClassConstructorParams !== null || newParentClass === null) {
                // This class has a trivial constructor, or no constructor + is the
                // top of the inheritance chain, so it's okay.
                return null;
            }
            // Go up the chain and continue
            baseClass = newParentClass;
        }
        return null;
    }
    exports.checkInheritanceOfDirective = checkInheritanceOfDirective;
    function getInheritedUndecoratedCtorDiagnostic(node, baseClass, reader) {
        var subclassMeta = reader.getDirectiveMetadata(new imports_1.Reference(node));
        var dirOrComp = subclassMeta.isComponent ? 'Component' : 'Directive';
        var baseClassName = baseClass.debugName;
        return diagnostics_1.makeDiagnostic(diagnostics_1.ErrorCode.DIRECTIVE_INHERITS_UNDECORATED_CTOR, node.name, "The " + dirOrComp.toLowerCase() + " " + node.name.text + " inherits its constructor from " + baseClassName + ", " +
            "but the latter does not have an Angular decorator of its own. Dependency injection will not be able to " +
            ("resolve the parameters of " + baseClassName + "'s constructor. Either add a @Directive decorator ") +
            ("to " + baseClassName + ", or add an explicit constructor to " + node.name.text + "."));
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGlhZ25vc3RpY3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9jb21waWxlci1jbGkvc3JjL25ndHNjL2Fubm90YXRpb25zL3NyYy9kaWFnbm9zdGljcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7O0lBSUgsMkVBQTREO0lBQzVELG1FQUF3QztJQU14Qyw2RUFBb0U7SUFFcEU7Ozs7O09BS0c7SUFDSCxTQUFnQixzQkFBc0IsQ0FDbEMsZUFBaUQsRUFBRSxvQkFBbUMsRUFDdEYsUUFBaUM7O1FBQ25DLElBQU0sV0FBVyxHQUFvQixFQUFFLENBQUM7O1lBRXhDLEtBQXVCLElBQUEsb0JBQUEsaUJBQUEsZUFBZSxDQUFBLGdEQUFBLDZFQUFFO2dCQUFuQyxJQUFNLFFBQVEsNEJBQUE7Z0JBQ2pCLElBQUksUUFBUSxDQUFDLFlBQVksQ0FBQyxRQUFRLENBQUMsSUFBSSxDQUFDLEVBQUU7b0JBQ3hDLFNBQVM7aUJBQ1Y7Z0JBRUQsSUFBTSxXQUFXLEdBQUcsUUFBUSxDQUFDLHVCQUF1QixDQUFDLG9CQUFvQixDQUFDLENBQUM7Z0JBQzNFLFdBQVcsQ0FBQyxJQUFJLENBQUMsNEJBQWMsQ0FDM0IsdUJBQVMsQ0FBQyxvQkFBb0IsRUFBRSxXQUFXLEVBQzNDLGdCQUNJLFFBQVEsQ0FBQyxJQUFJLENBQUMsSUFBSTtxQkFDYixJQUFJLHVMQUdULFFBQVEsQ0FBQyxJQUFJLENBQUMsSUFBSTtxQkFDYixJQUFJLG1GQUNwQixFQUNPLENBQUMsRUFBQyxJQUFJLEVBQUUsUUFBUSxDQUFDLElBQUksRUFBRSxXQUFXLEVBQUUsTUFBSSxRQUFRLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxJQUFJLHdCQUFxQixFQUFDLENBQUMsQ0FBQyxDQUFDLENBQUM7YUFDOUY7Ozs7Ozs7OztRQUVELE9BQU8sV0FBVyxDQUFDO0lBQ3JCLENBQUM7SUF6QkQs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MEJBQTBCLEtBQUssSUFBSSxJQUFJLGNBQWMsS0FBSyxJQUFJLEVBQUU7Z0JBQ3pFLG1FQUFtRTtnQkFDbkUsOENBQThDO2dCQUM5QyxPQUFPLElBQUksQ0FBQzthQUNiO1lBRUQsK0JBQStCO1lBQy9CLFNBQVMsR0FBRyxjQUFjLENBQUM7U0FDNUI7UUFFRCxPQUFPLElBQUksQ0FBQztJQUNkLENBQUM7SUEzQ0Qsa0VBMkNDO0lBRUQsU0FBUyxxQ0FBcUMsQ0FDMUMsSUFBc0IsRUFBRSxTQUFvQixFQUFFLE1BQXNCO1FBQ3RFLElBQU0sWUFBWSxHQUFHLE1BQU0sQ0FBQyxvQkFBb0IsQ0FBQyxJQUFJLG1CQUFTLENBQUMsSUFBSSxDQUFDLENBQUUsQ0FBQztRQUN2RSxJQUFNLFNBQVMsR0FBRyxZQUFZLENBQUMsV0FBVyxDQUFDLENBQUMsQ0FBQyxXQUFXLENBQUMsQ0FBQyxDQUFDLFdBQVcsQ0FBQztRQUN2RSxJQUFNLGFBQWEsR0FBRyxTQUFTLENBQUMsU0FBUyxDQUFDO1FBRTFDLE9BQU8sNEJBQWMsQ0FDakIsdUJBQVMsQ0FBQyxtQ0FBbUMsRUFBRSxJQUFJLENBQUMsSUFBSSxFQUN4RCxTQUFPLFNBQVMsQ0FBQyxXQUFXLEVBQUUsU0FBSSxJQUFJLENBQUMsSUFBSSxDQUFDLElBQUksdUNBQzVDLGFBQWEsT0FBSTtZQUNqQix5R0FBeUc7YUFDekcsK0JBQ0ksYUFBYSx1REFBb0QsQ0FBQTthQUNyRSxRQUFNLGFBQWEsNENBQXVDLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxNQUFHLENBQUEsQ0FBQyxDQUFDO0lBQ3ZGLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0ICogYXMgdHMgZnJvbSAndHlwZXNjcmlwdCc7XG5cbmltcG9ydCB7RXJyb3JDb2RlLCBtYWtlRGlhZ25vc3RpY30gZnJvbSAnLi4vLi4vZGlhZ25vc3RpY3MnO1xuaW1wb3J0IHtSZWZlcmVuY2V9IGZyb20gJy4uLy4uL2ltcG9ydHMnO1xuaW1wb3J0IHtJbmplY3RhYmxlQ2xhc3NSZWdpc3RyeSwgTWV0YWRhdGFSZWFkZXJ9IGZyb20gJy4uLy4uL21ldGFkYXRhJztcbmltcG9ydCB7UGFydGlhbEV2YWx1YXRvcn0gZnJvbSAnLi4vLi4vcGFydGlhbF9ldmFsdWF0b3InO1xuaW1wb3J0IHtDbGFzc0RlY2xhcmF0aW9uLCBSZWZsZWN0aW9uSG9zdH0gZnJvbSAnLi4vLi4vcmVmbGVjdGlvbic7XG5pbXBvcnQge0xvY2FsTW9kdWxlU2NvcGVSZWdpc3RyeX0gZnJvbSAnLi4vLi4vc2NvcGUnO1xuXG5pbXBvcnQge21ha2VEdXBsaWNhdGVEZWNsYXJhdGlvbkVycm9yLCByZWFkQmFzZUNsYXNzfSBmcm9tICcuL3V0aWwnO1xuXG4vKipcbiAqIEdldHMgdGhlIGRpYWdub3N0aWNzIGZvciBhIHNldCBvZiBwcm92aWRlciBjbGFzc2VzLlxuICogQHBhcmFtIHByb3ZpZGVyQ2xhc3NlcyBDbGFzc2VzIHRoYXQgc2hvdWxkIGJlIGNoZWNrZWQuXG4gKiBAcGFyYW0gcHJvdmlkZXJzRGVjbGFyYXRpb24gTm9kZSB0aGF0IGRlY2xhcmVzIHRoZSBwcm92aWRlcnMgYXJyYXkuXG4gKiBAcGFyYW0gcmVnaXN0cnkgUmVnaXN0cnkgdGhhdCBrZWVwcyB0cmFjayBvZiB0aGUgcmVnaXN0ZXJlZCBpbmplY3RhYmxlIGNsYXNzZXMuXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRQcm92aWRlckRpYWdub3N0aWNzKFxuICAgIHByb3ZpZGVyQ2xhc3NlczogU2V0PFJlZmVyZW5jZTxDbGFzc0RlY2xhcmF0aW9uPj4sIHByb3ZpZGVyc0RlY2xhcmF0aW9uOiB0cy5FeHByZXNzaW9uLFxuICAgIHJlZ2lzdHJ5OiBJbmplY3RhYmxlQ2xhc3NSZWdpc3RyeSk6IHRzLkRpYWdub3N0aWNbXSB7XG4gIGNvbnN0IGRpYWdub3N0aWNzOiB0cy5EaWFnbm9zdGljW10gPSBbXTtcblxuICBmb3IgKGNvbnN0IHByb3ZpZGVyIG9mIHByb3ZpZGVyQ2xhc3Nlcykge1xuICAgIGlmIChyZWdpc3RyeS5pc0luamVjdGFibGUocHJvdmlkZXIubm9kZSkpIHtcbiAgICAgIGNvbnRpbnVlO1xuICAgIH1cblxuICAgIGNvbnN0IGNvbnRleHROb2RlID0gcHJvdmlkZXIuZ2V0T3JpZ2luRm9yRGlhZ25vc3RpY3MocHJvdmlkZXJzRGVjbGFyYXRpb24pO1xuICAgIGRpYWdub3N0aWNzLnB1c2gobWFrZURpYWdub3N0aWMoXG4gICAgICAgIEVycm9yQ29kZS5VTkRFQ09SQVRFRF9QUk9WSURFUiwgY29udGV4dE5vZGUsXG4gICAgICAgIGBUaGUgY2xhc3MgJyR7XG4gICAgICAgICAgICBwcm92aWRlci5ub2RlLm5hbWVcbiAgICAgICAgICAgICAgICAudGV4dH0nIGNhbm5vdCBiZSBjcmVhdGVkIHZpYSBkZXBlbmRlbmN5IGluamVjdGlvbiwgYXMgaXQgZG9lcyBub3QgaGF2ZSBhbiBBbmd1bGFyIGRlY29yYXRvci4gVGhpcyB3aWxsIHJlc3VsdCBpbiBhbiBlcnJvciBhdCBydW50aW1lLlxuXG5FaXRoZXIgYWRkIHRoZSBASW5qZWN0YWJsZSgpIGRlY29yYXRvciB0byAnJHtcbiAgICAgICAgICAgIHByb3ZpZGVyLm5vZGUubmFtZVxuICAgICAgICAgICAgICAgIC50ZXh0fScsIG9yIGNvbmZpZ3VyZSBhIGRpZmZlcmVudCBwcm92aWRlciAoc3VjaCBhcyBhIHByb3ZpZGVyIHdpdGggJ3VzZUZhY3RvcnknKS5cbmAsXG4gICAgICAgIFt7bm9kZTogcHJvdmlkZXIubm9kZSwgbWVzc2FnZVRleHQ6IGAnJHtwcm92aWRlci5ub2RlLm5hbWUudGV4dH0nIGlzIGRlY2xhcmVkIGhlcmUuYH1dKSk7XG4gIH1cblxuICByZXR1cm4gZGlhZ25vc3RpY3M7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBnZXREaXJlY3RpdmVEaWFnbm9zdGljcyhcbiAgICBub2RlOiBDbGFzc0RlY2xhcmF0aW9uLCByZWFkZXI6IE1ldGFkYXRhUmVhZGVyLCBldmFsdWF0b3I6IFBhcnRpYWxFdmFsdWF0b3IsXG4gICAgcmVmbGVjdG9yOiBSZWZsZWN0aW9uSG9zdCwgc2NvcGVSZWdpc3RyeTogTG9jYWxNb2R1bGVTY29wZVJlZ2lzdHJ5LFxuICAgIGtpbmQ6IHN0cmluZyk6IHRzLkRpYWdub3N0aWNbXXxudWxsIHtcbiAgbGV0IGRpYWdub3N0aWNzOiB0cy5EaWFnbm9zdGljW118bnVsbCA9IFtdO1xuXG4gIGNvbnN0IGFkZERpYWdub3N0aWNzID0gKG1vcmU6IHRzLkRpYWdub3N0aWN8dHMuRGlhZ25vc3RpY1tdfG51bGwpID0+IHtcbiAgICBpZiAobW9yZSA9PT0gbnVsbCkge1xuICAgICAgcmV0dXJuO1xuICAgIH0gZWxzZSBpZiAoZGlhZ25vc3RpY3MgPT09IG51bGwpIHtcbiAgICAgIGRpYWdub3N0aWNzID0gQXJyYXkuaXNBcnJheShtb3JlKSA/IG1vcmUgOiBbbW9yZV07XG4gICAgfSBlbHNlIGlmIChBcnJheS5pc0FycmF5KG1vcmUpKSB7XG4gICAgICBkaWFnbm9zdGljcy5wdXNoKC4uLm1vcmUpO1xuICAgIH0gZWxzZSB7XG4gICAgICBkaWFnbm9zdGljcy5wdXNoKG1vcmUpO1xuICAgIH1cbiAgfTtcblxuICBjb25zdCBkdXBsaWNhdGVEZWNsYXJhdGlvbnMgPSBzY29wZVJlZ2lzdHJ5LmdldER1cGxpY2F0ZURlY2xhcmF0aW9ucyhub2RlKTtcblxuICBpZiAoZHVwbGljYXRlRGVjbGFyYXRpb25zICE9PSBudWxsKSB7XG4gICAgYWRkRGlhZ25vc3RpY3MobWFrZUR1cGxpY2F0ZURlY2xhcmF0aW9uRXJyb3Iobm9kZSwgZHVwbGljYXRlRGVjbGFyYXRpb25zLCBraW5kKSk7XG4gIH1cblxuICBhZGREaWFnbm9zdGljcyhjaGVja0luaGVyaXRhbmNlT2ZEaXJlY3RpdmUobm9kZSwgcmVhZGVyLCByZWZsZWN0b3IsIGV2YWx1YXRvcikpO1xuICByZXR1cm4gZGlhZ25vc3RpY3M7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBnZXRVbmRlY29yYXRlZENsYXNzV2l0aEFuZ3VsYXJGZWF0dXJlc0RpYWdub3N0aWMobm9kZTogQ2xhc3NEZWNsYXJhdGlvbik6XG4gICAgdHMuRGlhZ25vc3RpYyB7XG4gIHJldHVybiBtYWtlRGlhZ25vc3RpYyhcbiAgICAgIEVycm9yQ29kZS5VTkRFQ09SQVRFRF9DTEFTU19VU0lOR19BTkdVTEFSX0ZFQVRVUkVTLCBub2RlLm5hbWUsXG4gICAgICBgQ2xhc3MgaXMgdXNpbmcgQW5ndWxhciBmZWF0dXJlcyBidXQgaXMgbm90IGRlY29yYXRlZC4gUGxlYXNlIGFkZCBhbiBleHBsaWNpdCBgICtcbiAgICAgICAgICBgQW5ndWxhciBkZWNvcmF0b3IuYCk7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBjaGVja0luaGVyaXRhbmNlT2ZEaXJlY3RpdmUoXG4gICAgbm9kZTogQ2xhc3NEZWNsYXJhdGlvbiwgcmVhZGVyOiBNZXRhZGF0YVJlYWRlciwgcmVmbGVjdG9yOiBSZWZsZWN0aW9uSG9zdCxcbiAgICBldmFsdWF0b3I6IFBhcnRpYWxFdmFsdWF0b3IpOiB0cy5EaWFnbm9zdGljfG51bGwge1xuICBpZiAoIXJlZmxlY3Rvci5pc0NsYXNzKG5vZGUpIHx8IHJlZmxlY3Rvci5nZXRDb25zdHJ1Y3RvclBhcmFtZXRlcnMobm9kZSkgIT09IG51bGwpIHtcbiAgICAvLyBXZSBzaG91bGQgc2tpcCBub2RlcyB0aGF0IGFyZW4ndCBjbGFzc2VzLiBJZiBhIGNvbnN0cnVjdG9yIGV4aXN0cywgdGhlbiBubyBiYXNlIGNsYXNzXG4gICAgLy8gZGVmaW5pdGlvbiBpcyByZXF1aXJlZCBvbiB0aGUgcnVudGltZSBzaWRlIC0gaXQncyBsZWdhbCB0byBpbmhlcml0IGZyb20gYW55IGNsYXNzLlxuICAgIHJldHVybiBudWxsO1xuICB9XG5cbiAgLy8gVGhlIGV4dGVuZHMgY2xhdXNlIGlzIGFuIGV4cHJlc3Npb24gd2hpY2ggY2FuIGJlIGFzIGR5bmFtaWMgYXMgdGhlIHVzZXIgd2FudHMuIFRyeSB0b1xuICAvLyBldmFsdWF0ZSBpdCwgYnV0IGZhbGwgYmFjayBvbiBpZ25vcmluZyB0aGUgY2xhdXNlIGlmIGl0IGNhbid0IGJlIHVuZGVyc3Rvb2QuIFRoaXMgaXMgYSBWaWV3XG4gIC8vIEVuZ2luZSBjb21wYXRpYmlsaXR5IGhhY2s6IFZpZXcgRW5naW5lIGlnbm9yZXMgJ2V4dGVuZHMnIGV4cHJlc3Npb25zIHRoYXQgaXQgY2Fubm90IHVuZGVyc3RhbmQuXG4gIGxldCBiYXNlQ2xhc3MgPSByZWFkQmFzZUNsYXNzKG5vZGUsIHJlZmxlY3RvciwgZXZhbHVhdG9yKTtcblxuICB3aGlsZSAoYmFzZUNsYXNzICE9PSBudWxsKSB7XG4gICAgaWYgKGJhc2VDbGFzcyA9PT0gJ2R5bmFtaWMnKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG5cbiAgICAvLyBXZSBjYW4gc2tpcCB0aGUgYmFzZSBjbGFzcyBpZiBpdCBoYXMgbWV0YWRhdGEuXG4gICAgY29uc3QgYmFzZUNsYXNzTWV0YSA9IHJlYWRlci5nZXREaXJlY3RpdmVNZXRhZGF0YShiYXNlQ2xhc3MpO1xuICAgIGlmIChiYXNlQ2xhc3NNZXRhICE9PSBudWxsKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG5cbiAgICAvLyBJZiB0aGUgYmFzZSBjbGFzcyBoYXMgYSBibGFuayBjb25zdHJ1Y3RvciB3ZSBjYW4gc2tpcCBpdCBzaW5jZSBpdCBjYW4ndCBiZSB1c2luZyBESS5cbiAgICBjb25zdCBiYXNlQ2xhc3NDb25zdHJ1Y3RvclBhcmFtcyA9IHJlZmxlY3Rvci5nZXRDb25zdHJ1Y3RvclBhcmFtZXRlcnMoYmFzZUNsYXNzLm5vZGUpO1xuICAgIGNvbnN0IG5ld1BhcmVudENsYXNzID0gcmVhZEJhc2VDbGFzcyhiYXNlQ2xhc3Mubm9kZSwgcmVmbGVjdG9yLCBldmFsdWF0b3IpO1xuXG4gICAgaWYgKGJhc2VDbGFzc0NvbnN0cnVjdG9yUGFyYW1zICE9PSBudWxsICYmIGJhc2VDbGFzc0NvbnN0cnVjdG9yUGFyYW1zLmxlbmd0aCA+IDApIHtcbiAgICAgIC8vIFRoaXMgY2xhc3MgaGFzIGEgbm9uLXRyaXZpYWwgY29uc3RydWN0b3IsIHRoYXQncyBhbiBlcnJvciFcbiAgICAgIHJldHVybiBnZXRJbmhlcml0ZWRVbmRlY29yYXRlZEN0b3JEaWFnbm9zdGljKG5vZGUsIGJhc2VDbGFzcywgcmVhZGVyKTtcbiAgICB9IGVsc2UgaWYgKGJhc2VDbGFzc0NvbnN0cnVjdG9yUGFyYW1zICE9PSBudWxsIHx8IG5ld1BhcmVudENsYXNzID09PSBudWxsKSB7XG4gICAgICAvLyBUaGlzIGNsYXNzIGhhcyBhIHRyaXZpYWwgY29uc3RydWN0b3IsIG9yIG5vIGNvbnN0cnVjdG9yICsgaXMgdGhlXG4gICAgICAvLyB0b3Agb2YgdGhlIGluaGVyaXRhbmNlIGNoYWluLCBzbyBpdCdzIG9rYXkuXG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG5cbiAgICAvLyBHbyB1cCB0aGUgY2hhaW4gYW5kIGNvbnRpbnVlXG4gICAgYmFzZUNsYXNzID0gbmV3UGFyZW50Q2xhc3M7XG4gIH1cblxuICByZXR1cm4gbnVsbDtcbn1cblxuZnVuY3Rpb24gZ2V0SW5oZXJpdGVkVW5kZWNvcmF0ZWRDdG9yRGlhZ25vc3RpYyhcbiAgICBub2RlOiBDbGFzc0RlY2xhcmF0aW9uLCBiYXNlQ2xhc3M6IFJlZmVyZW5jZSwgcmVhZGVyOiBNZXRhZGF0YVJlYWRlcikge1xuICBjb25zdCBzdWJjbGFzc01ldGEgPSByZWFkZXIuZ2V0RGlyZWN0aXZlTWV0YWRhdGEobmV3IFJlZmVyZW5jZShub2RlKSkhO1xuICBjb25zdCBkaXJPckNvbXAgPSBzdWJjbGFzc01ldGEuaXNDb21wb25lbnQgPyAnQ29tcG9uZW50JyA6ICdEaXJlY3RpdmUnO1xuICBjb25zdCBiYXNlQ2xhc3NOYW1lID0gYmFzZUNsYXNzLmRlYnVnTmFtZTtcblxuICByZXR1cm4gbWFrZURpYWdub3N0aWMoXG4gICAgICBFcnJvckNvZGUuRElSRUNUSVZFX0lOSEVSSVRTX1VOREVDT1JBVEVEX0NUT1IsIG5vZGUubmFtZSxcbiAgICAgIGBUaGUgJHtkaXJPckNvbXAudG9Mb3dlckNhc2UoKX0gJHtub2RlLm5hbWUudGV4dH0gaW5oZXJpdHMgaXRzIGNvbnN0cnVjdG9yIGZyb20gJHtcbiAgICAgICAgICBiYXNlQ2xhc3NOYW1lfSwgYCArXG4gICAgICAgICAgYGJ1dCB0aGUgbGF0dGVyIGRvZXMgbm90IGhhdmUgYW4gQW5ndWxhciBkZWNvcmF0b3Igb2YgaXRzIG93bi4gRGVwZW5kZW5jeSBpbmplY3Rpb24gd2lsbCBub3QgYmUgYWJsZSB0byBgICtcbiAgICAgICAgICBgcmVzb2x2ZSB0aGUgcGFyYW1ldGVycyBvZiAke1xuICAgICAgICAgICAgICBiYXNlQ2xhc3NOYW1lfSdzIGNvbnN0cnVjdG9yLiBFaXRoZXIgYWRkIGEgQERpcmVjdGl2ZSBkZWNvcmF0b3IgYCArXG4gICAgICAgICAgYHRvICR7YmFzZUNsYXNzTmFtZX0sIG9yIGFkZCBhbiBleHBsaWNpdCBjb25zdHJ1Y3RvciB0byAke25vZGUubmFtZS50ZXh0fS5gKTtcbn1cbiJdfQ==