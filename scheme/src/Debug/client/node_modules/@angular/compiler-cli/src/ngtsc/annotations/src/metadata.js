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
        define("@angular/compiler-cli/src/ngtsc/annotations/src/metadata", ["require", "exports", "@angular/compiler", "typescript", "@angular/compiler-cli/src/ngtsc/annotations/src/util"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.generateSetClassMetadataCall = void 0;
    var compiler_1 = require("@angular/compiler");
    var ts = require("typescript");
    var util_1 = require("@angular/compiler-cli/src/ngtsc/annotations/src/util");
    /**
     * Given a class declaration, generate a call to `setClassMetadata` with the Angular metadata
     * present on the class or its member fields.
     *
     * If no such metadata is present, this function returns `null`. Otherwise, the call is returned
     * as a `Statement` for inclusion along with the class.
     */
    function generateSetClassMetadataCall(clazz, reflection, defaultImportRecorder, isCore, annotateForClosureCompiler) {
        if (!reflection.isClass(clazz)) {
            return null;
        }
        var id = ts.updateIdentifier(reflection.getAdjacentNameOfClass(clazz));
        // Reflect over the class decorators. If none are present, or those that are aren't from
        // Angular, then return null. Otherwise, turn them into metadata.
        var classDecorators = reflection.getDecoratorsOfDeclaration(clazz);
        if (classDecorators === null) {
            return null;
        }
        var ngClassDecorators = classDecorators.filter(function (dec) { return isAngularDecorator(dec, isCore); })
            .map(function (decorator) { return decoratorToMetadata(decorator, annotateForClosureCompiler); });
        if (ngClassDecorators.length === 0) {
            return null;
        }
        var metaDecorators = ts.createArrayLiteral(ngClassDecorators);
        // Convert the constructor parameters to metadata, passing null if none are present.
        var metaCtorParameters = new compiler_1.LiteralExpr(null);
        var classCtorParameters = reflection.getConstructorParameters(clazz);
        if (classCtorParameters !== null) {
            var ctorParameters = classCtorParameters.map(function (param) { return ctorParameterToMetadata(param, defaultImportRecorder, isCore); });
            metaCtorParameters = new compiler_1.FunctionExpr([], [
                new compiler_1.ReturnStatement(new compiler_1.LiteralArrayExpr(ctorParameters)),
            ]);
        }
        // Do the same for property decorators.
        var metaPropDecorators = ts.createNull();
        var classMembers = reflection.getMembersOfClass(clazz).filter(function (member) { return !member.isStatic && member.decorators !== null && member.decorators.length > 0; });
        var duplicateDecoratedMemberNames = classMembers.map(function (member) { return member.name; }).filter(function (name, i, arr) { return arr.indexOf(name) < i; });
        if (duplicateDecoratedMemberNames.length > 0) {
            // This should theoretically never happen, because the only way to have duplicate instance
            // member names is getter/setter pairs and decorators cannot appear in both a getter and the
            // corresponding setter.
            throw new Error("Duplicate decorated properties found on class '" + clazz.name.text + "': " +
                duplicateDecoratedMemberNames.join(', '));
        }
        var decoratedMembers = classMembers.map(function (member) { return classMemberToMetadata(member.name, member.decorators, isCore); });
        if (decoratedMembers.length > 0) {
            metaPropDecorators = ts.createObjectLiteral(decoratedMembers);
        }
        // Generate a pure call to setClassMetadata with the class identifier and its metadata.
        var setClassMetadata = new compiler_1.ExternalExpr(compiler_1.Identifiers.setClassMetadata);
        var fnCall = new compiler_1.InvokeFunctionExpr(
        /* fn */ setClassMetadata, 
        /* args */
        [
            new compiler_1.WrappedNodeExpr(id),
            new compiler_1.WrappedNodeExpr(metaDecorators),
            metaCtorParameters,
            new compiler_1.WrappedNodeExpr(metaPropDecorators),
        ]);
        var iifeFn = new compiler_1.FunctionExpr([], [fnCall.toStmt()], compiler_1.NONE_TYPE);
        var iife = new compiler_1.InvokeFunctionExpr(
        /* fn */ iifeFn, 
        /* args */ [], 
        /* type */ undefined, 
        /* sourceSpan */ undefined, 
        /* pure */ true);
        return iife.toStmt();
    }
    exports.generateSetClassMetadataCall = generateSetClassMetadataCall;
    /**
     * Convert a reflected constructor parameter to metadata.
     */
    function ctorParameterToMetadata(param, defaultImportRecorder, isCore) {
        // Parameters sometimes have a type that can be referenced. If so, then use it, otherwise
        // its type is undefined.
        var type = param.typeValueReference !== null ?
            util_1.valueReferenceToExpression(param.typeValueReference, defaultImportRecorder) :
            new compiler_1.LiteralExpr(undefined);
        var mapEntries = [
            { key: 'type', value: type, quoted: false },
        ];
        // If the parameter has decorators, include the ones from Angular.
        if (param.decorators !== null) {
            var ngDecorators = param.decorators.filter(function (dec) { return isAngularDecorator(dec, isCore); })
                .map(function (decorator) { return decoratorToMetadata(decorator); });
            var value = new compiler_1.WrappedNodeExpr(ts.createArrayLiteral(ngDecorators));
            mapEntries.push({ key: 'decorators', value: value, quoted: false });
        }
        return compiler_1.literalMap(mapEntries);
    }
    /**
     * Convert a reflected class member to metadata.
     */
    function classMemberToMetadata(name, decorators, isCore) {
        var ngDecorators = decorators.filter(function (dec) { return isAngularDecorator(dec, isCore); })
            .map(function (decorator) { return decoratorToMetadata(decorator); });
        var decoratorMeta = ts.createArrayLiteral(ngDecorators);
        return ts.createPropertyAssignment(name, decoratorMeta);
    }
    /**
     * Convert a reflected decorator to metadata.
     */
    function decoratorToMetadata(decorator, wrapFunctionsInParens) {
        if (decorator.identifier === null) {
            throw new Error('Illegal state: synthesized decorator cannot be emitted in class metadata.');
        }
        // Decorators have a type.
        var properties = [
            ts.createPropertyAssignment('type', ts.getMutableClone(decorator.identifier)),
        ];
        // Sometimes they have arguments.
        if (decorator.args !== null && decorator.args.length > 0) {
            var args = decorator.args.map(function (arg) {
                var expr = ts.getMutableClone(arg);
                return wrapFunctionsInParens ? util_1.wrapFunctionExpressionsInParens(expr) : expr;
            });
            properties.push(ts.createPropertyAssignment('args', ts.createArrayLiteral(args)));
        }
        return ts.createObjectLiteral(properties, true);
    }
    /**
     * Whether a given decorator should be treated as an Angular decorator.
     *
     * Either it's used in @angular/core, or it's imported from there.
     */
    function isAngularDecorator(decorator, isCore) {
        return isCore || (decorator.import !== null && decorator.import.from === '@angular/core');
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibWV0YWRhdGEuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9jb21waWxlci1jbGkvc3JjL25ndHNjL2Fubm90YXRpb25zL3NyYy9tZXRhZGF0YS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7SUFFSCw4Q0FBNk07SUFDN00sK0JBQWlDO0lBS2pDLDZFQUFtRjtJQUduRjs7Ozs7O09BTUc7SUFDSCxTQUFnQiw0QkFBNEIsQ0FDeEMsS0FBcUIsRUFBRSxVQUEwQixFQUFFLHFCQUE0QyxFQUMvRixNQUFlLEVBQUUsMEJBQW9DO1FBQ3ZELElBQUksQ0FBQyxVQUFVLENBQUMsT0FBTyxDQUFDLEtBQUssQ0FBQyxFQUFFO1lBQzlCLE9BQU8sSUFBSSxDQUFDO1NBQ2I7UUFDRCxJQUFNLEVBQUUsR0FBRyxFQUFFLENBQUMsZ0JBQWdCLENBQUMsVUFBVSxDQUFDLHNCQUFzQixDQUFDLEtBQUssQ0FBQyxDQUFDLENBQUM7UUFFekUsd0ZBQXdGO1FBQ3hGLGlFQUFpRTtRQUNqRSxJQUFNLGVBQWUsR0FBRyxVQUFVLENBQUMsMEJBQTBCLENBQUMsS0FBSyxDQUFDLENBQUM7UUFDckUsSUFBSSxlQUFlLEtBQUssSUFBSSxFQUFFO1lBQzVCLE9BQU8sSUFBSSxDQUFDO1NBQ2I7UUFDRCxJQUFNLGlCQUFpQixHQUNuQixlQUFlLENBQUMsTUFBTSxDQUFDLFVBQUEsR0FBRyxJQUFJLE9BQUEsa0JBQWtCLENBQUMsR0FBRyxFQUFFLE1BQU0sQ0FBQyxFQUEvQixDQUErQixDQUFDO2FBQ3pELEdBQUcsQ0FDQSxVQUFDLFNBQW9CLElBQUssT0FBQSxtQkFBbUIsQ0FBQyxTQUFTLEVBQUUsMEJBQTBCLENBQUMsRUFBMUQsQ0FBMEQsQ0FBQyxDQUFDO1FBQ2xHLElBQUksaUJBQWlCLENBQUMsTUFBTSxLQUFLLENBQUMsRUFBRTtZQUNsQyxPQUFPLElBQUksQ0FB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QUFHLENBQUMsVUFBQSxHQUFHO2dCQUNqQyxJQUFNLElBQUksR0FBRyxFQUFFLENBQUMsZUFBZSxDQUFDLEdBQUcsQ0FBQyxDQUFDO2dCQUNyQyxPQUFPLHFCQUFxQixDQUFDLENBQUMsQ0FBQyxzQ0FBK0IsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDO1lBQzlFLENBQUMsQ0FBQyxDQUFDO1lBQ0gsVUFBVSxDQUFDLElBQUksQ0FBQyxFQUFFLENBQUMsd0JBQXdCLENBQUMsTUFBTSxFQUFFLEVBQUUsQ0FBQyxrQkFBa0IsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLENBQUM7U0FDbkY7UUFDRCxPQUFPLEVBQUUsQ0FBQyxtQkFBbUIsQ0FBQyxVQUFVLEVBQUUsSUFBSSxDQUFDLENBQUM7SUFDbEQsQ0FBQztJQUVEOzs7O09BSUc7SUFDSCxTQUFTLGtCQUFrQixDQUFDLFNBQW9CLEVBQUUsTUFBZTtRQUMvRCxPQUFPLE1BQU0sSUFBSSxDQUFDLFNBQVMsQ0FBQyxNQUFNLEtBQUssSUFBSSxJQUFJLFNBQVMsQ0FBQyxNQUFNLENBQUMsSUFBSSxLQUFLLGVBQWUsQ0FBQyxDQUFDO0lBQzVGLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtFeHByZXNzaW9uLCBFeHRlcm5hbEV4cHIsIEZ1bmN0aW9uRXhwciwgSWRlbnRpZmllcnMsIEludm9rZUZ1bmN0aW9uRXhwciwgTGl0ZXJhbEFycmF5RXhwciwgTGl0ZXJhbEV4cHIsIGxpdGVyYWxNYXAsIE5PTkVfVFlQRSwgUmV0dXJuU3RhdGVtZW50LCBTdGF0ZW1lbnQsIFdyYXBwZWROb2RlRXhwcn0gZnJvbSAnQGFuZ3VsYXIvY29tcGlsZXInO1xuaW1wb3J0ICogYXMgdHMgZnJvbSAndHlwZXNjcmlwdCc7XG5cbmltcG9ydCB7RGVmYXVsdEltcG9ydFJlY29yZGVyfSBmcm9tICcuLi8uLi9pbXBvcnRzJztcbmltcG9ydCB7Q3RvclBhcmFtZXRlciwgRGVjb3JhdG9yLCBSZWZsZWN0aW9uSG9zdH0gZnJvbSAnLi4vLi4vcmVmbGVjdGlvbic7XG5cbmltcG9ydCB7dmFsdWVSZWZlcmVuY2VUb0V4cHJlc3Npb24sIHdyYXBGdW5jdGlvbkV4cHJlc3Npb25zSW5QYXJlbnN9IGZyb20gJy4vdXRpbCc7XG5cblxuLyoqXG4gKiBHaXZlbiBhIGNsYXNzIGRlY2xhcmF0aW9uLCBnZW5lcmF0ZSBhIGNhbGwgdG8gYHNldENsYXNzTWV0YWRhdGFgIHdpdGggdGhlIEFuZ3VsYXIgbWV0YWRhdGFcbiAqIHByZXNlbnQgb24gdGhlIGNsYXNzIG9yIGl0cyBtZW1iZXIgZmllbGRzLlxuICpcbiAqIElmIG5vIHN1Y2ggbWV0YWRhdGEgaXMgcHJlc2VudCwgdGhpcyBmdW5jdGlvbiByZXR1cm5zIGBudWxsYC4gT3RoZXJ3aXNlLCB0aGUgY2FsbCBpcyByZXR1cm5lZFxuICogYXMgYSBgU3RhdGVtZW50YCBmb3IgaW5jbHVzaW9uIGFsb25nIHdpdGggdGhlIGNsYXNzLlxuICovXG5leHBvcnQgZnVuY3Rpb24gZ2VuZXJhdGVTZXRDbGFzc01ldGFkYXRhQ2FsbChcbiAgICBjbGF6ejogdHMuRGVjbGFyYXRpb24sIHJlZmxlY3Rpb246IFJlZmxlY3Rpb25Ib3N0LCBkZWZhdWx0SW1wb3J0UmVjb3JkZXI6IERlZmF1bHRJbXBvcnRSZWNvcmRlcixcbiAgICBpc0NvcmU6IGJvb2xlYW4sIGFubm90YXRlRm9yQ2xvc3VyZUNvbXBpbGVyPzogYm9vbGVhbik6IFN0YXRlbWVudHxudWxsIHtcbiAgaWYgKCFyZWZsZWN0aW9uLmlzQ2xhc3MoY2xhenopKSB7XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cbiAgY29uc3QgaWQgPSB0cy51cGRhdGVJZGVudGlmaWVyKHJlZmxlY3Rpb24uZ2V0QWRqYWNlbnROYW1lT2ZDbGFzcyhjbGF6eikpO1xuXG4gIC8vIFJlZmxlY3Qgb3ZlciB0aGUgY2xhc3MgZGVjb3JhdG9ycy4gSWYgbm9uZSBhcmUgcHJlc2VudCwgb3IgdGhvc2UgdGhhdCBhcmUgYXJlbid0IGZyb21cbiAgLy8gQW5ndWxhciwgdGhlbiByZXR1cm4gbnVsbC4gT3RoZXJ3aXNlLCB0dXJuIHRoZW0gaW50byBtZXRhZGF0YS5cbiAgY29uc3QgY2xhc3NEZWNvcmF0b3JzID0gcmVmbGVjdGlvbi5nZXREZWNvcmF0b3JzT2ZEZWNsYXJhdGlvbihjbGF6eik7XG4gIGlmIChjbGFzc0RlY29yYXRvcnMgPT09IG51bGwpIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuICBjb25zdCBuZ0NsYXNzRGVjb3JhdG9ycyA9XG4gICAgICBjbGFzc0RlY29yYXRvcnMuZmlsdGVyKGRlYyA9PiBpc0FuZ3VsYXJEZWNvcmF0b3IoZGVjLCBpc0NvcmUpKVxuICAgICAgICAgIC5tYXAoXG4gICAgICAgICAgICAgIChkZWNvcmF0b3I6IERlY29yYXRvcikgPT4gZGVjb3JhdG9yVG9NZXRhZGF0YShkZWNvcmF0b3IsIGFubm90YXRlRm9yQ2xvc3VyZUNvbXBpbGVyKSk7XG4gIGlmIChuZ0NsYXNzRGVjb3JhdG9ycy5sZW5ndGggPT09IDApIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuICBjb25zdCBtZXRhRGVjb3JhdG9ycyA9IHRzLmNyZWF0ZUFycmF5TGl0ZXJhbChuZ0NsYXNzRGVjb3JhdG9ycyk7XG5cbiAgLy8gQ29udmVydCB0aGUgY29uc3RydWN0b3IgcGFyYW1ldGVycyB0byBtZXRhZGF0YSwgcGFzc2luZyBudWxsIGlmIG5vbmUgYXJlIHByZXNlbnQuXG4gIGxldCBtZXRhQ3RvclBhcmFtZXRlcnM6IEV4cHJlc3Npb24gPSBuZXcgTGl0ZXJhbEV4cHIobnVsbCk7XG4gIGNvbnN0IGNsYXNzQ3RvclBhcmFtZXRlcnMgPSByZWZsZWN0aW9uLmdldENvbnN0cnVjdG9yUGFyYW1ldGVycyhjbGF6eik7XG4gIGlmIChjbGFzc0N0b3JQYXJhbWV0ZXJzICE9PSBudWxsKSB7XG4gICAgY29uc3QgY3RvclBhcmFtZXRlcnMgPSBjbGFzc0N0b3JQYXJhbWV0ZXJzLm1hcChcbiAgICAgICAgcGFyYW0gPT4gY3RvclBhcmFtZXRlclRvTWV0YWRhdGEocGFyYW0sIGRlZmF1bHRJbXBvcnRSZWNvcmRlciwgaXNDb3JlKSk7XG4gICAgbWV0YUN0b3JQYXJhbWV0ZXJzID0gbmV3IEZ1bmN0aW9uRXhwcihbXSwgW1xuICAgICAgbmV3IFJldHVyblN0YXRlbWVudChuZXcgTGl0ZXJhbEFycmF5RXhwcihjdG9yUGFyYW1ldGVycykpLFxuICAgIF0pO1xuICB9XG5cbiAgLy8gRG8gdGhlIHNhbWUgZm9yIHByb3BlcnR5IGRlY29yYXRvcnMuXG4gIGxldCBtZXRhUHJvcERlY29yYXRvcnM6IHRzLkV4cHJlc3Npb24gPSB0cy5jcmVhdGVOdWxsKCk7XG4gIGNvbnN0IGNsYXNzTWVtYmVycyA9IHJlZmxlY3Rpb24uZ2V0TWVtYmVyc09mQ2xhc3MoY2xhenopLmZpbHRlcihcbiAgICAgIG1lbWJlciA9PiAhbWVtYmVyLmlzU3RhdGljICYmIG1lbWJlci5kZWNvcmF0b3JzICE9PSBudWxsICYmIG1lbWJlci5kZWNvcmF0b3JzLmxlbmd0aCA+IDApO1xuICBjb25zdCBkdXBsaWNhdGVEZWNvcmF0ZWRNZW1iZXJOYW1lcyA9XG4gICAgICBjbGFzc01lbWJlcnMubWFwKG1lbWJlciA9PiBtZW1iZXIubmFtZSkuZmlsdGVyKChuYW1lLCBpLCBhcnIpID0+IGFyci5pbmRleE9mKG5hbWUpIDwgaSk7XG4gIGlmIChkdXBsaWNhdGVEZWNvcmF0ZWRNZW1iZXJOYW1lcy5sZW5ndGggPiAwKSB7XG4gICAgLy8gVGhpcyBzaG91bGQgdGhlb3JldGljYWxseSBuZXZlciBoYXBwZW4sIGJlY2F1c2UgdGhlIG9ubHkgd2F5IHRvIGhhdmUgZHVwbGljYXRlIGluc3RhbmNlXG4gICAgLy8gbWVtYmVyIG5hbWVzIGlzIGdldHRlci9zZXR0ZXIgcGFpcnMgYW5kIGRlY29yYXRvcnMgY2Fubm90IGFwcGVhciBpbiBib3RoIGEgZ2V0dGVyIGFuZCB0aGVcbiAgICAvLyBjb3JyZXNwb25kaW5nIHNldHRlci5cbiAgICB0aHJvdyBuZXcgRXJyb3IoXG4gICAgICAgIGBEdXBsaWNhdGUgZGVjb3JhdGVkIHByb3BlcnRpZXMgZm91bmQgb24gY2xhc3MgJyR7Y2xhenoubmFtZS50ZXh0fSc6IGAgK1xuICAgICAgICBkdXBsaWNhdGVEZWNvcmF0ZWRNZW1iZXJOYW1lcy5qb2luKCcsICcpKTtcbiAgfVxuICBjb25zdCBkZWNvcmF0ZWRNZW1iZXJzID1cbiAgICAgIGNsYXNzTWVtYmVycy5tYXAobWVtYmVyID0+IGNsYXNzTWVtYmVyVG9NZXRhZGF0YShtZW1iZXIubmFtZSwgbWVtYmVyLmRlY29yYXRvcnMhLCBpc0NvcmUpKTtcbiAgaWYgKGRlY29yYXRlZE1lbWJlcnMubGVuZ3RoID4gMCkge1xuICAgIG1ldGFQcm9wRGVjb3JhdG9ycyA9IHRzLmNyZWF0ZU9iamVjdExpdGVyYWwoZGVjb3JhdGVkTWVtYmVycyk7XG4gIH1cblxuICAvLyBHZW5lcmF0ZSBhIHB1cmUgY2FsbCB0byBzZXRDbGFzc01ldGFkYXRhIHdpdGggdGhlIGNsYXNzIGlkZW50aWZpZXIgYW5kIGl0cyBtZXRhZGF0YS5cbiAgY29uc3Qgc2V0Q2xhc3NNZXRhZGF0YSA9IG5ldyBFeHRlcm5hbEV4cHIoSWRlbnRpZmllcnMuc2V0Q2xhc3NNZXRhZGF0YSk7XG4gIGNvbnN0IGZuQ2FsbCA9IG5ldyBJbnZva2VGdW5jdGlvbkV4cHIoXG4gICAgICAvKiBmbiAqLyBzZXRDbGFzc01ldGFkYXRhLFxuICAgICAgLyogYXJncyAqL1xuICAgICAgW1xuICAgICAgICBuZXcgV3JhcHBlZE5vZGVFeHByKGlkKSxcbiAgICAgICAgbmV3IFdyYXBwZWROb2RlRXhwcihtZXRhRGVjb3JhdG9ycyksXG4gICAgICAgIG1ldGFDdG9yUGFyYW1ldGVycyxcbiAgICAgICAgbmV3IFdyYXBwZWROb2RlRXhwcihtZXRhUHJvcERlY29yYXRvcnMpLFxuICAgICAgXSk7XG4gIGNvbnN0IGlpZmVGbiA9IG5ldyBGdW5jdGlvbkV4cHIoW10sIFtmbkNhbGwudG9TdG10KCldLCBOT05FX1RZUEUpO1xuICBjb25zdCBpaWZlID0gbmV3IEludm9rZUZ1bmN0aW9uRXhwcihcbiAgICAgIC8qIGZuICovIGlpZmVGbixcbiAgICAgIC8qIGFyZ3MgKi9bXSxcbiAgICAgIC8qIHR5cGUgKi8gdW5kZWZpbmVkLFxuICAgICAgLyogc291cmNlU3BhbiAqLyB1bmRlZmluZWQsXG4gICAgICAvKiBwdXJlICovIHRydWUpO1xuICByZXR1cm4gaWlmZS50b1N0bXQoKTtcbn1cblxuLyoqXG4gKiBDb252ZXJ0IGEgcmVmbGVjdGVkIGNvbnN0cnVjdG9yIHBhcmFtZXRlciB0byBtZXRhZGF0YS5cbiAqL1xuZnVuY3Rpb24gY3RvclBhcmFtZXRlclRvTWV0YWRhdGEoXG4gICAgcGFyYW06IEN0b3JQYXJhbWV0ZXIsIGRlZmF1bHRJbXBvcnRSZWNvcmRlcjogRGVmYXVsdEltcG9ydFJlY29yZGVyLFxuICAgIGlzQ29yZTogYm9vbGVhbik6IEV4cHJlc3Npb24ge1xuICAvLyBQYXJhbWV0ZXJzIHNvbWV0aW1lcyBoYXZlIGEgdHlwZSB0aGF0IGNhbiBiZSByZWZlcmVuY2VkLiBJZiBzbywgdGhlbiB1c2UgaXQsIG90aGVyd2lzZVxuICAvLyBpdHMgdHlwZSBpcyB1bmRlZmluZWQuXG4gIGNvbnN0IHR5cGUgPSBwYXJhbS50eXBlVmFsdWVSZWZlcmVuY2UgIT09IG51bGwgP1xuICAgICAgdmFsdWVSZWZlcmVuY2VUb0V4cHJlc3Npb24ocGFyYW0udHlwZVZhbHVlUmVmZXJlbmNlLCBkZWZhdWx0SW1wb3J0UmVjb3JkZXIpIDpcbiAgICAgIG5ldyBMaXRlcmFsRXhwcih1bmRlZmluZWQpO1xuXG4gIGNvbnN0IG1hcEVudHJpZXM6IHtrZXk6IHN0cmluZywgdmFsdWU6IEV4cHJlc3Npb24sIHF1b3RlZDogZmFsc2V9W10gPSBbXG4gICAge2tleTogJ3R5cGUnLCB2YWx1ZTogdHlwZSwgcXVvdGVkOiBmYWxzZX0sXG4gIF07XG5cbiAgLy8gSWYgdGhlIHBhcmFtZXRlciBoYXMgZGVjb3JhdG9ycywgaW5jbHVkZSB0aGUgb25lcyBmcm9tIEFuZ3VsYXIuXG4gIGlmIChwYXJhbS5kZWNvcmF0b3JzICE9PSBudWxsKSB7XG4gICAgY29uc3QgbmdEZWNvcmF0b3JzID0gcGFyYW0uZGVjb3JhdG9ycy5maWx0ZXIoZGVjID0+IGlzQW5ndWxhckRlY29yYXRvcihkZWMsIGlzQ29yZSkpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgIC5tYXAoKGRlY29yYXRvcjogRGVjb3JhdG9yKSA9PiBkZWNvcmF0b3JUb01ldGFkYXRhKGRlY29yYXRvcikpO1xuICAgIGNvbnN0IHZhbHVlID0gbmV3IFdyYXBwZWROb2RlRXhwcih0cy5jcmVhdGVBcnJheUxpdGVyYWwobmdEZWNvcmF0b3JzKSk7XG4gICAgbWFwRW50cmllcy5wdXNoKHtrZXk6ICdkZWNvcmF0b3JzJywgdmFsdWUsIHF1b3RlZDogZmFsc2V9KTtcbiAgfVxuICByZXR1cm4gbGl0ZXJhbE1hcChtYXBFbnRyaWVzKTtcbn1cblxuLyoqXG4gKiBDb252ZXJ0IGEgcmVmbGVjdGVkIGNsYXNzIG1lbWJlciB0byBtZXRhZGF0YS5cbiAqL1xuZnVuY3Rpb24gY2xhc3NNZW1iZXJUb01ldGFkYXRhKFxuICAgIG5hbWU6IHN0cmluZywgZGVjb3JhdG9yczogRGVjb3JhdG9yW10sIGlzQ29yZTogYm9vbGVhbik6IHRzLlByb3BlcnR5QXNzaWdubWVudCB7XG4gIGNvbnN0IG5nRGVjb3JhdG9ycyA9IGRlY29yYXRvcnMuZmlsdGVyKGRlYyA9PiBpc0FuZ3VsYXJEZWNvcmF0b3IoZGVjLCBpc0NvcmUpKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgLm1hcCgoZGVjb3JhdG9yOiBEZWNvcmF0b3IpID0+IGRlY29yYXRvclRvTWV0YWRhdGEoZGVjb3JhdG9yKSk7XG4gIGNvbnN0IGRlY29yYXRvck1ldGEgPSB0cy5jcmVhdGVBcnJheUxpdGVyYWwobmdEZWNvcmF0b3JzKTtcbiAgcmV0dXJuIHRzLmNyZWF0ZVByb3BlcnR5QXNzaWdubWVudChuYW1lLCBkZWNvcmF0b3JNZXRhKTtcbn1cblxuLyoqXG4gKiBDb252ZXJ0IGEgcmVmbGVjdGVkIGRlY29yYXRvciB0byBtZXRhZGF0YS5cbiAqL1xuZnVuY3Rpb24gZGVjb3JhdG9yVG9NZXRhZGF0YShcbiAgICBkZWNvcmF0b3I6IERlY29yYXRvciwgd3JhcEZ1bmN0aW9uc0luUGFyZW5zPzogYm9vbGVhbik6IHRzLk9iamVjdExpdGVyYWxFeHByZXNzaW9uIHtcbiAgaWYgKGRlY29yYXRvci5pZGVudGlmaWVyID09PSBudWxsKSB7XG4gICAgdGhyb3cgbmV3IEVycm9yKCdJbGxlZ2FsIHN0YXRlOiBzeW50aGVzaXplZCBkZWNvcmF0b3IgY2Fubm90IGJlIGVtaXR0ZWQgaW4gY2xhc3MgbWV0YWRhdGEuJyk7XG4gIH1cbiAgLy8gRGVjb3JhdG9ycyBoYXZlIGEgdHlwZS5cbiAgY29uc3QgcHJvcGVydGllczogdHMuT2JqZWN0TGl0ZXJhbEVsZW1lbnRMaWtlW10gPSBbXG4gICAgdHMuY3JlYXRlUHJvcGVydHlBc3NpZ25tZW50KCd0eXBlJywgdHMuZ2V0TXV0YWJsZUNsb25lKGRlY29yYXRvci5pZGVudGlmaWVyKSksXG4gIF07XG4gIC8vIFNvbWV0aW1lcyB0aGV5IGhhdmUgYXJndW1lbnRzLlxuICBpZiAoZGVjb3JhdG9yLmFyZ3MgIT09IG51bGwgJiYgZGVjb3JhdG9yLmFyZ3MubGVuZ3RoID4gMCkge1xuICAgIGNvbnN0IGFyZ3MgPSBkZWNvcmF0b3IuYXJncy5tYXAoYXJnID0+IHtcbiAgICAgIGNvbnN0IGV4cHIgPSB0cy5nZXRNdXRhYmxlQ2xvbmUoYXJnKTtcbiAgICAgIHJldHVybiB3cmFwRnVuY3Rpb25zSW5QYXJlbnMgPyB3cmFwRnVuY3Rpb25FeHByZXNzaW9uc0luUGFyZW5zKGV4cHIpIDogZXhwcjtcbiAgICB9KTtcbiAgICBwcm9wZXJ0aWVzLnB1c2godHMuY3JlYXRlUHJvcGVydHlBc3NpZ25tZW50KCdhcmdzJywgdHMuY3JlYXRlQXJyYXlMaXRlcmFsKGFyZ3MpKSk7XG4gIH1cbiAgcmV0dXJuIHRzLmNyZWF0ZU9iamVjdExpdGVyYWwocHJvcGVydGllcywgdHJ1ZSk7XG59XG5cbi8qKlxuICogV2hldGhlciBhIGdpdmVuIGRlY29yYXRvciBzaG91bGQgYmUgdHJlYXRlZCBhcyBhbiBBbmd1bGFyIGRlY29yYXRvci5cbiAqXG4gKiBFaXRoZXIgaXQncyB1c2VkIGluIEBhbmd1bGFyL2NvcmUsIG9yIGl0J3MgaW1wb3J0ZWQgZnJvbSB0aGVyZS5cbiAqL1xuZnVuY3Rpb24gaXNBbmd1bGFyRGVjb3JhdG9yKGRlY29yYXRvcjogRGVjb3JhdG9yLCBpc0NvcmU6IGJvb2xlYW4pOiBib29sZWFuIHtcbiAgcmV0dXJuIGlzQ29yZSB8fCAoZGVjb3JhdG9yLmltcG9ydCAhPT0gbnVsbCAmJiBkZWNvcmF0b3IuaW1wb3J0LmZyb20gPT09ICdAYW5ndWxhci9jb3JlJyk7XG59XG4iXX0=