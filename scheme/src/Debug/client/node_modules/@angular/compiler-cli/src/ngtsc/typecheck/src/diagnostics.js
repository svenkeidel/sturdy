(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define("@angular/compiler-cli/src/ngtsc/typecheck/src/diagnostics", ["require", "exports", "@angular/compiler", "typescript", "@angular/compiler-cli/src/ngtsc/util/src/typescript"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.isTemplateDiagnostic = exports.makeTemplateDiagnostic = exports.translateDiagnostic = exports.shouldReportDiagnostic = exports.addTemplateId = exports.addParseSpanInfo = exports.ignoreDiagnostics = exports.wrapForDiagnostics = void 0;
    /**
     * @license
     * Copyright Google LLC All Rights Reserved.
     *
     * Use of this source code is governed by an MIT-style license that can be
     * found in the LICENSE file at https://angular.io/license
     */
    var compiler_1 = require("@angular/compiler");
    var ts = require("typescript");
    var typescript_1 = require("@angular/compiler-cli/src/ngtsc/util/src/typescript");
    /**
     * Wraps the node in parenthesis such that inserted span comments become attached to the proper
     * node. This is an alias for `ts.createParen` with the benefit that it signifies that the
     * inserted parenthesis are for diagnostic purposes, not for correctness of the rendered TCB code.
     *
     * Note that it is important that nodes and its attached comment are not wrapped into parenthesis
     * by default, as it prevents correct translation of e.g. diagnostics produced for incorrect method
     * arguments. Such diagnostics would then be produced for the parenthesised node whereas the
     * positional comment would be located within that node, resulting in a mismatch.
     */
    function wrapForDiagnostics(expr) {
        return ts.createParen(expr);
    }
    exports.wrapForDiagnostics = wrapForDiagnostics;
    var IGNORE_MARKER = 'ignore';
    /**
     * Adds a marker to the node that signifies that any errors within the node should not be reported.
     */
    function ignoreDiagnostics(node) {
        ts.addSyntheticTrailingComment(node, ts.SyntaxKind.MultiLineCommentTrivia, IGNORE_MARKER, /* hasTrailingNewLine */ false);
    }
    exports.ignoreDiagnostics = ignoreDiagnostics;
    /**
     * Adds a synthetic comment to the expression that represents the parse span of the provided node.
     * This comment can later be retrieved as trivia of a node to recover original source locations.
     */
    function addParseSpanInfo(node, span) {
        var commentText;
        if (span instanceof compiler_1.AbsoluteSourceSpan) {
            commentText = span.start + "," + span.end;
        }
        else {
            commentText = span.start.offset + "," + span.end.offset;
        }
        ts.addSyntheticTrailingComment(node, ts.SyntaxKind.MultiLineCommentTrivia, commentText, /* hasTrailingNewLine */ false);
    }
    exports.addParseSpanInfo = addParseSpanInfo;
    /**
     * Adds a synthetic comment to the function declaration that contains the template id
     * of the class declaration.
     */
    function addTemplateId(tcb, id) {
        ts.addSyntheticLeadingComment(tcb, ts.SyntaxKind.MultiLineCommentTrivia, id, true);
    }
    exports.addTemplateId = addTemplateId;
    /**
     * Determines if the diagnostic should be reported. Some diagnostics are produced because of the
     * way TCBs are generated; those diagnostics should not be reported as type check errors of the
     * template.
     */
    function shouldReportDiagnostic(diagnostic) {
        var code = diagnostic.code;
        if (code === 6133 /* $var is declared but its value is never read. */) {
            return false;
        }
        else if (code === 6199 /* All variables are unused. */) {
            return false;
        }
        else if (code === 2695 /* Left side of comma operator is unused and has no side effects. */) {
            return false;
        }
        else if (code === 7006 /* Parameter '$event' implicitly has an 'any' type. */) {
            return false;
        }
        return true;
    }
    exports.shouldReportDiagnostic = shouldReportDiagnostic;
    /**
     * Attempts to translate a TypeScript diagnostic produced during template type-checking to their
     * location of origin, based on the comments that are emitted in the TCB code.
     *
     * If the diagnostic could not be translated, `null` is returned to indicate that the diagnostic
     * should not be reported at all. This prevents diagnostics from non-TCB code in a user's source
     * file from being reported as type-check errors.
     */
    function translateDiagnostic(diagnostic, resolver) {
        if (diagnostic.file === undefined || diagnostic.start === undefined) {
            return null;
        }
        // Locate the node that the diagnostic is reported on and determine its location in the source.
        var node = typescript_1.getTokenAtPosition(diagnostic.file, diagnostic.start);
        var sourceLocation = findSourceLocation(node, diagnostic.file);
        if (sourceLocation === null) {
            return null;
        }
        // Now use the external resolver to obtain the full `ParseSourceFile` of the template.
        var span = resolver.toParseSourceSpan(sourceLocation.id, sourceLocation.span);
        if (span === null) {
            return null;
        }
        var mapping = resolver.getSourceMapping(sourceLocation.id);
        return makeTemplateDiagnostic(mapping, span, diagnostic.category, diagnostic.code, diagnostic.messageText);
    }
    exports.translateDiagnostic = translateDiagnostic;
    /**
     * Constructs a `ts.Diagnostic` for a given `ParseSourceSpan` within a template.
     */
    function makeTemplateDiagnostic(mapping, span, category, code, messageText, relatedMessage) {
        if (mapping.type === 'direct') {
            var relatedInformation = undefined;
            if (relatedMessage !== undefined) {
                relatedInformation = [{
                        category: ts.DiagnosticCategory.Message,
                        code: 0,
                        file: mapping.node.getSourceFile(),
                        start: relatedMessage.span.start.offset,
                        length: relatedMessage.span.end.offset - relatedMessage.span.start.offset,
                        messageText: relatedMessage.text,
                    }];
            }
            // For direct mappings, the error is shown inline as ngtsc was able to pinpoint a string
            // constant within the `@Component` decorator for the template. This allows us to map the error
            // directly into the bytes of the source file.
            return {
                source: 'ngtsc',
                code: code,
                category: category,
                messageText: messageText,
                file: mapping.node.getSourceFile(),
                componentFile: mapping.node.getSourceFile(),
                start: span.start.offset,
                length: span.end.offset - span.start.offset,
                relatedInformation: relatedInformation,
            };
        }
        else if (mapping.type === 'indirect' || mapping.type === 'external') {
            // For indirect mappings (template was declared inline, but ngtsc couldn't map it directly
            // to a string constant in the decorator), the component's file name is given with a suffix
            // indicating it's not the TS file being displayed, but a template.
            // For external temoplates, the HTML filename is used.
            var componentSf = mapping.componentClass.getSourceFile();
            var componentName = mapping.componentClass.name.text;
            // TODO(alxhub): remove cast when TS in g3 supports this narrowing.
            var fileName = mapping.type === 'indirect' ?
                componentSf.fileName + " (" + componentName + " template)" :
                mapping.templateUrl;
            // TODO(alxhub): investigate creating a fake `ts.SourceFile` here instead of invoking the TS
            // parser against the template (HTML is just really syntactically invalid TypeScript code ;).
            // Also investigate caching the file to avoid running the parser multiple times.
            var sf = ts.createSourceFile(fileName, mapping.template, ts.ScriptTarget.Latest, false, ts.ScriptKind.JSX);
            var relatedInformation = [];
            if (relatedMessage !== undefined) {
                relatedInformation.push({
                    category: ts.DiagnosticCategory.Message,
                    code: 0,
                    file: sf,
                    start: relatedMessage.span.start.offset,
                    length: relatedMessage.span.end.offset - relatedMessage.span.start.offset,
                    messageText: relatedMessage.text,
                });
            }
            relatedInformation.push({
                category: ts.DiagnosticCategory.Message,
                code: 0,
                file: componentSf,
                // mapping.node represents either the 'template' or 'templateUrl' expression. getStart()
                // and getEnd() are used because they don't include surrounding whitespace.
                start: mapping.node.getStart(),
                length: mapping.node.getEnd() - mapping.node.getStart(),
                messageText: "Error occurs in the template of component " + componentName + ".",
            });
            return {
                source: 'ngtsc',
                category: category,
                code: code,
                messageText: messageText,
                file: sf,
                componentFile: componentSf,
                start: span.start.offset,
                length: span.end.offset - span.start.offset,
                // Show a secondary message indicating the component whose template contains the error.
                relatedInformation: relatedInformation,
            };
        }
        else {
            throw new Error("Unexpected source mapping type: " + mapping.type);
        }
    }
    exports.makeTemplateDiagnostic = makeTemplateDiagnostic;
    /**
     * Traverses up the AST starting from the given node to extract the source location from comments
     * that have been emitted into the TCB. If the node does not exist within a TCB, or if an ignore
     * marker comment is found up the tree, this function returns null.
     */
    function findSourceLocation(node, sourceFile) {
        // Search for comments until the TCB's function declaration is encountered.
        while (node !== undefined && !ts.isFunctionDeclaration(node)) {
            if (hasIgnoreMarker(node, sourceFile)) {
                // There's an ignore marker on this node, so the diagnostic should not be reported.
                return null;
            }
            var span = readSpanComment(sourceFile, node);
            if (span !== null) {
                // Once the positional information has been extracted, search further up the TCB to extract
                // the unique id that is attached with the TCB's function declaration.
                var id = getTemplateId(node, sourceFile);
                if (id === null) {
                    return null;
                }
                return { id: id, span: span };
            }
            node = node.parent;
        }
        return null;
    }
    function getTemplateId(node, sourceFile) {
        // Walk up to the function declaration of the TCB, the file information is attached there.
        while (!ts.isFunctionDeclaration(node)) {
            if (hasIgnoreMarker(node, sourceFile)) {
                // There's an ignore marker on this node, so the diagnostic should not be reported.
                return null;
            }
            node = node.parent;
            // Bail once we have reached the root.
            if (node === undefined) {
                return null;
            }
        }
        var start = node.getFullStart();
        return ts.forEachLeadingCommentRange(sourceFile.text, start, function (pos, end, kind) {
            if (kind !== ts.SyntaxKind.MultiLineCommentTrivia) {
                return null;
            }
            var commentText = sourceFile.text.substring(pos + 2, end - 2);
            return commentText;
        }) || null;
    }
    var parseSpanComment = /^(\d+),(\d+)$/;
    function readSpanComment(sourceFile, node) {
        return ts.forEachTrailingCommentRange(sourceFile.text, node.getEnd(), function (pos, end, kind) {
            if (kind !== ts.SyntaxKind.MultiLineCommentTrivia) {
                return null;
            }
            var commentText = sourceFile.text.substring(pos + 2, end - 2);
            var match = commentText.match(parseSpanComment);
            if (match === null) {
                return null;
            }
            return new compiler_1.AbsoluteSourceSpan(+match[1], +match[2]);
        }) || null;
    }
    function hasIgnoreMarker(node, sourceFile) {
        return ts.forEachTrailingCommentRange(sourceFile.text, node.getEnd(), function (pos, end, kind) {
            if (kind !== ts.SyntaxKind.MultiLineCommentTrivia) {
                return null;
            }
            var commentText = sourceFile.text.substring(pos + 2, end - 2);
            return commentText === IGNORE_MARKER;
        }) === true;
    }
    function isTemplateDiagnostic(diagnostic) {
        return diagnostic.hasOwnProperty('componentFile') &&
            ts.isSourceFile(diagnostic.componentFile);
    }
    exports.isTemplateDiagnostic = isTemplateDiagnostic;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGlhZ25vc3RpY3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9jb21waWxlci1jbGkvc3JjL25ndHNjL3R5cGVjaGVjay9zcmMvZGlhZ25vc3RpY3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7Ozs7Ozs7Ozs7O0lBQUE7Ozs7OztPQU1HO0lBQ0gsOENBQXNFO0lBQ3RFLCtCQUFpQztJQUVqQyxrRkFBNkQ7SUFrQzdEOzs7Ozs7Ozs7T0FTRztJQUNILFNBQWdCLGtCQUFrQixDQUFDLElBQW1CO1FBQ3BELE9BQU8sRUFBRSxDQUFDLFdBQVcsQ0FBQyxJQUFJLENBQUMsQ0FBQztJQUM5QixDQUFDO0lBRkQsZ0RBRUM7SUFFRCxJQUFNLGFBQWEsR0FBRyxRQUFRLENBQUM7SUFFL0I7O09BRUc7SUFDSCxTQUFnQixpQkFBaUIsQ0FBQyxJQUFhO1FBQzdDLEVBQUUsQ0FBQywyQkFBMkIsQ0FDMUIsSUFBSSxFQUFFLEVBQUUsQ0FBQyxVQUFVLENBQUMsc0JBQXNCLEVBQUUsYUFBYSxFQUFFLHdCQUF3QixDQUFDLEtBQUssQ0FBQyxDQUFDO0lBQ2pHLENBQUM7SUFIRCw4Q0FHQztJQUVEOzs7T0FHRztJQUNILFNBQWdCLGdCQUFnQixDQUFDLElBQWEsRUFBRSxJQUF3QztRQUN0RixJQUFJLFdBQW1CLENBQUM7UUFDeEIsSUFBSSxJQUFJLFlBQVksNkJBQWtCLEVBQUU7WUFDdEMsV0FBVyxHQUFNLElBQUksQ0FBQyxLQUFLLFNBQUksSUFBSSxDQUFDLEdBQUssQ0FBQztTQUMzQzthQUFNO1lBQ0wsV0FBVyxHQUFNLElBQUksQ0FBQyxLQUFLLENBQUMsTUFBTSxTQUFJLElBQUksQ0FBQyxHQUFHLENBQUMsTUFBUSxDQUFDO1NBQ3pEO1FBQ0QsRUFBRSxDQUFDLDJCQUEyQixDQUMxQixJQUFJLEVBQUUsRUFBRSxDQUFDLFVBQVUsQ0FBQyxzQkFBc0IsRUFBRSxXQUFXLEVBQUUsd0JBQXdC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XG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cbmltcG9ydCB7QWJzb2x1dGVTb3VyY2VTcGFuLCBQYXJzZVNvdXJjZVNwYW59IGZyb20gJ0Bhbmd1bGFyL2NvbXBpbGVyJztcbmltcG9ydCAqIGFzIHRzIGZyb20gJ3R5cGVzY3JpcHQnO1xuXG5pbXBvcnQge2dldFRva2VuQXRQb3NpdGlvbn0gZnJvbSAnLi4vLi4vdXRpbC9zcmMvdHlwZXNjcmlwdCc7XG5cbmltcG9ydCB7RXh0ZXJuYWxUZW1wbGF0ZVNvdXJjZU1hcHBpbmcsIFRlbXBsYXRlSWQsIFRlbXBsYXRlU291cmNlTWFwcGluZ30gZnJvbSAnLi9hcGknO1xuXG4vKipcbiAqIEEgYHRzLkRpYWdub3N0aWNgIHdpdGggYWRkaXRpb25hbCBpbmZvcm1hdGlvbiBhYm91dCB0aGUgZGlhZ25vc3RpYyByZWxhdGVkIHRvIHRlbXBsYXRlXG4gKiB0eXBlLWNoZWNraW5nLlxuICovXG5leHBvcnQgaW50ZXJmYWNlIFRlbXBsYXRlRGlhZ25vc3RpYyBleHRlbmRzIHRzLkRpYWdub3N0aWMge1xuICAvKipcbiAgICogVGhlIGNvbXBvbmVudCB3aXRoIHRoZSB0ZW1wbGF0ZSB0aGF0IHJlc3VsdGVkIGluIHRoaXMgZGlhZ25vc3RpYy5cbiAgICovXG4gIGNvbXBvbmVudEZpbGU6IHRzLlNvdXJjZUZpbGU7XG59XG5cbi8qKlxuICogQWRhcHRlciBpbnRlcmZhY2Ugd2hpY2ggYWxsb3dzIHRoZSB0ZW1wbGF0ZSB0eXBlLWNoZWNraW5nIGRpYWdub3N0aWNzIGNvZGUgdG8gaW50ZXJwcmV0IG9mZnNldHNcbiAqIGluIGEgVENCIGFuZCBtYXAgdGhlbSBiYWNrIHRvIG9yaWdpbmFsIGxvY2F0aW9ucyBpbiB0aGUgdGVtcGxhdGUuXG4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgVGVtcGxhdGVTb3VyY2VSZXNvbHZlciB7XG4gIC8qKlxuICAgKiBGb3IgdGhlIGdpdmVuIHRlbXBsYXRlIGlkLCByZXRyaWV2ZSB0aGUgb3JpZ2luYWwgc291cmNlIG1hcHBpbmcgd2hpY2ggZGVzY3JpYmVzIGhvdyB0aGUgb2Zmc2V0c1xuICAgKiBpbiB0aGUgdGVtcGxhdGUgc2hvdWxkIGJlIGludGVycHJldGVkLlxuICAgKi9cbiAgZ2V0U291cmNlTWFwcGluZyhpZDogVGVtcGxhdGVJZCk6IFRlbXBsYXRlU291cmNlTWFwcGluZztcblxuICAvKipcbiAgICogQ29udmVydCBhbiBhYnNvbHV0ZSBzb3VyY2Ugc3BhbiBhc3NvY2lhdGVkIHdpdGggdGhlIGdpdmVuIHRlbXBsYXRlIGlkIGludG8gYSBmdWxsXG4gICAqIGBQYXJzZVNvdXJjZVNwYW5gLiBUaGUgcmV0dXJuZWQgcGFyc2Ugc3BhbiBoYXMgbGluZSBhbmQgY29sdW1uIG51bWJlcnMgaW4gYWRkaXRpb24gdG8gb25seVxuICAgKiBhYnNvbHV0ZSBvZmZzZXRzIGFuZCBnaXZlcyBhY2Nlc3MgdG8gdGhlIG9yaWdpbmFsIHRlbXBsYXRlIHNvdXJjZS5cbiAgICovXG4gIHRvUGFyc2VTb3VyY2VTcGFuKGlkOiBUZW1wbGF0ZUlkLCBzcGFuOiBBYnNvbHV0ZVNvdXJjZVNwYW4pOiBQYXJzZVNvdXJjZVNwYW58bnVsbDtcbn1cblxuLyoqXG4gKiBXcmFwcyB0aGUgbm9kZSBpbiBwYXJlbnRoZXNpcyBzdWNoIHRoYXQgaW5zZXJ0ZWQgc3BhbiBjb21tZW50cyBiZWNvbWUgYXR0YWNoZWQgdG8gdGhlIHByb3BlclxuICogbm9kZS4gVGhpcyBpcyBhbiBhbGlhcyBmb3IgYHRzLmNyZWF0ZVBhcmVuYCB3aXRoIHRoZSBiZW5lZml0IHRoYXQgaXQgc2lnbmlmaWVzIHRoYXQgdGhlXG4gKiBpbnNlcnRlZCBwYXJlbnRoZXNpcyBhcmUgZm9yIGRpYWdub3N0aWMgcHVycG9zZXMsIG5vdCBmb3IgY29ycmVjdG5lc3Mgb2YgdGhlIHJlbmRlcmVkIFRDQiBjb2RlLlxuICpcbiAqIE5vdGUgdGhhdCBpdCBpcyBpbXBvcnRhbnQgdGhhdCBub2RlcyBhbmQgaXRzIGF0dGFjaGVkIGNvbW1lbnQgYXJlIG5vdCB3cmFwcGVkIGludG8gcGFyZW50aGVzaXNcbiAqIGJ5IGRlZmF1bHQsIGFzIGl0IHByZXZlbnRzIGNvcnJlY3QgdHJhbnNsYXRpb24gb2YgZS5nLiBkaWFnbm9zdGljcyBwcm9kdWNlZCBmb3IgaW5jb3JyZWN0IG1ldGhvZFxuICogYXJndW1lbnRzLiBTdWNoIGRpYWdub3N0aWNzIHdvdWxkIHRoZW4gYmUgcHJvZHVjZWQgZm9yIHRoZSBwYXJlbnRoZXNpc2VkIG5vZGUgd2hlcmVhcyB0aGVcbiAqIHBvc2l0aW9uYWwgY29tbWVudCB3b3VsZCBiZSBsb2NhdGVkIHdpdGhpbiB0aGF0IG5vZGUsIHJlc3VsdGluZyBpbiBhIG1pc21hdGNoLlxuICovXG5leHBvcnQgZnVuY3Rpb24gd3JhcEZvckRpYWdub3N0aWNzKGV4cHI6IHRzLkV4cHJlc3Npb24pOiB0cy5FeHByZXNzaW9uIHtcbiAgcmV0dXJuIHRzLmNyZWF0ZVBhcmVuKGV4cHIpO1xufVxuXG5jb25zdCBJR05PUkVfTUFSS0VSID0gJ2lnbm9yZSc7XG5cbi8qKlxuICogQWRkcyBhIG1hcmtlciB0byB0aGUgbm9kZSB0aGF0IHNpZ25pZmllcyB0aGF0IGFueSBlcnJvcnMgd2l0aGluIHRoZSBub2RlIHNob3VsZCBub3QgYmUgcmVwb3J0ZWQuXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBpZ25vcmVEaWFnbm9zdGljcyhub2RlOiB0cy5Ob2RlKTogdm9pZCB7XG4gIHRzLmFkZFN5bnRoZXRpY1RyYWlsaW5nQ29tbWVudChcbiAgICAgIG5vZGUsIHRzLlN5bnRheEtpbmQuTXVsdGlMaW5lQ29tbWVudFRyaXZpYSwgSUdOT1JFX01BUktFUiwgLyogaGFzVHJhaWxpbmdOZXdMaW5lICovIGZhbHNlKTtcbn1cblxuLyoqXG4gKiBBZGRzIGEgc3ludGhldGljIGNvbW1lbnQgdG8gdGhlIGV4cHJlc3Npb24gdGhhdCByZXByZXNlbnRzIHRoZSBwYXJzZSBzcGFuIG9mIHRoZSBwcm92aWRlZCBub2RlLlxuICogVGhpcyBjb21tZW50IGNhbiBsYXRlciBiZSByZXRyaWV2ZWQgYXMgdHJpdmlhIG9mIGEgbm9kZSB0byByZWNvdmVyIG9yaWdpbmFsIHNvdXJjZSBsb2NhdGlvbnMuXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBhZGRQYXJzZVNwYW5JbmZvKG5vZGU6IHRzLk5vZGUsIHNwYW46IEFic29sdXRlU291cmNlU3BhbnxQYXJzZVNvdXJjZVNwYW4pOiB2b2lkIHtcbiAgbGV0IGNvbW1lbnRUZXh0OiBzdHJpbmc7XG4gIGlmIChzcGFuIGluc3RhbmNlb2YgQWJzb2x1dGVTb3VyY2VTcGFuKSB7XG4gICAgY29tbWVudFRleHQgPSBgJHtzcGFuLnN0YXJ0fSwke3NwYW4uZW5kfWA7XG4gIH0gZWxzZSB7XG4gICAgY29tbWVudFRleHQgPSBgJHtzcGFuLnN0YXJ0Lm9mZnNldH0sJHtzcGFuLmVuZC5vZmZzZXR9YDtcbiAgfVxuICB0cy5hZGRTeW50aGV0aWNUcmFpbGluZ0NvbW1lbnQoXG4gICAgICBub2RlLCB0cy5TeW50YXhLaW5kLk11bHRpTGluZUNvbW1lbnRUcml2aWEsIGNvbW1lbnRUZXh0LCAvKiBoYXNUcmFpbGluZ05ld0xpbmUgKi8gZmFsc2UpO1xufVxuXG4vKipcbiAqIEFkZHMgYSBzeW50aGV0aWMgY29tbWVudCB0byB0aGUgZnVuY3Rpb24gZGVjbGFyYXRpb24gdGhhdCBjb250YWlucyB0aGUgdGVtcGxhdGUgaWRcbiAqIG9mIHRoZSBjbGFzcyBkZWNsYXJhdGlvbi5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGFkZFRlbXBsYXRlSWQodGNiOiB0cy5GdW5jdGlvbkRlY2xhcmF0aW9uLCBpZDogVGVtcGxhdGVJZCk6IHZvaWQge1xuICB0cy5hZGRTeW50aGV0aWNMZWFkaW5nQ29tbWVudCh0Y2IsIHRzLlN5bnRheEtpbmQuTXVsdGlMaW5lQ29tbWVudFRyaXZpYSwgaWQsIHRydWUpO1xufVxuXG4vKipcbiAqIERldGVybWluZXMgaWYgdGhlIGRpYWdub3N0aWMgc2hvdWxkIGJlIHJlcG9ydGVkLiBTb21lIGRpYWdub3N0aWNzIGFyZSBwcm9kdWNlZCBiZWNhdXNlIG9mIHRoZVxuICogd2F5IFRDQnMgYXJlIGdlbmVyYXRlZDsgdGhvc2UgZGlhZ25vc3RpY3Mgc2hvdWxkIG5vdCBiZSByZXBvcnRlZCBhcyB0eXBlIGNoZWNrIGVycm9ycyBvZiB0aGVcbiAqIHRlbXBsYXRlLlxuICovXG5leHBvcnQgZnVuY3Rpb24gc2hvdWxkUmVwb3J0RGlhZ25vc3RpYyhkaWFnbm9zdGljOiB0cy5EaWFnbm9zdGljKTogYm9vbGVhbiB7XG4gIGNvbnN0IHtjb2RlfSA9IGRpYWdub3N0aWM7XG4gIGlmIChjb2RlID09PSA2MTMzIC8qICR2YXIgaXMgZGVjbGFyZWQgYnV0IGl0cyB2YWx1ZSBpcyBuZXZlciByZWFkLiAqLykge1xuICAgIHJldHVybiBmYWxzZTtcbiAgfSBlbHNlIGlmIChjb2RlID09PSA2MTk5IC8qIEFsbCB2YXJpYWJsZXMgYXJlIHVudXNlZC4gKi8pIHtcbiAgICByZXR1cm4gZmFsc2U7XG4gIH0gZWxzZSBpZiAoY29kZSA9PT0gMjY5NSAvKiBMZWZ0IHNpZGUgb2YgY29tbWEgb3BlcmF0b3IgaXMgdW51c2VkIGFuZCBoYXMgbm8gc2lkZSBlZmZlY3RzLiAqLykge1xuICAgIHJldHVybiBmYWxzZTtcbiAgfSBlbHNlIGlmIChjb2RlID09PSA3MDA2IC8qIFBhcmFtZXRlciAnJGV2ZW50JyBpbXBsaWNpdGx5IGhhcyBhbiAnYW55JyB0eXBlLiAqLykge1xuICAgIHJldHVybiBmYWxzZTtcbiAgfVxuICByZXR1cm4gdHJ1ZTtcbn1cblxuLyoqXG4gKiBBdHRlbXB0cyB0byB0cmFuc2xhdGUgYSBUeXBlU2NyaXB0IGRpYWdub3N0aWMgcHJvZHVjZWQgZHVyaW5nIHRlbXBsYXRlIHR5cGUtY2hlY2tpbmcgdG8gdGhlaXJcbiAqIGxvY2F0aW9uIG9mIG9yaWdpbiwgYmFzZWQgb24gdGhlIGNvbW1lbnRzIHRoYXQgYXJlIGVtaXR0ZWQgaW4gdGhlIFRDQiBjb2RlLlxuICpcbiAqIElmIHRoZSBkaWFnbm9zdGljIGNvdWxkIG5vdCBiZSB0cmFuc2xhdGVkLCBgbnVsbGAgaXMgcmV0dXJuZWQgdG8gaW5kaWNhdGUgdGhhdCB0aGUgZGlhZ25vc3RpY1xuICogc2hvdWxkIG5vdCBiZSByZXBvcnRlZCBhdCBhbGwuIFRoaXMgcHJldmVudHMgZGlhZ25vc3RpY3MgZnJvbSBub24tVENCIGNvZGUgaW4gYSB1c2VyJ3Mgc291cmNlXG4gKiBmaWxlIGZyb20gYmVpbmcgcmVwb3J0ZWQgYXMgdHlwZS1jaGVjayBlcnJvcnMuXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiB0cmFuc2xhdGVEaWFnbm9zdGljKFxuICAgIGRpYWdub3N0aWM6IHRzLkRpYWdub3N0aWMsIHJlc29sdmVyOiBUZW1wbGF0ZVNvdXJjZVJlc29sdmVyKTogdHMuRGlhZ25vc3RpY3xudWxsIHtcbiAgaWYgKGRpYWdub3N0aWMuZmlsZSA9PT0gdW5kZWZpbmVkIHx8IGRpYWdub3N0aWMuc3RhcnQgPT09IHVuZGVmaW5lZCkge1xuICAgIHJldHVybiBudWxsO1xuICB9XG5cbiAgLy8gTG9jYXRlIHRoZSBub2RlIHRoYXQgdGhlIGRpYWdub3N0aWMgaXMgcmVwb3J0ZWQgb24gYW5kIGRldGVybWluZSBpdHMgbG9jYXRpb24gaW4gdGhlIHNvdXJjZS5cbiAgY29uc3Qgbm9kZSA9IGdldFRva2VuQXRQb3NpdGlvbihkaWFnbm9zdGljLmZpbGUsIGRpYWdub3N0aWMuc3RhcnQpO1xuICBjb25zdCBzb3VyY2VMb2NhdGlvbiA9IGZpbmRTb3VyY2VMb2NhdGlvbihub2RlLCBkaWFnbm9zdGljLmZpbGUpO1xuICBpZiAoc291cmNlTG9jYXRpb24gPT09IG51bGwpIHtcbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuXG4gIC8vIE5vdyB1c2UgdGhlIGV4dGVybmFsIHJlc29sdmVyIHRvIG9idGFpbiB0aGUgZnVsbCBgUGFyc2VTb3VyY2VGaWxlYCBvZiB0aGUgdGVtcGxhdGUuXG4gIGNvbnN0IHNwYW4gPSByZXNvbHZlci50b1BhcnNlU291cmNlU3Bhbihzb3VyY2VMb2NhdGlvbi5pZCwgc291cmNlTG9jYXRpb24uc3Bhbik7XG4gIGlmIChzcGFuID09PSBudWxsKSB7XG4gICAgcmV0dXJuIG51bGw7XG4gIH1cblxuICBjb25zdCBtYXBwaW5nID0gcmVzb2x2ZXIuZ2V0U291cmNlTWFwcGluZyhzb3VyY2VMb2NhdGlvbi5pZCk7XG4gIHJldHVybiBtYWtlVGVtcGxhdGVEaWFnbm9zdGljKFxuICAgICAgbWFwcGluZywgc3BhbiwgZGlhZ25vc3RpYy5jYXRlZ29yeSwgZGlhZ25vc3RpYy5jb2RlLCBkaWFnbm9zdGljLm1lc3NhZ2VUZXh0KTtcbn1cblxuLyoqXG4gKiBDb25zdHJ1Y3RzIGEgYHRzLkRpYWdub3N0aWNgIGZvciBhIGdpdmVuIGBQYXJzZVNvdXJjZVNwYW5gIHdpdGhpbiBhIHRlbXBsYXRlLlxuICovXG5leHBvcnQgZnVuY3Rpb24gbWFrZVRlbXBsYXRlRGlhZ25vc3RpYyhcbiAgICBtYXBwaW5nOiBUZW1wbGF0ZVNvdXJjZU1hcHBpbmcsIHNwYW46IFBhcnNlU291cmNlU3BhbiwgY2F0ZWdvcnk6IHRzLkRpYWdub3N0aWNDYXRlZ29yeSxcbiAgICBjb2RlOiBudW1iZXIsIG1lc3NhZ2VUZXh0OiBzdHJpbmd8dHMuRGlhZ25vc3RpY01lc3NhZ2VDaGFpbiwgcmVsYXRlZE1lc3NhZ2U/OiB7XG4gICAgICB0ZXh0OiBzdHJpbmcsXG4gICAgICBzcGFuOiBQYXJzZVNvdXJjZVNwYW4sXG4gICAgfSk6IFRlbXBsYXRlRGlhZ25vc3RpYyB7XG4gIGlmIChtYXBwaW5nLnR5cGUgPT09ICdkaXJlY3QnKSB7XG4gICAgbGV0IHJlbGF0ZWRJbmZvcm1hdGlvbjogdHMuRGlhZ25vc3RpY1JlbGF0ZWRJbmZvcm1hdGlvbltdfHVuZGVmaW5lZCA9IHVuZGVmaW5lZDtcbiAgICBpZiAocmVsYXRlZE1lc3NhZ2UgIT09IHVuZGVmaW5lZCkge1xuICAgICAgcmVsYXRlZEluZm9ybWF0aW9uID0gW3tcbiAgICAgICAgY2F0ZWdvcnk6IHRzLkRpYWdub3N0aWNDYXRlZ29yeS5NZXNzYWdlLFxuICAgICAgICBjb2RlOiAwLFxuICAgICAgICBmaWxlOiBtYXBwaW5nLm5vZGUuZ2V0U291cmNlRmlsZSgpLFxuICAgICAgICBzdGFydDogcmVsYXRlZE1lc3NhZ2Uuc3Bhbi5zdGFydC5vZmZzZXQsXG4gICAgICAgIGxlbmd0aDogcmVsYXRlZE1lc3NhZ2Uuc3Bhbi5lbmQub2Zmc2V0IC0gcmVsYXRlZE1lc3NhZ2Uuc3Bhbi5zdGFydC5vZmZzZXQsXG4gICAgICAgIG1lc3NhZ2VUZXh0OiByZWxhdGVkTWVzc2FnZS50ZXh0LFxuICAgICAgfV07XG4gICAgfVxuICAgIC8vIEZvciBkaXJlY3QgbWFwcGluZ3MsIHRoZSBlcnJvciBpcyBzaG93biBpbmxpbmUgYXMgbmd0c2Mgd2FzIGFibGUgdG8gcGlucG9pbnQgYSBzdHJpbmdcbiAgICAvLyBjb25zdGFudCB3aXRoaW4gdGhlIGBAQ29tcG9uZW50YCBkZWNvcmF0b3IgZm9yIHRoZSB0ZW1wbGF0ZS4gVGhpcyBhbGxvd3MgdXMgdG8gbWFwIHRoZSBlcnJvclxuICAgIC8vIGRpcmVjdGx5IGludG8gdGhlIGJ5dGVzIG9mIHRoZSBzb3VyY2UgZmlsZS5cbiAgICByZXR1cm4ge1xuICAgICAgc291cmNlOiAnbmd0c2MnLFxuICAgICAgY29kZSxcbiAgICAgIGNhdGVnb3J5LFxuICAgICAgbWVzc2FnZVRleHQsXG4gICAgICBmaWxlOiBtYXBwaW5nLm5vZGUuZ2V0U291cmNlRmlsZSgpLFxuICAgICAgY29tcG9uZW50RmlsZTogbWFwcGluZy5ub2RlLmdldFNvdXJjZUZpbGUoKSxcbiAgICAgIHN0YXJ0OiBzcGFuLnN0YXJ0Lm9mZnNldCxcbiAgICAgIGxlbmd0aDogc3Bhbi5lbmQub2Zmc2V0IC0gc3Bhbi5zdGFydC5vZmZzZXQsXG4gICAgICByZWxhdGVkSW5mb3JtYXRpb24sXG4gICAgfTtcbiAgfSBlbHNlIGlmIChtYXBwaW5nLnR5cGUgPT09ICdpbmRpcmVjdCcgfHwgbWFwcGluZy50eXBlID09PSAnZXh0ZXJuYWwnKSB7XG4gICAgLy8gRm9yIGluZGlyZWN0IG1hcHBpbmdzICh0ZW1wbGF0ZSB3YXMgZGVjbGFyZWQgaW5saW5lLCBidXQgbmd0c2MgY291bGRuJ3QgbWFwIGl0IGRpcmVjdGx5XG4gICAgLy8gdG8gYSBzdHJpbmcgY29uc3RhbnQgaW4gdGhlIGRlY29yYXRvciksIHRoZSBjb21wb25lbnQncyBmaWxlIG5hbWUgaXMgZ2l2ZW4gd2l0aCBhIHN1ZmZpeFxuICAgIC8vIGluZGljYXRpbmcgaXQncyBub3QgdGhlIFRTIGZpbGUgYmVpbmcgZGlzcGxheWVkLCBidXQgYSB0ZW1wbGF0ZS5cbiAgICAvLyBGb3IgZXh0ZXJuYWwgdGVtb3BsYXRlcywgdGhlIEhUTUwgZmlsZW5hbWUgaXMgdXNlZC5cbiAgICBjb25zdCBjb21wb25lbnRTZiA9IG1hcHBpbmcuY29tcG9uZW50Q2xhc3MuZ2V0U291cmNlRmlsZSgpO1xuICAgIGNvbnN0IGNvbXBvbmVudE5hbWUgPSBtYXBwaW5nLmNvbXBvbmVudENsYXNzLm5hbWUudGV4dDtcbiAgICAvLyBUT0RPKGFseGh1Yik6IHJlbW92ZSBjYXN0IHdoZW4gVFMgaW4gZzMgc3VwcG9ydHMgdGhpcyBuYXJyb3dpbmcuXG4gICAgY29uc3QgZmlsZU5hbWUgPSBtYXBwaW5nLnR5cGUgPT09ICdpbmRpcmVjdCcgP1xuICAgICAgICBgJHtjb21wb25lbnRTZi5maWxlTmFtZX0gKCR7Y29tcG9uZW50TmFtZX0gdGVtcGxhdGUpYCA6XG4gICAgICAgIChtYXBwaW5nIGFzIEV4dGVybmFsVGVtcGxhdGVTb3VyY2VNYXBwaW5nKS50ZW1wbGF0ZVVybDtcbiAgICAvLyBUT0RPKGFseGh1Yik6IGludmVzdGlnYXRlIGNyZWF0aW5nIGEgZmFrZSBgdHMuU291cmNlRmlsZWAgaGVyZSBpbnN0ZWFkIG9mIGludm9raW5nIHRoZSBUU1xuICAgIC8vIHBhcnNlciBhZ2FpbnN0IHRoZSB0ZW1wbGF0ZSAoSFRNTCBpcyBqdXN0IHJlYWxseSBzeW50YWN0aWNhbGx5IGludmFsaWQgVHlwZVNjcmlwdCBjb2RlIDspLlxuICAgIC8vIEFsc28gaW52ZXN0aWdhdGUgY2FjaGluZyB0aGUgZmlsZSB0byBhdm9pZCBydW5uaW5nIHRoZSBwYXJzZXIgbXVsdGlwbGUgdGltZXMuXG4gICAgY29uc3Qgc2YgPSB0cy5jcmVhdGVTb3VyY2VGaWxlKFxuICAgICAgICBmaWxlTmFtZSwgbWFwcGluZy50ZW1wbGF0ZSwgdHMuU2NyaXB0VGFyZ2V0LkxhdGVzdCwgZmFsc2UsIHRzLlNjcmlwdEtpbmQuSlNYKTtcblxuICAgIGxldCByZWxhdGVkSW5mb3JtYXRpb246IHRzLkRpYWdub3N0aWNSZWxhdGVkSW5mb3JtYXRpb25bXSA9IFtdO1xuICAgIGlmIChyZWxhdGVkTWVzc2FnZSAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICByZWxhdGVkSW5mb3JtYXRpb24ucHVzaCh7XG4gICAgICAgIGNhdGVnb3J5OiB0cy5EaWFnbm9zdGljQ2F0ZWdvcnkuTWVzc2FnZSxcbiAgICAgICAgY29kZTogMCxcbiAgICAgICAgZmlsZTogc2YsXG4gICAgICAgIHN0YXJ0OiByZWxhdGVkTWVzc2FnZS5zcGFuLnN0YXJ0Lm9mZnNldCxcbiAgICAgICAgbGVuZ3RoOiByZWxhdGVkTWVzc2FnZS5zcGFuLmVuZC5vZmZzZXQgLSByZWxhdGVkTWVzc2FnZS5zcGFuLnN0YXJ0Lm9mZnNldCxcbiAgICAgICAgbWVzc2FnZVRleHQ6IHJlbGF0ZWRNZXNzYWdlLnRleHQsXG4gICAgICB9KTtcbiAgICB9XG5cbiAgICByZWxhdGVkSW5mb3JtYXRpb24ucHVzaCh7XG4gICAgICBjYXRlZ29yeTogdHMuRGlhZ25vc3RpY0NhdGVnb3J5Lk1lc3NhZ2UsXG4gICAgICBjb2RlOiAwLFxuICAgICAgZmlsZTogY29tcG9uZW50U2YsXG4gICAgICAvLyBtYXBwaW5nLm5vZGUgcmVwcmVzZW50cyBlaXRoZXIgdGhlICd0ZW1wbGF0ZScgb3IgJ3RlbXBsYXRlVXJsJyBleHByZXNzaW9uLiBnZXRTdGFydCgpXG4gICAgICAvLyBhbmQgZ2V0RW5kKCkgYXJlIHVzZWQgYmVjYXVzZSB0aGV5IGRvbid0IGluY2x1ZGUgc3Vycm91bmRpbmcgd2hpdGVzcGFjZS5cbiAgICAgIHN0YXJ0OiBtYXBwaW5nLm5vZGUuZ2V0U3RhcnQoKSxcbiAgICAgIGxlbmd0aDogbWFwcGluZy5ub2RlLmdldEVuZCgpIC0gbWFwcGluZy5ub2RlLmdldFN0YXJ0KCksXG4gICAgICBtZXNzYWdlVGV4dDogYEVycm9yIG9jY3VycyBpbiB0aGUgdGVtcGxhdGUgb2YgY29tcG9uZW50ICR7Y29tcG9uZW50TmFtZX0uYCxcbiAgICB9KTtcblxuICAgIHJldHVybiB7XG4gICAgICBzb3VyY2U6ICduZ3RzYycsXG4gICAgICBjYXRlZ29yeSxcbiAgICAgIGNvZGUsXG4gICAgICBtZXNzYWdlVGV4dCxcbiAgICAgIGZpbGU6IHNmLFxuICAgICAgY29tcG9uZW50RmlsZTogY29tcG9uZW50U2YsXG4gICAgICBzdGFydDogc3Bhbi5zdGFydC5vZmZzZXQsXG4gICAgICBsZW5ndGg6IHNwYW4uZW5kLm9mZnNldCAtIHNwYW4uc3RhcnQub2Zmc2V0LFxuICAgICAgLy8gU2hvdyBhIHNlY29uZGFyeSBtZXNzYWdlIGluZGljYXRpbmcgdGhlIGNvbXBvbmVudCB3aG9zZSB0ZW1wbGF0ZSBjb250YWlucyB0aGUgZXJyb3IuXG4gICAgICByZWxhdGVkSW5mb3JtYXRpb24sXG4gICAgfTtcbiAgfSBlbHNlIHtcbiAgICB0aHJvdyBuZXcgRXJyb3IoYFVuZXhwZWN0ZWQgc291cmNlIG1hcHBpbmcgdHlwZTogJHsobWFwcGluZyBhcyB7dHlwZTogc3RyaW5nfSkudHlwZX1gKTtcbiAgfVxufVxuXG5pbnRlcmZhY2UgU291cmNlTG9jYXRpb24ge1xuICBpZDogVGVtcGxhdGVJZDtcbiAgc3BhbjogQWJzb2x1dGVTb3VyY2VTcGFuO1xufVxuXG4vKipcbiAqIFRyYXZlcnNlcyB1cCB0aGUgQVNUIHN0YXJ0aW5nIGZyb20gdGhlIGdpdmVuIG5vZGUgdG8gZXh0cmFjdCB0aGUgc291cmNlIGxvY2F0aW9uIGZyb20gY29tbWVudHNcbiAqIHRoYXQgaGF2ZSBiZWVuIGVtaXR0ZWQgaW50byB0aGUgVENCLiBJZiB0aGUgbm9kZSBkb2VzIG5vdCBleGlzdCB3aXRoaW4gYSBUQ0IsIG9yIGlmIGFuIGlnbm9yZVxuICogbWFya2VyIGNvbW1lbnQgaXMgZm91bmQgdXAgdGhlIHRyZWUsIHRoaXMgZnVuY3Rpb24gcmV0dXJucyBudWxsLlxuICovXG5mdW5jdGlvbiBmaW5kU291cmNlTG9jYXRpb24obm9kZTogdHMuTm9kZSwgc291cmNlRmlsZTogdHMuU291cmNlRmlsZSk6IFNvdXJjZUxvY2F0aW9ufG51bGwge1xuICAvLyBTZWFyY2ggZm9yIGNvbW1lbnRzIHVudGlsIHRoZSBUQ0IncyBmdW5jdGlvbiBkZWNsYXJhdGlvbiBpcyBlbmNvdW50ZXJlZC5cbiAgd2hpbGUgKG5vZGUgIT09IHVuZGVmaW5lZCAmJiAhdHMuaXNGdW5jdGlvbkRlY2xhcmF0aW9uKG5vZGUpKSB7XG4gICAgaWYgKGhhc0lnbm9yZU1hcmtlcihub2RlLCBzb3VyY2VGaWxlKSkge1xuICAgICAgLy8gVGhlcmUncyBhbiBpZ25vcmUgbWFya2VyIG9uIHRoaXMgbm9kZSwgc28gdGhlIGRpYWdub3N0aWMgc2hvdWxkIG5vdCBiZSByZXBvcnRlZC5cbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIGNvbnN0IHNwYW4gPSByZWFkU3BhbkNvbW1lbnQoc291cmNlRmlsZSwgbm9kZSk7XG4gICAgaWYgKHNwYW4gIT09IG51bGwpIHtcbiAgICAgIC8vIE9uY2UgdGhlIHBvc2l0aW9uYWwgaW5mb3JtYXRpb24gaGFzIGJlZW4gZXh0cmFjdGVkLCBzZWFyY2ggZnVydGhlciB1cCB0aGUgVENCIHRvIGV4dHJhY3RcbiAgICAgIC8vIHRoZSB1bmlxdWUgaWQgdGhhdCBpcyBhdHRhY2hlZCB3aXRoIHRoZSBUQ0IncyBmdW5jdGlvbiBkZWNsYXJhdGlvbi5cbiAgICAgIGNvbnN0IGlkID0gZ2V0VGVtcGxhdGVJZChub2RlLCBzb3VyY2VGaWxlKTtcbiAgICAgIGlmIChpZCA9PT0gbnVsbCkge1xuICAgICAgICByZXR1cm4gbnVsbDtcbiAgICAgIH1cbiAgICAgIHJldHVybiB7aWQsIHNwYW59O1xuICAgIH1cblxuICAgIG5vZGUgPSBub2RlLnBhcmVudDtcbiAgfVxuXG4gIHJldHVybiBudWxsO1xufVxuXG5mdW5jdGlvbiBnZXRUZW1wbGF0ZUlkKG5vZGU6IHRzLk5vZGUsIHNvdXJjZUZpbGU6IHRzLlNvdXJjZUZpbGUpOiBUZW1wbGF0ZUlkfG51bGwge1xuICAvLyBXYWxrIHVwIHRvIHRoZSBmdW5jdGlvbiBkZWNsYXJhdGlvbiBvZiB0aGUgVENCLCB0aGUgZmlsZSBpbmZvcm1hdGlvbiBpcyBhdHRhY2hlZCB0aGVyZS5cbiAgd2hpbGUgKCF0cy5pc0Z1bmN0aW9uRGVjbGFyYXRpb24obm9kZSkpIHtcbiAgICBpZiAoaGFzSWdub3JlTWFya2VyKG5vZGUsIHNvdXJjZUZpbGUpKSB7XG4gICAgICAvLyBUaGVyZSdzIGFuIGlnbm9yZSBtYXJrZXIgb24gdGhpcyBub2RlLCBzbyB0aGUgZGlhZ25vc3RpYyBzaG91bGQgbm90IGJlIHJlcG9ydGVkLlxuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIG5vZGUgPSBub2RlLnBhcmVudDtcblxuICAgIC8vIEJhaWwgb25jZSB3ZSBoYXZlIHJlYWNoZWQgdGhlIHJvb3QuXG4gICAgaWYgKG5vZGUgPT09IHVuZGVmaW5lZCkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICB9XG5cbiAgY29uc3Qgc3RhcnQgPSBub2RlLmdldEZ1bGxTdGFydCgpO1xuICByZXR1cm4gdHMuZm9yRWFjaExlYWRpbmdDb21tZW50UmFuZ2Uoc291cmNlRmlsZS50ZXh0LCBzdGFydCwgKHBvcywgZW5kLCBraW5kKSA9PiB7XG4gICAgaWYgKGtpbmQgIT09IHRzLlN5bnRheEtpbmQuTXVsdGlMaW5lQ29tbWVudFRyaXZpYSkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuICAgIGNvbnN0IGNvbW1lbnRUZXh0ID0gc291cmNlRmlsZS50ZXh0LnN1YnN0cmluZyhwb3MgKyAyLCBlbmQgLSAyKTtcbiAgICByZXR1cm4gY29tbWVudFRleHQ7XG4gIH0pIGFzIFRlbXBsYXRlSWQgfHwgbnVsbDtcbn1cblxuY29uc3QgcGFyc2VTcGFuQ29tbWVudCA9IC9eKFxcZCspLChcXGQrKSQvO1xuXG5mdW5jdGlvbiByZWFkU3BhbkNvbW1lbnQoc291cmNlRmlsZTogdHMuU291cmNlRmlsZSwgbm9kZTogdHMuTm9kZSk6IEFic29sdXRlU291cmNlU3BhbnxudWxsIHtcbiAgcmV0dXJuIHRzLmZvckVhY2hUcmFpbGluZ0NvbW1lbnRSYW5nZShzb3VyY2VGaWxlLnRleHQsIG5vZGUuZ2V0RW5kKCksIChwb3MsIGVuZCwga2luZCkgPT4ge1xuICAgIGlmIChraW5kICE9PSB0cy5TeW50YXhLaW5kLk11bHRpTGluZUNvbW1lbnRUcml2aWEpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cbiAgICBjb25zdCBjb21tZW50VGV4dCA9IHNvdXJjZUZpbGUudGV4dC5zdWJzdHJpbmcocG9zICsgMiwgZW5kIC0gMik7XG4gICAgY29uc3QgbWF0Y2ggPSBjb21tZW50VGV4dC5tYXRjaChwYXJzZVNwYW5Db21tZW50KTtcbiAgICBpZiAobWF0Y2ggPT09IG51bGwpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIHJldHVybiBuZXcgQWJzb2x1dGVTb3VyY2VTcGFuKCttYXRjaFsxXSwgK21hdGNoWzJdKTtcbiAgfSkgfHwgbnVsbDtcbn1cblxuZnVuY3Rpb24gaGFzSWdub3JlTWFya2VyKG5vZGU6IHRzLk5vZGUsIHNvdXJjZUZpbGU6IHRzLlNvdXJjZUZpbGUpOiBib29sZWFuIHtcbiAgcmV0dXJuIHRzLmZvckVhY2hUcmFpbGluZ0NvbW1lbnRSYW5nZShzb3VyY2VGaWxlLnRleHQsIG5vZGUuZ2V0RW5kKCksIChwb3MsIGVuZCwga2luZCkgPT4ge1xuICAgIGlmIChraW5kICE9PSB0cy5TeW50YXhLaW5kLk11bHRpTGluZUNvbW1lbnRUcml2aWEpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cbiAgICBjb25zdCBjb21tZW50VGV4dCA9IHNvdXJjZUZpbGUudGV4dC5zdWJzdHJpbmcocG9zICsgMiwgZW5kIC0gMik7XG4gICAgcmV0dXJuIGNvbW1lbnRUZXh0ID09PSBJR05PUkVfTUFSS0VSO1xuICB9KSA9PT0gdHJ1ZTtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGlzVGVtcGxhdGVEaWFnbm9zdGljKGRpYWdub3N0aWM6IHRzLkRpYWdub3N0aWMpOiBkaWFnbm9zdGljIGlzIFRlbXBsYXRlRGlhZ25vc3RpYyB7XG4gIHJldHVybiBkaWFnbm9zdGljLmhhc093blByb3BlcnR5KCdjb21wb25lbnRGaWxlJykgJiZcbiAgICAgIHRzLmlzU291cmNlRmlsZSgoZGlhZ25vc3RpYyBhcyBhbnkpLmNvbXBvbmVudEZpbGUpO1xufVxuIl19