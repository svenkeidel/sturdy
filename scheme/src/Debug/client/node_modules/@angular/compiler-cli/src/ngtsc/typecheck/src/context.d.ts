/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/// <amd-module name="@angular/compiler-cli/src/ngtsc/typecheck/src/context" />
import { BoundTarget, ParseSourceFile, SchemaMetadata } from '@angular/compiler';
import * as ts from 'typescript';
import { AbsoluteFsPath } from '../../file_system';
import { Reference, ReferenceEmitter } from '../../imports';
import { ClassDeclaration, ReflectionHost } from '../../reflection';
import { TemplateSourceMapping, TypeCheckableDirectiveMeta, TypeCheckingConfig, TypeCtorMetadata } from './api';
import { TemplateSourceResolver } from './diagnostics';
import { DomSchemaChecker } from './dom';
import { OutOfBandDiagnosticRecorder } from './oob';
import { TemplateSourceManager } from './source';
import { TypeCheckFile } from './type_check_file';
/**
 * Complete type-checking code generated for the user's program, ready for input into the
 * type-checking engine.
 */
export interface TypeCheckRequest {
    /**
     * Map of source filenames to new contents for those files.
     *
     * This includes both contents of type-checking shim files, as well as changes to any user files
     * which needed to be made to support template type-checking.
     */
    updates: Map<AbsoluteFsPath, string>;
    /**
     * Map containing additional data for each type-checking shim that is required to support
     * generation of diagnostics.
     */
    perFileData: Map<AbsoluteFsPath, FileTypeCheckingData>;
}
/**
 * Data for a type-checking shim which is required to support generation of diagnostics.
 */
export interface FileTypeCheckingData {
    /**
     * Whether the type-checking shim required any inline changes to the original file, which affects
     * whether the shim can be reused.
     */
    hasInlines: boolean;
    /**
     * Source mapping information for mapping diagnostics back to the original template.
     */
    sourceResolver: TemplateSourceResolver;
    /**
     * Any `ts.Diagnostic`s which were produced during the generation of this shim.
     *
     * Some diagnostics are produced during creation time and are tracked here.
     */
    genesisDiagnostics: ts.Diagnostic[];
    /**
     * Path to the shim file.
     */
    typeCheckFile: AbsoluteFsPath;
}
/**
 * Data for a type-checking shim which is still having its code generated.
 */
export interface PendingFileTypeCheckingData {
    /**
     * Whether any inline code has been required by the shim yet.
     */
    hasInlines: boolean;
    /**
     * `TemplateSourceManager` being used to track source mapping information for this shim.
     */
    sourceManager: TemplateSourceManager;
    /**
     * Recorder for out-of-band diagnostics which are raised during generation.
     */
    oobRecorder: OutOfBandDiagnosticRecorder;
    /**
     * The `DomSchemaChecker` in use for this template, which records any schema-related diagnostics.
     */
    domSchemaChecker: DomSchemaChecker;
    /**
     * Path to the shim file.
     */
    typeCheckFile: TypeCheckFile;
}
/**
 * A template type checking context for a program.
 *
 * The `TypeCheckContext` allows registration of components and their templates which need to be
 * type checked.
 */
export declare class TypeCheckContext {
    private config;
    private compilerHost;
    private refEmitter;
    private reflector;
    private fileMap;
    constructor(config: TypeCheckingConfig, compilerHost: Pick<ts.CompilerHost, 'getCanonicalFileName'>, refEmitter: ReferenceEmitter, reflector: ReflectionHost);
    /**
     * A `Map` of `ts.SourceFile`s that the context has seen to the operations (additions of methods
     * or type-check blocks) that need to be eventually performed on that file.
     */
    private opMap;
    /**
     * Tracks when an a particular class has a pending type constructor patching operation already
     * queued.
     */
    private typeCtorPending;
    /**
     * Map of data for file paths which was adopted from a prior compilation.
     *
     * This data allows the `TypeCheckContext` to generate a `TypeCheckRequest` which can interpret
     * diagnostics from type-checking shims included in the prior compilation.
     */
    private adoptedFiles;
    /**
     * Record the `FileTypeCheckingData` from a previous program that's associated with a particular
     * source file.
     */
    adoptPriorResults(sf: ts.SourceFile, data: FileTypeCheckingData): void;
    /**
     * Record a template for the given component `node`, with a `SelectorMatcher` for directive
     * matching.
     *
     * @param node class of the node being recorded.
     * @param template AST nodes of the template being recorded.
     * @param matcher `SelectorMatcher` which tracks directives that are in scope for this template.
     */
    addTemplate(ref: Reference<ClassDeclaration<ts.ClassDeclaration>>, boundTarget: BoundTarget<TypeCheckableDirectiveMeta>, pipes: Map<string, Reference<ClassDeclaration<ts.ClassDeclaration>>>, schemas: SchemaMetadata[], sourceMapping: TemplateSourceMapping, file: ParseSourceFile): void;
    /**
     * Record a type constructor for the given `node` with the given `ctorMetadata`.
     */
    addInlineTypeCtor(fileData: PendingFileTypeCheckingData, sf: ts.SourceFile, ref: Reference<ClassDeclaration<ts.ClassDeclaration>>, ctorMeta: TypeCtorMetadata): void;
    /**
     * Transform a `ts.SourceFile` into a version that includes type checking code.
     *
     * If this particular `ts.SourceFile` requires changes, the text representing its new contents
     * will be returned. Otherwise, a `null` return indicates no changes were necessary.
     */
    transform(sf: ts.SourceFile): string | null;
    finalize(): TypeCheckRequest;
    private addInlineTypeCheckBlock;
    private dataForFile;
}
