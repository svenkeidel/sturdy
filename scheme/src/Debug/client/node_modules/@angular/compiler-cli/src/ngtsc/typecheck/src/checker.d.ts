/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/// <amd-module name="@angular/compiler-cli/src/ngtsc/typecheck/src/checker" />
import * as ts from 'typescript';
import { ReferenceEmitter } from '../../imports';
import { IncrementalBuild } from '../../incremental/api';
import { ReflectionHost } from '../../reflection';
import { TypeCheckingConfig, TypeCheckingProgramStrategy } from './api';
import { FileTypeCheckingData, TypeCheckContext, TypeCheckRequest } from './context';
/**
 * Interface to trigger generation of type-checking code for a program given a new
 * `TypeCheckContext`.
 */
export interface ProgramTypeCheckAdapter {
    typeCheck(sf: ts.SourceFile, ctx: TypeCheckContext): void;
}
/**
 * Primary template type-checking engine, which performs type-checking using a
 * `TypeCheckingProgramStrategy` for type-checking program maintenance, and the
 * `ProgramTypeCheckAdapter` for generation of template type-checking code.
 */
export declare class TemplateTypeChecker {
    private originalProgram;
    private typeCheckingStrategy;
    private typeCheckAdapter;
    private config;
    private refEmitter;
    private reflector;
    private compilerHost;
    private priorBuild;
    private files;
    constructor(originalProgram: ts.Program, typeCheckingStrategy: TypeCheckingProgramStrategy, typeCheckAdapter: ProgramTypeCheckAdapter, config: TypeCheckingConfig, refEmitter: ReferenceEmitter, reflector: ReflectionHost, compilerHost: Pick<ts.CompilerHost, 'getCanonicalFileName'>, priorBuild: IncrementalBuild<unknown, FileTypeCheckingData>);
    /**
     * Reset the internal type-checking program by generating type-checking code from the user's
     * program.
     */
    refresh(): TypeCheckRequest;
    /**
     * Retrieve type-checking diagnostics from the given `ts.SourceFile` using the most recent
     * type-checking program.
     */
    getDiagnosticsForFile(sf: ts.SourceFile): ts.Diagnostic[];
}
