"use strict";
/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.HammerGesturesMigration = void 0;
const core_1 = require("@angular-devkit/core");
const schematics_1 = require("@angular/cdk/schematics");
const change_1 = require("@schematics/angular/utility/change");
const fs_1 = require("fs");
const path_1 = require("path");
const ts = require("typescript");
const find_hammer_script_tags_1 = require("./find-hammer-script-tags");
const find_main_module_1 = require("./find-main-module");
const hammer_template_check_1 = require("./hammer-template-check");
const import_manager_1 = require("./import-manager");
const remove_array_element_1 = require("./remove-array-element");
const remove_element_from_html_1 = require("./remove-element-from-html");
const GESTURE_CONFIG_CLASS_NAME = 'GestureConfig';
const GESTURE_CONFIG_FILE_NAME = 'gesture-config';
const GESTURE_CONFIG_TEMPLATE_PATH = './gesture-config.template';
const HAMMER_CONFIG_TOKEN_NAME = 'HAMMER_GESTURE_CONFIG';
const HAMMER_CONFIG_TOKEN_MODULE = '@angular/platform-browser';
const HAMMER_MODULE_NAME = 'HammerModule';
const HAMMER_MODULE_IMPORT = '@angular/platform-browser';
const HAMMER_MODULE_SPECIFIER = 'hammerjs';
const CANNOT_REMOVE_REFERENCE_ERROR = `Cannot remove reference to "GestureConfig". Please remove manually.`;
let HammerGesturesMigration = /** @class */ (() => {
    class HammerGesturesMigration extends schematics_1.DevkitMigration {
        constructor() {
            super(...arguments);
            // Only enable this rule if the migration targets v9 or v10 and is running for a non-test
            // target. We cannot migrate test targets since they have a limited scope
            // (in regards to source files) and therefore the HammerJS usage detection can be incorrect.
            this.enabled = (this.targetVersion === schematics_1.TargetVersion.V9 || this.targetVersion === schematics_1.TargetVersion.V10) &&
                !this.context.isTestTarget;
            this._printer = ts.createPrinter();
            this._importManager = new import_manager_1.ImportManager(this.fileSystem, this._printer);
            this._nodeFailures = [];
            /**
             * Whether custom HammerJS events provided by the Material gesture
             * config are used in a template.
             */
            this._customEventsUsedInTemplate = false;
            /** Whether standard HammerJS events are used in a template. */
            this._standardEventsUsedInTemplate = false;
            /** Whether HammerJS is accessed at runtime. */
            this._usedInRuntime = false;
            /**
             * List of imports that make "hammerjs" available globally. We keep track of these
             * since we might need to remove them if Hammer is not used.
             */
            this._installImports = [];
            /**
             * List of identifiers which resolve to the gesture config from Angular Material.
             */
            this._gestureConfigReferences = [];
            /**
             * List of identifiers which resolve to the "HAMMER_GESTURE_CONFIG" token from
             * "@angular/platform-browser".
             */
            this._hammerConfigTokenReferences = [];
            /**
             * List of identifiers which resolve to the "HammerModule" from
             * "@angular/platform-browser".
             */
            this._hammerModuleReferences = [];
            /**
             * List of identifiers that have been deleted from source files. This can be
             * used to determine if certain imports are still used or not.
             */
            this._deletedIdentifiers = [];
        }
        visitTemplate(template) {
            if (!this._customEventsUsedInTemplate || !this._standardEventsUsedInTemplate) {
                const { standardEvents, customEvents } = hammer_template_check_1.isHammerJsUsedInTemplate(template.content);
                this._customEventsUsedInTemplate = this._customEventsUsedInTemplate || customEvents;
                this._standardEventsUsedInTemplate = this._standardEventsUsedInTemplate || standardEvents;
            }
        }
        visitNode(node) {
            this._checkHammerImports(node);
            this._checkForRuntimeHammerUsage(node);
            this._checkForMaterialGestureConfig(node);
            this._checkForHammerGestureConfigToken(node);
            this._checkForHammerModuleReference(node);
        }
        postAnalysis() {
            // Walk through all hammer config token references and check if there
            // is a potential custom gesture config setup.
            const hasCustomGestureConfigSetup = this._hammerConfigTokenReferences.some(r => this._checkForCustomGestureConfigSetup(r));
            const usedInTemplate = this._standardEventsUsedInTemplate || this._customEventsUsedInTemplate;
            /*
              Possible scenarios and how the migration should change the project:
                1. We detect that a custom HammerJS gesture config is set up:
                    - Remove references to the Material gesture config if no HammerJS event is used.
                    - Print a warning about ambiguous configuration that cannot be handled completely
                      if there are references to the Material gesture config.
                2. We detect that HammerJS is only used programmatically:
                    - Remove references to GestureConfig of Material.
                    - Remove references to the "HammerModule" if present.
                3. We detect that standard HammerJS events are used in a template:
                    - Set up the "HammerModule" from platform-browser.
                    - Remove all gesture config references.
                4. We detect that custom HammerJS events provided by the Material gesture
                   config are used.
                    - Copy the Material gesture config into the app.
                    - Rewrite all gesture config references to the newly copied one.
                    - Set up the new gesture config in the root app module.
                    - Set up the "HammerModule" from platform-browser.
                4. We detect no HammerJS usage at all:
                    - Remove Hammer imports
                    - Remove Material gesture config references
                    - Remove HammerModule setup if present.
                    - Remove Hammer script imports in "index.html" files.
            */
            if (hasCustomGestureConfigSetup) {
                // If a custom gesture config is provided, we always assume that HammerJS is used.
                HammerGesturesMigration.globalUsesHammer = true;
                if (!usedInTemplate && this._gestureConfigReferences.length) {
                    // If the Angular Material gesture events are not used and we found a custom
                    // gesture config, we can safely remove references to the Material gesture config
                    // since events provided by the Material gesture config are guaranteed to be unused.
                    this._removeMaterialGestureConfigSetup();
                    this.printInfo('The HammerJS v9 migration for Angular Components detected that HammerJS is ' +
                        'manually set up in combination with references to the Angular Material gesture ' +
                        'config. This target cannot be migrated completely, but all references to the ' +
                        'deprecated Angular Material gesture have been removed. Read more here: ' +
                        'https://git.io/ng-material-v9-hammer-ambiguous-usage');
                }
                else if (usedInTemplate && this._gestureConfigReferences.length) {
                    // Since there is a reference to the Angular Material gesture config, and we detected
                    // usage of a gesture event that could be provided by Angular Material, we *cannot*
                    // automatically remove references. This is because we do *not* know whether the
                    // event is actually provided by the custom config or by the Material config.
                    this.printInfo('The HammerJS v9 migration for Angular Components detected that HammerJS is ' +
                        'manually set up in combination with references to the Angular Material gesture ' +
                        'config. This target cannot be migrated completely. Please manually remove ' +
                        'references to the deprecated Angular Material gesture config. Read more here: ' +
                        'https://git.io/ng-material-v9-hammer-ambiguous-usage');
                }
            }
            else if (this._usedInRuntime || usedInTemplate) {
                // We keep track of whether Hammer is used globally. This is necessary because we
                // want to only remove Hammer from the "package.json" if it is not used in any project
                // target. Just because it isn't used in one target doesn't mean that we can safely
                // remove the dependency.
                HammerGesturesMigration.globalUsesHammer = true;
                // If hammer is only used at runtime, we don't need the gesture config or "HammerModule"
                // and can remove it (along with the hammer config token import if no longer needed).
                if (!usedInTemplate) {
                    this._removeMaterialGestureConfigSetup();
                    this._removeHammerModuleReferences();
                }
                else if (this._standardEventsUsedInTemplate && !this._customEventsUsedInTemplate) {
                    this._setupHammerWithStandardEvents();
                }
                else {
                    this._setupHammerWithCustomEvents();
                }
            }
            else {
                this._removeHammerSetup();
            }
            // Record the changes collected in the import manager. Changes need to be applied
            // once the import manager registered all import modifications. This avoids collisions.
            this._importManager.recordChanges();
            // Create migration failures that will be printed by the update-tool on migration
            // completion. We need special logic for updating failure positions to reflect
            // the new source file after modifications from the import manager.
            this.failures.push(...this._createMigrationFailures());
            // The template check for HammerJS events is not completely reliable as the event
            // output could also be from a component having an output named similarly to a known
            // hammerjs event (e.g. "@Output() slide"). The usage is therefore somewhat ambiguous
            // and we want to print a message that developers might be able to remove Hammer manually.
            if (!hasCustomGestureConfigSetup && !this._usedInRuntime && usedInTemplate) {
                this.printInfo('The HammerJS v9 migration for Angular Components migrated the ' +
                    'project to keep HammerJS installed, but detected ambiguous usage of HammerJS. Please ' +
                    'manually check if you can remove HammerJS from your application. More details: ' +
                    'https://git.io/ng-material-v9-hammer-ambiguous-usage');
            }
        }
        /**
         * Sets up the hammer gesture config in the current project. To achieve this, the
         * following steps are performed:
         *   1) Create copy of Angular Material gesture config.
         *   2) Rewrite all references to the Angular Material gesture config to the
         *      new gesture config.
         *   3) Setup the HAMMER_GESTURE_CONFIG in the root app module (if not done already).
         *   4) Setup the "HammerModule" in the root app module (if not done already).
         */
        _setupHammerWithCustomEvents() {
            const project = this.context.project;
            const sourceRoot = core_1.normalize(project.sourceRoot || project.root);
            const newConfigPath = core_1.join(sourceRoot, this._getAvailableGestureConfigFileName(sourceRoot));
            // Copy gesture config template into the CLI project.
            this.fileSystem.create(newConfigPath, fs_1.readFileSync(require.resolve(GESTURE_CONFIG_TEMPLATE_PATH), 'utf8'));
            // Replace all Material gesture config references to resolve to the
            // newly copied gesture config.
            this._gestureConfigReferences.forEach(i => this._replaceGestureConfigReference(i, GESTURE_CONFIG_CLASS_NAME, getModuleSpecifier(newConfigPath, i.node.getSourceFile().fileName)));
            // Setup the gesture config provider and the "HammerModule" in the root module
            // if not done already. The "HammerModule" is needed in v9 since it enables the
            // Hammer event plugin that was previously enabled by default in v8.
            this._setupNewGestureConfigInRootModule(newConfigPath);
            this._setupHammerModuleInRootModule();
        }
        /**
         * Sets up the standard hammer module in the project and removes all
         * references to the deprecated Angular Material gesture config.
         */
        _setupHammerWithStandardEvents() {
            // Setup the HammerModule. The HammerModule enables support for
            // the standard HammerJS events.
            this._setupHammerModuleInRootModule();
            this._removeMaterialGestureConfigSetup();
        }
        /**
         * Removes Hammer from the current project. The following steps are performed:
         *   1) Delete all TypeScript imports to "hammerjs".
         *   2) Remove references to the Angular Material gesture config.
         *   3) Remove "hammerjs" from all index HTML files of the current project.
         */
        _removeHammerSetup() {
            this._installImports.forEach(i => this._importManager.deleteImportByDeclaration(i));
            this._removeMaterialGestureConfigSetup();
            this._removeHammerModuleReferences();
            this._removeHammerFromIndexFile();
        }
        /**
         * Removes the gesture config setup by deleting all found references to the Angular
         * Material gesture config. Additionally, unused imports to the hammer gesture config
         * token from "@angular/platform-browser" will be removed as well.
         */
        _removeMaterialGestureConfigSetup() {
            this._gestureConfigReferences.forEach(r => this._removeGestureConfigReference(r));
            this._hammerConfigTokenReferences.forEach(r => {
                if (r.isImport) {
                    this._removeHammerConfigTokenImportIfUnused(r);
                }
            });
        }
        /** Removes all references to the "HammerModule" from "@angular/platform-browser". */
        _removeHammerModuleReferences() {
            this._hammerModuleReferences.forEach(({ node, isImport, importData }) => {
                const sourceFile = node.getSourceFile();
                const recorder = this.fileSystem.edit(this.fileSystem.resolve(sourceFile.fileName));
                // Only remove the import for the HammerModule if the module has been accessed
                // through a non-namespaced identifier access.
                if (!isNamespacedIdentifierAccess(node)) {
                    this._importManager.deleteNamedBindingImport(sourceFile, HAMMER_MODULE_NAME, importData.moduleName);
                }
                // For references from within an import, we do not need to do anything other than
                // removing the import. For other references, we remove the import and the actual
                // identifier in the module imports.
                if (isImport) {
                    return;
                }
                // If the "HammerModule" is referenced within an array literal, we can
                // remove the element easily. Otherwise if it's outside of an array literal,
                // we need to replace the reference with an empty object literal w/ todo to
                // not break the application.
                if (ts.isArrayLiteralExpression(node.parent)) {
                    // Removes the "HammerModule" from the parent array expression. Removes
                    // the trailing comma token if present.
                    remove_array_element_1.removeElementFromArrayExpression(node, recorder);
                }
                else {
                    recorder.remove(node.getStart(), node.getWidth());
                    recorder.insertRight(node.getStart(), `/* TODO: remove */ {}`);
                    this._nodeFailures.push({
                        node: node,
                        message: 'Unable to delete reference to "HammerModule".',
                    });
                }
            });
        }
        /**
         * Checks if the given node is a reference to the hammer gesture config
         * token from platform-browser. If so, keeps track of the reference.
         */
        _checkForHammerGestureConfigToken(node) {
            if (ts.isIdentifier(node)) {
                const importData = schematics_1.getImportOfIdentifier(node, this.typeChecker);
                if (importData && importData.symbolName === HAMMER_CONFIG_TOKEN_NAME &&
                    importData.moduleName === HAMMER_CONFIG_TOKEN_MODULE) {
                    this._hammerConfigTokenReferences.push({ node, importData, isImport: ts.isImportSpecifier(node.parent) });
                }
            }
        }
        /**
         * Checks if the given node is a reference to the HammerModule from
         * "@angular/platform-browser". If so, keeps track of the reference.
         */
        _checkForHammerModuleReference(node) {
            if (ts.isIdentifier(node)) {
                const importData = schematics_1.getImportOfIdentifier(node, this.typeChecker);
                if (importData && importData.symbolName === HAMMER_MODULE_NAME &&
                    importData.moduleName === HAMMER_MODULE_IMPORT) {
                    this._hammerModuleReferences.push({ node, importData, isImport: ts.isImportSpecifier(node.parent) });
                }
            }
        }
        /**
         * Checks if the given node is an import to the HammerJS package. Imports to
         * HammerJS which load specific symbols from the package are considered as
         * runtime usage of Hammer. e.g. `import {Symbol} from "hammerjs";`.
         */
        _checkHammerImports(node) {
            if (ts.isImportDeclaration(node) && ts.isStringLiteral(node.moduleSpecifier) &&
                node.moduleSpecifier.text === HAMMER_MODULE_SPECIFIER) {
                // If there is an import to HammerJS that imports symbols, or is namespaced
                // (e.g. "import {A, B} from ..." or "import * as hammer from ..."), then we
                // assume that some exports are used at runtime.
                if (node.importClause &&
                    !(node.importClause.namedBindings && ts.isNamedImports(node.importClause.namedBindings) &&
                        node.importClause.namedBindings.elements.length === 0)) {
                    this._usedInRuntime = true;
                }
                else {
                    this._installImports.push(node);
                }
            }
        }
        /**
         * Checks if the given node accesses the global "Hammer" symbol at runtime. If so,
         * the migration rule state will be updated to reflect that Hammer is used at runtime.
         */
        _checkForRuntimeHammerUsage(node) {
            if (this._usedInRuntime) {
                return;
            }
            // Detects usages of "window.Hammer".
            if (ts.isPropertyAccessExpression(node) && node.name.text === 'Hammer') {
                const originExpr = unwrapExpression(node.expression);
                if (ts.isIdentifier(originExpr) && originExpr.text === 'window') {
                    this._usedInRuntime = true;
                }
                return;
            }
            // Detects usages of "window['Hammer']".
            if (ts.isElementAccessExpression(node) && ts.isStringLiteral(node.argumentExpression) &&
                node.argumentExpression.text === 'Hammer') {
                const originExpr = unwrapExpression(node.expression);
                if (ts.isIdentifier(originExpr) && originExpr.text === 'window') {
                    this._usedInRuntime = true;
                }
                return;
            }
            // Handles usages of plain identifier with the name "Hammer". These usage
            // are valid if they resolve to "@types/hammerjs". e.g. "new Hammer(myElement)".
            if (ts.isIdentifier(node) && node.text === 'Hammer' &&
                !ts.isPropertyAccessExpression(node.parent) && !ts.isElementAccessExpression(node.parent)) {
                const symbol = this._getDeclarationSymbolOfNode(node);
                if (symbol && symbol.valueDeclaration &&
                    symbol.valueDeclaration.getSourceFile().fileName.includes('@types/hammerjs')) {
                    this._usedInRuntime = true;
                }
            }
        }
        /**
         * Checks if the given node references the gesture config from Angular Material.
         * If so, we keep track of the found symbol reference.
         */
        _checkForMaterialGestureConfig(node) {
            if (ts.isIdentifier(node)) {
                const importData = schematics_1.getImportOfIdentifier(node, this.typeChecker);
                if (importData && importData.symbolName === GESTURE_CONFIG_CLASS_NAME &&
                    importData.moduleName.startsWith('@angular/material/')) {
                    this._gestureConfigReferences.push({ node, importData, isImport: ts.isImportSpecifier(node.parent) });
                }
            }
        }
        /**
         * Checks if the given Hammer gesture config token reference is part of an
         * Angular provider definition that sets up a custom gesture config.
         */
        _checkForCustomGestureConfigSetup(tokenRef) {
            // Walk up the tree to look for a parent property assignment of the
            // reference to the hammer gesture config token.
            let propertyAssignment = tokenRef.node;
            while (propertyAssignment && !ts.isPropertyAssignment(propertyAssignment)) {
                propertyAssignment = propertyAssignment.parent;
            }
            if (!propertyAssignment || !ts.isPropertyAssignment(propertyAssignment) ||
                getPropertyNameText(propertyAssignment.name) !== 'provide') {
                return false;
            }
            const objectLiteralExpr = propertyAssignment.parent;
            const matchingIdentifiers = findMatchingChildNodes(objectLiteralExpr, ts.isIdentifier);
            // We naively assume that if there is a reference to the "GestureConfig" export
            // from Angular Material in the provider literal, that the provider sets up the
            // Angular Material gesture config.
            return !this._gestureConfigReferences.some(r => matchingIdentifiers.includes(r.node));
        }
        /**
         * Determines an available file name for the gesture config which should
         * be stored in the specified file path.
         */
        _getAvailableGestureConfigFileName(sourceRoot) {
            if (!this.fileSystem.exists(core_1.join(sourceRoot, `${GESTURE_CONFIG_FILE_NAME}.ts`))) {
                return `${GESTURE_CONFIG_FILE_NAME}.ts`;
            }
            let possibleName = `${GESTURE_CONFIG_FILE_NAME}-`;
            let index = 1;
            while (this.fileSystem.exists(core_1.join(sourceRoot, `${possibleName}-${index}.ts`))) {
                index++;
            }
            return `${possibleName + index}.ts`;
        }
        /** Replaces a given gesture config reference with a new import. */
        _replaceGestureConfigReference({ node, importData, isImport }, symbolName, moduleSpecifier) {
            const sourceFile = node.getSourceFile();
            const recorder = this.fileSystem.edit(this.fileSystem.resolve(sourceFile.fileName));
            // List of all identifiers referring to the gesture config in the current file. This
            // allows us to add an import for the copied gesture configuration without generating a
            // new identifier for the import to avoid collisions. i.e. "GestureConfig_1". The import
            // manager checks for possible name collisions, but is able to ignore specific identifiers.
            // We use this to ignore all references to the original Angular Material gesture config,
            // because these will be replaced and therefore will not interfere.
            const gestureIdentifiersInFile = this._getGestureConfigIdentifiersOfFile(sourceFile);
            // If the parent of the identifier is accessed through a namespace, we can just
            // import the new gesture config without rewriting the import declaration because
            // the config has been imported through a namespaced import.
            if (isNamespacedIdentifierAccess(node)) {
                const newExpression = this._importManager.addImportToSourceFile(sourceFile, symbolName, moduleSpecifier, false, gestureIdentifiersInFile);
                recorder.remove(node.parent.getStart(), node.parent.getWidth());
                recorder.insertRight(node.parent.getStart(), this._printNode(newExpression, sourceFile));
                return;
            }
            // Delete the old import to the "GestureConfig".
            this._importManager.deleteNamedBindingImport(sourceFile, GESTURE_CONFIG_CLASS_NAME, importData.moduleName);
            // If the current reference is not from inside of a import, we need to add a new
            // import to the copied gesture config and replace the identifier. For references
            // within an import, we do nothing but removing the actual import. This allows us
            // to remove unused imports to the Material gesture config.
            if (!isImport) {
                const newExpression = this._importManager.addImportToSourceFile(sourceFile, symbolName, moduleSpecifier, false, gestureIdentifiersInFile);
                recorder.remove(node.getStart(), node.getWidth());
                recorder.insertRight(node.getStart(), this._printNode(newExpression, sourceFile));
            }
        }
        /**
         * Removes a given gesture config reference and its corresponding import from
         * its containing source file. Imports will be always removed, but in some cases,
         * where it's not guaranteed that a removal can be performed safely, we just
         * create a migration failure (and add a TODO if possible).
         */
        _removeGestureConfigReference({ node, importData, isImport }) {
            const sourceFile = node.getSourceFile();
            const recorder = this.fileSystem.edit(this.fileSystem.resolve(sourceFile.fileName));
            // Only remove the import for the gesture config if the gesture config has
            // been accessed through a non-namespaced identifier access.
            if (!isNamespacedIdentifierAccess(node)) {
                this._importManager.deleteNamedBindingImport(sourceFile, GESTURE_CONFIG_CLASS_NAME, importData.moduleName);
            }
            // For references from within an import, we do not need to do anything other than
            // removing the import. For other references, we remove the import and the reference
            // identifier if used inside of a provider definition.
            if (isImport) {
                return;
            }
            const providerAssignment = node.parent;
            // Only remove references to the gesture config which are part of a statically
            // analyzable provider definition. We only support the common case of a gesture
            // config provider definition where the config is set up through "useClass".
            // Otherwise, it's not guaranteed that we can safely remove the provider definition.
            if (!ts.isPropertyAssignment(providerAssignment) ||
                getPropertyNameText(providerAssignment.name) !== 'useClass') {
                this._nodeFailures.push({ node, message: CANNOT_REMOVE_REFERENCE_ERROR });
                return;
            }
            const objectLiteralExpr = providerAssignment.parent;
            const provideToken = objectLiteralExpr.properties.find((p) => ts.isPropertyAssignment(p) && getPropertyNameText(p.name) === 'provide');
            // Do not remove the reference if the gesture config is not part of a provider definition,
            // or if the provided toke is not referring to the known HAMMER_GESTURE_CONFIG token
            // from platform-browser.
            if (!provideToken || !this._isReferenceToHammerConfigToken(provideToken.initializer)) {
                this._nodeFailures.push({ node, message: CANNOT_REMOVE_REFERENCE_ERROR });
                return;
            }
            // Collect all nested identifiers which will be deleted. This helps us
            // determining if we can remove imports for the "HAMMER_GESTURE_CONFIG" token.
            this._deletedIdentifiers.push(...findMatchingChildNodes(objectLiteralExpr, ts.isIdentifier));
            // In case the found provider definition is not part of an array literal,
            // we cannot safely remove the provider. This is because it could be declared
            // as a variable. e.g. "const gestureProvider = {provide: .., useClass: GestureConfig}".
            // In that case, we just add an empty object literal with TODO and print a failure.
            if (!ts.isArrayLiteralExpression(objectLiteralExpr.parent)) {
                recorder.remove(objectLiteralExpr.getStart(), objectLiteralExpr.getWidth());
                recorder.insertRight(objectLiteralExpr.getStart(), `/* TODO: remove */ {}`);
                this._nodeFailures.push({
                    node: objectLiteralExpr,
                    message: `Unable to delete provider definition for "GestureConfig" completely. ` +
                        `Please clean up the provider.`
                });
                return;
            }
            // Removes the object literal from the parent array expression. Removes
            // the trailing comma token if present.
            remove_array_element_1.removeElementFromArrayExpression(objectLiteralExpr, recorder);
        }
        /** Removes the given hammer config token import if it is not used. */
        _removeHammerConfigTokenImportIfUnused({ node, importData }) {
            const sourceFile = node.getSourceFile();
            const isTokenUsed = this._hammerConfigTokenReferences.some(r => !r.isImport && !isNamespacedIdentifierAccess(r.node) &&
                r.node.getSourceFile() === sourceFile && !this._deletedIdentifiers.includes(r.node));
            // We don't want to remove the import for the token if the token is
            // still used somewhere.
            if (!isTokenUsed) {
                this._importManager.deleteNamedBindingImport(sourceFile, HAMMER_CONFIG_TOKEN_NAME, importData.moduleName);
            }
        }
        /** Removes Hammer from all index HTML files of the current project. */
        _removeHammerFromIndexFile() {
            const indexFilePaths = schematics_1.getProjectIndexFiles(this.context.project);
            indexFilePaths.forEach(filePath => {
                if (!this.fileSystem.exists(filePath)) {
                    return;
                }
                const htmlContent = this.fileSystem.read(filePath);
                const recorder = this.fileSystem.edit(filePath);
                find_hammer_script_tags_1.findHammerScriptImportElements(htmlContent)
                    .forEach(el => remove_element_from_html_1.removeElementFromHtml(el, recorder));
            });
        }
        /** Sets up the Hammer gesture config in the root module if needed. */
        _setupNewGestureConfigInRootModule(gestureConfigPath) {
            const { workspaceFsPath, project } = this.context;
            const mainFilePath = schematics_1.getProjectMainFile(project);
            const rootModuleSymbol = this._getRootModuleSymbol(mainFilePath);
            if (rootModuleSymbol === null) {
                this.failures.push({
                    filePath: mainFilePath,
                    message: `Could not setup Hammer gestures in module. Please ` +
                        `manually ensure that the Hammer gesture config is set up.`,
                });
                return;
            }
            const sourceFile = rootModuleSymbol.valueDeclaration.getSourceFile();
            const relativePath = path_1.relative(workspaceFsPath, sourceFile.fileName);
            const metadata = schematics_1.getDecoratorMetadata(sourceFile, 'NgModule', '@angular/core');
            // If no "NgModule" definition is found inside the source file, we just do nothing.
            if (!metadata.length) {
                return;
            }
            const recorder = this.fileSystem.edit(this.fileSystem.resolve(sourceFile.fileName));
            const providersField = schematics_1.getMetadataField(metadata[0], 'providers')[0];
            const providerIdentifiers = providersField ? findMatchingChildNodes(providersField, ts.isIdentifier) : null;
            const gestureConfigExpr = this._importManager.addImportToSourceFile(sourceFile, GESTURE_CONFIG_CLASS_NAME, getModuleSpecifier(gestureConfigPath, sourceFile.fileName), false, this._getGestureConfigIdentifiersOfFile(sourceFile));
            const hammerConfigTokenExpr = this._importManager.addImportToSourceFile(sourceFile, HAMMER_CONFIG_TOKEN_NAME, HAMMER_CONFIG_TOKEN_MODULE);
            const newProviderNode = ts.createObjectLiteral([
                ts.createPropertyAssignment('provide', hammerConfigTokenExpr),
                ts.createPropertyAssignment('useClass', gestureConfigExpr)
            ]);
            // If the providers field exists and already contains references to the hammer gesture
            // config token and the gesture config, we naively assume that the gesture config is
            // already set up. We only want to add the gesture config provider if it is not set up.
            if (!providerIdentifiers ||
                !(this._hammerConfigTokenReferences.some(r => providerIdentifiers.includes(r.node)) &&
                    this._gestureConfigReferences.some(r => providerIdentifiers.includes(r.node)))) {
                schematics_1.addSymbolToNgModuleMetadata(sourceFile, relativePath, 'providers', this._printNode(newProviderNode, sourceFile), null)
                    .forEach(change => {
                    if (change instanceof change_1.InsertChange) {
                        recorder.insertRight(change.pos, change.toAdd);
                    }
                });
            }
        }
        /**
         * Gets the TypeScript symbol of the root module by looking for the module
         * bootstrap expression in the specified source file.
         */
        _getRootModuleSymbol(mainFilePath) {
            const mainFile = this.program.getSourceFile(path_1.resolve(this.context.workspaceFsPath, mainFilePath));
            if (!mainFile) {
                return null;
            }
            const appModuleExpr = find_main_module_1.findMainModuleExpression(mainFile);
            if (!appModuleExpr) {
                return null;
            }
            const appModuleSymbol = this._getDeclarationSymbolOfNode(unwrapExpression(appModuleExpr));
            if (!appModuleSymbol || !appModuleSymbol.valueDeclaration) {
                return null;
            }
            return appModuleSymbol;
        }
        /** Sets up the "HammerModule" in the root module of the current project. */
        _setupHammerModuleInRootModule() {
            const { workspaceFsPath, project } = this.context;
            const mainFilePath = schematics_1.getProjectMainFile(project);
            const rootModuleSymbol = this._getRootModuleSymbol(mainFilePath);
            if (rootModuleSymbol === null) {
                this.failures.push({
                    filePath: mainFilePath,
                    message: `Could not setup HammerModule. Please manually set up the "HammerModule" ` +
                        `from "@angular/platform-browser".`,
                });
                return;
            }
            const sourceFile = rootModuleSymbol.valueDeclaration.getSourceFile();
            const relativePath = path_1.relative(workspaceFsPath, sourceFile.fileName);
            const metadata = schematics_1.getDecoratorMetadata(sourceFile, 'NgModule', '@angular/core');
            if (!metadata.length) {
                return;
            }
            const importsField = schematics_1.getMetadataField(metadata[0], 'imports')[0];
            const importIdentifiers = importsField ? findMatchingChildNodes(importsField, ts.isIdentifier) : null;
            const recorder = this.fileSystem.edit(this.fileSystem.resolve(sourceFile.fileName));
            const hammerModuleExpr = this._importManager.addImportToSourceFile(sourceFile, HAMMER_MODULE_NAME, HAMMER_MODULE_IMPORT);
            // If the "HammerModule" is not already imported in the app module, we set it up
            // by adding it to the "imports" field of the app module.
            if (!importIdentifiers ||
                !this._hammerModuleReferences.some(r => importIdentifiers.includes(r.node))) {
                schematics_1.addSymbolToNgModuleMetadata(sourceFile, relativePath, 'imports', this._printNode(hammerModuleExpr, sourceFile), null)
                    .forEach(change => {
                    if (change instanceof change_1.InsertChange) {
                        recorder.insertRight(change.pos, change.toAdd);
                    }
                });
            }
        }
        /** Prints a given node within the specified source file. */
        _printNode(node, sourceFile) {
            return this._printer.printNode(ts.EmitHint.Unspecified, node, sourceFile);
        }
        /** Gets all referenced gesture config identifiers of a given source file */
        _getGestureConfigIdentifiersOfFile(sourceFile) {
            return this._gestureConfigReferences.filter(d => d.node.getSourceFile() === sourceFile)
                .map(d => d.node);
        }
        /** Gets the symbol that contains the value declaration of the specified node. */
        _getDeclarationSymbolOfNode(node) {
            const symbol = this.typeChecker.getSymbolAtLocation(node);
            // Symbols can be aliases of the declaration symbol. e.g. in named import specifiers.
            // We need to resolve the aliased symbol back to the declaration symbol.
            // tslint:disable-next-line:no-bitwise
            if (symbol && (symbol.flags & ts.SymbolFlags.Alias) !== 0) {
                return this.typeChecker.getAliasedSymbol(symbol);
            }
            return symbol;
        }
        /**
         * Checks whether the given expression resolves to a hammer gesture config
         * token reference from "@angular/platform-browser".
         */
        _isReferenceToHammerConfigToken(expr) {
            const unwrapped = unwrapExpression(expr);
            if (ts.isIdentifier(unwrapped)) {
                return this._hammerConfigTokenReferences.some(r => r.node === unwrapped);
            }
            else if (ts.isPropertyAccessExpression(unwrapped)) {
                return this._hammerConfigTokenReferences.some(r => r.node === unwrapped.name);
            }
            return false;
        }
        /**
         * Creates migration failures of the collected node failures. The returned migration
         * failures are updated to reflect the post-migration state of source files. Meaning
         * that failure positions are corrected if source file modifications shifted lines.
         */
        _createMigrationFailures() {
            return this._nodeFailures.map(({ node, message }) => {
                const sourceFile = node.getSourceFile();
                const offset = node.getStart();
                const position = ts.getLineAndCharacterOfPosition(sourceFile, node.getStart());
                return {
                    position: this._importManager.correctNodePosition(node, offset, position),
                    message: message,
                    filePath: this.fileSystem.resolve(sourceFile.fileName),
                };
            });
        }
        /**
         * Static migration rule method that will be called once all project targets
         * have been migrated individually. This method can be used to make changes based
         * on the analysis of the individual targets. For example: we only remove Hammer
         * from the "package.json" if it is not used in *any* project target.
         */
        static globalPostMigration(tree, context) {
            // Always notify the developer that the Hammer v9 migration does not migrate tests.
            context.logger.info('\n⚠  General notice: The HammerJS v9 migration for Angular Components is not able to ' +
                'migrate tests. Please manually clean up tests in your project if they rely on ' +
                (this.globalUsesHammer ? 'the deprecated Angular Material gesture config.' : 'HammerJS.'));
            context.logger.info('Read more about migrating tests: https://git.io/ng-material-v9-hammer-migrate-tests');
            if (!this.globalUsesHammer && this._removeHammerFromPackageJson(tree)) {
                // Since Hammer has been removed from the workspace "package.json" file,
                // we schedule a node package install task to refresh the lock file.
                return { runPackageManager: true };
            }
            // Clean global state once the workspace has been migrated. This is technically
            // not necessary in "ng update", but in tests we re-use the same rule class.
            this.globalUsesHammer = false;
        }
        /**
         * Removes the hammer package from the workspace "package.json".
         * @returns Whether Hammer was set up and has been removed from the "package.json"
         */
        static _removeHammerFromPackageJson(tree) {
            if (!tree.exists('/package.json')) {
                return false;
            }
            const packageJson = JSON.parse(tree.read('/package.json').toString('utf8'));
            // We do not handle the case where someone manually added "hammerjs" to the dev dependencies.
            if (packageJson.dependencies && packageJson.dependencies[HAMMER_MODULE_SPECIFIER]) {
                delete packageJson.dependencies[HAMMER_MODULE_SPECIFIER];
                tree.overwrite('/package.json', JSON.stringify(packageJson, null, 2));
                return true;
            }
            return false;
        }
    }
    /** Global state of whether Hammer is used in any analyzed project target. */
    HammerGesturesMigration.globalUsesHammer = false;
    return HammerGesturesMigration;
})();
exports.HammerGesturesMigration = HammerGesturesMigration;
/**
 * Recursively unwraps a given expression if it is wrapped
 * by parenthesis, type casts or type assertions.
 */
function unwrapExpression(node) {
    if (ts.isParenthesizedExpression(node)) {
        return unwrapExpression(node.expression);
    }
    else if (ts.isAsExpression(node)) {
        return unwrapExpression(node.expression);
    }
    else if (ts.isTypeAssertion(node)) {
        return unwrapExpression(node.expression);
    }
    return node;
}
/**
 * Converts the specified path to a valid TypeScript module specifier which is
 * relative to the given containing file.
 */
function getModuleSpecifier(newPath, containingFile) {
    let result = path_1.relative(path_1.dirname(containingFile), newPath).replace(/\\/g, '/').replace(/\.ts$/, '');
    if (!result.startsWith('.')) {
        result = `./${result}`;
    }
    return result;
}
/**
 * Gets the text of the given property name.
 * @returns Text of the given property name. Null if not statically analyzable.
 */
function getPropertyNameText(node) {
    if (ts.isIdentifier(node) || ts.isStringLiteralLike(node)) {
        return node.text;
    }
    return null;
}
/** Checks whether the given identifier is part of a namespaced access. */
function isNamespacedIdentifierAccess(node) {
    return ts.isQualifiedName(node.parent) || ts.isPropertyAccessExpression(node.parent);
}
/**
 * Walks through the specified node and returns all child nodes which match the
 * given predicate.
 */
function findMatchingChildNodes(parent, predicate) {
    const result = [];
    const visitNode = (node) => {
        if (predicate(node)) {
            result.push(node);
        }
        ts.forEachChild(node, visitNode);
    };
    ts.forEachChild(parent, visitNode);
    return result;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaGFtbWVyLWdlc3R1cmVzLW1pZ3JhdGlvbi5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9zY2hlbWF0aWNzL25nLXVwZGF0ZS9taWdyYXRpb25zL2hhbW1lci1nZXN0dXJlcy12OS9oYW1tZXItZ2VzdHVyZXMtbWlncmF0aW9uLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQTs7Ozs7O0dBTUc7OztBQUVILCtDQUs4QjtBQUU5Qix3REFhaUM7QUFDakMsK0RBQWdFO0FBQ2hFLDJCQUFnQztBQUNoQywrQkFBZ0Q7QUFDaEQsaUNBQWlDO0FBRWpDLHVFQUF5RTtBQUN6RSx5REFBNEQ7QUFDNUQsbUVBQWlFO0FBQ2pFLHFEQUErQztBQUMvQyxpRUFBd0U7QUFDeEUseUVBQWlFO0FBRWpFLE1BQU0seUJBQXlCLEdBQUcsZUFBZSxDQUFDO0FBQ2xELE1BQU0sd0JBQXdCLEdBQUcsZ0JBQWdCLENBQUM7QUFDbEQsTUFBTSw0QkFBNEIsR0FBRywyQkFBMkIsQ0FBQztBQUVqRSxNQUFNLHdCQUF3QixHQUFHLHVCQUF1QixDQUFDO0FBQ3pELE1BQU0sMEJBQTBCLEdBQUcsMkJBQTJCLENBQUM7QUFFL0QsTUFBTSxrQkFBa0IsR0FBRyxjQUFjLENBQUM7QUFDMUMsTUFBTSxvQkFBb0IsR0FBRywyQkFBMkIsQ0FBQztBQUV6RCxNQUFNLHVCQUF1QixHQUFHLFVBQVUsQ0FBQztBQUUzQyxNQUFNLDZCQUE2QixHQUMvQixxRUFBcUUsQ0FBQztBQVExRTtJQUFBLE1BQWEsdUJBQXdCLFNBQVEsNEJBQXFCO1FBQWxFOztZQUNFLHlGQUF5RjtZQUN6Rix5RUFBeUU7WUFDekUsNEZBQTRGO1lBQzVGLFlBQU8sR0FDSCxDQUFDLElBQUksQ0FBQyxhQUFhLEtBQUssMEJBQWEsQ0FBQyxFQUFFLElBQUksSUFBSSxDQUFDLGFBQWEsS0FBSywwQkFBYSxDQUFDLEdBQUcsQ0FBQztnQkFDckYsQ0FB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QUMsSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLEVBQUUsQ0FBQywwQkFBMEIsQ0FBQyxJQUFJLENBQUMsTUFBTSxDQUFDLENBQUM7QUFDdkYsQ0FBQztBQUVEOzs7R0FHRztBQUNILFNBQVMsc0JBQXNCLENBQzNCLE1BQWUsRUFBRSxTQUF1QztJQUMxRCxNQUFNLE1BQU0sR0FBUSxFQUFFLENBQUM7SUFDdkIsTUFBTSxTQUFTLEdBQUcsQ0FBQyxJQUFhLEVBQUUsRUFBRTtRQUNsQyxJQUFJLFNBQVMsQ0FBQyxJQUFJLENBQUMsRUFBRTtZQUNuQixNQUFNLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO1NBQ25CO1FBQ0QsRUFBRSxDQUFDLFlBQVksQ0FBQyxJQUFJLEVBQUUsU0FBUyxDQUFDLENBQUM7SUFDbkMsQ0FBQyxDQUFDO0lBQ0YsRUFBRSxDQUFDLFlBQVksQ0FBQyxNQUFNLEVBQUUsU0FBUyxDQUFDLENBQUM7SUFDbkMsT0FBTyxNQUFNLENBQUM7QUFDaEIsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge1xuICBqb2luIGFzIGRldmtpdEpvaW4sXG4gIG5vcm1hbGl6ZSBhcyBkZXZraXROb3JtYWxpemUsXG4gIFBhdGgsXG4gIFBhdGggYXMgRGV2a2l0UGF0aFxufSBmcm9tICdAYW5ndWxhci1kZXZraXQvY29yZSc7XG5pbXBvcnQge1NjaGVtYXRpY0NvbnRleHQsIFRyZWV9IGZyb20gJ0Bhbmd1bGFyLWRldmtpdC9zY2hlbWF0aWNzJztcbmltcG9ydCB7XG4gIGFkZFN5bWJvbFRvTmdNb2R1bGVNZXRhZGF0YSxcbiAgRGV2a2l0TWlncmF0aW9uLFxuICBnZXREZWNvcmF0b3JNZXRhZGF0YSxcbiAgZ2V0SW1wb3J0T2ZJZGVudGlmaWVyLFxuICBnZXRNZXRhZGF0YUZpZWxkLFxuICBnZXRQcm9qZWN0SW5kZXhGaWxlcyxcbiAgZ2V0UHJvamVjdE1haW5GaWxlLFxuICBJbXBvcnQsXG4gIE1pZ3JhdGlvbkZhaWx1cmUsXG4gIFBvc3RNaWdyYXRpb25BY3Rpb24sXG4gIFJlc29sdmVkUmVzb3VyY2UsXG4gIFRhcmdldFZlcnNpb25cbn0gZnJvbSAnQGFuZ3VsYXIvY2RrL3NjaGVtYXRpY3MnO1xuaW1wb3J0IHtJbnNlcnRDaGFuZ2V9IGZyb20gJ0BzY2hlbWF0aWNzL2FuZ3VsYXIvdXRpbGl0eS9jaGFuZ2UnO1xuaW1wb3J0IHtyZWFkRmlsZVN5bmN9IGZyb20gJ2ZzJztcbmltcG9ydCB7ZGlybmFtZSwgcmVsYXRpdmUsIHJlc29sdmV9IGZyb20gJ3BhdGgnO1xuaW1wb3J0ICogYXMgdHMgZnJvbSAndHlwZXNjcmlwdCc7XG5cbmltcG9ydCB7ZmluZEhhbW1lclNjcmlwdEltcG9ydEVsZW1lbnRzfSBmcm9tICcuL2ZpbmQtaGFtbWVyLXNjcmlwdC10YWdzJztcbmltcG9ydCB7ZmluZE1haW5Nb2R1bGVFeHByZXNzaW9ufSBmcm9tICcuL2ZpbmQtbWFpbi1tb2R1bGUnO1xuaW1wb3J0IHtpc0hhbW1lckpzVXNlZEluVGVtcGxhdGV9IGZyb20gJy4vaGFtbWVyLXRlbXBsYXRlLWNoZWNrJztcbmltcG9ydCB7SW1wb3J0TWFuYWdlcn0gZnJvbSAnLi9pbXBvcnQtbWFuYWdlcic7XG5pbXBvcnQge3JlbW92ZUVsZW1lbnRGcm9tQXJyYXlFeHByZXNzaW9ufSBmcm9tICcuL3JlbW92ZS1hcnJheS1lbGVtZW50JztcbmltcG9ydCB7cmVtb3ZlRWxlbWVudEZyb21IdG1sfSBmcm9tICcuL3JlbW92ZS1lbGVtZW50LWZyb20taHRtbCc7XG5cbmNvbnN0IEdFU1RVUkVfQ09ORklHX0NMQVNTX05BTUUgPSAnR2VzdHVyZUNvbmZpZyc7XG5jb25zdCBHRVNUVVJFX0NPTkZJR19GSUxFX05BTUUgPSAnZ2VzdHVyZS1jb25maWcnO1xuY29uc3QgR0VTVFVSRV9DT05GSUdfVEVNUExBVEVfUEFUSCA9ICcuL2dlc3R1cmUtY29uZmlnLnRlbXBsYXRlJztcblxuY29uc3QgSEFNTUVSX0NPTkZJR19UT0tFTl9OQU1FID0gJ0hBTU1FUl9HRVNUVVJFX0NPTkZJRyc7XG5jb25zdCBIQU1NRVJfQ09ORklHX1RPS0VOX01PRFVMRSA9ICdAYW5ndWxhci9wbGF0Zm9ybS1icm93c2VyJztcblxuY29uc3QgSEFNTUVSX01PRFVMRV9OQU1FID0gJ0hhbW1lck1vZHVsZSc7XG5jb25zdCBIQU1NRVJfTU9EVUxFX0lNUE9SVCA9ICdAYW5ndWxhci9wbGF0Zm9ybS1icm93c2VyJztcblxuY29uc3QgSEFNTUVSX01PRFVMRV9TUEVDSUZJRVIgPSAnaGFtbWVyanMnO1xuXG5jb25zdCBDQU5OT1RfUkVNT1ZFX1JFRkVSRU5DRV9FUlJPUiA9XG4gICAgYENhbm5vdCByZW1vdmUgcmVmZXJlbmNlIHRvIFwiR2VzdHVyZUNvbmZpZ1wiLiBQbGVhc2UgcmVtb3ZlIG1hbnVhbGx5LmA7XG5cbmludGVyZmFjZSBJZGVudGlmaWVyUmVmZXJlbmNlIHtcbiAgbm9kZTogdHMuSWRlbnRpZmllcjtcbiAgaW1wb3J0RGF0YTogSW1wb3J0O1xuICBpc0ltcG9ydDogYm9vbGVhbjtcbn1cblxuZXhwb3J0IGNsYXNzIEhhbW1lckdlc3R1cmVzTWlncmF0aW9uIGV4dGVuZHMgRGV2a2l0TWlncmF0aW9uPG51bGw+IHtcbiAgLy8gT25seSBlbmFibGUgdGhpcyBydWxlIGlmIHRoZSBtaWdyYXRpb24gdGFyZ2V0cyB2OSBvciB2MTAgYW5kIGlzIHJ1bm5pbmcgZm9yIGEgbm9uLXRlc3RcbiAgLy8gdGFyZ2V0LiBXZSBjYW5ub3QgbWlncmF0ZSB0ZXN0IHRhcmdldHMgc2luY2UgdGhleSBoYXZlIGEgbGltaXRlZCBzY29wZVxuICAvLyAoaW4gcmVnYXJkcyB0byBzb3VyY2UgZmlsZXMpIGFuZCB0aGVyZWZvcmUgdGhlIEhhbW1lckpTIHVzYWdlIGRldGVjdGlvbiBjYW4gYmUgaW5jb3JyZWN0LlxuICBlbmFibGVkID1cbiAgICAgICh0aGlzLnRhcmdldFZlcnNpb24gPT09IFRhcmdldFZlcnNpb24uVjkgfHwgdGhpcy50YXJnZXRWZXJzaW9uID09PSBUYXJnZXRWZXJzaW9uLlYxMCkgJiZcbiAgICAgICF0aGlzLmNvbnRleHQuaXNUZXN0VGFyZ2V0O1xuXG4gIHByaXZhdGUgX3ByaW50ZXIgPSB0cy5jcmVhdGVQcmludGVyKCk7XG4gIHByaXZhdGUgX2ltcG9ydE1hbmFnZXIgPSBuZXcgSW1wb3J0TWFuYWdlcih0aGlzLmZpbGVTeXN0ZW0sIHRoaXMuX3ByaW50ZXIpO1xuICBwcml2YXRlIF9ub2RlRmFpbHVyZXM6IHtub2RlOiB0cy5Ob2RlLCBtZXNzYWdlOiBzdHJpbmd9W10gPSBbXTtcblxuICAvKipcbiAgICogV2hldGhlciBjdXN0b20gSGFtbWVySlMgZXZlbnRzIHByb3ZpZGVkIGJ5IHRoZSBNYXRlcmlhbCBnZXN0dXJlXG4gICAqIGNvbmZpZyBhcmUgdXNlZCBpbiBhIHRlbXBsYXRlLlxuICAgKi9cbiAgcHJpdmF0ZSBfY3VzdG9tRXZlbnRzVXNlZEluVGVtcGxhdGUgPSBmYWxzZTtcblxuICAvKiogV2hldGhlciBzdGFuZGFyZCBIYW1tZXJKUyBldmVudHMgYXJlIHVzZWQgaW4gYSB0ZW1wbGF0ZS4gKi9cbiAgcHJpdmF0ZSBfc3RhbmRhcmRFdmVudHNVc2VkSW5UZW1wbGF0ZSA9IGZhbHNlO1xuXG4gIC8qKiBXaGV0aGVyIEhhbW1lckpTIGlzIGFjY2Vzc2VkIGF0IHJ1bnRpbWUuICovXG4gIHByaXZhdGUgX3VzZWRJblJ1bnRpbWUgPSBmYWxzZTtcblxuICAvKipcbiAgICogTGlzdCBvZiBpbXBvcnRzIHRoYXQgbWFrZSBcImhhbW1lcmpzXCIgYXZhaWxhYmxlIGdsb2JhbGx5LiBXZSBrZWVwIHRyYWNrIG9mIHRoZXNlXG4gICAqIHNpbmNlIHdlIG1pZ2h0IG5lZWQgdG8gcmVtb3ZlIHRoZW0gaWYgSGFtbWVyIGlzIG5vdCB1c2VkLlxuICAgKi9cbiAgcHJpdmF0ZSBfaW5zdGFsbEltcG9ydHM6IHRzLkltcG9ydERlY2xhcmF0aW9uW10gPSBbXTtcblxuICAvKipcbiAgICogTGlzdCBvZiBpZGVudGlmaWVycyB3aGljaCByZXNvbHZlIHRvIHRoZSBnZXN0dXJlIGNvbmZpZyBmcm9tIEFuZ3VsYXIgTWF0ZXJpYWwuXG4gICAqL1xuICBwcml2YXRlIF9nZXN0dXJlQ29uZmlnUmVmZXJlbmNlczogSWRlbnRpZmllclJlZmVyZW5jZVtdID0gW107XG5cbiAgLyoqXG4gICAqIExpc3Qgb2YgaWRlbnRpZmllcnMgd2hpY2ggcmVzb2x2ZSB0byB0aGUgXCJIQU1NRVJfR0VTVFVSRV9DT05GSUdcIiB0b2tlbiBmcm9tXG4gICAqIFwiQGFuZ3VsYXIvcGxhdGZvcm0tYnJvd3NlclwiLlxuICAgKi9cbiAgcHJpdmF0ZSBfaGFtbWVyQ29uZmlnVG9rZW5SZWZlcmVuY2VzOiBJZGVudGlmaWVyUmVmZXJlbmNlW10gPSBbXTtcblxuICAvKipcbiAgICogTGlzdCBvZiBpZGVudGlmaWVycyB3aGljaCByZXNvbHZlIHRvIHRoZSBcIkhhbW1lck1vZHVsZVwiIGZyb21cbiAgICogXCJAYW5ndWxhci9wbGF0Zm9ybS1icm93c2VyXCIuXG4gICAqL1xuICBwcml2YXRlIF9oYW1tZXJNb2R1bGVSZWZlcmVuY2VzOiBJZGVudGlmaWVyUmVmZXJlbmNlW10gPSBbXTtcblxuICAvKipcbiAgICogTGlzdCBvZiBpZGVudGlmaWVycyB0aGF0IGhhdmUgYmVlbiBkZWxldGVkIGZyb20gc291cmNlIGZpbGVzLiBUaGlzIGNhbiBiZVxuICAgKiB1c2VkIHRvIGRldGVybWluZSBpZiBjZXJ0YWluIGltcG9ydHMgYXJlIHN0aWxsIHVzZWQgb3Igbm90LlxuICAgKi9cbiAgcHJpdmF0ZSBfZGVsZXRlZElkZW50aWZpZXJzOiB0cy5JZGVudGlmaWVyW10gPSBbXTtcblxuICB2aXNpdFRlbXBsYXRlKHRlbXBsYXRlOiBSZXNvbHZlZFJlc291cmNlKTogdm9pZCB7XG4gICAgaWYgKCF0aGlzLl9jdXN0b21FdmVudHNVc2VkSW5UZW1wbGF0ZSB8fCAhdGhpcy5fc3RhbmRhcmRFdmVudHNVc2VkSW5UZW1wbGF0ZSkge1xuICAgICAgY29uc3Qge3N0YW5kYXJkRXZlbnRzLCBjdXN0b21FdmVudHN9ID0gaXNIYW1tZXJKc1VzZWRJblRlbXBsYXRlKHRlbXBsYXRlLmNvbnRlbnQpO1xuICAgICAgdGhpcy5fY3VzdG9tRXZlbnRzVXNlZEluVGVtcGxhdGUgPSB0aGlzLl9jdXN0b21FdmVudHNVc2VkSW5UZW1wbGF0ZSB8fCBjdXN0b21FdmVudHM7XG4gICAgICB0aGlzLl9zdGFuZGFyZEV2ZW50c1VzZWRJblRlbXBsYXRlID0gdGhpcy5fc3RhbmRhcmRFdmVudHNVc2VkSW5UZW1wbGF0ZSB8fCBzdGFuZGFyZEV2ZW50cztcbiAgICB9XG4gIH1cblxuICB2aXNpdE5vZGUobm9kZTogdHMuTm9kZSk6IHZvaWQge1xuICAgIHRoaXMuX2NoZWNrSGFtbWVySW1wb3J0cyhub2RlKTtcbiAgICB0aGlzLl9jaGVja0ZvclJ1bnRpbWVIYW1tZXJVc2FnZShub2RlKTtcbiAgICB0aGlzLl9jaGVja0Zvck1hdGVyaWFsR2VzdHVyZUNvbmZpZyhub2RlKTtcbiAgICB0aGlzLl9jaGVja0ZvckhhbW1lckdlc3R1cmVDb25maWdUb2tlbihub2RlKTtcbiAgICB0aGlzLl9jaGVja0ZvckhhbW1lck1vZHVsZVJlZmVyZW5jZShub2RlKTtcbiAgfVxuXG4gIHBvc3RBbmFseXNpcygpOiB2b2lkIHtcbiAgICAvLyBXYWxrIHRocm91Z2ggYWxsIGhhbW1lciBjb25maWcgdG9rZW4gcmVmZXJlbmNlcyBhbmQgY2hlY2sgaWYgdGhlcmVcbiAgICAvLyBpcyBhIHBvdGVudGlhbCBjdXN0b20gZ2VzdHVyZSBjb25maWcgc2V0dXAuXG4gICAgY29uc3QgaGFzQ3VzdG9tR2VzdHVyZUNvbmZpZ1NldHVwID1cbiAgICAgICAgdGhpcy5faGFtbWVyQ29uZmlnVG9rZW5SZWZlcmVuY2VzLnNvbWUociA9PiB0aGlzLl9jaGVja0ZvckN1c3RvbUdlc3R1cmVDb25maWdTZXR1cChyKSk7XG4gICAgY29uc3QgdXNlZEluVGVtcGxhdGUgPSB0aGlzLl9zdGFuZGFyZEV2ZW50c1VzZWRJblRlbXBsYXRlIHx8IHRoaXMuX2N1c3RvbUV2ZW50c1VzZWRJblRlbXBsYXRlO1xuXG4gICAgLypcbiAgICAgIFBvc3NpYmxlIHNjZW5hcmlvcyBhbmQgaG93IHRoZSBtaWdyYXRpb24gc2hvdWxkIGNoYW5nZSB0aGUgcHJvamVjdDpcbiAgICAgICAgMS4gV2UgZGV0ZWN0IHRoYXQgYSBjdXN0b20gSGFtbWVySlMgZ2VzdHVyZSBjb25maWcgaXMgc2V0IHVwOlxuICAgICAgICAgICAgLSBSZW1vdmUgcmVmZXJlbmNlcyB0byB0aGUgTWF0ZXJpYWwgZ2VzdHVyZSBjb25maWcgaWYgbm8gSGFtbWVySlMgZXZlbnQgaXMgdXNlZC5cbiAgICAgICAgICAgIC0gUHJpbnQgYSB3YXJuaW5nIGFib3V0IGFtYmlndW91cyBjb25maWd1cmF0aW9uIHRoYXQgY2Fubm90IGJlIGhhbmRsZWQgY29tcGxldGVseVxuICAgICAgICAgICAgICBpZiB0aGVyZSBhcmUgcmVmZXJlbmNlcyB0byB0aGUgTWF0ZXJpYWwgZ2VzdHVyZSBjb25maWcuXG4gICAgICAgIDIuIFdlIGRldGVjdCB0aGF0IEhhbW1lckpTIGlzIG9ubHkgdXNlZCBwcm9ncmFtbWF0aWNhbGx5OlxuICAgICAgICAgICAgLSBSZW1vdmUgcmVmZXJlbmNlcyB0byBHZXN0dXJlQ29uZmlnIG9mIE1hdGVyaWFsLlxuICAgICAgICAgICAgLSBSZW1vdmUgcmVmZXJlbmNlcyB0byB0aGUgXCJIYW1tZXJNb2R1bGVcIiBpZiBwcmVzZW50LlxuICAgICAgICAzLiBXZSBkZXRlY3QgdGhhdCBzdGFuZGFyZCBIYW1tZXJKUyBldmVudHMgYXJlIHVzZWQgaW4gYSB0ZW1wbGF0ZTpcbiAgICAgICAgICAgIC0gU2V0IHVwIHRoZSBcIkhhbW1lck1vZHVsZVwiIGZyb20gcGxhdGZvcm0tYnJvd3Nlci5cbiAgICAgICAgICAgIC0gUmVtb3ZlIGFsbCBnZXN0dXJlIGNvbmZpZyByZWZlcmVuY2VzLlxuICAgICAgICA0LiBXZSBkZXRlY3QgdGhhdCBjdXN0b20gSGFtbWVySlMgZXZlbnRzIHByb3ZpZGVkIGJ5IHRoZSBNYXRlcmlhbCBnZXN0dXJlXG4gICAgICAgICAgIGNvbmZpZyBhcmUgdXNlZC5cbiAgICAgICAgICAgIC0gQ29weSB0aGUgTWF0ZXJpYWwgZ2VzdHVyZSBjb25maWcgaW50byB0aGUgYXBwLlxuICAgICAgICAgICAgLSBSZXdyaXRlIGFsbCBnZXN0dXJlIGNvbmZpZyByZWZlcmVuY2VzIHRvIHRoZSBuZXdseSBjb3BpZWQgb25lLlxuICAgICAgICAgICAgLSBTZXQgdXAgdGhlIG5ldyBnZXN0dXJlIGNvbmZpZyBpbiB0aGUgcm9vdCBhcHAgbW9kdWxlLlxuICAgICAgICAgICAgLSBTZXQgdXAgdGhlIFwiSGFtbWVyTW9kdWxlXCIgZnJvbSBwbGF0Zm9ybS1icm93c2VyLlxuICAgICAgICA0LiBXZSBkZXRlY3Qgbm8gSGFtbWVySlMgdXNhZ2UgYXQgYWxsOlxuICAgICAgICAgICAgLSBSZW1vdmUgSGFtbWVyIGltcG9ydHNcbiAgICAgICAgICAgIC0gUmVtb3ZlIE1hdGVyaWFsIGdlc3R1cmUgY29uZmlnIHJlZmVyZW5jZXNcbiAgICAgICAgICAgIC0gUmVtb3ZlIEhhbW1lck1vZHVsZSBzZXR1cCBpZiBwcmVzZW50LlxuICAgICAgICAgICAgLSBSZW1vdmUgSGFtbWVyIHNjcmlwdCBpbXBvcnRzIGluIFwiaW5kZXguaHRtbFwiIGZpbGVzLlxuICAgICovXG5cbiAgICBpZiAoaGFzQ3VzdG9tR2VzdHVyZUNvbmZpZ1NldHVwKSB7XG4gICAgICAvLyBJZiBhIGN1c3RvbSBnZXN0dXJlIGNvbmZpZyBpcyBwcm92aWRlZCwgd2UgYWx3YXlzIGFzc3VtZSB0aGF0IEhhbW1lckpTIGlzIHVzZWQuXG4gICAgICBIYW1tZXJHZXN0dXJlc01pZ3JhdGlvbi5nbG9iYWxVc2VzSGFtbWVyID0gdHJ1ZTtcbiAgICAgIGlmICghdXNlZEluVGVtcGxhdGUgJiYgdGhpcy5fZ2VzdHVyZUNvbmZpZ1JlZmVyZW5jZXMubGVuZ3RoKSB7XG4gICAgICAgIC8vIElmIHRoZSBBbmd1bGFyIE1hdGVyaWFsIGdlc3R1cmUgZXZlbnRzIGFyZSBub3QgdXNlZCBhbmQgd2UgZm91bmQgYSBjdXN0b21cbiAgICAgICAgLy8gZ2VzdHVyZSBjb25maWcsIHdlIGNhbiBzYWZlbHkgcmVtb3ZlIHJlZmVyZW5jZXMgdG8gdGhlIE1hdGVyaWFsIGdlc3R1cmUgY29uZmlnXG4gICAgICAgIC8vIHNpbmNlIGV2ZW50cyBwcm92aWRlZCBieSB0aGUgTWF0ZXJpYWwgZ2VzdHVyZSBjb25maWcgYXJlIGd1YXJhbnRlZWQgdG8gYmUgdW51c2VkLlxuICAgICAgICB0aGlzLl9yZW1vdmVNYXRlcmlhbEdlc3R1cmVDb25maWdTZXR1cCgpO1xuICAgICAgICB0aGlzLnByaW50SW5mbyhcbiAgICAgICAgICAgICdUaGUgSGFtbWVySlMgdjkgbWlncmF0aW9uIGZvciBBbmd1bGFyIENvbXBvbmVudHMgZGV0ZWN0ZWQgdGhhdCBIYW1tZXJKUyBpcyAnICtcbiAgICAgICAgICAgICdtYW51YWxseSBzZXQgdXAgaW4gY29tYmluYXRpb24gd2l0aCByZWZlcmVuY2VzIHRvIHRoZSBBbmd1bGFyIE1hdGVyaWFsIGdlc3R1cmUgJyArXG4gICAgICAgICAgICAnY29uZmlnLiBUaGlzIHRhcmdldCBjYW5ub3QgYmUgbWlncmF0ZWQgY29tcGxldGVseSwgYnV0IGFsbCByZWZlcmVuY2VzIHRvIHRoZSAnICtcbiAgICAgICAgICAgICdkZXByZWNhdGVkIEFuZ3VsYXIgTWF0ZXJpYWwgZ2VzdHVyZSBoYXZlIGJlZW4gcmVtb3ZlZC4gUmVhZCBtb3JlIGhlcmU6ICcgK1xuICAgICAgICAgICAgJ2h0dHBzOi8vZ2l0LmlvL25nLW1hdGVyaWFsLXY5LWhhbW1lci1hbWJpZ3VvdXMtdXNhZ2UnKTtcbiAgICAgIH0gZWxzZSBpZiAodXNlZEluVGVtcGxhdGUgJiYgdGhpcy5fZ2VzdHVyZUNvbmZpZ1JlZmVyZW5jZXMubGVuZ3RoKSB7XG4gICAgICAgIC8vIFNpbmNlIHRoZXJlIGlzIGEgcmVmZXJlbmNlIHRvIHRoZSBBbmd1bGFyIE1hdGVyaWFsIGdlc3R1cmUgY29uZmlnLCBhbmQgd2UgZGV0ZWN0ZWRcbiAgICAgICAgLy8gdXNhZ2Ugb2YgYSBnZXN0dXJlIGV2ZW50IHRoYXQgY291bGQgYmUgcHJvdmlkZWQgYnkgQW5ndWxhciBNYXRlcmlhbCwgd2UgKmNhbm5vdCpcbiAgICAgICAgLy8gYXV0b21hdGljYWxseSByZW1vdmUgcmVmZXJlbmNlcy4gVGhpcyBpcyBiZWNhdXNlIHdlIGRvICpub3QqIGtub3cgd2hldGhlciB0aGVcbiAgICAgICAgLy8gZXZlbnQgaXMgYWN0dWFsbHkgcHJvdmlkZWQgYnkgdGhlIGN1c3RvbSBjb25maWcgb3IgYnkgdGhlIE1hdGVyaWFsIGNvbmZpZy5cbiAgICAgICAgdGhpcy5wcmludEluZm8oXG4gICAgICAgICAgICAnVGhlIEhhbW1lckpTIHY5IG1pZ3JhdGlvbiBmb3IgQW5ndWxhciBDb21wb25lbnRzIGRldGVjdGVkIHRoYXQgSGFtbWVySlMgaXMgJyArXG4gICAgICAgICAgICAnbWFudWFsbHkgc2V0IHVwIGluIGNvbWJpbmF0aW9uIHdpdGggcmVmZXJlbmNlcyB0byB0aGUgQW5ndWxhciBNYXRlcmlhbCBnZXN0dXJlICcgK1xuICAgICAgICAgICAgJ2NvbmZpZy4gVGhpcyB0YXJnZXQgY2Fubm90IGJlIG1pZ3JhdGVkIGNvbXBsZXRlbHkuIFBsZWFzZSBtYW51YWxseSByZW1vdmUgJyArXG4gICAgICAgICAgICAncmVmZXJlbmNlcyB0byB0aGUgZGVwcmVjYXRlZCBBbmd1bGFyIE1hdGVyaWFsIGdlc3R1cmUgY29uZmlnLiBSZWFkIG1vcmUgaGVyZTogJyArXG4gICAgICAgICAgICAnaHR0cHM6Ly9naXQuaW8vbmctbWF0ZXJpYWwtdjktaGFtbWVyLWFtYmlndW91cy11c2FnZScpO1xuICAgICAgfVxuICAgIH0gZWxzZSBpZiAodGhpcy5fdXNlZEluUnVudGltZSB8fCB1c2VkSW5UZW1wbGF0ZSkge1xuICAgICAgLy8gV2Uga2VlcCB0cmFjayBvZiB3aGV0aGVyIEhhbW1lciBpcyB1c2VkIGdsb2JhbGx5LiBUaGlzIGlzIG5lY2Vzc2FyeSBiZWNhdXNlIHdlXG4gICAgICAvLyB3YW50IHRvIG9ubHkgcmVtb3ZlIEhhbW1lciBmcm9tIHRoZSBcInBhY2thZ2UuanNvblwiIGlmIGl0IGlzIG5vdCB1c2VkIGluIGFueSBwcm9qZWN0XG4gICAgICAvLyB0YXJnZXQuIEp1c3QgYmVjYXVzZSBpdCBpc24ndCB1c2VkIGluIG9uZSB0YXJnZXQgZG9lc24ndCBtZWFuIHRoYXQgd2UgY2FuIHNhZmVseVxuICAgICAgLy8gcmVtb3ZlIHRoZSBkZXBlbmRlbmN5LlxuICAgICAgSGFtbWVyR2VzdHVyZXNNaWdyYXRpb24uZ2xvYmFsVXNlc0hhbW1lciA9IHRydWU7XG5cbiAgICAgIC8vIElmIGhhbW1lciBpcyBvbmx5IHVzZWQgYXQgcnVudGltZSwgd2UgZG9uJ3QgbmVlZCB0aGUgZ2VzdHVyZSBjb25maWcgb3IgXCJIYW1tZXJNb2R1bGVcIlxuICAgICAgLy8gYW5kIGNhbiByZW1vdmUgaXQgKGFsb25nIHdpdGggdGhlIGhhbW1lciBjb25maWcgdG9rZW4gaW1wb3J0IGlmIG5vIGxvbmdlciBuZWVkZWQpLlxuICAgICAgaWYgKCF1c2VkSW5UZW1wbGF0ZSkge1xuICAgICAgICB0aGlzLl9yZW1vdmVNYXRlcmlhbEdlc3R1cmVDb25maWdTZXR1cCgpO1xuICAgICAgICB0aGlzLl9yZW1vdmVIYW1tZXJNb2R1bGVSZWZlcmVuY2VzKCk7XG4gICAgICB9IGVsc2UgaWYgKHRoaXMuX3N0YW5kYXJkRXZlbnRzVXNlZEluVGVtcGxhdGUgJiYgIXRoaXMuX2N1c3RvbUV2ZW50c1VzZWRJblRlbXBsYXRlKSB7XG4gICAgICAgIHRoaXMuX3NldHVwSGFtbWVyV2l0aFN0YW5kYXJkRXZlbnRzKCk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICB0aGlzLl9zZXR1cEhhbW1lcldpdGhDdXN0b21FdmVudHMoKTtcbiAgICAgIH1cbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5fcmVtb3ZlSGFtbWVyU2V0dXAoKTtcbiAgICB9XG5cbiAgICAvLyBSZWNvcmQgdGhlIGNoYW5nZXMgY29sbGVjdGVkIGluIHRoZSBpbXBvcnQgbWFuYWdlci4gQ2hhbmdlcyBuZWVkIHRvIGJlIGFwcGxpZWRcbiAgICAvLyBvbmNlIHRoZSBpbXBvcnQgbWFuYWdlciByZWdpc3RlcmVkIGFsbCBpbXBvcnQgbW9kaWZpY2F0aW9ucy4gVGhpcyBhdm9pZHMgY29sbGlzaW9ucy5cbiAgICB0aGlzLl9pbXBvcnRNYW5hZ2VyLnJlY29yZENoYW5nZXMoKTtcblxuICAgIC8vIENyZWF0ZSBtaWdyYXRpb24gZmFpbHVyZXMgdGhhdCB3aWxsIGJlIHByaW50ZWQgYnkgdGhlIHVwZGF0ZS10b29sIG9uIG1pZ3JhdGlvblxuICAgIC8vIGNvbXBsZXRpb24uIFdlIG5lZWQgc3BlY2lhbCBsb2dpYyBmb3IgdXBkYXRpbmcgZmFpbHVyZSBwb3NpdGlvbnMgdG8gcmVmbGVjdFxuICAgIC8vIHRoZSBuZXcgc291cmNlIGZpbGUgYWZ0ZXIgbW9kaWZpY2F0aW9ucyBmcm9tIHRoZSBpbXBvcnQgbWFuYWdlci5cbiAgICB0aGlzLmZhaWx1cmVzLnB1c2goLi4udGhpcy5fY3JlYXRlTWlncmF0aW9uRmFpbHVyZXMoKSk7XG5cbiAgICAvLyBUaGUgdGVtcGxhdGUgY2hlY2sgZm9yIEhhbW1lckpTIGV2ZW50cyBpcyBub3QgY29tcGxldGVseSByZWxpYWJsZSBhcyB0aGUgZXZlbnRcbiAgICAvLyBvdXRwdXQgY291bGQgYWxzbyBiZSBmcm9tIGEgY29tcG9uZW50IGhhdmluZyBhbiBvdXRwdXQgbmFtZWQgc2ltaWxhcmx5IHRvIGEga25vd25cbiAgICAvLyBoYW1tZXJqcyBldmVudCAoZS5nLiBcIkBPdXRwdXQoKSBzbGlkZVwiKS4gVGhlIHVzYWdlIGlzIHRoZXJlZm9yZSBzb21ld2hhdCBhbWJpZ3VvdXNcbiAgICAvLyBhbmQgd2Ugd2FudCB0byBwcmludCBhIG1lc3NhZ2UgdGhhdCBkZXZlbG9wZXJzIG1pZ2h0IGJlIGFibGUgdG8gcmVtb3ZlIEhhbW1lciBtYW51YWxseS5cbiAgICBpZiAoIWhhc0N1c3RvbUdlc3R1cmVDb25maWdTZXR1cCAmJiAhdGhpcy5fdXNlZEluUnVudGltZSAmJiB1c2VkSW5UZW1wbGF0ZSkge1xuICAgICAgdGhpcy5wcmludEluZm8oXG4gICAgICAgICAgJ1RoZSBIYW1tZXJKUyB2OSBtaWdyYXRpb24gZm9yIEFuZ3VsYXIgQ29tcG9uZW50cyBtaWdyYXRlZCB0aGUgJyArXG4gICAgICAgICAgJ3Byb2plY3QgdG8ga2VlcCBIYW1tZXJKUyBpbnN0YWxsZWQsIGJ1dCBkZXRlY3RlZCBhbWJpZ3VvdXMgdXNhZ2Ugb2YgSGFtbWVySlMuIFBsZWFzZSAnICtcbiAgICAgICAgICAnbWFudWFsbHkgY2hlY2sgaWYgeW91IGNhbiByZW1vdmUgSGFtbWVySlMgZnJvbSB5b3VyIGFwcGxpY2F0aW9uLiBNb3JlIGRldGFpbHM6ICcgK1xuICAgICAgICAgICdodHRwczovL2dpdC5pby9uZy1tYXRlcmlhbC12OS1oYW1tZXItYW1iaWd1b3VzLXVzYWdlJyk7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIFNldHMgdXAgdGhlIGhhbW1lciBnZXN0dXJlIGNvbmZpZyBpbiB0aGUgY3VycmVudCBwcm9qZWN0LiBUbyBhY2hpZXZlIHRoaXMsIHRoZVxuICAgKiBmb2xsb3dpbmcgc3RlcHMgYXJlIHBlcmZvcm1lZDpcbiAgICogICAxKSBDcmVhdGUgY29weSBvZiBBbmd1bGFyIE1hdGVyaWFsIGdlc3R1cmUgY29uZmlnLlxuICAgKiAgIDIpIFJld3JpdGUgYWxsIHJlZmVyZW5jZXMgdG8gdGhlIEFuZ3VsYXIgTWF0ZXJpYWwgZ2VzdHVyZSBjb25maWcgdG8gdGhlXG4gICAqICAgICAgbmV3IGdlc3R1cmUgY29uZmlnLlxuICAgKiAgIDMpIFNldHVwIHRoZSBIQU1NRVJfR0VTVFVSRV9DT05GSUcgaW4gdGhlIHJvb3QgYXBwIG1vZHVsZSAoaWYgbm90IGRvbmUgYWxyZWFkeSkuXG4gICAqICAgNCkgU2V0dXAgdGhlIFwiSGFtbWVyTW9kdWxlXCIgaW4gdGhlIHJvb3QgYXBwIG1vZHVsZSAoaWYgbm90IGRvbmUgYWxyZWFkeSkuXG4gICAqL1xuICBwcml2YXRlIF9zZXR1cEhhbW1lcldpdGhDdXN0b21FdmVudHMoKSB7XG4gICAgY29uc3QgcHJvamVjdCA9IHRoaXMuY29udGV4dC5wcm9qZWN0O1xuICAgIGNvbnN0IHNvdXJjZVJvb3QgPSBkZXZraXROb3JtYWxpemUocHJvamVjdC5zb3VyY2VSb290IHx8IHByb2plY3Qucm9vdCk7XG4gICAgY29uc3QgbmV3Q29uZmlnUGF0aCA9XG4gICAgICAgIGRldmtpdEpvaW4oc291cmNlUm9vdCwgdGhpcy5fZ2V0QXZhaWxhYmxlR2VzdHVyZUNvbmZpZ0ZpbGVOYW1lKHNvdXJjZVJvb3QpKTtcblxuICAgIC8vIENvcHkgZ2VzdHVyZSBjb25maWcgdGVtcGxhdGUgaW50byB0aGUgQ0xJIHByb2plY3QuXG4gICAgdGhpcy5maWxlU3lzdGVtLmNyZWF0ZShcbiAgICAgICAgbmV3Q29uZmlnUGF0aCwgcmVhZEZpbGVTeW5jKHJlcXVpcmUucmVzb2x2ZShHRVNUVVJFX0NPTkZJR19URU1QTEFURV9QQVRIKSwgJ3V0ZjgnKSk7XG5cbiAgICAvLyBSZXBsYWNlIGFsbCBNYXRlcmlhbCBnZXN0dXJlIGNvbmZpZyByZWZlcmVuY2VzIHRvIHJlc29sdmUgdG8gdGhlXG4gICAgLy8gbmV3bHkgY29waWVkIGdlc3R1cmUgY29uZmlnLlxuICAgIHRoaXMuX2dlc3R1cmVDb25maWdSZWZlcmVuY2VzLmZvckVhY2goXG4gICAgICAgIGkgPT4gdGhpcy5fcmVwbGFjZUdlc3R1cmVDb25maWdSZWZlcmVuY2UoXG4gICAgICAgICAgICBpLCBHRVNUVVJFX0NPTkZJR19DTEFTU19OQU1FLFxuICAgICAgICAgICAgZ2V0TW9kdWxlU3BlY2lmaWVyKG5ld0NvbmZpZ1BhdGgsIGkubm9kZS5nZXRTb3VyY2VGaWxlKCkuZmlsZU5hbWUpKSk7XG5cbiAgICAvLyBTZXR1cCB0aGUgZ2VzdHVyZSBjb25maWcgcHJvdmlkZXIgYW5kIHRoZSBcIkhhbW1lck1vZHVsZVwiIGluIHRoZSByb290IG1vZHVsZVxuICAgIC8vIGlmIG5vdCBkb25lIGFscmVhZHkuIFRoZSBcIkhhbW1lck1vZHVsZVwiIGlzIG5lZWRlZCBpbiB2OSBzaW5jZSBpdCBlbmFibGVzIHRoZVxuICAgIC8vIEhhbW1lciBldmVudCBwbHVnaW4gdGhhdCB3YXMgcHJldmlvdXNseSBlbmFibGVkIGJ5IGRlZmF1bHQgaW4gdjguXG4gICAgdGhpcy5fc2V0dXBOZXdHZXN0dXJlQ29uZmlnSW5Sb290TW9kdWxlKG5ld0NvbmZpZ1BhdGgpO1xuICAgIHRoaXMuX3NldHVwSGFtbWVyTW9kdWxlSW5Sb290TW9kdWxlKCk7XG4gIH1cblxuICAvKipcbiAgICogU2V0cyB1cCB0aGUgc3RhbmRhcmQgaGFtbWVyIG1vZHVsZSBpbiB0aGUgcHJvamVjdCBhbmQgcmVtb3ZlcyBhbGxcbiAgICogcmVmZXJlbmNlcyB0byB0aGUgZGVwcmVjYXRlZCBBbmd1bGFyIE1hdGVyaWFsIGdlc3R1cmUgY29uZmlnLlxuICAgKi9cbiAgcHJpdmF0ZSBfc2V0dXBIYW1tZXJXaXRoU3RhbmRhcmRFdmVudHMoKSB7XG4gICAgLy8gU2V0dXAgdGhlIEhhbW1lck1vZHVsZS4gVGhlIEhhbW1lck1vZHVsZSBlbmFibGVzIHN1cHBvcnQgZm9yXG4gICAgLy8gdGhlIHN0YW5kYXJkIEhhbW1lckpTIGV2ZW50cy5cbiAgICB0aGlzLl9zZXR1cEhhbW1lck1vZHVsZUluUm9vdE1vZHVsZSgpO1xuICAgIHRoaXMuX3JlbW92ZU1hdGVyaWFsR2VzdHVyZUNvbmZpZ1NldHVwKCk7XG4gIH1cblxuICAvKipcbiAgICogUmVtb3ZlcyBIYW1tZXIgZnJvbSB0aGUgY3VycmVudCBwcm9qZWN0LiBUaGUgZm9sbG93aW5nIHN0ZXBzIGFyZSBwZXJmb3JtZWQ6XG4gICAqICAgMSkgRGVsZXRlIGFsbCBUeXBlU2NyaXB0IGltcG9ydHMgdG8gXCJoYW1tZXJqc1wiLlxuICAgKiAgIDIpIFJlbW92ZSByZWZlcmVuY2VzIHRvIHRoZSBBbmd1bGFyIE1hdGVyaWFsIGdlc3R1cmUgY29uZmlnLlxuICAgKiAgIDMpIFJlbW92ZSBcImhhbW1lcmpzXCIgZnJvbSBhbGwgaW5kZXggSFRNTCBmaWxlcyBvZiB0aGUgY3VycmVudCBwcm9qZWN0LlxuICAgKi9cbiAgcHJpdmF0ZSBfcmVtb3ZlSGFtbWVyU2V0dXAoKSB7XG4gICAgdGhpcy5faW5zdGFsbEltcG9ydHMuZm9yRWFjaChpID0+IHRoaXMuX2ltcG9ydE1hbmFnZXIuZGVsZXRlSW1wb3J0QnlEZWNsYXJhdGlvbihpKSk7XG5cbiAgICB0aGlzLl9yZW1vdmVNYXRlcmlhbEdlc3R1cmVDb25maWdTZXR1cCgpO1xuICAgIHRoaXMuX3JlbW92ZUhhbW1lck1vZHVsZVJlZmVyZW5jZXMoKTtcbiAgICB0aGlzLl9yZW1vdmVIYW1tZXJGcm9tSW5kZXhGaWxlKCk7XG4gIH1cblxuICAvKipcbiAgICogUmVtb3ZlcyB0aGUgZ2VzdHVyZSBjb25maWcgc2V0dXAgYnkgZGVsZXRpbmcgYWxsIGZvdW5kIHJlZmVyZW5jZXMgdG8gdGhlIEFuZ3VsYXJcbiAgICogTWF0ZXJpYWwgZ2VzdHVyZSBjb25maWcuIEFkZGl0aW9uYWxseSwgdW51c2VkIGltcG9ydHMgdG8gdGhlIGhhbW1lciBnZXN0dXJlIGNvbmZpZ1xuICAgKiB0b2tlbiBmcm9tIFwiQGFuZ3VsYXIvcGxhdGZvcm0tYnJvd3NlclwiIHdpbGwgYmUgcmVtb3ZlZCBhcyB3ZWxsLlxuICAgKi9cbiAgcHJpdmF0ZSBfcmVtb3ZlTWF0ZXJpYWxHZXN0dXJlQ29uZmlnU2V0dXAoKSB7XG4gICAgdGhpcy5fZ2VzdHVyZUNvbmZpZ1JlZmVyZW5jZXMuZm9yRWFjaChyID0+IHRoaXMuX3JlbW92ZUdlc3R1cmVDb25maWdSZWZlcmVuY2UocikpO1xuXG4gICAgdGhpcy5faGFtbWVyQ29uZmlnVG9rZW5SZWZlcmVuY2VzLmZvckVhY2gociA9PiB7XG4gICAgICBpZiAoci5pc0ltcG9ydCkge1xuICAgICAgICB0aGlzLl9yZW1vdmVIYW1tZXJDb25maWdUb2tlbkltcG9ydElmVW51c2VkKHIpO1xuICAgICAgfVxuICAgIH0pO1xuICB9XG5cbiAgLyoqIFJlbW92ZXMgYWxsIHJlZmVyZW5jZXMgdG8gdGhlIFwiSGFtbWVyTW9kdWxlXCIgZnJvbSBcIkBhbmd1bGFyL3BsYXRmb3JtLWJyb3dzZXJcIi4gKi9cbiAgcHJpdmF0ZSBfcmVtb3ZlSGFtbWVyTW9kdWxlUmVmZXJlbmNlcygpIHtcbiAgICB0aGlzLl9oYW1tZXJNb2R1bGVSZWZlcmVuY2VzLmZvckVhY2goKHtub2RlLCBpc0ltcG9ydCwgaW1wb3J0RGF0YX0pID0+IHtcbiAgICAgIGNvbnN0IHNvdXJjZUZpbGUgPSBub2RlLmdldFNvdXJjZUZpbGUoKTtcbiAgICAgIGNvbnN0IHJlY29yZGVyID0gdGhpcy5maWxlU3lzdGVtLmVkaXQodGhpcy5maWxlU3lzdGVtLnJlc29sdmUoc291cmNlRmlsZS5maWxlTmFtZSkpO1xuXG4gICAgICAvLyBPbmx5IHJlbW92ZSB0aGUgaW1wb3J0IGZvciB0aGUgSGFtbWVyTW9kdWxlIGlmIHRoZSBtb2R1bGUgaGFzIGJlZW4gYWNjZXNzZWRcbiAgICAgIC8vIHRocm91Z2ggYSBub24tbmFtZXNwYWNlZCBpZGVudGlmaWVyIGFjY2Vzcy5cbiAgICAgIGlmICghaXNOYW1lc3BhY2VkSWRlbnRpZmllckFjY2Vzcyhub2RlKSkge1xuICAgICAgICB0aGlzLl9pbXBvcnRNYW5hZ2VyLmRlbGV0ZU5hbWVkQmluZGluZ0ltcG9ydChcbiAgICAgICAgICAgIHNvdXJjZUZpbGUsIEhBTU1FUl9NT0RVTEVfTkFNRSwgaW1wb3J0RGF0YS5tb2R1bGVOYW1lKTtcbiAgICAgIH1cblxuICAgICAgLy8gRm9yIHJlZmVyZW5jZXMgZnJvbSB3aXRoaW4gYW4gaW1wb3J0LCB3ZSBkbyBub3QgbmVlZCB0byBkbyBhbnl0aGluZyBvdGhlciB0aGFuXG4gICAgICAvLyByZW1vdmluZyB0aGUgaW1wb3J0LiBGb3Igb3RoZXIgcmVmZXJlbmNlcywgd2UgcmVtb3ZlIHRoZSBpbXBvcnQgYW5kIHRoZSBhY3R1YWxcbiAgICAgIC8vIGlkZW50aWZpZXIgaW4gdGhlIG1vZHVsZSBpbXBvcnRzLlxuICAgICAgaWYgKGlzSW1wb3J0KSB7XG4gICAgICAgIHJldHVybjtcbiAgICAgIH1cblxuICAgICAgLy8gSWYgdGhlIFwiSGFtbWVyTW9kdWxlXCIgaXMgcmVmZXJlbmNlZCB3aXRoaW4gYW4gYXJyYXkgbGl0ZXJhbCwgd2UgY2FuXG4gICAgICAvLyByZW1vdmUgdGhlIGVsZW1lbnQgZWFzaWx5LiBPdGhlcndpc2UgaWYgaXQncyBvdXRzaWRlIG9mIGFuIGFycmF5IGxpdGVyYWwsXG4gICAgICAvLyB3ZSBuZWVkIHRvIHJlcGxhY2UgdGhlIHJlZmVyZW5jZSB3aXRoIGFuIGVtcHR5IG9iamVjdCBsaXRlcmFsIHcvIHRvZG8gdG9cbiAgICAgIC8vIG5vdCBicmVhayB0aGUgYXBwbGljYXRpb24uXG4gICAgICBpZiAodHMuaXNBcnJheUxpdGVyYWxFeHByZXNzaW9uKG5vZGUucGFyZW50KSkge1xuICAgICAgICAvLyBSZW1vdmVzIHRoZSBcIkhhbW1lck1vZHVsZVwiIGZyb20gdGhlIHBhcmVudCBhcnJheSBleHByZXNzaW9uLiBSZW1vdmVzXG4gICAgICAgIC8vIHRoZSB0cmFpbGluZyBjb21tYSB0b2tlbiBpZiBwcmVzZW50LlxuICAgICAgICByZW1vdmVFbGVtZW50RnJvbUFycmF5RXhwcmVzc2lvbihub2RlLCByZWNvcmRlcik7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICByZWNvcmRlci5yZW1vdmUobm9kZS5nZXRTdGFydCgpLCBub2RlLmdldFdpZHRoKCkpO1xuICAgICAgICByZWNvcmRlci5pbnNlcnRSaWdodChub2RlLmdldFN0YXJ0KCksIGAvKiBUT0RPOiByZW1vdmUgKi8ge31gKTtcbiAgICAgICAgdGhpcy5fbm9kZUZhaWx1cmVzLnB1c2goe1xuICAgICAgICAgIG5vZGU6IG5vZGUsXG4gICAgICAgICAgbWVzc2FnZTogJ1VuYWJsZSB0byBkZWxldGUgcmVmZXJlbmNlIHRvIFwiSGFtbWVyTW9kdWxlXCIuJyxcbiAgICAgICAgfSk7XG4gICAgICB9XG4gICAgfSk7XG4gIH1cblxuICAvKipcbiAgICogQ2hlY2tzIGlmIHRoZSBnaXZlbiBub2RlIGlzIGEgcmVmZXJlbmNlIHRvIHRoZSBoYW1tZXIgZ2VzdHVyZSBjb25maWdcbiAgICogdG9rZW4gZnJvbSBwbGF0Zm9ybS1icm93c2VyLiBJZiBzbywga2VlcHMgdHJhY2sgb2YgdGhlIHJlZmVyZW5jZS5cbiAgICovXG4gIHByaXZhdGUgX2NoZWNrRm9ySGFtbWVyR2VzdHVyZUNvbmZpZ1Rva2VuKG5vZGU6IHRzLk5vZGUpIHtcbiAgICBpZiAodHMuaXNJZGVudGlmaWVyKG5vZGUpKSB7XG4gICAgICBjb25zdCBpbXBvcnREYXRhID0gZ2V0SW1wb3J0T2ZJZGVudGlmaWVyKG5vZGUsIHRoaXMudHlwZUNoZWNrZXIpO1xuICAgICAgaWYgKGltcG9ydERhdGEgJiYgaW1wb3J0RGF0YS5zeW1ib2xOYW1lID09PSBIQU1NRVJfQ09ORklHX1RPS0VOX05BTUUgJiZcbiAgICAgICAgICBpbXBvcnREYXRhLm1vZHVsZU5hbWUgPT09IEhBTU1FUl9DT05GSUdfVE9LRU5fTU9EVUxFKSB7XG4gICAgICAgIHRoaXMuX2hhbW1lckNvbmZpZ1Rva2VuUmVmZXJlbmNlcy5wdXNoKFxuICAgICAgICAgICAge25vZGUsIGltcG9ydERhdGEsIGlzSW1wb3J0OiB0cy5pc0ltcG9ydFNwZWNpZmllcihub2RlLnBhcmVudCl9KTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogQ2hlY2tzIGlmIHRoZSBnaXZlbiBub2RlIGlzIGEgcmVmZXJlbmNlIHRvIHRoZSBIYW1tZXJNb2R1bGUgZnJvbVxuICAgKiBcIkBhbmd1bGFyL3BsYXRmb3JtLWJyb3dzZXJcIi4gSWYgc28sIGtlZXBzIHRyYWNrIG9mIHRoZSByZWZlcmVuY2UuXG4gICAqL1xuICBwcml2YXRlIF9jaGVja0ZvckhhbW1lck1vZHVsZVJlZmVyZW5jZShub2RlOiB0cy5Ob2RlKSB7XG4gICAgaWYgKHRzLmlzSWRlbnRpZmllcihub2RlKSkge1xuICAgICAgY29uc3QgaW1wb3J0RGF0YSA9IGdldEltcG9ydE9mSWRlbnRpZmllcihub2RlLCB0aGlzLnR5cGVDaGVja2VyKTtcbiAgICAgIGlmIChpbXBvcnREYXRhICYmIGltcG9ydERhdGEuc3ltYm9sTmFtZSA9PT0gSEFNTUVSX01PRFVMRV9OQU1FICYmXG4gICAgICAgICAgaW1wb3J0RGF0YS5tb2R1bGVOYW1lID09PSBIQU1NRVJfTU9EVUxFX0lNUE9SVCkge1xuICAgICAgICB0aGlzLl9oYW1tZXJNb2R1bGVSZWZlcmVuY2VzLnB1c2goXG4gICAgICAgICAgICB7bm9kZSwgaW1wb3J0RGF0YSwgaXNJbXBvcnQ6IHRzLmlzSW1wb3J0U3BlY2lmaWVyKG5vZGUucGFyZW50KX0pO1xuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBDaGVja3MgaWYgdGhlIGdpdmVuIG5vZGUgaXMgYW4gaW1wb3J0IHRvIHRoZSBIYW1tZXJKUyBwYWNrYWdlLiBJbXBvcnRzIHRvXG4gICAqIEhhbW1lckpTIHdoaWNoIGxvYWQgc3BlY2lmaWMgc3ltYm9scyBmcm9tIHRoZSBwYWNrYWdlIGFyZSBjb25zaWRlcmVkIGFzXG4gICAqIHJ1bnRpbWUgdXNhZ2Ugb2YgSGFtbWVyLiBlLmcuIGBpbXBvcnQge1N5bWJvbH0gZnJvbSBcImhhbW1lcmpzXCI7YC5cbiAgICovXG4gIHByaXZhdGUgX2NoZWNrSGFtbWVySW1wb3J0cyhub2RlOiB0cy5Ob2RlKSB7XG4gICAgaWYgKHRzLmlzSW1wb3J0RGVjbGFyYXRpb24obm9kZSkgJiYgdHMuaXNTdHJpbmdMaXRlcmFsKG5vZGUubW9kdWxlU3BlY2lmaWVyKSAmJlxuICAgICAgICBub2RlLm1vZHVsZVNwZWNpZmllci50ZXh0ID09PSBIQU1NRVJfTU9EVUxFX1NQRUNJRklFUikge1xuICAgICAgLy8gSWYgdGhlcmUgaXMgYW4gaW1wb3J0IHRvIEhhbW1lckpTIHRoYXQgaW1wb3J0cyBzeW1ib2xzLCBvciBpcyBuYW1lc3BhY2VkXG4gICAgICAvLyAoZS5nLiBcImltcG9ydCB7QSwgQn0gZnJvbSAuLi5cIiBvciBcImltcG9ydCAqIGFzIGhhbW1lciBmcm9tIC4uLlwiKSwgdGhlbiB3ZVxuICAgICAgLy8gYXNzdW1lIHRoYXQgc29tZSBleHBvcnRzIGFyZSB1c2VkIGF0IHJ1bnRpbWUuXG4gICAgICBpZiAobm9kZS5pbXBvcnRDbGF1c2UgJiZcbiAgICAgICAgICAhKG5vZGUuaW1wb3J0Q2xhdXNlLm5hbWVkQmluZGluZ3MgJiYgdHMuaXNOYW1lZEltcG9ydHMobm9kZS5pbXBvcnRDbGF1c2UubmFtZWRCaW5kaW5ncykgJiZcbiAgICAgICAgICAgIG5vZGUuaW1wb3J0Q2xhdXNlLm5hbWVkQmluZGluZ3MuZWxlbWVudHMubGVuZ3RoID09PSAwKSkge1xuICAgICAgICB0aGlzLl91c2VkSW5SdW50aW1lID0gdHJ1ZTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIHRoaXMuX2luc3RhbGxJbXBvcnRzLnB1c2gobm9kZSk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIENoZWNrcyBpZiB0aGUgZ2l2ZW4gbm9kZSBhY2Nlc3NlcyB0aGUgZ2xvYmFsIFwiSGFtbWVyXCIgc3ltYm9sIGF0IHJ1bnRpbWUuIElmIHNvLFxuICAgKiB0aGUgbWlncmF0aW9uIHJ1bGUgc3RhdGUgd2lsbCBiZSB1cGRhdGVkIHRvIHJlZmxlY3QgdGhhdCBIYW1tZXIgaXMgdXNlZCBhdCBydW50aW1lLlxuICAgKi9cbiAgcHJpdmF0ZSBfY2hlY2tGb3JSdW50aW1lSGFtbWVyVXNhZ2Uobm9kZTogdHMuTm9kZSkge1xuICAgIGlmICh0aGlzLl91c2VkSW5SdW50aW1lKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgLy8gRGV0ZWN0cyB1c2FnZXMgb2YgXCJ3aW5kb3cuSGFtbWVyXCIuXG4gICAgaWYgKHRzLmlzUHJvcGVydHlBY2Nlc3NFeHByZXNzaW9uKG5vZGUpICYmIG5vZGUubmFtZS50ZXh0ID09PSAnSGFtbWVyJykge1xuICAgICAgY29uc3Qgb3JpZ2luRXhwciA9IHVud3JhcEV4cHJlc3Npb24obm9kZS5leHByZXNzaW9uKTtcbiAgICAgIGlmICh0cy5pc0lkZW50aWZpZXIob3JpZ2luRXhwcikgJiYgb3JpZ2luRXhwci50ZXh0ID09PSAnd2luZG93Jykge1xuICAgICAgICB0aGlzLl91c2VkSW5SdW50aW1lID0gdHJ1ZTtcbiAgICAgIH1cbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICAvLyBEZXRlY3RzIHVzYWdlcyBvZiBcIndpbmRvd1snSGFtbWVyJ11cIi5cbiAgICBpZiAodHMuaXNFbGVtZW50QWNjZXNzRXhwcmVzc2lvbihub2RlKSAmJiB0cy5pc1N0cmluZ0xpdGVyYWwobm9kZS5hcmd1bWVudEV4cHJlc3Npb24pICYmXG4gICAgICAgIG5vZGUuYXJndW1lbnRFeHByZXNzaW9uLnRleHQgPT09ICdIYW1tZXInKSB7XG4gICAgICBjb25zdCBvcmlnaW5FeHByID0gdW53cmFwRXhwcmVzc2lvbihub2RlLmV4cHJlc3Npb24pO1xuICAgICAgaWYgKHRzLmlzSWRlbnRpZmllcihvcmlnaW5FeHByKSAmJiBvcmlnaW5FeHByLnRleHQgPT09ICd3aW5kb3cnKSB7XG4gICAgICAgIHRoaXMuX3VzZWRJblJ1bnRpbWUgPSB0cnVlO1xuICAgICAgfVxuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIC8vIEhhbmRsZXMgdXNhZ2VzIG9mIHBsYWluIGlkZW50aWZpZXIgd2l0aCB0aGUgbmFtZSBcIkhhbW1lclwiLiBUaGVzZSB1c2FnZVxuICAgIC8vIGFyZSB2YWxpZCBpZiB0aGV5IHJlc29sdmUgdG8gXCJAdHlwZXMvaGFtbWVyanNcIi4gZS5nLiBcIm5ldyBIYW1tZXIobXlFbGVtZW50KVwiLlxuICAgIGlmICh0cy5pc0lkZW50aWZpZXIobm9kZSkgJiYgbm9kZS50ZXh0ID09PSAnSGFtbWVyJyAmJlxuICAgICAgICAhdHMuaXNQcm9wZXJ0eUFjY2Vzc0V4cHJlc3Npb24obm9kZS5wYXJlbnQpICYmICF0cy5pc0VsZW1lbnRBY2Nlc3NFeHByZXNzaW9uKG5vZGUucGFyZW50KSkge1xuICAgICAgY29uc3Qgc3ltYm9sID0gdGhpcy5fZ2V0RGVjbGFyYXRpb25TeW1ib2xPZk5vZGUobm9kZSk7XG4gICAgICBpZiAoc3ltYm9sICYmIHN5bWJvbC52YWx1ZURlY2xhcmF0aW9uICYmXG4gICAgICAgICAgc3ltYm9sLnZhbHVlRGVjbGFyYXRpb24uZ2V0U291cmNlRmlsZSgpLmZpbGVOYW1lLmluY2x1ZGVzKCdAdHlwZXMvaGFtbWVyanMnKSkge1xuICAgICAgICB0aGlzLl91c2VkSW5SdW50aW1lID0gdHJ1ZTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogQ2hlY2tzIGlmIHRoZSBnaXZlbiBub2RlIHJlZmVyZW5jZXMgdGhlIGdlc3R1cmUgY29uZmlnIGZyb20gQW5ndWxhciBNYXRlcmlhbC5cbiAgICogSWYgc28sIHdlIGtlZXAgdHJhY2sgb2YgdGhlIGZvdW5kIHN5bWJvbCByZWZlcmVuY2UuXG4gICAqL1xuICBwcml2YXRlIF9jaGVja0Zvck1hdGVyaWFsR2VzdHVyZUNvbmZpZyhub2RlOiB0cy5Ob2RlKSB7XG4gICAgaWYgKHRzLmlzSWRlbnRpZmllcihub2RlKSkge1xuICAgICAgY29uc3QgaW1wb3J0RGF0YSA9IGdldEltcG9ydE9mSWRlbnRpZmllcihub2RlLCB0aGlzLnR5cGVDaGVja2VyKTtcbiAgICAgIGlmIChpbXBvcnREYXRhICYmIGltcG9ydERhdGEuc3ltYm9sTmFtZSA9PT0gR0VTVFVSRV9DT05GSUdfQ0xBU1NfTkFNRSAmJlxuICAgICAgICAgIGltcG9ydERhdGEubW9kdWxlTmFtZS5zdGFydHNXaXRoKCdAYW5ndWxhci9tYXRlcmlhbC8nKSkge1xuICAgICAgICB0aGlzLl9nZXN0dXJlQ29uZmlnUmVmZXJlbmNlcy5wdXNoKFxuICAgICAgICAgICAge25vZGUsIGltcG9ydERhdGEsIGlzSW1wb3J0OiB0cy5pc0ltcG9ydFNwZWNpZmllcihub2RlLnBhcmVudCl9KTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogQ2hlY2tzIGlmIHRoZSBnaXZlbiBIYW1tZXIgZ2VzdHVyZSBjb25maWcgdG9rZW4gcmVmZXJlbmNlIGlzIHBhcnQgb2YgYW5cbiAgICogQW5ndWxhciBwcm92aWRlciBkZWZpbml0aW9uIHRoYXQgc2V0cyB1cCBhIGN1c3RvbSBnZXN0dXJlIGNvbmZpZy5cbiAgICovXG4gIHByaXZhdGUgX2NoZWNrRm9yQ3VzdG9tR2VzdHVyZUNvbmZpZ1NldHVwKHRva2VuUmVmOiBJZGVudGlmaWVyUmVmZXJlbmNlKTogYm9vbGVhbiB7XG4gICAgLy8gV2FsayB1cCB0aGUgdHJlZSB0byBsb29rIGZvciBhIHBhcmVudCBwcm9wZXJ0eSBhc3NpZ25tZW50IG9mIHRoZVxuICAgIC8vIHJlZmVyZW5jZSB0byB0aGUgaGFtbWVyIGdlc3R1cmUgY29uZmlnIHRva2VuLlxuICAgIGxldCBwcm9wZXJ0eUFzc2lnbm1lbnQ6IHRzLk5vZGUgPSB0b2tlblJlZi5ub2RlO1xuICAgIHdoaWxlIChwcm9wZXJ0eUFzc2lnbm1lbnQgJiYgIXRzLmlzUHJvcGVydHlBc3NpZ25tZW50KHByb3BlcnR5QXNzaWdubWVudCkpIHtcbiAgICAgIHByb3BlcnR5QXNzaWdubWVudCA9IHByb3BlcnR5QXNzaWdubWVudC5wYXJlbnQ7XG4gICAgfVxuXG4gICAgaWYgKCFwcm9wZXJ0eUFzc2lnbm1lbnQgfHwgIXRzLmlzUHJvcGVydHlBc3NpZ25tZW50KHByb3BlcnR5QXNzaWdubWVudCkgfHxcbiAgICAgICAgZ2V0UHJvcGVydHlOYW1lVGV4dChwcm9wZXJ0eUFzc2lnbm1lbnQubmFtZSkgIT09ICdwcm92aWRlJykge1xuICAgICAgcmV0dXJuIGZhbHNlO1xuICAgIH1cblxuICAgIGNvbnN0IG9iamVjdExpdGVyYWxFeHByID0gcHJvcGVydHlBc3NpZ25tZW50LnBhcmVudDtcbiAgICBjb25zdCBtYXRjaGluZ0lkZW50aWZpZXJzID0gZmluZE1hdGNoaW5nQ2hpbGROb2RlcyhvYmplY3RMaXRlcmFsRXhwciwgdHMuaXNJZGVudGlmaWVyKTtcblxuICAgIC8vIFdlIG5haXZlbHkgYXNzdW1lIHRoYXQgaWYgdGhlcmUgaXMgYSByZWZlcmVuY2UgdG8gdGhlIFwiR2VzdHVyZUNvbmZpZ1wiIGV4cG9ydFxuICAgIC8vIGZyb20gQW5ndWxhciBNYXRlcmlhbCBpbiB0aGUgcHJvdmlkZXIgbGl0ZXJhbCwgdGhhdCB0aGUgcHJvdmlkZXIgc2V0cyB1cCB0aGVcbiAgICAvLyBBbmd1bGFyIE1hdGVyaWFsIGdlc3R1cmUgY29uZmlnLlxuICAgIHJldHVybiAhdGhpcy5fZ2VzdHVyZUNvbmZpZ1JlZmVyZW5jZXMuc29tZShyID0+IG1hdGNoaW5nSWRlbnRpZmllcnMuaW5jbHVkZXMoci5ub2RlKSk7XG4gIH1cblxuICAvKipcbiAgICogRGV0ZXJtaW5lcyBhbiBhdmFpbGFibGUgZmlsZSBuYW1lIGZvciB0aGUgZ2VzdHVyZSBjb25maWcgd2hpY2ggc2hvdWxkXG4gICAqIGJlIHN0b3JlZCBpbiB0aGUgc3BlY2lmaWVkIGZpbGUgcGF0aC5cbiAgICovXG4gIHByaXZhdGUgX2dldEF2YWlsYWJsZUdlc3R1cmVDb25maWdGaWxlTmFtZShzb3VyY2VSb290OiBEZXZraXRQYXRoKSB7XG4gICAgaWYgKCF0aGlzLmZpbGVTeXN0ZW0uZXhpc3RzKGRldmtpdEpvaW4oc291cmNlUm9vdCwgYCR7R0VTVFVSRV9DT05GSUdfRklMRV9OQU1FfS50c2ApKSkge1xuICAgICAgcmV0dXJuIGAke0dFU1RVUkVfQ09ORklHX0ZJTEVfTkFNRX0udHNgO1xuICAgIH1cblxuICAgIGxldCBwb3NzaWJsZU5hbWUgPSBgJHtHRVNUVVJFX0NPTkZJR19GSUxFX05BTUV9LWA7XG4gICAgbGV0IGluZGV4ID0gMTtcbiAgICB3aGlsZSAodGhpcy5maWxlU3lzdGVtLmV4aXN0cyhkZXZraXRKb2luKHNvdXJjZVJvb3QsIGAke3Bvc3NpYmxlTmFtZX0tJHtpbmRleH0udHNgKSkpIHtcbiAgICAgIGluZGV4Kys7XG4gICAgfVxuICAgIHJldHVybiBgJHtwb3NzaWJsZU5hbWUgKyBpbmRleH0udHNgO1xuICB9XG5cbiAgLyoqIFJlcGxhY2VzIGEgZ2l2ZW4gZ2VzdHVyZSBjb25maWcgcmVmZXJlbmNlIHdpdGggYSBuZXcgaW1wb3J0LiAqL1xuICBwcml2YXRlIF9yZXBsYWNlR2VzdHVyZUNvbmZpZ1JlZmVyZW5jZShcbiAgICAgIHtub2RlLCBpbXBvcnREYXRhLCBpc0ltcG9ydH06IElkZW50aWZpZXJSZWZlcmVuY2UsIHN5bWJvbE5hbWU6IHN0cmluZyxcbiAgICAgIG1vZHVsZVNwZWNpZmllcjogc3RyaW5nKSB7XG4gICAgY29uc3Qgc291cmNlRmlsZSA9IG5vZGUuZ2V0U291cmNlRmlsZSgpO1xuICAgIGNvbnN0IHJlY29yZGVyID0gdGhpcy5maWxlU3lzdGVtLmVkaXQodGhpcy5maWxlU3lzdGVtLnJlc29sdmUoc291cmNlRmlsZS5maWxlTmFtZSkpO1xuXG4gICAgLy8gTGlzdCBvZiBhbGwgaWRlbnRpZmllcnMgcmVmZXJyaW5nIHRvIHRoZSBnZXN0dXJlIGNvbmZpZyBpbiB0aGUgY3VycmVudCBmaWxlLiBUaGlzXG4gICAgLy8gYWxsb3dzIHVzIHRvIGFkZCBhbiBpbXBvcnQgZm9yIHRoZSBjb3BpZWQgZ2VzdHVyZSBjb25maWd1cmF0aW9uIHdpdGhvdXQgZ2VuZXJhdGluZyBhXG4gICAgLy8gbmV3IGlkZW50aWZpZXIgZm9yIHRoZSBpbXBvcnQgdG8gYXZvaWQgY29sbGlzaW9ucy4gaS5lLiBcIkdlc3R1cmVDb25maWdfMVwiLiBUaGUgaW1wb3J0XG4gICAgLy8gbWFuYWdlciBjaGVja3MgZm9yIHBvc3NpYmxlIG5hbWUgY29sbGlzaW9ucywgYnV0IGlzIGFibGUgdG8gaWdub3JlIHNwZWNpZmljIGlkZW50aWZpZXJzLlxuICAgIC8vIFdlIHVzZSB0aGlzIHRvIGlnbm9yZSBhbGwgcmVmZXJlbmNlcyB0byB0aGUgb3JpZ2luYWwgQW5ndWxhciBNYXRlcmlhbCBnZXN0dXJlIGNvbmZpZyxcbiAgICAvLyBiZWNhdXNlIHRoZXNlIHdpbGwgYmUgcmVwbGFjZWQgYW5kIHRoZXJlZm9yZSB3aWxsIG5vdCBpbnRlcmZlcmUuXG4gICAgY29uc3QgZ2VzdHVyZUlkZW50aWZpZXJzSW5GaWxlID0gdGhpcy5fZ2V0R2VzdHVyZUNvbmZpZ0lkZW50aWZpZXJzT2ZGaWxlKHNvdXJjZUZpbGUpO1xuXG4gICAgLy8gSWYgdGhlIHBhcmVudCBvZiB0aGUgaWRlbnRpZmllciBpcyBhY2Nlc3NlZCB0aHJvdWdoIGEgbmFtZXNwYWNlLCB3ZSBjYW4ganVzdFxuICAgIC8vIGltcG9ydCB0aGUgbmV3IGdlc3R1cmUgY29uZmlnIHdpdGhvdXQgcmV3cml0aW5nIHRoZSBpbXBvcnQgZGVjbGFyYXRpb24gYmVjYXVzZVxuICAgIC8vIHRoZSBjb25maWcgaGFzIGJlZW4gaW1wb3J0ZWQgdGhyb3VnaCBhIG5hbWVzcGFjZWQgaW1wb3J0LlxuICAgIGlmIChpc05hbWVzcGFjZWRJZGVudGlmaWVyQWNjZXNzKG5vZGUpKSB7XG4gICAgICBjb25zdCBuZXdFeHByZXNzaW9uID0gdGhpcy5faW1wb3J0TWFuYWdlci5hZGRJbXBvcnRUb1NvdXJjZUZpbGUoXG4gICAgICAgICAgc291cmNlRmlsZSwgc3ltYm9sTmFtZSwgbW9kdWxlU3BlY2lmaWVyLCBmYWxzZSwgZ2VzdHVyZUlkZW50aWZpZXJzSW5GaWxlKTtcblxuICAgICAgcmVjb3JkZXIucmVtb3ZlKG5vZGUucGFyZW50LmdldFN0YXJ0KCksIG5vZGUucGFyZW50LmdldFdpZHRoKCkpO1xuICAgICAgcmVjb3JkZXIuaW5zZXJ0UmlnaHQobm9kZS5wYXJlbnQuZ2V0U3RhcnQoKSwgdGhpcy5fcHJpbnROb2RlKG5ld0V4cHJlc3Npb24sIHNvdXJjZUZpbGUpKTtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICAvLyBEZWxldGUgdGhlIG9sZCBpbXBvcnQgdG8gdGhlIFwiR2VzdHVyZUNvbmZpZ1wiLlxuICAgIHRoaXMuX2ltcG9ydE1hbmFnZXIuZGVsZXRlTmFtZWRCaW5kaW5nSW1wb3J0KFxuICAgICAgICBzb3VyY2VGaWxlLCBHRVNUVVJFX0NPTkZJR19DTEFTU19OQU1FLCBpbXBvcnREYXRhLm1vZHVsZU5hbWUpO1xuXG4gICAgLy8gSWYgdGhlIGN1cnJlbnQgcmVmZXJlbmNlIGlzIG5vdCBmcm9tIGluc2lkZSBvZiBhIGltcG9ydCwgd2UgbmVlZCB0byBhZGQgYSBuZXdcbiAgICAvLyBpbXBvcnQgdG8gdGhlIGNvcGllZCBnZXN0dXJlIGNvbmZpZyBhbmQgcmVwbGFjZSB0aGUgaWRlbnRpZmllci4gRm9yIHJlZmVyZW5jZXNcbiAgICAvLyB3aXRoaW4gYW4gaW1wb3J0LCB3ZSBkbyBub3RoaW5nIGJ1dCByZW1vdmluZyB0aGUgYWN0dWFsIGltcG9ydC4gVGhpcyBhbGxvd3MgdXNcbiAgICAvLyB0byByZW1vdmUgdW51c2VkIGltcG9ydHMgdG8gdGhlIE1hdGVyaWFsIGdlc3R1cmUgY29uZmlnLlxuICAgIGlmICghaXNJbXBvcnQpIHtcbiAgICAgIGNvbnN0IG5ld0V4cHJlc3Npb24gPSB0aGlzLl9pbXBvcnRNYW5hZ2VyLmFkZEltcG9ydFRvU291cmNlRmlsZShcbiAgICAgICAgICBzb3VyY2VGaWxlLCBzeW1ib2xOYW1lLCBtb2R1bGVTcGVjaWZpZXIsIGZhbHNlLCBnZXN0dXJlSWRlbnRpZmllcnNJbkZpbGUpO1xuXG4gICAgICByZWNvcmRlci5yZW1vdmUobm9kZS5nZXRTdGFydCgpLCBub2RlLmdldFdpZHRoKCkpO1xuICAgICAgcmVjb3JkZXIuaW5zZXJ0UmlnaHQobm9kZS5nZXRTdGFydCgpLCB0aGlzLl9wcmludE5vZGUobmV3RXhwcmVzc2lvbiwgc291cmNlRmlsZSkpO1xuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBSZW1vdmVzIGEgZ2l2ZW4gZ2VzdHVyZSBjb25maWcgcmVmZXJlbmNlIGFuZCBpdHMgY29ycmVzcG9uZGluZyBpbXBvcnQgZnJvbVxuICAgKiBpdHMgY29udGFpbmluZyBzb3VyY2UgZmlsZS4gSW1wb3J0cyB3aWxsIGJlIGFsd2F5cyByZW1vdmVkLCBidXQgaW4gc29tZSBjYXNlcyxcbiAgICogd2hlcmUgaXQncyBub3QgZ3VhcmFudGVlZCB0aGF0IGEgcmVtb3ZhbCBjYW4gYmUgcGVyZm9ybWVkIHNhZmVseSwgd2UganVzdFxuICAgKiBjcmVhdGUgYSBtaWdyYXRpb24gZmFpbHVyZSAoYW5kIGFkZCBhIFRPRE8gaWYgcG9zc2libGUpLlxuICAgKi9cbiAgcHJpdmF0ZSBfcmVtb3ZlR2VzdHVyZUNvbmZpZ1JlZmVyZW5jZSh7bm9kZSwgaW1wb3J0RGF0YSwgaXNJbXBvcnR9OiBJZGVudGlmaWVyUmVmZXJlbmNlKSB7XG4gICAgY29uc3Qgc291cmNlRmlsZSA9IG5vZGUuZ2V0U291cmNlRmlsZSgpO1xuICAgIGNvbnN0IHJlY29yZGVyID0gdGhpcy5maWxlU3lzdGVtLmVkaXQodGhpcy5maWxlU3lzdGVtLnJlc29sdmUoc291cmNlRmlsZS5maWxlTmFtZSkpO1xuICAgIC8vIE9ubHkgcmVtb3ZlIHRoZSBpbXBvcnQgZm9yIHRoZSBnZXN0dXJlIGNvbmZpZyBpZiB0aGUgZ2VzdHVyZSBjb25maWcgaGFzXG4gICAgLy8gYmVlbiBhY2Nlc3NlZCB0aHJvdWdoIGEgbm9uLW5hbWVzcGFjZWQgaWRlbnRpZmllciBhY2Nlc3MuXG4gICAgaWYgKCFpc05hbWVzcGFjZWRJZGVudGlmaWVyQWNjZXNzKG5vZGUpKSB7XG4gICAgICB0aGlzLl9pbXBvcnRNYW5hZ2VyLmRlbGV0ZU5hbWVkQmluZGluZ0ltcG9ydChcbiAgICAgICAgICBzb3VyY2VGaWxlLCBHRVNUVVJFX0NPTkZJR19DTEFTU19OQU1FLCBpbXBvcnREYXRhLm1vZHVsZU5hbWUpO1xuICAgIH1cblxuICAgIC8vIEZvciByZWZlcmVuY2VzIGZyb20gd2l0aGluIGFuIGltcG9ydCwgd2UgZG8gbm90IG5lZWQgdG8gZG8gYW55dGhpbmcgb3RoZXIgdGhhblxuICAgIC8vIHJlbW92aW5nIHRoZSBpbXBvcnQuIEZvciBvdGhlciByZWZlcmVuY2VzLCB3ZSByZW1vdmUgdGhlIGltcG9ydCBhbmQgdGhlIHJlZmVyZW5jZVxuICAgIC8vIGlkZW50aWZpZXIgaWYgdXNlZCBpbnNpZGUgb2YgYSBwcm92aWRlciBkZWZpbml0aW9uLlxuICAgIGlmIChpc0ltcG9ydCkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGNvbnN0IHByb3ZpZGVyQXNzaWdubWVudCA9IG5vZGUucGFyZW50O1xuXG4gICAgLy8gT25seSByZW1vdmUgcmVmZXJlbmNlcyB0byB0aGUgZ2VzdHVyZSBjb25maWcgd2hpY2ggYXJlIHBhcnQgb2YgYSBzdGF0aWNhbGx5XG4gICAgLy8gYW5hbHl6YWJsZSBwcm92aWRlciBkZWZpbml0aW9uLiBXZSBvbmx5IHN1cHBvcnQgdGhlIGNvbW1vbiBjYXNlIG9mIGEgZ2VzdHVyZVxuICAgIC8vIGNvbmZpZyBwcm92aWRlciBkZWZpbml0aW9uIHdoZXJlIHRoZSBjb25maWcgaXMgc2V0IHVwIHRocm91Z2ggXCJ1c2VDbGFzc1wiLlxuICAgIC8vIE90aGVyd2lzZSwgaXQncyBub3QgZ3VhcmFudGVlZCB0aGF0IHdlIGNhbiBzYWZlbHkgcmVtb3ZlIHRoZSBwcm92aWRlciBkZWZpbml0aW9uLlxuICAgIGlmICghdHMuaXNQcm9wZXJ0eUFzc2lnbm1lbnQocHJvdmlkZXJBc3NpZ25tZW50KSB8fFxuICAgICAgICBnZXRQcm9wZXJ0eU5hbWVUZXh0KHByb3ZpZGVyQXNzaWdubWVudC5uYW1lKSAhPT0gJ3VzZUNsYXNzJykge1xuICAgICAgdGhpcy5fbm9kZUZhaWx1cmVzLnB1c2goe25vZGUsIG1lc3NhZ2U6IENBTk5PVF9SRU1PVkVfUkVGRVJFTkNFX0VSUk9SfSk7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgY29uc3Qgb2JqZWN0TGl0ZXJhbEV4cHIgPSBwcm92aWRlckFzc2lnbm1lbnQucGFyZW50O1xuICAgIGNvbnN0IHByb3ZpZGVUb2tlbiA9IG9iamVjdExpdGVyYWxFeHByLnByb3BlcnRpZXMuZmluZChcbiAgICAgICAgKHApOiBwIGlzIHRzLlByb3BlcnR5QXNzaWdubWVudCA9PlxuICAgICAgICAgICAgdHMuaXNQcm9wZXJ0eUFzc2lnbm1lbnQocCkgJiYgZ2V0UHJvcGVydHlOYW1lVGV4dChwLm5hbWUpID09PSAncHJvdmlkZScpO1xuXG4gICAgLy8gRG8gbm90IHJlbW92ZSB0aGUgcmVmZXJlbmNlIGlmIHRoZSBnZXN0dXJlIGNvbmZpZyBpcyBub3QgcGFydCBvZiBhIHByb3ZpZGVyIGRlZmluaXRpb24sXG4gICAgLy8gb3IgaWYgdGhlIHByb3ZpZGVkIHRva2UgaXMgbm90IHJlZmVycmluZyB0byB0aGUga25vd24gSEFNTUVSX0dFU1RVUkVfQ09ORklHIHRva2VuXG4gICAgLy8gZnJvbSBwbGF0Zm9ybS1icm93c2VyLlxuICAgIGlmICghcHJvdmlkZVRva2VuIHx8ICF0aGlzLl9pc1JlZmVyZW5jZVRvSGFtbWVyQ29uZmlnVG9rZW4ocHJvdmlkZVRva2VuLmluaXRpYWxpemVyKSkge1xuICAgICAgdGhpcy5fbm9kZUZhaWx1cmVzLnB1c2goe25vZGUsIG1lc3NhZ2U6IENBTk5PVF9SRU1PVkVfUkVGRVJFTkNFX0VSUk9SfSk7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgLy8gQ29sbGVjdCBhbGwgbmVzdGVkIGlkZW50aWZpZXJzIHdoaWNoIHdpbGwgYmUgZGVsZXRlZC4gVGhpcyBoZWxwcyB1c1xuICAgIC8vIGRldGVybWluaW5nIGlmIHdlIGNhbiByZW1vdmUgaW1wb3J0cyBmb3IgdGhlIFwiSEFNTUVSX0dFU1RVUkVfQ09ORklHXCIgdG9rZW4uXG4gICAgdGhpcy5fZGVsZXRlZElkZW50aWZpZXJzLnB1c2goLi4uZmluZE1hdGNoaW5nQ2hpbGROb2RlcyhvYmplY3RMaXRlcmFsRXhwciwgdHMuaXNJZGVudGlmaWVyKSk7XG5cbiAgICAvLyBJbiBjYXNlIHRoZSBmb3VuZCBwcm92aWRlciBkZWZpbml0aW9uIGlzIG5vdCBwYXJ0IG9mIGFuIGFycmF5IGxpdGVyYWwsXG4gICAgLy8gd2UgY2Fubm90IHNhZmVseSByZW1vdmUgdGhlIHByb3ZpZGVyLiBUaGlzIGlzIGJlY2F1c2UgaXQgY291bGQgYmUgZGVjbGFyZWRcbiAgICAvLyBhcyBhIHZhcmlhYmxlLiBlLmcuIFwiY29uc3QgZ2VzdHVyZVByb3ZpZGVyID0ge3Byb3ZpZGU6IC4uLCB1c2VDbGFzczogR2VzdHVyZUNvbmZpZ31cIi5cbiAgICAvLyBJbiB0aGF0IGNhc2UsIHdlIGp1c3QgYWRkIGFuIGVtcHR5IG9iamVjdCBsaXRlcmFsIHdpdGggVE9ETyBhbmQgcHJpbnQgYSBmYWlsdXJlLlxuICAgIGlmICghdHMuaXNBcnJheUxpdGVyYWxFeHByZXNzaW9uKG9iamVjdExpdGVyYWxFeHByLnBhcmVudCkpIHtcbiAgICAgIHJlY29yZGVyLnJlbW92ZShvYmplY3RMaXRlcmFsRXhwci5nZXRTdGFydCgpLCBvYmplY3RMaXRlcmFsRXhwci5nZXRXaWR0aCgpKTtcbiAgICAgIHJlY29yZGVyLmluc2VydFJpZ2h0KG9iamVjdExpdGVyYWxFeHByLmdldFN0YXJ0KCksIGAvKiBUT0RPOiByZW1vdmUgKi8ge31gKTtcbiAgICAgIHRoaXMuX25vZGVGYWlsdXJlcy5wdXNoKHtcbiAgICAgICAgbm9kZTogb2JqZWN0TGl0ZXJhbEV4cHIsXG4gICAgICAgIG1lc3NhZ2U6IGBVbmFibGUgdG8gZGVsZXRlIHByb3ZpZGVyIGRlZmluaXRpb24gZm9yIFwiR2VzdHVyZUNvbmZpZ1wiIGNvbXBsZXRlbHkuIGAgK1xuICAgICAgICAgICAgYFBsZWFzZSBjbGVhbiB1cCB0aGUgcHJvdmlkZXIuYFxuICAgICAgfSk7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgLy8gUmVtb3ZlcyB0aGUgb2JqZWN0IGxpdGVyYWwgZnJvbSB0aGUgcGFyZW50IGFycmF5IGV4cHJlc3Npb24uIFJlbW92ZXNcbiAgICAvLyB0aGUgdHJhaWxpbmcgY29tbWEgdG9rZW4gaWYgcHJlc2VudC5cbiAgICByZW1vdmVFbGVtZW50RnJvbUFycmF5RXhwcmVzc2lvbihvYmplY3RMaXRlcmFsRXhwciwgcmVjb3JkZXIpO1xuICB9XG5cbiAgLyoqIFJlbW92ZXMgdGhlIGdpdmVuIGhhbW1lciBjb25maWcgdG9rZW4gaW1wb3J0IGlmIGl0IGlzIG5vdCB1c2VkLiAqL1xuICBwcml2YXRlIF9yZW1vdmVIYW1tZXJDb25maWdUb2tlbkltcG9ydElmVW51c2VkKHtub2RlLCBpbXBvcnREYXRhfTogSWRlbnRpZmllclJlZmVyZW5jZSkge1xuICAgIGNvbnN0IHNvdXJjZUZpbGUgPSBub2RlLmdldFNvdXJjZUZpbGUoKTtcbiAgICBjb25zdCBpc1Rva2VuVXNlZCA9IHRoaXMuX2hhbW1lckNvbmZpZ1Rva2VuUmVmZXJlbmNlcy5zb21lKFxuICAgICAgICByID0+ICFyLmlzSW1wb3J0ICYmICFpc05hbWVzcGFjZWRJZGVudGlmaWVyQWNjZXNzKHIubm9kZSkgJiZcbiAgICAgICAgICAgIHIubm9kZS5nZXRTb3VyY2VGaWxlKCkgPT09IHNvdXJjZUZpbGUgJiYgIXRoaXMuX2RlbGV0ZWRJZGVudGlmaWVycy5pbmNsdWRlcyhyLm5vZGUpKTtcblxuICAgIC8vIFdlIGRvbid0IHdhbnQgdG8gcmVtb3ZlIHRoZSBpbXBvcnQgZm9yIHRoZSB0b2tlbiBpZiB0aGUgdG9rZW4gaXNcbiAgICAvLyBzdGlsbCB1c2VkIHNvbWV3aGVyZS5cbiAgICBpZiAoIWlzVG9rZW5Vc2VkKSB7XG4gICAgICB0aGlzLl9pbXBvcnRNYW5hZ2VyLmRlbGV0ZU5hbWVkQmluZGluZ0ltcG9ydChcbiAgICAgICAgICBzb3VyY2VGaWxlLCBIQU1NRVJfQ09ORklHX1RPS0VOX05BTUUsIGltcG9ydERhdGEubW9kdWxlTmFtZSk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFJlbW92ZXMgSGFtbWVyIGZyb20gYWxsIGluZGV4IEhUTUwgZmlsZXMgb2YgdGhlIGN1cnJlbnQgcHJvamVjdC4gKi9cbiAgcHJpdmF0ZSBfcmVtb3ZlSGFtbWVyRnJvbUluZGV4RmlsZSgpIHtcbiAgICBjb25zdCBpbmRleEZpbGVQYXRocyA9IGdldFByb2plY3RJbmRleEZpbGVzKHRoaXMuY29udGV4dC5wcm9qZWN0KTtcbiAgICBpbmRleEZpbGVQYXRocy5mb3JFYWNoKGZpbGVQYXRoID0+IHtcbiAgICAgIGlmICghdGhpcy5maWxlU3lzdGVtLmV4aXN0cyhmaWxlUGF0aCkpIHtcbiAgICAgICAgcmV0dXJuO1xuICAgICAgfVxuXG4gICAgICBjb25zdCBodG1sQ29udGVudCA9IHRoaXMuZmlsZVN5c3RlbS5yZWFkKGZpbGVQYXRoKSE7XG4gICAgICBjb25zdCByZWNvcmRlciA9IHRoaXMuZmlsZVN5c3RlbS5lZGl0KGZpbGVQYXRoKTtcblxuICAgICAgZmluZEhhbW1lclNjcmlwdEltcG9ydEVsZW1lbnRzKGh0bWxDb250ZW50KVxuICAgICAgICAgIC5mb3JFYWNoKGVsID0+IHJlbW92ZUVsZW1lbnRGcm9tSHRtbChlbCwgcmVjb3JkZXIpKTtcbiAgICB9KTtcbiAgfVxuXG4gIC8qKiBTZXRzIHVwIHRoZSBIYW1tZXIgZ2VzdHVyZSBjb25maWcgaW4gdGhlIHJvb3QgbW9kdWxlIGlmIG5lZWRlZC4gKi9cbiAgcHJpdmF0ZSBfc2V0dXBOZXdHZXN0dXJlQ29uZmlnSW5Sb290TW9kdWxlKGdlc3R1cmVDb25maWdQYXRoOiBzdHJpbmcpIHtcbiAgICBjb25zdCB7d29ya3NwYWNlRnNQYXRoLCBwcm9qZWN0fSA9IHRoaXMuY29udGV4dDtcbiAgICBjb25zdCBtYWluRmlsZVBhdGggPSBnZXRQcm9qZWN0TWFpbkZpbGUocHJvamVjdCk7XG4gICAgY29uc3Qgcm9vdE1vZHVsZVN5bWJvbCA9IHRoaXMuX2dldFJvb3RNb2R1bGVTeW1ib2wobWFpbkZpbGVQYXRoKTtcblxuICAgIGlmIChyb290TW9kdWxlU3ltYm9sID09PSBudWxsKSB7XG4gICAgICB0aGlzLmZhaWx1cmVzLnB1c2goe1xuICAgICAgICBmaWxlUGF0aDogbWFpbkZpbGVQYXRoLFxuICAgICAgICBtZXNzYWdlOiBgQ291bGQgbm90IHNldHVwIEhhbW1lciBnZXN0dXJlcyBpbiBtb2R1bGUuIFBsZWFzZSBgICtcbiAgICAgICAgICAgIGBtYW51YWxseSBlbnN1cmUgdGhhdCB0aGUgSGFtbWVyIGdlc3R1cmUgY29uZmlnIGlzIHNldCB1cC5gLFxuICAgICAgfSk7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgY29uc3Qgc291cmNlRmlsZSA9IHJvb3RNb2R1bGVTeW1ib2wudmFsdWVEZWNsYXJhdGlvbi5nZXRTb3VyY2VGaWxlKCk7XG4gICAgY29uc3QgcmVsYXRpdmVQYXRoID0gcmVsYXRpdmUod29ya3NwYWNlRnNQYXRoLCBzb3VyY2VGaWxlLmZpbGVOYW1lKTtcbiAgICBjb25zdCBtZXRhZGF0YSA9IGdldERlY29yYXRvck1ldGFkYXRhKHNvdXJjZUZpbGUsICdOZ01vZHVsZScsICdAYW5ndWxhci9jb3JlJykgYXNcbiAgICAgICAgdHMuT2JqZWN0TGl0ZXJhbEV4cHJlc3Npb25bXTtcblxuICAgIC8vIElmIG5vIFwiTmdNb2R1bGVcIiBkZWZpbml0aW9uIGlzIGZvdW5kIGluc2lkZSB0aGUgc291cmNlIGZpbGUsIHdlIGp1c3QgZG8gbm90aGluZy5cbiAgICBpZiAoIW1ldGFkYXRhLmxlbmd0aCkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGNvbnN0IHJlY29yZGVyID0gdGhpcy5maWxlU3lzdGVtLmVkaXQodGhpcy5maWxlU3lzdGVtLnJlc29sdmUoc291cmNlRmlsZS5maWxlTmFtZSkpO1xuICAgIGNvbnN0IHByb3ZpZGVyc0ZpZWxkID0gZ2V0TWV0YWRhdGFGaWVsZChtZXRhZGF0YVswXSwgJ3Byb3ZpZGVycycpWzBdO1xuICAgIGNvbnN0IHByb3ZpZGVySWRlbnRpZmllcnMgPVxuICAgICAgICBwcm92aWRlcnNGaWVsZCA/IGZpbmRNYXRjaGluZ0NoaWxkTm9kZXMocHJvdmlkZXJzRmllbGQsIHRzLmlzSWRlbnRpZmllcikgOiBudWxsO1xuICAgIGNvbnN0IGdlc3R1cmVDb25maWdFeHByID0gdGhpcy5faW1wb3J0TWFuYWdlci5hZGRJbXBvcnRUb1NvdXJjZUZpbGUoXG4gICAgICAgIHNvdXJjZUZpbGUsIEdFU1RVUkVfQ09ORklHX0NMQVNTX05BTUUsXG4gICAgICAgIGdldE1vZHVsZVNwZWNpZmllcihnZXN0dXJlQ29uZmlnUGF0aCwgc291cmNlRmlsZS5maWxlTmFtZSksIGZhbHNlLFxuICAgICAgICB0aGlzLl9nZXRHZXN0dXJlQ29uZmlnSWRlbnRpZmllcnNPZkZpbGUoc291cmNlRmlsZSkpO1xuICAgIGNvbnN0IGhhbW1lckNvbmZpZ1Rva2VuRXhwciA9IHRoaXMuX2ltcG9ydE1hbmFnZXIuYWRkSW1wb3J0VG9Tb3VyY2VGaWxlKFxuICAgICAgICBzb3VyY2VGaWxlLCBIQU1NRVJfQ09ORklHX1RPS0VOX05BTUUsIEhBTU1FUl9DT05GSUdfVE9LRU5fTU9EVUxFKTtcbiAgICBjb25zdCBuZXdQcm92aWRlck5vZGUgPSB0cy5jcmVhdGVPYmplY3RMaXRlcmFsKFtcbiAgICAgIHRzLmNyZWF0ZVByb3BlcnR5QXNzaWdubWVudCgncHJvdmlkZScsIGhhbW1lckNvbmZpZ1Rva2VuRXhwciksXG4gICAgICB0cy5jcmVhdGVQcm9wZXJ0eUFzc2lnbm1lbnQoJ3VzZUNsYXNzJywgZ2VzdHVyZUNvbmZpZ0V4cHIpXG4gICAgXSk7XG5cbiAgICAvLyBJZiB0aGUgcHJvdmlkZXJzIGZpZWxkIGV4aXN0cyBhbmQgYWxyZWFkeSBjb250YWlucyByZWZlcmVuY2VzIHRvIHRoZSBoYW1tZXIgZ2VzdHVyZVxuICAgIC8vIGNvbmZpZyB0b2tlbiBhbmQgdGhlIGdlc3R1cmUgY29uZmlnLCB3ZSBuYWl2ZWx5IGFzc3VtZSB0aGF0IHRoZSBnZXN0dXJlIGNvbmZpZyBpc1xuICAgIC8vIGFscmVhZHkgc2V0IHVwLiBXZSBvbmx5IHdhbnQgdG8gYWRkIHRoZSBnZXN0dXJlIGNvbmZpZyBwcm92aWRlciBpZiBpdCBpcyBub3Qgc2V0IHVwLlxuICAgIGlmICghcHJvdmlkZXJJZGVudGlmaWVycyB8fFxuICAgICAgICAhKHRoaXMuX2hhbW1lckNvbmZpZ1Rva2VuUmVmZXJlbmNlcy5zb21lKHIgPT4gcHJvdmlkZXJJZGVudGlmaWVycy5pbmNsdWRlcyhyLm5vZGUpKSAmJlxuICAgICAgICAgIHRoaXMuX2dlc3R1cmVDb25maWdSZWZlcmVuY2VzLnNvbWUociA9PiBwcm92aWRlcklkZW50aWZpZXJzLmluY2x1ZGVzKHIubm9kZSkpKSkge1xuICAgICAgYWRkU3ltYm9sVG9OZ01vZHVsZU1ldGFkYXRhKFxuICAgICAgICAgIHNvdXJjZUZpbGUsIHJlbGF0aXZlUGF0aCwgJ3Byb3ZpZGVycycsIHRoaXMuX3ByaW50Tm9kZShuZXdQcm92aWRlck5vZGUsIHNvdXJjZUZpbGUpLCBudWxsKVxuICAgICAgICAgIC5mb3JFYWNoKGNoYW5nZSA9PiB7XG4gICAgICAgICAgICBpZiAoY2hhbmdlIGluc3RhbmNlb2YgSW5zZXJ0Q2hhbmdlKSB7XG4gICAgICAgICAgICAgIHJlY29yZGVyLmluc2VydFJpZ2h0KGNoYW5nZS5wb3MsIGNoYW5nZS50b0FkZCk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgfSk7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgdGhlIFR5cGVTY3JpcHQgc3ltYm9sIG9mIHRoZSByb290IG1vZHVsZSBieSBsb29raW5nIGZvciB0aGUgbW9kdWxlXG4gICAqIGJvb3RzdHJhcCBleHByZXNzaW9uIGluIHRoZSBzcGVjaWZpZWQgc291cmNlIGZpbGUuXG4gICAqL1xuICBwcml2YXRlIF9nZXRSb290TW9kdWxlU3ltYm9sKG1haW5GaWxlUGF0aDogUGF0aCk6IHRzLlN5bWJvbHxudWxsIHtcbiAgICBjb25zdCBtYWluRmlsZSA9IHRoaXMucHJvZ3JhbS5nZXRTb3VyY2VGaWxlKHJlc29sdmUoXG4gICAgICAgIHRoaXMuY29udGV4dC53b3Jrc3BhY2VGc1BhdGgsIG1haW5GaWxlUGF0aCkpO1xuICAgIGlmICghbWFpbkZpbGUpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIGNvbnN0IGFwcE1vZHVsZUV4cHIgPSBmaW5kTWFpbk1vZHVsZUV4cHJlc3Npb24obWFpbkZpbGUpO1xuICAgIGlmICghYXBwTW9kdWxlRXhwcikge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuXG4gICAgY29uc3QgYXBwTW9kdWxlU3ltYm9sID0gdGhpcy5fZ2V0RGVjbGFyYXRpb25TeW1ib2xPZk5vZGUodW53cmFwRXhwcmVzc2lvbihhcHBNb2R1bGVFeHByKSk7XG4gICAgaWYgKCFhcHBNb2R1bGVTeW1ib2wgfHwgIWFwcE1vZHVsZVN5bWJvbC52YWx1ZURlY2xhcmF0aW9uKSB7XG4gICAgICByZXR1cm4gbnVsbDtcbiAgICB9XG4gICAgcmV0dXJuIGFwcE1vZHVsZVN5bWJvbDtcbiAgfVxuXG4gIC8qKiBTZXRzIHVwIHRoZSBcIkhhbW1lck1vZHVsZVwiIGluIHRoZSByb290IG1vZHVsZSBvZiB0aGUgY3VycmVudCBwcm9qZWN0LiAqL1xuICBwcml2YXRlIF9zZXR1cEhhbW1lck1vZHVsZUluUm9vdE1vZHVsZSgpIHtcbiAgICBjb25zdCB7d29ya3NwYWNlRnNQYXRoLCBwcm9qZWN0fSA9IHRoaXMuY29udGV4dDtcbiAgICBjb25zdCBtYWluRmlsZVBhdGggPSBnZXRQcm9qZWN0TWFpbkZpbGUocHJvamVjdCk7XG4gICAgY29uc3Qgcm9vdE1vZHVsZVN5bWJvbCA9IHRoaXMuX2dldFJvb3RNb2R1bGVTeW1ib2wobWFpbkZpbGVQYXRoKTtcblxuICAgIGlmIChyb290TW9kdWxlU3ltYm9sID09PSBudWxsKSB7XG4gICAgICB0aGlzLmZhaWx1cmVzLnB1c2goe1xuICAgICAgICBmaWxlUGF0aDogbWFpbkZpbGVQYXRoLFxuICAgICAgICBtZXNzYWdlOiBgQ291bGQgbm90IHNldHVwIEhhbW1lck1vZHVsZS4gUGxlYXNlIG1hbnVhbGx5IHNldCB1cCB0aGUgXCJIYW1tZXJNb2R1bGVcIiBgICtcbiAgICAgICAgICAgIGBmcm9tIFwiQGFuZ3VsYXIvcGxhdGZvcm0tYnJvd3NlclwiLmAsXG4gICAgICB9KTtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICBjb25zdCBzb3VyY2VGaWxlID0gcm9vdE1vZHVsZVN5bWJvbC52YWx1ZURlY2xhcmF0aW9uLmdldFNvdXJjZUZpbGUoKTtcbiAgICBjb25zdCByZWxhdGl2ZVBhdGggPSByZWxhdGl2ZSh3b3Jrc3BhY2VGc1BhdGgsIHNvdXJjZUZpbGUuZmlsZU5hbWUpO1xuICAgIGNvbnN0IG1ldGFkYXRhID0gZ2V0RGVjb3JhdG9yTWV0YWRhdGEoc291cmNlRmlsZSwgJ05nTW9kdWxlJywgJ0Bhbmd1bGFyL2NvcmUnKSBhc1xuICAgICAgICB0cy5PYmplY3RMaXRlcmFsRXhwcmVzc2lvbltdO1xuICAgIGlmICghbWV0YWRhdGEubGVuZ3RoKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgY29uc3QgaW1wb3J0c0ZpZWxkID0gZ2V0TWV0YWRhdGFGaWVsZChtZXRhZGF0YVswXSwgJ2ltcG9ydHMnKVswXTtcbiAgICBjb25zdCBpbXBvcnRJZGVudGlmaWVycyA9XG4gICAgICAgIGltcG9ydHNGaWVsZCA/IGZpbmRNYXRjaGluZ0NoaWxkTm9kZXMoaW1wb3J0c0ZpZWxkLCB0cy5pc0lkZW50aWZpZXIpIDogbnVsbDtcbiAgICBjb25zdCByZWNvcmRlciA9IHRoaXMuZmlsZVN5c3RlbS5lZGl0KHRoaXMuZmlsZVN5c3RlbS5yZXNvbHZlKHNvdXJjZUZpbGUuZmlsZU5hbWUpKTtcbiAgICBjb25zdCBoYW1tZXJNb2R1bGVFeHByID0gdGhpcy5faW1wb3J0TWFuYWdlci5hZGRJbXBvcnRUb1NvdXJjZUZpbGUoXG4gICAgICAgIHNvdXJjZUZpbGUsIEhBTU1FUl9NT0RVTEVfTkFNRSwgSEFNTUVSX01PRFVMRV9JTVBPUlQpO1xuXG4gICAgLy8gSWYgdGhlIFwiSGFtbWVyTW9kdWxlXCIgaXMgbm90IGFscmVhZHkgaW1wb3J0ZWQgaW4gdGhlIGFwcCBtb2R1bGUsIHdlIHNldCBpdCB1cFxuICAgIC8vIGJ5IGFkZGluZyBpdCB0byB0aGUgXCJpbXBvcnRzXCIgZmllbGQgb2YgdGhlIGFwcCBtb2R1bGUuXG4gICAgaWYgKCFpbXBvcnRJZGVudGlmaWVycyB8fFxuICAgICAgICAhdGhpcy5faGFtbWVyTW9kdWxlUmVmZXJlbmNlcy5zb21lKHIgPT4gaW1wb3J0SWRlbnRpZmllcnMuaW5jbHVkZXMoci5ub2RlKSkpIHtcbiAgICAgIGFkZFN5bWJvbFRvTmdNb2R1bGVNZXRhZGF0YShcbiAgICAgICAgICBzb3VyY2VGaWxlLCByZWxhdGl2ZVBhdGgsICdpbXBvcnRzJywgdGhpcy5fcHJpbnROb2RlKGhhbW1lck1vZHVsZUV4cHIsIHNvdXJjZUZpbGUpLCBudWxsKVxuICAgICAgICAgIC5mb3JFYWNoKGNoYW5nZSA9PiB7XG4gICAgICAgICAgICBpZiAoY2hhbmdlIGluc3RhbmNlb2YgSW5zZXJ0Q2hhbmdlKSB7XG4gICAgICAgICAgICAgIHJlY29yZGVyLmluc2VydFJpZ2h0KGNoYW5nZS5wb3MsIGNoYW5nZS50b0FkZCk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgfSk7XG4gICAgfVxuICB9XG5cbiAgLyoqIFByaW50cyBhIGdpdmVuIG5vZGUgd2l0aGluIHRoZSBzcGVjaWZpZWQgc291cmNlIGZpbGUuICovXG4gIHByaXZhdGUgX3ByaW50Tm9kZShub2RlOiB0cy5Ob2RlLCBzb3VyY2VGaWxlOiB0cy5Tb3VyY2VGaWxlKTogc3RyaW5nIHtcbiAgICByZXR1cm4gdGhpcy5fcHJpbnRlci5wcmludE5vZGUodHMuRW1pdEhpbnQuVW5zcGVjaWZpZWQsIG5vZGUsIHNvdXJjZUZpbGUpO1xuICB9XG5cbiAgLyoqIEdldHMgYWxsIHJlZmVyZW5jZWQgZ2VzdHVyZSBjb25maWcgaWRlbnRpZmllcnMgb2YgYSBnaXZlbiBzb3VyY2UgZmlsZSAqL1xuICBwcml2YXRlIF9nZXRHZXN0dXJlQ29uZmlnSWRlbnRpZmllcnNPZkZpbGUoc291cmNlRmlsZTogdHMuU291cmNlRmlsZSk6IHRzLklkZW50aWZpZXJbXSB7XG4gICAgcmV0dXJuIHRoaXMuX2dlc3R1cmVDb25maWdSZWZlcmVuY2VzLmZpbHRlcihkID0+IGQubm9kZS5nZXRTb3VyY2VGaWxlKCkgPT09IHNvdXJjZUZpbGUpXG4gICAgICAgIC5tYXAoZCA9PiBkLm5vZGUpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIHN5bWJvbCB0aGF0IGNvbnRhaW5zIHRoZSB2YWx1ZSBkZWNsYXJhdGlvbiBvZiB0aGUgc3BlY2lmaWVkIG5vZGUuICovXG4gIHByaXZhdGUgX2dldERlY2xhcmF0aW9uU3ltYm9sT2ZOb2RlKG5vZGU6IHRzLk5vZGUpOiB0cy5TeW1ib2x8dW5kZWZpbmVkIHtcbiAgICBjb25zdCBzeW1ib2wgPSB0aGlzLnR5cGVDaGVja2VyLmdldFN5bWJvbEF0TG9jYXRpb24obm9kZSk7XG5cbiAgICAvLyBTeW1ib2xzIGNhbiBiZSBhbGlhc2VzIG9mIHRoZSBkZWNsYXJhdGlvbiBzeW1ib2wuIGUuZy4gaW4gbmFtZWQgaW1wb3J0IHNwZWNpZmllcnMuXG4gICAgLy8gV2UgbmVlZCB0byByZXNvbHZlIHRoZSBhbGlhc2VkIHN5bWJvbCBiYWNrIHRvIHRoZSBkZWNsYXJhdGlvbiBzeW1ib2wuXG4gICAgLy8gdHNsaW50OmRpc2FibGUtbmV4dC1saW5lOm5vLWJpdHdpc2VcbiAgICBpZiAoc3ltYm9sICYmIChzeW1ib2wuZmxhZ3MgJiB0cy5TeW1ib2xGbGFncy5BbGlhcykgIT09IDApIHtcbiAgICAgIHJldHVybiB0aGlzLnR5cGVDaGVja2VyLmdldEFsaWFzZWRTeW1ib2woc3ltYm9sKTtcbiAgICB9XG4gICAgcmV0dXJuIHN5bWJvbDtcbiAgfVxuXG4gIC8qKlxuICAgKiBDaGVja3Mgd2hldGhlciB0aGUgZ2l2ZW4gZXhwcmVzc2lvbiByZXNvbHZlcyB0byBhIGhhbW1lciBnZXN0dXJlIGNvbmZpZ1xuICAgKiB0b2tlbiByZWZlcmVuY2UgZnJvbSBcIkBhbmd1bGFyL3BsYXRmb3JtLWJyb3dzZXJcIi5cbiAgICovXG4gIHByaXZhdGUgX2lzUmVmZXJlbmNlVG9IYW1tZXJDb25maWdUb2tlbihleHByOiB0cy5FeHByZXNzaW9uKSB7XG4gICAgY29uc3QgdW53cmFwcGVkID0gdW53cmFwRXhwcmVzc2lvbihleHByKTtcbiAgICBpZiAodHMuaXNJZGVudGlmaWVyKHVud3JhcHBlZCkpIHtcbiAgICAgIHJldHVybiB0aGlzLl9oYW1tZXJDb25maWdUb2tlblJlZmVyZW5jZXMuc29tZShyID0+IHIubm9kZSA9PT0gdW53cmFwcGVkKTtcbiAgICB9IGVsc2UgaWYgKHRzLmlzUHJvcGVydHlBY2Nlc3NFeHByZXNzaW9uKHVud3JhcHBlZCkpIHtcbiAgICAgIHJldHVybiB0aGlzLl9oYW1tZXJDb25maWdUb2tlblJlZmVyZW5jZXMuc29tZShyID0+IHIubm9kZSA9PT0gdW53cmFwcGVkLm5hbWUpO1xuICAgIH1cbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cblxuICAvKipcbiAgICogQ3JlYXRlcyBtaWdyYXRpb24gZmFpbHVyZXMgb2YgdGhlIGNvbGxlY3RlZCBub2RlIGZhaWx1cmVzLiBUaGUgcmV0dXJuZWQgbWlncmF0aW9uXG4gICAqIGZhaWx1cmVzIGFyZSB1cGRhdGVkIHRvIHJlZmxlY3QgdGhlIHBvc3QtbWlncmF0aW9uIHN0YXRlIG9mIHNvdXJjZSBmaWxlcy4gTWVhbmluZ1xuICAgKiB0aGF0IGZhaWx1cmUgcG9zaXRpb25zIGFyZSBjb3JyZWN0ZWQgaWYgc291cmNlIGZpbGUgbW9kaWZpY2F0aW9ucyBzaGlmdGVkIGxpbmVzLlxuICAgKi9cbiAgcHJpdmF0ZSBfY3JlYXRlTWlncmF0aW9uRmFpbHVyZXMoKTogTWlncmF0aW9uRmFpbHVyZVtdIHtcbiAgICByZXR1cm4gdGhpcy5fbm9kZUZhaWx1cmVzLm1hcCgoe25vZGUsIG1lc3NhZ2V9KSA9PiB7XG4gICAgICBjb25zdCBzb3VyY2VGaWxlID0gbm9kZS5nZXRTb3VyY2VGaWxlKCk7XG4gICAgICBjb25zdCBvZmZzZXQgPSBub2RlLmdldFN0YXJ0KCk7XG4gICAgICBjb25zdCBwb3NpdGlvbiA9IHRzLmdldExpbmVBbmRDaGFyYWN0ZXJPZlBvc2l0aW9uKHNvdXJjZUZpbGUsIG5vZGUuZ2V0U3RhcnQoKSk7XG4gICAgICByZXR1cm4ge1xuICAgICAgICBwb3NpdGlvbjogdGhpcy5faW1wb3J0TWFuYWdlci5jb3JyZWN0Tm9kZVBvc2l0aW9uKG5vZGUsIG9mZnNldCwgcG9zaXRpb24pLFxuICAgICAgICBtZXNzYWdlOiBtZXNzYWdlLFxuICAgICAgICBmaWxlUGF0aDogdGhpcy5maWxlU3lzdGVtLnJlc29sdmUoc291cmNlRmlsZS5maWxlTmFtZSksXG4gICAgICB9O1xuICAgIH0pO1xuICB9XG5cbiAgLyoqIEdsb2JhbCBzdGF0ZSBvZiB3aGV0aGVyIEhhbW1lciBpcyB1c2VkIGluIGFueSBhbmFseXplZCBwcm9qZWN0IHRhcmdldC4gKi9cbiAgc3RhdGljIGdsb2JhbFVzZXNIYW1tZXIgPSBmYWxzZTtcblxuICAvKipcbiAgICogU3RhdGljIG1pZ3JhdGlvbiBydWxlIG1ldGhvZCB0aGF0IHdpbGwgYmUgY2FsbGVkIG9uY2UgYWxsIHByb2plY3QgdGFyZ2V0c1xuICAgKiBoYXZlIGJlZW4gbWlncmF0ZWQgaW5kaXZpZHVhbGx5LiBUaGlzIG1ldGhvZCBjYW4gYmUgdXNlZCB0byBtYWtlIGNoYW5nZXMgYmFzZWRcbiAgICogb24gdGhlIGFuYWx5c2lzIG9mIHRoZSBpbmRpdmlkdWFsIHRhcmdldHMuIEZvciBleGFtcGxlOiB3ZSBvbmx5IHJlbW92ZSBIYW1tZXJcbiAgICogZnJvbSB0aGUgXCJwYWNrYWdlLmpzb25cIiBpZiBpdCBpcyBub3QgdXNlZCBpbiAqYW55KiBwcm9qZWN0IHRhcmdldC5cbiAgICovXG4gIHN0YXRpYyBnbG9iYWxQb3N0TWlncmF0aW9uKHRyZWU6IFRyZWUsIGNvbnRleHQ6IFNjaGVtYXRpY0NvbnRleHQpOiBQb3N0TWlncmF0aW9uQWN0aW9uIHtcbiAgICAvLyBBbHdheXMgbm90aWZ5IHRoZSBkZXZlbG9wZXIgdGhhdCB0aGUgSGFtbWVyIHY5IG1pZ3JhdGlvbiBkb2VzIG5vdCBtaWdyYXRlIHRlc3RzLlxuICAgIGNvbnRleHQubG9nZ2VyLmluZm8oXG4gICAgICAgICdcXG7imqAgIEdlbmVyYWwgbm90aWNlOiBUaGUgSGFtbWVySlMgdjkgbWlncmF0aW9uIGZvciBBbmd1bGFyIENvbXBvbmVudHMgaXMgbm90IGFibGUgdG8gJyArXG4gICAgICAgICdtaWdyYXRlIHRlc3RzLiBQbGVhc2UgbWFudWFsbHkgY2xlYW4gdXAgdGVzdHMgaW4geW91ciBwcm9qZWN0IGlmIHRoZXkgcmVseSBvbiAnICtcbiAgICAgICAgKHRoaXMuZ2xvYmFsVXNlc0hhbW1lciA/ICd0aGUgZGVwcmVjYXRlZCBBbmd1bGFyIE1hdGVyaWFsIGdlc3R1cmUgY29uZmlnLicgOiAnSGFtbWVySlMuJykpO1xuICAgIGNvbnRleHQubG9nZ2VyLmluZm8oXG4gICAgICAgICdSZWFkIG1vcmUgYWJvdXQgbWlncmF0aW5nIHRlc3RzOiBodHRwczovL2dpdC5pby9uZy1tYXRlcmlhbC12OS1oYW1tZXItbWlncmF0ZS10ZXN0cycpO1xuXG4gICAgaWYgKCF0aGlzLmdsb2JhbFVzZXNIYW1tZXIgJiYgdGhpcy5fcmVtb3ZlSGFtbWVyRnJvbVBhY2thZ2VKc29uKHRyZWUpKSB7XG4gICAgICAvLyBTaW5jZSBIYW1tZXIgaGFzIGJlZW4gcmVtb3ZlZCBmcm9tIHRoZSB3b3Jrc3BhY2UgXCJwYWNrYWdlLmpzb25cIiBmaWxlLFxuICAgICAgLy8gd2Ugc2NoZWR1bGUgYSBub2RlIHBhY2thZ2UgaW5zdGFsbCB0YXNrIHRvIHJlZnJlc2ggdGhlIGxvY2sgZmlsZS5cbiAgICAgIHJldHVybiB7cnVuUGFja2FnZU1hbmFnZXI6IHRydWV9O1xuICAgIH1cblxuICAgIC8vIENsZWFuIGdsb2JhbCBzdGF0ZSBvbmNlIHRoZSB3b3Jrc3BhY2UgaGFzIGJlZW4gbWlncmF0ZWQuIFRoaXMgaXMgdGVjaG5pY2FsbHlcbiAgICAvLyBub3QgbmVjZXNzYXJ5IGluIFwibmcgdXBkYXRlXCIsIGJ1dCBpbiB0ZXN0cyB3ZSByZS11c2UgdGhlIHNhbWUgcnVsZSBjbGFzcy5cbiAgICB0aGlzLmdsb2JhbFVzZXNIYW1tZXIgPSBmYWxzZTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZW1vdmVzIHRoZSBoYW1tZXIgcGFja2FnZSBmcm9tIHRoZSB3b3Jrc3BhY2UgXCJwYWNrYWdlLmpzb25cIi5cbiAgICogQHJldHVybnMgV2hldGhlciBIYW1tZXIgd2FzIHNldCB1cCBhbmQgaGFzIGJlZW4gcmVtb3ZlZCBmcm9tIHRoZSBcInBhY2thZ2UuanNvblwiXG4gICAqL1xuICBwcml2YXRlIHN0YXRpYyBfcmVtb3ZlSGFtbWVyRnJvbVBhY2thZ2VKc29uKHRyZWU6IFRyZWUpOiBib29sZWFuIHtcbiAgICBpZiAoIXRyZWUuZXhpc3RzKCcvcGFja2FnZS5qc29uJykpIHtcbiAgICAgIHJldHVybiBmYWxzZTtcbiAgICB9XG5cbiAgICBjb25zdCBwYWNrYWdlSnNvbiA9IEpTT04ucGFyc2UodHJlZS5yZWFkKCcvcGFja2FnZS5qc29uJykhLnRvU3RyaW5nKCd1dGY4JykpO1xuXG4gICAgLy8gV2UgZG8gbm90IGhhbmRsZSB0aGUgY2FzZSB3aGVyZSBzb21lb25lIG1hbnVhbGx5IGFkZGVkIFwiaGFtbWVyanNcIiB0byB0aGUgZGV2IGRlcGVuZGVuY2llcy5cbiAgICBpZiAocGFja2FnZUpzb24uZGVwZW5kZW5jaWVzICYmIHBhY2thZ2VKc29uLmRlcGVuZGVuY2llc1tIQU1NRVJfTU9EVUxFX1NQRUNJRklFUl0pIHtcbiAgICAgIGRlbGV0ZSBwYWNrYWdlSnNvbi5kZXBlbmRlbmNpZXNbSEFNTUVSX01PRFVMRV9TUEVDSUZJRVJdO1xuICAgICAgdHJlZS5vdmVyd3JpdGUoJy9wYWNrYWdlLmpzb24nLCBKU09OLnN0cmluZ2lmeShwYWNrYWdlSnNvbiwgbnVsbCwgMikpO1xuICAgICAgcmV0dXJuIHRydWU7XG4gICAgfVxuICAgIHJldHVybiBmYWxzZTtcbiAgfVxufVxuXG4vKipcbiAqIFJlY3Vyc2l2ZWx5IHVud3JhcHMgYSBnaXZlbiBleHByZXNzaW9uIGlmIGl0IGlzIHdyYXBwZWRcbiAqIGJ5IHBhcmVudGhlc2lzLCB0eXBlIGNhc3RzIG9yIHR5cGUgYXNzZXJ0aW9ucy5cbiAqL1xuZnVuY3Rpb24gdW53cmFwRXhwcmVzc2lvbihub2RlOiB0cy5Ob2RlKTogdHMuTm9kZSB7XG4gIGlmICh0cy5pc1BhcmVudGhlc2l6ZWRFeHByZXNzaW9uKG5vZGUpKSB7XG4gICAgcmV0dXJuIHVud3JhcEV4cHJlc3Npb24obm9kZS5leHByZXNzaW9uKTtcbiAgfSBlbHNlIGlmICh0cy5pc0FzRXhwcmVzc2lvbihub2RlKSkge1xuICAgIHJldHVybiB1bndyYXBFeHByZXNzaW9uKG5vZGUuZXhwcmVzc2lvbik7XG4gIH0gZWxzZSBpZiAodHMuaXNUeXBlQXNzZXJ0aW9uKG5vZGUpKSB7XG4gICAgcmV0dXJuIHVud3JhcEV4cHJlc3Npb24obm9kZS5leHByZXNzaW9uKTtcbiAgfVxuICByZXR1cm4gbm9kZTtcbn1cblxuLyoqXG4gKiBDb252ZXJ0cyB0aGUgc3BlY2lmaWVkIHBhdGggdG8gYSB2YWxpZCBUeXBlU2NyaXB0IG1vZHVsZSBzcGVjaWZpZXIgd2hpY2ggaXNcbiAqIHJlbGF0aXZlIHRvIHRoZSBnaXZlbiBjb250YWluaW5nIGZpbGUuXG4gKi9cbmZ1bmN0aW9uIGdldE1vZHVsZVNwZWNpZmllcihuZXdQYXRoOiBzdHJpbmcsIGNvbnRhaW5pbmdGaWxlOiBzdHJpbmcpIHtcbiAgbGV0IHJlc3VsdCA9IHJlbGF0aXZlKGRpcm5hbWUoY29udGFpbmluZ0ZpbGUpLCBuZXdQYXRoKS5yZXBsYWNlKC9cXFxcL2csICcvJykucmVwbGFjZSgvXFwudHMkLywgJycpO1xuICBpZiAoIXJlc3VsdC5zdGFydHNXaXRoKCcuJykpIHtcbiAgICByZXN1bHQgPSBgLi8ke3Jlc3VsdH1gO1xuICB9XG4gIHJldHVybiByZXN1bHQ7XG59XG5cbi8qKlxuICogR2V0cyB0aGUgdGV4dCBvZiB0aGUgZ2l2ZW4gcHJvcGVydHkgbmFtZS5cbiAqIEByZXR1cm5zIFRleHQgb2YgdGhlIGdpdmVuIHByb3BlcnR5IG5hbWUuIE51bGwgaWYgbm90IHN0YXRpY2FsbHkgYW5hbHl6YWJsZS5cbiAqL1xuZnVuY3Rpb24gZ2V0UHJvcGVydHlOYW1lVGV4dChub2RlOiB0cy5Qcm9wZXJ0eU5hbWUpOiBzdHJpbmd8bnVsbCB7XG4gIGlmICh0cy5pc0lkZW50aWZpZXIobm9kZSkgfHwgdHMuaXNTdHJpbmdMaXRlcmFsTGlrZShub2RlKSkge1xuICAgIHJldHVybiBub2RlLnRleHQ7XG4gIH1cbiAgcmV0dXJuIG51bGw7XG59XG5cbi8qKiBDaGVja3Mgd2hldGhlciB0aGUgZ2l2ZW4gaWRlbnRpZmllciBpcyBwYXJ0IG9mIGEgbmFtZXNwYWNlZCBhY2Nlc3MuICovXG5mdW5jdGlvbiBpc05hbWVzcGFjZWRJZGVudGlmaWVyQWNjZXNzKG5vZGU6IHRzLklkZW50aWZpZXIpOiBib29sZWFuIHtcbiAgcmV0dXJuIHRzLmlzUXVhbGlmaWVkTmFtZShub2RlLnBhcmVudCkgfHwgdHMuaXNQcm9wZXJ0eUFjY2Vzc0V4cHJlc3Npb24obm9kZS5wYXJlbnQpO1xufVxuXG4vKipcbiAqIFdhbGtzIHRocm91Z2ggdGhlIHNwZWNpZmllZCBub2RlIGFuZCByZXR1cm5zIGFsbCBjaGlsZCBub2RlcyB3aGljaCBtYXRjaCB0aGVcbiAqIGdpdmVuIHByZWRpY2F0ZS5cbiAqL1xuZnVuY3Rpb24gZmluZE1hdGNoaW5nQ2hpbGROb2RlczxUIGV4dGVuZHMgdHMuTm9kZT4oXG4gICAgcGFyZW50OiB0cy5Ob2RlLCBwcmVkaWNhdGU6IChub2RlOiB0cy5Ob2RlKSA9PiBub2RlIGlzIFQpOiBUW10ge1xuICBjb25zdCByZXN1bHQ6IFRbXSA9IFtdO1xuICBjb25zdCB2aXNpdE5vZGUgPSAobm9kZTogdHMuTm9kZSkgPT4ge1xuICAgIGlmIChwcmVkaWNhdGUobm9kZSkpIHtcbiAgICAgIHJlc3VsdC5wdXNoKG5vZGUpO1xuICAgIH1cbiAgICB0cy5mb3JFYWNoQ2hpbGQobm9kZSwgdmlzaXROb2RlKTtcbiAgfTtcbiAgdHMuZm9yRWFjaENoaWxkKHBhcmVudCwgdmlzaXROb2RlKTtcbiAgcmV0dXJuIHJlc3VsdDtcbn1cbiJdfQ==