/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { MenuHarnessFilters, MenuItemHarnessFilters } from './menu-harness-filters';
/** Harness for interacting with a standard mat-menu in tests. */
export declare class MatMenuHarness extends ComponentHarness {
    /** The selector for the host element of a `MatMenu` instance. */
    static hostSelector: string;
    private _documentRootLocator;
    /**
     * Gets a `HarnessPredicate` that can be used to search for a `MatMenuHarness` that meets certain
     * criteria.
     * @param options Options for filtering which menu instances are considered a match.
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: MenuHarnessFilters): HarnessPredicate<MatMenuHarness>;
    /** Whether the menu is disabled. */
    isDisabled(): Promise<boolean>;
    /** Whether the menu is open. */
    isOpen(): Promise<boolean>;
    /** Gets the text of the menu's trigger element. */
    getTriggerText(): Promise<string>;
    /** Focuses the menu. */
    focus(): Promise<void>;
    /** Blurs the menu. */
    blur(): Promise<void>;
    /** Opens the menu. */
    open(): Promise<void>;
    /** Closes the menu. */
    close(): Promise<void>;
    /**
     * Gets a list of `MatMenuItemHarness` representing the items in the menu.
     * @param filters Optionally filters which menu items are included.
     */
    getItems(filters?: Omit<MenuItemHarnessFilters, 'ancestor'>): Promise<MatMenuItemHarness[]>;
    /**
     * Clicks an item in the menu, and optionally continues clicking items in subsequent sub-menus.
     * @param itemFilter A filter used to represent which item in the menu should be clicked. The
     *     first matching menu item will be clicked.
     * @param subItemFilters A list of filters representing the items to click in any subsequent
     *     sub-menus. The first item in the sub-menu matching the corresponding filter in
     *     `subItemFilters` will be clicked.
     */
    clickItem(itemFilter: Omit<MenuItemHarnessFilters, 'ancestor'>, ...subItemFilters: Omit<MenuItemHarnessFilters, 'ancestor'>[]): Promise<void>;
    /** Gets the menu panel associated with this menu. */
    private _getMenuPanel;
    /** Gets the id of the menu panel associated with this menu. */
    private _getPanelId;
}
/** Harness for interacting with a standard mat-menu-item in tests. */
export declare class MatMenuItemHarness extends ComponentHarness {
    /** The selector for the host element of a `MatMenuItem` instance. */
    static hostSelector: string;
    /**
     * Gets a `HarnessPredicate` that can be used to search for a `MatMenuItemHarness` that meets
     * certain criteria.
     * @param options Options for filtering which menu item instances are considered a match.
     * @return a `HarnessPredicate` configured with the given options.
     */
    static with(options?: MenuItemHarnessFilters): HarnessPredicate<MatMenuItemHarness>;
    /** Whether the menu is disabled. */
    isDisabled(): Promise<boolean>;
    /** Gets the text of the menu item. */
    getText(): Promise<string>;
    /** Focuses the menu item. */
    focus(): Promise<void>;
    /** Blurs the menu item. */
    blur(): Promise<void>;
    /** Clicks the menu item. */
    click(): Promise<void>;
    /** Whether this item has a submenu. */
    hasSubmenu(): Promise<boolean>;
    /** Gets the submenu associated with this menu item, or null if none. */
    getSubmenu(): Promise<MatMenuHarness | null>;
}
