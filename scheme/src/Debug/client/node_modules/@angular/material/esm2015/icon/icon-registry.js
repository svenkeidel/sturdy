/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { DOCUMENT } from '@angular/common';
import { HttpClient } from '@angular/common/http';
import { ErrorHandler, Inject, Injectable, Optional, SecurityContext, SkipSelf, } from '@angular/core';
import { DomSanitizer } from '@angular/platform-browser';
import { forkJoin, of as observableOf, throwError as observableThrow } from 'rxjs';
import { catchError, finalize, map, share, tap } from 'rxjs/operators';
import * as i0 from "@angular/core";
import * as i1 from "@angular/common/http";
import * as i2 from "@angular/platform-browser";
import * as i3 from "@angular/common";
/**
 * Returns an exception to be thrown in the case when attempting to
 * load an icon with a name that cannot be found.
 * @docs-private
 */
export function getMatIconNameNotFoundError(iconName) {
    return Error(`Unable to find icon with the name "${iconName}"`);
}
/**
 * Returns an exception to be thrown when the consumer attempts to use
 * `<mat-icon>` without including @angular/common/http.
 * @docs-private
 */
export function getMatIconNoHttpProviderError() {
    return Error('Could not find HttpClient provider for use with Angular Material icons. ' +
        'Please include the HttpClientModule from @angular/common/http in your ' +
        'app imports.');
}
/**
 * Returns an exception to be thrown when a URL couldn't be sanitized.
 * @param url URL that was attempted to be sanitized.
 * @docs-private
 */
export function getMatIconFailedToSanitizeUrlError(url) {
    return Error(`The URL provided to MatIconRegistry was not trusted as a resource URL ` +
        `via Angular's DomSanitizer. Attempted URL was "${url}".`);
}
/**
 * Returns an exception to be thrown when a HTML string couldn't be sanitized.
 * @param literal HTML that was attempted to be sanitized.
 * @docs-private
 */
export function getMatIconFailedToSanitizeLiteralError(literal) {
    return Error(`The literal provided to MatIconRegistry was not trusted as safe HTML by ` +
        `Angular's DomSanitizer. Attempted literal was "${literal}".`);
}
/**
 * Configuration for an icon, including the URL and possibly the cached SVG element.
 * @docs-private
 */
class SvgIconConfig {
    constructor(data, options) {
        this.options = options;
        // Note that we can't use `instanceof SVGElement` here,
        // because it'll break during server-side rendering.
        if (!!data.nodeName) {
            this.svgElement = data;
        }
        else {
            this.url = data;
        }
    }
}
/**
 * Service to register and display icons used by the `<mat-icon>` component.
 * - Registers icon URLs by namespace and name.
 * - Registers icon set URLs by namespace.
 * - Registers aliases for CSS classes, for use with icon fonts.
 * - Loads icons from URLs and extracts individual icons from icon sets.
 */
let MatIconRegistry = /** @class */ (() => {
    class MatIconRegistry {
        constructor(_httpClient, _sanitizer, document, _errorHandler) {
            this._httpClient = _httpClient;
            this._sanitizer = _sanitizer;
            this._errorHandler = _errorHandler;
            /**
             * URLs and cached SVG elements for individual icons. Keys are of the format "[namespace]:[icon]".
             */
            this._svgIconConfigs = new Map();
            /**
             * SvgIconConfig objects and cached SVG elements for icon sets, keyed by namespace.
             * Multiple icon sets can be registered under the same namespace.
             */
            this._iconSetConfigs = new Map();
            /** Cache for icons loaded by direct URLs. */
            this._cachedIconsByUrl = new Map();
            /** In-progress icon fetches. Used to coalesce multiple requests to the same URL. */
            this._inProgressUrlFetches = new Map();
            /** Map from font identifiers to their CSS class names. Used for icon fonts. */
            this._fontCssClassesByAlias = new Map();
            /**
             * The CSS class to apply when an `<mat-icon>` component has no icon name, url, or font specified.
             * The default 'material-icons' value assumes that the material icon font has been loaded as
             * described at http://google.github.io/material-design-icons/#icon-font-for-the-web
             */
            this._defaultFontSetClass = 'material-icons';
            this._document = document;
        }
        /**
         * Registers an icon by URL in the default namespace.
         * @param iconName Name under which the icon should be registered.
         * @param url
         */
        addSvgIcon(iconName, url, options) {
            return this.addSvgIconInNamespace('', iconName, url, options);
        }
        /**
         * Registers an icon using an HTML string in the default namespace.
         * @param iconName Name under which the icon should be registered.
         * @param literal SVG source of the icon.
         */
        addSvgIconLiteral(iconName, literal, options) {
            return this.addSvgIconLiteralInNamespace('', iconName, literal, options);
        }
        /**
         * Registers an icon by URL in the specified namespace.
         * @param namespace Namespace in which the icon should be registered.
         * @param iconName Name under which the icon should be registered.
         * @param url
         */
        addSvgIconInNamespace(namespace, iconName, url, options) {
            return this._addSvgIconConfig(namespace, iconName, new SvgIconConfig(url, options));
        }
        /**
         * Registers an icon using an HTML string in the specified namespace.
         * @param namespace Namespace in which the icon should be registered.
         * @param iconName Name under which the icon should be registered.
         * @param literal SVG source of the icon.
         */
        addSvgIconLiteralInNamespace(namespace, iconName, literal, options) {
            const sanitizedLiteral = this._sanitizer.sanitize(SecurityContext.HTML, literal);
            if (!sanitizedLiteral) {
                throw getMatIconFailedToSanitizeLiteralError(literal);
            }
            const svgElement = this._createSvgElementForSingleIcon(sanitizedLiteral, options);
            return this._addSvgIconConfig(namespace, iconName, new SvgIconConfig(svgElement, options));
        }
        /**
         * Registers an icon set by URL in the default namespace.
         * @param url
         */
        addSvgIconSet(url, options) {
            return this.addSvgIconSetInNamespace('', url, options);
        }
        /**
         * Registers an icon set using an HTML string in the default namespace.
         * @param literal SVG source of the icon set.
         */
        addSvgIconSetLiteral(literal, options) {
            return this.addSvgIconSetLiteralInNamespace('', literal, options);
        }
        /**
         * Registers an icon set by URL in the specified namespace.
         * @param namespace Namespace in which to register the icon set.
         * @param url
         */
        addSvgIconSetInNamespace(namespace, url, options) {
            return this._addSvgIconSetConfig(namespace, new SvgIconConfig(url, options));
        }
        /**
         * Registers an icon set using an HTML string in the specified namespace.
         * @param namespace Namespace in which to register the icon set.
         * @param literal SVG source of the icon set.
         */
        addSvgIconSetLiteralInNamespace(namespace, literal, options) {
            const sanitizedLiteral = this._sanitizer.sanitize(SecurityContext.HTML, literal);
            if (!sanitizedLiteral) {
                throw getMatIconFailedToSanitizeLiteralError(literal);
            }
            const svgElement = this._svgElementFromString(sanitizedLiteral);
            return this._addSvgIconSetConfig(namespace, new SvgIconConfig(svgElement, options));
        }
        /**
         * Defines an alias for a CSS class name to be used for icon fonts. Creating an matIcon
         * component with the alias as the fontSet input will cause the class name to be applied
         * to the `<mat-icon>` element.
         *
         * @param alias Alias for the font.
         * @param className Class name override to be used instead of the alias.
         */
        registerFontClassAlias(alias, className = alias) {
            this._fontCssClassesByAlias.set(alias, className);
            return this;
        }
        /**
         * Returns the CSS class name associated with the alias by a previous call to
         * registerFontClassAlias. If no CSS class has been associated, returns the alias unmodified.
         */
        classNameForFontAlias(alias) {
            return this._fontCssClassesByAlias.get(alias) || alias;
        }
        /**
         * Sets the CSS class name to be used for icon fonts when an `<mat-icon>` component does not
         * have a fontSet input value, and is not loading an icon by name or URL.
         *
         * @param className
         */
        setDefaultFontSetClass(className) {
            this._defaultFontSetClass = className;
            return this;
        }
        /**
         * Returns the CSS class name to be used for icon fonts when an `<mat-icon>` component does not
         * have a fontSet input value, and is not loading an icon by name or URL.
         */
        getDefaultFontSetClass() {
            return this._defaultFontSetClass;
        }
        /**
         * Returns an Observable that produces the icon (as an `<svg>` DOM element) from the given URL.
         * The response from the URL may be cached so this will not always cause an HTTP request, but
         * the produced element will always be a new copy of the originally fetched icon. (That is,
         * it will not contain any modifications made to elements previously returned).
         *
         * @param safeUrl URL from which to fetch the SVG icon.
         */
        getSvgIconFromUrl(safeUrl) {
            const url = this._sanitizer.sanitize(SecurityContext.RESOURCE_URL, safeUrl);
            if (!url) {
                throw getMatIconFailedToSanitizeUrlError(safeUrl);
            }
            const cachedIcon = this._cachedIconsByUrl.get(url);
            if (cachedIcon) {
                return observableOf(cloneSvg(cachedIcon));
            }
            return this._loadSvgIconFromConfig(new SvgIconConfig(safeUrl)).pipe(tap(svg => this._cachedIconsByUrl.set(url, svg)), map(svg => cloneSvg(svg)));
        }
        /**
         * Returns an Observable that produces the icon (as an `<svg>` DOM element) with the given name
         * and namespace. The icon must have been previously registered with addIcon or addIconSet;
         * if not, the Observable will throw an error.
         *
         * @param name Name of the icon to be retrieved.
         * @param namespace Namespace in which to look for the icon.
         */
        getNamedSvgIcon(name, namespace = '') {
            // Return (copy of) cached icon if possible.
            const key = iconKey(namespace, name);
            const config = this._svgIconConfigs.get(key);
            if (config) {
                return this._getSvgFromConfig(config);
            }
            // See if we have any icon sets registered for the namespace.
            const iconSetConfigs = this._iconSetConfigs.get(namespace);
            if (iconSetConfigs) {
                return this._getSvgFromIconSetConfigs(name, iconSetConfigs);
            }
            return observableThrow(getMatIconNameNotFoundError(key));
        }
        ngOnDestroy() {
            this._svgIconConfigs.clear();
            this._iconSetConfigs.clear();
            this._cachedIconsByUrl.clear();
        }
        /**
         * Returns the cached icon for a SvgIconConfig if available, or fetches it from its URL if not.
         */
        _getSvgFromConfig(config) {
            if (config.svgElement) {
                // We already have the SVG element for this icon, return a copy.
                return observableOf(cloneSvg(config.svgElement));
            }
            else {
                // Fetch the icon from the config's URL, cache it, and return a copy.
                return this._loadSvgIconFromConfig(config).pipe(tap(svg => config.svgElement = svg), map(svg => cloneSvg(svg)));
            }
        }
        /**
         * Attempts to find an icon with the specified name in any of the SVG icon sets.
         * First searches the available cached icons for a nested element with a matching name, and
         * if found copies the element to a new `<svg>` element. If not found, fetches all icon sets
         * that have not been cached, and searches again after all fetches are completed.
         * The returned Observable produces the SVG element if possible, and throws
         * an error if no icon with the specified name can be found.
         */
        _getSvgFromIconSetConfigs(name, iconSetConfigs) {
            // For all the icon set SVG elements we've fetched, see if any contain an icon with the
            // requested name.
            const namedIcon = this._extractIconWithNameFromAnySet(name, iconSetConfigs);
            if (namedIcon) {
                // We could cache namedIcon in _svgIconConfigs, but since we have to make a copy every
                // time anyway, there's probably not much advantage compared to just always extracting
                // it from the icon set.
                return observableOf(namedIcon);
            }
            // Not found in any cached icon sets. If there are icon sets with URLs that we haven't
            // fetched, fetch them now and look for iconName in the results.
            const iconSetFetchRequests = iconSetConfigs
                .filter(iconSetConfig => !iconSetConfig.svgElement)
                .map(iconSetConfig => {
                return this._loadSvgIconSetFromConfig(iconSetConfig).pipe(catchError((err) => {
                    const url = this._sanitizer.sanitize(SecurityContext.RESOURCE_URL, iconSetConfig.url);
                    // Swallow errors fetching individual URLs so the
                    // combined Observable won't necessarily fail.
                    const errorMessage = `Loading icon set URL: ${url} failed: ${err.message}`;
                    this._errorHandler.handleError(new Error(errorMessage));
                    return observableOf(null);
                }));
            });
            // Fetch all the icon set URLs. When the requests complete, every IconSet should have a
            // cached SVG element (unless the request failed), and we can check again for the icon.
            return forkJoin(iconSetFetchRequests).pipe(map(() => {
                const foundIcon = this._extractIconWithNameFromAnySet(name, iconSetConfigs);
                if (!foundIcon) {
                    throw getMatIconNameNotFoundError(name);
                }
                return foundIcon;
            }));
        }
        /**
         * Searches the cached SVG elements for the given icon sets for a nested icon element whose "id"
         * tag matches the specified name. If found, copies the nested element to a new SVG element and
         * returns it. Returns null if no matching element is found.
         */
        _extractIconWithNameFromAnySet(iconName, iconSetConfigs) {
            // Iterate backwards, so icon sets added later have precedence.
            for (let i = iconSetConfigs.length - 1; i >= 0; i--) {
                const config = iconSetConfigs[i];
                if (config.svgElement) {
                    const foundIcon = this._extractSvgIconFromSet(config.svgElement, iconName, config.options);
                    if (foundIcon) {
                        return foundIcon;
                    }
                }
            }
            return null;
        }
        /**
         * Loads the content of the icon URL specified in the SvgIconConfig and creates an SVG element
         * from it.
         */
        _loadSvgIconFromConfig(config) {
            return this._fetchIcon(config)
                .pipe(map(svgText => this._createSvgElementForSingleIcon(svgText, config.options)));
        }
        /**
         * Loads the content of the icon set URL specified in the SvgIconConfig and creates an SVG element
         * from it.
         */
        _loadSvgIconSetFromConfig(config) {
            // If the SVG for this icon set has already been parsed, do nothing.
            if (config.svgElement) {
                return observableOf(config.svgElement);
            }
            return this._fetchIcon(config).pipe(map(svgText => {
                // It is possible that the icon set was parsed and cached by an earlier request, so parsing
                // only needs to occur if the cache is yet unset.
                if (!config.svgElement) {
                    config.svgElement = this._svgElementFromString(svgText);
                }
                return config.svgElement;
            }));
        }
        /**
         * Creates a DOM element from the given SVG string, and adds default attributes.
         */
        _createSvgElementForSingleIcon(responseText, options) {
            const svg = this._svgElementFromString(responseText);
            this._setSvgAttributes(svg, options);
            return svg;
        }
        /**
         * Searches the cached element of the given SvgIconConfig for a nested icon element whose "id"
         * tag matches the specified name. If found, copies the nested element to a new SVG element and
         * returns it. Returns null if no matching element is found.
         */
        _extractSvgIconFromSet(iconSet, iconName, options) {
            // Use the `id="iconName"` syntax in order to escape special
            // characters in the ID (versus using the #iconName syntax).
            const iconSource = iconSet.querySelector(`[id="${iconName}"]`);
            if (!iconSource) {
                return null;
            }
            // Clone the element and remove the ID to prevent multiple elements from being added
            // to the page with the same ID.
            const iconElement = iconSource.cloneNode(true);
            iconElement.removeAttribute('id');
            // If the icon node is itself an <svg> node, clone and return it directly. If not, set it as
            // the content of a new <svg> node.
            if (iconElement.nodeName.toLowerCase() === 'svg') {
                return this._setSvgAttributes(iconElement, options);
            }
            // If the node is a <symbol>, it won't be rendered so we have to convert it into <svg>. Note
            // that the same could be achieved by referring to it via <use href="#id">, however the <use>
            // tag is problematic on Firefox, because it needs to include the current page path.
            if (iconElement.nodeName.toLowerCase() === 'symbol') {
                return this._setSvgAttributes(this._toSvgElement(iconElement), options);
            }
            // createElement('SVG') doesn't work as expected; the DOM ends up with
            // the correct nodes, but the SVG content doesn't render. Instead we
            // have to create an empty SVG node using innerHTML and append its content.
            // Elements created using DOMParser.parseFromString have the same problem.
            // http://stackoverflow.com/questions/23003278/svg-innerhtml-in-firefox-can-not-display
            const svg = this._svgElementFromString('<svg></svg>');
            // Clone the node so we don't remove it from the parent icon set element.
            svg.appendChild(iconElement);
            return this._setSvgAttributes(svg, options);
        }
        /**
         * Creates a DOM element from the given SVG string.
         */
        _svgElementFromString(str) {
            const div = this._document.createElement('DIV');
            div.innerHTML = str;
            const svg = div.querySelector('svg');
            if (!svg) {
                throw Error('<svg> tag not found');
            }
            return svg;
        }
        /**
         * Converts an element into an SVG node by cloning all of its children.
         */
        _toSvgElement(element) {
            const svg = this._svgElementFromString('<svg></svg>');
            const attributes = element.attributes;
            // Copy over all the attributes from the `symbol` to the new SVG, except the id.
            for (let i = 0; i < attributes.length; i++) {
                const { name, value } = attributes[i];
                if (name !== 'id') {
                    svg.setAttribute(name, value);
                }
            }
            for (let i = 0; i < element.childNodes.length; i++) {
                if (element.childNodes[i].nodeType === this._document.ELEMENT_NODE) {
                    svg.appendChild(element.childNodes[i].cloneNode(true));
                }
            }
            return svg;
        }
        /**
         * Sets the default attributes for an SVG element to be used as an icon.
         */
        _setSvgAttributes(svg, options) {
            svg.setAttribute('fit', '');
            svg.setAttribute('height', '100%');
            svg.setAttribute('width', '100%');
            svg.setAttribute('preserveAspectRatio', 'xMidYMid meet');
            svg.setAttribute('focusable', 'false'); // Disable IE11 default behavior to make SVGs focusable.
            if (options && options.viewBox) {
                svg.setAttribute('viewBox', options.viewBox);
            }
            return svg;
        }
        /**
         * Returns an Observable which produces the string contents of the given icon. Results may be
         * cached, so future calls with the same URL may not cause another HTTP request.
         */
        _fetchIcon(iconConfig) {
            var _a;
            const { url: safeUrl, options } = iconConfig;
            const withCredentials = (_a = options === null || options === void 0 ? void 0 : options.withCredentials) !== null && _a !== void 0 ? _a : false;
            if (!this._httpClient) {
                throw getMatIconNoHttpProviderError();
            }
            if (safeUrl == null) {
                throw Error(`Cannot fetch icon from URL "${safeUrl}".`);
            }
            const url = this._sanitizer.sanitize(SecurityContext.RESOURCE_URL, safeUrl);
            if (!url) {
                throw getMatIconFailedToSanitizeUrlError(safeUrl);
            }
            // Store in-progress fetches to avoid sending a duplicate request for a URL when there is
            // already a request in progress for that URL. It's necessary to call share() on the
            // Observable returned by http.get() so that multiple subscribers don't cause multiple XHRs.
            const inProgressFetch = this._inProgressUrlFetches.get(url);
            if (inProgressFetch) {
                return inProgressFetch;
            }
            // TODO(jelbourn): for some reason, the `finalize` operator "loses" the generic type on the
            // Observable. Figure out why and fix it.
            const req = this._httpClient.get(url, { responseType: 'text', withCredentials }).pipe(finalize(() => this._inProgressUrlFetches.delete(url)), share());
            this._inProgressUrlFetches.set(url, req);
            return req;
        }
        /**
         * Registers an icon config by name in the specified namespace.
         * @param namespace Namespace in which to register the icon config.
         * @param iconName Name under which to register the config.
         * @param config Config to be registered.
         */
        _addSvgIconConfig(namespace, iconName, config) {
            this._svgIconConfigs.set(iconKey(namespace, iconName), config);
            return this;
        }
        /**
         * Registers an icon set config in the specified namespace.
         * @param namespace Namespace in which to register the icon config.
         * @param config Config to be registered.
         */
        _addSvgIconSetConfig(namespace, config) {
            const configNamespace = this._iconSetConfigs.get(namespace);
            if (configNamespace) {
                configNamespace.push(config);
            }
            else {
                this._iconSetConfigs.set(namespace, [config]);
            }
            return this;
        }
    }
    MatIconRegistry.ɵprov = i0.ɵɵdefineInjectable({ factory: function MatIconRegistry_Factory() { return new MatIconRegistry(i0.ɵɵinject(i1.HttpClient, 8), i0.ɵɵinject(i2.DomSanitizer), i0.ɵɵinject(i3.DOCUMENT, 8), i0.ɵɵinject(i0.ErrorHandler)); }, token: MatIconRegistry, providedIn: "root" });
    MatIconRegistry.decorators = [
        { type: Injectable, args: [{ providedIn: 'root' },] }
    ];
    MatIconRegistry.ctorParameters = () => [
        { type: HttpClient, decorators: [{ type: Optional }] },
        { type: DomSanitizer },
        { type: undefined, decorators: [{ type: Optional }, { type: Inject, args: [DOCUMENT,] }] },
        { type: ErrorHandler }
    ];
    return MatIconRegistry;
})();
export { MatIconRegistry };
/** @docs-private */
export function ICON_REGISTRY_PROVIDER_FACTORY(parentRegistry, httpClient, sanitizer, errorHandler, document) {
    return parentRegistry || new MatIconRegistry(httpClient, sanitizer, document, errorHandler);
}
/** @docs-private */
export const ICON_REGISTRY_PROVIDER = {
    // If there is already an MatIconRegistry available, use that. Otherwise, provide a new one.
    provide: MatIconRegistry,
    deps: [
        [new Optional(), new SkipSelf(), MatIconRegistry],
        [new Optional(), HttpClient],
        DomSanitizer,
        ErrorHandler,
        [new Optional(), DOCUMENT],
    ],
    useFactory: ICON_REGISTRY_PROVIDER_FACTORY,
};
/** Clones an SVGElement while preserving type information. */
function cloneSvg(svg) {
    return svg.cloneNode(true);
}
/** Returns the cache key to use for an icon namespace and name. */
function iconKey(namespace, name) {
    return namespace + ':' + name;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaWNvbi1yZWdpc3RyeS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9pY29uL2ljb24tcmVnaXN0cnkudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLGlCQUFpQixDQUFDO0FBQ3pDLE9BQU8sRUFBQyxVQUFVLEVBQW9CLE1BQU0sc0JBQXNCLENBQUM7QUFDbkUsT0FBTyxFQUNMLFlBQVksRUFDWixNQUFNLEVBQ04sVUFBVSxFQUVWLFFBQVEsRUFDUixlQUFlLEVBQ2YsUUFBUSxHQUVULE1BQU0sZUFBZSxDQUFDO0FBQ3ZCLE9BQU8sRUFBQyxZQUFZLEVBQTRCLE1BQU0sMkJBQTJCLENBQUM7QUFDbEYsT0FBTyxFQUFDLFFBQVEsRUFBYyxFQUFFLElBQUksWUFBWSxFQUFFLFVBQVUsSUFBSSxlQUFlLEVBQUMsTUFBTSxNQUFNLENBQUM7QUFDN0YsT0FBTyxFQUFDLFVBQVUsRUFBRSxRQUFRLEVBQUUsR0FBRyxFQUFFLEtBQUssRUFBRSxHQUFHLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQzs7Ozs7QUFHckU7Ozs7R0FJRztBQUNILE1BQU0sVUFBVSwyQkFBMkIsQ0FBQyxRQUFnQjtJQUMxRCxPQUFPLEtBQUssQ0FBQyxzQ0FBc0MsUUFBUSxHQUFHLENBQUMsQ0FBQztBQUNsRSxDQUFDO0FBR0Q7Ozs7R0FJRztBQUNILE1BQU0sVUFBVSw2QkFBNkI7SUFDM0MsT0FBTyxLQUFLLENBQUMsMEVBQTBFO1FBQzFFLHdFQUF3RTtRQUN4RSxjQUFjLENBQUMsQ0FBQztBQUMvQixDQUFDO0FBR0Q7Ozs7R0FJRztBQUNILE1BQU0sVUFBVSxrQ0FBa0MsQ0FBQyxHQUFvQjtJQUNyRSxPQUFPLEtBQUssQ0FBQyx3RUFBd0U7UUFDeEUsa0RBQWtELEdBQUcsSUFBSSxDQUFDLENBQUM7QUFDMUUsQ0FBQztBQUVEOzs7O0dBSUc7QUFDSCxN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UUFBUTtnQkEvSDlCLFlBQVk7OzBCQVhkO0tBc25CQztTQTdnQlksZUFBZTtBQStnQjVCLG9CQUFvQjtBQUNwQixNQUFNLFVBQVUsOEJBQThCLENBQzVDLGNBQStCLEVBQy9CLFVBQXNCLEVBQ3RCLFNBQXVCLEVBQ3ZCLFlBQTBCLEVBQzFCLFFBQWM7SUFDZCxPQUFPLGNBQWMsSUFBSSxJQUFJLGVBQWUsQ0FBQyxVQUFVLEVBQUUsU0FBUyxFQUFFLFFBQVEsRUFBRSxZQUFZLENBQUMsQ0FBQztBQUM5RixDQUFDO0FBRUQsb0JBQW9CO0FBQ3BCLE1BQU0sQ0FBQyxNQUFNLHNCQUFzQixHQUFHO0lBQ3BDLDRGQUE0RjtJQUM1RixPQUFPLEVBQUUsZUFBZTtJQUN4QixJQUFJLEVBQUU7UUFDSixDQUFDLElBQUksUUFBUSxFQUFFLEVBQUUsSUFBSSxRQUFRLEVBQUUsRUFBRSxlQUFlLENBQUM7UUFDakQsQ0FBQyxJQUFJLFFBQVEsRUFBRSxFQUFFLFVBQVUsQ0FBQztRQUM1QixZQUFZO1FBQ1osWUFBWTtRQUNaLENBQUMsSUFBSSxRQUFRLEVBQUUsRUFBRSxRQUErQixDQUFDO0tBQ2xEO0lBQ0QsVUFBVSxFQUFFLDhCQUE4QjtDQUMzQyxDQUFDO0FBRUYsOERBQThEO0FBQzlELFNBQVMsUUFBUSxDQUFDLEdBQWU7SUFDL0IsT0FBTyxHQUFHLENBQUMsU0FBUyxDQUFDLElBQUksQ0FBZSxDQUFDO0FBQzNDLENBQUM7QUFFRCxtRUFBbUU7QUFDbkUsU0FBUyxPQUFPLENBQUMsU0FBaUIsRUFBRSxJQUFZO0lBQzlDLE9BQU8sU0FBUyxHQUFHLEdBQUcsR0FBRyxJQUFJLENBQUM7QUFDaEMsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0RPQ1VNRU5UfSBmcm9tICdAYW5ndWxhci9jb21tb24nO1xuaW1wb3J0IHtIdHRwQ2xpZW50LCBIdHRwRXJyb3JSZXNwb25zZX0gZnJvbSAnQGFuZ3VsYXIvY29tbW9uL2h0dHAnO1xuaW1wb3J0IHtcbiAgRXJyb3JIYW5kbGVyLFxuICBJbmplY3QsXG4gIEluamVjdGFibGUsXG4gIEluamVjdGlvblRva2VuLFxuICBPcHRpb25hbCxcbiAgU2VjdXJpdHlDb250ZXh0LFxuICBTa2lwU2VsZixcbiAgT25EZXN0cm95LFxufSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7RG9tU2FuaXRpemVyLCBTYWZlUmVzb3VyY2VVcmwsIFNhZmVIdG1sfSBmcm9tICdAYW5ndWxhci9wbGF0Zm9ybS1icm93c2VyJztcbmltcG9ydCB7Zm9ya0pvaW4sIE9ic2VydmFibGUsIG9mIGFzIG9ic2VydmFibGVPZiwgdGhyb3dFcnJvciBhcyBvYnNlcnZhYmxlVGhyb3d9IGZyb20gJ3J4anMnO1xuaW1wb3J0IHtjYXRjaEVycm9yLCBmaW5hbGl6ZSwgbWFwLCBzaGFyZSwgdGFwfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5cblxuLyoqXG4gKiBSZXR1cm5zIGFuIGV4Y2VwdGlvbiB0byBiZSB0aHJvd24gaW4gdGhlIGNhc2Ugd2hlbiBhdHRlbXB0aW5nIHRvXG4gKiBsb2FkIGFuIGljb24gd2l0aCBhIG5hbWUgdGhhdCBjYW5ub3QgYmUgZm91bmQuXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRNYXRJY29uTmFtZU5vdEZvdW5kRXJyb3IoaWNvbk5hbWU6IHN0cmluZyk6IEVycm9yIHtcbiAgcmV0dXJuIEVycm9yKGBVbmFibGUgdG8gZmluZCBpY29uIHdpdGggdGhlIG5hbWUgXCIke2ljb25OYW1lfVwiYCk7XG59XG5cblxuLyoqXG4gKiBSZXR1cm5zIGFuIGV4Y2VwdGlvbiB0byBiZSB0aHJvd24gd2hlbiB0aGUgY29uc3VtZXIgYXR0ZW1wdHMgdG8gdXNlXG4gKiBgPG1hdC1pY29uPmAgd2l0aG91dCBpbmNsdWRpbmcgQGFuZ3VsYXIvY29tbW9uL2h0dHAuXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRNYXRJY29uTm9IdHRwUHJvdmlkZXJFcnJvcigpOiBFcnJvciB7XG4gIHJldHVybiBFcnJvcignQ291bGQgbm90IGZpbmQgSHR0cENsaWVudCBwcm92aWRlciBmb3IgdXNlIHdpdGggQW5ndWxhciBNYXRlcmlhbCBpY29ucy4gJyArXG4gICAgICAgICAgICAgICAnUGxlYXNlIGluY2x1ZGUgdGhlIEh0dHBDbGllbnRNb2R1bGUgZnJvbSBAYW5ndWxhci9jb21tb24vaHR0cCBpbiB5b3VyICcgK1xuICAgICAgICAgICAgICAgJ2FwcCBpbXBvcnRzLicpO1xufVxuXG5cbi8qKlxuICogUmV0dXJucyBhbiBleGNlcHRpb24gdG8gYmUgdGhyb3duIHdoZW4gYSBVUkwgY291bGRuJ3QgYmUgc2FuaXRpemVkLlxuICogQHBhcmFtIHVybCBVUkwgdGhhdCB3YXMgYXR0ZW1wdGVkIHRvIGJlIHNhbml0aXplZC5cbiAqIEBkb2NzLXByaXZhdGVcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGdldE1hdEljb25GYWlsZWRUb1Nhbml0aXplVXJsRXJyb3IodXJsOiBTYWZlUmVzb3VyY2VVcmwpOiBFcnJvciB7XG4gIHJldHVybiBFcnJvcihgVGhlIFVSTCBwcm92aWRlZCB0byBNYXRJY29uUmVnaXN0cnkgd2FzIG5vdCB0cnVzdGVkIGFzIGEgcmVzb3VyY2UgVVJMIGAgK1xuICAgICAgICAgICAgICAgYHZpYSBBbmd1bGFyJ3MgRG9tU2FuaXRpemVyLiBBdHRlbXB0ZWQgVVJMIHdhcyBcIiR7dXJsfVwiLmApO1xufVxuXG4vKipcbiAqIFJldHVybnMgYW4gZXhjZXB0aW9uIHRvIGJlIHRocm93biB3aGVuIGEgSFRNTCBzdHJpbmcgY291bGRuJ3QgYmUgc2FuaXRpemVkLlxuICogQHBhcmFtIGxpdGVyYWwgSFRNTCB0aGF0IHdhcyBhdHRlbXB0ZWQgdG8gYmUgc2FuaXRpemVkLlxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5leHBvcnQgZnVuY3Rpb24gZ2V0TWF0SWNvbkZhaWxlZFRvU2FuaXRpemVMaXRlcmFsRXJyb3IobGl0ZXJhbDogU2FmZUh0bWwpOiBFcnJvciB7XG4gIHJldHVybiBFcnJvcihgVGhlIGxpdGVyYWwgcHJvdmlkZWQgdG8gTWF0SWNvblJlZ2lzdHJ5IHdhcyBub3QgdHJ1c3RlZCBhcyBzYWZlIEhUTUwgYnkgYCArXG4gICAgICAgICAgICAgICBgQW5ndWxhcidzIERvbVNhbml0aXplci4gQXR0ZW1wdGVkIGxpdGVyYWwgd2FzIFwiJHtsaXRlcmFsfVwiLmApO1xufVxuXG4vKiogT3B0aW9ucyB0aGF0IGNhbiBiZSB1c2VkIHRvIGNvbmZpZ3VyZSBob3cgYW4gaWNvbiBvciB0aGUgaWNvbnMgaW4gYW4gaWNvbiBzZXQgYXJlIHByZXNlbnRlZC4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgSWNvbk9wdGlvbnMge1xuICAvKiogVmlldyBib3ggdG8gc2V0IG9uIHRoZSBpY29uLiAqL1xuICB2aWV3Qm94Pzogc3RyaW5nO1xuXG4gIC8qKiBXaGV0aGVyIG9yIG5vdCB0byBmZXRjaCB0aGUgaWNvbiBvciBpY29uIHNldCB1c2luZyBIVFRQIGNyZWRlbnRpYWxzLiAqL1xuICB3aXRoQ3JlZGVudGlhbHM/OiBib29sZWFuO1xufVxuXG4vKipcbiAqIENvbmZpZ3VyYXRpb24gZm9yIGFuIGljb24sIGluY2x1ZGluZyB0aGUgVVJMIGFuZCBwb3NzaWJseSB0aGUgY2FjaGVkIFNWRyBlbGVtZW50LlxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5jbGFzcyBTdmdJY29uQ29uZmlnIHtcbiAgdXJsOiBTYWZlUmVzb3VyY2VVcmwgfCBudWxsO1xuICBzdmdFbGVtZW50OiBTVkdFbGVtZW50IHwgbnVsbDtcblxuICBjb25zdHJ1Y3Rvcih1cmw6IFNhZmVSZXNvdXJjZVVybCwgb3B0aW9ucz86IEljb25PcHRpb25zKTtcbiAgY29uc3RydWN0b3Ioc3ZnRWxlbWVudDogU1ZHRWxlbWVudCwgb3B0aW9ucz86IEljb25PcHRpb25zKTtcbiAgY29uc3RydWN0b3IoZGF0YTogU2FmZVJlc291cmNlVXJsIHwgU1ZHRWxlbWVudCwgcHVibGljIG9wdGlvbnM/OiBJY29uT3B0aW9ucykge1xuICAgIC8vIE5vdGUgdGhhdCB3ZSBjYW4ndCB1c2UgYGluc3RhbmNlb2YgU1ZHRWxlbWVudGAgaGVyZSxcbiAgICAvLyBiZWNhdXNlIGl0J2xsIGJyZWFrIGR1cmluZyBzZXJ2ZXItc2lkZSByZW5kZXJpbmcuXG4gICAgaWYgKCEhKGRhdGEgYXMgYW55KS5ub2RlTmFtZSkge1xuICAgICAgdGhpcy5zdmdFbGVtZW50ID0gZGF0YSBhcyBTVkdFbGVtZW50O1xuICAgIH0gZWxzZSB7XG4gICAgICB0aGlzLnVybCA9IGRhdGEgYXMgU2FmZVJlc291cmNlVXJsO1xuICAgIH1cbiAgfVxufVxuXG4vKipcbiAqIFNlcnZpY2UgdG8gcmVnaXN0ZXIgYW5kIGRpc3BsYXkgaWNvbnMgdXNlZCBieSB0aGUgYDxtYXQtaWNvbj5gIGNvbXBvbmVudC5cbiAqIC0gUmVnaXN0ZXJzIGljb24gVVJMcyBieSBuYW1lc3BhY2UgYW5kIG5hbWUuXG4gKiAtIFJlZ2lzdGVycyBpY29uIHNldCBVUkxzIGJ5IG5hbWVzcGFjZS5cbiAqIC0gUmVnaXN0ZXJzIGFsaWFzZXMgZm9yIENTUyBjbGFzc2VzLCBmb3IgdXNlIHdpdGggaWNvbiBmb250cy5cbiAqIC0gTG9hZHMgaWNvbnMgZnJvbSBVUkxzIGFuZCBleHRyYWN0cyBpbmRpdmlkdWFsIGljb25zIGZyb20gaWNvbiBzZXRzLlxuICovXG5ASW5qZWN0YWJsZSh7cHJvdmlkZWRJbjogJ3Jvb3QnfSlcbmV4cG9ydCBjbGFzcyBNYXRJY29uUmVnaXN0cnkgaW1wbGVtZW50cyBPbkRlc3Ryb3kge1xuICBwcml2YXRlIF9kb2N1bWVudDogRG9jdW1lbnQ7XG5cbiAgLyoqXG4gICAqIFVSTHMgYW5kIGNhY2hlZCBTVkcgZWxlbWVudHMgZm9yIGluZGl2aWR1YWwgaWNvbnMuIEtleXMgYXJlIG9mIHRoZSBmb3JtYXQgXCJbbmFtZXNwYWNlXTpbaWNvbl1cIi5cbiAgICovXG4gIHByaXZhdGUgX3N2Z0ljb25Db25maWdzID0gbmV3IE1hcDxzdHJpbmcsIFN2Z0ljb25Db25maWc+KCk7XG5cbiAgLyoqXG4gICAqIFN2Z0ljb25Db25maWcgb2JqZWN0cyBhbmQgY2FjaGVkIFNWRyBlbGVtZW50cyBmb3IgaWNvbiBzZXRzLCBrZXllZCBieSBuYW1lc3BhY2UuXG4gICAqIE11bHRpcGxlIGljb24gc2V0cyBjYW4gYmUgcmVnaXN0ZXJlZCB1bmRlciB0aGUgc2FtZSBuYW1lc3BhY2UuXG4gICAqL1xuICBwcml2YXRlIF9pY29uU2V0Q29uZmlncyA9IG5ldyBNYXA8c3RyaW5nLCBTdmdJY29uQ29uZmlnW10+KCk7XG5cbiAgLyoqIENhY2hlIGZvciBpY29ucyBsb2FkZWQgYnkgZGlyZWN0IFVSTHMuICovXG4gIHByaXZhdGUgX2NhY2hlZEljb25zQnlVcmwgPSBuZXcgTWFwPHN0cmluZywgU1ZHRWxlbWVudD4oKTtcblxuICAvKiogSW4tcHJvZ3Jlc3MgaWNvbiBmZXRjaGVzLiBVc2VkIHRvIGNvYWxlc2NlIG11bHRpcGxlIHJlcXVlc3RzIHRvIHRoZSBzYW1lIFVSTC4gKi9cbiAgcHJpdmF0ZSBfaW5Qcm9ncmVzc1VybEZldGNoZXMgPSBuZXcgTWFwPHN0cmluZywgT2JzZXJ2YWJsZTxzdHJpbmc+PigpO1xuXG4gIC8qKiBNYXAgZnJvbSBmb250IGlkZW50aWZpZXJzIHRvIHRoZWlyIENTUyBjbGFzcyBuYW1lcy4gVXNlZCBmb3IgaWNvbiBmb250cy4gKi9cbiAgcHJpdmF0ZSBfZm9udENzc0NsYXNzZXNCeUFsaWFzID0gbmV3IE1hcDxzdHJpbmcsIHN0cmluZz4oKTtcblxuICAvKipcbiAgICogVGhlIENTUyBjbGFzcyB0byBhcHBseSB3aGVuIGFuIGA8bWF0LWljb24+YCBjb21wb25lbnQgaGFzIG5vIGljb24gbmFtZSwgdXJsLCBvciBmb250IHNwZWNpZmllZC5cbiAgICogVGhlIGRlZmF1bHQgJ21hdGVyaWFsLWljb25zJyB2YWx1ZSBhc3N1bWVzIHRoYXQgdGhlIG1hdGVyaWFsIGljb24gZm9udCBoYXMgYmVlbiBsb2FkZWQgYXNcbiAgICogZGVzY3JpYmVkIGF0IGh0dHA6Ly9nb29nbGUuZ2l0aHViLmlvL21hdGVyaWFsLWRlc2lnbi1pY29ucy8jaWNvbi1mb250LWZvci10aGUtd2ViXG4gICAqL1xuICBwcml2YXRlIF9kZWZhdWx0Rm9udFNldENsYXNzID0gJ21hdGVyaWFsLWljb25zJztcblxuICBjb25zdHJ1Y3RvcihcbiAgICBAT3B0aW9uYWwoKSBwcml2YXRlIF9odHRwQ2xpZW50OiBIdHRwQ2xpZW50LFxuICAgIHByaXZhdGUgX3Nhbml0aXplcjogRG9tU2FuaXRpemVyLFxuICAgIEBPcHRpb25hbCgpIEBJbmplY3QoRE9DVU1FTlQpIGRvY3VtZW50OiBhbnksXG4gICAgcHJpdmF0ZSByZWFkb25seSBfZXJyb3JIYW5kbGVyOiBFcnJvckhhbmRsZXIpIHtcbiAgICAgIHRoaXMuX2RvY3VtZW50ID0gZG9jdW1lbnQ7XG4gICAgfVxuXG4gIC8qKlxuICAgKiBSZWdpc3RlcnMgYW4gaWNvbiBieSBVUkwgaW4gdGhlIGRlZmF1bHQgbmFtZXNwYWNlLlxuICAgKiBAcGFyYW0gaWNvbk5hbWUgTmFtZSB1bmRlciB3aGljaCB0aGUgaWNvbiBzaG91bGQgYmUgcmVnaXN0ZXJlZC5cbiAgICogQHBhcmFtIHVybFxuICAgKi9cbiAgYWRkU3ZnSWNvbihpY29uTmFtZTogc3RyaW5nLCB1cmw6IFNhZmVSZXNvdXJjZVVybCwgb3B0aW9ucz86IEljb25PcHRpb25zKTogdGhpcyB7XG4gICAgcmV0dXJuIHRoaXMuYWRkU3ZnSWNvbkluTmFtZXNwYWNlKCcnLCBpY29uTmFtZSwgdXJsLCBvcHRpb25zKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZWdpc3RlcnMgYW4gaWNvbiB1c2luZyBhbiBIVE1MIHN0cmluZyBpbiB0aGUgZGVmYXVsdCBuYW1lc3BhY2UuXG4gICAqIEBwYXJhbSBpY29uTmFtZSBOYW1lIHVuZGVyIHdoaWNoIHRoZSBpY29uIHNob3VsZCBiZSByZWdpc3RlcmVkLlxuICAgKiBAcGFyYW0gbGl0ZXJhbCBTVkcgc291cmNlIG9mIHRoZSBpY29uLlxuICAgKi9cbiAgYWRkU3ZnSWNvbkxpdGVyYWwoaWNvbk5hbWU6IHN0cmluZywgbGl0ZXJhbDogU2FmZUh0bWwsIG9wdGlvbnM/OiBJY29uT3B0aW9ucyk6IHRoaXMge1xuICAgIHJldHVybiB0aGlzLmFkZFN2Z0ljb25MaXRlcmFsSW5OYW1lc3BhY2UoJycsIGljb25OYW1lLCBsaXRlcmFsLCBvcHRpb25zKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZWdpc3RlcnMgYW4gaWNvbiBieSBVUkwgaW4gdGhlIHNwZWNpZmllZCBuYW1lc3BhY2UuXG4gICAqIEBwYXJhbSBuYW1lc3BhY2UgTmFtZXNwYWNlIGluIHdoaWNoIHRoZSBpY29uIHNob3VsZCBiZSByZWdpc3RlcmVkLlxuICAgKiBAcGFyYW0gaWNvbk5hbWUgTmFtZSB1bmRlciB3aGljaCB0aGUgaWNvbiBzaG91bGQgYmUgcmVnaXN0ZXJlZC5cbiAgICogQHBhcmFtIHVybFxuICAgKi9cbiAgYWRkU3ZnSWNvbkluTmFtZXNwYWNlKG5hbWVzcGFjZTogc3RyaW5nLCBpY29uTmFtZTogc3RyaW5nLCB1cmw6IFNhZmVSZXNvdXJjZVVybCxcbiAgICAgICAgICAgICAgICAgICAgICAgIG9wdGlvbnM/OiBJY29uT3B0aW9ucyk6IHRoaXMge1xuICAgIHJldHVybiB0aGlzLl9hZGRTdmdJY29uQ29uZmlnKG5hbWVzcGFjZSwgaWNvbk5hbWUsIG5ldyBTdmdJY29uQ29uZmlnKHVybCwgb3B0aW9ucykpO1xuICB9XG5cbiAgLyoqXG4gICAqIFJlZ2lzdGVycyBhbiBpY29uIHVzaW5nIGFuIEhUTUwgc3RyaW5nIGluIHRoZSBzcGVjaWZpZWQgbmFtZXNwYWNlLlxuICAgKiBAcGFyYW0gbmFtZXNwYWNlIE5hbWVzcGFjZSBpbiB3aGljaCB0aGUgaWNvbiBzaG91bGQgYmUgcmVnaXN0ZXJlZC5cbiAgICogQHBhcmFtIGljb25OYW1lIE5hbWUgdW5kZXIgd2hpY2ggdGhlIGljb24gc2hvdWxkIGJlIHJlZ2lzdGVyZWQuXG4gICAqIEBwYXJhbSBsaXRlcmFsIFNWRyBzb3VyY2Ugb2YgdGhlIGljb24uXG4gICAqL1xuICBhZGRTdmdJY29uTGl0ZXJhbEluTmFtZXNwYWNlKG5hbWVzcGFjZTogc3RyaW5nLCBpY29uTmFtZTogc3RyaW5nLCBsaXRlcmFsOiBTYWZlSHRtbCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBvcHRpb25zPzogSWNvbk9wdGlvbnMpOiB0aGlzIHtcbiAgICBjb25zdCBzYW5pdGl6ZWRMaXRlcmFsID0gdGhpcy5fc2FuaXRpemVyLnNhbml0aXplKFNlY3VyaXR5Q29udGV4dC5IVE1MLCBsaXRlcmFsKTtcblxuICAgIGlmICghc2FuaXRpemVkTGl0ZXJhbCkge1xuICAgICAgdGhyb3cgZ2V0TWF0SWNvbkZhaWxlZFRvU2FuaXRpemVMaXRlcmFsRXJyb3IobGl0ZXJhbCk7XG4gICAgfVxuXG4gICAgY29uc3Qgc3ZnRWxlbWVudCA9IHRoaXMuX2NyZWF0ZVN2Z0VsZW1lbnRGb3JTaW5nbGVJY29uKHNhbml0aXplZExpdGVyYWwsIG9wdGlvbnMpO1xuICAgIHJldHVybiB0aGlzLl9hZGRTdmdJY29uQ29uZmlnKG5hbWVzcGFjZSwgaWNvbk5hbWUsIG5ldyBTdmdJY29uQ29uZmlnKHN2Z0VsZW1lbnQsIG9wdGlvbnMpKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZWdpc3RlcnMgYW4gaWNvbiBzZXQgYnkgVVJMIGluIHRoZSBkZWZhdWx0IG5hbWVzcGFjZS5cbiAgICogQHBhcmFtIHVybFxuICAgKi9cbiAgYWRkU3ZnSWNvblNldCh1cmw6IFNhZmVSZXNvdXJjZVVybCwgb3B0aW9ucz86IEljb25PcHRpb25zKTogdGhpcyB7XG4gICAgcmV0dXJuIHRoaXMuYWRkU3ZnSWNvblNldEluTmFtZXNwYWNlKCcnLCB1cmwsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqXG4gICAqIFJlZ2lzdGVycyBhbiBpY29uIHNldCB1c2luZyBhbiBIVE1MIHN0cmluZyBpbiB0aGUgZGVmYXVsdCBuYW1lc3BhY2UuXG4gICAqIEBwYXJhbSBsaXRlcmFsIFNWRyBzb3VyY2Ugb2YgdGhlIGljb24gc2V0LlxuICAgKi9cbiAgYWRkU3ZnSWNvblNldExpdGVyYWwobGl0ZXJhbDogU2FmZUh0bWwsIG9wdGlvbnM/OiBJY29uT3B0aW9ucyk6IHRoaXMge1xuICAgIHJldHVybiB0aGlzLmFkZFN2Z0ljb25TZXRMaXRlcmFsSW5OYW1lc3BhY2UoJycsIGxpdGVyYWwsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqXG4gICAqIFJlZ2lzdGVycyBhbiBpY29uIHNldCBieSBVUkwgaW4gdGhlIHNwZWNpZmllZCBuYW1lc3BhY2UuXG4gICAqIEBwYXJhbSBuYW1lc3BhY2UgTmFtZXNwYWNlIGluIHdoaWNoIHRvIHJlZ2lzdGVyIHRoZSBpY29uIHNldC5cbiAgICogQHBhcmFtIHVybFxuICAgKi9cbiAgYWRkU3ZnSWNvblNldEluTmFtZXNwYWNlKG5hbWVzcGFjZTogc3RyaW5nLCB1cmw6IFNhZmVSZXNvdXJjZVVybCwgb3B0aW9ucz86IEljb25PcHRpb25zKTogdGhpcyB7XG4gICAgcmV0dXJuIHRoaXMuX2FkZFN2Z0ljb25TZXRDb25maWcobmFtZXNwYWNlLCBuZXcgU3ZnSWNvbkNvbmZpZyh1cmwsIG9wdGlvbnMpKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZWdpc3RlcnMgYW4gaWNvbiBzZXQgdXNpbmcgYW4gSFRNTCBzdHJpbmcgaW4gdGhlIHNwZWNpZmllZCBuYW1lc3BhY2UuXG4gICAqIEBwYXJhbSBuYW1lc3BhY2UgTmFtZXNwYWNlIGluIHdoaWNoIHRvIHJlZ2lzdGVyIHRoZSBpY29uIHNldC5cbiAgICogQHBhcmFtIGxpdGVyYWwgU1ZHIHNvdXJjZSBvZiB0aGUgaWNvbiBzZXQuXG4gICAqL1xuICBhZGRTdmdJY29uU2V0TGl0ZXJhbEluTmFtZXNwYWNlKG5hbWVzcGFjZTogc3RyaW5nLCBsaXRlcmFsOiBTYWZlSHRtbCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBvcHRpb25zPzogSWNvbk9wdGlvbnMpOiB0aGlzIHtcbiAgICBjb25zdCBzYW5pdGl6ZWRMaXRlcmFsID0gdGhpcy5fc2FuaXRpemVyLnNhbml0aXplKFNlY3VyaXR5Q29udGV4dC5IVE1MLCBsaXRlcmFsKTtcblxuICAgIGlmICghc2FuaXRpemVkTGl0ZXJhbCkge1xuICAgICAgdGhyb3cgZ2V0TWF0SWNvbkZhaWxlZFRvU2FuaXRpemVMaXRlcmFsRXJyb3IobGl0ZXJhbCk7XG4gICAgfVxuXG4gICAgY29uc3Qgc3ZnRWxlbWVudCA9IHRoaXMuX3N2Z0VsZW1lbnRGcm9tU3RyaW5nKHNhbml0aXplZExpdGVyYWwpO1xuICAgIHJldHVybiB0aGlzLl9hZGRTdmdJY29uU2V0Q29uZmlnKG5hbWVzcGFjZSwgbmV3IFN2Z0ljb25Db25maWcoc3ZnRWxlbWVudCwgb3B0aW9ucykpO1xuICB9XG5cbiAgLyoqXG4gICAqIERlZmluZXMgYW4gYWxpYXMgZm9yIGEgQ1NTIGNsYXNzIG5hbWUgdG8gYmUgdXNlZCBmb3IgaWNvbiBmb250cy4gQ3JlYXRpbmcgYW4gbWF0SWNvblxuICAgKiBjb21wb25lbnQgd2l0aCB0aGUgYWxpYXMgYXMgdGhlIGZvbnRTZXQgaW5wdXQgd2lsbCBjYXVzZSB0aGUgY2xhc3MgbmFtZSB0byBiZSBhcHBsaWVkXG4gICAqIHRvIHRoZSBgPG1hdC1pY29uPmAgZWxlbWVudC5cbiAgICpcbiAgICogQHBhcmFtIGFsaWFzIEFsaWFzIGZvciB0aGUgZm9udC5cbiAgICogQHBhcmFtIGNsYXNzTmFtZSBDbGFzcyBuYW1lIG92ZXJyaWRlIHRvIGJlIHVzZWQgaW5zdGVhZCBvZiB0aGUgYWxpYXMuXG4gICAqL1xuICByZWdpc3RlckZvbnRDbGFzc0FsaWFzKGFsaWFzOiBzdHJpbmcsIGNsYXNzTmFtZTogc3RyaW5nID0gYWxpYXMpOiB0aGlzIHtcbiAgICB0aGlzLl9mb250Q3NzQ2xhc3Nlc0J5QWxpYXMuc2V0KGFsaWFzLCBjbGFzc05hbWUpO1xuICAgIHJldHVybiB0aGlzO1xuICB9XG5cbiAgLyoqXG4gICAqIFJldHVybnMgdGhlIENTUyBjbGFzcyBuYW1lIGFzc29jaWF0ZWQgd2l0aCB0aGUgYWxpYXMgYnkgYSBwcmV2aW91cyBjYWxsIHRvXG4gICAqIHJlZ2lzdGVyRm9udENsYXNzQWxpYXMuIElmIG5vIENTUyBjbGFzcyBoYXMgYmVlbiBhc3NvY2lhdGVkLCByZXR1cm5zIHRoZSBhbGlhcyB1bm1vZGlmaWVkLlxuICAgKi9cbiAgY2xhc3NOYW1lRm9yRm9udEFsaWFzKGFsaWFzOiBzdHJpbmcpOiBzdHJpbmcge1xuICAgIHJldHVybiB0aGlzLl9mb250Q3NzQ2xhc3Nlc0J5QWxpYXMuZ2V0KGFsaWFzKSB8fCBhbGlhcztcbiAgfVxuXG4gIC8qKlxuICAgKiBTZXRzIHRoZSBDU1MgY2xhc3MgbmFtZSB0byBiZSB1c2VkIGZvciBpY29uIGZvbnRzIHdoZW4gYW4gYDxtYXQtaWNvbj5gIGNvbXBvbmVudCBkb2VzIG5vdFxuICAgKiBoYXZlIGEgZm9udFNldCBpbnB1dCB2YWx1ZSwgYW5kIGlzIG5vdCBsb2FkaW5nIGFuIGljb24gYnkgbmFtZSBvciBVUkwuXG4gICAqXG4gICAqIEBwYXJhbSBjbGFzc05hbWVcbiAgICovXG4gIHNldERlZmF1bHRGb250U2V0Q2xhc3MoY2xhc3NOYW1lOiBzdHJpbmcpOiB0aGlzIHtcbiAgICB0aGlzLl9kZWZhdWx0Rm9udFNldENsYXNzID0gY2xhc3NOYW1lO1xuICAgIHJldHVybiB0aGlzO1xuICB9XG5cbiAgLyoqXG4gICAqIFJldHVybnMgdGhlIENTUyBjbGFzcyBuYW1lIHRvIGJlIHVzZWQgZm9yIGljb24gZm9udHMgd2hlbiBhbiBgPG1hdC1pY29uPmAgY29tcG9uZW50IGRvZXMgbm90XG4gICAqIGhhdmUgYSBmb250U2V0IGlucHV0IHZhbHVlLCBhbmQgaXMgbm90IGxvYWRpbmcgYW4gaWNvbiBieSBuYW1lIG9yIFVSTC5cbiAgICovXG4gIGdldERlZmF1bHRGb250U2V0Q2xhc3MoKTogc3RyaW5nIHtcbiAgICByZXR1cm4gdGhpcy5fZGVmYXVsdEZvbnRTZXRDbGFzcztcbiAgfVxuXG4gIC8qKlxuICAgKiBSZXR1cm5zIGFuIE9ic2VydmFibGUgdGhhdCBwcm9kdWNlcyB0aGUgaWNvbiAoYXMgYW4gYDxzdmc+YCBET00gZWxlbWVudCkgZnJvbSB0aGUgZ2l2ZW4gVVJMLlxuICAgKiBUaGUgcmVzcG9uc2UgZnJvbSB0aGUgVVJMIG1heSBiZSBjYWNoZWQgc28gdGhpcyB3aWxsIG5vdCBhbHdheXMgY2F1c2UgYW4gSFRUUCByZXF1ZXN0LCBidXRcbiAgICogdGhlIHByb2R1Y2VkIGVsZW1lbnQgd2lsbCBhbHdheXMgYmUgYSBuZXcgY29weSBvZiB0aGUgb3JpZ2luYWxseSBmZXRjaGVkIGljb24uIChUaGF0IGlzLFxuICAgKiBpdCB3aWxsIG5vdCBjb250YWluIGFueSBtb2RpZmljYXRpb25zIG1hZGUgdG8gZWxlbWVudHMgcHJldmlvdXNseSByZXR1cm5lZCkuXG4gICAqXG4gICAqIEBwYXJhbSBzYWZlVXJsIFVSTCBmcm9tIHdoaWNoIHRvIGZldGNoIHRoZSBTVkcgaWNvbi5cbiAgICovXG4gIGdldFN2Z0ljb25Gcm9tVXJsKHNhZmVVcmw6IFNhZmVSZXNvdXJjZVVybCk6IE9ic2VydmFibGU8U1ZHRWxlbWVudD4ge1xuICAgIGNvbnN0IHVybCA9IHRoaXMuX3Nhbml0aXplci5zYW5pdGl6ZShTZWN1cml0eUNvbnRleHQuUkVTT1VSQ0VfVVJMLCBzYWZlVXJsKTtcblxuICAgIGlmICghdXJsKSB7XG4gICAgICB0aHJvdyBnZXRNYXRJY29uRmFpbGVkVG9TYW5pdGl6ZVVybEVycm9yKHNhZmVVcmwpO1xuICAgIH1cblxuICAgIGNvbnN0IGNhY2hlZEljb24gPSB0aGlzLl9jYWNoZWRJY29uc0J5VXJsLmdldCh1cmwpO1xuXG4gICAgaWYgKGNhY2hlZEljb24pIHtcbiAgICAgIHJldHVybiBvYnNlcnZhYmxlT2YoY2xvbmVTdmcoY2FjaGVkSWNvbikpO1xuICAgIH1cblxuICAgIHJldHVybiB0aGlzLl9sb2FkU3ZnSWNvbkZyb21Db25maWcobmV3IFN2Z0ljb25Db25maWcoc2FmZVVybCkpLnBpcGUoXG4gICAgICB0YXAoc3ZnID0+IHRoaXMuX2NhY2hlZEljb25zQnlVcmwuc2V0KHVybCEsIHN2ZykpLFxuICAgICAgbWFwKHN2ZyA9PiBjbG9uZVN2ZyhzdmcpKSxcbiAgICApO1xuICB9XG5cbiAgLyoqXG4gICAqIFJldHVybnMgYW4gT2JzZXJ2YWJsZSB0aGF0IHByb2R1Y2VzIHRoZSBpY29uIChhcyBhbiBgPHN2Zz5gIERPTSBlbGVtZW50KSB3aXRoIHRoZSBnaXZlbiBuYW1lXG4gICAqIGFuZCBuYW1lc3BhY2UuIFRoZSBpY29uIG11c3QgaGF2ZSBiZWVuIHByZXZpb3VzbHkgcmVnaXN0ZXJlZCB3aXRoIGFkZEljb24gb3IgYWRkSWNvblNldDtcbiAgICogaWYgbm90LCB0aGUgT2JzZXJ2YWJsZSB3aWxsIHRocm93IGFuIGVycm9yLlxuICAgKlxuICAgKiBAcGFyYW0gbmFtZSBOYW1lIG9mIHRoZSBpY29uIHRvIGJlIHJldHJpZXZlZC5cbiAgICogQHBhcmFtIG5hbWVzcGFjZSBOYW1lc3BhY2UgaW4gd2hpY2ggdG8gbG9vayBmb3IgdGhlIGljb24uXG4gICAqL1xuICBnZXROYW1lZFN2Z0ljb24obmFtZTogc3RyaW5nLCBuYW1lc3BhY2U6IHN0cmluZyA9ICcnKTogT2JzZXJ2YWJsZTxTVkdFbGVtZW50PiB7XG4gICAgLy8gUmV0dXJuIChjb3B5IG9mKSBjYWNoZWQgaWNvbiBpZiBwb3NzaWJsZS5cbiAgICBjb25zdCBrZXkgPSBpY29uS2V5KG5hbWVzcGFjZSwgbmFtZSk7XG4gICAgY29uc3QgY29uZmlnID0gdGhpcy5fc3ZnSWNvbkNvbmZpZ3MuZ2V0KGtleSk7XG5cbiAgICBpZiAoY29uZmlnKSB7XG4gICAgICByZXR1cm4gdGhpcy5fZ2V0U3ZnRnJvbUNvbmZpZyhjb25maWcpO1xuICAgIH1cblxuICAgIC8vIFNlZSBpZiB3ZSBoYXZlIGFueSBpY29uIHNldHMgcmVnaXN0ZXJlZCBmb3IgdGhlIG5hbWVzcGFjZS5cbiAgICBjb25zdCBpY29uU2V0Q29uZmlncyA9IHRoaXMuX2ljb25TZXRDb25maWdzLmdldChuYW1lc3BhY2UpO1xuXG4gICAgaWYgKGljb25TZXRDb25maWdzKSB7XG4gICAgICByZXR1cm4gdGhpcy5fZ2V0U3ZnRnJvbUljb25TZXRDb25maWdzKG5hbWUsIGljb25TZXRDb25maWdzKTtcbiAgICB9XG5cbiAgICByZXR1cm4gb2JzZXJ2YWJsZVRocm93KGdldE1hdEljb25OYW1lTm90Rm91bmRFcnJvcihrZXkpKTtcbiAgfVxuXG4gIG5nT25EZXN0cm95KCkge1xuICAgdGhpcy5fc3ZnSWNvbkNvbmZpZ3MuY2xlYXIoKTtcbiAgIHRoaXMuX2ljb25TZXRDb25maWdzLmNsZWFyKCk7XG4gICB0aGlzLl9jYWNoZWRJY29uc0J5VXJsLmNsZWFyKCk7XG4gIH1cblxuICAvKipcbiAgICogUmV0dXJucyB0aGUgY2FjaGVkIGljb24gZm9yIGEgU3ZnSWNvbkNvbmZpZyBpZiBhdmFpbGFibGUsIG9yIGZldGNoZXMgaXQgZnJvbSBpdHMgVVJMIGlmIG5vdC5cbiAgICovXG4gIHByaXZhdGUgX2dldFN2Z0Zyb21Db25maWcoY29uZmlnOiBTdmdJY29uQ29uZmlnKTogT2JzZXJ2YWJsZTxTVkdFbGVtZW50PiB7XG4gICAgaWYgKGNvbmZpZy5zdmdFbGVtZW50KSB7XG4gICAgICAvLyBXZSBhbHJlYWR5IGhhdmUgdGhlIFNWRyBlbGVtZW50IGZvciB0aGlzIGljb24sIHJldHVybiBhIGNvcHkuXG4gICAgICByZXR1cm4gb2JzZXJ2YWJsZU9mKGNsb25lU3ZnKGNvbmZpZy5zdmdFbGVtZW50KSk7XG4gICAgfSBlbHNlIHtcbiAgICAgIC8vIEZldGNoIHRoZSBpY29uIGZyb20gdGhlIGNvbmZpZydzIFVSTCwgY2FjaGUgaXQsIGFuZCByZXR1cm4gYSBjb3B5LlxuICAgICAgcmV0dXJuIHRoaXMuX2xvYWRTdmdJY29uRnJvbUNvbmZpZyhjb25maWcpLnBpcGUoXG4gICAgICAgIHRhcChzdmcgPT4gY29uZmlnLnN2Z0VsZW1lbnQgPSBzdmcpLFxuICAgICAgICBtYXAoc3ZnID0+IGNsb25lU3ZnKHN2ZykpLFxuICAgICAgKTtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogQXR0ZW1wdHMgdG8gZmluZCBhbiBpY29uIHdpdGggdGhlIHNwZWNpZmllZCBuYW1lIGluIGFueSBvZiB0aGUgU1ZHIGljb24gc2V0cy5cbiAgICogRmlyc3Qgc2VhcmNoZXMgdGhlIGF2YWlsYWJsZSBjYWNoZWQgaWNvbnMgZm9yIGEgbmVzdGVkIGVsZW1lbnQgd2l0aCBhIG1hdGNoaW5nIG5hbWUsIGFuZFxuICAgKiBpZiBmb3VuZCBjb3BpZXMgdGhlIGVsZW1lbnQgdG8gYSBuZXcgYDxzdmc+YCBlbGVtZW50LiBJZiBub3QgZm91bmQsIGZldGNoZXMgYWxsIGljb24gc2V0c1xuICAgKiB0aGF0IGhhdmUgbm90IGJlZW4gY2FjaGVkLCBhbmQgc2VhcmNoZXMgYWdhaW4gYWZ0ZXIgYWxsIGZldGNoZXMgYXJlIGNvbXBsZXRlZC5cbiAgICogVGhlIHJldHVybmVkIE9ic2VydmFibGUgcHJvZHVjZXMgdGhlIFNWRyBlbGVtZW50IGlmIHBvc3NpYmxlLCBhbmQgdGhyb3dzXG4gICAqIGFuIGVycm9yIGlmIG5vIGljb24gd2l0aCB0aGUgc3BlY2lmaWVkIG5hbWUgY2FuIGJlIGZvdW5kLlxuICAgKi9cbiAgcHJpdmF0ZSBfZ2V0U3ZnRnJvbUljb25TZXRDb25maWdzKG5hbWU6IHN0cmluZywgaWNvblNldENvbmZpZ3M6IFN2Z0ljb25Db25maWdbXSk6XG4gICAgICBPYnNlcnZhYmxlPFNWR0VsZW1lbnQ+IHtcbiAgICAvLyBGb3IgYWxsIHRoZSBpY29uIHNldCBTVkcgZWxlbWVudHMgd2UndmUgZmV0Y2hlZCwgc2VlIGlmIGFueSBjb250YWluIGFuIGljb24gd2l0aCB0aGVcbiAgICAvLyByZXF1ZXN0ZWQgbmFtZS5cbiAgICBjb25zdCBuYW1lZEljb24gPSB0aGlzLl9leHRyYWN0SWNvbldpdGhOYW1lRnJvbUFueVNldChuYW1lLCBpY29uU2V0Q29uZmlncyk7XG5cbiAgICBpZiAobmFtZWRJY29uKSB7XG4gICAgICAvLyBXZSBjb3VsZCBjYWNoZSBuYW1lZEljb24gaW4gX3N2Z0ljb25Db25maWdzLCBidXQgc2luY2Ugd2UgaGF2ZSB0byBtYWtlIGEgY29weSBldmVyeVxuICAgICAgLy8gdGltZSBhbnl3YXksIHRoZXJlJ3MgcHJvYmFibHkgbm90IG11Y2ggYWR2YW50YWdlIGNvbXBhcmVkIHRvIGp1c3QgYWx3YXlzIGV4dHJhY3RpbmdcbiAgICAgIC8vIGl0IGZyb20gdGhlIGljb24gc2V0LlxuICAgICAgcmV0dXJuIG9ic2VydmFibGVPZihuYW1lZEljb24pO1xuICAgIH1cblxuICAgIC8vIE5vdCBmb3VuZCBpbiBhbnkgY2FjaGVkIGljb24gc2V0cy4gSWYgdGhlcmUgYXJlIGljb24gc2V0cyB3aXRoIFVSTHMgdGhhdCB3ZSBoYXZlbid0XG4gICAgLy8gZmV0Y2hlZCwgZmV0Y2ggdGhlbSBub3cgYW5kIGxvb2sgZm9yIGljb25OYW1lIGluIHRoZSByZXN1bHRzLlxuICAgIGNvbnN0IGljb25TZXRGZXRjaFJlcXVlc3RzOiBPYnNlcnZhYmxlPFNWR0VsZW1lbnQgfCBudWxsPltdID0gaWNvblNldENvbmZpZ3NcbiAgICAgIC5maWx0ZXIoaWNvblNldENvbmZpZyA9PiAhaWNvblNldENvbmZpZy5zdmdFbGVtZW50KVxuICAgICAgLm1hcChpY29uU2V0Q29uZmlnID0+IHtcbiAgICAgICAgcmV0dXJuIHRoaXMuX2xvYWRTdmdJY29uU2V0RnJvbUNvbmZpZyhpY29uU2V0Q29uZmlnKS5waXBlKFxuICAgICAgICAgIGNhdGNoRXJyb3IoKGVycjogSHR0cEVycm9yUmVzcG9uc2UpOiBPYnNlcnZhYmxlPFNWR0VsZW1lbnQgfCBudWxsPiA9PiB7XG4gICAgICAgICAgICBjb25zdCB1cmwgPSB0aGlzLl9zYW5pdGl6ZXIuc2FuaXRpemUoU2VjdXJpdHlDb250ZXh0LlJFU09VUkNFX1VSTCwgaWNvblNldENvbmZpZy51cmwpO1xuXG4gICAgICAgICAgICAvLyBTd2FsbG93IGVycm9ycyBmZXRjaGluZyBpbmRpdmlkdWFsIFVSTHMgc28gdGhlXG4gICAgICAgICAgICAvLyBjb21iaW5lZCBPYnNlcnZhYmxlIHdvbid0IG5lY2Vzc2FyaWx5IGZhaWwuXG4gICAgICAgICAgICBjb25zdCBlcnJvck1lc3NhZ2UgPSBgTG9hZGluZyBpY29uIHNldCBVUkw6ICR7dXJsfSBmYWlsZWQ6ICR7ZXJyLm1lc3NhZ2V9YDtcbiAgICAgICAgICAgIHRoaXMuX2Vycm9ySGFuZGxlci5oYW5kbGVFcnJvcihuZXcgRXJyb3IoZXJyb3JNZXNzYWdlKSk7XG4gICAgICAgICAgICByZXR1cm4gb2JzZXJ2YWJsZU9mKG51bGwpO1xuICAgICAgICAgIH0pXG4gICAgICAgICk7XG4gICAgICB9KTtcblxuICAgIC8vIEZldGNoIGFsbCB0aGUgaWNvbiBzZXQgVVJMcy4gV2hlbiB0aGUgcmVxdWVzdHMgY29tcGxldGUsIGV2ZXJ5IEljb25TZXQgc2hvdWxkIGhhdmUgYVxuICAgIC8vIGNhY2hlZCBTVkcgZWxlbWVudCAodW5sZXNzIHRoZSByZXF1ZXN0IGZhaWxlZCksIGFuZCB3ZSBjYW4gY2hlY2sgYWdhaW4gZm9yIHRoZSBpY29uLlxuICAgIHJldHVybiBmb3JrSm9pbihpY29uU2V0RmV0Y2hSZXF1ZXN0cykucGlwZShtYXAoKCkgPT4ge1xuICAgICAgY29uc3QgZm91bmRJY29uID0gdGhpcy5fZXh0cmFjdEljb25XaXRoTmFtZUZyb21BbnlTZXQobmFtZSwgaWNvblNldENvbmZpZ3MpO1xuXG4gICAgICBpZiAoIWZvdW5kSWNvbikge1xuICAgICAgICB0aHJvdyBnZXRNYXRJY29uTmFtZU5vdEZvdW5kRXJyb3IobmFtZSk7XG4gICAgICB9XG5cbiAgICAgIHJldHVybiBmb3VuZEljb247XG4gICAgfSkpO1xuICB9XG5cbiAgLyoqXG4gICAqIFNlYXJjaGVzIHRoZSBjYWNoZWQgU1ZHIGVsZW1lbnRzIGZvciB0aGUgZ2l2ZW4gaWNvbiBzZXRzIGZvciBhIG5lc3RlZCBpY29uIGVsZW1lbnQgd2hvc2UgXCJpZFwiXG4gICAqIHRhZyBtYXRjaGVzIHRoZSBzcGVjaWZpZWQgbmFtZS4gSWYgZm91bmQsIGNvcGllcyB0aGUgbmVzdGVkIGVsZW1lbnQgdG8gYSBuZXcgU1ZHIGVsZW1lbnQgYW5kXG4gICAqIHJldHVybnMgaXQuIFJldHVybnMgbnVsbCBpZiBubyBtYXRjaGluZyBlbGVtZW50IGlzIGZvdW5kLlxuICAgKi9cbiAgcHJpdmF0ZSBfZXh0cmFjdEljb25XaXRoTmFtZUZyb21BbnlTZXQoaWNvbk5hbWU6IHN0cmluZywgaWNvblNldENvbmZpZ3M6IFN2Z0ljb25Db25maWdbXSk6XG4gICAgICBTVkdFbGVtZW50IHwgbnVsbCB7XG4gICAgLy8gSXRlcmF0ZSBiYWNrd2FyZHMsIHNvIGljb24gc2V0cyBhZGRlZCBsYXRlciBoYXZlIHByZWNlZGVuY2UuXG4gICAgZm9yIChsZXQgaSA9IGljb25TZXRDb25maWdzLmxlbmd0aCAtIDE7IGkgPj0gMDsgaS0tKSB7XG4gICAgICBjb25zdCBjb25maWcgPSBpY29uU2V0Q29uZmlnc1tpXTtcbiAgICAgIGlmIChjb25maWcuc3ZnRWxlbWVudCkge1xuICAgICAgICBjb25zdCBmb3VuZEljb24gPSB0aGlzLl9leHRyYWN0U3ZnSWNvbkZyb21TZXQoY29uZmlnLnN2Z0VsZW1lbnQsIGljb25OYW1lLCBjb25maWcub3B0aW9ucyk7XG4gICAgICAgIGlmIChmb3VuZEljb24pIHtcbiAgICAgICAgICByZXR1cm4gZm91bmRJY29uO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiBudWxsO1xuICB9XG5cbiAgLyoqXG4gICAqIExvYWRzIHRoZSBjb250ZW50IG9mIHRoZSBpY29uIFVSTCBzcGVjaWZpZWQgaW4gdGhlIFN2Z0ljb25Db25maWcgYW5kIGNyZWF0ZXMgYW4gU1ZHIGVsZW1lbnRcbiAgICogZnJvbSBpdC5cbiAgICovXG4gIHByaXZhdGUgX2xvYWRTdmdJY29uRnJvbUNvbmZpZyhjb25maWc6IFN2Z0ljb25Db25maWcpOiBPYnNlcnZhYmxlPFNWR0VsZW1lbnQ+IHtcbiAgICByZXR1cm4gdGhpcy5fZmV0Y2hJY29uKGNvbmZpZylcbiAgICAgICAgLnBpcGUobWFwKHN2Z1RleHQgPT4gdGhpcy5fY3JlYXRlU3ZnRWxlbWVudEZvclNpbmdsZUljb24oc3ZnVGV4dCwgY29uZmlnLm9wdGlvbnMpKSk7XG4gIH1cblxuICAvKipcbiAgICogTG9hZHMgdGhlIGNvbnRlbnQgb2YgdGhlIGljb24gc2V0IFVSTCBzcGVjaWZpZWQgaW4gdGhlIFN2Z0ljb25Db25maWcgYW5kIGNyZWF0ZXMgYW4gU1ZHIGVsZW1lbnRcbiAgICogZnJvbSBpdC5cbiAgICovXG4gIHByaXZhdGUgX2xvYWRTdmdJY29uU2V0RnJvbUNvbmZpZyhjb25maWc6IFN2Z0ljb25Db25maWcpOiBPYnNlcnZhYmxlPFNWR0VsZW1lbnQ+IHtcbiAgICAvLyBJZiB0aGUgU1ZHIGZvciB0aGlzIGljb24gc2V0IGhhcyBhbHJlYWR5IGJlZW4gcGFyc2VkLCBkbyBub3RoaW5nLlxuICAgIGlmIChjb25maWcuc3ZnRWxlbWVudCkge1xuICAgICAgcmV0dXJuIG9ic2VydmFibGVPZihjb25maWcuc3ZnRWxlbWVudCk7XG4gICAgfVxuXG4gICAgcmV0dXJuIHRoaXMuX2ZldGNoSWNvbihjb25maWcpLnBpcGUobWFwKHN2Z1RleHQgPT4ge1xuICAgICAgLy8gSXQgaXMgcG9zc2libGUgdGhhdCB0aGUgaWNvbiBzZXQgd2FzIHBhcnNlZCBhbmQgY2FjaGVkIGJ5IGFuIGVhcmxpZXIgcmVxdWVzdCwgc28gcGFyc2luZ1xuICAgICAgLy8gb25seSBuZWVkcyB0byBvY2N1ciBpZiB0aGUgY2FjaGUgaXMgeWV0IHVuc2V0LlxuICAgICAgaWYgKCFjb25maWcuc3ZnRWxlbWVudCkge1xuICAgICAgICBjb25maWcuc3ZnRWxlbWVudCA9IHRoaXMuX3N2Z0VsZW1lbnRGcm9tU3RyaW5nKHN2Z1RleHQpO1xuICAgICAgfVxuXG4gICAgICByZXR1cm4gY29uZmlnLnN2Z0VsZW1lbnQ7XG4gICAgfSkpO1xuICB9XG5cbiAgLyoqXG4gICAqIENyZWF0ZXMgYSBET00gZWxlbWVudCBmcm9tIHRoZSBnaXZlbiBTVkcgc3RyaW5nLCBhbmQgYWRkcyBkZWZhdWx0IGF0dHJpYnV0ZXMuXG4gICAqL1xuICBwcml2YXRlIF9jcmVhdGVTdmdFbGVtZW50Rm9yU2luZ2xlSWNvbihyZXNwb25zZVRleHQ6IHN0cmluZywgb3B0aW9ucz86IEljb25PcHRpb25zKTogU1ZHRWxlbWVudCB7XG4gICAgY29uc3Qgc3ZnID0gdGhpcy5fc3ZnRWxlbWVudEZyb21TdHJpbmcocmVzcG9uc2VUZXh0KTtcbiAgICB0aGlzLl9zZXRTdmdBdHRyaWJ1dGVzKHN2Zywgb3B0aW9ucyk7XG4gICAgcmV0dXJuIHN2ZztcbiAgfVxuXG4gIC8qKlxuICAgKiBTZWFyY2hlcyB0aGUgY2FjaGVkIGVsZW1lbnQgb2YgdGhlIGdpdmVuIFN2Z0ljb25Db25maWcgZm9yIGEgbmVzdGVkIGljb24gZWxlbWVudCB3aG9zZSBcImlkXCJcbiAgICogdGFnIG1hdGNoZXMgdGhlIHNwZWNpZmllZCBuYW1lLiBJZiBmb3VuZCwgY29waWVzIHRoZSBuZXN0ZWQgZWxlbWVudCB0byBhIG5ldyBTVkcgZWxlbWVudCBhbmRcbiAgICogcmV0dXJucyBpdC4gUmV0dXJucyBudWxsIGlmIG5vIG1hdGNoaW5nIGVsZW1lbnQgaXMgZm91bmQuXG4gICAqL1xuICBwcml2YXRlIF9leHRyYWN0U3ZnSWNvbkZyb21TZXQoaWNvblNldDogU1ZHRWxlbWVudCwgaWNvbk5hbWU6IHN0cmluZyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG9wdGlvbnM/OiBJY29uT3B0aW9ucyk6IFNWR0VsZW1lbnQgfCBudWxsIHtcbiAgICAvLyBVc2UgdGhlIGBpZD1cImljb25OYW1lXCJgIHN5bnRheCBpbiBvcmRlciB0byBlc2NhcGUgc3BlY2lhbFxuICAgIC8vIGNoYXJhY3RlcnMgaW4gdGhlIElEICh2ZXJzdXMgdXNpbmcgdGhlICNpY29uTmFtZSBzeW50YXgpLlxuICAgIGNvbnN0IGljb25Tb3VyY2UgPSBpY29uU2V0LnF1ZXJ5U2VsZWN0b3IoYFtpZD1cIiR7aWNvbk5hbWV9XCJdYCk7XG5cbiAgICBpZiAoIWljb25Tb3VyY2UpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIC8vIENsb25lIHRoZSBlbGVtZW50IGFuZCByZW1vdmUgdGhlIElEIHRvIHByZXZlbnQgbXVsdGlwbGUgZWxlbWVudHMgZnJvbSBiZWluZyBhZGRlZFxuICAgIC8vIHRvIHRoZSBwYWdlIHdpdGggdGhlIHNhbWUgSUQuXG4gICAgY29uc3QgaWNvbkVsZW1lbnQgPSBpY29uU291cmNlLmNsb25lTm9kZSh0cnVlKSBhcyBFbGVtZW50O1xuICAgIGljb25FbGVtZW50LnJlbW92ZUF0dHJpYnV0ZSgnaWQnKTtcblxuICAgIC8vIElmIHRoZSBpY29uIG5vZGUgaXMgaXRzZWxmIGFuIDxzdmc+IG5vZGUsIGNsb25lIGFuZCByZXR1cm4gaXQgZGlyZWN0bHkuIElmIG5vdCwgc2V0IGl0IGFzXG4gICAgLy8gdGhlIGNvbnRlbnQgb2YgYSBuZXcgPHN2Zz4gbm9kZS5cbiAgICBpZiAoaWNvbkVsZW1lbnQubm9kZU5hbWUudG9Mb3dlckNhc2UoKSA9PT0gJ3N2ZycpIHtcbiAgICAgIHJldHVybiB0aGlzLl9zZXRTdmdBdHRyaWJ1dGVzKGljb25FbGVtZW50IGFzIFNWR0VsZW1lbnQsIG9wdGlvbnMpO1xuICAgIH1cblxuICAgIC8vIElmIHRoZSBub2RlIGlzIGEgPHN5bWJvbD4sIGl0IHdvbid0IGJlIHJlbmRlcmVkIHNvIHdlIGhhdmUgdG8gY29udmVydCBpdCBpbnRvIDxzdmc+LiBOb3RlXG4gICAgLy8gdGhhdCB0aGUgc2FtZSBjb3VsZCBiZSBhY2hpZXZlZCBieSByZWZlcnJpbmcgdG8gaXQgdmlhIDx1c2UgaHJlZj1cIiNpZFwiPiwgaG93ZXZlciB0aGUgPHVzZT5cbiAgICAvLyB0YWcgaXMgcHJvYmxlbWF0aWMgb24gRmlyZWZveCwgYmVjYXVzZSBpdCBuZWVkcyB0byBpbmNsdWRlIHRoZSBjdXJyZW50IHBhZ2UgcGF0aC5cbiAgICBpZiAoaWNvbkVsZW1lbnQubm9kZU5hbWUudG9Mb3dlckNhc2UoKSA9PT0gJ3N5bWJvbCcpIHtcbiAgICAgIHJldHVybiB0aGlzLl9zZXRTdmdBdHRyaWJ1dGVzKHRoaXMuX3RvU3ZnRWxlbWVudChpY29uRWxlbWVudCksIG9wdGlvbnMpO1xuICAgIH1cblxuICAgIC8vIGNyZWF0ZUVsZW1lbnQoJ1NWRycpIGRvZXNuJ3Qgd29yayBhcyBleHBlY3RlZDsgdGhlIERPTSBlbmRzIHVwIHdpdGhcbiAgICAvLyB0aGUgY29ycmVjdCBub2RlcywgYnV0IHRoZSBTVkcgY29udGVudCBkb2Vzbid0IHJlbmRlci4gSW5zdGVhZCB3ZVxuICAgIC8vIGhhdmUgdG8gY3JlYXRlIGFuIGVtcHR5IFNWRyBub2RlIHVzaW5nIGlubmVySFRNTCBhbmQgYXBwZW5kIGl0cyBjb250ZW50LlxuICAgIC8vIEVsZW1lbnRzIGNyZWF0ZWQgdXNpbmcgRE9NUGFyc2VyLnBhcnNlRnJvbVN0cmluZyBoYXZlIHRoZSBzYW1lIHByb2JsZW0uXG4gICAgLy8gaHR0cDovL3N0YWNrb3ZlcmZsb3cuY29tL3F1ZXN0aW9ucy8yMzAwMzI3OC9zdmctaW5uZXJodG1sLWluLWZpcmVmb3gtY2FuLW5vdC1kaXNwbGF5XG4gICAgY29uc3Qgc3ZnID0gdGhpcy5fc3ZnRWxlbWVudEZyb21TdHJpbmcoJzxzdmc+PC9zdmc+Jyk7XG4gICAgLy8gQ2xvbmUgdGhlIG5vZGUgc28gd2UgZG9uJ3QgcmVtb3ZlIGl0IGZyb20gdGhlIHBhcmVudCBpY29uIHNldCBlbGVtZW50LlxuICAgIHN2Zy5hcHBlbmRDaGlsZChpY29uRWxlbWVudCk7XG5cbiAgICByZXR1cm4gdGhpcy5fc2V0U3ZnQXR0cmlidXRlcyhzdmcsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqXG4gICAqIENyZWF0ZXMgYSBET00gZWxlbWVudCBmcm9tIHRoZSBnaXZlbiBTVkcgc3RyaW5nLlxuICAgKi9cbiAgcHJpdmF0ZSBfc3ZnRWxlbWVudEZyb21TdHJpbmcoc3RyOiBzdHJpbmcpOiBTVkdFbGVtZW50IHtcbiAgICBjb25zdCBkaXYgPSB0aGlzLl9kb2N1bWVudC5jcmVhdGVFbGVtZW50KCdESVYnKTtcbiAgICBkaXYuaW5uZXJIVE1MID0gc3RyO1xuICAgIGNvbnN0IHN2ZyA9IGRpdi5xdWVyeVNlbGVjdG9yKCdzdmcnKSBhcyBTVkdFbGVtZW50O1xuXG4gICAgaWYgKCFzdmcpIHtcbiAgICAgIHRocm93IEVycm9yKCc8c3ZnPiB0YWcgbm90IGZvdW5kJyk7XG4gICAgfVxuXG4gICAgcmV0dXJuIHN2ZztcbiAgfVxuXG4gIC8qKlxuICAgKiBDb252ZXJ0cyBhbiBlbGVtZW50IGludG8gYW4gU1ZHIG5vZGUgYnkgY2xvbmluZyBhbGwgb2YgaXRzIGNoaWxkcmVuLlxuICAgKi9cbiAgcHJpdmF0ZSBfdG9TdmdFbGVtZW50KGVsZW1lbnQ6IEVsZW1lbnQpOiBTVkdFbGVtZW50IHtcbiAgICBjb25zdCBzdmcgPSB0aGlzLl9zdmdFbGVtZW50RnJvbVN0cmluZygnPHN2Zz48L3N2Zz4nKTtcbiAgICBjb25zdCBhdHRyaWJ1dGVzID0gZWxlbWVudC5hdHRyaWJ1dGVzO1xuXG4gICAgLy8gQ29weSBvdmVyIGFsbCB0aGUgYXR0cmlidXRlcyBmcm9tIHRoZSBgc3ltYm9sYCB0byB0aGUgbmV3IFNWRywgZXhjZXB0IHRoZSBpZC5cbiAgICBmb3IgKGxldCBpID0gMDsgaSA8IGF0dHJpYnV0ZXMubGVuZ3RoOyBpKyspIHtcbiAgICAgIGNvbnN0IHtuYW1lLCB2YWx1ZX0gPSBhdHRyaWJ1dGVzW2ldO1xuXG4gICAgICBpZiAobmFtZSAhPT0gJ2lkJykge1xuICAgICAgICBzdmcuc2V0QXR0cmlidXRlKG5hbWUsIHZhbHVlKTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICBmb3IgKGxldCBpID0gMDsgaSA8IGVsZW1lbnQuY2hpbGROb2Rlcy5sZW5ndGg7IGkrKykge1xuICAgICAgaWYgKGVsZW1lbnQuY2hpbGROb2Rlc1tpXS5ub2RlVHlwZSA9PT0gdGhpcy5fZG9jdW1lbnQuRUxFTUVOVF9OT0RFKSB7XG4gICAgICAgIHN2Zy5hcHBlbmRDaGlsZChlbGVtZW50LmNoaWxkTm9kZXNbaV0uY2xvbmVOb2RlKHRydWUpKTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICByZXR1cm4gc3ZnO1xuICB9XG5cbiAgLyoqXG4gICAqIFNldHMgdGhlIGRlZmF1bHQgYXR0cmlidXRlcyBmb3IgYW4gU1ZHIGVsZW1lbnQgdG8gYmUgdXNlZCBhcyBhbiBpY29uLlxuICAgKi9cbiAgcHJpdmF0ZSBfc2V0U3ZnQXR0cmlidXRlcyhzdmc6IFNWR0VsZW1lbnQsIG9wdGlvbnM/OiBJY29uT3B0aW9ucyk6IFNWR0VsZW1lbnQge1xuICAgIHN2Zy5zZXRBdHRyaWJ1dGUoJ2ZpdCcsICcnKTtcbiAgICBzdmcuc2V0QXR0cmlidXRlKCdoZWlnaHQnLCAnMTAwJScpO1xuICAgIHN2Zy5zZXRBdHRyaWJ1dGUoJ3dpZHRoJywgJzEwMCUnKTtcbiAgICBzdmcuc2V0QXR0cmlidXRlKCdwcmVzZXJ2ZUFzcGVjdFJhdGlvJywgJ3hNaWRZTWlkIG1lZXQnKTtcbiAgICBzdmcuc2V0QXR0cmlidXRlKCdmb2N1c2FibGUnLCAnZmFsc2UnKTsgLy8gRGlzYWJsZSBJRTExIGRlZmF1bHQgYmVoYXZpb3IgdG8gbWFrZSBTVkdzIGZvY3VzYWJsZS5cblxuICAgIGlmIChvcHRpb25zICYmIG9wdGlvbnMudmlld0JveCkge1xuICAgICAgc3ZnLnNldEF0dHJpYnV0ZSgndmlld0JveCcsIG9wdGlvbnMudmlld0JveCk7XG4gICAgfVxuXG4gICAgcmV0dXJuIHN2ZztcbiAgfVxuXG4gIC8qKlxuICAgKiBSZXR1cm5zIGFuIE9ic2VydmFibGUgd2hpY2ggcHJvZHVjZXMgdGhlIHN0cmluZyBjb250ZW50cyBvZiB0aGUgZ2l2ZW4gaWNvbi4gUmVzdWx0cyBtYXkgYmVcbiAgICogY2FjaGVkLCBzbyBmdXR1cmUgY2FsbHMgd2l0aCB0aGUgc2FtZSBVUkwgbWF5IG5vdCBjYXVzZSBhbm90aGVyIEhUVFAgcmVxdWVzdC5cbiAgICovXG4gIHByaXZhdGUgX2ZldGNoSWNvbihpY29uQ29uZmlnOiBTdmdJY29uQ29uZmlnKTogT2JzZXJ2YWJsZTxzdHJpbmc+IHtcbiAgICBjb25zdCB7dXJsOiBzYWZlVXJsLCBvcHRpb25zfSA9IGljb25Db25maWc7XG4gICAgY29uc3Qgd2l0aENyZWRlbnRpYWxzID0gb3B0aW9ucz8ud2l0aENyZWRlbnRpYWxzID8/IGZhbHNlO1xuXG4gICAgaWYgKCF0aGlzLl9odHRwQ2xpZW50KSB7XG4gICAgICB0aHJvdyBnZXRNYXRJY29uTm9IdHRwUHJvdmlkZXJFcnJvcigpO1xuICAgIH1cblxuICAgIGlmIChzYWZlVXJsID09IG51bGwpIHtcbiAgICAgIHRocm93IEVycm9yKGBDYW5ub3QgZmV0Y2ggaWNvbiBmcm9tIFVSTCBcIiR7c2FmZVVybH1cIi5gKTtcbiAgICB9XG5cbiAgICBjb25zdCB1cmwgPSB0aGlzLl9zYW5pdGl6ZXIuc2FuaXRpemUoU2VjdXJpdHlDb250ZXh0LlJFU09VUkNFX1VSTCwgc2FmZVVybCk7XG5cbiAgICBpZiAoIXVybCkge1xuICAgICAgdGhyb3cgZ2V0TWF0SWNvbkZhaWxlZFRvU2FuaXRpemVVcmxFcnJvcihzYWZlVXJsKTtcbiAgICB9XG5cbiAgICAvLyBTdG9yZSBpbi1wcm9ncmVzcyBmZXRjaGVzIHRvIGF2b2lkIHNlbmRpbmcgYSBkdXBsaWNhdGUgcmVxdWVzdCBmb3IgYSBVUkwgd2hlbiB0aGVyZSBpc1xuICAgIC8vIGFscmVhZHkgYSByZXF1ZXN0IGluIHByb2dyZXNzIGZvciB0aGF0IFVSTC4gSXQncyBuZWNlc3NhcnkgdG8gY2FsbCBzaGFyZSgpIG9uIHRoZVxuICAgIC8vIE9ic2VydmFibGUgcmV0dXJuZWQgYnkgaHR0cC5nZXQoKSBzbyB0aGF0IG11bHRpcGxlIHN1YnNjcmliZXJzIGRvbid0IGNhdXNlIG11bHRpcGxlIFhIUnMuXG4gICAgY29uc3QgaW5Qcm9ncmVzc0ZldGNoID0gdGhpcy5faW5Qcm9ncmVzc1VybEZldGNoZXMuZ2V0KHVybCk7XG5cbiAgICBpZiAoaW5Qcm9ncmVzc0ZldGNoKSB7XG4gICAgICByZXR1cm4gaW5Qcm9ncmVzc0ZldGNoO1xuICAgIH1cblxuICAgIC8vIFRPRE8oamVsYm91cm4pOiBmb3Igc29tZSByZWFzb24sIHRoZSBgZmluYWxpemVgIG9wZXJhdG9yIFwibG9zZXNcIiB0aGUgZ2VuZXJpYyB0eXBlIG9uIHRoZVxuICAgIC8vIE9ic2VydmFibGUuIEZpZ3VyZSBvdXQgd2h5IGFuZCBmaXggaXQuXG4gICAgY29uc3QgcmVxID0gdGhpcy5faHR0cENsaWVudC5nZXQodXJsLCB7cmVzcG9uc2VUeXBlOiAndGV4dCcsIHdpdGhDcmVkZW50aWFsc30pLnBpcGUoXG4gICAgICBmaW5hbGl6ZSgoKSA9PiB0aGlzLl9pblByb2dyZXNzVXJsRmV0Y2hlcy5kZWxldGUodXJsKSksXG4gICAgICBzaGFyZSgpLFxuICAgICk7XG5cbiAgICB0aGlzLl9pblByb2dyZXNzVXJsRmV0Y2hlcy5zZXQodXJsLCByZXEpO1xuICAgIHJldHVybiByZXE7XG4gIH1cblxuICAvKipcbiAgICogUmVnaXN0ZXJzIGFuIGljb24gY29uZmlnIGJ5IG5hbWUgaW4gdGhlIHNwZWNpZmllZCBuYW1lc3BhY2UuXG4gICAqIEBwYXJhbSBuYW1lc3BhY2UgTmFtZXNwYWNlIGluIHdoaWNoIHRvIHJlZ2lzdGVyIHRoZSBpY29uIGNvbmZpZy5cbiAgICogQHBhcmFtIGljb25OYW1lIE5hbWUgdW5kZXIgd2hpY2ggdG8gcmVnaXN0ZXIgdGhlIGNvbmZpZy5cbiAgICogQHBhcmFtIGNvbmZpZyBDb25maWcgdG8gYmUgcmVnaXN0ZXJlZC5cbiAgICovXG4gIHByaXZhdGUgX2FkZFN2Z0ljb25Db25maWcobmFtZXNwYWNlOiBzdHJpbmcsIGljb25OYW1lOiBzdHJpbmcsIGNvbmZpZzogU3ZnSWNvbkNvbmZpZyk6IHRoaXMge1xuICAgIHRoaXMuX3N2Z0ljb25Db25maWdzLnNldChpY29uS2V5KG5hbWVzcGFjZSwgaWNvbk5hbWUpLCBjb25maWcpO1xuICAgIHJldHVybiB0aGlzO1xuICB9XG5cbiAgLyoqXG4gICAqIFJlZ2lzdGVycyBhbiBpY29uIHNldCBjb25maWcgaW4gdGhlIHNwZWNpZmllZCBuYW1lc3BhY2UuXG4gICAqIEBwYXJhbSBuYW1lc3BhY2UgTmFtZXNwYWNlIGluIHdoaWNoIHRvIHJlZ2lzdGVyIHRoZSBpY29uIGNvbmZpZy5cbiAgICogQHBhcmFtIGNvbmZpZyBDb25maWcgdG8gYmUgcmVnaXN0ZXJlZC5cbiAgICovXG4gIHByaXZhdGUgX2FkZFN2Z0ljb25TZXRDb25maWcobmFtZXNwYWNlOiBzdHJpbmcsIGNvbmZpZzogU3ZnSWNvbkNvbmZpZyk6IHRoaXMge1xuICAgIGNvbnN0IGNvbmZpZ05hbWVzcGFjZSA9IHRoaXMuX2ljb25TZXRDb25maWdzLmdldChuYW1lc3BhY2UpO1xuXG4gICAgaWYgKGNvbmZpZ05hbWVzcGFjZSkge1xuICAgICAgY29uZmlnTmFtZXNwYWNlLnB1c2goY29uZmlnKTtcbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5faWNvblNldENvbmZpZ3Muc2V0KG5hbWVzcGFjZSwgW2NvbmZpZ10pO1xuICAgIH1cblxuICAgIHJldHVybiB0aGlzO1xuICB9XG59XG5cbi8qKiBAZG9jcy1wcml2YXRlICovXG5leHBvcnQgZnVuY3Rpb24gSUNPTl9SRUdJU1RSWV9QUk9WSURFUl9GQUNUT1JZKFxuICBwYXJlbnRSZWdpc3RyeTogTWF0SWNvblJlZ2lzdHJ5LFxuICBodHRwQ2xpZW50OiBIdHRwQ2xpZW50LFxuICBzYW5pdGl6ZXI6IERvbVNhbml0aXplcixcbiAgZXJyb3JIYW5kbGVyOiBFcnJvckhhbmRsZXIsXG4gIGRvY3VtZW50PzogYW55KSB7XG4gIHJldHVybiBwYXJlbnRSZWdpc3RyeSB8fCBuZXcgTWF0SWNvblJlZ2lzdHJ5KGh0dHBDbGllbnQsIHNhbml0aXplciwgZG9jdW1lbnQsIGVycm9ySGFuZGxlcik7XG59XG5cbi8qKiBAZG9jcy1wcml2YXRlICovXG5leHBvcnQgY29uc3QgSUNPTl9SRUdJU1RSWV9QUk9WSURFUiA9IHtcbiAgLy8gSWYgdGhlcmUgaXMgYWxyZWFkeSBhbiBNYXRJY29uUmVnaXN0cnkgYXZhaWxhYmxlLCB1c2UgdGhhdC4gT3RoZXJ3aXNlLCBwcm92aWRlIGEgbmV3IG9uZS5cbiAgcHJvdmlkZTogTWF0SWNvblJlZ2lzdHJ5LFxuICBkZXBzOiBbXG4gICAgW25ldyBPcHRpb25hbCgpLCBuZXcgU2tpcFNlbGYoKSwgTWF0SWNvblJlZ2lzdHJ5XSxcbiAgICBbbmV3IE9wdGlvbmFsKCksIEh0dHBDbGllbnRdLFxuICAgIERvbVNhbml0aXplcixcbiAgICBFcnJvckhhbmRsZXIsXG4gICAgW25ldyBPcHRpb25hbCgpLCBET0NVTUVOVCBhcyBJbmplY3Rpb25Ub2tlbjxhbnk+XSxcbiAgXSxcbiAgdXNlRmFjdG9yeTogSUNPTl9SRUdJU1RSWV9QUk9WSURFUl9GQUNUT1JZLFxufTtcblxuLyoqIENsb25lcyBhbiBTVkdFbGVtZW50IHdoaWxlIHByZXNlcnZpbmcgdHlwZSBpbmZvcm1hdGlvbi4gKi9cbmZ1bmN0aW9uIGNsb25lU3ZnKHN2ZzogU1ZHRWxlbWVudCk6IFNWR0VsZW1lbnQge1xuICByZXR1cm4gc3ZnLmNsb25lTm9kZSh0cnVlKSBhcyBTVkdFbGVtZW50O1xufVxuXG4vKiogUmV0dXJucyB0aGUgY2FjaGUga2V5IHRvIHVzZSBmb3IgYW4gaWNvbiBuYW1lc3BhY2UgYW5kIG5hbWUuICovXG5mdW5jdGlvbiBpY29uS2V5KG5hbWVzcGFjZTogc3RyaW5nLCBuYW1lOiBzdHJpbmcpIHtcbiAgcmV0dXJuIG5hbWVzcGFjZSArICc6JyArIG5hbWU7XG59XG4iXX0=