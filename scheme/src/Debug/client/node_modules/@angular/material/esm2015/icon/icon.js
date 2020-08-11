/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { coerceBooleanProperty } from '@angular/cdk/coercion';
import { DOCUMENT } from '@angular/common';
import { Attribute, ChangeDetectionStrategy, Component, ElementRef, ErrorHandler, inject, Inject, InjectionToken, Input, ViewEncapsulation, } from '@angular/core';
import { mixinColor } from '@angular/material/core';
import { Subscription } from 'rxjs';
import { take } from 'rxjs/operators';
import { MatIconRegistry } from './icon-registry';
// Boilerplate for applying mixins to MatIcon.
/** @docs-private */
class MatIconBase {
    constructor(_elementRef) {
        this._elementRef = _elementRef;
    }
}
const _MatIconMixinBase = mixinColor(MatIconBase);
/**
 * Injection token used to provide the current location to `MatIcon`.
 * Used to handle server-side rendering and to stub out during unit tests.
 * @docs-private
 */
export const MAT_ICON_LOCATION = new InjectionToken('mat-icon-location', {
    providedIn: 'root',
    factory: MAT_ICON_LOCATION_FACTORY
});
/** @docs-private */
export function MAT_ICON_LOCATION_FACTORY() {
    const _document = inject(DOCUMENT);
    const _location = _document ? _document.location : null;
    return {
        // Note that this needs to be a function, rather than a property, because Angular
        // will only resolve it once, but we want the current path on each call.
        getPathname: () => _location ? (_location.pathname + _location.search) : ''
    };
}
/** SVG attributes that accept a FuncIRI (e.g. `url(<something>)`). */
const funcIriAttributes = [
    'clip-path',
    'color-profile',
    'src',
    'cursor',
    'fill',
    'filter',
    'marker',
    'marker-start',
    'marker-mid',
    'marker-end',
    'mask',
    'stroke'
];
const ɵ0 = attr => `[${attr}]`;
/** Selector that can be used to find all elements that are using a `FuncIRI`. */
const funcIriAttributeSelector = funcIriAttributes.map(ɵ0).join(', ');
/** Regex that can be used to extract the id out of a FuncIRI. */
const funcIriPattern = /^url\(['"]?#(.*?)['"]?\)$/;
/**
 * Component to display an icon. It can be used in the following ways:
 *
 * - Specify the svgIcon input to load an SVG icon from a URL previously registered with the
 *   addSvgIcon, addSvgIconInNamespace, addSvgIconSet, or addSvgIconSetInNamespace methods of
 *   MatIconRegistry. If the svgIcon value contains a colon it is assumed to be in the format
 *   "[namespace]:[name]", if not the value will be the name of an icon in the default namespace.
 *   Examples:
 *     `<mat-icon svgIcon="left-arrow"></mat-icon>
 *     <mat-icon svgIcon="animals:cat"></mat-icon>`
 *
 * - Use a font ligature as an icon by putting the ligature text in the content of the `<mat-icon>`
 *   component. By default the Material icons font is used as described at
 *   http://google.github.io/material-design-icons/#icon-font-for-the-web. You can specify an
 *   alternate font by setting the fontSet input to either the CSS class to apply to use the
 *   desired font, or to an alias previously registered with MatIconRegistry.registerFontClassAlias.
 *   Examples:
 *     `<mat-icon>home</mat-icon>
 *     <mat-icon fontSet="myfont">sun</mat-icon>`
 *
 * - Specify a font glyph to be included via CSS rules by setting the fontSet input to specify the
 *   font, and the fontIcon input to specify the icon. Typically the fontIcon will specify a
 *   CSS class which causes the glyph to be displayed via a :before selector, as in
 *   https://fortawesome.github.io/Font-Awesome/examples/
 *   Example:
 *     `<mat-icon fontSet="fa" fontIcon="alarm"></mat-icon>`
 */
let MatIcon = /** @class */ (() => {
    class MatIcon extends _MatIconMixinBase {
        constructor(elementRef, _iconRegistry, ariaHidden, _location, _errorHandler) {
            super(elementRef);
            this._iconRegistry = _iconRegistry;
            this._location = _location;
            this._errorHandler = _errorHandler;
            this._inline = false;
            /** Subscription to the current in-progress SVG icon request. */
            this._currentIconFetch = Subscription.EMPTY;
            // If the user has not explicitly set aria-hidden, mark the icon as hidden, as this is
            // the right thing to do for the majority of icon use-cases.
            if (!ariaHidden) {
                elementRef.nativeElement.setAttribute('aria-hidden', 'true');
            }
        }
        /**
         * Whether the icon should be inlined, automatically sizing the icon to match the font size of
         * the element the icon is contained in.
         */
        get inline() {
            return this._inline;
        }
        set inline(inline) {
            this._inline = coerceBooleanProperty(inline);
        }
        /** Font set that the icon is a part of. */
        get fontSet() { return this._fontSet; }
        set fontSet(value) {
            this._fontSet = this._cleanupFontValue(value);
        }
        /** Name of an icon within a font set. */
        get fontIcon() { return this._fontIcon; }
        set fontIcon(value) {
            this._fontIcon = this._cleanupFontValue(value);
        }
        /**
         * Splits an svgIcon binding value into its icon set and icon name components.
         * Returns a 2-element array of [(icon set), (icon name)].
         * The separator for the two fields is ':'. If there is no separator, an empty
         * string is returned for the icon set and the entire value is returned for
         * the icon name. If the argument is falsy, returns an array of two empty strings.
         * Throws an error if the name contains two or more ':' separators.
         * Examples:
         *   `'social:cake' -> ['social', 'cake']
         *   'penguin' -> ['', 'penguin']
         *   null -> ['', '']
         *   'a:b:c' -> (throws Error)`
         */
        _splitIconName(iconName) {
            if (!iconName) {
                return ['', ''];
            }
            const parts = iconName.split(':');
            switch (parts.length) {
                case 1: return ['', parts[0]]; // Use default namespace.
                case 2: return parts;
                default: throw Error(`Invalid icon name: "${iconName}"`);
            }
        }
        ngOnChanges(changes) {
            // Only update the inline SVG icon if the inputs changed, to avoid unnecessary DOM operations.
            const svgIconChanges = changes['svgIcon'];
            if (svgIconChanges) {
                this._currentIconFetch.unsubscribe();
                if (this.svgIcon) {
                    const [namespace, iconName] = this._splitIconName(this.svgIcon);
                    this._currentIconFetch = this._iconRegistry.getNamedSvgIcon(iconName, namespace)
                        .pipe(take(1))
                        .subscribe(svg => this._setSvgElement(svg), (err) => {
                        const errorMessage = `Error retrieving icon ${namespace}:${iconName}! ${err.message}`;
                        this._errorHandler.handleError(new Error(errorMessage));
                    });
                }
                else if (svgIconChanges.previousValue) {
                    this._clearSvgElement();
                }
            }
            if (this._usingFontIcon()) {
                this._updateFontIconClasses();
            }
        }
        ngOnInit() {
            // Update font classes because ngOnChanges won't be called if none of the inputs are present,
            // e.g. <mat-icon>arrow</mat-icon> In this case we need to add a CSS class for the default font.
            if (this._usingFontIcon()) {
                this._updateFontIconClasses();
            }
        }
        ngAfterViewChecked() {
            const cachedElements = this._elementsWithExternalReferences;
            if (cachedElements && cachedElements.size) {
                const newPath = this._location.getPathname();
                // We need to check whether the URL has changed on each change detection since
                // the browser doesn't have an API that will let us react on link clicks and
                // we can't depend on the Angular router. The references need to be updated,
                // because while most browsers don't care whether the URL is correct after
                // the first render, Safari will break if the user navigates to a different
                // page and the SVG isn't re-rendered.
                if (newPath !== this._previousPath) {
                    this._previousPath = newPath;
                    this._prependPathToReferences(newPath);
                }
            }
        }
        ngOnDestroy() {
            this._currentIconFetch.unsubscribe();
            if (this._elementsWithExternalReferences) {
                this._elementsWithExternalReferences.clear();
            }
        }
        _usingFontIcon() {
            return !this.svgIcon;
        }
        _setSvgElement(svg) {
            this._clearSvgElement();
            // Workaround for IE11 and Edge ignoring `style` tags inside dynamically-created SVGs.
            // See: https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/10898469/
            // Do this before inserting the element into the DOM, in order to avoid a style recalculation.
            const styleTags = svg.querySelectorAll('style');
            for (let i = 0; i < styleTags.length; i++) {
                styleTags[i].textContent += ' ';
            }
            // Note: we do this fix here, rather than the icon registry, because the
            // references have to point to the URL at the time that the icon was created.
            const path = this._location.getPathname();
            this._previousPath = path;
            this._cacheChildrenWithExternalReferences(svg);
            this._prependPathToReferences(path);
            this._elementRef.nativeElement.appendChild(svg);
        }
        _clearSvgElement() {
            const layoutElement = this._elementRef.nativeElement;
            let childCount = layoutElement.childNodes.length;
            if (this._elementsWithExternalReferences) {
                this._elementsWithExternalReferences.clear();
            }
            // Remove existing non-element child nodes and SVGs, and add the new SVG element. Note that
            // we can't use innerHTML, because IE will throw if the element has a data binding.
            while (childCount--) {
                const child = layoutElement.childNodes[childCount];
                // 1 corresponds to Node.ELEMENT_NODE. We remove all non-element nodes in order to get rid
                // of any loose text nodes, as well as any SVG elements in order to remove any old icons.
                if (child.nodeType !== 1 || child.nodeName.toLowerCase() === 'svg') {
                    layoutElement.removeChild(child);
                }
            }
        }
        _updateFontIconClasses() {
            if (!this._usingFontIcon()) {
                return;
            }
            const elem = this._elementRef.nativeElement;
            const fontSetClass = this.fontSet ?
                this._iconRegistry.classNameForFontAlias(this.fontSet) :
                this._iconRegistry.getDefaultFontSetClass();
            if (fontSetClass != this._previousFontSetClass) {
                if (this._previousFontSetClass) {
                    elem.classList.remove(this._previousFontSetClass);
                }
                if (fontSetClass) {
                    elem.classList.add(fontSetClass);
                }
                this._previousFontSetClass = fontSetClass;
            }
            if (this.fontIcon != this._previousFontIconClass) {
                if (this._previousFontIconClass) {
                    elem.classList.remove(this._previousFontIconClass);
                }
                if (this.fontIcon) {
                    elem.classList.add(this.fontIcon);
                }
                this._previousFontIconClass = this.fontIcon;
            }
        }
        /**
         * Cleans up a value to be used as a fontIcon or fontSet.
         * Since the value ends up being assigned as a CSS class, we
         * have to trim the value and omit space-separated values.
         */
        _cleanupFontValue(value) {
            return typeof value === 'string' ? value.trim().split(' ')[0] : value;
        }
        /**
         * Prepends the current path to all elements that have an attribute pointing to a `FuncIRI`
         * reference. This is required because WebKit browsers require references to be prefixed with
         * the current path, if the page has a `base` tag.
         */
        _prependPathToReferences(path) {
            const elements = this._elementsWithExternalReferences;
            if (elements) {
                elements.forEach((attrs, element) => {
                    attrs.forEach(attr => {
                        element.setAttribute(attr.name, `url('${path}#${attr.value}')`);
                    });
                });
            }
        }
        /**
         * Caches the children of an SVG element that have `url()`
         * references that we need to prefix with the current path.
         */
        _cacheChildrenWithExternalReferences(element) {
            const elementsWithFuncIri = element.querySelectorAll(funcIriAttributeSelector);
            const elements = this._elementsWithExternalReferences =
                this._elementsWithExternalReferences || new Map();
            for (let i = 0; i < elementsWithFuncIri.length; i++) {
                funcIriAttributes.forEach(attr => {
                    const elementWithReference = elementsWithFuncIri[i];
                    const value = elementWithReference.getAttribute(attr);
                    const match = value ? value.match(funcIriPattern) : null;
                    if (match) {
                        let attributes = elements.get(elementWithReference);
                        if (!attributes) {
                            attributes = [];
                            elements.set(elementWithReference, attributes);
                        }
                        attributes.push({ name: attr, value: match[1] });
                    }
                });
            }
        }
    }
    MatIcon.decorators = [
        { type: Component, args: [{
                    template: '<ng-content></ng-content>',
                    selector: 'mat-icon',
                    exportAs: 'matIcon',
                    inputs: ['color'],
                    host: {
                        'role': 'img',
                        'class': 'mat-icon notranslate',
                        '[class.mat-icon-inline]': 'inline',
                        '[class.mat-icon-no-color]': 'color !== "primary" && color !== "accent" && color !== "warn"',
                    },
                    encapsulation: ViewEncapsulation.None,
                    changeDetection: ChangeDetectionStrategy.OnPush,
                    styles: [".mat-icon{background-repeat:no-repeat;display:inline-block;fill:currentColor;height:24px;width:24px}.mat-icon.mat-icon-inline{font-size:inherit;height:inherit;line-height:inherit;width:inherit}[dir=rtl] .mat-icon-rtl-mirror{transform:scale(-1, 1)}.mat-form-field:not(.mat-form-field-appearance-legacy) .mat-form-field-prefix .mat-icon,.mat-form-field:not(.mat-form-field-appearance-legacy) .mat-form-field-suffix .mat-icon{display:block}.mat-form-field:not(.mat-form-field-appearance-legacy) .mat-form-field-prefix .mat-icon-button .mat-icon,.mat-form-field:not(.mat-form-field-appearance-legacy) .mat-form-field-suffix .mat-icon-button .mat-icon{margin:auto}\n"]
                },] }
    ];
    MatIcon.ctorParameters = () => [
        { type: ElementRef },
        { type: MatIconRegistry },
        { type: String, decorators: [{ type: Attribute, args: ['aria-hidden',] }] },
        { type: undefined, decorators: [{ type: Inject, args: [MAT_ICON_LOCATION,] }] },
        { type: ErrorHandler }
    ];
    MatIcon.propDecorators = {
        inline: [{ type: Input }],
        svgIcon: [{ type: Input }],
        fontSet: [{ type: Input }],
        fontIcon: [{ type: Input }]
    };
    return MatIcon;
})();
export { MatIcon };
export { ɵ0 };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaWNvbi5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9pY29uL2ljb24udHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFlLHFCQUFxQixFQUFDLE1BQU0sdUJBQXVCLENBQUM7QUFDMUUsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLGlCQUFpQixDQUFDO0FBQ3pDLE9BQU8sRUFFTCxTQUFTLEVBQ1QsdUJBQXVCLEVBQ3ZCLFNBQVMsRUFDVCxVQUFVLEVBQ1YsWUFBWSxFQUNaLE1BQU0sRUFDTixNQUFNLEVBQ04sY0FBYyxFQUNkLEtBQUssRUFLTCxpQkFBaUIsR0FDbEIsTUFBTSxlQUFlLENBQUM7QUFDdkIsT0FBTyxFQUF5QixVQUFVLEVBQUMsTUFBTSx3QkFBd0IsQ0FBQztBQUMxRSxPQUFPLEVBQUMsWUFBWSxFQUFDLE1BQU0sTUFBTSxDQUFDO0FBQ2xDLE9BQU8sRUFBQyxJQUFJLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQUVwQyxPQUFPLEVBQUMsZUFBZSxFQUFDLE1BQU0saUJBQWlCLENBQUM7QUFHaEQsOENBQThDO0FBQzlDLG9CQUFvQjtBQUNwQixNQUFNLFdBQVc7SUFDZixZQUFtQixXQUF1QjtRQUF2QixnQkFBVyxHQUFYLFdBQVcsQ0FBWTtJQUFHLENBQUM7Q0FDL0M7QUFDRCxNQUFNLGlCQUFpQixHQUFzQyxVQUFVLENBQUMsV0FBVyxDQUFDLENBQUM7QUFFckY7Ozs7R0FJRztBQUNILE1BQU0sQ0FBQyxNQUFNLGlCQUFpQixHQUFHLElBQUksY0FBYyxDQUFrQixtQkFBbUIsRUFBRTtJQUN4RixVQUFVLEVBQUUsTUFBTTtJQUNsQixPQUFPLEVBQUUseUJBQXlCO0NBQ25DLENBQUMsQ0FBQztBQVVILG9CQUFvQjtBQUNwQixNQUFNLFVBQVUseUJBQXlCO0lBQ3ZDLE1BQU0sU0FBUyxHQUFHLE1BQU0sQ0FBQyxRQUFRLENBQUMsQ0FB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R0wsS0FBSzsyQkFRTCxLQUFLOztJQTRQUixjQUFDO0tBQUE7U0F4UlksT0FBTyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0Jvb2xlYW5JbnB1dCwgY29lcmNlQm9vbGVhblByb3BlcnR5fSBmcm9tICdAYW5ndWxhci9jZGsvY29lcmNpb24nO1xuaW1wb3J0IHtET0NVTUVOVH0gZnJvbSAnQGFuZ3VsYXIvY29tbW9uJztcbmltcG9ydCB7XG4gIEFmdGVyVmlld0NoZWNrZWQsXG4gIEF0dHJpYnV0ZSxcbiAgQ2hhbmdlRGV0ZWN0aW9uU3RyYXRlZ3ksXG4gIENvbXBvbmVudCxcbiAgRWxlbWVudFJlZixcbiAgRXJyb3JIYW5kbGVyLFxuICBpbmplY3QsXG4gIEluamVjdCxcbiAgSW5qZWN0aW9uVG9rZW4sXG4gIElucHV0LFxuICBPbkNoYW5nZXMsXG4gIE9uRGVzdHJveSxcbiAgT25Jbml0LFxuICBTaW1wbGVDaGFuZ2VzLFxuICBWaWV3RW5jYXBzdWxhdGlvbixcbn0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQge0NhbkNvbG9yLCBDYW5Db2xvckN0b3IsIG1peGluQ29sb3J9IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL2NvcmUnO1xuaW1wb3J0IHtTdWJzY3JpcHRpb259IGZyb20gJ3J4anMnO1xuaW1wb3J0IHt0YWtlfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5cbmltcG9ydCB7TWF0SWNvblJlZ2lzdHJ5fSBmcm9tICcuL2ljb24tcmVnaXN0cnknO1xuXG5cbi8vIEJvaWxlcnBsYXRlIGZvciBhcHBseWluZyBtaXhpbnMgdG8gTWF0SWNvbi5cbi8qKiBAZG9jcy1wcml2YXRlICovXG5jbGFzcyBNYXRJY29uQmFzZSB7XG4gIGNvbnN0cnVjdG9yKHB1YmxpYyBfZWxlbWVudFJlZjogRWxlbWVudFJlZikge31cbn1cbmNvbnN0IF9NYXRJY29uTWl4aW5CYXNlOiBDYW5Db2xvckN0b3IgJiB0eXBlb2YgTWF0SWNvbkJhc2UgPSBtaXhpbkNvbG9yKE1hdEljb25CYXNlKTtcblxuLyoqXG4gKiBJbmplY3Rpb24gdG9rZW4gdXNlZCB0byBwcm92aWRlIHRoZSBjdXJyZW50IGxvY2F0aW9uIHRvIGBNYXRJY29uYC5cbiAqIFVzZWQgdG8gaGFuZGxlIHNlcnZlci1zaWRlIHJlbmRlcmluZyBhbmQgdG8gc3R1YiBvdXQgZHVyaW5nIHVuaXQgdGVzdHMuXG4gKiBAZG9jcy1wcml2YXRlXG4gKi9cbmV4cG9ydCBjb25zdCBNQVRfSUNPTl9MT0NBVElPTiA9IG5ldyBJbmplY3Rpb25Ub2tlbjxNYXRJY29uTG9jYXRpb24+KCdtYXQtaWNvbi1sb2NhdGlvbicsIHtcbiAgcHJvdmlkZWRJbjogJ3Jvb3QnLFxuICBmYWN0b3J5OiBNQVRfSUNPTl9MT0NBVElPTl9GQUNUT1JZXG59KTtcblxuLyoqXG4gKiBTdHViYmVkIG91dCBsb2NhdGlvbiBmb3IgYE1hdEljb25gLlxuICogQGRvY3MtcHJpdmF0ZVxuICovXG5leHBvcnQgaW50ZXJmYWNlIE1hdEljb25Mb2NhdGlvbiB7XG4gIGdldFBhdGhuYW1lOiAoKSA9PiBzdHJpbmc7XG59XG5cbi8qKiBAZG9jcy1wcml2YXRlICovXG5leHBvcnQgZnVuY3Rpb24gTUFUX0lDT05fTE9DQVRJT05fRkFDVE9SWSgpOiBNYXRJY29uTG9jYXRpb24ge1xuICBjb25zdCBfZG9jdW1lbnQgPSBpbmplY3QoRE9DVU1FTlQpO1xuICBjb25zdCBfbG9jYXRpb24gPSBfZG9jdW1lbnQgPyBfZG9jdW1lbnQubG9jYXRpb24gOiBudWxsO1xuXG4gIHJldHVybiB7XG4gICAgLy8gTm90ZSB0aGF0IHRoaXMgbmVlZHMgdG8gYmUgYSBmdW5jdGlvbiwgcmF0aGVyIHRoYW4gYSBwcm9wZXJ0eSwgYmVjYXVzZSBBbmd1bGFyXG4gICAgLy8gd2lsbCBvbmx5IHJlc29sdmUgaXQgb25jZSwgYnV0IHdlIHdhbnQgdGhlIGN1cnJlbnQgcGF0aCBvbiBlYWNoIGNhbGwuXG4gICAgZ2V0UGF0aG5hbWU6ICgpID0+IF9sb2NhdGlvbiA/IChfbG9jYXRpb24ucGF0aG5hbWUgKyBfbG9jYXRpb24uc2VhcmNoKSA6ICcnXG4gIH07XG59XG5cblxuLyoqIFNWRyBhdHRyaWJ1dGVzIHRoYXQgYWNjZXB0IGEgRnVuY0lSSSAoZS5nLiBgdXJsKDxzb21ldGhpbmc+KWApLiAqL1xuY29uc3QgZnVuY0lyaUF0dHJpYnV0ZXMgPSBbXG4gICdjbGlwLXBhdGgnLFxuICAnY29sb3ItcHJvZmlsZScsXG4gICdzcmMnLFxuICAnY3Vyc29yJyxcbiAgJ2ZpbGwnLFxuICAnZmlsdGVyJyxcbiAgJ21hcmtlcicsXG4gICdtYXJrZXItc3RhcnQnLFxuICAnbWFya2VyLW1pZCcsXG4gICdtYXJrZXItZW5kJyxcbiAgJ21hc2snLFxuICAnc3Ryb2tlJ1xuXTtcblxuLyoqIFNlbGVjdG9yIHRoYXQgY2FuIGJlIHVzZWQgdG8gZmluZCBhbGwgZWxlbWVudHMgdGhhdCBhcmUgdXNpbmcgYSBgRnVuY0lSSWAuICovXG5jb25zdCBmdW5jSXJpQXR0cmlidXRlU2VsZWN0b3IgPSBmdW5jSXJpQXR0cmlidXRlcy5tYXAoYXR0ciA9PiBgWyR7YXR0cn1dYCkuam9pbignLCAnKTtcblxuLyoqIFJlZ2V4IHRoYXQgY2FuIGJlIHVzZWQgdG8gZXh0cmFjdCB0aGUgaWQgb3V0IG9mIGEgRnVuY0lSSS4gKi9cbmNvbnN0IGZ1bmNJcmlQYXR0ZXJuID0gL151cmxcXChbJ1wiXT8jKC4qPylbJ1wiXT9cXCkkLztcblxuLyoqXG4gKiBDb21wb25lbnQgdG8gZGlzcGxheSBhbiBpY29uLiBJdCBjYW4gYmUgdXNlZCBpbiB0aGUgZm9sbG93aW5nIHdheXM6XG4gKlxuICogLSBTcGVjaWZ5IHRoZSBzdmdJY29uIGlucHV0IHRvIGxvYWQgYW4gU1ZHIGljb24gZnJvbSBhIFVSTCBwcmV2aW91c2x5IHJlZ2lzdGVyZWQgd2l0aCB0aGVcbiAqICAgYWRkU3ZnSWNvbiwgYWRkU3ZnSWNvbkluTmFtZXNwYWNlLCBhZGRTdmdJY29uU2V0LCBvciBhZGRTdmdJY29uU2V0SW5OYW1lc3BhY2UgbWV0aG9kcyBvZlxuICogICBNYXRJY29uUmVnaXN0cnkuIElmIHRoZSBzdmdJY29uIHZhbHVlIGNvbnRhaW5zIGEgY29sb24gaXQgaXMgYXNzdW1lZCB0byBiZSBpbiB0aGUgZm9ybWF0XG4gKiAgIFwiW25hbWVzcGFjZV06W25hbWVdXCIsIGlmIG5vdCB0aGUgdmFsdWUgd2lsbCBiZSB0aGUgbmFtZSBvZiBhbiBpY29uIGluIHRoZSBkZWZhdWx0IG5hbWVzcGFjZS5cbiAqICAgRXhhbXBsZXM6XG4gKiAgICAgYDxtYXQtaWNvbiBzdmdJY29uPVwibGVmdC1hcnJvd1wiPjwvbWF0LWljb24+XG4gKiAgICAgPG1hdC1pY29uIHN2Z0ljb249XCJhbmltYWxzOmNhdFwiPjwvbWF0LWljb24+YFxuICpcbiAqIC0gVXNlIGEgZm9udCBsaWdhdHVyZSBhcyBhbiBpY29uIGJ5IHB1dHRpbmcgdGhlIGxpZ2F0dXJlIHRleHQgaW4gdGhlIGNvbnRlbnQgb2YgdGhlIGA8bWF0LWljb24+YFxuICogICBjb21wb25lbnQuIEJ5IGRlZmF1bHQgdGhlIE1hdGVyaWFsIGljb25zIGZvbnQgaXMgdXNlZCBhcyBkZXNjcmliZWQgYXRcbiAqICAgaHR0cDovL2dvb2dsZS5naXRodWIuaW8vbWF0ZXJpYWwtZGVzaWduLWljb25zLyNpY29uLWZvbnQtZm9yLXRoZS13ZWIuIFlvdSBjYW4gc3BlY2lmeSBhblxuICogICBhbHRlcm5hdGUgZm9udCBieSBzZXR0aW5nIHRoZSBmb250U2V0IGlucHV0IHRvIGVpdGhlciB0aGUgQ1NTIGNsYXNzIHRvIGFwcGx5IHRvIHVzZSB0aGVcbiAqICAgZGVzaXJlZCBmb250LCBvciB0byBhbiBhbGlhcyBwcmV2aW91c2x5IHJlZ2lzdGVyZWQgd2l0aCBNYXRJY29uUmVnaXN0cnkucmVnaXN0ZXJGb250Q2xhc3NBbGlhcy5cbiAqICAgRXhhbXBsZXM6XG4gKiAgICAgYDxtYXQtaWNvbj5ob21lPC9tYXQtaWNvbj5cbiAqICAgICA8bWF0LWljb24gZm9udFNldD1cIm15Zm9udFwiPnN1bjwvbWF0LWljb24+YFxuICpcbiAqIC0gU3BlY2lmeSBhIGZvbnQgZ2x5cGggdG8gYmUgaW5jbHVkZWQgdmlhIENTUyBydWxlcyBieSBzZXR0aW5nIHRoZSBmb250U2V0IGlucHV0IHRvIHNwZWNpZnkgdGhlXG4gKiAgIGZvbnQsIGFuZCB0aGUgZm9udEljb24gaW5wdXQgdG8gc3BlY2lmeSB0aGUgaWNvbi4gVHlwaWNhbGx5IHRoZSBmb250SWNvbiB3aWxsIHNwZWNpZnkgYVxuICogICBDU1MgY2xhc3Mgd2hpY2ggY2F1c2VzIHRoZSBnbHlwaCB0byBiZSBkaXNwbGF5ZWQgdmlhIGEgOmJlZm9yZSBzZWxlY3RvciwgYXMgaW5cbiAqICAgaHR0cHM6Ly9mb3J0YXdlc29tZS5naXRodWIuaW8vRm9udC1Bd2Vzb21lL2V4YW1wbGVzL1xuICogICBFeGFtcGxlOlxuICogICAgIGA8bWF0LWljb24gZm9udFNldD1cImZhXCIgZm9udEljb249XCJhbGFybVwiPjwvbWF0LWljb24+YFxuICovXG5AQ29tcG9uZW50KHtcbiAgdGVtcGxhdGU6ICc8bmctY29udGVudD48L25nLWNvbnRlbnQ+JyxcbiAgc2VsZWN0b3I6ICdtYXQtaWNvbicsXG4gIGV4cG9ydEFzOiAnbWF0SWNvbicsXG4gIHN0eWxlVXJsczogWydpY29uLmNzcyddLFxuICBpbnB1dHM6IFsnY29sb3InXSxcbiAgaG9zdDoge1xuICAgICdyb2xlJzogJ2ltZycsXG4gICAgJ2NsYXNzJzogJ21hdC1pY29uIG5vdHJhbnNsYXRlJyxcbiAgICAnW2NsYXNzLm1hdC1pY29uLWlubGluZV0nOiAnaW5saW5lJyxcbiAgICAnW2NsYXNzLm1hdC1pY29uLW5vLWNvbG9yXSc6ICdjb2xvciAhPT0gXCJwcmltYXJ5XCIgJiYgY29sb3IgIT09IFwiYWNjZW50XCIgJiYgY29sb3IgIT09IFwid2FyblwiJyxcbiAgfSxcbiAgZW5jYXBzdWxhdGlvbjogVmlld0VuY2Fwc3VsYXRpb24uTm9uZSxcbiAgY2hhbmdlRGV0ZWN0aW9uOiBDaGFuZ2VEZXRlY3Rpb25TdHJhdGVneS5PblB1c2gsXG59KVxuZXhwb3J0IGNsYXNzIE1hdEljb24gZXh0ZW5kcyBfTWF0SWNvbk1peGluQmFzZSBpbXBsZW1lbnRzIE9uQ2hhbmdlcywgT25Jbml0LCBBZnRlclZpZXdDaGVja2VkLFxuICBDYW5Db2xvciwgT25EZXN0cm95IHtcblxuICAvKipcbiAgICogV2hldGhlciB0aGUgaWNvbiBzaG91bGQgYmUgaW5saW5lZCwgYXV0b21hdGljYWxseSBzaXppbmcgdGhlIGljb24gdG8gbWF0Y2ggdGhlIGZvbnQgc2l6ZSBvZlxuICAgKiB0aGUgZWxlbWVudCB0aGUgaWNvbiBpcyBjb250YWluZWQgaW4uXG4gICAqL1xuICBASW5wdXQoKVxuICBnZXQgaW5saW5lKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLl9pbmxpbmU7XG4gIH1cbiAgc2V0IGlubGluZShpbmxpbmU6IGJvb2xlYW4pIHtcbiAgICB0aGlzLl9pbmxpbmUgPSBjb2VyY2VCb29sZWFuUHJvcGVydHkoaW5saW5lKTtcbiAgfVxuICBwcml2YXRlIF9pbmxpbmU6IGJvb2xlYW4gPSBmYWxzZTtcblxuICAvKiogTmFtZSBvZiB0aGUgaWNvbiBpbiB0aGUgU1ZHIGljb24gc2V0LiAqL1xuICBASW5wdXQoKSBzdmdJY29uOiBzdHJpbmc7XG5cbiAgLyoqIEZvbnQgc2V0IHRoYXQgdGhlIGljb24gaXMgYSBwYXJ0IG9mLiAqL1xuICBASW5wdXQoKVxuICBnZXQgZm9udFNldCgpOiBzdHJpbmcgeyByZXR1cm4gdGhpcy5fZm9udFNldDsgfVxuICBzZXQgZm9udFNldCh2YWx1ZTogc3RyaW5nKSB7XG4gICAgdGhpcy5fZm9udFNldCA9IHRoaXMuX2NsZWFudXBGb250VmFsdWUodmFsdWUpO1xuICB9XG4gIHByaXZhdGUgX2ZvbnRTZXQ6IHN0cmluZztcblxuICAvKiogTmFtZSBvZiBhbiBpY29uIHdpdGhpbiBhIGZvbnQgc2V0LiAqL1xuICBASW5wdXQoKVxuICBnZXQgZm9udEljb24oKTogc3RyaW5nIHsgcmV0dXJuIHRoaXMuX2ZvbnRJY29uOyB9XG4gIHNldCBmb250SWNvbih2YWx1ZTogc3RyaW5nKSB7XG4gICAgdGhpcy5fZm9udEljb24gPSB0aGlzLl9jbGVhbnVwRm9udFZhbHVlKHZhbHVlKTtcbiAgfVxuICBwcml2YXRlIF9mb250SWNvbjogc3RyaW5nO1xuXG4gIHByaXZhdGUgX3ByZXZpb3VzRm9udFNldENsYXNzOiBzdHJpbmc7XG4gIHByaXZhdGUgX3ByZXZpb3VzRm9udEljb25DbGFzczogc3RyaW5nO1xuXG4gIC8qKiBLZWVwcyB0cmFjayBvZiB0aGUgY3VycmVudCBwYWdlIHBhdGguICovXG4gIHByaXZhdGUgX3ByZXZpb3VzUGF0aD86IHN0cmluZztcblxuICAvKiogS2VlcHMgdHJhY2sgb2YgdGhlIGVsZW1lbnRzIGFuZCBhdHRyaWJ1dGVzIHRoYXQgd2UndmUgcHJlZml4ZWQgd2l0aCB0aGUgY3VycmVudCBwYXRoLiAqL1xuICBwcml2YXRlIF9lbGVtZW50c1dpdGhFeHRlcm5hbFJlZmVyZW5jZXM/OiBNYXA8RWxlbWVudCwge25hbWU6IHN0cmluZywgdmFsdWU6IHN0cmluZ31bXT47XG5cbiAgLyoqIFN1YnNjcmlwdGlvbiB0byB0aGUgY3VycmVudCBpbi1wcm9ncmVzcyBTVkcgaWNvbiByZXF1ZXN0LiAqL1xuICBwcml2YXRlIF9jdXJyZW50SWNvbkZldGNoID0gU3Vic2NyaXB0aW9uLkVNUFRZO1xuXG4gIGNvbnN0cnVjdG9yKFxuICAgICAgZWxlbWVudFJlZjogRWxlbWVudFJlZjxIVE1MRWxlbWVudD4sIHByaXZhdGUgX2ljb25SZWdpc3RyeTogTWF0SWNvblJlZ2lzdHJ5LFxuICAgICAgQEF0dHJpYnV0ZSgnYXJpYS1oaWRkZW4nKSBhcmlhSGlkZGVuOiBzdHJpbmcsXG4gICAgICBASW5qZWN0KE1BVF9JQ09OX0xPQ0FUSU9OKSBwcml2YXRlIF9sb2NhdGlvbjogTWF0SWNvbkxvY2F0aW9uLFxuICAgICAgcHJpdmF0ZSByZWFkb25seSBfZXJyb3JIYW5kbGVyOiBFcnJvckhhbmRsZXIpIHtcbiAgICBzdXBlcihlbGVtZW50UmVmKTtcblxuICAgIC8vIElmIHRoZSB1c2VyIGhhcyBub3QgZXhwbGljaXRseSBzZXQgYXJpYS1oaWRkZW4sIG1hcmsgdGhlIGljb24gYXMgaGlkZGVuLCBhcyB0aGlzIGlzXG4gICAgLy8gdGhlIHJpZ2h0IHRoaW5nIHRvIGRvIGZvciB0aGUgbWFqb3JpdHkgb2YgaWNvbiB1c2UtY2FzZXMuXG4gICAgaWYgKCFhcmlhSGlkZGVuKSB7XG4gICAgICBlbGVtZW50UmVmLm5hdGl2ZUVsZW1lbnQuc2V0QXR0cmlidXRlKCdhcmlhLWhpZGRlbicsICd0cnVlJyk7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIFNwbGl0cyBhbiBzdmdJY29uIGJpbmRpbmcgdmFsdWUgaW50byBpdHMgaWNvbiBzZXQgYW5kIGljb24gbmFtZSBjb21wb25lbnRzLlxuICAgKiBSZXR1cm5zIGEgMi1lbGVtZW50IGFycmF5IG9mIFsoaWNvbiBzZXQpLCAoaWNvbiBuYW1lKV0uXG4gICAqIFRoZSBzZXBhcmF0b3IgZm9yIHRoZSB0d28gZmllbGRzIGlzICc6Jy4gSWYgdGhlcmUgaXMgbm8gc2VwYXJhdG9yLCBhbiBlbXB0eVxuICAgKiBzdHJpbmcgaXMgcmV0dXJuZWQgZm9yIHRoZSBpY29uIHNldCBhbmQgdGhlIGVudGlyZSB2YWx1ZSBpcyByZXR1cm5lZCBmb3JcbiAgICogdGhlIGljb24gbmFtZS4gSWYgdGhlIGFyZ3VtZW50IGlzIGZhbHN5LCByZXR1cm5zIGFuIGFycmF5IG9mIHR3byBlbXB0eSBzdHJpbmdzLlxuICAgKiBUaHJvd3MgYW4gZXJyb3IgaWYgdGhlIG5hbWUgY29udGFpbnMgdHdvIG9yIG1vcmUgJzonIHNlcGFyYXRvcnMuXG4gICAqIEV4YW1wbGVzOlxuICAgKiAgIGAnc29jaWFsOmNha2UnIC0+IFsnc29jaWFsJywgJ2Nha2UnXVxuICAgKiAgICdwZW5ndWluJyAtPiBbJycsICdwZW5ndWluJ11cbiAgICogICBudWxsIC0+IFsnJywgJyddXG4gICAqICAgJ2E6YjpjJyAtPiAodGhyb3dzIEVycm9yKWBcbiAgICovXG4gIHByaXZhdGUgX3NwbGl0SWNvbk5hbWUoaWNvbk5hbWU6IHN0cmluZyk6IFtzdHJpbmcsIHN0cmluZ10ge1xuICAgIGlmICghaWNvbk5hbWUpIHtcbiAgICAgIHJldHVybiBbJycsICcnXTtcbiAgICB9XG4gICAgY29uc3QgcGFydHMgPSBpY29uTmFtZS5zcGxpdCgnOicpO1xuICAgIHN3aXRjaCAocGFydHMubGVuZ3RoKSB7XG4gICAgICBjYXNlIDE6IHJldHVybiBbJycsIHBhcnRzWzBdXTsgLy8gVXNlIGRlZmF1bHQgbmFtZXNwYWNlLlxuICAgICAgY2FzZSAyOiByZXR1cm4gPFtzdHJpbmcsIHN0cmluZ10+cGFydHM7XG4gICAgICBkZWZhdWx0OiB0aHJvdyBFcnJvcihgSW52YWxpZCBpY29uIG5hbWU6IFwiJHtpY29uTmFtZX1cImApO1xuICAgIH1cbiAgfVxuXG4gIG5nT25DaGFuZ2VzKGNoYW5nZXM6IFNpbXBsZUNoYW5nZXMpIHtcbiAgICAvLyBPbmx5IHVwZGF0ZSB0aGUgaW5saW5lIFNWRyBpY29uIGlmIHRoZSBpbnB1dHMgY2hhbmdlZCwgdG8gYXZvaWQgdW5uZWNlc3NhcnkgRE9NIG9wZXJhdGlvbnMuXG4gICAgY29uc3Qgc3ZnSWNvbkNoYW5nZXMgPSBjaGFuZ2VzWydzdmdJY29uJ107XG5cbiAgICBpZiAoc3ZnSWNvbkNoYW5nZXMpIHtcbiAgICAgIHRoaXMuX2N1cnJlbnRJY29uRmV0Y2gudW5zdWJzY3JpYmUoKTtcblxuICAgICAgaWYgKHRoaXMuc3ZnSWNvbikge1xuICAgICAgICBjb25zdCBbbmFtZXNwYWNlLCBpY29uTmFtZV0gPSB0aGlzLl9zcGxpdEljb25OYW1lKHRoaXMuc3ZnSWNvbik7XG5cbiAgICAgICAgdGhpcy5fY3VycmVudEljb25GZXRjaCA9IHRoaXMuX2ljb25SZWdpc3RyeS5nZXROYW1lZFN2Z0ljb24oaWNvbk5hbWUsIG5hbWVzcGFjZSlcbiAgICAgICAgICAgIC5waXBlKHRha2UoMSkpXG4gICAgICAgICAgICAuc3Vic2NyaWJlKHN2ZyA9PiB0aGlzLl9zZXRTdmdFbGVtZW50KHN2ZyksIChlcnI6IEVycm9yKSA9PiB7XG4gICAgICAgICAgICAgIGNvbnN0IGVycm9yTWVzc2FnZSA9IGBFcnJvciByZXRyaWV2aW5nIGljb24gJHtuYW1lc3BhY2V9OiR7aWNvbk5hbWV9ISAke2Vyci5tZXNzYWdlfWA7XG4gICAgICAgICAgICAgIHRoaXMuX2Vycm9ySGFuZGxlci5oYW5kbGVFcnJvcihuZXcgRXJyb3IoZXJyb3JNZXNzYWdlKSk7XG4gICAgICAgICAgICB9KTtcbiAgICAgIH0gZWxzZSBpZiAoc3ZnSWNvbkNoYW5nZXMucHJldmlvdXNWYWx1ZSkge1xuICAgICAgICB0aGlzLl9jbGVhclN2Z0VsZW1lbnQoKTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICBpZiAodGhpcy5fdXNpbmdGb250SWNvbigpKSB7XG4gICAgICB0aGlzLl91cGRhdGVGb250SWNvbkNsYXNzZXMoKTtcbiAgICB9XG4gIH1cblxuICBuZ09uSW5pdCgpIHtcbiAgICAvLyBVcGRhdGUgZm9udCBjbGFzc2VzIGJlY2F1c2UgbmdPbkNoYW5nZXMgd29uJ3QgYmUgY2FsbGVkIGlmIG5vbmUgb2YgdGhlIGlucHV0cyBhcmUgcHJlc2VudCxcbiAgICAvLyBlLmcuIDxtYXQtaWNvbj5hcnJvdzwvbWF0LWljb24+IEluIHRoaXMgY2FzZSB3ZSBuZWVkIHRvIGFkZCBhIENTUyBjbGFzcyBmb3IgdGhlIGRlZmF1bHQgZm9udC5cbiAgICBpZiAodGhpcy5fdXNpbmdGb250SWNvbigpKSB7XG4gICAgICB0aGlzLl91cGRhdGVGb250SWNvbkNsYXNzZXMoKTtcbiAgICB9XG4gIH1cblxuICBuZ0FmdGVyVmlld0NoZWNrZWQoKSB7XG4gICAgY29uc3QgY2FjaGVkRWxlbWVudHMgPSB0aGlzLl9lbGVtZW50c1dpdGhFeHRlcm5hbFJlZmVyZW5jZXM7XG5cbiAgICBpZiAoY2FjaGVkRWxlbWVudHMgJiYgY2FjaGVkRWxlbWVudHMuc2l6ZSkge1xuICAgICAgY29uc3QgbmV3UGF0aCA9IHRoaXMuX2xvY2F0aW9uLmdldFBhdGhuYW1lKCk7XG5cbiAgICAgIC8vIFdlIG5lZWQgdG8gY2hlY2sgd2hldGhlciB0aGUgVVJMIGhhcyBjaGFuZ2VkIG9uIGVhY2ggY2hhbmdlIGRldGVjdGlvbiBzaW5jZVxuICAgICAgLy8gdGhlIGJyb3dzZXIgZG9lc24ndCBoYXZlIGFuIEFQSSB0aGF0IHdpbGwgbGV0IHVzIHJlYWN0IG9uIGxpbmsgY2xpY2tzIGFuZFxuICAgICAgLy8gd2UgY2FuJ3QgZGVwZW5kIG9uIHRoZSBBbmd1bGFyIHJvdXRlci4gVGhlIHJlZmVyZW5jZXMgbmVlZCB0byBiZSB1cGRhdGVkLFxuICAgICAgLy8gYmVjYXVzZSB3aGlsZSBtb3N0IGJyb3dzZXJzIGRvbid0IGNhcmUgd2hldGhlciB0aGUgVVJMIGlzIGNvcnJlY3QgYWZ0ZXJcbiAgICAgIC8vIHRoZSBmaXJzdCByZW5kZXIsIFNhZmFyaSB3aWxsIGJyZWFrIGlmIHRoZSB1c2VyIG5hdmlnYXRlcyB0byBhIGRpZmZlcmVudFxuICAgICAgLy8gcGFnZSBhbmQgdGhlIFNWRyBpc24ndCByZS1yZW5kZXJlZC5cbiAgICAgIGlmIChuZXdQYXRoICE9PSB0aGlzLl9wcmV2aW91c1BhdGgpIHtcbiAgICAgICAgdGhpcy5fcHJldmlvdXNQYXRoID0gbmV3UGF0aDtcbiAgICAgICAgdGhpcy5fcHJlcGVuZFBhdGhUb1JlZmVyZW5jZXMobmV3UGF0aCk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgbmdPbkRlc3Ryb3koKSB7XG4gICAgdGhpcy5fY3VycmVudEljb25GZXRjaC51bnN1YnNjcmliZSgpO1xuXG4gICAgaWYgKHRoaXMuX2VsZW1lbnRzV2l0aEV4dGVybmFsUmVmZXJlbmNlcykge1xuICAgICAgdGhpcy5fZWxlbWVudHNXaXRoRXh0ZXJuYWxSZWZlcmVuY2VzLmNsZWFyKCk7XG4gICAgfVxuICB9XG5cbiAgcHJpdmF0ZSBfdXNpbmdGb250SWNvbigpOiBib29sZWFuIHtcbiAgICByZXR1cm4gIXRoaXMuc3ZnSWNvbjtcbiAgfVxuXG4gIHByaXZhdGUgX3NldFN2Z0VsZW1lbnQoc3ZnOiBTVkdFbGVtZW50KSB7XG4gICAgdGhpcy5fY2xlYXJTdmdFbGVtZW50KCk7XG5cbiAgICAvLyBXb3JrYXJvdW5kIGZvciBJRTExIGFuZCBFZGdlIGlnbm9yaW5nIGBzdHlsZWAgdGFncyBpbnNpZGUgZHluYW1pY2FsbHktY3JlYXRlZCBTVkdzLlxuICAgIC8vIFNlZTogaHR0cHM6Ly9kZXZlbG9wZXIubWljcm9zb2Z0LmNvbS9lbi11cy9taWNyb3NvZnQtZWRnZS9wbGF0Zm9ybS9pc3N1ZXMvMTA4OTg0NjkvXG4gICAgLy8gRG8gdGhpcyBiZWZvcmUgaW5zZXJ0aW5nIHRoZSBlbGVtZW50IGludG8gdGhlIERPTSwgaW4gb3JkZXIgdG8gYXZvaWQgYSBzdHlsZSByZWNhbGN1bGF0aW9uLlxuICAgIGNvbnN0IHN0eWxlVGFncyA9IHN2Zy5xdWVyeVNlbGVjdG9yQWxsKCdzdHlsZScpIGFzIE5vZGVMaXN0T2Y8SFRNTFN0eWxlRWxlbWVudD47XG5cbiAgICBmb3IgKGxldCBpID0gMDsgaSA8IHN0eWxlVGFncy5sZW5ndGg7IGkrKykge1xuICAgICAgc3R5bGVUYWdzW2ldLnRleHRDb250ZW50ICs9ICcgJztcbiAgICB9XG5cbiAgICAvLyBOb3RlOiB3ZSBkbyB0aGlzIGZpeCBoZXJlLCByYXRoZXIgdGhhbiB0aGUgaWNvbiByZWdpc3RyeSwgYmVjYXVzZSB0aGVcbiAgICAvLyByZWZlcmVuY2VzIGhhdmUgdG8gcG9pbnQgdG8gdGhlIFVSTCBhdCB0aGUgdGltZSB0aGF0IHRoZSBpY29uIHdhcyBjcmVhdGVkLlxuICAgIGNvbnN0IHBhdGggPSB0aGlzLl9sb2NhdGlvbi5nZXRQYXRobmFtZSgpO1xuICAgIHRoaXMuX3ByZXZpb3VzUGF0aCA9IHBhdGg7XG4gICAgdGhpcy5fY2FjaGVDaGlsZHJlbldpdGhFeHRlcm5hbFJlZmVyZW5jZXMoc3ZnKTtcbiAgICB0aGlzLl9wcmVwZW5kUGF0aFRvUmVmZXJlbmNlcyhwYXRoKTtcbiAgICB0aGlzLl9lbGVtZW50UmVmLm5hdGl2ZUVsZW1lbnQuYXBwZW5kQ2hpbGQoc3ZnKTtcbiAgfVxuXG4gIHByaXZhdGUgX2NsZWFyU3ZnRWxlbWVudCgpIHtcbiAgICBjb25zdCBsYXlvdXRFbGVtZW50OiBIVE1MRWxlbWVudCA9IHRoaXMuX2VsZW1lbnRSZWYubmF0aXZlRWxlbWVudDtcbiAgICBsZXQgY2hpbGRDb3VudCA9IGxheW91dEVsZW1lbnQuY2hpbGROb2Rlcy5sZW5ndGg7XG5cbiAgICBpZiAodGhpcy5fZWxlbWVudHNXaXRoRXh0ZXJuYWxSZWZlcmVuY2VzKSB7XG4gICAgICB0aGlzLl9lbGVtZW50c1dpdGhFeHRlcm5hbFJlZmVyZW5jZXMuY2xlYXIoKTtcbiAgICB9XG5cbiAgICAvLyBSZW1vdmUgZXhpc3Rpbmcgbm9uLWVsZW1lbnQgY2hpbGQgbm9kZXMgYW5kIFNWR3MsIGFuZCBhZGQgdGhlIG5ldyBTVkcgZWxlbWVudC4gTm90ZSB0aGF0XG4gICAgLy8gd2UgY2FuJ3QgdXNlIGlubmVySFRNTCwgYmVjYXVzZSBJRSB3aWxsIHRocm93IGlmIHRoZSBlbGVtZW50IGhhcyBhIGRhdGEgYmluZGluZy5cbiAgICB3aGlsZSAoY2hpbGRDb3VudC0tKSB7XG4gICAgICBjb25zdCBjaGlsZCA9IGxheW91dEVsZW1lbnQuY2hpbGROb2Rlc1tjaGlsZENvdW50XTtcblxuICAgICAgLy8gMSBjb3JyZXNwb25kcyB0byBOb2RlLkVMRU1FTlRfTk9ERS4gV2UgcmVtb3ZlIGFsbCBub24tZWxlbWVudCBub2RlcyBpbiBvcmRlciB0byBnZXQgcmlkXG4gICAgICAvLyBvZiBhbnkgbG9vc2UgdGV4dCBub2RlcywgYXMgd2VsbCBhcyBhbnkgU1ZHIGVsZW1lbnRzIGluIG9yZGVyIHRvIHJlbW92ZSBhbnkgb2xkIGljb25zLlxuICAgICAgaWYgKGNoaWxkLm5vZGVUeXBlICE9PSAxIHx8IGNoaWxkLm5vZGVOYW1lLnRvTG93ZXJDYXNlKCkgPT09ICdzdmcnKSB7XG4gICAgICAgIGxheW91dEVsZW1lbnQucmVtb3ZlQ2hpbGQoY2hpbGQpO1xuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIHByaXZhdGUgX3VwZGF0ZUZvbnRJY29uQ2xhc3NlcygpIHtcbiAgICBpZiAoIXRoaXMuX3VzaW5nRm9udEljb24oKSkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGNvbnN0IGVsZW06IEhUTUxFbGVtZW50ID0gdGhpcy5fZWxlbWVudFJlZi5uYXRpdmVFbGVtZW50O1xuICAgIGNvbnN0IGZvbnRTZXRDbGFzcyA9IHRoaXMuZm9udFNldCA/XG4gICAgICAgIHRoaXMuX2ljb25SZWdpc3RyeS5jbGFzc05hbWVGb3JGb250QWxpYXModGhpcy5mb250U2V0KSA6XG4gICAgICAgIHRoaXMuX2ljb25SZWdpc3RyeS5nZXREZWZhdWx0Rm9udFNldENsYXNzKCk7XG5cbiAgICBpZiAoZm9udFNldENsYXNzICE9IHRoaXMuX3ByZXZpb3VzRm9udFNldENsYXNzKSB7XG4gICAgICBpZiAodGhpcy5fcHJldmlvdXNGb250U2V0Q2xhc3MpIHtcbiAgICAgICAgZWxlbS5jbGFzc0xpc3QucmVtb3ZlKHRoaXMuX3ByZXZpb3VzRm9udFNldENsYXNzKTtcbiAgICAgIH1cbiAgICAgIGlmIChmb250U2V0Q2xhc3MpIHtcbiAgICAgICAgZWxlbS5jbGFzc0xpc3QuYWRkKGZvbnRTZXRDbGFzcyk7XG4gICAgICB9XG4gICAgICB0aGlzLl9wcmV2aW91c0ZvbnRTZXRDbGFzcyA9IGZvbnRTZXRDbGFzcztcbiAgICB9XG5cbiAgICBpZiAodGhpcy5mb250SWNvbiAhPSB0aGlzLl9wcmV2aW91c0ZvbnRJY29uQ2xhc3MpIHtcbiAgICAgIGlmICh0aGlzLl9wcmV2aW91c0ZvbnRJY29uQ2xhc3MpIHtcbiAgICAgICAgZWxlbS5jbGFzc0xpc3QucmVtb3ZlKHRoaXMuX3ByZXZpb3VzRm9udEljb25DbGFzcyk7XG4gICAgICB9XG4gICAgICBpZiAodGhpcy5mb250SWNvbikge1xuICAgICAgICBlbGVtLmNsYXNzTGlzdC5hZGQodGhpcy5mb250SWNvbik7XG4gICAgICB9XG4gICAgICB0aGlzLl9wcmV2aW91c0ZvbnRJY29uQ2xhc3MgPSB0aGlzLmZvbnRJY29uO1xuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBDbGVhbnMgdXAgYSB2YWx1ZSB0byBiZSB1c2VkIGFzIGEgZm9udEljb24gb3IgZm9udFNldC5cbiAgICogU2luY2UgdGhlIHZhbHVlIGVuZHMgdXAgYmVpbmcgYXNzaWduZWQgYXMgYSBDU1MgY2xhc3MsIHdlXG4gICAqIGhhdmUgdG8gdHJpbSB0aGUgdmFsdWUgYW5kIG9taXQgc3BhY2Utc2VwYXJhdGVkIHZhbHVlcy5cbiAgICovXG4gIHByaXZhdGUgX2NsZWFudXBGb250VmFsdWUodmFsdWU6IHN0cmluZykge1xuICAgIHJldHVybiB0eXBlb2YgdmFsdWUgPT09ICdzdHJpbmcnID8gdmFsdWUudHJpbSgpLnNwbGl0KCcgJylbMF0gOiB2YWx1ZTtcbiAgfVxuXG4gIC8qKlxuICAgKiBQcmVwZW5kcyB0aGUgY3VycmVudCBwYXRoIHRvIGFsbCBlbGVtZW50cyB0aGF0IGhhdmUgYW4gYXR0cmlidXRlIHBvaW50aW5nIHRvIGEgYEZ1bmNJUklgXG4gICAqIHJlZmVyZW5jZS4gVGhpcyBpcyByZXF1aXJlZCBiZWNhdXNlIFdlYktpdCBicm93c2VycyByZXF1aXJlIHJlZmVyZW5jZXMgdG8gYmUgcHJlZml4ZWQgd2l0aFxuICAgKiB0aGUgY3VycmVudCBwYXRoLCBpZiB0aGUgcGFnZSBoYXMgYSBgYmFzZWAgdGFnLlxuICAgKi9cbiAgcHJpdmF0ZSBfcHJlcGVuZFBhdGhUb1JlZmVyZW5jZXMocGF0aDogc3RyaW5nKSB7XG4gICAgY29uc3QgZWxlbWVudHMgPSB0aGlzLl9lbGVtZW50c1dpdGhFeHRlcm5hbFJlZmVyZW5jZXM7XG5cbiAgICBpZiAoZWxlbWVudHMpIHtcbiAgICAgIGVsZW1lbnRzLmZvckVhY2goKGF0dHJzLCBlbGVtZW50KSA9PiB7XG4gICAgICAgIGF0dHJzLmZvckVhY2goYXR0ciA9PiB7XG4gICAgICAgICAgZWxlbWVudC5zZXRBdHRyaWJ1dGUoYXR0ci5uYW1lLCBgdXJsKCcke3BhdGh9IyR7YXR0ci52YWx1ZX0nKWApO1xuICAgICAgICB9KTtcbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBDYWNoZXMgdGhlIGNoaWxkcmVuIG9mIGFuIFNWRyBlbGVtZW50IHRoYXQgaGF2ZSBgdXJsKClgXG4gICAqIHJlZmVyZW5jZXMgdGhhdCB3ZSBuZWVkIHRvIHByZWZpeCB3aXRoIHRoZSBjdXJyZW50IHBhdGguXG4gICAqL1xuICBwcml2YXRlIF9jYWNoZUNoaWxkcmVuV2l0aEV4dGVybmFsUmVmZXJlbmNlcyhlbGVtZW50OiBTVkdFbGVtZW50KSB7XG4gICAgY29uc3QgZWxlbWVudHNXaXRoRnVuY0lyaSA9IGVsZW1lbnQucXVlcnlTZWxlY3RvckFsbChmdW5jSXJpQXR0cmlidXRlU2VsZWN0b3IpO1xuICAgIGNvbnN0IGVsZW1lbnRzID0gdGhpcy5fZWxlbWVudHNXaXRoRXh0ZXJuYWxSZWZlcmVuY2VzID1cbiAgICAgICAgdGhpcy5fZWxlbWVudHNXaXRoRXh0ZXJuYWxSZWZlcmVuY2VzIHx8IG5ldyBNYXAoKTtcblxuICAgIGZvciAobGV0IGkgPSAwOyBpIDwgZWxlbWVudHNXaXRoRnVuY0lyaS5sZW5ndGg7IGkrKykge1xuICAgICAgZnVuY0lyaUF0dHJpYnV0ZXMuZm9yRWFjaChhdHRyID0+IHtcbiAgICAgICAgY29uc3QgZWxlbWVudFdpdGhSZWZlcmVuY2UgPSBlbGVtZW50c1dpdGhGdW5jSXJpW2ldO1xuICAgICAgICBjb25zdCB2YWx1ZSA9IGVsZW1lbnRXaXRoUmVmZXJlbmNlLmdldEF0dHJpYnV0ZShhdHRyKTtcbiAgICAgICAgY29uc3QgbWF0Y2ggPSB2YWx1ZSA/IHZhbHVlLm1hdGNoKGZ1bmNJcmlQYXR0ZXJuKSA6IG51bGw7XG5cbiAgICAgICAgaWYgKG1hdGNoKSB7XG4gICAgICAgICAgbGV0IGF0dHJpYnV0ZXMgPSBlbGVtZW50cy5nZXQoZWxlbWVudFdpdGhSZWZlcmVuY2UpO1xuXG4gICAgICAgICAgaWYgKCFhdHRyaWJ1dGVzKSB7XG4gICAgICAgICAgICBhdHRyaWJ1dGVzID0gW107XG4gICAgICAgICAgICBlbGVtZW50cy5zZXQoZWxlbWVudFdpdGhSZWZlcmVuY2UsIGF0dHJpYnV0ZXMpO1xuICAgICAgICAgIH1cblxuICAgICAgICAgIGF0dHJpYnV0ZXMhLnB1c2goe25hbWU6IGF0dHIsIHZhbHVlOiBtYXRjaFsxXX0pO1xuICAgICAgICB9XG4gICAgICB9KTtcbiAgICB9XG4gIH1cblxuICBzdGF0aWMgbmdBY2NlcHRJbnB1dFR5cGVfaW5saW5lOiBCb29sZWFuSW5wdXQ7XG59XG4iXX0=