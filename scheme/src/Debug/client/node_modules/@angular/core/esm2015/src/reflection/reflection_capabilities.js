/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { isType, Type } from '../interface/type';
import { newArray } from '../util/array_utils';
import { ANNOTATIONS, PARAMETERS, PROP_METADATA } from '../util/decorators';
import { global } from '../util/global';
import { stringify } from '../util/stringify';
/**
 * Attention: These regex has to hold even if the code is minified!
 */
export const DELEGATE_CTOR = /^function\s+\S+\(\)\s*{[\s\S]+\.apply\(this,\s*arguments\)/;
export const INHERITED_CLASS = /^class\s+[A-Za-z\d$_]*\s*extends\s+[^{]+{/;
export const INHERITED_CLASS_WITH_CTOR = /^class\s+[A-Za-z\d$_]*\s*extends\s+[^{]+{[\s\S]*constructor\s*\(/;
export const INHERITED_CLASS_WITH_DELEGATE_CTOR = /^class\s+[A-Za-z\d$_]*\s*extends\s+[^{]+{[\s\S]*constructor\s*\(\)\s*{\s*super\(\.\.\.arguments\)/;
/**
 * Determine whether a stringified type is a class which delegates its constructor
 * to its parent.
 *
 * This is not trivial since compiled code can actually contain a constructor function
 * even if the original source code did not. For instance, when the child class contains
 * an initialized instance property.
 */
export function isDelegateCtor(typeStr) {
    return DELEGATE_CTOR.test(typeStr) || INHERITED_CLASS_WITH_DELEGATE_CTOR.test(typeStr) ||
        (INHERITED_CLASS.test(typeStr) && !INHERITED_CLASS_WITH_CTOR.test(typeStr));
}
export class ReflectionCapabilities {
    constructor(reflect) {
        this._reflect = reflect || global['Reflect'];
    }
    isReflectionEnabled() {
        return true;
    }
    factory(t) {
        return (...args) => new t(...args);
    }
    /** @internal */
    _zipTypesAndAnnotations(paramTypes, paramAnnotations) {
        let result;
        if (typeof paramTypes === 'undefined') {
            result = newArray(paramAnnotations.length);
        }
        else {
            result = newArray(paramTypes.length);
        }
        for (let i = 0; i < result.length; i++) {
            // TS outputs Object for parameters without types, while Traceur omits
            // the annotations. For now we preserve the Traceur behavior to aid
            // migration, but this can be revisited.
            if (typeof paramTypes === 'undefined') {
                result[i] = [];
            }
            else if (paramTypes[i] && paramTypes[i] != Object) {
                result[i] = [paramTypes[i]];
            }
            else {
                result[i] = [];
            }
            if (paramAnnotations && paramAnnotations[i] != null) {
                result[i] = result[i].concat(paramAnnotations[i]);
            }
        }
        return result;
    }
    _ownParameters(type, parentCtor) {
        const typeStr = type.toString();
        // If we have no decorators, we only have function.length as metadata.
        // In that case, to detect whether a child class declared an own constructor or not,
        // we need to look inside of that constructor to check whether it is
        // just calling the parent.
        // This also helps to work around for https://github.com/Microsoft/TypeScript/issues/12439
        // that sets 'design:paramtypes' to []
        // if a class inherits from another class but has no ctor declared itself.
        if (isDelegateCtor(typeStr)) {
            return null;
        }
        // Prefer the direct API.
        if (type.parameters && type.parameters !== parentCtor.parameters) {
            return type.parameters;
        }
        // API of tsickle for lowering decorators to properties on the class.
        const tsickleCtorParams = type.ctorParameters;
        if (tsickleCtorParams && tsickleCtorParams !== parentCtor.ctorParameters) {
            // Newer tsickle uses a function closure
            // Retain the non-function case for compatibility with older tsickle
            const ctorParameters = typeof tsickleCtorParams === 'function' ? tsickleCtorParams() : tsickleCtorParams;
            const paramTypes = ctorParameters.map((ctorParam) => ctorParam && ctorParam.type);
            const paramAnnotations = ctorParameters.map((ctorParam) => ctorParam && convertTsickleDecoratorIntoMetadata(ctorParam.decorators));
            return this._zipTypesAndAnnotations(paramTypes, paramAnnotations);
        }
        // API for metadata created by invoking the decorators.
        const paramAnnotations = type.hasOwnProperty(PARAMETERS) && type[PARAMETERS];
        const paramTypes = this._reflect && this._reflect.getOwnMetadata &&
            this._reflect.getOwnMetadata('design:paramtypes', type);
        if (paramTypes || paramAnnotations) {
            return this._zipTypesAndAnnotations(paramTypes, paramAnnotations);
        }
        // If a class has no decorators, at least create metadata
        // based on function.length.
        // Note: We know that this is a real constructor as we checked
        // the content of the constructor above.
        return newArray(type.length);
    }
    parameters(type) {
        // Note: only report metadata if we have at least one class decorator
        // to stay in sync with the static reflector.
        if (!isType(type)) {
            return [];
        }
        const parentCtor = getParentCtor(type);
        let parameters = this._ownParameters(type, parentCtor);
        if (!parameters && parentCtor !== Object) {
            parameters = this.parameters(parentCtor);
        }
        return parameters || [];
    }
    _ownAnnotations(typeOrFunc, parentCtor) {
        // Prefer the direct API.
        if (typeOrFunc.annotations && typeOrFunc.annotations !== parentCtor.annotations) {
            let annotations = typeOrFunc.annotations;
            if (typeof annotations === 'function' && annotations.annotations) {
                annotations = annotations.annotations;
            }
            return annotations;
        }
        // API of tsickle for lowering decorators to properties on the class.
        if (typeOrFunc.decorators && typeOrFunc.decorators !== parentCtor.decorators) {
            return convertTsickleDecoratorIntoMetadata(typeOrFunc.decorators);
        }
        // API for metadata created by invoking the decorators.
        if (typeOrFunc.hasOwnProperty(ANNOTATIONS)) {
            return typeOrFunc[ANNOTATIONS];
        }
        return null;
    }
    annotations(typeOrFunc) {
        if (!isType(typeOrFunc)) {
            return [];
        }
        const parentCtor = getParentCtor(typeOrFunc);
        const ownAnnotations = this._ownAnnotations(typeOrFunc, parentCtor) || [];
        const parentAnnotations = parentCtor !== Object ? this.annotations(parentCtor) : [];
        return parentAnnotations.concat(ownAnnotations);
    }
    _ownPropMetadata(typeOrFunc, parentCtor) {
        // Prefer the direct API.
        if (typeOrFunc.propMetadata &&
            typeOrFunc.propMetadata !== parentCtor.propMetadata) {
            let propMetadata = typeOrFunc.propMetadata;
            if (typeof propMetadata === 'function' && propMetadata.propMetadata) {
                propMetadata = propMetadata.propMetadata;
            }
            return propMetadata;
        }
        // API of tsickle for lowering decorators to properties on the class.
        if (typeOrFunc.propDecorators &&
            typeOrFunc.propDecorators !== parentCtor.propDecorators) {
            const propDecorators = typeOrFunc.propDecorators;
            const propMetadata = {};
            Object.keys(propDecorators).forEach(prop => {
                propMetadata[prop] = convertTsickleDecoratorIntoMetadata(propDecorators[prop]);
            });
            return propMetadata;
        }
        // API for metadata created by invoking the decorators.
        if (typeOrFunc.hasOwnProperty(PROP_METADATA)) {
            return typeOrFunc[PROP_METADATA];
        }
        return null;
    }
    propMetadata(typeOrFunc) {
        if (!isType(typeOrFunc)) {
            return {};
        }
        const parentCtor = getParentCtor(typeOrFunc);
        const propMetadata = {};
        if (parentCtor !== Object) {
            const parentPropMetadata = this.propMetadata(parentCtor);
            Object.keys(parentPropMetadata).forEach((propName) => {
                propMetadata[propName] = parentPropMetadata[propName];
            });
        }
        const ownPropMetadata = this._ownPropMetadata(typeOrFunc, parentCtor);
        if (ownPropMetadata) {
            Object.keys(ownPropMetadata).forEach((propName) => {
                const decorators = [];
                if (propMetadata.hasOwnProperty(propName)) {
                    decorators.push(...propMetadata[propName]);
                }
                decorators.push(...ownPropMetadata[propName]);
                propMetadata[propName] = decorators;
            });
        }
        return propMetadata;
    }
    ownPropMetadata(typeOrFunc) {
        if (!isType(typeOrFunc)) {
            return {};
        }
        return this._ownPropMetadata(typeOrFunc, getParentCtor(typeOrFunc)) || {};
    }
    hasLifecycleHook(type, lcProperty) {
        return type instanceof Type && lcProperty in type.prototype;
    }
    guards(type) {
        return {};
    }
    getter(name) {
        return new Function('o', 'return o.' + name + ';');
    }
    setter(name) {
        return new Function('o', 'v', 'return o.' + name + ' = v;');
    }
    method(name) {
        const functionBody = `if (!o.${name}) throw new Error('"${name}" is undefined');
        return o.${name}.apply(o, args);`;
        return new Function('o', 'args', functionBody);
    }
    // There is not a concept of import uri in Js, but this is useful in developing Dart applications.
    importUri(type) {
        // StaticSymbol
        if (typeof type === 'object' && type['filePath']) {
            return type['filePath'];
        }
        // Runtime type
        return `./${stringify(type)}`;
    }
    resourceUri(type) {
        return `./${stringify(type)}`;
    }
    resolveIdentifier(name, moduleUrl, members, runtime) {
        return runtime;
    }
    resolveEnum(enumIdentifier, name) {
        return enumIdentifier[name];
    }
}
function convertTsickleDecoratorIntoMetadata(decoratorInvocations) {
    if (!decoratorInvocations) {
        return [];
    }
    return decoratorInvocations.map(decoratorInvocation => {
        const decoratorType = decoratorInvocation.type;
        const annotationCls = decoratorType.annotationCls;
        const annotationArgs = decoratorInvocation.args ? decoratorInvocation.args : [];
        return new annotationCls(...annotationArgs);
    });
}
function getParentCtor(ctor) {
    const parentProto = ctor.prototype ? Object.getPrototypeOf(ctor.prototype) : null;
    const parentCtor = parentProto ? parentProto.constructor : null;
    // Note: We always use `Object` as the null value
    // to simplify checking later on.
    return parentCtor || Object;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicmVmbGVjdGlvbl9jYXBhYmlsaXRpZXMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9jb3JlL3NyYy9yZWZsZWN0aW9uL3JlZmxlY3Rpb25fY2FwYWJpbGl0aWVzLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU8sRUFBQyxNQUFNLEVBQUUsSUFBSSxFQUFDLE1BQU0sbUJBQW1CLENBQUM7QUFDL0MsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLHFCQUFxQixDQUFDO0FBQzdDLE9BQU8sRUFBQyxXQUFXLEVBQUUsVUFBVSxFQUFFLGFBQWEsRUFBQyxNQUFNLG9CQUFvQixDQUFDO0FBQzFFLE9BQU8sRUFBQyxNQUFNLEVBQUMsTUFBTSxnQkFBZ0IsQ0FBQztBQUN0QyxPQUFPLEVBQUMsU0FBUyxFQUFDLE1BQU0sbUJBQW1CLENBQUM7QUFPNUM7O0dBRUc7QUFDSCxNQUFNLENBQUMsTUFBTSxhQUFhLEdBQUcsNERBQTRELENBQUM7QUFDMUYsTUFBTSxDQUFDLE1BQU0sZUFBZSxHQUFHLDJDQUEyQyxDQUFDO0FBQzNFLE1BQU0sQ0FBQyxNQUFNLHlCQUF5QixHQUNsQyxrRUFBa0UsQ0FBQztBQUN2RSxNQUFNLENBQUMsTUFBTSxrQ0FBa0MsR0FDM0MsbUdBQW1HLENBQUM7QUFFeEc7Ozs7Ozs7R0FPRztBQUNILE1BQU0sVUFBVSxjQUFjLENBQUMsT0FBZTtJQUM1QyxPQUFPLGFBQWEsQ0FBQyxJQUFJLENBQUMsT0FBTyxDQUFDLElBQUksa0NBQWtDLENBQUMsSUFBSSxDQUFDLE9BQU8sQ0FBQztRQUNsRixDQUFDLGVBQWUsQ0FBQyxJQUFJLENBQUMsT0FBTyxDQUFDLElBQUksQ0FBQyx5QkFBeUIsQ0FBQyxJQUFJLENBQUMsT0FBTyxDQUFDLENBQUMsQ0FBQztBQUNsRixDQUFDO0FBRUQsTUFBTSxPQUFPLHNCQUFzQjtJQUdqQyxZQUFZLE9BQWE7UUFDdkIs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QUFpQixFQUFFLE9BQWlCLEVBQUUsT0FBWTtRQUNoRixPQUFPLE9BQU8sQ0FBQztJQUNqQixDQUFDO0lBQ0QsV0FBVyxDQUFDLGNBQW1CLEVBQUUsSUFBWTtRQUMzQyxPQUFPLGNBQWMsQ0FBQyxJQUFJLENBQUMsQ0FBQztJQUM5QixDQUFDO0NBQ0Y7QUFFRCxTQUFTLG1DQUFtQyxDQUFDLG9CQUEyQjtJQUN0RSxJQUFJLENBQUMsb0JBQW9CLEVBQUU7UUFDekIsT0FBTyxFQUFFLENBQUM7S0FDWDtJQUNELE9BQU8sb0JBQW9CLENBQUMsR0FBRyxDQUFDLG1CQUFtQixDQUFDLEVBQUU7UUFDcEQsTUFBTSxhQUFhLEdBQUcsbUJBQW1CLENBQUMsSUFBSSxDQUFDO1FBQy9DLE1BQU0sYUFBYSxHQUFHLGFBQWEsQ0FBQyxhQUFhLENBQUM7UUFDbEQsTUFBTSxjQUFjLEdBQUcsbUJBQW1CLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQyxtQkFBbUIsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLEVBQUUsQ0FBQztRQUNoRixPQUFPLElBQUksYUFBYSxDQUFDLEdBQUcsY0FBYyxDQUFDLENBQUM7SUFDOUMsQ0FBQyxDQUFDLENBQUM7QUFDTCxDQUFDO0FBRUQsU0FBUyxhQUFhLENBQUMsSUFBYztJQUNuQyxNQUFNLFdBQVcsR0FBRyxJQUFJLENBQUMsU0FBUyxDQUFDLENBQUMsQ0FBQyxNQUFNLENBQUMsY0FBYyxDQUFDLElBQUksQ0FBQyxTQUFTLENBQUMsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDO0lBQ2xGLE1BQU0sVUFBVSxHQUFHLFdBQVcsQ0FBQyxDQUFDLENBQUMsV0FBVyxDQUFDLFdBQVcsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDO0lBQ2hFLGlEQUFpRDtJQUNqRCxpQ0FBaUM7SUFDakMsT0FBTyxVQUFVLElBQUksTUFBTSxDQUFDO0FBQzlCLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtpc1R5cGUsIFR5cGV9IGZyb20gJy4uL2ludGVyZmFjZS90eXBlJztcbmltcG9ydCB7bmV3QXJyYXl9IGZyb20gJy4uL3V0aWwvYXJyYXlfdXRpbHMnO1xuaW1wb3J0IHtBTk5PVEFUSU9OUywgUEFSQU1FVEVSUywgUFJPUF9NRVRBREFUQX0gZnJvbSAnLi4vdXRpbC9kZWNvcmF0b3JzJztcbmltcG9ydCB7Z2xvYmFsfSBmcm9tICcuLi91dGlsL2dsb2JhbCc7XG5pbXBvcnQge3N0cmluZ2lmeX0gZnJvbSAnLi4vdXRpbC9zdHJpbmdpZnknO1xuXG5pbXBvcnQge1BsYXRmb3JtUmVmbGVjdGlvbkNhcGFiaWxpdGllc30gZnJvbSAnLi9wbGF0Zm9ybV9yZWZsZWN0aW9uX2NhcGFiaWxpdGllcyc7XG5pbXBvcnQge0dldHRlckZuLCBNZXRob2RGbiwgU2V0dGVyRm59IGZyb20gJy4vdHlwZXMnO1xuXG5cblxuLyoqXG4gKiBBdHRlbnRpb246IFRoZXNlIHJlZ2V4IGhhcyB0byBob2xkIGV2ZW4gaWYgdGhlIGNvZGUgaXMgbWluaWZpZWQhXG4gKi9cbmV4cG9ydCBjb25zdCBERUxFR0FURV9DVE9SID0gL15mdW5jdGlvblxccytcXFMrXFwoXFwpXFxzKntbXFxzXFxTXStcXC5hcHBseVxcKHRoaXMsXFxzKmFyZ3VtZW50c1xcKS87XG5leHBvcnQgY29uc3QgSU5IRVJJVEVEX0NMQVNTID0gL15jbGFzc1xccytbQS1aYS16XFxkJF9dKlxccypleHRlbmRzXFxzK1tee10rey87XG5leHBvcnQgY29uc3QgSU5IRVJJVEVEX0NMQVNTX1dJVEhfQ1RPUiA9XG4gICAgL15jbGFzc1xccytbQS1aYS16XFxkJF9dKlxccypleHRlbmRzXFxzK1tee10re1tcXHNcXFNdKmNvbnN0cnVjdG9yXFxzKlxcKC87XG5leHBvcnQgY29uc3QgSU5IRVJJVEVEX0NMQVNTX1dJVEhfREVMRUdBVEVfQ1RPUiA9XG4gICAgL15jbGFzc1xccytbQS1aYS16XFxkJF9dKlxccypleHRlbmRzXFxzK1tee10re1tcXHNcXFNdKmNvbnN0cnVjdG9yXFxzKlxcKFxcKVxccyp7XFxzKnN1cGVyXFwoXFwuXFwuXFwuYXJndW1lbnRzXFwpLztcblxuLyoqXG4gKiBEZXRlcm1pbmUgd2hldGhlciBhIHN0cmluZ2lmaWVkIHR5cGUgaXMgYSBjbGFzcyB3aGljaCBkZWxlZ2F0ZXMgaXRzIGNvbnN0cnVjdG9yXG4gKiB0byBpdHMgcGFyZW50LlxuICpcbiAqIFRoaXMgaXMgbm90IHRyaXZpYWwgc2luY2UgY29tcGlsZWQgY29kZSBjYW4gYWN0dWFsbHkgY29udGFpbiBhIGNvbnN0cnVjdG9yIGZ1bmN0aW9uXG4gKiBldmVuIGlmIHRoZSBvcmlnaW5hbCBzb3VyY2UgY29kZSBkaWQgbm90LiBGb3IgaW5zdGFuY2UsIHdoZW4gdGhlIGNoaWxkIGNsYXNzIGNvbnRhaW5zXG4gKiBhbiBpbml0aWFsaXplZCBpbnN0YW5jZSBwcm9wZXJ0eS5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGlzRGVsZWdhdGVDdG9yKHR5cGVTdHI6IHN0cmluZyk6IGJvb2xlYW4ge1xuICByZXR1cm4gREVMRUdBVEVfQ1RPUi50ZXN0KHR5cGVTdHIpIHx8IElOSEVSSVRFRF9DTEFTU19XSVRIX0RFTEVHQVRFX0NUT1IudGVzdCh0eXBlU3RyKSB8fFxuICAgICAgKElOSEVSSVRFRF9DTEFTUy50ZXN0KHR5cGVTdHIpICYmICFJTkhFUklURURfQ0xBU1NfV0lUSF9DVE9SLnRlc3QodHlwZVN0cikpO1xufVxuXG5leHBvcnQgY2xhc3MgUmVmbGVjdGlvbkNhcGFiaWxpdGllcyBpbXBsZW1lbnRzIFBsYXRmb3JtUmVmbGVjdGlvbkNhcGFiaWxpdGllcyB7XG4gIHByaXZhdGUgX3JlZmxlY3Q6IGFueTtcblxuICBjb25zdHJ1Y3RvcihyZWZsZWN0PzogYW55KSB7XG4gICAgdGhpcy5fcmVmbGVjdCA9IHJlZmxlY3QgfHwgZ2xvYmFsWydSZWZsZWN0J107XG4gIH1cblxuICBpc1JlZmxlY3Rpb25FbmFibGVkKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0cnVlO1xuICB9XG5cbiAgZmFjdG9yeTxUPih0OiBUeXBlPFQ+KTogKGFyZ3M6IGFueVtdKSA9PiBUIHtcbiAgICByZXR1cm4gKC4uLmFyZ3M6IGFueVtdKSA9PiBuZXcgdCguLi5hcmdzKTtcbiAgfVxuXG4gIC8qKiBAaW50ZXJuYWwgKi9cbiAgX3ppcFR5cGVzQW5kQW5ub3RhdGlvbnMocGFyYW1UeXBlczogYW55W10sIHBhcmFtQW5ub3RhdGlvbnM6IGFueVtdKTogYW55W11bXSB7XG4gICAgbGV0IHJlc3VsdDogYW55W11bXTtcblxuICAgIGlmICh0eXBlb2YgcGFyYW1UeXBlcyA9PT0gJ3VuZGVmaW5lZCcpIHtcbiAgICAgIHJlc3VsdCA9IG5ld0FycmF5KHBhcmFtQW5ub3RhdGlvbnMubGVuZ3RoKTtcbiAgICB9IGVsc2Uge1xuICAgICAgcmVzdWx0ID0gbmV3QXJyYXkocGFyYW1UeXBlcy5sZW5ndGgpO1xuICAgIH1cblxuICAgIGZvciAobGV0IGkgPSAwOyBpIDwgcmVzdWx0Lmxlbmd0aDsgaSsrKSB7XG4gICAgICAvLyBUUyBvdXRwdXRzIE9iamVjdCBmb3IgcGFyYW1ldGVycyB3aXRob3V0IHR5cGVzLCB3aGlsZSBUcmFjZXVyIG9taXRzXG4gICAgICAvLyB0aGUgYW5ub3RhdGlvbnMuIEZvciBub3cgd2UgcHJlc2VydmUgdGhlIFRyYWNldXIgYmVoYXZpb3IgdG8gYWlkXG4gICAgICAvLyBtaWdyYXRpb24sIGJ1dCB0aGlzIGNhbiBiZSByZXZpc2l0ZWQuXG4gICAgICBpZiAodHlwZW9mIHBhcmFtVHlwZXMgPT09ICd1bmRlZmluZWQnKSB7XG4gICAgICAgIHJlc3VsdFtpXSA9IFtdO1xuICAgICAgfSBlbHNlIGlmIChwYXJhbVR5cGVzW2ldICYmIHBhcmFtVHlwZXNbaV0gIT0gT2JqZWN0KSB7XG4gICAgICAgIHJlc3VsdFtpXSA9IFtwYXJhbVR5cGVzW2ldXTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIHJlc3VsdFtpXSA9IFtdO1xuICAgICAgfVxuICAgICAgaWYgKHBhcmFtQW5ub3RhdGlvbnMgJiYgcGFyYW1Bbm5vdGF0aW9uc1tpXSAhPSBudWxsKSB7XG4gICAgICAgIHJlc3VsdFtpXSA9IHJlc3VsdFtpXS5jb25jYXQocGFyYW1Bbm5vdGF0aW9uc1tpXSk7XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiByZXN1bHQ7XG4gIH1cblxuICBwcml2YXRlIF9vd25QYXJhbWV0ZXJzKHR5cGU6IFR5cGU8YW55PiwgcGFyZW50Q3RvcjogYW55KTogYW55W11bXXxudWxsIHtcbiAgICBjb25zdCB0eXBlU3RyID0gdHlwZS50b1N0cmluZygpO1xuICAgIC8vIElmIHdlIGhhdmUgbm8gZGVjb3JhdG9ycywgd2Ugb25seSBoYXZlIGZ1bmN0aW9uLmxlbmd0aCBhcyBtZXRhZGF0YS5cbiAgICAvLyBJbiB0aGF0IGNhc2UsIHRvIGRldGVjdCB3aGV0aGVyIGEgY2hpbGQgY2xhc3MgZGVjbGFyZWQgYW4gb3duIGNvbnN0cnVjdG9yIG9yIG5vdCxcbiAgICAvLyB3ZSBuZWVkIHRvIGxvb2sgaW5zaWRlIG9mIHRoYXQgY29uc3RydWN0b3IgdG8gY2hlY2sgd2hldGhlciBpdCBpc1xuICAgIC8vIGp1c3QgY2FsbGluZyB0aGUgcGFyZW50LlxuICAgIC8vIFRoaXMgYWxzbyBoZWxwcyB0byB3b3JrIGFyb3VuZCBmb3IgaHR0cHM6Ly9naXRodWIuY29tL01pY3Jvc29mdC9UeXBlU2NyaXB0L2lzc3Vlcy8xMjQzOVxuICAgIC8vIHRoYXQgc2V0cyAnZGVzaWduOnBhcmFtdHlwZXMnIHRvIFtdXG4gICAgLy8gaWYgYSBjbGFzcyBpbmhlcml0cyBmcm9tIGFub3RoZXIgY2xhc3MgYnV0IGhhcyBubyBjdG9yIGRlY2xhcmVkIGl0c2VsZi5cbiAgICBpZiAoaXNEZWxlZ2F0ZUN0b3IodHlwZVN0cikpIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIC8vIFByZWZlciB0aGUgZGlyZWN0IEFQSS5cbiAgICBpZiAoKDxhbnk+dHlwZSkucGFyYW1ldGVycyAmJiAoPGFueT50eXBlKS5wYXJhbWV0ZXJzICE9PSBwYXJlbnRDdG9yLnBhcmFtZXRlcnMpIHtcbiAgICAgIHJldHVybiAoPGFueT50eXBlKS5wYXJhbWV0ZXJzO1xuICAgIH1cblxuICAgIC8vIEFQSSBvZiB0c2lja2xlIGZvciBsb3dlcmluZyBkZWNvcmF0b3JzIHRvIHByb3BlcnRpZXMgb24gdGhlIGNsYXNzLlxuICAgIGNvbnN0IHRzaWNrbGVDdG9yUGFyYW1zID0gKDxhbnk+dHlwZSkuY3RvclBhcmFtZXRlcnM7XG4gICAgaWYgKHRzaWNrbGVDdG9yUGFyYW1zICYmIHRzaWNrbGVDdG9yUGFyYW1zICE9PSBwYXJlbnRDdG9yLmN0b3JQYXJhbWV0ZXJzKSB7XG4gICAgICAvLyBOZXdlciB0c2lja2xlIHVzZXMgYSBmdW5jdGlvbiBjbG9zdXJlXG4gICAgICAvLyBSZXRhaW4gdGhlIG5vbi1mdW5jdGlvbiBjYXNlIGZvciBjb21wYXRpYmlsaXR5IHdpdGggb2xkZXIgdHNpY2tsZVxuICAgICAgY29uc3QgY3RvclBhcmFtZXRlcnMgPVxuICAgICAgICAgIHR5cGVvZiB0c2lja2xlQ3RvclBhcmFtcyA9PT0gJ2Z1bmN0aW9uJyA/IHRzaWNrbGVDdG9yUGFyYW1zKCkgOiB0c2lja2xlQ3RvclBhcmFtcztcbiAgICAgIGNvbnN0IHBhcmFtVHlwZXMgPSBjdG9yUGFyYW1ldGVycy5tYXAoKGN0b3JQYXJhbTogYW55KSA9PiBjdG9yUGFyYW0gJiYgY3RvclBhcmFtLnR5cGUpO1xuICAgICAgY29uc3QgcGFyYW1Bbm5vdGF0aW9ucyA9IGN0b3JQYXJhbWV0ZXJzLm1hcChcbiAgICAgICAgICAoY3RvclBhcmFtOiBhbnkpID0+XG4gICAgICAgICAgICAgIGN0b3JQYXJhbSAmJiBjb252ZXJ0VHNpY2tsZURlY29yYXRvckludG9NZXRhZGF0YShjdG9yUGFyYW0uZGVjb3JhdG9ycykpO1xuICAgICAgcmV0dXJuIHRoaXMuX3ppcFR5cGVzQW5kQW5ub3RhdGlvbnMocGFyYW1UeXBlcywgcGFyYW1Bbm5vdGF0aW9ucyk7XG4gICAgfVxuXG4gICAgLy8gQVBJIGZvciBtZXRhZGF0YSBjcmVhdGVkIGJ5IGludm9raW5nIHRoZSBkZWNvcmF0b3JzLlxuICAgIGNvbnN0IHBhcmFtQW5ub3RhdGlvbnMgPSB0eXBlLmhhc093blByb3BlcnR5KFBBUkFNRVRFUlMpICYmICh0eXBlIGFzIGFueSlbUEFSQU1FVEVSU107XG4gICAgY29uc3QgcGFyYW1UeXBlcyA9IHRoaXMuX3JlZmxlY3QgJiYgdGhpcy5fcmVmbGVjdC5nZXRPd25NZXRhZGF0YSAmJlxuICAgICAgICB0aGlzLl9yZWZsZWN0LmdldE93bk1ldGFkYXRhKCdkZXNpZ246cGFyYW10eXBlcycsIHR5cGUpO1xuICAgIGlmIChwYXJhbVR5cGVzIHx8IHBhcmFtQW5ub3RhdGlvbnMpIHtcbiAgICAgIHJldHVybiB0aGlzLl96aXBUeXBlc0FuZEFubm90YXRpb25zKHBhcmFtVHlwZXMsIHBhcmFtQW5ub3RhdGlvbnMpO1xuICAgIH1cblxuICAgIC8vIElmIGEgY2xhc3MgaGFzIG5vIGRlY29yYXRvcnMsIGF0IGxlYXN0IGNyZWF0ZSBtZXRhZGF0YVxuICAgIC8vIGJhc2VkIG9uIGZ1bmN0aW9uLmxlbmd0aC5cbiAgICAvLyBOb3RlOiBXZSBrbm93IHRoYXQgdGhpcyBpcyBhIHJlYWwgY29uc3RydWN0b3IgYXMgd2UgY2hlY2tlZFxuICAgIC8vIHRoZSBjb250ZW50IG9mIHRoZSBjb25zdHJ1Y3RvciBhYm92ZS5cbiAgICByZXR1cm4gbmV3QXJyYXk8YW55W10+KHR5cGUubGVuZ3RoKTtcbiAgfVxuXG4gIHBhcmFtZXRlcnModHlwZTogVHlwZTxhbnk+KTogYW55W11bXSB7XG4gICAgLy8gTm90ZTogb25seSByZXBvcnQgbWV0YWRhdGEgaWYgd2UgaGF2ZSBhdCBsZWFzdCBvbmUgY2xhc3MgZGVjb3JhdG9yXG4gICAgLy8gdG8gc3RheSBpbiBzeW5jIHdpdGggdGhlIHN0YXRpYyByZWZsZWN0b3IuXG4gICAgaWYgKCFpc1R5cGUodHlwZSkpIHtcbiAgICAgIHJldHVybiBbXTtcbiAgICB9XG4gICAgY29uc3QgcGFyZW50Q3RvciA9IGdldFBhcmVudEN0b3IodHlwZSk7XG4gICAgbGV0IHBhcmFtZXRlcnMgPSB0aGlzLl9vd25QYXJhbWV0ZXJzKHR5cGUsIHBhcmVudEN0b3IpO1xuICAgIGlmICghcGFyYW1ldGVycyAmJiBwYXJlbnRDdG9yICE9PSBPYmplY3QpIHtcbiAgICAgIHBhcmFtZXRlcnMgPSB0aGlzLnBhcmFtZXRlcnMocGFyZW50Q3Rvcik7XG4gICAgfVxuICAgIHJldHVybiBwYXJhbWV0ZXJzIHx8IFtdO1xuICB9XG5cbiAgcHJpdmF0ZSBfb3duQW5ub3RhdGlvbnModHlwZU9yRnVuYzogVHlwZTxhbnk+LCBwYXJlbnRDdG9yOiBhbnkpOiBhbnlbXXxudWxsIHtcbiAgICAvLyBQcmVmZXIgdGhlIGRpcmVjdCBBUEkuXG4gICAgaWYgKCg8YW55PnR5cGVPckZ1bmMpLmFubm90YXRpb25zICYmICg8YW55PnR5cGVPckZ1bmMpLmFubm90YXRpb25zICE9PSBwYXJlbnRDdG9yLmFubm90YXRpb25zKSB7XG4gICAgICBsZXQgYW5ub3RhdGlvbnMgPSAoPGFueT50eXBlT3JGdW5jKS5hbm5vdGF0aW9ucztcbiAgICAgIGlmICh0eXBlb2YgYW5ub3RhdGlvbnMgPT09ICdmdW5jdGlvbicgJiYgYW5ub3RhdGlvbnMuYW5ub3RhdGlvbnMpIHtcbiAgICAgICAgYW5ub3RhdGlvbnMgPSBhbm5vdGF0aW9ucy5hbm5vdGF0aW9ucztcbiAgICAgIH1cbiAgICAgIHJldHVybiBhbm5vdGF0aW9ucztcbiAgICB9XG5cbiAgICAvLyBBUEkgb2YgdHNpY2tsZSBmb3IgbG93ZXJpbmcgZGVjb3JhdG9ycyB0byBwcm9wZXJ0aWVzIG9uIHRoZSBjbGFzcy5cbiAgICBpZiAoKDxhbnk+dHlwZU9yRnVuYykuZGVjb3JhdG9ycyAmJiAoPGFueT50eXBlT3JGdW5jKS5kZWNvcmF0b3JzICE9PSBwYXJlbnRDdG9yLmRlY29yYXRvcnMpIHtcbiAgICAgIHJldHVybiBjb252ZXJ0VHNpY2tsZURlY29yYXRvckludG9NZXRhZGF0YSgoPGFueT50eXBlT3JGdW5jKS5kZWNvcmF0b3JzKTtcbiAgICB9XG5cbiAgICAvLyBBUEkgZm9yIG1ldGFkYXRhIGNyZWF0ZWQgYnkgaW52b2tpbmcgdGhlIGRlY29yYXRvcnMuXG4gICAgaWYgKHR5cGVPckZ1bmMuaGFzT3duUHJvcGVydHkoQU5OT1RBVElPTlMpKSB7XG4gICAgICByZXR1cm4gKHR5cGVPckZ1bmMgYXMgYW55KVtBTk5PVEFUSU9OU107XG4gICAgfVxuICAgIHJldHVybiBudWxsO1xuICB9XG5cbiAgYW5ub3RhdGlvbnModHlwZU9yRnVuYzogVHlwZTxhbnk+KTogYW55W10ge1xuICAgIGlmICghaXNUeXBlKHR5cGVPckZ1bmMpKSB7XG4gICAgICByZXR1cm4gW107XG4gICAgfVxuICAgIGNvbnN0IHBhcmVudEN0b3IgPSBnZXRQYXJlbnRDdG9yKHR5cGVPckZ1bmMpO1xuICAgIGNvbnN0IG93bkFubm90YXRpb25zID0gdGhpcy5fb3duQW5ub3RhdGlvbnModHlwZU9yRnVuYywgcGFyZW50Q3RvcikgfHwgW107XG4gICAgY29uc3QgcGFyZW50QW5ub3RhdGlvbnMgPSBwYXJlbnRDdG9yICE9PSBPYmplY3QgPyB0aGlzLmFubm90YXRpb25zKHBhcmVudEN0b3IpIDogW107XG4gICAgcmV0dXJuIHBhcmVudEFubm90YXRpb25zLmNvbmNhdChvd25Bbm5vdGF0aW9ucyk7XG4gIH1cblxuICBwcml2YXRlIF9vd25Qcm9wTWV0YWRhdGEodHlwZU9yRnVuYzogYW55LCBwYXJlbnRDdG9yOiBhbnkpOiB7W2tleTogc3RyaW5nXTogYW55W119fG51bGwge1xuICAgIC8vIFByZWZlciB0aGUgZGlyZWN0IEFQSS5cbiAgICBpZiAoKDxhbnk+dHlwZU9yRnVuYykucHJvcE1ldGFkYXRhICYmXG4gICAgICAgICg8YW55PnR5cGVPckZ1bmMpLnByb3BNZXRhZGF0YSAhPT0gcGFyZW50Q3Rvci5wcm9wTWV0YWRhdGEpIHtcbiAgICAgIGxldCBwcm9wTWV0YWRhdGEgPSAoPGFueT50eXBlT3JGdW5jKS5wcm9wTWV0YWRhdGE7XG4gICAgICBpZiAodHlwZW9mIHByb3BNZXRhZGF0YSA9PT0gJ2Z1bmN0aW9uJyAmJiBwcm9wTWV0YWRhdGEucHJvcE1ldGFkYXRhKSB7XG4gICAgICAgIHByb3BNZXRhZGF0YSA9IHByb3BNZXRhZGF0YS5wcm9wTWV0YWRhdGE7XG4gICAgICB9XG4gICAgICByZXR1cm4gcHJvcE1ldGFkYXRhO1xuICAgIH1cblxuICAgIC8vIEFQSSBvZiB0c2lja2xlIGZvciBsb3dlcmluZyBkZWNvcmF0b3JzIHRvIHByb3BlcnRpZXMgb24gdGhlIGNsYXNzLlxuICAgIGlmICgoPGFueT50eXBlT3JGdW5jKS5wcm9wRGVjb3JhdG9ycyAmJlxuICAgICAgICAoPGFueT50eXBlT3JGdW5jKS5wcm9wRGVjb3JhdG9ycyAhPT0gcGFyZW50Q3Rvci5wcm9wRGVjb3JhdG9ycykge1xuICAgICAgY29uc3QgcHJvcERlY29yYXRvcnMgPSAoPGFueT50eXBlT3JGdW5jKS5wcm9wRGVjb3JhdG9ycztcbiAgICAgIGNvbnN0IHByb3BNZXRhZGF0YSA9IDx7W2tleTogc3RyaW5nXTogYW55W119Pnt9O1xuICAgICAgT2JqZWN0LmtleXMocHJvcERlY29yYXRvcnMpLmZvckVhY2gocHJvcCA9PiB7XG4gICAgICAgIHByb3BNZXRhZGF0YVtwcm9wXSA9IGNvbnZlcnRUc2lja2xlRGVjb3JhdG9ySW50b01ldGFkYXRhKHByb3BEZWNvcmF0b3JzW3Byb3BdKTtcbiAgICAgIH0pO1xuICAgICAgcmV0dXJuIHByb3BNZXRhZGF0YTtcbiAgICB9XG5cbiAgICAvLyBBUEkgZm9yIG1ldGFkYXRhIGNyZWF0ZWQgYnkgaW52b2tpbmcgdGhlIGRlY29yYXRvcnMuXG4gICAgaWYgKHR5cGVPckZ1bmMuaGFzT3duUHJvcGVydHkoUFJPUF9NRVRBREFUQSkpIHtcbiAgICAgIHJldHVybiAodHlwZU9yRnVuYyBhcyBhbnkpW1BST1BfTUVUQURBVEFdO1xuICAgIH1cbiAgICByZXR1cm4gbnVsbDtcbiAgfVxuXG4gIHByb3BNZXRhZGF0YSh0eXBlT3JGdW5jOiBhbnkpOiB7W2tleTogc3RyaW5nXTogYW55W119IHtcbiAgICBpZiAoIWlzVHlwZSh0eXBlT3JGdW5jKSkge1xuICAgICAgcmV0dXJuIHt9O1xuICAgIH1cbiAgICBjb25zdCBwYXJlbnRDdG9yID0gZ2V0UGFyZW50Q3Rvcih0eXBlT3JGdW5jKTtcbiAgICBjb25zdCBwcm9wTWV0YWRhdGE6IHtba2V5OiBzdHJpbmddOiBhbnlbXX0gPSB7fTtcbiAgICBpZiAocGFyZW50Q3RvciAhPT0gT2JqZWN0KSB7XG4gICAgICBjb25zdCBwYXJlbnRQcm9wTWV0YWRhdGEgPSB0aGlzLnByb3BNZXRhZGF0YShwYXJlbnRDdG9yKTtcbiAgICAgIE9iamVjdC5rZXlzKHBhcmVudFByb3BNZXRhZGF0YSkuZm9yRWFjaCgocHJvcE5hbWUpID0+IHtcbiAgICAgICAgcHJvcE1ldGFkYXRhW3Byb3BOYW1lXSA9IHBhcmVudFByb3BNZXRhZGF0YVtwcm9wTmFtZV07XG4gICAgICB9KTtcbiAgICB9XG4gICAgY29uc3Qgb3duUHJvcE1ldGFkYXRhID0gdGhpcy5fb3duUHJvcE1ldGFkYXRhKHR5cGVPckZ1bmMsIHBhcmVudEN0b3IpO1xuICAgIGlmIChvd25Qcm9wTWV0YWRhdGEpIHtcbiAgICAgIE9iamVjdC5rZXlzKG93blByb3BNZXRhZGF0YSkuZm9yRWFjaCgocHJvcE5hbWUpID0+IHtcbiAgICAgICAgY29uc3QgZGVjb3JhdG9yczogYW55W10gPSBbXTtcbiAgICAgICAgaWYgKHByb3BNZXRhZGF0YS5oYXNPd25Qcm9wZXJ0eShwcm9wTmFtZSkpIHtcbiAgICAgICAgICBkZWNvcmF0b3JzLnB1c2goLi4ucHJvcE1ldGFkYXRhW3Byb3BOYW1lXSk7XG4gICAgICAgIH1cbiAgICAgICAgZGVjb3JhdG9ycy5wdXNoKC4uLm93blByb3BNZXRhZGF0YVtwcm9wTmFtZV0pO1xuICAgICAgICBwcm9wTWV0YWRhdGFbcHJvcE5hbWVdID0gZGVjb3JhdG9ycztcbiAgICAgIH0pO1xuICAgIH1cbiAgICByZXR1cm4gcHJvcE1ldGFkYXRhO1xuICB9XG5cbiAgb3duUHJvcE1ldGFkYXRhKHR5cGVPckZ1bmM6IGFueSk6IHtba2V5OiBzdHJpbmddOiBhbnlbXX0ge1xuICAgIGlmICghaXNUeXBlKHR5cGVPckZ1bmMpKSB7XG4gICAgICByZXR1cm4ge307XG4gICAgfVxuICAgIHJldHVybiB0aGlzLl9vd25Qcm9wTWV0YWRhdGEodHlwZU9yRnVuYywgZ2V0UGFyZW50Q3Rvcih0eXBlT3JGdW5jKSkgfHwge307XG4gIH1cblxuICBoYXNMaWZlY3ljbGVIb29rKHR5cGU6IGFueSwgbGNQcm9wZXJ0eTogc3RyaW5nKTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIHR5cGUgaW5zdGFuY2VvZiBUeXBlICYmIGxjUHJvcGVydHkgaW4gdHlwZS5wcm90b3R5cGU7XG4gIH1cblxuICBndWFyZHModHlwZTogYW55KToge1trZXk6IHN0cmluZ106IGFueX0ge1xuICAgIHJldHVybiB7fTtcbiAgfVxuXG4gIGdldHRlcihuYW1lOiBzdHJpbmcpOiBHZXR0ZXJGbiB7XG4gICAgcmV0dXJuIDxHZXR0ZXJGbj5uZXcgRnVuY3Rpb24oJ28nLCAncmV0dXJuIG8uJyArIG5hbWUgKyAnOycpO1xuICB9XG5cbiAgc2V0dGVyKG5hbWU6IHN0cmluZyk6IFNldHRlckZuIHtcbiAgICByZXR1cm4gPFNldHRlckZuPm5ldyBGdW5jdGlvbignbycsICd2JywgJ3JldHVybiBvLicgKyBuYW1lICsgJyA9IHY7Jyk7XG4gIH1cblxuICBtZXRob2QobmFtZTogc3RyaW5nKTogTWV0aG9kRm4ge1xuICAgIGNvbnN0IGZ1bmN0aW9uQm9keSA9IGBpZiAoIW8uJHtuYW1lfSkgdGhyb3cgbmV3IEVycm9yKCdcIiR7bmFtZX1cIiBpcyB1bmRlZmluZWQnKTtcbiAgICAgICAgcmV0dXJuIG8uJHtuYW1lfS5hcHBseShvLCBhcmdzKTtgO1xuICAgIHJldHVybiA8TWV0aG9kRm4+bmV3IEZ1bmN0aW9uKCdvJywgJ2FyZ3MnLCBmdW5jdGlvbkJvZHkpO1xuICB9XG5cbiAgLy8gVGhlcmUgaXMgbm90IGEgY29uY2VwdCBvZiBpbXBvcnQgdXJpIGluIEpzLCBidXQgdGhpcyBpcyB1c2VmdWwgaW4gZGV2ZWxvcGluZyBEYXJ0IGFwcGxpY2F0aW9ucy5cbiAgaW1wb3J0VXJpKHR5cGU6IGFueSk6IHN0cmluZyB7XG4gICAgLy8gU3RhdGljU3ltYm9sXG4gICAgaWYgKHR5cGVvZiB0eXBlID09PSAnb2JqZWN0JyAmJiB0eXBlWydmaWxlUGF0aCddKSB7XG4gICAgICByZXR1cm4gdHlwZVsnZmlsZVBhdGgnXTtcbiAgICB9XG4gICAgLy8gUnVudGltZSB0eXBlXG4gICAgcmV0dXJuIGAuLyR7c3RyaW5naWZ5KHR5cGUpfWA7XG4gIH1cblxuICByZXNvdXJjZVVyaSh0eXBlOiBhbnkpOiBzdHJpbmcge1xuICAgIHJldHVybiBgLi8ke3N0cmluZ2lmeSh0eXBlKX1gO1xuICB9XG5cbiAgcmVzb2x2ZUlkZW50aWZpZXIobmFtZTogc3RyaW5nLCBtb2R1bGVVcmw6IHN0cmluZywgbWVtYmVyczogc3RyaW5nW10sIHJ1bnRpbWU6IGFueSk6IGFueSB7XG4gICAgcmV0dXJuIHJ1bnRpbWU7XG4gIH1cbiAgcmVzb2x2ZUVudW0oZW51bUlkZW50aWZpZXI6IGFueSwgbmFtZTogc3RyaW5nKTogYW55IHtcbiAgICByZXR1cm4gZW51bUlkZW50aWZpZXJbbmFtZV07XG4gIH1cbn1cblxuZnVuY3Rpb24gY29udmVydFRzaWNrbGVEZWNvcmF0b3JJbnRvTWV0YWRhdGEoZGVjb3JhdG9ySW52b2NhdGlvbnM6IGFueVtdKTogYW55W10ge1xuICBpZiAoIWRlY29yYXRvckludm9jYXRpb25zKSB7XG4gICAgcmV0dXJuIFtdO1xuICB9XG4gIHJldHVybiBkZWNvcmF0b3JJbnZvY2F0aW9ucy5tYXAoZGVjb3JhdG9ySW52b2NhdGlvbiA9PiB7XG4gICAgY29uc3QgZGVjb3JhdG9yVHlwZSA9IGRlY29yYXRvckludm9jYXRpb24udHlwZTtcbiAgICBjb25zdCBhbm5vdGF0aW9uQ2xzID0gZGVjb3JhdG9yVHlwZS5hbm5vdGF0aW9uQ2xzO1xuICAgIGNvbnN0IGFubm90YXRpb25BcmdzID0gZGVjb3JhdG9ySW52b2NhdGlvbi5hcmdzID8gZGVjb3JhdG9ySW52b2NhdGlvbi5hcmdzIDogW107XG4gICAgcmV0dXJuIG5ldyBhbm5vdGF0aW9uQ2xzKC4uLmFubm90YXRpb25BcmdzKTtcbiAgfSk7XG59XG5cbmZ1bmN0aW9uIGdldFBhcmVudEN0b3IoY3RvcjogRnVuY3Rpb24pOiBUeXBlPGFueT4ge1xuICBjb25zdCBwYXJlbnRQcm90byA9IGN0b3IucHJvdG90eXBlID8gT2JqZWN0LmdldFByb3RvdHlwZU9mKGN0b3IucHJvdG90eXBlKSA6IG51bGw7XG4gIGNvbnN0IHBhcmVudEN0b3IgPSBwYXJlbnRQcm90byA/IHBhcmVudFByb3RvLmNvbnN0cnVjdG9yIDogbnVsbDtcbiAgLy8gTm90ZTogV2UgYWx3YXlzIHVzZSBgT2JqZWN0YCBhcyB0aGUgbnVsbCB2YWx1ZVxuICAvLyB0byBzaW1wbGlmeSBjaGVja2luZyBsYXRlciBvbi5cbiAgcmV0dXJuIHBhcmVudEN0b3IgfHwgT2JqZWN0O1xufVxuIl19