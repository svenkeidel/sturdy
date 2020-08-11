/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { HarnessPredicate } from '@angular/cdk/testing';
import { MatFormFieldControlHarness } from '@angular/material/form-field/testing/control';
/** Harness for interacting with a standard Material inputs in tests. */
let MatInputHarness = /** @class */ (() => {
    class MatInputHarness extends MatFormFieldControlHarness {
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatInputHarness` that meets
         * certain criteria.
         * @param options Options for filtering which input instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatInputHarness, options)
                .addOption('value', options.value, (harness, value) => __awaiter(this, void 0, void 0, function* () {
                return (yield harness.getValue()) === value;
            }))
                .addOption('placeholder', options.placeholder, (harness, placeholder) => __awaiter(this, void 0, void 0, function* () {
                return (yield harness.getPlaceholder()) === placeholder;
            }));
        }
        /** Whether the input is disabled. */
        isDisabled() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getProperty('disabled');
            });
        }
        /** Whether the input is required. */
        isRequired() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getProperty('required');
            });
        }
        /** Whether the input is readonly. */
        isReadonly() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).getProperty('readOnly');
            });
        }
        /** Gets the value of the input. */
        getValue() {
            return __awaiter(this, void 0, void 0, function* () {
                // The "value" property of the native input is never undefined.
                return (yield (yield this.host()).getProperty('value'));
            });
        }
        /** Gets the name of the input. */
        getName() {
            return __awaiter(this, void 0, void 0, function* () {
                // The "name" property of the native input is never undefined.
                return (yield (yield this.host()).getProperty('name'));
            });
        }
        /**
         * Gets the type of the input. Returns "textarea" if the input is
         * a textarea.
         */
        getType() {
            return __awaiter(this, void 0, void 0, function* () {
                // The "type" property of the native input is never undefined.
                return (yield (yield this.host()).getProperty('type'));
            });
        }
        /** Gets the placeholder of the input. */
        getPlaceholder() {
            return __awaiter(this, void 0, void 0, function* () {
                // The "placeholder" property of the native input is never undefined.
                return (yield (yield this.host()).getProperty('placeholder'));
            });
        }
        /** Gets the id of the input. */
        getId() {
            return __awaiter(this, void 0, void 0, function* () {
                // The input directive always assigns a unique id to the input in
                // case no id has been explicitly specified.
                return (yield (yield this.host()).getProperty('id'));
            });
        }
        /**
         * Focuses the input and returns a promise that indicates when the
         * action is complete.
         */
        focus() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).focus();
            });
        }
        /**
         * Blurs the input and returns a promise that indicates when the
         * action is complete.
         */
        blur() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this.host()).blur();
            });
        }
        /**
         * Sets the value of the input. The value will be set by simulating
         * keypresses that correspond to the given value.
         */
        setValue(newValue) {
            return __awaiter(this, void 0, void 0, function* () {
                const inputEl = yield this.host();
                yield inputEl.clear();
                // We don't want to send keys for the value if the value is an empty
                // string in order to clear the value. Sending keys with an empty string
                // still results in unnecessary focus events.
                if (newValue) {
                    yield inputEl.sendKeys(newValue);
                }
                // Some input types won't respond to key presses (e.g. `color`) so to be sure that the
                // value is set, we also set the property after the keyboard sequence. Note that we don't
                // want to do it before, because it can cause the value to be entered twice.
                // @breaking-change 11.0.0 Remove non-null assertion once `setInputValue` is required.
                if (inputEl.setInputValue) {
                    yield inputEl.setInputValue(newValue);
                }
            });
        }
    }
    // TODO: We do not want to handle `select` elements with `matNativeControl` because
    // not all methods of this harness work reasonably for native select elements.
    // For more details. See: https://github.com/angular/components/pull/18221.
    MatInputHarness.hostSelector = '[matInput], input[matNativeControl], textarea[matNativeControl]';
    return MatInputHarness;
})();
export { MatInputHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5wdXQtaGFybmVzcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9pbnB1dC90ZXN0aW5nL2lucHV0LWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBQ3RELE9BQU8sRUFBQywwQkFBMEIsRUFBQyxNQUFNLDhDQUE4QyxDQUFDO0FBR3hGLHdFQUF3RTtBQUN4RTtJQUFBLE1BQWEsZUFBZ0IsU0FBUSwwQkFBMEI7UUFNN0Q7Ozs7O1dBS0c7UUFDSCxNQUFNLENBQUMsSUFBSSxDQUFDLFVBQStCLEVBQUU7WUFDM0MsT0FBTyxJQUFJLGdCQUFnQixDQUFDLGVBQWUsRUFBRSxPQUFPLENBQUM7aUJBQ2hELFNBQVMsQ0FBQyxPQUFPLEVBQUUsT0FBTyxDQUFDLEtBQUssRUFBRSxDQUFPLE9BQU8sRUFBRSxLQUFLLEVBQUUsRUFBRTtnQkFDMUQsT0FBTyxDQUFDLE1BQU0sT0FBTyxDQUFDLFFBQVEsRUFBRSxDQUFDLEtBQUssS0FBSyxDQUFDO1lBQzlDLENBQUMsQ0FBQSxDQUFDO2lCQUNELFNBQVMsQ0FBQyxhQUFhLEVBQUUsT0FBTyxDQUFDLFdBQVcsRUFBRSxDQUFPLE9BQU8sRUFBRSxXQUFXLEVBQUUsRUFBRTtnQkFDNUUsT0FBTyxDQUFDLE1BQU0sT0FBTyxDQUFDLGNBQWMsRUFBRSxDQUFDLEtBQUssV0FBVyxDQUFDO1lBQzFELENBQUMsQ0FBQSxDQUFDLENBQUM7UUFDVCxDQUFDO1FBRUQscUNBQXFDO1FBQy9CLFVBQVU7O2dCQUNkLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFdBQVcsQ0FBQyxVQUFVLENBQUUsQ0FBQztZQUN0RCxDQUFDO1NBQUE7UUFFRCxxQ0FBcUM7UUFDL0IsVUFBVTs7Z0JBQ2QsT0FBTyxDQUFDLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDLENBQUMsV0FBVyxDQUFDLFVBQVUsQ0FBRSxDQUFDO1lBQ3RELENBQUM7U0FBQTtRQUVELHFDQUFxQztRQUMvQixVQUFVOztnQkFDZCxPQUFPLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxXQUFXLENBQUMsVUFBVSxDQUFFLENBQUM7WUFDdEQsQ0FBQztTQUFBO1FBRUQsbUNBQW1DO1FBQzdCLFFBQVE7O2dCQUNaLCtEQUErRDtnQkFDL0QsT0FBTyxDQUFDLE1BQU0sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFdBQVcsQ0FBQyxPQUFPLENBQUMsQ0FBRSxDQUFDO1lBQzNELENBQUM7U0FBQTtRQUVELGtDQUFrQztRQUM1QixPQUFPOztnQkFDWCw4REFBOEQ7Z0JBQzlELE9BQU8sQ0FBQyxNQUFNLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxXQUFXLENBQUMsTUFBTSxDQUFDLENBQUUsQ0FBQztZQUMxRCxDQUFDO1NBQUE7UUFFRDs7O1dBR0c7UUFDRyxPQUFPOztnQkFDWCw4REFBOEQ7Z0JBQzlELE9BQU8sQ0FBQyxNQUFNLENBQUMsTUFBTSxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQyxXQUFXLENBQUMsTUFBTSxDQUFDLENBQUUsQ0FBQztZQUMxRCxDQUFDO1NBQUE7UUFFRCx5Q0FBeUM7UUFDbkMsY0FBYzs7Z0JBQ2xCLHFFQUFxRTtnQkFDckUsT0FBTyxDQUFDLE1BQU0sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFdBQVcsQ0FBQyxhQUFhLENBQUMsQ0FBRSxDQUFDO1lBQ2pFLENBQUM7U0FBQTtRQUVELGdDQUFnQztRQUMxQixLQUFLOztnQkFDVCxpRUFBaUU7Z0JBQ2pFLDRDQUE0QztnQkFDNUMsT0FBTyxDQUFDLE1BQU0sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFdBQVcsQ0FBQyxJQUFJLENBQUMsQ0FBRSxDQUFDO1lBQ3hELENBQUM7U0FBQTtRQUVEOzs7V0FHRztRQUNHLEtBQUs7O2dCQUNULE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLEtBQUssRUFBRSxDQUFDO1lBQ3JDLENBQUM7U0FBQTtRQUVEOzs7V0FHRztRQUNHLElBQUk7O2dCQUNSLE9BQU8sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLElBQUksRUFBRSxDQUFDO1lBQ3BDLENBQUM7U0FBQTtRQUVEOzs7V0FHRztRQUNHLFFBQVEsQ0FBQyxRQUFnQjs7Z0JBQzdCLE1BQU0sT0FBTyxHQUFHLE1BQU0sSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDO2dCQUNsQyxNQUFNLE9BQU8sQ0FBQyxLQUFLLEVBQUUsQ0FBQztnQkFDdEIsb0VBQW9FO2dCQUNwRSx3RUFBd0U7Z0JBQ3hFLDZDQUE2QztnQkFDN0MsSUFBSSxRQUFRLEVBQUU7b0JBQ1osTUFBTSxPQUFPLENBQUMsUUFBUSxDQUFDLFFBQVEsQ0FBQyxDQUFDO2lCQUNsQztnQkFFRCxzRkFBc0Y7Z0JBQ3RGLHlGQUF5RjtnQkFDekYsNEVBQTRFO2dCQUM1RSxzRkFBc0Y7Z0JBQ3RGLElBQUksT0FBTyxDQUFDLGFBQWEsRUFBRTtvQkFDekIsTUFBTSxPQUFPLENBQUMsYUFBYSxDQUFDLFFBQVEsQ0FBQyxDQUFDO2lCQUN2QztZQUNILENBQUM7U0FBQTs7SUEzR0QsbUZBQW1GO0lBQ25GLDhFQUE4RTtJQUM5RSwyRUFBMkU7SUFDcEUsNEJBQVksR0FBRyxpRUFBaUUsQ0FBQztJQXlHMUYsc0JBQUM7S0FBQTtTQTdHWSxlQUFlIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7SGFybmVzc1ByZWRpY2F0ZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3Rlc3RpbmcnO1xuaW1wb3J0IHtNYXRGb3JtRmllbGRDb250cm9sSGFybmVzc30gZnJvbSAnQGFuZ3VsYXIvbWF0ZXJpYWwvZm9ybS1maWVsZC90ZXN0aW5nL2NvbnRyb2wnO1xuaW1wb3J0IHtJbnB1dEhhcm5lc3NGaWx0ZXJzfSBmcm9tICcuL2lucHV0LWhhcm5lc3MtZmlsdGVycyc7XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgTWF0ZXJpYWwgaW5wdXRzIGluIHRlc3RzLiAqL1xuZXhwb3J0IGNsYXNzIE1hdElucHV0SGFybmVzcyBleHRlbmRzIE1hdEZvcm1GaWVsZENvbnRyb2xIYXJuZXNzIHtcbiAgLy8gVE9ETzogV2UgZG8gbm90IHdhbnQgdG8gaGFuZGxlIGBzZWxlY3RgIGVsZW1lbnRzIHdpdGggYG1hdE5hdGl2ZUNvbnRyb2xgIGJlY2F1c2VcbiAgLy8gbm90IGFsbCBtZXRob2RzIG9mIHRoaXMgaGFybmVzcyB3b3JrIHJlYXNvbmFibHkgZm9yIG5hdGl2ZSBzZWxlY3QgZWxlbWVudHMuXG4gIC8vIEZvciBtb3JlIGRldGFpbHMuIFNlZTogaHR0cHM6Ly9naXRodWIuY29tL2FuZ3VsYXIvY29tcG9uZW50cy9wdWxsLzE4MjIxLlxuICBzdGF0aWMgaG9zdFNlbGVjdG9yID0gJ1ttYXRJbnB1dF0sIGlucHV0W21hdE5hdGl2ZUNvbnRyb2xdLCB0ZXh0YXJlYVttYXROYXRpdmVDb250cm9sXSc7XG5cbiAgLyoqXG4gICAqIEdldHMgYSBgSGFybmVzc1ByZWRpY2F0ZWAgdGhhdCBjYW4gYmUgdXNlZCB0byBzZWFyY2ggZm9yIGEgYE1hdElucHV0SGFybmVzc2AgdGhhdCBtZWV0c1xuICAgKiBjZXJ0YWluIGNyaXRlcmlhLlxuICAgKiBAcGFyYW0gb3B0aW9ucyBPcHRpb25zIGZvciBmaWx0ZXJpbmcgd2hpY2ggaW5wdXQgaW5zdGFuY2VzIGFyZSBjb25zaWRlcmVkIGEgbWF0Y2guXG4gICAqIEByZXR1cm4gYSBgSGFybmVzc1ByZWRpY2F0ZWAgY29uZmlndXJlZCB3aXRoIHRoZSBnaXZlbiBvcHRpb25zLlxuICAgKi9cbiAgc3RhdGljIHdpdGgob3B0aW9uczogSW5wdXRIYXJuZXNzRmlsdGVycyA9IHt9KTogSGFybmVzc1ByZWRpY2F0ZTxNYXRJbnB1dEhhcm5lc3M+IHtcbiAgICByZXR1cm4gbmV3IEhhcm5lc3NQcmVkaWNhdGUoTWF0SW5wdXRIYXJuZXNzLCBvcHRpb25zKVxuICAgICAgICAuYWRkT3B0aW9uKCd2YWx1ZScsIG9wdGlvbnMudmFsdWUsIGFzeW5jIChoYXJuZXNzLCB2YWx1ZSkgPT4ge1xuICAgICAgICAgIHJldHVybiAoYXdhaXQgaGFybmVzcy5nZXRWYWx1ZSgpKSA9PT0gdmFsdWU7XG4gICAgICAgIH0pXG4gICAgICAgIC5hZGRPcHRpb24oJ3BsYWNlaG9sZGVyJywgb3B0aW9ucy5wbGFjZWhvbGRlciwgYXN5bmMgKGhhcm5lc3MsIHBsYWNlaG9sZGVyKSA9PiB7XG4gICAgICAgICAgcmV0dXJuIChhd2FpdCBoYXJuZXNzLmdldFBsYWNlaG9sZGVyKCkpID09PSBwbGFjZWhvbGRlcjtcbiAgICAgICAgfSk7XG4gIH1cblxuICAvKiogV2hldGhlciB0aGUgaW5wdXQgaXMgZGlzYWJsZWQuICovXG4gIGFzeW5jIGlzRGlzYWJsZWQoKTogUHJvbWlzZTxib29sZWFuPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuZ2V0UHJvcGVydHkoJ2Rpc2FibGVkJykhO1xuICB9XG5cbiAgLyoqIFdoZXRoZXIgdGhlIGlucHV0IGlzIHJlcXVpcmVkLiAqL1xuICBhc3luYyBpc1JlcXVpcmVkKCk6IFByb21pc2U8Ym9vbGVhbj4ge1xuICAgIHJldHVybiAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldFByb3BlcnR5KCdyZXF1aXJlZCcpITtcbiAgfVxuXG4gIC8qKiBXaGV0aGVyIHRoZSBpbnB1dCBpcyByZWFkb25seS4gKi9cbiAgYXN5bmMgaXNSZWFkb25seSgpOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgICByZXR1cm4gKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRQcm9wZXJ0eSgncmVhZE9ubHknKSE7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgdmFsdWUgb2YgdGhlIGlucHV0LiAqL1xuICBhc3luYyBnZXRWYWx1ZSgpOiBQcm9taXNlPHN0cmluZz4ge1xuICAgIC8vIFRoZSBcInZhbHVlXCIgcHJvcGVydHkgb2YgdGhlIG5hdGl2ZSBpbnB1dCBpcyBuZXZlciB1bmRlZmluZWQuXG4gICAgcmV0dXJuIChhd2FpdCAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldFByb3BlcnR5KCd2YWx1ZScpKSE7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgbmFtZSBvZiB0aGUgaW5wdXQuICovXG4gIGFzeW5jIGdldE5hbWUoKTogUHJvbWlzZTxzdHJpbmc+IHtcbiAgICAvLyBUaGUgXCJuYW1lXCIgcHJvcGVydHkgb2YgdGhlIG5hdGl2ZSBpbnB1dCBpcyBuZXZlciB1bmRlZmluZWQuXG4gICAgcmV0dXJuIChhd2FpdCAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldFByb3BlcnR5KCduYW1lJykpITtcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXRzIHRoZSB0eXBlIG9mIHRoZSBpbnB1dC4gUmV0dXJucyBcInRleHRhcmVhXCIgaWYgdGhlIGlucHV0IGlzXG4gICAqIGEgdGV4dGFyZWEuXG4gICAqL1xuICBhc3luYyBnZXRUeXBlKCk6IFByb21pc2U8c3RyaW5nPiB7XG4gICAgLy8gVGhlIFwidHlwZVwiIHByb3BlcnR5IG9mIHRoZSBuYXRpdmUgaW5wdXQgaXMgbmV2ZXIgdW5kZWZpbmVkLlxuICAgIHJldHVybiAoYXdhaXQgKGF3YWl0IHRoaXMuaG9zdCgpKS5nZXRQcm9wZXJ0eSgndHlwZScpKSE7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgcGxhY2Vob2xkZXIgb2YgdGhlIGlucHV0LiAqL1xuICBhc3luYyBnZXRQbGFjZWhvbGRlcigpOiBQcm9taXNlPHN0cmluZz4ge1xuICAgIC8vIFRoZSBcInBsYWNlaG9sZGVyXCIgcHJvcGVydHkgb2YgdGhlIG5hdGl2ZSBpbnB1dCBpcyBuZXZlciB1bmRlZmluZWQuXG4gICAgcmV0dXJuIChhd2FpdCAoYXdhaXQgdGhpcy5ob3N0KCkpLmdldFByb3BlcnR5KCdwbGFjZWhvbGRlcicpKSE7XG4gIH1cblxuICAvKiogR2V0cyB0aGUgaWQgb2YgdGhlIGlucHV0LiAqL1xuICBhc3luYyBnZXRJZCgpOiBQcm9taXNlPHN0cmluZz4ge1xuICAgIC8vIFRoZSBpbnB1dCBkaXJlY3RpdmUgYWx3YXlzIGFzc2lnbnMgYSB1bmlxdWUgaWQgdG8gdGhlIGlucHV0IGluXG4gICAgLy8gY2FzZSBubyBpZCBoYXMgYmVlbiBleHBsaWNpdGx5IHNwZWNpZmllZC5cbiAgICByZXR1cm4gKGF3YWl0IChhd2FpdCB0aGlzLmhvc3QoKSkuZ2V0UHJvcGVydHkoJ2lkJykpITtcbiAgfVxuXG4gIC8qKlxuICAgKiBGb2N1c2VzIHRoZSBpbnB1dCBhbmQgcmV0dXJucyBhIHByb21pc2UgdGhhdCBpbmRpY2F0ZXMgd2hlbiB0aGVcbiAgICogYWN0aW9uIGlzIGNvbXBsZXRlLlxuICAgKi9cbiAgYXN5bmMgZm9jdXMoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuZm9jdXMoKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBCbHVycyB0aGUgaW5wdXQgYW5kIHJldHVybnMgYSBwcm9taXNlIHRoYXQgaW5kaWNhdGVzIHdoZW4gdGhlXG4gICAqIGFjdGlvbiBpcyBjb21wbGV0ZS5cbiAgICovXG4gIGFzeW5jIGJsdXIoKTogUHJvbWlzZTx2b2lkPiB7XG4gICAgcmV0dXJuIChhd2FpdCB0aGlzLmhvc3QoKSkuYmx1cigpO1xuICB9XG5cbiAgLyoqXG4gICAqIFNldHMgdGhlIHZhbHVlIG9mIHRoZSBpbnB1dC4gVGhlIHZhbHVlIHdpbGwgYmUgc2V0IGJ5IHNpbXVsYXRpbmdcbiAgICoga2V5cHJlc3NlcyB0aGF0IGNvcnJlc3BvbmQgdG8gdGhlIGdpdmVuIHZhbHVlLlxuICAgKi9cbiAgYXN5bmMgc2V0VmFsdWUobmV3VmFsdWU6IHN0cmluZyk6IFByb21pc2U8dm9pZD4ge1xuICAgIGNvbnN0IGlucHV0RWwgPSBhd2FpdCB0aGlzLmhvc3QoKTtcbiAgICBhd2FpdCBpbnB1dEVsLmNsZWFyKCk7XG4gICAgLy8gV2UgZG9uJ3Qgd2FudCB0byBzZW5kIGtleXMgZm9yIHRoZSB2YWx1ZSBpZiB0aGUgdmFsdWUgaXMgYW4gZW1wdHlcbiAgICAvLyBzdHJpbmcgaW4gb3JkZXIgdG8gY2xlYXIgdGhlIHZhbHVlLiBTZW5kaW5nIGtleXMgd2l0aCBhbiBlbXB0eSBzdHJpbmdcbiAgICAvLyBzdGlsbCByZXN1bHRzIGluIHVubmVjZXNzYXJ5IGZvY3VzIGV2ZW50cy5cbiAgICBpZiAobmV3VmFsdWUpIHtcbiAgICAgIGF3YWl0IGlucHV0RWwuc2VuZEtleXMobmV3VmFsdWUpO1xuICAgIH1cblxuICAgIC8vIFNvbWUgaW5wdXQgdHlwZXMgd29uJ3QgcmVzcG9uZCB0byBrZXkgcHJlc3NlcyAoZS5nLiBgY29sb3JgKSBzbyB0byBiZSBzdXJlIHRoYXQgdGhlXG4gICAgLy8gdmFsdWUgaXMgc2V0LCB3ZSBhbHNvIHNldCB0aGUgcHJvcGVydHkgYWZ0ZXIgdGhlIGtleWJvYXJkIHNlcXVlbmNlLiBOb3RlIHRoYXQgd2UgZG9uJ3RcbiAgICAvLyB3YW50IHRvIGRvIGl0IGJlZm9yZSwgYmVjYXVzZSBpdCBjYW4gY2F1c2UgdGhlIHZhbHVlIHRvIGJlIGVudGVyZWQgdHdpY2UuXG4gICAgLy8gQGJyZWFraW5nLWNoYW5nZSAxMS4wLjAgUmVtb3ZlIG5vbi1udWxsIGFzc2VydGlvbiBvbmNlIGBzZXRJbnB1dFZhbHVlYCBpcyByZXF1aXJlZC5cbiAgICBpZiAoaW5wdXRFbC5zZXRJbnB1dFZhbHVlKSB7XG4gICAgICBhd2FpdCBpbnB1dEVsLnNldElucHV0VmFsdWUobmV3VmFsdWUpO1xuICAgIH1cbiAgfVxufVxuIl19