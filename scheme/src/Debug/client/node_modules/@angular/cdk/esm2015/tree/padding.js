/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Directionality } from '@angular/cdk/bidi';
import { coerceNumberProperty } from '@angular/cdk/coercion';
import { Directive, ElementRef, Input, Optional, Renderer2 } from '@angular/core';
import { takeUntil } from 'rxjs/operators';
import { Subject } from 'rxjs';
import { CdkTree, CdkTreeNode } from './tree';
/** Regex used to split a string on its CSS units. */
const cssUnitPattern = /([A-Za-z%]+)$/;
/**
 * Indent for the children tree dataNodes.
 * This directive will add left-padding to the node to show hierarchy.
 */
let CdkTreeNodePadding = /** @class */ (() => {
    class CdkTreeNodePadding {
        constructor(_treeNode, _tree, 
        /**
         * @deprecated _renderer parameter no longer being used. To be removed.
         * @breaking-change 11.0.0
         */
        _renderer, _element, _dir) {
            this._treeNode = _treeNode;
            this._tree = _tree;
            this._element = _element;
            this._dir = _dir;
            /** Subject that emits when the component has been destroyed. */
            this._destroyed = new Subject();
            /** CSS units used for the indentation value. */
            this.indentUnits = 'px';
            this._indent = 40;
            this._setPadding();
            if (_dir) {
                _dir.change.pipe(takeUntil(this._destroyed)).subscribe(() => this._setPadding(true));
            }
            // In Ivy the indentation binding might be set before the tree node's data has been added,
            // which means that we'll miss the first render. We have to subscribe to changes in the
            // data to ensure that everything is up to date.
            _treeNode._dataChanges.subscribe(() => this._setPadding());
        }
        /** The level of depth of the tree node. The padding will be `level * indent` pixels. */
        get level() { return this._level; }
        set level(value) {
            // Set to null as the fallback value so that _setPadding can fall back to the node level if the
            // consumer set the directive as `cdkTreeNodePadding=""`. We still want to take this value if
            // they set 0 explicitly.
            this._level = coerceNumberProperty(value, null);
            this._setPadding();
        }
        /**
         * The indent for each level. Can be a number or a CSS string.
         * Default number 40px from material design menu sub-menu spec.
         */
        get indent() { return this._indent; }
        set indent(indent) {
            let value = indent;
            let units = 'px';
            if (typeof indent === 'string') {
                const parts = indent.split(cssUnitPattern);
                value = parts[0];
                units = parts[1] || units;
            }
            this.indentUnits = units;
            this._indent = coerceNumberProperty(value);
            this._setPadding();
        }
        ngOnDestroy() {
            this._destroyed.next();
            this._destroyed.complete();
        }
        /** The padding indent value for the tree node. Returns a string with px numbers if not null. */
        _paddingIndent() {
            const nodeLevel = (this._treeNode.data && this._tree.treeControl.getLevel)
                ? this._tree.treeControl.getLevel(this._treeNode.data)
                : null;
            const level = this._level == null ? nodeLevel : this._level;
            return typeof level === 'number' ? `${level * this._indent}${this.indentUnits}` : null;
        }
        _setPadding(forceChange = false) {
            const padding = this._paddingIndent();
            if (padding !== this._currentPadding || forceChange) {
                const element = this._element.nativeElement;
                const paddingProp = this._dir && this._dir.value === 'rtl' ? 'paddingRight' : 'paddingLeft';
                const resetProp = paddingProp === 'paddingLeft' ? 'paddingRight' : 'paddingLeft';
                element.style[paddingProp] = padding || '';
                element.style[resetProp] = '';
                this._currentPadding = padding;
            }
        }
    }
    CdkTreeNodePadding.decorators = [
        { type: Directive, args: [{
                    selector: '[cdkTreeNodePadding]',
                },] }
    ];
    CdkTreeNodePadding.ctorParameters = () => [
        { type: CdkTreeNode },
        { type: CdkTree },
        { type: Renderer2 },
        { type: ElementRef },
        { type: Directionality, decorators: [{ type: Optional }] }
    ];
    CdkTreeNodePadding.propDecorators = {
        level: [{ type: Input, args: ['cdkTreeNodePadding',] }],
        indent: [{ type: Input, args: ['cdkTreeNodePaddingIndent',] }]
    };
    return CdkTreeNodePadding;
})();
export { CdkTreeNodePadding };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicGFkZGluZy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3NyYy9jZGsvdHJlZS9wYWRkaW5nLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU8sRUFBQyxjQUFjLEVBQUMsTUFBTSxtQkFBbUIsQ0FBQztBQUNqRCxPQUFPLEVBQUMsb0JBQW9CLEVBQWMsTUFBTSx1QkFBdUIsQ0FBQztBQUN4RSxPQUFPLEVBQUMsU0FBUyxFQUFFLFVBQVUsRUFBRSxLQUFLLEVBQWEsUUFBUSxFQUFFLFNBQVMsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUMzRixPQUFPLEVBQUMsU0FBUyxFQUFDLE1BQU0sZ0JBQWdCLENBQUM7QUFDekMsT0FBTyxFQUFDLE9BQU8sRUFBQyxNQUFNLE1BQU0sQ0FBQztBQUM3QixPQUFPLEVBQUMsT0FBTyxFQUFFLFdBQVcsRUFBQyxNQUFNLFFBQVEsQ0FBQztBQUU1QyxxREFBcUQ7QUFDckQsTUFBTSxjQUFjLEdBQUcsZUFBZSxDQUFDO0FBRXZDOzs7R0FHRztBQUNIO0lBQUEsTUFHYSxrQkFBa0I7UUE0QzdCLFlBQW9CLFNBQXlCLEVBQ3pCLEtBQWlCO1FBQ3pCOzs7V0FHRztRQUNILFNBQW9CLEVBQ1osUUFBaUMsRUFDckIsSUFBb0I7WUFSaEMsY0FBUyxHQUFULFNBQVMsQ0FBZ0I7WUFDekIsVUFBSyxHQUFMLEtBQUssQ0FBWTtZQU1qQixhQUFRLEdBQVIsUUFBUSxDQUF5QjtZQUNyQixTQUFJLEdBQUosSUFBSSxDQUFnQjtZQWhEcEQsZ0VBQWdFO1lBQ3hELGVBQVUsR0FBRyxJQUFJLE9BQU8sRUFBUSxDQUFDO1lBRXpDLGdEQUFnRDtZQUNoRCxnQkFBVyxHQUFHLElBQUksQ0FBQztZQWtDbkIsWUFBTyxHQUFXLEVBQUUsQ0FBQztZQVduQixJQUFJLENBQUMsV0FBVyxFQUFFLENBQUM7WUFDbkIsSUFBSSxJQUFJLEVBQUU7Z0JBQ1IsSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsU0FBUyxDQUFDLElBQUksQ0FBQyxVQUFVLENBQUMsQ0FBQyxDQUFDLFNBQVMsQ0FBQyxHQUFHLEVBQUUsQ0FBQyxJQUFJLENBQUMsV0FBVyxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUM7YUFDdEY7WUFFRCwwRkFBMEY7WUFDMUYsdUZBQXVGO1lBQ3ZGLGdEQUFnRDtZQUNoRCxTQUFTLENBQUMsWUFBWSxDQUFDLFNBQVMsQ0FBQyxHQUFHLEVBQUUsQ0FBQyxJQUFJLENBQUMsV0FBVyxFQUFFLENBQUMsQ0FBQztRQUM3RCxDQUFDO1FBcERELHdGQUF3RjtRQUN4RixJQUNJLEtBQUssS0FBYSxPQUFPLElBQUksQ0FBQyxNQUFNLENBQUMsQ0FBQyxDQUFDO1FBQzNDLElBQUksS0FBSyxDQUFDLEtBQWE7WUFDckIsK0ZBQStGO1lBQy9GLDZGQUE2RjtZQUM3Rix5QkFBeUI7WUFDekIsSUFBSSxDQUFDLE1BQU0sR0FBRyxvQkFBb0IsQ0FBQyxLQUFLLEVBQUUsSUFBSSxDQUFFLENBQUM7WUFDakQsSUFBSSxDQUFDLFdBQVcsRUFBRSxDQUFDO1FBQ3JCLENBQUM7UUFHRDs7O1dBR0c7UUFDSCxJQUNJLE1BQU0sS0FBc0IsT0FBTyxJQUFJLENBQUMsT0FBTyxDQUFDLENBQUMsQ0FBQztRQUN0RCxJQUFJLE1BQU0sQ0FBQyxNQUF1QjtZQUNoQyxJQUFJLEtBQUssR0FBRyxNQUFNLENBQUM7WUFDbkIsSUFBSSxLQUFLLEdBQUcsSUFBSSxDQUFDO1lBRWpCLElBQUksT0FBTyxNQUFNLEtBQUssUUFBUSxFQUFFO2dCQUM5QixNQUFNLEtBQUssR0FBRyxNQUFNLENBQUMsS0FBSyxDQUFDLGNBQWMsQ0FBQyxDQUFDO2dCQUMzQyxLQUFLLEdBQUcsS0FBSyxDQUFDLENBQUMsQ0FBQyxDQUFDO2dCQUNqQixLQUFLLEdBQUcsS0FBSyxDQUFDLENBQUMsQ0FBQyxJQUFJLEtBQUssQ0FBQzthQUMzQjtZQUVELElBQUksQ0FBQyxXQUFXLEdBQUcsS0FBSyxDQUFDO1lBQ3pCLElBQUksQ0FBQyxPQUFPLEdBQUcsb0JBQW9CLENBQUMsS0FBSyxDQUFDLENBQUM7WUFDM0MsSUFBSSxDQUFDLFdBQVcsRUFBRSxDQUFDO1FBQ3JCLENBQUM7UUF1QkQsV0FBVztZQUNULElBQUksQ0FBQyxVQUFVLENBQUMsSUFBSSxFQUFFLENBQUM7WUFDdkIsSUFBSSxDQUFDLFVBQVUsQ0FBQyxRQUFRLEVBQUUsQ0FBQztRQUM3QixDQUFDO1FBRUQsZ0dBQWdHO1FBQ2hHLGNBQWM7WUFDWixNQUFNLFNBQVMsR0FBRyxDQUFDLElBQUksQ0FBQyxTQUFTLENBQUMsSUFBSSxJQUFJLElBQUksQ0FBQyxLQUFLLENBQUMsV0FBVyxDQUFDLFFBQVEsQ0FBQztnQkFDeEUsQ0FBQyxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsV0FBVyxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsU0FBUyxDQUFDLElBQUksQ0FBQztnQkFDdEQsQ0FBQyxDQUFDLElBQUksQ0FBQztZQUNULE1BQU0sS0FBSyxHQUFHLElBQUksQ0FBQyxNQUFNLElBQUksSUFBSSxDQUFDLENBQUMsQ0FBQyxTQUFTLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxNQUFNLENBQUM7WUFDNUQsT0FBTyxPQUFPLEtBQUssS0FBSyxRQUFRLENBQUMsQ0FBQyxDQUFDLEdBQUcsS0FBSyxHQUFHLElBQUksQ0FBQyxPQUFPLEdBQUcsSUFBSSxDQUFDLFdBQVcsRUFBRSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUM7UUFDekYsQ0FBQztRQUVELFdBQVcsQ0FBQyxXQUFXLEdBQUcsS0FBSztZQUM3QixNQUFNLE9BQU8sR0FBRyxJQUFJLENBQUMsY0FBYyxFQUFFLENBQUM7WUFFdEMsSUFBSSxPQUFPLEtBQUssSUFBSSxDQUFDLGVBQWUsSUFBSSxXQUFXLEVBQUU7Z0JBQ25ELE1BQU0sT0FBTyxHQUFHLElBQUksQ0FBQyxRQUFRLENBQUMsYUFBYSxDQUFDO2dCQUM1QyxNQUFNLFdBQVcsR0FBRyxJQUFJLENBQUMsSUFBSSxJQUFJLElBQUksQ0FBQyxJQUFJLENBQUMsS0FBSyxLQUFLLEtBQUssQ0FBQyxDQUFDLENBQUMsY0FBYyxDQUFDLENBQUMsQ0FBQyxhQUFhLENBQUM7Z0JBQzVGLE1BQU0sU0FBUyxHQUFHLFdBQVcsS0FBSyxhQUFhLENBQUMsQ0FBQyxDQUFDLGNBQWMsQ0FBQyxDQUFDLENBQUMsYUFBYSxDQUFDO2dCQUNqRixPQUFPLENBQUMsS0FBSyxDQUFDLFdBQVcsQ0FBQyxHQUFHLE9BQU8sSUFBSSxFQUFFLENBQUM7Z0JBQzNDLE9BQU8sQ0FBQyxLQUFLLENBQUMsU0FBUyxDQUFDLEdBQUcsRUFBRSxDQUFDO2dCQUM5QixJQUFJLENBQUMsZUFBZSxHQUFHLE9BQU8sQ0FBQzthQUNoQztRQUNILENBQUM7OztnQkE1RkYsU0FBUyxTQUFDO29CQUNULFFBQVEsRUFBRSxzQkFBc0I7aUJBQ2pDOzs7Z0JBWGdCLFdBQVc7Z0JBQXBCLE9BQU87Z0JBSDRDLFNBQVM7Z0JBQWpELFVBQVU7Z0JBRnJCLGNBQWMsdUJBcUVQLFFBQVE7Ozt3QkF6Q3BCLEtBQUssU0FBQyxvQkFBb0I7eUJBZTFCLEtBQUssU0FBQywwQkFBMEI7O0lBa0VuQyx5QkFBQztLQUFBO1NBNUZZLGtCQUFrQiIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0RpcmVjdGlvbmFsaXR5fSBmcm9tICdAYW5ndWxhci9jZGsvYmlkaSc7XG5pbXBvcnQge2NvZXJjZU51bWJlclByb3BlcnR5LCBOdW1iZXJJbnB1dH0gZnJvbSAnQGFuZ3VsYXIvY2RrL2NvZXJjaW9uJztcbmltcG9ydCB7RGlyZWN0aXZlLCBFbGVtZW50UmVmLCBJbnB1dCwgT25EZXN0cm95LCBPcHRpb25hbCwgUmVuZGVyZXIyfSBmcm9tICdAYW5ndWxhci9jb3JlJztcbmltcG9ydCB7dGFrZVVudGlsfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5pbXBvcnQge1N1YmplY3R9IGZyb20gJ3J4anMnO1xuaW1wb3J0IHtDZGtUcmVlLCBDZGtUcmVlTm9kZX0gZnJvbSAnLi90cmVlJztcblxuLyoqIFJlZ2V4IHVzZWQgdG8gc3BsaXQgYSBzdHJpbmcgb24gaXRzIENTUyB1bml0cy4gKi9cbmNvbnN0IGNzc1VuaXRQYXR0ZXJuID0gLyhbQS1aYS16JV0rKSQvO1xuXG4vKipcbiAqIEluZGVudCBmb3IgdGhlIGNoaWxkcmVuIHRyZWUgZGF0YU5vZGVzLlxuICogVGhpcyBkaXJlY3RpdmUgd2lsbCBhZGQgbGVmdC1wYWRkaW5nIHRvIHRoZSBub2RlIHRvIHNob3cgaGllcmFyY2h5LlxuICovXG5ARGlyZWN0aXZlKHtcbiAgc2VsZWN0b3I6ICdbY2RrVHJlZU5vZGVQYWRkaW5nXScsXG59KVxuZXhwb3J0IGNsYXNzIENka1RyZWVOb2RlUGFkZGluZzxUPiBpbXBsZW1lbnRzIE9uRGVzdHJveSB7XG4gIC8qKiBDdXJyZW50IHBhZGRpbmcgdmFsdWUgYXBwbGllZCB0byB0aGUgZWxlbWVudC4gVXNlZCB0byBhdm9pZCB1bm5lY2Vzc2FyaWx5IGhpdHRpbmcgdGhlIERPTS4gKi9cbiAgcHJpdmF0ZSBfY3VycmVudFBhZGRpbmc6IHN0cmluZ3xudWxsO1xuXG4gIC8qKiBTdWJqZWN0IHRoYXQgZW1pdHMgd2hlbiB0aGUgY29tcG9uZW50IGhhcyBiZWVuIGRlc3Ryb3llZC4gKi9cbiAgcHJpdmF0ZSBfZGVzdHJveWVkID0gbmV3IFN1YmplY3Q8dm9pZD4oKTtcblxuICAvKiogQ1NTIHVuaXRzIHVzZWQgZm9yIHRoZSBpbmRlbnRhdGlvbiB2YWx1ZS4gKi9cbiAgaW5kZW50VW5pdHMgPSAncHgnO1xuXG4gIC8qKiBUaGUgbGV2ZWwgb2YgZGVwdGggb2YgdGhlIHRyZWUgbm9kZS4gVGhlIHBhZGRpbmcgd2lsbCBiZSBgbGV2ZWwgKiBpbmRlbnRgIHBpeGVscy4gKi9cbiAgQElucHV0KCdjZGtUcmVlTm9kZVBhZGRpbmcnKVxuICBnZXQgbGV2ZWwoKTogbnVtYmVyIHsgcmV0dXJuIHRoaXMuX2xldmVsOyB9XG4gIHNldCBsZXZlbCh2YWx1ZTogbnVtYmVyKSB7XG4gICAgLy8gU2V0IHRvIG51bGwgYXMgdGhlIGZhbGxiYWNrIHZhbHVlIHNvIHRoYXQgX3NldFBhZGRpbmcgY2FuIGZhbGwgYmFjayB0byB0aGUgbm9kZSBsZXZlbCBpZiB0aGVcbiAgICAvLyBjb25zdW1lciBzZXQgdGhlIGRpcmVjdGl2ZSBhcyBgY2RrVHJlZU5vZGVQYWRkaW5nPVwiXCJgLiBXZSBzdGlsbCB3YW50IHRvIHRha2UgdGhpcyB2YWx1ZSBpZlxuICAgIC8vIHRoZXkgc2V0IDAgZXhwbGljaXRseS5cbiAgICB0aGlzLl9sZXZlbCA9IGNvZXJjZU51bWJlclByb3BlcnR5KHZhbHVlLCBudWxsKSE7XG4gICAgdGhpcy5fc2V0UGFkZGluZygpO1xuICB9XG4gIF9sZXZlbDogbnVtYmVyO1xuXG4gIC8qKlxuICAgKiBUaGUgaW5kZW50IGZvciBlYWNoIGxldmVsLiBDYW4gYmUgYSBudW1iZXIgb3IgYSBDU1Mgc3RyaW5nLlxuICAgKiBEZWZhdWx0IG51bWJlciA0MHB4IGZyb20gbWF0ZXJpYWwgZGVzaWduIG1lbnUgc3ViLW1lbnUgc3BlYy5cbiAgICovXG4gIEBJbnB1dCgnY2RrVHJlZU5vZGVQYWRkaW5nSW5kZW50JylcbiAgZ2V0IGluZGVudCgpOiBudW1iZXIgfCBzdHJpbmcgeyByZXR1cm4gdGhpcy5faW5kZW50OyB9XG4gIHNldCBpbmRlbnQoaW5kZW50OiBudW1iZXIgfCBzdHJpbmcpIHtcbiAgICBsZXQgdmFsdWUgPSBpbmRlbnQ7XG4gICAgbGV0IHVuaXRzID0gJ3B4JztcblxuICAgIGlmICh0eXBlb2YgaW5kZW50ID09PSAnc3RyaW5nJykge1xuICAgICAgY29uc3QgcGFydHMgPSBpbmRlbnQuc3BsaXQoY3NzVW5pdFBhdHRlcm4pO1xuICAgICAgdmFsdWUgPSBwYXJ0c1swXTtcbiAgICAgIHVuaXRzID0gcGFydHNbMV0gfHwgdW5pdHM7XG4gICAgfVxuXG4gICAgdGhpcy5pbmRlbnRVbml0cyA9IHVuaXRzO1xuICAgIHRoaXMuX2luZGVudCA9IGNvZXJjZU51bWJlclByb3BlcnR5KHZhbHVlKTtcbiAgICB0aGlzLl9zZXRQYWRkaW5nKCk7XG4gIH1cbiAgX2luZGVudDogbnVtYmVyID0gNDA7XG5cbiAgY29uc3RydWN0b3IocHJpdmF0ZSBfdHJlZU5vZGU6IENka1RyZWVOb2RlPFQ+LFxuICAgICAgICAgICAgICBwcml2YXRlIF90cmVlOiBDZGtUcmVlPFQ+LFxuICAgICAgICAgICAgICAvKipcbiAgICAgICAgICAgICAgICogQGRlcHJlY2F0ZWQgX3JlbmRlcmVyIHBhcmFtZXRlciBubyBsb25nZXIgYmVpbmcgdXNlZC4gVG8gYmUgcmVtb3ZlZC5cbiAgICAgICAgICAgICAgICogQGJyZWFraW5nLWNoYW5nZSAxMS4wLjBcbiAgICAgICAgICAgICAgICovXG4gICAgICAgICAgICAgIF9yZW5kZXJlcjogUmVuZGVyZXIyLFxuICAgICAgICAgICAgICBwcml2YXRlIF9lbGVtZW50OiBFbGVtZW50UmVmPEhUTUxFbGVtZW50PixcbiAgICAgICAgICAgICAgQE9wdGlvbmFsKCkgcHJpdmF0ZSBfZGlyOiBEaXJlY3Rpb25hbGl0eSkge1xuICAgIHRoaXMuX3NldFBhZGRpbmcoKTtcbiAgICBpZiAoX2Rpcikge1xuICAgICAgX2Rpci5jaGFuZ2UucGlwZSh0YWtlVW50aWwodGhpcy5fZGVzdHJveWVkKSkuc3Vic2NyaWJlKCgpID0+IHRoaXMuX3NldFBhZGRpbmcodHJ1ZSkpO1xuICAgIH1cblxuICAgIC8vIEluIEl2eSB0aGUgaW5kZW50YXRpb24gYmluZGluZyBtaWdodCBiZSBzZXQgYmVmb3JlIHRoZSB0cmVlIG5vZGUncyBkYXRhIGhhcyBiZWVuIGFkZGVkLFxuICAgIC8vIHdoaWNoIG1lYW5zIHRoYXQgd2UnbGwgbWlzcyB0aGUgZmlyc3QgcmVuZGVyLiBXZSBoYXZlIHRvIHN1YnNjcmliZSB0byBjaGFuZ2VzIGluIHRoZVxuICAgIC8vIGRhdGEgdG8gZW5zdXJlIHRoYXQgZXZlcnl0aGluZyBpcyB1cCB0byBkYXRlLlxuICAgIF90cmVlTm9kZS5fZGF0YUNoYW5nZXMuc3Vic2NyaWJlKCgpID0+IHRoaXMuX3NldFBhZGRpbmcoKSk7XG4gIH1cblxuICBuZ09uRGVzdHJveSgpIHtcbiAgICB0aGlzLl9kZXN0cm95ZWQubmV4dCgpO1xuICAgIHRoaXMuX2Rlc3Ryb3llZC5jb21wbGV0ZSgpO1xuICB9XG5cbiAgLyoqIFRoZSBwYWRkaW5nIGluZGVudCB2YWx1ZSBmb3IgdGhlIHRyZWUgbm9kZS4gUmV0dXJucyBhIHN0cmluZyB3aXRoIHB4IG51bWJlcnMgaWYgbm90IG51bGwuICovXG4gIF9wYWRkaW5nSW5kZW50KCk6IHN0cmluZ3xudWxsIHtcbiAgICBjb25zdCBub2RlTGV2ZWwgPSAodGhpcy5fdHJlZU5vZGUuZGF0YSAmJiB0aGlzLl90cmVlLnRyZWVDb250cm9sLmdldExldmVsKVxuICAgICAgPyB0aGlzLl90cmVlLnRyZWVDb250cm9sLmdldExldmVsKHRoaXMuX3RyZWVOb2RlLmRhdGEpXG4gICAgICA6IG51bGw7XG4gICAgY29uc3QgbGV2ZWwgPSB0aGlzLl9sZXZlbCA9PSBudWxsID8gbm9kZUxldmVsIDogdGhpcy5fbGV2ZWw7XG4gICAgcmV0dXJuIHR5cGVvZiBsZXZlbCA9PT0gJ251bWJlcicgPyBgJHtsZXZlbCAqIHRoaXMuX2luZGVudH0ke3RoaXMuaW5kZW50VW5pdHN9YCA6IG51bGw7XG4gIH1cblxuICBfc2V0UGFkZGluZyhmb3JjZUNoYW5nZSA9IGZhbHNlKSB7XG4gICAgY29uc3QgcGFkZGluZyA9IHRoaXMuX3BhZGRpbmdJbmRlbnQoKTtcblxuICAgIGlmIChwYWRkaW5nICE9PSB0aGlzLl9jdXJyZW50UGFkZGluZyB8fCBmb3JjZUNoYW5nZSkge1xuICAgICAgY29uc3QgZWxlbWVudCA9IHRoaXMuX2VsZW1lbnQubmF0aXZlRWxlbWVudDtcbiAgICAgIGNvbnN0IHBhZGRpbmdQcm9wID0gdGhpcy5fZGlyICYmIHRoaXMuX2Rpci52YWx1ZSA9PT0gJ3J0bCcgPyAncGFkZGluZ1JpZ2h0JyA6ICdwYWRkaW5nTGVmdCc7XG4gICAgICBjb25zdCByZXNldFByb3AgPSBwYWRkaW5nUHJvcCA9PT0gJ3BhZGRpbmdMZWZ0JyA/ICdwYWRkaW5nUmlnaHQnIDogJ3BhZGRpbmdMZWZ0JztcbiAgICAgIGVsZW1lbnQuc3R5bGVbcGFkZGluZ1Byb3BdID0gcGFkZGluZyB8fCAnJztcbiAgICAgIGVsZW1lbnQuc3R5bGVbcmVzZXRQcm9wXSA9ICcnO1xuICAgICAgdGhpcy5fY3VycmVudFBhZGRpbmcgPSBwYWRkaW5nO1xuICAgIH1cbiAgfVxuXG4gIHN0YXRpYyBuZ0FjY2VwdElucHV0VHlwZV9sZXZlbDogTnVtYmVySW5wdXQ7XG59XG4iXX0=