/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { DataSource } from '@angular/cdk/collections';
import { BehaviorSubject, merge } from 'rxjs';
import { map } from 'rxjs/operators';
/**
 * Data source for nested tree.
 *
 * The data source for nested tree doesn't have to consider node flattener, or the way to expand
 * or collapse. The expansion/collapsion will be handled by TreeControl and each non-leaf node.
 */
export class MatTreeNestedDataSource extends DataSource {
    constructor() {
        super(...arguments);
        this._data = new BehaviorSubject([]);
    }
    /**
     * Data for the nested tree
     */
    get data() { return this._data.value; }
    set data(value) { this._data.next(value); }
    connect(collectionViewer) {
        return merge(...[collectionViewer.viewChange, this._data])
            .pipe(map(() => {
            return this.data;
        }));
    }
    disconnect() {
        // no op
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibmVzdGVkLWRhdGEtc291cmNlLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vLi4vc3JjL21hdGVyaWFsL3RyZWUvZGF0YS1zb3VyY2UvbmVzdGVkLWRhdGEtc291cmNlLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRztBQUVILE9BQU8sRUFBbUIsVUFBVSxFQUFDLE1BQU0sMEJBQTBCLENBQUM7QUFDdEUsT0FBTyxFQUFDLGVBQWUsRUFBRSxLQUFLLEVBQWEsTUFBTSxNQUFNLENBQUM7QUFDeEQsT0FBTyxFQUFDLEdBQUcsRUFBQyxNQUFNLGdCQUFnQixDQUFDO0FBR25DOzs7OztHQUtHO0FBQ0gsTUFBTSxPQUFPLHVCQUEyQixTQUFRLFVBQWE7SUFBN0Q7O1FBQ0UsVUFBSyxHQUFHLElBQUksZUFBZSxDQUFNLEVBQUUsQ0FBQyxDQUFDO0lBa0J2QyxDQUFDO0lBaEJDOztPQUVHO0lBQ0gsSUFBSSxJQUFJLEtBQUssT0FBTyxJQUFJLENBQUMsS0FBSyxDQUFDLEtBQUssQ0FBQyxDQUFDLENBQUM7SUFDdkMsSUFBSSxJQUFJLENBQUMsS0FBVSxJQUFJLElBQUksQ0FBQyxLQUFLLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxDQUFDLENBQUMsQ0FBQztJQUVoRCxPQUFPLENBQUMsZ0JBQWtDO1FBQ3hDLE9BQU8sS0FBSyxDQUFDLEdBQUcsQ0FBQyxnQkFBZ0IsQ0FBQyxVQUFVLEVBQUUsSUFBSSxDQUFDLEtBQUssQ0FBQyxDQUFDO2FBQ3ZELElBQUksQ0FBQyxHQUFHLENBQUMsR0FBRyxFQUFFO1lBQ2IsT0FBTyxJQUFJLENBQUMsSUFBSSxDQUFDO1FBQ25CLENBQUMsQ0FBQyxDQUFDLENBQUM7SUFDUixDQUFDO0lBRUQsVUFBVTtRQUNSLFFBQVE7SUFDVixDQUFDO0NBQ0YiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtDb2xsZWN0aW9uVmlld2VyLCBEYXRhU291cmNlfSBmcm9tICdAYW5ndWxhci9jZGsvY29sbGVjdGlvbnMnO1xuaW1wb3J0IHtCZWhhdmlvclN1YmplY3QsIG1lcmdlLCBPYnNlcnZhYmxlfSBmcm9tICdyeGpzJztcbmltcG9ydCB7bWFwfSBmcm9tICdyeGpzL29wZXJhdG9ycyc7XG5cblxuLyoqXG4gKiBEYXRhIHNvdXJjZSBmb3IgbmVzdGVkIHRyZWUuXG4gKlxuICogVGhlIGRhdGEgc291cmNlIGZvciBuZXN0ZWQgdHJlZSBkb2Vzbid0IGhhdmUgdG8gY29uc2lkZXIgbm9kZSBmbGF0dGVuZXIsIG9yIHRoZSB3YXkgdG8gZXhwYW5kXG4gKiBvciBjb2xsYXBzZS4gVGhlIGV4cGFuc2lvbi9jb2xsYXBzaW9uIHdpbGwgYmUgaGFuZGxlZCBieSBUcmVlQ29udHJvbCBhbmQgZWFjaCBub24tbGVhZiBub2RlLlxuICovXG5leHBvcnQgY2xhc3MgTWF0VHJlZU5lc3RlZERhdGFTb3VyY2U8VD4gZXh0ZW5kcyBEYXRhU291cmNlPFQ+IHtcbiAgX2RhdGEgPSBuZXcgQmVoYXZpb3JTdWJqZWN0PFRbXT4oW10pO1xuXG4gIC8qKlxuICAgKiBEYXRhIGZvciB0aGUgbmVzdGVkIHRyZWVcbiAgICovXG4gIGdldCBkYXRhKCkgeyByZXR1cm4gdGhpcy5fZGF0YS52YWx1ZTsgfVxuICBzZXQgZGF0YSh2YWx1ZTogVFtdKSB7IHRoaXMuX2RhdGEubmV4dCh2YWx1ZSk7IH1cblxuICBjb25uZWN0KGNvbGxlY3Rpb25WaWV3ZXI6IENvbGxlY3Rpb25WaWV3ZXIpOiBPYnNlcnZhYmxlPFRbXT4ge1xuICAgIHJldHVybiBtZXJnZSguLi5bY29sbGVjdGlvblZpZXdlci52aWV3Q2hhbmdlLCB0aGlzLl9kYXRhXSlcbiAgICAgIC5waXBlKG1hcCgoKSA9PiB7XG4gICAgICAgIHJldHVybiB0aGlzLmRhdGE7XG4gICAgICB9KSk7XG4gIH1cblxuICBkaXNjb25uZWN0KCkge1xuICAgIC8vIG5vIG9wXG4gIH1cbn1cblxuIl19