/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { AbstractControlDirective } from './abstract_control_directive';
function unimplemented() {
    throw new Error('unimplemented');
}
/**
 * @description
 * A base class that all control `FormControl`-based directives extend. It binds a `FormControl`
 * object to a DOM element.
 *
 * @publicApi
 */
export class NgControl extends AbstractControlDirective {
    constructor() {
        super(...arguments);
        /**
         * @description
         * The parent form for the control.
         *
         * @internal
         */
        this._parent = null;
        /**
         * @description
         * The name for the control
         */
        this.name = null;
        /**
         * @description
         * The value accessor for the control
         */
        this.valueAccessor = null;
        /**
         * @description
         * The uncomposed array of synchronous validators for the control
         *
         * @internal
         */
        this._rawValidators = [];
        /**
         * @description
         * The uncomposed array of async validators for the control
         *
         * @internal
         */
        this._rawAsyncValidators = [];
    }
    /**
     * @description
     * The registered synchronous validator function for the control
     *
     * @throws An exception that this method is not implemented
     */
    get validator() {
        return unimplemented();
    }
    /**
     * @description
     * The registered async validator function for the control
     *
     * @throws An exception that this method is not implemented
     */
    get asyncValidator() {
        return unimplemented();
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibmdfY29udHJvbC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2Zvcm1zL3NyYy9kaXJlY3RpdmVzL25nX2NvbnRyb2wudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBR0gsT0FBTyxFQUFDLHdCQUF3QixFQUFDLE1BQU0sOEJBQThCLENBQUM7QUFLdEUsU0FBUyxhQUFhO0lBQ3BCLE1BQU0sSUFBSSxLQUFLLENBQUMsZUFBZSxDQUFDLENBQUM7QUFDbkMsQ0FBQztBQUVEOzs7Ozs7R0FNRztBQUNILE1BQU0sT0FBZ0IsU0FBVSxTQUFRLHdCQUF3QjtJQUFoRTs7UUFDRTs7Ozs7V0FLRztRQUNILFlBQU8sR0FBMEIsSUFBSSxDQUFDO1FBRXRDOzs7V0FHRztRQUNILFNBQUksR0FBdUIsSUFBSSxDQUFDO1FBRWhDOzs7V0FHRztRQUNILGtCQUFhLEdBQThCLElBQUksQ0FBQztRQUVoRDs7Ozs7V0FLRztRQUNILG1CQUFjLEdBQWlDLEVBQUUsQ0FBQztRQUVsRDs7Ozs7V0FLRztRQUNILHdCQUFtQixHQUEyQyxFQUFFLENBQUM7SUE2Qm5FLENBQUM7SUEzQkM7Ozs7O09BS0c7SUFDSCxJQUFJLFNBQVM7UUFDWCxPQUFvQixhQUFhLEVBQUUsQ0FBQztJQUN0QyxDQUFDO0lBRUQ7Ozs7O09BS0c7SUFDSCxJQUFJLGNBQWM7UUFDaEIsT0FBeUIsYUFBYSxFQUFFLENBQUM7SUFDM0MsQ0FBQztDQVNGIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cblxuaW1wb3J0IHtBYnN0cmFjdENvbnRyb2xEaXJlY3RpdmV9IGZyb20gJy4vYWJzdHJhY3RfY29udHJvbF9kaXJlY3RpdmUnO1xuaW1wb3J0IHtDb250cm9sQ29udGFpbmVyfSBmcm9tICcuL2NvbnRyb2xfY29udGFpbmVyJztcbmltcG9ydCB7Q29udHJvbFZhbHVlQWNjZXNzb3J9IGZyb20gJy4vY29udHJvbF92YWx1ZV9hY2Nlc3Nvcic7XG5pbXBvcnQge0FzeW5jVmFsaWRhdG9yLCBBc3luY1ZhbGlkYXRvckZuLCBWYWxpZGF0b3IsIFZhbGlkYXRvckZufSBmcm9tICcuL3ZhbGlkYXRvcnMnO1xuXG5mdW5jdGlvbiB1bmltcGxlbWVudGVkKCk6IGFueSB7XG4gIHRocm93IG5ldyBFcnJvcigndW5pbXBsZW1lbnRlZCcpO1xufVxuXG4vKipcbiAqIEBkZXNjcmlwdGlvblxuICogQSBiYXNlIGNsYXNzIHRoYXQgYWxsIGNvbnRyb2wgYEZvcm1Db250cm9sYC1iYXNlZCBkaXJlY3RpdmVzIGV4dGVuZC4gSXQgYmluZHMgYSBgRm9ybUNvbnRyb2xgXG4gKiBvYmplY3QgdG8gYSBET00gZWxlbWVudC5cbiAqXG4gKiBAcHVibGljQXBpXG4gKi9cbmV4cG9ydCBhYnN0cmFjdCBjbGFzcyBOZ0NvbnRyb2wgZXh0ZW5kcyBBYnN0cmFjdENvbnRyb2xEaXJlY3RpdmUge1xuICAvKipcbiAgICogQGRlc2NyaXB0aW9uXG4gICAqIFRoZSBwYXJlbnQgZm9ybSBmb3IgdGhlIGNvbnRyb2wuXG4gICAqXG4gICAqIEBpbnRlcm5hbFxuICAgKi9cbiAgX3BhcmVudDogQ29udHJvbENvbnRhaW5lcnxudWxsID0gbnVsbDtcblxuICAvKipcbiAgICogQGRlc2NyaXB0aW9uXG4gICAqIFRoZSBuYW1lIGZvciB0aGUgY29udHJvbFxuICAgKi9cbiAgbmFtZTogc3RyaW5nfG51bWJlcnxudWxsID0gbnVsbDtcblxuICAvKipcbiAgICogQGRlc2NyaXB0aW9uXG4gICAqIFRoZSB2YWx1ZSBhY2Nlc3NvciBmb3IgdGhlIGNvbnRyb2xcbiAgICovXG4gIHZhbHVlQWNjZXNzb3I6IENvbnRyb2xWYWx1ZUFjY2Vzc29yfG51bGwgPSBudWxsO1xuXG4gIC8qKlxuICAgKiBAZGVzY3JpcHRpb25cbiAgICogVGhlIHVuY29tcG9zZWQgYXJyYXkgb2Ygc3luY2hyb25vdXMgdmFsaWRhdG9ycyBmb3IgdGhlIGNvbnRyb2xcbiAgICpcbiAgICogQGludGVybmFsXG4gICAqL1xuICBfcmF3VmFsaWRhdG9yczogQXJyYXk8VmFsaWRhdG9yfFZhbGlkYXRvckZuPiA9IFtdO1xuXG4gIC8qKlxuICAgKiBAZGVzY3JpcHRpb25cbiAgICogVGhlIHVuY29tcG9zZWQgYXJyYXkgb2YgYXN5bmMgdmFsaWRhdG9ycyBmb3IgdGhlIGNvbnRyb2xcbiAgICpcbiAgICogQGludGVybmFsXG4gICAqL1xuICBfcmF3QXN5bmNWYWxpZGF0b3JzOiBBcnJheTxBc3luY1ZhbGlkYXRvcnxBc3luY1ZhbGlkYXRvckZuPiA9IFtdO1xuXG4gIC8qKlxuICAgKiBAZGVzY3JpcHRpb25cbiAgICogVGhlIHJlZ2lzdGVyZWQgc3luY2hyb25vdXMgdmFsaWRhdG9yIGZ1bmN0aW9uIGZvciB0aGUgY29udHJvbFxuICAgKlxuICAgKiBAdGhyb3dzIEFuIGV4Y2VwdGlvbiB0aGF0IHRoaXMgbWV0aG9kIGlzIG5vdCBpbXBsZW1lbnRlZFxuICAgKi9cbiAgZ2V0IHZhbGlkYXRvcigpOiBWYWxpZGF0b3JGbnxudWxsIHtcbiAgICByZXR1cm4gPFZhbGlkYXRvckZuPnVuaW1wbGVtZW50ZWQoKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBAZGVzY3JpcHRpb25cbiAgICogVGhlIHJlZ2lzdGVyZWQgYXN5bmMgdmFsaWRhdG9yIGZ1bmN0aW9uIGZvciB0aGUgY29udHJvbFxuICAgKlxuICAgKiBAdGhyb3dzIEFuIGV4Y2VwdGlvbiB0aGF0IHRoaXMgbWV0aG9kIGlzIG5vdCBpbXBsZW1lbnRlZFxuICAgKi9cbiAgZ2V0IGFzeW5jVmFsaWRhdG9yKCk6IEFzeW5jVmFsaWRhdG9yRm58bnVsbCB7XG4gICAgcmV0dXJuIDxBc3luY1ZhbGlkYXRvckZuPnVuaW1wbGVtZW50ZWQoKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBAZGVzY3JpcHRpb25cbiAgICogVGhlIGNhbGxiYWNrIG1ldGhvZCB0byB1cGRhdGUgdGhlIG1vZGVsIGZyb20gdGhlIHZpZXcgd2hlbiByZXF1ZXN0ZWRcbiAgICpcbiAgICogQHBhcmFtIG5ld1ZhbHVlIFRoZSBuZXcgdmFsdWUgZm9yIHRoZSB2aWV3XG4gICAqL1xuICBhYnN0cmFjdCB2aWV3VG9Nb2RlbFVwZGF0ZShuZXdWYWx1ZTogYW55KTogdm9pZDtcbn1cbiJdfQ==