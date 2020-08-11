/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { __awaiter } from "tslib";
import { ComponentHarness, HarnessPredicate } from '@angular/cdk/testing';
import { ɵTileCoordinator as TileCoordinator } from '@angular/material/grid-list';
import { MatGridTileHarness } from './grid-tile-harness';
/** Harness for interacting with a standard `MatGridList` in tests. */
let MatGridListHarness = /** @class */ (() => {
    class MatGridListHarness extends ComponentHarness {
        constructor() {
            super(...arguments);
            /**
             * Tile coordinator that is used by the "MatGridList" for computing
             * positions of tiles. We leverage the coordinator to provide an API
             * for retrieving tiles based on visual tile positions.
             */
            this._tileCoordinator = new TileCoordinator();
        }
        /**
         * Gets a `HarnessPredicate` that can be used to search for a `MatGridListHarness`
         * that meets certain criteria.
         * @param options Options for filtering which dialog instances are considered a match.
         * @return a `HarnessPredicate` configured with the given options.
         */
        static with(options = {}) {
            return new HarnessPredicate(MatGridListHarness, options);
        }
        /** Gets all tiles of the grid-list. */
        getTiles(filters = {}) {
            return __awaiter(this, void 0, void 0, function* () {
                return yield this.locatorForAll(MatGridTileHarness.with(filters))();
            });
        }
        /** Gets the amount of columns of the grid-list. */
        getColumns() {
            return __awaiter(this, void 0, void 0, function* () {
                return Number(yield (yield this.host()).getAttribute('cols'));
            });
        }
        /**
         * Gets a tile of the grid-list that is located at the given location.
         * @param row Zero-based row index.
         * @param column Zero-based column index.
         */
        getTileAtPosition({ row, column }) {
            return __awaiter(this, void 0, void 0, function* () {
                const [tileHarnesses, columns] = yield Promise.all([this.getTiles(), this.getColumns()]);
                const tileSpans = tileHarnesses.map(t => Promise.all([t.getColspan(), t.getRowspan()]));
                const tiles = (yield Promise.all(tileSpans)).map(([colspan, rowspan]) => ({ colspan, rowspan }));
                // Update the tile coordinator to reflect the current column amount and
                // rendered tiles. We update upon every call of this method since we do not
                // know if tiles have been added, removed or updated (in terms of rowspan/colspan).
                this._tileCoordinator.update(columns, tiles);
                // The tile coordinator respects the colspan and rowspan for calculating the positions
                // of tiles, but it does not create multiple position entries if a tile spans over multiple
                // columns or rows. We want to provide an API where developers can retrieve a tile based on
                // any position that lies within the visual tile boundaries. For example: If a tile spans
                // over two columns, then the same tile should be returned for either column indices.
                for (let i = 0; i < this._tileCoordinator.positions.length; i++) {
                    const position = this._tileCoordinator.positions[i];
                    const { rowspan, colspan } = tiles[i];
                    // Return the tile harness if the given position visually resolves to the tile.
                    if (column >= position.col && column <= position.col + colspan - 1 && row >= position.row &&
                        row <= position.row + rowspan - 1) {
                        return tileHarnesses[i];
                    }
                }
                throw Error('Could not find tile at given position.');
            });
        }
    }
    /** The selector for the host element of a `MatGridList` instance. */
    MatGridListHarness.hostSelector = '.mat-grid-list';
    return MatGridListHarness;
})();
export { MatGridListHarness };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZ3JpZC1saXN0LWhhcm5lc3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi8uLi9zcmMvbWF0ZXJpYWwvZ3JpZC1saXN0L3Rlc3RpbmcvZ3JpZC1saXN0LWhhcm5lc3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOztBQUVILE9BQU8sRUFBQyxnQkFBZ0IsRUFBRSxnQkFBZ0IsRUFBQyxNQUFNLHNCQUFzQixDQUFDO0FBQ3hFLE9BQU8sRUFBQyxnQkFBZ0IsSUFBSSxlQUFlLEVBQUMsTUFBTSw2QkFBNkIsQ0FBQztBQUVoRixPQUFPLEVBQUMsa0JBQWtCLEVBQUMsTUFBTSxxQkFBcUIsQ0FBQztBQUV2RCxzRUFBc0U7QUFDdEU7SUFBQSxNQUFhLGtCQUFtQixTQUFRLGdCQUFnQjtRQUF4RDs7WUFjRTs7OztlQUlHO1lBQ0sscUJBQWdCLEdBQUcsSUFBSSxlQUFlLEVBQUUsQ0FBQztRQTBDbkQsQ0FBQztRQXpEQzs7Ozs7V0FLRztRQUNILE1BQU0sQ0FBQyxJQUFJLENBQUMsVUFBa0MsRUFBRTtZQUM5QyxPQUFPLElBQUksZ0JBQWdCLENBQUMsa0JBQWtCLEVBQUUsT0FBTyxDQUFDLENBQUM7UUFDM0QsQ0FBQztRQVNELHVDQUF1QztRQUNqQyxRQUFRLENBQUMsVUFBa0MsRUFBRTs7Z0JBQ2pELE9BQU8sTUFBTSxJQUFJLENBQUMsYUFBYSxDQUFDLGtCQUFrQixDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsQ0FBQyxFQUFFLENBQUM7WUFDdEUsQ0FBQztTQUFBO1FBRUQsbURBQW1EO1FBQzdDLFVBQVU7O2dCQUNkLE9BQU8sTUFBTSxDQUFDLE1BQU0sQ0FBQyxNQUFNLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxDQUFDLFlBQVksQ0FBQyxNQUFNLENBQUMsQ0FBQyxDQUFDO1lBQ2hFLENBQUM7U0FBQTtRQUVEOzs7O1dBSUc7UUFDRyxpQkFBaUIsQ0FBQyxFQUFDLEdBQUcsRUFBRSxNQUFNLEVBQWdDOztnQkFFbEUsTUFBTSxDQUFDLGFBQWEsRUFBRSxPQUFPLENBQUMsR0FBRyxNQUFNLE9BQU8sQ0FBQyxHQUFHLENBQUMsQ0FBQyxJQUFJLENBQUMsUUFBUSxFQUFFLEVBQUUsSUFBSSxDQUFDLFVBQVUsRUFBRSxDQUFDLENBQUMsQ0FBQztnQkFDekYsTUFBTSxTQUFTLEdBQUcsYUFBYSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLE9BQU8sQ0FBQyxHQUFHLENBQUMsQ0FBQyxDQUFDLENBQUMsVUFBVSxFQUFFLEVBQUUsQ0FBQyxDQUFDLFVBQVUsRUFBRSxDQUFDLENBQUMsQ0FBQyxDQUFDO2dCQUN4RixNQUFNLEtBQUssR0FBRyxDQUFDLE1BQU0sT0FBTyxDQUFDLEdBQUcsQ0FBQyxTQUFTLENBQUMsQ0FBQyxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsT0FBTyxFQUFFLE9BQU8sQ0FBQyxFQUFFLEVBQUUsQ0FBQyxDQUFDLEVBQUMsT0FBTyxFQUFFLE9BQU8sRUFBQyxDQUFDLENBQUMsQ0FBQztnQkFDL0YsdUVBQXVFO2dCQUN2RSwyRUFBMkU7Z0JBQzNFLG1GQUFtRjtnQkFDbkYsSUFBSSxDQUFDLGdCQUFnQixDQUFDLE1BQU0sQ0FBQyxPQUFPLEVBQUUsS0FBSyxDQUFDLENBQUM7Z0JBQzdDLHNGQUFzRjtnQkFDdEYsMkZBQTJGO2dCQUMzRiwyRkFBMkY7Z0JBQzNGLHlGQUF5RjtnQkFDekYscUZBQXFGO2dCQUNyRixLQUFLLElBQUksQ0FBQyxHQUFHLENBQUMsRUFBRSxDQUFDLEdBQUcsSUFBSSxDQUFDLGdCQUFnQixDQUFDLFNBQVMsQ0FBQyxNQUFNLEVBQUUsQ0FBQyxFQUFFLEVBQUU7b0JBQy9ELE1BQU0sUUFBUSxHQUFHLElBQUksQ0FBQyxnQkFBZ0IsQ0FBQyxTQUFTLENBQUMsQ0FBQyxDQUFDLENBQUM7b0JBQ3BELE1BQU0sRUFBQyxPQUFPLEVBQUUsT0FBTyxFQUFDLEdBQUcsS0FBSyxDQUFDLENBQUMsQ0FBQyxDQUFDO29CQUNwQywrRUFBK0U7b0JBQy9FLElBQUksTUFBTSxJQUFJLFFBQVEsQ0FBQyxHQUFHLElBQUksTUFBTSxJQUFJLFFBQVEsQ0FBQyxHQUFHLEdBQUcsT0FBTyxHQUFHLENBQUMsSUFBSSxHQUFHLElBQUksUUFBUSxDQUFDLEdBQUc7d0JBQ3JGLEdBQUcsSUFBSSxRQUFRLENBQUMsR0FBRyxHQUFHLE9BQU8sR0FBRyxDQUFDLEVBQUU7d0JBQ3JDLE9BQU8sYUFBYSxDQUFDLENBQUMsQ0FBQyxDQUFDO3FCQUN6QjtpQkFDRjtnQkFDRCxNQUFNLEtBQUssQ0FBQyx3Q0FBd0MsQ0FBQyxDQUFDO1lBQ3hELENBQUM7U0FBQTs7SUEzREQscUVBQXFFO0lBQzlELCtCQUFZLEdBQUcsZ0JBQWdCLENBQUM7SUEyRHpDLHlCQUFDO0tBQUE7U0E3RFksa0JBQWtCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Q29tcG9uZW50SGFybmVzcywgSGFybmVzc1ByZWRpY2F0ZX0gZnJvbSAnQGFuZ3VsYXIvY2RrL3Rlc3RpbmcnO1xuaW1wb3J0IHvJtVRpbGVDb29yZGluYXRvciBhcyBUaWxlQ29vcmRpbmF0b3J9IGZyb20gJ0Bhbmd1bGFyL21hdGVyaWFsL2dyaWQtbGlzdCc7XG5pbXBvcnQge0dyaWRMaXN0SGFybmVzc0ZpbHRlcnMsIEdyaWRUaWxlSGFybmVzc0ZpbHRlcnN9IGZyb20gJy4vZ3JpZC1saXN0LWhhcm5lc3MtZmlsdGVycyc7XG5pbXBvcnQge01hdEdyaWRUaWxlSGFybmVzc30gZnJvbSAnLi9ncmlkLXRpbGUtaGFybmVzcyc7XG5cbi8qKiBIYXJuZXNzIGZvciBpbnRlcmFjdGluZyB3aXRoIGEgc3RhbmRhcmQgYE1hdEdyaWRMaXN0YCBpbiB0ZXN0cy4gKi9cbmV4cG9ydCBjbGFzcyBNYXRHcmlkTGlzdEhhcm5lc3MgZXh0ZW5kcyBDb21wb25lbnRIYXJuZXNzIHtcbiAgLyoqIFRoZSBzZWxlY3RvciBmb3IgdGhlIGhvc3QgZWxlbWVudCBvZiBhIGBNYXRHcmlkTGlzdGAgaW5zdGFuY2UuICovXG4gIHN0YXRpYyBob3N0U2VsZWN0b3IgPSAnLm1hdC1ncmlkLWxpc3QnO1xuXG4gIC8qKlxuICAgKiBHZXRzIGEgYEhhcm5lc3NQcmVkaWNhdGVgIHRoYXQgY2FuIGJlIHVzZWQgdG8gc2VhcmNoIGZvciBhIGBNYXRHcmlkTGlzdEhhcm5lc3NgXG4gICAqIHRoYXQgbWVldHMgY2VydGFpbiBjcml0ZXJpYS5cbiAgICogQHBhcmFtIG9wdGlvbnMgT3B0aW9ucyBmb3IgZmlsdGVyaW5nIHdoaWNoIGRpYWxvZyBpbnN0YW5jZXMgYXJlIGNvbnNpZGVyZWQgYSBtYXRjaC5cbiAgICogQHJldHVybiBhIGBIYXJuZXNzUHJlZGljYXRlYCBjb25maWd1cmVkIHdpdGggdGhlIGdpdmVuIG9wdGlvbnMuXG4gICAqL1xuICBzdGF0aWMgd2l0aChvcHRpb25zOiBHcmlkTGlzdEhhcm5lc3NGaWx0ZXJzID0ge30pOiBIYXJuZXNzUHJlZGljYXRlPE1hdEdyaWRMaXN0SGFybmVzcz4ge1xuICAgIHJldHVybiBuZXcgSGFybmVzc1ByZWRpY2F0ZShNYXRHcmlkTGlzdEhhcm5lc3MsIG9wdGlvbnMpO1xuICB9XG5cbiAgLyoqXG4gICAqIFRpbGUgY29vcmRpbmF0b3IgdGhhdCBpcyB1c2VkIGJ5IHRoZSBcIk1hdEdyaWRMaXN0XCIgZm9yIGNvbXB1dGluZ1xuICAgKiBwb3NpdGlvbnMgb2YgdGlsZXMuIFdlIGxldmVyYWdlIHRoZSBjb29yZGluYXRvciB0byBwcm92aWRlIGFuIEFQSVxuICAgKiBmb3IgcmV0cmlldmluZyB0aWxlcyBiYXNlZCBvbiB2aXN1YWwgdGlsZSBwb3NpdGlvbnMuXG4gICAqL1xuICBwcml2YXRlIF90aWxlQ29vcmRpbmF0b3IgPSBuZXcgVGlsZUNvb3JkaW5hdG9yKCk7XG5cbiAgLyoqIEdldHMgYWxsIHRpbGVzIG9mIHRoZSBncmlkLWxpc3QuICovXG4gIGFzeW5jIGdldFRpbGVzKGZpbHRlcnM6IEdyaWRUaWxlSGFybmVzc0ZpbHRlcnMgPSB7fSk6IFByb21pc2U8TWF0R3JpZFRpbGVIYXJuZXNzW10+IHtcbiAgICByZXR1cm4gYXdhaXQgdGhpcy5sb2NhdG9yRm9yQWxsKE1hdEdyaWRUaWxlSGFybmVzcy53aXRoKGZpbHRlcnMpKSgpO1xuICB9XG5cbiAgLyoqIEdldHMgdGhlIGFtb3VudCBvZiBjb2x1bW5zIG9mIHRoZSBncmlkLWxpc3QuICovXG4gIGFzeW5jIGdldENvbHVtbnMoKTogUHJvbWlzZTxudW1iZXI+IHtcbiAgICByZXR1cm4gTnVtYmVyKGF3YWl0IChhd2FpdCB0aGlzLmhvc3QoKSkuZ2V0QXR0cmlidXRlKCdjb2xzJykpO1xuICB9XG5cbiAgLyoqXG4gICAqIEdldHMgYSB0aWxlIG9mIHRoZSBncmlkLWxpc3QgdGhhdCBpcyBsb2NhdGVkIGF0IHRoZSBnaXZlbiBsb2NhdGlvbi5cbiAgICogQHBhcmFtIHJvdyBaZXJvLWJhc2VkIHJvdyBpbmRleC5cbiAgICogQHBhcmFtIGNvbHVtbiBaZXJvLWJhc2VkIGNvbHVtbiBpbmRleC5cbiAgICovXG4gIGFzeW5jIGdldFRpbGVBdFBvc2l0aW9uKHtyb3csIGNvbHVtbn06IHtyb3c6IG51bWJlciwgY29sdW1uOiBudW1iZXJ9KTpcbiAgICAgIFByb21pc2U8TWF0R3JpZFRpbGVIYXJuZXNzPiB7XG4gICAgY29uc3QgW3RpbGVIYXJuZXNzZXMsIGNvbHVtbnNdID0gYXdhaXQgUHJvbWlzZS5hbGwoW3RoaXMuZ2V0VGlsZXMoKSwgdGhpcy5nZXRDb2x1bW5zKCldKTtcbiAgICBjb25zdCB0aWxlU3BhbnMgPSB0aWxlSGFybmVzc2VzLm1hcCh0ID0+IFByb21pc2UuYWxsKFt0LmdldENvbHNwYW4oKSwgdC5nZXRSb3dzcGFuKCldKSk7XG4gICAgY29uc3QgdGlsZXMgPSAoYXdhaXQgUHJvbWlzZS5hbGwodGlsZVNwYW5zKSkubWFwKChbY29sc3Bhbiwgcm93c3Bhbl0pID0+ICh7Y29sc3Bhbiwgcm93c3Bhbn0pKTtcbiAgICAvLyBVcGRhdGUgdGhlIHRpbGUgY29vcmRpbmF0b3IgdG8gcmVmbGVjdCB0aGUgY3VycmVudCBjb2x1bW4gYW1vdW50IGFuZFxuICAgIC8vIHJlbmRlcmVkIHRpbGVzLiBXZSB1cGRhdGUgdXBvbiBldmVyeSBjYWxsIG9mIHRoaXMgbWV0aG9kIHNpbmNlIHdlIGRvIG5vdFxuICAgIC8vIGtub3cgaWYgdGlsZXMgaGF2ZSBiZWVuIGFkZGVkLCByZW1vdmVkIG9yIHVwZGF0ZWQgKGluIHRlcm1zIG9mIHJvd3NwYW4vY29sc3BhbikuXG4gICAgdGhpcy5fdGlsZUNvb3JkaW5hdG9yLnVwZGF0ZShjb2x1bW5zLCB0aWxlcyk7XG4gICAgLy8gVGhlIHRpbGUgY29vcmRpbmF0b3IgcmVzcGVjdHMgdGhlIGNvbHNwYW4gYW5kIHJvd3NwYW4gZm9yIGNhbGN1bGF0aW5nIHRoZSBwb3NpdGlvbnNcbiAgICAvLyBvZiB0aWxlcywgYnV0IGl0IGRvZXMgbm90IGNyZWF0ZSBtdWx0aXBsZSBwb3NpdGlvbiBlbnRyaWVzIGlmIGEgdGlsZSBzcGFucyBvdmVyIG11bHRpcGxlXG4gICAgLy8gY29sdW1ucyBvciByb3dzLiBXZSB3YW50IHRvIHByb3ZpZGUgYW4gQVBJIHdoZXJlIGRldmVsb3BlcnMgY2FuIHJldHJpZXZlIGEgdGlsZSBiYXNlZCBvblxuICAgIC8vIGFueSBwb3NpdGlvbiB0aGF0IGxpZXMgd2l0aGluIHRoZSB2aXN1YWwgdGlsZSBib3VuZGFyaWVzLiBGb3IgZXhhbXBsZTogSWYgYSB0aWxlIHNwYW5zXG4gICAgLy8gb3ZlciB0d28gY29sdW1ucywgdGhlbiB0aGUgc2FtZSB0aWxlIHNob3VsZCBiZSByZXR1cm5lZCBmb3IgZWl0aGVyIGNvbHVtbiBpbmRpY2VzLlxuICAgIGZvciAobGV0IGkgPSAwOyBpIDwgdGhpcy5fdGlsZUNvb3JkaW5hdG9yLnBvc2l0aW9ucy5sZW5ndGg7IGkrKykge1xuICAgICAgY29uc3QgcG9zaXRpb24gPSB0aGlzLl90aWxlQ29vcmRpbmF0b3IucG9zaXRpb25zW2ldO1xuICAgICAgY29uc3Qge3Jvd3NwYW4sIGNvbHNwYW59ID0gdGlsZXNbaV07XG4gICAgICAvLyBSZXR1cm4gdGhlIHRpbGUgaGFybmVzcyBpZiB0aGUgZ2l2ZW4gcG9zaXRpb24gdmlzdWFsbHkgcmVzb2x2ZXMgdG8gdGhlIHRpbGUuXG4gICAgICBpZiAoY29sdW1uID49IHBvc2l0aW9uLmNvbCAmJiBjb2x1bW4gPD0gcG9zaXRpb24uY29sICsgY29sc3BhbiAtIDEgJiYgcm93ID49IHBvc2l0aW9uLnJvdyAmJlxuICAgICAgICAgIHJvdyA8PSBwb3NpdGlvbi5yb3cgKyByb3dzcGFuIC0gMSkge1xuICAgICAgICByZXR1cm4gdGlsZUhhcm5lc3Nlc1tpXTtcbiAgICAgIH1cbiAgICB9XG4gICAgdGhyb3cgRXJyb3IoJ0NvdWxkIG5vdCBmaW5kIHRpbGUgYXQgZ2l2ZW4gcG9zaXRpb24uJyk7XG4gIH1cbn1cbiJdfQ==