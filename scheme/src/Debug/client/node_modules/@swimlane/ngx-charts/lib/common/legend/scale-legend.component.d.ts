import { OnChanges, SimpleChanges } from '@angular/core';
import { DomSanitizer } from '@angular/platform-browser';
import * as ɵngcc0 from '@angular/core';
export declare class ScaleLegendComponent implements OnChanges {
    private sanitizer;
    valueRange: any;
    colors: any;
    height: any;
    width: any;
    horizontal: boolean;
    gradient: any;
    constructor(sanitizer: DomSanitizer);
    ngOnChanges(changes: SimpleChanges): void;
    /**
     * Generates the string used in the gradient stylesheet properties
     * @param colors array of colors
     * @param splits array of splits on a scale of (0, 1)
     */
    gradientString(colors: any, splits: any): string;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<ScaleLegendComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<ScaleLegendComponent, "ngx-charts-scale-legend", never, { "horizontal": "horizontal"; "valueRange": "valueRange"; "colors": "colors"; "height": "height"; "width": "width"; }, {}, never, never>;
}

//# sourceMappingURL=scale-legend.component.d.ts.map