import { EventEmitter, TemplateRef } from '@angular/core';
import { ColorHelper } from '../common/color.helper';
import * as ɵngcc0 from '@angular/core';
export declare class GaugeArcComponent {
    backgroundArc: any;
    valueArc: any;
    cornerRadius: any;
    colors: ColorHelper;
    isActive: boolean;
    tooltipDisabled: boolean;
    valueFormatting: (value: any) => string;
    tooltipTemplate: TemplateRef<any>;
    animations: boolean;
    select: EventEmitter<any>;
    activate: EventEmitter<any>;
    deactivate: EventEmitter<any>;
    tooltipText(arc: any): string;
    static ɵfac: ɵngcc0.ɵɵFactoryDef<GaugeArcComponent, never>;
    static ɵcmp: ɵngcc0.ɵɵComponentDefWithMeta<GaugeArcComponent, "g[ngx-charts-gauge-arc]", never, { "isActive": "isActive"; "tooltipDisabled": "tooltipDisabled"; "animations": "animations"; "backgroundArc": "backgroundArc"; "valueArc": "valueArc"; "cornerRadius": "cornerRadius"; "colors": "colors"; "valueFormatting": "valueFormatting"; "tooltipTemplate": "tooltipTemplate"; }, { "select": "select"; "activate": "activate"; "deactivate": "deactivate"; }, never, never>;
}

//# sourceMappingURL=gauge-arc.component.d.ts.map