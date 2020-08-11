import { ElementRef, NgZone, ChangeDetectorRef, EventEmitter, AfterViewInit, OnDestroy, OnChanges, SimpleChanges } from '@angular/core';
import { VisibilityObserver } from '../utils/visibility-observer';
export declare class BaseChartComponent implements OnChanges, AfterViewInit, OnDestroy {
    protected chartElement: ElementRef;
    protected zone: NgZone;
    protected cd: ChangeDetectorRef;
    results: any;
    view: [number, number];
    scheme: any;
    schemeType: string;
    customColors: any;
    animations: boolean;
    select: EventEmitter<any>;
    width: number;
    height: number;
    resizeSubscription: any;
    visibilityObserver: VisibilityObserver;
    constructor(chartElement: ElementRef, zone: NgZone, cd: ChangeDetectorRef);
    ngAfterViewInit(): void;
    ngOnDestroy(): void;
    ngOnChanges(changes: SimpleChanges): void;
    update(): void;
    getContainerDims(): any;
    /**
     * Converts all date objects that appear as name
     * into formatted date strings
     */
    formatDates(): void;
    protected unbindEvents(): void;
    private bindWindowResizeEvent;
    /**
     * Clones the data into a new object
     *
     * @memberOf BaseChart
     */
    private cloneData;
}
