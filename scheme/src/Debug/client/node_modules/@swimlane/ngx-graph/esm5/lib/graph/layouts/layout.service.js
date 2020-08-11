import { __decorate } from "tslib";
import { Injectable } from '@angular/core';
import { DagreLayout } from './dagre';
import { DagreClusterLayout } from './dagreCluster';
import { DagreNodesOnlyLayout } from './dagreNodesOnly';
import { D3ForceDirectedLayout } from './d3ForceDirected';
import { ColaForceDirectedLayout } from './colaForceDirected';
var layouts = {
    dagre: DagreLayout,
    dagreCluster: DagreClusterLayout,
    dagreNodesOnly: DagreNodesOnlyLayout,
    d3ForceDirected: D3ForceDirectedLayout,
    colaForceDirected: ColaForceDirectedLayout
};
var LayoutService = /** @class */ (function () {
    function LayoutService() {
    }
    LayoutService.prototype.getLayout = function (name) {
        if (layouts[name]) {
            return new layouts[name]();
        }
        else {
            throw new Error("Unknown layout type '" + name + "'");
        }
    };
    LayoutService = __decorate([
        Injectable()
    ], LayoutService);
    return LayoutService;
}());
export { LayoutService };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibGF5b3V0LnNlcnZpY2UuanMiLCJzb3VyY2VSb290Ijoibmc6Ly9Ac3dpbWxhbmUvbmd4LWdyYXBoLyIsInNvdXJjZXMiOlsibGliL2dyYXBoL2xheW91dHMvbGF5b3V0LnNlcnZpY2UudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBLE9BQU8sRUFBRSxVQUFVLEVBQUUsTUFBTSxlQUFlLENBQUM7QUFFM0MsT0FBTyxFQUFFLFdBQVcsRUFBRSxNQUFNLFNBQVMsQ0FBQztBQUN0QyxPQUFPLEVBQUUsa0JBQWtCLEVBQUUsTUFBTSxnQkFBZ0IsQ0FBQztBQUNwRCxPQUFPLEVBQUUsb0JBQW9CLEVBQUUsTUFBTSxrQkFBa0IsQ0FBQztBQUN4RCxPQUFPLEVBQUUscUJBQXFCLEVBQUUsTUFBTSxtQkFBbUIsQ0FBQztBQUMxRCxPQUFPLEVBQUUsdUJBQXVCLEVBQUUsTUFBTSxxQkFBcUIsQ0FBQztBQUU5RCxJQUFNLE9BQU8sR0FBRztJQUNkLEtBQUssRUFBRSxXQUFXO0lBQ2xCLFlBQVksRUFBRSxrQkFBa0I7SUFDaEMsY0FBYyxFQUFFLG9CQUFvQjtJQUNwQyxlQUFlLEVBQUUscUJBQXFCO0lBQ3RDLGlCQUFpQixFQUFFLHVCQUF1QjtDQUMzQyxDQUFDO0FBR0Y7SUFBQTtJQVFBLENBQUM7SUFQQyxpQ0FBUyxHQUFULFVBQVUsSUFBWTtRQUNwQixJQUFJLE9BQU8sQ0FBQyxJQUFJLENBQUMsRUFBRTtZQUNqQixPQUFPLElBQUksT0FBTyxDQUFDLElBQUksQ0FBQyxFQUFFLENBQUM7U0FDNUI7YUFBTTtZQUNMLE1BQU0sSUFBSSxLQUFLLENBQUMsMEJBQXdCLElBQUksTUFBRyxDQUFDLENBQUM7U0FDbEQ7SUFDSCxDQUFDO0lBUFUsYUFBYTtRQUR6QixVQUFVLEVBQUU7T0FDQSxhQUFhLENBUXpCO0lBQUQsb0JBQUM7Q0FBQSxBQVJELElBUUM7U0FSWSxhQUFhIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgSW5qZWN0YWJsZSB9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHsgTGF5b3V0IH0gZnJvbSAnLi4vLi4vbW9kZWxzL2xheW91dC5tb2RlbCc7XG5pbXBvcnQgeyBEYWdyZUxheW91dCB9IGZyb20gJy4vZGFncmUnO1xuaW1wb3J0IHsgRGFncmVDbHVzdGVyTGF5b3V0IH0gZnJvbSAnLi9kYWdyZUNsdXN0ZXInO1xuaW1wb3J0IHsgRGFncmVOb2Rlc09ubHlMYXlvdXQgfSBmcm9tICcuL2RhZ3JlTm9kZXNPbmx5JztcbmltcG9ydCB7IEQzRm9yY2VEaXJlY3RlZExheW91dCB9IGZyb20gJy4vZDNGb3JjZURpcmVjdGVkJztcbmltcG9ydCB7IENvbGFGb3JjZURpcmVjdGVkTGF5b3V0IH0gZnJvbSAnLi9jb2xhRm9yY2VEaXJlY3RlZCc7XG5cbmNvbnN0IGxheW91dHMgPSB7XG4gIGRhZ3JlOiBEYWdyZUxheW91dCxcbiAgZGFncmVDbHVzdGVyOiBEYWdyZUNsdXN0ZXJMYXlvdXQsXG4gIGRhZ3JlTm9kZXNPbmx5OiBEYWdyZU5vZGVzT25seUxheW91dCxcbiAgZDNGb3JjZURpcmVjdGVkOiBEM0ZvcmNlRGlyZWN0ZWRMYXlvdXQsXG4gIGNvbGFGb3JjZURpcmVjdGVkOiBDb2xhRm9yY2VEaXJlY3RlZExheW91dFxufTtcblxuQEluamVjdGFibGUoKVxuZXhwb3J0IGNsYXNzIExheW91dFNlcnZpY2Uge1xuICBnZXRMYXlvdXQobmFtZTogc3RyaW5nKTogTGF5b3V0IHtcbiAgICBpZiAobGF5b3V0c1tuYW1lXSkge1xuICAgICAgcmV0dXJuIG5ldyBsYXlvdXRzW25hbWVdKCk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihgVW5rbm93biBsYXlvdXQgdHlwZSAnJHtuYW1lfSdgKTtcbiAgICB9XG4gIH1cbn1cbiJdfQ==