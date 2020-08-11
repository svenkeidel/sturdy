import { Layout } from '../../models/layout.model';
import { Graph } from '../../models/graph.model';
import { Edge } from '../../models/edge.model';
import { DagreSettings } from './dagre';
export interface DagreNodesOnlySettings extends DagreSettings {
    curveDistance?: number;
}
export declare class DagreNodesOnlyLayout implements Layout {
    defaultSettings: DagreNodesOnlySettings;
    settings: DagreNodesOnlySettings;
    dagreGraph: any;
    dagreNodes: any;
    dagreEdges: any;
    run(graph: Graph): Graph;
    updateEdge(graph: Graph, edge: Edge): Graph;
    createDagreGraph(graph: Graph): any;
}
