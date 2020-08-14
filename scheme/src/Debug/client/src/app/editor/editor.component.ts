import { Component, OnInit, ViewChild, ElementRef, NgZone, Input } from '@angular/core';
import { CommunicationService } from '../communication.service';
import { BehaviorSubject } from 'rxjs'
import {MatInputModule} from '@angular/material/input';
import {MatFormFieldModule} from '@angular/material/form-field';
import {MatButtonModule} from '@angular/material/button';
import {MatGridListModule} from '@angular/material/grid-list';
import {MatTableModule} from '@angular/material/table';
import {MatTableDataSource} from '@angular/material/table';
import {MatListModule} from '@angular/material/list';
import {MatTooltipModule} from '@angular/material/tooltip';
import {TooltipPosition} from '@angular/material/tooltip';


import * as ace from 'ace-builds'; // ace module ..
// language package, choose your own 
import 'ace-builds/src-noconflict/mode-javascript';
// ui-theme package
import 'ace-builds/src-noconflict/theme-monokai';

import {NgxGraphModule} from '@swimlane/ngx-graph'

import * as shape from 'd3-shape';
import { ElementSchemaRegistry } from '@angular/compiler';



interface StoreElem {
  addr: string;
  refs?: string[];
  val?: string;
  lastElem: boolean;
  closure: boolean;
  variableName?: string;
}

interface StackElem {
  name: string;
  addr: string;
}

interface EnvElem {
  var: string;
  addr: string;
}

interface Expr {
  short: string;
  long: string;
}

interface EvaluatedExpr {
  short: string;
  long: string;
  val: string;
}

interface StoreGraphNode {
  id: string;
  label: string;
  dimension: {width: number, height: number}
}

interface StoreGraphEdge{
  source: string;
  target: string;
  label: string;
}




const THEME = 'ace/theme/github';
const LANG = 'ace/mode/javascript';

@Component({
  selector: 'app-editor',
  templateUrl: './editor.component.html',
  styleUrls: ['./editor.component.css']
})



export class EditorComponent implements OnInit {

  @Input() path: String;
  code: string;                                     // text in editor
  stack;                                            // stack
  latestStore:StoreElem [] ;                        // store as list of store elements
  latestStoreTable;                                 // store as table object to allow filtering
  latestEnv: EnvElem [];                            // environment as list of env elements
  hierarchialGraph = {nodes: [], links: []}         // CFG
  edges;                                            // CFG edges
  nodes;                                            // CFG nodes
  hierarchialGraphStore = {nodes: [], links: []}    // graph of resolved store element
  storeGraphNodes: StoreGraphNode [];               // nodes of resolved store element
  storeGraphEdges: StoreGraphEdge [];               // edges of resolved store element
  derefStoreElem: string;                           // value of resolved store element
  derefStoreElemAddress: string;                    // address of resolved store element
  exprs: Expr [] = [];                              // list of processed expressions
  evaluatedExprs: EvaluatedExpr [] = [];            // list of evaluated expressions and values
  curve = shape.curveBundle.beta(1);                // CFG curves

  // apply filter on store table
  applyFilter(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    this.latestStoreTable.filter = filterValue.trim().toLowerCase();
  }

  constructor(private chatService: CommunicationService, private ngZone: NgZone) {
    // calls corresponding handler if websocket message arrives
    chatService.messages.subscribe(msg => {
      if(msg.tag == "LoadSourceCodeResponse"){
        this.loadSourceCodeResponseHandler(JSON.stringify(msg))
      }
      else if(msg.tag == "RefreshResponse"){
        this.refreshResponseHandler(JSON.stringify(msg))
      }
      else if(msg.tag == "BreakpointResponse"){
        this.breakpointResponseHandler(JSON.stringify(msg))
      }
      else if(msg.tag == "ExceptionResponse"){
        this.exceptionResponseHandler(JSON.stringify(msg))
      }
      else if(msg.tag == "CurrentExpressionResponse"){
        this.currentExpressionResponseHandler(JSON.stringify(msg))
      }
      else if(msg.tag == "EvaluatedExpressionResponse"){
        this.evaluatedExpressionResponseHandler(JSON.stringify(msg))
      }
    })
   }
  
  // initialize text editor
  ngOnInit(): void {
    var editor = ace.edit( "editor1" );
    editor.setTheme("ace/theme/monokai");
    editor.setValue("Insert code here!");
  }




// SEND REQUESTS

  // extracts code in text editor and sends it as StartDebuggerRequest -> server starts debugging
  sendStartRequest(){
    var editor = ace.edit( "editor1" );
    var code = editor.getValue();
    let startDebuggerRequest = {
      "tag":"StartDebuggerRequest",
      "code":code
    }
    this.chatService.startDebuggerRequests.next(startDebuggerRequest);
  }

  // send ContinueRequest -> server evaluates expressions until a breakpoint is reached
  sendContinueRequest(){
    let continueRequest = {
      "tag":"ContinueRequest"
    }
    this.chatService.continueRequests.next(continueRequest);
  }

  // send StepRequest -> server evaluates next expression  
  sendStepRequest(){
    let stepRequest = {
      "tag":"StepRequest"
    }
    this.chatService.stepRequests.next(stepRequest);
  }

  // extracts filename of form and sends LoadSourceCodeRequest -> server responds with source code of file
  sendLoadSourceCodeRequest(){
    var bla = this.path;
    let loadSourceCodeRequestMessage = {
      "tag":"LoadSourceCodeRequest",
      "path":this.path
    }
    this.chatService.messages.next(loadSourceCodeRequestMessage);

  }

  // send RefreshRequest -> server refreshs debug state
  sendRefreshRequest(){
    let refreshRequestMessage = {
      "tag":"RefreshRequest"
    }
    this.chatService.refreshRequests.next(refreshRequestMessage);

  }



// RESPONSE HANDLER

  // set value of editor corresponding to source code in websocket message
  loadSourceCodeResponseHandler(msg){
    var obj = JSON.parse(msg)
    this.code = obj.code
    var editor = ace.edit( "editor1" );
    editor.setValue(this.code);

  }

  // process debug information of websocket message
  breakpointResponseHandler(msg){

    var obj = JSON.parse(msg)

    this.nodes = obj.cfgNodes
    this.edges = obj.cfgEdges
    this.createCFG(this.nodes, this.edges)

    var unparsedEnv = obj.latestEnv
    this.latestEnv = this.parseEnv(unparsedEnv)

    this.latestStore = this.createStore(obj.latestStore)
    this.latestStoreTable = new MatTableDataSource(this.latestStore)

    var unparsedStack = this.createStack(obj.stack)
    console.log(unparsedStack)
    this.stack = this.parseStack(unparsedStack)
  }

  // sets debug information and editor text value to default values
  refreshResponseHandler(msg){
    this.code = "Insert code here!"
    this.nodes = []
    this.edges = []
    this.stack = []
    var editor = ace.edit( "editor1" );
    editor.setValue(this.code);
  }

  // logs exception to console
  exceptionResponseHandler(msg){
    var obj = JSON.parse(msg)
    console.log(obj.exception)
  }

  // displays current expression
  currentExpressionResponseHandler(msg){
    var obj = JSON.parse(msg)
    if(obj.expr.length < 60){
      this.exprs.push({short:obj.expr, long:obj.expr})
    }
    else{
      this.exprs.push({short:obj.expr.slice(0,50) + "...", long:obj.expr})
    }

    var objDiv = document.getElementById("exprsDiv");
    objDiv.scrollIntoView(false)
  }

  // displays the evaluated expression and the value
  evaluatedExpressionResponseHandler(msg){
    var obj = JSON.parse(msg)
    if(obj.expr.length < 60){
      this.evaluatedExprs.push({short:obj.expr, long:obj.expr, val: obj.val})
    }
    else{
      this.evaluatedExprs.push({short:obj.expr.slice(0,50) + "...", long:obj.expr, val: obj.val})
    }
    var objEvalDiv = document.getElementById("evaluatedExprsDiv");
    objEvalDiv.scrollIntoView(false)
  }

  // updates CFG to given nodes and edges
  changeGraph(nodes,edges) {
    this.hierarchialGraph.nodes = nodes;
    this.hierarchialGraph.links = edges;

  }

  // parses raw data of websocket message and calls changeGraph()
  createCFG(nodes, edges) {

    var parsedNodes = []
    var parsedEdges = []
    for (var node of nodes){
      parsedNodes.push({
        id: node[0],
        label: node[1],
        dimension: {width: (node[1].length + 1) * 7, height:30}})
    }
    for (var edge of edges){
      parsedEdges.push({
        source: edge[0],
        target: edge[1],
        label: ''
      })      
    }
    this.changeGraph(parsedNodes,parsedEdges)
  }

  // returns structured stack  
  createStack(stackElems){
    var outputList = []
    for (var stackElemC = 0; stackElemC < stackElems.length; stackElemC ++) { 
      var stackElem = stackElems[stackElemC]
      var store:any = stackElem[0]
      var functionBodys: any = stackElem[1]
      for (var functionBodyC = 0; functionBodyC < functionBodys.length; functionBodyC ++){
        var functionBody = functionBodys[functionBodyC]
        for (var storeElemC = 0; storeElemC < store.length; storeElemC ++){
          var storeElem = store[storeElemC]
          var slicedFunctionBody = functionBody.slice(1,functionBody.length-1)
          if (storeElem[1].includes(slicedFunctionBody)){
            outputList.push(storeElem)
          }
        }
      }
    }
    return outputList
  }

  // returnes stack as list of StackElem
  parseStack(unparsedStack){

    var stackElems: StackElem [] = []
    for (var count = 0; count < unparsedStack.length; count ++){
      var stackElem = unparsedStack[count]
      var addr = stackElem[0]
      var reAddr = /^(\S*)(#\d*)\[]$/
      var match = reAddr.exec(addr)
      if(match != null){
        var name = match[1]
        var number = match[2]
        stackElems.push({name: name,addr: number})
      }
    }
    return stackElems
  }

  // creates unparsed store from raw data
  createStore(latestStore){
    var store: StoreElem[] = [];
    for (var count = 0; count < latestStore.length; count ++){
      
      var storeElem = latestStore[count]
      var addr = storeElem[0]
      var val = storeElem[1]

      var usualAddr = /^(\d*)\[\]/
      var hashAddr = /^(\S*#\d*)\[\]/
      var refs = /^Cons\({(\d*)\[]},\{(\d*)\[]}\)/

      var matchUsualAddr = usualAddr.exec(addr)
      if(matchUsualAddr != null){
        var objAddr = matchUsualAddr[1]
        var objVariableName = this.getVarNameFromEnv(objAddr)
        var matchRefs = refs.exec(val)
        if(matchRefs != null){
          var objRefs = [matchRefs[1],matchRefs[2]]
          store.push({addr : objAddr, refs: objRefs, lastElem: false, closure: false, val:val, variableName: objVariableName})
        }
        else{
          var objVal = val
          store.push({addr : objAddr, val: objVal, lastElem: true, closure: false, variableName: objVariableName})
        }
      }

      var matchHashAddr = hashAddr.exec(addr)
      if(matchHashAddr != null){
        var objAddr = matchHashAddr[1]
        var objVariableName = this.getVarNameFromEnv(objAddr)
        var matchRefs = refs.exec(val)
        if(matchRefs != null){
          var objRefs = [matchRefs[1],matchRefs[2]]
          store.push({addr : objAddr, refs: objRefs, lastElem: false, closure: true, val: val, variableName: objVariableName})
        }
        else{
          var objVal = val
          store.push({addr : objAddr, val: objVal, lastElem: true, closure: true, variableName: objVariableName})
        }
      }
    }
    return store
  }

  // returns variable name of environment element to corresponding address
  getVarNameFromEnv(objAddr){
    var env = this.latestEnv;
    for (var envElem of env) {
      if(objAddr == envElem.addr){
        return (envElem.var)
      }
    }
    return ""
  }

  // parses environment to list of EnvElem
  parseEnv(unparsedEnv){
    var envElems: EnvElem [] = []
    for (var count = 0; count < unparsedEnv.length; count ++){
      var envElem = unparsedEnv[count]
      var name = envElem[0]
      var addr = envElem[1]
      name = name.slice(1,name.length-1)
      addr = addr.slice(0,addr.length-2)
      envElems.push({var: name, addr:addr})
    }
    return envElems
  }

  // updates graph and resolved value of chosen store element
  generateDereferencedStoreElem(storeElem: StoreElem){
    console.log(storeElem)
    this.storeGraphNodes = []
    this.storeGraphEdges = []
    this.derefStoreElemAddress = storeElem.addr
    this.generateDereferencedGraph(storeElem)
    this.changeStoreGraph()
    this.derefStoreElem = this.generateDereferencedValue(storeElem)
  }

  // create resolved graph of store element
  generateDereferencedGraph(storeElem: StoreElem){
    if (storeElem.lastElem){
      if(!this.checkIfNodeExists(storeElem.addr)){
        this.storeGraphNodes.push({
          id: storeElem.addr,
          label: storeElem.addr,
          dimension: {width: (storeElem.addr.length + 1) * 7, height:30}})
        this.storeGraphNodes.push({
          id: storeElem.val + storeElem.addr,
          label: storeElem.val,
          dimension: {width: (storeElem.val.length + 1) * 7, height:30}})
        this.storeGraphEdges.push({
          source: storeElem.addr,
          target: storeElem.val + storeElem.addr,
          label: ''
        })
      }
      else{
        this.storeGraphNodes.push({
          id: storeElem.val + storeElem.addr,
          label: storeElem.val,
          dimension: {width: (storeElem.val.length + 1) * 7, height:30}})
        this.storeGraphEdges.push({
          source: storeElem.addr,
          target: storeElem.val + storeElem.addr,
          label: ''})
      }
    }
    if(!storeElem.lastElem){
      if(!this.checkIfNodeExists(storeElem.addr)){
        this.storeGraphNodes.push({
          id: "160",
          label: storeElem.addr,
          dimension: {width: (storeElem.addr.length + 1) * 7, height:30}})
        var firstRefAddr = storeElem.refs[0]
        this.storeGraphNodes.push({
          id: firstRefAddr,
          label: firstRefAddr,
          dimension: {width: (firstRefAddr.length + 1) * 7, height:30}})
        var secondRefAddr = storeElem.refs[1]
        this.storeGraphNodes.push({
          id: secondRefAddr,
          label: secondRefAddr,
          dimension: {width: (secondRefAddr.length + 1) * 7, height:30}})
        this.storeGraphEdges.push({
          source: "160",
          target: firstRefAddr,
          label: 'Cons'
        })
        this.storeGraphEdges.push({
          source: "160",
          target: secondRefAddr,
          label: 'Cons'
        })
      }
      else{
        var firstRefAddr = storeElem.refs[0]
        this.storeGraphNodes.push({
          id: firstRefAddr,
          label: firstRefAddr,
          dimension: {width: (firstRefAddr.length + 1) * 7, height:30}})
        var secondRefAddr = storeElem.refs[1]
        this.storeGraphNodes.push({
          id: secondRefAddr,
          label: secondRefAddr,
          dimension: {width: (secondRefAddr.length + 1) * 7, height:30}})
        this.storeGraphEdges.push({
          source: storeElem.addr,
          target: firstRefAddr,
          label: 'Cons'
        })
        this.storeGraphEdges.push({
          source: storeElem.addr,
          target: secondRefAddr,
          label: 'Cons'
        })
      }
      var firstRefStoreElem = this.getObjectFromStore(storeElem.refs[0]) 
      var secondRefStoreElem = this.getObjectFromStore(storeElem.refs[1]) 

      this.generateDereferencedGraph(firstRefStoreElem)
      this.generateDereferencedGraph(secondRefStoreElem)


    }


  }

  // checks if addr exists in current nodes of resolved store element graph
  checkIfNodeExists(addr){
    for(var node of this.storeGraphNodes){
      if(node.id == addr){
        return true
      }
    }
    return false
  }

  // updates resolved graph of store element
  changeStoreGraph() {
    this.hierarchialGraphStore.nodes = this.storeGraphNodes;
    this.hierarchialGraphStore.links = this.storeGraphEdges;

  }

  // returns object from store by address
  getObjectFromStore(addr){
    for(var storeElem of this.latestStore){
      if(storeElem.addr == addr){
        return storeElem
      }
    }
  }

  // recursively generates resolved value of store element
  generateDereferencedValue(storeElem: StoreElem){
    if(storeElem.lastElem){
      return storeElem.val
    }
    var firstRefStoreElem = this.getObjectFromStore(storeElem.refs[0])
    var secondRefStoreElem = this.getObjectFromStore(storeElem.refs[1])
    return (this.generateDereferencedValue(firstRefStoreElem) + "," + this.generateDereferencedValue(secondRefStoreElem))
  }


}
