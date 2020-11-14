import { Injectable } from '@angular/core';
import { Observable, Subject } from 'rxjs';
import { WebsocketService } from './websocket.service';
import { environment } from '../environments/environment'
import { map } from "rxjs/operators";


export interface Message {
  tag: string,
  path: String
}

export interface StartDebuggerRequest {
  tag: string,
  code: String
}

export interface ContinueRequest {
  tag: string
}

export interface RefreshRequest {
  tag: string
}

export interface StepRequest {
  tag: string
}

@Injectable({
  providedIn: 'root'
})


export class CommunicationService {
  public messages: Subject<Message>;
  public startDebuggerRequests: Subject<StartDebuggerRequest>;
  public continueRequests: Subject<ContinueRequest>;
  public refreshRequests: Subject<RefreshRequest>;
  public stepRequests: Subject<StepRequest>;

  constructor(private wsService: WebsocketService) {

    this.messages = <Subject<Message>>wsService
    .connect(environment.CHAT_URL)
    .pipe(map((response: MessageEvent): Message => {
      let data = JSON.parse(response.data);
      
      return data
    }))

    this.startDebuggerRequests = <Subject<StartDebuggerRequest>>wsService
    .connect(environment.CHAT_URL)
    .pipe(map((response: MessageEvent): StartDebuggerRequest => {
      let data = JSON.parse(response.data);
      
      return data
    }))

    this.continueRequests = <Subject<ContinueRequest>>wsService
    .connect(environment.CHAT_URL)
    .pipe(map((response: MessageEvent): ContinueRequest => {
      let data = JSON.parse(response.data);
      
      return data
    }))

    this.refreshRequests = <Subject<RefreshRequest>>wsService
    .connect(environment.CHAT_URL)
    .pipe(map((response: MessageEvent): RefreshRequest => {
      let data = JSON.parse(response.data);
      
      return data
    }))

    this.stepRequests = <Subject<StepRequest>>wsService
    .connect(environment.CHAT_URL)
    .pipe(map((response: MessageEvent): StepRequest => {
      let data = JSON.parse(response.data);
      
      return data
    }))
   
   }


}
