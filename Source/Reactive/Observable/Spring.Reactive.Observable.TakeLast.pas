{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Reactive.Observable.TakeLast;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TTakeLast<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fCount: Integer;
//    fDuration: TTimeSpan; // TODO implement interval
//    fScheduler: IScheduler;
    fLoopScheduler: IScheduler;
    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TTakeLast<T>;
        fQueue: IQueue<T>;
        fSubscription: ISingleAssignmentDisposable;
        fLoop: ISingleAssignmentDisposable;
        procedure LoopRec(const recurse: Action);
      public
        constructor Create(const parent: TTakeLast<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
        procedure OnNext(const value: T);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; const count: Integer;
      const loopScheduler: IScheduler);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TTakeLast<T>'}

constructor TTakeLast<T>.Create(const source: IObservable<T>;
  const count: Integer; const loopScheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fCount := count;
  fLoopScheduler := loopScheduler;
end;

function TTakeLast<T>.Run(const observer: IObserver<T>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
//  if not Assigned(fScheduler) then
//  begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
//  end
//  else
//  begin
//
//  end;
end;

{$ENDREGION}


{$REGION 'TTakeLast<T>.TSink'}

constructor TTakeLast<T>.TSink.Create(const parent: TTakeLast<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
  fQueue := TCollections.CreateQueue<T>;
end;

destructor TTakeLast<T>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TTakeLast<T>.TSink.LoopRec(const recurse: Action);
begin
  if fQueue.Count > 0 then
  begin
    Observer.OnNext(fQueue.Dequeue);
    recurse;
  end
  else
  begin
    Observer.OnCompleted;
    Dispose;
  end;
end;

function TTakeLast<T>.TSink.Run: IDisposable;
begin
  fSubscription := TSingleAssignmentDisposable.Create;
  fLoop := TSingleAssignmentDisposable.Create;
  fSubscription.Disposable := fParent.fSource.Subscribe(Self);
  Result := TStableCompositeDisposable.Create(fSubscription, fLoop);
end;

procedure TTakeLast<T>.TSink.OnNext(const value: T);
begin
  fQueue.Enqueue(value);
  if fQueue.Count > fParent.fCount then
    fQueue.Dequeue;
end;

procedure TTakeLast<T>.TSink.OnCompleted;
begin
  fSubscription.Dispose;
  fLoop.Disposable := fParent.fLoopScheduler.Schedule(LoopRec);
end;

{$ENDREGION}


end.
