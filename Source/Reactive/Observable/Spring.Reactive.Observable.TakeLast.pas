{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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
  TTakeLast<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
    fCount: Integer;
//    fDuration: TTimeSpan; // TODO implement interval
//    fScheduler: IScheduler;
    fLoopScheduler: IScheduler;
    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      private
        fParent: TTakeLast<TSource>;
        fQueue: IQueue<TSource>;
        fSubscription: ISingleAssignmentDisposable;
        fLoop: ISingleAssignmentDisposable;
        procedure LoopRec(const recurse: Action);
      public
        constructor Create(const parent: TTakeLast<TSource>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
        procedure OnNext(const value: TSource);
        procedure OnCompleted;
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>; const count: Integer;
      const loopScheduler: IScheduler);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TTakeLast<TSource>'}

constructor TTakeLast<TSource>.Create(const source: IObservable<TSource>;
  const count: Integer; const loopScheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fCount := count;
  fLoopScheduler := loopScheduler;
end;

function TTakeLast<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TTakeLast<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run;
end;

{$ENDREGION}


{$REGION 'TTakeLast<TSource>.TSink'}

constructor TTakeLast<TSource>.TSink.Create(const parent: TTakeLast<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
  fQueue := TCollections.CreateQueue<TSource>;
end;

destructor TTakeLast<TSource>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TTakeLast<TSource>.TSink.LoopRec(const recurse: Action);
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

function TTakeLast<TSource>.TSink.Run: IDisposable;
begin
  fSubscription := TSingleAssignmentDisposable.Create;
  fLoop := TSingleAssignmentDisposable.Create;
  fSubscription.Disposable := fParent.fSource.Subscribe(Self);
  Result := TStableCompositeDisposable.Create(fSubscription, fLoop);
end;

procedure TTakeLast<TSource>.TSink.OnNext(const value: TSource);
begin
  fQueue.Enqueue(value);
  if fQueue.Count > fParent.fCount then
    fQueue.Dequeue;
end;

procedure TTakeLast<TSource>.TSink.OnCompleted;
begin
  fSubscription.Dispose;
  fLoop.Disposable := fParent.fLoopScheduler.Schedule(LoopRec);
end;

{$ENDREGION}


end.
