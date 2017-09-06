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

unit Spring.Reactive.Observable.Zip;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TZip<TFirst, TSecond, TResult> = class
  public type
    TObservable = class(TProducer<TResult>)
    private
      fFirst: IObservable<TFirst>;
      fSecond: IObservable<TSecond>;
      fResultSelector: Func<TFirst, TSecond, TResult>;

      type
        TSink = class(TSink<TResult>)
        private
          fResultSelector: Func<TFirst, TSecond, TResult>;

          type
            TSecondObserver = class;

            TFirstObserver = class(TInterfacedObject, IObserver<TFirst>, IDisposable)
            strict private
              fParent: TSink;
              fDisposable: IDisposable;
              fOther: TSecondObserver;
              fQueue: IQueue<TFirst>;
              fDone: Boolean;
            private
              property Other: TSecondObserver write fOther;
              property Queue: IQueue<TFirst> read fQueue;
              property Done: Boolean read fDone;
            public
              constructor Create(const parent: TSink; const disposable: IDisposable);
              destructor Destroy; override;
              procedure Dispose;

              procedure OnNext(const value: TFirst);
              procedure OnError(const error: Exception);
              procedure OnCompleted;
            end;

            TSecondObserver = class(TInterfacedObject, IObserver<TSecond>, IDisposable)
            strict private
              fParent: TSink;
              fDisposable: IDisposable;
              fOther: TFirstObserver;
              fQueue: IQueue<TSecond>;
              fDone: Boolean;
            private
              property Other: TFirstObserver write fOther;
              property Queue: IQueue<TSecond> read fQueue;
              property Done: Boolean read fDone;
            public
              constructor Create(const parent: TSink; const disposable: IDisposable);
              destructor Destroy; override;
              procedure Dispose;

              procedure OnNext(const value: TSecond);
              procedure OnError(const error: Exception);
              procedure OnCompleted;
            end;
        public
          constructor Create(
            const resultSelector: Func<TFirst, TSecond, TResult>;
            const observer: IObserver<TResult>;
            const cancel: IDisposable);
          function Run(
            const first: IObservable<TFirst>;
            const second: IObservable<TSecond>): IDisposable;
        end;
    protected
      function CreateSink(const observer: IObserver<TResult>; const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(
        const first: IObservable<TFirst>;
        const second: IObservable<TSecond>;
        const resultSelector: Func<TFirst, TSecond, TResult>);
    end;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TZip<TFirst, TSecond, TResult>.TObservable'}

constructor TZip<TFirst, TSecond, TResult>.TObservable.Create(
  const first: IObservable<TFirst>; const second: IObservable<TSecond>;
  const resultSelector: Func<TFirst, TSecond, TResult>);
begin
  inherited Create;
  fFirst := first;
  fSecond := second;
  fResultSelector := resultSelector;
end;

function TZip<TFirst, TSecond, TResult>.TObservable.CreateSink(
  const observer: IObserver<TResult>; const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(fResultSelector, observer, cancel);
end;

function TZip<TFirst, TSecond, TResult>.TObservable.Run(
  const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(fFirst, fSecond);
end;

{$ENDREGION}


{$REGION 'TZip<TFirst, TSecond, TResult>.TObservable.TSink'}

constructor TZip<TFirst, TSecond, TResult>.TObservable.TSink.Create(
  const resultSelector: Func<TFirst, TSecond, TResult>;
  const observer: IObserver<TResult>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fResultSelector := resultSelector;
end;

function TZip<TFirst, TSecond, TResult>.TObservable.TSink.Run(
  const first: IObservable<TFirst>;
  const second: IObservable<TSecond>): IDisposable;
var
  firstSubscription: ISingleAssignmentDisposable;
  secondSubscription: ISingleAssignmentDisposable;
  firstObserver: TFirstObserver;
  secondObserver: TSecondObserver;
begin
  firstSubscription := TSingleAssignmentDisposable.Create;
  secondSubscription := TSingleAssignmentDisposable.Create;

  firstObserver := TFirstObserver.Create(Self, firstSubscription);
  secondObserver := TSecondObserver.Create(Self, secondSubscription);

  firstObserver.Other := secondObserver;
  secondObserver.Other := firstObserver;

  firstSubscription.Disposable := first.Subscribe(firstObserver);
  secondSubscription.Disposable := second.Subscribe(secondObserver);

  Result := TStableCompositeDisposable.Create([
    firstSubscription, secondSubscription, firstObserver, secondObserver]);
end;

{$ENDREGION}


{$REGION 'TZip<TFirst, TSecond, TResult>.TObservable.TSink.TFirstObserver'}

constructor TZip<TFirst, TSecond, TResult>.TObservable.TSink.TFirstObserver.Create(
  const parent: TSink; const disposable: IDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fDisposable := disposable;
  fQueue := TCollections.CreateQueue<TFirst>;
end;

destructor TZip<TFirst, TSecond, TResult>.TObservable.TSink.TFirstObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TZip<TFirst, TSecond, TResult>.TObservable.TSink.TFirstObserver.Dispose;
begin
  fQueue.Clear;
end;

procedure TZip<TFirst, TSecond, TResult>.TObservable.TSink.TFirstObserver.OnNext(
  const value: TFirst);
var
  r: TSecond;
  res: TResult;
begin
  Lock(fParent);

  if fOther.Queue.Count > 0 then
  begin
    r := fOther.Queue.Dequeue;

    res := Default(TResult);
    try
      res := fParent.fResultSelector(value, r);
    except
      on e: Exception do
      begin
        fParent.Observer.OnError(e);
        fParent.Dispose;
        Exit;
      end;
    end;

    fParent.Observer.OnNext(res);
  end
  else
  begin
    if fOther.Done then
    begin
      fParent.Observer.OnCompleted;
      fParent.Dispose;
      Exit;
    end;

    fQueue.Enqueue(value);
  end;
end;

procedure TZip<TFirst, TSecond, TResult>.TObservable.TSink.TFirstObserver.OnError(
  const error: Exception);
begin
  Lock(fParent);

  fParent.Observer.OnError(error);
  fParent.Dispose;
end;

procedure TZip<TFirst, TSecond, TResult>.TObservable.TSink.TFirstObserver.OnCompleted;
begin
  Lock(fParent);

  fDone := True;

  if fOther.Done then
  begin
    fParent.Observer.OnCompleted;
    fParent.Dispose;
  end
  else
    Dispose;
end;

{$ENDREGION}


{$REGION 'TZip<TFirst, TSecond, TResult>.TObservable.TSink.TSecondObserver'}

constructor TZip<TFirst, TSecond, TResult>.TObservable.TSink.TSecondObserver.Create(
  const parent: TSink; const disposable: IDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fDisposable := disposable;
  fQueue := TCollections.CreateQueue<TSecond>;
end;

destructor TZip<TFirst, TSecond, TResult>.TObservable.TSink.TSecondObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TZip<TFirst, TSecond, TResult>.TObservable.TSink.TSecondObserver.Dispose;
begin
  fQueue.Clear;
end;

procedure TZip<TFirst, TSecond, TResult>.TObservable.TSink.TSecondObserver.OnNext(
  const value: TSecond);
var
  l: TFirst;
  res: TResult;
begin
  Lock(fParent);

  if fOther.Queue.Count > 0 then
  begin
    l := fOther.Queue.Dequeue;

    res := Default(TResult);
    try
      res := fParent.fResultSelector(l, value);
    except
      on e: Exception do
      begin
        fParent.Observer.OnError(e);
        fParent.Dispose;
        Exit;
      end;
    end;

    fParent.Observer.OnNext(res);
  end
  else
  begin
    if fOther.Done then
    begin
      fParent.Observer.OnCompleted;
      fParent.Dispose;
      Exit;
    end;

    fQueue.Enqueue(value);
  end;
end;

procedure TZip<TFirst, TSecond, TResult>.TObservable.TSink.TSecondObserver.OnError(
  const error: Exception);
begin
  Lock(fParent);

  fParent.Observer.OnError(error);
  fParent.Dispose;
end;

procedure TZip<TFirst, TSecond, TResult>.TObservable.TSink.TSecondObserver.OnCompleted;
begin
  Lock(fParent);

  fDone := True;

  if fOther.Done then
  begin
    fParent.Observer.OnCompleted;
    fParent.Dispose;
  end
  else
    Dispose;
end;

{$ENDREGION}


end.
