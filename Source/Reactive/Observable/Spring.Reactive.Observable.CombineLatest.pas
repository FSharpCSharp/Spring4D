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

unit Spring.Reactive.Observable.CombineLatest;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TCombineLatest<TFirst, TSecond, TResult> = class(TProducer<TResult>)
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

          TFirstObserver = class(TInterfacedObject, IObserver<TFirst>)
          strict private
            fParent: TSink;
            fDisposable: IDisposable;
            fOther: TSecondObserver;
            fHasValue: Boolean;
            fValue: TFirst;
            fDone: Boolean;
          private
            property Other: TSecondObserver write fOther;
            property HasValue: Boolean read fHasValue;
            property Value: TFirst read fValue;
            property Done: Boolean read fDone;
          public
            constructor Create(const parent: TSink; const disposable: IDisposable);
            destructor Destroy; override;
            procedure Dispose;

            procedure OnNext(const value: TFirst);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
          end;

          TSecondObserver = class(TInterfacedObject, IObserver<TSecond>)
          strict private
            fParent: TSink;
            fDisposable: IDisposable;
            fOther: TFirstObserver;
            fHasValue: Boolean;
            fValue: TSecond;
            fDone: Boolean;
          private
            property Other: TFirstObserver write fOther;
            property HasValue: Boolean read fHasValue;
            property Value: TSecond read fValue;
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
        constructor Create(const resultSelector: Func<TFirst, TSecond, TResult>;
          const observer: IObserver<TResult>; const cancel: IDisposable);
        function Run(const first: IObservable<TFirst>;
          const second: IObservable<TSecond>): IDisposable;
      end;
  protected
    function CreateSink(const observer: IObserver<TResult>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const first: IObservable<TFirst>;
      const second: IObservable<TSecond>;
      const resultSelector: Func<TFirst, TSecond, TResult>);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TCombineLatest<TFirst, TSecond, TResult>'}

constructor TCombineLatest<TFirst, TSecond, TResult>.Create(
  const first: IObservable<TFirst>; const second: IObservable<TSecond>;
  const resultSelector: Func<TFirst, TSecond, TResult>);
begin
  inherited Create;
  fFirst := first;
  fSecond := second;
  fResultSelector := resultSelector;
end;

function TCombineLatest<TFirst, TSecond, TResult>.CreateSink(
  const observer: IObserver<TResult>; const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(fResultSelector, observer, cancel);
end;

function TCombineLatest<TFirst, TSecond, TResult>.Run(
  const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(fFirst, fSecond);
end;

{$ENDREGION}


{$REGION 'TCombineLatest<TFirst, TSecond, TResult>.TSink'}

constructor TCombineLatest<TFirst, TSecond, TResult>.TSink.Create(
  const resultSelector: Func<TFirst, TSecond, TResult>;
  const observer: IObserver<TResult>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fResultSelector := resultSelector;
end;

function TCombineLatest<TFirst, TSecond, TResult>.TSink.Run(
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

  Result := TStableCompositeDisposable.Create(firstSubscription, secondSubscription);
end;

{$ENDREGION}


{$REGION 'TCombineLatest<TFirst, TSecond, TResult>.TSink.TFirstObserver'}

constructor TCombineLatest<TFirst, TSecond, TResult>.TSink.TFirstObserver.Create(
  const parent: TSink; const disposable: IDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fDisposable := disposable;
end;

destructor TCombineLatest<TFirst, TSecond, TResult>.TSink.TFirstObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TCombineLatest<TFirst, TSecond, TResult>.TSink.TFirstObserver.Dispose;
begin
end;

procedure TCombineLatest<TFirst, TSecond, TResult>.TSink.TFirstObserver.OnNext(
  const value: TFirst);
var
  res: TResult;
begin
  MonitorEnter(fParent);
  try
    fHasValue := True;
    fValue := value;

    if fOther.HasValue then
    begin
      res := Default(TResult);
      try
        res := fParent.fResultSelector(value, fOther.Value);
      except
        on e: Exception do
        begin
          with fParent do // TODO consider calling fParent.OnError(e);
          begin
            Observer.OnError(e);
            Dispose;
          end;
          Exit;
        end;
      end;

      fParent.Observer.OnNext(res);
    end
    else if fOther.Done then
    begin
      with fParent do // TODO consider calling fParent.OnCompleted
      begin
        Observer.OnCompleted;
        Dispose;
      end;
      Exit;
    end;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TCombineLatest<TFirst, TSecond, TResult>.TSink.TFirstObserver.OnError(
  const error: Exception);
begin
  MonitorEnter(fParent);
  try
    with fParent do // TODO consider calling fParent.OnError(error)
    begin
      Observer.OnError(error);
      Dispose;
    end;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TCombineLatest<TFirst, TSecond, TResult>.TSink.TFirstObserver.OnCompleted;
begin
  MonitorEnter(fParent);
  try
    fDone := True;

    if fOther.Done then
    begin
      with fParent do // TODO consider calling fParent.OnCompleted
      begin
        Observer.OnCompleted;
        Dispose;
        Exit;
      end;
    end
    else
      Dispose;
  finally
    MonitorExit(fParent);
  end;
end;

{$ENDREGION}


{$REGION 'TCombineLatest<TFirst, TSecond, TResult>.TSink.TSecondObserver'}

constructor TCombineLatest<TFirst, TSecond, TResult>.TSink.TSecondObserver.Create(
  const parent: TSink; const disposable: IDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fDisposable := disposable;
end;

destructor TCombineLatest<TFirst, TSecond, TResult>.TSink.TSecondObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TCombineLatest<TFirst, TSecond, TResult>.TSink.TSecondObserver.Dispose;
begin
end;

procedure TCombineLatest<TFirst, TSecond, TResult>.TSink.TSecondObserver.OnNext(
  const value: TSecond);
var
  res: TResult;
begin
  MonitorEnter(fParent);
  try
    fHasValue := True;
    fValue := value;

    if fOther.HasValue then
    begin
      res := Default(TResult);
      try
        res := fParent.fResultSelector(fOther.Value, value);
      except
        on e: Exception do
        begin
          with fParent do // TODO consider calling fParent.OnError(e);
          begin
            Observer.OnError(e);
            Dispose;
          end;
          Exit;
        end;
      end;

      fParent.Observer.OnNext(res);
    end
    else if fOther.Done then
    begin
      with fParent do // TODO consider calling fParent.OnCompleted
      begin
        Observer.OnCompleted;
        Dispose;
      end;
      Exit;
    end;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TCombineLatest<TFirst, TSecond, TResult>.TSink.TSecondObserver.OnError(
  const error: Exception);
begin
  MonitorEnter(fParent);
  try
    with fParent do // TODO consider calling fParent.OnError(error)
    begin
      Observer.OnError(error);
      Dispose;
    end;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TCombineLatest<TFirst, TSecond, TResult>.TSink.TSecondObserver.OnCompleted;
begin
  MonitorEnter(fParent);
  try
    fDone := True;

    if fOther.Done then
    begin
      with fParent do // TODO consider calling fParent.OnCompleted
      begin
        Observer.OnCompleted;
        Dispose;
        Exit;
      end;
    end
    else
      Dispose;
  finally
    MonitorExit(fParent);
  end;
end;

{$ENDREGION}


end.
