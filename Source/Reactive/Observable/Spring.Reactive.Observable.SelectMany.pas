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

unit Spring.Reactive.Observable.SelectMany;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSelectMany<TSource,  TResult> = class
  public type
    TObservableSelector = class(TProducer<TResult>)
    protected
      fSource: IObservable<TSource>;
      fSelector: Func<TSource, IObservable<TResult>>;
    private type
      TSink = class(TSink<TResult>, IObserver<TSource>)
      private
        fSelector: Func<TSource, IObservable<TResult>>;
        fSourceSubscription: ISingleAssignmentDisposable;
        fGroup: ICompositeDisposable;
        fIsStopped: Boolean;
        type
          TInnerObserver = class(TDisposableObject, IObserver<TResult>)
          private
            fParent: TSink;
            fDisposable: IDisposable;
          public
            constructor Create(const parent: TSink; const disposable: IDisposable);
            destructor Destroy; override;
            procedure OnNext(const value: TResult);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
          end;
      protected
        procedure DoCompleted; virtual;
        procedure SubscribeInner(const inner: IObservable<TResult>);
      public
        constructor Create(
          const parent: TObservableSelector;
          const observer: IObserver<TResult>;
          const cancel: IDisposable);
        function Run(const source: IObservable<TSource>): IDisposable;
        procedure OnNext(const value: TSource);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
    protected
      function CreateSink(
        const observer: IObserver<TResult>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(
        const source: IObservable<TSource>;
        const selector: Func<TSource, IObservable<TResult>>);
    end;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TSelectMany<TSource, TResult>.TObservableSelector'}

constructor TSelectMany<TSource, TResult>.TObservableSelector.Create(
  const source: IObservable<TSource>;
  const selector: Func<TSource, IObservable<TResult>>);
begin
  inherited Create;
  fSource := source;
  fSelector := selector;
end;

function TSelectMany<TSource, TResult>.TObservableSelector.CreateSink(
  const observer: IObserver<TResult>; const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TSelectMany<TSource, TResult>.TObservableSelector.Run(
  const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(fSource);
end;

{$ENDREGION}


{$REGION 'TSelectMany<TSource, TResult>.TObservableSelector.TSink'}

constructor TSelectMany<TSource, TResult>.TObservableSelector.TSink.Create(
  const parent: TObservableSelector; const observer: IObserver<TResult>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fSourceSubscription := TSingleAssignmentDisposable.Create;
  fGroup := TCompositeDisposable.Create([]);
  fSelector := parent.fSelector;
  fGroup.Add(fSourceSubscription);
end;

function TSelectMany<TSource, TResult>.TObservableSelector.TSink.Run(
  const source: IObservable<TSource>): IDisposable;
begin
  fIsStopped := False;
  fSourceSubscription.Disposable := source.Subscribe(Self);
  Result := fGroup;
end;

procedure TSelectMany<TSource, TResult>.TObservableSelector.TSink.OnNext(
  const value: TSource);
var
  inner: IObservable<TResult>;
begin
  try
    inner := fSelector(value);
  except
    on e: Exception do
    begin
      MonitorEnter(Self);
      try
        Observer.OnError(e);
        Dispose
      finally
        MonitorExit(Self);
      end;
      Exit;
    end;
  end;

  SubscribeInner(inner);
end;

procedure TSelectMany<TSource, TResult>.TObservableSelector.TSink.OnError(
  const error: Exception);
begin
  MonitorEnter(Self);
  try
    Observer.OnError(error);
    Dispose
  finally
    MonitorExit(Self);
  end;
end;

procedure TSelectMany<TSource, TResult>.TObservableSelector.TSink.OnCompleted;
begin
  DoCompleted;
end;

procedure TSelectMany<TSource, TResult>.TObservableSelector.TSink.DoCompleted;
begin
  fIsStopped := True;
  if fGroup.Count = 1 then
  begin
    MonitorEnter(Self);
    try
      Observer.OnCompleted;
      Dispose;
    finally
      MonitorExit(Self);
    end;
  end
  else
    fSourceSubscription.Dispose;
end;

procedure TSelectMany<TSource, TResult>.TObservableSelector.TSink.SubscribeInner(
  const inner: IObservable<TResult>);
var
  innerSubscription: ISingleAssignmentDisposable;
begin
  innerSubscription := TSingleAssignmentDisposable.Create;
  fGroup.Add(innerSubscription);
  innerSubscription.Disposable := inner.Subscribe(TInnerObserver.Create(Self, innerSubscription));
end;

{$ENDREGION}


{$REGION 'TSelectMany<TSource, TResult>.TObservableSelector.TSink.TInnerObserver'}

constructor TSelectMany<TSource, TResult>.TObservableSelector.TSink.TInnerObserver.Create(
  const parent: TSink; const disposable: IDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fDisposable := disposable;
end;

destructor TSelectMany<TSource, TResult>.TObservableSelector.TSink.TInnerObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSelectMany<TSource, TResult>.TObservableSelector.TSink.TInnerObserver.OnNext(
  const value: TResult);
begin
  MonitorEnter(fParent);
  try
    fParent.Observer.OnNext(value);
  finally
    MonitorExit(fParent);
  end;
end;

procedure TSelectMany<TSource, TResult>.TObservableSelector.TSink.TInnerObserver.OnError(
  const error: Exception);
begin
  MonitorEnter(fParent);
  try
    fParent.Observer.OnError(error);
    fParent.Dispose;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TSelectMany<TSource, TResult>.TObservableSelector.TSink.TInnerObserver.OnCompleted;
begin
  fParent.fGroup.Remove(fDisposable);
  if fParent.fIsStopped and (fParent.fGroup.Count = 1) then
  begin
    MonitorEnter(fParent);
    try
      fParent.Observer.OnCompleted;
      fParent.Dispose;
    finally
      MonitorExit(fParent);
    end;
  end;
end;

{$ENDREGION}


end.
