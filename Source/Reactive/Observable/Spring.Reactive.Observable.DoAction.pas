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

unit Spring.Reactive.Observable.DoAction;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TDoAction<TSource> = class
  public type
    TOnNext = class sealed(TProducer<TSource>)
    private
      fSource: IObservable<TSource>;
      fOnNext: Action<TSource>;

      type
        TSink = class sealed(TSink<TSource>, IObserver<TSource>)
        private
          fOnNext: Action<TSource>;
        public
          constructor Create(const onNext: Action<TSource>;
            const observer: IObserver<TSource>; const cancel: IDisposable);
          procedure OnNext(const value: TSource);
          procedure OnError(const error: Exception);
          procedure OnCompleted;
        end;
    protected
      function Run(const observer: IObserver<TSource>; const cancel: IDisposable;
        const setSink: Action<IDisposable>): IDisposable; override;
    public
      constructor Create(const source: IObservable<TSource>; const onNext: Action<TSource>);
    end;

    TObserver = class sealed(TProducer<TSource>)
    private
      fSource: IObservable<TSource>;
      fObserver: IObserver<TSource>;

      type
        TSink = class sealed(TSink<TSource>, IObserver<TSource>)
        private
          fDoObserver: IObserver<TSource>;
        public
          constructor Create(const doObserver: IObserver<TSource>;
            const observer: IObserver<TSource>; const cancel: IDisposable);
          procedure OnNext(const value: TSource);
          procedure OnError(const error: Exception);
          procedure OnCompleted;
        end;
    protected
      function Run(const observer: IObserver<TSource>; const cancel: IDisposable;
        const setSink: Action<IDisposable>): IDisposable; override;
    public
      constructor Create(const source: IObservable<TSource>; const observer: IObserver<TSource>);
    end;

    TActions = class sealed(TProducer<TSource>)
    private
      fSource: IObservable<TSource>;
      fOnNext: Action<TSource>;
      fOnError: Action<Exception>;
      fOnCompleted: Action;

      type
        TSink = class sealed(TSink<TSource>, IObserver<TSource>)
        private
          fParent: TActions;
        public
          constructor Create(const parent: TActions;
            const observer: IObserver<TSource>; const cancel: IDisposable);
          destructor Destroy; override;
          procedure OnNext(const value: TSource);
          procedure OnError(const error: Exception);
          procedure OnCompleted;
        end;
    protected
      function Run(const observer: IObserver<TSource>; const cancel: IDisposable;
        const setSink: Action<IDisposable>): IDisposable; override;
    public
      constructor Create(const source: IObservable<TSource>;
        const onNext: Action<TSource>;
        const onError: Action<Exception>;
        const onCompleted: Action);
    end;
  end;

implementation


{$REGION 'TDoAction<TSource>.TOnNext'}

constructor TDoAction<TSource>.TOnNext.Create(
  const source: IObservable<TSource>; const onNext: Action<TSource>);
begin
  inherited Create;
  fSource := source;
  fOnNext := onNext;
end;

function TDoAction<TSource>.TOnNext.Run(const observer: IObserver<TSource>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(fOnNext, observer, cancel);
  setSink(sink);
  Result := fSource.Subscribe(sink);
end;

{$ENDREGION}


{$REGION 'TDoAction<TSource>.TOnNext.TSink'}

constructor TDoAction<TSource>.TOnNext.TSink.Create(
  const onNext: Action<TSource>; const observer: IObserver<TSource>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fOnNext := onNext;
end;

procedure TDoAction<TSource>.TOnNext.TSink.OnNext(const value: TSource);
begin
  try
    fOnNext(value);
  except
    on ex: Exception do
    begin
      Observer.OnError(ex);
      Dispose;
      Exit;
    end;
  end;

  Observer.OnNext(value);
end;

procedure TDoAction<TSource>.TOnNext.TSink.OnError(const error: Exception);
begin
  Observer.OnError(error);
  Dispose;
end;

procedure TDoAction<TSource>.TOnNext.TSink.OnCompleted;
begin
  Observer.OnCompleted;
  Dispose;
end;

{$ENDREGION}


{$REGION 'TDoAction<TSource>.TObserver'}

constructor TDoAction<TSource>.TObserver.Create(
  const source: IObservable<TSource>; const observer: IObserver<TSource>);
begin
  inherited Create;
  fSource := source;
  fObserver := observer;
end;

function TDoAction<TSource>.TObserver.Run(const observer: IObserver<TSource>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(fObserver, observer, cancel);
  setSink(sink);
  Result := fSource.Subscribe(sink);
end;

{$ENDREGION}


{$REGION 'TDoAction<TSource>.TObserver.TSink' }

constructor TDoAction<TSource>.TObserver.TSink.Create(const doObserver,
  observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fDoObserver := doObserver;
end;

procedure TDoAction<TSource>.TObserver.TSink.OnNext(const value: TSource);
begin
  try
    fDoObserver.OnNext(value);
  except
    on ex: Exception do
    begin
      Observer.OnError(ex);
      Dispose;
      Exit;
    end;
  end;

  Observer.OnNext(value);
end;

procedure TDoAction<TSource>.TObserver.TSink.OnError(const error: Exception);
begin
  try
    fDoObserver.OnError(error);
  except
    on ex: Exception do
    begin
      Observer.OnError(ex);
      Dispose;
      Exit;
    end;
  end;

  Observer.OnError(error);
  Dispose;
end;

procedure TDoAction<TSource>.TObserver.TSink.OnCompleted;
begin
  try
    fDoObserver.OnCompleted;
  except
    on ex: Exception do
    begin
      Observer.OnError(ex);
      Dispose;
      Exit;
    end;
  end;

  Observer.OnCompleted;
  Dispose;
end;

{$ENDREGION}


{$REGION 'TDoAction<TSource>.TActions'}

constructor TDoAction<TSource>.TActions.Create(
  const source: IObservable<TSource>; const onNext: Action<TSource>;
  const onError: Action<Exception>; const onCompleted: Action);
begin
  inherited Create;
  fSource := source;
  fOnNext := onNext;
  fOnError := onError;
  fOnCompleted := onCompleted;
end;

function TDoAction<TSource>.TActions.Run(const observer: IObserver<TSource>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := fSource.Subscribe(sink);
end;

{$ENDREGION}


{$REGION 'TDoAction<TSource>.TActions.TSink'}

constructor TDoAction<TSource>.TActions.TSink.Create(const parent: TActions;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TDoAction<TSource>.TActions.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TDoAction<TSource>.TActions.TSink.OnNext(const value: TSource);
begin
  try
    fParent.fOnNext(value);
  except
    on ex: Exception do
    begin
      Observer.OnError(ex);
      Dispose;
      Exit;
    end;
  end;

  Observer.OnNext(value);
end;

procedure TDoAction<TSource>.TActions.TSink.OnError(const error: Exception);
begin
  try
    fParent.fOnError(error);
  except
    on ex: Exception do
    begin
      Observer.OnError(ex);
      Dispose;
      Exit;
    end;
  end;

  Observer.OnError(error);
  Dispose;
end;

procedure TDoAction<TSource>.TActions.TSink.OnCompleted;
begin
  try
    fParent.fOnCompleted();
  except
    on ex: Exception do
    begin
      Observer.OnError(ex);
      Dispose;
      Exit;
    end;
  end;

  Observer.OnCompleted;
  Dispose;
end;

{$ENDREGION}


end.
