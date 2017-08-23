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

unit Spring.Reactive.Observable.TakeUntil;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TTakeUntil<TSource, TOther> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
    fOther: IObservable<TOther>;

    type
      TSink = class(TSink<TSource>)
      private
        fParent: TTakeUntil<TSource, TOther>;

        type
          TSourceObserver = class(TInterfacedObject, IDisposable, IObserver<TSource>)
          private
            fParent: TSink;
            fOpen: Boolean;
          public
            constructor Create(const parent: TSink);
            destructor Destroy; override;
            procedure Dispose;
            procedure OnNext(const value: TSource);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
          end;

          TOtherObserver = class(TInterfacedObject, IDisposable, IObserver<TOther>)
          private
            fParent: TSink;
            fSourceObserver: TSourceObserver;
            fSubscription: ISingleAssignmentDisposable;
            procedure SetDisposable(const Value: IDisposable);
          public
            constructor Create(const parent: TSink; const sourceObserver: TSourceObserver);
            destructor Destroy; override;
            procedure Dispose;
            procedure OnNext(const value: TOther);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
            property Disposable: IDisposable write SetDisposable;
          end;
      public
        constructor Create(const parent: TTakeUntil<TSource, TOther>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<TSource>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>;
      const other: IObservable<TOther>);
  end;

implementation

uses
  Spring.Reactive.Disposables,
  Spring.Reactive.Internal.Observers;


{$REGION 'TTakeUntil<TSource, TOther>'}

constructor TTakeUntil<TSource, TOther>.Create(
  const source: IObservable<TSource>; const other: IObservable<TOther>);
begin
  inherited Create;
  fSource := source;
  fOther := other;
end;

function TTakeUntil<TSource, TOther>.Run(const observer: IObserver<TSource>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TTakeUntil<TSource, TOther>.TSink'}

constructor TTakeUntil<TSource, TOther>.TSink.Create(
  const parent: TTakeUntil<TSource, TOther>; const observer: IObserver<TSource>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TTakeUntil<TSource, TOther>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

function TTakeUntil<TSource, TOther>.TSink.Run: IDisposable;
var
  sourceObserver: TSourceObserver;
  otherObserver: TOtherObserver;
  sourceSubscription: IDisposable;
  otherSubscription: IDisposable;
begin
  sourceObserver := TSourceObserver.Create(Self);
  otherObserver := TOtherObserver.Create(Self, sourceObserver);

  otherSubscription := fParent.fOther.Subscribe(otherObserver);
  otherObserver.Disposable := otherSubscription;

  sourceSubscription := fParent.fSource.Subscribe(sourceObserver);

  Result := TStableCompositeDisposable.Create(otherSubscription, sourceSubscription);
end;

{$ENDREGION}


{$REGION 'TTakeUntil<TSource, TOther>.TSink.TSourceObserver'}

constructor TTakeUntil<TSource, TOther>.TSink.TSourceObserver.Create(
  const parent: TSink);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
end;

destructor TTakeUntil<TSource, TOther>.TSink.TSourceObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TTakeUntil<TSource, TOther>.TSink.TSourceObserver.Dispose;
begin
end;

procedure TTakeUntil<TSource, TOther>.TSink.TSourceObserver.OnCompleted;
begin
  MonitorEnter(fParent);
  try
    fParent.Observer.OnCompleted;
    fParent.Dispose;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TTakeUntil<TSource, TOther>.TSink.TSourceObserver.OnError(
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

procedure TTakeUntil<TSource, TOther>.TSink.TSourceObserver.OnNext(
  const value: TSource);
begin
  if fOpen then
    fParent.Observer.OnNext(value)
  else
  begin
    MonitorEnter(fParent);
    try
      fParent.Observer.OnNext(value);
    finally
      MonitorExit(fParent);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TTakeUntil<TSource, TOther>.TSink.TOtherObserver'}

constructor TTakeUntil<TSource, TOther>.TSink.TOtherObserver.Create(
  const parent: TSink; const sourceObserver: TSourceObserver);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fSourceObserver := sourceObserver;
  fSubscription := TSingleAssignmentDisposable.Create;
end;

destructor TTakeUntil<TSource, TOther>.TSink.TOtherObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TTakeUntil<TSource, TOther>.TSink.TOtherObserver.Dispose;
begin
end;

procedure TTakeUntil<TSource, TOther>.TSink.TOtherObserver.OnCompleted;
begin
  MonitorEnter(fParent);
  try
    fSourceObserver.fOpen := True;
    fSubscription.Dispose;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TTakeUntil<TSource, TOther>.TSink.TOtherObserver.OnError(
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

procedure TTakeUntil<TSource, TOther>.TSink.TOtherObserver.OnNext(
  const value: TOther);
begin
  MonitorEnter(fParent);
  try
    fParent.Observer.OnCompleted;
    fParent.Dispose;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TTakeUntil<TSource, TOther>.TSink.TOtherObserver.SetDisposable(
  const Value: IDisposable);
begin
  fSubscription.Disposable := value;
end;

{$ENDREGION}


end.
