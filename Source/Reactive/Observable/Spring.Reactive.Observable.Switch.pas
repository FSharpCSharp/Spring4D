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

unit Spring.Reactive.Observable.Switch;

interface

uses
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSwitch<TSource> = class(TProducer<TSource>)
  private
    fSources: IObservable<IObservable<TSource>>;

    type
      TSink = class(TSink<TSource>, IObserver<IObservable<TSource>>)
      private
        fSubscription: IDisposable;
        fInnerSubscription: ISerialDisposable;
        fIsStopped: Boolean;
        fLatest: UInt64;
        fHasLatest: Boolean;

        type
          TInnerObserver = class(TDisposableObject, IObserver<TSource>)
          private
            fParent: TSink;
            fId: UInt64;
            fDisposable: IDisposable;
          public
            constructor Create(const parent: TSink; id: UInt64;
              const disposable: IDisposable);
            destructor Destroy; override;
            procedure OnNext(const value: TSource);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
          end;
      public
        function Run(const parent: TSwitch<TSource>): IDisposable;
        procedure OnNext(const value: IObservable<TSource>);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const sources: IObservable<IObservable<TSource>>);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TSwitch<TSource>'}

constructor TSwitch<TSource>.Create(
  const sources: IObservable<IObservable<TSource>>);
begin
  inherited Create;
  fSources := sources;
end;

function TSwitch<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(observer, cancel);
end;

function TSwitch<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(Self);
end;

{$ENDREGION}


{$REGION 'TSwitch<TSource>.TSink'}

function TSwitch<TSource>.TSink.Run(
  const parent: TSwitch<TSource>): IDisposable;
var
  subscription: ISingleAssignmentDisposable;
begin
  fInnerSubscription := TSerialDisposable.Create;
  fIsStopped := False;
  fLatest := 0;
  fHasLatest := False;

  subscription := TSingleAssignmentDisposable.Create;
  fSubscription := subscription;
  subscription.Disposable := parent.fSources.Subscribe(Self);

  Result := TStableCompositeDisposable.Create(fSubscription, fInnerSubscription);
end;

procedure TSwitch<TSource>.TSink.OnNext(const value: IObservable<TSource>);
var
  id: UInt64;
  d: ISingleAssignmentDisposable;
begin
  id := 0;
  MonitorEnter(Self);
  try
    Inc(fLatest);
    id := fLatest; // unchecked
    fHasLatest := True;
  finally
    MonitorExit(Self);
  end;

  d := TSingleAssignmentDisposable.Create;
  fInnerSubscription.Disposable := d;
  d.Disposable := value.Subscribe(TInnerObserver.Create(Self, id, d) as IObserver<TSource>);
end;

procedure TSwitch<TSource>.TSink.OnError(const error: Exception);
begin
  MonitorEnter(Self);
  try
    Observer.OnError(error);
  finally
    MonitorExit(Self);
  end;
end;

procedure TSwitch<TSource>.TSink.OnCompleted;
begin
  MonitorEnter(Self);
  try
    fSubscription.Dispose;

    fIsStopped := True;
    if not fHasLatest then
    begin
      Observer.OnCompleted;
      Dispose;
    end;
  finally
    MonitorExit(Self);
  end;
end;

{$ENDREGION}


{$REGION 'TSwitch<TSource>.TSink.TInnerObserver'}

constructor TSwitch<TSource>.TSink.TInnerObserver.Create(const parent: TSink;
  id: UInt64; const disposable: IDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fId := id;
  fDisposable := disposable;
end;

destructor TSwitch<TSource>.TSink.TInnerObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSwitch<TSource>.TSink.TInnerObserver.OnNext(const value: TSource);
begin
  MonitorEnter(fParent);
  try
    if fParent.fLatest = fId then
      fParent.Observer.OnNext(value);
  finally
    MonitorExit(fParent);
  end;
end;

procedure TSwitch<TSource>.TSink.TInnerObserver.OnError(const error: Exception);
begin
  MonitorEnter(fParent);
  try
    fDisposable.Dispose;

    if fParent.fLatest = fId then
    begin
      fParent.Observer.OnError(error);
      fParent.Dispose;
    end;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TSwitch<TSource>.TSink.TInnerObserver.OnCompleted;
begin
  MonitorEnter(fParent);
  try
    fDisposable.Dispose;

    if fParent.fLatest = fId then
    begin
      fParent.fHasLatest := False;

      if fParent.fIsStopped then
      begin
        fParent.Observer.OnCompleted;
        fParent.Dispose;
      end;
    end;
  finally
    MonitorExit(fParent);
  end;
end;

{$ENDREGION}


end.
