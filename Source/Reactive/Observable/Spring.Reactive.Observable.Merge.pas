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

unit Spring.Reactive.Observable.Merge;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TMerge<TSource> = class
  public type
    TObservables = class(TProducer<TSource>)
    private
      fSources: IObservable<IObservable<TSource>>;

      type
        TSink = class(TSink<TSource>, IObserver<IObservable<TSource>>)
        private
          fIsStopped: Boolean;
          fGroup: ICompositeDisposable;
          fSourceSubscription: ISingleAssignmentDisposable;

          type
            TInnerObserver = class(TDisposableObject, IObserver<TSource>)
            private
              fParent: TSink;
              fDisposable: IDisposable;
            public
              constructor Create(const parent: TSink; const disposable: IDisposable);
              destructor Destroy; override;
              procedure Dispose; override;
              procedure OnNext(const value: TSource);
              procedure OnError(const error: Exception);
              procedure OnCompleted;
            end;
        public
          constructor Create(const observer: IObserver<TSource>;
            const cancel: IDisposable);
          destructor Destroy; override;
          function Run(const parent: TObservables): IDisposable;
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
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TMerge<TSource>.TObservables'}

constructor TMerge<TSource>.TObservables.Create(
  const sources: IObservable<IObservable<TSource>>);
begin
  inherited Create;
  fSources := sources;
end;

function TMerge<TSource>.TObservables.CreateSink(
  const observer: IObserver<TSource>; const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(observer, cancel);
end;

function TMerge<TSource>.TObservables.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(Self);
end;

{$ENDREGION}


{$REGION 'TMerge<TSource>.TObservables.TSink'}

constructor TMerge<TSource>.TObservables.TSink.Create(
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
end;

destructor TMerge<TSource>.TObservables.TSink.Destroy;
begin
  inherited Destroy;
end;

function TMerge<TSource>.TObservables.TSink.Run(const parent: TObservables): IDisposable;
begin
  fIsStopped := False;
  fGroup := TCompositeDisposable.Create([]);

  fSourceSubscription := TSingleAssignmentDisposable.Create;
  fGroup.Add(fSourceSubscription);
  fSourceSubscription.Disposable := parent.fSources.Subscribe(Self);

  Result := fGroup;
end;

procedure TMerge<TSource>.TObservables.TSink.OnNext(
  const value: IObservable<TSource>);
var
  innerSubscription: ISingleAssignmentDisposable;
begin
  innerSubscription := TSingleAssignmentDisposable.Create;
  fGroup.Add(innerSubscription);
  innerSubscription.Disposable := value.Subscribe(TInnerObserver.Create(Self, innerSubscription) as IObserver<TSource>)
end;

procedure TMerge<TSource>.TObservables.TSink.OnError(
  const error: Exception);
begin
  MonitorEnter(Self);
  try
    Observer.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TMerge<TSource>.TObservables.TSink.OnCompleted;
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

{$ENDREGION}


{$REGION 'TMerge<TSource>.TObservables.TSink.TInnerObserver'}

constructor TMerge<TSource>.TObservables.TSink.TInnerObserver.Create(
  const parent: TSink; const disposable: IDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fDisposable := disposable;
end;

destructor TMerge<TSource>.TObservables.TSink.TInnerObserver.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TMerge<TSource>.TObservables.TSink.TInnerObserver.Dispose;
begin
  if not IsDisposed then
  begin
    fParent._Release;
    fParent := nil;
    fDisposable := nil;
  end;

  inherited Dispose;
end;

procedure TMerge<TSource>.TObservables.TSink.TInnerObserver.OnNext(const value: TSource);
begin
  MonitorEnter(fParent);
  try
    fParent.Observer.OnNext(value);
  finally
    MonitorExit(fParent);
  end;
end;

procedure TMerge<TSource>.TObservables.TSink.TInnerObserver.OnError(const error: Exception);
begin
  MonitorEnter(fParent);
  try
    fParent.Observer.OnError(error);
    fParent.Dispose;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TMerge<TSource>.TObservables.TSink.TInnerObserver.OnCompleted;
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
