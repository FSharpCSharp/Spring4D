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
  TMerge<TSource> = class(TProducer<TSource>)
  private
    fSources: IObservable<IObservable<TSource>>;

    type
      TSink = class(TSink<TSource>, IObserver<IObservable<TSource>>)
      private
        fParent: TMerge<TSource>;
        fIsStopped: Boolean;
        fGroup: ICompositeDisposable;
        fSourceSubscription: ISingleAssignmentDisposable;

        type
          TIter = class(TDisposableObject, IObserver<TSource>)
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
        constructor Create(const parent: TMerge<TSource>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
        procedure OnNext(const value: IObservable<TSource>);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<TSource>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const sources: IObservable<IObservable<TSource>>);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TMerge<TSource>'}

constructor TMerge<TSource>.Create(
  const sources: IObservable<IObservable<TSource>>);
begin
  inherited Create;
  fSources := sources;
end;

function TMerge<TSource>.Run(const observer: IObserver<TSource>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TMerge<TSource>.TSink'}

constructor TMerge<TSource>.TSink.Create(const parent: TMerge<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TMerge<TSource>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

function TMerge<TSource>.TSink.Run: IDisposable;
begin
  fIsStopped := False;
  fGroup := TCompositeDisposable.Create([]);

  fSourceSubscription := TSingleAssignmentDisposable.Create;
  fGroup.Add(fSourceSubscription);
  fSourceSubscription.Disposable := fParent.fSources.Subscribe(Self);

  Result := fGroup;
end;

procedure TMerge<TSource>.TSink.OnNext(const value: IObservable<TSource>);
var
  innerSubscription: ISingleAssignmentDisposable;
begin
  innerSubscription := TSingleAssignmentDisposable.Create;
  fGroup.Add(innerSubscription);
  innerSubscription.Disposable := value.Subscribe(TIter.Create(Self, innerSubscription) as IObserver<TSource>)
end;

procedure TMerge<TSource>.TSink.OnError(const error: Exception);
begin
  MonitorEnter(Self);
  try
    fObserver.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TMerge<TSource>.TSink.OnCompleted;
begin
  fIsStopped := True;
  if fGroup.Count = 1 then
  begin
    MonitorEnter(Self);
    try
      fObserver.OnCompleted;
      Dispose;
    finally
      MonitorExit(Self);
    end;
  end
  else
    fSourceSubscription.Dispose;
end;

{$ENDREGION}


{$REGION 'TMerge<TSource>.TSink.TIter'}

constructor TMerge<TSource>.TSink.TIter.Create(const parent: TSink;
  const disposable: IDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fDisposable := disposable;
end;

destructor TMerge<TSource>.TSink.TIter.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TMerge<TSource>.TSink.TIter.Dispose;
begin
//  if not IsDisposed then
//  begin
//    fParent._Release;
//    fParent := nil;
//    fDisposable := nil;
//  end;

  inherited Dispose;
end;

procedure TMerge<TSource>.TSink.TIter.OnNext(const value: TSource);
begin
  MonitorEnter(fParent);
  try
    fParent.fObserver.OnNext(value);
  finally
    MonitorExit(fParent);
  end;
end;

procedure TMerge<TSource>.TSink.TIter.OnError(const error: Exception);
var
  observer: IObserver<TSource>;
begin
  MonitorEnter(fParent);
  try
    observer := fParent.fObserver;
    observer.OnError(error);
    observer := nil;
    fParent.Dispose;
  finally
    MonitorExit(fParent);
  end;
end;

procedure TMerge<TSource>.TSink.TIter.OnCompleted;
var
  observer: IObserver<TSource>;
begin
  fParent.fGroup.Remove(fDisposable);
  if fParent.fIsStopped and (fParent.fGroup.Count = 1) then
  begin
    MonitorEnter(fParent);
    try
      observer := fParent.fObserver;
      observer.OnCompleted;
      observer := nil;
      fParent.Dispose;
    finally
      MonitorExit(fParent);
    end;
  end;
end;

{$ENDREGION}


end.
