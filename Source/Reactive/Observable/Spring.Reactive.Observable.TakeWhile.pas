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

unit Spring.Reactive.Observable.TakeWhile;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TTakeWhile<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
    fPredicate: Predicate<TSource>;
    fPredicateIndex: Func<TSource, Integer, Boolean>;

    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      private
        fParent: TTakeWhile<TSource>;
        fRunning: Boolean;
      public
        constructor Create(const parent: TTakeWhile<TSource>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: TSource);
      end;

      TSinkIndex = class(TSink<TSource>, IObserver<TSource>)
      private
        fParent: TTakeWhile<TSource>;
        fRunning: Boolean;
        fIndex: Integer;
      public
        constructor Create(const parent: TTakeWhile<TSource>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: TSource);
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>; const predicate: Predicate<TSource>); overload;
    constructor Create(const source: IObservable<TSource>; const predicate: Func<TSource, Integer, Boolean>); overload;
  end;

implementation


{$REGION 'TTakeWhile<TSource>'}

constructor TTakeWhile<TSource>.Create(const source: IObservable<TSource>;
  const predicate: Predicate<TSource>);
begin
  inherited Create;
  fSource := source;
  fPredicate := predicate;
end;

constructor TTakeWhile<TSource>.Create(const source: IObservable<TSource>;
  const predicate: Func<TSource, Integer, Boolean>);
begin
  inherited Create;
  fSource := source;
  fPredicateIndex := predicate;
end;

function TTakeWhile<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TTakeWhile<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TTTakeWhileSkipWhile<TSource>.TSink'}

constructor TTakeWhile<TSource>.TSink.Create(const parent: TTakeWhile<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
  fRunning := True;
end;

destructor TTakeWhile<TSource>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TTakeWhile<TSource>.TSink.OnNext(const value: TSource);
begin
  if fRunning then
  begin
    try
      fRunning := fParent.fPredicate(value);
    except
      on e: Exception do
      begin
        Observer.OnError(e);
        Dispose;
        Exit;
      end;
    end;
  end;

  if fRunning then
    Observer.OnNext(value)
  else
  begin
    Observer.OnCompleted;
    Dispose;
  end;
end;

{$ENDREGION}


{$REGION 'TTakeWhile<TSource>.TSinkIndex'}

constructor TTakeWhile<TSource>.TSinkIndex.Create(const parent: TTakeWhile<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TTakeWhile<TSource>.TSinkIndex.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TTakeWhile<TSource>.TSinkIndex.OnNext(const value: TSource);
begin
  if not fRunning then
  begin
    try
      fRunning := not fParent.fPredicateIndex(value, fIndex);
      Inc(fIndex);
    except
      on e: Exception do
      begin
        Observer.OnError(e);
        Dispose;
        Exit;
      end;
    end;
  end;

  if fRunning then
    Observer.OnNext(value);
end;

{$ENDREGION}


end.
