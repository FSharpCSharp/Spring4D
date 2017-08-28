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

unit Spring.Reactive.Observable.SkipWhile;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSkipWhile<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
    fPredicate: Predicate<TSource>;
    fPredicateIndex: Func<TSource, Integer, Boolean>;

    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      private
        fParent: TSkipWhile<TSource>;
        fRunning: Boolean;
      public
        constructor Create(const parent: TSkipWhile<TSource>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: TSource);
      end;

      TSinkIndex = class(TSink<TSource>, IObserver<TSource>)
      private
        fParent: TSkipWhile<TSource>;
        fRunning: Boolean;
        fIndex: Integer;
      public
        constructor Create(const parent: TSkipWhile<TSource>;
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


{$REGION 'TSkipWhile<TSource>'}

constructor TSkipWhile<TSource>.Create(const source: IObservable<TSource>;
  const predicate: Predicate<TSource>);
begin
  inherited Create;
  fSource := source;
  fPredicate := predicate;
end;

constructor TSkipWhile<TSource>.Create(const source: IObservable<TSource>;
  const predicate: Func<TSource, Integer, Boolean>);
begin
  inherited Create;
  fSource := source;
  fPredicateIndex := predicate;
end;

function TSkipWhile<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TSkipWhile<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TSkipWhile<TSource>.TSink'}

constructor TSkipWhile<TSource>.TSink.Create(const parent: TSkipWhile<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TSkipWhile<TSource>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSkipWhile<TSource>.TSink.OnNext(const value: TSource);
begin
  if not fRunning then
  begin
    try
      fRunning := not fParent.fPredicate(value);
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


{$REGION 'TSkipWhile<TSource>.TSinkIndex'}

constructor TSkipWhile<TSource>.TSinkIndex.Create(const parent: TSkipWhile<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TSkipWhile<TSource>.TSinkIndex.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSkipWhile<TSource>.TSinkIndex.OnNext(const value: TSource);
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
