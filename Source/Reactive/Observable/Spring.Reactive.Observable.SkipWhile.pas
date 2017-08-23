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
  TSkipWhile<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fPredicate: Predicate<T>;
    fPredicateIndex: Func<T, Integer, Boolean>;

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TSkipWhile<T>;
        fRunning: Boolean;
      public
        constructor Create(const parent: TSkipWhile<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: T);
      end;

      TSinkIndex = class(TSink<T>, IObserver<T>)
      private
        fParent: TSkipWhile<T>;
        fRunning: Boolean;
        fIndex: Integer;
      public
        constructor Create(const parent: TSkipWhile<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: T);
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; const predicate: Predicate<T>); overload;
    constructor Create(const source: IObservable<T>; const predicate: Func<T, Integer, Boolean>); overload;
  end;

implementation


{$REGION 'TSkipWhile<T>'}

constructor TSkipWhile<T>.Create(const source: IObservable<T>;
  const predicate: Predicate<T>);
begin
  inherited Create;
  fSource := source;
  fPredicate := predicate;
end;

constructor TSkipWhile<T>.Create(const source: IObservable<T>;
  const predicate: Func<T, Integer, Boolean>);
begin
  inherited Create;
  fSource := source;
  fPredicateIndex := predicate;
end;

function TSkipWhile<T>.Run(const observer: IObserver<T>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: IObserver<T>;
begin
  if Assigned(fPredicate) then
  begin
    sink := TSink.Create(Self, observer, cancel);
    setSink(sink);
    Result := fSource.Subscribe(sink);
  end
  else
  begin
    sink := TSinkIndex.Create(Self, observer, cancel);
    setSink(sink);
    Result := fSource.Subscribe(sink);
  end;
end;

{$ENDREGION}


{$REGION 'TSkipWhile<T>.TSink'}

constructor TSkipWhile<T>.TSink.Create(const parent: TSkipWhile<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TSkipWhile<T>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSkipWhile<T>.TSink.OnNext(const value: T);
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


{$REGION 'TSkipWhile<T>.TSinkIndex'}

constructor TSkipWhile<T>.TSinkIndex.Create(const parent: TSkipWhile<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TSkipWhile<T>.TSinkIndex.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSkipWhile<T>.TSinkIndex.OnNext(const value: T);
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
