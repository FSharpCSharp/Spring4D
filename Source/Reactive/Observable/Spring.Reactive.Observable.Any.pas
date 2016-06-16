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

unit Spring.Reactive.Observable.Any;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TAny<T> = class(TProducer<Boolean>)
  private
    fSource: IObservable<T>;
    fPredicate: Predicate<T>;

    type
      TSink = class(TSink<Boolean>, IObserver<T>)
      public
        procedure OnNext(const value: T);
        procedure OnCompleted;
      end;

      TSinkPredicate = class(TSink<Boolean>, IObserver<T>)
      private
        fParent: TAny<T>;
      public
        constructor Create(const parent: TAny<T>;
          const observer: IObserver<Boolean>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: T);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<Boolean>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>); overload;
    constructor Create(const source: IObservable<T>; const predicate: Predicate<T>); overload;
  end;

implementation


{$REGION 'TAny<T>'}

constructor TAny<T>.Create(const source: IObservable<T>);
begin
  inherited Create;
  fSource := source;
end;

constructor TAny<T>.Create(const source: IObservable<T>;
  const predicate: Predicate<T>);
begin
  inherited Create;
  fSource := source;
  fPredicate := predicate;
end;

function TAny<T>.Run(const observer: IObserver<Boolean>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: IObserver<T>;
begin
  if Assigned(fPredicate) then
  begin
    sink := TSinkPredicate.Create(Self, observer, cancel);
    setSink(sink);
    Result := fSource.Subscribe(sink);
  end
  else
  begin
    sink := TSink.Create(observer, cancel);
    setSink(sink);
    Result := fSource.Subscribe(sink);
  end;
end;

{$ENDREGION}


{$REGION 'TAny<T>.TSink'}

procedure TAny<T>.TSink.OnCompleted;
begin
  fObserver.OnNext(False);
  fObserver.OnCompleted;
  Dispose;
end;

procedure TAny<T>.TSink.OnNext(const value: T);
begin
  fObserver.OnNext(True);
  fObserver.OnCompleted;
  Dispose;
end;

{$ENDREGION}


{$REGION 'TAny<T>.TSinkPredicate'}

constructor TAny<T>.TSinkPredicate.Create(const parent: TAny<T>;
  const observer: IObserver<Boolean>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TAny<T>.TSinkPredicate.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TAny<T>.TSinkPredicate.OnCompleted;
begin
  fObserver.OnNext(False);
  fObserver.OnCompleted;
  Dispose;
end;

procedure TAny<T>.TSinkPredicate.OnNext(const value: T);
var
  res: Boolean;
begin
  res := False;
  try
    res := fParent.fPredicate(value);
  except
    on e: Exception do
    begin
      fObserver.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if res then
  begin
    fObserver.OnNext(True);
    fObserver.OnCompleted;
    Dispose;
  end;
end;

{$ENDREGION}


end.
