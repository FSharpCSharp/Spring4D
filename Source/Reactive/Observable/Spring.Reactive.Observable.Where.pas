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

unit Spring.Reactive.Observable.Where;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TWhere<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fPredicate: Predicate<T>;

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TWhere<T>;
      public
        constructor Create(const parent: TWhere<T>; const observer: IObserver<T>;
          const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: T);
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; const predicate: Predicate<T>);
  end;


implementation


{$REGION 'TWhere<T>'}

constructor TWhere<T>.Create(const source: IObservable<T>;
  const predicate: Predicate<T>);
begin
  inherited Create;
  fSource := source;
  fPredicate := predicate;
end;

function TWhere<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  if Assigned(fPredicate) then
  begin
    sink := TSink.Create(Self, observer, cancel);
    setSink(sink);
    // TODO implement SubscribeSafe
    Result := fSource.Subscribe(sink);
  end;
end;

{$ENDREGION}


{$REGION 'TWhere<T>.TSink'}

constructor TWhere<T>.TSink.Create(const parent: TWhere<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TWhere<T>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TWhere<T>.TSink.OnNext(const value: T);
var
  shouldRun: Boolean;
begin
  shouldRun := False;
  try
    shouldRun := fParent.fPredicate(value);
  except
    on e: Exception do
    begin
      fObserver.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if shouldRun then
    fObserver.OnNext(value);
end;

{$ENDREGION}


end.
