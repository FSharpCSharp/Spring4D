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

unit Spring.Reactive.Observable.AddRef;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TAddRef<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fRefCount: IRefCountDisposable;

    type
      TSink = class(TSink<T>, IObserver<T>)
      public
        procedure OnNext(const value: T);
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; const refCount: IRefCountDisposable);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TAddRef<T>'}

constructor TAddRef<T>.Create(const source: IObservable<T>;
  const refCount: IRefCountDisposable);
begin
  inherited Create;
  fSource := source;
  fRefCount := refCount;
end;

function TAddRef<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  d: ICancelable;
  sink: TSink;
begin
  d := TStableCompositeDisposable.Create(fRefCount.GetDisposable, cancel);

  sink := TSink.Create(observer, d);
  setSink(sink);
  Result := fSource.Subscribe(sink);
end;

{$ENDREGION}


{$REGION 'TAddRef<T>.TSink'}

procedure TAddRef<T>.TSink.OnNext(const value: T);
begin
  Observer.OnNext(value);
end;

{$ENDREGION}


end.
