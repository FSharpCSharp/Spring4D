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

unit Spring.Reactive.Observable.DistinctUntilChanged;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TDistinctUntilChanged<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
//    fKeySelector: Func<TSource,TKey>
    fComparer: IEqualityComparer<T>;//TKey

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TDistinctUntilChanged<T>;
        fCurrentKey: T;//TKey
        fHasCurrentKey: Boolean;
      public
        constructor Create(const parent: TDistinctUntilChanged<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        procedure OnNext(const value: T);
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    // TODO keySelector and comparer
    constructor Create(const source: IObservable<T>);
  end;

implementation


{$REGION 'TDistinctUntilChanged<T>'}

constructor TDistinctUntilChanged<T>.Create(const source: IObservable<T>);
begin
  inherited Create;
  fSource := source;
  fComparer := TEqualityComparer<T>.Default;
end;

function TDistinctUntilChanged<T>.Run(const observer: IObserver<T>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  // TODO implement SubscribeSafe
  Result := fSource.Subscribe(sink);
end;

{$ENDREGION}


{$REGION 'TDistinctUntilChanged<T>.TSink'}

constructor TDistinctUntilChanged<T>.TSink.Create(
  const parent: TDistinctUntilChanged<T>; const observer: IObserver<T>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fCurrentKey := Default(T);
  fHasCurrentKey := False;
end;

procedure TDistinctUntilChanged<T>.TSink.OnNext(const value: T);
var
  comparerEquals: Boolean;
begin
  if fHasCurrentKey then
  try
    comparerEquals := fParent.fComparer.Equals(fCurrentKey, value);
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if not fHasCurrentKey or not comparerEquals then
  begin
    fHasCurrentKey := True;
    fCurrentKey := value;
    Observer.OnNext(value);
  end;
end;

{$ENDREGION}


end.
