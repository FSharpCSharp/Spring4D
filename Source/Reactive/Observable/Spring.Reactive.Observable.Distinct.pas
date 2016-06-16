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

unit Spring.Reactive.Observable.Distinct;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TDistinct<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TDistinct<T>;
        fHashSet: ISet<T>;
      public
        constructor Create(const parent: TDistinct<T>; const observer: IObserver<T>;
          const cancel: IDisposable);
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


{$REGION 'TDistinct<T>'}

constructor TDistinct<T>.Create(const source: IObservable<T>);
begin
  inherited Create;
  fSource := source;
end;

function TDistinct<T>.Run(const observer: IObserver<T>;
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


{$REGION 'TDistinct<T>.TSink'}

constructor TDistinct<T>.TSink.Create(const parent: TDistinct<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fHashSet := TCollections.CreateSet<T>;
end;

procedure TDistinct<T>.TSink.OnNext(const value: T);
var
  hasAdded: Boolean;
begin
  // TODO keySelector and comparer
  try
    hasAdded := fHashSet.Add(value);
  except
    on e: Exception do
    begin
      fObserver.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if hasAdded then
    fObserver.OnNext(value);
end;

{$ENDREGION}


end.
