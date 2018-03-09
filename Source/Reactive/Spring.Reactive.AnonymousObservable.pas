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

unit Spring.Reactive.AnonymousObservable;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.ObservableBase;

type
  TAnonymousObservable<T> = class(TObservableBase<T>)
  private
    fSubscribe: Func<IObserver<T>, IDisposable>;
  protected
    function SubscribeCore(const observer: IObserver<T>): IDisposable; override;
  public
    constructor Create(const subscribe: Func<IObserver<T>, IDisposable>);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TAnonymousObservable<T>'}

constructor TAnonymousObservable<T>.Create(
  const subscribe: Func<IObserver<T>, IDisposable>);
begin
  Guard.CheckNotNull(Assigned(subscribe), 'subscribe');
  inherited Create;
  fSubscribe := subscribe;
end;

function TAnonymousObservable<T>.SubscribeCore(
  const observer: IObserver<T>): IDisposable;
begin
  Result := fSubscribe(observer);
  if not Assigned(Result) then
    Result := Disposable.Empty;
end;

{$ENDREGION}


end.
