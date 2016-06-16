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

unit Spring.Reactive.Observable.Return;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TReturn<T> = class(TProducer<T>)
  private
    fValue: T;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>)
      private
        fParent: TReturn<T>;
        procedure Invoke;
      public
        constructor Create(const parent: TReturn<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const value: T; const scheduler: IScheduler);
  end;

implementation


{$REGION 'TReturn<T>'}

constructor TReturn<T>.Create(const value: T; const scheduler: IScheduler);
begin
  inherited Create;
  fValue := value;
  fScheduler := scheduler;
end;

function TReturn<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TReturn<T>.TSink'}

constructor TReturn<T>.TSink.Create(const parent: TReturn<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
end;

procedure TReturn<T>.TSink.Invoke;
begin
  fObserver.OnNext(fParent.fValue);
  fObserver.OnCompleted;
  Dispose;
end;

function TReturn<T>.TSink.Run: IDisposable;
begin
  Result := fParent.fScheduler.Schedule(Invoke);
end;

{$ENDREGION}


end.
