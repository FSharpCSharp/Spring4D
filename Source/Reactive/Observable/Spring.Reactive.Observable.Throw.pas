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

unit Spring.Reactive.Observable.Throw;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TThrow<T> = class(TProducer<T>)
  private
    fError: Exception;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>)
      private
        fParent: TThrow<T>;
        procedure Invoke;
      public
        constructor Create(const parent: TThrow<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const error: Exception; const scheduler: IScheduler);
    procedure Dispose; override;
  end;

implementation

uses
  SysUtils;


{$REGION 'TThrow<T>'}

constructor TThrow<T>.Create(const error: Exception;
  const scheduler: IScheduler);
begin
  inherited Create;
  fError := error;
  fScheduler := scheduler;
end;

procedure TThrow<T>.Dispose;
begin
  FreeAndNil(fError);
  inherited;
end;

function TThrow<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TThrow<T>.TSink'}

constructor TThrow<T>.TSink.Create(const parent: TThrow<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
end;

procedure TThrow<T>.TSink.Invoke;
begin
  Observer.OnError(fParent.fError);
  Dispose;
end;

function TThrow<T>.TSink.Run: IDisposable;
begin
  Result := fParent.fScheduler.Schedule(Invoke);
end;

{$ENDREGION}


end.
