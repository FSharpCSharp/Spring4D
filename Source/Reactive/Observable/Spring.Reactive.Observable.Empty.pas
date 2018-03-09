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

unit Spring.Reactive.Observable.Empty;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TEmpty<TResult> = class(TProducer<TResult>)
  private
    fScheduler: IScheduler;

    type
      TSink = class(TSink<TResult>)
      private
        fParent: TEmpty<TResult>;
        procedure Invoke;
      public
        constructor Create(const parent: TEmpty<TResult>;
          const observer: IObserver<TResult>; const cancel: IDisposable);
        function Run: IDisposable;
      end;
  protected
    function CreateSink(const observer: IObserver<TResult>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const scheduler: IScheduler);
  end;

implementation


{$REGION 'TEmpty<TResult>'}

constructor TEmpty<TResult>.Create(const scheduler: IScheduler);
begin
  inherited Create;
  fScheduler := scheduler;
end;

function TEmpty<TResult>.CreateSink(const observer: IObserver<TResult>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TEmpty<TResult>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run;
end;

{$ENDREGION}


{$REGION 'TEmpty<TResult>.TSink'}

constructor TEmpty<TResult>.TSink.Create(const parent: TEmpty<TResult>;
  const observer: IObserver<TResult>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
end;

procedure TEmpty<TResult>.TSink.Invoke;
begin
  Observer.OnCompleted;
  Dispose;
end;

function TEmpty<TResult>.TSink.Run: IDisposable;
begin
  Result := fParent.fScheduler.Schedule(Invoke);
end;

{$ENDREGION}


end.
