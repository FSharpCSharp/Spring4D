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
  TThrow<TSource> = class(TProducer<TSource>)
  private
    fError: Exception;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<TSource>)
      private
        fParent: TThrow<TSource>;
        procedure Invoke;
      public
        constructor Create(const parent: TThrow<TSource>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        function Run: IDisposable;
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const error: Exception; const scheduler: IScheduler);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;


{$REGION 'TThrow<TSource>'}

constructor TThrow<TSource>.Create(const error: Exception;
  const scheduler: IScheduler);
begin
  inherited Create;
  fError := error;
  fScheduler := scheduler;
end;

function TThrow<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

destructor TThrow<TSource>.Destroy;
begin
  FreeAndNil(fError);
  inherited;
end;

function TThrow<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run;
end;

{$ENDREGION}


{$REGION 'TThrow<TSource>.TSink'}

constructor TThrow<TSource>.TSink.Create(const parent: TThrow<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
end;

procedure TThrow<TSource>.TSink.Invoke;
begin
  Observer.OnError(fParent.fError);
  Dispose;
end;

function TThrow<TSource>.TSink.Run: IDisposable;
begin
  Result := fParent.fScheduler.Schedule(Invoke);
end;

{$ENDREGION}


end.
