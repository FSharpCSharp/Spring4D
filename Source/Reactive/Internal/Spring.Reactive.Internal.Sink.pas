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

unit Spring.Reactive.Internal.Sink;

interface

uses
  Spring,
  Spring.Reactive;

type
  TSink<TSource> = class abstract(TDisposableObject)
  private
    fObserver: IObserver<TSource>;
    fCancel: IDisposable;
    // getter functions are necessary to ensure the
    // instance is alive during any calls made on it
    function GetCancel: IDisposable;
    function GetObserver: IObserver<TSource>;
  protected
    property Observer: IObserver<TSource> read GetObserver;
    property Cancel: IDisposable read GetCancel;
  public
    constructor Create(const observer: IObserver<TSource>; const cancel: IDisposable);
    destructor Destroy; override;
    procedure Dispose; override;

    procedure OnError(const error: Exception);
    procedure OnCompleted;
  end;

implementation

uses
  Spring.Reactive.Internal.Observers;


{$REGION 'TSink<TSource>'}

constructor TSink<TSource>.Create(const observer: IObserver<TSource>;
  const cancel: IDisposable);
begin
  inherited Create;
  fObserver := observer;
  fCancel := cancel;
end;

destructor TSink<TSource>.Destroy;
begin

  inherited;
end;

procedure TSink<TSource>.Dispose;
var
  cancel: IDisposable;
begin
  inherited Dispose;
  fObserver := TNopObserver<TSource>.Instance;
  cancel := TInterlocked.Exchange<IDisposable>(fCancel, nil);
  if Assigned(cancel) then
    cancel.Dispose;
end;

function TSink<TSource>.GetCancel: IDisposable;
begin
  Result := fCancel;
end;

function TSink<TSource>.GetObserver: IObserver<TSource>;
begin
  Result := fObserver;
end;

procedure TSink<TSource>.OnCompleted;
begin
  Observer.OnCompleted;
  Dispose;
end;

procedure TSink<TSource>.OnError(const error: Exception);
begin
  Observer.OnError(error);
  Dispose;
end;

{$ENDREGION}


end.
