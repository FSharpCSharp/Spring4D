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

unit Spring.Reactive.ObserverBase;

interface

uses
  Spring.Reactive;

type
  TObserverBase<T> = class abstract(TDisposableObject, IObserver<T>)
  private
    fIsStopped: Integer;
  protected
    constructor Create;
    procedure OnNextCore(const value: T); virtual; abstract;
    procedure OnErrorCore(const error: Exception); virtual; abstract;
    procedure OnCompletedCore; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Dispose; override;

    procedure OnNext(const value: T);
    procedure OnError(const error: Exception);
    procedure OnCompleted;
  end;

implementation

uses
  Spring;


{$REGION 'TObserverBase<T>'}

constructor TObserverBase<T>.Create;
begin
  inherited Create;
end;

destructor TObserverBase<T>.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TObserverBase<T>.Dispose;
begin
  fIsStopped := 1;
end;

procedure TObserverBase<T>.OnNext(const value: T);
begin
  if fIsStopped = 0 then
    OnNextCore(value);
end;

procedure TObserverBase<T>.OnError(const error: Exception);
begin
  Guard.CheckNotNull(error, 'error');

  if AtomicExchange(fIsStopped, 1) = 0 then
    OnErrorCore(error);
end;

procedure TObserverBase<T>.OnCompleted;
begin
  if AtomicExchange(fIsStopped, 1) = 0 then
    OnCompletedCore;
end;

{$ENDREGION}


end.
