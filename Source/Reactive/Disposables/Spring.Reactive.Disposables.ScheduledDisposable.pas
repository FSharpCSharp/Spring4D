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

unit Spring.Reactive.Disposables.ScheduledDisposable;

interface

uses
  Spring.Reactive;

type
  TScheduledDisposable = class(TInterfacedObject, IDisposable, ICancelable)
  private
    fScheduler: IScheduler;
    fDisposable: IDisposable;
    function GetIsDisposed: Boolean;
    procedure DisposeInner;
  public
    constructor Create(const scheduler: IScheduler; const disposable: IDisposable);
    procedure Dispose;
  end;

implementation

uses
  Spring,
  Spring.Reactive.Disposables;


{$REGION 'TScheduledDisposable'}

constructor TScheduledDisposable.Create(const scheduler: IScheduler;
  const disposable: IDisposable);
begin
  Guard.CheckNotNull(scheduler, 'scheduler');
  Guard.CheckNotNull(disposable, 'disposable');

  fScheduler := scheduler;
  fDisposable := disposable;
end;

procedure TScheduledDisposable.Dispose;
begin
  fScheduler.Schedule(DisposeInner);
end;

procedure TScheduledDisposable.DisposeInner;
var
  disposable: IDisposable;
begin
  disposable := TInterlocked.Exchange<IDisposable>(fDisposable, TBooleanDisposable.True);
  if disposable <> TBooleanDisposable.True then
    disposable.Dispose;
end;

function TScheduledDisposable.GetIsDisposed: Boolean;
begin
  Result := fDisposable = TBooleanDisposable.True;
end;

{$ENDREGION}


end.
