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

unit Spring.Reactive.Disposables.SerialDisposable;

interface

uses
  Spring.Reactive;

type
  TSerialDisposable = class(TInterfacedObject,
    IDisposable, ICancelable, ISerialDisposable)
  private
    fDisposed: Boolean;
    fCurrent: IDisposable;
    function GetIsDisposed: Boolean;
    function GetDisposable: IDisposable;
    procedure SetDisposable(const value: IDisposable);
  public
    procedure Dispose;
    property Disposable: IDisposable read GetDisposable write SetDisposable;
  end;

implementation


{$REGION 'TSerialDisposable'}

procedure TSerialDisposable.Dispose;
var
  old: IDisposable;
begin
  MonitorEnter(Self);
  try
    if not fDisposed then
    begin
      fDisposed := True;
      old := fCurrent;
      fCurrent := nil;
    end;
  finally
    MonitorExit(Self);
  end;

  if Assigned(old) then
    old.Dispose;
end;

function TSerialDisposable.GetDisposable: IDisposable;
begin
  Result := fCurrent;
end;

function TSerialDisposable.GetIsDisposed: Boolean;
begin
  Result := fDisposed;
end;

procedure TSerialDisposable.SetDisposable(const value: IDisposable);
var
  shouldDispose: Boolean;
  old: IDisposable;
begin
  MonitorEnter(Self);
  try
    shouldDispose := fDisposed;
    if not shouldDispose then
    begin
      old := fCurrent;
      fCurrent := value;
    end;
  finally
    MonitorExit(Self);
  end;

  if Assigned(old) then
    old.Dispose;
  if shouldDispose and Assigned(value) then
    value.Dispose;
end;

{$ENDREGION}


end.
