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

unit Spring.Reactive.Disposables.SingleAssignmentDisposable;

interface

uses
  Spring,
  Spring.Reactive;

type
  TSingleAssignmentDisposable = class(TInterfacedObject,
    IDisposable, ICancelable, ISingleAssignmentDisposable)
  private
    fCurrent: IDisposable;
    function GetIsDisposed: Boolean;
    function GetDisposable: IDisposable;
    procedure SetDisposable(const value: IDisposable);
  public
    procedure Dispose;
    property Disposable: IDisposable read GetDisposable write SetDisposable;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TSingleAssignmentDisposable'}

procedure TSingleAssignmentDisposable.Dispose;
var
  old: IDisposable;
begin
  old := TInterlocked.Exchange<IDisposable>(fCurrent, TBooleanDisposable.True);
  if Assigned(old) then
    old.Dispose;
end;

function TSingleAssignmentDisposable.GetDisposable: IDisposable;
var
  current: IDisposable;
begin
  current := fCurrent;
  if current = IDisposable(TBooleanDisposable.True) then
    Result := TDefaultDisposable.Instance
  else
    Result := current;
end;

function TSingleAssignmentDisposable.GetIsDisposed: Boolean;
begin
  Result := fCurrent = IDisposable(TBooleanDisposable.True);
end;

procedure TSingleAssignmentDisposable.SetDisposable(const value: IDisposable);
var
  old: IDisposable;
begin
  old := TInterlocked.CompareExchange<IDisposable>(fCurrent, value, nil);
  if old = nil then
    Exit;

  if old <> IDisposable(TBooleanDisposable.True) then
    raise EInvalidOperationException.Create('disposable already assigned');

  if Assigned(value) then
    value.Dispose;
end;

{$ENDREGION}


end.
