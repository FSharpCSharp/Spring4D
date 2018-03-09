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

unit Spring.Reactive.Disposables.BooleanDisposable;

interface

uses
  Spring.Reactive;

type
  TBooleanDisposable = class(TInterfacedObject, IDisposable, ICancelable)
  private
    fIsDisposed: Boolean;
    function GetIsDisposed: Boolean;
    class function GetTrue: ICancelable; static;
  public
    procedure Dispose;
    class property True: ICancelable read GetTrue;
  end;

implementation

function BooleanDisposable_QueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

function BooleanDisposable_AddRef(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function BooleanDisposable_Release(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

procedure BooleanDisposable_Dispose(inst: Pointer);
begin
end;

function BooleanDisposable_GetIsDisposed(inst: Pointer): Boolean;
begin
  Result := True;
end;

const
  BooleanDisposable_Vtable: array[0..4] of Pointer =
  (
    @BooleanDisposable_QueryInterface,
    @BooleanDisposable_AddRef,
    @BooleanDisposable_Release,
    @BooleanDisposable_Dispose,
    @BooleanDisposable_GetIsDisposed
  );
  BooleanDisposable_Instance: Pointer = @BooleanDisposable_Vtable;


{$REGION 'TBooleanDisposable'}

procedure TBooleanDisposable.Dispose;
begin
  fIsDisposed := System.True;
end;

function TBooleanDisposable.GetIsDisposed: Boolean;
begin
  Result := fIsDisposed;
end;

class function TBooleanDisposable.GetTrue: ICancelable;
begin
  Pointer(Result) := @BooleanDisposable_Instance;
end;

{$ENDREGION}


end.
