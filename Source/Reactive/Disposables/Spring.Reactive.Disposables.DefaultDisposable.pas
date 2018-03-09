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

unit Spring.Reactive.Disposables.DefaultDisposable;

interface

uses
  Spring,
  Spring.Reactive;

type
  TDefaultDisposable = record
  private
    class function GetInstance: IDisposable; static;
  public
    class property Instance: IDisposable read GetInstance;
  end;

implementation

function DefaultDisposable_QueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

function DefaultDisposable_AddRef(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function DefaultDisposable_Release(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

procedure DefaultDisposable_Dispose(inst: Pointer);
begin
end;

const
  DefaultDisposable_Vtable: array[0..3] of Pointer =
  (
    @DefaultDisposable_QueryInterface,
    @DefaultDisposable_AddRef,
    @DefaultDisposable_Release,
    @DefaultDisposable_Dispose
  );
  DefaultDisposable_Instance: Pointer = @DefaultDisposable_Vtable;


{$REGION 'TDefaultDisposable'}

class function TDefaultDisposable.GetInstance: IDisposable;
begin
  Pointer(Result) := @DefaultDisposable_Instance;
end;

{$ENDREGION}


end.
