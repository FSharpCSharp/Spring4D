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

unit Spring.Reactive.Disposables.BooleanDisposable;

interface

uses
  Spring.Reactive;

type
  TBooleanDisposable = class(TInterfacedObject, IDisposable, ICancelable)
  strict private class var
    fTrue: TBooleanDisposable;
  private
    fIsDisposed: Boolean;
    function GetIsDisposed: Boolean;
  public
    class constructor Create;
    class destructor Destroy;
    procedure Dispose;
    class property True: TBooleanDisposable read fTrue;
  end;

implementation


{$REGION 'TBooleanDisposable'}

class constructor TBooleanDisposable.Create;
begin
  fTrue := TBooleanDisposable.Create;
  fTrue.fIsDisposed := System.True;
  fTrue._AddRef;
end;

class destructor TBooleanDisposable.Destroy;
begin
  fTrue._Release;
  fTrue := nil;
end;

procedure TBooleanDisposable.Dispose;
begin
  fIsDisposed := System.True;
end;

function TBooleanDisposable.GetIsDisposed: Boolean;
begin
  Result := fIsDisposed;
end;

{$ENDREGION}


end.
