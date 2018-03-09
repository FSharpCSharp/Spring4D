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

unit Spring.Reactive.Disposables.AnonymousDisposable;

interface

uses
  Spring,
  Spring.Reactive;

type
  TAnonymousDisposable = class(TInterfacedObject, IDisposable, ICancelable)
  private
    fDispose: Action;
    function GetIsDisposed: Boolean; inline;
  public
    constructor Create(const dispose: Action);
    procedure Dispose;
    property IsDisposed: Boolean read GetIsDisposed;
  end;

implementation


{$REGION 'TAnonymousDisposable'}

constructor TAnonymousDisposable.Create(const dispose: Action);
begin
  inherited Create;
  fDispose := dispose;
end;

procedure TAnonymousDisposable.Dispose;
var
  dispose: Action;
begin
  dispose := TInterlocked.Exchange<Action>(fDispose, nil);
  if Assigned(dispose) then
    dispose;
end;

function TAnonymousDisposable.GetIsDisposed: Boolean;
begin
  Result := not Assigned(fDispose);
end;

{$ENDREGION}


end.
