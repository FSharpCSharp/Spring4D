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

unit Spring.Reactive.Disposables.DefaultDisposable;

interface

uses
  Spring,
  Spring.Reactive;

type
  // TODO consider handcrafted IMT in Disposable.Empty for less overhead
  TDefaultDisposable = class(TInterfaceBase, IDisposable)
  strict private class var
    fInstance: TDefaultDisposable;
  public
    class constructor Create;
    class destructor Destroy;
    procedure Dispose;
    class property Instance: TDefaultDisposable read fInstance;
  end;

implementation


{$REGION 'TDefaultDisposable'}

class constructor TDefaultDisposable.Create;
begin
  fInstance := TDefaultDisposable.Create;
end;

class destructor TDefaultDisposable.Destroy;
begin
  fInstance.Free;
end;

procedure TDefaultDisposable.Dispose;
begin
end;

{$ENDREGION}


end.
