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

unit Spring.Reactive.Internal.Stubs;

interface

uses
  SysUtils,
  Spring,
  Spring.Reactive;

type
  Stubs = record
  strict private class var
    fNop: Action;
    fThrow: Action<Exception>;
  public
    class constructor Create;
    class destructor Destroy;
    class property Nop: Action read fNop;
    class property Throw: Action<Exception> read fThrow;
  end;

  Stubs<T> = record
  strict private class var
    fIgnore: Action<T>;
  public
    class constructor Create;
    class destructor Destroy;
    class property Ignore: Action<T> read fIgnore;
  end;

implementation


{$REGION 'Stubs'}

class constructor Stubs.Create;
begin
  fNop := procedure begin end;
  fThrow := procedure(const e: Exception) begin raise e; end;
end;

class destructor Stubs.Destroy;
begin
  fNop := nil;
  fThrow := nil;
end;

{$ENDREGION}


{$REGION 'Stubs<T>'}

class constructor Stubs<T>.Create;
begin
  fIgnore := procedure(const _: T) begin end;
end;

class destructor Stubs<T>.Destroy;
begin
  fIgnore := nil;
end;

{$ENDREGION}


end.
