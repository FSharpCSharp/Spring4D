{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Persistence.Core.Types;

interface

uses
  SysUtils;

type
  /// <summary>
  /// Generic enumeration type
  /// <remarks>
  /// Should be used only for enumerations!
  /// </remarks>
  /// </summary>
  TEnum<T> = record
  private
    FValue: NativeInt;
  private
    function GetValue: T;
    procedure SetValue(const Value: T);
  public
    class operator Implicit(const AEnum: TEnum<T>): T; inline;
    class operator Implicit(const AEnum: T): TEnum<T>; inline;
    class operator Implicit(const AEnum: TEnum<T>): Integer; inline;
    class operator Implicit(const AEnum: Integer): TEnum<T>; inline;

    function ToString: string;

    property Value: T read GetValue write SetValue;
  end;


implementation

uses
  TypInfo,
  Rtti,
  Spring.Persistence.Core.Exceptions
  ;

{ TEnum<T> }

function TEnum<T>.GetValue: T;
begin
  Result := TValue.FromOrdinal(TypeInfo(T), FValue).AsType<T>;
end;

class operator TEnum<T>.Implicit(const AEnum: T): TEnum<T>;
begin
  Result.Value := AEnum;
end;

class operator TEnum<T>.Implicit(const AEnum: TEnum<T>): T;
begin
  Result := AEnum.Value;
end;

class operator TEnum<T>.Implicit(const AEnum: Integer): TEnum<T>;
begin
  Result.FValue := AEnum;
end;

class operator TEnum<T>.Implicit(const AEnum: TEnum<T>): Integer;
begin
  Result := AEnum.FValue;
end;

procedure TEnum<T>.SetValue(const Value: T);
var
  AValue: TValue;
begin
  AValue := TValue.From<T>(Value);
  if AValue.Kind = tkEnumeration then
    FValue := AValue.AsOrdinal
  else
    raise EORMEnumException.Create('Incorrect value type. Can set only enumerations.');
end;

function TEnum<T>.ToString: string;
begin
  Result := GetEnumName(TypeInfo(T), FValue);
end;

end.
