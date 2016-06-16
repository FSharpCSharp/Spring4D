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

unit Spring.Reactive.Testing.Recorded;

interface

type
  TRecorded<T> = record
  strict private
    fTime: Int64;
    fValue: T;
  public
    constructor Create(const time: Int64; const value: T);

    function Equals(const other: TRecorded<T>): Boolean; //inline;
    function ToString: string;

    property Time: Int64 read fTime;
    property Value: T read fValue;

    class operator Equal(const left, right: TRecorded<T>): Boolean;
    class operator NotEqual(const left, right: TRecorded<T>): Boolean;
  end;

implementation

uses
  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections;


{$REGION 'TRecorded<T>'}

constructor TRecorded<T>.Create(const time: Int64; const value: T);
begin
  fTime := time;
  fValue := value;
end;

function TRecorded<T>.Equals(const other: TRecorded<T>): Boolean;
var
  equatable: IEquatable;
begin
  if (TType.Kind<T> = tkInterface) and Supports(PInterface(@Value)^, IEquatable, equatable) then
    Exit((Time = other.Time) and equatable.Equals(PInterface(@other.Value)^ as TObject));
  Result := (Time = other.Time) and TEqualityComparer<T>.Default.Equals(Value, other.Value);
end;

class operator TRecorded<T>.Equal(const left, right: TRecorded<T>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator TRecorded<T>.NotEqual(const left, right: TRecorded<T>): Boolean;
begin
  Result := not left.Equals(right);
end;

function TRecorded<T>.ToString: string;
begin
  Result := TValue.From<T>(fValue).ToString + '@' + IntToStr(fTime);
end;

{$ENDREGION}


end.
