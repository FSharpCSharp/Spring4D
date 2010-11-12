{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

/// <preliminary />
unit Spring.Helpers.Vcl;

{$I Spring.inc}

interface

uses
  DB,
  Rtti,
  Spring
  ;

type
  (*
  TApplicationHelper = class helper for TApplication
  public
    function GetFileName(const releativePath: string): string;
    property Path: string read GetPath;
  end;
  *)

  /// <preliminary />
  TDataSetHelper = class helper for TDataSet
  public
    function GetValueOrDefault<T>(const fieldName: string; const default: T): T;
    procedure CopyRecordFrom(source: TDataSet);
//    procedure CopyRecordTo(target: TDataSet);
//    procedure Reopen;
//    procedure EnumerateRows(proc: TProc<TDataSet>);
//    procedure Clear;
//    property IsModified: Boolean;
  end;

  /// <preliminary />
  TFieldHelper = class helper for TField
  public
    function GetValueOrDefault<T>(const default: T): T;
//    property IsNullOrWhiteSpace: Boolean;
//    property IsModified: Boolean;
  end;

implementation


{$REGION 'DB'}

{ TDataSetHelper }

procedure TDataSetHelper.CopyRecordFrom(source: TDataSet);
var
  field: TField;
  sourceField: TField;
begin
  TArgument.CheckNotNull(source, 'source');
  for field in Fields do
  begin
    if not field.ReadOnly and (field.FieldKind = fkData) then
    begin
      sourceField := source.FindField(field.FieldName);
      if sourceField <> nil then
      begin
        field.Value := sourceField.Value;
      end;
    end;
  end;
end;

function TDataSetHelper.GetValueOrDefault<T>(const fieldName: string;
  const default: T): T;
var
  field: TField;
begin
  field := FieldByName(fieldName);
  Result := field.GetValueOrDefault<T>(default);
end;

{ TFieldHelper }

function TFieldHelper.GetValueOrDefault<T>(const default: T): T;
var
  v: TValue;
begin
  if not IsNull then
  begin
    v := TValue.FromVariant(Value);
    Result := v.AsType<T>;
  end
  else
  begin
    Result := default;
  end;
end;

{$ENDREGION}

end.
