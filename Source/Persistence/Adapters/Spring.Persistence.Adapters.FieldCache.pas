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

unit Spring.Persistence.Adapters.FieldCache;

{$I Spring.inc}

interface

uses
  DB,
  Spring.Collections;

type
  IFieldCache = interface(IInvokable)
    ['{11B51ABB-0C29-40CA-A2C1-623CBFF86F4F}']
    function FieldNameExists(const fieldName: string): Boolean;
    function GetFieldValue(const fieldName: string): Variant;
  end;

  TFieldCache = class(TInterfacedObject, IFieldCache)
  private
    fDataSet: TDataSet;
    fValues: IDictionary<string,TField>;
    procedure Build;
  protected
    function FieldNameExists(const fieldName: string): Boolean;
    function GetFieldValue(const fieldName: string): Variant;
  public
    constructor Create(const dataSet: TDataset);
  end;

implementation


{$REGION 'TFieldCache'}

procedure TFieldCache.Build;
var
  field: TField;
begin
  if fValues.IsEmpty then
    for field in fDataSet.Fields do
      fValues.Add(field.FieldName, field);
end;

constructor TFieldCache.Create(const dataSet: TDataset);
begin
  inherited Create;
  fDataSet := dataSet;
  fValues := TCollections.CreateDictionary<string,TField>(
    TStringComparer.OrdinalIgnoreCase);
  Build;
end;

function TFieldCache.FieldNameExists(const fieldName: string): Boolean;
begin
  Result := fValues.ContainsKey(fieldName);
end;

function TFieldCache.GetFieldValue(const fieldName: string): Variant;
begin
  Result := fValues[fieldName].Value;
end;

{$ENDREGION}


end.
