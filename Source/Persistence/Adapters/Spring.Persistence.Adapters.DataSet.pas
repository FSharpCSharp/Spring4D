{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2015 Spring4D Team                           }
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

unit Spring.Persistence.Adapters.DataSet;

interface

uses
  DB,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces;

type
  TDataSetResultSetAdapter<T: TDataSet> = class(TDriverResultSetAdapter<T>)
  private
    fFieldCache: IFieldCache;
  public
    constructor Create(const dataSet: T;
      const exceptionHandler: IORMExceptionHandler); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; override;
    function GetFieldValue(const fieldName: string): Variant; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  end;

implementation

uses
  Spring.Persistence.Adapters.FieldCache;


{$REGION 'TDataSetResultSetAdapter<T>'}

constructor TDataSetResultSetAdapter<T>.Create(const dataSet: T;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(DataSet, exceptionHandler);
  dataSet.DisableControls;
  fFieldCache := TFieldCache.Create(dataSet);
end;

destructor TDataSetResultSetAdapter<T>.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  DataSet.Free;
{$ELSE}
  Dataset.DisposeOf;
{$ENDIF}
  inherited Destroy;
end;

function TDataSetResultSetAdapter<T>.FieldExists(
  const fieldName: string): Boolean;
begin
  Result := fFieldCache.FieldExists(fieldName);
end;

function TDataSetResultSetAdapter<T>.GetFieldCount: Integer;
begin
  Result := DataSet.FieldCount;
end;

function TDataSetResultSetAdapter<T>.GetFieldName(index: Integer): string;
begin
  Result := DataSet.Fields[index].FieldName;
end;

function TDataSetResultSetAdapter<T>.GetFieldValue(index: Integer): Variant;
begin
  try
    Result := DataSet.Fields[index].Value;
  except
    raise HandleException;
  end;
end;

function TDataSetResultSetAdapter<T>.GetFieldValue(
  const fieldName: string): Variant;
begin
  try
    Result := fFieldCache.GetFieldValue(fieldName);
  except
    raise HandleException;
  end;
end;

function TDataSetResultSetAdapter<T>.IsEmpty: Boolean;
begin
  Result := DataSet.Eof;
end;

function TDataSetResultSetAdapter<T>.Next: Boolean;
begin
  try
    DataSet.Next;
  except
    raise HandleException;
  end;
  Result := not DataSet.Eof;
end;

{$ENDREGION}

end.
