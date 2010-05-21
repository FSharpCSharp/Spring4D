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

unit Spring.Numbering.Utils;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  DB,
  ADODB,
  ADOInt,
  Variants,
  Spring,
  Spring.Numbering;

type
  /// <summary>
  /// Provides a simple implementation of number source using ADODB & MS SQL Server.
  /// </summary>
  TAdoDBNumberSource = class(TInterfacedObject, INumberSource)
  private
    const
      fCSelectStatement =
        'SELECT * ' +
        'FROM %0:S ' +
        'WITH (ROWLOCK) ' +
        'WHERE %1:S = ''%%S''';
      fCUpdateStatement =
        'UPDATE %0:S ' +
        'SET %1:S = ''%%0:S'' ' +
        'FROM %0:S ' +
        'WITH (ROWLOCK) ' +
        'WHERE %2:S = ''%%1:S''';
      fCGetDateStatement =
        'SELECT GETDATE() AS Value';
  private
    fConnection: TAdoConnection;
    fDataSet: TAdoQuery;
    fTableName: string;
    fKeyFieldName: string;
    fValueFieldName: string;
    fKeyFieldValue: string;
    fSelectSql: string;
    fUpdateSql: string;
  protected
    function GetSqlServerDateTime: TDateTime;
  public
    constructor Create(connection: TADOConnection;
      const tableName, keyFieldName, valueFieldName, keyFieldValue: string);
    destructor Destroy; override;
    procedure UpdateNumber(proc: TNumberProc);
  end;

implementation

uses
  Math;

{$REGION 'TAdoDBNumberSource'}

constructor TAdoDBNumberSource.Create(connection: TADOConnection;
  const tableName, keyFieldName, valueFieldName, keyFieldValue: string);
begin
  inherited Create;
  fConnection := connection;
  fTableName := tableName;
  fKeyFieldName := keyFieldName;
  fValueFieldName := valueFieldName;
  fKeyFieldValue := keyFieldValue;
  fDataSet := TADOQuery.Create(nil);
  fDataSet.Connection := fConnection;
  fSelectSql := Format(fCSelectStatement, [fTableName, fKeyFieldName]);
  fUpdateSql := Format(fCUpdateStatement, [fTableName, fValueFieldName, fKeyFieldName]);
end;

destructor TAdoDBNumberSource.Destroy;
begin
  fDataSet.Free;
  inherited Destroy;
end;

function TAdoDBNumberSource.GetSqlServerDateTime: TDateTime;
var
  recordSet: _RecordSet;
  field: ADOInt.Field;
begin
  recordSet := fConnection.Execute(fCGetDateStatement);
  Assert(recordSet <> nil, 'recordSet should not be nil.');
  field := recordSet.Fields[0];
  Result := VarToDateTime(field.Value);
end;

procedure TAdoDBNumberSource.UpdateNumber(proc: TNumberProc);
var
  oldIsolationLevel: TIsolationLevel;
  updateSql: string;
  lastNumber: string;
begin
  TArgument.CheckNotNull(Assigned(proc), 'proc');
  with fConnection do
  begin
    oldIsolationLevel := IsolationLevel;
    IsolationLevel := ilRepeatableRead;
    try
      BeginTrans;
      try
        fDataSet.SQL.Text := Format(fSelectSql, [fKeyFieldValue]);
        fDataSet.Open;
        try
          lastNumber := Trim(VarToStrDef(fDataSet.FieldByName(fValueFieldName).Value, ''));
          proc(lastNumber);
          updateSql := Format(fUpdateSql, [lastNumber, fKeyFieldValue]);
          Execute(updateSql);
        finally
          fDataSet.Close;
        end;
        CommitTrans;
      except
        RollbackTrans;
        raise;
      end;
    finally
      IsolationLevel := oldIsolationLevel;
    end;
  end;
end;

{$ENDREGION}

end.
