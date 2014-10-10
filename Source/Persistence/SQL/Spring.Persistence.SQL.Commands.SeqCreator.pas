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

unit Spring.Persistence.SQL.Commands.SeqCreator;

interface

uses
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Responsible for building and executing statements which create
  ///   sequences in the database.
  /// </summary>
  TSequenceCreateExecutor = class(TAbstractCommandExecutor)
  private
    fSequence: TCreateSequenceCommand;
  protected
    function SequenceExists: Boolean; virtual;
    function GetCommand: TDMLCommand; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Build(entityClass: TClass); override;
    procedure Execute(const entity: TObject); override;
    procedure CreateSequence(entityClass: TClass);
  end;

implementation

uses
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces;


{$REGION 'TSequenceCreateCommand'}

constructor TSequenceCreateExecutor.Create;
begin
  inherited Create;
  fSequence := TCreateSequenceCommand.Create(nil);
end;

destructor TSequenceCreateExecutor.Destroy;
begin
  fSequence.Free;
  inherited Destroy;
end;

procedure TSequenceCreateExecutor.Build(entityClass: TClass);
begin
  inherited EntityClass := entityClass;
  fSequence.Sequence := TEntityCache.Get(entityClass).Sequence;
  SQL := '';
  if Assigned(fSequence.Sequence) then
  begin
    fSequence.SequenceExists := SequenceExists;
    SQL := Generator.GenerateCreateSequence(fSequence);
  end;
end;

procedure TSequenceCreateExecutor.CreateSequence(entityClass: TClass);
begin
  Execute(nil);
end;

procedure TSequenceCreateExecutor.Execute(const entity: TObject);
var
  LStmt: IDBStatement;
begin
  if SQL = '' then
    Exit;

  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);
  LStmt.Execute;
end;

function TSequenceCreateExecutor.GetCommand: TDMLCommand;
begin
  Result := nil;
end;

function TSequenceCreateExecutor.SequenceExists: Boolean;
var
  LSqlSequenceCount: string;
  LStmt: IDBStatement;
  LResults: IDBResultset;
begin
  Result := False;
  LSqlSequenceCount := Generator.GetSQLSequenceCount(fSequence.Sequence.SequenceName);
  if LSqlSequenceCount <> '' then
  try
    LStmt := Connection.CreateStatement;
    LStmt.SetSQLCommand(LSqlSequenceCount);
    LResults := LStmt.ExecuteQuery;
    Result := not LResults.IsEmpty;
  except
    Result := False;
  end;
end;

{$ENDREGION}


end.
