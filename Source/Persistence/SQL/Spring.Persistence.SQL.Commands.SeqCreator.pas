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

unit Spring.Persistence.SQL.Commands.SeqCreator;

interface

uses
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building and executing statements which create
  ///	  sequences in the database.
  ///	</summary>
  {$ENDREGION}
  TSequenceCreateExecutor = class(TAbstractCommandExecutor)
  private
    FSequence: TCreateSequenceCommand;
  protected
    function SequenceExists: Boolean; virtual;
    function GetCommand: TDMLCommand; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;

    procedure Execute(AEntity: TObject); override;

    procedure CreateSequence(AEntity: TClass);
  end;

implementation

uses
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces;

{ TSequenceCreateCommand }

procedure TSequenceCreateExecutor.Build(AClass: TClass);
begin
  EntityClass := AClass;
  FSequence.Sequence := TEntityCache.Get(AClass).Sequence;
  SQL := '';
  if Assigned(FSequence.Sequence) then
  begin
    FSequence.SequenceExists := SequenceExists;
    SQL := Generator.GenerateCreateSequence(FSequence);
  end;
end;

constructor TSequenceCreateExecutor.Create;
begin
  inherited Create;
  FSequence := TCreateSequenceCommand.Create(nil);
end;

procedure TSequenceCreateExecutor.CreateSequence(AEntity: TClass);
begin
  Execute(nil);
end;

destructor TSequenceCreateExecutor.Destroy;
begin
  FSequence.Free;
  inherited Destroy;
end;

procedure TSequenceCreateExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
begin
  if (SQL = '') then
    Exit;

  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);
  //inherited only when SQL's are constructed
  inherited Execute(AEntity);

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
  LSqlSequenceCount := Generator.GetSQLSequenceCount(FSequence.Sequence.SequenceName);
  if (LSqlSequenceCount <> '') then
  begin
    try
      LStmt := Connection.CreateStatement;
      LStmt.SetSQLCommand(LSqlSequenceCount);
      LResults := LStmt.ExecuteQuery;
      Result := not LResults.IsEmpty;
    except
      Result := False;
    end;
  end;
end;

end.
