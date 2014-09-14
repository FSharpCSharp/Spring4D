(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit Spring.Persistence.SQL.Commands.Delete;

interface

uses
  Spring.Persistence.SQL.AbstractCommandExecutor, Spring.Persistence.SQL.Types
  , Spring.Persistence.SQL.Commands, Spring.Persistence.SQL.Params
  , Spring.Persistence.Mapping.Attributes, Spring.Persistence.Core.Interfaces, Rtti;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building and executing <c>delete</c> statements. 
  ///	</summary>
  {$ENDREGION}
  TDeleteExecutor = class(TAbstractCommandExecutor)
  private
    FTable: TSQLTable;
    FCommand: TDeleteCommand;
    FPrimaryKeyColumnName: string;
    FEntity: TObject;
  protected
    function GetCommand: TDMLCommand; override;

    function GetPrimaryKeyValue(): TValue; virtual;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;
    procedure BuildParams(AEntity: TObject); override;

    procedure Execute(AEntity: TObject); override;
  end;

  TDeleteByValueExecutor = class(TDeleteExecutor)
  private
    FPrimaryKeyValue: TValue;
  protected
    function GetPrimaryKeyValue(): TValue; override;
  public
    procedure Execute(AEntity: TObject); override;

    property PrimaryKeyValue: TValue read FPrimaryKeyValue write FPrimaryKeyValue;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions
  ,Spring.Persistence.Core.Utils
  ,Spring.Persistence.Mapping.RttiExplorer
  ;

{ TDeleteCommand }

procedure TDeleteExecutor.Build(AClass: TClass);
var
  LAtrTable: TableAttribute;
begin
  EntityClass := AClass;
  LAtrTable := TRttiExplorer.GetTable(EntityClass);
  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%S"', [AClass.ClassName]);

  FTable.SetFromAttribute(LAtrTable);

  FPrimaryKeyColumnName := TRttiExplorer.GetPrimaryKeyColumnName(EntityClass);
   //add fields to tsqltable
  FCommand.PrimaryKeyColumnName := FPrimaryKeyColumnName;
  FCommand.SetTable(nil);

  SQL := Generator.GenerateDelete(FCommand);
end;

procedure TDeleteExecutor.BuildParams(AEntity: TObject);
var
  LParam: TDBParam;
  LVal: TValue;
begin
  Assert(FPrimaryKeyColumnName <> '');
  FEntity := AEntity;
  inherited BuildParams(AEntity);

  LParam := TDBParam.Create;
  LParam.Name := Command.GetExistingParameterName(FPrimaryKeyColumnName);
  LVal :=  GetPrimaryKeyValue();
  LParam.Value := TUtils.AsVariant(LVal);
  SQLParameters.Add(LParam);
end;

constructor TDeleteExecutor.Create();
begin
  inherited Create();
  FTable := TSQLTable.Create;
  FCommand := TDeleteCommand.Create(FTable);
  FPrimaryKeyColumnName := '';
end;

procedure TDeleteExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
begin
  Assert(Assigned(AEntity));

  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);

  BuildParams(AEntity);
  try
    LStmt.SetParams(SQLParameters);

    inherited Execute(AEntity);

    LStmt.Execute();
  finally
    LStmt := nil;
  end;
end;

function TDeleteExecutor.GetCommand: TDMLCommand;
begin
  Result := FCommand;
end;

function TDeleteExecutor.GetPrimaryKeyValue: TValue;
begin
  Result := TRttiExplorer.GetPrimaryKeyValue(FEntity);
end;

destructor TDeleteExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  inherited Destroy;
end;

{ TDeleteByValueExecutor }

procedure TDeleteByValueExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
begin
  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);

  BuildParams(AEntity);
  try
    LStmt.SetParams(SQLParameters);

    LStmt.Execute();
  finally
    LStmt := nil;
  end;
end;

function TDeleteByValueExecutor.GetPrimaryKeyValue: TValue;
begin
  Result := FPrimaryKeyValue;
end;

end.
