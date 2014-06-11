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
unit SQL.Commands.Update;

interface

uses
  SQL.AbstractCommandExecutor, SQL.Types, SQL.Commands, SQL.Params, Generics.Collections
  , Mapping.Attributes, Core.EntityMap, Classes, Core.Interfaces, Core.EntityCache;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building and executing <c>update</c> statements.
  ///	</summary>
  {$ENDREGION}
  TUpdateExecutor = class(TAbstractCommandExecutor)
  private
    FTable: TSQLTable;
    FCommand: TUpdateCommand;
    FColumns: TList<ColumnAttribute>;
    FEntityMap: TEntityMap;
    FEntityCache: TEntityData;
    FMapped: Boolean;
  protected
    function GetCommand: TDMLCommand; override;
    function TryIncrementVersionFor(AEntity: TObject): Boolean; virtual;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;
    procedure BuildParams(AEntity: TObject); override;

    procedure Execute(AEntity: TObject); override;
    procedure Update(AEntity: TObject; AEntity2: TObject); overload;

    property EntityMap: TEntityMap read FEntityMap write FEntityMap;
  end;

implementation

uses
  Core.Exceptions
  ,Core.Utils
  ,Mapping.RttiExplorer
  ,SysUtils
  ,Rtti
  ,Core.Reflection
  ,Variants
  ;

{ TUpdateCommand }

procedure TUpdateExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
  LDirtyObject: TObject;
begin
  Assert(Assigned(AEntity));

  if FEntityCache.HasVersionColumn and not TryIncrementVersionFor(AEntity) then
  begin
    raise EORMOptimisticLockException.Create(AEntity);
  end;

  LStmt := Connection.CreateStatement;

  FMapped := FEntityMap.IsMapped(AEntity);
  if FMapped then
  begin
    LDirtyObject := FEntityMap.Get(AEntity);
    FColumns.Clear;
    TRttiExplorer.GetChangedMembers(AEntity, LDirtyObject, FColumns);
    if (FColumns.Count = 1) and (FColumns.First.IsVersionColumn) then
      Exit;
  end;

  FCommand.SetTable(FColumns);
  FCommand.Entity := AEntity;

  SQL := Generator.GenerateUpdate(FCommand);

  if (SQL = '') then
    Exit;

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

function TUpdateExecutor.GetCommand: TDMLCommand;
begin
  Result := FCommand;
end;

function TUpdateExecutor.TryIncrementVersionFor(AEntity: TObject): Boolean;
var
  LStatement: IDBStatement;
  LVersionValue, LPKValue: TValue;
  LQuery: Variant;
  LQueryMetadata: TQueryMetadata;
begin
  LStatement := Connection.CreateStatement;
  LVersionValue := TRttiExplorer.GetMemberValue(AEntity, FEntityCache.VersionColumn.ClassMemberName);
  LPKValue := TRttiExplorer.GetMemberValue(AEntity, FEntityCache.PrimaryKeyColumn.ClassMemberName);
  LQuery := Generator.GetUpdateVersionFieldQuery(FCommand, FEntityCache.VersionColumn
    , TUtils.AsVariant(LVersionValue), TUtils.AsVariant(LPKValue));
  LQueryMetadata.QueryType := ctUpdateVersion;
  case VarType(LQuery) of
    varUString, varString, varStrArg, varUStrArg, varOleStr: LStatement.SetSQLCommand(LQuery)
    else
    begin
      LStatement.SetSQLCommand(Format('S[%S]{}', [FCommand.Table.Name]));
      LStatement.SetQuery(LQueryMetadata, LQuery);
    end;
  end;
  Result := (LStatement.Execute > 0);
  if Result then
    TRttiExplorer.SetMemberValueSimple(AEntity, FEntityCache.VersionColumn.ClassMemberName, LVersionValue.AsInteger + 1);
end;

procedure TUpdateExecutor.Build(AClass: TClass);
var
  LAtrTable: TableAttribute;
begin
  EntityClass := AClass;
  FEntityCache := TEntityCache.Get(EntityClass);
  LAtrTable := FEntityCache.EntityTable;

  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%S"', [AClass.ClassName]);

  FTable.SetFromAttribute(LAtrTable);
  FColumns.Clear;
  FColumns.AddRange(FEntityCache.Columns);

  FCommand.PrimaryKeyColumn := FEntityCache.PrimaryKeyColumn;
   //add fields to tsqltable
//  FCommand.SetTable(FColumns);

 // SQL := Generator.GenerateUpdate(FCommand);
end;

procedure TUpdateExecutor.BuildParams(AEntity: TObject);
var
  LParam: TDBParam;
  LColumn: ColumnAttribute;
begin
  inherited BuildParams(AEntity);

  for LColumn in FColumns do
  begin
    if LColumn.CanUpdate then
    begin
      LParam := CreateParam(AEntity, LColumn);
      SQLParameters.Add(LParam);
    end;
  end;

  if Assigned(FCommand.PrimaryKeyColumn) then
  begin
    LParam := CreateParam(AEntity, FCommand.PrimaryKeyColumn);
    SQLParameters.Add(LParam);
  end;
end;

constructor TUpdateExecutor.Create();
begin
  inherited Create();
  FTable := TSQLTable.Create;
  FColumns := TList<ColumnAttribute>.Create;
  FCommand := TUpdateCommand.Create(FTable);
end;

destructor TUpdateExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  FColumns.Free;
  inherited Destroy;
end;

procedure TUpdateExecutor.Update(AEntity, AEntity2: TObject);
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

end.
