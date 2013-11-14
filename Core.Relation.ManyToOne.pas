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
unit Core.Relation.ManyToOne;

interface

uses
  Core.Relation.Abstract, Core.Interfaces, Mapping.Attributes, Generics.Collections
  ,Core.EntityCache
  ;

type
  TManyToOneRelation = class(TAbstractRelation)
  private
    FNewColumns: TColumnDataList;
    FNewEntityClass: TClass;
    FMappedByCol: ColumnAttribute;
    FNewTableName: string;
    FEntityData: TEntityData;
  protected
    function DoBuildColumnName(AColumn: TColumnData): string; virtual;
    procedure ResolveColumns(AResultset: IDBResultset); virtual;
  public
    NewEntity: TObject;

    constructor Create(); virtual;
    destructor Destroy; override;

    class function BuildColumnName(const ATableName, AMappedByColName, AColumnName: string): string;
    class function GetMappedByColumn(AFromColumn: ManyToOneAttribute; AClass: TClass): ColumnAttribute;

    procedure SetAssociation(AAtribute: TORMAttribute; AEntity: TObject; AResultset: IDBResultset); override;

    property EntityData: TEntityData read FEntityData;
    property NewEntityClass: TClass read FNewEntityClass;
    property NewColumns: TColumnDataList read FNewColumns;
  end;

implementation

uses
  Core.Exceptions
  ,Mapping.RttiExplorer
  ,Rtti
  ,StrUtils
  ,SysUtils
  ;

{ TManyToOneRelation }

class function TManyToOneRelation.BuildColumnName(const ATableName, AMappedByColName,
  AColumnName: string): string;
begin
  Result := Format('%0:S_%1:S_%2:S', [
                                      ATableName                  //table name
                                     ,AMappedByColName              //mapped by column name
                                     ,AColumnName                   //column name
                                      ]);
end;

constructor TManyToOneRelation.Create;
begin
  inherited Create;
  FNewColumns := nil;
end;

destructor TManyToOneRelation.Destroy;
begin
  if Assigned(FNewColumns) then
    FNewColumns.Free;
  inherited Destroy;
end;

function TManyToOneRelation.DoBuildColumnName(AColumn: TColumnData): string;
begin
  Result := BuildColumnName(FNewTableName, FMappedByCol.Name, AColumn.Name);
end;

class function TManyToOneRelation.GetMappedByColumn(AFromColumn: ManyToOneAttribute; AClass: TClass): ColumnAttribute;
begin
  Result := nil;
  if not TRttiExplorer.TryGetColumnByMemberName(AClass, AFromColumn.MappedBy, Result) then
    raise EORMManyToOneMappedByColumnNotFound.CreateFmt('Mapped by column ("%S") not found in the base class "%S".'
      , [AFromColumn.MappedBy, AClass.ClassName]);
end;

procedure TManyToOneRelation.ResolveColumns(AResultset: IDBResultset);
var
  LCol: TColumnData;
  LColName: string;
  i: Integer;
begin
  for i := FNewColumns.Count - 1 downto 0 do
  begin
    LCol := FNewColumns[i];
    LColName := DoBuildColumnName(LCol);
    if not AResultset.FieldnameExists(LColName) then
    begin
      FNewColumns.Delete(i);
      Continue;
    end;
    LCol.Name := LColName;
    FNewColumns[i] := LCol;
    if LCol.IsPrimaryKey then
      FNewColumns.PrimaryKeyColumn := LCol;
  end;
end;

procedure TManyToOneRelation.SetAssociation(AAtribute: TORMAttribute; AEntity: TObject; AResultset: IDBResultset);
var
  LCol: ManyToOneAttribute;
begin
  LCol := AAtribute as ManyToOneAttribute;
  //check if entity has associations attributes
  FNewEntityClass := TRttiContext.Create.GetType(LCol.GetColumnTypeInfo).AsInstance.MetaclassType;
  FEntityData := TEntityCache.Get(FNewEntityClass);
  FNewTableName := FEntityData.EntityTable.TableName;
  NewEntity := TRttiExplorer.CreateType(FNewEntityClass);
  if Assigned(FNewColumns) then
    FNewColumns.Free;
  FNewColumns := TEntityCache.CreateColumnsData(FNewEntityClass);
  //get column name from MappedBy field or property
  FMappedByCol := GetMappedByColumn(LCol, AEntity.ClassType);
  //resolve columns which we need to set
  ResolveColumns(AResultset);
  //assign newly created column to base entity property
  TRttiExplorer.SetMemberValueSimple(AEntity, LCol.ClassMemberName, NewEntity);
end;

end.
