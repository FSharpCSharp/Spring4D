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
unit Core.EntityCache;

interface

uses
  Mapping.Attributes
  ,Generics.Collections
  ;

type
  TEntityData = class
  private
    FColumns: TList<Column>;
    FForeignKeyColumns: TList<ForeignJoinColumnAttribute>;
    FPrimaryKeyColumn: Column;
    FTable: Table;
    FManyValuedColumns: TList<ManyValuedAssociation>;
  protected
    procedure SetEntityData(AClass: TClass); virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function IsTableEntity(): Boolean;
    function ColumnByMemberName(const AMemberName: string): Column;
    function ColumnByName(const AColumnName: string): Column;

    property Columns: TList<Column> read FColumns;
    property ForeignColumns: TList<ForeignJoinColumnAttribute> read FForeignKeyColumns;
    property ManyValuedColumns: TList<ManyValuedAssociation> read FManyValuedColumns;
    property PrimaryKeyColumn: Column read FPrimaryKeyColumn;

    property EntityTable: Table read FTable;
  end;

  TEntityCache = class
  private
    class var FEntities: TObjectDictionary<TClass,TEntityData>;
  public
    class constructor Create;
    class destructor Destroy;

    class function Get(AClass: TClass): TEntityData;
    class function TryGet(AClass: TClass; out AEntityData: TEntityData): Boolean;
    class function GetColumns(AClass: TClass): TList<Column>;


    class property Entities: TObjectDictionary<TClass, TEntityData> read FEntities;
  end;


implementation

uses
  Mapping.RttiExplorer
  ,Core.Exceptions
  ,SysUtils
  ;


{ TEntityData }

function TEntityData.ColumnByMemberName(const AMemberName: string): Column;
begin
  for Result in FColumns do
  begin
    if SameText(Result.ClassMemberName, AMemberName) then
      Exit;
  end;
  Result := nil;
end;

function TEntityData.ColumnByName(const AColumnName: string): Column;
begin
  for Result in FColumns do
  begin
    if SameText(Result.Name, AColumnName) then
      Exit;
  end;
  Result := nil;  
end;

constructor TEntityData.Create;
begin
  inherited Create;
  FColumns := TList<Column>.Create;
  FPrimaryKeyColumn := nil;
  FTable := nil;
  FForeignKeyColumns := TList<ForeignJoinColumnAttribute>.Create;
  FManyValuedColumns := TList<ManyValuedAssociation>.Create;
end;

destructor TEntityData.Destroy;
begin
  FColumns.Free;
  FForeignKeyColumns.Free;
  FManyValuedColumns.Free;
  inherited Destroy;
end;


function TEntityData.IsTableEntity: Boolean;
begin
  Result := Assigned(FTable);
end;

procedure TEntityData.SetEntityData(AClass: TClass);
begin
  TRttiExplorer.GetColumns(AClass, FColumns);
  FPrimaryKeyColumn := TRttiExplorer.GetPrimaryKeyColumn(AClass);
  FTable := TRttiExplorer.GetTable(AClass);
  TRttiExplorer.GetClassMembers<ForeignJoinColumnAttribute>(AClass, FForeignKeyColumns);
  TRttiExplorer.GetClassMembers<ManyValuedAssociation>(AClass, FManyValuedColumns);
end;

{ TEntityCache }

class constructor TEntityCache.Create;
begin
  FEntities := TObjectDictionary<TClass,TEntityData>.Create([doOwnsValues], 100);
end;

class destructor TEntityCache.Destroy;
begin
  FEntities.Free;
end;



class function TEntityCache.Get(AClass: TClass): TEntityData;
begin
  if not TryGet(AClass, Result) then
  begin
    Result := TEntityData.Create;
    Result.SetEntityData(AClass);
    FEntities.Add(AClass, Result);
  end;
end;

class function TEntityCache.GetColumns(AClass: TClass): TList<Column>;
begin
  Result := Get(AClass).Columns;
end;

class function TEntityCache.TryGet(AClass: TClass; out AEntityData: TEntityData): Boolean;
begin
  Result := FEntities.TryGetValue(AClass, AEntityData);
end;

end.
