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
    FPrimaryKeyColumn: Column;
    FTable: Table;
  protected
    procedure SetEntityData(AClass: TClass); virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    property Columns: TList<Column> read FColumns;
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
  ;


{ TEntityData }

constructor TEntityData.Create;
begin
  inherited Create;
  FColumns := nil;
  FPrimaryKeyColumn := nil;
  FTable := nil;
end;

destructor TEntityData.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;


procedure TEntityData.SetEntityData(AClass: TClass);
begin
  FColumns := TRttiExplorer.GetColumns(AClass);
  FPrimaryKeyColumn := TRttiExplorer.GetPrimaryKeyColumn(AClass);
  FTable := TRttiExplorer.GetTable(AClass);
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
