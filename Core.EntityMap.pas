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
unit Core.EntityMap;

interface

uses
  Generics.Collections, Rtti;

type
  TEntityMapKey = record
  private
    FModelClass: TClass;
    FID: TValue;
  public
    function GetIDAsInt(): Int64;

    property ModelClass: TClass read FModelClass write FModelClass;
    property ID: TValue read FID write FID;
  end;

  TEntityMap = class
  private
    FMap: TObjectDictionary<TEntityMapKey,TObject>;
  protected
    function GetObjectKey(AObject: TObject): TEntityMapKey; virtual;
  public
    constructor Create(AOwnsValues: Boolean); virtual;
    destructor Destroy; override;

    function IsMapped(AObject: TObject): Boolean;
    function IsIDMapped(): Boolean;
    procedure Add(AObject: TObject);
    procedure AddOrReplace(AObject: TObject);

    function Get(AObject: TObject): TObject;
    procedure Remove(AObject: TObject);
    procedure Replace(AObject: TObject);
    procedure Clear(AAll: Boolean);
    function GetList(): TList<TObject>;
    function HasIdValue(AObject: TObject): Boolean;

  end;

implementation

uses
  Core.Exceptions
  ,Core.Reflection
  ,Core.EntityCache
  ,Mapping.Attributes
  ,Mapping.RttiExplorer
  ,Generics.Defaults
  ;

{ TEntityMapKey }

function TEntityMapKey.GetIDAsInt: Int64;
var
  LResult: TValue;
  bFree: Boolean;
begin
  Result := -1;
  if FID.TryConvert(TypeInfo(Int64), LResult, bFree) then
    Result := LResult.AsInt64;
end;

{ TModelMap }

procedure TEntityMap.Add(AObject: TObject);
var
  LKey: TEntityMapKey;
begin
  Assert(Assigned(AObject), 'Entity not assigned');

  LKey := GetObjectKey(AObject);
  FMap.Add(LKey, AObject);
end;

procedure TEntityMap.AddOrReplace(AObject: TObject);
var
  LKey: TEntityMapKey;
begin
  Assert(Assigned(AObject), 'Entity not assigned');

  LKey := GetObjectKey(AObject);
  FMap.AddOrSetValue(LKey, AObject);
end;

procedure TEntityMap.Clear(AAll: Boolean);
begin
  FMap.Clear;
end;

constructor TEntityMap.Create(AOwnsValues: Boolean);
var
  LOwnerships: TDictionaryOwnerships;
  LComparer: IEqualityComparer<TEntityMapKey>;
begin
  inherited Create;

  if AOwnsValues then
    LOwnerships := [doOwnsValues]
  else
    LOwnerships := [];

  LComparer := TDelegatedEqualityComparer<TEntityMapKey>.Construct(
    function(const Left, Right: TEntityMapKey): Boolean
    begin
      Result := (Left.ModelClass = Right.ModelClass) and (SameValue(Left.ID, Right.ID));
    end,
    function(const Value: TEntityMapKey): Integer
    begin
      Result := Value.GetIDAsInt - NativeInt(Value.ModelClass);
    end);

  FMap := TObjectDictionary<TEntityMapKey,TObject>.Create(LOwnerships, LComparer);
end;

destructor TEntityMap.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

function TEntityMap.Get(AObject: TObject): TObject;
var
  LKey: TEntityMapKey;
begin
  LKey := GetObjectKey(AObject);
  Result := FMap[LKey];
end;

function TEntityMap.GetList: TList<TObject>;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityMap.GetObjectKey(AObject: TObject): TEntityMapKey;
var
  LPrimaryKeyCol: ColumnAttribute;
begin
  Result.FModelClass := AObject.ClassType;
  LPrimaryKeyCol := TEntityCache.Get(AObject.ClassType).PrimaryKeyColumn;
  if Assigned(LPrimaryKeyCol) then
  begin
    Result.FID := TRttiExplorer.GetMemberValue(AObject, LPrimaryKeyCol.ClassMemberName);
  end
  else
    Result.FID := TValue.Empty;
end;

function TEntityMap.HasIdValue(AObject: TObject): Boolean;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityMap.IsIDMapped: Boolean;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityMap.IsMapped(AObject: TObject): Boolean;
var
  LKey: TEntityMapKey;
begin
  LKey := GetObjectKey(AObject);
  Result := FMap.ContainsKey(LKey);
end;

procedure TEntityMap.Remove(AObject: TObject);
var
  LKey: TEntityMapKey;
begin
  LKey := GetObjectKey(AObject);
  FMap.Remove(LKey);
end;

procedure TEntityMap.Replace(AObject: TObject);
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

end.
