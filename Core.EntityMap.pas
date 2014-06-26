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
  Spring.Collections, Rtti;

type
  TEntityMapKey = string;

  TEntityMap = class
  private
    FMap: IDictionary<TEntityMapKey,TObject>;
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
  ,Core.Utils
  ,Variants
  ;

{ TEntityMap }

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
begin
  inherited Create;

  if AOwnsValues then
    LOwnerships := [doOwnsValues]
  else
    LOwnerships := [];

  FMap := TCollections.CreateDictionary<TEntityMapKey,TObject>(LOwnerships);
end;

destructor TEntityMap.Destroy;
begin
  inherited Destroy;
end;

function TEntityMap.Get(AObject: TObject): TObject;
var
  LKey: TEntityMapKey;
begin
  LKey := GetObjectKey(AObject);
  Result := FMap[LKey];
end;

function TEntityMap.GetObjectKey(AObject: TObject): TEntityMapKey;
var
  LPrimaryKeyCol: ColumnAttribute;
  LId: TValue;
begin
  LPrimaryKeyCol := TEntityCache.Get(AObject.ClassType).PrimaryKeyColumn;
  if Assigned(LPrimaryKeyCol) then
  begin
    LId := TRttiExplorer.GetMemberValue(AObject, LPrimaryKeyCol.ClassMemberName);
  end
  else
    LId := TValue.Empty;

  Result := AObject.ClassName + '_' + LId.ToString;
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
