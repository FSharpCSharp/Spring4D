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

unit Spring.Persistence.Core.EntityMap;

{$I Spring.inc}

interface

uses
  Rtti,
  Spring.Collections;

type
  TEntityMapKey = string;

  TEntityMap = class
  private
    FMap: IDictionary<TEntityMapKey,TObject>;
  protected
    function GetObjectKey(AObject: TObject): TEntityMapKey; virtual;
  public
    constructor Create(AOwnsValues: Boolean); virtual;

    function IsMapped(AObject: TObject): Boolean;
    function IsIDMapped: Boolean;
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
  Generics.Defaults,
  Variants,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Core.Utils;

{ TEntityMap }

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
    LId := TRttiExplorer.GetMemberValue(AObject, LPrimaryKeyCol.ClassMemberName)
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
