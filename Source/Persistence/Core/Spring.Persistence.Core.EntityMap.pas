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

unit Spring.Persistence.Core.EntityMap;

interface

uses
  Spring.Collections;

type
  TEntityMapKey = string;

  TEntityMap = class
  private
    fMap: IDictionary<TEntityMapKey,TObject>;
  protected
    function GetObjectKey(const instance: TObject): TEntityMapKey;
  public
    constructor Create(ownsValues: Boolean);

    function IsMapped(const instance: TObject): Boolean;
    function IsIDMapped: Boolean;
    procedure Add(const instance: TObject);
    procedure AddOrReplace(const instance: TObject);

    function Get(const instance: TObject): TObject;
    procedure Remove(const instance: TObject);
    procedure Replace(const instance: TObject);
    procedure Clear;
    function HasIdValue(const instance: TObject): Boolean;
  end;

implementation

uses
  Spring,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Core.Utils;


{$REGION 'TEntityMap'}

constructor TEntityMap.Create(ownsValues: Boolean);
var
  LOwnerships: TDictionaryOwnerships;
begin
  inherited Create;

  if ownsValues then
    LOwnerships := [doOwnsValues]
  else
    LOwnerships := [];

  fMap := TCollections.CreateDictionary<TEntityMapKey,TObject>(LOwnerships);
end;

procedure TEntityMap.Add(const instance: TObject);
var
  LKey: TEntityMapKey;
begin
  Assert(Assigned(instance), 'Entity not assigned');

  LKey := GetObjectKey(instance);
  fMap.Add(LKey, instance);
end;

procedure TEntityMap.AddOrReplace(const instance: TObject);
var
  LKey: TEntityMapKey;
begin
  Assert(Assigned(instance), 'Entity not assigned');

  LKey := GetObjectKey(instance);
  fMap.AddOrSetValue(LKey, instance);
end;

procedure TEntityMap.Clear;
begin
  fMap.Clear;
end;

function TEntityMap.Get(const instance: TObject): TObject;
var
  LKey: TEntityMapKey;
begin
  LKey := GetObjectKey(instance);
  Result := fMap[LKey];
end;

function TEntityMap.GetObjectKey(const instance: TObject): TEntityMapKey;
var
  LPrimaryKeyCol: ColumnAttribute;
  LId: TValue;
begin
  LPrimaryKeyCol := TEntityCache.Get(instance.ClassType).PrimaryKeyColumn;
  if Assigned(LPrimaryKeyCol) then
    LId := TRttiExplorer.GetMemberValue(instance, LPrimaryKeyCol.MemberName)
  else
    LId := TValue.Empty;

  Result := instance.ClassName + '_' + LId.ToString;
end;

function TEntityMap.HasIdValue(const instance: TObject): Boolean;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityMap.IsIDMapped: Boolean;
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

function TEntityMap.IsMapped(const instance: TObject): Boolean;
var
  LKey: TEntityMapKey;
begin
  LKey := GetObjectKey(instance);
  Result := fMap.ContainsKey(LKey);
end;

procedure TEntityMap.Remove(const instance: TObject);
var
  LKey: TEntityMapKey;
begin
  LKey := GetObjectKey(instance);
  fMap.Remove(LKey);
end;

procedure TEntityMap.Replace(const instance: TObject);
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

{$ENDREGION}


end.
