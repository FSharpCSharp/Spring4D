{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2012 Spring4D Team                           }
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

unit Spring.Collections.Sets;

interface

{$I Spring.inc}

uses
  Generics.Defaults,
  Generics.Collections,
  Spring.Collections,
  Spring.Collections.Base;

type
  THashSet<T> = class(TCollectionBase<T>, ISet<T>)
  private
    fDictionary: Generics.Collections.TDictionary<T,Integer>; // TEMP Impl
  protected
    function GetCount: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Add(const item: T); override;
    function  Remove(const item: T): Boolean; override;
    procedure Clear; override;

    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; override;
    procedure ExceptWith(const collection: IEnumerable<T>);
    procedure IntersectWith(const collection: IEnumerable<T>);
    procedure UnionWith(const collection: IEnumerable<T>);
    function SetEquals(const collection: IEnumerable<T>): Boolean;
    function Overlaps(const collection: IEnumerable<T>): Boolean;
  end;

implementation

uses
  Spring,
  Spring.Collections.Lists,
  Spring.Collections.Extensions;


{$REGION 'THashSet<T>'}

constructor THashSet<T>.Create;
begin
  inherited Create;
  fDictionary := Generics.Collections.TDictionary<T, Integer>.Create;
end;

destructor THashSet<T>.Destroy;
begin
  fDictionary.Free;
  inherited Destroy;
end;

procedure THashSet<T>.Add(const item: T);
begin
  fDictionary.AddOrSetValue(item, 0);
end;

//function THashSet<T>.Add(const item: T): Boolean;
//begin
//  Result := not fDictionary.ContainsKey(item);
//  if Result then
//  begin
//    fDictionary.Add(item, 0);
//  end;
//end;

function THashSet<T>.Remove(const item: T): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
  if Result then
    fDictionary.Remove(item);
end;

procedure THashSet<T>.Clear;
begin
  fDictionary.Clear;
end;

function THashSet<T>.Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
end;

procedure THashSet<T>.ExceptWith(const collection: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF DELPHIXE_UP}
  TArgument.CheckNotNull(collection, 'collection');
{$ELSE}
  Assert(Assigned(collection), 'collection');
{$ENDIF}

  for item in collection do
  begin
    fDictionary.Remove(item);
  end;
end;

procedure THashSet<T>.IntersectWith(const collection: IEnumerable<T>);
var
  item: T;
  list: IList<T>;
begin
{$IFDEF DELPHIXE_UP}
  TArgument.CheckNotNull(collection, 'collection');
{$ELSE}
  Assert(Assigned(collection), 'collection');
{$ENDIF}

  list := TList<T>.Create;
  for item in Self do
  begin
    if not collection.Contains(item) then
      list.Add(item);
  end;

  for item in list do
  begin
    Remove(item);
  end;
end;

procedure THashSet<T>.UnionWith(const collection: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF DELPHIXE_UP}
  TArgument.CheckNotNull(collection, 'collection');
{$ELSE}
  Assert(Assigned(collection), 'collection');
{$ENDIF}

  for item in collection do
  begin
    Add(item);
  end;
end;

function THashSet<T>.Overlaps(const collection: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF DELPHIXE_UP}
  TArgument.CheckNotNull(collection, 'collection');
{$ELSE}
  Assert(Assigned(collection), 'collection');
{$ENDIF}

  for item in collection do
  begin
    if Contains(item) then
      Exit(True)
  end;
  Result := False;
end;

function THashSet<T>.SetEquals(const collection: IEnumerable<T>): Boolean;
var
  item: T;
  localSet: ISet<T>;
begin
{$IFDEF DELPHIXE_UP}
  TArgument.CheckNotNull(collection, 'collection');
{$ELSE}
  Assert(Assigned(collection), 'collection');
{$ENDIF}

  localSet := THashSet<T>.Create;

  for item in collection do
  begin
    localSet.Add(item);
    if not Contains(item) then
      Exit(False);
  end;

  for item in Self do
  begin
    if not localSet.Contains(item) then
      Exit(False);
  end;

  Result := True;
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fDictionary.Keys);
end;

function THashSet<T>.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}

end.
