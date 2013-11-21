{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2013 Spring4D Team                           }
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
  THashSet<T> = class(TCollectionBase<T>, ISet<T>, ISet)
  private
    type
      TGenericDictionary = Generics.Collections.TDictionary<T, Integer>;
  private
    fDictionary: TGenericDictionary;
  protected
    function GetCount: Integer; override;

    procedure NonGenericExceptWith(const collection: IEnumerable);
    procedure NonGenericIntersectWith(const collection: IEnumerable);
    procedure NonGenericUnionWith(const collection: IEnumerable);
    function NonGenericSetEquals(const collection: IEnumerable): Boolean;
    function NonGenericOverlaps(const collection: IEnumerable): Boolean;

    procedure ISet.ExceptWith = NonGenericExceptWith;
    procedure ISet.IntersectWith = NonGenericIntersectWith;
    procedure ISet.UnionWith = NonGenericUnionWith;
    function ISet.SetEquals = NonGenericSetEquals;
    function ISet.Overlaps = NonGenericOverlaps;

    function AddInternal(const item: T): Boolean;
    function ISet<T>.Add = AddInternal;
  public
    constructor Create; overload;
    constructor Create(const comparer: IEqualityComparer<T>); overload;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Add(const item: T); override;
    function Remove(const item: T): Boolean; override;
    procedure Clear; override;

    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; override;
    procedure ExceptWith(const collection: IEnumerable<T>);
    procedure IntersectWith(const collection: IEnumerable<T>);
    procedure UnionWith(const collection: IEnumerable<T>);
    function SetEquals(const collection: IEnumerable<T>): Boolean;
    function Overlaps(const collection: IEnumerable<T>): Boolean;
    function AsSet: ISet;
  end;

implementation

uses
  Spring,
  Spring.Collections.Lists,
  Spring.Collections.Extensions;


{$REGION 'THashSet<T>'}

constructor THashSet<T>.Create;
var
  // use variable to pass nil because of codegen bug in XE2 and XE3 in x64
  comparer: IEqualityComparer<T>;
begin
  Create(comparer);
end;

constructor THashSet<T>.Create(const comparer: IEqualityComparer<T>);
begin
  inherited Create;
  fDictionary := TGenericDictionary.Create(comparer);
end;

destructor THashSet<T>.Destroy;
begin
  fDictionary.Free;
  inherited Destroy;
end;

procedure THashSet<T>.Add(const item: T);
begin
  AddInternal(item);
end;

function THashSet<T>.AddInternal(const item: T): Boolean;
begin
  Result := not fDictionary.ContainsKey(item);
  if Result then
    fDictionary.Add(item, 0);
end;

function THashSet<T>.Remove(const item: T): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
  if Result then
    fDictionary.Remove(item);
end;

function THashSet<T>.AsSet: ISet;
begin
  Result := Self;
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
  Guard.CheckNotNull(collection <> nil, 'collection');

  for item in collection do
    fDictionary.Remove(item);
end;

procedure THashSet<T>.IntersectWith(const collection: IEnumerable<T>);
var
  item: T;
  list: IList<T>;
begin
  Guard.CheckNotNull(collection <> nil, 'collection');

  list := TList<T>.Create;
  for item in Self do
    if not collection.Contains(item) then
      list.Add(item);

  for item in list do
    Remove(item);
end;

procedure THashSet<T>.NonGenericExceptWith(const collection: IEnumerable);
begin
  ExceptWith(collection as THashSet<T>);
end;

procedure THashSet<T>.NonGenericIntersectWith(const collection: IEnumerable);
begin
  IntersectWith(collection as THashSet<T>);
end;

function THashSet<T>.NonGenericOverlaps(const collection: IEnumerable): Boolean;
begin
  Result := Overlaps(collection as THashSet<T>);
end;

function THashSet<T>.NonGenericSetEquals(
  const collection: IEnumerable): Boolean;
begin
  Result := SetEquals(collection as THashSet<T>);
end;

procedure THashSet<T>.NonGenericUnionWith(const collection: IEnumerable);
begin
  UnionWith(collection as THashSet<T>);
end;

procedure THashSet<T>.UnionWith(const collection: IEnumerable<T>);
var
  item: T;
begin
  Guard.CheckNotNull(collection <> nil, 'collection');

  for item in collection do
    Add(item);
end;

function THashSet<T>.Overlaps(const collection: IEnumerable<T>): Boolean;
var
  item: T;
begin
  Guard.CheckNotNull(collection <> nil, 'collection');

  for item in collection do
    if Contains(item) then
      Exit(True);

  Result := False;
end;

function THashSet<T>.SetEquals(const collection: IEnumerable<T>): Boolean;
var
  item: T;
  localSet: ISet<T>;
begin
  Guard.CheckNotNull(collection <> nil, 'collection');

  localSet := THashSet<T>.Create;

  for item in collection do
  begin
    localSet.Add(item);
    if not Contains(item) then
      Exit(False);
  end;

  for item in Self do
    if not localSet.Contains(item) then
      Exit(False);

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
