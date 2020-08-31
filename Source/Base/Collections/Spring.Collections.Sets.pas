{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2020 Spring4D Team                           }
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

unit Spring.Collections.Sets;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.HashTable,
  Spring.Collections.Trees;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  /// <summary>
  ///   The abstract base class for all set implementations.
  /// </summary>
  TSetBase<T> = class abstract(TCollectionBase<T>)
  protected
    function CreateSet: ISet<T>; virtual; abstract;
  public
    procedure ExceptWith(const other: IEnumerable<T>);
    procedure IntersectWith(const other: IEnumerable<T>);
    procedure UnionWith(const other: IEnumerable<T>);
    function IsSubsetOf(const other: IEnumerable<T>): Boolean;
    function IsSupersetOf(const other: IEnumerable<T>): Boolean;
    function SetEquals(const other: IEnumerable<T>): Boolean;
    function Overlaps(const other: IEnumerable<T>): Boolean;
  end;

  THashSetItem<T> = packed record
  public
    HashCode: Integer;
    Item: T;
  end;

  /// <summary>
  ///   Represents a set of values.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements in the hash set.
  /// </typeparam>
  THashSet<T> = class(TSetBase<T>, IInterface, IEnumerable<T>,
    IReadOnlyCollection<T>, ICollection<T>, ISet<T>)
  private
  {$REGION 'Nested Types'}
    type
      TItem = THashSetItem<T>;
      TItems = TArray<TItem>;
      PItem = ^TItem;

      TEnumerator = class(THashTableEnumerator, IEnumerator<T>)
      private
        fCurrent: T;
        function GetCurrent: T;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fKeyComparer: IEqualityComparer<T>;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
    class function EqualsThunk(instance: Pointer; const left, right): Boolean; static;
  protected
    function CreateSet: ISet<T>; override;
    function TryGetElementAt(var item: T; index: Integer): Boolean;
    property Capacity: Integer read GetCapacity;
  public
    constructor Create(capacity: Integer; const comparer: IEqualityComparer<T>);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const item: T): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean;
    function Remove(const item: T): Boolean;
    function Extract(const item: T): T;

    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements ISet<T>'}
    procedure TrimExcess;
  {$ENDREGION}
  end;

  TSortedSet<T> = class(TSetBase<T>, IEnumerable<T>,
    IReadOnlyCollection<T>, ICollection<T>, ISet<T>)
  private
  {$REGION 'Nested Types'}
    type
      PNode = TNodes<T>.PRedBlackTreeNode;
      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TSortedSet<T>;
        fVersion: Integer;
        fCurrent: PNode;
        fFinished: Boolean;
        function GetCurrent: T;
      public
        constructor Create(const source: TSortedSet<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    fTree: TRedBlackTree<T>;
    fVersion: Integer;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
  protected
    function CreateSet: ISet<T>; override;
  public
    constructor Create(const comparer: IComparer<T>);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const item: T): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean;
    function Remove(const item: T): Boolean;
    function Extract(const item: T): T;
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements ISet<T>'}
    procedure TrimExcess;
  {$ENDREGION}
  end;

  TFoldedHashSet<T> = class(THashSet<T>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(elementType: PTypeInfo;
      capacity: Integer;
      const comparer: IEqualityComparer<T>);
  end;

implementation

uses
  TypInfo,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TSetBase<T>'}

procedure TSetBase<T>.ExceptWith(const other: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    ICollection<T>(this).Remove(item);
  end;
end;

procedure TSetBase<T>.IntersectWith(const other: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  items: TArray<T>;
  item: T;
  i: Integer;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  if not other.IsEmpty then
  begin
    SetLength(items, IEnumerable<T>(this).Count);
    i := 0;
    enumerator := other.GetEnumerator;
    while enumerator.MoveNext do
    begin
      item := enumerator.Current;
      if ICollection<T>(this).Remove(item) then
      begin
        items[i] := item;
        Inc(i);
      end;
    end;
    SetLength(items, i);
  end;
  ICollection<T>(this).Clear;
  ICollection<T>(this).AddRange(items);
end;

function TSetBase<T>.IsSubsetOf(const other: IEnumerable<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    if not other.Contains(item) then
      Exit(False);
  end;

  Result := True;
end;

function TSetBase<T>.IsSupersetOf(const other: IEnumerable<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    if not IEnumerable<T>(this).Contains(item) then
      Exit(False);
  end;

  Result := True;
end;

function TSetBase<T>.Overlaps(const other: IEnumerable<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    if IEnumerable<T>(this).Contains(item) then
      Exit(True);
  end;

  Result := False;
end;

function TSetBase<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  localSet: ISet<T>;
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  localSet := CreateSet;

  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    localSet.Add(item);
    if not IEnumerable<T>(this).Contains(item) then
      Exit(False);
  end;

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    if not localSet.Contains(item) then
      Exit(False);
  end;

  Result := True;
end;

procedure TSetBase<T>.UnionWith(const other: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    ICollection<T>(this).Add(item);
  end;
end;

{$ENDREGION}


{$REGION 'THashSet<T>'}

constructor THashSet<T>.Create(capacity: Integer; const comparer: IEqualityComparer<T>);
begin
  fKeyComparer := comparer;
  SetCapacity(capacity);
end;

procedure THashSet<T>.AfterConstruction;
begin
  inherited AfterConstruction;

  if not Assigned(fKeyComparer) then
    fKeyComparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T)));
  fHashTable.Initialize(TypeInfo(TItems), @EqualsThunk, fKeyComparer);
end;

procedure THashSet<T>.BeforeDestruction;
begin
  Clear;
  inherited BeforeDestruction;
end;

function THashSet<T>.CreateSet: ISet<T>;
begin
  Result := THashSet<T>.Create(0, fKeyComparer);
end;

procedure THashSet<T>.SetCapacity(value: Integer);
begin
  fHashTable.Capacity := value;
end;

procedure THashSet<T>.TrimExcess;
begin
  fHashTable.Capacity := fHashTable.Count;
end;

function THashSet<T>.TryGetElementAt(var item: T; index: Integer): Boolean;
begin
  Result := Cardinal(index) < Cardinal(fHashTable.Count);
  if Result then
  begin
    fHashTable.EnsureCompact;
    item := TItems(fHashTable.Items)[index].Item;
  end;
end;

function THashSet<T>.Add(const item: T): Boolean;
var
  entry: PItem;
begin
  entry := fHashTable.Add(item, fKeyComparer.GetHashCode(item));
  Result := Assigned(entry);
  if Result then
  begin
    entry.Item := item;
    DoNotify(item, caAdded);
  end;
end;

procedure THashSet<T>.Clear;
var
  item: PItem;
  i: Integer;
begin
  if fHashTable.ItemCount = 0 then
    Exit;

  if Assigned(Notify) then
  begin
    fHashTable.ClearCount;
    item := PItem(fHashTable.Items);
    for i := 1 to fHashTable.ItemCount do
      if item.HashCode >= 0 then
        Notify(Self, item.Item, caRemoved);
  end;

  fHashTable.Clear;
end;

class function THashSet<T>.EqualsThunk(instance: Pointer; const left, right): Boolean;
begin
  Result := TEqualsMethod<T>(instance^)(T(left), T(right));
end;

function THashSet<T>.Contains(const item: T): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(item);
  Result := fHashTable.Find(item, entry);
end;

function THashSet<T>.Extract(const item: T): T;
var
  entry: PItem;
begin
  entry := fHashTable.Delete(item, fKeyComparer.GetHashCode(item));
  if Assigned(entry) then
  begin
    DoNotify(entry.Item, caExtracted);
    Result := entry.Item;
    entry.Item := Default(T);
  end
  else
    Result := Default(T);
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self, @fHashTable);
end;

function THashSet<T>.GetCapacity: Integer;
begin
  Result := fHashTable.Capacity;
end;

function THashSet<T>.GetCount: Integer;
begin
  Result := fHashTable.Count;
end;

function THashSet<T>.GetIsEmpty: Boolean;
begin
  Result := fHashTable.Count = 0;
end;

function THashSet<T>.Remove(const item: T): Boolean;
var
  entry: PItem;
begin
  entry := fHashTable.Delete(item, fKeyComparer.GetHashCode(item));
  if Assigned(entry) then
  begin
    DoNotify(entry.Item, caRemoved);
    entry.Item := Default(T);
    Result := True;
  end
  else
    Result := False;
end;

function THashSet<T>.ToArray: TArray<T>;
var
  sourceIndex, targetIndex: Integer;
begin
  SetLength(Result, fHashTable.Count);
  targetIndex := 0;
  for sourceIndex := 0 to fHashTable.ItemCount - 1 do
    if TItems(fHashTable.Items)[sourceIndex].HashCode >= 0 then
    begin
      Result[targetIndex] := TItems(fHashTable.Items)[sourceIndex].Item;
      Inc(targetIndex);
    end;
end;

{$ENDREGION}


{$REGION 'THashSet<T>.TEnumerator'}

function THashSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function THashSet<T>.TEnumerator.MoveNext: Boolean;
begin
  if inherited MoveNext then
  begin
    fCurrent := PItem(fItem).Item;
    Exit(True);
  end;
  Result := False;
end;

{$ENDREGION}


{$REGION 'TSortedSet<T>'}

constructor TSortedSet<T>.Create(const comparer: IComparer<T>);
begin
  fComparer := comparer;
end;

procedure TSortedSet<T>.AfterConstruction;
begin
  inherited AfterConstruction;

  fTree := TRedBlackTree<T>.Create(fComparer);
end;

procedure TSortedSet<T>.BeforeDestruction;
begin
  Clear;
  fTree.Free;
  inherited BeforeDestruction;
end;

function TSortedSet<T>.CreateSet: ISet<T>;
begin
  Result := TSortedSet<T>.Create(Comparer);
end;

function TSortedSet<T>.Add(const item: T): Boolean;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Result := fTree.Add(item);
  if Result then
    DoNotify(item, caAdded);
end;

procedure TSortedSet<T>.Clear;
var
  node: PNode;
begin
  if fTree.Count = 0 then
    Exit;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if Assigned(Notify) then // optimization: if no notification needs to be send the entire tree traversal won't be done
    for node in fTree.Root^ do
      Notify(Self, PNode(node).Key, caRemoved);

  fTree.Clear;
end;

function TSortedSet<T>.Contains(const item: T): Boolean;
begin
  Result := Assigned(fTree.FindNode(item));
end;

function TSortedSet<T>.Extract(const item: T): T;
var
  node: PNode;
begin
  node := fTree.FindNode(item);
  if Assigned(node) then
  begin
    Result := node.Key;
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    fTree.DeleteNode(node);
    DoNotify(Result, caExtracted);
  end
  else
    Result := Default(T);
end;

function TSortedSet<T>.GetCapacity: Integer;
begin
  Result := fTree.Capacity;
end;

function TSortedSet<T>.GetCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TSortedSet<T>.Remove(const item: T): Boolean;
var
  node: PNode;
begin
  node := fTree.FindNode(item);
  Result := Assigned(node);
  if Result then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    fTree.DeleteNode(node);
    DoNotify(item, caRemoved);
  end;
end;

function TSortedSet<T>.ToArray: TArray<T>;
var
  i: Integer;
  node: PNode;
begin
  SetLength(Result, fTree.Count);
  i := 0;
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    Result[i] := node.Key;
    node := node.Next;
    Inc(i);
  end;
end;

{$ENDREGION}


{$REGION 'TSortedSet<T>.TEnumerator'}

constructor TSortedSet<T>.TEnumerator.Create(const source: TSortedSet<T>);
begin
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TSortedSet<T>.TEnumerator.Destroy;
begin
  fSource._Release;
end;

function TSortedSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent.Key;
end;

function TSortedSet<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion = fSource.fVersion then
  begin
    if fFinished then
      Exit(False);
    if not Assigned(fCurrent) then
      fCurrent := fSource.fTree.Root.LeftMost
    else
      fCurrent := fCurrent.Next;
    Result := Assigned(fCurrent);
    if not Result then
      fFinished := True;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

procedure TSortedSet<T>.SetCapacity(value: Integer);
begin
  fTree.Capacity := value;
end;

procedure TSortedSet<T>.TrimExcess;
begin
  fTree.TrimExcess;
end;

{$ENDREGION}


{$REGION 'TFoldedHashSet<T>'}

constructor TFoldedHashSet<T>.Create(elementType: PTypeInfo; capacity: Integer;
  const comparer: IEqualityComparer<T>);
begin
  inherited Create(capacity, comparer);
  fElementType := elementType;
end;

function TFoldedHashSet<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


end.
