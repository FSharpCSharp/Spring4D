{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2019 Spring4D Team                           }
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

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

type
  /// <summary>
  ///   The abstract base class for all set implementations.
  /// </summary>
  TSetBase<T> = class abstract(TCollectionBase<T>)
  protected
    class function CreateSet: ISet<T>; virtual; abstract;
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

      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: THashSet<T>;
        fItemIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
        function GetCurrent: T;
      public
        constructor Create(const source: THashSet<T>);
        destructor Destroy; override;
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
    class function CompareKey(Self: Pointer; const left, right): Boolean; static;
    procedure ClearInternal;
  protected
    class function CreateSet: ISet<T>; override;
    function TryGetElementAt(out item: T; index: Integer): Boolean; override;
    property Capacity: Integer read GetCapacity;
  public
    constructor Create; overload; override;
    constructor Create(capacity: Integer); overload;
    constructor Create(const comparer: IEqualityComparer<T>); overload;
    constructor Create(capacity: Integer;
      const comparer: IEqualityComparer<T>); overload;
    destructor Destroy; override;

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
    class function CreateSet: ISet<T>; override;
  public
    constructor Create; overload; override;
    constructor Create(const comparer: IComparer<T>); overload;
    destructor Destroy; override;

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

implementation

uses
  Math,
  TypInfo,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TSetBase<T>'}

procedure TSetBase<T>.ExceptWith(const other: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    ICollection<T>(this).Remove(item);
end;

procedure TSetBase<T>.IntersectWith(const other: IEnumerable<T>);
var
  item: T;
  items: TArray<T>;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  if not other.IsEmpty then
  begin
    SetLength(items, IEnumerable<T>(this).Count);
    i := 0;
    for item in other do
      if IEnumerable<T>(this).Contains(item) then
      begin
        items[i] := item;
        Inc(i);
        ICollection<T>(this).Remove(item);
      end;
    SetLength(items, i);
  end;
  ICollection<T>(this).Clear;
  ICollection<T>(this).AddRange(items);
end;

function TSetBase<T>.IsSubsetOf(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in IEnumerable<T>(this) do
    if not other.Contains(item) then
      Exit(False);

  Result := True;
end;

function TSetBase<T>.IsSupersetOf(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    if not IEnumerable<T>(this).Contains(item) then
      Exit(False);

  Result := True;
end;

function TSetBase<T>.Overlaps(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    if IEnumerable<T>(this).Contains(item) then
      Exit(True);

  Result := False;
end;

function TSetBase<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  item: T;
  localSet: ISet<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  localSet := CreateSet;

  for item in other do
  begin
    localSet.Add(item);
    if not IEnumerable<T>(this).Contains(item) then
      Exit(False);
  end;

  for item in IEnumerable<T>(this) do
    if not localSet.Contains(item) then
      Exit(False);

  Result := True;
end;

procedure TSetBase<T>.UnionWith(const other: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    ICollection<T>(this).Add(item);
end;

{$ENDREGION}


{$REGION 'THashSet<T>'}

constructor THashSet<T>.Create;
begin
  Create(0, nil);
end;

constructor THashSet<T>.Create(capacity: Integer);
begin
  Create(capacity, nil);
end;

constructor THashSet<T>.Create(const comparer: IEqualityComparer<T>);
begin
  Create(0, comparer);
end;

constructor THashSet<T>.Create(capacity: Integer; const comparer: IEqualityComparer<T>);
begin
  inherited Create;

  if Assigned(comparer) then
    fKeyComparer := comparer
  else
    fKeyComparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T)));

  fHashTable := THashTable.Create(TypeInfo(TItems), CompareKey);

  SetCapacity(capacity);
end;

destructor THashSet<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class function THashSet<T>.CreateSet: ISet<T>;
begin
  Result := THashSet<T>.Create;
end;

procedure THashSet<T>.SetCapacity(value: Integer);
begin
  fHashTable.Capacity := value;
end;

procedure THashSet<T>.TrimExcess;
begin
  fHashTable.Capacity := fHashTable.Count;
end;

function THashSet<T>.TryGetElementAt(out item: T; index: Integer): Boolean;
begin
  Result := InRange(index, 0, fHashTable.Count - 1);
  if Result then
  begin
    fHashTable.EnsureCompact;
    item := TItems(fHashTable.Items)[index].Item;
  end;
end;

function THashSet<T>.Add(const item: T): Boolean;
var
  entry: ^TItem;
begin
  entry := fHashTable.Add(item, fKeyComparer.GetHashCode(item));
  Result := Assigned(entry);
  if Result then
  begin
    entry.Item := item;
    Changed(item, caAdded);
  end;
end;

procedure THashSet<T>.Clear;
begin
  if not Assigned(Notify) then
    fHashTable.Clear
  else
    ClearInternal;
end;

procedure THashSet<T>.ClearInternal;
var
  oldItemIndex, oldItemCount: Integer;
  oldItems: TArray<TItem>;
begin
  oldItemCount := fHashTable.ItemCount;
  oldItems := TItems(fHashTable.Items);

  fHashTable.Clear;

  for oldItemIndex := 0 to oldItemCount - 1 do
    if oldItems[oldItemIndex].HashCode >= 0 then
      Changed(oldItems[oldItemIndex].Item, caRemoved);
end;

class function THashSet<T>.CompareKey(Self: Pointer; const left, right): Boolean;
begin
  Result := IEqualityComparer<T>(PPointer(PByte(Self) + SizeOf(THashTable))^).Equals(T(left), T(right));
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
  entry: ^TItem;
begin
  entry := fHashTable.Delete(item, fKeyComparer.GetHashCode(item));
  if Assigned(entry) then
  begin
    Changed(entry.Item, caExtracted);
    Result := entry.Item;
    entry.Item := Default(T);
  end
  else
    Result := Default(T);
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
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
  entry: ^TItem;
begin
  entry := fHashTable.Delete(item, fKeyComparer.GetHashCode(item));
  Result := Assigned(entry);
  if Result then
  begin
    Changed(entry.Item, caRemoved);
    entry.Item := Default(T);
  end;
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

constructor THashSet<T>.TEnumerator.Create(const source: THashSet<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fItemIndex := -1;
  fVersion := fSource.fHashTable.Version;
end;

destructor THashSet<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function THashSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function THashSet<T>.TEnumerator.MoveNext: Boolean;
var
  hashTable: ^THashTable;
begin
  hashTable := @fSource.fHashTable;
  if fVersion <> hashTable.Version then
    raise Error.EnumFailedVersion;

  while fItemIndex < hashTable.ItemCount - 1 do
  begin
    Inc(fItemIndex);
    if TItems(hashTable.Items)[fItemIndex].HashCode >= 0 then
    begin
      fCurrent := TItems(hashTable.Items)[fItemIndex].Item;
      Exit(True);
    end;
  end;
  fCurrent := Default(T);
  Result := False;
end;

{$ENDREGION}


{$REGION 'TSortedSet<T>'}

constructor TSortedSet<T>.Create;
var
  // use variable to pass nil because of codegen bug in XE2 and XE3 in x64
  comparer: IComparer<T>;
begin
  Create(comparer);
end;

constructor TSortedSet<T>.Create(const comparer: IComparer<T>);
begin
  inherited Create;
  fTree := TRedBlackTree<T>.Create(comparer);
end;

class function TSortedSet<T>.CreateSet: ISet<T>;
begin
  Result := TSortedSet<T>.Create;
end;

destructor TSortedSet<T>.Destroy;
begin
  Clear;
  fTree.Free;
  inherited Destroy;
end;

function TSortedSet<T>.Add(const item: T): Boolean;
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}
  Result := fTree.Add(item);
  if Result then
    Changed(item, caAdded);
end;

procedure TSortedSet<T>.Clear;
var
  node: PNode;
begin
  if fTree.Count = 0 then
    Exit;

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}
  if Assigned(OnChanged) and OnChanged.CanInvoke then // optimization: if no notification needs to be send the entire tree traversal won't be done
    for node in fTree.Root^ do
      Changed(PNode(node).Key, caRemoved);

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
    {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}
    fTree.DeleteNode(node);
    Changed(Result, caExtracted);
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
    {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}
    fTree.DeleteNode(node);
    Changed(item, caRemoved);
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
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TSortedSet<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TSortedSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent.Key;
end;

function TSortedSet<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion <> fSource.fVersion then
    raise Error.EnumFailedVersion;

  if fFinished then
    Exit(False);
  if not Assigned(fCurrent) then
    fCurrent := fSource.fTree.Root.LeftMost
  else
    fCurrent := fCurrent.Next;
  Result := Assigned(fCurrent);
  if not Result then
    fFinished := True;
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


end.
