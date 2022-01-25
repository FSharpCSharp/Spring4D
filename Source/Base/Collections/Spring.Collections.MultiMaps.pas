{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2022 Spring4D Team                           }
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

unit Spring.Collections.MultiMaps;

interface

uses
  Classes,
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Trees,
  Spring.Events,
  Spring.Events.Base,
  Spring.HashTable;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  TMultiMapItem<TKey, TValue> = packed record
  public
    HashCode: Integer;
    Key: TKey;
    Values: ICollection<TValue>;
  end;

  TValueCollection<T> = class(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fHashTable: PHashTable;
      fIndex: Integer;
      fVersion: Integer;
      fEnumerator: IEnumerator<T>;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fSource: TRefCountedObject;
    fHashTable: PHashTable;
    fCount: PInteger;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
  {$ENDREGION}
  public
    constructor Create(const source: TRefCountedObject;
      hashTable: PHashTable; count: PInteger);

  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  {$ENDREGION}

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const value: T;
      const comparer: IEqualityComparer<T>): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}
  end;

  TTreeValueCollection<T> = class(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private type
  {$REGION 'Nested Types'}
    PNode = ^TNode;
    TNode = record
      Parent, Left, Right: Pointer;
      Key: array[0..0] of Byte;
    end;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fTree: TBinaryTree;
      fSourceVersion: PInteger;
      fVersion: Integer;
      fOffset: Integer;
      fNode: PBinaryTreeNode;
      fEnumerator: IEnumerator<T>;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fSource: TRefCountedObject;
    fTree: TBinaryTree;
    fCount: PInteger;
    fVersion: PInteger;
    fOffset: Integer;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
  {$ENDREGION}
  public
    constructor Create(const source: TRefCountedObject; const tree: TBinaryTree;
      const version, count: PInteger; const offset: Integer);

  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  {$ENDREGION}

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const value: T;
      const comparer: IEqualityComparer<T>): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}
  end;

  TCollectionWrapper<T> = class(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TCollectionWrapper<T>;
      fCollection: ICollection<T>;
      fEnumerator: IEnumerator<T>;
      procedure ValidateEnumerator;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fCollection: ICollection<T>;
    fOnDestroy: TNotifyEventImpl;
    fUpdateValues: TNotifyEvent;
    function GetCount: Integer;
    function GetCountFast: Integer;
    procedure RefreshIfEmpty;
    procedure HandleDestroy(Sender: TObject);
  public
    procedure BeforeDestruction; override;

    function Contains(const value: T;
      const comparer: IEqualityComparer<T>): Boolean; overload;
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
  end;

  TMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>)
  private type
  {$REGION 'Nested Types'}
    TKeyValuePair = TPair<TKey, TValue>;
    TItem = TMultiMapItem<TKey, TValue>;
    TItems = TArray<TItem>;
    PItem = ^TItem;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TMultiMapBase<TKey, TValue>;
      fIndex: Integer;
      fVersion: Integer;
      fEnumerator: IEnumerator<TValue>;
      fItem: PItem;
      function GetCurrent: TKeyValuePair;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;

    TKeyCollection = TInnerCollection<TKey>;
    TValueCollection = TValueCollection<TValue>;

    TCollectionWrapper = class(TCollectionWrapper<TValue>)
    private
      fKey: TKey;
    end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fCount: Integer;
    fOnDestroy: TNotifyEventImpl;
    function CreateWrappedCollection(const key: TKey): IReadOnlyCollection<TValue>;
    procedure DoValueChanged(sender: TObject; const item: TValue;
      action: TCollectionChangedAction);
    procedure UpdateValues(collection: TObject);
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetValues: IReadOnlyCollection<TValue>;
  {$ENDREGION}
    procedure CreateCollection(var result: ICollection<TValue>); virtual; abstract;
    procedure DoRemove(const entry: THashTableEntry;
      action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); inline;
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction); inline;
  public
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      ownerships: TDictionaryOwnerships);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    function TryAdd(const key: TKey; const value: TValue): Boolean;
    function Remove(const key: TKey): Boolean; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; overload;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; overload;
    function Contains(const key: TKey; const value: TValue): Boolean; overload;
    function ContainsKey(const key: TKey): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IMultiMap<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean; overload;
    procedure AddRange(const key: TKey; const values: array of TValue); overload;
    procedure AddRange(const key: TKey; const values: IEnumerable<TValue>); overload;
    function Extract(const key: TKey): ICollection<TValue>; overload;
    function TryGetValues(const key: TKey; var values: IReadOnlyCollection<TValue>): Boolean;
  {$ENDREGION}
  end;

  TSortedMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>)
  private type
  {$REGION 'Nested Types'}
    TKeyValuePair = TPair<TKey, TValue>;
    PNode = ^TNode;
    TNode = packed record // same layout as TRedBlackTreeBase<TKey, IInterface>.TNode
      Parent, Right, Left: Pointer;
      Key: TKey;
      Values: ICollection<TValue>;
    end;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TSortedMultiMapBase<TKey, TValue>;
      fNode: PBinaryTreeNode;
      fVersion: Integer;
      fEnumerator: IEnumerator<TValue>;
      function GetCurrent: TKeyValuePair;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;

    TKeyCollection = TSortedKeyCollection<TKey>;
    TValueCollection = TTreeValueCollection<TValue>;

    TCollectionWrapper = class(TCollectionWrapper<TValue>)
    private
      fKey: TKey;
    end;
  {$ENDREGION}
  private
    fTree: TRedBlackTreeBase<TKey, IInterface>;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fVersion: Integer;
    fCount: Integer;
    fOnDestroy: TNotifyEventImpl;
    fKeyComparer: IComparer<TKey>;
    fOwnerships: TDictionaryOwnerships;
    function CreateWrappedCollection(const key: TKey): IReadOnlyCollection<TValue>;
    procedure DoValueChanged(sender: TObject; const item: TValue;
      action: TCollectionChangedAction);
    procedure UpdateValues(collection: TObject);
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetValues: IReadOnlyCollection<TValue>;
  {$ENDREGION}
    procedure CreateCollection(var result: ICollection<TValue>); virtual; abstract;
    procedure DoRemove(const node: PNode;
      action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); inline;
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction); inline;
  public
    constructor Create(const keyComparer: IComparer<TKey>;
      ownerships: TDictionaryOwnerships);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    function TryAdd(const key: TKey; const value: TValue): Boolean;
    function Remove(const key: TKey): Boolean; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; overload;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; overload;
    function Contains(const key: TKey; const value: TValue): Boolean; overload;
    function ContainsKey(const key: TKey): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IMultiMap<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean; overload;
    procedure AddRange(const key: TKey; const values: array of TValue); overload;
    procedure AddRange(const key: TKey; const values: IEnumerable<TValue>); overload;
    function Extract(const key: TKey): ICollection<TValue>; overload;
    function TryGetValues(const key: TKey; var values: IReadOnlyCollection<TValue>): Boolean;
  {$ENDREGION}
  end;

  TListMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  end;

  THashMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    fValueComparer: IEqualityComparer<TValue>;
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  public
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

  TTreeMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    fValueComparer: IComparer<TValue>;
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  public
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

  TSortedListMultiMap<TKey, TValue> = class(TSortedMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  end;

  TSortedHashMultiMap<TKey, TValue> = class(TSortedMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    fValueComparer: IEqualityComparer<TValue>;
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  public
    constructor Create(const keyComparer: IComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

  TSortedTreeMultiMap<TKey, TValue> = class(TSortedMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    fValueComparer: IComparer<TValue>;
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  public
    constructor Create(const keyComparer: IComparer<TKey>;
      const valueComparer: IComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

  TFoldedListMultiMap<TKey, TValue> = class(TListMultiMap<TKey, TValue>)
  private
    fElementType: PTypeInfo;
    fKeyType: PTypeInfo;
    fValueType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
    function GetKeyType: PTypeInfo; override;
    function GetValueType: PTypeInfo; override;
  public
    constructor Create(keyType, valueType, elementType: PTypeInfo;
      const keyComparer: IEqualityComparer<TKey>;
      ownerships: TDictionaryOwnerships);
  end;

  TCollections = class(Spring.Collections.TCollections);

procedure HandleOnChanged(const collection: IInterface;
  const code: Pointer = nil; const data: Pointer = nil);

implementation

uses
  Types,
  TypInfo,
  Spring.Collections.Lists,
  Spring.Collections.Sets,
  Spring.Comparers,
  Spring.ResourceStrings;

procedure HandleOnChanged(const collection: IInterface; const code, data: Pointer);
var
  onChanged: IEvent;
  handler: TMethod;
begin
  onChanged := ICollection<Integer>(collection).OnChanged;
  if Assigned(code) then
  begin
    onChanged.UseFreeNotification := False;
    handler.Code := code;
    handler.Data := data;
    onChanged.Add(handler);
  end
  else
    onChanged.Clear;
end;


{$REGION 'TValueCollection<T>'}

constructor TValueCollection<T>.Create(
  const source: TRefCountedObject; hashTable: PHashTable; count: PInteger);
begin
  fSource := source;
  fHashTable := hashTable;
  fCount := count;
end;

function TValueCollection<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  hashTable: PHashTable;
  item: PByte;
  itemCount, itemSize: Integer;
begin
  hashTable := fHashTable;
  item := hashTable.Items;
  itemSize := hashTable.ItemSize;
  itemCount := hashTable.ItemCount;
  while itemCount > 0 do
  begin
    if PInteger(item)^ >= 0 then
      if ICollection<T>(PPointer(@item[itemSize - SizeOf(Pointer)])^).Contains(value, comparer) then
        Exit(True);
    Inc(item, itemSize);
    Dec(itemCount);
  end;
  Result := False;
end;

function TValueCollection<T>.GetCount: Integer;
begin
  Result := fCount^;
end;

function TValueCollection<T>.GetCountFast: Integer;
begin
  Result := fCount^;
end;

function TValueCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    Parent := Self.fSource;
    fHashTable := Self.fHashTable;
    fVersion := fHashTable.Version;
  end;
end;

function TValueCollection<T>.ToArray: TArray<T>;
var
  hashTable: PHashTable;
  item: PByte;
  itemCount, itemSize, targetIndex, offset: Integer;
  collection: Pointer;
begin
  hashTable := fHashTable;
  SetLength(Result, fCount^);
  item := hashTable.Items;
  itemCount := hashTable.ItemCount;
  itemSize := hashTable.ItemSize;
  targetIndex := 0;
  while itemCount > 0 do
  begin
    if PInteger(item)^ >= 0 then
    begin
      collection := PPointer(item + itemSize - SizeOf(Pointer))^;
      ICollection<T>(collection).CopyTo(Result, targetIndex);
      Inc(targetIndex, ICollection<T>(collection).Count);
    end;
    Inc(item, itemSize);
    Dec(itemCount);
  end;
end;

function TValueCollection<T>._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TValueCollection<T>._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TValueCollection<T>.TEnumerator'}

function TValueCollection<T>.TEnumerator.GetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TValueCollection<T>.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PByte;
begin
  hashTable := fHashTable;
  if fVersion = hashTable.Version then
  begin
    repeat
      if Assigned(fEnumerator) then
      begin
        Result := fEnumerator.MoveNext;
        if Result then Exit;
      end;

      repeat
        if fIndex < hashTable.ItemCount then
        begin
          item := fHashTable.Items + NativeInt(fIndex) * fHashTable.ItemSize;
          Inc(fIndex);
          if PInteger(item)^ >= 0 then
          begin
            Inc(item, fHashTable.ItemSize - SizeOf(Pointer));
            {$IFDEF MSWINDOWS}
            IEnumerableInternal(PPointer(item)^).GetEnumerator(IEnumerator(fEnumerator));
            {$ELSE}
            fEnumerator := ICollection<T>(PPointer(item)^).GetEnumerator;
            {$ENDIF}
            Break;
          end;
        end
        else
        begin
          fEnumerator := nil;
          Exit(False);
        end;
      until False;
    until False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TTreeValueCollection<T>'}

constructor TTreeValueCollection<T>.Create(const source: TRefCountedObject;
  const tree: TBinaryTree; const version, count: PInteger; const offset: Integer);
begin
  fSource := source;
  fTree := tree;
  fVersion := version;
  fCount := count;
  fOffset := offset;
end;

function TTreeValueCollection<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  node: PBinaryTreeNode;
  next: Pointer;
  offset: Integer;
begin
  offset := fOffset;
  next := fTree.Root.LeftMost;
  if not Assigned(next) then Exit(Boolean(next));
  repeat
    node := next;
    {$R-}
    Result := ICollection<T>(PPointer(@PNode(node).Key[offset])^).Contains(value, comparer);
    {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
    if Result then Exit;
    next := node.Next;
  until not Assigned(next);
  Result := Boolean(next);
end;

function TTreeValueCollection<T>.GetCount: Integer;
begin
  Result := fCount^;
end;

function TTreeValueCollection<T>.GetCountFast: Integer;
begin
  Result := fCount^;
end;

function TTreeValueCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    Parent := Self.fSource;
    fTree := Self.fTree;
    fSourceVersion := Self.fVersion;
    fVersion := Self.fVersion^;
    fOffset := Self.fOffset;
  end;
end;

function TTreeValueCollection<T>.ToArray: TArray<T>;
var
  node, next: PBinaryTreeNode;
  offset, i: Integer;
  collection: Pointer;
begin
  SetLength(Result, fCount^);
  offset := fOffset;
  next := fTree.Root.LeftMost;
  if Assigned(next) then
  begin
    i := 0;
    repeat
      node := next;
      {$R-}
      Inc(i, ICollection<T>(PPointer(@PNode(node).Key[offset])^).CopyTo(Result, i));
      {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
      next := node.Next;
    until not Assigned(next);
  end;
end;

function TTreeValueCollection<T>._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TTreeValueCollection<T>._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TTreeValueCollection<T>.TEnumerator'}

function TTreeValueCollection<T>.TEnumerator.GetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TTreeValueCollection<T>.TEnumerator.MoveNext: Boolean;
var
  tree: TBinaryTree;
  node: PBinaryTreeNode;
begin
  tree := fTree;
  if fVersion = fSourceVersion^ then
  begin
    repeat
      if Assigned(fEnumerator) then
      begin
        Result := fEnumerator.MoveNext;
        if Result then Exit;
      end;

      if (tree.Count > 0) and (fNode <> Pointer(1)) then
      begin
        if Assigned(fNode) then
          node := fNode.Next
        else
          node := tree.Root.LeftMost;
        if Assigned(node) then
        begin
          fNode := node;
          {$R-}
          {$IFDEF MSWINDOWS}
          IEnumerableInternal(PPointer(@PNode(node).Key[fOffset])^).GetEnumerator(IEnumerator(fEnumerator));
          {$ELSE}
          fEnumerator := IEnumerable<T>(PPointer(@PNode(node).Key[fOffset])^).GetEnumerator;
          {$ENDIF}
          {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
          Continue;
        end;
      end;

      fNode := Pointer(1);
      fEnumerator := nil;
      Exit(False);
    until False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper<T>'}

procedure TCollectionWrapper<T>.BeforeDestruction;
begin
  if Assigned(fOnDestroy) then
    fOnDestroy.Remove(HandleDestroy);
  inherited;
end;

function TCollectionWrapper<T>.Contains(
  const value: T; const comparer: IEqualityComparer<T>): Boolean;
begin
  RefreshIfEmpty;
  Result := fCollection.Contains(value, comparer);
end;

procedure TCollectionWrapper<T>.RefreshIfEmpty;
begin
  if fCollection.IsEmpty and Assigned(fUpdateValues) then
    fUpdateValues(Self);
end;

function TCollectionWrapper<T>.GetCount: Integer;
begin
  RefreshIfEmpty;
  Result := fCollection.Count;
end;

function TCollectionWrapper<T>.GetCountFast: Integer;
begin
  RefreshIfEmpty;
  Result := fCollection.Count;
end;

function TCollectionWrapper<T>.GetEnumerator: IEnumerator<T>;
begin
  RefreshIfEmpty;
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fCollection := Self.fCollection;
    fEnumerator := fCollection.GetEnumerator;
  end;
end;

procedure TCollectionWrapper<T>.HandleDestroy(Sender: TObject);
begin
  fOnDestroy := nil;
  fUpdateValues := nil;
end;

function TCollectionWrapper<T>.ToArray: TArray<T>;
begin
  RefreshIfEmpty;
  Result := fCollection.ToArray;
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper<T>.TEnumerator'}

function TCollectionWrapper<T>.TEnumerator.GetCurrent: T;
begin
  ValidateEnumerator;
  Result := fEnumerator.Current;
end;

function TCollectionWrapper<T>.TEnumerator.MoveNext: Boolean;
begin
  ValidateEnumerator;
  Result := fEnumerator.MoveNext;
end;

procedure TCollectionWrapper<T>.TEnumerator.ValidateEnumerator;
begin
  fSource.RefreshIfEmpty;
  if fSource.fCollection <> fCollection then
    RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>'}

constructor TMultiMapBase<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;
end;

procedure TMultiMapBase<TKey, TValue>.AfterConstruction;
var
  keyType: PTypeInfo;
begin
  inherited AfterConstruction;

  keyType := GetKeyType;
  fHashTable.ItemsInfo := TypeInfo(TItems);
  fHashTable.Initialize(@TComparerThunks<TKey>.Equals, @TComparerThunks<TKey>.GetHashCode, keyType);
  {$IFDEF DELPHIXE7_UP}
  if fHashTable.DefaultComparer then
    fHashTable.Find := @THashTable<TKey>.FindWithoutComparer
  else
  {$ENDIF}
    fHashTable.Find := @THashTable<TKey>.FindWithComparer;

  fKeys := TKeyCollection.Create(Self, @fHashTable, IEqualityComparer<TKey>(fHashTable.Comparer), keyType, 0);
  fValues := TValueCollection.Create(Self, @fHashTable, @fCount);
  fOnDestroy := TNotifyEventImpl.Create;
  fOnDestroy.UseFreeNotification := False;
end;

procedure TMultiMapBase<TKey, TValue>.BeforeDestruction;
begin
  fOnDestroy.Invoke(Self);
  fOnDestroy.Free;
  Clear;
  fKeys.Free;
  fValues.Free;
  inherited BeforeDestruction;
end;

function TMultiMapBase<TKey, TValue>.CreateWrappedCollection(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  collection: TCollectionWrapper;
begin
  collection := TCollectionWrapper.Create;
  collection.fKey := key;
  collection.fOnDestroy := fOnDestroy;
  fOnDestroy.Add(collection.HandleDestroy);
  collection.fUpdateValues := UpdateValues;
  CreateCollection(collection.fCollection);

  Result := collection;
end;

procedure TMultiMapBase<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsKeys in fHashTable.Ownerships) then
    FreeObject(item);
end;

procedure TMultiMapBase<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsValues in fHashTable.Ownerships) then
    FreeObject(item);
end;

function TMultiMapBase<TKey, TValue>.Add(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := TryAdd(key, value);
end;

procedure TMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: array of TValue);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    Add(key, values[i]);
end;

procedure TMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: IEnumerable<TValue>);
var
  enumerator: IEnumerator<TValue>;
  item: TValue;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    Add(key, item);
  end;
end;

procedure TMultiMapBase<TKey, TValue>.Clear;
var
  oldItemCount, i: Integer;
  oldItems: TArray<TItem>;
  oldValue: TValue;
begin
  oldItemCount := fHashTable.ItemCount;
  oldItems := TItems(fHashTable.Items);
  fHashTable.Clear;

  for i := 0 to oldItemCount - 1 do
    if oldItems[i].HashCode >= 0 then
    begin
      HandleOnChanged(oldItems[i].Values);
      for oldValue in oldItems[i].Values do
      begin
        if Assigned(Notify) then
          DoNotify(oldItems[i].Key, oldValue, caRemoved);
        ValueChanged(oldValue, caRemoved);
      end;
      oldItems[i].Values.Clear;
      oldItems[i].Values := nil;
      KeyChanged(oldItems[i].Key, caRemoved);
    end;
  fCount := 0;
end;

function TMultiMapBase<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if not Assigned(item) then Exit(Boolean(Pointer(item)));
  Result := item.Values.Contains(value);
end;

function TMultiMapBase<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  Result := Assigned(item);
end;

function TMultiMapBase<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  i: Integer;
begin
  for i := 0 to fHashTable.ItemCount - 1 do
    if TItems(fHashTable.Items)[i].HashCode >= 0 then
      if TItems(fHashTable.Items)[i].Values.Contains(value) then
        Exit(True);
  Result := False;
end;

procedure TMultiMapBase<TKey, TValue>.DoRemove(const entry: THashTableEntry;
  action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
var
  item: PItem;
  value: TValue;
begin
  item := THashTable(fHashTable).DeleteEntry(entry);
  HandleOnChanged(item.Values);
  for value in item.Values do
  begin
    Dec(fCount);
    if Assigned(Notify) then
      DoNotify(item.Key, value, action);
    ValueChanged(value, action);
  end;
  if action = caRemoved then
    item.Values.Clear
  else
    item.Values.MoveTo(extractTarget);
  KeyChanged(item.Key, action);
  item.HashCode := RemovedFlag;
  item.Key := Default(TKey);
  item.Values := nil;
end;

procedure TMultiMapBase<TKey, TValue>.DoValueChanged(sender: TObject;
  const item: TValue; action: TCollectionChangedAction);
begin
  case action of //FI:W535
    caAdded: Inc(fCount);
    caRemoved, caExtracted: Dec(fCount);
  end;
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
{$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsValues in fHashTable.Ownerships) then
    FreeObject(item);
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  item: PItem;
  count: Integer;
begin
  Result.Key := key;
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if Assigned(item) then
  begin
    count := item.Values.Count;
    Result.Value := item.Values.Extract(value);
    if item.Values.Count < count then
    begin
      if Assigned(Notify) then
        DoNotify(key, value, caExtracted);
      KeyChanged(item.Key, caExtracted);
    end;
  end
  else
    Result.Value := Default(TValue);
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey): ICollection<TValue>;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  CreateCollection(Result);
  if THashTable(fHashTable).FindEntry(key, entry) then
    DoRemove(entry, caExtracted, Result);
end;

function TMultiMapBase<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TMultiMapBase<TKey, TValue>.GetCountFast: Integer;
begin
  Result := fCount;
end;

function TMultiMapBase<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fHashTable.Version;
  end;
end;

function TMultiMapBase<TKey, TValue>.GetItems(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if Assigned(item) then
    Result := item.Values as IReadOnlyCollection<TValue>
  else
    Result := CreateWrappedCollection(key);
end;

function TMultiMapBase<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TMultiMapBase<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  entry: THashTableEntry;
  item: PItem;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  Result := THashTable(fHashTable).FindEntry(key, entry);
  if not Result then Exit;
  item := @TItems(fHashTable.Items)[entry.ItemIndex];
  Result := item.Values.Remove(value);
  if not Result then Exit;
  if Assigned(Notify) then
    DoNotify(item.Key, value, caRemoved);
  if item.Values.Any then
    {$Q-}
    Inc(PInteger(@fHashTable.Version)^)
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  else
    DoRemove(entry, caRemoved, nil);
  Result := True;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  Result := THashTable(fHashTable).FindEntry(key, entry);
  if Result then
  begin
    DoRemove(entry, caRemoved, nil);
    Result := True;
  end;
end;

function TMultiMapBase<TKey, TValue>.TryAdd(const key: TKey; const value: TValue): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key, InsertNonExisting);
  if not Assigned(item.Values) then
  begin
    item.Key := key;
    CreateCollection(item.Values);
    HandleOnChanged(item.Values, @TMultiMapBase<TKey, TValue>.DoValueChanged, Self);
    KeyChanged(key, caAdded);
  end;

  Result := item.Values.Add(value);
  if Result then
  begin
    {$Q-}
    Inc(PInteger(@fHashTable.Version)^);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    if Assigned(Notify) then
    begin
      DoNotify(key, value, caAdded);
      Result := True;
    end;
  end;
end;

procedure TMultiMapBase<TKey, TValue>.UpdateValues(collection: TObject);
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(TCollectionWrapper(collection).fKey);
  if Assigned(item) then
    TCollectionWrapper(collection).fCollection := item.Values;
end;

function TMultiMapBase<TKey, TValue>.TryGetValues(const key: TKey;
  var values: IReadOnlyCollection<TValue>): Boolean;
var
  temp: Pointer;
  item: PItem;
begin
  temp := IHashTable<TKey>(@fHashTable).Find(key);
  if not Assigned(temp) then Exit(Boolean(temp));
  item := temp;
  values := item.Values as IReadOnlyCollection<TValue>;
  Result := True;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TEnumerator'}

function TMultiMapBase<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result.Key := fItem.Key;
  Result.Value := fEnumerator.Current;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PItem;
begin
  hashTable := @fSource.fHashTable;
  if fVersion = hashTable.Version then
  begin
    repeat
      if Assigned(fEnumerator) then
      begin
        Result := fEnumerator.MoveNext;
        if Result then Exit;
      end;

      repeat
        if fIndex < hashTable.ItemCount then
        begin
          item := @TItems(hashTable.Items)[fIndex];
          Inc(fIndex);
          if item.HashCode >= 0 then
          begin
            fItem := item;
            fEnumerator := item.Values.GetEnumerator;
            Break;
          end;
        end
        else
        begin
          fEnumerator := nil;
          Exit(False);
        end;
      until False;
    until False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TSortedMultiMapBase<TKey, TValue>'}

constructor TSortedMultiMapBase<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>; ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fKeyComparer := keyComparer;
  fOwnerships := ownerships;
end;

procedure TSortedMultiMapBase<TKey, TValue>.AfterConstruction;
begin
  inherited AfterConstruction;

  fTree := TRedBlackTreeBase<TKey,IInterface>.Create(fKeyComparer);
  fKeys := TKeyCollection.Create(Self, fTree, @fVersion);
  fValues := TValueCollection.Create(Self, fTree, @fVersion, @fCount, SizeOf(TKey));
  fOnDestroy := TNotifyEventImpl.Create;
  fOnDestroy.UseFreeNotification := False;
end;

procedure TSortedMultiMapBase<TKey, TValue>.BeforeDestruction;
begin
  fOnDestroy.Invoke(Self);
  fOnDestroy.Free;
  Clear;
  fTree.Free;
  fKeys.Free;
  fValues.Free;
  inherited BeforeDestruction;
end;

function TSortedMultiMapBase<TKey, TValue>.CreateWrappedCollection(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  collection: TCollectionWrapper;
begin
  collection := TCollectionWrapper.Create;
  collection.fKey := key;
  collection.fOnDestroy := fOnDestroy;
  fOnDestroy.Add(collection.HandleDestroy);
  collection.fUpdateValues := UpdateValues;
  CreateCollection(collection.fCollection);

  Result := collection;
end;

procedure TSortedMultiMapBase<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
    FreeObject(item);
end;

procedure TSortedMultiMapBase<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
    FreeObject(item);
end;

function TSortedMultiMapBase<TKey, TValue>.Add(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := TryAdd(key, value);
end;

procedure TSortedMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: array of TValue);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    Add(key, values[i]);
end;

procedure TSortedMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: IEnumerable<TValue>);
var
  enumerator: IEnumerator<TValue>;
  item: TValue;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    Add(key, item);
  end;
end;

procedure TSortedMultiMapBase<TKey, TValue>.Clear;
var
  oldItemCount, i: Integer;
  oldItems: TArray<TPair<TKey,IInterface>>;
  oldValue: TValue;
  node: PBinaryTreeNode;
begin
  oldItemCount := fTree.Count;

  SetLength(oldItems, oldItemCount);
  node := fTree.Root.LeftMost;
  for i := 0 to oldItemCount - 1 do
  begin
    oldItems[i].Key := PNode(node).Key;
    oldItems[i].Value := PNode(node).Values;
    node := node.Next;
  end;

  fTree.Clear;

  for i := 0 to oldItemCount - 1 do
  begin
    HandleOnChanged(oldItems[i].Value);
    for oldValue in ICollection<TValue>(oldItems[i].Value) do
    begin
      if Assigned(Notify) then
        DoNotify(oldItems[i].Key, oldValue, caRemoved);
      ValueChanged(oldValue, caRemoved);
    end;
    ICollection<TValue>(oldItems[i].Value).Clear;
    oldItems[i].Value := nil;
    KeyChanged(oldItems[i].Key, caRemoved);
  end;
  fCount := 0;
end;

function TSortedMultiMapBase<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if not Assigned(node) then Exit(Boolean(node));
  Result := PNode(node).Values.Contains(value);
end;

function TSortedMultiMapBase<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node);
end;

function TSortedMultiMapBase<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  node: PBinaryTreeNode;
begin
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    if PNode(node).Values.Contains(value) then Break;
    node := node.Next;
  end;
  Result := Assigned(node);
end;

procedure TSortedMultiMapBase<TKey, TValue>.DoRemove(const node: PNode;
  action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
var
  value: TValue;
begin
  HandleOnChanged(node.Values);
  for value in node.Values do
  begin
    Dec(fCount);
    if Assigned(Notify) then
      DoNotify(node.Key, value, action);
    ValueChanged(value, action);
  end;
  if action = caRemoved then
    node.Values.Clear
  else
    node.Values.MoveTo(extractTarget);
  KeyChanged(node.Key, action);
  fTree.DeleteNode(Pointer(node));
end;

procedure TSortedMultiMapBase<TKey, TValue>.DoValueChanged(sender: TObject;
  const item: TValue; action: TCollectionChangedAction);
begin
  case action of //FI:W535
    caAdded: Inc(fCount);
    caRemoved, caExtracted: Dec(fCount);
  end;
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
{$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
    FreeObject(item);
end;

function TSortedMultiMapBase<TKey, TValue>.Extract(
  const key: TKey): ICollection<TValue>;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  CreateCollection(Result);
  if Assigned(node) then
    DoRemove(node, caExtracted, Result);
end;

function TSortedMultiMapBase<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  node: PNode;
  count: Integer;
begin
  Result.Key := key;
  node := Pointer(fTree.FindNode(key));
  if Assigned(node) then
  begin
    count := node.Values.Count;
    Result.Value := node.Values.Extract(value);
    if node.Values.Count < count then
    begin
      if Assigned(Notify) then
        DoNotify(key, value, caExtracted);
      KeyChanged(node.Key, caExtracted);
    end;
  end
  else
    Result.Value := Default(TValue);
end;

function TSortedMultiMapBase<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TSortedMultiMapBase<TKey, TValue>.GetCountFast: Integer;
begin
  Result := fCount;
end;

function TSortedMultiMapBase<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fVersion;
  end;
end;

function TSortedMultiMapBase<TKey, TValue>.GetItems(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  node: PNode;
begin
  node := Pointer(fTree.FindNode(key));
  if Assigned(node) then
    Result := node.Values as IReadOnlyCollection<TValue>
  else
    Result := CreateWrappedCollection(key);
end;

function TSortedMultiMapBase<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TSortedMultiMapBase<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TSortedMultiMapBase<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if not Assigned(node) then Exit(Boolean(node));
  DoRemove(node, caRemoved, nil);
  Result := True;
end;

function TSortedMultiMapBase<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  temp: Pointer;
  node: PNode;
begin
  temp := fTree.FindNode(key);
  if not Assigned(temp) then Exit(Boolean(temp));
  node := temp;
  Result := node.Values.Remove(value);
  if Result then
  begin
    if Assigned(Notify) then
      DoNotify(key, value, caRemoved);
    if not node.Values.Any then
      DoRemove(node, caRemoved, nil);
    Result := True;
  end;
end;

function TSortedMultiMapBase<TKey, TValue>.TryAdd(const key: TKey;
  const value: TValue): Boolean;
var
  node: PNode;
begin
  node := Pointer(fTree.AddNode(key, True));
  node := Pointer(IntPtr(node) and not 1);
  if not Assigned(node.Values) then
  begin
    CreateCollection(node.Values);
    HandleOnChanged(node.Values, @TSortedMultiMapBase<TKey, TValue>.DoValueChanged, Self);
    with fOnKeyChanged do if CanInvoke then
      Invoke(Self, node.Key, caAdded);
  end;
  Result := node.Values.Add(value);
  if Result then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    if Assigned(Notify) then
    begin
      DoNotify(node.Key, value, caAdded);
      Result := True;
    end;
  end;
end;

function TSortedMultiMapBase<TKey, TValue>.TryGetValues(const key: TKey;
  var values: IReadOnlyCollection<TValue>): Boolean;
var
  temp: Pointer;
  node: PNode;
begin
  temp := fTree.FindNode(key);
  if not Assigned(temp) then Exit(Boolean(temp));
  node := temp;
  values := node.Values as IReadOnlyCollection<TValue>;
  Result := True;
end;

procedure TSortedMultiMapBase<TKey, TValue>.UpdateValues(collection: TObject);
var
  node: PNode;
begin
  node := Pointer(fTree.FindNode(TCollectionWrapper(collection).fKey));
  if Assigned(node) then
    TCollectionWrapper(collection).fCollection := node.Values;
end;

{$ENDREGION}


{$REGION 'TSortedMultiMapBase<TKey, TValue>.TEnumerator'}

function TSortedMultiMapBase<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result.Key := PNode(fNode).Key;
  Result.Value := fEnumerator.Current;
end;

function TSortedMultiMapBase<TKey, TValue>.TEnumerator.MoveNext: Boolean;
var
  tree: TBinaryTree;
  node: PBinaryTreeNode;
begin
  tree := fSource.fTree;
  if fVersion = fSource.fVersion then
  begin
    repeat
      if Assigned(fEnumerator) then
      begin
        Result := fEnumerator.MoveNext;
        if Result then Exit;
      end;

      if (tree.Count > 0) and (fNode <> Pointer(1)) then
      begin
        if Assigned(fNode) then
          node := fNode.Next
        else
          node := tree.Root.LeftMost;
        if Assigned(node) then
        begin
          fNode := node;
          {$IFDEF MSWINDOWS}
          IEnumerableInternal(PNode(node).Values).GetEnumerator(IEnumerator(fEnumerator));
          {$ELSE}
          fEnumerator := PNode(node).Values.GetEnumerator;
          {$ENDIF}
          Continue;
        end;
      end;

      fNode := Pointer(1);
      fEnumerator := nil;
      Exit(False);
    until False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TListMultiMap<TKey, TValue>'}

function TListMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

procedure TListMultiMap<TKey, TValue>.CreateCollection(var result: ICollection<TValue>);
var
  valueType: PTypeInfo;
  comparer: Pointer;
begin
  valueType := GetValueType;
  comparer := _LookupVtableInfo(giComparer, valueType, SizeOf(TValue));
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollections.CreateList_Object(Pointer(comparer), False, Result, valueType);
    tkInterface: TCollections.CreateList_Interface(Pointer(comparer), Result, valueType);
    tkUString: TCollections.CreateList_String(Pointer(comparer), Result, valueType);
    tkMethod: TCollections.CreateList_Method(Pointer(comparer), Result, valueType);
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(TValue) of
        1: TCollections.CreateList_Int8(Pointer(comparer), Result, valueType);
        2: TCollections.CreateList_Int16(Pointer(comparer), Result, valueType);
        4: TCollections.CreateList_Int32(Pointer(comparer), Result, valueType);
        8: TCollections.CreateList_Int64(Pointer(comparer), Result, valueType);
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<TValue>.Create(IComparer<TValue>(comparer));
  end;
end;

{$ENDREGION}


{$REGION 'THashMultiMap<TKey, TValue>'}

constructor THashMultiMap<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;
  fValueComparer := valueComparer;
end;

procedure THashMultiMap<TKey, TValue>.CreateCollection(var result: ICollection<TValue>);
var
  valueType: PTypeInfo;
  comparer: Pointer;
begin
  valueType := GetValueType;
  comparer := Pointer(fValueComparer);
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollections.CreateHashSet_Object(0, nil, result, valueType);
    tkInterface: TCollections.CreateHashSet_Interface(0, nil, result, valueType);
    tkUString: TCollections.CreateHashSet_String(0, nil, result, valueType);
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(TValue) of
        1: TCollections.CreateHashSet_Int8(0, nil, result, valueType);
        2: TCollections.CreateHashSet_Int16(0, nil, result, valueType);
        4: TCollections.CreateHashSet_Int32(0, nil, result, valueType);
        8: TCollections.CreateHashSet_Int64(0, nil, result, valueType);
      end;
  else{$ELSE}begin{$ENDIF}
    result := THashSet<TValue>.Create(0, nil);
  end;
end;

function THashMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TTreeMultiMap<TKey, TValue>'}

constructor TTreeMultiMap<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IComparer<TValue>; ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;
  fValueComparer := valueComparer;
end;

procedure TTreeMultiMap<TKey, TValue>.CreateCollection(var result: ICollection<TValue>);
begin
  result := TCollections.CreateSortedSet<TValue>(fValueComparer);
end;

function TTreeMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TSortedListMultiMap<TKey, TValue>'}

function TSortedListMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

procedure TSortedListMultiMap<TKey, TValue>.CreateCollection(
  var result: ICollection<TValue>);
var
  valueType: PTypeInfo;
  comparer: Pointer;
begin
  valueType := GetValueType;
  comparer := _LookupVtableInfo(giComparer, valueType, SizeOf(TValue));
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollections.CreateList_Object(Pointer(comparer), False, Result, valueType);
    tkInterface: TCollections.CreateList_Interface(Pointer(comparer), Result, valueType);
    tkUString: TCollections.CreateList_String(Pointer(comparer), Result, valueType);
    tkMethod: TCollections.CreateList_Method(Pointer(comparer), Result, valueType);
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(TValue) of
        1: TCollections.CreateList_Int8(Pointer(comparer), Result, valueType);
        2: TCollections.CreateList_Int16(Pointer(comparer), Result, valueType);
        4: TCollections.CreateList_Int32(Pointer(comparer), Result, valueType);
        8: TCollections.CreateList_Int64(Pointer(comparer), Result, valueType);
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<TValue>.Create(IComparer<TValue>(comparer));
  end;
end;

{$ENDREGION}


{$REGION 'TSortedHashMultiMap<TKey, TValue>'}

constructor TSortedHashMultiMap<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fKeyComparer := keyComparer;
  fOwnerships := ownerships;
  fValueComparer := valueComparer;
end;

procedure TSortedHashMultiMap<TKey, TValue>.CreateCollection(
  var result: ICollection<TValue>);
var
  valueType: PTypeInfo;
  comparer: Pointer;
begin
  valueType := GetValueType;
  comparer := Pointer(fValueComparer);
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollections.CreateHashSet_Object(0, nil, result, valueType);
    tkInterface: TCollections.CreateHashSet_Interface(0, nil, result, valueType);
    tkUString: TCollections.CreateHashSet_String(0, nil, result, valueType);
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(TValue) of
        1: TCollections.CreateHashSet_Int8(0, nil, result, valueType);
        2: TCollections.CreateHashSet_Int16(0, nil, result, valueType);
        4: TCollections.CreateHashSet_Int32(0, nil, result, valueType);
        8: TCollections.CreateHashSet_Int64(0, nil, result, valueType);
      end;
  else{$ELSE}begin{$ENDIF}
    result := THashSet<TValue>.Create(0, nil);
  end;
end;

function TSortedHashMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TSortedTreeMultiMap<TKey, TValue>'}

constructor TSortedTreeMultiMap<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>; const valueComparer: IComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fKeyComparer := keyComparer;
  fOwnerships := ownerships;
  fValueComparer := valueComparer;
end;

procedure TSortedTreeMultiMap<TKey, TValue>.CreateCollection(
  var result: ICollection<TValue>);
begin
  result := TCollections.CreateSortedSet<TValue>(fValueComparer);
end;

function TSortedTreeMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TFoldedListMultiMap<TKey, TValue>'}

constructor TFoldedListMultiMap<TKey, TValue>.Create(keyType,
  valueType, elementType: PTypeInfo; const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(keyType);

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(valueType);

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;

  fElementType := elementType;
  fKeyType := keyType;
  fValueType := valueType;
end;

function TFoldedListMultiMap<TKey, TValue>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TFoldedListMultiMap<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := fKeyType;
end;

function TFoldedListMultiMap<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := fValueType;
end;

{$ENDREGION}


end.
