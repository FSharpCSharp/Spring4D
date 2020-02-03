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

unit Spring.Collections.Dictionaries;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.HashTable,
  Spring.Collections.Trees,
  Spring.Events.Base;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

type
  TDictionaryItem<TKey, TValue> = packed record
  public
    HashCode: Integer;
    Key: TKey;
    Value: TValue;
  end;

  TDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IDictionary<TKey, TValue>)
  protected
  {$REGION 'Nested Types'}
    type
      TKeyValuePair = TPair<TKey, TValue>;
      PKeyValuePair = ^TKeyValuePair;
      TItem = TDictionaryItem<TKey, TValue>;
      TItems = TArray<TItem>;
      PItem = ^TItem;

      TEnumerator = class(THashTableEnumerator, IEnumerator<TKeyValuePair>)
      private
        fCurrent: TKeyValuePair;
        function GetCurrent: TKeyValuePair;
        function MoveNext: Boolean;
      end;

      TKeyCollection = TInnerCollection<TKey>;
      TValueCollection = TInnerCollection<TValue>;

      TComparer = TPairByKeyComparer<TKey, TValue>;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fKeyComparer: IEqualityComparer<TKey>;
    fValueComparer: IEqualityComparer<TValue>;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fOwnerships: TDictionaryOwnerships;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItem(const key: TKey): TValue;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetValues: IReadOnlyCollection<TValue>;
    procedure SetCapacity(value: Integer);
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}
    procedure DoRemove(const entry: THashTableEntry; action: TCollectionChangedAction);
    class function EqualsThunk(instance: Pointer; const left, right): Boolean; static;
  protected
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); inline;
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction); inline;
    property Capacity: Integer read GetCapacity;
  public
    constructor Create(capacity: Integer;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);

    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
    function Contains(const value: TKeyValuePair;
      const comparer: IEqualityComparer<TKeyValuePair>): Boolean; overload;
    function ToArray: TArray<TKeyValuePair>;
    function TryGetElementAt(out item: TKeyValuePair; index: Integer): Boolean;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); overload;
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

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
    function TryExtract(const key: TKey; out value: TValue): Boolean;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    function TryUpdateValue(const key: TKey; const newValue: TValue; out oldValue: TValue): Boolean;
    procedure TrimExcess;
    function AsReadOnly: IReadOnlyDictionary<TKey, TValue>;
  {$ENDREGION}
  end;

  TBidiDictionaryItem<TKey, TValue> = record
  public
    KeyHashCode: Integer;
    ValueHashCode: Integer;
    Key: TKey;
    Value: TValue;
    function Removed: Boolean; inline;
  end;

  TBidiDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>,
    IDictionary<TKey, TValue>, IBidiDictionary<TKey, TValue>)
  protected
  {$REGION 'Nested Types'}
    type
      TKeyValuePair = TPair<TKey, TValue>;
      TValueKeyPair = TPair<TValue, TKey>;
      TItem = TBidiDictionaryItem<TKey, TValue>;
      PItem = ^TItem;

      TInverse = class(TCollectionBase<TValueKeyPair>,
        IEnumerable<TValueKeyPair>, IReadOnlyCollection<TValueKeyPair>,
        IReadOnlyMap<TValue, TKey>, IReadOnlyDictionary<TValue, TKey>,
        ICollection<TValueKeyPair>, IMap<TValue, TKey>,
        IDictionary<TValue, TKey>, IBidiDictionary<TValue, TKey>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TBidiDictionary<TKey, TValue>;
      {$REGION 'Property Accessors'}
        function GetCapacity: Integer;
        function GetCount: Integer;
        function GetInverse: IBidiDictionary<TKey, TValue>;
        function GetIsEmpty: Boolean;
        function GetItem(const value: TValue): TKey;
        function GetKeys: IReadOnlyCollection<TValue>;
        function GetKeyType: PTypeInfo;
        function GetOnKeyChanged: ICollectionChangedEvent<TValue>;
        function GetOnValueChanged: ICollectionChangedEvent<TKey>;
        function GetValues: IReadOnlyCollection<TKey>;
        function GetValueType: PTypeInfo;
        procedure SetCapacity(value: Integer);
        procedure SetItem(const value: TValue; const key: TKey);
      {$ENDREGION}
      protected
        procedure Changed(const item: TValueKeyPair; action: TCollectionChangedAction); override;
      public
        constructor Create(const source: TBidiDictionary<TKey, TValue>);

      {$REGION 'Implements IInterface'}
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
      {$ENDREGION}

      {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
        function GetEnumerator: IEnumerator<TValueKeyPair>;
        function Contains(const value: TValueKeyPair): Boolean; overload;
        function Contains(const value: TValueKeyPair;
          const comparer: IEqualityComparer<TValueKeyPair>): Boolean; overload;
        function ToArray: TArray<TValueKeyPair>;
        function TryGetElementAt(out item: TValueKeyPair; index: Integer): Boolean;
      {$ENDREGION}

      {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
        function Add(const item: TValueKeyPair): Boolean; overload;
        function Remove(const item: TValueKeyPair): Boolean; overload;
        function Extract(const item: TValueKeyPair): TValueKeyPair; overload;
        procedure Clear;
      {$ENDREGION}

      {$REGION 'Implements IMap<TValue, TKey>'}
        procedure Add(const value: TValue; const key: TKey); overload;
        function TryAdd(const value: TValue; const key: TKey): Boolean;
        function Remove(const value: TValue): Boolean; overload;
        function Remove(const value: TValue; const key: TKey): Boolean; overload;
        function Extract(const value: TValue; const key: TKey): TValueKeyPair; overload;
        function Contains(const value: TValue; const key: TKey): Boolean; overload;
        function ContainsKey(const value: TValue): Boolean;
        function ContainsValue(const key: TKey): Boolean;
        property Keys: IReadOnlyCollection<TValue> read GetKeys;
        property Values: IReadOnlyCollection<TKey> read GetValues;
      {$ENDREGION}

      {$REGION 'Implements IDictionary<TValue, TKey>'}
        procedure AddOrSetValue(const value: TValue; const key: TKey);
        function Extract(const value: TValue): TKey; overload;
        function GetValueOrDefault(const value: TValue): TKey; overload;
        function GetValueOrDefault(const value: TValue; const defaultKey: TKey): TKey; overload;
        function TryExtract(const value: TValue; out key: TKey): Boolean;
        function TryGetValue(const value: TValue; out key: TKey): Boolean;
        function TryUpdateValue(const value: TValue; const newKey: TKey; out oldKey: TKey): Boolean;
        procedure TrimExcess;
        function AsReadOnly: IReadOnlyDictionary<TValue, TKey>;
      {$ENDREGION}
      end;

      TEnumerator = class(TRefCountedObject,
        IEnumerator<TKeyValuePair>, IEnumerator<TKey>, IEnumerator<TValue>,
        IEnumerator<TValueKeyPair>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TBidiDictionary<TKey, TValue>;
        fItemIndex: Integer;
        fVersion: Integer;
        function GetCurrent: TKeyValuePair;
        function GetCurrentInverse: TValueKeyPair;
        function GetCurrentKey: TKey;
        function GetCurrentValue: TValue;
        function IEnumerator<TValueKeyPair>.GetCurrent = GetCurrentInverse;
        function IEnumerator<TKey>.GetCurrent = GetCurrentKey;
        function IEnumerator<TValue>.GetCurrent = GetCurrentValue;
      public
        constructor Create(const source: TBidiDictionary<TKey, TValue>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;

      TKeyCollection = class(TEnumerableBase<TKey>,
        IEnumerable<TKey>, IReadOnlyCollection<TKey>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TBidiDictionary<TKey, TValue>;
      {$REGION 'Property Accessors'}
        function GetCount: Integer;
        function GetIsEmpty: Boolean;
      {$ENDREGION}
      public
        constructor Create(const source: TBidiDictionary<TKey, TValue>);

      {$REGION 'Implements IInterface'}
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
      {$ENDREGION}

      {$REGION 'Implements IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<TKey>;
        function Contains(const value: TKey): Boolean; overload;
        function ToArray: TArray<TKey>;
        function TryGetElementAt(out key: TKey; index: Integer): Boolean;
      {$ENDREGION}
      end;

      TValueCollection = class(TEnumerableBase<TValue>,
        IEnumerable<TValue>, IReadOnlyCollection<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TBidiDictionary<TKey, TValue>;
      {$REGION 'Property Accessors'}
        function GetCount: Integer;
        function GetIsEmpty: Boolean;
      {$ENDREGION}
      public
        constructor Create(const source: TBidiDictionary<TKey, TValue>);

      {$REGION 'Implements IInterface'}
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
      {$ENDREGION}

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>;
        function Contains(const value: TValue): Boolean; overload;
        function ToArray: TArray<TValue>;
        function TryGetElementAt(out value: TValue; index: Integer): Boolean;
      {$ENDREGION}
      end;
  {$ENDREGION}
  private
    fKeyBuckets: TArray<Integer>;
    fValueBuckets: TArray<Integer>;
    fItems: TArray<TItem>;
    fCount: Integer;
    fItemCount: Integer;
    fVersion: Integer;
    fBucketIndexMask: Integer;
    fBucketHashCodeMask: Integer;
    fKeyComparer: IEqualityComparer<TKey>;
    fValueComparer: IEqualityComparer<TValue>;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fInverse: TInverse;
    fOwnerships: TDictionaryOwnerships;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer;
    function GetInverse: IBidiDictionary<TValue, TKey>;
    function GetIsEmpty: Boolean;
    function GetItem(const key: TKey): TValue;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetValues: IReadOnlyCollection<TValue>;
    procedure SetCapacity(value: Integer);
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}
    procedure Rehash(newCapacity: Integer);
    procedure EnsureCompact;
    procedure Grow;
    function FindKey(const key: TKey; hashCode: Integer;
      out bucketIndex, itemIndex: Integer): Boolean;
    function FindValue(const value: TValue; hashCode: Integer;
      out bucketIndex, itemIndex: Integer): Boolean;
    function KeyHash(const key: TKey): Integer; inline;
    function ValueHash(const value: TValue): Integer; inline;
    procedure DoAdd(keyhashCode, keyBucketIndex, valueHashCode, valueBucketIndex,
      itemIndex: Integer; const key: TKey; const value: TValue);
    procedure DoRemove(keyBucketIndex, valueBucketIndex, itemIndex: Integer;
      action: TCollectionChangedAction);
    procedure DoSetKey(valueBucketIndex, itemIndex, keyHashCode: Integer;
      const key: TKey);
    procedure DoSetValue(keyBucketIndex, itemIndex, valueHashCode: Integer;
      const value: TValue);
    function DoMoveNext(var itemIndex: Integer;
      iteratorVersion: Integer): Boolean;

    procedure AddOrSetKey(const value: TValue; const key: TKey);
  protected
    procedure Changed(const item: TPair<TKey, TValue>; action: TCollectionChangedAction); override;
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction);
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction);
    property Capacity: Integer read GetCapacity;
  public
    constructor Create(capacity: Integer;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
    function Contains(const value: TKeyValuePair;
      const comparer: IEqualityComparer<TKeyValuePair>): Boolean; overload;
    function ToArray: TArray<TKeyValuePair>;
    function TryGetElementAt(out item: TKeyValuePair; index: Integer): Boolean;
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

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
    function TryExtract(const key: TKey; out value: TValue): Boolean;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    function TryUpdateValue(const key: TKey; const newValue: TValue; out oldValue: TValue): Boolean;
    procedure TrimExcess;
    function AsReadOnly: IReadOnlyDictionary<TKey, TValue>;

    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  {$ENDREGION}
  end;

  TSortedDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IDictionary<TKey, TValue>)
  private
  {$REGION 'Nested Types'}
    type
      TKeyValuePair = TPair<TKey, TValue>;
      PKeyValuePair = ^TKeyValuePair;
      PNode = TNodes<TKey, TValue>.PRedBlackTreeNode;

      TEnumerator = class(TRefCountedObject,
        IEnumerator<TKeyValuePair>, IEnumerator<TKey>, IEnumerator<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TSortedDictionary<TKey, TValue>;
        fCurrentNode: PNode;
        fFinished: Boolean;
        fVersion: Integer;
        function GetCurrent: TKeyValuePair;
        function GetCurrentKey: TKey;
        function GetCurrentValue: TValue;
        function IEnumerator<TKey>.GetCurrent = GetCurrentKey;
        function IEnumerator<TValue>.GetCurrent = GetCurrentValue;
      public
        constructor Create(const source: TSortedDictionary<TKey, TValue>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;

      TKeyCollection = class(TEnumerableBase<TKey>,
        IEnumerable<TKey>, IReadOnlyCollection<TKey>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TSortedDictionary<TKey, TValue>;
      {$REGION 'Property Accessors'}
        function GetCount: Integer;
        function GetIsEmpty: Boolean;
      {$ENDREGION}
      public
        constructor Create(const source: TSortedDictionary<TKey, TValue>);

      {$REGION 'Implements IInterface'}
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
      {$ENDREGION}

      {$REGION 'Implements IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<TKey>;
        function Contains(const value: TKey): Boolean; overload;
        function ToArray: TArray<TKey>;
      {$ENDREGION}
      end;

      TValueCollection = class(TEnumerableBase<TValue>,
        IEnumerable<TValue>, IReadOnlyCollection<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TSortedDictionary<TKey, TValue>;
      {$REGION 'Property Accessors'}
        function GetCount: Integer;
        function GetIsEmpty: Boolean;
      {$ENDREGION}
      public
        constructor Create(const source: TSortedDictionary<TKey, TValue>);

      {$REGION 'Implements IInterface'}
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
      {$ENDREGION}

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>;
        function Contains(const value: TValue): Boolean; overload;
        function ToArray: TArray<TValue>;
      {$ENDREGION}
      end;

      TComparer = TPairByKeyComparer<TKey, TValue>;
  {$ENDREGION}
  private
    fTree: TRedBlackTree<TKey,TValue>;
    fKeyComparer: IComparer<TKey>;
    fValueComparer: IEqualityComparer<TValue>;
    fVersion: Integer;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItem(const key: TKey): TValue;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetValues: IReadOnlyCollection<TValue>;
    procedure SetCapacity(value: Integer);
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}
    function DoMoveNext(var currentNode: PNode; var finished: Boolean;
      iteratorVersion: Integer): Boolean;
  public
    constructor Create; override;
    constructor Create(const keyComparer: IComparer<TKey>; const valueComparer: IEqualityComparer<TValue>); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
    function Contains(const value: TKeyValuePair;
      const comparer: IEqualityComparer<TKeyValuePair>): Boolean; overload;
    function ToArray: TArray<TKeyValuePair>;
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

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; overload;
    function TryExtract(const key: TKey; out value: TValue): Boolean;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    function TryUpdateValue(const key: TKey; const newValue: TValue; out oldValue: TValue): Boolean;
    procedure TrimExcess;
    function AsReadOnly: IReadOnlyDictionary<TKey, TValue>;

    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyDictionary<TKey, TValue>'}
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
  {$ENDREGION}
  end;

  TFoldedDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
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
      capacity: Integer;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

  TFoldedBidiDictionary<TKey, TValue> = class(TBidiDictionary<TKey, TValue>)
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
      capacity: Integer;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

implementation

uses
  SysUtils,
  Types,
  TypInfo,
  Spring.ResourceStrings;


{$REGION 'TDictionary<TKey, TValue>'}

constructor TDictionary<TKey, TValue>.Create(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(capacity >= 0, 'capacity');
{$ENDIF}

  if doOwnsKeys in ownerships then
    if KeyType.Kind <> tkClass then
      raise Error.NoClassType(KeyType);

  if doOwnsValues in ownerships then
    if ValueType.Kind <> tkClass then
      raise Error.NoClassType(ValueType);

  inherited Create();
  fComparer := TComparer.Create(nil);
  fOwnerships := ownerships;
  if Assigned(keyComparer) then
    fKeyComparer := keyComparer
  else
    fKeyComparer := IEqualityComparer<TKey>(_LookupVtableInfo(giEqualityComparer, KeyType, SizeOf(TKey)));
  if Assigned(valueComparer) then
    fValueComparer := valueComparer
  else
    fValueComparer := IEqualityComparer<TValue>(_LookupVtableInfo(giEqualityComparer, ValueType, SizeOf(TValue)));

  fKeys := TKeyCollection.Create(Self, @fHashTable, KeyType, fKeyComparer, 0);
  fValues := TValueCollection.Create(Self, @fHashTable, ValueType, fValueComparer, SizeOf(TKey));

  fHashTable.Initialize(TypeInfo(TItems), @EqualsThunk, fKeyComparer);

  SetCapacity(capacity);
end;

destructor TDictionary<TKey, TValue>.Destroy;
begin
  Clear;
  fKeys.Free;
  fValues.Free;
  inherited Destroy;
end;

procedure TDictionary<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
    FreeObject(item);
end;

procedure TDictionary<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
    FreeObject(item);
end;

function TDictionary<TKey, TValue>.GetCapacity: Integer;
begin
  Result := fHashTable.Capacity;
end;

procedure TDictionary<TKey, TValue>.SetCapacity(value: Integer);
begin
  fHashTable.Capacity := value;
end;

procedure TDictionary<TKey, TValue>.DoRemove(const entry: THashTableEntry;
  action: TCollectionChangedAction);
var
  item: PItem;
begin
  item := fHashTable.Delete(entry);

  if Assigned(Notify) then
    DoNotify(item.Key, item.Value, action);
  KeyChanged(item.Key, action);
  ValueChanged(item.Value, action);

  item.Key := Default(TKey);
  item.Value := Default(TValue);
end;

function TDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  Result := TEnumerator.Create(Self, @fHashTable);
end;

procedure TDictionary<TKey, TValue>.Clear;
var
  oldItemCount, i: Integer;
  oldItems: TArray<TItem>;
begin
  oldItems := TItems(fHashTable.Items);
  oldItemCount := fHashTable.ItemCount;

  fHashTable.Clear;

  for i := 0 to oldItemCount - 1 do
    if oldItems[i].HashCode >= 0 then
    begin
      if Assigned(Notify) then
        DoNotify(oldItems[i].Key, oldItems[i].Value, caRemoved);
      KeyChanged(oldItems[i].Key, caRemoved);
      ValueChanged(oldItems[i].Value, caRemoved);
    end;
end;

function TDictionary<TKey, TValue>.Contains(const value: TKeyValuePair;
  const comparer: IEqualityComparer<TKeyValuePair>): Boolean;
var
  pair: TKeyValuePair;
begin
  pair.Key := value.Key;
  Result := TryGetValue(value.Key, pair.Value)
    and comparer.Equals(pair, value);
end;

function TDictionary<TKey, TValue>.ToArray: TArray<TKeyValuePair>;
var
  target: PKeyValuePair;
  source: PItem;
  i: Integer;
begin
  SetLength(Result, fHashTable.Count);
  target := Pointer(Result);
  source := PItem(fHashTable.Items);
  for i := 1 to fHashTable.ItemCount do
  begin
    if source.HashCode <> RemovedFlag then
    begin
      target.Key := source.Key;
      target.Value := source.Value;
      Inc(target);
    end;
    Inc(source);
  end;
end;

function TDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fHashTable.Count;
end;

function TDictionary<TKey, TValue>.GetIsEmpty: Boolean;
begin
  Result := fHashTable.Count = 0;
end;

procedure TDictionary<TKey, TValue>.Add(const key: TKey; const value: TValue);
var
  item: PItem;
begin
  item := fHashTable.Add(key, fKeyComparer.GetHashCode(key));
  if Assigned(item) then
  begin
    item.Key := key;
    item.Value := value;

    if Assigned(Notify) then
      DoNotify(key, value, caAdded);
    KeyChanged(key, caAdded);
    ValueChanged(value, caAdded);
  end
  else
    raise Error.DuplicateKey;
end;

procedure TDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
begin
  SetItem(key, value);
end;

function TDictionary<TKey, TValue>.AsReadOnly: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

function TDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  Result := fHashTable.Find(key, entry);
end;

class function TDictionary<TKey, TValue>.EqualsThunk(instance: Pointer; const left, right): Boolean;
begin
  Result := TEqualsMethod<TKey>(instance^)(TKey(left), TKey(right));
end;

function TDictionary<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := TryGetValue(key, item) and fValueComparer.Equals(item, value);
end;

function TDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
var
  i: Integer;
begin
  for i := 0 to fHashTable.ItemCount - 1 do
    if TItems(fHashTable.Items)[i].HashCode >= 0 then
      if fValueComparer.Equals(TItems(fHashTable.Items)[i].Value, value) then
        Exit(True);
  Result := False;
end;

function TDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  TryExtract(key, Result);
end;

function TDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  entry: THashTableEntry;
  item: PItem;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  if fHashTable.Find(key, entry) then
  begin
    item := @TItems(fHashTable.Items)[entry.ItemIndex];
    if fValueComparer.Equals(item.Value, Value) then
    begin
      Result.Key := item.Key;
      Result.Value := item.Value;
      DoRemove(entry, caExtracted);
      Exit;
    end;
  end;
  Result := Default(TKeyValuePair);
end;

procedure TDictionary<TKey, TValue>.TrimExcess;
begin
  SetCapacity(fHashTable.Count);
end;

function TDictionary<TKey, TValue>.TryAdd(const key: TKey;
  const value: TValue): Boolean;
var
  item: PItem;
begin
  item := fHashTable.Add(key, fKeyComparer.GetHashCode(key));
  Result := Assigned(item);
  if Result then
  begin
    item.Key := key;
    item.Value := value;

    if Assigned(Notify) then
      DoNotify(key, value, caAdded);
    KeyChanged(key, caAdded);
    ValueChanged(value, caAdded);
  end;
end;

function TDictionary<TKey, TValue>.TryExtract(const key: TKey;
  out value: TValue): Boolean;
var
  item: PItem;
begin
  item := fHashTable.Delete(key, fKeyComparer.GetHashCode(key));
  Result := Assigned(item);
  if Result then
  begin
    value := item.Value;

    if Assigned(Notify) then
      DoNotify(item.Key, item.Value, caExtracted);
    KeyChanged(item.Key, caExtracted);
    ValueChanged(item.Value, caExtracted);

    item.Key := Default(TKey);
    item.Value := Default(TValue);
  end
  else
    value := Default(TValue);
end;

function TDictionary<TKey, TValue>.TryGetElementAt(out item: TKeyValuePair; index: Integer): Boolean;
begin
  Result := Cardinal(index) < Cardinal(fHashTable.Count);
  if Result then
  begin
    fHashTable.EnsureCompact;
    item.Key := TItems(fHashTable.Items)[index].Key;
    item.Value := TItems(fHashTable.Items)[index].Value;
  end;
end;

function TDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  Result := fHashTable.Find(key, entry);
  if Result then
    value := TItems(fHashTable.Items)[entry.ItemIndex].Value
  else
    value := Default(TValue);
end;

function TDictionary<TKey, TValue>.TryUpdateValue(const key: TKey;
  const newValue: TValue; out oldValue: TValue): Boolean;
var
  entry: THashTableEntry;
  item: PItem;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  Result := fHashTable.Find(key, entry);
  if Result then
  begin
    item := @TItems(fHashTable.Items)[entry.ItemIndex];
    oldValue := item.Value;

    fHashTable.IncrementVersion;

    if Assigned(Notify) then
      DoNotify(key, oldValue, caRemoved);
    ValueChanged(oldValue, caRemoved);

    item.Value := newValue;

    if Assigned(Notify) then
      DoNotify(key, newValue, caAdded);
    ValueChanged(newValue, caAdded);
  end
  else
    oldValue := Default(TValue);
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  item: PItem;
begin
  item := fHashTable.Delete(key, fKeyComparer.GetHashCode(key));
  Result := Assigned(item);
  if Result then
  begin
    if Assigned(Notify) then
      DoNotify(item.Key, item.Value, caRemoved);
    KeyChanged(item.Key, caRemoved);
    ValueChanged(item.Value, caRemoved);

    item.Key := Default(TKey);
    item.Value := Default(TValue);
  end;
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  Result := fHashTable.Find(key, entry)
    and fValueComparer.Equals(TItems(fHashTable.Items)[entry.ItemIndex].Value, value);
  if Result then
    DoRemove(entry, caRemoved);
end;

function TDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
begin
  TryGetValue(key, Result);
end;

function TDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not TryGetValue(key, Result) then
    Result := defaultValue;
end;

function TDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  if not fHashTable.Find(key, entry) then
    raise Error.KeyNotFound;
  Result := TItems(fHashTable.Items)[entry.ItemIndex].Value;
end;

procedure TDictionary<TKey, TValue>.SetItem(const key: TKey; const value: TValue);
var
  item: PItem;
  isExisting: Boolean;
begin
  item := fHashTable.AddOrSet(key, fKeyComparer.GetHashCode(key), isExisting);

  if isExisting then
  begin
    if Assigned(Notify) then
      DoNotify(item.Key, item.Value, caRemoved);
    ValueChanged(item.Value, caRemoved);
  end;

  item.Key := key;
  item.Value := value;

  if Assigned(Notify) then
    DoNotify(key, value, caAdded);
  if not isExisting then
    KeyChanged(key, caAdded);
  ValueChanged(value, caAdded);
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TEnumerator'}

function TDictionary<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result := fCurrent;
end;

function TDictionary<TKey, TValue>.TEnumerator.MoveNext: Boolean;
var
  item: PItem;
begin
  Result := inherited MoveNext;
  if Result then
  begin
    item := @TItems(fHashTable.Items)[fIndex - 1];
    fCurrent.Key := item.Key;
    fCurrent.Value := item.Value;
  end;
end;

{$ENDREGION}


{$REGION 'TBidiDictionaryItem<TKey, TValue>' }

function TBidiDictionaryItem<TKey, TValue>.Removed: Boolean;
begin
  Result := KeyHashCode < 0;
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>'}

constructor TBidiDictionary<TKey, TValue>.Create(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(capacity >= 0, 'capacity');
{$ENDIF}

  if doOwnsKeys in ownerships then
    if KeyType.Kind <> tkClass then
      raise Error.NoClassType(KeyType);

  if doOwnsValues in ownerships then
    if ValueType.Kind <> tkClass then
      raise Error.NoClassType(ValueType);

  inherited Create;
  fOwnerships := ownerships;
  fKeys := TKeyCollection.Create(Self);
  fValues := TValueCollection.Create(Self);
  if Assigned(keyComparer) then
    fKeyComparer := keyComparer
  else
    fKeyComparer := IEqualityComparer<TKey>(_LookupVtableInfo(giEqualityComparer, TypeInfo(TKey), SizeOf(TKey)));
  if Assigned(valueComparer) then
    fValueComparer := valueComparer
  else
    fValueComparer := IEqualityComparer<TValue>(_LookupVtableInfo(giEqualityComparer, TypeInfo(TValue), SizeOf(TValue)));
  fInverse := TInverse.Create(Self);
  SetCapacity(capacity);
end;

destructor TBidiDictionary<TKey, TValue>.Destroy;
begin
  Clear;
  fInverse.Free;
  fKeys.Free;
  fValues.Free;
  inherited Destroy;
end;

procedure TBidiDictionary<TKey, TValue>.Changed(const item: TPair<TKey, TValue>;
  action: TCollectionChangedAction);
var
  inverseItem: TValueKeyPair;
begin
  inherited Changed(item, action);
  inverseItem.Key := item.Value;
  inverseItem.Value := item.Key;
  fInverse.Changed(inverseItem, action);
end;

procedure TBidiDictionary<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
    FreeObject(item);
end;

procedure TBidiDictionary<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
    FreeObject(item);
end;

function TBidiDictionary<TKey, TValue>.GetCapacity: Integer;
begin
  Result := DynArrayLength(fItems);
end;

procedure TBidiDictionary<TKey, TValue>.SetCapacity(value: Integer);
var
  newCapacity: Integer;
begin
  if value = 0 then
    newCapacity := 0
  else
  begin
    newCapacity := value;
    if newCapacity < MinCapacity then
      newCapacity := MinCapacity;
  end;
  if newCapacity <> Capacity then
    Rehash(newCapacity);
end;

procedure TBidiDictionary<TKey, TValue>.Rehash(newCapacity: Integer);
var
  newBucketCount: Integer;
  bucketIndex, itemIndex: Integer;
  sourceItemIndex, targetItemIndex: Integer;
begin
  if newCapacity = 0 then
  begin
    Assert(fCount = 0);
    Assert(fItemCount = 0);
    Assert(not Assigned(fKeyBuckets));
    Assert(not Assigned(fValueBuckets));
    Assert(not Assigned(fItems));
    Exit;
  end;

  Assert(newCapacity >= fCount);

  IncUnchecked(fVersion);

  newBucketCount := NextPowerOf2(newCapacity * 4 div 3 - 1); // 75% load factor

  // compact the items array, if necessary
  if fItemCount > fCount then
  begin
    targetItemIndex := 0;
    for sourceItemIndex := 0 to fItemCount - 1 do
      if not fItems[sourceItemIndex].Removed then
      begin
        if targetItemIndex < sourceItemIndex then
          TArrayManager<TItem>.Move(fItems, sourceItemIndex, targetItemIndex, 1);
        Inc(targetItemIndex);
      end;
    TArrayManager<TItem>.Finalize(fItems, targetItemIndex, fItemCount - fCount);
  end;

  // resize the items array, safe now that we have compacted it
  SetLength(fItems, newBucketCount * 3 div 4);
  Assert(Capacity >= fCount);

  // repopulate the bucket array
  Assert(IsPowerOf2(newBucketCount));
  fBucketIndexMask := newBucketCount - 1;
  fBucketHashCodeMask := not fBucketIndexMask and not BucketSentinelFlag;
  SetLength(fKeyBuckets, newBucketCount);
  for bucketIndex := 0 to newBucketCount - 1 do
    fKeyBuckets[bucketIndex] := EmptyBucket;
  SetLength(fValueBuckets, newBucketCount);
  for bucketIndex := 0 to newBucketCount - 1 do
    fValueBuckets[bucketIndex] := EmptyBucket;
  fItemCount := 0;
  while fItemCount < fCount do
  begin
    FindKey(fItems[fItemCount].Key, fItems[fItemCount].KeyHashCode, bucketIndex, itemIndex);
    Assert(itemIndex = fItemCount);
    fKeyBuckets[bucketIndex] := itemIndex or (fItems[itemIndex].KeyHashCode and fBucketHashCodeMask);

    FindValue(fItems[fItemCount].Value, fItems[fItemCount].ValueHashCode, bucketIndex, itemIndex);
    Assert(itemIndex = fItemCount);
    fValueBuckets[bucketIndex] := itemIndex or (fItems[itemIndex].ValueHashCode and fBucketHashCodeMask);

    Inc(fItemCount);
  end;
end;

procedure TBidiDictionary<TKey, TValue>.Grow;
var
  newCapacity: Integer;
begin
  newCapacity := Capacity;
  if newCapacity = 0 then
    newCapacity := MinCapacity
  else if 2 * fCount >= Length(fKeyBuckets) then
    // only grow if load factor is greater than 0.5
    newCapacity := newCapacity * 2;
  Rehash(newCapacity);
end;

function TBidiDictionary<TKey, TValue>.FindKey(const key: TKey; hashCode: Integer;
  out bucketIndex, itemIndex: Integer): Boolean;
var
  bucketValue: Integer;
begin
  if fItems = nil then
  begin
    bucketIndex := EmptyBucket;
    itemIndex := -1;
    Exit(False);
  end;

  bucketIndex := hashCode and fBucketIndexMask;
  while True do
  begin
    bucketValue := fKeyBuckets[bucketIndex];

    if bucketValue = EmptyBucket then
    begin
      itemIndex := fItemCount;
      Exit(False);
    end;

    if (bucketValue <> UsedBucket)
      and (bucketValue and fBucketHashCodeMask = hashCode and fBucketHashCodeMask) then
    begin
      itemIndex := bucketValue and fBucketIndexMask;
      if fKeyComparer.Equals(fItems[itemIndex].Key, key) then
        Exit(True);
    end;

    bucketIndex := (bucketIndex + 1) and fBucketIndexMask;
  end;
end;

function TBidiDictionary<TKey, TValue>.FindValue(const value: TValue; hashCode: Integer;
  out bucketIndex, itemIndex: Integer): Boolean;
var
  bucketValue: Integer;
begin
  if fItems = nil then
  begin
    bucketIndex := EmptyBucket;
    itemIndex := -1;
    Exit(False);
  end;

  bucketIndex := hashCode and fBucketIndexMask;
  while True do
  begin
    bucketValue := fValueBuckets[bucketIndex];

    if bucketValue = EmptyBucket then
    begin
      itemIndex := fItemCount;
      Exit(False);
    end;

    if (bucketValue <> UsedBucket)
      and (bucketValue and fBucketHashCodeMask = hashCode and fBucketHashCodeMask) then
    begin
      itemIndex := bucketValue and fBucketIndexMask;
      if fValueComparer.Equals(fItems[itemIndex].Value, value) then
        Exit(True);
    end;

    bucketIndex := (bucketIndex + 1) and fBucketIndexMask;
  end;
end;

function TBidiDictionary<TKey, TValue>.KeyHash(const key: TKey): Integer;
begin
  Result := fKeyComparer.GetHashCode(key) and not RemovedFlag;
end;

function TBidiDictionary<TKey, TValue>.ValueHash(const value: TValue): Integer;
begin
  Result := fValueComparer.GetHashCode(value) and not RemovedFlag;
end;

procedure TBidiDictionary<TKey, TValue>.DoAdd(keyhashCode, keyBucketIndex, valueHashCode,
  valueBucketIndex, itemIndex: Integer; const key: TKey; const value: TValue);
begin
  IncUnchecked(fVersion);
  fKeyBuckets[keyBucketIndex] := itemIndex or (keyHashCode and fBucketHashCodeMask);
  fValueBuckets[valueBucketIndex] := itemIndex or (valueHashCode and fBucketHashCodeMask);
  fItems[itemIndex].KeyHashCode := keyHashCode;
  fItems[itemIndex].ValueHashCode := valueHashCode;
  fItems[itemIndex].Key := key;
  fItems[itemIndex].Value := value;
  Inc(fCount);
  Inc(fItemCount);

  if Assigned(Notify) then
    DoNotify(key, value, caAdded);
  KeyChanged(key, caAdded);
  ValueChanged(value, caAdded);
end;

procedure TBidiDictionary<TKey, TValue>.DoRemove(keyBucketIndex, valueBucketIndex,
  itemIndex: Integer; action: TCollectionChangedAction);
var
  oldKey: TKey;
  oldValue: TValue;
begin
  oldKey := fItems[itemIndex].Key;
  oldValue := fItems[itemIndex].Value;

  IncUnchecked(fVersion);
  fKeyBuckets[keyBucketIndex] := UsedBucket;
  fValueBuckets[valueBucketIndex] := UsedBucket;
  fItems[itemIndex].Key := Default(TKey);
  fItems[itemIndex].Value := Default(TValue);
  fItems[itemIndex].KeyHashCode := RemovedFlag;
  fItems[itemIndex].ValueHashCode := RemovedFlag;
  Dec(fCount);

  if Assigned(Notify) then
    DoNotify(oldKey, oldValue, action);
  KeyChanged(oldKey, action);
  ValueChanged(oldValue, action);
end;

procedure TBidiDictionary<TKey, TValue>.DoSetKey(valueBucketIndex, itemIndex,
  keyHashCode: Integer; const key: TKey);
var
  oldKey: TKey;
  oldValue: TValue;
  oldKeyHashCode, valueHashCode, oldKeyBucketIndex, oldKeyItemIndex, keyBucketIndex: Integer;
begin
  oldKey := fItems[itemIndex].Key;
  oldValue := fItems[itemIndex].Value;
  oldKeyHashCode := fItems[itemIndex].KeyHashCode;
  valueHashCode := fItems[itemIndex].ValueHashCode;

  IncUnchecked(fVersion);
  if fItemCount = Capacity then
  begin
    Grow;
    FindValue(oldValue, valueHashCode, valueBucketIndex, itemIndex);
  end;
  FindKey(oldKey, oldKeyHashCode, oldKeyBucketIndex, oldKeyItemIndex);
  Assert(oldKeyItemIndex = itemIndex);
  fKeyBuckets[oldKeyBucketIndex] := UsedBucket;
  FindKey(key, keyHashCode, keyBucketIndex, itemIndex);
  Assert(itemIndex = fItemCount);

  fKeyBuckets[keyBucketIndex] := oldKeyItemIndex or (keyHashCode and fBucketHashCodeMask);
  fValueBuckets[valueBucketIndex] := oldKeyItemIndex or (valueHashCode and fBucketHashCodeMask);

  fItems[itemIndex].Key := Default(TKey);
  fItems[itemIndex].Value := Default(TValue);
  fItems[itemIndex].KeyHashCode := RemovedFlag;
  fItems[itemIndex].ValueHashCode := RemovedFlag;

  fItems[oldKeyItemIndex].KeyHashCode := keyHashCode;
  Assert(fItems[oldKeyItemIndex].ValueHashCode = valueHashCode);
  fItems[oldKeyItemIndex].Key := key;
  Assert(fValueComparer.Equals(fItems[oldKeyItemIndex].Value, oldValue));

  Inc(fItemCount);

  if Assigned(Notify) then
    DoNotify(oldKey, oldValue, caRemoved);
  KeyChanged(oldKey, caRemoved);
  if Assigned(Notify) then
    DoNotify(key, oldValue, caAdded);
  KeyChanged(key, caAdded);
end;

procedure TBidiDictionary<TKey, TValue>.DoSetValue(keyBucketIndex, itemIndex,
  valueHashCode: Integer; const value: TValue);
var
  oldKey: TKey;
  oldValue: TValue;
  keyHashCode, oldValueHashCode, oldValueBucketIndex, oldValueItemIndex, valueBucketIndex: Integer;
begin
  oldKey := fItems[itemIndex].Key;
  oldValue := fItems[itemIndex].Value;
  keyHashCode := fItems[itemIndex].KeyHashCode;
  oldValueHashCode := fItems[itemIndex].ValueHashCode;

  IncUnchecked(fVersion);
  if fItemCount = Capacity then
  begin
    Grow;
    FindKey(oldKey, keyHashCode, keyBucketIndex, itemIndex);
  end;
  FindValue(oldValue, oldValueHashCode, oldValueBucketIndex, oldValueItemIndex);
  Assert(oldValueItemIndex = itemIndex);
  fValueBuckets[oldValueBucketIndex] := UsedBucket;
  FindValue(value, valueHashCode, valueBucketIndex, itemIndex);
  Assert(itemIndex = fItemCount);

  fKeyBuckets[keyBucketIndex] := oldValueItemIndex or (keyHashCode and fBucketHashCodeMask);
  fValueBuckets[valueBucketIndex] := oldValueItemIndex or (valueHashCode and fBucketHashCodeMask);

  fItems[itemIndex].Key := Default(TKey);
  fItems[itemIndex].Value := Default(TValue);
  fItems[itemIndex].KeyHashCode := RemovedFlag;
  fItems[itemIndex].ValueHashCode := RemovedFlag;

  Assert(fItems[oldValueItemIndex].KeyHashCode = keyHashCode);
  fItems[oldValueItemIndex].ValueHashCode := valueHashCode;
  Assert(fKeyComparer.Equals(fItems[oldValueItemIndex].Key, oldKey));
  fItems[oldValueItemIndex].Value := value;

  Inc(fItemCount);

  if Assigned(Notify) then
    DoNotify(oldKey, oldValue, caRemoved);
  ValueChanged(oldValue, caRemoved);
  if Assigned(Notify) then
    DoNotify(oldKey, value, caAdded);
  ValueChanged(value, caAdded);
end;

function TBidiDictionary<TKey, TValue>.DoMoveNext(var itemIndex: Integer;
  iteratorVersion: Integer): Boolean;
begin
  if iteratorVersion <> fVersion then
    raise Error.EnumFailedVersion;

  while itemIndex < fItemCount - 1 do
  begin
    Inc(itemIndex);
    if not fItems[itemIndex].Removed then
      Exit(True);
  end;
  Result := False;
end;

function TBidiDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  Result := TEnumerator.Create(self);
end;

procedure TBidiDictionary<TKey, TValue>.Clear;
var
  oldItemIndex, oldItemCount: Integer;
  oldItems: TArray<TItem>;
begin
  oldItemCount := fItemCount;
  oldItems := fItems;

  IncUnchecked(fVersion);
  fCount := 0;
  fItemCount := 0;
  fKeyBuckets := nil;
  fValueBuckets := nil;
  fItems := nil;
  SetCapacity(0);

  for oldItemIndex := 0 to oldItemCount - 1 do
    if not oldItems[oldItemIndex].Removed then
    begin
      if Assigned(Notify) then
        DoNotify(oldItems[oldItemIndex].Key, oldItems[oldItemIndex].Value, caRemoved);
      KeyChanged(oldItems[oldItemIndex].Key, caRemoved);
      ValueChanged(oldItems[oldItemIndex].Value, caRemoved);
    end;
end;

function TBidiDictionary<TKey, TValue>.Contains(const value: TKeyValuePair;
  const comparer: IEqualityComparer<TKeyValuePair>): Boolean;
var
  pair: TKeyValuePair;
begin
  pair.Key := value.Key;
  Result := TryGetValue(value.Key, pair.Value) and comparer.Equals(pair, value);
end;

function TBidiDictionary<TKey, TValue>.ToArray: TArray<TKeyValuePair>;
var
  sourceIndex, targetIndex: Integer;
begin
  SetLength(Result, fCount);
  targetIndex := 0;
  for sourceIndex := 0 to fItemCount - 1 do
    if not fItems[sourceIndex].Removed then
    begin
      Result[targetIndex].Key := fItems[sourceIndex].Key;
      Result[targetIndex].Value := fItems[sourceIndex].Value;
      Inc(targetIndex);
    end;
end;

function TBidiDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

procedure TBidiDictionary<TKey, TValue>.AddOrSetKey(const value: TValue; const key: TKey);
var
  keyHashCode, keyBucketIndex, valueHashCode, valueBucketIndex, keyItemIndex, valueItemIndex: Integer;
  keyFound, valueFound: Boolean;
begin
  valueHashCode := ValueHash(value);
  valueFound := FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex);
  keyHashCode := KeyHash(key);
  keyFound := FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex);

  if keyFound then
  begin
    if valueFound and (keyItemIndex = valueItemIndex) then
      Exit; // this key/value pair are already mapped to each other
    raise Error.DuplicateKey;
  end
  else if valueFound then
    // value found, but key not found, this is a replace value operation
    DoSetKey(valueBucketIndex, valueItemIndex, keyHashCode, key)
  else
  begin
    // neither value nor key found, this is an add operation
    if fItemCount = Capacity then
    begin
      Grow;
      // rehash invalidates the indices
      FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex);
      FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex);
    end;
    Assert(keyItemIndex = valueItemIndex);
    DoAdd(keyhashCode, keyBucketIndex, valueHashCode, valueBucketIndex, keyItemIndex, key, value);
  end;
end;

procedure TBidiDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
begin
  SetItem(key, value);
end;

function TBidiDictionary<TKey, TValue>.AsReadOnly: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

function TBidiDictionary<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := TryGetValue(key, item) and fValueComparer.Equals(item, value);
end;

function TBidiDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := FindKey(key, KeyHash(key), bucketIndex, itemIndex);
end;

function TBidiDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := FindValue(value, ValueHash(value), bucketIndex, itemIndex);
end;

procedure TBidiDictionary<TKey, TValue>.EnsureCompact;
begin
  if fCount <> fItemCount then
    Rehash(Capacity);
end;

function TBidiDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  TryExtract(key, Result);
end;

function TBidiDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
  foundItem: PItem;
begin
  if FindKey(key, KeyHash(key), keyBucketIndex, keyItemIndex)
    and FindValue(value, ValueHash(value), valueBucketIndex, valueItemIndex)
    and (keyItemIndex = valueItemIndex) then
  begin
    foundItem := @fItems[keyItemIndex];
    Result.Key := foundItem.Key;
    Result.Value := foundItem.Value;
    DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caExtracted);
    Exit;
  end;
  Result := Default(TKeyValuePair);
end;

procedure TBidiDictionary<TKey, TValue>.TrimExcess;
begin
  SetCapacity(fCount);
end;

function TBidiDictionary<TKey, TValue>.TryAdd(const key: TKey;
  const value: TValue): Boolean;
var
  keyHashCode, keyBucketIndex, valueHashCode, valueBucketIndex, keyItemIndex, valueItemIndex: Integer;
begin
  keyHashCode := KeyHash(key);
  if FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex) then
    Exit(False);
  valueHashCode := ValueHash(value);
  if FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex) then
    Exit(False);
  if fItemCount = Capacity then
  begin
    Grow;
    // rehash invalidates the indices
    FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex);
    FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex);
  end;
  Assert(keyItemIndex = valueItemIndex);
  DoAdd(keyhashCode, keyBucketIndex, valueHashCode, valueBucketIndex, keyItemIndex, key, value);
  Result := True;
end;

function TBidiDictionary<TKey, TValue>.TryExtract(const key: TKey;
  out value: TValue): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := FindKey(key, KeyHash(key), keyBucketIndex, keyItemIndex);
  if Result then
  begin
    value := fItems[keyItemIndex].Value;
    FindValue(value, fItems[keyItemIndex].ValueHashCode, valueBucketIndex, valueItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caExtracted);
  end
  else
    value := Default(TValue);
end;

function TBidiDictionary<TKey, TValue>.TryGetElementAt(out item: TKeyValuePair;
  index: Integer): Boolean;
begin
  Result := Cardinal(index) < Cardinal(fCount);
  if Result then
  begin
    EnsureCompact;
    item.Key := fItems[index].Key;
    item.Value := fItems[index].Value;
  end;
end;

function TBidiDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := FindKey(key, KeyHash(key), bucketIndex, itemIndex);
  if Result then
    value := fItems[itemIndex].Value
  else
    value := Default(TValue);
end;

function TBidiDictionary<TKey, TValue>.TryUpdateValue(const key: TKey; const newValue: TValue; out oldValue: TValue): Boolean;
begin
  Result := TryGetValue(key, oldValue);
  if Result then
    SetItem(key, newValue);
end;

function TBidiDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := FindKey(key, KeyHash(key), keyBucketIndex, keyItemIndex);
  if Result then
  begin
    FindValue(fItems[keyItemIndex].Value, fItems[keyItemIndex].ValueHashCode, valueBucketIndex, valueItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caRemoved);
  end;
end;

function TBidiDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := FindKey(key, KeyHash(key), keyBucketIndex, keyItemIndex)
    and fValueComparer.Equals(fItems[keyItemIndex].Value, value);
  if Result then
  begin
    FindValue(value, fItems[keyItemIndex].ValueHashCode, valueBucketIndex, valueItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caRemoved);
  end;
end;

function TBidiDictionary<TKey, TValue>.GetInverse: IBidiDictionary<TValue, TKey>;
begin
  Result := fInverse;
end;

function TBidiDictionary<TKey, TValue>.GetIsEmpty: Boolean;
begin
  Result := fCount = 0;
end;

function TBidiDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TBidiDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
begin
  TryGetValue(key, Result);
end;

function TBidiDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not TryGetValue(key, Result) then
    Result := defaultValue;
end;

function TBidiDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TBidiDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
var
  keyBucketIndex, keyItemIndex: Integer;
begin
  if not FindKey(key, KeyHash(key), keyBucketIndex, keyItemIndex) then
    raise Error.KeyNotFound;
  Result := fItems[keyItemIndex].Value;
end;

procedure TBidiDictionary<TKey, TValue>.SetItem(const key: TKey; const value: TValue);
var
  keyHashCode, keyBucketIndex, keyItemIndex: Integer;
  valueHashCode, valueBucketIndex, valueItemIndex: Integer;
  keyFound, valueFound: Boolean;
begin
  keyHashCode := KeyHash(key);
  keyFound := FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex);
  valueHashCode := ValueHash(value);
  valueFound := FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex);

  if valueFound then
  begin
    if keyFound and (keyItemIndex = valueItemIndex) then
      Exit; // this key/value pair are already mapped to each other
    raise Error.DuplicateKey;
  end
  else if keyFound then
    // key found, but value not found, this is a replace value operation
    DoSetValue(keyBucketIndex, keyItemIndex, valueHashCode, value)
  else
  begin
    // neither key nor value found, this is an add operation
    if fItemCount = Capacity then
    begin
      Grow;
      // rehash invalidates the indices
      FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex);
      FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex);
    end;
    Assert(keyItemIndex = valueItemIndex);
    DoAdd(keyhashCode, keyBucketIndex, valueHashCode, valueBucketIndex, keyItemIndex, key, value);
  end;
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>.TInverse'}

constructor TBidiDictionary<TKey, TValue>.TInverse.Create(
  const source: TBidiDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.Add(const value: TValue;
  const key: TKey);
begin
  fSource.Add(key, value);
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.AddOrSetValue(
  const value: TValue; const key: TKey);
begin
  SetItem(value, key);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Add(const item: TValueKeyPair): Boolean;
begin
  Result := fSource.TryAdd(item.Value, item.Key);
end;

function TBidiDictionary<TKey, TValue>.TInverse.AsReadOnly: IReadOnlyDictionary<TValue, TKey>;
begin
  Result := Self;
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.Changed(
  const item: TValueKeyPair; action: TCollectionChangedAction);
begin
  if Assigned(OnChanged) and OnChanged.CanInvoke then
    OnChanged.Invoke(fSource, item, action);
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.Clear;
begin
  fSource.Clear;
end;

function TBidiDictionary<TKey, TValue>.TInverse.Contains(const value: TValue;
  const key: TKey): Boolean;
begin
  Result := fSource.Contains(key, value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Contains(
  const value: TValueKeyPair): Boolean;
begin
  Result := fSource.Contains(value.Value, value.Key);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Contains(
  const value: TValueKeyPair;
  const comparer: IEqualityComparer<TValueKeyPair>): Boolean;
var
  pair: TValueKeyPair;
begin
  pair.Key := value.Key;
  Result := TryGetValue(value.Key, pair.Value) and comparer.Equals(pair, value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.ContainsKey(
  const value: TValue): Boolean;
begin
  Result := fSource.ContainsValue(value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.ContainsValue(
  const key: TKey): Boolean;
begin
  Result := fSource.ContainsKey(key);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Extract(
  const item: TValueKeyPair): TValueKeyPair;
begin
  Result := Extract(item.Key, item.Value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Extract(
  const value: TValue): TKey;
begin
  TryExtract(value, Result);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Extract(const value: TValue;
  const key: TKey): TValueKeyPair;
var
  pair: TKeyValuePair;
begin
  pair := fSource.Extract(key, value);
  Result.Key := pair.Value;
  Result.Value := pair.Key;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetCapacity: Integer;
begin
  Result := fSource.Capacity;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetCount: Integer;
begin
  Result := fSource.fCount;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetEnumerator: IEnumerator<TValueKeyPair>;
begin
  Result := TEnumerator.Create(fSource);
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetInverse: IBidiDictionary<TKey, TValue>;
begin
  Result := fSource;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetIsEmpty: Boolean;
begin
  Result := fSource.fCount = 0;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetItem(
  const value: TValue): TKey;
var
  valueBucketIndex, valueItemIndex: Integer;
begin
  if not fSource.FindValue(value, fSource.ValueHash(value), valueBucketIndex, valueItemIndex) then
    raise Error.KeyNotFound;
  Result := fSource.fItems[valueItemIndex].Key;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetKeys: IReadOnlyCollection<TValue>;
begin
  Result := fSource.fValues;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetKeyType: PTypeInfo;
begin
  Result := fSource.ValueType;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetOnKeyChanged: ICollectionChangedEvent<TValue>;
begin
  Result := fSource.fOnValueChanged;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetOnValueChanged: ICollectionChangedEvent<TKey>;
begin
  Result := fSource.fOnKeyChanged;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetValueOrDefault(
  const value: TValue): TKey;
begin
  TryGetValue(value, Result);
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetValueOrDefault(
  const value: TValue; const defaultKey: TKey): TKey;
begin
  if not TryGetValue(value, Result) then
    Result := defaultKey;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetValues: IReadOnlyCollection<TKey>;
begin
  Result := fSource.fKeys;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetValueType: PTypeInfo;
begin
  Result := fSource.KeyType;
end;

function TBidiDictionary<TKey, TValue>.TInverse.Remove(
  const value: TValue): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := fSource.FindValue(value, fSource.ValueHash(value), valueBucketIndex, valueItemIndex);
  if Result then
  begin
    fSource.FindKey(fSource.fItems[valueItemIndex].Key,
      fSource.fItems[valueItemIndex].KeyHashCode, keyBucketIndex, keyItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    fSource.DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caRemoved);
  end;
end;

function TBidiDictionary<TKey, TValue>.TInverse.Remove(const value: TValue;
  const key: TKey): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := fSource.FindValue(value, fSource.ValueHash(value), valueBucketIndex, valueItemIndex)
    and fSource.fKeyComparer.Equals(fSource.fItems[valueItemIndex].Key, key);
  if Result then
  begin
    fSource.FindKey(key, fSource.fItems[valueItemIndex].KeyHashCode, keyBucketIndex, keyItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    fSource.DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caRemoved);
  end;
end;

function TBidiDictionary<TKey, TValue>.TInverse.Remove(
  const item: TValueKeyPair): Boolean;
begin
  Result := Remove(item.Key, item.Value);
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.SetCapacity(value: Integer);
begin
  fSource.SetCapacity(value);
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.SetItem(const value: TValue;
  const key: TKey);
begin
  fSource.AddOrSetKey(value, key);
end;

function TBidiDictionary<TKey, TValue>.TInverse.ToArray: TArray<TValueKeyPair>;
var
  sourceIndex, targetIndex: Integer;
begin
  SetLength(Result, fSource.fCount);
  targetIndex := 0;
  for sourceIndex := 0 to fSource.fItemCount - 1 do
    if not fSource.fItems[sourceIndex].Removed then
    begin
      Result[targetIndex].Key := fSource.fItems[sourceIndex].Value;
      Result[targetIndex].Value := fSource.fItems[sourceIndex].Key;
      Inc(targetIndex);
    end;
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.TrimExcess;
begin
  fSource.TrimExcess;
end;

function TBidiDictionary<TKey, TValue>.TInverse.TryAdd(const value: TValue;
  const key: TKey): Boolean;
begin
  Result := fSource.TryAdd(key, value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.TryExtract(const value: TValue;
  out key: TKey): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := fSource.FindValue(value, fSource.ValueHash(value), valueBucketIndex, valueItemIndex);
  if Result then
  begin
    key := fSource.fItems[valueItemIndex].Key;
    fSource.FindKey(key, fSource.fItems[valueItemIndex].KeyHashCode, keyBucketIndex, keyItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    fSource.DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caExtracted);
  end
  else
    key := Default(TKey);
end;

function TBidiDictionary<TKey, TValue>.TInverse.TryGetElementAt(
  out item: TValueKeyPair; index: Integer): Boolean;
var
  pair: TKeyValuePair;
begin
  Result := fSource.TryGetElementAt(pair, index);
  if Result then
  begin
   item.Key := pair.Value;
   item.Value := pair.Key;
 end;
end;

function TBidiDictionary<TKey, TValue>.TInverse.TryGetValue(const value: TValue;
  out key: TKey): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := fSource.FindValue(value, fSource.ValueHash(value), bucketIndex, itemIndex);
  if Result then
    key := fSource.fItems[itemIndex].Key
  else
    key := Default(TKey);
end;

function TBidiDictionary<TKey, TValue>.TInverse.TryUpdateValue(const value: TValue; const newKey: TKey; out oldKey: TKey): Boolean;
begin
  Result := TryGetValue(value, oldKey);
  if Result then
    SetItem(value, newKey);
end;

function TBidiDictionary<TKey, TValue>.TInverse._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TBidiDictionary<TKey, TValue>.TInverse._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>.TEnumerator' }

constructor TBidiDictionary<TKey, TValue>.TEnumerator.Create(
  const source: TBidiDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fItemIndex := -1;
  fVersion := fSource.fVersion;
end;

destructor TBidiDictionary<TKey, TValue>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited Destroy;
end;

function TBidiDictionary<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result.Key := fSource.fItems[fItemIndex].Key;
  Result.Value := fSource.fItems[fItemIndex].Value;
end;

function TBidiDictionary<TKey, TValue>.TEnumerator.GetCurrentInverse: TValueKeyPair;
begin
  Result.Key := fSource.fItems[fItemIndex].Value;
  Result.Value := fSource.fItems[fItemIndex].Key;
end;

function TBidiDictionary<TKey, TValue>.TEnumerator.GetCurrentKey: TKey;
begin
  Result := fSource.fItems[fItemIndex].Key;
end;

function TBidiDictionary<TKey, TValue>.TEnumerator.GetCurrentValue: TValue;
begin
  Result := fSource.fItems[fItemIndex].Value;
end;

function TBidiDictionary<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  Result := fSource.DoMoveNext(fItemIndex, fVersion);
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>.TKeyCollection'}

constructor TBidiDictionary<TKey, TValue>.TKeyCollection.Create(
  const source: TBidiDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.Contains(const value: TKey): Boolean;
begin
  Result := fSource.ContainsKey(value);
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := fSource.fCount;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TEnumerator.Create(fSource);
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.GetIsEmpty: Boolean;
begin
  Result := fSource.fCount = 0;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  sourceIndex, targetIndex: Integer;
begin
  SetLength(Result, fSource.fCount);
  targetIndex := 0;
  for sourceIndex := 0 to fSource.fItemCount - 1 do
    if not fSource.fItems[sourceIndex].Removed then
    begin
      Result[targetIndex] := fSource.fItems[sourceIndex].Key;
      Inc(targetIndex);
    end;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.TryGetElementAt(
  out key: TKey; index: Integer): Boolean;
begin
  Result := Cardinal(index) < Cardinal(fSource.fCount);
  if Result then
  begin
    fSource.EnsureCompact;
    key := fSource.fItems[index].Key;
  end;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>.TValueCollection'}

constructor TBidiDictionary<TKey, TValue>.TValueCollection.Create(
  const source: TBidiDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.Contains(const value: TValue): Boolean;
begin
  Result := fSource.ContainsValue(value);
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fSource.fCount;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TEnumerator.Create(fSource);
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.GetIsEmpty: Boolean;
begin
  Result := fSource.fCount = 0;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  sourceIndex, targetIndex: Integer;
begin
  SetLength(Result, fSource.fCount);
  targetIndex := 0;
  for sourceIndex := 0 to fSource.fItemCount - 1 do
    if not fSource.fItems[sourceIndex].Removed then
    begin
      Result[targetIndex] := fSource.fItems[sourceIndex].Value;
      Inc(targetIndex);
    end;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.TryGetElementAt(
  out value: TValue; index: Integer): Boolean;
begin
  Result := Cardinal(index) < Cardinal(fSource.fCount);
  if Result then
  begin
    fSource.EnsureCompact;
    value := fSource.fItems[index].Value;
  end;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TSortedDictionary<TKey, TValue>'}

constructor TSortedDictionary<TKey, TValue>.Create;
begin
  Create(nil, nil);
end;

constructor TSortedDictionary<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>);
begin
  inherited Create;

  fKeys := TKeyCollection.Create(Self);
  fValues := TValueCollection.Create(Self);

  fKeyComparer := keyComparer;
  if Assigned(keyComparer) then
    fKeyComparer := keyComparer
  else
    fKeyComparer := IComparer<TKey>(_LookupVtableInfo(giComparer, TypeInfo(TKey), SizeOf(TKey)));
  fComparer := TComparer.Create(nil);
  if Assigned(valueComparer) then
    fValueComparer := valueComparer
  else
    fValueComparer := IEqualityComparer<TValue>(_LookupVtableInfo(giEqualityComparer, TypeInfo(TValue), SizeOf(TValue)));
  fTree := TRedBlackTree<TKey,TValue>.Create(keyComparer);
end;

destructor TSortedDictionary<TKey, TValue>.Destroy;
begin
  Clear;
  fTree.Free;
  fKeys.Free;
  fValues.Free;
  inherited Destroy;
end;

procedure TSortedDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
begin
  SetItem(key, value);
end;

function TSortedDictionary<TKey, TValue>.AsReadOnly: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

procedure TSortedDictionary<TKey, TValue>.Clear;
var
  node: PNode;
begin
  IncUnchecked(fVersion);

  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    if Assigned(Notify) then
      DoNotify(node.Key, node.Value, caRemoved);
    KeyChanged(node.Key, caRemoved);
    ValueChanged(node.Value, caRemoved);
    node := node.Next;
  end;

  fTree.Clear;
end;

function TSortedDictionary<TKey, TValue>.Contains(const value: TKeyValuePair;
  const comparer: IEqualityComparer<TKeyValuePair>): Boolean;
var
  found: TValue;
begin
  Result := fTree.Find(value.Key, found)
    and comparer.Equals(value, TKeyValuePair.Create(value.Key, found));
end;

function TSortedDictionary<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  found: TValue;
begin
  Result := fTree.Find(key, found) and fValueComparer.Equals(value, found);
end;

function TSortedDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fTree.Exists(key);
end;

function TSortedDictionary<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  found: TKeyValuePair;
begin
  for found in fTree do
    if fValueComparer.Equals(value, found.Value) then
      Exit(True);
  Result := False;
end;

function TSortedDictionary<TKey, TValue>.DoMoveNext(var currentNode: PNode;
  var finished: Boolean; iteratorVersion: Integer): Boolean;
begin
  if iteratorVersion <> fVersion then
    raise Error.EnumFailedVersion;

  if (fTree.Count = 0) or finished then
    Exit(False);

  if not Assigned(currentNode) then
    currentNode := fTree.Root.LeftMost
  else
    currentNode := currentNode.Next;
  Result := Assigned(currentNode);
  finished := not Result;
end;

function TSortedDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  if Assigned(node) and fValueComparer.Equals(value, node.Value) then
  begin
    Result.Key := node.Key;
    Result.Value := node.Value;
    IncUnchecked(fVersion);
    fTree.DeleteNode(node);
    Changed(Result, caExtracted);
    KeyChanged(Result.Key, caExtracted);
    ValueChanged(Result.Value, caExtracted);
  end
  else
    Result := Default(TKeyValuePair);
end;

function TSortedDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  TryExtract(key, Result);
end;

function TSortedDictionary<TKey, TValue>.GetCapacity: Integer;
begin
  Result := fTree.Capacity;
end;

function TSortedDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  Result := TEnumerator.Create(Self);
end;

function TSortedDictionary<TKey, TValue>.GetIsEmpty: Boolean;
begin
  Result := fTree.Count = 0;
end;

function TSortedDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  if not TryGetValue(key, Result) then
    raise Error.KeyNotFound;
end;

function TSortedDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TSortedDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
begin
  if not fTree.Find(key, Result) then
    Result := Default(TValue);
end;

function TSortedDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not fTree.Find(key, Result) then
    Result := defaultValue;
end;

function TSortedDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TSortedDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node);
  if Result then
  begin
    IncUnchecked(fVersion);
    if Assigned(Notify) then
      DoNotify(node.Key, node.Value, caRemoved);
    KeyChanged(node.Key, caRemoved);
    ValueChanged(node.Value, caRemoved);
    fTree.DeleteNode(node);
  end;
end;

function TSortedDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node) and fValueComparer.Equals(value, node.Value);
  if Result then
  begin
    IncUnchecked(fVersion);
    if Assigned(Notify) then
      DoNotify(node.Key, node.Value, caRemoved);
    KeyChanged(node.Key, caRemoved);
    ValueChanged(node.Value, caRemoved);
    fTree.DeleteNode(node);
  end;
end;

procedure TSortedDictionary<TKey, TValue>.SetCapacity(value: Integer);
begin
  fTree.Capacity := value;
end;

procedure TSortedDictionary<TKey, TValue>.SetItem(const key: TKey; const value: TValue);
var
  node: PNode;
begin
  IncUnchecked(fVersion);
  node := fTree.FindNode(key);
  if Assigned(node) then
  begin
    if Assigned(Notify) then
      DoNotify(key, node.Value, caRemoved);
    ValueChanged(node.Value, caRemoved);
    node.Value := value;
    if Assigned(Notify) then
      DoNotify(key, value, caAdded);
    ValueChanged(value, caAdded);
  end
  else
  begin
    fTree.Add(key, value);
    if Assigned(Notify) then
      DoNotify(key, value, caAdded);
    KeyChanged(key, caAdded);
    ValueChanged(value, caAdded);
  end;
end;

function TSortedDictionary<TKey, TValue>.ToArray: TArray<TKeyValuePair>;
var
  node: PNode;
  target: PKeyValuePair;
begin
  SetLength(Result, fTree.Count);
  target := Pointer(Result);
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    target.Key := node.Key;
    target.Value :=  node.Value;
    node := node.Next;
    Inc(target);
  end;
end;

procedure TSortedDictionary<TKey, TValue>.TrimExcess;
begin
  fTree.TrimExcess;
end;

function TSortedDictionary<TKey, TValue>.TryAdd(const key: TKey;
  const value: TValue): Boolean;
begin
  if not fTree.Add(key, value) then
    Exit(False);
  IncUnchecked(fVersion);
  if Assigned(Notify) then
    DoNotify(key, value, caAdded);
  KeyChanged(key, caAdded);
  ValueChanged(value, caAdded);
  Result := True;
end;

function TSortedDictionary<TKey, TValue>.TryExtract(const key: TKey; out value: TValue): Boolean;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node);
  if Result then
  begin
    value := node.Value;
    IncUnchecked(fVersion);
    fTree.DeleteNode(node);
    if Assigned(Notify) then
      DoNotify(key, value, caExtracted);
    KeyChanged(key, caExtracted);
    ValueChanged(value, caExtracted);
  end
  else
    value := Default(TValue);
end;

function TSortedDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fTree.Find(key, value);
end;

function TSortedDictionary<TKey, TValue>.TryUpdateValue(const key: TKey;
  const newValue: TValue; out oldValue: TValue): Boolean;
var
  node: PNode;
begin
  if fTree.fCount = 0 then
  begin
    oldValue := Default(TValue);
    Exit(False);
  end;

  node := fTree.FindNode(key);
  Result := Assigned(node);
  if Result then
  begin
    oldValue := node.Value;

    IncUnchecked(fVersion);

    if Assigned(Notify) then
      DoNotify(key, oldValue, caRemoved);
    ValueChanged(oldValue, caRemoved);

    node.Value := newValue;

    if Assigned(Notify) then
      DoNotify(key, newValue, caAdded);
    ValueChanged(newValue, caAdded);
  end
  else
    oldValue := Default(TValue);
end;

{$ENDREGION}


{$REGION 'TSortedDictionary<TKey, TValue>.TEnumerator'}

constructor TSortedDictionary<TKey, TValue>.TEnumerator.Create(
  const source: TSortedDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TSortedDictionary<TKey, TValue>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited Destroy;
end;

function TSortedDictionary<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result.Key := fCurrentNode.Key;
  Result.Value := fCurrentNode.Value;
end;

function TSortedDictionary<TKey, TValue>.TEnumerator.GetCurrentKey: TKey;
begin
  Result := fCurrentNode.Key;
end;

function TSortedDictionary<TKey, TValue>.TEnumerator.GetCurrentValue: TValue;
begin
  Result := fCurrentNode.Value;
end;

function TSortedDictionary<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  Result := fSource.DoMoveNext(fCurrentNode, fFinished, fVersion);
end;

{$ENDREGION}


{$REGION 'TSortedDictionary<TKey, TValue>.TKeyCollection'}

constructor TSortedDictionary<TKey, TValue>.TKeyCollection.Create(
  const source: TSortedDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.Contains(
  const value: TKey): Boolean;
begin
  Result := fSource.fTree.Exists(value);
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := fSource.fTree.Count;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TEnumerator.Create(fSource);
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.GetIsEmpty: Boolean;
begin
  Result := fSource.fTree.Count = 0;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  i: Integer;
  node: PNode;
begin
  SetLength(Result, fSource.fTree.Count);
  i := 0;
  node := fSource.fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    Result[i] := node.Key;
    node := node.Next;
    Inc(i);
  end;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TSortedDictionary<TKey, TValue>.TValueCollection'}

constructor TSortedDictionary<TKey, TValue>.TValueCollection.Create(
  const source: TSortedDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.Contains(
  const value: TValue): Boolean;
begin
  Result := fSource.ContainsValue(value);
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fSource.fTree.Count;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TEnumerator.Create(fSource);
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.GetIsEmpty: Boolean;
begin
  Result := fSource.fTree.Count = 0;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  i: Integer;
  node: PNode;
begin
  SetLength(Result, fSource.fTree.Count);
  i := 0;
  node := fSource.fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    Result[i] := node.Value;
    node := node.Next;
    Inc(i);
  end;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TFoldedDictionary<TKey, TValue>'}

constructor TFoldedDictionary<TKey, TValue>.Create(keyType,
  valueType, elementType: PTypeInfo; capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  fElementType := elementType;
  fKeyType := keyType;
  fValueType := valueType;
  inherited Create(capacity, keyComparer, valueComparer, ownerships);
end;

function TFoldedDictionary<TKey, TValue>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TFoldedDictionary<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := fKeyType;
end;

function TFoldedDictionary<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := fValueType;
end;

{$ENDREGION}


{$REGION 'TFoldedBidiDictionary<TKey, TValue>'}

constructor TFoldedBidiDictionary<TKey, TValue>.Create(keyType, valueType,
  elementType: PTypeInfo; capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  fElementType := elementType;
  fKeyType := keyType;
  fValueType := valueType;
  inherited Create(capacity, keyComparer, valueComparer, ownerships);
end;

function TFoldedBidiDictionary<TKey, TValue>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TFoldedBidiDictionary<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := fKeyType;
end;

function TFoldedBidiDictionary<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := fValueType;
end;

{$ENDREGION}


end.
