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

unit Spring.Collections.Adapters;

interface

uses
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Collections.Adapters.Interfaces,
  Spring.Collections.Base;

type
  TBaseAdapter<T> = class(TRefCountedObject, IEnumerable)
  private type
    TEnumerator = class(TRefCountedObject, IInterface, IEnumerator)
    private
      fEnumerator: IEnumerator<T>;
      function GetCurrent: TValue;
    public
      constructor Create(const enumerator: IEnumerator<T>);
      function MoveNext: Boolean;
    end;
  private
    fSource: IEnumerable<T>;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetElementType: PTypeInfo;
    function GetIsEmpty: Boolean;
  {$ENDREGION}
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: IEnumerable<T>);
    function GetEnumerator: IEnumerator;
  end;

  TCollectionAdapter<T> = class(TBaseAdapter<T>, ICollection)
  private
    function GetIsReadOnly: Boolean;
    function GetOnChanged: IEvent;

    procedure Add(const item: TValue);
    procedure AddRange(const values: array of TValue); overload;
    procedure AddRange(const values: IEnumerable); overload;

    function Remove(const item: TValue): Boolean;
    procedure RemoveRange(const values: array of TValue); overload;
    procedure RemoveRange(const values: IEnumerable); overload;

    function Extract(const item: TValue): TValue; overload;
    procedure ExtractRange(const values: array of TValue); overload;
    procedure ExtractRange(const values: IEnumerable); overload;

    procedure Clear;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: ICollection<T>);
  end;

  TListAdapter<T> = class(TCollectionAdapter<T>, IReadOnlyList, IList)
  private
    function GetCapacity: Integer;
    function GetItem(index: Integer): TValue;
    procedure SetCapacity(value: Integer);
    procedure SetItem(index: Integer; const item: TValue);

    function Add(const item: TValue): Integer;

    procedure Insert(index: Integer; const item: TValue);
    procedure InsertRange(index: Integer; const values: array of TValue); overload;
    procedure InsertRange(index: Integer; const values: IEnumerable); overload;

    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer);

    procedure Reverse; overload;
    procedure Reverse(index, count: Integer); overload;

    procedure Sort;

    function IndexOf(const item: TValue): Integer; overload;
    function IndexOf(const item: TValue; index: Integer): Integer; overload;
    function IndexOf(const item: TValue; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: TValue): Integer; overload;
    function LastIndexOf(const item: TValue; index: Integer): Integer; overload;
    function LastIndexOf(const item: TValue; index, count: Integer): Integer; overload;

    function AsReadOnly: IReadOnlyList;
    procedure TrimExcess;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: IList<T>);
  end;

  TDictionaryAdapter<TKey, T> = class(TCollectionAdapter<TPair<TKey, T>>,
    IReadOnlyDictionary, IDictionary)
  private
    function GetCapacity: Integer;
    function GetItem(const key: TValue): TValue;
    function GetKeyType: PTypeInfo;
    function GetOnKeyChanged: IEvent;
    function GetOnValueChanged: IEvent;
    function GetValueType: PTypeInfo;
    procedure SetCapacity(value: Integer);
    procedure SetItem(const key: TValue; const value: TValue);

    procedure Add(const key, value: TValue);

    function Remove(const key: TValue): Boolean;

    function ContainsKey(const key: TValue): Boolean;
    function ContainsValue(const value: TValue): Boolean;

    function TryExtract(const key: TValue; out value: TValue): Boolean;
    function TryGetValue(const key: TValue; out value: TValue): Boolean;

    function AsReadOnly: IReadOnlyDictionary;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: IDictionary<TKey, T>);
  end;

  TStackAdapter<T> = class(TBaseAdapter<T>, IStack)
  private
    function GetOnChanged: IEvent;

    procedure Clear;
    procedure Push(const item: TValue);
    function Pop: TValue;
    function Peek: TValue;
    function PeekOrDefault: TValue;
    function TryPeek(out item: TValue): Boolean;
    function TryPop(out item: TValue): Boolean;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: IStack<T>);
  end;

  TQueueAdapter<T> = class(TBaseAdapter<T>, IQueue)
  private
    function GetOnChanged: IEvent;

    procedure Clear;
    procedure Enqueue(const item: TValue);
    function Dequeue: TValue;
    function Peek: TValue;
    function PeekOrDefault: TValue;
    function TryDequeue(out item: TValue): Boolean;
    function TryPeek(out item: TValue): Boolean;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: IQueue<T>);
  end;

  THashSetAdapter<T> = class(TCollectionAdapter<T>, ISet)
  private
    function Add(const item: TValue): Boolean;
    procedure ExceptWith(const other: IEnumerable);
    procedure IntersectWith(const other: IEnumerable);
    procedure UnionWith(const other: IEnumerable);
    function IsSubsetOf(const other: IEnumerable): Boolean;
    function IsSupersetOf(const other: IEnumerable): Boolean;
    function Overlaps(const other: IEnumerable): Boolean;
    function SetEquals(const other: IEnumerable): Boolean;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: ISet<T>);
  end;

  TEnumeratorAdapter<T> = class(TRefCountedObject, IInterface, IEnumerator<T>)
  private type
    TRTLEnumerator = Generics.Collections.TEnumerator<T>;
  private
    fSource: TRTLEnumerator;
    fOwnsObject: Boolean;
    function GetCurrent: T;
  public
    constructor Create(const source: TRTLEnumerator; ownsObject: Boolean = True);
    destructor Destroy; override;
    function MoveNext: Boolean;
  end;

  TEnumerableAdapter<T> = class(TEnumerableBase<T>, IInterface, IEnumerable<T>)
  private type
    TRTLEnumerable = Generics.Collections.TEnumerable<T>;
  private
    fSource: TRTLEnumerable;
    fOwnsObject: Boolean;
  public
    constructor Create(const source: TRTLEnumerable; ownsObject: Boolean = True);
    destructor Destroy; override;
    function GetEnumerator: IEnumerator<T>;
  end;

  TAdapters = class
  public
    class function CreateCollection<T>(const source: ICollection<T>): ICollection;
    class function CreateList<T>(const source: IList<T>): IList;
    class function CreateDictionary<TKey, TValue>(const source: IDictionary<TKey, TValue>): IDictionary;
    class function CreateStack<T>(const source: IStack<T>): IStack;
    class function CreateQueue<T>(const source: IQueue<T>): IQueue;
    class function CreateSet<T>(const source: ISet<T>): ISet;

    class function From<T>(const source: TEnumerable<T>): IEnumerable<T>; overload; static;
  end;

implementation

uses
  SysUtils;


{$REGION 'TBaseAdapter<T>'}

constructor TBaseAdapter<T>.Create(const source: IEnumerable<T>);
begin
  inherited Create;
  fSource := source;
end;

function TBaseAdapter<T>.GetCount: Integer;
begin
  Result := fSource.Count;
end;

function TBaseAdapter<T>.GetElementType: PTypeInfo;
begin
  Result := fSource.ElementType;
end;

function TBaseAdapter<T>.GetEnumerator: IEnumerator;
begin
  Result := TEnumerator.Create(fSource.GetEnumerator);
end;

function TBaseAdapter<T>.GetIsEmpty: Boolean;
begin
  Result := fSource.IsEmpty;
end;

function TBaseAdapter<T>.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IID = IEnumerable<T> then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited QueryInterface(IID, Obj);
end;

{$ENDREGION}


{$REGION 'TBaseAdapter<T>.TEnumerator'}

constructor TBaseAdapter<T>.TEnumerator.Create(
  const enumerator: IEnumerator<T>);
begin
  inherited Create;
  fEnumerator := enumerator;
end;

function TBaseAdapter<T>.TEnumerator.GetCurrent: TValue;
var
  current: T;
begin
  current := fEnumerator.Current;
  Result := TValue.From(@current, TypeInfo(T));
end;

function TBaseAdapter<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TCollectionAdapter<T>'}

constructor TCollectionAdapter<T>.Create(const source: ICollection<T>);
begin
  inherited Create(source);
end;

procedure TCollectionAdapter<T>.Add(const item: TValue);
begin
  ICollection<T>(fSource).Add(item.AsType<T>);
end;

procedure TCollectionAdapter<T>.AddRange(const values: array of TValue);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    ICollection<T>(fSource).Add(values[i].AsType<T>);
end;

procedure TCollectionAdapter<T>.AddRange(const values: IEnumerable);
var
  item: TValue;
begin
  for item in values do
    ICollection<T>(fSource).Add(item.AsType<T>);
end;

procedure TCollectionAdapter<T>.Clear;
begin
  ICollection<T>(fSource).Clear;
end;

function TCollectionAdapter<T>.Extract(const item: TValue): TValue;
begin
  Result := TValue.From<T>(ICollection<T>(fSource).Extract(item.AsType<T>));
end;

procedure TCollectionAdapter<T>.ExtractRange(const values: array of TValue);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    ICollection<T>(fSource).Extract(values[i].AsType<T>);
end;

procedure TCollectionAdapter<T>.ExtractRange(const values: IEnumerable);
var
  item: TValue;
begin
  for item in values do
    ICollection<T>(fSource).Extract(item.AsType<T>);
end;

function TCollectionAdapter<T>.GetIsReadOnly: Boolean;
begin
  Result := ICollection<T>(fSource).IsReadOnly;
end;

function TCollectionAdapter<T>.GetOnChanged: IEvent;
begin
  Result := ICollection<T>(fSource).OnChanged;
end;

function TCollectionAdapter<T>.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IID = ICollection<T> then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited QueryInterface(IID, Obj);
end;

function TCollectionAdapter<T>.Remove(const item: TValue): Boolean;
begin
  Result := ICollection<T>(fSource).Remove(item.AsType<T>);
end;

procedure TCollectionAdapter<T>.RemoveRange(const values: array of TValue);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    ICollection<T>(fSource).Remove(values[i].AsType<T>);
end;

procedure TCollectionAdapter<T>.RemoveRange(
  const values: IEnumerable);
var
  item: TValue;
begin
  for item in values do
    ICollection<T>(fSource).Remove(item.AsType<T>);
end;

{$ENDREGION}


{$REGION 'TListAdapter<T>'}

constructor TListAdapter<T>.Create(const source: IList<T>);
begin
  inherited Create(source);
end;

function TListAdapter<T>.Add(const item: TValue): Integer;
begin
  Result := IList<T>(fSource).Add(item.AsType<T>);
end;

function TListAdapter<T>.AsReadOnly: IReadOnlyList;
begin
  Result := Self;
end;

procedure TListAdapter<T>.Delete(index: Integer);
begin
  IList<T>(fSource).Delete(index);
end;

procedure TListAdapter<T>.DeleteRange(index, count: Integer);
begin
  IList<T>(fSource).DeleteRange(index, count);
end;

procedure TListAdapter<T>.Exchange(index1, index2: Integer);
begin
  IList<T>(fSource).Exchange(index1, index2);
end;

function TListAdapter<T>.GetCapacity: Integer;
begin
  Result := IList<T>(fSource).Capacity;
end;

function TListAdapter<T>.GetItem(index: Integer): TValue;
begin
  Result := TValue.From<T>(IList<T>(fSource)[index]);
end;

function TListAdapter<T>.IndexOf(const item: TValue): Integer;
begin
  Result := IList<T>(fSource).IndexOf(item.AsType<T>);
end;

function TListAdapter<T>.IndexOf(const item: TValue; index: Integer): Integer;
begin
  Result := IList<T>(fSource).IndexOf(item.AsType<T>, index);
end;

function TListAdapter<T>.IndexOf(const item: TValue; index,
  count: Integer): Integer;
begin
  Result := IList<T>(fSource).IndexOf(item.AsType<T>, index, count);
end;

procedure TListAdapter<T>.Insert(index: Integer; const item: TValue);
begin
  IList<T>(fSource).Insert(index, item.AsType<T>);
end;

procedure TListAdapter<T>.InsertRange(index: Integer;
  const values: array of TValue);
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fSource.Count), 'index');
{$ENDIF}

  for i := Low(values) to High(values) do
  begin
    IList<T>(fSource).Insert(index, values[i].AsType<T>);
    Inc(index);
  end;
end;

procedure TListAdapter<T>.InsertRange(index: Integer;
  const values: IEnumerable);
var
  item: TValue;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fSource.Count), 'index');
{$ENDIF}

  for item in values do
  begin
    IList<T>(fSource).Insert(index, item.AsType<T>);
    Inc(index);
  end;
end;

function TListAdapter<T>.LastIndexOf(const item: TValue): Integer;
begin
  Result := IList<T>(fSource).LastIndexOf(item.AsType<T>);
end;

function TListAdapter<T>.LastIndexOf(const item: TValue; index: Integer): Integer;
begin
  Result := IList<T>(fSource).LastIndexOf(item.AsType<T>, index);
end;

function TListAdapter<T>.LastIndexOf(const item: TValue; index,
  count: Integer): Integer;
begin
  Result := IList<T>(fSource).LastIndexOf(item.AsType<T>, index, count);
end;

procedure TListAdapter<T>.Move(currentIndex, newIndex: Integer);
begin
  IList<T>(fSource).Move(currentIndex, newIndex);
end;

function TListAdapter<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IID = IList<T> then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited QueryInterface(IID, Obj);
end;

procedure TListAdapter<T>.Reverse;
begin
  IList<T>(fSource).Reverse;
end;

procedure TListAdapter<T>.Reverse(index, count: Integer);
begin
  IList<T>(fSource).Reverse(index, count);
end;

procedure TListAdapter<T>.SetCapacity(value: Integer);
begin
  IList<T>(fSource).Capacity := value;
end;

procedure TListAdapter<T>.SetItem(index: Integer; const item: TValue);
begin
  IList<T>(fSource)[index] := item.AsType<T>;
end;

procedure TListAdapter<T>.Sort;
begin
  IList<T>(fSource).Sort;
end;

procedure TListAdapter<T>.TrimExcess;
begin
  IList<T>(fSource).TrimExcess;
end;

{$ENDREGION}


{$REGION 'TDictionaryAdapter<TKey, T>'}

constructor TDictionaryAdapter<TKey, T>.Create(
  const source: IDictionary<TKey, T>);
begin
  inherited Create(source);
end;

procedure TDictionaryAdapter<TKey, T>.Add(const key, value: TValue);
begin
  IDictionary<TKey, T>(fSource).Add(key.AsType<TKey>, value.AsType<T>);
end;

function TDictionaryAdapter<TKey, T>.AsReadOnly: IReadOnlyDictionary;
begin
  Result := Self;
end;

function TDictionaryAdapter<TKey, T>.ContainsKey(const key: TValue): Boolean;
begin
  Result := IDictionary<TKey, T>(fSource).ContainsKey(key.AsType<TKey>);
end;

function TDictionaryAdapter<TKey, T>.ContainsValue(
  const value: TValue): Boolean;
begin
  Result := IDictionary<TKey, T>(fSource).ContainsValue(value.AsType<T>);
end;

function TDictionaryAdapter<TKey, T>.GetCapacity: Integer;
begin
  Result := IDictionary<TKey, T>(fSource).Capacity;
end;

function TDictionaryAdapter<TKey, T>.GetItem(const key: TValue): TValue;
var
  item: T;
begin
  item := IDictionary<TKey, T>(fSource).GetItem(key.AsType<TKey>);
  Result := TValue.From<T>(item);
end;

function TDictionaryAdapter<TKey, T>.GetKeyType: PTypeInfo;
begin
  Result := IDictionary<TKey, T>(fSource).KeyType;
end;

function TDictionaryAdapter<TKey, T>.GetOnKeyChanged: IEvent;
begin
  Result := IDictionary<TKey, T>(fSource).OnKeyChanged;
end;

function TDictionaryAdapter<TKey, T>.GetOnValueChanged: IEvent;
begin
  Result := IDictionary<TKey, T>(fSource).OnValueChanged;
end;

function TDictionaryAdapter<TKey, T>.GetValueType: PTypeInfo;
begin
  Result := IDictionary<TKey, T>(fSource).ValueType;
end;

function TDictionaryAdapter<TKey, T>.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IID = IDictionary<TKey, T> then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited QueryInterface(IID, Obj);
end;

function TDictionaryAdapter<TKey, T>.Remove(const key: TValue): Boolean;
begin
  Result := IDictionary<TKey, T>(fSource).Remove(key.AsType<TKey>);
end;

procedure TDictionaryAdapter<TKey, T>.SetCapacity(value: Integer);
begin
  IDictionary<TKey, T>(fSource).Capacity := value;
end;

procedure TDictionaryAdapter<TKey, T>.SetItem(const key, value: TValue);
begin
  IDictionary<TKey, T>(fSource)[key.AsType<TKey>] := value.AsType<T>;
end;

function TDictionaryAdapter<TKey, T>.TryExtract(const key: TValue; out value: TValue): Boolean;
var
  item: T;
begin
  Result := IDictionary<TKey, T>(fSource).TryExtract(key.AsType<TKey>, item);
  value := TValue.From<T>(item);
end;

function TDictionaryAdapter<TKey, T>.TryGetValue(const key: TValue;
  out value: TValue): Boolean;
var
  item: T;
begin
  Result := IDictionary<TKey, T>(fSource).TryGetValue(key.AsType<TKey>, item);
  if Result then
    value := TValue.From<T>(item);
end;

{$ENDREGION}


{$REGION 'TStackAdapter<T>'}

constructor TStackAdapter<T>.Create(const source: IStack<T>);
begin
  inherited Create(source);
end;

procedure TStackAdapter<T>.Clear;
begin
  IStack<T>(fSource).Clear;
end;

function TStackAdapter<T>.GetOnChanged: IEvent;
begin
  Result := IStack<T>(fSource).OnChanged;
end;

function TStackAdapter<T>.Peek: TValue;
begin
  Result := TValue.From<T>(IStack<T>(fSource).Peek);
end;

function TStackAdapter<T>.PeekOrDefault: TValue;
begin
  Result := TValue.From<T>(IStack<T>(fSource).PeekOrDefault);
end;

function TStackAdapter<T>.Pop: TValue;
begin
  Result := TValue.From<T>(IStack<T>(fSource).Pop);
end;

procedure TStackAdapter<T>.Push(const item: TValue);
begin
  IStack<T>(fSource).Push(item.AsType<T>);
end;

function TStackAdapter<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IID = IStack<T> then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited QueryInterface(IID, Obj);
end;

function TStackAdapter<T>.TryPeek(out item: TValue): Boolean;
var
  value: T;
begin
  Result := IStack<T>(fSource).TryPeek(value);
  if Result then
    item := TValue.From<T>(value)
  else
    item := TValue.Empty;
end;

function TStackAdapter<T>.TryPop(out item: TValue): Boolean;
var
  value: T;
begin
  Result := IStack<T>(fSource).TryPop(value);
  if Result then
    item := TValue.From<T>(value)
  else
    item := TValue.Empty;
end;

{$ENDREGION}


{$REGION 'TQueueAdapter<T>'}

constructor TQueueAdapter<T>.Create(const source: IQueue<T>);
begin
  inherited Create(source);
end;

procedure TQueueAdapter<T>.Clear;
begin
  IQueue<T>(fSource).Clear;
end;

function TQueueAdapter<T>.Dequeue: TValue;
begin
  Result := TValue.From<T>(IQueue<T>(fSource).Dequeue);
end;

procedure TQueueAdapter<T>.Enqueue(const item: TValue);
begin
  IQueue<T>(fSource).Enqueue(item.AsType<T>);
end;

function TQueueAdapter<T>.GetOnChanged: IEvent;
begin
  Result := IQueue<T>(fSource).OnChanged;
end;

function TQueueAdapter<T>.Peek: TValue;
begin
  Result := TValue.From<T>(IQueue<T>(fSource).Peek);
end;

function TQueueAdapter<T>.PeekOrDefault: TValue;
begin
  Result := TValue.From<T>(IQueue<T>(fSource).PeekOrDefault);
end;

function TQueueAdapter<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IID = IQueue<T> then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited QueryInterface(IID, Obj);
end;

function TQueueAdapter<T>.TryDequeue(out item: TValue): Boolean;
var
  value: T;
begin
  Result := IQueue<T>(fSource).TryDequeue(value);
  if Result then
    item := TValue.From<T>(value)
  else
    item := TValue.Empty;
end;

function TQueueAdapter<T>.TryPeek(out item: TValue): Boolean;
var
  value: T;
begin
  Result := IQueue<T>(fSource).TryPeek(value);
  if Result then
    item := TValue.From<T>(value)
  else
    item := TValue.Empty;
end;

{$ENDREGION}


{$REGION 'THashSetAdapter<T>'}

constructor THashSetAdapter<T>.Create(const source: ISet<T>);
begin
  inherited Create(source);
end;

function THashSetAdapter<T>.Add(const item: TValue): Boolean;
begin
  Result := ISet<T>(fSource).Add(item.AsType<T>);
end;

procedure THashSetAdapter<T>.ExceptWith(const other: IEnumerable);
begin
  ISet<T>(fSource).ExceptWith(other as IEnumerable<T>);
end;

procedure THashSetAdapter<T>.IntersectWith(const other: IEnumerable);
begin
  ISet<T>(fSource).IntersectWith(other as IEnumerable<T>);
end;

function THashSetAdapter<T>.IsSubsetOf(const other: IEnumerable): Boolean;
begin
  Result := ISet<T>(fSource).IsSubsetOf(other as IEnumerable<T>);
end;

function THashSetAdapter<T>.IsSupersetOf(const other: IEnumerable): Boolean;
begin
  Result := ISet<T>(fSource).IsSupersetOf(other as IEnumerable<T>);
end;

function THashSetAdapter<T>.Overlaps(const other: IEnumerable): Boolean;
begin
  Result := ISet<T>(fSource).Overlaps(other as IEnumerable<T>);
end;

function THashSetAdapter<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IID = ISet<T> then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited QueryInterface(IID, Obj);
end;

function THashSetAdapter<T>.SetEquals(const other: IEnumerable): Boolean;
begin
  Result := ISet<T>(fSource).SetEquals(other as IEnumerable<T>);
end;

procedure THashSetAdapter<T>.UnionWith(const other: IEnumerable);
begin
  ISet<T>(fSource).UnionWith(other as IEnumerable<T>);
end;

{$ENDREGION}

{$REGION 'TEnumeratorAdapter<T>'}

constructor TEnumeratorAdapter<T>.Create(const source: TRTLEnumerator;
  ownsObject: Boolean);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
  fOwnsObject := ownsObject;
end;

destructor TEnumeratorAdapter<T>.Destroy;
begin
  if fOwnsObject then
    fSource.Free;
  inherited Destroy;
end;

function TEnumeratorAdapter<T>.GetCurrent: T;
begin
  Result := fSource.Current;
end;

function TEnumeratorAdapter<T>.MoveNext: Boolean;
begin
  Result := fSource.MoveNext;
end;

{$ENDREGION}


{$REGION 'TEnumerableAdapter<T>'}

constructor TEnumerableAdapter<T>.Create(const source: TRTLEnumerable;
  ownsObject: Boolean);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
  fOwnsObject := ownsObject;
end;

destructor TEnumerableAdapter<T>.Destroy;
begin
  if fOwnsObject then
    fSource.Free;
  inherited Destroy;
end;

function TEnumerableAdapter<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fSource.GetEnumerator);
end;

{$ENDREGION}


{$REGION 'TAdapters'}

class function TAdapters.CreateCollection<T>(
  const source: ICollection<T>): ICollection;
begin
  Result := TCollectionAdapter<T>.Create(source);
end;

class function TAdapters.CreateDictionary<TKey, TValue>(
  const source: IDictionary<TKey, TValue>): IDictionary;
begin
  Result := TDictionaryAdapter<TKey, TValue>.Create(source);
end;

class function TAdapters.CreateList<T>(const source: IList<T>): IList;
begin
  Result := TListAdapter<T>.Create(source);
end;

class function TAdapters.CreateQueue<T>(const source: IQueue<T>): IQueue;
begin
  Result := TQueueAdapter<T>.Create(source);
end;

class function TAdapters.CreateSet<T>(const source: ISet<T>): ISet;
begin
  Result := THashSetAdapter<T>.Create(source);
end;

class function TAdapters.CreateStack<T>(const source: IStack<T>): IStack;
begin
  Result := TStackAdapter<T>.Create(source);
end;

class function TAdapters.From<T>(const source: TEnumerable<T>): IEnumerable<T>;
begin
  Result := TEnumerableAdapter<T>.Create(source);
end;

{$ENDREGION}


end.
