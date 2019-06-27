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

unit Spring.Collections.Queues;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Events;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

type
  TAbstractQueue<T> = class abstract(TCircularArrayBuffer<T>)
  public
  {$REGION 'Implements IQueue<T>'}
    function Dequeue: T;
    function Extract: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryDequeue(out item: T): Boolean;
    function TryExtract(out item: T): Boolean;
    function TryPeek(out item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a first-in, first-out collection of items which grows
  ///   automatically.
  /// </summary>
  TQueue<T> = class(TAbstractQueue<T>, IInterface, IEnumerable<T>, IQueue<T>)
  private
    procedure Grow;
  public
    constructor Create(const values: array of T); overload;
    constructor Create(const values: IEnumerable<T>); overload;

  {$REGION 'Implements IQueue<T>'}
    function Enqueue(const item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a first-in, first-out collection of items which has a fixed
  ///   size and prevents adding items when full.
  /// </summary>
  TBoundedQueue<T> = class(TAbstractQueue<T>, IInterface, IEnumerable<T>, IQueue<T>)
  public
  {$REGION 'Implements IQueue<T>'}
    function Enqueue(const item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a first-in, first-out collection of items which has a fixed
  ///   size and removes the oldest item when full.
  /// </summary>
  TEvictingQueue<T> = class(TAbstractQueue<T>, IInterface, IEnumerable<T>, IQueue<T>)
  public
  {$REGION 'Implements IQueue<T>'}
    function Enqueue(const item: T): Boolean;
  {$ENDREGION}
  end;

  TAbstractDeque<T> = class(TCircularArrayBuffer<T>)
  public
  {$REGION 'Implements IDeque<T>'}
    function RemoveFirst: T;
    function RemoveLast: T;
    function ExtractFirst: T;
    function ExtractLast: T;

    function TryRemoveFirst(out item: T): Boolean;
    function TryRemoveLast(out item: T): Boolean;
    function TryExtractFirst(out item: T): Boolean;
    function TryExtractLast(out item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a double-ended queue of items which grows automatically.
  /// </summary>
  TDeque<T> = class(TAbstractDeque<T>, IInterface, IEnumerable<T>, IDeque<T>)
  private
    procedure Grow;
  public
    constructor Create(const values: array of T); overload;
    constructor Create(const values: IEnumerable<T>); overload;

  {$REGION 'Implements IDeque<T>'}
    function AddFirst(const item: T): Boolean;
    function AddLast(const item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a double-ended queue of items which has a fixed size and
  ///   prevents adding items when full.
  /// </summary>
  TBoundedDeque<T> = class(TAbstractDeque<T>, IInterface, IEnumerable<T>, IDeque<T>)
  public
  {$REGION 'Implements IDeque<T>'}
    function AddFirst(const item: T): Boolean;
    function AddLast(const item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a double-ended queue of items which has a fixed size and
  ///   and removes items from the opposite end when full.
  /// </summary>
  TEvictingDeque<T> = class(TAbstractDeque<T>, IInterface, IEnumerable<T>, IDeque<T>)
  public
  {$REGION 'Implements IDeque<T>'}
    function AddFirst(const item: T): Boolean;
    function AddLast(const item: T): Boolean;
  {$ENDREGION}
  end;

  TFoldedQueue<T> = class(TQueue<T>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const elementType: PTypeInfo;
      const comparer: IComparer<T>; ownsObjects: Boolean = False);
  end;

  /// <summary>
  ///   Represents an unbounded priority queue based on a priority heap.
  /// </summary>
  TPriorityQueue<T> = class(TEnumerableBase<T>, IEnumerable<T>, IQueue<T>)
  private
  {$REGION 'Nested Types'}
    type
      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TPriorityQueue<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
        function GetCurrent: T;
      public
        constructor Create(const source: TPriorityQueue<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    fItems: TArray<T>;
    fCount: Integer;
    fVersion: Integer;
    fOnChanged: TCollectionChangedEventImpl<T>;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetIsEmpty: Boolean; inline;
    function GetOnChanged: ICollectionChangedEvent<T>;
    function GetOwnsObjects: Boolean; inline;
    procedure SetCapacity(value: Integer);
    procedure SetOwnsObjects(value: Boolean);
  {$ENDREGION}
    procedure Grow;
    procedure SiftDown(index: Integer);
    procedure SiftUp(index: Integer);
  protected
    procedure DequeueInternal(action: TCollectionChangedAction); inline;
    procedure DoNotify(const item: T; action: TCollectionChangedAction); inline;
    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create; override;
    constructor Create(ownsObjects: Boolean); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IQueue<T>'}
    function Enqueue(const item: T): Boolean;
    function Dequeue: T;
    function Extract: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryDequeue(out item: T): Boolean;
    function TryExtract(out item: T): Boolean;
    function TryPeek(out item: T): Boolean;
    procedure TrimExcess;
  {$ENDREGION}
  end;

implementation

uses
  Classes,
  SysUtils,
  TypInfo,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TAbstractQueue<T>'}

function TAbstractQueue<T>.Dequeue: T;
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      Result := Default(T)
    else
      Result := Items[Head];
    DeleteFromHead(caRemoved);
  end
  else
    raise Error.NoElements;
end;

function TAbstractQueue<T>.Extract: T;
begin
  if Count > 0 then
  begin
    Result := Items[Head];
    DeleteFromHead(caExtracted);
  end
  else
    raise Error.NoElements;
end;

function TAbstractQueue<T>.Peek: T;
begin
  if Count > 0 then
    Result := Items[Head]
  else
    raise Error.NoElements;
end;

function TAbstractQueue<T>.PeekOrDefault: T;
begin
  if Count > 0 then
    Result := Items[Head]
  else
    Result := Default(T);
end;

function TAbstractQueue<T>.TryDequeue(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    if OwnsObjects then
      item := Default(T)
    else
      item := Items[Head];
    DeleteFromHead(caRemoved);
  end
  else
    item := Default(T);
end;

function TAbstractQueue<T>.TryExtract(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    item := Items[Head];
    DeleteFromHead(caExtracted);
  end
  else
    item := Default(T);
end;

function TAbstractQueue<T>.TryPeek(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    item := Items[Head]
  else
    item := Default(T);
end;

{$ENDREGION}


{$REGION 'TQueue<T>'}

constructor TQueue<T>.Create(const values: array of T);
var
  i: Integer;
begin
  inherited Create;
  SetCapacity(Length(values));
  for i := 0 to High(values) do
    AddToTail(values[i]);
end;

constructor TQueue<T>.Create(const values: IEnumerable<T>);
var
  value: T;
begin
  inherited Create;
  for value in values do
    Enqueue(value);
end;

function TQueue<T>.Enqueue(const item: T): Boolean;
begin
  if Count = Capacity then
    Grow;
  AddToTail(item);
  Result := True;
end;

procedure TQueue<T>.Grow;
begin
  SetCapacity(GrowCapacity(Capacity));
end;

{$ENDREGION}


{$REGION 'TBoundedQueue<T>'}

function TBoundedQueue<T>.Enqueue(const item: T): Boolean;
begin
  if Count <> Capacity then
  begin
    AddToTail(item);
    Result := True;
  end
  else
    Result := False;
end;

{$ENDREGION}


{$REGION 'TEvictingQueue<T>'}

function TEvictingQueue<T>.Enqueue(const item: T): Boolean;
begin
  if Count = Capacity then
    DeleteFromHead(caRemoved);
  AddToTail(item);
  Result := True;
end;

{$ENDREGION}


{$REGION 'TAbstractDeque<T>'}

function TAbstractDeque<T>.RemoveFirst: T;
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      Result := Default(T)
    else
      Result := Items[Head];
    DeleteFromHead(caRemoved);
  end
  else
    raise Error.NoElements;
end;

function TAbstractDeque<T>.RemoveLast: T;
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      Result := Default(T)
    else
      Result := Tail^;
    DeleteFromTail(caRemoved);
  end
  else
    raise Error.NoElements;
end;

function TAbstractDeque<T>.ExtractFirst: T;
begin
  if Count > 0 then
  begin
    Result := Items[Head];
    DeleteFromHead(caExtracted);
  end
  else
    raise Error.NoElements;
end;

function TAbstractDeque<T>.ExtractLast: T;
begin
  if Count > 0 then
  begin
    Result := Tail^;
    DeleteFromTail(caExtracted);
  end
  else
    raise Error.NoElements;
end;

function TAbstractDeque<T>.TryRemoveFirst(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    if OwnsObjects then
      item := Default(T)
    else
      item := Items[Head];
    DeleteFromHead(caRemoved);
  end
  else
    item := Default(T);
end;

function TAbstractDeque<T>.TryRemoveLast(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    if OwnsObjects then
      item := Default(T)
    else
      item := Tail^;
    DeleteFromTail(caRemoved);
  end
  else
    item := Default(T);
end;

function TAbstractDeque<T>.TryExtractFirst(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    item := Items[Head];
    DeleteFromHead(caExtracted);
  end
  else
    item := Default(T);
end;

function TAbstractDeque<T>.TryExtractLast(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    item := Tail^;
    DeleteFromTail(caExtracted);
  end
  else
    item := Default(T);
end;

{$ENDREGION}


{$REGION 'TDeque<T>'}

constructor TDeque<T>.Create(const values: array of T);
var
  i: Integer;
begin
  Create;
  SetCapacity(Length(values));
  for i := Low(values) to High(values) do
    AddToTail(values[i]);
end;

constructor TDeque<T>.Create(const values: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in values do
    AddLast(item);
end;

function TDeque<T>.AddFirst(const item: T): Boolean;
begin
  if Count = Capacity then
    Grow;
  AddToHead(item);
  Result := True;
end;

function TDeque<T>.AddLast(const item: T): Boolean;
begin
  if Count = Capacity then
    Grow;
  AddToTail(item);
  Result := True;
end;

procedure TDeque<T>.Grow;
begin
  SetCapacity(GrowCapacity(Capacity));
end;

{$ENDREGION}


{$REGION 'TBoundedDeque<T>'}

function TBoundedDeque<T>.AddFirst(const item: T): Boolean;
begin
  if Count = Capacity then
    Exit(False);
  AddToHead(item);
  Result := True;
end;

function TBoundedDeque<T>.AddLast(const item: T): Boolean;
begin
  if Count = Capacity then
    Exit(False);
  AddToTail(item);
  Result := True;
end;

{$ENDREGION}


{$REGION 'TEvictingDeque<T>'}

function TEvictingDeque<T>.AddFirst(const item: T): Boolean;
begin
  if Count = Capacity then
    DeleteFromTail(caRemoved);
  AddToHead(item);
  Result := True;
end;

function TEvictingDeque<T>.AddLast(const item: T): Boolean;
begin
  if Count = Capacity then
    DeleteFromHead(caRemoved);
  AddToTail(item);
  Result := True;
end;

{$ENDREGION}


{$REGION 'TFoldedQueue<T>'}

constructor TFoldedQueue<T>.Create(const elementType: PTypeInfo;
  const comparer: IComparer<T>; ownsObjects: Boolean);
begin
  inherited Create(comparer);
  fElementType := elementType;
  SetOwnsObjects(ownsObjects);
end;

function TFoldedQueue<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


{$REGION 'TPriorityQueue<T>'}

constructor TPriorityQueue<T>.Create;
begin
  inherited Create;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

constructor TPriorityQueue<T>.Create(ownsObjects: Boolean);
begin
  Create;
  SetOwnsObjects(ownsObjects);
end;

constructor TPriorityQueue<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  SetOwnsObjects(ownsObjects);
end;

destructor TPriorityQueue<T>.Destroy;
begin
  Clear;
  fOnChanged.Free;
  inherited Destroy;
end;

procedure TPriorityQueue<T>.DoNotify(const item: T;
  action: TCollectionChangedAction);
begin
  if fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
end;

function TPriorityQueue<T>.GetCapacity: Integer;
begin
  Result := DynArrayLength(fItems);
end;

function TPriorityQueue<T>.GetCount: Integer;
begin
  Result := fCount and CountMask;
end;

function TPriorityQueue<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TPriorityQueue<T>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TPriorityQueue<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

function TPriorityQueue<T>.GetOwnsObjects: Boolean;
begin
  Result := {$IFDEF DELPHIXE7_UP}(GetTypeKind(T) = tkClass) and {$ENDIF}(fCount < 0);
end;

procedure TPriorityQueue<T>.SetCapacity(value: Integer);
begin
  Guard.CheckRange(value >= Count, 'capacity');
  SetLength(fItems, value);
end;

procedure TPriorityQueue<T>.SetOwnsObjects(value: Boolean);
begin
  fCount := (fCount and CountMask) or BitMask[value];
end;

procedure TPriorityQueue<T>.Clear;
var
  i: Integer;
begin
  if fOnChanged.CanInvoke then
    for i := 0 to Count - 1 do
      fOnChanged.Invoke(Self, fItems[i], caRemoved);
  if fOnChanged.CanInvoke then
  begin
    for i := 0 to Count - 1 do
      FreeObject(fItems[i]);
    fCount := not CountMask;
  end
  else
    fCount := 0;
  SetLength(fItems, 0);
end;

procedure TPriorityQueue<T>.DequeueInternal(action: TCollectionChangedAction);
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);
  DoNotify(fItems[0], action);
  if OwnsObjects and (action = caRemoved) then
    FreeObject(fItems[0]);
  fItems[0] := fItems[Count];
  fItems[Count] := Default(T);
  SiftDown(0);
end;

procedure TPriorityQueue<T>.SiftDown(index: Integer);
var
  child, size: Integer;
  temp: T;
begin
  size := Count;
  if index >= size then
    Exit;

  temp := fItems[index];
  while True do
  begin
    child := index * 2 + 1;
    if child >= size then
      Break;

    if (child + 1 < size) and (Comparer.Compare(fItems[child], fItems[child + 1]) > 0) then
      Inc(child);
    if Comparer.Compare(temp, fItems[child]) <= 0 then
      Break;
    fItems[index] := fItems[child];
    index := child;
  end;
  fItems[index] := temp;
end;

procedure TPriorityQueue<T>.SiftUp(index: Integer);
var
  parent: Integer;
  temp: T;
begin
  if index < 1 then
    Exit;

  temp := fItems[index];
  while index > 0 do
  begin
    parent := (index - 1) div 2;
    if Comparer.Compare(temp, fItems[parent]) >= 0 then
      Break;
    fItems[index] := fItems[parent];
    index := parent;
  end;
  fItems[index] := temp;
end;

procedure TPriorityQueue<T>.Grow;
var
  newCapacity: Integer;
begin
  newCapacity := Length(fItems) * 2;
  if newCapacity = 0 then
    newCapacity := 4
  else if newCapacity < 0 then
    OutOfMemoryError;
  SetCapacity(newCapacity);
end;

function TPriorityQueue<T>.Enqueue(const item: T): Boolean;
var
  index: Integer;
begin
  index := Count;
  if index = Capacity then
    Grow;

  IncUnchecked(fVersion);
  Inc(fCount);
  fItems[index] := item;
  SiftUp(index);

  DoNotify(item, caAdded);
  Result := True;
end;

function TPriorityQueue<T>.Dequeue: T;
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      Result := Default(T)
    else
      Result := fItems[0];
    DequeueInternal(caRemoved);
  end
  else
    raise Error.NoElements;
end;

function TPriorityQueue<T>.Extract: T;
begin
  if Count > 0 then
  begin
    Result := fItems[0];
    DequeueInternal(caExtracted);
  end
  else
    raise Error.NoElements;
end;

function TPriorityQueue<T>.Peek: T;
begin
  if Count > 0 then
    Result := fItems[0]
  else
    raise Error.NoElements;
end;

function TPriorityQueue<T>.PeekOrDefault: T;
begin
  if Count > 0 then
    Result := fItems[0]
  else
    Result := Default(T);
end;

procedure TPriorityQueue<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TPriorityQueue<T>.TryDequeue(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    if OwnsObjects then
      item := Default(T)
    else
      item := fItems[0];
    DequeueInternal(caRemoved);
  end
  else
    item := Default(T);
end;

function TPriorityQueue<T>.TryExtract(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    item := fItems[0];
    DequeueInternal(caExtracted);
  end
  else
    item := Default(T);
end;

function TPriorityQueue<T>.TryPeek(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    item := fItems[0]
  else
    item := Default(T);
end;

{$ENDREGION}


{$REGION 'TPriorityQueue<T>.TEnumerator'}

constructor TPriorityQueue<T>.TEnumerator.Create(
  const source: TPriorityQueue<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TPriorityQueue<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited Destroy;
end;

function TPriorityQueue<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TPriorityQueue<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion <> fSource.fVersion then
    raise Error.EnumFailedVersion;

  if fIndex < fSource.Count then
  begin
    fCurrent := fSource.fItems[fIndex];
    Inc(fIndex);
    Result := True;
  end
  else
  begin
    fCurrent := Default(T);
    Result := False;
  end;
end;

{$ENDREGION}


end.
