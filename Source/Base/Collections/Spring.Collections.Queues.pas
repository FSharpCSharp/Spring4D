{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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

type
  TDequeEnd = (deFront, deBack);
  TDequeKind = (dkUnbounded, dkBounded, dkEvicting);

  /// <summary>
  ///   Represents a double-ended queue.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the collection.
  /// </typeparam>
  TDeque<T> = class(TEnumerableBase<T>, IEnumerable<T>, IQueue<T>, IDeque<T>)
  private
  {$REGION 'Nested Types'}
    type
      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TDeque<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
        function GetCurrent: T;
      public
        constructor Create(const deque: TDeque<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
      TArrayManager = TArrayManager<T>;
  {$ENDREGION}
  private
    fOnChanged: TCollectionChangedEventImpl<T>;
    fItems: TArray<T>;
    fCount: Integer;
    fVersion: Integer;
    fFront: Integer;
    fBack: Integer;
    fKind: TDequeKind;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer; inline;
    function GetIsEmpty: Boolean;
    function GetOnChanged: ICollectionChangedEvent<T>;
    function GetOwnsObjects: Boolean; inline;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
    procedure Grow;
    procedure DeleteFirst(action: TCollectionChangedAction);
    procedure DeleteLast(action: TCollectionChangedAction);
  protected
    procedure DoNotify(const item: T; action: TCollectionChangedAction); inline;
    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create; override;
    constructor Create(const values: array of T); overload;
    constructor Create(const values: IEnumerable<T>); overload;
    constructor Create(capacity: Integer; kind: TDequeKind); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;

    function First: T; overload;
    function FirstOrDefault: T; overload;
    function Single: T; overload;
    function SingleOrDefault(const defaultValue: T): T; overload;
    function TryGetFirst(out value: T): Boolean; overload;
    function TryGetLast(out value: T): Boolean; overload;
  {$ENDREGION}

  {$REGION 'Implements IDeque<T>'}
    procedure Clear;

    function AddFirst(const item: T): Boolean;
    function AddLast(const item: T): Boolean;

    function RemoveFirst: T;
    function RemoveLast: T;
    function ExtractFirst: T;
    function ExtractLast: T;

    function TryRemoveFirst(out item: T): Boolean;
    function TryRemoveLast(out item: T): Boolean;
    function TryExtractFirst(out item: T): Boolean;
    function TryExtractLast(out item: T): Boolean;

    procedure TrimExcess;
  {$ENDREGION}

  {$REGION 'Implements IQueue<T>'}
    function IQueue<T>.Enqueue = AddLast;
    function IQueue<T>.Dequeue = RemoveFirst;
    function IQueue<T>.Extract = ExtractFirst;
    function IQueue<T>.Peek = First;
    function IQueue<T>.PeekOrDefault = FirstOrDefault;
    function IQueue<T>.TryDequeue = TryRemoveFirst;
    function IQueue<T>.TryExtract = TryExtractFirst;
    function IQueue<T>.TryPeek = TryGetFirst;
  {$ENDREGION}
  end;

  TObjectDeque<T: class> = class(TDeque<T>)
  private
  {$REGION 'Property Accessors'}
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  public
    constructor Create; override;
    constructor Create(ownsObjects: Boolean); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;
    constructor Create(capacity: Integer; kind: TDequeKind; ownsObjects: Boolean = True); overload;
  end;

implementation

uses
  Classes,
  SysUtils,
  TypInfo,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TDeque<T>'}

constructor TDeque<T>.Create;
begin
  inherited Create;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

constructor TDeque<T>.Create(const values: array of T);
var
  i: Integer;
begin
  Create;
  SetCapacity(Length(values));
  for i := Low(values) to High(values) do
    AddLast(values[i]);
end;

constructor TDeque<T>.Create(const values: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in values do
    AddLast(item);
end;

constructor TDeque<T>.Create(capacity: Integer; kind: TDequeKind);
begin
  Create;
  SetCapacity(capacity);
  fKind := kind;
end;

destructor TDeque<T>.Destroy;
begin
  Clear;
  fOnChanged.Free;
  inherited Destroy;
end;

procedure TDeque<T>.DoNotify(const item: T; action: TCollectionChangedAction);
begin
  if fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
end;

procedure TDeque<T>.DeleteFirst(action: TCollectionChangedAction);
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);

  DoNotify(fItems[fFront], action);
  if OwnsObjects and (action = caRemoved) then
    FreeObject(fItems[fFront]);
  fItems[fFront] := Default(T);
  if fFront < DynArrayHigh(fItems) then
    Inc(fFront)
  else
    fFront := 0;
end;

procedure TDeque<T>.DeleteLast(action: TCollectionChangedAction);
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);

  DoNotify(fItems[fBack], action);
  if OwnsObjects and (action = caRemoved) then
    FreeObject(fItems[fBack]);
  fItems[fBack] := Default(T);
  if fBack > 0 then
    Dec(fBack)
  else
    fBack := DynArrayHigh(fItems);
end;

function TDeque<T>.GetCount: Integer;
begin
  Result := fCount and CountMask;
end;

function TDeque<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

function TDeque<T>.GetOwnsObjects: Boolean;
begin
  Result := {$IFDEF DELPHIXE7_UP}(GetTypeKind(T) = tkClass) and {$ENDIF}(fCount < 0);
end;

function TDeque<T>.AddFirst(const item: T): Boolean;
begin
  if Count = DynArrayLength(fItems) then
    case fKind of
      dkUnbounded: Grow;
      dkBounded: Exit(False);
      dkEvicting: DeleteLast(caRemoved);
    end;
  Result := True;

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);
  if fFront > 0 then
    Dec(fFront)
  else
    fFront := DynArrayHigh(fItems);
  fItems[fFront] := item;

  DoNotify(item, caAdded);
end;

function TDeque<T>.AddLast(const item: T): Boolean;
begin
  if Count = DynArrayLength(fItems) then
    case fKind of
      dkUnbounded: Grow;
      dkBounded: Exit(False);
      dkEvicting: DeleteFirst(caRemoved);
    end;
  Result := True;

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);
  if fBack < DynArrayHigh(fItems) then
    Inc(fBack)
  else
    fBack := 0;
  fItems[fBack] := item;

  DoNotify(item, caAdded);
end;

procedure TDeque<T>.Clear;
begin
  while Count > 0 do
    DeleteFirst(caRemoved);
end;

function TDeque<T>.ExtractFirst: T;
begin
  if Count > 0 then
  begin
    Result := fItems[fFront];
    DeleteFirst(caExtracted);
  end
  else
    raise Error.NoElements;
end;

function TDeque<T>.ExtractLast: T;
begin
  if Count > 0 then
  begin
    Result := fItems[fBack];
    DeleteLast(caExtracted);
  end
  else
    raise Error.NoElements;
end;

function TDeque<T>.First: T;
begin
  if Count > 0 then
    Result := fItems[fFront]
  else
    raise Error.NoElements;
end;

function TDeque<T>.FirstOrDefault: T;
begin
  if Count > 0 then
    Result := fItems[fFront]
  else
    Result := Default(T);
end;

function TDeque<T>.GetCapacity: Integer;
begin
  Result := Length(fItems);
end;

function TDeque<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TDeque<T>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TDeque<T>.Grow;
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

procedure TDeque<T>.SetCapacity(value: Integer);
var
  wrapped: Boolean;
  offset, oldCapacity, itemCount: Integer;
begin
  Guard.CheckRange(value >= Count, 'capacity');

  offset := value - Length(fItems);
  if offset = 0 then
    Exit;

  itemCount := Count;
  if itemCount = 0 then
  begin
    fFront := 0;
    fBack := value - 1;
    SetLength(fItems, value);
    Exit;
  end;

  wrapped := fBack < fFront;
  oldCapacity := Length(fItems);
  if offset > 0 then
    SetLength(fItems, value);
  if wrapped then
  begin
    TArrayManager.Move(fItems, fItems, fFront, fFront + offset, oldCapacity - fFront);
    if offset > 0 then
      TArrayManager.Finalize(fItems, fFront, offset)
    else
      TArrayManager.Finalize(fItems, itemCount, -offset);
    Inc(fFront, offset);
  end
  else
  begin
    if fFront + itemCount > value then
    begin
      TArrayManager.Move(fItems, fItems, fFront, 0, itemCount);
      TArrayManager.Finalize(fItems, itemCount, fFront);
      fFront := 0;
    end;
    fBack := itemCount - 1;
  end;
  if offset < 0 then
    SetLength(fItems, value);
end;

function TDeque<T>.Single: T;
begin
  case Count of
    0: raise Error.NoElements;
    1: Result := fItems[fFront];
  else
    raise Error.MoreThanOneElement;
  end;
end;

function TDeque<T>.SingleOrDefault(const defaultValue: T): T;
begin
  case Count of
    0: Result := defaultValue;
    1: Result := fItems[fFront];
  else
    raise Error.MoreThanOneElement;
  end;
end;

procedure TDeque<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TDeque<T>.TryExtractFirst(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    item := fItems[fFront];
    DeleteFirst(caExtracted);
  end
  else
    item := Default(T);
end;

function TDeque<T>.TryExtractLast(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    item := fItems[fBack];
    DeleteLast(caExtracted);
  end
  else
    item := Default(T);
end;

function TDeque<T>.TryGetFirst(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := fItems[fFront]
  else
    value := Default(T);
end;

function TDeque<T>.TryGetLast(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := fItems[fBack]
  else
    value := Default(T);
end;

function TDeque<T>.TryRemoveFirst(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    if OwnsObjects then
      item := Default(T)
    else
      item := fItems[fFront];
    DeleteFirst(caRemoved);
  end
  else
    item := Default(T);
end;

function TDeque<T>.TryRemoveLast(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    if OwnsObjects then
      item := Default(T)
    else
      item := fItems[fBack];
    DeleteLast(caRemoved);
  end
  else
    item := Default(T);
end;

function TDeque<T>.RemoveFirst: T;
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      Result := Default(T)
    else
      Result := fItems[fFront];
    DeleteFirst(caRemoved);
  end
  else
    raise Error.NoElements;
end;

function TDeque<T>.RemoveLast: T;
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      Result := Default(T)
    else
      Result := fItems[fBack];
    DeleteLast(caRemoved);
  end
  else
    raise Error.NoElements;
end;

{$ENDREGION}


{$REGION 'TDeque<T>.TEnumerator'}

constructor TDeque<T>.TEnumerator.Create(const deque: TDeque<T>);
begin
  inherited Create;
  fSource := deque;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TDeque<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited Destroy;
end;

function TDeque<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TDeque<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion <> fSource.fVersion then
    raise Error.EnumFailedVersion;

  Result := fIndex < fSource.Count;
  if Result then
  begin
    fCurrent := fSource.fItems[(fSource.fFront + fIndex) mod DynArrayLength(fSource.fItems)];
    Inc(fIndex);
  end
  else
    fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TObjectDeque<T>'}

constructor TObjectDeque<T>.Create;
begin
  inherited Create;
  SetOwnsObjects(True);
end;

constructor TObjectDeque<T>.Create(ownsObjects: Boolean);
begin
  inherited Create;
  SetOwnsObjects(ownsObjects);
end;

constructor TObjectDeque<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  SetOwnsObjects(ownsObjects);
end;

constructor TObjectDeque<T>.Create(capacity: Integer; kind: TDequeKind;
  ownsObjects: Boolean);
begin
  inherited Create(capacity, kind);
  SetOwnsObjects(ownsObjects);
end;

procedure TObjectDeque<T>.SetOwnsObjects(const value: Boolean);
begin
  fCount := (fCount and CountMask) or BitMask[value];
end;

{$ENDREGION}


end.
