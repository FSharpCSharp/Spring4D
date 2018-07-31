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
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Events;

type
  TDequeEnd = (deFront, deBack);

  /// <summary>
  ///   Represents a double-ended queue.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the collection.
  /// </typeparam>
  TDeque<T> = class(TEnumerableBase<T>, INotifyCollectionChanged<T>,
    IEnumerable<T>, {ICollection<T>, IReadOnlyCollection<T>, }IQueue<T>, IDeque<T>)
  private
  {$REGION 'Nested Types'}
    type
      TEnumerator = class(TInterfacedObject, IEnumerator<T>)
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
    fItems: TArray<T>;
    fCount: Integer;
    fVersion: Integer;
    fFront: Integer;
    fOnChanged: TCollectionChangedEventImpl<T>;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetOnChanged: ICollectionChangedEvent<T>;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
    function GetBack: Integer; inline;
    procedure Grow;
    property Back: Integer read GetBack;
    property Front: Integer read fFront;
  protected
    procedure Changed(const item: T; action: TCollectionChangedAction);
    procedure AddInternal(const item: T; dequeEnd: TDequeEnd);
    procedure RemoveInternal(var item: T; dequeEnd: TDequeEnd; notification: TCollectionChangedAction); virtual;
  public
    constructor Create; overload; override;
    constructor Create(const values: array of T); overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;

    function Single: T;
    function SingleOrDefault(const defaultValue: T): T;
    function TryGetFirst(out value: T): Boolean;
    function TryGetLast(out value: T): Boolean;
  {$ENDREGION}

  {$REGION 'Implements IDeque<T>'}
    procedure Clear;

    procedure AddFirst(const item: T);
    procedure AddLast(const item: T);

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
    procedure IQueue<T>.Enqueue = AddLast;
    function IQueue<T>.Dequeue = RemoveFirst;
    function IQueue<T>.Extract = ExtractFirst;
    function IQueue<T>.Peek = First;
    function IQueue<T>.PeekOrDefault = FirstOrDefault;
    function IQueue<T>.TryDequeue = TryRemoveFirst;
    function IQueue<T>.TryExtract = TryExtractFirst;
    function IQueue<T>.TryPeek = TryGetFirst;
  {$ENDREGION}
  end;

  TObjectDeque<T: class> = class(TDeque<T>, ICollectionOwnership)
  private
    fOwnsObjects: Boolean;
  {$REGION 'Property Accessors'}
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  protected
    procedure RemoveInternal(var item: T; dequeEnd: TDequeEnd; notification: TCollectionChangedAction); override;
  public
    constructor Create; override;
    constructor Create(ownsObjects: Boolean); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;
  end;

implementation

uses
  Classes,
  SysUtils,
  TypInfo,
  Spring,
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
    AddInternal(values[i], deBack);
end;

constructor TDeque<T>.Create(const collection: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
    AddInternal(item, deBack);
end;

destructor TDeque<T>.Destroy;
begin
  Clear;
  fOnChanged.Free;
  inherited Destroy;
end;

function TDeque<T>.GetBack: Integer;
begin
  Result := fFront + fCount - 1;
  if Result > High(fItems) then
    Dec(Result, Length(fItems));
end;

procedure TDeque<T>.AddInternal(const item: T; dequeEnd: TDequeEnd);
begin
  if fCount = Length(fItems) then
    Grow;

  IncUnchecked(fVersion);
  Inc(fCount);
  case dequeEnd of
    deFront:
    begin
      Dec(fFront);
      if fFront = -1 then
        fFront := High(fItems);
      fItems[Front] := item;
    end;
    deBack: fItems[Back] := item;
  end;

  Changed(item, caAdded);
end;

procedure TDeque<T>.AddFirst(const item: T);
begin
  AddInternal(item, deFront);
end;

procedure TDeque<T>.AddLast(const item: T);
begin
  AddInternal(item, deBack);
end;

procedure TDeque<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  if fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
end;

procedure TDeque<T>.Clear;
var
  item: T;
begin
  while fCount > 0 do
    RemoveInternal(item, deFront, caRemoved);
end;

function TDeque<T>.ExtractFirst: T;
begin
  RemoveInternal(Result, deFront, caExtracted);
end;

function TDeque<T>.ExtractLast: T;
begin
  RemoveInternal(Result, deBack, caExtracted);
end;

function TDeque<T>.GetCapacity: Integer;
begin
  Result := Length(fItems);
end;

function TDeque<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TDeque<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TDeque<T>.GetIsEmpty: Boolean;
begin
  Result := fCount = 0;
end;

function TDeque<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
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

procedure TDeque<T>.RemoveInternal(var item: T; dequeEnd: TDequeEnd; notification: TCollectionChangedAction);
begin
  if fCount = 0 then
    raise Error.NoElements;

  IncUnchecked(fVersion);
  case dequeEnd of
    deFront:
    begin
      item := fItems[Front];
      fItems[Front] := Default(T);
      Inc(fFront);
      if fFront = Length(fItems) then
        fFront := 0;
    end;
    deBack:
    begin
      item := fItems[Back];
      fItems[Back] := Default(T);
    end;
  end;
  Dec(fCount);

  Changed(item, notification);
end;

function TDeque<T>.RemoveFirst: T;
begin
  RemoveInternal(Result, deFront, caRemoved);
end;

function TDeque<T>.RemoveLast: T;
begin
  RemoveInternal(Result, deBack, caRemoved);
end;

procedure TDeque<T>.SetCapacity(value: Integer);
var
  wrapped: Boolean;
  offset, oldCapacity: Integer;
begin
  Guard.CheckRange(value >= fCount, 'capacity');

  offset := value - Length(fItems);
  if offset = 0 then
    Exit;

  if fCount = 0 then
  begin
    fFront := 0;
    SetLength(fItems, value);
    Exit;
  end;

  wrapped := Back < Front;
  oldCapacity := Length(fItems);
  if offset > 0 then
    SetLength(fItems, value);
  if wrapped then
  begin
    TArrayManager.Move(fItems, fItems, Front, Front + offset, oldCapacity - Front);
    if offset > 0 then
      TArrayManager.Finalize(fItems, Front, offset)
    else
      TArrayManager.Finalize(fItems, fCount, -offset);
    Inc(fFront, offset);
  end
  else if Front + fCount > value then
  begin
    TArrayManager.Move(fItems, fItems, Front, 0, fCount);
    TArrayManager.Finalize(fItems, fCount, Front);
    fFront := 0;
  end;
  if offset < 0 then
    SetLength(fItems, value);
end;

function TDeque<T>.Single: T;
begin
  case fCount of
    0: raise Error.NoElements;
    1: Result := fItems[Front];
  else
    raise Error.MoreThanOneElement;
  end;
end;

function TDeque<T>.SingleOrDefault(const defaultValue: T): T;
begin
  case fCount of
    0: Result := defaultValue;
    1: Result := fItems[Front];
  else
    raise Error.MoreThanOneElement;
  end;
end;

procedure TDeque<T>.TrimExcess;
begin
  SetCapacity(fCount);
end;

function TDeque<T>.TryExtractFirst(out item: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    RemoveInternal(item, deFront, caExtracted)
  else
    item := Default(T);
end;

function TDeque<T>.TryExtractLast(out item: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    RemoveInternal(item, deBack, caExtracted)
  else
    item := Default(T);
end;

function TDeque<T>.TryGetFirst(out value: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    value := fItems[Front]
  else
    value := Default(T);
end;

function TDeque<T>.TryGetLast(out value: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    value := fItems[Back]
  else
    value := Default(T);
end;

function TDeque<T>.TryRemoveFirst(out item: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    RemoveInternal(item, deFront, caRemoved)
  else
    item := Default(T);
end;

function TDeque<T>.TryRemoveLast(out item: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    RemoveInternal(item, deBack, caRemoved)
  else
    item := Default(T);
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

  Result := fIndex < fSource.fCount;
  if Result then
  begin
    fCurrent := fSource.fItems[(fSource.fFront + fIndex) mod Length(fSource.fItems)];
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
  fOwnsObjects := True;
end;

constructor TObjectDeque<T>.Create(ownsObjects: Boolean);
begin
  inherited Create;
  fOwnsObjects := ownsObjects;
end;

constructor TObjectDeque<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  fOwnsObjects := ownsObjects;
end;

procedure TObjectDeque<T>.RemoveInternal(var item: T; dequeEnd: TDequeEnd; notification: TCollectionChangedAction);
begin
  inherited RemoveInternal(item, dequeEnd, notification);
  if fOwnsObjects and (notification = caRemoved) then
  begin
{$IFNDEF AUTOREFCOUNT}
    item.Free;
{$ELSE}
    item.DisposeOf;
{$ENDIF}
    item := nil;
  end;
end;

function TObjectDeque<T>.GetOwnsObjects: Boolean;
begin
  Result := fOwnsObjects;
end;

procedure TObjectDeque<T>.SetOwnsObjects(const value: Boolean);
begin
  fOwnsObjects := value;
end;

{$ENDREGION}


end.
