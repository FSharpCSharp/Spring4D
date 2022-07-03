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

unit Spring.Collections.Adapters;

interface

uses
  Generics.Defaults,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type
  TCollectionAdapters = class
  private type
    TListAdapter<T> = class(TCollectionBase<T>, IInterface , IEnumerable<T>,
      IReadOnlyCollection<T>, IReadOnlyList<T>, ICollection<T>, IList<T>)
    private
      {$REGION 'Nested Types'}
      type
        PEnumerator = ^TEnumerator;
        TEnumerator = record
          Vtable: Pointer;
          RefCount: Integer;
          TypeInfo: PTypeInfo;
          fSource: TList<T>;
          fIndex: Integer;
          function GetCurrent: T;
          function MoveNext: Boolean;
          class var Enumerator_Vtable: TEnumeratorVtable;
        end;
    private
      fSource: TList<T>;
      procedure HandleNotify(sender: TObject; const item: T; action: TCollectionNotification);
    protected
    {$REGION 'Property Accessors'}
      function GetCapacity: Integer; inline;
      function GetCount: Integer; inline;
      function GetCountFast: Integer;
      function GetItem(index: Integer): T;
      function GetOwnsObjects: Boolean; inline;
      procedure SetCapacity(value: Integer);
      procedure SetCount(value: Integer);
      procedure SetItem(index: Integer; const value: T);
      procedure SetOwnsObjects(value: Boolean); inline;
    {$ENDREGION}

      //property Capacity: Integer read GetCapacity write SetCapacity;
      property Count: Integer read GetCount;
      //property Items[index: Integer]: T read GetItem write SetItem; default;
      //property OwnsObjects: Boolean read GetOwnsObjects;
    public
      constructor Create(const source: TList<T>; connectChangeEvent: Boolean);

    {$REGION 'Implements IEnumerable<T>'}
      function GetEnumerator: IEnumerator<T>;

      function Contains(const value: T): Boolean; overload;
      function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload;

      function Single: T; overload;
      function SingleOrDefault(const defaultValue: T): T; overload;

      function TryGetElementAt(var value: T; index: Integer): Boolean;
      function TryGetFirst(var value: T): Boolean; overload;
      function TryGetLast(var value: T): Boolean; overload;
      function TryGetSingle(var value: T): Boolean; overload;

      function ToArray: TArray<T>;
    {$ENDREGION}

    {$REGION 'Implements ICollection<T>'}
      procedure AddRange(const values: array of T); overload;
      procedure AddRange(const values: IEnumerable<T>); overload;

      function Remove(const item: T): Boolean;
      function RemoveAll(const match: Predicate<T>): Integer;

      function Extract(const item: T): T;
      function ExtractAll(const match: Predicate<T>): TArray<T>;

      procedure Clear;

      function CopyTo(var values: TArray<T>; index: Integer): Integer;
      function MoveTo(const collection: ICollection<T>): Integer; overload;
      function MoveTo(const collection: ICollection<T>; const predicate: Predicate<T>): Integer; overload;
    {$ENDREGION}

    {$REGION 'Implements IList<T>'}
      function Add(const item: T): Integer; overload;

      procedure Insert(index: Integer; const item: T);
      procedure InsertRange(index: Integer; const values: array of T); overload;
      procedure InsertRange(index: Integer; const values: IEnumerable<T>); overload;

      procedure Delete(index: Integer);
      procedure DeleteRange(index, count: Integer);

      function ExtractAt(index: Integer): T;
      function ExtractRange(index, count: Integer): TArray<T>; overload;

      function GetRange(index, count: Integer): IList<T>;

      procedure Exchange(index1, index2: Integer);
      procedure Move(currentIndex, newIndex: Integer);

      procedure Reverse; overload;
      procedure Reverse(index, count: Integer); overload;

      procedure Sort; overload;
      procedure Sort(const comparer: IComparer<T>); overload;
      procedure Sort(const comparer: TComparison<T>); overload;
      procedure Sort(const comparer: TComparison<T>; index, count: Integer); overload;
      procedure Sort(const comparer: IComparer<T>; index, count: Integer); overload;

      function IndexOf(const item: T): Integer; overload;
      function IndexOf(const item: T; index: Integer): Integer; overload;
      function IndexOf(const item: T; index, count: Integer): Integer; overload;

      function LastIndexOf(const item: T): Integer; overload;
      function LastIndexOf(const item: T; index: Integer): Integer; overload;
      function LastIndexOf(const item: T; index, count: Integer): Integer; overload;

      procedure TrimExcess;

      function AsReadOnly: IReadOnlyList<T>;
    {$ENDREGION}
    end;
  public
    class function CreateList<T>(const source: TList<T>; connectChangeEvent: Boolean): IList<T>;
  end;

implementation

uses
  Spring.Events.Base;


{$REGION 'TCollectionAdapters'}

class function TCollectionAdapters.CreateList<T>(const source: TList<T>;
  connectChangeEvent: Boolean): IList<T>;
begin // TODO: apply generic folding like in Spring.Collections
  Result := TListAdapter<T>.Create(source, connectChangeEvent);
end;

{$ENDREGION}


{$REGION 'TCollectionAdapters.TListAdapter<T>'}

constructor TCollectionAdapters.TListAdapter<T>.Create(const source: TList<T>;
  connectChangeEvent: Boolean);
begin
  fSource := source;
  fSource.OnNotify := HandleNotify;
end;

function TCollectionAdapters.TListAdapter<T>.AsReadOnly: IReadOnlyList<T>;
begin
  Result := Self;
end;

function TCollectionAdapters.TListAdapter<T>.GetCapacity: Integer;
begin
  Result := fSource.Capacity;
end;

function TCollectionAdapters.TListAdapter<T>.GetCount: Integer;
begin
  Result := fSource.Count;
end;

function TCollectionAdapters.TListAdapter<T>.GetCountFast: Integer;
begin
  Result := fSource.Count;
end;

function TCollectionAdapters.TListAdapter<T>.GetItem(index: Integer): T;
begin
  Result := fSource.Items[index];
end;

function TCollectionAdapters.TListAdapter<T>.GetOwnsObjects: Boolean;
begin
  // TODO: check inheritance from TObjectList<T> -
  // not possible simply with InheritsFrom(TObjectList<T>) because of class constraint
  RaiseHelper.NotSupported;
end;

procedure TCollectionAdapters.TListAdapter<T>.SetCapacity(value: Integer);
begin
  fSource.Capacity := value;
end;

procedure TCollectionAdapters.TListAdapter<T>.SetCount(value: Integer);
begin
  fSource.Count := value;
end;

procedure TCollectionAdapters.TListAdapter<T>.SetItem(index: Integer; const value: T);
begin
  fSource.Items[index] := value;
end;

procedure TCollectionAdapters.TListAdapter<T>.SetOwnsObjects(value: Boolean);
begin
  // TODO: check inheritance from TObjectList<T> -
  // not possible simply with InheritsFrom(TObjectList<T>) because of class constraint
  RaiseHelper.NotSupported;
end;

procedure TCollectionAdapters.TListAdapter<T>.HandleNotify(sender: TObject; const item: T;
  action: TCollectionNotification);
const
  actions: array[TCollectionNotification] of TCollectionChangedAction = (
    caAdded, caAdded, caExtracted, caExtracted, caRemoved, caRemoved);
begin
  with OnChanged do if CanInvoke then
    Invoke(sender, item, actions[action]);
end;

function TCollectionAdapters.TListAdapter<T>.Add(const item: T): Integer;
begin
  Result := fSource.Add(item);
end;

procedure TCollectionAdapters.TListAdapter<T>.AddRange(const values: array of T);
begin
  fSource.AddRange(values);
end;

procedure TCollectionAdapters.TListAdapter<T>.AddRange(const values: IEnumerable<T>);
var
  item: T;
begin
  for item in values do
    fSource.Add(item);
end;

procedure TCollectionAdapters.TListAdapter<T>.Clear;
begin
  fSource.Clear;
end;

function TCollectionAdapters.TListAdapter<T>.Contains(const value: T): Boolean;
begin
  Result := fSource.Contains(value);
end;

function TCollectionAdapters.TListAdapter<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  i: Integer;
begin
  for i := 0 to fSource.Count - 1 do
    if comparer.Equals(value, fSource.List[i]) then
      Exit(True);
  Result := False;
end;

function TCollectionAdapters.TListAdapter<T>.CopyTo(var values: TArray<T>; index: Integer): Integer;
begin
  RaiseHelper.NotSupported;
end;

procedure TCollectionAdapters.TListAdapter<T>.Delete(index: Integer);
begin
  fSource.Delete(index);
end;

procedure TCollectionAdapters.TListAdapter<T>.DeleteRange(index, count: Integer);
begin
  fSource.DeleteRange(index, count);
end;

procedure TCollectionAdapters.TListAdapter<T>.Exchange(index1, index2: Integer);
begin
  fSource.Exchange(index1, index2);
end;

function TCollectionAdapters.TListAdapter<T>.Extract(const item: T): T;
begin
  fSource.Extract(item);
end;

function TCollectionAdapters.TListAdapter<T>.ExtractAll(const match: Predicate<T>): TArray<T>;
var
  i: Integer;
begin
  i := 0;
  while i < fSource.Count do
    if match(fSource.List[i]) then
      Result := Result + [fSource.ExtractAt(i)]
    else
      Inc(i);
end;

function TCollectionAdapters.TListAdapter<T>.ExtractAt(index: Integer): T;
begin
  Result := fSource.ExtractAt(index);
end;

function TCollectionAdapters.TListAdapter<T>.ExtractRange(index, count: Integer): TArray<T>;
begin
  while count > 0 do
  begin
    Result := Result + [fSource.ExtractAt(index)];
    Dec(count);
  end;
end;

function TCollectionAdapters.TListAdapter<T>.GetRange(index, count: Integer): IList<T>;
var
  i: Integer;
begin
  // TODO: possibly implement this in an optimized way
  Result := TCollections.CreateList<T>;
  for i := 0 to fSource.Count - 1 do
    Result.Add(fSource.List[0]);
end;

function TCollectionAdapters.TListAdapter<T>.IndexOf(const item: T): Integer;
begin
  Result := fSource.IndexOf(item);
end;

function TCollectionAdapters.TListAdapter<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := IndexOf(item, index, Count - index);
end;

function TCollectionAdapters.TListAdapter<T>.IndexOf(const item: T; index, count: Integer): Integer;
begin
  // TODO: implement this in a faster way just like TAbstractArrayList in Spring.Collections.Lists
  // also currently this does not consider the list comparer because this uses an IEqComparer
  TArray.IndexOf<T>(fSource.List, item, index, count);
end;

procedure TCollectionAdapters.TListAdapter<T>.Insert(index: Integer; const item: T);
begin
  fSource.Insert(index, item);
end;

procedure TCollectionAdapters.TListAdapter<T>.InsertRange(index: Integer; const values: array of T);
begin
  fSource.InsertRange(index, values);
end;

procedure TCollectionAdapters.TListAdapter<T>.InsertRange(index: Integer; const values: IEnumerable<T>);
var
  item: T;
begin
  for item in values do
    fSource.Insert(index, item);
end;

function TCollectionAdapters.TListAdapter<T>.MoveTo(const collection: ICollection<T>): Integer;
begin
  // TODO: check for optimized way to do this
  while fSource.Count > 0 do
    collection.Add(fSource.ExtractAt(0));
end;

procedure TCollectionAdapters.TListAdapter<T>.Move(currentIndex, newIndex: Integer);
begin
  fSource.Move(currentIndex, newIndex);
end;

function TCollectionAdapters.TListAdapter<T>.MoveTo(const collection: ICollection<T>;
  const predicate: Predicate<T>): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while i < fSource.Count do
    if predicate(fSource.List[i]) then
    begin
      collection.Add(fSource.ExtractAt(i));
      Inc(Result);
    end
    else
      Inc(i);
end;

function TCollectionAdapters.TListAdapter<T>.LastIndexOf(const item: T): Integer;
begin
  Result := fSource.LastIndexOf(item);
end;

function TCollectionAdapters.TListAdapter<T>.LastIndexOf(const item: T; index: Integer): Integer;
begin
  Result := LastIndexOf(item, index, index + 1);
end;

function TCollectionAdapters.TListAdapter<T>.LastIndexOf(const item: T; index, count: Integer): Integer;
begin
  // TODO: implement this in a faster way just like TAbstractArrayList in Spring.Collections.Lists
  // also currently this does not consider the list comparer because this uses an IEqComparer
  Result := TArray.LastIndexOf<T>(fSource.List, item, count, index);
end;

function TCollectionAdapters.TListAdapter<T>.Remove(const item: T): Boolean;
begin
  Result := fSource.Remove(item) >= 0;
end;

function TCollectionAdapters.TListAdapter<T>.RemoveAll(const match: Predicate<T>): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while i < fSource.Count do
    if match(fSource.List[i]) then
    begin
      fSource.Delete(i);
      Inc(Result);
    end
    else
      Inc(i);
end;

procedure TCollectionAdapters.TListAdapter<T>.Reverse;
begin
  fSource.Reverse;
end;

procedure TCollectionAdapters.TListAdapter<T>.Reverse(index, count: Integer);
begin
  RaiseHelper.NotSupported;
end;

function TCollectionAdapters.TListAdapter<T>.GetEnumerator: IEnumerator<T>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
    fSource := Self.fSource;
end;

function TCollectionAdapters.TListAdapter<T>.Single: T;
begin
  case Count of
    0: RaiseHelper.NoElements;
    1: Result := fSource.List[0];
  else
    RaiseHelper.MoreThanOneElement;
  end;
end;

function TCollectionAdapters.TListAdapter<T>.SingleOrDefault(const defaultValue: T): T;
begin
  case Count of
    0: Result := defaultValue;
    1: Result := fSource.List[0];
  else
    RaiseHelper.MoreThanOneElement;
  end;
end;

procedure TCollectionAdapters.TListAdapter<T>.Sort;
begin
  Sort(fComparer, 0, Count);
end;

procedure TCollectionAdapters.TListAdapter<T>.Sort(const comparer: IComparer<T>);
begin
  Sort(comparer, 0, Count);
end;

procedure TCollectionAdapters.TListAdapter<T>.Sort(const comparer: TComparison<T>);
begin
  Sort(IComparer<T>(PPointer(@comparer)^), 0, Count);
end;

procedure TCollectionAdapters.TListAdapter<T>.Sort(const comparer: IComparer<T>; index, count: Integer);
begin
  // TODO: consider doing this ourselves - it will be faster ;)
  fSource.Sort(comparer, index, count);
end;

procedure TCollectionAdapters.TListAdapter<T>.Sort(const comparer: TComparison<T>; index, count: Integer);
begin
  Sort(IComparer<T>(PPointer(@comparer)^), index, count);
end;

function TCollectionAdapters.TListAdapter<T>.ToArray: TArray<T>;
begin
  Result := fSource.ToArray;
end;

procedure TCollectionAdapters.TListAdapter<T>.TrimExcess;
begin
  fSource.TrimExcess;
end;

function TCollectionAdapters.TListAdapter<T>.TryGetElementAt(var value: T; index: Integer): Boolean;
begin
  if Cardinal(index) < Cardinal(Count) then
  begin
    value := fSource.List[index];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TCollectionAdapters.TListAdapter<T>.TryGetFirst(var value: T): Boolean;
begin
  if Count > 0 then
  begin
    value := fSource.List[0];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TCollectionAdapters.TListAdapter<T>.TryGetLast(var value: T): Boolean;
var
  index: Integer;
begin
  index := Count - 1;
  if index >= 0 then
  begin
    value := fSource.List[index];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TCollectionAdapters.TListAdapter<T>.TryGetSingle(var value: T): Boolean;
begin
  if Count = 1 then
  begin
    value := fSource.List[0];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

{$ENDREGION}


{$REGION 'TListAdapter<T>.TEnumerator' }

function TCollectionAdapters.TListAdapter<T>.TEnumerator.GetCurrent: T;
begin
  Result := fSource.List[fIndex - 1];
end;

function TCollectionAdapters.TListAdapter<T>.TEnumerator.MoveNext: Boolean;
var
  index: Integer;
begin
  index := fIndex;
  fIndex := index + 1;
  Result := index < fSource.Count;
end;

{$ENDREGION}


end.
