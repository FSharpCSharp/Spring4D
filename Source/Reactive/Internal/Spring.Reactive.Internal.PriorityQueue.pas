{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

unit Spring.Reactive.Internal.PriorityQueue;

interface

type
  TPriorityQueue<T> = class
  private type
    TIndexedItem = record
      Value: T;
      Id: Integer;
      function CompareTo(const other: TIndexedItem): Integer;
    end;
    class var fCount: Integer;
  private
    fItems: array of TIndexedItem;
    fSize: Integer;
    function IsHigherPriority(left, right: Integer): Boolean;
    procedure Percolate(index: Integer);
    procedure Heapify(index: Integer = 0);
    procedure RemoveAt(index: Integer);
  public
    constructor Create(capacity: Integer = 16);
    property Count: Integer read fSize;

    function Peek: T;
    function Dequeue: T;
    procedure Enqueue(const item: T);
    function Remove(const item: T): Boolean;
  end;

// TODO move to Spring or some other base unit
function CompareIntf(const left, right: IInterface): Integer;

implementation

uses
  Generics.Defaults,
  Math,
  SysUtils,
  TypInfo,
  Spring;

function CompareIntf(const left, right: IInterface): Integer;
var
  comparable: IComparable;
begin
  Assert(Supports(left, IComparable, comparable));
  Result := comparable.CompareTo(right as TObject);
end;


{$REGION 'TPriorityQueue<T>'}

constructor TPriorityQueue<T>.Create(capacity: Integer);
begin
  inherited Create;
  SetLength(fItems, capacity);
end;

function TPriorityQueue<T>.Dequeue: T;
begin
  Result := Peek;
  RemoveAt(0);
end;

procedure TPriorityQueue<T>.Enqueue(const item: T);
var
  index: Integer;
begin
  if fSize >= Length(fItems) then
    SetLength(fItems, Length(fItems) * 2);
  index := fSize;
  Inc(fSize);
  fItems[index].Value := item;
  fItems[index].Id := AtomicIncrement(fCount);
  Percolate(index);
end;

procedure TPriorityQueue<T>.Heapify(index: Integer);
var
  left: Integer;
  right: Integer;
  first: Integer;
  temp: TIndexedItem;
begin
  if (index >= fSize) or (index < 0) then
    Exit;

  left := 2 * index + 1;
  right := 2 * index + 2;
  first := index;

  if (left < fSize) and IsHigherPriority(left, first) then
    first := left;
  if (right < fSize) and IsHigherPriority(right, first) then
    first := right;
  if first <> index then
  begin
    temp := fItems[index];
    fItems[index] := fItems[first];
    fItems[first] := temp;
    Heapify(first);
  end;
end;

function TPriorityQueue<T>.IsHigherPriority(left, right: Integer): Boolean;
begin
  Result := fItems[left].CompareTo(fItems[right]) < 0;
end;

function TPriorityQueue<T>.Peek: T;
begin
  if fSize = 0 then
    raise EInvalidOperationException.Create('heap empty');
  Result := fItems[0].Value;
end;

procedure TPriorityQueue<T>.Percolate(index: Integer);
var
  parent: Integer;
  temp: TIndexedItem;
begin
  if (index >= fSize) or (index < 0) then
    Exit;
  parent := (index - 1) div 2;
  if (parent < 0) or (parent = index) then
    Exit;

  if IsHigherPriority(index, parent) then
  begin
    temp := fItems[index];
    fItems[index] := fItems[parent];
    fItems[parent] := temp;
    Percolate(parent);
  end;
end;

function TPriorityQueue<T>.Remove(const item: T): Boolean;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  comparer := TEqualityComparer<T>.Default;
  for i := 0 to fSize - 1 do
    if comparer.Equals(fItems[i].Value, item) then
    begin
      RemoveAt(i);
      Exit(True);
    end;
  Result := False;
end;

procedure TPriorityQueue<T>.RemoveAt(index: Integer);
begin
  Dec(fSize);
  fItems[index] := fItems[fSize];
  fItems[fSize] := Default(TIndexedItem);
  Heapify;
  if fSize < (Length(fItems) div 4) then
    SetLength(fItems, Length(fItems) div 2);
end;

{$ENDREGION}


{$REGION 'TPriorityQueue<T>.TIndexedItem'}

function TPriorityQueue<T>.TIndexedItem.CompareTo(const other: TIndexedItem): Integer;
begin
  case TType.Kind<T> of
    tkInterface:
      Result := CompareIntf(PInterface(@Value)^, PInterface(@other.Value)^);
  else
    Result := TComparer<T>.Default.Compare(Value, other.Value);
  end;

  if Result = 0 then
    Result := CompareValue(Id, other.Id);
end;

{$ENDREGION}


end.
