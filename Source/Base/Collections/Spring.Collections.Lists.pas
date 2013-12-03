{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2013 Spring4D Team                           }
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

unit Spring.Collections.Lists;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type

  ///	<summary>
  ///	  Represents a strongly typed list of elements that can be accessed by
  ///	  index. Provides methods to search, sort, and manipulate lists.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of elements in the list.
  ///	</typeparam>
  TList<T> = class(TListBase<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TList<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TList<T>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
        procedure Reset; override;
      end;
  private
  {$IFDEF NEXTGEN}
    fArrayManager: TArrayManager<T>;
  {$ENDIF}
    fItems: array of T;
    fCount: Integer;
    fVersion: Integer;
    procedure DeleteInternal(index: Integer; notification: TCollectionChangedAction);
    procedure IncreaseVersion; inline;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer; override;
    function GetItem(index: Integer): T; override;
    procedure SetCapacity(value: Integer);
    procedure SetItem(index: Integer; const value: T); override;
  {$ENDREGION}

    function EnsureCapacity(value: Integer): Integer;
  public
    function GetEnumerator: IEnumerator<T>; override;

    procedure Clear; override;

    procedure Insert(index: Integer; const item: T); override;

    procedure Delete(index: Integer); override;
    procedure DeleteRange(index, count: Integer); override;

    function Extract(const item: T): T; override;

    procedure Exchange(index1, index2: Integer); override;
    procedure Move(currentIndex, newIndex: Integer); override;

    procedure Reverse(index, count: Integer); override;
    procedure Sort(const comparer: IComparer<T>); override;

    procedure CopyTo(var values: TArray<T>; index: Integer); override;
    function ToArray: TArray<T>; override;

    property Capacity: Integer read GetCapacity write SetCapacity;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  TObjectList<T: class> = class(TList<T>, ICollectionOwnership)
  private
    fOwnsObjects: Boolean;
  {$REGION 'Property Accessors'}
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  protected
    procedure Changed(const item: T; action: TCollectionChangedAction); override;
  public
    constructor Create(ownsObjects: Boolean = True); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;
    constructor Create(const collection: array of T; ownsObjects: Boolean = True); overload;
    constructor Create(const collection: IEnumerable<T>; ownsObjects: Boolean = True); overload;
    constructor Create(collection: TEnumerable<T>; ownsObjects: Boolean = True); overload;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

implementation

uses
  SysUtils,
  Spring.ResourceStrings;


{$REGION 'TList<T>'}

function TList<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TList<T>.GetItem(index: Integer): T;
begin
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');

  Result := fItems[index];
end;

{$IFOPT Q+}{$DEFINE OVERFLOW_CHECKS_ON}{$Q-}{$ENDIF}
procedure TList<T>.IncreaseVersion;
begin
  Inc(fVersion);
end;
{$IFDEF OVERFLOW_CHECKS_ON}{$Q+}{$ENDIF}

procedure TList<T>.SetItem(index: Integer; const value: T);
var
  oldItem: T;
begin
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');

  oldItem := fItems[index];
  fItems[index] := value;
  IncreaseVersion;

  Changed(oldItem, caRemoved);
  Changed(value, caAdded);
end;

procedure TList<T>.Insert(index: Integer; const item: T);
begin
  Guard.CheckRange((index >= 0) and (index <= fCount), 'index');

  EnsureCapacity(fCount + 1);
  if index <> fCount then
  begin
{$IFNDEF NEXTGEN}
    System.Move(fItems[index], fItems[index + 1], (fCount - index) * SizeOf(T));
    System.FillChar(fItems[index], SizeOf(fItems[index]), 0);
{$ELSE}
    fArrayManager.Move(fItems, index, index + 1, fCount - index);
    fArrayManager.Finalize(fItems, index, 1);
{$ENDIF}
  end;
  fItems[index] := item;
  Inc(fCount);
  IncreaseVersion;

  Changed(item, caAdded);
end;

procedure TList<T>.DeleteInternal(index: Integer;
  notification: TCollectionChangedAction);
var
  oldItem: T;
begin
  oldItem := fItems[index];
  fItems[index] := Default(T);
  Dec(fCount);
  if index <> fCount then
  begin
{$IFNDEF NEXTGEN}
    System.Move(fItems[index + 1], fItems[index], (fCount - index) * SizeOf(T));
    System.FillChar(fItems[fCount], SizeOf(T), 0);
{$ELSE}
    fArrayManager.Move(fItems, index + 1, index, fCount - index);
    fArrayManager.Finalize(fItems, fCount, 1);
{$ENDIF}
  end;
  IncreaseVersion;

  Changed(oldItem, notification);
end;

procedure TList<T>.DeleteRange(index, count: Integer);
var
  oldItems: array of T;
  tailCount,
  i: Integer;
begin
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');

  if count = 0 then
    Exit;

  SetLength(oldItems, count);
{$IFNDEF NEXTGEN}
  System.Move(fItems[index], oldItems[0], count * SizeOf(T));
{$ELSE}
  fArrayManager.Move(fItems, oldItems, index, 0, count);
{$ENDIF}

  tailCount := fCount - (index + count);
  if tailCount > 0 then
  begin
{$IFNDEF NEXTGEN}
    System.Move(fItems[index + count], fItems[index], tailCount * SizeOf(T));
    System.FillChar(fItems[fCount - count], count * SizeOf(T), 0);
{$ELSE}
    fArrayManager.Move(fItems, index + count, index, tailCount);
    fArrayManager.Finalize(fItems, fCount - count, count);
{$ENDIF}
  end
  else
{$IFNDEF NEXTGEN}
    System.FillChar(fItems[index], count * SizeOf(T), 0);
{$ELSE}
    fArrayManager.Finalize(fItems, index, count);
{$ENDIF}

  Dec(fCount, count);
  IncreaseVersion;

  for i := 0 to Length(oldItems) - 1 do
    Changed(oldItems[i], caRemoved);
end;

procedure TList<T>.Sort(const comparer: IComparer<T>);
begin
  TArray.Sort<T>(fItems, comparer, 0, fCount);
  IncreaseVersion;

  Changed(Default(T), caReseted);
end;

procedure TList<T>.Move(currentIndex, newIndex: Integer);
var
  temp: T;
begin
  Guard.CheckRange((currentIndex >= 0) and (currentIndex < fCount), 'currentIndex');
  Guard.CheckRange((newIndex >= 0) and (newIndex < fCount), 'newIndex');

  temp := fItems[currentIndex];
  fItems[currentIndex] := Default(T);
  if currentIndex < newIndex then
{$IFNDEF NEXTGEN}
    System.Move(fItems[currentIndex + 1], fItems[currentIndex], (newIndex - currentIndex) * SizeOf(T))
{$ELSE}
    fArrayManager.Move(fItems, currentIndex + 1, currentIndex, newIndex - currentIndex)
{$ENDIF}
  else
{$IFNDEF NEXTGEN}
    System.Move(fItems[newIndex], fItems[newIndex + 1], (currentIndex - newIndex) * SizeOf(T));
{$ELSE}
    fArrayManager.Move(fItems, newIndex, newIndex + 1, currentIndex - newIndex);
{$ENDIF}

{$IFNDEF NEXTGEN}
  System.FillChar(fItems[newIndex], SizeOf(T), 0);
{$ELSE}
  fArrayManager.Finalize(fItems, newIndex, 1);
{$ENDIF}
  fItems[newIndex] := temp;
  IncreaseVersion;

  Changed(temp, caMoved);
end;

procedure TList<T>.AfterConstruction;
begin
  inherited;
{$IFDEF NEXTGEN}
{$IF Defined(WEAKREF)}
  if HasWeakRef then
    fArrayManager := TManualArrayManager<T>.Create
  else
{$IFEND}
   fArrayManager := TMoveArrayManager<T>.Create;
{$ENDIF}
end;

destructor TList<T>.Destroy;
begin
  inherited;
{$IFDEF NEXTGEN}
{$IFNDEF AUTOREFCOUNT}
  fArrayManager.Free;
{$ENDIF}
{$ENDIF}
end;

procedure TList<T>.Clear;
begin
  inherited Clear;
  Capacity := 0;
end;

function TList<T>.EnsureCapacity(value: Integer): Integer;
var
  newCapacity: Integer;
begin
  newCapacity := Length(fItems);
  if newCapacity >= value then
    Exit(newCapacity);

  if newCapacity = 0 then
    newCapacity := value
  else
    repeat
      newCapacity := newCapacity * 2;
      if newCapacity < 0 then
        OutOfMemoryError;
    until newCapacity >= value;
  Capacity := newCapacity;
  Result := newCapacity;
end;

procedure TList<T>.Exchange(index1, index2: Integer);
var
  temp: T;
begin
  Guard.CheckRange((index1 >= 0) and (index1 < fCount), 'index1');
  Guard.CheckRange((index2 >= 0) and (index2 < fCount), 'index2');

  temp := fItems[index1];
  fItems[index1] := fItems[index2];
  fItems[index2] := temp;
  IncreaseVersion;

  Changed(fItems[index2], caMoved);
  Changed(fItems[index1], caMoved);
end;

function TList<T>.GetCapacity: Integer;
begin
  Result := Length(fItems);
end;

procedure TList<T>.Reverse(index, count: Integer);
var
  temp: T;
  index1, index2: Integer;
begin
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');

  index1 := index;
  index2 := index + count - 1;
  while index1 < index2 do
  begin
    temp := fItems[index1];
    fItems[index1] := fItems[index2];
    fItems[index2] := temp;
    Inc(index1);
    Dec(index2);
  end;
  IncreaseVersion;

  Changed(Default(T), caReseted);
end;

procedure TList<T>.SetCapacity(value: Integer);
begin
  if value < Count then
    DeleteRange(Count - value + 1, Count - value);
  SetLength(fItems, value);
end;

procedure TList<T>.Delete(index: Integer);
begin
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');

  DeleteInternal(index, caRemoved);
end;

function TList<T>.Extract(const item: T): T;
var
  index: Integer;
begin
  index := IndexOf(item);
  if index < 0 then
    Result := Default(T)
  else
  begin
    Result := fItems[index];
    DeleteInternal(index, caExtracted);
  end;
end;

procedure TList<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  i: Integer;
begin
  Guard.CheckRange(Length(values), index, fCount);
  Guard.CheckRange(Length(fItems), 0, fCount);

  for i := 0 to fCount - 1 do
    values[i + index] := fItems[i];
end;

function TList<T>.ToArray: TArray<T>;
begin
  Result := TArray<T>(fItems);
  SetLength(Result, fCount);
end;

{$ENDREGION}


{$REGION 'TList<T>.TEnumerator'}

constructor TList<T>.TEnumerator.Create(const list: TList<T>);
begin
  inherited Create;
  fList := list;
  fList._AddRef;
  fVersion := fList.fVersion;
end;

destructor TList<T>.TEnumerator.Destroy;
begin
  fList._Release;
  inherited Destroy;
end;

function TList<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  if fIndex < fList.fCount then
  begin
    fCurrent := fList.fItems[fIndex];
    Inc(fIndex);
    Result := True;
  end
  else
    fCurrent := Default(T);
end;

function TList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

procedure TList<T>.TEnumerator.Reset;
begin
  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  fIndex := 0;
  fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TObjectList<T>'}

constructor TObjectList<T>.Create(ownsObjects: Boolean);
begin
  inherited Create;
  fOwnsObjects := ownsObjects;
end;

constructor TObjectList<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  fOwnsObjects := ownsObjects;
end;

constructor TObjectList<T>.Create(const collection: array of T;
  ownsObjects: Boolean);
begin
  inherited Create(collection);
  fOwnsObjects := ownsObjects;
end;

constructor TObjectList<T>.Create(const collection: IEnumerable<T>;
  ownsObjects: Boolean);
begin
  inherited Create(collection);
  fOwnsObjects := ownsObjects;
end;

constructor TObjectList<T>.Create(collection: TEnumerable<T>;
  ownsObjects: Boolean);
begin
  inherited Create(collection);
  fOwnsObjects := ownsObjects;
end;

function TObjectList<T>.GetOwnsObjects: Boolean;
begin
  Result := fOwnsObjects;
end;

procedure TObjectList<T>.SetOwnsObjects(const value: Boolean);
begin
  fOwnsObjects := value;
end;

procedure TObjectList<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  inherited Changed(item, action);
  if OwnsObjects and (action = caRemoved) then
{$IFNDEF AUTOREFCOUNT}
    item.Free;
{$ELSE}
    item.DisposeOf;
{$ENDIF}
end;

{$ENDREGION}


end.
