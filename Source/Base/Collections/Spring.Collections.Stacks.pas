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

unit Spring.Collections.Stacks;

interface

uses
  Generics.Defaults,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Events;

type
  /// <summary>
  ///   Represents a last-in, first-out (LIFO) collection of items.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the stack.
  /// </typeparam>
  TStack<T> = class(TEnumerableBase<T>, INotifyCollectionChanged<T>,
    IEnumerable<T>, {ICollection<T>, IReadOnlyCollection<T>, }IStack<T>)
  private
  {$REGION 'Nested Types'}
    type
      TEnumerator = class(TInterfacedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TStack<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
        function GetCurrent: T;
      public
        constructor Create(const source: TStack<T>);
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
    function GetCapacity: Integer;
    function GetCount: Integer; inline;
    function GetIsEmpty: Boolean;
    function GetOnChanged: ICollectionChangedEvent<T>;
    function GetOwnsObjects: Boolean; inline;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
    procedure Grow;
  protected
    procedure Changed(const item: T; action: TCollectionChangedAction); inline;
    procedure PopInternal(var item: T; notification: TCollectionChangedAction); inline;
    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create; overload; override;
    constructor Create(const values: array of T); overload;
    constructor Create(const values: IEnumerable<T>); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
  {$ENDREGION}

  {$REGION 'Implements IStack<T>'}
    procedure Clear;
    procedure Push(const item: T);
    function Pop: T;
    function Extract: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryExtract(out item: T): Boolean;
    function TryPeek(out item: T): Boolean;
    function TryPop(out item: T): Boolean;

    procedure TrimExcess;
  {$ENDREGION}
  end;

  TObjectStack<T: class> = class(TStack<T>)
  private
  {$REGION 'Property Accessors'}
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  public
    constructor Create; override;
    constructor Create(ownsObjects: Boolean); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;
  end;

implementation

uses
  Classes,
  RTLConsts,
  SysUtils,
  Spring,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TStack<T>'}

constructor TStack<T>.Create;
begin
  inherited Create;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

constructor TStack<T>.Create(const values: array of T);
var
  i: Integer;
begin
  Create;
  for i := Low(values) to High(values) do
    Push(values[i]);
end;

constructor TStack<T>.Create(const values: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in values do
    Push(item);
end;

destructor TStack<T>.Destroy;
begin
  Clear;
  fOnChanged.Free;
  inherited Destroy;
end;

procedure TStack<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  if fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
end;

function TStack<T>.GetCount: Integer;
begin
  Result := fCount and CountMask;
end;

function TStack<T>.GetOwnsObjects: Boolean;
begin
  Result := {$IFDEF DELPHIXE7_UP}(GetTypeKind(T) = tkClass) and {$ENDIF}(fCount < 0);
end;

procedure TStack<T>.Clear;
begin
  while Count > 0 do
    Pop;
  SetLength(fItems, 0);
end;

function TStack<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TStack<T>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TStack<T>.GetCapacity: Integer;
begin
  Result := Length(fItems);
end;

function TStack<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

procedure TStack<T>.Grow;
var
  newCapacity: Integer;
begin
  newCapacity := Length(fItems) * 2;
  if newCapacity = 0 then
    newCapacity := 4
  else if newCapacity < 0 then
    OutOfMemoryError;
  SetLength(fItems, newCapacity);
end;

function TStack<T>.Peek: T;
begin
  if Count = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);
  Result := fItems[Count - 1];
end;

function TStack<T>.PeekOrDefault: T;
begin
  if Count = 0 then
    Result := Default(T)
  else
    Result := fItems[Count - 1];
end;

procedure TStack<T>.Push(const item: T);
begin
  if Count = DynArrayLength(fItems) then
    Grow;

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fItems[Count] := item;
  Inc(fCount);

  Changed(item, caAdded);
end;

procedure TStack<T>.SetCapacity(value: Integer);
begin
  Guard.CheckRange(value >= Count, 'capacity');

  SetLength(fItems, value);
end;

procedure TStack<T>.TrimExcess;
begin
  SetLength(fItems, Count);
end;

function TStack<T>.TryExtract(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    item := Extract
  else
    item := Default(T);
end;

function TStack<T>.TryPeek(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    item := Peek
  else
    item := Default(T);
end;

function TStack<T>.TryPop(out item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    item := Pop
  else
    item := Default(T);
end;

procedure TStack<T>.PopInternal(var item: T; notification: TCollectionChangedAction);
begin
  if Count = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);
  item := fItems[Count];
  fItems[Count] := Default(T);

  Changed(item, notification);

  if OwnsObjects and (notification = caRemoved) then
  begin
    FreeObject(item);
    item := Default(T);
  end;
end;

function TStack<T>.Extract: T;
begin
  PopInternal(Result, caExtracted);
end;

function TStack<T>.Pop: T;
begin
  PopInternal(Result, caRemoved);
end;

{$ENDREGION}


{$REGION 'TStack<T>.TEnumerator'}

constructor TStack<T>.TEnumerator.Create(const source: TStack<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TStack<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited Destroy;
end;

function TStack<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TStack<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fVersion <> fSource.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  if fIndex < fSource.Count then
  begin
    fCurrent := fSource.fItems[fIndex];
    Inc(fIndex);
    Result := True;
  end
  else
    fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TObjectStack<T>'}

constructor TObjectStack<T>.Create;
begin
  inherited Create;
  SetOwnsObjects(True);
end;

constructor TObjectStack<T>.Create(ownsObjects: Boolean);
begin
  inherited Create;
  SetOwnsObjects(ownsObjects);
end;

constructor TObjectStack<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  SetOwnsObjects(ownsObjects);
end;

procedure TObjectStack<T>.SetOwnsObjects(const value: Boolean);
begin
  fCount := (fCount and CountMask) or BitMask[value];
end;

{$ENDREGION}


end.
