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
    procedure Grow;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer; 
    function GetIsEmpty: Boolean;
    function GetOnChanged: ICollectionChangedEvent<T>;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
    procedure Changed(const item: T; action: TCollectionChangedAction);
    function PopInternal(notification: TCollectionChangedAction): T; virtual;
  public
    constructor Create; overload; override;
    constructor Create(const values: array of T); overload;
    constructor Create(const collection: IEnumerable<T>); overload;
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

  TObjectStack<T: class> = class(TStack<T>, ICollectionOwnership)
  private
    fOwnsObjects: Boolean;
  {$REGION 'Property Accessors'}
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  protected
    function PopInternal(notification: TCollectionChangedAction): T; override;
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

constructor TStack<T>.Create(const collection: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
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

procedure TStack<T>.Clear;
begin
  while fCount > 0 do
    Pop;
  SetLength(fItems, 0);
end;

function TStack<T>.Extract: T;
begin
  Result := PopInternal(caExtracted);
end;

function TStack<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TStack<T>.GetIsEmpty: Boolean;
begin
  Result := fCount = 0;
end;

function TStack<T>.GetCapacity: Integer;
begin
  Result := Length(fItems);
end;

function TStack<T>.GetCount: Integer;
begin
  Result := fCount;
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
  if fCount = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);
  Result := fItems[fCount - 1];
end;

function TStack<T>.PeekOrDefault: T;
begin
  if fCount = 0 then
    Result := Default(T)
  else
    Result := fItems[fCount - 1];
end;

function TStack<T>.Pop: T;
begin
  Result := PopInternal(caRemoved);
end;

function TStack<T>.PopInternal(notification: TCollectionChangedAction): T;
begin
  if fCount = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);

  IncUnchecked(fVersion);
  Dec(fCount);
  Result := fItems[fCount];
  fItems[fCount] := Default(T);

  Changed(Result, notification);
end;

procedure TStack<T>.Push(const item: T);
begin
  if fCount = Length(fItems) then
    Grow;

  IncUnchecked(fVersion);
  fItems[fCount] := item;
  Inc(fCount);

  Changed(item, caAdded);
end;

procedure TStack<T>.SetCapacity(value: Integer);
begin
  Guard.CheckRange(value >= fCount, 'capacity');

  SetLength(fItems, value);
end;

procedure TStack<T>.TrimExcess;
begin
  SetLength(fItems, fCount);
end;

function TStack<T>.TryExtract(out item: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    item := Extract
  else
    item := Default(T);
end;

function TStack<T>.TryPeek(out item: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    item := Peek
  else
    item := Default(T);
end;

function TStack<T>.TryPop(out item: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    item := Pop
  else
    item := Default(T);
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

  if fIndex < fSource.fCount then
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
  fOwnsObjects := True;
end;

constructor TObjectStack<T>.Create(ownsObjects: Boolean);
begin
  inherited Create;
  fOwnsObjects := ownsObjects;
end;

constructor TObjectStack<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  fOwnsObjects := ownsObjects;
end;

function TObjectStack<T>.GetOwnsObjects: Boolean;
begin
  Result := fOwnsObjects;
end;

function TObjectStack<T>.PopInternal(notification: TCollectionChangedAction): T;
begin
  Result := inherited PopInternal(notification);
  if fOwnsObjects and (notification = caRemoved) then
  begin
{$IFNDEF AUTOREFCOUNT}
    Result.Free;
{$ELSE}
    Result.DisposeOf;
{$ENDIF}
    Result := nil;
  end;
end;

procedure TObjectStack<T>.SetOwnsObjects(const value: Boolean);
begin
  fOwnsObjects := value;
end;

{$ENDREGION}


end.
