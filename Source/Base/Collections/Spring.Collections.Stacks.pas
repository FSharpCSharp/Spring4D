{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2020 Spring4D Team                           }
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
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Events;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

type
  /// <summary>
  ///   Represents a last-in, first-out (LIFO) collection of items.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the stack.
  /// </typeparam>
  TAbstractStack<T> = class(TEnumerableBase<T>)
  private
  {$REGION 'Nested Types'}
    type
      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TAbstractStack<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
        function GetCurrent: T;
      public
        constructor Create(const source: TAbstractStack<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    fOnChanged: TCollectionChangedEventImpl<T>;
    fItems: TArray<T>;
    fCapacity: Integer;
    fCount: Integer;
    fVersion: Integer;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetIsEmpty: Boolean; inline;
    function GetOnChanged: ICollectionChangedEvent<T>;
    function GetOwnsObjects: Boolean; inline;
    procedure SetCapacity(value: Integer);
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
    procedure DoNotify(const item: T; action: TCollectionChangedAction); inline;
    procedure PopInternal(var item: T; action: TCollectionChangedAction); inline;
    procedure PushInternal(const item: T); inline;
    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create; override;
    constructor Create(const values: array of T); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
  {$ENDREGION}

  {$REGION 'Implements IStack<T>'}
    function Pop: T;
    function Extract: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryPop(var item: T): Boolean;
    function TryExtract(var item: T): Boolean;
    function TryPeek(var item: T): Boolean;

    procedure Clear;
    procedure TrimExcess;
  {$ENDREGION}
  end;

  TStack<T> = class(TAbstractStack<T>, IInterface, IEnumerable<T>, IStack<T>)
  private
    procedure Grow;
  public
    constructor Create(const values: IEnumerable<T>); overload;
    procedure Clear;
    function Push(const item: T): Boolean;
  end;

  TBoundedStack<T> = class(TAbstractStack<T>, IInterface, IEnumerable<T>, IStack<T>)
  public
    constructor Create(capacity: Integer);
    function Push(const item: T): Boolean;
  end;

  TFoldedStack<T> = class(TStack<T>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const elementType: PTypeInfo;
      const comparer: IComparer<T>; ownsObjects: Boolean = False);
  end;

implementation

uses
  Classes,
  RTLConsts,
  SysUtils,
  TypInfo,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TAbstractStack<T>'}

constructor TAbstractStack<T>.Create;
begin
  inherited Create;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;

  Pointer(this) := Pointer(PByte(Self) + GetInterfaceEntry(IStack<T>).IOffset);
end;

constructor TAbstractStack<T>.Create(const values: array of T);
var
  count, i: Integer;
begin
  Create;
  fCount := Length(values);
  if fCount > 0 then
  begin
    fCapacity := fCount;
    SetLength(fItems, fCount);
    for i := Low(values) to High(values) do
      fItems[i] := values[i];
  end;
end;

destructor TAbstractStack<T>.Destroy;
begin
  IStack<T>(this).Clear;
  fOnChanged.Free;
  inherited Destroy;
end;

procedure TAbstractStack<T>.DoNotify(const item: T; action: TCollectionChangedAction);
begin
  if fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
end;

function TAbstractStack<T>.GetCapacity: Integer;
begin
  Result := fCapacity;
end;

function TAbstractStack<T>.GetCount: Integer;
begin
  Result := fCount and CountMask;
end;

function TAbstractStack<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TAbstractStack<T>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TAbstractStack<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

function TAbstractStack<T>.GetOwnsObjects: Boolean;
begin
  Result := {$IFDEF DELPHIXE7_UP}(GetTypeKind(T) = tkClass) and {$ENDIF}(fCount < 0);
end;

procedure TAbstractStack<T>.Clear;
var
  stackCount, i: Integer;
begin
  stackCount := Count;
  if stackCount > 0 then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    Dec(fCount, stackCount);

    if fOnChanged.CanInvoke then
      if OwnsObjects then
        for i := stackCount - 1 downto 0 do
        begin
          fOnChanged.Invoke(Self, fItems[i], caRemoved);
          FreeObject(fItems[i]);
        end
      else
        for i := stackCount - 1 downto 0 do
          fOnChanged.Invoke(Self, fItems[i], caRemoved)
    else
      if OwnsObjects then
        for i := stackCount - 1 downto 0 do
          FreeObject(fItems[i]);

    if TType.IsManaged<T> then
      System.FinalizeArray(@fItems[0], TypeInfo(T), stackCount)
    else
      System.FillChar(fItems[0], SizeOf(T) * stackCount, 0);
  end;
end;

procedure TAbstractStack<T>.PopInternal(var item: T; action: TCollectionChangedAction);
var
  stackCount: Integer;
  stackItem: ^T;
begin
  stackCount := Count;
  if stackCount > 0 then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    Dec(fCount);
    stackItem := @fItems[stackCount - 1];
    item := stackItem^;
    stackItem^ := Default(T);

    DoNotify(item, action);
    if OwnsObjects and (action = caRemoved) then
    begin
      FreeObject(item);
      item := Default(T);
    end;
  end
  else
    raise Error.NoElements;
end;

procedure TAbstractStack<T>.PushInternal(const item: T);
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fItems[Count] := item;
  Inc(fCount);

  DoNotify(item, caAdded);
end;

function TAbstractStack<T>.Pop: T;
begin
  PopInternal(Result, caRemoved);
end;

function TAbstractStack<T>.Extract: T;
begin
  PopInternal(Result, caExtracted);
end;

function TAbstractStack<T>.Peek: T;
begin
  if Count > 0 then
    Result := fItems[Count - 1]
  else
    raise Error.NoElements;
end;

function TAbstractStack<T>.PeekOrDefault: T;
begin
  if Count > 0 then
    Result := fItems[Count - 1]
  else
    Result := Default(T);
end;

procedure TAbstractStack<T>.SetCapacity(value: Integer);
begin
  Guard.CheckRange(value >= Count, 'capacity');

  fCapacity := value;
  SetLength(fItems, value);
end;

procedure TAbstractStack<T>.SetOwnsObjects(const value: Boolean);
begin
  if GetElementType.Kind = tkClass then
    fCount := (fCount and CountMask) or BitMask[value];
end;

procedure TAbstractStack<T>.TrimExcess;
begin
  fCapacity := Count;
  SetLength(fItems, fCapacity);
end;

function TAbstractStack<T>.TryPop(var item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    PopInternal(item, caRemoved)
  else
    item := Default(T);
end;

function TAbstractStack<T>.TryExtract(var item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    PopInternal(item, caExtracted)
  else
    item := Default(T);
end;

function TAbstractStack<T>.TryPeek(var item: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    item := fItems[Count - 1]
  else
    item := Default(T);
end;

{$ENDREGION}


{$REGION 'TAbstractStack<T>.TEnumerator'}

constructor TAbstractStack<T>.TEnumerator.Create(const source: TAbstractStack<T>);
begin
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TAbstractStack<T>.TEnumerator.Destroy;
begin
  fSource._Release;
end;

function TAbstractStack<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TAbstractStack<T>.TEnumerator.MoveNext: Boolean;
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


{$REGION 'TStack<T>'}

constructor TStack<T>.Create(const values: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in values do
    Push(item);
end;

procedure TStack<T>.Clear;
begin
  inherited Clear;
  fCapacity := 0;
  SetLength(fItems, 0);
end;

function TStack<T>.Push(const item: T): Boolean;
begin
  if Count = Capacity then
    Grow;
  PushInternal(item);
  Result := True;
end;

procedure TStack<T>.Grow;
begin
  fCapacity := GrowCapacity(fCapacity);
  SetLength(fItems, fCapacity);
end;

{$ENDREGION}


{$REGION 'TBoundedStack<T>'}

constructor TBoundedStack<T>.Create(capacity: Integer);
begin
  inherited Create;
  SetCapacity(capacity);
end;

function TBoundedStack<T>.Push(const item: T): Boolean;
begin
  if Count = Capacity then
    Exit(False);
  PushInternal(item);
  Result := True;
end;

{$ENDREGION}


{$REGION 'TFoldedStack<T>'}

constructor TFoldedStack<T>.Create(const elementType: PTypeInfo;
  const comparer: IComparer<T>; ownsObjects: Boolean);
begin
  inherited Create(comparer);
  fElementType := elementType;
  SetOwnsObjects(ownsObjects);
end;

function TFoldedStack<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


end.
