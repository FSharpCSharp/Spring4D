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

unit Spring.Collections.Stacks;

interface

uses
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type
  TStack<T> = class(TEnumerableBase<T>, IStack<T>)
  private
    type
      TGenericStack = Generics.Collections.TStack<T>;
  private
    fStack: TGenericStack;
    fOwnership: TOwnershipType;
    fOnChanged: ICollectionChangedEvent<T>;
    function GetOnChanged: ICollectionChangedEvent<T>;
  protected
    function GetCount: Integer; override;

    procedure Changed(const item: T; action: TCollectionChangedAction); virtual;
    class function GetStackItem(stack: TGenericStack; index: Integer): T;
  public
    constructor Create; overload; override;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(const collection: TEnumerable<T>); overload;
    constructor Create(stack: TGenericStack; ownership: TOwnershipType); overload;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;
    procedure Clear;

    procedure Push(const item: T);
    function Pop: T;
    function Peek: T;
    function PeekOrDefault: T; 
    function TryPeek(out item: T): Boolean;
    procedure TrimExcess;
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

implementation

uses
  Spring.Collections.Events,
  Spring.Collections.Extensions;


{$REGION 'TStack<T>'}

constructor TStack<T>.Create(stack: TGenericStack;
  ownership: TOwnershipType);
begin
  inherited Create;
  fStack := stack;
  fOwnership := ownership;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

constructor TStack<T>.Create(const collection: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
  begin
    Push(item);
  end;
end;

constructor TStack<T>.Create(const collection: TEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
  begin
    Push(item);
  end;
end;

constructor TStack<T>.Create;
var
  stack: TGenericStack;
begin
  stack := TGenericStack.Create;
  Create(stack, otOwned);
end;

destructor TStack<T>.Destroy;
begin
  if fOwnership = otOwned then
  begin
    // call our Clear to trigger Changed
    Clear;
    fStack.Free;
  end;

  inherited Destroy;
end;

function TStack<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fStack);
end;

function TStack<T>.GetCount: Integer;
begin
  Result := fStack.Count;
end;

function TStack<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

class function TStack<T>.GetStackItem(stack: TGenericStack; index: Integer): T;
begin
  Result := TArray<T>(PInteger(NativeInt(stack) + hfFieldSize + SizeOf(Integer))^)[index];
end;

procedure TStack<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  fOnChanged.Invoke(Self, item, action);
end;

procedure TStack<T>.Push(const item: T);
begin
  fStack.Push(item);

  Changed(item, caAdded);
end;

function TStack<T>.Pop: T;
begin
  Result := fStack.Pop;

  Changed(Result, caRemoved);
end;

procedure TStack<T>.Clear;
begin
  // do not call fStack.Clear because we want our Changed to be triggered.
  while Count > 0 do
    Pop;
  TrimExcess;
end;

function TStack<T>.Peek: T;
begin
  Result := fStack.Peek;
end;

function TStack<T>.PeekOrDefault: T;
begin
  if fStack.Count > 0 then
    Result := fStack.Peek
  else
    Result := Default(T);
end;

procedure TStack<T>.TrimExcess;
begin
  fStack.TrimExcess;
end;

function TStack<T>.TryPeek(out item: T): Boolean;
begin
  Result := fStack.Count > 0;
  if Result then
    item := fStack.Peek
  else
    item := Default(T);
end;

{$ENDREGION}


end.
