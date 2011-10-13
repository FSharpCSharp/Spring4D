{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
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
//      TGenericObjectStack = Generics.Collections.TObjectStack<T>;

      TStackEnumerator = class(TEnumeratorBase<T>)
      private
        fStack: TGenericStack;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(stack: TGenericStack);
        function MoveNext: Boolean; override;
      end;
  private
    fStack: TGenericStack;
    fOwnership: TOwnershipType;
    fOnNotify: ICollectionNotifyDelegate<T>;
    function GetOnNotify: ICollectionNotifyDelegate<T>;
  protected
    function GetCount: Integer; override;

//    function TryGetFirst(out value: T): Boolean; override;
//    function TryGetLast(out value: T): Boolean; override;
    class function GetStackItem(stack: TGenericStack; index: Integer): T;
  public
    constructor Create; overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(const collection: TEnumerable<T>); overload;
    constructor Create(stack: TGenericStack; ownership: TOwnershipType); overload;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    {$REGION 'Implements ICollection<T>'}
      procedure Add(const item: T); // override;
      function  Remove(const item: T): Boolean; // override;
      procedure Clear;
    {$ENDREGION}

    procedure Push(const item: T);
    function Pop: T;
    function Peek: T;
    function PeekOrDefault: T; overload;
    function PeekOrDefault(const predicate: TPredicate<T>): T; overload;
    function TryPeek(out item: T): Boolean;
    procedure TrimExcess;
    property OnNotify: ICollectionNotifyDelegate<T> read GetOnNotify;
  end;

implementation


{$REGION 'TStack<T>'}

constructor TStack<T>.Create(stack: TGenericStack;
  ownership: TOwnershipType);
begin
  inherited Create;
  fStack := stack;
  fOwnership := ownership;
end;

constructor TStack<T>.Create(const collection: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
  begin
    push(item);
  end;
end;

constructor TStack<T>.Create(const collection: TEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
  begin
    push(item);
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
    fStack.Free;
  inherited Destroy;
end;

function TStack<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TStackEnumerator.Create(fStack);
end;

function TStack<T>.GetCount: Integer;
begin
  Result := fStack.Count;
end;

function TStack<T>.GetOnNotify: ICollectionNotifyDelegate<T>;
begin
  if fOnNotify = nil then
  begin
    fOnNotify := TCollectionNotifyDelegate<T>.Create;
  end;
  Result := fOnNotify;
end;

{
  TStack<T> = class(TEnumerable<T>)
  private
    FCount: Integer;
    FItems: array of T;
    //...
  end;
}
class function TStack<T>.GetStackItem(stack: TGenericStack; index: Integer): T;
begin
  Result := TArray<T>(PInteger(NativeInt(stack) + hfFieldSize + SizeOf(Integer))^)[index];
end;

procedure TStack<T>.Push(const item: T);
begin
  fStack.Push(item);
end;

function TStack<T>.Pop: T;
begin
  Result := fStack.Pop;
end;

procedure TStack<T>.Add(const item: T);
begin
  fStack.Push(item);
end;

function TStack<T>.Remove(const item: T): Boolean;
//var
//  stack: TStackAccess<T>;
//  comparer: IComparer<T>;
//  element: T;
begin
  // TODO: TStack<T>.Remove
//  stack := TStackAccess<T>(fStack);
//  comparer := TComparer<T>.Default;
  Result := False;
end;

procedure TStack<T>.Clear;
begin
  fStack.Clear;
end;

function TStack<T>.Peek: T;
begin
  Result := fStack.Peek;
end;

function TStack<T>.PeekOrDefault(const predicate: TPredicate<T>): T;
var
  item: T;
begin
  Result := Default(T);
  if (fStack.Count = 1) and predicate(fStack.Peek) then
  begin
    Result := fStack.Peek;
  end
  else if fStack.Count > 0 then
  begin
    for item in fStack do
    begin
      if predicate(item) then
      begin
        Result := item;
        Break;
      end;
    end;
  end;
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


{$REGION 'TStack<T>.TEnumerator'}

constructor TStack<T>.TStackEnumerator.Create(stack: TGenericStack);
begin
  inherited Create;
  fStack := stack;
  fIndex := fStack.Count;
end;

function TStack<T>.TStackEnumerator.GetCurrent: T;
begin
  Result := TStack<T>.GetStackItem(fStack, fIndex);
end;

function TStack<T>.TStackEnumerator.MoveNext: Boolean;
begin
  Result := fIndex > 0;
  if Result then
    Dec(fIndex);
end;

{$ENDREGION}

end.
