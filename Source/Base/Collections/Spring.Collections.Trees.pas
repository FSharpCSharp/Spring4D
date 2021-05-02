{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2021 Spring4D Team                           }
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

unit Spring.Collections.Trees;

{$I Spring.inc}

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

const
  ColorMask = IntPtr(1);
  PointerMask = not ColorMask;

type
  TNodeColor = (Black, Red);

  TBlockAllocatedArray<T> = record
  strict private type
    PT = ^T;
  strict private const
    BlockSizeBits = 6;
    BlockSize = 1 shl BlockSizeBits;
  strict private
    fItems: TArray<TArray<T>>;
    function GetItem(index: Integer): PT; inline;
    function GetCapacity: Integer;
    procedure SetCapacity(const value: Integer);
  public
    procedure Grow;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[index: Integer]: PT read GetItem; default;
  end;

  TTraverseMode = (tmInOrder, tmPreOrder, tmPostOrder);

  TBinaryTree = class abstract(TRefCountedObject)
  private type
    PNode = ^TNode;

    TEnumerator = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
      fCurrent: PNode;
    strict private
      function MoveNextInOrder: Boolean;
      function MoveNextPreOrder: Boolean;
      function MoveNextPostOrder: Boolean;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean;
      property Current: PNode read fCurrent;
    end;

    TNode = record
    strict private
      fParent, fLeft, fRight: PNode;
      function GetParent: PNode; inline;
      function GetLeftMost: PNode;
      function GetRightMost: PNode;
      function GetNext: PNode;
      function GetPrev: PNode;
      function GetHeight: Integer;
    public
      property Left: PNode read fLeft;
      property Parent: PNode read GetParent;
      property Right: PNode read fRight;

      property LeftMost: PNode read GetLeftMost;
      property RightMost: PNode read GetRightMost;
      property Next: PNode read GetNext;
      property Prev: PNode read GetPrev;

      property Height: Integer read GetHeight;
    end;
  protected
    fRoot: Pointer;
    fCount: Integer;

  {$REGION 'Property Accessors'}
    function GetCount: Integer; inline;
    function GetHeight: Integer;
    function GetRoot: PNode; inline;
  {$ENDREGION}

    property Root: PNode read GetRoot;
  end;

  TRedBlackTree = class abstract(TBinaryTree)
  private type
    PNode = ^TNode;
    TNode = record
    strict private
      fParent: PNode;
      function GetColor: TNodeColor; inline;
      function GetIsBlack: Boolean; inline;
      function GetParent: PNode; inline;
      procedure SetColor(const value: TNodeColor); inline;
      procedure SetLeft(const value: PNode);
      procedure SetRight(const value: PNode);
      procedure ClearParent;
    private
      fLeft, fRight: PNode;
      procedure SetParent(const value: PNode); inline;
      property Color: TNodeColor read GetColor write SetColor;
    public
      property Left: PNode read fLeft write SetLeft;
      property Parent: PNode read GetParent;
      property Right: PNode read fRight write SetRight;
      property IsBlack: Boolean read GetIsBlack;
    end;
  private
    procedure InsertLeft(node, newNode: PNode);
    procedure InsertRight(node, newNode: PNode);
    procedure RotateLeft(node: PNode);
    procedure RotateRight(node: PNode);
    procedure FixupAfterInsert(node: PNode);
    procedure FixupAfterDelete(node: PNode);
    procedure Delete(node: PNode);
    procedure SetRoot(value: PNode);
    property Root: PNode write SetRoot;
  protected
    procedure FreeNode(node: Pointer); virtual; abstract;
  public
    destructor Destroy; override;

    procedure Clear; virtual;
  end;

  PBinaryTreeNode = TBinaryTree.PNode;
  PRedBlackTreeNode = TRedBlackTree.PNode;

  TNodes<T> = record
  strict private type
    PNode = ^TNode;

    TKeyEnumerator = record
    private
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: T; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean; inline;
      property Current: T read GetCurrent;
    end;

    TKeyEnumerable = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function GetEnumerator: TKeyEnumerator; inline;
    end;

    TEnumerator = record
    private
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: PNode; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean; inline;
      property Current: PNode read GetCurrent;
    end;

    TEnumerable = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
      function GetKeys: TKeyEnumerable; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function GetEnumerator: TEnumerator; inline;
      property Keys: TKeyEnumerable read GetKeys;
    end;

    TNode = record
    strict private
      function GetLeftMost: PNode; inline;
      function GetRightMost: PNode; inline;
      function GetNext: PNode; inline;
      function GetPrev: PNode; inline;

      function GetColor: TNodeColor; inline;
      function GetParent: PNode; inline;
    private
      fParent, fLeft, fRight: PNode;
      fKey: T;
      procedure SetParent(const value: PNode); inline;
      function GetEnumerable(mode: TTraverseMode): TEnumerable; inline;
    public
      function GetEnumerator: TEnumerator; inline;

      property LeftMost: PNode read GetLeftMost;
      property RightMost: PNode read GetRightMost;
      property Next: PNode read GetNext;
      property Prev: PNode read GetPrev;

      property Left: PNode read fLeft;
      property Parent: PNode read GetParent;
      property Right: PNode read fRight;
      property Color: TNodeColor read GetColor;
      property Key: T read fKey;

      property InOrder: TEnumerable index tmInOrder read GetEnumerable;
      property PreOrder: TEnumerable index tmPreOrder read GetEnumerable;
      property PostOrder: TEnumerable index tmPostOrder read GetEnumerable;
    end;
  public type
    TRedBlackTreeNode = TNode;
    PRedBlackTreeNode = PNode;
  end;

  TNodes<TKey, TValue> = record
  strict private type
    PNode = ^TNode;

    TKeyEnumerator = record
    private
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: TKey; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean; inline;
      property Current: TKey read GetCurrent;
    end;

    TKeyEnumerable = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function GetEnumerator: TKeyEnumerator; inline;
    end;

    TValueEnumerator = record
    private
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: TValue; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean; inline;
      property Current: TValue read GetCurrent;
    end;

    TValueEnumerable = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function GetEnumerator: TValueEnumerator; inline;
    end;

    TEnumerator = record
    private
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: PNode; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean; inline;
      property Current: PNode read GetCurrent;
    end;

    TEnumerable = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
      function GetKeys: TKeyEnumerable; inline;
      function GetValues: TValueEnumerable; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function GetEnumerator: TEnumerator; inline;
      property Keys: TKeyEnumerable read GetKeys;
      property Values: TValueEnumerable read GetValues;
    end;

    TNode = record
    strict private
      function GetLeftMost: PNode; inline;
      function GetRightMost: PNode; inline;
      function GetNext: PNode; inline;
      function GetPrev: PNode; inline;

      function GetColor: TNodeColor; inline;
      function GetParent: PNode; inline;
    private
      fParent, fLeft, fRight: PNode;
      fKey: TKey;
      fValue: TValue;
      procedure SetParent(const value: PNode); inline;
      function GetEnumerable(mode: TTraverseMode): TEnumerable; inline;
    public
      function GetEnumerator: TEnumerator; inline;

      property LeftMost: PNode read GetLeftMost;
      property RightMost: PNode read GetRightMost;
      property Next: PNode read GetNext;
      property Prev: PNode read GetPrev;

      property Left: PNode read fLeft;
      property Parent: PNode read GetParent;
      property Right: PNode read fRight;
      property Color: TNodeColor read GetColor;
      property Key: TKey read fKey;
      property Value: TValue read fValue write fValue;

      property InOrder: TEnumerable index tmInOrder read GetEnumerable;
      property PreOrder: TEnumerable index tmPreOrder read GetEnumerable;
      property PostOrder: TEnumerable index tmPostOrder read GetEnumerable;
    end;
  public type
    TRedBlackTreeNode = TNode;
    PRedBlackTreeNode = PNode;
  end;

  IBinaryTree = interface
    ['{2A6DBEEA-FFBA-40DB-9274-5057238CDFAB}']
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetHeight: Integer;
    function GetRoot: PBinaryTreeNode;
  {$ENDREGION}

    property Count: Integer read GetCount;
    property Height: Integer read GetHeight;
    property Root: PBinaryTreeNode read GetRoot;
  end;

  IBinaryTree<T> = interface(IBinaryTree)
    ['{06E837A5-29B7-4F33-AC5C-46BC82F00D15}']
  {$REGION 'Property Accessors'}
    function GetRoot: TNodes<T>.PRedBlackTreeNode;
  {$ENDREGION}

    function Add(const key: T): Boolean;
    function Delete(const key: T): Boolean;
    function Exists(const key: T): Boolean;
    function Find(const key: T; var value: T): Boolean;
    procedure Clear;

    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;

    property Root: TNodes<T>.PRedBlackTreeNode read GetRoot;
  end;

  IBinaryTree<TKey, TValue> = interface(IBinaryTree)
    ['{7F554520-BD51-4B53-953B-61B43ED6D59E}']
  {$REGION 'Property Accessors'}
    function GetRoot: TNodes<TKey, TValue>.PRedBlackTreeNode;
  {$ENDREGION}

    function Add(const key: TKey; const value: TValue): Boolean;
    function AddOrSet(const key: TKey; const value: TValue): Boolean;
    function Delete(const key: TKey): Boolean;
    function Exists(const key: TKey): Boolean;
    function Find(const key: TKey; var foundValue: TValue): Boolean;
    procedure Clear;

    function GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
    function ToArray: TArray<TPair<TKey,TValue>>;

    property Root: TNodes<TKey, TValue>.PRedBlackTreeNode read GetRoot;
  end;

  IRedBlackTree<T> = interface(IBinaryTree<T>)
    ['{59BB2B37-D85F-4092-8E80-1EFEE1D2E8F8}']
    function FindNode(const value: T): TNodes<T>.PRedBlackTreeNode;
    procedure DeleteNode(node: TNodes<T>.PRedBlackTreeNode);
  end;

  IRedBlackTree<TKey, TValue> = interface(IBinaryTree<TKey, TValue>)
    ['{8C6F6C1A-92C1-4F4A-A1A9-DD5EA70921CB}']
    function FindNode(const key: TKey): TNodes<TKey, TValue>.PRedBlackTreeNode;
    procedure DeleteNode(node: TNodes<TKey, TValue>.PRedBlackTreeNode);
  end;

  TRedBlackTree<T> = class(TRedBlackTree, IInterface, IBinaryTree<T>, IRedBlackTree<T>)
  private type
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;

    TNode = TNodes<T>.TRedBlackTreeNode;
    PNode = TNodes<T>.PRedBlackTreeNode;
  private
    fStorage: TBlockAllocatedArray<TNode>;
    function GetCapacity: Integer;
    procedure SetCapacity(value: Integer);
    procedure DestroyNode(node: PNode);
    function GetRoot: TNodes<T>.PRedBlackTreeNode; overload;
  protected
    fComparer: IComparer<T>;
    function CreateNode(const key: T): PNode;
    function FindNode(const key: T): PNode;
    procedure DeleteNode(node: PNode);
    procedure FreeNode(node: Pointer); override;
  public
    constructor Create; overload;
    constructor Create(const comparer: IComparer<T>); overload;

    procedure Clear; override;

    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;

  {$REGION 'Implements IBinaryTree<T>'}
    function Add(const key: T): Boolean;
    function Delete(const key: T): Boolean;
    function Exists(const key: T): Boolean;
    function Find(const key: T; var value: T): Boolean;
  {$ENDREGION}

    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Root: PNode read GetRoot;
  end;

  TRedBlackTree<TKey, TValue> = class(TRedBlackTree, IInterface,
    IBinaryTree<TKey, TValue>, IRedBlackTree<TKey, TValue>)
  private type
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: TPair<TKey,TValue>;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;

    TNode = TNodes<TKey, TValue>.TRedBlackTreeNode;
    PNode = TNodes<TKey, TValue>.PRedBlackTreeNode;
  private
    fStorage: TBlockAllocatedArray<TNode>;
    function InternalAdd(const key: TKey; const value: TValue; allowReplace: Boolean): Boolean;
    function GetCapacity: Integer;
    procedure SetCapacity(value: Integer);
    procedure DestroyNode(node: PNode);
    function GetRoot: PNode; overload; inline;
  protected
    fComparer: IComparer<TKey>;
    function CreateNode(const key: TKey; const value: TValue): PNode;
    function FindNode(const key: TKey): PNode;
    procedure DeleteNode(node: PNode);
    procedure FreeNode(node: Pointer); override;
  public
    constructor Create; overload;
    constructor Create(const comparer: IComparer<TKey>); overload;

    procedure Clear; override;

    function GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
    function ToArray: TArray<TPair<TKey,TValue>>;

  {$REGION 'Implements IBinaryTree<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean;
    function AddOrSet(const key: TKey; const value: TValue): Boolean;
    function Delete(const key: TKey): Boolean;
    function Exists(const key: TKey): Boolean;
    function Find(const key: TKey; var foundValue: TValue): Boolean;
  {$ENDREGION}

    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Comparer: IComparer<TKey> read fComparer;
    property Count: Integer read GetCount;
    property Root: PNode read GetRoot;
  end;

implementation

uses
  Math,
  Spring.Comparers;


{$REGION 'TBlockAllocatedArray<T>'}

function TBlockAllocatedArray<T>.GetCapacity: Integer;
begin
  Result := DynArrayLength(fItems) * BlockSize;
end;

function TBlockAllocatedArray<T>.GetItem(index: Integer): PT;
begin
  Result := @fItems[index shr BlockSizeBits, index and (BlockSize - 1)];
end;

procedure TBlockAllocatedArray<T>.Grow;
var
  n: Integer;
begin
  n := DynArrayLength(fItems);
  SetLength(fItems, n + 1);
  SetLength(fItems[n], BlockSize);
end;

procedure TBlockAllocatedArray<T>.SetCapacity(const value: Integer);
var
  oldLength, newLength, i: Integer;
begin
  oldLength := DynArrayLength(fItems);
  newLength := value shr BlockSizeBits;
  if value and (BlockSize - 1) > 0 then
    Inc(newLength);
  SetLength(fItems, newLength);
  for i := oldLength to newLength - 1 do
    SetLength(fItems[i], BlockSize);
end;

{$ENDREGION}


{$REGION 'TBinaryTree'}

function TBinaryTree.GetCount: Integer;
begin
  Result := fCount;
end;

function TBinaryTree.GetHeight: Integer;
begin
  Result := PBinaryTreeNode(fRoot).Height;
end;

function TBinaryTree.GetRoot: PBinaryTreeNode;
begin
  Result := fRoot;
end;

{$ENDREGION}


{$REGION 'TBinaryTree.TNode'}

function TBinaryTree.TNode.GetParent: PNode;
begin
  Result := Pointer(IntPtr(fParent) and PointerMask);
end;

function TBinaryTree.TNode.GetHeight: Integer;
begin
  if Assigned(@Self) then
    Result := Max(fLeft.Height, fRight.Height) + 1
  else
    Result := 0;
end;

function TBinaryTree.TNode.GetLeftMost: PNode;
begin
  Result := @Self;
  if Result = nil then Exit;
  while Assigned(Result.Left) do
    Result := Result.Left;
end;

function TBinaryTree.TNode.GetRightMost: PNode;
begin
  Result := @Self;
  if Result = nil then Exit;
  while Assigned(Result.Right) do
    Result := Result.Right;
end;

function TBinaryTree.TNode.GetNext: PNode;
var
  node: PNode;
begin
  if Assigned(Right) then
    Exit(Right.GetLeftMost);
  Result := Parent;
  node := @Self;
  while Assigned(Result) and (node = Result.Right) do
  begin
    node := Result;
    Result := Result.Parent;
  end;
end;

function TBinaryTree.TNode.GetPrev: PNode;
var
  node: PNode;
begin
  if Assigned(Left) then
    Exit(Left.GetRightMost);
  Result := Parent;
  node := @Self;
  while Assigned(Result) and (node = Result.Left) do
  begin
    node := Result;
    Result := Result.Parent;
  end;
end;

{$ENDREGION}


{$REGION 'TBinaryTree.TEnumerator'}

constructor TBinaryTree.TEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fCurrent := nil;
  fMode := mode;
end;

function TBinaryTree.TEnumerator.MoveNext: Boolean;
begin
  if fMode = tmInOrder then
    Result := MoveNextInOrder
  else if fMode = tmPreOrder then
    Result := MoveNextPreOrder
  else
    Result := MoveNextPostOrder;
end;

function TBinaryTree.TEnumerator.MoveNextInOrder: Boolean;
begin
  if not Assigned(fCurrent) then
    fCurrent := fRoot.LeftMost
  else
    fCurrent := fCurrent.Next;

  Result := Assigned(fCurrent);
  if not Result then
    fRoot := nil;
end;

function TBinaryTree.TEnumerator.MoveNextPreOrder: Boolean;
var
  sibling: PBinaryTreeNode;
begin
  if not Assigned(fCurrent) then
    fCurrent := fRoot
  else if Assigned(fCurrent.Left) then // walk down left
    fCurrent := fCurrent.Left
  else if Assigned(fCurrent.Right) then // walk down right
    fCurrent := fCurrent.Right
  else
  begin
    while Assigned(fCurrent.Parent) and (fCurrent <> fRoot) do // walk up ...
    begin
      sibling := fCurrent.Parent.Right;
      if Assigned(sibling) and (sibling <> fCurrent) then // ... and down right
      begin
        fCurrent := sibling;
        Exit(True);
      end;
      fCurrent := fCurrent.Parent;
    end;
    fCurrent := nil;
  end;

  Result := Assigned(fCurrent);
  if not Result then
    fRoot := nil;
end;

function TBinaryTree.TEnumerator.MoveNextPostOrder: Boolean;
var
  sibling: PBinaryTreeNode;
begin
  if not Assigned(fCurrent) then
    fCurrent := fRoot
  else if Assigned(fCurrent.Parent) and (fCurrent <> fRoot) then // walk up ...
  begin
    sibling := fCurrent.Parent.Right;
    if Assigned(sibling) and (sibling <> fCurrent) then // ... and down right
      fCurrent := sibling
    else
    begin
      fCurrent := fCurrent.Parent;
      Exit(True);
    end;
  end
  else
    fCurrent := nil;

  while Assigned(fCurrent) do // walk down to leftmost leaf
    if Assigned(fCurrent.Left) then
      fCurrent := fCurrent.Left
    else if Assigned(fCurrent.Right) then
      fCurrent := fCurrent.Right
    else
      Break;

  Result := Assigned(fCurrent);
  if not Result then
    fRoot := nil;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree'}

destructor TRedBlackTree.Destroy;
begin
  Clear;
end;

procedure TRedBlackTree.Clear;
begin
  fRoot := nil;
  fCount := 0;
end;

procedure TRedBlackTree.Delete(node: PNode);
var
  child: PNode;
begin
  try
    if Assigned(node.Left) then
      child := node.Left
    else
      child := node.Right;

    // node has a child
    if Assigned(child) then
    begin
      if not Assigned(node.Parent) then
        Root := child
      else if node.Parent.Left = node then
        node.Parent.Left := child
      else
        node.Parent.Right := child;

      if node.IsBlack then
        FixupAfterDelete(child);
    end else
    // node is the root
    if not Assigned(node.Parent) then
      fRoot := nil
    else
    begin
      if node.IsBlack then
        FixupAfterDelete(node);
      // unlink the node which could not be done before
      // because DeleteFixUp requires Parent to be set
      node.SetParent(nil);
    end;
  finally
    FreeNode(node);
  end;
end;

procedure TRedBlackTree.FixupAfterDelete(node: PNode);
var
  sibling: PNode;
begin
  while (node <> fRoot) and node.IsBlack do
  begin
    // node is a left child
    if node = node.Parent.Left then
    begin
      sibling := node.Parent.Right;

      // case 1: sibling is red
      if not sibling.IsBlack then
      begin
        sibling.Color := Black;
        node.Parent.Color := Red;
        RotateLeft(node.Parent);
        sibling := node.Parent.Right;
      end;

      // case 2: both of siblings children are black
      if sibling.Left.IsBlack and sibling.Right.IsBlack then
      begin
        sibling.Color := Red;
        node := node.Parent;
      end else
      begin
        // case 3: siblings right child is black
        if sibling.Right.IsBlack then
        begin
          sibling.Left.Color := Black;
          sibling.Color := Red;
          RotateRight(sibling);
          sibling := node.Parent.Right;
        end;

        // case 4: siblings right child is red
        sibling.Color := node.Parent.Color;
        node.Parent.Color := Black;
        sibling.Right.Color := Black;
        RotateLeft(node.Parent);
        node := fRoot;
      end;
    end else
    // node is a right child
    begin
      sibling := node.Parent.Left;

      // case 1: sibling is red
      if not sibling.IsBlack then
      begin
        sibling.Color := Black;
        node.Parent.Color := Red;
        RotateRight(node.Parent);
        sibling := node.Parent.Left;
      end;

      // case 2: both of siblings children are black
      if sibling.Right.IsBlack and sibling.Left.IsBlack then
      begin
        sibling.Color := Red;
        node := node.Parent;
      end else
      begin
        // case 3: siblings left child is black
        if sibling.Left.IsBlack then
        begin
          sibling.Right.Color := Black;
          sibling.Color := Red;
          RotateLeft(sibling);
          sibling := node.Parent.Left;
        end;

        // case 4: siblings left child is red
        sibling.Color := node.Parent.Color;
        node.Parent.Color := Black;
        sibling.Left.Color := Black;
        RotateRight(node.Parent);
        node := fRoot;
      end;
    end;
  end;

  node.Color := Black;
end;

procedure TRedBlackTree.FixupAfterInsert(node: PNode);
var
  uncle: PNode;
begin
  node.Color := Red;

  while Assigned(node) and (node <> fRoot) do
  begin
    if node.Parent.IsBlack then
      Break;

    // if nodes parent is the left child of its parent
    if node.Parent = node.Parent.Parent.Left then
    begin
      uncle := node.Parent.Parent.Right;

      // case 1: uncle is red
      if not uncle.IsBlack then
      begin
        node.Parent.Color := Black;
        uncle.Color := Black;
        node.Parent.Parent.Color := Red;
        node := node.Parent.Parent;
      end else
      // case 2: uncle is black and node is a right child
      if node = node.Parent.Right then
      begin
        node := node.Parent;
        RotateLeft(node);
      end else
      // case 3: uncle is black and node is a left child
      begin
        node.Parent.Color := Black;
        node.Parent.Parent.Color := Red;
        RotateRight(node.Parent.Parent);
      end;
    end else
    // if nodes parent is the right child of its parent
    begin
      uncle := node.Parent.Parent.Left;

      // case 1: uncle is red
      if not uncle.IsBlack then
      begin
        node.Parent.Color := Black;
        uncle.Color := Black;
        node.Parent.Parent.Color := Red;
        node := node.Parent.Parent;
      end else
      // case 2: uncle is black and node is a left child
      if node = node.Parent.Left then
      begin
        node := node.Parent;
        RotateRight(node);
      end else
      // case 3: uncle is black and node is right child
      begin
        node.Parent.Color := Black;
        node.Parent.Parent.Color := Red;
        RotateLeft(node.Parent.Parent);
      end
    end
  end;

  PNode(fRoot).Color := Black;
end;

procedure TRedBlackTree.InsertLeft(node, newNode: PNode);
begin
  Assert(not Assigned(node.Left));
  node.Left := newNode;
  FixupAfterInsert(newNode);
end;

procedure TRedBlackTree.InsertRight(node, newNode: PNode);
begin
  Assert(not Assigned(node.Right));
  node.Right := newNode;
  FixupAfterInsert(newNode);
end;

procedure TRedBlackTree.RotateLeft(node: PNode);
var
  right: PRedBlackTreeNode;
begin
  right := node.Right;
  node.Right := right.Left;

  if not Assigned(node.Parent) then
    Root := right
  else if node.Parent.Left = node then
    node.Parent.Left := right
  else
    node.Parent.Right := right;

  right.Left := node;
end;

procedure TRedBlackTree.RotateRight(node: PNode);
var
  left: PRedBlackTreeNode;
begin
  left := node.Left;
  node.Left := left.Right;

  if not Assigned(node.Parent) then
    Root := left
  else if node.Parent.Right = node then
    node.Parent.Right := left
  else
    node.Parent.Left := left;

  left.Right := node;
end;

procedure TRedBlackTree.SetRoot(value: PRedBlackTreeNode);
begin
  fRoot := value;
  if Assigned(value) then
    value.SetParent(nil);
end;

{$ENDREGION}


{$REGION 'TRedBlackTree.TNode'}

procedure TRedBlackTree.TNode.ClearParent;
var
  parent: PNode;
begin
  parent := GetParent;
  if Assigned(parent) then
    if parent.fLeft = @Self then
      parent.fLeft := nil
    else if parent.fRight = @Self then
      parent.fRight := nil;
  fParent := nil;
end;

function TRedBlackTree.TNode.GetColor: TNodeColor;
begin
  Result := TNodeColor(IntPtr(fParent) and ColorMask);
end;

function TRedBlackTree.TNode.GetIsBlack: Boolean;
begin
  Result := (@Self = nil) or not Odd(IntPtr(fParent));
end;

function TRedBlackTree.TNode.GetParent: PNode;
begin
  Result := Pointer(IntPtr(fParent) and PointerMask);
end;

procedure TRedBlackTree.TNode.SetColor(const value: TNodeColor);
begin
  IntPtr(fParent) := IntPtr(fParent) and PointerMask or Byte(value);
end;

procedure TRedBlackTree.TNode.SetParent(const value: PNode);
begin
  if Assigned(value) then
    IntPtr(fParent) := IntPtr(value) or Byte(Color)
  else
    ClearParent;
end;

procedure TRedBlackTree.TNode.SetLeft(const value: PNode);
begin
  fLeft := value;
  if Assigned(value) then
    value.SetParent(@Self);
end;

procedure TRedBlackTree.TNode.SetRight(const value: PNode);
begin
  fRight := value;
  if Assigned(value) then
    value.SetParent(@Self);
end;

{$ENDREGION}


{$REGION 'TNodes<T>.TNode'}

function TNodes<T>.TNode.GetColor: TNodeColor;
begin
  Result := TNodeColor(IntPtr(fParent) and ColorMask);
end;

function TNodes<T>.TNode.GetEnumerable(mode: TTraverseMode): TEnumerable;
begin
  Result := TEnumerable.Create(@Self, mode);
end;

function TNodes<T>.TNode.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(@Self, tmInOrder);
end;

function TNodes<T>.TNode.GetLeftMost: PNode;
begin
  Result := PNode(PBinaryTreeNode(@Self).LeftMost);
end;

function TNodes<T>.TNode.GetNext: PNode;
begin
  Result := PNode(PBinaryTreeNode(@Self).Next);
end;

function TNodes<T>.TNode.GetParent: PNode;
begin
  IntPtr(Result) := IntPtr(fParent) and PointerMask;
end;

function TNodes<T>.TNode.GetPrev: PNode;
begin
  Result := PNode(PBinaryTreeNode(@Self).Prev);
end;

function TNodes<T>.TNode.GetRightMost: PNode;
begin
  Result := PNode(PBinaryTreeNode(@Self).RightMost);
end;

procedure TNodes<T>.TNode.SetParent(
  const value: PNode);
begin
  TRedBlackTree.PNode(@Self).SetParent(TRedBlackTree.PNode(value));
end;

{$ENDREGION}


{$REGION 'TNodes<T>.TKeyEnumerator'}

constructor TNodes<T>.TKeyEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fEnumerator := TBinaryTree.TEnumerator.Create(Pointer(root), mode);
end;

function TNodes<T>.TKeyEnumerator.GetCurrent: T;
begin
  Result := PNode(fEnumerator.fCurrent).Key;
end;

function TNodes<T>.TKeyEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TNodes<T>.TKeyEnumerable'}

constructor TNodes<T>.TKeyEnumerable.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fMode := mode;
end;

function TNodes<T>.TKeyEnumerable.GetEnumerator: TKeyEnumerator;
begin
  Result := TKeyEnumerator.Create(fRoot, fMode);
end;

{$ENDREGION}


{$REGION 'TNodes<T>.TEnumerator'}

constructor TNodes<T>.TEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fEnumerator := TBinaryTree.TEnumerator.Create(Pointer(root), mode);
end;

function TNodes<T>.TEnumerator.GetCurrent: PNode;
begin
  Result := PNode(fEnumerator.fCurrent);
end;

function TNodes<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TNodes<T>.TEnumerable'}

constructor TNodes<T>.TEnumerable.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fMode := mode;
end;

function TNodes<T>.TEnumerable.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(fRoot, fMode);
end;

function TNodes<T>.TEnumerable.GetKeys: TKeyEnumerable;
begin
  Result := TKeyEnumerable.Create(fRoot, fMode);
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TNode'}

function TNodes<TKey, TValue>.TNode.GetColor: TNodeColor;
begin
  Result := TNodeColor(IntPtr(fParent) and ColorMask);
end;

function TNodes<TKey, TValue>.TNode.GetEnumerable(
  mode: TTraverseMode): TEnumerable;
begin
  Result := TEnumerable.Create(@Self, mode);
end;

function TNodes<TKey, TValue>.TNode.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(@Self, tmInOrder);
end;

function TNodes<TKey, TValue>.TNode.GetLeftMost: PNode;
begin
  Result := PNode(PBinaryTreeNode(@Self).LeftMost);
end;

function TNodes<TKey, TValue>.TNode.GetNext: PNode;
begin
  Result := PNode(PBinaryTreeNode(@Self).Next);
end;

function TNodes<TKey, TValue>.TNode.GetParent: PNode;
begin
  IntPtr(Result) := IntPtr(fParent) and PointerMask;
end;

function TNodes<TKey, TValue>.TNode.GetPrev: PNode;
begin
  Result := PNode(PBinaryTreeNode(@Self).Prev);
end;

function TNodes<TKey, TValue>.TNode.GetRightMost: PNode;
begin
  Result := PNode(PBinaryTreeNode(@Self).RightMost);
end;

procedure TNodes<TKey, TValue>.TNode.SetParent(const value: PNode);
begin
  TRedBlackTree.PNode(@Self).SetParent(TRedBlackTree.PNode(value));
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TKeyEnumerator'}

constructor TNodes<TKey, TValue>.TKeyEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fEnumerator := TBinaryTree.TEnumerator.Create(Pointer(root), mode);
end;

function TNodes<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := PNode(fEnumerator.fCurrent).Key;
end;

function TNodes<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TKeyEnumerable'}

constructor TNodes<TKey, TValue>.TKeyEnumerable.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fMode := mode;
end;

function TNodes<TKey, TValue>.TKeyEnumerable.GetEnumerator: TKeyEnumerator;
begin
  Result := TKeyEnumerator.Create(fRoot, fMode);
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TValueEnumerator'}

constructor TNodes<TKey, TValue>.TValueEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fEnumerator := TBinaryTree.TEnumerator.Create(Pointer(root), mode);
end;

function TNodes<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := PNode(fEnumerator.fCurrent).Value;
end;

function TNodes<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TValueEnumerable'}

constructor TNodes<TKey, TValue>.TValueEnumerable.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fMode := mode;
end;

function TNodes<TKey, TValue>.TValueEnumerable.GetEnumerator: TValueEnumerator;
begin
  Result := TValueEnumerator.Create(fRoot, fMode);
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TEnumerator'}

constructor TNodes<TKey, TValue>.TEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fEnumerator := TBinaryTree.TEnumerator.Create(Pointer(root), mode);
end;

function TNodes<TKey, TValue>.TEnumerator.GetCurrent: PNode;
begin
  Result := PNode(fEnumerator.fCurrent);
end;

function TNodes<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TEnumerable'}

constructor TNodes<TKey, TValue>.TEnumerable.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fMode := mode;
end;

function TNodes<TKey, TValue>.TEnumerable.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(fRoot, fMode);
end;

function TNodes<TKey, TValue>.TEnumerable.GetKeys: TKeyEnumerable;
begin
  Result := TKeyEnumerable.Create(fRoot, fMode);
end;

function TNodes<TKey, TValue>.TEnumerable.GetValues: TValueEnumerable;
begin
  Result := TValueEnumerable.Create(fRoot, fMode);
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<T>'}

constructor TRedBlackTree<T>.Create;
begin
  Create(nil);
end;

constructor TRedBlackTree<T>.Create(const comparer: IComparer<T>);
begin
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := IComparer<T>(_LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T)));
end;

function TRedBlackTree<T>.CreateNode(const key: T): PNode;
begin
  if fCount = fStorage.Capacity then
    fStorage.Grow;

  Result := PNode(fStorage[fCount]);
  Result.fKey := key;
  Inc(fCount);
end;

procedure TRedBlackTree<T>.DestroyNode(node: PNode);
var
  lastNode: PNode;
begin
  if fCount > 1 then
  begin
    lastNode := PNode(fStorage[fCount - 1]);

    if lastNode <> node then
    begin
      node^ := lastNode^;
      if Assigned(node.fLeft) then
        node.fLeft.SetParent(node);
      if Assigned(node.fRight) then
        node.fRight.SetParent(node);
      if Assigned(node.Parent) then
      begin
        if node.Parent.fLeft = lastNode then
          node.Parent.fLeft := node
        else if node.Parent.fRight = lastNode then
          node.Parent.fRight := node
      end
      else
        fRoot := node;
    end;

    node := lastNode;
  end;
  Dec(fCount);
  node.fLeft := nil;
  node.fParent := nil;
  node.fRight := nil;
  node.fKey := Default(T);
end;

procedure TRedBlackTree<T>.FreeNode(node: Pointer);
begin
  DestroyNode(PNode(node));
end;

function TRedBlackTree<T>.Add(const key: T): Boolean;
var
  node: PRedBlackTreeNode;
  compareResult: Integer;
begin
  if not Assigned(fRoot) then
  begin
    fRoot := CreateNode(key);
    Exit(True);
  end;

  node := fRoot;
  while True do
  begin
    compareResult := fComparer.Compare(key, PNode(node).Key);

    if compareResult > 0 then
      if Assigned(node.Right) then
        node := node.Right
      else
      begin
        InsertRight(node, PRedBlackTreeNode(CreateNode(key)));
        Exit(True);
      end
    else if compareResult < 0 then
      if Assigned(node.Left) then
        node := node.Left
      else
      begin
        InsertLeft(node, PRedBlackTreeNode(CreateNode(key)));
        Exit(True);
      end
    else
      Exit(False);
  end;
end;

procedure TRedBlackTree<T>.Clear;
begin
  inherited Clear;
  fStorage.Capacity := 0;
end;

function TRedBlackTree<T>.Delete(const key: T): Boolean;
var
  node: PNode;
begin
  if fCount = 0 then
    Exit(False);
  node := FindNode(key);
  Result := Assigned(node);
  if Result then
    DeleteNode(node);
end;

procedure TRedBlackTree<T>.DeleteNode(node: PNode);
var
  next: PNode;
begin
  if Assigned(node.Left) and Assigned(node.Right) then
  begin
    next := node.Next;
    node.fKey := next.Key;
    node := next;
  end;
  inherited Delete(PRedBlackTreeNode(node));
end;

function TRedBlackTree<T>.Exists(const key: T): Boolean;
var
  node: PNode;
begin
  node := FindNode(key);
  Result := Assigned(node);
end;

function TRedBlackTree<T>.Find(const key: T; var value: T): Boolean;
var
  node: PNode;
begin
  if fCount = 0 then
    Exit(False);
  node := FindNode(key);
  Result := Assigned(node);
  if Result then
    value := node.Key
  else
    value := Default(T);
end;

function TRedBlackTree<T>.FindNode(const key: T): PNode;
var
  compareResult: Integer;
begin
  Result := PNode(fRoot);
  while Assigned(Result) do
  begin
    compareResult := fComparer.Compare(key, Result.Key);

    if compareResult < 0 then
      Result := PNode(Result.Left)
    else if compareResult > 0 then
      Result := PNode(Result.Right)
    else
      Exit;
  end;
end;

function TRedBlackTree<T>.GetCapacity: Integer;
begin
  Result := fStorage.Capacity;
end;

function TRedBlackTree<T>.GetEnumerator: IEnumerator<T>;
begin
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
    fEnumerator := TBinaryTree.TEnumerator.Create(fRoot, tmInOrder);
end;

function TRedBlackTree<T>.GetRoot: TNodes<T>.PRedBlackTreeNode;
begin
  Result := fRoot;
end;

procedure TRedBlackTree<T>.SetCapacity(value: Integer);
begin
  if value < fCount then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.value, ExceptionResource.ArgumentOutOfRange_Capacity);

  fStorage.Capacity := value;
end;

function TRedBlackTree<T>.ToArray: TArray<T>;
var
  node: PNode;
  i: Integer;
begin
  SetLength(Result, fCount);
  if fCount > 0 then
    node := Root.LeftMost;
  for i := 0 to fCount - 1 do
  begin
    Result[i] := node.Key;
    node := node.Next;
  end;
end;

procedure TRedBlackTree<T>.TrimExcess;
begin
  SetCapacity(fCount);
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<T>.TEnumerator'}

function TRedBlackTree<T>.TEnumerator.GetCurrent: T;
begin
  Result := PNode(fEnumerator.fCurrent).Key;
end;

function TRedBlackTree<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<TKey, TValue>'}

constructor TRedBlackTree<TKey, TValue>.Create;
begin
  Create(nil);
end;

constructor TRedBlackTree<TKey, TValue>.Create(const comparer: IComparer<TKey>);
begin
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := IComparer<TKey>(_LookupVtableInfo(giComparer, TypeInfo(TKey), SizeOf(TKey)));
end;

function TRedBlackTree<TKey, TValue>.CreateNode(const key: TKey;
  const value: TValue): PNode;
begin
  if fCount = fStorage.Capacity then
    fStorage.Grow;

  Result := PNode(fStorage[fCount]);
  Result.fKey := key;
  Result.fValue := value;
  Inc(fCount);
end;

procedure TRedBlackTree<TKey, TValue>.DestroyNode(node: PNode);
var
  lastNode: PNode;
begin
  if fCount > 1 then
  begin
    lastNode := PNode(fStorage[fCount - 1]);

    if lastNode <> node then
    begin
      node^ := lastNode^;
      if Assigned(node.fLeft) then
        node.fLeft.SetParent(node);
      if Assigned(node.fRight) then
        node.fRight.SetParent(node);
      if Assigned(node.Parent) then
      begin
        if node.Parent.fLeft = lastNode then
          node.Parent.fLeft := node
        else if node.Parent.fRight = lastNode then
          node.Parent.fRight := node
      end
      else
        fRoot := node;
    end;

    node := lastNode;
  end;
  Dec(fCount);
  node.fLeft := nil;
  node.fParent := nil;
  node.fRight := nil;
  node.fKey := Default(TKey);
  node.fValue := Default(TValue);
end;

procedure TRedBlackTree<TKey, TValue>.FreeNode(node: Pointer);
begin
  DestroyNode(PNode(node));
end;

function TRedBlackTree<TKey, TValue>.Add(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := InternalAdd(key, value, False);
end;

function TRedBlackTree<TKey, TValue>.AddOrSet(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := InternalAdd(key, value, True);
end;

procedure TRedBlackTree<TKey, TValue>.Clear;
begin
  inherited Clear;
  fStorage.Capacity := 0;
end;

function TRedBlackTree<TKey, TValue>.Delete(const key: TKey): Boolean;
var
  node: PNode;
begin
  if fCount = 0 then
    Exit(False);
  node := FindNode(key);
  Result := Assigned(node);
  if Result then
    DeleteNode(node);
end;

procedure TRedBlackTree<TKey, TValue>.DeleteNode(node: PNode);
var
  next: PNode;
begin
  if Assigned(node.Left) and Assigned(node.Right) then
  begin
    next := node.Next;
    node.fKey := next.Key;
    node.fValue := next.Value;
    node := next;
  end;
  inherited Delete(PRedBlackTreeNode(node));
end;

function TRedBlackTree<TKey, TValue>.Exists(const key: TKey): Boolean;
var
  node: PNode;
begin
  node := FindNode(key);
  Result := Assigned(node);
end;

function TRedBlackTree<TKey, TValue>.Find(const key: TKey;
  var foundValue: TValue): Boolean;
var
  node: PNode;
begin
  if fCount = 0 then
    Exit(False);
  node := FindNode(key);
  Result := Assigned(node);
  if Result then
    foundValue := node.Value
  else
    foundValue := Default(TValue);
end;

function TRedBlackTree<TKey, TValue>.FindNode(const key: TKey): PNode;
var
  compareResult: Integer;
begin
  Result := PNode(fRoot);
  while Assigned(Result) do
  begin
    compareResult := fComparer.Compare(key, Result.Key);

    if compareResult < 0 then
      Result := PNode(Result.Left)
    else if compareResult > 0 then
      Result := PNode(Result.Right)
    else
      Exit;
  end;
end;

function TRedBlackTree<TKey, TValue>.GetCapacity: Integer;
begin
  Result := fStorage.Capacity;
end;

function TRedBlackTree<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
    fEnumerator := TBinaryTree.TEnumerator.Create(fRoot, tmInOrder);
end;

function TRedBlackTree<TKey, TValue>.GetRoot: PNode;
begin
  Result := fRoot;
end;

function TRedBlackTree<TKey, TValue>.InternalAdd(const key: TKey;
  const value: TValue; allowReplace: Boolean): Boolean;
var
  node: PRedBlackTreeNode;
  compareResult: Integer;
begin
  if not Assigned(fRoot) then
  begin
    fRoot := CreateNode(key, value);
    Exit(True);
  end;

  node := fRoot;
  while True do
  begin
    compareResult := fComparer.Compare(key, PNode(node).Key);

    if compareResult > 0 then
      if Assigned(node.Right) then
        node := node.Right
      else
      begin
        InsertRight(node, PRedBlackTreeNode(CreateNode(key, value)));
        Exit(True);
      end
    else if compareResult < 0 then
      if Assigned(node.Left) then
        node := node.Left
      else
      begin
        InsertLeft(node, PRedBlackTreeNode(CreateNode(key, value)));
        Exit(True);
      end
    else
      if allowReplace then
      begin
        PNode(node).Value := value;
        Exit(True);
      end
      else
        Exit(False);
  end;
end;

procedure TRedBlackTree<TKey, TValue>.SetCapacity(value: Integer);
begin
  if value < fCount then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.value, ExceptionResource.ArgumentOutOfRange_Capacity);

  fStorage.Capacity := value;
end;

function TRedBlackTree<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
var
  node: PNode;
  i: Integer;
begin
  SetLength(Result, fCount);
  if fCount > 0 then
    node := Root.LeftMost;
  for i := 0 to fCount - 1 do
  begin
    Result[i].Key := node.Key;
    Result[i].Value := node.Value;
    node := node.Next;
  end;
end;

procedure TRedBlackTree<TKey, TValue>.TrimExcess;
begin
  SetCapacity(fCount);
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<TKey, TValue>.TEnumerator'}

function TRedBlackTree<TKey, TValue>.TEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  Result.Key := PNode(fEnumerator.fCurrent).Key;
  Result.Value := PNode(fEnumerator.fCurrent).Value;
end;

function TRedBlackTree<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


end.
