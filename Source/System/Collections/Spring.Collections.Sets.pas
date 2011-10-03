unit Spring.Collections.Sets;

interface

{$I Spring.inc}

uses
  Generics.Defaults,
  Generics.Collections,
  Spring,
  Spring.Collections;

type
  THashSet<T> = class(TCollectionBase<T>, ISet<T>)
  private
    fDictionary: Generics.Collections.TDictionary<T,Integer>; // TEMP Impl
  protected
    function GetCount: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Add(const item: T); override;
    function  Remove(const item: T): Boolean; override;
    procedure Clear; override;

    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; override;
    procedure ExceptWith(const collection: IEnumerable<T>);
    procedure IntersectWith(const collection: IEnumerable<T>);
    procedure UnionWith(const collection: IEnumerable<T>);
    function SetEquals(const collection: IEnumerable<T>): Boolean;
    function Overlaps(const collection: IEnumerable<T>): Boolean;
  end;

implementation


{$REGION 'THashSet<T>'}

constructor THashSet<T>.Create;
begin
  inherited Create;
  fDictionary := Generics.Collections.TDictionary<T, Integer>.Create;
end;

destructor THashSet<T>.Destroy;
begin
  fDictionary.Free;
  inherited Destroy;
end;

procedure THashSet<T>.Add(const item: T);
begin
  fDictionary.AddOrSetValue(item, 0);
end;

//function THashSet<T>.Add(const item: T): Boolean;
//begin
//  Result := not fDictionary.ContainsKey(item);
//  if Result then
//  begin
//    fDictionary.Add(item, 0);
//  end;
//end;

function THashSet<T>.Remove(const item: T): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
  if Result then
    fDictionary.Remove(item);
end;

procedure THashSet<T>.Clear;
begin
  fDictionary.Clear;
end;

function THashSet<T>.Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
end;

procedure THashSet<T>.ExceptWith(const collection: IEnumerable<T>);
var
  item: T;
begin
  TArgument.CheckNotNull(collection, 'collection');

  for item in collection do
  begin
    fDictionary.Remove(item);
  end;
end;

procedure THashSet<T>.IntersectWith(const collection: IEnumerable<T>);
var
  item: T;
  list: IList<T>;
begin
  TArgument.CheckNotNull(collection, 'collection');

  list := TCollections.CreateList<T>;
  for item in Self do
  begin
    if not collection.Contains(item) then
      list.Add(item);
  end;

  for item in list do
  begin
    Remove(item);
  end;
end;

procedure THashSet<T>.UnionWith(const collection: IEnumerable<T>);
var
  item: T;
begin
  TArgument.CheckNotNull(collection, 'collection');

  for item in collection do
  begin
    Add(item);
  end;
end;

function THashSet<T>.Overlaps(const collection: IEnumerable<T>): Boolean;
var
  item: T;
begin
  TArgument.CheckNotNull(collection, 'collection');

  for item in collection do
  begin
    if Contains(item) then
      Exit(True)
  end;
  Result := False;
end;

function THashSet<T>.SetEquals(const collection: IEnumerable<T>): Boolean;
var
  item: T;
  localSet: ISet<T>;
begin
  TArgument.CheckNotNull(collection, 'collection');

  localSet := THashSet<T>.Create;

  for item in collection do
  begin
    localSet.Add(item);
    if not Contains(item) then
      Exit(False);
  end;

  for item in Self do
  begin
    if not localSet.Contains(item) then
      Exit(False);
  end;

  Result := True;
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fDictionary.Keys);
end;

function THashSet<T>.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}

end.
