{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2019 Spring4D Team                           }
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

unit Spring.Tests.Collections.Dictionaries;

{$I Spring.inc}

interface

uses
  Generics.Defaults,
  TestFramework,
  Spring.TestUtils,
  Spring,
  Spring.Collections,
  Spring.Tests.Collections;

type
  TTestDictionaryKeyComparerBase = class(TTestCase)
  private
    SUT: IDictionary<Integer, string>;
  protected
    class function CreateSUT(const keyComparer: IEqualityComparer<Integer>): IDictionary<Integer, string>; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestContains;
    procedure TestContainsKey;
    procedure TestExtract;
    procedure TestRemove;
  end;

  TTestDictionaryKeyComparer = class(TTestDictionaryKeyComparerBase)
  protected
    class function CreateSUT(const keyComparer: IEqualityComparer<Integer>): IDictionary<Integer, string>; override;
  end;

  TTestBidiDictionaryKeyComparer = class(TTestDictionaryKeyComparerBase)
  protected
    class function CreateSUT(const keyComparer: IEqualityComparer<Integer>): IDictionary<Integer, string>; override;
  end;

  TTestBidiDictionaryInverseKeyComparer = class(TTestDictionaryKeyComparerBase)
  protected
    class function CreateSUT(const keyComparer: IEqualityComparer<Integer>): IDictionary<Integer, string>; override;
  end;

  TTestDictionaryValueComparerBase = class(TTestCase)
  private
    SUT: IDictionary<string, Integer>;
  protected
    class function CreateSUT(const valueComparer: IEqualityComparer<Integer>): IDictionary<string, Integer>; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestContains;
    procedure TestContainsValue;
    procedure TestExtract;
    procedure TestRemove;
  end;

  TTestDictionaryValueComparer = class(TTestDictionaryValueComparerBase)
  protected
    class function CreateSUT(const valueComparer: IEqualityComparer<Integer>): IDictionary<string, Integer>; override;
  end;

  TTestBidiDictionaryValueComparer = class(TTestDictionaryValueComparerBase)
  protected
    class function CreateSUT(const valueComparer: IEqualityComparer<Integer>): IDictionary<string, Integer>; override;
  end;

  TTestBidiDictionaryInverseValueComparer = class(TTestDictionaryValueComparerBase)
  protected
    class function CreateSUT(const valueComparer: IEqualityComparer<Integer>): IDictionary<string, Integer>; override;
  end;

  TTestDictionaryBase = class(TTestCase)
  private
    procedure CheckCount(expected: Integer);
  protected
    SUT: IDictionary<Integer, string>;
    procedure TearDown; override;
    function IsSorted: Boolean; virtual;
    procedure FillTestData;
  published
    procedure TestAddDictionary;
    procedure TestAddKeyValue;
    procedure TestCollectionAdd;
    procedure TestCollectionExtract;
    procedure TestCollectionRemove;
    procedure TestContains;
    procedure TestContainsKey;
    procedure TestContainsValue;
    procedure TestElementAt;
    procedure TestEnumerableContains;
    procedure TestEnumeratorVersion;
    procedure TestExtract;
    procedure TestGetEnumerator;
    procedure TestGetItem;
    procedure TestGetValueOrDefault;
    procedure TestIsInitializedEmpty;
    procedure TestKeysContains;
    procedure TestKeysElementAt;
    procedure TestKeysEnumerate;
    procedure TestKeysEnumeratorVersion;
    procedure TestKeysGetEnumerator;
    procedure TestKeysReferenceCounting;
    procedure TestKeysToArray;
    procedure TestMapAdd;
    procedure TestMapExtract;
    procedure TestMapRemove;
    procedure TestOrdered;
    procedure TestOrdered_Issue179;
    procedure TestRemove;
    procedure TestSetItem;
    procedure TestSetItemOrder;
    procedure TestSetItem_Issue318;
    procedure TestTryAdd;
    procedure TestTryExtract;
    procedure TestTryGetElementAt;
    procedure TestTryUpdateValue;
    procedure TestToArray;
    procedure TestValuesContains;
    procedure TestValuesElementAt;
    procedure TestValuesEnumerate;
    procedure TestValuesEnumeratorVersion;
    procedure TestValuesGetEnumerator;
    procedure TestValuesReferenceCounting;
    procedure TestValuesToArray;
  end;

  TTestDictionary = class(TTestDictionaryBase)
  protected
    procedure SetUp; override;
  end;

  TTestSortedDictionary = class(TTestDictionaryBase)
  protected
    procedure SetUp; override;
    function IsSorted: Boolean; override;
  end;

  TTestBidiDictionaryBase = class(TTestDictionaryBase)
  protected
    SUTinverse: IBidiDictionary<string, Integer>;
    procedure TearDown; override;
  published
    procedure TestSetItemBidi;
    procedure TestSetItemBidiMultipleTimes;
  end;

  TTestBidiDictionary = class(TTestBidiDictionaryBase)
  protected
    procedure SetUp; override;
  end;

  TTestBidiDictionaryInverse = class(TTestBidiDictionaryBase)
  protected
    procedure SetUp; override;
  end;

  TTestDictionaryOwnershipBase = class(TTestCase)
  protected
    class function CreateOwnedKeysDict: IDictionary<TObject, Integer>; virtual; abstract;
    class function CreateOwnedValuesDict: IDictionary<Integer, TObject>; virtual; abstract;
  published
    procedure TestKeys;
    procedure TestValues;
  end;

  TTestDictionaryOwnership = class(TTestDictionaryOwnershipBase)
  protected
    class function CreateOwnedKeysDict: IDictionary<TObject, Integer>; override;
    class function CreateOwnedValuesDict: IDictionary<Integer, TObject>; override;
  end;

  TTestBidiDictionaryOwnership = class(TTestDictionaryOwnershipBase)
  protected
    class function CreateOwnedKeysDict: IDictionary<TObject, Integer>; override;
    class function CreateOwnedValuesDict: IDictionary<Integer, TObject>; override;
  end;

  TTestBidiDictionaryInverseOwnership = class(TTestDictionaryOwnershipBase)
  protected
    class function CreateOwnedKeysDict: IDictionary<TObject, Integer>; override;
    class function CreateOwnedValuesDict: IDictionary<Integer, TObject>; override;
  end;

  TTestDictionaryChangedEventBase = class(TTestMapChangedEventBase)
  private
    procedure AddEventHandlers;
  protected
    SUT: IDictionary<Integer, string>;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestClear;
    procedure TestDestroy;
    procedure TestExtract;
    procedure TestRemove;
    procedure TestSetItem;
    procedure TestTryAdd;
    procedure TestTryExtract;
    procedure TestTryUpdateValue;
  end;

  TTestDictionaryChangedEvent = class(TTestDictionaryChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestSortedDictionaryChangedEvent = class(TTestDictionaryChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestBidiDictionaryChangedEvent = class(TTestDictionaryChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestBidiDictionaryChangedEventInverse = class(TTestDictionaryChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestDictionaryCreationExceptions = class(TTestCase)
  published
    procedure TestCapacity;
    procedure TestKeyOwnership;
    procedure TestValueOwnership;
  end;

implementation

uses
  SysUtils;


{$REGION 'TTestDictionaryKeyComparerBase'}

procedure TTestDictionaryKeyComparerBase.SetUp;
var
  keyComparer: IEqualityComparer<Integer>;
begin
  inherited;
  keyComparer := TEqualityComparer<Integer>.Construct(
    function(const left, right: Integer): Boolean
    begin
      Result := left = -right;
    end,
    function(const Key: Integer): Integer
    begin
      Result := Abs(Key);
    end);
  SUT := CreateSUT(keyComparer);
  SUT.Add(1, 'one');
  SUT.Add(2, 'two');
  SUT.Add(3, 'three');
end;

procedure TTestDictionaryKeyComparerBase.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestDictionaryKeyComparerBase.TestContains;
begin
  CheckTrue(SUT.Contains(-1, 'one'));
  CheckFalse(SUT.Contains(-1, 'two'));
  CheckFalse(SUT.Contains(1, 'one'));
end;

procedure TTestDictionaryKeyComparerBase.TestContainsKey;
var
  Key: Integer;
begin
  for Key := -5 to 5 do begin
    CheckEquals((Key >= -3) and (Key <= -1), SUT.ContainsKey(Key));
  end;
end;

procedure TTestDictionaryKeyComparerBase.TestExtract;
var
  pair: TPair<Integer, string>;
begin
  pair := SUT.Extract(2, 'two');
  CheckEquals(pair.Key, Default(Integer));
  CheckEquals(pair.Value, Default(string));

  pair := SUT.Extract(-2, 'two');
  CheckEquals(pair.Key, 2);
  CheckEquals(pair.Value, 'two');

  pair := SUT.Extract(3, 'three');
  CheckEquals(pair.Key, Default(Integer));
  CheckEquals(pair.Value, Default(string));

  pair := SUT.Extract(-3, 'three');
  CheckEquals(pair.Key, 3);
  CheckEquals(pair.Value, 'three');
end;

procedure TTestDictionaryKeyComparerBase.TestRemove;
begin
  CheckFalse(SUT.Remove(2, 'two'));
  CheckTrue(SUT.Remove(-2, 'two'));
  CheckFalse(SUT.Remove(3, 'three'));
  CheckTrue(SUT.Remove(-3, 'three'));
end;

{$ENDREGION}


{$REGION 'TTestDictionaryKeyComparer'}

class function TTestDictionaryKeyComparer.CreateSUT(const keyComparer: IEqualityComparer<Integer>): IDictionary<Integer, string>;
begin
  Result := TCollections.CreateDictionary<Integer, string>(keyComparer, nil);
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionaryKeyComparer'}

class function TTestBidiDictionaryKeyComparer.CreateSUT(const keyComparer: IEqualityComparer<Integer>): IDictionary<Integer, string>;
begin
  Result := TCollections.CreateBidiDictionary<Integer, string>(keyComparer, nil);
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionaryInverseKeyComparer'}

class function TTestBidiDictionaryInverseKeyComparer.CreateSUT(const keyComparer: IEqualityComparer<Integer>): IDictionary<Integer, string>;
begin
  Result := TCollections.CreateBidiDictionary<string, Integer>(nil, keyComparer).Inverse;
end;

{$ENDREGION}


{$REGION 'TTestDictionaryValueComparerBase'}

procedure TTestDictionaryValueComparerBase.SetUp;
var
  valueComparer: IEqualityComparer<Integer>;
begin
  inherited;
  valueComparer := TEqualityComparer<Integer>.Construct(
    function(const left, right: Integer): Boolean
    begin
      Result := left = -right;
    end,
    function(const value: Integer): Integer
    begin
      Result := Abs(value);
    end);
  SUT := CreateSUT(valueComparer);
  SUT.Add('one', 1);
  SUT.Add('two', 2);
  SUT.Add('three', 3);
end;

procedure TTestDictionaryValueComparerBase.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestDictionaryValueComparerBase.TestContains;
begin
  CheckTrue(SUT.Contains('one', -1));
  CheckFalse(SUT.Contains('two', -1));
  CheckFalse(SUT.Contains('one', 1));
end;

procedure TTestDictionaryValueComparerBase.TestContainsValue;
var
  value: Integer;
begin
  for value := -5 to 5 do begin
    CheckEquals((value >= -3) and (value <= -1), SUT.ContainsValue(value));
  end;
end;

procedure TTestDictionaryValueComparerBase.TestExtract;
var
  pair: TPair<string, Integer>;
begin
  pair := SUT.Extract('two', 2);
  CheckEquals(pair.Key, Default(string));
  CheckEquals(pair.Value, Default(Integer));

  pair := SUT.Extract('two', -2);
  CheckEquals(pair.Key, 'two');
  CheckEquals(pair.Value, 2);

  pair := SUT.Extract('three', 3);
  CheckEquals(pair.Key, Default(string));
  CheckEquals(pair.Value, Default(Integer));

  pair := SUT.Extract('three', -3);
  CheckEquals(pair.Key, 'three');
  CheckEquals(pair.Value, 3);
end;

procedure TTestDictionaryValueComparerBase.TestRemove;
begin
  CheckFalse(SUT.Remove('two', 2));
  CheckTrue(SUT.Remove('two', -2));
  CheckFalse(SUT.Remove('three', 3));
  CheckTrue(SUT.Remove('three', -3));
end;

{$ENDREGION}


{$REGION 'TTestDictionaryValueComparer'}

class function TTestDictionaryValueComparer.CreateSUT(const valueComparer: IEqualityComparer<Integer>): IDictionary<string, Integer>;
begin
  Result := TCollections.CreateDictionary<string, Integer>(nil, valueComparer);
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionaryValueComparer'}

class function TTestBidiDictionaryValueComparer.CreateSUT(const valueComparer: IEqualityComparer<Integer>): IDictionary<string, Integer>;
begin
  Result := TCollections.CreateBidiDictionary<string, Integer>(nil, valueComparer);
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionaryInverseValueComparer'}

class function TTestBidiDictionaryInverseValueComparer.CreateSUT(const valueComparer: IEqualityComparer<Integer>): IDictionary<string, Integer>;
begin
  Result := TCollections.CreateBidiDictionary<Integer, string>(valueComparer, nil).Inverse;
end;

{$ENDREGION}


{$REGION 'TTestDictionaryBase'}

procedure TTestDictionaryBase.CheckCount(expected: Integer);
begin
  CheckEquals(expected, SUT.Count, 'Count');
  CheckEquals(expected, SUT.Keys.Count, 'Keys.Count');
  CheckEquals(expected, SUT.Values.Count, 'Values.Count');
end;

procedure TTestDictionaryBase.FillTestData;
begin
  if IsSorted then
  begin
    SUT.Add(3, 'c');
    SUT.Add(1, 'a');
    SUT.Add(4, 'd');
    SUT.Add(2, 'b');
  end
  else
  begin
    SUT.Add(1, 'a');
    SUT.Add(2, 'b');
    SUT.Add(3, 'c');
    SUT.Add(4, 'd');
  end;
end;

function TTestDictionaryBase.IsSorted: Boolean;
begin
  Result := False;
end;

procedure TTestDictionaryBase.TearDown;
begin
  SUT := nil;
  inherited;
end;

procedure TTestDictionaryBase.TestAddDictionary;
var
  dict: IDictionary<Integer, string>;
begin
  FillTestData;

  dict := TCollections.CreateDictionary<Integer, string>;
  dict.AddRange(SUT);
  CheckTrue(dict.EqualsTo(SUT));
end;

procedure TTestDictionaryBase.TestAddKeyValue;
begin
  FillTestData;

  CheckCount(4);
  CheckException(EArgumentException, procedure begin SUT.Add(1, 'e') end);
end;

procedure TTestDictionaryBase.TestCollectionAdd;
var
  pair: TPair<Integer, string>;
  coll: ICollection<TPair<Integer,string>>;
begin
  coll := SUT as ICollection<TPair<Integer, string>>;

  pair.Key := 1;
  pair.Value := 'a';
  Check(coll.Add(pair));
  Check(not coll.Add(pair));
  pair.Value := 'b';
  Check(not coll.Add(pair));
  pair.Key := 2;
  Check(coll.Add(pair));
  CheckEquals(2, coll.Count);
end;

procedure TTestDictionaryBase.TestCollectionExtract;
var
  pair: TPair<Integer, string>;
begin
  FillTestData;

  CheckEquals(4, SUT.Count);
  pair := (SUT as ICollection<TPair<Integer, string>>).Extract(TPair<Integer, string>.Create(2, 'a'));
  CheckEquals(4, SUT.Count);
  CheckEquals(Default(Integer), pair.Key);
  CheckEquals(Default(string), pair.Value);

  pair := (SUT as ICollection<TPair<Integer, string>>).Extract(TPair<Integer, string>.Create(1, 'a'));
  CheckEquals(3, SUT.Count);
  CheckEquals(1, pair.Key);
  CheckEquals('a', pair.Value);
  Check(not SUT.ContainsKey(1));
  Check(not SUT.ContainsValue('a'));
end;

procedure TTestDictionaryBase.TestCollectionRemove;
var
  pair: TPair<Integer, string>;
  coll: ICollection<TPair<Integer,string>>;
begin
  FillTestData;

  coll := SUT as ICollection<TPair<Integer, string>>;

  pair.Key := 1;
  pair.Value := 'a';
  Check(coll.Remove(pair));
  Check(not coll.Remove(pair));
  pair.Value := 'b';
  Check(not coll.Remove(pair));
  pair.Key := 2;
  Check(coll.Remove(pair));
  CheckEquals(2, coll.Count);
end;

procedure TTestDictionaryBase.TestContains;
begin
  FillTestData;

  Check(not SUT.Contains(0, 'a'));
  Check(not SUT.Contains(1, 'b'));
  Check(SUT.Contains(1, 'a'));
  Check(SUT.Contains(2, 'b'));
  Check(SUT.Contains(3, 'c'));
  Check(SUT.Contains(4, 'd'));
  Check(not SUT.Contains(5, 'e'));
end;

procedure TTestDictionaryBase.TestContainsKey;
begin
  FillTestData;

  Check(not SUT.ContainsKey(0));
  Check(SUT.ContainsKey(1));
  Check(SUT.ContainsKey(2));
  Check(SUT.ContainsKey(3));
  Check(SUT.ContainsKey(4));
  Check(not SUT.ContainsKey(5));
end;

procedure TTestDictionaryBase.TestContainsValue;
begin
  FillTestData;

  Check(not SUT.ContainsValue(''));
  Check(not SUT.ContainsValue('aa'));
  Check(SUT.ContainsValue('a'));
  Check(SUT.ContainsValue('b'));
  Check(SUT.ContainsValue('c'));
  Check(SUT.ContainsValue('d'));
  Check(not SUT.ContainsValue('e'));
end;

procedure TTestDictionaryBase.TestElementAt;
var
  i: Integer;
begin
  FillTestData;

  if IsSorted then
  begin
    for i := 0 to SUT.Count - 1 do
      CheckEquals(i + 1, SUT.ElementAt(i).Key);

    // remove and re-add, because this dict is sorted, the order must not change
    SUT.Remove(2);
    SUT.Add(2, 'b');
    for i := 0 to SUT.Count - 1 do
      CheckEquals(i + 1, SUT.ElementAt(i).Key);
  end
  else
  begin
    for i := 0 to SUT.Count - 1 do
      CheckEquals(i + 1, SUT.ElementAt(i).Key);

    // remove and re-add, ensure that we exercise code which compacts the item array
    SUT.Remove(2);
    SUT.Add(2, 'b');
    CheckEquals(1, SUT.ElementAt(0).Key);
    CheckEquals(2, SUT.ElementAt(3).Key);
    CheckEquals(3, SUT.ElementAt(1).Key);
    CheckEquals(4, SUT.ElementAt(2).Key);
  end;
end;

procedure TTestDictionaryBase.TestEnumerableContains;
var
  enumerable: IEnumerable<TPair<Integer, string>>;
begin
  FillTestData;

  enumerable := SUT as IEnumerable<TPair<Integer, string>>;
  Check(not enumerable.Contains(TPair<Integer, string>.Create(0, 'a')));
  Check(not enumerable.Contains(TPair<Integer, string>.Create(1, 'b')));
  Check(enumerable.Contains(TPair<Integer, string>.Create(1, 'a')));
  Check(enumerable.Contains(TPair<Integer, string>.Create(2, 'b')));
  Check(enumerable.Contains(TPair<Integer, string>.Create(3, 'c')));
  Check(enumerable.Contains(TPair<Integer, string>.Create(4, 'd')));
  Check(not enumerable.Contains(TPair<Integer, string>.Create(5, 'e')));
end;

procedure TTestDictionaryBase.TestEnumeratorVersion;
var
  e: IEnumerator<TPair<Integer, string>>;
begin
  FillTestData;

  e := SUT.GetEnumerator;
  e.MoveNext;
  SUT.Remove(1);
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.GetEnumerator;
  e.MoveNext;
  SUT.Add(1, 'a');
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.GetEnumerator;
  e.MoveNext;
  SUT.Extract(2);
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.GetEnumerator;
  e.MoveNext;
  SUT[3] := 'foo';
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.GetEnumerator;
  e.MoveNext;
  SUT.Clear;
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);
end;

procedure TTestDictionaryBase.TestExtract;
begin
  FillTestData;

  CheckEquals('', SUT.Extract(5));
  CheckCount(4);
  CheckEquals('d', SUT.Extract(4));
  CheckCount(3);
  CheckEquals('c', SUT.Extract(3));
  CheckCount(2);
end;

procedure TTestDictionaryBase.TestGetEnumerator;
var
  pair: TPair<Integer, string>;
  i: Integer;
begin
  FillTestData;

  i := 0;
  for pair in SUT do
  begin
    Inc(i);
    CheckEquals(i, pair.Key);
  end;
  CheckEquals(4, i);
end;

procedure TTestDictionaryBase.TestGetItem;
begin
  FillTestData;

  CheckException(EKeyNotFoundException, procedure begin SUT[0] end);
  CheckEquals('a', SUT[1]);
  CheckEquals('b', SUT[2]);
  CheckEquals('c', SUT[3]);
  CheckEquals('d', SUT[4]);
  CheckException(EKeyNotFoundException, procedure begin SUT[5] end);
end;

procedure TTestDictionaryBase.TestGetValueOrDefault;
begin
  FillTestData;

  CheckEquals('', SUT.GetValueOrDefault(0));
  CheckEquals('foo', SUT.GetValueOrDefault(0, 'foo'));
  CheckEquals('a', SUT.GetValueOrDefault(1));
  CheckEquals('a', SUT.GetValueOrDefault(1, 'foo'));
end;

procedure TTestDictionaryBase.TestIsInitializedEmpty;
begin
  CheckEquals(0, SUT.Count);
  CheckEquals(0, SUT.Keys.Count);
  CheckEquals(0, SUT.Values.Count);
end;

procedure TTestDictionaryBase.TestKeysContains;
begin
  FillTestData;

  Check(not SUT.Keys.Contains(0));
  Check(SUT.Keys.Contains(1));
  Check(SUT.Keys.Contains(2));
  Check(SUT.Keys.Contains(3));
  Check(SUT.Keys.Contains(4));
  Check(not SUT.Keys.Contains(5));
end;

procedure TTestDictionaryBase.TestKeysElementAt;
var
  i: Integer;
begin
  FillTestData;

  if IsSorted then
  begin
    for i := 0 to SUT.Keys.Count - 1 do
      CheckEquals(i + 1, SUT.Keys.ElementAt(i));

    // remove and re-add, because this dict is sorted, the order must not change
    SUT.Remove(2);
    SUT.Add(2, 'b');
    for i := 0 to SUT.Keys.Count - 1 do
      CheckEquals(i + 1, SUT.Keys.ElementAt(i));
  end
  else
  begin
    for i := 0 to SUT.Keys.Count - 1 do
      CheckEquals(i + 1, SUT.Keys.ElementAt(i));

    // remove and re-add, ensure that we exercise code which compacts the item array
    SUT.Remove(2);
    SUT.Add(2, 'b');
    CheckEquals(1, SUT.Keys.ElementAt(0));
    CheckEquals(2, SUT.Keys.ElementAt(3));
    CheckEquals(3, SUT.Keys.ElementAt(1));
    CheckEquals(4, SUT.Keys.ElementAt(2));
  end;
end;

procedure TTestDictionaryBase.TestKeysEnumerate;
var
  e: IEnumerator<Integer>;
begin
  FillTestData;

  Check(SUT.Keys.EqualsTo([1, 2, 3, 4]));

  e := SUT.Keys.GetEnumerator;
  SUT := nil;
  e.MoveNext;
  e.MoveNext;
  e.MoveNext;
  CheckTrue(e.MoveNext);
  CheckFalse(e.MoveNext);
end;

procedure TTestDictionaryBase.TestKeysEnumeratorVersion;
var
  e: IEnumerator<Integer>;
begin
  FillTestData;

  e := SUT.Keys.GetEnumerator;
  e.MoveNext;
  SUT.Remove(1);
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.Keys.GetEnumerator;
  e.MoveNext;
  SUT.Add(1, 'a');
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.Keys.GetEnumerator;
  e.MoveNext;
  SUT.Extract(2);
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.Keys.GetEnumerator;
  e.MoveNext;
  SUT[3] := 'foo';
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.Keys.GetEnumerator;
  e.MoveNext;
  SUT.Clear;
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);
end;

procedure TTestDictionaryBase.TestKeysGetEnumerator;
var
  i, key: Integer;
  keys: IReadOnlyCollection<Integer>;
begin
  FillTestData;

  keys := SUT.Keys;
  SUT := nil;

  i := 0;
  for key in keys do
  begin
    Inc(i);
    CheckEquals(i, key);
  end;
  CheckEquals(4, i);

  i := 0;
  for key in keys do
  begin
    keys := nil;
    Inc(i);
    CheckEquals(i, key);
  end;
  CheckEquals(4, i);
end;

procedure TTestDictionaryBase.TestKeysReferenceCounting;
var
  query: IEnumerable<Integer>;
begin
  query := SUT.Keys.Skip(1);
  CheckNotNull(query);
end;

procedure TTestDictionaryBase.TestKeysToArray;
var
  keys: TArray<Integer>;
begin
  FillTestData;
  SUT.Remove(3);
  SUT.Add(5, 'e');

  keys := SUT.Keys.ToArray;
  CheckEquals(4, Length(keys));
  CheckEquals(1, keys[0]);
  CheckEquals(2, keys[1]);
  CheckEquals(4, keys[2]);
  CheckEquals(5, keys[3]);
end;

procedure TTestDictionaryBase.TestMapAdd;
begin
  FillTestData;

  (SUT as IMap<Integer, string>).Add(10, 'ten'); //check if correctly overriden (not abstract)
  CheckEquals('ten', SUT[10]);
end;

procedure TTestDictionaryBase.TestMapExtract;
var
  pair: TPair<Integer, string>;
begin
  FillTestData;

  CheckEquals(4, SUT.Count);
  Check(SUT.ContainsKey(2));
  pair := (SUT as IMap<Integer, string>).Extract(2, 'a');
  CheckEquals(4, SUT.Count);
  CheckEquals(Default(Integer), pair.Key);
  CheckEquals(Default(string), pair.Value);

  pair := (SUT as IMap<Integer, string>).Extract(1, 'a');
  CheckEquals(3, SUT.Count);
  CheckEquals(1, pair.Key);
  CheckEquals('a', pair.Value);
  Check(not SUT.ContainsKey(1));
  Check(not SUT.ContainsValue('a'));
end;

procedure TTestDictionaryBase.TestMapRemove;
begin
  FillTestData;

  Check(SUT.ContainsKey(1));
  Check((SUT as IMap<Integer, string>).Remove(1));
  Check(not SUT.ContainsKey(1));
end;

procedure TTestDictionaryBase.TestOrdered;
var
  items: TArray<TPair<Integer, string>>;
begin
  SUT.Add(3, 'b');
  SUT.Add(1, 'd');
  SUT.Add(4, 'a');
  SUT.Add(2, 'c');

  items := SUT.Ordered.ToArray;
  CheckEquals(1, items[0].Key);
  CheckEquals(2, items[1].Key);
  CheckEquals(3, items[2].Key);
  CheckEquals(4, items[3].Key);
end;

procedure TTestDictionaryBase.TestOrdered_Issue179;
var
  o: IEnumerable<TPair<Integer, string>>;
  e: IEnumerator<TPair<Integer, string>>;
  i: Integer;
begin
  // this test is making sure that .Ordered is properly reference counted
  // and captures the dictionary keeping it alive

  FillTestData;

  o := SUT.Ordered;
  e := o.GetEnumerator;

  // should not destroy the dictionary because of the ordered enumerable
  SUT := nil;

  // even now it should not be destroyed because
  // the enumerator is still keeping it alive
  o := nil;

  // make sure that the dictionary is really still there and contains the items
  i := 0;
  while e.MoveNext do
    Inc(i);
  CheckEquals(4, i);

  // now setting the reference to the enumerator should finally
  // trigger the destruction of the dictionary as this was the
  // last reference keeping it alive
  e := nil;
end;

procedure TTestDictionaryBase.TestRemove;
begin
  FillTestData;

  CheckTrue(SUT.Remove(3));
  CheckCount(3);
  CheckFalse(SUT.Remove(4, 'e'));
  CheckCount(3);
  CheckTrue(SUT.Remove(4, 'd'));
  CheckCount(2);
  CheckTrue(SUT.ContainsKey(1));
  CheckTrue(SUT.ContainsKey(2));
  CheckTrue(SUT.ContainsValue('a'));
  CheckTrue(SUT.ContainsValue('b'));
end;

procedure TTestDictionaryBase.TestSetItem;
var
  values: TArray<string>;
begin
  FillTestData;
  SUT[2] := 'e';

  CheckCount(4);

  values := SUT.Values.ToArray;
  CheckEquals(values[0], 'a');
  CheckEquals(values[1], 'e');
  CheckEquals(values[2], 'c');
  CheckEquals(values[3], 'd');
end;

procedure TTestDictionaryBase.TestSetItemOrder;
begin
  SUT[1] := 'a';
  SUT[2] := 'b';
  SUT[1] := 'c';
  Check(SUT.Keys.EqualsTo([1, 2]));
end;

procedure TTestDictionaryBase.TestSetItem_Issue318;
var
  i: Integer;
begin
  for i := 1 to 100 do
    SUT.Add(i, IntToStr(i));
  for i := 1 to 100 do
    SUT[i] := 'foo' + IntToStr(i);
  for i := 1 to 100 do
    Check(SUT.ContainsKey(i));
end;

procedure TTestDictionaryBase.TestTryAdd;
begin
  FillTestData;

  CheckTrue(SUT.TryAdd(5, 'e'));
  CheckCount(5);
  CheckFalse(SUT.TryAdd(1, 'e'));
  CheckCount(5);
end;

procedure TTestDictionaryBase.TestTryExtract;
var
  value: string;
begin
  FillTestData;

  CheckFalse(SUT.TryExtract(5, value));
  CheckEquals(Default(string), value);
  CheckCount(4);
  CheckTrue(SUT.TryExtract(4, value));
  CheckEquals('d', value);
  CheckCount(3);
  CheckTrue(SUT.TryExtract(3, value));
  CheckEquals('c', value);
  CheckCount(2);
end;

procedure TTestDictionaryBase.TestTryGetElementAt;
var
  item: TPair<Integer,string>;
begin
  FillTestData;

  CheckTrue(SUT.TryGetElementAt(item, 0));
  CheckEquals(item.Key, 1);
  CheckEquals(item.Value, 'a');
  CheckTrue(SUT.TryGetElementAt(item, 1));
  CheckEquals(item.Key, 2);
  CheckEquals(item.Value, 'b');
  CheckTrue(SUT.TryGetElementAt(item, 2));
  CheckEquals(item.Key, 3);
  CheckEquals(item.Value, 'c');
  CheckTrue(SUT.TryGetElementAt(item, 3));
  CheckEquals(item.Key, 4);
  CheckEquals(item.Value, 'd');
  CheckFalse(SUT.TryGetElementAt(item, 4));
end;

procedure TTestDictionaryBase.TestTryUpdateValue;
var
  oldValue: string;
  items: TArray<TPair<Integer, string>>;
begin
  FillTestData;

  oldValue := 'foo';
  CheckFalse(SUT.TryUpdateValue(5, '', oldValue));
  CheckEquals(OldValue, Default(string));

  oldValue := 'foo';
  CheckTrue(SUT.TryUpdateValue(4, 'dd', oldValue));
  CheckEquals(oldValue, 'd');
  CheckEquals(SUT[4], 'dd');

  oldValue := 'foo';
  CheckTrue(SUT.TryUpdateValue(2, 'bb', oldValue));
  CheckEquals(oldValue, 'b');
  CheckEquals(SUT[2], 'bb');

  items := SUT.ToArray;
  CheckEquals(4, Length(items));
  CheckEquals(1, items[0].Key);
  CheckEquals(2, items[1].Key);
  CheckEquals(3, items[2].Key);
  CheckEquals(4, items[3].Key);
  CheckEquals('a', items[0].Value);
  CheckEquals('bb', items[1].Value);
  CheckEquals('c', items[2].Value);
  CheckEquals('dd', items[3].Value);
end;

procedure TTestDictionaryBase.TestToArray;
var
  items: TArray<TPair<Integer, string>>;
begin
  FillTestData;

  items := SUT.ToArray;
  CheckEquals(4, Length(items));
  CheckEquals(1, items[0].Key);
  CheckEquals(2, items[1].Key);
  CheckEquals(3, items[2].Key);
  CheckEquals(4, items[3].Key);
  CheckEquals('a', items[0].Value);
  CheckEquals('b', items[1].Value);
  CheckEquals('c', items[2].Value);
  CheckEquals('d', items[3].Value);
end;

procedure TTestDictionaryBase.TestValuesContains;
begin
  FillTestData;

  Check(not SUT.Values.Contains(''));
  Check(not SUT.Values.Contains('aa'));
  Check(SUT.Values.Contains('a'));
  Check(SUT.Values.Contains('b'));
  Check(SUT.Values.Contains('c'));
  Check(SUT.Values.Contains('d'));
  Check(not SUT.Values.Contains('e'));
end;

procedure TTestDictionaryBase.TestValuesElementAt;
var
  i: Integer;
begin
  FillTestData;

  if IsSorted then
  begin
    for i := 0 to SUT.Count - 1 do
      CheckEquals(Chr(Ord('a') + i), SUT.Values.ElementAt(i));

    // remove and re-add, because this dict is sorted, the order must not change
    SUT.Remove(2);
    SUT.Add(2, 'b');
    for i := 0 to SUT.Count - 1 do
      CheckEquals(Chr(Ord('a') + i), SUT.Values.ElementAt(i));
  end
  else
  begin
    for i := 0 to SUT.Count - 1 do
      CheckEquals(Chr(Ord('a') + i), SUT.Values.ElementAt(i));

    // remove and re-add, ensures that item array is compacted
    SUT.Remove(2);
    SUT.Add(2, 'b');
    CheckEquals('a', SUT.Values.ElementAt(0));
    CheckEquals('b', SUT.Values.ElementAt(3));
    CheckEquals('c', SUT.Values.ElementAt(1));
    CheckEquals('d', SUT.Values.ElementAt(2));
  end;
end;

procedure TTestDictionaryBase.TestValuesEnumerate;
var
  e: IEnumerator<string>;
begin
  FillTestData;

  Check(SUT.Values.EqualsTo(['a', 'b', 'c', 'd']));

  e := SUT.Values.GetEnumerator;
  SUT := nil;
  e.MoveNext;
  e.MoveNext;
  e.MoveNext;
  CheckTrue(e.MoveNext);
  CheckFalse(e.MoveNext);
end;

procedure TTestDictionaryBase.TestValuesEnumeratorVersion;
var
  e: IEnumerator<string>;
begin
  FillTestData;

  e := SUT.Values.GetEnumerator;
  e.MoveNext;
  SUT.Remove(1);
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.Values.GetEnumerator;
  e.MoveNext;
  SUT.Add(1, 'a');
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.Values.GetEnumerator;
  e.MoveNext;
  SUT.Extract(2);
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.Values.GetEnumerator;
  e.MoveNext;
  SUT[3] := 'foo';
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);

  e := SUT.Values.GetEnumerator;
  e.MoveNext;
  SUT.Clear;
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);
end;

procedure TTestDictionaryBase.TestValuesGetEnumerator;
var
  i: Integer;
  value: string;
begin
  FillTestData;

  i := 0;
  for value in SUT.Values do
  begin
    CheckEquals(Chr(ord('a') + i), value);
    Inc(i);
  end;
  CheckEquals(4, i);
end;

procedure TTestDictionaryBase.TestValuesReferenceCounting;
var
  query: IEnumerable<string>;
begin
  query := SUT.Values.Skip(1);
  CheckNotNull(query);
end;

procedure TTestDictionaryBase.TestValuesToArray;
var
  values: TArray<string>;
begin
  FillTestData;
  SUT.Remove(3);
  SUT.Add(5, 'e');

  values := SUT.Values.ToArray;
  CheckEquals(4, Length(values));
  CheckEquals('a', values[0]);
  CheckEquals('b', values[1]);
  CheckEquals('d', values[2]);
  CheckEquals('e', values[3]);
end;

{$ENDREGION}


{$REGION 'TTestDictionary'}

procedure TTestDictionary.SetUp;
begin
  inherited;
  SUT := TCollections.CreateDictionary<Integer, string>;
end;

{$ENDREGION}


{$REGION 'TTestSortedDictionary'}

function TTestSortedDictionary.IsSorted: Boolean;
begin
  Result := True;
end;

procedure TTestSortedDictionary.SetUp;
begin
  inherited;
  SUT := TCollections.CreateSortedDictionary<Integer, string>;
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionaryBase'}

procedure TTestBidiDictionaryBase.TearDown;
begin
  SUTinverse := nil;
  inherited;
end;

procedure TTestBidiDictionaryBase.TestSetItemBidi;
begin
  SUT[1] := 'a';
  CheckException(EArgumentException, procedure begin SUT[2] := 'a' end);
  SUT[1] := 'a';
  CheckEquals(1, SUT.Count);
  SUT[1] := 'b';
  CheckEquals(1, SUT.Count);
end;

procedure TTestBidiDictionaryBase.TestSetItemBidiMultipleTimes;
var
  c: Char;
begin
  FillTestData;

  for c in ['e'..'i'] do
  begin
    SUT[3] := c;
    CheckEquals(3, SUTinverse[c]);
  end;
  SUT[3] := 'c';
  CheckEquals(3, SUTinverse['c']);

  SUT.Remove(2);
  SUT.Add(2, 'b');

  Check(SUT.Keys.EqualsTo([1, 3, 4, 2]));
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionary'}

procedure TTestBidiDictionary.SetUp;
var
  dict: IBidiDictionary<Integer, string>;
begin
  inherited;
  dict := TCollections.CreateBidiDictionary<Integer, string>;
  SUT := dict;
  SUTinverse := dict.Inverse;
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionaryInverse'}

procedure TTestBidiDictionaryInverse.SetUp;
var
  dict: IBidiDictionary<string, Integer>;
begin
  inherited;
  dict := TCollections.CreateBidiDictionary<string, Integer>;
  SUT := dict.Inverse;
  SUTinverse := dict;
end;

{$ENDREGION}


{$REGION 'TTestDictionaryOwnershipBase'}

procedure TTestDictionaryOwnershipBase.TestKeys;
var
  SUT: IDictionary<TObject, Integer>;
  index: Integer;
  keys: TArray<TObject>;
  obj: TObject;
begin
  SUT := CreateOwnedKeysDict;
  keys := TArray<TObject>.Create(TObject.Create, TObject.Create, TObject.Create, TObject.Create);
  for index := 0 to 3 do
    SUT.Add(keys[index], index);
  obj := SUT.Extract(keys[1], 1).Key;
  obj.Free;
  Check(SUT.Remove(keys[2]));
  SUT.Clear;
  Pass;
end;

procedure TTestDictionaryOwnershipBase.TestValues;
var
  SUT: IBidiDictionary<Integer, TObject>;
  obj: TObject;
begin
  SUT := TCollections.CreateBidiDictionary<Integer, TObject>([doOwnsValues]);
  SUT.Add(0, TObject.Create);
  SUT.Add(1, TObject.Create);
  SUT.Add(2, TObject.Create);
  SUT.Add(3, TObject.Create);
  obj := SUT.Extract(1);
  obj.Free;
  SUT.Remove(2);
  SUT.Remove(2);
  SUT.Clear;
  Pass;
end;

{$ENDREGION}


{$REGION 'TTestDictionaryOwnership'}

class function TTestDictionaryOwnership.CreateOwnedKeysDict: IDictionary<TObject, Integer>;
begin
  Result := TCollections.CreateDictionary<TObject, Integer>([doOwnsKeys]);
end;

class function TTestDictionaryOwnership.CreateOwnedValuesDict: IDictionary<Integer, TObject>;
begin
  Result := TCollections.CreateDictionary<Integer, TObject>([doOwnsValues]);
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionaryOwnership'}

class function TTestBidiDictionaryOwnership.CreateOwnedKeysDict: IDictionary<TObject, Integer>;
begin
  Result := TCollections.CreateBidiDictionary<TObject, Integer>([doOwnsKeys]);
end;

class function TTestBidiDictionaryOwnership.CreateOwnedValuesDict: IDictionary<Integer, TObject>;
begin
  Result := TCollections.CreateBidiDictionary<Integer, TObject>([doOwnsValues]);
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionaryInverseOwnership'}

class function TTestBidiDictionaryInverseOwnership.CreateOwnedKeysDict: IDictionary<TObject, Integer>;
begin
  Result := TCollections.CreateBidiDictionary<Integer, TObject>([doOwnsValues]).Inverse;
end;

class function TTestBidiDictionaryInverseOwnership.CreateOwnedValuesDict: IDictionary<Integer, TObject>;
begin
  Result := TCollections.CreateBidiDictionary<TObject, Integer>([doOwnsKeys]).Inverse;
end;

{$ENDREGION}


{$REGION 'TTestDictionaryChangedEventBase'}

procedure TTestDictionaryChangedEventBase.TearDown;
begin
  SUT := nil;
  inherited;
end;

procedure TTestDictionaryChangedEventBase.AddEventHandlers;
begin
  SUT.OnChanged.Add(Changed);
  SUT.OnKeyChanged.Add(KeyChanged);
  SUT.OnValueChanged.Add(ValueChanged);
end;

procedure TTestDictionaryChangedEventBase.TestAdd;
begin
  AddEventHandlers;
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT[2] := 'c';

  CheckEquals(4, fChangedEvents.Count);
  CheckChanged(0, 1, 'a', caAdded);
  CheckChanged(1, 2, 'b', caAdded);
  CheckChanged(2, 2, 'b', caRemoved);
  CheckChanged(3, 2, 'c', caAdded);

  CheckEquals(2, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 1, caAdded);
  CheckKeyChanged(1, 2, caAdded);

  CheckEquals(4, fValueChangedEvents.Count);
  CheckValueChanged(0, 'a', caAdded);
  CheckValueChanged(1, 'b', caAdded);
  CheckValueChanged(2, 'b', caRemoved);
  CheckValueChanged(3, 'c', caAdded);
end;

procedure TTestDictionaryChangedEventBase.TestClear;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  AddEventHandlers;
  SUT.Clear;

  CheckEquals(2, fChangedEvents.Count);
  CheckChanged(0, 1, 'a', caRemoved);
  CheckChanged(1, 2, 'b', caRemoved);

  CheckEquals(2, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 1, caRemoved);
  CheckKeyChanged(1, 2, caRemoved);

  CheckEquals(2, fValueChangedEvents.Count);
  CheckValueChanged(0, 'a', caRemoved);
  CheckValueChanged(1, 'b', caRemoved);
end;

procedure TTestDictionaryChangedEventBase.TestDestroy;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  AddEventHandlers;
  SUT := nil;

  CheckEquals(2, fChangedEvents.Count);
  CheckChanged(0, 1, 'a', caRemoved);
  CheckChanged(1, 2, 'b', caRemoved);

  CheckEquals(2, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 1, caRemoved);
  CheckKeyChanged(1, 2, caRemoved);

  CheckEquals(2, fValueChangedEvents.Count);
  CheckValueChanged(0, 'a', caRemoved);
  CheckValueChanged(1, 'b', caRemoved);
end;

procedure TTestDictionaryChangedEventBase.TestExtract;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  AddEventHandlers;
  CheckEquals('c', SUT.Extract(3));
  CheckEquals('a', SUT.Extract(1));
  CheckEquals('', SUT.Extract(1));

  CheckEquals(2, fChangedEvents.Count);
  CheckChanged(0, 3, 'c', caExtracted);
  CheckChanged(1, 1, 'a', caExtracted);

  CheckEquals(2, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 3, caExtracted);
  CheckKeyChanged(1, 1, caExtracted);

  CheckEquals(2, fValueChangedEvents.Count);
  CheckValueChanged(0, 'c', caExtracted);
  CheckValueChanged(1, 'a', caExtracted);
end;

procedure TTestDictionaryChangedEventBase.TestRemove;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  SUT.Add(5, 'e');
  AddEventHandlers;
  Check(SUT.Remove(3));
  Check(not SUT.Remove(4, 'a'));
  Check(SUT.Remove(4, 'd'));
  Check(SUT.Remove(1));
  Check(not SUT.Remove(1));

  CheckEquals(3, fChangedEvents.Count);
  CheckChanged(0, 3, 'c', caRemoved);
  CheckChanged(1, 4, 'd', caRemoved);
  CheckChanged(2, 1, 'a', caRemoved);

  CheckEquals(3, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 3, caRemoved);
  CheckKeyChanged(1, 4, caRemoved);
  CheckKeyChanged(2, 1, caRemoved);

  CheckEquals(3, fValueChangedEvents.Count);
  CheckValueChanged(0, 'c', caRemoved);
  CheckValueChanged(1, 'd', caRemoved);
  CheckValueChanged(2, 'a', caRemoved);
end;

procedure TTestDictionaryChangedEventBase.TestSetItem;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  AddEventHandlers;
  SUT[2] := 'c';

  CheckEquals(2, fChangedEvents.Count);
  CheckChanged(0, 2, 'b', caRemoved);
  CheckChanged(1, 2, 'c', caAdded);

  CheckEquals(0, fKeyChangedEvents.Count);

  CheckEquals(2, fValueChangedEvents.Count);
  CheckValueChanged(0, 'b', caRemoved);
  CheckValueChanged(1, 'c', caAdded);
end;

procedure TTestDictionaryChangedEventBase.TestTryAdd;
begin
  AddEventHandlers;
  SUT.TryAdd(1, 'a');
  SUT.TryAdd(2, 'b');
  SUT[2] := 'c';

  CheckEquals(4, fChangedEvents.Count);
  CheckChanged(0, 1, 'a', caAdded);
  CheckChanged(1, 2, 'b', caAdded);
  CheckChanged(2, 2, 'b', caRemoved);
  CheckChanged(3, 2, 'c', caAdded);

  CheckEquals(2, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 1, caAdded);
  CheckKeyChanged(1, 2, caAdded);

  CheckEquals(4, fValueChangedEvents.Count);
  CheckValueChanged(0, 'a', caAdded);
  CheckValueChanged(1, 'b', caAdded);
  CheckValueChanged(2, 'b', caRemoved);
  CheckValueChanged(3, 'c', caAdded);
end;

procedure TTestDictionaryChangedEventBase.TestTryExtract;
var
  value: string;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  AddEventHandlers;
  Check(SUT.TryExtract(3, value));
  CheckEquals('c', value);
  Check(SUT.TryExtract(1, value));
  CheckEquals('a', value);
  Check(not SUT.TryExtract(1, value));
  CheckEquals('', value);

  CheckEquals(2, fChangedEvents.Count);
  CheckChanged(0, 3, 'c', caExtracted);
  CheckChanged(1, 1, 'a', caExtracted);

  CheckEquals(2, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 3, caExtracted);
  CheckKeyChanged(1, 1, caExtracted);

  CheckEquals(2, fValueChangedEvents.Count);
  CheckValueChanged(0, 'c', caExtracted);
  CheckValueChanged(1, 'a', caExtracted);
end;

procedure TTestDictionaryChangedEventBase.TestTryUpdateValue;
var
  oldValue: string;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  AddEventHandlers;
  Check(SUT.TryUpdateValue(3, 'e', oldValue));

  CheckEquals(2, fChangedEvents.Count);
  CheckChanged(0, 3, 'c', caRemoved);
  CheckChanged(1, 3, 'e', caAdded);

  CheckEquals(0, fKeyChangedEvents.Count);

  CheckEquals(2, fValueChangedEvents.Count);
  CheckValueChanged(0, 'c', caRemoved);
  CheckValueChanged(1, 'e', caAdded);

  Check(not SUT.TryUpdateValue(5, 'f', oldValue));
  Check(oldValue=Default(string));
  CheckEquals(2, fChangedEvents.Count);
  CheckEquals(0, fKeyChangedEvents.Count);
  CheckEquals(2, fValueChangedEvents.Count);
end;

{$ENDREGION}


{$REGION 'TTestDictionaryChangedEvent'}

procedure TTestDictionaryChangedEvent.Setup;
begin
  inherited;
  SUT := TCollections.CreateDictionary<Integer, string>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}


{$REGION 'TTestSortedDictionaryChangedEvent'}

procedure TTestSortedDictionaryChangedEvent.Setup;
begin
  inherited;
  SUT := TCollections.CreateSortedDictionary<Integer, string>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionaryChangedEvent'}

procedure TTestBidiDictionaryChangedEvent.Setup;
begin
  inherited;
  SUT := TCollections.CreateBidiDictionary<Integer, string>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionaryChangedEventInverse'}

procedure TTestBidiDictionaryChangedEventInverse.Setup;
var
  dict: IBidiDictionary<Integer, string>;
begin
  inherited;
  dict := TCollections.CreateBidiDictionary<string, Integer>.Inverse;
  SUT := dict;
  Sender := dict.Inverse.AsObject;
end;

{$ENDREGION}


{$REGION 'TTestDictionaryCreationExceptions'}

procedure TTestDictionaryCreationExceptions.TestCapacity;
begin
  CheckException(EArgumentOutOfRangeException, procedure begin TCollections.CreateDictionary<Integer, Integer>(-1) end);
  CheckException(EArgumentOutOfRangeException, procedure begin TCollections.CreateBidiDictionary<Integer, Integer>(-1) end);
end;

procedure TTestDictionaryCreationExceptions.TestKeyOwnership;
begin
  CheckException(EInvalidCast, procedure begin TCollections.CreateDictionary<Integer, TObject>([doOwnsKeys]) end);
  CheckException(EInvalidCast, procedure begin TCollections.CreateBidiDictionary<Integer, TObject>([doOwnsKeys]) end);
end;

procedure TTestDictionaryCreationExceptions.TestValueOwnership;
begin
  CheckException(EInvalidCast, procedure begin TCollections.CreateDictionary<TObject, Integer>([doOwnsValues]) end);
  CheckException(EInvalidCast, procedure begin TCollections.CreateBidiDictionary<TObject, Integer>([doOwnsValues]) end);
end;

{$ENDREGION}


end.
