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

unit Spring.Tests.Collections.Dictionaries;

{$I Spring.inc}

interface

uses
  Generics.Collections,
  Generics.Defaults,
  TestFramework,
  Spring.TestUtils,
  Spring,
  Spring.Collections;

type
  TTestStringIntegerDictionary = class(TTestCase)
  private
    SUT: IDictionary<string, Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDictionaryCountWithAdd;
    procedure TestDictionarySimpleValues;
    procedure TestDictionaryKeys;
    procedure TestDictionaryValues;
    procedure TestDictionaryContainsValue;
    procedure TestDictionaryContainsKey;
    procedure TestCollectionExtract;
    procedure TestMapExtract;
  end;

  TTestIntegerStringDictionary = class(TTestCase)
  private
    SUT: IDictionary<Integer, string>;
    const NumItems = 100;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCount;
    procedure TestMapSimpleValues;
    procedure TestKeys;
    procedure TestValues;
    procedure TestContainsValue;
    procedure TestContainsKey;
    procedure TestEnumeration;
    procedure TestToArray;
    procedure TestOrdered;
    procedure TestAddValue;
    procedure TestAddOrSetValue;
    procedure TestExtract;
    procedure TestRemoveKey;
    procedure TestGetValueOrDefault;
    procedure TestTryGetValue;
    procedure TestIndexedGet;
    procedure TestIndexedSet;
  end;

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
    procedure FillTestData;
  published
    procedure TestAddDictionary;
    procedure TestAddKeyValue;
    procedure TestAddOrSetValue;
    procedure TestAddOrSetValueOrder;
    procedure TestExtract;
    procedure TestGetEnumerator;
    procedure TestIsInitializedEmpty;
    procedure TestKeysEnumerate;
    procedure TestKeysGetEnumerator;
    procedure TestKeysReferenceCounting;
    procedure TestKeysToArray;
    procedure TestMapAdd;
    procedure TestMapRemove;
    procedure TestOrdered;
    procedure TestOrdered_Issue179;
    procedure TestRemove;
    procedure TestTryExtract;
    procedure TestToArray;
    procedure TestValuesEnumerate;
    procedure TestValuesGetEnumerator;
    procedure TestValuesReferenceCounting;
    procedure TestValuesToArray;
  end;

  TTestDictionary = class(TTestDictionaryBase)
  protected
    procedure SetUp; override;
  end;

  TTestBidiDictionaryBase = class(TTestDictionaryBase)
  protected
    SUTinverse: IBidiDictionary<string, Integer>;
  published
    procedure TestAddOrSetValueBidi;
    procedure TestAddOrSetValueBidiMultipleTimes;
  end;

  TTestBidiDictionary = class(TTestBidiDictionaryBase)
  protected
    procedure SetUp; override;
  end;

  TTestBidiDictionaryInverse = class(TTestBidiDictionaryBase)
  protected
    procedure SetUp; override;
  end;

  TTestOrderedDictionary = class(TTestCase)
  private
    SUT: IOrderedDictionary<Integer, string>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetItemByIndex;
    procedure TestIndexOf;
  end;

  TTestSortedDictionary = class(TTestCase)
  private
    SUT: IDictionary<Integer, string>;
    procedure CheckCount(expected: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddKeyValue;
    procedure TestKeysGetEnumerator;
    procedure TestKeysToArray;
    procedure TestValuesGetEnumerator;
    procedure TestValuesToArray;
    procedure TestGetEnumerator;
    procedure TestAddOrSetValue;
    procedure TestRemove;
    procedure TestExtract;
    procedure TestTryExtract;
    procedure TestToArray;
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

  TTestDictionaryChangedEventBase = class(TTestCase)
  private
    type
      TEvent<T> = record
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        sender: TObject;
        item: T;
        action: TCollectionChangedAction;
      end;
      TKeyValuePair = Generics.Collections.TPair<Integer, string>;
  private
    fChangedEvents: IList<TEvent<TKeyValuePair>>;
    fKeyChangedEvents: IList<TEvent<Integer>>;
    fValueChangedEvents: IList<TEvent<string>>;
    procedure Changed(Sender: TObject; const Item: TKeyValuePair; Action: TCollectionChangedAction);
    procedure KeyChanged(Sender: TObject; const Item: Integer; Action: TCollectionChangedAction);
    procedure ValueChanged(Sender: TObject; const Item: string; Action: TCollectionChangedAction);
    procedure AddEventHandlers;
    procedure CheckChanged(index: Integer; key: Integer; const value: string; action: TCollectionChangedAction);
    procedure CheckKeyChanged(index: Integer; key: Integer; action: TCollectionChangedAction);
    procedure CheckValueChanged(index: Integer; const value: string; action: TCollectionChangedAction);
  protected
    SUT: IDictionary<Integer, string>;
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    Sender: TObject;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestClear;
    procedure TestDestroy;
    procedure TestExtract;
    procedure TestRemove;
    procedure TestSetItem;
    procedure TestTryExtract;
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

implementation

uses
  SysUtils;


{$REGION 'TTestStringIntegerDictionary'}

procedure TTestStringIntegerDictionary.SetUp;
begin
  inherited;
  SUT := TCollections.CreateDictionary<string, Integer>;
  SUT.Add('one', 1);
  SUT.Add('two', 2);
  SUT.Add('three', 3);
end;

procedure TTestStringIntegerDictionary.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestStringIntegerDictionary.TestDictionaryContainsKey;
begin
  CheckTrue(SUT.ContainsKey('one'), '"one" not found by ContainsKey');
  CheckTrue(SUT.ContainsKey('two'), '"two" not found by ContainsKey');
  CheckTrue(SUT.ContainsKey('three'), '"three" not found by ContainsKey');
end;

procedure TTestStringIntegerDictionary.TestDictionaryContainsValue;
begin
  CheckTrue(SUT.ContainsValue(1), '1 not found by ContainsValue');
  CheckTrue(SUT.ContainsValue(2), '2 not found by ContainsValue');
  CheckTrue(SUT.ContainsValue(3), '3 not found by ContainsValue');
end;

procedure TTestStringIntegerDictionary.TestDictionaryCountWithAdd;
begin
  CheckEquals(3, SUT.Count, 'TestDictionaryCountWithAdd: Count is not correct');
end;

procedure TTestStringIntegerDictionary.TestDictionaryKeys;
var
  Result: IReadOnlyCollection<string>;
begin
  Result := SUT.Keys;
  CheckEquals(3, Result.Count, 'TestDictionaryKeys: Keys call returns wrong count');

  CheckTrue(Result.Contains('one'), 'TestDictionaryKeys: Keys doesn''t contain "one"');
  CheckTrue(Result.Contains('two'), 'TestDictionaryKeys: Keys doesn''t contain "two"');
  CheckTrue(Result.Contains('three'), 'TestDictionaryKeys: Keys doesn''t contain "three"');
end;

procedure TTestStringIntegerDictionary.TestDictionarySimpleValues;
begin
  CheckEquals(3, SUT.Count, 'TestDictionarySimpleValues: Count is not correct');

  CheckEquals(1, SUT['one']);
  CheckEquals(2, SUT['two']);
  CheckEquals(3, SUT['three']);
end;

procedure TTestStringIntegerDictionary.TestDictionaryValues;
var
  Result: IReadOnlyCollection<Integer>;
begin
  Result := SUT.Values;
  CheckEquals(3, Result.Count, 'TestDictionaryKeys: Values call returns wrong count');

  CheckTrue(Result.Contains(1), 'TestDictionaryKeys: Values doesn''t contain "one"');
  CheckTrue(Result.Contains(2), 'TestDictionaryKeys: Values doesn''t contain "two"');
  CheckTrue(Result.Contains(3), 'TestDictionaryKeys: Values doesn''t contain "three"');
end;

procedure TTestStringIntegerDictionary.TestCollectionExtract;
var
  pair: TPair<string, Integer>;
begin
  pair := (SUT as ICollection<TPair<string, Integer>>).Extract(TPair<string, Integer>.Create('one', 2));
  CheckEquals(3, SUT.Count);
  CheckEquals(Default(string), pair.Key);
  CheckEquals(Default(Integer), pair.Value);

  pair := (SUT as ICollection<TPair<string, Integer>>).Extract(TPair<string, Integer>.Create('one', 1));
  CheckEquals(2, SUT.Count);
  CheckEquals('one', pair.Key);
  CheckEquals(1, pair.Value);
  CheckFalse(SUT.ContainsKey('one'), 'TestMapExtract: Values does contain "one"');
end;

procedure TTestStringIntegerDictionary.TestMapExtract;
var
  pair: TPair<string, Integer>;
begin
  pair := (SUT as IMap<string, Integer>).Extract('one', 2);
  CheckEquals(3, SUT.Count);
  CheckEquals(Default(string), pair.Key);
  CheckEquals(Default(Integer), pair.Value);

  pair := (SUT as IMap<string, Integer>).Extract('one', 1);
  CheckEquals(2, SUT.Count);
  CheckEquals('one', pair.Key);
  CheckEquals(1, pair.Value);
  CheckFalse(SUT.ContainsKey('one'), 'TestMapExtract: Values does contain "one"');
end;

{$ENDREGION}


{$REGION 'TTestIntegerStringMap'}

procedure TTestIntegerStringDictionary.SetUp;
var
  i, n: Integer;
begin
  SUT := TCollections.CreateSortedDictionary<Integer, string>;
  // Add a reasonable number of items to the map, adding in an order that might
  // cause a mismatched tree if the backing red-black tree had balancing errors.
  n := Round(NumItems * 0.667);
  for i := n to Pred(NumItems) do
    SUT.Add(i, IntToStr(i));
  for i := 0 to Pred(n) do
    SUT.Add(i, IntToStr(i));
end;

procedure TTestIntegerStringDictionary.TearDown;
begin
  SUT := nil;
end;

procedure TTestIntegerStringDictionary.TestCount;
begin
  Check(SUT.Count = NumItems);
end;

procedure TTestIntegerStringDictionary.TestMapSimpleValues;
var
  i: Integer;
begin
  // Check all items are in the map
  Check(SUT.Count = NumItems);
  for i := 0 to Pred(NumItems) do
    Check(SUT[i] = IntToStr(i));
  // Check a couple of others are not there
  Check(not SUT.ContainsKey(-1));
  Check(not SUT.ContainsKey(NumItems));
end;

procedure TTestIntegerStringDictionary.TestKeys;
var
  keys: IReadOnlyCollection<Integer>;
  i: Integer;
begin
  keys := SUT.Keys;
  Check(keys.Count = NumItems);
  for i := 0 to Pred(NumItems) do
    Check(Keys.Contains(i));
end;

procedure TTestIntegerStringDictionary.TestValues;
var
  values: IReadOnlyCollection<string>;
  i: Integer;
begin
  Values := SUT.Values;
  Check(Values.Count = NumItems);
  for i := 0 to Pred(NumItems) do
    Check(Values.Contains(IntToStr(i)));
end;

procedure TTestIntegerStringDictionary.TestContainsValue;
var
  i: Integer;
begin
  // Check it contains each value it should
  for i := 0 to Pred(NumItems) do
    Check(SUT.ContainsValue(IntToStr(i)));

  // And check it does not contain other values
  Check(not SUT.ContainsValue('-1'));
  Check(not SUT.ContainsValue('test'));
  Check(not SUT.ContainsValue(''));
end;

procedure TTestIntegerStringDictionary.TestContainsKey;
var
  i: Integer;
begin
  // Check it contains each value it should
  for i := 0 to Pred(NumItems) do
    Check(SUT.ContainsKey(i));

  // And check it does not contain other keys
  Check(not SUT.ContainsKey(-1));
  Check(not SUT.ContainsKey(NumItems));
  Check(not SUT.ContainsKey(MaxInt));
end;

procedure TTestIntegerStringDictionary.TestEnumeration;
var
  i: Integer;
  item: TPair<Integer, string>;
begin
  // Check it enumerates each item, and in the expected order
  i := 0;
  for item in SUT do
  begin
    Check(item.Key = i);
    Check(item.Value = IntToStr(i));
    Inc(i);
  end;
  Check(i = NumItems);
end;

procedure TTestIntegerStringDictionary.TestToArray;
var
  i: Integer;
  items: TArray<TPair<Integer, string>>;
begin
  items := SUT.ToArray;
  Check(Assigned(items));
  Check(Length(items) = NumItems);

  for i := Low(items) to High(items) do
  begin
    Check(items[i].Key = i);
    Check(items[i].Value = IntToStr(i));
  end;
end;

procedure TTestIntegerStringDictionary.TestOrdered;
var
  items: IEnumerable<TPair<Integer, string>>;
  i: Integer;
  item: TPair<Integer, string>;
begin
  items := SUT.Ordered;
  Check(Assigned(items));

  // Check it enumerates each item, and in the expected order
  i := 0;
  for item in SUT do
  begin
    Check(item.Key = i);
    Check(item.Value = IntToStr(i));
    Inc(i);
  end;
  Check(i = NumItems);
end;

procedure TTestIntegerStringDictionary.TestAddValue;
begin
  SUT.Add(NumItems, IntToStr(NumItems));
  Check(SUT.Items[NumItems] = IntToStr(NumItems));
  Check(SUT.Count = NumItems+1);

  // Add should raise an exception when the item already exists
  CheckException(EInvalidOperationException,
    procedure
    begin
      SUT.Add(NumItems, IntToStr(NumItems));
    end);
  CheckException(EInvalidOperationException,
    procedure
    begin
      SUT.Add(0, IntToStr(0));
    end);
  CheckException(EInvalidOperationException,
    procedure
    begin
      SUT.Add(NumItems div 2, IntToStr(NumItems div 2));
    end);

  // Check it didn't actually add anything in the failure items above
  Check(SUT.Count = NumItems + 1);
end;

procedure TTestIntegerStringDictionary.TestAddOrSetValue;
begin
  // AddOrSet should never raise an exception when there's a duplicate already,
  // but should simply overwrite it.

  SUT.AddOrSetValue(NumItems, IntToStr(NumItems));
  Check(SUT.Items[NumItems] = IntToStr(NumItems));
  Check(SUT.Count = NumItems+1);

  // Add again
  SUT.AddOrSetValue(NumItems, 'test');
  Check(SUT.Items[NumItems] = 'test');
end;

procedure TTestIntegerStringDictionary.TestExtract;
begin
  Check(SUT.Count = NumItems);

  Check(SUT.Extract(0) = '0');
  Check(SUT.Count = NumItems-1); // Extract removes an item

  Check(SUT.Extract(NumItems-1) = IntToStr(NumItems-1));
  Check(SUT.Count = NumItems-2);

  Check(SUT.Extract(NumItems div 2) = IntToStr(NumItems div 2));
  Check(SUT.Count = NumItems-3);
end;

procedure TTestIntegerStringDictionary.TestRemoveKey;
begin
  Check(SUT.Count = NumItems);

  Check(SUT.Remove(0));
  Check(SUT.Count = NumItems-1);

  Check(SUT.Remove(NumItems-1));
  Check(SUT.Count = NumItems-2);

  Check(SUT.Remove(NumItems div 2));
  Check(SUT.Count = NumItems-3);

  // But Remove should fail for items that are not present
  Check(not SUT.Remove(NumItems div 2)); // Already removed
  Check(SUT.Count = NumItems-3); // Count unchanged

  Check(not SUT.Remove(NumItems * 2)); // Never added
  Check(SUT.Count = NumItems-3); // Count unchanged
end;

procedure TTestIntegerStringDictionary.TestGetValueOrDefault;
begin
  // An item that exists
  Check((SUT as IReadOnlyDictionary<Integer, string>).GetValueOrDefault(0, 'test') = '0');
  // An item that does not exist
  Check((SUT as IReadOnlyDictionary<Integer, string>).GetValueOrDefault(NumItems*2, 'test') = 'test');
end;

procedure TTestIntegerStringDictionary.TestTryGetValue;
var
  value: string;
begin
  // An item that exists
  Check(SUT.TryGetValue(0, value));
  Check(value = '0');

  // An item that does not exist
  value := 'blah';
  Check(not SUT.TryGetValue(NumItems * 2, value));
end;

procedure TTestIntegerStringDictionary.TestIndexedGet;
begin
  Check(SUT[0] = '0');
  Check(SUT[NumItems div 2] = IntToStr(NumItems div 2));

  // Get returns the default value when the item doesn't exist
  Check(SUT[NumItems * 10] = '');
  Check(SUT[MaxInt] = '');
  Check(SUT[-500] = '');
end;

procedure TTestIntegerStringDictionary.TestIndexedSet;
var
  item: TPair<Integer, string>;
  i: Integer;
begin
  // Set overwrites existing items without an error
  Check(SUT[0] = '0');
  SUT[0] := 'hello';
  Check(SUT[0] = 'hello');

  // Use .Items explicitly (it should be the default property)
  Check(SUT.Items[NumItems div 2] = IntToStr(NumItems div 2));
  SUT.Items[NumItems div 2] := 'hello again';
  Check(SUT.Items[NumItems div 2] = 'hello again');

  // And adds new items
  Check(not SUT.ContainsKey(10000));
  SUT[10000] := 'large number';
  Check(SUT.Count = NumItems + 1);
  Check(SUT.ContainsKey(10000));
  Check(SUT.Items[10000] = 'large number');

  // And check that all the other items are untouched
  i := 0;
  for item in SUT do
  begin
    // The items changed or added above
    if i = 0 then
    begin
      Check(item.Key = 0);
      Check(item.Value = 'hello');
    end else if i = NumItems div 2 then
    begin
      Check(item.Key = NumItems div 2);
      Check(item.Value = 'hello again');
    end else if i = NumItems then
    begin
      // The last item should be the large number added above
      Check(i = NumItems);
      Check(item.Key = 10000);
      Check(item.Value = 'large number');
    end else
    begin
      // All other items in the map should not have been affected by the above
      Check(item.Key = i);
      Check(item.Value = IntToStr(i));
    end;
    Inc(i);
  end;
  Check(i = NumItems+1);
end;

{$ENDREGION}


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
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
end;

procedure TTestDictionaryBase.TearDown;
begin
  SUT := nil;
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
end;

procedure TTestDictionaryBase.TestAddOrSetValue;
var
  values: TArray<string>;
begin
  FillTestData;
  SUT.AddOrSetValue(2, 'e');

  CheckCount(4);

  values := SUT.Values.ToArray;
  CheckEquals(values[0], 'a');
  CheckEquals(values[1], 'e');
  CheckEquals(values[2], 'c');
  CheckEquals(values[3], 'd');
end;

procedure TTestDictionaryBase.TestAddOrSetValueOrder;
begin
  SUT.AddOrSetValue(1, 'a');
  SUT.AddOrSetValue(2, 'b');
  SUT.AddOrSetValue(1, 'c');
  Check(SUT.Keys.EqualsTo([1, 2]));
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

procedure TTestDictionaryBase.TestIsInitializedEmpty;
begin
  CheckEquals(0, SUT.Count);
  CheckEquals(0, SUT.Keys.Count);
  CheckEquals(0, SUT.Values.Count);
end;

procedure TTestDictionaryBase.TestKeysEnumerate;
var
  e: IEnumerator<Integer>;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');

  Check(SUT.Keys.EqualsTo([1, 2, 3, 4]));

  e := SUT.Keys.GetEnumerator;
  SUT := nil;
  e.MoveNext;
  e.MoveNext;
  e.MoveNext;
  CheckTrue(e.MoveNext);
  CheckFalse(e.MoveNext);
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

procedure TTestDictionaryBase.TestValuesEnumerate;
var
  e: IEnumerator<string>;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');

  Check(SUT.Values.EqualsTo(['a', 'b', 'c', 'd']));

  e := SUT.Values.GetEnumerator;
  SUT := nil;
  e.MoveNext;
  e.MoveNext;
  e.MoveNext;
  CheckTrue(e.MoveNext);
  CheckFalse(e.MoveNext);
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


{$REGION 'TTestBidiDictionaryBase'}

procedure TTestBidiDictionaryBase.TestAddOrSetValueBidi;
begin
  SUT.AddOrSetValue(1, 'a');
  CheckException(EInvalidOperationException, procedure begin SUT.AddOrSetValue(2, 'a') end, 'EInvalidOperationException was not raised');
  SUT.AddOrSetValue(1, 'a');
  CheckEquals(1, SUT.Count);
  SUT.AddOrSetValue(1, 'b');
  CheckEquals(1, SUT.Count);
end;

procedure TTestBidiDictionaryBase.TestAddOrSetValueBidiMultipleTimes;
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


{$REGION 'TTestOrderedDictionary'}

procedure TTestOrderedDictionary.SetUp;
begin
  SUT := TCollections.CreateDictionary<Integer, string>;
end;

procedure TTestOrderedDictionary.TearDown;
begin
  SUT := nil;
end;

procedure TTestOrderedDictionary.TestGetItemByIndex;
var
  i: Integer;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');

  for i := 0 to SUT.Count - 1 do
    CheckEquals(i + 1, SUT.Items[i].Key);

  // remove and re-add, ensures that item array is compacted
  SUT.Remove(2);
  SUT.Add(2, 'b');
  CheckEquals(1, SUT.Items[0].Key);
  CheckEquals(2, SUT.Items[3].Key);
  CheckEquals(3, SUT.Items[1].Key);
  CheckEquals(4, SUT.Items[2].Key);
end;

procedure TTestOrderedDictionary.TestIndexOf;
var
  i: Integer;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');

  for i := 0 to SUT.Count - 1 do
    CheckEquals(i, SUT.IndexOf(i + 1));

  // remove and re-add, ensures that item array is compacted
  SUT.Remove(2);
  SUT.Add(2, 'b');
  CheckEquals(-1, SUT.IndexOf(0));
  CheckEquals(0, SUT.IndexOf(1));
  CheckEquals(1, SUT.IndexOf(3));
  CheckEquals(2, SUT.IndexOf(4));
  CheckEquals(3, SUT.IndexOf(2));
  CheckEquals(-1, SUT.IndexOf(5));
end;

{$ENDREGION}


{$REGION 'TTestSortedDictionary'}

procedure TTestSortedDictionary.SetUp;
begin
  inherited;
  SUT := TCollections.CreateSortedDictionary<Integer, string>;
end;

procedure TTestSortedDictionary.TearDown;
begin
  SUT := nil;
  inherited;
end;

procedure TTestSortedDictionary.CheckCount(expected: Integer);
begin
  CheckEquals(expected, SUT.Count, 'Count');
  CheckEquals(expected, SUT.Keys.Count, 'Keys.Count');
  CheckEquals(expected, SUT.Values.Count, 'Values.Count');
end;

procedure TTestSortedDictionary.TestAddKeyValue;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');

  CheckCount(4);
end;

procedure TTestSortedDictionary.TestAddOrSetValue;
var
  values: TArray<string>;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');
  SUT.AddOrSetValue(2, 'e');

  CheckCount(4);

  values := SUT.Values.ToArray;
  CheckEquals(values[0], 'a');
  CheckEquals(values[1], 'e');
  CheckEquals(values[2], 'c');
  CheckEquals(values[3], 'd');
end;

procedure TTestSortedDictionary.TestGetEnumerator;
var
  pair: TPair<Integer, string>;
  i: Integer;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');

  i := 0;
  for pair in SUT do
  begin
    Inc(i);
    CheckEquals(i, pair.Key);
  end;
  CheckEquals(4, i);
end;

procedure TTestSortedDictionary.TestKeysGetEnumerator;
var
  i, key: Integer;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');

  i := 0;
  for key in SUT.Keys do
  begin
    Inc(i);
    CheckEquals(i, key);
  end;
  CheckEquals(4, i);
end;

procedure TTestSortedDictionary.TestKeysToArray;
var
  keys: TArray<Integer>;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');
  SUT.Remove(3);
  SUT.Add(5, 'e');

  keys := SUT.Keys.ToArray;
  CheckEquals(4, Length(keys));
  CheckEquals(1, keys[0]);
  CheckEquals(2, keys[1]);
  CheckEquals(4, keys[2]);
  CheckEquals(5, keys[3]);
end;

procedure TTestSortedDictionary.TestRemove;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');

  CheckTrue(SUT.Remove(3));
  CheckCount(3);
  CheckFalse(SUT.Remove(4, 'e'));
  CheckCount(3);
  CheckTrue(SUT.Remove(4, 'd'));
  CheckCount(2);
end;

procedure TTestSortedDictionary.TestExtract;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');

  CheckEquals('', SUT.Extract(5));
  CheckCount(4);
  CheckEquals('d', SUT.Extract(4));
  CheckCount(3);
  CheckEquals('c', SUT.Extract(3));
  CheckCount(2);
end;

procedure TTestSortedDictionary.TestTryExtract;
var
  value: string;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');

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

procedure TTestSortedDictionary.TestToArray;
var
  items: TArray<TPair<Integer, string>>;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');

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

procedure TTestSortedDictionary.TestValuesGetEnumerator;
const
  Values: array[1..4] of string = ('a', 'b', 'c', 'd');
var
  i: Integer;
  value: string;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');

  i := 0;
  for value in SUT.Values do
  begin
    Inc(i);
    CheckEquals(Values[i], value);
  end;
  CheckEquals(4, i);
end;

procedure TTestSortedDictionary.TestValuesToArray;
var
  values: TArray<string>;
begin
  SUT.Add(3, 'c');
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(4, 'd');
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


{$REGION 'TTestDictionaryOwnershipBase'}

procedure TTestDictionaryOwnershipBase.TestKeys;
var
  SUT: IDictionary<TObject, Integer>;
  index: Integer;
  keys: TArray<TObject>;
begin
  SUT := CreateOwnedKeysDict;
  keys := TArray<TObject>.Create(TObject.Create, TObject.Create, TObject.Create, TObject.Create);
  for index := 0 to 3 do
    SUT.Add(keys[index], index);
  SUT.Extract(keys[1], 1).Key.Free;
  Check(SUT.Remove(keys[2]));
  Check(not SUT.Remove(keys[2]));
  SUT.Clear;
  Pass;
end;

procedure TTestDictionaryOwnershipBase.TestValues;
var
  SUT: IBidiDictionary<Integer, TObject>;
begin
  SUT := TCollections.CreateBidiDictionary<Integer, TObject>([doOwnsValues]);
  SUT.Add(0, TObject.Create);
  SUT.Add(1, TObject.Create);
  SUT.Add(2, TObject.Create);
  SUT.Add(3, TObject.Create);
  SUT.Extract(1).Free;
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

procedure TTestDictionaryChangedEventBase.SetUp;
begin
  inherited;
  fChangedEvents := TCollections.CreateList<TEvent<TKeyValuePair>>;
  fKeyChangedEvents := TCollections.CreateList<TEvent<Integer>>;
  fValueChangedEvents := TCollections.CreateList<TEvent<string>>;
end;

procedure TTestDictionaryChangedEventBase.TearDown;
begin
  SUT := nil;
  fValueChangedEvents := nil;
  fKeyChangedEvents := nil;
  fChangedEvents := nil;
  inherited;
end;

procedure TTestDictionaryChangedEventBase.Changed(Sender: TObject;
  const Item: TKeyValuePair; Action: TCollectionChangedAction);
var
  event: TEvent<TKeyValuePair>;
begin
  event.sender := Sender;
  event.item := Item;
  event.action := Action;
  fChangedEvents.Add(event);
end;

procedure TTestDictionaryChangedEventBase.KeyChanged(Sender: TObject;
  const Item: Integer; Action: TCollectionChangedAction);
var
  event: TEvent<Integer>;
begin
  event.sender := Sender;
  event.item := Item;
  event.action := Action;
  fKeyChangedEvents.Add(event);
end;

procedure TTestDictionaryChangedEventBase.ValueChanged(Sender: TObject;
  const Item: string; Action: TCollectionChangedAction);
var
  event: TEvent<string>;
begin
  event.sender := Sender;
  event.item := Item;
  event.action := Action;
  fValueChangedEvents.Add(event);
end;

procedure TTestDictionaryChangedEventBase.AddEventHandlers;
begin
  SUT.OnChanged.Add(Changed);
  SUT.OnKeyChanged.Add(KeyChanged);
  SUT.OnValueChanged.Add(ValueChanged);
end;

procedure TTestDictionaryChangedEventBase.CheckChanged(index: Integer; key: Integer; const value: string; action: TCollectionChangedAction);
begin
  Check(Sender = fChangedEvents[index].sender);
  Check(key = fChangedEvents[index].item.Key);
  Check(value = fChangedEvents[index].item.Value);
  Check(action = fChangedEvents[index].action);
end;

procedure TTestDictionaryChangedEventBase.CheckKeyChanged(index: Integer; key: Integer; action: TCollectionChangedAction);
begin
  Check(Sender = fKeyChangedEvents[index].sender);
  Check(key = fKeyChangedEvents[index].item);
  Check(action = fKeyChangedEvents[index].action);
end;

procedure TTestDictionaryChangedEventBase.CheckValueChanged(index: Integer; const value: string; action: TCollectionChangedAction);
begin
  Check(Sender = fValueChangedEvents[index].sender);
  Check(value = fValueChangedEvents[index].item);
  Check(action = fValueChangedEvents[index].action);
end;

procedure TTestDictionaryChangedEventBase.TestAdd;
begin
  AddEventHandlers;
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.AddOrSetValue(2, 'c');

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


end.
