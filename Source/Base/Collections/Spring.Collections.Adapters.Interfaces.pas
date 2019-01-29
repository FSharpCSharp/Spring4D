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

{$I Spring.inc}

unit Spring.Collections.Adapters.Interfaces;

interface

uses
  Spring,
  Spring.Collections;

type
  IReadOnlyCollection = interface;
  ICollection = interface;
  IReadOnlyList = interface;
  IList = interface;
  IReadOnlyDictionary = interface;
  IDictionary = interface;
  IStack = interface;
  IQueue = interface;
  ISet = interface;

  IReadOnlyCollection = interface(IEnumerable)
    ['{4DE35086-06DC-4F99-AE63-BCF4ADB2828D}']
  end;

  ICollection = interface(IEnumerable)
    ['{AC8A0302-C530-46A0-83FC-D88302ECCE3D}']
  {$REGION 'Property Accessors'}
    function GetIsReadOnly: Boolean;
    function GetOnChanged: IEvent;
  {$ENDREGION}

    procedure Add(const item: TValue);
    procedure AddRange(const values: array of TValue); overload;
    procedure AddRange(const values: IEnumerable); overload;

    procedure Clear;

    function Remove(const item: TValue): Boolean;
    procedure RemoveRange(const values: array of TValue); overload;
    procedure RemoveRange(const values: IEnumerable); overload;

    function Extract(const item: TValue): TValue;
    procedure ExtractRange(const values: array of TValue); overload;
    procedure ExtractRange(const values: IEnumerable); overload;

    property IsReadOnly: Boolean read GetIsReadOnly;
    property OnChanged: IEvent read GetOnChanged;
  end;

  IReadOnlyList = interface(IReadOnlyCollection)
    ['{3DFEBF5A-8BF2-4152-A105-BECF01AFB60F}']
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): TValue;
  {$ENDREGION}

    function IndexOf(const item: TValue): Integer; overload;
    function IndexOf(const item: TValue; index: Integer): Integer; overload;
    function IndexOf(const item: TValue; index, count: Integer): Integer; overload;

    property Item[index: Integer]: TValue read GetItem; default;
  end;

  IList = interface(ICollection)
    ['{43FF6143-3B87-4298-B48C-2ABB9353BF68}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetItem(index: Integer): TValue;
    procedure SetCapacity(value: Integer);
    procedure SetItem(index: Integer; const item: TValue);
  {$ENDREGION}

    function Add(const item: TValue): Integer;

    procedure Insert(index: Integer; const item: TValue);
    procedure InsertRange(index: Integer; const values: array of TValue); overload;
    procedure InsertRange(index: Integer; const values: IEnumerable); overload;

    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer);

    procedure Reverse; overload;
    procedure Reverse(index, count: Integer); overload;

    procedure Sort;

    function IndexOf(const item: TValue): Integer; overload;
    function IndexOf(const item: TValue; index: Integer): Integer; overload;
    function IndexOf(const item: TValue; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: TValue): Integer; overload;
    function LastIndexOf(const item: TValue; index: Integer): Integer; overload;
    function LastIndexOf(const item: TValue; index, count: Integer): Integer; overload;

    function AsReadOnly: IReadOnlyList;
    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[index: Integer]: TValue read GetItem write SetItem; default;
  end;

  IReadOnlyDictionary = interface(IReadOnlyCollection)
    ['{D963ED30-C16F-488B-9BC6-1292DD57B295}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetItem(const key: TValue): TValue;
    function GetKeyType: PTypeInfo;
    function GetValueType: PTypeInfo;
    procedure SetCapacity(value: Integer);
    procedure SetItem(const key: TValue; const value: TValue);
  {$ENDREGION}

    function ContainsKey(const key: TValue): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    function TryGetValue(const key: TValue; out value: TValue): Boolean;

    property KeyType: PTypeInfo read GetKeyType;
    property ValueType: PTypeInfo read GetValueType;
  end;

  IDictionary = interface(ICollection)
    ['{9AC642EE-F236-421D-8546-DCA0D8D53791}']
  {$REGION 'Property Accessors'}
    function GetOnKeyChanged: IEvent;
    function GetOnValueChanged: IEvent;
    function GetKeyType: PTypeInfo;
    function GetValueType: PTypeInfo;
  {$ENDREGION}

    procedure Add(const key, value: TValue);

    function Remove(const key: TValue): Boolean; overload;

    function ContainsKey(const key: TValue): Boolean;
    function ContainsValue(const value: TValue): Boolean;

    function TryExtract(const key: TValue; out value: TValue): Boolean;
    function TryGetValue(const key: TValue; out value: TValue): Boolean;

    function AsReadOnly: IReadOnlyDictionary;

    property OnKeyChanged: IEvent read GetOnKeyChanged;
    property OnValueChanged: IEvent read GetOnValueChanged;
    property KeyType: PTypeInfo read GetKeyType;
    property ValueType: PTypeInfo read GetValueType;
  end;

  IStack = interface(IEnumerable)
    ['{82F7B40F-3B32-417F-8001-51458BCE553A}']
  {$REGION 'Property Accessors'}
    function GetOnChanged: IEvent;
  {$ENDREGION}

    procedure Clear;
    procedure Push(const item: TValue);
    function Pop: TValue;
    function Peek: TValue;
    function PeekOrDefault: TValue;
    function TryPeek(out item: TValue): Boolean;
    function TryPop(out item: TValue): Boolean;
    property OnChanged: IEvent read GetOnChanged;
  end;

  IQueue = interface(IEnumerable)
    ['{B3377E32-ADA1-414F-8762-1EA0E4FEF794}']
  {$REGION 'Property Accessors'}
    function GetOnChanged: IEvent;
  {$ENDREGION}

    procedure Clear;
    procedure Enqueue(const item: TValue);
    function Dequeue: TValue;
    function Peek: TValue;
    function PeekOrDefault: TValue;
    function TryDequeue(out item: TValue): Boolean;
    function TryPeek(out item: TValue): Boolean;
    property OnChanged: IEvent read GetOnChanged;
  end;

  ISet = interface(ICollection)
    ['{D83ED568-A7C8-4142-BA0F-5A273AF1AA07}']
    function Add(const item: TValue): Boolean;
    procedure ExceptWith(const other: IEnumerable);
    procedure IntersectWith(const other: IEnumerable);
    procedure UnionWith(const other: IEnumerable);
    function IsSubsetOf(const other: IEnumerable): Boolean;
    function IsSupersetOf(const other: IEnumerable): Boolean;
    function SetEquals(const other: IEnumerable): Boolean;
    function Overlaps(const other: IEnumerable): Boolean;
  end;

implementation

end.
