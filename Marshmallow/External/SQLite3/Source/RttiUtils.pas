unit RttiUtils;
// MIT License
//
// Copyright (c) 2009 - Robert Love
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE
//

interface
uses
  Generics.Collections,
  SysUtils,
  Classes,
  Rtti,
  TypInfo;
type
  ERttiMemberHelperException = class(Exception);
  // Make things a bit easier.
  TRttiMemberHelper = class helper for TRttiMember
  private
    function GetType: TRttiType;
  published
  public
    function GetValue(Instance: Pointer): TValue; overload;
    function GetValue(const Instance : TValue) : TValue; overload;
    procedure SetValue(Instance: Pointer; const AValue: TValue); overload;
    procedure SetValue(const Instance: TValue; const AValue: TValue); overload;
    property MemberType : TRttiType read GetType;
  end;

  TCustomAttributeClass = class of TCustomAttribute;

  TAttrUtils = class(TObject)
   public
     class function HasAttribute(aType : pTypeinfo;aClass : TCustomAttributeClass;var Attr : TCustomAttribute) : Boolean; overload;
     class function HasAttribute(aContext : TRttiContext; aType : TRttiObject;aClass : TCustomAttributeClass;var Attr : TCustomAttribute) : Boolean; overload;
     class function HasAttributes(aType : pTypeinfo;aClass : TCustomAttributeClass;var Attrs : TArray<TCustomAttribute>) : Boolean; overload;
     class function HasAttributes(aContext : TRttiContext; aType : TRttiObject;aClass : TCustomAttributeClass;var Attrs : TArray<TCustomAttribute>) : Boolean; overload;

     class function GetAttribute(aType : pTypeinfo;aClass : TCustomAttributeClass) :  TCustomAttribute; overload;
     class function GetAttribute(aContext : TRttiContext; aType : TRttiObject;aClass : TCustomAttributeClass): TCustomAttribute ; overload;

     class function GetAttributes(aType : pTypeinfo;aClass : TCustomAttributeClass) :  TArray<TCustomAttribute>; overload;
     class function GetAttributes(aContext : TRttiContext; aType : TRttiObject;aClass : TCustomAttributeClass): TArray<TCustomAttribute> ; overload;
  end;

  TRttiEnumerator = class(TEnumerator<TValue>) // Adapater allowing a common interface
  protected
    FContext : TRttiContext;
    FCurrentValue : TRttiProperty;
    FMoveNext : TRttiMethod;
    FInstance : TObject;
    function DoGetCurrent: TValue; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(aEnumerator : TObject;aContext : TRttiContext;aCurrent :TRttiProperty; aMoveNext: TRttiMethod);
  end;

  TArrayEnumerator = class(TEnumerator<TValue>)// Adapater allowing a common interface
  protected
    FArray : TValue;
    FIndex : Integer;
    function DoGetCurrent: TValue; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(aArray : TValue);
  end;

  EEnumerableFactoryException = class(Exception);
  // Factory to create the correct adapter
  TEnumerableFactory = class (TEnumerable<TValue>)
  protected
     FValue : TValue;
     function CreateRttiEnum(aValue : TValue): TRttiEnumerator;
     function DoGetEnumerator: TEnumerator<TValue>; override;
  public
     constructor Create(aValue : TValue);
     class function IsTypeSupported(aType : TRttiType): Boolean;
  end;

  EElementAddException = class(Exception);
  ERttiElementAddException = class(EElementAddException);
  EArrayElementAddException = class(EElementAddException);


  TElementAdd = class abstract(TObject) // Adapater base class
  protected
    FList : TValue;
  public
    procedure Add(aAddElement : TValue); virtual; abstract;
    procedure AddFinalize; virtual;  // Can't depend on the data to be added to FList until this called.
    property List : TValue read FList;
    constructor Create(aList : TValue); virtual;
    class function TypeSupported(aListType : pTypeInfo) : Boolean; virtual; abstract;
    class function GetAddType(aListType : pTypeInfo): pTypeInfo; virtual; abstract;
  end;

  TRttiElementAdd = class(TElementAdd) // Adapater
  protected
    FContext : TRttiContext;
    FAddMethod : TRttiMethod;
  public
    procedure Add(aAddElement : TValue); override;
    constructor Create(aList : TValue); override;
    class function TypeSupported(aListType : pTypeInfo) : Boolean; override;
    class function GetAddType(aListType : pTypeInfo): pTypeInfo; override;
  end;

  TArrayElementAdd = class(TElementAdd) // Adapater
  protected
    FTempList : TList<TValue>;
  public
    procedure Add(aAddElement : TValue); override;
    procedure AddFinalize; override;
    constructor Create(aList : TValue); override;
    destructor Destroy; override;
    class function TypeSupported(aListType : pTypeInfo) : Boolean; override;
    class function GetAddType(aListType : pTypeInfo): pTypeInfo; override;
  end;

  TElementAddFactory = class(Tobject)
    class function CreateElementAdd(Value : TValue) : TElementAdd;
    class function TypeSupported(Value : pTypeInfo) : Boolean;
    class function GetAddType(aListType : pTypeInfo): pTypeInfo;
  end;



implementation

{ TRttiMemberHelper }

function TRttiMemberHelper.GetType: TRttiType;
begin
 Assert(Assigned(Self)); // For those who forget to check first.
 if Self is TRttiProperty then
    result := TRttiProperty(Self).PropertyType
 else if Self is TRttiField then
     result := TRttiField(Self).FieldType
 else //if Self is TRttiMethod then
      //     result := TRttiMethod(self).  hmmm Don't know how to get to the  TRttiMethodType and I don't need it
   result := nil;
end;

function TRttiMemberHelper.GetValue(Instance: Pointer): TValue;
begin
  Assert(Assigned(Self)); // For those who forget to check first.
  if InheritsFrom(TRttiProperty) then
     result := TRttiProperty(Self).GetValue(Instance)
  else if InheritsFrom(TRttiField) then
     result := TRttiField(Self).GetValue(Instance)
  else  raise ERttiMemberHelperException.CreateFmt('Expecting Property or Field, found: %s',[ClassName]);

end;

procedure TRttiMemberHelper.SetValue(Instance: Pointer; const AValue: TValue);
begin
  Assert(Assigned(Self)); // For those who forget to check first.
  if InheritsFrom(TRttiProperty) then
     TRttiProperty(Self).SetValue(Instance,aValue)
  else if InheritsFrom(TRttiField) then
     TRttiField(Self).SetValue(Instance,aValue)
  else raise ERttiMemberHelperException.Create('Expecting Property or Field');
end;

function TRttiMemberHelper.GetValue(const Instance: TValue): TValue;
begin
  if Instance.isObject then
  begin
    result := GetValue(Instance.AsObject);
  end
  else
  begin
    result := GetValue(Instance.GetReferenceToRawData);
  end;
end;

procedure TRttiMemberHelper.SetValue(const Instance: TValue; const AValue: TValue);
begin
  if Instance.isObject then
  begin
    SetValue(Instance.AsObject,AValue);
  end
  else
  begin
    SetValue(Instance.GetReferenceToRawData,aValue)
  end;
end;

class function TAttrUtils.GetAttribute(aType: pTypeinfo;
  aClass: TCustomAttributeClass): TCustomAttribute;
var
 c : TRttiContext;
begin
 c := TRttiContext.Create;
 try
   result := GetAttribute(c, c.GetType(aType),aClass);
 finally
   c.Free;
 end;
end;

class function TAttrUtils.GetAttribute(aContext: TRttiContext; aType: TRttiObject;
  aClass: TCustomAttributeClass): TCustomAttribute;
var
 lAttr : TCustomAttribute;
begin
  Assert(Assigned(aType));
  for lAttr in aType.GetAttributes do
  begin
    if lAttr is aClass then
    begin
      exit(lAttr);
    end;
  end;
  result := nil;
end;

class function TAttrUtils.GetAttributes(aContext: TRttiContext;
  aType: TRttiObject; aClass: TCustomAttributeClass): TArray<TCustomAttribute>;
var
  Attrs : TArray<TCustomAttribute>;
  lp,idx : Integer;
begin
  Assert(Assigned(aType));
  Attrs := aType.GetAttributes;
  SetLength(result,Length(Attrs));
  idx := 0;
  for lp := 0 to Length(Attrs) - 1 do
  begin
    if Attrs[lp] is aClass then
    begin
      result[idx] := Attrs[lp];
      inc(idx);
    end;
  end;
  SetLength(result,idx);
end;

class function TAttrUtils.GetAttributes(aType: pTypeinfo;
  aClass: TCustomAttributeClass): TArray<TCustomAttribute>;
var
 c : TRttiContext;
begin
 c := TRttiContext.Create;
 try
   result := GetAttributes(c, c.GetType(aType),aClass);
 finally
   c.Free;
 end;
end;

class function TAttrUtils.HasAttribute(aType: pTypeinfo;
  aClass: TCustomAttributeClass; var Attr: TCustomAttribute): Boolean;
var
 c : TRttiContext;
begin
 c := TRttiContext.Create;
 try
   result := HasAttribute(c, c.GetType(aType),aClass,Attr);
 finally
   c.Free;
 end;
end;

class function TAttrUtils.HasAttribute(aContext: TRttiContext; aType: TRttiObject;
  aClass: TCustomAttributeClass; var Attr: TCustomAttribute): Boolean;
begin
  Attr := GetAttribute(aContext,aType,aClass);
  result := Assigned(Attr);
end;

class function TAttrUtils.HasAttributes(aContext: TRttiContext;
  aType: TRttiObject; aClass: TCustomAttributeClass;
  var Attrs: TArray<TCustomAttribute>): Boolean;
begin
  Attrs := GetAttributes(aContext,aType,aClass);
  result := Length(Attrs) > 0;
end;

class function TAttrUtils.HasAttributes(aType: pTypeinfo;
  aClass: TCustomAttributeClass; var Attrs: TArray<TCustomAttribute>): Boolean;
var
 c : TRttiContext;
begin
 c := TRttiContext.Create;
 try
   result := HasAttributes(c, c.GetType(aType),aClass,Attrs);
 finally
   c.Free;
 end;
end;

{ TRttiEnumerator }

constructor TRttiEnumerator.Create(aEnumerator : TObject;aContext : TRttiContext; aCurrent :TRttiProperty; aMoveNext: TRttiMethod);
begin
  FCurrentValue := aCurrent;
  FMoveNext := aMoveNext;
  FInstance := aEnumerator;
  // Only need to keep a reference around to keep Method's from being freed.
  FContext := aContext;
end;

function TRttiEnumerator.DoGetCurrent: TValue;
begin
  result := FCurrentValue.GetValue(FInstance);
end;

function TRttiEnumerator.DoMoveNext: Boolean;
begin
  result := FMoveNext.Invoke(FInstance,[]).AsBoolean;
end;

{ TArrayEnumerator }

constructor TArrayEnumerator.Create(aArray: TValue);
begin
  FArray := aArray;
  FIndex := -1;
end;

function TArrayEnumerator.DoGetCurrent: TValue;
begin
  result := FArray.GetArrayElement(FIndex);
end;

function TArrayEnumerator.DoMoveNext: Boolean;
begin
  inc(FIndex);
  result := (FIndex > FArray.GetArrayLength);
end;

{ TEnumerableFactory }

constructor TEnumerableFactory.Create(aValue: TValue);
begin
  FValue := aValue;
end;

function TEnumerableFactory.CreateRttiEnum(aValue: TValue): TRttiEnumerator;
var
 lContext : TRttiContext;
 lGetEnum : TRttiMethod;
 lEnumerator : TValue;
 lMoveNext : TRttiMethod;
 lCurrent : TRttiProperty;
begin
 lContext := TRttiContext.Create;
 lGetEnum := lContext.GetType(aValue.TypeInfo).GetMethod('GetEnumerator');
 if Not Assigned(lGetEnum) then
    raise EEnumerableFactoryException.CreateFmt('No Enumerator Adapter avalable for Value Specified: %s',[FValue.TypeInfo.Name]);
 lEnumerator := lGetEnum.Invoke(aValue,[]);

 lMoveNext := lContext.GetType(lEnumerator.TypeInfo).GetMethod('MoveNext');
 lCurrent := lContext.GetType(lEnumerator.TypeInfo).GetProperty('Current');

 if Not Assigned(lMoveNext) then
    raise EEnumerableFactoryException.CreateFmt('GetEnumerator did not return a method named MoveNext',[FValue.TypeInfo.Name]);

 if Not Assigned(lCurrent) then
    raise EEnumerableFactoryException.CreateFmt('GetEnumerator did not return a property named Current',[FValue.TypeInfo.Name]);

 result := TRttiEnumerator.Create(lEnumerator.AsObject,lContext,lCurrent,lMoveNext);
end;

function TEnumerableFactory.DoGetEnumerator: TEnumerator<TValue>;
begin
  if FValue.IsEmpty then
     raise EEnumerableFactoryException.Create('Value Specified is Empty, DoGetEnumerator requires an assigned TValue');

  if FValue.IsArray then
  begin
     result := TArrayEnumerator.Create(FValue)
  end
  else if FValue.IsObject then
       begin
         result := CreateRttiEnum(FValue);
       end
       else raise EEnumerableFactoryException.CreateFmt('No Enumerator Adapter avalable for Value Type Specified: %s',[FValue.TypeInfo.Name]);
end;

class function TEnumerableFactory.IsTypeSupported(aType : TRttiType): Boolean;
var
 lContext : TRttiContext;
 lGetEnum : TRttiMethod;
 lEnumerator : TValue;
begin
 // Dynamic Arrays Supported
 result := (aType.TypeKind = tkDynArray);

 // if TObject then check for Enumerator
 if aType.IsInstance then
 begin
     result := true;
     lContext := TRttiContext.Create;
     lGetEnum := aType.GetMethod('GetEnumerator');
     if Not Assigned(lGetEnum) then
        exit(false);
     if not Assigned(lGetEnum.ReturnType) then
        exit(false);
     if not Assigned(lGetEnum.ReturnType.GetMethod('MoveNext')) then
        exit(false);
     if not Assigned(lGetEnum.ReturnType.GetProperty('Current')) then
        exit(false);
 end;
end;

{ TElementAdd }

procedure TElementAdd.AddFinalize;
begin
 // Do Nothing by default
end;

constructor TElementAdd.Create(aList: TValue);
begin
  FList := aList;
  if aList.IsEmpty then
     raise EElementAddException.Create('Empty TValue passed to TElementAdd.Create');
end;

{ TRttiElementAdd }

procedure TRttiElementAdd.Add(aAddElement: TValue);
begin
  FAddMethod.Invoke(FList,[aAddElement]);
end;

constructor TRttiElementAdd.Create(aList: TValue);
begin
  inherited;
  FContext := TRttiContext.Create;
  FAddMethod := FContext.GetType(aList.TypeInfo).GetMethod('Add');
  if Not Assigned(FAddMethod) then
     raise ERttiElementAddException.Create('Expected Add Method not found');
  if Length(FAddMethod.GetParameters) <> 1 then
     raise ERttiElementAddException.Create('Add Method with only one Parameter expected')
end;

class function TRttiElementAdd.GetAddType(aListType : pTypeInfo): pTypeInfo;
var
  lContext : TRttiContext;
  lAddMethod : TRttiMethod;
begin
  lContext := TRttiContext.Create;
  lAddMethod := lContext.GetType(aListType).GetMethod('Add');
  if Not Assigned(lAddMethod) then
     raise ERttiElementAddException.Create('Expected Add Method not found');
  if Length(lAddMethod.GetParameters) <> 1 then
     raise ERttiElementAddException.Create('Add Method with only one Parameter expected');
  result := lAddMethod.GetParameters[0].ParamType.Handle;
end;

class function TRttiElementAdd.TypeSupported(aListType: pTypeInfo): Boolean;
var
 lContext : TRttiContext;
 lAddMethod : TRttiMethod;
begin
  if aListType.Kind <> tkClass then
     exit(false);
  lContext := TRttiContext.Create;
  lAddMethod := lContext.GetType(aListType).GetMethod('Add');
  if Not Assigned(lAddMethod) then
     exit(false);
  if Length(lAddMethod.GetParameters) <> 1 then
     exit(false);
  result := true;
end;

{ TArrayElementAdd }

procedure TArrayElementAdd.Add(aAddElement: TValue);
begin
  FTempList.Add(aAddElement);
end;

procedure TArrayElementAdd.AddFinalize;
// Copy the FTempList to the End of the FList which will be an Array
var
  lNewArray : TValue;
  lNewArrayPtr : Pointer;
  Len : LongInt;
  I : Integer;
  lExistingArrayLen : Integer;
begin
  // Create New array
  TValue.Make(nil,FList.TypeInfo,lNewArray);
  // Set it's size and have to resort to lower levels as we don't have something that will work with SetLength()
  lNewArrayPtr := lNewArray.GetReferenceToRawData;
  lExistingArrayLen := FList.GetArrayLength;
  Len := lExistingArrayLen + FTempList.Count;
  DynArraySetLength(lNewArrayPtr,FList.TypeInfo,1,@Len);
  // Copy Existing Values to New Array
  for I := 0 to lExistingArrayLen - 1 do
     lNewArray.SetArrayElement(I,FList.GetArrayElement(I));
  // Copy Added Values to New Array
  for I := 0 to FTempList.Count -1 do
    lNewArray.SetArrayElement(I + lExistingArrayLen,FTempList.Items[I]);
  // Finally Replace old Array with New Array
  FList := lNewArray;
end;

constructor TArrayElementAdd.Create(aList: TValue);
begin
  inherited;
  if not (aList.Kind = tkDynArray) then
    raise EArrayElementAddException.Create('Expected an Dynamic array Type');
  FTempList := TList<TValue>.Create;
end;

destructor TArrayElementAdd.Destroy;
begin
  FTempList.Free;
  inherited;
end;

class function TArrayElementAdd.GetAddType(aListType : pTypeInfo): pTypeInfo;
var
 C : TRttiContext;
begin
  C := TRttiContext.Create;
  result := (C.GetType(aListType) as TRttiDynamicArrayType).ElementType.Handle;
end;

class function TArrayElementAdd.TypeSupported(aListType: pTypeInfo): Boolean;
begin
 result := aListType.Kind = tkDynArray;
end;

{ TElementAddFactory }

class function TElementAddFactory.CreateElementAdd(Value: TValue): TElementAdd;
begin
  if TArrayElementAdd.TypeSupported(Value.TypeInfo) then
     result := TArrayElementAdd.Create(Value)
  else if TRttiElementAdd.TypeSupported(Value.TypeInfo) then
         result := TRttiElementAdd.Create(value)
    else raise EElementAddException.CreateFmt('Unsupported TValue type: %s',[Value.TypeInfo.Name]);
end;


class function TElementAddFactory.GetAddType(aListType: pTypeInfo): pTypeInfo;
begin
  if TArrayElementAdd.TypeSupported(aListType) then
     result := TArrayElementAdd.GetAddType(aListType)
  else if TRttiElementAdd.TypeSupported(aListType) then
         result := TRttiElementAdd.GetAddType(aListType)
    else result := nil;
end;

class function TElementAddFactory.TypeSupported(Value: pTypeInfo): Boolean;
begin
 result := TRttiElementAdd.TypeSupported(Value) or TArrayElementAdd.TypeSupported(Value);
end;

end.
