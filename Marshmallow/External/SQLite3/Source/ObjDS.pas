unit ObjDS;

interface
uses
  Sysutils, Classes, db, DBClient, TypInfo, RTTI, RttiUtils, Generics.Collections;
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

type
  EDataSetMappingError = class (Exception);

  TMemberVisibilitySet = set of TMemberVisibility;

  TFieldMap = class(TCustomAttribute)
  private
    FName: String;
    FRequired: Boolean;
    FDataType: TFieldType;
    FSize: Integer;
    FGetData : TFunc<TField, TValue>;
    FSetData : TProc<TValue, TField>;
    procedure SetDataType(const Value: TFieldType);
    procedure SetName(const Value: String);
    procedure SetRequired(const Value: Boolean);
    procedure SetSize(const Value: Integer);
  public
    constructor Create(aName: string; aDataType: TFieldType; aSize: Integer; aRequired: Boolean);   overload;
    constructor Create(aName: string; aDataType: TFieldType; aSize: Integer; aRequired: Boolean;aGetData : TFunc<TField,TValue>;aSetData : TProc<TValue,TField>);   overload;
    property Name : String read FName write SetName;
    property DataType : TFieldType read FDataType write SetDataType;
    property Size : Integer read FSize write SetSize;
    property Required : Boolean read FRequired write SetRequired;
    property GetData : TFunc<TField,TValue> read FGetData write FGetData;
    property SetData : TProc<TValue,TField> read FSetData write FSetData;
  end;

  TFieldNoMap = class(TCustomAttribute)
  end;

  TFieldTypeMapping = class
    class function SetIntToInt : TProc<TValue,TField>;
    class function GetIntToInt : TFunc<TField,TValue>;
    class function SetStrToStr : TProc<TValue,TField>;
    class function GetStrToStr : TFunc<TField,TValue>;
    class function SetFloatToFloat : TProc<TValue,TField>;
    class function GetFloatToFloat : TFunc<TField,TValue>;

    class function GetStrToTest : TFunc<TField,TValue>;

    class function SetCurrencyToCurrency : TProc<TValue,TField>;
    class function GetCurrencyToCurrency : TFunc<TField,TValue>;

    class function SetBoolToBool : TProc<TValue,TField>;
    class function GetBoolToBool : TFunc<TField,TValue>;
    class function SetDateTimeToDateTime : TProc<TValue,TField>;
    class function GetDateTimeToDateTime : TFunc<TField,TValue>;

    class function FindGetMapping(FieldType : TFieldType;MemberType : TRttiType) : TFunc<TField,TValue>;
    class function FindSetMapping(FieldType : TFieldType;MemberType : TRttiType) : TProc<TValue,TField>;
  end;



  TDataSetObjMap = record
  public
    FieldDef : TFieldDef;
    Member   : TRttiMember;
    SetData  : TProc<TValue,TField>;
    GetData  : TFunc<TField,TValue>;
    class function CreateMap(aCtx : TRttiContext;aMember : TRttiMember;aDataSet : TDataset;aVisibility : TMemberVisibilitySet = [mvPublic,mvPublished] ) : TDataSetObjMap; static;
  end;

  TMapListType = (mtStruct, mtList); //Maybe add array support later
  TDataSetMapping = class
  protected
    FMapListType : TMapListType;
    FMapping : TList<TDataSetObjMap>;
    FMappedType: pTypeInfo;
    FDataSet : TCustomClientDataSet;
    FCtx : TRttiContext;
    FValue: TValue;
    procedure OpenDataSet;
    procedure DefineDS;
    procedure SetMappedType(const Value: pTypeInfo);
    procedure SetValue(const Value: TValue);
  protected
  public
    constructor Create(aDataSet : TCustomClientDataSet); virtual;
    destructor Destroy; override;
    property MappedType : pTypeInfo read FMappedType write SetMappedType;
    // To Open Dataset and populate values set the value property
    property Value : TValue read FValue write SetValue;

    procedure PopulateRow(Value : TValue);
    procedure PersitRow(Value : TValue); //Yes this is here but it's read only right now.
    procedure PopulateDataSet(Value : TValue);
    property DataSet : TCustomClientDataset read FDataSet;

  end;




implementation

{ TFieldMap }

constructor TFieldMap.Create(aName: string; aDataType: TFieldType;
  aSize: Integer; aRequired: Boolean);
begin
  FName := aName;
  FDataType := aDataType;
  FSize := aSize;
  FRequired := aRequired;
  FGetData := nil;
  FSetData := nil;
end;

constructor TFieldMap.Create(aName: string; aDataType: TFieldType;
  aSize: Integer; aRequired: Boolean; aGetData: TFunc<TField, TValue>;
  aSetData: TProc<TValue, TField>);
begin
  FName := aName;
  FDataType := aDataType;
  FSize := aSize;
  FRequired := aRequired;
  FGetData := aGetData;
  FSetData := aSetData;
end;

procedure TFieldMap.SetDataType(const Value: TFieldType);
begin
  FDataType := Value;
end;


procedure TFieldMap.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TFieldMap.SetRequired(const Value: Boolean);
begin
  FRequired := Value;
end;


procedure TFieldMap.SetSize(const Value: Integer);
begin
  FSize := Value;
end;

{ TDataSetObjMap }

class function TDataSetObjMap.CreateMap(aCtx: TRttiContext;
  aMember: TRttiMember; aDataSet: TDataset;aVisibility : TMemberVisibilitySet): TDataSetObjMap;
var
 Attr : TCustomAttribute;
 MapAttr : TFieldMap;
 MemberType : TRttiType;
begin
// TODO: Set - SetData and GetData Members
  if TAttrUtils.HasAttribute(aCtx,aMember,TFieldMap,Attr) then
  begin
    MapAttr := TFieldMap(Attr);
    Result.Member := aMember;
    Result.FieldDef := TFieldDef.Create(aDataSet.FieldDefs, MapAttr.Name,
                                        MapAttr.DataType, MapAttr.Size,
                                        MapAttr.Required,0);

   if aMember is TRttiProperty then
      MemberType := TRttiProperty(aMember).propertyType
   else
      MemberType := (aMember as TRttiField).FieldType;
   if Assigned(MapAttr.GetData) then
      result.GetData := MapAttr.GetData
   else
      result.GetData := TFieldTypeMapping.FindGetMapping(MapAttr.DataType,MemberType);
   if Assigned(MapAttr.SetData) then
      result.SetData := MapAttr.SetData
   else
     result.SetData := TFieldTypeMapping.FindSetMapping(MapAttr.DataType,MemberType);
  end
  else
  begin
    result.member := nil;
    result.FieldDef := nil;
    if Not TAttrUtils.HasAttribute(aCtx,aMember,TFieldNoMap,Attr) then
    begin
       if aMember.Visibility in aVisibility then
       begin
          // TODO: Infer Field Mapping from Type
       end
    end;
  end;

  if Assigned(result.member) and (Not Assigned(result.GetData) or Not Assigned(result.SetData)) then
    raise EDataSetMappingError.Create('Unable To Map Type');
end;

{ TDataSetMapping }

constructor TDataSetMapping.Create(aDataSet: TCustomClientDataSet);
begin
  FCtx     := TRttiContext.Create;
  FDataSet := aDataSet;
  FMappedType := nil;
  FMapping := TList<TDataSetObjMap>.Create;
  FMapListType := mtStruct;
end;

procedure TDataSetMapping.DefineDS;
var
 aType : TRttiType;
 Field : TRttiField;
 Prop  : TRttiProperty;
 Map   : TDataSetObjMap;
 EnumMethod : TRttiMethod;
begin
 FMapping.Clear;
 aType := FCtx.GetType(FMappedType);

 if aType.TypeKind in [tkArray,tkDynArray] then
 begin
   raise EDataSetMappingError.Create('Array Type''s are currently not supported.');
 end;

 EnumMethod := aType.GetMethod('GetEnumerator');
 if Assigned(EnumMethod) then
 begin
   FMapListType := mtList;
   aType := EnumMethod.ReturnType.GetProperty('Current').PropertyType;
 end
 else
   FMapListType := mtStruct;

 for Field in aType.GetFields do
 begin
   Map := TDataSetObjMap.CreateMap(FCtx,Field,FDataSet);
   if Assigned(Map.Member) then
   begin
     FMapping.Add(Map);
   end;
 end;

 for Prop in aType.GetProperties do
 begin
   Map := TDataSetObjMap.CreateMap(FCtx,Prop,FDataSet);
   if Assigned(Map.Member) then
   begin
     FMapping.Add(Map);
   end;
 end;


end;

destructor TDataSetMapping.Destroy;
begin
  FreeAndNil(FMapping);
  FDataSet := nil;
  FCtx.Free;
  inherited;
end;

procedure TDataSetMapping.OpenDataSet;
begin
  if FDataSet.Active then
     FDataSet.Close;
  FDataSet.CreateDataSet;
  FDataset.Open;
end;

procedure TDataSetMapping.PersitRow(Value: TValue);
var
 Map : TDataSetObjMap;
 Data : TValue;
 ValuePtr : Pointer;
begin
 for Map in FMapping do
 begin
    if Value.isObject then
       ValuePtr := Value.asObject
    else
       ValuePtr := Value.GetReferenceToRawData;
    Data := Map.GetData(FDataSet.FieldByName(Map.FieldDef.Name));
    if Map.Member is TRttiProperty then
      TrttiProperty(Map.Member).SetValue(ValuePtr ,Data)
    else
      (Map.Member as TrttiField).SetValue(ValuePtr,Data);
 end;
end;

procedure TDataSetMapping.PopulateDataSet(Value: TValue);
var
 lEnumMethod : TRttiMethod;
 lType : TRttiType;
 lEnumerator : TValue;
 lEnumType : TRttiType;
 lMoveNextMethod : TRttiMethod;
 lCurrentProp : TRttiProperty;
 ValuePtr : Pointer;
begin
 if FMapListType = mtStruct then
 begin
    FDataset.Append;
    PopulateRow(Value);
    FDataset.Post;
 end
 else
 begin
   lType := FCtx.GetType(FMappedType);
   lEnumMethod := lType.GetMethod('GetEnumerator');
   lEnumerator := lEnumMethod.Invoke(Value,[]);
   lEnumType :=  FCtx.GetType(lEnumerator.TypeInfo);
   lMoveNextMethod := lEnumType.GetMethod('MoveNext');
   lCurrentProp := lEnumType.GetProperty('Current');
   Assert(Assigned(LMoveNextMethod),'MoveNext method not found');
   Assert(Assigned(lCurrentProp),'Current property not found');
   while lMoveNextMethod.Invoke(lEnumerator.AsObject,[]).asBoolean do
   begin
      FDataset.Append;
      PopulateRow(lCurrentProp.GetValue(lEnumerator.AsObject));
      FDataSet.post;
   end;
   //Linas memory leak fix
   if lEnumerator.IsObject then
   begin
     lEnumerator.AsObject.Free;
   end;
 end;
end;

procedure TDataSetMapping.PopulateRow(Value: TValue);
var
 Map : TDataSetObjMap;
 Data : TValue;
 ValuePtr : Pointer;
begin
 for Map in FMapping do
 begin
    if Value.isObject then
       ValuePtr := Value.asObject
    else
       ValuePtr := Value.GetReferenceToRawData;

    if Map.Member is TRttiProperty then
      Data := TrttiProperty(Map.Member).GetValue(ValuePtr)
    else
      Data := (Map.Member as TrttiField).GetValue(ValuePtr);
    Map.SetData(Data, FDataSet.FieldByName(Map.FieldDef.Name));
 end;
end;

procedure TDataSetMapping.SetMappedType(const Value: pTypeInfo);
begin
  FMappedType := Value;
  if Assigned(FMappedType) then
  begin
    DefineDS;
  end
  else
  begin
    FMapping.Clear;
    FDataSet.Free;
  end;
end;

procedure TDataSetMapping.SetValue(const Value: TValue);
begin
  FValue := Value;
  if FValue.isEmpty then
  begin
    FDataSet.Close;
  end
  else
  begin
     OpenDataSet;
     PopulateDataSet(FValue);
  end;
end;


{ TFieldTypeMapping }

class function TFieldTypeMapping.FindGetMapping(FieldType: TFieldType;
  MemberType: TRttiType): TFunc<TField, TValue>;
begin
  result := nil;
  case FieldType of
    ftInteger : begin
                   if MemberType.TypeKind = tkInteger then
                      result := TFieldTypeMapping.GetIntToInt();
                end;
    ftFloat   : begin
                   if MemberType.TypeKind = tkFloat then
                      result := TFieldTypeMapping.GetFloatToFloat()
                end;
    ftString, ftWideString  : begin
                   if MemberType.TypeKind in [tkString,tkUString, tkLString, tkWString]  then
                      result := TFieldTypeMapping.GetStrToStr();
                end;
    ftDateTime : begin
                   // Let any float be assigned to TDateTime
                   if MemberType.TypeKind = tkFloat then
                      result := TFieldTypeMapping.GetDateTimeToDateTime();
                 end;
    ftCurrency : begin
                   if MemberType.QualifiedName = 'System.Currency' then
                      result := TFieldTypeMapping.GetCurrencyToCurrency();
                 end;
    ftBoolean : begin
                   if MemberType.QualifiedName = 'System.Boolean' then
                      result := TFieldTypeMapping.GetBoolToBool();
                end;
  end;
end;

class function TFieldTypeMapping.FindSetMapping(FieldType: TFieldType;
  MemberType: TRttiType): TProc<TValue, TField>;
begin
  result := nil;
  case FieldType of
    ftInteger : begin
                   if MemberType.TypeKind = tkInteger then
                      result := TFieldTypeMapping.SetIntToInt();
                end;
    ftFloat   : begin
                   if MemberType.TypeKind = tkFloat then
                      result := TFieldTypeMapping.SetFloatToFloat()
                end;
    ftString, ftWideString  : begin
                   if MemberType.TypeKind in [tkString,tkUString, tkLString, tkWString]  then
                      result := TFieldTypeMapping.SetStrToStr();
                end;
    ftDateTime : begin
                   // Let any float be assigned to TDateTime
                   if MemberType.TypeKind = tkFloat then
                      result := TFieldTypeMapping.SetDateTimeToDateTime();
                 end;
    ftCurrency : begin
                   if MemberType.QualifiedName = 'System.Currency' then
                      result := TFieldTypeMapping.SetCurrencyToCurrency();
                 end;
    ftBoolean : begin
                   if MemberType.QualifiedName = 'System.Boolean' then
                      result := TFieldTypeMapping.SetBoolToBool();
                end;
  end;
end;

class function TFieldTypeMapping.GetBoolToBool: TFunc<TField, TValue>;
begin
 result := function(Field : TField) : TValue
 begin
   result := Field.asBoolean;
 end;
end;

class function TFieldTypeMapping.GetCurrencyToCurrency: TFunc<TField, TValue>;
begin
 result := function(Field : TField) : TValue
 begin
   result := Field.asCurrency;
 end;
end;

class function TFieldTypeMapping.GetDateTimeToDateTime: TFunc<TField, TValue>;
begin
 result := function(Field : TField) : TValue
 begin
   result := Field.asDateTime;
 end;
end;

class function TFieldTypeMapping.GetFloatToFloat: TFunc<TField, TValue>;
begin
 result := function(Field : TField) : TValue
 begin
   result := Field.asFloat;
 end;
end;

class function TFieldTypeMapping.GetIntToInt: TFunc<TField, TValue>;
begin
 result := function(Field : TField) : TValue
           begin
             result := Field.asInteger;
           end;
end;

class function TFieldTypeMapping.GetStrToStr: TFunc<TField, TValue>;
begin
 result := function(Field : TField) : TValue
 begin
   result := Field.AsWideString;
 end;
end;

class function TFieldTypeMapping.GetStrToTest: TFunc<TField, TValue>;
begin
 result := function(Field : TField) : TValue
 begin
   result := 'Test' ;
 end;
end;

class function TFieldTypeMapping.SetBoolToBool: TProc<TValue, TField>;
begin
 result := procedure(Value : TValue;Field : TField)
 begin
   Field.asBoolean := Value.AsBoolean;
 end;
end;

class function TFieldTypeMapping.SetCurrencyToCurrency: TProc<TValue, TField>;
begin
 result := procedure(Value : TValue;Field : TField)
 begin
   Field.asCurrency := Value.AsCurrency;
 end;
end;

class function TFieldTypeMapping.SetDateTimeToDateTime: TProc<TValue, TField>;
begin
 result := procedure(Value : TValue;Field : TField)
 begin
   Field.asDateTime := Value.AsExtended;
 end;
end;

class function TFieldTypeMapping.SetFloatToFloat: TProc<TValue, TField>;
begin
 result := procedure(Value : TValue;Field : TField)
 begin
   Field.asFloat := Value.AsExtended;
 end;
end;

class function TFieldTypeMapping.SetIntToInt: TProc<TValue, TField>;
begin
 result := procedure(Value : TValue;Field : TField)
 begin
   Field.asInteger := Value.asInteger;
 end;
end;

class function TFieldTypeMapping.SetStrToStr: TProc<TValue, TField>;
begin
 result := procedure(Value : TValue;Field : TField)
 begin
   Field.AsWideString := Value.asString;
 end;
end;

end.
