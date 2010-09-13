unit uTypeReg;
interface
uses
  SysUtils,
  TypInfo,
  Rtti,
  Generics.Defaults,
  Generics.Collections;

type
  { Base for a type-based registry. A type based registry requires a bit more complex
    lookups then a key-value based map. For instance, many types, though having separate
    RTTI, share the same aspects. }
  TTypeRegistry<TKey, TInstance> = class abstract(TInterfacedObject)
  private
    FContext: TRttiContext;
    FCustomTypeMap: TDictionary<PTypeInfo, TKey>;

    { Internal. Will use TypeKind to fire the appropriate method. }
    function DoInstantiateStandard(const ARttiType: TRttiType): TInstance;
  protected
    { Instantiate the key into a product of type TInstance. Override in descendants. }
    function DoInstantiateCustom(const AKey: TKey): TInstance; virtual; abstract;

    { The following functions need to be overriden to provide type based instantiations }
    function DoInstantiateInt8(const AType: TRttiOrdinalType): TInstance; virtual; abstract;
    function DoInstantiateUInt8(const AType: TRttiOrdinalType): TInstance; virtual; abstract;
    function DoInstantiateInt16(const AType: TRttiOrdinalType): TInstance; virtual; abstract;
    function DoInstantiateUInt16(const AType: TRttiOrdinalType): TInstance; virtual; abstract;
    function DoInstantiateInt32(const AType: TRttiOrdinalType): TInstance; virtual; abstract;
    function DoInstantiateUInt32(const AType: TRttiOrdinalType): TInstance; virtual; abstract;
    function DoInstantiateInt64(const AType: TRttiInt64Type): TInstance; virtual; abstract;
    function DoInstantiateUInt64(const AType: TRttiInt64Type): TInstance; virtual; abstract;
    function DoInstantiateSingle(const AType: TRttiFloatType): TInstance; virtual; abstract;
    function DoInstantiateDouble(const AType: TRttiFloatType): TInstance; virtual; abstract;
    function DoInstantiateExtended(const AType: TRttiFloatType): TInstance; virtual; abstract;
    function DoInstantiateComp(const AType: TRttiFloatType): TInstance; virtual; abstract;
    function DoInstantiateCurrency(const AType: TRttiFloatType): TInstance; virtual; abstract;
    function DoInstantiateAnsiChar(const AType: TRttiOrdinalType): TInstance; virtual; abstract;
    function DoInstantiateWideChar(const AType: TRttiOrdinalType): TInstance; virtual; abstract;
    function DoInstantiateShortString(const AType: TRttiStringType): TInstance; virtual; abstract;
    function DoInstantiateAnsiString(const AType: TRttiStringType): TInstance; virtual; abstract;
    function DoInstantiateWideString(const AType: TRttiStringType): TInstance; virtual; abstract;
    function DoInstantiateUnicodeString(const AType: TRttiStringType): TInstance; virtual; abstract;
    function DoInstantiateClass(const AType: TRttiInstanceType): TInstance; virtual; abstract;
    function DoInstantiatePointer(const AType: TRttiPointerType): TInstance; virtual; abstract;
    function DoInstantiateMethod(const AType: TRttiMethodType): TInstance; virtual; abstract;
    function DoInstantiateInterface(const AType: TRttiInterfaceType): TInstance; virtual; abstract;
    function DoInstantiateMetaclass(const AType: TRttiClassRefType): TInstance; virtual; abstract;
    function DoInstantiateProcedure(const AType: TRttiProcedureType): TInstance; virtual; abstract;
    function DoInstantiateVariant(const AType: TRttiType): TInstance; virtual; abstract;
    function DoInstantiateStaticArray(const AType: TRttiArrayType): TInstance; virtual; abstract;
    function DoInstantiateDynamicArray(const AType: TRttiDynamicArrayType): TInstance; virtual; abstract;
    function DoInstantiateRecord(const AType: TRttiRecordType): TInstance; virtual; abstract;
    function DoInstantiateEnumeration(const AType: TRttiEnumerationType): TInstance; virtual; abstract;
    function DoInstantiateSet(const AType: TRttiSetType): TInstance; virtual; abstract;
  public
    { Construction & Destruction }
    constructor Create;
     destructor Destroy; override;

    { Registration of custom types }
    procedure Register(const ATypeInfo: PTypeInfo; const AKey: TKey);
    procedure Unregister(const ATypeInfo: PTypeInfo);

    { Lookup of types }
    function GetObjectForType(const ATypeInfo: PTypeInfo): TInstance;
  end;


type
  IComparer = interface
    function Compare(const Left, Right: TValue): Integer;
  end;

  TComparer = class(TInterfacedObject, IComparer)
    function Compare(const Left, Right: TValue): Integer; virtual; abstract;
    constructor Create; virtual; abstract;
  end;

  TComparerClass = class of TComparer;

  TComparer<T> = class(TComparer, IComparer<T>)
    function Compare(const Left, Right: T): Integer; virtual; abstract;
  end;

  TComparerTypeRegistry = class(TTypeRegistry<TComparerClass, IComparer>)
  protected
    { Instantiate the key into a product of type TInstance. Override in descendants. }
    function DoInstantiateCustom(const AKey: TComparerClass): IComparer; override;

    { The following functions need to be overriden to provide type based instantiations }
    function DoInstantiateInt8(const AType: TRttiOrdinalType): IComparer; override;
    function DoInstantiateUInt8(const AType: TRttiOrdinalType): IComparer; override;
    function DoInstantiateInt16(const AType: TRttiOrdinalType): IComparer; override;
    function DoInstantiateUInt16(const AType: TRttiOrdinalType): IComparer; override;
    function DoInstantiateInt32(const AType: TRttiOrdinalType): IComparer; override;
    function DoInstantiateUInt32(const AType: TRttiOrdinalType): IComparer; override;
    function DoInstantiateInt64(const AType: TRttiInt64Type): IComparer; override;
    function DoInstantiateUInt64(const AType: TRttiInt64Type): IComparer; override;
    function DoInstantiateSingle(const AType: TRttiFloatType): IComparer; override;
    function DoInstantiateDouble(const AType: TRttiFloatType): IComparer; override;
    function DoInstantiateExtended(const AType: TRttiFloatType): IComparer; override;
    function DoInstantiateComp(const AType: TRttiFloatType): IComparer; override;
    function DoInstantiateCurrency(const AType: TRttiFloatType): IComparer; override;
    function DoInstantiateAnsiChar(const AType: TRttiOrdinalType): IComparer; override;
    function DoInstantiateWideChar(const AType: TRttiOrdinalType): IComparer; override;
    function DoInstantiateShortString(const AType: TRttiStringType): IComparer; override;
    function DoInstantiateAnsiString(const AType: TRttiStringType): IComparer; override;
    function DoInstantiateWideString(const AType: TRttiStringType): IComparer; override;
    function DoInstantiateUnicodeString(const AType: TRttiStringType): IComparer; override;
    function DoInstantiateClass(const AType: TRttiInstanceType): IComparer; override;
    function DoInstantiatePointer(const AType: TRttiPointerType): IComparer; override;
    function DoInstantiateMethod(const AType: TRttiMethodType): IComparer; override;
    function DoInstantiateInterface(const AType: TRttiInterfaceType): IComparer; override;
    function DoInstantiateMetaclass(const AType: TRttiClassRefType): IComparer; override;
    function DoInstantiateProcedure(const AType: TRttiProcedureType): IComparer; override;
    function DoInstantiateVariant(const AType: TRttiType): IComparer; override;
    function DoInstantiateStaticArray(const AType: TRttiArrayType): IComparer; override;
    function DoInstantiateDynamicArray(const AType: TRttiDynamicArrayType): IComparer; override;
    function DoInstantiateRecord(const AType: TRttiRecordType): IComparer; override;
    function DoInstantiateEnumeration(const AType: TRttiEnumerationType): IComparer; override;
    function DoInstantiateSet(const AType: TRttiSetType): IComparer; override;
  end;

implementation

{ TTypeRegistry<TKey, TInstance> }

constructor TTypeRegistry<TKey, TInstance>.Create;
begin
  inherited;

  { Instantiate internal resources }
  FCustomTypeMap := TDictionary<PTypeInfo, TKey>.Create();
  FContext := TRttiContext.Create();
end;

destructor TTypeRegistry<TKey, TInstance>.Destroy;
begin
  { Free internal resources }
  FCustomTypeMap.Free;
  FContext.Free;

  inherited;
end;

function TTypeRegistry<TKey, TInstance>.DoInstantiateStandard(const ARttiType: TRttiType): TInstance;
begin
  ASSERT(Assigned(ARttiType));

  { Call the specific instantiation methods based on the specific types }
  case ARttiType.TypeKind of
    tkUnknown:
      ASSERT(false); // Implement me

    tkInteger:
    begin
      { Select the appropriate integer type }
      case TRttiOrdinalType(ARttiType).OrdType of
        otSByte:
          Result := DoInstantiateInt8(TRttiOrdinalType(ARttiType));

        otUByte:
          Result := DoInstantiateUInt8(TRttiOrdinalType(ARttiType));

        otSWord:
          Result := DoInstantiateInt16(TRttiOrdinalType(ARttiType));

        otUWord:
          Result := DoInstantiateUInt16(TRttiOrdinalType(ARttiType));

        otSLong:
          Result := DoInstantiateInt32(TRttiOrdinalType(ARttiType));

        otULong:
          Result := DoInstantiateUInt32(TRttiOrdinalType(ARttiType));

        else
          ASSERT(false); // Maybe a new types is added here anytime in the future!
      end;
    end;

    tkChar:
      Result := DoInstantiateAnsiChar(TRttiOrdinalType(ARttiType));

    tkEnumeration:
      Result := DoInstantiateEnumeration(TRttiEnumerationType(ARttiType));

    tkFloat:
    begin
      { Select the appropriate integer type }
      case TRttiFloatType(ARttiType).FloatType of
        ftSingle:
          Result := DoInstantiateSingle(TRttiFloatType(ARttiType));

        ftDouble:
          Result := DoInstantiateDouble(TRttiFloatType(ARttiType));

        ftExtended:
          Result := DoInstantiateExtended(TRttiFloatType(ARttiType));

        ftComp:
          Result := DoInstantiateComp(TRttiFloatType(ARttiType));

        ftCurr:
          Result := DoInstantiateCurrency(TRttiFloatType(ARttiType));

        else
          ASSERT(false); // Maybe a new types is added here anytime in the future!
      end;
    end;

    tkString:
      Result := DoInstantiateShortString(TRttiStringType(ARttiType));

    tkSet:
      Result := DoInstantiateSet(TRttiSetType(ARttiType));

    tkClass:
      Result := DoInstantiateClass(TRttiInstanceType(ARttiType));

    tkMethod:
      Result := DoInstantiateMethod(TRttiMethodType(ARttiType));

    tkWChar:
      Result := DoInstantiateWideChar(TRttiOrdinalType(ARttiType));

    tkLString:
      Result := DoInstantiateAnsiString(TRttiStringType(ARttiType));

    tkWString:
      Result := DoInstantiateWideString(TRttiStringType(ARttiType));

    tkVariant:
      Result := DoInstantiateVariant(TRttiType(ARttiType));

    tkArray:
      Result := DoInstantiateStaticArray(TRttiArrayType(ARttiType));

    tkRecord:
      Result := DoInstantiateRecord(TRttiRecordType(ARttiType));

    tkInterface:
      Result := DoInstantiateInterface(TRttiInterfaceType(ARttiType));

    tkInt64:
    begin
      { Select the appropriate int 64 type }
      if TRttiInt64Type(ARttiType).MaxValue > TRttiInt64Type(ARttiType).MinValue then
        Result := DoInstantiateInt64(TRttiInt64Type(ARttiType))
      else
        Result := DoInstantiateUInt64(TRttiInt64Type(ARttiType));
    end;

    tkDynArray:
      Result := DoInstantiateDynamicArray(TRttiDynamicArrayType(ARttiType));

    tkUString:
      Result := DoInstantiateUnicodeString(TRttiStringType(ARttiType));

    tkClassRef:
      Result := DoInstantiateMetaclass(TRttiClassRefType(ARttiType));

    tkPointer:
      Result := DoInstantiatePointer(TRttiPointerType(ARttiType));

    tkProcedure:
      Result := DoInstantiateProcedure(TRttiProcedureType(ARttiType));

    else
      ASSERT(false); // Maybe a new types is added here anytime in the future!
  end;
end;

function TTypeRegistry<TKey, TInstance>.GetObjectForType(
  const ATypeInfo: PTypeInfo): TInstance;
var
  LKey: TKey;
  LHasCustom: Boolean;
begin
  { Check input }
  if not Assigned(ATypeInfo) then
    raise EArgumentNilException.Create('ATypeInfo is nil.');

  { Check whether there is a custom type registered }
  MonitorEnter(FCustomTypeMap);
  try
    LHasCustom := FCustomTypeMap.TryGetValue(ATypeInfo, LKey);
  finally
    MonitorExit(FCustomTypeMap);
  end;

  { Simply instantiate the stuff }
  if LHasCustom then
    Result := DoInstantiateCustom(LKey)
  else
    { OK, there is no custom type associated here. Let's go for defaults. }
    Result := DoInstantiateStandard(FContext.GetType(ATypeInfo));
end;

procedure TTypeRegistry<TKey, TInstance>.Register(const ATypeInfo: PTypeInfo; const AKey: TKey);
begin
  { Check input }
  if not Assigned(ATypeInfo) then
    raise EArgumentNilException.Create('ATypeInfo is nil.');

  { Check whether there is a custom type registered }
  MonitorEnter(FCustomTypeMap);
  try
    if FCustomTypeMap.ContainsKey(ATypeInfo) then
      raise EArgumentException.Create('ATypeInfo is alredy associated with another value.');

    { Register into teh map }
    FCustomTypeMap.Add(ATypeInfo, AKey);
  finally
    MonitorExit(FCustomTypeMap);
  end;
end;

procedure TTypeRegistry<TKey, TInstance>.Unregister(const ATypeInfo: PTypeInfo);
begin
  { Check input }
  if not Assigned(ATypeInfo) then
    raise EArgumentNilException.Create('ATypeInfo is nil.');

  { Check whether there is a custom type registered }
  MonitorEnter(FCustomTypeMap);
  try
    if not FCustomTypeMap.ContainsKey(ATypeInfo) then
      raise EArgumentException.Create('ATypeInfo is not associated with a value.');

    { Register into teh map }
    FCustomTypeMap.Remove(ATypeInfo);
  finally
    MonitorExit(FCustomTypeMap);
  end;
end;

end.
