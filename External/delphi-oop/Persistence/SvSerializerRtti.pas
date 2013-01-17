unit SvSerializerRtti;

interface

uses
  Rtti, Generics.Collections, TypInfo
  ;

type
  TConstructorMethod = reference to function(): TObject;

  TSvRttiInfo = class
  strict private
    class var
      FCtx: TRttiContext;
    class var
      FRegisteredConstructors: TDictionary<PTypeInfo,TConstructorMethod>;
    class constructor Create;
    class destructor Destroy;
  public
    class property Context: TRttiContext read FCtx;
    class function FindType(const AQualifiedName: string): TRttiType;
    class function GetType(ATypeInfo: Pointer): TRttiType; overload;
    class function GetType(AClass: TClass): TRttiType; overload;
    class function GetType(const Value: TValue): TRttiType; overload;
    class function GetTypes: TArray<TRttiType>;
    class function GetPackages: TArray<TRttiPackage>;
    class function GetBasicMethod(const AMethodName: string; AType: TRttiType): TRttiMethod;
    class procedure SetValue(AProp: TRttiProperty; const AInstance, AValue: TValue); overload;
    class procedure SetValue(AField: TRttiField; const AInstance, AValue: TValue); overload;
    class function GetValue(AProp: TRttiProperty; const AInstance: TValue): TValue;

    class function CreateType(ATypeInfo: PTypeInfo): TObject; overload;

    class procedure RegisterConstructor(ATypeInfo: PTypeInfo; AMethod: TConstructorMethod);
  end;

implementation

uses
  SysUtils
  ;


{ TSvRttiInfo }

class constructor TSvRttiInfo.Create;
begin
  FCtx := TRttiContext.Create;
  FRegisteredConstructors := TDictionary<PTypeInfo,TConstructorMethod>.Create;
end;

class destructor TSvRttiInfo.Destroy;
begin
  FCtx.Free;
  FRegisteredConstructors.Free;
end;

class function TSvRttiInfo.FindType(const AQualifiedName: string): TRttiType;
begin
  Result := FCtx.FindType(AQualifiedName);
end;

class function TSvRttiInfo.GetBasicMethod(const AMethodName: string; AType: TRttiType): TRttiMethod;
var
  LMethod: TRttiMethod;
  iParCount, iCurrParCount, iCount: Integer;
begin
  LMethod := nil;
  iParCount := 0;
  iCurrParCount := 0;
  for Result in AType.GetMethods do
  begin
    if SameText(Result.Name, AMethodName) then
    begin
      iCount := Length(Result.GetParameters);
      if (iCount < iParCount) or (iCount = 0) then
      begin
        Exit;
      end
      else
      begin
        if (iCount > iCurrParCount) then
        begin
          Inc(iParCount);
        end;

        iCurrParCount := iCount;
        LMethod := Result;
      end;
    end;
  end;

  Result := LMethod;
end;

class function TSvRttiInfo.GetPackages: TArray<TRttiPackage>;
begin
  Result := FCtx.GetPackages;
end;

class function TSvRttiInfo.GetType(AClass: TClass): TRttiType;
begin
  Result := FCtx.GetType(AClass);
end;

class function TSvRttiInfo.GetTypes: TArray<TRttiType>;
begin
  Result := FCtx.GetTypes;
end;

class function TSvRttiInfo.GetValue(AProp: TRttiProperty; const AInstance: TValue): TValue;
begin
  if AInstance.IsObject then
    Result := AProp.GetValue(AInstance.AsObject)
  else
    Result := AProp.GetValue(AInstance.GetReferenceToRawData);
end;

class procedure TSvRttiInfo.RegisterConstructor(ATypeInfo: PTypeInfo; AMethod: TConstructorMethod);
begin
  FRegisteredConstructors.AddOrSetValue(ATypeInfo, AMethod);
end;

class procedure TSvRttiInfo.SetValue(AField: TRttiField; const AInstance, AValue: TValue);
begin
  if AInstance.IsObject then
    AField.SetValue(AInstance.AsObject, AValue)
  else
    AField.SetValue(AInstance.GetReferenceToRawData, AValue);
end;

class procedure TSvRttiInfo.SetValue(AProp: TRttiProperty; const AInstance, AValue: TValue);
begin
  if AInstance.IsObject then
    AProp.SetValue(AInstance.AsObject, AValue)
  else
    AProp.SetValue(AInstance.GetReferenceToRawData, AValue);
end;

class function TSvRttiInfo.GetType(ATypeInfo: Pointer): TRttiType;
begin
  Result := FCtx.GetType(ATypeInfo);
end;

class function TSvRttiInfo.GetType(const Value: TValue): TRttiType;
begin
  Result := GetType(Value.TypeInfo);
end;

class function TSvRttiInfo.CreateType(ATypeInfo: PTypeInfo): TObject;
var
  LType: TRttiType;
  LMethCreate: TRttiMethod;
  LInstanceType: TRttiInstanceType;
  LRegisteredConstructorMethod: TConstructorMethod;
begin
  if FRegisteredConstructors.TryGetValue(ATypeInfo, LRegisteredConstructorMethod) then
  begin
    Result := LRegisteredConstructorMethod();
  end
  else
  begin
    LType := GetType(ATypeInfo);
    for LMethCreate in LType.GetMethods do
    begin
      if (LMethCreate.IsConstructor) and (Length(LMethCreate.GetParameters) = 0)  then
      begin
        LInstanceType := LType.AsInstance;

        Result := LMethCreate.Invoke(LInstanceType.MetaclassType, []).AsObject;
        Exit;
      end;
    end;
    Result := nil;
  end;
end;

end.
