unit Spring.Persistence.Core.Repository.Proxy;

{$I sv.inc}

interface

{$IF CompilerVersion < 23}
{$Message Fatal 'Proxy repository supports XE2 or higher'}
{$IFEND}

uses
  Rtti, TypInfo, Spring.Persistence.Core.Interfaces, Spring.Persistence.Core.Session
  , Spring.Persistence.Core.Repository.Simple, Spring.Collections
  ;

type
  TMethodReference = reference to function(const Args: TArray<TValue>): TValue;

  TConstArray = array of TVarRec;

  TProxyRepository<T: class, constructor; TID> = class(TVirtualInterface)
  private
    FSimpleRepository: IPagedRepository<T,TID>;
    FSession: TSession;
    FDefaultMethods: IDictionary<string,TMethodReference>;
    FTypeName, FQualifiedTypeName: string;
    FIdTypeName, FQualifiedIdTypeName: string;
  protected
    function DoOnInvoke(Method: TRttiMethod; const Args: TArray<TValue>): TValue;
    procedure RegisterDefaultMethods();
    procedure RegisterMethod(const AMethodSignature: string; AMethodRef: TMethodReference);
  public
    constructor Create(ASession: TSession; AInterfaceTypeInfo: PTypeInfo; ARepositoryClass: TClass = nil); reintroduce;
    destructor Destroy; override;
  end;

  function FromArgsToConstArray(const Args: TArray<TValue>): TConstArray;
  function RemoveItemFromArgs(AIndex: Integer; const Args: TArray<TValue>): TArray<TValue>;
  procedure FinalizeVarRec(var Item: TVarRec);
  procedure FinalizeVarRecArray(var Arr: TConstArray);
  function GetPageArgs(const Args: TArray<TValue>; out APage: Integer; out APageSize: Integer): TArray<TValue>;

implementation

uses
  Spring.Persistence.Core.Comparers
  ,Spring.Persistence.Core.Exceptions
  ,Spring.Persistence.Core.Utils
  ,Spring.Persistence.Mapping.RttiExplorer
  ,SysUtils
  ,Math
  ,Variants
  ;

function FromArgsToConstArray(const Args: TArray<TValue>): TConstArray;
var
  i: Integer;
  LItem: TVarRec;
  LValue: TValue;
begin
  SetLength(Result, Max(0, Length(Args) - 1));
  for i := Low(Args)+1 to High(Args) do
  begin
    LValue := Args[i];
    FillChar(LItem, SizeOf(LItem), 0);
    case LValue.Kind of
      tkInteger:
      begin
        LItem.VInteger := LValue.AsInteger;
        LItem.VType := vtInteger;
      end;
      tkEnumeration:
      begin
        LItem.VBoolean := LValue.AsBoolean;
        LItem.VType := vtBoolean;
      end;
      tkString, tkUString, tkLString, tkWString:
      begin
        LItem.VUnicodeString := nil;
        string(LItem.VUnicodeString) := LValue.AsString;
        LItem.VType := vtUnicodeString;
      end;
      tkFloat:
      begin
        New(LItem.VExtended);
        LItem.VExtended^ := LValue.AsExtended;
        LItem.VType := vtExtended;
      end;
      tkRecord:
      begin
        //empty TVarRec
      end;
      tkInt64:
      begin
        New(LItem.VInt64);
        LItem.VInt64^ := LValue.AsInt64;
        LItem.VType := vtInt64;
      end
      else
      begin
        raise EORMUnsupportedType.CreateFmt('Unknown open argument type (%S)', [LValue.ToString]);
      end;
    end;
    Result[i-1] := LItem;
  end;
end;

function RemoveItemFromArgs(AIndex: Integer; const Args: TArray<TValue>): TArray<TValue>;
var
  i, ix: Integer;
begin
  ix := 0;
  SetLength(Result, Length(Args)-1);
  for i := Low(Args) to High(Args) do
  begin
    if i = AIndex then
      Continue;

    Result[ix] := Args[i];
    Inc(ix);
  end;
end;

procedure FinalizeVarRec(var Item: TVarRec);
begin
  case Item.VType of
    vtExtended: Dispose(Item.VExtended);
    vtString: Dispose(Item.VString);
    vtPWideChar: FreeMem(Item.VPWideChar);
    vtAnsiString: string(Item.VAnsiString) := '';
    vtCurrency: Dispose(Item.VCurrency);
    vtVariant: Dispose(Item.VVariant);
    vtInterface: IInterface(Item.VInterface) := nil;
    vtWideString: WideString(Item.VWideString) := '';
    vtUnicodeString: String(Item.VUnicodeString) := '';
    vtInt64: Dispose(Item.VInt64);
  end;
  Item.VInteger := 0;
end;

procedure FinalizeVarRecArray(var Arr: TConstArray);
var
  I: Integer;
begin
  for I := Low(Arr) to High(Arr) do
    FinalizeVarRec(Arr[I]);
  Arr := nil;
end;

function GetPageArgs(const Args: TArray<TValue>; out APage: Integer; out APageSize: Integer): TArray<TValue>;
var
  i: Integer;
begin
  try
    APageSize := Args[High(Args)].AsInteger;
    APage := Args[High(Args)-1].AsInteger;
  except
    raise EORMInvalidArguments.Create('Last 2 arguments for Paged requests should be Page(Integer) and PageSize(Integer).');
  end;
  SetLength(Result, Length(Args)-2);
  for i := Low(Result) to High(Result) do
  begin
    Result[i] := Args[i];
  end;
end;

{ TProxyRepository<T, TID> }

constructor TProxyRepository<T, TID>.Create(ASession: TSession;
  AInterfaceTypeInfo: PTypeInfo; ARepositoryClass: TClass);
begin
  inherited Create(AInterfaceTypeInfo);
  FSession := ASession;
  FDefaultMethods := TCollections.CreateDictionary<string, TMethodReference>(TStringCaseInsensitiveComparer.Create());
  if not Assigned(ARepositoryClass) then
    FSimpleRepository := TSimpleRepository<T,TID>.Create(ASession)
  else
    FSimpleRepository := TRttiExplorer.CreateExternalType(ARepositoryClass, [ASession]) as TSimpleRepository<T,TID>;
  FTypeName := string( PTypeInfo(TypeInfo(T)).Name );
  FIdTypeName := string ( PTypeInfo(TypeInfo(TID)).Name );
  FQualifiedTypeName := TRttiContext.Create.GetType(TypeInfo(T)).QualifiedName;
  FQualifiedIdTypeName := TRttiContext.Create.GetType(TypeInfo(TID)).QualifiedName;
  RegisterDefaultMethods();
  OnInvoke := procedure(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue)
    begin
      Result := DoOnInvoke(Method, Args);
    end;
end;

destructor TProxyRepository<T, TID>.Destroy;
begin
  inherited Destroy;
end;

function TProxyRepository<T, TID>.DoOnInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>): TValue;
var
  LMethodRef: TMethodReference;
  LMethodSignature: string;
  LItems: IList<T>;
  LConstArray: TConstArray;
  LArgs: TArray<TValue>;
  LPage, LPageSize: Integer;
begin
  LMethodSignature := TRttiExplorer.GetMethodSignature(Method);
  if FDefaultMethods.TryGetValue(LMethodSignature, LMethodRef) then
  begin
    Result := LMethodRef(Args);
  end
  else
  begin
    case Method.ReturnType.TypeKind of
      tkInteger, tkInt64: Result := FSimpleRepository.Count();
      tkClass, tkClassRef, tkPointer:
      begin
        LConstArray := FromArgsToConstArray(Args);
        try
          LItems := FSimpleRepository.Query(TRttiExplorer.GetQueryTextFromMethod(Method), LConstArray);
          (LItems as ICollectionOwnership).OwnsObjects := False;
          Result := LItems.FirstOrDefault;
        finally
          FinalizeVarRecArray(LConstArray);
        end;
      end;
      tkInterface:
      begin
        if TUtils.IsPageType(Method.ReturnType.Handle) then
        begin
          //last two arguments should be page and pagesize
          LArgs := GetPageArgs(Args, LPage, LPageSize);
          LConstArray := FromArgsToConstArray(LArgs);
          try
            Result := TValue.From( FSession.Page<T>(LPage, LPageSize, TRttiExplorer.GetQueryTextFromMethod(Method), LConstArray) );
          finally
            FinalizeVarRecArray(LConstArray);
          end;
        end
        else
        begin
          LConstArray := FromArgsToConstArray(Args);
          try
            Result := TValue.From( FSimpleRepository.Query(TRttiExplorer.GetQueryTextFromMethod(Method), LConstArray) );
          finally
            FinalizeVarRecArray(LConstArray);
          end;
        end;
      end
      else
      begin
        raise EORMUnsupportedType.CreateFmt('Unknown Method (%S) return type: %S', [Method.ToString, Method.ReturnType.ToString]);
      end;
    end;
  end;
end;

procedure TProxyRepository<T, TID>.RegisterDefaultMethods;
begin
  RegisterMethod('function Count: Int64', function(const Args: TArray<TValue>): TValue
    begin
      Result := FSimpleRepository.Count();
    end);
  RegisterMethod(Format('function Page(APage: Integer; AItemsPerPage: Integer): IDBPage<%S>', [FQualifiedTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      Result := TValue.From( FSimpleRepository.Page(Args[1].AsInteger, Args[2].AsInteger) );
    end);
  RegisterMethod(Format( 'function FindOne(const AID: %S): %S', [FIdTypeName, FTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      Result := FSimpleRepository.FindOne(Args[1].AsType<TID>);
    end);
  RegisterMethod(Format( 'function FindAll: IList<%S>', [FQualifiedTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      Result := TValue.From( FSimpleRepository.FindAll());
    end);
  RegisterMethod(Format( 'function Exists(const AId: %S): Boolean', [FIdTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      Result := FSimpleRepository.Exists(Args[1].AsType<TID>);
    end);
  RegisterMethod(Format( 'procedure Insert(AEntity: %S)', [FTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      FSimpleRepository.Insert(Args[1].AsType<T>);
    end);
  RegisterMethod(Format( 'procedure Insert(AEntities: ICollection<%S>)', [FQualifiedTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      FSimpleRepository.Insert(Args[1].AsInterface as ICollection<T>);
    end);
  RegisterMethod(Format( 'function Save(AEntity: %0:S): %0:S', [FTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      Result := FSimpleRepository.Save(Args[1].AsType<T>);
    end);
  RegisterMethod(Format( 'function Save(AEntities: ICollection<%0:S>): ICollection<%0:S>', [FQualifiedTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      Result := TValue.From( FSimpleRepository.Save(Args[1].AsInterface as ICollection<T>) );
    end);
  RegisterMethod(Format( 'procedure SaveCascade(AEntity: %S)', [FTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      FSimpleRepository.SaveCascade(Args[1].AsType<T>);
    end);
  RegisterMethod(Format( 'procedure Delete(AEntity: %S)', [FTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      FSimpleRepository.Delete(Args[1].AsType<T>) ;
    end);
  RegisterMethod(Format( 'procedure Delete(AEntities: ICollection<%S>)', [FQualifiedTypeName])
  , function(const Args: TArray<TValue>): TValue
    begin
      FSimpleRepository.Delete(Args[1].AsInterface as ICollection<T>) ;
    end);
  RegisterMethod('procedure DeleteAll'
  , function(const Args: TArray<TValue>): TValue
    begin
      FSimpleRepository.DeleteAll();
    end);
  RegisterMethod(Format('function Query(const AQuery: string; const AParams: TVarRec): IList<%S>', [FQualifiedTypeName])
  , function(const Args: TArray<TValue>): TValue
    var
      LConstArray: TConstArray;
    begin
      LConstArray := FromArgsToConstArray(RemoveItemFromArgs(1, Args));
      try
        Result := TValue.From( FSimpleRepository.Query(Args[1].AsString, LConstArray));
      finally
        FinalizeVarRecArray(LConstArray);
      end;
    end);
  RegisterMethod('function Execute(const AQuery: string; const AParams: TVarRec): NativeUInt'
  , function(const Args: TArray<TValue>): TValue
    var
      LConstArray: TConstArray;
    begin
      LConstArray := FromArgsToConstArray(RemoveItemFromArgs(1, Args));
      try
        Result := FSimpleRepository.Execute(Args[1].AsString, LConstArray);
      finally
        FinalizeVarRecArray(LConstArray);
      end;
    end);
end;

procedure TProxyRepository<T, TID>.RegisterMethod(
  const AMethodSignature: string; AMethodRef: TMethodReference);
begin
  FDefaultMethods.AddOrSetValue(AMethodSignature, AMethodRef);
end;

end.
