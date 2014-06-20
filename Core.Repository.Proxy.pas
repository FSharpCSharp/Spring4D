unit Core.Repository.Proxy;

{$I sv.inc}

interface

{$IF CompilerVersion < 22}
{$Message Fatal 'Proxy repository supports XE2 or higher'}
{$IFEND}

uses
  Rtti, TypInfo, Core.Interfaces, Core.Session, Core.Repository.Simple
  ,Spring.Collections
  ;

type
  TMethodReference = reference to function(const Args: TArray<TValue>): TValue;

  TProxyRepository<T: class, constructor; TID> = class(TVirtualInterface)
  private
    FSimpleRepository: IPagedRepository<T,TID>;
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

implementation

uses
  Core.Comparers
  ,Core.Exceptions
  ,Mapping.RttiExplorer
  ,SysUtils
  ;



{ TProxyRepository<T, TID> }

constructor TProxyRepository<T, TID>.Create(ASession: TSession;
  AInterfaceTypeInfo: PTypeInfo; ARepositoryClass: TClass);
begin
  inherited Create(AInterfaceTypeInfo);
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
        {TODO -oOwner -cGeneral : set args}
        LItems := FSimpleRepository.Query(TRttiExplorer.GetQueryTextFromMethod(Method), []);
        (LItems as ICollectionOwnership).OwnsObjects := False;
        Result := LItems.FirstOrDefault;
      end;
      tkInterface: Result := TValue.From( FSimpleRepository.Query(TRttiExplorer.GetQueryTextFromMethod(Method), []) )
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
    begin
      Result := TValue.From( FSimpleRepository.Query(Args[1].AsString, [])); {TODO -oOwner -cGeneral : set array of const from args}
    end);
  RegisterMethod('function Execute(const AQuery: string; const AParams: TVarRec): NativeUInt'
  , function(const Args: TArray<TValue>): TValue
    begin
      Result := FSimpleRepository.Execute(Args[1].AsString, []); {TODO -oOwner -cGeneral : set array of const from args}
    end);
end;

procedure TProxyRepository<T, TID>.RegisterMethod(
  const AMethodSignature: string; AMethodRef: TMethodReference);
begin
  FDefaultMethods.AddOrSetValue(AMethodSignature, AMethodRef);
end;

end.
