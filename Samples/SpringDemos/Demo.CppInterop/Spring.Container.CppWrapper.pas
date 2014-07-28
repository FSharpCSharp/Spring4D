unit Spring.Container.CppWrapper;

interface

{$I Spring.inc}

//Will be exposed to C++
uses System.SysUtils, System.TypInfo, System.Rtti;

{$SCOPEDENUMS ON}
type
  ///	<summary>
  ///	  Defines if type is using reference counting
  ///	</summary>
  TRefCounting = (
    ///	<summary>
    ///	  Container decides (Yes for TInterfacedObject descendants, No for others)
    ///	</summary>
    Unknown,

    ///	<summary>
    ///	  Type is using reference counting
    ///	</summary>
    True,

    ///	<summary>
    ///	  Type is not using reference counting
    ///	</summary>
    False
  );
  ///	<summary>
  ///	  Internal helper class for non-generic fluent style registration of a
  ///	  component.
  ///	</summary>
  TRegistration = record
  private
    fDelegate: IInterface;
    constructor Create(const delegate: IInterface);
  public
    function Implements(serviceType: PTypeInfo): TRegistration; overload;
    function Implements(serviceType: PTypeInfo; const name: string): TRegistration; overload;

    //function DelegateTo(const delegate: TActivatorDelegate): TRegistration; overload;

    {$REGION 'Typed Injections'}

    function InjectConstructor(const parameterTypes: array of PTypeInfo): TRegistration; overload;
    function InjectProperty(const propertyName: string): TRegistration; overload;
    function InjectMethod(const methodName: string): TRegistration; overload;
    function InjectMethod(const methodName: string; const parameterTypes: array of PTypeInfo): TRegistration; overload;
    function InjectField(const fieldName: string): TRegistration; overload;

    {$ENDREGION}

    {$REGION 'Named/Valued Injections'}

    function InjectConstructor(const arguments: array of TValue): TRegistration; overload;
    function InjectProperty(const propertyName: string; const value: TValue): TRegistration; overload;
    function InjectMethod(const methodName: string; const arguments: array of TValue): TRegistration; overload;
    function InjectField(const fieldName: string; const value: TValue): TRegistration; overload;

    {$ENDREGION}

    function AsSingleton(refCounting: TRefCounting = TRefCounting.Unknown): TRegistration;
    function AsSingletonPerThread(refCounting: TRefCounting = TRefCounting.Unknown): TRegistration;
    function AsTransient: TRegistration;
    function AsPooled(minPoolSize, maxPoolSize: Integer): TRegistration; {$IFDEF CPUARM}experimental;{$ENDIF}

    function AsDefault: TRegistration; overload;
    function AsDefault(serviceType: PTypeInfo): TRegistration; overload;

{$IFDEF DELPHIXE_UP}
    function AsFactory: TRegistration; overload;
    function AsFactory(const name: string): TRegistration; overload;
{$ENDIF}
  end;

  TContainer = class
  private
      FContainer: TObject;
  public
    constructor Create(const AContainer: TObject);

    function RegisterType(TypeInfo: PTypeInfo) : TRegistration; overload;

    procedure Build;

    //Not usable from C++
    function RegisterType<T : class>: TRegistration; overload;
    function Resolve<T>: T; overload;
    function Resolve<T>(const name: string): T; overload;
  end;


implementation

//Will not be exposed to C++
uses
  //SharedIntfs, InternalImpl,
  Spring.Container,
  Spring.Container.Common,
  Spring.Container.Registration;

type
  PRegistration = ^Spring.Container.Registration.TRegistration;

  IRegistration = interface
    ['{F951478B-1560-4488-AE4E-B93F91FEEC59}']
    function GetRegistration: PRegistration;
    property Registration: PRegistration read GetRegistration;
  end;

  TRegistrationImpl = class(TInterfacedObject, IRegistration)
  private
    FRegistration: Spring.Container.Registration.TRegistration;
  public
    constructor Create(const Reg: Spring.Container.Registration.TRegistration);
{$IFDEF DEBUG}
    destructor Destroy; override;
{$ENDIF}

    function GetRegistration: PRegistration;
  end;

  TRegistrationHelper = record helper for TRegistration
  public
    function Registration: PRegistration; inline;
  end;

  TContainerHelper = class helper for TContainer
  public
      function Container: Spring.Container.TContainer; inline;
  end;

{ TRegistration }

function TRegistration.AsDefault: TRegistration;
begin
  Registration.AsDefault;
  Result := Self;
end;

function TRegistration.AsDefault(serviceType: PTypeInfo): TRegistration;
begin
  Registration.AsDefault(serviceType);
  Result := Self;
end;

function TRegistration.AsPooled(minPoolSize,
  maxPoolSize: Integer): TRegistration;
begin
  Registration.AsPooled(minPoolSize, maxPoolSize);
  Result := Self;
end;

function TRegistration.AsSingleton(refCounting: TRefCounting): TRegistration;
begin
  Registration.AsSingleton(Spring.Container.Common.TRefCounting(refCounting));
  Result := Self;
end;

function TRegistration.AsSingletonPerThread(
  refCounting: TRefCounting): TRegistration;
begin
  Registration.AsSingletonPerThread(Spring.Container.Common.TRefCounting(refCounting));
  Result := Self;
end;

function TRegistration.AsTransient: TRegistration;
begin
  Registration.AsTransient;
  Result := Self;
end;

constructor TRegistration.Create(const delegate: IInterface);
begin
  //We want to optimize GetRegistration so we'll assign the IRegistration
  //to our interface holder and just cast it statically
  if (delegate.QueryInterface(IRegistration, fDelegate) <> 0) then
    Assert(false);
end;

function TRegistration.Implements(serviceType: PTypeInfo;
  const name: string): TRegistration;
begin
  Registration.Implements(serviceType, name);
  Result := Self;
end;

function TRegistration.Implements(serviceType: PTypeInfo): TRegistration;
begin
  Registration.Implements(serviceType);
  Result := Self;
end;

function TRegistration.InjectConstructor(
  const parameterTypes: array of PTypeInfo): TRegistration;
begin
  Registration.InjectConstructor(parameterTypes);
  Result := Self;
end;

function TRegistration.InjectConstructor(
  const arguments: array of TValue): TRegistration;
begin
  Registration.InjectConstructor(arguments);
  Result := Self;
end;

function TRegistration.InjectField(const fieldName: string;
  const value: TValue): TRegistration;
begin
  Registration.InjectField(fieldName, value);
  Result := Self;
end;

function TRegistration.InjectField(const fieldName: string): TRegistration;
begin
  Registration.InjectField(fieldName);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string;
  const arguments: array of TValue): TRegistration;
begin
  Registration.InjectMethod(methodName, arguments);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string): TRegistration;
begin
  Registration.InjectMethod(methodName);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string;
  const parameterTypes: array of PTypeInfo): TRegistration;
begin
  Registration.InjectMethod(methodName, parameterTypes);
  Result := Self;
end;

function TRegistration.InjectProperty(const propertyName: string;
  const value: TValue): TRegistration;
begin
  Registration.InjectProperty(propertyName, value);
  Result := Self;
end;

function TRegistration.InjectProperty(
  const propertyName: string): TRegistration;
begin
  Registration.InjectProperty(propertyName);
  Result := Self;
end;

{$IFDEF DELPHIXE_UP}
function TRegistration.AsFactory: TRegistration;
begin
  Registration.AsFactory;
  Result := Self;
end;

function TRegistration.AsFactory(const name: string): TRegistration;
begin
  Registration.AsFactory(name);
  Result := Self;
end;
{$ENDIF}

{ TRegistrationImpl }

constructor TRegistrationImpl.Create(
  const Reg: Spring.Container.Registration.TRegistration);
begin
  inherited Create;
  FRegistration := Reg;
end;

{$IFDEF DEBUG}
destructor TRegistrationImpl.Destroy;
begin
  inherited;
end;
{$ENDIF}

function TRegistrationImpl.GetRegistration: PRegistration;
begin
  Result := @FRegistration;
end;

{ TRegistrationHelper }

function TRegistrationHelper.Registration: PRegistration;
begin
  //Result := (Self.fDelegate as IRegistration).Registration;
  //We're assigning the interface directly so this should be relatively safe
  Result := IRegistration(Self.fDelegate).Registration;
end;

{ TContainerHelper }

function TContainerHelper.Container: Spring.Container.TContainer;
begin
  TObject(Result) := FContainer;
end;

{ TContainer }

procedure TContainer.Build;
begin
  Container.Build;
end;

constructor TContainer.Create(const AContainer: TObject);
begin
  Assert(AContainer is Spring.Container.TContainer);
  inherited Create;
  FContainer := AContainer;
end;

function TContainer.RegisterType(TypeInfo: PTypeInfo): TRegistration;
var I  : IRegistration;
begin
  I := TRegistrationImpl.Create(
    Container.RegisterType(TypeInfo));
  Result := TRegistration.Create(I);
end;

function TContainer.RegisterType<T>: TRegistration;
begin
  Result := RegisterType(TypeInfo(T));
end;

function TContainer.Resolve<T>: T;
begin
  Result := Container.Resolve<T>;
end;

function TContainer.Resolve<T>(const name: string): T;
begin
  Result := Container.Resolve<T>(name);
end;

end.
