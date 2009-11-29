{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

unit Spring.Tests.IoC;

{$I Spring.inc}

interface

uses
  SysUtils,
  TestFramework,
  Spring.System,
  Spring.IoC,
  Spring.Tests.IoC.Components;

type
  TContainerTestCase = class abstract(TTestCase)
  protected
    fContainer: TContainer;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestEmptyContainer = class(TContainerTestCase)
  published
    procedure TestResolveUnknownIntferfaceService;
//    procedure TestResolveUnknownClassService;
    procedure TestRegisterNonGuidInterfaceService;
    procedure TestRegisterGenericInterfaceService;
    procedure TestRegisterUnassignableService;
    procedure TestResolveAll;
    procedure TestResolveAllNonGeneric;
  end;

  TTestSimpleContainer = class(TContainerTestCase)
  published
    procedure TestInterfaceService;
    procedure TestAbstractClassService;
    procedure TestServiceSameAsComponent;
    procedure TestBootstrap;
    procedure TestSingleton;
    procedure TestTransient;
  end;

  // Same Service, Different Implementations
  TTestDifferentServiceImplementations = class(TContainerTestCase)
  private
    fNameService: INameService;
    fAnotherNameService: INameService;
    fServices: TArray<INameService>;
    fServiceValues: TArray<TValue>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNameService;
    procedure TestAnotherNameService;
    procedure TestResolveAll;
    procedure TestResolveAllNonGeneric;
    procedure TestUnsatisfiedDependency;
  end;

  // Same Component, Different Services
  TTestImplementsDifferentServices = class(TContainerTestCase)
  private
    fNameService: INameService;
    fAgeService: IAgeService;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNameService;
    procedure TestAgeService;
  end;

  TTestActivatorDelegate = class(TContainerTestCase)
  private
    fPrimitive: IPrimitive;
    fExpectedInteger: Integer;
    fExpectedString: string;
  protected
    procedure SetUp; override;
  published
    procedure TestNameService;
    procedure TestIntegerArgument;
    procedure TestStringArgument;
  end;

  TTypedInjectionTestCase = class abstract(TContainerTestCase)
  private
    fNameService: INameService;
    fInjectionExplorer: IInjectionExplorer;
  protected
    procedure DoRegisterComponents; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConstructorInjection;
    procedure TestMethodInjection;
    procedure TestPropertyInjection;
    procedure TestFieldInjection;
  end;

  TTestTypedInjectionByCoding = class(TTypedInjectionTestCase)
  protected
    procedure DoRegisterComponents; override;
  end;

  TTestTypedInjectionsByAttribute = class(TTypedInjectionTestCase)
  protected
    procedure DoRegisterComponents; override;
  end;

  { TODO: Support Primitive values }
  TTestNamedInjections = class(TContainerTestCase)
  private
    fExplorer: IInjectionExplorer;
  protected
    procedure SetUp; override;
  published
    procedure TestConstructorInjection;
    procedure TestMethodInjection;
    procedure TestPropertyInjection;
    procedure TestFieldInjection;
  end;

  TTestDirectCircularDependency = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestResolve;
  end;

  TTestCrossedCircularDependency = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestResolveChicken;
    procedure TestResolveEgg;
  end;

implementation


{$REGION 'TContainerTestCase'}

procedure TContainerTestCase.SetUp;
begin
  inherited;
  fContainer := TContainer.Create;
end;

procedure TContainerTestCase.TearDown;
begin
  fContainer.Free;
  inherited;
end;

{$ENDREGION}


{$REGION 'TTestEmptyContainer'}

procedure TTestEmptyContainer.TestResolveUnknownIntferfaceService;
begin
  ExpectedException := EResolveException;
  fContainer.Resolve<INameService>;
end;

//procedure TTestEmptyContainer.TestResolveUnknownClassService;
//begin
//  ExpectedException := EResolveException;
//  fContainer.Resolve<TFoo2>;
//end;

procedure TTestEmptyContainer.TestRegisterNonGuidInterfaceService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterComponent<TNonGuid>.Implements<INonGuid>;
end;

procedure TTestEmptyContainer.TestRegisterGenericInterfaceService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterComponent<TNonGuid<TObject>>.Implements<INonGuid<TObject>>;
end;

procedure TTestEmptyContainer.TestRegisterUnassignableService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterComponent<TContainer>.Implements<IDispatch>;
end;

procedure TTestEmptyContainer.TestResolveAll;
var
  services: TArray<INameService>;
begin
  services := fContainer.ResolveAll<INameService>;
  CheckEquals(0, Length(services));
end;

procedure TTestEmptyContainer.TestResolveAllNonGeneric;
var
  services: TArray<TValue>;
begin
  services := fContainer.ResolveAll(TypeInfo(INameService));
  CheckEquals(0, Length(services));
end;

{$ENDREGION}


{$REGION 'TTestSimpleContainer'}

procedure TTestSimpleContainer.TestInterfaceService;
var
  service: INameService;
begin
  fContainer.RegisterComponent<TNameService>.Implements<INameService>;
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  try
    CheckNotNull(service, 'service should not be nil.');
    CheckTrue(service is TNameService, 'service should be a TNameService instance.');
    CheckEquals(TNameService.NameString, service.Name);
  finally
    fContainer.Release(service);
  end;
end;

procedure TTestSimpleContainer.TestAbstractClassService;
var
  service: TAgeServiceBase;
begin
  fContainer.RegisterComponent<TAgeServiceImpl>.Implements<TAgeServiceBase>;
  fContainer.Build;
  service := fContainer.Resolve<TAgeServiceBase>;
  try
    CheckIs(service, TAgeServiceImpl, 'service should be a TNameService instance.');
    CheckEquals(TAgeServiceImpl.DefaultAge, service.Age);
  finally
    fContainer.Release(service);
  end;
end;

procedure TTestSimpleContainer.TestServiceSameAsComponent;
var
  service: TNameService;
begin
  fContainer.RegisterComponent<TNameService>.Implements<TNameService>;
  fContainer.Build;
  service := fContainer.Resolve<TNameService>;
  try
    CheckNotNull(service, 'service should not be null.');
    CheckEquals(TNameService.NameString, service.Name);
  finally
    fContainer.Release(service);
  end;
end;

procedure TTestSimpleContainer.TestBootstrap;
var
  component: TBootstrapComponent;
begin
  fContainer.RegisterComponent<TNameService>.Implements<INameService>.AsSingleton;
  fContainer.RegisterComponent<TAgeServiceImpl>.Implements<TAgeServiceBase>.AsSingleton;
  fContainer.Build;
  component := fContainer.Resolve<TBootstrapComponent>;
  try
    CheckNotNull(component, 'component should not be nil.');
    CheckNotNull(component.NameService, 'NameService');
    CheckEquals(TNameService.NameString, component.NameService.Name);
    CheckNotNull(component.AgeService, 'AgeService');
    CheckEquals(TAgeServiceImpl.DefaultAge, component.AgeService.Age);
  finally
    fContainer.Release(component);
  end;
end;

procedure TTestSimpleContainer.TestSingleton;
var
  obj1, obj2: TAgeServiceBase;
begin
  fContainer.RegisterComponent<TAgeServiceImpl>
    .Implements<TAgeServiceBase>
    .AsSingleton;
  fContainer.Build;
  obj1 := fContainer.Resolve<TAgeServiceBase>;
  obj2 := fContainer.Resolve<TAgeServiceBase>;
  try
    CheckNotNull(obj1, 'obj1 should not be nil');
    CheckNotNull(obj2, 'obj2 should not be nil');
    CheckSame(obj1, obj2, 'obj1 should be the same as obj2.');
  finally
    fContainer.Release(obj1);
    fContainer.Release(obj2);
  end;
end;

procedure TTestSimpleContainer.TestTransient;
var
  obj1, obj2: TAgeServiceBase;
begin
  fContainer.RegisterComponent<TAgeServiceImpl>
    .Implements<TAgeServiceBase>;
  fContainer.Build;
  obj1 := fContainer.Resolve<TAgeServiceBase>;
  obj2 := fContainer.Resolve<TAgeServiceBase>;
  try
    CheckNotNull(obj1, 'obj1 should not be nil');
    CheckNotNull(obj2, 'obj2 should not be nil');
    CheckTrue(obj1 <> obj2, 'obj1 should not be the same as obj2.');
  finally
    fContainer.Release(obj1);
    fContainer.Release(obj2);
  end;
end;

{$ENDREGION}


{$REGION 'TTestDifferentImplementations'}

procedure TTestDifferentServiceImplementations.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterComponent<TNameService>.Implements<INameService>('default').AsSingleton;
  fContainer.RegisterComponent<TAnotherNameService>.Implements<INameService>('another');
  fContainer.Build;
  fNameService := fContainer.Resolve<INameService>('default');
  fAnotherNameService := fContainer.Resolve<INameService>('another');
  fServices := fContainer.ResolveAll<INameService>;
  fServiceValues := fContainer.ResolveAll(TypeInfo(INameService));
end;

procedure TTestDifferentServiceImplementations.TearDown;
begin
  fContainer.Release(fAnotherNameService);
  fContainer.Release(fNameService);
  inherited TearDown;
end;

procedure TTestDifferentServiceImplementations.TestNameService;
begin
  CheckNotNull(fNameService, 'fNameService should not be nil.');
  CheckTrue(fNameService is TNameService, 'fNameService should be an instance of TNameService.');
  CheckEquals(TNameService.NameString, fNameService.Name);
end;

procedure TTestDifferentServiceImplementations.TestAnotherNameService;
begin
  CheckNotNull(fAnotherNameService, 'fAnotherNameService should not be nil.');
  CheckTrue(fAnotherNameService is TAnotherNameService, 'fAnotherNameService should be an instance of TAnotherNameService.');
  CheckEquals(TAnotherNameService.NameString, fAnotherNameService.Name);
end;

procedure TTestDifferentServiceImplementations.TestResolveAll;
begin
  CheckEquals(2, Length(fServices), 'Count of fServices should be 2.');
  CheckTrue(fServices[0] is TNameService);
  CheckTrue(fServices[1] is TAnotherNameService);
end;

procedure TTestDifferentServiceImplementations.TestResolveAllNonGeneric;
begin
  CheckEquals(2, Length(fServiceValues), 'Count of fServiceValues should be 2.');
  CheckTrue((fServiceValues[0].AsType<INameService>) is TNameService);
  CheckTrue((fServiceValues[1].AsType<INameService>) is TAnotherNameService);
end;

/// <remarks>
/// EUnsatisfiedDependencyException will be raised when resolving a service type
//  with an ambiguous name.
/// </remarks>
procedure TTestDifferentServiceImplementations.TestUnsatisfiedDependency;
begin
  ExpectedException := EUnsatisfiedDependencyException;
  fContainer.Resolve<INameService>;
end;

{$ENDREGION}


{$REGION 'TTestActivatorDelegate'}

procedure TTestActivatorDelegate.SetUp;
begin
  inherited SetUp;
  fExpectedInteger := 26;
  fExpectedString := 'String';
  fContainer.RegisterComponent<TNameService>
    .Implements<INameService>
    .AsSingleton;
  fContainer.RegisterComponent<TPrimitiveComponent>
    .Implements<IPrimitive>
    .DelegateTo(
      function: TPrimitiveComponent
      begin
        Result := TPrimitiveComponent.Create(
          fContainer.Resolve<INameService>,
          fExpectedInteger,
          fExpectedString
        );
      end
    );
  fContainer.Build;
  fPrimitive := fContainer.Resolve<IPrimitive>;
  Assert(fPrimitive <> nil, 'fPrimitive should not be nil.');
  Assert(fPrimitive.NameService <> nil, 'fPrimitive.NameService should not be nil.');
end;

procedure TTestActivatorDelegate.TestNameService;
begin
  CheckNotNull(fPrimitive.NameService, 'NameService should not be nil.');
  CheckTrue(fPrimitive.NameService is TNameService, 'Unexpected type.');
  CheckEquals(TNameService.NameString, fPrimitive.NameService.Name);
end;

procedure TTestActivatorDelegate.TestIntegerArgument;
begin
  CheckEquals(fExpectedInteger, fPrimitive.IntegerArg);
end;

procedure TTestActivatorDelegate.TestStringArgument;
begin
  CheckEquals(fExpectedString, fPrimitive.StringArg);
end;

{$ENDREGION}


{$REGION 'TTestTypedInjections'}

procedure TTypedInjectionTestCase.SetUp;
begin
  inherited SetUp;
  DoRegisterComponents;
  fContainer.Build;
  fNameService := fContainer.Resolve<INameService>;
  Assert(fNameService is TNameService, 'fNameService should be TNameService.');
  Assert(fNameService.Name = TNameService.NameString, 'fNameService.Name is wrong.');
  fInjectionExplorer := fContainer.Resolve<IInjectionExplorer>;
end;

procedure TTypedInjectionTestCase.TearDown;
begin
  fContainer.Release(fInjectionExplorer);
  fContainer.Release(fNameService);
  inherited TearDown;
end;

procedure TTypedInjectionTestCase.TestConstructorInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.ConstructorInjection);
end;

procedure TTypedInjectionTestCase.TestPropertyInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.PropertyInjection);
end;

procedure TTypedInjectionTestCase.TestMethodInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.MethodInjection);
end;

procedure TTypedInjectionTestCase.TestFieldInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.FieldInjection);
end;

{$ENDREGION}


{$REGION 'TTestTypedInjectionByCoding'}

procedure TTestTypedInjectionByCoding.DoRegisterComponents;
begin
  fContainer.RegisterComponent<TNameService>
    .Implements<INameService>
    .AsSingleton;
  fContainer.RegisterComponent<TInjectionExplorer>
    .Implements<IInjectionExplorer>
    .InjectConstructor([TypeInfo(INameService)])
    .InjectProperty('PropertyInjection')
    .InjectMethod('SetMethodInjection')
    .InjectField('fFieldInjection');
end;

{$ENDREGION}


{$REGION 'TTestTypedInjectionsByAttribute'}

procedure TTestTypedInjectionsByAttribute.DoRegisterComponents;
begin
  fContainer.RegisterComponent<TNameService>
    .Implements<INameService>
    .AsSingleton;
  fContainer.RegisterComponent<TInjectionExplorerComponent>
    .Implements<IInjectionExplorer>;
end;

{$ENDREGION}


{$REGION 'TTestDirectCircularDependency'}

procedure TTestDirectCircularDependency.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterComponent<TCircularDependencyChicken>.Implements<IChicken>;
  fContainer.Build;
end;

procedure TTestDirectCircularDependency.TestResolve;
var
  chicken: IChicken;
begin
  ExpectedException := ECircularDependencyException;
  chicken := fContainer.Resolve<IChicken>;
end;

{$ENDREGION}


{$REGION 'TTestCrossedCircularDependency'}

procedure TTestCrossedCircularDependency.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterComponent<TCircularDependencyChicken>.Implements<IChicken>;
  fContainer.RegisterComponent<TEgg>.Implements<IEgg>;
  fContainer.Build;
end;

procedure TTestCrossedCircularDependency.TestResolveChicken;
var
  chicken: IChicken;
begin
  ExpectedException := ECircularDependencyException;
  chicken := fContainer.Resolve<IChicken>;
end;

procedure TTestCrossedCircularDependency.TestResolveEgg;
var
  egg: IEgg;
begin
  ExpectedException := ECircularDependencyException;
  egg := fContainer.Resolve<IEgg>;
end;

{$ENDREGION}


{$REGION 'TTestNamedInjections'}

procedure TTestNamedInjections.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterComponent<TNameService>
    .Implements<INameService>('default');
  fContainer.RegisterComponent<TAnotherNameService>
    .Implements<INameService>('another');
  fContainer.RegisterComponent<TInjectionExplorer>
    .Implements<IInjectionExplorer>
    .InjectConstructor(['default'])
    .InjectProperty('PropertyInjection', 'another')
    .InjectMethod('SetMethodInjection', ['another'])
    .InjectField('fFieldInjection', 'default')
    .AsSingleton;
  fContainer.Build;
  fExplorer := fContainer.Resolve<IInjectionExplorer>;
end;

procedure TTestNamedInjections.TestConstructorInjection;
begin
  CheckNotNull(fExplorer.ConstructorInjection);
  CheckEquals(TNameService.NameString, fExplorer.ConstructorInjection.Name);
end;

procedure TTestNamedInjections.TestPropertyInjection;
begin
  CheckNotNull(fExplorer.PropertyInjection);
  CheckEquals(TAnotherNameService.NameString, fExplorer.PropertyInjection.Name);
end;

procedure TTestNamedInjections.TestMethodInjection;
begin
  CheckNotNull(fExplorer.MethodInjection);
  CheckEquals(TAnotherNameService.NameString, fExplorer.MethodInjection.Name);
end;

procedure TTestNamedInjections.TestFieldInjection;
begin
  CheckNotNull(fExplorer.FieldInjection);
  CheckEquals(TNameService.NameString, fExplorer.FieldInjection.Name);
end;

{$ENDREGION}

{ TTestImplementsDifferentServices }

procedure TTestImplementsDifferentServices.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterComponent<TNameService>
    .Implements<INameService>('another');
  fContainer.RegisterComponent<TNameAgeComponent>
    .Implements<INameService>('default')
    .Implements<IAgeService>
    .AsSingleton;
  fContainer.Build;
  fNameService := fContainer.Resolve<INameService>('default');
  fAgeService := fContainer.Resolve<IAgeService>;
  Assert(fNameService <> nil, 'fNameService should not be nil.');
  Assert(fAgeService <> nil, 'fAgeService should not be nil.');
end;

procedure TTestImplementsDifferentServices.TearDown;
begin
  fContainer.Release(fAgeService);
  fContainer.Release(fNameService);
  inherited TearDown;
end;

procedure TTestImplementsDifferentServices.TestNameService;
begin
  Check(fNameService is TNameAgeComponent);
  CheckEquals(TNameAgeComponent.NameString, fNameService.Name);
end;

procedure TTestImplementsDifferentServices.TestAgeService;
begin
  Check(fAgeService is TNameAgeComponent);
  CheckEquals(TNameAgeComponent.DefaultAge, fAgeService.Age);
end;

end.
