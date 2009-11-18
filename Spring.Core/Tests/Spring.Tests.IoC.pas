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
  Spring.IoC.Core,
  Spring.IoC.LifetimeManager,
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
    // TODO: TestResolveUnknownClassService
//    procedure TestResolveUnknownClassService;
    procedure TestRegisterNonGuidInterfaceService;
    procedure TestRegisterGenericInterfaceService;
    procedure TestResolveAll;
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

  TTestDifferentServiceImplementations = class(TContainerTestCase)
  private
    fDefault: INameService;
    fNameService: INameService;
    fAnotherNameService: INameService;
    fServices: TArray<INameService>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefault;
    procedure TestNameService;
    procedure TestAnotherNameService;
    procedure TestResolveAll;
  end;

  TTestPrimitiveArguments = class(TContainerTestCase)
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

  TTestMemberInjections = class(TContainerTestCase)
  private
    fNameService: INameService;
    fInjectionExplorer: IInjectionExplorer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConstructorInjection;
    procedure TestMethodInjection;
    procedure TestPropertyInjection;
    procedure TestFieldInjection;
//    procedure TestInjectConstructor;
//    procedure TestInjectMethod;
//    procedure TestInjectProperty;
//    procedure TestInjectField;
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
  fContainer.RegisterType<INonGuid, TNonGuid>;
end;

procedure TTestEmptyContainer.TestRegisterGenericInterfaceService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterType<INonGuid<TObject>, TNonGuid<TObject>>;
end;

procedure TTestEmptyContainer.TestResolveAll;
var
  services: TArray<INameService>;
begin
  services := fContainer.ResolveAll<INameService>;
  CheckEquals(0, Length(services));
end;

{$ENDREGION}


{$REGION 'TTestSimpleContainer'}

procedure TTestSimpleContainer.TestInterfaceService;
var
  service: INameService;
begin
  fContainer.RegisterType<INameService, TNameService>;
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
  fContainer.RegisterType<TAgeServiceBase, TAgeServiceImpl>;
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
  fContainer.RegisterType<TNameService, TNameService>;
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
  fContainer.RegisterType<INameService, TNameService>(ltSingleton);
  fContainer.RegisterType<TAgeServiceBase, TAgeServiceImpl>(ltSingleton);
  component := fContainer.Resolve<TBootstrapComponent>;
  try
    CheckNotNull(component);
    CheckEquals(TNameService.NameString, component.NameService.Name);
    CheckEquals(TAgeServiceImpl.DefaultAge, component.AgeService.Age);
  finally
    fContainer.Release(component);
  end;
end;

procedure TTestSimpleContainer.TestSingleton;
var
  obj1, obj2: TAgeServiceBase;
begin
  fContainer.RegisterType<TAgeServiceBase, TAgeServiceImpl>(ltSingleton);
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
  fContainer.RegisterType<TAgeServiceBase, TAgeServiceImpl>(ltTransient);
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
  fContainer.RegisterType<INameService, TNameService>('default', ltSingleton);
  fContainer.RegisterType<INameService, TAnotherNameService>('another');
  fDefault := fContainer.Resolve<INameService>;
  fNameService := fContainer.Resolve<INameService>('default');
  fAnotherNameService := fContainer.Resolve<INameService>('another');
  fServices := fContainer.ResolveAll<INameService>;
end;

procedure TTestDifferentServiceImplementations.TearDown;
begin
  fContainer.Release(fAnotherNameService);
  fContainer.Release(fNameService);
  inherited TearDown;
end;

procedure TTestDifferentServiceImplementations.TestDefault;
begin
  CheckTrue(fDefault is TNameService);
  CheckEquals(TNameService.NameString, fDefault.Name);
end;

procedure TTestDifferentServiceImplementations.TestNameService;
begin
  CheckSame(fDefault, fNameService);
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

{$ENDREGION}


{$REGION 'TTestPrimitiveArguments'}

procedure TTestPrimitiveArguments.SetUp;
begin
  inherited SetUp;
  fExpectedInteger := 26;
  fExpectedString := 'String';
  fContainer.RegisterType<INameService, TNameService>;
  fContainer.RegisterType<IPrimitive, TPrimitiveComponent>(
    function: TPrimitiveComponent
    begin
      Result := TPrimitiveComponent.Create(
        fContainer.Resolve<INameService>,
        fExpectedInteger,
        fExpectedString
      );
    end
  );
  fPrimitive := fContainer.Resolve<IPrimitive>;
  Assert(fPrimitive <> nil, 'fPrimitive should not be nil.');
  Assert(fPrimitive.NameService <> nil, 'fPrimitive.NameService should not be nil.');
end;

procedure TTestPrimitiveArguments.TestNameService;
begin
  CheckNotNull(fPrimitive.NameService, 'NameService should not be nil.');
  CheckTrue(fPrimitive.NameService is TNameService, 'Unexpected type.');
  CheckEquals(TNameService.NameString, fPrimitive.NameService.Name);
end;

procedure TTestPrimitiveArguments.TestIntegerArgument;
begin
  CheckEquals(fExpectedInteger, fPrimitive.IntegerArg);
end;

procedure TTestPrimitiveArguments.TestStringArgument;
begin
  CheckEquals(fExpectedString, fPrimitive.StringArg);
end;

{$ENDREGION}


{$REGION 'TTestMemberInjections'}

procedure TTestMemberInjections.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<INameService, TNameService>(ltSingleton);
  fContainer.RegisterType<IInjectionExplorer, TInjectionServiceImpl>(ltSingleton);
  fNameService := fContainer.Resolve<INameService>;
  Assert(fNameService is TNameService, 'fNameService should be TNameService.');
  Assert(fNameService.Name = TNameService.NameString, 'fNameService.Name is wrong.');
  fInjectionExplorer := fContainer.Resolve<IInjectionExplorer>;
end;

procedure TTestMemberInjections.TearDown;
begin
  fContainer.Release(fInjectionExplorer);
  fContainer.Release(fNameService);
  inherited TearDown;
end;

procedure TTestMemberInjections.TestConstructorInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.ConstructorInjection);
end;

procedure TTestMemberInjections.TestPropertyInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.PropertyInjection);
end;

procedure TTestMemberInjections.TestMethodInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.MethodInjection);
end;

procedure TTestMemberInjections.TestFieldInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.FieldInjection);
end;

{$ENDREGION}


{$REGION 'TTestDirectCircularDependency'}

procedure TTestDirectCircularDependency.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<IChicken, TCircularDependencyChicken>;
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
  fContainer.RegisterType<IChicken, TChicken>;
  fContainer.RegisterType<IEgg, TEgg>;
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


end.
