unit FreezableTests;

interface

uses
  TestFramework,
  HasCount,
  Spring.Interception;

type
  TFreezableTest = class(TTestCase)
  private
    function GetInterceptedMethodsCountFor<TInterceptor: class,
      IInterceptor, IHasCount>(freezable: TObject): Integer;
  published
    procedure IsFreezable_should_be_false_for_objects_created_with_ctor;
    procedure IsFreezable_should_be_true_for_objects_created_with_MakeFreezable;
    procedure Freezable_should_work_normally;
    procedure Frozen_object_should_throw_ObjectFrozenException_when_trying_to_set_a_property;
    procedure Frozen_object_should_not_throw_when_trying_to_read_it;
    procedure Freeze_nonFreezable_object_should_throw_NotFreezableObjectException;
    procedure Freezable_should_not_intercept_property_getters;
    procedure Freezable_should_not_intercept_normal_methods;
    procedure Freezable_should_intercept_property_setters;
    procedure DynProxyGetTarget_should_return_proxy_itself;
    procedure Freezable_should_log_getters_and_setters;
    procedure Freezable_should_not_intercept_methods;
//    procedure Freezable_should_not_hold_any_reference_to_created_objects;
    procedure Freezable_should_freeze_classes_with_nonVirtual_methods;
    procedure Freezable_should_throw_when_trying_to_freeze_classes_with_nonVirtual_setters;
  end;

implementation

uses
  SysUtils,
  Spring,
  Spring.Collections,
  CallLoggingInterceptor,
  Freezable,
  Pet;

{ TFreezableTest }

procedure TFreezableTest.IsFreezable_should_be_false_for_objects_created_with_ctor;
var
  nonFreezablePet: TPet;
begin
  nonFreezablePet := TPet.Create;
  try
    CheckFalse(TFreezable.IsFreezable(nonFreezablePet));
  finally
    nonFreezablePet.Free;
  end;
end;

procedure TFreezableTest.IsFreezable_should_be_true_for_objects_created_with_MakeFreezable;
var
  freezablePet: TPet;
begin
  freezablePet := TFreezable.MakeFreezable<TPet>;
  try
    CheckTrue(TFreezable.IsFreezable(freezablePet));
  finally
    freezablePet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_work_normally;
var
  pet: TPet;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.Age := 3;
    pet.Deceased := True;
    pet.Name := 'Rex';
    pet.Age := pet.Age + Length(pet.Name);
    pet.ToString;
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Frozen_object_should_throw_ObjectFrozenException_when_trying_to_set_a_property;
var
  pet: TPet;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.Age := 3;

    TFreezable.Freeze(pet);

    ExpectedException := EObjectFrozenException;
    pet.Name := 'This should throw';
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Frozen_object_should_not_throw_when_trying_to_read_it;
var
  pet: TPet;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.Age := 3;

    TFreezable.Freeze(pet);

    CheckEquals(3, pet.Age);
    CheckEquals('', pet.Name);
    CheckEquals(False, pet.Deceased);
    pet.ToString;
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freeze_nonFreezable_object_should_throw_NotFreezableObjectException;
var
  pet: TPet;
begin
  pet := TPet.Create;
  try
    ExpectedException := ENotFreezableObjectException;
    TFreezable.Freeze(pet);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_not_intercept_property_getters;
var
  pet: TPet;
  interceptedMethodsCount: Integer;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    TFreezable.Freeze(pet);
    CheckEquals(0, pet.Age);
    interceptedMethodsCount := GetInterceptedMethodsCountFor<TFreezableInterceptor>(pet);
    CheckEquals(0, interceptedMethodsCount);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_not_intercept_normal_methods;
var
  pet: TPet;
  notUsed: string;
  interceptedMethodsCount: Integer;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    notUsed := pet.ToString;
    interceptedMethodsCount := GetInterceptedMethodsCountFor<TFreezableInterceptor>(pet);
    CheckEquals(0, interceptedMethodsCount);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_intercept_property_setters;
var
  pet: TPet;
  interceptedMethodsCount: Integer;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.Age := 5;
    interceptedMethodsCount := GetInterceptedMethodsCountFor<TFreezableInterceptor>(pet);
    CheckEquals(1, interceptedMethodsCount);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.DynProxyGetTarget_should_return_proxy_itself;
var
  pet: TPet;
  hack: IProxyTargetAccessor;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.GetInterface(IProxyTargetAccessor, hack);
    CheckNotNull(hack);
    CheckSame(pet, hack.GetTarget.AsObject);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_log_getters_and_setters;
var
  pet: TPet;
  logsCount, freezeCount: Integer;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.Age := 4;
    CheckEquals(4, pet.Age);
    logsCount := GetInterceptedMethodsCountFor<TCallLoggingInterceptor>(pet);
    freezeCount := GetInterceptedMethodsCountFor<TFreezableInterceptor>(pet);
    CheckEquals(2, logsCount);
    CheckEquals(1, freezeCount);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_not_intercept_methods;
var
  pet: TPet;
  logsCount, freezeCount: Integer;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.ToString;
    logsCount := GetInterceptedMethodsCountFor<TCallLoggingInterceptor>(pet);
    freezeCount := GetInterceptedMethodsCountFor<TFreezableInterceptor>(pet);

    // base implementation of ToString calls each property getter, that we intercept
    // so there will be 3 calls if method is not intercepter, otherwise 4.
    CheckEquals(3, logsCount);
    CheckEquals(0, freezeCount);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_freeze_classes_with_nonVirtual_methods;
var
  pet: TPetWithNonVirtualMethod;
begin
  pet := TFreezable.MakeFreezable<TPetWithNonVirtualMethod>;
  try
    pet.Name := 'Rex';
    pet.NonVirtualMethod;
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_throw_when_trying_to_freeze_classes_with_nonVirtual_setters;
begin
  ExpectedException := EInvalidOperationException;
  try
    TFreezable.MakeFreezable<TPetWithNonVirtualSetter>;
  except
    on E: Exception do
    begin
      CheckEquals('Property NonVirtualProperty is not virtual. Cannot freeze classes with non-virtual properties.', E.Message);
      raise;
    end;
  end;
end;

function TFreezableTest.GetInterceptedMethodsCountFor<TInterceptor>(
  freezable: TObject): Integer;
var
  hack: IProxyTargetAccessor;
  loggingInterceptor: IHasCount;
begin
  Assert(TFreezable.IsFreezable(freezable));
  Supports(freezable, IProxyTargetAccessor, hack);
  Assert(Assigned(hack));
  loggingInterceptor := hack.GetInterceptors.Where(
    function(const i: IInterceptor): Boolean
    begin
      Result := i is TInterceptor;
    end).Single as IHasCount;
  Result := loggingInterceptor.Count;
end;

initialization
  RegisterTest(TFreezableTest.Suite);

end.
