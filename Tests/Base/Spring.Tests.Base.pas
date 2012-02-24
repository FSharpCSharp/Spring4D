{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Tests.Base;

{$I Spring.inc}

interface

uses
  Classes,
  TypInfo,
  DateUtils,
  SysUtils,
  Graphics,
  Variants,
  Types,
  TestFramework,
  TestExtensions,
  Generics.Defaults,
  Spring,
  Spring.SystemUtils;

type
  TTestNullableInteger = class(TTestCase)
  private
    fInteger: Nullable<Integer>;
  published
    procedure TestInitialValue;
    procedure GetValueOrDefault;
    procedure TestAssignFive;
    procedure TestAssignNil;
    procedure TestException;
    procedure TestLocalVariable;
    procedure TestFromVariant;
    procedure TestEquals;
  end;

  TTestLazy = class(TTestCase)
  private
    fBalance: ILazy<Integer>;
  protected
    const
      CExpectedBalance = 100;
  published
    procedure TestByValueFactory;
    procedure TestByValue;
  end;

  TTestEmptyMulticastEvent = class(TTestCase)
  private
    fEvent: IMulticastNotifyEvent;
    fASender: TObject;
    fAInvoked: Boolean;
    fBSender: TObject;
    fBInvoked: Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure HandlerA(sender: TObject);
    procedure HandlerB(sender: TObject);
  published
    procedure TestEmpty;
    procedure TestInvoke;
    procedure TestOneHandler;
    procedure TestTwoHandlers;
  end;


implementation


{$REGION 'TTestNullableInteger'}

procedure TTestNullableInteger.TestInitialValue;
begin
  CheckFalse(fInteger.HasValue);
end;

procedure TTestNullableInteger.GetValueOrDefault;
begin
  Assert(not fInteger.HasValue);
  CheckEquals(Default(Integer), fInteger.GetValueOrDefault);
  CheckEquals(18, fInteger.GetValueOrDefault(18));
end;

procedure TTestNullableInteger.TestAssignFive;
begin
  fInteger := 5;
  Check(fInteger.HasValue);
  CheckEquals(5, fInteger.Value);
  Check(fInteger.Value = 5);
  Check(fInteger.Value <> 3);
end;

procedure TTestNullableInteger.TestAssignNil;
begin
  fInteger := 5;
  Assert(fInteger.HasValue);
  fInteger := nil;
  CheckFalse(fInteger.HasValue);
end;

procedure TTestNullableInteger.TestException;
begin
  ExpectedException := EInvalidOperationException;
  fInteger.Value;
end;

procedure TTestNullableInteger.TestLocalVariable;
var
  dirtyValue: Nullable<Integer>;  { lives in stack }
begin
  CheckFalse(dirtyValue.HasValue);
  dirtyValue := 5;
end;

procedure TTestNullableInteger.TestFromVariant;
var
  value: Variant;
const
  ExpectedInteger: Integer = 5;
begin
  value := Null;
  fInteger := Nullable<Integer>.Create(value);
  CheckFalse(fInteger.HasValue);

  fInteger := value;
  CheckFalse(fInteger.HasValue);

  value := ExpectedInteger;
  fInteger := Nullable<Integer>.Create(value);
  CheckTrue(fInteger.HasValue);
  CheckEquals(ExpectedInteger, fInteger.Value);
end;

procedure TTestNullableInteger.TestEquals;
var
  a, b: Nullable<Integer>;
begin
  Assert(not a.HasValue);
  Assert(not b.HasValue);

  CheckTrue(a.Equals(b));
  CheckTrue(b.Equals(a));

  a := 2;
  CheckFalse(a.Equals(b));
  CheckFalse(b.Equals(a));

  b := 2;
  CheckTrue(a.Equals(b));

  b := 3;
  CheckFalse(a.Equals(b));
end;

{$ENDREGION}


{$REGION 'TTestEmptyMulticastEvent'}

procedure TTestEmptyMulticastEvent.SetUp;
begin
  inherited;
  fEvent := TMulticastNotifyEvent.Create;
end;

procedure TTestEmptyMulticastEvent.TearDown;
begin
  inherited;
  fEvent := nil;
  fASender := nil;
  fAInvoked := False;
  fBSender := nil;
  fBInvoked := False;
end;

procedure TTestEmptyMulticastEvent.HandlerA(sender: TObject);
begin
  fASender := sender;
  fAInvoked := True;
end;

procedure TTestEmptyMulticastEvent.HandlerB(sender: TObject);
begin
  fBSender := sender;
  fBInvoked := True;
end;

procedure TTestEmptyMulticastEvent.TestEmpty;
begin
  CheckEquals(0, fEvent.Count);
  CheckTrue(fEvent.IsEmpty);
end;

procedure TTestEmptyMulticastEvent.TestInvoke;
begin
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);
end;

procedure TTestEmptyMulticastEvent.TestOneHandler;
begin
  fEvent.Add(HandlerA);
  CheckEquals(1, fEvent.Count);
  CheckFalse(fEvent.IsEmpty);

  fEvent.Invoke(Self);
  CheckTrue(fAInvoked);
  CheckSame(Self, fASender);
  CheckFalse(fBInvoked);
  CheckSame(nil, fBSender);

  fEvent.Remove(HandlerA);
  CheckEquals(0, fEvent.Count);
end;

procedure TTestEmptyMulticastEvent.TestTwoHandlers;
begin
  fEvent.Add(HandlerA);
  fEvent.Add(HandlerB);
  fEvent.Invoke(nil);

  CheckTrue(fAInvoked);
  CheckSame(nil, fASender);
  CheckTrue(fBInvoked);
  CheckSame(nil, fBSender);

  fEvent.Remove(HandlerA);
  CheckEquals(1, fEvent.Count);

  fEvent.Remove(HandlerB);
  CheckEquals(0, fEvent.Count);
end;

{$ENDREGION}


{$REGION 'TTestLazy'}

procedure TTestLazy.TestByValueFactory;
var
  factory: TFunc<Integer>;
begin
  factory :=
    function: Integer
    begin
      Result := CExpectedBalance;
    end;
  fBalance := TLazy<Integer>.Create(factory);

  CheckFalse(fBalance.IsValueCreated);

  CheckEquals(CExpectedBalance, fBalance.Value);
  CheckEquals(CExpectedBalance, (fBalance as ILazy).Value.AsInteger);

  CheckTrue(fBalance.IsValueCreated);
end;

procedure TTestLazy.TestByValue;
begin
  fBalance := TLazy<Integer>.CreateFrom(CExpectedBalance);

  CheckTrue(fBalance.IsValueCreated);

  CheckEquals(CExpectedBalance, fBalance.Value);
  CheckEquals(CExpectedBalance, (fBalance as ILazy).Value.AsInteger);

  CheckTrue(fBalance.IsValueCreated);
end;

{$ENDREGION}

end.
