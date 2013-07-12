{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2013 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
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
  Spring;

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

  TTestNullableBoolean = class(TTestCase)
  private
    fBoolean: Nullable<Boolean>;
  published
    procedure TestIssue55;
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
    type
      TEventInt64 = procedure(const Value: Int64 ) of object;
      TEventSingle = procedure(const Value: Single ) of object;
      TEventDouble = procedure(const Value: Double ) of object;
      TEventExtended = procedure(const Value: Extended ) of object;
  private
    fEvent: IMulticastNotifyEvent;
    fASender: TObject;
    fAInvoked: Boolean;
    fBSender: TObject;
    fBInvoked: Boolean;
    fHandlerInvokeCount: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure HandlerA(sender: TObject);
    procedure HandlerB(sender: TObject);

    procedure HandlerInt64(const value: Int64);
    procedure HandlerSingle(const value: Single);
    procedure HandlerDouble(const value: Double);
    procedure HandlerExtended(const value: Extended);
  published
    procedure TestEmpty;
    procedure TestInvoke;
    procedure TestOneHandler;
    procedure TestTwoHandlers;
    procedure TestIssue58;
    procedure TestIssue60;
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


{$REGION 'TTestNullableBoolean'}

procedure TTestNullableBoolean.TestIssue55;
var
  v: Variant;
begin
  fBoolean := True;
  v := fBoolean;
  CheckTrue(v);
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
  fHandlerInvokeCount := 0;
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

procedure TTestEmptyMulticastEvent.HandlerDouble(const value: Double);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestEmptyMulticastEvent.HandlerExtended(const value: Extended);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestEmptyMulticastEvent.HandlerInt64(const value: Int64);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestEmptyMulticastEvent.HandlerSingle(const value: Single);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
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

procedure TTestEmptyMulticastEvent.TestIssue58;
var
  e: Event<TNotifyEvent>;
  i: IEvent<TNotifyEvent>;
  t: TNotifyEvent;
begin
  i := e;
  t := e.Invoke;
  Check(Assigned(i));
end;

procedure TTestEmptyMulticastEvent.TestIssue60;
var
  eventInt64: Event<TEventInt64>;
  eventSingle: Event<TEventSingle>;
  eventDouble: Event<TEventDouble>;
  eventExtended: Event<TEventExtended>;
begin
  eventInt64 := Event<TEventInt64>.Create;
  eventSingle := Event<TEventSingle>.Create;
  eventDouble := Event<TEventDouble>.Create;
  eventExtended := Event<TEventExtended>.Create;

  eventInt64.Add(HandlerInt64);
  eventSingle.Add(HandlerSingle);
  eventDouble.Add(HandlerDouble);
  eventExtended.Add(HandlerExtended);

  eventInt64.Invoke(42);
  eventSingle.Invoke(42);
  eventDouble.Invoke(42);
  eventExtended.Invoke(42);

  CheckEquals(4, fHandlerInvokeCount);
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
