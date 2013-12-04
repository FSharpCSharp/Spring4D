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

{$IFDEF MACOS}
  {$DEFINE LogConsole}
{$ENDIF MACOS}

interface

uses
  TestFramework,
  Spring.TestUtils,
  Spring,
  Spring.Events;

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
    procedure TestDefaultReturnsInitialValue;
  end;

  TTestNullableBoolean = class(TTestCase)
  private
    fBoolean: Nullable<Boolean>;
  published
    procedure TestIssue55;
  end;

  TTestGuard = class(TExceptionCheckerTestCase)
  published
    procedure TestIsNullReference;
    procedure TestCheckRange;
    procedure TestNotNull;
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

  {$M+}
  TProc<T1, T2> = reference to procedure(arg1: T1; arg2: T2);
  {$M-}

  TTestMulticastEvent = class(TTestCase)
  strict private
    type
      TEventInt64 = procedure(const Value: Int64) of object;
      TEventSingle = procedure(const Value: Single) of object;
      TEventDouble = procedure(const Value: Double) of object;
      TEventExtended = procedure(const Value: Extended) of object;
    const
      CNumber = 5;
      CText = 'test';
  strict private
    fEvent: IMulticastNotifyEvent;
    fASender: TObject;
    fAInvoked: Boolean;
    fBSender: TObject;
    fBInvoked: Boolean;
    fHandlerInvokeCount: Integer;
    fProc: TProc<Integer, string>;
  strict protected
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
    procedure TestRecordType;
    procedure TestIssue58;
    procedure TestDelegate;
    procedure TestIssue60();
  end;

type
  TTestMulticastEventStackSize = class(TTestCase)
  strict private
    type
      {TODO -o##jwp -cEnhance : Add more data types: all the int and float types, records, classes, interfaces, variants }
      TEventDouble = procedure(const Value: Double) of object;
      TEventExtended = procedure(const Value: Extended) of object;
      TEventGuid = procedure(const Value: TGUID) of object;
      TEventInt64 = procedure(const Value: Int64) of object;
      TEventSingle = procedure(const Value: Single) of object;
    const
      Integer42: Integer = 42;
      Float42 = 42.0;
      Double42: Double = 42.0;
      Extended42: Extended = 42.0;
      Int6442: Int64 = 42;
      Single42: Single = 42;
      GUID42: TGUID = '{CCD21A05-9527-411F-AB44-AAF44C0E0DAF}';
  strict private
    fHandlerInvokeCount: Integer;
  strict protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure HandlerDouble(const value: Double);
    procedure HandlerExtended(const value: Extended);
    procedure HandlerGuid(const value: TGUID);
    procedure HandlerInt64(const value: Int64);
    procedure HandlerSingle(const value: Single);
    procedure LogEnter(const expected: Integer; const MethodName: string); virtual;
    procedure LogLeave(const expected: Integer); virtual;
  published
    procedure TestIssue60Double();
    procedure TestIssue60DoubleAssignedConst();
    procedure TestIssue60Extended();
    procedure TestIssue60ExtendedAssignedConst();
    procedure TestIssue60GuidAssignedConst();
    procedure TestIssue60Int64();
    procedure TestIssue60Int64AssignedConst();
    procedure TestIssue60Single();
    procedure TestIssue60SingleAssignedConst();
  end;

implementation

uses
  Classes,
  SysUtils,
  Variants;

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

procedure TTestNullableInteger.TestDefaultReturnsInitialValue;
begin
  fInteger := Default(Nullable<Integer>);
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


{$REGION 'TTestMulticastEvent'}

procedure TTestMulticastEvent.SetUp;
begin
  inherited;
  fEvent := TMulticastNotifyEvent.Create();
  fProc :=
    procedure(i: Integer; s: string)
    begin
      Inc(fHandlerInvokeCount, i);
      CheckEquals(CText, s);
    end;
end;

procedure TTestMulticastEvent.TearDown;
begin
  inherited;
  fEvent := nil;
  fASender := nil;
  fAInvoked := False;
  fBSender := nil;
  fBInvoked := False;
  fHandlerInvokeCount := 0;
end;

procedure TTestMulticastEvent.HandlerA(sender: TObject);
begin
  fASender := sender;
  fAInvoked := True;
end;

procedure TTestMulticastEvent.HandlerB(sender: TObject);
begin
  fBSender := sender;
  fBInvoked := True;
end;

procedure TTestMulticastEvent.HandlerDouble(const value: Double);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.HandlerExtended(const value: Extended);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.HandlerInt64(const value: Int64);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.HandlerSingle(const value: Single);
begin
  CheckEquals(42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestDelegate;
var
  e: Event<TProc<Integer, string>>;
begin
  e.Add(fProc);
  CheckEquals(1, e.Count);
  e.Invoke(CNumber, CText);
  CheckEquals(CNumber, fHandlerInvokeCount);
  e.Remove(fProc);
  CheckEquals(0, e.Count);
  e.Invoke(CNumber, CText);
  CheckEquals(CNumber, fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestEmpty;
begin
  CheckEquals(0, fEvent.Count);
  CheckTrue(fEvent.IsEmpty);
end;

procedure TTestMulticastEvent.TestInvoke;
begin
  fEvent.Invoke(Self);
  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);
end;

procedure TTestMulticastEvent.TestIssue58;
var
  e: Event<TNotifyEvent>;
  i: IEvent<TNotifyEvent>;
  t: TNotifyEvent;
begin
  i := e;
  t := e.Invoke;
  Check(Assigned(i));
end;

procedure TTestMulticastEvent.TestIssue60();
var
  eventInt64: Event<TEventInt64>;
  eventSingle: Event<TEventSingle>;
  eventDouble: Event<TEventDouble>;
  eventExtended: Event<TEventExtended>;
  expected: Integer;
begin
  expected := 0;

  eventInt64 := Event<TEventInt64>.Create();
  eventSingle := Event<TEventSingle>.Create();
  eventDouble := Event<TEventDouble>.Create();
  eventExtended := Event<TEventExtended>.Create();

  eventInt64.Add(HandlerInt64);
  eventSingle.Add(HandlerSingle);
  eventDouble.Add(HandlerDouble);
  eventExtended.Add(HandlerExtended);

  eventInt64.Invoke(42); Inc(expected);
  eventSingle.Invoke(42); Inc(expected);
  eventDouble.Invoke(42); Inc(expected);
  eventExtended.Invoke(42); Inc(expected);

  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEvent.TestOneHandler;
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

procedure TTestMulticastEvent.TestRecordType;
var
  e: Event<TNotifyEvent>;
begin
  CheckTrue(e.Enabled);
  CheckTrue(e.IsEmpty);

  e.Add(HandlerA);
  e.Add(HandlerB);
  e.Invoke(nil);

  CheckFalse(e.IsEmpty);
  CheckEquals(2, e.Count);

  CheckTrue(fAInvoked);
  CheckSame(nil, fASender);
  CheckTrue(fBInvoked);
  CheckSame(nil, fBSender);

  e.Remove(HandlerA);
  CheckEquals(1, e.Count);

  e.Remove(HandlerB);
  CheckEquals(0, e.Count);
end;

procedure TTestMulticastEvent.TestTwoHandlers;
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


{$REGION 'TTestGuard'}

procedure TTestGuard.TestCheckRange;
var
  dynArray: array of Byte;
const
  len = 4;
  idx = 1;
begin
  // check string (1-based)
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange('abcde', 0);
    end);
  Guard.CheckRange('abcde', 1);
  Guard.CheckRange('abcde', 5);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange('abcde', 6);
    end);

  // check 0-based byte array
  SetLength(dynArray, 4);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(dynArray, -1);
    end);
  Guard.CheckRange(dynArray, 0);
  Guard.CheckRange(dynArray, 3);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(dynArray, 4);
    end);

  // check 1-based range
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 0, 0, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 0, 1, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 0, 5, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 1, -1, idx);
    end);
  Guard.CheckRange(len, 1, 0, idx);
  Guard.CheckRange(len, 1, 1, idx);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 1, 5, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 5, 0, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 5, 1, idx);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      Guard.CheckRange(len, 5, 5, idx);
    end);
end;

procedure TTestGuard.TestIsNullReference;
var
  obj: TObject;
  intf: IInterface;
  e: TNotifyEvent;
begin
  obj := nil;
  CheckTrue(Guard.IsNullReference(obj, TypeInfo(TObject)));
  CheckTrue(Guard.IsNullReference(intf, TypeInfo(IInterface)));
  e := nil;
  CheckTrue(Guard.IsNullReference(e, TypeInfo(TNotifyEvent)));
  TMethod(e).Data := Self;
  CheckFalse(Assigned(e));
  CheckFalse(Guard.IsNullReference(e, TypeInfo(TNotifyEvent)));
end;

procedure TTestGuard.TestNotNull;
var
  intf: IInterface;
begin
  StartExpectingException(EArgumentNullException);
  Guard.CheckNotNull(intf, 'intf');
  StopExpectingException();
end;

{$ENDREGION}

{$REGION 'TTestMulticastEventStackSize'}

procedure TTestMulticastEventStackSize.SetUp;
begin
  inherited;
end;

procedure TTestMulticastEventStackSize.TearDown;
begin
  inherited;
  fHandlerInvokeCount := 0;
end;

procedure TTestMulticastEventStackSize.HandlerDouble(const value: Double);
begin
  CheckEquals(Double42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.HandlerExtended(const value: Extended);
begin
  CheckEquals(Extended42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.HandlerGuid(const value: TGUID);
begin
  CheckEquals(GUIDToString(GUID42), GUIDToString(value));
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.HandlerInt64(const value: Int64);
begin
  CheckEquals(Int6442, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.HandlerSingle(const value: Single);
begin
  CheckEquals(Single42, value);
  Inc(fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.LogEnter(const expected: Integer; const MethodName: string);
begin
  Writeln(Format('%s.%s', [ClassName, MethodName]));
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
end;

procedure TTestMulticastEventStackSize.LogLeave(const expected: Integer);
begin
  Writeln(Format('Exit: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
end;

procedure TTestMulticastEventStackSize.TestIssue60Double();
var
  eventDouble: Event<TEventDouble>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  LogEnter(expected, 'TestIssue60Double');
{$ENDIF LogConsole}
  eventDouble := Event<TEventDouble>.Create();
  eventDouble.Add(HandlerDouble);
  eventDouble.Invoke(42); Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60DoubleAssignedConst();
var
  eventDouble: Event<TEventDouble>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  LogEnter(expected, 'TestIssue60DoubleAssignedConst');
{$ENDIF LogConsole}
  eventDouble := Event<TEventDouble>.Create();
  eventDouble.Add(HandlerDouble);
  eventDouble.Invoke(Double42); Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60Extended();
var
  eventExtended: Event<TEventExtended>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  LogEnter(expected, 'TestIssue60Extended');
{$ENDIF LogConsole}
  eventExtended := Event<TEventExtended>.Create();
  eventExtended.Add(HandlerExtended);
  eventExtended.Invoke(42); Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60ExtendedAssignedConst();
var
  eventExtended: Event<TEventExtended>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  LogEnter(expected, 'TestIssue60ExtendedAssignedConst');
{$ENDIF LogConsole}
  eventExtended := Event<TEventExtended>.Create();
  eventExtended.Add(HandlerExtended);
  eventExtended.Invoke(Extended42); Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60GuidAssignedConst();
var
  eventExtended: Event<TEventGuid>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  Writeln('TTestMulticastEventStackSize.TestIssue60GuidAssignedConst');
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
{$ENDIF LogConsole}
  eventExtended := Event<TEventGuid>.Create();
  eventExtended.Add(HandlerGuid);
  eventExtended.Invoke(GUID42); Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60Int64();
var
  eventInt64: Event<TEventInt64>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  Writeln('TTestMulticastEventStackSize.TestIssue60Int64');
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
{$ENDIF LogConsole}
  eventInt64 := Event<TEventInt64>.Create();
  eventInt64.Add(HandlerInt64);
  eventInt64.Invoke(42); Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60Int64AssignedConst();
var
  eventInt64: Event<TEventInt64>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  Writeln('TTestMulticastEventStackSize.TestIssue60Int64AssignedConst');
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
{$ENDIF LogConsole}
  eventInt64 := Event<TEventInt64>.Create();
  eventInt64.Add(HandlerInt64);
  eventInt64.Invoke(Int6442); Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60Single();
var
  eventSingle: Event<TEventSingle>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  Writeln('TTestMulticastEventStackSize.TestIssue60Single');
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
{$ENDIF LogConsole}
  eventSingle := Event<TEventSingle>.Create();
  eventSingle.Add(HandlerSingle);
  eventSingle.Invoke(42); Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

procedure TTestMulticastEventStackSize.TestIssue60SingleAssignedConst();
var
  eventSingle: Event<TEventSingle>;
  expected: Integer;
begin
  expected := 0;
{$IFDEF LogConsole}
  Writeln('TTestMulticastEventStackSize.TestIssue60SingleAssignedConst');
  Writeln(Format('Entry: Expected=%d, got fHandlerInvokeCount=%d', [expected, fHandlerInvokeCount]));
{$ENDIF LogConsole}
  eventSingle := Event<TEventSingle>.Create();
  eventSingle.Add(HandlerSingle);
  eventSingle.Invoke(Single42); Inc(expected);
{$IFDEF LogConsole}
  LogLeave(expected);
{$ENDIF LogConsole}
  CheckEquals(expected, fHandlerInvokeCount);
end;

{$ENDREGION}

end.
