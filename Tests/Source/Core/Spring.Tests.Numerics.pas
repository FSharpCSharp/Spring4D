{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 Alexandru Ciobanu                       }
{                                                                           }
{           http://alex.ciobanu.org                                         }
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

unit Spring.Tests.Numerics;
interface
uses SysUtils,
     Classes,
     Math,
     Variants,
     TestFramework,
     Spring,
     Spring.ResourceStrings,
     Spring.Numerics.BigDecimal,
     Spring.Numerics.BigInteger,
     Spring.Numerics.BigCardinal,
     Spring.Numerics.Half;

type
  TTextCaseEx = class(TTestCase)
  protected
    procedure CheckException(const AProcedure: TProc; const AExceptionClass: ExceptionClass; const AMessage: string); overload;
  end;

  TTestHalf = class(TTextCaseEx)
  private
    FHalf: Half;

    function GetWord: Word;
    property FWord: Word read GetWord;

  published
    procedure Test_Implicit_Single_Half;
    procedure Test_Implicit_Half_Single;
    procedure Test_Implicit_Variant_Half;
    procedure Test_Implicit_Half_Variant;
    procedure Test_Add_Half_Half;
    procedure Test_Add_Half_Single;
    procedure Test_Add_Single_Half;
    procedure Test_Subtract_Half_Half;
    procedure Test_Subtract_Half_Single;
    procedure Test_Subtract_Single_Half;
    procedure Test_Multiply_Half_Half;
    procedure Test_Multiply_Half_Single;
    procedure Test_Multiply_Single_Half;
    procedure Test_Divide_Half_Half;
    procedure Test_Divide_Half_Single;
    procedure Test_Divide_Single_Half;
    procedure Test_Negative;
    procedure Test_Positive;
    procedure Test_Equal_Half_Half;
    procedure Test_Equal_Half_Single;
    procedure Test_Equal_Single_Half;
    procedure Test_NotEqual_Half_Half;
    procedure Test_NotEqual_Half_Single;
    procedure Test_NotEqual_Single_Half;
    procedure Test_GreaterThan_Half_Half;
    procedure Test_GreaterThan_Half_Single;
    procedure Test_GreaterThan_Single_Half;
    procedure Test_GreaterThanOrEqual_Half_Half;
    procedure Test_GreaterThanOrEqual_Half_Single;
    procedure Test_GreaterThanOrEqual_Single_Half;
    procedure Test_LessThan_Half_Half;
    procedure Test_LessThan_Half_Single;
    procedure Test_LessThan_Single_Half;
    procedure Test_LessThanOrEqual_Half_Half;
    procedure Test_LessThanOrEqual_Half_Single;
    procedure Test_LessThanOrEqual_Single_Half;
    procedure Test_Min;
    procedure Test_Max;
    procedure Test_Zero;
    procedure Test_MinusZero;
    procedure Test_One;
    procedure Test_MinusOne;
    procedure Test_Ten;
    procedure Test_MinusTen;
    procedure Test_Infinity;
    procedure Test_MinusInfinity;
  end;

  TTestBigCardinal = class(TTextCaseEx)
  private
    procedure TestAllCompOperators(const X, Y: BigCardinal; const IsStrict: Boolean);

  published
    procedure TestCreateAndToXXX();
    procedure TestIntToStrAndBack();
    procedure TestIntToStrAndTryBack();
    procedure TestHexToStrAndBack();
    procedure TestHexToStrAndTryBack();
    procedure TestIntToStrHexAndBack();
    procedure TestIntToStrHexAndTryBack();
    procedure TestCompOps();
    procedure TestArithmOps();
    procedure TestBitOps();
    procedure TestImplicits();
    procedure TestExplicits();
    procedure TestBigPow2();
    procedure TestDiv2ShrEq();
    procedure TestMul2ShlEq();
    procedure TestExceptions();
    procedure TestArithmOverflows();
    procedure TestStatNums();
    procedure TestIsProps();
    procedure TestPow();
    procedure TestDivMod();
    procedure TestVariantSupport();

    procedure Test_Bug_0();
  end;

  TTestBigInteger = class(TTextCaseEx)
  private
     function FromHex(const AStr: string; const DoNeg: Boolean): BigInteger;
    procedure TestAllCompOperatorsAndCompareTo(const X, Y: BigInteger; const IsStrict: Boolean);

  published
    procedure TestCreateAndToXXX();
    procedure TestIntToStrAndBack();
    procedure TestIntToStrAndTryBack();
    procedure TestCompOps();
    procedure TestArithmOps_Positive();
    procedure TestArithmOps_Negative();
    procedure TestBitOps();
    procedure TestImplicits();
    procedure TestExplicits();
    procedure TestBigPow2_Positive();
    procedure TestBigPow2_Negative();
    procedure TestExceptions();
    procedure TestAbs();
    procedure TestStatNums();
    procedure TestIsProps();
    procedure TestSign();
    procedure TestPow();
    procedure TestDivMod();

    procedure TestVariantSupport;
  end;

  TTestBigDecimal = class(TTextCaseEx)
  private
    FOldDec, FOldTh: Char;

    procedure TestOp(const X, Y: BigDecimal; const AComp: NativeInt);
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Test_Create_Integer_Scale;
    procedure Test_Create_Cardinal_Scale;
    procedure Test_Create_Int64_Scale;
    procedure Test_Create_UInt64_Scale;
    procedure Test_Create_BigInteger_Scale;
    procedure Test_Create_BigCardinal_Scale;
    procedure Test_Create_Double;
    procedure Test_ToDouble;
    procedure Test_ToBigInteger;
    procedure Test_CompareTo_And_Ops;
    procedure Test_Abs;
    procedure Test_IsZero;
    procedure Test_IsNegative;
    procedure Test_IsPositive;
    procedure Test_Precision;
    procedure Test_Scale;
    procedure Test_Sign;
    procedure Test_Rescale;
    procedure Test_Pow;
    procedure Test_ScaleByPowerOfTen;
    procedure Test_Divide;
    procedure Test_Round;
    procedure Test_TryParse_FmtSettings;
    procedure Test_TryParse;
    procedure Test_Parse_FmtSettings;
    procedure Test_Parse;
    procedure Test_ToString_FmtSettings;
    procedure Test_ToString;
    procedure Test_Op_Add;
    procedure Test_Op_Subtract;
    procedure Test_Op_Multiply;
    procedure Test_Op_Divide;
    procedure Test_Op_Negative;
    procedure Test_Op_Positive;
    procedure Test_Implicit_From_Cardinal;
    procedure Test_Implicit_From_UInt64;
    procedure Test_Implicit_From_Integer;
    procedure Test_Implicit_From_In64;
    procedure Test_Implicit_From_Double;
    procedure Test_Implicit_From_BigCardinal;
    procedure Test_Implicit_From_BigInteger;
    procedure Test_Implicit_To_Variant;
    procedure Test_Explicit_From_Variant;
    procedure Test_Explicit_To_Double;
    procedure Test_Explicit_To_Extended;
    procedure Test_VarType;
    procedure Test_VariantSupport;
    procedure Test_Zero;
    procedure Test_One;
    procedure Test_MinusOne;
    procedure Test_Ten;
    procedure Test_MinusTen;

    { Situational tests ... more to come }
    procedure Test_Conformance_1;
    procedure Test_Conformance_2;
    procedure Test_Conformance_3;
    procedure Test_Conformance_4;
    procedure Test_Conformance_5;
  end;

implementation

{ TTestHalf }

function TTestHalf.GetWord: Word;
var
  LHalf: Half absolute Result;
begin
  LHalf := FHalf;
end;

procedure TTestHalf.Test_Add_Half_Half;
begin
  FHalf := Half(1) + Half(2.3);
  CheckEquals(17049, FWord);
end;

procedure TTestHalf.Test_Add_Half_Single;
begin
  FHalf := Half(1) + 2.3;
  CheckEquals(17049, FWord);
end;

procedure TTestHalf.Test_Add_Single_Half;
begin
  FHalf := 1 + Half(2.3);
  CheckEquals(17049, FWord);
end;

procedure TTestHalf.Test_Divide_Half_Half;
begin
  FHalf := Half(10)/Half(4.4);
  CheckEquals(16524, FWord);
end;

procedure TTestHalf.Test_Divide_Half_Single;
begin
  FHalf := Half(10)/4.4;
  CheckEquals(16524, FWord);
end;

procedure TTestHalf.Test_Divide_Single_Half;
begin
  FHalf := 10/Half(4.4);
  CheckEquals(16524, FWord);
end;

procedure TTestHalf.Test_Equal_Half_Half;
begin
  CheckTrue(Half(10) = Half(10));
  CheckFalse(Half(10) = Half(-10));
end;

procedure TTestHalf.Test_Equal_Half_Single;
begin
  CheckTrue(Half(10) = 10);
  CheckFalse(Half(10) = -10);
end;

procedure TTestHalf.Test_Equal_Single_Half;
begin
  CheckTrue(10 = Half(10));
  CheckFalse(10 = Half(-10));
end;

procedure TTestHalf.Test_GreaterThanOrEqual_Half_Half;
begin
  CheckTrue(Half(10) >= Half(10));
  CheckTrue(Half(20) >= Half(10));
  CheckTrue(Half(10) >= Half(-10));
  CheckTrue(Half(-0) >= Half(0));
end;

procedure TTestHalf.Test_GreaterThanOrEqual_Half_Single;
begin
  CheckTrue(Half(10) >= 10);
  CheckTrue(Half(20) >= 10);
  CheckTrue(Half(10) >= -10);
  CheckTrue(Half(-0) >= 0);
end;

procedure TTestHalf.Test_GreaterThanOrEqual_Single_Half;
begin
  CheckTrue(10 >= Half(10));
  CheckTrue(20 >= Half(10));
  CheckTrue(10 >= Half(-10));
  CheckTrue(-0 >= Half(0));
end;

procedure TTestHalf.Test_GreaterThan_Half_Half;
begin
  CheckFalse(Half(10) > Half(10));
  CheckTrue(Half(20) > Half(10));
  CheckTrue(Half(10) > Half(-10));
  CheckFalse(Half(-0) > Half(0));
end;

procedure TTestHalf.Test_GreaterThan_Half_Single;
begin
  CheckFalse(Half(10) > 10);
  CheckTrue(Half(20) > 10);
  CheckTrue(Half(10) > -10);
  CheckFalse(Half(-0) > 0);
end;

procedure TTestHalf.Test_GreaterThan_Single_Half;
begin
  CheckFalse(10 > Half(10));
  CheckTrue(20 > Half(10));
  CheckTrue(10 > Half(-10));
  CheckFalse(-0 > Half(0));
end;

procedure TTestHalf.Test_Implicit_Half_Single;
begin
  FHalf := 0;
  CheckEquals(0, Single(FHalf));

  FHalf := 100;
  CheckEquals(100, Single(FHalf));

  FHalf := -10.54;
  CheckTrue(Abs(-10.54 - Single(FHalf)) < 0.01);
end;

procedure TTestHalf.Test_Implicit_Half_Variant;
var
  V: Variant;
begin
  V := Half(0);
  CheckTrue(CompareValue(Single(V), 0) = 0);

  V := Half(100);
  CheckTrue(CompareValue(Single(V), 100) = 0);

  V := Half(-10.54);
  CheckTrue(Abs(-10.54 - Single(V)) < 0.01);
end;

procedure TTestHalf.Test_Implicit_Single_Half;
begin
  FHalf := 0;
  CheckEquals(0, FWord);

  FHalf := 100;
  CheckEquals(22080, FWord);

  FHalf := -10.54;
  CheckEquals(51525, FWord);
end;

procedure TTestHalf.Test_Implicit_Variant_Half;
begin
  FHalf := Variant(0);
  CheckEquals(0, FWord);

  FHalf := Variant(100);
  CheckEquals(22080, FWord);

  FHalf := Variant(-10.54);
  CheckEquals(51525, FWord);
end;

procedure TTestHalf.Test_Infinity;
begin
  FHalf := Half.Infinity;
  CheckEquals(31743, FWord);
end;

procedure TTestHalf.Test_LessThanOrEqual_Half_Half;
begin
  CheckTrue(Half(10) <= Half(10));
  CheckFalse(Half(20) <= Half(10));
  CheckFalse(Half(10) <= Half(-10));
  CheckTrue(Half(-0) <= Half(0));
end;

procedure TTestHalf.Test_LessThanOrEqual_Half_Single;
begin
  CheckTrue(Half(10) <= 10);
  CheckFalse(Half(20) <= 10);
  CheckFalse(Half(10) <= -10);
  CheckTrue(Half(-0) <= 0);
end;

procedure TTestHalf.Test_LessThanOrEqual_Single_Half;
begin
  CheckTrue(10 <= Half(10));
  CheckFalse(20 <= Half(10));
  CheckFalse(10 <= Half(-10));
  CheckTrue(-0 <= Half(0));
end;

procedure TTestHalf.Test_LessThan_Half_Half;
begin
  CheckFalse(Half(10) < Half(10));
  CheckTrue(Half(-3) < Half(0));
  CheckFalse(Half(20) < Half(10));
  CheckFalse(Half(10) < Half(-10));
  CheckFalse(Half(-0) < Half(0));
end;

procedure TTestHalf.Test_LessThan_Half_Single;
begin
  CheckFalse(Half(10) < 10);
  CheckTrue(Half(-3) < 0);
  CheckFalse(Half(20) < 10);
  CheckFalse(Half(10) < -10);
  CheckFalse(Half(-0) < 0);
end;

procedure TTestHalf.Test_LessThan_Single_Half;
begin
  CheckFalse(10 < Half(10));
  CheckTrue(-3 < Half(0));
  CheckFalse(20 < Half(10));
  CheckFalse(10 < Half(-10));
  CheckFalse(-0 < Half(0));
end;

procedure TTestHalf.Test_Max;
begin
  FHalf := Half.Max;
  CheckEquals(31743, FWord);
end;

procedure TTestHalf.Test_Min;
begin
  FHalf := Half.Min;
  CheckEquals(1024, FWord);
end;

procedure TTestHalf.Test_Multiply_Half_Half;
begin
  FHalf := Half(3) * Half(0.1);
  CheckEquals(13516, FWord);
end;

procedure TTestHalf.Test_Multiply_Half_Single;
begin
  FHalf := Half(3) * 0.1;
  CheckEquals(13516, FWord);
end;

procedure TTestHalf.Test_Multiply_Single_Half;
begin
  FHalf := 3 * Half(0.1);
  CheckEquals(13516, FWord);
end;

procedure TTestHalf.Test_Negative;
begin
  FHalf := -Half(4.55);
  CheckEquals(50316, FWord);
end;

procedure TTestHalf.Test_MinusInfinity;
begin
  FHalf := Half.MinusInfinity;
  CheckEquals(64512, FWord);
end;

procedure TTestHalf.Test_MinusOne;
begin
  FHalf := Half.MinusOne;
  CheckEquals($BC00, FWord);
end;

procedure TTestHalf.Test_MinusTen;
begin
  FHalf := Half.MinusTen;
  CheckEquals($4900, FWord);
end;

procedure TTestHalf.Test_MinusZero;
begin
  FHalf := Half.MinusZero;
  CheckEquals(32768, FWord);
end;

procedure TTestHalf.Test_NotEqual_Half_Half;
begin
  CheckFalse(Half(10) <> Half(10));
  CheckTrue(Half(10) <> Half(-10));
end;

procedure TTestHalf.Test_NotEqual_Half_Single;
begin
  CheckFalse(Half(10) <> 10);
  CheckTrue(Half(10) <> -10);
end;

procedure TTestHalf.Test_NotEqual_Single_Half;
begin
  CheckFalse(10 <> Half(10));
  CheckTrue(10 <> Half(-10));
end;

procedure TTestHalf.Test_One;
begin
  FHalf := Half.One;
  CheckEquals($3C00, FWord);
end;

procedure TTestHalf.Test_Positive;
begin
  FHalf := +Half(4.55);
  CheckEquals(17548, FWord);
end;

procedure TTestHalf.Test_Subtract_Half_Half;
begin
  FHalf := Half(1) - Half(2.3);
  CheckEquals(48434, FWord);
end;

procedure TTestHalf.Test_Subtract_Half_Single;
begin
  FHalf := Half(1) - 2.3;
  CheckEquals(48434, FWord);
end;

procedure TTestHalf.Test_Subtract_Single_Half;
begin
  FHalf := 1 - Half(2.3);
  CheckEquals(48434, FWord);
end;

procedure TTestHalf.Test_Ten;
begin
  FHalf := Half.Ten;
  CheckEquals($C900, FWord);
end;

procedure TTestHalf.Test_Zero;
begin
  FHalf := Half.Zero;
  CheckEquals(0, FWord);
end;

{ TTestBigCardinal }

procedure TTestBigCardinal.TestAllCompOperators(const X, Y: BigCardinal; const IsStrict: Boolean);
var
  AErr: String;
begin
  AErr := ' (X = "' + X.ToString + '"; Y = "' + X.ToString + '")';

  Check(X = X, 'Expected X = X' + AErr);
  Check(Y = Y, 'Expected Y = Y' + AErr);

  Check(X.CompareTo(X) = 0, 'Expected X.CompareTo(X) = 0' + AErr);
  Check(Y.CompareTo(Y) = 0, 'Expected Y.CompareTo(Y) = 0' + AErr);

  Check(X >= X, 'Expected X >= X' + AErr);
  Check(X <= X, 'Expected X <= X' + AErr);

  Check(Y >= Y, 'Expected Y >= Y' + AErr);
  Check(Y <= Y, 'Expected Y <= Y' + AErr);

  Check(not (X > X), 'Expected not (X > X)' + AErr);
  Check(not (X < X), 'Expected not (X < X)' + AErr);
  Check(not (Y > Y), 'Expected not (Y > Y)' + AErr);
  Check(not (Y < Y), 'Expected not (Y > Y)' + AErr);

  Check(X >= Y, 'Expected X >= Y' + AErr);
  Check(Y <= X, 'Expected Y <= X' + AErr);

  if not IsStrict then
  begin
    Check(X > Y, 'Expected X > Y' + AErr);
    Check(Y < X, 'Expected Y < X' + AErr);

    Check(X.CompareTo(Y) > 0, 'Expected X.CompareTo(Y) > 0' + AErr);
    Check(Y.CompareTo(X) < 0, 'Expected Y.CompareTo(X) < 0' + AErr);

    Check(X <> Y, 'Expected X <> Y' + AErr);
    Check(Y <> X, 'Expected Y <> X' + AErr);

    Check(not (Y = X), 'Expected not (Y = X)' + AErr);
    Check(not (X = Y), 'Expected not (X = Y)' + AErr);
  end else
  begin
    Check(X.CompareTo(Y) = 0, 'Expected X.CompareTo(Y) = 0' + AErr);
    Check(y.CompareTo(X) = 0, 'Expected Y.CompareTo(X) = 0' + AErr);

    Check(not (X > Y), 'Expected not (X > Y)' + AErr);
    Check(not (Y > X), 'Expected not (Y > X)' + AErr);

    Check(not (X < Y), 'Expected not (X > Y)' + AErr);
    Check(not (Y < X), 'Expected not (Y > X)' + AErr);

    Check(Y = X, 'Expected Y = X' + AErr);
    Check(X = Y, 'Expected X = Y' + AErr);

    Check(not (Y <> X), 'Expected not (Y <> X)' + AErr);
    Check(not (X <> Y), 'Expected not (X <> Y)' + AErr);
  end
end;

procedure TTestBigCardinal.TestArithmOps;
var
  X, Y, Z: BigCardinal;
begin
  X := BigCardinal.Parse('742038403297403256248056320847328947309842374092374392743974023904732904');

  { Subtraction 1 }
  Z := X - 1;
  Check(Z.ToString = '742038403297403256248056320847328947309842374092374392743974023904732903', 'Expected Z = "742038403297403256248056320847328947309842374092374392743974023904732903"');

  { Addition 1 }
  Z := X + 1;
  Check(Z.ToString = '742038403297403256248056320847328947309842374092374392743974023904732905', 'Expected Z = "742038403297403256248056320847328947309842374092374392743974023904732905"');

  { Multiplication 1 }
  Z := X * 1;
  Check(Z.ToString = '742038403297403256248056320847328947309842374092374392743974023904732904', 'Expected Z = "742038403297403256248056320847328947309842374092374392743974023904732904"');

  { Division 1 }
  Z := X div 1;
  Check(Z.ToString = '742038403297403256248056320847328947309842374092374392743974023904732904', 'Expected Z = "742038403297403256248056320847328947309842374092374392743974023904732904"');

  { Modulo 1 }
  Z := X mod 1;
  Check(Z.ToString = '0', 'Expected Z = "0"');

  { ---------------------------------------------------- }

  X := BigCardinal.Parse('34662493847238423894629524590275259020753492304930000947329473482347387474');

  { Subtraction 0 }
  Z := X - 0;
  Check(Z.ToString = '34662493847238423894629524590275259020753492304930000947329473482347387474', 'Expected Z = "34662493847238423894629524590275259020753492304930000947329473482347387474"');

  { Addition 0 }
  Z := X + 0;
  Check(Z.ToString = '34662493847238423894629524590275259020753492304930000947329473482347387474', 'Expected Z = "34662493847238423894629524590275259020753492304930000947329473482347387474"');

  { Multiplication 0 }
  Z := X * 0;
  Check(Z.ToString = '0', 'Expected Z = "0"');

  { ---------------------------------------------------- }

  X := BigCardinal.Parse('12222222220000000000000000000000000000000000000000000000000000');
  Y := BigCardinal.Parse('2222222220000000000000000000000000000000000000000000000000000');

  { Subtraction x }
  Z := X - Y;
  Check(Z.ToString = '10000000000000000000000000000000000000000000000000000000000000', 'Expected Z = "10000000000000000000000000000000000000000000000000000000000000"');

  { Addition x }
  Z := X + Y;
  Check(Z.ToString = '14444444440000000000000000000000000000000000000000000000000000', 'Expected Z = "14444444440000000000000000000000000000000000000000000000000000"');

  { Multiplication 400 }
  Z := X * 400;
  Check(Z.ToString = '4888888888000000000000000000000000000000000000000000000000000000', 'Expected Z = "4888888888000000000000000000000000000000000000000000000000000000"');

  { Division 100000 }
  Z := X div 100000;
  Check(Z.ToString = '122222222200000000000000000000000000000000000000000000000', 'Expected Z = "122222222200000000000000000000000000000000000000000000000"');

  { Division 200 }
  Z := X div 200;
  Check(Z.ToString = '61111111100000000000000000000000000000000000000000000000000', 'Expected Z = "61111111100000000000000000000000000000000000000000000000000"');

  { --------------------------------- SOME BASICS --------------- }
  X := 10;
  Y := 10;

  Check(X - Y = 0, 'X - Y expected to be 0');
  Check(X + Y = 20, 'X + Y expected to be 20');
  Check(X * Y = 100, 'X * Y expected to be 100');
  Check(X div Y = 1, 'X div Y expected to be 1');
  Check(X mod Y = 0, 'X mod Y expected to be 0');

  { Some other stuff }
  X := 10;
  X := +X;
  Check(X = 10, 'X was expected to be 10');


  X := BigCardinal.Parse('734832789423798427394625642736436434634623452367438527598465298562398423');
  Check(X = +X, 'X was expected to be equal to +X');

  { Check Inc, Dec }
  X := BigCardinal.Parse('734832789423798427394625642736436434634623452367438527598465298562398423');

  Inc(X);
  Check(X = BigCardinal.Parse('734832789423798427394625642736436434634623452367438527598465298562398424'), 'X was expected to be equal to "734832789423798427394625642736436434634623452367438527598465298562398424"');

  Dec(X);
  Check(X = BigCardinal.Parse('734832789423798427394625642736436434634623452367438527598465298562398423'), 'X was expected to be equal to "734832789423798427394625642736436434634623452367438527598465298562398423"');

  X := 100;

  Inc(X, 100);
  Check(X = 200, 'X was expected to be 200');

  Dec(X, 50);
  Check(X = 150, 'X was expected to be 150');
end;

procedure TTestBigCardinal.TestArithmOverflows;
var
  X: BigCardinal;
begin
  {$IFOPT Q+}
  { Do nothing if Q+ is present }
  Check(true, '');
  Exit;
  {$ENDIF}

  { Subtraction }
  X := 1;
  X := X - 2;

  Check(X = $FFFFFFFF, 'Expected X = $FFFFFFFF');

  X := 1;
  X := X - 3;
  Check(X = $FFFFFFFE, 'Expected X = $FFFFFFFE');

  X := BigCardinal.Parse('1000000000000000000000000000000000000000000000000000000000000000000000');
  X := X - X;
  Check(X = 0, 'Expected X = 0');

  X := BigCardinal.Parse('$FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF');
  X := X - BigCardinal.Parse('$100000000000000000000000000000000');
  Check(X.ToHexString = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', 'Expected X = FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF');

  { Dec }
  X := 0;
  Dec(X);
  Check(X = $FFFFFFFF, 'X expected to be FFFFFFFF');

  X := 1;
  Dec(X, 2);
  Check(X = $FFFFFFFF, 'X expected to be FFFFFFFF');

  { Negative }
  X := 1;
  X := -X;
  Check(X = $FFFFFFFF, 'X was expected to be $FFFFFFFF');

  X := $FFFFFFFFFFFFFFFF;
  X := -X;
  Check(X = 1, 'X was expected to be 1');
end;

procedure TTestBigCardinal.TestBigPow2;
const
 Iters = 500;

var
  X: BigCardinal;
  I: Integer;
begin
  { Let's calculate the a power of 2 }
  X := 2;

  { multiply by 2 on each iteration}
  for I := 0 to Iters - 1 do
    X := X * 2;

  { Divide by 4 this time twice as fast }
  for I := 0 to (Iters div 2) - 1 do
    X := X div 4;

  Check(X = 2, 'X is supposed to be 2');
end;

procedure TTestBigCardinal.TestBitOps;
var
  X, Y: BigCardinal;
begin
  { SHR }
  X := BigCardinal.ParseHex('112233445566778899AABBCCDDEEFF');

  Y := X shr 0;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEEFF', 'Expected Y = "112233445566778899AABBCCDDEEFF"');

  Y := X shr 8;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEE', 'Expected Y = "112233445566778899AABBCCDDEE"');

  Y := X shr 12;
  Check(Y.ToHexString = '112233445566778899AABBCCDDE', 'Expected Y = "112233445566778899AABBCCDDE"');

  X := BigCardinal.ParseHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF');
  Y := X shr 1;
  Check(Y.ToHexString = '7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', 'Expected Y = "7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"');

  {SHL}
  X := BigCardinal.ParseHex('112233445566778899AABBCCDDEEFF');

  Y := X shl 0;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEEFF', 'Expected Y = "112233445566778899AABBCCDDEEFF"');

  Y := X shl 8;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEEFF00', 'Expected Y = "112233445566778899AABBCCDDEEFF00"');

  Y := X shl 12;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEEFF000', 'Expected Y = "112233445566778899AABBCCDDEEFF000"');

  X := BigCardinal.ParseHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF');
  Y := X shl 1;
  Check(Y.ToHexString = '1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE', 'Expected Y = "1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE"');

  {XOR}
  X := BigCardinal.ParseHex('112233445566778899AABBCCDDEEFF');
  Y := X xor X;
  Check(Y = 0, 'Expected Y = "0"');

  Y := X xor 0;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEEFF', 'Expected Y = "112233445566778899AABBCCDDEEFF"');

  Y := X xor 1;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEEFE', 'Expected Y = "112233445566778899AABBCCDDEEFE"');

  Y := X xor BigCardinal.ParseHex('002233445566778899AABBCCDDEE00');
  Check(Y.ToHexString = '1100000000000000000000000000FF', 'Expected Y = "1100000000000000000000000000FF"');

  {OR}
  Y := X or X;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEEFF', 'Expected Y = "112233445566778899AABBCCDDEEFF"');

  Y := X or 0;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEEFF', 'Expected Y = "112233445566778899AABBCCDDEEFF"');

  Y := X or 1;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEEFF', 'Expected Y = "112233445566778899AABBCCDDEEFF"');

  Y := X or BigCardinal.ParseHex('FFFF0000000000000000000000FFFF');
  Check(Y.ToHexString = 'FFFF33445566778899AABBCCDDFFFF', 'Expected Y = "FFFF33445566778899AABBCCDDFFFF"');

  {AND}
  Y := X and X;
  Check(Y.ToHexString = '112233445566778899AABBCCDDEEFF', 'Expected Y = "112233445566778899AABBCCDDEEFF"');

  Y := X and 0;
  Check(Y.ToHexString = '0', 'Expected Y = "0"');

  Y := X and 1;
  Check(Y.ToHexString = '1', 'Expected Y = "1"');

  Y := X and BigCardinal.ParseHex('FFFF0000000000000000000000FFFF');
  Check(Y.ToHexString = '11220000000000000000000000EEFF', 'Expected Y = "11220000000000000000000000EEFF"');

  {NOT}
  X := BigCardinal.ParseHex('11111111111111111111111111111111');
  Y := not X;
  Check(Y.ToHexString = 'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE', 'Expected Y = "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"');

  X := BigCardinal.ParseHex('0');
  Y := not X;
  Check(Y.ToHexString = 'FFFFFFFF', 'Expected Y = "FFFFFFFF"');

  X := BigCardinal.ParseHex('FFFFFFFF');
  Y := not X;
  Check(Y.ToHexString = '0', 'Expected Y = "0"');
end;

procedure TTestBigCardinal.TestCompOps;
var
  X, Y, Z, W: BigCardinal;
begin
  TestAllCompOperators(X, 0, true);
  TestAllCompOperators(0, Y, true);
  TestAllCompOperators(Z, W, true);

  TestAllCompOperators(0, 0, true);
  TestAllCompOperators(1, 0, false);

  TestAllCompOperators(2000000, 100, false);
  TestAllCompOperators($FFFFFFFF, $FFFFFFFF, true);

  TestAllCompOperators(
    BigCardinal.Parse('33821903821093821309839210382091830921830291382130928301293821903821309231029382039489'),
    BigCardinal.Parse('33821903821093821309839210382091830921830291382130928301293821903821309231029382039489'),
    true);

  TestAllCompOperators(
    BigCardinal.Parse('44821903821093821309839210382091833123213213382130928301293821903821309231029382039489'),
    BigCardinal.Parse('33821903821093821309839210382091830921830291382130928301293821903821309231029382039489'),
    false);

  TestAllCompOperators(
    BigCardinal.Parse('44821903821093821309839210382091833123213213382130928301293821903821309231029382039489'),
    BigCardinal.Parse('0900940923605360892376489562085658065662000286864823086460236515430846'),
    false);
end;

procedure TTestBigCardinal.TestCreateAndToXXX;
var
  X, Y: BigCardinal;
begin

  { Check un-initialied }
  Check(X.ToByte() = 0, 'ToByte() expected to be 0');
  Check(X.ToWord() = 0, 'ToWord() expected to be 0');
  Check(X.ToCardinal() = 0, 'ToCardinal() expected to be 0');
  Check(X.ToUInt64() = 0, 'ToUInt64() expected to be 0');
  Check(X.ToShortInt() = 0, 'ToShortInt() expected to be 0');
  Check(X.ToSmallInt() = 0, 'ToSmallInt() expected to be 0');
  Check(X.ToInteger() = 0, 'ToInteger() expected to be 0');
  Check(X.ToInt64() = 0, 'ToInt64() expected to be 0');


  { Test initial value }
  X := X * 2;
  Check(X = 0, '(*) X must be zero by default!');

  { Create from Cardinal }
  X := BigCardinal.Create(Cardinal($FFEEBBAA));

  Check(X.ToByte() = $AA, 'ToByte() expected to be $AA');
  Check(X.ToWord() = $BBAA, 'ToWord() expected to be $BBAA');
  Check(X.ToCardinal() = $FFEEBBAA, 'ToCardinal() expected to be $FFEEBBAA');
  Check(X.ToUInt64() = $FFEEBBAA, 'ToUInt64() expected to be $FFEEBBAA');

  { Create from UInt64 }
  X := BigCardinal.Create($11223344FFEEBBAA);

  Check(X.ToByte() = $AA, 'ToByte() expected to be $AA');
  Check(X.ToWord() = $BBAA, 'ToWord() expected to be $BBAA');
  Check(X.ToCardinal() = $FFEEBBAA, 'ToCardinal() expected to be $FFEEBBAA');
  Check(X.ToUInt64() = $11223344FFEEBBAA, 'ToUInt64() expected to be $11223344FFEEBBAA');

  { Create from another BigInt }
  Y := BigCardinal.Create(X);

  Check(X.ToByte() = $AA, 'ToByte() expected to be $AA');
  Check(X.ToWord() = $BBAA, 'ToWord() expected to be $BBAA');
  Check(X.ToCardinal() = $FFEEBBAA, 'ToCardinal() expected to be $FFEEBBAA');
  Check(X.ToUInt64() = $11223344FFEEBBAA, 'ToUInt64() expected to be $11223344FFEEBBAA');

  { Let's raise the size of the Y }
  Y := Y * $100;

  Check(Y.ToByte() = $00, 'ToByte() expected to be $00');
  Check(Y.ToWord() = $AA00, 'ToWord() expected to be $AA00');
  Check(Y.ToCardinal() = $EEBBAA00, 'ToCardinal() expected to be $EEBBAA00');

  Check(Y.ToUInt64() = $223344FFEEBBAA00, 'ToUInt64() expected to be $223344FFEEBBAA00');

  { Other tests }
  X := BigCardinal.Parse('894378473298473984723984732984732984374938473928473842379483263745164725372');
  Y := BigCardinal.Create(X);

  Check(X.ToUInt64() = Y.ToUInt64, 'X.ToUInt64() expected to be equal to Y.ToUInt64()');


  X := BigCardinal.ParseHex('AABBCCDDEEFFAABBCCDDEEFF00112233445566778899');

  { Lets test to chars }
  Check(X.ToAnsiChar() = #$99, 'X.ToAnsiChar() expected to be #$99');
  Check(X.ToWideChar() = #$8899, 'X.ToWideChar() expected to be #$8899');

  { Lets check int types }
  Check(X.ToShortInt() = -103, 'X.ToShortInt() expected to be -103');
  Check(X.ToSmallInt() = -30567, 'X.ToSmallInt() expected to be -30567');
  Check(X.ToInteger() = $66778899, 'X.ToInteger() expected to be $66778899');
  Check(X.ToInt64() = $2233445566778899, 'X.ToInt64() expected to be $2233445566778899');

  { Test create and To from Ints }
  X := BigCardinal.Create(Int64(-2200));
  Check(X.ToInt64() = -2200, 'X.ToInt64() is expected to be -2200');

  X := BigCardinal.Create(Integer(-88088));
  Check(X.ToInteger() = -88088, 'X.ToInteger() is expected to be -88088');

  X := BigCardinal.Create(SmallInt(-8808));
  Check(X.ToSmallInt() = -8808, 'X.ToSmallInt() is expected to be -8808');

  X := BigCardinal.Create(ShortInt(-88));
  Check(X.ToShortInt() = -88, 'X.ToShortInt() is expected to be -88');
end;

procedure TTestBigCardinal.TestDiv2ShrEq;
const
  Iter = 500;
var
  X, Y: BigCardinal;
  I: Integer;
begin
  X := 1;

  { Generate a very big number }
  for I := 1 to Iter - 1 do
    X := X * I;

  { Copy }
  Y := X;

  while (X > 0) and (Y > 0) do
  begin
    X := X div 2;
    Y := Y shr 1;

    Check(X = Y, 'X is supposed to be equal to Y in shl/div combo');
  end;

end;

procedure TTestBigCardinal.TestDivMod;
var
  X, Y, R: BigCardinal;
begin
  X := BigCardinal(12345) * BigCardinal(778881) + BigCardinal(123);

  Y := X.DivMod(778881, R);
  CheckTrue(Y = 12345);
  CheckTrue(R = 123);

  Y := X.DivMod(12345, R);
  CheckTrue(Y = 778881);
  CheckTrue(R = 123);

  X := X - BigCardinal(123);

  Y := X.DivMod(778881, R);
  CheckTrue(Y = 12345);
  CheckTrue(R = 0);

  Y := X.DivMod(12345, R);
  CheckTrue(Y = 778881);
  CheckTrue(R = 0);
end;

procedure TTestBigCardinal.TestExceptions;
{$IFOPT Q+}
var
  B: BigCardinal;
{$ENDIF}
begin
  { Str to Int }
  CheckException(procedure begin
    BigCardinal.Parse('');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.Parse');

  CheckException(procedure begin
    BigCardinal.Parse(' ');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.Parse');

  CheckException(procedure begin
    BigCardinal.Parse('22 ');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.Parse');

  CheckException(procedure begin
    BigCardinal.Parse('x');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.Parse');

  CheckException(procedure begin
    BigCardinal.Parse('-8940823098423');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.Parse');

  CheckException(procedure begin
    BigCardinal.Parse('788 78788');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.Parse');

  { Hex to Int }
  CheckException(procedure begin
    BigCardinal.ParseHex('');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.ParseHex');

  CheckException(procedure begin
    BigCardinal.ParseHex(' ');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.ParseHex');

  CheckException(procedure begin
    BigCardinal.ParseHex('22 ');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.ParseHex');

  CheckException(procedure begin
    BigCardinal.ParseHex('x');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.ParseHex');

  CheckException(procedure begin
    BigCardinal.ParseHex('-ABC32345');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.ParseHex');

  CheckException(procedure begin
    BigCardinal.ParseHex('AAA 55');
  end, EConvertError, 'EConvertError not thrown in BigCardinal.ParseHex');

  {$IFOPT Q+}
  { Subtract }
  CheckException(procedure begin
    BigCardinal.Create(10) - BigCardinal(11);
  end, EOverflow, 'EOverflow not thrown in Subtract operator');
  {$ENDIF}

  { Div }
  CheckException(procedure begin
    BigCardinal.Create(10) div BigCardinal(0);
  end, EDivByZero, 'EDivByZero not thrown in Div operator');

  CheckException(procedure begin
    BigCardinal.Parse('4387492384723984732984723984732948723984') div BigCardinal(0);
  end, EDivByZero, 'EDivByZero not thrown in Div operator');

  { Mod }
  CheckException(procedure begin
    BigCardinal.Create(10) mod BigCardinal(0);
  end, EDivByZero, 'EDivByZero not thrown in Mod operator');

  CheckException(procedure begin
    BigCardinal.Parse('4387492384723984732984723984732948723984') mod BigCardinal(0);
  end, EDivByZero, 'EDivByZero not thrown in Mod operator');

end;

procedure TTestBigCardinal.TestExplicits;
var
  X: BigCardinal;
  V: Variant;
begin
  X := BigCardinal.ParseHex('AABBCCDDEEFFAABBCCDDEEFF00112233445566778899');
  V := X;

  { Standard }
  Check(Byte(X) = $99, 'Byte(X) expected to be $99');
  Check(Word(X) = $8899, 'Word(X) expected to be $8899');
  Check(Cardinal(X) = $66778899, 'Cardinal(X) expected to be $66778899');

  { Char }
  Check(AnsiChar(X) = #$99, 'AnsiChar(X) expected to be #$99');
  Check(WideChar(X) = #$8899, 'AnsiChar(X) expected to be #$8899');

  { Signed standards }
  Check(ShortInt(X) = -103, 'ShortInt(X) expected to be -103');
  Check(SmallInt(X) = -30567, 'SmallInt(X) expected to be -30567');
  Check(Integer(X) = $66778899, 'Integer(X) expected to be $66778899');
  Check(Int64(X) = $2233445566778899, 'Int64(X) expected to be $2233445566778899');

  Check(BigCardinal(V) = X, 'BigCardinal(V) expected to be $AABBCCDDEEFFAABBCCDDEEFF00112233445566778899');
end;

procedure TTestBigCardinal.TestHexToStrAndBack;
var
  X: BigCardinal;
  B: String;
begin
  { Byte size }
  X := BigCardinal.ParseHex('A90');
  B := X.ToHexString;
  Check(B = 'A90', 'Expected B to be "A90"');

  { Word size }
  X := BigCardinal.ParseHex('ABCDE');
  B := X.ToHexString;
  Check(B = 'ABCDE', 'Expected B to be "ABCDE"');

  { Int size }
  X := BigCardinal.ParseHex('AABBFFEB');
  B := X.ToHexString;
  Check(B = 'AABBFFEB', 'Expected B to be "AABBFFEB"');

  { Int64 size }
  X := BigCardinal.ParseHex('FFFE6677FE43');
  B := X.ToHexString;
  Check(B = 'FFFE6677FE43', 'Expected B to be "FFFE6677FE43"');

  { Check big number }
  X := BigCardinal.ParseHex('AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA');
  B := X.ToHexString;
  Check(B = 'AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA', 'Expected B to be "AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA"');

  { Check even bigger number }
  X := BigCardinal.ParseHex('AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA');
  B := X.ToHexString;
  Check(B = 'AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA', 'Expected B to be "AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA"');

  { Check front spaces }
  X := BigCardinal.ParseHex('  12345678901234567890ABCDEF');
  B := X.ToHexString;
  Check(B = '12345678901234567890ABCDEF', 'Expected B to be "12345678901234567890ABCDEF"');

  { Check front spaces }
  X := BigCardinal.ParseHex(' 001234567890ABCDEF');
  B := X.ToHexString;
  Check(B = '1234567890ABCDEF', 'Expected B to be "1234567890ABCDEF"');

  { Check small chars }
  X := BigCardinal.ParseHex('abce90a');
  B := X.ToHexString;
  Check(B = 'ABCE90A', 'Expected B to be "ABCE90A"');
end;

procedure TTestBigCardinal.TestHexToStrAndTryBack;
var
  X: BigCardinal;
  B: String;
begin
  { Byte size }
  CheckTrue(BigCardinal.TryParseHex('A90', X));
  B := X.ToHexString;
  Check(B = 'A90', 'Expected B to be "A90"');

  { Word size }
  CheckTrue(BigCardinal.TryParseHex('ABCDE', X));
  B := X.ToHexString;
  Check(B = 'ABCDE', 'Expected B to be "ABCDE"');

  { Int size }
  CheckTrue(BigCardinal.TryParseHex('AABBFFEB', X));
  B := X.ToHexString;
  Check(B = 'AABBFFEB', 'Expected B to be "AABBFFEB"');

  { Int64 size }
  CheckTrue(BigCardinal.TryParseHex('FFFE6677FE43', X));
  B := X.ToHexString;
  Check(B = 'FFFE6677FE43', 'Expected B to be "FFFE6677FE43"');

  { Check big number }
  CheckTrue(BigCardinal.TryParseHex('AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA', X));
  B := X.ToHexString;
  Check(B = 'AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA', 'Expected B to be "AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA"');

  { Check even bigger number }
  CheckTrue(BigCardinal.TryParseHex('AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA', X));
  B := X.ToHexString;
  Check(B = 'AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA', 'Expected B to be "AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA"');

  { Check front spaces }
  CheckTrue(BigCardinal.TryParseHex('  12345678901234567890ABCDEF', X));
  B := X.ToHexString;
  Check(B = '12345678901234567890ABCDEF', 'Expected B to be "12345678901234567890ABCDEF"');

  { Check front spaces }
  CheckTrue(BigCardinal.TryParseHex(' 001234567890ABCDEF', X));
  B := X.ToHexString;
  Check(B = '1234567890ABCDEF', 'Expected B to be "1234567890ABCDEF"');

  { Check small chars }
  CheckTrue(BigCardinal.TryParseHex('abce90a', X));
  B := X.ToHexString;
  Check(B = 'ABCE90A', 'Expected B to be "ABCE90A"');

  CheckFalse(BigCardinal.TryParseHex('', X));
  CheckFalse(BigCardinal.TryParseHex(' ', X));
  CheckFalse(BigCardinal.TryParseHex('22 ', X));
  CheckFalse(BigCardinal.TryParse('x', X));
  CheckFalse(BigCardinal.TryParseHex('-8940823098423', X));
  CheckFalse(BigCardinal.TryParseHex('788 78788', X));
  CheckFalse(BigCardinal.TryParseHex('ABCDEFG', X));
end;

procedure TTestBigCardinal.TestImplicits;
var
  X: BigCardinal;

begin
  X := Byte(100);
  Check(X = 100, 'X is supposed to be 100');

  X := Word(10000);
  Check(X = 10000, 'X is supposed to be 10000');

  X := Cardinal($10FFBBAA);
  Check(X = $10FFBBAA, 'X is supposed to be $10FFBBAA');

  X := UInt64($BA10FFBBAA);
  Check(X = $BA10FFBBAA, 'X is supposed to be $BA10FFBBAA');
end;

procedure TTestBigCardinal.TestIntToStrAndBack;
var
  X: BigCardinal;
  B: String;
begin
  { Byte size }
  X := BigCardinal.Parse('90');
  B := X.ToString;
  Check(B = '90', 'Expected B to be "90"');

  { Word size }
  X := BigCardinal.Parse('16120');
  B := X.ToString;
  Check(B = '16120', 'Expected B to be "16120"');

  { Int size }
  X := BigCardinal.Parse('88989998');
  B := X.ToString;
  Check(B = '88989998', 'Expected B to be "88989998"');

  { Int64 size }
  X := BigCardinal.Parse('889899989990');
  B := X.ToString;
  Check(B = '889899989990', 'Expected B to be "889899989990"');

  { Check big number }
  X := BigCardinal.Parse('779948200474738991364628209377748291298233');
  B := X.ToString;
  Check(B = '779948200474738991364628209377748291298233', 'Expected B to be "779948200474738991364628209377748291298233"');

  { Check even bigger number }
  X := BigCardinal.Parse('779948472398473000466100971094770921074720917401200474738991364628209377748291298233');
  B := X.ToString;
  Check(B = '779948472398473000466100971094770921074720917401200474738991364628209377748291298233', 'Expected B to be "779948472398473000466100971094770921074720917401200474738991364628209377748291298233"');

  { Check front spaces }
  X := BigCardinal.Parse('  12345678901234567890');
  B := X.ToString;
  Check(B = '12345678901234567890', 'Expected B to be "12345678901234567890"');

  { Check front spaces }
  X := BigCardinal.Parse(' 001234567890');
  B := X.ToString;
  Check(B = '1234567890', 'Expected B to be "1234567890"');
end;

procedure TTestBigCardinal.TestIntToStrHexAndBack;
var
  X: BigCardinal;
  B: String;
begin
  { Byte size }
  X := BigCardinal.Parse('$A90');
  B := X.ToHexString;
  Check(B = 'A90', 'Expected B to be "A90"');

  { Word size }
  X := BigCardinal.Parse('$ABCDE');
  B := X.ToHexString;
  Check(B = 'ABCDE', 'Expected B to be "ABCDE"');

  { Int size }
  X := BigCardinal.Parse('$AABBFFEB');
  B := X.ToHexString;
  Check(B = 'AABBFFEB', 'Expected B to be "AABBFFEB"');

  { Int64 size }
  X := BigCardinal.Parse('$FFFE6677FE43');
  B := X.ToHexString;
  Check(B = 'FFFE6677FE43', 'Expected B to be "FFFE6677FE43"');

  { Check big number }
  X := BigCardinal.Parse('$AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA');
  B := X.ToHexString;
  Check(B = 'AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA', 'Expected B to be "AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA"');

  { Check even bigger number }
  X := BigCardinal.Parse('$AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA');
  B := X.ToHexString;
  Check(B = 'AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA', 'Expected B to be "AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA"');

  { Check front spaces }
  X := BigCardinal.Parse('  $12345678901234567890ABCDEF');
  B := X.ToHexString;
  Check(B = '12345678901234567890ABCDEF', 'Expected B to be "12345678901234567890ABCDEF"');

  { Check front spaces }
  X := BigCardinal.Parse(' $001234567890ABCDEF');
  B := X.ToHexString;
  Check(B = '1234567890ABCDEF', 'Expected B to be "1234567890ABCDEF"');

  { Check small chars }
  X := BigCardinal.Parse('$abce90a');
  B := X.ToHexString;
  Check(B = 'ABCE90A', 'Expected B to be "ABCE90A"');
end;

procedure TTestBigCardinal.TestIntToStrHexAndTryBack;
var
  X: BigCardinal;
  B: String;
begin
  { Byte size }
  CheckTrue(BigCardinal.TryParse('$A90', X));
  B := X.ToHexString;
  Check(B = 'A90', 'Expected B to be "A90"');

  { Word size }
  CheckTrue(BigCardinal.TryParse('$ABCDE', X));
  B := X.ToHexString;
  Check(B = 'ABCDE', 'Expected B to be "ABCDE"');

  { Int size }
  CheckTrue(BigCardinal.TryParse('$AABBFFEB', X));
  B := X.ToHexString;
  Check(B = 'AABBFFEB', 'Expected B to be "AABBFFEB"');

  { Int64 size }
  CheckTrue(BigCardinal.TryParse('$FFFE6677FE43', X));
  B := X.ToHexString;
  Check(B = 'FFFE6677FE43', 'Expected B to be "FFFE6677FE43"');

  { Check big number }
  CheckTrue(BigCardinal.TryParse('$AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA', X));
  B := X.ToHexString;
  Check(B = 'AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA', 'Expected B to be "AB3354892933CFFDEF3362DFAAAC33455C3C55555DDEABA"');

  { Check even bigger number }
  CheckTrue(BigCardinal.TryParse('$AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA', X));
  B := X.ToHexString;
  Check(B = 'AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA', 'Expected B to be "AB33A5489246FDE933CFFDB344EF3362DFACEAAAC33455C3BDEFCC5555522AAAC5DDBEABA"');

  { Check front spaces }
  CheckTrue(BigCardinal.TryParse('  $12345678901234567890ABCDEF', X));
  B := X.ToHexString;
  Check(B = '12345678901234567890ABCDEF', 'Expected B to be "12345678901234567890ABCDEF"');

  { Check front spaces }
  CheckTrue(BigCardinal.TryParse(' $001234567890ABCDEF', X));
  B := X.ToHexString;
  Check(B = '1234567890ABCDEF', 'Expected B to be "1234567890ABCDEF"');

  { Check small chars }
  CheckTrue(BigCardinal.TryParse('$abce90a', X));
  B := X.ToHexString;
  Check(B = 'ABCE90A', 'Expected B to be "ABCE90A"');
end;

procedure TTestBigCardinal.TestIsProps;
var
  X: BigCardinal;
begin
  CheckTrue(X.IsZero, 'x.isZero');
  CheckTrue(X.IsEven, 'x.isEven');
  CheckFalse(X.IsOdd, '!x.isOdd');

  CheckTrue(BigCardinal.Zero.IsZero, 'zero.isZero');
  CheckTrue(BigCardinal.Zero.IsEven, 'zero.isEven');
  CheckFalse(BigCardinal.Zero.IsOdd, '!zero.isOdd');

  CheckFalse(BigCardinal.One.IsZero, '!one.isZero');
  CheckFalse(BigCardinal.One.IsEven, '!one.isEven');
  CheckTrue(BigCardinal.One.IsOdd, 'one.isOdd');

  CheckFalse(BigCardinal.Ten.IsZero, '!ten.isZero');
  CheckTrue(BigCardinal.Ten.IsEven, 'ten.isEven');
  CheckFalse(BigCardinal.Ten.IsOdd, '!ten.isOdd');
end;

procedure TTestBigCardinal.TestMul2ShlEq;
const
  Iter = 500;
var
  X, Y: BigCardinal;
  I: Integer;
begin
  X := 1;
  Y := 1;

  { Generate a very big number }
  for I := 0 to Iter - 1 do
  begin
    X := X * 2;
    Y := Y shl 1;

    Check(X = Y, 'X is supposed to be equal to Y in shr/mul combo');
  end;
end;

procedure TTestBigCardinal.TestPow;
begin
  CheckTrue(BigCardinal.Ten.Pow(0) = 1, '10^0');
  CheckTrue(BigCardinal.Ten.Pow(1) = BigCardinal.Ten, '10^1');
  CheckTrue(BigCardinal.Ten.Pow(2) = BigCardinal.Ten * BigCardinal.Ten, '10^2');

  CheckTrue(BigCardinal.One.Pow(0) = 1, '1^0');
  CheckTrue(BigCardinal.One.Pow(1) = BigCardinal.One, '1^1');
  CheckTrue(BigCardinal.One.Pow(2) = BigCardinal.One, '1^2');

  CheckTrue(BigCardinal.Zero.Pow(0) = 1, '0^0');
  CheckTrue(BigCardinal.Zero.Pow(1) = BigCardinal.Zero, '0^1');
  CheckTrue(BigCardinal.Zero.Pow(2) = BigCardinal.Zero, '0^2');

  CheckTrue(BigCardinal(5).Pow(0) = 1, '5^0');
  CheckTrue(BigCardinal(5).Pow(1) = 5, '5^1');
  CheckTrue(BigCardinal(5).Pow(2) = 25, '5^2');
end;

procedure TTestBigCardinal.TestStatNums;
begin
  { Simple }
  CheckTrue(BigCardinal.Zero = 0, 'zero');
  CheckTrue(BigCardinal.One = 1, 'one');
  CheckTrue(BigCardinal.Ten = 10, 'ten');
end;

procedure TTestBigCardinal.TestIntToStrAndTryBack;
var
  X: BigCardinal;
  B: String;
begin
  { Byte size }
  CheckTrue(BigCardinal.TryParse('90', X));
  B := X.ToString;
  Check(B = '90', 'Expected B to be "90"');

  { Word size }
  CheckTrue(BigCardinal.TryParse('16120', X));
  B := X.ToString;
  Check(B = '16120', 'Expected B to be "16120"');

  { Int size }
  CheckTrue(BigCardinal.TryParse('88989998', X));
  B := X.ToString;
  Check(B = '88989998', 'Expected B to be "88989998"');

  { Int64 size }
  CheckTrue(BigCardinal.TryParse('889899989990', X));
  B := X.ToString;
  Check(B = '889899989990', 'Expected B to be "889899989990"');

  { Check big number }
  CheckTrue(BigCardinal.TryParse('779948200474738991364628209377748291298233', X));
  B := X.ToString;
  Check(B = '779948200474738991364628209377748291298233', 'Expected B to be "779948200474738991364628209377748291298233"');

  { Check even bigger number }
  CheckTrue(BigCardinal.TryParse('779948472398473000466100971094770921074720917401200474738991364628209377748291298233', X));
  B := X.ToString;
  Check(B = '779948472398473000466100971094770921074720917401200474738991364628209377748291298233', 'Expected B to be "779948472398473000466100971094770921074720917401200474738991364628209377748291298233"');

  { Check front spaces }
  CheckTrue(BigCardinal.TryParse('  12345678901234567890', X));
  B := X.ToString;
  Check(B = '12345678901234567890', 'Expected B to be "12345678901234567890"');

  { Check front spaces }
  CheckTrue(BigCardinal.TryParse('  001234567890', X));
  B := X.ToString;
  Check(B = '1234567890', 'Expected B to be "1234567890"');

  CheckFalse(BigCardinal.TryParse('', X));
  CheckFalse(BigCardinal.TryParse(' ', X));
  CheckFalse(BigCardinal.TryParse('22 ', X));
  CheckFalse(BigCardinal.TryParse('x', X));
  CheckFalse(BigCardinal.TryParse('-8940823098423', X));
  CheckFalse(BigCardinal.TryParse('788 78788', X));
end;

procedure TTestBigCardinal.TestVariantSupport;
var
  X, Y: Variant;
  M: Integer;
begin
  { Check conversions }
  X := BigCardinal.Parse('39712903721983712893712893712893718927389217312321893712986487234623785');
  Y := BigCardinal(100);

  Check(X = '39712903721983712893712893712893718927389217312321893712986487234623785', 'Variant value expected to be "39712903721983712893712893712893718927389217312321893712986487234623785"');
  Check(Y = 100, 'Variant value expected to be "100"');

  { Check opeartors a bit }
  X := X + Y;
  Check(X = '39712903721983712893712893712893718927389217312321893712986487234623885', 'Variant value expected to be "39712903721983712893712893712893718927389217312321893712986487234623885"');

  X := X - Y;
  Check(X = '39712903721983712893712893712893718927389217312321893712986487234623785', 'Variant value expected to be "39712903721983712893712893712893718927389217312321893712986487234623785"');

  X := BigCardinal(1);
  Y := X shl 1;
  Check(Y = '2', 'Variant value expected to be "2"');

  X := BigCardinal(8);
  Y := X shr 1;
  Check(Y = 4, 'Variant value expected to be "4"');

  X := BigCardinal(3);
  Y := X and 1;
  Check(Y = 1, 'Variant value expected to be "1"');

  X := BigCardinal(2);
  Y := X or 1;
  Check(Y = 3, 'Variant value expected to be "3"');

  X := BigCardinal(10);
  Y := X div 3;
  Check(Y = 3, 'Variant value expected to be "3"');

  X := BigCardinal(10);
  Y := X mod 3;
  Check(Y = 1, 'Variant value expected to be "1"');

  X := BigCardinal(100);
  Y := X * 3;
  Check(Y = 300, 'Variant value expected to be "300"');

  X := BigCardinal($FF);
  Y := X xor $F;
  Check(Y = $F0, 'Variant value expected to be "$F0"');

  X := BigCardinal(78);

  CheckException(procedure begin
    Y := X / 4;
  end, Exception,
  'Expected an exception!');

  M := X;
  Check(M = 78, 'M''s value expected to be "78"');

  VarClear(X);
  Check(X = 0, 'Variant value expected to be "0"');

  X := BigCardinal(100);
  Y := BigCardinal(200);

  Check(X < Y, 'X Expected to be less than Y');
  Check(Y > X, 'Y Expected to be greater than X');
  Check(Y >= X, 'Y Expected to be greater or equal than X');
  Check(X <= Y, 'X Expected to be less or equal than Y');

  { An now some random computations }
  X := BigCardinal.Parse('389173892731283721890372089371232893721083921738927138912738196437463278463736478');
  X := X - 8;
  Check(X = '389173892731283721890372089371232893721083921738927138912738196437463278463736470', 'X expected to be "389173892731283721890372089371232893721083921738927138912738196437463278463736470"');

  X := -X;
  Check(X = '497322847235893910871660357774731468867562429713425978513325064154963059549254911796586', 'X expected to be "497322847235893910871660357774731468867562429713425978513325064154963059549254911796586"');

  X := BigCardinal.Parse('0');
  X := not X;
  Check(X = '4294967295', 'X expected to be "4294967295"');

  X := BigCardinal.Parse('$FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF');
  X := not X;
  Check(X = '497323206767011797402436219712648677876351740360231643036479582792017990986772187709440', 'X expected to be "497323206767011797402436219712648677876351740360231643036479582792017990986772187709440"');
end;

procedure TTestBigCardinal.Test_Bug_0;
var
  A, B, C: BigCardinal;
begin
  A := BigCardinal.ParseHex('E0000000000000000000000000000000');
  B := BigCardinal.ParseHex('11111111111111111111111111111111');
  C := (A and B) + 1;

  Check(UIntToStr(C) = '1', 'C expected to be equal to "1"');
end;

{ TTestBigInteger }

function TTestBigInteger.FromHex(const AStr: string; const DoNeg: Boolean): BigInteger;
begin
  Result := BigInteger(BigCardinal.ParseHex(AStr));
  if DoNeg then
    Result := - Result;
end;

procedure TTestBigInteger.TestAbs;
var
  X: BigInteger;
begin
  X := BigInteger.Parse('-438764927489274983274398473946278542397432849632784647326487234324342333333');

  X := X.Abs;
  Check(X.ToString = '438764927489274983274398473946278542397432849632784647326487234324342333333');

  X := X.Abs;
  Check(X.ToString = '438764927489274983274398473946278542397432849632784647326487234324342333333');

  X := BigInteger.Zero.Abs;
  Check(X.ToString = '0');
end;

procedure TTestBigInteger.TestAllCompOperatorsAndCompareTo(const X, Y: BigInteger; const IsStrict: Boolean);
var
  AErr: String;
begin
  AErr := ' (X = "' + X.ToString + '"; Y = "' + Y.ToString + '")';

  Check(X = X, 'Expected X = X' + AErr);
  Check(Y = Y, 'Expected Y = Y' + AErr);

  Check(X.CompareTo(X) = 0, 'Expected X.CompareTo(X) = 0' + AErr);
  Check(Y.CompareTo(Y) = 0, 'Expected Y.CompareTo(Y) = 0' + AErr);

  Check(X >= X, 'Expected X >= X' + AErr);
  Check(X <= X, 'Expected X <= X' + AErr);

  Check(Y >= Y, 'Expected Y >= Y' + AErr);
  Check(Y <= Y, 'Expected Y <= Y' + AErr);

  Check(not (X > X), 'Expected not (X > X)' + AErr);
  Check(not (X < X), 'Expected not (X < X)' + AErr);
  Check(not (Y > Y), 'Expected not (Y > Y)' + AErr);
  Check(not (Y < Y), 'Expected not (Y > Y)' + AErr);

  Check(X >= Y, 'Expected X >= Y' + AErr);
  Check(Y <= X, 'Expected Y <= X' + AErr);

  if not IsStrict then
  begin
    Check(X > Y, 'Expected X > Y' + AErr);
    Check(Y < X, 'Expected Y < X' + AErr);

    Check(X.CompareTo(Y) > 0, 'Expected X.CompareTo(Y) > 0' + AErr);
    Check(Y.CompareTo(X) < 0, 'Expected Y.CompareTo(X) < 0' + AErr);

    Check(X <> Y, 'Expected X <> Y' + AErr);
    Check(Y <> X, 'Expected Y <> X' + AErr);

    Check(not (Y = X), 'Expected not (Y = X)' + AErr);
    Check(not (X = Y), 'Expected not (X = Y)' + AErr);
  end else
  begin
    Check(X.CompareTo(Y) = 0, 'Expected X.CompareTo(Y) = 0' + AErr);
    Check(y.CompareTo(X) = 0, 'Expected Y.CompareTo(X) = 0' + AErr);

    Check(not (X > Y), 'Expected not (X > Y)' + AErr);
    Check(not (Y > X), 'Expected not (Y > X)' + AErr);

    Check(not (X < Y), 'Expected not (X > Y)' + AErr);
    Check(not (Y < X), 'Expected not (Y > X)' + AErr);

    Check(Y = X, 'Expected Y = X' + AErr);
    Check(X = Y, 'Expected X = Y' + AErr);

    Check(not (Y <> X), 'Expected not (Y <> X)' + AErr);
    Check(not (X <> Y), 'Expected not (X <> Y)' + AErr);
  end
end;

procedure TTestBigInteger.TestArithmOps_Positive;
var
  X, Y, Z: BigInteger;
begin
  X := BigInteger.Parse('742038403297403256248056320847328947309842374092374392743974023904732904');

  { Subtraction 1 }
  Z := X - 1;
  Check(Z.ToString = '742038403297403256248056320847328947309842374092374392743974023904732903', 'Expected Z = "742038403297403256248056320847328947309842374092374392743974023904732903"');

  { Addition 1 }
  Z := X + 1;
  Check(Z.ToString = '742038403297403256248056320847328947309842374092374392743974023904732905', 'Expected Z = "742038403297403256248056320847328947309842374092374392743974023904732905"');

  { Multiplication 1 }
  Z := X * 1;
  Check(Z.ToString = '742038403297403256248056320847328947309842374092374392743974023904732904', 'Expected Z = "742038403297403256248056320847328947309842374092374392743974023904732904"');

  { Multiplication -1 }
  Z := X * -1;
  Check(Z.ToString = '-742038403297403256248056320847328947309842374092374392743974023904732904', 'Expected Z = "-742038403297403256248056320847328947309842374092374392743974023904732904"');

  { Division 1 }
  Z := X div 1;
  Check(Z.ToString = '742038403297403256248056320847328947309842374092374392743974023904732904', 'Expected Z = "742038403297403256248056320847328947309842374092374392743974023904732904"');

  { Division -1 }
  Z := X div -1;
  Check(Z.ToString = '-742038403297403256248056320847328947309842374092374392743974023904732904', 'Expected Z = "-742038403297403256248056320847328947309842374092374392743974023904732904"');

  { Modulo 1 }
  Z := X mod 1;
  Check(Z.ToString = '0', 'Expected Z = "0"');

  { Modulo -1 }
  Z := X mod -1;
  Check(Z.ToString = '0', 'Expected Z = "0"');

  { ---------------------------------------------------- }

  X := BigInteger.Parse('34662493847238423894629524590275259020753492304930000947329473482347387474');

  { Subtraction 0 }
  Z := X - 0;
  Check(Z.ToString = '34662493847238423894629524590275259020753492304930000947329473482347387474', 'Expected Z = "34662493847238423894629524590275259020753492304930000947329473482347387474"');

  { Addition 0 }
  Z := X + 0;
  Check(Z.ToString = '34662493847238423894629524590275259020753492304930000947329473482347387474', 'Expected Z = "34662493847238423894629524590275259020753492304930000947329473482347387474"');

  { Multiplication 0 }
  Z := X * 0;
  Check(Z.ToString = '0', 'Expected Z = "0"');

  { ---------------------------------------------------- }

  X := BigInteger.Parse('12222222220000000000000000000000000000000000000000000000000000');
  Y := BigInteger.Parse('2222222220000000000000000000000000000000000000000000000000000');

  { Subtraction x }
  Z := X - Y;
  Check(Z.ToString = '10000000000000000000000000000000000000000000000000000000000000', 'Expected Z = "10000000000000000000000000000000000000000000000000000000000000"');

  { Addition x }
  Z := X + Y;
  Check(Z.ToString = '14444444440000000000000000000000000000000000000000000000000000', 'Expected Z = "14444444440000000000000000000000000000000000000000000000000000"');

  { Multiplication 400 }
  Z := X * 400;
  Check(Z.ToString = '4888888888000000000000000000000000000000000000000000000000000000', 'Expected Z = "4888888888000000000000000000000000000000000000000000000000000000"');

  { Multiplication -400 }
  Z := X * -400;
  Check(Z.ToString = '-4888888888000000000000000000000000000000000000000000000000000000', 'Expected Z = "-4888888888000000000000000000000000000000000000000000000000000000"');

  { Division 100000 }
  Z := X div 100000;
  Check(Z.ToString = '122222222200000000000000000000000000000000000000000000000', 'Expected Z = "122222222200000000000000000000000000000000000000000000000"');

  { Division -100000 }
  Z := X div -100000;
  Check(Z.ToString = '-122222222200000000000000000000000000000000000000000000000', 'Expected Z = "-122222222200000000000000000000000000000000000000000000000"');

  { Division 200 }
  Z := X div 200;
  Check(Z.ToString = '61111111100000000000000000000000000000000000000000000000000', 'Expected Z = "61111111100000000000000000000000000000000000000000000000000"');

  { Division 200 }
  Z := X div -200;
  Check(Z.ToString = '-61111111100000000000000000000000000000000000000000000000000', 'Expected Z = "-61111111100000000000000000000000000000000000000000000000000"');

  { --------------------------------- SOME BASICS --------------- }
  X := 10;
  Y := 10;

  Check(X - Y = 0, 'X - Y expected to be 0');
  Check(X + Y = 20, 'X + Y expected to be 20');
  Check(X * Y = 100, 'X * Y expected to be 100');
  Check(X div Y = 1, 'X div Y expected to be 1');
  Check(X mod Y = 0, 'X mod Y expected to be 0');

  { Some other stuff }
  X := 10;
  X := +X;
  Check(X = 10, 'X was expected to be 10');


  X := BigInteger.Parse('734832789423798427394625642736436434634623452367438527598465298562398423');
  Check(X = +X, 'X was expected to be equal to +X');

  { Check Inc, Dec }
  X := BigInteger.Parse('734832789423798427394625642736436434634623452367438527598465298562398423');

  Inc(X);
  Check(X = BigInteger.Parse('734832789423798427394625642736436434634623452367438527598465298562398424'), 'X was expected to be equal to "734832789423798427394625642736436434634623452367438527598465298562398424"');

  Dec(X);
  Check(X = BigInteger.Parse('734832789423798427394625642736436434634623452367438527598465298562398423'), 'X was expected to be equal to "734832789423798427394625642736436434634623452367438527598465298562398423"');

  X := 100;

  Inc(X, 100);
  Check(X = 200, 'X was expected to be 200');

  Dec(X, 50);
  Check(X = 150, 'X was expected to be 150');
end;

procedure TTestBigInteger.TestArithmOps_Negative;
var
  X, Y, Z, R: BigInteger;
begin
  X := BigInteger.Parse('-742038403297403256248056320847328947309842374092374392743974023904732904');

  { Subtraction 1 }
  Z := X - 1;
  Check(Z.ToString = '-742038403297403256248056320847328947309842374092374392743974023904732905', 'Expected Z = "-742038403297403256248056320847328947309842374092374392743974023904732905"');

  { Addition 1 }
  Z := X + 1;
  Check(Z.ToString = '-742038403297403256248056320847328947309842374092374392743974023904732903', 'Expected Z = "-742038403297403256248056320847328947309842374092374392743974023904732903"');

  { Multiplication 1 }
  Z := X * 1;
  Check(Z.ToString = '-742038403297403256248056320847328947309842374092374392743974023904732904', 'Expected Z = "-742038403297403256248056320847328947309842374092374392743974023904732904"');

  { Multiplication -1 }
  Z := X * -1;
  Check(Z.ToString = '742038403297403256248056320847328947309842374092374392743974023904732904', 'Expected Z = "742038403297403256248056320847328947309842374092374392743974023904732904"');

  { Division 1 }
  Z := X div 1;
  Check(Z.ToString = '-742038403297403256248056320847328947309842374092374392743974023904732904', 'Expected Z = "-742038403297403256248056320847328947309842374092374392743974023904732904"');

  { Division -1 }
  Z := X div -1;
  Check(Z.ToString = '742038403297403256248056320847328947309842374092374392743974023904732904', 'Expected Z = "742038403297403256248056320847328947309842374092374392743974023904732904"');

  { Modulo 1 }
  Z := X mod 1;
  Check(Z.ToString = '0', 'Expected Z = "0"');

  { Modulo -1 }
  Z := X mod -1;
  Check(Z.ToString = '0', 'Expected Z = "0"');

  { ---------------------------------------------------- }

  X := BigInteger.Parse('-34662493847238423894629524590275259020753492304930000947329473482347387474');

  { Subtraction 0 }
  Z := X - 0;
  Check(Z.ToString = '-34662493847238423894629524590275259020753492304930000947329473482347387474', 'Expected Z = "-34662493847238423894629524590275259020753492304930000947329473482347387474"');

  { Addition 0 }
  Z := X + 0;
  Check(Z.ToString = '-34662493847238423894629524590275259020753492304930000947329473482347387474', 'Expected Z = "-34662493847238423894629524590275259020753492304930000947329473482347387474"');

  { Multiplication 0 }
  Z := X * 0;
  Check(Z.ToString = '0', 'Expected Z = "0"');

  { ---------------------------------------------------- }

  X := BigInteger.Parse('-12222222220000000000000000000000000000000000000000000000000000');
  Y := BigInteger.Parse('-2222222220000000000000000000000000000000000000000000000000000');

  { Addition x }
  Z := X - Y;
  Check(Z.ToString = '-10000000000000000000000000000000000000000000000000000000000000', 'Expected Z = "-10000000000000000000000000000000000000000000000000000000000000"');

  { Subtraction x }
  Z := X + Y;
  Check(Z.ToString = '-14444444440000000000000000000000000000000000000000000000000000', 'Expected Z = "-14444444440000000000000000000000000000000000000000000000000000"');

  { Multiplication 400 }
  Z := X * 400;
  Check(Z.ToString = '-4888888888000000000000000000000000000000000000000000000000000000', 'Expected Z = "-4888888888000000000000000000000000000000000000000000000000000000"');

  { Division 100000 }
  Z := X div 100000;
  Check(Z.ToString = '-122222222200000000000000000000000000000000000000000000000', 'Expected Z = "-122222222200000000000000000000000000000000000000000000000"');

  { Division -100000 }
  Z := X div -100000;
  Check(Z.ToString = '122222222200000000000000000000000000000000000000000000000', 'Expected Z = "122222222200000000000000000000000000000000000000000000000"');

  { Division 200 }
  Z := X div 200;
  Check(Z.ToString = '-61111111100000000000000000000000000000000000000000000000000', 'Expected Z = "-61111111100000000000000000000000000000000000000000000000000"');

  { Division -200 }
  Z := X div -200;
  Check(Z.ToString = '61111111100000000000000000000000000000000000000000000000000', 'Expected Z = "61111111100000000000000000000000000000000000000000000000000"');

  { --------------------------------- SOME BASICS --------------- }
  X := -10;
  Y := -10;

  Check(X - Y = 0, 'X - Y expected to be 0');
  Check(X + Y = -20, 'X + Y expected to be 20');
  Check(X * Y = 100, 'X * Y expected to be 100');
  Check(X div Y = 1, 'X div Y expected to be 1');
  Check(X mod Y = 0, 'X mod Y expected to be 0');

  { Some other stuff }
  X := -10;
  X := +X;
  Check(X = -10, 'X was expected to be -10');

  X := -10;
  X := -X;
  Check(X = 10, 'X was expected to be 10');


  X := BigInteger.Parse('-734832789423798427394625642736436434634623452367438527598465298562398423');
  Check(X = +X, 'X was expected to be equal to +X');

  { Check Inc, Dec }
  X := BigInteger.Parse('-734832789423798427394625642736436434634623452367438527598465298562398423');

  Inc(X);
  Check(X = BigInteger.Parse('-734832789423798427394625642736436434634623452367438527598465298562398422'), 'X was expected to be equal to "-734832789423798427394625642736436434634623452367438527598465298562398422"');

  Dec(X);
  Check(X = BigInteger.Parse('-734832789423798427394625642736436434634623452367438527598465298562398423'), 'X was expected to be equal to "-734832789423798427394625642736436434634623452367438527598465298562398423"');

  X := -100;

  Inc(X, 100);
  Check(X = 0, 'X was expected to be 0');

  Dec(X, 50);
  Check(X = -50, 'X was expected to be -50');

  { Modulo behavior }
  X := -10;

  Z := X div 3;
  R := X mod 3;
  Check(Z = -3, 'Z was expected to be -3');
  Check(R = -1, 'R was expected to be -1');

  Z := X div -3;
  R := X mod -3;
  Check(Z = 3, 'Z was expected to be 3');
  Check(R = -1, 'R was expected to be -1');

  X := 10;

  Z := X div -3;
  R := X mod -3;
  Check(Z = -3, 'Z was expected to be -3');
  Check(R = 1, 'R was expected to be 1');

  Z := X div 3;
  R := X mod 3;
  Check(Z = 3, 'Z was expected to be 3');
  Check(R = 1, 'R was expected to be 1');
end;

procedure TTestBigInteger.TestBigPow2_Positive;
const
 Iters = 500;

var
  X: BigInteger;
  I: Integer;
begin
  { Let's calculate the a power of 2 }
  X := 2;

  { multiply by 2 on each iteration}
  for I := 0 to Iters - 1 do
    X := X * 2;

  { Divide by 4 this time twice as fast }
  for I := 0 to (Iters div 2) - 1 do
    X := X div 4;

  Check(X = 2, 'X is supposed to be -2');
end;

procedure TTestBigInteger.TestBitOps;
var
  X, Y: BigInteger;
begin
  { SHR }
  X := FromHex('112233445566778899AABBCCDDEEFF', true);

  Y := X shr 0;
  Check(Y = FromHex('112233445566778899AABBCCDDEEFF', true), 'Expected Y = "112233445566778899AABBCCDDEEFF"');

  Y := X shr 8;
  Check(Y = FromHex('112233445566778899AABBCCDDEE', true), 'Expected Y = "112233445566778899AABBCCDDEE"');

  Y := X shr 12;
  Check(Y = FromHex('112233445566778899AABBCCDDE', true), 'Expected Y = "112233445566778899AABBCCDDE"');

  X := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', false);
  Y := X shr 1;
  Check(Y = FromHex('7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', false), 'Expected Y = "7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"');

  {SHL}
  X := FromHex('112233445566778899AABBCCDDEEFF', true);

  Y := X shl 0;
  Check(Y = FromHex('112233445566778899AABBCCDDEEFF', true), 'Expected Y = "112233445566778899AABBCCDDEEFF"');

  Y := X shl 8;
  Check(Y = FromHex('112233445566778899AABBCCDDEEFF00', true), 'Expected Y = "112233445566778899AABBCCDDEEFF00"');

  Y := X shl 12;
  Check(Y = FromHex('112233445566778899AABBCCDDEEFF000', true), 'Expected Y = "112233445566778899AABBCCDDEEFF000"');

  X := FromHex('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', false);
  Y := X shl 1;
  Check(Y = FromHex('1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE', false), 'Expected Y = "1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE"');
end;

procedure TTestBigInteger.TestBigPow2_Negative;
const
 Iters = 500;

var
  X: BigInteger;
  I: Integer;
begin
  { Let's calculate the a power of 2 }
  X := -2;

  { multiply by 2 on each iteration}
  for I := 0 to Iters - 1 do
    X := X * 2;

  { Divide by 4 this time twice as fast }
  for I := 0 to (Iters div 2) - 1 do
    X := X div 4;

  Check(X = -2, 'X is supposed to be -2');
end;

procedure TTestBigInteger.TestCompOps;
var
  X, Y, Z, W: BigInteger;
begin
  { Only positive values }
  TestAllCompOperatorsAndCompareTo(X, 0, true);
  TestAllCompOperatorsAndCompareTo(0, Y, true);
  TestAllCompOperatorsAndCompareTo(Z, W, true);

  TestAllCompOperatorsAndCompareTo(0, 0, true);
  TestAllCompOperatorsAndCompareTo(1, 0, false);

  TestAllCompOperatorsAndCompareTo(2000000, 100, false);
  TestAllCompOperatorsAndCompareTo($FFFFFFFF, $FFFFFFFF, true);

  TestAllCompOperatorsAndCompareTo(
    BigInteger.Parse('33821903821093821309839210382091830921830291382130928301293821903821309231029382039489'),
    BigInteger.Parse('33821903821093821309839210382091830921830291382130928301293821903821309231029382039489'),
    true);

  TestAllCompOperatorsAndCompareTo(
    BigInteger.Parse('44821903821093821309839210382091833123213213382130928301293821903821309231029382039489'),
    BigInteger.Parse('33821903821093821309839210382091830921830291382130928301293821903821309231029382039489'),
    false);

  TestAllCompOperatorsAndCompareTo(
    BigInteger.Parse('44821903821093821309839210382091833123213213382130928301293821903821309231029382039489'),
    BigInteger.Parse('0900940923605360892376489562085658065662000286864823086460236515430846'),
    false);

  { And now intermixed }
  TestAllCompOperatorsAndCompareTo(0, 0, true);
  TestAllCompOperatorsAndCompareTo(0, -1, false);
  TestAllCompOperatorsAndCompareTo(-10, -10, true);
  TestAllCompOperatorsAndCompareTo(-10, -11, false);

  TestAllCompOperatorsAndCompareTo(100, -2000000, false);
  TestAllCompOperatorsAndCompareTo(-$FFFFFFFF, -$FFFFFFFF, true);

  TestAllCompOperatorsAndCompareTo(
    BigInteger.Parse('-33821903821093821309839210382091830921830291382130928301293821903821309231029382039489'),
    BigInteger.Parse('-33821903821093821309839210382091830921830291382130928301293821903821309231029382039489'),
    true);

  TestAllCompOperatorsAndCompareTo(
    BigInteger.Parse('44821903821093821309839210382091833123213213382130928301293821903821309231029382039489'),
    BigInteger.Parse('-33821903821093821309839210382091830921830291382130928301293821903821309231029382039489'),
    false);

  TestAllCompOperatorsAndCompareTo(
    BigInteger.Parse('0900940923605360892376489562085658065662000286864823086460236515430846'),
    BigInteger.Parse('-44821903821093821309839210382091833123213213382130928301293821903821309231029382039489'),
    false);
end;

procedure TTestBigInteger.TestCreateAndToXXX;
var
  X, Y: BigInteger;
begin
  { Check un-initialied }
  Check(X.ToShortInt() = 0, 'ToShortInt() expected to be 0');
  Check(X.ToSmallInt() = 0, 'ToSmallInt() expected to be 0');
  Check(X.ToInteger() = 0, 'ToInteger() expected to be 0');
  Check(X.ToInt64() = 0, 'ToInt64() expected to be 0');

  { Test initial value }
  X := X * 2;
  Check(X = 0, '(*) X must be zero by default!');

  { Other tests }
  X := BigInteger.Parse('-894378473298473984723984732984732984374938473928473842379483263745164725372');
  Y := BigInteger.Create(X);
  Check(X.ToInt64() = Y.ToInt64, 'X.ToUInt64() expected to be equal to Y.ToInt64()');

  X := BigInteger.Parse('-100');

  { Lets check int types }
  Check(X.ToShortInt() = -100, 'X.ToShortInt() expected to be -100');
  Check(X.ToSmallInt() = -100, 'X.ToSmallInt() expected to be -100');
  Check(X.ToInteger() = -100, 'X.ToInteger() expected to be -100');
  Check(X.ToInt64() = -100, 'X.ToInt64() expected to be -100');

  { Test create and To from Ints }
  X := BigInteger.Create(Int64(-2200));
  Check(X.ToInt64() = -2200, 'X.ToInt64() is expected to be -2200');

  X := BigInteger.Create(Integer(-88088));
  Check(X.ToInteger() = -88088, 'X.ToInteger() is expected to be -88088');

  X := BigInteger.Create(SmallInt(-8808));
  Check(X.ToSmallInt() = -8808, 'X.ToSmallInt() is expected to be -8808');

  X := BigInteger.Create(ShortInt(-88));
  Check(X.ToShortInt() = -88, 'X.ToShortInt() is expected to be -88');
end;

procedure TTestBigInteger.TestDivMod;
var
  X, Y, R: BigInteger;
begin
  X := BigInteger(-12345) * BigInteger(778881) - BigInteger(123);

  Y := X.DivMod(778881, R);
  CheckTrue(Y = -12345);
  CheckTrue(R = -123);

  Y := X.DivMod(-12345, R);
  CheckTrue(Y = 778881);
  CheckTrue(R = -123);

  X := X + BigInteger(123);
  Y := X.DivMod(778881, R);
  CheckTrue(Y = -12345);
  CheckTrue(R = 0);

  Y := X.DivMod(12345, R);
  CheckTrue(Y = -778881);
  CheckTrue(R = 0);
end;

procedure TTestBigInteger.TestExceptions;
begin
  { Str to Int }
  CheckException(procedure begin
    BigInteger.Parse('');
  end, EConvertError, 'EConvertError not thrown in BigInteger.Parse');

  CheckException(procedure begin
    BigInteger.Parse(' ');
  end, EConvertError, 'EConvertError not thrown in BigInteger.Parse');

  CheckException(procedure begin
    BigInteger.Parse('22 ');
  end, EConvertError, 'EConvertError not thrown in BigInteger.Parse');

  CheckException(procedure begin
    BigInteger.Parse('x');
  end, EConvertError, 'EConvertError not thrown in BigInteger.Parse');

  CheckException(procedure begin
    BigInteger.Parse(' +-8940823098423');
  end, EConvertError, 'EConvertError not thrown in BigInteger.Parse');

  CheckException(procedure begin
    BigInteger.Parse('788 78788');
  end, EConvertError, 'EConvertError not thrown in BigInteger.Parse');

  { Div }
  CheckException(procedure begin
    BigInteger.Create(10) div BigInteger(0);
  end, EDivByZero, 'EDivByZero not thrown in Div operator');

  CheckException(procedure begin
    BigInteger.Create(-100) div BigInteger(0);
  end, EDivByZero, 'EDivByZero not thrown in Div operator');

  CheckException(procedure begin
    BigInteger.Parse('4387492384723984732984723984732948723984') div BigInteger(0);
  end, EDivByZero, 'EDivByZero not thrown in Div operator');

  CheckException(procedure begin
    BigInteger.Parse('-4387492384723984732984723984732948723984') div BigInteger(0);
  end, EDivByZero, 'EDivByZero not thrown in Div operator');

  { Mod }
  CheckException(procedure begin
    BigInteger.Create(10) mod BigInteger(0);
  end, EDivByZero, 'EDivByZero not thrown in Mod operator');

  CheckException(procedure begin
    BigInteger.Create(-10) mod BigInteger(0);
  end, EDivByZero, 'EDivByZero not thrown in Mod operator');

  CheckException(procedure begin
    BigInteger.Parse('4387492384723984732984723984732948723984') mod BigInteger(0);
  end, EDivByZero, 'EDivByZero not thrown in Mod operator');

  CheckException(procedure begin
    BigInteger.Parse('-4387492384723984732984723984732948723984') mod BigInteger(0);
  end, EDivByZero, 'EDivByZero not thrown in Mod operator');

end;

procedure TTestBigInteger.TestExplicits;
var
  X: BigInteger;
  V: Variant;
begin
  X := -120;

  { Signed standards }
  Check(ShortInt(X) = -120, 'ShortInt(X) expected to be -120');
  Check(SmallInt(X) = -120, 'SmallInt(X) expected to be -120');
  Check(Integer(X) = -120, 'Integer(X) expected to be -120');
  Check(Int64(X) = -120, 'Int64(X) expected to be -120');

  X := BigInteger.Parse('-3721673562173561725321673521736125376215376123123213');
  V := X;

  CheckTrue(BigInteger(V) = X, 'explicit = -3721673562173561725321673521736125376215376123123213');
end;

procedure TTestBigInteger.TestImplicits;
var
  X: BigInteger;

begin
  X := Byte(100);
  Check(X = 100, 'X is supposed to be 100');

  X := Word(10000);
  Check(X = 10000, 'X is supposed to be 10000');

  X := Cardinal($10FFBBAA);
  Check(X = $10FFBBAA, 'X is supposed to be $10FFBBAA');

  X := UInt64($BA10FFBBAA);
  Check(X = $BA10FFBBAA, 'X is supposed to be $BA10FFBBAA');

  X := ShortInt(-100);
  Check(X = -100, 'X is supposed to be -100');

  X := SmallInt(-10000);
  Check(X = -10000, 'X is supposed to be -10000');

  X := Integer(-$10FFBBAA);
  Check(X = -$10FFBBAA, 'X is supposed to be -$10FFBBAA');

  X := Int64(-$BA10FFBBAA);
  Check(X = -$BA10FFBBAA, 'X is supposed to be -$BA10FFBBAA');

  X := BigCardinal.Parse('450923784097235907983474236752309720983492840392483209472308563458');
  Check(X.ToString = '450923784097235907983474236752309720983492840392483209472308563458', 'X is supposed to be "450923784097235907983474236752309720983492840392483209472308563458"');
end;

procedure TTestBigInteger.TestIntToStrAndBack;
var
  X: BigInteger;
  B: String;
begin
  { -- Positives -- }

  { Byte size }
  X := BigInteger.Parse('90');
  B := X.ToString;
  Check(B = '90', 'Expected B to be "90"');

  { Word size }
  X := BigInteger.Parse('16120');
  B := X.ToString;
  Check(B = '16120', 'Expected B to be "16120"');

  { Int size }
  X := BigInteger.Parse('88989998');
  B := X.ToString;
  Check(B = '88989998', 'Expected B to be "88989998"');

  { Int64 size }
  X := BigInteger.Parse('889899989990');
  B := X.ToString;
  Check(B = '889899989990', 'Expected B to be "889899989990"');

  { Check big number }
  X := BigInteger.Parse('779948200474738991364628209377748291298233');
  B := X.ToString;
  Check(B = '779948200474738991364628209377748291298233', 'Expected B to be "779948200474738991364628209377748291298233"');

  { Check even bigger number }
  X := BigInteger.Parse('779948472398473000466100971094770921074720917401200474738991364628209377748291298233');
  B := X.ToString;
  Check(B = '779948472398473000466100971094770921074720917401200474738991364628209377748291298233', 'Expected B to be "779948472398473000466100971094770921074720917401200474738991364628209377748291298233"');

  { Check front spaces }
  X := BigInteger.Parse('  12345678901234567890');
  B := X.ToString;
  Check(B = '12345678901234567890', 'Expected B to be "12345678901234567890"');

  { Check front spaces }
  X := BigInteger.Parse(' 001234567890');
  B := X.ToString;
  Check(B = '1234567890', 'Expected B to be "1234567890"');

  { -- Negatives -- }

  { SmallInt size }
  X := BigInteger.Parse('-90');
  B := X.ToString;
  Check(B = '-90', 'Expected B to be "-90"');

  { ShortInt size }
  X := BigInteger.Parse('-1200');
  B := X.ToString;
  Check(B = '-1200', 'Expected B to be "-1200"');

  { Integer size }
  X := BigInteger.Parse('-88989998');
  B := X.ToString;
  Check(B = '-88989998', 'Expected B to be "-88989998"');

  { Int64 size }
  X := BigInteger.Parse('-889899989990');
  B := X.ToString;
  Check(B = '-889899989990', 'Expected B to be "-889899989990"');

  { Check big number }
  X := BigInteger.Parse('-779948200474738991364628209377748291298233');
  B := X.ToString;
  Check(B = '-779948200474738991364628209377748291298233', 'Expected B to be "-779948200474738991364628209377748291298233"');

  { Check even bigger number }
  X := BigInteger.Parse('-779948472398473000466100971094770921074720917401200474738991364628209377748291298233');
  B := X.ToString;
  Check(B = '-779948472398473000466100971094770921074720917401200474738991364628209377748291298233', 'Expected B to be "-779948472398473000466100971094770921074720917401200474738991364628209377748291298233"');

  { Check front spaces }
  X := BigInteger.Parse('  -12345678901234567890');
  B := X.ToString;
  Check(B = '-12345678901234567890', 'Expected B to be "-12345678901234567890"');

  { Check front spaces }
  X := BigInteger.Parse(' -001234567890');
  B := X.ToString;
  Check(B = '-1234567890', 'Expected B to be "-1234567890"');
end;

procedure TTestBigInteger.TestIntToStrAndTryBack;
var
  X: BigInteger;
  B: String;
begin
  { -- Positives -- }

  { Byte size }
  CheckTrue(BigInteger.TryParse('90', X));
  B := X.ToString;
  Check(B = '90', 'Expected B to be "90"');

  { Word size }
  CheckTrue(BigInteger.TryParse('16120', X));
  B := X.ToString;
  Check(B = '16120', 'Expected B to be "16120"');

  { Int size }
  CheckTrue(BigInteger.TryParse('88989998', X));
  B := X.ToString;
  Check(B = '88989998', 'Expected B to be "88989998"');

  { Int64 size }
  CheckTrue(BigInteger.TryParse('889899989990', X));
  B := X.ToString;
  Check(B = '889899989990', 'Expected B to be "889899989990"');

  { Check big number }
  CheckTrue(BigInteger.TryParse('779948200474738991364628209377748291298233', X));
  B := X.ToString;
  Check(B = '779948200474738991364628209377748291298233', 'Expected B to be "779948200474738991364628209377748291298233"');

  { Check even bigger number }
  CheckTrue(BigInteger.TryParse('779948472398473000466100971094770921074720917401200474738991364628209377748291298233', X));
  B := X.ToString;
  Check(B = '779948472398473000466100971094770921074720917401200474738991364628209377748291298233', 'Expected B to be "779948472398473000466100971094770921074720917401200474738991364628209377748291298233"');

  { Check front spaces }
  CheckTrue(BigInteger.TryParse('  12345678901234567890', X));
  B := X.ToString;
  Check(B = '12345678901234567890', 'Expected B to be "12345678901234567890"');

  { Check front spaces }
  CheckTrue(BigInteger.TryParse(' 001234567890', X));
  B := X.ToString;
  Check(B = '1234567890', 'Expected B to be "1234567890"');

  { -- Negatives -- }

  { SmallInt size }
  CheckTrue(BigInteger.TryParse('-90', X));
  B := X.ToString;
  Check(B = '-90', 'Expected B to be "-90"');

  { ShortInt size }
  CheckTrue(BigInteger.TryParse('-1200', X));
  B := X.ToString;
  Check(B = '-1200', 'Expected B to be "-1200"');

  { Integer size }
  CheckTrue(BigInteger.TryParse('-88989998', X));
  B := X.ToString;
  Check(B = '-88989998', 'Expected B to be "-88989998"');

  { Int64 size }
  CheckTrue(BigInteger.TryParse('-889899989990', X));
  B := X.ToString;
  Check(B = '-889899989990', 'Expected B to be "-889899989990"');

  { Check big number }
  CheckTrue(BigInteger.TryParse('-779948200474738991364628209377748291298233', X));
  B := X.ToString;
  Check(B = '-779948200474738991364628209377748291298233', 'Expected B to be "-779948200474738991364628209377748291298233"');

  { Check even bigger number }
  CheckTrue(BigInteger.TryParse('-779948472398473000466100971094770921074720917401200474738991364628209377748291298233', X));
  B := X.ToString;
  Check(B = '-779948472398473000466100971094770921074720917401200474738991364628209377748291298233', 'Expected B to be "-779948472398473000466100971094770921074720917401200474738991364628209377748291298233"');

  { Check front spaces }
  CheckTrue(BigInteger.TryParse('  -12345678901234567890', X));
  B := X.ToString;
  Check(B = '-12345678901234567890', 'Expected B to be "-12345678901234567890"');

  { Check front spaces }
  CheckTrue(BigInteger.TryParse(' -001234567890', X));
  B := X.ToString;
  Check(B = '-1234567890', 'Expected B to be "-1234567890"');

  CheckFalse(BigInteger.TryParse('', X));
  CheckFalse(BigInteger.TryParse(' ', X));
  CheckFalse(BigInteger.TryParse('22 ', X));
  CheckFalse(BigInteger.TryParse('x', X));
  CheckFalse(BigInteger.TryParse(' +-8940823098423', X));
  CheckFalse(BigInteger.TryParse('788 78788', X));
end;

procedure TTestBigInteger.TestIsProps;
var
  X: BigInteger;
begin
  CheckTrue(X.IsZero, 'x.isZero');
  CheckTrue(X.IsEven, 'x.isEven');
  CheckFalse(X.IsOdd, '!x.isOdd');
  CheckFalse(X.IsNegative, '!x.isNeg');
  CheckTrue(X.IsPositive, 'x.isPos');

  CheckTrue(BigInteger.Zero.IsZero, 'zero.isZero');
  CheckTrue(BigInteger.Zero.IsEven, 'zero.isEven');
  CheckFalse(BigInteger.Zero.IsOdd, '!zero.isOdd');
  CheckFalse(BigInteger.Zero.IsNegative, '!zero.isNeg');
  CheckTrue(BigInteger.Zero.IsPositive, 'zero.isPos');

  CheckFalse(BigInteger.One.IsZero, '!one.isZero');
  CheckFalse(BigInteger.One.IsEven, '!one.isEven');
  CheckTrue(BigInteger.One.IsOdd, 'one.isOdd');
  CheckFalse(BigInteger.One.IsNegative, '!one.isNeg');
  CheckTrue(BigInteger.One.IsPositive, 'one.isPos');

  CheckFalse(BigInteger.Ten.IsZero, '!ten.isZero');
  CheckTrue(BigInteger.Ten.IsEven, 'ten.isEven');
  CheckFalse(BigInteger.Ten.IsOdd, '!ten.isOdd');
  CheckFalse(BigInteger.Ten.IsNegative, '!ten.isNeg');
  CheckTrue(BigInteger.Ten.IsPositive, 'ten.isPos');

  CheckFalse(BigInteger.MinusOne.IsZero, '!one.isZero');
  CheckFalse(BigInteger.MinusOne.IsEven, '!one.isEven');
  CheckTrue(BigInteger.MinusOne.IsOdd, 'one.isOdd');
  CheckTrue(BigInteger.MinusOne.IsNegative, 'one.isNeg');
  CheckFalse(BigInteger.MinusOne.IsPositive, '!one.isPos');

  CheckFalse(BigInteger.MinusTen.IsZero, '!ten.isZero');
  CheckTrue(BigInteger.MinusTen.IsEven, 'ten.isEven');
  CheckFalse(BigInteger.MinusTen.IsOdd, '!ten.isOdd');
  CheckTrue(BigInteger.MinusTen.IsNegative, 'ten.isNeg');
  CheckFalse(BigInteger.MinusTen.IsPositive, '!ten.isPos');
end;

procedure TTestBigInteger.TestPow;
begin
  CheckTrue(BigInteger.Ten.Pow(0) = 1, '10^0');
  CheckTrue(BigInteger.Ten.Pow(1) = 10, '10^1');
  CheckTrue(BigInteger.Ten.Pow(2) = 100, '10^2');

  CheckTrue(BigInteger.MinusTen.Pow(0) = 1, '-10^0');
  CheckTrue(BigInteger.MinusTen.Pow(1) = -10, '-10^1');
  CheckTrue(BigInteger.MinusTen.Pow(2) = 100, '-10^2');
  CheckTrue(BigInteger.MinusTen.Pow(3) = -1000, '-10^3');

  CheckTrue(BigInteger.One.Pow(0) = 1, '1^0');
  CheckTrue(BigInteger.One.Pow(1) = BigInteger.One, '1^1');
  CheckTrue(BigInteger.One.Pow(2) = BigInteger.One, '1^2');

  CheckTrue(BigInteger.MinusOne.Pow(0) = 1, '-1^0');
  CheckTrue(BigInteger.MinusOne.Pow(1) = BigInteger.MinusOne, '-1^1');
  CheckTrue(BigInteger.MinusOne.Pow(2) = BigInteger.One, '-1^2');
  CheckTrue(BigInteger.MinusOne.Pow(3) = BigInteger.MinusOne, '-1^3');

  CheckTrue(BigInteger.Zero.Pow(0) = 1, '0^0');
  CheckTrue(BigInteger.Zero.Pow(1) = BigInteger.Zero, '0^1');
  CheckTrue(BigInteger.Zero.Pow(2) = BigInteger.Zero, '0^2');

  CheckTrue(BigInteger(-5).Pow(0) = 1, '5^0');
  CheckTrue(BigInteger(-5).Pow(1) = -5, '5^1');
  CheckTrue(BigInteger(-5).Pow(2) = 25, '5^2');
end;

procedure TTestBigInteger.TestSign;
var
  X: BigInteger;
begin
  CheckEquals(0, X.Sign);
  CheckEquals(0, BigInteger.Zero.Sign);
  CheckEquals(1, BigInteger.One.Sign);
  CheckEquals(1, BigInteger.Ten.Sign);
  CheckEquals(-1, BigInteger.MinusOne.Sign);
  CheckEquals(-1, BigInteger.MinusTen.Sign);
end;

procedure TTestBigInteger.TestStatNums;
begin
  { Simple }
  CheckTrue(BigInteger.Zero = 0, 'zero');
  CheckTrue(BigInteger.One = 1, 'one');
  CheckTrue(BigInteger.MinusOne = -1, 'min one');
  CheckTrue(BigInteger.Ten = 10, 'ten');
  CheckTrue(BigInteger.MinusTen = -10, 'min ten');
end;

procedure TTestBigInteger.TestVariantSupport;
var
  X, Y: Variant;
  M: Integer;
begin
  { Check conversions }
  X := BigInteger.Parse('-39712903721983712893712893712893718927389217312321893712986487234623785');
  Y := BigInteger(100);

  Check(X = '-39712903721983712893712893712893718927389217312321893712986487234623785', 'Variant value expected to be "-39712903721983712893712893712893718927389217312321893712986487234623785"');
  Check(Y = 100, 'Variant value expected to be "100"');

  { Check opeartors a bit }
  X := X + Y;
  Check(X = '-39712903721983712893712893712893718927389217312321893712986487234623685', 'Variant value expected to be "-39712903721983712893712893712893718927389217312321893712986487234623685"');

  X := X - Y;
  Check(X = '-39712903721983712893712893712893718927389217312321893712986487234623785', 'Variant value expected to be "-39712903721983712893712893712893718927389217312321893712986487234623785"');

  X := BigInteger(10);
  Y := X div -3;
  Check(Y = -3, 'Variant value expected to be "-3"');

  X := BigInteger(10);
  Y := X mod 3;
  Check(Y = 1, 'Variant value expected to be "1"');

  X := BigInteger(-100);
  Y := X * 3;
  Check(Y = -300, 'Variant value expected to be "-300"');

  X := BigInteger(-78);

  CheckException(procedure begin
    Y := X / 4;
  end, Exception,
  'Expected an exception!');

  M := X;
  Check(M = -78, 'M''s value expected to be "-78"');

  X := -X;
  Check(X = 78, 'Variant value expected to be "78"');

  VarClear(X);
  Check(X = 0, 'Variant value expected to be "0"');

  X := BigInteger(-100);
  Y := BigInteger(-20);

  Check(X < Y, 'X Expected to be less than Y');
  Check(Y > X, 'Y Expected to be greater than X');
  Check(Y >= X, 'Y Expected to be greater or equal than X');
  Check(X <= Y, 'X Expected to be less or equal than Y');

  { An now some random computations }
  X := '389173892731283721890372089371232893721083921738927138912738196437463278463736478';
  X := X - 8;
  Check(X = '389173892731283721890372089371232893721083921738927138912738196437463278463736470', 'X expected to be "389173892731283721890372089371232893721083921738927138912738196437463278463736470"');
end;

{ TTestBigDecimal }

procedure TTestBigDecimal.SetUp;
begin
  inherited;

  FOldDec := FormatSettings.DecimalSeparator;
  FOldTh := FormatSettings.ThousandSeparator;

  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := ',';
end;

procedure TTestBigDecimal.TearDown;
begin
  inherited;

  FormatSettings.DecimalSeparator := FOldDec;
  FormatSettings.ThousandSeparator := FOldTh;
end;

procedure TTestBigDecimal.TestOp(const X, Y: BigDecimal; const AComp: NativeInt);
begin
  { Actual comparison }
  CheckTrue(X.CompareTo(Y) = AComp);
  CheckTrue(Y.CompareTo(X) = -AComp);

  { Equality }
  CheckEquals(AComp = 0, X = Y);
  CheckEquals(AComp = 0, Y = X);

  { Inequality }
  CheckEquals(AComp <> 0, X <> Y);
  CheckEquals(AComp <> 0, Y <> X);

  { Greater than }
  CheckEquals(AComp > 0, X > Y);
  CheckEquals(AComp > 0, Y < X);

  { Lower than }
  CheckEquals(AComp < 0, X < Y);
  CheckEquals(AComp < 0, Y > X);

  { Greater than or Equal }
  CheckEquals(AComp >= 0, X >= Y);
  CheckEquals(AComp >= 0, Y <= X);

  { Lower than or Equal }
  CheckEquals(AComp <= 0, X <= Y);
  CheckEquals(AComp <= 0, Y >= X);
end;

procedure TTestBigDecimal.Test_Conformance_1;
var
  D: BigDecimal;
begin
  D := 0.1950;
  D := D.Rescale(2, rmHalfDown);
  CheckEquals('0.20', D.ToString(false));

  D := BigDecimal.Parse('0.1950');
  D := D.Rescale(2, rmHalfDown);
  CheckEquals('0.19', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Conformance_2;
var
  D, txR, V, R: BigDecimal;
begin
  { The extension of scale and precision }
  D := BigDecimal.Parse('1115.32');
  txR := BigDecimal.Parse('0.0049');
  R := D * txR;
  CheckEquals('5.465068', R.ToString(false));

  { Rescaling }
  R := R.Rescale(2, rmHalfUp);
  CheckEquals('5.47', R.ToString(false));

  { Proper division }
  txR := BigDecimal.Parse('30');
  R := D.Divide(txR, 2, rmHalfUp);
  CheckEquals('37.18', R.ToString(false));

  D := BigDecimal.Parse('9500.00');
  txR := BigDecimal.Parse('0.067');
  V := BigDecimal.Parse('0.25');
  R := (D * txR) * V;
  CheckEquals('159.1250000', R.ToString(false));

  R := R.Rescale(2, rmDown);
  CheckEquals('159.12', R.ToString(false));
end;

procedure TTestBigDecimal.Test_Conformance_3;
var
  X, rUp, rDown, rCeiling,
    rFloor, rHalfUp, rHalfDown,
      rHalfEven, rNone: BigDecimal;
begin
  { 5.5 }
  X := BigDecimal.Parse('5.5');
  rUp := X.Round(1, rmUp);
  rDown := X.Round(1, rmDown);
  rCeiling := X.Round(1, rmCeiling);
  rFloor := X.Round(1, rmFloor);
  rHalfUp := X.Round(1, rmHalfUp);
  rHalfDown := X.Round(1, rmHalfDown);
  rHalfEven := X.Round(1, rmHalfEven);

  CheckException(procedure begin
    rNone := X.Round(1, rmNone);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckEquals('6', rUp.ToString(false));
  CheckEquals('5', rDown.ToString(false));
  CheckEquals('6', rCeiling.ToString(false));
  CheckEquals('5', rFloor.ToString(false));
  CheckEquals('6', rHalfUp.ToString(false));
  CheckEquals('5', rHalfDown.ToString(false));
  CheckEquals('6', rHalfEven.ToString(false));

  { 2.5 }
  X := BigDecimal.Parse('2.5');
  rUp := X.Round(1, rmUp);
  rDown := X.Round(1, rmDown);
  rCeiling := X.Round(1, rmCeiling);
  rFloor := X.Round(1, rmFloor);
  rHalfUp := X.Round(1, rmHalfUp);
  rHalfDown := X.Round(1, rmHalfDown);
  rHalfEven := X.Round(1, rmHalfEven);

  CheckException(procedure begin
    rNone := X.Round(1, rmNone);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckEquals('3', rUp.ToString(false));
  CheckEquals('2', rDown.ToString(false));
  CheckEquals('3', rCeiling.ToString(false));
  CheckEquals('2', rFloor.ToString(false));
  CheckEquals('3', rHalfUp.ToString(false));
  CheckEquals('2', rHalfDown.ToString(false));
  CheckEquals('2', rHalfEven.ToString(false));

  { 1.6 }
  X := BigDecimal.Parse('1.6');
  rUp := X.Round(1, rmUp);
  rDown := X.Round(1, rmDown);
  rCeiling := X.Round(1, rmCeiling);
  rFloor := X.Round(1, rmFloor);
  rHalfUp := X.Round(1, rmHalfUp);
  rHalfDown := X.Round(1, rmHalfDown);
  rHalfEven := X.Round(1, rmHalfEven);

  CheckException(procedure begin
    rNone := X.Round(1, rmNone);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckEquals('2', rUp.ToString(false));
  CheckEquals('1', rDown.ToString(false));
  CheckEquals('2', rCeiling.ToString(false));
  CheckEquals('1', rFloor.ToString(false));
  CheckEquals('2', rHalfUp.ToString(false));
  CheckEquals('2', rHalfDown.ToString(false));
  CheckEquals('2', rHalfEven.ToString(false));

  { 1.1 }
  X := BigDecimal.Parse('1.1');
  rUp := X.Round(1, rmUp);
  rDown := X.Round(1, rmDown);
  rCeiling := X.Round(1, rmCeiling);
  rFloor := X.Round(1, rmFloor);
  rHalfUp := X.Round(1, rmHalfUp);
  rHalfDown := X.Round(1, rmHalfDown);
  rHalfEven := X.Round(1, rmHalfEven);

  CheckException(procedure begin
    rNone := X.Round(1, rmNone);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckEquals('2', rUp.ToString(false));
  CheckEquals('1', rDown.ToString(false));
  CheckEquals('2', rCeiling.ToString(false));
  CheckEquals('1', rFloor.ToString(false));
  CheckEquals('1', rHalfUp.ToString(false));
  CheckEquals('1', rHalfDown.ToString(false));
  CheckEquals('1', rHalfEven.ToString(false));

  { 1.0 }
  X := BigDecimal.Parse('1.0');
  rUp := X.Round(1, rmUp);
  rDown := X.Round(1, rmDown);
  rCeiling := X.Round(1, rmCeiling);
  rFloor := X.Round(1, rmFloor);
  rHalfUp := X.Round(1, rmHalfUp);
  rHalfDown := X.Round(1, rmHalfDown);
  rHalfEven := X.Round(1, rmHalfEven);
  rNone := X.Round(1, rmNone);

  CheckEquals('1', rUp.ToString(false));
  CheckEquals('1', rDown.ToString(false));
  CheckEquals('1', rCeiling.ToString(false));
  CheckEquals('1', rFloor.ToString(false));
  CheckEquals('1', rHalfUp.ToString(false));
  CheckEquals('1', rHalfDown.ToString(false));
  CheckEquals('1', rHalfEven.ToString(false));
  CheckEquals('1', rNone.ToString(false));

  { -1.0 }
  X := BigDecimal.Parse('-1.0');
  rUp := X.Round(1, rmUp);
  rDown := X.Round(1, rmDown);
  rCeiling := X.Round(1, rmCeiling);
  rFloor := X.Round(1, rmFloor);
  rHalfUp := X.Round(1, rmHalfUp);
  rHalfDown := X.Round(1, rmHalfDown);
  rHalfEven := X.Round(1, rmHalfEven);
  rNone := X.Round(1, rmNone);

  CheckEquals('-1', rUp.ToString(false));
  CheckEquals('-1', rDown.ToString(false));
  CheckEquals('-1', rCeiling.ToString(false));
  CheckEquals('-1', rFloor.ToString(false));
  CheckEquals('-1', rHalfUp.ToString(false));
  CheckEquals('-1', rHalfDown.ToString(false));
  CheckEquals('-1', rHalfEven.ToString(false));
  CheckEquals('-1', rNone.ToString(false));

  { -1.1 }
  X := BigDecimal.Parse('-1.1');
  rUp := X.Round(1, rmUp);
  rDown := X.Round(1, rmDown);
  rCeiling := X.Round(1, rmCeiling);
  rFloor := X.Round(1, rmFloor);
  rHalfUp := X.Round(1, rmHalfUp);
  rHalfDown := X.Round(1, rmHalfDown);
  rHalfEven := X.Round(1, rmHalfEven);

  CheckException(procedure begin
    rNone := X.Round(1, rmNone);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckEquals('-2', rUp.ToString(false));
  CheckEquals('-1', rDown.ToString(false));
  CheckEquals('-1', rCeiling.ToString(false));
  CheckEquals('-2', rFloor.ToString(false));
  CheckEquals('-1', rHalfUp.ToString(false));
  CheckEquals('-1', rHalfDown.ToString(false));
  CheckEquals('-1', rHalfEven.ToString(false));

  { -1.6 }
  X := BigDecimal.Parse('-1.6');
  rUp := X.Round(1, rmUp);
  rDown := X.Round(1, rmDown);
  rCeiling := X.Round(1, rmCeiling);
  rFloor := X.Round(1, rmFloor);
  rHalfUp := X.Round(1, rmHalfUp);
  rHalfDown := X.Round(1, rmHalfDown);
  rHalfEven := X.Round(1, rmHalfEven);

  CheckException(procedure begin
    rNone := X.Round(1, rmNone);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckEquals('-2', rUp.ToString(false));
  CheckEquals('-1', rDown.ToString(false));
  CheckEquals('-1', rCeiling.ToString(false));
  CheckEquals('-2', rFloor.ToString(false));
  CheckEquals('-2', rHalfUp.ToString(false));
  CheckEquals('-2', rHalfDown.ToString(false));
  CheckEquals('-2', rHalfEven.ToString(false));

  { -2.5 }
  X := BigDecimal.Parse('-2.5');
  rUp := X.Round(1, rmUp);
  rDown := X.Round(1, rmDown);
  rCeiling := X.Round(1, rmCeiling);
  rFloor := X.Round(1, rmFloor);
  rHalfUp := X.Round(1, rmHalfUp);
  rHalfDown := X.Round(1, rmHalfDown);
  rHalfEven := X.Round(1, rmHalfEven);

  CheckException(procedure begin
    rNone := X.Round(1, rmNone);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckEquals('-3', rUp.ToString(false));
  CheckEquals('-2', rDown.ToString(false));
  CheckEquals('-2', rCeiling.ToString(false));
  CheckEquals('-3', rFloor.ToString(false));
  CheckEquals('-3', rHalfUp.ToString(false));
  CheckEquals('-2', rHalfDown.ToString(false));
  CheckEquals('-2', rHalfEven.ToString(false));

  { -5.5 }
  X := BigDecimal.Parse('-5.5');
  rUp := X.Round(1, rmUp);
  rDown := X.Round(1, rmDown);
  rCeiling := X.Round(1, rmCeiling);
  rFloor := X.Round(1, rmFloor);
  rHalfUp := X.Round(1, rmHalfUp);
  rHalfDown := X.Round(1, rmHalfDown);
  rHalfEven := X.Round(1, rmHalfEven);

  CheckException(procedure begin
    rNone := X.Round(1, rmNone);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckEquals('-6', rUp.ToString(false));
  CheckEquals('-5', rDown.ToString(false));
  CheckEquals('-5', rCeiling.ToString(false));
  CheckEquals('-6', rFloor.ToString(false));
  CheckEquals('-6', rHalfUp.ToString(false));
  CheckEquals('-5', rHalfDown.ToString(false));
  CheckEquals('-6', rHalfEven.ToString(false));
end;


procedure TTestBigDecimal.Test_Conformance_4;
var
  S, PA, PPS, SP, AV: BigDecimal;
begin
  { Some monetary stuff I found laying on the web }
  S := BigDecimal.Parse('754.495');
  PA := BigDecimal.Parse('200.00');
  PPS := BigDecimal.Parse('10.38');
  SP := PA.Divide(PPS, 3, rmHalfUp);
  S := S + SP;
  AV := S * PPS;
  AV := AV.Round(7, rmHalfEven);

  CheckEquals('773.763', S.ToString(false));
  CheckEquals('8031.660', AV.ToString(false));
end;

procedure TTestBigDecimal.Test_Conformance_5;
var
  A, DP, D, T, TP, TX, TT: BigDecimal;
begin
  { Another accounting example found on teh webz }
  A := BigDecimal.Parse('100.05');
  DP := BigDecimal.Parse('0.10');
  D := A * DP;
  D := D.Rescale(2, rmHalfUp);

  T := A - D;
  T := T.Rescale(2, rmHalfUp);
  TP := BigDecimal.Parse('0.05');
  TX := T * TP;
  TX := TX.Rescale(2, rmHalfUp);

  TT := T + TX;
  TT := TT.Rescale(2, rmHalfUp);

  CheckEquals('100.05', A.ToString(false));
  CheckEquals('10.01', D.ToString(false));
  CheckEquals('90.04', T.ToString(false));
  CheckEquals('4.50', TX.ToString(false));
  CheckEquals('94.54', TT.ToString(false));
end;

procedure TTestBigDecimal.Test_Abs;
var
  X: BigDecimal;
begin
  X := 0;
  CheckEquals(X.ToString, X.Abs().ToString);

  X := -10000;
  CheckEquals((-X).ToString, X.Abs().ToString);

  X := BigDecimal.Parse('-38912638721637812638721637821637862178361278.78351276356');
  CheckEquals((-X).ToString, X.Abs().ToString);

  X := BigDecimal.Parse('38912638721637812638721637821637862178361278.78351276356');
  CheckEquals(X.ToString, X.Abs().ToString);
end;

procedure TTestBigDecimal.Test_CompareTo_And_Ops;
begin
  TestOp(0, 0, 0);
  TestOp(1, 0, 1);
  TestOp(-1, 0, -1);

  TestOp(3.14, 3.14, 0);

  TestOp(
    BigDecimal.Parse('3617253762153716235216735123761233123213.99'),
    BigDecimal.Parse('3617253762153716235216735123761233123214.00'),
    -1
  );

  TestOp(
    BigDecimal.Parse('100'),
    BigDecimal.Parse('100.0000000000'),
    0
  );

  TestOp(
    BigDecimal.Create(1, -2),
    BigDecimal.Create(100),
    0
  );

  TestOp(
    BigDecimal.Create(1, 2),
    BigDecimal.Parse('0.01'),
    0
  );
end;


procedure TTestBigDecimal.Test_Create_BigCardinal_Scale;
var
  D: BigDecimal;
begin
  D := BigDecimal.Create(BigCardinal(0));
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Create(BigCardinal(0), 10);
  CheckEquals('0.0000000000', D.ToString(false));

  D := BigDecimal.Create(BigCardinal(1));
  CheckEquals('1', D.ToString(false));

  D := BigDecimal.Create(BigCardinal(1), 10);
  CheckEquals('0.0000000001', D.ToString(false));

  D := BigDecimal.Create(BigCardinal(1), -10);
  CheckEquals('10000000000', D.ToString(false));

  D := BigDecimal.Create(BigCardinal(550), 2);
  CheckEquals('5.50', D.ToString(false));

  D := BigDecimal.Create(BigCardinal.Parse('378126357812632178637821632138726138721'), 2);
  CheckEquals('3781263578126321786378216321387261387.21', D.ToString(false));

  D := BigDecimal.Create(BigCardinal.Parse('378126357812632178637821632138726138721'));
  CheckEquals('378126357812632178637821632138726138721', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Create_BigInteger_Scale;
var
  D: BigDecimal;
begin
  D := BigDecimal.Create(BigInteger(0));
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Create(BigInteger(0), 10);
  CheckEquals('0.0000000000', D.ToString(false));

  D := BigDecimal.Create(BigInteger(1));
  CheckEquals('1', D.ToString(false));

  D := BigDecimal.Create(BigInteger(1), 10);
  CheckEquals('0.0000000001', D.ToString(false));

  D := BigDecimal.Create(BigInteger(1), -10);
  CheckEquals('10000000000', D.ToString(false));

  D := BigDecimal.Create(BigInteger(-550), 2);
  CheckEquals('-5.50', D.ToString(false));

  D := BigDecimal.Create(BigInteger.Parse('-378126357812632178637821632138726138721'), 2);
  CheckEquals('-3781263578126321786378216321387261387.21', D.ToString(false));

  D := BigDecimal.Create(BigInteger.Parse('-378126357812632178637821632138726138721'));
  CheckEquals('-378126357812632178637821632138726138721', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Create_Cardinal_Scale;
var
  D: BigDecimal;
begin
  D := BigDecimal.Create(Cardinal(0));
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Create(Cardinal(0), 10);
  CheckEquals('0.0000000000', D.ToString(false));

  D := BigDecimal.Create(Cardinal(1));
  CheckEquals('1', D.ToString(false));

  D := BigDecimal.Create(Cardinal(1), 10);
  CheckEquals('0.0000000001', D.ToString(false));

  D := BigDecimal.Create(Cardinal(1), -10);
  CheckEquals('10000000000', D.ToString(false));

  D := BigDecimal.Create(Cardinal(550), 2);
  CheckEquals('5.50', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Create_Double;
var
  D: BigDecimal;
begin
  D := BigDecimal.Create(1.0);
  CheckEquals('1', D.ToString(false));

  D := BigDecimal.Create(100.0);
  CheckEquals('100', D.ToString(false));

  D := BigDecimal.Create(-0.1);
  CheckEquals('-0.1000000000000000055511151231257827021181583404541015625', D.ToString(false));

  D := BigDecimal.Create(-66.11);
  CheckEquals('-66.1099999999999994315658113919198513031005859375', D.ToString(false));

  CheckException(procedure begin
    D := BigDecimal.Create(NaN);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckException(procedure begin
    D := BigDecimal.Create(Infinity);
  end, EInvalidOp, 'Expected an EInvalidOp!');
end;

procedure TTestBigDecimal.Test_Create_Int64_Scale;
var
  D: BigDecimal;
begin
  D := BigDecimal.Create(Int64(0));
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Create(Int64(0), 10);
  CheckEquals('0.0000000000', D.ToString(false));

  D := BigDecimal.Create(Int64(1));
  CheckEquals('1', D.ToString(false));

  D := BigDecimal.Create(Int64(1), 10);
  CheckEquals('0.0000000001', D.ToString(false));

  D := BigDecimal.Create(Int64(1), -10);
  CheckEquals('10000000000', D.ToString(false));

  D := BigDecimal.Create(Int64(-550), 2);
  CheckEquals('-5.50', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Create_Integer_Scale;
var
  D: BigDecimal;
begin
  D := BigDecimal.Create(Integer(0));
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Create(Integer(0), 10);
  CheckEquals('0.0000000000', D.ToString(false));

  D := BigDecimal.Create(Integer(1));
  CheckEquals('1', D.ToString(false));

  D := BigDecimal.Create(Integer(1), 10);
  CheckEquals('0.0000000001', D.ToString(false));

  D := BigDecimal.Create(Integer(1), -10);
  CheckEquals('10000000000', D.ToString(false));

  D := BigDecimal.Create(Integer(-550), 2);
  CheckEquals('-5.50', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Create_UInt64_Scale;
var
  D: BigDecimal;
begin
  D := BigDecimal.Create(UInt64(0));
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Create(UInt64(0), 10);
  CheckEquals('0.0000000000', D.ToString(false));

  D := BigDecimal.Create(UInt64(1));
  CheckEquals('1', D.ToString(false));

  D := BigDecimal.Create(UInt64(1), 10);
  CheckEquals('0.0000000001', D.ToString(false));

  D := BigDecimal.Create(UInt64(1), -10);
  CheckEquals('10000000000', D.ToString(false));

  D := BigDecimal.Create(UInt64(550), 2);
  CheckEquals('5.50', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Divide;
var
  X, Y, Z: BigDecimal;
begin
  X := 0;
  Y := 1;
  Z := X.Divide(Y);
  CheckEquals('0', Z.ToString);

  X := 5;
  Y := 5;
  Z := X.Divide(Y);
  CheckEquals('1', Z.ToString);

  X := 10;
  Y := 2;
  Z := X.Divide(Y);
  CheckEquals('5', Z.ToString);

  X := BigDecimal.Parse('-12.50');
  Y := 10;
  Z := X.Divide(Y);
  CheckEquals('-1.25', Z.ToString);

  { Exceptional cases }
  CheckException(procedure begin
    X := 100;
    Y := 3;
    Z := X.Divide(Y);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckException(procedure begin
    X := 1;
    Y := 2;
    Z := X.Divide(Y);
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckException(procedure begin
    X := 100;
    Y := 0;
    Z := X.Divide(Y);
  end, EDivByZero, 'Expected an EDivByZero!');

  { More complex tests }
  X := -100;
  Y := 3;
  Z := X.Divide(Y, rmHalfEven);
  CheckEquals('-33', Z.ToString);

  Z := X.Divide(Y, 2, rmHalfEven);
  CheckEquals('-33.33', Z.ToString);

  Z := X.Divide(Y, 2, rmHalfUp);
  CheckEquals('-33.33', Z.ToString);

  Z := X.Divide(-Y, 1, rmHalfDown);
  CheckEquals('33.3', Z.ToString);

  Z := X.Divide(Y, 2, rmUp);
  CheckEquals('-33.34', Z.ToString);

  Z := X.Divide(Y, rmUp);
  CheckEquals('-34', Z.ToString);
end;

procedure TTestBigDecimal.Test_Explicit_From_Variant;
var
  D, F: BigDecimal;
  V: Variant;
begin
  D := BigDecimal.Parse('100');
  V := D;
  F := BigDecimal(V);
  CheckTrue(F = D);

  D := BigDecimal.Parse('5666213812379812739128731928371928378216626723123.88');
  V := D;
  F := BigDecimal(V);
  CheckTrue(F = D);

  V := 100;
  F := BigDecimal(V);
  CheckTrue(F = 100);
end;

procedure TTestBigDecimal.Test_Explicit_To_Double;
var
  D: BigDecimal;
  E: Double;
begin
  D := 100;
  E := Double(D);
  CheckTrue(SameValue(E, 100));

  D := -0.15;
  E := Double(D);
  CheckTrue(SameValue(E, -0.15));
end;

procedure TTestBigDecimal.Test_Explicit_To_Extended;
var
  D: BigDecimal;
  E: Extended;
begin
  D := 100;
  E := Extended(D);
  CheckTrue(SameValue(E, 100));

  D := -0.15;
  E := Extended(D);
  CheckTrue(SameValue(E, -0.15));
end;

procedure TTestBigDecimal.Test_Implicit_From_BigCardinal;
var
  B: BigCardinal;
  D: BigDecimal;
begin
  B := 0;
  D := B;
  CheckEquals('0', D.ToString);

  B := 100;
  D := B;
  CheckEquals('100', D.ToString);

  B := BigCardinal.Parse('378612783612873612873682613786127382163781236872136127836218736');
  D := B;
  CheckEquals('378612783612873612873682613786127382163781236872136127836218736', D.ToString);
end;

procedure TTestBigDecimal.Test_Implicit_From_BigInteger;
var
  B: BigInteger;
  D: BigDecimal;
begin
  B := 0;
  D := B;
  CheckEquals('0', D.ToString);

  B := -100;
  D := B;
  CheckEquals('-100', D.ToString);

  B := BigInteger.Parse('-378612783612873612873682613786127382163781236872136127836218736');
  D := B;
  CheckEquals('-378612783612873612873682613786127382163781236872136127836218736', D.ToString);
end;

procedure TTestBigDecimal.Test_Implicit_From_Cardinal;
var
  D: BigDecimal;
begin
  D := Cardinal(0);
  CheckEquals('0', D.ToString(false));

  D := Cardinal(1);
  CheckEquals('1', D.ToString(false));

  D := Cardinal(2221212121);
  CheckEquals('2221212121', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Implicit_From_Double;
var
  D: BigDecimal;
begin
  CheckException(procedure begin
    D := NaN;
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckException(procedure begin
    D := Infinity;
  end, EInvalidOp, 'Expected an EInvalidOp!');

  D := 1.0;
  CheckEquals('1', D.ToString(false));

  D := 100.0;
  CheckEquals('100', D.ToString(false));

  D := -0.1;
  CheckEquals('-0.1000000000000000055511151231257827021181583404541015625', D.ToString(false));

  D := -66.11;
  CheckEquals('-66.1099999999999994315658113919198513031005859375', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Implicit_From_In64;
var
  D: BigDecimal;
begin
  D := Int64(0);
  CheckEquals('0', D.ToString(false));

  D := Int64(1);
  CheckEquals('1', D.ToString(false));

  D := Int64(-67627816222221212);
  CheckEquals('-67627816222221212', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Implicit_From_Integer;
var
  D: BigDecimal;
begin
  D := Integer(0);
  CheckEquals('0', D.ToString(false));

  D := Integer(1);
  CheckEquals('1', D.ToString(false));

  D := Integer(-222121212);
  CheckEquals('-222121212', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Implicit_From_UInt64;
var
  D: BigDecimal;
begin
  D := UInt64(0);
  CheckEquals('0', D.ToString(false));

  D := UInt64(1);
  CheckEquals('1', D.ToString(false));

  D := UInt64(67627816222221212);
  CheckEquals('67627816222221212', D.ToString(false));
end;

procedure TTestBigDecimal.Test_Implicit_To_Variant;
var
  D: BigDecimal;
  V: Variant;
begin
  D := BigDecimal.Parse('100');
  V := D;
  CheckEquals(D.ToString, string(V));

  D := BigDecimal.Parse('5666213812379812739128731928371928378216626723123.88');
  V := D;
  CheckEquals(D.ToString, string(V));

  D := 100;
  V := D;
  CheckEquals(D.ToString, string(V));

  D := BigDecimal.Parse('-3687123672835666213812379812739128731928371928378216626723123.31232312388');
  V := D;
  CheckEquals(D.ToString, string(V));
end;

procedure TTestBigDecimal.Test_IsNegative;
var
  D: BigDecimal;
begin
  CheckFalse(BigDecimal(D).IsNegative);

  CheckFalse(BigDecimal(0).IsNegative);
  CheckFalse(BigDecimal(1).IsNegative);
  CheckFalse(BigDecimal(11.22).IsNegative);
  CheckTrue(BigDecimal(-11.22).IsNegative);
  CheckFalse(BigDecimal(32673512763).IsNegative);
  CheckFalse(BigDecimal.Parse('37861238762138716238721638172387126317823').IsNegative);
  CheckTrue(BigDecimal.Parse('-37861238762138716238721638172.387126317823').IsNegative);
end;

procedure TTestBigDecimal.Test_IsPositive;
var
  D: BigDecimal;
begin
  CheckTrue(BigDecimal(D).IsPositive);

  CheckTrue(BigDecimal(0).IsPositive);
  CheckTrue(BigDecimal(1).IsPositive);
  CheckTrue(BigDecimal(11.22).IsPositive);
  CheckFalse(BigDecimal(-11.22).IsPositive);
  CheckTrue(BigDecimal(32673512763).IsPositive);
  CheckTrue(BigDecimal.Parse('37861238762138716238721638172387126317823').IsPositive);
  CheckFalse(BigDecimal.Parse('-37861238762138716238721638172.387126317823').IsPositive);
end;

procedure TTestBigDecimal.Test_IsZero;
var
  D: BigDecimal;
begin
  CheckTrue(D.IsZero);
  CheckTrue(BigDecimal.Zero.IsZero);
  CheckFalse(BigDecimal.One.IsZero);
end;

procedure TTestBigDecimal.Test_MinusOne;
begin
  CheckTrue(BigDecimal.MinusOne = -1);
end;

procedure TTestBigDecimal.Test_MinusTen;
begin
  CheckTrue(BigDecimal.MinusTen = -10);
end;

procedure TTestBigDecimal.Test_One;
begin
  CheckTrue(BigDecimal.One = 1);
end;

procedure TTestBigDecimal.Test_Op_Add;
var
  X: BigDecimal;
begin
  X := BigDecimal(0) + BigDecimal(0);
  CheckEquals('0', X.ToString(false));

  X := BigDecimal.Parse('1.00') + BigDecimal(0);
  CheckEquals('1.00', X.ToString(false));

  X := BigDecimal.Parse('1.10') + BigDecimal.Parse('0.001');
  CheckEquals('1.101', X.ToString(false));

  X := BigDecimal.Parse('-5.55') + BigDecimal.Parse('-1.11');
  CheckEquals('-6.66', X.ToString(false));

  X := BigDecimal.Parse('1.00001') + BigDecimal.Parse('-1.00');
  CheckEquals('0.00001', X.ToString(false));
end;

procedure TTestBigDecimal.Test_Op_Divide;
var
  X, Y, Z: BigDecimal;
begin
  X := 0;
  Y := 1;
  Z := X / Y;
  CheckEquals('0', Z.ToString);

  X := 5;
  Y := 5;
  Z := X / Y;
  CheckEquals('1', Z.ToString);

  X := 10;
  Y := 2;
  Z := X / Y;
  CheckEquals('5', Z.ToString);

  X := BigDecimal.Parse('-12.50');
  Y := 10;
  Z := X / Y;
  CheckEquals('-1.25', Z.ToString);

  { Exceptional cases }
  CheckException(procedure begin
    X := 100;
    Y := 3;
    Z := X / Y;
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckException(procedure begin
    X := 1;
    Y := 2;
    Z := X / Y;
  end, EInvalidOp, 'Expected an EInvalidOp!');

  CheckException(procedure begin
    X := 100;
    Y := 0;
    Z := X / Y;
  end, EDivByZero, 'Expected an EDivByZero!');
end;

procedure TTestBigDecimal.Test_Op_Multiply;
var
  X: BigDecimal;
begin
  X := BigDecimal(0) * BigDecimal(0);
  CheckEquals('0', X.ToString(false));

  X := BigDecimal(1) * BigDecimal(0);
  CheckEquals('0', X.ToString(false));

  X := BigDecimal(1.122) * BigDecimal(0);
  CheckEquals('0', X.ToString(false));

  X := BigDecimal(0) * BigDecimal(-1111.77);
  CheckEquals('0', X.ToString(false));

  X := BigDecimal(100) * BigDecimal(10);
  CheckEquals('1000', X.ToString(false));

  X := BigDecimal.Parse('100.10') * BigDecimal(2);
  CheckEquals('200.20', X.ToString(false));

  X := BigDecimal.Parse('00.50') * BigDecimal(2);
  CheckEquals('1.00', X.ToString(false));
end;

procedure TTestBigDecimal.Test_Op_Negative;
var
  X, Y: BigDecimal;
begin
  X := 0; Y := -X;
  CheckTrue(X = Y);

  X := 10000; Y := -X;
  CheckTrue(Y = -10000);

  X := BigDecimal.Parse('-312783612783612783612321783.8821212'); Y := -X;
  CheckTrue(Y = BigDecimal.Parse('312783612783612783612321783.8821212'));

  X := BigDecimal.Parse('-3.67'); Y := -X;
  CheckTrue(Y = BigDecimal.Parse('3.67'));
end;

procedure TTestBigDecimal.Test_Op_Positive;
var
  X, Y: BigDecimal;
begin
  X := 0; Y := +X;
  CheckTrue(X = Y);

  X := 10000; Y := +X;
  CheckTrue(X = Y);

  X := BigDecimal.Parse('-312783612783612783612321783.8821212'); Y := +X;
  CheckTrue(X = Y);

  X := BigDecimal.Parse('-3.67'); Y := +X;
  CheckTrue(X = Y);
end;

procedure TTestBigDecimal.Test_Op_Subtract;
var
  X: BigDecimal;
begin
  X := BigDecimal(0) - BigDecimal(0);
  CheckEquals('0', X.ToString(false));

  X := BigDecimal.Parse('1.00') - BigDecimal(0);
  CheckEquals('1.00', X.ToString(false));

  X := BigDecimal.Parse('1.10') - BigDecimal.Parse('0.001');
  CheckEquals('1.099', X.ToString(false));

  X := BigDecimal.Parse('-5.55') - BigDecimal.Parse('-1.11');
  CheckEquals('-4.44', X.ToString(false));

  X := BigDecimal.Parse('1.00001') - BigDecimal.Parse('-1.00');
  CheckEquals('2.00001', X.ToString(false));
end;

procedure TTestBigDecimal.Test_Parse;
var
  D: BigDecimal;
begin
  D := BigDecimal.Parse('0');
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Parse('0.0');
  CheckEquals('0.0', D.ToString(false));

  D := BigDecimal.Parse(',000.000,');
  CheckEquals('0.000', D.ToString(false));

  D := BigDecimal.Parse('-0');
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Parse(' +0');
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Parse('   -99');
  CheckEquals('-99', D.ToString(false));

  D := BigDecimal.Parse('1,000.88');
  CheckEquals('1000.88', D.ToString(false));

  D := BigDecimal.Parse(',100.999,');
  CheckEquals('100.999', D.ToString(false));

  D := BigDecimal.Parse('-3652167,532,163,526,532,635,621,321,321.893,281,3928132111');
  CheckEquals('-3652167532163526532635621321321.8932813928132111', D.ToString(false));

  D := BigDecimal.Parse('1,234.5E-4');
  CheckEquals('0.12345', D.ToString());

  D := BigDecimal.Parse('0E+7');
  CheckEquals('0E+7', D.ToString());

  D := BigDecimal.Parse(',123E+6');
  CheckEquals('1.23E+8', D.ToString());

  D := BigDecimal.Parse('12.3E+7');
  CheckEquals('1.23E+8', D.ToString());

  D := BigDecimal.Parse('- 1');
  CheckEquals('-1', D.ToString(false));

  { Bad cases }
  CheckException(procedure begin
    D := BigDecimal.Parse(' +1. 0');
  end, EConvertError, 'Expected EConvertError!');

  CheckException(procedure begin
    D := BigDecimal.Parse('0.1,000');
  end, EConvertError, 'Expected EConvertError!');

  CheckException(procedure begin
    D := BigDecimal.Parse('10,00.000,');
  end, EConvertError, 'Expected EConvertError!');

  CheckException(procedure begin
    D := BigDecimal.Parse('1,23,456.789');
  end, EConvertError, 'Expected EConvertError!');

  CheckException(procedure begin
    D := BigDecimal.Parse('123A.99');
  end, EConvertError, 'Expected EConvertError!');
end;

procedure TTestBigDecimal.Test_Parse_FmtSettings;
var
  D: BigDecimal;
  L: TFormatSettings;
begin
  L.DecimalSeparator := '|';
  L.ThousandSeparator := '_';

  D := BigDecimal.Parse('0', L);
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Parse('0|0', L);
  CheckEquals('0.0', D.ToString(false));

  D := BigDecimal.Parse('_000|000_', L);
  CheckEquals('0.000', D.ToString(false));

  D := BigDecimal.Parse('-0', L);
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Parse(' +0', L);
  CheckEquals('0', D.ToString(false));

  D := BigDecimal.Parse('   -99', L);
  CheckEquals('-99', D.ToString(false));

  D := BigDecimal.Parse('1_000|88', L);
  CheckEquals('1000.88', D.ToString(false));

  D := BigDecimal.Parse('_100|999_', L);
  CheckEquals('100.999', D.ToString(false));

  D := BigDecimal.Parse('-3652167_532_163_526_532_635_621_321_321|893_281_3928132111', L);
  CheckEquals('-3652167532163526532635621321321.8932813928132111', D.ToString(false));

  D := BigDecimal.Parse('1_234|5E-4', L);
  CheckEquals('0.12345', D.ToString());

  D := BigDecimal.Parse('0E+7', L);
  CheckEquals('0E+7', D.ToString());

  D := BigDecimal.Parse('_123E+6', L);
  CheckEquals('1.23E+8', D.ToString());

  D := BigDecimal.Parse('12|3E+7', L);
  CheckEquals('1.23E+8', D.ToString());

  D := BigDecimal.Parse('- 1', L);
  CheckEquals('-1', D.ToString(false));

  { Bad cases }
  CheckException(procedure begin
    D := BigDecimal.Parse(' +1| 0', L);
  end, EConvertError, 'Expected EConvertError!');

  CheckException(procedure begin
    D := BigDecimal.Parse('0|1_000', L);
  end, EConvertError, 'Expected EConvertError!');

  CheckException(procedure begin
    D := BigDecimal.Parse('10_00|000,', L);
  end, EConvertError, 'Expected EConvertError!');

  CheckException(procedure begin
    D := BigDecimal.Parse('1_23,456|789', L);
  end, EConvertError, 'Expected EConvertError!');

  CheckException(procedure begin
    D := BigDecimal.Parse('123A.99', L);
  end, EConvertError, 'Expected EConvertError!');
end;

procedure TTestBigDecimal.Test_Pow;
var
  D: BigDecimal;
begin
  CheckEquals('0', D.Pow(5).ToString(false));
  CheckEquals('1', D.Pow(0).ToString(false));

  D := 3;
  CheckEquals('1', D.Pow(0).ToString(false));
  CheckEquals('3', D.Pow(1).ToString(false));
  CheckEquals('9.00', D.Pow(2, 2).ToString(false));

  D := 2;
  CheckEquals('0.5', D.Pow(-1, 1, rmHalfEven).ToString(false));
  CheckEquals('0.5', D.Pow(-1, 1, rmUp).ToString(false));
  CheckEquals('1', D.Pow(-1, 0, rmUp).ToString(false));
  CheckEquals('0.250', D.Pow(-2, 3, rmDown).ToString(false));
end;

procedure TTestBigDecimal.Test_Precision;
var
  D: BigDecimal;
begin
  CheckEquals(1, BigDecimal(D).Precision);

  CheckEquals(2, BigDecimal.Create(10, 0).Precision);
  CheckEquals(2, BigDecimal.Create(10, 1).Precision);
  CheckEquals(3, BigDecimal.Create(100).Precision);
  CheckEquals(55, BigDecimal.Create(-0.1).Precision);
  CheckEquals(48, BigDecimal.Create(-66.11).Precision);
end;

procedure TTestBigDecimal.Test_Rescale;
var
  LDec: BigDecimal;
begin
  { Normal rescaling }
  LDec := BigDecimal.Parse('100').Rescale(0);
  CheckEquals('100', LDec.ToString(false));

  LDec := BigDecimal.Parse('100.20').Rescale(1);
  CheckEquals('100.2', LDec.ToString(false));

  LDec := BigDecimal.Parse('-1.99').Rescale(3);
  CheckEquals('-1.990', LDec.ToString(false));

  LDec := BigDecimal.Parse('0').Rescale(3);
  CheckEquals(3, LDec.Scale);
  CheckEquals('0.000', LDec.ToString(false));

  { Check for rounding error }
  CheckException(procedure begin
    LDec := BigDecimal.Parse('-1.99').Rescale(1);
  end, EInvalidOp, 'Expected an exception!');

  CheckException(procedure begin
    LDec := BigDecimal.Parse('-1.1').Rescale(0);
  end, EInvalidOp, 'Expected an exception!');

  CheckException(procedure begin
    LDec := BigDecimal.Parse('0.1').Rescale(0);
  end, EInvalidOp, 'Expected an exception!');

  { Rescaling with rounding }
  LDec := BigDecimal.Parse('100.1').Rescale(0, rmUp);
  CheckEquals('101', LDec.ToString(false));

  LDec := BigDecimal.Parse('100.29').Rescale(1, rmDown);
  CheckEquals('100.2', LDec.ToString(false));

  LDec := BigDecimal.Parse('-1.99').Rescale(3, rmUp { not used } );
  CheckEquals('-1.990', LDec.ToString(false));

  LDec := BigDecimal.Parse('0.000000000012').Rescale(1, rmCeiling);
  CheckEquals('0.1', LDec.ToString(false));
end;

procedure TTestBigDecimal.Test_Round;
var
  X, R: BigDecimal;
begin
  X := BigDecimal.Parse('12345');
  R := X.Round(2);
  CheckEquals('12000', R.ToString(false));

  X := BigDecimal.Parse('12.23');
  R := X.Round(4);
  CheckEquals('12.23', R.ToString(false));

  X := BigDecimal.Parse('12.23');
  R := X.Round(5);
  CheckEquals('12.23', R.ToString(false));

  X := BigDecimal.Parse('-12.23');
  R := X.Round(3);
  CheckEquals('-12.2', R.ToString(false));

  X := BigDecimal.Parse('-12.23');
  R := X.Round(3, rmUp);
  CheckEquals('-12.3', R.ToString(false));

  CheckException(procedure begin
    X := BigDecimal.Parse('-12.23');
    R := X.Round(3, rmNone);
  end, Exception,
  'Expected an exception!');
end;

procedure TTestBigDecimal.Test_Scale;
var
  D: BigDecimal;
begin
  CheckEquals(0, BigDecimal(D).Scale);

  CheckEquals(0, BigDecimal.Create(10, 0).Scale);
  CheckEquals(1, BigDecimal.Create(10, 1).Scale);
  CheckEquals(0, BigDecimal.Create(100).Scale);
  CheckEquals(55, BigDecimal.Create(-0.1).Scale);
  CheckEquals(-5, BigDecimal.Create(1, -5).Scale);
end;

procedure TTestBigDecimal.Test_ScaleByPowerOfTen;
var
  X: BigDecimal;
begin
  X := X.ScaleByPowerOfTen(1);
  CheckEquals('00', X.ToString(false));

  X := X.ScaleByPowerOfTen(5);
  CheckEquals('000000', X.ToString(false));

  X := 2;
  X := X.ScaleByPowerOfTen(2);
  CheckEquals('200', X.ToString(false));

  X := X.ScaleByPowerOfTen(-4);
  CheckEquals('0.02', X.ToString(false));

  X := X.ScaleByPowerOfTen(-1);
  CheckEquals('0.002', X.ToString(false));
end;

procedure TTestBigDecimal.Test_Sign;
var
  X: BigDecimal;
begin
  CheckEquals(0, X.Sign);
  CheckEquals(0, BigDecimal.Zero.Sign);
  CheckEquals(1, BigDecimal.One.Sign);
  CheckEquals(1, BigDecimal.Ten.Sign);
  CheckEquals(-1, BigDecimal.MinusOne.Sign);
  CheckEquals(-1, BigDecimal.MinusTen.Sign);
end;

procedure TTestBigDecimal.Test_Ten;
begin
  CheckTrue(BigDecimal.Ten = 10);
end;

procedure TTestBigDecimal.Test_ToBigInteger;
var
  LInt: BigInteger;
begin
  LInt := BigDecimal.Parse('100.99').Truncate;
  CheckEquals('100', LInt.ToString);

  LInt := BigDecimal.Parse('-0.01').Truncate;
  CheckEquals('0', LInt.ToString);

  LInt := BigDecimal.Parse('-199.99999999').Truncate;
  CheckEquals('-199', LInt.ToString);

  LInt := BigDecimal.Parse('-32132132132139999882223232111113232.32132132132139999882223232111113232').Truncate;
  CheckEquals('-32132132132139999882223232111113232', LInt.ToString);
end;

procedure TTestBigDecimal.Test_ToDouble;
var
  D: BigDecimal;
  E: Double;
begin
  D := 100;
  E := D.ToDouble;
  CheckTrue(SameValue(E, 100));

  D := -0.15;
  E := D.ToDouble;
  CheckTrue(SameValue(E, -0.15));
end;

procedure TTestBigDecimal.Test_ToString;
var
  S: string;
begin
  { non-E notation }
  S := '-1';
  CheckEquals(S, BigDecimal.Parse(S).ToString(false));

  S := '-100.10';
  CheckEquals(S, BigDecimal.Parse(S).ToString(false));

  S := '38172636578125376123523217635127635213762';
  CheckEquals(S, BigDecimal.Parse(S).ToString(false));

  S := '-0.38172636578125376123523217635127635213762';
  CheckEquals(S, BigDecimal.Parse(S).ToString(false));

  S := '1111111111111111111111111111111111111111111111111111111111111111111111111.88';
  CheckEquals(S, BigDecimal.Parse(S).ToString(false));

  { E notation }
  S := '-1';
  CheckEquals(S, BigDecimal.Parse(S).ToString());

  S := '-100.10';
  CheckEquals(S, BigDecimal.Parse(S).ToString());

  S := '38172636578125376123523217635127635213762';
  CheckEquals(S, BigDecimal.Parse(S).ToString());

  S := '-0.38172636578125376123523217635127635213762';
  CheckEquals(S, BigDecimal.Parse(S).ToString());

  S := '1111111111111111111111111111111111111111111111111111111111111111111111111.88';
  CheckEquals(S, BigDecimal.Parse(S).ToString());

  S := '12345';
  CheckEquals('1.2E+4', BigDecimal.Parse(S).Round(2).ToString());
  CheckEquals('1E+1', BigDecimal.Create(1, -1).ToString());
  CheckEquals('1.2E+6', BigDecimal.Create(12, -5).ToString());
  CheckEquals('0E+5', BigDecimal.Create(0, -5).ToString());
end;

procedure TTestBigDecimal.Test_ToString_FmtSettings;
var
  S: string;
  L: TFormatSettings;
begin
  { non-E notation }
  S := '-1';
  L.DecimalSeparator := '|';

  CheckEquals(S, BigDecimal.Parse(S, L).ToString(L, false));

  S := '-100|10';
  CheckEquals(S, BigDecimal.Parse(S, L).ToString(L, false));

  S := '38172636578125376123523217635127635213762';
  CheckEquals(S, BigDecimal.Parse(S, L).ToString(L, false));

  S := '-0|38172636578125376123523217635127635213762';
  CheckEquals(S, BigDecimal.Parse(S, L).ToString(L, false));

  S := '1111111111111111111111111111111111111111111111111111111111111111111111111|88';
  CheckEquals(S, BigDecimal.Parse(S, L).ToString(L, false));

  { E notation }
  S := '-1';
  CheckEquals(S, BigDecimal.Parse(S, L).ToString(L));

  S := '-100|10';
  CheckEquals(S, BigDecimal.Parse(S, L).ToString(L));

  S := '38172636578125376123523217635127635213762';
  CheckEquals(S, BigDecimal.Parse(S, L).ToString(L));

  S := '-0|38172636578125376123523217635127635213762';
  CheckEquals(S, BigDecimal.Parse(S, L).ToString(L));

  S := '1111111111111111111111111111111111111111111111111111111111111111111111111|88';
  CheckEquals(S, BigDecimal.Parse(S, L).ToString(L));

  S := '12345';
  CheckEquals('1|2E+4', BigDecimal.Parse(S, L).Round(2).ToString(L));
  CheckEquals('1E+1', BigDecimal.Create(1, -1).ToString(L));
  CheckEquals('1|2E+6', BigDecimal.Create(12, -5).ToString(L));
  CheckEquals('0E+5', BigDecimal.Create(0, -5).ToString(L));
end;

procedure TTestBigDecimal.Test_TryParse;
var
  D: BigDecimal;
begin
  CheckTrue(BigDecimal.TryParse('0', D));
  CheckEquals('0', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('0.0', D));
  CheckEquals('0.0', D.ToString(false));

  CheckTrue(BigDecimal.TryParse(',000.000,', D));
  CheckEquals('0.000', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('-0', D));
  CheckEquals('0', D.ToString(false));

  CheckTrue(BigDecimal.TryParse(' +0', D));
  CheckEquals('0', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('   -99', D));
  CheckEquals('-99', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('1,000.88', D));
  CheckEquals('1000.88', D.ToString(false));

  CheckTrue(BigDecimal.TryParse(',100.999,', D));
  CheckEquals('100.999', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('-3652167,532,163,526,532,635,621,321,321.893,281,3928132111', D));
  CheckEquals('-3652167532163526532635621321321.8932813928132111', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('1,234.5E-4', D));
  CheckEquals('0.12345', D.ToString());

  CheckTrue(BigDecimal.TryParse('0E+7', D));
  CheckEquals('0E+7', D.ToString());

  CheckTrue(BigDecimal.TryParse(',123E+6', D));
  CheckEquals('1.23E+8', D.ToString());

  CheckTrue(BigDecimal.TryParse('12.3E+7', D));
  CheckEquals('1.23E+8', D.ToString());

  CheckTrue(BigDecimal.TryParse('- 1', D));
  CheckEquals('-1', D.ToString());

  { Bad cases }
  CheckFalse(BigDecimal.TryParse(' +1. 0', D));
  CheckFalse(BigDecimal.TryParse('0.1,000', D));
  CheckFalse(BigDecimal.TryParse('10,00.000,', D));
  CheckFalse(BigDecimal.TryParse('1,23,456.789', D));
  CheckFalse(BigDecimal.TryParse('123A.99', D));
end;

procedure TTestBigDecimal.Test_TryParse_FmtSettings;
var
  D: BigDecimal;
  L: TFormatSettings;
begin
  L.DecimalSeparator := '|';
  L.ThousandSeparator := '_';

  CheckTrue(BigDecimal.TryParse('0', D, L));
  CheckEquals('0', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('0|0', D, L));
  CheckEquals('0.0', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('_000|000_', D, L));
  CheckEquals('0.000', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('-0', D, L));
  CheckEquals('0', D.ToString(false));

  CheckTrue(BigDecimal.TryParse(' +0', D, L));
  CheckEquals('0', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('   -99', D, L));
  CheckEquals('-99', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('1_000|88', D, L));
  CheckEquals('1000.88', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('_100|999_', D, L));
  CheckEquals('100.999', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('-3652167_532_163_526_532_635_621_321_321|893_281_3928132111', D, L));
  CheckEquals('-3652167532163526532635621321321.8932813928132111', D.ToString(false));

  CheckTrue(BigDecimal.TryParse('1_234|5E-4', D, L));
  CheckEquals('0.12345', D.ToString());

  CheckTrue(BigDecimal.TryParse('0E+7', D, L));
  CheckEquals('0E+7', D.ToString());

  CheckTrue(BigDecimal.TryParse('_123E+6', D, L));
  CheckEquals('1.23E+8', D.ToString());

  CheckTrue(BigDecimal.TryParse('12|3E+7', D, L));
  CheckEquals('1.23E+8', D.ToString());

  CheckTrue(BigDecimal.TryParse('- 1', D, L));
  CheckEquals('-1', D.ToString(false));

  { Bad cases }
  CheckFalse(BigDecimal.TryParse(' +1| 0', D, L));
  CheckFalse(BigDecimal.TryParse('0|1_000', D, L));
  CheckFalse(BigDecimal.TryParse('10_00|000,', D, L));
  CheckFalse(BigDecimal.TryParse('1_23,456|789', D, L));
  CheckFalse(BigDecimal.TryParse('123A.99', D, L));
end;

procedure TTestBigDecimal.Test_VariantSupport;
var
  X, Y: Variant;
  M: Integer;
begin
  { Check conversions }
  X := BigDecimal.Parse('397129037219837128937128937128937189273892173123218937129864872346237.85');
  Y := BigDecimal(100);

  Check(X = '397129037219837128937128937128937189273892173123218937129864872346237.85', 'Variant value expected to be "397129037219837128937128937128937189273892173123218937129864872346237.85"');
  Check(Y = 100, 'Variant value expected to be "100"');

  { Check opeartors a bit }
  X := X + Y;
  Check(X = '397129037219837128937128937128937189273892173123218937129864872346337.85', 'Variant value expected to be "397129037219837128937128937128937189273892173123218937129864872346337.85"');

  X := X - Y;
  Check(X = '397129037219837128937128937128937189273892173123218937129864872346237.85', 'Variant value expected to be "397129037219837128937128937128937189273892173123218937129864872346237.85"');

  X := BigDecimal(100);
  Y := X / 3;
  CheckEquals('33', string(Y), 'Variant value expected to be "34"');

  X := BigDecimal(100);
  Y := X * 3;
  CheckEquals('300', string(Y), 'Variant value expected to be "300"');

  X := BigDecimal(78);

  CheckException(procedure begin
    Y := X div 4;
  end, Exception,
  'Expected an exception!');

  M := X;
  CheckEquals('78', IntToStr(M), 'M''s value expected to be "78"');

  VarClear(X);
  Check(X = 0, 'Variant value expected to be "0"');

  X := BigDecimal(100);
  Y := BigDecimal(200);

  Check(X < Y, 'X Expected to be less than Y');
  Check(Y > X, 'Y Expected to be greater than X');
  Check(Y >= X, 'Y Expected to be greater or equal than X');
  Check(X <= Y, 'X Expected to be less or equal than Y');
end;

procedure TTestBigDecimal.Test_VarType;
var
  L: Variant;
begin
  L := BigDecimal(10);
  CheckEquals(VarType(L), BigDecimal.VarType);
end;

procedure TTestBigDecimal.Test_Zero;
begin
  CheckTrue(BigDecimal.Zero = 0);
end;

{ TTextCaseEx }

procedure TTextCaseEx.CheckException(const AProcedure: TProc;
  const AExceptionClass: ExceptionClass; const AMessage: string);
var
  bWasEx : Boolean;
begin
  bWasEx := False;

  try
    { Cannot self-link }
    AProcedure();
  except
    on E : Exception do
    begin
      if E is AExceptionClass then
        bWasEx := True;
    end;
  end;

  CheckTrue(bWasEx, AMessage);
end;

end.
