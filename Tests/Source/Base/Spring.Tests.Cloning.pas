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

unit Spring.Tests.Cloning;
interface
uses SysUtils,
     Types,
     Math,
     DateUtils,
     TestFramework,
     Spring,
     Spring.Cloning;

type
  TTextCaseEx = class(TTestCase)
  protected
    procedure CheckException(const AProcedure: TProc; const AExceptionClass: ExceptionClass; const AMessage: string); overload;
  end;

  { Exception for field tests }
  EFieldFailError = class(Exception)
    constructor Create(const AFieldName, AExp, AActual: String);
  end;

  TTestCloning = class(TTextCaseEx)
  private
    class function Replicate<T>(const AValue: T): T; static;

  published
    procedure Test_Simple;
    procedure Test_Integers;
    procedure Test_Floats;
    procedure Test_Strings;
    procedure Test_Booleans;
    procedure Test_Arrays;
    procedure Test_DTs;
    procedure Test_EnumsAndSets;
    procedure Test_ClassSelfRef;
    procedure Test_ClassNil;
    procedure Test_FlatArray;
    procedure Test_RecordSelfRef;
    procedure Test_DoubleRefs;
    procedure Test_ClassComplicated;
    procedure Test_ClassSameFields;
    procedure Test_ByReference;
    procedure Test_ByReference_Defaults;
    procedure Test_NonReplicable;
    procedure Test_InterfaceCopy;
    procedure Test_UStringCopy;
    procedure Test_AStringCopy;
    procedure Test_DynArrayCopy;
  end;

  TTestCloneableObject = class(TTextCaseEx)
  published
    procedure Test_Clone;
    procedure Test_Clone_With_Intf;
    procedure Test_ICloneable_Deep;
  end;

type
  TBytes = array of Byte; // HACK, SysUtils.TBytes has no RTTI anymore :(

{ By-refence tests }
type
  PFormatSettings = ^TFormatSettings;
  TByRefTest = record
    [CloneKind(ckReference)]
    FObject: TObject;

    [CloneKind(ckReference)]
    FArray: TBytes;

    [CloneKind(ckReference)]
    FPtrArr: PFormatSettings;
  end;

  TDefaultsTest = record
    FObject: TObject;
    FArray: TBytes;
    FPtrArr: PFormatSettings;
  end;

{ Testing for non-replicable }
type
  TNonReplicableTest = record
    FString: String;

    [CloneKind(ckSkip)]
    FNonReplString: String;
  end;

type
  { Define a cloneable object }
  TMyCloneable = class(TCloneableObject)
    FData: String;

    [CloneKind(ckSkip)]
    FNoCopy: Integer;

    [CloneKind(ckReference)]
    FDynArray: TBytes;
  end;

  { Clone in clone test }
  TCoolCloneable = class(TCloneableObject)
    FIndex: Integer;

    [CloneKind(ckDeep)]
    FCloneMeToo: TCoolCloneable;

    function Clone(): TObject; override;
  end;

{ Flat tests }
type
  TFlatCopyTest = record
    [CloneKind(ckFlat)]
    FArr: array of TObject;
  end;

{ Double refs }
type
  TDoubleRefCopyTest = record
    [CloneKind(ckDeep)]
    FObj1: TObject;

    [CloneKind(ckReference)]
    FObj2: TObject;

    [CloneKind(ckDeep)]
    FRec1: PFormatSettings;

    [CloneKind(ckReference)]
    FRec2: PFormatSettings;

    [CloneKind(ckDeep)]
    FArr1: TBytes;

    [CloneKind(ckReference)]
    FArr2: TBytes;
  end;

{ Arrays record }
type
  TOneStrArray = array[0..0] of String;
  TOneIntArray = array[0..0] of Integer;

  TThreeStrArray = array[1..3] of String;
  TThreeIntArray = array[1..3] of Integer;

  TTwoStrArray = array[0..0, 0..0] of String;
  TTwoIntArray = array[0..0, 0..0] of Integer;

  TFourStrArray = array[1..2, 0..1] of String;
  TFourIntArray = array[1..2, 0..1] of Integer;

  TArraysRecord = record
  private
    { Dynamic arrays }
    FDynArrayStr_Empty, FDynArrayStr_One, FDynArrayStr_Many: TArray<String>;
    FDynArrayInt_Empty, FDynArrayInt_One, FDynArrayInt_Many: TIntegerDynArray;

    { One dimensional arrays }
    FOneDimArrayStr_One: TOneStrArray;
    FOneDimArrayStr_Three: TThreeStrArray;
    FOneDimArrayInt_One: TOneIntArray;
    FOneDimArrayInt_Three: TThreeIntArray;

    { Two dim arrays }
    FTwoDimArrayStr_Two: TTwoStrArray;
    FTwoDimArrayStr_Four: TFourStrArray;
    FTwoDimArrayInt_Two: TTwoIntArray;
    FTwoDimArrayInt_Four: TFourIntArray;
  public
    class function Create(): TArraysRecord; static;
    procedure CompareTo(const AOther: TArraysRecord);
  end;

{ Boolean record }
type
  TBooleanRecord = record
  private
    FBoolean_True, FBoolean_False: Boolean;
    FByteBool_True, FByteBool_False: ByteBool;
    FWordBool_True, FWordBool_False: WordBool;
    FLongBool_True, FLongBool_False: LongBool;

  public
    class function Create(): TBooleanRecord; static;
    procedure CompareTo(const AOther: TBooleanRecord);
  end;

{ Class with array of different subclasses }
type
  TContainer = class
  public type
    TOne = class
      FOneField: String;
      constructor Create;
      procedure Test;
    end;

    TTwo = class(TOne)
      FTwoField: Cardinal;
      constructor Create;
      procedure Test;
    end;

    TThree = class
      // no fields
    end;

  private
    [CloneKind(ckDeep)]
    FArray: array of TObject;

  public
    constructor Create(const CreateSubs: Boolean);
    destructor Destroy(); override;

    procedure Test(const WithSubs: Boolean);
  end;

{ Test interitance with same field names }
type
  TInhBase = class
  private
    FField: String;

  public
    procedure Test; virtual;
    constructor Create();
  end;

  TInhDerived = class(TInhBase)
  private
    FField: String;

  public
    procedure Test; override;
    constructor Create();
  end;

  TInhDerived2 = class(TInhDerived)
  private
    FField: String;

  public
    procedure Test; override;
    constructor Create();
  end;

{ Check for a chained class }
type
  TChainedClass = class
  public
    FSelf: TObject;
    FNil: TObject;

  public
    constructor Create();
  end;

{ Date-time record }
type
  TDateTimeRecord = record
  private
    FDateTime_Zero, FDateTime_Now: System.TDateTime;
    FTime_Zero, FTime_Now: System.TTime;
    FDate_Zero, FDate_Now: System.TDate;

  public
    class function Create(): TDateTimeRecord; static;
    procedure CompareTo(const AOther: TDateTimeRecord);
  end;

{ Enums and sets }
type
  TSomeEnum = (someOne, someTwo, someThree);
  TSomeSet = set of TSomeEnum;

  TEnumSetRecord = record
  private
    FEnum_One, FEnum_Two: TSomeEnum;
    FSet_Empty, FSet_One, FSet_All: TSomeSet;

  public
    class function Create(): TEnumSetRecord; static;
    procedure CompareTo(const AOther: TEnumSetRecord);
  end;

{ Floats record }
type
  TFloatsRecord = record
  private
    FHalf_Zero, FHalf_Lo, FHalf_Hi: Single;
    FSingle_Zero, FSingle_Lo, FSingle_Hi: Single;
    FDouble_Zero, FDouble_Lo, FDouble_Hi: Double;
    FExtended_Zero, FExtended_Lo, FExtended_Hi: Extended;
    FCurrency_Zero, FCurrency_Lo, FCurrency_Hi: Currency;
    FComp_Zero, FComp_Lo, FComp_Hi: Comp;
  public
    class function Create(): TFloatsRecord; static;
    procedure CompareTo(const AOther: TFloatsRecord);
  end;

{ Integers record }
type
  TIntsRecord = record
  private
    FByte_Zero, FByte_Lo, FByte_Hi: Byte;
    FShortInt_Zero, FShortInt_Lo, FShortInt_Hi: ShortInt;
    FWord_Zero, FWord_Lo, FWord_Hi: Word;
    FSmallInt_Zero, FSmallInt_Lo, FSmallInt_Hi: SmallInt;
    FCardinal_Zero, FCardinal_Lo, FCardinal_Hi: Cardinal;
    FInteger_Zero, FInteger_Lo, FInteger_Hi: Integer;
    FUInt64_Zero, FUInt64_Lo, FUInt64_Hi: UInt64;
    FInt64_Zero, FInt64_Lo, FInt64_Hi: Int64;

  public
    class function Create(): TIntsRecord; static;
    procedure CompareTo(const AOther: TIntsRecord);
  end;

{ Records by ref test }
type
  PLinkedItem = ^TLinkedItem;
  TLinkedItem = record
    FData: String;

    [CloneKind(ckDeep)]
    FSelf, FNil: PLinkedItem;
  end;

{ Strings record }
type
  TStringsRecord = record
  private
    FShortString_Empty, FShortString_One, FShortString_Long: ShortString;
    FUnicodeString_Empty, FUnicodeString_One, FUnicodeString_Long: UnicodeString;
    FAnsiString_Empty, FAnsiString_One, FAnsiString_Long: AnsiString;
    FWideString_Empty, FWideString_One, FWideString_Long: WideString;

    FUcs4String_Empty, FUcs4String_One, FUcs4String_Long: UCS4String;
    FRawByteString_Empty, FRawByteString_One, FRawByteString_Long: RawByteString;

    FAnsiChar_Zero, FAnsiChar_Lo, FAnsiChar_Hi: AnsiChar;
    FWideChar_Zero, FWideChar_Lo, FWideChar_Hi: WideChar;
    FUcs4Char_Zero, FUcs4Char_Lo, FUcs4Char_Hi: UCS4Char;
  public
    class function Create(): TStringsRecord; static;
    procedure CompareTo(const AOther: TStringsRecord);
  end;

implementation

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


{ TTestCloning }

class function TTestCloning.Replicate<T>(const AValue: T): T;
var
  LReplicator: TReplicator<T>;
begin
  LReplicator := TReplicator<T>.Create();

  try
    LReplicator.Replicate(AValue, Result);
  finally
    LReplicator.Free;
  end;
end;

procedure TTestCloning.Test_Arrays;
var
  LInput, LOutput: TArraysRecord;
begin
  LInput := TArraysRecord.Create;
  LOutput := Replicate<TArraysRecord>(LInput);
  LInput.CompareTo(LOutput);
end;

procedure TTestCloning.Test_AStringCopy;
var
  LInput, LOutput: AnsiString;
begin
  LInput := 'Test me!';
  LOutput := Replicate<AnsiString>(LInput);
  LInput := '';

  CheckEquals(1, StringRefCount(LOutput), 'StringRefCount(LOutput)');
  CheckEquals('Test me!', LOutput, 'LOutput');
end;

procedure TTestCloning.Test_Booleans;
var
  LInput, LOutput: TBooleanRecord;
begin
  LInput := TBooleanRecord.Create;
  LOutput := Replicate<TBooleanRecord>(LInput);
  LInput.CompareTo(LOutput);
end;

procedure TTestCloning.Test_ByReference;
var
  LInput, LOutput: TByRefTest;
  LFmtSett: TFormatSettings;
begin
  LInput.FObject := TObject.Create;
  LInput.FArray := TBytes.Create(1, 2, 3);
  LInput.FPtrArr := @LFmtSett;

  try
    LOutput := Replicate<TByRefTest>(LInput);

    CheckTrue(LInput.FObject = LOutput.FObject, 'FObject not copied by ref');
    CheckTrue(Pointer(LInput.FArray) = Pointer(LOutput.FArray), 'FArray not copied by ref');
    CheckTrue(LInput.FPtrArr = LOutput.FPtrArr, 'FPtrArr not copied by ref');
  finally
    LInput.FObject.Free;
  end;
end;

procedure TTestCloning.Test_ByReference_Defaults;
var
  LInput, LOutput: TDefaultsTest;
  LFmtSett: TFormatSettings;
begin
  LInput.FObject := TObject.Create;
  LInput.FArray := TBytes.Create(1, 2, 3);
  LInput.FPtrArr := @LFmtSett;

  try
    LOutput := Replicate<TDefaultsTest>(LInput);

    CheckTrue(LInput.FObject = LOutput.FObject, 'FObject not copied by ref');
    CheckTrue(Pointer(LInput.FArray) <> Pointer(LOutput.FArray), 'FArray copied by ref');
    CheckTrue(Length(LInput.FArray) = Length(LOutput.FArray), 'FArray bad copied');
    CheckTrue(LInput.FPtrArr = LOutput.FPtrArr, 'FPtrArr not copied by ref');
  finally
    LInput.FObject.Free;
  end;
end;

procedure TTestCloning.Test_ClassComplicated;
var
  LInput, LOutput: TContainer;
begin
  LInput := TContainer.Create(true);
  LOutput := nil;
  try
    LOutput := TContainer(Replicate<TObject>(LInput));

    CheckTrue(LOutput <> nil, 'LOutput is nil');
    CheckTrue(LOutput is TContainer, 'LOutput is not TContainer');

    LOutput.Test(true);
  finally
    LInput.Free;
    LOutput.Free;
  end;
end;

procedure TTestCloning.Test_ClassNil;
var
  LOutput: TObject;
begin
  LOutput := Replicate<TObject>(nil);
  CheckTrue(LOutput = nil, 'LOutput is not nil');
end;

procedure TTestCloning.Test_ClassSameFields;
var
  LInput, LOutput: TInhDerived2;
begin
  LInput := TInhDerived2.Create();
  LOutput := nil;

  try
    LOutput := Replicate<TInhDerived2>(LInput);

    CheckTrue(LOutput <> nil, 'LOutXml is nil');

    LOutput.Test();
  finally
    LInput.Free;
    LOutput.Free;
  end;
end;

procedure TTestCloning.Test_ClassSelfRef;
var
  LInput, LOutput: TChainedClass;
begin
  LInput := TChainedClass.Create;

  LOutput := nil;

  try
    LOutput := Replicate<TChainedClass>(LInput);

    CheckTrue(LOutput <> nil, 'LOutput is nil');
    CheckTrue(LOutput.FSelf = LOutput, 'LOutput.FSelf <> LOutXml');
    CheckTrue(LOutput.FNil = nil, 'LOutput.FSelf <> nil');
  finally
    LInput.Free;
    LOutput.Free;
  end;
end;

procedure TTestCloning.Test_DoubleRefs;
var
  LInput, LOutput: TDoubleRefCopyTest;
  LFmt: TFormatSettings;
begin
  LFmt := TFormatSettings.Create();

  { Initialize }
  LInput.FObj1 := TObject.Create;
  LInput.FObj2 := LInput.FObj1;
  LInput.FRec1 := @LFmt;
  LInput.FRec2 := @LFmt;
  LInput.FArr1 := TBytes.Create(1);
  LInput.FArr2 := LInput.FArr1;

  try
    LOutput := Replicate<TDoubleRefCopyTest>(LInput);

    CheckTrue(LInput.FObj1 <> LOutput.FObj1, 'FObj should not be the same object!');
    CheckTrue(LOutput.FObj2 = LOutput.FObj1, 'Both references should FObj be the same.');

    CheckTrue(LInput.FRec1 <> LOutput.FRec1, 'FRec1 should not be the same object!');
    CheckTrue(LOutput.FRec1 = LOutput.FRec2, 'Both references should FRec be the same.');

    CheckTrue(Pointer(LInput.FArr1) <> Pointer(LOutput.FArr1), 'FArr should not be the same object!');
    CheckTrue(Pointer(LOutput.FArr2) = Pointer(LOutput.FArr2), 'Both references should FArr be the same.');
    CheckTrue(Length(LOutput.FArr2) = Length(LOutput.FArr2), 'Lengths of FArr be the same.');
  finally
    LInput.FObj1.Free;
    LOutput.FObj1.Free;
    Dispose(LOutput.FRec1);
  end;
end;

procedure TTestCloning.Test_DTs;
var
  LInput, LOutput: TDateTimeRecord;
begin
  LInput := TDateTimeRecord.Create;
  LOutput := Replicate<TDateTimeRecord>(LInput);
  LInput.CompareTo(LOutput);
end;

procedure TTestCloning.Test_DynArrayCopy;
var
  LInput, LOutput: TBytes;
begin
  LInput := TBytes.Create(1, 2, 3);
  LOutput := Replicate<TBytes>(LInput);
  LInput := nil;

  CheckEquals(3, Length(LOutput), 'Length(LOutput)');
  CheckEquals(1, LOutput[0], 'LOutput[0]');
  CheckEquals(2, LOutput[1], 'LOutput[1]');
  CheckEquals(3, LOutput[2], 'LOutput[2]');
end;

procedure TTestCloning.Test_EnumsAndSets;
var
  LInput, LOutput: TEnumSetRecord;
begin
  LInput := TEnumSetRecord.Create;
  LOutput := Replicate<TEnumSetRecord>(LInput);
  LInput.CompareTo(LOutput);
end;

procedure TTestCloning.Test_FlatArray;
var
  LInput, LOutput: TFlatCopyTest;
begin
  SetLength(LInput.FArr, 1);
  LInput.FArr[0] := TObject.Create;

  try
    LOutput := Replicate<TFlatCopyTest>(LInput);

    CheckEquals(1, Length(LOutput.FArr), 'Length of FArr');
    CheckTrue(Pointer(LInput.FArr) <> Pointer(LOutput.FArr), 'Same array FArr');
    CheckTrue(LInput.FArr[0] = LOutput.FArr[0], 'The actual object');
  finally
    LInput.FArr[0].Free;
  end;
end;

procedure TTestCloning.Test_Floats;
var
  LInput, LOutput: TFloatsRecord;
begin
  LInput := TFloatsRecord.Create;
  LOutput := Replicate<TFloatsRecord>(LInput);
  LInput.CompareTo(LOutput);
end;

procedure TTestCloning.Test_Integers;
var
  LInput, LOutput: TIntsRecord;
begin
  LInput := TIntsRecord.Create;
  LOutput := Replicate<TIntsRecord>(LInput);
  LInput.CompareTo(LOutput);
end;

procedure TTestCloning.Test_InterfaceCopy;
var
  LObj: TInterfacedObject;
  LInput, LOutput: IInterface;
begin
  LObj := TInterfacedObject.Create;
  LInput := LObj;

  LOutput := Replicate<IInterface>(LInput);

  CheckTrue(Pointer(LInput) = Pointer(LOutput), 'Interface not copied properly.');
  CheckEquals(2, LObj.RefCount, 'Interface''s RefCount not adjusted.');
end;

procedure TTestCloning.Test_NonReplicable;
var
  LInput, LOutput: TNonReplicableTest;
begin
  LInput.FString := 'Hello Dudes!';
  LInput.FNonReplString := 'No copy me!';

  LOutput := Replicate<TNonReplicableTest>(LInput);

  CheckTrue(LInput.FString = LOutput.FString, 'FString not copied!');
  CheckTrue(LOutput.FNonReplString = '', 'FNonReplString was copied!');
end;

procedure TTestCloning.Test_RecordSelfRef;
var
  LInput, LOutput: PLinkedItem;
begin
  New(LInput);
  LInput.FData := 'Hello World!';
  LInput.FSelf := LInput;
  LInput.FNil := nil;

  LOutput := nil;
  try
    LOutput := Replicate<PLinkedItem>(LInput);

    CheckTrue(LOutput <> nil, 'LOutput is nil');
    CheckTrue(LOutput^.FSelf = LOutput, 'LOutput.FSelf <> LOutput');
    CheckTrue(LOutput^.FNil = nil, 'LOutput.FSelf <> nil');
  finally
    Dispose(LInput);

    if LOutput <> nil then
      Dispose(LOutput);
  end;
end;

procedure TTestCloning.Test_Simple;
var
  LInput, LOutput: Integer;
begin
  LInput := -100;
  LOutput := Replicate<Integer>(LInput);
  CheckEquals(LInput, LOutput, '(Integer)');
end;

procedure TTestCloning.Test_UStringCopy;
var
  LInput, LOutput: String;
begin
  LInput := 'Test me!';
  LOutput := Replicate<String>(LInput);
  LInput := '';

  CheckEquals(1, StringRefCount(LOutput), 'StringRefCount(LOutput)');
  CheckEquals('Test me!', LOutput, 'LOutput');
end;

procedure TTestCloning.Test_Strings;
var
  LInput, LOutput: TStringsRecord;
begin
  LInput := TStringsRecord.Create;
  LOutput := Replicate<TStringsRecord>(LInput);
  LInput.CompareTo(LOutput);
end;

{ TTestCloneableObject }

procedure TTestCloneableObject.Test_Clone;
var
  LObj, LCopy: TMyCloneable;
begin
  LObj := TMyCloneable.Create;
  LObj.FData := 'Some data';
  LObj.FNoCopy := 100;
  LObj.FDynArray := TBytes.Create(1, 2, 3, 4);

  { Clone }
  LCopy := LObj.Clone() as TMyCloneable;

  CheckTrue(LCopy <> nil, 'LCopy is nil');
  CheckEquals(0, LObj.RefCount, 'LObj.RefCount');
  CheckEquals(0, LCopy.RefCount, 'LCopy.RefCount');
  CheckEquals(LObj.FData, LCopy.FData, 'LObj.FData <> LCopy.FData');
  CheckNotEquals(LObj.FNoCopy, LCopy.FNoCopy, 'LObj.FNoCopy = LCopy.FNoCopy');
  CheckTrue(Pointer(LObj.FDynArray) = Pointer(LCopy.FDynArray), 'LObj.FDynArray <> LCopy.FDynArray');

  LObj.Free;
  LCopy.Free;
end;


procedure TTestCloneableObject.Test_Clone_With_Intf;
var
  LObj, LCopy: TMyCloneable;
  LIntf: ICloneable;
begin
  LObj := TMyCloneable.Create;
  LObj.FData := 'Some data';
  LObj.FNoCopy := 100;
  LObj.FDynArray := TBytes.Create(1, 2, 3, 4);
  LIntf := LObj;

  { Clone }
  LCopy := LIntf.Clone() as TMyCloneable;

  CheckTrue(LCopy <> nil, 'LCopy is nil');
  CheckEquals(1, LObj.RefCount, 'LObj.RefCount');
  CheckEquals(0, LCopy.RefCount, 'LCopy.RefCount');
  CheckEquals(LObj.FData, LCopy.FData, 'LObj.FData <> LCopy.FData');
  CheckNotEquals(LObj.FNoCopy, LCopy.FNoCopy, 'LObj.FNoCopy = LCopy.FNoCopy');
  CheckTrue(Pointer(LObj.FDynArray) = Pointer(LCopy.FDynArray), 'LObj.FDynArray <> LCopy.FDynArray');

  LCopy.Free;
end;

procedure TTestCloneableObject.Test_ICloneable_Deep;
var
  LObj, LCopy: TCoolCloneable;
begin
  LObj := TCoolCloneable.Create;
  LObj.FCloneMeToo := TCoolCloneable.Create;
  LObj.FCloneMeToo.FIndex := 99;
  LCopy := nil;

  try
    { Clone }
    LCopy := LObj.Clone() as TCoolCloneable;

    CheckTrue(LCopy <> nil, 'LCopy is nil');
    CheckTrue(LCopy.FCloneMeToo <> nil, 'LCopy.FCloneMeToos is nil');

    CheckEquals(0, LObj.RefCount, 'LCopy.RefCount');
    CheckEquals(0, LObj.FCloneMeToo.RefCount, 'LCopy.FCloneMeToo.RefCount');

    CheckEquals(1, LCopy.FIndex, 'LCopy.FIndex <> 1');
    CheckEquals(100, LCopy.FCloneMeToo.FIndex, 'LCopy.FCloneMeToo.FIndex <> 100');

    CheckEquals(0, LObj.FIndex, 'LObj.FIndex <> 0');
    CheckEquals(99, LObj.FCloneMeToo.FIndex, 'LObj.FCloneMeToo.FIndex <> 99');
  finally
    LObj.FCloneMeToo.Free;
    LObj.Free;

    if LCopy <> nil then
      LCopy.FCloneMeToo.Free;
    LCopy.Free;
  end;
end;

{ TCoolCloneable }

function TCoolCloneable.Clone: TObject;
begin
  Result := inherited Clone();
  TCoolCloneable(Result).FIndex := FIndex + 1;
end;

{ TArraysRecord }

procedure TArraysRecord.CompareTo(const AOther: TArraysRecord);
var
  I: Integer;
begin
  if Length(FDynArrayStr_Empty) <> Length(AOther.FDynArrayStr_Empty) then
    raise EFieldFailError.Create('(len) FDynArrayStr_Empty', '...', '...');

  if Length(FDynArrayStr_One) <> Length(AOther.FDynArrayStr_One) then
    raise EFieldFailError.Create('(len) FDynArrayStr_One', '...', '...')
  else
  begin
    for I := 0 to Length(FDynArrayStr_One) - 1 do
      if FDynArrayStr_One[I] <> AOther.FDynArrayStr_One[I] then
        raise EFieldFailError.Create('(' + IntToStr(I) + ') FDynArrayStr_One', '...', '...');
  end;

  if Length(FDynArrayStr_Many) <> Length(AOther.FDynArrayStr_Many) then
    raise EFieldFailError.Create('(len) FDynArrayStr_Many', '...', '...')
  else
  begin
    for I := 0 to Length(FDynArrayStr_Many) - 1 do
      if FDynArrayStr_Many[I] <> AOther.FDynArrayStr_Many[I] then
        raise EFieldFailError.Create('(' + IntToStr(I) + ') FDynArrayStr_Many', '...', '...');
  end;

  if Length(FDynArrayInt_Empty) <> Length(AOther.FDynArrayInt_Empty) then
    raise EFieldFailError.Create('(len) FDynArrayInt_Empty', '...', '...');

  if Length(FDynArrayInt_One) <> Length(AOther.FDynArrayInt_One) then
    raise EFieldFailError.Create('(len) FDynArrayInt_One', '...', '...')
  else
  begin
    for I := 0 to Length(FDynArrayInt_One) - 1 do
      if FDynArrayInt_One[I] <> AOther.FDynArrayInt_One[I] then
        raise EFieldFailError.Create('(' + IntToStr(I) + ') FDynArrayInt_One', '...', '...');
  end;

  if Length(FDynArrayInt_Many) <> Length(AOther.FDynArrayInt_Many) then
    raise EFieldFailError.Create('(len) FDynArrayInt_Many', '...', '...')
  else
  begin
    for I := 0 to Length(FDynArrayInt_Many) - 1 do
      if FDynArrayInt_Many[I] <> AOther.FDynArrayInt_Many[I] then
        raise EFieldFailError.Create('(' + IntToStr(I) + ') FDynArrayInt_Many', '...', '...');
  end;

  if FOneDimArrayStr_One[0] <> AOther.FOneDimArrayStr_One[0] then
    raise EFieldFailError.Create('FOneDimArrayStr_One', FOneDimArrayStr_One[0], AOther.FOneDimArrayStr_One[0]);

  if FOneDimArrayInt_One[0] <> AOther.FOneDimArrayInt_One[0] then
    raise EFieldFailError.Create('FOneDimArrayInt_One', IntToStr(FOneDimArrayInt_One[0]), IntToStr(AOther.FOneDimArrayInt_One[0]));


  if FOneDimArrayStr_Three[1] <> AOther.FOneDimArrayStr_Three[1] then
    raise EFieldFailError.Create('FOneDimArrayStr_Three[1]', FOneDimArrayStr_Three[1], AOther.FOneDimArrayStr_Three[1]);

  if FOneDimArrayStr_Three[2] <> AOther.FOneDimArrayStr_Three[2] then
    raise EFieldFailError.Create('FOneDimArrayStr_Three[2]', FOneDimArrayStr_Three[2], AOther.FOneDimArrayStr_Three[2]);

  if FOneDimArrayStr_Three[3] <> AOther.FOneDimArrayStr_Three[3] then
    raise EFieldFailError.Create('FOneDimArrayStr_Three[3]', FOneDimArrayStr_Three[3], AOther.FOneDimArrayStr_Three[3]);


  if FOneDimArrayInt_Three[1] <> AOther.FOneDimArrayInt_Three[1] then
    raise EFieldFailError.Create('FOneDimArrayInt_Three[1]', IntToStr(FOneDimArrayInt_Three[1]), IntToStr(AOther.FOneDimArrayInt_Three[1]));

  if FOneDimArrayInt_Three[2] <> AOther.FOneDimArrayInt_Three[2] then
    raise EFieldFailError.Create('FOneDimArrayStr_Three[2]', IntToStr(FOneDimArrayInt_Three[2]), IntToStr(AOther.FOneDimArrayInt_Three[2]));

  if FOneDimArrayInt_Three[3] <> AOther.FOneDimArrayInt_Three[3] then
    raise EFieldFailError.Create('FOneDimArrayStr_Three[3]', IntToStr(FOneDimArrayInt_Three[3]), IntToStr(AOther.FOneDimArrayInt_Three[3]));


  if FTwoDimArrayStr_Two[0, 0] <> AOther.FTwoDimArrayStr_Two[0, 0] then
    raise EFieldFailError.Create('FTwoDimArrayStr_Two', FTwoDimArrayStr_Two[0, 0], AOther.FTwoDimArrayStr_Two[0, 0]);

  if FTwoDimArrayInt_Two[0, 0] <> AOther.FTwoDimArrayInt_Two[0, 0] then
    raise EFieldFailError.Create('FTwoDimArrayInt_Two', IntToStr(FTwoDimArrayInt_Two[0, 0]), IntToStr(AOther.FTwoDimArrayInt_Two[0, 0]));


  if FTwoDimArrayStr_Four[1, 0] <> AOther.FTwoDimArrayStr_Four[1, 0] then
    raise EFieldFailError.Create('FTwoDimArrayStr_Four[1, 0]', FTwoDimArrayStr_Four[1, 0], AOther.FTwoDimArrayStr_Four[1, 0]);

  if FTwoDimArrayStr_Four[1, 1] <> AOther.FTwoDimArrayStr_Four[1, 1] then
    raise EFieldFailError.Create('FTwoDimArrayStr_Four[1, 1]', FTwoDimArrayStr_Four[1, 1], AOther.FTwoDimArrayStr_Four[1, 1]);

  if FTwoDimArrayStr_Four[2, 0] <> AOther.FTwoDimArrayStr_Four[2, 0] then
    raise EFieldFailError.Create('FTwoDimArrayStr_Four[2, 0]', FTwoDimArrayStr_Four[2, 0], AOther.FTwoDimArrayStr_Four[2, 0]);

  if FTwoDimArrayStr_Four[2, 1] <> AOther.FTwoDimArrayStr_Four[2, 1] then
    raise EFieldFailError.Create('FTwoDimArrayStr_Four[2, 1]', FTwoDimArrayStr_Four[2, 1], AOther.FTwoDimArrayStr_Four[2, 1]);


  if FTwoDimArrayInt_Four[1, 0] <> AOther.FTwoDimArrayInt_Four[1, 0] then
    raise EFieldFailError.Create('FOneDimArrayInt_Three[1, 0]', IntToStr(FTwoDimArrayInt_Four[1, 0]), IntToStr(AOther.FTwoDimArrayInt_Four[1, 0]));

  if FTwoDimArrayInt_Four[1, 1] <> AOther.FTwoDimArrayInt_Four[1, 1] then
    raise EFieldFailError.Create('FOneDimArrayStr_Three[1, 1]', IntToStr(FTwoDimArrayInt_Four[1, 1]), IntToStr(AOther.FTwoDimArrayInt_Four[1, 1]));

  if FTwoDimArrayInt_Four[2, 0] <> AOther.FTwoDimArrayInt_Four[2, 0] then
    raise EFieldFailError.Create('FOneDimArrayStr_Three[2, 0]', IntToStr(FTwoDimArrayInt_Four[2, 0]), IntToStr(AOther.FTwoDimArrayInt_Four[2, 0]));

  if FTwoDimArrayInt_Four[2, 1] <> AOther.FTwoDimArrayInt_Four[2, 1] then
    raise EFieldFailError.Create('FOneDimArrayStr_Three[2, 1]', IntToStr(FTwoDimArrayInt_Four[2, 1]), IntToStr(AOther.FTwoDimArrayInt_Four[2, 1]));
end;

class function TArraysRecord.Create: TArraysRecord;
begin
  { Dynamic arrays }
  SetLength(Result.FDynArrayStr_Empty, 0);
  Result.FDynArrayStr_One := TArray<String>.Create('A lovely string');
  Result.FDynArrayStr_Many := TArray<String>.Create('One', 'More', 'String');

  SetLength(Result.FDynArrayInt_Empty, 0);
  Result.FDynArrayInt_One := TIntegerDynArray.Create(1);
  Result.FDynArrayInt_Many := TIntegerDynArray.Create(10, 9, 8);

  { One dimensional arrays }
  Result.FOneDimArrayStr_One[0] := '(0)';
  Result.FOneDimArrayStr_Three[1] := '(1)'; Result.FOneDimArrayStr_Three[2] := '(2)'; Result.FOneDimArrayStr_Three[3] := '(3)';
  Result.FOneDimArrayInt_One[0] := 10;
  Result.FOneDimArrayInt_Three[1] := 11; Result.FOneDimArrayInt_Three[2] := 12; Result.FOneDimArrayInt_Three[3] := 13;

    { Two dim arrays }
  Result.FTwoDimArrayStr_Two[0, 0] := '(0,0)';
  Result.FTwoDimArrayStr_Four[1,0] := '(1,0)'; Result.FTwoDimArrayStr_Four[1,1] := '(1,1)';
  Result.FTwoDimArrayStr_Four[2,0] := '(2,0)'; Result.FTwoDimArrayStr_Four[2,1] := '(2,1)';

  Result.FTwoDimArrayInt_Two[0, 0] := 100;
  Result.FTwoDimArrayInt_Four[1,0] := 100; Result.FTwoDimArrayInt_Four[1,1] := 110;
  Result.FTwoDimArrayInt_Four[2,0] := 200; Result.FTwoDimArrayInt_Four[2,1] := 210;
end;

{ TBooleanRecord }

procedure TBooleanRecord.CompareTo(const AOther: TBooleanRecord);
begin
  if FBoolean_True <> AOther.FBoolean_True then
    raise EFieldFailError.Create('FBoolean_True', BoolToStr(FBoolean_True), BoolToStr(AOther.FBoolean_True));

  if FBoolean_False <> AOther.FBoolean_False then
    raise EFieldFailError.Create('FBoolean_False', BoolToStr(FBoolean_False), BoolToStr(AOther.FBoolean_False));


  if FByteBool_True <> AOther.FByteBool_True then
    raise EFieldFailError.Create('FByteBool_True', BoolToStr(FByteBool_True), BoolToStr(AOther.FByteBool_True));

  if FByteBool_False <> AOther.FByteBool_False then
    raise EFieldFailError.Create('FByteBool_False', BoolToStr(FByteBool_False), BoolToStr(AOther.FByteBool_False));


  if FWordBool_True <> AOther.FWordBool_True then
    raise EFieldFailError.Create('FWordBool_True', BoolToStr(FWordBool_True), BoolToStr(AOther.FWordBool_True));

  if FWordBool_False <> AOther.FWordBool_False then
    raise EFieldFailError.Create('FWordBool_False', BoolToStr(FWordBool_False), BoolToStr(AOther.FWordBool_False));


  if FLongBool_True <> AOther.FLongBool_True then
    raise EFieldFailError.Create('FLongBool_True', BoolToStr(FLongBool_True), BoolToStr(AOther.FLongBool_True));

  if FLongBool_False <> AOther.FLongBool_False then
    raise EFieldFailError.Create('FLongBool_False', BoolToStr(FLongBool_False), BoolToStr(AOther.FLongBool_False));
end;

class function TBooleanRecord.Create: TBooleanRecord;
begin
  Result.FBoolean_True := true;
  Result.FBoolean_False := false;

  Result.FByteBool_True := true;
  Result.FByteBool_False := false;

  Result.FWordBool_True := true;
  Result.FWordBool_False := false;

  Result.FLongBool_True := true;
  Result.FLongBool_False := false;
end;

{ TContainer }

constructor TContainer.Create(const CreateSubs: Boolean);
begin
  SetLength(FArray, 6);

  { 0 }
  FArray[0] := TOne.Create();

  { 1 }
  FArray[1] := TTwo.Create();

  { 2 }
  FArray[2] := TThree.Create();

  { 3 --> 0 }
  FArray[3] := FArray[0];

  { 4 --> nil }
  FArray[4] := nil;

  { 5 -- Another container }
  if CreateSubs then
    FArray[5] := TContainer.Create(false)
  else
    FArray[5] := Self;
end;

destructor TContainer.Destroy;
begin
  if Length(FArray) > 0 then
    FArray[0].Free;

  if Length(FArray) > 1 then
    FArray[1].Free;

  if Length(FArray) > 2 then
    FArray[2].Free;

  if Length(FArray) > 5 then
    if FArray[5] <> Self then
      FArray[5].Free;

  inherited;
end;

procedure TContainer.Test(const WithSubs: Boolean);
begin
  if Length(FArray) <> 6 then
    raise EFieldFailError.Create('(length) FArray', '6', IntToStr(Length(FArray)));

  if FArray[0] = nil then
    raise EFieldFailError.Create('FArray[0]', 'non-nil', 'nil');

  if not (FArray[0] is TOne) then
    raise EFieldFailError.Create('(class) FArray[0]', 'TOne', FArray[0].ClassName);

  { Test object }
  (FArray[0] as TOne).Test;


  if FArray[1] = nil then
    raise EFieldFailError.Create('FArray[1]', 'non-nil', 'nil');

  if not (FArray[1] is TTwo) then
    raise EFieldFailError.Create('(class) FArray[1]', 'TTwo', FArray[1].ClassName);

  { Test object }
  (FArray[1] as TTwo).Test;

  if FArray[2] = nil then
    raise EFieldFailError.Create('FArray[2]', 'non-nil', 'nil');

  if not (FArray[2] is TThree) then
    raise EFieldFailError.Create('(class) FArray[2]', 'TThree', FArray[2].ClassName);


  if FArray[3] <> FArray[0] then
    raise EFieldFailError.Create('FArray[3]', 'equals to FArray[0]', 'different');


  if FArray[4] <> nil then
    raise EFieldFailError.Create('FArray[4]', 'nil', 'different');


  if WithSubs then
  begin
    if FArray[5] = nil then
      raise EFieldFailError.Create('FArray[5]', 'non-nil', 'nil');

    if not (FArray[5] is TContainer) then
      raise EFieldFailError.Create('(class) FArray[5]', 'TContainer', FArray[5].ClassName);

    { Invoke sub test }
    (FArray[5] as TContainer).Test(false);
  end else
  begin
    if FArray[5] <> Self then
      raise EFieldFailError.Create('FArray[5]', '=Self', '<>Self');
  end;
end;

{ TContainer.TOne }

constructor TContainer.TOne.Create;
begin
  FOneField := 'TOne';
end;

procedure TContainer.TOne.Test;
begin
  if FOneField <> 'TOne' then
    raise EFieldFailError.Create('TOne.FOneField', 'TOne', FOneField);
end;

{ TContainer.TTwo }

constructor TContainer.TTwo.Create;
begin
  FOneField := 'TTwo';
  FTwoField := $DEADBABE;
end;

procedure TContainer.TTwo.Test;
begin
  if FOneField <> 'TTwo' then
    raise EFieldFailError.Create('TTwo.FOneField', 'TTwo', FOneField);

  if FTwoField <> $DEADBABE then
    raise EFieldFailError.Create('TTwo.FTwoField', '$DEADBABE', IntToStr(FTwoField));
end;


{ TInhBase }

constructor TInhBase.Create();
begin
  FField := '1';
end;

procedure TInhBase.Test;
begin
  if FField <> '1' then
    raise EFieldFailError.Create('TInhBase.FField', '1', FField);
end;

{ TInhDerived }

constructor TInhDerived.Create();
begin
  inherited;
  FField := '2';
end;

procedure TInhDerived.Test;
begin
  inherited;

  if FField <> '2' then
    raise EFieldFailError.Create('TInhDerived.FField', '2', FField);
end;

{ TInhDerived2 }

constructor TInhDerived2.Create();
begin
  inherited;
  FField := '3';
end;

procedure TInhDerived2.Test;
begin
  inherited;

  if FField <> '3' then
    raise EFieldFailError.Create('TInhDerived2.FField', '3', FField);
end;

{ TChainedClass }

constructor TChainedClass.Create;
begin
  FSelf := Self;
  FNil := nil;
end;


{ TDateTimeRecord }

procedure TDateTimeRecord.CompareTo(const AOther: TDateTimeRecord);
begin
  if FDateTime_Zero <> AOther.FDateTime_Zero then
    raise EFieldFailError.Create('FDateTime_Zero', DateTimeToStr(FDateTime_Zero), DateTimeToStr(AOther.FDateTime_Zero));

  if System.Abs(FDateTime_Now - AOther.FDateTime_Now) > 0.0001 then
    raise EFieldFailError.Create('FDateTime_Now', DateTimeToStr(FDateTime_Now), DateTimeToStr(AOther.FDateTime_Now));


  if FTime_Zero <> AOther.FTime_Zero then
    raise EFieldFailError.Create('FTime_Zero', TimeToStr(FTime_Zero), TimeToStr(AOther.FTime_Zero));

  if System.Abs(FTime_Now - AOther.FTime_Now) > 0.0001 then
    raise EFieldFailError.Create('FTime_Now', TimeToStr(FTime_Now), TimeToStr(AOther.FTime_Now));


  if FDate_Zero <> AOther.FDate_Zero then
    raise EFieldFailError.Create('FDate_Zero', DateToStr(FDate_Zero), DateToStr(AOther.FDate_Zero));

  if not SameDate(FDate_Now, AOther.FDate_Now) then
    raise EFieldFailError.Create('FDate_Now', DateToStr(FDate_Now), DateToStr(AOther.FDate_Now));

end;

class function TDateTimeRecord.Create: TDateTimeRecord;
begin
  Result.FDateTime_Zero := 0;
  Result.FDateTime_Now := Now;

  Result.FTime_Zero := 0;
  Result.FTime_Now := Time;

  Result.FDate_Zero := 0;
  Result.FDate_Now := Date;
end;


{ TEnumSetRecord }

procedure TEnumSetRecord.CompareTo(const AOther: TEnumSetRecord);
begin
  if FEnum_One <> AOther.FEnum_One then
    raise EFieldFailError.Create('FEnum_One', IntToStr(Integer(FEnum_One)), IntToStr(Integer(AOther.FEnum_One)));

  if FEnum_Two <> AOther.FEnum_Two then
    raise EFieldFailError.Create('FEnum_Two', IntToStr(Integer(FEnum_Two)), IntToStr(Integer(AOther.FEnum_Two)));


  if FSet_Empty <> AOther.FSet_Empty then
    raise EFieldFailError.Create('FSet_Empty', '...', '...');

  if FSet_One <> AOther.FSet_One then
    raise EFieldFailError.Create('FSet_One', '...', '...');

  if FSet_All <> AOther.FSet_All then
    raise EFieldFailError.Create('FSet_All', '...', '...');
end;

class function TEnumSetRecord.Create: TEnumSetRecord;
begin
  Result.FEnum_One := someOne;
  Result.FEnum_Two := someTwo;

  Result.FSet_Empty := [];
  Result.FSet_One := [someOne];
  Result.FSet_All := [someOne .. someThree];
end;

{ TFloatsRecord }

procedure TFloatsRecord.CompareTo(const AOther: TFloatsRecord);
begin
  if FHalf_Zero <> AOther.FHalf_Zero then
    raise EFieldFailError.Create('FHalf_Zero', FloatToStr(FHalf_Zero), FloatToStr(AOther.FHalf_Zero));

  if FHalf_Lo <> AOther.FHalf_Lo then
    raise EFieldFailError.Create('FHalf_Lo', FloatToStr(FHalf_Lo), FloatToStr(AOther.FHalf_Lo));

  if FHalf_Hi <> AOther.FHalf_Hi then
    raise EFieldFailError.Create('FHalf_Hi', FloatToStr(FHalf_Hi), FloatToStr(AOther.FHalf_Hi));


  if FSingle_Zero <> AOther.FSingle_Zero then
    raise EFieldFailError.Create('FSingle_Zero', FloatToStr(FSingle_Zero), FloatToStr(AOther.FSingle_Zero));

  if FSingle_Lo <> AOther.FSingle_Lo then
    raise EFieldFailError.Create('FSingle_Lo', FloatToStr(FSingle_Lo), FloatToStr(AOther.FSingle_Lo));

  if FSingle_Hi <> AOther.FSingle_Hi then
    raise EFieldFailError.Create('FSingle_Hi', FloatToStr(FSingle_Hi), FloatToStr(AOther.FSingle_Hi));


  if FDouble_Zero <> AOther.FDouble_Zero then
    raise EFieldFailError.Create('FDouble_Zero', FloatToStr(FDouble_Zero), FloatToStr(AOther.FDouble_Zero));

  if FDouble_Lo <> AOther.FDouble_Lo then
    raise EFieldFailError.Create('FDouble_Lo', FloatToStr(FDouble_Lo), FloatToStr(AOther.FDouble_Lo));

  if FDouble_Hi <> AOther.FDouble_Hi then
    raise EFieldFailError.Create('FDouble_Hi', FloatToStr(FDouble_Hi), FloatToStr(AOther.FDouble_Hi));


  if FExtended_Zero <> AOther.FExtended_Zero then
    raise EFieldFailError.Create('FExtended_Zero', FloatToStr(FExtended_Zero), FloatToStr(AOther.FExtended_Zero));

  if FExtended_Lo <> AOther.FExtended_Lo then
    raise EFieldFailError.Create('FExtended_Lo', FloatToStr(FExtended_Lo), FloatToStr(AOther.FExtended_Lo));

  if FExtended_Hi <> AOther.FExtended_Hi then
    raise EFieldFailError.Create('FExtended_Hi', FloatToStr(FExtended_Hi), FloatToStr(AOther.FExtended_Hi));


  if FCurrency_Zero <> AOther.FCurrency_Zero then
    raise EFieldFailError.Create('FCurrency_Zero', FloatToStr(FCurrency_Zero), FloatToStr(AOther.FCurrency_Zero));

  if FCurrency_Lo <> AOther.FCurrency_Lo then
    raise EFieldFailError.Create('FCurrency_Lo', FloatToStr(FCurrency_Lo), FloatToStr(AOther.FCurrency_Lo));

  if FCurrency_Hi <> AOther.FCurrency_Hi then
    raise EFieldFailError.Create('FCurrency_Hi', FloatToStr(FCurrency_Hi), FloatToStr(AOther.FCurrency_Hi));


  if FComp_Zero <> AOther.FComp_Zero then
    raise EFieldFailError.Create('FComp_Zero', FloatToStr(FComp_Zero), FloatToStr(AOther.FComp_Zero));

  if FComp_Lo <> AOther.FComp_Lo then
    raise EFieldFailError.Create('FComp_Lo', FloatToStr(FComp_Lo), FloatToStr(AOther.FComp_Lo));

  if FComp_Hi <> AOther.FComp_Hi then
    raise EFieldFailError.Create('FComp_Hi', FloatToStr(FComp_Hi), FloatToStr(AOther.FComp_Hi));

end;

class function TFloatsRecord.Create: TFloatsRecord;
begin
  Result.FSingle_Zero := 0;
  Result.FSingle_Lo := -1000.7878;
  Result.FSingle_Hi := 10000.733;

  Result.FHalf_Zero := 0;
  Result.FHalf_Lo := -1000.7878;
  Result.FHalf_Hi := 10000.733;

  Result.FDouble_Zero := 0;
  Result.FDouble_Lo := MinSingle - 100;
  Result.FDouble_Hi := MaxSingle + 100;

  Result.FExtended_Zero := 0;
  Result.FExtended_Lo := MinDouble - 100;
  Result.FExtended_Hi := MaxDouble + 100;

  Result.FCurrency_Zero := 0;
  Result.FCurrency_Lo := -289892.3232;
  Result.FCurrency_Hi := 37889881.32322;

  Result.FComp_Zero := 0;
  Result.FComp_Lo := -2789788728;
  Result.FComp_Hi := 2121212121;
end;

{ TIntsRecord }

procedure TIntsRecord.CompareTo(const AOther: TIntsRecord);
begin
  if FByte_Zero <> AOther.FByte_Zero then
    raise EFieldFailError.Create('FByte_Zero', UIntToStr(FByte_Zero), UIntToStr(AOther.FByte_Zero));

  if FByte_Lo <> AOther.FByte_Lo then
    raise EFieldFailError.Create('FByte_Lo', UIntToStr(FByte_Lo), UIntToStr(AOther.FByte_Lo));

  if FByte_Hi <> AOther.FByte_Hi then
    raise EFieldFailError.Create('FByte_Hi', UIntToStr(FByte_Hi), UIntToStr(AOther.FByte_Hi));


  if FShortInt_Zero <> AOther.FShortInt_Zero then
    raise EFieldFailError.Create('FShortInt_Zero', IntToStr(FShortInt_Zero), IntToStr(AOther.FShortInt_Zero));

  if FShortInt_Lo <> AOther.FShortInt_Lo then
    raise EFieldFailError.Create('FShortInt_Lo', IntToStr(FShortInt_Lo), IntToStr(AOther.FShortInt_Lo));

  if FShortInt_Hi <> AOther.FShortInt_Hi then
    raise EFieldFailError.Create('FShortInt_Hi', IntToStr(FShortInt_Hi), IntToStr(AOther.FShortInt_Hi));


  if FWord_Zero <> AOther.FWord_Zero then
    raise EFieldFailError.Create('FWord_Zero', UIntToStr(FWord_Zero), UIntToStr(AOther.FWord_Zero));

  if FWord_Lo <> AOther.FWord_Lo then
    raise EFieldFailError.Create('FWord_Lo', UIntToStr(FWord_Lo), UIntToStr(AOther.FWord_Lo));

  if FWord_Hi <> AOther.FWord_Hi then
    raise EFieldFailError.Create('FWord_Hi', UIntToStr(FWord_Hi), UIntToStr(AOther.FWord_Hi));


  if FSmallInt_Zero <> AOther.FSmallInt_Zero then
    raise EFieldFailError.Create('FSmallInt_Zero', IntToStr(FSmallInt_Zero), IntToStr(AOther.FSmallInt_Zero));

  if FSmallInt_Lo <> AOther.FSmallInt_Lo then
    raise EFieldFailError.Create('FSmallInt_Lo', IntToStr(FSmallInt_Lo), IntToStr(AOther.FSmallInt_Lo));

  if FSmallInt_Hi <> AOther.FSmallInt_Hi then
    raise EFieldFailError.Create('FSmallInt_Hi', IntToStr(FSmallInt_Hi), IntToStr(AOther.FSmallInt_Hi));


  if FCardinal_Zero <> AOther.FCardinal_Zero then
    raise EFieldFailError.Create('FCardinal_Zero', UIntToStr(FCardinal_Zero), UIntToStr(AOther.FCardinal_Zero));

  if FCardinal_Lo <> AOther.FCardinal_Lo then
    raise EFieldFailError.Create('FCardinal_Lo', UIntToStr(FCardinal_Lo), UIntToStr(AOther.FCardinal_Lo));

  if FCardinal_Hi <> AOther.FCardinal_Hi then
    raise EFieldFailError.Create('FCardinal_Hi', UIntToStr(FCardinal_Hi), UIntToStr(AOther.FCardinal_Hi));


  if FInteger_Zero <> AOther.FInteger_Zero then
    raise EFieldFailError.Create('FInteger_Zero', IntToStr(FInteger_Zero), IntToStr(AOther.FInteger_Zero));

  if FInteger_Lo <> AOther.FInteger_Lo then
    raise EFieldFailError.Create('FInteger_Lo', IntToStr(FInteger_Lo), IntToStr(AOther.FInteger_Lo));

  if FInteger_Hi <> AOther.FInteger_Hi then
    raise EFieldFailError.Create('FInteger_Hi', IntToStr(FInteger_Hi), IntToStr(AOther.FInteger_Hi));


  if FUInt64_Zero <> AOther.FUInt64_Zero then
    raise EFieldFailError.Create('FUInt64_Zero', UIntToStr(FUInt64_Zero), UIntToStr(AOther.FUInt64_Zero));

  if FUInt64_Lo <> AOther.FUInt64_Lo then
    raise EFieldFailError.Create('FUInt64_Lo', UIntToStr(FUInt64_Lo), UIntToStr(AOther.FUInt64_Lo));

  if FUInt64_Hi <> AOther.FUInt64_Hi then
    raise EFieldFailError.Create('FUInt64_Hi', UIntToStr(FUInt64_Hi), UIntToStr(AOther.FUInt64_Hi));


  if FInt64_Zero <> AOther.FInt64_Zero then
    raise EFieldFailError.Create('FInt64_Zero', IntToStr(FInt64_Zero), IntToStr(AOther.FInt64_Zero));

  if FInt64_Lo <> AOther.FInt64_Lo then
    raise EFieldFailError.Create('FInt64_Lo', IntToStr(FInt64_Lo), IntToStr(AOther.FInt64_Lo));

  if FInt64_Hi <> AOther.FInt64_Hi then
    raise EFieldFailError.Create('FInt64_Hi', IntToStr(FInt64_Hi), IntToStr(AOther.FInt64_Hi));
end;

class function TIntsRecord.Create: TIntsRecord;
begin
  Result.FByte_Zero := 0;
  Result.FByte_Lo := Low(Byte);
  Result.FByte_Hi := High(Byte);

  Result.FShortInt_Zero := 0;
  Result.FShortInt_Lo := Low(ShortInt);
  Result.FShortInt_Hi := High(ShortInt);

  Result.FWord_Zero := 0;
  Result.FWord_Lo := Low(Word);
  Result.FWord_Hi := High(Word);

  Result.FSmallInt_Zero := 0;
  Result.FSmallInt_Lo := Low(SmallInt);
  Result.FSmallInt_Hi := High(SmallInt);

  Result.FCardinal_Zero := 0;
  Result.FCardinal_Lo := Low(Cardinal);
  Result.FCardinal_Hi := High(Cardinal);

  Result.FInteger_Zero := 0;
  Result.FInteger_Lo := Low(Integer);
  Result.FInteger_Hi := High(Integer);

  Result.FUInt64_Zero := 0;
  Result.FUInt64_Lo := Low(UInt64);
  Result.FUInt64_Hi := High(UInt64);

  Result.FInt64_Zero := 0;
  Result.FInt64_Lo := Low(Int64);
  Result.FInt64_Hi := High(Int64);
end;

{ TStringsRecord }

procedure TStringsRecord.CompareTo(const AOther: TStringsRecord);
begin
  if FShortString_Empty <> AOther.FShortString_Empty then
    raise EFieldFailError.Create('FShortString_Empty', string(FShortString_Empty), string(AOther.FShortString_Empty));

  if FShortString_One <> AOther.FShortString_One then
    raise EFieldFailError.Create('FShortString_One', string(FShortString_One), string(AOther.FShortString_One));

  if FShortString_Long <> AOther.FShortString_Long then
    raise EFieldFailError.Create('FShortString_Long', string(FShortString_Long), string(AOther.FShortString_Long));


  if FUnicodeString_Empty <> AOther.FUnicodeString_Empty then
    raise EFieldFailError.Create('FUnicodeString_Empty', FUnicodeString_Empty, AOther.FUnicodeString_Empty);

  if FUnicodeString_One <> AOther.FUnicodeString_One then
    raise EFieldFailError.Create('FUnicodeString_One', FUnicodeString_One, AOther.FUnicodeString_One);

  if FUnicodeString_Long <> AOther.FUnicodeString_Long then
    raise EFieldFailError.Create('FUnicodeString_Long', FUnicodeString_Long, AOther.FUnicodeString_Long);


  if FAnsiString_Empty <> AOther.FAnsiString_Empty then
    raise EFieldFailError.Create('FAnsiString_Empty', string(FAnsiString_Empty), string(AOther.FAnsiString_Empty));

  if FAnsiString_One <> AOther.FAnsiString_One then
    raise EFieldFailError.Create('FAnsiString_One', string(FAnsiString_One), string(AOther.FAnsiString_One));

  if FAnsiString_Long <> AOther.FAnsiString_Long then
    raise EFieldFailError.Create('FAnsiString_Long', string(FAnsiString_Long), string(AOther.FAnsiString_Long));


  if FWideString_Empty <> AOther.FWideString_Empty then
    raise EFieldFailError.Create('FWideString_Empty', FWideString_Empty, AOther.FWideString_Empty);

  if FWideString_One <> AOther.FWideString_One then
    raise EFieldFailError.Create('FWideString_One', FWideString_One, AOther.FWideString_One);

  if FWideString_Long <> AOther.FWideString_Long then
    raise EFieldFailError.Create('FWideString_Long', FWideString_Long, AOther.FWideString_Long);

  if FRawByteString_Empty <> AOther.FRawByteString_Empty then
    raise EFieldFailError.Create('FRawByteString_Empty', FRawByteString_Empty, AOther.FRawByteString_Empty);

  if FRawByteString_One <> AOther.FRawByteString_One then
    raise EFieldFailError.Create('FRawByteString_One', FRawByteString_One, AOther.FRawByteString_One);

  if FRawByteString_Long <> AOther.FRawByteString_Long then
    raise EFieldFailError.Create('FRawByteString_Long', FRawByteString_Long, AOther.FRawByteString_Long);


  if UCS4StringToWideString(FUcs4String_Empty) <> UCS4StringToWideString(AOther.FUcs4String_Empty) then
    raise EFieldFailError.Create('FUcs4String_Empty', UCS4StringToWideString(FUcs4String_Empty), UCS4StringToWideString(AOther.FUcs4String_Empty));

  if UCS4StringToWideString(FUcs4String_One) <> UCS4StringToWideString(AOther.FUcs4String_One) then
    raise EFieldFailError.Create('FUcs4String_One', UCS4StringToWideString(FUcs4String_One), UCS4StringToWideString(AOther.FUcs4String_One));

  if UCS4StringToWideString(FUcs4String_Long) <> UCS4StringToWideString(AOther.FUcs4String_Long) then
    raise EFieldFailError.Create('FUcs4String_Long', UCS4StringToWideString(FUcs4String_Long), UCS4StringToWideString(AOther.FUcs4String_Long));


  if FAnsiChar_Zero <> AOther.FAnsiChar_Zero then
    raise EFieldFailError.Create('FAnsiChar_Zero', string(FAnsiChar_Zero), string(AOther.FAnsiChar_Zero));

  if FAnsiChar_Lo <> AOther.FAnsiChar_Lo then
    raise EFieldFailError.Create('FAnsiChar_Lo', string(FAnsiChar_Lo), string(AOther.FAnsiChar_Lo));

  if FAnsiChar_Hi <> AOther.FAnsiChar_Hi then
    raise EFieldFailError.Create('FAnsiChar_Hi', string(FAnsiChar_Hi), string(AOther.FAnsiChar_Hi));


  if FWideChar_Zero <> AOther.FWideChar_Zero then
    raise EFieldFailError.Create('FWideChar_Zero', FWideChar_Zero, AOther.FWideChar_Zero);

  if FWideChar_Lo <> AOther.FWideChar_Lo then
    raise EFieldFailError.Create('FWideChar_Lo', FWideChar_Lo, AOther.FWideChar_Lo);

  if FWideChar_Hi <> AOther.FWideChar_Hi then
    raise EFieldFailError.Create('FWideChar_Hi', FWideChar_Hi, AOther.FWideChar_Hi);


  if FUcs4Char_Zero <> AOther.FUcs4Char_Zero then
    raise EFieldFailError.Create('FUcs4Char_Zero', IntToStr(FUcs4Char_Zero), IntToStr(AOther.FUcs4Char_Zero));

  if FUcs4Char_Lo <> AOther.FUcs4Char_Lo then
    raise EFieldFailError.Create('FUcs4Char_Lo', IntToStr(FUcs4Char_Lo), IntToStr(AOther.FUcs4Char_Lo));

  if FUcs4Char_Hi <> AOther.FUcs4Char_Hi then
    raise EFieldFailError.Create('FUcs4Char_Hi', IntToStr(FUcs4Char_Hi), IntToStr(AOther.FUcs4Char_Hi));
end;

class function TStringsRecord.Create: TStringsRecord;
begin
  Result.FShortString_Empty := '';
  Result.FShortString_One := '1';
  Result.FShortString_Long := 'Testing Serialization!';

  Result.FUnicodeString_Empty := '';
  Result.FUnicodeString_One := '2';
  Result.FUnicodeString_Long := 'тестинг сериализэйшан!';

  Result.FAnsiString_Empty := '';
  Result.FAnsiString_One := '3';
  Result.FAnsiString_Long := 'Ansi Testing Serialization!';

  Result.FWideString_Empty := '';
  Result.FWideString_One := '4';
  Result.FWideString_Long := 'Re-тестинг сериализэйшан!';

  Result.FUcs4String_Empty := WideStringToUCS4String('');
  Result.FUcs4String_One := WideStringToUCS4String('4');
  Result.FUcs4String_Long := WideStringToUCS4String('Re-тестинг сериализэйшан!');

  Result.FRawByteString_Empty := '';
  Result.FRawByteString_One := '4';
  Result.FRawByteString_Long := 'A block of data';

  Result.FAnsiChar_Zero := #9;
  Result.FAnsiChar_Lo := ' ';
  Result.FAnsiChar_Hi := 'z';

  Result.FWideChar_Zero := #9;
  Result.FWideChar_Lo := ' ';
  Result.FWideChar_Hi := #$FF00;

  Result.FUcs4Char_Zero := 9;
  Result.FUcs4Char_Lo := 32;
  Result.FUcs4Char_Hi := $FF00;
end;

{ EFieldFailError }

constructor EFieldFailError.Create(const AFieldName, AExp, AActual: String);
begin
  inherited CreateFmt('Field [%s]. Expected = "%s" but actual was "%s".', [AFieldName, AExp, AActual]);
end;


end.
