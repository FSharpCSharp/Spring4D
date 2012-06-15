{*******************************************************}
{                                                       }
{       Lightweight object oriented string type         }
{                                                       }
{       Copyright (C) 2011 "Linas Naginionis"           }
{                                                       }
{*******************************************************}

unit SvStrings;

{$I sv.inc}
interface

uses
  SysUtils, Generics.Collections, Classes;

type
  {$WARNINGS OFF}
  TWideCharSet = set of Char;
  {$WARNINGS ON}

  TStringCompressionLevel = (scNone, scFastest, scDefault, scMax);

  TStringComparison =
  (
    ///  <summary>The comparison is perfomed using current locale rules.</summary>
    scLocale,
    ///  <summary>The comparison is perfomed using current locale rules and is case-independant.</summary>
    scLocaleIgnoreCase,
    ///  <summary>The comparison is perfomed using Unicode rules.</summary>
    scInvariant,
    ///  <summary>The comparison is perfomed using Unicode rules and is case-independant.</summary>
    scInvariantIgnoreCase,
    ///  <summary>The comparison is perfomed using Unicode character tables.</summary>
    scOrdinal,
    ///  <summary>The comparison is perfomed using Unicode character tables and is case-independant.</summary>
    scOrdinalIgnoreCase
  );

  TSvStringFormatSettings = class
  private
    class var
      FFmtSettings: TFormatSettings;
  strict private
    class constructor Create();

  public
    class procedure Apply(const Value: TFormatSettings);

    class function Get(): TFormatSettings;
  end;

  PSvString = ^TSvString;

  /// <summary>
  ///  TSvString is mutable type but it also contains functions that could be treated as immutable.
  ///  These functions have prefix F at the end of the name. They won't change the current value,
  ///  just return the new one.
  /// </summary>
  TSvString = record
  private type
    { The enumerator object }
    TEnumerator = class(TEnumerator<Char>)
    private
      FString: string;
      FIndex: NativeInt;
      FCurrent: Char;
    protected
      function DoGetCurrent: Char; override;
      function DoMoveNext: Boolean; override;
    public
      { Constructor }
      constructor Create(const AString: string);
    end;
  private const
    CEmpty = '';

    { Defines the position of the first character in the string }
    {$IFDEF TSTRING_ZERO_INDEXED}
        CFirstCharacterIndex = 0;
    {$ELSE}
        CFirstCharacterIndex = 1;
    {$ENDIF}
  private
    FValue: string;
    function GetLength: Integer;
    function GetChar(const AIndex: NativeInt): Char;
    class function InternalCompare(const ALeft, ARight: PWideChar; const MaxLen: NativeUInt; const LType: TStringComparison): NativeInt; static;
    function GetIsEmpty: Boolean;
    function GetIsWhiteSpace: Boolean;
  public
    class operator Implicit(const AString: TSvString): string; inline;

    class operator Implicit(const AString: String): TSvString; inline;

    class operator Implicit(const AString: AnsiString): TSvString; inline;

    class operator Implicit(const AString: PSvString): TSvString; inline;

    class operator Implicit(const AString: TSvString): Variant; inline;

    class operator Implicit(const AValue: Variant): TSvString; inline;

    class operator Explicit(const AString: String): TSvString; inline;

    class operator Explicit(const AString: TSvString): string; inline;

    class operator Add(const ALeft, ARight: TSvString): TSvString; inline;

    class operator Add(const ALeft: TSvString; const ARight: string): TSvString; inline;

    class operator Add(const ALeft: string; const ARight: TSvString): TSvString; inline;

    class operator Equal(const ALeft: TSvString; const ARight: TSvString): Boolean; inline;

    class operator NotEqual(const ALeft: TSvString; const ARight: TSvString): Boolean; inline;

    function GetEnumerator: TEnumerator;

    property Chars[const AIndex: NativeInt]: Char read GetChar; default;

    property IsEmpty: Boolean read GetIsEmpty;
    property IsNullOrEmpty: Boolean read GetIsEmpty;
    property IsWhiteSpace: Boolean read GetIsWhiteSpace;
    /// <summary>
    /// string's length
    /// </summary>
    property Length: Integer read GetLength;
    /// <summary>
    /// string's value
    /// </summary>
    property Value: string read FValue write FValue;
    /// <summary>
    /// Quote string with default quote symbol
    /// </summary>
    procedure AddQuotes(); overload; inline;
    /// <summary>
    /// Quote string with specified quote symbol
    /// </summary>
    /// <param name="AQuote"></param>
    procedure AddQuotes(const AQuote: Char); overload; inline;

    function AddQuotesF(): TSvString; overload; inline;
    function AddQuotesF(const AQuote: Char): TSvString; overload; inline;
    /// <summary>
    /// Make a clone of the current string
    /// </summary>
    /// <returns></returns>
    function Clone(): TSvString;
    /// <summary>
    /// Compares two specified String objects and returns an integer that indicates their relative position in the sort order.
    /// </summary>
    /// <param name="ALeft">The first string to compare</param>
    /// <param name="ARight">The second string to compare</param>
    /// <param name="ACompareOption"></param>
    /// <returns>A 32-bit signed integer that indicates the lexical relationship between the two comparands.</returns>
    class function Compare(const ALeft, ARight: string; const ACompareOption: TStringComparison = scInvariant): NativeInt; static; inline;

    function CompareTo(const AString: string; const ACompareOption: TStringComparison = scInvariant): NativeInt; inline;
    /// <summary>
    /// Compress string using ZLib algorythm
    /// </summary>
    /// <param name="ACompressionLevel">Compression level to use while compressing</param>
    procedure Compress(ACompressionLevel: TStringCompressionLevel = scDefault);
    /// <summary>
    /// Decompress string using ZLib algorythm
    /// </summary>
    procedure Decompress();

    function CompressF(ACompressionLevel: TStringCompressionLevel = scDefault): TSvString; inline;
    function DecompressF(): TSvString; inline;
    /// <summary>
    /// Concatenates specified instances of String.
    /// </summary>
    /// <param name="AString">string to concatenate</param>
    procedure Concat(const AString: string); overload; inline;
    procedure Concat(const AString1, AString2: string); overload; inline;
    procedure Concat(const AString1, AString2, AString3: string); overload; inline;
    procedure Concat(const AString1, AString2, AString3, AString4: string); overload; inline;
    procedure Concat(const AStrings: array of string); overload;
    procedure Concat(const AString: TSvString); overload; inline;

    function ConcatF(const AString: string): TSvString; overload; inline;
    function ConcatF(const AString1, AString2: string): TSvString; overload; inline;
    function ConcatF(const AString1, AString2, AString3: string): TSvString; overload; inline;
    function ConcatF(const AString1, AString2, AString3, AString4: string): TSvString; overload; inline;
    function ConcatF(const AStrings: array of string): TSvString; overload;
    function ConcatF(const AString: TSvString): TSvString; overload; inline;

    function Contains(const AWhat: string; const ACompareOption: TStringComparison = scInvariant): Boolean; inline;
    /// <summary>
    /// Duplicate string specified number of times
    /// </summary>
    /// <param name="ACount">How many times duplicate value</param>
    procedure Dupe(const ACount: Integer); inline;

    function DupeF(const ACount: Integer): TSvString; inline;
    /// <summary>
    /// Checks if strings are equal using OS API functions
    /// </summary>
    /// <param name="AString">string to test</param>
    /// <param name="ACompareOption">Compare options</param>
    /// <returns>Boolean</returns>
    function Equals(const AString: string; const ACompareOption: TStringComparison = scInvariant): Boolean; inline;
    /// <summary>
    /// Checks if strings are equal using standard Delphi functions. Much faster in the tests.
    /// </summary>
    /// <param name="AString">string to test</param>
    /// <param name="ACompareOption">Compare options</param>
    /// <returns>Boolean</returns>
    function EqualsEx(const AString: string; const ACompareOption: TStringComparison = scInvariant): Boolean; inline;
    /// <summary>
    /// Escape string with HTML rules
    /// </summary>
    procedure HtmlEscape();
    function HtmlEscapeF(): string;
    /// <summary>
    /// Make a string with specified values separated by specified separator
    /// </summary>
    /// <param name="ASeparator">Separator value</param>
    /// <param name="AStrings">Strings to join</param>
    procedure Join(const ASeparator: string; const AStrings: array of string);

    function JoinF(const ASeparator: string; const AStrings: array of string): TSvString;
    /// <summary>
    /// Insert given string into the specified position
    /// </summary>
    /// <param name="AIndex">Index where to insert</param>
    /// <param name="AWhat">What value insert</param>
    procedure Insert(const AIndex: NativeInt; const AWhat: string); inline;

    function InsertF(const AIndex: NativeInt; const AWhat: string): TsvString; inline;

    /// <summary>
    /// Very fast index position retrieval from given substring. Uses standard Delphi PosEx function
    /// </summary>
    /// <param name="AWhat">string to search</param>
    /// <param name="ACompareOption">Compare option to use</param>
    /// <param name="AOffset">From what position begin searching</param>
    /// <returns>index if found, 0 if not found</returns>
    function Pos(const AWhat: string; const ACompareOption: TStringComparison = scInvariant; const AOffset: Integer = 1): Integer; inline;
    /// <summary>
    /// Gets index of specified string in the current value. Iterates through all the elements, can be slower then Pos
    /// </summary>
    /// <param name="AWhat">What to search</param>
    /// <param name="ACompareOption"></param>
    /// <returns>Index of found string</returns>
    function IndexOf(const AWhat: string; const ACompareOption: TStringComparison = scInvariant): NativeInt; inline;
    function LastIndexOf(const AWhat: string; const ACompareOption: TStringComparison = scInvariant): NativeInt; inline;
    function IndexOfAny(const AWhat: array of string; const ACompareOption: TStringComparison = scInvariant): NativeInt;
    function LastIndexOfAny(const AWhat: array of string; const ACompareOption: TStringComparison = scInvariant): NativeInt;

    function ToCharArray: TArray<Char>;
    /// <summary>
    /// Formats value
    /// </summary>
    /// <param name="AFormat">Format string</param>
    /// <param name="AParams">Format parameters</param>
    procedure Format(const AFormat: string; const AParams: array of const); overload;

    procedure Format(const AFormat: string; const AParams: array of const;
      const AFormatSettings: TFormatSettings); overload;

    class function FormatF(const AFormat: string; const AParams: array of const): TSvString; overload; static;

    class function FormatF(const AFormat: string; const AParams: array of const;
      const AFormatSettings: TFormatSettings): TSvString; overload; static;
    /// <summary>
    /// Load value from stream
    /// </summary>
    /// <param name="AStream">Stream object</param>
    procedure LoadFromStream(AStream: TStream);
    /// <summary>
    /// Load value from file
    /// </summary>
    /// <param name="AFilename">Filename</param>
    procedure LoadFromFile(const AFilename: string);
    /// <summary>
    /// Saves string value to file
    /// </summary>
    /// <param name="AFilename">Filename to save</param>
    procedure SaveToFile(const AFilename: string);
    /// <summary>
    /// Saves value to stream
    /// </summary>
    /// <param name="AStream">Stream object</param>
    procedure SaveToStream(AStream: TStream);
    /// <summary>
    /// Get Soundex value of current string
    /// </summary>
    /// <param name="ALength">Soundex length</param>
    /// <returns>string</returns>
    function Soundex(const ALength: Integer = 4): string;
    function SoundexSimilar(const AWith: string; const ALength: Integer = 4): Boolean;
    /// <summary>
    /// Calculates MD5 hash of the current value
    /// </summary>
    procedure MD5();
    function MD5F(): string;

    procedure RandomFrom(const AValues: array of string);

    class function RandomFromF(const AValues: array of string): TSvString; static;

    /// <summary>
    /// Removes values at specified indexes
    /// </summary>
    /// <param name="AStart">Start index</param>
    procedure Remove(const AStart: NativeInt); overload;
    procedure Remove(const AStart: NativeInt; const ACount: NativeUInt); overload;

    function RemoveF(const AStart: NativeInt): TsvString; overload; inline;
    function RemoveF(const AStart: NativeInt; const ACount: NativeUInt): TsvString; overload; inline;

    procedure Replace(const AWhat, AWith: Char); overload; inline;
    procedure Replace(const AWhat, AWith: string; const ACompareOption: TStringComparison = scInvariant); overload; inline;

    function ReplaceF(const AWhat, AWith: Char): TSvString; overload;
    function ReplaceF(const AWhat, AWith: string; const ACompareOption: TStringComparison = scInvariant): TSvString; overload; inline;

    /// <summary>
    /// Reverses current value
    /// </summary>
    procedure Reverse(); inline;
    procedure ReverseInvariant(); inline;

    function ReverseF(): TSvString; inline;
    function ReverseInvariantF(): TSvString; inline;

    /// <summary>
    /// Splits current value into new array where each new element is separated by ADelimiter value
    /// </summary>
    /// <param name="ADelimiter">Separator value</param>
    /// <param name="ARemoveEmptyEntries">Should remove empty entries?</param>
    /// <returns>Array of string</returns>
    function Split(const ADelimiter: Char; const ARemoveEmptyEntries: Boolean = false): TArray<string>; overload; inline;

    function Split(const ADelimiters: TWideCharSet; const ARemoveEmptyEntries: Boolean = false): TArray<string>; overload; inline;

    function StartsWith(const AWhat: string; const ACompareOption: TStringComparison = scInvariant): Boolean;
    /// <summary>
    /// Gets substring copy of the current value from start index with ACount length
    /// </summary>
    /// <param name="AStart">Start index of the new substring value</param>
    /// <param name="ACount">Length of the substring</param>
    /// <returns>New string value </returns>
    function Substring(const AStart: NativeInt; const ACount: NativeUInt): TSvString; overload;

    function Substring(const AStart: NativeInt): TSvString; overload;
    /// <summary>
    /// Left-aligns the characters in this instance by padding them with spaces on the right, for a specified total length.
    /// </summary>
    /// <param name="ACount">Length to pad</param>
    /// <param name="AChar">Char to add</param>
    procedure PadLeft(const ACount: NativeInt; const AChar: Char = ' ' ); inline;
    /// <summary>
    /// Right-aligns the characters in this instance by padding them with spaces on the left, for a specified total length.
    /// </summary>
    /// <param name="ACount">Length to pad</param>
    /// <param name="AChar">Char to add</param>
    procedure PadRight(const ACount: NativeInt; const AChar: Char = ' '); inline;

    function PadLeftF(const ACount: NativeInt; const AChar: Char = ' ' ): TSvString; inline;
    function PadRightF(const ACount: NativeInt; const AChar: Char = ' ' ): TSvString; inline;

 {   function Substring(const AStart: NativeInt; const ACount: NativeUInt): string; overload;

    function Substring(const AStart: NativeInt): string; overload;  }
    {TODO -oLinas -cGeneral : From value type conversion routines}
    {TODO -oLinas -cGeneral : Encrypt functions}
    function ToBool(): Boolean; overload; inline;
    function ToBool(const ADef: Boolean): Boolean; overload; inline;
    function ToBytes(): TBytes;
    function ToDate(): TDate; overload; inline;
    function ToDate(const AFormatSettings: TFormatSettings): TDate; overload; inline;
    function ToDate(const AFormatSettings: TFormatSettings; const ADef: TDate): TDate; overload; inline;
    function ToDate(const ADef: TDate): TDate; overload; inline;
    function ToDateTime(): TDateTime; overload; inline;
    function ToDateTime(const AFormatSettings: TFormatSettings): TDateTime; overload; inline;
    function ToDateTime(const AFormatSettings: TFormatSettings; const ADef: TDateTime): TDateTime; overload; inline;
    function ToDateTime(const ADef: TDateTime): TDateTime; overload; inline;
    function ToDouble(): Double; overload; inline;
    function ToDouble(const ADef: Double): Double; overload; inline;
    function ToDouble(const AFormatSettings: TFormatSettings; const ADef: Double): Double; overload; inline;
    function ToInt(): Integer; overload; inline;
    function ToInt(const ADef: Integer): Integer; overload; inline;
    function ToNativeInt(): NativeInt; overload; inline;
    function ToNativeInt(const ADef: NativeInt): NativeInt; overload; inline;
    function ToInt64(): Int64; overload; inline;
    function ToInt64(const ADef: Int64): Int64; overload; inline;
    function ToString(): string; inline;
    function ToSqlString(): string; inline;
    function ToHex(): string;
    function ToBin(): string;
    ///  <summary>Converts this string to an UTF-8 encoded string.</summary>
    ///  <returns>A <c>RawByteString</c> containing the converted value.</returns>
    function ToUTF8String(): RawByteString; inline;
    ///  <summary>Converts this string to an UTF-32 encoded string.</summary>
    ///  <returns>An <c>UCS4String</c> containing the converted value.</returns>
    function ToUCS4String(): UCS4String; inline;

    function ToPchar(): PChar; inline;

    procedure FromBool(const AValue: Boolean); inline;
    procedure FromBytes(const AValue: TBytes); inline;
    procedure FromDate(const AValue: TDate); overload; inline;
    procedure FromDate(const AValue: TDate; const AFormatSettings: TFormatSettings); overload; inline;
    procedure FromDateTime(const AValue: TDateTime); overload; inline;
    procedure FromDateTime(const AValue: TDateTime; const AFormatSettings: TFormatSettings); overload; inline;
    procedure FromDouble(const AValue: Double); overload; inline;
    procedure FromDouble(const AValue: Double; const AFormatSettings: TFormatSettings); overload; inline;
    procedure FromInt(const AValue: Integer); inline;
    procedure FromInt64(const AValue: Int64); inline;

    procedure Trim(); overload; inline;
    procedure Trim(const ACharSet: TWideCharSet); overload; inline;
    procedure TrimLeft(); overload; inline;
    procedure TrimLeft(const ACharSet: TWideCharSet); overload; inline;
    procedure TrimLeft(const ASymbolsCount: Integer); overload; inline;
    procedure TrimRight(); overload; inline;
    procedure TrimRight(const ACharSet: TWideCharSet); overload; inline;
    procedure TrimRight(const ASymbolsCount: Integer); overload; inline;

    function TrimF(): TSvString; overload; inline;
    function TrimF(const ACharSet: TWideCharSet): TSvString; overload; inline;
    function TrimLeftF(): TSvString; overload; inline;
    function TrimLeftF(const ACharSet: TWideCharSet): TSvString; overload; inline;
    function TrimLeftF(const ASymbolsCount: Integer): TSvString; overload; inline;
    function TrimRightF(): TSvString; overload; inline;
    function TrimRightF(const ACharSet: TWideCharSet): TSvString; overload; inline;
    function TrimRightF(const ASymbolsCount: Integer): TSvString; overload; inline;

    procedure ToUpper(); inline;
    procedure ToUpperInvariant(); inline;
    procedure ToLower(); inline;
    procedure ToLowerInvariant; inline;

    function ToUpperF(): TSvString; inline;
    function ToUpperInvariantF(): TSvString; inline;
    function ToLowerF(): TSvString; inline;
    function ToLowerInvariantF(): TSvString; inline;

    class function ValueOf(const AVar: Variant): TSvString; static; inline;
  end;

  /// <summary>
  ///  Create a TSvString from Delphi's native string
  /// </summary>
  /// <param name="AString">string to convert</param>
  /// <returns>New TSvString value</returns>
  /// <remarks>
  /// Useful for writing expressions: (e.g. <c>F('Some String').ToUpper().Trim()</c>)
  /// </remarks>
  function F(const AString: string): TSvString; inline;

implementation

uses
  StrUtils,
  Character,
  //MessageDigest_5,
  IdGlobal,
  idHash,
  IdHashMessageDigest,
  Types,
  HTTPUtil,
  ZLib
  {$IF DEFINED(MSWINDOWS)}
  ,Windows
  {$IFEND};



type
  TStringCompareProc = function(const ALeft, ARight: PWideChar; const MaxLen: NativeUInt): NativeInt;

var
  { Used for comparison utilities }
  FStrCompareFuncs: array[TStringComparison] of TStringCompareProc;


function F(const AString: string): TSvString; inline;
begin
  Result.FValue := AString;
end;

{
   These bridge functions are required to properly call the SysUtils wide
   versions.
}
function __LocaleCaseSensitive(const ALeft, ARight: PWideChar; const MaxLen: NativeUInt): NativeInt;
begin
  Result := SysUtils.AnsiStrLComp(ALeft, ARight, MaxLen);
end;

function __LocaleCaseInsensitive(const ALeft, ARight: PWideChar; const MaxLen: NativeUInt): NativeInt;
begin
  Result := SysUtils.AnsiStrLIComp(ALeft, ARight, MaxLen);
end;

function __InvariantCaseSensitive(const ALeft, ARight: PWideChar; const MaxLen: NativeUInt): NativeInt;
begin
  Result := SysUtils.StrLComp(ALeft, ARight, MaxLen);
(*{$IF defined(MSWINDOWS)}
  Result := CompareStringW(LOCALE_INVARIANT, 0, ALeft, MaxLen, ARight, MaxLen) - CSTR_EQUAL;
{$ELSE}
  Result := SysUtils.StrLComp(ALeft, ARight, MaxLen);
{$IFEND} *)
end;

function __InvariantCaseInsensitive(const ALeft, ARight: PWideChar; const MaxLen: NativeUInt): NativeInt;
begin
  Result := SysUtils.StrLIComp(ALeft, ARight, MaxLen);
(*{$IF defined(MSWINDOWS)}
  Result := CompareStringW(LOCALE_INVARIANT, NORM_IGNORECASE, ALeft, MaxLen, ARight, MaxLen) - CSTR_EQUAL;
{$ELSE}
  Result := SysUtils.StrLIComp(ALeft, ARight, MaxLen);
{$IFEND}   *)
end;

function BinaryCompare(const ALeft, ARight: Pointer; const ASize: NativeUInt): NativeInt;
var
  LLPtr, LRPtr: Pointer;
  LLen: NativeUInt;
begin
  { Init }
  LLPtr := ALeft;
  LRPtr := ARight;
  LLen := ASize;
  Result := 0; // Equal!

  { Compare by NativeInts at first }
  while LLen > SizeOf(NativeInt) do
  begin
    { Compare left to right }
    if PNativeInt(LLPtr)^ > PNativeInt(LRPtr)^ then Exit(1)
    else if PNativeInt(LLPtr)^ < PNativeInt(LRPtr)^ then Exit(-1);

    Dec(LLen, SizeOf(NativeInt));
    Inc(PNativeInt(LLPtr));
    Inc(PNativeInt(LRPtr));
  end;

  { If there are bytes left to compare, use byte traversal }
  if LLen > 0 then
  begin
    while LLen > 0 do
    begin
      Result := PByte(LLPtr)^ - PByte(LRPtr)^;
      if Result <> 0 then
        Exit;

      Dec(LLen);
      Inc(PByte(LLPtr));
      Inc(PByte(LRPtr));
    end;
  end;
end;

function __OrdinalCaseSensitive(const ALeft, ARight: PWideChar; const MaxLen: NativeUInt): NativeInt;
begin
  { Very simple. Call the binary compare utility routine }
  Result := BinaryCompare(ALeft, ARight, MaxLen * SizeOf(WideChar));
end;

function __OrdinalCaseInsensitive(const ALeft, ARight: PWideChar; const MaxLen: NativeUInt): NativeInt;
var
  LUpLeft, LUpRight: String;
begin
  if MaxLen = 0 then
    Exit(0); // Equal for 0 length

  { Create strings }
  SetString(LUpLeft, ALeft, MaxLen);
  SetString(LUpRight, ARight, MaxLen);

  { Upper case them }
  LUpLeft := Character.ToUpper(LUpLeft);
  LUpRight := Character.ToUpper(LUpRight);

  { And finally we can compare! }
  Result := BinaryCompare(Pointer(LUpLeft), Pointer(LUpRight), MaxLen * SizeOf(WideChar));
end;

{ TSvString }

class operator TSvString.Implicit(const AString: TSvString): string;
begin
  Result := AString.FValue;
end;

class operator TSvString.Implicit(const AString: String): TSvString;
begin
  Result.FValue := AString;
end;

class operator TSvString.Add(const ALeft, ARight: TSvString): TSvString;
begin
  Result := ALeft.FValue + ARight.FValue;
end;

procedure TSvString.Concat(const AString: string);
begin
  FValue := FValue + AString;
end;

procedure TSvString.AddQuotes;
begin
  FValue := QuotedStr(FValue);
end;

class operator TSvString.Add(const ALeft: TSvString; const ARight: string): TSvString;
begin
  Result.FValue := ALeft.FValue + ARight;
end;

class operator TSvString.Add(const ALeft: string; const ARight: TSvString): TSvString;
begin
  Result.FValue := ALeft + ARight.FValue;
end;

procedure TSvString.AddQuotes(const AQuote: Char);
begin
  FValue := AnsiQuotedStr(FValue, AQuote);
end;

function TSvString.AddQuotesF: TSvString;
begin
  Result.FValue := FValue;
  Result.AddQuotes;
end;

function TSvString.AddQuotesF(const AQuote: Char): TSvString;
begin
  Result.FValue := FValue;
  Result.AddQuotes(AQuote);
end;

function TSvString.Clone: TSvString;
begin
  Result.FValue := FValue;
end;

class function TSvString.Compare(const ALeft, ARight: string;
  const ACompareOption: TStringComparison): NativeInt;
var
  LLeftLen, LRightLen: NativeInt;
begin
  { Calculate the lengths }
  LLeftLen := System.Length(ALeft);
  LRightLen := System.Length(ARight);

  { The difference }
  Result := LLeftLen - LRightLen;

  { Do a hard-core comparison if the lenghts are equal }
  if Result = 0 then
    Result := InternalCompare(PWideChar(ALeft), PWideChar(ARight), LLeftLen, ACompareOption);
end;

function TSvString.CompareTo(const AString: string;
  const ACompareOption: TStringComparison): NativeInt;
begin
  Result := Compare(FValue, AString, ACompareOption);
end;

procedure TSvString.Compress(ACompressionLevel: TStringCompressionLevel);
var
  strInput,
  strOutput: TStringStream;
  Zipper: TZCompressionStream;
begin
  strInput:= TStringStream.Create(FValue);
  strOutput:= TStringStream.Create;
  try
    Zipper:= TZCompressionStream.Create(strOutput, TZCompressionLevel(ACompressionLevel) {$IFDEF DELPHI16_UP}, 128 {$ENDIF});
    try
      Zipper.CopyFrom(strInput, strInput.Size);
    finally
      Zipper.Free;
    end;
    FValue:= strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

function TSvString.CompressF(ACompressionLevel: TStringCompressionLevel): TSvString;
begin
  Result.FValue := FValue;
  Result.Compress(ACompressionLevel);
end;

procedure TSvString.Concat(const AString: TSvString);
begin
  FValue := FValue + AString.FValue;
end;

function TSvString.ConcatF(const AString1, AString2, AString3: string): TSvString;
begin
  Result.FValue := FValue;
  Result.Concat(AString1, AString2, AString3);
end;

function TSvString.ConcatF(const AString1, AString2: string): TSvString;
begin
  Result.FValue := FValue;
  Result.Concat(AString1, AString2);
end;

function TSvString.ConcatF(const AString: string): TSvString;
begin
  Result.FValue := FValue;
  Result.Concat(AString);
end;

function TSvString.ConcatF(const AString: TSvString): TSvString;
begin
  Result.FValue := FValue;
  Result.Concat(AString);
end;

function TSvString.ConcatF(const AStrings: array of string): TSvString;
begin
  Result.FValue := FValue;
  Result.Concat(AStrings);
end;

function TSvString.ConcatF(const AString1, AString2, AString3, AString4: string): TSvString;
begin
  Result.FValue := FValue;
  Result.Concat(AString1, AString2, AString3, AString4);
end;

procedure TSvString.Concat(const AString1, AString2, AString3: string);
begin
  FValue := FValue + AString1 + AString2 + AString3;
end;

procedure TSvString.Concat(const AString1, AString2, AString3, AString4: string);
begin
  FValue := FValue + AString1 + AString2 + AString3 + AString4;
end;

procedure TSvString.Concat(const AString1, AString2: string);
begin
  FValue := FValue + AString1 + AString2;
end;

function TSvString.Contains(const AWhat: string; const ACompareOption: TStringComparison): Boolean;
begin
  Result := IndexOf(AWhat, ACompareOption) > (CFirstCharacterIndex - 1);
end;

procedure TSvString.Decompress;
var
  strInput,
  strOutput: TStringStream;
  Unzipper: TZDecompressionStream;
begin
  strInput:= TStringStream.Create(FValue);
  strOutput:= TStringStream.Create;
  try
    Unzipper:= TZDecompressionStream.Create(strInput);
    try
      strOutput.CopyFrom(Unzipper, Unzipper.Size);
    finally
      Unzipper.Free;
    end;
    FValue := strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

function TSvString.DecompressF: TSvString;
begin
  Result.FValue := FValue;
  Result.Decompress;
end;

procedure TSvString.Dupe(const ACount: Integer);
begin
  DupeString(FValue, ACount);
end;

function TSvString.DupeF(const ACount: Integer): TSvString;
begin
  Result.FValue := FValue;
  Result.Dupe(ACount);
end;

procedure TSvString.Concat(const AStrings: array of string);
var
  I: Integer;
begin
  for I := Low(AStrings) to High(AStrings) do
  begin
    Self.Concat(AStrings[i]);
  end;
end;

class operator TSvString.Equal(const ALeft, ARight: TSvString): Boolean;
begin
  Result := ALeft.FValue = ARight.FValue;
end;

function TSvString.Equals(const AString: string; const ACompareOption: TStringComparison): Boolean;
begin
  Result := (CompareTo(AString, ACompareOption) = 0);
end;

function TSvString.EqualsEx(const AString: string; const ACompareOption: TStringComparison): Boolean;
begin
  case ACompareOption of
    scLocale: Result := SameStr(AString, FValue, loUserLocale);
    scLocaleIgnoreCase: Result := SameText(AString, FValue, loUserLocale);
    scInvariant: Result := SameStr(AString, FValue, loInvariantLocale);
    scInvariantIgnoreCase: Result := SameText(AString, FValue, loInvariantLocale) ;
    scOrdinal: Result := AnsiSameStr(AString, FValue);
    scOrdinalIgnoreCase: Result := AnsiSameText(AString, FValue)
    else
    begin
      Result := False;
    end;
  end;
end;

class operator TSvString.Explicit(const AString: TSvString): string;
begin
  Result := AString.FValue;
end;

class operator TSvString.Explicit(const AString: String): TSvString;
begin
  Result.FValue := AString;
end;

procedure TSvString.Format(const AFormat: string; const AParams: array of const);
begin
  FValue := SysUtils.Format(AFormat, AParams, TSvStringFormatSettings.Get);
end;

procedure TSvString.Format(const AFormat: string; const AParams: array of const;
  const AFormatSettings: TFormatSettings);
begin
  FValue := SysUtils.Format(AFormat, AParams, AFormatSettings);
end;

class function TSvString.FormatF(const AFormat: string; const AParams: array of const): TSvString;
begin
  Result.Format(AFormat, AParams);
end;

class function TSvString.FormatF(const AFormat: string; const AParams: array of const;
  const AFormatSettings: TFormatSettings): TSvString;
begin
  Result.Format(AFormat, AParams, AFormatSettings);
end;

procedure TSvString.FromBool(const AValue: Boolean);
begin
  FValue := BoolToStr(AValue);
end;

procedure TSvString.FromBytes(const AValue: TBytes);
begin
  FValue := WideStringOf(AValue);
end;

procedure TSvString.FromDate(const AValue: TDate);
begin
  FValue := DateToStr(AValue, TSvStringFormatSettings.Get);
end;

procedure TSvString.FromDateTime(const AValue: TDateTime);
begin
  FValue := DateTimeToStr(AValue, TSvStringFormatSettings.Get);
end;

procedure TSvString.FromDouble(const AValue: Double);
begin
  FValue := FloatToStr(AValue, TSvStringFormatSettings.Get);
end;

procedure TSvString.FromInt(const AValue: Integer);
begin
  FValue := IntToStr(AValue);
end;

procedure TSvString.FromInt64(const AValue: Int64);
begin
  FValue := IntToStr(AValue);
end;

function TSvString.GetChar(const AIndex: NativeInt): Char;
var
  LIndex: NativeInt;
begin
  { Calculate the index proper }
  LIndex := AIndex + (1 - CFirstCharacterIndex);

  { Get the char }
{$IFDEF TSTRING_CHECK_RANGES}
  if (LIndex > System.Length(FValue)) or (LIndex < 1) then
    raise Exception.Create('Argument Out Of Range');
{$ENDIF}

  Result := FValue[LIndex];
end;

function TSvString.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FValue);
end;

function TSvString.GetIsEmpty: Boolean;
begin
  Result := System.Length(FValue) = 0;
end;

function TSvString.GetIsWhiteSpace: Boolean;
var
  I: NativeInt;
begin
  { Check if each char is whitespace }
  for I := 1 to System.Length(FValue) do
    if not Character.IsWhiteSpace(FValue, I) then
      Exit(false);

  { String was either empty or contained whitespaces only }
  Result := true;
end;

function TSvString.GetLength: Integer;
begin
  Result := System.Length(FValue);
end;

procedure TSvString.HtmlEscape;
begin
  FValue := HtmlEscapeF();
end;

function TSvString.HtmlEscapeF: string;
begin
  Result := HTTPUtil.HTMLEscape(FValue);
end;

class operator TSvString.Implicit(const AString: TSvString): Variant;
begin
  Result := AString.FValue;
end;

class operator TSvString.Implicit(const AValue: Variant): TSvString;
begin
  Result.FValue := AValue;
end;

function TSvString.IndexOf(const AWhat: string; const ACompareOption: TStringComparison): NativeInt;
var
  I: NativeInt;
  L, LW: NativeUInt;
begin
  { Prepare! Calculate lengths }
  L := System.Length(FValue);
  LW := System.Length(AWhat);

  Result := CFirstCharacterIndex - 1; // Nothing.

  { Do not continue if there are no substrings or the string is empty }
  if (L = 0) or (LW > L) or (LW = 0) then
    Exit;

  { Start from the beggining and try to search for what we need }
  for I := 1 to (L - LW + 1) do
    if InternalCompare(PWideChar(FValue) + I - 1, PWideChar(AWhat), LW, ACompareOption) = 0 then
      Exit(I - 1 + CFirstCharacterIndex);
end;

function TSvString.IndexOfAny(const AWhat: array of string;
  const ACompareOption: TStringComparison): NativeInt;
var
  LW: array of NativeUInt;
  I, L, X, C: NativeUInt;
begin
  { Prepare! Calculate lengths }
  L := System.Length(FValue);
  C := System.Length(AWhat);
  Result := CFirstCharacterIndex - 1; // Nothing.

  { Do not continue if there are no substrings or the string is empty }
  if (L = 0) or (C = 0) then
    Exit;

  { Setup the lengths }
  SetLength(LW, C);
  for I := 0 to C - 1 do
    LW[I] := System.Length(AWhat[I]);

  { Start from the beggining and try to search for what we need }
  for I := L downto 1 do
    for X := 0 to C - 1 do
    begin
      { Check whether the current substr can fit into what's left }
      if (LW[X] > (L - I + 1)) or (LW[X] = 0) then
        continue;

      if InternalCompare(PWideChar(FValue) + I - 1, PWideChar(AWhat[X]), LW[X], ACompareOption) = 0 then
        Exit(I - 1 + CFirstCharacterIndex);
    end;
end;

procedure TSvString.Insert(const AIndex: NativeInt; const AWhat: string);
begin
  System.Insert(AWhat, FValue, AIndex);
end;

function TSvString.InsertF(const AIndex: NativeInt; const AWhat: string): TsvString;
begin
  Result.FValue := FValue;
  Result.Insert(AIndex, AWhat);
end;

class function TSvString.InternalCompare(const ALeft, ARight: PWideChar; const MaxLen: NativeUInt;
  const LType: TStringComparison): NativeInt;
begin
  Result := FStrCompareFuncs[LType](ALeft, ARight, MaxLen);
end;

procedure TSvString.Join(const ASeparator: string; const AStrings: array of string);
var
  I, L: NativeInt;
begin
  { This may look weird but it's actually optinmized for the most common cases }
  L := System.Length(AStrings);

  case L of
    0: FValue := CEmpty;
    1: FValue := AStrings[0];
    2: FValue := AStrings[0] + ASeparator + AStrings[1];
    3: FValue := AStrings[0] + ASeparator + AStrings[1] + ASeparator + AStrings[2];
    4: FValue := AStrings[0] + ASeparator + AStrings[1] + ASeparator +
      AStrings[2] + ASeparator + AStrings[3];
    5: FValue := AStrings[0] + ASeparator + AStrings[1] + ASeparator + AStrings[2] +
      ASeparator + AStrings[3] + ASeparator + AStrings[4];
    else
    begin
      FValue := AStrings[0];

      for I := 1 to L - 1 do
        FValue := FValue + ASeparator + AStrings[I];
    end;
  end;

end;

function TSvString.JoinF(const ASeparator: string; const AStrings: array of string): TSvString;
begin
  Result.FValue := FValue;
  Result.Join(ASeparator, AStrings);
end;

function TSvString.LastIndexOf(const AWhat: string;
  const ACompareOption: TStringComparison): NativeInt;
var
  I: NativeInt;
  L, LW: NativeUInt;
begin
  { Prepare! Calculate lengths }
  L := System.Length(FValue);
  LW := System.Length(AWhat);

  { Special case of nil string }
  Result := CFirstCharacterIndex - 1; // Nothing.

  { Do not continue if there are no substrings or the string is empty }
  if (L = 0) or (LW > L) or (LW = 0) then
    Exit;

  { Start from the beggining and try to search for what we need }
  for I := (L - LW + 1) downto 1 do
    if InternalCompare(PWideChar(FValue) + I - 1, PWideChar(AWhat), LW, ACompareOption) = 0 then
      Exit(I - 1 + CFirstCharacterIndex);
end;

function TSvString.LastIndexOfAny(const AWhat: array of string;
  const ACompareOption: TStringComparison): NativeInt;
var
  LW: array of NativeUInt;
  I, L, X, C: NativeUInt;
begin
  { Prepare! Calculate lengths }
  L := System.Length(FValue);
  C := System.Length(AWhat);
  Result := CFirstCharacterIndex - 1; // Nothing.

  { Do not continue if there are no substrings or the string is empty }
  if (L = 0) or (C = 0) then
    Exit;

  { Setup the lengths }
  SetLength(LW, C);
  for I := 0 to C - 1 do
    LW[I] := System.Length(AWhat[I]);

  { Start from the beggining and try to search for what we need }
  for I := L downto 1 do
    for X := 0 to C - 1 do
    begin
      { Check whether the current substr can fit into what's left }
      if (LW[X] > (L - I + 1)) or (LW[X] = 0) then
        continue;

      if InternalCompare(PWideChar(FValue) + I - 1, PWideChar(AWhat[X]), LW[X], ACompareOption) = 0 then
        Exit(I - 1 + CFirstCharacterIndex);
    end;
end;

procedure TSvString.LoadFromFile(const AFilename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TSvString.LoadFromStream(AStream: TStream);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('', TEncoding.UTF8);
  try
    ss.CopyFrom(AStream, AStream.Size);

    FValue := ss.DataString;
  finally
    ss.Free;
  end;
end;

procedure TSvString.MD5;
begin
  FValue := MD5F();
end;

function TSvString.MD5F: string;
var
  AMd5: TIdHashMessageDigest5;
begin
  AMd5 := TIdHashMessageDigest5.Create();
  try
    Result := SysUtils.LowerCase(AMd5.HashStringAsHex(FValue, TEncoding.UTF8));
  finally
    AMd5.Free;
  end;
end;

class operator TSvString.NotEqual(const ALeft, ARight: TSvString): Boolean;
begin
  Result := ALeft.FValue <> ARight.FValue;
end;

procedure TSvString.PadLeft(const ACount: NativeInt; const AChar: Char);
var
  LPad: string;
  I: NativeInt;
begin
  { Create the pad string }
  SetLength(LPad, ACount);
  for I := 1 to ACount do
    LPad[I] := AChar;

  { [PAD] + String }
  FValue := LPad + FValue;
end;

function TSvString.PadLeftF(const ACount: NativeInt; const AChar: Char): TSvString;
begin
  Result.FValue := FValue;
  Result.PadLeft(ACount, AChar);
end;

procedure TSvString.PadRight(const ACount: NativeInt; const AChar: Char);
var
  LPad: string;
  I: NativeInt;
begin
  { Create the pad string }
  SetLength(LPad, ACount);
  for I := 1 to ACount do
    LPad[I] := AChar;

  { [PAD] + String }
  FValue := FValue + LPad;
end;

function TSvString.PadRightF(const ACount: NativeInt; const AChar: Char): TSvString;
begin
  Result.FValue := FValue;
  Result.PadRight(ACount, AChar);
end;

function TSvString.Pos(const AWhat: string; const ACompareOption: TStringComparison = scInvariant; const AOffset: Integer = 1): Integer;
var
  sWhat, sValue: string;
begin
  case ACompareOption of
    scLocale, scInvariant, scOrdinal:
    begin
      sWhat := AWhat;
      sValue := FValue;
    end;
    scLocaleIgnoreCase:
    begin
      sWhat := SysUtils.UpperCase(AWhat, loUserLocale);
      sValue := SysUtils.UpperCase(FValue, loUserLocale);
    end;
    scInvariantIgnoreCase:
    begin
      sWhat := SysUtils.UpperCase(AWhat, loInvariantLocale);
      sValue := SysUtils.UpperCase(FValue, loInvariantLocale);
    end;
    scOrdinalIgnoreCase:
    begin
      sWhat := SysUtils.AnsiUpperCase(AWhat);
      sValue := SysUtils.AnsiUpperCase(FValue);
    end;
  end;

  Result := StrUtils.PosEx(sWhat, sValue, AOffset);
end;

function TSvString.ReplaceF(const AWhat, AWith: Char): TSvString;
var
  I: NativeInt;
begin
  { Copy the string }
  Result.FValue := FValue;

  { Start working }
  for I := 1 to System.Length(FValue) do
    if Result.FValue[I] = AWhat then
      Result.FValue[I] := AWith;
end;

procedure TSvString.Remove(const AStart: NativeInt);
begin
  Remove(AStart, Self.Length);
end;

procedure TSvString.RandomFrom(const AValues: array of string);
begin
  FValue := StrUtils.RandomFrom(AValues);
end;

class function TSvString.RandomFromF(const AValues: array of string): TSvString;
begin
  Result.FValue := StrUtils.RandomFrom(AValues);
end;

procedure TSvString.Remove(const AStart: NativeInt; const ACount: NativeUInt);
begin
  Delete(FValue, AStart, ACount);
end;

function TSvString.RemoveF(const AStart: NativeInt): TsvString;
begin
  Result.FValue := FValue;
  Result.Remove(AStart);
end;

function TSvString.RemoveF(const AStart: NativeInt; const ACount: NativeUInt): TsvString;
begin
  Result.FValue := FValue;
  Result.Remove(AStart, ACount);
end;

procedure TSvString.Replace(const AWhat, AWith: Char);
var
  I: NativeInt;
begin
  { Start working }
  for I := 1 to System.Length(FValue) do
    if FValue[I] = AWhat then
      FValue[I] := AWith;
end;

procedure TSvString.Replace(const AWhat, AWith: string; const ACompareOption: TStringComparison);
var
  LResult: string;
  LLength, LWhatLen, I, L: NativeInt;
begin
  { Init }
  LResult := CEmpty;
  LLength := System.Length(FValue);
  LWhatLen := System.Length(AWhat);

  { Nothing to do? }
  if (LLength = 0) or (LWhatLen = 0) or (LWhatLen > LLength) then
  begin
    Exit;
  end;

  L := 1;

  { Start from the beggining abd do search }
  for I := 1 to (LLength - LWhatLen + 1) do
    if InternalCompare(PWideChar(FValue) + I - 1, PWideChar(AWhat), LWhatLen, ACompareOption) = 0 then
    begin
      LResult := LResult + System.Copy(FValue, L, (I - L)) + AWith;
      L := I + LWhatLen;
    end;

  if L < LLength then
    LResult := LResult + System.Copy(FValue, L, MaxInt);

  FValue := LResult;
end;

function TSvString.ReplaceF(const AWhat, AWith: string;
  const ACompareOption: TStringComparison): TSvString;
var
  LResult: string;
  LLength, LWhatLen, I, L: NativeInt;
begin
  { Init }
  LResult := CEmpty;
  LLength := System.Length(FValue);
  LWhatLen := System.Length(AWhat);

  { Nothing to do? }
  if (LLength = 0) or (LWhatLen = 0) or (LWhatLen > LLength) then
  begin
    Result.FValue := FValue;
    Exit;
  end;

  L := 1;

  { Start from the beggining abd do search }
  for I := 1 to (LLength - LWhatLen + 1) do
    if InternalCompare(PWideChar(FValue) + I - 1, PWideChar(AWhat), LWhatLen, ACompareOption) = 0 then
    begin
      LResult := LResult + System.Copy(FValue, L, (I - L)) + AWith;
      L := I + LWhatLen;
    end;

  if L < LLength then
    LResult := LResult + System.Copy(FValue, L, MaxInt);

  Result.FValue := LResult;
end;

procedure TSvString.Reverse;
begin
  StrUtils.AnsiReverseString(FValue);
end;

function TSvString.ReverseF: TSvString;
begin
  Result.FValue := FValue;
  Result.Reverse;
end;

procedure TSvString.ReverseInvariant;
begin
  StrUtils.ReverseString(FValue);
end;

function TSvString.ReverseInvariantF: TSvString;
begin
  Result.FValue := FValue;
  Result.ReverseInvariant;
end;

function TSvString.Split(const ADelimiter: Char;
  const ARemoveEmptyEntries: Boolean): TArray<string>;
begin
  Result := Split([ADelimiter], ARemoveEmptyEntries);
end;

procedure TSvString.SaveToFile(const AFilename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TSvString.SaveToStream(AStream: TStream);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(FValue, TEncoding.UTF8);
  try
    ss.Position := 0;
    AStream.CopyFrom(ss, ss.Size);
  finally
    ss.Free;
  end;
end;

function TSvString.Soundex(const ALength: Integer): string;
begin
  Result := StrUtils.Soundex(FValue, ALength);
end;

function TSvString.SoundexSimilar(const AWith: string; const ALength: Integer): Boolean;
begin
  Result := StrUtils.SoundexSimilar(FValue, AWith, ALength);
end;

function TSvString.Split(const ADelimiters: TWideCharSet;
  const ARemoveEmptyEntries: Boolean): TArray<string>;
var
  LResCount, I, LLag,
    LPrevIndex, LCurrPiece: NativeInt;
  LPiece: string;
begin
  { Initialize, set all to zero }
  SetLength(Result , 0);

  { Do nothing for empty strings }
  if System.Length(FValue) = 0 then
    Exit;

  { Determine the length of the resulting array }
  LResCount := 0;

  for I := 1 to System.Length(FValue) do
    if CharInSet(FValue[I], ADelimiters) then
      Inc(LResCount);

  { Set the length of the output split array }
  SetLength(Result, LResCount + 1);

  { Split the string and fill the resulting array }
  LPrevIndex := 1;
  LCurrPiece := 0;
  LLag := 0;

  for I := 1 to System.Length(FValue) do
    if CharInSet(FValue[I], ADelimiters) then
    begin
      LPiece := System.Copy(FValue, LPrevIndex, (I - LPrevIndex));

      if ARemoveEmptyEntries and (System.Length(LPiece) = 0) then
        Inc(LLag)
      else
        Result[LCurrPiece - LLag] := LPiece;

      { Adjust prev index and current piece }
      LPrevIndex := I + 1;
      Inc(LCurrPiece);
    end;

  { Copy the remaining piece of the string }
  LPiece := Copy(FValue, LPrevIndex, System.Length(FValue) - LPrevIndex + 1);

  { Doom! }
  if ARemoveEmptyEntries and (System.Length(LPiece) = 0) then
    Inc(LLag)
  else
    Result[LCurrPiece - LLag] := LPiece;

  { Re-adjust the array for the missing pieces }
  if LLag > 0 then
    System.SetLength(Result, LResCount - LLag + 1);
end;

function TSvString.StartsWith(const AWhat: string;
  const ACompareOption: TStringComparison): Boolean;
begin
  Result := IndexOf(AWhat, ACompareOption) = CFirstCharacterIndex;
end;

function TSvString.Substring(const AStart: NativeInt): TSvString;
var
  LIndex, LLength: NativeInt;
begin
  { Calculate the index proper }
  LIndex := AStart + (1 - CFirstCharacterIndex);
  LLength := System.Length(FValue);

  Result.FValue := Copy(FValue, LIndex, LLength);
end;

function TSvString.Substring(const AStart: NativeInt; const ACount: NativeUInt): TSvString;
var
  LIndex: NativeInt;
begin
  { Calculate the index proper }
  LIndex := AStart + (1 - CFirstCharacterIndex);

  Result.FValue := Copy(FValue, LIndex, ACount);
end;

(*
function TSvString.Substring(const AStart: NativeInt): string;
var
  LIndex, LLength: NativeInt;
begin
  { Calculate the index proper }
  LIndex := AStart + (1 - CFirstCharacterIndex);
  LLength := System.Length(FValue);

  Result := Copy(FValue, LIndex, LLength);
end;

function TSvString.Substring(const AStart: NativeInt; const ACount: NativeUInt): string;
var
  LIndex, LLength: NativeInt;
begin
  { Calculate the index proper }
  LIndex := AStart + (1 - CFirstCharacterIndex);
  LLength := System.Length(FValue);

  Result := Copy(FValue, LIndex, ACount);
end;

*)

function TSvString.ToBool: Boolean;
begin
  Result := StrToBool(FValue);
end;

function TSvString.ToBin: string;
begin
  if FValue = '' then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, System.Length(FValue) div 4);
  HexToBin(PWideChar(FValue), Result[1], System.Length(FValue) div SizeOf(Char));
end;

function TSvString.ToBool(const ADef: Boolean): Boolean;
begin
  Result := StrToBoolDef(FValue, ADef);
end;

function TSvString.ToBytes: TBytes;
begin
  Result := WideBytesOf(FValue);
end;

function TSvString.ToCharArray: TArray<Char>;
var
  LLength: NativeUInt;
begin
  LLength := System.Length(FValue);
  SetLength(Result, LLength);

  if LLength > 0 then
    MoveChars(FValue[1], Result[0], LLength);
end;

function TSvString.ToDate: TDate;
begin
  Result := StrToDate(FValue, TSvStringFormatSettings.Get);
end;

function TSvString.ToDate(const ADef: TDate): TDate;
begin
  Result := StrToDateDef(FValue, ADef, TSvStringFormatSettings.Get);
end;

function TSvString.ToDate(const AFormatSettings: TFormatSettings; const ADef: TDate): TDate;
begin
  Result := StrToDateDef(FValue, ADef, AFormatSettings);
end;

function TSvString.ToDateTime: TDateTime;
begin
  Result := StrToDateTime(FValue, TSvStringFormatSettings.Get);
end;

function TSvString.ToDateTime(const AFormatSettings: TFormatSettings): TDateTime;
begin
  Result := StrToDateTime(FValue, AFormatSettings);
end;

function TSvString.ToDateTime(const ADef: TDateTime): TDateTime;
begin
  Result := StrToDateTimeDef(FValue, ADef, TSvStringFormatSettings.Get);
end;

function TSvString.ToDateTime(const AFormatSettings: TFormatSettings; const ADef: TDateTime): TDateTime;
begin
  Result := StrToDateTimeDef(FValue, ADef, AFormatSettings);
end;

function TSvString.ToDouble: Double;
begin
  Result := StrToFloat(FValue, TSvStringFormatSettings.Get);
end;

function TSvString.ToDouble(const ADef: Double): Double;
begin
  Result := StrToFloatDef(FValue, ADef, TSvStringFormatSettings.Get);
end;

function TSvString.ToDate(const AFormatSettings: TFormatSettings): TDate;
begin
  Result := StrToDate(FValue, AFormatSettings);
end;

function TSvString.ToInt: Integer;
begin
  Result := StrToInt(FValue);
end;

function TSvString.ToInt(const ADef: Integer): Integer;
begin
  Result := StrToIntDef(FValue, ADef);
end;

function TSvString.ToInt64: Int64;
begin
  Result := StrToInt64(FValue);
end;

function TSvString.ToInt64(const ADef: Int64): Int64;
begin
  Result := StrToInt64Def(FValue, ADef);
end;

procedure TSvString.ToLower;
begin
  LowerCase(FValue, loUserLocale);
end;

function TSvString.ToLowerF: TSvString;
begin
  Result.FValue := LowerCase(FValue, loUserLocale);
end;

procedure TSvString.ToLowerInvariant;
begin
  LowerCase(FValue, loInvariantLocale);
end;

function TSvString.ToNativeInt: NativeInt;
begin
  Result := StrToInt(FValue);
end;

function TSvString.ToLowerInvariantF: TSvString;
begin
  Result.FValue := LowerCase(FValue, loInvariantLocale);
end;

function TSvString.ToNativeInt(const ADef: NativeInt): NativeInt;
begin
  Result := StrToIntDef(FValue, ADef);
end;

function TSvString.ToPchar: PChar;
begin
  Result := PChar(FValue);
end;

function TSvString.ToSqlString: string;
begin
  Result := 'NULL';
  if FValue <> '' then
  begin
    Result := 'N' + QuotedStr(FValue);
  end;
end;

function TSvString.ToString: string;
begin
  Result := FValue;
end;

function TSvString.ToUCS4String: UCS4String;
begin
  Result := UnicodeStringToUCS4String(FValue);
end;

procedure TSvString.ToUpper;
begin
  UpperCase(FValue, loUserLocale);
end;

function TSvString.ToUpperF: TSvString;
begin
  Result.FValue := UpperCase(FValue, loUserLocale);
end;

procedure TSvString.ToUpperInvariant;
begin
  UpperCase(FValue, loInvariantLocale);
end;

function TSvString.ToUpperInvariantF: TSvString;
begin
  Result.FValue := UpperCase(FValue, loInvariantLocale);
end;

function TSvString.ToUTF8String: RawByteString;
begin
  Result := UTF8Encode(FValue);
end;

procedure TSvString.Trim;
begin
  SysUtils.Trim(FValue);
end;

procedure TSvString.Trim(const ACharSet: TWideCharSet);
var
  I, L, R: NativeInt;
begin
  { Defaults }
  L := System.Length(FValue);
  R := 1;

  { Find the left point }
  for I := 1 to System.Length(FValue) do
    if not (CharInSet(FValue[I], ACharSet)) then
    begin
      L := I;
      Break;
    end;

  { Find the right point }
  for I := System.Length(FValue) downto 1 do
    if not (CharInSet(FValue[I], ACharSet)) then
    begin
      R := I;
      Break;
    end;

  { Copy }
  FValue := System.Copy(FValue, L, (R - L + 1));
end;

function TSvString.TrimF(const ACharSet: TWideCharSet): TSvString;
begin
  Result.FValue := FValue;
  Result.Trim(ACharSet);
end;

function TSvString.TrimF: TSvString;
begin
  Result.FValue := FValue;
  Result.Trim;
end;

procedure TSvString.TrimLeft(const ACharSet: TWideCharSet);
var
  I: NativeInt;
begin
  { Loop until we get to the first non-whitespace char. We've determined that 1st char is whitespace. }
  for I := 1 to System.Length(FValue) do
    if not (CharInSet(FValue[I], ACharSet)) then
    begin
      { If nothing was done, take the ref, or copy otherwise }
      if I = 1 then
        FValue := FValue
      else
        FValue := System.Copy(FValue, I, MaxInt);

      Exit;
    end;

  { It's all whitespaces }
  FValue := CEmpty;
end;

procedure TSvString.TrimLeft(const ASymbolsCount: Integer);
begin
  FValue := System.Copy(FValue, ASymbolsCount, Length);
end;

function TSvString.TrimLeftF(const ASymbolsCount: Integer): TSvString;
begin
  Result.FValue := FValue;
  Result.TrimLeft(ASymbolsCount);
end;

function TSvString.TrimLeftF(const ACharSet: TWideCharSet): TSvString;
begin
  Result.FValue := FValue;
  Result.TrimLeft(ACharSet);
end;

function TSvString.TrimLeftF: TSvString;
begin
  Result.FValue := FValue;
  Result.TrimLeft;
end;

procedure TSvString.TrimLeft;
begin
  SysUtils.TrimLeft(FValue);
end;

procedure TSvString.TrimRight(const ACharSet: TWideCharSet);
var
  I: NativeInt;
begin
  { Loop until we get to the first non-whitespace char. We've determined that 1st char is whitespace. }
  for I := System.Length(FValue) downto 1 do
    if not (CharInSet(FValue[I], ACharSet)) then
    begin
      { If nothing was done, take the ref, or copy otherwise }
      if I = System.Length(FValue) then
        FValue := FValue
      else
        FValue := System.Copy(FValue, 1, I);

      Exit;
    end;

  { It's all whitespaces }
  FValue := CEmpty;
end;

function TSvString.TrimRightF: TSvString;
begin
  Result.FValue := FValue;
  Result.TrimRight;
end;

function TSvString.TrimRightF(const ACharSet: TWideCharSet): TSvString;
begin
  Result.FValue := FValue;
  Result.TrimRight(ACharSet);
end;

procedure TSvString.TrimRight;
begin
  SysUtils.TrimRight(FValue);
end;

class operator TSvString.Implicit(const AString: AnsiString): TSvString;
begin
  Result.FValue := string(AString);
end;

class operator TSvString.Implicit(const AString: PSvString): TSvString;
begin
  Result.FValue := AString.FValue;
end;

procedure TSvString.FromDate(const AValue: TDate; const AFormatSettings: TFormatSettings);
begin
  FValue := DateToStr(AValue, AFormatSettings);
end;

procedure TSvString.FromDateTime(const AValue: TDateTime; const AFormatSettings: TFormatSettings);
begin
  FValue := DateTimeToStr(AValue, AFormatSettings);
end;

procedure TSvString.FromDouble(const AValue: Double; const AFormatSettings: TFormatSettings);
begin
  FValue := FloatToStr(AValue, AFormatSettings);
end;

function TSvString.ToDouble(const AFormatSettings: TFormatSettings; const ADef: Double): Double;
begin
  Result := StrToFloatDef(FValue, ADef, AFormatSettings);
end;

function TSvString.ToHex: string;
var
  s1: string;
begin
  if FValue = '' then
  begin
    Result := '';
    Exit;
  end;
  s1 := FValue;
  SetLength(Result, System.Length(s1) * 4);
  BinToHex(s1[1], PWideChar(Result), System.Length(s1) * SizeOf(Char));
end;

procedure TSvString.TrimRight(const ASymbolsCount: Integer);
begin
  FValue := System.Copy(FValue, 1, Length - ASymbolsCount);
end;

function TSvString.TrimRightF(const ASymbolsCount: Integer): TSvString;
begin
  Result.FValue := FValue;
  Result.TrimRight(ASymbolsCount);
end;

class function TSvString.ValueOf(const AVar: Variant): TSvString;
begin
  Result.FValue := AVar;
end;

{ TSvString.TEnumerator }

constructor TSvString.TEnumerator.Create(const AString: string);
begin
  inherited Create();
  FString := AString;
  FCurrent := #0;
  FIndex := 0;
end;

function TSvString.TEnumerator.DoGetCurrent: Char;
begin
  Result := FCurrent;
end;

function TSvString.TEnumerator.DoMoveNext: Boolean;
begin
  { Check for end }
  Inc(FIndex);
  Result := FIndex <= System.Length(FString);

  { Read current }
  if Result then
    FCurrent := FString[FIndex];
end;

{ TSvStringFormatSettings }

class constructor TSvStringFormatSettings.Create;
begin
  FFmtSettings := TFormatSettings.Create();
end;

class function TSvStringFormatSettings.Get: TFormatSettings;
begin
  Result := FFmtSettings;
end;

class procedure TSvStringFormatSettings.Apply(const Value: TFormatSettings);
begin
  FFmtSettings := Value;
end;

initialization
  { Register comparison functions }
  FStrCompareFuncs[scLocale]              := @__LocaleCaseSensitive;
  FStrCompareFuncs[scLocaleIgnoreCase]    := @__LocaleCaseInsensitive;
  FStrCompareFuncs[scInvariant]           := @__InvariantCaseSensitive;
  FStrCompareFuncs[scInvariantIgnoreCase] := @__InvariantCaseInsensitive;
  FStrCompareFuncs[scOrdinal]             := @__OrdinalCaseSensitive;
  FStrCompareFuncs[scOrdinalIgnoreCase]   := @__OrdinalCaseInsensitive;


end.
