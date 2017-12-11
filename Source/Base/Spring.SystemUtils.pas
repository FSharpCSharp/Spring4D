{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

{$I Spring.inc}

/// <summary>
///   Provides the internal utilities for Spring4D.
/// </summary>
unit Spring.SystemUtils;

interface

uses
  SysUtils,
  Types,
  TypInfo,
  Spring;

type
  TEnum = Spring.TEnum deprecated 'Use Spring.TEnum instead';

/// <summary>
///   Retrieves the byte length of a unicode string.
/// </summary>
/// <param name="s">
///   the unicode string.
/// </param>
/// <returns>
///   The byte length of the unicode string.
/// </returns>
/// <remarks>
///   Although there is already a routine <c>SysUtils.ByteLength(string)</c>
///   function, it only supports unicode strings and doesn't provide overloads
///   for WideStrings and AnsiStrings.
/// </remarks>
/// <seealso cref="GetByteLength(WideString)" />
/// <seealso cref="GetByteLength(RawByteString)" />
function GetByteLength(const s: string): Integer; overload; inline;

{$IFNDEF NEXTGEN}
/// <summary>
///   Retrieves the byte length of a WideString.
/// </summary>
/// <param name="s">
///   A wide string.
/// </param>
/// <returns>
///   The byte length of the wide string.
/// </returns>
/// <seealso cref="GetByteLength(string)" />
/// <seealso cref="GetByteLength(RawByteString)" />
function GetByteLength(const s: WideString): Integer; overload; inline;

/// <summary>
///   Retrieves the byte length of a <c>RawByteString</c> (AnsiString or
///   UTF8String).
/// </summary>
/// <returns>
///   The byte length of the raw byte string.
/// </returns>
/// <seealso cref="GetByteLength(string)" />
/// <seealso cref="GetByteLength(WideString)" />
function GetByteLength(const s: RawByteString): Integer; overload; inline;
{$ENDIF NEXTGEN}


/// <summary>
///   Overloads. SplitString
/// </summary>
/// <remarks>
///   Each element of separator defines a separate delimiter character. If two
///   delimiters are adjacent, or a delimiter is found at the beginning or end
///   of the buffer, the corresponding array element contains Empty.
/// </remarks>
function SplitString(const buffer: string; const separators: TSysCharSet;
  removeEmptyEntries: Boolean = False): TStringDynArray; overload;
function SplitString(const buffer: TCharArray; const separators: TSysCharSet;
  removeEmptyEntries: Boolean = False): TStringDynArray; overload;
function SplitString(const buffer: PChar; len: Integer; const separators: TSysCharSet;
  removeEmptyEntries: Boolean = False): TStringDynArray; overload;

/// <summary>
///   Returns a string array that contains the substrings in the buffer that
///   are delimited by null char (#0) and ends with an additional null char.
/// </summary>
/// <example>
///   <code lang="Delphi">procedure TestSplitNullTerminatedStrings;
/// var
///   buffer: string;
///   strings: TStringDynArray;
///   s: string;
/// begin
///   buffer := 'C:'#0'D:'#0'E:'#0#0;
///   strings := SplitString(PChar(buffer));
///   for s in strings do
///   begin
///     Writeln(s);
///   end;
/// end;</code>
/// </example>
function SplitString(const buffer: PChar): TStringDynArray; overload;

/// <summary>
///   Returns a string array that contains the substrings in the buffer that
///   are delimited by null char (#0) and ends with an additional null char.
/// </summary>
function SplitNullTerminatedStrings(const buffer: PChar): TStringDynArray;
  deprecated 'Use the SplitString(PChar) function instead.';

/// <summary>
///   Converts a string to a TDateTime value using the specified format, with a
///   Boolean success code.
/// </summary>
function TryStrToDateTimeFmt(const s, format: string; out value: TDateTime): Boolean;

/// <summary>
///   Converts a string to a TDateTime value using the specified format.
/// </summary>
function StrToDateTimeFmt(const s, format: string): TDateTime;

implementation

uses
  DateUtils,
  Spring.ResourceStrings;


function GetByteLength(const s: string): Integer;
begin
  Result := Length(s) * SizeOf(Char);
end;

{$IFNDEF NEXTGEN}
function GetByteLength(const s: WideString): Integer;
begin
  Result := Length(s) * SizeOf(WideChar);
end;

function GetByteLength(const s: RawByteString): Integer;
begin
  Result := Length(s);
end;
{$ENDIF}

function SplitString(const buffer: string; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
begin
  Result := SplitString(PChar(buffer), Length(buffer), separators, removeEmptyEntries);
end;

function SplitString(const buffer: TCharArray; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
begin
  Result := SplitString(PChar(buffer), Length(buffer), separators, removeEmptyEntries)
end;

function SplitString(const buffer: PChar; len: Integer; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
var
  head: PChar;
  tail: PChar;
  p: PChar;

  procedure AppendEntry(buffer: PChar; len: Integer; var strings: TStringDynArray);
  var
    entry: string;
  begin
    SetString(entry, buffer, len);
    if not removeEmptyEntries or (entry <> '') then
    begin
      SetLength(strings, Length(strings) + 1);
      strings[High(strings)] := entry;
    end;
  end;
begin
  Guard.CheckRange(len >= 0, 'len');

  if (buffer = nil) or (len = 0) then Exit;
  Result := nil;
  head := buffer;
  tail := head + len - 1;
  p := head;
  while p <= tail do
  begin
    if CharInSet(p^, separators) then
    begin
      AppendEntry(head, p - head, Result);
      head := StrNextChar(p);
    end;
    if p = tail then
    begin
      AppendEntry(head, p - head + 1, Result);
    end;
    p := StrNextChar(p);
  end;
end;

function SplitString(const buffer: PChar): TStringDynArray;
var
  p: PChar;
  entry: string;
begin
  if (buffer = nil) or (buffer^ = #0) then Exit;
  Result := nil;
  p := buffer;
  while p^ <> #0 do
  begin
    entry := p;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := entry;
    Inc(p, Length(entry) + 1);  // Jump to the next entry
  end;
end;

function SplitNullTerminatedStrings(const buffer: PChar): TStringDynArray;
begin
  Result := SplitString(buffer);
end;

function TryStrToDateTimeFmt(const s, format: string; out value: TDateTime): Boolean;
var
  localString: string;
  stringFormat: string;
  year, month, day: Word;
  hour, minute, second, milliSecond: Word;

  function ExtractElementDef(const element: string; const defaultValue: Integer = 0): Integer;
  var
    position: Integer;
  begin
    position := Pos(element, stringFormat);
    if position > 0 then
      Result := StrToInt(Copy(localString, position, Length(element)))
    else
      Result := defaultValue;
  end;

begin
  localString := Trim(s);
  stringFormat := UpperCase(format);
  Result := Length(localString) = Length(stringFormat);
  if Result then
  try
    year := ExtractElementDef('YYYY', 0);
    if year = 0 then
    begin
      year := ExtractElementDef('YY', 1899);
      if year < 1899 then
        Inc(year, (YearOf(Today) div 100) * 100);
    end;
    month := ExtractElementDef('MM', 12);
    day := ExtractElementDef('DD', 30);
    hour := ExtractElementDef('HH');
    minute := ExtractElementDef('NN');
    second := ExtractElementDef('SS');
    milliSecond := ExtractElementDef('ZZZ');
    value := EncodeDateTime(year, month, day, hour, minute, second, milliSecond);
  except
    Result := False;
  end;
end;

function StrToDateTimeFmt(const s, format: string): TDateTime;
begin
  if not TryStrToDateTimeFmt(s, format, Result) then
    raise EConvertError.CreateResFmt(@SInvalidDateTime, [s]);
end;

end.
