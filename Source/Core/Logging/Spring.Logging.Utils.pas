{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
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

unit Spring.Logging.Utils;

interface

uses
  Classes,
  Windows,
  SysUtils,
  Character,
  Spring,
  Spring.Collections,
  Spring.Services.Logging,
  Spring.Logging.Core,
  Spring.Logging.Loggers;

type
  TInternalLogger = class sealed(TLoggerBase)
  strict private
    class var
      fInstance: ILogger;
    class constructor Create;
  private
    fQuietMode: Boolean;
  protected
    function ShouldLog(const level: TLevel): Boolean; override;
    procedure InternalLog(const event: TLoggingEvent); overload; override;
  public
    property QuietMode: Boolean read fQuietMode write fQuietMode;
    class property Instance: ILogger read fInstance;
  end;

const
  tpEof = #0;
  tpPercent = #1;
  tpText = #3;
  tpFormat = #4;
  tpKeyWord= #5;

type
  TPatternType = (ptCustomer, ptText, ptDate, ptTab,
    ptThread, ptLevel, ptLogger, ptMessage,
    ptNewLine, ptUser, ptComputer, ptPercent, ptApplicationVersion);

  TPatternToken = record
    PatternPart : string;
    Convertor : string;
    PatternType: TPatternType;
  end;

  TPatternTokens = TDictionary<string, TPatternToken>;

  TPatternParser = class
  private type
    TMappingTokenType = (mttOther, mttPercent, mttLetter, mttNumber, mttHash, mttSpace);
  private
    fPattern: string;
    fCurPosition: Integer;
    fTokenPostion: Integer;
    fToken: Char;
    fKeyPatterns: TPatternTokens;
    function Mapping(source: char): TMappingTokenType;
  public
    constructor Create(const pattern: string);
    destructor Destroy; override;
    function GetPatternToken(const keyWord: string): TPatternToken;
    function NextToken: Char;
    function TokenString: string;
    function TokenPattern: TPatternToken;
    procedure InitializePatternString(const patternString: string);
    procedure AddKeyPatternTokens(const keyword: string; const convertor: string; patternType: TPatternType);
    property Token: Char read fToken;
  end;

  TEncodingRegistry = class
  private
    fEncodings: IDictionary<string, TEncoding>;
  public
    constructor Create;
    function GetEncodingOrDefault(const name: string): TEncoding;
  end;

/// <summary>
/// Returns the internal logger.
/// </summary>
function InternalLogger: ILogger;

function LastIndexOf(const value: Char; const s: string): Integer;

implementation

function InternalLogger: ILogger;
begin
  Result := TInternalLogger.Instance;
end;

function LastIndexOf(const value: Char; const s: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
  begin
    if s[i] = value then
    begin
      Result := i;
      Break;
    end;
  end;
end;


{$REGION 'TInternalLogger'}

class constructor TInternalLogger.Create;
begin
  fInstance := TInternalLogger.Create('InternalLogger');
  TInternalLogger(fInstance).QuietMode := True;
end;

procedure TInternalLogger.InternalLog(const event: TLoggingEvent);
const
  CLogFormat = '%S [%4d] %-5S %S %S';
var
  s: string;
begin
  s := Format(CLogFormat, [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', event.TimeStamp),
    event.ThreadID,
    event.LoggerLevel.Name,
    event.Message,
    event.ErrorMessage
  ]);
  Windows.OutputDebugString(PChar(s));
end;

function TInternalLogger.ShouldLog(const level: TLevel): Boolean;
begin
  Result := not QuietMode;
end;

{$ENDREGION}


{$REGION 'TPatternParser'}

procedure TPatternParser.AddKeyPatternTokens(const keyword, convertor: string;
  patternType: TPatternType);
var
  pattern : TPatternToken;
begin
  pattern.PatternPart := keyword;
  pattern.Convertor := convertor;
  pattern.PatternType := patternType;
  fKeyPatterns.Add(LowerCase(keyword), pattern);
end;

constructor TPatternParser.Create(const pattern: string);
begin
  inherited Create;
  fKeyPatterns:= TPatternTokens.Create;
  InitializePatternString(pattern);
end;

destructor TPatternParser.Destroy;
begin
  fKeyPatterns.Free;
  inherited;
end;

function TPatternParser.GetPatternToken(const keyWord: string): TPatternToken;
begin
  if not fKeyPatterns.TryGetValue(LowerCase(keyWord) , Result) then
  begin
    Result.PatternPart := keyWord;
    Result.Convertor := keyWord;
    Result.PatternType := ptText;
  end;
end;

procedure TPatternParser.InitializePatternString(const patternString: string);
begin
  fPattern := patternString;
  fTokenPostion := 1;
  fCurPosition := 1;
  fToken := tpEof;
  NextToken;
end;

function TPatternParser.Mapping(source: char): TMappingTokenType;
begin
  case source of
    'A'..'Z', 'a'..'z', '_': Result := mttLetter;
    '0'..'9': Result := mttNumber;
    '%': Result := mttPercent;
    '-': Result := mttHash;
    ' ': Result := mttSpace;
  else
    Result := mttOther;
  end;
end;

function TPatternParser.NextToken: Char;
var
  cursor: Integer;
  tokenType: TMappingTokenType;
  function LookForKeywordOrText: Char;
  var
    patternPart: string;
  begin
    patternPart := '';
    Result := tpText;
    repeat
      patternPart := patternPart + TCharacter.ToLower(fPattern[cursor]);
      Inc(cursor);
      if fKeyPatterns.ContainsKey(patternPart) then begin
        Result := tpKeyWord;
        Break;
      end
    until not (Mapping(fPattern[cursor])  in [mttLetter, mttNumber]);
  end;
begin
  if fPattern = '' then Exit(tpEof);

  fTokenPostion := fCurPosition;
  cursor := fTokenPostion;
  tokenType:= Mapping(fPattern[cursor]) ;
  case tokenType of
    mttPercent:
    begin
      inc(cursor);
      case Mapping(fPattern[cursor]) of
        mttPercent :
        begin
          inc(cursor);
          result := tpPercent;
        end ;
        mttHash:
        begin
          inc(cursor);
          if  Mapping(fPattern[cursor]) = mttNumber then
          begin
            inc(cursor);
            while Mapping(fPattern[cursor]) = mttNumber do
              inc(cursor);
            result:= tpFormat;
          end
          else begin
            while Mapping(fPattern[cursor]) = mttLetter do
              Inc(cursor);
            Result := tpText;
          end;
        end;
        mttLetter:
        begin
          Result := tpFormat;
        end;//Result := LookForKeywordOrText;
        mttNumber:
        begin
          inc(cursor);
          while Mapping(fPattern[cursor]) = mttNumber do
            inc(cursor);
          // to do : if it is not a valid numeric , consider changing result to tpText.
          result:= tpFormat;
        end;
      else
        Result := tpText;
      end;
    end;
    mttLetter, mttNumber, mttSpace:
    begin
      Result := LookForKeywordOrText;
    end;
  else
    Result := fPattern[cursor];
    inc(cursor);
  end;
  fCurPosition := cursor;
  fToken := Result;
end;

function TPatternParser.TokenPattern: TPatternToken;
begin
  if Token <> tpKeyWord then
    raise Exception.Create('Invalid Pattern Token');
  Result := GetPatternToken(TokenString);
end;

function TPatternParser.TokenString: string;
begin
  Result := Copy(fPattern, fTokenPostion, fCurPosition - fTokenPostion);
end;

{$ENDREGION}


{$REGION 'TEncodingRegistry'}

constructor TEncodingRegistry.Create;
begin
  inherited Create;
  fEncodings := TDictionary<string, TEncoding>.Create;
  with fEncodings do
  begin
    AddOrSetValue('default', TEncoding.Default);
    AddOrSetValue('utf-7',TEncoding.UTF7);
    AddOrSetValue('utf7',TEncoding.UTF7);
    AddOrSetValue('utf-8', TEncoding.UTF8);
    AddOrSetValue('utf8', TEncoding.UTF8);
    AddOrSetValue('unicode', TEncoding.Unicode);
    AddOrSetValue('utf16', TEncoding.Unicode);
    AddOrSetValue('utf-16', TEncoding.Unicode);
    AddOrSetValue('utf16-le', TEncoding.Unicode);
  end;
end;

function TEncodingRegistry.GetEncodingOrDefault(const name: string): TEncoding;
begin
  if not fEncodings.TryGetValue(name, Result) then
    Exit(TEncoding.Default);
end;

{$ENDREGION}

end.
