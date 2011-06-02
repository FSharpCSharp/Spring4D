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
  Spring.Configuration,
  Spring.Logging.Core;

type
  TInternalLogger = class sealed(TLoggerBase)
  strict private
    class var
      fInstance: ILogger;
    class constructor Create;
  private
    fQuietMode: Boolean;
  protected
    function IsEnabledFor(const level: TLevel): Boolean; override;
    procedure DoLog(const event: TLoggingEvent); overload; override;
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

/// <summary>
/// Returns the internal logger.
/// </summary>
function InternalLogger: ILogger;

function TryGetAttributeValue(const node: IConfigurationNode;
  const name: string; out value: string): Boolean;

implementation

function InternalLogger: ILogger;
begin
  Result := TInternalLogger.Instance;
end;

function TryGetAttributeValue(const node: IConfigurationNode;
  const name: string; out value: string): Boolean;
begin
  Result := node.TryGetAttribute(name, value) and (value <> '');
  if not Result then
  begin
    InternalLogger.ErrorFormat('The attribute "%s" was expected but there is none.', [name]);
  end;
end;


{$REGION 'TInternalLogger'}

class constructor TInternalLogger.Create;
begin
  fInstance := TInternalLogger.Create('InternalLogger');
  TInternalLogger(fInstance).QuietMode := True;
end;

procedure TInternalLogger.DoLog(const event: TLoggingEvent);
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
    event.ExceptionString
  ]);
  Windows.OutputDebugString(PChar(s));
end;

function TInternalLogger.IsEnabledFor(const level: TLevel): Boolean;
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


end.
