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

unit Spring.Logging.Utils;

interface

uses
  Classes,
  Character,
  SysUtils,
  Generics.Collections,
  Spring,
  Spring.Logging.Core;

type
  TInternalLogger = class sealed(TObject)
  strict private
    class var
      fDebugEnabled: Boolean;
      fQuietMode: Boolean;
    class constructor Create;
  public
    class procedure Debug(const msg: string);
    class procedure Warn(const msg: string);
    class procedure Error(const msg: string);
    class procedure ErrorFmt(const format: string; const args: array of const);
    class property InternalDebugging: Boolean read fDebugEnabled write fDebugEnabled;
    class property QuietMode: Boolean read fQuietMode write fQuietMode;
  end;


Const
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


implementation


{$IFDEF SUPPORTS_REGION} {$REGION 'TInternalLogger'} {$ENDIF}

class constructor TInternalLogger.Create;
begin
  fDebugEnabled := True;
  { TODO: Read Option from Configuration }
end;

class procedure TInternalLogger.Debug(const msg: string);
begin

end;

class procedure TInternalLogger.Error(const msg: string);
begin

end;

class procedure TInternalLogger.ErrorFmt(const format: string;
  const args: array of const);
var
  msg: string;
begin
  msg := SysUtils.Format(format, args);
  //...
end;

class procedure TInternalLogger.Warn(const msg: string);
begin

end;

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{ TPatternParser }

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


end.
