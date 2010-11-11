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

unit Spring.Logging.Layouts;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  Spring,
  Spring.Utils,
  Spring.Logging.Core;

type
  TLayoutBase = class abstract(TInterfacedObject, IOptionHandler, ILayout)
  protected
    function GetContentType: string; virtual;
    function GetHeader: string; virtual;
    function GetFooter: string; virtual;
    function GetIgnoresException: Boolean; virtual;
  public
    { IOptionHandler }
    procedure ActivateOptions; virtual; abstract;
    { ILayout }
    function Format(const event: TLoggingEvent): string; virtual; abstract;
    property ContentType: string read GetContentType;
    property Header: string read GetHeader;
    property Footer: string read GetFooter;
    property IgnoresException: Boolean read GetIgnoresException;
  end;

  // conversionPattern
  /// <summary>
  ///   %date             DateTime <using the specified format in configuration>
  ///   %thread           ThreadId Or ThreadName(if name exists)
  ///   %level            LevelName
  ///   %logger           Logger Name
  ///   %message          Log Message
  ///   %newline          Change Lines
  ///   %user             Username - Computer Account
  ///   %computer         Computer Name
  ///   %%                %
  ///   %exception        Exception
  /// </summary>

  /// <remarks>
  ///   %method           Calling method name < unsupported >
  ///   %filename         File name of calling unit < unsupported >
  ///   %Line             Linenumber of calling method < unsupported >
  ///   %ndc              NDC  eg : 'messageA|B|C|D|E|F' < unsupported >
  /// </remarks>
  TPatternType = (ptCustomer, ptText, ptDate,
    ptThread, ptLevel, ptLogger, ptMessage,
    ptNewLine, ptUser, ptComputer, ptPercent, ptApplicationVersion);


  TPatternConvertor = record
    ConvertorString: String;
    PatternType: TPatternType;
  end;

  TPatternConvertors = TDictionary<string, TPatternConvertor>;

  TPatternLayout = class(TLayoutBase)
  private
    Const DefaultLayoutPattern = '';
  private
    fPattern: string;
    fPatternParts : TStrings;
    fPatternConvertors: TPatternConvertors;
    fDateFormatPattern: string;
    function GetConvertor(const keyword: string; var convertor: TPatternConvertor): Boolean;
    procedure AddConvertor(const keyword: string; const convertorString: string; const patternType: TPatternType);
    procedure SetPattern(const Value: string);
    procedure ParsePattern;
    procedure InitializeKeyWords(const keyWords: TPatternConvertors);
  protected
    procedure DoParsePattern; virtual;
    procedure AddPatternPart(const patternPart: string; patternType: TPatternType);
    procedure ClearPatternParts;
  public
    function Format(const event: TLoggingEvent): string; override;
    constructor Create(const pattern: string = DefaultLayoutPattern);
    destructor Destroy; override;
    property Pattern: string read fPattern write SetPattern;
    property DateFormatPattern: string read fDateFormatPattern write fDateFormatPattern;
  end;

implementation


{$REGION 'TLayoutBase'}

function TLayoutBase.GetContentType: string;
begin
  Result := 'text/plain';
end;

function TLayoutBase.GetFooter: string;
begin
  Result := '';
end;

function TLayoutBase.GetHeader: string;
begin
  Result := '';
end;

function TLayoutBase.GetIgnoresException: Boolean;
begin
  Result := True;
end;

{$ENDREGION}

{ TPatternLayout }


procedure TPatternLayout.AddConvertor(const keyword, convertorString: string;
  const patternType: TPatternType);
var
  patternConvertor: TPatternConvertor;
begin
  patternConvertor.ConvertorString := convertorString;
  patternConvertor.PatternType:= patternType;
  fPatternConvertors.Add(keyword, patternConvertor);
end;

procedure TPatternLayout.AddPatternPart(const patternPart: string;
  patternType: TPatternType);
begin
  fPatternParts.AddObject(patternPart, Pointer(patternType));
end;

procedure TPatternLayout.ClearPatternParts;
begin
  fPatternParts.Clear;
end;

constructor TPatternLayout.Create(const pattern: string);
begin
  inherited Create;
  fPatternParts:= TStringList.Create;
  fPatternConvertors := TPatternConvertors.Create;
  InitializeKeyWords(fPatternConvertors);
  AddPatternPart('%s', ptMessage);
  SetPattern(pattern);
end;

destructor TPatternLayout.Destroy;
begin
  FreeAndNil(fPatternParts);
  FreeAndNil(fPatternConvertors);
  inherited;
end;

procedure TPatternLayout.ParsePattern;
var
  patternPart : string;
  startPos: Integer;
begin
  ClearPatternParts;
  if fPattern = '' then
  begin
    AddPatternPart('%s', ptMessage);
  end
  else begin
    DoParsePattern;
  end;
end;

procedure TPatternLayout.DoParsePattern;
var
  parser: TParser;
  stream: TStringStream;
  patternPart: string;
  tokenString: String;
  patternConvertor: TPatternConvertor;
begin
  stream:= TStringStream.Create;
  try
    stream.WriteString(Pattern);
    stream.Position := 0;
    parser := TParser.Create(stream);
    try
      Repeat
        patternConvertor.ConvertorString := '';
        patternConvertor.PatternType := ptText;
        case parser.Token of
          ' ': patternPart := ' ' ;
          '%':
          begin
            patternPart := '%';
            if parser.NextToken <> toEof then
            begin
              case parser.token of
                '%': patternPart := '%';
                Classes.toInteger, classes.toFloat:
                Begin
                  patternPart := patternPart + parser.TokenString;
                  if parser.NextToken <> toEof then
                  begin
                    tokenString:= parser.TokenString;
                    if GetConvertor(tokenString, patternConvertor) then
                      patternPart := patternPart + patternConvertor.ConvertorString
                    else
                      patternPart := patternPart + tokenString;
                  end
                End;
                toSymbol:
                begin
                  tokenString:= parser.TokenString;
                  if GetConvertor(tokenString, patternConvertor) then
                    patternPart := patternPart + patternConvertor.ConvertorString
                  else
                    patternPart := patternPart + tokenString;
                end;
              else
                tokenString := parser.TokenString;
                patternPart := patternPart + tokenString;;
              end;
            end;
          end;
          else
            patternPart := parser.TokenString;
          end;
          AddPatternPart(patternPart, patternConvertor.PatternType);
      until parser.NextToken = toEof;
    finally
      parser.Free;
    end;
  finally
    stream.free;
  end;
end;


function TPatternLayout.Format(const event: TLoggingEvent): string;
var
  index: Integer;
  part: String;
  builder: TStringBuilder;
begin
  builder := TStringBuilder.Create;
  try
    for index := 0 to fPatternParts.Count -1 do
    begin
      part := fPatternParts[Index];
      case TPatternType(fPatternParts.Objects[index]) of
        ptText:
          builder.Append(part);
        ptMessage:
          builder.AppendFormat(part, [event.Message]);
        ptDate:
          builder.Append(FormatDateTime(DateFormatPattern, event.TimeStamp));
        ptLevel:
          builder.AppendFormat(part, [event.LoggerLevel.Name]);
        ptThread:
          builder.AppendFormat(part, [event.ThreadID]);
        ptLogger:
          builder.AppendFormat(part, [event.LoggerName]);
        ptNewLine:
          builder.AppendLine;
        ptUser:
          builder.AppendFormat(part, [Environment.UserName]);
        ptComputer:
          builder.AppendFormat(part, [Environment.MachineName]);
        ptApplicationVersion:
          builder.AppendFormat(part, [Environment.ApplicationVersionString]);
        ptPercent: builder.Append('%');
      else
        builder.Append(part);
      end;
    end;
    Result := builder.ToString;
  finally
    builder.Free;
  end;
end;

function TPatternLayout.GetConvertor(const keyword: string;
  var convertor: TPatternConvertor): Boolean;
begin
  result := fPatternConvertors.TryGetValue(keyword, convertor);
end;

procedure TPatternLayout.InitializeKeyWords(const keyWords: TPatternConvertors);
begin
  AddConvertor('%%',          's', ptPercent);
  AddConvertor('date',        's', ptDate);
  AddConvertor('thread',      's', ptThread);
  AddConvertor('level',       's', ptLevel);
  AddConvertor('logger',      's', ptLogger);
  AddConvertor('message',     's', ptMessage);
  AddConvertor('newline',     's', ptNewLine);
  AddConvertor('user',        's', ptUser);
  AddConvertor('computer',    's', ptComputer);
  AddConvertor('appversion',  's', ptApplicationVersion);
end;

procedure TPatternLayout.SetPattern(const Value: string);
begin
  if CompareText(fPattern, Value) <> 0 then
  begin
    fPattern := Value;
    ParsePattern();
  end;
end;

end.
