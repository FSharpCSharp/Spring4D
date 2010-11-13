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
  Spring,
  Spring.Configuration,
  Spring.Utils,
  Spring.Logging.Core,
  Spring.Logging.Utils;

type
  TLayoutBase = class abstract(TInterfacedObject, ILayout, IConfigurable)
  protected
    function GetContentType: string; virtual;
    function GetHeader: string; virtual;
    function GetFooter: string; virtual;
    function GetIgnoresException: Boolean; virtual;
  protected
    { IConfigurable }
    procedure Configure(const configuration: IConfigurationNode); virtual;
  public
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
  ///
  /// </remarks>

  TPatternLayout = class(TLayoutBase)
  private
    Const DefaultLayoutPattern = '%Date [%Thread] %-5Level %Logger - %Message%Newline';
  private
    fParser: TPatternParser;
    fPattern: string;
    fPatternParts : TStrings;
    fDateFormatPattern: string;
    procedure SetPattern(const Value: string);
    procedure ParsePattern;
    procedure InitializeKeyWords;
  protected
    procedure DoParsePattern; virtual;
    procedure AddPatternPart(const patternPart: string; patternType: TPatternType);
    procedure AddKeyPattern(const keyword, patternConvertor: string; const patternType: TPatternType);
    procedure ClearPatternParts;
  protected
    procedure Configure(const configuration: IConfigurationNode); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Format(const event: TLoggingEvent): string; override;
    property Pattern: string read fPattern write SetPattern;
    property DateFormatPattern: string read fDateFormatPattern write fDateFormatPattern;
  end;

implementation


{$REGION 'TLayoutBase'}

procedure TLayoutBase.Configure(const configuration: IConfigurationNode);
begin
end;

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


{$REGION 'TPatternLayout'}

constructor TPatternLayout.Create;
begin
  inherited Create;
  fPatternParts:= TStringList.Create;
  fParser := TPatternParser.Create(DefaultLayoutPattern);
  InitializeKeyWords;
  AddPatternPart('%s', ptMessage);
end;

destructor TPatternLayout.Destroy;
begin
  FreeAndNil(fPatternParts);
  FreeAndNil(fParser);
  inherited;
end;

procedure TPatternLayout.AddKeyPattern(const keyword, patternConvertor: string;
  const patternType: TPatternType);
begin
  fParser.AddKeyPatternTokens(
      keyword,
      patternConvertor,
      patternType);
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

procedure TPatternLayout.Configure(const configuration: IConfigurationNode);
var
  node: IConfigurationNode;
begin
  node := configuration.FindNode('conversionPattern');
  if node <> nil then
    SetPattern(node.Attributes['value'])
  else
    SetPattern(DefaultLayoutPattern);
end;

procedure TPatternLayout.ParsePattern;
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
  tokenPattern: TPatternToken;
  formatString: string;
begin
  fParser.InitializePatternString(fPattern);
  repeat
    case fParser.Token of
      tpFormat:
      begin
        formatString := fParser.TokenString;
        if fParser.NextToken = tpKeyWord then
        begin
          tokenPattern := fParser.TokenPattern;
          AddPatternPart(formatString + tokenPattern.Convertor, tokenPattern.PatternType);
        end
        else
          AddPatternPart(formatString + fParser.TokenString, ptText);
      end;
      tpPercent: AddPatternPart('%', ptPercent);
    else
      AddPatternPart(fParser.TokenString, ptText);
    end;
  until fParser.NextToken = tpEof ;
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
          builder.AppendFormat(part, [FormatDateTime(DateFormatPattern, event.TimeStamp)]);
        ptLevel:
          builder.AppendFormat(part, [event.LoggerLevel.Name]);
        ptThread:
          begin
            if MainThreadID = event.ThreadID then
              builder.AppendFormat(part, ['Main'])
            else
              builder.AppendFormat(part, [IntToStr(event.ThreadID)]);
          end;
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
        ptPercent:
          builder.Append('%');
        ptTab:
          builder.Append(#9);
      else
        builder.Append(part);
      end;
    end;
    Result := builder.ToString;
  finally
    builder.Free;
  end;
end;

procedure TPatternLayout.InitializeKeyWords;
begin
  AddKeyPattern('%%',          's', ptPercent);
  AddKeyPattern('date',        's', ptDate);
  AddKeyPattern('thread',      's', ptThread);
  AddKeyPattern('level',       's', ptLevel);
  AddKeyPattern('logger',      's', ptLogger);
  AddKeyPattern('message',     's', ptMessage);
  AddKeyPattern('newline',     's', ptNewLine);
  AddKeyPattern('user',        's', ptUser);
  AddKeyPattern('computer',    's', ptComputer);
  AddKeyPattern('appversion',  's', ptApplicationVersion);
  AddKeyPattern('tab',         '' , ptTab);
end;

procedure TPatternLayout.SetPattern(const Value: string);
begin
  if CompareText(fPattern, Value) <> 0 then
  begin
    fPattern := Value;
    ParsePattern;
  end;
end;

{$ENDREGION}

end.
