{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

{ TODO: Consider support variable length }
{ TODO: Add GetEndNumber to INumberRule }

unit Spring.Numbering.Rules;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  Spring.System,
  Spring.Utils;

type
  /// <summary>
  /// Represents a number rule.
  /// </summary>
  INumberRule = interface
    ['{28CE5D58-1B35-4072-8BAF-9F4061C1E78C}']
  {$REGION 'Property Getters'}
    function GetMinLength: Integer;
    function GetMaxLength: Integer;
  {$ENDREGION}
    function IsValid(const number: string): Boolean;
    function Validate(const number: string; out errorMessage: string): Boolean;
    function GetNextNumber(const number: string): string;
    function GetFirstNumber: string;
    function GetLastNumber: string;
    property MinLength: Integer read GetMinLength;
    property MaxLength: Integer read GetMaxLength;
  end;

  /// <summary>
  /// TNumberRuleBase
  /// </summary>
  TNumberRuleBase = class abstract(TInterfacedObject, INumberRule)
  private
    fMinLength: Integer;
    fMaxLength: Integer;
    function GetMinLength: Integer;
    function GetMaxLength: Integer;
  protected
    procedure CheckNumber(const number: string); virtual;
    function DoGetNextNumber(const number: string): string; virtual; abstract;
  public
    constructor Create(length: Integer); overload;
    constructor Create(minLength, maxLength: Integer); overload;
    function IsValid(const number: string): Boolean;
    function Validate(const number: string; out errorMessage: string): Boolean; virtual;
    function GetNextNumber(const number: string): string; virtual;
    function GetFirstNumber: string; virtual; abstract;
    function GetLastNumber: string; virtual; abstract;
    property MinLength: Integer read GetMinLength;
    property MaxLength: Integer read GetMaxLength;
  end;

  /// <summary>
  /// Represents a composite number rule which consists of some ordered number rules.
  /// </summary>
  TCompositeNumberRule = class(TInterfacedObject, INumberRule)
  private
    fRules: TArray<INumberRule>;
    fMinLength: Integer;
    fMaxLength: Integer;
    function GetMinLength: Integer;
    function GetMaxLength: Integer;
  protected
    procedure CheckLength(const number: string);
    function ShouldResetSequences(const parts: TArray<string>): Boolean; virtual;
    function ParseNumberParts(const number: string): TArray<string>; virtual;
    function CreateNumber(const parts: TArray<string>): string;
    function GenerateNextNumber(const parts: TArray<string>): string; virtual;
  public
    constructor Create(const rules: TArray<INumberRule>);
    function IsValid(const number: string): Boolean;
    function Validate(const number: string; out errorMessage: string): Boolean;
    function GetNextNumber(const number: string): string;
    function GetFirstNumber: string;
    function GetLastNumber: string;
    property MinLength: Integer read GetMinLength;
    property MaxLength: Integer read GetMaxLength;
  end;

  ISequenceResetTrigger = interface
    ['{F87EF01B-655B-4CEC-A211-7CFF12C831D6}']
    function CanTrigger(const value: string): Boolean;
  end;

  ISequenceRule = interface
    ['{1BB08C01-DCED-4D51-B3E4-0CD314AF3230}']
//    function HasNextNumber(const number: string): Boolean;
//    property ElementCount: Integer;
  end;

  TGetDateTimeFunc = reference to function: TDateTime;

  TCodePartRule = class(TNumberRuleBase)
  private
    fCode: string;
  protected
    function DoGetNextNumber(const number: string): string; override;
  public
    constructor Create(const code: string);
    function Validate(const number: string; out errorMessage: string): Boolean; override;
    function GetFirstNumber: string; override;
    function GetLastNumber: string; override;
  end;

  /// <remarks>
  /// WeekOf uses the ISO 8601 standard to define the week of the year.
  /// That is, a week is defined as running from Monday through Sunday,
  /// and the first week of the year is the one that includes the first
  /// Thursday of the year (the first week that includes four or more days
  /// in the year).
  /// </remarks>
  TDateTimePartRule = class(TNumberRuleBase, ISequenceResetTrigger)
  private
    fFormat: string;
    fFunc: TGetDateTimeFunc;
    function DoGetSystemDateTime: TDateTime;
  protected
    function DoGetNextNumber(const number: string): string; override;
    function GetDateTimeString: string; virtual;
    property Format: string read fFormat;
    { ISequenceResetTrigger }
    function CanTrigger(const value: string): Boolean; virtual;
  public
    constructor Create(const format: string); overload;
    constructor Create(const format: string; func: TGetDateTimeFunc); overload;
    function Validate(const number: string; out errorMessage: string): Boolean; override;
    function GetFirstNumber: string; override;
    function GetLastNumber: string; override;
  end;

  TSequencePartRule = class(TNumberRuleBase, ISequenceRule)
  private
    fCalculator: TBaseNCalculator;
    fFirstNumber: string;
    fLastNumber: string;
    function GetElements: string;
  protected
    function DoGetNextNumber(const number: string): string; override;
    property Elements: string read GetElements;
  public
    constructor Create(const elements: string; length: Integer;
      const firstNumber, lastNumber: string);
    function Validate(const number: string; out errorMessage: string): Boolean; override;
    function GetFirstNumber: string; override;
    function GetLastNumber: string; override;
    function GetEndNumber(const startNumber: string; const quantity: Int64; var carried: Boolean): string;
  end;

  ENumberException = class(Exception);
  ENumberOverflowException = class(ENumberException);

implementation

uses
  DateUtils,
  StrUtils,
  Spring.Extensions.ResourceStrings;

{$REGION 'TNumberRuleBase'}

constructor TNumberRuleBase.Create(minLength, maxLength: Integer);
begin
  inherited Create;
  fMinLength := minLength;
  fMaxLength := maxLength;
end;

constructor TNumberRuleBase.Create(length: Integer);
begin
  Create(length, length);
end;

function TNumberRuleBase.IsValid(const number: string): Boolean;
var
  errorMessage: string;
begin
  Result := Validate(number, errorMessage);
end;

procedure TNumberRuleBase.CheckNumber(const number: string);
var
  errorMessage: string;
begin
  if not Validate(number, errorMessage) then
  begin
    raise ENumberException.Create(errorMessage);
  end;
end;

function TNumberRuleBase.Validate(const number: string; out errorMessage: string): Boolean;
begin
  Result := (Length(number) >= Self.MinLength) and (Length(number) <= Self.MaxLength);
  if not Result then
  begin
    errorMessage := Format(SIllegalNumberLength, [number]);
  end;
end;

function TNumberRuleBase.GetMinLength: Integer;
begin
  Result := fMinLength;
end;

function TNumberRuleBase.GetNextNumber(const number: string): string;
begin
  CheckNumber(number);
  Result := DoGetNextNumber(number);
end;

function TNumberRuleBase.GetMaxLength: Integer;
begin
  Result := fMaxLength;
end;

{$ENDREGION}


{$REGION 'TCompositeNumberRule'}

constructor TCompositeNumberRule.Create(const rules: TArray<INumberRule>);
var
  rule: INumberRule;
begin
  inherited Create;
  fRules := rules;
  for rule in fRules do
  begin
    Inc(fMinLength, rule.MinLength);
    Inc(fMaxLength, rule.MaxLength);
  end;
end;

function TCompositeNumberRule.CreateNumber(const parts: TArray<string>): string;
var
  s: string;
begin
  Result := '';
  for s in parts do
  begin
    Result := Result + s;
  end;
end;

procedure TCompositeNumberRule.CheckLength(const number: string);
begin
  if (Length(number) < MinLength) or (Length(number) > MaxLength) then
  begin
    raise ENumberException.CreateResFmt(@SIllegalNumberLength, [number]);
  end;
end;

function TCompositeNumberRule.IsValid(const number: string): Boolean;
var
  errorMessage: string;
begin
  Result := Validate(number, errorMessage);
end;

function TCompositeNumberRule.Validate(const number: string;
  out errorMessage: string): Boolean;
var
  parts: TArray<string>;
  i: Integer;
begin
  if (Length(number) < MinLength) or (Length(number) > MaxLength) then
  begin
    errorMessage := Format(SIllegalNumberLength, [number]);
    Exit(False);
  end;
  Result := True;
  try
    parts := ParseNumberParts(number);
    for i := 0 to High(fRules) do
    begin
      if not fRules[i].Validate(parts[i], errorMessage) then
      begin
        Exit(False);
      end;
    end;
  except
    on e: Exception do
    begin
      errorMessage := e.Message;
      Result := False;
    end;
  end;
end;

function TCompositeNumberRule.ParseNumberParts(
  const number: string): TArray<string>;
var
  index: Integer;
  i: Integer;
begin
  CheckLength(number);
  SetLength(Result, Length(fRules));
  index := 1;
  for i := 0 to High(fRules) do
  begin
    Result[i] := Copy(number, index, fRules[i].MinLength);
    Inc(index, fRules[i].MinLength);
  end;
end;

function TCompositeNumberRule.ShouldResetSequences(
  const parts: TArray<string>): Boolean;
var
  rule: INumberRule;
  trigger: ISequenceResetTrigger;
  i: Integer;
begin
  Result := False;
  for i := 0 to High(fRules) do
  begin
    rule := fRules[i];
    if Supports(rule, ISequenceResetTrigger, trigger) and trigger.CanTrigger(parts[i]) then
    begin
      Exit(True);
    end;
  end;
end;

function TCompositeNumberRule.GenerateNextNumber(
  const parts: TArray<string>): string;
var
  newParts: TArray<string>;
  part: string;
  rule: INumberRule;
  sequence: ISequenceRule;
  isFirstSequence: Boolean;
  hasCarried: Boolean;
  resetSequences: Boolean;
  i: Integer;
begin
  SetLength(newParts, Length(parts));
  resetSequences := ShouldResetSequences(parts);
  isFirstSequence := True;
  hasCarried := False;
  for i := High(fRules) downto 0 do
  begin
    rule := fRules[i];
    part := parts[i];
    if Supports(rule, ISequenceRule, sequence) then
    begin
      if resetSequences then
      begin
        newParts[i] := rule.GetFirstNumber;
        Continue;
      end;
      if hasCarried or isFirstSequence then
      begin
        isFirstSequence := False;
        hasCarried := SameText(part, rule.GetLastNumber);
        if not hasCarried  then
        begin
          newParts[i] := rule.GetNextNumber(part);
        end
        else
        begin
          newParts[i] := rule.GetFirstNumber;
        end;
      end
      else
      begin
        newParts[i] := part;
      end;
    end
    else
    begin
      newParts[i] := rule.GetNextNumber(parts[i]);
    end;
  end;
  if hasCarried then
  begin
    raise ENumberOverflowException.CreateResFmt(@SNumberOverflow, [CreateNumber(parts)]);
  end;
  Result := CreateNumber(newParts);
end;

function TCompositeNumberRule.GetFirstNumber: string;
var
  rule: INumberRule;
begin
  Result := '';
  for rule in fRules do
  begin
    Result := Result + rule.GetFirstNumber;
  end;
end;

function TCompositeNumberRule.GetLastNumber: string;
var
  rule: INumberRule;
begin
  Result := '';
  for rule in fRules do
  begin
    Result := Result + rule.GetLastNumber;
  end;
end;

function TCompositeNumberRule.GetNextNumber(const number: string): string;
var
  parts: TArray<string>;
begin
  parts := ParseNumberParts(number);
  Result := GenerateNextNumber(parts);
end;

function TCompositeNumberRule.GetMinLength: Integer;
begin
  Result := fMinLength;
end;

function TCompositeNumberRule.GetMaxLength: Integer;
begin
  Result := fMaxLength;
end;

{$ENDREGION}


{$REGION 'TCodePartRule'}

constructor TCodePartRule.Create(const code: string);
begin
  inherited Create(Length(code));
  fCode := code;
end;

function TCodePartRule.DoGetNextNumber(const number: string): string;
begin
  Result := fCode;
end;

function TCodePartRule.Validate(const number: string; out errorMessage: string): Boolean;
begin
  Result := SameText(number, fCode);
  if not Result then
  begin
    errorMessage := Format(SUnexpectedCode, [number]);
  end;
end;

function TCodePartRule.GetFirstNumber: string;
begin
  Result := fCode;
end;

function TCodePartRule.GetLastNumber: string;
begin
  Result := fCode;
end;

{$ENDREGION}


{$REGION 'TDateTimePartRule'}

constructor TDateTimePartRule.Create(const format: string);
begin
  Create(format, DoGetSystemDateTime);
end;

constructor TDateTimePartRule.Create(const format: string;
  func: TGetDateTimeFunc);
begin
  inherited Create(Length(format));
  fFormat := format;
  fFunc := func;
end;

function TDateTimePartRule.CanTrigger(const value: string): Boolean;
begin
  Result := not SameText(GetDateTimeString, value);
end;

function TDateTimePartRule.Validate(const number: string; out errorMessage: string): Boolean;
begin
  Result := inherited Validate(number, errorMessage);
end;

function TDateTimePartRule.DoGetSystemDateTime: TDateTime;
begin
  Result := Now;
end;

function TDateTimePartRule.GetDateTimeString: string;
var
  value: TDateTime;
  startDay: TDateTime;
  endDay: TDateTime;
  week: Integer;
begin
  value := fFunc;
  week := WeekOf(value);
  startDay := StartOfTheWeek(value);
  endDay := EndOfTheWeek(value);
  Result := fFormat;
  if (Pos('WW', fFormat) = 0) or (YearOf(startDay) = YearOf(endDay)) then
  begin
    Result := StringReplace(Result, 'YYYY', FormatDateTime('YYYY', value), [rfIgnoreCase]);
    Result := StringReplace(Result, 'YYYY', FormatDateTime('YY', value), [rfIgnoreCase]);
  end
  else if week = 1 then
  begin
    Result := StringReplace(Result, 'YYYY', FormatDateTime('YYYY', endDay), [rfIgnoreCase]);
    Result := StringReplace(Result, 'YYYY', FormatDateTime('YY', endDay), [rfIgnoreCase]);
  end
  else
  begin
    Result := StringReplace(Result, 'YYYY', FormatDateTime('YYYY', startDay), [rfIgnoreCase]);
    Result := StringReplace(Result, 'YYYY', FormatDateTime('YY', startDay), [rfIgnoreCase]);
  end;
  Result := StringReplace(Result, 'MM', FormatDateTime('MM', value), [rfIgnoreCase]);
  Result := StringReplace(Result, 'DD', FormatDateTime('DD', value), [rfIgnoreCase]);
  Result := StringReplace(Result, 'WW', SysUtils.Format('%.2d', [week]), [rfIgnoreCase]);
  Result := StringReplace(Result, 'HH', FormatDateTime('HH', value), [rfIgnoreCase]);
  Result := StringReplace(Result, 'NN', FormatDateTime('NN', value), [rfIgnoreCase]);
  Result := StringReplace(Result, 'ZZZ', FormatDateTime('ZZZ', value), [rfIgnoreCase]);
end;

function TDateTimePartRule.DoGetNextNumber(const number: string): string;
begin
  Result := GetDateTimeString;
end;

function TDateTimePartRule.GetFirstNumber: string;
begin
  Result := GetDateTimeString;
end;

function TDateTimePartRule.GetLastNumber: string;
begin
  Result := GetDateTimeString;
end;

{$ENDREGION}


{$REGION 'TSequencePartRule'}

constructor TSequencePartRule.Create(const elements: string; length: Integer;
  const firstNumber, lastNumber: string);
begin
  inherited Create(length);
  fCalculator := TBaseNCalculator.Create(elements);
  fFirstNumber := fCalculator.FormatNumber(firstNumber, length);
  fLastNumber := fCalculator.FormatNumber(lastNumber, length);
end;

function TSequencePartRule.DoGetNextNumber(const number: string): string;
var
  carried: Boolean;
begin
  Result := GetEndNumber(number, 2, carried);
  if carried then
  begin
    raise ENumberOverflowException.CreateResFmt(@SNumberOverflow, [number]);
  end;
end;

function TSequencePartRule.Validate(const number: string; out errorMessage: string): Boolean;
begin
  Result := inherited Validate(number, errorMessage);
  if Result then
  begin
    Result := fCalculator.IsValid(number);
    if not Result then
    begin
      errorMessage := SIllegalElement;
    end;
  end;
end;

function TSequencePartRule.GetFirstNumber: string;
begin
  Result := fFirstNumber;
end;

function TSequencePartRule.GetLastNumber: string;
begin
  Result := fLastNumber;
end;

function TSequencePartRule.GetEndNumber(const startNumber: string; const quantity: Int64;
  var carried: Boolean): string;
begin
  Result := fCalculator.GetEndNumber(startNumber, quantity);
  carried := Length(Result) > Self.MinLength;
  if carried then
  begin
    Result := GetFirstNumber;
  end;
end;

function TSequencePartRule.GetElements: string;
begin
  Result := fCalculator.Elements;
end;

{$ENDREGION}

end.
