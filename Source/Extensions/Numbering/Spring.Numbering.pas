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

unit Spring.Numbering;

{$I Spring.inc}

interface

uses
  Classes,
  Contnrs,
  Windows,
  SysUtils,
  Spring.System,
  Spring.Collections;

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
  /// Retrieves and updates the number.
  /// </summary>
  TNumberProc = reference to procedure(var number: string);

  /// <summary>
  /// Represents a data source which can retrieve and update a number.
  /// </summary>
  INumberSource = interface
    ['{662FC755-C0CB-412A-BB5B-2F54A87E2EBC}']
    /// <summary>
    /// Selects and updates the number in the data source.
    /// </summary>
    procedure UpdateNumber(proc: TNumberProc);
  end;

  /// <summary>
  /// Generates an available number.
  /// </summary>
  INumberGenerator = interface
    ['{52A04371-9251-4A44-818F-D3B177B432A3}']
    /// <summary>
    /// Initializes the generator with a number rule and a source.
    /// </summary>
    procedure Initialize(const rule: INumberRule; const source: INumberSource);
    /// <summary>
    /// Retrieves the next available number and updates the number source.
    /// </summary>
    function NextNumber: string;
  end;

  TGetDateTimeFunc = reference to function: TDateTime;

  /// <summary>
  /// Number Rule Builder
  /// </summary>
  TNumberRuleBuilder = record
  private
    fRules: IList<INumberRule>;
    function GetRules: IList<INumberRule>;
  public
    class function Create: TNumberRuleBuilder; static;
    procedure Clear;
    function AddCode(const code: string): TNumberRuleBuilder;
    function AddDateTime(const format: string): TNumberRuleBuilder; overload;
    function AddDateTime(const format: string; getDateTimeFunc: TGetDateTimeFunc): TNumberRuleBuilder; overload;
    function AddDigits(const firstNumber, lastNumber: string): TNumberRuleBuilder;
    function AddLetters: TNumberRuleBuilder;
    function AddSequence(const elements: string; len: Integer; const firstNumber, lastNumber: string): TNumberRuleBuilder;
    function AddRule(const rule: INumberRule): TNumberRuleBuilder;
    function ToRule: INumberRule;
  end;

  /// <summary>
  /// Provides a default implementation of INumberGenerator.
  /// </summary>
  TNumberGenerator = class(TInterfacedObject, INumberGenerator)
  private
    fRule: INumberRule;
    fSource: INumberSource;
  public
    constructor Create(const rule: INumberRule; const source: INumberSource);
    procedure Initialize(const rule: INumberRule; const source: INumberSource);
    function NextNumber: string; virtual;
  end;

implementation

uses
  Spring.Numbering.Rules;

{$REGION 'TNumberRuleBuilder'}

class function TNumberRuleBuilder.Create: TNumberRuleBuilder;
begin
  Result.fRules := TCollections.CreateList<INumberRule>;
end;

function TNumberRuleBuilder.GetRules: IList<INumberRule>;
begin
  if fRules = nil then
  begin
    fRules := TCollections.CreateList<INumberRule>;
  end;
  Result := fRules;
end;

procedure TNumberRuleBuilder.Clear;
begin
  if fRules <> nil then
  begin
    fRules.Clear;
  end;
end;

function TNumberRuleBuilder.AddCode(const code: string): TNumberRuleBuilder;
var
  rule: INumberRule;
begin
  if Length(code) > 0 then
  begin
    rule := TCodePartRule.Create(code);
    AddRule(rule);
  end;
  Result := Self;
end;

function TNumberRuleBuilder.AddDigits(const firstNumber,
  lastNumber: string): TNumberRuleBuilder;
begin
  Result := AddSequence('0123456789', Length(firstNumber), firstNumber, lastNumber);
end;

function TNumberRuleBuilder.AddLetters: TNumberRuleBuilder;
begin
  Result := AddSequence('ABCDEFGHIJKLMNOPQRSTUVWXYZ', 1, 'A', 'Z');
end;

function TNumberRuleBuilder.AddDateTime(const format: string): TNumberRuleBuilder;
var
  rule: INumberRule;
begin
  rule := TDateTimePartRule.Create(format);
  Result := AddRule(rule);
end;

function TNumberRuleBuilder.AddDateTime(const format: string;
  getDateTimeFunc: TGetDateTimeFunc): TNumberRuleBuilder;
var
  rule: INumberRule;
begin
  rule := TDateTimePartRule.Create(format, getDateTimeFunc);
  Result := AddRule(rule);
end;

function TNumberRuleBuilder.AddSequence(const elements: string; len: Integer;
  const firstNumber, lastNumber: string): TNumberRuleBuilder;
var
  rule: INumberRule;
begin
  rule := TSequencePartRule.Create(elements, len, firstNumber, lastNumber);
  Result := AddRule(rule);
end;

function TNumberRuleBuilder.AddRule(
  const rule: INumberRule): TNumberRuleBuilder;
begin
  GetRules.Add(rule);
  Result := Self;
end;

function TNumberRuleBuilder.ToRule: INumberRule;
begin
  Result := TCompositeNumberRule.Create(GetRules.ToArray);
end;

{$ENDREGION}


{$REGION 'TNumberGenerator'}

constructor TNumberGenerator.Create(const rule: INumberRule;
  const source: INumberSource);
begin
  inherited Create;
  Initialize(rule, source);
end;

procedure TNumberGenerator.Initialize(const rule: INumberRule;
  const source: INumberSource);
begin
  TArgument.CheckNotNull(rule, 'rule');
  TArgument.CheckNotNull(source, 'source');
  fRule := rule;
  fSource := source;
end;

function TNumberGenerator.NextNumber: string;
var
  number: string;
begin
  fSource.UpdateNumber(
    procedure(var lastNumber: string)
    begin
      number := fRule.GetNextNumber(lastNumber);
      lastNumber := number;
    end
  );
  Result := number;
end;

{$ENDREGION}

end.
