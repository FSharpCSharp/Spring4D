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

unit Spring.Objects;

{$I Spring.inc}

{
  TMoney, TCurrency
  TQuantity, TUnit
  TRange<T>
  TRatio<T>
  TTimePoint (TDateTime, TDate, TTime)
}

interface

uses
  SysUtils,
  ComObj,
  Generics.Defaults,
  Spring.System;

type
  TRatio<T> = record
  strict private
    fNumerator: T;
    fDenominator: T;
  public
    constructor Create(const numerator, denominator: T);
    function ToString: string;
    property Numerator: T read fNumerator;
    property Denominator: T read fDenominator;
  end;


  {$REGION 'TMoney'}

  TCurrency = record
  end;

  TMoney = record
  private
    fAmount: Currency;
  private
    type
      TMoneyRatio = TRatio<TMoney>;
  public
    constructor Create(const amount: Currency);
    function Add(const amount: TMoney): TMoney; overload;
    function Subtract(const amount: TMoney): TMoney; overload;
    function Divide(const amount: TMoney): TMoneyRatio; overload;
    function Negative: TMoney;
    function Equals(const money: TMoney): Boolean;
    function ToString: string;
    property Amount: Currency read fAmount;
    { Comparison }
    class operator NotEqual(const left, right: TMoney): Boolean;
    class operator Equal(const left, right: TMoney): Boolean;
    { Conversion }
    class operator Implicit(const value: Currency): TMoney;
    { Binary }
    class operator Add(const left, right: TMoney): TMoney;
    class operator Subtract(const left, right: TMoney): TMoney;
    class operator Divide(const left, right: TMoney): TMoneyRatio;
  end;

  TMoneyRatio = TMoney.TMoneyRatio;

  {$ENDREGION}


  IRange<T> = interface
  {$REGION 'Property Getters and Setters'}
    function GetIsEmpty: Boolean;
  {$ENDREGION}
    function Contains(const value: T): Boolean; overload;
    function Contains(const range: IRange<T>): Boolean; overload;
    function CompareTo(const range: IRange<T>): Integer;
    function Intersect(const range: IRange<T>): IRange<T>;
    function Union(const range: IRange<T>): IRange<T>;
    function Complement(const range: IRange<T>): IRange<T>;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TRange<T> = record
  private
    fComparer: IComparer<T>;
    fStart: TNullable<T>;
    fEnd: TNullable<T>;
    fIncludeStart: Boolean;
    fIncludeEnd: Boolean;
    function GetIsOpen: Boolean;
    function GetIsClosed: Boolean;
    function GetIsEmpty: Boolean;
    function GetComparer: IComparer<T>;
  public
    constructor Create(const start, &end: TNullable<T>;
      includeStart, includeEnd: Boolean);
    function Contains(const value: T): Boolean; overload;
    function Contains(const range: TRange<T>): Boolean; overload;
    property Start: TNullable<T> read fStart;
    property &End: TNullable<T> read fEnd;
    property IncludeStart: Boolean read fIncludeStart;
    property IncludeEnd: Boolean read fIncludeEnd;
    property IsOpen: Boolean read GetIsOpen;
    property IsClosed: Boolean read GetIsClosed;
    property IsEmpty: Boolean read GetIsEmpty;
//    function Intersect(const range: TRange<T>): TRange<T>;
//    function Union(const range: TRange<T>): TRange<T>;
//    function Complement(const range: TRange<T>): TRange<T>;
//    function ToString: string;
    { class function }
    class function Between(const start, &end: T): TRange<T>; static;
    class function EqualTo(const value: T): TRange<T>; static;
    class function GreaterThan(const value: T): TRange<T>; static;
    class function GreaterThanOrEqualTo(const value: T): TRange<T>; static;
    class function LessThan(const value: T): TRange<T>; static;
    class function LessThanOrEqualTo(const value: T): TRange<T>; static;
    { Comparison }
    class operator Equal(const left, right: TRange<T>): Boolean;
    class operator NotEqual(const left, right: TRange<T>): Boolean;
  end;

  TDateTimeRange = TRange<TDateTime>;
  TDateRange     = TRange<TDate>;
  TTimeRange     = TRange<TTime>;

  TIntegerRange  = TRange<Integer>;
  TInt64Range    = TRange<Int64>;

  TDoubleRange   = TRange<Double>;
  TCurrencyRange = TRange<Currency>;

implementation


{$REGION 'TRatio<T>'}

constructor TRatio<T>.Create(const numerator, denominator: T);
begin
  fNumerator   := numerator;
  fDenominator := denominator;
end;

function TRatio<T>.ToString: string;
begin
  Result := '';
end;

{$ENDREGION}


{$REGION 'TMoney<T>'}

constructor TMoney.Create(const amount: Currency);
begin
  fAmount := amount;
end;

function TMoney.Equals(const money: TMoney): Boolean;
begin
  Result := fAmount = money.fAmount;
end;

function TMoney.Add(const amount: TMoney): TMoney;
begin
  Result := fAmount + amount.fAmount;
end;

function TMoney.Subtract(const amount: TMoney): TMoney;
begin
  Result := fAmount - amount.fAmount;
end;

function TMoney.Divide(const amount: TMoney): TMoneyRatio;
begin
//  if amount.fAmount = 0 then
//  begin
//    Result := INFINITE;
//  end
//  else
//  begin
//    Result := Self.fAmount / amount.fAmount;   // TEMP
//  end;
  Result := TMoneyRatio.Create(Self, amount);
end;

function TMoney.Negative: TMoney;
begin
  Result := TMoney.Create(fAmount * -1);
end;

function TMoney.ToString: string;
begin
  Result := Format('%m', [fAmount]);
end;

class operator TMoney.Equal(const left, right: TMoney): Boolean;
begin
  Result := left.Equals(right);
end;

class operator TMoney.NotEqual(const left, right: TMoney): Boolean;
begin
  Result := not left.Equals(right);
end;

class operator TMoney.Implicit(const value: Currency): TMoney;
begin
  Result := TMoney.Create(value);
end;

class operator TMoney.Add(const left, right: TMoney): TMoney;
begin
  Result := left.Add(right);
end;

class operator TMoney.Subtract(const left, right: TMoney): TMoney;
begin
  Result := left.Subtract(right);
end;

class operator TMoney.Divide(const left, right: TMoney): TMoneyRatio;
begin
  Result := left.Divide(right);
end;

{$ENDREGION}


{$REGION 'TRange<T>'}

constructor TRange<T>.Create(const start, &end: TNullable<T>;
  includeStart, includeEnd: Boolean);
begin
//  inherited Create;
  fComparer := TComparer<T>.Default;
  if (not start.HasValue or not &end.HasValue) or
    (GetComparer.Compare(start.value, &end.value) <= 0) then
  begin
    fStart        := start;
    fEnd          := &end;
    fIncludeStart := includeStart;
    fIncludeEnd   := includeEnd;
  end
  else
  begin
    fStart        := &end;
    fEnd          := start;
    fIncludeStart := includeEnd;
    fIncludeEnd   := includeStart;
  end;
end;

function TRange<T>.GetComparer: IComparer<T>;
begin
  if fComparer = nil then
  begin
    fComparer := TComparer<T>.Default;
  end;
end;

function TRange<T>.Contains(const value: T): Boolean;
var
  m, n: Integer;
begin
  Result := True;
  if fStart.HasValue then
  begin
    m := GetComparer.Compare(value, fStart.value);
    Result := (m > 0) or (fIncludeStart and (m = 0));
  end;
  if fEnd.HasValue then
  begin
    n := GetComparer.Compare(value, fEnd.value);
    Result := Result and ((n < 0) or (fIncludeEnd and (n = 0)));
  end;
end;

function TRange<T>.Contains(const range: TRange<T>): Boolean;
begin
  Result := range.Start.HasValue and Contains(range.Start.value);
  Result := Result and range.&End.HasValue and Contains(range.&End.value);
end;

function TRange<T>.GetIsOpen: Boolean;
begin
  Result := not IncludeStart and not IncludeEnd;
end;

function TRange<T>.GetIsClosed: Boolean;
begin
  Result := IncludeStart and IncludeEnd;
end;

function TRange<T>.GetIsEmpty: Boolean;
begin
  Result := not fStart.HasValue and not fEnd.HasValue;
end;

class function TRange<T>.Between(const start, &end: T): TRange<T>;
begin
  Result := TRange<T>.Create(TNullable<T>.Create(start), TNullable<T>.Create(&end), True, True);
end;

class function TRange<T>.EqualTo(const value: T): TRange<T>;
begin
  Result := TRange<T>.Between(value, value);
end;

class function TRange<T>.GreaterThan(const value: T): TRange<T>;
begin
  Result := TRange<T>.Create(value, nil, False, False);
end;

class function TRange<T>.GreaterThanOrEqualTo(const value: T): TRange<T>;
begin
  Result := TRange<T>.Create(value, nil, True, False);
end;

class function TRange<T>.LessThan(const value: T): TRange<T>;
begin
  Result := TRange<T>.Create(nil, value, False, False);
end;

class function TRange<T>.LessThanOrEqualTo(const value: T): TRange<T>;
begin
  Result := TRange<T>.Create(nil, value, False, True);
end;

class operator TRange<T>.Equal(const left, right: TRange<T>): Boolean;
begin
//  Result := (left.Start = right.Start) and
//    (left.&End = right.&End) and
//    (left.IncludeStart = right.IncludeStart) and
//    (left.IncludeEnd = right.IncludeEnd);
end;

class operator TRange<T>.NotEqual(const left, right: TRange<T>): Boolean;
begin
//  Result := (left.Start <> right.Start) or
//    (left.&End <> right.&End) or
//    (left.IncludeStart <> right.IncludeStart) or
//    (left.IncludeEnd <> right.IncludeEnd);
end;

{$ENDREGION}


end.
