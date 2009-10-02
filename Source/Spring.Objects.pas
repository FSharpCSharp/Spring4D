{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://delphi-spring-framework.googlecode.com               }
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

{ TODO: TMoney, TCurrency, TQuantity, TUnit, TTimePoint (TDateTime, TDate, TTime),
  TRange<T>, TRatio }

interface

uses
  SysUtils, ComObj;

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

end.
