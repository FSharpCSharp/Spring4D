{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Persistence.Criteria.Restrictions;

interface

uses
  Spring,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Criterion.Conjunction,
  Spring.Persistence.Criteria.Criterion.Disjunction,
  Spring.Persistence.SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  The criterion package may be used by applications as a framework for
  ///	  building new kinds of Criterion. However, it is intended that most
  ///	  applications will simply use the built-in criterion types via the
  ///	  static factory methods of this class.
  ///	</summary>
  {$ENDREGION}
  TRestrictions = class sealed
  public
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply an <b>"equal"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function Eq(const APropertyName: string; const AValue: TValue): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a <b>"not equal"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function NotEq(const APropertyName: string; const AValue: TValue): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a <b>"greater than or equal"</b> constraint to the named
    ///	  property.
    ///	</summary>
    {$ENDREGION}
    class function GEq(const APropertyName: string; const AValue: TValue): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a <b>"greater than"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function Gt(const APropertyName: string; const AValue: TValue): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply an <b>"is null"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function IsNull(const APropertyName: string): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply an <b>"is not null"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function IsNotNull(const APropertyName: string): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a <b>"like"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function Like(const APropertyName, AValue: string; AMatchMode: TMatchMode = mmExact): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a <b>"not like"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function NotLike(const APropertyName, AValue: string; AMatchMode: TMatchMode = mmExact): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a <b>"less than or equal"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function LEq(const APropertyName: string; const AValue: TValue): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a <b>"less than"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function Lt(const APropertyName: string; const AValue: TValue): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply an <b>"in"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function &In(const APropertyName: string; const AValues: TArray<TValue>): ICriterion; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply an <b>"in"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function &In<T>(const APropertyName: string; const AValues: TArray<T>): ICriterion; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply an <b>"not in"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function NotIn(const APropertyName: string; const AValues: TArray<TValue>): ICriterion; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply an <b>"not in"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function NotIn<T>(const APropertyName: string; const AValues: TArray<T>): ICriterion; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Return the conjuction of two expressions.
    ///	</summary>
    {$ENDREGION}
    class function &And(ALeft, ARight: ICriterion): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Return the disjuction of two expressions.
    ///	</summary>
    {$ENDREGION}
    class function &Or(ALeft, ARight: ICriterion): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Return the negation of an expression.
    ///	</summary>
    {$ENDREGION}
    class function &Not(AExpression: ICriterion): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a "<b>not equal</b>" constraint to two properties.
    ///	</summary>
    {$ENDREGION}
    class function NeProperty(const APropertyName, AOtherPropertyName: string): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply an "<b>equal</b>" constraint to two properties.
    ///	</summary>
    {$ENDREGION}
    class function EqProperty(const APropertyName, AOtherPropertyName: string): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a "<b>greater than or equal</b>" constraint to two properties.
    ///	</summary>
    {$ENDREGION}
    class function GeProperty(const APropertyName, AOtherPropertyName: string): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a "<b>greater than</b>" constraint to two properties.
    ///	</summary>
    {$ENDREGION}
    class function GtProperty(const APropertyName, AOtherPropertyName: string): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a "<b>less than or equal</b>" constraint to two properties.
    ///	</summary>
    {$ENDREGION}
    class function LeProperty(const APropertyName, AOtherPropertyName: string): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a "<b>less than</b>" constraint to two properties.
    ///	</summary>
    {$ENDREGION}
    class function LtProperty(const APropertyName, AOtherPropertyName: string): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply a "<b>between</b>" constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function Between(const APropertyName: string; const ALow, AHigh: TValue): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Group expressions together in a single conjunction (A and B and C...)
    ///	</summary>
    {$ENDREGION}
    class function Conjunction: TConjunction;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Group expressions together in a single disjunction (A or B or C...)
    ///	</summary>
    {$ENDREGION}
    class function Disjunction: TDisjunction;
  end;

implementation

uses
  Spring.Persistence.Criteria.Criterion.SimpleExpression,
  Spring.Persistence.Criteria.Criterion.NullExpression,
  Spring.Persistence.Criteria.Criterion.LikeExpression,
  Spring.Persistence.Criteria.Criterion.InExpression,
  Spring.Persistence.Criteria.Criterion.LogicalExpression,
  Spring.Persistence.Criteria.Criterion.PropertyExpression,
  Spring.Persistence.Criteria.Criterion.BetweenExpression;

{ TRestrictions }

class function TRestrictions.&Or(ALeft, ARight: ICriterion): ICriterion;
begin
  Result := TLogicalExpression.Create(ALeft, ARight, woOr);
end;

class function TRestrictions.NeProperty(const APropertyName, AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(APropertyName, AOtherPropertyName, woNotEqual);
end;

class function TRestrictions.&Not(AExpression: ICriterion): ICriterion;
begin
  Result := TLogicalExpression.Create(AExpression, nil, woNot);
end;

class function TRestrictions.&And(ALeft, ARight: ICriterion): ICriterion;
begin
  Result := TLogicalExpression.Create(ALeft, ARight, woAnd);
end;

class function TRestrictions.Between(const APropertyName: string; const ALow,
  AHigh: TValue): ICriterion;
begin
  Result := TBetweenExpression.Create(APropertyName, ALow, AHigh, woBetween);
end;

class function TRestrictions.Conjunction: TConjunction;
begin
  Result := TConjunction.Create;
end;

class function TRestrictions.Disjunction: TDisjunction;
begin
  Result := TDisjunction.Create;
end;

class function TRestrictions.Eq(const APropertyName: string;
  const AValue: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(APropertyName, AValue, woEqual);
end;

class function TRestrictions.EqProperty(const APropertyName, AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(APropertyName, AOtherPropertyName, woEqual);
end;

class function TRestrictions.GeProperty(const APropertyName, AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(APropertyName, AOtherPropertyName, woMoreOrEqual);
end;

class function TRestrictions.GEq(const APropertyName: string; const AValue: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(APropertyName, AValue, woMoreOrEqual);
end;

class function TRestrictions.Gt(const APropertyName: string; const AValue: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(APropertyName, AValue, woMore);
end;

class function TRestrictions.GtProperty(const APropertyName, AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(APropertyName, AOtherPropertyName, woMore);
end;

class function TRestrictions.&In(const APropertyName: string;
  const AValues: TArray<TValue>): ICriterion;
begin
  Result := TInExpression.Create(APropertyName, AValues, woIn);
end;

class function TRestrictions.&In<T>(const APropertyName: string;
  const AValues: TArray<T>): ICriterion;
begin
  Result := TInExpression<T>.Create(APropertyName, AValues, woIn);
end;

class function TRestrictions.IsNotNull(const APropertyName: string): ICriterion;
begin
  Result := TNullExpression.Create(APropertyName, woIsNotNull);
end;

class function TRestrictions.IsNull(const APropertyName: string): ICriterion;
begin
  Result := TNullExpression.Create(APropertyName, woIsNull);
end;

class function TRestrictions.LeProperty(const APropertyName, AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(APropertyName, AOtherPropertyName, woLessOrEqual);
end;

class function TRestrictions.LEq(const APropertyName: string; const AValue: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(APropertyName, AValue, woLessOrEqual);
end;

class function TRestrictions.Like(const APropertyName, AValue: string; AMatchMode: TMatchMode): ICriterion;
begin
  Result := TLikeExpression.Create(APropertyName, AValue, woLike, AMatchMode);
end;

class function TRestrictions.Lt(const APropertyName: string; const AValue: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(APropertyName, AValue, woLess);
end;

class function TRestrictions.LtProperty(const APropertyName, AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(APropertyName, AOtherPropertyName, woLess);
end;

class function TRestrictions.NotEq(const APropertyName: string; const AValue: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(APropertyName, AValue, woNotEqual);
end;

class function TRestrictions.NotIn(const APropertyName: string;
  const AValues: TArray<TValue>): ICriterion;
begin
  Result := TInExpression.Create(APropertyName, AValues, woNotIn);
end;

class function TRestrictions.NotIn<T>(const APropertyName: string;
  const AValues: TArray<T>): ICriterion;
begin
  Result := TInExpression<T>.Create(APropertyName, AValues, woNotIn);
end;

class function TRestrictions.NotLike(const APropertyName, AValue: string; AMatchMode: TMatchMode): ICriterion;
begin
  Result := TLikeExpression.Create(APropertyName, AValue, woNotLike, AMatchMode);
end;

end.
