(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit Core.Criteria.Restrictions;

interface

{$I sv.inc}

uses
  Core.Interfaces
  ,Core.Criteria.Criterion
  ,Rtti
  ,SQL.Types
  ,Core.Criteria.Criterion.Disjunction
  ,Core.Criteria.Criterion.Conjunction
  ;


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
    class function &In<T>(const APropertyName: string; const AValues: TArray<T>): ICriterion;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Apply an <b>"not in"</b> constraint to the named property.
    ///	</summary>
    {$ENDREGION}
    class function NotIn<T>(const APropertyName: string; const AValues: TArray<T>): ICriterion;

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
    class function Conjunction(): TConjunction;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Group expressions together in a single disjunction (A or B or C...)
    ///	</summary>
    {$ENDREGION}
    class function Disjunction(): TDisjunction;
  end;

implementation

uses
  Core.Criteria.Criterion.SimpleExpression
  ,Core.Criteria.Criterion.NullExpression
  ,Core.Criteria.Criterion.LikeExpression
  ,Core.Criteria.Criterion.InExpression
  ,Core.Criteria.Criterion.LogicalExpression
  ,Core.Criteria.Criterion.PropertyExpression
  ,Core.Criteria.Criterion.BetweenExpression
  ;

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
  Result := TConjunction.Create();
end;

class function TRestrictions.Disjunction: TDisjunction;
begin
  Result := TDisjunction.Create();
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
