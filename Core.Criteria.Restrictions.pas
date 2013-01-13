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
  ;


type
  TRestrictions = class sealed
  public
    class function Eq(const APropertyName: string; const AValue: TValue): ICriterion;
    class function NotEq(const APropertyName: string; const AValue: TValue): ICriterion;
    class function IsNull(const APropertyName: string): ICriterion;
    class function IsNotNull(const APropertyName: string): ICriterion;
  end;

implementation

uses
  Core.Criteria.Criterion.SimpleExpression
  ,Core.Criteria.Criterion.NullExpression
  ,SQL.Types
  ;

{ TRestrictions }

class function TRestrictions.Eq(const APropertyName: string;
  const AValue: TValue): ICriterion;
begin
  {TODO -oOwner -cGeneral : implement Eq criterion}
  Result := TSimpleExpression.Create(APropertyName, AValue, woEqual);
end;

class function TRestrictions.IsNotNull(const APropertyName: string): ICriterion;
begin
  Result := TNullExpression.Create(APropertyName, woIsNotNull);
end;

class function TRestrictions.IsNull(const APropertyName: string): ICriterion;
begin
  Result := TNullExpression.Create(APropertyName, woIsNull);
end;

class function TRestrictions.NotEq(const APropertyName: string; const AValue: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(APropertyName, AValue, woNotEqual);
end;

end.
