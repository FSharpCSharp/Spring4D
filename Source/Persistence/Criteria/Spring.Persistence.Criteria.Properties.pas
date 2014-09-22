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

unit Spring.Persistence.Criteria.Properties;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Restrictions,
  Spring.Persistence.SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  A factory for property-specific criterion and projection instances.
  ///	</summary>
  {$ENDREGION}
  TProperty = class(TInterfacedObject, IProperty)
  private
    FPropertyName: string;
    FEntityClass: TClass;
  protected
    constructor Create; virtual;
  protected
    function Eq(const AValue: TValue): ICriterion; virtual;
    function NotEq(const AValue: TValue): ICriterion; virtual;
    function GEq(const AValue: TValue): ICriterion; virtual;
    function Gt(const AValue: TValue): ICriterion; virtual;
    function IsNull: ICriterion; virtual;
    function IsNotNull: ICriterion; virtual;
    function Like(const AValue: string; AMatchMode: TMatchMode = mmExact): ICriterion; virtual;
    function NotLike(const AValue: string; AMatchMode: TMatchMode = mmExact): ICriterion; virtual;
    function LEq(const AValue: TValue): ICriterion; virtual;
    function Lt(const AValue: TValue): ICriterion; virtual;
    function &In<T>(const AValues: TArray<T>): ICriterion;
    function NotIn<T>(const AValues: TArray<T>): ICriterion;
    function &InStr(const AValues: TArray<string>): ICriterion; virtual;
    function NotInStr(const AValues: TArray<string>): ICriterion; virtual;
    function &InInt(const AValues: TArray<Integer>): ICriterion; virtual;
    function NotInInt(const AValues: TArray<Integer>): ICriterion; virtual;
    function Between(const ALow, AHigh: TValue): ICriterion; virtual;
    function Asc: IOrder; virtual;
    function Desc: IOrder; virtual;

    function EqProperty(AOther: IProperty): ICriterion; overload; virtual;
    function EqProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;
    function NeProperty(AOther: IProperty): ICriterion; overload; virtual;
    function NeProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;
    function GeProperty(AOther: IProperty): ICriterion; overload; virtual;
    function GeProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;
    function GtProperty(AOther: IProperty): ICriterion; overload; virtual;
    function GtProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;
    function LeProperty(AOther: IProperty): ICriterion; overload; virtual;
    function LeProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;
    function LtProperty(AOther: IProperty): ICriterion; overload; virtual;
    function LtProperty(const AOtherPropertyName: string): ICriterion; overload; virtual;

    function GetEntityClass: TClass; virtual;
    function GetPropertyName: string; virtual;
    procedure SetEntityClass(AClass: TClass); virtual;
    procedure SetPropertyName(const Value: string); virtual;
  public
    class function ForName(const APropertyName: string): TProperty;

    property PropertyName: string read GetPropertyName write SetPropertyName;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  A factory for property-specific criterion and projection instances.
  ///	</summary>
  {$ENDREGION}
  TProperty<T: class> = class(TProperty)
  public
    class function ForName(const APropertyName: string): TProperty;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Criteria.Criterion.PropertyExpression,
  Spring.Persistence.Criteria.Order;

{ TProperty }

constructor TProperty.Create;
begin
  inherited Create;
end;

function TProperty.Asc: IOrder;
begin
  Result := TOrder.Asc(FPropertyName);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.Between(const ALow, AHigh: TValue): ICriterion;
begin
  Result := TRestrictions.Between(PropertyName, ALow, AHigh);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.Desc: IOrder;
begin
  Result := TOrder.Desc(FPropertyName);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.Eq(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.Eq(FPropertyName, AValue);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.EqProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woEqual
    , TSQLTable.CreateFromClass(FEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.EqProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woEqual
    , TSQLTable.CreateFromClass(FEntityClass), TSQLTable.CreateFromClass(AOther.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

class function TProperty.ForName(const APropertyName: string): TProperty;
begin
  Result := TProperty.Create;
  Result.FPropertyName := APropertyName;
end;

function TProperty.GeProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woMoreOrEqual
    , TSQLTable.CreateFromClass(FEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.GeProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woMoreOrEqual
    , TSQLTable.CreateFromClass(FEntityClass), TSQLTable.CreateFromClass(AOther.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.GEq(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.GEq(FPropertyName, AValue);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.GetEntityClass: TClass;
begin
  Result := FEntityClass;
end;

function TProperty.GetPropertyName: string;
begin
  Result := FPropertyName;
end;

function TProperty.Gt(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.Gt(FPropertyName, AValue);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.GtProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woMore
    , TSQLTable.CreateFromClass(FEntityClass), TSQLTable.CreateFromClass(AOther.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.GtProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woMore
    , TSQLTable.CreateFromClass(FEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.&In<T>(const AValues: TArray<T>): ICriterion;
begin
  Result := TRestrictions.&In<T>(FPropertyName, AValues);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.InInt(const AValues: TArray<Integer>): ICriterion;
begin
  Result := &In<Integer>(AValues);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.InStr(const AValues: TArray<string>): ICriterion;
begin
  Result := &In<string>(AValues);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.IsNotNull: ICriterion;
begin
  Result := TRestrictions.IsNotNull(FPropertyName);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.IsNull: ICriterion;
begin
  Result := TRestrictions.IsNull(FPropertyName);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.LeProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woLessOrEqual
    , TSQLTable.CreateFromClass(FEntityClass), TSQLTable.CreateFromClass(AOther.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.LeProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woLessOrEqual
    , TSQLTable.CreateFromClass(FEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.LEq(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.LEq(FPropertyName, AValue);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.Like(const AValue: string; AMatchMode: TMatchMode): ICriterion;
begin
  Result := TRestrictions.Like(FPropertyName, AValue, AMatchMode);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.Lt(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.Lt(FPropertyName, AValue);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.LtProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woLess
    , TSQLTable.CreateFromClass(FEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.LtProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woLess
    , TSQLTable.CreateFromClass(FEntityClass), TSQLTable.CreateFromClass(AOther.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NeProperty(AOther: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOther.GetPropertyName, woNotEqual
    , TSQLTable.CreateFromClass(FEntityClass), TSQLTable.CreateFromClass(AOther.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NeProperty(const AOtherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, AOtherPropertyName, woNotEqual
    , TSQLTable.CreateFromClass(FEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NotEq(const AValue: TValue): ICriterion;
begin
  Result := TRestrictions.NotEq(FPropertyName, AValue);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NotIn<T>(const AValues: TArray<T>): ICriterion;
begin
  Result := TRestrictions.NotIn<T>(FPropertyName, AValues);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NotInInt(const AValues: TArray<Integer>): ICriterion;
begin
  Result := NotIn<Integer>(AValues);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NotInStr(const AValues: TArray<string>): ICriterion;
begin
  Result := NotIn<string>(AValues);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NotLike(const AValue: string; AMatchMode: TMatchMode): ICriterion;
begin
  Result := TRestrictions.NotLike(FPropertyName, AValue, AMatchMode);
  Result.SetEntityClass(GetEntityClass);
end;

procedure TProperty.SetEntityClass(AClass: TClass);
begin
  FEntityClass := AClass;
end;

procedure TProperty.SetPropertyName(const Value: string);
begin
  FPropertyName := Value;
end;

{ TGenericProperty<T> }

class function TProperty<T>.ForName(const APropertyName: string): TProperty;
begin
  Result := TProperty.ForName(APropertyName);
  Result.FEntityClass := T;
end;

end.
