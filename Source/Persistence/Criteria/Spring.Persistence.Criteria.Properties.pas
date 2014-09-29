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
  /// <summary>
  ///   A factory for property-specific criterion and projection instances.
  /// </summary>
  TProperty = class(TInterfacedObject, IProperty)
  private
    fPropertyName: string;
    fEntityClass: TClass;
  protected
    constructor Create; virtual;
  protected
    function Eq(const value: TValue): ICriterion; virtual;
    function NotEq(const value: TValue): ICriterion; virtual;
    function GEq(const value: TValue): ICriterion; virtual;
    function Gt(const value: TValue): ICriterion; virtual;
    function IsNull: ICriterion; virtual;
    function IsNotNull: ICriterion; virtual;
    function Like(const value: string; matchMode: TMatchMode = mmExact): ICriterion; virtual;
    function NotLike(const value: string; matchMode: TMatchMode = mmExact): ICriterion; virtual;
    function LEq(const value: TValue): ICriterion; virtual;
    function Lt(const value: TValue): ICriterion; virtual;
    function &In<T>(const value: TArray<T>): ICriterion;
    function NotIn<T>(const value: TArray<T>): ICriterion;
    function InStr(const value: TArray<string>): ICriterion; virtual;
    function NotInStr(const value: TArray<string>): ICriterion; virtual;
    function InInt(const value: TArray<Integer>): ICriterion; virtual;
    function NotInInt(const value: TArray<Integer>): ICriterion; virtual;
    function Between(const low, high: TValue): ICriterion; virtual;
    function Asc: IOrderBy; virtual;
    function Desc: IOrderBy; virtual;

    function EqProperty(const other: IProperty): ICriterion; overload; virtual;
    function EqProperty(const otherPropertyName: string): ICriterion; overload; virtual;
    function NeProperty(const other: IProperty): ICriterion; overload; virtual;
    function NeProperty(const otherPropertyName: string): ICriterion; overload; virtual;
    function GeProperty(const other: IProperty): ICriterion; overload; virtual;
    function GeProperty(const otherPropertyName: string): ICriterion; overload; virtual;
    function GtProperty(const other: IProperty): ICriterion; overload; virtual;
    function GtProperty(const otherPropertyName: string): ICriterion; overload; virtual;
    function LeProperty(const other: IProperty): ICriterion; overload; virtual;
    function LeProperty(const otherPropertyName: string): ICriterion; overload; virtual;
    function LtProperty(const other: IProperty): ICriterion; overload; virtual;
    function LtProperty(const otherPropertyName: string): ICriterion; overload; virtual;

    function GetEntityClass: TClass; virtual;
    function GetPropertyName: string; virtual;
    procedure SetEntityClass(value: TClass); virtual;
    procedure SetPropertyName(const value: string); virtual;
  public
    class function ForName(const propertyName: string): TProperty;

    property PropertyName: string read GetPropertyName write SetPropertyName;
  end;

  /// <summary>
  ///   A factory for property-specific criterion and projection instances.
  /// </summary>
  TProperty<T: class> = class(TProperty)
  public
    class function ForName(const propertyName: string): TProperty;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Criteria.Criterion.PropertyExpression,
  Spring.Persistence.Criteria.OrderBy;


{$REGION 'TProperty'}

constructor TProperty.Create;
begin
  inherited Create;
end;

function TProperty.Asc: IOrderBy;
begin
  Result := TOrderBy.Asc(fPropertyName);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.Between(const low, high: TValue): ICriterion;
begin
  Result := TRestrictions.Between(PropertyName, low, high);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.Desc: IOrderBy;
begin
  Result := TOrderBy.Desc(fPropertyName);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.Eq(const value: TValue): ICriterion;
begin
  Result := TRestrictions.Eq(fPropertyName, value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.EqProperty(const otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(PropertyName, otherPropertyName, woEqual
    , TSQLTable.CreateFromClass(fEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.EqProperty(const other: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, other.GetPropertyName, woEqual,
    TSQLTable.CreateFromClass(fEntityClass),
    TSQLTable.CreateFromClass(other.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

class function TProperty.ForName(const propertyName: string): TProperty;
begin
  Result := TProperty.Create;
  Result.fPropertyName := propertyName;
end;

function TProperty.GeProperty(const otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, otherPropertyName, woMoreOrEqual,
    TSQLTable.CreateFromClass(fEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.GeProperty(const other: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, other.GetPropertyName, woMoreOrEqual,
    TSQLTable.CreateFromClass(fEntityClass),
    TSQLTable.CreateFromClass(other.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.GEq(const value: TValue): ICriterion;
begin
  Result := TRestrictions.GEq(fPropertyName, value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.GetEntityClass: TClass;
begin
  Result := fEntityClass;
end;

function TProperty.GetPropertyName: string;
begin
  Result := fPropertyName;
end;

function TProperty.Gt(const value: TValue): ICriterion;
begin
  Result := TRestrictions.Gt(fPropertyName, value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.GtProperty(const other: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, other.GetPropertyName, woMore,
    TSQLTable.CreateFromClass(fEntityClass),
    TSQLTable.CreateFromClass(other.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.GtProperty(const otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, otherPropertyName, woMore,
    TSQLTable.CreateFromClass(fEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.&In<T>(const value: TArray<T>): ICriterion;
begin
  Result := TRestrictions.&In<T>(fPropertyName, value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.InInt(const value: TArray<Integer>): ICriterion;
begin
  Result := &In<Integer>(value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.InStr(const value: TArray<string>): ICriterion;
begin
  Result := &In<string>(value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.IsNotNull: ICriterion;
begin
  Result := TRestrictions.IsNotNull(fPropertyName);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.IsNull: ICriterion;
begin
  Result := TRestrictions.IsNull(fPropertyName);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.LeProperty(const other: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, other.GetPropertyName, woLessOrEqual,
    TSQLTable.CreateFromClass(fEntityClass),
    TSQLTable.CreateFromClass(other.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.LeProperty(const otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, otherPropertyName, woLessOrEqual,
    TSQLTable.CreateFromClass(fEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.LEq(const value: TValue): ICriterion;
begin
  Result := TRestrictions.LEq(fPropertyName, value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.Like(const value: string; matchMode: TMatchMode): ICriterion;
begin
  Result := TRestrictions.Like(fPropertyName, value, matchMode);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.Lt(const value: TValue): ICriterion;
begin
  Result := TRestrictions.Lt(fPropertyName, value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.LtProperty(const otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, otherPropertyName, woLess,
    TSQLTable.CreateFromClass(fEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.LtProperty(const other: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, other.GetPropertyName, woLess,
    TSQLTable.CreateFromClass(fEntityClass),
    TSQLTable.CreateFromClass(other.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NeProperty(const other: IProperty): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, other.GetPropertyName, woNotEqual,
    TSQLTable.CreateFromClass(fEntityClass),
    TSQLTable.CreateFromClass(other.GetEntityClass));
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NeProperty(const otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(
    PropertyName, otherPropertyName, woNotEqual,
    TSQLTable.CreateFromClass(fEntityClass), nil);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NotEq(const value: TValue): ICriterion;
begin
  Result := TRestrictions.NotEq(fPropertyName, value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NotIn<T>(const value: TArray<T>): ICriterion;
begin
  Result := TRestrictions.NotIn<T>(fPropertyName, value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NotInInt(const value: TArray<Integer>): ICriterion;
begin
  Result := NotIn<Integer>(value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NotInStr(const value: TArray<string>): ICriterion;
begin
  Result := NotIn<string>(value);
  Result.SetEntityClass(GetEntityClass);
end;

function TProperty.NotLike(const value: string; matchMode: TMatchMode): ICriterion;
begin
  Result := TRestrictions.NotLike(fPropertyName, value, matchMode);
  Result.SetEntityClass(GetEntityClass);
end;

procedure TProperty.SetEntityClass(value: TClass);
begin
  fEntityClass := value;
end;

procedure TProperty.SetPropertyName(const Value: string);
begin
  fPropertyName := Value;
end;

{$ENDREGION}


{$REGION 'TGenericProperty<T>'}

class function TProperty<T>.ForName(const propertyName: string): TProperty;
begin
  Result := TProperty.ForName(propertyName);
  Result.fEntityClass := T;
end;

{$ENDREGION}


end.
