{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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

unit Spring.Patterns.Specification;

interface

uses
  Spring;

type

  /// <summary>
  ///   Defines the core methods of a specification interface.
  /// </summary>
  /// <remarks>
  ///   This interface is designed to be binary compatible with
  ///   Spring.Predicate&lt;T&gt;.
  /// </remarks>
  ISpecification<T> = interface(IInvokable)
    ['{95E8259B-1397-4A66-9E12-A734E97C1C7C}']
    function IsSatisfiedBy(const item: T): Boolean;
    // DO NOT ADD ANY METHODS HERE!!!
  end;

  /// <summary>
  ///   Provides the easy-going specification holder with operator overloads.
  /// </summary>
  Specification<T> = record
  private
    fInstance: ISpecification<T>;
  public
    function IsSatisfiedBy(const item: T): Boolean; inline;

    class operator Implicit(const specification: ISpecification<T>): Specification<T>;
    class operator Implicit(const specification: Predicate<T>): Specification<T>;
    class operator Implicit(const specification: Specification<T>): ISpecification<T>;
    class operator Implicit(const specification: Specification<T>): Predicate<T>;
    class operator Explicit(const specification: ISpecification<T>): Specification<T>;
    class operator Explicit(const specification: Predicate<T>): Specification<T>;
    class operator Explicit(const specification: Specification<T>): ISpecification<T>;

    class operator LogicalAnd(const left, right: Specification<T>): Specification<T>;
    class operator LogicalOr(const left, right: Specification<T>): Specification<T>;
    class operator LogicalNot(const value: Specification<T>): Specification<T>;

    class operator In(const left: T; const right: Specification<T>): Boolean; inline;
  end;

  /// <summary>
  ///   Provides the abstract base class for ISpecification<T>.
  /// </summary>
  TSpecification<T> = class abstract(TInterfacedObject, ISpecification<T>, Predicate<T>)
  protected
    function Predicate<T>.Invoke = IsSatisfiedBy;
    function IsSatisfiedBy(const item: T): Boolean; virtual; abstract;
  end;

  TUnarySpecification<T> = class abstract(TSpecification<T>)
  protected
    fValue: ISpecification<T>;
  public
    constructor Create(const value: ISpecification<T>);
  end;

  TBinarySpecification<T> = class abstract(TSpecification<T>)
  protected
    fLeft: ISpecification<T>;
    fRight: ISpecification<T>;
  public
    constructor Create(const left, right: ISpecification<T>);
  end;

  TLogicalNotSpecification<T> = class sealed(TUnarySpecification<T>)
  protected
    function IsSatisfiedBy(const item: T): Boolean; override;
  end;

  TLogicalAndSpecification<T> = class sealed(TBinarySpecification<T>)
  protected
    function IsSatisfiedBy(const item: T): Boolean; override;
  end;

  TLogicalOrSpecification<T> = class sealed(TBinarySpecification<T>)
  protected
    function IsSatisfiedBy(const item: T): Boolean; override;
  end;

implementation


{$REGION 'Specification<T>'}

function Specification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := Assigned(fInstance) and fInstance.IsSatisfiedBy(item);
end;

class operator Specification<T>.Implicit(
  const specification: ISpecification<T>): Specification<T>;
begin
  Result.fInstance := specification;
end;

class operator Specification<T>.Implicit(
  const specification: Predicate<T>): Specification<T>;
begin
  Predicate<T>(Result.fInstance) := specification;
end;

class operator Specification<T>.Implicit(
  const specification: Specification<T>): ISpecification<T>;
begin
  Result := specification.fInstance;
end;

class operator Specification<T>.Implicit(
  const specification: Specification<T>): Predicate<T>;
begin
  ISpecification<T>(Result) := specification.fInstance;
end;

class operator Specification<T>.In(const left: T;
  const right: Specification<T>): Boolean;
begin
  Result := right.IsSatisfiedBy(left);
end;

class operator Specification<T>.Explicit(
  const specification: ISpecification<T>): Specification<T>;
begin
  Result.fInstance := specification;
end;

class operator Specification<T>.Explicit(
  const specification: Predicate<T>): Specification<T>;
begin
  Predicate<T>(Result.fInstance) := specification;
end;

class operator Specification<T>.Explicit(
  const specification: Specification<T>): ISpecification<T>;
begin
  Result := specification.fInstance;
end;

class operator Specification<T>.LogicalAnd(const left,
  right: Specification<T>): Specification<T>;
begin
  Result.fInstance := TLogicalAndSpecification<T>.Create(
    left.fInstance, right.fInstance)
end;

class operator Specification<T>.LogicalOr(const left,
  right: Specification<T>): Specification<T>;
begin
  Result.fInstance := TLogicalOrSpecification<T>.Create(
    left.fInstance, right.fInstance);
end;

class operator Specification<T>.LogicalNot(
  const value: Specification<T>): Specification<T>;
begin
  Result.fInstance := TLogicalNotSpecification<T>.Create(
    value.fInstance);
end;

{$ENDREGION}


{$REGION 'TUnarySpecification<T>'}

constructor TUnarySpecification<T>.Create(const value: ISpecification<T>);
begin
  inherited Create;
  fValue := value;
end;

{$ENDREGION}


{$REGION 'TBinarySpecification<T>'}

constructor TBinarySpecification<T>.Create(const left, right: ISpecification<T>);
begin
  inherited Create;
  fLeft := left;
  fRight := right;
end;

{$ENDREGION}


{$REGION 'TLogicalNotSpecification<T>'}

function TLogicalNotSpecification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := not fValue.IsSatisfiedBy(item);
end;

{$ENDREGION}


{$REGION 'TLogicalAndSpecification<T>'}

function TLogicalAndSpecification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := fLeft.IsSatisfiedBy(item) and fRight.IsSatisfiedBy(item);
end;

{$ENDREGION}


{$REGION 'TLogicalOrSpecification<T>'}

function TLogicalOrSpecification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := fLeft.IsSatisfiedBy(item) or fRight.IsSatisfiedBy(item);
end;

{$ENDREGION}


end.
