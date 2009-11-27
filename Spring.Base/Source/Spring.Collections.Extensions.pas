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

unit Spring.Collections.Extensions;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Generics.Defaults,
  Generics.Collections,
  Spring.System,
  Spring.Collections;

type
  /// <summary>
  /// TEnumeratorDecorator<T>
  /// </summary>
  TEnumeratorDecorator<T> = class abstract(TInterfacedObject, IEnumeratorEx<T>, IInterface)
  private
    fEnumerator: IEnumeratorEx<T>;
  protected
    function GetCurrent: T; virtual;
    property Enumerator: IEnumeratorEx<T> read fEnumerator;
  public
    constructor Create(const enumerator: IEnumeratorEx<T>);
    function MoveNext: Boolean; virtual;
    procedure Reset; virtual;
    property Current: T read GetCurrent;
  end;

  /// <summary>
  /// TEnumerableDecorator<T>
  /// </summary>
  TEnumerableDecorator<T> = class abstract(TEnumerableBase<T>)
  private
    fCollection: IEnumerable_<T>;
  protected
    property Collection: IEnumerable_<T> read fCollection;
  public
    constructor Create(const collection: IEnumerable_<T>);
    function GetEnumerator: IEnumeratorEx<T>; override;
  end;

  TEnumeratorWithPredicate<T> = class(TEnumeratorDecorator<T>)
  private
    fPredicate: TPredicate<T>;
  public
    constructor Create(const enumerator: IEnumeratorEx<T>; const predicate: TPredicate<T>);
    function MoveNext: Boolean; override;
  end;

  TEnumerableWithPredicate<T> = class(TEnumerableDecorator<T>)
  private
    fPredicate: TPredicate<T>;
  public
    constructor Create(const collection: IEnumerable_<T>; const predicate: TPredicate<T>);
    function GetEnumerator: IEnumeratorEx<T>; override;
  end;

implementation


{$REGION 'TEnumerableDecorator<T>'}

constructor TEnumerableDecorator<T>.Create(const collection: IEnumerable_<T>);
begin
  inherited Create;
  fCollection := collection;
end;

function TEnumerableDecorator<T>.GetEnumerator: IEnumeratorEx<T>;
begin
  Result := fCollection.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TEnumeratorDecorator<T>'}

constructor TEnumeratorDecorator<T>.Create(const enumerator: IEnumeratorEx<T>);
begin
  inherited Create;
  fEnumerator := enumerator;
end;

function TEnumeratorDecorator<T>.GetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TEnumeratorDecorator<T>.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

procedure TEnumeratorDecorator<T>.Reset;
begin
  fEnumerator.Reset;
end;

{$ENDREGION}


{$REGION 'TEnumeratorWithPredicate<T>'}

constructor TEnumeratorWithPredicate<T>.Create(
  const enumerator: IEnumeratorEx<T>; const predicate: TPredicate<T>);
begin
  inherited Create(enumerator);
  fPredicate := predicate;
end;

function TEnumeratorWithPredicate<T>.MoveNext: Boolean;
begin
  Result := Enumerator.MoveNext;
  while Result and not fPredicate(Enumerator.Current) do
  begin
    Result := Enumerator.MoveNext;
  end;
end;

{$ENDREGION}


{$REGION 'TEnumerableWithPredicate'}

constructor TEnumerableWithPredicate<T>.Create(
  const collection: IEnumerable_<T>; const predicate: TPredicate<T>);
begin
  inherited Create(collection);
  fPredicate := predicate;
end;

function TEnumerableWithPredicate<T>.GetEnumerator: IEnumeratorEx<T>;
begin
  Result := TEnumeratorWithPredicate<T>.Create(Collection.GetEnumerator, fPredicate);
end;

{$ENDREGION}

end.
