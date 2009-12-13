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
  TEnumeratorDecorator<T> = class abstract(TEnumeratorBase<T>)
  private
    fEnumerator: IEnumerator<T>;
  protected
    function DoGetCurrent: T; override;
    property Enumerator: IEnumerator<T> read fEnumerator;
  public
    constructor Create(const enumerator: IEnumerator<T>);
    function MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  /// <summary>
  /// TEnumerableDecorator<T>
  /// </summary>
  TEnumerableDecorator<T> = class abstract(TEnumerableEx<T>)
  private
    fCollection: IEnumerable<T>;
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
    property Collection: IEnumerable<T> read fCollection;
  public
    constructor Create(const collection: IEnumerable<T>);
  end;

  TEnumeratorWithPredicate<T> = class(TEnumeratorDecorator<T>)
  private
    fPredicate: TPredicate<T>;
  public
    constructor Create(const enumerator: IEnumerator<T>; const predicate: TPredicate<T>);
    function MoveNext: Boolean; override;
  end;

  TEnumerableWithPredicate<T> = class(TEnumerableDecorator<T>)
  private
    fPredicate: TPredicate<T>;
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create(const collection: IEnumerable<T>; const predicate: TPredicate<T>);
  end;

implementation


{$REGION 'TEnumerableDecorator<T>'}

constructor TEnumerableDecorator<T>.Create(const collection: IEnumerable<T>);
begin
  inherited Create;
  fCollection := collection;
end;

function TEnumerableDecorator<T>.DoGetEnumerator: IEnumerator<T>;
begin
  Result := fCollection.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TEnumeratorDecorator<T>'}

constructor TEnumeratorDecorator<T>.Create(const enumerator: IEnumerator<T>);
begin
  inherited Create;
  fEnumerator := enumerator;
end;

function TEnumeratorDecorator<T>.DoGetCurrent: T;
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
  const enumerator: IEnumerator<T>; const predicate: TPredicate<T>);
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
  const collection: IEnumerable<T>; const predicate: TPredicate<T>);
begin
  inherited Create(collection);
  fPredicate := predicate;
end;

function TEnumerableWithPredicate<T>.DoGetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorWithPredicate<T>.Create(Collection.GetEnumerator, fPredicate);
end;

{$ENDREGION}

end.
