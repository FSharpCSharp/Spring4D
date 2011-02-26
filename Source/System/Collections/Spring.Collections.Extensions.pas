{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
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
  Spring,
  Spring.Collections;

type
  TNullEnumerator<T> = class(TEnumeratorBase<T>)
  protected
    function GetCurrent: T; override;
  public
    function MoveNext: Boolean; override;
  end;

  TNullEnumerable<T> = class(TEnumerableBase<T>)
  public
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TEnumeratorDecorator<T> = class abstract(TEnumeratorBase<T>)
  private
    fEnumerator: IEnumerator<T>;
  protected
    function GetCurrent: T; override;
    property Enumerator: IEnumerator<T> read fEnumerator;
  public
    constructor Create(const enumerator: IEnumerator<T>);
    function MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  TEnumerableDecorator<T> = class abstract(TEnumerableBase<T>)
  private
    fCollection: IEnumerable<T>;
  protected
    property Collection: IEnumerable<T> read fCollection;
  public
    constructor Create(const collection: IEnumerable<T>);
    function GetEnumerator: IEnumerator<T>; override;
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
  public
    constructor Create(const collection: IEnumerable<T>; const predicate: TPredicate<T>);
    function GetEnumerator: IEnumerator<T>; override;
  end;

implementation

uses
  Spring.ResourceStrings;

{$REGION 'TNullEnumerator<T>'}

function TNullEnumerator<T>.GetCurrent: T;
begin
  raise EInvalidOperation.CreateRes(@SEnumEmpty);
end;

function TNullEnumerator<T>.MoveNext: Boolean;
begin
  Result := False;
end;

{$ENDREGION}


{$REGION 'TNullEnumerable<T>'}

function TNullEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TNullEnumerator<T>.Create;
end;

{$ENDREGION}


{$REGION 'TEnumerableDecorator<T>'}

constructor TEnumerableDecorator<T>.Create(const collection: IEnumerable<T>);
begin
  inherited Create;
  fCollection := collection;
end;

function TEnumerableDecorator<T>.GetEnumerator: IEnumerator<T>;
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

function TEnumerableWithPredicate<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorWithPredicate<T>.Create(Collection.GetEnumerator, fPredicate);
end;

{$ENDREGION}

end.
