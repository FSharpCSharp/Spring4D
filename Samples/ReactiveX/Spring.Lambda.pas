{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

unit Spring.Lambda;

interface

uses
  Spring,
  Spring.Collections;

type
  Lambda<T> = record
  strict private
    function GetCount: Func<IList<T>,Integer>;
  public
    class operator GreaterThanOrEqual(const left: Lambda<T>; const right: T): Predicate<T>;
    class operator Equal(const left: Lambda<T>; const right: T): Predicate<T>;

    property Count: Func<IList<T>,Integer> read GetCount;
  end;

implementation


{$REGION 'Lambda<T>'}

class operator Lambda<T>.Equal(const left: Lambda<T>;
  const right: T): Predicate<T>;
begin
  case TType.Kind<T> of
    tkInteger:
      Predicate<Integer>(Result) :=
        function(const x: Integer): Boolean
        begin
          Result := x = PInteger(@right)^;
        end;
  end;
end;

function Lambda<T>.GetCount: Func<IList<T>,Integer>;
begin
  Result :=
    function(const x: IList<T>): Integer
    begin
      Result := x.Count;
    end;
end;

class operator Lambda<T>.GreaterThanOrEqual(const left: Lambda<T>;
  const right: T): Predicate<T>;
begin
  case TType.Kind<T> of
    tkInteger:
      Predicate<Integer>(Result) :=
        function(const x: Integer): Boolean
        begin
          Result := x >= PInteger(@right)^;
        end;
  end;
end;

{$ENDREGION}


end.
