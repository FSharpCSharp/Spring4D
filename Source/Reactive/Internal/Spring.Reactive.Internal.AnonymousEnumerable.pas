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

unit Spring.Reactive.Internal.AnonymousEnumerable;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type
  TAnonymousEnumerable<T> = class(TEnumerableBase<T>)
  private
    fGetEnumerator: Func<IEnumerator<T>>;
  public
    constructor Create(const getEnumerator: Func<IEnumerator<T>>);
    function GetEnumerator: IEnumerator<T>; override;
  end;

implementation


{$REGION 'TAnonymousEnumerable<T>'}

constructor TAnonymousEnumerable<T>.Create(
  const getEnumerator: Func<IEnumerator<T>>);
begin
  inherited Create;
  fGetEnumerator := getEnumerator;
end;

function TAnonymousEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := fGetEnumerator;
end;

{$ENDREGION}


end.
