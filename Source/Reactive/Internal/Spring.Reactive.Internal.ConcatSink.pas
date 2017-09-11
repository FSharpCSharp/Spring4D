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

unit Spring.Reactive.Internal.ConcatSink;

interface

uses
  Spring.Reactive,
  Spring.Reactive.Internal.TailRecursiveSink;

type
  TConcatSink<TSource> = class(TTailRecursiveSink<TSource>)
  protected
    function Extract(const source: IObservable<TSource>): TArray<IObservable<TSource>>; override;
  public
    procedure OnCompleted; override;
  end;

implementation

uses
  SysUtils;


{$REGION 'TConcatSink<TSource>'}

function TConcatSink<TSource>.Extract(
  const source: IObservable<TSource>): TArray<IObservable<TSource>>;
var
  c: IConcatenatable<TSource>;
begin
  if Supports(source, IConcatenatable<TSource>, c) then
    Result := c.GetSources
  else
    Result := nil;
end;

procedure TConcatSink<TSource>.OnCompleted;
begin
  fRecurse;
end;

{$ENDREGION}


end.
