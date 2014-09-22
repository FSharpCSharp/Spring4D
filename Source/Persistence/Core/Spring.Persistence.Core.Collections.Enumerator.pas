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

unit Spring.Persistence.Core.Collections.Enumerator;

{$I Spring.inc}

interface

uses
  Rtti,
  Spring.Persistence.Core.Interfaces;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent enumerator for collection adapter.
  ///	</summary>
  {$ENDREGION}
  TCollectionEnumerator<T: class, constructor> = class(TInterfacedObject, ICollectionEnumerator<T>)
  private
    FCollection: TValue;
    FGetEnumeratorMethod: TRttiMethod;
    FEnumerator: TValue;
    FMoveNextMethod: TRttiMethod;
    FGetCurrentMethod: TRttiMethod;
    FCurrentProp: TRttiProperty;
    function GetCurrent: T;
  protected
    constructor Create(const ACollection: TValue); virtual;
  public
    destructor Destroy; override;

    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

implementation

uses
  Spring.Persistence.Mapping.RttiExplorer;

{ TCollectionEnumerator<T> }

constructor TCollectionEnumerator<T>.Create(const ACollection: TValue);
begin
  inherited Create;
  FCollection := ACollection;
  FGetEnumeratorMethod := nil;
  TRttiExplorer.TryGetMethod(FCollection.TypeInfo, 'GetEnumerator', FGetEnumeratorMethod, 0);
  if Assigned(FGetEnumeratorMethod) then
  begin
    FEnumerator := FGetEnumeratorMethod.Invoke(FCollection, []);
    TRttiExplorer.TryGetMethod(FEnumerator.TypeInfo, 'MoveNext', FMoveNextMethod, 0);
    TRttiExplorer.TryGetMethod(FEnumerator.TypeInfo, 'GetCurrent', FGetCurrentMethod, 0);
    FCurrentProp := TRttiContext.Create.GetType(FEnumerator.TypeInfo).GetProperty('Current');
  end;
end;

destructor TCollectionEnumerator<T>.Destroy;
begin
  if FEnumerator.IsObject then
    FEnumerator.AsObject.Free;
  inherited Destroy;
end;

function TCollectionEnumerator<T>.GetCurrent: T;
begin
  if Assigned(FGetCurrentMethod) then
    Result := FGetCurrentMethod.Invoke(FEnumerator, []).AsType<T>
  else if Assigned(FCurrentProp) then
    Result := FCurrentProp.GetValue(TRttiExplorer.GetRawPointer(FEnumerator)).AsType<T>;
end;

function TCollectionEnumerator<T>.MoveNext: Boolean;
begin
  Result := False;
  if Assigned(FMoveNextMethod) then
    Result := FMoveNextMethod.Invoke(FEnumerator, []).AsBoolean;
end;

end.
