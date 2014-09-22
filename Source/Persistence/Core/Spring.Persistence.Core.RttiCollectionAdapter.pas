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

unit Spring.Persistence.Core.RttiCollectionAdapter;

{$I Spring.inc}

interface

uses
  Rtti,
  Spring.Persistence.Core.Interfaces;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent adapter of any enumerable collection. Can be used if there is
  ///	  a need to load results into different collection types.
  ///	</summary>
  {$ENDREGION}
  TRttiCollectionAdapter<T: class, constructor> = class(TInterfacedObject, ICollectionAdapter<T>)
  private
    FCollection: TValue;
    FAddMethod: TRttiMethod;
    FClearMethod: TRttiMethod;
    FCountProp: TRttiProperty;
    FCountMethod: TRttiMethod;
  protected
    procedure GetMethodsFromRtti; virtual;
  public
    constructor Create(const ACollection: TValue); virtual;

    procedure Add(AEntity: T);
    procedure Clear;
    function Count: Integer;
    function GetEnumerator: ICollectionEnumerator<T>;

    function IsAddSupported: Boolean;
  end;

implementation

uses
  Spring.Persistence.Core.Collections.Enumerator,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Mapping.RttiExplorer;

{ TCollectionAdapter<T> }

constructor TRttiCollectionAdapter<T>.Create(const ACollection: TValue);
begin
  inherited Create;
  FCollection := ACollection;
  GetMethodsFromRtti;
end;

procedure TRttiCollectionAdapter<T>.Add(AEntity: T);
begin
  FAddMethod.Invoke(FCollection, [AEntity]);
end;

procedure TRttiCollectionAdapter<T>.Clear;
begin
  FClearMethod.Invoke(FCollection, []);
end;

function TRttiCollectionAdapter<T>.Count: Integer;
begin
  if Assigned(FCountMethod) then
    Result := FCountMethod.Invoke(FCollection, []).AsInteger
  else if Assigned(FCountProp) then
    Result := FCountProp.GetValue(TRttiExplorer.GetRawPointer(FCollection)).AsInteger
  else
    raise EORMContainerDoesNotHaveCountMethod.CreateFmt('Count method not found for container type "%S"', [FCollection.ToString]);
end;

function TRttiCollectionAdapter<T>.GetEnumerator: ICollectionEnumerator<T>;
begin
  Result := TCollectionEnumerator<T>.Create(FCollection);
end;

procedure TRttiCollectionAdapter<T>.GetMethodsFromRtti;
begin
  TRttiExplorer.TryGetMethod(FCollection.TypeInfo, 'Add', FAddMethod, 1);
  TRttiExplorer.TryGetMethod(FCollection.TypeInfo, 'Clear', FClearMethod, 0);
  TRttiExplorer.TryGetMethod(FCollection.TypeInfo, 'GetCount', FCountMethod, 0);
  FCountProp := TRttiContext.Create.GetType(FCollection.TypeInfo).GetProperty('Count');
end;

function TRttiCollectionAdapter<T>.IsAddSupported: Boolean;
begin
  Result := Assigned(FAddMethod) and Assigned(FClearMethod);
end;

end.
