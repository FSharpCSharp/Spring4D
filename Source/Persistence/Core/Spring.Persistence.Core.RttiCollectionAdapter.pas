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
  /// <summary>
  ///   Represent adapter of any enumerable collection. Can be used if there is
  ///   a need to load results into different collection types.
  /// </summary>
  TRttiCollectionAdapter<T: class, constructor> = class(TInterfacedObject, ICollectionAdapter<T>)
  private
    fCollection: TValue;
    fAddMethod: TRttiMethod;
    fClearMethod: TRttiMethod;
    fCountProperty: TRttiProperty;
    fCountMethod: TRttiMethod;
    procedure GetMethodsFromRtti;

    type
      TEnumerator = class(TInterfacedObject, ICollectionEnumerator<T>)
      private
        fCollection: TValue;
        fGetEnumeratorMethod: TRttiMethod;
        fEnumerator: TValue;
        fMoveNextMethod: TRttiMethod;
        fGetCurrentMethod: TRttiMethod;
        fCurrentProperty: TRttiProperty;
        function GetCurrent: T;
        procedure GetMethodsFromRtti;
      public
        constructor Create(const collection: TValue); virtual;
        destructor Destroy; override;

        function MoveNext: Boolean;
        property Current: T read GetCurrent;
      end;
  public
    constructor Create(const collection: TValue);

    procedure Add(const entity: T);
    procedure Clear;
    function Count: Integer;
    function GetEnumerator: ICollectionEnumerator<T>;

    function IsAddSupported: Boolean;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Reflection;


{$REGION 'TCollectionAdapter<T>'}

constructor TRttiCollectionAdapter<T>.Create(const collection: TValue);
begin
  inherited Create;
  fCollection := collection;
  GetMethodsFromRtti;
end;

procedure TRttiCollectionAdapter<T>.Add(const entity: T);
begin
  fAddMethod.Invoke(fCollection, [entity]);
end;

procedure TRttiCollectionAdapter<T>.Clear;
begin
  fClearMethod.Invoke(fCollection, []);
end;

function TRttiCollectionAdapter<T>.Count: Integer;
begin
  if Assigned(fCountMethod) then
    Result := fCountMethod.Invoke(fCollection, []).AsInteger
  else if Assigned(fCountProperty) then
    Result := fCountProperty.GetValue(TRttiExplorer.GetRawPointer(fCollection)).AsInteger
  else
    raise EORMContainerDoesNotHaveCountMethod.CreateFmt('Count method not found for container type "%S"', [fCollection.ToString]);
end;

function TRttiCollectionAdapter<T>.GetEnumerator: ICollectionEnumerator<T>;
begin
  Result := TEnumerator.Create(fCollection);
end;

procedure TRttiCollectionAdapter<T>.GetMethodsFromRtti;
begin
  TRttiExplorer.TryGetMethod(fCollection.TypeInfo, 'Add', fAddMethod, 1);
  TRttiExplorer.TryGetMethod(fCollection.TypeInfo, 'Clear', fClearMethod, 0);
  TRttiExplorer.TryGetMethod(fCollection.TypeInfo, 'GetCount', fCountMethod, 0);
  fCountProperty := TRttiContext.Create.GetType(fCollection.TypeInfo).GetProperty('Count');
end;

function TRttiCollectionAdapter<T>.IsAddSupported: Boolean;
begin
  Result := Assigned(fAddMethod) and Assigned(fClearMethod);
end;

{$ENDREGION}


{$REGION 'TRttiCollectionAdapter<T>.TEnumerator'}

constructor TRttiCollectionAdapter<T>.TEnumerator.Create(const collection: TValue);
begin
  inherited Create;
  fCollection := collection;
  GetMethodsFromRtti;
end;

destructor TRttiCollectionAdapter<T>.TEnumerator.Destroy;
begin
  if fEnumerator.IsObject then
    fEnumerator.AsObject.Free;
  inherited Destroy;
end;

function TRttiCollectionAdapter<T>.TEnumerator.GetCurrent: T;
begin
  if Assigned(fCurrentProperty) then
    Result := fCurrentProperty.GetValue(TRttiExplorer.GetRawPointer(fEnumerator)).AsType<T>
  else if Assigned(fGetCurrentMethod) then
    Result := fGetCurrentMethod.Invoke(fEnumerator, []).AsType<T>;
end;

procedure TRttiCollectionAdapter<T>.TEnumerator.GetMethodsFromRtti;
begin
  if TRttiExplorer.TryGetMethod(fCollection.TypeInfo, 'GetEnumerator', fGetEnumeratorMethod, 0) then
  begin
    fEnumerator := fGetEnumeratorMethod.Invoke(fCollection, []);
    TRttiExplorer.TryGetMethod(fEnumerator.TypeInfo, 'MoveNext', fMoveNextMethod, 0);
    TRttiExplorer.TryGetMethod(fEnumerator.TypeInfo, 'GetCurrent', fGetCurrentMethod, 0);
    fCurrentProperty := TType.GetType(fEnumerator.TypeInfo).GetProperty('Current');
  end;
end;

function TRttiCollectionAdapter<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := Assigned(fMoveNextMethod)
    and fMoveNextMethod.Invoke(fEnumerator, []).AsBoolean;
end;

{$ENDREGION}


end.
