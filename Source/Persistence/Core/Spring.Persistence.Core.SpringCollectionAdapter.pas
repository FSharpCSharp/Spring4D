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

unit Spring.Persistence.Core.SpringCollectionAdapter;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces;

type
  TSpringCollectionAdapter<T: class, constructor> = class(TInterfacedObject, ICollectionAdapter<T>)
  private
    fCollection: ICollection<T>;

    type
      TEnumerator = class(TInterfacedObject, ICollectionEnumerator<T>)
      private
        fEnumerator: IEnumerator<T>;
        function GetCurrent: T;
      public
        constructor Create(const enumerator: IEnumerator<T>);
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


{$REGION 'TSpringCollectionAdapter<T>'}

constructor TSpringCollectionAdapter<T>.Create(const collection: TValue);
begin
  inherited Create;
  fCollection := collection.AsInterface as ICollection<T>;
end;

procedure TSpringCollectionAdapter<T>.Add(const entity: T);
begin
  fCollection.Add(entity);
end;

procedure TSpringCollectionAdapter<T>.Clear;
begin
  fCollection.Clear;
end;

function TSpringCollectionAdapter<T>.Count: Integer;
begin
  Result := fCollection.Count;
end;

function TSpringCollectionAdapter<T>.GetEnumerator: ICollectionEnumerator<T>;
begin
  Result := TEnumerator.Create(fCollection.GetEnumerator);
end;

function TSpringCollectionAdapter<T>.IsAddSupported: Boolean;
begin
  Result := True;
end;

{$ENDREGION}


{$REGION 'TSpringCollectionAdapter<T>.TEnumerator'}

constructor TSpringCollectionAdapter<T>.TEnumerator.Create(
  const enumerator: IEnumerator<T>);
begin
  inherited Create;
  fEnumerator := enumerator;
end;

function TSpringCollectionAdapter<T>.TEnumerator.GetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TSpringCollectionAdapter<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


end.
