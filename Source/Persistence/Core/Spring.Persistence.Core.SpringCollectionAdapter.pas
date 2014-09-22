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
    FCollection: ICollection<T>;
  public
    constructor Create(const ACollection: TValue); virtual;

    procedure Add(AEntity: T);
    procedure Clear;
    function Count: Integer;
    function GetEnumerator: ICollectionEnumerator<T>;

    function IsAddSupported: Boolean;
  end;

  TSpringCollectionEnumerator<T: class, constructor> = class(TInterfacedObject, ICollectionEnumerator<T>)
  private
    FEnumerator: IEnumerator<T>;
  protected
    constructor Create(const ACollection: ICollection<T>); virtual;
  public
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

implementation

{ TSpringCollectionAdapter<T> }

procedure TSpringCollectionAdapter<T>.Add(AEntity: T);
begin
  FCollection.Add(AEntity);
end;

procedure TSpringCollectionAdapter<T>.Clear;
begin
  FCollection.Clear;
end;

function TSpringCollectionAdapter<T>.Count: Integer;
begin
  Result := FCollection.Count;
end;

constructor TSpringCollectionAdapter<T>.Create(const ACollection: TValue);
begin
  inherited Create;
  FCollection := ACollection.AsInterface as ICollection<T>;
end;

function TSpringCollectionAdapter<T>.GetEnumerator: ICollectionEnumerator<T>;
begin
  Result := TSpringCollectionEnumerator<T>.Create(FCollection);
end;

function TSpringCollectionAdapter<T>.IsAddSupported: Boolean;
begin
  Result := True;
end;

{ TSpringCollectionEnumerator<T> }

constructor TSpringCollectionEnumerator<T>.Create(
  const ACollection: ICollection<T>);
begin
  inherited Create;
  FEnumerator := ACollection.GetEnumerator;
end;

function TSpringCollectionEnumerator<T>.GetCurrent: T;
begin
  Result := FEnumerator.Current;
end;

function TSpringCollectionEnumerator<T>.MoveNext: Boolean;
begin
  Result := FEnumerator.MoveNext;
end;

end.
