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

unit Spring.Persistence.Criteria.Order;

{$I Spring.inc}

interface

uses
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Implementation of IOrder interface.
  ///	</summary>
  {$ENDREGION}
  TOrder = class(TInterfacedObject, IOrder)
  private
    FOrderType: TOrderType;
    FPropertyName: string;
    FEntityClass: TClass;
  protected
    constructor Create(const APropertyName: string; AOrderType: TOrderType); virtual;

    function GetPropertyName: string; virtual;
    function GetOrderType: TOrderType; virtual;

    function GetEntityClass: TClass; virtual;
    procedure SetEntityClass(AClass: TClass); virtual;
  public
    class function Asc(const APropertyName: string): IOrder;
    class function Desc(const APropertyName: string): IOrder;
  end;

implementation

{ TOrder }

class function TOrder.Asc(const APropertyName: string): IOrder;
begin
  Result := TOrder.Create(APropertyName, otAscending);
end;

constructor TOrder.Create(const APropertyName: string; AOrderType: TOrderType);
begin
  inherited Create;
  FPropertyName := APropertyName;
  FOrderType := AOrderType;
  FEntityClass := nil;
end;

class function TOrder.Desc(const APropertyName: string): IOrder;
begin
  Result := TOrder.Create(APropertyName, otDescending);
end;

function TOrder.GetEntityClass: TClass;
begin
  Result := FEntityClass;
end;

function TOrder.GetOrderType: TOrderType;
begin
  Result := FOrderType;
end;

function TOrder.GetPropertyName: string;
begin
  Result := FPropertyName;
end;

procedure TOrder.SetEntityClass(AClass: TClass);
begin
  FEntityClass := AClass;
end;

end.


