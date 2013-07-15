{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2013 Spring4D Team                           }
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

unit Spring.Container.Extensions;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Container.Core;

type
  TContainerExtension = class(TInterfacedObject, IContainerExtension)
  private
    fContext: IContainerContext;
    function GetContext: IContainerContext;
    procedure SetContext(const value: IContainerContext);
  protected
    procedure DoResolve(Sender: TObject; var instance: TValue); virtual;
  end;

implementation


{$REGION 'TContainerExtension'}

procedure TContainerExtension.DoResolve(Sender: TObject; var instance: TValue);
begin

end;

function TContainerExtension.GetContext: IContainerContext;
begin
  Result := fContext;
end;

procedure TContainerExtension.SetContext(
  const value: IContainerContext);
begin
  if Assigned(fContext) then
  begin
    fContext.DependencyResolver.OnResolve.Remove(DoResolve);
    fContext.ServiceResolver.OnResolve.Remove(DoResolve);
  end;

  fContext := value;

  if Assigned(fContext) then
  begin
    fContext.DependencyResolver.OnResolve.Add(DoResolve);
    fContext.ServiceResolver.OnResolve.Add(DoResolve);
  end;
end;

{$ENDREGION}


end.
