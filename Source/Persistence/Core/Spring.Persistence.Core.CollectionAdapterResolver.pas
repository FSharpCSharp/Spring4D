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

unit Spring.Persistence.Core.CollectionAdapterResolver;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces;

type
  TCollectionAdapterResolver = class
  public
    class function Resolve<T: class, constructor>(
      const collection: TValue): ICollectionAdapter<T>;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.RttiCollectionAdapter,
  Spring.Persistence.Core.SpringCollectionAdapter;


{$REGION 'TCollectionAdapterResolver'}

class function TCollectionAdapterResolver.Resolve<T>(
  const collection: TValue): ICollectionAdapter<T>;
begin
  if collection.IsEmpty then
    raise EORMContainerItemTypeNotSupported.Create('Collection type is empty');

  if (collection.Kind = tkInterface) and Supports(collection.AsInterface, ICollection<TObject>) then
    Result := TSpringCollectionAdapter<T>.Create(collection)
  else
    Result := TRttiCollectionAdapter<T>.Create(collection);
end;

{$ENDREGION}


end.
