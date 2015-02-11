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

{$I Spring.inc}

unit Spring.Persistence.Core.Session.MongoDB;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.Session;

type
  TMongoDBSession = class(TSession)
  public
    procedure BulkInsert<T: class, constructor>(ACollection: IEnumerable<T>);
  end;

implementation

uses
  Spring.Persistence.Adapters.MongoDB,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.EntityWrapper,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Commands.BulkInsert.MongoDB;


{$REGION 'TMongoDBSession'}

procedure TMongoDBSession.BulkInsert<T>(ACollection: IEnumerable<T>);
var
  inserter: TMongoDBBulkInsertExecutor;
  entity: T;
  entityWrapper: IEntityWrapper;
begin
  inserter := TMongoDBBulkInsertExecutor.Create(Connection);
  try
    inserter.EntityClass := T;
    inserter.Build(T);
    for entity in ACollection do
    begin
      entityWrapper := TEntityWrapper.Create(entity);
      SetLazyColumns(entityWrapper);
      AttachEntity(entityWrapper);
    end;
    inserter.BulkExecute<T>(ACollection);
  finally
    inserter.Free;
  end;
end;

{$ENDREGION}


end.
