unit Spring.Persistence.Core.Session.MongoDB;

interface

uses
  Spring.Persistence.Core.Session
  ,Spring.Collections
  ,Spring.Persistence.SQL.Commands.BulkInsert.MongoDB
  ;

type
  TMongoDBSession = class(TSession)
  public
    procedure BulkInsert<T: class, constructor>(ACollection: ICollection<T>);
  end;


implementation

uses
  Spring.Persistence.Adapters.MongoDB
  ,Spring.Persistence.SQL.Commands.Factory
  ,Spring.Persistence.Core.EntityCache
  ,Spring.Persistence.Mapping.RttiExplorer
  ;

{ TMongoDBSession }

procedure TMongoDBSession.BulkInsert<T>(ACollection: ICollection<T>);
var
  LInserter: TMongoDBBulkInsertExecutor;
  LEntity: T;
begin
  LInserter := TMongoDBBulkInsertExecutor.Create;
  try
    LInserter.EntityClass := T;
    LInserter.Connection := Connection;
    LInserter.Build(T);
    for LEntity in ACollection do
    begin
      SetLazyColumns(LEntity, TEntityCache.Get(T));
      AttachEntity(LEntity);
    end;
    LInserter.BulkExecute<T>(ACollection);
  finally
    LInserter.Free;
  end;
end;

end.
