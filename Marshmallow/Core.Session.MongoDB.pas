unit Core.Session.MongoDB;

interface

uses
  Core.Session
  ,Spring.Collections
  ,SQL.Commands.BulkInsert.MongoDB
  ;

type
  TMongoDBSession = class(TSession)
  public
    procedure BulkInsert<T: class, constructor>(ACollection: ICollection<T>);
  end;


implementation

uses
  Adapters.MongoDB
  ,SQL.Commands.Factory
  ,Core.EntityCache
  ,Mapping.RttiExplorer
  ;

{ TMongoDBSession }

procedure TMongoDBSession.BulkInsert<T>(ACollection: ICollection<T>);
var
  LInserter: TMongoDBBulkInsertExecutor;
  LEntity: T;
begin
  LInserter := TMongoDBBulkInsertExecutor.Create();
  try
    LInserter.EntityClass := T;
    LInserter.Connection := Connection;
    LInserter.Build(T);
    for LEntity in ACollection do
    begin
      SetLazyColumns(LEntity, TEntityCache.Get(T));
      OldStateEntities.AddOrReplace(TRttiExplorer.Clone(LEntity));
    end;
    LInserter.BulkExecute<T>(ACollection);
  finally
    LInserter.Free;
  end;
end;

end.
