unit SQL.Commands.BulkInsert.MongoDB;

interface

uses
  SQL.Commands.Insert, Spring.Collections;

type
  TMongoDBBulkInsertExecutor = class(TInsertExecutor)
  public
    procedure BulkExecute<T: class, constructor>(AEntities: ICollection<T>);
  end;

implementation

uses
  Adapters.MongoDB
  ,bsonDoc
  ,bsonUtils
  ,Mapping.RttiExplorer
  ,Rtti
  ;


{ TMongoDBBulkInsertExecutor }

procedure TMongoDBBulkInsertExecutor.BulkExecute<T>(AEntities: ICollection<T>);
var
  LEntity: T;
  LQuery: string;
  LStatement: TMongoStatementAdapter;
  LConn: TMongoDBConnection;
  LDocs: array of IBSONDocument;
  LCollection: string;
  i: Integer;
begin
  LConn := (Connection as TMongoConnectionAdapter).Connection;
  LStatement := TMongoStatementAdapter.Create(nil);
  try
    SetLength(LDocs, AEntities.Count);
    i := 0;
    for LEntity in AEntities do
    begin
      if CanClientAutogenerateValue then
      begin
        TRttiExplorer.SetMemberValue(nil, LEntity, GetPrimaryKeyColumn(), TValue.FromVariant(Generator.GenerateUniqueId));
      end;

      GetInsertCommand.Entity := LEntity;
      Command.Entity := LEntity;
      LQuery := Generator.GenerateInsert(GetInsertCommand);
      LStatement.SetSQLCommand(LQuery);
      if (LCollection = '') then
        LCollection := LStatement.GetFullCollectionName;
      LDocs[i] := JsonToBson(LStatement.GetQueryText);
      Inc(i);
    end;

    LConn.Insert(LCollection, LDocs);
  finally
    LStatement.Free;
  end;
end;

end.
