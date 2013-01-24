unit SQL.Generator.MongoDB;

interface

uses
  SQL.Generator.NoSQL, SQL.Interfaces
  ;

type
  TMongoDBGenerator = class(TNoSQLGenerator)
  public
    function GetQueryLanguage(): TQueryLanguage; override;
  end;

implementation

uses
  SQL.Register
  ;


{ TMongoDBGenerator }

function TMongoDBGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlMongoDB;
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TMongoDBGenerator.Create());

end.
