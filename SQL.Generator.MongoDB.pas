unit SQL.Generator.MongoDB;

interface

uses
  SQL.Generator.NoSQL, SQL.Interfaces, SQL.Commands, Mapping.Attributes
  ;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <b>MongoDB</b> query generator.
  ///	</summary>
  {$ENDREGION}
  TMongoDBGenerator = class(TNoSQLGenerator)
  public
    function GetQueryLanguage(): TQueryLanguage; override;
    function GenerateUniqueId(): Variant; override;
    function GetUpdateVersionFieldQuery(AUpdateCommand: TUpdateCommand; AVersionColumn: VersionAttribute; AVersionValue, APKValue: Variant): Variant; override;
  end;

implementation

uses
  SQL.Register
  ,mongoID
  ,MongoBson
  ,Variants
  ;


{ TMongoDBGenerator }

function TMongoDBGenerator.GenerateUniqueId: Variant;
begin
  Result := mongoObjectId();
end;

function TMongoDBGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlMongoDB;
end;

function TMongoDBGenerator.GetUpdateVersionFieldQuery(
  AUpdateCommand: TUpdateCommand; AVersionColumn: VersionAttribute;
  AVersionValue, APKValue: Variant): Variant;
begin
  Result := BSON([AUpdateCommand.PrimaryKeyColumn.Name, APKValue, AVersionColumn.Name, AVersionValue]);
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TMongoDBGenerator.Create());

end.
