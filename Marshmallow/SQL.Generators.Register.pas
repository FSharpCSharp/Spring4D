unit SQL.Generators.Register;

interface

  //register all implemented generators

uses
// TODO -oCesar : Check how to register in the package Spring.Persitence.Adapter.SQLite
//  SQL.Generator.SQLite3,
  SQL.Generator.PostgreSQL
  ,SQL.Generator.Oracle
  ,SQL.Generator.MSSQL
  ,SQL.Generator.MySQL
  ,SQL.Generator.Firebird
  ,SQL.Generator.ASA
  ;

implementation

end.
