unit Spring.Persistence.SQL.Generators.Register;

interface

// register all implemented generators
uses
  Spring.Persistence.SQL.Generator.SQLite3,
  Spring.Persistence.SQL.Generator.PostgreSQL,
  Spring.Persistence.SQL.Generator.Oracle,
  Spring.Persistence.SQL.Generator.MSSQL,
  Spring.Persistence.SQL.Generator.MySQL,
  Spring.Persistence.SQL.Generator.Firebird,
  //Spring.Persistence.SQL.Generator.MongoDB,
  Spring.Persistence.SQL.Generator.ASA;

implementation

end.
