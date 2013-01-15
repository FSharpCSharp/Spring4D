// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program ORMTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}
{$I sv.inc}
uses
  Forms,
  SysUtils,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestSession in 'TestSession.pas',
  Core.AbstractManager in '..\Core.AbstractManager.pas',
  Core.Interfaces in '..\Core.Interfaces.pas',
  SQL.Params in '..\SQL.Params.pas',
  Core.Session in '..\Core.Session.pas',
  Core.EntityMap in '..\Core.EntityMap.pas',
  Core.DatabaseManager in '..\Core.DatabaseManager.pas',
  SQL.AbstractCommandExecutor in '..\SQL.AbstractCommandExecutor.pas',
  SQL.Interfaces in '..\SQL.Interfaces.pas',
  SQL.Commands.Select in '..\SQL.Commands.Select.pas',
  SQL.Commands.Delete in '..\SQL.Commands.Delete.pas',
  SQL.Commands.Update in '..\SQL.Commands.Update.pas',
  SQL.Commands.Insert in '..\SQL.Commands.Insert.pas',
  SQL.Commands.FKCreator in '..\SQL.Commands.FKCreator.pas',
  SQL.Commands.TableCreator in '..\SQL.Commands.TableCreator.pas',
  SQL.Commands.SeqCreator in '..\SQL.Commands.SeqCreator.pas',
  SQL.AbstractSQLGenerator in '..\SQL.AbstractSQLGenerator.pas',
  SQL.Generator.Ansi in '..\SQL.Generator.Ansi.pas',
  SQL.Register in '..\SQL.Register.pas',
  Adapters.SQLite in '..\Adapters.SQLite.pas',
  Core.Base in '..\Core.Base.pas',
  Mapping.Attributes in '..\Mapping.Attributes.pas',
  Core.Exceptions in '..\Core.Exceptions.pas',
  SQL.Commands.Factory in '..\SQL.Commands.Factory.pas',
  Mapping.RttiExplorer in '..\Mapping.RttiExplorer.pas',
  TestMapping.RttiExplorer in 'TestMapping.RttiExplorer.pas',
  uModels in 'uModels.pas',
  Core.Types in '..\Core.Types.pas',
  TestSQLiteAdapter in 'TestSQLiteAdapter.pas',
  Core.ConnectionFactory in '..\Core.ConnectionFactory.pas',
  TestAnsiSQLGenerator in 'TestAnsiSQLGenerator.pas',
  SQL.Commands in '..\SQL.Commands.pas',
  SQL.Types in '..\SQL.Types.pas',
  Core.EntitySerializer in '..\Core.EntitySerializer.pas',
  Adapters.ADO in '..\Adapters.ADO.pas',
  TestADOAdapter in 'TestADOAdapter.pas',
  Adapters.MSSQL in '..\Adapters.MSSQL.pas',
  Core.Utils in '..\Core.Utils.pas',
  Core.Reflection in '..\Core.Reflection.pas',
  Core.EntityCache in '..\Core.EntityCache.pas',
  TestCoreEntityMap in 'TestCoreEntityMap.pas',
  SQL.Commands.Page in '..\SQL.Commands.Page.pas',
  VARTOTMASTModel in 'VARTOTMASTModel.pas',
  RttiPatch in '..\RttiPatch.pas',
  PatchUtils in '..\PatchUtils.pas',
  Adapters.ASA in '..\Adapters.ASA.pas',
  TestAdaptersASA in 'TestAdaptersASA.pas',
  SQL.Generator.Oracle in '..\SQL.Generator.Oracle.pas',
  Adapters.DBX in '..\Adapters.DBX.pas',
  Adapters.UIB in '..\Adapters.UIB.pas',
  TestAdapterUIB in 'TestAdapterUIB.pas',
  SQL.Generator.Firebird in '..\SQL.Generator.Firebird.pas',
  TestConnectionFactory in 'TestConnectionFactory.pas',
  SQL.Generator.MSSQL in '..\SQL.Generator.MSSQL.pas',
  SQL.Generator.ASA in '..\SQL.Generator.ASA.pas',
  SQL.Generator.SQLite3 in '..\SQL.Generator.SQLite3.pas',
  SQL.Generator.PostgreSQL in '..\SQL.Generator.PostgreSQL.pas',
  SQL.Generator.MySQL in '..\SQL.Generator.MySQL.pas',
  TestDatabaseManager in 'TestDatabaseManager.pas',
  SQLite3 in '..\External\SQLite3\Source\SQLite3.pas',
  SQLiteTable3 in '..\External\SQLite3\Source\SQLiteTable3.pas',
  Core.Relation.Abstract in '..\Core.Relation.Abstract.pas',
  Core.Relation.ManyToOne in '..\Core.Relation.ManyToOne.pas',
  Core.Consts in '..\Core.Consts.pas',
  Core.Criteria.Abstract in '..\Core.Criteria.Abstract.pas',
  Core.Criteria in '..\Core.Criteria.pas',
  Core.Criteria.AbstractCriterion in '..\Core.Criteria.AbstractCriterion.pas',
  Core.Criteria.Criterion in '..\Core.Criteria.Criterion.pas',
  Core.Criteria.Restrictions in '..\Core.Criteria.Restrictions.pas',
  TestCoreCriteria in 'TestCoreCriteria.pas',
  Core.Criteria.Criterion.SimpleExpression in '..\Core.Criteria.Criterion.SimpleExpression.pas',
  Core.Criteria.Criterion.NullExpression in '..\Core.Criteria.Criterion.NullExpression.pas',
  Core.Criteria.Order in '..\Core.Criteria.Order.pas',
  Core.Criteria.Criterion.LikeExpression in '..\Core.Criteria.Criterion.LikeExpression.pas';

{$R *.RES}



begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  OutputDir := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  PictureFilename := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir + '..\..')) + 'DelphiOOP.png';

  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

