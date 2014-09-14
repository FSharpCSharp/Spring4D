// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program ORMTests;
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
  FireDAC.VCLUI.Wait,
  TestSession in 'TestSession.pas',
  Spring.Persistence.Core.AbstractManager in '..\Spring.Persistence.Core.AbstractManager.pas',
  Spring.Persistence.Core.Interfaces in '..\Spring.Persistence.Core.Interfaces.pas',
  Spring.Persistence.SQL.Params in '..\Spring.Persistence.SQL.Params.pas',
  Spring.Persistence.Core.Session in '..\Spring.Persistence.Core.Session.pas',
  Spring.Persistence.Core.EntityMap in '..\Spring.Persistence.Core.EntityMap.pas',
  Spring.Persistence.Core.DatabaseManager in '..\Spring.Persistence.Core.DatabaseManager.pas',
  Spring.Persistence.SQL.AbstractCommandExecutor in '..\Spring.Persistence.SQL.AbstractCommandExecutor.pas',
  Spring.Persistence.SQL.Interfaces in '..\Spring.Persistence.SQL.Interfaces.pas',
  Spring.Persistence.SQL.Commands.Select in '..\Spring.Persistence.SQL.Commands.Select.pas',
  Spring.Persistence.SQL.Commands.Delete in '..\Spring.Persistence.SQL.Commands.Delete.pas',
  Spring.Persistence.SQL.Commands.Update in '..\Spring.Persistence.SQL.Commands.Update.pas',
  Spring.Persistence.SQL.Commands.Insert in '..\Spring.Persistence.SQL.Commands.Insert.pas',
  Spring.Persistence.SQL.Commands.FKCreator in '..\Spring.Persistence.SQL.Commands.FKCreator.pas',
  Spring.Persistence.SQL.Commands.TableCreator in '..\Spring.Persistence.SQL.Commands.TableCreator.pas',
  Spring.Persistence.SQL.Commands.SeqCreator in '..\Spring.Persistence.SQL.Commands.SeqCreator.pas',
  Spring.Persistence.SQL.AbstractSQLGenerator in '..\Spring.Persistence.SQL.AbstractSQLGenerator.pas',
  Spring.Persistence.SQL.Generator.Ansi in '..\Spring.Persistence.SQL.Generator.Ansi.pas',
  Spring.Persistence.SQL.Register in '..\Spring.Persistence.SQL.Register.pas',
  Spring.Persistence.Adapters.SQLite in '..\Spring.Persistence.Adapters.SQLite.pas',
  Spring.Persistence.Core.Base in '..\Spring.Persistence.Core.Base.pas',
  Spring.Persistence.Mapping.Attributes in '..\Spring.Persistence.Mapping.Attributes.pas',
  Spring.Persistence.Core.Exceptions in '..\Spring.Persistence.Core.Exceptions.pas',
  Spring.Persistence.SQL.Commands.Factory in '..\Spring.Persistence.SQL.Commands.Factory.pas',
  Spring.Persistence.Mapping.RttiExplorer in '..\Spring.Persistence.Mapping.RttiExplorer.pas',
  TestMapping.RttiExplorer in 'TestMapping.RttiExplorer.pas',
  uModels in 'uModels.pas',
  Spring.Persistence.Core.Types in '..\Spring.Persistence.Core.Types.pas',
  TestSQLiteAdapter in 'TestSQLiteAdapter.pas',
  Spring.Persistence.Core.ConnectionFactory in '..\Spring.Persistence.Core.ConnectionFactory.pas',
  TestAnsiSQLGenerator in 'TestAnsiSQLGenerator.pas',
  Spring.Persistence.SQL.Commands in '..\Spring.Persistence.SQL.Commands.pas',
  Spring.Persistence.SQL.Types in '..\Spring.Persistence.SQL.Types.pas',
  Spring.Persistence.Core.EntitySerializer in '..\Spring.Persistence.Core.EntitySerializer.pas',
  Spring.Persistence.Adapters.ADO in '..\Spring.Persistence.Adapters.ADO.pas',
  TestADOAdapter in 'TestADOAdapter.pas',
  Spring.Persistence.Adapters.MSSQL in '..\Spring.Persistence.Adapters.MSSQL.pas',
  Spring.Persistence.Core.Utils in '..\Spring.Persistence.Core.Utils.pas',
  Spring.Persistence.Core.Reflection in '..\Spring.Persistence.Core.Reflection.pas',
  Spring.Persistence.Core.EntityCache in '..\Spring.Persistence.Core.EntityCache.pas',
  TestCoreEntityMap in 'TestCoreEntityMap.pas',
  Spring.Persistence.SQL.Commands.Page in '..\Spring.Persistence.SQL.Commands.Page.pas',
  VARTOTMASTModel in 'VARTOTMASTModel.pas',
  Spring.Persistence.RttiPatch in '..\Spring.Persistence.RttiPatch.pas',
  Spring.Persistence.PatchUtils in '..\Spring.Persistence.PatchUtils.pas',
  Spring.Persistence.Adapters.ASA in '..\Spring.Persistence.Adapters.ASA.pas',
  TestAdaptersASA in 'TestAdaptersASA.pas',
  Spring.Persistence.SQL.Generator.Oracle in '..\Spring.Persistence.SQL.Generator.Oracle.pas',
  Spring.Persistence.Adapters.DBX in '..\Spring.Persistence.Adapters.DBX.pas',
  Spring.Persistence.SQL.Generator.Firebird in '..\Spring.Persistence.SQL.Generator.Firebird.pas',
  TestConnectionFactory in 'TestConnectionFactory.pas',
  Spring.Persistence.SQL.Generator.MSSQL in '..\Spring.Persistence.SQL.Generator.MSSQL.pas',
  Spring.Persistence.SQL.Generator.ASA in '..\Spring.Persistence.SQL.Generator.ASA.pas',
  Spring.Persistence.SQL.Generator.SQLite3 in '..\Spring.Persistence.SQL.Generator.SQLite3.pas',
  Spring.Persistence.SQL.Generator.PostgreSQL in '..\Spring.Persistence.SQL.Generator.PostgreSQL.pas',
  Spring.Persistence.SQL.Generator.MySQL in '..\Spring.Persistence.SQL.Generator.MySQL.pas',
  TestDatabaseManager in 'TestDatabaseManager.pas',
  SQLite3 in '..\External\SQLite3\Source\SQLite3.pas',
  SQLiteTable3 in '..\External\SQLite3\Source\SQLiteTable3.pas',
  Spring.Persistence.Core.Relation.Abstract in '..\Spring.Persistence.Core.Relation.Abstract.pas',
  Spring.Persistence.Core.Relation.ManyToOne in '..\Spring.Persistence.Core.Relation.ManyToOne.pas',
  Spring.Persistence.Core.Consts in '..\Spring.Persistence.Core.Consts.pas',
  Spring.Persistence.Core.Criteria.Abstract in '..\Spring.Persistence.Core.Criteria.Abstract.pas',
  Spring.Persistence.Core.Criteria in '..\Spring.Persistence.Core.Criteria.pas',
  Spring.Persistence.Core.Criteria.AbstractCriterion in '..\Spring.Persistence.Core.Criteria.AbstractCriterion.pas',
  Spring.Persistence.Core.Criteria.Criterion in '..\Spring.Persistence.Core.Criteria.Criterion.pas',
  Spring.Persistence.Core.Criteria.Restrictions in '..\Spring.Persistence.Core.Criteria.Restrictions.pas',
  TestCoreCriteria in 'TestCoreCriteria.pas',
  Spring.Persistence.Core.Criteria.Criterion.SimpleExpression in '..\Spring.Persistence.Core.Criteria.Criterion.SimpleExpression.pas',
  Spring.Persistence.Core.Criteria.Criterion.NullExpression in '..\Spring.Persistence.Core.Criteria.Criterion.NullExpression.pas',
  Spring.Persistence.Core.Criteria.Order in '..\Spring.Persistence.Core.Criteria.Order.pas',
  Spring.Persistence.Core.Criteria.Criterion.LikeExpression in '..\Spring.Persistence.Core.Criteria.Criterion.LikeExpression.pas',
  Spring.Persistence.Core.Criteria.Criterion.InExpression in '..\Spring.Persistence.Core.Criteria.Criterion.InExpression.pas',
  Spring.Persistence.Core.Criteria.Properties in '..\Spring.Persistence.Core.Criteria.Properties.pas',
  TestPersistence in 'TestPersistence.pas',
  TestConsts in 'TestConsts.pas',
  Spring.Persistence.SQL.Generator.NoSQL in '..\Spring.Persistence.SQL.Generator.NoSQL.pas',
  Spring.Persistence.SQL.Generator.MongoDB in '..\Spring.Persistence.SQL.Generator.MongoDB.pas',
  Spring.Persistence.Core.Criteria.Criterion.LogicalExpression in '..\Spring.Persistence.Core.Criteria.Criterion.LogicalExpression.pas',
  SvTesting.DUnit in '..\External\delphi-oop\Core\SvTesting.DUnit.pas',
  Spring.Persistence.Core.Criteria.Criterion.PropertyExpression in '..\Spring.Persistence.Core.Criteria.Criterion.PropertyExpression.pas',
  Spring.Persistence.Core.Criteria.Criterion.BetweenExpression in '..\Spring.Persistence.Core.Criteria.Criterion.BetweenExpression.pas',
  Spring.Persistence.Core.RttiCollectionAdapter in '..\Spring.Persistence.Core.RttiCollectionAdapter.pas',
  TestCoreCollections in 'TestCoreCollections.pas',
  Spring.Persistence.Core.Collections.Enumerator in '..\Spring.Persistence.Core.Collections.Enumerator.pas',
  TestObjectDataset in 'TestObjectDataset.pas',
  Spring.Persistence.Adapters.ObjectDataset.Abstract in '..\Spring.Persistence.Adapters.ObjectDataset.Abstract.pas',
  Spring.Persistence.Adapters.ObjectDataset in '..\Spring.Persistence.Adapters.ObjectDataset.pas',
  ViewTestObjectDataset in 'ViewTestObjectDataset.pas' {frmObjectDatasetTest},
  Spring.Persistence.Adapters.ObjectDataset.Blobs in '..\Spring.Persistence.Adapters.ObjectDataset.Blobs.pas',
  Spring.Persistence.Adapters.ObjectDataset.ExprParser in '..\Spring.Persistence.Adapters.ObjectDataset.ExprParser.pas',
  Spring.Persistence.Core.Algorythms.Sort in '..\Spring.Persistence.Core.Algorythms.Sort.pas',
  Spring.Persistence.Adapters.ObjectDataset.ActiveX in '..\Spring.Persistence.Adapters.ObjectDataset.ActiveX.pas',
  Spring.Persistence.Adapters.ObjectDataset.IndexList in '..\Spring.Persistence.Adapters.ObjectDataset.IndexList.pas',
  Spring.Persistence.Adapters.ObjectDataset.ExprParser.Functions in '..\Spring.Persistence.Adapters.ObjectDataset.ExprParser.Functions.pas',
  Spring.Persistence.Core.ListSession in '..\Spring.Persistence.Core.ListSession.pas',
  Spring.Persistence.SQL.Generators.Register in '..\Spring.Persistence.SQL.Generators.Register.pas',
  TestAdaptersOracle in 'TestAdaptersOracle.pas',
  Spring.Persistence.Adapters.Oracle in '..\Spring.Persistence.Adapters.Oracle.pas',
  TestCommands in 'TestCommands.pas',
  TestCoreUtils in 'TestCoreUtils.pas',
  CodeGeneratorTest in 'CodeGeneratorTest.pas',
  Spring.Persistence.Mapping.CodeGenerator in '..\Spring.Persistence.Mapping.CodeGenerator.pas',
  Spring.Persistence.Mapping.CodeGenerator.Abstract in '..\Spring.Persistence.Mapping.CodeGenerator.Abstract.pas',
  Spring.Persistence.Mapping.CodeGenerator.DB in '..\Spring.Persistence.Mapping.CodeGenerator.DB.pas',
  EntityModelDataLoaderTests in 'EntityModelDataLoaderTests.pas',
  Spring.Persistence.Core.Comparers in '..\Spring.Persistence.Core.Comparers.pas',
  Spring.Persistence.Core.Criteria.Criterion.Junction in '..\Spring.Persistence.Core.Criteria.Criterion.Junction.pas',
  Spring.Persistence.Core.Criteria.Criterion.Disjunction in '..\Spring.Persistence.Core.Criteria.Criterion.Disjunction.pas',
  Spring.Persistence.Core.Criteria.Criterion.Conjunction in '..\Spring.Persistence.Core.Criteria.Criterion.Conjunction.pas',
  Spring.Persistence.Core.Repository.Simple in '..\Spring.Persistence.Core.Repository.Simple.pas',
  TestSimpleRepository in 'TestSimpleRepository.pas',
  Spring.Persistence.Adapters.MongoDB in '..\Spring.Persistence.Adapters.MongoDB.pas',
  Spring.Persistence.Core.Session.MongoDB in '..\Spring.Persistence.Core.Session.MongoDB.pas',
  Spring.Persistence.SQL.Commands.BulkInsert.MongoDB in '..\Spring.Persistence.SQL.Commands.BulkInsert.MongoDB.pas',
  TestAdapterMongoDB in 'TestAdapterMongoDB.pas',
  Spring.Persistence.Core.Repository.MongoDB in '..\Spring.Persistence.Core.Repository.MongoDB.pas',
  Spring.Persistence.Core.EmbeddedEntity in '..\Spring.Persistence.Core.EmbeddedEntity.pas',
  Spring.Persistence.Core.Repository.Proxy in '..\Spring.Persistence.Core.Repository.Proxy.pas',
  Spring.Persistence.Adapters.FieldCache in '..\Spring.Persistence.Adapters.FieldCache.pas',
  Spring.Persistence.Adapters.FireDAC in '..\Spring.Persistence.Adapters.FireDAC.pas',
  TestFireDACAdapter in 'TestFireDACAdapter.pas',
  TestCollectionsAdapterResolver in 'TestCollectionsAdapterResolver.pas',
  Spring.Persistence.Core.CollectionAdapterResolver in '..\Spring.Persistence.Core.CollectionAdapterResolver.pas',
  Spring.Persistence.Core.SpringCollectionAdapter in '..\Spring.Persistence.Core.SpringCollectionAdapter.pas',
  Spring.Persistence.Core.AbstractSession in '..\Spring.Persistence.Core.AbstractSession.pas',
  Spring.Persistence.Core.DetachedSession in '..\Spring.Persistence.Core.DetachedSession.pas';

{$R *.RES}



begin
  Application.Initialize;
  OutputDir := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  PictureFilename := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir + '..\..')) + 'DelphiOOP.png';

  if IsConsole then
  begin
    with TextTestRunner.RunRegisteredTests(rxbHaltOnFailures) do
    begin
      //run automatically for CI
      ReportMemoryLeaksOnShutdown := False;
      ExitCode := FailureCount + ErrorCount;
      Free;
    end;
  end
  else
  begin
    ReportMemoryLeaksOnShutdown := True;
    TSvGUITestRunner.RunRegisteredTests;
  end;
end.

