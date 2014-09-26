// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program ORMTests;
{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}
{$I Spring.inc}
uses
  Forms,
{$IFNDEF DELPHIXE2_UP}
  RttiPatch,
{$ENDIF}
  SysUtils,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
{$IFDEF DELPHIXE5_UP}
  FireDAC.VCLUI.Wait,
{$ENDIF}
  CodeGeneratorTest in 'CodeGeneratorTest.pas',
  EntityModelDataLoaderTests in 'EntityModelDataLoaderTests.pas',
  Spring.Persistence.Adapters.ADO in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.ADO.pas',
  Spring.Persistence.Adapters.ASA in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.ASA.pas',
  Spring.Persistence.Adapters.DBX in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.DBX.pas',
  Spring.Persistence.Adapters.FieldCache in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.FieldCache.pas',
{$IFDEF DELPHIXE5_UP}
  Spring.Persistence.Adapters.FireDAC in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.FireDAC.pas',
{$ENDIF}
  Spring.Persistence.Adapters.MongoDB in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.MongoDB.pas',
  Spring.Persistence.Adapters.MSSQL in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.MSSQL.pas',
  Spring.Persistence.Adapters.Oracle in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.Oracle.pas',
  Spring.Persistence.Adapters.SQLite in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.SQLite.pas',
  Spring.Persistence.Core.AbstractManager in '..\..\Source\Persistence\Core\Spring.Persistence.Core.AbstractManager.pas',
  Spring.Persistence.Core.AbstractSession in '..\..\Source\Persistence\Core\Spring.Persistence.Core.AbstractSession.pas',
  Spring.Persistence.Core.Base in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Base.pas',
  Spring.Persistence.Core.CollectionAdapterResolver in '..\..\Source\Persistence\Core\Spring.Persistence.Core.CollectionAdapterResolver.pas',
  Spring.Persistence.Core.Collections.Enumerator in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Collections.Enumerator.pas',
  Spring.Persistence.Core.ConnectionFactory in '..\..\Source\Persistence\Core\Spring.Persistence.Core.ConnectionFactory.pas',
  Spring.Persistence.Core.Consts in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Consts.pas',
  Spring.Persistence.Core.DatabaseManager in '..\..\Source\Persistence\Core\Spring.Persistence.Core.DatabaseManager.pas',
  Spring.Persistence.Core.DetachedSession in '..\..\Source\Persistence\Core\Spring.Persistence.Core.DetachedSession.pas',
  Spring.Persistence.Core.EmbeddedEntity in '..\..\Source\Persistence\Core\Spring.Persistence.Core.EmbeddedEntity.pas',
  Spring.Persistence.Core.EntityCache in '..\..\Source\Persistence\Core\Spring.Persistence.Core.EntityCache.pas',
  Spring.Persistence.Core.EntityMap in '..\..\Source\Persistence\Core\Spring.Persistence.Core.EntityMap.pas',
  Spring.Persistence.Core.EntitySerializer in '..\..\Source\Persistence\Core\Spring.Persistence.Core.EntitySerializer.pas',
  Spring.Persistence.Core.Exceptions in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Exceptions.pas',
  Spring.Persistence.Core.Interfaces in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Interfaces.pas',
  Spring.Persistence.Core.ListSession in '..\..\Source\Persistence\Core\Spring.Persistence.Core.ListSession.pas',
  Spring.Persistence.Core.Reflection in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Reflection.pas',
  Spring.Persistence.Core.Relation.Abstract in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Relation.Abstract.pas',
  Spring.Persistence.Core.Relation.ManyToOne in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Relation.ManyToOne.pas',
  Spring.Persistence.Core.Repository.MongoDB in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Repository.MongoDB.pas',
  Spring.Persistence.Core.Repository.Proxy in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Repository.Proxy.pas',
  Spring.Persistence.Core.Repository.Simple in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Repository.Simple.pas',
  Spring.Persistence.Core.RttiCollectionAdapter in '..\..\Source\Persistence\Core\Spring.Persistence.Core.RttiCollectionAdapter.pas',
  Spring.Persistence.Core.Session in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Session.pas',
  Spring.Persistence.Core.Session.MongoDB in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Session.MongoDB.pas',
  Spring.Persistence.Core.SpringCollectionAdapter in '..\..\Source\Persistence\Core\Spring.Persistence.Core.SpringCollectionAdapter.pas',
  Spring.Persistence.Core.Types in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Types.pas',
  Spring.Persistence.Core.Utils in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Utils.pas',
  Spring.Persistence.Criteria in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.pas',
  Spring.Persistence.Criteria.Abstract in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Abstract.pas',
  Spring.Persistence.Criteria.Criterion.Abstract in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.Abstract.pas',
  Spring.Persistence.Criteria.Criterion.BetweenExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.BetweenExpression.pas',
  Spring.Persistence.Criteria.Criterion.Conjunction in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.Conjunction.pas',
  Spring.Persistence.Criteria.Criterion.Disjunction in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.Disjunction.pas',
  Spring.Persistence.Criteria.Criterion.InExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.InExpression.pas',
  Spring.Persistence.Criteria.Criterion.Junction in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.Junction.pas',
  Spring.Persistence.Criteria.Criterion.LikeExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.LikeExpression.pas',
  Spring.Persistence.Criteria.Criterion.LogicalExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.LogicalExpression.pas',
  Spring.Persistence.Criteria.Criterion.NullExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.NullExpression.pas',
  Spring.Persistence.Criteria.Criterion.PropertyExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.PropertyExpression.pas',
  Spring.Persistence.Criteria.Criterion.SimpleExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.SimpleExpression.pas',
  Spring.Persistence.Criteria.Order in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Order.pas',
  Spring.Persistence.Criteria.Properties in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Properties.pas',
  Spring.Persistence.Criteria.Restrictions in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Restrictions.pas',
  Spring.Persistence.Mapping.Attributes in '..\..\Source\Persistence\Mapping\Spring.Persistence.Mapping.Attributes.pas',
  Spring.Persistence.Mapping.CodeGenerator in '..\..\Source\Persistence\Mapping\Spring.Persistence.Mapping.CodeGenerator.pas',
  Spring.Persistence.Mapping.CodeGenerator.Abstract in '..\..\Source\Persistence\Mapping\Spring.Persistence.Mapping.CodeGenerator.Abstract.pas',
  Spring.Persistence.Mapping.CodeGenerator.DB in '..\..\Source\Persistence\Mapping\Spring.Persistence.Mapping.CodeGenerator.DB.pas',
  Spring.Persistence.Mapping.RttiExplorer in '..\..\Source\Persistence\Mapping\Spring.Persistence.Mapping.RttiExplorer.pas',
  Spring.Persistence.ObjectDataset in '..\..\Source\Persistence\ObjectDataset\Spring.Persistence.ObjectDataset.pas',
  Spring.Persistence.ObjectDataset.Abstract in '..\..\Source\Persistence\ObjectDataset\Spring.Persistence.ObjectDataset.Abstract.pas',
  Spring.Persistence.ObjectDataset.ActiveX in '..\..\Source\Persistence\ObjectDataset\Spring.Persistence.ObjectDataset.ActiveX.pas',
  Spring.Persistence.ObjectDataset.Algorithms.Sort in '..\..\Source\Persistence\ObjectDataset\Spring.Persistence.ObjectDataset.Algorithms.Sort.pas',
  Spring.Persistence.ObjectDataset.Blobs in '..\..\Source\Persistence\ObjectDataset\Spring.Persistence.ObjectDataset.Blobs.pas',
  Spring.Persistence.ObjectDataset.ExprParser in '..\..\Source\Persistence\ObjectDataset\Spring.Persistence.ObjectDataset.ExprParser.pas',
  Spring.Persistence.ObjectDataset.ExprParser.Functions in '..\..\Source\Persistence\ObjectDataset\Spring.Persistence.ObjectDataset.ExprParser.Functions.pas',
  Spring.Persistence.ObjectDataset.IndexList in '..\..\Source\Persistence\ObjectDataset\Spring.Persistence.ObjectDataset.IndexList.pas',
  Spring.Persistence.SQL.Commands in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.pas',
  Spring.Persistence.SQL.Commands.Abstract in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Abstract.pas',
  Spring.Persistence.SQL.Commands.BulkInsert.MongoDB in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.BulkInsert.MongoDB.pas',
  Spring.Persistence.SQL.Commands.Delete in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Delete.pas',
  Spring.Persistence.SQL.Commands.FKCreator in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.FKCreator.pas',
  Spring.Persistence.SQL.Commands.Insert in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Insert.pas',
  Spring.Persistence.SQL.Commands.Page in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Page.pas',
  Spring.Persistence.SQL.Commands.Select in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Select.pas',
  Spring.Persistence.SQL.Commands.SeqCreator in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.SeqCreator.pas',
  Spring.Persistence.SQL.Commands.TableCreator in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.TableCreator.pas',
  Spring.Persistence.SQL.Commands.Update in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Update.pas',
  Spring.Persistence.SQL.Generators.Abstract in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.Abstract.pas',
  Spring.Persistence.SQL.Generators.Ansi in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.Ansi.pas',
  Spring.Persistence.SQL.Generators.ASA in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.ASA.pas',
  Spring.Persistence.SQL.Generators.Firebird in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.Firebird.pas',
  Spring.Persistence.SQL.Generators.MongoDB in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.MongoDB.pas',
  Spring.Persistence.SQL.Generators.MSSQL in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.MSSQL.pas',
  Spring.Persistence.SQL.Generators.MySQL in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.MySQL.pas',
  Spring.Persistence.SQL.Generators.NoSQL in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.NoSQL.pas',
  Spring.Persistence.SQL.Generators.Oracle in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.Oracle.pas',
  Spring.Persistence.SQL.Generators.PostgreSQL in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.PostgreSQL.pas',
  Spring.Persistence.SQL.Generators.Register in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.Register.pas',
  Spring.Persistence.SQL.Generators.SQLite3 in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.SQLite3.pas',
  Spring.Persistence.SQL.Interfaces in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Interfaces.pas',
  Spring.Persistence.SQL.Params in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Params.pas',
  Spring.Persistence.SQL.Register in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Register.pas',
  Spring.Persistence.SQL.Types in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Types.pas',
  SQLite3 in '..\External\SQLite3\Source\SQLite3.pas',
  SQLiteTable3 in '..\External\SQLite3\Source\SQLiteTable3.pas',
  TestAdapterMongoDB in 'TestAdapterMongoDB.pas',
  TestAdaptersASA in 'TestAdaptersASA.pas',
  TestAdaptersOracle in 'TestAdaptersOracle.pas',
  TestADOAdapter in 'TestADOAdapter.pas',
  TestAnsiSQLGenerator in 'TestAnsiSQLGenerator.pas',
  TestCollectionsAdapterResolver in 'TestCollectionsAdapterResolver.pas',
  TestCommands in 'TestCommands.pas',
  TestConnectionFactory in 'TestConnectionFactory.pas',
  TestConsts in 'TestConsts.pas',
  TestCoreCollections in 'TestCoreCollections.pas',
  TestCoreCriteria in 'TestCoreCriteria.pas',
  TestCoreEntityMap in 'TestCoreEntityMap.pas',
  TestCoreUtils in 'TestCoreUtils.pas',
  TestDatabaseManager in 'TestDatabaseManager.pas',
{$IFDEF DELPHIXE5_UP}
  TestFireDACAdapter in 'TestFireDACAdapter.pas',
{$ENDIF}
  TestMapping.RttiExplorer in 'TestMapping.RttiExplorer.pas',
  TestObjectDataset in 'TestObjectDataset.pas',
  TestPersistence in 'TestPersistence.pas',
  TestSession in 'TestSession.pas',
  TestSimpleRepository in 'TestSimpleRepository.pas',
  TestSQLiteAdapter in 'TestSQLiteAdapter.pas',
  uModels in 'uModels.pas',
  VARTOTMASTModel in 'VARTOTMASTModel.pas',
  ViewTestObjectDataset in 'ViewTestObjectDataset.pas' {frmObjectDatasetTest};

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
    TGUITestRunner.RunRegisteredTests;
  end;
end.

