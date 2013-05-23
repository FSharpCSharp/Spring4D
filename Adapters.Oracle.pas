unit Adapters.Oracle;

interface

uses
  Adapters.ADO, SysUtils, Core.Interfaces, SQL.Params;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Oracle resultset.
  ///	</summary>
  {$ENDREGION}
  TOracleResultsetAdapter = class(TADOResultSetAdapter);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Oracle statement.
  ///	</summary>
  {$ENDREGION}
  TOracleStatementAdapter = class(TADOStatementAdapter)
  public
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; override;
    procedure SetParam(ADBParam: TDBParam); override;
  end;


  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Oracle connection.
  ///	</summary>
  {$ENDREGION}
  TOracleConnectionAdapter = class(TADOConnectionAdapter)
  public
    function GetDriverName: string; override;
    function CreateStatement: IDBStatement; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Oracle transaction.
  ///	</summary>
  {$ENDREGION}
  TOracleTransactionAdapter = class(TADOTransactionAdapter);

  EOracleStatementAdapterException = Exception;

implementation


uses
  Core.ConnectionFactory
  ,Core.Consts
  ,StrUtils
  ,Variants
  {$IFDEF MSWINDOWS}
  ,ADODB
  {$ENDIF}
  ;

{ TOracleConnectionAdapter }

function TOracleConnectionAdapter.CreateStatement: IDBStatement;
var
  LStatement: TADOQuery;
  LAdapter: TOracleStatementAdapter;
begin
  if Connection = nil then
    Exit(nil);

  LStatement := TADOQuery.Create(nil);
  LStatement.Connection := Connection;

  LAdapter := TOracleStatementAdapter.Create(LStatement);
  LAdapter.ExecutionListeners := ExecutionListeners;
  Result := LAdapter;
end;

function TOracleConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_ORACLE;
end;

{ TOracleStatementAdapter }

function TOracleStatementAdapter.ExecuteQuery(AServerSideCursor: Boolean): IDBResultSet;
begin
  Result := inherited ExecuteQuery(AServerSideCursor);
end;


procedure TOracleStatementAdapter.SetParam(ADBParam: TDBParam);
var
  sParamName: string;
begin
  sParamName := ADBParam.Name;
  //strip leading : in param name because ADO does not like them
  if (ADBParam.Name <> '') and (StartsStr(':', ADBParam.Name)) then
  begin
    sParamName := Copy(ADBParam.Name, 2, Length(ADBParam.Name));
  end;

  if VarIsEmpty(ADBParam.Value) or VarIsNull(ADBParam.Value) then
  begin
    //if we set param value to Null, we must provide correct field type to Oracle, otherwise it will raise an error
    Statement.Parameters.ParamByName(sParamName).Value := Null;
    ADBParam.SetParamTypeFromTypeInfo(ADBParam.TypeInfo);
    Statement.Parameters.ParamByName(sParamName).DataType := ADBParam.ParamType;
  end
  else
    Statement.Parameters.ParamValues[sParamName] := ADBParam.Value;
end;

initialization
  TConnectionFactory.RegisterConnection<TOracleConnectionAdapter>(dtOracle);

end.
