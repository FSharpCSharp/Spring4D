(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit SQL.AbstractCommandExecutor;

interface

uses
  Core.Interfaces, SQL.Interfaces, Generics.Collections, SQL.Params, Mapping.Attributes, SQL.Types
  ,SQL.Commands
  ;

type
  TAbstractCommandExecutor = class
  private
    FConnection: IDBConnection;
    FGenerator: ISQLGenerator;
    FClass: TClass;
    FSQL: string;
    FParams: TObjectList<TDBParam>;
    procedure SetConnection(const Value: IDBConnection);
  protected
    function DoCreateParam(AColumn: ColumnAttribute; AValue: Variant): TDBParam; virtual;
    function CreateParam(AEntity: TObject; AColumn: ColumnAttribute): TDBParam; overload; virtual;
    function CreateParam(AEntity: TObject; AForeignColumn: ForeignJoinColumnAttribute): TDBParam; overload; virtual;
    function GetCommand: TDMLCommand; virtual; abstract;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function TableExists(const ATablename: string): Boolean; virtual;
    procedure FillDbTableColumns(const ATablename: string; AColumns: TList<string>); virtual;

    procedure Execute(AEntity: TObject); virtual;
    procedure Build(AClass: TClass); virtual; abstract;
    procedure BuildParams(AEntity: TObject); virtual;

    property Command: TDMLCommand read GetCommand;
    property Connection: IDBConnection read FConnection write SetConnection;
    property Generator: ISQLGenerator read FGenerator;
    property EntityClass: TClass read FClass write FClass;
    property SQLParameters: TObjectList<TDBParam> read FParams;
    property SQL: string read FSQL write FSQL;
  end;

implementation

uses
  SQL.Register
  ,Rtti
  ,Mapping.RttiExplorer
  ,Core.Reflection
  ,Classes
  ,Core.Utils
  ;

{ TAbstractCommandExecutor }

procedure TAbstractCommandExecutor.BuildParams(AEntity: TObject);
begin
  FParams.Clear;
  if Assigned(Command) then
    Command.Entity := AEntity;
end;

constructor TAbstractCommandExecutor.Create();
begin
  inherited Create;
  FGenerator := nil; //TSQLGeneratorRegister.GetCurrentGenerator();
  FParams := TObjectList<TDBParam>.Create();
end;

function TAbstractCommandExecutor.CreateParam(AEntity: TObject;
  AForeignColumn: ForeignJoinColumnAttribute): TDBParam;
var
  LVal, LRes: TValue;
  bFree: Boolean;
begin
  bFree := False;
  Result := TDBParam.Create;
  Result.Name := Command.GetExistingParameterName(AForeignColumn.Name);// ':' + AForeignColumn.Name;
  LVal := TRttiExplorer.GetMemberValue(AEntity, AForeignColumn.ReferencedColumnName);
  //convert/serialize objects to stream
  if LVal.IsObject then
  begin
    if LVal.TryConvert(TypeInfo(TStream), LRes, bFree) then
    begin
      LVal := LRes.AsObject;
    end;
  end;
  Result.Value := TUtils.AsVariant(LVal);
  if bFree then
  begin
    FreeValueObject(LVal);
  end;
end;

function TAbstractCommandExecutor.CreateParam(AEntity: TObject; AColumn: ColumnAttribute): TDBParam;
var
  LVal, LRes: TValue;
  bFree: Boolean;
begin
  Result := TDBParam.Create;
  Result.Name := Command.GetExistingParameterName(AColumn.Name); //':' + AColumn.Name;
  LVal := TRttiExplorer.GetMemberValueDeep(AEntity, AColumn.ClassMemberName);
  //convert/serialize objects to stream. If value is nullable or lazy get it's real value
  if LVal.IsObject then
  begin
    if LVal.TryConvert(TypeInfo(TStream), LRes, bFree) then
    begin
      LVal := LRes.AsObject;
    end;
  end;

  Result.Value := TUtils.AsVariant(LVal);

  if bFree then
  begin
    FreeValueObject(LVal);
  end;
end;

destructor TAbstractCommandExecutor.Destroy;
begin
 // FExecutionListeners.Free;
  FParams.Free;
  FConnection := nil;
  FGenerator := nil;
  inherited Destroy;
end;

function TAbstractCommandExecutor.DoCreateParam(AColumn: ColumnAttribute; AValue: Variant): TDBParam;
begin
  Result := TDBParam.Create;
  Result.Name := Command.GetExistingParameterName(AColumn.Name);// ':' + AColumn.Name;
  Result.Value := AValue;
end;

procedure TAbstractCommandExecutor.Execute(AEntity: TObject);
begin
  if (SQL = '') then
    Exit;
  Connection.NotifyExecutionListeners(SQL, SQLParameters);
end;


procedure TAbstractCommandExecutor.FillDbTableColumns(const ATablename: string; AColumns: TList<string>);
var
  LSqlTableCount: string;
  LStmt: IDBStatement;
  LResults: IDBResultset;
  i: Integer;
begin
  LSqlTableCount := Generator.GetTableColumns(ATablename);
  if (LSqlTableCount <> '') then
  begin
    LStmt := Connection.CreateStatement;
    LStmt.SetSQLCommand(LSqlTableCount);
    LResults := LStmt.ExecuteQuery;
    AColumns.Clear;
    for i := 0 to LResults.GetFieldCount - 1 do
    begin
      AColumns.Add(LResults.GetFieldName(i));
    end;
  end;
end;

procedure TAbstractCommandExecutor.SetConnection(const Value: IDBConnection);
begin
  FConnection := Value;
  if Assigned(FConnection) then
    FGenerator := TSQLGeneratorRegister.GetGenerator(FConnection.GetQueryLanguage);
end;

function TAbstractCommandExecutor.TableExists(const ATablename: string): Boolean;
var
  LSqlTableCount: string;
  LStmt: IDBStatement;
  LResults: IDBResultset;
begin
  Result := False;
  LSqlTableCount := Generator.GetSQLTableCount(ATablename);
  if (LSqlTableCount <> '') then
  begin
    LStmt := Connection.CreateStatement;
    try
      try
        LStmt.SetSQLCommand(LSqlTableCount);

        LResults := LStmt.ExecuteQuery;
        Result := not LResults.IsEmpty;
      except
        Result := False;
      end;
    finally
      LResults := nil;
      LStmt := nil;
    end;
  end;
end;

end.
