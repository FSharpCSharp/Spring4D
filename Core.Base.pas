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
unit Core.Base;

interface

uses
  Core.Interfaces, SQL.Params, Generics.Collections;

type
  TDriverResultSetAdapter<T> = class(TInterfacedObject, IDBResultset)
  private
    FDataset: T;
  protected
    function IsEmpty(): Boolean; virtual; abstract;
    function Next(): Boolean; virtual; abstract;
    function GetFieldValue(AIndex: Integer): Variant; overload; virtual; abstract;
    function GetFieldValue(const AFieldname: string): Variant; overload; virtual; abstract;
    function GetFieldCount(): Integer; virtual; abstract;
  public
    constructor Create(const ADataset: T); virtual;
    destructor Destroy; override;

    property Dataset: T read FDataset;
  end;

  TDriverConnectionAdapter<T> = class(TInterfacedObject, IDBConnection)
  private
    FConnection: T;
  protected
    procedure Connect(); virtual; abstract;
    procedure Disconnect(); virtual; abstract;
    function IsConnected(): Boolean; virtual; abstract;
    function CreateStatement(): IDBStatement; virtual; abstract;
    function BeginTransaction(): IDBTransaction; virtual; abstract;
    function GetDriverName(): string; virtual; abstract;
  public
    constructor Create(const AConnection: T); virtual;
    destructor Destroy; override;

    property Connection: T read FConnection;
  end;

  TDriverStatementAdapter<T> = class(TInterfacedObject, IDBStatement)
  private
    FStmt: T;
  public
    constructor Create(const AStatement: T); virtual;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); virtual; abstract;
    procedure SetParams(Params: TEnumerable<TDBParam>); overload; virtual; abstract;
    procedure SetParams(const AParams: array of const); overload;
    function Execute(): NativeUInt; virtual; abstract;
    function ExecuteQuery(): IDBResultSet; virtual; abstract;

    property Statement: T read FStmt;
  end;

implementation

{ TDriverResultSetAdapter<T> }

constructor TDriverResultSetAdapter<T>.Create(const ADataset: T);
begin
  inherited Create;
  FDataset := ADataset;
end;

destructor TDriverResultSetAdapter<T>.Destroy;
begin
  inherited Destroy;
end;

{ TDriverConnectionAdapter<T> }

constructor TDriverConnectionAdapter<T>.Create(const AConnection: T);
begin
  inherited Create;
  FConnection := AConnection;
end;

destructor TDriverConnectionAdapter<T>.Destroy;
begin
  inherited Destroy;
end;

{ TDriverStatementAdapter<T> }

constructor TDriverStatementAdapter<T>.Create(const AStatement: T);
begin
  inherited Create;
  FStmt := AStatement;
end;

destructor TDriverStatementAdapter<T>.Destroy;
begin
  inherited Destroy;
end;

procedure TDriverStatementAdapter<T>.SetParams(const AParams: array of const);
var
  LParams: TObjectList<TDBParam>;
begin
  if Length(AParams) > 0 then
  begin
    LParams := TObjectList<TDBParam>.Create;
    try
      ConvertParams(AParams, LParams);
      SetParams(LParams);
    finally
      LParams.Free;
    end;
  end;
end;

end.
