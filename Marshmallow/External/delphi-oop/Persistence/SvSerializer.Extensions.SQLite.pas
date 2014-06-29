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
unit SvSerializer.Extensions.SQLite;

interface

uses
  SvSerializer, SysUtils;

type
  ESQLiteSerializerException = class(Exception);

  /// <summary>
  /// Serializer which uses SQLite embedded database for data storage.
  /// Very solid storage because SQLite transactions are atomic, consistent, isolated, and durable (ACID) even after system crashes and power failures.
  /// </summary>
  TSQLiteSerializer = class(TSvSerializer)
  private
    FUniqueKey: string;
  public
    constructor Create(AFormat: TSvSerializeFormat = sstJson); override;
    /// <summary>
    /// Deserializes data from SQLite database file
    /// </summary>
    /// <param name="AFromFilename">SQLite database filename</param>
    procedure DeSerialize(const AFromFilename: string); override;
    /// <summary>
    /// Serializes data to SQLite database file
    /// </summary>
    /// <param name="AToFilename">SQLite database filename</param>
    procedure Serialize(const AToFilename: string); override;
    /// <summary>
    /// Unique key, which will be used to store and retrieve current serializer data.
    /// This key should be unique for the same serializer which serializes and deserializes data
    /// </summary>
    property UniqueKey: string read FUniqueKey write FUniqueKey;
  end;

implementation

uses
  SQLiteTable3,
  Classes;

const
  TABLENAME = 'TBL_SERIALIZER';

  SQL_CREATE_TABLE = 'CREATE TABLE "TBL_SERIALIZER" ('+
    '"AKEY" TEXT NOT NULL CONSTRAINT "UQ_KEY" UNIQUE,'+
    '"DATA" BLOB);';

  SQL_SELECT_DATA = 'SELECT "DATA" FROM "TBL_SERIALIZER" WHERE "AKEY" = ?;';

  SQL_UPDATE_DATA = 'INSERT OR REPLACE INTO "TBL_SERIALIZER" ("AKEY","DATA") VALUES (?,?);';

  UNIQUE_KEY = '{6FA8C01F-3B4F-46D3-B73C-CE0FFD28AD68}';

resourcestring
  ERR_UPDATE = 'Cannot update data in the table.';
  ERR_INCOMPATIBLE_FILE = 'Incompatible file for deserialization.';
  ERR_CANNOT_FIND_DATA = 'Cannot find data for the current unique key.';
  ERR_CANNOT_RETRIEVE_DATA = 'Cannot retrieve data for the current unique key.';

{ TSQLiteSerializer }

constructor TSQLiteSerializer.Create(AFormat: TSvSerializeFormat);
begin
  inherited;
  FUniqueKey := UNIQUE_KEY;
end;

procedure TSQLiteSerializer.DeSerialize(const AFromFilename: string);
var
  db: TSQLiteDatabase;
  Stream: TStream;
  Qry: ISQLiteTable;
begin
  db := TSQLiteDatabase.Create(AFromFilename);
  try
    if not db.TableExists(TABLENAME) then
    begin
      Errors.Add(ERR_INCOMPATIBLE_FILE);
      raise ESQLiteSerializerException.Create(ERR_INCOMPATIBLE_FILE + '. File: ' + AFromFilename);
    end;

    Qry := db.GetUniTableIntf(SQL_SELECT_DATA, [FUniqueKey]);

    if Qry.EOF then
    begin
      Errors.Add(ERR_CANNOT_FIND_DATA);
      Exit;
    end;

    Stream := Qry.Fields[0].AsBlob;
    if not Assigned(Stream) then
    begin
      Errors.Add(ERR_CANNOT_RETRIEVE_DATA);
      Exit;
    end;

    try
      Stream.Position := 0;

      DeSerialize(Stream);

    finally
      Stream.Free;
    end;

  finally
    db.Free;
  end;
end;

procedure TSQLiteSerializer.Serialize(const AToFilename: string);
var
  db: TSQLiteDatabase;
  Stream: TStream;
  iRows: Integer;
begin
  db := TSQLiteDatabase.Create(AToFilename);
  Stream := TMemoryStream.Create;
  try
    if not db.TableExists(TABLENAME) then
    begin
      db.ExecSQL(SQL_CREATE_TABLE);
    end;

    Serialize(Stream);

    Stream.Position := 0;

    db.ExecSQL(SQL_UPDATE_DATA, [FUniqueKey, Stream], iRows);

    if iRows < 1 then
    begin
      {TODO -oLinas -cGeneral : do something with errors}
      //AbstractSerializer.Errors.Add(ERR_UPDATE);
    end;

  finally
    db.Free;
    Stream.Free;
  end;
end;

end.
