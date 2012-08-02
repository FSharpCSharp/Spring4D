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
unit Core.Interfaces;

interface

uses
  SQL.Params, Generics.Collections;

type
  TDBDriverType = (dtSQLite = 0 {$IFDEF MSWINDOWS}, dtMSSQL{$ENDIF});

  TExecutionListenerProc = reference to procedure(const ACommand: string; const AParams: TObjectList<TDBParam>);

  IDBResultset = interface
    ['{4FA97CFB-4992-4DAA-BB2A-B5CAF84B6B47}']
    function IsEmpty(): Boolean;
    function Next(): Boolean;
    function GetFieldValue(AIndex: Integer): Variant; overload;
    function GetFieldValue(const AFieldname: string): Variant; overload;
  end;

  IDBStatement = interface
    ['{DA905CAA-0FC2-4570-9788-1DC206600171}']
    procedure SetSQLCommand(const ACommandText: string);
    procedure SetParams(AParams: TEnumerable<TDBParam>); overload;
    procedure SetParams(const AParams: array of const); overload;
    function Execute(): NativeUInt;
    function ExecuteQuery(): IDBResultset;
  end;

  IDBTransaction = interface
    ['{AA35EE88-7271-4894-B6F0-06080C797BCF}']
    procedure Commit();
    procedure Rollback();
  end;

  IDBConnection = interface
    ['{256B8F14-7FF1-4442-A202-358B24756654}']
    procedure Connect();
    procedure Disconnect();
    function IsConnected(): Boolean;
    function CreateStatement(): IDBStatement;
    function BeginTransaction(): IDBTransaction;
    function GetDriverName(): string;
    procedure AddExecutionListener(const AListenerProc: TExecutionListenerProc);
    procedure ClearExecutionListeners();
    function GetExecutionListeners: TList<TExecutionListenerProc>;
    property ExecutionListeners: TList<TExecutionListenerProc> read GetExecutionListeners;
    procedure NotifyExecutionListeners(const ACommand: string; const AParams: TObjectList<TDBParam>);
  end;

  IEntitySerializer = interface
    ['{BF7320F9-2B57-4B8C-997D-2F157D626D0D}']

  end;

  IODBC = interface
    ['{7A235A2E-1ABA-4AD6-A6FD-276A16374596}']
    function GetDatasources: TArray<string>;
  end;

implementation

end.
