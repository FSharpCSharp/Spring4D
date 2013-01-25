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
unit Adapters.ASA;

interface

uses
  Adapters.ADO, SysUtils, Core.Interfaces;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent Sybase ASA resultset.
  ///	</summary>
  {$ENDREGION}
  TASAResultsetAdapter = class(TADOResultSetAdapter);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent Sybase ASA statement.
  ///	</summary>
  {$ENDREGION}
  TASAStatementAdapter = class(TADOStatementAdapter);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent Sybase ASA connection.
  ///	</summary>
  {$ENDREGION}
  TASAConnectionAdapter = class(TADOConnectionAdapter)
  public
    function GetDriverName: string; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent Sybase ASA transaction.
  ///	</summary>
  {$ENDREGION}
  TASATransactionAdapter = class(TADOTransactionAdapter);

  ESybaseASAStatementAdapterException = Exception;

implementation

uses
  Core.ConnectionFactory
  ,Core.Consts
  ;

{ TASAConnectionAdapter }

function TASAConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_SYBASE_ASA;
end;

initialization
  TConnectionFactory.RegisterConnection<TASAConnectionAdapter>(dtASA);
end.
