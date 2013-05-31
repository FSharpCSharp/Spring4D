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
unit Core.Consts;

interface

const
  METHODNAME_CONTAINER_ADD = 'Add';
  METHODNAME_CONTAINER_OWNSOBJECTS = 'OwnsObjects';

  DRIVER_MSSQL = 'MSSQL';
  DRIVER_SQLITE = 'SQLite3';
  DRIVER_ORACLE = 'Oracle';  //not used yet
  DRIVER_SYBASE_ASA = 'ASA';
  DRIVER_MYSQL = 'MySQL';  //not used yet
  DRIVER_FIREBIRD = 'Firebird';  //not used yet
  DRIVER_POSTGRESQL = 'PostgreSQL';   //not used yet
  DRIVER_ADO = 'ADO';
  DRIVER_DBX = 'DBX';
  DRIVER_UIB = 'UIB';

resourcestring
  EXCEPTION_CANNOT_COMMIT = 'Cannot commit unstarted transaction';
  EXCEPTION_CANNOT_ROLLBACK = 'Cannot rollback unstarted transaction';
  EXCEPTION_PRIMARYKEY_NOTFOUND = 'Primary key column "%S" not found.';
  EXCEPTION_COLUMN_NOTFOUND = 'Column "%S" not found.';
  EXCEPTION_CONTAINER_DOESNOTHAVE_ADD = 'Container does not have Add method';
  EXCEPTION_CONTAINER_ADD_ONE_PARAM = 'Container''s Add method must have only one parameter.';
  EXCEPTION_CONTAINER_ITEM_TYPE_NOTSUPPORTED = 'Container''s items type not supported';
  EXCEPTION_UNSUPPORTED_LAZY_TYPE = 'Unsupported type for lazy value: %S.';
  EXCEPTION_UNSUPPORTED_CONTAINER_TYPE = 'List must be Spring interface IList<T>.';
  EXCEPTION_QUERY_NO_RECORDS = 'Query returned 0 records.';
  EXCEPTION_CANNOT_OPEN_QUERY = 'Cannot open query. Error Message: ' + #13#10 + '%S';
  EXCEPTION_CANNOT_CONVERT_TYPE = 'Cannot convert from type "%0:S" into type "%1:S"';

implementation

end.
