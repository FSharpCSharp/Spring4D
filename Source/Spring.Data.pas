{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://www.zuobaoquan.com (Simplified Chinese)              }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit Spring.Data experimental;

{$I Spring.inc}

interface

type
  IDBConnection  = interface;
  IDBTransaction = interface;

  // Closed	      The connection is closed.
  // Open	        The connection is open.
  // Connecting	  The connection object is connecting to the data source. (This value is reserved for future versions of the product.)
  // Executing	  The connection object is executing a command. (This value is reserved for future versions of the product.)
  // Fetching	    The connection object is retrieving data. (This value is reserved for future versions of the product.)
  // Broken	      The connection to the data source is broken. This can occur only after the connection has been opened. A connection in this state may be closed and then re-opened. (This value is reserved for future versions of the product.)
  TConnectionState = (
    csClosed,
    csOpen,
    csConnecting,
    csExecuting,
    csFetching,
    csBroken
  );

  //  Unspecified	      A different isolation level than the one specified is being used, but the level cannot be determined.
  //  Chaos	            The pending changes from more highly isolated transactions cannot be overwritten.
  //  ReadUncommitted	  A dirty read is possible, meaning that no shared locks are issued and no exclusive locks are honored.
  //  ReadCommitted	    Shared locks are held while the data is being read to avoid dirty reads, but the data can be changed before the end of the transaction, resulting in non-repeatable reads or phantom data.
  //  RepeatableRead	  Locks are placed on all data that is used in a query, preventing other users from updating the data. Prevents non-repeatable reads but phantom rows are still possible.
  //  Serializable	    A range lock is placed on the DataSet, preventing other users from updating or inserting rows into the dataset until the transaction is complete.
  //  Snapshot	        Reduces blocking by storing a version of data that one application can read while another is modifying the same data. Indicates that from one transaction you cannot see changes made in other transactions, even if you requery.
  TIsolationLevel = (
    ilUnspecified,
    ilChaos,
    ilReadUncommitted,
    ilReadCommitted,
    ilRepeatableRead,
    ilSerializable,
    ilSnapshot
  );

  IDBConnection = interface
    {$REGION 'Property Getters & Setters'}
      function GetConnected: Boolean;
      function GetConnectionTimeout: Integer;
      function GetConnectionString: string;
      function GetDatabase: string;
      function GetState: TConnectionState;
      function GetInTransaction: Boolean;
      procedure SetConnectionString(const value: string);
      procedure SetConnected(value: Boolean);
    {$ENDREGION}
    procedure Open;
    procedure Close;
    function BeginTransaction: IDBTransaction; overload;
    function BeginTransaction(isolationLevel: TIsolationLevel): IDBTransaction; overload;
    property Connected: Boolean read GetConnected write SetConnected;
    property ConnectionString: string read GetConnectionString write SetConnectionString;
    property ConnectionTimeout: Integer read GetConnectionTimeout;
    property Database: string read GetDatabase;
    property State: TConnectionState read GetState;
    property InTransaction: Boolean read GetInTransaction;
  end;

  IDBTransaction = interface
    {$REGION 'Property Getters & Setters'}
      function GetConnection: IDBConnection;
      function GetIsolationLevel: TIsolationLevel;
    {$ENDREGION}
    procedure Commit;
    procedure Rollback;
    property Connection: IDBConnection read GetConnection;
    property IsolationLevel: TIsolationLevel read GetIsolationLevel;
  end;


implementation

end.
