{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2015 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
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

{$I Spring.inc}

unit Spring.Persistence.Core.Exceptions;

interface

uses
  SysUtils,
  Spring;

type
  /// <summary>
  ///   Base class for all ORM related exceptions. Cannot be instantiated
  ///   directly.
  /// </summary>
  EORMException = class abstract(Exception);

  EBaseORMException = class(EORMException)
  protected
    function EntityToString(const entity: TObject): string; virtual;
  public
    constructor Create(const entity: TObject); reintroduce; overload;
  end;

  EEntityAlreadyPersisted = class(EBaseORMException);

  ECannotPersististEntityWithId = class(EBaseORMException);

  ETableNotSpecified = class(EBaseORMException);

  EORMMethodNotImplemented = class(EORMException);

  EUnknownMember = class(EORMException);

  EORMEnumException = class(EORMException);

  EEntityManagerNotSet = class(EORMException);

  EUnknownJoinType = class(EORMException);

  EORMRecordNotFoundException = class(EORMException);

  EORMUpdateNotSuccessfulException = class(EBaseORMException);

  EORMColumnCannotBeNull = class(EBaseORMException);

  EORMColumnNotFound = class(EBaseORMException);
  EORMPrimaryKeyColumnNotFound = class(EBaseORMException);

  EORMInvalidConversion = class(EBaseORMException);

  EORMContainerDoesNotHaveAddMethod = class(EORMException);

  EORMContainerDoesNotHaveClearMethod = class(EORMException);

  EORMContainerDoesNotHaveCountMethod = class(EORMException);

  EORMContainerAddMustHaveOneParameter = class(EORMException);

  EORMContainerItemTypeNotSupported = class(EORMException);

  EORMUnsupportedType = class(EORMException);

  EORMUnsupportedOperation = class(EORMException);

  EORMConnectionAlreadyRegistered = class(EORMException);
  EORMRowMapperAlreadyRegistered = class(EORMException);
  EORMConnectionNotRegistered = class(EORMException);

  EORMManyToOneMappedByColumnNotFound = class(EORMException);

  EORMTransactionNotStarted = class(EORMException);

  EORMListInSession = class(EORMException);

  EORMCannotConvertValue = class(EORMException);

  EORMInvalidArguments = class(EORMException);

  EORMOptimisticLockException = class(EBaseORMException);

  EORMCannotGenerateQueryStatement = class(EBaseORMException);

  /// <summary>
  ///   Base class for all adapter related exceptions. Ie. exceptions that may
  ///   occur when calling directly into the DB driver (ie. outside ORM scope).
  ///   Unless generic failure exception is encountered, adapter functions may
  ///   not raise any other exception that doesn't descent from this base
  ///   class.
  /// </summary>
  /// <remarks>
  ///   Additionally optional <c>Code</c> property has been added that may be
  ///   used by any descendant which can provide such information.
  /// </remarks>
  EORMAdapterException = class abstract(EORMException)
  strict private
    fCode: Nullable<Integer>;
  public
    constructor Create(const msg: string); overload;
    constructor CreateFmt(const msg: string; const args: array of const); overload;
    constructor Create(const msg: string; aCode: Integer); overload;
    constructor CreateFmt(const msg: string; const args: array of const; aCode: Integer); overload;

    /// <summary>
    ///   Status code of the operation (optional).
    /// </summary>
    /// <remarks>
    ///   Driver dependent
    /// </remarks>
    property Code: Nullable<Integer> read fCode;
  end;

implementation

uses
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Mapping.Attributes,
  Spring.Reflection;


{$REGION 'EBaseORMException'}

constructor EBaseORMException.Create(const entity: TObject);
begin
  inherited Create(EntityToString(entity));
end;

function EBaseORMException.EntityToString(const entity: TObject): string;
begin
  if not Assigned(entity) then
    Result := 'null'
  else
    Result := 'ClassName: ' + entity.ClassName;
end;

{$ENDREGION}


{ EORMAdapterException }

constructor EORMAdapterException.Create(const msg: string);
begin
  inherited Create(msg);
end;

constructor EORMAdapterException.Create(const msg: string; aCode: Integer);
begin
  Create(msg);
  fCode := aCode;
end;

constructor EORMAdapterException.CreateFmt(const msg: string;
  const args: array of const);
begin
  inherited CreateFmt(msg, args);
end;

constructor EORMAdapterException.CreateFmt(const msg: string;
  const args: array of const; aCode: Integer);
begin
  CreateFmt(msg, args);
  fCode := aCode;
end;

end.
