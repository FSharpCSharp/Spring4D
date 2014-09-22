{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Persistence.Core.Exceptions;

{$I Spring.inc}

interface

uses
  SysUtils;

type
  EBaseORMException = class(Exception)
  protected
    function EntityToString(AEntity: TObject): string; virtual;
  public
    constructor Create(AEntity: TObject); reintroduce; overload;
  end;

  EEntityAlreadyPersisted = class(EBaseORMException);

  ECannotPersististEntityWithId = class(EBaseORMException);

  ETableNotSpecified = class(EBaseORMException);

  EORMMethodNotImplemented = class(Exception);

  EUnknownMember = class(Exception);

  EORMEnumException = class(Exception);

  EEntityManagerNotSet = class(Exception);

  EUnknownJoinType = class(Exception);

  EORMRecordNotFoundException = class(Exception);

  EORMUpdateNotSuccessfulException = class(EBaseORMException);

  EORMColumnCannotBeNull = class(EBaseORMException);

  EORMColumnNotFound = class(EBaseORMException);

  EORMContainerDoesNotHaveAddMethod = class(Exception);

  EORMContainerDoesNotHaveClearMethod = class(Exception);

  EORMContainerDoesNotHaveCountMethod = class(Exception);

  EORMContainerAddMustHaveOneParameter = class(Exception);

  EORMContainerItemTypeNotSupported = class(Exception);

  EORMUnsupportedType = class(Exception);

  EORMConnectionAlreadyRegistered = class(Exception);
  EORMConnectionNotRegistered = class(Exception);

  EORMManyToOneMappedByColumnNotFound = class(Exception);

  EORMTransactionNotStarted = class(Exception);

  EORMListInSession = class(Exception);

  EORMCannotConvertValue = class(Exception);

  EORMInvalidArguments = class(Exception);

  EORMOptimisticLockException = class(EBaseORMException);

implementation

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.Mapping.RttiExplorer;

{ EBaseORMException }

constructor EBaseORMException.Create(AEntity: TObject);
begin
  inherited Create(EntityToString(AEntity));
end;

function EBaseORMException.EntityToString(AEntity: TObject): string;
var
  LBuilder: TStringBuilder;
  LColumns: IList<ColumnAttribute>;
  LColumn: ColumnAttribute;
  LValue: TValue;
begin
  if not Assigned(AEntity) then
    Exit('null');
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendFormat('ClassName: %S', [AEntity.ClassName]).AppendLine;
    LColumns := TRttiExplorer.GetColumns(AEntity.ClassType);
    for LColumn in LColumns do
    begin
      LValue := TRttiExplorer.GetMemberValue(AEntity, LColumn.ClassMemberName);
      LBuilder.AppendFormat('[%S] : %S', [LColumn.Name, LValue.ToString]).AppendLine;
    end;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

end.
