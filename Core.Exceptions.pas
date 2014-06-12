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
unit Core.Exceptions;

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

  EORMOptimisticLockException = class(EBaseORMException);

implementation

uses
  Mapping.RttiExplorer
  ,Mapping.Attributes
  ,Generics.Collections
  ,Rtti
  ;

{ EBaseORMException }

constructor EBaseORMException.Create(AEntity: TObject);
begin
  inherited Create(EntityToString(AEntity));
end;

function EBaseORMException.EntityToString(AEntity: TObject): string;
var
  LBuilder: TStringBuilder;
  LColumns: TList<ColumnAttribute>;
  LColumn: ColumnAttribute;
  LValue: TValue;
begin
  if not Assigned(AEntity) then
    Exit('null');
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendFormat('ClassName: %S', [AEntity.QualifiedClassName]).AppendLine;
    LColumns := TRttiExplorer.GetColumns(AEntity.ClassType);
    try
      for LColumn in LColumns do
      begin
        LValue := TRttiExplorer.GetMemberValue(AEntity, LColumn.ClassMemberName);
        LBuilder.AppendFormat('[%S] : %S', [LColumn.Name, LValue.ToString]).AppendLine;
      end;
    finally
      LColumns.Free;
    end;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

end.
