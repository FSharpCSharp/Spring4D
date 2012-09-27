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
unit Core.Criteria;

{$I sv.inc}

interface

uses
  Core.Criteria.Abstract
  ,Core.Criteria.Criterion
  ,Generics.Collections
  {$IFDEF USE_SPRING},Spring.Collections {$ENDIF}
  ;

type
  TCriteria<T: class, constructor> = class(TAbstractCriteria)
  private
    FCriterions: TObjectList<TCriterion>;
  public
    constructor Create();
    destructor Destroy; override;

    function Add(const ACriterion: TCriterion): TCriteria<T>;

    function Fetch(): {$IFDEF USE_SPRING}IList<T>{$ELSE}TObjectList<T>{$ENDIF};
  end;

implementation

{ TCriteria }

function TCriteria<T>.Add(const ACriterion: TCriterion): TCriteria<T>;
begin
  ACriterion.EntityClass := EntityClass;
  FCriterions.Add(ACriterion);
  Result := Self;
end;

constructor TCriteria<T>.Create();
begin
  inherited Create(T);
  FCriterions := TObjectList<TCriterion>.Create(True);
end;

destructor TCriteria<T>.Destroy;
begin
  FCriterions.Free;
  inherited Destroy;
end;

function TCriteria<T>.Fetch: {$IFDEF USE_SPRING}IList<T>{$ELSE}TObjectList<T>{$ENDIF};
begin
  {TODO -oOwner -cGeneral : return the list of entities satisfying criteria}
end;

end.
