unit Core.Repository.Simple;

{$I sv.inc}

interface

uses
  Core.Interfaces
  ,Spring.Collections
  ,Core.Session
  ;


type
  TSimpleRepository<T: class, constructor; TID> = class(TInterfacedObject, IPagedRepository<T, TID>)
  private
    FSession: TSession;
  protected
    function Execute(const ASql: string; const AParams: array of const): NativeUInt; virtual;

    function GetList(const ASql: string;
      const AParams: array of const): IList<T>; virtual;

    function Count(): Int64; virtual;

    function FindOne(const AID: TID): T; virtual;

    function FindAll(): IList<T>; virtual;

    function Save(AEntity: T): T; overload; virtual;

    procedure SaveAll(AEntity: T); virtual;

    function Save(AEntities: ICollection<T>): ICollection<T>; overload; virtual;

    procedure Insert(AEntity: T); overload; virtual;

    procedure Insert(AEntities: ICollection<T>); overload; virtual;

    procedure Delete(AEntity: T); overload; virtual;

    procedure Delete(AEntities: ICollection<T>); overload; virtual;

    function Page(APage: Integer; AItemsPerPage: Integer): IDBPage<T>; virtual;

  public
    constructor Create(ASession: TSession); virtual;
  end;


implementation

uses
  Rtti
  ;

{ TSimpleRepository<T, TID> }

function TSimpleRepository<T, TID>.Count: Int64;
begin
  Result := FSession.Page<T>(1,1).GetTotalItems;
end;

constructor TSimpleRepository<T, TID>.Create(ASession: TSession);
begin
  inherited Create;
  FSession := ASession;
end;

procedure TSimpleRepository<T, TID>.Delete(AEntity: T);
begin
  FSession.Delete(AEntity);
end;

procedure TSimpleRepository<T, TID>.Delete(AEntities: ICollection<T>);
var
  LTransaction: IDBTransaction;
begin
  LTransaction := FSession.BeginTransaction;
  FSession.DeleteList<T>(AEntities);
  LTransaction.Commit;
end;

function TSimpleRepository<T, TID>.Execute(const ASql: string; const AParams: array of const): NativeUInt;
begin
  Result := FSession.Execute(ASql, AParams);
end;

function TSimpleRepository<T, TID>.FindAll: IList<T>;
begin
  Result := FSession.FindAll<T>;
end;

function TSimpleRepository<T, TID>.FindOne(const AID: TID): T;
begin
  Result := FSession.FindOne<T>(TValue.From<TID>(AID));
end;

function TSimpleRepository<T, TID>.GetList(const ASql: string; const AParams: array of const): IList<T>;
begin
  Result := FSession.GetList<T>(ASql, AParams);
end;

procedure TSimpleRepository<T, TID>.Insert(AEntity: T);
begin
  FSession.Insert(AEntity);
end;

procedure TSimpleRepository<T, TID>.Insert(AEntities: ICollection<T>);
var
  LTransaction: IDBTransaction;
begin
  LTransaction := FSession.BeginTransaction;
  FSession.InsertList<T>(AEntities);
  LTransaction.Commit;
end;

function TSimpleRepository<T, TID>.Page(APage, AItemsPerPage: Integer): IDBPage<T>;
begin
  Result := FSession.Page<T>(APage, AItemsPerPage);
end;

function TSimpleRepository<T, TID>.Save(AEntity: T): T;
begin
  FSession.Save(AEntity);
  Result := AEntity;
end;

procedure TSimpleRepository<T, TID>.SaveAll(AEntity: T);
var
  LTransaction: IDBTransaction;
begin
  LTransaction := FSession.BeginTransaction;
  FSession.SaveAll(AEntity);
  LTransaction.Commit;
end;

function TSimpleRepository<T, TID>.Save(AEntities: ICollection<T>): ICollection<T>;
var
  LTransaction: IDBTransaction;
begin
  LTransaction := FSession.BeginTransaction;
  FSession.SaveList<T>(AEntities);
  Result := AEntities;
  LTransaction.Commit;
end;

end.
