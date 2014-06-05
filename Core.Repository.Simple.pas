unit Core.Repository.Simple;

{$I sv.inc}

interface

uses
  Core.Interfaces
  ,Spring.Collections
  ,Core.Session
  ;


type
  TSimpleRepository<T: class, constructor; TID> = class(TInterfacedObject, IRepository<T, TID>)
  private
    FSession: TSession;
  protected
    function BeginListSession(AList: IList<T>): IListSession<T>; virtual;

    function CreateCriteria(): ICriteria<T>; virtual;

    function Execute(const ASql: string; const AParams: array of const): NativeUInt; virtual;

    function GetList(const ASql: string;
      const AParams: array of const): IList<T>; virtual;

    function FindOne(const AID: TID): T; virtual;

    function FindAll(): IList<T>; virtual;

    procedure Save(AEntity: T); virtual;

    procedure SaveAll(AEntity: T); virtual;

    procedure SaveList(ACollection: ICollection<T>); virtual;

    procedure Insert(AEntity: T); virtual;

    procedure InsertList(ACollection: ICollection<T>); virtual;

    function IsNew(AEntity: T): Boolean; virtual;

    procedure Update(AEntity: T); virtual;

    procedure UpdateList(ACollection: ICollection<T>); virtual;

    procedure Delete(AEntity: T); virtual;

    procedure DeleteList(ACollection: ICollection<T>); virtual;

    function Page(APage: Integer; AItemsPerPage: Integer): IDBPage<T>; virtual;

  public
    constructor Create(ASession: TSession); virtual;
  end;


implementation

uses
  Rtti
  ;

{ TSimpleRepository<T, TID> }

function TSimpleRepository<T, TID>.BeginListSession(AList: IList<T>): IListSession<T>;
begin
  Result := FSession.BeginListSession<T>(AList);
end;

constructor TSimpleRepository<T, TID>.Create(ASession: TSession);
begin
  inherited Create;
  FSession := ASession;
end;

function TSimpleRepository<T, TID>.CreateCriteria: ICriteria<T>;
begin
  Result := FSession.CreateCriteria<T>();
end;

procedure TSimpleRepository<T, TID>.Delete(AEntity: T);
begin
  FSession.Delete(AEntity);
end;

procedure TSimpleRepository<T, TID>.DeleteList(ACollection: ICollection<T>);
begin
  FSession.DeleteList<T>(ACollection);
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

procedure TSimpleRepository<T, TID>.InsertList(ACollection: ICollection<T>);
begin
  FSession.InsertList<T>(ACollection);
end;

function TSimpleRepository<T, TID>.IsNew(AEntity: T): Boolean;
begin
  Result := FSession.IsNew(AEntity);
end;

function TSimpleRepository<T, TID>.Page(APage, AItemsPerPage: Integer): IDBPage<T>;
begin
  Result := FSession.Page<T>(APage, AItemsPerPage);
end;

procedure TSimpleRepository<T, TID>.Save(AEntity: T);
begin
  FSession.Save(AEntity);
end;

procedure TSimpleRepository<T, TID>.SaveAll(AEntity: T);
begin
  FSession.SaveAll(AEntity);
end;

procedure TSimpleRepository<T, TID>.SaveList(ACollection: ICollection<T>);
begin
  FSession.SaveList<T>(ACollection);
end;

procedure TSimpleRepository<T, TID>.Update(AEntity: T);
begin
  FSession.Update(AEntity);
end;

procedure TSimpleRepository<T, TID>.UpdateList(ACollection: ICollection<T>);
begin
  FSession.UpdateList<T>(ACollection);
end;

end.
