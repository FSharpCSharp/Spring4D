unit Core.Repository.MongoDB;

{$I sv.inc}

interface

uses
  Core.Repository.Simple
  ,Core.Session
  ,Core.Session.MongoDB
  ,Spring.Collections
  ;

type
  TMongoDBRepository<T:class, constructor; TID> = class(TSimpleRepository<T,TID>)
  private
    FSession: TMongoDBSession;
  protected
    procedure InsertList(ACollection: ICollection<T>); override;
  public
    constructor Create(ASession: TSession); override;
  end;

implementation

{ TMongoDBRepository<T, TID> }

constructor TMongoDBRepository<T, TID>.Create(ASession: TSession);
begin
  inherited Create(ASession);
  FSession := ASession as TMongoDBSession;
end;

procedure TMongoDBRepository<T, TID>.InsertList(ACollection: ICollection<T>);
begin
  FSession.BulkInsert<T>(ACollection);
end;

end.
