unit Spring.Persistence.Core.Repository.MongoDB;

{$I sv.inc}

interface

uses
  Spring.Persistence.Core.Repository.Simple
  ,Spring.Persistence.Core.Session
  ,Spring.Persistence.Core.Session.MongoDB
  ,Spring.Collections
  ;

type
  TMongoDBRepository<T:class, constructor; TID> = class(TSimpleRepository<T,TID>)
  private
    FSession: TMongoDBSession;
  public
    procedure Insert(AEntities: ICollection<T>); overload; override;
    function Query(const AQuery: string;
      const AParams: array of const): IList<T>; override;
  public
    constructor Create(ASession: TSession); override;
  end;

implementation

uses
  SysUtils
  ;

{ TMongoDBRepository<T, TID> }

constructor TMongoDBRepository<T, TID>.Create(ASession: TSession);
begin
  inherited Create(ASession);
  FSession := ASession as TMongoDBSession;
end;

procedure TMongoDBRepository<T, TID>.Insert(AEntities: ICollection<T>);
begin
  FSession.BulkInsert<T>(AEntities);
end;

function TMongoDBRepository<T, TID>.Query(const AQuery: string;
  const AParams: array of const): IList<T>;
var
  LQuery: string;
begin
  LQuery := Format('S[%S]%S', [Namespace, AQuery]);
  Result := inherited Query(LQuery, AParams);
end;

end.
