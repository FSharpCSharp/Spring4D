unit Spring.Persistence.Core.ListSession;

interface

uses
  Spring.Collections
  ,Rtti
  ,Spring.Persistence.Core.Interfaces
  ,Spring.Persistence.Core.Session
  ;

type
  TListSession<T: class, constructor> = class(TInterfacedObject, IListSession<T>)
  private
    FOwner: TObject;
    FList: IList<T>;
    FPrimaryKeys: IDictionary<TValue, Boolean>;
  protected
    procedure DoOnListChanged(Sender: TObject; const Item: T; Action: TCollectionChangedAction);

    procedure CommitListSession(); virtual;
    procedure RollbackListSession(); virtual;

    procedure DeleteEntities();
  public
    constructor Create(AOwner: TObject; AList: IList<T>); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions
  ,Spring.Persistence.Mapping.RttiExplorer
  ,Spring.Persistence.SQL.Commands.Delete
  ,Spring.Persistence.SQL.Commands.Factory
  ,Generics.Collections
  ;

{ TListSession }

procedure TListSession<T>.CommitListSession;
var
  LSession: TSession;
begin
  LSession := FOwner as TSession;
  //delete first
  DeleteEntities();

  LSession.SaveList<T>(FList);
end;

constructor TListSession<T>.Create(AOwner: TObject; AList: IList<T>);
begin
  inherited Create();
  FPrimaryKeys := TCollections.CreateDictionary<TValue, Boolean>();
  FOwner := AOwner;
  FList := AList;
  FList.OnChanged.Add(DoOnListChanged);
end;

procedure TListSession<T>.DeleteEntities;
var
  LDeleter: TDeleteByValueExecutor;
  LKey: TPair<TValue, Boolean>;
begin
  LDeleter := CommandFactory.GetCommand<TDeleteByValueExecutor>(T, (FOwner as TSession).Connection );
  try
    for LKey in FPrimaryKeys do
    begin
      LDeleter.PrimaryKeyValue := LKey.Key;
      LDeleter.Execute(nil);
    end;

  finally
    LDeleter.Free;
  end;
end;

destructor TListSession<T>.Destroy;
begin
  FList.OnChanged.Remove(DoOnListChanged);
  inherited Destroy;
end;


procedure TListSession<T>.DoOnListChanged(Sender: TObject; const Item: T; Action: TCollectionChangedAction);
var
  LSession: TSession;
  LValue: TValue;
begin
  case Action of
    caAdded: ;
    caRemoved:
    begin
      LSession := FOwner as TSession;
      if not LSession.IsNew(Item) then
      begin
        LValue := TRttiExplorer.GetPrimaryKeyValue(Item);
        if not FPrimaryKeys.ContainsKey(LValue) then
        begin
          FPrimaryKeys.Add(LValue, True);
          LSession.OldStateEntities.Remove(Item);
        end;
      end;
    end;
    caReplaced: ;
    caMoved: ;
    caReseted: ;
  end;
end;

procedure TListSession<T>.RollbackListSession;
begin
  FPrimaryKeys.Clear;
end;

end.
