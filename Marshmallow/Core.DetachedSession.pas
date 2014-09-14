unit Core.DetachedSession;

interface

uses
  Core.Session
  ;

type
  {$REGION 'Documentation'}
    ///	<summary>
    ///	  Represents detached session. Detached session doesn't hold any history of loaded, saved, deleted entities,
    ///   so Save method always inserts new entity, because it doesn't know the state of an entity.
    ///  Detached session could be useful in those scenarios where you always know what action should be done (insert, update, or delete).
    ///  Also it is faster than ordinary session (no need to save entities state).
    ///	</summary>
    {$ENDREGION}
  TDetachedSession = class(TSession)
  protected
    procedure AttachEntity(AEntity: TObject); override;
    procedure DetachEntity(AEntity: TObject); override;
  end;

implementation

{ TDetachedSession }

procedure TDetachedSession.AttachEntity(AEntity: TObject);
begin
  //do nothing
end;

procedure TDetachedSession.DetachEntity(AEntity: TObject);
begin
  //do nothing
end;

end.
