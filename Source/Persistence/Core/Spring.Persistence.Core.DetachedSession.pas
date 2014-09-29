unit Spring.Persistence.Core.DetachedSession;

interface

uses
  Spring.Persistence.Core.Session;

type
  /// <summary>
  ///   Represents detached session. Detached session doesn't hold any history
  ///   of loaded, saved, deleted entities, so Save method always inserts new
  ///   entity, because it doesn't know the state of an entity. Detached
  ///   session could be useful in those scenarios where you always know what
  ///   action should be done (insert, update, or delete). Also it is faster
  ///   than ordinary session (no need to save entities state).
  /// </summary>
  TDetachedSession = class(TSession)
  protected
    procedure AttachEntity(const entity: TObject); override;
    procedure DetachEntity(const entity: TObject); override;
  end;

implementation


{$REGION 'TDetachedSession'}

procedure TDetachedSession.AttachEntity(const entity: TObject);
begin
  // do nothing
end;

procedure TDetachedSession.DetachEntity(const entity: TObject);
begin
  // do nothing
end;

{$ENDREGION}


end.
