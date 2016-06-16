unit Utilities;

interface

uses
  SyncObjs;

type
  TManualResetEvent = class(TEvent)
  public
    constructor Create(initialState: Boolean);
  end;

implementation

constructor TManualResetEvent.Create(initialState: Boolean);
begin
  inherited Create(nil, True, initialState, '');
end;

end.
