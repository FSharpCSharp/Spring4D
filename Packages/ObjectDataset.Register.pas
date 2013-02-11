unit ObjectDataset.Register;

interface

{$R ObjectDataset.res}

uses
  Adapters.ObjectDataset
  ,Classes
  ;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MarshmallowORM',
    [
      TObjectDataset
    ]
  );

end;

end.
