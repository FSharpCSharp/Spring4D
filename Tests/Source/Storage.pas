unit Storage;

interface

uses
  Spring,
  Spring.Collections,
  Interfaces;

type
  TSecondaryStorage = class(TInterfacedObject, IStorage)
  private
    fItems: IList<string>;
  public
    constructor Create;
    procedure Save(const items: string);
    property Items: IList<string> read fItems;
  end;

  TPrimaryStorage = class(TSecondaryStorage)
  private
    fIsUp: Boolean;
  public
    property IsUp: Boolean read fIsUp write fIsUp;
  end;

implementation

{ TSecondaryStorage }

constructor TSecondaryStorage.Create;
begin
  fItems := TCollections.CreateList<string>;
end;

procedure TSecondaryStorage.Save(const items: string);
begin
  fItems.Add(items);
end;

end.
