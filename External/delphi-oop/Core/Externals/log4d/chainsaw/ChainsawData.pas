unit ChainsawData;

{
  Data for Chainsaw.

  Written by Keith Wood (kbwood@iprimus.com.au)
  Version 1.0 - 19 September 2003.
}

interface

uses
  SysUtils, Classes, DB, DBClient;

type
  TdtmLogging = class(TDataModule)
    cdsLogging: TClientDataSet;
    cdsLoggingThreadId: TStringField;
    cdsLoggingTimestamp: TSQLTimeStampField;
    cdsLoggingElapsedTime: TIntegerField;
    cdsLoggingLevelName: TStringField;
    cdsLoggingLevelValue: TIntegerField;
    cdsLoggingLoggerName: TStringField;
    cdsLoggingMessage: TMemoField;
    cdsLoggingNDC: TStringField;
    cdsLoggingErrorMessage: TStringField;
    cdsLoggingErrorClass: TStringField;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure cdsLoggingMessageGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
  private
    FFieldMapping: TStringList;
  public
    property FieldMapping: TStringList read FFieldMapping;
    procedure AddLog(const Message, ThreadId: string;
      const Timestamp: TDateTime; const ElapsedTime: Integer;
      const LevelName: string; const LevelValue: Integer;
      const LoggerName, NDC, ErrorMessage, ErrorClass: string);
    procedure AddSort(const FieldName: string);
    procedure EmptyDataSet;
  end;

var
  dtmLogging: TdtmLogging;

implementation

{$R *.dfm}

{ Initialisation - set field mappings both ways
  between field names and display labels. }
procedure TdtmLogging.DataModuleCreate(Sender: TObject);
var
  Index: Integer;
  Field: TField;
begin
  FFieldMapping := TStringList.Create;
  for Index := 0 to cdsLogging.FieldCount - 1 do
  begin
    Field                                    := cdsLogging.Fields[Index];
    FFieldMapping.Values[Field.FieldName]    := Field.DisplayLabel;
    FFieldMapping.Values[Field.DisplayLabel] := Field.FieldName;
  end;
end;

{ Release resources. }
procedure TdtmLogging.DataModuleDestroy(Sender: TObject);
begin
  FFieldMapping.Free;
end;

{ Add a logging event to the dataset. }
procedure TdtmLogging.AddLog(const Message, ThreadId: string;
  const Timestamp: TDateTime; const ElapsedTime: Integer;
  const LevelName: string; const LevelValue: Integer;
  const LoggerName, NDC, ErrorMessage, ErrorClass: string);
begin
  with cdsLogging do
  begin
    Insert;
    cdsLoggingMessage.AsString      := Message;
    cdsLoggingThreadId.AsString     := ThreadId;
    cdsLoggingTimestamp.AsDateTime  := Timestamp;
    cdsLoggingElapsedTime.AsInteger := ElapsedTime;
    cdsLoggingLevelName.AsString    := LevelName;
    cdsLoggingLevelValue.AsInteger  := LevelValue;
    cdsLoggingLoggerName.AsString   := LoggerName;
    cdsLoggingNDC.AsString          := NDC;
    cdsLoggingErrorMessage.AsString := ErrorMessage;
    cdsLoggingErrorClass.AsString   := ErrorClass;
    Post;
  end;
end;

{ Add a field to the list of those used to sort the dataset (ascending only). }
procedure TdtmLogging.AddSort(const FieldName: string);
var
  FieldNames: string;
  Index: Integer;
begin
  FieldNames := cdsLogging.IndexFieldNames;
  Index := Pos(FieldName, FieldNames);
  if Index > 0 then
    Delete(FieldNames, Index, Length(FieldName) + 1);
  FieldNames                 := FieldName + ';' + FieldNames;
  cdsLogging.IndexFieldNames := FieldNames;
end;

{ Return the memo message field contents. }
procedure TdtmLogging.cdsLoggingMessageGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  Text := Sender.AsString;
end;

{ Remove all log events from the dataset. }
procedure TdtmLogging.EmptyDataSet;
begin
  cdsLogging.EmptyDataSet;
end;

end.
