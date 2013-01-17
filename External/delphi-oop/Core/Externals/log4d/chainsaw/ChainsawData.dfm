object dtmLogging: TdtmLogging
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 201
  Top = 107
  Height = 150
  Width = 215
  object cdsLogging: TClientDataSet
    Active = True
    Aggregates = <>
    Filtered = True
    FieldDefs = <
      item
        Name = 'ThreadId'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Timestamp'
        DataType = ftTimeStamp
      end
      item
        Name = 'ElapsedTime'
        DataType = ftInteger
      end
      item
        Name = 'LevelName'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'LevelValue'
        DataType = ftInteger
      end
      item
        Name = 'LoggerName'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'Message'
        DataType = ftMemo
      end
      item
        Name = 'NDC'
        DataType = ftString
        Size = 100
      end
      item
        Name = 'ErrorMessage'
        DataType = ftString
        Size = 100
      end
      item
        Name = 'ErrorClass'
        DataType = ftString
        Size = 40
      end>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'Timestamp;'
    Params = <>
    StoreDefs = True
    Left = 36
    Top = 16
    Data = {
      560100009619E0BD02000000180000000A000000000003000000560108546872
      656164496401004900000001000557494454480200020014000954696D657374
      616D7010001100000001000753554254595045020049000A00466F726D617474
      6564000B456C617073656454696D650400010000000000094C6576656C4E616D
      6501004900000001000557494454480200020014000A4C6576656C56616C7565
      04000100000000000A4C6F676765724E616D6501004900000001000557494454
      48020002003200074D65737361676504004B0000000100075355425459504502
      00490005005465787400034E4443010049000000010005574944544802000200
      64000C4572726F724D6573736167650100490000000100055749445448020002
      0064000A4572726F72436C617373010049000000010005574944544802000200
      280001000D44454641554C545F4F524445520200820000000000}
    object cdsLoggingThreadId: TStringField
      DisplayLabel = 'Thread Id'
      DisplayWidth = 8
      FieldName = 'ThreadId'
    end
    object cdsLoggingTimestamp: TSQLTimeStampField
      DisplayWidth = 20
      FieldName = 'Timestamp'
    end
    object cdsLoggingElapsedTime: TIntegerField
      DisplayLabel = 'Elapsed Time'
      FieldName = 'ElapsedTime'
    end
    object cdsLoggingLevelName: TStringField
      DisplayLabel = 'Level Name'
      DisplayWidth = 10
      FieldName = 'LevelName'
    end
    object cdsLoggingLevelValue: TIntegerField
      DisplayLabel = 'Level Value'
      FieldName = 'LevelValue'
    end
    object cdsLoggingLoggerName: TStringField
      DisplayLabel = 'Logger Name'
      DisplayWidth = 20
      FieldName = 'LoggerName'
      Size = 50
    end
    object cdsLoggingMessage: TMemoField
      DisplayWidth = 40
      FieldName = 'Message'
      OnGetText = cdsLoggingMessageGetText
      BlobType = ftMemo
    end
    object cdsLoggingNDC: TStringField
      DisplayWidth = 20
      FieldName = 'NDC'
      Size = 100
    end
    object cdsLoggingErrorMessage: TStringField
      DisplayLabel = 'Error Message'
      DisplayWidth = 40
      FieldName = 'ErrorMessage'
      Size = 100
    end
    object cdsLoggingErrorClass: TStringField
      DisplayLabel = 'Error Class'
      DisplayWidth = 20
      FieldName = 'ErrorClass'
      Size = 40
    end
  end
end
