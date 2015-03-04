object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample 1'
  ClientHeight = 654
  ClientWidth = 1056
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1056
    654)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 433
    Top = 136
    Width = 91
    Height = 13
    Caption = 'ProductionGroupID'
  end
  object Label2: TLabel
    Left = 433
    Top = 152
    Width = 24
    Height = 13
    Caption = 'Type'
  end
  object LabelProductionGroupID: TLabel
    Left = 537
    Top = 136
    Width = 4
    Height = 13
    Caption = '-'
  end
  object LabelType: TLabel
    Left = 537
    Top = 152
    Width = 4
    Height = 13
    Caption = '-'
  end
  object Bevel1: TBevel
    Left = 431
    Top = 331
    Width = 616
    Height = 36
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsBottomLine
  end
  object Bevel2: TBevel
    Left = 431
    Top = 120
    Width = 616
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Memo1: TMemo
    Left = 601
    Top = 373
    Width = 447
    Height = 273
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 14
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object MemoJSON: TMemo
    Left = 8
    Top = 48
    Width = 417
    Height = 563
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 14
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      '['
      '    {'
      '        "ProductionGroupId": 101,'
      '        "Type": "Undefined",'
      '        "Datapoints": ['
      '            {'
      '                "StartTimeUTC": "2014-05-24T06:00:00",'
      '                "EndTimeUTC": "2014-05-24T06:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 697.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T06:15:00",'
      '                "EndTimeUTC": "2014-05-24T06:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 699.6'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T06:30:00",'
      '                "EndTimeUTC": "2014-05-24T06:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 734'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T06:45:00",'
      '                "EndTimeUTC": "2014-05-24T07:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 712.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T07:00:00",'
      '                "EndTimeUTC": "2014-05-24T07:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 719.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T07:15:00",'
      '                "EndTimeUTC": "2014-05-24T07:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 651.4'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T07:30:00",'
      '                "EndTimeUTC": "2014-05-24T07:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 888'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T07:45:00",'
      '                "EndTimeUTC": "2014-05-24T08:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 759.2'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T08:00:00",'
      '                "EndTimeUTC": "2014-05-24T08:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 741.5'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T08:15:00",'
      '                "EndTimeUTC": "2014-05-24T08:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 829'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T08:30:00",'
      '                "EndTimeUTC": "2014-05-24T08:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 773.7'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T08:45:00",'
      '                "EndTimeUTC": "2014-05-24T09:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 725.7'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T09:00:00",'
      '                "EndTimeUTC": "2014-05-24T09:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 767.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T09:15:00",'
      '                "EndTimeUTC": "2014-05-24T09:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 694.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T09:30:00",'
      '                "EndTimeUTC": "2014-05-24T09:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 602.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T09:45:00",'
      '                "EndTimeUTC": "2014-05-24T10:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 665.2'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T10:00:00",'
      '                "EndTimeUTC": "2014-05-24T10:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 622.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T10:15:00",'
      '                "EndTimeUTC": "2014-05-24T10:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 773.5'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T10:30:00",'
      '                "EndTimeUTC": "2014-05-24T10:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 766'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T10:45:00",'
      '                "EndTimeUTC": "2014-05-24T11:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 844'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T11:00:00",'
      '                "EndTimeUTC": "2014-05-24T11:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 826.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T11:15:00",'
      '                "EndTimeUTC": "2014-05-24T11:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 703.7'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T11:30:00",'
      '                "EndTimeUTC": "2014-05-24T11:45:00",'
      '                "RepresentedCapacity": 0,'
      '               "ValueMWh": 1059.4'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T11:45:00",'
      '                "EndTimeUTC": "2014-05-24T12:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 711.8'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T12:00:00",'
      '                "EndTimeUTC": "2014-05-24T12:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 938.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T12:15:00",'
      '                "EndTimeUTC": "2014-05-24T12:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1096.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T12:30:00",'
      '                "EndTimeUTC": "2014-05-24T12:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 977'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T12:45:00",'
      '                "EndTimeUTC": "2014-05-24T13:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 930.6'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T13:00:00",'
      '                "EndTimeUTC": "2014-05-24T13:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 983.5'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T13:15:00",'
      '                "EndTimeUTC": "2014-05-24T13:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1290.1'
      '           },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T13:30:00",'
      '                "EndTimeUTC": "2014-05-24T13:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1271.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T13:45:00",'
      '                "EndTimeUTC": "2014-05-24T14:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1168.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T14:00:00",'
      '                "EndTimeUTC": "2014-05-24T14:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1119.4'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T14:15:00",'
      '                "EndTimeUTC": "2014-05-24T14:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1286.8'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T14:30:00",'
      '                "EndTimeUTC": "2014-05-24T14:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1485.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T14:45:00",'
      '                "EndTimeUTC": "2014-05-24T15:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1654.1'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T15:00:00",'
      '                "EndTimeUTC": "2014-05-24T15:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1723.1'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T15:15:00",'
      '                "EndTimeUTC": "2014-05-24T15:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1844.8'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T15:30:00",'
      '                "EndTimeUTC": "2014-05-24T15:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1516.2'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T15:45:00",'
      '                "EndTimeUTC": "2014-05-24T16:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1729.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T16:00:00",'
      '                "EndTimeUTC": "2014-05-24T16:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1800.6'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T16:15:00",'
      '                "EndTimeUTC": "2014-05-24T16:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 2020.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T16:30:00",'
      '                "EndTimeUTC": "2014-05-24T16:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1871.8'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T16:45:00",'
      '                "EndTimeUTC": "2014-05-24T17:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1495.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T17:00:00",'
      '                "EndTimeUTC": "2014-05-24T17:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1365.2'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T17:15:00",'
      '                "EndTimeUTC": "2014-05-24T17:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 994.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T17:30:00",'
      '                "EndTimeUTC": "2014-05-24T17:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1126.4'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T17:45:00",'
      '                "EndTimeUTC": "2014-05-24T18:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 1053.4'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T18:00:00",'
      '                "EndTimeUTC": "2014-05-24T18:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 962.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T18:15:00",'
      '                "EndTimeUTC": "2014-05-24T18:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 636.4'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T18:30:00",'
      '                "EndTimeUTC": "2014-05-24T18:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 404.6'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T18:45:00",'
      '                "EndTimeUTC": "2014-05-24T19:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 352.4'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T19:00:00",'
      '                "EndTimeUTC": "2014-05-24T19:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 378.7'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T19:15:00",'
      '                "EndTimeUTC": "2014-05-24T19:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 363'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T19:30:00",'
      '                "EndTimeUTC": "2014-05-24T19:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 418.4'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T19:45:00",'
      '                "EndTimeUTC": "2014-05-24T20:00:00",'
      '                "RepresentedCapacity": 0,'
      '               "ValueMWh": 215.8'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T20:00:00",'
      '                "EndTimeUTC": "2014-05-24T20:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 295'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T20:15:00",'
      '                "EndTimeUTC": "2014-05-24T20:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 325'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T20:30:00",'
      '                "EndTimeUTC": "2014-05-24T20:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 292.2'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T20:45:00",'
      '                "EndTimeUTC": "2014-05-24T21:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 422.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T21:00:00",'
      '                "EndTimeUTC": "2014-05-24T21:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 281'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T21:15:00",'
      '                "EndTimeUTC": "2014-05-24T21:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 236.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T21:30:00",'
      '                "EndTimeUTC": "2014-05-24T21:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 263.6'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T21:45:00",'
      '                "EndTimeUTC": "2014-05-24T22:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 304.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T22:00:00",'
      '                "EndTimeUTC": "2014-05-24T22:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 371.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T22:15:00",'
      '                "EndTimeUTC": "2014-05-24T22:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 382.7'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T22:30:00",'
      '                "EndTimeUTC": "2014-05-24T22:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 476.2'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T22:45:00",'
      '                "EndTimeUTC": "2014-05-24T23:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 363.5'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T23:00:00",'
      '                "EndTimeUTC": "2014-05-24T23:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 305.6'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T23:15:00",'
      '                "EndTimeUTC": "2014-05-24T23:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 303.8'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T23:30:00",'
      '                "EndTimeUTC": "2014-05-24T23:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 285.1'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-24T23:45:00",'
      '                "EndTimeUTC": "2014-05-25T00:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 448.4'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T00:00:00",'
      '                "EndTimeUTC": "2014-05-25T00:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 589.1'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T00:15:00",'
      '                "EndTimeUTC": "2014-05-25T00:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 543.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T00:30:00",'
      '                "EndTimeUTC": "2014-05-25T00:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 556.7'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T00:45:00",'
      '                "EndTimeUTC": "2014-05-25T01:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 452.3'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T01:00:00",'
      '                "EndTimeUTC": "2014-05-25T01:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 564.7'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T01:15:00",'
      '                "EndTimeUTC": "2014-05-25T01:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 553.5'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T01:30:00",'
      '                "EndTimeUTC": "2014-05-25T01:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 451.6'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T01:45:00",'
      '                "EndTimeUTC": "2014-05-25T02:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 325.1'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T02:00:00",'
      '                "EndTimeUTC": "2014-05-25T02:15:00",'
      '                "RepresentedCapacity": 0,'
      '               "ValueMWh": 264.2'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T02:15:00",'
      '                "EndTimeUTC": "2014-05-25T02:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 434.5'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T02:30:00",'
      '                "EndTimeUTC": "2014-05-25T02:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 498.7'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T02:45:00",'
      '                "EndTimeUTC": "2014-05-25T03:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 404.2'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T03:00:00",'
      '                "EndTimeUTC": "2014-05-25T03:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 485.5'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T03:15:00",'
      '                "EndTimeUTC": "2014-05-25T03:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 526'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T03:30:00",'
      '                "EndTimeUTC": "2014-05-25T03:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 724.4'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T03:45:00",'
      '                "EndTimeUTC": "2014-05-25T04:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 571.6'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T04:00:00",'
      '                "EndTimeUTC": "2014-05-25T04:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 575.8'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T04:15:00",'
      '                "EndTimeUTC": "2014-05-25T04:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 468'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T04:30:00",'
      '                "EndTimeUTC": "2014-05-25T04:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 500.5'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T04:45:00",'
      '                "EndTimeUTC": "2014-05-25T05:00:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 335.9'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T05:00:00",'
      '                "EndTimeUTC": "2014-05-25T05:15:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 303.1'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T05:15:00",'
      '                "EndTimeUTC": "2014-05-25T05:30:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 356.5'
      '            },'
      '            {'
      '                "StartTimeUTC": "2014-05-25T05:30:00",'
      '                "EndTimeUTC": "2014-05-25T05:45:00",'
      '                "RepresentedCapacity": 0,'
      '                "ValueMWh": 287.2'
      '            }'
      '        ]'
      '    }'
      ']')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object ButtonJSONToList: TButton
    Left = 431
    Top = 39
    Width = 164
    Height = 25
    Caption = 'JSON -> List'
    TabOrder = 2
    OnClick = ButtonJSONToListClick
  end
  object ButtonParseToObjectSpeed: TButton
    Left = 432
    Top = 403
    Width = 163
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Speedtest: Parse JSON to obj'
    TabOrder = 3
    OnClick = ButtonParseToObjectSpeedClick
  end
  object ButtonParseSpeed: TButton
    Left = 433
    Top = 372
    Width = 163
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Speedtest: Parse JSON'
    TabOrder = 4
    OnClick = ButtonParseSpeedClick
  end
  object StringGrid1: TStringGrid
    Left = 601
    Top = 136
    Width = 447
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 4
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goThumbTracking]
    TabOrder = 5
    ColWidths = (
      115
      110
      64
      72)
  end
  object ButtonJSONToObject: TButton
    Left = 432
    Top = 8
    Width = 163
    Height = 25
    Caption = 'JSON -> Obj'
    TabOrder = 6
    OnClick = ButtonJSONToObjectClick
  end
  object ListBox1: TListBox
    Left = 601
    Top = 8
    Width = 366
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 7
    OnClick = ListBox1Click
  end
  object ButtonClear: TButton
    Left = 973
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Clear'
    TabOrder = 8
    OnClick = ButtonClearClick
  end
  object ButtonGet: TButton
    Left = 8
    Top = 623
    Width = 66
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Get'
    TabOrder = 9
    OnClick = ButtonGetClick
  end
  object EditURL: TEdit
    Left = 80
    Top = 627
    Width = 345
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 10
    Text = 
      'http://api.neasenergy.com/v1/productiongroups/102/onlinedata?sta' +
      'rt=2014-07-20T22:00:00Z&end=2014-07-21T22:00:00Z&output=json'
  end
  object ButtonURLToObject: TButton
    Left = 431
    Top = 70
    Width = 164
    Height = 25
    Caption = 'URL -> List'
    TabOrder = 11
    OnClick = ButtonURLToObjectClick
  end
  object Button1: TButton
    Left = 449
    Top = 480
    Width = 75
    Height = 25
    Caption = 'DTO'
    TabOrder = 12
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Simple'
    TabOrder = 13
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'iTunes'
    TabOrder = 14
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 184
    Top = 8
    Width = 75
    Height = 25
    Caption = 'lyrics.wikia.com'
    TabOrder = 15
    OnClick = Button4Click
  end
end
