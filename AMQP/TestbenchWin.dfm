object TestbenchForm: TTestbenchForm
  Left = 0
  Top = 0
  Caption = 'AMQP Testbench'
  ClientHeight = 788
  ClientWidth = 1218
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
    1218
    788)
  PixelsPerInch = 96
  TextHeight = 13
  object MemoMessages: TMemo
    Left = 8
    Top = 596
    Width = 897
    Height = 184
    Anchors = [akLeft, akRight, akBottom]
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = 18
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ButtonConnect: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 0
    OnClick = ButtonConnectClick
  end
  object MemoSent: TMemo
    Left = 8
    Top = 71
    Width = 401
    Height = 520
    Anchors = [akLeft, akTop, akBottom]
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object MemoReceived: TMemo
    Left = 617
    Top = 71
    Width = 401
    Height = 520
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object MemoSentBytes: TMemo
    Left = 415
    Top = 70
    Width = 196
    Height = 520
    Anchors = [akLeft, akTop, akBottom]
    Color = clGreen
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = 16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object MemoReceivedBytes: TMemo
    Left = 1024
    Top = 71
    Width = 186
    Height = 520
    Anchors = [akTop, akRight, akBottom]
    Color = clGreen
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = 16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object ButtonDisconnect: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 6
    OnClick = ButtonDisconnectClick
  end
  object ButtonPublishRed: TButton
    Left = 600
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Publish Red'
    TabOrder = 7
    OnClick = ButtonPublishRedClick
  end
  object ButtonOpenChannel: TButton
    Left = 95
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Open Channel'
    TabOrder = 8
    OnClick = ButtonOpenChannelClick
  end
  object ButtonCloseChannel: TButton
    Left = 95
    Top = 40
    Width = 90
    Height = 25
    Caption = 'Close Channel'
    TabOrder = 9
    OnClick = ButtonCloseChannelClick
  end
  object ButtonExchangeDeclare: TButton
    Left = 199
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Declare Exchange'
    TabOrder = 10
    OnClick = ButtonExchangeDeclareClick
  end
  object ButtonExchangeDelete: TButton
    Left = 199
    Top = 40
    Width = 106
    Height = 25
    Caption = 'Delete Exchange'
    TabOrder = 11
    OnClick = ButtonExchangeDeleteClick
  end
  object ButtonQueueDeclare: TButton
    Left = 322
    Top = 8
    Width = 87
    Height = 25
    Caption = 'Declare Queue'
    TabOrder = 12
    OnClick = ButtonQueueDeclareClick
  end
  object ButtonQueueDelete: TButton
    Left = 322
    Top = 39
    Width = 87
    Height = 25
    Caption = 'Delete Queue'
    TabOrder = 13
    OnClick = ButtonQueueDeleteClick
  end
  object ButtonQueueBind: TButton
    Left = 415
    Top = 8
    Width = 82
    Height = 25
    Caption = 'Bind Queue'
    TabOrder = 14
    OnClick = ButtonQueueBindClick
  end
  object ButtonQueueUnbind: TButton
    Left = 415
    Top = 39
    Width = 82
    Height = 25
    Caption = 'Unbind queue'
    TabOrder = 15
    OnClick = ButtonQueueUnbindClick
  end
  object ButtonPublishBlue: TButton
    Left = 600
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Publish Blue'
    TabOrder = 16
    OnClick = ButtonPublishBlueClick
  end
  object ButtonGetRed: TButton
    Left = 696
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Get red'
    TabOrder = 17
    OnClick = ButtonGetRedClick
  end
  object ButtonGetBlue: TButton
    Left = 696
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Get Blue'
    TabOrder = 18
    OnClick = ButtonGetBlueClick
  end
  object ButtonConfirmSelect: TButton
    Left = 864
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Confirm Select'
    TabOrder = 19
    OnClick = ButtonConfirmSelectClick
  end
  object ButtonConsumeBlue: TButton
    Left = 976
    Top = 8
    Width = 91
    Height = 25
    Caption = 'Consume Blue'
    TabOrder = 20
    OnClick = ButtonConsumeBlueClick
  end
  object Panel1: TPanel
    Left = 911
    Top = 597
    Width = 299
    Height = 183
    Anchors = [akRight, akBottom]
    BorderStyle = bsSingle
    Caption = 'Panel1'
    Ctl3D = False
    ParentCtl3D = False
    ShowCaption = False
    TabOrder = 21
    object LabelStatus: TLabel
      Left = 8
      Top = 8
      Width = 56
      Height = 13
      Caption = 'LabelStatus'
    end
  end
  object ButtonPurgeRed: TButton
    Left = 503
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Purge Red'
    TabOrder = 22
    OnClick = ButtonPurgeRedClick
  end
  object ButtonCancelBlue: TButton
    Left = 976
    Top = 40
    Width = 91
    Height = 25
    Caption = 'Cancel Blue'
    TabOrder = 23
    OnClick = ButtonCancelBlueClick
  end
  object ButtonReject: TButton
    Left = 777
    Top = 9
    Width = 75
    Height = 25
    Caption = 'Reject red'
    TabOrder = 24
    OnClick = ButtonRejectClick
  end
  object ButtonThreadConsume: TButton
    Left = 1073
    Top = 8
    Width = 96
    Height = 25
    Caption = 'Consume Thread'
    TabOrder = 25
    OnClick = ButtonThreadConsumeClick
  end
  object ButtonCancelRed: TButton
    Left = 1073
    Top = 40
    Width = 96
    Height = 25
    Caption = 'Cancel Red'
    TabOrder = 26
    OnClick = ButtonCancelRedClick
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 1167
    Top = 613
  end
end
