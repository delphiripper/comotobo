object WizardForm: TWizardForm
  Left = 0
  Top = 0
  Caption = 'Import JSON DTO'
  ClientHeight = 432
  ClientWidth = 1005
  Color = 14737632
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1005
    432)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 386
    Width = 1005
    Height = 46
    Align = alBottom
    Brush.Color = 16767954
    Pen.Color = 5329233
  end
  object LabelInvalidJSON: TLabel
    Left = 97
    Top = 404
    Width = 61
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Invalid JSON'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object Label1: TLabel
    Left = 512
    Top = 359
    Width = 58
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Class name:'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 26
    Height = 13
    Caption = 'JSON'
  end
  object Label3: TLabel
    Left = 512
    Top = 40
    Width = 56
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'DTO source'
  end
  object ButtonImport: TButton
    Left = 11
    Top = 397
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Import'
    Enabled = False
    TabOrder = 5
    OnClick = ButtonImportClick
  end
  object ButtonCancel: TButton
    Left = 919
    Top = 397
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = ButtonCancelClick
  end
  object EditURL: TEdit
    Left = 8
    Top = 11
    Width = 907
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 14
    Font.Name = 'Consolas'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    Text = 'http://itunes.apple.com/search?term=metallica'
    OnKeyDown = EditURLKeyDown
    OnKeyPress = EditURLKeyPress
    OnKeyUp = EditURLKeyUp
  end
  object ButtonGet: TButton
    Left = 922
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Get'
    TabOrder = 1
    OnClick = ButtonGetClick
  end
  object EditName: TEdit
    Left = 576
    Top = 357
    Width = 169
    Height = 20
    Anchors = [akRight, akBottom]
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 14
    Font.Name = 'Consolas'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 4
    Text = 'NewDTO'
    OnChange = EditNameChange
  end
  object PanelJSON: TPanel
    Left = 8
    Top = 58
    Width = 497
    Height = 290
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'PanelJSON'
    Ctl3D = False
    ParentCtl3D = False
    ShowCaption = False
    TabOrder = 2
    object MemoJSON: TMemo
      Left = 0
      Top = 0
      Width = 495
      Height = 288
      Align = alClient
      BevelInner = bvLowered
      BevelOuter = bvRaised
      BorderStyle = bsNone
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 14
      Font.Name = 'Consolas'
      Font.Style = []
      Lines.Strings = (
        '['
        '  {'
        '    "Name": "Thomas",'
        '    "ID": 1'
        '  },'
        '  {'
        '    "Name": "Maria",'
        '    "ID": 2'
        '  }'
        ']')
      ParentCtl3D = False
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      OnChange = MemoJSONChange
    end
  end
  object PanelSource: TPanel
    Left = 511
    Top = 58
    Width = 486
    Height = 290
    Anchors = [akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Panel1'
    Ctl3D = False
    ParentCtl3D = False
    ShowCaption = False
    TabOrder = 3
    object MemoSource: TMemo
      Left = 0
      Top = 0
      Width = 484
      Height = 288
      Align = alClient
      BevelInner = bvLowered
      BevelOuter = bvRaised
      BorderStyle = bsNone
      Color = 14737632
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 14
      Font.Name = 'Consolas'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 208
    Top = 72
  end
end
