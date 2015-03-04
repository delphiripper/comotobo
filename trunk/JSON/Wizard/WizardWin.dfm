object WizardForm: TWizardForm
  Left = 0
  Top = 0
  Caption = 'Import JSON DTO'
  ClientHeight = 432
  ClientWidth = 626
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
    626
    432)
  PixelsPerInch = 96
  TextHeight = 13
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
  object Shape1: TShape
    Left = 0
    Top = 386
    Width = 626
    Height = 46
    Align = alBottom
    Brush.Color = 16767954
    Pen.Color = 5329233
  end
  object Label1: TLabel
    Left = 8
    Top = 356
    Width = 58
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Class name:'
  end
  object ButtonImport: TButton
    Left = 11
    Top = 397
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Import'
    Enabled = False
    TabOrder = 0
    OnClick = ButtonImportClick
  end
  object ButtonCancel: TButton
    Left = 540
    Top = 397
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object EditURL: TEdit
    Left = 8
    Top = 11
    Width = 528
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
    TabOrder = 2
    Text = 'http://itunes.apple.com/search?term=metallica'
  end
  object Button3: TButton
    Left = 543
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Get'
    TabOrder = 3
    OnClick = Button3Click
  end
  object EditName: TEdit
    Left = 72
    Top = 354
    Width = 169
    Height = 20
    Anchors = [akLeft, akBottom]
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
  end
  object Panel1: TPanel
    Left = 8
    Top = 37
    Width = 610
    Height = 304
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Panel1'
    Ctl3D = False
    ParentCtl3D = False
    ShowCaption = False
    TabOrder = 5
    object MemoJSON: TMemo
      Left = 0
      Top = 0
      Width = 608
      Height = 302
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
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 208
    Top = 64
  end
end
