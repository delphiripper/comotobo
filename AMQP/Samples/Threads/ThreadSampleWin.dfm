object ThreadSampleForm: TThreadSampleForm
  Left = 0
  Top = 0
  Caption = 'Thread sample'
  ClientHeight = 441
  ClientWidth = 789
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    789
    441)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 96
    Top = 8
    Width = 72
    Height = 13
    Caption = 'Message count'
  end
  object Label2: TLabel
    Left = 192
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Interval'
  end
  object MemoConsumer2: TMemo
    Left = 537
    Top = 72
    Width = 244
    Height = 361
    Anchors = [akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object MemoProducer: TMemo
    Left = 8
    Top = 72
    Width = 244
    Height = 361
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object ButtonStartConsumer2: TButton
    Left = 537
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Consume'
    TabOrder = 2
    OnClick = ButtonStartConsumer2Click
  end
  object ButtonStartProducer: TButton
    Left = 8
    Top = 9
    Width = 75
    Height = 25
    Caption = 'Produce'
    TabOrder = 3
    OnClick = ButtonStartProducerClick
  end
  object ButtonStopProducer: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 4
    OnClick = ButtonStopProducerClick
  end
  object ButtonStopConsumer2: TButton
    Left = 537
    Top = 41
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Stop'
    TabOrder = 5
    OnClick = ButtonStopConsumer2Click
  end
  object SpinEditCount: TSpinEdit
    Left = 97
    Top = 28
    Width = 64
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 6
    Value = 100
  end
  object SpinEditInterval: TSpinEdit
    Left = 188
    Top = 28
    Width = 64
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 100
  end
  object MemoConsumer1: TMemo
    Left = 287
    Top = 72
    Width = 244
    Height = 361
    Anchors = [akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 8
  end
  object ButtonStartConsumer1: TButton
    Left = 287
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Consume'
    TabOrder = 9
    OnClick = ButtonStartConsumer1Click
  end
  object ButtonStopConsumer1: TButton
    Left = 287
    Top = 41
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Stop'
    TabOrder = 10
    OnClick = ButtonStopConsumer1Click
  end
end
