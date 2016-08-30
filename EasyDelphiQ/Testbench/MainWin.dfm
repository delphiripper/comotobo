object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'QTestbench'
  ClientHeight = 337
  ClientWidth = 635
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
    635
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonPublish: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Publish'
    TabOrder = 0
    OnClick = ButtonPublishClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 64
    Width = 619
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ButtonGet: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Get'
    TabOrder = 2
    OnClick = ButtonGetClick
  end
  object ButtonSubscribe: TButton
    Left = 170
    Top = 8
    Width = 115
    Height = 25
    Caption = 'Subscribe TestDTO'
    TabOrder = 3
    OnClick = ButtonSubscribeClick
  end
  object ButtonCancelSubscription: TButton
    Left = 291
    Top = 8
    Width = 118
    Height = 25
    Caption = 'Cancel subscription'
    TabOrder = 4
    OnClick = ButtonCancelSubscriptionClick
  end
  object SubscribeTimeseries: TButton
    Left = 170
    Top = 33
    Width = 115
    Height = 25
    Caption = 'Subscribe Timeseries'
    TabOrder = 5
    OnClick = SubscribeTimeseriesClick
  end
end
