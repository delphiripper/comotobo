object Form1: TForm1
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Form1'
  ClientHeight = 604
  ClientWidth = 1087
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1087
    604)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonDownload: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Download'
    TabOrder = 0
    OnClick = ButtonDownloadClick
  end
  object ButtonParseLastDownload: TButton
    Left = 950
    Top = 8
    Width = 129
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Parse last download'
    TabOrder = 4
    OnClick = ButtonParseLastDownloadClick
  end
  object ButtonXPath: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'XPath'
    Default = True
    TabOrder = 2
    OnClick = ButtonXPathClick
  end
  object EditXPath: TEdit
    Left = 89
    Top = 41
    Width = 720
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 
      '//div[@id='#39'tab_de'#39']/table[3]/tbody/tr[ not(td[ text()='#39'MWh'#39' ] ) ' +
      ']'
  end
  object EditURL: TEdit
    Left = 89
    Top = 9
    Width = 720
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 
      'http://www.epexspot.com/en/market-data/dayaheadauction/auction-t' +
      'able/2015-08-23/DE'
  end
  object PanelContent: TPanel
    Left = 8
    Top = 70
    Width = 1071
    Height = 509
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'PanelContent'
    ShowCaption = False
    TabOrder = 7
    object Splitter1: TSplitter
      Left = 441
      Top = 1
      Width = 8
      Height = 507
      Beveled = True
      ResizeStyle = rsUpdate
      ExplicitLeft = 290
      ExplicitHeight = 383
    end
    object PageControlRight: TPageControl
      Left = 449
      Top = 1
      Width = 621
      Height = 507
      ActivePage = TabXPath
      Align = alClient
      Constraints.MinWidth = 200
      TabOrder = 0
      object TabDom: TTabSheet
        Caption = 'HTML input'
        Constraints.MinWidth = 200
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          613
          479)
        object ButtonParseMemo: TButton
          Left = 3
          Top = 3
          Width = 94
          Height = 25
          Caption = 'Parse memo'
          TabOrder = 0
          OnClick = ButtonParseMemoClick
        end
        object MemoHtml: TMemo
          Left = 3
          Top = 34
          Width = 607
          Height = 442
          Anchors = [akLeft, akTop, akRight, akBottom]
          Lines.Strings = (
            ''
            '<!DOCTYPE html public >'
            ''
            '<!-- comment -->'
            ''
            '<![CDATA[ stuff ]]>'
            ''
            ''
            '<html>'
            '  Hello<a>world</a>'
            '  <br>'
            '  I am groot'
            '</html>'
            ''
            '<script>'
            '  Hello'
            '</script>')
          TabOrder = 1
        end
      end
      object TabXPath: TTabSheet
        Caption = 'XPath Result'
        ImageIndex = 1
        DesignSize = (
          613
          479)
        object Grid: TStringGrid
          Left = 3
          Top = 34
          Width = 607
          Height = 442
          Anchors = [akLeft, akTop, akRight, akBottom]
          DefaultRowHeight = 18
          DrawingStyle = gdsClassic
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goThumbTracking]
          TabOrder = 0
        end
        object Button1: TButton
          Left = 3
          Top = 3
          Width = 94
          Height = 25
          Caption = 'Copy'
          TabOrder = 1
          OnClick = Button1Click
        end
        object CheckBoxTrimTags: TCheckBox
          Left = 112
          Top = 8
          Width = 97
          Height = 17
          Caption = 'Trim HTML tags'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
      end
    end
    object PageControlLeft: TPageControl
      Left = 1
      Top = 1
      Width = 440
      Height = 507
      ActivePage = TabSheet1
      Align = alLeft
      Constraints.MinWidth = 200
      TabOrder = 1
      object TabSheet1: TTabSheet
        Caption = 'DOM'
        object TreeViewDOM: TTreeView
          Left = 0
          Top = 0
          Width = 432
          Height = 479
          Align = alClient
          Indent = 19
          TabOrder = 0
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'XPath Result'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object TreeViewXPath: TTreeView
          Left = 0
          Top = 0
          Width = 432
          Height = 479
          Align = alClient
          Constraints.MinWidth = 200
          Indent = 19
          TabOrder = 0
        end
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 585
    Width = 1087
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object ButtonEpexTest: TButton
    Left = 950
    Top = 39
    Width = 129
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Epex test'
    TabOrder = 6
    OnClick = ButtonEpexTestClick
  end
  object ButtonExaaTest: TButton
    Left = 815
    Top = 39
    Width = 129
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'EXAA test'
    TabOrder = 5
    OnClick = ButtonExaaTestClick
  end
end
