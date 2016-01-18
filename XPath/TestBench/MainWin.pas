unit MainWin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Grids, HtmlParser, XPath;

type
  TForm1 = class(TForm)
    ButtonDownload: TButton;
    ButtonParseLastDownload: TButton;
    ButtonXPath: TButton;
    EditXPath: TEdit;
    EditURL: TEdit;
    PanelContent: TPanel;
    PageControlRight: TPageControl;
    TabDom: TTabSheet;
    TabXPath: TTabSheet;
    Splitter1: TSplitter;
    PageControlLeft: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ButtonParseMemo: TButton;
    MemoHtml: TMemo;
    TreeViewDOM: TTreeView;
    TreeViewXPath: TTreeView;
    StatusBar: TStatusBar;
    ButtonEpexTest: TButton;
    Grid: TStringGrid;
    Button1: TButton;
    ButtonExaaTest: TButton;
    CheckBoxTrimTags: TCheckBox;
    procedure ButtonDownloadClick(Sender: TObject);
    procedure ButtonParseLastDownloadClick(Sender: TObject);
    procedure ButtonParseMemoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonXPathClick(Sender: TObject);
    procedure ButtonEpexTestClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ButtonExaaTestClick(Sender: TObject);
  private
    FDom: TDom;
    FNodeList: TNodeList;
    FLastParse: Int64;
    FLastXPath: Int64;
    FXPathResult: TXValue;
    Procedure MakeTree( ATreeView: TTreeView; AParent: TTreeNode; ANode: TNode );
    procedure ShowDom( ATreeView: TTreeView; ANode: TNode );
    procedure ShowXPathTree;
    procedure ShowXPathTable;
    procedure DownloadURL( AURL: String );
    procedure RunXPath( AXPath: String );
    procedure UpdateStatusBar;
    procedure ParseDOM( Fil: TStrings; ARootName: String );
    procedure ParseLastDownloadClick;
  end;

var
  Form1: TForm1;

implementation

Uses
  IdHttp, System.Math, System.Diagnostics, VCL.clipbrd;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Text: String;
  Row: Integer;
begin
  Text := '';
  for Row := 0 to Grid.RowCount-1 do
    Text := Text + Grid.Rows[Row].CommaText + #13#10;
  Clipboard.AsText := Text;
end;

procedure TForm1.ButtonExaaTestClick(Sender: TObject);
begin
  EditURL.Text   := 'http://www.exaa.at/de/marktdaten/handelsergebnisse';
  EditXPath.Text := '//div[@class=''pimcore_area_cismo_wysiwyg hourProducts'']/table//tr';
  DownloadURL( EditURL.Text );
  RunXPath( EditXPath.Text );
end;

procedure TForm1.ButtonDownloadClick(Sender: TObject);
Begin
  DownloadURL( Trim(EditURL.Text) );
End;

procedure TForm1.DownloadURL( AURL: String );
var
  Http      : TIdHttp;
  Stream    : TStringStream;
  Fil       : TStringList;
begin
  Http      := TIdHttp.Create(nil);
  Stream    := TStringStream.Create( '', TEncoding.UTF8 );
  Fil       := TStringList.Create;
  Try
    Http.Get( AURL, Stream );
    if Http.ResponseCode = 200 then
    Begin
      Stream.Position := 0;
      Fil.Text := Stream.DataString; //UTF8 --> Unicode
      Fil.SaveToFile( 'Download.html' );
      ParseDOM( Fil, AURL );
    End;
  Finally
    Fil.Free;
    Stream.Free;
    Http.Free;
  End;
end;

procedure TForm1.ButtonEpexTestClick(Sender: TObject);
begin
  EditURL.Text   := 'http://www.epexspot.com/en/market-data/dayaheadauction/auction-table/2015-08-23/DE';
  EditXPath.Text := '//div[@id=''tab_de'']/table[3]/tbody/tr[ not(td[ text()=''MWh'' ] ) ]';
  DownloadURL( EditURL.Text );
  RunXPath( EditXPath.Text );
end;

procedure TForm1.ButtonParseLastDownloadClick(Sender: TObject);
Begin
  ParseLastDownloadClick;
End;

procedure TForm1.ParseLastDownloadClick;
var
  Fil: TStringList;
begin
  if FileExists( 'Download.html' ) then
  Begin
    Fil := TStringList.Create;
    Try
      Fil.LoadFromFile( 'Download.html' );
      ParseDOM( Fil, 'Download.html' );
    Finally
      Fil.Free;
    End;
  End;
end;

procedure TForm1.ButtonParseMemoClick(Sender: TObject);
begin
  ParseDOM( MemoHtml.Lines, 'Memo' );
end;

procedure TForm1.ButtonXPathClick(Sender: TObject);
Begin
  RunXPath( Trim(EditXPath.Text) );
End;

procedure TForm1.ShowXPathTable;

  Function TrimHtmlTags( S: String ): String;
  var
    Start, EndTag: Integer;
  Begin
    Result := S;
    Start := Pos( '<', S );
    EndTag := Pos( '>', S );
    while (Start > 0) and (EndTag > 0) and (EndTag > Start) do
    Begin
      S := Copy( S, 1, Start-1 ) + Copy( S, EndTag+1, MaxInt );
      Start  := Pos( '<', S );
      EndTag := Pos( '>', S );
    End;
    Result := S;
  End;

  Function TrimCellText( S: String ): String;
  Begin
    Result := S;
    if CheckBoxTrimTags.Checked then
      Result := TrimHtmlTags( Result );
    Result := Trim( Result );
  End;

var
  ColCount, Col, Row: Integer;
  Node, Child: TNode;
begin
  if FXPathResult.XType <> xNodelist then
  Begin
    Grid.ColCount := 1;
    Grid.RowCount := 2;
    Grid.Rows[0].Text := 'Value';
    case FXPathResult.XType of
      xBool   : Grid.Rows[1].Text := BoolToStr(  FXPathResult.AsBoolean, True );
      xInt    : Grid.Rows[1].Text := IntToStr(   FXPathResult.AsInt );
      xFloat  : Grid.Rows[1].Text := FloatToStr( FXPathResult.AsFloat );
      xString : Grid.Rows[1].Text := FXPathResult.AsString;
    end;
  End
  else
  Begin
    Grid.RowCount := 1 + Max( 1, FNodeList.Count );
    ColCount := 0;
    For Node in FNodeList do
      ColCount := Max( ColCount, Node.Count );
    Grid.ColCount := Max( 1, ColCount );
    Grid.Rows[0].Text := '';
    Grid.Rows[1].Text := '';

    for Col := 0 to Grid.ColCount-1 do
      Grid.Cells[ Col, 0 ] := 'Col ' + Col.ToString;
    Row := 1;
    For Node in FNodeList do
    Begin
      Col := 0;
      if ColCount = 0 then
        Grid.Cells[ Col, Row ] := TrimCellText( Node.Text )
      Else
      Begin
        Grid.Rows[ Row ].Text := '';
        For Child in Node do
        Begin
          Grid.Cells[ Col, Row ] := TrimCellText( Child.Text );
          Inc( Col );
        End;
      End;
      Inc( Row );
    End;
  End;
end;

procedure TForm1.ShowXPathTree;
var
  Node: TNode;
begin
  TreeViewXPath.Items.BeginUpdate;
  Try
    TreeViewXPath.Items.Clear;
    For Node in FNodeList do
      MakeTree( TreeViewXPath, nil, Node );
  Finally
    TreeViewXPath.Items.EndUpdate;
  End;
  UpdateStatusBar;
end;

procedure TForm1.RunXPath( AXPath: String );
var
  StopWatch : TStopWatch;
begin
  FNodeList.Clear;
  StopWatch := TStopWatch.StartNew;
  if AXPath <> '' then
  Begin
    FXPathResult := TXPath.Eval( FDom, AXPath );
    if FXPathResult.XType = xNodelist then
      FNodeList.AddRange( FXPathResult.AsNodes );
  End;
  FLastXPath := StopWatch.ElapsedMilliseconds;
  ShowXPathTree;
  ShowXPathTable;
  PageControlLeft.ActivePageIndex := 1;
  PageControlRight.ActivePageIndex := 1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FDom := TDom.Create;
  FDom.StrictMode := False;
  FLastParse := 0;
  FLastXPath := 0;
  FNodeList := TNodeList.Create( False );
  PageControlLeft.ActivePageIndex := 0;
  PageControlRight.ActivePageIndex := 0;
  ParseLastDownloadClick;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDom.Free;
  FNodeList.Free;
end;

Procedure TForm1.MakeTree( ATreeView: TTreeView; AParent: TTreeNode; ANode: TNode );
var
  Item: TTreeNode;
  Child: TNode;
  Text, Value, AttributeText: String;
  Attribute: TAttribute;
Begin
  AttributeText := '';
  if ANode.Attributes.Count > 0 then
  Begin
    for Attribute in ANode.Attributes do
    begin
      if AttributeText <> '' then
        AttributeText := AttributeText + ', ';
      AttributeText := AttributeText + ' @' + Attribute.Name + ' = "' + Attribute.Value + '"';
    end;
    AttributeText := '  [' + AttributeText + ' ]';
  End;
  Text := Trim( ANode.Name ) + AttributeText;
  Value := Trim( ANode.Text );
  if (Value <> '') and (Value.Length < 50) then
    Text := Text + ' :: ' + Value;

  Item := ATreeView.Items.AddChild( AParent, Text );
  for Child in ANode do
    MakeTree( ATreeView, Item, Child );
  Item.Expanded := True;
End;

procedure TForm1.ParseDOM(Fil: TStrings; ARootName: String);
var
  StopWatch : TStopWatch;
begin
  StopWatch := TStopWatch.StartNew;
  FDom.LoadFromStrings( Fil );
  FLastParse := StopWatch.ElapsedMilliseconds;

  FDom.Root.Name := ARootName;
  ShowDom( TreeViewDOM, FDom.Root );
  PageControlLeft.ActivePageIndex := 0;
  UpdateStatusBar;
end;

procedure TForm1.ShowDom(ATreeView: TTreeView; ANode: TNode);
begin
  ATreeView.Items.BeginUpdate;
  Try
    ATreeView.Items.Clear;
    MakeTree( TreeViewDOM, nil, ANode );
  Finally
    ATreeView.Items.EndUpdate;
  End;
end;

procedure TForm1.UpdateStatusBar;
begin
  StatusBar.SimpleText := 'Last parse: ' + FLastParse.ToString + ' ms' +
                          '                      ' +
                          'Last XPath: ' + FLastXPath.ToString + ' ms';
end;

end.

