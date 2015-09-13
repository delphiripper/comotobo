unit Sample1Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ExtCtrls, System.Generics.Collections,
  System.Diagnostics, TimeseriesDTO, JSON;

type
  TStuff = Class
  strict private
    FName: String;
    FNumber: Integer;
    FIDList: TList<Integer>;
    FLines: TStringList;
  private
    FADate: TDatetime;
  Public
    Property Name: String read FName write FName;
    Property Number: Integer read FNumber write FNumber;
    Property ADate: TDatetime read FADate write FADate;
    Property IDList: TList<Integer> read FIDList;
    Property Lines: TStringList read FLines;
    Constructor Create;
    Destructor Destroy; Override;
  End;

  TEnum = ( eBobby, eThomas, eKurt );

  TOuter = Class
  strict private
    FItems: TObjectList<TStuff>;
    FKind: Integer;
    FContent: String;
  private
    FSomeBool: Boolean;
    FNameEnum: TEnum;
  Public
    Property Content: String read FContent write FContent;
    Property Kind: Integer read FKind write FKind;
    Property NameEnum: TEnum read FNameEnum write FNameEnum;
    Property SomeBool: Boolean read FSomeBool write FSomeBool;
    Property Items: TObjectList<TStuff> read FItems;
    Constructor Create;
    Destructor Destroy; Override;
  End;

  TForm1 = class(TForm)
    Memo1: TMemo;
    MemoJSON: TMemo;
    ButtonJSONToList: TButton;
    ButtonParseToObjectSpeed: TButton;
    ButtonParseSpeed: TButton;
    StringGrid1: TStringGrid;
    ButtonJSONToObject: TButton;
    Label1: TLabel;
    Label2: TLabel;
    LabelProductionGroupID: TLabel;
    LabelType: TLabel;
    ListBox1: TListBox;
    ButtonClear: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    ButtonGet: TButton;
    EditURL: TEdit;
    ButtonURLToObject: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonJSONToObjectClick(Sender: TObject);
    procedure ButtonJSONToListClick(Sender: TObject);
    procedure ButtonParseToObjectSpeedClick(Sender: TObject);
    procedure ButtonParseSpeedClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ButtonGetClick(Sender: TObject);
    procedure ButtonURLToObjectClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    List: TObjectList<TTimeseries>;
    procedure ClearList;
  public
    Procedure AddTimeseries( TimeSeries: TTimeseries );
    Procedure Show( Timeseries: TTimeseries ); Overload;
  end;

var
  Form1: TForm1;

implementation

Uses
  System.DateUtils, JSON2DTO;

{$R *.dfm}

{ TForm1 }

procedure TForm1.AddTimeseries(TimeSeries: TTimeseries);
begin
  List.Add( TimeSeries );
  ListBox1.ItemIndex := ListBox1.Items.Add( IntToStr(TimeSeries.ProductionGroupID) + ' : ' + TimeSeries.&Type );
  Show( TimeSeries );
end;

procedure TForm1.ButtonJSONToListClick(Sender: TObject);
var
  TS: TTimeseries;
  Timer: TStopwatch;
begin
  ClearList;
  Memo1.Lines.Clear;
  Timer := TStopwatch.StartNew;
  TJSONParser.Parse( MemoJSON.Text, List );  { <--- The magic! }
  Memo1.Lines.Add( 'Parsing: ' + IntToStr(Timer.ElapsedMilliseconds) + ' ms.' );
  for TS in List do
    ListBox1.Items.Add( IntToStr(TS.ProductionGroupID) + ' : ' + TS.&Type );
  ListBox1.ItemIndex := -1;
end;

procedure TForm1.ButtonGetClick(Sender: TObject);
begin
  MemoJSON.Text := TJSON.Get( EditURL.Text );
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
  TDTOGenerator.Parse( MemoJSON.Text, 'Imported', 'DTO.Imported', Memo1.Lines, '' );
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MemoJSON.Clear;
  MemoJSON.Lines.Add( '{' );
  MemoJSON.Lines.Add( '  "ID": 45,' );
  MemoJSON.Lines.Add( '  "Source":' );
  MemoJSON.Lines.Add( '  {' );
  MemoJSON.Lines.Add( '    "ID": 14,' );
  MemoJSON.Lines.Add( '    "Value": 0.0,' );
  MemoJSON.Lines.Add( '    "Name": "Jesper",' );
  MemoJSON.Lines.Add( '    "Alias": "Ripper"' );
  MemoJSON.Lines.Add( '  }' );
//  MemoJSON.Lines.Add( '  "Timeseries":' );
//  MemoJSON.Lines.Add( '  [' );
//  MemoJSON.Lines.Add( '    { "Timestamp": "2015-01-01T00:00", "Value": 45.98 },' );
//  MemoJSON.Lines.Add( '    { "Timestamp": "2015-01-01T00:15", "Value": 13.12 }' );
//  MemoJSON.Lines.Add( '  ]' );
  MemoJSON.Lines.Add( '}' );
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  JSON: TJSON_Element;
begin
  MemoJSON.Text := TJSON.Get( 'http://itunes.apple.com/search?term=metallica' );
  //Pretty print
  JSON := nil;
  Try
    JSON := TJSON.ParseText( MemoJSON.Text );
    MemoJSON.Text := JSON.ToJSON;
  Finally
    JSON.Free;
  End;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  JSON: TJSON_Element;
begin
  MemoJSON.Text := TJSON.Get( 'http://lyrics.wikia.com/api.php?artist=Metallica&fmt=json' );
  //Pretty print
  JSON := nil;
  Try
    JSON := TJSON.ParseText( MemoJSON.Text );
    MemoJSON.Text := JSON.ToJSON;
  Finally
    JSON.Free;
  End;
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  ClearList;
end;

procedure TForm1.ButtonParseToObjectSpeedClick(Sender: TObject);
Const
  Cnt = 100;
Var
  i: Integer;
  Timeseries: TTimeseries;
  Timer: TStopwatch;
begin
  Memo1.Lines.Clear;
  Timeseries := nil;
  Try
    Timer := TStopwatch.StartNew;
    for i := 1 to Cnt do
    Begin
      Timeseries.Free;
      Timeseries := TTimeseries.Create;
      TJSONParser.Parse( MemoJSON.Text, Timeseries );  { <--- The magic! }
    End;
    Memo1.Lines.Add( 'Parsing: ' + FloatToStrF(Timer.ElapsedMilliseconds/Cnt, ffFixed, 10, 3) + ' ms. (avg.)' );
    Show( Timeseries );
  Finally
    Timeseries.Free;
  End;
end;

procedure TForm1.ButtonURLToObjectClick(Sender: TObject);
var
  TS: TTimeseries;
begin
  ClearList;
  TJSON.Get( EditURL.text, List );
  for TS in List do
    ListBox1.Items.Add( IntToStr(TS.ProductionGroupID) + ' : ' + TS.&Type );
  ListBox1.ItemIndex := -1;
end;

procedure TForm1.ButtonParseSpeedClick(Sender: TObject);
Const
  Cnt = 1000;
var
  J: TJSON_Element;
  Timer: TStopwatch;
  i: Integer;
  JSON: String;
begin
  Memo1.Lines.Clear;
  JSON := MemoJSON.Text;
  J:= nil;
  Timer := TStopwatch.StartNew;
  for i := 1 to Cnt do
  Begin
    J.Free;
    J := TJSON.ParseText( JSON );
  End;
  With Memo1.Lines do
  Try
    BeginUpdate;
    Add( 'JSON Parsing: ' + FloatToStrF(Timer.ElapsedMilliseconds/Cnt, ffFixed, 10, 3) + ' ms. (avg.)' );
    If J.SelfType = jtObject then
      Add( 'Object' )
    else If (J.SelfType = jtArray) and
            (TJSON_Array(J).Count = 1) and
            (TJSON_Array(J).Elements[0].SelfType = jtObject) then
      Add( 'Object (in Array)' )
    else If (J.SelfType = jtArray) then
      Add( 'Array' );
    Add('');
    Add('Re-serialized:');
    Add( J.ToJSON );
  Finally
    EndUpdate;
    J.Free;
  End;
end;

procedure TForm1.Show(Timeseries: TTimeseries);
var
  Datapoint: TDatapoint;
  Row: Integer;
begin
  LabelProductionGroupID.Caption := IntToStr(Timeseries.ProductionGroupID);
  LabelType.Caption              := Timeseries.&Type;
  StringGrid1.RowCount := 1 + Timeseries.Datapoints.Count;
  Row := 1;
  for Datapoint in Timeseries.Datapoints do
  Begin
    StringGrid1.Cells[0,Row] := DateTimeToStr(Datapoint.StartTimeUTC);
    StringGrid1.Cells[1,Row] := DateTimeToStr(Datapoint.EndTimeUTC);
    StringGrid1.Cells[2,Row] := FloatToStr(Datapoint.RepresentedCapacity);
    StringGrid1.Cells[3,Row] := FloatToStr(Datapoint.ValueMWh);
    Inc(Row);
  End;
end;

procedure TForm1.ClearList;
begin
  List.Clear;
  ListBox1.Clear;
  StringGrid1.RowCount := 2;
  StringGrid1.Rows[0].CommaText := '"Start time","End time","Capacity","Value (MWh)"';
  StringGrid1.Rows[1].CommaText := '';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  List := TObjectList<TTimeseries>.Create;
  ClearList;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  List.Free;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    Show( List[ ListBox1.ItemIndex ] );
end;

procedure TForm1.ButtonJSONToObjectClick(Sender: TObject);
var
  TimeSeries: TTimeseries;
begin
  TimeSeries := TTimeseries.Create;
  TJSONParser.Parse( MemoJSON.Text, TimeSeries );
  AddTimeseries( TimeSeries );
  Show( TimeSeries );
  Memo1.Text := TJSONSerializer.Serialize(TimeSeries);
end;

{ TStuff }

constructor TStuff.Create;
begin
  FName   := '';
  FNumber := 0;
  FIDList := TList<Integer>.Create;
  FLines  := TStringList.Create;
  FADate  := EncodeDateTime( 2014, 02, 15, 06, 15, 00, 0 );
end;

destructor TStuff.Destroy;
begin
  FIDList.Free;
  FLines.Free;
  inherited;
end;

{ TOuter }

constructor TOuter.Create;
begin
  FKind := 0;
  FContent := '';
  FItems := TObjectList<TStuff>.Create;
  FSomeBool := True;
  FNameEnum := eThomas;
end;

destructor TOuter.Destroy;
begin
  FItems.Free;
  inherited;
end;

end.

