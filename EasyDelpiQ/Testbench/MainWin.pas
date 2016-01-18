unit MainWin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, EasyDelphiQ, EasyDelphiQ.Interfaces, Some.namespace;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    FBus: TBus;
    FSubscription: ISubscriptionResult;
    Procedure Handler( var Msg: TestDTO );
  public
  end;

var
  MainForm: TMainForm;

implementation

Uses
  EasyDelphiQ.DTO;

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
var
  DTO: TestDTO;
begin
  DTO := TestDTO.Create;
  Try
    DTO.ID := 42;
    DTO.Name := 'Zaphod';
    FBus.Publish( DTO );
    Memo1.Lines.Add( 'Message published' );
  Finally
    DTO.Free;
  End;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  DTO: TestDTO;
begin
  DTO := FBus.Get<TestDTO>( 'Testbench' );
  if DTO = nil then
    Memo1.Lines.Add( 'No message' )
  else
  Try
    Memo1.Lines.Add( 'Received:' );
    Memo1.Lines.Add( '  DTO.ID:   ' + DTO.ID.ToString );
    Memo1.Lines.Add( '  DTO.Name: ' + DTO.Name );
  Finally
    DTO.Free;
  End;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  FSubscription := FBus.Subscribe<TestDTO>( 'Testbench', '', Handler );
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  FSubscription.Cancel;
  FSubscription := nil;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FBus := RabbitHutch.CreateBus( 'localhost', 'TestUser', 'password' );
  FSubscription := nil;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FBus.Free;
end;

procedure TMainForm.Handler(var Msg: TestDTO);
var
  ID: Integer;
  Name: String;
begin
  ID := Msg.ID;
  Name := Msg.Name;
  TThread.Queue( nil,
    Procedure
    begin
      Memo1.Lines.Add( 'Received:' );
      Memo1.Lines.Add( '  DTO.ID:   ' + ID.ToString );
      Memo1.Lines.Add( '  DTO.Name: ' + Name );
    end );
end;

end.
