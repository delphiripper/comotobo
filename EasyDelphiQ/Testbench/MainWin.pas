unit MainWin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, EasyDelphiQ, EasyDelphiQ.Interfaces, Some.namespace,
  Neas.PowermanApi.Notifications.DTOs.V1, System.Generics.Collections;

type
  TMainForm = class(TForm)
    ButtonPublish: TButton;
    Memo1: TMemo;
    ButtonGet: TButton;
    ButtonSubscribe: TButton;
    ButtonCancelSubscription: TButton;
    SubscribeTimeseries: TButton;
    procedure ButtonPublishClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonGetClick(Sender: TObject);
    procedure ButtonSubscribeClick(Sender: TObject);
    procedure ButtonCancelSubscriptionClick(Sender: TObject);
    procedure SubscribeTimeseriesClick(Sender: TObject);
  private
    FBus: TBus;
    FBusPM: TBus;
    FSubscription: ISubscriptionResult;
    FSubscriptionPM: ISubscriptionResult;
    FList: TThreadList<ProductionGroupDataSerieCollectionV1>;
    Procedure BusConnected(Sender: TObject);
    Procedure BusDisconnected(Sender: TObject);
    Procedure Handler( var Msg: TestDTO );
    Procedure HandlerPM( var Msg: ProductionGroupDataSerieCollectionV1 );
    Procedure WMUSER(var Msg: TMessage); Message WM_USER;
    procedure ReadMessagesFromList;
  public
  end;

var
  MainForm: TMainForm;

implementation

Uses
  EasyDelphiQ.DTO, AMQP.Arguments, DJSON;

{$R *.dfm}

procedure TMainForm.BusConnected(Sender: TObject);
begin
  Memo1.Lines.Add( 'Connected' );
end;

procedure TMainForm.BusDisconnected(Sender: TObject);
begin
  Memo1.Lines.Add( 'Disconnected' );
end;

procedure TMainForm.ButtonPublishClick(Sender: TObject);
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

procedure TMainForm.ButtonGetClick(Sender: TObject);
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

procedure TMainForm.ButtonSubscribeClick(Sender: TObject);
begin
  FSubscription := FBus.Subscribe<TestDTO>( 'Testbench', Handler, MakeArguments.SetMessageTTL( 30000 ) );
end;

procedure TMainForm.ButtonCancelSubscriptionClick(Sender: TObject);
begin
  FSubscription.Cancel;
  FSubscription := nil;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FBus := RabbitHutch.CreateBus( 'host=localhost;username=TestUser;password=password' );
  FBus.OnConnected := BusConnected;
  FBus.OnDisconnected := BusDisconnected;
  FSubscription := nil;

  FBusPM := RabbitHutch.CreateBus( 'host=rabbitmq_test;username=rabbit;password=rabbit' );
  FBusPM.OnConnected := BusConnected;
  FBusPM.OnDisconnected := BusDisconnected;
  FSubscriptionPM := nil;

  FList := TThreadList<ProductionGroupDataSerieCollectionV1>.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FBus.Free;
  FBusPM.Free;
  FList.Free;
end;

procedure TMainForm.Handler(var Msg: TestDTO);
var
  DTO: TestDTO;
begin
  DTO := Msg; //Necessary to capture the object in the anonymous method below
  TThread.Queue( nil,
    Procedure
    begin
      Memo1.Lines.Add( 'Received:' );
      Memo1.Lines.Add( '  DTO.ID:   ' + DTO.ID.ToString );
      Memo1.Lines.Add( '  DTO.Name: ' + DTO.Name );
      DTO.Free; //Free the object here - we are done with it
    end );
  Msg := nil; //Don't free the object here
end;

procedure TMainForm.HandlerPM(var Msg: ProductionGroupDataSerieCollectionV1);
begin
  FList.Add( Msg );
  Msg := nil;
  PostMessage( Handle, WM_USER, 0, 0 )
end;

procedure TMainForm.SubscribeTimeseriesClick(Sender: TObject);
begin
  FSubscriptionPM := FBusPM.Subscribe<ProductionGroupDataSerieCollectionV1>( 'NexusDevTest', 'PowermanTest', '', HandlerPM,
                       MakeArguments.Add( 'x-message-ttl', 20000 ) );
end;

procedure TMainForm.WMUSER(var Msg: TMessage);
begin
  ReadMessagesFromList;
end;

procedure TMainForm.ReadMessagesFromList;
var
  List: TList<ProductionGroupDataSerieCollectionV1>;
  Msg: ProductionGroupDataSerieCollectionV1;
  Dataseries: ProductionGroupDataSerieV1;
begin
  List := FList.LockList;
  Try
    for Msg in List do
    Begin
      Memo1.Lines.Add( 'Powerman Notification (' + Msg.Count.ToString + ' dataseries)' );
      for Dataseries in Msg do
        Memo1.Lines.Add( TJSONSerializer.Serialize(Dataseries) );
      Msg.Free;
    End;
    List.Clear;
  Finally
    FList.UnlockList;
  End;
end;

end.
