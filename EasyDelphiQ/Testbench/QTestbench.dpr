// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program QTestbench;

uses
  Vcl.Forms,
  MainWin in 'MainWin.pas' {MainForm},
  Some.namespace in 'Some.namespace.pas',
  EasyDelphiQ.DTO in '..\EasyDelphiQ.DTO.pas',
  EasyDelphiQ in '..\EasyDelphiQ.pas',
  AMQP.Channel in '..\..\AMQP\Source\AMQP.Channel.pas',
  AMQP.Classes in '..\..\AMQP\Source\AMQP.Classes.pas',
  AMQP.Connection in '..\..\AMQP\Source\AMQP.Connection.pas',
  AMQP.Frame in '..\..\AMQP\Source\AMQP.Frame.pas',
  AMQP.Header in '..\..\AMQP\Source\AMQP.Header.pas',
  AMQP.Helper in '..\..\AMQP\Source\AMQP.Helper.pas',
  AMQP.Interfaces in '..\..\AMQP\Source\AMQP.Interfaces.pas',
  AMQP.Message in '..\..\AMQP\Source\AMQP.Message.pas',
  AMQP.MessageProperties in '..\..\AMQP\Source\AMQP.MessageProperties.pas',
  AMQP.Method in '..\..\AMQP\Source\AMQP.Method.pas',
  AMQP.Payload in '..\..\AMQP\Source\AMQP.Payload.pas',
  AMQP.Protocol in '..\..\AMQP\Source\AMQP.Protocol.pas',
  AMQP.StreamHelper in '..\..\AMQP\Source\AMQP.StreamHelper.pas',
  AMQP.Types in '..\..\AMQP\Source\AMQP.Types.pas',
  WinHttp_TLB in '..\..\JSON\WinHttp_TLB.pas',
  EasyDelphiQ.Interfaces in '..\EasyDelphiQ.Interfaces.pas',
  EasyDelphiQ.Classes in '..\EasyDelphiQ.Classes.pas',
  Neas.PowermanApi.Notifications.DTOs.V1 in 'Neas.PowermanApi.Notifications.DTOs.V1.pas',
  AMQP.IMessageProperties in '..\..\AMQP\Source\AMQP.IMessageProperties.pas',
  DJSON in '..\..\JSON\DJSON.pas',
  AMQP.Arguments in '..\..\AMQP\Source\AMQP.Arguments.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'AMQP Testbench';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
