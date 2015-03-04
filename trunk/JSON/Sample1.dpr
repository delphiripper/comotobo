// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program Sample1;

uses
  FastMM4,
  Vcl.Forms,
  Sample1Form in 'Sample1Form.pas' {Form1},
  TimeseriesDTO in 'TimeseriesDTO.pas',
  JSON in 'JSON.pas',
  JSON2DTO in 'JSON2DTO.pas',
  DTO in 'DTO.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
