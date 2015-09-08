program HtmlTest;

uses
  Vcl.Forms,
  MainWin in 'MainWin.pas' {Form1},
  HtmlParser in '..\HtmlParser.pas',
  XPathLexer in '..\XPathLexer.pas',
  XPath in '..\XPath.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
