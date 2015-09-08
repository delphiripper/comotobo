program XPathTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  TestHtmlParser in 'TestHtmlParser.pas',
  HtmlParser in '..\HtmlParser.pas',
  XPathLexer in '..\XPathLexer.pas',
  TestXPath in 'TestXPath.pas',
  XPath in '..\XPath.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

