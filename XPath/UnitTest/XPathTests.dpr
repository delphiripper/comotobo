program XPathTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  TestLexer in 'TestLexer.pas',
  TestHtmlParser in 'TestHtmlParser.pas',
  TestXPath in 'TestXPath.pas',
  HtmlParser in '..\HtmlParser.pas',
  XPathLexer in '..\XPathLexer.pas',
  XPath in '..\XPath.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

