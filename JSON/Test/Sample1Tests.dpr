program Sample1Tests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4,
  DUnitTestRunner,
  TestJSON in 'TestJSON.pas',
  DJSON in '..\DJSON.pas',
  TestObjects in 'TestObjects.pas',
  WinHttp_TLB in '..\WinHttp_TLB.pas';

{R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

