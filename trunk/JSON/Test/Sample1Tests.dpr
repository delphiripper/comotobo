program Sample1Tests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4,
  DUnitTestRunner,
  TestJSON in 'TestJSON.pas',
  JSON in '..\JSON.pas',
  TestObjects in 'TestObjects.pas';

{R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

