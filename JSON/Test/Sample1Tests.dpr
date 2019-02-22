// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program Sample1Tests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  TestJSON in 'TestJSON.pas',
  DJSON in '..\DJSON.pas',
  TestObjects in 'TestObjects.pas',
  WinHttp_TLB in '..\WinHttp_TLB.pas';

{R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

