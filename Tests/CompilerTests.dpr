program CompilerTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  FD.Compiler.Lexer in '..\Compiler\Lexer\FD.Compiler.Lexer.pas',
  FD.Compiler.Lexer.Pascal in '..\Compiler\Lexer\FD.Compiler.Lexer.Pascal.pas',
  FD.Compiler.Lexer.Tokens in '..\Compiler\Lexer\FD.Compiler.Lexer.Tokens.pas',
  FD.Compiler.Environment in '..\Compiler\Environment\FD.Compiler.Environment.pas',
  FD.Compiler.Exceptions in '..\Compiler\Base\FD.Compiler.Exceptions.pas',
  TestFD.Compiler.Lexer.Pascal in 'Compiler\Lexer\TestFD.Compiler.Lexer.Pascal.pas',
  TestFD.Compiler.Lexer in 'Compiler\Lexer\TestFD.Compiler.Lexer.pas',
  TestFD.TestBase in 'TestBase\TestFD.TestBase.pas',
  System.BTree in '..\RTL\System.BTree.pas',
  System.Comparers in '..\RTL\System.Comparers.pas',
  FD.Compiler.StateMachine in '..\Compiler\Base\FD.Compiler.StateMachine.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

