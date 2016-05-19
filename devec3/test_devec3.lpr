program test_devec3;

{$mode objfpc}{$H+}
//{$mode tp}

{$RANGECHECKS ON}
{$DEBUGINFO ON}
{$ASSERTIONS ON}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  udevec3, decl, TestFunctions;

begin
  writeln('test unit devec3...');
  devec3;
end.

