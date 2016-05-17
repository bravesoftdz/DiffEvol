program DE;

//{$MODE Delphi}
{$mode objfpc}{$H+}
uses
  Forms, Interfaces, DEGui;

(*  {$R *.res}
http://wiki.freepascal.org/Lazarus_Resources/ru *)

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormVSDE, FormVSDE);
  Application.Run;
end.
