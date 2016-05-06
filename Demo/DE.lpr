program DE;

{$MODE Delphi}

uses
  Forms, Interfaces, DEGui;

(*  {$R *.res}
http://wiki.freepascal.org/Lazarus_Resources/ru *)

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
