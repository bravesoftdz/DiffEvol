unit TestFunctions;

//{$MODE Delphi}
{$mode objfpc}{$H+}
{$RANGECHECKS ON}
{$DEBUGINFO ON}

interface

uses
  Classes, SysUtils;

function Rastrigin(x: array of double): double;

implementation

function Rastrigin(x: array of double): double;
var
  A, n, i: integer;
  sum: double;
  //pi: Double;
begin
  A := 10;   //pi := 3.14159;
  n := Length(x);
  sum := 0.0;
  for i := 0 to n - 1 do
  begin
    sum := sum + x[i] * x[i] - A * cos(2 * pi * x[i]);
  end;
  Rastrigin := A * n + sum;
end;

function TryDecimalStrToInt(const S: string; out Value: integer): boolean;
begin
  Result := (pos('$', S) = 0) and TryStrToInt(S, Value);
end;

end.
