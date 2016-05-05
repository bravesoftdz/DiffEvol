unit TestFunctions;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function Rastrigin(x:  array of Double): Double;

implementation

function Rastrigin(x: array of Double): Double;
var
  A, n, i: Integer;
  sum: Double;
  //pi: Double;
begin
  A := 10;   //pi := 3.14159;
  n := Length(x);
  sum := 0.0;
  for i := 1 to n do
  begin
    sum := sum + x[i]*x[i] - A*cos(2*pi*x[i]);
  end;
  Rastrigin := A*n + sum;
end;

end.
