unit genps;

{$mode objfpc}{$H+}
//{$mode tp}

{$RANGECHECKS ON}
{$DEBUGINFO ON}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, DOS;

var
  oldrand: array[1..55] of double;
  jrand: integer;

function rand_time_date: double;
procedure advance_random;
function rand: real;
procedure warmup_random(random_seed: real);


implementation

function rand_time_date: double; { subfunction }
const
  ia = 16807;
  ic = 2147483647;
var
  Hour, Minute, Second, Drop: word;
  Year, Month, Day, Day_of_week: word;
  iseed: integer;
  mod_iseed: integer;
  iq, ir: double;
  ih, il, it: double;
  iqint: longint;
  iseed_r: double;
begin
  iq := ic / ia;
  iqint := trunc(iq);
  ir := (ic mod ia);
  GetDate(Year, Month, Day, Day_of_week);
  GetTime(Hour, Minute, Second, Drop);
  iseed := year + 70 * (Month + 12 * (Day + 31 * (Hour + 23 * (Minute + 59 * Second))));
  mod_iseed := (iseed mod 2);
  if (mod_iseed = 0) then
    iseed := iseed - 1
  else
    iseed := iseed;
  ih := iseed / iqint;
  il := (iseed mod iqint);
  it := ia * il - ir * ih;
  if it > 0 then
    iseed_r := it
  else
    iseed_r := ic + it;
  rand_time_date := iseed_r / ic;
end; {rand_time_date}

procedure advance_random;     { Procedure }
var
  j: integer;
  new_random: real;
begin
  for j := 1 to 24 do
  begin
    new_random := oldrand[j] - oldrand[j + 31];
    if (new_random < 0.0) then
      new_random := new_random + 1.0;
    oldrand[j] := new_random;
  end;
  for j := 25 to 55 do
  begin
    new_random := oldrand[j] - oldrand[j - 24];
    if (new_random < 0.0) then
      new_random := new_random + 1.0;
    oldrand[j] := new_random;
  end;
end; { advance_random }

function rand: real;    { Subfunction }
begin
  jrand := jrand + 1;
  if (jrand > 55) then
  begin
    jrand := 1;
    advance_random;
  end;
  rand := oldrand[jrand];
end; { rand }

procedure warmup_random(random_seed: real); { get random off and running }
var
  i, j: integer;
  new_random, prev_random: real;
begin
  oldrand[55] := random_seed;
  new_random := 1.0e-9;
  prev_random := random_seed;
  for j := 1 to 54 do
  begin
    i := 21 * j mod 55;
    oldrand[i] := new_random;
    new_random := prev_random - new_random;
    if (new_random < 0.0) then
      new_random := new_random + 1.0;
    prev_random := oldrand[i];
  end;
  advance_random;
  advance_random;
  advance_random;
  jrand := 0;
end; { warmup_random }

end.
