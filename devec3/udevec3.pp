unit udevec3;

(*
  About devec3
  Differential Evolution for MATLAB
  Copyright (C) 1996, 1997 R. Storn
  International Computer Science Institute (ICSI)
  1947 Center Street, Suite 600
  Berkeley, CA 94704
  E-mail: storn@icsi.berkeley.edu
  WWW:    http://www.icsi.berkeley.edu/~storn/
  Translated into Pascal by Hubert Geldon hgeldon@wp.pl
  and Piotr A. Gauden gaudi@cc.uni.toun.pl
  N. Copernicus Univ.
  Department of Chemistry
  87-100 Torun

  Translated into Lazarus/FPC by https://github.com/tmeits/
*)

//{$mode objfpc}{$H+}
{$mode tp}

{$RANGECHECKS ON}
{$DEBUGINFO ON}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils,
  crt, printer, graph, decl, genps;

procedure devec3; (* main procedure *)

implementation

(*  *)
function Determination(n_pop: integer; tempui: popsizematrix): double;
var
  suma1, suma2, suma3: double;
begin
  suma1 := 0.0;
  suma2 := 0.0;
  suma3 := 0.0;

  for i := 1 to XY do
    ydata_obl[i] := tempui[n_pop, 1] + tempui[n_pop, 2] * xdata[i] +
      tempui[n_pop, 3] * xdata[i] * xdata[i];

  for i := 1 to XY do
    suma2 := suma2 + ydata[i];
  suma2 := suma2 / XY;

  for i := 1 to XY do
  begin
    suma1 := suma1 + ((ydata[i] - ydata_obl[i]) * (ydata[i] - ydata_obl[i]));
    suma3 := suma3 + ((ydata[i] - suma2) * (ydata[i] - suma2));
  end;

  Determination := suma1 / suma3;
end;

procedure print_out;
begin
  TextColor(15);
  Gotoxy(18, 1);
  Writeln('**********************************************************');
  Gotoxy(18, 2);
  Writeln('Iteration            : ', cur_gen);
  Gotoxy(18, 4);
  Writeln('**********************************************************');
  Gotoxy(18, 5);
  Writeln('VTR:      ', VTR);
  Gotoxy(18, 6);
  Writeln('NP:       ', NP);
  Gotoxy(18, 7);
  Writeln('F:        ', F);
  Gotoxy(18, 8);
  Writeln('CR:       ', CR);
  Gotoxy(18, 9);
  Writeln('strategy: ', strategy);
  Gotoxy(18, 10);
  Writeln('**********************************************************');
  Gotoxy(18, 11);
  Writeln('Best Fitness(st_dev) : ', bestval);
  Gotoxy(18, 12);
  Writeln('Best Fitness(DC)     : ', DC);
  Gotoxy(18, 13);
  Writeln('**********************************************************');
  Gotoxy(18, 14);
  Writeln;
  Gotoxy(18, 15);
  Writeln;
  Gotoxy(18, 18);
  Writeln('**********************************************************');
  Gotoxy(18, 19);
  Writeln('BEST values of opt par: ');
  for k := 1 to D do
  begin
    Gotoxy(18, 19 + k);
    Writeln('best(', k, ') : ', bestmemiter[1, k]);
  end;
  Gotoxy(18, 19 + k + 1);
  Writeln('**********************************************************');
end;

procedure print_file;
begin
  Assign(res, 'result.txt');
  rewrite(res);
  Writeln(res, '**********************************************************');
  Writeln(res, '**********************************************************');
  Writeln(res, 'VTR:      ', VTR);
  Writeln(res, 'NP:       ', NP);
  Writeln(res, 'F:        ', F);
  Writeln(res, 'CR:       ', CR);
  Writeln(res, 'strategy: ', strategy);
  Writeln(res, '**********************************************************');
  Writeln(res, 'Iteration            : ', cur_gen);
  Writeln(res, 'Best Fitness(st_dev) : ', bestval);
  Writeln(res, 'Best Fitness(DC)     : ', DC);
  Writeln(res, '**********************************************************');
  Writeln(res);
  Writeln(res);
  Writeln(res, 'BEST values of opt par: ');
  for k := 1 to D do
    Writeln(res, 'best(', k, ') : ', bestmemiter[1, k]);
  Writeln(res, '**********************************************************');
  Writeln(res, '**********************************************************');
  Writeln(res, '** l.p.  X      Y_theor    Y_calc');
  Writeln(res);
  for k := 1 to XY do
  begin
    Write(res, '  ', k);
    Write(res, '  ', xdata[k]);
    Write(res, '  ', ydata[k]);
    Writeln(res, '  ', ydata_obl[k]);
  end;
  Close(res);
end;

procedure input_variables;
begin
  Assign(xs, 'x.txt');
  Assign(ys, 'y.txt');
  reset(xs);
  reset(ys);
  readln(xs, FDX);
  readln(ys, FDY);
  if FDX <> FDY then
  begin
    writeln('Number of data points in data files is different. Correct it.');
    halt;
  end;
  if FDX <> XY then
  begin
    writeln('Number of data points in data files is different then XY value');
    writeln('in "const" part of program. Correct it.');
    halt;
  end;
  for datnum := 1 to XY do
  begin
    readln(xs, xdata[datnum]);
    readln(ys, ydata[datnum]);
  end;
  Close(xs);
  Close(ys);
end;

procedure devec3; (* main procedure *)
label
  theend;
begin
  (* input variables *)

  (* Enter seed random number (0.000 ... 1.000) *)
  randomseed := rand_time_date;
  warmup_random(randomseed);

  input_variables;

  (* Evaluate the best member after initialization
     pop is a matrix of size NPxD. It will be initialized with random values
     between the min and max values of the parameters *)
  for i := 1 to NP do
    for j := 1 to D do
    begin
      pop[i, j] := XVmin[j] + (rand * (XVmax[j] - XVmin[j]));
    end;
  n_pop := 1; (*------check the first member-----------------*)
  odch_st := Determination(n_pop, pop);
  DC := 1 - odch_st;
  memberval[1] := odch_st;
  bestval := memberval[1];
  tempmemberval := memberval[1];
  ClrScr;
  Print_Out;
  Print_File;

  for i := 1 to D do
    bestmemiter[1, i] := pop[1, i]; (*------check the first member-----------------*)
  for j := 2 to NP do (*------check the rest of members--------------*)
  begin
    n_pop := j;
    odch_st := Determination(n_pop, pop);
    DC := 1 - odch_st;
    memberval[j] := odch_st;
    tempmemberval := memberval[j];
    if tempmemberval < bestval then
    begin
      bestval := memberval[j];
      for k := 1 to D do
        bestmemiter[1, k] := pop[j, k];
      Print_Out;
      Print_File;
    end;
  end;

  for k := 1 to D do
    bestmem[1, k] := bestmemiter[1, k];  (*-best member ever, after first generation*)
  bestvalev := bestval;  (*------check the rest of members----------*)

  {*------DE-Minimization-------------------------------------------------------------------*}
  {*------popold is the population which has to compete. It is------------------------------*}
  {*------static through one iteration. pop is the newly------------------------------------*}
  {*------emerging population.--------------------------------------------------------------*}
  cur_gen := 2;
  while cur_gen <= itermax do
  begin
    for i := 1 to NP do
      for j := 1 to D do
        popold[i, j] := pop[i, j];
    {*-------save the old population--------------*}

    {%%%%%% ind}
    for i := 1 to 4 do
    begin
      randmatrix[i] := rand;
      randmatrixint[i] := i;
    end;
    for j := 1 to 4 do
    begin
      valmin := 1.0;
      for i := 1 to 4 do
      begin
        if randmatrix[i] < valmin then
        begin
          valmin := randmatrix[i];
          ind[j] := randmatrixint[i];
          {*-------shuffle locations of vectors-*}
        end;
      end;
      for k := 1 to 4 do
      begin
        if randmatrix[k] = valmin then
          randmatrix[k] := 1;
      end;
    end;
    {%%%%%% ind}


    {%%%%%% rot}
    for i := 1 to NP do
      rot[i] := i - 1;  {*-------rotating index array (size NP)-------------------*}
    {%%%%%% rot}


    {%%%%%% a1}
    for i := 1 to NP do
    begin
      randmatrixnp[i] := rand;
      randmatrixnpint[i] := i;
    end;
    for j := 1 to NP do
    begin
      valmin := 1.0;
      for i := 1 to NP do
      begin
        if randmatrixnp[i] < valmin then
        begin
          valmin := randmatrixnp[i];
          a1[j] := randmatrixnpint[i];
          {*-------shuffle locations of vectors*}
        end;
      end;
      for k := 1 to NP do
      begin
        if randmatrixnp[k] = valmin then
          randmatrixnp[k] := 1;
      end;
    end;
    {%%%%%% a1}

    {%%%%%% a2}
    for i := 1 to NP do
    begin
      transmata[i] := rot[i] + ind[1];
      tempmata[i] := transmata[i] / NP;
      rt[i] := trunc(tempmata[i]);
      rt[i] := transmata[i] - NP * rt[i];
      {*--rotate indices by ind[1] position*}
    end;

    for i := 1 to NP do
    begin
      k := rt[i] + 1;
      a2[i] := a1[k];
      {*-------rotate vector locations----------------------*}
    end;
    {%%%%%% a2}

    {%%%%%% a3}
    for i := 1 to NP do
    begin
      transmata[i] := rot[i] + ind[2];
      tempmata[i] := transmata[i] / NP;
      rt[i] := trunc(tempmata[i]);
      rt[i] := transmata[i] - NP * rt[i];
      {*--rotate indices by ind[1] position*}
    end;
    for i := 1 to NP do
    begin
      k := rt[i] + 1;
      a3[i] := a2[k];
    end;
    {%%%%%% a3}

    {%%%%%% a4}
    for i := 1 to NP do
    begin
      transmata[i] := rot[i] + ind[3];
      tempmata[i] := transmata[i] / NP;
      rt[i] := trunc(tempmata[i]);
      rt[i] := transmata[i] - NP * rt[i];
      {*--rotate indices by ind[1] position*}
    end;
    for i := 1 to NP do
    begin
      k := rt[i] + 1;
      a4[i] := a3[k];
    end;
    {%%%%%% a4}

    {%%%%%% a5}
    for i := 1 to NP do
    begin
      transmata[i] := rot[i] + ind[4];
      tempmata[i] := transmata[i] / NP;
      rt[i] := trunc(tempmata[i]);
      rt[i] := transmata[i] - NP * rt[i];
      {*--rotate indices by ind[1] position*}
    end;
    for i := 1 to NP do
    begin
      k := rt[i] + 1;
      a5[i] := a4[k];
    end;
    {%%%%%% a5}

    {%%%%%% pm1}
    for i := 1 to NP do
    begin
      k := a1[i];
      for j := 1 to D do
        pm1[i, j] := popold[k, j];
      {*-------shuffled population 1--------------*}
    end;
    {%%%%%% pm1}

    {%%%%%% pm2}
    for i := 1 to NP do
    begin
      k := a2[i];
      for j := 1 to D do
        pm2[i, j] := popold[k, j];
      {*-------shuffled population 2--------------*}
    end;
    {%%%%%% pm2}

    {%%%%%% pm3}
    for i := 1 to NP do
    begin
      k := a3[i];
      for j := 1 to D do
        pm3[i, j] := popold[k, j];
      {*-------shuffled population 3--------------*}
    end;
    {%%%%%% pm3}

    {%%%%%% pm4}
    for i := 1 to NP do
    begin
      k := a4[i];
      for j := 1 to D do
        pm4[i, j] := popold[k, j];
      {*-------shuffled population 4--------------*}
    end;
    {%%%%%% pm4}

    {%%%%%% pm5}
    for i := 1 to NP do
    begin
      k := a5[i];
      for j := 1 to D do
        pm5[i, j] := popold[k, j];
      {*-------shuffled population 5--------------*}
    end;
    {%%%%%% pm5}

    {%%%%%% bm}
    for i := 1 to NP do
      for j := 1 to D do
        bm[i, j] := bestmemiter[1, j];
    {*-------population filled with-----------*}
    {%%%%%% bm}{*--the best member of the last iteration-*}

    {%%%%%% mui<CR}
    for i := 1 to NP do
      for j := 1 to D do
      begin
        teval := rand;
        if teval < CR   {*-------all random numbers < CR are 1, 0 otherwise-*}
        then
          mui[i, j] := 1
        else
          mui[i, j] := 0;
      end;
    {%%%%%% mui<CR}


    {%%%%%% strategy<5}
    if strategy > 5 then
      st := strategy - 5   {*-------biniomial crossover---------------------------*}
    else
    begin
      st := strategy;
      {*-------exponential crossover-------------------------*}
      for i := 1 to NP do
        {*-------transpose, collect 1's in each column---------*}
        for j := 1 to D do
          muitrans[j, i] := mui[i, j];

      for j := 1 to NP do
      begin
        ip := 0;
        ik := D + 1;
        for i := 1 to D do
        begin
          if muitrans[i, j] = 0 then
          begin
            ip := ip + 1;
            muitratm[ip, j] := 0;
            {*-------shuffle locations of vectors------*}
          end
          else
          begin
            ik := ik - 1;
            muitratm[ik, j] := 1;
          end;
        end;
      end;

      for j := 1 to NP do
        for i := 1 to D do
          muitrans[i, j] := muitratm[i, j];

      for i := 1 to D do
        rotd[i] := i - 1;

      for j := 1 to NP do
      begin
        teval := rand * D;
        n := trunc(teval);
        if n > 0 then
        begin
          for i := 1 to D do
          begin
            transmata[i] := rotd[i] + n;
            tempmata[i] := transmata[i] / D;
            rtd[i] := trunc(tempmata[i]);
            rtd[i] := transmata[i] - D * rtd[i];
            {*----rotate column i by n-*}
          end;
          for i := 1 to D do
          begin
            k := rtd[i] + 1;
            muitratm[i, j] := muitrans[k, j];
          end;
        end;
      end;

      for i := 1 to D do
        {*-------transpose back------------------------------------*}
        for j := 1 to NP do
          mui[j, i] := muitratm[i, j];
    end;
    {%%%%%% strategy<5}

    {%%%%%% mpo}
    for i := 1 to NP do
      for j := 1 to D do
        if mui[i, j] = 1 then
          mpo[i, j] := 0
        else
          mpo[i, j] := 1;  {*-------inverse mask to mui----------------------*}
    {%%%%%% mpo}

    {%%%%%% st=1}
    if st = 1
    {*-------DE/best/1----------------------------------------*} then
      for i := 1 to NP do
        for j := 1 to D do
          {*-------differential variation and crossover---------*}
          ui[i, j] := (popold[i, j] * mpo[i, j]) +
            ((bm[i, j] + F * (pm1[i, j] - pm2[i, j])) * mui[i, j])
    else
    {%%%%%% st=2}
    if st = 2
    {*-------DE/rand/1----------------------------------------*} then
      for i := 1 to NP do
        for j := 1 to D do
          {*-------differential variation and crossover--------*}
          ui[i, j] :=
            (popold[i, j] * mpo[i, j]) +
            ((pm3[i, j] + F * (pm1[i, j] - pm2[i, j])) * mui[i, j])
    else
    {%%%%%% st=3}
    if st = 3
    {*-------DE/rand-to-best/1--------------------------------*} then
      for i := 1 to NP do
        for j := 1 to D do
          {*-------differential variation and crossover------*}
        begin
          ui[i, j] :=
            popold[i, j] + F * (bm[i, j] - popold[i, j]) + F * (pm1[i, j] - pm2[i, j]);
          ui[i, j] := (popold[i, j] * mpo[i, j]) + (ui[i, j] * mui[i, j]);
        end
    else
    {%%%%%% st=4}
    if st = 4
    {*-------DE/best/2----------------------------------------*} then
      for i := 1 to NP do
        for j := 1 to D do
          {*-------differential variation and crossover----*}
          ui[i, j] :=
            (popold[i, j] * mpo[i, j]) +
            ((bm[i, j] + F * (pm1[i, j] - pm2[i, j] + pm3[i, j] - pm4[i, j])) *
            mui[i, j])
    else
    {%%%%%% st=5}
    if st = 5
    {*-------DE/rand/2----------------------------------------*} then
      for i := 1 to NP do
        for j := 1 to D do
          {*-------differential variation and crossover--*}
          ui[i, j] :=
            (popold[i, j] * mpo[i, j]) +
            ((pm5[i, j] + F * (pm1[i, j] - pm2[i, j] + pm3[i, j] - pm4[i, j])) *
            mui[i, j]);
    {%%%%%% end st}



    {%%%%%% start of main loop}
    for i := 1 to NP do
      {*-------select which vectors are allowed to enter the new population*}
    begin
      for j := 1 to D do
        {*-------check range-----------------------------------------------*}
        if (ui[i, j] < XVmin[j]) then
        begin
          ui[i, j] := XVmin[j] + rand * (XVmax[j] - XVmin[j]);
        end
        else if (ui[i, j] > XVmax[j]) then
        begin
          ui[i, j] := XVmin[j] + rand * (XVmax[j] - XVmin[j]);
        end;


      n_pop := i;
      odch_st := Determination(n_pop, ui);
      DC := 1 - odch_st;
      tempmemberval := odch_st;
      i := n_pop;
      if (tempmemberval <= memberval[i]) then
        {* if competitor is better than value in "cost array"}
      begin
        for j := 1 to D do
        begin
          pop[i, j] := ui[i, j];
          {* replace old vector with new one (for new iteration}
        end;
        memberval[i] := tempmemberval;        {* save value in "cost array"}
      end;
      {*-----------we update bestval only in case of success to save time-----------}
      if (tempmemberval < bestval) then
        {* if competitor better than the best one ever}
      begin
        bestval := tempmemberval;             {* new best value}
        for j := 1 to D do
        begin
          bestmemiter[1, j] := ui[i, j];
          {* replace old vector with new one (for new iteration}
        end;
        Print_File;
        Print_Out;
      end;

      {*----------- freeze the best member of this iteration for the coming-------------------}
      {*----------- iteration. This is needed for some of the strategies----------------------}

    end;
    {%%%%%% end of main loop}

    {*-----------Select which vectors are allowed to enter the new population-----------*}
    for j := 1 to D do
    begin
      bestmem[1, j] := bestmemiter[1, j];
      bestvalev := bestval;
    end;


    if bestvalev < VTR then
      goto theend;

    cur_gen := cur_gen + 1;

  end;

  theend:
  writeln;
  writeln;
  writeln;
  writeln;
  writeln;
  writeln('Calculations are done. "Value To Reach" has been reached.');
  writeln('Results are in ?:/result.txt file.');
  writeln('Press any key...');
  repeat
  until keypressed;
end;


end.
