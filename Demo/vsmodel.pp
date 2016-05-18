unit VSModel;

(* VSModel
   Based on an implementation by A.Shashkin
   Fortran v.31 19 may 98

   Lazarus/FPC port 2016 by Ivan Tychkov & tmeits
   https://github.com/tmeits/DiffEvol

   Модель формирования годичных колец хвойных Ваганова-Шашкина V6 9.5.16 BIMODAL
   *)

{$mode objfpc}{$H+}
{$RANGECHECKS ON}
{$DEBUGINFO ON}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  Math;

type
  FloatType = Double;

type
  array30    = array [1..31] of real;
  array366   = array [1..366] of real;
  integer20  = array [1..20] of integer;
  real10     = array [1..10] of real;
  rmax_int   = array [1..200] of integer;
  rmax_real  = array [1..200] of real;
  integer10  = array [1..10] of integer;
  real20     = array [1..20] of real;
  real100    = array [1..100] of real;
  integer100 = array [1..100] of integer;
  maxreal    = array [1..300] of real;

const
  RMAX = 200;

var
  (*
  tmeits
  в эти переменные заносятся значения с формы DEGui и затем передаются в
     оригинальную функцию growth *)
  vsde_filename_1, vsde_path, vsde_filename_4: string;
  vsde_latitude:    string;
  vsde_endyear_str: string;
  vsde_Year_number: integer;
  vsde_gr_ini:      real100;
  vsde_gr_middle:   real100;
  vsde_BEGYEAR:     integer;
  vsde_ENDYEAR:     integer;
  vsde_years:       integer100;


  //Mainform: TMainform;
  p1_TB, p2_TB, p3_TB, p4_TB, p5_TB, p6_TB, p7_TB, P8_TB, P9_TB, P10_TB, p11_TB, p12_TB,
  p13_TB, p14_TB, p15_TB, p16_TB, p17_TB, p18_TB, p19_TB, p22_TB, PC1_TB, pc6_TB,
  pc7_TB, pc8_TB, pc9_TB, P31_TB: real;
  //глобальная переменная, принимающая значение при изменении бегунка

  p1_TB_l, p2_TB_l, p3_TB_l, p4_TB_l, p5_TB_l, p6_TB_l, p7_TB_l, P8_TB_l, P9_TB_l,
  P10_TB_l, p11_TB_l, p12_TB_l, p13_TB_l, p14_TB_l, p15_TB_l, p16_TB_l, p17_TB_l,
  p18_TB_l, p19_TB_l, p22_TB_l, PC1_TB_l, pc6_TB_l, pc7_TB_l, pc8_TB_l,
  pc9_TB_L, SolarMod_TB_L: boolean;
  //логическая переменная, которая будет показывать основной процедуре, что данные изменились

  k1:               integer;
  p24:              real; //добавить описание, если сработает!
  //переменные со значениями прироста для графиков
  years_array, years_ini: integer100;
  //годы из файла с реальными данными
  gr_ini:           real100; //значения из файла с реальными данными
  gr_norm, ini_norm, gr_middle, another_gr_middle: real100;
  //значения из программы
  jj1:              integer; //количество лет в рассматриваемый период
  num:              integer;
  FILT:             string;
  BEGYEAR, ENDYEAR: integer;
  result_namefile:  string;
  P_res:            array30;
  pc_res:           real20;
  K_res:            integer20;
  KC_res, PS_res, KS_res: real10;
  kurs_old, kurs_older: real100;
  //имена файлов
  edit1text, edit2text, edit3text, edit4text, edit6text, edit7text, edit8text, edit5text: string;
  //коэффициент корреляции
  c_coef:           real;
  old_coef:         real;
  Leap_year:        integer;
  Beg_gr1, End_gr1, Beg_gr2, End_gr2: integer; //начало и конец сезона роста
  check_c, check_s, check_t, check_p: integer;
  mean_rates:       integer;
// переменная для сохранения среднего значения при стандартизации суммарных скоростей роста

function GrowthFitnessFunction(x: array of FloatType): FloatType;


implementation

procedure inp(a: string; var F1: text);     //open and read
begin
 {$I-}
  assignfile(F1, a);
  reset(F1);
 {$I+}
  If IOresult <> 0 then
    showmessage('error in data of file ');
end;


procedure noof(path: string; var pathcli, filecli: string);
var
  p, id4: integer;
begin
  p       := 1;
  filecli := path;
  pathcli := path;
  while p <> 0 do
  begin
    id4     := length(filecli);
    p       := pos('\', filecli);
    filecli := copy(filecli, p + 1, id4);
  end;
  filecli := copy(filecli, p + 1, id4 - 4);
  p       := pos(filecli, pathcli);
  pathcli := copy(pathcli, 1, p - 1);
end;

procedure L_y(year_str: string);
var
  year, id: integer;
  year1:    string;
begin
  id    := length(year_str);
  year1 := copy(year_str, id - 7, 4);
  year  := strtoint(year1);
  if (year mod 100 = 0) {and (year mod 400<>0)} then
  begin
    if year mod 4 = 0 then
      Leap_year := 366
    else //showmessage('âèñîêîñíûé' ) else
      Leap_year := 365; //showmessage('íå âèñîêîñíûé' );
  end
  else
  if year mod 4 = 0 then
    Leap_year := 366
  else //showmessage('âèñîêîñíûé' ) else
    Leap_year := 365//showmessage('íå âèñîêîñíûé' );
  ;
end;

procedure INTPOL(x: array366; var M1, M2: integer);
var
  {main}
  {other}
  f:                real;
  j, i, k:          integer;
  check20, check51: boolean;
begin
  M1 := 1;
  M2 := Leap_year;
  i  := 1;
  WHILE (x[i] < -900) DO {!Beginning of season}
    i := i + 1;
  M1 := i;  {!1st group of missing values}
  check20 := true;
  {20}
  while check20 = true do
  begin

    check20 := false;
    i       := i + 1;
    IF (x[i] < -900) THEN
    begin{1}
      j := i;
      WHILE (x[j] < -900) AND (j <> Leap_year) do
        j := j + 1;
      IF (j < Leap_year) THEN
      begin{2}
        f := (x[j] - x[i - 1]) / (j - i);
        for k := i to j - 1 do
          x[k] := x[k - 1] + f; {! Estimate}
        i := j - 1;
        check20 := true;
      end; {2}
      IF (x[j] > -900) THEN {! End of year}
      begin {3}
        f := (x[j] - x[i - 1]) / (j - i);
        for k := i to j - 1 do
          x[k] := x[k - 1] + f;
        i := j;
        M2 := i;
        check51 := true;
      end  {3}
      ELSE
      begin {4}
        IF (j - i < 11) THEN
        begin
          for k := i to j do
            x[k] := x[k - 1];
          i := j;
          M2 := i;
          check51 := true; {!Return}
        end  {4}
        ELSE
        begin  {5}
          M2      := i - 1;
          check51 := true; {!Return}
        end;    {6}
      end;
    end  {1}
    ELSE {! x greater than -900}
    IF (i = Leap_year) THEN
    begin
      M2      := i;
      check51 := true; {//! Return}
    END;
    if check51 = true then
      check20 := false;
  end;
end;

procedure INPCLI(climfile: string; P: array30;
  var temp, prec: array366; var M: integer;
  var ERR: integer; var NU: integer);
var
  f:                text;
  {output}
  M1, M2, M11, M12: integer;
  {normal}
  FC:               string[30];
  fc1:              string[30];
  I, ii:            INTEGER;
  N:                integer;
  {dopolnit}
  txt:              string;
  Da{Day}, Mo{Month}, Ye{Year}: integer;
  Min_Temp, Max_Temp, Mean_cloud, Sun_rel, Sun_abs, Trans, Press, humid: integer;
  check:            real;
  error:            integer;
begin
{$I-}
  L_y(climfile);
  assignfile(f, ClimFile);
  reset(f);
  //for i:=1 to 8 do
  //readln(f);
  for i := 1 to Leap_year do
  begin //delete
    read(f, Da);
    read(f, Mo);
    read(f, Ye);
    //read(f,Min_Temp);
    //read(f,Max_Temp);
    //read(f,Mean_cloud);
    //read(f,Sun_rel);
    //read(f,Sun_abs);
    read(f, prec[i]);
    //read(f,Trans);
    //read(f,Press);
    read(f, temp[i]{,humid});
    readln(f);
{$I+}
    If IOresult <> 0 then
      error := error + 1;
{ Application.MessageBox(PChar('error reading of '+ClimFile +'. Error in string #'+inttostr(i)
 +' '+floattostr(temp[i])),'Warning',1);}

  end;//delete
  closefile(f);
  For i := 1 to Leap_year do
  begin
    if temp[i] = -9999 then
      temp[i] := (temp[i - 1] + temp[i - 2]) / 2;
    if prec[i] = -9999 then
      prec[i] := 0;
  end;
  for i := 1 to Leap_year do
    temp[I] := Temp[i] / 10;
  for i     := 1 to Leap_year do
    prec[I] := prec[i] / 10;
  M1 := 1;
  M2 := Leap_year;
  ii := Leap_year;
  INTPOL(Temp, M11, M12); {procedure}
  M1 := M11;
  M2 := M12;
  M  := M2 - M1 + 1;
  NU := M1 - 1;
  FOR I := 1 to M do
  begin
    temp[I] := P[26] * temp[I] + P[27];
    prec[I] := P[28] * prec[I] + P[29];
    IF (PREC[I] <= 0.0) then
      PREC[I] := 0;
  END;
end;

procedure FILTER(F: array366; N: integer; TFL: string;
  var F_1: array366);
var
  x:          array [-11..377] of real;
  wt:         array [1..12] of real;
  WTL:        array [1..7] of real;
  WTH:        array [1..7] of real;
  WTLL:       array [1..12] of real;
  j, i, k, L: integer;
  LIMIT:      integer;
  SUM, XMN:   real;
begin
  WTL[1] := 0.3680;
  WTL[2] := 0.3153;
  WTL[3] := 0.1971;
  WTL[4] := 0.0876;
  WTL[5] := 0.0263;
  WTL[6] := 0.0049;
  WTL[7] := 0.0005;

  WTH[1] := 0.1429;
  WTH[2] := 0.1429;
  WTH[3] := 0.1429;
  WTH[4] := 0.1429;
  WTH[5] := 0.1429;
  WTH[6] := 0.1429;
  WTH[7] := 0.1429;

  WTLL[1]  := 0.1538;
  WTLL[2]  := 0.1502;
  WTLL[3]  := 0.1429;
  WTLL[4]  := 0.1319;
  WTLL[5]  := 0.1172;
  WTLL[6]  := 0.0989;
  WTLL[7]  := 0.0769;
  WTLL[8]  := 0.0549;
  WTLL[9]  := 0.0363;
  WTLL[10] := 0.0220;
  WTLL[11] := 0.0110;
  WTLL[12] := 0.0037;
  {__________________}
  for i := -11 to 377 do
    x[i]  := 0;
  for I   := 1 to N do
    x[I]  := F[I];
  for i   := 1 to 12 do
    wt[i] := 0;
  IF (TFL = 'V') OR (TFL = 'v') THEN
  begin
    for K := 1 to 12 do
      WT[K] := WTLL[K];
    LIMIT := 11;
  end;
  IF (TFL = 'L') OR (TFL = 'l') THEN
  begin
    LIMIT := 6;
    for K := 1 to 12 do
      WT[K] := WTL[K];
  end;
  IF (TFL = 'A') OR (TFL = 'a') THEN
  begin
    LIMIT := 2;    //фильтр А вместо 6 поставил 2
    for K := 1 to 12 do
      WT[K] := WTH[K];
  END;
  SUM := 0.0;
  for K := 1 to LIMIT + 1 do
    SUM := SUM + X[K];
  XMN := SUM / (LIMIT + 1);
  for K := -LIMIT to 0 do
    X[K] := XMN;
  SUM := 0.0;
  For K := N - LIMIT to N do
    SUM := SUM + X[K];
  XMN := SUM / (LIMIT + 1);
  For k := N + 1 to N + Limit do
    X[k] := xmn;
  For I  := 1 to N do
  begin
    F_1[i] := 0.0;
    For j := 1 to (Limit + 1) do
    Begin
      L      := I - J + 1;
      F_1[i] := f_1[i] + WT[j] * X[l];
    End;
  End;
end;

function arcsin(x: real): real;
begin
  if x = 1.0 then
    arcsin := Pi / 2.0
  else
    arcsin := arctan(x / (1 - x * x));
end;

function max(a, b: real): real;  {JUST FOR real}
begin
  if a > b then
    max := a
  else
    max := b;
end;

procedure DAYLENGTH(LAT: real; NU: integer; M: integer; var DL: array366);
{входные\выходные}
var
  Pi:   real;
  i:    integer;
  fi:   real;
  sin_tau, cos_tau, cos_hs, sin_hs, tau, hs: array366;
  cos1: real;
  DTSI: real;
  s:    real;
begin
  // if lat>54 then lat:=54;
  PI  := 3.14159;
  LAT := LAT * PI / 180.0;
  for i := 1 to m do
  begin
    sin_tau[i] := sin(23.5 * Pi / 180) * sin(Pi * (i - 80) / 180);
    cos1       := sqr(sin_tau[i]);
    cos_tau[i] := sqrt(1 - cos1);
    cos_hs[i]  := -(sin(lat) / cos(lat)) * (sin_tau[i] / cos_tau[i]);
    if cos_hs[i] > 1 then
      cos_hs[i] := 1;
    if cos_hs[i] < -1 then
      cos_hs[i] := -0.9;
    if sqr(cos_hs[i]) < 1 then
    begin
      sin_hs[i] := sqrt(1 - cos_hs[i] * cos_hs[i]);
      Dl[i]     := arccos(cos_hs[i]) * sin_tau[i] * sin(lat) + sin_hs[i] * cos_tau[i] * cos(Lat);
    end
    else
      dl[i] := 0;
  end;
  s := dl[1];
  for i := 2 to m do
    s     := max(s, dl[i]);
  for i   := 1 to m do
    dl[i] := dl[i] / s;
end;

//пока не исправлю ошибку эта процедура только для влажности
procedure TFUNC_pro(x: real; a1: real; a2: real; a3: real; a4: real; var TFUNC: real);
begin
  IF (x <= a1) or (x >= a4) THEN
    TFUNC := 0.0;
  IF (x > a1) and (x <= a2) THEN
    TFUNC := (x - a1) / (a2 - a1);
  if (x > a2) and (x <= a3) THEN
    TFUNC := 1;
  if (x > a3) and (x < a4) THEN
    TFUNC := (a4 - x) / (a4 - a3);
  IF (TFUNC <= 0) then
    TFUNC := 0.0;
  if (TFUNC > 1) then
    TFUNC := 1.0;
end;

function Min1(a: real; b: real): real;
begin
  if a < b then
    min1 := a
  else
    min1 := b;
end;


//------------------ GRRT50----------------------


procedure RATE(J: integer; B: real20; NU, N0: integer;
  K: real10; var VP: real; var VMIN: real);
var
  Sk, Sc, b6: real;
begin
  VMIN := B[9] * EXP(B[8] * j * b[5]);
  IF (K[7] = 1) THEN
  begin
    Sk := (B[18] - B[19]) / (K[5] - K[6]);
    Sc := B[19] - K[6] * sk;
    b6 := sk * B[16] + sc;
    IF (b6 > B[18]) then
      b6 := B[18];
    IF (b6 < B[19]) then
      b6 := B[19];
    VP := b6 * j * b[5] + B[7];
  End
  ELSE
    VP := B[6] * j * b[5] + B[7];
end;


procedure grrt50(TEM: array366; PRE: array366; Nu: integer;
  M: integer; k: integer20; p: array30;
  pc: real20; KC: real10; dl: array366;
  filex, result_file: string; year: integer;
  //год для которого производятся расчеты
  grrt_check: integer; n_beg, n_end: integer;
  total_precin: real; pinim_in: real;
  //влажность почвы в первый день с момента начала оттаивания почвы
  var gr: array366; var sm: array366;
  var gr_middle: real; var MB: INTEGER;
  var Beg_gr1, end_gr1, Beg_gr2, end_gr2: integer;
  //начало сезона роста по суммарной температуре
  var pinim: real;
  // влажность почвы когда блок оттаивания не включен
  var pinim_out: real;
  //влажность почвы в последий день с момента начала замерзания почвы
  var growth_rate: real;
  //критическая скорость в день когда суммарная температура больше критической
  var total_precout: real;
  //суммарная влажность с начала года по sm_dep - день
  var begdep, enddep: integer;
  //номер дня когда глубина корней не отрицательная
  var temtmin, temtbeg: real;
  //temtmin суммарная температура в день когда tem[i]>tmin; temtbeg температура когда total_temp>tbeg
  var daytmin: integer //день когда tem[i]>tmin
  );
var
  dep, grtemp, vw, snow: array [1..366] of real;
  season:          array [1..10] of INTEGER;
  //MB:INTEGER;
  sumtemp:         real;
  FILE1:           string;
  i, j, MM, FNU, id, FI: integer;
  trw, trd, ddep, prrain, prsnow, xmelt: real;
  f1:              text;
  frost:           integer;
  //new
  m1, m2, z1, z2:  boolean;
  //check
  check_str:       string;
  i_index:         integer;
  gtT_file, GrW_file: string;
  Grt_f, GrW_f:    textfile;
  smoist, drenage, moisture, sm_double, gr_double: array366; //проверка дренажа
  //critical growth rate
  BG_check, EG_check: integer;
  vp:{array [1..366] of }real;
  VMIN:            real;
  vcr:             real;
  v:               array [1..366] of real;
  growth_season:   integer;
  w, tr1, trdep:   array366;
  //кумулятивная скорость (суммарная скорость за все предудыщие дни)
  cumulitive_rate: array366;
  real_moisture, K6, tr, trt, xm: array366;
  // суммарная температура за 10 дней
  Total_temp:      array366;
  //количество осадков и расчет накопившейся влажности к началу сезона роста 07.07.15
  total_prec:      real;// суммарное количество осадков к моменту оттаивания прочвы
begin
  temtmin   := 0;
  temtbeg   := 0;
  daytmin   := 0;
  m1        := false;
  m2        := false;
  z1        := false;
  z2        := false;
  frost     := 0;
  gr_middle := 0;
  for i := 1 to 366 do
    TEM[i] := TEM[i] + p[6];
  K[16] := 0;
  MM := 0;        {! begin of melting}
  IF (K[1] = 0) THEN
    for i := 1 to M do
      dep[i] := P[13]
  ELSE
  begin
    i := 1;
    sumtemp := 0;
    WHILE (sumtemp < P[7]) DO
    begin
      sumtemp := 0.0;
      for j := i to (i + 10{K[10]}) DO
        sumtemp := sumtemp + TEM[j];
      MM := i + 10{K[10]};
      i := i + 1;
      if sumtemp < p[7] then
        sumtemp := 0.0;
    END;
    for i := 1 to (MM) do
      dep[i] := 0;
    for i    := MM + 1 to M do
    begin
      dep[i] := dep[i - 1] + P[8] * TEM[i - 1] * EXP((-1) * P[9] * dep[i - 1]);
      IF (dep[i] <= 0) then
        dep[i] := 0;
      if (Dep[i] > p[13]) then
        dep[i] := P[13];
      //  if i>n_end then dep[i]:=0;
    end;
  END;

  //begin-end of growth season
  Beg_gr1 := 0;
  Beg_gr2 := 0;
  End_gr1 := 0;
  End_gr2 := 0;
  vcr     := (pc[1]);
  //begin-end of growth season

  //суммарная температура за 10 дней
  for i := 1 to m do
    Total_temp[i] := 0;
  for i           := 10 to m do
    for j := i - 9 to i do
      Total_temp[i] := Total_temp[i] + tem[j];
  for i := 1 to m do
    if (tem[i] > p[1]) and (daytmin = 0) then
    begin
      temtmin := Total_temp[i];
      daytmin := i;
    end;


  //суммарная влажность 06.08.15
  begdep := 0;
  enddep := 0;
  for i := 1 to m do
    if (begdep = 0) and (total_temp[i] > p[22]) and (tem[i] > p[1]) then
      begdep := i;
  Beg_gr1 := begdep; //день начала сезона роста 06.08.15
  temtbeg := total_temp[Beg_gr1];
  total_prec := total_precin;
  {for i:=1 to (begdep-1) do total_prec:=total_prec+pre[i];
  pre[begdep-1]:=total_prec*0.6;
  total_precout:=0; }
  //суммарная влажность 07.07.15


  //обнуление
  for i := 1 to 366 do
    moisture[i] := 0;
  for i         := 1 to M do
    TFUNC_pro(TEM[i], p[1], p[2], p[3], p[4], grtemp[i]);
  //влажность в первый день когда почва начала оттаивать, если включен блок оттаивания, если нет, то принимается значение с бегунка
  moisture[1] := p[10];
  if moisture[1] < p[10] then
    moisture[1] := p[10];
  snow[1] := p[23]; // начальное значение объема снежного покрова
  for i := 2 to m do
    snow[i] := 0;
  //влажность в первый день когда почва начала оттаивать, если включен блок оттаивания


  for i := 1 to (M - 1) do
  begin
    TFUNC_pro(moisture[i], p[17], p[18], p[19], p[5], trw);
    //если убрать то значения не сохраняются
    trd := 1;
    if trw > 1 then
      trw := 1;
    IF (k[1] = 1) then
      trd := dep[i] / P[13];
    sm_double[i] := trw * trd;
    dl[i] := dl[i] * p[31]; //модификация скорости от иррадиации
    gr_double[i] := (dl[i] * MIN1(grtemp[i], sm_double[i]));

    //определение конца сезона роста
    //   if (i>(begdep+14)) and (vcr>gr_double[i]) and (vcr>gr_double[i-1]) and (enddep=0) then begin enddep:=i-1; End_gr1:=i-1; end;

    tr[i] := gr_double[i] * P[14] * EXP(TEM[i] * P[15]);
    //транспирация воды кроной дерева
    //gr_double[i]
    IF (K[4] = 0) THEN
    begin
      xmelt  := 0;
      prrain := PRE[i];
    end
    ELSE
    begin
      IF (TEM[i] <= P[25]) THEN
      begin
        prsnow := PRE[i];
        prrain := 0.0;
        xmelt  := 0;
      end
      ELSE
      begin
        prrain := PRE[i];
        prsnow := 0.0;
        xmelt  := 1 * (TEM[i] - P[25]);
      end;
      snow[i + 1] := snow[i] - xmelt + prsnow;
      IF (snow[i + 1] <= 0) THEN
      begin
        snow[i + 1] := 0;
        xmelt       := snow[i] + prsnow;
        IF (K[16] = 0) then
          K[16] := i;
      end;
      K[4] := 1; //оттаивание почвы
    end;
    {available precipitation}
    K6[i] := MIN1(P[16] * pre[i], P[11]);
    xm[i] := MIN1(xmelt, P[11]);

    smoist[i] := (dep[i] * moisture[i] * (1 - P[21]) + K6[i] - tr[i] + xm[i]);// без изменений
    IF (smoist[i] <= 0) then
      smoist[i] := 0;
    //08.07
    if k[1] = 1 then
      if (dep[i + 1] > 0) and (dep[i] > 0) then
        moisture[i + 1] := smoist[i] / dep[i + 1]
      else
        moisture[i + 1] := moisture[i];
    if k[1] = 0 then
      moisture[i + 1] := smoist[i] / dep[i + 1];

    IF (moisture[i + 1] >= P[5]) then
      moisture[i + 1] := P[5];
    IF (moisture[i + 1] <= P[17]) then
      moisture[i + 1] := P[17];
    //минимальное занчение не может быть меньше W1
  end;
  //  IF(k[1]=1)and (dep[m]=0) then moisture[m]:=moisture[m-1];
  TFUNC_pro(moisture[m], p[17], p[18], p[19], p[5], sm_double[m]);
  if sm_double[m] > 1 then
    sm_double[m] := 1;
  if (m = 366) or (m = 365) then
    sm_double[m] := sm_double[m - 1];
  gr_double[m] := (dl[m] * MIN1(grtemp[m], sm_double[m]));
  pinim := moisture[m];
  p_res[23] := snow[m]; //snow output
  K[11]     := MB;
  K[17]     := MM;
  m1        := false;
  m2        := false;
  z1        := false; //what is this?
  z2        := false;
  {//output files}
  FNU       := NU;


  //расчет кумулятивной скорости
  for i := 1 to m do
    cumulitive_rate[i] := 0;
  for i                := 1 to m do
    for j := 1 to i do
      cumulitive_rate[i] := cumulitive_rate[i] + gr_double[j];

  id := Length(filex);
  if grrt_check = 0 then
  begin
    assign(F1, result_file);
    rewrite(f1);
    WRITEln(f1, 'year   t    Tem     Prec    sm       k6    tran   Dep    snow    Gr     Grw         GrT   Solar   Cumul  Total_temp  Tbeg');
  end
  else
  begin
    assign(F1, result_file);
    append(f1);
  end;
  for i := 1 to M do
  begin
    FI := NU + i;
    WRITE(f1, year: 4, (NU + i): 4, ' ', TEM[i]: 8: 2, PRE[i]: 8: 2, moisture[i]: 8: 4, k6[i]: 8: 2,
      tr[i]: 6: 2, dep[i]: 8: 2, snow[i]: 6: 2, gr_double[i]: 8: 4);
    writeln(f1, ' ', sm_double[i]: 8: 4, ' ', grtemp[i]: 8: 2, ' ', dl[i]: 6: 2,
      cumulitive_rate[i]: 8: 2, Total_temp[i]: 8: 2, P[22]: 10: 0);
  end;
  i_index := 0;


  //начало и конец сезона роста находится в цикле
  for i := 1 to m do
    v[i] := gr_double[i] * (pc[4] + 0.001);
  i := 1;   // работает!
  for i := 1 to m do
  begin
    //if (total_temp[i]>p[22]) and(tem[i]>p[1]) then Beg_gr1:=i;
    if (i > (Beg_gr1 + 14)) and (vcr > gr_double[i]) and (vcr > gr_double[i - 1]) and (End_gr1 = 0) then
      End_gr1 := i - 1;
    //bimodal growth season
    if (beg_gr2 = 0) and (end_gr1 <> 0) and (i > (end_gr1 + 14)) {and (gr_double[i]>vcr)} and
      (total_temp[i] > p[22]) and (tem[i] > p[1]) then
      Beg_gr2 := i;
    // end_gr1 - end of first growth period; growth rate higher than critical rate, and total temperatures higher
    if (beg_gr2 = M) or (beg_gr2 + 14 >= M) then
      beg_gr2 := 0;
    if (Beg_gr2 <> 0) and (i > (Beg_gr2 + 10)) and (vcr > gr_double[i]) and (vcr > gr_double[i - 1]) and
      (End_gr2 = 0) then
      End_gr2 := i - 1;
    if (Beg_gr2 <> 0) and (End_gr2 = 0) and (i = M - 1) then
      End_gr2 := Beg_gr2 + 14;
    If (Beg_gr2 <> 0) and (Beg_gr2 + 14 > M) then
      end_gr2 := M;
    if (Beg_gr2 <> 0) then
      enddep := Beg_gr2;
  end;
  {while (Beg_gr1=0)  do
  if (P[22]<Total_temp[i])  and (Beg_gr1=0) and(tem[i]>p[1]) then begin Beg_gr1:=i; growth_rate:=gr_double[i] end else i:=i+1;
  i:=beg_gr1+1;
  while (end_gr1=0) and (i<(m-21)) do
  if  (vcr>v[i]) and (vcr>v[i+1]) and (vcr>v[i+2]) and (vcr>v[i+15]) and (END_gr1=0) then begin End_gr1:=i+1; end else i:=i+1;
  i:=end_gr1;
  while (beg_gr2=0) and (i<(m-21)) do
  if (P[22]<Total_temp[i]) and (Beg_gr2=0) and (tem[i]>p[1]) then begin Beg_gr2:=i; End_gr2:=0; i:=beg_gr2+1; end else i:=i+1;
  if (beg_gr2<>0) then
  while (end_gr2=0) do
  begin
  if (i<m) and (vcr>v[i]) and (vcr>v[i+1]) and (vcr>v[i+2]) and (vcr>v[i+15]) and (END_gr2=0) then  begin End_gr2:=i; end else i:=i+1;
  if i=m then end_gr2:=m;
  end;
  if ((Beg_gr2-end_gr1)<14) and (Beg_gr2>0) then
  begin
   end_gr1:=end_gr2;
   Beg_gr2:=0;
   End_gr2:=0;
  end; }

  //суммарная влажность от конца сезона роста до конца года
 { total_precout:=0;
  if end_gr2=0 then enddep:=end_gr1 else enddep:=end_gr2;
  pinim_out:=moisture[enddep];
  for i:= enddep to m do  total_precout:=total_precout+pre[i];}


  for i := 1 to m do
    //если интегральная скорость больше критической увеличенной!
    if ((i >= beg_gr1) and (i <= end_gr1)) or ((i >= beg_gr2) and (i <= end_gr2) and (beg_gr2 <> 0)) then
    begin
      gr_middle := gr_middle + gr_double[i];
      i_index   := i_index + 1;
    end;
  //  сумма всех скоростей за год
  if i_index <> 0 then
    gr_middle := gr_middle{/growth_season}
  else //деление на количество элементов
    gr_middle := PC1_TB;
  growth_rate := gr_double[Beg_gr1];
  //интегральная скорость роста в начало сезона роста
  closefile(f1);
end;
//------------------ GRRT50----------------------

{-------------procedure CAMB50---------------------}


Procedure CAMB50(VEXT: array366; NU: integer;
  FILEX, file_ini: string; NVC, KTIME: integer;
  B: real20; K: real10; year: integer;
  check_c, check_p: integer; var NRING: integer;
  var NR: rmax_int; var TD: rmax_real;
  var SI: rmax_real);
Var
  AT, RT, TG:    array [1..RMAX] of real;
  Size:          array [1..RMAX] of real;
  NRD, NC:       array [1..RMAX] of integer;
  KP:            array [1..RMAX] of integer;
  JDIV:          array [1..RMAX, 1..20] of integer;
  {имена файлов}
  FY, FILE1, FP: string;
  div1:          array [1..200] of string;
  v0:            real;
  SIG1, SIG2, SIs, SIm, t0: real;
  a:             textfile;
  j:             integer;
  n1, n0:        integer;
  t, it:         real;
  NDIV, K0, K00: integer;
  check120, check130, check131, check140, check150, check155: boolean;
  NDT0, NDIF:    integer;
  VP:            real;
  VMIN:          real;
  VS:            real;
  v:             real;
  l1, l2, idd:   integer;
  z:             real;
  {undeclaired identifiers}
  i:             integer;
  s:             real;
  f1, f2:        textfile;
  id:            integer;
  {more for tracking - after check DELETE}
  nring_str:     string;
BEGIN    //1
  id := length(Filex);
  {stack owerflow,check in delphi changed stack on 20150 }
  If K[1] >= 1 THEN
    FOR I := 1 TO KTIME DO
    BEGIN   //3
      S       := 3.14 / KTIME;
      VEXT[I] := SQR((SIN(S * I)));
      if (k[1] = 2) then
        vext[i] := b[3];
    END//2
    //3
  ; //2
  V0   := B[10];
  SIG1 := B[11];
  SIS  := B[12];
  SIG2 := B[13];
  SIM  := B[14];
  T0   := B[15];
  {//C-----------------------initial conditions}
  For i := 1 to rmax do
  Begin   //4
    For j := 1 to 20 do
      JDIV[I, J] := 0;
    SI[I] := 0;
    AT[I] := 0;
    RT[I]   := 0;
    DIV1[I] := 'N';
    KP[I]   := 0;
    NR[I]   := 0;
    NC[I]   := 0;
    TD[I]   := 0;
  End;  //4
  assignfile(a, file_ini); //условие на случай если файла нет
{$I-}
  reset(a);
{$I+}
  if IOResult = 0 then
  begin    //5
    read(a, N1);
    for i := 1 to n1 do
      Readln(a, si[i], DIV1[i]);
    closefile(a);
    NVC := NVC + 1;
  end
  else  //5
  begin  //6
    N1      := 1;
    ID      := 20;
    DIV1[1] := 'Y';
    SI[1]   := SIM / 2;
    rewrite(a);
    closefile(a);
  end; //6
  //Closefile(a);    закрытие файла после записи
  T        := 1;
  NRING    := N1;
  NDIV     := 0;
  K0       := 1;
  K00      := 1;
  {160}
  TD[K0]   := T;
  NC[K0]   := N1;
  NR[K0]   := NRING - NC[K0];
  K0       := K0 + 1;
  TG[k00]  := T;
  NRD[k00] := NRING - N1;
  k00      := k00 + 1;
  {160}

  check130 := true; {check}
  check131 := false;
  check140 := false;
  check150 := false;
  check155 := false;

  {130}
  while check130 = true do
  begin  //7
    check130 := false;
    N0       := N1;
    IF (K0 > RMAX) then
      check131 := true;
    if check131 = false then
    begin  //8
      T  := T + T0;
      IT := T;
      IF (IT <= KTIME) then
        check140 := true;
    end;  //8
    if check140 = false then
      check131 := true;
    {130}
    if check140 = true then
    begin   //9
      check140 := false;
      NDT0     := 0;
      NDIF     := 0;
      J        := 1;
      {120}
      check120 := true;
      while check120 = true do
      begin  //10
        check120 := false;
        if (IT < K[9] - NU) then
          check130 := true;
        if check130 = false then
        begin  //11
          IF (SI[J] < 0.1) then
            check155 := true;
          if check155 = false then
          begin  //12
            if (DIV1[J] = 'N') then
              check150 := true;
            if check150 = false then
            begin  //13
              RATE(J, B, NU, N0, K, VP, VMIN{,VS});
              vs := b[1];
              V  := VP * VEXT[trunc(IT)] * b[4];
              //пропущенный фрагмент         проверить begin\end:

              if (SI[j] <= SIG1) then    //cell in phase G1
                if (V < VS) then
                  check150 := true  //cell in rest - next j
                else
                begin //15
                  if (v < Vmin) and (j > 2) then   //cell in position j is differentiated
                  begin  //16
                    div1[j] := 'N';
                    RT[J]   := T;
                    NDIF    := 1;
                  end   //16                     //cell growth
                  ELSE
                  BEGIN   //17
                    SI[J] := SI[J] + V * T0;
                    AT[J] := AT[J] + T0;
                  end;   //17
                  check150 := true;
                end//14
                //15
              ; //14
              if check150 = false then         //S, G2 or M phase
              begin  //18
                AT[J] := AT[J] + T0;
                SI[J] := SI[J] + V0 * T0;
                IF SI[J] <= SIM THEN
                  Check150 := true;
              end;  //18
              if check150 = false then            //division in pozition J
              begin  //19
                NDT0  := NDT0 + 1;
                NDIV  := NDIV + 1;
                NRING := NRING + 1;
                JDIV[K0, NDT0] := J - NDT0 + 1;
                FOR I := RMAX DOWNTO (J + 2) DO
                BEGIN   //20
                  AT[I] := AT[I - 1];
                  RT[I] := RT[I - 1];
                  DIV1[I] := DIV1[I - 1];
                  SI[I] := SI[I - 1];
                  KP[I] := KP[I - 1];
                END;  //20
                //position J
                RT[J] := 0.0;
                SI[J] := SI[J] / 2;
                Kp[J] := Kp[J] + 1;
                J     := J + 1;
                SI[J] := SI[J - 1];
                RT[J] := RT[J - 1];
                AT[J] := AT[J - 1];
                KP[J] := KP[J - 1];
                DIV1[J] := DIV1[J - 1];
                IF (J = 2) THEN
                BEGIN //21
                  AT[J] := 0.0;
                  KP[J] := 1;
                END;    //21
              end;  //19
              //пропущенный фрагмент
              //  end; //13
            end; {check150}     //12
            j := j + 1;
            IF (J <= NRING) then
              check120 := true;
          end; {check155}
          if check120 = false then
          begin //22
            TD[K0]   := T;
            NC[K0]   := N1;
            NR[K0]   := NRING - NC[K0];
            K0       := K0 + 1;
            TG[k00]  := T;
            NRD[k00] := NRING - N1;
            k00      := k00 + 1;
          end;  //22
        end; {if check130=false}
      end; {while check120}
    end; {140}
  end; {while check130}
  {130}
  {131}
  K0    := K0 - 1;
  N0    := NRING;
  //command that makes nring = 0
  // NRING:=NRING-N1;
  {Atention______________}
  K[9]  := RT[N0]; {different types k-integer, rt real}
  K[10] := RT[N1 + 1];
  {Atention}
  IF (NVC = 1) OR (NVC = 0) THEN {do we need this file?}
  begin
    file1 := file_ini;
    assign(f1, file1);
    rewrite(f1);
    writeln(f1, N1);
    for i := 1 to n1 do
      writeln(f1, SI[I]: 2: 2, ' ', DIV1[I]);
    CLOSE(f1);
  ENd;
  {Atention}
  IF (K[3] = 1) THEN
  begin
    file1 := copy(filex, 1, id - 6) + 't.tre';
    if check_t = 0 then
    begin
      assign(f2, file1);
      rewrite(f2);
      writeln(f2, K0, ' ', NU, ' ', NDIV);
    end
    else
    begin
      assignfile(f2, filex);
      rewrite(f2);
    end;
    for i := 1 to K0 do
      writeln(f2, TD[I]: 2: 2, ' ', NC[I]);
    for I := 1 to K0 do
      for j := 1 to 20 do
        write(f2, JDIV[I, J], ' ');
    CLOSE(f2);
  END;
  FILE1 := copy(FILEX, 1, id - 4) + ' c.dat';
  if check_c = 0 then
  begin
    assign(f2, file1);
    rewrite(f2);
    WRITEln(f2, 'year   t    ', '  Nc', '  Nr');
  end
  else
  begin
    assignfile(f2, file1);
    append(f2);
  end;
  for i := 1 to K0 DO
    WRITEln(f2, year: 4, '  ', (TD[i] + NU): 6: 2, ' ', NC[i]: 3, '  ', NR[i]: 3);       {format of output data}
  CLOSE(f2);

  for j := N1 + 1 to N0 do
  begin
    s  := 0;
    L1 := Trunc(AT[j]);
    IF (L1 = 0) then
      l1 := 1;
    L2 := Trunc(RT[j]);
    for i := L2 - L1 to L2 do
      s := s + VEXT[i];
    s := s / L1;
    size[j] := s;
  END;
  for j := 1 to N1 DO
    size[j] := 0.0;

  IF (K[2] = 1) THEN
  begin
    FP := copy(FILEX, 1, id - 4) + ' P.dat';
    if check_p = 0 then
    begin
      assignfile(F2, FP);
      rewrite(f2);
      WRITEln(f2, 'year j  ', ' Kp  ', '  Rt    ', '  At    ', '  At/Kp ', '  Dj    ', 'idd', '  size');
    end
    else
    begin
      assignfile(F2, FP);
      append(f2);
    end;
    for I := 1 to N0 DO
    begin
      IDD := 0;
      IF (DIV1[I] = 'Y') then
        IDD := 1;
      z := 0;
      IF (I >= 2) AND (KP[I] <> 0) then
        z := AT[I] / KP[I];
      WRITEln(f2, year, ' ', I, '   ', KP[I], '    ', RT[I]: 2: 2, '     ', AT[I]: 2: 2, '    ',
        z: 2: 2, '    ', SI[I]: 2: 2, '   ', IDD, '   ', (size[i] * 45.0 + 5.0): 2: 2);    {check format}
    END;
    CLOSE(f2);
  END;
  begin
    j       := N0 + N1 - (i - 1);
    size[j] := 0.0;
  end;
end;

{-------------procedure CAMB50---------------------}

{------------procedure CSCAL50-------------}
//not sure, delphi already has this function
function max1(a, b: integer): integer;
begin
  if a < b then
    max1 := b
  else
    max1 := a;
end;

procedure RTMR(year: integer; n: integer; fname: string;
  ne: integer;
  // byear:integer; begyear
  var rd, rw: rmax_real);
{n изменяется в конце процедуры, после процедуры мы испоьзуем первоначальное n или полученное?}
var
  byear:  integer;
  nf:     array [1..50] of integer;
  fmt:    string;
  ntrach: integer;
  k:      integer;
  {proc}
  f:      text{file};
  check6: boolean;
  i:      integer;
begin
  check6 := false;
  ne     := 0;
  inp(fname, f);
  readln(f, byear, ntrach);
  i := 0;
  for i := 1 to ntrach do
  begin
    read(f, nf[i]);
    if eoln(f) then
      readln(f);
  end;
  while i < ntrach do
  begin
    i := i + 1;
    read(f, nf[i]);
  end;
  k := year - byear + 1;
  IF (k > ntrach) THEN
  begin
    ne     := 0;
    check6 := true;
  END;
  if check6 = false then
    IF (k <= 0) THEN
    begin
      ne     := 0;
      check6 := true;
    END;
  if check6 = false then
  begin
    ne := 1;
    for i := 1 to nf[k] do
    begin
      read(f, rd[i], rw[i]);
      if eoln(f) then
        readln(f);
    end;
    n := nf[k];
  end;
  closefile(f);
end;

procedure nor(n: integer; trach: rmax_real; nn: integer);
var
  t:          array [1..200] of real;
  s:          real;
  l1, l2, ii: integer;
  k, i:       integer;
begin
  for i := 1 to nn DO
  begin
    s  := 0;
    l1 := (i - 1) * n + 1;
    l2 := i * n;
    for ii := l1 to l2 DO
    begin
      k := ((ii - 1) div nn) + 1;
      s := s + trach[k];
    END;
    t[i] := s / n;     {! correction of trw (NR(j)}
  END;
  for i := 1 to nn do
    trach[i] := t[i];
end;

procedure CSCALC50(ps: real10; ks: integer10; a: array366;
  m1, m2: integer; nc: integer;
  fncli, fndata: string; nr: integer;
  trwr: real; cd, rd: rmax_real;
  check_s: integer{array200}
  //endyear
  );
var
  rw, t:     rmax_real;
  dav:       real;
  ne, id, id4, i, year: integer;
  check12:   boolean;
  YEAR_str, filecli: string;
  code:      integer;
  n:         integer;
  check110, check112, check_TI, check_v: boolean;
  v:         real;
  MMM:       integer;
  is1:       real;
  f:         text;
  s:{integer}real;
  j:         integer;
  trwc:      real;
  dd:        real;
  nn:        integer;
  {new}
  s1:        real;
begin
  id := Length(fncli); {check}
  {conditions to begin calculation M1<M2}
  nr := 1;
  ne := 0;
  IF (M2 <= M1) then
    check12 := True;
  If check12 = false then
  begin
    for i := 1 to 200 do
    begin
      rd[i] := 0;
      rw[i] := 0;
    END;
    {last 4 numbers in number of year}
    id4      := length(fncli);
    YEAR_str := copy(fncli, id4 - 3, 4);
    val(year_str, year, code);
    RTMR(year, nr, fndata, ne, rd, rw);
    dav := ps[7];
    IF (ne > 0) AND (NR >= 2) THEN
    begin
      trwr := 0;
      for i := 1 to nr do
        trwr := trwr + rd[i];
      IF (NR = 0) then
        NR := 0;{значение}
      dav := trwr / nr;
    end;
  end;
  ///
  check112 := true;  //просто проверка, в Lazarus boolean поумолчанию false
  ///
  n        := nc;
  IF (n < 2) then
    check112 := true;
  if check112 = false then
    IF (ks[3] = 0) THEN
    begin
      {c ------- calculation cell size (theoretical)--------}
      V              := 0;
      { S1:=0.1;} s1 := 1;
      MMM            := (M2 - M1) * 9;
      FOr I := 1 to MMM do
      begin
        IS1 := M1 + S1 * I;
        V   := V + A[trunc(IS1)] * 0.1; {type of s1}
      END;
      {dialog windows with result}
      IF (N = 0) then
        showmessage('N=0 should stop')
      else {dialog window b delphi!!}
      begin
        V := V / N;
        i := -1;
        s := 0;
        for j := 1 to n do
        begin
          check110 := true;
          while check110 = true do
          begin
            i        := i + 1;
            check110 := false;
            if (i <= MMM) then
            begin
              is1 := m1 + trunc(s1) * i;
              s   := s + A[trunc(IS1)] * 0.1; {attention different types}
              //перепишем формулу, оригинал выше
              //s:=s+A[IS1]*0.1;
              {Atention check type s1 should be real not ineger}
            end;
            if (s < V * j) then
              check110 := true;
            if check110 = false then
              t[j] := S1 * I;
          end;
        end;
        for i := n downto 2 do
        begin
          t[i] := t[i] - t[i - 1];
          IF (t[i] = 0) then
            t[i] := S1;
        END;
      end;
      if (t[1] <> 0) then
      begin
        v        := 1.0 / t[1];
        check_TI := false;
        for i := 2 to n do
        begin
          if (t[i] = 0) then
            check_TI := true;
          if check_TI = false then
            v := v + 1.0 / t[1];
        end;
        if check_TI = false then
        begin
          trwc := 0.0;
          for i := 1 to n do
          begin
            if (v = 0) then
              check_v := true;
            if (t[i] = 0) then
              check_TI := true;
            if (check_v = false) and (check_TI = false) then
            begin
              cd[i] := 7.0 + (dav - 7.0) * n / (v * t[i]);
              trwc  := trwc + rd[i];
            end;
          end;
        end;
      end
      else
      begin
        s := 0;
        for i := 1 to n do
          s := s + trunc(cd[i]); {or change s - type or trunc}
        dd := (dav - 7.0) / s * n;
        for i := 1 to n do
          cd[i] := cd[i] * dd + 7.0;
      END;
    end//переход после  112
  ;

  fncli := copy(fncli, 1, id - 4){+copy(fncli,id-1,id)} + ' s.dat';
  if check_s = 0 then
  begin
    assign(f, fncli);
    rewrite(f);
    writeln(f, 'i  rd  cd');
  end
  else
  begin
    assignfile(f, fncli);
    append(f);
  end;
  if (ne = 1) and (ks[2] = 1) then
  begin
    NOR(nr, rd, nc);
    nn := nc;
  end
  else
    nn  := MAX1(n, nr);
  for i := 1 to nn do
    writeln(f, i, ' ', rd[i]: 2: 2, '  ', cd[i]: 2: 2);
  close(f);
  //end;
end; //Statement expected but 'PROCEDURE' found
{------------------------------------------}

//-------reading---------------
procedure reading(namefile: string; begyear, endyear: integer; var year: integer100;
  var gr_result: real100);
var
  f:             textfile;
  i, j, k:       integer;
  year1:         integer100;
  gr_result1:    real100;
  number1:       integer;
  //more
  id, code:      integer;
  path, pathcli: string;
begin
  number1 := 0;
  assignfile(f, namefile);
  reset(f);
  i := 0;
  while not eof(f) do
  begin
    i := i + 1;
    readln(f, year[i], gr_result[i]);
  end;
  closefile(f);
  year1[1] := begyear;
  for j := 1 to i do
    if year[j] = year1[1] then
    begin
      gr_result1[1] := gr_result[j];
      for k := 2 to i - j + 1 do
      begin
        year1[k]      := year[j + k - 1];
        gr_result1[k] := gr_result[j + k - 1];
      end;
    end;
  for i := 1 to k do
    if year1[i] = endyear then
      for j := 1 to i do
      begin
        year[j]      := year1[j];
        gr_result[j] := gr_result1[j];
        number1      := number1 + 1;
      end;
end;
//-------reading--------------

//-----middle growth rate-----
procedure middle(n: integer; //number rate
  gr_middle: real100;// sum year growth rate
  mean_in: real; var another_gr_middle: real100;
  var mean_out: real);
var
  gr_y:   real;
  growth: real100;
  i:      integer;
  //проверка
  f:      textfile;
begin
  gr_y := 0;

  for i := 1 to n do
    another_gr_middle[i] := 0;
  //условие для деления на среднее значение от предыдущих расчетов
  if mean_rates = 0 then
  begin
    for i := 1 to n do
      gr_y := gr_y + gr_middle[i];
    gr_y := gr_y / n;
    mean_out := gr_y;
  end;
  if mean_rates = 1 then
    gr_y := mean_in;
  for i  := 1 to n do
    another_gr_middle[i] := gr_middle[i] / gr_y;
end;
//-----middle growth rate-----


procedure deviation(a: real100; n: integer; var norm: real100);
var
  i:              integer;
  dev_sub{Subtraction}: real100;
  mean, sum, dev: real;
begin
  sum  := 0;
  mean := 0;
  for i := 1 to n do
    mean := mean + a[i];
  mean := mean / n;
  for i := 1 to n do
    dev_sub[i] := sqr(a[i] - mean);
  for i        := 1 to n do
    sum        := sum + dev_sub[i];
  dev := sqrt(sum / (n - 1));
  if dev = 0 then
    dev     := 0.001;
  for i     := 1 to n do
    norm[i] := (a[i] - mean) / dev;
end;

//-------------------

//-----------------CORRELATION---------------
procedure Correlation(n: integer; x, y: real100; var c_coef: real);
var
  x_mid, y_mid: real;
  x_1, y_1:     real;
  numerator:    real;
  i:            integer;
  correl, correl_str: string;
  //проверка
  f1, f2:       textfile;
begin
  x_mid     := 0;
  y_mid     := 0;
  x_1       := 0;
  y_1       := 0;
  numerator := 0;
  for i := 1 to n do
    x_mid := x[i] + x_mid;
  x_mid := x_mid / n;
  for i := 1 to n do
    y_mid := y[i] + y_mid;
  y_mid := y_mid / n;
  x_1 := 0;
  y_1 := 0;
  for i := 1 to n do
  begin
    x_1 := x_1 + (x[i] - x_mid) * (x[i] - x_mid);
    y_1 := y_1 + (y[i] - y_mid) * (y[i] - y_mid);
  end;
  for i := 1 to n do
    numerator := numerator + (x[i] - x_mid) * (y[i] - y_mid);
  c_coef := numerator / sqrt(x_1 * y_1);
  //Application.MessageBox(PChar('Коэффициент корреляции: '+floattostr(c_coef)),
  // 'Внимание',1);
  correl := floattostr(c_coef);
  correl_str := copy(correl, 1, 5);
 (* /tmeits
 graphform.label1.caption:=correl_str{floattostr(c_coef)};
 if c_coef>old_coef then graphform.label1.font.color:=clgreen else graphform.label1.font.color:=clred;
 if c_coef=old_coef then graphform.label1.font.color:=clblack;
  tmeits/ *)
  old_coef   := c_coef;
end;
//-----------------CORRELATION---------------

//определение дней начала сезона роста (суммарная температура больше Tbeg) и конца (суммарная за 10 дней 0)
Procedure season_temp(tem: array366; m: integer; Tbeg: real; var n1, n2: integer);
Var
  I, j: integer;
  sum:  real;
Begin
  N1 := 0;
  N2 := 0;
  I  := 0;
  While n1 = 0 do
  Begin
    I   := i + 1;
    sum := 0;
    for j := I to (i + 10) do
      sum := sum + tem[j];
    if (sum > Tbeg) and (n1 = 0) then
      n1 := I + 9;
  end;
  if n1 < 346 then
    i := n1 + 20;
  if (n1 > 366) or (n1 < 0) then
    n1 := 366;
  i := n1;
  While n2 = 0 do
  Begin
    I   := i + 1;
    sum := 0;
    for j := I to (i + 10) do
      sum := sum + tem[j];
    if (sum < Tbeg) and (n2 = 0) then
      n2 := I;
  end;
  if n2 >= 366 then
    n2 := 366;
end;


Procedure grwdown(tem: array366; m: integer; var n1, n2: integer);
Var
  I, j:       integer;
  sum:        integer;
  above_zero: boolean;
Begin
  above_zero := true;
  N1         := 0;
  N2         := 0;
  I          := 0;
  sum        := 0;
  for i := 1 to (m - 14) do
    if n1 = 0 then
    begin
      for j := I to (i + 14) do
        if (tem[j] > 0.0) then
          sum := sum + 1;
      if sum = 15 then
        n1  := j
      else
        sum := 0;
    end;
  sum := 0;
  for i := n1 to (m - 14) do
    if n2 = 0 then
    begin
      for j := i to (i + 14) do
        if (tem[j] < 0.0) then
          sum := sum + 1;
      if sum = 15 then
        n2  := i
      else
        sum := 0;
    end;
  if n2 = 0 then
    n2 := m;
{ for i:=1 to m do
 if tem[i]<0 then above_zero:=false;
 if above_zero=true then begin n1:=1; n2:=m; end;}
end;


(* /growth *)
procedure growth(filename_1, path, filename_4: string;
  latitude: string; endyear_str: string;
  var Year_number: integer; var gr_ini: real100;
  var gr_middle: real100; var BEGYEAR: integer;
  var ENDYEAR: integer; var years {_ar}: integer100);
var
  TEM, PRE, GR, DL, SM, Tem1: array366; //array [1..366] of real;
  P:                array30;
  pc:               real20;
  PS, PINI:         real10;
  SIZER, SIZEC, TR: rmax_real;
  RINGC, RINGR, CRN: array [1..100] of real;
  rint:             array[1..100, 1..5] of real;
  NRING:            rmax_int;
  BGROWTH1, BGROWTH2, BMELT, BSNOW, CMB, EGROWTH1, EGROWTH2, BGT, EGT: array [1..100] of integer;
  BGW, EGW:         array [1..100] of integer;
  K:                integer20;
  KC, KS1, season:  real10;
  ks:               integer10;
  icycle:           integer;     //!
  LAT:              real;
  FILECLI, FILECLI1, FY, areaname: string;
  PATHCLI, FX, FC:  string;
  CLIMFILE, climfile1: string;
  FILECS, FILECRN:  string;
  PATHCS, PATHCRN:  string;
  HEAD:             string;
  // значение скорости когда начинается рост 02.07.2015
  vcr:              array [1..100] of real;
  growth_rate:      real;
  //FILT:string;
  TXT2:             string;
  i, perem, j:      integer;
  str:              string;
  code:             integer;
  begyear_str:      string;
  F1, F2, F3, F4, F5: textfile;  {F1=GRRT50.PAR, F2=CMB50.PAR, F3=CSC50.PAR, F4=COKU1.CRN, F5 = COKU.MTR}
  result_path, result_file, file_ini: string;
  result_file_rate: string; //имена файлов для записи
  grrt_check, check_c, check_s, check_p, check_t: integer;
  //счетчик действий в процедурах для зпписи файлов
  JJ:               integer;
  check100, check20: boolean;
  id, id1, id3, id4, id5: integer;
  ik, js, ig:       integer;
  //jj1:integer; //!
  x:                real;
  s, ss:            real;
  l, M, ERR, NU, Nreal: integer;
  folder:           string;
  //undeclared identifier
  nr:               integer;
  SL:               real;
  //год в который делались измерения
  years_ini:        integer100;//годы из файла с реальными данными
  //gr_ini:real100; //значения из файла с реальными данными
  dot:              integer;
  v, VP, VMIN:      real;//Начало и конец сезона
  BG_check:         integer;
  FILEDATA1:        string;
  T_beg:            integer;
  t_sum:            real;
  //год
  year, gr_ini_number: integer;
  year_str:         string;
  n_beg, n_end, z_beg, z_end: integer;
  file_exist:       boolean;
  //переменная для проверки пропуска года климатики
  pinim, pinim_in, pinim_out, total_precin, total_precout: real;
  piniin, piniout, precin, precout: array366;
  snow_out:         real; // начальное значение объема снежного покрова
  begdep, enddep:   integer;
  //начало и конец сезона оттаивания/замерзания почвы
  Dep_beg, Dep_end: array [1..100] of integer;
  temtmin, temtbeg: array [1..100] of real;
  //temtmin суммарная температура в день когда tem[i]>tmin; temtbeg температура когда total_temp>tbeg
  daytmin:          array [1..100] of integer; //день когда tem[i]>tmin
  temmin, tembeg:   real;
  daymin:           integer;
begin
  jj         := 1;
  grrt_check := 0;
  check_c    := 0;
  check_t    := 0;
  check_s    := 0;
  check_p    := 0;
  //сохраняем имена файлов, т.к. в mouseup процедура не работает с .text
  edit1text  := filename_1;
  edit2text  := path;
  edit5text  := filename_4;
  edit7text  := latitude;
  edit8text  := endyear_str;

  //creation directory for results
  result_path := copy(filename_1, 1, length(filename_1) - 4) + ' result\';
  ForceDirectories(result_path);

  //latitude
  dot := pos('.', Latitude);
  if dot <> 0 then
  begin
    DecimalSeparator := '.';
    lat              := strtofloat(Latitude);
    ;
  end
  else
    lat := strtofloat(Latitude);
  //file *.ini
  noof(filename_1, pathcli, filecli);
  file_ini := pathcli + 'cmb50.ini';
  noof(path, pathcli, filecli);


 {$I-}
  assignfile(F1, filename_1);
  reset(F1);
  for i := 1 to 2 do
    readln(F1);
  readln(F1, FILT);
  readln(f1);
  for i := 1 to 31 do
    readln(F1, P[i]);
  readln(f1);
  for i := 1 to 20 do
    readln(F1, K[i]);
  readln(f1);
  for i := 1 to 10 do
    readln(F1, KC[i]);
  readln(f1);
  for i := 1 to 20 do
    readln(F1, PC[i]);
  readln(F1);
  for i := 1 to 10 do
    readln(F1, PS[i]);
  readln(F1);
  for i := 1 to 10 do
    readln(F1, KS[i]);
 {$I+}
  If IOresult <> 0 then
    Application.MessageBox(PChar('Error reading! File of Parametres' + filename_1
      + ' check file please'),
      'Внимание', 1);
  if IOresult <> 0 then
    exit;
  closefile(f1);
 {$I-}

  PC[16]      := KC[8];
  FILECLI1    := FILECLI;
  id          := length(pathcli);
  id4         := length(filecli);
  id1         := length(pathcs);
  id3         := length(filecs);
  BEGYEAR_str := copy(filecli, id4 - 3, 4);
  val(begyear_str, begyear, code);

  endyear  := strtoint(endyear_str); //attention
  ID5      := length(PATHCRN);
  areaname := copy(filecli, 1, id4 - 4); //имя участка TURA, COKU, ALGI


  jj1         := endyear - begyear + 1;
  icycle      := 1;
  filecli     := filecli1;
  js          := 1;
  ig          := begyear;
  check20     := true;
  Year_number := 0;
  reading(filename_4, BEGYEAR, ENDYEAR, years_ini, gr_ini);


  // здесь менятся данные вручную
  if p1_TB_l = True then
    p[1] := p1_TB;
  if p2_TB_l = True then
    P[2] := p2_TB;
  if p3_TB_l = True then
    P[3] := p3_TB;
  if p4_TB_l = True then
    P[4] := p4_TB;
  if p5_TB_l = True then
    P[5] := p5_TB;
  if p7_TB_l = True then
    P[7] := p7_TB;
  if p8_TB_l = True then
    P[8] := p8_TB;
  if p9_TB_l = True then
    P[9] := p9_TB;
  if P10_TB_l = True then
    P[10] := P10_TB;
  if p12_TB_l = True then
    P[12] := p12_TB;
  if p13_TB_l = True then
    P[13] := p13_TB;
  if p14_TB_l = True then
    P[14] := p14_TB;
  if p15_TB_l = True then
    P[15] := p15_TB;
  if p16_TB_l = True then
    P[16] := p16_TB;
  if p17_TB_l = True then
    P[17] := p17_TB;
  if p18_TB_l = True then
    P[18] := p18_TB;
  if p19_TB_l = True then
    P[19] := p19_TB;
  if p22_TB_l = True then
    P[22] := p22_TB;
  if PC1_TB_l = true then
    PC[1] := PC1_TB;
  if PC6_TB_l = true then
    PC[6] := PC6_TB;
  if PC7_TB_l = true then
    PC[7] := PC7_TB;
  if PC8_TB_l = true then
    PC[8] := PC8_TB;
  if PC9_TB_l = true then
    PC[9] := PC9_TB;
  If SolarMod_TB_l = true then
    P[31] := P31_TB;
  k[1] := k1;
  //k[4]:=k1;

  p1_TB  := p[1];
  p2_TB  := p[2];
  p3_TB  := p[3];
  p4_TB  := p[4];
  p5_TB  := P[5];
  p7_TB  := P[7];
  p8_TB  := P[8];
  p9_TB  := P[9];
  P10_TB := P[10];
  p12_TB := P[12];
  p13_TB := P[13];
  p14_TB := P[14];
  p15_TB := P[15];
  p16_TB := P[16];
  p17_TB := P[17];
  p18_TB := P[18];
  p19_TB := P[19];
  p22_TB := P[22];
  PC1_TB := PC[1];
  PC6_TB := PC[6];
  PC7_TB := PC[7];
  PC8_TB := PC[8];
  PC9_TB := PC[9];
  P31_TB := P[31];

  //проверка, если минимальное значение больше максимального
  if p[1] > p[2] then
    p[2] := p[1] + 1;
  if p[2] > p[3] then
    p[3] := p[2] + 1;           // исправить в самих бегунках
  if p[3] > p[4] then
    p[4] := p[3] + 1;

  //сохранение данных для файла
  for i := 1 to 31 do
    P_res[i]  := p[i];
  for i       := 1 to 20 do
    pc_res[i] := pc[i];
  for i       := 1 to 20 do
    K_res[i]  := k[i];
  for i       := 1 to 10 do
    KC_res[i] := kc[i];
  for i       := 1 to 10 do
    PS_res[i] := ps[i];
  for i       := 1 to 10 do
    KS_res[i] := ks[i];

  gr_ini_number := 1; //нумерация массива исходеых лет и хронологий
  total_precin  := 0; //начальные значения для суммарного количества осадков в первый год
  pinim_in      := p[10];
  while check20 = true do
  begin
    check20          := false;
    {--------------Cycle------------}
    filecli          := filecli1;
    year_str         := copy(filecli, id4 - 3, 4);
    val(year_str, year, code);
    climfile         := pathcli + filecli + '.cli';
    result_file      := result_path + filecli;
    result_file_rate := result_path + areaname + ' rate ' + inttostr(begyear) + ' - ' +
      inttostr(endyear) + '.dat';
    Year_number      := Year_number + 1;
    {procedures}

    //проверка существование файла
    file_exist := false;
    while file_exist = false do
    begin
      assignfile(f1, climfile);
      reset(f1);
      if IOResult <> 0 then
      begin
        jj1      := jj1 - 1;
        ig       := ig + 1;
        filecli1 := copy(filecli, 1, id4 - 4);
        filecli1 := filecli1 + Inttostr(IG);
        climfile := pathcli + filecli1 + '.cli';
        year     := year + 1;
        for i := icycle to jj1 do
        begin
          gr_ini[i]    := gr_ini[gr_ini_number + 1];
          years_ini[i] := years_ini[gr_ini_number + 1];
          gr_ini_number := gr_ini_number + 1;
        end;
      end
      else
        file_exist := true;
    end;
    //проверка существование файла


    INPCLI(climfile, p, tem, pre, M, ERR, NU);
 { for i:=1 to 366 do tem1[i]:=tem[i+1];
  for i:=1 to 366 do tem[i]:=tem1[i];    проверка на первое значение}
    for i := 1 to 366 do
      tem1[i] := 0;
{  IF (FILT='A') or (FILT='L') or (FILT='V') then
  Filter(Tem,M,Filt,tem1);
  for i:=1 to 366 do tem[i]:=tem1[i];}
    {result of error}
    X := (JS - 1) * 365 + NU;
    DAYLENGTH(LAT, NU, M, DL);
    //season_temp(tem,m,p[22],n_beg,n_end);
    grwdown(TEM, M, z_beg, z_end);
    years[Year_number] := year;
    //массиву лет присваиваем значение года
    GRRT50(TEM, PRE, Nu, M, k, p, pc, kc, dl, climfile, result_file_rate, year, grrt_check,
      n_beg, z_end, total_precin, pinim_in, gr, sm,
      gr_middle[Year_number], T_beg, Beg_gr1, End_gr1, Beg_gr2, End_gr2, Pinim, pinim_out,
      growth_rate, total_precout, begdep, enddep, temmin, tembeg, daymin);
    //n_beg - начало сезона роста по температуре, z_end - конец сезона, когда почва замерзает
    {if k[1]=0 then} p[10] := pinim;
    p[23]              := p_res[23];//snow output
    grrt_check         := 1;
    K[11]              := T_beg + NU;
    BMELT[JJ]          := K[17];
    BSNOW[JJ]          := K[16];
    KC[9]              := K[11];

    //данные для проверки
    piniin[jj]  := pinim_in;
    piniout[jj] := pinim_out;
    precin[jj]  := total_precin;
    precout[jj] := total_precout;
    Dep_beg[jj] := begdep;
    Dep_end[jj] := enddep;

    total_precin := total_precout;
    pinim_in     := pinim_out;


    // сезон роста по температуре
    bgt[jj]          := n_beg;
    egt[jj]          := n_end;
    BGROWTH1[JJ]     := Beg_gr1;
    EGROWTH1[JJ]     := End_gr1;
    BGROWTH2[JJ]     := Beg_gr2;
    EGROWTH2[JJ]     := End_gr2;
    BGW[jj]          := z_beg;
    EGW[jj]          := z_end;
    vcr[jj]          := growth_rate;
    temtmin[jj]      := temmin;
    temtbeg[jj]      := tembeg;
    daytmin[jj]      := daymin;
    //vcr, growth_rate - значение скорости когда начинается рост
    RINGC[JJ]        := NR;
    rint[jj, icycle] := nr;
    PC[16]           := NR;

    {procedures}
    js := js + 1;
    jj := jj + 1;
    if (ig < endyear) then
    begin
      ig       := ig + 1;
      filecli1 := copy(filecli, 1, id4 - 4);
      filecli1 := filecli1 + Inttostr(IG);
      check20  := true;
      {result in file}
    end;
    icycle        := icycle + 1;
    gr_ini_number := gr_ini_number + 1;
  end;
  {------CREATE FILE FOR OUTPUT INFORMATION-----}
  climfile1 := result_path + areaname + ' ' + inttostr(begyear) + ' - ' + inttostr(endyear) + '.dat';
  assign(f5, climfile1);
  rewrite(f5);
  middle(jj1, gr_middle, p[24], another_gr_middle, p_res[24]);//new
  deviation(another_gr_middle, jj1, gr_norm);
  deviation(gr_ini, jj1, ini_norm);
  writeln(f5, '              Calculation from ', begyear, ' to ', endyear);
  writeln(f5, '  year  model    crn     NMOD    NCRN    Tmin  	 BG1     EG1    BG2   EG2   Tem1   Tem2      Gr');
  for i := 1 to jj1 do
    writeln(f5, years[i]: 6, another_gr_middle[i]: 8: 4, gr_ini[I]: 8: 4, gr_norm[i]: 8: 4,
      ini_norm[i]: 8: 4, daytmin[i]: 6, BGROWTH1[I]: 8, EGROWTH1[I]: 8, BGROWTH2[I]: 6, EGROWTH2[I]: 6{, p[22]:8:1,GT[i]:6,EGT[i]:6,
  Dep_beg[i]:8,Dep_end[i]:8,piniin[i]:8:2,piniout[i]:8:2,BGW[i]:6,EGW[i]:6},
      temtmin[i]: 8: 2, temtbeg[i]: 8: 2, vcr[i]: 8: 2);
  {_______________}
 {for i:=1 to jj1 do
 years_ar[i]:=years[i];}//begyear+i-1;
  closefile(f5);
end;

(* growth/ *)

procedure saveResult();
(*  *)
var
  f1, f2, f3:    textfile;
  f_1, f_2, f_3: string;
  i:             integer;
  P:             array30;  //поменять имена
  K:             integer20;
  pc:            real20;
  KC:            real10;
  ps, ks:        real10;
  filter:        string;
  p_text:        array [1..31] of string[50];
  k_text:        array [1..30] of string[50];
  KC_text, PS_text, KS_text: array [1..10] of string[62];
  PC_text:       array [1..20] of string[50];
begin

  assignfile(f1, 'parameters.par');
  reset(f1);
  for i := 1 to 2 do
    readln(f1);
  readln(f1, filter);
  readln(f1);
  for i := 1 to 31 do
    readln(f1, p[i], p_text[i]);
  readln(f1);
  for i := 1 to 20 do
    readln(f1, k[i], k_text[i]);
  readln(f1);
  for i := 1 to 10 do
    readln(F1, KC[i], KC_text[i]);
  readln(f1);
  for i := 1 to 20 do
    readln(F1, PC[i], PC_text[i]);
  readln(F1);
  for i := 1 to 10 do
    readln(F1, PS[i], PS_text[i]);
  readln(F1);
  for i := 1 to 10 do
    readln(F1, KS[i], KS_text[i]);
  closefile(f1);

  //text
  p_text[1]  := 'T1   - Minimum temperature for tree growth';
  p_text[2]  := 'T2   - Lower end of range of optimal temperatures T2-T3';
  p_text[3]  := 'T3   - Upper end of range of optimal temperatures';
  p_text[4]  := 'T4   - Maximum temperature for tree growth';
  p_text[5]  := 'W4   - Maximum soil moisture for tree growth';
  p_text[6]  := '     - Coefficient of temperature modulation T+b6';
  p_text[7]  := 'Tm   - Sum of temperature for start soil melting';
  p_text[8]  := 'sm1  - First coefficient of soil melting';
  p_text[9]  := 'sm2  - Second coefficient of soil melting';
  p_text[10] := 'Wo   - Initial soil moisture';
  p_text[11] := 'Pmax - Maximum daily precipitation for saturated soil';
  p_text[12] := 'Wmin - Minimum soil moisture (wilting point)';
  p_text[13] := 'lr   - Root depth';
  p_text[14] := 'C2   - First coefficient for calculation of transpiration';
  p_text[15] := 'C3   - Second coefficient for calculation of transpiration';
  p_text[16] := 'C1   - Fraction of precip. penetrating soil (not caught by crown) (1-p16)';
  p_text[17] := 'W1   - Minimum soil moisture for tree growth, relative to saturated soil (v/vs)';
  p_text[18] := 'W2   - Lower end of range of optimal soil moistures (v/vs) W2-W3';
  p_text[19] := 'W3   - Upper end of range of optimal soil moistures (v/vs)';
  p_text[20] := 'W4   - Growth is stoped at this soil moisture';
  p_text[21] := 'Cd   - Coefficient for water drainage from soil (rel. unit)';
  p_text[22] := 'Tg   - Sum of temperature to start growth';
  p_text[23] := 'Sno  - Initial snowpack';
  p_text[24] := '     - Rate of snow melting';
  p_text[25] := '     - Minimum temperature snow melting';
  p_text[26] := '     - Temperature correction on elevation';
  p_text[27] := '     - This parameter is changed by program';
  p_text[28] := '     - Coefficient of precipitation modification';
  p_text[29] := '     - This parameter is changed by program';
  p_text[30] := '     - Delta for parameter';
  p_text[31] := '	     - Coefficient of solar modification';
  k_text[1]  := ' 	 - Soil melting  (1), no (0)';
  k_text[2]  := '	 - This parameter is changed by program';
  k_text[3]  := '	 - Estimation with pause for each 5 years if 1, without if 0';
  k_text[4]  := '	 - Snow melting (1), no (0)';
  k_text[5]  := '	 - This parameter is changed by program';
  k_text[6]  := '	 - This parameter is changed by program';
  k_text[7]  := '	 - This parameter is changed by program';
  k_text[8]  := '	 - Maximum duration (days) of latewood formation';
  k_text[9]  := '	 - Period of sum temp. to start growth';
  k_text[10] := '	 - Period of sum temp. to start soil melting';
  k_text[11] := '	 - This parameter is changed by program';
  k_text[12] := '	 - This parameter is changed by program';
  k_text[13] := '	 - Options CSCALC 0-4, no calculation (5)';
  k_text[14] := '	 - This parameter is changed by program';
  k_text[15] := '	 - Period of cell enlargement after last division';
  k_text[16] := '	 - This parameter is changed by program';
  k_text[17] := '	 - This parameter is changed by program';
  k_text[18] := '	 - Number of parameter which is changed';
  k_text[19] := '	 - Nnumber of itterations';
  k_text[20] := '	 - Skale win 5';


  assignfile(f1, 'deparameters.par');         //grrt50
  rewrite(f1);
  writeln(f1, 'File of parameters');
  writeln(f1);
  writeln(f1, Filt);
  Writeln(f1, 'PARAMETERS   FORMAT');
  for i := 1 to 31 do
    writeln(f1, (p_res[i]): 8: 4, i: 4, ' ', p_text[i]);
  writeln(f1, 'INTEGER ARRAY K(20)');
  for i := 1 to 20 do
    writeln(f1, (k_res[i]): 6, i: 4, ' ', k_text[i]);
  writeln(f1);
  for i := 1 to 10 do
    writeln(F1, KC_res[i]: 2: 2, ' ', KC_text[i]);
  writeln(f1);
  for i := 1 to 20 do
    writeln(F1, PC_res[i]: 2: 2, ' ', PC_text[i]);
  writeln(F1, 'Real array ps(10)');
  for i := 1 to 10 do
    writeln(F1, PS_res[i]: 2: 2, PS_text[i]);
  writeln(F1, 'integer arrey ks(10)');
  for i := 1 to 10 do
    writeln(F1, KS_res[i]: 2: 0, KS_text[i]);
  closefile(f1);

end;


(*
 tmeits
/GrowthFitnessFunction *)
function GrowthFitnessFunction(x: array of FloatType): FloatType;
var
  rTo, rFrom: integer;
begin
  rTo   := 300;
  rFrom := 10;
  GrowthFitnessFunction := (Random(rTo - rFrom) + rFrom) / 100.0;
  (* Присвоим вектор параметрам бегункам *)
   (* growth(Edit1.Text,Edit2.Text,Edit5.Text,Edit7.Text,Edit8.Text,num,gr_ini,gr_middle,BEGYEAR,ENDYEAR,years_array);
      growth(Edit1.Text,Edit2.Text,Edit5.Text,Edit7.Text,Edit8.Text,num,gr_ini,gr_middle,BEGYEAR,ENDYEAR,years_array
   *)
   (*
  vsde_filename_1, vsde_path, vsde_filename_4: string;
  vsde_latitude: string;
  vsde_endyear_str: string;
  vsde_Year_number: integer;
  vsde_gr_ini: real100;
  vsde_gr_middle: real100;
  vsde_BEGYEAR: integer;
  vsde_ENDYEAR: integer;
  vsde_years: integer100;
   *)

end;

(* GrowthFitnessFunction/ *)
end. (* VSModel/ *)
