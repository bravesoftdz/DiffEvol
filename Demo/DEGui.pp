unit DEGui;

//{$MODE Delphi}
{$mode objfpc}{$H+}
{$RANGECHECKS ON}
{$DEBUGINFO ON}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, DiffEvol, Math, Dialogs, StdCtrls, Buttons, ExtCtrls, TestFunctions;

type
  (* http://wiki.freepascal.org/TAChart_Tutorial:_Getting_started
  http://www.mathworks.com/matlabcentral/fileexchange/15164-speedyga--a-fast-simple-genetic-algorithm
  http://pubs.rsc.org/en/content/articlehtml/2015/ja/c4ja00470a
  *)
  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BeginYear: TLabeledEdit;
    CheckBox1: TCheckBox;
    SizeTracheids: TLabeledEdit; (* индекс прироста, соответствующий данным измерений размеров трахеид *)
    PlotResult: TCheckBox;
    EndYear: TLabeledEdit;
    SoilMelting: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Chronology: TLabeledEdit;
    Latitude: TLabeledEdit;
    ClimaticData: TLabeledEdit;
    PopulationCount: TLabeledEdit;
    VariableCount: TLabeledEdit;
    Generation: TLabeledEdit;
    MSE: TLabeledEdit;
    LogBox: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonVSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure ButtonClick1(Sender: TObject);
    procedure TestFunctionRastrigin(Sender: TObject);
  private
    de: TDiffEvol;
  public
    function TestFunction(Sender: TObject; const Population: TDiffEvolPopulation): Double;
    function TestDiffEvolFunctionRastrigin(Sender: TObject; const Population: TDiffEvolPopulation): Double;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const ORDER = 4;
      Coefficients : Array[0..ORDER] of Double = (2.25, -4.9, 3.58, 0.7, -0.169);

procedure TForm1.TestFunctionRastrigin(Sender: TObject);  (* call on click button Start Rastrigin*)
const
  MM = 100;
var
  rde: TDiffEvol;
  mn, mx, x: TDiffEvolPopulation;
  pass, i, m: Integer;
  Cost, error: Double;
  best_pop: TDiffEvolPopulation;
   (* Берем с формы *)
  VS_ORDER, PASS_COUNT, POP_COUNT: Integer;
  VS_MSE: Double;

begin
  VS_ORDER := StrToInt(VariableCount.Text);
  POP_COUNT := StrToInt(PopulationCount.Text);
  PASS_COUNT := StrToInt(Generation.Text);
  VS_MSE := StrToFloat(MSE.Text);

  LogBox.Lines.Clear;
  LogBox.Lines.Add('TestFunctionRastrigin');
  (* Test Function *)
  SetLength(x, VS_ORDER);
  LogBox.Lines.Add('Order= ' + Inttostr(Length(x)));
  for i:=0 to Length(x) - 1 do begin
      x[i] := Random;
      LogBox.Lines.Add('x[' + Inttostr(i) + ']= ' + FloattostrF(x[i],ffFixed,4,2));
  end;
  LogBox.Lines.Add('Rastrigin(x)= ' + FloattostrF(Rastrigin(x),ffFixed,4,2));
  for i:=0 to Length(x) - 1 do begin
      x[i] := 0.0;
  end;
  LogBox.Lines.Add('Rastrigin(x=0.0)= ' + FloattostrF(Rastrigin(x),ffFixed,4,2));
  LogBox.Lines.Add('TestDiffEvolFunctionRastrigin');

  (* Create *)
  SetLength(mn, VS_ORDER);
  SetLength(mx, VS_ORDER);
  for i := 0 to VS_ORDER - 1 do begin
    mn[i]:= -5.7;
    mx[i]:=  5.7;
  end;
  rde:=TDiffEvol.Create(POP_COUNT, VS_ORDER, mn, mx, @TestDiffEvolFunctionRastrigin);
  (* *)
  //rde.OnCalcCosts := Form1.TestDiffEvolFunctionRastrigin; (* set fitness function *)
  (* Here, the exact coefficients are found after about N iterations *)
  m := 0;
  for pass:=0 to PASS_COUNT do begin
    rde.evolve (0, -0.7, 0.7, 1.0, 1.0);
    Cost := rde.getBestCost;
    if m >= MM then begin
       LogBox.Lines.Add('Pass ' + Inttostr(Pass) + ': ' + FloattostrF(Cost, ffFixed, 6, 2));
       m := 0;
       end
    else m := m + 1;
  end;
  (* Print result *)
  LogBox.Lines.Add('Theoric / Found / Error');
  best_pop:=rde.getBestPopulation;
  for i:= 0 to VS_ORDER - 1 do begin
    error:=abs(0.0  - best_pop[i]);
    LogBox.Lines.Add(FloattostrF(0.0,ffFixed,6,2)+'     '+
                    FloattostrF(best_pop[i],ffFixed,6,2)+'     '+
                    FloattostrF(Error,ffFixed,6,2));
  end;
  LogBox.Lines.Add('******************************');
  LogBox.Lines.Add(FloattostrF(Rastrigin(x),ffFixed,6,2)+'     '+
                    FloattostrF(RAstrigin(best_pop),ffFixed,6,2)+'     '+
                    FloattostrF(Rastrigin(x) - RAstrigin(best_pop),ffFixed,6,2));
  rde.Free;
  mn := nil; mx := nil; x := nil;
end;

function TForm1.TestDiffEvolFunctionRastrigin(Sender: TObject; const Population :TDiffEvolPopulation):Double;
(* Rastrigin Function *)
begin
  Result := Rastrigin(Population);
end;

function TForm1.TestFunction(Sender: TObject; const Population :TDiffEvolPopulation):Double;
var y_Population  : Double;
    y_Reference   : Double;
    i             : Integer;
    step,x        : Double;
    err_sum       : Double;
    err_val       : Double;
begin
 assert(Length(Population)=ORDER + 1);
 err_sum:=0;
 // Integration of (ftest (x) - fref (x)) ^ 2 over [-5; 5]
 step:=1/128;
 x:=-5;
 while x<=5 do
  begin
   y_Population:=0;
   y_Reference:=0;
   for i:=ORDER downto 0 do
    begin
     y_Population:=y_Population * x + Population[i];
     y_Reference:=y_Reference * x + Coefficients[i];
    end;
   err_val:=y_Population-y_Reference;
   err_sum:=err_sum+err_val*err_val;
   x:=x+step;
  end;

 // Log has no impact on performances here but it's easier to follow...
 err_sum:=err_sum+1e-200; // To ensure it's strictly positive
 Result:=ln(err_sum);  // =cost...!!! TODO
end;

procedure TForm1.FormCreate(Sender: TObject);
var mn, mx : TDiffEvolPopulation;
    i      : Integer;
begin
 SetLength(mn,ORDER+1);
 SetLength(mx,ORDER+1);
 for i:=0 to ORDER do
  begin
   mn[i]:=-1000;
   mx[i]:= 1000;
  end;
 de:=TDiffEvol.Create(100,ORDER+1,mn,mx);
 (* http://forum.lazarus.freepascal.org/index.php/topic,30880.msg196840.html#msg196840*)
 (* use {$mode delphi}
 de.OnCalcCosts:=TestFunction;  *)
 de.OnCalcCosts:=@TestFunction;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Halt(0);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Halt(0);
end;

procedure TForm1.ButtonVSClick(Sender: TObject);
begin

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 de.Free;
end;

procedure TForm1.ButtonClick(Sender: TObject);
var pass     : Integer;
    cost     : Double;
    i        : Integer;
    error    : Double;
    best_pop : TDiffEvolPopulation;
begin
 LogBox.Lines.Clear;

 // Here, the exact coefficients are found after about 700 iterations
 for pass:=0 to 700 do
  begin
   de.evolve (0, -0.7, 0.7, 1.0, 1.0);
   Cost:=de.getBestCost;
   LogBox.Lines.Add('Pass '+Inttostr(Pass)+': '+FloattostrF(Cost,ffFixed,4,1)+' dB');
  end;

 // Print result
 LogBox.Lines.Add('Theoric / Found / Error');

 best_pop:=de.getBestPopulation;
 for i:= 0 to ORDER do
  begin
   //error:=f_abs(Coefficients[i]-best_pop[i]);
   error:=abs(Coefficients[i]-best_pop[i]);
   LogBox.Lines.Add(FloattostrF(Coefficients[i],ffFixed,4,2)+'     '+
                    FloattostrF(best_pop[i],ffFixed,4,2)+'     '+
                    FloattostrF(Error,ffFixed,4,2));
  end;
end;
procedure TForm1.ButtonClick1(Sender: TObject);
var pass     : Integer;
    cost     : Double;
    i        : Integer;
    error    : Double;
    best_pop : TDiffEvolPopulation;
begin
 LogBox.Lines.Clear;

 // Here, the exact coefficients are found after about 700 iterations
 for pass:=0 to 700 do
  begin
   de.evolve (0, -0.7, 0.7, 1.0, 1.0);
   Cost:=de.getBestCost;
   LogBox.Lines.Add('Pass '+Inttostr(Pass)+': '+FloattostrF(Cost,ffFixed,4,1)+' dB');
  end;

 // Print result
 LogBox.Lines.Add('Theoric / Found / Error');

 best_pop:=de.getBestPopulation;
 for i:= 0 to ORDER do
  begin
   //error:=f_abs(Coefficients[i]-best_pop[i]);
   error:=abs(Coefficients[i]-best_pop[i]);
   LogBox.Lines.Add(FloattostrF(Coefficients[i],ffFixed,4,2)+'     '+
                    FloattostrF(best_pop[i],ffFixed,4,2)+'     '+
                    FloattostrF(Error,ffFixed,4,2));
  end;
end;

end.
