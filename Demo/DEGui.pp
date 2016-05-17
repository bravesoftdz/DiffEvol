unit DEGui;

//{$MODE Delphi}
{$mode objfpc}{$H+}

{$RANGECHECKS ON}
{$DEBUGINFO ON}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, DiffEvol, Math, Dialogs, StdCtrls, Buttons, ExtCtrls, CheckLst,
  ComCtrls, TestFunctions, VSModel, VSDEparameters, Oscilloscope;

type
  (* http://wiki.freepascal.org/TAChart_Tutorial:_Getting_started
  http://www.mathworks.com/matlabcentral/fileexchange/15164-speedyga--a-fast-simple-genetic-algorithm
  http://pubs.rsc.org/en/content/articlehtml/2015/ja/c4ja00470a
  *)
  { TFormVSDE }

  TFormVSDE = class(TForm)
    ButtonExit: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BeginYear: TLabeledEdit;
    BitBtn4: TBitBtn;
    EachResult: TComboBox;
    EditGainR3: TEdit;
    EditGainR2: TEdit;
    EditGainR1: TEdit;
    EditGainB: TEdit;
    LabelGainR3: TLabel;
    LabelGainR2: TLabel;
    LabelGainR1: TLabel;
    LabelGainB: TLabel;
    LabelEachResult: TLabel;
    CrossingOver: TLabeledEdit;
    SaveParam: TCheckBox;
    EditParameters: TCheckBox;
    SizeTracheids: TLabeledEdit; (* индекс прироста, соответствующий данным измерений размеров трахеид *)
    FormOscilloscope: TCheckBox;
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

    procedure ButtonExitClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonVSClick(Sender: TObject);
    procedure EditParametersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure ButtonClick1(Sender: TObject);
    procedure EditParametersClick(Sender: TObject);
    procedure FormOscilloscopeChange(Sender: TObject);
    procedure FormOscilloscopeClick(Sender: TObject);
    procedure SaveParamChange(Sender: TObject);
    procedure TestFunctionRastrigin(Sender: TObject);
  private
    de: TDiffEvol;
  public
    function TestFunction(Sender: TObject; const Population: TDiffEvolPopulation): Double;
    function TestDiffEvolFunctionRastrigin(Sender: TObject; const Population: TDiffEvolPopulation): Double;
    function DiffEvolGrowthFitnessFunction(Sender: TObject; const Population :TDiffEvolPopulation):Double;
  end;

var
  FormVSDE: TFormVSDE;
  FormEditParameters1: TFormEditParameters;

implementation

{$R *.lfm}

const ORDER = 4;
      Coefficients : Array[0..ORDER] of Double = (2.25, -4.9, 3.58, 0.7, -0.169);

procedure TFormVSDE.ButtonVSClick(Sender: TObject);
var
  vsde: TDiffEvol;
  mn, mx, x: TDiffEvolPopulation;
  pass, i, m: Integer;
  Cost, error: Double;
  best_pop: TDiffEvolPopulation;

  VS_ORDER, PASS_COUNT, POP_COUNT: Integer;
  VS_MSE, GAIN_BEST, GAIN_R1, GAIN_R2, GAIN_R3, CR: Double;

  VS_CLIMDATA, VS_CHRONOLOGY, VS_SIZETRACHEIDS: string;
  VS_BYEAR, VS_EYEAR: integer;
  VS_LATITUDE: string;

begin
  (* Get Form DE Parameters *)
  VS_ORDER := StrToInt(VariableCount.Text);
  POP_COUNT := StrToInt(PopulationCount.Text);
  PASS_COUNT := StrToInt(Generation.Text);
  VS_MSE := StrToFloat(MSE.Text);
  GAIN_BEST := StrToFloat(EditGainB.Text);
  GAIN_R1 := StrToFloat(EditGainR1.Text);;
  GAIN_R2 := StrToFloat(EditGainR2.Text);;
  GAIN_R3 := StrToFloat(EditGainR3.Text);;
  CR := 1.0;
  LogBox.Lines.Clear;
  LogBox.Lines.Add('VSDE DE Parameters OK.');

  (* Get Form VS Parameters *)
  VS_BYEAR := StrToInt(BeginYear.Text); VS_EYEAR := StrToInt(EndYear.Text);
  LogBox.Lines.Add('VSDE VS Parameters OK.');

  (* Create *)
  SetLength(mn, VS_ORDER);
  SetLength(mx, VS_ORDER);
  for i := 0 to VS_ORDER - 1 do begin
    mn[i]:= -5.7;
    mx[i]:=  5.7;
  end;
  vsde:=TDiffEvol.Create(POP_COUNT, VS_ORDER, mn, mx);
  vsde.OnCalcCosts := @DiffEvolGrowthFitnessFunction; (* set fitness function *)

  (* Here, the exact coefficients are found after about N iterations *)
  m := 0;
  for pass:=0 to PASS_COUNT do begin
    vsde.evolve (GAIN_BEST, GAIN_R1, GAIN_R2, GAIN_R3, CR);
    Cost := vsde.getBestCost;
    if m >= StrToInt(EachResult.Text) then begin
       LogBox.Lines.Add('Pass ' + Inttostr(Pass) + ': ' + FloattostrF(Cost, ffFixed, 6, 2));
       m := 0;
       end
    else m := m + 1;
  end;
  (* Print result *)
  LogBox.Lines.Add('Theoric / Found / Error');
  (* Application.MessageBox(PChar('В процессе разработки....'), 'Внимание',0); *)
end;

function TFormVSDE.DiffEvolGrowthFitnessFunction(Sender: TObject; const Population :TDiffEvolPopulation):Double;
(* Cost function *)
begin
  Result := GrowthFitnessFunction(Population);
end;

procedure TFormVSDE.TestFunctionRastrigin(Sender: TObject);  (* call on click button Start Rastrigin*)
const
  MM = 100;
var
  rde: TDiffEvol;
  mn, mx, x: TDiffEvolPopulation;
  pass, i, m: Integer;
  Cost, error: Double;
  best_pop: TDiffEvolPopulation;

  VS_ORDER, PASS_COUNT, POP_COUNT: Integer;
  VS_MSE, GAIN_BEST, GAIN_R1, GAIN_R2, GAIN_R3, CR: Double;


begin
  (* Get Form *)
  VS_ORDER := StrToInt(VariableCount.Text);
  POP_COUNT := StrToInt(PopulationCount.Text);
  PASS_COUNT := StrToInt(Generation.Text);
  VS_MSE := StrToFloat(MSE.Text);
  GAIN_BEST := StrToFloat(EditGainB.Text);

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
  //rde.OnCalcCosts := FormVSDE.TestDiffEvolFunctionRastrigin; (* set fitness function *)
  (* Here, the exact coefficients are found after about N iterations *)
  m := 0;
  for pass:=0 to PASS_COUNT do begin
    rde.evolve (GAIN_BEST, -0.7, 0.7, 1.0, 1.0);
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



function TFormVSDE.TestDiffEvolFunctionRastrigin(Sender: TObject; const Population :TDiffEvolPopulation):Double;
(* Rastrigin Function *)
begin
  Result := Rastrigin(Population);
end;

function TFormVSDE.TestFunction(Sender: TObject; const Population :TDiffEvolPopulation):Double;
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

procedure TFormVSDE.FormCreate(Sender: TObject);
var mn, mx : TDiffEvolPopulation;
    i      : Integer;
begin

 FormEditParameters := TFormEditParameters.Create(Self); (*  *)
 OneOscilloscope := TOscilloscope.Create(Self);
 //LoadGrid;
 FillGrid;
 SaveGrid;

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

 LogBox.Lines.Clear;
 LogBox.Lines.Add('The search of optimal parameters for models');
 LogBox.Lines.Add('of formation of annual rings of coniferous Vaganov-Shashkin V6 9.5.16');
end;


procedure TFormVSDE.Button4Click(Sender: TObject);
begin
  Halt(0);
end;

procedure TFormVSDE.Button2Click(Sender: TObject);
begin

end;

procedure TFormVSDE.Button1Click(Sender: TObject);
begin

end;

procedure TFormVSDE.ButtonExitClick(Sender: TObject);
begin
  Halt(0);
end;


procedure TFormVSDE.EditParametersChange(Sender: TObject);
begin
  if FormVSDE.EditParameters.Checked = true then
     FormEditParameters.Show
  else
     FormEditParameters.Hide
end;

procedure TFormVSDE.SaveParamChange(Sender: TObject);
begin
  Application.MessageBox(PChar('В процессе разработки....'), 'Внимание',0);
end;

procedure TFormVSDE.FormDestroy(Sender: TObject);
begin
 de.Free;
end;

procedure TFormVSDE.ButtonClick(Sender: TObject);
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
procedure TFormVSDE.ButtonClick1(Sender: TObject);
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

procedure TFormVSDE.EditParametersClick(Sender: TObject);
begin
  (* set boolean flag and wrrite logbox values *)
end;

procedure TFormVSDE.FormOscilloscopeChange(Sender: TObject);
(* *)
begin
  if FormVSDE.FormOscilloscope.Checked = true then
     OneOscilloscope.Show
  else
     OneOscilloscope.Hide
end;

procedure TFormVSDE.FormOscilloscopeClick(Sender: TObject);
begin
  (* Plot .CRN and evalCRN *)

end;

end.
