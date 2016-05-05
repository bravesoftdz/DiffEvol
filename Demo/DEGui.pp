unit DEGui;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  DiffEvol, Math, {DDSPUtils,} Dialogs, StdCtrls, TestFunctions;

type
  (* http://wiki.freepascal.org/TAChart_Tutorial:_Getting_started
  http://www.mathworks.com/matlabcentral/fileexchange/15164-speedyga--a-fast-simple-genetic-algorithm
  http://pubs.rsc.org/en/content/articlehtml/2015/ja/c4ja00470a
  *)
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LogBox: TMemo;
    Button: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure ButtonClick1(Sender: TObject);
    procedure ButtonClickSetDefault(Sender: TObject);
    procedure ButtonClickChart(Sender: TObject);
    procedure TestFunctionRastrigin(Sender: TObject);
  private
    de         : TDiffEvol;
  public
    function TestFunction(Sender: TObject; const Population :TDiffEvolPopulation):Double;
    function TestFunction1(Sender: TObject; const Population :TDiffEvolPopulation):Double;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const ORDER = 4;
      Coefficients : Array[0..ORDER] of Double = (2.25, -4.9, 3.58, 0.7, -0.169);

procedure TForm1.TestFunctionRastrigin(Sender: TObject);
var
  x: array [1..30] of Double;
  i: Integer;
begin
  LogBox.Lines.Clear;
  LogBox.Lines.Add('TestFunctionRastrigin');
  LogBox.Lines.Add('LenX= ' + Inttostr(Length(x)));
  for i:=1 to Length(x) do
  begin
      x[i] := Random;
      LogBox.Lines.Add('x[' + Inttostr(i) + ']= ' + FloattostrF(x[i],ffFixed,4,2));
  end;
  LogBox.Lines.Add('Rastrigin(x)= ' + FloattostrF(Rastrigin(x),ffFixed,4,2));
  for i:=1 to Length(x) do begin
      x[i] := 0.0;
  end;
  LogBox.Lines.Add('Rastrigin(x=0.0)= ' + FloattostrF(Rastrigin(x),ffFixed,4,2));
end;

function TForm1.TestFunction(Sender: TObject; const Population :TDiffEvolPopulation):Double;
(* Rastrigin Function *)
var y : Double;

begin
 assert(Length(Population)=ORDER + 1);

 Result := 0.0;
end;

function TForm1.TestFunction1(Sender: TObject; const Population :TDiffEvolPopulation):Double;
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
    i     : Integer;
begin
 SetLength(mn,ORDER+1);
 SetLength(mx,ORDER+1);
 for i:=0 to ORDER do
  begin
   mn[i]:=-100;
   mx[i]:= 100;
  end;
 de:=TDiffEvol.Create(100,ORDER+1,mn,mx);
 de.OnCalcCosts:=TestFunction;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 de.Free;
end;

procedure TForm1.ButtonClickSetDefault(Sender: TObject);
begin

end;

procedure TForm1.ButtonClickChart(Sender: TObject);
begin

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
