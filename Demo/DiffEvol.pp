unit DiffEvol;

(* {$MODE Delphi} *)
{$mode objfpc}{$H+}
{$RANGECHECKS ON}
{$DEBUGINFO ON}

(* DiffEvol
   Delphi Port 2005 by Christian-W. Budde
   http://www.pcjv.de/delphi/
   Lazarus/FPC port 2016 by tmeits
   https://github.com/tmeits/DiffEvol

   Algorithm by Kenneth Price and Rainer Storn
   http://www.icsi.berkeley.edu/~storn/code.html

   Based on an implementation by Laurent de Soras & Olli Niemitalo.
   http://yehar.com/blog/
   http://yehar.com/blog/?p=643

   This class implements Differential Evolution. It is a genetic algorithm aimed
   to find the best set of values to minimize a "cost" function provided by the
   client. See the D.E. web page for more information. *)

interface

type
  FloatType            = Double;
  TDiffEvolPopulation  = array of FloatType;
  TDiffEvolCostEvent   = function (Sender: TObject; const Population: TDiffEvolPopulation): Double of object;

  TEvaluatedPopulation = class
  public
    fPopulation        : TDiffEvolPopulation;
    fCost              : Double;
    fValidCostFlag     : Boolean;
    constructor Create; overload;
    constructor Create(const DiffEvolPopulation: TDiffEvolPopulation; Cost: Double); overload;
  end;

  TDiffEvol = class
  private
    fPopulationCount    : Integer; (* Number of populations *)
    fVariableCount      : Integer; (* Number of variables in population *)
    fBestPopulation     : Integer; (* Negative if not yet determinated *)
    fCurrentGeneration  : array of TEvaluatedPopulation;
    fNextGeneration     : array of TEvaluatedPopulation;
    constructor Create; overload;
    procedure Init(const min_arr, max_arr, best_arr: TDiffEvolPopulation);
    procedure RandomizePopulationulation(const min_arr, max_arr: TDiffEvolPopulation);
    function FindBest: Double;
  protected
    fOnCalcCosts : TDiffEvolCostEvent;
  public
    constructor Create(PopulationCount, VariableCount : Integer; const min_arr, max_arr: TDiffEvolPopulation); overload;
    constructor Create(PopulationCount, VariableCount : Integer; const min_arr, max_arr, best_arr: TDiffEvolPopulation); overload;
    destructor Destroy; override;
    function Evolve(gain_best, gain_r1, gain_r2, gain_r3, cr: Double): Double;
    function getBestPopulation: TDiffEvolPopulation;
    function getBestCost: Double;
    property OnCalcCosts: TDiffEvolCostEvent read fOnCalcCosts write fOnCalcCosts;
  end;

implementation

constructor TEvaluatedPopulation.Create;
begin
 inherited;
 fCost := 0;
 fValidCostFlag := false;
end;

constructor TEvaluatedPopulation.Create(const DiffEvolPopulation: TDiffEvolPopulation; Cost: Double);
begin
 inherited Create;
 fCost := Cost;
 fPopulation := DiffEvolPopulation;
end;

constructor TDiffEvol.Create;
begin
 fBestPopulation := -1;
end;

(*
Name: Constructor
Description:
        Build and initialize the DE object. The min/max array must respect the
	following requirements:
	- Both arrays should have the same length, > 0
	- For every i, min [i] <= max [i]
	- The length of these arrays sets the number of parameters per population.
Input parameters:
	- PopulationCount: Number of populations. Must be >= 5.
	- min_arr: Array of the minimum values for the initial generation
	- max_arr: Array of the maximum values for the initial generation
Throws: exceptions related to memory allocation for vectors *)

constructor TDiffEvol.Create(PopulationCount, VariableCount: Integer; const min_arr, max_arr: TDiffEvolPopulation);
begin
 inherited Create;
 Randomize;
 fPopulationCount := PopulationCount;
 fVariableCount := Length(min_arr);
 assert(fPopulationCount >= 5);
 assert(fVariableCount > 0);
 assert(Length(max_arr) = fVariableCount);
 init(min_arr, max_arr, nil);
end;

(*
Name: Constructor
Description:
	Same things than other above, gives a clue to the DE for the initial best
	population, so the algorithm does not start from total randomness.
Input parameters:
	- PopulationCount: Number of populations. Must be >= 5.
	- min_arr: Array of the minimum values for the initial generation
	- max_arr: Array of the maximum values for the initial generation
	- best_arr: Set of initial values.
Throws: exceptions related to memory allocation for vectors *)

constructor TDiffEvol.Create(PopulationCount, VariableCount : Integer; const min_arr, max_arr, best_arr: TDiffEvolPopulation);
begin
 inherited Create;
 fPopulationCount:=PopulationCount;
 fVariableCount:=Length(min_arr);
 assert(fPopulationCount >= 5);
 assert(fVariableCount > 0);
 assert(Length(max_arr) = fVariableCount);
 assert(Length(min_arr) = fVariableCount);
 init(min_arr, max_arr, best_arr);
end;

(*
Name: Evolve
Description:
        Compute the next generation, trying to find a population which cost is
        lower than the previous one. Note: coefficient for the original population
        is 1.0 minus sum of gain_*. The coefficients can be negative as well as
        positive, and be higher than 1.
Input parameters:
        - gain_best: Coefficient applyed to the previous best population
        - gain_r1: Coefficient for 1st random population
        - gain_r2: Coefficient for 2nd random population
        - gain_r3: Coefficient for 3rd random population
        - cr: Crossing-over amount, in [0 ; 1]
Input/output parameters:
        - func: Object implementing the cost function. It shall accept populations
        of the size given at the DiffEvol creation.
Returns: The new best cost.
Throws: Nothing *)

function TDiffEvol.Evolve(gain_best, gain_r1, gain_r2, gain_r3, cr: Double): Double;
var gain_r0              : Double;
    new_BestPopulation   : Integer;
    new_best_Cost        : Double;
    r1,r2,r3,pop,i       : Integer;
    fr                   : Double;
    fvar,fvar_cnt        : Integer;
begin
 assert (cr >= 0);
 assert (cr <= 1);

 if (fBestPopulation < 0) then FindBest;

 gain_r0:=1-gain_best-gain_r1-gain_r2-gain_r3;
 new_BestPopulation:=fBestPopulation;
 new_best_Cost:=fCurrentGeneration[fBestPopulation].fCost;

 for pop:=0 to fPopulationCount-1 do
  begin
   (* Find 3 different populations randomly *)
   repeat
    r1:=random(fPopulationCount);
   until (r1 <> pop) and (r1 <> fBestPopulation);

   repeat
    r2:=random(fPopulationCount);
   until (r2 <> pop) and (r2 <> fBestPopulation) and (r2 <> r1);

   repeat
    r3:=random(fPopulationCount);
   until (r3 <> pop) and (r3 <> fBestPopulation) and (r3 <> r2) and (r3 <> r1);

   (* Generate trial vector with crossing-over *)
   fvar:=random(fVariableCount);
   fvar_cnt:=0;

   if pop=new_BestPopulation
    then fvar_cnt:=0;
   repeat
    fNextGeneration[pop].fPopulation[fvar]:= fCurrentGeneration[            pop].fPopulation[fvar]*gain_r0
                                           + fCurrentGeneration[             r1].fPopulation[fvar]*gain_r1
                                           + fCurrentGeneration[             r2].fPopulation[fvar]*gain_r2
                                           + fCurrentGeneration[             r3].fPopulation[fvar]*gain_r3
                                           + fCurrentGeneration[fBestPopulation].fPopulation[fvar]*gain_best;
    inc(fvar);
    if fvar>=fVariableCount then fvar:=0;
    Inc(fvar_cnt);
    fr:=random;
   until (fvar_cnt >= fVariableCount) or (fr >= cr);

   while (fvar_cnt < fVariableCount) do
    begin
     fNextGeneration[pop].fPopulation[fvar]:=fCurrentGeneration[pop].fPopulation[fvar];
     inc(fvar);
     if fvar>=fVariableCount then fvar:=0;
     Inc(fvar_cnt);
    end;

   (* Evaluate the new population *)
   fNextGeneration[pop].fCost:=fOnCalcCosts(self, fNextGeneration[pop].fPopulation);
   fNextGeneration[pop].fValidCostFlag:=true;

   if (fNextGeneration[pop].fCost < fCurrentGeneration[pop].fCost)
    then
     begin
      if (fNextGeneration[pop].fCost < new_best_Cost) then
       begin (* New best *)
        new_BestPopulation:=pop;
        new_best_Cost:=fNextGeneration[pop].fCost;
       end;
      move(fNextGeneration[pop].fPopulation[0],fCurrentGeneration[pop].fPopulation[0],fVariableCount*SizeOf(FloatType));
      fCurrentGeneration[pop].fCost:=fNextGeneration[pop].fCost;
      fCurrentGeneration[pop].fValidCostFlag:=fNextGeneration[pop].fValidCostFlag;
     end;
  end;

 fBestPopulation:=new_BestPopulation;
 Result:=new_best_Cost;
end;

(*
==============================================================================
Name: getBestPopulation
Description:
         Return the current best population. This function must not be called
         before the first call to evolve ().
Returns: A reference on the population. It remains valid until subsequent
         call to evolve() function.
Throws:  Nothing
==============================================================================
*)

function TDiffEvol.getBestPopulation: TDiffEvolPopulation;
begin
 assert (fBestPopulation >= 0);
 Result:=(fCurrentGeneration[fBestPopulation].fPopulation);
end;

(*
==============================================================================
Name: getBestCost
Description:
	Return the cost evaluation for the current best population. This function
	must not be called before the first call to evolve ().
Returns: The cost.
Throws: Nothing
==============================================================================
*)

function TDiffEvol.getBestCost: Double;
begin
 assert (fBestPopulation >= 0);
 assert (fCurrentGeneration[fBestPopulation].fValidCostFlag);
 Result:=fCurrentGeneration[fBestPopulation].fCost;
end;

destructor TDiffEvol.Destroy;
begin
 inherited;
end;

procedure TDiffEvol.Init(const min_arr, max_arr, best_arr: TDiffEvolPopulation);
var i,pop : Integer;
begin
 (* Size all the arrays *)
 SetLength(fCurrentGeneration,fPopulationCount);
 SetLength(fNextGeneration,fPopulationCount);
 for pop:=0 to fPopulationCount-1 do
  begin
   fCurrentGeneration[pop]:=TEvaluatedPopulation.Create;
   fNextGeneration[pop]:=TEvaluatedPopulation.Create;
   SetLength(fCurrentGeneration[pop].fPopulation,fVariableCount);
   SetLength(fNextGeneration[pop].fPopulation,fVariableCount);
  end;

 (* Initialize populations with random values *)
 RandomizePopulationulation(min_arr, max_arr);

 (* Introduce the "best" population if it is provided *)
 if assigned(best_arr) then
  begin
   for i:=0 to fVariableCount-1 do fCurrentGeneration[0].fPopulation[i]:=best_arr[i];
   fBestPopulation:=0;
  end;
end;

procedure TDiffEvol.RandomizePopulationulation(const min_arr, max_arr : TDiffEvolPopulation);
var offset  : Double;
    mul     : Double;
    i,j     : Integer;
    val_rnd : Double;
begin
 for i:=0 to fVariableCount-1 do
  begin
   assert(min_arr[i] <= max_arr[i]);
   offset:=min_arr[i];
   mul:=max_arr[i]-min_arr[i];
   for j:=0 to fPopulationCount-1 do
    begin
     val_rnd:=random;
     fCurrentGeneration[j].fPopulation[i]:=val_rnd*mul+offset;
    end;
  end;
 fBestPopulation:=-1;
end;

function TDiffEvol.FindBest: Double;
var best_Cost : double;
    cur_Cost  : Double;
    pop       : Integer;
begin
 if (fBestPopulation < 0)
  then fBestPopulation:= 0;

 if (not fCurrentGeneration[fBestPopulation].fValidCostFlag) then
  begin
   fCurrentGeneration[fBestPopulation].fCost:=fOnCalcCosts(self, fCurrentGeneration[fBestPopulation].fPopulation);
   fCurrentGeneration[fBestPopulation].fValidCostFlag:= true;
  end;

 best_Cost:=fCurrentGeneration[fBestPopulation].fCost;
 for pop:=0 to fPopulationCount-1 do
  begin
   if (pop <> fBestPopulation) then
    begin
     cur_Cost:=fOnCalcCosts(self, fCurrentGeneration[pop].fPopulation);
     fCurrentGeneration[pop].fCost:=cur_Cost;
     fCurrentGeneration[pop].fValidCostFlag:=True;
     if (cur_Cost < best_Cost) then
      begin
       fBestPopulation:=pop;
       best_Cost:=cur_Cost;
      end;
    end;
  end;
 Result:=best_Cost;
end;

end.
