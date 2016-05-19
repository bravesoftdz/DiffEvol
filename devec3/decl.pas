unit decl;

{$mode objfpc}{$H+}
//{$mode tp}

{$RANGECHECKS ON}
{$DEBUGINFO ON}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils;

const     (* Change any of these parameters to match your needs *)

  VTR = 1.0e-25;      (* "Value To Reach". devec3 will stop its minimization
                           if either the maximum number of iterations "itermax"
                           is reached or the best parameter vector "bestmem"
                           has found a value f(bestmem,y) <= VTR. *)

  D = 3;              (* number of parameters of the objective function *)

  NP = 30;            (* number of population members *)

  itermax = 50000;    (* maximum number of iterations (generations) *)

  F = 0.8;            (* DE-stepsize F ex [0, 2] *)

  CR = 0.5;           (* crossover probabililty constant ex [0, 1] *)

  strategy = 8;       (* 1 --> DE/best/1/exp           6 --> DE/best/1/bin
                          2 --> DE/rand/1/exp           7 --> DE/rand/1/bin
                          3 --> DE/rand-to-best/1/exp   8 --> DE/rand-to-best/1/bin
                          4 --> DE/best/2/exp           9 --> DE/best/2/bin
                          5 --> DE/rand/2/exp           else  DE/rand/2/bin *)

  XVmin: array [1..D] of double = (-1000, -10000, -1000);
  XVmax: array [1..D] of double = (1000, 1000, 1000);
                      (* XVmin,XVmax   vector of lower and bounds of initial population and
                                       futhref iterations
                                     the algorithm seems to work well only if [XVmin,XVmax]
                                   covers the region where the global minimum is expected *)

  XY = 200;           (* number of points; first element of
                         INPUT DATA
                         load x_dos.txt -ASCII
                         load y_dos.txt -ASCII  *)


(*     The first four arguments are essential (though they have
       default values, too). In particular, the algorithm seems to
       work well only if [XVmin,XVmax] covers the region where the
       global minimum is expected. DE is also somewhat sensitive to
       the choice of the stepsize F. A good initial guess is to
       choose F from interval [0.5, 1], e.g. 0.8. CR, the crossover
       probability constant from interval [0, 1] helps to maintain
       the diversity of the population and is rather uncritical. The
       number of population members NP is also not very critical. A
       good initial guess is 10*D. Depending on the difficulty of the
       problem NP can be lower than 10*D or must be higher than 10*D
       to achieve convergence.
       If the parameters are correlated, high values of CR work better.
       The reverse is true for no correlation.

 default values in case of missing input arguments:
   VTR = 1.e-6;
   D = ?;
   NP = 10*?;
   itermax = D*100;
   F = 0.8;
   CR = 0.5;
   strategy = 7;

 Cost function:    function result = f(x,y);
                        has to be defined by the user and is minimized
      w.r. to  x(1:D).

*)




type

  popsizematrix = array[1..NP, 1..D] of double;         (* matrix of size NPxD *)
  indexarray = array[1..NP] of integer;                 (* index array *)

var

  a1, a2, a3, a4, a5: indexarray;                       (* index arrays *)
  bestmem: array[1..1, 1..D] of double;
  (* best population member ever *)
  bestmemiter: array[1..1, 1..D] of double;
  (* temporary best member of iteration *)
  bestval: double;
  (* best value of member in iteration *)
  bestvalev: double;                                    (* value of best member ever *)
  bm: popsizematrix;
  cur_gen: integer;
  DC, odch_st: double;
  FDX, FDY, datnum: integer;
  i, j, k, n: integer;
  ik, iP: integer;
  ind: array[1..4] of integer;
  jrand: integer;
  memberval: array[1..NP] of double;                    (* array of members values *)
  mpo: array[1..NP, 1..D] of integer;
  mui: array[1..NP, 1..D] of integer;
  muitrans: array[1..D, 1..NP] of integer;
  muitratm: array[1..D, 1..NP] of integer;
  n_pop: integer;
  oldrand: array[1..55] of double;
  pm1, pm2, pm3, pm4, pm5: popsizematrix;
  (* population matrix 1, 2, 3, 4, 5 *)
  pop: popsizematrix;
  (* pop is a matrix of size NPxD *)
  popold: popsizematrix;                                (* toggle population *)
  randmatrix: array[1..4] of double;
  randmatrixint: array[1..4] of integer;
  randmatrixnp: array[1..NP] of double;
  randmatrixnpint: array[1..NP] of integer;
  randomseed: double;
  res: Text;
  rot: array[1..NP] of integer;
  (* rotating index array (size NP) *)
  rotd: array[1..D] of integer;
  (* rotating index array (size D) *)
  rt: array[1..NP] of integer;
  (* another rotating index array *)
  rtd: array[1..D] of integer;
  (* rotating index array for exponential crossover *)
  tempmata: array[1..NP] of double;
  st: integer;
  tempmemberval: double;
  teval: double;
  transmata: array[1..NP] of integer;
  ui: popsizematrix;
  valmin: double;
  xs, ys: TextFile;
  xdata, ydata, ydata_obl: array[1..XY] of double;


implementation

end.
