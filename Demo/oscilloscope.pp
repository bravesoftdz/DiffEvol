unit Oscilloscope;

{$mode objfpc}

{$RANGECHECKS ON}
{$DEBUGINFO ON}

interface

uses
  Classes, SysUtils, FileUtil, TASources, TAGraph, TASeries, TAFuncSeries,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Types, TACustomSeries;

type

  FloatType = Double;

  { TOscilloscope }

  TOscilloscope = class(TForm)
    PlotButton: TButton;
    GroupBoxWindow: TGroupBox;
    procedure PlotButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  OneOscilloscope: TOscilloscope;

implementation

{$R *.lfm}

{ TOscilloscope }




procedure TOscilloscope.PlotButtonClick(Sender: TObject);
var i,amp,x01,y01,x02,y02,imax,dt1,dt2,func:integer;
begin
imax:=100;                                    // число точек в периоде
dt1:=2;                                             // цена деления X
dt2:=2;                                             // шаг во времени
amp:=70;                                         // амплитуда
x01:=20;                                         // начала координат
x02:=20;
y01:=20+amp;
y02:=y01+2*amp+40;

Canvas.Clear;
Canvas.Brush.Color:= clGreen; {what color you want}
Canvas.Clear;

// Рисуем график с Pixels
Canvas.TextOut(0,0,'    График функции с Pixels');
Canvas.MoveTo(x01,y01);                    //Рисуем ось X
Canvas.LineTo(x01+imax*dt1,y01);
Canvas.MoveTo(x01,y01+amp);         //Рисуем ось Y
Canvas.LineTo(x01,y01-amp);
for i:=0 to imax do                         //Рисуем график
begin
func:=-round(amp*sin(2*pi/imax*i*dt2));
Canvas.Pixels[x01+i*dt1,y01+func]:=clBlack;
end;


// Рисуем график с LineTo
Canvas.TextOut(0,y02-amp-20,'   График функции с LineTo');
Canvas.MoveTo(x02,y02);                    //Рисуем ось X
Canvas.LineTo(x02+imax*dt1,y02);
Canvas.MoveTo(x02,y02+amp);             //Рисуем ось Y
Canvas.LineTo(x02,y02-amp);
canvas.Brush.color:= clred;
for i:=0 to imax do                         //Рисуем график
begin
func:=-round(amp*sin(2*pi/imax*i*dt2));
//Canvas.LineTo[x02+i*dt1,y02+func]:=clBlack;
  Canvas.LineTo(x02+i*dt1,y02+func);
end;
end;


end.

(* http://wiki.freepascal.org/TAChart_Tutorial:_Function_Series
   http://www.gamedev.ru/code/terms/Genetic_Algorithm
*)

