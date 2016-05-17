unit VSDEparameters;

{$mode objfpc} {$H+}

{$RANGECHECKS ON}
{$DEBUGINFO ON}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids;

const
  NumOfRecords = 30;
  NumParam = 31;

type

  FloatType = Double;

  TVSDEParam = record
    on_off: array [1..NumParam] of boolean;
    name: array [1..NumParam] of string;
    current: array [1..NumParam] of FloatType;
    min_bounds: array [1..NumParam] of FloatType;
    max_bounds: array [1..NumParam] of FloatType;
    info: array [1..NumParam] of string;
  end;


  { TFormEditParameters }

  TFormEditParameters = class(TForm)
    EditParameters: TGroupBox;
    StringGridParameters: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;


var
  FormEditParameters: TFormEditParameters;
  VSDEParam: TVSDEParam;

procedure FillGrid;
procedure SaveGrid;
procedure LoadGrid;
procedure InitVSDEParam;

implementation

{$R *.lfm}

procedure InitVSDEParam;
begin
  with VSDEParam do
    begin
      info[1]:='T1   - Minimum temperature for tree growth';
      info[2]:='T2   - Lower end of range of optimal temperatures T2-T3';
      info[3]:='T3   - Upper end of range of optimal temperatures';
      info[4]:='T4   - Maximum temperature for tree growth';
      info[5]:='W4   - Maximum soil moisture for tree growth';
      info[6]:='     - Coefficient of temperature modulation T+b6';
      info[7]:='Tm   - Sum of temperature for start soil melting';
      info[8]:='sm1  - First coefficient of soil melting';
      info[9]:='sm2  - Second coefficient of soil melting';
      info[10]:='Wo   - Initial soil moisture';
      info[11]:='Pmax - Maximum daily precipitation for saturated soil';
      info[12]:='Wmin - Minimum soil moisture (wilting point)';
      info[13]:='lr   - Root depth';
      info[14]:='C2   - First coefficient for calculation of transpiration';
      info[15]:='C3   - Second coefficient for calculation of transpiration';
      info[16]:='C1   - Fraction of precip. penetrating soil (not caught by crown) (1-p16)';
      info[17]:='W1   - Minimum soil moisture for tree growth, relative to saturated soil (v/vs)';
      info[18]:='W2   - Lower end of range of optimal soil moistures (v/vs) W2-W3';
      info[19]:='W3   - Upper end of range of optimal soil moistures (v/vs)';
      info[20]:='W4   - Growth is stoped at this soil moisture';
      info[21]:='Cd   - Coefficient for water drainage from soil (rel. unit)';
      info[22]:='Tg   - Sum of temperature to start growth';
      info[23]:='Sno  - Initial snowpack';
      info[24]:='     - Rate of snow melting';
      info[25]:='     - Minimum temperature snow melting';
      info[26]:='     - Temperature correction on elevation';
      info[27]:='     - This parameter is changed by program';
      info[28]:='     - Coefficient of precipitation modification';
      info[29]:='     - This parameter is changed by program';
      info[30]:='     - Delta for parameter';
      info[31]:='	     - Coefficient of solar modification';

      name[1]:='T1';
      name[2]:='T2';
      name[3]:='T3';
      name[4]:='T4';
      name[5]:='W4';
      name[6]:='T+b6';
      name[7]:='Tm';
      name[8]:='sm1';
      name[9]:='sm2';
      name[10]:='Wo';
      name[11]:='Pmax';
      name[12]:='Wmin';
      name[13]:='lr';
      name[14]:='C2';
      name[15]:='C3';
      name[16]:='C1';
      name[17]:='W1';
      name[18]:='W2';
      name[19]:='W3';
      name[20]:='W4';
      name[21]:='Cd';
      name[22]:='Tg';
      name[23]:='Sno';
      name[24]:='   ';
      name[25]:='   ';
      name[26]:='   ';
      name[27]:='   ';
      name[28]:='   ';
      name[29]:='   ';
      name[30]:='   ';
      name[31]:='   ';

      min_bounds[1] := 1.50;
      min_bounds[2] := 15.0;
      min_bounds[3] := 15.0;
      min_bounds[4] := 25.0;
      min_bounds[5] := 0.250;
      min_bounds[6] := 0.0;
      min_bounds[7] := 19.0;
      min_bounds[8] := 9.0;
      min_bounds[9] := 0.0040;
      min_bounds[10]:= 0.0010;
      min_bounds[11]:= 30.0;
      min_bounds[12]:= 0.040;
      min_bounds[13]:= 400.0;
      min_bounds[14]:= 0.110;
      min_bounds[15]:= 0.1650;
      min_bounds[16]:= 0.520;
      min_bounds[17]:= 0.020;
      min_bounds[18]:= 0.10;
      min_bounds[19]:= 0.60;
      min_bounds[20]:= 0.060;
      min_bounds[21]:= 0.0;
      min_bounds[22]:= 90.0;
      min_bounds[23]:= 6.90;
      min_bounds[24]:= 0.0;
      min_bounds[25]:= -3.10;
      min_bounds[26]:= 0.0;
      min_bounds[27]:= 0.0;
      min_bounds[28]:= 1.0;
      min_bounds[29]:= 0.0;
      min_bounds[30]:= 0.50;
      min_bounds[31]:= 0.50;

      max_bounds[1] := 10.0;
      max_bounds[2] := 20.0;
      max_bounds[3] := 22.0;
      max_bounds[4] := 29.0;
      max_bounds[5] := 0.450;
      max_bounds[6] := 0.10;
      max_bounds[7] := 20.0;
      max_bounds[8] := 11.0;
      max_bounds[9] := 0.070;
      max_bounds[10]:= 0.10;
      max_bounds[11]:= 40.0;
      max_bounds[12]:= 0.090;
      max_bounds[13]:= 500.0;
      max_bounds[14]:= 0.120;
      max_bounds[15]:= 0.1850;
      max_bounds[16]:= 0.820;
      max_bounds[17]:= 0.050;
      max_bounds[18]:= 0.30;
      max_bounds[19]:= 0.80;
      max_bounds[20]:= 1.0;
      max_bounds[21]:= -0.10;
      max_bounds[22]:= 110.0;
      max_bounds[23]:= 8.90;
      max_bounds[24]:= 2.10;
      max_bounds[25]:= 3.10;
      max_bounds[26]:= 5.0;
      max_bounds[27]:= 5.0;
      max_bounds[28]:= 2.0;
      max_bounds[29]:= 2.0;
      max_bounds[30]:= 1.0;
      max_bounds[31]:= 2.0;

      current[1] := 5.0;
      current[2] := 18.0;
      current[3] := 20.0;
      current[4] := 27.0;
      current[5] := 0.35;
      current[6] := 0.0;
      current[7] := 20.0;
      current[8] := 10.0;
      current[9] := 0.0060;
      current[10]:= 0.09330;
      current[11]:= 40.0;
      current[12]:= 0.070;
      current[13]:= 500.0;
      current[14]:= 0.120;
      current[15]:= 0.1750;
      current[16]:= 0.720;
      current[17]:= 0.040;
      current[18]:= 0.20;
      current[19]:= 0.80;
      current[20]:= 0.090;
      current[21]:= 0.0;
      current[22]:= 100.0;
      current[23]:= 7.90;
      current[24]:= 1.0;
      current[25]:= 2.0;
      current[26]:= 1.0;
      current[27]:= 0.0;
      current[28]:= 1.0;
      current[29]:= 0.0;
      current[30]:= 0.5;
      current[31]:= 1.0;
    end;
end;

procedure FillGrid;
var
  i : integer;
begin
  with FormEditParameters.StringGridParameters do
    begin
      RowCount := NumParam + 1; (* Top row is for headings *)
      cells[0,0] := 'Name'; (* имя левого столбца *)
      for i := 1 to NumOfRecords do
        begin
          with VSDEParam do
            begin
              Cells[0, i] := name[i];
              Cells[1, i] := FloattostrF(min_bounds[i],ffFixed,6,2);
              Cells[2, i] := FloattostrF(current[i],ffFixed,6,2);;
              Cells[3, i] := FloattostrF(max_bounds[i],ffFixed,6,2);;
              Cells[4, i] := info[i];
            end;
        end;
     end;
end;


procedure SaveGrid;
begin
   FormEditParameters.StringGridParameters.SaveToCSVFile('parameters.csv');
end;

procedure LoadGrid;
begin
   FormEditParameters.StringGridParameters.LoadFromCSVFile('parameters.csv');
end;

procedure TFormEditParameters.FormCreate(Sender: TObject);
begin
  //FormEditParameters := TFormEditParameters.Create(Self);
  //Application.MessageBox(PChar('В процессе разработки....'), 'Внимание',0);
  //FillGrid;
  InitVSDEParam;
end;


procedure TFormEditParameters.FormDestroy(Sender: TObject);
begin

end;

end.

(*
Tips
http://www.pp4s.co.uk/main/tu-form-stringgrid_demo.html
http://freepascal.ru/article/lazarus/20050425000000/
*)
