unit SPINA_Engine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

const
UFT4 = 1.28e-11;
UFT3 = 1.54e-12;
ALPHAT = 0.1;
BETAT = 1.1e-6;
THY = 1;
I = 1;
DT = 2.75;
KT = 1;
KI = 1;
TBG = 3e-7;
TBPA = 4.5e-6;
K30 = 2e9;
K41 = 2e10;
K42 = 2e8;
ALPHA31 = 0.026;
BETA31 = 8e-6;
KM1 = 5e-7;

type
  Str255 = String[255];
  tLabMethod = (freeHormone, totalHormone);
  tCaseRecord = record
	TSH, T4, T3, GT, GD: real;
        GTs, GDs: Str255;
	MessageString: String[255];
        end;
  tCodeList = set of 0..255;
  tPreferences = record
         T4Method, T3Method: tLabMethod;
         TSHUnitFactor, T4UnitFactor, T3UnitFactor: real;
         TSHPopUpItem, T4PopUpItem, T3PopUpItem: integer;
         T4MethodPopUpItem, T3MethodPopUpItem: integer;
         end;

var
  gNotCalculatable: String;
  gPreferences: tPreferences;

function Calculate(TSH, T4, T3: real): tCaseRecord;

implementation

function Calculate(TSH, T4, T3: real): tCaseRecord;
var
  FT4, TT4, TT3: real;
  tempRecord: tCaseRecord;
begin
if TSH > 0 then
   begin
   case gPreferences.T4Method of
   freeHormone:
      TT4 := (1 + k41 * TBG + k42 * TBPA) * T4; {T4 = FT4}
   totalHormone:
      TT4 := T4                                 {T4 = TT4}
   end;
   tempRecord.GT := betaT * (DT + TSH) * TT4 / (alphaT * TSH);
   tempRecord.GTs:=FloatToStrF(1e12*tempRecord.GT, ffFixed, 5,2);
   tempRecord.GTs:=concat(tempRecord.GTs,' pmol/s');
    end
else
   tempRecord.GTs := gNotCalculatable;
if T4 > 0 then
   begin
   case gPreferences.T4Method of
   freeHormone:
     FT4 := T4;
   totalHormone:
     FT4 := T4 / (1 + k41 * TBG + k42 * TBPA);
   end;
   case gPreferences.T3Method of
   freeHormone:
     TT3 := (1 + k30 * TBG) * T3;
   totalHormone:
     TT3 := T3
   end;
   tempRecord.GD := beta31 * (kM1 + FT4) * TT3 / (alpha31 * FT4);
   tempRecord.GDs:=FloatToStrF(1e9*tempRecord.GD, ffFixed, 5,2);
   tempRecord.GDs:=concat(tempRecord.GDs,' nmol/s');
   end
else
   tempRecord.GDs := gNotCalculatable;
  Calculate := tempRecord;
end;

end.
