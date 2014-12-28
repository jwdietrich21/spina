{ SPINA-Thyr }
{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }
{ Version 3.2 }

{ J. W. Dietrich, Klinikum der LMU München 1997-2001 }
{ J. W. Dietrich, Universitätsklinikum Ulm 2002-2004 }
{ J. W. Dietrich, Universitätsklinikum Bergmannsheil 2005-2010 }

{ This software is provided via a BSD licence }
{ See http://spina.medical-cybernetics.de for details }

unit spina_engine;

interface

uses PSL, SysUtils;

const
  UT4 = 1.28e-11;
  UT3 = 1.54e-12;
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
  tCaseRecord = record
	TSH, T4, T3, GT, GD: real;
	GTs, GDs, MessageString: String;
        end;
  tLabMethod = (freeHormone, totalHormone);
  tPreferences = record
       T4Method, T3Method: tLabMethod;
       TSHUnitFactor, T4UnitFactor, T3UnitFactor: real;
       TSHPopUpItem, T4PopUpItem, T3PopUpItem: integer;
       T4MethodPopUpItem, T3MethodPopUpItem: integer;
       end;

var
  gcalcTitle, gcalcString, gnotcalculatableString: String;
  gNotCalculatable, gVerhaltensparameter, gStrukturparameter: String;
  gCaseRecord: tCaseRecord;
  gPreferences: tPreferences;

procedure Calculate(TSH, T4, T3: real);

implementation

procedure Calculate(TSH, T4, T3: real);
var
  FT4, TT4, TT3: real;
begin
if TSH > 0 then
   begin
     case gPreferences.T4Method of
     freeHormone:
        TT4 := (1 + k41 * TBG + k42 * TBPA) * T4; {T4 = FT4}
     totalHormone:
        TT4 := T4                                 {T4 = TT4}
     end;
     gCaseRecord.GT := betaT * (DT + TSH) * TT4 / (alphaT * TSH);
     gCaseRecord.GTs:=FloatToStr(1e12*gCaseRecord.GT, 2);
     gCaseRecord.GTs:=gCaseRecord.GTs + ' pmol/s';
    end
else
   gCaseRecord.GTs := gNotCalculatable;
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
     gCaseRecord.GD := beta31 * (kM1 + FT4) * TT3 / (alpha31 * FT4);
     gCaseRecord.GDs:=FloatToStr(1e9*gCaseRecord.GD, 2);
     gCaseRecord.GDs:=gCaseRecord.GDs + ' nmol/s';
   end
else
   gCaseRecord.GDs := gNotCalculatable;
end;

end.