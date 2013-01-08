unit SPINA_Engine;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.3 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit implements the calculation engine }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SPINA_Types;

const
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
    GTs, GDs, flaggedGTs, flaggedGDs: Str255;
    MessageString: string[255];
  end;


var
  gNotCalculable: string;

function Calculate(TSH, T4, T3: real): tCaseRecord;

implementation

function Calculate(TSH, T4, T3: real): tCaseRecord;
{main calculation function}
{TSH is expected in mU/l, T4 and T3 in mol/l}
var
  FT4, TT4, TT3: real;
  tempRecord: tCaseRecord;
  GTFlag, GDFlag: string;
begin
  GTFlag := '';
  GDFlag := '';
  if (TSH > 0) and not gT4Therapy then
  begin
    case gPreferences.T4Method of
      freeHormone:
        TT4 := (1 + k41 * TBG + k42 * TBPA) * T4; {T4 = FT4}
      totalHormone:
        TT4 := T4                                 {T4 = TT4}
    end;
    tempRecord.GT := betaT * (DT + TSH) * TT4 / (alphaT * TSH);
    tempRecord.GTs := FloatToStrF(1e12 * tempRecord.GT, ffFixed, 5, 2);
    tempRecord.GTs := concat(tempRecord.GTs, ' pmol/s');
    if (tempRecord.GT < gReferenceRanges.GT.ln) or (tempRecord.GT >
      gReferenceRanges.GT.hn) then
      GTFlag := '*';
    tempRecord.flaggedGTs := FloatToStrF(1e12 * tempRecord.GT, ffFixed, 5, 2);
    tempRecord.flaggedGTs := concat(tempRecord.flaggedGTs, GTFlag, ' pmol/s ');
  end
  else
  begin
    tempRecord.GTs := gNotCalculable;
    tempRecord.flaggedGTs := gNotCalculable;
  end;
  if (T4 > 0) and not gT3Therapy then
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
    tempRecord.GDs := FloatToStrF(1e9 * tempRecord.GD, ffFixed, 5, 2);
    tempRecord.GDs := concat(tempRecord.GDs, ' nmol/s');
    if (tempRecord.GD < gReferenceRanges.GD.ln) or (tempRecord.GD >
      gReferenceRanges.GD.hn) then
      GDFlag := '*';
    tempRecord.flaggedGDs := FloatToStrF(1e9 * tempRecord.GD, ffFixed, 5, 2);
    tempRecord.flaggedGDs := concat(tempRecord.flaggedGDs, GDFlag, ' nmol/s ');
  end
  else
  begin
    tempRecord.GDs := gNotCalculable;
    tempRecord.flaggedGDs := gNotCalculable;
  end;
  Calculate := tempRecord;
end;

end.
