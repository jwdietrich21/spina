unit SPINA_Engine;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.5.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ This unit implements the calculation engine }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SPINA_Types, Math;

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
    TSH, FT4, FT3, TT4, TT3: real;
    GT, GD, TSHI, rawTTSI, TTSI: real;
    TSH_UOM, FT4_UOM, FT3_UOM, TT4_UOM, TT3_UOM: Str255;
    GT_UOM, GD_UOM: str255;
    GTs, GDs, flaggedGTs, flaggedGDs: Str255;
    TSHIs, TTSIs, flaggedTSHIs, flaggedTTSIs: Str255;
    CaseID, PID, Name, GivenNames, Placer: string;
    DoBDate, OBDate: TDateTime;
    MessageString: string[255];
  end;

var
  gNotCalculable: string;

function Calculate(TSH, T4, T3: real): tCaseRecord;
procedure InsertTTSI(var theCase: tCaseRecord; FT4UpperLimit: real);
procedure FormatCase(var theCase: tCaseRecord; referenceRanges: tReferenceValues);

implementation

function Calculate(TSH, T4, T3: real): tCaseRecord;
  {main calculation function}
  {TSH is expected in mU/l, T4 and T3 in mol/l}
var
  tempRecord: tCaseRecord;
  GTFlag, GDFlag: string;
  FT4, TT4, TT3: real;
begin
  FillChar(tempRecord, SizeOf(tCaseRecord), 0); // empties record
  case gPreferences.T4.Method of
    freeHormone:
    begin
      tempRecord.FT4 := T4;
      tempRecord.TT4 := NaN;
    end;
    totalHormone:
    begin
      tempRecord.TT4 := T4;
      tempRecord.FT4 := NaN;
    end;
  end;
  case gPreferences.T3.Method of
    freeHormone:
    begin
      tempRecord.FT3 := T3;
      tempRecord.TT3 := NaN;
    end;
    totalHormone:
    begin
      tempRecord.TT3 := T3;
      tempRecord.FT3 := NaN;
    end;
  end;
  tempRecord.TSH := TSH;
  if not isNan(TSH) and (TSH > 0) and not isNan(T4) and not gT4Therapy then
  begin
    case gPreferences.T4.Method of
      freeHormone:
        TT4 := (1 + k41 * TBG + k42 * TBPA) * T4; {T4 = FT4}
      totalHormone:
        TT4 := T4                                 {T4 = TT4}
    end;
    tempRecord.GT := betaT * (DT + TSH) * TT4 / (alphaT * TSH);
  end
  else
    tempRecord.GT := NaN;
  if not isNan(T4) and (T4 > 0) and not isNan(T3) and not gT3Therapy then
  begin
    case gPreferences.T4.Method of
      freeHormone:
        FT4 := T4;
      totalHormone:
        FT4 := T4 / (1 + k41 * TBG + k42 * TBPA);
    end;
    case gPreferences.T3.Method of
      freeHormone:
        TT3 := (1 + k30 * TBG) * T3;
      totalHormone:
        TT3 := T3
    end;
    tempRecord.GD := beta31 * (kM1 + FT4) * TT3 / (alpha31 * FT4);
  end
  else
    tempRecord.GD := NaN;
  if (gPreferences.T4.Method = freeHormone) and not isNan(TSH) and (TSH > 0)
    and not isNan(T4)  and not gTSHTherapy then
  begin
    tempRecord.TSHI := ln(TSH) + 0.1345 * T4 * 1e12;
  end
  else
  begin
    tempRecord.TSHI := NaN;
  end;
  Calculate := tempRecord;
end;

procedure InsertTTSI(var theCase: tCaseRecord; FT4UpperLimit: real);
begin
  if (gPreferences.T4.Method = freeHormone) and not isNan(theCase.TSH) and
    not isNan(FT4UpperLimit) and not isNan(theCase.FT4) and not gTSHTherapy then
  begin
    theCase.TTSI := 100 * theCase.TSH * theCase.FT4 / FT4UpperLimit;
  end
  else
  begin
    theCase.TTSI := NaN;
  end;
  if not isNaN(theCase.TTSI) then
  begin
    theCase.TTSIs := FloatToStrF(theCase.TTSI, ffFixed, 5, 2);
    theCase.flaggedTTSIs := FloatToStrF(theCase.TTSI, ffFixed, 5, 2);
  end
  else
  begin
    theCase.TTSIs := gNotCalculable;
    theCase.flaggedTTSIs := gNotCalculable;
  end;
end;

procedure FormatCase(var theCase: tCaseRecord; referenceRanges: tReferenceValues);
var
  GTFlag, GDFlag, TSHIFlag, TTSIFlag: string;
begin
  GTFlag := '';
  GDFlag := '';
  TSHIFlag := '';
  TTSIFlag := '';
  if not isNaN(theCase.GT) then
  begin
    theCase.GTs := FloatToStrF(1e12 * theCase.GT, ffFixed, 5, 2);
    theCase.GTs := concat(theCase.GTs, ' pmol/s');
    if not isNaN(gReferenceRanges.GT.ln) and not isNan(gReferenceRanges.GT.hn) and
      ((theCase.GT < gReferenceRanges.GT.ln) or (theCase.GT >
      gReferenceRanges.GT.hn)) then
      GTFlag := REF_RANGE_FLAG;
    theCase.flaggedGTs := FloatToStrF(1e12 * theCase.GT, ffFixed, 5, 2);
    theCase.flaggedGTs := concat(theCase.flaggedGTs, GTFlag, ' pmol/s ');
  end
  else
  begin
    theCase.GTs := gNotCalculable;
    theCase.flaggedGTs := gNotCalculable;
  end;
  if not isNaN(theCase.GD) then
  begin
    theCase.GDs := FloatToStrF(1e9 * theCase.GD, ffFixed, 5, 2);
    theCase.GDs := concat(theCase.GDs, ' nmol/s');
    if not isNaN(gReferenceRanges.GD.ln) and not isNan(gReferenceRanges.GD.hn) and
      ((theCase.GD < gReferenceRanges.GD.ln) or
      (theCase.GD > gReferenceRanges.GD.hn)) then
      GDFlag := REF_RANGE_FLAG;
    theCase.flaggedGDs := FloatToStrF(1e9 * theCase.GD, ffFixed, 5, 2);
    theCase.flaggedGDs := concat(theCase.flaggedGDs, GDFlag, ' nmol/s ');
  end
  else
  begin
    theCase.GDs := gNotCalculable;
    theCase.flaggedGDs := gNotCalculable;
  end;
  if not isNaN(theCase.TSHI) then
  begin
    theCase.TSHIs := FloatToStrF(theCase.TSHI, ffFixed, 5, 1);
    if not isNaN(gReferenceRanges.TSHI.ln) and not isNan(gReferenceRanges.TSHI.hn) and
      ((theCase.TSHI < gReferenceRanges.TSHI.ln) or
      (theCase.TSHI > gReferenceRanges.TSHI.hn)) then
      TSHIFlag := REF_RANGE_FLAG;
    theCase.flaggedTSHIs := FloatToStrF(theCase.TSHI, ffFixed, 5, 1);
    theCase.flaggedTSHIs := concat(theCase.flaggedTSHIs, TSHIFlag);
  end
  else
  begin
    theCase.TSHIs := gNotCalculable;
    theCase.flaggedTSHIs := gNotCalculable;
  end;
  if not isNaN(theCase.TTSI) then
  begin
    theCase.TTSIs := FloatToStrF(theCase.TTSI, ffFixed, 5, 0);
    if not isNaN(gReferenceRanges.TTSI.ln) and not isNan(gReferenceRanges.TTSI.hn) and
      ((theCase.TTSI < gReferenceRanges.TTSI.ln) or
      (theCase.TTSI > gReferenceRanges.TTSI.hn)) then
      TTSIFlag := REF_RANGE_FLAG;
    theCase.flaggedTTSIs := FloatToStrF(theCase.TTSI, ffFixed, 5, 0);
    theCase.flaggedTTSIs := concat(theCase.flaggedTTSIs, TTSIFlag);
  end
  else
  begin
    theCase.TTSIs := gNotCalculable;
    theCase.flaggedTTSIs := gNotCalculable;
  end;
end;

end.
