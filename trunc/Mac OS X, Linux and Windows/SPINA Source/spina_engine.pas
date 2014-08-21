unit SPINA_Engine;

{ SPINA-Thyr }

 { Application for calculating structure parameters }
 { of thyrotropic feedback control }

 { Programm zur Berechnung von Strukturparametern }
 { des thyreotropen Regelkreises }

{ Version 4.0.0 }

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
  BETAT  = 1.1e-6;
  THY    = 1;
  I      = 1;
  DT     = 2.75;
  KT     = 1;
  KI     = 1;
  TBG    = 3e-7;
  TBPA   = 4.5e-6;
  K30    = 2e9;
  K41    = 2e10;
  K42    = 2e8;
  ALPHA31 = 0.026;
  BETA31 = 8e-6;
  KM1    = 5e-7;

type
  tCaseRecord = record
    TSH, FT4, FT3, TT4, TT3: real;
    GT, GD, TSHI, rawTTSI, TTSI: real;
    TSH_UOM, FT4_UOM, FT3_UOM, TT4_UOM, TT3_UOM: Str255;
    GT_UOM, GD_UOM:  str255;
    GTs, GDs, flaggedGTs, flaggedGDs: Str255;
    TSHIs, TTSIs, flaggedTSHIs, flaggedTTSIs: Str255;
    CaseID, PID, Name, GivenNames, Placer: string;
    DoBDate, OBDate: TDateTime;
    TSHTherapy, T4Therapy, T3Therapy: boolean;
    MessageString: string[255];
  end;

var
  gNotCalculable: string;

procedure NewCaseRecord(var aCaseRecord: tCaseRecord);
procedure Calculate(var theCaseRecord: tCaseRecord);
procedure InsertTTSI(var theCase: tCaseRecord; FT4UpperLimit: real);
procedure FormatCase(var theCase: tCaseRecord; referenceRanges: tReferenceValues);

implementation

procedure NewCaseRecord(var aCaseRecord: tCaseRecord);
begin
  FillChar(aCaseRecord, SizeOf(tCaseRecord), 0); // empties record
  aCaseRecord.TSH  := NaN;
  aCaseRecord.FT4  := NaN;
  aCaseRecord.FT3  := NaN;
  aCaseRecord.TT4  := NaN;
  aCaseRecord.TT3  := NaN;
  aCaseRecord.GT   := NaN;
  aCaseRecord.GD   := NaN;
  aCaseRecord.TSHI := NaN;
  aCaseRecord.TTSI := NaN;
end;

procedure Calculate(var theCaseRecord: tCaseRecord);
 {main calculation function}
 {TSH is expected in mU/l, T4 and T3 in mol/l}
var
  GTFlag, GDFlag: string;
  T4, T3: real;
begin
  with theCaseRecord do
  begin
    GT   := NaN;
    GD   := NaN;
    TSHI := NaN;
    TTSI := NaN;
    if not isNan(TSH) and (TSH > 0) and not isNan(TT4) and not T4Therapy then
      GT := betaT * (DT + TSH) * TT4 / (alphaT * TSH) {total T4 used}
    else if not isNan(TSH) and (TSH > 0) and not isNan(FT4) and not T4Therapy then
    begin
      T4 := (1 + k41 * TBG + k42 * TBPA) * FT4; {free T4 used}
      GT := betaT * (DT + TSH) * T4 / (alphaT * TSH);
    end
    else
      GT := NaN;
    if not isNan(TT4) and (TT4 > 0) then
    begin
      if not isNan(TT3) and not T3Therapy then
      begin
        T4 := TT4 / (1 + k41 * TBG + k42 * TBPA);
        GD := beta31 * (kM1 + T4) * TT3 / (alpha31 * T4);
      end
      else if not isNan(FT3) and not T3Therapy then
      begin
        T4 := TT4 / (1 + k41 * TBG + k42 * TBPA);
        T3 := (1 + k30 * TBG) * FT3;
        GD := beta31 * (kM1 + T4) * T3 / (alpha31 * T4);
      end
      else
        GD := NaN;
    end
    else if not isNan(FT4) and (FT4 > 0) then
    begin
      if not isNan(TT3) and not T3Therapy then
      begin
        GD := beta31 * (kM1 + FT4) * TT3 / (alpha31 * FT4);
      end
      else if not isNan(FT3) and not T3Therapy then
      begin
        T3 := (1 + k30 * TBG) * FT3;
        GD := beta31 * (kM1 + FT4) * T3 / (alpha31 * FT4);
      end
      else
        GD := NaN;
    end;
    if not isNan(FT4) and (FT4 > 0) and not isNan(TSH) and (TSH > 0) and
      not TSHTherapy then
    begin
      TSHI := ln(TSH) + 0.1345 * FT4 * 1e12;
    end
    else
    begin
      TSHI := NaN;
    end;
  end;
end;

procedure InsertTTSI(var theCase: tCaseRecord; FT4UpperLimit: real);
begin
  if not isNan(theCase.FT4) and not isNan(theCase.TSH) and not
    isNan(FT4UpperLimit) and not theCase.TSHTherapy then
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
  GTFlag   := '';
  GDFlag   := '';
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
