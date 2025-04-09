unit CaseBroker;

{ SPINA Carb }

{ Application for calculating structure parameters }
{ of insulin-glucose feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des Insulin-Glukose-Regelkreises }

{ Version 5.1.0 (Cyclone) }

{ (c) J. W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ This unit filters and prepares inputs for the calculation engine, and it }
{ provides several services for high-level units }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, UnitConverter, SPINATypes, SPINA_Engine;

type
  tLabRecord = record
    Insulin, Glucose, CPeptide: extended;
    SPINA_GBeta, SPINA_GR, SPINA_DI, HOMA_Beta, HOMA_IR, HOMA_IS,
    QUICKI, AIGR, CGR: extended;
  end;

  tCaseRecord = record
    LabRecord: tLabRecord;
    CaseID, PID, Name, GivenNames, Placer: string;
    DoBDate, OBDate, OBTime: TDateTime;
    InsulinTherapy: boolean;
    Comment, BParMessage, SParMessage, CombMessage: string;
    BRefMessage1, BRefMessage2, SRefMessage1, SRefMessage2: string;
    RCombMessage1, RCombMessage2: string;
  end;

procedure NewCaseRecord(var aCaseRecord: tCaseRecord);
function InsulinSI(RawIns: extended; InsUom: string): extended;
function GlucoseSI(RawGlc: extended; GlcUom: string): extended;
function CPeptideSI(RawCPt: extended; CPtUom: string): extended;
procedure Calculate(var LabRecord: tLabRecord);
procedure CreateMessages(var CaseRecord: tCaseRecord);
function RRFlag(theParameter: extended; theRanges: tReferenceLimits): string;
function Marked(theParameter: extended; theRanges: tReferenceLimits;
  Precision: integer; Digits: integer): string;
function MarkedC(theParameter: extended; theRanges: tReferenceLimits;
  ConversionFactor: real; UoM1, Uom2: string; Precision: integer;
  Digits: integer): string;

implementation

procedure NewCaseRecord(var aCaseRecord: tCaseRecord);
{Delivers an empty case record}
begin
  FillChar(aCaseRecord, SizeOf(tCaseRecord), 0); // empties record
  aCaseRecord.LabRecord.Glucose := NaN;
  aCaseRecord.LabRecord.Insulin := NaN;
  aCaseRecord.LabRecord.CPeptide := NaN;
  aCaseRecord.LabRecord.SPINA_GBeta := NaN;
  aCaseRecord.LabRecord.SPINA_GR := NaN;
  aCaseRecord.LabRecord.SPINA_DI := NaN;
  aCaseRecord.LabRecord.HOMA_Beta := NaN;
  aCaseRecord.LabRecord.HOMA_IR := NaN;
  aCaseRecord.LabRecord.HOMA_IS := NaN;
  aCaseRecord.LabRecord.QUICKI := NaN;
  aCaseRecord.LabRecord.AIGR := NaN;
  aCaseRecord.LabRecord.CGR := NaN;
end;

function InsulinSI(RawIns: extended; InsUom: string): extended;
begin
  Result := ConvertedValue(RawIns, kInsulinConversionFactor, InsUom, 'pmol/l');
end;

function GlucoseSI(RawGlc: extended; GlcUom: string): extended;
begin
  Result := ConvertedValue(RawGlc, kGlucoseMolarMass, GlcUom, 'mmol/l');
end;

function CPeptideSI(RawCPt: extended; CPtUom: string): extended;
begin
  Result := ConvertedValue(RawCPt, kCPeptideMolarMass, CPtUom, 'nmol/l');
end;

procedure Calculate(var LabRecord: tLabRecord);
begin
  LabRecord.SPINA_GBeta := SPINA_GBeta(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.SPINA_GR := SPINA_GR(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.SPINA_DI := SPINA_DI(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.HOMA_Beta := HOMA_Beta(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.HOMA_IR := HOMA_IR(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.HOMA_IS := HOMA_IS(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.QUICKI := QUICKI(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.AIGR := AIGR(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.CGR := CGR(LabRecord.CPeptide, LabRecord.Glucose);
end;

procedure CreateMessages(var CaseRecord: tCaseRecord);
{Creates messages for calculated parameters}
begin
  CaseRecord.SParMessage := kSPars + LineEnding + '   ' + kSPINA_GBeta +
    ': ' + Marked(CaseRecord.LabRecord.SPINA_GBeta,
    gPreferences.ReferenceValues.SPINA_GBeta, 4, 2) + ' ' + GBetaUoM +
    LineEnding + '   ' + kSPINA_GR + ': ' + Marked(CaseRecord.LabRecord.SPINA_GR,
    gPreferences.ReferenceValues.SPINA_GR, 4, 2) + ' ' + GRUoM +
    LineEnding + '   ' + kSPINA_DI + ': ' + Marked(CaseRecord.LabRecord.SPINA_DI,
    gPreferences.ReferenceValues.SPINA_DI, 4, 2) + LineEnding + '   ' +
    kHOMA_Beta + ': ' + Marked(CaseRecord.LabRecord.HOMA_Beta,
    gPreferences.ReferenceValues.HOMA_Beta, 4, 1) + ' ' + HOMABetaUoM +
    LineEnding + '   ' + kHOMA_IR + ': ' + Marked(CaseRecord.LabRecord.HOMA_IR,
    gPreferences.ReferenceValues.HOMA_IR, 4, 1) + LineEnding + '   ' +
    kHOMA_IS + ': ' + Marked(CaseRecord.LabRecord.HOMA_IS,
    gPreferences.ReferenceValues.HOMA_IS, 4, 1) + LineEnding + '   ' +
    kQUICKI + ': ' + Marked(CaseRecord.LabRecord.QUICKI,
    gPreferences.ReferenceValues.QUICKI, 4, 1) + LineEnding + '   ' +
    kAIGR + ': ' + Marked(CaseRecord.LabRecord.AIGR, gPreferences.ReferenceValues.AIGR,
    4, 1) + ' ' + AIGRUoM + LineEnding + '   ' + kCGR + ': ' +
    Marked(CaseRecord.LabRecord.CGR, gPreferences.ReferenceValues.CGR, 4, 1);
  CaseRecord.SRefMessage1 :=
    FloatToStrF(gPreferences.ReferenceValues.SPINA_GBeta.ln, ffFixed, 4, 2) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.SPINA_GBeta.hn, ffFixed, 4, 2) +
    ' ' + gPreferences.ReferenceValues.SPINA_GBeta.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.SPINA_GR.ln, ffFixed, 4, 2) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.SPINA_GR.hn, ffFixed, 4, 2) +
    ' ' + gPreferences.ReferenceValues.SPINA_GR.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.SPINA_DI.ln, ffFixed, 4, 2) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.SPINA_DI.hn, ffFixed, 4, 2) +
    ' ' + gPreferences.ReferenceValues.SPINA_DI.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.HOMA_Beta.ln, ffFixed, 4, 1) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.HOMA_Beta.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.HOMA_Beta.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.HOMA_IR.ln, ffFixed, 4, 1) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.HOMA_IR.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.HOMA_IR.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.HOMA_IS.ln, ffFixed, 4, 1) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.HOMA_IS.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.HOMA_IS.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.QUICKI.ln, ffFixed, 4, 1) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.QUICKI.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.QUICKI.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.AIGR.ln, ffFixed, 4, 1) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.AIGR.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.AIGR.UoM + LineEnding + FloatToStrF(
    gPreferences.ReferenceValues.CGR.ln, ffFixed, 4, 1) + '–' +
    FloatToStrF(gPreferences.ReferenceValues.CGR.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.CGR.UoM;
end;

function RRFlag(theParameter: extended; theRanges: tReferenceLimits): string;
begin
  if (isNan(theRanges.ln) or isNan(theRanges.hn)) then
    Result := ''
  else if (theParameter < theRanges.ln) or (theParameter > theRanges.hn) then
    Result := REF_RANGE_FLAG;
end;

function Marked(theParameter: extended; theRanges: tReferenceLimits;
  Precision: integer; Digits: integer): string;
begin
  if IsNaN(theParameter) then
    Result := NA_MARK
  else
    Result := FloatToStrF(theParameter, ffFixed, Precision, Digits) +
      RRFlag(theParameter, theRanges);
end;

function MarkedC(theParameter: extended; theRanges: tReferenceLimits;
  ConversionFactor: real; UoM1, Uom2: string; Precision: integer;
  Digits: integer): string;
var
  LocalRange: tReferenceLimits;
begin
  LocalRange.ln := ConvertedValue(theRanges.ln, ConversionFactor, UoM1, Uom2);
  LocalRange.hn := ConvertedValue(theRanges.hn, ConversionFactor, UoM1, Uom2);
  Result := Marked(ConvertedValue(theParameter, ConversionFactor, UoM1, UoM2),
    LocalRange, Precision, Digits);
end;

end.
