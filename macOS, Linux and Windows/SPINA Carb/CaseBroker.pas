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
  Classes, SysUtils, Math, UnitConverter, SPINATypes, SPINA_Resources,
  LocaleServices, SPINA_Engine;

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
function ConvertedAIGR(Value: extended; UoM1, UoM2: string): extended;
function MarkedA(theParameter: extended; theRanges: tReferenceLimits;
  UoM1, Uom2: string; Precision: integer; Digits: integer): string;
function FormattedResults(const LabRecord: tLabRecord): string;

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
var
  correctedConvFac: real;
begin
  correctedConvFac := 1e9 / kInsulinConversionFactor;
  // correct for the empiric nature of the conversion factor
  Result := ConvertedValue(RawIns, correctedConvFac, InsUom, kEngineUoMs.Insulin);
end;

function GlucoseSI(RawGlc: extended; GlcUom: string): extended;
begin
  Result := ConvertedValue(RawGlc, kGlucoseMolarMass, GlcUom, kEngineUoMs.Glucose);
end;

function CPeptideSI(RawCPt: extended; CPtUom: string): extended;
begin
  Result := ConvertedValue(RawCPt, kCPeptideMolarMass, CPtUom, kEngineUoMs.CPeptide);
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

function RefMessage(ln, hn: extended; UoM: string; precision, digits: integer): string;
  {Converts message with reference ranges to a form that is better human-readable}
begin
  if (ln = 0) or IsInfinite(ln) then
    Result := '< ' + FloatToStrF(hn, ffFixed, precision, digits)
  else if IsInfinite(hn) then
    Result := '> ' + FloatToStrF(ln, ffFixed, precision, digits)
  else
    Result := FloatToStrF(ln, ffFixed, precision, digits) + '–' +
      FloatToStrF(hn, ffFixed, precision, digits);
  if UoM <> '' then
    Result := Result + ' ' + UoM;
end;

procedure CreateMessages(var CaseRecord: tCaseRecord);
{Creates messages for calculated parameters}
var
  SPars: string;
begin
  if gPreferredLanguage = 'de' then
    SPars := KSPars_de
  else
    SPars := KSPars_en;
  CaseRecord.SParMessage := SPars + LineEnding + '   ' + kSPINA_GBeta +
    ': ' + Marked(CaseRecord.LabRecord.SPINA_GBeta,
    gPreferences.ReferenceValues.SPINA_GBeta, 4, 2) + ' ' + GBetaUoM +
    LineEnding + '   ' + kSPINA_GR + ': ' +
    Marked(CaseRecord.LabRecord.SPINA_GR, gPreferences.ReferenceValues.SPINA_GR, 4, 2) +
    ' ' + GRUoM + LineEnding + '   ' + kSPINA_DI + ': ' +
    Marked(CaseRecord.LabRecord.SPINA_DI, gPreferences.ReferenceValues.SPINA_DI, 4, 2) +
    LineEnding + '   ' + kHOMA_Beta + ': ' +
    Marked(CaseRecord.LabRecord.HOMA_Beta, gPreferences.ReferenceValues.HOMA_Beta,
    4, 1) + ' ' + HOMABetaUoM + LineEnding + '   ' + kHOMA_IR + ': ' +
    Marked(CaseRecord.LabRecord.HOMA_IR, gPreferences.ReferenceValues.HOMA_IR, 4, 1) +
    LineEnding + '   ' + kHOMA_IS + ': ' + Marked(CaseRecord.LabRecord.HOMA_IS,
    gPreferences.ReferenceValues.HOMA_IS, 4, 1) + LineEnding + '   ' +
    kQUICKI + ': ' + Marked(CaseRecord.LabRecord.QUICKI,
    gPreferences.ReferenceValues.QUICKI, 4, 1) + LineEnding + '   ' +
    kAIGR + ': ' + MarkedA(CaseRecord.LabRecord.AIGR, gPreferences.ReferenceValues.AIGR,
    kEngineUoMs.AIGR, gPreferences.ReferenceValues.AIGR.UoM, 4, 1) +
    ' ' + gPreferences.ReferenceValues.AIGR.UoM + LineEnding + '   ' +
    kCGR + ': ' + Marked(CaseRecord.LabRecord.CGR, gPreferences.ReferenceValues.CGR,
    4, 1) + ' ' + CGRUoM;
  CaseRecord.SRefMessage1 :=
    RefMessage(gPreferences.ReferenceValues.SPINA_GBeta.ln,
    gPreferences.ReferenceValues.SPINA_GBeta.hn,
    gPreferences.ReferenceValues.SPINA_GBeta.UoM, 4, 2) + LineEnding +
    RefMessage(gPreferences.ReferenceValues.SPINA_GR.ln,
    gPreferences.ReferenceValues.SPINA_GR.hn,
    gPreferences.ReferenceValues.SPINA_GR.UoM, 4, 2) + LineEnding +
    RefMessage(gPreferences.ReferenceValues.SPINA_DI.ln,
    gPreferences.ReferenceValues.SPINA_DI.hn,
    gPreferences.ReferenceValues.SPINA_DI.UoM, 4, 2) + LineEnding +
    RefMessage(gPreferences.ReferenceValues.HOMA_Beta.ln,
    gPreferences.ReferenceValues.HOMA_Beta.hn,
    gPreferences.ReferenceValues.HOMA_Beta.UoM, 4, 1) + LineEnding +
    RefMessage(gPreferences.ReferenceValues.HOMA_IR.ln,
    gPreferences.ReferenceValues.HOMA_IR.hn, gPreferences.ReferenceValues.HOMA_IR.UoM,
    4, 1) + LineEnding + RefMessage(gPreferences.ReferenceValues.HOMA_IS.ln,
    gPreferences.ReferenceValues.HOMA_IS.hn, gPreferences.ReferenceValues.HOMA_IS.UoM,
    4, 1) + LineEnding + RefMessage(gPreferences.ReferenceValues.QUICKI.ln,
    gPreferences.ReferenceValues.QUICKI.hn, gPreferences.ReferenceValues.QUICKI.UoM,
    4, 1) + LineEnding + RefMessage(gPreferences.ReferenceValues.AIGR.ln,
    gPreferences.ReferenceValues.AIGR.hn, gPreferences.ReferenceValues.AIGR.UoM,
    4, 1) + LineEnding + RefMessage(gPreferences.ReferenceValues.CGR.ln,
    gPreferences.ReferenceValues.CGR.hn, gPreferences.ReferenceValues.CGR.UoM, 4, 1);
end;

function RRFlag(theParameter: extended; theRanges: tReferenceLimits): string;
  {Adds predefined flag for results outside their respective reference ranges}
begin
  Result := '';
  if isNan(theParameter) or isNan(theRanges.ln) or isNan(theRanges.hn) then
    Result := ''
  else if (theParameter < theRanges.ln) or (theParameter > theRanges.hn) then
    Result := REF_RANGE_FLAG;
end;

function Marked(theParameter: extended; theRanges: tReferenceLimits;
  Precision: integer; Digits: integer): string;
  {Delivers formatted result}
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
  {Converts units and delivers formatted result}
var
  LocalRange: tReferenceLimits;
begin
  LocalRange.ln := ConvertedValue(theRanges.ln, ConversionFactor, UoM1, Uom2);
  LocalRange.hn := ConvertedValue(theRanges.hn, ConversionFactor, UoM1, Uom2);
  Result := Marked(ConvertedValue(theParameter, ConversionFactor, UoM1, UoM2),
    LocalRange, Precision, Digits);
end;

function ConvertedAIGR(Value: extended; UoM1, UoM2: string): extended;
const
  kUnit1 = 'pmol/mmol';
  kUnit2 = 'miu/mmol';      // lowercase variant for universal comparison
  kUnit3 = 'pmol/l/mg/dl';
  kUnit4 = 'miu/l/mg/dl';   // lowercase variant for universal comparison
var
  factorIn, factorOut, factor: extended;
begin
  case LowerCase(UoM1) of
    kUnit1: factorIn := 1;
    kUnit2: factorIn := 1 * kInsulinConversionFactor;
    kUnit3: factorIn := 1 * kGlucoseConversionFactor;
    kUnit4: factorIn := 1 * kInsulinConversionFactor * kGlucoseConversionFactor;
  end;
  case LowerCase(UoM2) of
    kUnit1: factorOut := 1;
    kUnit2: factorOut := 1 / kInsulinConversionFactor;
    kUnit3: factorOut := 1 / kGlucoseConversionFactor;
    kUnit4: factorOut := 1 / kInsulinConversionFactor / kGlucoseConversionFactor;
  end;
  factor := factorIn * factorOut;
  Result := Value * factor;
end;

function MarkedA(theParameter: extended; theRanges: tReferenceLimits;
  UoM1, Uom2: string; Precision: integer; Digits: integer): string;
  {Converts units and delivers formatted result for AIGR}
begin
  Result := Marked(ConvertedAIGR(theParameter, UoM1, UoM2), theRanges,
    Precision, Digits);
end;

function FormattedResults(const LabRecord: tLabRecord): string;
  {Delivers formatted result}
begin
  Result := kSPINA_GBeta + kTAB + kTAB + kTAB +
    FloatToStrF(LabRecord.SPINA_GBeta, ffFixed, 4, 2) + kS4TAB +
    kParOpen + RefMessage(gPreferences.ReferenceValues.SPINA_GBeta.ln,
    gPreferences.ReferenceValues.SPINA_GBeta.hn,
    gPreferences.ReferenceValues.SPINA_GBeta.UoM, 4, 2) + kParClose +
    LineEnding + kSPINA_GR + kS4TAB + kTAB + kTAB +
    FloatToStrF(LabRecord.SPINA_GR, ffFixed, 4, 2) + kS4TAB + kParOpen +
    RefMessage(gPreferences.ReferenceValues.SPINA_GR.ln,
    gPreferences.ReferenceValues.SPINA_GR.hn,
    gPreferences.ReferenceValues.SPINA_GR.UoM, 4, 2) + kParClose +
    LineEnding + kSPINA_DI + kS4TAB + kTAB + kTAB +
    FloatToStrF(LabRecord.SPINA_DI, ffFixed, 4, 2) + kS4TAB + kParOpen +
    RefMessage(gPreferences.ReferenceValues.SPINA_DI.ln,
    gPreferences.ReferenceValues.SPINA_DI.hn,
    gPreferences.ReferenceValues.SPINA_DI.UoM, 4, 2) + kParClose +
    LineEnding + kHOMA_Beta + kTAB + kTAB + kTAB +
    FloatToStrF(LabRecord.HOMA_Beta, ffFixed, 4, 1) + kS4TAB + kParOpen +
    RefMessage(gPreferences.ReferenceValues.HOMA_Beta.ln,
    gPreferences.ReferenceValues.HOMA_Beta.hn,
    gPreferences.ReferenceValues.HOMA_Beta.UoM, 4, 1) + kParClose +
    LineEnding + kHOMA_IR + kS4TAB + kTAB + kTAB +
    FloatToStrF(LabRecord.HOMA_IR, ffFixed, 4, 1) + ' ' + kS4TAB +
    kParOpen + RefMessage(gPreferences.ReferenceValues.HOMA_IR.ln,
    gPreferences.ReferenceValues.HOMA_IR.hn, gPreferences.ReferenceValues.HOMA_IR.UoM,
    4, 1) + kParClose + LineEnding + kHOMA_IS + kS4TAB + kTAB + kTAB +
    FloatToStrF(LabRecord.HOMA_IS, ffFixed, 4, 1) + ' ' + kS4TAB +
    kParOpen + RefMessage(gPreferences.ReferenceValues.HOMA_IS.ln,
    gPreferences.ReferenceValues.HOMA_IS.hn, gPreferences.ReferenceValues.HOMA_IS.UoM,
    4, 1) + kParClose + LineEnding + kQUICKI + kS10Tab + kTAB + kTAB +
    FloatToStrF(LabRecord.QUICKI, ffFixed, 4, 1) + ' ' + kS4TAB +
    kParOpen + RefMessage(gPreferences.ReferenceValues.QUICKI.ln,
    gPreferences.ReferenceValues.QUICKI.hn, gPreferences.ReferenceValues.QUICKI.UoM,
    4, 1) + kParClose + LineEnding + kAIGR + kS12Tab + kTAB + kTAB +
    FloatToStrF(ConvertedAIGR(LabRecord.AIGR, kEngineUoMs.AIGR,
    gPreferences.ReferenceValues.AIGR.UoM), ffFixed, 4, 1) + ' ' + kS4TAB +
    kParOpen + RefMessage(gPreferences.ReferenceValues.AIGR.ln,
    gPreferences.ReferenceValues.AIGR.hn, gPreferences.ReferenceValues.AIGR.UoM,
    4, 1) + kParClose + LineEnding + kCGR + ' ' + kS10Tab + kTAB +
    kTAB + FloatToStrF(LabRecord.CGR, ffFixed, 4, 1) + ' ' + kS4TAB +
    kParOpen + RefMessage(gPreferences.ReferenceValues.CGR.ln,
    gPreferences.ReferenceValues.CGR.hn, gPreferences.ReferenceValues.CGR.UoM, 4, 1) +
    kParClose;
end;

end.
