unit CaseBroker;

{ SPINA Carb }

{ Application for calculating structure parameters }
{ of insulin-glucose feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des Insulin-Glukose-Regelkreises }

{ Version 5.1.0 (Cyclone) }

{ (c) J. W. Dietrich, 1994 - 2024 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2024 }

{ This unit filters and prepares inputs for the calculation engine }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UnitConverter, SPINA_Engine;

type
  tLabRecord = record
    Insulin, Glucose: extended;
    SPINA_GBeta, SPINA_GR, SPINA_DI, HOMA_Beta, HOMA_IR, HOMA_IS,
      QUICKI: extended;
  end;
  tCaseRecord = record
    LabRecord:tLabRecord;
  end;

function InsulinSI(RawIns: extended; InsUom: String): extended;
function GlucoseSI(RawGlc: extended; GlcUom: String): extended;
procedure Calculate(LabRecord: tLabRecord);

implementation

function InsulinSI(RawIns: extended; InsUom: String): extended;
begin
  result := ConvertedValue(RawIns, kInsulinConversionFactor, InsUom, 'pmol/l');
end;

function GlucoseSI(RawGlc: extended; GlcUom: String): extended;
begin
  result := ConvertedValue(RawGlc, kGlucoseMolarMass, GlcUom, 'mmol/l');
end;

procedure Calculate(LabRecord: tLabRecord);
begin
  LabRecord.SPINA_GBeta := SPINA_GBeta(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.SPINA_GR := SPINA_GR(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.SPINA_DI := SPINA_DI(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.HOMA_Beta := HOMA_Beta(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.HOMA_IR := HOMA_IR(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.HOMA_IS := HOMA_IS(LabRecord.Insulin, LabRecord.Glucose);
  LabRecord.QUICKI := QUICKI(LabRecord.Insulin, LabRecord.Glucose);
end;

end.

