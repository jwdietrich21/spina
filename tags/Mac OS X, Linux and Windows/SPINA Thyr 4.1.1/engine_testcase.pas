unit engine_testcase;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 4.0.2 (Mercator) }

{ (c) J. W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ Test case unit for PPCUnit, according to SPINA Technical Reference, Part R }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, math, testutils, testregistry, SPINA_Types, UnitConverter, SPINA_Engine;

type

  { TEngineTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

  TUnitParserTestCases = class(TTestCase)
  published
    procedure TestCase1;
    procedure TestCase2;
    procedure TestCase3;
    procedure TestCase4;
    procedure TestCase5;
    procedure TestCase11;
  end;

  TMeasurementParserTestCases = class(TTestCase)
  published
    procedure TestCase1;
    procedure TestCase2;
    procedure TestCase3;
    procedure TestCase4;
    procedure TestCase5;
    procedure TestCase6;
    procedure TestCase7;
  end;

  TconverterTestCases = class(TTestCase)
  published
    procedure TestCase1;
    procedure TestCase2;
    procedure TestCase3;
    procedure TestCase4;
    procedure TestCase5;
    procedure TestCase6;
    procedure TestCase7;
    procedure TestCase8;
    procedure TestCase9;
    procedure TestCase10;
    procedure TestCase11;
    procedure TestCase12;
    procedure TestCase13;
    procedure TestCase14;
    procedure TestCase15;
    procedure TestCase16;
  end;

  TEngineTestCases = class(TTestCase)
  published
    procedure TestCase1;
    procedure TestCase2;
    procedure TestCase3;
    procedure TestCase4;
    procedure TestCase5;
    procedure TestCase6;
    procedure TestCase7;
    procedure TestCase8;
    procedure TestCase9;
    procedure TestCase10;
    procedure TestCase11;
    procedure TestCase12;
    procedure TestCase13;
    procedure TestCase14;
    procedure TestCase15;
    procedure TestCase16;
    procedure TestCase17;
  end;

var
  testCaseRecord: tCaseRecord;
  TSH_String, T4_String, T3_String: String;

implementation

{ -- Base functionality test -- }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

{ -- Unit parser tests -- }

procedure TUnitParserTestCases.TestCase1;
{ NA string }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('NA');
  AssertEquals('NA', theUnitElements.MassPrefix);
  AssertEquals('NA', theUnitElements.MassUnit);
  AssertEquals('NA', theUnitElements.VolumePrefix);
  AssertEquals('NA', theUnitElements.VolumeUnit);
end;

procedure TUnitParserTestCases.TestCase2;
{ empty string }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('');
  AssertEquals('', theUnitElements.MassPrefix);
  AssertEquals('', theUnitElements.MassUnit);
  AssertEquals('', theUnitElements.VolumePrefix);
  AssertEquals('', theUnitElements.VolumeUnit);
end;

procedure TUnitParserTestCases.TestCase3;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('mU/l');
  AssertEquals('m', theUnitElements.MassPrefix);
  AssertEquals('U', theUnitElements.MassUnit);
  AssertEquals('', theUnitElements.VolumePrefix);
  AssertEquals('l', theUnitElements.VolumeUnit);
end;

procedure TUnitParserTestCases.TestCase4;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('ng/dl');
  AssertEquals('n', theUnitElements.MassPrefix);
  AssertEquals('g', theUnitElements.MassUnit);
  AssertEquals('d', theUnitElements.VolumePrefix);
  AssertEquals('l', theUnitElements.VolumeUnit);
end;

procedure TUnitParserTestCases.TestCase5;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('pg/ml');
  AssertEquals('p', theUnitElements.MassPrefix);
  AssertEquals('g', theUnitElements.MassUnit);
  AssertEquals('m', theUnitElements.VolumePrefix);
  AssertEquals('l', theUnitElements.VolumeUnit);
end;

procedure TUnitParserTestCases.TestCase11;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('pmol/l');
  AssertEquals('p', theUnitElements.MassPrefix);
  AssertEquals('mol', theUnitElements.MassUnit);
  AssertEquals('', theUnitElements.VolumePrefix);
  AssertEquals('l', theUnitElements.VolumeUnit);
end;

{ -- Measurement parser tests -- }

procedure TMeasurementParserTestCases.TestCase1;
{ Empty string }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('');
  AssertTrue(isNaN(theMeasurement.Value));
  AssertEquals('', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase2;
{ Zero value }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('0');
  AssertEquals(0, theMeasurement.Value);
  AssertEquals('', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase3;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1 mU/l');
  AssertEquals(1, theMeasurement.Value);
  AssertEquals('mU/l', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase4;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1,3 ng/dl');
  AssertEquals(1.3, theMeasurement.Value);
  AssertEquals('ng/dl', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase5;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('4 pg/ml');
  AssertEquals(4, theMeasurement.Value);
  AssertEquals('pg/ml', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase6;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1.6 ng/dl');
  AssertEquals(1.6, theMeasurement.Value);
  AssertEquals('ng/dl', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase7;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('0.01 mU/l');
  AssertEquals(0.01, theMeasurement.Value);
  AssertEquals('mU/l', theMeasurement.uom);
end;

{ -- Unit converter tests -- }

procedure TconverterTestCases.TestCase1;
{empty value}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('', 1, 'ng/dl');
  AssertEquals('', theResultString);
end;

procedure TconverterTestCases.TestCase2;
{T4: pmol/l to ng/dl}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('20 pmol/l', T4_MOLAR_MASS, 'ng/dl');
  AssertEquals('1.55', LeftStr(theResultString, 4));
  AssertEquals('ng/dl', RightStr(theResultString, 5));
end;

procedure TconverterTestCases.TestCase3;
{T4: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('20 pmol/l', T4_MOLAR_MASS, 'ng/l');
  AssertEquals('15.5', LeftStr(theResultString, 4));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

procedure TconverterTestCases.TestCase4;
{T4: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('18 ng/l', T4_MOLAR_MASS, 'pmol/l');
  AssertEquals('23.1', LeftStr(theResultString, 4));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase5;
{T4: ng/dl to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('1.8 ng/dl', T4_MOLAR_MASS, 'pmol/l');
  AssertEquals('23.1', LeftStr(theResultString, 4));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase6;
{T4: identical units}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('18 ng/l', T4_MOLAR_MASS, 'ng/l');
  AssertEquals('18', LeftStr(theResultString, 2));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

procedure TconverterTestCases.TestCase7;
{T3: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('3.2 ng/l', T3_MOLAR_MASS, 'pmol/l');
  AssertEquals('4.9', LeftStr(theResultString, 3));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase8;
{T3: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('5 pmol/l', T3_MOLAR_MASS, 'ng/l');
  AssertEquals('3.2', LeftStr(theResultString, 3));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

procedure TconverterTestCases.TestCase9;
{T3: pmol/l to pg/ml}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('5 pmol/l', T3_MOLAR_MASS, 'pg/ml');
  AssertEquals('3.2', LeftStr(theResultString, 3));
  AssertEquals('pg/ml', RightStr(theResultString, 5));
end;

procedure TconverterTestCases.TestCase10;
{T4: ng/l to mol/l}
var
  theResult: real;
begin
  theResult := ValueFromUnit('18 ng/l', T4_MOLAR_MASS, 'mol/l');
  AssertTrue((theResult > 23.0e-12) and (theResult < 23.2e-12));
end;

procedure TconverterTestCases.TestCase11;
{T4: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := UnitFromValue(20, T4_MOLAR_MASS, 'pmol/l', 'ng/l');
  AssertEquals('15.5', LeftStr(theResultString, 4));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

procedure TconverterTestCases.TestCase12;
{T4: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := UnitFromValue(18, T4_MOLAR_MASS, 'ng/l', 'pmol/l');
  AssertEquals('23.1', LeftStr(theResultString, 4));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase13;
{T4: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := UnitFromValueF(20, T4_MOLAR_MASS, 'pmol/l', 'ng/l', ffNumber, 2, 2);
  AssertEquals('15.5', LeftStr(theResultString, 4));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

procedure TconverterTestCases.TestCase14;
{T4: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := UnitFromValue(18, T4_MOLAR_MASS, 'ng/l', 'pmol/l');
  theResultString := UnitFromValueF(18, T4_MOLAR_MASS, 'ng/l', 'pmol/l', ffNumber, 2, 2);
  AssertEquals('23.1', LeftStr(theResultString, 4));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase15;
{T3: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnitF('3.2 ng/l', T3_MOLAR_MASS, 'pmol/l', ffNumber, 2, 2);
  AssertEquals('4.9', LeftStr(theResultString, 3));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase16;
{T3: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnitF('5 pmol/l', T3_MOLAR_MASS, 'ng/l', ffNumber, 2, 2);
  AssertEquals('3.2', LeftStr(theResultString, 3));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

{ -- Calculation engine tests -- }

procedure TEngineTestCases.TestCase1;
{test case #1}
{Empty values}
{TSH NaN, FT4 NaN, FT3 0 NaN}
{=> GT = NaN, GD = NaN}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  testCaseRecord.TSH := NaN;
  testCaseRecord.FT4 := NaN;
  testCaseRecord.FT3 := NaN;
  lReferenceRanges.FT4.ln := 7;
  lReferenceRanges.FT4.hn := 20;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue(isNaN(testCaseRecord.GT));
  AssertTrue(isNaN(testCaseRecord.GD));
  AssertEquals(gNotCalculable, testCaseRecord.GTs);
  AssertEquals(gNotCalculable, testCaseRecord.GDs);
  AssertEquals(gNotCalculable, testCaseRecord.TSHIs);
  AssertEquals(gNotCalculable, testCaseRecord.TTSIs);
end;

procedure TEngineTestCases.TestCase2;
{test case #2}
{Zero values}
{TSH 0 mU/l, FT4 0 pmol/l, FT3 0 pmol/l}
{=> GT = NaN, GD = NaN}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  testCaseRecord.TSH := 0;
  testCaseRecord.FT4 := 0;
  testCaseRecord.FT3 := 0;
  lReferenceRanges.FT4.ln := 7;
  lReferenceRanges.FT4.hn := 20;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  Calculate(testCaseRecord);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  FormatCase(testCaseRecord, gReferenceRanges);
  AssertTrue(isNaN(testCaseRecord.GT));
  AssertTrue(isNaN(testCaseRecord.GD));
  AssertEquals(gNotCalculable, testCaseRecord.GTs);
  AssertEquals(gNotCalculable, testCaseRecord.GDs);
  AssertEquals(gNotCalculable, testCaseRecord.TSHIs);
  AssertEquals(0, testCaseRecord.TTSI);
end;

procedure TEngineTestCases.TestCase3;
{test case #3}
{TSH 1 mU/l, FT4 2 ng/dl, FT3 3 pg/ml}
{=> GT = 7.29 pmol/s, GD = 16.69 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '1 mU/l';
  T4_String := '2 ng/dl';
  T3_String := '3 pg/ml';
  lReferenceRanges.FT4.ln := 0.7;
  lReferenceRanges.FT4.hn := 2;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.FT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 7.29e-12) and (testCaseRecord.GT < 1.01 * 7.29e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 16.69e-9) and (testCaseRecord.GD < 1.01 * 16.69e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 3.45) and (testCaseRecord.TSHI < 1.01 * 3.45));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 100) and (testCaseRecord.TTSI < 1.01 * 100));
  AssertEquals('7.', LeftStr(testCaseRecord.GTs, 2));
  AssertEquals('pmol/s', RightStr(testCaseRecord.GTs, 6));
  AssertEquals('16.', LeftStr(testCaseRecord.GDs, 3));
  AssertEquals('nmol/s', RightStr(testCaseRecord.GDs, 6));
end;

procedure TEngineTestCases.TestCase4;
{test case #4}
{Euthyroidism with normal values}
{TSH 1 mU/l, FT4 1.3 ng/dl, FT3 3 pg/ml}
{=> GT = 4.74 pmol/s, GD = 25.67 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '1 mU/l';
  T4_String := '1.3 ng/dl';
  T3_String := '3 pg/ml';
  lReferenceRanges.FT4.ln := 0.7;
  lReferenceRanges.FT4.hn := 2;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.FT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 4.74e-12) and (testCaseRecord.GT < 1.01 * 4.74e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 25.67e-9) and (testCaseRecord.GD < 1.01 * 25.67e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 2.24) and (testCaseRecord.TSHI < 1.01 * 2.24));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 65) and (testCaseRecord.TTSI < 1.01 * 65));
end;

procedure TEngineTestCases.TestCase5;
{test case #5}
{Sublatent hyperthyroidism}
{TSH 0.5 mU/l, FT4 1.5 ng/dl, FT3 4 pg/ml}
{=> GT = 9.47 pmol/s, GD = 29.67 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '0.5 mU/l';
  T4_String := '1.5 ng/dl';
  T3_String := '4 pg/ml';
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  lReferenceRanges.FT4.ln := 0.7;
  lReferenceRanges.FT4.hn := 2;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.FT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 9.47e-12) and (testCaseRecord.GT < 1.01 * 9.47e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 29.67e-9) and (testCaseRecord.GD < 1.01 * 29.67e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 1.89) and (testCaseRecord.TSHI < 1.01 * 1.89));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 37.5) and (testCaseRecord.TTSI < 1.01 * 37.5));
end;

procedure TEngineTestCases.TestCase6;
{test case #6}
{Latent or subclinical hyperthyroidism}
{TSH 0.3 mU/l, FT4 1.6 ng/dl, FT3 5 pg/ml}
{=> GT = 15.81 pmol/s, GD = 34.74 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '0.3 mU/l';
  T4_String := '1.6 ng/dl';
  T3_String := '5 pg/ml';
  lReferenceRanges.FT4.ln := 0.7;
  lReferenceRanges.FT4.hn := 2;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.FT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 15.81e-12) and (testCaseRecord.GT < 1.01 * 15.81e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 34.74e-9) and (testCaseRecord.GD < 1.01 * 34.74e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 1.56) and (testCaseRecord.TSHI < 1.01 * 1.56));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 24) and (testCaseRecord.TTSI < 1.01 * 24));
end;

procedure TEngineTestCases.TestCase7;
{test case #7}
{Overt hyperthyroidism}
{TSH 0.01 mU/l, FT4 2.2 ng/dl, FT3 6 pg/ml}
{=> GT = 589.99 pmol/s, GD = 30.34 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '0.01 mU/l';
  T4_String := '2.2 ng/dl';
  T3_String := '6 pg/ml';
  lReferenceRanges.FT4.ln := 0.7;
  lReferenceRanges.FT4.hn := 2;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.FT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 589.99e-12) and (testCaseRecord.GT < 1.01 * 589.99e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 30.34e-9) and (testCaseRecord.GD < 1.01 * 30.34e-9));
  AssertTrue((testCaseRecord.TSHI > 1.02 * -0.81) and (testCaseRecord.TSHI < 0.98 * -0.81));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 1.1) and (testCaseRecord.TTSI < 1.01 * 1.1));
end;

procedure TEngineTestCases.TestCase8;
{test case #8}
{Sublatent hypothyroidism}
{TSH 3.9 mU/l, FT4 0.8 ng/dl, FT3 2 pg/ml}
{=> GT = 1.33 pmol/s, GD = 27.81 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '3.9 mU/l';
  T4_String := '0.8 ng/dl';
  T3_String := '2 pg/ml';
  lReferenceRanges.FT4.ln := 0.7;
  lReferenceRanges.FT4.hn := 2;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.FT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 1.33e-12) and (testCaseRecord.GT < 1.01 * 1.33e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 27.81e-9) and (testCaseRecord.GD < 1.01 * 27.81e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 2.74) and (testCaseRecord.TSHI < 1.01 * 2.74));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 156) and (testCaseRecord.TTSI < 1.01 * 156));
end;

procedure TEngineTestCases.TestCase9;
{test case #9}
{Latent or subclinical hypothyroidism}
{TSH 7 mU/l, FT4 0.8 ng/dl, FT3 2 pg/ml}
{=> GT = 1.08 pmol/s, GD = 27.81 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '7 mU/l';
  T4_String := '0.8 ng/dl';
  T3_String := '2 pg/ml';
  lReferenceRanges.FT4.ln := 0.7;
  lReferenceRanges.FT4.hn := 2;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.FT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 1.08e-12) and (testCaseRecord.GT < 1.01 * 1.084e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 27.81e-9) and (testCaseRecord.GD < 1.01 * 27.81e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 3.33) and (testCaseRecord.TSHI < 1.01 * 3.33));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 280) and (testCaseRecord.TTSI < 1.01 * 280));
end;

procedure TEngineTestCases.TestCase10;
{test case #10}
{Overt hypothyroidism}
{TSH 13 mU/l, FT4 0.4 ng/dl, FT3 1 pg/ml}
{=> GT = 0.47 pmol/s, GD = 27.81 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '13 mU/l';
  T4_String := '0.4 ng/dl';
  T3_String := '1 pg/ml';
  lReferenceRanges.FT4.ln := 0.7;
  lReferenceRanges.FT4.hn := 2;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.FT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 0.47e-12) and (testCaseRecord.GT < 1.01 * 0.47e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 27.81e-9) and (testCaseRecord.GD < 1.01 * 27.81e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 3.25) and (testCaseRecord.TSHI < 1.01 * 3.25));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 260) and (testCaseRecord.TTSI < 1.01 * 260));
end;

procedure TEngineTestCases.TestCase11;
{test case #11}
{Normal values, units ng/l and pmol/l}
{TSH 1 mU/l, FT4 13 ng/l, FT3 4.5 pmol/l}
{=> GT = 4.74 pmol/s, GD = 25.01 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '1 mU/l';
  T4_String := '13 ng/l';
  T3_String := '4.5 pmol/l';
  lReferenceRanges.FT4.ln := 7;
  lReferenceRanges.FT4.hn := 20;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.FT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, lReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 4.74e-12) and (testCaseRecord.GT < 1.01 * 4.74e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 25.01e-9) and (testCaseRecord.GD < 1.01 * 25.01e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 2.24) and (testCaseRecord.TSHI < 1.01 * 2.24));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 65) and (testCaseRecord.TTSI < 1.01 * 65));
end;

procedure TEngineTestCases.TestCase12;
  {test case #12}
  {Normal values}
  {TSH 1 mU/l, FT4 16.5 pmol/l, FT3 4.5 pmol/l}
  {=> GT = 4.70 pmol/s, GD = 25.22 nmol/l}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  testCaseRecord.TSH := 1;
  testCaseRecord.FT4 := 16.5e-12;
  testCaseRecord.FT3 := 4.5e-12;
  lReferenceRanges.FT4.ln := 7;
  lReferenceRanges.FT4.hn := 20;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 4.70e-12) and (testCaseRecord.GT < 1.01 * 4.70e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 25.22e-9) and (testCaseRecord.GD < 1.01 * 25.22e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 2.21) and (testCaseRecord.TSHI < 1.01 * 2.21));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 64.1) and (testCaseRecord.TTSI < 1.01 * 64.1));
  AssertEquals('4.7', LeftStr(testCaseRecord.GTs, 3));
  AssertEquals('pmol/s', RightStr(testCaseRecord.GTs, 6));
  AssertEquals('25.2', LeftStr(testCaseRecord.GDs, 4));
  AssertEquals('nmol/s', RightStr(testCaseRecord.GDs, 6));
end;

procedure TEngineTestCases.TestCase13;
  {test case #13}
  {T3 thyrotoxicosis}
  {TSH 3.24 mU/l, FT4 7.7 pmol/l, FT3 28 pmol/l}
  {=> GT = 1.08 pmol/s, GD = 336.2 nmol/l}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  testCaseRecord.TSH := 3.24;
  testCaseRecord.FT4 := 7.7e-12;
  testCaseRecord.FT3 := 28e-12;
  lReferenceRanges.FT4.ln := 7;
  lReferenceRanges.FT4.hn := 20;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 1.08e-12) and (testCaseRecord.GT < 1.01 * 1.08e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 336.2e-9) and (testCaseRecord.GD < 1.01 * 336.2e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 2.21) and (testCaseRecord.TSHI < 1.01 * 2.21));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 97.2) and (testCaseRecord.TTSI < 1.01 * 97.2));
  AssertEquals('1.0', LeftStr(testCaseRecord.GTs, 3));
  AssertEquals('pmol/s', RightStr(testCaseRecord.GTs, 6));
  AssertEquals('336.', LeftStr(testCaseRecord.GDs, 4));
  AssertEquals('nmol/s', RightStr(testCaseRecord.GDs, 6));
end;

procedure TEngineTestCases.TestCase14;
  {test case #14}
  {Latent hyperdeiodation (hyperdeiodination)}
  {TSH 0.7 mU/l, FT4 9 pmol/l, FT3 6.2 pmol/l}
  {=> GT = 3.37 pmol/s, GD = 63.7 nmol/l}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  testCaseRecord.TSH := 0.7;
  testCaseRecord.FT4 := 9e-12;
  testCaseRecord.FT3 := 6.2e-12;
  lReferenceRanges.FT4.ln := 7;
  lReferenceRanges.FT4.hn := 20;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 3.37e-12) and (testCaseRecord.GT < 1.01 * 3.37e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 63.7e-9) and (testCaseRecord.GD < 1.01 * 63.7e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 0.85) and (testCaseRecord.TSHI < 1.01 * 0.85));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 24.5) and (testCaseRecord.TTSI < 1.01 * 24.5));
  AssertEquals('3.3',LeftStr(testCaseRecord.GTs, 3));
  AssertEquals('pmol/s', RightStr(testCaseRecord.GTs, 6));
  AssertEquals('63.7', LeftStr(testCaseRecord.GDs, 4));
  AssertEquals('nmol/s', RightStr(testCaseRecord.GDs, 6));
end;

procedure TEngineTestCases.TestCase15;
{test case #15}
{Structure parameters calculated from total hormone values}
{TSH 0.86 mU/l, TT4 163 nmol/l, TT3 3 nmol/l}
{=> GT = 7.53 pmol/s, GD = 19.54 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '0.86 mU/l';
  T4_String := '163 nmol/l';
  T3_String := '3 nmol/l';
  lReferenceRanges.FT4.ln := 7;
  lReferenceRanges.FT4.hn := 20;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.TT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.TT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 7.53e-12) and (testCaseRecord.GT < 1.01 * 7.53e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 19.54e-9) and (testCaseRecord.GD < 1.01 * 19.54e-9));
  AssertTrue(isNan(testCaseRecord.TSHI));
  AssertTrue(isNan(testCaseRecord.TTSI));
  AssertEquals(gNotCalculable, testCaseRecord.TSHIs);
  AssertEquals(gNotCalculable, testCaseRecord.TTSIs);
end;

procedure TEngineTestCases.TestCase16;
{test case #16}
{Secondary hypothyroidism due to thyrotropic insufficiency}
{TSH 0.2 mU/l, FT4 4 ng/l, FT3 2 pmol/l}
{=> GT = 5.73 pmol/s, GD = 36.12 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '0.2 mU/l';
  T4_String := '4 ng/l';
  T3_String := '2 pmol/l';
  lReferenceRanges.FT4.ln := 7;
  lReferenceRanges.FT4.hn := 20;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.FT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 5.73e-12) and (testCaseRecord.GT < 1.01 * 5.73e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 36.12e-9) and (testCaseRecord.GD < 1.01 * 36.12e-9));
  AssertTrue((testCaseRecord.TSHI > 1.02 * -0.91) and (testCaseRecord.TSHI < 0.98 * -0.91));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 4) and (testCaseRecord.TTSI < 1.01 * 4));
end;

procedure TEngineTestCases.TestCase17;
{test case #17}
{Secondary hyperthyoidism in case of TSH-producing adenoma}
{TSH 16.13 mU/l, FT4 24 ng/l, TT3 1.9 pmol/l}
{=> GT = 2.73 pmol/s, GD = 14.65 nmol/s}
var
  lReferenceRanges, lSIReferenceRanges: tReferenceValues;
begin
  NewCaseRecord(testCaseRecord);
  TSH_String := '16.13 mU/l';
  T4_String := '24 ng/l';
  T3_String := '1.9 µg/l';
  lReferenceRanges.FT4.ln := 7;
  lReferenceRanges.FT4.hn := 20;
  lSIReferenceRanges.FT4.ln := ConvertedValue(lReferenceRanges.FT4.ln, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  lSIReferenceRanges.FT4.hn := ConvertedValue(lReferenceRanges.FT4.hn, T4_MOLAR_MASS, 'ng/l', 'mol/l');
  testCaseRecord.TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  testCaseRecord.FT4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  testCaseRecord.TT3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  Calculate(testCaseRecord);
  FormatCase(testCaseRecord, gReferenceRanges);
  InsertTTSI(testCaseRecord, lSIReferenceRanges.FT4.hn);
  AssertTrue((testCaseRecord.GT > 0.99 * 2.73e-12) and (testCaseRecord.GT < 1.01 * 2.73e-12));
  AssertTrue((testCaseRecord.GD > 0.99 * 14.65e-9) and (testCaseRecord.GD < 1.01 * 14.65e-9));
  AssertTrue((testCaseRecord.TSHI > 0.99 * 6.92) and (testCaseRecord.TSHI < 1.01 * 6.92));
  AssertTrue((testCaseRecord.TTSI > 0.99 * 1936) and (testCaseRecord.TTSI < 1.01 * 1936));
end;

initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TUnitParserTestCases);
  RegisterTest(TMeasurementParserTestCases);
  RegisterTest(TconverterTestCases);
  RegisterTest(TEngineTestCases);
end.

