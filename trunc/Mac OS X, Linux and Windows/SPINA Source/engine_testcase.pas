unit engine_testcase;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.4.0 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ Test case unit for PPCUnit, according to SPINA Technical Reference, Part R }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, math, testutils, testregistry, SPINA_Types, MeasurementParser, SPINA_Engine;

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
  end;

  TEngineTestCases = class(TTestCase)
  published
    procedure TestCase1;
    procedure TestCase2;
    procedure TestCase3;
    procedure TestCase12;
    procedure TestCase13;
    procedure TestCase14;
  end;

var
  testCaseRecord: tCaseRecord;
  TSH, T4, T3: real;
  TSH_String, T4_String, T3_String: String;

implementation

{ -- Base functionality test -- }

procedure TControlTestCases.PositiveCheck;
begin
  AssertNull('This test is bound to succeed', nil);
end;

{ -- Unit parser tests -- }

procedure TUnitParserTestCases.TestCase1;
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
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('');
  AssertEquals(true, isNaN(theMeasurement.Value));
  AssertEquals('', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase2;
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('0');
  AssertEquals(0, theMeasurement.Value);
  AssertEquals('', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase3;
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1 mU/l');
  AssertEquals(1, theMeasurement.Value);
  AssertEquals('mU/l', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase4;
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1,3 ng/dl');
  AssertEquals(1.3, theMeasurement.Value);
  AssertEquals('ng/dl', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase5;
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('4 pg/ml');
  AssertEquals(4, theMeasurement.Value);
  AssertEquals('pg/ml', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase6;
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1.6 ng/dl');
  AssertEquals(1.6, theMeasurement.Value);
  AssertEquals('ng/dl', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase7;
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
  AssertEquals(true, (theResult > 23.0e-12) and (theResult < 23.2e-12));
end;

{ -- Calculation engine tests -- }

procedure TEngineTestCases.TestCase1;
{test case #1}
{Empty values}
{TSH NaN, FT4 NaN, FT3 0 NaN}
{=> GT = NaN, GD = NaN}
begin
  gPreferences.T4.Method := freeHormone;
  gPreferences.T3.Method := freeHormone;
  TSH := NaN;
  T4 := NaN;
  T3 := NaN;
  testCaseRecord := Calculate(TSH, T4, T3);
  AssertEquals(true, isNaN(testCaseRecord.GT));
  AssertEquals(true, isNaN(testCaseRecord.GD));
  AssertEquals(gNotCalculable, (testCaseRecord.GTs));
  AssertEquals(gNotCalculable, (testCaseRecord.GDs));
end;

procedure TEngineTestCases.TestCase2;
{test case #2}
{Zero values}
{TSH 0 mU/l, FT4 0 pmol/l, FT3 0 pmol/l}
{=> GT = NaN, GD = NaN}
begin
  gPreferences.T4.Method := freeHormone;
  gPreferences.T3.Method := freeHormone;
  TSH := 0;
  T4 := 0;
  T3 := 0;
  testCaseRecord := Calculate(TSH, T4, T3);
  AssertEquals(true, isNaN(testCaseRecord.GT));
  AssertEquals(true, isNaN(testCaseRecord.GD));
  AssertEquals(gNotCalculable, (testCaseRecord.GTs));
  AssertEquals(gNotCalculable, (testCaseRecord.GDs));
end;

procedure TEngineTestCases.TestCase3;
{test case #3}
{TSH 1 mU/l, FT4 2 ng/dl, FT3 3 pg/ml}
{=> GT = 7.29 pmol/s, GD = 16.69 nmol/s}
var
  tempGT, tempGD: real;
begin
  gPreferences.T4.Method := freeHormone;
  gPreferences.T3.Method := freeHormone;
  TSH_String := '1 mU/l';
  T4_String := '2 ng/dl';
  T3_String := '3 pg/ml';
  TSH := ValueFromUnit(TSH_String, 1, 'mU/l');
  T4 := ValueFromUnit(T4_String, T4_MOLAR_MASS, 'mol/l');
  T3 := ValueFromUnit(T3_String, T3_MOLAR_MASS, 'mol/l');
  testCaseRecord := Calculate(TSH, T4, T3);
  tempGT := testCaseRecord.GT;
  tempGD := testCaseRecord.GD;
  AssertEquals(true, (testCaseRecord.GT > 0.99 * 7.29e-12) and (testCaseRecord.GT < 1.01 * 7.29e-12));
  AssertEquals(true, (testCaseRecord.GD > 0.99 * 16.69e-9) and (testCaseRecord.GD < 1.01 * 16.69e-9));
  AssertEquals('7.', LeftStr(testCaseRecord.GTs, 2));
  AssertEquals('pmol/s', RightStr(testCaseRecord.GTs, 6));
  AssertEquals('16.', LeftStr(testCaseRecord.GDs, 3));
  AssertEquals('nmol/s', RightStr(testCaseRecord.GDs, 6));
end;

procedure TEngineTestCases.TestCase12;
  {test case #12}
  {Normal values}
  {TSH 1 mU/l, FT4 16.5 pmol/l, FT3 4.5 pmol/l}
  {=> GT = 4.70 pmol/s, GD = 25.22 nmol/l}
begin
  gPreferences.T4.Method := freeHormone;
  gPreferences.T3.Method := freeHormone;
  TSH := 1;
  T4 := 16.5e-12;
  T3 := 4.5e-12;
  testCaseRecord := Calculate(TSH, T4, T3);
  AssertEquals(true, (testCaseRecord.GT > 0.99 * 4.70e-12) and (testCaseRecord.GT < 1.01 * 4.70e-12));
  AssertEquals(true, (testCaseRecord.GD > 0.99 * 25.22e-9) and (testCaseRecord.GD < 1.01 * 25.22e-9));
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
begin
  gPreferences.T4.Method := freeHormone;
  gPreferences.T3.Method := freeHormone;
  TSH := 3.24;
  T4 := 7.7e-12;
  T3 := 28e-12;
  testCaseRecord := Calculate(TSH, T4, T3);
  AssertEquals(true, (testCaseRecord.GT > 0.99 * 1.08e-12) and (testCaseRecord.GT < 1.01 * 1.08e-12));
  AssertEquals(true, (testCaseRecord.GD > 0.99 * 336.2e-9) and (testCaseRecord.GD < 1.01 * 336.2e-9));
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
begin
  gPreferences.T4.Method := freeHormone;
  gPreferences.T3.Method := freeHormone;
  TSH := 0.7;
  T4 := 9e-12;
  T3 := 6.2e-12;
  testCaseRecord := Calculate(TSH, T4, T3);
  AssertEquals(true, (testCaseRecord.GT > 0.99 * 3.37e-12) and (testCaseRecord.GT < 1.01 * 3.37e-12));
  AssertEquals(true, (testCaseRecord.GD > 0.99 * 63.7e-9) and (testCaseRecord.GD < 1.01 * 63.7e-9));
  AssertEquals('3.3',LeftStr(testCaseRecord.GTs, 3));
  AssertEquals('pmol/s', RightStr(testCaseRecord.GTs, 6));
  AssertEquals('63.7', LeftStr(testCaseRecord.GDs, 4));
  AssertEquals('nmol/s', RightStr(testCaseRecord.GDs, 6));
end;

initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TUnitParserTestCases);
  RegisterTest(TMeasurementParserTestCases);
  RegisterTest(TconverterTestCases);
  RegisterTest(TEngineTestCases);
end.

