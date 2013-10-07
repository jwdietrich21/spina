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
  Classes, SysUtils, fpcunit, math, testutils, testregistry, SPINA_Types, SPINA_Engine;

type

  { TEngineTestCases }

  TEngineTestCases= class(TTestCase)
  published
    procedure PositiveCheck;
    procedure TestCase2;
    procedure TestCase12;
    procedure TestCase13;
    procedure TestCase14;
  end;

var
  testCaseRecord: tCaseRecord;
  TSH, T4, T3: real;

implementation

procedure TEngineTestCases.PositiveCheck;
begin
  AssertNull('This test is bound to succeed', nil);
end;

procedure TEngineTestCases.TestCase2;
{test case #2}
{Empty values}
{TSH 0 mU/l, FT4 0 pmol/l, FT3 0 pmol/l}
{=> GT = NaN, GD = NaN}
begin
  gPreferences.T4.Method := freeHormone;
  gPreferences.T3.Method := freeHormone;
  TSH := 0;
  T4 := 0;
  T3 := 0;
  testCaseRecord := Calculate(TSH, T4, T3);
  AssertEquals((testCaseRecord.GT), NaN);
  AssertEquals((testCaseRecord.GD), NaN);
  AssertEquals(gNotCalculable, (testCaseRecord.GTs));
  AssertEquals(gNotCalculable, (testCaseRecord.GDs));
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
  AssertEquals(true, (testCaseRecord.GT > 4.69e-12) and (testCaseRecord.GT < 4.71e-12));
  AssertEquals(true, (testCaseRecord.GD > 25.2e-9) and (testCaseRecord.GD < 25.3e-9));
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
  AssertEquals(true, (testCaseRecord.GT > 1.07e-12) and (testCaseRecord.GT < 1.09e-12));
  AssertEquals(true, (testCaseRecord.GD > 336.0e-9) and (testCaseRecord.GD < 336.4e-9));
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
  AssertEquals(true, (testCaseRecord.GT > 3.36e-12) and (testCaseRecord.GT < 3.38e-12));
  AssertEquals(true, (testCaseRecord.GD > 63.4e-9) and (testCaseRecord.GD < 64e-9));
  AssertEquals('3.3',LeftStr(testCaseRecord.GTs, 3));
  AssertEquals('pmol/s', RightStr(testCaseRecord.GTs, 6));
  AssertEquals('63.7', LeftStr(testCaseRecord.GDs, 4));
  AssertEquals('nmol/s', RightStr(testCaseRecord.GDs, 6));
end;

initialization

  RegisterTest(TEngineTestCases);
end.

