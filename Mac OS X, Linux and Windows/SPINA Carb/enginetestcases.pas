unit EngineTestCases;

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

{ This unit contains test cases }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  SPINA_Engine;

type

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

  { TEngineTestCases }

  TEngineTestCases = class(TTestCase)
  published
    procedure TestCaseV1;
    procedure TestCaseV21;
    procedure TestCaseV30;
    procedure TestCaseV39;
    procedure TestCaseV114;
  end;

var
  insulin, glucose: real;

implementation

function WithinTolerance(a, b, t: real): boolean;
begin
  result := false;
  if abs(b - a) < t * a then
    result := true;
end;

{ TEngineTestCases }

procedure TEngineTestCases.TestCaseV1;
begin
  glucose := 92 / kGlucoseConversionFactor;
  insulin := 1.2 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(0.29, SPINA_GBeta(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(15.83, SPINA_GR(insulin, glucose), 0.1));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(14.9, HOMA_Beta(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(0.3, HOMA_IR(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(3.67, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.5, QUICKI(insulin, glucose), 0.2));
end;

procedure TEngineTestCases.TestCaseV21;
begin
  glucose := 85 / kGlucoseConversionFactor;
  insulin := 0.5 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(0.13, SPINA_GBeta(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(41.91, SPINA_GR(insulin, glucose), 0.1));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(8.2, HOMA_Beta(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(0.1, HOMA_IR(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(9.54, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.6, QUICKI(insulin, glucose), 0.2));
end;

procedure TEngineTestCases.TestCaseV30;
begin
  glucose := 89 / kGlucoseConversionFactor;
  insulin := 10.7 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(2.64, SPINA_GBeta(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(1.92, SPINA_GR(insulin, glucose), 0.1));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(148.7, HOMA_Beta(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(2.3, HOMA_IR(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(0.43, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.3, QUICKI(insulin, glucose), 0.2));
end;

procedure TEngineTestCases.TestCaseV39;
begin
  glucose := 108 / kGlucoseConversionFactor;
  insulin := 29.6 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(6.54, SPINA_GBeta(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(0.58, SPINA_GR(insulin, glucose), 0.1));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(237.4, HOMA_Beta(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(7.9, HOMA_IR(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(0.13, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.3, QUICKI(insulin, glucose), 0.2));
end;

procedure TEngineTestCases.TestCaseV114;
begin
  glucose := 123 / kGlucoseConversionFactor;
  insulin := 122.8 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(25.36, SPINA_GBeta(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(0.15, SPINA_GR(insulin, glucose), 0.1));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(738.3, HOMA_Beta(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(37.3, HOMA_IR(insulin, glucose), 0.1));
  AssertTrue(WithinTolerance(0.03, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.2, QUICKI(insulin, glucose), 0.2));
end;

procedure TControlTestCases.PositiveCheck;
begin
  AssertNull('This test is bound to succeed', nil);
end;


initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TEngineTestCases);
end.

