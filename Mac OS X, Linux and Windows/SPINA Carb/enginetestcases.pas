unit EngineTestCases;

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
    procedure TestCaseB1;
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
  if abs(b - a) <= t * a then
    result := true;
end;

{ TEngineTestCases }

procedure TEngineTestCases.TestCaseB1;
begin
  glucose := 594 / kGlucoseConversionFactor;
  insulin := 111 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(13.73, SPINA_GBeta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(0, SPINA_GR(insulin, glucose), 0.01));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(75.3, HOMA_Beta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(162.6, HOMA_IR(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(0.006, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.2, QUICKI(insulin, glucose), 0.2));
end;

procedure TEngineTestCases.TestCaseV1;
begin
  glucose := 92 / kGlucoseConversionFactor;
  insulin := 1.2 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(0.290, SPINA_GBeta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(15.855, SPINA_GR(insulin, glucose), 0.01));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(14.897, HOMA_Beta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(0.273, HOMA_IR(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(3.668, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.489, QUICKI(insulin, glucose), 0.2));
end;

procedure TEngineTestCases.TestCaseV21;
begin
  glucose := 85 / kGlucoseConversionFactor;
  insulin := 0.5 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(0.127, SPINA_GBeta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(41.926, SPINA_GR(insulin, glucose), 0.01));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(8.182, HOMA_Beta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(0.105, HOMA_IR(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(9.529, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.614, QUICKI(insulin, glucose), 0.2));
end;

procedure TEngineTestCases.TestCaseV30;
begin
  glucose := 89 / kGlucoseConversionFactor;
  insulin := 10.7 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(2.637, SPINA_GBeta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(1.938, SPINA_GR(insulin, glucose), 0.1));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(148.154, HOMA_Beta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(2.351, HOMA_IR(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(0.425, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.336, QUICKI(insulin, glucose), 0.2));
end;

procedure TEngineTestCases.TestCaseV39;
begin
  glucose := 108 / kGlucoseConversionFactor;
  insulin := 29.6 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(6.54, SPINA_GBeta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(0.58, SPINA_GR(insulin, glucose), 0.01));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(237.4, HOMA_Beta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(7.9, HOMA_IR(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(0.13, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.3, QUICKI(insulin, glucose), 0.2));
end;

procedure TEngineTestCases.TestCaseV114;
begin
  glucose := 123 / kGlucoseConversionFactor;
  insulin := 122.8 * kInsulinConversionFactor;
  AssertTrue(WithinTolerance(25.357, SPINA_GBeta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(0.16, SPINA_GR(insulin, glucose), 0.1));
  AssertEquals(SPINA_GBeta(insulin, glucose) * SPINA_GR(insulin, glucose),
               SPINA_DI(insulin, glucose));
  AssertTrue(WithinTolerance(736.800, HOMA_Beta(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(37.295, HOMA_IR(insulin, glucose), 0.01));
  AssertTrue(WithinTolerance(0.027, HOMA_IS(insulin, glucose), 0.2));
  AssertTrue(WithinTolerance(0.239, QUICKI(insulin, glucose), 0.2));
end;

procedure TControlTestCases.PositiveCheck;
begin
  AssertNull('This test is bound to succeed', nil);
end;


initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TEngineTestCases);
end.

