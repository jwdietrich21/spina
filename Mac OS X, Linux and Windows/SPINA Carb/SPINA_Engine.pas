unit SPINA_Engine;

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

{ This unit implements the calculation engine }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}
{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, Math;

const
  kInsulinConversionFactor = 6;  // A Voelund 1993, L. Heinemann 2010
  kGlucoseConversionFactor = 18; // derived from molar mass
  kPicoFactor = 1e12;
  kMilliFactor = 1e3;
  betaI = 3.4e-3;
  alphaI = 0.2;
  dBeta = 7e-3;
  alphaG = 0.11;
  betaG = 7.1e-4; 
  P0 = 150e-6;
  DR = 1.6e-9;
  GE = 50;
  p1 = 22.5;
  p2 = 20;
  p3 = 3.5;
  kError101 = 'Runtime error: Negative parameters';
  kError102 = 'Runtime error: Parameter out of range';

function SPINA_GBeta(const Insulin, Glucose: real): real;
function SPINA_GR(const Insulin, Glucose: real): real;
function SPINA_DI(const Insulin, Glucose: real): real;
function HOMA_IR(const Insulin, Glucose: real): real;
function HOMA_Beta(const Insulin, Glucose: real): real;
function HOMA_IS(const Insulin, Glucose: real): real;
function QUICKI(const Insulin, Glucose: real): real;

implementation

function SPINA_GBeta(const Insulin, Glucose: real): real;
// Insulin expected in pmol/l, Glucose in mmol/l
begin
  assert((isNan(Insulin) or (Insulin >= 0)) and (isNan(Glucose) or (Glucose >= 0)), kError101);
  result := NaN;
  if not isNan(Insulin) and (Insulin > 0) and not isNan(Glucose) and (Glucose > 0) then 
    result := kPicoFactor * betaI * Insulin / kPicoFactor * (dBeta + Glucose / kMilliFactor) / (alphaI * Glucose / kMilliFactor);
  if (Insulin = 0) and (not isNan(Glucose)) then result := 0;
end;

function SPINA_GR(const Insulin, Glucose: real): real;
// Insulin in pmol/l, Glucose in mmol/l
begin
  assert((isNan(Insulin) or (Insulin >= 0)) and (isNan(Glucose) or (Glucose >= 0)), kError101);
  result := NaN;
  if not isNan(Insulin) and (Insulin > 0) and not isNan(Glucose) and (Glucose > 0) then 
    result := alphaG * P0 * (DR + Insulin / kPicoFactor) / (betaG * GE * Insulin / kPicoFactor * Glucose / kMilliFactor) - DR / (GE * Insulin / kPicoFactor) - 1 / GE;
  if result < 0 then result := 0; // correction for extreme insulin resistance
end;

function SPINA_DI(const Insulin, Glucose: real): real;
// Insulin in pmol/l, Glucose in mmol/l
begin
  assert((isNan(Insulin) or (Insulin >= 0)) and (isNan(Glucose) or (Glucose >= 0)), kError101);
  result := NaN;
  if not isNan(Insulin) and (Insulin > 0) and not isNan(Glucose) and (Glucose > 0) then
    result := SPINA_GBeta(Insulin, Glucose) * SPINA_GR(Insulin, Glucose);
end;

function HOMA_IR(const Insulin, Glucose: real): real;
// Insulin in pmol/l, Glucose in mmol/l
begin
  assert((isNan(Insulin) or (Insulin >= 0)) and (isNan(Glucose) or (Glucose >= 0)), kError101);
  result := NaN;
  if not isNan(Insulin) and (Insulin > 0) and not isNan(Glucose) and (Glucose > 0) then 
    result := Glucose * Insulin / kInsulinConversionFactor / p1;
end;

function HOMA_Beta(const Insulin, Glucose: real): real;
// Insulin in pmol/l, Glucose in mmol/l
begin
  assert((isNan(Insulin) or (Insulin >= 0)) and (isNan(Glucose) or (Glucose >= 0)), kError101);
  result := NaN;
  if not isNan(Insulin) and (Insulin > 0) and not isNan(Glucose) and (Glucose > p3) then
    result := p2 * Insulin / kInsulinConversionFactor / (Glucose - p3);
end;

function HOMA_IS(const Insulin, Glucose: real): real;
// Insulin in pmol/l, Glucose in mmol/l
begin
  assert((isNan(Insulin) or (Insulin >= 0)) and (isNan(Glucose) or (Glucose >= 0)), kError101);
  result := NaN;
  if not isNan(Insulin) and (Insulin > 0) and not isNan(Glucose) and (Glucose > 0) and (HOMA_IR(Insulin, Glucose) <> 0) then
    result := 1 / HOMA_IR(Insulin, Glucose);
end;

function QUICKI(const Insulin, Glucose: real): real;
// Insulin in pmol/l, Glucose in mmol/l
begin
  assert((isNan(Insulin) or (Insulin >= 0)) and (isNan(Glucose) or (Glucose >= 0)), kError101);
  result := NaN;
  if not isNan(Insulin) and (Insulin > 0) and not isNan(Glucose) and (Glucose > 0) then 
    result := 1 / (log10(Insulin / kInsulinConversionFactor) + log10(Glucose * kGlucoseConversionFactor));
end;

end.
