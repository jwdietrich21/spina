unit CaseDemonstrator;

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

{ This unit shows examples of cases in the CLI interface }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SPINA_Engine;

const
  Glucose_Vellore: array[0..116] of real = (92, 85, 91, 76, 86, 7.04, 89, 91,
    94, 98, 109, 84, 87, 10.04, 88, 81, 85, 80, 89, 98, 85, 104, 97, 84, 87,
    87, 94, 100, 90, 89, 90, 84, 100, 93, 100, 95, 91, 100, 108, 93, 93, 82,
    86, 92, 8, 89, 94, 107, 77, 83, 77, 88, 97, 82, 91, 85, 85, 84, 90, 90,
    80, 90, 104, 106, 97, 87, 81, 83, 105, 103, 87, 86, 85, 81, 87, 85, 101,
    86, 80, 87, 91, 94, 97, 75, 83, 84, 87, 94, 97, 89, 101, 88, 85, 93, 101,
    98, 103, 102, 92, 102, 95, 78, 89, 81, 89, 86, 94, 85, 99, 97, 87, 91, 88,
    123, 107, 96, 87);
  Insulin_Vellore: array[0..116] of real = (1.2, 0.8, 1.2, 3.8, 5.9, 3.1, 5,
    3.2, 4.7, 0.9, 8.5, 2.3, 1.5, 3.8, 3.7, 1.1, 3.3, 6.6, 2.8, 12.9, 0.5, 1,
    3.5, 6.4, 4.1, 4.5, 2.6, 1.4, 2.8, 10.7, 6, 8.9, 2.2, 2.1, 1.4, 8.29, 4.3,
    1.9, 29.6, 0.9, 1.9, 4.3, 0.9, 1, 5.1, 5.7, 7.5, 17.5, 3.5, 2.5, 3.3, 3.5,
    18, 2.7, 9.4, 4.2, 1.8, 4.3, 6.9, 3.1, 4.5, 2.2, 10, 4.9, 4.5, 1, 2.5, 3.5,
    6.67, 4.3, 3.8, 2.9, 5.3, 1.1, 2, 8.3, 6.5, 1.5, 3.6, 12.5, 22, 0.9, 3.1,
    5, 0.5, 1.3, 1.1, 0.9, 2.1, 4.9, 2.3, 1, 7.2, 4.5, 5.4, 4.2, 11.18, 2.3,
    11.1, 4.31, 6.8, 3.7, 31.4, 1, 11.9, 2.1, 5.8, 3.7, 22.9, 57.1, 2.6, 1,
    4, 122.8, 3.2, 6.4, 5.5);

procedure ShowVelloreExamples(n, precision, digits: integer);

implementation

procedure ShowVelloreExamples(n, precision, digits: integer);
var
  i: integer;
  insulin, glucose: real;
begin
  for i := 0 to n - 1 do
  begin
    writeln('Scenario Vellore ' + IntToStr(i + 1) + ':');
    Insulin := Insulin_Vellore[i] * kInsulinConversionFactor;
    Glucose := Glucose_Vellore[i] / kGlucoseConversionFactor;
    Write('Insulin: ');
    write(FloatToStrF(Insulin, ffFixed, precision, digits));
    writeln(' pmol/L');
    Write('Glucose: ');
    write(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln(' mmol/L');
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-Beta: ' + FloatToStrF(HOMA_Beta(Insulin, Glucose),
      ffFixed, precision, digits) + '%');
    writeln('HOMA-IR: ' + FloatToStrF(HOMA_IR(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-IS: ' + FloatToStrF(HOMA_IS(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('QUICKI: ' + FloatToStrF(QUICKI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();
  end;
end;

end.
{ References:

Dietrich JW, Dasgupta R, Anoop S, Jebasingh F, Kurian ME, Inbakumari M,
Boehm BO, Thomas N. SPINA Carb: a simple mathematical model supporting
fast in-vivo estimation of insulin sensitivity and beta cell function.
Sci Rep. 2022 Oct 21;12(1):17659. doi: 10.1038/s41598-022-22531-3.
PMID: 36271244; PMCID: PMC9587026.

Dietrich JW, Abood A, Dasgupta R, Anoop S, Jebasingh FK, Spurgeon R,
Thomas N, Boehm BO. A novel simple disposition index (SPINA-DI) from
fasting insulin and glucose concentration as a robust measure of
carbohydrate homeostasis. J Diabetes. 2024 Jan 2.
doi: 10.1111/1753-0407.13525. Epub ahead of print. PMID: 38169110.

}
