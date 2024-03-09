unit InputFilter;

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

function InsulinSI(RawIns: extended; InsUom: String): extended;
function GlucoseSI(RawGlc: extended; GlcUom: String): extended;

implementation

function InsulinSI(RawIns: extended; InsUom: String): extended;
begin
  result := ConvertedValue(RawIns, kInsulinConversionFactor, InsUom, 'pmol/l');
end;

function GlucoseSI(RawGlc: extended; GlcUom: String): extended;
begin

end;

end.

