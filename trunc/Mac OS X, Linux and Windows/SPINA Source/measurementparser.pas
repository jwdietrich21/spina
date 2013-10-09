unit MeasurementParser;

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

{ This unit implements the calculation engine }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  tMeasurement = record
    Value: extended;
    uom: string;
  end;

var
  gPosition: integer;

function parsedMeasurement(measurement: string): tMeasurement;

implementation

const
  kTAB = chr(9);
  kLF = chr(10);
  kCR = chr(13);
  kETB = char(23);
  DEC_POINT = '.';
  DEC_COMMA = ',';

function ValidChar(theChar: char): boolean;
  {check a character in a string representing a number for validity}
type
  format1 = set of char;
var
  formatn1, formatn2, formatd1, formatd2, formate, formata, formato,
  formatc, formatl, validformat: format1;
begin
  formatn1 := ['1'..'9', '0', kTAB];
  formatn2 := ['1'..'9', '0'];
  formatd1 := ['.', ','];
  formatd2 := ['.'];
  formate := ['e', 'E'];
  formata := [' '..chr(255)];
  formato := ['+', '-'];
  formatc := [char($1c), char($1d), char($1e), char($1f), char($08)];
  formatl := [kCR, kLF];
  validformat := formatn2 + formatd1 + formate + formato;
  if theChar in validformat then
    ValidChar := True
  else
    ValidChar := False;
end;

function NextChar(theString: string): char;
{ read next char of string }
begin
  if gPosition <= length(theString) then
  begin
    NextChar := theString[gPosition];
    gPosition := gPosition + 1;
  end
  else
    NextChar := kETB;
end;

function parsedMeasurement(measurement: string): tMeasurement;
{ decompose measurement result into numeric value and unit }
var
  ch: char;

  function Number: extended;
  {$IFDEF FPC}  {version for FPC}
  var
    i, n: integer;
    valString: string;
  begin
    valString := '';
    n := length(measurement);
    number := NaN;
    if n > 0 then
    begin
      i := 1;
      ch := measurement[i];
      while (ValidChar(ch)) and (i <= n) do
      begin
        valString := valString + ch;
        inc(i);
        ch := measurement[i];
      end;
      if pos(DEC_COMMA, valString) > 0 then
        decimalSeparator := DEC_COMMA
      else
        decimalSeparator := DEC_POINT;
      number := StrToFloat(valString);
      gPosition := i + 1;
    end;
  end;
  {$ELSE}  {version for other compilers}
  var
    i, j, d, e, f: integer;
    k, l: extended;
    dig: array[0..31] of char;
    wholes, exponent: boolean;
    exponent_sign, base_sign: integer;
  begin
    wholes := True;
    exponent := False;
    f := 0;
    i := 0;
    exponent_sign := 1;
    base_sign := 1;
    repeat
      dig[i] := ch;
      i := i + 1;
      ch := NextChar(measurement);
    until not (ValidChar(ch));
    j := 0;
    k := 0;
    l := 0;
    repeat
      if (dig[j] <> '.') and (dig[j] <> ',') and (uppercase(dig[j]) <>
        uppercase('e')) and (dig[j] <> '+') and (dig[j] <> '-') then
      begin
        d := integer(dig[j]) - $30;
        if not exponent then
        begin
          if wholes then
          begin
            if (d < 10) and ((maxlongint - d) div 10 >= k) then
              k := k * 10 + d;
          end
          else
          begin
            f := f + 1;
            k := k + d / (exp(ln(10) * f));
          end;
        end
        else
        begin
          if wholes then
          begin
            if (d < 10) and ((maxlongint - d) div 10 >= l) then
              l := l * 10 + d;
          end
          else
          begin
            f := f + 1;
            l := l + d / (exp(ln(10) * f));
          end;
        end;
      end
      else if (dig[j] = '.') or (dig[j] = ',') then
        wholes := False
      else if uppercase(dig[j]) = uppercase('e') then
      begin
        exponent := True;
        wholes := True;
      end
      else if dig[j] = '-' then
        if exponent then
          exponent_sign := -1
        else
          base_sign := -1;
      j := j + 1;
    until j = i;
    k := k * base_sign;
    l := l * exponent_sign;
    number := k * exp(ln(10) * l);
  end;
  {$ENDIF}

begin
  gPosition := 1;
  ch := NextChar(measurement);
  if not ValidChar(ch) then
  begin
    parsedMeasurement.Value := NaN;
    parsedMeasurement.uom := '';
  end
  else
  begin
    parsedMeasurement.Value := Number;
    parsedMeasurement.uom := RightStr(measurement, length(measurement) - gPosition + 1);
  end;
end;

end.
