unit SPINA_Resources;

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

{ Locale-specific strings }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  { Language 1: German (deutsch); language 2: English }
  kUncertified11 = ' (Version für Forschungszwecke)';
  kUncertified12 = ' (Research version)';
  kUncertified21 =
    'Einsatz für Forschungszwecke nach §3 und §6 MPG und Art. 1(2)h der Richtlinie 93/42/EWG des Rates.';
  kUncertified22 = 'Research use according to article 1(2)h of the council directive 93/42/EEC.';
  kUncertified31 = 'Einsatz nur zu Forschungszwecken.';
  kUncertified32 = 'Research use only.';
  kUnCertified41 = 'Die Verwendung ist gemäß GCP, nach §12 MPG und Anhang X der Richtlinie 93/42/EWG zu dokumentieren.';
  kUnCertified42 = 'Usage has to be documented in accordance with GCP and annex X of the council directive 93/42/EEC.';

  kBetaHint = 'This version of SPINA Carb is beta software that is provided for purposes of testing.' + LineEnding +
              'Please report any errors via https://sourceforge.net/p/spina/_list/tickets.' + LineEnding + LineEnding +
              'Diese Version von SPINA Carb ist Beta-Software, die für Testzwecke bereitgestellt wird.' + LineEnding +
              'Bitte melden Sie Fehler über https://sourceforge.net/p/spina/_list/tickets.';

  HL7_Error = 'HL7 Error';
  pathError = 'Empty path';

  kMacUnavailable = 'Unavailable on macOS';

var
  gUncertified1, gUncertified2, gUncertified3, gUncertified4: string;

implementation

end.

