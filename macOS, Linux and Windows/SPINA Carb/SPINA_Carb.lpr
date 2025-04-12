program SPINA_Carb;

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

{ Main program unit }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  {$IFDEF Darwin}
  MacOSAll,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, GUI, SPINA_Engine, UnitConverter, CaseBroker, ResultWindow, SPINATypes,
  SPINA_GUIServices, EnvironmentInfo, PrintCase, printer4lazarus,
  spina_aboutbox, HandlePreferences, setpreferences, HandleCDISC, Barcode,
  SPINA_Resources, HandleImpEx, ERR, EVN, HL7, MLLP, MSA, MSH, NK1, NTE, OBR,
  OBX, PID, PV1, PV2, SPM, LocaleServices;

{$R *.res}

begin
  {$IFDEF debug}
  if FileExists('heaptrace.trc') then
    DeleteFile('heaptrace.trc');
  SetHeapTraceOutput('heaptrace.trc');
  {$ENDIF}
  gCEcertified := false;
  GetMacDateFormats;
  RequireDerivedFormResource:=True;
  Application.Title:='SPINA Carb';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(THauptschirm, Hauptschirm);
  Application.CreateForm(TResultForm, ResultForm);
  Application.BringToFront;
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  ReadPreferences;
  ReadRefRanges;
  Application.Run;
end.

