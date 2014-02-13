program spina_thyr;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm  zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.4.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ Main unit }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}
{off $DEFINE DEBUG}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Controls, SPINA_UserInterface, SPINA_SplashScreen, SPINA_AboutBox,
  SPINA_ResultDialog, SPINA_Engine, Printer4Lazarus, SPINA_Types,
  HandlePreferences, spina_toolbar, SetPreferences, spina_help, unitconverter,
  HandleImpEx, HL7, obx, msa, msh, obr, CDISC;

{{$IFDEF WINDOWS}{$R spina_thyr.rc}{$ENDIF}}

{$R *.res}

begin
  Application.Title:='SPINA Thyr';
  {$IFDEF DEBUG}{$IFDEF UNIX}
  SetHeapTraceOutput('~/heaptrace.trc');
  {$ELSE}
  SetHeapTraceOutput('c:\heaptrace.trc');
  {$ENDIF}{$ENDIF}
  InitThyroidHormoneConversionFactors;
  Application.Initialize;
  Application.CreateForm(THauptschirm, Hauptschirm);
  SplashScreen := TSplashScreen.Create(nil);
  SplashScreen.ShowOnTop;
  SplashScreen.FormStyle := fsSplash;
  SplashScreen.AlphaBlendValue := 200;
  Application.ProcessMessages;
  gStartup := true;
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TResultForm, ResultForm);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.CreateForm(TSPINAToolbar, SPINAToolbar);
  with SPINAToolbar do
  begin
    hide;
    {$IFDEF LCLCarbon}
    left := 0;
    top := 20;
    width := Screen.Width;
    height := Toolbar1.Height + 3;
    {$ELSE}
    left := 1;
    top := 0;
    width := Screen.Width - 6;
    height := Toolbar1.Height + 26;
    {$ENDIF}
    WindowState := wsNormal;
    AlphaBlend := false;
  end;
  AboutBox.Tabs.TabIndex := 0;
  AboutBox.Close;
  ResultForm.Close;
  gStartup := false;
  SPINAToolbar.Show;
  gUseReferenceRanges := true;
  Hauptschirm.AlphaBlendValue := 255;
  Application.CreateForm(THelpWindow, HelpWindow);
  Application.Run;
  if (SplashScreen<>nil) then begin
    SplashScreen.Free;
    SplashScreen:=nil;
  end;
  if gPreferences.rememberUsedUnits then
    SavePreferences;
  Hauptschirm.Free;
  Hauptschirm:=nil;
end.
