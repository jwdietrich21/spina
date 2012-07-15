{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.3 }

{ (c) J. W. Dietrich, 1994 - 2012 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2012 }

{ Main unit }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

program spina_thyr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
SPINA_UserInterface, SPINA_SplashScreen, SPINA_AboutBox, SPINA_ResultDialog,
SPINA_Engine, Printer4Lazarus, SPINA_Types;

{{$IFDEF WINDOWS}{$R spina_thyr.rc}{$ENDIF}}

{$R *.res}

begin
  gStartup := true;
  Application.Title:='SPINA Thyr';
  Application.Initialize;
  SplashScreen := TSplashScreen.Create(nil);
  SplashScreen.ShowOnTop;
  Application.ProcessMessages;
  Application.CreateForm(THauptschirm, Hauptschirm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TResultForm, ResultForm);
  AboutBox.Close;
  ResultForm.Close;
  gStartup := false;
  Hauptschirm.AlphaBlendValue := 255;
  Application.Run;
  if (SplashScreen<>nil) then begin
    SplashScreen.Free;
    SplashScreen:=nil;
  end;

  SavePreferences;
  Hauptschirm.Free;
  Hauptschirm:=nil;
end.

