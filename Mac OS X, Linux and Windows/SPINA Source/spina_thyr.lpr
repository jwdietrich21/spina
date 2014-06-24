{ SPINA-Thyr }
{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }
{ Version 3.2 }

{ J. W. Dietrich, Klinikum der LMU München 1997-2001 }
{ J. W. Dietrich, Universitätsklinikum Ulm 2002-2004 }
{ J. W. Dietrich, Universitätsklinikum Bergmannsheil 2005-2010 }

{ This software is provided via a BSD licence }
{ See http://spina.medical-cybernetics.de for details }

program spina_thyr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources
  { you can add units after this },
SPINA_UserInterface, SPINA_SplashScreen, SPINA_AboutBox, SPINA_ResultDialog,
SPINA_Engine, Printer4Lazarus;

{$IFDEF WINDOWS}{$R spina_thyr.rc}{$ENDIF}

begin
  {$I spina_thyr.lrs}
  Application.Title:='SPINA Thyr';
  Application.Initialize;
  SplashScreen := TSplashScreen.Create(nil);
  SplashScreen.Show;
  Application.ProcessMessages;
  Application.CreateForm(THauptschirm, Hauptschirm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TResultForm, ResultForm);
  Application.Run;
  if (SplashScreen<>nil) then begin
    SplashScreen.Free;
    SplashScreen:=nil;
  end;

  SavePreferences;
  Hauptschirm.Free;
  Hauptschirm:=nil;
end.

