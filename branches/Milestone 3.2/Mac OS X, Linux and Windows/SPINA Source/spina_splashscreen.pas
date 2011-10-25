{ SPINA-Thyr }
{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }
{ Version 3.2 }

{ J. W. Dietrich, Klinikum der LMU München 1997-2001 }
{ J. W. Dietrich, Universitätsklinikum Ulm 2002-2004 }
{ J. W. Dietrich, Universitätsklinikum Bergmannsheil 2005-2010 }

{ This software is provided via a BSD licence }
{ See http://spina.medical-cybernetics.de for details }

unit SPINA_SplashScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Grids, SPINA_UserInterface;

type

  { TSplashScreen }

  TSplashScreen = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure TimerEvent(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SplashScreen: TSplashScreen;

implementation

{ TSplashScreen }

procedure TSplashScreen.TimerEvent(Sender: TObject);
begin
  close;
end;

procedure TSplashScreen.FormCreate(Sender: TObject);
begin
  refresh;
  {$IFDEF LCLcarbon}
  FormStyle:=fsNormal;
  {$ELSE}
  FormStyle:=fsStayOnTop;
  {$ENDIF}

end;

procedure TSplashScreen.Image1Click(Sender: TObject);
begin

end;

initialization
  {$I spina_splashscreen.lrs}

end.

