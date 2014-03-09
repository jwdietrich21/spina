unit SPINA_SplashScreen;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.4.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ This unit implements a splash screen }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Grids, Menus, StdCtrls, SPINA_UserInterface;

type

  { TSplashScreen }

  TSplashScreen = class(TForm)
    CopyrightLabel1: TLabel;
    CopyrightLabel2: TLabel;
    CopyrightLabel3: TLabel;
    CopyrightLabel4: TLabel;
    CopyrightLabel5: TLabel;
    CopyrightLabel6: TLabel;
    CopyrightLabel7: TLabel;
    Divider: TLabel;
    IdleTimer1: TIdleTimer;
    Image1: TImage;
    URL1: TLabel;
    URL2: TLabel;
    VersionLabel: TLabel;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyResultMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HandleAbout(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);
    procedure PrintMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SplashScreen: TSplashScreen;

implementation

{ TSplashScreen }

procedure TSplashScreen.FormCreate(Sender: TObject);
begin
  refresh;
  {$IFDEF LCLcarbon}
  FormStyle:=fsNormal;
  {$ELSE}
  FormStyle:=fsStayOnTop;
  {$ENDIF}
end;

procedure TSplashScreen.IdleTimer1Timer(Sender: TObject);
begin
  close;
end;

procedure TSplashScreen.HandleAbout(Sender: TObject);
begin
  Hauptschirm.HandleAbout(Sender);
end;

procedure TSplashScreen.CloseMenuItemClick(Sender: TObject);
begin
  close;
end;

procedure TSplashScreen.CopyResultMenuItemClick(Sender: TObject);
begin
  Hauptschirm.Ergebniskopieren1Click(Sender);
end;

procedure TSplashScreen.Image1Click(Sender: TObject);
begin

end;

procedure TSplashScreen.PageSetupMenuItemClick(Sender: TObject);
begin
  Hauptschirm.PageSetupMenuItemClick(Sender);
end;

procedure TSplashScreen.PrintMenuItemClick(Sender: TObject);
begin
  Hauptschirm.PrintMenuItemClick(Sender);
end;

procedure TSplashScreen.QuitMenuItemClick(Sender: TObject);
begin
  Hauptschirm.Beenden1Click(Sender);
end;

initialization
  {$I spina_splashscreen.lrs}

end.
