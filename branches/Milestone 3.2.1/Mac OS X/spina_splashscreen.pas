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
  ExtCtrls, Grids, Menus, SPINA_UserInterface;

type

  { TSplashScreen }

  TSplashScreen = class(TForm)
    AboutMenuItem: TMenuItem;
    AppleAboutMenuItem: TMenuItem;
    AppleMenu: TMenuItem;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CopyResultMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    Image1: TImage;
    MainMenu: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    NewMenuItem: TMenuItem;
    PageSetupMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    PrintMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    Timer1: TTimer;
    UndoMenuItem: TMenuItem;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyResultMenuItemClick(Sender: TObject);
    procedure HandleAbout(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);
    procedure PrintMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
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
