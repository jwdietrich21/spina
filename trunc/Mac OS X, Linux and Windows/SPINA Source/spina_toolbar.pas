unit spina_toolbar;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdActns, StdCtrls, LCLType, Menus, ActnList, ComCtrls,
  SPINA_AboutBox, SPINA_Userinterface, VersionSupport;

type

  { TSPINAToolbar }

  TSPINAToolbar = class(TForm)
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
    ImageList1: TImageList;
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
    SPINAThyrLabel: TLabel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    UndoMenuItem: TMenuItem;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure AppleAboutMenuItemClick(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyResultMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);
    procedure PrintMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SPINAToolbar: TSPINAToolbar;

implementation

{ TSPINAToolbar }

procedure TSPINAToolbar.ToolButton1Click(Sender: TObject);
begin

end;

procedure TSPINAToolbar.FormCreate(Sender: TObject);
var
  modifierKey: TShiftState;
begin
  SPINAToolbar.SPINAThyrLabel.Caption := 'SPINA Thyr ' + GetFileVersion;
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  SPINAToolbar.HelpMenu.Visible := False;
  SPINAToolbar.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  SPINAToolbar.HelpMenu.Visible := True;
  SPINAToolbar.AppleMenu.Visible := False;
  {$ENDIF}
  SPINAToolbar.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  SPINAToolbar.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  SPINAToolbar.PrintMenuItem.ShortCut := ShortCut(VK_P, modifierKey);
  SPINAToolbar.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  SPINAToolbar.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  SPINAToolbar.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  SPINAToolbar.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  SPINAToolbar.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
  SPINAToolbar.CopyResultMenuItem.ShortCut := ShortCut(VK_R, modifierKey);
end;

procedure TSPINAToolbar.PageSetupMenuItemClick(Sender: TObject);
begin
  Hauptschirm.PageSetupMenuItemClick(Sender);
end;

procedure TSPINAToolbar.PrintMenuItemClick(Sender: TObject);
begin
  Hauptschirm.PrintMenuItemClick(Sender);
end;

procedure TSPINAToolbar.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TSPINAToolbar.AppleAboutMenuItemClick(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TSPINAToolbar.AboutMenuItemClick(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TSPINAToolbar.CloseMenuItemClick(Sender: TObject);
begin
  Hauptschirm.Close;
end;

procedure TSPINAToolbar.CopyResultMenuItemClick(Sender: TObject);
begin
  Hauptschirm.Ergebniskopieren1Click(Sender);
end;

procedure TSPINAToolbar.ToolButton2Click(Sender: TObject);
begin

end;

initialization
  {$I spina_toolbar.lrs}

end.
