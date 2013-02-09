unit spina_help;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, LCLIntf, SPINA_Types, SPINA_AboutBox;

type

  { THelpWindow }

  THelpWindow = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  HelpWindow: THelpWindow;

implementation

{ THelpWindow }

procedure THelpWindow.FormCreate(Sender: TObject);
begin

end;

procedure THelpWindow.Image1Click(Sender: TObject);
begin

end;

procedure THelpWindow.Label3Click(Sender: TObject);
begin
  OpenURL(PORTAL_URL);
end;

procedure THelpWindow.Label4Click(Sender: TObject);
begin
  OpenURL(HELP_URL);
end;

procedure THelpWindow.Label6Click(Sender: TObject);
begin
  HelpWindow.Close;
  ShowAboutBox;
end;

initialization
  {$I spina_help.lrs}

end.

