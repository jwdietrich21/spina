{ SPINA-Thyr }
{ Splash-Screen }

unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TSplashScreen = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  SplashScreen: TSplashScreen;

procedure ShowSplash;

implementation

uses Unit1;

{$R *.DFM}

procedure ShowSplash;
begin
  with TSplashScreen.Create(Application) do
  try
    Hauptschirm.Visible:=false;
    SplashScreen.Visible:=true;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TSplashScreen.Timer1Timer(Sender: TObject);
begin
Close;
end;

procedure TSplashScreen.FormCreate(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_Style, GetWindowLong(Handle, GWL_Style) and not WS_CAPTION);
  ClientHeight := Height;
  Refresh;
end;

end.
