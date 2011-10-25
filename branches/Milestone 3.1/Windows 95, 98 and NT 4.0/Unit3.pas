{ SPINA-Thyr }
{ AboutBox }

unit Unit3;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, ShellApi;

type
  TAboutBox = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ClickURL(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

  procedure ShowAboutBox;

implementation

{$R *.DFM}

procedure ShowAboutBox;
begin
  with TAboutBox.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_Style, GetWindowLong(Handle, GWL_Style) and not WS_CAPTION);
  ClientHeight := Height;
  Refresh;
end;

procedure TAboutBox.ClickURL(Sender: TObject);
begin
 ShellExecute (Handle, 'Open', 'http://spina.medical-cybernetics.de', nil, nil, SW_Show);
end;

end.
