unit SPINA_Aboutbox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, EnvironmentInfo, SPINA_GUIServices;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    Logo: TImage;
    VersionLabel: TLabel;
    OKButton: TButton;
    PageControl1: TPageControl;
    SPINALabel: TImage;
    TabSheet1: TTabSheet;
    procedure CloseAboutBox(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

{ TAboutBox }


procedure TAboutBox.CloseAboutBox(Sender: TObject);
begin
  close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  VersionLabel.Caption := 'Version ' + FileVersion;
end;

end.

