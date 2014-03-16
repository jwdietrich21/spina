unit about_form;

interface

uses PSL;

const
  MainForm = AutoID;
  AboutHelpString = AutoID;
  MiniAppIcon = AutoID;
  AboutForm = AutoID;

function HandleEvent(var Event: EventType): Boolean;

implementation

const
  Label1 = AutoID;
  Label2 = AutoID;
  Label3 = AutoID;
  Label4 = AutoID;
  Label5 = AutoID;
  Button1 = AutoID;

resource
  STRING AboutHelpString '' + 'Please enter simultaneously measured values for TSH, T4 or FT4 and T3 or FT3 and click on "Calculate" to obtain structure parameters of thyrotropic feedback control.' + #10 + #10 + 'You may change measurement methods and units by using the small popup menus.' + #10 + #10 + 'Please find additional information at http://spina.medical-cybernetics.de.';
  BITMAP MiniAppIcon 'IconMBW.bmp' 'IconMC.bmp' {DENSITY 72/108/144} {DIRECTCOLOR} TRANSPARENCY 0 0;
  FORM AboutForm AT (2 57 156 100) NOFRAME MODAL HELP AboutHelpString
  BEGIN
    TITLE 'About SPINA Thyr'
    FORMBITMAP AT (12 18) BITMAP MiniAppIcon
    LABEL Label1 'SPINA' AT (34 20) FONT 1
    LABEL Label2 'Thyr' AT (50 30) FONT 1
    LABEL Label3 '3.2' AT (34 38) FONT 1
    LABEL Label4 #169' 1997-2010 J. W. Dietrich' AT (34 51)
    LABEL Label5 'http://spina.medical-cybernetics.de' AT (4 66)
    BUTTON Button1 'OK' AT (5 83 34 12)
  END;

procedure AboutFormOpen;
begin
  PSForm.Draw;
end;

function AboutFormCustomEvent: Boolean;
begin
  Result := False;
end;

procedure Button1Select;
begin
  FrmReturnToForm(MainForm);
end;

function HandleEvent(var Event: EventType): Boolean;
begin
  // generated code, don't modify this function
  PSApplication.Event := @Event;
  PSApplication.Handled := True;

  if not AboutFormCustomEvent then
    case Event.eType of
      ctlSelectEvent:
        with Event.ctlSelect do
          case controlID of
            Button1:
              Button1Select;

            else
              PSApplication.Handled := False;
          end;

      frmOpenEvent:
        AboutFormOpen;

      else
        PSApplication.Handled := False;
    end;

  Result := PSApplication.Handled;
end;

end.