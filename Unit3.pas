unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  ev_sim_unit;

type
  TSplashForm = class(TForm)
    Timer1: TTimer;
    Image1: TImage;
    procedure Time1rOnTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FTimeIsUp : boolean;
    function ChkTimeIsUp: boolean;
  public
    { Public declarations }
    property TimeIsUp: boolean read ChkTimeIsUp write FTimeIsUp;
  end;


var
  SplashForm: TSplashForm;

implementation

{$R *.dfm}

procedure TSplashForm.Time1rOnTimer(Sender: TObject);
begin
   TimeIsUp := true;
   Timer1.Enabled := false;
end;


function TSplashForm.ChkTimeIsUp: boolean;
begin
  sleep(50);
  result := FTimeIsUp;
end;


procedure TSplashForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  FormStyle := fsStayOnTop;
  FTimeIsUp := false;
  // display splash scr logo at the center of the screen
  Left:=(Screen.Width-Width)  div 2;
  Top:=(Screen.Height-Height) div 2;
end;

procedure TSplashForm.FormDestroy(Sender: TObject);
begin
     SplashForm := nil;
end;

procedure TSplashForm.FormShow(Sender: TObject);
begin
  //nothing to do
end;

end.
