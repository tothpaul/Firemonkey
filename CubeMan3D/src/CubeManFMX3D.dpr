program CubeManFMX3D;

uses
 // Removing those 2 units speedup a little the application starting process
 // System.StartUpCopy,
 // FMX.MobilePreview,
  FMX.Forms,
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
