program GLPanelDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form2},
  Execute.FMX.GLPanels in 'Execute.FMX.GLPanels.pas',
  Execute.FMX.GLPanels.Win in 'Execute.FMX.GLPanels.Win.pas',
  Execute.CubeMan in 'Execute.CubeMan.pas',
  Execute.CrossGL in 'Execute.CrossGL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
