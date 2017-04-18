program GLPanelDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form2},
  Execute.FMX.GLPanels in 'Execute.FMX.GLPanels.pas',
  Execute.CubeMan in 'Execute.CubeMan.pas',
  Execute.CrossGL in 'Execute.CrossGL.pas',
  testGl in 'testGl.pas' {GlView},
  Execute.FMX.GLPanels.Types in 'Execute.FMX.GLPanels.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TGlView, GlView);
  Application.Run;
end.
