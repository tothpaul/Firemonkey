program PuzzleSolver;

uses
  System.StartUpCopy,
  FMX.Forms,
  PuzzleSolver.Main in 'PuzzleSolver.Main.pas' {Main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
