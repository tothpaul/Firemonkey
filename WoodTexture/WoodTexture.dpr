program WoodTexture;

uses
  System.StartUpCopy,
  FMX.Forms,
  WoodTexture.Main in 'WoodTexture.Main.pas' {Main},
  NathaanTFM.AS3Perlin in 'NathaanTFM.AS3Perlin.pas',
  Wood.FlashLike in 'Wood.FlashLike.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
