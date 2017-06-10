unit Main;

{

  Isometric rendering under Delphi FMX (c) 2017 Execute SARL
  http://www.execute.fr

}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math.Vectors,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Controls3D, FMX.Objects3D;

type
  TMainForm = class(TForm3D)
    procedure Form3DRender(Sender: TObject; Context: TContext3D);
  private
    { Déclarations privées }
    procedure RenderMap(Context: TContext3D; x, y: Integer);
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.Form3DRender(Sender: TObject; Context: TContext3D);
var
  x, y: Integer;
begin
  // Unseing cs2DScene allows to replace the Projection Martrix by the RendeToMatrix matrix !
  Context.SetContextState(TContextState.cs2DScene);

  // General settings
  Context.SetContextState(TContextState.csZTestOn);
  Context.SetContextState(TContextState.csZWriteOn);
  Context.SetContextState(TContextState.csAlphaBlendOff);
  Context.SetContextState(TContextState.csStencilOff);
  Context.SetContextState(TContextState.csColorWriteOn);
  Context.SetContextState(TContextState.csScissorOff);
  Context.SetContextState(TContextState.csFrontFace);

  // Setup the Projection Matrix
  Context.SetRenderToMatrix(
  // 45° rotation on Y
      TMatrix3D.CreateRotationY(-45*PI/180)
  // 20° rotation on X
    * TMatrix3D.CreateRotationX(-20*PI/180)
  // move the scene to the back to avoid Z Clipping
    * TMatrix3D.CreateTranslation(TPoint3D.Create(0, 0, -500))
  // create an iometric projection
    * TMatrix3D.CreateOrthoOffCenterRH(
        -Context.Width/50, -Context.Height/50,
        +Context.Width/50, +Context.Height/50,
        1, 1000
      )
  );

  // Clear the screen and the ZBuffer
  Context.Clear([TClearTarget.Color, TClearTarget.Depth], TAlphaColors.White, 1, 0);

  // Render some Cubes
  for x := -5 to +5 do
    for y := -5 to +5 do
     RenderMap(Context, x, y);
end;

procedure TMainForm.RenderMap(Context: TContext3D; x, y: Integer);
const
  K = 2;    // width of the cubes
  L = 1.9;  // a little less to avoid ZBuffer conflit when drawing borders
var
  Center: TPoint3D;
  Size  : TPoint3D;
  H     : Single;
begin
  // compute the Height of the map at x, y
  H := (50 - 2 * x * y) * K/25;
  // Center of the cube
  Center := TPoint3D.Create(x * K, -H/2, y * K);
  // Size of the so called cube
  Size := TPoint3D.Create(L, H, L);

  // Draw faces
  Context.FillCube(
    Center,
    Size,
    1,
    TAlphaColors.Greenyellow
  );

  // Draw borders
  Context.DrawCube(
    Center,
    Size,
    1,
    TAlphaColors.Gray
  );

end;

end.
