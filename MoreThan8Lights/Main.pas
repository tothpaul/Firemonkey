unit Main;

interface

{
  How to enable more than 8 lights on a FMX 3D Project

  (c)2020 Execute SARL

  this project activate the 9th light when Plane9 is rendered
  (from the Parent dummy because the OnRender event occurs to late in the rendering process of the object)

  french video available here
	https://youtu.be/fL5CmQdDTfg


}

uses
  Winapi.Windows,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Types3D, FMX.Controls3D, FMX.MaterialSources,
  FMX.Objects3D, FMX.Viewport3D;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Plane1: TPlane;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Plane2: TPlane;
    Light2: TLight;
    Plane3: TPlane;
    Light3: TLight;
    Plane4: TPlane;
    Light4: TLight;
    Plane5: TPlane;
    Light5: TLight;
    Plane6: TPlane;
    Light6: TLight;
    Plane7: TPlane;
    Light7: TLight;
    Plane8: TPlane;
    Light8: TLight;
    Plane9: TPlane;
    Light9: TLight;
    Dummy1: TDummy;
    procedure Dummy1Render(Sender: TObject; Context: TContext3D);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Generics.Collections;

type
  THackList<T> = class(TEnumerable<T>)
  private
    Items: array of T;
  end;
  TLightList = THackList<TLightDescription>;

procedure TForm1.Dummy1Render(Sender: TObject; Context: TContext3D);
begin
  TLightList(Context.Lights).Items[0].Enabled := False;
end;

end.
