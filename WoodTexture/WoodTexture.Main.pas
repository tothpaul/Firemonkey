unit WoodTexture.Main;

{
  Wood FlashPascal demo ported to Delphi FMX
  (c)2024 Exectute SARL

}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.Math.Vectors, FMX.MaterialSources, FMX.Controls3D,
  FMX.Objects3D, FMX.Objects, FMX.Viewport3D, FMX.Ani;

type
  TMain = class(TForm)
    Cube1: TCube;
    TextureMaterialSource1: TTextureMaterialSource;
    Viewport3D1: TViewport3D;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    FloatAnimation1: TFloatAnimation;
    Dummy1: TDummy;
    TextureMaterialSource2: TTextureMaterialSource;
    procedure Form3DCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Main: TMain;

implementation

{$R *.fmx}

uses
  Wood.FlashLike;

type
  TCustomCube = class(TCube)
  public
    constructor Create(AOwner: TComponent; Index: Integer);
    property Data;
  end;

procedure emboss(b: BitmapData);
begin
  b.applyFilter(
    b, b.rectangle, TPoint.Create(0, 0),
    ConvolutionFilter.Create(
      3, 3, [
        -2, -1, 0,
        -1,  1, 1,
         0,  1, 2
      ], 1
    )
  );
end;

procedure TMain.Form3DCreate(Sender: TObject);
const
  Size = 256;
var
  b1, b2, b3, b4, b5, b6: BitmapData;
begin
// create 2 bitmaps
  b1 := BitmapData.Create(Size, Size);
  b2 := BitmapData.Create(Size, Size);
// fill them with a Perlin Noise
  b1.perlinNoise( 3, 150, 1, 123, False, True, 0, True);
  b2.perlinNoise(30, 200, 2, 456, False, True, 0, True);

// Combine them, b2 is a displacement map for b1
  b3 := BitmapData.Create(Size, Size);
  b3.applyFilter(
    b1, b1.rectangle, TPoint.Create(0, 0),
    DisplacementMapFilter.Create(b2, TPoint.Create(0, 0), 1, 1, 50, 0, 'wrap')
  );

// add wood color
  b4 := BitmapData.Create(Size, Size);
  b4.applyFilter(
    b3, b3.rectangle, TPoint.Create(0, 0),
    ColorMatrixFilter.Create([
    2/3,  0, 0, 0,64,
      0,1/2, 0, 0, 0,
      0,  0, 0, 0, 0,
      0,  0, 0, 1, 0
    ])
  );

// create plank colors
  b5 := BitmapData.Create(Size,Size);
  b5.fillRect(Rectangle.Create(  0, 0, 31, Size),$FFDA793D);
  b5.fillRect(Rectangle.Create( 32, 0, 31, Size),$FFD56826);
  b5.fillRect(Rectangle.Create( 64, 0, 31, Size),$FF5D250D);
  b5.fillRect(Rectangle.Create( 96, 0, 31, Size),$FF944719);
  b5.fillRect(Rectangle.Create(128, 0, 31, Size),$FF672B0F);
  b5.fillRect(Rectangle.Create(160, 0, 31, Size),$FF4C2113);
  b5.fillRect(Rectangle.Create(192, 0, 31, Size),$FF6D2E10);
  b5.fillRect(Rectangle.Create(224, 0, 31, Size),$FF944719);

// add borders
  emboss(b5);

// combine them for the final texture
  b6 := b4.clone;
  b6.merge(b5, b5.rectangle, TPoint.Create(0, 0), 128, 128, 128, 128);

// shows the different steps
  Image1.Bitmap.Assign(b1);
  Image2.Bitmap.Assign(b2);
  Image3.Bitmap.Assign(b3);
  Image4.Bitmap.Assign(b4);
  Image5.Bitmap.Assign(b5);
  Image6.Bitmap.Assign(b6);

// assign the texture
  TextureMaterialSource1.Texture.Assign(b6);

//  TextureMaterialSource1.Texture.SaveToFile('wood.png');

  b1.Free;
  b2.Free;
  b3.Free;
  b4.Free;
  b5.Free;
  b6.Free;

// add borders on the box
  var C := TCustomCube.Create(Self, 1);
  C.Position.Z := 2.6;
  C.Position.Y := -2;
  C.MaterialSource := Cube1.MaterialSource;
  C.Parent := Cube1;

  C := TCustomCube.Create(Self, 2);
  C.Position.Z := 2.6;
  C.Position.Y := +2;
  C.MaterialSource := Cube1.MaterialSource;
  C.Parent := Cube1;

  C := TCustomCube.Create(Self, 3);
  C.RotationAngle.Y := 90;
  C.Position.X := 2.6;
  C.Position.Y := -2;
  C.MaterialSource := Cube1.MaterialSource;
  C.Parent := Cube1;

  C := TCustomCube.Create(Self, 4);
  C.RotationAngle.Y := 90;
  C.Position.X := 2.6;
  C.Position.Y := +2;
  C.MaterialSource := Cube1.MaterialSource;
  C.Parent := Cube1;

  C := TCustomCube.Create(Self, 2);
  C.RotationAngle.Y := 90;
  C.Position.X := -2.6;
  C.Position.Y := -2;
  C.MaterialSource := Cube1.MaterialSource;
  C.Parent := Cube1;

  C := TCustomCube.Create(Self, 1);
  C.RotationAngle.Y := 90;
  C.Position.X := -2.6;
  C.Position.Y := +2;
  C.MaterialSource := Cube1.MaterialSource;
  C.Parent := Cube1;

  C := TCustomCube.Create(Self, 2);
  C.RotationAngle.Y := 180;
  C.Position.Z := -2.6;
  C.Position.Y := -2;
  C.MaterialSource := Cube1.MaterialSource;
  C.Parent := Cube1;

  C := TCustomCube.Create(Self, 1);
  C.RotationAngle.Y := 180;
  C.Position.Z := -2.6;
  C.Position.Y := +2;
  C.MaterialSource := Cube1.MaterialSource;
  C.Parent := Cube1;

  C := TCustomCube.Create(Self, 1);
  C.RotationAngle.Z := 45;
  C.Position.Z := 2.8;
  C.MaterialSource := Cube1.MaterialSource;
  C.Parent := Cube1;

  C := TCustomCube.Create(Self, 3);
  C.RotationAngle.Z := -45;
  C.Position.Z := -2.8;
  C.MaterialSource := Cube1.MaterialSource;
  C.Parent := Cube1;
end;

{ TCustomCube }

constructor TCustomCube.Create(AOwner: TComponent; Index: Integer);
begin
  inherited Create(AOwner);
  Width := 5.2;
  Depth := 0.2;
// we need to fix U texture coordonates to show only one plank
  for var I := 0 to Data.VertexBuffer.Length - 1 do
  begin
    var T := Data.VertexBuffer.TexCoord0[I];
    Data.VertexBuffer.TexCoord0[I] := TPointF.Create(Abs(T.X/8 - 0.1) + Index/8, T.Y);
  end;
end;

end.
