unit Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Math.Vectors,
  FMX.Types,
  FMX.Controls,
  FMX.Forms3D,
  FMX.Forms,
  FMX.Ani,
  FMX.Controls3D,
  FMX.Objects3D,
  Execute.ShaderMaterial;

type
  TForm1 = class(TForm3D)
    Timer1: TTimer;
    Plane1: TPlane;
    FloatAnimation1: TFloatAnimation;
    procedure Timer1Timer(Sender: TObject);
    procedure Form3DCreate(Sender: TObject);
  private
    { Déclarations privées }
    FShaderSource: TShaderMaterialSource;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

const
{$IFDEF MSWINDOWS}

  PIXEL_SHADER =

    ' float4 main() : COLOR {'
  + '  return float4(1.0, 0.5, 0.5, 1.0);'
  + '}';

{$ELSE}

  PIXEL_SHADER =

    'void main() {'+
    '  gl_FragColor = vec4(1.0, 0.5, 0.5, 1.0);'+
    '}';

{$ENDIF}


procedure TForm1.Form3DCreate(Sender: TObject);
begin
  FShaderSource := TShaderMaterialSource.Create(Self);
  FShaderSource.VertexShader := DEFAULT_VERTEX_SHADER;
  FShaderSource.PixelShader := PIXEL_SHADER;
  Plane1.MaterialSource := FShaderSource;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Invalidate;
end;

end.
