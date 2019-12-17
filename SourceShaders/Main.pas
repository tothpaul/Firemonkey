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
  FMX.Dialogs,
  Execute.ShaderMaterial;

type
  TForm1 = class(TForm3D)
    Timer1: TTimer;
    Plane1: TPlane;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    FloatAnimation3: TFloatAnimation;
    Plane2: TPlane;
    Plane3: TPlane;
    Plane4: TPlane;
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

  VERTEX_SHADER =
    'matrix MVPMatrix;'+
  // input parameters
    'struct Input {'+
    '  float4 p: POSITION;'+
    '  float2 t: TEXCOORD;'+
    '};'+
  // output parameters
    'struct Output {'+
    '  float4 p: POSITION;'+
    '  float2 t: TEXCOORD;'+
    '};'+
  // main program
    'Output main(Input input) {'+
    '  Output output;'+
    '  output.p = mul(MVPMatrix, input.p);'+
    '  output.t = input.t;'+
    '  return output;'+
    '}';

// https://digitalerr0r.net/2010/12/13/fundamentals-of-fractals-5-the-mandelbrot-set/
  PIXEL_SHADER =

    'struct Input {'
  + ' float4 p: POSITION;'
  + ' float2 t: TEXTCOORD;'
  + '};'

  + 'float4 main(Input input): COLOR {'
  + ' float2 C = (input.t - 0.5)*4;'
  + ' float2 v = C;'
  + ' int Iteration = 29;'
  + ' float3 Color = input.p.xyz;'
  + ' int prevIteration = Iteration;'
  + ' int i = 0;'
  + ' do {'
  + '  v = float2((v.x*v.x)-(v.y*v.y),v.x*v.y*2)+C;'
  + '  i++;'
  + '  if ((prevIteration == Iteration) && ((v.x*v.x)+(v.y*v.y))>4.0) {'
  + '    prevIteration = i + 1;'
  + '  }'
  + ' } while(i<prevIteration);'
  + ' float NIC = (float(i) - (log(log(sqrt((v.x*v.x)+(v.y*v.y))))/log(2.0)))/float(Iteration);'
  + ' return float4(sin(NIC*Color.x),sin(NIC*Color.y),sin(NIC*Color.z),1);'
  + '}';

{$ELSE}

  VERTEX_SHADER =

    'uniform mat4 MVPMatrix;'+
  // Input
    'attribute vec2 a_TexCoord0;'+
    'attribute vec4 a_Position;'+
  // Interpolation
    'varying vec2 TEX0;'+
  // main program
    'void main() {'+
    '  TEX0 = a_TexCoord0;'+
    '  gl_Position = MVPMatrix * a_Position;'+
    '}';

  PIXEL_SHADER =

  // input
    'varying vec2 TEX0;'
  // main
  + 'void main() {'
  + ' vec2 C = (TEX0.xy - 0.5) * 4.0;'
  + ' vec2 v = C;'
  + ' int Iteration = 29;'
  + ' vec3 Color = gl_FragCoord.xyz;'
  + ' int prevIteration = Iteration;'
  + ' int i = 0;'
  + ' do {'
  + '  v = vec2((v.x*v.x)-(v.y*v.y),v.x*v.y*2.0)+C;'
  + '  i++;'
  + '  if ((prevIteration == Iteration) && ((v.x*v.x)+(v.y*v.y))>4.0) {'
  + '    prevIteration = i + 1;'
  + '  }'
  + ' } while(i<prevIteration);'
  + ' float NIC = (float(i) - (log(log(sqrt((v.x*v.x)+(v.y*v.y))))/log(2.0)))/float(Iteration);'
  + ' gl_FragColor = vec4(sin(NIC*Color.x),sin(NIC*Color.y),sin(NIC*Color.z),1);'
  + '}';

{$ENDIF}


procedure TForm1.Form3DCreate(Sender: TObject);
begin
  try
    FShaderSource := TShaderMaterialSource.Create(Self);
    FShaderSource.VertexShader := VERTEX_SHADER;
    FShaderSource.PixelShader := PIXEL_SHADER;
    Plane1.MaterialSource := FShaderSource;
    Plane2.MaterialSource := FShaderSource;
    Plane3.MaterialSource := FShaderSource;
    Plane4.MaterialSource := FShaderSource;
  except
    on e: Exception do
      ShowMessage(e.Message);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Invalidate;
end;

end.
