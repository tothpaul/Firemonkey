unit Execute.ShaderMaterial;

{
  Sample Shader compiler (c)2019 Execute SARL

}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math.Vectors,
  FMX.Types3D,
  FMX.Materials,
  FMX.MaterialSources;

const
  DEFAULT_VERTEX_SHADER =
{$IFDEF MSWINDOWS}
    'matrix MVPMatrix;'+
  // input parameters
    'struct Input {'+
    '  float4 p: POSITION;'+
    '  float2 t: TEXCOORD0;'+
    '};'+
  // output parameters
    'struct Output {'+
    '  float4 p: POSITION;'+
    '  float2 t: TEXCOORD0;'+
    '};'+
  // main program
    'Output main(Input input) {'+
    '  Output output;'+
    '  output.p = mul(MVPMatrix, input.p);'+
    '  output.t = input.t;'+
    '  return output;'+
    '}';
{$ELSE}
    'uniform mat4 _MVPMatrix;'+
  // Input
    'attribute vec2 a_TexCoord0;'+
    'attribute vec4 a_Position;'+
  // Interpolation
    'varying vec2 t;'+
  // main program
    'void main() {'+
    '  t = a_TexCoord0;'+
    '  gl_Position = _MVPMatrix * a_Position;'+
    '}';
{$ENDIF}

type
  TApplyShaderEvent = procedure(Sender: TObject; Context: TContext3D) of object;

  TShaderMaterial = class(TCustomMaterial)
  private
    FVertexShaderSource: string;
    FPixelShaderSource : string;
    FVertexShaderBytes : TBytes;
    FPixelShaderBytes  : TBytes;
    FPixelShaderVars   : TContextShaderVariables;
    MVPMatrix          : TContextShaderVariable;
    FOnApply           : TApplyShaderEvent;
    procedure SetVertexShader(const Value: string);
    procedure SetPixelShader(const Value: string);
  protected
    procedure DoInitialize; override;
    procedure DoApply(const Context: TContext3D); override;
    class function DoGetMaterialProperty(const Prop: TMaterial.TProperty): string; override;
  public
    constructor Create(const AVertexShader, APixelShader: string; const APixelShaderVars: TContextShaderVariables); reintroduce;
    property OnApply: TApplyShaderEvent read FOnApply write FOnApply;
    property VertexShader: string read FVertexShaderSource write SetVertexShader;
    property PixelShader: string read FPixelShaderSource write SetPixelShader;
  end;

  TShaderMaterialSource = class(TMaterialSource)
  private
    function GetVertexShader: string;
    procedure SetVertexShader(const Value: string);
    function GetPixelShader: string;
    procedure SetPixelShader(const Value: string);
  protected
    function CreateMaterial: TMaterial; override;
  public
    property VertexShader: string read GetVertexShader write SetVertexShader;
    property PixelShader: string read GetPixelShader write SetPixelShader;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.D3DCommon,
  Winapi.D3DCompiler;

function DXCompile(const Source: string; Kind: TContextShaderKind; Arch: TContextShaderArch = TContextShaderArch.DX11): TBytes;
var
  Data   : TBytes;
  Target : AnsiString;
  Flags  : Cardinal;
  Code   : ID3DBlob;
  Err    : ID3DBlob;
  Str    : string;
begin
  Data := TEncoding.ANSI.GetBytes(Source);
  case Arch of
    TContextShaderArch.DX11_level_9: Target := '2_0'; // D3D_FEATURE_LEVEL_9_1
    TContextShaderArch.DX10        : Target := '4_0'; // D3D_FEATURE_LEVEL_10_0
    TContextShaderArch.DX11        : Target := '5_0'; // D3D_FEATURE_LEVEL_11_0
  else
    raise Exception.Create('Unsupported architecture');
  end;
  case Kind of
    TContextShaderKind.VertexShader: Target := 'vs_' + Target;
    TContextShaderKind.PixelShader : Target := 'ps_' + Target;
  end;

  Flags := D3DCOMPILE_OPTIMIZATION_LEVEL3
//    or D3DCOMPILE_ENABLE_STRICTNESS
    or D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY
    or D3DCOMPILE_WARNINGS_ARE_ERRORS;

  if D3DCompile(Data, Length(Data), nil, nil, nil, 'main', PAnsiChar(Target), Flags, 0, Code, Err) = 0 then
  begin
    SetLength(Result, Code.GetBufferSize);
    Move(Code.GetBufferPointer^, Result[0], Length(Result));
  end else begin
    SetString(Str, PAnsiChar(Err.GetBufferPointer), Err.GetBufferSize);
    raise Exception.Create('Shader compilation error :'#13 + Str);
  end;
end;
{$ENDIF}

{$IFDEF OSX}
uses
  Macapi.CocoaTypes,
  Macapi.OpenGL;
{$ENDIF}

{ TShaderMaterial }

const
  ARCH = {$IFDEF MSWINDOWS}TContextShaderArch.DX11{$ELSE}TContextShaderArch.GLSL{$ENDIF};

constructor TShaderMaterial.Create(const AVertexShader, APixelShader: string; const APixelShaderVars: TContextShaderVariables);
begin
  VertexShader := AVertexShader;
  PixelShader := APixelShader;
  FPixelShaderVars := APixelShaderVars;
  inherited Create;
end;

procedure TShaderMaterial.DoInitialize;
begin
// Why is the MaterialShader initialized from the constructor !!!!
  if (FVertexShaderBytes = nil) or (FPixelShaderBytes = nil) then
    Exit;

  inherited;

  MVPMatrix := TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 64);
  FVertexShader := TShaderManager.RegisterShaderFromData(
    '',
    TContextShaderKind.VertexShader,
    '',
    [TContextShaderSource.Create(ARCH, FVertexShaderBytes,[MVPMatrix])]
  );
  FPixelShader := TShaderManager.RegisterShaderFromData(
    '', TContextShaderKind.PixelShader, '',
    [TContextShaderSource.Create(ARCH, FPixelShaderBytes, FPixelShaderVars)]
  );
end;

procedure TShaderMaterial.SetPixelShader(const Value: string);
begin
  if Value <> FPixelShaderSource then
  begin
    FPixelShaderSource := Value;
    if FPixelShaderSource = '' then
      FPixelShaderBytes := nil
    else begin
    {$IFDEF MSWINDOWS}
      FPixelShaderBytes := DXCompile(FPixelShaderSource, TContextShaderKind.PixelShader);
    {$ELSE}
      FPixelShaderBytes := TEncoding.ANSI.GetBytes(FPixelShaderSource);
    {$ENDIF}
      DoInitialize;
    end;
  end;
end;

procedure TShaderMaterial.SetVertexShader(const Value: string);
begin
  if Value <> FVertexShaderSource then
  begin
    FVertexShaderSource := Value;
    if FVertexShaderSource = '' then
      FVertexShaderBytes := nil
    else begin
    {$IFDEF MSWINDOWS}
      FVertexShaderBytes := DXCompile(FVertexShaderSource, TContextShaderKind.VertexShader);
    {$ELSE}
      FVertexShaderBytes := TEncoding.ANSI.GetBytes(FVertexShaderSource);
    {$ENDIF}
      DoInitialize;
    end;
  end;
end;

procedure TShaderMaterial.DoApply(const Context: TContext3D);
{$IFNDEF MSWINDOWS}
var
  M: TMatrix3D;
{$ENDIF}
begin
  inherited;
{$IFNDEF MSWINDOWS}
// https://quality.embarcadero.com/browse/RSP-16323
  M := Context.CurrentModelViewProjectionMatrix;
  glUniformMatrix4fv(MVPMatrix.Index, 1, GL_FALSE, @M.m11);
{$ENDIF}
  if Assigned(FOnApply) then
    FOnApply(Self, Context);
end;

class function TShaderMaterial.DoGetMaterialProperty(
  const Prop: TMaterial.TProperty): string;
begin
{$IFDEF MSWINDOWS}
// https://quality.embarcadero.com/browse/RSP-16323
  if Prop = TProperty.ModelViewProjection then
  begin
    Exit('MVPMatrix');
  end;
{$ENDIF}
  Result := '';
end;

{ TShaderMaterialSource }

function TShaderMaterialSource.CreateMaterial: TMaterial;
begin
  Result := TShaderMaterial.Create('', '', []);
end;

function TShaderMaterialSource.GetPixelShader: string;
begin
  Result := TShaderMaterial(Material).PixelShader;
end;

function TShaderMaterialSource.GetVertexShader: string;
begin
  Result := TShaderMaterial(Material).VertexShader;
end;

procedure TShaderMaterialSource.SetPixelShader(const Value: string);
begin
  TShaderMaterial(Material).PixelShader := Value;
end;

procedure TShaderMaterialSource.SetVertexShader(const Value: string);
begin
  TShaderMaterial(Material).VertexShader := Value;
end;

end.
