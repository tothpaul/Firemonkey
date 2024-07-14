unit Wood.FlashLike;

{
  Delphi implementation of some of Flash (AS3) function (c) 2024 Execute SARL

}

interface
{$POINTERMATH ON}
uses
  System.Types,
  System.UITypes,
  System.Math,
  FMX.Types,
  FMX.Surfaces,
  NathaanTFM.AS3Perlin;

type
  Number = Integer;

  BitmapData = class;

  BitmapFilter = interface
    procedure Filter(source: BitmapData; x, y: Integer; var pixel: Cardinal);
  end;

  BitmapData = class(TBitmapSurface)
    constructor Create(w, h: Integer);
    procedure perlinNoise(baseX, baseY: Double; numOctaves, randomSeed: Number; stitch, fractalNoise: Boolean; channelOptions: Number = 0; grayScale: Boolean = False; offsets: Pointer = nil);
    function Clone: BitmapData;
    function rectangle: TRect;
    procedure applyFilter(sourceBitmapData: BitmapData; const sourceRec: TRect; const destPoint: TPoint; filter: BitmapFilter);
    procedure fillRect(const R: TRect; color: Cardinal);
    procedure merge(sourceBitmapData: BitmapData; const sourceRect: TRect; const destPoint: TPoint; redMultiplier, greenMultiplier, blueMultiplier, alphaMultiplier: Integer);
  end;

  TDisplacementMapFilterMode = (clamp, color, ignore, wrap);

  DisplacementMapFilter = class(TInterfacedObject, BitmapFilter)
  protected
    mapBitmap: BitmapData;
    mapPoint: TPoint;
    componentX, componentY: Integer;
    scaleX, scaleY: Double;
    mode: TDisplacementMapFilterMode;
    color: Cardinal;
    alpha: Double;
    procedure Filter(source: BitmapData; x, y: Integer; var pixel: Cardinal);
    function getColor(x, y: Integer; source: BitmapData): Cardinal;
  public
    constructor Create(mapBitmap: BitmapData; mapPoint: TPoint; componentX, componentY: Integer; scaleX, scaleY: Double; mode: string = 'wrap'; color: Cardinal = 0; alpha: Double = 0);
  end;

  ColorMatrixFilter = class(TInterfacedObject, BitmapFilter)
  private
    Matrix: TArray<Single>;
    procedure Filter(source: BitmapData; x, y: Integer; var pixel: Cardinal);
  public
    constructor Create(const Matrix: TArray<Single>);
  end;

  ConvolutionFilter = class(TInterfacedObject, BitmapFilter)
  private
    MatrixX: Integer;
    MatrixY: Integer;
    Matrix: TArray<Single>;
    Bias: Integer;
    procedure Filter(source: BitmapData; x, y: Integer; var pixel: Cardinal);
  public
    constructor Create(MatrixX, MatrixY: Integer; const Matrix: TArray<Single>; Bias: Integer);
  end;

type
  Rectangle = class
    class function Create(l, t, w, h: Integer): TRect;
  end;

implementation

function clampByte(color: Single): Byte;
begin
  Result := Round(Min(Max(0, color), 255));
end;

{ Rectangle }

class function Rectangle.Create(l: Integer; t: Integer; w: Integer; h: Integer): TRect;
begin
  Result.Left := l;
  Result.Top := t;
  Result.Width := w - 1;
  Result.Height := h - 1;
end;

{ BitmapData }

procedure BitmapData.applyFilter(sourceBitmapData: BitmapData;
  const sourceRec: TRect; const destPoint: TPoint; filter: BitmapFilter);
begin
  var source := sourceBitmapData;
  if source = Self then
    source := Clone();
  var x1 := Min(Max(0, destPoint.X), Width);
  var X2 := Min(Max(0, destPoint.X + sourceRec.Width), Width);
  var y1 := Min(Max(0, destPoint.Y), Height);
  var y2 := Min(Max(0, destPoint.Y + sourcerec.Height), Height);
  var dx := x2 - x1 - 1;
  var dy := y2 - y1 - 1;

  for var y := 0 to dy do
  begin
    var p: PCardinal := ScanLine[y1 + y];
    for var x := 0 to dx do
    begin
      filter.Filter(source, x + sourceRec.Left, y + sourceRec.Top, p[x1 + x]);
    end;
  end;
  if source <> sourceBitmapData then
    source.Free;
end;

function BitmapData.Clone: BitmapData;
begin
  Result := BitmapData.Create(Width, Height);
  Result.Assign(Self);
end;

constructor BitmapData.Create(w, h: Integer);
begin
  inherited Create;
  SetSize(w, h, TPixelFormat.BGRA);
  FillRect(TRect.Create(0, 0, w, h), $ff000000);
end;

procedure BitmapData.fillRect(const R: TRect; color: Cardinal);
begin
  var x1 := Min(Max(0, R.Left), Width - 1);
  var x2 := Min(Max(0, R.Right), Width - 1);
  var y1 := Min(Max(0, R.Top), Height - 1);
  var y2 := Min(Max(0, R.Bottom), Height - 1);
  for var y := y1 to y2 do
  begin
    var p: PCardinal := ScanLine[y];
    for var x := x1 to x2 do
    begin
      p[x] := color;
    end;
  end;

end;

procedure BitmapData.merge(sourceBitmapData: BitmapData;
  const sourceRect: TRect; const destPoint: TPoint; redMultiplier,
  greenMultiplier, blueMultiplier, alphaMultiplier: Integer);
begin
  var x1 := Min(Max(0, destPoint.X), Width - 1);
  var x2 := Min(Max(0, destPoint.X + sourceRect.Width), Width - 1);
  var y1 := Min(Max(0, destPoint.Y), Height - 1);
  var y2 := Min(Max(0, destPoint.Y + sourceRect.Height), Height - 1);
  var dx := x2 - x1 - 1;
  var dy := y2 - y1 - 1;
  for var y := 0 to dy do
  begin
    var sy := sourceRect.Top + y;
    if sy < 0 then
      Continue;
    if sy > sourceBitmapData.Height then
      Exit;
    var P: PColorRec := ScanLine[y1 + y];
    var S: PColorRec := sourceBitmapData.ScanLine[sy];
    for var x := 0  to dx do
    begin
      var sx := sourceRect.Left + x;
      if sx < 0 then
        Continue;
      if sx > sourceBitmapData.Width then
        break;
      var PC: PColorRec := @P[x1 + x];
      var PS: PColorRec := @S[sx];
      PC.B := ClampByte(((PS.B * blueMultiplier) + (PC.B * (256 - blueMultiplier))) / 256);
      PC.G := ClampByte(((PS.G * greenMultiplier) + (PC.G * (256 - greenMultiplier))) / 256);
      PC.R := ClampByte(((PS.R * redMultiplier) + (PC.R * (256 - redMultiplier))) / 256);
      PC.A := ClampByte(((PS.A * alphaMultiplier) + (PC.A * (256 - alphaMultiplier))) / 256);
    end;
  end;
end;

procedure BitmapData.perlinNoise(baseX, baseY: Double; numOctaves, randomSeed: Number; stitch, fractalNoise: Boolean; channelOptions: Number = 0; grayScale: Boolean = False; offsets: Pointer = nil);
var
  v: Cardinal;
begin
  var perlin := TAS3PerlinNoise.Create(Width, Height, baseX, baseY, numOctaves, randomSeed, stitch, fractalNoise, channelOptions, grayScale, offsets);
  for var y := 0 to Height - 1 do
  begin
    var p: PCardinal := ScanLine[y];
    for var x := 0 to Width - 1 do
    begin
      p[x]:= perlin.generatePerlinNoise(x, y);
    end;
  end;

end;

function BitmapData.rectangle: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Width := Width;
  Result.Height := Height;
end;

{ DisplacementMapFilter }

function getComponent(component: Integer; color: Cardinal): Integer;
begin
  case component of
    1: // RED
      Result := color and $FF;
    2: // GREEN
      Result := (color shr 8) and $FF;
    4: // BLUE
      Result := (color shr 16) and $FF;
    8: // ALPHA
      Result := (color shr 24);
  end;
end;

procedure WrapValue(var x: Integer; max: Integer);
begin
  while x < 0 do Inc(x, max);
  while x >= max do Dec(x, max);
end;

constructor DisplacementMapFilter.Create(mapBitmap: BitmapData;
  mapPoint: TPoint; componentX, componentY: Integer; scaleX, scaleY: Double;
  mode: string; color: Cardinal; alpha: Double);
begin
  Self.mapBitmap := mapBitmap;
  Self.mapPoint := mapPoint;
  Self.componentX := componentX;
  Self.componentY := componentY;
  Self.scaleX := ScaleX;
  Self.scaleY := scaleY;
  if mode = 'clamp' then
    Self.mode := TDisplacementMapFilterMode.clamp
  else
  if mode = 'color' then
    Self.mode := TDisplacementMapFilterMode.color
  else
  if mode = 'ignore' then
    Self.mode := TDisplacementMapFilterMode.ignore
  else
    Self.mode := TDisplacementMapFilterMode.wrap;
  Self.color := color;
  Self.alpha := alpha;
end;

procedure DisplacementMapFilter.Filter(source: BitmapData; x, y: Integer; var pixel: Cardinal);
begin
// dstPixel[x, y] = srcPixel[x + ((componentX(x, y) - 128) * scaleX) / 256, y + ((componentY(x, y) - 128) *scaleY) / 256)
  if (x < 0)
  or (y < 0)
  or (x >= mapBitmap.Width)
  or (y >= mapBitmap.Height) then
  begin
    Exit;
  end;

  var l: PCardinal := mapBitmap.ScanLine[y];
  var c := l[x];

{$IFDEF NO_SMOOTH}
  var sx: Integer := x + Round(((getComponent(componentX, c) - 128) * scaleX) / 256);
  var sy: Integer := y + Round(((getComponent(componentY, c) - 128) * scaleY) / 256);

  Pixel := getColor(sx, sy, source);
{$ELSE}
  // I've tried to implement a smoothing but the result is poor

  var sx: Double := x + ((getComponent(componentX, c) - 128) * scaleX) / 256;
  var sy: Double := y + ((getComponent(componentY, c) - 128) * scaleY) / 256;

  var tx: Integer := Round(sx);
  var ty: Integer := Round(sy);

  var fx := sx - tx;
  var fy := sy - ty;

  var c1 := getColor(tx, ty, source);
  var c2 := getColor(tx + 1, ty, source);
  var c3 := getColor(tx, ty + 1, source);
  var c4 := getColor(tx + 1, ty + 1, source);

  var f2 := 1 - fx;

  var r1 := TColorRec(c1).R * f2 + TColorRec(c2).R * fx;
  var g1 := TColorRec(c1).G * f2 + TColorRec(c2).G * fx;
  var b1 := TColorRec(c1).B * f2 + TColorRec(c2).B * fx;
  var a1 := TColorRec(c1).A * f2 + TColorRec(c2).A * fx;

  var r2 := TColorRec(c3).R * f2 + TColorRec(c4).R * fx;
  var g2 := TColorRec(c3).G * f2 + TColorRec(c4).G * fx;
  var b2 := TColorRec(c3).B * f2 + TColorRec(c4).B * fx;
  var a2 := TColorRec(c3).A * f2 + TColorRec(c4).A * fx;

  f2 := 1 - fy;

  with TColorRec(pixel) do
  begin
    R := ClampByte(r1 * f2 + r2 * fy);
    G := ClampByte(g1 * f2 + g2 * fy);
    B := ClampByte(b1 * f2 + b2 * fy);
    A := ClampByte(a1 * f2 + a2 * fy);
  end;
{$ENDIF}
end;

function DisplacementMapFilter.getColor(x, y: Integer; source: BitmapData): Cardinal;
begin
  var cx := Min(Max(0, x), source.Width - 1);
  var cy := Min(Max(0, y), source.Height - 1);
  var clamped := (cx <> x) or (cy <> y);
  if clamped then
  begin
    case Mode of
      TDisplacementMapFilterMode.clamp:
      begin
        x := cx;
        y := cy;
      end;
      TDisplacementMapFilterMode.color:
      begin
        Result := Color;
        Exit;
      end;
      TDisplacementMapFilterMode.ignore:
      begin
        Result := 0; // ?
        Exit;
      end;
      TDisplacementMapFilterMode.wrap:
      begin
        WrapValue(x, source.Width);
        WrapValue(y, source.Height);
      end;
    end;
  end;
  var L: PCardinal := source.ScanLine[y];
  Result := L[x];
end;

{ ColorMatrixFilter }

constructor ColorMatrixFilter.Create(const Matrix: TArray<Single>);
begin
  Self.Matrix := Matrix;
end;

procedure ColorMatrixFilter.Filter(source: BitmapData; x, y: Integer;
  var pixel: Cardinal);
begin
  if (x < 0)
  or (y < 0)
  or (x >= source.Width)
  or (y >= source.Height) then
  begin
    Exit;
  end;
  var l: PColorRec := source.ScanLine[y];
  var c: TColorRec := l[x];
  with TColorRec(pixel) do
  begin
    B := clampByte(c.R * Matrix[ 0] + c.G * Matrix[ 1] + c.B * Matrix[ 2] + c.A * Matrix[ 3] + Matrix[ 4]);
    G := clampByte(c.R * Matrix[ 5] + c.G * Matrix[ 6] + c.B * Matrix[ 7] + c.A * Matrix[ 8] + Matrix[ 9]);
    R := clampByte(c.R * Matrix[10] + c.G * Matrix[11] + c.B * Matrix[12] + c.A * Matrix[13] + Matrix[14]);
    A := clampByte(c.R * Matrix[15] + c.G * Matrix[16] + c.B * Matrix[17] + c.A * Matrix[18] + Matrix[19]);
  end;
end;

{ ConvolutionFilter }

constructor ConvolutionFilter.Create(MatrixX, MatrixY: Integer; const Matrix: TArray<Single>; Bias: Integer);
begin
  Self.MatrixX := MatrixX;
  Self.MatrixY := MatrixY;
  Self.Matrix := Matrix;
  Self.Bias := Bias;
end;

procedure ConvolutionFilter.Filter(source: BitmapData; x, y: Integer;
  var pixel: Cardinal);
var
  ar, ag, ab, aa: Single;
  sum: Single;
begin
  ar := 0;
  ag := 0;
  ab := 0;
  aa := 0;
  sum := 0;
  var oy := y - MatrixY div 2;
  var m := 0;
  for var my := 0 to MatrixY - 1 do
  begin
    if oy >= source.Height then
      Break;
    if oy >= 0 then
    begin
      var P: PColorRec := source.ScanLine[oy];
      var ox := x - MatrixX div 2;
      for var mx := 0 to MatrixX - 1 do
      begin
        if ox >= source.Width then
          Break;
        var f := Matrix[m];
        if ox >= 0 then
        begin
          ar := ar + p[ox].R * f;
          ag := ag + p[ox].G * f;
          ab := ab + p[ox].B * f;
          aa := aa + p[ox].A * f;
          sum := sum + f;
        end;
        Inc(ox);
        Inc(m);
      end;
    end else begin
      Inc(m, MatrixX);
    end;
    Inc(oy);
  end;
  if sum = 0 then
    Exit;
  with TColorRec(pixel) do
  begin
    var f := 1/sum;
    R := ClampByte(ar * f + bias);   // bias ?!
    G := ClampByte(ag * f + bias);
    B := ClampByte(ab * f + bias);
    A := ClampByte(aa * f + bias);
  end;
end;

end.
