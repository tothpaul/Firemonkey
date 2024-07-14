unit NathaanTFM.AS3Perlin;

// Delphi translation of https://github.com/NathaanTFM/as3-perlin
// (c)2024 Execute SARL

interface
{$POINTERMATH ON}
{.$DEFINE OFFSETS}
{$Q-}
uses
  System.Math;

type
  TPerlinVector2 = record
    x: Double;
    y: Double;
  end;
  PPerlinVector2 = ^TPerlinVector2;

  TAS3PerlinNoise = record
  private
    baseX: Double;
    baseY: Double;

    numOctaves: Cardinal;

    permutations: array[0..255] of Byte;
    vectors: array[0..3, 0..255] of TPerlinVector2;

    stitch: Boolean;
    stitchArr: array[0..1] of Cardinal;

    fractalNoise: Boolean;

    channelOptions: Byte;
    channelCount: Byte;
    grayScale: Boolean;

  {$IFDEF OFFSETS}
    offsets: TArray<TPerlinVector2>;
  {$ENDIF}
    procedure generateRandom(randomSeed: Integer);
  public
    constructor Create(Width, height: Integer; baseX, baseY: Double; numOctaves, randomSeed: Integer; stitch, fractalNoise: Boolean; channelOptions: Cardinal = 0; grayScale: Boolean = False; offsets: PPerlinVector2 = nil);
    function generatePerlinNoise(x, y: Integer): Cardinal;
  end;

implementation

const
  Epsilon = 0.001;

function getNextRandomSeed(randomSeed: Integer): Integer;
begin
  Result := -2836 * (randomSeed div 127773) + 16807 * (randomSeed mod 127773);
  if Result <= 0 then
    Inc(Result, $7FFFFFFF); // what if it's equals to -0x80000000??
end;

function interpolate(a0, a1, w: Double): Double; inline;
begin
  Result := (a1 - a0) * (3.0 - w * 2.0) * w * w + a0;
end;

function unmultiplyColor(red, green, blue, alpha: Byte): Cardinal;
begin
  if alpha <> 255 then
  begin
    var val: Cardinal := 0;
    if alpha <> 0 then
      val := $FF00 div alpha;
    red := (val * red + $7F) shr 8;
    green := (val * green + $7F) shr 8;
    blue := (val * blue + $7F) shr 8;
  end;
  Result := (alpha shl 24) or (red shl 16) or (green shl 8) or blue;
end;

{ TAS3PerlinNoise }

constructor TAS3PerlinNoise.Create(Width, height: Integer; baseX, baseY: Double; numOctaves,
  randomSeed: Integer; stitch, fractalNoise: Boolean; channelOptions: Cardinal;
  grayScale: Boolean; offsets: PPerlinVector2);
begin

  if Abs(baseX) > Epsilon then
    baseX := 1/Abs(baseX);

  if Abs(baseY) > Epsilon then
    baseY := 1/Abs(baseY);

  Self.numOctaves := numOctaves;

  generateRandom(randomSeed);

  Self.stitch := stitch;

  if stitch then
  begin
    if Abs(baseX) > Epsilon then
    begin
      var tmp1 := Floor(baseX * width) / width;
      var tmp2 := Ceil(baseX * width) / width;

      if (Abs(tmp1) < Epsilon) or (baseX / tmp1 >= tmp2 / baseX) then
        baseX := tmp2
      else
        baseX := tmp1;
    end;

    if Abs(baseY) > Epsilon then
    begin
      var tmp1 := Floor(baseY * height) / height;
      var tmp2 := Ceil(baseY * height) / height;

      if (Abs(tmp1) < Epsilon) or (baseX / tmp1 >= tmp2 / baseX) then
        baseY := tmp2
      else
        baseY := tmp1;
    end;

    stitchArr[0] := Round(Width * baseX);
    stitchArr[1] := Round(height * baseY);
  end;

  Self.baseX := baseX;
  Self.baseY := baseY;
  Self.fractalNoise := fractalNoise;

  Self.channelOptions := channelOptions;
  Self.grayScale := grayScale;

  if grayScale then
    channelCount := 1
  else
    channelCount := Ord(channelOptions and 1 <> 0) + Ord(channelOptions and 2 <> 0) + Ord(channelOptions and 4 <> 0);

  Inc(channelCount, Ord(channelOptions and 8 <> 0));

{$IFDEF OFFSETS}
  SetLength(Self.offsets, numOctaves);

  if offsets <> nil then
    Move(offsets^, Self.offsets[0], numOctaves * SizeOf(TPerlinVector2));
{$ENDIF}
end;

procedure TAS3PerlinNoise.generateRandom(randomSeed: Integer);
begin
  if randomSeed <= 0 then
    randomSeed := Abs(randomSeed - 1)
  else
  if randomSeed = $7FFFFFFF then
    Dec(randomSeed);

  for var i := 0 to 3 do
  begin
    for var j := 0 to 255 do
    begin
      var vector: PPerlinVector2 := @vectors[i, j];

      randomSeed := getNextRandomSeed(randomSeed);
      vector.x := (randomSeed mod 512 - 256) / 256.0;

      randomSeed := getNextRandomSeed(randomSeed);
      vector.y := (randomSeed mod 512 - 256) / 256.0;

      var dist := sqrt(Power(vector.x, 2) + Power(vector.y, 2));

      if dist < Epsilon then
      begin
        vector.x := 0;
        vector.y := 0;
      end else begin
        vector.x := vector.x / dist;
        vector.y := vector.y / dist;
      end;
    end;
  end;

  for var i := 0 to 255 do
    permutations[i] := i;

  for var i := 255 downto 1 do
  begin
    randomSeed := getNextRandomSeed(randomSeed);

    var j := randomSeed and $FF;
    var temp := permutations[j];
    permutations[j] := permutations[i];
    permutations[i] := temp;
  end;
end;


function TAS3PerlinNoise.generatePerlinNoise(x, y: Integer): Cardinal;
var
  red, green, blue, alpha: Cardinal;
  channelAlpha: Double;
  channels: array[0..3] of Double;
begin
  red := 0; green := 0; blue := 0; alpha := 255;

  var baseX := Self.baseX;
  var baseY := Self.baseY;

  channelAlpha := 255;

  FillChar(channels, SizeOf(channels), 0);

  for var octave := 0 to numOctaves - 1 do
  begin
    var offsetX := ({$IFDEF OFFSETS}offsets[octave].x +{$ENDIF} x) * baseX + 4096.0;
    var offsetY := ({$IFDEF OFFSETS}offsets[octave].y +{$ENDIF} y) * baseY + 4096.0;

    var x0 := Floor(offsetX);
    var x1 := x0 + 1;

    var y0 := Floor(offsetY);
    var y1 := y0 + 1;

    var dx0 := offsetX - x0; // Floor(offsetX);
    var dx1 := dx0 - 1.0;

    var dy0 := offsetY - y0; // Floor(offsetY);
    var dy1 := dy0 - 1.0;

    if stitch then
    begin
      var tmp1 := stitchArr[0] + 4096;
      var tmp2 := stitchArr[1] + 4096;

      if x0 >= tmp1 then
        x0 := x0 - stitchArr[0];

      if x1 >= tmp1 then
        x1 := x1 - stitchArr[0];

      if y0 >= tmp2 then
        y0 := y0 - stitchArr[1];

      if y1 >= tmp2 then
        y1 := y1 - stitchArr[1];
    end;

    var idx1 := permutations[x0 and 255];
    var idx2 := permutations[x1 and 255];

    var v1 := permutations[(y0 + idx1) and 255];
    var v2 := permutations[(y0 + idx2) and 255];
    var v3 := permutations[(y1 + idx1) and 255];
    var v4 := permutations[(y1 + idx2) and 255];

    for var channel := 0 to channelCount - 1 do
    begin
      var n0, n1: Double;
      var vectorArray: PPerlinVector2 := @vectors[channel];

      n0 := vectorArray[v1].x * dx0 + vectorArray[v1].y * dy0;
      n1 := vectorArray[v2].x * dx1 + vectorArray[v2].y * dy0;
      var ix1 := interpolate(n0, n1, dx0);

      n0 := vectorArray[v3].x * dx0 + vectorArray[v3].y * dy1;
      n1 := vectorArray[v4].x * dx1 + vectorArray[v4].y * dy1;
      var ix2 := interpolate(n0, n1, dx0);

      var value := interpolate(ix1, ix2, dy0);
      if fractalNoise then
        channels[channel] := channels[channel] + value * channelAlpha
      else
        channels[channel] := channels[channel] + Abs(value) * channelAlpha
    end;

    channelAlpha := channelAlpha * 0.5;
    baseX := baseX * 2.0;
    baseY := baseY * 2.0;

    if stitch then
    begin
      stitchArr[0] := stitchArr[0] * 2;
      stitchArr[1] := stitchArr[1] * 2;
    end;
  end;

  var nextChannel := 0;
  if fractalNoise then
  begin
    if grayScale then
    begin
      red := Cardinal(Round(channels[nextChannel] + 255.0) shr 1); Inc(nextChannel);
      green := red;
      blue := red;
    end else begin
      if channelOptions and 1 <> 0 then begin red := Cardinal(round(channels[nextChannel] + 255.0) shr 1); Inc(nextChannel); end;
      if channelOptions and 2 <> 0 then begin green := Cardinal(round(channels[nextChannel] + 255.0) shr 1); Inc(nextChannel); end;
      if channelOptions and 4 <> 0 then begin blue := Cardinal(round(channels[nextChannel] + 255.0) shr 1); Inc(nextChannel); end;
    end;
    if channelOptions and 8 <> 0 then alpha := round(channels[nextChannel] + 255.0) shr 1;
  end else begin
    if grayScale then
    begin
      red := round(channels[nextChannel]); Inc(nextChannel);
      green := red;
      blue := red;
    end else begin
      if channelOptions and 1 <> 0 then begin red := round(channels[nextChannel]); Inc(nextChannel); end;
      if channelOptions and 2 <> 0 then begin green := round(channels[nextChannel]); Inc(nextChannel); end;
      if channelOptions and 4 <> 0 then begin blue := round(channels[nextChannel]); Inc(nextChannel); end;
    end;
    if channelOptions and 8 <> 0 then alpha := round(channels[nextChannel]);
  end;

  alpha := Min(Max(alpha, 0), 255);
  red := Min(Max(red, 0), alpha);
  green := Min(Max(green, 0), alpha);
  blue := Min(Max(blue, 0), alpha);

  Result := unmultiplyColor(red, green, blue, alpha);
end;

end.
