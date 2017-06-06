unit Execute.FontBuilder;

{
  Create a vectorial text in a FMX TPath (c)2017 Execute SARL
  http://www.execute.fr


  NB: this font builder works only under Windows, but the generated Path can be used on any plateform
}

interface

uses
  System.Classes,
  System.UITypes,
  Fmx.Graphics;

procedure GetFontList(List: TStrings);
procedure BuildText(const Text, FontName: string; Size: Integer; Style: TFontStyle; Path: TPathData);

implementation

uses
  System.SysUtils,
  System.Types,
  Winapi.Windows;

function FixToX(const AFix: TFixed): Double;
begin
  Result := (AFix.fract/65536.0 + AFix.value);
end;

function FixToY(const AFix: TFixed): Double;
begin
  Result := - (AFix.fract/65536.0 + AFix.value);
end;

procedure BuildGlyph(DC: HDC; var x, y: Single; Ch: Char; Path: TPathData; const TextMetric: TTextMetric);
var
  Matrix : TMAT2;
  Size   : Cardinal;
  Metrics: TGLYPHMETRICS;
  Buffer : TBytes;
  Index  : Integer;
  Header : PTTPolygonHeader;
  p      : TPointF;
  Len    : Integer;
  Curve  : PTTPolyCurve;
  Point  : PPointfx;
  Count  : Integer;
  c      : TPointF;
begin
  if Ch = ' ' then
  begin
    GetCharWidth(DC, 32 , 32, Len);
    x := x + Len;
    Exit;
  end;

  FillChar(Matrix, SizeOf(Matrix), 0);
  Matrix.eM11.Value := 1;
  Matrix.eM22.Value := 1;

  Size := GetGlyphOutline(DC, Ord(Ch), GGO_NATIVE, Metrics, 0, nil, Matrix);
  if (Size = 0) or (Size = GDI_ERROR) then
    Exit;

  SetLength(Buffer, Size);
  Size := GetGlyphOutline(DC, Ord(Ch), GGO_NATIVE, Metrics, Size, Buffer, Matrix);
  if (Size = 0) or (Size = GDI_ERROR) then
    Exit;

  Index := 0;
  while Index < Size do
  begin
    Header := @Buffer[Index];
    if Header.dwType <> TT_POLYGON_TYPE then
      Exit;
    p.x := x + FixToX(Header.pfxStart.x);
    p.y := y + FixToY(Header.pfxStart.y);
    if Index > 0 then
      Path.ClosePath;
    Path.MoveTo(p);
    Len := Index + Header.cb;
    Inc(Index, SizeOf(TTPOLYGONHEADER));
    while Index < Len do
    begin
      Curve := @Buffer[Index];
      Point := @Curve.apfx;
      case Curve.wType of
        TT_PRIM_LINE:
          for Count := 1 to Curve.cpfx do
          begin
            p.X := x + FixToX(Point.x);
            p.Y := y + FixToY(Point.y);
            Inc(Point);
            Path.LineTo(P);
          end;
        TT_PRIM_QSPLINE:
          for Count := 1 to Curve.cpfx - 1 do
          begin
            p.X := x + FixToX(Point.x);
            p.Y := y + FixToY(Point.y);
            Inc(Point);
            c.X := x + FixToX(Point.x);
            c.Y := y + FixToY(Point.y);
            if Count < Curve.cpfx - 1 then
            begin
              c.x := (p.X + c.X) / 2;
              c.y := (p.Y + c.Y) / 2;
            end;
            Path.QuadCurveTo(p, c);
          end;
      else
        Exit;
      end;
      Inc(Index, SizeOf(TTPOLYCURVE) + Pred(Curve.cpfx) * SizeOf(TPOINTFX));
    end;
  end;

  x := x + Metrics.gmCellIncX;
  y := y + Metrics.gmCellIncY;

  Path.ClosePath;
end;

procedure BuildText(const Text, FontName: string; Size: Integer; Style: TFontStyle; Path: TPathData);
var
  LogFont   : TLogFont;
  Font      : HFont;
  DC        : HDC;
  OldFont   : HFont;
  TextMetric: TTextMetric;
  Index     : Integer;
  x, y      : Single;
begin
  Path.Clear;
  FillChar(LogFont, SizeOf(LogFont), 0);
  LogFont.lfHeight := -Size;
  LogFont.lfCharSet := DEFAULT_CHARSET;
  if TFontStyle.fsBold in Style then
    LogFont.lfWeight := FW_BOLD
  else
    LogFont.lfWeight := FW_NORMAL;
  LogFont.lfItalic := Byte(TFontStyle.fsItalic in Style);
  StrPLCopy(LogFont.lfFaceName, UTF8ToString(FontName), Length(LogFont.lfFaceName) - 1);
  Font := CreateFontIndirect(LogFont);
  DC := CreateCompatibleDC(GetDC(0));
  try
    OldFont := SelectObject(DC, Font);
    GetTextMetrics(DC, TextMetric);
    x := 0;
    y := 0;
    for Index := 1 to Length(Text) do
    begin
      BuildGlyph(DC, x, y, Text[Index], Path, TextMetric);
    end;
    SelectObject(DC, OldFont);

    DeleteObject(Font);
  finally
    DeleteDC(DC);
  end;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  if FontType and TRUETYPE_FONTTYPE <> 0 then
  begin
    S := TStrings(Data);
    Temp := LogFont.lfFaceName;
    if (S.Count = 0) or (Temp <> S[S.Count-1]) then
      S.Add(Temp);
  end;
  Result := 1;
end;

procedure GetFontList(List: TStrings);
var
  DC: HDC;
  LList: TStringList;
  LFont: TLogFont;
begin
  DC := GetDC(0);
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  LList := TStringList.Create;
  try
    LList.Sorted := True;
    EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, Winapi.Windows.LPARAM(LList), 0);
    List.Assign(LList);
  finally
    LList.Free;
  end;
  ReleaseDC(0, DC);
end;

end.

