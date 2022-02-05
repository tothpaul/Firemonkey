unit PuzzleSolver.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Controls3D, FMX.Objects3D, FMX.Viewport3D,
  FMX.Types3D, FMX.MaterialSources, FMX.Ani, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects;

const
{                         ....
                  YZ01     ....
          MNOP     2345     ....
   ABCD    QRST     6789     ....
    EFGH    UVWX     ....
     IJKL    ....
      ....                                     48 49 50 51
                                  32 33 34 35      52 53 54 55
                  16 17 18 19       36 37 38 39      56 57 58 59
   00 01 02 03     20 21 22 23        40 41 42 43      60 61 62 63
    04 05 06 07      24 25 26 27        44 45 46 47
     08 09 10 11       28 29 30 31
      12 13 14 15
}

  Coords = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';

  Puzzle: array[0..9] of string = (
    'ABFNO',
    'ABCEG',
    'ABCFJV',
    'ABCIMQU',
    'ABCDFJMO0',
    'ABCDEIJK',
    'ACEFGHL',
    'ABCDEM',
    'AEFGHJ',
    'ABCEI'
  );

  Colors: array[0..9] of TAlphaColor = (
    TAlphaColorRec.Springgreen,
    TAlphaColorRec.Royalblue,
    TAlphaColorRec.Orchid,
    TAlphaColorRec.Moccasin,
    TAlphaColorRec.Plum,
    TAlphaColorRec.Sandybrown,
    TAlphaColorRec.Turquoise,
    TAlphaColorRec.Salmon,
    TAlphaColorRec.Khaki,
    TAlphaColorRec.Red
  );

type
  TPiece = class(TDummy)
  private
    BaseShape: UInt64;      // current position
    Shapes: TArray<UInt64>; // list of all available positions
    iShape: Integer;        // last tried position
    procedure CreateShapes;
    function MoveLeft: Boolean;
    function MoveDown: Boolean;
    function MoveUp: Boolean;
    procedure RotateX;
    procedure RotateZ;
    procedure RotateY;
    procedure ShowShape;
    procedure SetShape(Value: Integer);
  end;

  TMain = class(TForm)
    Viewport3D1: TViewport3D;
    Dummy1: TDummy;
    Light1: TLight;
    FloatAnimation1: TFloatAnimation;
    Grid3D1: TGrid3D;
    Grid3D2: TGrid3D;
    Grid3D3: TGrid3D;
    Grid3D4: TGrid3D;
    Grid3D5: TGrid3D;
    Grid3D6: TGrid3D;
    FloatAnimation2: TFloatAnimation;
    Dummy2: TDummy;
    Pie1: TPie;
    Pie2: TPie;
    Pie3: TPie;
    Pie4: TPie;
    Pie5: TPie;
    Pie6: TPie;
    Pie7: TPie;
    Pie8: TPie;
    Pie9: TPie;
    Pie10: TPie;
    TrackBar1: TTrackBar;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
  private
    { Déclarations privées }
    Pieces: TArray<TPiece>;
    Progress: TArray<TPie>;
    Checks: TArray<TCheckBox>;
    Down: TPointF;
    CurMatrix, Matrix: TMatrix3D;
    function AddPuzzle(Index: Integer): TPiece;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    function Check: Boolean;
    procedure CubeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure CubeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
  public
    { Déclarations publiques }
  end;

var
  Main: TMain;

implementation

{$R *.fmx}

type
  TControl3DHelper = class helper for TControl3D
    procedure SetMatrix(const M: TMatrix3D);
  end;

procedure TControl3DHelper.SetMatrix(const M: TMatrix3D);
begin
  FLocalMatrix := M;
  RecalcAbsolute;
  RebuildRenderingList;
  Repaint;
end;

function TMain.AddPuzzle(Index: Integer): TPiece;
var
  C: TCube;
  L: TLightMaterialSource;
begin
  Result := TPiece.Create(Self);
  Result.Parent := Dummy2;
  L := TLightMaterialSource.Create(Result);
  L.Diffuse := Colors[Index];
  var Def := Puzzle[Index];
  for var I := 1 to Length(Def) do
  begin
    var Bit := Pos(Def[I], Coords) - 1;
    // 0.........11 12........23 24........35
    // ABCDEFGHIJKL-MNOPQRSTUVWX-YZ0123456789
    // convert from 4x3x3 matrix to 4x4x4
    if Bit > 23 then
      Inc(Bit, 8)
    else
    if Bit > 11 then
      Inc(Bit, 4);
    Result.BaseShape := Result.BaseShape or (UInt64(1) shl Bit);
    C := TCube.Create(Result);
    C.Tag := Index;
    C.OnMouseDown := CubeMouseDown;
    C.OnMouseUp := CubeMouseUp;
    C.MaterialSource := L;
    C.Parent := Result;
  end;
  Result.ShowShape;
  Result.CreateShapes;
end;

procedure TMain.Button2Click(Sender: TObject);
begin
  if FloatAnimation1.Enabled then
  begin
    Matrix := TMatrix3D.Identity;
    Button2.Text := 'Start';
    FloatAnimation1.Enabled := False;
    FloatAnimation2.Enabled := False;
    for var Index := 0 to 9 do
      Checks[Index].IsChecked := Pieces[Index].Visible;
  end else begin
    Button2.Text := 'Stop';
    FloatAnimation1.Enabled := True;
    FloatAnimation2.Enabled := True;
  end;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  SetLength(Pieces, Length(Puzzle));
  for var P := High(Puzzle) downto Low(Puzzle) do
  begin
    Pieces[P] := AddPuzzle(P);
  end;
  Progress := [Pie1, Pie2, Pie3, Pie4, Pie5, Pie6, Pie7, Pie8, Pie9, Pie10];
  Checks := [CheckBox1,CheckBox2,CheckBox3,CheckBox4,CheckBox5,CheckBox6,CheckBox7,CheckBox8,CheckBox9,CheckBox10];

  Application.OnIdle := OnIdle;
end;

procedure TMain.OnIdle(Sender: TObject; var Done: Boolean);
begin
  Done := False;
  for var Index: Int64 := 0 to Round(TrackBar1.Value) do
  begin
    if Check then
    begin
      Done := True;
      Break;
    end;
  end;

  // Show current positions
  var S: UInt64 := 0;
  for var Index := 0 to Length(Pieces) - 1 do
  begin
    var P := Pieces[Index];

    // show the progression for each piece
    Progress[Index].EndAngle := -90 + 359 * P.iShape / Length(P.Shapes);

    P.ShowShape;
    P.Visible := (S and P.BaseShape) = 0; // hide piece when there a conflict
    S := S or P.BaseShape;
  end;
end;

function TMain.Check: Boolean;
var
  S: UInt64;
  I: Integer;
  P: TPiece;
  N: Integer;
begin
  Result := False;

  S := 0;  // Current solution
  for I := 0 to Length(Pieces) - 1 do
  begin
    P := Pieces[I];
    if (S and P.BaseShape) = 0 then // no collision beween solution and the new piece
    begin
      S := S or P.BaseShape;  // add the piece to the solution
    end else begin
      if P.iShape = Length(P.Shapes) - 1 then // is that the last position
      begin
        P.SetShape(0); // reset
        for N := I - 1 downto 0 do  // change previous pieces
        begin
          P := Pieces[N];
          if P.iShape  < Length(P.Shapes) - 1 then // not the last position
          begin
            P.SetShape(P.iShape + 1);  // try the next one
            Exit(False);  // no solution found yet
          end;
          P.SetShape(0); // reset this one, try next position of the previous one
        end;
        // no more previous piece, so there's no solution
        Application.OnIdle := nil;
        ShowMessage('No solution ?');
        Exit(True);
      end;
      P.SetShape(P.iShape + 1); // go to next position
      Exit;   // no solution yet
    end;
  end;

  if S = $FFFFFFFFFFFFFFFF then // solution found when the 64 bits are set !
  begin
    Application.OnIdle := nil;
    ShowMessage('Solution found !');
    Exit(True);
  end;
end;

procedure TMain.CheckBox1Change(Sender: TObject);
begin
  Pieces[TComponent(Sender).Tag].Visible := Checks[TComponent(Sender).Tag].IsChecked;
end;

procedure TMain.CubeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  var Current := TComponent(Sender).Tag;
  if Button = TMouseButton.mbRight then
  begin
    Pieces[Current].Visible := False;
    Checks[Current].IsChecked := False;
  end else
  for var Index := 0 to Length(Pieces) - 1 do
  begin
    Pieces[Index].Visible := Index = Current;
  end;
end;

procedure TMain.CubeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  for var Index := 0 to Length(Pieces) - 1 do
  begin
    Pieces[Index].Visible := Checks[Index].IsChecked;
  end;
end;

procedure TMain.Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Down.X := X;
  Down.Y := Y;
  CurMatrix := Matrix;
end;

procedure TMain.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if ssLeft in Shift then
  begin
    Matrix := CurMatrix
            * TMatrix3D.CreateRotationY((Down.X - X) / 100)
            * TMatrix3D.CreateRotationX((Y - Down.Y) / 100);
    Dummy2.SetMatrix(Matrix);
  end;
end;

{ TPiece }

procedure TPiece.CreateShapes;
var
  x, y, z, rx, ry, rz, s: Integer;
  ix,iy,iz: Integer;
  Shape, ShapeZ, ShapeY, ShapeX, ShapeRX, ShapeRY: UInt64;

  procedure AddShape;
  var
    i: Integer;
  begin
    for i := 0 to s - 1 do
    begin
      if Shapes[i] = BaseShape then
        Exit;
    end;
    Shapes[s] := BaseShape;
    Inc(s);
  end;

begin
  x := 1;
  y := 1;
  z := 1;
  Shape := BaseShape;
  while MoveLeft do
    Inc(x);
  BaseShape := Shape;
  while MoveDown do
    Inc(y);
  BaseShape := Shape;
  while MoveUp do
    Inc(z);
  BaseShape := Shape;
  SetLength(Shapes, 4 * 4  * 4 * x * y * z);
  s := 0;
  for iz := 1 to z do
  begin
    ShapeZ := BaseShape;
    for iy := 1 to y do
    begin
      ShapeY := BaseShape;
      for ix := 1 to x do
      begin
        ShapeX := BaseShape;
        for rx := 0 to 3 do
        begin
          ShapeRX := BaseShape;
          for ry := 0 to 3 do
          begin
            ShapeRY := BaseShape;
            for rz := 0 to 3 do
            begin
              AddShape;
              RotateZ;
            end;
            BaseShape := ShapeRY;
            RotateY;
          end;
          BaseShape := ShapeRX;
          RotateX;
        end;
        BaseShape := ShapeX;
        MoveLeft;
      end;
      BaseShape := ShapeY;
      MoveDown;
    end;
    BaseShape := ShapeZ;
    MoveUp;
  end;
  SetLength(Shapes, s);
  BaseShape := Shapes[0];
end;

function TPiece.MoveLeft: Boolean;
begin
  Result := (BaseShape and (
     (UInt64(1) shl 03)
  or (UInt64(1) shl 07)
  or (UInt64(1) shl 11)
  or (UInt64(1) shl 15)

  or (UInt64(1) shl 19)
  or (UInt64(1) shl 23)
  or (UInt64(1) shl 27)
  or (UInt64(1) shl 31)

  or (UInt64(1) shl 35)
  or (UInt64(1) shl 39)
  or (UInt64(1) shl 43)
  or (UInt64(1) shl 47)

  or (UInt64(1) shl 51)
  or (UInt64(1) shl 55)
  or (UInt64(1) shl 59)
  or (UInt64(1) shl 63)
  )) = 0;
  if Result then
    BaseShape := BaseShape shl 1;
end;

function TPiece.MoveDown: Boolean;
begin
  Result := (BaseShape and (
     (UInt64(1) shl 12)
  or (UInt64(1) shl 13)
  or (UInt64(1) shl 14)
  or (UInt64(1) shl 15)
  or (UInt64(1) shl 28)
  or (UInt64(1) shl 29)
  or (UInt64(1) shl 30)
  or (UInt64(1) shl 31)
  or (UInt64(1) shl 44)
  or (UInt64(1) shl 45)
  or (UInt64(1) shl 46)
  or (UInt64(1) shl 47)
  or (UInt64(1) shl 60)
  or (UInt64(1) shl 61)
  or (UInt64(1) shl 62)
  or (UInt64(1) shl 63)
  )) = 0;
  if Result then
    BaseShape := BaseShape shl 4;
end;

function TPiece.MoveUp: Boolean;
begin
  Result := (BaseShape and (
     (UInt64(1) shl 48)
  or (UInt64(1) shl 49)
  or (UInt64(1) shl 50)
  or (UInt64(1) shl 51)
  or (UInt64(1) shl 52)
  or (UInt64(1) shl 53)
  or (UInt64(1) shl 54)
  or (UInt64(1) shl 55)
  or (UInt64(1) shl 56)
  or (UInt64(1) shl 57)
  or (UInt64(1) shl 58)
  or (UInt64(1) shl 59)
  or (UInt64(1) shl 60)
  or (UInt64(1) shl 61)
  or (UInt64(1) shl 62)
  or (UInt64(1) shl 63)
  )) = 0;
  if Result then
    BaseShape := BaseShape shl 16;
end;

procedure TPiece.RotateX;
{
                                                 48 49 50 51
                                  32 33 34 35      52 53 54 55
                  16 17 18 19       36 37 38 39      56 57 58 59
   00 01 02 03     20 21 22 23        40 41 42 43      60 61 62 63
    04 05 06 07      24 25 26 27        44 45 46 47
     08 09 10 11       28 29 30 31
      12 13 14 15

                                                 60 61 62 63
                                  56 57 58 59      44 45 46 47
                  52 53 54 55       40 41 42 43      28 29 30 31
   48 49 50 51     36 37 38 39        24 25 26 27      12 13 14 15
    32 33 34 35      20 21 22 23        08 09 10 11
     16 17 18 19       04 05 06 07
      00 01 02 03

}
const
  Map: array[0..15] of Byte = (
    48, 49, 50, 51,
    32, 33, 34, 35,
    16, 17, 18, 19,
    00, 01, 02, 03
  );
var
  Shape: UInt64;
  I, J: Integer;
begin
  Shape := BaseShape;
  BaseShape := 0;
  for I := 0 to 3 do
  begin
    for J := 0 to 15 do
    begin
      if Shape and (UInt64(1) shl (J + 16 * I)) > 0 then
      begin
        Inc(BaseShape, (UInt64(1) shl (Map[J] + 4 * I)));
      end;
    end;
  end;
end;

procedure TPiece.RotateZ;
{
                                                 48 49 50 51
                                  32 33 34 35      52 53 54 55
                  16 17 18 19       36 37 38 39      56 57 58 59
   00 01 02 03     20 21 22 23        40 41 42 43      60 61 62 63
    04 05 06 07      24 25 26 27        44 45 46 47
     08 09 10 11       28 29 30 31
      12 13 14 15

                                                 00 16 32 48
                                  01 17 33 49      04 20 36 52
                  02 18 34 50       05 21 37 53      08 24 40 56
   03 19 35 51     06 22 38 54        09 25 41 57      12 28 44 60
    07 23 39 55      10 26 42 58        13 29 45 61
     11 27 43 59       14 30 46 62
      15 31 47 63

}
const
  Map: array[0..15] of Byte = (
    03, 19, 35, 51,
    07, 23, 39, 55,
    11, 27, 43, 59,
    15, 31, 47, 63
  );
var
  Shape: UInt64;
  I, J: Integer;
begin
  Shape := BaseShape;
  BaseShape := 0;
  for I := 0 to 3 do
  begin
    for J := 0 to 15 do
    begin
      if Shape and (UInt64(1) shl (J + 16 * I)) > 0 then
      begin
        Inc(BaseShape, (UInt64(1) shl (Map[J] - I)));
      end;
    end;
  end;
end;

procedure TPiece.RotateY;
{
                                                 48 49 50 51
                                  32 33 34 35      52 53 54 55
                  16 17 18 19       36 37 38 39      56 57 58 59
   00 01 02 03     20 21 22 23        40 41 42 43      60 61 62 63
    04 05 06 07      24 25 26 27        44 45 46 47
     08 09 10 11       28 29 30 31
      12 13 14 15

                                                 60 56 52 48
                                  44 40 36 32      61 57 53 49
                  28 24 20 16       45 41 37 33      62 58 54 50
   12 08 04 00     29 25 21 17        46 42 38 34      63 59 55 51
    13 09 05 01      30 26 22 18        47 43 39 35
     08 09 10 11       31 27 23 19
      15 11 07 03

}
const
  Map: array[0..15] of Byte = (
    12, 08, 04, 00,
    13, 09, 05, 01,
    14, 10, 06, 02,
    15, 11, 07, 03
  );
var
  Shape: UInt64;
  I, J: Integer;
begin
  Shape := BaseShape;
  BaseShape := 0;
  for I := 0 to 3 do
  begin
    for J := 0 to 15 do
    begin
      if Shape and (UInt64(1) shl (J + 16 * I)) > 0 then
      begin
        Inc(BaseShape, (UInt64(1) shl (Map[J] + 16 * I)));
      end;
    end;
  end;
end;

procedure TPiece.SetShape(Value: Integer);
begin
  if Value < 0 then
    Value := 0;

  if Value >= Length(Shapes) then
    Value := 0;

  if Value <> iShape then
  begin
    iShape := Value;
    BaseShape := Shapes[iShape];
  end;
end;

procedure TPiece.ShowShape;
var
  Bit: Integer;
  N: Integer;
  C: TCube;
begin
  N := 0;
  for Bit := 0 to 63 do
  begin
    if BaseShape and (UInt64(1) shl Bit) > 0 then
    begin
      C := Children[N] as TCube;
      C.Position.X :=  (Bit mod 4 - 1.5);
      C.Position.Y := -((Bit div 4) mod 4 - 1.5);
      C.Position.Z :=  (Bit div 4) div 4 - 1.5;
      Inc(N);
    end;
  end;
  Assert(N = ChildrenCount);
end;

end.
