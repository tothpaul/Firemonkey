unit Execute.FMX.TextPath;
{
  Create a vectorial text in a FMX TPath (c)2017 Execute SARL
  http://www.execute.fr

}
interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  DesignEditors,
  DesignIntf,
  FMX.Forms, FMX.Layouts, FMX.Types, FMX.Controls,
  FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox, FMX.Objects;

type
  TPathEditor = class(TComponentEditor)
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TTextPathEditor = class(TForm)
    Layout1: TLayout;
    Label2: TLabel;
    edText: TEdit;
    Path: TPath;
    Label1: TLabel;
    lbFonts: TListBox;
    Layout2: TLayout;
    Button1: TButton;
    Button2: TButton;
    edSize: TEdit;
    Label3: TLabel;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure edTextChange(Sender: TObject);
  public
    class procedure Edit(APath: TPath);
  end;

procedure Register;

implementation

{$R *.fmx}

uses
  Execute.FontBuilder;

procedure Register;
begin
  RegisterComponentEditor(TPath, TPathEditor);
end;

{ TPathEditor }

procedure TPathEditor.ExecuteVerb(Index: Integer);
var
  Path: TPath;
begin
  Path := GetComponent as TPath;
  TTextPathEditor.Edit(Path);
end;

function TPathEditor.GetVerb(Index: Integer): string;
begin
  Result := 'TextPath ...';
end;

function TPathEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TTextPathEditor }

class procedure TTextPathEditor.Edit(APath: TPath);
var
  Editor: TTextPathEditor;
begin
  Editor := TTextPathEditor.Create(nil);
  try
    if Editor.ShowModal() = mrOK then
    begin
      APath.Data.Assign(Editor.Path.Data);
    end;
  finally
    Editor.Free;
  end;
end;

procedure TTextPathEditor.edTextChange(Sender: TObject);
var
  Size : Integer;
  Style: TFontStyles;
begin
  if lbFonts.ItemIndex >= 0 then
  begin
    Size := StrToInt(edSize.Text);
    Style := [];
    if cbBold.IsChecked then
      Style := [TFontStyle.fsBold];
    if cbItalic.IsChecked then
      Style := Style + [TFontStyle.fsItalic];
    BuildText(edText.Text, lbFonts.Items[lbFonts.ItemIndex], Size, Style, Path.Data);
  end;
end;

procedure TTextPathEditor.FormCreate(Sender: TObject);
begin
  GetFontList(lbFonts.Items);
  lbFonts.ItemIndex := 0;
end;

end.
