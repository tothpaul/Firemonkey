unit Execute.FMX.GLPanels.Win;

{
   FMX GLPanel for Delphi Tokyo (c)2017 Execute SARL
}


interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  Winapi.OpenGLExt,
  System.Classes,
  FMX.Controls,
  FMX.Controls.Model,
  FMX.Controls.Win,
  FMX.Presentation.Factory,
  FMX.Presentation.Win,
  Execute.FMX.GLPanels;

type
  TWinGLPanel = class(TWinPresentation)
  private
  // Windows stuffs
    FDC   : HDC;
    FGL   : HGLRC;
    FSetup: Boolean;
    procedure CreateGLContext;
    procedure DestroyGLContext;
    procedure ResizeGL;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMPaint(var Msg: TMessage); message WM_PAINT;
  // Presentation messages
    procedure PMInvalidate(var Msg: TDispatchMessage); message TGLPanel.PM_INVALIDE;
  private
   [unsafe] FModel: TGLPanelModel;
  protected
  // need this
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure Resized; override;
  // link to the model
    function DefineModelClass: TDataModelClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TWinGLPanel }

constructor TWinGLPanel.Create(AOwner: TComponent);
begin
  inherited;
  FModel := TGLPanelModel(inherited Model);
end;

procedure TWinGLPanel.CreateGLContext;
var
  pfd: TPIXELFORMATDESCRIPTOR;
  pixelformat: Integer;
  Cl: TRGBQuad;
begin
  FDC := GetDC(Handle);
  if FDC = 0 then
    Exit;
 // set pixel format
  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize       := sizeof(pfd);
  pfd.nVersion    := 1;
  pfd.dwFlags     := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iLayerType  := PFD_MAIN_PLANE;
  pfd.iPixelType  := PFD_TYPE_RGBA;
  pfd.cColorBits  := 32;
  pfd.iLayerType  := PFD_MAIN_PLANE;
  pfd.cStencilBits:= 0;
  pixelformat := ChoosePixelFormat(FDC, @pfd);
  if PixelFormat = 0 then
    Exit;
  if not SetPixelFormat(FDC, pixelformat, @pfd) then
    Exit;
 // create OpenGL Context
  FGL := wglCreateContext(FDC);
 // select it
  wglMakeCurrent(FDC, FGL);
 // setup GL mode
 // setup the clear color
//  Integer(cl) := ColorToRGB(Color);
//  glClearColor(cl.rgbRed/255, cl.rgbGreen/255, cl.rgbBlue/255, 1);
 // setup the clear depth
  glClearDepth(1);

  InitOpenGLext;

  FSetup := FModel.OnEvent(TGLPanelModel.TGLEvent.glSetup);

  ResizeGL();
end;

procedure TWinGLPanel.CreateHandle;
begin
  inherited;
  if HandleAllocated then
  begin
    CreateGLContext;
  end;
end;

procedure TWinGLPanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WindowClass.hbrBackground := GetStockObject(WHITE_BRUSH);
end;

function TWinGLPanel.DefineModelClass: TDataModelClass;
begin
  Result := TGLPanelModel;
end;

procedure TWinGLPanel.DestroyGLContext;
begin
  if FGL <> 0 then
  begin
    wglMakeCurrent(FDC, 0);
    wglDeleteContext(FGL);
    FGL := 0;
  end;
  if FDC <> 0 then
  begin
    DeleteDC(FDC);
    FDC := 0;
  end;
end;

procedure TWinGLPanel.DestroyHandle;
begin
  if HandleAllocated then
  begin
    DestroyGLContext;
  end;
  inherited;
end;

procedure TWinGLPanel.PMInvalidate(var Msg: TDispatchMessage);
begin
  InvalidateRect(Handle, nil, False);
end;

procedure TWinGLPanel.Resized;
begin
  inherited;
  if FGL <> 0 then
  begin
    ResizeGL;
    InvalidateRect(Handle, nil, False);
  end;
end;

procedure TWinGLPanel.ResizeGL;
var
  LSize: TSize;
begin
  LSize := Size;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  if LSize.cy <> 0 then
    gluPerspective(45, LSize.cx / LSize.cy, 1, 1000);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glViewport(0, 0, LSize.cx, LSize.cy);
  FModel.OnEvent(TGLPanelModel.TGLEvent.glResize);
end;

procedure TWinGLPanel.WMEraseBkGnd(var Msg: TMessage);
begin
  if FGL <> 0 then
    Msg.Result := 1
  else
    inherited;
end;

procedure TWinGLPanel.WMPaint(var Msg: TMessage);
begin
  if FGL <> 0 then
  begin
    wglMakeCurrent(FDC, FGL);
    if FSetup = False then
    begin
      FModel.OnEvent(TGLPanelModel.TGLEvent.glSetup);
      FSetup := True;
    end;
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    FModel.OnEvent(TGLPanelModel.TGLEvent.glPaint);
    glFlush();
    SwapBuffers(FDC);
    ValidateRect(Handle, nil);
  end else begin
    inherited;
  end;
end;

initialization
  TPresentationProxyFactory.Current.Register(TGLPanel.STYLE_NAME, TWinPresentationProxy<TWinGLPanel>);
finalization
  TPresentationProxyFactory.Current.Unregister(TGLPanel.STYLE_NAME, TWinPresentationProxy<TWinGLPanel>);
end.
