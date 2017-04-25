{ ******************************************************* }
{ }
{ Delphi FireMonkey Platform }
{ }
{ Copyright(c) 2016 Embarcadero Technologies, Inc. }
{ All rights reserved }
{ }
{ ******************************************************* }

unit Execute.Presentation.Mac;

interface

{$SCOPEDENUMS ON}

uses
  System.TypInfo,
  System.Types,
  System.Classes,
  Macapi.ObjectiveC,
  Macapi.Foundation,
  Macapi.CocoaTypes,
  Macapi.AppKit,
  Macapi.CoreGraphics,
  FMX.Graphics,
  FMX.Controls.Presentation,
  FMX.Controls,
  FMX.Presentation.Messages,
  FMX.Forms,
  FMX.Types,
  Execute.FMX.GLPanels.Types,
  FMX.Controls.Model;

type

  { TMacNativeView }

  TMacNativeView = class;

  IFMXNsView = interface(NsOpenglView)
    ['{BA60202C-47E6-4FD2-9999-30EC62C6384C}']
    procedure drawRect(dirtyRect: NSRect); cdecl;
  end;

  TMacNativeView = class(TOCLocal)
  private
  private
    [Weak]
    FParentView: NSView;
    [Weak]
    FControl: TControl;
    [Weak]
    FModel: TGLPanelModel;
    [Weak]
    FForm: TCommonCustomForm;
    FSize: TSizeF;
    FVisible: Boolean;
    FControlSize: TSizeF;

    FContext: NSOpenGLContext;
    FPixelFormat: NSOpenGLPixelFormat;
    FContentRect: NSRect;

    procedure RefreshNativeParent; virtual;
    procedure SetupOpenGLContext;
    procedure UpdateOrderAndBounds;
    procedure ResizeGL;

  protected
    procedure InitView; virtual;
    function GetViewFrame: NSRect;
    procedure SetSize(const ASize: TSizeF); virtual;
    function GetObjectiveCClass: PTypeInfo; override;

    function GetView: NsOpenglView; overload;

    function GetParentView: NSView;
    procedure Resized; virtual;
    { Messages from PresentationProxy }
    procedure PMGetNativeObject(var AMessage: TDispatchMessageWithValue<IInterface>); message PM_GET_NATIVE_OBJECT;
    procedure PMRootChanged(var AMessage: TDispatchMessageWithValue<IRoot>); message PM_ROOT_CHANGED;
    procedure PMAncesstorPresentationLoaded(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESTOR_PRESENTATION_LOADED;
    procedure PMRefreshParent(var AMessage: TDispatchMessage); message PM_REFRESH_PARENT;
    procedure PMChangeOrder(var AMessage: TDispatchMessage); message PM_CHANGE_ORDER;
    procedure PMAbsoluteChanged(var AMessage: TDispatchMessage); message PM_ABSOLUTE_CHANGED;
    procedure PMSetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_SET_SIZE;
    procedure PMGetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_SIZE;
    procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    procedure PMGetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_GET_VISIBLE;
    procedure PMAncesstorVisibleChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESSTOR_VISIBLE_CHANGED;
    procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    procedure PMGetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_GET_ABSOLUTE_ENABLED;
    procedure PMDoExit(var AMessage: TDispatchMessage); message PM_DO_EXIT;
    procedure PMDoEnter(var AMessage: TDispatchMessage); message PM_DO_ENTER;
    procedure PMResetFocus(var AMessage: TDispatchMessage); message PM_RESET_FOCUS;

    procedure PMInvalidate(var AMessage: TDispatchMessage); message TGLPanelModel.PM_INVALIDE;



  public
    procedure drawRect(dirtyRect: NSRect); cdecl;
    property Control: TControl read FControl;

    function DefineModelClass: TDataModelClass; virtual;
    constructor Create; overload; virtual;
    constructor Create(const AModel: TDataModel; const AControl: TControl); overload; virtual;
    destructor Destroy; override;
    function HasControl: Boolean;
    procedure SetFocus; virtual;
    property Model: TGLPanelModel read FModel;
    property View: NsOpenglView read GetView;

  end;

  TMacNativeViewClass = class of TMacNativeView;

  /// <summary>Generics proxy for all Mac native presentations</summary>
  TMacPresentationProxy<T: TMacNativeView, constructor> = class(TPresentationProxy)
  protected
    function CreateReceiver: TObject; override;
  end;

implementation

uses
  System.UITypes,
  System.SysUtils,
  System.Math,
  Macapi.Helpers,
  Macapi.ObjCRuntime,
  Macapi.OpenGL,
  FMX.Platform.Mac,
  FMX.Surfaces,
  FMX.Presentation.Factory,
  FMX.Helpers.Mac,
  FMX.Consts;

constructor TMacNativeView.Create;
begin
  inherited;
  FVisible := True;
end;

procedure TMacNativeView.InitView;
var
  V: Pointer;

begin
  V := NsOpenglView(Super).initWithFrame(GetViewFrame, TNSOpenGLView.OCClass.defaultPixelFormat);
  if NSAppKitVersionNumber >= NSAppKitVersionNumber10_7 then
    NsOpenglView(Super).setWantsBestResolutionOpenGLSurface(True);
  if GetObjectID <> V then
    UpdateObjectID(V);
  if HasControl then
    begin
      FParentView := GetParentView;
      if FParentView <> nil then
        FParentView.addSubview(NSView(Super));
    end;
end;

constructor TMacNativeView.Create(const AModel: TDataModel; const AControl: TControl);
begin
  FControl := AControl;
  FModel := AModel as TGLPanelModel;
  if FModel is DefineModelClass then
    FModel.Receiver := Self
  else
    raise EPresentationWrongModel.CreateFmt(SWrongModelClassType, [DefineModelClass.ClassName, FModel.ClassName]);

  Create;
  InitView;
end;

function TMacNativeView.DefineModelClass: TDataModelClass;
begin
  Result := TDataModel;
end;

destructor TMacNativeView.Destroy;
begin
  View.removeFromSuperview;
  inherited;
end;

function TMacNativeView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXNsView);
end;

function TMacNativeView.GetView: NsOpenglView;
begin
  Result := NsOpenglView(Super);
end;

procedure TMacNativeView.SetupOpenGLContext;
var
  Attributes: TArray<NSOpenGLPixelFormatAttribute>;
begin
  Attributes := TArray<NSOpenGLPixelFormatAttribute>.Create(NSOpenGLPFAOpenGLProfile, NSOpenGLProfileVersionLegacy, NSOpenGLPFADoubleBuffer, NSOpenGLPFADepthSize, 16);
  Attributes := Attributes + [0];
  FPixelFormat := TNSOpenGLPixelFormat.Wrap(TNSOpenGLPixelFormat.Alloc.initWithAttributes(@Attributes[0]));
  FContext := TNSOpenGLContext.Wrap(TNSOpenGLContext.Alloc.initWithFormat(FPixelFormat, nil));
  View.setOpenGLContext(FContext);
  FContext.makeCurrentContext;
  ResizeGL();
end;

function TMacNativeView.GetViewFrame: NSRect;
begin
  if HasControl then
      Result :=   Nsrect.Create(FControl.AbsoluteRect)
  else
    Result := CGRectMake(0, 0, 50, 50);
end;

function TMacNativeView.HasControl: Boolean;
begin
  Result := FControl <> nil;
end;


procedure TMacNativeView.SetFocus;
begin
  View.becomeFirstResponder;
end;

procedure TMacNativeView.SetSize(const ASize: TSizeF);
var
  ViewSize: NSSize;
begin
  FSize := ASize;
  View.setFrameSize(cgsize.Create(FSize));
  UpdateOrderAndBounds;
end;


function TMacPresentationProxy<T>.CreateReceiver: TObject;
var
  PresentationClass: TMacNativeViewClass;
begin
  PresentationClass := T;
  Result := PresentationClass.Create(Model, PresentedControl);
end;

procedure TMacNativeView.PMRootChanged(var AMessage: TDispatchMessageWithValue<IRoot>);
begin
  if AMessage.Value is TCommonCustomForm then
    FForm := TCommonCustomForm(AMessage.Value)
  else
    FForm := nil;
  RefreshNativeParent;
end;

procedure TMacNativeView.PMGetNativeObject(var AMessage: TDispatchMessageWithValue<IInterface>);
begin
  AMessage.Value := View;
end;

procedure TMacNativeView.PMGetSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
var
  Size: NSSize;
begin
  AMessage.Value := FControlSize;
end;

function TMacNativeView.GetParentView: NSView;
var
  FormHandle: TMacWindowHandle;
begin
  if FForm <> nil then
    begin
      FormHandle := WindowHandleToPlatform(FForm.Handle);
      Result := FormHandle.View;
    end
  else
    Result := nil;
end;

procedure TMacNativeView.RefreshNativeParent;
begin
  FParentView := nil;
  if HasControl then
    begin
      FParentView := GetParentView;
      if FParentView = nil then
        begin
          View.removeFromSuperview
        end
      else
        begin
          FParentView.addSubview(View);
          SetupOpenGLContext;
        end;
    end;
end;

procedure TMacNativeView.drawRect(dirtyRect: NSRect);
begin
  glClearColor(0, 0, 0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  glLoadIdentity();
  FModel.Events[glSetup](Self);
  FModel.Events[glPaint](Self);
  FContext.flushBuffer;
end;

procedure TMacNativeView.PMAncesstorPresentationLoaded(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  RefreshNativeParent;
   UpdateOrderAndBounds;
end;

procedure TMacNativeView.PMRefreshParent(var AMessage: TDispatchMessage);
begin
  RefreshNativeParent;
end;

procedure TMacNativeView.PMChangeOrder(var AMessage: TDispatchMessage);
begin
  UpdateOrderAndBounds;
end;

procedure TMacNativeView.PMAbsoluteChanged(var AMessage: TDispatchMessage);
begin
  UpdateOrderAndBounds;
end;

procedure TMacNativeView.PMSetSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  FControlSize := AMessage.Value;
  UpdateOrderAndBounds;
  Resized;
end;

procedure TMacNativeView.PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
 View.setHidden(not AMessage.Value);
end;

procedure TMacNativeView.PMGetVisible(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  AMessage.Value := not View.isHiddenOrHasHiddenAncestor;
end;

procedure TMacNativeView.PMAncesstorVisibleChanged(var AMessage: TDispatchMessageWithValue<Boolean>);
var
  LMessage: TDispatchMessageWithValue<Boolean>;
begin
  LMessage.Value := Control.Visible and Control.ParentedVisible;
  PMSetVisible(LMessage);
  if LMessage.Value then
    UpdateOrderAndBounds;
end;

procedure TMacNativeView.PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>);
begin

end;

procedure TMacNativeView.PMGetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  AMessage.Value := not View.isHiddenOrHasHiddenAncestor;
end;

procedure TMacNativeView.PMDoExit(var AMessage: TDispatchMessage);
begin
end;

procedure TMacNativeView.PMDoEnter(var AMessage: TDispatchMessage);
begin
end;

procedure TMacNativeView.PMResetFocus(var AMessage: TDispatchMessage);
begin
end;

procedure TMacNativeView.UpdateOrderAndBounds;
var
  temp: NSRect;
  winrect : NSRect;
begin
  temp := GetViewFrame;
  if  GetParentView <> nil then
 begin
   winrect := GetParentView.frame;
  temp.origin.y := winrect.size.height -  (temp.origin.y +  temp.size.height);
 end;
  View.setFrame(temp);
  Resized;
end;

procedure TMacNativeView.Resized;
begin
  ResizeGL;
  FModel.OnEvent(TGLPanelModel.TGLEvent.glResize);
end;

procedure TMacNativeView.PMInvalidate(var AMessage: TDispatchMessage);
begin
  View.display;
end;

procedure TMacNativeView.ResizeGL;

  procedure perspective(const fovy, aspect, zNear, zFar: double);
  var
    xmin, xmax, ymin, ymax: double;
  begin
    ymax := zNear * tan(fovy * PI / 360.0);
    ymin := - ymax;
    xmin := ymin * aspect;
    xmax := ymax * aspect;
    glFrustum(xmin, xmax, ymin, ymax, zNear, zFar);
  end;

var
  LSize: TSizeF;

begin
  LSize :=   GetViewFrame.size.ToSizeF;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  if LSize.cy <> 0 then
    perspective(45, LSize.cx / LSize.cy, 1, 1000);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

initialization

InitOpenGL;
TPresentationProxyFactory.Current.Register(TGLPanelModel.STYLE_NAME, TMacPresentationProxy<TMacNativeView>);

finalization

TPresentationProxyFactory.Current.Unregister(TGLPanelModel.STYLE_NAME, TMacPresentationProxy<TMacNativeView>);

end.
