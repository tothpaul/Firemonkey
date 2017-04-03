unit Execute.FMX.GLPanels;

{
   FMX GLPanel for Delphi Tokyo (c)2017 Execute SARL
}

interface

// http://yaroslavbrovin.ru/new-approach-of-development-of-firemonkey-control-control-model-presentation-part-1-en/

uses
  System.Classes,
  FMX.Controls.Model,
  FMX.Controls.Presentation;

type
// the connection between platform specific presentation(ie TGLPanelWin) and TGLPanel
  TGLPanelModel = class(TDataModel)
  type
    TGLEvent = (
      glSetup,
      glResize,
      glPaint
    );
  private
    FEvents: array[TGLEvent] of TNotifyEvent;
  public
    function OnEvent(Event: TGLEvent): Boolean;
  end;

// the published GLPanel component
  TGLPanel = class(TPresentedControl)
  const
  // link to the registered presentation (TPresentationProxyFactory.Current.Register)
    STYLE_NAME  = 'GLPanel-style';
  // Presentation Messages
    PM_INVALIDE = PM_USER + 1;
  private
  // could be a [weak] reference, [unsafe] is fine also, but the value of FModel could be invalide
    [unsafe] FModel: TGLPanelModel;
  // generic event getter/setter
    function GetGLEvent(Index: TGLPanelModel.TGLEvent): TNotifyEvent;
    procedure SetGLEvent(Index: TGLPanelModel.TGLEvent; Value: TNotifyEvent);
  protected
  // explicit link to model class
    function DefineModelClass: TDataModelClass; override;
  // link to presentation (by name)
    function DefinePresentationName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  // refresh the OpenGL panel
    procedure Invalidate;
  published
  // change the general OpenGL settings
    property OnGLSetup: TNotifyEvent index TGLPanelModel.TGLEvent.glSetup read GetGLEvent write SetGLEvent;
  // when the Panel is resized
    property OnGLResize: TNotifyEvent index TGLPanelModel.TGLEvent.glResize read GetGLEvent write SetGLEvent;
  // when the Panel is rendering
    property OnGLPaint: TNotifyEvent index TGLPanelModel.TGLEvent.glPaint read GetGLEvent write SetGLEvent;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
// Windows presentation for TGLPanel
  Execute.FMX.GLPanels.Win;
{$ENDIF}

{ TGLPanelModel }

function TGLPanelModel.OnEvent(Event: TGLEvent): Boolean;
begin
  Result := Assigned(FEvents[Event]);
  if Result then
    FEvents[Event](Owner);
end;

{ TGLPanel }

constructor TGLPanel.Create(AOwner: TComponent);
begin
  inherited;
  FModel := GetModel<TGLPanelModel>;
  SetAcceptsControls(False);
  CanFocus := True;
  ClipChildren := False;
end;

function TGLPanel.DefineModelClass: TDataModelClass;
begin
  Result := TGLPanelModel;
end;

function TGLPanel.DefinePresentationName: string;
begin
  Result := STYLE_NAME;
end;

function TGLPanel.GetGLEvent(Index: TGLPanelModel.TGLEvent): TNotifyEvent;
begin
  Result := FModel.FEvents[Index];
end;

procedure TGLPanel.SetGLEvent(Index: TGLPanelModel.TGLEvent; Value: TNotifyEvent);
begin
  FModel.FEvents[Index] := Value;
end;

procedure TGLPanel.Invalidate;
begin
  PresentationProxy.SendMessage(PM_INVALIDE);
end;

end.
