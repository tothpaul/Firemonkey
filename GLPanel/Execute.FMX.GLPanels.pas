unit Execute.FMX.GLPanels;

{
   FMX GLPanel for Delphi Tokyo (c)2017 Execute SARL
}

interface

// http://yaroslavbrovin.ru/new-approach-of-development-of-firemonkey-control-control-model-presentation-part-1-en/

uses
  System.Classes,
  FMX.Types,
  FMX.Presentation.Messages,
  FMX.Controls.Model,
  Execute.FMX.GLPanels.Types,
  FMX.Controls.Presentation;

type
// the published GLPanel component
   TGLPanel = class(TPresentedControl)
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

{$IFDEF MACOS}
uses
 Execute.Presentation.Mac;
{$ENDIF}

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
  Result :=  TGLPanelModel.STYLE_NAME;
end;

function TGLPanel.GetGLEvent(Index: TGLPanelModel.TGLEvent): TNotifyEvent;
begin
  Result := FModel.Events[Index];
end;

procedure TGLPanel.SetGLEvent(Index: TGLPanelModel.TGLEvent; Value: TNotifyEvent);
begin
  FModel.Events[Index] := Value;
end;

procedure TGLPanel.Invalidate;
begin
  PresentationProxy.SendMessage(TGLPanelModel.PM_INVALIDE);
end;


end.
