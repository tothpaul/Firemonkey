unit Execute.FMX.GLPanels.Types;

interface
 uses
 System.Classes,
 FMX.Controls.Presentation,
 FMX.Controls.Model;

 type
  TGLPanelModel = class(TDataModel)
  const
       STYLE_NAME  = 'GLPanel-style';
       PM_INVALIDE = PM_USER + 1;
  type
    TGLEvent = (
      glSetup,
      glResize,
      glPaint
    );
  private
    FEvents: array[TGLEvent] of TNotifyEvent;

    function getEvents(index : TGlevent) : TNotifyEvent;
    procedure setEvents(index : TGlevent; Value : TNotifyEvent);
  public
    function OnEvent(Event: TGLEvent): Boolean;
    property Events[index : TglEvent]:  TNotifyEvent read getevents write setEvents;
  end;
implementation


function TGLPanelModel.OnEvent(Event: TGLEvent): Boolean;
begin
  Result := Assigned(FEvents[Event]);
  if Result then
    FEvents[Event](Owner);
end;

function TGLPanelModel.getEvents(index: TGlevent): TNotifyEvent;
begin
 Result := FEvents[index];
end;

procedure TGLPanelModel.setEvents(index: TGlevent; Value: TNotifyEvent);
begin
 FEvents[index] := Value;
end;

end.
