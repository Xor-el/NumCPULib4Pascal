unit UsageSamplesFMXMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, NumCPULib;

type
  TForm1 = class(TForm)
    LogicalProcessorCount: TButton;
    procedure LogicalProcessorCountClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.LogicalProcessorCountClick(Sender: TObject);
begin
  ShowMessage(Format('Logical CPU Count is %d',
    [TNumCPULib.GetLogicalCPUCount()]));
end;

end.
