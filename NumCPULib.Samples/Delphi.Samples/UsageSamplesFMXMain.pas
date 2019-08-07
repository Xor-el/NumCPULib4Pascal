unit UsageSamplesFMXMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, NumCPULib;

type
  TForm1 = class(TForm)
    GetCPUCount: TButton;
    procedure GetCPUCountClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.GetCPUCountClick(Sender: TObject);
begin
  ShowMessage(Format('Logical CPU Count is %d',
    [TNumCPULib.GetLogicalCPUCount()]));

  ShowMessage(Format('Physical CPU Count is %d',
    [TNumCPULib.GetPhysicalCPUCount()]));
end;

end.
