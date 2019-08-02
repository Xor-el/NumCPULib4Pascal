program UsageSamplesFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UsageSamplesFMXMain in 'UsageSamplesFMXMain.pas' {Form1},
  NumCPULib in '..\..\NumCPULib\src\NumCPULib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
