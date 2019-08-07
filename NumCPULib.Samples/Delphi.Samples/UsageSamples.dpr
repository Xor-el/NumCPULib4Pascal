program UsageSamples;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  NumCPULib in '..\..\NumCPULib\src\NumCPULib.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    WriteLn(Format('Logical CPU Count is %d', [TNumCPULib.GetLogicalCPUCount()]));
    WriteLn(Format('Physical CPU Count is %d', [TNumCPULib.GetPhysicalCPUCount()]));
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
