program UsageSamples;

{$IFDEF FPC}
   {$MODE DELPHI}
{$ENDIF}

//{$APPTYPE CONSOLE}

uses
  SysUtils,
  NumCPULib;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    WriteLn(Format('Logical CPU Count is %d', [TNumCPULib.GetLogicalCPUCount()]));
    WriteLn(Format('Physical CPU Count is %d', [TNumCPULib.GetPhysicalCPUCount()]));
    //ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
