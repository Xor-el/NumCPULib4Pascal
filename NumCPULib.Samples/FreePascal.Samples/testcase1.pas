unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Process, NumCPULib;

type

  TTestCase1= class(TTestCase)
  published
    procedure GetLogicalCPUCount;
    procedure GetPhysicalCPUCount;
  end;

implementation

procedure TTestCase1.GetLogicalCPUCount;
var
  Output: string;
begin
  {$IFDEF MSWINDOWS}
  if RunCommand('pwsh', ['-command', 'wmic cpu get NumberOfLogicalProcessors'], Output) then
    AssertEquals('Test Logical CPU Count', StrToInt(Output.Split(LineEnding)[2]), TNumCPULib.GetLogicalCPUCount)
  else
    Fail(Output);
  {$ELSE}
  if RunCommand('grep', ['-c', 'cpu cores', '/proc/cpuinfo'], Output) then
    AssertEquals('Test Logical CPU Count', StrToInt(Output.Replace(LineEnding, '')), TNumCPULib.GetLogicalCPUCount)
  else
    Fail(Output);
  {$ENDIF}
end;

procedure TTestCase1.GetPhysicalCPUCount;
var
  Output: string;
begin
  {$IFDEF MSWINDOWS}
  if RunCommand('pwsh', ['-command', 'wmic cpu get NumberOfCores'], Output) then
    AssertEquals('Test Physical CPU Count', StrToInt(Output.Split(LineEnding)[2]), TNumCPULib.GetPhysicalCPUCount)
  else
    Fail(Output);
  {$ELSE}
  if RunCommand('awk', ['/cpu cores/{ print $4}', '/proc/cpuinfo'], Output) then
    AssertEquals('Physical CPU Count', StrToInt(Output.Split(LineEnding)[1]), TNumCPULib.GetPhysicalCPUCount)
  else
    Fail(Output);
  {$ENDIF}
end;

initialization

  RegisterTest(TTestCase1);
end.

