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
  if RunCommand('WNIC', ['CPU', 'GET', 'DeviceID,', 'NumberOfLogicalProcessors'], Output) then
    AssertEquals('Test Logical CPU Count', StrToInt(Output.Split(LineEnding)[2]), TNumCPULib.GetLogicalCPUCount);
  {$ELSE}
  if RunCommand('grep', ['-c', 'cpu cores', '/proc/cpuinfo'], Output) then
    AssertEquals('Test Logical CPU Count', StrToInt(Output.Replace(LineEnding, '')), TNumCPULib.GetLogicalCPUCount);
  {$ENDIF}
end;

procedure TTestCase1.GetPhysicalCPUCount;
var
  Output: string;
begin
  {$IFDEF MSWINDOWS}
  if RunCommand('WNIC', ['CPU', 'GET', 'DeviceID,', 'NumberOfCores'], Output) then
    AssertEquals('Test Logical CPU Count', StrToInt(Output.Split(LineEnding)[2]), TNumCPULib.GetPhysicalCPUCount);
  {$ELSE}
  if RunCommand('awk', ['/cpu cores/{ print $4}', '/proc/cpuinfo'], Output) then
    AssertEquals('Physical CPU Count', StrToInt(Output.Split(LineEnding)[1]), TNumCPULib.GetPhysicalCPUCount);
  {$ENDIF}
end;

initialization

  RegisterTest(TTestCase1);
end.

