# NumCPULib4Pascal


Count the number of CPUs (Logical and Physical) on the current machine.

## Usage

Add `NumCPULib` to uses clause:

```pascal
uses
  NumCPULib;

var
  lcc, pcc: Int32;
begin
// count logical cpus (aka logical processors)
 lcc := TNumCPULib.GetLogicalCPUCount();
// count physical cpus (aka cores)
 pcc := TNumCPULib.GetPhysicalCPUCount();
end;
```
