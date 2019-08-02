# NumCPULib4Pascal


Count the number of CPUs on the current machine.

## Usage

Add `NumCPULib` to uses clause:

```pascal
uses
  NumCPULib;

var
  num: Int32;
begin
// count logical cores this process could try to use
 num := TNumCPULib.GetLogicalCPUCount();
end;
```
