# NumCPULib4Pascal


Count the number of CPUs (Logical and Physical) on the current machine.

**Build Status**
[![Build Status](https://github.com/Xor-el/NumCPULib4Pascal/actions/workflows/make.yml/badge.svg)](https://github.com/Xor-el/NumCPULib4Pascal/actions/workflows/make.yml)

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
