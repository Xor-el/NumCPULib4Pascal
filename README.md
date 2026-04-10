<p align="center">
  <img src="assets/branding/logo.svg" width="160" alt="NumCPULib4Pascal logo" />
  <h1 align="center">NumCPULib4Pascal</h1>
  <p align="center">
    <strong>CPU count detection for Object Pascal</strong>
  </p>
  <p align="center">
    <a href="https://github.com/Xor-el/NumCPULib4Pascal/actions/workflows/make.yml"><img src="https://github.com/Xor-el/NumCPULib4Pascal/actions/workflows/make.yml/badge.svg" alt="Build Status"></a>
    <a href="https://github.com/Xor-el/NumCPULib4Pascal/blob/master/LICENSE"><img src="https://img.shields.io/badge/license-MIT-blue.svg" alt="License: MIT"></a>
    <a href="https://www.embarcadero.com/products/delphi"><img src="https://img.shields.io/badge/Delphi-2010%2B-red.svg" alt="Delphi"></a>
    <a href="https://www.freepascal.org/"><img src="https://img.shields.io/badge/FreePascal-3.0.0%2B-blue.svg" alt="FreePascal 3.0.0+"></a>
  </p>
</p>

---

NumCPULib4Pascal is a library for detecting the number of logical and physical CPUs on the current machine in Object Pascal, compatible with both Delphi and FreePascal across multiple operating systems, released under the permissive [MIT License](LICENSE).

## Table of Contents

- [Features](#features)
- [Getting Started](#getting-started)
- [Quick Examples](#quick-examples)
- [Contributing](#contributing)
- [Tip Jar](#tip-jar)
- [License](#license)
- [Branding](assets/branding/README.md)

## Features

- **Logical CPU count** -- detect the number of logical processors (hardware threads)
- **Physical CPU count** -- detect the number of physical cores
- **Cross-platform** -- Windows, Linux, macOS, Solaris, and BSD variants
- **Cross-compiler** -- Delphi and FreePascal

## Getting Started

### Prerequisites

| Compiler | Minimum Version |
| --- | --- |
| Delphi | 2010 or later |
| FreePascal | 3.0.0 or later |

### Installation

#### Delphi

1. Open package:
   - `NumCPULib/src/Packages/Delphi/NumCPULib4PascalPackage.dpk`
2. Build and install the package in the IDE.
3. Add `NumCPULib/src` subfolders to your project search path if needed.

#### FreePascal / Lazarus

1. Open package:
   - `NumCPULib/src/Packages/FPC/NumCPULib4PascalPackage.lpk`
2. Build/install package in Lazarus, or add `NumCPULib/src` paths to your FPC project.

## Quick Examples

### Get CPU Counts

```pascal
uses
  NumCPULib;

var
  LLogicalCount, LPhysicalCount: Int32;
begin
  // Count logical CPUs (hardware threads)
  LLogicalCount := TNumCPULib.GetLogicalCPUCount();

  // Count physical CPUs (cores)
  LPhysicalCount := TNumCPULib.GetPhysicalCPUCount();

  WriteLn(Format('Logical CPUs:  %d', [LLogicalCount]));
  WriteLn(Format('Physical CPUs: %d', [LPhysicalCount]));
end;
```

Additional samples can be found in the `NumCPULib.Samples` folder.

## Contributing

Contributions are welcome. Please open an [issue](https://github.com/Xor-el/NumCPULib4Pascal/issues) for bug reports or feature requests, and submit pull requests.

## Tip Jar

If you find this library useful and would like to support its continued development, tips are greatly appreciated! 🙏

| Cryptocurrency | Wallet Address |
|---|---|
| <img src="https://raw.githubusercontent.com/spothq/cryptocurrency-icons/master/32/icon/btc.png" width="20" alt="Bitcoin" /> **Bitcoin (BTC)** | `bc1quqhe342vw4ml909g334w9ygade64szqupqulmu` |
| <img src="https://raw.githubusercontent.com/spothq/cryptocurrency-icons/master/32/icon/eth.png" width="20" alt="Ethereum" /> **Ethereum (ETH)** | `0x53651185b7467c27facab542da5868bfebe2bb69` |
| <img src="https://raw.githubusercontent.com/spothq/cryptocurrency-icons/master/32/icon/sol.png" width="20" alt="Solana" /> **Solana (SOL)** | `BPZHjY1eYCdQjLecumvrTJRi5TXj3Yz1vAWcmyEB9Miu` |

## License

This project is licensed under the [MIT License](https://github.com/Xor-el/NumCPULib4Pascal/blob/master/LICENSE).