{ *********************************************************************************** }
{ *                              NumCPULib Library                                  * }
{ *                Copyright (c) 2019 Ugochukwu Mmaduekwe                           * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit NumCPULib;

{$DEFINE DELPHI}

{$IFDEF FPC}
{$UNDEF DELPHI}
{$MODE DELPHI}

{$IFDEF CPU386}
   {$DEFINE NUMCPULIB_X86}
{$ENDIF}

{$IFDEF CPUX64}
   {$DEFINE NUMCPULIB_X86_64}
{$ENDIF}

{$IFDEF CPUARM}
   {$DEFINE NUMCPULIB_ARM}
{$ENDIF}

{$IFDEF CPUAARCH64}
   {$DEFINE NUMCPULIB_AARCH64}
{$ENDIF}

{$IF DEFINED(NUMCPULIB_ARM) OR DEFINED(NUMCPULIB_AARCH64)}
   {$DEFINE NUMCPULIB_ARMCPU}
{$IFEND}

{$IFDEF IPHONESIM}
   {$DEFINE NUMCPULIB_IOSSIM}
{$ENDIF}

{$IF DEFINED(MSWINDOWS)}
   {$DEFINE NUMCPULIB_MSWINDOWS}
{$ELSEIF DEFINED(UNIX)}
   {$DEFINE NUMCPULIB_UNIX}
   {$IF DEFINED(BSD)}
      {$IF DEFINED(DARWIN)}
         {$DEFINE NUMCPULIB_APPLE}
         {$IF DEFINED(NUMCPULIB_ARM) OR DEFINED(NUMCPULIB_AARCH64)}
            {$DEFINE NUMCPULIB_IOS}
         {$ELSE}
            {$DEFINE NUMCPULIB_MACOS}
         {$IFEND}
      {$ELSEIF DEFINED(FREEBSD) OR DEFINED(NETBSD) OR DEFINED(OPENBSD) OR DEFINED(DRAGONFLY)}
         {$DEFINE NUMCPULIB_GENERIC_BSD}
      {$IFEND}
  {$ELSEIF DEFINED(ANDROID)}
     {$DEFINE NUMCPULIB_ANDROID}
  {$ELSEIF DEFINED(LINUX)}
     {$DEFINE NUMCPULIB_LINUX}
  {$ELSEIF DEFINED(SOLARIS)}
     {$DEFINE NUMCPULIB_SOLARIS}
  {$ELSE}
     {$DEFINE NUMCPULIB_UNDEFINED_UNIX_VARIANTS}
  {$IFEND}
{$ELSE}
   {$MESSAGE ERROR 'UNSUPPORTED TARGET.'}
{$IFEND}

{$IFDEF NUMCPULIB_ANDROID}
   {$DEFINE NUMCPULIB_LINUX}
{$ENDIF}

{$IF DEFINED(NUMCPULIB_GENERIC_BSD) OR DEFINED(NUMCPULIB_APPLE)}
   {$DEFINE NUMCPULIB_HAS_SYSCTL}
{$IFEND}

{$IF DEFINED(NUMCPULIB_LINUX) OR DEFINED(NUMCPULIB_GENERIC_BSD) OR DEFINED(NUMCPULIB_SOLARIS) OR DEFINED(NUMCPULIB_APPLE)}
   {$DEFINE NUMCPULIB_HAS_SYSCONF}
{$IFEND}

{$ENDIF FPC}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

{$IFDEF DELPHI}

 // XE3 and Above
{$IF CompilerVersion >= 24.0}
   {$DEFINE DELPHIXE3_UP}
   {$LEGACYIFEND ON}
{$IFEND}

{$IFDEF CPU386}
   {$DEFINE NUMCPULIB_X86}
{$ENDIF}

{$IFDEF CPUX64}
   {$DEFINE NUMCPULIB_X86_64}
{$ENDIF}

{$IFDEF CPUARM32}
   {$DEFINE NUMCPULIB_ARM}
{$ENDIF}

{$IFDEF CPUARM64}
   {$DEFINE NUMCPULIB_AARCH64}
{$ENDIF}

{$IF DEFINED(NUMCPULIB_ARM) OR DEFINED(NUMCPULIB_AARCH64)}
   {$DEFINE NUMCPULIB_ARMCPU}
{$IFEND}

{$IFDEF IOS}
  {$IFNDEF CPUARM}
     {$DEFINE NUMCPULIB_IOSSIM}
  {$ENDIF}
{$ENDIF}

{$IFDEF IOS}
   {$DEFINE NUMCPULIB_IOS}
{$ENDIF}

{$IFDEF MSWINDOWS}
   {$DEFINE NUMCPULIB_MSWINDOWS}
{$ENDIF}

{$IFDEF MACOS}
   {$IFNDEF IOS}
      {$DEFINE NUMCPULIB_MACOS}
   {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
   {$DEFINE NUMCPULIB_ANDROID}
{$ENDIF}

{$IF DEFINED(NUMCPULIB_IOS) OR DEFINED(NUMCPULIB_MACOS)}
   {$DEFINE NUMCPULIB_APPLE}
{$IFEND}

{$IF DEFINED(LINUX) OR DEFINED(NUMCPULIB_ANDROID)}
   {$DEFINE NUMCPULIB_LINUX}
{$IFEND}

{$IF DEFINED(NUMCPULIB_APPLE)}
   {$DEFINE NUMCPULIB_HAS_SYSCTL}
{$IFEND}

{$IF DEFINED(NUMCPULIB_LINUX) OR DEFINED(NUMCPULIB_APPLE)}
   {$DEFINE NUMCPULIB_HAS_SYSCONF}
{$IFEND}

{$ENDIF DELPHI}

interface

uses
{$IFDEF NUMCPULIB_MSWINDOWS}
  Windows,
{$ENDIF} // ENDIF NUMCPULIB_MSWINDOWS
  // ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCONF}
{$IFDEF FPC}
  ctypes,
{$ELSE}
  Posix.Unistd,
{$ENDIF}   // ENDIF FPC
{$ENDIF}  // ENDIF NUMCPULIB_HAS_SYSCONF
  // ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCTL}
{$IFDEF FPC}
  sysctl,
{$ELSE}
  Posix.SysSysctl,
{$ENDIF}   // ENDIF FPC
{$ENDIF} // ENDIF NUMCPULIB_HAS_SYSCTL
  // ================================================================//
{$IFDEF NUMCPULIB_APPLE}
{$IFDEF NUMCPULIB_MACOS}
{$IFDEF FPC}
  CocoaAll,
{$ELSE}
  Macapi.AppKit,
{$ENDIF} // ENDIF FPC
{$ENDIF} // ENDIF NUMCPULIB_MACOS
{$ENDIF}   // ENDIF NUMCPULIB_APPLE

  // ================================================================//
  SysUtils;

resourcestring
  SErrorOccuredGettingLogicalCPUCount =
    'An Error Occured while getting the Logical CPU Count.';

type
  ENumCPULibException = class(Exception);

type
  /// <summary>
  /// <para>
  /// A class with utilities to determine the number of CPUs available on
  /// the current system.
  /// </para>
  /// <para>
  /// This information can be used as a guide to how many tasks can be
  /// run in parallel.
  /// </para>
  /// <para>
  /// There are many properties of the system architecture that will
  /// affect parallelism, for example memory access speeds (for all the
  /// caches and RAM) and the physical architecture of the processor, so
  /// the number of CPUs should be used as a rough guide only.
  /// </para>
  /// </summary>
  TNumCPULib = class sealed(TObject)

  strict private

    // ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCONF}
    class function GetAppropriateSysConfNumber(): Int32; static;
{$ENDIF}
    // ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCTL}
    class function GetLogicalCPUCountUsingSysCtl(): Int32; static;
{$ENDIF}
    // ================================================================//
{$IFDEF NUMCPULIB_MSWINDOWS}
    class function GetLogicalCPUCountWindows(): Int32; static;
{$ENDIF}
    // ================================================================//
{$IFDEF NUMCPULIB_APPLE}
    class function GetLogicalCPUCountApple(): Int32; static;
{$ENDIF}
    // ================================================================//
{$IFDEF NUMCPULIB_LINUX}
    class function GetLogicalCPUCountLinux(): Int32; static;
{$ENDIF}
    // ================================================================//
{$IFDEF NUMCPULIB_SOLARIS}
    class function GetLogicalCPUCountSolaris(): Int32; static;
{$ENDIF}
    // ================================================================//
{$IFDEF NUMCPULIB_GENERIC_BSD}
    class function GetLogicalCPUCountGenericBSD(): Int32; static;
{$ENDIF}
    // ================================================================//
  public

    /// <summary>
    /// This function will get the number of logical cores. Sometimes this is
    /// different from the number of physical cores.
    /// </summary>
    class function GetLogicalCPUCount(): Int32; static;
  end;

{$IFDEF NUMCPULIB_HAS_SYSCONF}
{$IFDEF FPC}

function sysconf(i: cint): clong; cdecl; external 'c' name 'sysconf';
{$ENDIF}
{$ENDIF}

implementation

{ TNumCPULib }

// ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCONF}

class function TNumCPULib.GetAppropriateSysConfNumber(): Int32;
begin
  // On ARM targets, processors could be turned off to save power So we
  // use `_SC_NPROCESSORS_CONF` to get the real number.
  // ****************************************************************//
  // NUMCPULIB_LINUX
{$IFDEF NUMCPULIB_LINUX}
{$IFDEF NUMCPULIB_ARMCPU}
{$IFDEF NUMCPULIB_ANDROID}
  Result := 96; // _SC_NPROCESSORS_CONF
{$ELSE}
  // Devices like RPI
  Result := 83; // _SC_NPROCESSORS_CONF
{$ENDIF}
{$ELSE}
  // for non ARM Linux like CPU's
{$IFDEF NUMCPULIB_ANDROID}
  Result := 97; // _SC_NPROCESSORS_ONLN
{$ELSE}
  Result := 84; // _SC_NPROCESSORS_ONLN
{$ENDIF}  // ENDIF NUMCPULIB_ANDROID

{$ENDIF}  // ENDIF NUMCPULIB_ARMCPU
{$ENDIF} // ENDIF NUMCPULIB_LINUX
  // ****************************************************************//
  // NUMCPULIB_GENERIC_BSD
{$IFDEF NUMCPULIB_GENERIC_BSD}
{$IF DEFINED(FREEBSD) OR DEFINED(DRAGONFLY)}
  Result := 58; // _SC_NPROCESSORS_ONLN
{$IFEND}
{$IFDEF OPENBSD}
  Result := 503; // _SC_NPROCESSORS_ONLN
{$ENDIF}
{$IFDEF NETBSD}
  Result := 1002; // _SC_NPROCESSORS_ONLN
{$ENDIF}
{$ENDIF} // ENDIF NUMCPULIB_GENERIC_BSD
  // ****************************************************************//
  // NUMCPULIB_SOLARIS
{$IFDEF NUMCPULIB_SOLARIS}
{$IFDEF NUMCPULIB_ARMCPU}
  Result := 14; // _SC_NPROCESSORS_CONF
{$ELSE}
  Result := 15; // _SC_NPROCESSORS_ONLN
{$ENDIF}
{$ENDIF}  // ENDIF NUMCPULIB_SOLARIS
  // ****************************************************************//
  // NUMCPULIB_APPLE
{$IFDEF NUMCPULIB_APPLE}
{$IFDEF NUMCPULIB_ARMCPU}
  Result := 57; // _SC_NPROCESSORS_CONF
{$ELSE}
  Result := 58; // _SC_NPROCESSORS_ONLN
{$ENDIF}
{$ENDIF}  // ENDIF NUMCPULIB_APPLE
end;
{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCTL}

class function TNumCPULib.GetLogicalCPUCountUsingSysCtl(): Int32;
var
  LMib: array [0 .. 1] of Int32;
  LLen, LT: Int32;
begin
  LMib[0] := CTL_HW;
  LMib[1] := HW_NCPU;
  LLen := System.SizeOf(LT);
{$IFDEF FPC}
{$IF DEFINED(VER3_0_0) OR DEFINED(VER3_0_2)}
  fpsysctl(PChar(@LMib), 2, @LT, @LLen, nil, 0);
{$ELSE}
  fpsysctl(@LMib, 2, @LT, @LLen, nil, 0);
{$IFEND}
{$ELSE}
  sysctl(@LMib, 2, @LT, @LLen, nil, 0);
{$ENDIF}
  Result := LT;
end;
{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_MSWINDOWS}

class function TNumCPULib.GetLogicalCPUCountWindows(): Int32;
var
  LIdx: Int32;
  LProcessAffinityMask, LSystemAffinityMask: DWORD_PTR;
  LMask: DWORD;
  LSystemInfo: SYSTEM_INFO;
begin
  // returns total number of processors available to system including logical hyperthreaded processors
  if GetProcessAffinityMask(GetCurrentProcess, LProcessAffinityMask,
    LSystemAffinityMask) then
  begin
    Result := 0;
    for LIdx := 0 to 31 do
    begin
      LMask := DWORD(1) shl LIdx;
      if (LProcessAffinityMask and LMask) <> 0 then
      begin
        System.Inc(Result);
      end;
    end;
  end
  else
  begin
    // can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(LSystemInfo);
    Result := LSystemInfo.dwNumberOfProcessors;
  end;
end;
{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_APPLE}

class function TNumCPULib.GetLogicalCPUCountApple(): Int32;
begin
{$IF DEFINED(NUMCPULIB_MACOS)}
  // >= (Mac OS X 10.4+)
  if NSAppKitVersionNumber >= 824 then // NSAppKitVersionNumber10_4
  begin
    Result := sysconf(GetAppropriateSysConfNumber());
  end
  else
  begin
    // fallback for when sysconf API is not available
    Result := GetLogicalCPUCountUsingSysCtl();
  end;
{$ELSE}
  Result := sysconf(GetAppropriateSysConfNumber());
{$IFEND}
end;
{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_LINUX}

class function TNumCPULib.GetLogicalCPUCountLinux(): Int32;
begin
  Result := sysconf(GetAppropriateSysConfNumber());
end;
{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_SOLARIS}

class function TNumCPULib.GetLogicalCPUCountSolaris(): Int32;
begin
  Result := sysconf(GetAppropriateSysConfNumber());
end;
{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_GENERIC_BSD}

class function TNumCPULib.GetLogicalCPUCountGenericBSD(): Int32;
begin
  Result := sysconf(GetAppropriateSysConfNumber());
  if Result < 1 then
  begin
    Result := GetLogicalCPUCountUsingSysCtl();
  end;
end;
{$ENDIF}

class function TNumCPULib.GetLogicalCPUCount(): Int32;
begin
{$IF DEFINED(NUMCPULIB_MSWINDOWS)}
  Result := GetLogicalCPUCountWindows();
  if Result < 1 then
  begin
    raise ENumCPULibException.CreateRes(@SErrorOccuredGettingLogicalCPUCount);
  end;

{$ELSEIF DEFINED(NUMCPULIB_APPLE)}
  Result := GetLogicalCPUCountApple();
  if Result < 1 then
  begin
    raise ENumCPULibException.CreateRes(@SErrorOccuredGettingLogicalCPUCount);
  end;

{$ELSEIF DEFINED(NUMCPULIB_LINUX)}
  Result := GetLogicalCPUCountLinux();
  if Result < 1 then
  begin
    raise ENumCPULibException.CreateRes(@SErrorOccuredGettingLogicalCPUCount);
  end;

{$ELSEIF DEFINED(NUMCPULIB_SOLARIS)}
  Result := GetLogicalCPUCountSolaris();
  if Result < 1 then
  begin
    raise ENumCPULibException.CreateRes(@SErrorOccuredGettingLogicalCPUCount);
  end;

{$ELSEIF DEFINED(NUMCPULIB_GENERIC_BSD)}
  Result := GetLogicalCPUCountGenericBSD();
  if Result < 1 then
  begin
    raise ENumCPULibException.CreateRes(@SErrorOccuredGettingLogicalCPUCount);
  end;
{$ELSE}
  // fallback for other Unsupported Oses
  Result := 1;
{$IFEND}
end;

end.
