import Lake
open Lake DSL

package fugue where
  version := v!"0.1.0"

require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.3"

-- macOS AudioToolbox framework linking + library paths
def audioLinkArgs : Array String :=
  if System.Platform.isOSX then
    #["-framework", "AudioToolbox",
      "-framework", "CoreFoundation",
      "-L/opt/homebrew/lib"]
  else
    #[]

@[default_target]
lean_lib Fugue where
  roots := #[`Fugue]

lean_lib FugueTests where
  globs := #[.submodules `FugueTests]

@[test_driver]
lean_exe fugue_tests where
  root := `FugueTests.Main
  moreLinkArgs := audioLinkArgs

-- Demo executable
lean_exe demo where
  root := `Demo
  moreLinkArgs := audioLinkArgs

-- Native FFI code
target fugue_ffi_o pkg : System.FilePath := do
  let oFile := pkg.buildDir / "native" / "fugue_ffi.o"
  let srcFile := pkg.dir / "native" / "src" / "fugue_ffi.c"
  let includeDir := pkg.dir / "native" / "include"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc" getLeanTrace

extern_lib libfugue_native pkg := do
  let name := nameToStaticLib "fugue_native"
  let ffiO ← fugue_ffi_o.fetch
  buildStaticLib (pkg.staticLibDir / name) #[ffiO]
