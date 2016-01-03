#r @"packages/FAKE.4.11.3/tools/FakeLib.dll"

open Fake
open Fake.Testing

Target "Clean" (fun _ ->
    directExec (fun info ->
        info.FileName <- "git"
        info.Arguments <- "clean -xdf")
    |> ignore)

Target "Build" (fun _ ->
    !! "Numsense.sln"
    |> MSBuildRelease "" "Rebuild"
    |> ignore)

Target "Test" (fun _ ->
    !! "*/bin/Release/*Ploeh.*.*Tests*.dll"
    |> xUnit2 (fun p -> { p with Parallel = ParallelMode.All }))

Target "PackageNuGet" (fun _ ->
    let version = GetAssemblyVersion "Numsense/bin/Release/Ploeh.Numsense.dll"
    let semVerString (v : System.Version) =
        sprintf "%i.%i.%i" v.Major v.Minor v.Build

    NuGet (fun p ->
        { p with
            Version = semVerString version
            WorkingDir = "."
            OutputPath = "."}) "Numsense.nuspec")

"Clean"
==> "Build"
==> "Test"
==> "PackageNuGet"

RunTargetOrDefault "PackageNuGet"