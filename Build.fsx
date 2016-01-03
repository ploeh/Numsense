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

"Clean"
==> "Build"
==> "Test"

RunTargetOrDefault "Test"