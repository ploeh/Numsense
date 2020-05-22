#r @"packages/FAKE.5.16.0/tools/FakeLib.dll"

open System
open System.Diagnostics
open System.IO
open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.Api
open Fake.DotNet.Testing
open Fake.IO
open Fake.Core.TargetOperators
open Fake.IO.Globbing.Operators

Target.create "Clean" <| fun _ ->
    let psi = ProcessStartInfo()
    psi.FileName <- "git"
    psi.Arguments <- "clean -xdf"
    psi.UseShellExecute <- true

    Process.Start psi |> ignore

Target.create "Build" <| fun _ ->
    DotNet.build (fun opts -> {
        opts with Configuration = DotNet.Release
    }) "Numsense.sln"

Target.create "Test" <| fun _ ->
    DotNet.test (fun opts -> { opts with Configuration = DotNet.Release }) "Numsense.sln"

Target.create "CopyNuGetPackage" <| fun _ ->
     !! "Numsense/bin/Release/*.nupkg"
    |> Shell.copy "."

"Clean"
==> "Build"
==> "Test"
==> "CopyNuGetPackage"

Target.runOrDefault "CopyNuGetPackage"