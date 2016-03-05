namespace Ploeh.Numsense

open System.Globalization
open System.Threading
open Xunit.Sdk

type UseCultureAttribute (culture : string) =
    inherit BeforeAfterTestAttribute ()

    let targetCulture = CultureInfo culture
    let mutable originalCulture = None
    let mutable originalUICulture = None

    override this.Before methodUndertest =
        originalCulture   <- Some Thread.CurrentThread.CurrentCulture
        originalUICulture <- Some Thread.CurrentThread.CurrentUICulture

        Thread.CurrentThread.CurrentCulture   <- targetCulture
        Thread.CurrentThread.CurrentUICulture <- targetCulture
    
    override this.After methodUnderTest =
        originalCulture
        |> Option.iter (fun x -> Thread.CurrentThread.CurrentCulture <- x)

        originalUICulture
        |> Option.iter (fun x -> Thread.CurrentThread.CurrentUICulture <- x)