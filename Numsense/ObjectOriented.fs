namespace Ploeh.Numsense.ObjectOriented

open System.Runtime.InteropServices
open Ploeh.Numsense

type INumeralConverter =
    abstract ToNumeral : number : int -> string
    abstract TryParse : s : string * [<Out>]result : int byref -> bool

module Numeral =
    let internal asConverter toNumeral tryParse =
        { new INumeralConverter with
            member this.ToNumeral number = toNumeral number
            member this.TryParse (s, result) =
                match isNull s, lazy (tryParse s) with
                | true, _
                | false, Lazy None -> false
                | false, Lazy (Some i) -> result <- i; true }

    let English = asConverter Numeral.toEnglish Numeral.tryParseEnglish
    let Danish =  asConverter Numeral.toDanish  Numeral.tryParseDanish