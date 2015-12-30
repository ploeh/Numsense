namespace Ploeh.Numsense.ObjectOriented

open System.Runtime.InteropServices
open Ploeh.Numsense

type INumeralConverter =
    abstract ToNumeral : number : int -> string
    abstract TryParse : s : string * [<Out>]result : int byref -> bool

type EnglishNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toEnglish number
        member this.TryParse (s, result) = 
            match s = null, lazy (Numeral.tryParseEnglish s) with
            | true, _
            | false, Lazy None -> false
            | false, Lazy (Some i) -> result <- i; true
