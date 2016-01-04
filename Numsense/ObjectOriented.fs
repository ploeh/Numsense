namespace Ploeh.Numsense.ObjectOriented

open System.Runtime.InteropServices
open Ploeh.Numsense

type INumeralConverter =
    abstract ToNumeral : number : int -> string
    abstract TryParse : s : string * [<Out>]result : int byref -> bool

module internal Helper =
    let tryParse f (s, result : int byref) =
        match isNull s, lazy (f s) with
        | true, _
        | false, Lazy None -> false
        | false, Lazy (Some i) -> result <- i; true

type EnglishNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toEnglish number
        member this.TryParse (s, result) = 
            Helper.tryParse Numeral.tryParseEnglish (s, &result)

type DanishNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toDanish number
        member this.TryParse (s, result) =
            Helper.tryParse Numeral.tryParseDanish (s, &result)

type Numeral private () =
    static member val English = EnglishNumeralConverter () :> INumeralConverter
    static member val Danish  =  DanishNumeralConverter () :> INumeralConverter