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
            match isNull s, lazy (Numeral.tryParseEnglish s) with
            | true, _
            | false, Lazy None -> false
            | false, Lazy (Some i) -> result <- i; true

type DanishNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toDanish number
        member this.TryParse (s, result) =
            match isNull s, lazy (Numeral.tryParseDanish s) with
            | true, _
            | false, Lazy None -> false
            | false, Lazy (Some i) -> result <- i; true

type FarsiNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toFarsi number
        member this.TryParse (s, result) =
            match s = null, lazy (Numeral.tryParseFarsi s) with
            | true, _
            | false, Lazy None -> false
            | false, Lazy (Some i) -> result <- i; true
         
type Numeral private () =
    static member val English = EnglishNumeralConverter () :> INumeralConverter
    static member val Danish  = DanishNumeralConverter () :> INumeralConverter
    static member val Farsi   = FarsiNumeralConverter () :> INumeralConverter