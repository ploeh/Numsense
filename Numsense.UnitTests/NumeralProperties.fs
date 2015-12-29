module Ploeh.Numsense.NumeralProperties

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

[<Property(QuietOnSuccess = true)>]
let ``tryOfEnglish is the inverse of toEnglish`` (x : int) =
    test <@ Some x = (x |> Numeral.toEnglish |> Numeral.tryOfEnglish) @>

[<Property(QuietOnSuccess = true)>]
let ``negative English is the inverse of positive English`` (x : int) =
    x <> 0 ==> lazy
    let x = abs x

    let actualEnglish = Numeral.toEnglish -x
    let actualInteger = Numeral.tryOfEnglish actualEnglish

    sprintf "minus %s" (Numeral.toEnglish x) =! actualEnglish
    Some -x =! actualInteger

[<Property(QuietOnSuccess = true)>]
let ``tryOfDanish is the inverse of toDanish`` (x : int) =
    test <@ Some x = (x |> Numeral.toDanish |> Numeral.tryOfDanish) @>

[<Property(QuietOnSuccess = true)>]
let ``negative Danish is the inverse of positive Danish`` (x : int) =
    x <> 0 ==> lazy
    let x = abs x

    let actualDanish = Numeral.toDanish -x
    let actualInteger = Numeral.tryOfDanish actualDanish

    sprintf "minus %s" (Numeral.toDanish x) =! actualDanish
    Some -x =! actualInteger