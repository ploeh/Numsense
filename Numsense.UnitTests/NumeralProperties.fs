module Ploeh.Numsense.NumeralProperties

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

[<Property(QuietOnSuccess = true)>]
let ``tryOfEnglish is the inverse of toEnglish`` x =
    test <@ Some x = (x |> Numeral.toEnglish |> Numeral.tryParseEnglish) @>

[<Property(QuietOnSuccess = true)>]
let ``negative English is the inverse of positive English`` x =
    x <> 0 ==> lazy
    let x = abs x

    let actualEnglish = Numeral.toEnglish -x
    let actualInteger = Numeral.tryParseEnglish actualEnglish

    sprintf "minus %s" (Numeral.toEnglish x) =! actualEnglish
    Some -x =! actualInteger

[<Property(QuietOnSuccess = true)>]
let ``tryOfDanish is the inverse of toDanish`` x =
    test <@ Some x = (x |> Numeral.toDanish |> Numeral.tryParseDanish) @>

[<Property(QuietOnSuccess = true)>]
let ``negative Danish is the inverse of positive Danish`` x =
    x <> 0 ==> lazy
    let x = abs x

    let actualDanish = Numeral.toDanish -x
    let actualInteger = Numeral.tryParseDanish actualDanish

    sprintf "minus %s" (Numeral.toDanish x) =! actualDanish
    Some -x =! actualInteger

[<Property(QuietOnSuccess = true)>]
let ``tryOfFarsi is the inverse of toFarsi`` x =
    test <@ Some x = (x |> Numeral.toFarsi |> Numeral.tryParseFarsi) @>

[<Property(QuietOnSuccess = true)>]
let ``negative Farsi is the inverse of positive Farsi`` x =
    x <> 0 ==> lazy
    let x = abs x

    let actualFarsi = Numeral.toFarsi -x
    let actualInteger = Numeral.tryParseFarsi actualFarsi

    sprintf "منفی %s" (Numeral.toFarsi x) =! actualFarsi
    Some -x =! actualInteger
