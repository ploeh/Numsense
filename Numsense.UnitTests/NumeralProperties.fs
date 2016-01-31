module Ploeh.Numsense.NumeralProperties

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

[<Property(QuietOnSuccess = true)>]
let ``tryParseEnglish is the inverse of toEnglish`` x =
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
let ``tryParseDanish is the inverse of toDanish`` x =
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
let ``tryParsePolish is the inverse of toPolish`` x =
    test <@ Some x = (x |> Numeral.toPolish |> Numeral.tryParsePolish) @>

[<Property(QuietOnSuccess = true)>]
let ``negative Polish is the inverse of positive Polish`` x =
    x <> 0 ==> lazy
    let x = abs x

    let actualPolish = Numeral.toPolish -x
    let actualInteger = Numeral.tryParsePolish actualPolish

    sprintf "minus %s" (Numeral.toPolish x) =! actualPolish
    Some -x =! actualInteger

[<Property(QuietOnSuccess = true)>]
let ``tryParseDutch is the inverse of toDutch`` x =
    test <@ Some x = (x |> Numeral.toDutch |> Numeral.tryParseDutch) @>

[<Property(QuietOnSuccess = true)>]
let ``negative Dutch is the inverse of positive Dutch`` x =
    x <> 0 ==> lazy
    let x = abs x

    let actualDutch = Numeral.toDutch -x
    let actualInteger = Numeral.tryParseDutch actualDutch

    sprintf "min %s" (Numeral.toDutch x) =! actualDutch
    Some -x =! actualInteger

[<Property(QuietOnSuccess = true)>]
let ``tryParseRussian is the inverse of toRussian`` x =
    test <@ Some x = (x |> Numeral.toRussian |> Numeral.tryParseRussian) @>

[<Property(QuietOnSuccess = true)>]
let ``negative Russian is the inverse of positive Russian`` x =
    x <> 0 ==> lazy
    let x = abs x

    let actualRussian = Numeral.toRussian -x
    let actualInteger = Numeral.tryParseRussian actualRussian

    sprintf "минус %s" (Numeral.toRussian x) =! actualRussian
    Some -x =! actualInteger


[<Property(QuietOnSuccess = true)>]
let ``tryParseGerman is the inverse of toGerman`` x =
    test <@ Some x = (x |> Numeral.toGerman |> Numeral.tryParseGerman) @>

[<Property(QuietOnSuccess = true)>]
let ``negative German is the inverse of positive German`` x =
    x <> 0 ==> lazy
    let x = abs x

    let actualGerman = Numeral.toGerman -x
    let actualInteger = Numeral.tryParseGerman actualGerman

    sprintf "minus-%s" (Numeral.toGerman x) =! actualGerman
    Some -x =! actualInteger
