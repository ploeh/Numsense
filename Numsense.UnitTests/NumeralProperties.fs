module Ploeh.Numsense.NumeralProperties

open FsCheck.Xunit
open Swensen.Unquote

[<Property(QuietOnSuccess = true)>]
let ``ofEnglish is the inverse of toEnglish`` (x : int) =
    test <@ x = (x |> Numeral.toEnglish |> Numeral.ofEnglish) @>

